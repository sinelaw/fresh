//! Measure startup time of the fresh editor.
//!
//! Spawns the editor in a real pseudo-terminal and measures time from exec
//! until the marker string (default: "File") appears in the rendered screen.
//!
//! Uses the `vt100` crate as a virtual terminal emulator to process raw PTY
//! output into actual screen contents, so we detect exactly what a user would
//! see. A dedicated reader thread responds to crossterm's DA1 query with
//! minimal latency.
//!
//! Usage:
//!   cargo run --release --bin measure_startup --features dev-bins,runtime -- [OPTIONS] [-- editor-args...]

#[cfg(not(unix))]
fn main() {
    eprintln!("measure_startup requires a Unix platform (PTY/fork support).");
    std::process::exit(1);
}

#[cfg(unix)]
use std::ffi::CString;
#[cfg(unix)]
use std::sync::atomic::{AtomicBool, Ordering};
#[cfg(unix)]
use std::sync::Arc;
#[cfg(unix)]
use std::time::{Duration, Instant};

#[cfg(unix)]
/// DA1 query that crossterm sends to detect keyboard enhancement support.
const DA1_QUERY: &[u8] = b"\x1b[c";
#[cfg(unix)]
/// DA1 response: VT420-compatible with common attributes.
const DA1_RESPONSE: &[u8] = b"\x1b[?64;1;2;6;22c";

#[cfg(unix)]
fn find_binary() -> String {
    let exe = std::env::current_exe().unwrap();
    let repo = exe
        .ancestors()
        .find(|p| p.join("Cargo.toml").exists() && p.join("crates").exists())
        .expect("could not find repo root");

    for profile in &["release", "debug"] {
        let path = repo.join("target").join(profile).join("fresh");
        if path.is_file() {
            return path.to_string_lossy().into_owned();
        }
    }

    if let Ok(output) = std::process::Command::new("which").arg("fresh").output() {
        if output.status.success() {
            return String::from_utf8_lossy(&output.stdout).trim().to_string();
        }
    }

    eprintln!("Error: could not find 'fresh' binary. Build with `cargo build --release` first.");
    std::process::exit(1);
}

#[cfg(unix)]
fn set_pty_size(fd: i32, rows: u16, cols: u16) {
    let ws = libc::winsize {
        ws_row: rows,
        ws_col: cols,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    unsafe {
        libc::ioctl(fd, libc::TIOCSWINSZ, &ws);
    }
}

#[cfg(unix)]
struct PtyResult {
    first_output_ms: Option<f64>,
    first_content_ms: Option<f64>,
}

#[cfg(unix)]
fn run_once(
    binary: &str,
    args: &[String],
    rows: u16,
    cols: u16,
    marker: &str,
) -> Option<PtyResult> {
    let mut master_fd: i32 = 0;
    let mut slave_fd: i32 = 0;
    let rc = unsafe {
        libc::openpty(
            &mut master_fd,
            &mut slave_fd,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        )
    };
    if rc != 0 {
        eprintln!("openpty failed");
        return None;
    }

    set_pty_size(master_fd, rows, cols);

    let start = Instant::now();

    let pid = unsafe { libc::fork() };
    if pid < 0 {
        eprintln!("fork failed");
        return None;
    }

    if pid == 0 {
        unsafe {
            libc::close(master_fd);
            libc::setsid();
            libc::ioctl(slave_fd, libc::TIOCSCTTY as libc::c_ulong, 0);
            libc::dup2(slave_fd, 0);
            libc::dup2(slave_fd, 1);
            libc::dup2(slave_fd, 2);
            if slave_fd > 2 {
                libc::close(slave_fd);
            }
        }

        std::env::set_var("TERM", "xterm-256color");

        let c_binary = CString::new(binary).unwrap();
        let mut c_args: Vec<CString> = vec![CString::new(binary).unwrap()];
        for a in args {
            c_args.push(CString::new(a.as_str()).unwrap());
        }
        let c_argv: Vec<*const libc::c_char> = c_args
            .iter()
            .map(|a| a.as_ptr())
            .chain(std::iter::once(std::ptr::null()))
            .collect();

        unsafe {
            libc::execv(c_binary.as_ptr(), c_argv.as_ptr());
            libc::_exit(127);
        }
    }

    // Parent
    unsafe {
        libc::close(slave_fd);
    }

    let master_raw = master_fd;

    let done = Arc::new(AtomicBool::new(false));
    let done2 = done.clone();
    let marker_owned = marker.to_string();
    let rows_copy = rows;
    let cols_copy = cols;

    let handle = std::thread::spawn(move || {
        let mut parser = vt100::Parser::new(rows_copy, cols_copy, 0);
        let mut raw_buf = Vec::new();
        let mut da1_replied = false;
        let mut first_output: Option<Instant> = None;
        let mut first_content: Option<Instant> = None;
        let mut read_buf = [0u8; 65536];

        while !done2.load(Ordering::Relaxed) {
            let mut pfd = libc::pollfd {
                fd: master_raw,
                events: libc::POLLIN,
                revents: 0,
            };
            let ret = unsafe { libc::poll(&mut pfd, 1, 1) };
            if ret <= 0 {
                continue;
            }
            if pfd.revents & libc::POLLIN == 0 {
                if pfd.revents & (libc::POLLHUP | libc::POLLERR) != 0 {
                    break;
                }
                continue;
            }

            let n = unsafe {
                libc::read(
                    master_raw,
                    read_buf.as_mut_ptr() as *mut libc::c_void,
                    read_buf.len(),
                )
            };
            if n <= 0 {
                break;
            }

            let chunk = &read_buf[..n as usize];
            let now = Instant::now();

            // Respond to DA1 immediately — critical for avoiding crossterm's 2s timeout.
            if !da1_replied {
                raw_buf.extend_from_slice(chunk);
                if raw_buf.windows(DA1_QUERY.len()).any(|w| w == DA1_QUERY) {
                    let _ = unsafe {
                        libc::write(
                            master_raw,
                            DA1_RESPONSE.as_ptr() as *const libc::c_void,
                            DA1_RESPONSE.len(),
                        )
                    };
                    da1_replied = true;
                    raw_buf.clear(); // No longer needed
                }
            }

            if first_output.is_none() {
                first_output = Some(now);
            }

            // Feed bytes to the virtual terminal emulator
            parser.process(chunk);

            // Check if the marker is visible on the virtual screen
            if first_content.is_none() {
                let contents = parser.screen().contents();
                if contents.contains(&marker_owned) {
                    first_content = Some(now);
                    break;
                }
            }
        }

        (first_output, first_content)
    });

    // Wait for reader thread with timeout
    let timeout = Duration::from_secs(10);
    let result = loop {
        if handle.is_finished() {
            break handle.join().ok();
        }
        if start.elapsed() > timeout {
            done.store(true, Ordering::Relaxed);
            break handle.join().ok();
        }
        std::thread::sleep(Duration::from_millis(10));
    };

    // Kill the editor
    unsafe {
        libc::kill(pid, libc::SIGTERM);
        let mut status = 0;
        for _ in 0..20 {
            let r = libc::waitpid(pid, &mut status, libc::WNOHANG);
            if r != 0 {
                break;
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        libc::kill(pid, libc::SIGKILL);
        libc::waitpid(pid, &mut status, 0);
    }

    unsafe {
        libc::close(master_fd);
    }

    let (first_output, first_content) = result?;
    Some(PtyResult {
        first_output_ms: first_output.map(|t| t.duration_since(start).as_secs_f64() * 1000.0),
        first_content_ms: first_content.map(|t| t.duration_since(start).as_secs_f64() * 1000.0),
    })
}

#[cfg(unix)]
fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let mut runs: usize = 5;
    let mut warmup: usize = 1;
    let mut rows: u16 = 24;
    let mut cols: u16 = 80;
    let mut binary: Option<String> = None;
    let mut marker = "File".to_string();
    let mut editor_args: Vec<String> = Vec::new();

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--runs" => {
                runs = args[i + 1].parse().unwrap();
                i += 2;
            }
            "--warmup" => {
                warmup = args[i + 1].parse().unwrap();
                i += 2;
            }
            "--rows" => {
                rows = args[i + 1].parse().unwrap();
                i += 2;
            }
            "--cols" => {
                cols = args[i + 1].parse().unwrap();
                i += 2;
            }
            "--binary" => {
                binary = Some(args[i + 1].clone());
                i += 2;
            }
            "--marker" => {
                marker = args[i + 1].clone();
                i += 2;
            }
            "--" => {
                editor_args = args[i + 1..].to_vec();
                break;
            }
            other if !other.starts_with("--") && binary.is_none() => {
                binary = Some(other.to_string());
                i += 1;
            }
            _ => {
                eprintln!("Unknown argument: {}", args[i]);
                std::process::exit(1);
            }
        }
    }

    let binary = binary.unwrap_or_else(find_binary);
    let total_runs = warmup + runs;

    println!("Binary:  {binary}");
    println!("PTY:     {rows}x{cols}");
    println!("Marker:  {marker:?}");
    println!("Runs:    {runs} (+ {warmup} warmup)");
    if !editor_args.is_empty() {
        println!("Args:    {}", editor_args.join(" "));
    }
    println!();

    let mut results = Vec::new();

    for i in 0..total_runs {
        let is_warmup = i < warmup;
        let label = if is_warmup {
            format!("  warmup {}/{warmup}", i + 1)
        } else {
            format!("  run {}/{runs}", i - warmup + 1)
        };

        let timing = run_once(&binary, &editor_args, rows, cols, &marker);

        match timing {
            Some(t) if t.first_content_ms.is_some() => {
                let content_ms = t.first_content_ms.unwrap();
                let output_detail = t
                    .first_output_ms
                    .map(|ms| format!("  (first output: {ms:.1}ms)"))
                    .unwrap_or_default();
                println!("{label}: {content_ms:.1}ms{output_detail}");
                if !is_warmup {
                    results.push(content_ms);
                }
            }
            Some(t) if t.first_output_ms.is_some() => {
                let ms = t.first_output_ms.unwrap();
                println!("{label}: {ms:.1}ms (output only, marker {marker:?} not found)");
            }
            _ => {
                println!("{label}: FAILED (no output detected)");
            }
        }
    }

    if results.is_empty() {
        println!("\nNo successful runs.");
        std::process::exit(1);
    }

    results.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = results.len();
    let mean: f64 = results.iter().sum::<f64>() / n as f64;
    let median = if n % 2 == 1 {
        results[n / 2]
    } else {
        (results[n / 2 - 1] + results[n / 2]) / 2.0
    };
    let min = results[0];
    let max = results[n - 1];
    let variance: f64 = results.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / n as f64;
    let stddev = variance.sqrt();
    let p95 = results[(n as f64 * 0.95).min((n - 1) as f64) as usize];

    println!();
    println!("Results ({n} runs):");
    println!("  min:    {min:.1}ms");
    println!("  median: {median:.1}ms");
    println!("  mean:   {mean:.1}ms");
    println!("  p95:    {p95:.1}ms");
    println!("  max:    {max:.1}ms");
    println!("  stddev: {stddev:.1}ms");
}
