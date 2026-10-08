//! The Linux console mouse reaches a daemon session (#3517).
//!
//! On a VT the mouse does not arrive on stdin: GPM delivers it over its own
//! socket, read through libgpm. Only the in-process editor ever connected to
//! GPM, so once a bare `fresh` became a client of a background daemon, the
//! console mouse did nothing at all — the client relayed stdin and nothing
//! else, and the daemon has no terminal of its own to ask.
//!
//! Asserted end to end: a real daemon, a real client on a pty, and a stand-in
//! libgpm the client loads in place of the system one (`FRESH_TEST_GPM_LIB`,
//! which also waives the "is this a VT" check a pty would fail). The stand-in
//! is a few lines of C compiled here: `Gpm_Open` hands back a FIFO and
//! `Gpm_GetEvent` reads one `Gpm_Event` from it, so the test writes mouse
//! reports exactly as the GPM daemon would and watches the editor answer
//! them — click, wheel and drag — through the client's real FFI path.
//!
//! Skipped where there is no pty or no C compiler, like the other
//! binary-driving tests skip without a pty.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin, PtyChild};
use std::ffi::CString;
use std::fs::File;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

/// How long the editor may take to answer one mouse report. Generous for a
/// debug binary on a loaded runner; the failure mode is silence.
const BUDGET: Duration = Duration::from_secs(10);

/// The stand-in libgpm. `Gpm_Event` here is gpm.h's layout, which is what
/// the editor's FFI declares.
const FAKE_LIBGPM: &str = r#"
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
typedef struct {
    unsigned char buttons, modifiers;
    unsigned short vc;
    short dx, dy, x, y;
    int type, clicks, margin;
    short wdx, wdy;
} Gpm_Event;
int gpm_visiblepointer = 0;
static int fd = -1;
int Gpm_Open(void *conn, int flag) {
    const char *path = getenv("FAKE_GPM_FIFO");
    if (!path) return -1;
    fd = open(path, O_RDWR);
    return fd;
}
int Gpm_Close(void) { if (fd >= 0) close(fd); fd = -1; return 0; }
int Gpm_GetEvent(Gpm_Event *ev) {
    ssize_t n = read(fd, ev, sizeof *ev);
    return n == (ssize_t)sizeof *ev ? 1 : (n == 0 ? 0 : -1);
}
"#;

// gpm.h's button bits and event types.
const LEFT: u8 = 4;
const MOVE: i32 = 1;
const DRAG: i32 = 2;
const DOWN: i32 = 4;
const UP: i32 = 8;
const SINGLE: i32 = 16;

/// Build the stand-in into `dir`, or `None` when there is no C compiler.
fn build_fake_libgpm(dir: &Path) -> Option<PathBuf> {
    let src = dir.join("fakegpm.c");
    let lib = dir.join("libfakegpm.so");
    std::fs::write(&src, FAKE_LIBGPM).unwrap();
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());
    let built = Command::new(cc)
        .args(["-shared", "-fPIC", "-o"])
        .arg(&lib)
        .arg(&src)
        .status()
        .is_ok_and(|s| s.success());
    built.then_some(lib)
}

/// The GPM side of the console: mouse reports written as the daemon would.
struct Gpm(File);

impl Gpm {
    /// One `Gpm_Event`. `col`/`row` are 0-based screen cells; GPM's are
    /// 1-based.
    fn send(&mut self, col: u16, row: u16, event_type: i32, buttons: u8, wdy: i16) {
        let mut ev = Vec::with_capacity(28);
        ev.push(buttons);
        ev.push(0); // modifiers
        ev.extend_from_slice(&0u16.to_ne_bytes()); // vc
        ev.extend_from_slice(&0i16.to_ne_bytes()); // dx
        ev.extend_from_slice(&0i16.to_ne_bytes()); // dy
        ev.extend_from_slice(&(col as i16 + 1).to_ne_bytes());
        ev.extend_from_slice(&(row as i16 + 1).to_ne_bytes());
        ev.extend_from_slice(&event_type.to_ne_bytes());
        ev.extend_from_slice(&1i32.to_ne_bytes()); // clicks
        ev.extend_from_slice(&0i32.to_ne_bytes()); // margin
        ev.extend_from_slice(&0i16.to_ne_bytes()); // wdx
        ev.extend_from_slice(&wdy.to_ne_bytes());
        self.0.write_all(&ev).unwrap();
    }

    fn click(&mut self, col: u16, row: u16) {
        self.send(col, row, DOWN | SINGLE, LEFT, 0);
        self.send(col, row, UP | SINGLE, LEFT, 0);
    }
}

/// A `fresh` whose config, state and sockets all live under `home`.
fn isolated_fresh(home: &Path) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_fresh"));
    cmd.current_dir(home.join("project"))
        .env("HOME", home)
        .env("TMPDIR", home)
        .env("XDG_CONFIG_HOME", home.join("config"))
        .env("XDG_DATA_HOME", home.join("data"))
        .env("XDG_STATE_HOME", home.join("state"))
        .env("XDG_CACHE_HOME", home.join("cache"))
        .env("XDG_RUNTIME_DIR", home.join("run"))
        .env("TERM", "xterm-256color")
        .env("LANG", "C.UTF-8")
        .env_remove("FRESH_SESSION")
        .env_remove("FRESH_BIN")
        .env_remove("FRESH_TEST_GPM_LIB");
    cmd
}

fn setup(home: &Path) {
    std::fs::create_dir_all(home.join("project")).unwrap();
    std::fs::create_dir_all(home.join("run")).unwrap();
    let config_dir = home.join("config").join("fresh");
    std::fs::create_dir_all(&config_dir).unwrap();
    std::fs::write(
        config_dir.join("config.json"),
        "{\n  \"check_for_updates\": false\n}\n",
    )
    .unwrap();
    let text: String = (1..=80).map(|i| format!("line {i:02}\n")).collect();
    std::fs::write(home.join("project").join("note.txt"), text).unwrap();
}

/// Where `text` starts on screen, as (col, row), read off the cell grid.
fn find(screen: &vt100::Screen, text: &str) -> Option<(u16, u16)> {
    let (rows, cols) = screen.size();
    let chars: Vec<String> = text.chars().map(String::from).collect();
    for row in 0..rows {
        for col in 0..cols.saturating_sub(chars.len() as u16) {
            if chars.iter().enumerate().all(|(i, c)| {
                screen
                    .cell(row, col + i as u16)
                    .is_some_and(|cell| cell.contents() == *c)
            }) {
                return Some((col, row));
            }
        }
    }
    None
}

fn kill_daemon(home: &Path, session: &str) {
    let pid_file = home.join("run").join("fresh").join(format!("{session}.pid"));
    if let Ok(pid) = std::fs::read_to_string(pid_file) {
        if let Ok(pid) = pid.trim().parse::<i32>() {
            // SAFETY: a plain `kill(2)`; an already-dead pid just returns
            // ESRCH, which is ignored.
            unsafe { libc::kill(pid, libc::SIGKILL) };
        }
    }
}

fn wait_for(client: &mut PtyChild, what: &str, pred: impl Fn(&vt100::Screen) -> bool) {
    if let Err(e) = client.wait_for_cells_within(BUDGET, pred) {
        panic!("{what}: {e}");
    }
}

/// Click, wheel and drag from the console mouse all reach the daemon.
#[test]
fn the_console_mouse_reaches_a_daemon_session() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let home = home.path();
    setup(home);
    let Some(lib) = build_fake_libgpm(home) else {
        eprintln!("Skipping: no C compiler to build the stand-in libgpm");
        return;
    };

    let fifo = home.join("gpm.fifo");
    let fifo_c = CString::new(fifo.as_os_str().as_bytes()).unwrap();
    // SAFETY: a NUL-terminated path we own.
    assert_eq!(unsafe { libc::mkfifo(fifo_c.as_ptr(), 0o600) }, 0, "mkfifo");
    // Read-write, so opening never blocks waiting for the other end and the
    // client never sees EOF between reports.
    let mut gpm = Gpm(File::options().read(true).write(true).open(&fifo).unwrap());

    let session = "gpm-mouse";
    let mut server = isolated_fresh(home);
    server
        .args(["--server", "--session-name", session, "--no-plugins"])
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null());
    let mut server = server.spawn().expect("spawn the daemon");

    let mut cmd = isolated_fresh(home);
    cmd.args(["--no-plugins", "-a", session, "note.txt"])
        .env("FRESH_TEST_GPM_LIB", &lib)
        .env("FAKE_GPM_FIFO", &fifo);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 100, 30).expect("spawn fresh on a pty");

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        wait_for(&mut client, "the daemon never rendered the file", |s| {
            find(s, "line 07").is_some()
        });

        // A click on the "0" of "line 07" puts the cursor there.
        let (col, row) = find(client.modes(), "line 07").unwrap();
        gpm.click(col + 5, row);
        wait_for(&mut client, "a GPM click did not move the cursor", |s| {
            s.contents().contains("Ln 7, Col 6")
        });

        // The wheel scrolls the buffer.
        for _ in 0..3 {
            gpm.send(col + 5, row, MOVE, 0, -1);
        }
        wait_for(&mut client, "the GPM wheel did not scroll", |s| {
            find(s, "line 01").is_none()
        });

        // A drag selects: typing over "line " leaves "X20".
        let (col, row) = find(client.modes(), "line 20").expect("line 20 on screen");
        gpm.send(col, row, DOWN | SINGLE, LEFT, 0);
        for dx in 1..=5 {
            gpm.send(col + dx, row, DRAG, LEFT, 0);
        }
        gpm.send(col + 5, row, UP | SINGLE, LEFT, 0);
        // The selection is not drawn as text, so wait for the release to
        // land before typing: the cursor sits at the drag's end.
        wait_for(&mut client, "the GPM drag did not reach the editor", |s| {
            s.contents().contains("Ln 20, Col 6")
        });
        client.send(b"X").unwrap();
        wait_for(&mut client, "the GPM drag did not select", |s| {
            find(s, "X20").is_some()
        });
    }));

    client.kill();
    kill_daemon(home, session);
    let _ = server.kill();
    let _ = server.wait();
    if let Err(panic) = result {
        std::panic::resume_unwind(panic);
    }
}
