//! Unix-specific relay loop using poll() (select() for macOS's `/dev/tty`)

use std::io::{self, Read, Write};
use std::os::unix::io::AsRawFd;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use super::{get_terminal_size, ClientExitReason};
use crate::server::ipc::ClientConnection;
use crate::server::protocol::ClientControl;
use crate::services::fd_wait::wait_readable;

/// Main relay loop - bidirectional byte forwarding using poll() (see `fd_wait`)
pub fn relay_loop(
    conn: &mut ClientConnection,
    resize_flag: Arc<AtomicBool>,
) -> io::Result<ClientExitReason> {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut stdin_buf = [0u8; 4096];
    let mut data_buf = [0u8; 4096];

    let stdin_fd = stdin.as_raw_fd();
    let (data_fd, ctrl_fd) = conn.as_raw_fds();

    // The control socket is polled, so read it via try_read() (which reads
    // non-blocking and restores blocking mode for writes) and accumulate bytes
    // here until we have a complete newline-delimited message. A blocking
    // BufReader::read_line() would hang the whole relay if a control message
    // ever arrived split across reads (e.g. a large SetClipboard), and a fresh
    // BufReader per read would silently drop bytes buffered past the newline.
    let mut ctrl_buf: Vec<u8> = Vec::new();
    let mut ctrl_read = [0u8; 4096];

    // On a Linux VT the mouse reaches us over GPM's socket, not stdin, so it
    // has to be read here and forwarded like the keyboard — the server has
    // no terminal of its own to ask (#3517). `None` everywhere else.
    #[cfg(target_os = "linux")]
    let gpm = match crate::services::gpm::GpmClient::connect() {
        Ok(gpm) => gpm,
        Err(e) => {
            tracing::warn!("Failed to connect to GPM: {}", e);
            None
        }
    };
    #[cfg(target_os = "linux")]
    if gpm.is_some() {
        tracing::info!("Forwarding the GPM mouse to the server");
        // GPM cannot draw its pointer over a full-screen program; ask the
        // editor to draw one, as it does when it reads GPM itself.
        let msg = serde_json::to_string(&ClientControl::GpmPointer).unwrap_or_default();
        conn.write_control(&msg)?;
    }

    loop {
        // Check for resize
        if resize_flag.swap(false, Ordering::SeqCst) {
            if let Ok(size) = get_terminal_size() {
                let resize_msg = serde_json::to_string(&ClientControl::Resize {
                    cols: size.cols,
                    rows: size.rows,
                })
                .unwrap();
                conn.write_control(&resize_msg)?;
            }
        }

        // Wait on stdin, the data socket and the control socket.
        //
        // Through `fd_wait` rather than poll(2) directly: on macOS a stdin
        // reopened from `/dev/tty` (`fresh -a </dev/tty`, or a bare `fresh`
        // that auto-attaches) makes poll(2) report `POLLNVAL` at once, so
        // stdin was never read and this loop spun (#3683).
        #[cfg_attr(not(target_os = "linux"), allow(unused_mut))]
        let mut fds: Vec<std::os::unix::io::RawFd> = vec![stdin_fd, data_fd, ctrl_fd];
        #[cfg(target_os = "linux")]
        if let Some(gpm) = &gpm {
            fds.push(gpm.fd());
        }

        // 100ms timeout for the resize check.
        let ready = match wait_readable(&fds, Duration::from_millis(100)) {
            Ok(ready) if ready.is_empty() => continue, // Timeout, check resize
            Ok(ready) => ready,
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        };

        // Check stdin
        if ready.readable(0) {
            match stdin.read(&mut stdin_buf) {
                Ok(0) => {
                    // EOF on stdin - detach
                    let detach_msg = serde_json::to_string(&ClientControl::Detach).unwrap();
                    conn.write_control(&detach_msg)?;
                    return Ok(ClientExitReason::Detached);
                }
                Ok(n) => {
                    // The server does the parsing, but teardown here needs to
                    // know whether a click is still held (#3474).
                    #[cfg(feature = "runtime")]
                    crate::services::tty_input::note_mouse_bytes(&stdin_buf[..n]);
                    conn.write_data(&stdin_buf[..n])?;
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
                Err(e) => return Err(e),
            }
        }

        #[cfg(target_os = "linux")]
        if let Some(gpm) = &gpm {
            if ready.readable(3) {
                let reports = gpm.read_sgr_reports();
                if !reports.is_empty() {
                    conn.write_data(&reports)?;
                }
            }
        }

        // Check data socket
        if ready.readable(1) {
            match conn.read_data(&mut data_buf) {
                Ok(0) => {
                    // Server closed connection
                    return Ok(ClientExitReason::ServerQuit);
                }
                Ok(n) => {
                    stdout.write_all(&data_buf[..n])?;
                    stdout.flush()?;
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
                Err(e) => return Err(e),
            }
        }

        // Check control socket for server messages
        if ready.readable(2) {
            // Drain whatever is available without blocking, then process every
            // complete newline-delimited message. Partial trailing bytes stay
            // in `ctrl_buf` for the next iteration.
            loop {
                match conn.control.try_read(&mut ctrl_read) {
                    Ok(0) => {
                        // Control stream closed by the server.
                        return Ok(ClientExitReason::ServerQuit);
                    }
                    Ok(n) => ctrl_buf.extend_from_slice(&ctrl_read[..n]),
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => break,
                    Err(e) => return Err(e),
                }
            }

            while let Some(pos) = ctrl_buf.iter().position(|&b| b == b'\n') {
                let line: Vec<u8> = ctrl_buf.drain(..=pos).collect();
                let Ok(msg) = std::str::from_utf8(&line) else {
                    continue;
                };
                let msg = msg.trim();
                if msg.is_empty() {
                    continue;
                }
                if let Ok(ctrl) =
                    serde_json::from_str::<crate::server::protocol::ServerControl>(msg)
                {
                    match ctrl {
                        crate::server::protocol::ServerControl::Quit { reason } => {
                            tracing::debug!("Server sent quit: {}", reason);
                            return Ok(ClientExitReason::ServerQuit);
                        }
                        crate::server::protocol::ServerControl::SetClipboard {
                            text,
                            use_osc52,
                            use_system_clipboard,
                        } => {
                            super::set_client_clipboard(&text, use_osc52, use_system_clipboard);
                        }
                        crate::server::protocol::ServerControl::SetTitle { title } => {
                            crate::services::terminal_title::write_terminal_title(&title);
                        }
                        crate::server::protocol::ServerControl::SuspendClient => {
                            suspend_client(&mut stdout, conn)?;
                        }
                        crate::server::protocol::ServerControl::Pong => {
                            // Ignore pong responses
                        }
                        _ => {
                            // Ignore other control messages
                        }
                    }
                }
            }
        }

        // Check for socket errors
        if ready.hung_up(1) {
            return Ok(ClientExitReason::ServerQuit);
        }
    }
}

/// Suspend the client with SIGTSTP and restore its terminal on resume.
///
/// The server keeps running in session mode, so the client is the only piece
/// that has a real foreground shell to hand control back to. We emit the same
/// teardown bytes the server would have sent had the client detached, drop
/// raw mode, raise SIGTSTP, and on resume re-enable raw mode and nudge the
/// server to repaint by echoing the current terminal size back.
fn suspend_client(stdout: &mut io::Stdout, conn: &mut ClientConnection) -> io::Result<()> {
    use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
    use nix::sys::signal::{raise, Signal};

    // Leave alternate screen, disable mouse + bracketed paste, etc. — mirrors
    // what `TerminalModes::undo()` does in direct mode. The server already
    // marked this client `needs_full_render`, so the bytes it queues next
    // will include the matching setup sequences + a full paint.
    let teardown = crate::server::capture_backend::terminal_teardown_sequences();
    stdout.write_all(&teardown)?;
    stdout.flush()?;

    if let Err(e) = disable_raw_mode() {
        tracing::warn!("Failed to disable raw mode before suspend: {}", e);
    }
    crate::services::stderr_redirect::restore();

    // Block until the parent shell sends SIGCONT (typically via `fg`).
    if let Err(e) = raise(Signal::SIGTSTP) {
        tracing::error!("Failed to raise SIGTSTP: {}", e);
        // Best-effort re-enable so we don't leave the terminal in a bad state.
        if let Err(re) = enable_raw_mode() {
            tracing::error!("Failed to re-enable raw mode after failed suspend: {}", re);
        }
        crate::services::stderr_redirect::redirect_to_log();
        return Err(io::Error::other(format!("raise(SIGTSTP) failed: {}", e)));
    }

    // Resumed. Re-enable raw mode; the server will repaint on its next tick
    // because `needs_full_render` was set before we left.
    if let Err(e) = enable_raw_mode() {
        tracing::error!("Failed to re-enable raw mode after resume: {}", e);
        return Err(io::Error::other(format!("enable_raw_mode failed: {}", e)));
    }
    crate::services::stderr_redirect::redirect_to_log();

    // Echo our current size back so the server is guaranteed to tick a
    // render (Resize always flips `needs_render`) without waiting on input.
    if let Ok(size) = super::get_terminal_size() {
        let resize_msg = serde_json::to_string(&ClientControl::Resize {
            cols: size.cols,
            rows: size.rows,
        })
        .unwrap_or_default();
        if let Err(e) = conn.write_control(&resize_msg) {
            tracing::warn!("Failed to send post-resume resize to server: {}", e);
        }
    }

    Ok(())
}

/// Set up SIGWINCH handler for terminal resize
pub fn setup_resize_handler(flag: Arc<AtomicBool>) -> io::Result<()> {
    // Use a static to store the flag reference
    // This is safe because we only set it once
    static RESIZE_FLAG: std::sync::OnceLock<Arc<AtomicBool>> = std::sync::OnceLock::new();
    RESIZE_FLAG.get_or_init(|| flag.clone());

    extern "C" fn handle_sigwinch(_: libc::c_int) {
        if let Some(flag) = RESIZE_FLAG.get() {
            flag.store(true, Ordering::SeqCst);
        }
    }

    unsafe {
        let mut action: libc::sigaction = std::mem::zeroed();
        action.sa_sigaction = handle_sigwinch as *const () as usize;
        action.sa_flags = libc::SA_RESTART;

        if libc::sigaction(libc::SIGWINCH, &action, std::ptr::null_mut()) != 0 {
            return Err(io::Error::last_os_error());
        }
    }

    Ok(())
}
