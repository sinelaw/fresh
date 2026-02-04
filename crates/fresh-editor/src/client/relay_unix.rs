//! Unix-specific relay loop using poll()

use std::io::{self, Read, Write};
use std::os::unix::io::{AsRawFd, BorrowedFd};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use nix::poll::{poll, PollFd, PollFlags};

use super::{get_terminal_size, ClientExitReason};
use crate::server::ipc::ClientConnection;
use crate::server::protocol::ClientControl;

/// Main relay loop - bidirectional byte forwarding using poll()
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

        // Poll stdin, data socket, and control socket
        // SAFETY: The file descriptors are valid for the duration of this scope
        let stdin_borrowed = unsafe { BorrowedFd::borrow_raw(stdin_fd) };
        let data_borrowed = unsafe { BorrowedFd::borrow_raw(data_fd) };
        let ctrl_borrowed = unsafe { BorrowedFd::borrow_raw(ctrl_fd) };
        let mut fds = [
            PollFd::new(stdin_borrowed, PollFlags::POLLIN),
            PollFd::new(data_borrowed, PollFlags::POLLIN),
            PollFd::new(ctrl_borrowed, PollFlags::POLLIN),
        ];

        match poll(&mut fds, nix::poll::PollTimeout::from(100u8)) {
            // 100ms timeout for resize check
            Ok(0) => continue, // Timeout, check resize
            Ok(_) => {}
            Err(nix::errno::Errno::EINTR) => continue,
            Err(e) => return Err(io::Error::other(e.to_string())),
        }

        // Check stdin
        if fds[0]
            .revents()
            .map(|r| r.contains(PollFlags::POLLIN))
            .unwrap_or(false)
        {
            match stdin.read(&mut stdin_buf) {
                Ok(0) => {
                    // EOF on stdin - detach
                    let detach_msg = serde_json::to_string(&ClientControl::Detach).unwrap();
                    conn.write_control(&detach_msg)?;
                    return Ok(ClientExitReason::Detached);
                }
                Ok(n) => {
                    conn.write_data(&stdin_buf[..n])?;
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
                Err(e) => return Err(e),
            }
        }

        // Check data socket
        if fds[1]
            .revents()
            .map(|r| r.contains(PollFlags::POLLIN))
            .unwrap_or(false)
        {
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
        if fds[2]
            .revents()
            .map(|r| r.contains(PollFlags::POLLIN))
            .unwrap_or(false)
        {
            if let Ok(Some(msg)) = conn.read_control() {
                if let Ok(ctrl) =
                    serde_json::from_str::<crate::server::protocol::ServerControl>(&msg)
                {
                    match ctrl {
                        crate::server::protocol::ServerControl::Quit { reason } => {
                            tracing::debug!("Server sent quit: {}", reason);
                            return Ok(ClientExitReason::ServerQuit);
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
        if fds[1]
            .revents()
            .map(|r| r.contains(PollFlags::POLLHUP) || r.contains(PollFlags::POLLERR))
            .unwrap_or(false)
        {
            return Ok(ClientExitReason::ServerQuit);
        }
    }
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
        action.sa_sigaction = handle_sigwinch as usize;
        action.sa_flags = libc::SA_RESTART;

        if libc::sigaction(libc::SIGWINCH, &action, std::ptr::null_mut()) != 0 {
            return Err(io::Error::last_os_error());
        }
    }

    Ok(())
}
