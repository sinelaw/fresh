//! Ultra-light client for session persistence
//!
//! The client is intentionally minimal:
//! - Connect to server (data + control sockets)
//! - Perform handshake (send env, check version)
//! - Set terminal to raw mode
//! - Relay bytes bidirectionally (stdin↔data socket, data socket↔stdout)
//! - Forward resize events via control socket
//! - Restore terminal on exit
//!
//! All complexity (input parsing, rendering, editor logic) lives server-side.

use std::io;
#[cfg(unix)]
use std::sync::atomic::AtomicBool;
#[cfg(unix)]
use std::sync::Arc;

use crate::server::ipc::ClientConnection;
use crate::server::protocol::TermSize;
#[cfg(windows)]
use crate::server::protocol::{ClientControl, ServerControl};

#[cfg(unix)]
mod relay_unix;

/// Reason the client exited
#[derive(Debug)]
pub enum ClientExitReason {
    /// Server closed the connection normally
    ServerQuit,
    /// User requested detach
    Detached,
    /// Version mismatch between client and server
    VersionMismatch { server_version: String },
    /// Connection error
    Error(io::Error),
}

/// Run the relay loop with an already-handshaked connection
///
/// Use this when handshake has already been performed externally.
/// Caller must have already enabled raw mode.
pub fn run_client_relay(
    #[allow(unused_mut)] mut conn: ClientConnection,
) -> io::Result<ClientExitReason> {
    // Set up for relay
    // On Windows, don't set nonblocking here - the relay loop uses try_read() which handles this
    // Setting nonblocking can fail with error 233 if the pipe state isn't fully established
    #[cfg(not(windows))]
    conn.set_data_nonblocking(true)?;

    // Run the platform-specific relay loop
    #[cfg(unix)]
    {
        let resize_flag = Arc::new(AtomicBool::new(false));
        relay_unix::setup_resize_handler(resize_flag.clone())?;
        relay_unix::relay_loop(&mut conn, resize_flag)
    }

    #[cfg(windows)]
    {
        let result = fresh_winterm::relay_loop(&mut conn)?;
        return Ok(match result {
            fresh_winterm::RelayExitReason::ServerQuit => ClientExitReason::ServerQuit,
            fresh_winterm::RelayExitReason::Detached => ClientExitReason::Detached,
        });
    }
}

/// Set the system clipboard on the client side.
///
/// Delegates to the shared clipboard implementation which uses a persistent
/// arboard handle (critical on X11/Wayland where the owner must stay alive).
fn set_client_clipboard(text: &str, use_osc52: bool, use_system_clipboard: bool) {
    crate::services::clipboard::copy_to_system_clipboard(text, use_osc52, use_system_clipboard);
}

/// Get current terminal size
pub fn get_terminal_size() -> io::Result<TermSize> {
    #[cfg(unix)]
    {
        let mut size: libc::winsize = unsafe { std::mem::zeroed() };
        let result = unsafe { libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut size) };
        if result == -1 {
            return Err(io::Error::last_os_error());
        }
        Ok(TermSize::new(size.ws_col, size.ws_row))
    }

    #[cfg(windows)]
    {
        let size = fresh_winterm::get_terminal_size()?;
        Ok(TermSize::new(size.cols, size.rows))
    }
}

// --- RelayConnection trait impl for ClientConnection (Windows only) ---

#[cfg(windows)]
impl fresh_winterm::RelayConnection for ClientConnection {
    fn try_read_data(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.read_data(buf)
    }

    fn try_read_control_byte(&mut self, buf: &mut [u8; 1]) -> io::Result<usize> {
        self.control.try_read(buf)
    }

    fn write_data(&mut self, buf: &[u8]) -> io::Result<()> {
        ClientConnection::write_data(self, buf)
    }

    fn send_resize(&mut self, cols: u16, rows: u16) -> io::Result<()> {
        let msg = serde_json::to_string(&ClientControl::Resize { cols, rows }).unwrap();
        ClientConnection::write_control(self, &msg)
    }

    fn handle_server_control(&mut self, msg: &str) -> Option<fresh_winterm::RelayExitReason> {
        if let Ok(ctrl) = serde_json::from_str::<ServerControl>(msg) {
            match ctrl {
                ServerControl::Quit { .. } => Some(fresh_winterm::RelayExitReason::ServerQuit),
                ServerControl::SetClipboard {
                    text,
                    use_osc52,
                    use_system_clipboard,
                } => {
                    set_client_clipboard(&text, use_osc52, use_system_clipboard);
                    None
                }
                ServerControl::SuspendClient => {
                    // Windows has no SIGTSTP; surface the limitation and keep running.
                    tracing::warn!("SuspendClient received on Windows — suspend is not supported");
                    None
                }
                _ => None,
            }
        } else {
            None
        }
    }
}
