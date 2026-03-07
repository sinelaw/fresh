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

use crate::server::ipc::{ClientConnection, SocketPaths};
use crate::server::protocol::{
    ClientControl, ClientHello, ServerControl, TermSize, PROTOCOL_VERSION,
};

#[cfg(unix)]
mod relay_unix;
#[cfg(windows)]
mod relay_windows;
#[cfg(windows)]
pub mod win_vt_input;

/// Client configuration
pub struct ClientConfig {
    /// Socket paths for the session
    pub socket_paths: SocketPaths,
    /// Initial terminal size
    pub term_size: TermSize,
}

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

/// Run the client, connecting to an existing server
///
/// This function blocks until the connection is closed or an error occurs.
/// It handles:
/// - Handshake with version negotiation
/// - Raw mode setup
/// - Bidirectional byte relay
/// - Resize events (via SIGWINCH on Unix)
/// - Clean terminal restoration
pub fn run_client(config: ClientConfig) -> io::Result<ClientExitReason> {
    let conn = ClientConnection::connect(&config.socket_paths)?;
    run_client_with_connection(config, conn)
}

/// Run the client with an already-established connection
///
/// This is useful when the caller has already established a connection
/// (e.g., after retrying connection attempts). Performs handshake then relay.
pub fn run_client_with_connection(
    config: ClientConfig,
    conn: ClientConnection,
) -> io::Result<ClientExitReason> {
    // Perform handshake
    let hello = ClientHello::new(config.term_size);
    let hello_json = serde_json::to_string(&ClientControl::Hello(hello))
        .map_err(|e| io::Error::other(e.to_string()))?;
    conn.write_control(&hello_json)?;

    // Read server response
    let response = conn
        .read_control()?
        .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "Server closed connection"))?;

    let server_msg: ServerControl =
        serde_json::from_str(&response).map_err(|e| io::Error::other(e.to_string()))?;

    match server_msg {
        ServerControl::Hello(server_hello) => {
            if server_hello.protocol_version != PROTOCOL_VERSION {
                return Ok(ClientExitReason::VersionMismatch {
                    server_version: server_hello.server_version,
                });
            }
            tracing::info!(
                "Connected to session '{}' (server {})",
                server_hello.session_id,
                server_hello.server_version
            );
        }
        ServerControl::VersionMismatch(mismatch) => {
            return Ok(ClientExitReason::VersionMismatch {
                server_version: mismatch.server_version,
            });
        }
        ServerControl::Error { message } => {
            return Err(io::Error::other(format!("Server error: {}", message)));
        }
        _ => {
            return Err(io::Error::other("Unexpected server response"));
        }
    }

    run_client_relay(conn)
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
        return relay_unix::relay_loop(&mut conn, resize_flag);
    }

    #[cfg(windows)]
    return relay_windows::relay_loop(&mut conn);
}

/// Set the system clipboard on the client side
///
/// Respects the server's clipboard configuration:
/// - `use_osc52`: send OSC 52 escape sequences to the terminal
/// - `use_system_clipboard`: use native clipboard via arboard (X11/Wayland/macOS)
fn set_client_clipboard(text: &str, use_osc52: bool, use_system_clipboard: bool) {
    use std::io::Write;

    if use_osc52 {
        use crossterm::clipboard::CopyToClipboard;
        use crossterm::execute;

        if let Err(e) = execute!(io::stdout(), CopyToClipboard::to_clipboard_from(text)) {
            tracing::debug!("Client OSC 52 clipboard copy failed: {}", e);
        }
        #[allow(clippy::let_underscore_must_use)]
        let _ = io::stdout().flush();
    }

    if use_system_clipboard {
        match arboard::Clipboard::new() {
            Ok(mut clipboard) => {
                if let Err(e) = clipboard.set_text(text) {
                    tracing::debug!("Client arboard clipboard copy failed: {}", e);
                }
            }
            Err(e) => {
                tracing::debug!("Client arboard clipboard init failed: {}", e);
            }
        }
    }
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
        use windows_sys::Win32::System::Console::{
            GetConsoleScreenBufferInfo, GetStdHandle, CONSOLE_SCREEN_BUFFER_INFO, STD_OUTPUT_HANDLE,
        };

        unsafe {
            let handle = GetStdHandle(STD_OUTPUT_HANDLE);
            let mut info: CONSOLE_SCREEN_BUFFER_INFO = std::mem::zeroed();
            if GetConsoleScreenBufferInfo(handle, &mut info) == 0 {
                return Err(io::Error::last_os_error());
            }
            let cols = (info.srWindow.Right - info.srWindow.Left + 1) as u16;
            let rows = (info.srWindow.Bottom - info.srWindow.Top + 1) as u16;
            Ok(TermSize::new(cols, rows))
        }
    }
}
