//! Windows-specific client/server relay loop using VT input.
//!
//! Uses `ENABLE_VIRTUAL_TERMINAL_INPUT` with `ReadConsoleInputW` to receive raw
//! VT escape sequences (including bracketed paste markers) from the terminal.
//! Raw VT bytes are forwarded directly to the server's data pipe, where the
//! server-side `InputParser` handles all parsing. This matches how the Unix
//! relay works (raw stdin bytes forwarded to server).
//!
//! The relay is decoupled from concrete IPC types via the [`RelayConnection`]
//! trait, which the editor crate implements on its `ClientConnection`.

use std::io::{self, Write};

use crate::vt_input::{self, VtInputEvent, VtInputReader};

/// Why the relay loop exited.
#[derive(Debug)]
pub enum RelayExitReason {
    /// Server closed the connection normally.
    ServerQuit,
    /// Client detached from the session.
    Detached,
}

/// Abstraction over the IPC connection to the server.
///
/// Implemented by `ClientConnection` in the editor crate. This trait keeps
/// `fresh-winterm` free of any dependency on the editor's IPC and protocol types.
pub trait RelayConnection {
    /// Try to read data from the server (non-blocking).
    /// Returns `Ok(0)` on EOF, `Err(WouldBlock)` if nothing available.
    fn try_read_data(&mut self, buf: &mut [u8]) -> io::Result<usize>;

    /// Try to read one byte from the control channel (non-blocking).
    /// Returns `Ok(0)` on EOF, `Err(WouldBlock)` if nothing available.
    fn try_read_control_byte(&mut self, buf: &mut [u8; 1]) -> io::Result<usize>;

    /// Write raw data (VT input bytes) to the server.
    fn write_data(&mut self, buf: &[u8]) -> io::Result<()>;

    /// Send a resize notification to the server.
    fn send_resize(&mut self, cols: u16, rows: u16) -> io::Result<()>;

    /// Handle a complete control message line from the server.
    /// Returns `Some(reason)` if the relay should exit.
    fn handle_server_control(&mut self, msg: &str) -> Option<RelayExitReason>;
}

/// Main relay loop — reads VT input and forwards to server.
///
/// Enables VT input mode on the console, reads INPUT_RECORD events, extracts
/// raw VT bytes from key events, and forwards them to the server. Resize and
/// focus events are handled separately via the control channel.
pub fn relay_loop(conn: &mut impl RelayConnection) -> io::Result<RelayExitReason> {
    tracing::debug!("[windows_loop] Starting VT input relay loop");

    let old_console_mode = vt_input::enable_vt_input()?;
    // Default to CellMotion (safe, low event volume). The server will send
    // the correct mode via terminal_setup_sequences() shortly after connection.
    if let Err(e) = vt_input::enable_mouse_tracking(vt_input::MouseMode::CellMotion) {
        tracing::warn!("Failed to enable mouse tracking: {}", e);
    }
    // Spawn dedicated reader thread (same as direct mode)
    let reader = VtInputReader::spawn();
    let result = relay_loop_inner(conn, &reader);

    // Restore mouse tracking and console mode on exit
    let _ = vt_input::disable_mouse_tracking();
    if let Err(e) = vt_input::restore_console_mode(old_console_mode) {
        tracing::warn!("Failed to restore console mode: {}", e);
    }

    result
}

fn relay_loop_inner(
    conn: &mut impl RelayConnection,
    reader: &VtInputReader,
) -> io::Result<RelayExitReason> {
    let mut stdout = io::stdout();
    let mut data_buf = [0u8; 4096];
    let mut control_buf = Vec::new();
    let mut control_byte = [0u8; 1];
    let mut last_size = crate::get_terminal_size().ok();

    loop {
        // Check for data from server (non-blocking)
        match conn.try_read_data(&mut data_buf) {
            Ok(0) => {
                return Ok(RelayExitReason::ServerQuit);
            }
            Ok(n) => {
                stdout.write_all(&data_buf[..n])?;
                stdout.flush()?;
            }
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
            Err(e) => {
                return Err(e);
            }
        }

        // Check for control messages from server (non-blocking)
        match conn.try_read_control_byte(&mut control_byte) {
            Ok(0) => {
                return Ok(RelayExitReason::ServerQuit);
            }
            Ok(1) => {
                if control_byte[0] == b'\n' {
                    if let Ok(msg) = String::from_utf8(control_buf.clone()) {
                        if let Some(reason) = conn.handle_server_control(&msg) {
                            return Ok(reason);
                        }
                    }
                    control_buf.clear();
                } else {
                    control_buf.push(control_byte[0]);
                }
            }
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
            Err(e) => {
                tracing::debug!("[loop] Control pipe error: {:?}", e);
            }
            _ => {}
        }

        // Drain VT input events from the reader thread (non-blocking)
        loop {
            match reader.try_recv() {
                Some(VtInputEvent::VtBytes(bytes)) => {
                    conn.write_data(&bytes)?;
                }
                Some(VtInputEvent::Resize) => {
                    if let Ok(size) = crate::get_terminal_size() {
                        conn.send_resize(size.cols, size.rows)?;
                    }
                }
                Some(VtInputEvent::FocusGained | VtInputEvent::FocusLost) => {}
                None => break,
            }
        }

        // Check for terminal resize (polling fallback)
        if let Ok(new_size) = crate::get_terminal_size() {
            if last_size.as_ref() != Some(&new_size) {
                last_size = Some(new_size);
                let _ = conn.send_resize(new_size.cols, new_size.rows);
            }
        }
    }
}
