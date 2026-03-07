//! Windows-specific relay loop using VT input
//!
//! Uses `ENABLE_VIRTUAL_TERMINAL_INPUT` with `ReadConsoleInputW` to receive raw
//! VT escape sequences (including bracketed paste markers) from the terminal.
//! Raw VT bytes are forwarded directly to the server's data pipe, where the
//! server-side `InputParser` handles all parsing. This matches how the Unix
//! relay works (raw stdin bytes forwarded to server).

use std::io::{self, Write};

use super::{get_terminal_size, ClientExitReason};
use crate::client::win_vt_input::{self, VtInputEvent};
use crate::server::ipc::ClientConnection;
use crate::server::protocol::{ClientControl, ServerControl};

/// Main relay loop - reads VT input and forwards to server
///
/// Enables VT input mode on the console, reads INPUT_RECORD events, extracts
/// raw VT bytes from key events, and forwards them to the server. Resize and
/// focus events are handled separately via the control channel.
pub fn relay_loop(conn: &mut ClientConnection) -> io::Result<ClientExitReason> {
    tracing::debug!("[windows_loop] Starting VT input relay loop");

    let old_console_mode = win_vt_input::enable_vt_input()?;
    let result = relay_loop_inner(conn);

    // Restore console mode on exit
    if let Err(e) = win_vt_input::restore_console_mode(old_console_mode) {
        tracing::warn!("Failed to restore console mode: {}", e);
    }

    result
}

fn relay_loop_inner(conn: &mut ClientConnection) -> io::Result<ClientExitReason> {
    let mut stdout = io::stdout();
    let mut data_buf = [0u8; 4096];
    let mut control_buf = Vec::new();
    let mut control_byte = [0u8; 1];
    let mut last_size = get_terminal_size().ok();

    loop {
        // Check for data from server (non-blocking)
        match conn.data.try_read(&mut data_buf) {
            Ok(0) => {
                return Ok(ClientExitReason::ServerQuit);
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
        match conn.control.try_read(&mut control_byte) {
            Ok(0) => {
                return Ok(ClientExitReason::ServerQuit);
            }
            Ok(1) => {
                if control_byte[0] == b'\n' {
                    if let Ok(msg) = String::from_utf8(control_buf.clone()) {
                        if let Ok(ctrl) = serde_json::from_str::<ServerControl>(&msg) {
                            match ctrl {
                                ServerControl::Quit { .. } => {
                                    return Ok(ClientExitReason::ServerQuit);
                                }
                                ServerControl::SetClipboard {
                                    text,
                                    use_osc52,
                                    use_system_clipboard,
                                } => {
                                    super::set_client_clipboard(
                                        &text,
                                        use_osc52,
                                        use_system_clipboard,
                                    );
                                }
                                _ => {}
                            }
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

        // Read VT input events from console
        match win_vt_input::read_vt_input() {
            Ok(events) => {
                for event in events {
                    match event {
                        VtInputEvent::VtBytes(bytes) => {
                            // Forward raw VT bytes directly to the server.
                            // The server's InputParser handles all parsing including
                            // bracketed paste, mouse events, key sequences, etc.
                            conn.write_data(&bytes)?;
                        }
                        VtInputEvent::Resize => {
                            if let Ok(size) = get_terminal_size() {
                                let resize_msg = serde_json::to_string(&ClientControl::Resize {
                                    cols: size.cols,
                                    rows: size.rows,
                                })
                                .unwrap();
                                conn.write_control(&resize_msg)?;
                            }
                        }
                        VtInputEvent::FocusGained | VtInputEvent::FocusLost => {
                            // Focus events arrive as VT sequences when enabled
                        }
                    }
                }
            }
            Err(e) => {
                tracing::debug!("[loop] VT input read error: {:?}", e);
                let detach_msg = serde_json::to_string(&ClientControl::Detach).unwrap();
                let _ = conn.write_control(&detach_msg);
                return Ok(ClientExitReason::Detached);
            }
        }

        // Check for terminal resize (polling fallback)
        if let Ok(new_size) = get_terminal_size() {
            if last_size.as_ref() != Some(&new_size) {
                last_size = Some(new_size);
                let resize_msg = serde_json::to_string(&ClientControl::Resize {
                    cols: new_size.cols,
                    rows: new_size.rows,
                })
                .unwrap();
                let _ = conn.write_control(&resize_msg);
            }
        }
    }
}
