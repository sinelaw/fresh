//! Windows-specific relay loop using crossterm events

use std::io::{self, Write};
use std::time::Duration;

use crossterm::event::{self, Event, KeyEventKind, KeyModifiers, MouseButton, MouseEventKind};

use super::{get_terminal_size, ClientExitReason};
use crate::server::ipc::ClientConnection;
use crate::server::protocol::{ClientControl, ServerControl};
use crate::services::terminal::pty::key_to_pty_bytes;

/// Main relay loop - single-threaded event loop for Windows
///
/// Uses crossterm for terminal input events and non-blocking pipe reads.
/// No threads, no mutex contention - just poll input and pipe in a loop.
pub fn relay_loop(conn: &mut ClientConnection) -> io::Result<ClientExitReason> {
    tracing::debug!("[windows_loop] Starting single-threaded event loop");

    let mut stdout = io::stdout();
    let mut data_buf = [0u8; 4096];
    let mut control_buf = Vec::new();
    let mut control_byte = [0u8; 1];
    let mut last_size = get_terminal_size().ok();

    loop {
        let mut did_work = false;

        // 1. Poll for terminal input events
        // Note: Duration::ZERO causes issues on Windows after mouse events
        // Using a small timeout instead
        tracing::debug!("[loop] poll start");
        match event::poll(Duration::from_millis(1)) {
            Ok(true) => {
                tracing::debug!("[loop] poll returned true, reading event");
                did_work = true;
                match event::read() {
                    Ok(Event::Key(key_event)) => {
                        tracing::debug!("[loop] Key event: {:?}", key_event);
                        // Only handle key press events, not release
                        if key_event.kind == KeyEventKind::Press {
                            if let Some(bytes) =
                                key_to_pty_bytes(key_event.code, key_event.modifiers)
                            {
                                tracing::debug!(
                                    "[loop] Writing {} key bytes to data pipe",
                                    bytes.len()
                                );
                                conn.write_data(&bytes)?;
                                tracing::debug!("[loop] Key write complete");
                            }
                        }
                    }
                    Ok(Event::Mouse(mouse_event)) => {
                        tracing::debug!("[loop] Mouse event: {:?}", mouse_event);
                        if let Some(bytes) = encode_mouse_event(&mouse_event) {
                            tracing::debug!(
                                "[loop] Writing {} mouse bytes to data pipe",
                                bytes.len()
                            );
                            conn.write_data(&bytes)?;
                            tracing::debug!("[loop] Mouse write complete");
                        }
                    }
                    Ok(Event::Paste(text)) => {
                        tracing::debug!("[loop] Paste event: {} bytes", text.len());
                        conn.write_data(text.as_bytes())?;
                    }
                    Ok(Event::Resize(cols, rows)) => {
                        tracing::debug!("[loop] Resize event: {}x{}", cols, rows);
                        let resize_msg =
                            serde_json::to_string(&ClientControl::Resize { cols, rows }).unwrap();
                        conn.write_control(&resize_msg)?;
                    }
                    Ok(Event::FocusGained | Event::FocusLost) => {
                        tracing::debug!("[loop] Focus event");
                    }
                    Err(e) => {
                        tracing::debug!("[loop] Event read error: {:?}", e);
                        // Input error - detach
                        let detach_msg = serde_json::to_string(&ClientControl::Detach).unwrap();
                        let _ = conn.write_control(&detach_msg);
                        return Ok(ClientExitReason::Detached);
                    }
                }
                tracing::debug!("[loop] event processing complete");
            }
            Ok(false) => {
                // No input events
            }
            Err(e) => {
                tracing::debug!("[loop] Poll error: {:?}", e);
                // Poll error - detach
                let detach_msg = serde_json::to_string(&ClientControl::Detach).unwrap();
                let _ = conn.write_control(&detach_msg);
                return Ok(ClientExitReason::Detached);
            }
        }

        // 2. Read from data pipe (non-blocking) and write to stdout
        tracing::debug!("[loop] try_read data pipe");
        match conn.data.try_read(&mut data_buf) {
            Ok(0) => {
                tracing::debug!("[loop] Data pipe EOF - server quit");
                // Server closed connection
                return Ok(ClientExitReason::ServerQuit);
            }
            Ok(n) => {
                tracing::debug!("[loop] Read {} bytes from data pipe, writing to stdout", n);
                did_work = true;
                stdout.write_all(&data_buf[..n])?;
                stdout.flush()?;
                tracing::debug!("[loop] Stdout write complete");
            }
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                // No data available
            }
            Err(e) => {
                tracing::debug!("[loop] Data pipe read error: {:?}", e);
                return Err(e);
            }
        }

        // 3. Check for control messages from server (non-blocking)
        tracing::debug!("[loop] try_read control pipe");
        match conn.control.try_read(&mut control_byte) {
            Ok(0) => {
                tracing::debug!("[loop] Control pipe EOF - server quit");
                // Control pipe closed
                return Ok(ClientExitReason::ServerQuit);
            }
            Ok(1) => {
                did_work = true;
                if control_byte[0] == b'\n' {
                    if let Ok(msg) = String::from_utf8(control_buf.clone()) {
                        tracing::debug!("[loop] Control message: {}", msg);
                        if let Ok(ctrl) = serde_json::from_str::<ServerControl>(&msg) {
                            match ctrl {
                                ServerControl::Quit { .. } => {
                                    tracing::debug!("[loop] Server sent quit");
                                    return Ok(ClientExitReason::ServerQuit);
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
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                // No control messages
            }
            Err(e) => {
                tracing::debug!("[loop] Control pipe error: {:?}", e);
                // Control pipe error - continue, data pipe is primary
            }
            _ => {}
        }

        // 4. Check for terminal resize
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

        // 5. If nothing happened, sleep briefly to avoid busy-spinning
        if !did_work {
            std::thread::sleep(Duration::from_millis(1));
        }
    }
}

/// Encode a crossterm mouse event to SGR mouse format bytes
fn encode_mouse_event(event: &crossterm::event::MouseEvent) -> Option<Vec<u8>> {
    // SGR uses 1-based coordinates
    let cx = event.column + 1;
    let cy = event.row + 1;

    // Build button code based on event kind
    let (mut button_code, is_release) = match event.kind {
        MouseEventKind::Down(btn) => {
            let code = match btn {
                MouseButton::Left => 0,
                MouseButton::Middle => 1,
                MouseButton::Right => 2,
            };
            (code, false)
        }
        MouseEventKind::Up(btn) => {
            let code = match btn {
                MouseButton::Left => 0,
                MouseButton::Middle => 1,
                MouseButton::Right => 2,
            };
            (code, true)
        }
        MouseEventKind::Drag(btn) => {
            let code = match btn {
                MouseButton::Left => 0,
                MouseButton::Middle => 1,
                MouseButton::Right => 2,
            };
            // Drag events have motion bit (32) set
            (code + 32, false)
        }
        MouseEventKind::Moved => {
            // Motion with no button = 35 (32 + 3)
            (35, false)
        }
        MouseEventKind::ScrollUp => (64, false),
        MouseEventKind::ScrollDown => (65, false),
        MouseEventKind::ScrollLeft => (66, false),
        MouseEventKind::ScrollRight => (67, false),
    };

    // Add modifier flags
    if event.modifiers.contains(KeyModifiers::SHIFT) {
        button_code += 4;
    }
    if event.modifiers.contains(KeyModifiers::ALT) {
        button_code += 8;
    }
    if event.modifiers.contains(KeyModifiers::CONTROL) {
        button_code += 16;
    }

    // SGR format: CSI < Cb ; Cx ; Cy M (or m for release)
    let terminator = if is_release { 'm' } else { 'M' };
    Some(format!("\x1b[<{};{};{}{}", button_code, cx, cy, terminator).into_bytes())
}
