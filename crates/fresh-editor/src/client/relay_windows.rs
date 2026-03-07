//! Windows-specific relay loop using VT input
//!
//! Uses `ENABLE_VIRTUAL_TERMINAL_INPUT` with `ReadConsoleInputW` to receive raw
//! VT escape sequences (including bracketed paste markers) from the terminal.
//! Raw VT bytes are forwarded directly to the server's data pipe, where the
//! server-side `InputParser` handles all parsing. This matches how the Unix
//! relay works (raw stdin bytes forwarded to server).

use std::io::{self, Write};
use std::time::Duration;

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

    // Enable VT input mode to receive ANSI escape sequences (bracketed paste, etc.)
    let old_console_mode = match win_vt_input::enable_vt_input() {
        Ok(mode) => mode,
        Err(e) => {
            tracing::warn!(
                "Failed to enable VT input mode: {}. Falling back to crossterm relay.",
                e
            );
            return relay_loop_crossterm(conn);
        }
    };

    let result = relay_loop_vt(conn);

    // Restore console mode on exit
    if let Err(e) = win_vt_input::restore_console_mode(old_console_mode) {
        tracing::warn!("Failed to restore console mode: {}", e);
    }

    result
}

/// VT input relay loop - the primary path when VT input is available
fn relay_loop_vt(conn: &mut ClientConnection) -> io::Result<ClientExitReason> {
    let mut stdout = io::stdout();
    let mut data_buf = [0u8; 4096];
    let mut control_buf = Vec::new();
    let mut control_byte = [0u8; 1];
    let mut last_size = get_terminal_size().ok();

    loop {
        // 1. Read console input events (blocks until at least one is available)
        // We use a thread to avoid blocking the pipe reads
        // For now, use a simple polling approach similar to the old crossterm loop

        // Check for data from server first (non-blocking)
        match conn.data.try_read(&mut data_buf) {
            Ok(0) => {
                tracing::debug!("[loop] Data pipe EOF - server quit");
                return Ok(ClientExitReason::ServerQuit);
            }
            Ok(n) => {
                stdout.write_all(&data_buf[..n])?;
                stdout.flush()?;
            }
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
            Err(e) => {
                tracing::debug!("[loop] Data pipe read error: {:?}", e);
                return Err(e);
            }
        }

        // Check for control messages from server (non-blocking)
        match conn.control.try_read(&mut control_byte) {
            Ok(0) => {
                tracing::debug!("[loop] Control pipe EOF - server quit");
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
                            // Query actual terminal size and send via control channel
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
                            // Focus events are handled via VT sequences when enabled
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

/// Fallback relay loop using crossterm events (for legacy Windows without VT input support)
fn relay_loop_crossterm(conn: &mut ClientConnection) -> io::Result<ClientExitReason> {
    use crossterm::event::{self, Event, KeyEventKind, KeyModifiers, MouseButton, MouseEventKind};

    use crate::services::terminal::pty::key_to_pty_bytes;

    tracing::debug!("[crossterm_loop] Starting crossterm fallback event loop");

    let mut stdout = io::stdout();
    let mut data_buf = [0u8; 4096];
    let mut control_buf = Vec::new();
    let mut control_byte = [0u8; 1];
    let mut last_size = get_terminal_size().ok();

    loop {
        let mut did_work = false;

        // 1. Poll for terminal input events
        match event::poll(Duration::from_millis(1)) {
            Ok(true) => {
                did_work = true;
                match event::read() {
                    Ok(Event::Key(key_event)) => {
                        if key_event.kind == KeyEventKind::Press {
                            if let Some(bytes) =
                                key_to_pty_bytes(key_event.code, key_event.modifiers)
                            {
                                conn.write_data(&bytes)?;
                            }
                        }
                    }
                    Ok(Event::Mouse(mouse_event)) => {
                        if let Some(bytes) = encode_mouse_event(&mouse_event) {
                            conn.write_data(&bytes)?;
                        }
                    }
                    Ok(Event::Paste(text)) => {
                        // Wrap in bracketed paste markers so the server-side
                        // InputParser recognizes this as a paste event.
                        conn.write_data(b"\x1b[200~")?;
                        conn.write_data(text.as_bytes())?;
                        conn.write_data(b"\x1b[201~")?;
                    }
                    Ok(Event::Resize(cols, rows)) => {
                        let resize_msg =
                            serde_json::to_string(&ClientControl::Resize { cols, rows }).unwrap();
                        conn.write_control(&resize_msg)?;
                    }
                    Ok(Event::FocusGained | Event::FocusLost) => {}
                    Err(e) => {
                        tracing::debug!("[loop] Event read error: {:?}", e);
                        let detach_msg = serde_json::to_string(&ClientControl::Detach).unwrap();
                        let _ = conn.write_control(&detach_msg);
                        return Ok(ClientExitReason::Detached);
                    }
                }
            }
            Ok(false) => {}
            Err(e) => {
                tracing::debug!("[loop] Poll error: {:?}", e);
                let detach_msg = serde_json::to_string(&ClientControl::Detach).unwrap();
                let _ = conn.write_control(&detach_msg);
                return Ok(ClientExitReason::Detached);
            }
        }

        // 2. Read from data pipe
        match conn.data.try_read(&mut data_buf) {
            Ok(0) => return Ok(ClientExitReason::ServerQuit),
            Ok(n) => {
                did_work = true;
                stdout.write_all(&data_buf[..n])?;
                stdout.flush()?;
            }
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
            Err(e) => return Err(e),
        }

        // 3. Check control messages
        match conn.control.try_read(&mut control_byte) {
            Ok(0) => return Ok(ClientExitReason::ServerQuit),
            Ok(1) => {
                did_work = true;
                if control_byte[0] == b'\n' {
                    if let Ok(msg) = String::from_utf8(control_buf.clone()) {
                        if let Ok(ctrl) = serde_json::from_str::<ServerControl>(&msg) {
                            if matches!(ctrl, ServerControl::Quit { .. }) {
                                return Ok(ClientExitReason::ServerQuit);
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

        if !did_work {
            std::thread::sleep(Duration::from_millis(1));
        }
    }
}

/// Encode a crossterm mouse event to SGR mouse format bytes (for crossterm fallback)
fn encode_mouse_event(event: &crossterm::event::MouseEvent) -> Option<Vec<u8>> {
    use crossterm::event::{KeyModifiers, MouseButton, MouseEventKind};

    let cx = event.column + 1;
    let cy = event.row + 1;

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
            (code + 32, false)
        }
        MouseEventKind::Moved => (35, false),
        MouseEventKind::ScrollUp => (64, false),
        MouseEventKind::ScrollDown => (65, false),
        MouseEventKind::ScrollLeft => (66, false),
        MouseEventKind::ScrollRight => (67, false),
    };

    if event.modifiers.contains(KeyModifiers::SHIFT) {
        button_code += 4;
    }
    if event.modifiers.contains(KeyModifiers::ALT) {
        button_code += 8;
    }
    if event.modifiers.contains(KeyModifiers::CONTROL) {
        button_code += 16;
    }

    let terminator = if is_release { 'm' } else { 'M' };
    Some(format!("\x1b[<{};{};{}{}", button_code, cx, cy, terminator).into_bytes())
}
