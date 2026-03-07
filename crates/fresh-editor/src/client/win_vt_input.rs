//! Windows VT input reader
//!
//! Enables `ENABLE_VIRTUAL_TERMINAL_INPUT` on the console input handle and reads
//! `INPUT_RECORD` events via `ReadConsoleInputW`. When VT input mode is active,
//! key events contain raw VT escape sequences (including bracketed paste markers)
//! in `uChar`, which can be forwarded as raw bytes or parsed by `InputParser`.
//!
//! This module also handles non-VT events like window resize and focus changes,
//! which are delivered as structured `INPUT_RECORD` fields.

use std::io;
use std::time::Duration;

use windows_sys::Win32::Foundation::{INVALID_HANDLE_VALUE, WAIT_OBJECT_0};
use windows_sys::Win32::System::Console::{
    GetConsoleMode, GetStdHandle, ReadConsoleInputW, SetConsoleMode, ENABLE_VIRTUAL_TERMINAL_INPUT,
    ENABLE_WINDOW_INPUT, FOCUS_EVENT, INPUT_RECORD, KEY_EVENT, MOUSE_EVENT, STD_INPUT_HANDLE,
    WINDOW_BUFFER_SIZE_EVENT,
};
use windows_sys::Win32::System::Threading::WaitForSingleObject;

/// Enable VT input mode on the console input handle.
///
/// This sets `ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_WINDOW_INPUT` so that:
/// - Key input arrives as raw VT sequences in `KEY_EVENT_RECORD.uChar`
/// - Window resize events are still delivered as `WINDOW_BUFFER_SIZE_EVENT`
///
/// Returns the previous console mode so it can be restored later.
pub fn enable_vt_input() -> io::Result<u32> {
    unsafe {
        let handle = GetStdHandle(STD_INPUT_HANDLE);
        if handle.is_null() || handle == INVALID_HANDLE_VALUE {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Failed to get stdin handle",
            ));
        }

        let mut old_mode: u32 = 0;
        if GetConsoleMode(handle, &mut old_mode) == 0 {
            return Err(io::Error::last_os_error());
        }

        // Enable VT input for ANSI escape sequences (bracketed paste, etc.)
        // Enable window input for resize events
        // Preserve other flags that crossterm may have set (e.g., ENABLE_EXTENDED_FLAGS)
        // but clear ENABLE_MOUSE_INPUT since mouse events will arrive as VT sequences,
        // and clear line/echo/processed input since we want raw VT bytes
        let new_mode = (old_mode & !0x0010) // clear ENABLE_MOUSE_INPUT
            | ENABLE_VIRTUAL_TERMINAL_INPUT
            | ENABLE_WINDOW_INPUT;
        if SetConsoleMode(handle, new_mode) == 0 {
            return Err(io::Error::last_os_error());
        }

        tracing::debug!(
            "Enabled VT input mode (old=0x{:x}, new=0x{:x})",
            old_mode,
            new_mode
        );

        Ok(old_mode)
    }
}

/// Restore the console input mode to a previous value.
pub fn restore_console_mode(mode: u32) -> io::Result<()> {
    unsafe {
        let handle = GetStdHandle(STD_INPUT_HANDLE);
        if SetConsoleMode(handle, mode) == 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(())
    }
}

/// Result from reading console input events.
pub enum VtInputEvent {
    /// Raw VT bytes from key events (may contain escape sequences, bracketed paste, etc.)
    VtBytes(Vec<u8>),
    /// Window resize event (caller should use get_terminal_size() for accurate dimensions)
    Resize,
    /// Focus gained
    FocusGained,
    /// Focus lost
    FocusLost,
}

/// Read available console input events and return them as VT bytes + structured events.
///
/// This reads all pending `INPUT_RECORD` events from the console input buffer.
/// Key events have their `uChar` values extracted as raw bytes (which contain VT
/// sequences when `ENABLE_VIRTUAL_TERMINAL_INPUT` is active). Resize and focus
/// events are returned as structured events.
///
/// This function blocks until at least one event is available.
pub fn read_vt_input() -> io::Result<Vec<VtInputEvent>> {
    let mut records: [INPUT_RECORD; 64] = unsafe { std::mem::zeroed() };
    let mut count: u32 = 0;

    let handle = unsafe { GetStdHandle(STD_INPUT_HANDLE) };

    let result = unsafe {
        ReadConsoleInputW(
            handle,
            records.as_mut_ptr(),
            records.len() as u32,
            &mut count,
        )
    };

    if result == 0 {
        return Err(io::Error::last_os_error());
    }

    let mut events = Vec::new();
    let mut vt_bytes = Vec::new();
    let mut surrogate_high: Option<u16> = None;

    for i in 0..count as usize {
        let record = &records[i];
        match record.EventType as u32 {
            KEY_EVENT => {
                let key_event = unsafe { record.Event.KeyEvent };
                // Only process key-down events to avoid duplicates
                if key_event.bKeyDown != 0 {
                    let ch = unsafe { key_event.uChar.UnicodeChar };
                    if ch != 0 {
                        // Handle UTF-16 surrogate pairs
                        if (0xD800..=0xDBFF).contains(&ch) {
                            // High surrogate — buffer it
                            surrogate_high = Some(ch);
                        } else if (0xDC00..=0xDFFF).contains(&ch) {
                            // Low surrogate — combine with buffered high surrogate
                            if let Some(high) = surrogate_high.take() {
                                if let Some(c) =
                                    char::decode_utf16([high, ch]).next().and_then(|r| r.ok())
                                {
                                    let mut buf = [0u8; 4];
                                    let s = c.encode_utf8(&mut buf);
                                    vt_bytes.extend_from_slice(s.as_bytes());
                                }
                            }
                        } else {
                            // Regular BMP character
                            surrogate_high = None;
                            if let Some(c) = char::from_u32(ch as u32) {
                                let mut buf = [0u8; 4];
                                let s = c.encode_utf8(&mut buf);
                                vt_bytes.extend_from_slice(s.as_bytes());
                            }
                        }
                    }
                }
            }
            WINDOW_BUFFER_SIZE_EVENT => {
                // Flush any accumulated VT bytes first
                if !vt_bytes.is_empty() {
                    events.push(VtInputEvent::VtBytes(std::mem::take(&mut vt_bytes)));
                }
                // The caller should use get_terminal_size() for accurate window dimensions,
                // since WINDOW_BUFFER_SIZE_EVENT reports the buffer size, not the window size.
                events.push(VtInputEvent::Resize);
            }
            FOCUS_EVENT => {
                // Flush any accumulated VT bytes first
                if !vt_bytes.is_empty() {
                    events.push(VtInputEvent::VtBytes(std::mem::take(&mut vt_bytes)));
                }
                let focus_event = unsafe { record.Event.FocusEvent };
                if focus_event.bSetFocus != 0 {
                    events.push(VtInputEvent::FocusGained);
                } else {
                    events.push(VtInputEvent::FocusLost);
                }
            }
            MOUSE_EVENT => {
                // With VT input enabled, mouse events arrive as VT sequences
                // in key events (SGR format). Raw MOUSE_EVENT records are not
                // produced when VT input mode is active.
            }
            _ => {
                // Menu events and other unhandled types — ignore
            }
        }
    }

    // Flush remaining VT bytes
    if !vt_bytes.is_empty() {
        events.push(VtInputEvent::VtBytes(vt_bytes));
    }

    Ok(events)
}

/// Wait for console input to become available, with a timeout.
///
/// Returns `true` if input is available, `false` if the timeout expired.
pub fn poll_vt_input(timeout: Duration) -> io::Result<bool> {
    let handle = unsafe { GetStdHandle(STD_INPUT_HANDLE) };
    let timeout_ms = timeout.as_millis().min(u32::MAX as u128) as u32;
    let result = unsafe { WaitForSingleObject(handle, timeout_ms) };
    Ok(result == WAIT_OBJECT_0)
}
