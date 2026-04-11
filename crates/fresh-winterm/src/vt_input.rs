//! Windows VT input reader with corrupt sequence detection.
//!
//! Sets `ENABLE_VIRTUAL_TERMINAL_INPUT` on the console input handle and reads
//! `INPUT_RECORD` events via `ReadConsoleInputW`. Mouse events arrive as VT
//! SGR sequences in KEY_EVENT records (mode 1003 all-motion + 1006 SGR).
//!
//! Under heavy mouse movement, the Windows console sporadically drops the
//! ESC (0x1b) KEY_EVENT record from VT mouse sequences. The reader detects
//! these corrupted batches — a single ReadConsoleInputW call returning bytes
//! matching `[<digits;digits;digits[Mm]` without a leading ESC — and discards
//! them instead of forwarding as literal text.

use std::ffi::c_void;
use std::io;
use std::mem::MaybeUninit;
use std::ptr;
use std::time::Duration;

use windows_sys::Win32::Foundation::{INVALID_HANDLE_VALUE, WAIT_OBJECT_0, WAIT_TIMEOUT};
use windows_sys::Win32::System::Console::{
    GetConsoleMode, GetStdHandle, ReadConsoleInputW, SetConsoleMode, ENABLE_VIRTUAL_TERMINAL_INPUT,
    ENABLE_WINDOW_INPUT, FOCUS_EVENT, INPUT_RECORD, KEY_EVENT, STD_INPUT_HANDLE,
    WINDOW_BUFFER_SIZE_EVENT,
};
use windows_sys::Win32::System::Threading::WaitForSingleObject;

/// Enable VT input mode on the console input handle.
///
/// Sets `ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_WINDOW_INPUT`.
/// `ENABLE_MOUSE_INPUT` is NOT set — mouse events arrive as VT sequences
/// in KEY_EVENT records once VT mouse tracking is enabled via stdout.
///
/// Returns the previous console mode so it can be restored later.
pub fn enable_vt_input() -> io::Result<u32> {
    unsafe {
        let handle = GetStdHandle(STD_INPUT_HANDLE);
        if handle.is_null() || ptr::eq(handle, INVALID_HANDLE_VALUE) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Failed to get stdin handle",
            ));
        }

        let mut old_mode: u32 = 0;
        if GetConsoleMode(handle, &mut old_mode) == 0 {
            return Err(io::Error::last_os_error());
        }

        // Don't set ENABLE_EXTENDED_FLAGS — it disables Quick Edit mode,
        // which then stays disabled after exit if cleanup doesn't restore
        // it properly. Quick Edit doesn't interfere with VT mouse tracking.
        let new_mode = ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_WINDOW_INPUT;
        if SetConsoleMode(handle, new_mode) == 0 {
            return Err(io::Error::last_os_error());
        }

        tracing::debug!(
            "Enabled VT input mode (old=0x{:x}, new=0x{:x})",
            old_mode,
            new_mode,
        );

        Ok(old_mode)
    }
}

/// Enable mouse tracking and bracketed paste via VT escape sequences.
///
/// When `mode` is `AllMotion`, enables mode 1003 (all-motion) + 1006 (SGR) +
/// 2004 (bracketed paste). When `mode` is `CellMotion`, enables mode 1002
/// (cell-motion) + 1006 (SGR) + 2004 (bracketed paste) — lower event volume,
/// no hover/mousemove tracking.
pub fn enable_mouse_tracking(mode: MouseMode) -> io::Result<()> {
    use std::io::Write;
    let mut stdout = io::stdout();
    match mode {
        MouseMode::CellMotion => stdout.write_all(b"\x1b[?1002;1006h\x1b[?2004h")?,
        MouseMode::AllMotion => stdout.write_all(b"\x1b[?1003;1006h\x1b[?2004h")?,
    }
    stdout.flush()?;
    Ok(())
}

/// Mouse tracking mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MouseMode {
    /// Mode 1002: Cell Motion — reports button press, release, and drag.
    CellMotion,
    /// Mode 1003: All Motion — reports every mouse movement.
    AllMotion,
}

/// Switch mouse tracking mode at runtime.
pub fn set_mouse_mode(mode: MouseMode) -> io::Result<()> {
    use std::io::Write;
    let mut stdout = io::stdout();
    stdout.write_all(b"\x1b[?1002;1003l")?;
    match mode {
        MouseMode::CellMotion => stdout.write_all(b"\x1b[?1002;1006h")?,
        MouseMode::AllMotion => stdout.write_all(b"\x1b[?1003;1006h")?,
    }
    stdout.flush()?;
    tracing::debug!("Switched mouse tracking to {:?}", mode);
    Ok(())
}

/// Disable mouse tracking and bracketed paste.
pub fn disable_mouse_tracking() -> io::Result<()> {
    use std::io::Write;
    let mut stdout = io::stdout();
    stdout.write_all(b"\x1b[?1002;1003;1006l\x1b[?2004l")?;
    stdout.flush()?;
    Ok(())
}

/// Save the current console input mode.
///
/// Call this BEFORE any terminal setup (crossterm raw mode, enable_vt_input,
/// etc.) to capture the original state. Restore with `restore_console_mode`.
pub fn save_console_mode() -> u32 {
    unsafe {
        let handle = GetStdHandle(STD_INPUT_HANDLE);
        let mut mode: u32 = 0;
        GetConsoleMode(handle, &mut mode);
        mode
    }
}

/// Restore the console input mode to a previously saved value.
///
/// Call this AFTER all other terminal cleanup (crossterm disable_raw_mode,
/// DisableMouseCapture, etc.) to ensure Quick Edit mode and other flags
/// are properly restored.
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
    /// Window resize event
    Resize,
    /// Focus gained
    FocusGained,
    /// Focus lost
    FocusLost,
}

/// Threaded VT input reader with corrupt mouse sequence detection.
///
/// Under heavy mouse movement with mode 1003, the Windows console
/// sporadically drops the ESC KEY_EVENT record from VT mouse sequences.
/// The reader detects these corrupted batches and discards them.
pub struct VtInputReader {
    rx: std::sync::mpsc::Receiver<VtInputEvent>,
    /// Windows Event handle, signaled by the reader thread when data is queued.
    pub event_handle: *mut c_void,
    _thread: std::thread::JoinHandle<()>,
}

unsafe impl Send for VtInputReader {}
unsafe impl Sync for VtInputReader {}

impl VtInputReader {
    /// Spawn the background reader thread.
    pub fn spawn() -> Self {
        use windows_sys::Win32::System::Threading::CreateEventW;

        let (tx, rx) = std::sync::mpsc::channel();

        let event_handle = unsafe { CreateEventW(ptr::null(), 0, 0, ptr::null()) };
        assert!(!event_handle.is_null(), "CreateEventW failed");

        let thread_event = event_handle as usize;
        let thread = std::thread::Builder::new()
            .name("win-vt-input".into())
            .spawn(move || {
                Self::reader_loop(tx, thread_event as *mut c_void);
            })
            .expect("failed to spawn VT input reader thread");

        Self {
            rx,
            event_handle,
            _thread: thread,
        }
    }

    /// Try to receive an event with a timeout.
    pub fn poll(&self, timeout: Duration) -> Option<VtInputEvent> {
        self.rx.recv_timeout(timeout).ok()
    }

    /// Try to receive an event without blocking.
    pub fn try_recv(&self) -> Option<VtInputEvent> {
        self.rx.try_recv().ok()
    }

    /// Background reader loop — runs on dedicated thread.
    fn reader_loop(tx: std::sync::mpsc::Sender<VtInputEvent>, event_handle: *mut c_void) {
        let handle = unsafe { GetStdHandle(STD_INPUT_HANDLE) };
        let mut records: [MaybeUninit<INPUT_RECORD>; 16384] =
            [const { MaybeUninit::uninit() }; 16384];
        let mut surrogate_high: Option<u16> = None;
        let mut last_heartbeat = std::time::Instant::now();

        loop {
            // Periodic console mode heartbeat
            if last_heartbeat.elapsed() >= Duration::from_secs(30) {
                last_heartbeat = std::time::Instant::now();
                let desired_mode = ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_WINDOW_INPUT;
                unsafe {
                    let mut current_mode: u32 = 0;
                    if GetConsoleMode(handle, &mut current_mode) != 0
                        && current_mode != desired_mode
                    {
                        tracing::warn!(
                            "Console mode drifted (current=0x{:x}, expected=0x{:x}), re-asserting",
                            current_mode,
                            desired_mode,
                        );
                        SetConsoleMode(handle, desired_mode);
                    }
                }
            }

            let wait = unsafe { WaitForSingleObject(handle as _, 5000) };
            if wait == WAIT_TIMEOUT {
                continue;
            }
            if wait != WAIT_OBJECT_0 {
                break;
            }

            let mut count: u32 = 0;
            let ok = unsafe {
                ReadConsoleInputW(
                    handle,
                    records[0].as_mut_ptr(),
                    records.len() as u32,
                    &mut count,
                )
            };

            if ok == 0 || count == 0 {
                continue;
            }

            let mut vt_bytes = Vec::new();
            let mut structured_events: Vec<VtInputEvent> = Vec::new();

            for i in 0..count as usize {
                let rec = unsafe { records[i].assume_init_ref() };
                match rec.EventType as u32 {
                    KEY_EVENT => {
                        let key_event = unsafe { rec.Event.KeyEvent };
                        let ch = unsafe { key_event.uChar.UnicodeChar };
                        if key_event.bKeyDown != 0 && ch != 0 {
                            let repeat = (key_event.wRepeatCount as usize).max(1);
                            if (0xD800..=0xDBFF).contains(&ch) {
                                surrogate_high = Some(ch);
                            } else if (0xDC00..=0xDFFF).contains(&ch) {
                                if let Some(high) = surrogate_high.take() {
                                    if let Some(c) =
                                        char::decode_utf16([high, ch]).next().and_then(|r| r.ok())
                                    {
                                        let mut buf = [0u8; 4];
                                        let s = c.encode_utf8(&mut buf);
                                        for _ in 0..repeat {
                                            vt_bytes.extend_from_slice(s.as_bytes());
                                        }
                                    }
                                }
                            } else {
                                surrogate_high = None;
                                if let Some(c) = char::from_u32(ch as u32) {
                                    let mut buf = [0u8; 4];
                                    let s = c.encode_utf8(&mut buf);
                                    for _ in 0..repeat {
                                        vt_bytes.extend_from_slice(s.as_bytes());
                                    }
                                }
                            }
                        }
                    }
                    WINDOW_BUFFER_SIZE_EVENT => {
                        if !vt_bytes.is_empty() {
                            structured_events.push(VtInputEvent::VtBytes(strip_corrupt_mouse(
                                std::mem::take(&mut vt_bytes),
                            )));
                        }
                        structured_events.push(VtInputEvent::Resize);
                    }
                    FOCUS_EVENT => {
                        if !vt_bytes.is_empty() {
                            structured_events.push(VtInputEvent::VtBytes(strip_corrupt_mouse(
                                std::mem::take(&mut vt_bytes),
                            )));
                        }
                        let focus_event = unsafe { rec.Event.FocusEvent };
                        if focus_event.bSetFocus != 0 {
                            structured_events.push(VtInputEvent::FocusGained);
                        } else {
                            structured_events.push(VtInputEvent::FocusLost);
                        }
                    }
                    _ => {}
                }
            }

            // Strip corrupt mouse sequences before sending
            if !vt_bytes.is_empty() {
                vt_bytes = strip_corrupt_mouse(vt_bytes);
            }

            if !vt_bytes.is_empty() {
                if tx.send(VtInputEvent::VtBytes(vt_bytes)).is_err() {
                    return;
                }
            }
            for event in structured_events {
                if tx.send(event).is_err() {
                    return;
                }
            }

            unsafe { windows_sys::Win32::System::Threading::SetEvent(event_handle) };
        }
    }
}

impl Drop for VtInputReader {
    fn drop(&mut self) {
        unsafe { windows_sys::Win32::Foundation::CloseHandle(self.event_handle) };
    }
}

/// Strip corrupt mouse sequences from a VT byte buffer.
///
/// ## Why this exists (the full horror story)
///
/// With `ENABLE_VIRTUAL_TERMINAL_INPUT` and VT mouse tracking mode 1003
/// (all-motion), every mouse pixel movement generates an SGR escape sequence
/// like `\x1b[<35;120;45M` — approximately 15 KEY_EVENT records per movement.
/// Under heavy mouse movement, the Windows console (conhost.exe / Windows
/// Terminal) sporadically fails to write the ESC (0x1b) KEY_EVENT record into
/// the console input buffer, even when the buffer is nearly empty (confirmed
/// by raw INPUT_RECORD hex dumps showing 10-record batches starting with
/// `[<35;...M` instead of `\x1b[<35;...M`). Neovim avoids this by defaulting
/// to mode 1002 (cell-motion, far fewer events).
///
/// Approaches that did NOT fix the problem:
/// - Bulk reads (4096, 16384 records) — still drops
/// - One-at-a-time reads matching libuv — made it worse (more syscalls)
/// - Dropping ENABLE_EXTENDED_FLAGS — no change
/// - Using native ENABLE_MOUSE_INPUT instead of VT — terminal still sends
///   VT mouse sequences alongside native events (double delivery), and
///   disabling VT mouse tracking (\x1b[?1003l) also kills native mouse
/// - Increasing read buffer size — drops happen with <20 records pending
/// - Never flushing ESC on timeout in InputParser — doesn't help because
///   the ESC byte is never delivered by ReadConsoleInputW in the first place
///
/// The fix: detect and strip corrupt sequences. A corrupted mouse sequence
/// is `[<digits;digits;digits[Mm]` without a leading `\x1b`. These arrive
/// in a single ReadConsoleInputW batch (all KEY_EVENTs from one mouse
/// movement at once). A human typing `[<35;42;5M` would produce individual
/// keystrokes across many separate ReadConsoleInputW calls, so this pattern
/// in a single batch is unambiguously a corrupt mouse sequence.
///
/// See `docs/internal/windows-mouse-input.md` for the complete investigation.
fn strip_corrupt_mouse(mut bytes: Vec<u8>) -> Vec<u8> {
    let mut i = 0;
    while i < bytes.len() {
        // Look for `[<` not preceded by ESC
        if bytes[i] == b'['
            && i + 1 < bytes.len()
            && bytes[i + 1] == b'<'
            && (i == 0 || bytes[i - 1] != 0x1b)
        {
            // Check if the rest matches `[<digits;digits;digits[Mm]`
            if let Some(seq_len) = match_sgr_mouse(&bytes[i..]) {
                tracing::debug!(
                    "Stripped corrupt mouse sequence ({} bytes) at offset {}",
                    seq_len,
                    i,
                );
                bytes.drain(i..i + seq_len);
                continue; // Don't advance i — new content shifted into this position
            }
        }
        i += 1;
    }
    bytes
}

/// Check if bytes starting at `data` match `[<digits;digits;digits[Mm]`.
/// Returns the length of the match, or None if no match.
fn match_sgr_mouse(data: &[u8]) -> Option<usize> {
    if data.len() < 6 || data[0] != b'[' || data[1] != b'<' {
        return None;
    }

    let mut pos = 2;
    let mut semicolons = 0;

    // Parse digits and semicolons: expect `digits;digits;digits`
    while pos < data.len() {
        match data[pos] {
            b'0'..=b'9' => pos += 1,
            b';' => {
                semicolons += 1;
                pos += 1;
            }
            b'M' | b'm' => {
                // Must have exactly 2 semicolons (3 numeric fields)
                if semicolons == 2 {
                    return Some(pos + 1); // Include the M/m terminator
                }
                return None;
            }
            _ => return None,
        }
    }

    None // Incomplete sequence
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_corrupt_mouse_single() {
        // `[<35;42;5M` — corrupt mouse sequence (no ESC)
        let input = b"[<35;42;5M".to_vec();
        let result = strip_corrupt_mouse(input);
        assert!(result.is_empty());
    }

    #[test]
    fn test_strip_corrupt_mouse_preserves_valid() {
        // `\x1b[<35;42;5M` — valid mouse sequence (has ESC)
        let input = b"\x1b[<35;42;5M".to_vec();
        let result = strip_corrupt_mouse(input);
        assert_eq!(result, b"\x1b[<35;42;5M");
    }

    #[test]
    fn test_strip_corrupt_mouse_mixed() {
        // Valid sequence, then corrupt sequence
        let mut input = b"\x1b[<35;10;3M".to_vec();
        input.extend_from_slice(b"[<35;42;5M");
        let result = strip_corrupt_mouse(input);
        assert_eq!(result, b"\x1b[<35;10;3M");
    }

    #[test]
    fn test_strip_corrupt_mouse_multiple_corrupt() {
        // Multiple corrupt sequences
        let mut input = b"[<35;10;3M".to_vec();
        input.extend_from_slice(b"[<35;42;5M");
        let result = strip_corrupt_mouse(input);
        assert!(result.is_empty());
    }

    #[test]
    fn test_strip_preserves_normal_bracket() {
        // `[` followed by non-`<` — not a mouse sequence
        let input = b"[hello".to_vec();
        let result = strip_corrupt_mouse(input);
        assert_eq!(result, b"[hello");
    }

    #[test]
    fn test_strip_preserves_user_typed_bracket_lt() {
        // `[<` followed by non-digits — not a mouse sequence
        let input = b"[<hello".to_vec();
        let result = strip_corrupt_mouse(input);
        assert_eq!(result, b"[<hello");
    }
}
