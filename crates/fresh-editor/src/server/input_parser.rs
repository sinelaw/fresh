//! Server-side input parsing
//!
//! Parses raw bytes from the client into crossterm events.
//! This allows the server to handle all input parsing, keeping the client ultra-light.

use crossterm::event::{
    Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
};

/// Parser state for incremental input parsing
#[derive(Debug)]
pub struct InputParser {
    /// Buffer for incomplete escape sequences
    buffer: Vec<u8>,
    /// Maximum buffer size before we give up on an escape sequence
    max_buffer_size: usize,
    /// When the buffer last received a byte (for ESC timeout)
    /// Buffer for bracketed paste content (between \x1b[200~ and \x1b[201~)
    paste_buffer: Option<Vec<u8>>,
}

impl Default for InputParser {
    fn default() -> Self {
        Self::new()
    }
}

impl InputParser {
    pub fn new() -> Self {
        Self {
            buffer: Vec::with_capacity(32),
            max_buffer_size: 256,
            paste_buffer: None,
        }
    }

    /// Suggests a timeout for the next input read (matching Microsoft Edit).
    ///
    /// Returns 100ms if the parser has a buffered ESC (might be standalone
    /// Escape or the start of an escape sequence). Returns `Duration::MAX`
    /// otherwise (no timeout needed — wait indefinitely for input).
    ///
    /// Parse input bytes and return any complete events.
    ///
    /// If called with empty input and the buffer contains a standalone ESC,
    /// emits it as an Escape key event (matching Microsoft Edit's pattern:
    /// timeout expired, no more input coming, so the ESC is standalone).
    pub fn parse(&mut self, bytes: &[u8]) -> Vec<Event> {
        let mut events = Vec::new();

        if !bytes.is_empty() || !self.buffer.is_empty() {
            tracing::trace!(
                "InputParser.parse: input={} bytes, buffer={} bytes ({:02x?})",
                bytes.len(),
                self.buffer.len(),
                &self.buffer,
            );
        }

        // If buffer has a lone ESC and new bytes arrived, the next byte
        // disambiguates: `[` means CSI sequence, anything else means the
        // ESC was standalone. We never flush ESC on timeout — we always
        // wait for the next byte. This prevents the bug where a mouse
        // sequence split across ReadConsoleInput batches at the \x1b
        // boundary gets its ESC flushed as standalone, causing the
        // continuation `[<35;...M` to be dumped as literal text.
        //
        // Empty input (timeout) is a no-op when ESC is buffered — the
        // ESC stays in the buffer until real bytes arrive.
        if bytes.is_empty() {
            return events;
        }

        for &byte in bytes {
            // If we're inside a bracketed paste, buffer bytes until end marker
            if let Some(ref mut paste_buf) = self.paste_buffer {
                paste_buf.push(byte);
                // Check for end marker: \x1b[201~
                if paste_buf.len() >= 6 && paste_buf.ends_with(b"\x1b[201~") {
                    // Remove the end marker from the paste content
                    let content_len = paste_buf.len() - 6;
                    let text = String::from_utf8_lossy(&paste_buf[..content_len]).into_owned();
                    self.paste_buffer = None;
                    events.push(Event::Paste(text));
                }
                continue;
            }

            self.buffer.push(byte);

            // Try to parse the buffer
            match self.try_parse() {
                ParseResult::Complete(event) => {
                    events.push(event);
                    self.buffer.clear();
                }
                ParseResult::PasteStart => {
                    // Enter bracketed paste mode
                    self.paste_buffer = Some(Vec::new());
                    self.buffer.clear();
                }
                ParseResult::Incomplete => {
                    // Need more bytes
                    if self.buffer.len() > self.max_buffer_size {
                        // Buffer too large, discard and treat as raw bytes
                        for &b in &self.buffer {
                            if let Some(event) = self.byte_to_event(b) {
                                events.push(event);
                            }
                        }
                        self.buffer.clear();
                    }
                }
                ParseResult::Invalid => {
                    // Invalid sequence, treat first byte as raw and retry rest
                    tracing::trace!(
                        "InputParser: Invalid sequence, buffer={:02x?}",
                        &self.buffer,
                    );
                    if !self.buffer.is_empty() {
                        let first = self.buffer[0];
                        if let Some(event) = self.byte_to_event(first) {
                            events.push(event);
                        }
                        let rest: Vec<u8> = self.buffer[1..].to_vec();
                        self.buffer.clear();
                        // Re-parse the rest
                        events.extend(self.parse(&rest));
                    }
                }
            }
        }

        events
    }

    /// Try to parse the current buffer
    fn try_parse(&self) -> ParseResult {
        if self.buffer.is_empty() {
            return ParseResult::Incomplete;
        }

        let bytes = &self.buffer;

        // Check for escape sequences
        if bytes[0] == 0x1b {
            return self.parse_escape_sequence();
        }

        // Single byte - convert directly
        if let Some(event) = self.byte_to_event(bytes[0]) {
            return ParseResult::Complete(event);
        }

        ParseResult::Invalid
    }

    /// Parse an escape sequence
    fn parse_escape_sequence(&self) -> ParseResult {
        let bytes = &self.buffer;

        if bytes.len() < 2 {
            return ParseResult::Incomplete;
        }

        match bytes[1] {
            // CSI sequences: ESC [
            b'[' => self.parse_csi_sequence(),
            // SS3 sequences: ESC O (function keys on some terminals)
            b'O' => self.parse_ss3_sequence(),
            // ESC followed by another ESC: the first is standalone Escape,
            // the second starts a new escape sequence. Return Invalid so the
            // first byte is emitted as Escape and the second \x1b is re-parsed.
            0x1b => ParseResult::Invalid,
            // Alt + key: ESC + key
            _ => {
                let key = bytes[1];
                let event = Event::Key(KeyEvent::new(byte_to_keycode(key), KeyModifiers::ALT));
                ParseResult::Complete(event)
            }
        }
    }

    /// Parse CSI (Control Sequence Introducer) sequence: ESC [ ...
    fn parse_csi_sequence(&self) -> ParseResult {
        let bytes = &self.buffer;

        if bytes.len() < 3 {
            return ParseResult::Incomplete;
        }

        // Find the final byte (0x40-0x7E)
        let final_idx = bytes[2..].iter().position(|&b| (0x40..=0x7E).contains(&b));

        match final_idx {
            None => {
                // Check if we have parameter bytes (0x30-0x3F) or intermediate bytes (0x20-0x2F)
                let all_valid = bytes[2..].iter().all(|&b| (0x20..=0x3F).contains(&b));
                if all_valid {
                    ParseResult::Incomplete
                } else {
                    ParseResult::Invalid
                }
            }
            Some(idx) => {
                let final_byte = bytes[2 + idx];
                let params = &bytes[2..2 + idx];

                self.parse_csi_final(params, final_byte)
            }
        }
    }

    /// Parse CSI sequence with final byte
    fn parse_csi_final(&self, params: &[u8], final_byte: u8) -> ParseResult {
        match final_byte {
            // Cursor keys
            b'A' => ParseResult::Complete(Event::Key(KeyEvent::new(
                KeyCode::Up,
                self.parse_modifiers(params),
            ))),
            b'B' => ParseResult::Complete(Event::Key(KeyEvent::new(
                KeyCode::Down,
                self.parse_modifiers(params),
            ))),
            b'C' => ParseResult::Complete(Event::Key(KeyEvent::new(
                KeyCode::Right,
                self.parse_modifiers(params),
            ))),
            b'D' => ParseResult::Complete(Event::Key(KeyEvent::new(
                KeyCode::Left,
                self.parse_modifiers(params),
            ))),
            b'H' => ParseResult::Complete(Event::Key(KeyEvent::new(
                KeyCode::Home,
                self.parse_modifiers(params),
            ))),
            b'F' => ParseResult::Complete(Event::Key(KeyEvent::new(
                KeyCode::End,
                self.parse_modifiers(params),
            ))),

            // Special keys with tilde
            b'~' => self.parse_tilde_sequence(params),

            // Mouse events (SGR format): CSI < Cb ; Cx ; Cy M/m
            b'M' | b'm' => {
                if !params.is_empty() && params[0] == b'<' {
                    self.parse_sgr_mouse(params, final_byte == b'M')
                } else {
                    // X10 mouse format
                    self.parse_x10_mouse()
                }
            }

            // Shift+Tab (Back Tab): CSI Z
            b'Z' => ParseResult::Complete(Event::Key(KeyEvent::new(
                KeyCode::BackTab,
                KeyModifiers::SHIFT,
            ))),

            // Focus events
            b'I' => ParseResult::Complete(Event::FocusGained),
            b'O' => ParseResult::Complete(Event::FocusLost),

            // CSI u (fixterms / kitty keyboard protocol): CSI keycode ; modifiers u
            b'u' => self.parse_csi_u_sequence(params),

            _ => ParseResult::Invalid,
        }
    }

    /// Parse tilde sequences: CSI number ~
    fn parse_tilde_sequence(&self, params: &[u8]) -> ParseResult {
        let params_str = std::str::from_utf8(params).unwrap_or("");
        let parts: Vec<&str> = params_str.split(';').collect();

        // xterm modifyOtherKeys mode 2: CSI 27 ; modifier ; keycode ~
        if parts.len() == 3 && parts[0] == "27" {
            let mods_param: u8 = parts[1].parse().unwrap_or(1);
            let codepoint: u32 = parts[2].parse().unwrap_or(0);
            let modifiers = modifiers_from_param(mods_param);

            let keycode = match codepoint {
                9 => KeyCode::Tab,
                13 => KeyCode::Enter,
                27 => KeyCode::Esc,
                127 => KeyCode::Backspace,
                cp => match char::from_u32(cp) {
                    Some(c) => KeyCode::Char(c),
                    None => return ParseResult::Invalid,
                },
            };

            return ParseResult::Complete(Event::Key(KeyEvent::new(keycode, modifiers)));
        }

        let (num, modifiers) = self.parse_num_and_modifiers(params);

        // Bracketed paste start: CSI 200 ~
        if num == 200 {
            return ParseResult::PasteStart;
        }

        // Bracketed paste end: CSI 201 ~ (shouldn't appear outside paste mode,
        // but handle gracefully by ignoring)
        if num == 201 {
            return ParseResult::Complete(Event::Key(KeyEvent::new(
                KeyCode::Null,
                KeyModifiers::empty(),
            )));
        }

        let keycode = match num {
            1 => KeyCode::Home,
            2 => KeyCode::Insert,
            3 => KeyCode::Delete,
            4 => KeyCode::End,
            5 => KeyCode::PageUp,
            6 => KeyCode::PageDown,
            7 => KeyCode::Home,
            8 => KeyCode::End,
            11 => KeyCode::F(1),
            12 => KeyCode::F(2),
            13 => KeyCode::F(3),
            14 => KeyCode::F(4),
            15 => KeyCode::F(5),
            17 => KeyCode::F(6),
            18 => KeyCode::F(7),
            19 => KeyCode::F(8),
            20 => KeyCode::F(9),
            21 => KeyCode::F(10),
            23 => KeyCode::F(11),
            24 => KeyCode::F(12),
            _ => return ParseResult::Invalid,
        };

        ParseResult::Complete(Event::Key(KeyEvent::new(keycode, modifiers)))
    }

    /// Parse CSI u (fixterms / kitty keyboard protocol): CSI keycode ; modifiers u
    ///
    /// The keycode is a Unicode codepoint. Special codepoints map to functional
    /// keys (Enter, Tab, Esc, Backspace, etc.); printable codepoints map to
    /// Char. Modifiers use the same encoding as standard CSI sequences.
    fn parse_csi_u_sequence(&self, params: &[u8]) -> ParseResult {
        let params_str = std::str::from_utf8(params).unwrap_or("");
        let parts: Vec<&str> = params_str.split(';').collect();

        let codepoint: u32 = parts.first().and_then(|s| s.parse().ok()).unwrap_or(0);
        let mods_param: u8 = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(1);
        let modifiers = modifiers_from_param(mods_param);

        let keycode = match codepoint {
            9 => KeyCode::Tab,
            13 => KeyCode::Enter,
            27 => KeyCode::Esc,
            127 => KeyCode::Backspace,
            cp => match char::from_u32(cp) {
                Some(c) => KeyCode::Char(c),
                None => return ParseResult::Invalid,
            },
        };

        ParseResult::Complete(Event::Key(KeyEvent::new(keycode, modifiers)))
    }

    /// Parse SGR mouse format: CSI < Cb ; Cx ; Cy M/m
    fn parse_sgr_mouse(&self, params: &[u8], pressed: bool) -> ParseResult {
        // Skip the '<'
        let params_str = std::str::from_utf8(&params[1..]).unwrap_or("");
        let parts: Vec<&str> = params_str.split(';').collect();

        if parts.len() != 3 {
            return ParseResult::Invalid;
        }

        let cb: u16 = parts[0].parse().unwrap_or(0);
        let cx: u16 = parts[1].parse().unwrap_or(1);
        let cy: u16 = parts[2].parse().unwrap_or(1);

        let button_bits = cb & 0b11;
        let button = match button_bits {
            0 => MouseButton::Left,
            1 => MouseButton::Middle,
            2 => MouseButton::Right,
            _ => MouseButton::Left, // 3 = no button (for motion)
        };

        let modifiers = KeyModifiers::from_bits_truncate(
            if cb & 4 != 0 {
                KeyModifiers::SHIFT.bits()
            } else {
                0
            } | if cb & 8 != 0 {
                KeyModifiers::ALT.bits()
            } else {
                0
            } | if cb & 16 != 0 {
                KeyModifiers::CONTROL.bits()
            } else {
                0
            },
        );

        let kind = if cb & 32 != 0 {
            // Motion event
            if cb & 64 != 0 {
                // Scroll while moving (unusual)
                if cb & 1 != 0 {
                    MouseEventKind::ScrollDown
                } else {
                    MouseEventKind::ScrollUp
                }
            } else if button_bits == 3 {
                // Motion with no button pressed (hover)
                MouseEventKind::Moved
            } else {
                // Motion with button pressed (drag)
                MouseEventKind::Drag(button)
            }
        } else if cb & 64 != 0 {
            // Scroll
            if cb & 1 != 0 {
                MouseEventKind::ScrollDown
            } else {
                MouseEventKind::ScrollUp
            }
        } else if pressed {
            MouseEventKind::Down(button)
        } else {
            MouseEventKind::Up(button)
        };

        ParseResult::Complete(Event::Mouse(MouseEvent {
            kind,
            column: cx.saturating_sub(1),
            row: cy.saturating_sub(1),
            modifiers,
        }))
    }

    /// Parse X10 mouse format (legacy)
    fn parse_x10_mouse(&self) -> ParseResult {
        let bytes = &self.buffer;

        if bytes.len() < 6 {
            return ParseResult::Incomplete;
        }

        let cb = bytes[3].wrapping_sub(32);
        let cx = bytes[4].wrapping_sub(32);
        let cy = bytes[5].wrapping_sub(32);

        let button = match cb & 0b11 {
            0 => MouseButton::Left,
            1 => MouseButton::Middle,
            2 => MouseButton::Right,
            3 => {
                // Release
                return ParseResult::Complete(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Up(MouseButton::Left),
                    column: cx as u16,
                    row: cy as u16,
                    modifiers: KeyModifiers::empty(),
                }));
            }
            _ => MouseButton::Left,
        };

        ParseResult::Complete(Event::Mouse(MouseEvent {
            kind: MouseEventKind::Down(button),
            column: cx as u16,
            row: cy as u16,
            modifiers: KeyModifiers::empty(),
        }))
    }

    /// Parse SS3 sequence: ESC O ...
    fn parse_ss3_sequence(&self) -> ParseResult {
        let bytes = &self.buffer;

        if bytes.len() < 3 {
            return ParseResult::Incomplete;
        }

        let keycode = match bytes[2] {
            b'P' => KeyCode::F(1),
            b'Q' => KeyCode::F(2),
            b'R' => KeyCode::F(3),
            b'S' => KeyCode::F(4),
            b'A' => KeyCode::Up,
            b'B' => KeyCode::Down,
            b'C' => KeyCode::Right,
            b'D' => KeyCode::Left,
            b'H' => KeyCode::Home,
            b'F' => KeyCode::End,
            _ => return ParseResult::Invalid,
        };

        ParseResult::Complete(Event::Key(KeyEvent::new(keycode, KeyModifiers::empty())))
    }

    /// Parse modifiers from CSI parameters
    fn parse_modifiers(&self, params: &[u8]) -> KeyModifiers {
        // Format: [num;modifiers] where modifiers = 1 + (shift) + 2*(alt) + 4*(ctrl)
        let params_str = std::str::from_utf8(params).unwrap_or("");
        if let Some(idx) = params_str.find(';') {
            if let Ok(mods) = params_str[idx + 1..].parse::<u8>() {
                return modifiers_from_param(mods);
            }
        }
        KeyModifiers::empty()
    }

    /// Parse number and modifiers from CSI parameters
    fn parse_num_and_modifiers(&self, params: &[u8]) -> (u8, KeyModifiers) {
        let params_str = std::str::from_utf8(params).unwrap_or("");
        let parts: Vec<&str> = params_str.split(';').collect();

        let num = parts.first().and_then(|s| s.parse().ok()).unwrap_or(0);
        let mods = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(1);

        (num, modifiers_from_param(mods))
    }

    /// Convert a single byte to an event
    fn byte_to_event(&self, byte: u8) -> Option<Event> {
        let keycode = byte_to_keycode(byte);
        let modifiers = if byte < 32 && byte != 9 && byte != 10 && byte != 13 && byte != 27 {
            // Control character (but not Tab, LF, CR, or Esc)
            KeyModifiers::CONTROL
        } else {
            KeyModifiers::empty()
        };

        Some(Event::Key(KeyEvent::new(keycode, modifiers)))
    }
}

/// Result of trying to parse the buffer
enum ParseResult {
    /// Successfully parsed a complete event
    Complete(Event),
    /// Bracketed paste start marker detected (\x1b[200~)
    PasteStart,
    /// Need more bytes to complete the sequence
    Incomplete,
    /// Invalid sequence
    Invalid,
}

/// Convert a byte to a KeyCode
fn byte_to_keycode(byte: u8) -> KeyCode {
    match byte {
        0 => KeyCode::Char('@'), // Ctrl+@
        9 => KeyCode::Tab,
        10 | 13 => KeyCode::Enter,                          // LF or CR
        1..=26 => KeyCode::Char((b'a' + byte - 1) as char), // Ctrl+A through Ctrl+Z
        27 => KeyCode::Esc,
        28..=31 => KeyCode::Char((b'\\' + byte - 28) as char),
        32 => KeyCode::Char(' '),
        127 => KeyCode::Backspace,
        b if (32..127).contains(&b) => KeyCode::Char(b as char),
        _ => KeyCode::Null,
    }
}

/// Convert modifier parameter to KeyModifiers
fn modifiers_from_param(param: u8) -> KeyModifiers {
    let param = param.saturating_sub(1);
    KeyModifiers::from_bits_truncate(
        if param & 1 != 0 {
            KeyModifiers::SHIFT.bits()
        } else {
            0
        } | if param & 2 != 0 {
            KeyModifiers::ALT.bits()
        } else {
            0
        } | if param & 4 != 0 {
            KeyModifiers::CONTROL.bits()
        } else {
            0
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_characters() {
        let mut parser = InputParser::new();
        let events = parser.parse(b"abc");
        assert_eq!(events.len(), 3);
        match &events[0] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Char('a')),
            _ => panic!("Expected key event"),
        }
    }

    #[test]
    fn test_control_characters_have_ctrl_modifier() {
        let mut parser = InputParser::new();
        // Ctrl+C = 0x03
        let events = parser.parse(&[0x03]);
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Char('c'));
                assert!(ke.modifiers.contains(KeyModifiers::CONTROL));
            }
            _ => panic!("Expected key event"),
        }
    }

    #[test]
    fn test_escape_buffers_until_complete() {
        let mut parser = InputParser::new();
        // ESC alone should buffer
        assert!(parser.parse(&[0x1b]).is_empty());
        // Adding more should still buffer
        assert!(parser.parse(b"[").is_empty());
        // Final byte completes the sequence
        let events = parser.parse(b"A");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Up),
            _ => panic!("Expected Up key"),
        }
    }

    #[test]
    fn test_csi_sequences_parse_arrow_keys() {
        let mut parser = InputParser::new();
        // CSI format: ESC [ <final>
        let events = parser.parse(b"\x1b[A");
        match &events[0] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Up),
            _ => panic!("Expected key event"),
        }
    }

    #[test]
    fn test_ss3_sequences_parse_function_keys() {
        let mut parser = InputParser::new();
        // SS3 format: ESC O <letter>
        let events = parser.parse(b"\x1bOP");
        match &events[0] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::F(1)),
            _ => panic!("Expected key event"),
        }
    }

    #[test]
    fn test_alt_key_via_esc_prefix() {
        let mut parser = InputParser::new();
        // Alt+a: ESC a (ESC followed by non-sequence char)
        let events = parser.parse(b"\x1ba");
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Char('a'));
                assert!(ke.modifiers.contains(KeyModifiers::ALT));
            }
            _ => panic!("Expected key event"),
        }
    }

    #[test]
    fn test_csi_modifiers_parsed_correctly() {
        let mut parser = InputParser::new();
        // Shift+Up: ESC [ 1 ; 2 A (2 = shift)
        let events = parser.parse(b"\x1b[1;2A");
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Up);
                assert!(ke.modifiers.contains(KeyModifiers::SHIFT));
            }
            _ => panic!("Expected Shift+Up"),
        }
    }

    #[test]
    fn test_sgr_mouse_events_are_1_indexed() {
        let mut parser = InputParser::new();
        // SGR mouse: CSI < button ; x ; y M
        // Terminal sends 1-indexed, we convert to 0-indexed
        let events = parser.parse(b"\x1b[<0;10;5M");
        match &events[0] {
            Event::Mouse(me) => {
                assert_eq!(me.column, 9); // 10-1
                assert_eq!(me.row, 4); // 5-1
            }
            _ => panic!("Expected mouse event"),
        }
    }

    #[test]
    fn test_focus_events() {
        let mut parser = InputParser::new();
        let events = parser.parse(b"\x1b[I");
        assert!(matches!(&events[0], Event::FocusGained));

        let events = parser.parse(b"\x1b[O");
        assert!(matches!(&events[0], Event::FocusLost));
    }

    #[test]
    fn test_mixed_input_preserves_order() {
        let mut parser = InputParser::new();
        let events = parser.parse(b"a\x1b[Ab");
        assert_eq!(events.len(), 3);
        // Order: 'a', Up, 'b'
        assert!(matches!(&events[0], Event::Key(ke) if ke.code == KeyCode::Char('a')));
        assert!(matches!(&events[1], Event::Key(ke) if ke.code == KeyCode::Up));
        assert!(matches!(&events[2], Event::Key(ke) if ke.code == KeyCode::Char('b')));
    }

    #[test]
    fn test_enter_key() {
        let mut parser = InputParser::new();
        // CR (carriage return) = 0x0D = 13
        let events = parser.parse(&[0x0D]);
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Enter);
                assert!(ke.modifiers.is_empty());
            }
            _ => panic!("Expected Enter key event"),
        }

        // LF (line feed) = 0x0A = 10
        let events = parser.parse(&[0x0A]);
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Enter);
                assert!(ke.modifiers.is_empty());
            }
            _ => panic!("Expected Enter key event"),
        }
    }

    #[test]
    fn test_tab_key() {
        let mut parser = InputParser::new();
        // Tab = 0x09 = 9
        let events = parser.parse(&[0x09]);
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Tab);
                assert!(ke.modifiers.is_empty());
            }
            _ => panic!("Expected Tab key event"),
        }
    }

    #[test]
    fn test_mouse_motion_without_button() {
        let mut parser = InputParser::new();
        // SGR mouse motion with no button: CSI < 35 ; x ; y M
        // 35 = 32 (motion) + 3 (no button)
        let events = parser.parse(b"\x1b[<35;10;5M");
        match &events[0] {
            Event::Mouse(me) => {
                assert!(matches!(me.kind, MouseEventKind::Moved));
                assert_eq!(me.column, 9); // 10 - 1 (0-indexed)
                assert_eq!(me.row, 4); // 5 - 1 (0-indexed)
            }
            _ => panic!("Expected mouse motion event"),
        }
    }

    // ---- Regression tests for issue #1089 ----

    #[test]
    fn test_shift_tab_csi_z() {
        let mut parser = InputParser::new();
        // Shift+Tab sends CSI Z = ESC [ Z
        let events = parser.parse(b"\x1b[Z");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::BackTab);
                assert!(ke.modifiers.contains(KeyModifiers::SHIFT));
            }
            _ => panic!("Expected BackTab key event, got {:?}", events[0]),
        }
    }

    #[test]
    fn test_esc_then_mouse_event_same_chunk() {
        let mut parser = InputParser::new();
        // User presses Escape, then moves mouse. Both arrive in one chunk:
        // ESC (0x1b) followed by mouse event ESC [ < 35 ; 67 ; 18 M
        let events = parser.parse(b"\x1b\x1b[<35;67;18M");
        assert_eq!(
            events.len(),
            2,
            "Expected Escape + mouse event, got: {:?}",
            events
        );

        // First event: standalone Escape
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Esc);
                assert!(ke.modifiers.is_empty());
            }
            _ => panic!("Expected Esc key event, got {:?}", events[0]),
        }

        // Second event: mouse motion
        match &events[1] {
            Event::Mouse(me) => {
                assert!(matches!(me.kind, MouseEventKind::Moved));
                assert_eq!(me.column, 66); // 67 - 1
                assert_eq!(me.row, 17); // 18 - 1
            }
            _ => panic!("Expected mouse motion event, got {:?}", events[1]),
        }
    }

    #[test]
    fn test_esc_then_mouse_event_separate_chunks() {
        let mut parser = InputParser::new();

        // First chunk: standalone ESC (buffered, waiting for more bytes)
        let events = parser.parse(&[0x1b]);
        assert!(events.is_empty(), "ESC should be buffered");

        // Second chunk: mouse event arrives later
        let events = parser.parse(b"\x1b[<35;67;18M");
        assert_eq!(
            events.len(),
            2,
            "Expected Escape + mouse event, got: {:?}",
            events
        );

        // First event: standalone Escape (disambiguated by seeing another ESC)
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Esc);
                assert!(ke.modifiers.is_empty());
            }
            _ => panic!("Expected Esc key event, got {:?}", events[0]),
        }

        // Second event: mouse motion
        match &events[1] {
            Event::Mouse(me) => {
                assert!(matches!(me.kind, MouseEventKind::Moved));
            }
            _ => panic!("Expected mouse motion event, got {:?}", events[1]),
        }
    }

    #[test]
    fn test_esc_then_csi_arrow_separate_chunks() {
        let mut parser = InputParser::new();

        // ESC buffered
        let events = parser.parse(&[0x1b]);
        assert!(events.is_empty());

        // Arrow key sequence arrives (starts with another ESC)
        let events = parser.parse(b"\x1b[A");
        assert_eq!(events.len(), 2, "Expected Escape + Up, got: {:?}", events);

        match &events[0] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Esc),
            _ => panic!("Expected Esc"),
        }
        match &events[1] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Up),
            _ => panic!("Expected Up"),
        }
    }

    #[test]
    fn test_esc_waits_for_next_byte() {
        let mut parser = InputParser::new();

        // ESC buffered
        let events = parser.parse(&[0x1b]);
        assert!(events.is_empty());

        // Buffer still has the ESC
        assert_eq!(parser.buffer.len(), 1);

        // Next byte `[` disambiguates: it's a CSI sequence, not standalone ESC
        let events = parser.parse(b"[A");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Up),
            _ => panic!("Expected Up"),
        }
    }

    #[test]
    fn test_esc_then_printable_byte_emits_alt_key() {
        let mut parser = InputParser::new();

        // ESC buffered
        let events = parser.parse(&[0x1b]);
        assert!(events.is_empty());

        // Next byte `a` completes the sequence as Alt+a (standard terminal behavior)
        let events = parser.parse(b"a");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Char('a'));
                assert!(ke.modifiers.contains(KeyModifiers::ALT));
            }
            _ => panic!("Expected Alt+a"),
        }
    }

    #[test]
    fn test_esc_then_esc_emits_standalone_esc() {
        let mut parser = InputParser::new();

        // First ESC buffered
        let events = parser.parse(&[0x1b]);
        assert!(events.is_empty());

        // Second ESC: first ESC is standalone, second starts new sequence
        let events = parser.parse(&[0x1b]);
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Esc);
                assert!(ke.modifiers.is_empty());
            }
            _ => panic!("Expected standalone Esc"),
        }
        // Second ESC still buffered
        assert_eq!(parser.buffer, vec![0x1b]);
    }

    #[test]
    fn test_split_mouse_sequence_across_batches() {
        let mut parser = InputParser::new();

        // Batch 1: just the ESC byte (split at batch boundary)
        let events = parser.parse(&[0x1b]);
        assert!(events.is_empty());
        assert_eq!(parser.buffer.len(), 1);

        // Batch 2: rest of mouse sequence arrives
        let events = parser.parse(b"[<35;42;5M");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Mouse(_) => {} // Mouse event parsed correctly
            other => panic!("Expected mouse event, got {:?}", other),
        }
    }

    #[test]
    fn test_partial_csi_sequence_not_flushed() {
        let mut parser = InputParser::new();

        // Partial CSI mouse sequence (split across batches)
        let events = parser.parse(b"\x1b[<35;");
        assert!(events.is_empty());
        assert_eq!(parser.buffer.len(), 6);

        // Now the rest of the sequence arrives
        let events = parser.parse(b"42;5M");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Mouse(me) => {
                assert_eq!(me.column, 41); // 42 - 1 (1-indexed to 0-indexed)
                assert_eq!(me.row, 4); // 5 - 1
            }
            _ => panic!("Expected mouse event, got {:?}", events[0]),
        }
    }

    #[test]
    fn test_esc_then_mouse_click() {
        let mut parser = InputParser::new();
        // ESC followed by mouse button press: ESC [ < 0 ; 10 ; 5 M
        let events = parser.parse(b"\x1b\x1b[<0;10;5M");
        assert_eq!(
            events.len(),
            2,
            "Expected Escape + mouse click, got: {:?}",
            events
        );

        match &events[0] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Esc),
            _ => panic!("Expected Esc"),
        }
        match &events[1] {
            Event::Mouse(me) => {
                assert!(matches!(me.kind, MouseEventKind::Down(MouseButton::Left)));
            }
            _ => panic!("Expected mouse down event, got {:?}", events[1]),
        }
    }

    // ---- Bracketed paste tests ----

    #[test]
    fn test_bracketed_paste_simple() {
        let mut parser = InputParser::new();
        // Bracketed paste: \x1b[200~ ... \x1b[201~
        let events = parser.parse(b"\x1b[200~Hello, world!\x1b[201~");
        assert_eq!(events.len(), 1, "Expected 1 paste event, got: {:?}", events);
        match &events[0] {
            Event::Paste(text) => assert_eq!(text, "Hello, world!"),
            _ => panic!("Expected Paste event, got {:?}", events[0]),
        }
    }

    #[test]
    fn test_bracketed_paste_with_newlines() {
        let mut parser = InputParser::new();
        let events = parser.parse(b"\x1b[200~line1\nline2\nline3\x1b[201~");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Paste(text) => assert_eq!(text, "line1\nline2\nline3"),
            _ => panic!("Expected Paste event"),
        }
    }

    #[test]
    fn test_bracketed_paste_split_across_chunks() {
        let mut parser = InputParser::new();

        // Start marker arrives
        let events = parser.parse(b"\x1b[200~Hello");
        assert!(events.is_empty(), "Paste not complete yet");

        // More content
        let events = parser.parse(b", world!");
        assert!(events.is_empty(), "Paste not complete yet");

        // End marker arrives
        let events = parser.parse(b"\x1b[201~");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Paste(text) => assert_eq!(text, "Hello, world!"),
            _ => panic!("Expected Paste event"),
        }
    }

    #[test]
    fn test_bracketed_paste_followed_by_keypress() {
        let mut parser = InputParser::new();
        // Paste followed by a regular keypress
        let events = parser.parse(b"\x1b[200~pasted\x1b[201~a");
        assert_eq!(
            events.len(),
            2,
            "Expected paste + key event, got: {:?}",
            events
        );
        match &events[0] {
            Event::Paste(text) => assert_eq!(text, "pasted"),
            _ => panic!("Expected Paste event"),
        }
        match &events[1] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Char('a')),
            _ => panic!("Expected key event"),
        }
    }

    #[test]
    fn test_bracketed_paste_empty() {
        let mut parser = InputParser::new();
        let events = parser.parse(b"\x1b[200~\x1b[201~");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Paste(text) => assert_eq!(text, ""),
            _ => panic!("Expected empty Paste event"),
        }
    }

    #[test]
    fn test_bracketed_paste_with_escape_sequences_inside() {
        let mut parser = InputParser::new();
        // Pasted text might contain escape sequences (e.g., colored text from another terminal)
        let events = parser.parse(b"\x1b[200~\x1b[31mred text\x1b[0m\x1b[201~");
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Paste(text) => assert_eq!(text, "\x1b[31mred text\x1b[0m"),
            _ => panic!("Expected Paste event with escape sequences"),
        }
    }

    #[test]
    fn test_keypress_then_bracketed_paste() {
        let mut parser = InputParser::new();
        let events = parser.parse(b"x\x1b[200~pasted\x1b[201~");
        assert_eq!(events.len(), 2);
        match &events[0] {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Char('x')),
            _ => panic!("Expected key event"),
        }
        match &events[1] {
            Event::Paste(text) => assert_eq!(text, "pasted"),
            _ => panic!("Expected Paste event"),
        }
    }
}
