//! Server-side input parsing
//!
//! Parses raw bytes from the client into crossterm events.
//! This allows the server to handle all input parsing, keeping the client ultra-light.

use crossterm::event::{
    Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
};

/// Parser state for incremental input parsing
#[derive(Debug, Default)]
pub struct InputParser {
    /// Buffer for incomplete escape sequences
    buffer: Vec<u8>,
    /// Maximum buffer size before we give up on an escape sequence
    max_buffer_size: usize,
}

impl InputParser {
    pub fn new() -> Self {
        Self {
            buffer: Vec::with_capacity(32),
            max_buffer_size: 256,
        }
    }

    /// Parse input bytes and return any complete events
    pub fn parse(&mut self, bytes: &[u8]) -> Vec<Event> {
        let mut events = Vec::new();

        for &byte in bytes {
            self.buffer.push(byte);

            // Try to parse the buffer
            match self.try_parse() {
                ParseResult::Complete(event) => {
                    events.push(event);
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

            // Focus events
            b'I' => ParseResult::Complete(Event::FocusGained),
            b'O' => ParseResult::Complete(Event::FocusLost),

            _ => ParseResult::Invalid,
        }
    }

    /// Parse tilde sequences: CSI number ~
    fn parse_tilde_sequence(&self, params: &[u8]) -> ParseResult {
        let (num, modifiers) = self.parse_num_and_modifiers(params);

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
        b if b >= 32 && b < 127 => KeyCode::Char(b as char),
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
}
