//! PTY utilities using portable-pty
//!
//! This module provides PTY-related utilities and helpers.

use crossterm::event::{KeyCode, KeyModifiers};

/// Convert a crossterm key event to bytes to send to the PTY
///
/// This handles special keys and modifier combinations that need
/// to be sent as escape sequences or control characters.
pub fn key_to_pty_bytes(code: KeyCode, modifiers: KeyModifiers) -> Option<Vec<u8>> {
    let ctrl = modifiers.contains(KeyModifiers::CONTROL);
    let alt = modifiers.contains(KeyModifiers::ALT);
    let shift = modifiers.contains(KeyModifiers::SHIFT);

    // Handle Ctrl+key combinations (send as control characters)
    if ctrl && !alt {
        if let KeyCode::Char(c) = code {
            let c = c.to_ascii_lowercase();
            if c.is_ascii_lowercase() {
                // Ctrl+A = 0x01, Ctrl+B = 0x02, etc.
                let ctrl_char = (c as u8) - b'a' + 1;
                return Some(vec![ctrl_char]);
            }
            // Special Ctrl combinations
            match c {
                '[' | '3' => return Some(vec![0x1b]), // Escape
                '\\' | '4' => return Some(vec![0x1c]),
                ']' | '5' => return Some(vec![0x1d]),
                '^' | '6' => return Some(vec![0x1e]),
                '_' | '7' => return Some(vec![0x1f]),
                '@' | '2' => return Some(vec![0x00]), // NUL
                ' ' => return Some(vec![0x00]),       // Ctrl+Space = NUL
                '?' => return Some(vec![0x7f]),       // DEL
                _ => {}
            }
        }
    }

    // Handle Alt+key (send as ESC + key)
    if alt && !ctrl {
        if let KeyCode::Char(c) = code {
            let c = if shift { c.to_ascii_uppercase() } else { c };
            return Some(vec![0x1b, c as u8]);
        }
    }

    // Handle regular keys and special keys
    match code {
        KeyCode::Char(c) => {
            let c = if shift { c.to_ascii_uppercase() } else { c };
            let mut bytes = vec![0u8; 4];
            let len = c.encode_utf8(&mut bytes).len();
            bytes.truncate(len);
            Some(bytes)
        }
        KeyCode::Enter => Some(vec![b'\r']),
        KeyCode::Tab => {
            if shift {
                // Shift+Tab (backtab)
                Some(vec![0x1b, b'[', b'Z'])
            } else {
                Some(vec![b'\t'])
            }
        }
        KeyCode::Backspace => {
            if ctrl {
                // Ctrl+Backspace - delete word
                Some(vec![0x17]) // Ctrl+W
            } else {
                Some(vec![0x7f]) // DEL
            }
        }
        KeyCode::Delete => {
            if ctrl {
                Some(vec![0x1b, b'[', b'3', b';', b'5', b'~']) // Ctrl+Delete
            } else if shift {
                Some(vec![0x1b, b'[', b'3', b';', b'2', b'~']) // Shift+Delete
            } else {
                Some(vec![0x1b, b'[', b'3', b'~'])
            }
        }
        KeyCode::Esc => Some(vec![0x1b]),
        KeyCode::Up => {
            if ctrl {
                Some(vec![0x1b, b'[', b'1', b';', b'5', b'A'])
            } else if shift {
                Some(vec![0x1b, b'[', b'1', b';', b'2', b'A'])
            } else if alt {
                Some(vec![0x1b, b'[', b'1', b';', b'3', b'A'])
            } else {
                Some(vec![0x1b, b'[', b'A'])
            }
        }
        KeyCode::Down => {
            if ctrl {
                Some(vec![0x1b, b'[', b'1', b';', b'5', b'B'])
            } else if shift {
                Some(vec![0x1b, b'[', b'1', b';', b'2', b'B'])
            } else if alt {
                Some(vec![0x1b, b'[', b'1', b';', b'3', b'B'])
            } else {
                Some(vec![0x1b, b'[', b'B'])
            }
        }
        KeyCode::Right => {
            if ctrl {
                Some(vec![0x1b, b'[', b'1', b';', b'5', b'C'])
            } else if shift {
                Some(vec![0x1b, b'[', b'1', b';', b'2', b'C'])
            } else if alt {
                Some(vec![0x1b, b'[', b'1', b';', b'3', b'C'])
            } else {
                Some(vec![0x1b, b'[', b'C'])
            }
        }
        KeyCode::Left => {
            if ctrl {
                Some(vec![0x1b, b'[', b'1', b';', b'5', b'D'])
            } else if shift {
                Some(vec![0x1b, b'[', b'1', b';', b'2', b'D'])
            } else if alt {
                Some(vec![0x1b, b'[', b'1', b';', b'3', b'D'])
            } else {
                Some(vec![0x1b, b'[', b'D'])
            }
        }
        KeyCode::Home => {
            if ctrl {
                Some(vec![0x1b, b'[', b'1', b';', b'5', b'H'])
            } else {
                Some(vec![0x1b, b'[', b'H'])
            }
        }
        KeyCode::End => {
            if ctrl {
                Some(vec![0x1b, b'[', b'1', b';', b'5', b'F'])
            } else {
                Some(vec![0x1b, b'[', b'F'])
            }
        }
        KeyCode::PageUp => {
            if ctrl {
                Some(vec![0x1b, b'[', b'5', b';', b'5', b'~'])
            } else {
                Some(vec![0x1b, b'[', b'5', b'~'])
            }
        }
        KeyCode::PageDown => {
            if ctrl {
                Some(vec![0x1b, b'[', b'6', b';', b'5', b'~'])
            } else {
                Some(vec![0x1b, b'[', b'6', b'~'])
            }
        }
        KeyCode::Insert => Some(vec![0x1b, b'[', b'2', b'~']),
        KeyCode::F(n) => {
            // F1-F12 escape sequences
            let base = match n {
                1 => vec![0x1b, b'O', b'P'],
                2 => vec![0x1b, b'O', b'Q'],
                3 => vec![0x1b, b'O', b'R'],
                4 => vec![0x1b, b'O', b'S'],
                5 => vec![0x1b, b'[', b'1', b'5', b'~'],
                6 => vec![0x1b, b'[', b'1', b'7', b'~'],
                7 => vec![0x1b, b'[', b'1', b'8', b'~'],
                8 => vec![0x1b, b'[', b'1', b'9', b'~'],
                9 => vec![0x1b, b'[', b'2', b'0', b'~'],
                10 => vec![0x1b, b'[', b'2', b'1', b'~'],
                11 => vec![0x1b, b'[', b'2', b'3', b'~'],
                12 => vec![0x1b, b'[', b'2', b'4', b'~'],
                _ => return None,
            };
            Some(base)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regular_char() {
        let bytes = key_to_pty_bytes(KeyCode::Char('a'), KeyModifiers::NONE);
        assert_eq!(bytes, Some(vec![b'a']));
    }

    #[test]
    fn test_ctrl_c() {
        let bytes = key_to_pty_bytes(KeyCode::Char('c'), KeyModifiers::CONTROL);
        assert_eq!(bytes, Some(vec![0x03])); // ETX (Ctrl+C)
    }

    #[test]
    fn test_enter() {
        let bytes = key_to_pty_bytes(KeyCode::Enter, KeyModifiers::NONE);
        assert_eq!(bytes, Some(vec![b'\r']));
    }

    #[test]
    fn test_arrow_keys() {
        assert_eq!(
            key_to_pty_bytes(KeyCode::Up, KeyModifiers::NONE),
            Some(vec![0x1b, b'[', b'A'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Down, KeyModifiers::NONE),
            Some(vec![0x1b, b'[', b'B'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Right, KeyModifiers::NONE),
            Some(vec![0x1b, b'[', b'C'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Left, KeyModifiers::NONE),
            Some(vec![0x1b, b'[', b'D'])
        );
    }

    #[test]
    fn test_alt_key() {
        let bytes = key_to_pty_bytes(KeyCode::Char('x'), KeyModifiers::ALT);
        assert_eq!(bytes, Some(vec![0x1b, b'x']));
    }
}
