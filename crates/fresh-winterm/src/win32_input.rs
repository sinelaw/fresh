//! The win32-input-mode key report, `CSI Vk ; Sc ; Uc ; Kd ; Cs ; Rc _`.
//!
//! With win32-input-mode (`CSI ? 9001 h`) a terminal reports each key as the
//! console's own KEY_EVENT record — virtual key, scan code, UTF-16 unit,
//! key-down flag, control-key state and repeat count — instead of ConPTY's VT
//! encoding, which drops every modifier the legacy encoding has no spelling
//! for. This module knows the record's format: how to read one, what its
//! fields mean, and how to turn reports back into text. Mapping a report onto
//! the parser's own key spelling is left to the parser.
//!
//! Plain byte matching, so it builds — and its tests run — on every platform.

use std::borrow::Cow;

use crossterm::event::{KeyCode, KeyModifiers};

/// Upper bound on the repeat count of one report. A held key reports one press
/// per auto-repeat, so a larger count is a malformed report; the bound keeps it
/// from flooding the event queue.
pub const MAX_REPEAT: u16 = 64;

// `dwControlKeyState` bits.
const RIGHT_ALT: u32 = 0x0001;
const LEFT_ALT: u32 = 0x0002;
const RIGHT_CTRL: u32 = 0x0004;
const LEFT_CTRL: u32 = 0x0008;
const SHIFT: u32 = 0x0010;
/// All nine `dwControlKeyState` bits a KEY_EVENT record can carry.
const CONTROL_KEY_STATE_MASK: u32 = 0x01FF;

/// One win32-input-mode key report.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Win32Key {
    /// `wVirtualKeyCode`.
    pub vk: u16,
    /// `uChar.UnicodeChar`: one UTF-16 unit, or 0 when the key types nothing.
    pub unicode: u16,
    /// `bKeyDown`.
    pub down: bool,
    /// `dwControlKeyState`.
    pub state: u32,
    /// `wRepeatCount`.
    pub repeat: u16,
}

impl Win32Key {
    /// Read the parameters of a report (the bytes between `CSI` and `_`).
    ///
    /// An omitted field takes its default: 0, except the repeat count's 1.
    pub fn parse(params: &[u8]) -> Option<Self> {
        Self::from_fields(fields(params, false)?)
    }

    /// [`Self::parse`] for bytes that may not be a report at all: all six
    /// fields present, and each in the range a KEY_EVENT record can hold — a
    /// virtual-key code from 1 to 254, a scan code and UTF-16 unit of at most
    /// 0xFFFF, a key-down flag of 0 or 1, only the nine control-key state bits,
    /// and a repeat count of at least 1. `1;2;3;4;5;6` and `;;;;;` are text by
    /// these rules; `69;18;5;1;24;1` (Ctrl+Shift+E) is a report.
    pub fn parse_exact(params: &[u8]) -> Option<Self> {
        let fields = fields(params, true)?;
        let [vk, scan, unicode, down, state, repeat] = fields;
        let plausible = (1..=0xFE).contains(&vk)
            && scan <= 0xFFFF
            && unicode <= 0xFFFF
            && down <= 1
            && state & !CONTROL_KEY_STATE_MASK == 0
            && (1..=0xFFFF).contains(&repeat);
        if !plausible {
            return None;
        }
        Self::from_fields(fields)
    }

    fn from_fields([vk, _scan, unicode, down, state, repeat]: [u32; 6]) -> Option<Self> {
        Some(Self {
            vk: u16::try_from(vk).ok()?,
            // The scan code names the physical key, which nothing here needs.
            unicode: u16::try_from(unicode).ok()?,
            down: down != 0,
            state,
            repeat: u16::try_from(repeat).ok()?,
        })
    }

    /// Shift, Ctrl and Alt from the control-key state, either side of the
    /// keyboard. Windows reports AltGr as Ctrl+Alt.
    pub fn modifiers(&self) -> KeyModifiers {
        let mut modifiers = KeyModifiers::empty();
        if self.state & SHIFT != 0 {
            modifiers |= KeyModifiers::SHIFT;
        }
        if self.state & (LEFT_CTRL | RIGHT_CTRL) != 0 {
            modifiers |= KeyModifiers::CONTROL;
        }
        if self.state & (LEFT_ALT | RIGHT_ALT) != 0 {
            modifiers |= KeyModifiers::ALT;
        }
        modifiers
    }

    /// The key the virtual-key code names when it is not a character key.
    pub fn functional_key(&self) -> Option<KeyCode> {
        Some(match self.vk {
            0x08 => KeyCode::Backspace,
            0x09 => KeyCode::Tab,
            // VK_CLEAR: the keypad's center "5" with NumLock off.
            0x0C => KeyCode::KeypadBegin,
            0x0D => KeyCode::Enter,
            0x1B => KeyCode::Esc,
            0x20 => KeyCode::Char(' '),
            0x21 => KeyCode::PageUp,
            0x22 => KeyCode::PageDown,
            0x23 => KeyCode::End,
            0x24 => KeyCode::Home,
            0x25 => KeyCode::Left,
            0x26 => KeyCode::Up,
            0x27 => KeyCode::Right,
            0x28 => KeyCode::Down,
            0x2D => KeyCode::Insert,
            0x2E => KeyCode::Delete,
            vk @ 0x70..=0x87 => KeyCode::F((vk - 0x70 + 1) as u8),
            _ => return None,
        })
    }

    /// The base character of a letter or digit key (`VK_A`–`VK_Z`,
    /// `VK_0`–`VK_9`), whose virtual-key codes are the uppercase ASCII letters
    /// and the digits.
    pub fn letter_or_digit(&self) -> Option<char> {
        match self.vk {
            vk @ 0x41..=0x5A => Some(char::from(vk as u8).to_ascii_lowercase()),
            vk @ 0x30..=0x39 => Some(char::from(vk as u8)),
            _ => None,
        }
    }
}

/// The six fields of a report's parameters. `exact` asks for all six, none
/// empty; otherwise a missing or empty field takes its default.
fn fields(params: &[u8], exact: bool) -> Option<[u32; 6]> {
    let params = std::str::from_utf8(params).ok()?;
    let mut fields = [0, 0, 0, 0, 0, 1];
    let mut count = 0;
    for field in params.split(';') {
        if count == fields.len() {
            return None;
        }
        if !field.is_empty() {
            fields[count] = field.parse().ok()?;
        } else if exact {
            return None;
        }
        count += 1;
    }
    if exact && count != fields.len() {
        return None;
    }
    Some(fields)
}

/// The parameters of a report at the start of `rest` (what follows `CSI`),
/// and the length of the report through its `_`. `None` when `rest` does not
/// start with digits and semicolons ending in `_`, or ends before the `_`.
pub(crate) fn split_report(rest: &[u8]) -> Option<(&[u8], usize)> {
    let end = rest
        .iter()
        .position(|&b| !(b.is_ascii_digit() || b == b';'))?;
    (rest[end] == b'_').then(|| (&rest[..end], end + 1))
}

/// Joins the UTF-16 units of successive reports into characters: an IME
/// commits text, and an emoji picker types a character outside the BMP, one
/// unit per report.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Utf16Joiner {
    high: Option<u16>,
}

impl Utf16Joiner {
    /// The character `unit` completes, or `None` for the first half of a
    /// surrogate pair (kept for the next unit) and for a low surrogate with no
    /// high one before it.
    pub fn push(&mut self, unit: u16) -> Option<char> {
        match unit {
            0xD800..=0xDBFF => {
                self.high = Some(unit);
                None
            }
            0xDC00..=0xDFFF => {
                let high = self.high.take()?;
                char::decode_utf16([high, unit]).next()?.ok()
            }
            _ => {
                self.high = None;
                char::from_u32(u32::from(unit))
            }
        }
    }

    /// Forget a pending high surrogate: the key after it typed no character.
    pub fn reset(&mut self) {
        self.high = None;
    }
}

/// `bytes` with every report in it replaced by the character it types.
///
/// Windows Terminal sends pasted text as plain characters but each line break
/// as an Enter key report (a key-down and a key-up), so a bracketed paste has
/// to be decoded before its text is used.
pub fn decode_text(bytes: &[u8]) -> Cow<'_, [u8]> {
    if !bytes.windows(2).any(|w| w == b"\x1b[") {
        return Cow::Borrowed(bytes);
    }
    let mut out = Vec::with_capacity(bytes.len());
    let mut joiner = Utf16Joiner::default();
    let mut i = 0;
    while i < bytes.len() {
        let Some((key, len)) = report_at(&bytes[i..]) else {
            out.push(bytes[i]);
            i += 1;
            continue;
        };
        i += len;
        if !key.down || key.unicode == 0 {
            continue;
        }
        let Some(ch) = joiner.push(key.unicode) else {
            continue;
        };
        let mut buf = [0u8; 4];
        for _ in 0..key.repeat.clamp(1, MAX_REPEAT) {
            out.extend_from_slice(ch.encode_utf8(&mut buf).as_bytes());
        }
    }
    Cow::Owned(out)
}

/// The report at the start of `bytes`, ESC included, with its length.
fn report_at(bytes: &[u8]) -> Option<(Win32Key, usize)> {
    let (params, len) = split_report(bytes.strip_prefix(b"\x1b[")?)?;
    Some((Win32Key::parse(params)?, 2 + len))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_fills_omitted_fields_with_defaults() {
        let key = Win32Key::parse(b"65").unwrap();
        assert_eq!(
            key,
            Win32Key {
                vk: 65,
                unicode: 0,
                down: false,
                state: 0,
                repeat: 1,
            }
        );
        assert_eq!(
            Win32Key::parse(b"65;;97;1"),
            Win32Key::parse(b"65;0;97;1;0;1")
        );
        assert_eq!(Win32Key::parse(b"1;2;3;4;5;6;7"), None);
        assert_eq!(Win32Key::parse(b"70000"), None);
    }

    #[test]
    fn parse_exact_takes_only_what_a_key_event_can_hold() {
        assert!(Win32Key::parse_exact(b"69;18;5;1;24;1").is_some());
        for text in [
            "1;2;3;4;5;6",
            ";;;;;",
            "0;0;0;0;0;0",
            "69;18;5;1;24",
            "69;18;5;1;24;1;1",
            "69;18;99999;1;24;1",
            "69;18;5;1;512;1",
        ] {
            assert_eq!(Win32Key::parse_exact(text.as_bytes()), None, "{text}");
        }
    }

    #[test]
    fn modifiers_from_either_side() {
        let key = |state| Win32Key::parse(format!("65;30;1;1;{state};1").as_bytes()).unwrap();
        assert_eq!(key(0x0008).modifiers(), KeyModifiers::CONTROL);
        assert_eq!(key(0x0004).modifiers(), KeyModifiers::CONTROL);
        assert_eq!(key(0x0002).modifiers(), KeyModifiers::ALT);
        assert_eq!(key(0x0001).modifiers(), KeyModifiers::ALT);
        assert_eq!(
            key(0x0018).modifiers(),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT
        );
        // NumLock (0x20) is not a modifier.
        assert_eq!(key(0x0020).modifiers(), KeyModifiers::empty());
    }

    #[test]
    fn joiner_reassembles_surrogate_pairs() {
        let mut joiner = Utf16Joiner::default();
        assert_eq!(joiner.push(0xD83D), None);
        assert_eq!(joiner.push(0xDE00), Some('😀'));
        // A low surrogate with nothing before it, or after a reset.
        assert_eq!(joiner.push(0xDE00), None);
        assert_eq!(joiner.push(0xD83D), None);
        joiner.reset();
        assert_eq!(joiner.push(0xDE00), None);
        assert_eq!(joiner.push(u16::from(b'a')), Some('a'));
    }

    #[test]
    fn decode_text_replaces_reports_with_their_characters() {
        assert!(matches!(decode_text(b"plain"), Cow::Borrowed(b"plain")));
        // Enter's key-down types `\r`; its key-up types nothing.
        let pasted = b"one\x1b[13;28;13;1;0;1_\x1b[13;28;13;0;0;1_two";
        assert_eq!(&*decode_text(pasted), b"one\rtwo");
        // Not a report: left as it is.
        assert_eq!(&*decode_text(b"\x1b[1;2X"), b"\x1b[1;2X");
    }
}
