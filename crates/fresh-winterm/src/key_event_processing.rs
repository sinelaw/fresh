//! Platform-independent key event processing logic.
//!
//! Extracted from vt_input.rs so it can be tested on all platforms (the Windows
//! console types are not available on Linux/macOS).

// On non-Windows, these types and functions are only used by tests.
#![cfg_attr(not(windows), allow(dead_code))]

/// Fields from a Windows `KEY_EVENT_RECORD` needed for character processing.
#[derive(Debug, Clone)]
pub(crate) struct KeyEventFields {
    /// `bKeyDown` — true for key press, false for key release.
    pub key_down: bool,
    /// `uChar.UnicodeChar` — the UTF-16 code unit (0 if no character).
    pub unicode_char: u16,
    /// `wVirtualKeyCode` — the virtual-key code of the key.
    pub virtual_key_code: u16,
    /// `wRepeatCount` — number of times the keystroke is auto-repeated.
    pub repeat_count: u16,
}

/// Mutable state carried across key events for UTF-16 surrogate pair decoding.
#[derive(Debug, Default)]
pub(crate) struct SurrogateState {
    pub high: Option<u16>,
}

/// Returns `true` if this key event should produce character output.
///
/// Normal key-down events with a character are always processed.
///
/// Key-up events are normally ignored (the key-down already produced the
/// character). The exception is **IME-composed characters**: on Windows,
/// the terminal delivers committed IME text (Chinese, Japanese, Korean, …)
/// as key-up events with `virtual_key_code == 0` and the composed Unicode
/// character in `unicode_char`. Regular physical key releases have a
/// non-zero `virtual_key_code` matching the physical key, so this check
/// safely distinguishes IME input from regular key repeats.
pub(crate) fn should_process_key_event(
    key_down: bool,
    unicode_char: u16,
    virtual_key_code: u16,
) -> bool {
    unicode_char != 0 && (key_down || virtual_key_code == 0)
}

/// Process a single key event, appending any resulting UTF-8 bytes to `out`.
pub(crate) fn process_key_event(
    event: &KeyEventFields,
    surrogate: &mut SurrogateState,
    out: &mut Vec<u8>,
) {
    let ch = event.unicode_char;
    if !should_process_key_event(event.key_down, ch, event.virtual_key_code) {
        return;
    }

    let repeat = (event.repeat_count as usize).max(1);
    if (0xD800..=0xDBFF).contains(&ch) {
        surrogate.high = Some(ch);
    } else if (0xDC00..=0xDFFF).contains(&ch) {
        if let Some(high) = surrogate.high.take() {
            if let Some(c) = char::decode_utf16([high, ch]).next().and_then(|r| r.ok()) {
                let mut buf = [0u8; 4];
                let s = c.encode_utf8(&mut buf);
                for _ in 0..repeat {
                    out.extend_from_slice(s.as_bytes());
                }
            }
        }
    } else {
        surrogate.high = None;
        if let Some(c) = char::from_u32(ch as u32) {
            let mut buf = [0u8; 4];
            let s = c.encode_utf8(&mut buf);
            for _ in 0..repeat {
                out.extend_from_slice(s.as_bytes());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: process a list of key events and return the resulting bytes.
    fn process_events(events: &[KeyEventFields]) -> Vec<u8> {
        let mut out = Vec::new();
        let mut surrogate = SurrogateState::default();
        for event in events {
            process_key_event(event, &mut surrogate, &mut out);
        }
        out
    }

    fn key_down(ch: u16, vk: u16) -> KeyEventFields {
        KeyEventFields {
            key_down: true,
            unicode_char: ch,
            virtual_key_code: vk,
            repeat_count: 1,
        }
    }

    fn key_up(ch: u16, vk: u16) -> KeyEventFields {
        KeyEventFields {
            key_down: false,
            unicode_char: ch,
            virtual_key_code: vk,
            repeat_count: 1,
        }
    }

    // ---------------------------------------------------------------
    // Basic ASCII key handling
    // ---------------------------------------------------------------

    #[test]
    fn test_regular_key_down_produces_character() {
        let events = vec![key_down(b'a' as u16, 0x41)]; // 'a', VK_A
        assert_eq!(process_events(&events), b"a");
    }

    #[test]
    fn test_regular_key_up_is_ignored() {
        // Key-up for a regular key has the same vk as key-down → must be ignored
        // to avoid duplicating the character.
        let events = vec![key_up(b'a' as u16, 0x41)]; // 'a', VK_A
        assert_eq!(process_events(&events), b"");
    }

    #[test]
    fn test_regular_key_down_and_up_no_duplicate() {
        // Full press+release cycle: only key-down should produce output.
        let events = vec![key_down(b'a' as u16, 0x41), key_up(b'a' as u16, 0x41)];
        assert_eq!(process_events(&events), b"a");
    }

    // ---------------------------------------------------------------
    // IME-composed characters (the bug from issue #1538)
    // ---------------------------------------------------------------

    #[test]
    fn test_ime_composed_chinese_character() {
        // IME delivers committed text as key-up events with vk=0.
        // '你' = U+4F60 = 0x4F60 — fits in a single UTF-16 code unit.
        let events = vec![key_up(0x4F60, 0)]; // 你, vk=0 (IME)
        let result = process_events(&events);
        assert_eq!(
            String::from_utf8(result).unwrap(),
            "你",
            "IME-composed Chinese character should be processed"
        );
    }

    #[test]
    fn test_ime_composed_multiple_characters() {
        // IME might commit a whole word at once, e.g. "你好" (nǐ hǎo).
        // Each character arrives as a separate key-up event with vk=0.
        let events = vec![
            key_up(0x4F60, 0), // 你
            key_up(0x597D, 0), // 好
        ];
        let result = process_events(&events);
        assert_eq!(
            String::from_utf8(result).unwrap(),
            "你好",
            "Multiple IME-composed characters should all be processed"
        );
    }

    #[test]
    fn test_ime_composed_japanese_character() {
        // Japanese IME: 'あ' = U+3042
        let events = vec![key_up(0x3042, 0)]; // あ, vk=0 (IME)
        let result = process_events(&events);
        assert_eq!(
            String::from_utf8(result).unwrap(),
            "あ",
            "IME-composed Japanese character should be processed"
        );
    }

    #[test]
    fn test_ime_composed_korean_character() {
        // Korean IME: '한' = U+D55C
        let events = vec![key_up(0xD55C, 0)]; // 한, vk=0 (IME)
        let result = process_events(&events);
        assert_eq!(
            String::from_utf8(result).unwrap(),
            "한",
            "IME-composed Korean character should be processed"
        );
    }

    // ---------------------------------------------------------------
    // Mixed IME and regular input
    // ---------------------------------------------------------------

    #[test]
    fn test_mixed_regular_and_ime_input() {
        // User types "a你b" — 'a' and 'b' are regular keys, '你' is IME.
        let events = vec![
            key_down(b'a' as u16, 0x41), // regular 'a'
            key_up(b'a' as u16, 0x41),   // regular 'a' release (ignored)
            key_up(0x4F60, 0),           // IME '你'
            key_down(b'b' as u16, 0x42), // regular 'b'
            key_up(b'b' as u16, 0x42),   // regular 'b' release (ignored)
        ];
        let result = process_events(&events);
        assert_eq!(String::from_utf8(result).unwrap(), "a你b",);
    }

    // ---------------------------------------------------------------
    // UTF-16 surrogate pairs
    // ---------------------------------------------------------------

    #[test]
    fn test_surrogate_pair_emoji() {
        // '😀' = U+1F600 → UTF-16: [0xD83D, 0xDE00]
        let events = vec![
            key_down(0xD83D, 0), // high surrogate
            key_down(0xDE00, 0), // low surrogate
        ];
        let result = process_events(&events);
        assert_eq!(String::from_utf8(result).unwrap(), "😀");
    }

    // ---------------------------------------------------------------
    // Repeat count
    // ---------------------------------------------------------------

    #[test]
    fn test_repeat_count() {
        let events = vec![KeyEventFields {
            key_down: true,
            unicode_char: b'x' as u16,
            virtual_key_code: 0x58, // VK_X
            repeat_count: 3,
        }];
        assert_eq!(process_events(&events), b"xxx");
    }

    // ---------------------------------------------------------------
    // Edge cases
    // ---------------------------------------------------------------

    #[test]
    fn test_zero_char_ignored() {
        // Key events with ch=0 (e.g. modifier-only presses) are always ignored.
        let events = vec![
            key_down(0, 0x10), // Shift key down (no char)
            key_up(0, 0x10),   // Shift key up
        ];
        assert_eq!(process_events(&events), b"");
    }
}
