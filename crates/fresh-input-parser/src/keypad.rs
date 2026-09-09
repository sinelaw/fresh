//! The numeric keypad, named once.
//!
//! Three things need to agree about what a keypad key *is*, and they used to
//! be three separate hand-written tables:
//!
//! * the kitty keyboard protocol's Private Use Area codepoints, decoded by
//!   `kitty_functional_key`;
//! * the application-keypad (SS3) bytes, decoded by `feed_ss3`;
//! * the X11 keysym spellings a user writes in `keybindings` config, parsed by
//!   `KeybindingResolver::parse_key` over in `fresh-editor`.
//!
//! They describe one relation, so they are one table. [`KEYPAD_KEYS`] is that
//! table; the decoders resolve through it and the config parser resolves
//! through it, which is what keeps a name that binds and a key that arrives
//! from drifting apart.
//!
//! **The normalisation is lossy, and deliberately so.** A terminal reports the
//! keypad through the same code as the main keyboard — `KP_MULTIPLY` arrives
//! as `*`, `KP_ENTER` as `Enter`, `KP_LEFT` as `Left` — so a keypad name is an
//! *alias* for the main-keyboard key, not a distinct binding. [`KP_BEGIN`] (the
//! `5` with Num Lock off) is the sole exception: nothing on the main keyboard
//! means it, so it keeps a [`KeyCode`] of its own.

use crossterm::event::KeyCode;

/// One keypad key, in the three vocabularies that have to agree about it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct KeypadKey {
    /// The kitty keyboard protocol's Private Use Area codepoint.
    pub kitty_codepoint: u32,
    /// The X11 keysym name, lowercased — the spelling accepted in config.
    pub keysym: &'static str,
    /// What the key normalises to by the time the editor sees it.
    pub code: KeyCode,
}

/// The `5` key with Num Lock off: the one keypad key with no main-keyboard
/// equivalent, and so the one that keeps a [`KeyCode`] of its own.
pub const KP_BEGIN: u32 = 57427;

/// The keypad `0`. The ten digits are contiguous from here, in both the kitty
/// protocol and the SS3 (`ESC O p` … `ESC O y`) encoding.
pub const KP_0: u32 = 57399;

/// Every keypad key the terminal protocols name, in kitty-codepoint order.
///
/// Order is load-bearing only for the doc table generated from it; lookups are
/// by name or codepoint.
pub const KEYPAD_KEYS: &[KeypadKey] = &[
    KeypadKey {
        kitty_codepoint: 57399,
        keysym: "kp_0",
        code: KeyCode::Char('0'),
    },
    KeypadKey {
        kitty_codepoint: 57400,
        keysym: "kp_1",
        code: KeyCode::Char('1'),
    },
    KeypadKey {
        kitty_codepoint: 57401,
        keysym: "kp_2",
        code: KeyCode::Char('2'),
    },
    KeypadKey {
        kitty_codepoint: 57402,
        keysym: "kp_3",
        code: KeyCode::Char('3'),
    },
    KeypadKey {
        kitty_codepoint: 57403,
        keysym: "kp_4",
        code: KeyCode::Char('4'),
    },
    KeypadKey {
        kitty_codepoint: 57404,
        keysym: "kp_5",
        code: KeyCode::Char('5'),
    },
    KeypadKey {
        kitty_codepoint: 57405,
        keysym: "kp_6",
        code: KeyCode::Char('6'),
    },
    KeypadKey {
        kitty_codepoint: 57406,
        keysym: "kp_7",
        code: KeyCode::Char('7'),
    },
    KeypadKey {
        kitty_codepoint: 57407,
        keysym: "kp_8",
        code: KeyCode::Char('8'),
    },
    KeypadKey {
        kitty_codepoint: 57408,
        keysym: "kp_9",
        code: KeyCode::Char('9'),
    },
    KeypadKey {
        kitty_codepoint: 57409,
        keysym: "kp_decimal",
        code: KeyCode::Char('.'),
    },
    KeypadKey {
        kitty_codepoint: 57410,
        keysym: "kp_divide",
        code: KeyCode::Char('/'),
    },
    KeypadKey {
        kitty_codepoint: 57411,
        keysym: "kp_multiply",
        code: KeyCode::Char('*'),
    },
    KeypadKey {
        kitty_codepoint: 57412,
        keysym: "kp_subtract",
        code: KeyCode::Char('-'),
    },
    KeypadKey {
        kitty_codepoint: 57413,
        keysym: "kp_add",
        code: KeyCode::Char('+'),
    },
    KeypadKey {
        kitty_codepoint: 57414,
        keysym: "kp_enter",
        code: KeyCode::Enter,
    },
    KeypadKey {
        kitty_codepoint: 57415,
        keysym: "kp_equal",
        code: KeyCode::Char('='),
    },
    KeypadKey {
        kitty_codepoint: 57416,
        keysym: "kp_separator",
        code: KeyCode::Char(','),
    },
    KeypadKey {
        kitty_codepoint: 57417,
        keysym: "kp_left",
        code: KeyCode::Left,
    },
    KeypadKey {
        kitty_codepoint: 57418,
        keysym: "kp_right",
        code: KeyCode::Right,
    },
    KeypadKey {
        kitty_codepoint: 57419,
        keysym: "kp_up",
        code: KeyCode::Up,
    },
    KeypadKey {
        kitty_codepoint: 57420,
        keysym: "kp_down",
        code: KeyCode::Down,
    },
    KeypadKey {
        kitty_codepoint: 57421,
        keysym: "kp_page_up",
        code: KeyCode::PageUp,
    },
    KeypadKey {
        kitty_codepoint: 57422,
        keysym: "kp_page_down",
        code: KeyCode::PageDown,
    },
    KeypadKey {
        kitty_codepoint: 57423,
        keysym: "kp_home",
        code: KeyCode::Home,
    },
    KeypadKey {
        kitty_codepoint: 57424,
        keysym: "kp_end",
        code: KeyCode::End,
    },
    KeypadKey {
        kitty_codepoint: 57425,
        keysym: "kp_insert",
        code: KeyCode::Insert,
    },
    KeypadKey {
        kitty_codepoint: 57426,
        keysym: "kp_delete",
        code: KeyCode::Delete,
    },
    KeypadKey {
        kitty_codepoint: KP_BEGIN,
        keysym: "kp_begin",
        code: KeyCode::KeypadBegin,
    },
];

/// The `KeyCode` for an already-lowercased X11 keypad keysym name.
pub fn code_for_keysym(name: &str) -> Option<KeyCode> {
    KEYPAD_KEYS
        .iter()
        .find(|k| k.keysym == name)
        .map(|k| k.code)
}

/// The `KeyCode` for a kitty keyboard-protocol keypad codepoint.
pub fn code_for_kitty_codepoint(cp: u32) -> Option<KeyCode> {
    KEYPAD_KEYS
        .iter()
        .find(|k| k.kitty_codepoint == cp)
        .map(|k| k.code)
}

/// The `KeyCode` for keypad digit `digit` (`0..=9`).
///
/// Both encodings lay the ten digits out contiguously, so a decoder that has
/// the digit in hand asks for it here rather than rebuilding `Char('0' + n)`
/// and quietly disagreeing with the table on what a keypad digit is.
pub fn code_for_digit(digit: u8) -> Option<KeyCode> {
    if digit > 9 {
        return None;
    }
    code_for_kitty_codepoint(KP_0 + digit as u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every entry is reachable by both of its keys, and the two lookups agree.
    #[test]
    fn both_lookups_find_every_entry() {
        for k in KEYPAD_KEYS {
            assert_eq!(
                code_for_keysym(k.keysym),
                Some(k.code),
                "keysym {}",
                k.keysym
            );
            assert_eq!(
                code_for_kitty_codepoint(k.kitty_codepoint),
                Some(k.code),
                "codepoint {}",
                k.kitty_codepoint
            );
        }
    }

    /// No two entries claim the same name or the same codepoint — either would
    /// make one of them unreachable through the lookup that collides.
    #[test]
    fn names_and_codepoints_are_unique() {
        let mut names: Vec<&str> = KEYPAD_KEYS.iter().map(|k| k.keysym).collect();
        names.sort_unstable();
        let count = names.len();
        names.dedup();
        assert_eq!(names.len(), count, "duplicate keysym in KEYPAD_KEYS");

        let mut cps: Vec<u32> = KEYPAD_KEYS.iter().map(|k| k.kitty_codepoint).collect();
        cps.sort_unstable();
        let count = cps.len();
        cps.dedup();
        assert_eq!(cps.len(), count, "duplicate codepoint in KEYPAD_KEYS");
    }

    #[test]
    fn digits_are_contiguous_from_kp_0() {
        for d in 0..=9u8 {
            assert_eq!(
                code_for_digit(d),
                Some(KeyCode::Char((b'0' + d) as char)),
                "keypad digit {d}"
            );
        }
        assert_eq!(code_for_digit(10), None);
    }

    /// `KeypadBegin` is the only keypad key that does not collapse onto a
    /// main-keyboard code — the property the docs promise users.
    #[test]
    fn keypad_begin_is_the_only_distinct_key() {
        let distinct: Vec<&str> = KEYPAD_KEYS
            .iter()
            .filter(|k| k.code == KeyCode::KeypadBegin)
            .map(|k| k.keysym)
            .collect();
        assert_eq!(distinct, vec!["kp_begin"]);
    }
}
