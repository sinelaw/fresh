//! Key names, the compact syntax (`C-x`, `C-S-Left`, `C-x C-s`), and the
//! typed value surfaces resolve to.
//!
//! Keypad and media names live in the terminal input-parser crate, which this
//! crate does not depend on; callers pass them in as [`ExtraNames`].

use crossterm::event::{KeyCode, KeyModifiers};

pub struct KeyName {
    /// Lowercase; the first is canonical.
    pub names: &'static [&'static str],
    pub code: KeyCode,
}

pub const NAMED_KEYS: &[KeyName] = &[
    KeyName {
        names: &["enter", "ret", "return"],
        code: KeyCode::Enter,
    },
    KeyName {
        names: &["backspace", "bs"],
        code: KeyCode::Backspace,
    },
    KeyName {
        names: &["delete", "del"],
        code: KeyCode::Delete,
    },
    KeyName {
        names: &["insert", "ins"],
        code: KeyCode::Insert,
    },
    KeyName {
        names: &["tab"],
        code: KeyCode::Tab,
    },
    KeyName {
        names: &["backtab", "shift+tab"],
        code: KeyCode::BackTab,
    },
    KeyName {
        names: &["escape", "esc"],
        code: KeyCode::Esc,
    },
    KeyName {
        names: &["space", "spc"],
        code: KeyCode::Char(' '),
    },
    KeyName {
        names: &["left"],
        code: KeyCode::Left,
    },
    KeyName {
        names: &["right"],
        code: KeyCode::Right,
    },
    KeyName {
        names: &["up"],
        code: KeyCode::Up,
    },
    KeyName {
        names: &["down"],
        code: KeyCode::Down,
    },
    KeyName {
        names: &["home"],
        code: KeyCode::Home,
    },
    KeyName {
        names: &["end"],
        code: KeyCode::End,
    },
    KeyName {
        names: &["pageup", "pgup"],
        code: KeyCode::PageUp,
    },
    KeyName {
        names: &["pagedown", "pgdn"],
        code: KeyCode::PageDown,
    },
    // Lock and system keys. A terminal speaking the kitty keyboard protocol
    // reports these (the input parser decodes them at codepoints 57358-57363),
    // so the keybinding editor can record one — and without a name here it
    // would write a `{:?}` spelling that the loader then refused, which is the
    // `Insert` bug one line up, repeated.
    KeyName {
        names: &["capslock"],
        code: KeyCode::CapsLock,
    },
    KeyName {
        names: &["scrolllock"],
        code: KeyCode::ScrollLock,
    },
    KeyName {
        names: &["numlock"],
        code: KeyCode::NumLock,
    },
    KeyName {
        names: &["printscreen"],
        code: KeyCode::PrintScreen,
    },
    KeyName {
        names: &["pause"],
        code: KeyCode::Pause,
    },
    KeyName {
        names: &["menu"],
        code: KeyCode::Menu,
    },
];

/// X11 keysym spellings for ASCII punctuation, for the characters JSON makes
/// awkward to write literally. The bare character stays canonical.
pub const PUNCTUATION_KEYS: &[KeyName] = &[
    KeyName {
        names: &["asterisk", "star"],
        code: KeyCode::Char('*'),
    },
    KeyName {
        names: &["plus"],
        code: KeyCode::Char('+'),
    },
    KeyName {
        names: &["minus", "hyphen"],
        code: KeyCode::Char('-'),
    },
    KeyName {
        names: &["slash"],
        code: KeyCode::Char('/'),
    },
    KeyName {
        names: &["period", "dot"],
        code: KeyCode::Char('.'),
    },
    KeyName {
        names: &["equal", "equals"],
        code: KeyCode::Char('='),
    },
    KeyName {
        names: &["backslash"],
        code: KeyCode::Char('\\'),
    },
    KeyName {
        names: &["comma"],
        code: KeyCode::Char(','),
    },
    KeyName {
        names: &["semicolon"],
        code: KeyCode::Char(';'),
    },
    KeyName {
        names: &["colon"],
        code: KeyCode::Char(':'),
    },
    KeyName {
        names: &["apostrophe", "quote"],
        code: KeyCode::Char('\''),
    },
    KeyName {
        names: &["quotedbl", "doublequote"],
        code: KeyCode::Char('"'),
    },
    KeyName {
        names: &["grave", "backtick"],
        code: KeyCode::Char('`'),
    },
    KeyName {
        names: &["tilde"],
        code: KeyCode::Char('~'),
    },
    KeyName {
        names: &["exclam", "exclamation"],
        code: KeyCode::Char('!'),
    },
    KeyName {
        names: &["at"],
        code: KeyCode::Char('@'),
    },
    KeyName {
        names: &["numbersign", "hash"],
        code: KeyCode::Char('#'),
    },
    KeyName {
        names: &["dollar"],
        code: KeyCode::Char('$'),
    },
    KeyName {
        names: &["percent"],
        code: KeyCode::Char('%'),
    },
    KeyName {
        names: &["asciicircum", "caret"],
        code: KeyCode::Char('^'),
    },
    KeyName {
        names: &["ampersand"],
        code: KeyCode::Char('&'),
    },
    KeyName {
        names: &["underscore"],
        code: KeyCode::Char('_'),
    },
    KeyName {
        names: &["bar", "pipe"],
        code: KeyCode::Char('|'),
    },
    KeyName {
        names: &["question"],
        code: KeyCode::Char('?'),
    },
    KeyName {
        names: &["less", "lessthan"],
        code: KeyCode::Char('<'),
    },
    KeyName {
        names: &["greater", "greaterthan"],
        code: KeyCode::Char('>'),
    },
    KeyName {
        names: &["parenleft"],
        code: KeyCode::Char('('),
    },
    KeyName {
        names: &["parenright"],
        code: KeyCode::Char(')'),
    },
    KeyName {
        names: &["bracketleft"],
        code: KeyCode::Char('['),
    },
    KeyName {
        names: &["bracketright"],
        code: KeyCode::Char(']'),
    },
    KeyName {
        names: &["braceleft"],
        code: KeyCode::Char('{'),
    },
    KeyName {
        names: &["braceright"],
        code: KeyCode::Char('}'),
    },
];

pub type ExtraNames = fn(&str) -> Option<KeyCode>;

pub fn name_to_code(lower: &str, extra: Option<ExtraNames>) -> Option<KeyCode> {
    NAMED_KEYS
        .iter()
        .chain(PUNCTUATION_KEYS)
        .find(|k| k.names.contains(&lower))
        .map(|k| k.code)
        .or_else(|| extra.and_then(|f| f(lower)))
}

pub fn canonical_name(code: KeyCode) -> Option<&'static str> {
    NAMED_KEYS
        .iter()
        .chain(PUNCTUATION_KEYS)
        .find(|k| k.code == code)
        .and_then(|k| k.names.first().copied())
}

/// Longest first, so `Super-` is not read as `S-`.
const PREFIXES: &[(&str, KeyModifiers)] = &[
    ("super-", KeyModifiers::SUPER),
    ("cmd-", KeyModifiers::SUPER),
    ("meta-", KeyModifiers::META),
    ("hyper-", KeyModifiers::HYPER),
    ("c-", KeyModifiers::CONTROL),
    ("m-", KeyModifiers::ALT),
    ("a-", KeyModifiers::ALT),
    ("s-", KeyModifiers::SHIFT),
    ("h-", KeyModifiers::HYPER),
];

const PRINTED_PREFIXES: &[(&str, KeyModifiers)] = &[
    ("Super-", KeyModifiers::SUPER),
    ("Meta-", KeyModifiers::META),
    ("H-", KeyModifiers::HYPER),
    ("C-", KeyModifiers::CONTROL),
    ("M-", KeyModifiers::ALT),
    ("S-", KeyModifiers::SHIFT),
];

/// Fields are private so [`Key::new`]'s normalisation cannot be bypassed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Key {
    code: KeyCode,
    mods: KeyModifiers,
}

impl Key {
    pub fn new(code: KeyCode, mods: KeyModifiers) -> Self {
        Self { code, mods }.normalized()
    }

    pub const fn plain(code: KeyCode) -> Self {
        Self {
            code,
            mods: KeyModifiers::NONE,
        }
    }

    pub fn code(&self) -> KeyCode {
        self.code
    }

    /// Never a Shift that belongs to the code.
    pub fn mods(&self) -> KeyModifiers {
        self.mods
    }

    pub fn to_key_event(&self) -> crossterm::event::KeyEvent {
        crossterm::event::KeyEvent::new(self.code, self.mods)
    }

    /// `Tab`+Shift, `BackTab` and `BackTab`+Shift are one keystroke.
    fn normalized(self) -> Self {
        match (self.code, self.mods.contains(KeyModifiers::SHIFT)) {
            (KeyCode::Tab, true) => Self {
                code: KeyCode::BackTab,
                mods: self.mods.difference(KeyModifiers::SHIFT),
            },
            (KeyCode::BackTab, true) => Self {
                code: KeyCode::BackTab,
                mods: self.mods.difference(KeyModifiers::SHIFT),
            },
            _ => self,
        }
    }

    /// An uppercase bare character carries Shift (`F` is Shift+f); named keys
    /// are case-insensitive.
    pub fn parse(s: &str, extra: Option<ExtraNames>) -> Option<Self> {
        let mut mods = KeyModifiers::NONE;
        let mut rest = s;
        'peel: loop {
            let lower = rest.to_lowercase();
            for (prefix, m) in PREFIXES {
                if lower.starts_with(prefix) {
                    // `"-"` is a key.
                    if rest.len() > prefix.len() {
                        mods |= *m;
                        rest = &rest[prefix.len()..];
                        continue 'peel;
                    }
                }
            }
            break;
        }
        let lower = rest.to_lowercase();
        let code = if let Some(code) = name_to_code(&lower, extra) {
            code
        } else if rest.chars().count() == 1 {
            let c = rest.chars().next()?;
            if c.is_uppercase() {
                mods |= KeyModifiers::SHIFT;
            }
            // Multi-char lowerings (ß, İ) fall back to the char itself.
            KeyCode::Char(c.to_lowercase().next().unwrap_or(c))
        } else if let Some(n) = lower.strip_prefix('f') {
            KeyCode::F(n.parse::<u8>().ok()?)
        } else {
            return None;
        };
        Some(Self::new(code, mods))
    }

    fn fmt_into(&self, out: &mut String) {
        for (prefix, m) in PRINTED_PREFIXES {
            if self.mods.contains(*m) {
                out.push_str(prefix);
            }
        }
        match canonical_name(self.code) {
            Some(name) => out.push_str(name),
            None => match self.code {
                // Lowercase, because uppercase would parse back as Shift+it.
                KeyCode::Char(c) => out.extend(c.to_lowercase()),
                KeyCode::F(n) => out.push_str(&format!("f{n}")),
                other => out.push_str(&format!("{other:?}")),
            },
        }
    }
}

impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        self.fmt_into(&mut s);
        f.write_str(&s)
    }
}

/// Space-separated; the space key is spelled `space`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KeySeq(Vec<Key>);

impl KeySeq {
    pub fn new(keys: Vec<Key>) -> Option<Self> {
        (!keys.is_empty()).then_some(Self(keys))
    }

    pub fn one(key: Key) -> Self {
        Self(vec![key])
    }

    pub fn keys(&self) -> &[Key] {
        &self.0
    }

    pub fn single(&self) -> Option<Key> {
        match self.0.as_slice() {
            [k] => Some(*k),
            _ => None,
        }
    }

    pub fn parse(s: &str, extra: Option<ExtraNames>) -> Option<Self> {
        let keys: Option<Vec<Key>> = s.split_whitespace().map(|p| Key::parse(p, extra)).collect();
        Self::new(keys?)
    }
}

impl std::fmt::Display for KeySeq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for (i, k) in self.0.iter().enumerate() {
            if i > 0 {
                s.push(' ');
            }
            k.fmt_into(&mut s);
        }
        f.write_str(&s)
    }
}

impl std::str::FromStr for Key {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        Self::parse(s, None).ok_or(())
    }
}

impl std::str::FromStr for KeySeq {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        Self::parse(s, None).ok_or(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn names() -> impl Iterator<Item = &'static str> {
        NAMED_KEYS
            .iter()
            .chain(PUNCTUATION_KEYS)
            .flat_map(|k| k.names.iter().copied())
    }

    /// Characters must be their own lowercase and non-whitespace: uppercase
    /// is not a distinct value, and whitespace is the chord separator.
    fn any_key() -> impl Strategy<Value = Key> {
        let named: Vec<KeyCode> = NAMED_KEYS
            .iter()
            .chain(PUNCTUATION_KEYS)
            .map(|k| k.code)
            .collect();
        let code = prop_oneof![
            proptest::sample::select(named),
            any::<char>()
                .prop_filter("its own lowercase, and not whitespace", |c| {
                    !c.is_whitespace() && c.to_lowercase().eq(std::iter::once(*c))
                })
                .prop_map(KeyCode::Char),
            any::<u8>().prop_map(KeyCode::F),
        ];
        (code, any::<u8>())
            .prop_map(|(code, bits)| Key::new(code, KeyModifiers::from_bits_truncate(bits)))
    }

    proptest! {
        #[test]
        fn printing_then_parsing_is_the_identity(key in any_key()) {
            let printed = key.to_string();
            prop_assert!(!printed.contains(char::is_whitespace), "printed as {:?}", printed);
            prop_assert_eq!(Key::parse(&printed, None), Some(key), "printed as {:?}", printed);
        }

        #[test]
        fn parsing_then_printing_preserves_meaning(key in any_key()) {
            let printed = key.to_string();
            let reparsed = Key::parse(&printed, None).expect("just printed");
            prop_assert_eq!(reparsed.to_string(), printed);
        }

        #[test]
        fn a_sequence_round_trips_as_its_presses(keys in prop::collection::vec(any_key(), 1..6)) {
            let seq = KeySeq::new(keys.clone()).expect("non-empty");
            prop_assert_eq!(seq.keys(), keys.as_slice());
            prop_assert_eq!(seq.single(), (keys.len() == 1).then(|| keys[0]));
            let printed = seq.to_string();
            prop_assert_eq!(KeySeq::parse(&printed, None), Some(seq));
        }

        /// A printer that drops a bit and a parser that never sets it would
        /// round-trip happily, so this is separate.
        #[test]
        fn no_modifier_is_dropped_in_printing(key in any_key()) {
            let printed = key.to_string();
            let reparsed = Key::parse(&printed, None).expect("just printed");
            prop_assert_eq!(reparsed.mods(), key.mods(), "printed as {:?}", printed);
        }
    }

    #[test]
    fn a_named_key_is_case_insensitive() {
        for name in names() {
            let lower = Key::parse(name, None);
            assert!(lower.is_some(), "{name:?} did not parse");
            assert_eq!(Key::parse(&name.to_uppercase(), None), lower, "{name:?}");
            assert_eq!(
                lower.map(|k| k.mods),
                Some(KeyModifiers::NONE),
                "{name:?} picked up a modifier from its spelling"
            );
        }
    }

    #[test]
    fn aliases_agree_with_their_canonical_spelling() {
        let mut seen = std::collections::HashSet::new();
        for entry in NAMED_KEYS.iter().chain(PUNCTUATION_KEYS) {
            let canonical = canonical_name(entry.code).expect("a named code has a canonical name");
            for name in entry.names {
                assert_eq!(*name, name.to_lowercase(), "{name:?} is not lowercase");
                assert!(seen.insert(*name), "{name:?} appears twice");
                assert_eq!(
                    name_to_code(name, None),
                    name_to_code(canonical, None),
                    "{name:?}"
                );
            }
        }
    }

    #[test]
    fn modifier_prefixes_commute() {
        for name in names() {
            for (a, b) in [("C-S-", "S-C-"), ("C-M-", "M-C-"), ("M-", "A-")] {
                assert_eq!(
                    Key::parse(&format!("{a}{name}"), None),
                    Key::parse(&format!("{b}{name}"), None),
                    "{a}{name} vs {b}{name}"
                );
            }
        }
    }

    #[test]
    fn the_legacy_spellings_are_still_names() {
        for legacy in [
            "ret",
            "return",
            "spc",
            "bs",
            "del",
            "esc",
            "pgup",
            "pgdn",
            "shift+tab",
        ] {
            assert!(
                name_to_code(legacy, None).is_some(),
                "{legacy:?} stopped being a name"
            );
        }
    }

    #[test]
    fn the_spellings_of_shift_tab_are_one_keystroke() {
        let spellings = ["BackTab", "S-BackTab", "S-Tab", "Shift+Tab", "S-Shift+Tab"];
        let parsed: Vec<_> = spellings.iter().map(|s| Key::parse(s, None)).collect();
        assert!(parsed[0].is_some());
        assert!(
            parsed.windows(2).all(|w| w[0] == w[1]),
            "{spellings:?} parsed as {parsed:?}"
        );
        for built in [
            Key::new(KeyCode::Tab, KeyModifiers::SHIFT),
            Key::new(KeyCode::BackTab, KeyModifiers::SHIFT),
            Key::new(KeyCode::BackTab, KeyModifiers::NONE),
        ] {
            assert_eq!(built, Key::parse("Shift+Tab", None).unwrap());
            assert_eq!(built.mods(), KeyModifiers::NONE);
        }
    }

    #[test]
    fn an_uppercase_character_carries_shift() {
        for c in ['f', 'c', 'w', 'z'] {
            let upper = c.to_ascii_uppercase().to_string();
            assert_eq!(
                Key::parse(&upper, None),
                Some(Key::new(KeyCode::Char(c), KeyModifiers::SHIFT)),
                "{upper:?}"
            );
            assert_eq!(
                Key::parse(&c.to_string(), None),
                Some(Key::new(KeyCode::Char(c), KeyModifiers::NONE))
            );
        }
        assert_eq!(
            Key::parse("F1", None),
            Some(Key::new(KeyCode::F(1), KeyModifiers::NONE))
        );
    }

    #[test]
    fn a_prefix_needs_a_key_after_it() {
        assert_eq!(
            Key::parse("-", None),
            Some(Key::new(KeyCode::Char('-'), KeyModifiers::NONE))
        );
        assert_eq!(
            Key::parse("C--", None),
            Some(Key::new(KeyCode::Char('-'), KeyModifiers::CONTROL))
        );
    }

    #[test]
    fn an_unparseable_press_fails_the_whole_sequence() {
        for empty in ["", "   "] {
            assert_eq!(KeySeq::parse(empty, None), None);
        }
        assert_eq!(Key::parse("nosuchkey", None), None);
        assert_eq!(KeySeq::parse("C-x nosuchkey", None), None);
        assert_eq!(KeySeq::parse("nosuchkey C-x", None), None);
    }

    #[test]
    fn an_extra_family_extends_but_does_not_redefine() {
        fn extra(name: &str) -> Option<KeyCode> {
            match name {
                "kp_begin" => Some(KeyCode::KeypadBegin),
                // Deliberately collides with a name the shared tables own.
                "enter" => Some(KeyCode::F(24)),
                _ => None,
            }
        }
        assert_eq!(Key::parse("kp_begin", None), None, "not a shared name");
        assert_eq!(
            Key::parse("C-kp_begin", Some(extra)),
            Some(Key::new(KeyCode::KeypadBegin, KeyModifiers::CONTROL))
        );
        assert_eq!(
            Key::parse("enter", Some(extra)).map(|k| k.code),
            Some(KeyCode::Enter),
            "the shared tables win"
        );
    }
}
