//! One key vocabulary: the name table, the compact string syntax, and the
//! typed value both sides resolve through.
//!
//! **The problem this exists to end.** A key reaches Fresh as a *string* from
//! several directions — the config's `key` / `modifiers` fields and its `keys`
//! chord arrays, a plugin mode's `defineMode` binding table, the widget wire —
//! and each direction used to carry its own parser over its own name table.
//! Escape answered to `esc` in one and `ESCAPE` in another; Shift+Tab was
//! spelled three ways; and the widget path *formatted* a name in one place and
//! *re-matched* the text in another, so the two halves could disagree with no
//! compile error. That is issue #1128's defect class — a name that binds and a
//! key that arrives drifting apart — one layer out from where #1128 fixed it.
//!
//! **What is shared, precisely.** The name tables below are the one vocabulary;
//! [`Key::parse`] is the one parser for the compact `C-x` / `C-x C-s` form that
//! plugin modes and the widget wire both speak. The config's split-field form
//! (a `key` name beside a `modifiers` array) is a different *surface syntax*
//! and keeps its own modifier handling — but it resolves its names here, so it
//! cannot drift from the rest.
//!
//! **Parsing widens, printing narrows.** Every spelling any of the old parsers
//! accepted is in the table as an alias, so nothing that used to parse stopped
//! parsing. Each key has exactly one canonical spelling for output — the first
//! entry in its `names` — and `round_trips` holds printing and parsing together.
//!
//! **The keypad and media families are not here.** They live in the terminal
//! input-parser crate, which the data layer neither depends on nor should: the
//! data layer has no business knowing about escape sequences. Name resolution
//! splits instead of the table moving wholesale — [`name_to_code`] answers from
//! the shared tables, and the editor layer passes its own families in as
//! [`ExtraNames`] behind them. Both are still reached only through this module.

use crossterm::event::{KeyCode, KeyModifiers};

/// A key a config entry can name in words, and the spellings it answers to.
///
/// The tables below plus the keypad table are the
/// whole accepted vocabulary, and they are data rather than match arms for two
/// reasons: the parser reads them, and so does the generator that writes the
/// table in `docs/configuration/keyboard.md`. A name that is not documented is
/// a name nobody can find.
pub struct KeyName {
    /// Accepted spellings, lowercase. The first is canonical — the one the
    /// generated documentation lists, the one [`canonical_name`] prints, and
    /// the one to prefer in examples.
    pub names: &'static [&'static str],
    /// What the name resolves to.
    pub code: KeyCode,
}

/// Keys that have a name of their own — neither a character nor the keypad.
///
/// Every spelling the keybinding editor's write-back can produce must appear
/// here, or the editor would record a binding that its own loader then
/// rejects; `config_names_round_trip` holds that.
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

/// X11 keysym spellings for ASCII punctuation.
///
/// A single-character key name is still the canonical spelling (and what the
/// keybinding editor writes back), but people reach for the X11 keysym name
/// they know — `"key": "asterisk"` is what issue #1128 was actually configured
/// with, and it bound nothing at all. JSON also makes some of these awkward to
/// write literally (`"\\"` for backslash, `"\""` for the double quote), so a
/// name is the friendlier spelling.
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

/// A name family resolved *behind* the shared tables.
///
/// The keypad and media/modifier families are decoded by the terminal
/// input-parser crate, which the data layer does not depend on. Rather than
/// drag that crate down here — or, worse, keep a second copy of those names —
/// the editor layer hands its lookup in and the one parser consults it last.
pub type ExtraNames = fn(&str) -> Option<KeyCode>;

/// Resolve an already-lowercased key name against the shared tables, then
/// against `extra` if the caller supplied one.
///
/// Order matters only in that the shared tables win: a family passed in as
/// `extra` extends the vocabulary, it never redefines a name that is already
/// in it.
pub fn name_to_code(lower: &str, extra: Option<ExtraNames>) -> Option<KeyCode> {
    NAMED_KEYS
        .iter()
        .chain(PUNCTUATION_KEYS)
        .find(|k| k.names.contains(&lower))
        .map(|k| k.code)
        .or_else(|| extra.and_then(|f| f(lower)))
}

/// The one spelling to print for `code`, if it has a name of its own.
///
/// `None` for a code the tables do not name — an ordinary character, a
/// function key, or a keypad/media code whose family lives in the editor
/// layer. [`Key::fmt_into`] handles those.
pub fn canonical_name(code: KeyCode) -> Option<&'static str> {
    NAMED_KEYS
        .iter()
        .chain(PUNCTUATION_KEYS)
        .find(|k| k.code == code)
        .and_then(|k| k.names.first().copied())
}

/// Modifier prefixes, longest first so `Super-` is not read as `S-` followed
/// by `uper-`. `M-` and `A-` are the same modifier: emacs spells Alt one way
/// and the widget vocabulary the other, and both have shipped.
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

/// The canonical prefix for each modifier, in the order they are printed, so
/// one value has one spelling. `C-S-Left` and `C-M-s` are what the
/// vocabularies that predate this module already wrote.
///
/// **Every bit a `KeyModifiers` can hold is here**, not just the four the
/// config and the plugin vocabularies use. A printer that covered only the
/// common ones would drop Hyper and Meta on the floor — and a modifier
/// silently discarded on the way to a string is precisely the quiet-drop
/// defect (issue #1128) this module exists to end. `printing_then_parsing_is_
/// the_identity` quantifies over all of them.
const PRINTED_PREFIXES: &[(&str, KeyModifiers)] = &[
    ("Super-", KeyModifiers::SUPER),
    ("Meta-", KeyModifiers::META),
    ("H-", KeyModifiers::HYPER),
    ("C-", KeyModifiers::CONTROL),
    ("M-", KeyModifiers::ALT),
    ("S-", KeyModifiers::SHIFT),
];

/// One key press: what was struck, and what was held while it was.
///
/// **The fields are private so that [`Key::new`] is the only way in.** Two
/// spellings of the same keystroke must be one value or a binding registered
/// under one cannot match a key that arrives as the other, and `new` is where
/// that folding happens (see [`Key::normalized`]). A public field would let a
/// caller build `Tab` + Shift directly and bypass it — which is not a
/// hypothetical: the generated round-trip property caught exactly that
/// construction before the fields were closed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Key {
    code: KeyCode,
    mods: KeyModifiers,
}

impl Key {
    pub fn new(code: KeyCode, mods: KeyModifiers) -> Self {
        Self { code, mods }.normalized()
    }

    /// An unmodified press, usable in a `const`.
    ///
    /// `new` cannot be const (its normalisation branches), and the surfaces
    /// that name a key they are pressing on the user's behalf want one.
    /// Restricted to no modifiers, which is exactly the case that needs no
    /// normalisation: only Shift folds into a code.
    pub const fn plain(code: KeyCode) -> Self {
        Self {
            code,
            mods: KeyModifiers::NONE,
        }
    }

    /// What was struck. Always canonical — see the type's own note.
    pub fn code(&self) -> KeyCode {
        self.code
    }

    /// What was held. Never carries a Shift that belongs to the code.
    pub fn mods(&self) -> KeyModifiers {
        self.mods
    }

    /// The `KeyEvent` this press is, for the shared text-key table and any
    /// other consumer that speaks the terminal's own type.
    pub fn to_key_event(&self) -> crossterm::event::KeyEvent {
        crossterm::event::KeyEvent::new(self.code, self.mods)
    }

    /// Fold the spellings that mean the same keystroke onto one value.
    ///
    /// **Shift+Tab is the whole of it.** A terminal delivers `BackTab`, and the
    /// lookup side strips the now-redundant Shift; a plugin writes `S-Tab`; the
    /// widget wire writes `Shift+Tab`. Three spellings, one keystroke — and
    /// while they stayed three values, a binding registered under one of them
    /// could not match a key that arrived as another. That is exactly the
    /// silently-dead binding this module exists to end, so the fold happens
    /// here, in the constructor, rather than at each of the three call sites
    /// that would otherwise have to remember it.
    fn normalized(self) -> Self {
        match (self.code, self.mods.contains(KeyModifiers::SHIFT)) {
            // `BackTab` *is* Shift+Tab, so a Shift beside it is redundant —
            // and a terminal may or may not send it. Folding both directions
            // is what makes "one keystroke, one value" true rather than true
            // for the spellings someone happened to test.
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

    /// Parse the compact form — `Left`, `C-x`, `C-S-Left`, `M-o`, `Shift+Tab`.
    ///
    /// Modifier prefixes come in any order and any case. What follows them is
    /// a name from the shared tables, a function key (`f1`), or a single
    /// character.
    ///
    /// **An uppercase single character carries Shift**, so `F` is Shift+f —
    /// the rule the plugin-mode parser has always applied, and what bindings
    /// that ship today (`["F", …]`, `["C", …]`, `["W", …]`) mean. It applies
    /// only to a bare character: a *named* key is matched case-insensitively,
    /// because `PageUp` and `pageup` are the same key and never a shifted one.
    pub fn parse(s: &str, extra: Option<ExtraNames>) -> Option<Self> {
        let mut mods = KeyModifiers::NONE;
        let mut rest = s;
        'peel: loop {
            let lower = rest.to_lowercase();
            for (prefix, m) in PREFIXES {
                if lower.starts_with(prefix) {
                    // A prefix is only a prefix while something follows it:
                    // `"-"` is a key, and `"s-"` on its own is not Shift.
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
            // `to_lowercase` on a char can yield several chars (ß, İ); the
            // single-char keys we bind never do, and taking the first is what
            // every previous parser did. Fall back to the char itself so a
            // multi-char lowering cannot silently drop the key.
            KeyCode::Char(c.to_lowercase().next().unwrap_or(c))
        } else if let Some(n) = lower.strip_prefix('f') {
            KeyCode::F(n.parse::<u8>().ok()?)
        } else {
            return None;
        };
        Some(Self::new(code, mods))
    }

    /// Write the canonical spelling of this press.
    ///
    /// The inverse of [`Key::parse`] for every value that parser can produce;
    /// `round_trips` is what holds the two together.
    fn fmt_into(&self, out: &mut String) {
        for (prefix, m) in PRINTED_PREFIXES {
            if self.mods.contains(*m) {
                out.push_str(prefix);
            }
        }
        match canonical_name(self.code) {
            Some(name) => out.push_str(name),
            None => match self.code {
                // A bare character prints as itself — except that an uppercase
                // one would parse back as Shift+it, so the shift lives in the
                // prefix and the character stays lowercase.
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

/// A non-empty sequence of presses — one key, or an emacs-style chord.
///
/// Spelled with a space between presses (`C-x C-s`), which is what a plugin
/// mode's binding table has always used and what the emacs keymap's own
/// comments describe. The separator is unambiguous because the space *key* is
/// spelled `space`, never a literal space.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KeySeq(Vec<Key>);

impl KeySeq {
    /// `None` for an empty sequence — a binding to nothing is not a binding.
    pub fn new(keys: Vec<Key>) -> Option<Self> {
        (!keys.is_empty()).then_some(Self(keys))
    }

    pub fn one(key: Key) -> Self {
        Self(vec![key])
    }

    pub fn keys(&self) -> &[Key] {
        &self.0
    }

    /// The single press this sequence is, or `None` if it is a chord.
    ///
    /// What a caller with no chord vocabulary of its own asks — which is every
    /// widget kind today. Saying so at the top of a handler makes "this kind
    /// answers single presses only" a statement rather than an accident.
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

    /// Any keystroke the parser can produce, generated rather than listed —
    /// every named key, every punctuation keysym, arbitrary characters and
    /// function keys, under an arbitrary set of modifier bits.
    ///
    /// Characters are constrained to those that are their own lowercase: an
    /// uppercase character is not a distinct value in this vocabulary (it
    /// parses as Shift + the lowercase one), and a character whose lowering
    /// is multi-char has no single-character spelling to round-trip through.
    /// Whitespace other than the space key is excluded because it would be
    /// eaten by the chord separator — the space key itself is spelled by
    /// name, and the tables cover it.
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
        /// **The property the whole module exists to provide**: format and
        /// parse are inverses. Generate a keystroke, print it, parse it back,
        /// and get the value you started with.
        ///
        /// This is what lets the widget wire hand a string across the plugin
        /// boundary and get the same keystroke back on the other side, and
        /// what stops a formatter and a matcher drifting apart with no
        /// compile error — the defect that made a panel's Shift+Tab dead.
        #[test]
        fn printing_then_parsing_is_the_identity(key in any_key()) {
            let printed = key.to_string();
            // A printed key must never contain the chord separator, or a
            // sequence could not be split back into the presses it was
            // built from. The space *key* is spelled by name, which is what
            // makes the separator safe to use at all.
            prop_assert!(!printed.contains(char::is_whitespace), "printed as {:?}", printed);
            prop_assert_eq!(Key::parse(&printed, None), Some(key), "printed as {:?}", printed);
        }

        /// The same, entered from the other side: whatever a *string* means,
        /// printing that meaning re-parses to it. Together with the property
        /// above this pins the syntax from both ends, so neither a value nor
        /// a spelling can be one the round trip does not preserve.
        #[test]
        fn parsing_then_printing_preserves_meaning(key in any_key()) {
            let printed = key.to_string();
            let reparsed = Key::parse(&printed, None).expect("just printed");
            prop_assert_eq!(reparsed.to_string(), printed);
        }

        /// A sequence is its presses, in order, however long — and survives
        /// the same round trip. The separator cannot swallow a press.
        #[test]
        fn a_sequence_round_trips_as_its_presses(keys in prop::collection::vec(any_key(), 1..6)) {
            let seq = KeySeq::new(keys.clone()).expect("non-empty");
            prop_assert_eq!(seq.keys(), keys.as_slice());
            prop_assert_eq!(seq.single(), (keys.len() == 1).then(|| keys[0]));
            let printed = seq.to_string();
            prop_assert_eq!(KeySeq::parse(&printed, None), Some(seq));
        }

        /// No modifier is lost on the way to a string. Stated separately from
        /// the round trip because a printer that dropped a bit *and* a parser
        /// that never set it would round-trip happily while quietly changing
        /// what the user bound.
        #[test]
        fn no_modifier_is_dropped_in_printing(key in any_key()) {
            let printed = key.to_string();
            let reparsed = Key::parse(&printed, None).expect("just printed");
            prop_assert_eq!(reparsed.mods(), key.mods(), "printed as {:?}", printed);
        }
    }

    /// Case is not meaning for a *named* key: `PageUp`, `pageup` and `PAGEUP`
    /// are one keystroke. (A bare character is the exception — see below.)
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

    /// Every alias resolves to the same keystroke as its canonical spelling,
    /// and no name is claimed twice. An alias that resolved elsewhere would be
    /// a name you can bind and the editor can never write back.
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

    /// Modifier prefixes commute, and the two spellings of Alt are one
    /// modifier. Order and dialect were how the older parsers differed from
    /// each other; here they cannot.
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

    /// The spellings that reached here from the vocabularies this module
    /// replaces. Their *existence* cannot be derived from the tables — it is
    /// the compatibility promise — but their behaviour is covered by the
    /// properties above, so this only asserts that each one is still a name.
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

    /// Shift+Tab is one keystroke however it is spelled. Each spelling
    /// arrived from a different vocabulary — the terminal's, a plugin mode's,
    /// the widget wire's — and while they stayed three values, a binding
    /// registered under one could not match a key that arrived as another.
    #[test]
    fn the_spellings_of_shift_tab_are_one_keystroke() {
        let spellings = ["BackTab", "S-BackTab", "S-Tab", "Shift+Tab", "S-Shift+Tab"];
        let parsed: Vec<_> = spellings.iter().map(|s| Key::parse(s, None)).collect();
        assert!(parsed[0].is_some());
        assert!(
            parsed.windows(2).all(|w| w[0] == w[1]),
            "{spellings:?} parsed as {parsed:?}"
        );
        // And Shift is folded into the code rather than left beside it, from
        // either direction, so there is only one value to match against —
        // whether the terminal sends `Tab`+Shift, `BackTab`, or `BackTab`
        // with a redundant Shift still set.
        for built in [
            Key::new(KeyCode::Tab, KeyModifiers::SHIFT),
            Key::new(KeyCode::BackTab, KeyModifiers::SHIFT),
            Key::new(KeyCode::BackTab, KeyModifiers::NONE),
        ] {
            assert_eq!(built, Key::parse("Shift+Tab", None).unwrap());
            assert_eq!(built.mods(), KeyModifiers::NONE);
        }
    }

    /// An uppercase *bare character* carries Shift — what `["F", …]` in a
    /// binding table that ships today means. This is the one place case is
    /// meaning, and it must not leak into named or function keys.
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
        // `F1` is a function key, not Shift+f with a stray 1.
        assert_eq!(
            Key::parse("F1", None),
            Some(Key::new(KeyCode::F(1), KeyModifiers::NONE))
        );
    }

    /// A modifier prefix needs something to modify: `-` is a key in its own
    /// right, and the prefix peeler must not eat it.
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

    /// Nothing is not a binding, and a chord is only as good as its worst
    /// press — a half-understood sequence must fail rather than bind short.
    #[test]
    fn an_unparseable_press_fails_the_whole_sequence() {
        for empty in ["", "   "] {
            assert_eq!(KeySeq::parse(empty, None), None);
        }
        assert_eq!(Key::parse("nosuchkey", None), None);
        assert_eq!(KeySeq::parse("C-x nosuchkey", None), None);
        assert_eq!(KeySeq::parse("nosuchkey C-x", None), None);
    }

    /// A family the data layer cannot see resolves through the hook, behind
    /// the shared tables — it extends the vocabulary and never redefines it.
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
