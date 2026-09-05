//! Media and standalone-modifier keys, named once.
//!
//! The same relation [`crate::keypad`] describes for the numeric keypad, for
//! the two remaining families of kitty functional keys: the media transport
//! and volume keys, and the modifier keys reported *as keys* when a terminal
//! is asked for all key events.
//!
//! They are here for the reason the keypad is: without a name, a key the
//! parser decodes is a key the keybinding editor writes back as a `{:?}`
//! spelling — `"Media(MuteVolume)"` — that its own loader then refuses,
//! dropping the binding at load with a warning badge and no explanation.
//! That is issue #1128 exactly, and naming the keys the decoder already knows
//! is what stops it being re-reported for the next family.
//!
//! **Unlike the keypad, these are not aliases.** No main-keyboard key means
//! "mute" or "left hyper", so each name here binds a key of its own — and
//! only a terminal that reports it (kitty's protocol, with key events for
//! modifiers enabled) will ever deliver one.

use crossterm::event::{KeyCode, MediaKeyCode, ModifierKeyCode};

/// One named key, in the two vocabularies that have to agree about it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NamedKey {
    /// The kitty keyboard protocol's Private Use Area codepoint.
    pub kitty_codepoint: u32,
    /// The name accepted in config, lowercased.
    pub keysym: &'static str,
    /// What the key decodes to.
    pub code: KeyCode,
}

/// The media transport and volume keys, in kitty-codepoint order.
pub const MEDIA_KEYS: &[NamedKey] = &[
    NamedKey {
        kitty_codepoint: 57428,
        keysym: "media_play",
        code: KeyCode::Media(MediaKeyCode::Play),
    },
    NamedKey {
        kitty_codepoint: 57429,
        keysym: "media_pause",
        code: KeyCode::Media(MediaKeyCode::Pause),
    },
    NamedKey {
        kitty_codepoint: 57430,
        keysym: "media_play_pause",
        code: KeyCode::Media(MediaKeyCode::PlayPause),
    },
    NamedKey {
        kitty_codepoint: 57431,
        keysym: "media_reverse",
        code: KeyCode::Media(MediaKeyCode::Reverse),
    },
    NamedKey {
        kitty_codepoint: 57432,
        keysym: "media_stop",
        code: KeyCode::Media(MediaKeyCode::Stop),
    },
    NamedKey {
        kitty_codepoint: 57433,
        keysym: "media_fast_forward",
        code: KeyCode::Media(MediaKeyCode::FastForward),
    },
    NamedKey {
        kitty_codepoint: 57434,
        keysym: "media_rewind",
        code: KeyCode::Media(MediaKeyCode::Rewind),
    },
    NamedKey {
        kitty_codepoint: 57435,
        keysym: "media_next",
        code: KeyCode::Media(MediaKeyCode::TrackNext),
    },
    NamedKey {
        kitty_codepoint: 57436,
        keysym: "media_previous",
        code: KeyCode::Media(MediaKeyCode::TrackPrevious),
    },
    NamedKey {
        kitty_codepoint: 57437,
        keysym: "media_record",
        code: KeyCode::Media(MediaKeyCode::Record),
    },
    NamedKey {
        kitty_codepoint: 57438,
        keysym: "volume_down",
        code: KeyCode::Media(MediaKeyCode::LowerVolume),
    },
    NamedKey {
        kitty_codepoint: 57439,
        keysym: "volume_up",
        code: KeyCode::Media(MediaKeyCode::RaiseVolume),
    },
    NamedKey {
        kitty_codepoint: 57440,
        keysym: "volume_mute",
        code: KeyCode::Media(MediaKeyCode::MuteVolume),
    },
];

/// The modifier keys, reported as keys in their own right, in
/// kitty-codepoint order.
pub const MODIFIER_KEYS: &[NamedKey] = &[
    NamedKey {
        kitty_codepoint: 57441,
        keysym: "left_shift",
        code: KeyCode::Modifier(ModifierKeyCode::LeftShift),
    },
    NamedKey {
        kitty_codepoint: 57442,
        keysym: "left_ctrl",
        code: KeyCode::Modifier(ModifierKeyCode::LeftControl),
    },
    NamedKey {
        kitty_codepoint: 57443,
        keysym: "left_alt",
        code: KeyCode::Modifier(ModifierKeyCode::LeftAlt),
    },
    NamedKey {
        kitty_codepoint: 57444,
        keysym: "left_super",
        code: KeyCode::Modifier(ModifierKeyCode::LeftSuper),
    },
    NamedKey {
        kitty_codepoint: 57445,
        keysym: "left_hyper",
        code: KeyCode::Modifier(ModifierKeyCode::LeftHyper),
    },
    NamedKey {
        kitty_codepoint: 57446,
        keysym: "left_meta",
        code: KeyCode::Modifier(ModifierKeyCode::LeftMeta),
    },
    NamedKey {
        kitty_codepoint: 57447,
        keysym: "right_shift",
        code: KeyCode::Modifier(ModifierKeyCode::RightShift),
    },
    NamedKey {
        kitty_codepoint: 57448,
        keysym: "right_ctrl",
        code: KeyCode::Modifier(ModifierKeyCode::RightControl),
    },
    NamedKey {
        kitty_codepoint: 57449,
        keysym: "right_alt",
        code: KeyCode::Modifier(ModifierKeyCode::RightAlt),
    },
    NamedKey {
        kitty_codepoint: 57450,
        keysym: "right_super",
        code: KeyCode::Modifier(ModifierKeyCode::RightSuper),
    },
    NamedKey {
        kitty_codepoint: 57451,
        keysym: "right_hyper",
        code: KeyCode::Modifier(ModifierKeyCode::RightHyper),
    },
    NamedKey {
        kitty_codepoint: 57452,
        keysym: "right_meta",
        code: KeyCode::Modifier(ModifierKeyCode::RightMeta),
    },
    NamedKey {
        kitty_codepoint: 57453,
        keysym: "iso_level3_shift",
        code: KeyCode::Modifier(ModifierKeyCode::IsoLevel3Shift),
    },
    NamedKey {
        kitty_codepoint: 57454,
        keysym: "iso_level5_shift",
        code: KeyCode::Modifier(ModifierKeyCode::IsoLevel5Shift),
    },
];

/// Both tables, which is what every lookup here walks.
pub fn all() -> impl Iterator<Item = &'static NamedKey> {
    MEDIA_KEYS.iter().chain(MODIFIER_KEYS)
}

/// The [`KeyCode`] a config name means, or `None` when it names nothing here.
///
/// `name` is expected already lowercased, as [`crate::keypad::code_for_keysym`]
/// expects it.
pub fn code_for_keysym(name: &str) -> Option<KeyCode> {
    all().find(|k| k.keysym == name).map(|k| k.code)
}

/// The [`KeyCode`] a kitty Private Use Area codepoint means.
pub fn code_for_kitty_codepoint(cp: u32) -> Option<KeyCode> {
    all().find(|k| k.kitty_codepoint == cp).map(|k| k.code)
}

/// The config name for a [`KeyCode`] this module owns.
///
/// The inverse of [`code_for_keysym`], and the reason the keybinding editor
/// can record one of these keys without writing a name its own loader
/// refuses.
pub fn keysym_for_code(code: KeyCode) -> Option<&'static str> {
    all().find(|k| k.code == code).map(|k| k.keysym)
}

/// First codepoint either table names — the media keys begin here.
pub const FIRST: u32 = 57428;
/// Last codepoint either table names — the modifier keys end here.
pub const LAST: u32 = 57454;

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn both_lookups_find_every_entry() {
        for k in all() {
            assert_eq!(code_for_keysym(k.keysym), Some(k.code), "{}", k.keysym);
            assert_eq!(
                code_for_kitty_codepoint(k.kitty_codepoint),
                Some(k.code),
                "{}",
                k.keysym
            );
            assert_eq!(keysym_for_code(k.code), Some(k.keysym), "{}", k.keysym);
        }
    }

    #[test]
    fn names_and_codepoints_and_codes_are_unique() {
        let mut names = HashSet::new();
        let mut points = HashSet::new();
        let mut codes = HashSet::new();
        for k in all() {
            assert!(names.insert(k.keysym), "duplicate name {}", k.keysym);
            assert!(
                points.insert(k.kitty_codepoint),
                "duplicate codepoint {}",
                k.kitty_codepoint
            );
            assert!(codes.insert(k.code), "duplicate code for {}", k.keysym);
            assert_eq!(
                k.keysym.to_lowercase(),
                k.keysym,
                "names are matched lowercased: {}",
                k.keysym
            );
        }
    }

    /// The two families are contiguous and adjacent, which is what lets the
    /// decoder delegate one range rather than twenty-seven arms.
    #[test]
    fn the_range_is_contiguous() {
        let points: Vec<u32> = all().map(|k| k.kitty_codepoint).collect();
        assert_eq!(points.first(), Some(&FIRST));
        assert_eq!(points.last(), Some(&LAST));
        for pair in points.windows(2) {
            assert_eq!(pair[1], pair[0] + 1, "gap at {}", pair[0]);
        }
    }
}
