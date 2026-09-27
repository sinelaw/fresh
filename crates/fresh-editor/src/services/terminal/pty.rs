//! PTY utilities using portable-pty
//!
//! This module provides PTY-related utilities and helpers.

use crossterm::event::{KeyCode, KeyModifiers};

/// Convert a crossterm key event to bytes to send to the PTY
///
/// This handles special keys and modifier combinations that need
/// to be sent as escape sequences or control characters.
///
/// The encoding is the legacy xterm one: a modified key is expressed by the
/// `1 + bits` modifier parameter of its escape sequence
/// ([`xterm_modifier_param`]) rather than a CSI-u form. A child that turned on
/// the kitty keyboard protocol gets CSI-u for the keys legacy encoding cannot
/// modify — see [`kitty_encoded_key`], which callers try first.
///
/// When `app_cursor` is true (DECCKM mode), unmodified arrow keys use SS3
/// sequences (`\x1bOA`) instead of CSI (`\x1b[A`). Programs like less and
/// git log enable this mode.
pub fn key_to_pty_bytes(
    code: KeyCode,
    modifiers: KeyModifiers,
    app_cursor: bool,
) -> Option<Vec<u8>> {
    let ctrl = modifiers.contains(KeyModifiers::CONTROL);
    let alt = modifiers.contains(KeyModifiers::ALT);
    let shift = modifiers.contains(KeyModifiers::SHIFT);

    // Ctrl+key combinations (send as control characters). Alt on top of them
    // adds the `ESC` prefix below (xterm's metaSendsEscape), so Ctrl+Alt+C is
    // `ESC 0x03` rather than a bare `c` — which is what it used to send, since
    // neither the Ctrl nor the Alt branch accepted the combination.
    //
    // Windows is the exception: crossterm reports AltGr as Ctrl+Alt, and that
    // is a plain character key, not a control sequence.
    if ctrl && !(alt && cfg!(windows)) {
        if let KeyCode::Char(c) = code {
            if let Some(ctrl_byte) = control_byte(c) {
                return Some(if alt {
                    vec![0x1b, ctrl_byte]
                } else {
                    vec![ctrl_byte]
                });
            }
        }
    }

    // Alt+key (send as ESC + key).
    if alt && !ctrl {
        if let KeyCode::Char(c) = code {
            let c = if shift { c.to_ascii_uppercase() } else { c };
            return Some(esc_prefixed(&encode_char(c)));
        }
    }

    // Handle regular keys and special keys
    match code {
        KeyCode::Char(c) => {
            let c = if shift { c.to_ascii_uppercase() } else { c };
            Some(encode_char(c))
        }
        // The keys below have no parameterised escape sequence to carry a
        // modifier, so Alt is expressed the only way legacy encoding can: an
        // `ESC` prefix (metaSendsEscape). Alt+Backspace as `ESC DEL` is
        // readline's delete-previous-word; before this, Alt was simply dropped
        // and the child saw a bare Backspace.
        KeyCode::Enter => Some(maybe_esc(alt, vec![b'\r'])),
        KeyCode::Tab => {
            if shift {
                // Shift+Tab (backtab)
                Some(vec![0x1b, b'[', b'Z'])
            } else {
                Some(maybe_esc(alt, vec![b'\t']))
            }
        }
        // Crossterm reports Shift+Tab as `KeyCode::BackTab` (with the
        // SHIFT modifier already stripped by Fresh's `normalize_key`
        // in `app/mod.rs`). Without this arm the BackTab variant
        // fell into the `_ => None` catch-all below and Shift+Tab was
        // silently dropped before reaching the PTY (issue #2029,
        // sub-bug 2).
        KeyCode::BackTab => Some(vec![0x1b, b'[', b'Z']),
        KeyCode::Backspace => {
            if ctrl {
                // Ctrl+Backspace - delete word
                Some(maybe_esc(alt, vec![0x17])) // Ctrl+W
            } else {
                Some(maybe_esc(alt, vec![0x7f])) // DEL
            }
        }
        KeyCode::Esc => Some(maybe_esc(alt, vec![0x1b])),
        KeyCode::Up => Some(cursor_key(b'A', modifiers, app_cursor)),
        KeyCode::Down => Some(cursor_key(b'B', modifiers, app_cursor)),
        KeyCode::Right => Some(cursor_key(b'C', modifiers, app_cursor)),
        KeyCode::Left => Some(cursor_key(b'D', modifiers, app_cursor)),
        // Home and End take the same modifier parameter as the arrows but keep
        // the CSI form when unmodified: DECCKM's SS3 variant is deliberately
        // not applied to them here, to leave the long-standing unmodified
        // output byte-for-byte unchanged.
        KeyCode::Home => Some(cursor_key(b'H', modifiers, false)),
        KeyCode::End => Some(cursor_key(b'F', modifiers, false)),
        KeyCode::Insert => Some(tilde_key(2, modifiers)),
        KeyCode::Delete => Some(tilde_key(3, modifiers)),
        KeyCode::PageUp => Some(tilde_key(5, modifiers)),
        KeyCode::PageDown => Some(tilde_key(6, modifiers)),
        // F1-F4 are SS3 when unmodified and take the cursor-key CSI form once
        // a modifier is involved; F5 and up are `CSI <n> ~` throughout. Every
        // one of these used to ignore its modifiers entirely, so Shift+F3
        // reached the child as a bare F3 — the same class of bug as #699, on
        // the outgoing side.
        KeyCode::F(n @ 1..=4) => {
            let final_byte = b'P' + (n - 1);
            Some(match xterm_modifier_param(modifiers) {
                Some(param) => csi(&format!("1;{param}"), final_byte),
                None => vec![0x1b, b'O', final_byte],
            })
        }
        KeyCode::F(n) => function_key_number(n).map(|num| tilde_key(num, modifiers)),
        _ => None,
    }
}

/// The kitty keyboard protocol flags a child has pushed (`CSI > flags u`),
/// as far as they concern how keys are encoded.
///
/// The emulator accepts, and reports back on a `CSI ? u` query, every flag of
/// the protocol, so each is honoured here — with one limit: the editor only
/// forwards key presses (and auto-repeats, which arrive as presses), never
/// releases, so "report event types" can only ever produce press events,
/// which the protocol encodes without an event-type field.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct KittyKeyFlags(u8);

impl KittyKeyFlags {
    /// `0b1`: Esc, and keys with Ctrl/Alt/Super…, in CSI-u form; functional
    /// keys always in their CSI form.
    pub const DISAMBIGUATE: Self = Self(1);
    /// `0b10`: press/repeat/release event types.
    pub const REPORT_EVENT_TYPES: Self = Self(2);
    /// `0b100`: the shifted key alongside the base key.
    pub const REPORT_ALTERNATE_KEYS: Self = Self(4);
    /// `0b1000`: every key, text-producing ones included, as an escape code.
    pub const REPORT_ALL_KEYS: Self = Self(8);
    /// `0b10000`: the text a key produces, embedded in its escape code.
    pub const REPORT_ASSOCIATED_TEXT: Self = Self(16);

    /// No enhancement: the child reads legacy encoding.
    pub const fn empty() -> Self {
        Self(0)
    }

    /// Whether every flag of `other` is set.
    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
}

impl std::ops::BitOr for KittyKeyFlags {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

/// A key's encoding under the kitty keyboard protocol, for a child that
/// pushed `flags`. Follows the reference encoder (kitty's `key_encoding.c`).
///
/// Legacy encoding is ambiguous in ways the protocol exists to fix: Esc is
/// the first byte of every escape sequence, Ctrl+I is Tab, Ctrl+M is Enter,
/// Ctrl+Shift+A is Ctrl+A, Alt+[ starts a CSI, Shift+Enter is Enter
/// (sinelaw/fresh#3323, sinelaw/fresh#3408). With "disambiguate" on:
///
/// - Esc is `CSI 27 u`;
/// - a text key with a modifier other than Shift is `CSI <key> ; <mods> u`,
///   where `<key>` is the unshifted (lower-case) code point: Ctrl+I is
///   `CSI 105;5u`, Alt+Shift+A is `CSI 97;4u`;
/// - modified Enter, Tab and Backspace are `CSI 13/9/127 ; <mods> u`
///   (Shift+Tab is `CSI 9;2u`); unmodified they keep their legacy bytes so a
///   shell stays usable if the program dies without popping the mode;
/// - functional keys use their CSI form even unmodified (`CSI A`, not the
///   DECCKM `SS3 A`; F3 is `CSI 13 ~`, since `CSI 1;<mods> R` collides
///   with a cursor position report), and keys with no legacy form at all —
///   F13 and up, Menu, the lock keys, media keys — get their protocol code.
///
/// "Report all keys" sends text keys (and unmodified Enter/Tab/Backspace)
/// as escape codes too, and reports the modifier keys themselves.
/// "Alternate keys" adds `:<shifted>` after the key when Shift is held, and
/// "associated text" appends the text the key types.
///
/// `None` means the legacy encoding is also the protocol's: the flags are
/// empty (or only refine escape codes nothing here produces), the key types
/// text as-is, or it is a modifier key that is only reported under "report
/// all keys" — the caller falls back to [`key_to_pty_bytes`].
pub fn kitty_encoded_key(
    code: KeyCode,
    modifiers: KeyModifiers,
    flags: KittyKeyFlags,
) -> Option<Vec<u8>> {
    let disambiguate = flags.contains(KittyKeyFlags::DISAMBIGUATE);
    let report_all = flags.contains(KittyKeyFlags::REPORT_ALL_KEYS);
    // Flags 4 and 16 only shape escape codes; on their own they leave the
    // encoding legacy, like kitty's `legacy_mode`.
    let legacy_mode =
        !disambiguate && !report_all && !flags.contains(KittyKeyFlags::REPORT_EVENT_TYPES);
    if legacy_mode {
        return None;
    }
    match code {
        KeyCode::Char(c) => kitty_text_key(c, modifiers, flags),
        KeyCode::Modifier(key) => {
            if !report_all {
                return None;
            }
            let number = kitty_modifier_key_code(key);
            Some(kitty_csi(number, None, modifiers, None, b'u'))
        }
        // Crossterm reports Shift+Tab as BackTab with SHIFT already
        // stripped (`normalize_key`); it is Tab with Shift to the protocol.
        KeyCode::BackTab => kitty_encoded_key(KeyCode::Tab, modifiers | KeyModifiers::SHIFT, flags),
        code => {
            let modified = kitty_modifier_param(modifiers).is_some();
            if !modified {
                match code {
                    KeyCode::Esc if !disambiguate && !report_all => return Some(vec![0x1b]),
                    KeyCode::Enter if !report_all => return Some(vec![b'\r']),
                    KeyCode::Tab if !report_all => return Some(vec![b'\t']),
                    KeyCode::Backspace if !report_all => return Some(vec![0x7f]),
                    _ => {}
                }
            }
            let (number, final_byte) = kitty_functional_key(code)?;
            Some(kitty_csi(number, None, modifiers, None, final_byte))
        }
    }
}

/// [`kitty_encoded_key`] for a key that types a character.
fn kitty_text_key(c: char, modifiers: KeyModifiers, flags: KittyKeyFlags) -> Option<Vec<u8>> {
    let mut modifiers = modifiers;
    // Windows reports AltGr as Ctrl+Alt: that is a character, not a chord.
    // (The protocol is not enabled on Windows today; keep the rule anyway.)
    if cfg!(windows) && modifiers.contains(KeyModifiers::CONTROL | KeyModifiers::ALT) {
        modifiers.remove(KeyModifiers::CONTROL | KeyModifiers::ALT);
    }
    // The protocol names a key by its unshifted code point and carries Shift
    // as a modifier. A legacy host reports Shift+A as `A` (usually with
    // SHIFT set, not always); an upper-case letter implies Shift either way.
    let lower = single_char(c.to_lowercase());
    let upper = single_char(c.to_uppercase());
    let has_case = lower != upper;
    if has_case && Some(c) == upper && Some(c) != lower {
        modifiers.insert(KeyModifiers::SHIFT);
    }
    let shift = modifiers.contains(KeyModifiers::SHIFT);
    let key = if has_case { lower.unwrap_or(c) } else { c };
    let shifted = (has_case && shift).then(|| upper.unwrap_or(c));
    // The text the key types: only when no modifier beyond Shift is held.
    let text_modifiers = modifiers.difference(KeyModifiers::SHIFT);
    let text = text_modifiers.is_empty().then(|| shifted.unwrap_or(key));

    if !flags.contains(KittyKeyFlags::REPORT_ALL_KEYS) {
        // Text goes out as text (the legacy bytes); anything else is a chord
        // the disambiguate level encodes, when it is on.
        if text.is_some() || !flags.contains(KittyKeyFlags::DISAMBIGUATE) {
            return None;
        }
    }
    let alternate = shifted.filter(|_| flags.contains(KittyKeyFlags::REPORT_ALTERNATE_KEYS));
    let text = text.filter(|_| {
        flags.contains(KittyKeyFlags::REPORT_ALL_KEYS)
            && flags.contains(KittyKeyFlags::REPORT_ASSOCIATED_TEXT)
    });
    Some(kitty_csi(key as u32, alternate, modifiers, text, b'u'))
}

fn single_char(mut chars: impl Iterator<Item = char>) -> Option<char> {
    let c = chars.next()?;
    chars.next().is_none().then_some(c)
}

/// `CSI <number>[:<shifted>][;<mods>[;<text>]] <final>`, omitting a bare
/// `1` number the way the protocol does (`CSI A` for an unmodified Up).
fn kitty_csi(
    number: u32,
    shifted: Option<char>,
    modifiers: KeyModifiers,
    text: Option<char>,
    final_byte: u8,
) -> Vec<u8> {
    let param = kitty_modifier_param(modifiers);
    let mut params = String::new();
    if number != 1 || shifted.is_some() || param.is_some() || text.is_some() {
        params.push_str(&number.to_string());
    }
    if let Some(shifted) = shifted {
        params.push_str(&format!(":{}", shifted as u32));
    }
    if param.is_some() || text.is_some() {
        params.push(';');
        if let Some(param) = param {
            params.push_str(&param.to_string());
        }
    }
    if let Some(text) = text {
        params.push_str(&format!(";{}", text as u32));
    }
    csi(&params, final_byte)
}

/// The protocol's number and final byte for a functional key.
fn kitty_functional_key(code: KeyCode) -> Option<(u32, u8)> {
    use crossterm::event::MediaKeyCode as M;
    Some(match code {
        KeyCode::Esc => (27, b'u'),
        KeyCode::Enter => (13, b'u'),
        KeyCode::Tab => (9, b'u'),
        KeyCode::Backspace => (127, b'u'),
        KeyCode::Insert => (2, b'~'),
        KeyCode::Delete => (3, b'~'),
        KeyCode::Left => (1, b'D'),
        KeyCode::Right => (1, b'C'),
        KeyCode::Up => (1, b'A'),
        KeyCode::Down => (1, b'B'),
        KeyCode::PageUp => (5, b'~'),
        KeyCode::PageDown => (6, b'~'),
        KeyCode::Home => (1, b'H'),
        KeyCode::End => (1, b'F'),
        KeyCode::KeypadBegin => (1, b'E'),
        KeyCode::F(1) => (1, b'P'),
        KeyCode::F(2) => (1, b'Q'),
        KeyCode::F(3) => (13, b'~'),
        KeyCode::F(4) => (1, b'S'),
        KeyCode::F(n @ 5..=12) => (function_key_number(n)?.into(), b'~'),
        KeyCode::F(n @ 13..=35) => (57376 + u32::from(n - 13), b'u'),
        KeyCode::CapsLock => (57358, b'u'),
        KeyCode::ScrollLock => (57359, b'u'),
        KeyCode::NumLock => (57360, b'u'),
        KeyCode::PrintScreen => (57361, b'u'),
        KeyCode::Pause => (57362, b'u'),
        KeyCode::Menu => (57363, b'u'),
        KeyCode::Media(media) => (
            match media {
                M::Play => 57428,
                M::Pause => 57429,
                M::PlayPause => 57430,
                M::Reverse => 57431,
                M::Stop => 57432,
                M::FastForward => 57433,
                M::Rewind => 57434,
                M::TrackNext => 57435,
                M::TrackPrevious => 57436,
                M::Record => 57437,
                M::LowerVolume => 57438,
                M::RaiseVolume => 57439,
                M::MuteVolume => 57440,
            },
            b'u',
        ),
        _ => return None,
    })
}

/// The protocol's code for a modifier key reported on its own.
fn kitty_modifier_key_code(key: crossterm::event::ModifierKeyCode) -> u32 {
    use crossterm::event::ModifierKeyCode as K;
    match key {
        K::LeftShift => 57441,
        K::LeftControl => 57442,
        K::LeftAlt => 57443,
        K::LeftSuper => 57444,
        K::LeftHyper => 57445,
        K::LeftMeta => 57446,
        K::RightShift => 57447,
        K::RightControl => 57448,
        K::RightAlt => 57449,
        K::RightSuper => 57450,
        K::RightHyper => 57451,
        K::RightMeta => 57452,
        K::IsoLevel3Shift => 57453,
        K::IsoLevel5Shift => 57454,
    }
}

/// The kitty protocol's modifier parameter: `1 + bits`, with shift = 1,
/// alt = 2, ctrl = 4, super = 8, hyper = 16, meta = 32. Unlike the legacy
/// [`xterm_modifier_param`], Super and Hyper survive. `None` when unmodified.
fn kitty_modifier_param(modifiers: KeyModifiers) -> Option<u8> {
    let bits = [
        (KeyModifiers::SHIFT, 1),
        (KeyModifiers::ALT, 2),
        (KeyModifiers::CONTROL, 4),
        (KeyModifiers::SUPER, 8),
        (KeyModifiers::HYPER, 16),
        (KeyModifiers::META, 32),
    ]
    .into_iter()
    .filter(|(modifier, _)| modifiers.contains(*modifier))
    .fold(0u8, |bits, (_, bit)| bits | bit);
    (bits != 0).then_some(1 + bits)
}

/// The control byte for Ctrl + this character, or `None` when the combination
/// has no control-character equivalent (the key is then sent as itself).
fn control_byte(c: char) -> Option<u8> {
    let c = c.to_ascii_lowercase();
    if c.is_ascii_lowercase() {
        // Ctrl+A = 0x01, Ctrl+B = 0x02, etc.
        return Some((c as u8) - b'a' + 1);
    }
    Some(match c {
        '[' | '3' => 0x1b, // Escape
        '\\' | '4' => 0x1c,
        ']' | '5' => 0x1d,
        '^' | '6' => 0x1e,
        // Ctrl+/ is the same `US` byte as Ctrl+_ and Ctrl+7. Without the '/'
        // arm the chord fell through to the plain-character path and the child
        // saw a literal `/` — which is what a kitty-protocol terminal, where
        // the chord arrives as Ctrl+/ rather than Ctrl+_, hit.
        '_' | '7' | '/' => 0x1f,
        '@' | '2' => 0x00, // NUL
        ' ' => 0x00,       // Ctrl+Space = NUL
        '?' => 0x7f,       // DEL
        _ => return None,
    })
}

/// UTF-8 bytes for a character. Multi-byte characters must go out whole — an
/// `as u8` truncation would send a mangled byte for anything non-ASCII.
fn encode_char(c: char) -> Vec<u8> {
    let mut bytes = [0u8; 4];
    c.encode_utf8(&mut bytes).as_bytes().to_vec()
}

/// Prefix `bytes` with `ESC` (xterm's metaSendsEscape encoding of Alt).
fn esc_prefixed(bytes: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(bytes.len() + 1);
    out.push(0x1b);
    out.extend_from_slice(bytes);
    out
}

/// [`esc_prefixed`] when `alt` is set, otherwise `bytes` unchanged.
fn maybe_esc(alt: bool, bytes: Vec<u8>) -> Vec<u8> {
    if alt {
        esc_prefixed(&bytes)
    } else {
        bytes
    }
}

/// `CSI <params> <final_byte>`.
fn csi(params: &str, final_byte: u8) -> Vec<u8> {
    let mut out = vec![0x1b, b'['];
    out.extend_from_slice(params.as_bytes());
    out.push(final_byte);
    out
}

/// The xterm modifier parameter for a legacy escape sequence: `1 + bits`, with
/// shift = 1, alt = 2, ctrl = 4, meta = 8. `None` means no modifier the legacy
/// encoding can express, which selects the short unparameterised form of a
/// sequence.
///
/// The bits are additive, which is the whole point: Ctrl+Shift+Right is
/// `1 + 4 + 1` = 6, not "whichever modifier was tested first". Each key used to
/// run its own if/else-if chain over the individual modifiers, so every
/// combination collapsed onto its first matching branch and reached the child
/// as a lesser chord.
///
/// Super and Hyper have no legacy xterm encoding and are dropped — the kitty
/// protocol is where they survive ([`kitty_modifier_param`]).
fn xterm_modifier_param(modifiers: KeyModifiers) -> Option<u8> {
    let mut bits = 0u8;
    if modifiers.contains(KeyModifiers::SHIFT) {
        bits |= 1;
    }
    if modifiers.contains(KeyModifiers::ALT) {
        bits |= 2;
    }
    if modifiers.contains(KeyModifiers::CONTROL) {
        bits |= 4;
    }
    if modifiers.contains(KeyModifiers::META) {
        bits |= 8;
    }
    (bits != 0).then_some(1 + bits)
}

/// A cursor-style key: `CSI 1 ; <mods> <final>` when modified, and
/// `CSI <final>` — or `SS3 <final>` under DECCKM — when not.
fn cursor_key(final_byte: u8, modifiers: KeyModifiers, app_cursor: bool) -> Vec<u8> {
    match xterm_modifier_param(modifiers) {
        Some(param) => csi(&format!("1;{param}"), final_byte),
        None if app_cursor => vec![0x1b, b'O', final_byte],
        None => vec![0x1b, b'[', final_byte],
    }
}

/// An editing/function key: `CSI <num> ; <mods> ~`, or `CSI <num> ~` unmodified.
fn tilde_key(num: u8, modifiers: KeyModifiers) -> Vec<u8> {
    match xterm_modifier_param(modifiers) {
        Some(param) => csi(&format!("{num};{param}"), b'~'),
        None => csi(&num.to_string(), b'~'),
    }
}

/// The `CSI <n> ~` number for F5 and up. F13-F20 continue the xterm sequence
/// (they used to be dropped outright); beyond F20 there is no legacy encoding,
/// so those keys are still dropped rather than mis-encoded as some other key.
fn function_key_number(n: u8) -> Option<u8> {
    Some(match n {
        5 => 15,
        6 => 17,
        7 => 18,
        8 => 19,
        9 => 20,
        10 => 21,
        11 => 23,
        12 => 24,
        13 => 25,
        14 => 26,
        15 => 28,
        16 => 29,
        17 => 31,
        18 => 32,
        19 => 33,
        20 => 34,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regular_char() {
        let bytes = key_to_pty_bytes(KeyCode::Char('a'), KeyModifiers::NONE, false);
        assert_eq!(bytes, Some(vec![b'a']));
    }

    #[test]
    fn test_ctrl_c() {
        let bytes = key_to_pty_bytes(KeyCode::Char('c'), KeyModifiers::CONTROL, false);
        assert_eq!(bytes, Some(vec![0x03])); // ETX (Ctrl+C)
    }

    #[test]
    fn test_enter() {
        let bytes = key_to_pty_bytes(KeyCode::Enter, KeyModifiers::NONE, false);
        assert_eq!(bytes, Some(vec![b'\r']));
    }

    #[test]
    fn test_tab() {
        let bytes = key_to_pty_bytes(KeyCode::Tab, KeyModifiers::NONE, false);
        assert_eq!(bytes, Some(vec![b'\t']));
    }

    /// Shift+Tab must emit the standard backtab escape sequence
    /// (`ESC [ Z`). Crossterm reports it as either
    /// `Tab + KeyModifiers::SHIFT` or as `BackTab` (with the SHIFT
    /// modifier already stripped by `normalize_key`). Both shapes
    /// must reach the PTY child as the same bytes — issue #2029
    /// sub-bug 2.
    #[test]
    fn test_shift_tab_via_tab_variant() {
        let bytes = key_to_pty_bytes(KeyCode::Tab, KeyModifiers::SHIFT, false);
        assert_eq!(bytes, Some(vec![0x1b, b'[', b'Z']));
    }

    #[test]
    fn test_shift_tab_via_backtab_variant() {
        let bytes = key_to_pty_bytes(KeyCode::BackTab, KeyModifiers::NONE, false);
        assert_eq!(bytes, Some(vec![0x1b, b'[', b'Z']));
    }

    #[test]
    fn test_arrow_keys() {
        assert_eq!(
            key_to_pty_bytes(KeyCode::Up, KeyModifiers::NONE, false),
            Some(vec![0x1b, b'[', b'A'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Down, KeyModifiers::NONE, false),
            Some(vec![0x1b, b'[', b'B'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Right, KeyModifiers::NONE, false),
            Some(vec![0x1b, b'[', b'C'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Left, KeyModifiers::NONE, false),
            Some(vec![0x1b, b'[', b'D'])
        );
    }

    #[test]
    fn test_arrow_keys_app_cursor() {
        // When DECCKM (application cursor keys) is active, unmodified arrows use SS3
        assert_eq!(
            key_to_pty_bytes(KeyCode::Up, KeyModifiers::NONE, true),
            Some(vec![0x1b, b'O', b'A'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Down, KeyModifiers::NONE, true),
            Some(vec![0x1b, b'O', b'B'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Right, KeyModifiers::NONE, true),
            Some(vec![0x1b, b'O', b'C'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Left, KeyModifiers::NONE, true),
            Some(vec![0x1b, b'O', b'D'])
        );
        // Modified arrows still use CSI even with app_cursor
        assert_eq!(
            key_to_pty_bytes(KeyCode::Up, KeyModifiers::CONTROL, true),
            Some(vec![0x1b, b'[', b'1', b';', b'5', b'A'])
        );
    }

    #[test]
    fn test_alt_key() {
        let bytes = key_to_pty_bytes(KeyCode::Char('x'), KeyModifiers::ALT, false);
        assert_eq!(bytes, Some(vec![0x1b, b'x']));
    }

    /// Bytes as a printable string, so a failure reads as `ESC[1;6C` rather
    /// than a list of integers.
    fn seq(code: KeyCode, modifiers: KeyModifiers) -> String {
        String::from_utf8(key_to_pty_bytes(code, modifiers, false).expect("key was dropped"))
            .expect("not utf-8")
    }

    /// Modifiers are additive: each key used to run its own if/else-if chain
    /// over the individual modifiers, so a combination collapsed onto its
    /// first matching branch and the child received a lesser chord —
    /// Ctrl+Shift+Right arrived as Ctrl+Right.
    #[test]
    fn combined_modifiers_are_encoded_additively() {
        let shift = KeyModifiers::SHIFT;
        let alt = KeyModifiers::ALT;
        let ctrl = KeyModifiers::CONTROL;

        assert_eq!(seq(KeyCode::Right, ctrl | shift), "\x1b[1;6C");
        assert_eq!(seq(KeyCode::Up, alt | shift), "\x1b[1;4A");
        assert_eq!(seq(KeyCode::Left, ctrl | alt), "\x1b[1;7D");
        assert_eq!(seq(KeyCode::Down, ctrl | alt | shift), "\x1b[1;8B");

        // Single modifiers keep their established encodings.
        assert_eq!(seq(KeyCode::Up, ctrl), "\x1b[1;5A");
        assert_eq!(seq(KeyCode::Up, shift), "\x1b[1;2A");
        assert_eq!(seq(KeyCode::Up, alt), "\x1b[1;3A");
    }

    /// Every spelling of the `US` chord has to reach the child as 0x1F. `/` was
    /// missing from the table, so Ctrl+/ — the way the chord arrives from a
    /// kitty-protocol terminal, and now from the legacy path too — fell through
    /// to the plain-character branch and the child saw a bare `/`.
    #[test]
    fn ctrl_slash_reaches_the_child_as_us() {
        let ctrl = KeyModifiers::CONTROL;

        for key in ['/', '_', '7'] {
            assert_eq!(
                key_to_pty_bytes(KeyCode::Char(key), ctrl, false),
                Some(vec![0x1f]),
                "Ctrl+{key} should send 0x1F"
            );
        }

        // Ctrl+Alt is deliberately not asserted: it is the one part of this
        // encoding that varies by platform, since Windows reports AltGr as
        // Ctrl+Alt and routes it to the plain-character path instead.

        // Without Ctrl it is still an ordinary slash.
        assert_eq!(
            key_to_pty_bytes(KeyCode::Char('/'), KeyModifiers::empty(), false),
            Some(vec![b'/'])
        );
    }

    /// Home/End/PageUp/PageDown/Insert accepted only Ctrl (or nothing at all),
    /// so every other modifier on them was silently dropped.
    #[test]
    fn editing_keys_carry_every_modifier() {
        assert_eq!(seq(KeyCode::Home, KeyModifiers::SHIFT), "\x1b[1;2H");
        assert_eq!(seq(KeyCode::End, KeyModifiers::ALT), "\x1b[1;3F");
        assert_eq!(seq(KeyCode::PageUp, KeyModifiers::SHIFT), "\x1b[5;2~");
        assert_eq!(
            seq(
                KeyCode::PageDown,
                KeyModifiers::CONTROL | KeyModifiers::SHIFT
            ),
            "\x1b[6;6~"
        );
        assert_eq!(seq(KeyCode::Insert, KeyModifiers::SHIFT), "\x1b[2;2~");

        // Unmodified and previously-handled forms are unchanged.
        assert_eq!(seq(KeyCode::Home, KeyModifiers::NONE), "\x1b[H");
        assert_eq!(seq(KeyCode::End, KeyModifiers::NONE), "\x1b[F");
        assert_eq!(seq(KeyCode::Insert, KeyModifiers::NONE), "\x1b[2~");
        assert_eq!(seq(KeyCode::Delete, KeyModifiers::NONE), "\x1b[3~");
        assert_eq!(seq(KeyCode::Delete, KeyModifiers::CONTROL), "\x1b[3;5~");
        assert_eq!(seq(KeyCode::Delete, KeyModifiers::SHIFT), "\x1b[3;2~");
        assert_eq!(seq(KeyCode::PageUp, KeyModifiers::CONTROL), "\x1b[5;5~");
    }

    /// `KeyCode::F(n)` ignored its modifiers entirely, so Shift+F3 reached the
    /// child as a bare F3 — the outgoing-side twin of #699.
    #[test]
    fn function_keys_carry_their_modifiers() {
        // F1-F4: SS3 unmodified, cursor-key CSI form once modified.
        assert_eq!(seq(KeyCode::F(1), KeyModifiers::NONE), "\x1bOP");
        assert_eq!(seq(KeyCode::F(3), KeyModifiers::NONE), "\x1bOR");
        assert_eq!(seq(KeyCode::F(3), KeyModifiers::SHIFT), "\x1b[1;2R");
        assert_eq!(seq(KeyCode::F(4), KeyModifiers::CONTROL), "\x1b[1;5S");

        // F5 and up: `CSI <n> ~` throughout.
        assert_eq!(seq(KeyCode::F(5), KeyModifiers::NONE), "\x1b[15~");
        assert_eq!(seq(KeyCode::F(5), KeyModifiers::CONTROL), "\x1b[15;5~");
        assert_eq!(seq(KeyCode::F(12), KeyModifiers::NONE), "\x1b[24~");
        assert_eq!(
            seq(KeyCode::F(12), KeyModifiers::ALT | KeyModifiers::SHIFT),
            "\x1b[24;4~"
        );

        // F13-F20 were dropped outright; they continue the xterm numbering.
        assert_eq!(seq(KeyCode::F(13), KeyModifiers::NONE), "\x1b[25~");
        assert_eq!(seq(KeyCode::F(20), KeyModifiers::NONE), "\x1b[34~");

        // Past F20 there is no legacy encoding — still dropped, rather than
        // mis-encoded as some other key.
        assert_eq!(
            key_to_pty_bytes(KeyCode::F(21), KeyModifiers::NONE, false),
            None
        );
    }

    /// Keys with no parameterised sequence express Alt as an `ESC` prefix
    /// (metaSendsEscape). Alt used to be dropped for all of them.
    #[test]
    fn alt_prefixes_keys_that_have_no_modifier_parameter() {
        assert_eq!(seq(KeyCode::Enter, KeyModifiers::ALT), "\x1b\r");
        assert_eq!(seq(KeyCode::Tab, KeyModifiers::ALT), "\x1b\t");
        assert_eq!(seq(KeyCode::Esc, KeyModifiers::ALT), "\x1b\x1b");
        // readline's delete-previous-word.
        assert_eq!(
            key_to_pty_bytes(KeyCode::Backspace, KeyModifiers::ALT, false),
            Some(vec![0x1b, 0x7f])
        );

        // Without Alt these are unchanged.
        assert_eq!(seq(KeyCode::Enter, KeyModifiers::NONE), "\r");
        assert_eq!(seq(KeyCode::Tab, KeyModifiers::NONE), "\t");
        assert_eq!(seq(KeyCode::Esc, KeyModifiers::NONE), "\x1b");
        assert_eq!(
            key_to_pty_bytes(KeyCode::Backspace, KeyModifiers::NONE, false),
            Some(vec![0x7f])
        );
    }

    /// Ctrl+Alt+key matched neither the Ctrl branch nor the Alt branch and fell
    /// through to the plain-character arm, so the child got a bare letter.
    #[cfg(not(windows))]
    #[test]
    fn ctrl_alt_char_is_escape_prefixed_control_byte() {
        assert_eq!(
            key_to_pty_bytes(
                KeyCode::Char('c'),
                KeyModifiers::CONTROL | KeyModifiers::ALT,
                false
            ),
            Some(vec![0x1b, 0x03])
        );
    }

    /// A non-ASCII character behind Alt was truncated by an `as u8` cast.
    #[test]
    fn alt_non_ascii_char_keeps_its_utf8_bytes() {
        let mut expected = vec![0x1b];
        expected.extend_from_slice("é".as_bytes());
        assert_eq!(
            key_to_pty_bytes(KeyCode::Char('é'), KeyModifiers::ALT, false),
            Some(expected)
        );
    }

    const DISAMBIGUATE: KittyKeyFlags = KittyKeyFlags::DISAMBIGUATE;

    /// `kitty_encoded_key` as a string, `None` when it defers to legacy.
    fn kitty_with(flags: KittyKeyFlags, code: KeyCode, mods: KeyModifiers) -> Option<String> {
        kitty_encoded_key(code, mods, flags).map(|b| String::from_utf8(b).unwrap())
    }

    fn kitty(code: KeyCode, mods: KeyModifiers) -> Option<String> {
        kitty_with(DISAMBIGUATE, code, mods)
    }

    fn some(s: &str) -> Option<String> {
        Some(s.to_string())
    }

    /// Under the kitty protocol, the modified keys legacy encoding flattens
    /// get their CSI-u form, with every modifier bit (Super included).
    #[test]
    fn kitty_encodes_modified_enter_tab_and_backspace_as_csi_u() {
        assert_eq!(
            kitty(KeyCode::Enter, KeyModifiers::SHIFT),
            some("\x1b[13;2u")
        );
        assert_eq!(
            kitty(KeyCode::Enter, KeyModifiers::CONTROL),
            some("\x1b[13;5u")
        );
        assert_eq!(
            kitty(KeyCode::Enter, KeyModifiers::ALT | KeyModifiers::SHIFT),
            some("\x1b[13;4u")
        );
        assert_eq!(
            kitty(KeyCode::Enter, KeyModifiers::SUPER),
            some("\x1b[13;9u")
        );
        assert_eq!(
            kitty(KeyCode::Tab, KeyModifiers::CONTROL),
            some("\x1b[9;5u")
        );
        assert_eq!(
            kitty(KeyCode::Backspace, KeyModifiers::CONTROL),
            some("\x1b[127;5u")
        );
        // Shift+Tab is Tab with Shift, whichever shape crossterm gives it.
        assert_eq!(kitty(KeyCode::Tab, KeyModifiers::SHIFT), some("\x1b[9;2u"));
        assert_eq!(
            kitty(KeyCode::BackTab, KeyModifiers::NONE),
            some("\x1b[9;2u")
        );
    }

    /// Unmodified Enter, Tab and Backspace keep their legacy bytes at the
    /// disambiguate level, so a shell stays usable if a program dies without
    /// popping the mode; plain text is still just text.
    #[test]
    fn kitty_disambiguate_keeps_text_and_unmodified_enter_tab_backspace() {
        assert_eq!(kitty(KeyCode::Enter, KeyModifiers::NONE), some("\r"));
        assert_eq!(kitty(KeyCode::Tab, KeyModifiers::NONE), some("\t"));
        assert_eq!(kitty(KeyCode::Backspace, KeyModifiers::NONE), some("\x7f"));
        assert_eq!(kitty(KeyCode::Char('a'), KeyModifiers::NONE), None);
        assert_eq!(kitty(KeyCode::Char('A'), KeyModifiers::SHIFT), None);
        assert_eq!(kitty(KeyCode::Char('é'), KeyModifiers::NONE), None);
    }

    /// Esc and every Ctrl/Alt chord on a text key are ambiguous in legacy
    /// encoding (Esc starts every sequence, Ctrl+I is Tab, Ctrl+Shift+A is
    /// Ctrl+A, Alt+[ starts a CSI); disambiguate sends them as CSI u, named
    /// by the unshifted key (sinelaw/fresh#3408).
    #[test]
    fn kitty_disambiguates_esc_and_ctrl_alt_chords() {
        assert_eq!(kitty(KeyCode::Esc, KeyModifiers::NONE), some("\x1b[27u"));
        assert_eq!(kitty(KeyCode::Esc, KeyModifiers::SHIFT), some("\x1b[27;2u"));
        assert_eq!(
            kitty(KeyCode::Char('a'), KeyModifiers::CONTROL),
            some("\x1b[97;5u")
        );
        assert_eq!(
            kitty(KeyCode::Char('i'), KeyModifiers::CONTROL),
            some("\x1b[105;5u")
        );
        assert_eq!(
            kitty(KeyCode::Char('m'), KeyModifiers::CONTROL),
            some("\x1b[109;5u")
        );
        assert_eq!(
            kitty(KeyCode::Char('a'), KeyModifiers::ALT),
            some("\x1b[97;3u")
        );
        assert_eq!(
            kitty(KeyCode::Char('['), KeyModifiers::ALT),
            some("\x1b[91;3u")
        );
        assert_eq!(
            kitty(KeyCode::Char(' '), KeyModifiers::CONTROL),
            some("\x1b[32;5u")
        );
        assert_eq!(
            kitty(KeyCode::Char('a'), KeyModifiers::SUPER),
            some("\x1b[97;9u")
        );
        // Shift on a letter comes as an upper-case char; the key is still `a`.
        assert_eq!(
            kitty(
                KeyCode::Char('A'),
                KeyModifiers::CONTROL | KeyModifiers::SHIFT
            ),
            some("\x1b[97;6u")
        );
        assert_eq!(
            kitty(KeyCode::Char('A'), KeyModifiers::ALT | KeyModifiers::SHIFT),
            some("\x1b[97;4u")
        );
        assert_eq!(
            kitty(KeyCode::Char('A'), KeyModifiers::CONTROL),
            some("\x1b[97;6u")
        );
        assert_eq!(
            kitty(KeyCode::Char('é'), KeyModifiers::ALT),
            some("\x1b[233;3u")
        );
        #[cfg(not(windows))]
        assert_eq!(
            kitty(
                KeyCode::Char('c'),
                KeyModifiers::CONTROL | KeyModifiers::ALT
            ),
            some("\x1b[99;7u")
        );
    }

    /// Functional keys take their CSI form even unmodified (never DECCKM's
    /// SS3), F3 avoids the cursor-position-report clash, and keys with no
    /// legacy encoding get their protocol numbers.
    #[test]
    fn kitty_encodes_functional_keys() {
        assert_eq!(kitty(KeyCode::Up, KeyModifiers::NONE), some("\x1b[A"));
        assert_eq!(kitty(KeyCode::Up, KeyModifiers::SHIFT), some("\x1b[1;2A"));
        assert_eq!(kitty(KeyCode::Home, KeyModifiers::NONE), some("\x1b[H"));
        assert_eq!(kitty(KeyCode::F(1), KeyModifiers::NONE), some("\x1b[P"));
        assert_eq!(kitty(KeyCode::F(3), KeyModifiers::NONE), some("\x1b[13~"));
        assert_eq!(
            kitty(KeyCode::F(3), KeyModifiers::SHIFT),
            some("\x1b[13;2~")
        );
        assert_eq!(
            kitty(KeyCode::F(5), KeyModifiers::CONTROL),
            some("\x1b[15;5~")
        );
        assert_eq!(
            kitty(KeyCode::F(13), KeyModifiers::NONE),
            some("\x1b[57376u")
        );
        assert_eq!(kitty(KeyCode::Delete, KeyModifiers::NONE), some("\x1b[3~"));
        assert_eq!(
            kitty(KeyCode::PageDown, KeyModifiers::ALT),
            some("\x1b[6;3~")
        );
        assert_eq!(
            kitty(KeyCode::Menu, KeyModifiers::NONE),
            some("\x1b[57363u")
        );
        assert_eq!(
            kitty(KeyCode::CapsLock, KeyModifiers::NONE),
            some("\x1b[57358u")
        );
        assert_eq!(
            kitty(
                KeyCode::Media(crossterm::event::MediaKeyCode::PlayPause),
                KeyModifiers::NONE
            ),
            some("\x1b[57430u")
        );
        // Modifier keys alone are only reported under "report all keys".
        let left_shift = KeyCode::Modifier(crossterm::event::ModifierKeyCode::LeftShift);
        assert_eq!(kitty(left_shift, KeyModifiers::SHIFT), None);
        assert_eq!(
            kitty_with(
                KittyKeyFlags::REPORT_ALL_KEYS,
                left_shift,
                KeyModifiers::SHIFT
            ),
            some("\x1b[57441;2u")
        );
    }

    /// "Report all keys as escape codes" covers text keys and unmodified
    /// Enter/Tab/Backspace too; "alternate keys" adds the shifted key and
    /// "associated text" the typed text.
    #[test]
    fn kitty_report_all_keys_alternates_and_text() {
        let all = KittyKeyFlags::DISAMBIGUATE | KittyKeyFlags::REPORT_ALL_KEYS;
        let k = |code, mods| kitty_with(all, code, mods);
        assert_eq!(k(KeyCode::Char('a'), KeyModifiers::NONE), some("\x1b[97u"));
        assert_eq!(
            k(KeyCode::Char('A'), KeyModifiers::SHIFT),
            some("\x1b[97;2u")
        );
        assert_eq!(k(KeyCode::Char('1'), KeyModifiers::NONE), some("\x1b[49u"));
        assert_eq!(k(KeyCode::Enter, KeyModifiers::NONE), some("\x1b[13u"));
        assert_eq!(k(KeyCode::Tab, KeyModifiers::NONE), some("\x1b[9u"));
        assert_eq!(k(KeyCode::Backspace, KeyModifiers::NONE), some("\x1b[127u"));
        assert_eq!(k(KeyCode::Esc, KeyModifiers::NONE), some("\x1b[27u"));

        let alt = all | KittyKeyFlags::REPORT_ALTERNATE_KEYS;
        assert_eq!(
            kitty_with(alt, KeyCode::Char('A'), KeyModifiers::SHIFT),
            some("\x1b[97:65;2u")
        );
        assert_eq!(
            kitty_with(alt, KeyCode::Char('a'), KeyModifiers::NONE),
            some("\x1b[97u")
        );
        // Alternate keys alone also shape a disambiguated chord.
        assert_eq!(
            kitty_with(
                DISAMBIGUATE | KittyKeyFlags::REPORT_ALTERNATE_KEYS,
                KeyCode::Char('A'),
                KeyModifiers::CONTROL | KeyModifiers::SHIFT
            ),
            some("\x1b[97:65;6u")
        );

        let text = all | KittyKeyFlags::REPORT_ASSOCIATED_TEXT;
        assert_eq!(
            kitty_with(text, KeyCode::Char('a'), KeyModifiers::NONE),
            some("\x1b[97;;97u")
        );
        assert_eq!(
            kitty_with(text, KeyCode::Char('A'), KeyModifiers::SHIFT),
            some("\x1b[97;2;65u")
        );
        // A chord types no text.
        assert_eq!(
            kitty_with(text, KeyCode::Char('a'), KeyModifiers::CONTROL),
            some("\x1b[97;5u")
        );
    }

    /// "Report event types" alone moves functional keys to their CSI form
    /// but leaves text chords and Esc legacy, as in kitty.
    #[test]
    fn kitty_event_types_alone() {
        let f = KittyKeyFlags::REPORT_EVENT_TYPES;
        assert_eq!(
            kitty_with(f, KeyCode::Up, KeyModifiers::NONE),
            some("\x1b[A")
        );
        assert_eq!(
            kitty_with(f, KeyCode::Esc, KeyModifiers::NONE),
            some("\x1b")
        );
        assert_eq!(
            kitty_with(f, KeyCode::Char('a'), KeyModifiers::CONTROL),
            None
        );
    }

    /// With no flags (or only flags that refine escape codes), everything is
    /// left to the legacy encoder.
    #[test]
    fn kitty_without_flags_defers_to_legacy() {
        for flags in [
            KittyKeyFlags::empty(),
            KittyKeyFlags::REPORT_ALTERNATE_KEYS | KittyKeyFlags::REPORT_ASSOCIATED_TEXT,
        ] {
            for (code, mods) in [
                (KeyCode::Esc, KeyModifiers::NONE),
                (KeyCode::Enter, KeyModifiers::SHIFT),
                (KeyCode::Char('a'), KeyModifiers::CONTROL),
                (KeyCode::Up, KeyModifiers::NONE),
            ] {
                assert_eq!(kitty_with(flags, code, mods), None, "{code:?} {mods:?}");
            }
        }
    }
}
