//! PTY utilities using portable-pty
//!
//! This module provides PTY-related utilities and helpers.

use crossterm::event::{KeyCode, KeyModifiers};

/// The modes the program inside the terminal has put it in that change how a
/// key is encoded on its way to that program.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PtyKeyModes {
    /// DECCKM (application cursor keys): unmodified arrow keys use SS3
    /// sequences (`\x1bOA`) instead of CSI (`\x1b[A`). Programs like less and
    /// git log enable this mode.
    pub app_cursor: bool,
    /// The kitty keyboard protocol (`CSI > flags u`). A program that asked for
    /// it can read the CSI-u form, which carries a modifier for keys the
    /// legacy encoding has nowhere to put one — Enter above all, where
    /// Shift+Enter and Ctrl+Enter otherwise flatten to a bare `\r`
    /// (issue #3124).
    pub kitty_keyboard: bool,
}

impl PtyKeyModes {
    /// Neither mode set: the plain legacy xterm encoding.
    pub const LEGACY: Self = Self {
        app_cursor: false,
        kitty_keyboard: false,
    };
}

/// Convert a crossterm key event to bytes to send to the PTY
///
/// This handles special keys and modifier combinations that need
/// to be sent as escape sequences or control characters.
///
/// The encoding is the legacy xterm one unless the child asked for the kitty
/// keyboard protocol: a modified key is expressed by the `1 + bits` modifier
/// parameter of its escape sequence ([`xterm_modifier_param`]) rather than a
/// CSI-u form. Keys whose legacy sequence takes no such parameter — Enter
/// chief among them — can only carry Alt, as an `ESC` prefix; every other
/// modifier on them is unrepresentable and dropped.
///
/// When [`PtyKeyModes::kitty_keyboard`] is set the child has enabled that
/// protocol with `CSI > flags u`, so a modified Enter goes out as
/// `CSI 13 ; <mods> u` — the encoding kitty itself would send — instead of
/// being flattened.
pub fn key_to_pty_bytes(
    code: KeyCode,
    modifiers: KeyModifiers,
    modes: PtyKeyModes,
) -> Option<Vec<u8>> {
    let app_cursor = modes.app_cursor;
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
        //
        // A child that enabled the kitty keyboard protocol reads the CSI-u
        // form, though, so Enter's modifiers survive for it rather than
        // flattening onto the bare `\r` — Shift+Enter arriving as plain Enter
        // is what made multi-line input impossible in Claude Code, which reads
        // Enter as "submit" and Shift+Enter as "insert a newline" (#3124).
        KeyCode::Enter => Some(match kitty_key_param(modifiers, modes) {
            Some(param) => csi(&format!("13;{param}"), b'u'),
            None => maybe_esc(alt, vec![b'\r']),
        }),
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
/// protocol is where they would survive, and fresh's emulator does not speak it
/// to the child.
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

/// The kitty keyboard protocol's modifier parameter (`1 + bits`) for a key
/// that is about to be encoded as CSI-u, or `None` when the protocol is not in
/// play or the key carries no modifier — either of which selects the legacy
/// encoding instead.
///
/// The bits differ from [`xterm_modifier_param`] past ctrl: kitty assigns
/// super = 8, hyper = 16 and meta = 32, where legacy xterm has only its own
/// meta at 8. Super and Hyper have no legacy encoding at all, so this is the
/// one path on which they survive.
fn kitty_key_param(modifiers: KeyModifiers, modes: PtyKeyModes) -> Option<u8> {
    if !modes.kitty_keyboard {
        return None;
    }
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
    if modifiers.contains(KeyModifiers::SUPER) {
        bits |= 8;
    }
    if modifiers.contains(KeyModifiers::HYPER) {
        bits |= 16;
    }
    if modifiers.contains(KeyModifiers::META) {
        bits |= 32;
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
        let bytes = key_to_pty_bytes(KeyCode::Char('a'), KeyModifiers::NONE, PtyKeyModes::LEGACY);
        assert_eq!(bytes, Some(vec![b'a']));
    }

    #[test]
    fn test_ctrl_c() {
        let bytes = key_to_pty_bytes(
            KeyCode::Char('c'),
            KeyModifiers::CONTROL,
            PtyKeyModes::LEGACY,
        );
        assert_eq!(bytes, Some(vec![0x03])); // ETX (Ctrl+C)
    }

    #[test]
    fn test_enter() {
        let bytes = key_to_pty_bytes(KeyCode::Enter, KeyModifiers::NONE, PtyKeyModes::LEGACY);
        assert_eq!(bytes, Some(vec![b'\r']));
    }

    #[test]
    fn test_tab() {
        let bytes = key_to_pty_bytes(KeyCode::Tab, KeyModifiers::NONE, PtyKeyModes::LEGACY);
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
        let bytes = key_to_pty_bytes(KeyCode::Tab, KeyModifiers::SHIFT, PtyKeyModes::LEGACY);
        assert_eq!(bytes, Some(vec![0x1b, b'[', b'Z']));
    }

    #[test]
    fn test_shift_tab_via_backtab_variant() {
        let bytes = key_to_pty_bytes(KeyCode::BackTab, KeyModifiers::NONE, PtyKeyModes::LEGACY);
        assert_eq!(bytes, Some(vec![0x1b, b'[', b'Z']));
    }

    #[test]
    fn test_arrow_keys() {
        assert_eq!(
            key_to_pty_bytes(KeyCode::Up, KeyModifiers::NONE, PtyKeyModes::LEGACY),
            Some(vec![0x1b, b'[', b'A'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Down, KeyModifiers::NONE, PtyKeyModes::LEGACY),
            Some(vec![0x1b, b'[', b'B'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Right, KeyModifiers::NONE, PtyKeyModes::LEGACY),
            Some(vec![0x1b, b'[', b'C'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Left, KeyModifiers::NONE, PtyKeyModes::LEGACY),
            Some(vec![0x1b, b'[', b'D'])
        );
    }

    #[test]
    fn test_arrow_keys_app_cursor() {
        // When DECCKM (application cursor keys) is active, unmodified arrows use SS3
        assert_eq!(
            key_to_pty_bytes(KeyCode::Up, KeyModifiers::NONE, APP_CURSOR),
            Some(vec![0x1b, b'O', b'A'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Down, KeyModifiers::NONE, APP_CURSOR),
            Some(vec![0x1b, b'O', b'B'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Right, KeyModifiers::NONE, APP_CURSOR),
            Some(vec![0x1b, b'O', b'C'])
        );
        assert_eq!(
            key_to_pty_bytes(KeyCode::Left, KeyModifiers::NONE, APP_CURSOR),
            Some(vec![0x1b, b'O', b'D'])
        );
        // Modified arrows still use CSI even with app_cursor
        assert_eq!(
            key_to_pty_bytes(KeyCode::Up, KeyModifiers::CONTROL, APP_CURSOR),
            Some(vec![0x1b, b'[', b'1', b';', b'5', b'A'])
        );
    }

    #[test]
    fn test_alt_key() {
        let bytes = key_to_pty_bytes(KeyCode::Char('x'), KeyModifiers::ALT, PtyKeyModes::LEGACY);
        assert_eq!(bytes, Some(vec![0x1b, b'x']));
    }

    /// DECCKM on, kitty keyboard protocol off.
    const APP_CURSOR: PtyKeyModes = PtyKeyModes {
        app_cursor: true,
        ..PtyKeyModes::LEGACY
    };

    /// The child asked for the kitty keyboard protocol with `CSI > 1 u`.
    const KITTY: PtyKeyModes = PtyKeyModes {
        kitty_keyboard: true,
        ..PtyKeyModes::LEGACY
    };

    /// Bytes as a printable string, so a failure reads as `ESC[1;6C` rather
    /// than a list of integers.
    fn seq(code: KeyCode, modifiers: KeyModifiers) -> String {
        String::from_utf8(
            key_to_pty_bytes(code, modifiers, PtyKeyModes::LEGACY).expect("key was dropped"),
        )
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
                key_to_pty_bytes(KeyCode::Char(key), ctrl, PtyKeyModes::LEGACY),
                Some(vec![0x1f]),
                "Ctrl+{key} should send 0x1F"
            );
        }

        // Ctrl+Alt is deliberately not asserted: it is the one part of this
        // encoding that varies by platform, since Windows reports AltGr as
        // Ctrl+Alt and routes it to the plain-character path instead.

        // Without Ctrl it is still an ordinary slash.
        assert_eq!(
            key_to_pty_bytes(
                KeyCode::Char('/'),
                KeyModifiers::empty(),
                PtyKeyModes::LEGACY
            ),
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
            key_to_pty_bytes(KeyCode::F(21), KeyModifiers::NONE, PtyKeyModes::LEGACY),
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
            key_to_pty_bytes(KeyCode::Backspace, KeyModifiers::ALT, PtyKeyModes::LEGACY),
            Some(vec![0x1b, 0x7f])
        );

        // Without Alt these are unchanged.
        assert_eq!(seq(KeyCode::Enter, KeyModifiers::NONE), "\r");
        assert_eq!(seq(KeyCode::Tab, KeyModifiers::NONE), "\t");
        assert_eq!(seq(KeyCode::Esc, KeyModifiers::NONE), "\x1b");
        assert_eq!(
            key_to_pty_bytes(KeyCode::Backspace, KeyModifiers::NONE, PtyKeyModes::LEGACY),
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
                PtyKeyModes::LEGACY
            ),
            Some(vec![0x1b, 0x03])
        );
    }

    /// A child that asked for the kitty keyboard protocol gets the CSI-u form
    /// for a modified Enter, instead of the modifier being dropped onto a bare
    /// `\r` — issue #3124. Claude Code reads Enter as "submit" and Shift+Enter
    /// as "insert a newline", so the two must not arrive identical.
    #[test]
    fn modified_enter_is_csi_u_under_the_kitty_keyboard_protocol() {
        let seq = |modifiers| {
            String::from_utf8(
                key_to_pty_bytes(KeyCode::Enter, modifiers, KITTY).expect("key was dropped"),
            )
            .expect("not utf-8")
        };

        assert_eq!(seq(KeyModifiers::SHIFT), "\x1b[13;2u");
        assert_eq!(seq(KeyModifiers::ALT), "\x1b[13;3u");
        assert_eq!(seq(KeyModifiers::CONTROL), "\x1b[13;5u");
        // The bits are additive here too.
        assert_eq!(
            seq(KeyModifiers::CONTROL | KeyModifiers::SHIFT),
            "\x1b[13;6u"
        );
        // Super and Hyper have no legacy encoding at all; CSI-u is where they
        // survive, on kitty's own bit assignment (super = 8, hyper = 16).
        assert_eq!(seq(KeyModifiers::SUPER), "\x1b[13;9u");
        assert_eq!(seq(KeyModifiers::HYPER), "\x1b[13;17u");

        // Unmodified Enter keeps its legacy byte: the protocol only asks for
        // CSI-u for keys it would otherwise be unable to describe.
        assert_eq!(seq(KeyModifiers::NONE), "\r");
    }

    /// Without the protocol the encoding is unchanged: a bare `\r`, with Alt
    /// as an `ESC` prefix. A child that never asked for CSI-u must not start
    /// receiving it.
    #[test]
    fn modified_enter_stays_legacy_without_the_kitty_keyboard_protocol() {
        assert_eq!(seq(KeyCode::Enter, KeyModifiers::SHIFT), "\r");
        assert_eq!(seq(KeyCode::Enter, KeyModifiers::CONTROL), "\r");
        assert_eq!(seq(KeyCode::Enter, KeyModifiers::ALT), "\x1b\r");
        assert_eq!(seq(KeyCode::Enter, KeyModifiers::NONE), "\r");
    }

    /// The protocol changes Enter only. Arrow keys already carried their
    /// modifiers through the legacy parameter (that they did is what narrowed
    /// #3124 to Enter), and must keep the same encoding either way.
    #[test]
    fn kitty_keyboard_protocol_leaves_other_keys_legacy() {
        let kitty = |code, modifiers| {
            String::from_utf8(key_to_pty_bytes(code, modifiers, KITTY).expect("key was dropped"))
                .expect("not utf-8")
        };

        assert_eq!(kitty(KeyCode::Right, KeyModifiers::CONTROL), "\x1b[1;5C");
        assert_eq!(kitty(KeyCode::Right, KeyModifiers::SHIFT), "\x1b[1;2C");
        assert_eq!(kitty(KeyCode::Char('a'), KeyModifiers::NONE), "a");
        assert_eq!(kitty(KeyCode::Tab, KeyModifiers::NONE), "\t");
    }

    /// A non-ASCII character behind Alt was truncated by an `as u8` cast.
    #[test]
    fn alt_non_ascii_char_keeps_its_utf8_bytes() {
        let mut expected = vec![0x1b];
        expected.extend_from_slice("é".as_bytes());
        assert_eq!(
            key_to_pty_bytes(KeyCode::Char('é'), KeyModifiers::ALT, PtyKeyModes::LEGACY),
            Some(expected)
        );
    }
}
