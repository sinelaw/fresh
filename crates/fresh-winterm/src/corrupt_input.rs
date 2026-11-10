//! Detection of input sequences whose ESC the Windows console dropped.
//!
//! Plain byte matching, so it builds — and its tests run — on every platform;
//! only the Windows reader (`vt_input`) uses it.

use crate::win32_input::{split_report, Win32Key};

/// Strip corrupt mouse sequences from a VT byte buffer.
///
/// ## Why this exists (the full horror story)
///
/// With `ENABLE_VIRTUAL_TERMINAL_INPUT` and VT mouse tracking mode 1003
/// (all-motion), every mouse pixel movement generates an SGR escape sequence
/// like `\x1b[<35;120;45M` — approximately 15 KEY_EVENT records per movement.
/// Under heavy mouse movement, the Windows console (conhost.exe / Windows
/// Terminal) sporadically fails to write the ESC (0x1b) KEY_EVENT record into
/// the console input buffer, even when the buffer is nearly empty (confirmed
/// by raw INPUT_RECORD hex dumps showing 10-record batches starting with
/// `[<35;...M` instead of `\x1b[<35;...M`). Neovim avoids this by defaulting
/// to mode 1002 (cell-motion, far fewer events).
///
/// Approaches that did NOT fix the problem:
/// - Bulk reads (4096, 16384 records) — still drops
/// - One-at-a-time reads matching libuv — made it worse (more syscalls)
/// - Dropping ENABLE_EXTENDED_FLAGS — no change
/// - Using native ENABLE_MOUSE_INPUT instead of VT — terminal still sends
///   VT mouse sequences alongside native events (double delivery), and
///   disabling VT mouse tracking (\x1b[?1003l) also kills native mouse
/// - Increasing read buffer size — drops happen with <20 records pending
/// - Never flushing ESC on timeout in InputParser — doesn't help because
///   the ESC byte is never delivered by ReadConsoleInputW in the first place
///
/// The fix: detect and strip corrupt sequences. A corrupted mouse sequence
/// is `[<digits;digits;digits[Mm]` without a leading `\x1b`. These arrive
/// in a single ReadConsoleInputW batch (all KEY_EVENTs from one mouse
/// movement at once). A human typing `[<35;42;5M` would produce individual
/// keystrokes across many separate ReadConsoleInputW calls, so this pattern
/// in a single batch is unambiguously a corrupt mouse sequence.
///
/// See `docs/internal/windows-mouse-input.md` for the complete investigation.
///
/// A win32-input-mode key report (`[Vk;Sc;Uc;Kd;Cs;Rc_` once its ESC is gone)
/// is written to the input buffer the same way, one KEY_EVENT record per byte,
/// so with `win32_reports` it is stripped the same way when its ESC goes
/// missing. That is only safe while the terminal really reports keys in that
/// mode: then a typed `[` arrives wrapped in a report of its own. Pasted text
/// does not, and neither does anything on a terminal that ignored the
/// request, so the match is kept narrow (see [`match_win32_input`]) and is
/// only tried once the mode is known to be on ([`CorruptInputFilter`]).
fn strip_corrupt_mouse(mut bytes: Vec<u8>, win32_reports: bool) -> Vec<u8> {
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'[' && (i == 0 || bytes[i - 1] != 0x1b) {
            // `[<digits;digits;digits[Mm]` (mouse) or a win32-input-mode report.
            if let Some(seq_len) = match_sgr_mouse(&bytes[i..]).or_else(|| {
                win32_reports
                    .then(|| match_win32_input(&bytes[i..]))
                    .flatten()
            }) {
                tracing::debug!(
                    "Stripped corrupt input sequence ({} bytes) at offset {}",
                    seq_len,
                    i,
                );
                bytes.drain(i..i + seq_len);
                continue; // Don't advance i — new content shifted into this position
            }
        }
        i += 1;
    }
    bytes
}

/// Check if bytes starting at `data` match a win32-input-mode report without
/// its ESC, `[Vk;Sc;Uc;Kd;Cs;Rc_`, and return its length.
///
/// What the stripper deletes has to be a key report and not text that merely
/// looks like one, so this asks for more than the parser does: the fields of
/// a KEY_EVENT record, each in its range ([`Win32Key::parse_exact`]).
/// `[1;2;3;4;5;6_` and `[;;;;;_` are text by these rules; a real report such
/// as `[69;18;5;1;24;1_` (Ctrl+Shift+E) is not.
fn match_win32_input(data: &[u8]) -> Option<usize> {
    let (params, len) = split_report(data.strip_prefix(b"[")?)?;
    Win32Key::parse_exact(params)?;
    Some(1 + len)
}

/// Whether `bytes` holds an intact win32-input-mode key report, ESC included.
fn contains_win32_report(bytes: &[u8]) -> bool {
    bytes
        .windows(2)
        .enumerate()
        .any(|(i, w)| w == b"\x1b[" && match_win32_input(&bytes[i + 1..]).is_some())
}

/// [`strip_corrupt_mouse`] with what the reader has learned so far about the
/// terminal.
///
/// fresh asks for win32-input-mode, but a terminal that does not know it (the
/// classic console host, for one) ignores the request, and then no key ever
/// arrives as a report. Stripping the report pattern there could only ever
/// remove real text. So the pattern is left alone until the terminal has sent
/// one intact report, which shows the mode is on; from then on it is stripped.
#[derive(Debug, Default)]
pub(crate) struct CorruptInputFilter {
    win32_reports: bool,
}

impl CorruptInputFilter {
    pub(crate) fn strip(&mut self, bytes: Vec<u8>) -> Vec<u8> {
        if !self.win32_reports && contains_win32_report(&bytes) {
            self.win32_reports = true;
        }
        strip_corrupt_mouse(bytes, self.win32_reports)
    }
}

/// Check if bytes starting at `data` match `[<digits;digits;digits[Mm]`.
/// Returns the length of the match, or None if no match.
fn match_sgr_mouse(data: &[u8]) -> Option<usize> {
    if data.len() < 6 || data[0] != b'[' || data[1] != b'<' {
        return None;
    }

    let mut pos = 2;
    let mut semicolons = 0;

    // Parse digits and semicolons: expect `digits;digits;digits`
    while pos < data.len() {
        match data[pos] {
            b'0'..=b'9' => pos += 1,
            b';' => {
                semicolons += 1;
                pos += 1;
            }
            b'M' | b'm' => {
                // Must have exactly 2 semicolons (3 numeric fields)
                if semicolons == 2 {
                    return Some(pos + 1); // Include the M/m terminator
                }
                return None;
            }
            _ => return None,
        }
    }

    None // Incomplete sequence
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_corrupt_mouse_single() {
        // `[<35;42;5M` — corrupt mouse sequence (no ESC)
        let input = b"[<35;42;5M".to_vec();
        let result = strip_corrupt_mouse(input, true);
        assert!(result.is_empty());
    }

    #[test]
    fn test_strip_corrupt_mouse_preserves_valid() {
        // `\x1b[<35;42;5M` — valid mouse sequence (has ESC)
        let input = b"\x1b[<35;42;5M".to_vec();
        let result = strip_corrupt_mouse(input, true);
        assert_eq!(result, b"\x1b[<35;42;5M");
    }

    #[test]
    fn test_strip_corrupt_mouse_mixed() {
        // Valid sequence, then corrupt sequence
        let mut input = b"\x1b[<35;10;3M".to_vec();
        input.extend_from_slice(b"[<35;42;5M");
        let result = strip_corrupt_mouse(input, true);
        assert_eq!(result, b"\x1b[<35;10;3M");
    }

    #[test]
    fn test_strip_corrupt_mouse_multiple_corrupt() {
        // Multiple corrupt sequences
        let mut input = b"[<35;10;3M".to_vec();
        input.extend_from_slice(b"[<35;42;5M");
        let result = strip_corrupt_mouse(input, true);
        assert!(result.is_empty());
    }

    #[test]
    fn test_strip_preserves_normal_bracket() {
        // `[` followed by non-`<` — not a mouse sequence
        let input = b"[hello".to_vec();
        let result = strip_corrupt_mouse(input, true);
        assert_eq!(result, b"[hello");
    }

    #[test]
    fn test_strip_corrupt_win32_input() {
        // A key report that lost its ESC, between two intact ones.
        let input = b"\x1b[9;15;0;1;8;1_[69;18;5;1;24;1_\x1b[9;15;0;0;8;1_".to_vec();
        let result = strip_corrupt_mouse(input, true);
        assert_eq!(result, b"\x1b[9;15;0;1;8;1_\x1b[9;15;0;0;8;1_");
    }

    #[test]
    fn test_strip_preserves_bracket_text_that_is_not_a_report() {
        // Too few fields, or no `_`, is not a key report.
        let input = b"[1;2_ [1;2;3;4;5;6".to_vec();
        let result = strip_corrupt_mouse(input, true);
        assert_eq!(result, b"[1;2_ [1;2;3;4;5;6");
    }

    #[test]
    fn test_strip_preserves_text_shaped_like_a_report() {
        // Six fields and an underscore, but not values a key event can have:
        // a key-down flag of 4, empty fields, a virtual-key code and repeat
        // count of 0, a field too large for its type, a seventh field.
        for text in [
            "arr[1;2;3;4;5;6_]",
            "[;;;;;_",
            "[0;0;0;0;0;0_",
            "[69;18;5;1;24;1;1_",
            "[69;18;99999;1;24;1_",
            "[69;18;5;1;512;1_",
        ] {
            let result = strip_corrupt_mouse(text.as_bytes().to_vec(), true);
            assert_eq!(result, text.as_bytes(), "{text}");
        }
    }

    #[test]
    fn test_filter_strips_reports_only_once_the_mode_is_on() {
        let mut filter = CorruptInputFilter::default();
        // A terminal that ignored the request: the pattern is text.
        let text = b"x[69;18;5;1;24;1_y".to_vec();
        assert_eq!(filter.strip(text.clone()), text);
        // The first intact report shows the mode is on, and a report that
        // lost its ESC in the same batch is stripped.
        let batch = b"\x1b[65;30;97;1;0;1_[69;18;5;1;24;1_".to_vec();
        assert_eq!(filter.strip(batch), b"\x1b[65;30;97;1;0;1_");
        // And from then on.
        assert_eq!(filter.strip(text), b"xy");
        // Corrupt mouse reports are stripped either way.
        let mut fresh = CorruptInputFilter::default();
        assert!(fresh.strip(b"[<35;42;5M".to_vec()).is_empty());
    }

    #[test]
    fn test_strip_preserves_user_typed_bracket_lt() {
        // `[<` followed by non-digits — not a mouse sequence
        let input = b"[<hello".to_vec();
        let result = strip_corrupt_mouse(input, true);
        assert_eq!(result, b"[<hello");
    }
}
