//! Property-based tests for [`InputParser`].
//!
//! These complement the example-based tests in `tests.rs` with generated
//! inputs. The headline property is **chunk-invariance**: the parser is a pure
//! function of the *concatenated* byte stream, independent of how that stream
//! is split across `parse()` calls. That is the formal statement of the
//! sinelaw/fresh#2745 fix — "a sequence split across two reads parses exactly
//! as if it arrived whole."

use super::*;
use proptest::prelude::*;

/// Split `stream` into consecutive chunks at the given (unsorted, unclamped)
/// cut positions and feed them through one parser, collecting all events.
fn parse_chunked(stream: &[u8], cuts: &[usize]) -> Vec<Event> {
    let mut bounds: Vec<usize> = cuts.iter().map(|&c| c % (stream.len() + 1)).collect();
    bounds.sort_unstable();
    let mut p = InputParser::new();
    let mut events = Vec::new();
    let mut prev = 0;
    for &b in &bounds {
        events.extend(p.parse(&stream[prev..b]));
        prev = b;
    }
    events.extend(p.parse(&stream[prev..]));
    events
}

/// Any Char key whose byte is a structural byte of a mouse/CSI sequence
/// (`[ ] < ; M m` or an ASCII digit) — the bytes that leaked in #2745.
fn structural_char_leaks(events: &[Event]) -> usize {
    events
        .iter()
        .filter(|e| {
            matches!(
                e,
                Event::Key(ke)
                    if matches!(ke.code, KeyCode::Char('[' | ']' | '<' | ';' | 'M' | 'm' | '0'..='9'))
            )
        })
        .count()
}

fn mouse_events(events: &[Event]) -> Vec<&MouseEvent> {
    events
        .iter()
        .filter_map(|e| match e {
            Event::Mouse(me) => Some(me),
            _ => None,
        })
        .collect()
}

proptest! {
    /// **Chunk-invariance.** For any byte stream and any splitting into chunks,
    /// the events are identical to parsing the whole stream at once. This is
    /// the core guarantee that read boundaries cannot affect the result.
    #[test]
    fn chunk_invariance(stream in proptest::collection::vec(any::<u8>(), 0..512),
                        cuts in proptest::collection::vec(any::<usize>(), 0..16)) {
        let whole = InputParser::new().parse(&stream);
        let chunked = parse_chunked(&stream, &cuts);
        prop_assert_eq!(whole, chunked);
    }

    /// **Never panics** on arbitrary bytes delivered in arbitrary chunks.
    #[test]
    fn never_panics(stream in proptest::collection::vec(any::<u8>(), 0..1024),
                    cuts in proptest::collection::vec(any::<usize>(), 0..32)) {
        let _ = parse_chunked(&stream, &cuts);
    }

    /// A well-formed **SGR mouse** report parses to exactly one mouse event with
    /// the right 0-indexed coordinates, at every split point, with no leak.
    #[test]
    fn wellformed_sgr_mouse_parses_at_any_split(
        cb in 0u16..=255, cx in 1u16..=4000, cy in 1u16..=4000,
        press in any::<bool>(), cut in 0usize..64,
    ) {
        let final_byte = if press { 'M' } else { 'm' };
        let seq = format!("\x1b[<{cb};{cx};{cy}{final_byte}").into_bytes();
        let cut = cut % (seq.len() + 1);
        let mut p = InputParser::new();
        let mut ev = p.parse(&seq[..cut]);
        ev.extend(p.parse(&seq[cut..]));

        prop_assert_eq!(structural_char_leaks(&ev), 0, "leak: {:?}", ev);
        let mice = mouse_events(&ev);
        prop_assert_eq!(mice.len(), 1, "expected 1 mouse event, got {:?}", ev);
        prop_assert_eq!(mice[0].column, cx - 1);
        prop_assert_eq!(mice[0].row, cy - 1);
    }

    /// A well-formed **X10 mouse** report (three coordinate bytes, each a valid
    /// `value + 32` byte `>= 0x20`) parses to exactly one mouse event at every
    /// split point, with no structural leak.
    #[test]
    fn wellformed_x10_mouse_parses_at_any_split(
        b0 in 0x20u8..=0xff, b1 in 0x20u8..=0xff, b2 in 0x20u8..=0xff,
        cut in 0usize..8,
    ) {
        let seq = [0x1b, b'[', b'M', b0, b1, b2];
        let cut = cut % (seq.len() + 1);
        let mut p = InputParser::new();
        let mut ev = p.parse(&seq[..cut]);
        ev.extend(p.parse(&seq[cut..]));

        prop_assert_eq!(structural_char_leaks(&ev), 0, "leak: {:?}", ev);
        prop_assert_eq!(mouse_events(&ev).len(), 1, "expected 1 mouse event, got {:?}", ev);
    }

    /// Ground-state **printable ASCII text** (no `ESC`) passes through 1:1 — one
    /// `Key(Char)` per byte, in order, and nothing else. No printable byte is
    /// ever swallowed into or misread as a control sequence.
    #[test]
    fn ascii_text_roundtrips(text in proptest::collection::vec(0x20u8..=0x7e, 0..256)) {
        let ev = InputParser::new().parse(&text);
        prop_assert_eq!(ev.len(), text.len());
        for (e, &b) in ev.iter().zip(text.iter()) {
            match e {
                Event::Key(ke) => prop_assert_eq!(ke.code, KeyCode::Char(b as char)),
                other => prop_assert!(false, "expected Char key, got {:?}", other),
            }
        }
    }

    /// **#2745 generative core.** A *truncated* X10 report (`ESC[M` plus 0–2
    /// coordinate bytes) immediately followed by a well-formed SGR mouse report
    /// must resync: the SGR's leading `ESC` (< 0x20) can never be an X10
    /// coordinate, so the truncated report is abandoned (emitting nothing) and
    /// the SGR parses cleanly. Exactly one mouse event survives — the SGR one —
    /// and no structural bytes leak, at every split point.
    #[test]
    fn truncated_x10_then_sgr_resyncs(
        prefix_coords in proptest::collection::vec(0x20u8..=0xff, 0..3),
        cb in 0u16..=255, cx in 1u16..=4000, cy in 1u16..=4000, cut in 0usize..64,
    ) {
        let mut seq = vec![0x1b, b'[', b'M'];
        seq.extend_from_slice(&prefix_coords); // 0..=2 coord bytes: truncated
        let sgr = format!("\x1b[<{cb};{cx};{cy}M");
        let sgr_start = seq.len();
        seq.extend_from_slice(sgr.as_bytes());

        let cut = cut % (seq.len() + 1);
        let mut p = InputParser::new();
        let mut ev = p.parse(&seq[..cut]);
        ev.extend(p.parse(&seq[cut..]));

        prop_assert_eq!(structural_char_leaks(&ev), 0, "leak: {:?} (seq {:?})", ev, seq);
        let mice = mouse_events(&ev);
        prop_assert_eq!(mice.len(), 1, "expected 1 mouse (the SGR), got {:?}", ev);
        prop_assert_eq!(mice[0].column, cx - 1);
        prop_assert_eq!(mice[0].row, cy - 1);
        let _ = sgr_start;
    }

    /// Wrapping a well-formed SGR mouse report in arbitrary **ESC-free**
    /// printable noise yields exactly: one `Char` key per pre-noise byte, then
    /// the single mouse event, then one `Char` key per post-noise byte. The
    /// mouse report is neither lost nor smeared into the surrounding text, and
    /// no surrounding byte is consumed by it. (ESC-free noise cannot open a
    /// control sequence, so the report is the only sequence in the stream.)
    #[test]
    fn sgr_mouse_amid_esc_free_noise(
        pre in proptest::collection::vec(0x20u8..=0x7e, 0..64),
        post in proptest::collection::vec(0x20u8..=0x7e, 0..64),
        cb in 0u16..=255, cx in 1u16..=4000, cy in 1u16..=4000,
    ) {
        let mut seq = pre.clone();
        seq.extend_from_slice(format!("\x1b[<{cb};{cx};{cy}M").as_bytes());
        seq.extend_from_slice(&post);
        let ev = InputParser::new().parse(&seq);

        prop_assert_eq!(ev.len(), pre.len() + 1 + post.len(), "events: {:?}", ev);
        for (e, &b) in ev.iter().zip(pre.iter()) {
            prop_assert_eq!(e, &Event::Key(KeyEvent::new(KeyCode::Char(b as char), KeyModifiers::empty())));
        }
        match &ev[pre.len()] {
            Event::Mouse(me) => {
                prop_assert_eq!(me.column, cx - 1);
                prop_assert_eq!(me.row, cy - 1);
            }
            other => prop_assert!(false, "expected mouse at index {}, got {:?}", pre.len(), other),
        }
        for (e, &b) in ev[pre.len() + 1..].iter().zip(post.iter()) {
            prop_assert_eq!(e, &Event::Key(KeyEvent::new(KeyCode::Char(b as char), KeyModifiers::empty())));
        }
    }

    /// **String-type sequences never leak.** A DCS/OSC/APC/PM/SOS string with an
    /// arbitrary ESC-free body, terminated by `ST` or `BEL`, must be consumed
    /// whole — producing no key events — at every split point. This is the §5.1
    /// invariant for the string states.
    #[test]
    fn string_sequences_are_swallowed_at_any_split(
        introducer in prop::sample::select(vec![b'P', b']', b'_', b'^', b'X']),
        body in proptest::collection::vec(0x20u8..=0x7e, 0..64),
        bel in any::<bool>(),
        cut in 0usize..80,
    ) {
        let mut seq = vec![0x1b, introducer];
        seq.extend_from_slice(&body);
        if bel {
            seq.push(0x07); // BEL terminator
        } else {
            seq.extend_from_slice(b"\x1b\\"); // ST terminator
        }
        let cut = cut % (seq.len() + 1);
        let mut p = InputParser::new();
        let mut ev = p.parse(&seq[..cut]);
        ev.extend(p.parse(&seq[cut..]));
        let key_events = ev.iter().filter(|e| matches!(e, Event::Key(_))).count();
        prop_assert_eq!(key_events, 0, "string leaked keys: {:?} (seq {:02x?})", ev, seq);
    }

    /// **No Private Use Area codepoint ever becomes a `Char` key.** For any
    /// CSI-u codepoint in the PUA range (U+E000–U+F8FF), the result is either a
    /// non-`Char` functional key or nothing — never an invisible PUA glyph
    /// inserted into the buffer. This is the §5.1 invariant for kitty keys.
    #[test]
    fn pua_codepoints_never_become_char_keys(cp in 0xe000u32..=0xf8ff) {
        let seq = format!("\x1b[{cp}u").into_bytes();
        let ev = InputParser::new().parse(&seq);
        for e in &ev {
            if let Event::Key(ke) = e {
                if let KeyCode::Char(c) = ke.code {
                    prop_assert!(
                        !('\u{e000}'..='\u{f8ff}').contains(&c),
                        "cp {} produced PUA char {:?}", cp, c
                    );
                }
            }
        }
    }
}
