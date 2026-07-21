//! Regression tests for issue #2745: mouse-tracking escape sequences leak into
//! a focused embedded terminal as literal input.
//!
//! In session mode, raw terminal bytes flow through `InputParser` on the server
//! side. When a mouse report was split across two reads (or was truncated /
//! out-of-spec), the old buffer-rescan parser desynced and dumped the sequence
//! remainder as literal `Char` key events, which were then forwarded verbatim
//! to the focused pane's child pty. The state-machine parser must instead keep
//! mouse bytes as `Mouse` events (or drop malformed ones) and never surface a
//! mouse sequence's structural bytes (`[ < ; M m` / digits) as `Char` keys.
//!
//! These mirror `csi_u_session_input.rs`: the leak manifests as literal text,
//! so we both assert on the parsed event stream and drive the resulting keys
//! into the editor and check nothing leaked into the buffer.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{Event, KeyCode};
use fresh::server::input_parser::InputParser;

/// Structural bytes of a mouse/CSI sequence — the characters that leaked in
/// #2745 when a report desynced.
fn is_structural_char(c: char) -> bool {
    matches!(c, '[' | ']' | '<' | ';' | 'M' | 'm' | '0'..='9')
}

fn structural_char_leaks(events: &[Event]) -> Vec<char> {
    events
        .iter()
        .filter_map(|e| match e {
            Event::Key(ke) => match ke.code {
                KeyCode::Char(c) if is_structural_char(c) => Some(c),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

/// A well-formed mouse report split at *every* byte boundary always yields one
/// mouse event and never leaks a structural char.
#[test]
fn split_mouse_reports_never_leak_as_literal_keys() {
    let seqs: &[&[u8]] = &[
        b"\x1b[<35;67;18M",    // SGR motion
        b"\x1b[<0;10;5M",      // SGR press
        b"\x1b[<0;10;5m",      // SGR release
        b"\x1b[M\x43\x48\x34", // legacy X10
    ];
    for seq in seqs {
        for cut in 0..=seq.len() {
            let mut parser = InputParser::new();
            let mut events = parser.parse(&seq[..cut]);
            events.extend(parser.parse(&seq[cut..]));
            let leaks = structural_char_leaks(&events);
            assert!(
                leaks.is_empty(),
                "seq {:02x?} cut {cut} leaked {:?}: {:?}",
                seq,
                leaks,
                events
            );
            assert_eq!(
                events
                    .iter()
                    .filter(|e| matches!(e, Event::Mouse(_)))
                    .count(),
                1,
                "seq {:02x?} cut {cut}: expected exactly one mouse event, got {:?}",
                seq,
                events
            );
        }
    }
}

/// The heart of #2745: a truncated X10 report immediately followed by a
/// well-formed SGR report. The SGR's leading `ESC` must resync a fresh
/// sequence instead of being swallowed as an X10 coordinate — so the SGR
/// parses cleanly and nothing leaks.
#[test]
fn truncated_x10_then_sgr_does_not_leak() {
    let mut parser = InputParser::new();
    let mut events = parser.parse(b"\x1b[Mxy"); // truncated X10 (2 coord bytes)
    events.extend(parser.parse(b"\x1b[<35;41;20M")); // clean SGR
    assert!(
        structural_char_leaks(&events).is_empty(),
        "leaked: {:?}",
        events
    );
    assert_eq!(
        events
            .iter()
            .filter(|e| matches!(e, Event::Mouse(_)))
            .count(),
        1,
        "expected exactly the SGR mouse event, got {:?}",
        events
    );
}

/// End-to-end: mouse fragments fed through `InputParser`, with the resulting
/// key events forwarded into the editor, must not deposit any mouse-sequence
/// text into the buffer (mouse events themselves don't type text).
#[test]
fn mouse_sequences_not_inserted_as_literal_text() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let mut parser = InputParser::new();

    // A flood of well-formed, truncated, and split mouse input.
    let sequences: &[&[u8]] = &[
        b"hello",
        b"\x1b[<35;10;20M",
        b"\x1b[M\x43\x48\x34",
        b"\x1b[Mxy",                 // truncated X10
        b"\x1b[<35;41;20M",          // well-formed after truncation
        b"\x1b[<0;5;5M\x1b[<0;5;5m", // press + release
    ];
    for seq in sequences {
        for event in parser.parse(seq) {
            if let Event::Key(ke) = event {
                harness.send_key(ke.code, ke.modifiers).unwrap();
            }
        }
    }

    let content = harness.get_buffer_content().unwrap_or_default();
    // "hello" is legitimately typed; the mouse structural fragments must not be.
    for literal in &["[<35", "[M", ";20M", "[<0"] {
        assert!(
            !content.contains(literal),
            "mouse sequence leaked as literal text {literal:?}: {content:?}",
        );
    }
}
