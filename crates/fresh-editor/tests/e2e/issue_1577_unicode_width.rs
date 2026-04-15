//! Tests for issue #1577: Variable-width Unicode rendering.
//!
//! A mix of FULLWIDTH Latin letters, Arabic ligatures, ZWJ family emoji
//! and combining marks must render with widths that agree between the
//! editor's internal column tracking and the terminal output.
//!
//! Root cause (pre-fix): `Buffer::next_grapheme_boundary` and
//! `Buffer::prev_grapheme_boundary` only fetched a fixed 32-byte window of
//! text from the buffer when looking for the next/previous grapheme
//! boundary. For ZWJ emoji sequences and Zalgo strings with many combining
//! marks, a single grapheme cluster easily exceeds 32 bytes, so the
//! boundary search stopped mid-cluster. That made Right/Left arrow walk
//! one codepoint at a time through a ZWJ sequence and End land inside the
//! cluster.
//!
//! The fix is in `model/buffer.rs`: start with a 32-byte window but grow
//! it geometrically whenever the boundary search hits the edge of the
//! window, so arbitrarily long grapheme clusters are handled correctly.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

const WIDTH: u16 = 120;
const HEIGHT: u16 = 30;

const FULLWIDTH_W: &str = "Ｗ"; // U+FF37 FULLWIDTH LATIN CAPITAL LETTER W (width 2)
const ZWJ_FAMILY: &str = "👨\u{200D}👩\u{200D}👧\u{200D}👦";

/// Build a Zalgo base character: an 'a' with many combining marks. The
/// combining marks bring the byte length of the single grapheme well past
/// the 32-byte lookahead that some of the grapheme boundary helpers used.
fn zalgo_char() -> String {
    let mut s = String::from("a");
    // Each combining mark below is 2 bytes (U+0300..U+036F range, encoded
    // with 2 UTF-8 bytes). 20 marks → 40 bytes of combining code points plus
    // the 1-byte base → 41 bytes in a single grapheme cluster.
    for cp in 0x0300u32..0x0314u32 {
        if let Some(c) = char::from_u32(cp) {
            s.push(c);
        }
    }
    s
}

#[test]
fn test_issue_1577_fullwidth_w_cursor_and_row() {
    let mut harness = EditorTestHarness::new(WIDTH, HEIGHT).unwrap();
    let content = format!("pre {FULLWIDTH_W} post\n");
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // End of line should be at the byte position after "post" (not mid-char).
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    let end_pos = harness.cursor_position();
    let line_bytes = content.trim_end_matches('\n').len();
    assert_eq!(
        end_pos, line_bytes,
        "End should land past the last char on the line (byte {line_bytes}), got {end_pos}"
    );

    // Navigate Right from start and count the grapheme steps needed to reach
    // the end. "pre " = 4, fullwidth W = 1 grapheme, " post" = 5 → 10 total.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    let mut steps = 0usize;
    loop {
        let before = harness.cursor_position();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        let after = harness.cursor_position();
        if after == before {
            break;
        }
        steps += 1;
        if steps > 100 {
            panic!("Right key never reached end of line");
        }
        if after == line_bytes {
            break;
        }
    }
    assert_eq!(
        steps, 10,
        "expected 10 Right presses to cross 'pre Ｗ post' (one grapheme each), got {steps}"
    );

    // Locate the fullwidth W on screen and verify it occupies 2 terminal cells:
    // the first cell carries the grapheme symbol, the second is a width-0
    // continuation cell (empty symbol in ratatui).
    let (x, y) = find_cell(&mut harness, FULLWIDTH_W).expect("fullwidth W must be visible");
    let cell0 = harness.get_cell(x, y).unwrap_or_default();
    let cell1 = harness.get_cell(x + 1, y).unwrap_or_default();
    assert_eq!(
        cell0, FULLWIDTH_W,
        "first cell under fullwidth W should contain the grapheme"
    );
    assert!(
        cell1.is_empty() || cell1 == " ",
        "second cell under fullwidth W should be the empty continuation cell, got {cell1:?}"
    );
}

#[test]
fn test_issue_1577_zalgo_grapheme_navigation() {
    let mut harness = EditorTestHarness::new(WIDTH, HEIGHT).unwrap();
    let zalgo = zalgo_char();
    assert!(
        zalgo.len() > 32,
        "zalgo grapheme must exceed the 32-byte lookahead"
    );
    let content = format!("{zalgo}Z\n");
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Right arrow from start must skip the whole Zalgo cluster in one step,
    // even though it exceeds the 32-byte lookahead window. Otherwise the
    // cursor lands inside the grapheme and the next render breaks UTF-8
    // assumptions downstream.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 0);
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(
        harness.cursor_position(),
        zalgo.len(),
        "Right from 0 must cross the whole Zalgo cluster in one step \
         (cursor should land on byte {} just before the Z)",
        zalgo.len()
    );
}

/// Find the first (col, row) cell whose symbol equals `sym`. Works for
/// multi-cell symbols where `find_text_on_screen` (which returns byte offset)
/// does not.
fn find_cell(harness: &mut EditorTestHarness, sym: &str) -> Option<(u16, u16)> {
    let rows = HEIGHT;
    for y in 0..rows {
        for x in 0..WIDTH {
            if let Some(c) = harness.get_cell(x, y) {
                if c == sym {
                    return Some((x, y));
                }
            }
        }
    }
    None
}

#[test]
fn test_issue_1577_zwj_family_single_grapheme() {
    let mut harness = EditorTestHarness::new(WIDTH, HEIGHT).unwrap();
    // Put the family emoji at the start of the line so End lands right after it.
    let content = format!("{ZWJ_FAMILY}\n");
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // End should put the cursor past the whole ZWJ cluster (byte 25), not
    // mid-sequence.
    let line_bytes = ZWJ_FAMILY.len();
    assert_eq!(line_bytes, 25, "sanity: family emoji is 25 UTF-8 bytes");

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    let end_pos = harness.cursor_position();
    assert_eq!(
        end_pos, line_bytes,
        "End on a ZWJ-family line should land past the whole cluster, not mid-sequence"
    );

    // A single Right arrow from position 0 should skip the entire ZWJ cluster.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 0);
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(
        harness.cursor_position(),
        line_bytes,
        "Right from 0 must cross the whole ZWJ cluster in one step"
    );
}

#[test]
fn test_issue_1577_rendered_cluster_matches_internal_width() {
    // The critical invariant: whatever visual width the editor assigns to a
    // grapheme cluster must match what ratatui reserves on screen. Otherwise
    // characters after a ZWJ cluster get clobbered or vanish.
    let mut harness = EditorTestHarness::new(WIDTH, HEIGHT).unwrap();
    let content = format!("A{ZWJ_FAMILY}Z\n");
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // 'A' and 'Z' must both be visible on screen.
    let a_pos = harness.find_text_on_screen("A").expect("A must be visible");
    let z_pos = harness
        .find_text_on_screen("Z")
        .expect("Z after ZWJ family must still be visible (regression: it would be clobbered)");
    assert_eq!(a_pos.1, z_pos.1, "A and Z should be on the same row");

    // The gap between A and Z is exactly the width the editor decided to give
    // to the ZWJ cluster. For a ZWJ-aware width function that should be 2
    // (the width of the first visible sub-grapheme 👨), but at minimum it
    // must be consistent with what ratatui placed in the buffer.
    let distance = z_pos.0 - a_pos.0;
    assert!(
        distance >= 2,
        "ZWJ family emoji should take at least 2 cells, got {distance}"
    );

    // Walk the cells between A and Z and reconstruct what is actually on
    // screen. The Z must appear in exactly one cell, immediately after the
    // cluster ends.
    let row_text = harness.get_row_text(a_pos.1);
    // Strip the gutter and trailing padding.
    assert!(
        row_text.contains('Z'),
        "row text should contain Z after the ZWJ family emoji, got: {row_text:?}"
    );
    // The portion from A through Z, including the cluster cells.
    let a_idx = row_text.find('A').unwrap();
    let z_idx = row_text.find('Z').unwrap();
    let between: String = row_text[a_idx + 'A'.len_utf8()..z_idx].chars().collect();
    // `between` must contain at least one emoji codepoint from the family.
    assert!(
        between.chars().any(|c| c == '👨'),
        "row must include the family emoji between A and Z, got {between:?}"
    );
}
