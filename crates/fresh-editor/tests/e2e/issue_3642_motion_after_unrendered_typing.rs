//! Home and Down right after typing, before a frame is drawn (#3642, item 3).
//!
//! Input is handled in batches between frames, so fast typing followed by
//! Home or Down reaches the editor with no frame drawn after the typing. Home
//! and Down walk the rows the last frame drew, and those still showed the
//! line before the typing: Home landed on a byte of the line below, Down in
//! the wrong column, and the next text went in there.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Type `text` with no frame drawn after each key, the way a batch of input
/// arrives between two frames.
fn type_unrendered(harness: &mut EditorTestHarness, text: &str) {
    for ch in text.chars() {
        harness
            .send_key_without_drain(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
}

fn harness_on_one_two() -> (EditorTestHarness, crate::common::fixtures::TestFixture) {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness.load_buffer_from_text("one\ntwo").unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    (harness, fixture)
}

#[test]
fn home_after_unrendered_typing_goes_to_line_start() {
    let (mut harness, _fixture) = harness_on_one_two();

    type_unrendered(&mut harness, "ZAP");
    harness
        .send_key_without_drain(KeyCode::Home, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("zap").unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("zaponeZAP");
    harness.assert_buffer_content("zaponeZAP\ntwo");
}

#[test]
fn down_after_unrendered_typing_keeps_the_column() {
    let (mut harness, _fixture) = harness_on_one_two();

    type_unrendered(&mut harness, "ZAP");
    harness
        .send_key_without_drain(KeyCode::Down, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("zap").unwrap();
    harness.render().unwrap();

    // The caret was at the end of `oneZAP`, column 6; `two` is shorter, so
    // Down lands at its end.
    harness.assert_screen_contains("twozap");
    harness.assert_buffer_content("oneZAP\ntwozap");
}

/// Down keeps the column the typing took the caret to, onto a line long
/// enough to have it.
#[test]
fn down_after_unrendered_typing_lands_on_the_typed_column() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text("one\ntwoXXXXXXX").unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    type_unrendered(&mut harness, "ZAP");
    harness
        .send_key_without_drain(KeyCode::Down, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("!").unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("twoXXX!XXXX");
    harness.assert_buffer_content("oneZAP\ntwoXXX!XXXX");
}

/// A paragraph that wraps onto several rows, then a short line.
const PARAGRAPH: &str = "alpha bravo charlie delta echo foxtrot golf hotel india juliet \
                         kilo lima mike november oscar papa quebec romeo sierra tango";

fn harness_on_wrapped_paragraph() -> (EditorTestHarness, crate::common::fixtures::TestFixture) {
    let mut harness = EditorTestHarness::new(40, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text(&format!("{PARAGRAPH}\nNEXTLINE"))
        .unwrap();
    harness.render().unwrap();
    (harness, fixture)
}

/// On a wrapped line, Home after typing goes to the start of the caret's
/// row, not of the whole line: the rows are mapped through the typing, not
/// dropped for whole-line motion.
#[test]
fn home_after_unrendered_typing_on_a_wrapped_row_goes_to_the_row_start() {
    let (mut harness, _fixture) = harness_on_wrapped_paragraph();
    // The second row of the paragraph
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let row_start = harness.cursor_position();
    assert!(row_start > 0, "the paragraph wraps");
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    type_unrendered(&mut harness, "x");
    harness
        .send_key_without_drain(KeyCode::Home, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Y").unwrap();
    harness.render().unwrap();

    let text = harness.get_buffer_content().unwrap();
    assert_eq!(&text[row_start..row_start + 1], "Y", "{text}");
    assert!(text.starts_with("alpha"), "{text}");
}

/// On a wrapped line, Down after typing goes to the next row of the same
/// line, not to the next line.
#[test]
fn down_after_unrendered_typing_on_a_wrapped_row_goes_to_the_next_row() {
    let (mut harness, _fixture) = harness_on_wrapped_paragraph();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    type_unrendered(&mut harness, "x");
    harness
        .send_key_without_drain(KeyCode::Down, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Y").unwrap();
    harness.render().unwrap();

    let text = harness.get_buffer_content().unwrap();
    let (paragraph, next) = text.split_once('\n').unwrap();
    assert_eq!(next, "NEXTLINE");
    assert!(paragraph.contains('Y'), "{text}");
}
