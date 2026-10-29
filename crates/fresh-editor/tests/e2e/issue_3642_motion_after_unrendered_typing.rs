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
