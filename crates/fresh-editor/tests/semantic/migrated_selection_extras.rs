//! Migrations of `tests/e2e/selection.rs` claims not covered by
//! `migrated_selection_full.rs` or `semantic/selection.rs`.
//!
//! Focus: Shift+Up/Down line-extending selection, selection
//! reversal across the anchor, and word-selection through
//! multi-script accented graphemes.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_select_up_extends_to_previous_line() {
    // Original: `test_select_up`. Cursor at start of line 3 of
    // "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n", SelectUp once
    // selects "Line 2\n", second SelectUp adds "Line 1\n".
    assert_buffer_scenario(BufferScenario {
        description: "Two SelectUp from start of line 3 selects 'Line 1\\nLine 2\\n'".into(),
        initial_text: "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n".into(),
        actions: vec![
            Action::MoveDown,
            Action::MoveDown,
            Action::SelectUp,
            Action::SelectUp,
        ],
        expected_text: "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n".into(),
        // Cursor at byte 0 (start of line 1), anchor at byte 14
        // (start of line 3).
        expected_primary: CursorExpect::range(14, 0),
        expected_selection_text: Some("Line 1\nLine 2\n".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_down_extends_to_next_line() {
    // Original: `test_select_down`. Mirror of select_up: cursor
    // at start of line 1, two SelectDown selects "Line 1\nLine 2\n".
    assert_buffer_scenario(BufferScenario {
        description: "Two SelectDown from byte 0 selects 'Line 1\\nLine 2\\n'".into(),
        initial_text: "Line 1\nLine 2\nLine 3\n".into(),
        actions: vec![Action::SelectDown, Action::SelectDown],
        expected_text: "Line 1\nLine 2\nLine 3\n".into(),
        // Cursor lands at start of line 3 (byte 14), anchor at 0.
        expected_primary: CursorExpect::range(0, 14),
        expected_selection_text: Some("Line 1\nLine 2\n".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_down_then_up_shrinks_selection() {
    // Original: `test_select_up_down_reversal`. From line 2,
    // SelectDown × 2 then SelectUp reduces selection by one line.
    assert_buffer_scenario(BufferScenario {
        description: "SelectDown ×2 then SelectUp shrinks selection by one line".into(),
        initial_text: "Line 1\nLine 2\nLine 3\nLine 4\n".into(),
        actions: vec![
            Action::MoveDown, // cursor at byte 7 (line 2)
            Action::SelectDown,
            Action::SelectDown,
            Action::SelectUp,
        ],
        expected_text: "Line 1\nLine 2\nLine 3\nLine 4\n".into(),
        // Anchor at byte 7 (line 2), cursor at byte 14 (line 3).
        expected_primary: CursorExpect::range(7, 14),
        expected_selection_text: Some("Line 2\n".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_picks_hungarian_word() {
    // Subset of `test_select_word_accented_characters`.
    // SelectWord on a Hungarian word with diacritics.
    let word = "hibajavítás";
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord picks the entire Hungarian accented word 'hibajavítás'".into(),
        initial_text: word.into(),
        actions: vec![Action::SelectWord],
        expected_text: word.into(),
        expected_primary: CursorExpect::range(0, word.len()),
        expected_selection_text: Some(word.into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_picks_german_word() {
    let word = "Änderung";
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord picks the entire German word with umlaut".into(),
        initial_text: word.into(),
        actions: vec![Action::SelectWord],
        expected_text: word.into(),
        expected_primary: CursorExpect::range(0, word.len()),
        expected_selection_text: Some(word.into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_picks_korean_hangul() {
    let word = "안녕하세요";
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord picks the entire Korean Hangul word".into(),
        initial_text: word.into(),
        actions: vec![Action::SelectWord],
        expected_text: word.into(),
        expected_primary: CursorExpect::range(0, word.len()),
        expected_selection_text: Some(word.into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_picks_cjk_word() {
    let word = "你好世界";
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord picks the entire CJK word".into(),
        initial_text: word.into(),
        actions: vec![Action::SelectWord],
        expected_text: word.into(),
        expected_primary: CursorExpect::range(0, word.len()),
        expected_selection_text: Some(word.into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_picks_combining_diacritic_word() {
    // 'café' with combining acute (U+0301) on the 'e' instead of
    // the precomposed U+00E9. The grapheme cluster contains two
    // codepoints; word selection must include both.
    let word = "caf\u{0065}\u{0301}";
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord includes a combining diacritic at the end of the word".into(),
        initial_text: word.into(),
        actions: vec![Action::SelectWord],
        expected_text: word.into(),
        expected_primary: CursorExpect::range(0, word.len()),
        expected_selection_text: Some(word.into()),
        ..Default::default()
    });
}

/// Anti-test: drops `SelectWord`. Without it, the cursor stays at
/// byte 0 with no selection, so the word-selection-text expectation
/// must NOT match.
#[test]
fn anti_select_word_dropping_action_yields_check_err() {
    let word = "hibajavítás";
    let scenario = BufferScenario {
        description: "anti: SelectWord dropped — selection_text expectation cannot match".into(),
        initial_text: word.into(),
        actions: vec![],
        expected_text: word.into(),
        expected_primary: CursorExpect::range(0, word.len()),
        expected_selection_text: Some(word.into()),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: with no SelectWord, no selection exists; the word-text \
         expectation must NOT match"
    );
}
