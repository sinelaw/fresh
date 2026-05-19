//! Migrations of `tests/e2e/selection.rs` claims not covered by
//! `migrated_selection_full.rs` or `semantic/selection.rs`.
//!
//! Focus: Shift+Up/Down line-extending selection, selection
//! reversal across the anchor, and word-selection through
//! multi-script accented graphemes.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
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

/// Issue #1332 regression coverage: SelectWord from any grapheme
/// position inside a multi-script word must select the entire
/// word, not split mid-grapheme.
///
/// Original: `test_select_word_accented_characters` (tests/e2e/
/// selection.rs:207). The e2e iterates over every grapheme of
/// 13 multi-script words. The original bug:
///   On "hibajavítás" with cursor on 'í', Ctrl+W selected only
///   "hibajav" — splitting mid-grapheme because the word-end
///   scan used codepoint indices instead of grapheme cluster
///   boundaries.
///
/// The migration walks every grapheme of every entry and asserts
/// SelectWord yields the full word. Driving through
/// `EditorTestHarness` directly because per-position cursor
/// placement + selection-text readback doesn't fit the
/// single-shot BufferScenario shape (see "Direct-harness for
/// cross-state claims" in docs/internal/scenario-migration-status.md).
#[test]
fn migrated_select_word_at_every_grapheme_position_in_multi_script_words() {
    use crate::common::harness::EditorTestHarness;
    use fresh::test_api::EditorTestApi;
    use unicode_segmentation::UnicodeSegmentation;

    let words: &[&str] = &[
        // Original bug report (issue #1332).
        "hibajavítás",
        // German with umlaut.
        "Änderung",
        // French accented.
        "résumé",
        // Czech.
        "příliš",
        // Polish.
        "żółć",
        // Cyrillic (Russian).
        "Привет",
        // Greek.
        "Ελληνικά",
        // Korean Hangul.
        "안녕하세요",
        // Japanese Hiragana.
        "こんにちは",
        // CJK.
        "你好世界",
        // Combining diacritic: 'e' + U+0301 (the cluster has two
        // codepoints; word selection must include both).
        "caf\u{0065}\u{0301}",
        // Emoji (single grapheme word; classifier treats as
        // punctuation, so SelectWord from inside the cluster
        // selects the cluster itself).
        "🇫🇷",
        "👨\u{200D}👩\u{200D}👧",
    ];

    for word in words {
        let grapheme_count = word.graphemes(true).count();
        for grapheme_idx in 0..grapheme_count {
            let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
            let _f = harness.load_buffer_from_text(word).unwrap();
            let api = harness.api_mut();

            api.dispatch(Action::MoveLineStart);
            for _ in 0..grapheme_idx {
                api.dispatch(Action::MoveRight);
            }
            api.dispatch(Action::SelectWord);

            let selected = api.selection_text();
            assert_eq!(
                selected, *word,
                "SelectWord on {word:?} from grapheme index {grapheme_idx} \
                 must select the whole word; got {selected:?}",
            );
        }
    }
}

/// Anti-test: with SelectWord dropped, no selection exists at any
/// grapheme position. Proves the loop's assertion is load-bearing.
#[test]
fn anti_select_word_at_every_grapheme_yields_no_selection() {
    use crate::common::harness::EditorTestHarness;
    use fresh::test_api::EditorTestApi;
    use unicode_segmentation::UnicodeSegmentation;

    let word = "hibajavítás";
    let grapheme_count = word.graphemes(true).count();
    for grapheme_idx in 0..grapheme_count {
        let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
        let _f = harness.load_buffer_from_text(word).unwrap();
        let api = harness.api_mut();
        api.dispatch(Action::MoveLineStart);
        for _ in 0..grapheme_idx {
            api.dispatch(Action::MoveRight);
        }
        // No SelectWord dispatch.
        let selected = api.selection_text();
        assert_eq!(
            selected, "",
            "anti: without SelectWord, no selection should exist at grapheme {grapheme_idx}",
        );
    }
}
