//! DECLARATIVE migration of `tests/e2e/selection.rs` claims not
//! covered by `migrated_selection_full.rs` or `semantic/selection.rs`.
//!
//! Focus:
//!   - Shift+Up / Shift+Down line-extending selection
//!   - Selection reversal across the anchor
//!   - Word-selection through multi-script accented graphemes
//!     (issue #1332)
//!
//! Every test in this file is a `BufferScenario` literal — or, in
//! the matrix case (`SelectWord at every grapheme position`), an
//! iteration over a const data table where each row constructs
//! exactly one `BufferScenario`. No raw `EditorTestHarness` usage.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;
use unicode_segmentation::UnicodeSegmentation;

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

// ─────────────────────────────────────────────────────────────────────
// Matrix: SelectWord from every grapheme position of every word
// ─────────────────────────────────────────────────────────────────────
//
// Issue #1332: SelectWord must select the whole word regardless of
// which grapheme inside it the cursor sits on. Bug pre-fix: on
// "hibajavítás" with cursor on 'í', Ctrl+W returned "hibajav" —
// the word-end scan used codepoint indices instead of grapheme
// cluster boundaries.
//
// The migration walks every grapheme of every word below and
// asserts SelectWord yields the full word. Each `(word,
// grapheme_idx)` pair builds exactly one `BufferScenario` literal
// via `select_word_at_grapheme_scenario` — the iteration is over
// a const data table, the per-case test is still pure data.

/// Multi-script words to exercise. Each contains at least one
/// non-ASCII grapheme; some contain multi-codepoint clusters
/// (combining diacritics, ZWJ emoji sequences) where the byte
/// length per grapheme is ≥ 2.
const SELECT_WORD_GRAPHEME_WORDS: &[&str] = &[
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
    // Combining diacritic: 'e' + U+0301 (two codepoints, one cluster).
    "caf\u{0065}\u{0301}",
    // Emoji (single grapheme word; the classifier treats it as
    // punctuation, so SelectWord from inside the cluster selects
    // the cluster itself).
    "🇫🇷",
    "👨\u{200D}👩\u{200D}👧",
];

/// Byte offset of the start of the `grapheme_idx`-th grapheme of
/// `word`. Used to spell out the expected cursor position before
/// SelectWord runs.
fn byte_offset_of_grapheme(word: &str, grapheme_idx: usize) -> usize {
    word.graphemes(true).take(grapheme_idx).map(str::len).sum()
}

/// Build the positive scenario for `(word, grapheme_idx)`. The
/// cursor walks to the target grapheme via MoveLineStart +
/// `grapheme_idx` MoveRights (one per grapheme), then SelectWord
/// fires. The expected end-state pins both the selection text
/// (the whole word) and the caret range (anchor at word start,
/// position at word end — the SelectWord implementation in
/// `src/input/actions.rs::Action::SelectWord` parks the caret at
/// `word_end` with `anchor = word_start`).
fn select_word_at_grapheme_scenario(word: &str, grapheme_idx: usize) -> BufferScenario {
    let mut actions = vec![Action::MoveLineStart];
    for _ in 0..grapheme_idx {
        actions.push(Action::MoveRight);
    }
    actions.push(Action::SelectWord);

    BufferScenario {
        description: format!(
            "SelectWord on {word:?} from grapheme index {grapheme_idx} selects the whole word"
        ),
        initial_text: word.to_string(),
        actions,
        expected_text: word.to_string(),
        // Word spans the entire buffer ⇒ word_start = 0, word_end = word.len().
        expected_primary: CursorExpect::range(0, word.len()),
        expected_selection_text: Some(word.to_string()),
        ..Default::default()
    }
}

#[test]
fn migrated_select_word_at_every_grapheme_position_in_multi_script_words() {
    // Issue #1332 regression coverage. The iteration here is
    // over the const `SELECT_WORD_GRAPHEME_WORDS` table; each
    // iteration builds exactly one `BufferScenario` literal and
    // runs it through the standard declarative runner. No
    // EditorTestHarness usage in this file.
    for word in SELECT_WORD_GRAPHEME_WORDS {
        let grapheme_count = word.graphemes(true).count();
        for grapheme_idx in 0..grapheme_count {
            assert_buffer_scenario(select_word_at_grapheme_scenario(word, grapheme_idx));
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Anti-test
// ─────────────────────────────────────────────────────────────────────

/// Anti-test (declarative): without `SelectWord`, no selection
/// exists at any grapheme position. Proves the matrix assertion
/// above is load-bearing rather than a tautology over some
/// accidental pre-existing selection state. Each grapheme
/// position yields its own `BufferScenario` literal where the
/// cursor sits at the start of that grapheme with no selection.
#[test]
fn anti_select_word_at_every_grapheme_yields_no_selection() {
    let word = "hibajavítás";
    let grapheme_count = word.graphemes(true).count();
    for grapheme_idx in 0..grapheme_count {
        let mut actions = vec![Action::MoveLineStart];
        for _ in 0..grapheme_idx {
            actions.push(Action::MoveRight);
        }
        // No SelectWord dispatch — cursor sits at the start of
        // the target grapheme with no anchor.
        let cursor_byte = byte_offset_of_grapheme(word, grapheme_idx);
        assert_buffer_scenario(BufferScenario {
            description: format!(
                "anti: no SelectWord at grapheme {grapheme_idx} of {word:?} ⇒ empty selection"
            ),
            initial_text: word.to_string(),
            actions,
            expected_text: word.to_string(),
            expected_primary: CursorExpect::at(cursor_byte),
            expected_selection_text: Some(String::new()),
            ..Default::default()
        });
    }
}
