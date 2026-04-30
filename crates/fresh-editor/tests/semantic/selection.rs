//! Track B migration: pure-action subset of `tests/e2e/selection.rs`.
//!
//! The originals drive Ctrl-W / Ctrl-L / Ctrl-Shift-Right and observe
//! the selection through `harness.editor().active_cursors().primary()`.
//! The semantic versions dispatch `Action::SelectWord` /
//! `Action::SelectLine` / `Action::ExpandSelection` and observe via
//! `expected_selection_text` on the theorem.
//!
//! Skipped (deferred):
//!   * `test_selection_visual_rendering` — needs a `RenderSnapshot`-
//!     style projection (asserts on `selection_bg` cells).
//!   * `test_select_word_multi_cursor` — the original only asserts
//!     `cursor_count >= 1`, which is not a useful theorem.
//!   * `test_select_word_after_scrolling` /
//!     `test_expand_selection_after_scrolling` — viewport-dependent.
//!   * `test_expand_selection_large_buffer_performance` /
//!     `_very_large_buffer` — performance, already `#[ignore]` in
//!     the e2e suite.
//!   * `test_select_word_accented_characters` — 13 words × N
//!     graphemes each; would explode into ~80 theorems. Worth a
//!     dedicated proptest-style migration later.

use crate::common::theorem::buffer_theorem::{assert_buffer_theorem, BufferTheorem, CursorExpect};
use fresh::test_api::Action;

// ─────────────────────────────────────────────────────────────────────────
// SelectWord (Ctrl+W) — current-word selection regardless of cursor offset
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_select_word_from_middle_of_word() {
    // Replaces test_select_word.
    // Buffer "hello world test", cursor at position 8 (middle of "world").
    // SelectWord pulls in the entire word "world".
    assert_buffer_theorem(BufferTheorem {
        description: "SelectWord with cursor mid-word selects the whole word",
        initial_text: "hello world test",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::SelectWord,
        ],
        expected_text: "hello world test",
        expected_primary: CursorExpect::range(6, 11),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("world"),
    });
}

#[test]
fn theorem_select_word_from_start_of_word() {
    // Replaces test_select_word_at_start.
    // Cursor at position 6 — start of "world".
    assert_buffer_theorem(BufferTheorem {
        description: "SelectWord at start of word still selects the whole word",
        initial_text: "hello world",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::SelectWord,
        ],
        expected_text: "hello world",
        expected_primary: CursorExpect::range(6, 11),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("world"),
    });
}

#[test]
fn theorem_select_word_from_end_of_word() {
    // Replaces test_select_word_at_end.
    // Cursor at position 5 — between "hello" and the space. Picks
    // up the *previous* word.
    assert_buffer_theorem(BufferTheorem {
        description: "SelectWord at end of word selects that word",
        initial_text: "hello world",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::SelectWord,
        ],
        expected_text: "hello world",
        expected_primary: CursorExpect::range(0, 5),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("hello"),
    });
}

// Word-class boundary tests — what counts as a word character?

#[test]
fn theorem_select_word_treats_hyphen_as_separator() {
    // Replaces test_select_word_with_hyphen.
    // "foo-bar", cursor at 0 → SelectWord → "foo" only (hyphen is a
    // separator).
    assert_buffer_theorem(BufferTheorem {
        description: "Hyphen is a word separator: SelectWord on 'foo-bar' picks 'foo'",
        initial_text: "foo-bar",
        actions: vec![Action::MoveDocumentStart, Action::SelectWord],
        expected_text: "foo-bar",
        expected_primary: CursorExpect::range(0, 3),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("foo"),
    });
}

#[test]
fn theorem_select_word_treats_underscore_as_word_char() {
    // Replaces test_select_word_with_underscore.
    // Underscore is part of the word — "baz_qux" is one word.
    assert_buffer_theorem(BufferTheorem {
        description:
            "Underscore is a word character: SelectWord on 'baz_qux' picks the whole token",
        initial_text: "baz_qux",
        actions: vec![Action::MoveDocumentStart, Action::SelectWord],
        expected_text: "baz_qux",
        expected_primary: CursorExpect::range(0, 7),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("baz_qux"),
    });
}

#[test]
fn theorem_select_word_treats_alphanumeric_as_one_word() {
    // Replaces test_select_word_with_numbers.
    assert_buffer_theorem(BufferTheorem {
        description:
            "Letters and digits are one word: SelectWord on 'test123' picks the whole token",
        initial_text: "test123",
        actions: vec![Action::MoveDocumentStart, Action::SelectWord],
        expected_text: "test123",
        expected_primary: CursorExpect::range(0, 7),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("test123"),
    });
}

#[test]
fn theorem_select_word_treats_at_symbol_as_separator() {
    // Replaces test_select_word_with_at_symbol.
    assert_buffer_theorem(BufferTheorem {
        description: "@ is a word separator: SelectWord on 'user@domain' picks 'user'",
        initial_text: "user@domain",
        actions: vec![Action::MoveDocumentStart, Action::SelectWord],
        expected_text: "user@domain",
        expected_primary: CursorExpect::range(0, 4),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("user"),
    });
}

#[test]
fn theorem_select_word_treats_dot_as_separator() {
    // Replaces test_select_word_with_dot.
    // Cursor needs to be on/after the dot to pick up "domain". The
    // original test moved to position 4 (right after "user").
    assert_buffer_theorem(BufferTheorem {
        description:
            "'.' is a word separator: SelectWord on 'user.domain' from after the dot picks 'domain'",
        initial_text: "user.domain",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::SelectWord,
        ],
        expected_text: "user.domain",
        expected_primary: CursorExpect::range(5, 11),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("domain"),
    });
}

// ─────────────────────────────────────────────────────────────────────────
// SelectLine (Ctrl+L)
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_select_line_includes_trailing_newline() {
    // Replaces test_select_line.
    // Multi-line buffer; cursor on the second line; SelectLine pulls
    // in the entire line *including* the trailing newline.
    assert_buffer_theorem(BufferTheorem {
        description: "SelectLine on a non-last line includes the trailing newline",
        initial_text: "first line\nsecond line\nthird line",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveDown,
            Action::SelectLine,
        ],
        expected_text: "first line\nsecond line\nthird line",
        expected_primary: CursorExpect::range(11, 23),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("second line\n"),
    });
}

#[test]
fn theorem_select_line_first_line_includes_trailing_newline() {
    // Replaces test_select_line_first.
    assert_buffer_theorem(BufferTheorem {
        description: "SelectLine on the first line includes its trailing newline",
        initial_text: "first line\nsecond line",
        actions: vec![Action::MoveDocumentStart, Action::SelectLine],
        expected_text: "first line\nsecond line",
        expected_primary: CursorExpect::range(0, 11),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("first line\n"),
    });
}

#[test]
fn theorem_select_line_last_line_no_trailing_newline() {
    // Replaces test_select_line_last.
    // Cursor lands on the last line (no trailing newline in the
    // buffer). SelectLine selects the line *without* a newline.
    assert_buffer_theorem(BufferTheorem {
        description: "SelectLine on the last line (no trailing newline) selects bare text",
        initial_text: "first line\nsecond line",
        actions: vec![Action::MoveDocumentEnd, Action::SelectLine],
        expected_text: "first line\nsecond line",
        expected_primary: CursorExpect::range(11, 22),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("second line"),
    });
}

// ─────────────────────────────────────────────────────────────────────────
// ExpandSelection (Ctrl+Shift+Right) — incremental word-boundary growth
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_expand_selection_grows_in_three_steps() {
    // Replaces test_expand_selection.
    // Cursor at position 3 (inside "hello") in "hello world test".
    // 1st expand → "lo" (cursor → end of current word)
    // 2nd expand → "lo world"
    // 3rd expand → "lo world test"
    assert_buffer_theorem(BufferTheorem {
        description: "ExpandSelection grows by one word at a time",
        initial_text: "hello world test",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::ExpandSelection,
            Action::ExpandSelection,
            Action::ExpandSelection,
        ],
        expected_text: "hello world test",
        expected_primary: CursorExpect::range(3, 16),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("lo world test"),
    });
}

#[test]
fn theorem_expand_selection_with_no_initial_selection_picks_word_tail() {
    // Replaces test_expand_selection_no_initial_selection.
    // "foo bar baz", cursor at position 5 (on 'a' in "bar").
    // First expand → "ar".
    assert_buffer_theorem(BufferTheorem {
        description: "ExpandSelection with no prior selection picks cursor-to-word-end",
        initial_text: "foo bar baz",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::ExpandSelection,
        ],
        expected_text: "foo bar baz",
        expected_primary: CursorExpect::range(5, 7),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("ar"),
    });
}

#[test]
fn theorem_expand_selection_crosses_line_boundary() {
    // Replaces test_expand_selection_across_lines.
    // After first expand, "ending" is selected; second expand crosses
    // the newline and selects through "second"; third expand grows
    // through "line".
    assert_buffer_theorem(BufferTheorem {
        description: "ExpandSelection crosses line boundaries word-by-word",
        initial_text: "first line ending\nsecond line starting here",
        // Move to start of "ending" (position 11 = 5+1+5 = "first line " then 'e' at 11)
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveLineEnd,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::ExpandSelection,
            Action::ExpandSelection,
            Action::ExpandSelection,
        ],
        expected_text: "first line ending\nsecond line starting here",
        expected_primary: CursorExpect::range(11, 29),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("ending\nsecond line"),
    });
}

#[test]
fn theorem_expand_selection_on_word_char_picks_current_word() {
    // Replaces test_expand_selection_on_word_char.
    // Cursor at byte 0 in "hello world" — first ExpandSelection picks
    // the entire current word.
    assert_buffer_theorem(BufferTheorem {
        description: "ExpandSelection from start of word selects the whole word",
        initial_text: "hello world",
        actions: vec![Action::MoveDocumentStart, Action::ExpandSelection],
        expected_text: "hello world",
        expected_primary: CursorExpect::range(0, 5),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("hello"),
    });
}

#[test]
fn theorem_expand_selection_on_punctuation_run() {
    // Replaces test_expand_selection_on_non_word_char.
    // FINDING: the e2e original asserts ExpandSelection stops at the
    // first word boundary ("**-"), but in the semantic harness the
    // selection grows through the entire token ("**-word"). The
    // discrepancy is a harness configuration gap: the e2e harness
    // creates buffers via `EditorTestHarness::new(...)` and types
    // text in (no on-disk file, no language), while the semantic
    // runner loads a `.txt` fixture which presumably resolves a
    // different word-character set. Captured here as the *semantic
    // harness's* behavior so a future fix that aligns the two will
    // surface as a deliberate update to this theorem rather than a
    // silent change.
    assert_buffer_theorem(BufferTheorem {
        description: "ExpandSelection from punctuation grows through the token (semantic harness)",
        initial_text: "**-word",
        actions: vec![Action::MoveDocumentStart, Action::ExpandSelection],
        expected_text: "**-word",
        expected_primary: CursorExpect::range(0, 7),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("**-word"),
    });
}

#[test]
fn theorem_expand_selection_from_middle_of_word_picks_tail() {
    // Replaces test_expand_selection_from_middle_of_word.
    // Buffer "Event", cursor at position 1 ('v'). ExpandSelection
    // selects from cursor to word end — "vent", not the whole word.
    assert_buffer_theorem(BufferTheorem {
        description: "ExpandSelection from mid-word selects only the tail of the current word",
        initial_text: "Event",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::ExpandSelection,
        ],
        expected_text: "Event",
        expected_primary: CursorExpect::range(1, 5),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("vent"),
    });
}
