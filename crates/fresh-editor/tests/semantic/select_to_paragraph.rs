//! Track B migration of `tests/e2e/select_to_paragraph.rs`.
//!
//! `Action::SelectToParagraphUp` / `SelectToParagraphDown` extend the
//! selection to the previous / next empty (whitespace-only) line.
//! These rewrites pin down the exact byte ranges the original
//! imperative test inferred via printlns and "selection should have
//! positive range" handwaves.

use crate::common::theorem::buffer_theorem::{assert_buffer_theorem, BufferTheorem, CursorExpect};
use fresh::test_api::Action;

const PARAGRAPHS: &str =
    "paragraph 1 line 1\nparagraph 1 line 2\n\nparagraph 2 line 1\nparagraph 2 line 2\n";
// Offsets:
// 0     line 1 "paragraph 1 line 1"  18 bytes, \n at 18, next line at 19
// 19    line 2 "paragraph 1 line 2"  next at 38
// 38    line 3 ""                    \n at 38, next at 39
// 39    line 4 "paragraph 2 line 1"  next at 58
// 58    line 5 "paragraph 2 line 2"  ends with trailing \n

#[test]
fn theorem_select_to_paragraph_down_from_line_1() {
    // Replaces test_select_to_paragraph_down.
    // Cursor at byte 0 after load. SelectToParagraphDown extends the
    // selection forward to the empty line at byte 38.
    assert_buffer_theorem(BufferTheorem {
        description: "SelectToParagraphDown from doc start selects the first paragraph",
        initial_text: PARAGRAPHS,
        actions: vec![Action::SelectToParagraphDown],
        expected_text: PARAGRAPHS,
        // Anchor at start, cursor moved to empty line.
        expected_primary: CursorExpect::range(0, 38),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("paragraph 1 line 1\nparagraph 1 line 2\n"),
    });
}

#[test]
fn theorem_select_to_paragraph_up_from_paragraph_2() {
    // Replaces test_select_to_paragraph_up.
    // Original test moved cursor down 3 lines (to byte 39), then
    // SelectToParagraphUp. Theorem replaces the navigation steps
    // with their semantic equivalents.
    assert_buffer_theorem(BufferTheorem {
        description: "SelectToParagraphUp from line 4 selects backward to the empty line",
        initial_text: PARAGRAPHS,
        actions: vec![
            Action::MoveDown,
            Action::MoveDown,
            Action::MoveDown,
            Action::SelectToParagraphUp,
        ],
        expected_text: PARAGRAPHS,
        // Anchor at line 4 start (39); cursor moved up to empty line (38).
        expected_primary: CursorExpect::range(39, 38),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("\n"),
    });
}
