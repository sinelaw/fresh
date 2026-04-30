//! Track B migration: rewrites of `tests/e2e/sort_lines.rs` as
//! declarative theorems.
//!
//! Notable: the original tests invoke "sort lines" through the
//! command palette (Ctrl+P → type → Enter). The semantic action
//! `Action::SortLines` exists and bypasses the palette entirely, so
//! the theorem version is dramatically shorter — it tests the
//! transformation, not the palette UX.

use crate::common::theorem::buffer_theorem::{assert_buffer_theorem, BufferTheorem, CursorExpect};
use crate::common::theorem::trace_theorem::{assert_trace_theorem, TraceTheorem};
use fresh::test_api::Action;

#[test]
fn theorem_sort_lines_basic_alphabetical() {
    // Replaces tests/e2e/sort_lines.rs::test_sort_lines_basic.
    //
    // FINDING: when SortLines actually mutates text, the selection
    // is cleared (position 19, anchor None). The original imperative
    // test was silent about this. The companion theorem
    // `theorem_sort_lines_already_sorted_is_idempotent` shows that
    // when SortLines is a no-op, the selection is preserved — an
    // asymmetry pinned down by the declarative form.
    assert_buffer_theorem(BufferTheorem {
        description: "SelectAll + SortLines orders three lines alphabetically",
        initial_text: "cherry\napple\nbanana",
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "apple\nbanana\ncherry",
        expected_primary: CursorExpect::at(19),
        expected_extra_cursors: vec![],
        expected_selection_text: Some(""),
    });
}

#[test]
fn theorem_sort_lines_already_sorted_is_idempotent() {
    // Replaces tests/e2e/sort_lines.rs::test_sort_lines_already_sorted.
    // See finding in `theorem_sort_lines_basic_alphabetical`: the
    // selection is preserved here because SortLines is a no-op.
    assert_buffer_theorem(BufferTheorem {
        description: "SortLines on sorted input is idempotent and preserves the selection",
        initial_text: "apple\nbanana\ncherry",
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "apple\nbanana\ncherry",
        expected_primary: CursorExpect::range(0, 19),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("apple\nbanana\ncherry"),
    });
}

#[test]
fn theorem_sort_lines_undo_restores_original_order() {
    // Replaces tests/e2e/sort_lines.rs::test_sort_lines_undo.
    // Forward: select all, sort. Reverse: one undo restores order.
    // SortLines is one transactional unit, so undo_count = 1
    // (not "one undo per line").
    assert_trace_theorem(TraceTheorem {
        description: "SortLines is a single undo unit — one Undo restores the input",
        initial_text: "cherry\napple\nbanana",
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "apple\nbanana\ncherry",
        undo_count: 1,
    });
}
