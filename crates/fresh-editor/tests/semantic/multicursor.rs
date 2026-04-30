//! Phase 3 — multi-cursor coverage using `BufferTheorem`.
//!
//! `BufferTheorem` already supports asserting on N cursors via
//! `expected_extra_cursors`, so multi-cursor tests do not need a new
//! theorem type. We just declare what each cursor should look like at
//! t=∞.
//!
//! This file rewrites
//! `tests/e2e/multicursor.rs::test_multi_cursor_typing` as a theorem.
//! That E2E test calls `editor.add_cursor_below()` (an internal
//! method); the theorem replaces it with the semantic equivalent
//! `Action::AddCursorBelow`, which is the keybinding-independent name
//! for the same effect.

use crate::common::theorem::buffer_theorem::{assert_buffer_theorem, BufferTheorem, CursorExpect};
use crate::common::theorem::trace_theorem::{assert_trace_theorem, TraceTheorem};
use fresh::test_api::Action;

#[test]
fn theorem_multi_cursor_insertion_is_vectorized() {
    // Initial:  "aaa\nbbb\nccc\nddd"  (cursor at byte 0 after load)
    //
    // After AddCursorBelow ×2, three cursors live at the start of
    // lines 0/1/2; the most-recently-added one (line 2) is primary.
    //
    // Typing "xyz" inserts at each cursor in lock-step:
    //   "xyzaaa\nxyzbbb\nxyzccc\nddd"
    //    0  3    7  10   14 17    21
    //
    // Each cursor sits at the end of its inserted "xyz".
    assert_buffer_theorem(BufferTheorem {
        description: "InsertChar applied across 3 cursors mutates each in lock-step",
        initial_text: "aaa\nbbb\nccc\nddd",
        actions: vec![
            Action::AddCursorBelow,
            Action::AddCursorBelow,
            Action::InsertChar('x'),
            Action::InsertChar('y'),
            Action::InsertChar('z'),
        ],
        expected_text: "xyzaaa\nxyzbbb\nxyzccc\nddd",
        // Primary is the most-recently-added cursor — the one on line 2.
        expected_primary: CursorExpect::at(17),
        // Other two cursors live on lines 0 and 1.
        expected_extra_cursors: vec![CursorExpect::at(3), CursorExpect::at(10)],
        expected_selection_text: Some(""),
    });
}

#[test]
fn theorem_multi_cursor_undo_is_atomic() {
    // Replaces `tests/e2e/multicursor.rs::test_multi_cursor_undo_atomic`.
    //
    // Property: typing N characters across K cursors records N
    // (vectorized) edits, and undoing those N edits restores the
    // initial buffer exactly. The K-fold parallelism does not change
    // the number of undo steps — each character typed is one undo
    // unit. This is the algebraic statement of "multi-cursor edits are
    // atomic per keystroke".
    assert_trace_theorem(TraceTheorem {
        description: "3 cursors × InsertChar(x), (y), (z) = 3 undo units, \
             not 9 — the vectorization is transparent to history",
        initial_text: "aaa\nbbb\nccc\nddd",
        actions: vec![
            Action::AddCursorBelow,
            Action::AddCursorBelow,
            Action::InsertChar('x'),
            Action::InsertChar('y'),
            Action::InsertChar('z'),
        ],
        expected_text: "xyzaaa\nxyzbbb\nxyzccc\nddd",
        // Three character insertions ⇒ three undo steps to fully
        // restore. If multi-cursor batching weren't atomic, this would
        // need nine.
        undo_count: 3,
    });
}
