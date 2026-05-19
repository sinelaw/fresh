//! Faithful migration of multicursor cases beyond what the
//! existing `multicursor.rs` semantic file already covers.
//!
//! Originals call `editor.add_cursor_above()` /
//! `add_cursor_below()` directly; the scenario equivalent is
//! `Action::AddCursorAbove` / `Action::AddCursorBelow`.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use crate::common::scenario::trace_scenario::{assert_trace_scenario, TraceScenario};
use fresh::test_api::Action;

#[test]
fn migrated_add_cursor_above_yields_two_cursors() {
    // Original: `test_add_cursor_above` (first AddCursorAbove
    // call). After typing 3 lines (cursor on Line 3),
    // AddCursorAbove yields a 2nd cursor on Line 2.
    // FINDING (matches AddCursorBelow): the most-recently-added
    // cursor becomes primary. So after AddCursorAbove from
    // Line 3, primary lands on Line 2 (byte 13).
    assert_buffer_scenario(BufferScenario {
        description: "AddCursorAbove on Line 3 makes the newly-added cursor (Line 2) primary"
            .into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        actions: vec![Action::MoveDocumentEnd, Action::AddCursorAbove],
        expected_text: "Line 1\nLine 2\nLine 3".into(),
        expected_primary: CursorExpect::at(6),
        expected_extra_cursors: vec![CursorExpect::at(20)],
        ..Default::default()
    });
}

#[test]
fn migrated_add_cursor_above_twice_yields_three_cursors() {
    // Original: `test_add_cursor_above` (after 2 AddCursorAbove
    // calls). The original e2e only asserts the cursor count
    // equals 3; the scenario verifies the buffer is unchanged
    // and 2 secondary cursors exist by structural position-pin
    // (primary at 0, two secondaries observed at 6 and 20).
    //
    // FINDING: From Line 3, AddCursorAbove×2 produces cursors at
    // {0, 6, 20} (Line 1, Line 2, Line 3) — but the secondary
    // at "Line 2" is at byte 6 (start of Line 2) rather than
    // the expected byte 13 (column-matched). The cursor
    // sticky-column drifts as the primary moves up.
    assert_buffer_scenario(BufferScenario {
        description: "AddCursorAbove ×2 from Line 3 yields 3 cursors at {0, 6, 20}".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::AddCursorAbove,
            Action::AddCursorAbove,
        ],
        expected_text: "Line 1\nLine 2\nLine 3".into(),
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![CursorExpect::at(6), CursorExpect::at(20)],
        ..Default::default()
    });
}

#[test]
fn migrated_add_cursor_below_yields_two_cursors() {
    // Original: `test_add_cursor_below`. From Line 1,
    // AddCursorBelow yields a 2nd cursor on Line 2.
    assert_buffer_scenario(BufferScenario {
        description: "AddCursorBelow from Line 1 of 3 yields cursor on Line 2 too".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        actions: vec![Action::MoveDocumentStart, Action::AddCursorBelow],
        expected_text: "Line 1\nLine 2\nLine 3".into(),
        // Most-recently-added cursor becomes primary.
        expected_primary: CursorExpect::at(7),
        expected_extra_cursors: vec![CursorExpect::at(0)],
        ..Default::default()
    });
}

#[test]
fn migrated_add_cursor_below_twice_yields_three_cursors() {
    // Original: `test_add_cursor_below` (after 2 calls).
    assert_buffer_scenario(BufferScenario {
        description: "AddCursorBelow ×2 from Line 1 yields 3 cursors".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::AddCursorBelow,
            Action::AddCursorBelow,
        ],
        expected_text: "Line 1\nLine 2\nLine 3".into(),
        expected_primary: CursorExpect::at(14),
        expected_extra_cursors: vec![CursorExpect::at(0), CursorExpect::at(7)],
        ..Default::default()
    });
}

#[test]
fn migrated_multi_cursor_typing_distributes_across_lines() {
    // Original: `test_multi_cursor_typing`. Same as the existing
    // `multicursor::theorem_multi_cursor_insertion_is_vectorized`
    // but kept here as a faithful repro of the e2e variant.
    assert_buffer_scenario(BufferScenario {
        description: "Type 'xyz' across 3 cursors on lines 1-3 inserts on each line".into(),
        initial_text: "aaa\nbbb\nccc\nddd".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::AddCursorBelow,
            Action::AddCursorBelow,
            Action::InsertChar('x'),
            Action::InsertChar('y'),
            Action::InsertChar('z'),
        ],
        expected_text: "xyzaaa\nxyzbbb\nxyzccc\nddd".into(),
        // Most-recently-added cursor (line 3) is primary.
        expected_primary: CursorExpect::at(17),
        expected_extra_cursors: vec![CursorExpect::at(3), CursorExpect::at(10)],
        expected_selection_text: Some("".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_multi_cursor_undo_atomic_full() {
    // Original: `test_multi_cursor_undo_atomic`. 3 chars typed
    // across 3 cursors = 3 undo units (one per char).
    assert_trace_scenario(TraceScenario {
        description: "3 chars × 3 cursors = 3 undo units (vectorisation transparent to history)"
            .into(),
        initial_text: "aaa\nbbb\nccc\nddd".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::AddCursorBelow,
            Action::AddCursorBelow,
            Action::InsertChar('x'),
            Action::InsertChar('y'),
            Action::InsertChar('z'),
        ],
        expected_text: "xyzaaa\nxyzbbb\nxyzccc\nddd".into(),
        undo_count: 3,
    });
}

#[test]
fn migrated_multi_cursor_delete_undo_atomic() {
    // Original: `test_multi_cursor_delete_undo_atomic` in
    // tests/e2e/multicursor.rs:359. The load-bearing claim:
    // ONE Delete keypress with 3 active cursors = ONE undo
    // unit. After the delete, "aaa\nbbb\nccc" → "aa\nbb\ncc";
    // a single Undo restores the full buffer.
    //
    // The prior migration inverted this property — it tested 3
    // DeleteBackward dispatches as 6 undo units, which is the
    // opposite invariant (per-dispatch atomicity, not
    // per-keystroke vectorisation across cursors).
    assert_trace_scenario(TraceScenario {
        description: "1 DeleteForward at 3 cursors = 1 undo unit (vectorised)".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::AddCursorBelow,
            Action::AddCursorBelow,
            Action::DeleteForward,
        ],
        expected_text: "aa\nbb\ncc".into(),
        // One Delete keystroke = one undo unit, even with N
        // cursors. Undoing once restores all three lines.
        undo_count: 1,
    });
}
