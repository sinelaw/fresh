//! Track B migration: rewrites of the action-level subset of
//! `tests/e2e/undo_redo.rs` as declarative theorems.
//!
//! The originals drive Ctrl-Z / Ctrl-Y through the keymap and assert
//! on buffer content. The semantic versions dispatch
//! `Action::Undo` / `Action::Redo` directly, plus the cursor moves
//! and inserts that produce the undo history. The invariant under
//! test — *Undo and Redo skip over readonly cursor-movement actions
//! and step through write-actions only* — is the same in both forms.
//!
//! Migrated to `semantic/migrated_undo_save_point.rs` (the
//! `is_modified` save-point invariants now have a dedicated home
//! that reaches for `EditorTestHarness` + `EditorTestApi`):
//!   * `test_undo_to_save_point_marks_buffer_unmodified`
//!   * `test_undo_after_save_as_marks_buffer_unmodified`
//!   * `test_undo_past_save_point`
//!   * `test_undo_to_empty_after_save_as`

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn theorem_undo_skips_readonly_movement_actions() {
    // Replaces test_undo_skips_readonly_movement_actions.
    // Type "hello", move cursor left twice (readonly), then Undo.
    // The Undo restores the cursor *before* the movements (position 4,
    // end of "hell") and removes the last typed 'o'.
    assert_buffer_scenario(BufferScenario {
        description:
            "Undo restores cursor to before the readonly movements and pops the last write".into(),
        initial_text: "".into(),
        actions: vec![
            Action::InsertChar('h'),
            Action::InsertChar('e'),
            Action::InsertChar('l'),
            Action::InsertChar('l'),
            Action::InsertChar('o'),
            Action::MoveLeft,
            Action::MoveLeft,
            Action::Undo,
        ],
        expected_text: "hell".into(),
        expected_primary: CursorExpect::at(4),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_multiple_undo_skips_all_readonly_actions() {
    // Replaces test_multiple_undo_skips_all_readonly_actions.
    // Type "abc", scatter readonly motions, then Undo three times.
    // Each Undo should peel off one write-action ('c', 'b', 'a) and
    // skip every intervening movement.
    assert_buffer_scenario(BufferScenario {
        description: "Three Undos peel off three writes, skipping interleaved movements".into(),
        initial_text: "".into(),
        actions: vec![
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::InsertChar('c'),
            Action::MoveLeft,
            Action::MoveRight,
            Action::MoveLineStart,
            Action::MoveLineEnd,
            Action::Undo,
            Action::Undo,
            Action::Undo,
        ],
        expected_text: "".into(),
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_redo_skips_readonly_movement_actions() {
    // Replaces test_redo_skips_readonly_movement_actions.
    // Type "xyz" (cursor 3), MoveLeft (cursor 2), Undo (buffer "xy",
    // cursor 2 — pre-insert position). FINDING: Redo re-applies the
    // buffer edit but does *not* re-advance the cursor; the cursor
    // stays at 2 even though the re-inserted byte is at position 2.
    // The original e2e test never asserted the cursor here, so this
    // asymmetry between Undo (restores cursor) and Redo (doesn't
    // advance cursor) was invisible.
    assert_buffer_scenario(BufferScenario {
        description: "Redo re-applies the most-recent write and skips the readonly movement".into(),
        initial_text: "".into(),
        actions: vec![
            Action::InsertChar('x'),
            Action::InsertChar('y'),
            Action::InsertChar('z'),
            Action::MoveLeft,
            Action::Undo,
            Action::Redo,
        ],
        expected_text: "xyz".into(),
        expected_primary: CursorExpect::at(2),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_undo_redo_with_mixed_actions() {
    // Replaces test_undo_redo_with_mixed_actions.
    // Type "ab", go to start, type "x", scatter motions, then two
    // undos. First Undo skips motions and removes 'x' → "ab". Second
    // Undo skips the Home motion and removes 'b' → "a".
    assert_buffer_scenario(BufferScenario {
        description: "Undo correctly walks over interleaved motions and writes".into(),
        initial_text: "".into(),
        actions: vec![
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::MoveLineStart,
            Action::InsertChar('x'),
            Action::MoveLineEnd,
            Action::MoveLeft,
            Action::Undo,
            Action::Undo,
        ],
        expected_text: "a".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}
