//! Miscellaneous wave-3 migrations — short claims that don't
//! fit a domain file but capture useful intents.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_empty_buffer_no_actions_no_change() {
    assert_buffer_scenario(BufferScenario {
        description: "empty buffer + no actions = empty buffer + cursor at 0".into(),
        initial_text: String::new(),
        actions: vec![],
        expected_text: String::new(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_single_char_left_right_round_trip() {
    assert_buffer_scenario(BufferScenario {
        description: "MoveRight then MoveLeft on single char returns to origin".into(),
        initial_text: "a".into(),
        actions: vec![Action::MoveRight, Action::MoveLeft],
        expected_text: "a".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_select_left_at_byte_zero_creates_empty_selection() {
    // SelectLeft at byte 0 doesn't move the cursor, but it
    // *does* set an anchor at the same position — the selection
    // shape becomes "empty range at 0" rather than "no selection".
    // Pinning that asymmetry as a regression test.
    assert_buffer_scenario(BufferScenario {
        description: "SelectLeft at byte 0 sets anchor at 0 with cursor at 0".into(),
        initial_text: "abc".into(),
        actions: vec![Action::SelectLeft],
        expected_text: "abc".into(),
        expected_primary: CursorExpect::range(0, 0),
        ..Default::default()
    });
}

#[test]
fn migrated_repeated_undo_at_initial_state_is_no_op() {
    // Five Undos on a buffer with no edit history should leave
    // text unchanged.
    assert_buffer_scenario(BufferScenario {
        description: "Undo on unedited buffer is a no-op".into(),
        initial_text: "stable".into(),
        actions: vec![Action::Undo, Action::Undo, Action::Undo],
        expected_text: "stable".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_redo_with_no_redo_stack_is_no_op() {
    assert_buffer_scenario(BufferScenario {
        description: "Redo with no redo history leaves buffer intact".into(),
        initial_text: "stable".into(),
        actions: vec![Action::Redo],
        expected_text: "stable".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_insert_then_delete_forward_returns_to_initial() {
    assert_buffer_scenario(BufferScenario {
        description: "InsertChar then DeleteForward + Backward roundtrip".into(),
        initial_text: "ab".into(),
        actions: vec![Action::InsertChar('X'), Action::DeleteBackward],
        expected_text: "ab".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}
