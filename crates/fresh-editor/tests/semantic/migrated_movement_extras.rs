//! Migrations of `tests/e2e/movement.rs` claims not yet covered by
//! `migrated_movement.rs`. Focus: line-shuffling (Alt+Up/Down)
//! semantics, asserting on buffer text and cursor.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_move_line_up_swaps_with_previous_line() {
    // Original: `test_move_line_up_down_shortcuts` (first half).
    // Cursor on line 2 of "A\nB\nC"; Alt+Up swaps lines 1+2.
    assert_buffer_scenario(BufferScenario {
        description: "Action::MoveLineUp swaps line 2 with line 1 → 'B\\nA\\nC'".into(),
        initial_text: "A\nB\nC".into(),
        // Place cursor on line 2 (byte 2 = start of "B"), then move
        // line up.
        actions: vec![Action::MoveDown, Action::MoveLineUp],
        expected_text: "B\nA\nC".into(),
        // After the swap the cursor follows the moved line up to
        // line 1, byte 0.
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_move_line_up_then_down_round_trips() {
    // Original: `test_move_line_up_down_shortcuts` (second half).
    // After MoveLineUp, MoveLineDown should restore the original
    // ordering.
    assert_buffer_scenario(BufferScenario {
        description: "MoveLineUp then MoveLineDown restores original 'A\\nB\\nC'".into(),
        initial_text: "A\nB\nC".into(),
        actions: vec![Action::MoveDown, Action::MoveLineUp, Action::MoveLineDown],
        expected_text: "A\nB\nC".into(),
        // The cursor follows the line back down to line 2 (byte 2).
        expected_primary: CursorExpect::at(2),
        ..Default::default()
    });
}

#[test]
fn migrated_move_line_down_on_middle_line_swaps_with_next() {
    // Same Action::MoveLineDown semantics, but exercised
    // independently: cursor on the middle of three lines, Alt+Down
    // should swap lines 2 + 3.
    assert_buffer_scenario(BufferScenario {
        description: "MoveLineDown on the middle of 'A\\nB\\nC' yields 'A\\nC\\nB'".into(),
        initial_text: "A\nB\nC".into(),
        actions: vec![Action::MoveDown, Action::MoveLineDown],
        expected_text: "A\nC\nB".into(),
        // Cursor follows the moved line down to line 3 (byte 4).
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    });
}

/// Anti-test: drops `MoveLineUp` from the swap scenario — without
/// the swap action the buffer cannot end up as `"B\nA\nC"`, so
/// `check_buffer_scenario` must return `Err`.
#[test]
fn anti_move_line_up_without_action_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: MoveLineUp dropped — buffer must not swap lines".into(),
        initial_text: "A\nB\nC".into(),
        actions: vec![Action::MoveDown],
        expected_text: "B\nA\nC".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: MoveDown alone cannot rearrange the buffer text — \
         the swap-text expectation must NOT match"
    );
}
