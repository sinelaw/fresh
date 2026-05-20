//! Migrated from `tests/e2e/movement.rs`.
//!
//! The originals drive `KeyCode::Char/Left/Right/Up/Down/Home/End`
//! through the harness and assert with `harness.cursor_position()`.
//! The scenarios below state the same claims as data: action
//! sequence in, expected text + cursor out.
//!
//! What's gained:
//! - keymap-independent (Alt+U vs Cmd+U vs Vi binding doesn't change
//!   any of these),
//! - render-independent (no `harness.render()` calls),
//! - faster (single-digit ms per scenario),
//! - shrinkable as proptest seeds (the corpus dump emits these).

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_typing_and_cursor_movement_basic() {
    // Cleaned-up version of the first half of
    // `test_typing_and_cursor_movement` — type "Hello", end at
    // cursor 5.
    assert_buffer_scenario(BufferScenario {
        description: "type 'Hello' from empty buffer leaves cursor at 5".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('H'),
            Action::InsertChar('e'),
            Action::InsertChar('l'),
            Action::InsertChar('l'),
            Action::InsertChar('o'),
        ],
        expected_text: "Hello".into(),
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    });
}

#[test]
fn migrated_type_then_arrow_left_then_insert_in_middle() {
    assert_buffer_scenario(BufferScenario {
        description: "type 'abcd', MoveLeft 2, insert 'X' produces 'abXcd' with cursor at 3".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::InsertChar('c'),
            Action::InsertChar('d'),
            Action::MoveLeft,
            Action::MoveLeft,
            Action::InsertChar('X'),
        ],
        expected_text: "abXcd".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_home_end_navigation() {
    // Home jumps to line start; End jumps to line end. On a single
    // line, Home → 0, End → length.
    assert_buffer_scenario(BufferScenario {
        description: "MoveLineStart on 'hello world' parks cursor at 0".into(),
        initial_text: "hello world".into(),
        actions: vec![Action::MoveDocumentEnd, Action::MoveLineStart],
        expected_text: "hello world".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_multiline_navigation_up_down() {
    // 3-line buffer; from end, MoveUp lands on line 2 end.
    assert_buffer_scenario(BufferScenario {
        description: "MoveUp from end of line 3 jumps to end of line 2".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        actions: vec![Action::MoveDocumentEnd, Action::MoveUp],
        expected_text: "Line 1\nLine 2\nLine 3".into(),
        // "Line 1\n" (7) + "Line 2" (6) = 13.
        expected_primary: CursorExpect::at(13),
        ..Default::default()
    });
}

#[test]
fn migrated_backspace_deletes_previous_char() {
    assert_buffer_scenario(BufferScenario {
        description: "DeleteBackward at position 5 removes the previous char".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::InsertChar('c'),
            Action::DeleteBackward,
        ],
        expected_text: "ab".into(),
        expected_primary: CursorExpect::at(2),
        ..Default::default()
    });
}

#[test]
fn migrated_up_from_line_below_empty_lands_on_empty_line() {
    // Original: tests/e2e/movement.rs:480 test_movement_across_empty_lines.
    // The regression direction was Up, NOT Down. The bug:
    // pressing Up from "Line 3" (cursor at byte 8) used to skip
    // the empty line at byte 7 and land at byte 0 ("Line 1");
    // the fix ensures Up lands at byte 7 (the empty line).
    assert_buffer_scenario(BufferScenario {
        description:
            "Down/Down/Up: cursor at byte 8 then Up must land at byte 7 (empty line), not byte 0"
                .into(),
        initial_text: "Line 1\n\nLine 3\n".into(),
        actions: vec![Action::MoveDown, Action::MoveDown, Action::MoveUp],
        expected_text: "Line 1\n\nLine 3\n".into(),
        expected_primary: CursorExpect::at(7),
        ..Default::default()
    });
}

/// Anti-test: drops the final `MoveUp` from
/// `migrated_up_from_line_below_empty_lands_on_empty_line`.
/// Without it, after Down/Down the cursor sits at byte 8 (start
/// of "Line 3"); the expectation of byte 7 (the empty line)
/// cannot match. Proves MoveUp's stop-at-empty-line behavior is
/// what the test pins.
#[test]
fn anti_movement_dropping_move_up_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: final MoveUp dropped — cursor ends at byte 8 not 7".into(),
        initial_text: "Line 1\n\nLine 3\n".into(),
        actions: vec![Action::MoveDown, Action::MoveDown],
        expected_text: "Line 1\n\nLine 3\n".into(),
        expected_primary: CursorExpect::at(7),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without the final MoveUp the cursor stays at byte 8 \
         (start of Line 3); the empty-line landing at byte 7 cannot appear"
    );
}

#[test]
fn migrated_up_up_from_line_3_reaches_line_1() {
    // Companion to the regression: a SECOND Up from the empty
    // line at byte 7 must land at byte 0 (Line 1 start), proving
    // the post-fix traversal reaches Line 1 in two Ups not one.
    assert_buffer_scenario(BufferScenario {
        description: "Down/Down/Up/Up walks back through the empty line to Line 1".into(),
        initial_text: "Line 1\n\nLine 3\n".into(),
        actions: vec![
            Action::MoveDown,
            Action::MoveDown,
            Action::MoveUp,
            Action::MoveUp,
        ],
        expected_text: "Line 1\n\nLine 3\n".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}
