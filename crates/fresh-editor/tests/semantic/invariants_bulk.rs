//! Bulk migration of small, focused buffer-state claims pulled
//! from across the e2e suite. Each is intentionally tiny so a
//! regression points at one specific behaviour.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BehaviorFlags, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn bulk_insert_newline_at_origin_pushes_text_down() {
    assert_buffer_scenario(BufferScenario {
        description: "InsertNewline at byte 0 prepends an empty line".into(),
        initial_text: "abc".into(),
        actions: vec![Action::InsertNewline],
        expected_text: "\nabc".into(),
        expected_primary: CursorExpect::at(1),
        ..Default::default()
    });
}

#[test]
fn bulk_three_newlines_yield_three_empty_lines() {
    assert_buffer_scenario(BufferScenario {
        description: "three InsertNewlines produce three blank lines".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertNewline,
            Action::InsertNewline,
            Action::InsertNewline,
        ],
        expected_text: "\n\n\n".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn bulk_select_all_then_select_all_is_idempotent() {
    assert_buffer_scenario(BufferScenario {
        description: "SelectAll twice has the same effect as SelectAll once".into(),
        initial_text: "alpha".into(),
        actions: vec![Action::SelectAll, Action::SelectAll],
        expected_text: "alpha".into(),
        expected_primary: CursorExpect::range(0, 5),
        expected_selection_text: Some("alpha".into()),
        ..Default::default()
    });
}

#[test]
fn bulk_move_right_past_eof_clamps() {
    assert_buffer_scenario(BufferScenario {
        description: "MoveRight 10 times on a 3-byte buffer clamps at byte 3".into(),
        initial_text: "abc".into(),
        actions: vec![
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
        ],
        expected_text: "abc".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn bulk_move_left_past_bof_clamps_at_zero() {
    assert_buffer_scenario(BufferScenario {
        description: "MoveLeft 5 times from byte 0 stays at byte 0".into(),
        initial_text: "abc".into(),
        actions: vec![
            Action::MoveLeft,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::MoveLeft,
        ],
        expected_text: "abc".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn bulk_select_down_then_left_collapses_selection_at_anchor() {
    // SelectDown then MoveLeft (deselect-on-move) collapses the
    // selection. Cursor lands at the anchor (start of the
    // selection).
    assert_buffer_scenario(BufferScenario {
        description: "SelectDown then MoveLeft collapses selection at anchor".into(),
        initial_text: "abc\ndef".into(),
        actions: vec![Action::SelectDown, Action::MoveLeft],
        expected_text: "abc\ndef".into(),
        // Anchor was 0; deselect-on-move parks cursor at anchor-1
        // if available, but at byte 0 we clamp to 0.
        expected_primary: CursorExpect::at(0),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

#[test]
fn bulk_uppercase_with_no_selection_uppercases_full_buffer() {
    // FINDING (theorem-only): the editor's ToUpperCase action
    // uppercases the *entire buffer* when no selection is active,
    // not the word/line under cursor. Pinning that down.
    assert_buffer_scenario(BufferScenario {
        description: "ToUpperCase with no selection uppercases the entire buffer".into(),
        initial_text: "hello".into(),
        actions: vec![Action::ToUpperCase],
        expected_text: "HELLO".into(),
        // Cursor lands at end of the upcased range.
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    });
}

#[test]
fn bulk_lowercase_after_uppercase_round_trip_to_original_case() {
    assert_buffer_scenario(BufferScenario {
        description: "ToUpperCase + ToLowerCase on selection round-trips ASCII".into(),
        initial_text: "hello".into(),
        actions: vec![
            Action::SelectAll,
            Action::ToUpperCase,
            Action::SelectAll,
            Action::ToLowerCase,
        ],
        expected_text: "hello".into(),
        // SelectAll + Lower collapses cursor at selection end.
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    });
}

#[test]
fn bulk_uppercase_full_buffer_via_select_all() {
    assert_buffer_scenario(BufferScenario {
        description: "SelectAll + ToUpperCase uppercases everything".into(),
        initial_text: "abc".into(),
        actions: vec![Action::SelectAll, Action::ToUpperCase],
        expected_text: "ABC".into(),
        // Selection collapses at end after case ops.
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn bulk_auto_indent_off_means_newline_does_not_pad() {
    // With auto_indent off, InsertNewline doesn't carry indent.
    assert_buffer_scenario(BufferScenario {
        description: "auto_indent=false: InsertNewline starts the new line at col 0".into(),
        initial_text: "    indented line".into(),
        behavior: BehaviorFlags {
            auto_indent: false,
            ..Default::default()
        },
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "    indented line\n".into(),
        expected_primary: CursorExpect::at(18),
        ..Default::default()
    });
}

#[test]
fn bulk_auto_indent_on_carries_indent_through_newline() {
    assert_buffer_scenario(BufferScenario {
        description: "auto_indent=true: InsertNewline at end of '    foo' carries 4 spaces".into(),
        initial_text: "    foo".into(),
        behavior: BehaviorFlags {
            auto_indent: true,
            ..Default::default()
        },
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "    foo\n    ".into(),
        expected_primary: CursorExpect::at(12),
        ..Default::default()
    });
}
