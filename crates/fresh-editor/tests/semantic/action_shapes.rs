//! Action-shape parity scenarios. These tests pin down what
//! single Action verbs do on canonical inputs — they're the
//! "unit tests" of the editor's action alphabet.
//!
//! Each test exercises one specific Action variant in isolation,
//! so a regression points at exactly one verb's semantics rather
//! than at a multi-step interaction.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn action_move_line_start_jumps_to_byte_zero_on_single_line() {
    assert_buffer_scenario(BufferScenario {
        description: "MoveLineStart from byte 5 of single line lands at byte 0".into(),
        initial_text: "hello".into(),
        actions: vec![Action::MoveDocumentEnd, Action::MoveLineStart],
        expected_text: "hello".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn action_move_line_end_jumps_to_byte_n_on_single_line() {
    assert_buffer_scenario(BufferScenario {
        description: "MoveLineEnd from byte 0 of 'hello' lands at byte 5".into(),
        initial_text: "hello".into(),
        actions: vec![Action::MoveLineEnd],
        expected_text: "hello".into(),
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    });
}

#[test]
fn action_move_document_start_lands_at_byte_zero() {
    assert_buffer_scenario(BufferScenario {
        description: "MoveDocumentStart from anywhere lands at byte 0".into(),
        initial_text: "alpha\nbravo".into(),
        actions: vec![Action::MoveDocumentEnd, Action::MoveDocumentStart],
        expected_text: "alpha\nbravo".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn action_move_document_end_lands_at_buffer_length() {
    let text = "alpha\nbravo";
    assert_buffer_scenario(BufferScenario {
        description: "MoveDocumentEnd lands at byte len".into(),
        initial_text: text.into(),
        actions: vec![Action::MoveDocumentEnd],
        expected_text: text.into(),
        expected_primary: CursorExpect::at(text.len()),
        ..Default::default()
    });
}

#[test]
fn action_select_all_yields_full_buffer_range() {
    let text = "the quick brown fox";
    assert_buffer_scenario(BufferScenario {
        description: "SelectAll yields range 0..len".into(),
        initial_text: text.into(),
        actions: vec![Action::SelectAll],
        expected_text: text.into(),
        expected_primary: CursorExpect::range(0, text.len()),
        expected_selection_text: Some(text.into()),
        ..Default::default()
    });
}

#[test]
fn action_insert_char_at_cursor_advances_by_one_byte_for_ascii() {
    assert_buffer_scenario(BufferScenario {
        description: "InsertChar(ASCII) advances cursor by 1 byte".into(),
        initial_text: String::new(),
        actions: vec![Action::InsertChar('z')],
        expected_text: "z".into(),
        expected_primary: CursorExpect::at(1),
        ..Default::default()
    });
}

#[test]
fn action_insert_char_for_3_byte_codepoint_advances_by_3_bytes() {
    assert_buffer_scenario(BufferScenario {
        description: "InsertChar(BMP CJK char) advances cursor by 3 bytes".into(),
        initial_text: String::new(),
        actions: vec![Action::InsertChar('日')],
        expected_text: "日".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn action_delete_backward_at_cursor_removes_one_char_for_ascii() {
    assert_buffer_scenario(BufferScenario {
        description: "DeleteBackward at end of 'abc' removes 'c'".into(),
        initial_text: "abc".into(),
        actions: vec![Action::MoveDocumentEnd, Action::DeleteBackward],
        expected_text: "ab".into(),
        expected_primary: CursorExpect::at(2),
        ..Default::default()
    });
}

#[test]
fn action_delete_forward_at_cursor_removes_one_char_for_ascii() {
    assert_buffer_scenario(BufferScenario {
        description: "DeleteForward at byte 0 of 'abc' removes 'a'".into(),
        initial_text: "abc".into(),
        actions: vec![Action::DeleteForward],
        expected_text: "bc".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn action_undo_after_no_edits_is_no_op() {
    assert_buffer_scenario(BufferScenario {
        description: "Undo with empty history is a no-op".into(),
        initial_text: "stable".into(),
        actions: vec![Action::Undo],
        expected_text: "stable".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn action_redo_after_no_undos_is_no_op() {
    assert_buffer_scenario(BufferScenario {
        description: "Redo with no redo stack is a no-op".into(),
        initial_text: "stable".into(),
        actions: vec![Action::Redo],
        expected_text: "stable".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn action_insert_newline_at_byte_zero_creates_leading_empty_line() {
    assert_buffer_scenario(BufferScenario {
        description: "InsertNewline at byte 0 creates a leading blank line".into(),
        initial_text: "x".into(),
        actions: vec![Action::InsertNewline],
        expected_text: "\nx".into(),
        expected_primary: CursorExpect::at(1),
        ..Default::default()
    });
}
