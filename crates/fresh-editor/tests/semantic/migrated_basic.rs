//! Migrated from `tests/e2e/basic.rs`.
//!
//! The originals exercise basic editing workflows through
//! send_key/type_text. The scenarios below capture the same
//! editing-level claims as data — file-open and multi-buffer
//! workflows belong in WorkspaceScenario / PersistenceScenario,
//! not here.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_basic_editing_workflow_typing_inserts_at_cursor() {
    assert_buffer_scenario(BufferScenario {
        description: "typing builds the buffer left-to-right".into(),
        initial_text: String::new(),
        actions: vec![Action::InsertChar('h'), Action::InsertChar('i')],
        expected_text: "hi".into(),
        expected_primary: CursorExpect::at(2),
        ..Default::default()
    });
}

#[test]
fn migrated_append_at_end_of_file() {
    // Original: `test_append_at_end_of_file` (tests/e2e/basic.rs:278).
    // The e2e walks the full append sequence: Ctrl+End, two
    // chars, Enter, six more chars. The buggy path was
    // specifically "after appending past the original EOF, can
    // we still Enter and type on a new line?" — pinning the
    // full sequence so a regression at any step surfaces.
    assert_buffer_scenario(BufferScenario {
        description: "Ctrl+End + '!' + '!' + Enter + 'Line 4' appends through new line at EOF"
            .into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::InsertChar('!'),
            Action::InsertChar('!'),
            Action::InsertNewline,
            Action::InsertChar('L'),
            Action::InsertChar('i'),
            Action::InsertChar('n'),
            Action::InsertChar('e'),
            Action::InsertChar(' '),
            Action::InsertChar('4'),
        ],
        expected_text: "Line 1\nLine 2\nLine 3!!\nLine 4".into(),
        expected_primary: CursorExpect::at(29),
        ..Default::default()
    });
}

#[test]
fn migrated_enter_in_middle_splits_line() {
    assert_buffer_scenario(BufferScenario {
        description: "InsertNewline mid-line splits into two lines".into(),
        initial_text: "abcde".into(),
        actions: vec![Action::MoveRight, Action::MoveRight, Action::InsertNewline],
        expected_text: "ab\ncde".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_delete_forward_at_eof_is_noop() {
    assert_buffer_scenario(BufferScenario {
        description: "DeleteForward at end-of-buffer leaves text intact".into(),
        initial_text: "abc".into(),
        actions: vec![Action::MoveDocumentEnd, Action::DeleteForward],
        expected_text: "abc".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_delete_backward_at_bof_is_noop() {
    assert_buffer_scenario(BufferScenario {
        description: "DeleteBackward at beginning-of-buffer leaves text intact".into(),
        initial_text: "abc".into(),
        actions: vec![Action::DeleteBackward],
        expected_text: "abc".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}
