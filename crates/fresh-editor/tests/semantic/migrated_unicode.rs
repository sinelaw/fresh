//! Migrated unicode-related claims, distinct from the earlier
//! `unicode_cursor.rs` (which already covered Norwegian / Thai /
//! emoji grapheme behaviour). The scenarios below pin specific
//! byte offsets for inserting / deleting / selecting in
//! multibyte buffers — the kinds of asserts the imperative
//! `tests/e2e/multibyte_characters.rs` makes.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_typing_after_two_byte_char_lands_at_correct_offset() {
    // 'é' is 2 bytes in UTF-8. Typing after it should leave the
    // cursor at byte 3, not byte 2 (which would be mid-codepoint).
    assert_buffer_scenario(BufferScenario {
        description: "InsertChar after 'é' lands at byte 3".into(),
        initial_text: "é".into(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertChar('x')],
        expected_text: "éx".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_select_all_on_multibyte_buffer_covers_all_bytes() {
    // 'café' = 5 bytes (c=1, a=1, f=1, é=2). SelectAll → range 0..5.
    assert_buffer_scenario(BufferScenario {
        description: "SelectAll on 'café' selects the full 5-byte range".into(),
        initial_text: "café".into(),
        actions: vec![Action::SelectAll],
        expected_text: "café".into(),
        expected_primary: CursorExpect::range(0, 5),
        expected_selection_text: Some("café".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_delete_backward_removes_full_codepoint() {
    // Deleting after a multibyte char should remove the whole
    // codepoint, not just one byte.
    assert_buffer_scenario(BufferScenario {
        description: "DeleteBackward after 'é' removes both bytes".into(),
        initial_text: "café".into(),
        actions: vec![Action::MoveDocumentEnd, Action::DeleteBackward],
        expected_text: "caf".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

/// Anti-test: drops `DeleteBackward` from
/// `migrated_delete_backward_removes_full_codepoint`. Without
/// it, the buffer stays "café" with cursor at byte 5, so the
/// expected "caf" with cursor at byte 3 cannot match.
#[test]
fn anti_unicode_dropping_delete_backward_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: DeleteBackward dropped — 'é' is not removed from 'café'".into(),
        initial_text: "café".into(),
        actions: vec![Action::MoveDocumentEnd],
        expected_text: "caf".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without DeleteBackward the buffer stays 'café'; \
         the trailing 'é' is not stripped to leave 'caf'"
    );
}

#[test]
fn migrated_move_left_steps_over_multibyte() {
    // From byte 5 (after 'é'), MoveLeft should land at byte 3, not 4.
    assert_buffer_scenario(BufferScenario {
        description: "MoveLeft over 'é' lands at byte 3".into(),
        initial_text: "café".into(),
        actions: vec![Action::MoveDocumentEnd, Action::MoveLeft],
        expected_text: "café".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}
