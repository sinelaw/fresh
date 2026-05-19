//! Faithful migration of `tests/e2e/multibyte_characters.rs`
//! (cursor / movement / selection subset).
//!
//! Each Chinese (CJK) char is 3 bytes in UTF-8. Tests pin exact
//! byte positions so any future regression that breaks
//! grapheme-cluster movement surfaces here.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_end_key_with_chinese_characters() {
    // Original: `test_end_key_with_chinese_characters`.
    // Type 4 CJK chars (12 bytes), Home, End — cursor at 12.
    assert_buffer_scenario(BufferScenario {
        description: "MoveLineEnd on '你好世界' (12 bytes) lands at byte 12".into(),
        initial_text: "你好世界".into(),
        actions: vec![Action::MoveLineStart, Action::MoveLineEnd],
        expected_text: "你好世界".into(),
        expected_primary: CursorExpect::at(12),
        ..Default::default()
    });
}

#[test]
fn migrated_end_key_with_mixed_ascii_and_chinese() {
    // Original: `test_end_key_with_mixed_ascii_and_chinese`.
    // "Hello 你好 World" = 18 bytes.
    let text = "Hello 你好 World";
    assert_buffer_scenario(BufferScenario {
        description: "MoveLineEnd on mixed ASCII+CJK lands at full byte length".into(),
        initial_text: text.into(),
        actions: vec![Action::MoveLineStart, Action::MoveLineEnd],
        expected_text: text.into(),
        expected_primary: CursorExpect::at(text.len()),
        ..Default::default()
    });
}

#[test]
fn migrated_typing_after_mixed_ascii_chinese_lands_at_end() {
    // Companion to above: `harness.type_text("!")` after End.
    let text = "Hello 你好 World";
    assert_buffer_scenario(BufferScenario {
        description: "InsertChar after MoveLineEnd appends to mixed-content line".into(),
        initial_text: text.into(),
        actions: vec![Action::MoveLineEnd, Action::InsertChar('!')],
        expected_text: format!("{text}!"),
        expected_primary: CursorExpect::at(text.len() + 1),
        ..Default::default()
    });
}

#[test]
fn migrated_cursor_left_steps_over_full_chinese_codepoint() {
    // Original: `test_cursor_left_with_chinese_characters`.
    // "你好" (6 bytes). From byte 6, MoveLeft → 3, then 0.
    assert_buffer_scenario(BufferScenario {
        description: "MoveLeft from end of '你好' lands at byte 3 (between chars)".into(),
        initial_text: "你好".into(),
        actions: vec![Action::MoveDocumentEnd, Action::MoveLeft],
        expected_text: "你好".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_cursor_left_twice_lands_at_byte_zero() {
    // Continuation of above. Two Lefts from end → byte 0.
    assert_buffer_scenario(BufferScenario {
        description: "Two MoveLefts from end of '你好' lands at byte 0".into(),
        initial_text: "你好".into(),
        actions: vec![Action::MoveDocumentEnd, Action::MoveLeft, Action::MoveLeft],
        expected_text: "你好".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_typing_at_byte_zero_of_chinese_buffer_prepends() {
    // Companion: from byte 0, InsertChar('X') prepends.
    assert_buffer_scenario(BufferScenario {
        description: "InsertChar at byte 0 of CJK buffer prepends".into(),
        initial_text: "你好".into(),
        actions: vec![Action::MoveLineStart, Action::InsertChar('X')],
        expected_text: "X你好".into(),
        expected_primary: CursorExpect::at(1),
        ..Default::default()
    });
}

#[test]
fn migrated_cursor_right_steps_over_full_chinese_codepoint() {
    // Original: `test_cursor_right_with_chinese_characters`.
    // From byte 0 of "你好世界" (12 bytes), one MoveRight → byte 3.
    assert_buffer_scenario(BufferScenario {
        description: "MoveRight from byte 0 of '你好世界' lands at byte 3".into(),
        initial_text: "你好世界".into(),
        actions: vec![Action::MoveLineStart, Action::MoveRight],
        expected_text: "你好世界".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_backspace_removes_full_chinese_codepoint() {
    // Original: `test_backspace_chinese_characters`.
    // From end of "你好" (byte 6), DeleteBackward → buffer "你",
    // cursor at 3.
    assert_buffer_scenario(BufferScenario {
        description: "DeleteBackward on CJK buffer removes full 3-byte codepoint".into(),
        initial_text: "你好".into(),
        actions: vec![Action::MoveDocumentEnd, Action::DeleteBackward],
        expected_text: "你".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_delete_forward_removes_full_chinese_codepoint() {
    // Original: `test_delete_forward_chinese_characters`.
    // From byte 0 of "你好", DeleteForward → buffer "好",
    // cursor at 0.
    assert_buffer_scenario(BufferScenario {
        description: "DeleteForward on CJK buffer removes full 3-byte codepoint".into(),
        initial_text: "你好".into(),
        actions: vec![Action::MoveLineStart, Action::DeleteForward],
        expected_text: "好".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_select_shift_right_chinese() {
    // Original: `test_selection_shift_right_chinese`.
    // From byte 0, SelectRight ⇒ selects first CJK char (3 bytes).
    assert_buffer_scenario(BufferScenario {
        description: "SelectRight from byte 0 of '你好' selects 3-byte first char".into(),
        initial_text: "你好".into(),
        actions: vec![Action::SelectRight],
        expected_text: "你好".into(),
        expected_primary: CursorExpect::range(0, 3),
        expected_selection_text: Some("你".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_shift_left_chinese() {
    // Original: `test_selection_shift_left_chinese`.
    // From end (byte 6), SelectLeft ⇒ selects last CJK char.
    assert_buffer_scenario(BufferScenario {
        description: "SelectLeft from end of '你好' selects last 3-byte char".into(),
        initial_text: "你好".into(),
        actions: vec![Action::MoveDocumentEnd, Action::SelectLeft],
        expected_text: "你好".into(),
        expected_primary: CursorExpect::range(6, 3),
        expected_selection_text: Some("好".into()),
        ..Default::default()
    });
}

/// Anti-test: drops one `SelectRight` from
/// `migrated_select_multiple_chinese_characters`. Without all
/// four SelectRight presses, the selection range falls short of
/// 0..12 — proves each SelectRight extends by exactly one
/// 3-byte CJK grapheme.
#[test]
fn anti_multibyte_dropping_select_right_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: one SelectRight dropped — selection ends at byte 9 not 12".into(),
        initial_text: "你好世界".into(),
        actions: vec![
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            // 4th SelectRight removed.
        ],
        expected_text: "你好世界".into(),
        expected_primary: CursorExpect::range(0, 12),
        expected_selection_text: Some("你好世界".into()),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: with only 3 SelectRights the selection covers 9 bytes, \
         not the full 12-byte range; the four-char selection cannot match"
    );
}

#[test]
fn migrated_select_multiple_chinese_characters() {
    // Original: `test_selection_multiple_chinese_characters`.
    // 4 SelectRights from byte 0 of "你好世界" select all 4 chars.
    assert_buffer_scenario(BufferScenario {
        description: "4 SelectRights select all 4 CJK chars".into(),
        initial_text: "你好世界".into(),
        actions: vec![
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
        ],
        expected_text: "你好世界".into(),
        expected_primary: CursorExpect::range(0, 12),
        expected_selection_text: Some("你好世界".into()),
        ..Default::default()
    });
}
