//! Migrations of `tests/e2e/multibyte_characters.rs` claims not
//! covered by `migrated_multibyte_full.rs`. Focus on UTF-8 safety
//! across mixed ASCII/CJK content for backspace, delete, and
//! shift-home/shift-end selection.
//!
//! No mocks: every test routes the production grapheme-boundary
//! code through `Action::Backspace` / `Action::DeleteForward` /
//! `Action::SelectLineEnd` etc.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, repeat, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_backspace_in_middle_of_mixed_ascii_chinese() {
    // Original: `test_backspace_middle_of_mixed_content`.
    // Buffer "abc你好def" (12 bytes: 3 ASCII + 你 (3 bytes, U+4F60)
    // + 好 (3 bytes, U+597D) + 3 ASCII = 12).
    // Move 4 graphemes right (after 你, byte 6), Backspace must
    // remove the entire 你 codepoint, not 1 byte.
    assert_buffer_scenario(BufferScenario {
        description: "Backspace in mixed ASCII/CJK removes the full 3-byte 你 codepoint".into(),
        initial_text: "abc你好def".into(),
        actions: repeat(Action::MoveRight, 4)
            .chain(std::iter::once(Action::DeleteBackward))
            .collect(),
        expected_text: "abc好def".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_delete_forward_in_middle_of_mixed_ascii_chinese() {
    // Original: `test_delete_forward_middle_of_mixed_content`.
    // Move 3 graphemes right (before 你, byte 3), DeleteForward
    // must remove the full 你 codepoint.
    assert_buffer_scenario(BufferScenario {
        description: "DeleteForward in mixed ASCII/CJK removes the full 你 codepoint".into(),
        initial_text: "abc你好def".into(),
        actions: repeat(Action::MoveRight, 3)
            .chain(std::iter::once(Action::DeleteForward))
            .collect(),
        expected_text: "abc好def".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_repeated_backspace_drains_mixed_buffer_atomically() {
    // Original: `test_backspace_delete_never_corrupt_utf8`
    // (backward half). 7 backspaces from end-of-buffer must drain
    // "a中b文c字d" entirely and never leave invalid UTF-8 along
    // the way. Final state: empty buffer.
    assert_buffer_scenario(BufferScenario {
        description: "7 Backspaces from end of 'a中b文c字d' empty the buffer (no UTF-8 corruption)".into(),
        initial_text: "a中b文c字d".into(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::DeleteBackward,
            Action::DeleteBackward,
            Action::DeleteBackward,
            Action::DeleteBackward,
            Action::DeleteBackward,
            Action::DeleteBackward,
            Action::DeleteBackward,
        ],
        expected_text: String::new(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_repeated_delete_forward_drains_mixed_buffer_atomically() {
    // Original: `test_backspace_delete_never_corrupt_utf8`
    // (forward half). 7 forward-deletes from byte 0.
    assert_buffer_scenario(BufferScenario {
        description: "7 DeleteForwards from start of 'a中b文c字d' empty the buffer".into(),
        initial_text: "a中b文c字d".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::DeleteForward,
            Action::DeleteForward,
            Action::DeleteForward,
            Action::DeleteForward,
            Action::DeleteForward,
            Action::DeleteForward,
            Action::DeleteForward,
        ],
        expected_text: String::new(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_shift_end_selects_to_eol_through_chinese() {
    // Original: `test_selection_shift_end_chinese`. Buffer is
    // "你好世界" (12 bytes). MoveRight from byte 0 → byte 3
    // (after 你). SelectLineEnd → selection 3..12. Replace with
    // 'X' → "你X".
    assert_buffer_scenario(BufferScenario {
        description: "SelectLineEnd through CJK selects 3..12; type X yields '你X'".into(),
        initial_text: "你好世界".into(),
        actions: vec![
            Action::MoveRight,
            Action::SelectLineEnd,
            Action::InsertChar('X'),
        ],
        expected_text: "你X".into(),
        // After replacement: cursor at byte 4 (3 bytes for 你 + 1
        // byte for X).
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    });
}

#[test]
fn migrated_shift_home_selects_to_bol_through_chinese() {
    // Original: `test_selection_shift_home_chinese`. From end of
    // "你好世界" (byte 12), MoveLeft → byte 9 (before 界).
    // SelectLineStart → selection 0..9. Replace with 'X'.
    // Buffer becomes "X界" (X + 界 = 1 + 3 = 4 bytes).
    assert_buffer_scenario(BufferScenario {
        description: "SelectLineStart through CJK selects 0..9; type X yields 'X界'".into(),
        initial_text: "你好世界".into(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::SelectLineStart,
            Action::InsertChar('X'),
        ],
        expected_text: "X界".into(),
        expected_primary: CursorExpect::at(1),
        ..Default::default()
    });
}

/// Anti-test: drops the leading `MoveRight` from the
/// shift-end-chinese scenario. Without it, cursor stays at byte 0,
/// SelectLineEnd selects 0..12 ("你好世界" entirely), 'X' replaces
/// everything → "X" (1 byte). The expectation "你X" cannot match.
#[test]
fn anti_shift_end_chinese_dropping_move_right_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: leading MoveRight dropped — selection covers whole buffer".into(),
        initial_text: "你好世界".into(),
        actions: vec![Action::SelectLineEnd, Action::InsertChar('X')],
        expected_text: "你X".into(),
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without the leading MoveRight, SelectLineEnd covers the \
         entire buffer and the 你-prefixed expectation cannot match"
    );
}
