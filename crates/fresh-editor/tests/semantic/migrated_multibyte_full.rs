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
    // From byte 0 of "你好世界" (4 chars, 12 bytes), each MoveRight
    // steps over a full 3-byte codepoint: byte 3, 6, 9, 12. The
    // original asserts all four positions, then verifies by typing
    // "X" after two Rights → "你好X世界".
    //
    // BufferScenario pins final state only, so each position is a
    // separate scenario; together they reproduce the per-Right
    // assertions, and a fifth scenario reproduces the insert check.
    for (rights, expected_byte) in [(1usize, 3usize), (2, 6), (3, 9), (4, 12)] {
        let mut actions = vec![Action::MoveLineStart];
        actions.extend(std::iter::repeat(Action::MoveRight).take(rights));
        assert_buffer_scenario(BufferScenario {
            description: format!("MoveRight ×{rights} on '你好世界' lands at byte {expected_byte}"),
            initial_text: "你好世界".into(),
            actions,
            expected_text: "你好世界".into(),
            expected_primary: CursorExpect::at(expected_byte),
            ..Default::default()
        });
    }
    // Insert verification: Home, Right ×2, type 'X' → "你好X世界".
    assert_buffer_scenario(BufferScenario {
        description: "Home + MoveRight ×2 + 'X' on '你好世界' yields '你好X世界'".into(),
        initial_text: "你好世界".into(),
        actions: vec![
            Action::MoveLineStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::InsertChar('X'),
        ],
        expected_text: "你好X世界".into(),
        expected_primary: CursorExpect::at(7), // byte 6 + 'X'
        ..Default::default()
    });
}

#[test]
fn migrated_backspace_removes_full_chinese_codepoint() {
    // Original: `test_backspace_chinese_characters`.
    // "你好": one DeleteBackward → "你", a second → "" (each
    // removes a full 3-byte codepoint, not a single byte).
    assert_buffer_scenario(BufferScenario {
        description: "DeleteBackward ×1 on '你好' removes full 3-byte '好' → '你'".into(),
        initial_text: "你好".into(),
        actions: vec![Action::MoveDocumentEnd, Action::DeleteBackward],
        expected_text: "你".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
    assert_buffer_scenario(BufferScenario {
        description: "DeleteBackward ×2 on '你好' empties the buffer".into(),
        initial_text: "你好".into(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::DeleteBackward,
            Action::DeleteBackward,
        ],
        expected_text: String::new(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_delete_forward_removes_full_chinese_codepoint() {
    // Original: `test_delete_forward_chinese_characters`.
    // "你好" from byte 0: one DeleteForward → "好", a second → ""
    // (each removes a full 3-byte codepoint).
    assert_buffer_scenario(BufferScenario {
        description: "DeleteForward ×1 on '你好' removes full 3-byte '你' → '好'".into(),
        initial_text: "你好".into(),
        actions: vec![Action::MoveLineStart, Action::DeleteForward],
        expected_text: "好".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
    assert_buffer_scenario(BufferScenario {
        description: "DeleteForward ×2 on '你好' empties the buffer".into(),
        initial_text: "你好".into(),
        actions: vec![
            Action::MoveLineStart,
            Action::DeleteForward,
            Action::DeleteForward,
        ],
        expected_text: String::new(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_select_shift_right_chinese() {
    // Original: `test_selection_shift_right_chinese`.
    // "你好世界", Home, Shift+Right selects the first 3-byte char
    // (cursor at byte 3), then Backspace deletes the selection →
    // "好世界".
    assert_buffer_scenario(BufferScenario {
        description: "SelectRight then DeleteBackward on '你好世界' yields '好世界'".into(),
        initial_text: "你好世界".into(),
        actions: vec![
            Action::MoveLineStart,
            Action::SelectRight,
            Action::DeleteBackward,
        ],
        expected_text: "好世界".into(),
        expected_primary: CursorExpect::at(0),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_shift_left_chinese() {
    // Original: `test_selection_shift_left_chinese`.
    // "你好世界" from end (byte 12), Shift+Left selects the last
    // 3-byte char (cursor at byte 9), then typing 'X' replaces the
    // selection → "你好世X".
    assert_buffer_scenario(BufferScenario {
        description: "SelectLeft then type 'X' on '你好世界' yields '你好世X'".into(),
        initial_text: "你好世界".into(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::SelectLeft,
            Action::InsertChar('X'),
        ],
        expected_text: "你好世X".into(),
        expected_primary: CursorExpect::at(10), // byte 9 + 'X'
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

/// Anti-test: drops the final `DeleteBackward` from
/// `migrated_select_multiple_chinese_characters`. Without the
/// delete, the buffer is still "一二三四五", not "四五" — proving
/// the delete (over a 3-char selection) is what removes the
/// leading three CJK codepoints.
#[test]
fn anti_multibyte_dropping_select_delete_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: DeleteBackward dropped — buffer stays '一二三四五', not '四五'".into(),
        initial_text: "一二三四五".into(),
        actions: vec![
            Action::MoveLineStart,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            // DeleteBackward removed.
        ],
        expected_text: "四五".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without DeleteBackward the 3-char selection isn't removed; \
         the buffer cannot become '四五'"
    );
}

#[test]
fn migrated_select_multiple_chinese_characters() {
    // Original: `test_selection_multiple_chinese_characters`.
    // "一二三四五", Home, Shift+Right ×3 selects the first three
    // CJK chars (cursor at byte 9), then Backspace deletes them →
    // "四五".
    assert_buffer_scenario(BufferScenario {
        description: "3 SelectRights + DeleteBackward on '一二三四五' yields '四五'".into(),
        initial_text: "一二三四五".into(),
        actions: vec![
            Action::MoveLineStart,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::DeleteBackward,
        ],
        expected_text: "四五".into(),
        expected_primary: CursorExpect::at(0),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}
