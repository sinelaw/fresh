//! Additional unicode/multibyte scenarios — claims from
//! `tests/e2e/multibyte_characters.rs` and
//! `tests/e2e/issue_1577_unicode_width.rs`.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_chinese_typing_lands_at_3_byte_offset() {
    // CJK chars are 3 bytes in UTF-8.
    assert_buffer_scenario(BufferScenario {
        description: "InsertChar('中') leaves cursor at byte 3".into(),
        initial_text: String::new(),
        actions: vec![Action::InsertChar('中')],
        expected_text: "中".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_select_all_on_japanese_buffer_covers_full_byte_range() {
    let text = "こんにちは"; // 5 chars, 3 bytes each = 15 bytes
    assert_buffer_scenario(BufferScenario {
        description: "SelectAll on 'こんにちは' yields range 0..15".into(),
        initial_text: text.into(),
        actions: vec![Action::SelectAll],
        expected_text: text.into(),
        expected_primary: CursorExpect::range(0, 15),
        expected_selection_text: Some(text.into()),
        ..Default::default()
    });
}

#[test]
fn migrated_delete_backward_removes_full_japanese_codepoint() {
    let text = "ありがとう";
    let len = text.len();
    assert_buffer_scenario(BufferScenario {
        description: "DeleteBackward removes one full multibyte codepoint".into(),
        initial_text: text.into(),
        actions: vec![Action::MoveDocumentEnd, Action::DeleteBackward],
        expected_text: "ありがと".into(),
        // Each Japanese char is 3 bytes; we deleted one.
        expected_primary: CursorExpect::at(len - 3),
        ..Default::default()
    });
}

#[test]
fn migrated_mixed_ascii_and_multibyte_byte_positions() {
    // "a中b" is 1 + 3 + 1 = 5 bytes. MoveLeft from end → byte 4
    // (between '中' and 'b').
    assert_buffer_scenario(BufferScenario {
        description: "MoveLeft from end of 'a中b' lands at byte 4".into(),
        initial_text: "a中b".into(),
        actions: vec![Action::MoveDocumentEnd, Action::MoveLeft],
        expected_text: "a中b".into(),
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    });
}

/// Anti-test: drops `MoveLeft` from
/// `migrated_mixed_ascii_and_multibyte_byte_positions`. Without
/// it, the cursor stays at byte 5 (end of "a中b") rather than
/// stepping back over the 3-byte CJK char to byte 4. Proves
/// MoveLeft is the load-bearing grapheme-aware step.
#[test]
fn anti_more_unicode_dropping_move_left_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: MoveLeft dropped — cursor stays at byte 5 not 4".into(),
        initial_text: "a中b".into(),
        actions: vec![Action::MoveDocumentEnd],
        expected_text: "a中b".into(),
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without MoveLeft the cursor sits at byte 5; \
         the grapheme-aware byte-4 landing cannot be observed"
    );
}
