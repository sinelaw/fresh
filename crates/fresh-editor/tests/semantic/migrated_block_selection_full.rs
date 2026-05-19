//! Faithful migration of `tests/e2e/block_selection.rs` (those
//! cases that don't require Esc/clear-selection or
//! cycle-state semantics).

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_block_select_down_creates_selection() {
    // Original: `test_block_select_down_basic`. Type 3 lines,
    // MoveDocumentStart, MoveRight ×6 (to col 6), BlockSelectDown.
    // The original asserts `harness.has_selection()` only.
    // Scenario adds the exact byte range pin.
    assert_buffer_scenario(BufferScenario {
        description: "BlockSelectDown from col 6 of line 1 selects to col 6 of line 2".into(),
        initial_text: "line1 text here\nline2 text here\nline3 text here".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::BlockSelectDown,
        ],
        expected_text: "line1 text here\nline2 text here\nline3 text here".into(),
        // BlockSelectDown adds a 2nd cursor on the next line at
        // the same column.
        expected_primary: CursorExpect::range(6, 22),
        ..Default::default()
    });
}

#[test]
fn migrated_block_select_down_three_times_extends_through_lines() {
    // Original: `test_block_select_multiple_consecutive`.
    // 5 lines × "aaaa bbbb cccc" (14 chars + \n = 15 bytes).
    // Move to col 5; 3 BlockSelectDown calls extend selection
    // through line 4. The original only asserts has_selection
    // throughout; scenario verifies the final state.
    assert_buffer_scenario(BufferScenario {
        description: "Three BlockSelectDown calls extend selection across 4 lines".into(),
        initial_text:
            "aaaa bbbb cccc\naaaa bbbb cccc\naaaa bbbb cccc\naaaa bbbb cccc\naaaa bbbb cccc".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::BlockSelectDown,
            Action::BlockSelectDown,
            Action::BlockSelectDown,
        ],
        expected_text:
            "aaaa bbbb cccc\naaaa bbbb cccc\naaaa bbbb cccc\naaaa bbbb cccc\naaaa bbbb cccc".into(),
        // Selection: anchor at col 5 of line 1 (byte 5), cursor
        // at col 5 of line 4 (byte 5+15*3 = 50).
        expected_primary: CursorExpect::range(5, 50),
        ..Default::default()
    });
}

#[test]
fn migrated_block_select_then_type_inserts_at_each_cursor() {
    // Original: `test_block_select_then_type`. After
    // BlockSelectDown creates a multi-cursor block, typing
    // inserts at each cursor position.
    //
    // E2e:
    //   type "line1 text\nline2 text"  (cursor at byte 21)
    //   Ctrl+Home → byte 0
    //   Right ×5 → byte 5
    //   Alt+Shift+Down → block-select to col 5 of line 2
    //   type "X" at each cursor
    //
    // Result: "line1X text\nline2X text".
    assert_buffer_scenario(BufferScenario {
        description: "BlockSelectDown + InsertChar inserts at each cursor".into(),
        initial_text: "line1 text\nline2 text".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::BlockSelectDown,
            Action::InsertChar('X'),
        ],
        expected_text: "line1X text\nline2X text".into(),
        // Two cursors, both advanced past inserted X.
        // Primary on line 2 at byte 18 (= "line1X text\n" = 12,
        // then "line2X" = 6 → 18).
        expected_primary: CursorExpect::at(18),
        expected_extra_cursors: vec![CursorExpect::at(6)],
        expected_selection_text: Some("".into()),
        ..Default::default()
    });
}

/// Anti-test: drops `BlockSelectDown` from
/// `migrated_block_select_then_type_inserts_at_each_cursor`.
/// Without it, only one cursor exists and InsertChar('X') lands
/// on line 1 only, so the expected
/// "line1X text\nline2X text" cannot match.
#[test]
fn anti_block_selection_dropping_block_select_down_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: BlockSelectDown dropped — only one cursor, X inserts on line 1 only"
            .into(),
        initial_text: "line1 text\nline2 text".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::InsertChar('X'),
        ],
        expected_text: "line1X text\nline2X text".into(),
        expected_primary: CursorExpect::at(18),
        expected_extra_cursors: vec![CursorExpect::at(6)],
        expected_selection_text: Some("".into()),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without BlockSelectDown no secondary cursor exists, \
         so X cannot be inserted on line 2 simultaneously"
    );
}
