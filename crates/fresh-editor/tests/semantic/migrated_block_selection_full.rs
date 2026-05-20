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

// NOTE: `test_block_select_then_type` is NOT migrated here — it is
// faithfully covered by
// `migrated_block_selection_extras::migrated_block_select_then_type_clears_selection`
// (buffer "aaaa\nbbbb\ncccc", BlockSelectDown + BlockSelectRight ×2 +
// InsertChar('X'), asserting cleared selection). A previously-present
// scenario in this file claimed to migrate it but used an invented
// buffer "line1 text\nline2 text" + a different action sequence; it
// was a synthesized duplicate of the BlockSelectDown coverage already
// pinned by `migrated_block_select_down_creates_selection` below, so
// it was removed as redundant/mislabeled.

/// Anti-test: drops `BlockSelectDown` from
/// `migrated_block_select_down_creates_selection`. Without it the
/// primary cursor stays collapsed at byte 6 (no anchor), so the
/// expected backward/forward range 6..22 cannot match.
#[test]
fn anti_block_selection_dropping_block_select_down_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: BlockSelectDown dropped — cursor stays collapsed at byte 6, no range"
            .into(),
        initial_text: "line1 text here\nline2 text here\nline3 text here".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
        ],
        expected_text: "line1 text here\nline2 text here\nline3 text here".into(),
        // Same expectation as the real test, but without the
        // BlockSelectDown there is no selection range to match.
        expected_primary: CursorExpect::range(6, 22),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without BlockSelectDown the cursor stays collapsed at byte 6, \
         so the 6..22 selection range cannot match"
    );
}
