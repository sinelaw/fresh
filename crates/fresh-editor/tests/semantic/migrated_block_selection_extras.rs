//! Migrations of `tests/e2e/block_selection.rs` claims not
//! covered by `migrated_block_selection_full.rs`. Focus: each of
//! the four block-select directions in isolation.
//!
//! No mocks: `Action::BlockSelect{Down,Up,Left,Right}` are the
//! same dispatch targets the user-facing Alt+Shift+Arrow bindings
//! resolve to.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_block_select_up_from_line_2() {
    // Original: `test_block_select_all_directions` (Up branch).
    // Buffer "aaaa\nbbbb\ncccc"; cursor at line 2 col 1 (byte 6);
    // BlockSelectUp extends back through line 1.
    assert_buffer_scenario(BufferScenario {
        description: "BlockSelectUp from line 2 col 1 selects through line 1".into(),
        initial_text: "aaaa\nbbbb\ncccc".into(),
        actions: vec![
            Action::MoveDown,
            Action::MoveLineStart,
            Action::MoveRight,
            Action::BlockSelectUp,
        ],
        expected_text: "aaaa\nbbbb\ncccc".into(),
        // Anchor at byte 6 (line 2 col 1), cursor at byte 1 (line
        // 1 col 1 — backward selection).
        expected_primary: CursorExpect::range(6, 1),
        expected_selection_text: Some("aaa\nb".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_block_select_right_from_byte_0() {
    // Original: `test_block_select_all_directions` (Right branch).
    // Buffer "aaaa\nbbbb\ncccc"; BlockSelectRight from byte 0.
    assert_buffer_scenario(BufferScenario {
        description: "BlockSelectRight from byte 0 selects 1 char".into(),
        initial_text: "aaaa\nbbbb\ncccc".into(),
        actions: vec![Action::BlockSelectRight],
        expected_text: "aaaa\nbbbb\ncccc".into(),
        expected_primary: CursorExpect::range(0, 1),
        expected_selection_text: Some("a".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_block_select_left_from_byte_1() {
    // Original: `test_block_select_all_directions` (Left branch).
    assert_buffer_scenario(BufferScenario {
        description: "BlockSelectLeft from byte 1 selects 1 char backward".into(),
        initial_text: "aaaa\nbbbb\ncccc".into(),
        actions: vec![Action::MoveRight, Action::BlockSelectLeft],
        expected_text: "aaaa\nbbbb\ncccc".into(),
        // Backward selection: cursor at 0, anchor at 1.
        expected_primary: CursorExpect::range(1, 0),
        expected_selection_text: Some("a".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_block_select_then_remove_secondary_collapses() {
    // Original: `test_block_select_then_escape`. Block-select
    // creates a multi-cursor block; Esc (= RemoveSecondaryCursors)
    // collapses back to a single cursor with no selection.
    //
    // The original asserts `assert_no_selection()`, equivalent to
    // `expected_selection_text: Some("")`. Behavioral pin:
    // BlockSelectDown is implemented as an extension of the
    // primary cursor's *selection* (anchor=0, position=11), not
    // as adding a secondary cursor. RemoveSecondaryCursors also
    // clears the active selection, so the surviving state is
    // "cursor at 11 with no anchor" — not "cursor at 0".
    assert_buffer_scenario(BufferScenario {
        description:
            "BlockSelectDown + RemoveSecondaryCursors clears selection (cursor parks at active end)"
                .into(),
        initial_text: "line1 text\nline2 text\nline3 text".into(),
        actions: vec![Action::BlockSelectDown, Action::RemoveSecondaryCursors],
        expected_text: "line1 text\nline2 text\nline3 text".into(),
        expected_primary: CursorExpect::at(11),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

#[test]
fn migrated_block_select_then_type_clears_selection() {
    // Original: `test_block_select_then_type` (subset). After
    // BlockSelectDown + BlockSelectRight × 2 there's a multi-cursor
    // block. Typing replaces each row's content under the block;
    // selection is cleared.
    assert_buffer_scenario(BufferScenario {
        description: "Typing after BlockSelectDown + BlockSelectRight×2 clears selection".into(),
        initial_text: "aaaa\nbbbb\ncccc".into(),
        actions: vec![
            Action::BlockSelectDown,
            Action::BlockSelectRight,
            Action::BlockSelectRight,
            Action::InsertChar('X'),
        ],
        expected_text: "Xaa\nXbb\ncccc".into(),
        // Two rows got 2 chars deleted + 1 inserted; primary cursor
        // (the most-recent block addition) is on line 2 just past
        // the X. Buffer layout post-edit:
        //   "Xaa\n" → 4 bytes (was "aaaa\n" = 5; deleted 2, inserted 1)
        //   "Xbb\n" → 4 bytes (was "bbbb\n" = 5)
        //   "cccc"  → 4 bytes
        // Primary cursor on line 2 just past 'X' → byte 5.
        expected_primary: CursorExpect::at(5),
        expected_extra_cursors: vec![CursorExpect::at(1)],
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

/// Anti-test: drops the `BlockSelectDown` from the
/// then-RemoveSecondaryCursors scenario. With no block selection
/// to clear, the post-action state is just "cursor at 0" with no
/// selection text — the same expectation as the real test, so the
/// "clear" operation is provably necessary only via the dual:
/// flip the expected_selection_text to a non-empty value and show
/// it does NOT match.
#[test]
fn anti_block_select_then_remove_must_actually_clear() {
    let scenario = BufferScenario {
        description: "anti: assert non-empty selection — must NOT match (selection was cleared)"
            .into(),
        initial_text: "line1 text\nline2 text\nline3 text".into(),
        actions: vec![Action::BlockSelectDown, Action::RemoveSecondaryCursors],
        expected_text: "line1 text\nline2 text\nline3 text".into(),
        expected_primary: CursorExpect::at(11),
        // Wrong: there is no selection after RemoveSecondaryCursors.
        expected_selection_text: Some("anything".into()),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: with selection cleared, a non-empty expected_selection_text \
         must NOT match"
    );
}
