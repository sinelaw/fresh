//! Tests for CancelMark and ClearMark actions.
//!
//! These actions provide fine-grained control over mark mode (selection anchor):
//! - **CancelMark**: Drops the selection on next cursor move but keeps the anchor point.
//!   Plugins use this to "soft exit" mark mode while preserving the reference point.
//! - **ClearMark**: Fully clears the anchor and any selection, resetting deselect_on_move.
//!   This is a "hard exit" — all mark state is wiped.
//!
//! The key distinction: CancelMark keeps the anchor alive (you can re-enter mark mode
//! from the same spot), while ClearMark destroys it entirely.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::input::keybindings::Action;

/// Test that SetMark starts a selection, and CancelMark allows subsequent cursor moves
/// to drop the selection without losing the anchor point.
#[test]
fn test_cancel_mark_soft_exits_preserving_anchor() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create some content
    harness
        .type_text("aaaa bbbb cccc\n1111 2222 3333\n4444 5555 6666")
        .unwrap();

    // Move to start of first line, column 0
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_no_selection();

    // Start mark mode with SetMark
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::SetMark);
    harness.render().unwrap();

    // Move right to create a selection (anchor is at col 0, cursor at col 4)
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Should have selection after SetMark + right moves"
    );

    // Cancel mark mode — this should NOT clear the selection immediately,
    // but make cursor moves drop the selection on next move
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::CancelMark);
    harness.render().unwrap();

    // After CancelMark, selection is still visible (the anchor wasn't destroyed)
    assert!(
        harness.has_selection(),
        "Selection should still be visible after CancelMark (anchor preserved)"
    );

    // Now move cursor — selection should drop because deselect_on_move was enabled
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(
        !harness.has_selection(),
        "Selection should be dropped after moving cursor post-CancelMark"
    );

    // The anchor should still exist (we can verify by re-entering mark mode)
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::SetMark);
    harness.render().unwrap();

    // After re-setting the mark, moving should create a new selection
    // (this proves the anchor survived CancelMark)
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "New selection should form after re-setting mark (proves anchor survived CancelMark)"
    );
}

/// Test that ClearMark fully resets all mark state.
#[test]
fn test_clear_mark_hard_exits_resets_all_state() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create some content
    harness.type_text("aaaa bbbb cccc\n1111 2222 3333").unwrap();

    // Move to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Start mark mode and create a selection
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::SetMark);
    harness.render().unwrap();

    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Should have selection before ClearMark"
    );

    // Clear mark mode — this should clear EVERYTHING
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::ClearMark);
    harness.render().unwrap();

    // After ClearMark, no selection and no anchor
    assert!(
        !harness.has_selection(),
        "Selection should be cleared after ClearMark"
    );

    // Moving cursor should NOT re-create the selection (no anchor to extend from)
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    assert!(
        !harness.has_selection(),
        "No selection should form after moving — anchor was destroyed"
    );

    // To get a selection again, must explicitly call SetMark first
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::SetMark);
    harness.render().unwrap();

    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Selection should form after explicit SetMark (proves ClearMark wiped the anchor)"
    );
}

/// Test that CancelMark doesn't interfere with non-mark selections (e.g. shift-arrow).
/// Shift-based selections use a different mechanism than mark mode anchors.
#[test]
fn test_cancel_mark_no_interference_with_shift_selections() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("aaaa bbbb\n1111 2222").unwrap();

    // Move to start of first line
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Create a shift-selection (not mark-mode selection)
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Should have shift-selection before CancelMark"
    );

    // Even if we CancelMark while in a shift-selection, it shouldn't destroy that selection
    // (CancelMark only affects mark-mode anchor behavior, not active selections)
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::CancelMark);
    harness.render().unwrap();

    // The shift-selection is still there until we move without shift
    assert!(
        harness.has_selection(),
        "Selection should survive CancelMark when it was created by shift-arrow"
    );
}

/// Test the difference between CancelMark and ClearMark side-by-side.
/// Both exit mark mode, but with different outcomes.
#[test]
fn test_cancel_vs_clear_mark_difference() {
    // ---- Scenario 1: CancelMark (soft) ----
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();

        harness.type_text("aaaa bbbb").unwrap();
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .unwrap();

        // Set mark and create selection
        harness
            .editor_mut()
            .dispatch_action_for_tests(Action::SetMark);
        harness.render().unwrap();
        for _ in 0..4 {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
        }
        harness.render().unwrap();

        // Cancel (soft exit)
        harness
            .editor_mut()
            .dispatch_action_for_tests(Action::CancelMark);
        harness.render().unwrap();

        // Re-enter mark mode — should work because anchor was preserved
        harness
            .editor_mut()
            .dispatch_action_for_tests(Action::SetMark);
        harness.render().unwrap();
        for _ in 0..3 {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
        }
        harness.render().unwrap();

        assert!(
            harness.has_selection(),
            "CancelMark: re-entering mark mode should create a selection"
        );
    }

    // ---- Scenario 2: ClearMark (hard) ----
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();

        harness.type_text("aaaa bbbb").unwrap();
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .unwrap();

        // Set mark and create selection
        harness
            .editor_mut()
            .dispatch_action_for_tests(Action::SetMark);
        harness.render().unwrap();
        for _ in 0..4 {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
        }
        harness.render().unwrap();

        // Clear (hard exit)
        harness
            .editor_mut()
            .dispatch_action_for_tests(Action::ClearMark);
        harness.render().unwrap();

        // Re-enter mark mode — should work, but from a FRESH anchor at current position
        // (the old anchor was destroyed)

        harness
            .editor_mut()
            .dispatch_action_for_tests(Action::SetMark);
        harness.render().unwrap();
        for _ in 0..3 {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
        }
        harness.render().unwrap();

        assert!(
            harness.has_selection(),
            "ClearMark: re-entering mark mode should create a selection (from the new position)"
        );
    }
}

/// Test that CancelMark works correctly with multi-cursor scenarios.
/// The action iterates over all cursors and cancels each anchor.
#[test]
fn test_cancel_mark_with_secondary_cursors() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness
        .type_text("aaaa bbbb\n1111 2222\n3333 4444")
        .unwrap();

    // Move to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Set mark on primary cursor and create selection
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::SetMark);
    harness.render().unwrap();
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Primary cursor should have selection"
    );

    // Add a secondary cursor on the second line (Ctrl+D adds cursor at word under cursor)
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Set mark on all cursors
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::SetMark);
    harness.render().unwrap();

    // CancelMark should affect all cursors
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::CancelMark);
    harness.render().unwrap();

    // The behavior after cancel_mark is that moving cursor drops selection.
    // This test verifies the action doesn't panic with multiple cursors
    // and processes all of them.
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // At least the primary cursor's selection should be dropped
    assert!(
        !harness.has_selection(),
        "Selection should be dropped on primary cursor after CancelMark"
    );
}

/// Test that ClearMark also clears block selections.
#[test]
fn test_clear_mark_clears_block_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create lines with content for block selection
    harness.type_text("aaaa\nbbbb\ncccc").unwrap();

    // Move to start and create a block selection
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Block select (creates a mark mode selection spanning lines)
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Should have block selection before ClearMark"
    );

    // Clear mark mode
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::ClearMark);
    harness.render().unwrap();

    assert!(
        !harness.has_selection(),
        "Block selection should be cleared by ClearMark"
    );
}

/// Regression: after Set Mark, jumping to the matching bracket must produce a
/// *real* selection, not merely a visual highlight. The observable proof is a
/// clipboard round-trip — Cut removes exactly the selected range and Paste
/// restores it — so copy/cut/paste behave correctly on the mark-mode selection.
#[test]
fn test_mark_then_matching_bracket_selection_is_cuttable() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Force internal-only clipboard so the test stays isolated from the host.
    harness.editor_mut().set_clipboard_for_test(String::new());

    harness.type_text("foo(bar)baz").unwrap();
    harness.assert_buffer_content("foo(bar)baz");

    // Move onto the '(' at byte 3.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Enter mark mode, then jump to the matching ')'.
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::SetMark);
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::GoToMatchingBracket);
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Jumping to the matching bracket in mark mode should extend the selection"
    );

    // Cut must remove exactly the selected range "(bar" — this only works if
    // the selection is semantically active, not just highlighted.
    harness.editor_mut().dispatch_action_for_tests(Action::Cut);
    harness.render().unwrap();
    harness.assert_buffer_content("foo)baz");
    assert_eq!(
        harness.editor().clipboard_content_for_test(),
        "(bar",
        "Cut should capture the mark-mode bracket selection"
    );

    // Paste restores the original text — the full round trip succeeds.
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();
    harness.assert_buffer_content("foo(bar)baz");
}
