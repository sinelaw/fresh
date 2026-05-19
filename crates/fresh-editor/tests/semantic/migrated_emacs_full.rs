//! Partial migration of `tests/e2e/emacs_actions.rs` — covers
//! TransposeChars, OpenLine, and SetMark verbs. The
//! Ctrl+T / Ctrl+O / Ctrl+@ keybindings translate to
//! `Action::TransposeChars` / `OpenLine` / `SetMark` (the
//! semantic verbs they bind to).
//!
//! Deferred from this file (tracked in #2058):
//!   - `test_recenter_basic` — needs LayoutScenario to assert
//!     viewport scrolling; Ctrl+L → Action::Recenter exists but
//!     has no buffer-state observable.
//!   - `test_set_mark_then_shift_move`, `test_escape_cancels_
//!     mark_mode`, `test_ctrl_g_cancels_mark_mode` — need a
//!     `deselect_on_move` projection on EditorTestApi to assert
//!     mark-mode cancellation.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

// ─── TransposeChars ────────────────────────────────────────────

#[test]
fn migrated_transpose_chars_basic() {
    // Original: `test_transpose_chars_basic`. Type "abc",
    // MoveLeft to byte 2, TransposeChars → "acb".
    assert_buffer_scenario(BufferScenario {
        description: "TransposeChars at byte 2 of 'abc' yields 'acb'".into(),
        initial_text: "abc".into(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::TransposeChars,
        ],
        expected_text: "acb".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_transpose_chars_at_beginning_is_noop() {
    // Original: `test_transpose_chars_at_beginning`.
    assert_buffer_scenario(BufferScenario {
        description: "TransposeChars at byte 0 leaves buffer unchanged".into(),
        initial_text: "abc".into(),
        actions: vec![Action::MoveLineStart, Action::TransposeChars],
        expected_text: "abc".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_transpose_chars_at_end_is_noop() {
    // Original: `test_transpose_chars_at_end`. Type "ab", cursor
    // at end (byte 2), TransposeChars is a no-op (no char at
    // cursor position). The editor diverges from Emacs C-t-at-EOL
    // (which swaps the previous two chars); pinned as "no-op"
    // because that's the editor's actual behavior and the e2e
    // claim.
    assert_buffer_scenario(BufferScenario {
        description: "TransposeChars at end of 'ab' is a no-op (cursor at EOF)".into(),
        initial_text: "ab".into(),
        actions: vec![Action::MoveDocumentEnd, Action::TransposeChars],
        expected_text: "ab".into(),
        expected_primary: CursorExpect::at(2),
        ..Default::default()
    });
}

// ─── OpenLine ──────────────────────────────────────────────────

#[test]
fn migrated_open_line_basic() {
    // Original: `test_open_line_basic`. Type "hello", MoveLeft
    // ×2 to byte 3, OpenLine → "hel\nlo" with cursor at byte 3
    // (after the fix in commit 90ef113e — was cursor at 4
    // before the fix).
    assert_buffer_scenario(BufferScenario {
        description: "OpenLine at byte 3 of 'hello' produces 'hel\\nlo' with cursor unchanged"
            .into(),
        initial_text: "hello".into(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::OpenLine,
        ],
        expected_text: "hel\nlo".into(),
        // After OpenLine fix: cursor stays at byte 3 (Emacs C-o
        // semantics).
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

#[test]
fn migrated_open_line_at_beginning() {
    // Original: `test_open_line_at_beginning`.
    assert_buffer_scenario(BufferScenario {
        description: "OpenLine at byte 0 of 'hello' produces '\\nhello'".into(),
        initial_text: "hello".into(),
        actions: vec![Action::MoveLineStart, Action::OpenLine],
        expected_text: "\nhello".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

// ─── SetMark ──────────────────────────────────────────────────

#[test]
fn migrated_set_mark_creates_zero_width_selection() {
    // Original: `test_set_mark_basic`. After SetMark at cursor
    // position, the selection is zero-width at that position.
    assert_buffer_scenario(BufferScenario {
        description: "SetMark at byte 0 sets anchor at byte 0 with cursor at byte 0".into(),
        initial_text: "hello".into(),
        actions: vec![Action::SetMark],
        expected_text: "hello".into(),
        // anchor=0, cursor=0 — selection range 0..0.
        expected_primary: CursorExpect::range(0, 0),
        ..Default::default()
    });
}

/// Anti-test: drops `TransposeChars` from
/// `migrated_transpose_chars_basic`. Without it, the buffer
/// stays "abc" and the expected swapped "acb" cannot match.
#[test]
fn anti_emacs_transpose_chars_dropping_action_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: TransposeChars dropped — 'abc' never becomes 'acb'".into(),
        initial_text: "abc".into(),
        actions: vec![Action::MoveDocumentEnd, Action::MoveLeft],
        expected_text: "acb".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without TransposeChars the buffer stays 'abc'; \
         the swapped 'acb' result cannot appear"
    );
}

#[test]
fn migrated_set_mark_then_move_extends_selection() {
    // Original: `test_set_mark_then_regular_move_creates_selection`.
    // After SetMark + MoveRight ×3, selection covers bytes 0..3.
    assert_buffer_scenario(BufferScenario {
        description: "SetMark + 3 MoveRights selects bytes 0..3".into(),
        initial_text: "hello".into(),
        actions: vec![
            Action::SetMark,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
        ],
        expected_text: "hello".into(),
        expected_primary: CursorExpect::range(0, 3),
        expected_selection_text: Some("hel".into()),
        ..Default::default()
    });
}
