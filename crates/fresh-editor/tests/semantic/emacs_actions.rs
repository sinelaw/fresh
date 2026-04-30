//! Track B migration: rewrites of pure-state tests from
//! `tests/e2e/emacs_actions.rs` as declarative theorems.
//!
//! The original tests configure an Emacs keybinding map and drive the
//! editor through `Ctrl-T` / `Ctrl-O` / `Ctrl-Space`. The semantic
//! version dispatches `Action::TransposeChars` / `Action::OpenLine` /
//! `Action::SetMark` directly, so it tests the *action* (the Emacs
//! semantics) without depending on the keybinding-map plumbing.
//!
//! Skipped:
//!   * `test_recenter_basic` — viewport-dependent, would need a
//!     LayoutTheorem with explicit dimensions and scrolling.
//!   * `test_escape_cancels_mark_mode` / `test_ctrl_g_cancels_mark_mode`
//!     — these test the *keybinding* (Esc / Ctrl-G → cancel-mark-mode)
//!     plus the internal `deselect_on_move` flag, which isn't part of
//!     the public `Caret` projection.

use crate::common::theorem::buffer_theorem::{assert_buffer_theorem, BufferTheorem, CursorExpect};
use fresh::test_api::Action;

// ─────────────────────────────────────────────────────────────────────────
// TransposeChars (Emacs C-t)
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_transpose_chars_swaps_two_characters() {
    // Replaces tests/e2e/emacs_actions.rs::test_transpose_chars_basic.
    // With cursor between 'b' and 'c' in "abc", TransposeChars swaps
    // the chars on either side of the cursor.
    assert_buffer_theorem(BufferTheorem {
        description: "TransposeChars swaps the chars on either side of the cursor",
        initial_text: "abc",
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::TransposeChars,
        ],
        expected_text: "acb",
        expected_primary: CursorExpect::at(3),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_transpose_chars_at_beginning_is_noop() {
    // Replaces test_transpose_chars_at_beginning.
    // At position 0 there is no char to the left, so TransposeChars
    // is a no-op (text unchanged, cursor unchanged).
    assert_buffer_theorem(BufferTheorem {
        description: "TransposeChars at beginning of buffer is a no-op",
        initial_text: "abc",
        actions: vec![Action::MoveDocumentStart, Action::TransposeChars],
        expected_text: "abc",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_transpose_chars_at_end_is_noop() {
    // Replaces test_transpose_chars_at_end.
    // With the cursor at end-of-buffer, there is no char *at* the
    // cursor to swap with the previous one; TransposeChars is a no-op.
    assert_buffer_theorem(BufferTheorem {
        description: "TransposeChars at end of buffer is a no-op",
        initial_text: "ab",
        actions: vec![Action::MoveDocumentEnd, Action::TransposeChars],
        expected_text: "ab",
        expected_primary: CursorExpect::at(2),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

// ─────────────────────────────────────────────────────────────────────────
// OpenLine (Emacs C-o)
// ─────────────────────────────────────────────────────────────────────────

// FINDING: `Action::OpenLine`'s source comment in
// `src/input/actions.rs:2885-2887` says "Insert a newline at cursor
// position but don't move cursor". The implementation just emits an
// `Event::Insert("\n")`, and `apply_insert` advances the cursor by
// `text.len()` — so OpenLine *does* advance the cursor past the
// inserted newline. The original e2e tests `test_open_line_basic` /
// `test_open_line_at_beginning` were silent about cursor position
// (they captured `cursor_before` and never compared it), so this
// divergence between intent and behavior was invisible.
//
// The theorems below pin the *actual* behavior. The intent is
// captured separately as `#[ignore]`d theorems below to make the gap
// visible. Removing the `#[ignore]` once OpenLine is fixed will
// upgrade those theorems to permanent regression coverage.

#[test]
fn theorem_open_line_inserts_newline_actual_behavior() {
    // Pins the *actual* current behavior (cursor advances past the
    // inserted newline, like Enter). See the FINDING block above.
    assert_buffer_theorem(BufferTheorem {
        description: "OpenLine inserts a newline (cursor advances past it — actual behavior)",
        initial_text: "hello",
        // Move to position 3 ("hel|lo") then OpenLine.
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::OpenLine,
        ],
        expected_text: "hel\nlo",
        expected_primary: CursorExpect::at(4),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_open_line_at_beginning_actual_behavior() {
    // Pins the *actual* behavior at position 0.
    assert_buffer_theorem(BufferTheorem {
        description: "OpenLine at beginning inserts a leading newline (cursor advances)",
        initial_text: "hello",
        actions: vec![Action::MoveDocumentStart, Action::OpenLine],
        expected_text: "\nhello",
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
#[ignore = "BUG: actions.rs:2885 — OpenLine advances cursor; comment says it shouldn't (Emacs C-o intent)"]
fn theorem_open_line_intended_behavior_cursor_stays_put() {
    // The intended behavior per the source comment and Emacs C-o
    // semantics: cursor stays at the insertion point so subsequent
    // typing appears on the original (upper) line.
    assert_buffer_theorem(BufferTheorem {
        description: "OpenLine should leave cursor at insertion point (intended)",
        initial_text: "hello",
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::OpenLine,
        ],
        expected_text: "hel\nlo",
        expected_primary: CursorExpect::at(3),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

// ─────────────────────────────────────────────────────────────────────────
// SetMark (Emacs C-Space) — mark-mode anchor behavior
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_set_mark_creates_anchor_at_cursor_position() {
    // Replaces test_set_mark_basic.
    // After SetMark at position 0, the cursor has an anchor at 0.
    // No characters are between cursor and anchor, so the selection
    // text is empty.
    assert_buffer_theorem(BufferTheorem {
        description: "SetMark sets the anchor at the cursor position",
        initial_text: "hello world",
        actions: vec![Action::MoveDocumentStart, Action::SetMark],
        expected_text: "hello world",
        expected_primary: CursorExpect::range(0, 0),
        expected_extra_cursors: vec![],
        expected_selection_text: Some(""),
    });
}

#[test]
fn theorem_set_mark_then_move_extends_selection() {
    // Replaces test_set_mark_then_regular_move_creates_selection.
    // The defining property of Emacs mark-mode: after SetMark, plain
    // (non-shift) movements extend the selection rather than clearing
    // the anchor. Selecting "hello" via MoveRight x5.
    assert_buffer_theorem(BufferTheorem {
        description: "SetMark + MoveRight extends the selection (mark mode)",
        initial_text: "hello world",
        actions: vec![
            Action::MoveDocumentStart,
            Action::SetMark,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
        ],
        expected_text: "hello world",
        expected_primary: CursorExpect::range(0, 5),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("hello"),
    });
}

#[test]
fn theorem_set_mark_then_shift_move_extends_selection() {
    // Replaces test_set_mark_then_shift_move_creates_selection.
    // Even with shift movements, the anchor set by SetMark is the one
    // that remains; selection still spans 0..5.
    assert_buffer_theorem(BufferTheorem {
        description: "SetMark + SelectRight extends the selection",
        initial_text: "hello world",
        actions: vec![
            Action::MoveDocumentStart,
            Action::SetMark,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
        ],
        expected_text: "hello world",
        expected_primary: CursorExpect::range(0, 5),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("hello"),
    });
}
