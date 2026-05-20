//! Finding #0 for combination meta-testing — see
//! `docs/internal/scenario-meta-testing.md`.
//!
//! Before stacking scenarios with a reset between them, the reset
//! itself must be sound: applied after *any* edit sequence, the
//! active reset (`reset::reset_actions`) must return the editor to a
//! clean baseline (initial text, cursor at 0, no selection, single
//! cursor). If this fails, reset is incomplete and combination can't
//! be trusted.

use crate::common::scenario::property::evaluate_actions;
use crate::common::scenario::reset::reset_actions;
use fresh::test_api::Action;

fn assert_resets_to_baseline(initial: &str, label: &str, mut actions: Vec<Action>) {
    actions.extend(reset_actions(initial));
    let st = evaluate_actions(initial, &actions);
    assert_eq!(st.buffer_text, initial, "{label}: buffer text not restored");
    assert_eq!(
        st.primary.position, 0,
        "{label}: cursor not parked at byte 0"
    );
    assert_eq!(
        st.primary.anchor, None,
        "{label}: a selection is still active"
    );
    assert_eq!(st.all_carets.len(), 1, "{label}: secondary cursors remain");
    assert_eq!(
        st.selection_text, "",
        "{label}: selection text is non-empty"
    );
}

/// Reset returns to baseline across diverse end-states: trailing
/// edits, multiline edits, an active selection, multiple cursors, an
/// empty initial buffer, and a multibyte buffer.
#[test]
fn reset_restores_baseline_after_diverse_edits() {
    use Action::*;
    assert_resets_to_baseline(
        "hello world",
        "insert+move",
        vec![MoveDocumentEnd, InsertChar('!'), MoveLineStart],
    );
    assert_resets_to_baseline(
        "a\nb\nc",
        "multiline edit",
        vec![MoveDocumentEnd, InsertNewline, InsertChar('d')],
    );
    assert_resets_to_baseline("hello", "active selection", vec![SelectAll]);
    assert_resets_to_baseline(
        "aaa\nbbb\nccc",
        "multi-cursor insert",
        vec![AddCursorBelow, AddCursorBelow, InsertChar('x')],
    );
    assert_resets_to_baseline(
        "",
        "empty buffer typed into",
        vec![InsertChar('z'), InsertChar('y')],
    );
    assert_resets_to_baseline(
        "你好世界",
        "multibyte selection",
        vec![MoveDocumentEnd, SelectLeft, SelectLeft],
    );
}
