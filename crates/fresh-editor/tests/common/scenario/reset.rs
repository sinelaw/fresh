//! Active-reset primitive for combination meta-testing — see
//! `docs/internal/scenario-meta-testing.md`.
//!
//! "Reset" is a sequence of *editor actions* (not a fresh harness)
//! that returns the buffer/cursor/selection to a scenario's initial
//! state: collapse secondary cursors, select-all, replace with the
//! initial text, park the cursor at byte 0 with no selection.
//!
//! It restores only **text + cursor + selection**. The undo/event
//! log, modified flag, config toggles, markers and clipboard are NOT
//! reset — they aren't reachable through buffer-edit actions. That
//! bounds combination to buffer-layer scenarios that don't observe
//! those (the doc spells out the scoping).

use fresh::test_api::Action;

/// Actions that reverse the editor to a clean baseline holding
/// `initial_text`, cursor at byte 0, no selection, single cursor.
pub fn reset_actions(initial_text: &str) -> Vec<Action> {
    let mut actions = vec![Action::RemoveSecondaryCursors, Action::SelectAll];
    if initial_text.is_empty() {
        // Delete the select-all selection to clear the buffer; a no-op
        // when already empty (DeleteBackward at byte 0).
        actions.push(Action::DeleteBackward);
    } else {
        // The first inserted char replaces the select-all selection;
        // the rest append, reconstructing `initial_text` exactly.
        for ch in initial_text.chars() {
            actions.push(match ch {
                '\n' => Action::InsertNewline,
                c => Action::InsertChar(c),
            });
        }
    }
    actions.push(Action::MoveDocumentStart);
    actions
}
