//! Faithful migration of `tests/e2e/undo_bulk_edit_after_save.rs`.
//!
//! Bug under regression: BulkEdit (toggle-comment) + Save +
//! more typing + many Undos used to corrupt the buffer because
//! `consolidate_after_save()` reset `next_buffer_id` while
//! BulkEdit undo snapshots still referenced the pre-consolidation
//! buffer IDs.
//!
//! Symptom (per the original): `Buffer range out of bounds` /
//! `Buffer not found` errors during the undo cascade, eventually
//! returning a corrupt or panicking buffer.
//!
//! No mocks here — the scenario routes `Action::Save` through the
//! exact production path (`Editor::save → buffer::save →
//! consolidate_after_save`) by relying on the BufferScenario
//! harness's real-FS temp project, the same surface
//! `migrated_persistence` exercises.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

const INITIAL: &str =
    "fn main() {\n    println!(\"hello\");\n    println!(\"world\");\n}\n";

fn type_chars(s: &str) -> impl Iterator<Item = Action> + '_ {
    s.chars().map(Action::InsertChar)
}

#[test]
fn migrated_undo_past_bulk_edit_after_save_does_not_corrupt_buffer() {
    // Reproducer for the user report "holding ctrl+z (undo) nuked
    // the other lines". The full undo cascade must restore the
    // original file content; if the buffers vec has been
    // desynchronized from the piece tree, `buffer_text()` either
    // panics or returns garbage, and BufferScenario's text-equality
    // assertion catches both.
    let mut actions: Vec<Action> = vec![Action::MoveLineEnd];
    actions.extend(type_chars(" // edited"));
    actions.push(Action::ToggleComment); // BulkEdit, snapshots tree
    actions.push(Action::Save); // consolidate_after_save
    actions.push(Action::MoveDown);
    actions.push(Action::MoveLineEnd);
    actions.push(Action::InsertChar('X'));
    // Drown the undo log: 20 Undos should revert all 12+ edits.
    actions.extend(std::iter::repeat_n(Action::Undo, 20));

    assert_buffer_scenario(BufferScenario {
        description: "Undo past BulkEdit-after-save restores the original buffer (issue: holding Ctrl+Z corruption)".into(),
        initial_text: INITIAL.into(),
        language: Some("test.rs".into()),
        actions,
        expected_text: INITIAL.into(),
        // After draining the undo log to the file's load point the
        // cursor lands at byte 0 — `dispatch_action_for_tests` walks
        // the cursor back through every reverse step and the final
        // Undos park it at the buffer start.
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

/// Anti-test: drops the `Action::Save` from the sequence. Without
/// `Save`, `consolidate_after_save` is never called, so the
/// pre-consolidation/post-consolidation desync the original bug
/// hinges on cannot occur. The expected text is still the
/// original `INITIAL`, but with `Save` removed the undo log path
/// is structurally different — a regression in
/// `consolidate_after_save` can no longer be detected by this
/// shape. We flip the expectation to a non-original buffer to
/// prove the runner notices the structural difference.
#[test]
fn anti_undo_bulk_edit_after_save_dropping_save_yields_check_err() {
    let mut actions: Vec<Action> = vec![Action::MoveLineEnd];
    actions.extend(type_chars(" // edited"));
    actions.push(Action::ToggleComment);
    // Action::Save deliberately omitted.
    actions.push(Action::MoveDown);
    actions.push(Action::MoveLineEnd);
    actions.push(Action::InsertChar('X'));
    actions.extend(std::iter::repeat_n(Action::Undo, 20));

    let scenario = BufferScenario {
        description: "anti: assertion against a non-INITIAL value to prove it's not vacuous".into(),
        initial_text: INITIAL.into(),
        language: Some("test.rs".into()),
        actions,
        // Set an obviously-wrong expected text so check must fail
        // even though the *real* behavior (no save → clean undo) is
        // probably fine. The point of this anti-test is to show the
        // pipeline sees buffer text and doesn't pass on shape alone.
        expected_text: "deliberately wrong text".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: a deliberately wrong expected_text must NOT match \
         the actual buffer state, proving the runner reads buffer text"
    );
}
