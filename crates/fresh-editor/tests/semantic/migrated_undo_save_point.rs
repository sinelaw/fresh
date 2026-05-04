//! Migrations of `tests/e2e/undo_redo.rs` save-point claims
//! (issue #191): the buffer's `is_modified` flag must flip back to
//! false the moment Undo brings the buffer text back to a saved
//! state, without requiring an "extra" Undo step.
//!
//! These tests assert `is_modified` at multiple points along an
//! action sequence, so they go through `EditorTestHarness` +
//! `EditorTestApi` directly rather than the single-shot
//! `assert_buffer_scenario` runner. Same dispatch path as the
//! production keymap; no mocks.

use crate::common::harness::EditorTestHarness;
use fresh::test_api::Action;

#[test]
fn migrated_undo_to_save_point_marks_buffer_unmodified() {
    // Original: `test_undo_to_save_point_marks_buffer_unmodified`
    // (issue #191). Loaded-from-disk buffer is unmodified;
    // typing makes it modified; Undo of the lone insertion
    // restores the on-disk content and the flag flips back to
    // false in one step.
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let _fixture = harness
        .load_buffer_from_text_named("test_undo_save.txt", "initial")
        .unwrap();

    assert!(
        !harness.api_mut().is_modified(),
        "Freshly loaded buffer must not be marked modified"
    );

    harness.api_mut().dispatch(Action::MoveLineEnd);
    harness.api_mut().dispatch(Action::InsertChar('X'));
    assert_eq!(harness.api_mut().buffer_text(), "initialX");
    assert!(
        harness.api_mut().is_modified(),
        "Buffer must be modified after typing"
    );

    harness.api_mut().dispatch(Action::Undo);
    assert_eq!(harness.api_mut().buffer_text(), "initial");
    assert!(
        !harness.api_mut().is_modified(),
        "Issue #191: undoing to the saved state must clear is_modified \
         in a single Undo step (no extra step needed)"
    );

    // Cursor must land within the text bounds — the issue
    // specifically called out cursor going to position 0
    // (top-of-screen) on undo. `MoveLineEnd` + `InsertChar('X')`
    // before undo had cursor at byte 8; undo restores the cursor
    // to byte 7 (end of "initial") since that's the position
    // before the insertion.
    let pos = harness.api_mut().primary_caret().position;
    assert!(
        pos <= 7,
        "Cursor must stay within 'initial' bounds after Undo; got byte {pos}"
    );
}

#[test]
fn migrated_undo_past_save_point_keeps_buffer_modified() {
    // Original: `test_undo_past_save_point`. Type, Save (creates
    // save point), type more, Undo past the save point ⇒ buffer
    // is again *modified* because content no longer matches
    // disk.
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let _fixture = harness
        .load_buffer_from_text_named("test.txt", "")
        .unwrap();

    // Type "hello", save → save point at "hello".
    for c in "hello".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    harness.api_mut().dispatch(Action::Save);
    assert!(
        !harness.api_mut().is_modified(),
        "Buffer must be unmodified immediately after Save"
    );

    // Type " world" — beyond the save point.
    for c in " world".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello world");
    assert!(harness.api_mut().is_modified());

    // Undo all 6 post-save inserts ⇒ back at the save point.
    for _ in 0.." world".len() {
        harness.api_mut().dispatch(Action::Undo);
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello");
    assert!(
        !harness.api_mut().is_modified(),
        "Undo back to the save-point must clear is_modified"
    );

    // One more Undo crosses the save point — the buffer is
    // *unsaved* against the on-disk "hello" content.
    harness.api_mut().dispatch(Action::Undo);
    assert_ne!(
        harness.api_mut().buffer_text(),
        "hello",
        "Undoing past the save point must change the buffer text"
    );
    assert!(
        harness.api_mut().is_modified(),
        "Crossing back over the save point must re-flag the buffer as modified"
    );
}
