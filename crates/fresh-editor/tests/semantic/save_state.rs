//! Track B migration: the save-point / dirty-state subset of
//! `tests/e2e/undo_redo.rs`.
//!
//! These tests exercise the *transition* of `is_modified()` across
//! a chain of edits and undos — the property under test is "undoing
//! back to the save point flips the buffer back to clean", and the
//! 2024 regression at issue #191 was an *extra* undo step needed
//! to reach unmodified state.
//!
//! The shape doesn't fit `BufferTheorem` cleanly (it's a state
//! trajectory, not just a final state), so the runner is open-coded
//! against the test-API's `is_modified()` observable. Still
//! observes only `fresh::test_api`; isolation lint passes.

use crate::common::harness::EditorTestHarness;
use fresh::test_api::Action;

/// Replaces `tests/e2e/undo_redo.rs::test_undo_to_save_point_marks_buffer_unmodified`
/// (issue #191).
///
/// 1. Load a file with content "initial" (clean).
/// 2. Type one character → buffer is dirty.
/// 3. Undo → buffer is clean *and* matches the saved content.
///
/// The bug-fixed behavior: a single Undo must restore both the text
/// AND the clean flag. Before the fix, an extra Undo was needed.
#[test]
fn theorem_undo_back_to_save_point_marks_buffer_clean() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text("initial")
        .expect("load_buffer_from_text failed");
    let api = harness.api_mut();

    assert_eq!(
        api.buffer_text(),
        "initial",
        "freshly loaded buffer should match the file"
    );
    assert!(
        !api.is_modified(),
        "freshly loaded buffer should be clean (is_modified=false)"
    );

    api.dispatch_seq(&[Action::MoveDocumentEnd, Action::InsertChar('X')]);
    assert_eq!(api.buffer_text(), "initialX");
    assert!(
        api.is_modified(),
        "after typing one char, buffer should be dirty"
    );

    api.dispatch(Action::Undo);
    assert_eq!(api.buffer_text(), "initial");
    assert!(
        !api.is_modified(),
        "after undoing back to the save point, buffer should be clean (issue #191)"
    );
}

/// Replaces `tests/e2e/undo_redo.rs::test_undo_past_save_point`.
///
/// Once we undo *past* the save point — into pre-save history — the
/// buffer should *stay* dirty even though the text might happen to
/// match a prior version of the file.
#[test]
fn theorem_undoing_past_save_point_keeps_buffer_dirty() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text("a")
        .expect("load_buffer_from_text failed");
    let api = harness.api_mut();

    // Two edits past the save point.
    api.dispatch_seq(&[
        Action::MoveDocumentEnd,
        Action::InsertChar('b'),
        Action::InsertChar('c'),
    ]);
    assert_eq!(api.buffer_text(), "abc");
    assert!(api.is_modified());

    // Undo once — back one edit, still dirty.
    api.dispatch(Action::Undo);
    assert_eq!(api.buffer_text(), "ab");
    assert!(
        api.is_modified(),
        "1 undo from 'abc' to 'ab' is still dirty"
    );

    // Undo again — back to "a", which is the save point: clean.
    api.dispatch(Action::Undo);
    assert_eq!(api.buffer_text(), "a");
    assert!(
        !api.is_modified(),
        "undoing exactly back to the save point should mark the buffer clean"
    );
}

/// The "redo into a dirty state" companion. Going Undo → Undo lands
/// us at the save point (clean); Redo immediately should mark dirty
/// again because we're now ahead of the save point.
#[test]
fn theorem_redo_past_save_point_marks_buffer_dirty() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text("a")
        .expect("load_buffer_from_text failed");
    let api = harness.api_mut();

    api.dispatch_seq(&[
        Action::MoveDocumentEnd,
        Action::InsertChar('b'),
        Action::Undo,
    ]);
    assert_eq!(api.buffer_text(), "a");
    assert!(
        !api.is_modified(),
        "undo back to save point should be clean"
    );

    api.dispatch(Action::Redo);
    assert_eq!(api.buffer_text(), "ab");
    assert!(
        api.is_modified(),
        "redoing past the save point should mark the buffer dirty"
    );
}

/// Buffer freshly loaded from disk with content matching the file is
/// clean. Sanity check that the harness `is_modified()` observable
/// is wired through correctly — without this, the other theorems
/// could pass for the wrong reason.
#[test]
fn theorem_freshly_loaded_buffer_is_clean() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text("hello")
        .expect("load_buffer_from_text failed");
    let api = harness.api_mut();

    assert_eq!(api.buffer_text(), "hello");
    assert!(!api.is_modified());
}
