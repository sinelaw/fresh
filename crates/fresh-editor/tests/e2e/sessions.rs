//! Wiring tests for the editor `Session` abstraction.
//!
//! Step 1 of the Conductor migration adds a single forced session to
//! `Editor` without changing any user-visible behaviour. These tests
//! pin the boot invariants so subsequent migration steps don't break
//! them silently:
//!
//! - The editor boots with exactly one session.
//! - That session's id is `SessionId(1)` (the "base").
//! - Its `root` matches `editor.working_dir()` — call sites can swap
//!   from `working_dir()` to `active_session().root` and read the
//!   same value.
//! - Its `label` is non-empty.
//!
//! These are model invariants, not user-visible behaviour, so they
//! observe via `harness.editor()` rather than the rendered screen.
//! That follows the existing pattern in `workspace.rs` for
//! invariants that have no screen surface yet.

use crate::common::harness::EditorTestHarness;
use fresh_core::SessionId;

#[test]
fn editor_boots_with_one_base_session() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    let editor = harness.editor();
    assert_eq!(editor.session_count(), 1, "expected exactly one session");
    assert_eq!(editor.active_session_id(), SessionId(1));
}

#[test]
fn active_session_root_matches_working_dir() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    let editor = harness.editor();
    let session = editor.active_session();
    assert_eq!(
        session.root,
        editor.working_dir(),
        "session root must mirror working_dir for the migration to be \
         a behaviour-preserving refactor"
    );
}

#[test]
fn active_session_has_non_empty_label() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    let session = harness.editor().active_session();
    assert!(
        !session.label.is_empty(),
        "session label fell through to empty string; \
         basename fallback in Session::new is broken"
    );
}

// ---------------------------------------------------------------------------
// Step 2: createSession / setActiveSession / closeSession lifecycle.
//
// These tests drive the editor methods directly rather than going through
// the JS plugin runtime. The plugin command dispatch is identical to
// existing fire-and-forget commands (registerCommand, focusSplit) — wiring
// is trusted and exercised by other e2e tests; here we focus on the
// session-state semantics.
// ---------------------------------------------------------------------------

use std::path::PathBuf;

#[test]
fn create_session_inserts_with_monotonic_id() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let editor = harness.editor_mut();

    let id_a = editor.create_session_at(PathBuf::from("/tmp/wt-a"), "alpha".into());
    let id_b = editor.create_session_at(PathBuf::from("/tmp/wt-b"), "beta".into());

    assert_eq!(
        id_a,
        SessionId(2),
        "first new session should take id 2 (after base)"
    );
    assert_eq!(id_b, SessionId(3), "ids must be monotonic");
    assert_eq!(editor.session_count(), 3);

    // Active session is unchanged by createSession alone.
    assert_eq!(editor.active_session_id(), SessionId(1));
}

#[test]
fn create_session_falls_back_to_basename_label() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let id = harness
        .editor_mut()
        .create_session_at(PathBuf::from("/tmp/feat-auth"), String::new());

    let session = harness.editor().session(id).unwrap();
    assert_eq!(
        session.label, "feat-auth",
        "empty label must fall back to the root basename"
    );
}

#[test]
fn set_active_session_swaps_pointer_and_working_dir() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let new_root = PathBuf::from("/tmp/wt-feat-auth");
    let new_id = harness
        .editor_mut()
        .create_session_at(new_root.clone(), "feat-auth".into());

    harness.editor_mut().set_active_session(new_id);

    assert_eq!(harness.editor().active_session_id(), new_id);
    assert_eq!(
        harness.editor().active_session().root,
        new_root,
        "active session's root must be the new path"
    );
    assert_eq!(
        harness.editor().working_dir(),
        new_root,
        "working_dir must follow active_session().root for the migration \
         to be a behaviour-preserving refactor"
    );
}

#[test]
fn set_active_session_unknown_id_is_noop() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let original_root = harness.editor().working_dir().to_path_buf();
    let original_active = harness.editor().active_session_id();

    harness.editor_mut().set_active_session(SessionId(99));

    assert_eq!(harness.editor().active_session_id(), original_active);
    assert_eq!(harness.editor().working_dir(), original_root);
}

#[test]
fn close_session_drops_inactive_session() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let id = harness
        .editor_mut()
        .create_session_at(PathBuf::from("/tmp/wt-feat"), "feat".into());

    let removed = harness.editor_mut().close_session(id);

    assert!(
        removed,
        "close_session should succeed for an inactive session"
    );
    assert_eq!(harness.editor().session_count(), 1);
    assert_eq!(harness.editor().active_session_id(), SessionId(1));
}

#[test]
fn close_session_refuses_active_session() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let id = harness
        .editor_mut()
        .create_session_at(PathBuf::from("/tmp/wt-feat"), "feat".into());
    harness.editor_mut().set_active_session(id);

    let removed = harness.editor_mut().close_session(id);

    assert!(!removed, "close_session must refuse the active session");
    assert_eq!(harness.editor().session_count(), 2);
}

#[test]
fn close_session_refuses_base_session() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    // Add an inactive session and switch to it so the base is no
    // longer active — only the "is base" rule should keep it
    // alive.
    let id = harness
        .editor_mut()
        .create_session_at(PathBuf::from("/tmp/wt-feat"), "feat".into());
    harness.editor_mut().set_active_session(id);

    let removed = harness.editor_mut().close_session(SessionId(1));

    assert!(
        !removed,
        "close_session must refuse the base session even when not active"
    );
    assert_eq!(harness.editor().session_count(), 2);
}
