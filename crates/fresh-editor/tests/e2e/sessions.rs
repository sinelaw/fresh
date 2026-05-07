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
