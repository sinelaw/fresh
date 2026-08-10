//! Coverage for the workspace that exists before its contents do.
//!
//! Creating an Orchestrator workspace runs `git worktree add`, which on a
//! large repo or a slow disk takes long enough that the user does something
//! else in the meantime. The old flow left them on the *previous* workspace
//! for the whole wait and then yanked focus into the new one the moment the
//! worktree landed — and for that whole wait the dock row was a stub that
//! could only be dismissed, not renamed, filed, or entered.
//!
//! The fix is to open the workspace immediately as a real `Window` showing a
//! "still being built" page (`Editor::preparing_windows`), then grow that
//! window into the live session via `create_window_with_terminal`'s adopt
//! path. These tests pin the two properties that make the model work:
//!
//! 1. The placeholder is a first-class window while it builds.
//! 2. Adopting it keeps its identity — same `WindowId`, same durable
//!    `stable_id` — so a rename or a dock folder filed against it during the
//!    wait still points at the workspace afterwards.

#![cfg(feature = "plugins")]

use crate::common::harness::EditorTestHarness;
use portable_pty::{native_pty_system, PtySize};

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

/// A workspace whose contents are still being built renders its progress
/// instead of the empty scratch buffer it technically holds — the same page
/// a not-yet-connected remote session shows.
#[test]
fn preparing_window_paints_its_progress_instead_of_an_empty_buffer() {
    fresh::i18n::set_locale("en");
    let mut harness = EditorTestHarness::with_temp_project(160, 50).unwrap();
    harness.tick_and_render().unwrap();
    let project_root = harness.project_dir().unwrap().canonicalize().unwrap();

    let id = harness.editor_mut().open_preparing_window(
        project_root,
        "wip-workspace".into(),
        "Adding worktree…".into(),
    );
    harness.editor_mut().set_active_window(id);
    harness.tick_and_render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("wip-workspace"),
        "the placeholder page names the workspace being built; screen was:\n{screen}"
    );
    assert!(
        screen.contains("Adding worktree"),
        "the placeholder page carries the build's progress line; screen was:\n{screen}"
    );
    assert!(
        !screen.contains("[No Name]"),
        "a workspace that isn't ready must not pretend to be an empty editor; \
         screen was:\n{screen}"
    );
}

/// Adopting the placeholder must not mint a new workspace: the window id and
/// the durable `stable_id` both carry over, because the Orchestrator keys a
/// mid-build rename and folder assignment off exactly those.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell as the agent command.
fn adopting_a_preparing_window_keeps_its_ids() {
    if !pty_available() {
        eprintln!("Skipping preparing-window adopt test: PTY not available");
        return;
    }
    fresh::i18n::set_locale("en");
    let mut harness = EditorTestHarness::with_temp_project(160, 50).unwrap();
    harness.tick_and_render().unwrap();
    let project_root = harness.project_dir().unwrap().canonicalize().unwrap();

    let placeholder = harness.editor_mut().open_preparing_window(
        project_root.clone(),
        "wip-workspace".into(),
        "Creating workspace…".into(),
    );
    let stable_id = harness
        .editor()
        .session(placeholder)
        .expect("placeholder window present")
        .stable_id
        .clone();
    assert!(
        !stable_id.is_empty(),
        "a placeholder is minted with a durable id"
    );

    let authority = harness.editor().local_session_authority(&project_root);
    let (adopted, _terminal, _buffer) = harness
        .editor_mut()
        .create_window_with_terminal(
            project_root.clone(),
            "wip-workspace".into(),
            Some(project_root),
            Some(vec!["sh".into(), "-c".into(), "sleep 60".into()]),
            Some("agent".into()),
            authority,
            None,
            None,
            false,
            Some(placeholder),
        )
        .expect("adopting the placeholder should succeed");
    harness.tick_and_render().unwrap();

    assert_eq!(
        adopted, placeholder,
        "the live session takes over the placeholder's window id"
    );
    assert_eq!(
        harness
            .editor()
            .session(adopted)
            .expect("adopted window present")
            .stable_id,
        stable_id,
        "the durable workspace id survives the adopt — a rename or folder filed \
         against it while the workspace was building must still point at it"
    );
    assert!(
        !harness.editor().is_window_preparing(adopted),
        "a workspace with its terminal seeded is no longer 'being created'"
    );

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Creating workspace"),
        "the placeholder page is gone once the workspace is live; screen was:\n{screen}"
    );
}
