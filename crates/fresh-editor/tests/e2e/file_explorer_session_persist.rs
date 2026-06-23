//! The file explorer's open/closed state must survive a relaunch.
//!
//! Regression: opening a bare directory (`fresh <dir>`) showed the file
//! explorer by default, but the launch path then re-`show`ed it
//! unconditionally *after* restore — so a deliberately-closed explorer
//! sprang back open on every relaunch. The launch/enter flow now defers
//! to the workspace's persisted `file_explorer_visible`, defaulting to
//! the tree only for a brand-new directory (see `Editor::restore_window`
//! / `Window::apply_fresh_session_explorer_default`).
//!
//! These drive the real launch entrypoint (`restore_active_window_on_launch`)
//! and assert on rendered output.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use std::fs;
use tempfile::TempDir;

fn launch_harness(
    project_dir: &std::path::Path,
    dir_context: &DirectoryContext,
) -> EditorTestHarness {
    EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_working_dir(project_dir.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap()
}

/// Closed explorer stays closed across a relaunch of the same directory.
#[test]
fn explorer_closed_state_survives_relaunch() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    fs::create_dir(&project_dir).unwrap();
    fs::write(project_dir.join("a.txt"), "hello").unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: bare-directory launch shows the explorer by default;
    // the user closes it (Ctrl+B), then we shut down (saving the
    // workspace with the explorer closed).
    {
        let mut harness = launch_harness(&project_dir, &dir_context);
        let restored = harness
            .editor_mut()
            .restore_active_window_on_launch(false)
            .unwrap();
        assert!(!restored, "no workspace exists yet on the first launch");
        harness
            .wait_until(|h| h.screen_to_string().contains("File Explorer"))
            .unwrap();

        // Ctrl+B toggles the sidebar/file-explorer visibility off.
        harness
            .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
            .unwrap();
        harness
            .wait_until(|h| !h.screen_to_string().contains("File Explorer"))
            .unwrap();

        harness.shutdown(true).unwrap();
    }

    // Session 2: relaunching the same directory must NOT re-open the
    // explorer — the persisted closed state wins.
    {
        let mut harness = launch_harness(&project_dir, &dir_context);
        let restored = harness
            .editor_mut()
            .restore_active_window_on_launch(false)
            .unwrap();
        assert!(restored, "the saved workspace should restore");
        harness.render().unwrap();
        assert!(
            !harness.screen_to_string().contains("File Explorer"),
            "explorer must stay closed after being closed in the previous session"
        );

        // Prove the closed state deterministically (the panel paints its
        // tree asynchronously, so a single absent frame alone is weak):
        // from closed, Ctrl+B opens the explorer. If it had wrongly been
        // re-opened, the same key would close it and the panel would
        // never appear — hanging this wait into a timeout failure.
        harness
            .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
            .unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains("File Explorer"))
            .unwrap();
    }
}

// The orchestrator dock switch (`set_active_window` → `materialize_window`)
// funnels through the same `Editor::restore_window` these tests exercise,
// so a directory whose session was active in a previous execution restores
// its persisted explorer state identically. That path is verified manually
// (background-session discovery isn't wired up in the vanilla test harness).

/// Inverse control: an explorer left open is restored open, so the fix
/// is not just "always hide".
#[test]
fn explorer_open_state_survives_relaunch() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    fs::create_dir(&project_dir).unwrap();
    fs::write(project_dir.join("a.txt"), "hello").unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: leave the default-opened explorer open, then shut down.
    {
        let mut harness = launch_harness(&project_dir, &dir_context);
        harness
            .editor_mut()
            .restore_active_window_on_launch(false)
            .unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains("File Explorer"))
            .unwrap();
        harness.shutdown(true).unwrap();
    }

    // Session 2: the explorer should still be open.
    {
        let mut harness = launch_harness(&project_dir, &dir_context);
        let restored = harness
            .editor_mut()
            .restore_active_window_on_launch(false)
            .unwrap();
        assert!(restored, "the saved workspace should restore");
        // The explorer initialises its tree asynchronously; wait for the
        // restored-open panel to paint rather than asserting on one frame.
        harness
            .wait_until(|h| h.screen_to_string().contains("File Explorer"))
            .unwrap();
    }
}
