//! Regression coverage for issue #2735.
//!
//! `--no-restore` (alias `--no-session`) gates the quit-time
//! `save_all_windows_workspaces`, but the mid-session crash-safety
//! checkpoint (`checkpoint_window_workspace`, fired when Extract Tab to
//! New Workspace switches away from the source window) did NOT honor the
//! flag. The result was an asymmetric on-disk state: the *source*
//! workspace was persisted while the freshly-extracted co-tenant was not,
//! so the next launch restored the source and silently dropped the
//! extracted workspace and its tab.
//!
//! The fix gates ALL workspace writes centrally in `save_workspace_for`
//! on the session's workspace-persistence flag: a `--no-restore` session
//! never writes workspace files, checkpoint or quit alike.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::workspace::find_workspace_file_by_root;
use std::fs;

/// Open the command palette, type the query, accept the first suggestion
/// via Tab, execute with Enter (same helper as `extract_tab_to_workspace`).
fn run_command_palette(harness: &mut EditorTestHarness, query: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(query).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Project with two files opened as tabs, ready for an extraction. Wide so
/// the extraction status line isn't clipped.
fn harness_with_two_tabs() -> EditorTestHarness {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(220, 30, Default::default()).unwrap();
    let root = harness.project_dir().unwrap();
    fs::write(root.join("keep.txt"), "keep\n").unwrap();
    fs::write(root.join("notes.txt"), "notes\n").unwrap();
    harness.open_file(&root.join("keep.txt")).unwrap();
    harness.open_file(&root.join("notes.txt")).unwrap();
    harness.render().unwrap();
    harness
}

#[test]
fn test_no_restore_session_writes_no_workspace_files_on_extract() {
    let mut harness = harness_with_two_tabs();
    let root = harness.project_dir().unwrap();

    // Simulate `--no-restore`: the session must never persist workspaces.
    harness.editor_mut().set_workspace_persistence(false);

    // Extract the focused tab into a new co-tenant workspace. This
    // switches windows, which fires the workspace checkpoint for the
    // source window — the write that used to ignore `--no-restore`.
    run_command_palette(&mut harness, "Extract Tab to New Workspace");
    harness.assert_screen_contains("Extracted notes.txt into workspace");

    assert_eq!(
        find_workspace_file_by_root(&root).unwrap(),
        None,
        "the extraction checkpoint must not write a workspace file in a --no-restore session"
    );

    // The quit-time save path funnels through the same gate.
    harness.editor_mut().save_all_windows_workspaces().unwrap();
    assert_eq!(
        find_workspace_file_by_root(&root).unwrap(),
        None,
        "quit-time save must not write workspace files in a --no-restore session"
    );
}

/// Control: with persistence enabled (the default), the same extraction
/// flow persists the project's workspaces — the gate must not suppress
/// normal sessions.
#[test]
fn test_default_session_still_persists_workspaces_on_extract() {
    let mut harness = harness_with_two_tabs();
    let root = harness.project_dir().unwrap();

    run_command_palette(&mut harness, "Extract Tab to New Workspace");
    harness.assert_screen_contains("Extracted notes.txt into workspace");

    // The window-switch checkpoint has already written the source
    // workspace; the quit-time save persists the rest.
    harness.editor_mut().save_all_windows_workspaces().unwrap();
    assert!(
        find_workspace_file_by_root(&root).unwrap().is_some(),
        "a default session must persist workspace files"
    );
}
