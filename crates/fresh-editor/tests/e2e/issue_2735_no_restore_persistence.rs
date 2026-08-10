//! Regression coverage for issue #2735: `--no-restore` gated the quit-time
//! save but not the mid-session checkpoint, so an Extract Tab persisted the
//! source workspace while the extracted co-tenant was never written — and
//! the next launch silently dropped it.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::workspace::find_workspace_file_by_root;
use std::fs;

/// Palette open, query, Tab to accept the first suggestion, Enter to run
/// (same flow as `extract_tab_to_workspace`).
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

/// Two tabs, ready for an extraction. Wide so the status line isn't clipped.
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

    // Simulate `--no-restore`.
    harness.editor_mut().set_workspace_persistence(false);

    // Extracting switches windows, firing the source window's checkpoint —
    // the write that used to ignore `--no-restore`.
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

/// Control: the gate must not suppress an ordinary session.
#[test]
fn test_default_session_still_persists_workspaces_on_extract() {
    let mut harness = harness_with_two_tabs();
    let root = harness.project_dir().unwrap();

    run_command_palette(&mut harness, "Extract Tab to New Workspace");
    harness.assert_screen_contains("Extracted notes.txt into workspace");

    harness.editor_mut().save_all_windows_workspaces().unwrap();
    assert!(
        find_workspace_file_by_root(&root).unwrap().is_some(),
        "a default session must persist workspace files"
    );
}
