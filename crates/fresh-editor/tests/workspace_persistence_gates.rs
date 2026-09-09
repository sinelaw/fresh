//! What the editor is allowed to write into a workspace file, for two
//! bugs whose whole symptom is on disk: `--no-restore` sessions leaving
//! workspace files behind (#2735) and per-file state being remembered for
//! files something else rewrites (#2761).
//!
//! Like `workspace_virtual_buffer_clobber.rs`, these drive the Editor and
//! assert against the saved workspace rather than the screen — nothing
//! here is visible in a single session, so it does not belong in an e2e
//! test (CONTRIBUTING.md Testing §2). The user-visible halves live in
//! `e2e/issue_2761_git_commit_msg_state.rs`.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use fresh::workspace::{find_workspace_file_by_root, Workspace};
use std::fs;
use std::path::{Path, PathBuf};

/// Every `file_states` key across all splits of the project's workspace.
fn persisted_state_keys(project_dir: &Path) -> Vec<PathBuf> {
    let ws = Workspace::load(project_dir)
        .unwrap()
        .expect("workspace was just saved");
    ws.split_states
        .values()
        .flat_map(|s| s.file_states.keys().cloned())
        .collect()
}

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

// ---------------------------------------------------------------------------
// #2735 — `--no-restore` must suppress mid-session checkpoints too
// ---------------------------------------------------------------------------

#[test]
fn test_no_restore_session_writes_no_workspace_files_on_extract() {
    let mut harness = harness_with_two_tabs();
    let root = harness.project_dir().unwrap();

    harness.editor_mut().set_workspace_persistence(false);

    // Extracting switches windows, firing the source window's checkpoint —
    // the write that used to ignore `--no-restore`.
    harness
        .run_palette_command("Extract Tab to New Workspace")
        .unwrap();
    harness
        .wait_for_screen_contains("Extracted notes.txt into workspace")
        .unwrap();

    assert_eq!(
        find_workspace_file_by_root(&root).unwrap(),
        None,
        "the extraction checkpoint must not write a workspace file in a --no-restore session"
    );

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

    harness
        .run_palette_command("Extract Tab to New Workspace")
        .unwrap();
    harness
        .wait_for_screen_contains("Extracted notes.txt into workspace")
        .unwrap();

    harness.editor_mut().save_all_windows_workspaces().unwrap();
    assert!(
        find_workspace_file_by_root(&root).unwrap().is_some(),
        "a default session must persist workspace files"
    );
}

// ---------------------------------------------------------------------------
// #2761 — per-file state is never persisted for ephemeral files
// ---------------------------------------------------------------------------

#[test]
fn test_workspace_file_states_exclude_ephemeral_files() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_dir = temp.path().join("repo");
    fs::create_dir(&project_dir).unwrap();
    fs::create_dir(project_dir.join(".git")).unwrap();
    let msg = project_dir.join(".git").join("COMMIT_EDITMSG");
    fs::write(&msg, "message line one\nline two\n").unwrap();
    let regular = project_dir.join("notes.txt");
    fs::write(&regular, "regular file\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Config::default(),
        project_dir.clone(),
    )
    .unwrap();
    harness.open_file(&regular).unwrap();
    harness.open_file(&msg).unwrap();
    harness.editor_mut().save_workspace().unwrap();

    let keys = persisted_state_keys(&project_dir);
    assert!(
        keys.iter().any(|k| k.ends_with("notes.txt")),
        "sanity: the regular file's state is persisted, got {keys:?}"
    );
    assert!(
        !keys.iter().any(|k| k.ends_with("COMMIT_EDITMSG")),
        "no ephemeral file may appear in workspace file_states, got {keys:?}"
    );
}

/// The exclusion is driven by config, not by a built-in git rule: a custom
/// pattern list keeps a non-git file out and lets git's scratch file back in.
#[test]
fn test_workspace_file_states_follow_configured_patterns() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_dir = temp.path().join("repo");
    fs::create_dir(&project_dir).unwrap();
    fs::create_dir(project_dir.join(".git")).unwrap();
    let msg = project_dir.join(".git").join("COMMIT_EDITMSG");
    fs::write(&msg, "message line one\n").unwrap();
    let generated = project_dir.join("schema.generated.rs");
    fs::write(&generated, "// generated\n").unwrap();
    let regular = project_dir.join("notes.txt");
    fs::write(&regular, "regular file\n").unwrap();

    let mut config = Config::default();
    config.editor.ephemeral_file_patterns = vec!["*.generated.rs".to_string()];

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_dir.clone())
            .unwrap();
    harness.open_file(&regular).unwrap();
    harness.open_file(&generated).unwrap();
    harness.open_file(&msg).unwrap();
    harness.editor_mut().save_workspace().unwrap();

    let keys = persisted_state_keys(&project_dir);
    assert!(
        !keys.iter().any(|k| k.ends_with("schema.generated.rs")),
        "the configured pattern must exclude a non-git file, got {keys:?}"
    );
    assert!(
        keys.iter().any(|k| k.ends_with("notes.txt")),
        "unmatched files keep their state, got {keys:?}"
    );
    assert!(
        keys.iter().any(|k| k.ends_with("COMMIT_EDITMSG")),
        "replacing the defaults must stop excluding git's files, got {keys:?}"
    );
}
