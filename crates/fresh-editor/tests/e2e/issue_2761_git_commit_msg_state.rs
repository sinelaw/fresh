//! Regression coverage for issue #2761: with fresh as git's editor, the
//! cursor offset persisted for one `.git/COMMIT_EDITMSG` was restored into
//! the next one's regenerated content, landing inside git's comment block.
//!
//! Covers both stores such state lives in: the global per-file store
//! (`file_states/`, applied on open) and the per-project workspace
//! `file_states` map (applied on session restore).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::workspace::{is_git_internal_path, SerializedTabRef, Workspace};
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

fn write_commit_editmsg(project_dir: &Path, content: &str) -> PathBuf {
    let git_dir = project_dir.join(".git");
    fs::create_dir_all(&git_dir).unwrap();
    let msg = git_dir.join("COMMIT_EDITMSG");
    fs::write(&msg, content).unwrap();
    msg
}

fn project_harness(project_dir: &Path) -> EditorTestHarness {
    EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Config::default(),
        project_dir.to_path_buf(),
    )
    .unwrap()
}

/// The core repro: the second open starts at the beginning of the file.
#[test]
fn test_commit_editmsg_reopens_at_start_after_regeneration() {
    let temp = TempDir::new().unwrap();
    let project_dir = temp.path().join("repo");
    fs::create_dir(&project_dir).unwrap();
    let msg = write_commit_editmsg(&project_dir, "First message\nsecond line\nthird line\n");

    // Session 1: move the cursor away from the start, then flush per-file
    // state the way the quit path does.
    {
        let mut harness = project_harness(&project_dir);
        harness.open_file(&msg).unwrap();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        assert!(
            harness.cursor_position() > 0,
            "precondition: the cursor moved away from the start"
        );
        harness.editor_mut().save_workspace().unwrap();
    }

    // Git regenerates the file for the next commit.
    write_commit_editmsg(
        &project_dir,
        "\n# Please enter the commit message for your changes. Lines starting\n# with '#' will be ignored, and an empty message aborts the commit.\n",
    );

    // Session 2: the stale offset must not be applied to the new content.
    {
        let mut harness = project_harness(&project_dir);
        harness.open_file(&msg).unwrap();
        assert_eq!(
            harness.cursor_position(),
            0,
            "reopening a regenerated .git/COMMIT_EDITMSG must start at the beginning"
        );
    }
}

/// The workspace file must not accumulate `file_states` for `.git/` files.
#[test]
fn test_workspace_file_states_exclude_git_internal_files() {
    let temp = TempDir::new().unwrap();
    let project_dir = temp.path().join("repo");
    fs::create_dir(&project_dir).unwrap();
    let msg = write_commit_editmsg(&project_dir, "message line one\nline two\n");
    let regular = project_dir.join("notes.txt");
    fs::write(&regular, "regular file\n").unwrap();

    let mut harness = project_harness(&project_dir);
    harness.open_file(&regular).unwrap();
    harness.open_file(&msg).unwrap();
    harness.editor_mut().save_workspace().unwrap();

    let ws = Workspace::load(&project_dir)
        .unwrap()
        .expect("workspace was just saved");
    let all_keys: Vec<_> = ws
        .split_states
        .values()
        .flat_map(|s| s.file_states.keys().cloned())
        .collect();
    assert!(
        all_keys.iter().any(|k| k.ends_with("notes.txt")),
        "sanity: the regular file's state is persisted, got {all_keys:?}"
    );
    assert!(
        !all_keys.iter().any(|k| is_git_internal_path(k)),
        "no .git-internal file may appear in workspace file_states, got {all_keys:?}"
    );
}

/// A pre-fix build's workspace can still carry a `.git/COMMIT_EDITMSG`
/// entry; restore must open the tab without applying its cursor state.
#[test]
fn test_restore_ignores_poisoned_git_file_state_in_workspace() {
    let temp = TempDir::new().unwrap();
    let project_dir = temp.path().join("repo");
    fs::create_dir(&project_dir).unwrap();
    let regular = project_dir.join("notes.txt");
    fs::write(&regular, "regular file\n").unwrap();

    {
        let mut harness = project_harness(&project_dir);
        harness.open_file(&regular).unwrap();
        harness.editor_mut().save_workspace().unwrap();
    }

    // Inject the entry a pre-fix build would have written.
    let stale_offset = 15;
    let mut ws = Workspace::load(&project_dir)
        .unwrap()
        .expect("workspace was just saved");
    {
        let split_state = ws
            .split_states
            .values_mut()
            .next()
            .expect("saved workspace has a split state");
        let rel: PathBuf = [".git", "COMMIT_EDITMSG"].iter().collect();
        split_state
            .open_tabs
            .push(SerializedTabRef::File(rel.clone()));
        split_state.active_tab_index = Some(split_state.open_tabs.len() - 1);
        let mut poisoned = split_state
            .file_states
            .values()
            .next()
            .expect("saved workspace has a file state to clone")
            .clone();
        poisoned.cursor.position = stale_offset;
        poisoned.cursor.anchor = None;
        split_state.file_states.insert(rel, poisoned);
    }
    ws.save().unwrap();

    write_commit_editmsg(
        &project_dir,
        "\n# Please enter the commit message for your changes.\n",
    );

    {
        let mut harness = project_harness(&project_dir);
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();
        harness.assert_screen_contains("COMMIT_EDITMSG");
        assert_eq!(
            harness.cursor_position(),
            0,
            "restore must not apply persisted cursor state to a .git-internal file"
        );
    }
}
