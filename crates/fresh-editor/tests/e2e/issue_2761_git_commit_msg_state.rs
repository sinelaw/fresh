//! Regression coverage for issue #2761: with fresh as git's editor, the
//! cursor offset persisted for one `.git/COMMIT_EDITMSG` was restored into
//! the next one's regenerated content, so the commit opened with the cursor
//! parked inside git's comment block and typing corrupted it.
//!
//! Observed the way the user does — the status bar's `Ln`/`Col` readout and
//! where a typed character lands on screen. The on-disk half (what the
//! workspace file is allowed to contain) is in
//! `tests/workspace_persistence_gates.rs`.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
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
        100,
        24,
        Config::default(),
        project_dir.to_path_buf(),
    )
    .unwrap()
}

/// The commit message git regenerates for the next commit: an empty first
/// line to type into, then git's comment block.
const REGENERATED: &str = "\n# Please enter the commit message for your changes. Lines starting\n\
     # with '#' will be ignored, and an empty message aborts the commit.\n";

/// The core repro: the second `git commit` must open at the top of the new
/// message, and the character the user types must land there.
#[test]
fn test_commit_editmsg_reopens_at_start_after_regeneration() {
    let temp = TempDir::new().unwrap();
    let project_dir = temp.path().join("repo");
    fs::create_dir(&project_dir).unwrap();
    let msg = write_commit_editmsg(&project_dir, "First message\nsecond line\nthird line\n");

    // Session 1: move the cursor off the first line, then quit the way the
    // editor does — flushing per-file state.
    {
        let mut harness = project_harness(&project_dir);
        harness.open_file(&msg).unwrap();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("Ln 3,");
        harness.editor_mut().save_workspace().unwrap();
    }

    write_commit_editmsg(&project_dir, REGENERATED);

    // Session 2: the stale offset must not be applied to the new content.
    {
        let mut harness = project_harness(&project_dir);
        harness.open_file(&msg).unwrap();
        harness.render().unwrap();
        assert!(
            harness.get_status_bar().contains("Ln 1,"),
            "a regenerated .git/COMMIT_EDITMSG must open at line 1, status bar was: {:?}\nScreen:\n{}",
            harness.get_status_bar(),
            harness.screen_to_string()
        );

        // And typing goes into the message, not into git's comment block.
        harness.type_text("subject").unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        let subject_row = screen
            .lines()
            .position(|l| l.contains("subject"))
            .unwrap_or_else(|| panic!("typed text is not on screen\nScreen:\n{screen}"));
        let comment_row = screen
            .lines()
            .position(|l| l.contains("# Please enter"))
            .unwrap_or_else(|| panic!("comment block is not on screen\nScreen:\n{screen}"));
        assert!(
            subject_row < comment_row,
            "typed text landed inside git's comment block\nScreen:\n{screen}"
        );
    }
}

/// A workspace written before the gate existed can still carry a
/// `.git/COMMIT_EDITMSG` entry; restoring it must reopen the tab without
/// applying the stale position.
#[test]
fn test_restore_ignores_poisoned_git_file_state_in_workspace() {
    use fresh::workspace::{SerializedTabRef, Workspace};

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
        poisoned.cursor.position = 40;
        poisoned.cursor.anchor = None;
        split_state.file_states.insert(rel, poisoned);
    }
    ws.save().unwrap();

    write_commit_editmsg(&project_dir, REGENERATED);

    {
        let mut harness = project_harness(&project_dir);
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();
        harness.assert_screen_contains("COMMIT_EDITMSG");
        assert!(
            harness.get_status_bar().contains("Ln 1,"),
            "restore must not apply persisted cursor state to a .git-internal file, \
             status bar was: {:?}\nScreen:\n{}",
            harness.get_status_bar(),
            harness.screen_to_string()
        );
    }
}
