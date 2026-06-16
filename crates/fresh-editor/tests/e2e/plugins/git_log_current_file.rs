//! E2E test for the "Git Log (Current File)" command.
//!
//! The command scopes the magit-style log to the focused buffer's file
//! (`git log -- <file>`), as opposed to the full-repository "Git Log"
//! command. The test asserts on rendered output only: the focused file's
//! commits appear, and a commit that touches a *different* file does not.

use crate::common::git_test_helper::{DirGuard, GitTestRepo};
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Opening the command on a focused file shows only that file's history.
// TODO: git command output differs on Windows; the other git_log tests skip it too.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_current_file_filters_to_focused_file() {
    let repo = GitTestRepo::new();

    // Two files, each with its own commit, so a file-scoped log can be told
    // apart from the full-repo log: alpha's history must not contain beta's
    // commit. A second alpha commit gives the scoped log two entries.
    repo.create_file("alpha.txt", "alpha one\n");
    repo.git_add(&["alpha.txt"]);
    repo.git_commit("Add alpha file");

    repo.create_file("beta.txt", "beta one\n");
    repo.git_add(&["beta.txt"]);
    repo.git_commit("Add beta file");

    repo.create_file("alpha.txt", "alpha one\nalpha two\n");
    repo.git_add(&["alpha.txt"]);
    repo.git_commit("Update alpha file");

    repo.setup_git_log_plugin();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&repo.path.join("alpha.txt")).unwrap();
    harness
        .wait_until(|h| h.get_buffer_content().unwrap().contains("alpha"))
        .unwrap();

    // Invoke the new command from the command palette. The command is gated
    // on a buffer being focused, which it is (alpha.txt is open).
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Git Log (Current File)").unwrap();
    harness
        .wait_for_screen_contains("Git Log (Current File)")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The scoped log lists alpha's two commits.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Update alpha file") && s.contains("Add alpha file")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Git log (current file) screen:\n{screen}");

    // The defining property of a file-scoped log: a commit that does not
    // touch alpha.txt must be excluded.
    assert!(
        !screen.contains("Add beta file"),
        "File-scoped log must exclude commits that don't touch alpha.txt.\nScreen:\n{screen}"
    );
    // The tab title carries the file's basename.
    assert!(
        screen.contains("alpha.txt"),
        "The log tab should reference the scoped file name.\nScreen:\n{screen}"
    );
}
