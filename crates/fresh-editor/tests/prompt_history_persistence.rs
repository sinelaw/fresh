//! Prompt history (search / replace / goto-line) must survive restarts in
//! every launch mode, not only in a full workspace-restore launch.
//!
//! Driven through the harness `startup` / `shutdown` helpers, which mirror
//! `handle_first_run_setup` and the quit path in `main.rs`. Persistence is
//! isolated by `common::global_state::isolated_dir_context` so the workspace
//! store (`get_data_dir()`) and the `DirectoryContext` that
//! `load_prompt_histories` reads name one private tree.

use crate::common::global_state::isolated_dir_context;
use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use std::path::{Path, PathBuf};

fn harness_in(project: &Path, dir_context: &DirectoryContext, config: Config) -> EditorTestHarness {
    EditorTestHarness::create(
        100,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .with_empty_plugins_dir(),
    )
    .unwrap()
}

/// Config for a launch that skips the full session restore. `main.rs` takes
/// the same branch (`restore_full_session == false`) for
/// `restore_previous_session = false` and for `fresh <file>` under the default
/// `skip_session_restore_when_files_passed = true`; the harness `startup`
/// only models the former, so that is the knob used here.
fn no_restore_config() -> Config {
    let mut config = Config::default();
    config.editor.restore_previous_session = false;
    config
}

fn search_for(harness: &mut EditorTestHarness, needle: &str) {
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    // Clear any pre-filled history entry (it sits selected, so typing replaces it).
    harness.type_text(needle).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Open the search prompt and return the screen; the prompt pre-fills with the
/// most recent search-history entry (`prompt_lifecycle.rs`, "selection > last
/// history > empty").
fn open_search_prompt(harness: &mut EditorTestHarness) -> String {
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    screen
}

fn project_with_file(base: &Path) -> (PathBuf, PathBuf) {
    let project = base.join("project");
    std::fs::create_dir_all(&project).unwrap();
    let project = project.canonicalize().unwrap();
    let file = project.join("notes.txt");
    std::fs::write(&file, "alpha beta gamma\nsecond line\n").unwrap();
    (project, file)
}

/// A search made in one session is offered again after a restart that does
/// not restore the workspace (claim (a): the global history rings are loaded
/// at startup but nothing writes them any more).
#[test]
fn test_search_history_survives_restart_without_session_restore() {
    let sandbox = tempfile::tempdir().unwrap();
    let (dir_context, _data_pin) = isolated_dir_context(sandbox.path());
    let (project, file) = project_with_file(sandbox.path());

    // Session 1: ordinary launch, search for "alpha", quit.
    {
        let mut harness = harness_in(&project, &dir_context, Config::default());
        harness.startup(true, &[]).unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        search_for(&mut harness, "alpha");
        harness.shutdown(true).unwrap();
    }

    // Session 2: restore skipped (`fresh notes.txt` / restore disabled).
    {
        let mut harness = harness_in(&project, &dir_context, no_restore_config());
        let restored = harness.startup(true, std::slice::from_ref(&file)).unwrap();
        assert!(!restored, "sanity: this launch must skip the workspace restore");
        harness.render().unwrap();

        let screen = open_search_prompt(&mut harness);
        assert!(
            screen.contains("Search: alpha"),
            "search history from the previous session must be offered even when \
             the workspace layout is not restored.\nScreen:\n{screen}"
        );
    }
}

/// A launch that skips the restore must not erase the history the next
/// restoring launch would have offered (claim (b): the skipped launch starts
/// with empty rings and its quit-time workspace save overwrites
/// `WorkspaceHistories` wholesale).
#[test]
fn test_non_restoring_launch_does_not_erase_saved_search_history() {
    let sandbox = tempfile::tempdir().unwrap();
    let (dir_context, _data_pin) = isolated_dir_context(sandbox.path());
    let (project, file) = project_with_file(sandbox.path());

    // Session 1: ordinary launch, search for "alpha", quit (workspace saved
    // with histories.search = ["alpha"]).
    {
        let mut harness = harness_in(&project, &dir_context, Config::default());
        harness.startup(true, &[]).unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        search_for(&mut harness, "alpha");
        harness.shutdown(true).unwrap();
    }

    // Control: a restoring relaunch does offer "alpha" — proves the save/restore
    // path itself works and the failure below is caused by session 2.
    {
        let mut harness = harness_in(&project, &dir_context, Config::default());
        assert!(harness.startup(true, &[]).unwrap(), "workspace should restore");
        let screen = open_search_prompt(&mut harness);
        assert!(
            screen.contains("Search: alpha"),
            "sanity: a full restore offers the saved search.\nScreen:\n{screen}"
        );
        harness.shutdown(true).unwrap();
    }

    // Session 2: open the file without restoring the session; no searching.
    {
        let mut harness = harness_in(&project, &dir_context, no_restore_config());
        let restored = harness.startup(true, std::slice::from_ref(&file)).unwrap();
        assert!(!restored, "sanity: this launch must skip the workspace restore");
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    // Session 3: ordinary restoring launch — "alpha" must still be there.
    {
        let mut harness = harness_in(&project, &dir_context, Config::default());
        assert!(harness.startup(true, &[]).unwrap(), "workspace should restore");
        let screen = open_search_prompt(&mut harness);
        assert!(
            screen.contains("Search: alpha"),
            "a launch that skipped the session restore must not wipe the search \
             history saved by earlier sessions.\nScreen:\n{screen}"
        );
    }
}

/// The workspace's own copy of the history is kept by a non-restoring launch
/// even when the global ring doesn't have it — e.g. the first launch after
/// upgrading from a build that never wrote the global files, whose history
/// lives only in the workspace.
#[test]
fn test_non_restoring_launch_keeps_workspace_only_search_history() {
    let sandbox = tempfile::tempdir().unwrap();
    let (dir_context, _data_pin) = isolated_dir_context(sandbox.path());
    let (project, file) = project_with_file(sandbox.path());

    {
        let mut harness = harness_in(&project, &dir_context, Config::default());
        harness.startup(true, &[]).unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        search_for(&mut harness, "alpha");
        harness.shutdown(true).unwrap();
    }
    std::fs::remove_file(dir_context.prompt_history_path("search")).unwrap();

    // Non-restoring launch: starts with an empty search ring, searches for
    // something else, quits (saving the workspace).
    {
        let mut harness = harness_in(&project, &dir_context, no_restore_config());
        let restored = harness.startup(true, std::slice::from_ref(&file)).unwrap();
        assert!(!restored, "sanity: this launch must skip the workspace restore");
        search_for(&mut harness, "gamma");
        harness.shutdown(true).unwrap();
    }

    // Restoring launch: both searches are there, the newer one on top.
    {
        let mut harness = harness_in(&project, &dir_context, Config::default());
        assert!(harness.startup(true, &[]).unwrap(), "workspace should restore");
        let screen = open_search_prompt(&mut harness);
        assert!(
            screen.contains("Search: gamma"),
            "the newest search is offered first.\nScreen:\n{screen}"
        );
        // Up past the pre-filled newest entry reaches the older one.
        harness
            .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
            .unwrap();
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        assert!(
            screen.contains("Search: alpha"),
            "the workspace-only history must survive a non-restoring launch.\nScreen:\n{screen}"
        );
    }
}

/// `--no-restore` neither offers the saved history nor saves its own
/// (`main.rs` sets `set_workspace_persistence(false)` and quits with
/// workspaces disabled).
#[test]
fn test_no_restore_session_neither_reads_nor_writes_search_history() {
    let sandbox = tempfile::tempdir().unwrap();
    let (dir_context, _data_pin) = isolated_dir_context(sandbox.path());
    let (project, file) = project_with_file(sandbox.path());

    {
        let mut harness = harness_in(&project, &dir_context, Config::default());
        harness.startup(true, &[]).unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        search_for(&mut harness, "alpha");
        harness.shutdown(true).unwrap();
    }

    {
        let mut harness = harness_in(&project, &dir_context, Config::default());
        harness.editor_mut().set_workspace_persistence(false);
        harness.startup(false, std::slice::from_ref(&file)).unwrap();
        let screen = open_search_prompt(&mut harness);
        assert!(
            !screen.contains("Search: alpha"),
            "a --no-restore session must not offer saved history.\nScreen:\n{screen}"
        );
        search_for(&mut harness, "gamma");
        harness.shutdown(false).unwrap();
    }

    {
        let mut harness = harness_in(&project, &dir_context, no_restore_config());
        harness.startup(true, std::slice::from_ref(&file)).unwrap();
        let screen = open_search_prompt(&mut harness);
        assert!(
            screen.contains("Search: alpha"),
            "a --no-restore session must not replace the saved history.\nScreen:\n{screen}"
        );
    }
}
