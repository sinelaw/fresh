//! Unsaved work comes back whichever way the editor is launched (#3633).
//!
//! The recovery store used to be scoped by launch mode — a direct launch's
//! by its directory, a daemon's by its name — so work left in one was
//! invisible from the others. It is now one store filed by project root,
//! where each running editor holds its own entries and leaves everyone
//! else's alone.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::services::recovery::{RecoveryChunk, RecoveryStorage};
use std::path::{Path, PathBuf};
use tempfile::TempDir;

fn hot_exit_config() -> Config {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.editor.auto_recovery_save_interval_secs = 0;
    config
}

/// A launch in `dir` sharing `dir_context`: the same user and installation.
fn launch_in(dir: &Path, dir_context: &DirectoryContext) -> EditorTestHarness {
    launch_with(dir, dir_context, hot_exit_config())
}

fn launch_with(dir: &Path, dir_context: &DirectoryContext, config: Config) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(dir.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .with_empty_plugins_dir(),
    )
    .unwrap()
}

/// A project directory with `a.txt` in it, and a store beside it.
fn project() -> (TempDir, PathBuf, DirectoryContext) {
    let temp = TempDir::new().unwrap();
    let project = temp.path().join("project");
    std::fs::create_dir(&project).unwrap();
    std::fs::write(project.join("a.txt"), "on disk\n").unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());
    (temp, project, dir_context)
}

/// How many entries the store holds for `root`, held or not.
fn entries_for(dir_context: &DirectoryContext, root: &Path) -> usize {
    fresh::services::recovery::entries_under(&fresh::services::recovery::partition_dir(
        &dir_context.recovery_dir(),
        root,
    ))
    .len()
}

/// Work a named daemon (`fresh -a NAME`) kept at a clean exit comes back in
/// a direct launch in the same project. The two kept separate stores, and
/// nothing ever moved a named daemon's entries.
#[test]
fn work_a_named_daemon_kept_comes_back_in_a_direct_launch() {
    let (_temp, project, dir_context) = project();
    {
        let mut daemon = launch_in(&project, &dir_context);
        daemon.editor_mut().set_session_mode(true);
        daemon.editor_mut().set_session_name(Some("work".into()));
        daemon.startup(true, &[]).unwrap();
        daemon.open_file(&project.join("a.txt")).unwrap();
        daemon.type_text("DAEMON-EDIT ").unwrap();
        daemon.render().unwrap();
        daemon.shutdown(true).unwrap();
    }

    let mut direct = launch_in(&project, &dir_context);
    direct.startup(true, &[]).unwrap();
    direct.render().unwrap();
    direct.assert_screen_contains("DAEMON-EDIT");
}

/// Write an entry the way an earlier version did, flat in an old store.
fn plant_legacy_entry(store: &Path, id: &str, path: Option<&Path>, text: &str) {
    RecoveryStorage::with_dir(store.to_path_buf())
        .save_recovery_owned(
            id,
            vec![RecoveryChunk::new(0, 0, text.as_bytes().to_vec())],
            path,
            None,
            Some(1),
            0,
            text.len(),
            None,
            false,
        )
        .unwrap();
}

/// An earlier version's daemon store is read: its entry for a file in this
/// project comes back, and leaves the old store once taken.
#[test]
fn an_earlier_versions_daemon_store_is_read() {
    let (_temp, project, dir_context) = project();
    let file = project.join("a.txt").canonicalize().unwrap();
    let old = dir_context
        .recovery_dir()
        .join("sessions")
        .join("orchestrator");
    plant_legacy_entry(&old, "abc123", Some(&file), "LEGACY-DAEMON\n");

    let mut direct = launch_in(&project, &dir_context);
    direct.startup(true, &[]).unwrap();
    direct.render().unwrap();
    direct.assert_screen_contains("LEGACY-DAEMON");
    assert!(
        RecoveryStorage::with_dir(old)
            .list_entries()
            .unwrap()
            .is_empty(),
        "the entry moves when it is taken"
    );
}

/// An earlier version's direct-launch store for this project is read,
/// unnamed buffers included.
#[test]
fn an_earlier_versions_direct_launch_store_is_read() {
    let (_temp, project, dir_context) = project();
    let old = dir_context.recovery_dir().join("default").join(
        fresh::workspace::encode_path_for_filename(&project.canonicalize().unwrap()),
    );
    plant_legacy_entry(&old, "unsaved_old", None, "LEGACY-SCRATCH");

    let mut direct = launch_in(&project, &dir_context);
    direct.startup(true, &[]).unwrap();
    direct.render().unwrap();
    direct.assert_screen_contains("LEGACY-SCRATCH");
}

/// A running editor's unsaved work is its own: a second editor on the same
/// project leaves it alone. Once the first stops — here, a crash — the work
/// is anyone's to recover.
#[test]
fn a_running_editors_unsaved_work_is_left_to_it() {
    let (_temp, project, dir_context) = project();
    let mut first = launch_in(&project, &dir_context);
    first.startup(true, &[]).unwrap();
    first.new_buffer().unwrap();
    first.type_text("FIRST-EDITORS-WORK").unwrap();
    first.render().unwrap();
    first
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();

    {
        let mut second = launch_in(&project, &dir_context);
        second.startup(true, &[]).unwrap();
        second.render().unwrap();
        second.assert_screen_not_contains("FIRST-EDITORS-WORK");
        second.shutdown(true).unwrap();
    }
    assert_eq!(
        entries_for(&dir_context, &project),
        1,
        "the second editor's exit left the first one's entry alone"
    );

    drop(first); // a crash: nothing settled, the lock let go
    let mut third = launch_in(&project, &dir_context);
    third.startup(true, &[]).unwrap();
    third.render().unwrap();
    third.assert_screen_contains("FIRST-EDITORS-WORK");
}

/// Undoing back to the saved state leaves no unsaved work, so the entry
/// goes: left behind, a crash would bring the undone edit back.
#[test]
fn undoing_back_to_the_saved_state_leaves_nothing_to_recover() {
    let (_temp, project, dir_context) = project();
    let mut harness = launch_in(&project, &dir_context);
    harness.startup(true, &[]).unwrap();
    harness.open_file(&project.join("a.txt")).unwrap();
    harness.type_text("X").unwrap();
    harness.render().unwrap();
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();
    assert_eq!(entries_for(&dir_context, &project), 1);

    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(!harness.editor().active_state().buffer.is_modified());
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();
    // The delete runs off the main loop: wait for it
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    while entries_for(&dir_context, &project) != 0 && std::time::Instant::now() < deadline {
        harness.render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    assert_eq!(entries_for(&dir_context, &project), 0);
}

fn hot_exit_off() -> Config {
    let mut config = hot_exit_config();
    config.editor.hot_exit = false;
    config
}

/// With `hot_exit` off, closing a workspace window still leaves its unsaved
/// work in the store, up to the last keystroke: nothing asked about it, and
/// holding it would have this editor's exit delete it.
#[test]
fn with_hot_exit_off_a_closed_windows_work_survives_the_exit() {
    let (temp, project, dir_context) = project();
    let other_root = temp.path().join("other");
    std::fs::create_dir(&other_root).unwrap();
    let mut harness = launch_with(&project, &dir_context, hot_exit_off());
    harness.startup(true, &[]).unwrap();
    let home = harness.editor().active_window_id();
    let other = harness
        .editor_mut()
        .create_window_at(other_root.clone(), "other".to_string());
    harness.editor_mut().set_active_window(other);
    harness.render().unwrap();
    harness.new_buffer().unwrap();
    harness.type_text("CLOSED-WINDOW-WORK").unwrap();
    // No backup tick yet: the close itself writes it
    assert_eq!(entries_for(&dir_context, &other_root), 0);

    harness.editor_mut().set_active_window(home);
    harness.render().unwrap();
    assert!(harness.editor_mut().close_window(other));
    harness.shutdown(true).unwrap();
    drop(harness);
    assert_eq!(
        entries_for(&dir_context, &other_root),
        1,
        "the closed window's unsaved work was deleted at exit"
    );

    let mut next = launch_with(&other_root, &dir_context, hot_exit_off());
    next.startup(true, &[]).unwrap();
    next.render().unwrap();
    next.assert_screen_contains("CLOSED-WINDOW-WORK");
}

/// With `hot_exit` off, an exit that asked nothing (a daemon shutting down,
/// Force Quit, the GUI window closing) keeps this session's unsaved work, up
/// to the last keystroke: only the user discards it, at the quit prompt.
#[test]
fn with_hot_exit_off_an_exit_that_asked_nothing_keeps_unsaved_work() {
    let (_temp, project, dir_context) = project();
    {
        let mut off = launch_with(&project, &dir_context, hot_exit_off());
        off.startup(true, &[]).unwrap();
        off.open_file(&project.join("a.txt")).unwrap();
        off.type_text("FILE-EDIT ").unwrap();
        off.new_buffer().unwrap();
        off.type_text("SCRATCH-WORK").unwrap();
        off.render().unwrap();
        // No backup tick, no quit prompt
        off.shutdown(true).unwrap();
    }
    assert_eq!(
        entries_for(&dir_context, &project),
        2,
        "both buffers' work is kept"
    );

    let mut next = launch_with(&project, &dir_context, hot_exit_off());
    next.startup(true, &[]).unwrap();
    next.render().unwrap();
    next.assert_screen_contains("SCRATCH-WORK");
}

/// With `hot_exit` off, `fresh FILE` brings back that file's unsaved work
/// from an exit that asked nothing: kept, it has to be findable too.
#[test]
fn with_hot_exit_off_a_file_launch_brings_back_its_files_work() {
    let (_temp, project, dir_context) = project();
    let file = project.join("a.txt");
    {
        let mut off = launch_with(&project, &dir_context, hot_exit_off());
        off.startup(true, &[]).unwrap();
        off.open_file(&file).unwrap();
        off.type_text("KEPT-FILE-EDIT ").unwrap();
        off.render().unwrap();
        off.shutdown(true).unwrap();
    }

    let mut next = launch_with(&project, &dir_context, hot_exit_off());
    next.startup(true, std::slice::from_ref(&file)).unwrap();
    next.render().unwrap();
    next.assert_screen_contains("KEPT-FILE-EDIT");
}

/// With `hot_exit` off, a buffer an earlier session kept is shown — and an
/// exit that asked nothing about it leaves it in the store.
#[test]
fn with_hot_exit_off_work_taken_over_survives_an_exit_that_asked_nothing() {
    let (_temp, project, dir_context) = project();
    {
        let mut seed = launch_in(&project, &dir_context);
        seed.startup(true, &[]).unwrap();
        seed.new_buffer().unwrap();
        seed.type_text("KEPT-BY-HOT-EXIT").unwrap();
        seed.render().unwrap();
        seed.shutdown(true).unwrap();
    }

    {
        let mut off = launch_with(&project, &dir_context, hot_exit_off());
        off.startup(true, &[]).unwrap();
        off.render().unwrap();
        off.assert_screen_contains("KEPT-BY-HOT-EXIT");
        // The exit with no quit prompt in front of it
        off.shutdown(true).unwrap();
    }

    let mut on = launch_in(&project, &dir_context);
    on.startup(true, &[]).unwrap();
    on.render().unwrap();
    on.assert_screen_contains("KEPT-BY-HOT-EXIT");
}

/// With `hot_exit` off, Discard and Quit at the quit prompt is the user's
/// choice to drop the work: nothing of it is left in the store.
#[test]
fn with_hot_exit_off_discard_at_the_quit_prompt_leaves_nothing() {
    let (_temp, project, dir_context) = project();
    let mut off = launch_with(&project, &dir_context, hot_exit_off());
    off.startup(true, &[]).unwrap();
    off.new_buffer().unwrap();
    off.type_text("THROW-AWAY").unwrap();
    off.render().unwrap();
    off.editor_mut().auto_recovery_save_dirty_buffers().unwrap();
    assert_eq!(entries_for(&dir_context, &project), 1);

    off.send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    off.render().unwrap();
    off.send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    off.render().unwrap();
    assert!(off.should_quit());
    off.shutdown(true).unwrap();
    drop(off);
    assert_eq!(entries_for(&dir_context, &project), 0);
}
