//! Issue #3189 — quitting from a clean Orchestrator workspace silently
//! destroyed unsaved work parked in another one.
//!
//! Two defects, both covered here:
//!
//! 1. The quit prompt only ever looked at the *active* window's buffers, so
//!    `Ctrl+Q` from a clean workspace exited immediately with no prompt while
//!    a background workspace held a dirty buffer.
//! 2. The exit path then cleaned the recovery store down to the active
//!    window's preserve list, deleting the background workspace's
//!    auto-recovery files. The edit was unrecoverable on the next start, and
//!    the user was never warned at any point.
//!
//! Two more places had the same active-window blind spot and the same
//! consequence, covered at the bottom of this file: the periodic
//! auto-recovery save (so a *crash* lost a background workspace's edits even
//! though a clean exit no longer does), and `close_window`, which drops a
//! workspace's buffers without asking about or preserving what they hold.

use crate::common::harness::{layout, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh_core::WindowId;
use std::path::{Path, PathBuf};

/// The ids of the recovery entries in project directory `dir` of the store.
fn recovery_entry_ids(dir: &Path) -> Vec<String> {
    let mut ids: Vec<String> = fresh::services::recovery::entries_under(dir)
        .into_iter()
        .map(|e| e.id)
        .collect();
    ids.sort();
    ids
}

/// The first content file of entry `id` in project directory `dir`.
fn chunk_path(dir: &Path, id: &str) -> PathBuf {
    let entry = fresh::services::recovery::entries_under(dir)
        .into_iter()
        .find(|e| e.id == id)
        .expect("entry is in the store");
    entry
        .metadata_path
        .parent()
        .unwrap()
        .join(format!("{id}.chunk.0"))
}

/// Harness with a dirty, file-backed buffer in the starting workspace and a
/// second, clean workspace active — the exact state of the bug report.
///
/// Returns the harness, the dirty file's path, the recovery directory the
/// editor is actually writing to (captured *before* the workspace switch,
/// since the store is scoped to the launch working directory), and the id of
/// the now-background workspace holding the dirty buffer.
fn two_workspaces_with_dirty_background(
    config: Config,
) -> (EditorTestHarness, PathBuf, PathBuf, WindowId) {
    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 24, config).unwrap();
    let project_dir = harness.project_dir().unwrap();

    let file_path = project_dir.join("a.txt");
    std::fs::write(&file_path, "alpha original\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.type_text("DIRTY-EDIT").unwrap();
    harness.render().unwrap();

    let recovery_dir = harness.recovery_dir().unwrap();
    let dirty_window = harness.editor().active_window_id();

    // A second Orchestrator workspace over a different root, with nothing
    // unsaved in it, and make it the active one.
    let other_root = project_dir.parent().unwrap().join("workspace_b");
    std::fs::create_dir_all(&other_root).unwrap();
    let other = harness
        .editor_mut()
        .create_window_at(other_root, "workspace_b".to_string());
    harness.editor_mut().set_active_window(other);
    harness.render().unwrap();

    (harness, file_path, recovery_dir, dirty_window)
}

/// Defect 1: the unsaved-changes prompt must count buffers in *every* open
/// workspace. Quitting from the clean workspace used to exit with no prompt.
#[test]
fn quit_from_clean_workspace_prompts_for_dirty_background_workspace() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, _file, _recovery_dir, _dirty) = two_workspaces_with_dirty_background(config);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(
        !harness.should_quit(),
        "quitting must not exit while another workspace holds unsaved changes"
    );
    // Same question the single-workspace case asks, with the cross-workspace
    // total in it.
    harness.assert_screen_contains("1 buffer has unsaved changes");
    harness.assert_screen_contains("Save and Quit");
    harness.assert_screen_contains("Quit (recoverable)");
}

/// The count is a total across workspaces, not the active window's.
#[test]
fn quit_prompt_counts_dirty_buffers_across_workspaces() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, _file, _recovery_dir, _dirty) = two_workspaces_with_dirty_background(config);

    // Dirty a second file, this time in the (now active) second workspace.
    let project_dir = harness.project_dir().unwrap();
    let other_file = project_dir.join("b.txt");
    std::fs::write(&other_file, "beta original\n").unwrap();
    harness.open_file(&other_file).unwrap();
    harness.type_text("ALSO-DIRTY").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("2 buffers have unsaved changes");
}

/// Defect 2: a clean exit must not delete recovery data for a workspace whose
/// unsaved content was never offered to the user. Before the fix the exit
/// flush and the preserve list were both active-window-only, so the
/// background workspace's chunk + meta were deleted on the way out.
#[test]
fn clean_exit_preserves_recovery_for_dirty_background_workspace() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, file, recovery_dir, _dirty) = two_workspaces_with_dirty_background(config);

    // Exit exactly as production does (the "quit, recoverable" outcome).
    harness.shutdown(true).unwrap();

    let ids = recovery_entry_ids(&recovery_dir);
    assert_eq!(
        ids.len(),
        1,
        "the background workspace's unsaved buffer must still have recovery data \
         after a clean exit; found {ids:?} in {}",
        recovery_dir.display()
    );
    let chunk = chunk_path(&recovery_dir, &ids[0]);
    assert!(
        chunk.exists(),
        "recovery content must survive too, not just the metadata"
    );
    let saved = std::fs::read_to_string(&chunk).unwrap();
    assert!(
        saved.contains("DIRTY-EDIT"),
        "the preserved recovery chunk must hold the unsaved edit, got {saved:?}"
    );
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "alpha original\n",
        "the file itself is untouched — the edit only exists in recovery"
    );
}

/// "Save and quit" is a promise about the whole editor: it has to write the
/// dirty buffer in the background workspace too, not just the active one.
#[test]
fn save_and_quit_writes_background_workspace_buffers_to_disk() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, file, recovery_dir, _dirty) = two_workspaces_with_dirty_background(config);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.should_quit(),
        "'s' must quit once everything is saved"
    );
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "DIRTY-EDITalpha original\n",
        "the background workspace's buffer must have been written to disk"
    );

    harness.shutdown(true).unwrap();
    assert!(
        recovery_entry_ids(&recovery_dir).is_empty(),
        "recovery data for a buffer that was saved is resolved and cleaned up"
    );
}

/// "Discard and quit" likewise applies to every workspace: the background
/// workspace's changes must be dropped, recovery data included, or the next
/// start would resurrect edits the user explicitly threw away.
#[test]
fn discard_and_quit_drops_background_workspace_recovery() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, file, recovery_dir, _dirty) = two_workspaces_with_dirty_background(config);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.should_quit(), "'d' must quit");
    harness.shutdown(true).unwrap();

    assert!(
        recovery_entry_ids(&recovery_dir).is_empty(),
        "explicitly discarded changes must not be left behind in recovery"
    );
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "alpha original\n",
        "discard must not write the change to disk either"
    );
}

/// A clean workspace in the background must not invent a prompt: the fix
/// widens *which* buffers are considered, it does not make quitting sticky.
#[test]
fn quit_still_exits_immediately_when_no_workspace_is_dirty() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 24, config).unwrap();
    let project_dir = harness.project_dir().unwrap();

    let file_path = project_dir.join("clean.txt");
    std::fs::write(&file_path, "untouched\n").unwrap();
    harness.open_file(&file_path).unwrap();

    let other_root = project_dir.parent().unwrap().join("workspace_b");
    std::fs::create_dir_all(&other_root).unwrap();
    let other = harness
        .editor_mut()
        .create_window_at(other_root, "workspace_b".to_string());
    harness.editor_mut().set_active_window(other);
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.should_quit(),
        "with nothing unsaved anywhere, Ctrl+Q must still exit without a prompt"
    );
}

/// The periodic auto-recovery save has to sweep every workspace. It only ever
/// ran against the active window, so a buffer edited and then left behind had
/// recovery data no newer than the last tick it was on screen for — and a
/// crash, which gets no chance to flush the way a clean exit does, lost
/// everything typed since.
#[test]
fn periodic_recovery_save_covers_background_workspaces() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    // Every call is past the rate limit, so one call is one full sweep.
    config.editor.auto_recovery_save_interval_secs = 0;
    let (mut harness, _file, recovery_dir, _dirty) = two_workspaces_with_dirty_background(config);

    // No shutdown, no quit: just the tick the event loop runs every frame.
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();

    let ids = recovery_entry_ids(&recovery_dir);
    assert_eq!(
        ids.len(),
        1,
        "the background workspace's dirty buffer must be swept into recovery \
         without waiting for a clean exit; found {ids:?}"
    );
    let saved = std::fs::read_to_string(chunk_path(&recovery_dir, &ids[0])).unwrap();
    assert!(
        saved.contains("DIRTY-EDIT"),
        "the swept chunk must hold the unsaved edit, got {saved:?}"
    );
}

/// Closing a workspace drops its buffers outright — nothing on that path asks
/// the user about unsaved changes. The content must at least be flushed to
/// recovery on the way out, and must then survive the exit that follows: the
/// close leaves it as unsaved work no editor holds, which an exit never
/// deletes.
#[test]
fn closing_a_workspace_preserves_its_unsaved_content() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, _file, recovery_dir, dirty_window) =
        two_workspaces_with_dirty_background(config);

    // The dirty workspace is the one we started in; the clean second one is
    // active (close_window refuses to close the active window).
    assert!(
        harness.editor_mut().close_window(dirty_window),
        "closing the non-active workspace must succeed"
    );

    let ids = recovery_entry_ids(&recovery_dir);
    assert_eq!(
        ids.len(),
        1,
        "closing a workspace must flush its unsaved buffers to recovery; found {ids:?}"
    );
    let chunk = chunk_path(&recovery_dir, &ids[0]);
    assert!(
        std::fs::read_to_string(&chunk)
            .unwrap()
            .contains("DIRTY-EDIT"),
        "the flushed chunk must hold the unsaved edit"
    );

    // And the exit that follows must not undo that: no live buffer backs the
    // entry any more, so it is unaccounted for and kept.
    harness.shutdown(true).unwrap();
    assert_eq!(
        recovery_entry_ids(&recovery_dir),
        ids,
        "a closed workspace's flushed content must survive the exit too"
    );
}

/// The quit prompt names the workspaces when some of the unsaved work is
/// somewhere the user cannot see. A bare count is what made the original bug
/// survivable-looking: it says there is something to lose without saying
/// where, in exactly the situation where "where" is the whole problem.
#[test]
fn quit_prompt_names_the_workspaces_holding_unsaved_work() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, _file, _recovery_dir, _dirty) = two_workspaces_with_dirty_background(config);

    // Second dirty buffer, this one in the active workspace, so the prompt
    // has to distinguish two workspaces with different counts.
    let project_dir = harness.project_dir().unwrap();
    let other_file = project_dir.join("b.txt");
    std::fs::write(&other_file, "beta original\n").unwrap();
    harness.open_file(&other_file).unwrap();
    harness.type_text("ALSO-DIRTY").unwrap();
    let third = project_dir.join("c.txt");
    std::fs::write(&third, "gamma original\n").unwrap();
    harness.open_file(&third).unwrap();
    harness.type_text("AND-ANOTHER").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("3 buffers have unsaved changes");
    // The workspace holding more than one is qualified with its count; the
    // one holding a single buffer is named bare.
    harness.assert_screen_contains("workspace_b: 2");
}

/// The converse: when everything unsaved is in the workspace on screen, the
/// prompt stays in its plain form. The modified markers are right there in
/// the tab bar — naming the workspace would be noise, and this is the shape
/// every single-workspace user sees.
#[test]
fn quit_prompt_stays_plain_when_all_unsaved_work_is_in_view() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 24, config).unwrap();
    let project_dir = harness.project_dir().unwrap();

    let file_path = project_dir.join("only.txt");
    std::fs::write(&file_path, "initial\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.type_text("edit").unwrap();

    // A second workspace exists but holds nothing unsaved.
    let other_root = project_dir.parent().unwrap().join("workspace_b");
    std::fs::create_dir_all(&other_root).unwrap();
    harness
        .editor_mut()
        .create_window_at(other_root, "workspace_b".to_string());
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("1 buffer has unsaved changes.");
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("workspace_b"),
        "the prompt should not name workspaces when the unsaved work is all \
         in the active one.\nScreen: {screen}"
    );
}

/// Crash recovery puts each buffer back in the workspace it was edited in.
/// Restoring everything into whichever workspace happened to be in front
/// reshuffled unsaved work between projects.
#[test]
fn crash_recovery_restores_each_buffer_into_its_own_workspace() {
    let temp = tempfile::TempDir::new().unwrap();
    let project = temp.path().join("project");
    let other_root = temp.path().join("workspace_b");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&other_root).unwrap();
    let dir_context = fresh::config_io::DirectoryContext::for_testing(temp.path());
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.editor.auto_recovery_save_interval_secs = 0;
    let launch = |dir: &Path| {
        EditorTestHarness::create(
            120,
            24,
            crate::common::harness::HarnessOptions::new()
                .with_config(config.clone())
                .with_working_dir(dir.to_path_buf())
                .with_shared_dir_context(dir_context.clone())
                .with_empty_plugins_dir(),
        )
        .unwrap()
    };

    {
        let mut harness = launch(&project);
        let a = project.join("a.txt");
        std::fs::write(&a, "alpha original\n").unwrap();
        harness.open_file(&a).unwrap();
        harness.type_text("ALPHA-EDIT").unwrap();

        // A second workspace, on another root, with an edit of its own to a
        // file of the first project's.
        let other = harness
            .editor_mut()
            .create_window_at(other_root.clone(), "workspace_b".to_string());
        harness.editor_mut().set_active_window(other);
        let b = project.join("b.txt");
        std::fs::write(&b, "beta original\n").unwrap();
        harness.open_file(&b).unwrap();
        harness.type_text("BETA-EDIT").unwrap();
        harness
            .editor_mut()
            .auto_recovery_save_dirty_buffers()
            .unwrap();
        // Dropped without a shutdown: a crash.
    }

    let mut harness = launch(&project);
    harness.startup(true, &[]).unwrap();
    harness.assert_screen_contains("ALPHA-EDIT");
    let tabs = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
    assert!(
        !tabs.contains("b.txt"),
        "the other workspace's buffer must NOT be pulled into this one by \
         crash recovery, though its file is in this project.\nTabs: {tabs}"
    );
    drop(harness);

    let mut harness = launch(&other_root);
    harness.startup(true, &[]).unwrap();
    harness.assert_screen_contains("BETA-EDIT");
}

/// Quitting after visiting only some workspaces must leave the unvisited
/// ones' recovery data alone — they were never offered to the user, so
/// nothing here can decide they are finished with.
#[test]
fn quitting_without_visiting_a_workspace_keeps_its_recovery_data() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.editor.auto_recovery_save_interval_secs = 0;
    let (mut harness, _file, recovery_dir, _dirty) = two_workspaces_with_dirty_background(config);

    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();
    let before = recovery_entry_ids(&recovery_dir);
    assert_eq!(before.len(), 1);

    // Never dive into the workspace that owns it; just shut down.
    harness.shutdown(true).unwrap();

    assert_eq!(
        recovery_entry_ids(&recovery_dir),
        before,
        "an unvisited workspace's unsaved work must survive the exit"
    );
}

fn unnamed_tabs(harness: &EditorTestHarness) -> usize {
    harness
        .screen_row_text(layout::TAB_BAR_ROW as u16)
        .matches("[No Name]")
        .count()
}

/// Switching back to a workspace must not open its unsaved scratch buffer a
/// second time.
#[test]
fn reactivating_a_workspace_does_not_duplicate_its_unnamed_buffers() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.editor.auto_recovery_save_interval_secs = 0;
    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 24, config).unwrap();
    let project_dir = harness.project_dir().unwrap();

    harness.new_buffer().unwrap();
    harness.type_text("scratch").unwrap();
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();
    harness.render().unwrap();
    let home = harness.editor().active_window_id();
    let tabs_before = unnamed_tabs(&harness);

    let other_root = project_dir.parent().unwrap().join("workspace_b");
    std::fs::create_dir_all(&other_root).unwrap();
    let other = harness
        .editor_mut()
        .create_window_at(other_root, "workspace_b".to_string());

    for _ in 0..3 {
        harness.editor_mut().set_active_window(other);
        harness.render().unwrap();
        harness.editor_mut().set_active_window(home);
        harness.render().unwrap();
    }

    assert_eq!(
        unnamed_tabs(&harness),
        tabs_before,
        "Tabs: {}",
        harness.screen_row_text(layout::TAB_BAR_ROW as u16)
    );
}

/// A restored unsaved scratch buffer stays a single tab over restarts, each
/// with a switch away and back.
#[test]
fn unnamed_buffer_is_restored_once_across_restarts() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    let other_root = temp_dir.path().join("workspace_b");
    std::fs::create_dir(&project_dir).unwrap();
    std::fs::create_dir(&other_root).unwrap();
    let dir_context = fresh::config_io::DirectoryContext::for_testing(temp_dir.path());

    let harness_for = || {
        let mut config = Config::default();
        config.editor.hot_exit = true;
        let mut harness = EditorTestHarness::create(
            120,
            24,
            crate::common::harness::HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.editor_mut().set_session_mode(true);
        harness
    };

    {
        let mut harness = harness_for();
        harness.new_buffer().unwrap();
        harness.type_text("scratch").unwrap();
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    for start in 1..=3 {
        let mut harness = harness_for();
        assert!(harness.startup(true, &[]).unwrap());
        harness.render().unwrap();

        let home = harness.editor().active_window_id();
        let other = harness
            .editor_mut()
            .create_window_at(other_root.clone(), "workspace_b".to_string());
        harness.editor_mut().set_active_window(other);
        harness.render().unwrap();
        harness.editor_mut().set_active_window(home);
        harness.render().unwrap();

        assert_eq!(
            unnamed_tabs(&harness),
            1,
            "start {start}. Tabs: {}",
            harness.screen_row_text(layout::TAB_BAR_ROW as u16)
        );
        harness.shutdown(true).unwrap();
    }
}
