//! Issue #3626 — a `[No Name]` buffer saved with Save As came back as an
//! unsaved buffer the next time its workspace was switched to, and took
//! focus from the tab the user had left.
//!
//! Save As never deleted the buffer's hot-exit entry, and every switch to a
//! workspace adopts the unsaved work its entries hold. Saves now record
//! themselves in the entry before writing the file, so an entry left behind
//! by a save — a delete that never ran, or a crash before it could — reads
//! as superseded by the file it was saved to, whatever stopped the delete.

use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// How many tabs the tab bar shows for unnamed buffers.
fn no_name_tabs(harness: &EditorTestHarness) -> usize {
    harness
        .screen_row_text(layout::TAB_BAR_ROW as u16)
        .matches("No Name")
        .count()
}

fn hot_exit_config() -> Config {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    // Write recovery on the next tick rather than waiting out the interval:
    // the entry has to be on disk for the bug to exist at all.
    config.editor.auto_recovery_save_interval_secs = 0;
    config
}

/// The issue's steps: type into a new buffer, let the recovery save run,
/// Save As, switch to another workspace and back.
#[test]
fn save_as_then_switching_back_does_not_bring_the_buffer_back() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 24, hot_exit_config()).unwrap();
    let project_dir = harness.project_dir().unwrap();

    harness.new_buffer().unwrap();
    harness.type_text("FOOTNOTES-3626").unwrap();
    harness.render().unwrap();
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Save as:");
    harness.type_text("input-23.txt").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        std::fs::read_to_string(project_dir.join("input-23.txt")).unwrap(),
        "FOOTNOTES-3626"
    );

    let home = harness.editor().active_window_id();
    let saved = harness.editor().active_buffer();
    let tabs_before = no_name_tabs(&harness);

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
        no_name_tabs(&harness),
        tabs_before,
        "a buffer saved with Save As came back as an unsaved one.\nTabs: {}",
        harness.screen_row_text(layout::TAB_BAR_ROW as u16)
    );
    assert_eq!(
        harness.editor().active_buffer(),
        saved,
        "switching back must land on the tab that was left"
    );
}

/// A direct launch in `dir` sharing `dir_context`, so a second one sees the
/// first one's recovery store, as a relaunch in the same directory would.
fn launch_in(dir: &Path, dir_context: &DirectoryContext) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_config(hot_exit_config())
            .with_working_dir(dir.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap()
}

/// How the crash test stops the save.
enum CrashPoint {
    /// After the save recorded itself in the entry, before it wrote the file.
    BeforeWrite,
    /// After the file was written, before anything deleted the entry.
    AfterWrite,
}

/// Type into a new buffer in a launch in `project`, let the recovery save
/// run, start a Save As to `project/input-23.txt` and die at `crash`. Then
/// launch again in `project` and return that launch.
fn crash_during_save_as(
    temp_dir: &TempDir,
    crash: CrashPoint,
) -> (EditorTestHarness, PathBuf, DirectoryContext) {
    let project = temp_dir.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let target = project.join("input-23.txt");

    {
        let mut harness = launch_in(&project, &dir_context);
        harness.startup(true, &[]).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text("FOOTNOTES-3626").unwrap();
        harness.render().unwrap();
        harness
            .editor_mut()
            .auto_recovery_save_dirty_buffers()
            .unwrap();

        let entry = harness
            .editor()
            .active_state()
            .buffer
            .hot_exit_entry()
            .cloned()
            .expect("the recovery save leaves the buffer backed by its entry");
        match crash {
            CrashPoint::BeforeWrite => {
                let size = "FOOTNOTES-3626".len() as u64;
                assert!(entry.begin_save(&target, size).unwrap());
            }
            CrashPoint::AfterWrite => {
                // The buffer's own save, without the editor's follow-up:
                // the process is gone the moment the file is written.
                let recovery_dir = dir_context.recovery_dir();
                harness
                    .editor_mut()
                    .active_state_mut()
                    .buffer
                    .save_to_file(&target, &recovery_dir)
                    .unwrap();
            }
        }
        // Dropped without a shutdown, as a crash would leave it: nothing
        // the exit path does runs, and the editor's lock is let go.
    }

    let mut harness = launch_in(&project, &dir_context);
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    (harness, target, dir_context)
}

/// Crash before the write: nothing reached the file, so the work is only in
/// the entry, and the next launch brings it back. This is also what shows the
/// relaunch below really does restore unnamed work.
#[test]
fn a_crash_before_save_as_wrote_brings_the_work_back() {
    let temp_dir = TempDir::new().unwrap();
    let (harness, target, _) = crash_during_save_as(&temp_dir, CrashPoint::BeforeWrite);
    assert!(!target.exists());
    harness.assert_screen_contains("FOOTNOTES-3626");
}

/// Crash after the write, before the entry was deleted: the work is in the
/// file, and the next launch must not offer the entry back as an unsaved
/// buffer — or keep it around to come back later.
#[test]
fn a_crash_after_save_as_wrote_brings_nothing_back() {
    let temp_dir = TempDir::new().unwrap();
    let (harness, target, dir_context) = crash_during_save_as(&temp_dir, CrashPoint::AfterWrite);
    assert_eq!(std::fs::read_to_string(&target).unwrap(), "FOOTNOTES-3626");
    harness.assert_screen_not_contains("FOOTNOTES-3626");

    let entries = entry_ids(&store_of(&harness, &dir_context));
    assert!(entries.is_empty(), "left in the store: {entries:?}");
}

/// The recovery store's directory for the project of `harness`'s launch.
fn store_of(harness: &EditorTestHarness, dir_context: &DirectoryContext) -> PathBuf {
    fresh::services::recovery::partition_dir(
        &dir_context.recovery_dir(),
        harness.editor().working_dir(),
    )
}

/// The recovery entries in project directory `store`.
fn entries(store: &Path) -> Vec<fresh::services::recovery::RecoveryEntry> {
    fresh::services::recovery::entries_under(store)
}

/// The ids of the recovery entries in project directory `store`.
fn entry_ids(store: &Path) -> Vec<String> {
    entries(store).into_iter().map(|e| e.id).collect()
}

/// Save As the active buffer to `path`, whether or not it has a name.
fn save_as(harness: &mut EditorTestHarness, path: &Path) {
    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::SaveAs);
    harness.render().unwrap();
    // Clear whatever the prompt offers
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(path.to_str().unwrap()).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Edits made after the last recovery save, then Save As: the buffer is
/// clean, so the next recovery save must not write an entry for the saved
/// file — one that a crash, or closing the workspace, would bring back as a
/// modified tab.
#[test]
fn save_as_leaves_nothing_for_the_next_recovery_save() {
    let temp_dir = TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let target = project.join("g.txt");

    let mut harness = launch_in(&project, &dir_context);
    harness.startup(true, &[]).unwrap();
    harness.new_buffer().unwrap();
    harness.type_text("FOOTNOTES").unwrap();
    harness.render().unwrap();
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();
    harness.type_text("-MORE").unwrap();
    harness.render().unwrap();

    save_as(&mut harness, &target);
    assert_eq!(std::fs::read_to_string(&target).unwrap(), "FOOTNOTES-MORE");
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();

    let entries = entry_ids(&store_of(&harness, &dir_context));
    assert!(entries.is_empty(), "left in the store: {entries:?}");
}

/// Two workspaces with the same file open and edited each keep their own
/// entry for it. Save As to a new name in one of them resolves that one's
/// work only: the other's entry stays its unsaved work, untouched.
#[test]
fn save_as_keeps_another_workspaces_unsaved_work_on_the_same_file() {
    let temp_dir = TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let shared = project.join("shared.txt");
    std::fs::write(&shared, "base\n").unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let mut harness = launch_in(&project, &dir_context);
    harness.startup(true, &[]).unwrap();
    let store = store_of(&harness, &dir_context);
    let home = harness.editor().active_window_id();

    harness.open_file(&shared).unwrap();
    harness.type_text("A-EDIT ").unwrap();
    harness.render().unwrap();
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();

    let other_root = temp_dir.path().join("workspace_b");
    std::fs::create_dir_all(&other_root).unwrap();
    let other_store =
        fresh::services::recovery::partition_dir(&dir_context.recovery_dir(), &other_root);
    let other = harness
        .editor_mut()
        .create_window_at(other_root, "workspace_b".to_string());
    harness.editor_mut().set_active_window(other);
    harness.render().unwrap();
    harness.editor_mut().open_file(&shared).unwrap();
    harness.render().unwrap();
    harness.type_text("B-EDIT ").unwrap();
    harness.render().unwrap();
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();
    assert_eq!(entry_ids(&store).len(), 1);
    let theirs = entry_ids(&other_store);
    assert_eq!(theirs.len(), 1);

    harness.editor_mut().set_active_window(home);
    harness.render().unwrap();
    save_as(&mut harness, &project.join("renamed.txt"));
    assert!(project.join("renamed.txt").exists());

    assert!(
        entry_ids(&store).is_empty(),
        "the saved buffer's own entry is resolved"
    );
    let left = entries(&other_store);
    assert_eq!(
        left.iter().map(|e| e.id.clone()).collect::<Vec<_>>(),
        theirs,
        "the other workspace's unsaved edits to shared.txt lost their entry"
    );
    assert!(
        left[0].metadata.saving.is_none(),
        "the other workspace's entry was marked as saved"
    );
}
