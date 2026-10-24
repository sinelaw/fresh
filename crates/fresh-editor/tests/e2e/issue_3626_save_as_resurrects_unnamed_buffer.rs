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

    let store =
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
                    assert!(entry.begin_save(&target).unwrap());
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
            dir_context.recovery_dir().join("default").join(
                fresh::workspace::encode_path_for_filename(harness.editor().working_dir()),
            )
            // Dropped without a shutdown: nothing the exit path does runs.
        };

    // What a crash leaves: a session lock naming a process that is gone.
    std::fs::write(
        store.join("session.lock"),
        r#"{"pid": 4194303, "started_at": 0, "working_dir": null}"#,
    )
    .unwrap();

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

    let store = dir_context.recovery_dir().join("default").join(
        fresh::workspace::encode_path_for_filename(harness.editor().working_dir()),
    );
    let entries: Vec<_> = std::fs::read_dir(&store)
        .unwrap()
        .flatten()
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .filter(|name| name.ends_with(".meta.json"))
        .collect();
    assert!(entries.is_empty(), "left in the store: {entries:?}");
}
