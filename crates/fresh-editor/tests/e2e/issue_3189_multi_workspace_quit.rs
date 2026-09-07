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

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::path::{Path, PathBuf};

/// Recovery entries (`<id>.meta.json`) sitting in `dir`.
fn recovery_entry_ids(dir: &Path) -> Vec<String> {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Vec::new();
    };
    let mut ids: Vec<String> = entries
        .flatten()
        .filter_map(|e| {
            let name = e.file_name().to_string_lossy().into_owned();
            name.strip_suffix(".meta.json").map(|id| id.to_string())
        })
        .collect();
    ids.sort();
    ids
}

/// Harness with a dirty, file-backed buffer in the starting workspace and a
/// second, clean workspace active — the exact state of the bug report.
///
/// Returns the harness, the dirty file's path, and the recovery directory the
/// editor is actually writing to (captured *before* the workspace switch,
/// since the store is scoped to the launch working directory).
fn two_workspaces_with_dirty_background(config: Config) -> (EditorTestHarness, PathBuf, PathBuf) {
    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 24, config).unwrap();
    let project_dir = harness.project_dir().unwrap();

    let file_path = project_dir.join("a.txt");
    std::fs::write(&file_path, "alpha original\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.type_text("DIRTY-EDIT").unwrap();
    harness.render().unwrap();

    let recovery_dir = harness.recovery_dir().unwrap();

    // A second Orchestrator workspace over a different root, with nothing
    // unsaved in it, and make it the active one.
    let other_root = project_dir.parent().unwrap().join("workspace_b");
    std::fs::create_dir_all(&other_root).unwrap();
    let other = harness
        .editor_mut()
        .create_window_at(other_root, "workspace_b".to_string());
    harness.editor_mut().set_active_window(other);
    harness.render().unwrap();

    (harness, file_path, recovery_dir)
}

/// Defect 1: the unsaved-changes prompt must count buffers in *every* open
/// workspace. Quitting from the clean workspace used to exit with no prompt.
#[test]
fn quit_from_clean_workspace_prompts_for_dirty_background_workspace() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, _file, _recovery_dir) = two_workspaces_with_dirty_background(config);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(
        !harness.should_quit(),
        "quitting must not exit while another workspace holds unsaved changes"
    );
    // Same prompt the single-workspace case shows, with the cross-workspace
    // total in it.
    harness.assert_screen_contains("1 buffer has unsaved changes");
    harness.assert_screen_contains("(s)ave and quit");
    harness.assert_screen_contains("(q)uit (recoverable)");
}

/// The count is a total across workspaces, not the active window's.
#[test]
fn quit_prompt_counts_dirty_buffers_across_workspaces() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, _file, _recovery_dir) = two_workspaces_with_dirty_background(config);

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
    let (mut harness, file, recovery_dir) = two_workspaces_with_dirty_background(config);

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
    let chunk = recovery_dir.join(format!("{}.chunk.0", ids[0]));
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
    let (mut harness, file, recovery_dir) = two_workspaces_with_dirty_background(config);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
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
    let (mut harness, file, recovery_dir) = two_workspaces_with_dirty_background(config);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
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
