//! E2E tests for the recent-files feature (issue #926).
//!
//! "Open Recent" lists recently opened files (most recent first) in a
//! prompt picker reachable from the File menu and the command palette.
//! The list is persisted in the workspace file and restored across
//! sessions.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Create a project dir with two distinctly named files.
fn setup_project() -> (
    TempDir,
    std::path::PathBuf,
    std::path::PathBuf,
    std::path::PathBuf,
) {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let alpha = project_dir.join("alpha.txt");
    let bravo = project_dir.join("bravo.txt");
    std::fs::write(&alpha, "Content of alpha").unwrap();
    std::fs::write(&bravo, "Content of bravo").unwrap();
    (temp_dir, project_dir, alpha, bravo)
}

/// The File menu shows the "Open Recent..." entry.
#[test]
fn test_file_menu_shows_open_recent() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Open Recent...");
}

/// Invoking Open Recent with no recorded files shows a status message
/// instead of an empty picker.
#[test]
fn test_open_recent_empty_shows_status_message() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Run "Open Recent" from the command palette.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Open Recent").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("No recent files");
}

/// Opening files records them; the picker lists them most recent
/// first, and selecting an entry re-opens that file.
#[test]
fn test_open_recent_lists_and_opens_files() {
    let (_temp_dir, project_dir, alpha, bravo) = setup_project();
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Config::default(), project_dir)
            .unwrap();

    harness.open_file(&alpha).unwrap();
    harness.open_file(&bravo).unwrap();
    harness.assert_buffer_content("Content of bravo");

    // Run "Open Recent" from the command palette.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Open Recent").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Both entries are listed in the picker.
    harness.assert_screen_contains("Open recent: ");
    harness.assert_screen_contains("alpha.txt");
    harness.assert_screen_contains("bravo.txt");

    // Type to filter down to alpha.txt and confirm.
    harness.type_text("alpha").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("Content of alpha");
}

/// The most recently opened file is the picker's pre-selected first
/// entry, so Enter alone re-opens it.
#[test]
fn test_open_recent_orders_most_recent_first() {
    let (_temp_dir, project_dir, alpha, bravo) = setup_project();
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Config::default(), project_dir)
            .unwrap();

    // Open bravo, then alpha: alpha is the most recent.
    harness.open_file(&bravo).unwrap();
    harness.open_file(&alpha).unwrap();
    // Switch back to bravo so the active buffer differs from the
    // most recent entry.
    harness.open_file(&bravo).unwrap();
    harness.assert_buffer_content("Content of bravo");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Open Recent").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Re-opening bravo moved it back to the front; pressing Enter on
    // the pre-selected first entry opens it.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content of bravo");
}

/// Recent files survive a restart via workspace persistence.
#[test]
fn test_open_recent_persists_across_sessions() {
    let (_temp_dir, project_dir, alpha, bravo) = setup_project();

    // First session: open both files and save the workspace.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            100,
            30,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&alpha).unwrap();
        harness.open_file(&bravo).unwrap();
        harness.editor_mut().save_workspace().unwrap();
    }

    // Second session: restore, then open a recent file via the picker.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            100,
            30,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Workspace should have been restored");
        harness.render().unwrap();

        harness
            .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap();
        harness.type_text("Open Recent").unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        harness.assert_screen_contains("Open recent: ");

        // Filter to alpha.txt and open it.
        harness.type_text("alpha").unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
        harness.assert_buffer_content("Content of alpha");
    }
}

/// Entries whose file was deleted are not offered in the picker.
#[test]
fn test_open_recent_skips_deleted_files() {
    let (_temp_dir, project_dir, alpha, bravo) = setup_project();
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Config::default(), project_dir)
            .unwrap();

    harness.open_file(&alpha).unwrap();
    harness.open_file(&bravo).unwrap();

    // Close the bravo buffer, then delete it from disk.
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    std::fs::remove_file(&bravo).unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Open Recent").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Open recent: ");
    harness.assert_screen_contains("alpha.txt");
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("bravo.txt"),
        "Deleted file should not be listed in the Open Recent picker:\n{}",
        screen
    );
}

/// Open the Open Recent picker via the command palette.
fn open_recent_picker(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Open Recent").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Set up a second window (orchestrator session) rooted at its own
/// project dir with one opened file. Leaves the second window active.
/// Returns the second window's TempDir (must stay alive) and id.
fn add_second_window(harness: &mut EditorTestHarness) -> (tempfile::TempDir, fresh_core::WindowId) {
    let second_temp = tempfile::tempdir().unwrap();
    std::fs::write(second_temp.path().join("beta.txt"), "Content of beta").unwrap();
    let second_id = harness
        .editor_mut()
        .create_window_at(second_temp.path().to_path_buf(), "second".to_string());
    harness.editor_mut().set_active_window(second_id);
    harness
        .open_file(&second_temp.path().join("beta.txt"))
        .unwrap();
    (second_temp, second_id)
}

/// Each window (orchestrator session) keeps its own recent-files
/// list: files opened in one window never appear in another window's
/// picker.
#[test]
fn test_open_recent_is_per_window() {
    let (_temp_dir, project_dir, alpha, _bravo) = setup_project();
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        Config::default(),
        project_dir.clone(),
    )
    .unwrap();

    let first_id = harness.editor_mut().active_window_id();
    harness.open_file(&alpha).unwrap();

    let (_second_temp, _second_id) = add_second_window(&mut harness);

    // Second window's picker lists only its own file.
    open_recent_picker(&mut harness);
    harness.assert_screen_contains("Open recent: ");
    harness.assert_screen_contains("beta.txt");
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("alpha.txt"),
        "First window's file must not leak into second window's picker:\n{}",
        screen
    );
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Back in the first window, the picker lists only the first
    // window's file.
    harness.editor_mut().set_active_window(first_id);
    harness.render().unwrap();
    open_recent_picker(&mut harness);
    harness.assert_screen_contains("Open recent: ");
    harness.assert_screen_contains("alpha.txt");
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("beta.txt"),
        "Second window's file must not leak into first window's picker:\n{}",
        screen
    );
}

/// Quitting with multiple windows saves each window's recent list
/// into its own per-directory workspace file; restarting rooted at
/// either directory restores only that directory's list.
#[test]
fn test_open_recent_multi_window_quit_and_restart() {
    let (_temp_dir, project_dir, alpha, _bravo) = setup_project();
    let second_temp;
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            100,
            30,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&alpha).unwrap();
        let (temp, _) = add_second_window(&mut harness);
        second_temp = temp;

        // Quit path: save every window's workspace.
        harness.editor_mut().save_all_windows_workspaces().unwrap();
    }

    // Restart rooted at the first project: only alpha.txt is recent.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            100,
            30,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        assert!(harness.editor_mut().try_restore_workspace().unwrap());
        harness.render().unwrap();

        open_recent_picker(&mut harness);
        harness.assert_screen_contains("alpha.txt");
        let screen = harness.screen_to_string();
        assert!(
            !screen.contains("beta.txt"),
            "Other session's file must not appear after restart:\n{}",
            screen
        );
    }

    // Restart rooted at the second project: only beta.txt is recent.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            100,
            30,
            Config::default(),
            second_temp.path().to_path_buf(),
        )
        .unwrap();
        assert!(harness.editor_mut().try_restore_workspace().unwrap());
        harness.render().unwrap();

        open_recent_picker(&mut harness);
        harness.assert_screen_contains("beta.txt");
        let screen = harness.screen_to_string();
        assert!(
            !screen.contains("alpha.txt"),
            "Other session's file must not appear after restart:\n{}",
            screen
        );
    }
}

/// Restarting in a directory that was never opened before starts
/// with an empty recent list — another project's list doesn't leak.
#[test]
fn test_restart_in_different_directory_starts_empty() {
    let (_temp_dir, project_dir, alpha, _bravo) = setup_project();

    // First session in project A records alpha.txt.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            100,
            30,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&alpha).unwrap();
        harness.editor_mut().save_workspace().unwrap();
    }

    // Restart in a fresh, unrelated directory: no recent files.
    {
        let other_temp = tempfile::tempdir().unwrap();
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            100,
            30,
            Config::default(),
            other_temp.path().to_path_buf(),
        )
        .unwrap();
        let _ = harness.editor_mut().try_restore_workspace().unwrap();
        harness.render().unwrap();

        open_recent_picker(&mut harness);
        harness.assert_screen_contains("No recent files");
    }
}
