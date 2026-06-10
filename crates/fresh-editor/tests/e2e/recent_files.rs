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
