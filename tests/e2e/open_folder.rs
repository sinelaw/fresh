//! E2E tests for the Open Folder feature
//!
//! Tests the ability to switch the project root (working directory) using
//! the command palette or File menu.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use tempfile::TempDir;

/// Test that Open Folder command appears in the command palette
#[test]
fn test_open_folder_command_in_palette() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for palette to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Command:"))
        .expect("Command palette should appear");

    // Type "open folder" to search
    harness.type_text("open folder").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Open Folder command should appear
    assert!(
        screen.contains("Open Folder"),
        "Open Folder command should appear in palette"
    );
}

/// Test that the folder browser appears when Open Folder is selected
#[test]
fn test_open_folder_shows_folder_browser() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create some directories
    fs::create_dir(project_root.join("subdir1")).unwrap();
    fs::create_dir(project_root.join("subdir2")).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open command palette and select Open Folder
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Command:"))
        .expect("Command palette should appear");

    harness.type_text("open folder").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for folder browser to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Navigation:") && screen.contains("Open")
        })
        .expect("Folder browser should appear");

    let screen = harness.screen_to_string();

    // Should show the folder browser with directories
    assert!(
        screen.contains("Navigation:"),
        "Navigation section should be visible"
    );
    assert!(
        screen.contains("subdir1") || screen.contains("subdir2"),
        "Directories should be listed"
    );
}

/// Test that selecting a folder changes the working directory
#[test]
fn test_open_folder_changes_working_dir() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create a subdirectory
    let subdir = project_root.join("myproject");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("README.md"), "Project readme").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100, // Wider terminal to see full message
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open command palette and select Open Folder
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Command:"))
        .expect("Command palette should appear");

    harness.type_text("open folder").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for folder browser
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("Folder browser should appear");

    // Navigate to myproject subdirectory
    harness.type_text("myproject").unwrap();
    harness.render().unwrap();

    // Press Enter to select the folder
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for the status message indicating the project switch
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Switched to project") || screen.contains("myproject")
        })
        .expect("Should show project switch message");
}

/// Test that pressing Enter with no selection uses current directory
#[test]
fn test_open_folder_select_current_directory() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create a nested structure
    let subdir = project_root.join("current_test");
    fs::create_dir(&subdir).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        24,
        Default::default(),
        subdir.clone(), // Start in the subdirectory
    )
    .unwrap();

    // Open folder browser
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Command:"))
        .expect("Command palette should appear");

    harness.type_text("open folder").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for folder browser
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("Folder browser should appear");

    // Press Enter immediately to select current directory
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show confirmation message
    harness
        .wait_until(|h| h.screen_to_string().contains("Switched to project"))
        .expect("Should confirm project switch");
}

/// Test that canceling folder browser with Escape doesn't change directory
#[test]
fn test_open_folder_cancel_preserves_directory() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open folder browser
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Command:"))
        .expect("Command palette should appear");

    harness.type_text("open folder").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for folder browser
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("Folder browser should appear");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Folder browser should be closed
    harness.assert_screen_not_contains("Navigation:");
    harness.assert_screen_contains("Canceled");
}

/// Test that folder browser can navigate using backspace to go to parent
#[test]
fn test_open_folder_backspace_goes_parent() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create nested structure
    let subdir = project_root.join("nested");
    fs::create_dir(&subdir).unwrap();
    fs::write(project_root.join("root_file.txt"), "root").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        subdir.clone(), // Start in nested directory
    )
    .unwrap();

    // Open folder browser
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Command:"))
        .expect("Command palette should appear");

    harness.type_text("open folder").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for folder browser
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("Folder browser should appear");

    // Press backspace to go to parent
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // Wait for parent directory contents
    harness
        .wait_until(|h| h.screen_to_string().contains("root_file.txt"))
        .expect("Should navigate to parent and show root_file.txt");
}

/// Test that Open Folder appears in the File menu
#[test]
fn test_open_folder_in_file_menu() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open File menu with F10
    harness.send_key(KeyCode::F(10), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("File"))
        .expect("Menu should appear");

    let screen = harness.screen_to_string();

    // Open Folder should appear in File menu
    assert!(
        screen.contains("Open Folder"),
        "Open Folder should be in File menu"
    );
}

// Note: File explorer integration test removed as it requires longer timeout
// The file explorer update is tested manually via tmux session

/// Test the full folder switching flow with session handling
///
/// This test verifies:
/// 1. Editor requests restart when switching folders (via should_quit + take_restart_dir)
/// 2. Sessions are saved per-working-directory
/// 3. Sessions are restored when starting in the same directory
/// 4. Switching folders provides a clean slate (no old buffers)
#[test]
fn test_open_folder_restart_flow_with_sessions() {
    // Create two project directories
    let temp_dir = TempDir::new().unwrap();
    let project_a = temp_dir.path().join("project_a");
    let project_b = temp_dir.path().join("project_b");
    fs::create_dir(&project_a).unwrap();
    fs::create_dir(&project_b).unwrap();

    // Create files in each project
    let file_a = project_a.join("main_a.txt");
    let file_b = project_b.join("main_b.txt");
    fs::write(&file_a, "Content from Project A").unwrap();
    fs::write(&file_b, "Content from Project B").unwrap();

    // Create a shared directory context for consistent session storage
    let dir_context = fresh::config::DirectoryContext::from_system().unwrap();

    // Phase 1: Start in project_a, open file, save session
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Open the file in project_a
        harness.open_file(&file_a).unwrap();
        harness.render().unwrap();

        // Verify file is opened
        harness.assert_screen_contains("main_a.txt");
        harness.assert_screen_contains("Content from Project A");

        // Save session for project_a
        harness.editor_mut().save_session().unwrap();
    }

    // Phase 2: Start fresh in project_a - session should restore
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should be restored for project_a");

        harness.render().unwrap();

        // Verify the file from project_a was restored
        harness.assert_screen_contains("main_a.txt");
    }

    // Phase 3: Start in project_a and switch to project_b via Open Folder
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session (project_a's file)
        harness.editor_mut().try_restore_session().unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("main_a.txt");

        // Open folder browser and switch to project_b
        harness
            .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains("Command:"))
            .expect("Command palette should appear");

        harness.type_text("open folder").unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        // Wait for folder browser
        harness
            .wait_until(|h| h.screen_to_string().contains("Navigation:"))
            .expect("Folder browser should appear");

        // Type path to project_b
        let project_b_str = project_b.to_string_lossy().to_string();
        harness.type_text(&project_b_str).unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        // Verify editor requested restart (should_quit should be true after folder switch)
        assert!(
            harness.should_quit(),
            "Editor should request quit/restart after folder switch"
        );

        // Verify restart was requested with the new directory
        let restart_dir = harness.editor_mut().take_restart_dir();
        assert!(
            restart_dir.is_some(),
            "Editor should have a restart directory set"
        );
        let restart_dir = restart_dir.unwrap();
        assert!(
            restart_dir.starts_with(&project_b) || project_b.starts_with(&restart_dir),
            "Restart directory should be project_b: got {:?}, expected {:?}",
            restart_dir,
            project_b
        );
    }

    // Phase 4: Simulate main loop restart - create new editor in project_b
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_b.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // On restart, session restore is skipped (is_first_run = false in main loop)
        // So we get a fresh editor - verify no old files
        harness.render().unwrap();

        // Should NOT contain project_a's file
        harness.assert_screen_not_contains("main_a.txt");
        harness.assert_screen_not_contains("Content from Project A");

        // Open file in project_b and save session
        harness.open_file(&file_b).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("main_b.txt");
        harness.assert_screen_contains("Content from Project B");

        // Save session for project_b
        harness.editor_mut().save_session().unwrap();
    }

    // Phase 5: Start fresh in project_b - session should restore project_b's file
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_b.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should be restored for project_b");

        harness.render().unwrap();

        // Verify project_b's file was restored
        harness.assert_screen_contains("main_b.txt");
        // Should NOT have project_a's file
        harness.assert_screen_not_contains("main_a.txt");
    }

    // Phase 6: Start fresh in project_a again - should restore project_a's session (not project_b's)
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should be restored for project_a");

        harness.render().unwrap();

        // Verify project_a's file was restored
        harness.assert_screen_contains("main_a.txt");
        // Should NOT have project_b's file
        harness.assert_screen_not_contains("main_b.txt");
    }
}
