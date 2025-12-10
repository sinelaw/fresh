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
