//! E2E tests for the native file browser (Open File dialog)
//!
//! Tests the plugin-free file browser that appears when pressing Ctrl+O.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use tempfile::TempDir;

/// Test that the file browser popup appears when opening the Open File prompt
#[test]
fn test_file_browser_popup_appears() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create some test files
    fs::write(project_root.join("file1.txt"), "content1").unwrap();
    fs::write(project_root.join("file2.txt"), "content2").unwrap();
    fs::create_dir(project_root.join("subdir")).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Trigger Open File with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for async directory loading
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Should see the file browser with navigation section
            screen.contains("Navigation:")
        })
        .expect("File browser popup should appear");

    let screen = harness.screen_to_string();

    // Should show the prompt
    assert!(screen.contains("Open file:"), "Prompt should be visible");

    // Should show navigation shortcuts
    assert!(
        screen.contains("..") || screen.contains("Navigation"),
        "Navigation section should be visible"
    );
}

/// Test that files are listed in the file browser
#[test]
fn test_file_browser_lists_files() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create test files with unique names
    fs::write(project_root.join("alpha_test.txt"), "alpha").unwrap();
    fs::write(project_root.join("beta_test.txt"), "beta").unwrap();
    fs::create_dir(project_root.join("gamma_dir")).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("alpha_test.txt")
        })
        .expect("Files should be listed");

    let screen = harness.screen_to_string();

    // Should show all files
    assert!(screen.contains("alpha_test.txt"), "alpha file should be listed");
    assert!(screen.contains("beta_test.txt"), "beta file should be listed");

    // Directories should have a trailing slash indicator
    assert!(
        screen.contains("gamma_dir") || screen.contains("/gamma_dir"),
        "directory should be listed"
    );
}

/// Test navigation with arrow keys
#[test]
fn test_file_browser_arrow_navigation() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create test files (sorted alphabetically)
    fs::write(project_root.join("aaa.txt"), "a").unwrap();
    fs::write(project_root.join("bbb.txt"), "b").unwrap();
    fs::write(project_root.join("ccc.txt"), "c").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| h.screen_to_string().contains("aaa.txt"))
        .expect("Files should load");

    // Move down twice
    harness
        .send_key(KeyCode::Down, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Move back up
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The test passes if no crash occurs and we can still see the files
    let screen = harness.screen_to_string();
    assert!(screen.contains("aaa.txt"));
    assert!(screen.contains("bbb.txt"));
    assert!(screen.contains("ccc.txt"));
}

/// Test filtering files by typing
#[test]
fn test_file_browser_filter() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("apple.txt"), "apple").unwrap();
    fs::write(project_root.join("banana.txt"), "banana").unwrap();
    fs::write(project_root.join("apricot.txt"), "apricot").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| h.screen_to_string().contains("apple.txt"))
        .expect("Files should load");

    // Type filter text
    harness.type_text("ap").unwrap();

    // Give it time to filter
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // "apple" and "apricot" match "ap", "banana" doesn't
    // Matching files should still be visible
    assert!(
        screen.contains("apple.txt"),
        "apple.txt should match filter 'ap'"
    );
    assert!(
        screen.contains("apricot.txt"),
        "apricot.txt should match filter 'ap'"
    );

    // Non-matching file should be grayed out (still visible but at bottom)
    // We can't easily test for gray styling, but the file should still be present
    assert!(
        screen.contains("banana.txt"),
        "banana.txt should still be visible (grayed out)"
    );
}

/// Test opening a file by pressing Enter
#[test]
fn test_file_browser_open_file() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("target.txt"), "Target file content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for file to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("target.txt"))
        .expect("File should be listed");

    // Press Enter to open the selected file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // File browser should close and file should be opened
    harness.assert_screen_not_contains("Navigation:");

    // File content should be visible
    harness.assert_screen_contains("Target file content");
}

/// Test navigating into a directory
#[test]
fn test_file_browser_navigate_directory() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create nested structure
    let subdir = project_root.join("subdir");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("nested.txt"), "nested content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for subdir to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("subdir"))
        .expect("Subdir should be listed");

    // Press Enter to navigate into subdirectory
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for nested file to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("nested.txt"))
        .expect("Should navigate into subdir and show nested.txt");

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("nested.txt"),
        "Should show nested file after navigating into directory"
    );
}

/// Test canceling with Escape
#[test]
fn test_file_browser_cancel() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("test.txt"), "test").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for popup
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("File browser should appear");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // File browser should be closed
    harness.assert_screen_not_contains("Navigation:");
    harness.assert_screen_contains("Canceled");
}

/// Test that column headers are shown (Name, Size, Modified)
#[test]
fn test_file_browser_column_headers() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("test.txt"), "test content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for file browser
    harness
        .wait_until(|h| h.screen_to_string().contains("Name"))
        .expect("Column headers should appear");

    let screen = harness.screen_to_string();

    // Should show column headers
    assert!(screen.contains("Name"), "Name column header should be visible");
    assert!(screen.contains("Size"), "Size column header should be visible");
    assert!(
        screen.contains("Modified"),
        "Modified column header should be visible"
    );
}

/// Test that hidden files are not shown by default
#[test]
fn test_file_browser_hides_dotfiles() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("visible.txt"), "visible").unwrap();
    fs::write(project_root.join(".hidden"), "hidden").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for visible file
    harness
        .wait_until(|h| h.screen_to_string().contains("visible.txt"))
        .expect("Visible file should appear");

    let screen = harness.screen_to_string();

    // Visible file should be shown
    assert!(screen.contains("visible.txt"));

    // Hidden file should NOT be shown by default
    assert!(
        !screen.contains(".hidden"),
        "Hidden files should not be shown by default"
    );
}

/// Test backspace goes to parent directory when filter is empty
#[test]
fn test_file_browser_backspace_parent() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create nested structure
    let subdir = project_root.join("subdir");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("child.txt"), "child").unwrap();
    fs::write(project_root.join("parent.txt"), "parent").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        subdir.clone(), // Start in subdir
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for child file
    harness
        .wait_until(|h| h.screen_to_string().contains("child.txt"))
        .expect("Should start in subdir");

    // Press backspace to go to parent
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // Wait for parent directory contents
    harness
        .wait_until(|h| h.screen_to_string().contains("parent.txt"))
        .expect("Should navigate to parent and show parent.txt");
}

/// Test that the file browser is native (doesn't depend on plugin hooks)
/// The native implementation loads files directly via FsManager, not plugins.
#[test]
fn test_file_browser_is_native_implementation() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("native_test.txt"), "content").unwrap();

    // Even with default config, the file browser should work natively
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Config::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // File browser should work - this tests the native implementation
    harness
        .wait_until(|h| h.screen_to_string().contains("native_test.txt"))
        .expect("Native file browser should work");

    let screen = harness.screen_to_string();
    assert!(screen.contains("Navigation:"), "File browser popup should appear");
    assert!(screen.contains("native_test.txt"), "Files should be listed");
}
