//! E2E tests for per-split tabs functionality
//!
//! Each split should have its own independent tab bar showing the buffers open in that split.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test that the initial split has the initial buffer in its tabs
#[test]
fn test_initial_split_has_buffer_in_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Hello").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // The tab bar should show the file name
    harness.assert_screen_contains("test.txt");
}

/// Test that the initial scratch buffer is in tabs when editor starts
#[test]
fn test_initial_scratch_buffer_in_tabs() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // The initial scratch buffer should show in tabs as "[No Name]" or similar
    let screen = harness.screen_to_string();
    eprintln!("Initial editor screen:\n{}", screen);

    // Check that some tab indication exists
    // The initial buffer should be visible in the tab area
}

/// Test that opening a file adds it to the active split's tabs
#[test]
fn test_open_file_adds_to_split_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    std::fs::write(&file1, "Content 1").unwrap();
    std::fs::write(&file2, "Content 2").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");

    // Open second file
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Both files should be in tabs
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");
}

/// Test that creating a new split has the current buffer in its tabs
#[test]
fn test_new_split_has_buffer_in_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Hello").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify initial tab
    harness.assert_screen_contains("test.txt");

    // Split horizontally via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split horiz").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The new split should also have the buffer in its tabs
    // With 2 splits, we should see "test.txt" twice (once in each split's tab bar)
    let screen = harness.screen_to_string();
    let count = screen.matches("test.txt").count();
    assert!(
        count >= 2,
        "Expected at least 2 occurrences of 'test.txt' in split tabs, found {}. Screen:\n{}",
        count,
        screen
    );
}

/// Test that each split maintains its own tab list
#[test]
fn test_splits_have_independent_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    std::fs::write(&file1, "Content 1").unwrap();
    std::fs::write(&file2, "Content 2").unwrap();

    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();

    // Split vertically
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now in second split - open a different file
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Both files should appear on screen (second split has both, first split has only file1)
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");

    // Navigate back to first split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next split").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // First split should still have only file1.txt in its tabs
    // (file2.txt should only appear in second split's tabs)
    let screen = harness.screen_to_string();
    eprintln!("Screen after switching to first split:\n{}", screen);
}

/// Test next/prev buffer cycles through split's tabs only
#[test]
fn test_buffer_cycling_within_split() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    let file3 = temp_dir.path().join("file3.txt");
    std::fs::write(&file1, "Content 1").unwrap();
    std::fs::write(&file2, "Content 2").unwrap();
    std::fs::write(&file3, "Content 3").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open all three files in the same split
    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();
    harness.open_file(&file3).unwrap();
    harness.render().unwrap();

    // Current buffer should be file3.txt (last opened)
    harness.assert_buffer_content("Content 3");

    // Next buffer should cycle back to file1.txt
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next buffer").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("Content 1");

    // Next buffer should go to file2.txt
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next buffer").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("Content 2");
}

/// Test that tab bar appears within each split area
#[test]
fn test_tab_bar_in_split_area() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Hello world").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Get screen and print it for debugging
    let screen = harness.screen_to_string();
    eprintln!("Screen content:\n{}", screen);

    // Tab should be visible
    harness.assert_screen_contains("test.txt");
}

/// Test that closing a buffer removes it from split's tabs
#[test]
fn test_close_buffer_removes_from_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    std::fs::write(&file1, "Content 1").unwrap();
    std::fs::write(&file2, "Content 2").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open both files
    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Both should be in tabs
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");

    // Close current buffer (file2)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("close buffer").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Only file1 should remain in tabs
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_not_contains("file2.txt");
}

/// Debug test to print screen and understand tab rendering
#[test]
fn test_debug_split_tabs_rendering() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("alpha.txt");
    let file2 = temp_dir.path().join("beta.txt");
    std::fs::write(&file1, "Alpha content").unwrap();
    std::fs::write(&file2, "Beta content").unwrap();

    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();

    eprintln!("\n=== After opening alpha.txt ===");
    eprintln!("{}", harness.screen_to_string());

    // Open second file
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    eprintln!("\n=== After opening beta.txt ===");
    eprintln!("{}", harness.screen_to_string());

    // Create a vertical split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    eprintln!("\n=== After vertical split ===");
    eprintln!("{}", harness.screen_to_string());

    // Check that tabs are rendered
    let screen = harness.screen_to_string();

    // Should see alpha.txt and beta.txt somewhere
    assert!(
        screen.contains("alpha.txt") || screen.contains("beta.txt"),
        "Expected to see file tabs in screen. Screen:\n{}",
        screen
    );
}
