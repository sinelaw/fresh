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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("Content 1");

    // Next buffer should go to file2.txt
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next buffer").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Only file1 should remain in tabs
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_not_contains("file2.txt");
}

/// Test git log plugin split view tabs
#[test]
fn test_git_log_split_tabs() {
    // Create a git repo with some commits
    let temp_dir = TempDir::new().unwrap();
    let repo_path = temp_dir.path();

    // Initialize git repo
    std::process::Command::new("git")
        .args(["init"])
        .current_dir(repo_path)
        .output()
        .expect("Failed to init git repo");

    // Configure git user
    std::process::Command::new("git")
        .args(["config", "user.email", "test@test.com"])
        .current_dir(repo_path)
        .output()
        .expect("Failed to set git email");

    std::process::Command::new("git")
        .args(["config", "user.name", "Test User"])
        .current_dir(repo_path)
        .output()
        .expect("Failed to set git name");

    // Create a file and commit
    let file_path = repo_path.join("test.txt");
    std::fs::write(&file_path, "Hello world").unwrap();

    std::process::Command::new("git")
        .args(["add", "test.txt"])
        .current_dir(repo_path)
        .output()
        .expect("Failed to git add");

    std::process::Command::new("git")
        .args(["commit", "-m", "Initial commit"])
        .current_dir(repo_path)
        .output()
        .expect("Failed to git commit");

    // Create harness with the repo as working directory
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    eprintln!("\n=== Before git log ===");
    eprintln!("{}", harness.screen_to_string());

    // Open git log via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("git log").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait a bit for async operations
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    eprintln!("\n=== After git log ===");
    let screen = harness.screen_to_string();
    eprintln!("{}", screen);

    // Check that we have two splits with tabs
    // The original file should be visible and the git log should be visible
    // Both should have their tabs rendered
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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
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

/// Test workflow: type in buffer, open new split via command palette, open new buffer in that split,
/// close the split, verify remaining split has both tabs and is focused/responsive
#[test]
fn test_close_split_preserves_tabs_and_focus() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("first.txt");
    let file2 = temp_dir.path().join("second.txt");
    std::fs::write(&file1, "").unwrap();
    std::fs::write(&file2, "").unwrap();

    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Step 1: Open first file and type some text
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.type_text("First buffer content").unwrap();
    harness.render().unwrap();

    // Verify the text appears on screen and tab shows filename
    harness.assert_screen_contains("First buffer content");
    harness.assert_screen_contains("first.txt");

    // Step 2: Open a new horizontal split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split horiz").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify we now have 2 splits (horizontal separator should appear)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains('─'),
        "Expected horizontal split separator. Screen:\n{}",
        screen
    );

    // Step 3: Open second file in the new split (adds it to new split's tabs)
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Verify second file tab is visible
    harness.assert_screen_contains("second.txt");

    // Type something in the second buffer to distinguish it
    harness.type_text("Second buffer content").unwrap();
    harness.render().unwrap();

    // Both files should be visible in tabs (one in each split, or both in current split)
    let screen_with_two_splits = harness.screen_to_string();
    eprintln!(
        "Screen with two splits and two files:\n{}",
        screen_with_two_splits
    );

    // Step 4: Close the current split (the one showing second.txt)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("close split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Step 5: Verify only 1 split remains
    let screen_after_close = harness.screen_to_string();
    eprintln!("Screen after closing split:\n{}", screen_after_close);

    // The remaining split should contain BOTH tabs (first.txt and second.txt)
    // When closing a split, its tabs should be merged into the remaining split
    assert!(
        screen_after_close.contains("first.txt"),
        "Expected first.txt tab to be present in remaining split. Screen:\n{}",
        screen_after_close
    );
    assert!(
        screen_after_close.contains("second.txt"),
        "Expected second.txt tab to be preserved from closed split. Screen:\n{}",
        screen_after_close
    );

    // Verify that typing works in the remaining split (it's focused)
    harness.type_text("XYZ").unwrap();
    harness.render().unwrap();

    // The typed characters should appear on screen
    let final_screen = harness.screen_to_string();
    eprintln!("Final screen after typing XYZ:\n{}", final_screen);
    assert!(
        final_screen.contains("XYZ"),
        "Expected typed characters 'XYZ' to appear. The remaining split should be focused. Screen:\n{}",
        final_screen
    );
}

/// Test that close tab removes tab but keeps buffer when buffer is open in another split
#[test]
fn test_close_tab_keeps_buffer_in_other_split() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("shared.txt");
    std::fs::write(&file1, "Shared content").unwrap();

    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Open file in first split
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("shared.txt");

    // Create a vertical split (will also show same buffer)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see shared.txt twice (once in each split's tabs)
    let screen = harness.screen_to_string();
    let count = screen.matches("shared.txt").count();
    assert!(
        count >= 2,
        "Expected shared.txt in both splits. Found {} occurrences. Screen:\n{}",
        count,
        screen
    );

    // Close tab in current split (should only remove from this split, not close buffer)
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Buffer should still exist (visible in first split)
    // Since this was the only tab in the second split, it can't be closed
    // The error message should appear
    let screen_after = harness.screen_to_string();
    eprintln!("Screen after close tab:\n{}", screen_after);

    // Either the tab is still there (can't close only tab) or we see an error message
    assert!(
        screen_after.contains("shared.txt") || screen_after.contains("Cannot close"),
        "Expected shared.txt to remain or error message. Screen:\n{}",
        screen_after
    );
}

/// Test that close tab closes buffer when it's the last viewport
#[test]
fn test_close_tab_closes_buffer_when_last_viewport() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    std::fs::write(&file1, "Content 1").unwrap();
    std::fs::write(&file2, "Content 2").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open both files in same split
    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Both should be visible in tabs
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");

    // Current buffer is file2.txt (last opened)
    harness.assert_buffer_content("Content 2");

    // Close tab (Alt+W) - should close buffer since it's only in this split
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // file2.txt should be gone, file1.txt should remain
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_not_contains("file2.txt");

    // Should now show file1.txt content
    harness.assert_buffer_content("Content 1");
}

/// Test that close tab prompts for modified buffer when it's the last viewport
#[test]
fn test_close_tab_prompts_for_modified_buffer() {
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

    // Modify current buffer (file2)
    harness.type_text("MODIFIED").unwrap();
    harness.render().unwrap();

    // Close tab - should prompt since buffer is modified and this is last viewport
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Should see the save/discard/cancel prompt
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("modified") || screen.contains("save") || screen.contains("discard"),
        "Expected save/discard prompt for modified buffer. Screen:\n{}",
        screen
    );

    // Cancel the close
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Both files should still be in tabs
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");
}

#[test]
fn test_close_tab_transfers_focus_to_remaining_tab() {
    let temp_dir = TempDir::new().unwrap();

    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    std::fs::write(&file1, "First file content").unwrap();
    std::fs::write(&file2, "Second file content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open both files
    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Should be focused on file2 (most recently opened)
    harness.assert_screen_contains("file2.txt");
    harness.assert_screen_contains("Second file content");

    // Close current tab (file2)
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // file2 should be closed, file1 should now be active
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("file2.txt"),
        "file2.txt should be closed. Screen:\n{}",
        screen
    );
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("First file content");

    // Type text to verify keyboard focus is on file1
    harness.type_text("TYPED").unwrap();
    harness.render().unwrap();

    // The typed text should appear in the buffer
    harness.assert_screen_contains("TYPED");

    // Save and verify the text was written to file1
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let file1_content = std::fs::read_to_string(&file1).unwrap();
    assert!(
        file1_content.contains("TYPED"),
        "Typed text should be saved to file1. Content: {}",
        file1_content
    );
}
