//! Regression test for issue #1434: Saving a file within a non-existent directory
//! should not crash the editor.
//!
//! Steps to reproduce:
//! 1. Open a new file with Ctrl+O, type a path with a non-existent parent directory
//! 2. Type some content
//! 3. Press Ctrl+S to save
//! Expected: Prompt asking to create the directory
//! Actual (before fix): Editor crashes with "No such file or directory (os error 2)"

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that saving to a non-existent directory prompts the user and creates it on confirm.
#[test]
fn test_issue_1434_save_file_in_nonexistent_directory_confirm() {
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Path where parent directory does NOT exist, relative to project
    let nonexistent_path = project_dir.join("newdir").join("readme.md");

    // Create a new unnamed buffer
    harness.new_buffer().unwrap();

    // Type some content
    harness.type_text("hello world").unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("hello world");

    // Ctrl+S on unnamed buffer triggers save-as prompt
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Save as:");

    // Type a relative path with non-existent parent directory and press Enter
    harness.type_text("newdir/readme.md").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see the confirmation prompt about creating the directory
    harness.assert_screen_contains("does not exist");

    // Confirm creation by typing "c"
    harness.type_text("c").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The file should be saved successfully
    harness.wait_for_screen_contains("Saved").unwrap();

    // Verify the file was actually written to disk
    assert!(nonexistent_path.exists(), "File should exist on disk");
    assert_eq!(
        std::fs::read_to_string(&nonexistent_path).unwrap(),
        "hello world"
    );
}

/// Test that saving to a non-existent directory can be cancelled.
#[test]
fn test_issue_1434_save_file_in_nonexistent_directory_cancel() {
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let nonexistent_path = project_dir.join("newdir").join("readme.md");

    // Create a new unnamed buffer
    harness.new_buffer().unwrap();
    harness.type_text("hello world").unwrap();
    harness.render().unwrap();

    // Ctrl+S triggers save-as prompt
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type non-existent path and confirm
    harness.type_text("newdir/readme.md").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see the confirmation prompt
    harness.assert_screen_contains("does not exist");

    // Cancel by pressing Enter without typing "c" (default is abort)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show cancellation, file should NOT exist
    assert!(
        !nonexistent_path.exists(),
        "File should not exist after cancel"
    );
}
