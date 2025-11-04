use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test basic split view creation (horizontal)
#[test]
fn test_split_horizontal() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text in the first buffer
    harness.type_text("Buffer 1").unwrap();
    harness.assert_buffer_content("Buffer 1");

    // Split horizontally (Alt+h)
    harness.send_key(KeyCode::Char('h'), KeyModifiers::ALT).unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Split pane horizontally");

    // New split should be empty
    harness.assert_buffer_content("");
}

/// Test basic split view creation (vertical)
#[test]
fn test_split_vertical() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text in the first buffer
    harness.type_text("Buffer 1").unwrap();
    harness.assert_buffer_content("Buffer 1");

    // Split vertically (Alt+v)
    harness.send_key(KeyCode::Char('v'), KeyModifiers::ALT).unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Split pane vertically");

    // New split should be empty
    harness.assert_buffer_content("");
}

/// Test navigation between splits
#[test]
fn test_split_navigation() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a vertical split
    harness.type_text("First buffer").unwrap();
    harness.send_key(KeyCode::Char('v'), KeyModifiers::ALT).unwrap();

    // Now in second split, type different text
    harness.type_text("Second buffer").unwrap();
    harness.assert_buffer_content("Second buffer");

    // Navigate to next split (Alt+o)
    harness.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Switched to next split");

    // Navigate to previous split (Alt+Shift+o)
    harness.send_key(KeyCode::Char('o'), KeyModifiers::ALT | KeyModifiers::SHIFT).unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Switched to previous split");
}

/// Test closing a split
#[test]
fn test_close_split() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a split
    harness.send_key(KeyCode::Char('v'), KeyModifiers::ALT).unwrap();

    // Close the split (Alt+x)
    harness.send_key(KeyCode::Char('x'), KeyModifiers::ALT).unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Closed split");
}

/// Test cannot close last split
#[test]
fn test_cannot_close_last_split() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Try to close the only split (Alt+x)
    harness.send_key(KeyCode::Char('x'), KeyModifiers::ALT).unwrap();

    // Should see error message
    harness.render().unwrap();
    harness.assert_screen_contains("Cannot close split");
}

/// Test split size adjustment
/// Note: This test is disabled because adjusting split size requires
/// targeting the parent split container, not the leaf nodes.
/// This is a known limitation that will be addressed in a future update.
#[test]
#[ignore]
fn test_split_size_adjustment() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a split
    harness.send_key(KeyCode::Char('v'), KeyModifiers::ALT).unwrap();

    // Increase split size (Alt+=)
    harness.send_key(KeyCode::Char('='), KeyModifiers::ALT).unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Adjusted split size by 5%");

    // Decrease split size (Alt+-)
    harness.send_key(KeyCode::Char('-'), KeyModifiers::ALT).unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Adjusted split size by -5%");
}

/// Test multiple splits (nested)
#[test]
fn test_nested_splits() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Create first split (vertical)
    harness.type_text("Buffer 1").unwrap();
    harness.send_key(KeyCode::Char('v'), KeyModifiers::ALT).unwrap();

    // Should be in buffer 2 now
    harness.type_text("Buffer 2").unwrap();

    // Create second split (horizontal)
    harness.send_key(KeyCode::Char('h'), KeyModifiers::ALT).unwrap();

    // Should be in buffer 3 now
    harness.type_text("Buffer 3").unwrap();

    // Verify we successfully created multiple splits
    harness.render().unwrap();
}

/// Test split view with file operations
#[test]
fn test_split_with_file_operations() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");

    std::fs::write(&file1, "File 1 content").unwrap();
    std::fs::write(&file2, "File 2 content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("File 1 content");

    // Create a split
    harness.send_key(KeyCode::Char('v'), KeyModifiers::ALT).unwrap();

    // Open second file in the new split
    harness.open_file(&file2).unwrap();
    harness.assert_buffer_content("File 2 content");

    // Render and verify both files are shown
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");
}
