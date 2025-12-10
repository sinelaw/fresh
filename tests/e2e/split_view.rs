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

    // Split horizontally via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split horiz").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Split pane horizontally");

    // New split should show the same buffer content (Emacs-style)
    harness.assert_buffer_content("Buffer 1");
}

/// Test basic split view creation (vertical)
#[test]
fn test_split_vertical() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text in the first buffer
    harness.type_text("Buffer 1").unwrap();
    harness.assert_buffer_content("Buffer 1");

    // Split vertically via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Split pane vertically");

    // New split should show the same buffer content (Emacs-style)
    harness.assert_buffer_content("Buffer 1");
}

/// Test navigation between splits
#[test]
fn test_split_navigation() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a vertical split via command palette
    harness.type_text("First buffer").unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now in second split - it shows the same buffer content (Emacs-style)
    // The cursor in the new split starts at position 0
    harness.assert_buffer_content("First buffer");

    // Move cursor to end and type more text
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" - extended").unwrap();
    harness.assert_buffer_content("First buffer - extended");

    // Navigate to next split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Switched to next split");

    // Navigate to previous split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Switched to previous split");
}

/// Test closing a split
#[test]
fn test_close_split() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Close the split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("close split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Closed split");
}

/// Test cannot close last split
#[test]
fn test_cannot_close_last_split() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Try to close the only split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("close split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see error message
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
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();

    // Increase split size (Alt+=)
    harness
        .send_key(KeyCode::Char('='), KeyModifiers::ALT)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Adjusted split size by 5%");

    // Decrease split size (Alt+-)
    harness
        .send_key(KeyCode::Char('-'), KeyModifiers::ALT)
        .unwrap();

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
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();

    // Should be in buffer 2 now
    harness.type_text("Buffer 2").unwrap();

    // Create second split (horizontal)
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::ALT)
        .unwrap();

    // Should be in buffer 3 now
    harness.type_text("Buffer 3").unwrap();

    // Verify we successfully created multiple splits
    harness.render().unwrap();
}

/// Test split view with file operations
#[test]
fn test_split_with_file_operations() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file1 = project_dir.join("file1.txt");
    let file2 = project_dir.join("file2.txt");

    std::fs::write(&file1, "File 1 content").unwrap();
    std::fs::write(&file2, "File 2 content").unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("File 1 content");

    // Create a split
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();

    // Open second file in the new split
    harness.open_file(&file2).unwrap();
    harness.assert_buffer_content("File 2 content");

    // Render and verify both files are shown
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");
}

/// Test toggle maximize split via command palette (maximize)
#[test]
fn test_toggle_maximize_split() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type in first buffer
    harness.type_text("Buffer 1").unwrap();

    // Create vertical split via command palette (like test_split_horizontal)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Both splits should show "Buffer 1" (Emacs-style)
    harness.assert_screen_contains("Split pane vertically");

    // Toggle maximize the current split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("togmax").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Maximized split");
}

/// Test toggle maximize split to unmaximize via command palette
#[test]
fn test_toggle_unmaximize_split() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file1 = project_dir.join("file1.txt");
    let file2 = project_dir.join("file2.txt");

    std::fs::write(&file1, "File 1 content").unwrap();
    std::fs::write(&file2, "File 2 content").unwrap();

    // Open first file and create a split via command palette
    harness.open_file(&file1).unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open second file in the new split
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Toggle maximize the current split (first toggle = maximize)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("togmax").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Toggle again to unmaximize (second toggle = unmaximize)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("togmax").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see status message
    harness.assert_screen_contains("Restored all splits");

    // Both files should be visible again
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");
}

/// Test cannot toggle maximize when only one split exists
#[test]
fn test_cannot_toggle_maximize_single_split() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Try to toggle maximize the only split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("togmax").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see error message (may be truncated in status bar)
    harness.assert_screen_contains("Cannot maximize");
}
