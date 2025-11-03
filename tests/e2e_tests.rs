// End-to-end tests - testing complete user workflows

mod common;

use common::harness::EditorTestHarness;
use tempfile::TempDir;

/// Test basic file creation and editing workflow
#[test]
fn test_basic_editing_workflow() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // New buffer should be empty
    harness.assert_buffer_content("");

    // Status bar should show "[No Name]"
    harness.render().unwrap();
    harness.assert_screen_contains("[No Name]");

    // TODO: When action_to_events() is implemented, we can simulate typing:
    // harness.type_text("Hello, World!").unwrap();
    // harness.assert_buffer_content("Hello, World!");
}

/// Test file open and save workflow
#[test]
fn test_file_open_save_workflow() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with some content
    std::fs::write(&file_path, "Initial content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file
    harness.open_file(&file_path).unwrap();

    // Should display the filename
    harness.render().unwrap();
    harness.assert_screen_contains("test.txt");

    // Should show the file content in the buffer
    harness.assert_buffer_content("Initial content");

    // TODO: When action_to_events() is implemented:
    // - Edit the file
    // - Save it
    // - Verify the file on disk has the new content
}

/// Test multi-buffer workflow
#[test]
fn test_multi_buffer_workflow() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");

    std::fs::write(&file1, "File 1 content").unwrap();
    std::fs::write(&file2, "File 2 content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("File 1 content");

    // Open second file
    harness.open_file(&file2).unwrap();
    harness.assert_buffer_content("File 2 content");

    // Should show tabs for both files
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");

    // TODO: When action_to_events() is implemented:
    // - Switch between buffers
    // - Edit both files
    // - Verify buffer switching works correctly
}

/// Test rendering of empty buffer
#[test]
fn test_empty_buffer_rendering() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Should have some output (status bar, etc.)
    assert!(!screen.is_empty());

    // Should show empty buffer indicator
    harness.assert_screen_contains("[No Name]");
}

/// Test rendering of file with content
#[test]
fn test_file_content_rendering() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("render_test.txt");

    // Create a test file with multiple lines
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should show file content on screen
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 2");
    harness.assert_screen_contains("Line 3");

    // Should show filename in status bar
    harness.assert_screen_contains("render_test.txt");
}

/// Test that editor doesn't quit prematurely
#[test]
fn test_editor_lifecycle() {
    let harness = EditorTestHarness::new(80, 24).unwrap();

    // New editor should not want to quit
    assert!(!harness.should_quit());

    // TODO: When action_to_events() is implemented:
    // - Send quit command
    // - Verify should_quit() returns true
}

/// Test viewport scrolling with large file
#[test]
fn test_large_file_viewport() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large.txt");

    // Create a file with many lines (more than viewport height)
    let mut content = String::new();
    for i in 0..100 {
        content.push_str(&format!("Line {}\n", i));
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should show first few lines
    harness.assert_screen_contains("Line 0");
    harness.assert_screen_contains("Line 1");

    // Should NOT show lines beyond viewport
    harness.assert_screen_not_contains("Line 50");

    // TODO: When action_to_events() is implemented:
    // - Scroll down
    // - Verify different lines are visible
}

/// Test typing characters and cursor movement
#[test]
fn test_typing_and_cursor_movement() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Initial state: empty buffer, cursor at position 0
    harness.assert_buffer_content("");
    assert_eq!(harness.cursor_position(), 0);

    // Type "Hello"
    harness.type_text("Hello").unwrap();

    // Buffer should contain "Hello"
    harness.assert_buffer_content("Hello");

    // Cursor should be at position 5 (after "Hello")
    assert_eq!(harness.cursor_position(), 5);

    // Type a space
    harness.type_text(" ").unwrap();
    harness.assert_buffer_content("Hello ");
    assert_eq!(harness.cursor_position(), 6);

    // Type "World!"
    harness.type_text("World!").unwrap();
    harness.assert_buffer_content("Hello World!");
    assert_eq!(harness.cursor_position(), 12);

    // Press Enter to create a new line
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.assert_buffer_content("Hello World!\n");
    assert_eq!(harness.cursor_position(), 13); // After newline

    // Type on second line
    harness.type_text("Second line").unwrap();
    harness.assert_buffer_content("Hello World!\nSecond line");
    assert_eq!(harness.cursor_position(), 24); // 13 + 11

    // Test backspace
    harness.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();
    harness.assert_buffer_content("Hello World!\nSecond lin");
    assert_eq!(harness.cursor_position(), 23);

    // Test cursor movement - move left
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 22);

    // Type while cursor is in the middle
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("Hello World!\nSecond liXn");
    assert_eq!(harness.cursor_position(), 23); // After X

    // Move to start of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 13); // Start of "Second liXn"

    // Move to end of line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 24); // End of "Second liXn"
}

/// Test multi-line editing and navigation
#[test]
fn test_multiline_editing() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines
    harness.type_text("Line 1").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("Line 2").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("Line 3").unwrap();

    harness.assert_buffer_content("Line 1\nLine 2\nLine 3");

    // Cursor should be at end of Line 3
    assert_eq!(harness.cursor_position(), 20); // "Line 1\n" (7) + "Line 2\n" (7) + "Line 3" (6)

    // Move up to Line 2
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 13); // End of Line 2

    // Move up to Line 1
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 6); // End of Line 1

    // Move down to Line 2
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 13); // End of Line 2

    // Move to start of Line 2
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 7); // Start of Line 2 (after "Line 1\n")

    // Type at start of Line 2
    harness.type_text(">>> ").unwrap();
    harness.assert_buffer_content("Line 1\n>>> Line 2\nLine 3");
}
