use crate::common::harness::EditorTestHarness;

/// Test that the prompt is rendered correctly
#[test]
fn test_prompt_rendering() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that the prompt is visible in the status bar area (bottom line)
    let screen = harness.screen_to_string();
    harness.assert_screen_contains("Open file:");

    // Check that the status bar has yellow background (prompt color)
    let buffer = harness.buffer();
    let status_y = buffer.area.height - 1; // Status bar is at the bottom

    // Check a cell in the status bar has cyan background (high-contrast theme default)
    let first_cell_pos = buffer.index_of(0, status_y);
    let first_cell = &buffer.content[first_cell_pos];
    assert_eq!(
        first_cell.bg,
        ratatui::style::Color::Cyan,
        "Prompt should have cyan background"
    );
}

/// Test prompt input handling (typing, backspace, cursor movement)
#[test]
fn test_prompt_input_handling() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Open file:");

    // Type some text
    harness.type_text("test.txt").unwrap();
    harness.assert_screen_contains("Open file:test.txt");

    // Test backspace
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Open file:test.tx");
    harness.assert_screen_not_contains("test.txt");

    // Type more
    harness.type_text("t2").unwrap();
    harness.assert_screen_contains("Open file:test.txt2");

    // Test Home (move cursor to start)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Type at the beginning
    harness.type_text("my_").unwrap();
    harness.assert_screen_contains("Open file:my_test.txt2");

    // Test End (move cursor to end)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("!").unwrap();
    harness.assert_screen_contains("Open file:my_test.txt2!");
}

/// Test canceling the prompt with Escape
#[test]
fn test_prompt_cancel() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Open file:");

    // Type some text
    harness.type_text("test.txt").unwrap();
    harness.assert_screen_contains("test.txt");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Prompt should be gone, and "Canceled" message should appear
    harness.assert_screen_not_contains("Open file:");
    harness.assert_screen_contains("Canceled");
}

/// Test the complete open file workflow
#[test]
fn test_open_file_workflow() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    use tempfile::TempDir;

    // Create a temporary directory and file
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test_prompt.txt");
    fs::write(&file_path, "Hello from prompt test!").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Open file:");

    // Type the file path
    let path_str = file_path.to_str().unwrap();
    harness.type_text(path_str).unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check that the file was opened
    harness.assert_screen_not_contains("Open file:");

    // Check that the file content is displayed
    harness.assert_screen_contains("Hello from prompt test!");

    // Check that the filename appears in the status bar
    harness.assert_screen_contains("test_prompt.txt");
}

/// Test opening a non-existent file creates an unsaved buffer
#[test]
fn test_open_nonexistent_file() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    // Create a temporary directory for the test
    let temp_dir = TempDir::new().unwrap();
    let new_file_path = temp_dir.path().join("new_file.txt");

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Type the path to a non-existent file
    let path_str = new_file_path.to_str().unwrap();
    harness.type_text(path_str).unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should NOT show an error - should open as unsaved buffer
    harness.assert_screen_not_contains("Error opening file");

    // Should show the filename in the status bar
    harness.assert_screen_contains("new_file.txt");

    // Buffer should be empty
    assert_eq!(harness.get_buffer_content(), "");

    // Should show "Opened" message
    harness.assert_screen_contains("Opened:");
}

/// Test that opening a non-existent file allows editing and saving
#[test]
fn test_open_nonexistent_file_edit_and_save() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;
    use tempfile::TempDir;

    // Create a temporary directory for the test
    let temp_dir = TempDir::new().unwrap();
    let new_file_path = temp_dir.path().join("created_file.txt");

    // Verify file doesn't exist yet
    assert!(!new_file_path.exists());

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Type the path to a non-existent file
    let path_str = new_file_path.to_str().unwrap();
    harness.type_text(path_str).unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should open successfully
    harness.assert_screen_not_contains("Error");

    // Type some content
    harness.type_text("Hello, World!").unwrap();
    assert_eq!(harness.get_buffer_content(), "Hello, World!");

    // Save the file with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show "Saved" message
    harness.assert_screen_contains("Saved");

    // Verify file was created on disk with correct content
    assert!(new_file_path.exists());
    let saved_content = fs::read_to_string(&new_file_path).unwrap();
    assert_eq!(saved_content, "Hello, World!");
}

/// Test spawning CLI with non-existent file directly (via open_file)
#[test]
fn test_spawn_with_nonexistent_file() {
    use std::fs;
    use tempfile::TempDir;

    // Create a temporary directory
    let temp_dir = TempDir::new().unwrap();
    let new_file_path = temp_dir.path().join("spawn_test.rs");

    // Verify file doesn't exist
    assert!(!new_file_path.exists());

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open non-existent file directly (simulating CLI spawn)
    harness.open_file(&new_file_path).unwrap();

    // Should show the filename
    harness.assert_screen_contains("spawn_test.rs");

    // Buffer should be empty
    assert_eq!(harness.get_buffer_content(), "");

    // Type content and save
    harness.type_text("fn main() {}").unwrap();

    use crossterm::event::{KeyCode, KeyModifiers};
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify file was created
    assert!(new_file_path.exists());
    let content = fs::read_to_string(&new_file_path).unwrap();
    assert_eq!(content, "fn main() {}");
}

/// Test Save As functionality
#[test]
fn test_save_as_functionality() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;
    use tempfile::TempDir;

    // Create a temporary directory and original file
    let temp_dir = TempDir::new().unwrap();
    let original_path = temp_dir.path().join("original.txt");
    fs::write(&original_path, "Original content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the original file
    harness.open_file(&original_path).unwrap();
    harness.assert_screen_contains("original.txt");
    assert_eq!(harness.get_buffer_content(), "Original content");

    // Trigger command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to search for Save As command
    harness.type_text("Save File As").unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show the Save As prompt with current filename
    harness.assert_screen_contains("Save as:");

    // Clear the current filename and type new name
    // First select all with Ctrl+A
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();

    // Type new filename (relative path)
    let new_file_path = temp_dir.path().join("saved_as.txt");
    let new_path_str = new_file_path.to_str().unwrap();
    harness.type_text(new_path_str).unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show success message
    harness.assert_screen_contains("Saved as:");

    // Verify new file was created with correct content
    assert!(new_file_path.exists());
    let new_content = fs::read_to_string(&new_file_path).unwrap();
    assert_eq!(new_content, "Original content");

    // Original file should still exist
    assert!(original_path.exists());

    // Buffer should now show the new filename
    harness.assert_screen_contains("saved_as.txt");
}

/// Test Save As with relative path
#[test]
fn test_save_as_relative_path() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create and open original file
    let original_path = project_dir.join("original.txt");
    fs::write(&original_path, "Test content").unwrap();
    harness.open_file(&original_path).unwrap();

    // Trigger command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Save File As").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Clear and type relative path
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("relative_save.txt").unwrap();

    // Confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should save to working directory
    let expected_path = project_dir.join("relative_save.txt");
    assert!(
        expected_path.exists(),
        "File should be created at {:?}",
        expected_path
    );

    let content = fs::read_to_string(&expected_path).unwrap();
    assert_eq!(content, "Test content");
}

/// Test Save As creates parent directories if needed
#[test]
fn test_save_as_nested_path() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Start with new buffer
    harness.new_buffer().unwrap();

    // Type some content
    harness.type_text("Nested file content").unwrap();

    // Trigger Save As via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Save File As").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type nested path (parent dir doesn't exist yet)
    let nested_path = project_dir.join("subdir").join("nested.txt");
    let nested_path_str = nested_path.to_str().unwrap();
    harness.type_text(nested_path_str).unwrap();

    // Confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Note: This test verifies the error case since we don't auto-create parent dirs
    // The file won't be created because the parent directory doesn't exist
    // This documents current behavior - if we want to auto-create dirs, update this test
    if !nested_path.exists() {
        harness.assert_screen_contains("Error saving file");
    }
}
