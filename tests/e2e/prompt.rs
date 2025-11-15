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
    harness.assert_screen_contains("Find file: ");

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
    harness.assert_screen_contains("Find file: ");

    // Type some text
    harness.type_text("test.txt").unwrap();
    harness.assert_screen_contains("Find file: test.txt");

    // Test backspace
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Find file: test.tx");
    harness.assert_screen_not_contains("test.txt");

    // Type more
    harness.type_text("t2").unwrap();
    harness.assert_screen_contains("Find file: test.txt2");

    // Test Home (move cursor to start)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Type at the beginning
    harness.type_text("my_").unwrap();
    harness.assert_screen_contains("Find file: my_test.txt2");

    // Test End (move cursor to end)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("!").unwrap();
    harness.assert_screen_contains("Find file: my_test.txt2!");
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
    harness.assert_screen_contains("Find file: ");

    // Type some text
    harness.type_text("test.txt").unwrap();
    harness.assert_screen_contains("test.txt");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Prompt should be gone, and "Canceled" message should appear
    harness.assert_screen_not_contains("Find file: ");
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
    harness.assert_screen_contains("Find file: ");

    // Type the file path
    let path_str = file_path.to_str().unwrap();
    harness.type_text(path_str).unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check that the file was opened
    harness.assert_screen_not_contains("Find file: ");

    // Check that the file content is displayed
    harness.assert_screen_contains("Hello from prompt test!");

    // Check that the filename appears in the status bar
    harness.assert_screen_contains("test_prompt.txt");
}

/// Test opening a non-existent file shows an error
#[test]
fn test_open_nonexistent_file() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Type a non-existent file path
    harness.type_text("/nonexistent/file/path.txt").unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show an error message
    harness.assert_screen_contains("Error opening file");
}
