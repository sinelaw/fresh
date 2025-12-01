use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test rendering of buffer with CRLF line endings
/// Modern editors should handle CRLF transparently without showing CR characters
#[test]
fn test_crlf_buffer_rendering() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_test.txt");

    // Create a test file with CRLF line endings (Windows-style)
    let content = "Line 1\r\nLine 2\r\nLine 3\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Modern editor behavior: CRLF should be handled transparently
    // Lines should be displayed cleanly without visible CR characters
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 2");
    harness.assert_screen_contains("Line 3");

    // Should NOT show CR as visible characters like <0D> or ^M in normal view
    assert!(
        !screen.contains("<0D>") && !screen.contains("^M"),
        "CRLF line endings should be handled transparently, not shown as visible characters"
    );

    // Should show filename in status bar
    harness.assert_screen_contains("crlf_test.txt");
}

/// Test rendering of buffer with mixed CRLF and LF line endings
/// Modern editors handle mixed line endings gracefully
#[test]
fn test_mixed_line_endings_rendering() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("mixed_endings.txt");

    // Create a test file with mixed line endings (CRLF and LF)
    let content = "CRLF line 1\r\nLF line 2\nCRLF line 3\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // All lines should be visible regardless of line ending type
    harness.assert_screen_contains("CRLF line 1");
    harness.assert_screen_contains("LF line 2");
    harness.assert_screen_contains("CRLF line 3");

    // CR characters should not be visible in normal view
    assert!(
        !screen.contains("<0D>") && !screen.contains("^M"),
        "Mixed line endings should be handled transparently"
    );
}

/// Test cursor movement in CRLF buffer
/// Cursor should move correctly through CRLF line breaks
#[test]
fn test_crlf_cursor_movement() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_cursor.txt");

    // Create a test file with CRLF line endings
    let content = "First\r\nSecond\r\nThird\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Verify we start at position 0
    let initial_pos = harness.cursor_position();
    assert_eq!(initial_pos, 0, "Should start at position 0");

    // Move cursor down through CRLF lines
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have moved past first line (First\r\n = 7 bytes)
    let pos_after_down = harness.cursor_position();
    assert!(pos_after_down > initial_pos, "Cursor should move forward");

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have moved past second line as well
    let pos_after_second_down = harness.cursor_position();
    assert!(
        pos_after_second_down > pos_after_down,
        "Cursor should continue moving forward"
    );
}

/// Test editing in CRLF buffer
/// Editing should work naturally with CRLF line endings
#[test]
fn test_crlf_editing() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_edit.txt");

    // Create a test file with CRLF line endings
    let content = "Hello\r\nWorld\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to end of first line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Type some text
    harness.type_text(" there").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Should see the edited text without visible CR characters
    harness.assert_screen_contains("Hello there");
    harness.assert_screen_contains("World");

    // Verify buffer content has the edit
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("Hello there"),
        "Buffer should contain edited text"
    );

    // CR should not be visible on screen
    assert!(
        !screen.contains("<0D>") && !screen.contains("^M"),
        "CR characters should not be visible during editing"
    );
}

/// Test creating new lines in CRLF buffer preserves CRLF format
/// When pressing Enter in a CRLF file, new lines should also use CRLF
#[test]
fn test_crlf_new_line_insertion() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_newline.txt");

    // Create a test file with CRLF line endings
    let content = "Line 1\r\nLine 3\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to end of first line and insert a new line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Line 2").unwrap();
    harness.render().unwrap();

    // Should see all three lines
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 2");
    harness.assert_screen_contains("Line 3");

    // Save the file using Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Read the file back and verify CRLF line endings are preserved
    let saved_content = std::fs::read_to_string(&file_path).unwrap();

    // All lines should have CRLF endings
    assert!(
        saved_content.contains("Line 1\r\n"),
        "Line 1 should have CRLF ending"
    );
    assert!(
        saved_content.contains("Line 2\r\n"),
        "Newly inserted Line 2 should have CRLF ending"
    );
    assert!(
        saved_content.contains("Line 3\r\n"),
        "Line 3 should have CRLF ending"
    );

    // Verify the file format was detected and preserved
    assert_eq!(
        saved_content, "Line 1\r\nLine 2\r\nLine 3\r\n",
        "File should maintain CRLF format throughout"
    );
}

/// Test that empty lines with CRLF are rendered correctly
#[test]
fn test_crlf_empty_lines() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_empty.txt");

    // Create a test file with empty lines (CRLF)
    let content = "Line 1\r\n\r\nLine 3\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should see both non-empty lines
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 3");

    // Navigate to verify empty line exists
    let initial_pos = harness.cursor_position();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Move to line 1 (empty)
    let pos_on_empty = harness.cursor_position();
    assert!(pos_on_empty > initial_pos, "Should move past first line");

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Move to line 2
    let pos_on_line3 = harness.cursor_position();
    assert!(pos_on_line3 > pos_on_empty, "Should move past empty line");
}

/// Test creating new lines in LF/Unix buffer preserves LF format
/// When pressing Enter in a LF file, new lines should also use LF
#[test]
fn test_lf_new_line_insertion() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("lf_newline.txt");

    // Create a test file with LF line endings (Unix-style)
    let content = "Line 1\nLine 3\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to end of first line and insert a new line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Line 2").unwrap();
    harness.render().unwrap();

    // Should see all three lines
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 2");
    harness.assert_screen_contains("Line 3");

    // Save the file using Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Read the file back and verify LF line endings are preserved
    let saved_content = std::fs::read_to_string(&file_path).unwrap();

    // All lines should have LF endings (not CRLF)
    assert!(
        saved_content.contains("Line 1\n"),
        "Line 1 should have LF ending"
    );
    assert!(
        saved_content.contains("Line 2\n"),
        "Newly inserted Line 2 should have LF ending"
    );
    assert!(
        saved_content.contains("Line 3\n"),
        "Line 3 should have LF ending"
    );

    // Verify no CRLF sequences were introduced
    assert!(
        !saved_content.contains("\r\n"),
        "Unix file should not contain CRLF sequences"
    );

    // Verify the file format was detected and preserved
    assert_eq!(
        saved_content, "Line 1\nLine 2\nLine 3\n",
        "File should maintain LF format throughout"
    );
}
