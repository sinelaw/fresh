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

/// Test backspace at beginning of line in CRLF buffer
/// Should delete the entire \r\n sequence, joining lines
#[test]
fn test_crlf_backspace_at_line_start() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_backspace.txt");

    // Create a test file with CRLF line endings
    let content = "Line 1\r\nLine 2\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to beginning of second line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Press backspace - should delete \r\n and join lines
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Lines should be joined
    harness.assert_screen_contains("Line 1Line 2");

    // Save and verify
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let saved_content = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        saved_content, "Line 1Line 2\r\n",
        "Backspace should have joined the lines"
    );
}

/// Test delete at end of line in CRLF buffer
/// Should delete the entire \r\n sequence, joining lines
#[test]
fn test_crlf_delete_at_line_end() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_delete.txt");

    // Create a test file with CRLF line endings
    let content = "Line 1\r\nLine 2\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to end of first line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Press delete - should delete \r\n and join lines
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Lines should be joined
    harness.assert_screen_contains("Line 1Line 2");

    // Save and verify
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let saved_content = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        saved_content, "Line 1Line 2\r\n",
        "Delete should have joined the lines"
    );
}

/// Test cut and paste in CRLF buffer
/// Cut text should preserve CRLF when pasted
#[test]
fn test_crlf_cut_paste() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_cut_paste.txt");

    // Create a test file with CRLF line endings
    let content = "Line 1\r\nLine 2\r\nLine 3\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Enable internal-only clipboard to avoid system clipboard interference in parallel tests
    harness.editor_mut().set_clipboard_for_test("".to_string());

    // Select "Line 2\r\n" - go to start of line 2, select to start of line 3
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();

    // Cut (Ctrl+X)
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should now have Line 1 and Line 3
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Line 2"),
        "Line 2 should be cut from display"
    );

    // Go to end of file and paste
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should now see Line 2 at end
    harness.assert_screen_contains("Line 2");

    // Save and verify CRLF preserved
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let saved_content = std::fs::read_to_string(&file_path).unwrap();
    // The pasted line should have CRLF
    assert!(
        saved_content.contains("Line 2\r\n"),
        "Pasted line should preserve CRLF ending"
    );
}

/// Test that CR characters in LF files are shown as <0D>
/// In Unix/LF files, \r is unusual and should be visible - even in \r\n sequences
#[test]
fn test_cr_shown_in_lf_file() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("lf_with_cr.txt");

    // Create a test file with LF line endings but containing CR characters
    // The file has more LF than CRLF, so it should be detected as LF
    // Even the \r\n sequence should show \r as <0D> because this is a Unix file
    let content = "Line1\nHello\rWorld\nLine3\r\nLine4\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Both standalone \r and \r in \r\n should be shown as <0D> in LF files
    // because any \r is unusual in a Unix file
    assert!(
        screen.contains("<0D>"),
        "CR characters in LF file should be shown as <0D>, screen: {}",
        screen
    );

    // The text should still be visible
    harness.assert_screen_contains("Line1");
    harness.assert_screen_contains("Hello");
    harness.assert_screen_contains("World");
    harness.assert_screen_contains("Line3");
    harness.assert_screen_contains("Line4");
}

/// Test cursor visibility after setting line ending to CRLF
/// Creates content, switches to CRLF, duplicates via copy/paste, then verifies
/// cursor visibility at all positions (start/end of each line, navigation)
#[test]
fn test_crlf_cursor_visibility() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("Test.java");

    // Create a Java file with syntax highlighting
    let java_content = r#"public class Test {
    public static void main(String[] args) {
        System.out.println("Hello");
        int x = 42;
    }
}"#;
    std::fs::write(&file_path, java_content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Step 2: Set line ending to CRLF via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("set line ending").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    // Select CRLF (Windows) - it's the second option
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the prompt closed (we're back to editing)
    // Status message may be truncated, so just verify we're not in prompt mode
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Line ending:"),
        "Should have closed the line ending prompt"
    );

    // Step 3: Select all, copy, go to end, paste twice to grow the file
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap(); // Select all
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap(); // Copy
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap(); // Go to end
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // New line
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap(); // Paste 1
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // New line
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap(); // Paste 2
    harness.render().unwrap();

    // Count lines in buffer (should be 6 original + 6 paste1 + 6 paste2 = 18+ lines)
    let content = harness.get_buffer_content().unwrap();
    let line_count = content.lines().count();
    assert!(
        line_count >= 18,
        "Should have at least 18 lines after pasting, got {}",
        line_count
    );

    // Helper to check cursor is visible on screen
    let check_cursor_visible = |harness: &mut EditorTestHarness, location: &str| {
        harness.render().unwrap();
        let (cursor_x, cursor_y) = harness.screen_cursor_position();
        let (content_start, content_end) = harness.content_area_rows();

        assert!(
            cursor_y as usize >= content_start && cursor_y as usize <= content_end,
            "Cursor at {} should be in content area: y={} not in range [{}, {}]",
            location,
            cursor_y,
            content_start,
            content_end
        );
        assert!(
            cursor_x < 80,
            "Cursor at {} should be within screen width: x={} >= 80",
            location,
            cursor_x
        );
    };

    // Step 4: Go to start of buffer
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    check_cursor_visible(&mut harness, "start of buffer");
    assert_eq!(harness.cursor_position(), 0, "Should be at byte 0");

    // Step 5: Iterate through ALL lines, checking visibility at start and end of each
    for line_num in 0..line_count {
        // Check cursor visible at start of line
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        check_cursor_visible(&mut harness, &format!("line {} start", line_num));

        // Check cursor visible at end of line
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        check_cursor_visible(&mut harness, &format!("line {} end", line_num));

        // Type a marker character and verify it appears
        harness.type_text("*").unwrap();
        harness.render().unwrap();

        // Move to next line
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    // Step 6: Navigate back up through all lines
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    check_cursor_visible(&mut harness, "end of buffer");

    for line_num in (0..line_count).rev() {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        check_cursor_visible(&mut harness, &format!("line {} (going up)", line_num));
    }

    // Step 7: Verify we can type at start of buffer
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("// START>>").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("// START>>");

    // Final verification: original content structure preserved (with markers)
    let final_content = harness.get_buffer_content().unwrap();
    assert!(
        final_content.contains("public class Test"),
        "Should contain class declaration"
    );
    assert!(
        final_content.contains("public static void main"),
        "Should contain main method"
    );
    assert!(
        final_content.contains("System.out.println"),
        "Should contain println"
    );
    assert!(
        final_content.contains("int x = 42"),
        "Should contain variable declaration"
    );
}

/// Test that changing line ending format and saving converts the file
/// When a file with LF endings has its format changed to CRLF via command palette,
/// saving should convert all line endings to CRLF
#[test]
fn test_set_line_ending_converts_on_save_lf_to_crlf() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("lf_to_crlf.txt");

    // Create a test file with LF line endings
    let content = "Line 1\nLine 2\nLine 3\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Use command palette to change line ending format to CRLF
    // First open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.render().unwrap();

    // Type "set line" to filter to "Set Line Ending" command
    harness.type_text("set line").unwrap();
    harness.render().unwrap();

    // Select the command with Enter (this opens the Set Line Ending prompt)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for the new prompt to open
    harness.wait_for_screen_contains("Line ending:").unwrap();
    harness.render().unwrap();

    // The line ending prompt opens with suggestions:
    // - LF (Unix/Linux/Mac) <- current selection (since file has LF)
    // - CRLF (Windows)
    // - CR (Classic Mac)
    // Use Down arrow to move to CRLF, then Enter to select
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Select CRLF with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for prompt to close
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("Saved").unwrap();
    harness.render().unwrap();

    // Read the file back and verify CRLF line endings
    let saved_bytes = std::fs::read(&file_path).unwrap();
    let saved_content = String::from_utf8_lossy(&saved_bytes);

    // All lines should have CRLF endings
    assert!(
        saved_content.contains("\r\n"),
        "File should contain CRLF sequences after conversion"
    );
    assert_eq!(
        saved_content, "Line 1\r\nLine 2\r\nLine 3\r\n",
        "All line endings should be converted to CRLF"
    );
}

/// Test that changing line ending format and saving converts the file
/// When a file with CRLF endings has its format changed to LF via command palette,
/// saving should convert all line endings to LF
#[test]
fn test_set_line_ending_converts_on_save_crlf_to_lf() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_to_lf.txt");

    // Create a test file with CRLF line endings
    let content = "Line 1\r\nLine 2\r\nLine 3\r\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Use command palette to change line ending format to LF
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.render().unwrap();

    harness.type_text("set line").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for the new prompt to open
    harness.wait_for_screen_contains("Line ending:").unwrap();
    harness.render().unwrap();

    // The line ending prompt opens with suggestions:
    // - LF (Unix/Linux/Mac)
    // - CRLF (Windows) <- current selection (since file has CRLF)
    // - CR (Classic Mac)
    // Use Up arrow to move to LF, then Enter to select
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for prompt to close
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("Saved").unwrap();
    harness.render().unwrap();

    // Read the file back and verify LF line endings
    let saved_bytes = std::fs::read(&file_path).unwrap();
    let saved_content = String::from_utf8_lossy(&saved_bytes);

    // All lines should have LF endings (no CRLF)
    assert!(
        !saved_content.contains("\r\n"),
        "File should not contain CRLF sequences after conversion to LF"
    );
    assert_eq!(
        saved_content, "Line 1\nLine 2\nLine 3\n",
        "All line endings should be converted to LF"
    );
}
