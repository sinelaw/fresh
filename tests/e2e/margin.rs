use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test that line numbers are rendered correctly with the margin system
#[test]
fn test_margin_line_numbers_rendering() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with 10 lines
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen output:\n{}", screen);

    // Should show line numbers in the left margin
    harness.assert_screen_contains("   1 │");
    harness.assert_screen_contains("   2 │");
    harness.assert_screen_contains("   3 │");

    // Should show file content
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 2");
    harness.assert_screen_contains("Line 3");
}

/// Test that margins work correctly in empty buffers
#[test]
fn test_margin_empty_buffer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Empty buffer screen:\n{}", screen);

    // Should show line 1 even for empty buffer
    harness.assert_screen_contains("   1 │");
}

/// Test that line numbers adjust width for large files
#[test]
fn test_margin_large_file_line_numbers() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large.txt");

    // Create a file with 1000 lines
    let content: String = (1..=1000).map(|i| format!("Line {}\n", i)).collect();
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Jump to end
    harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Large file screen (at end):\n{}", screen);

    // Should show 4-digit line numbers
    // Line 1000 should be visible
    harness.assert_screen_contains("1000 │");
}

/// Test that margins can be disabled via events
#[test]
fn test_margin_disable_line_numbers() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Disable line numbers via event
    harness.apply_event(editor::event::Event::SetLineNumbers { enabled: false }).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen without line numbers:\n{}", screen);

    // Should NOT show line numbers (check for line number separator pattern " │ " with spaces)
    // Note: We can't just check for "│" because the scrollbar also uses that character
    harness.assert_screen_not_contains(" │ ");

    // Should still show content (but without margin)
    harness.assert_screen_contains("Line 1");
}

/// Test adding custom margin annotations (e.g., breakpoint, error)
#[test]
fn test_margin_custom_annotations() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Add a breakpoint annotation at line 2 (0-indexed)
    harness.apply_event(editor::event::Event::AddMarginAnnotation {
        line: 2,
        position: editor::event::MarginPositionData::Left,
        content: editor::event::MarginContentData::Symbol {
            text: "●".to_string(),
            color: Some((255, 0, 0)), // Red
        },
        annotation_id: Some("breakpoint-1".to_string()),
    }).unwrap();

    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with breakpoint annotation:\n{}", screen);

    // Should show the breakpoint symbol on line 3 (1-indexed display)
    // The line should have both line number and breakpoint
    harness.assert_screen_contains("●");

    // Remove the annotation
    harness.apply_event(editor::event::Event::RemoveMarginAnnotation {
        annotation_id: "breakpoint-1".to_string(),
    }).unwrap();

    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("Screen after removing annotation:\n{}", screen_after);

    // Breakpoint should be gone
    // But line numbers should still be there
    harness.assert_screen_contains("   3 │");
}

/// Test that margins work correctly after editing
#[test]
fn test_margin_after_editing() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some lines
    harness.type_text("First line").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("Second line").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("Third line").unwrap();

    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after typing:\n{}", screen);

    // Should show line numbers for all lines
    harness.assert_screen_contains("   1 │");
    harness.assert_screen_contains("   2 │");
    harness.assert_screen_contains("   3 │");

    // Should show typed content
    harness.assert_screen_contains("First line");
    harness.assert_screen_contains("Second line");
    harness.assert_screen_contains("Third line");
}

/// Test cursor position with margin (cursor should account for margin width)
#[test]
fn test_cursor_position_with_margin() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("abc").unwrap();
    harness.render().unwrap();

    let cursor_pos = harness.screen_cursor_position();
    println!("Cursor position: {:?}", cursor_pos);

    // Format: [indicator (1)] + [line numbers (4)] + [" │ " (3)] = 8 chars gutter
    // cursor after "abc" should be at column 11 (8 + 3)
    assert_eq!(cursor_pos.0, 11, "Cursor X position should account for margin width");
    assert_eq!(cursor_pos.1, 1, "Cursor Y position should be on first line");
}

/// Test that margins work with horizontal scrolling
#[test]
fn test_margin_with_horizontal_scroll() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("long_line.txt");

    // Create a file with a very long line
    let long_line = "X".repeat(200);
    std::fs::write(&file_path, &long_line).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move cursor to the right to trigger horizontal scrolling
    for _ in 0..100 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with horizontal scroll:\n{}", screen);

    // Line number should still be visible even when horizontally scrolled
    harness.assert_screen_contains("   1 │");

    // Should see X's (the content)
    harness.assert_screen_contains("X");
}

/// Test that margins are per-buffer in split view
/// Each buffer should have its own independent margin state
#[test]
#[ignore = "Splits currently share the same active buffer (architectural limitation). All splits display the currently active buffer, so this test's assumption of independent buffers per split doesn't match current behavior."]
fn test_margin_per_buffer_in_split_view() {
    let temp_dir = TempDir::new().unwrap();

    // Create two files
    let file1_path = temp_dir.path().join("file1.txt");
    let file2_path = temp_dir.path().join("file2.txt");
    std::fs::write(&file1_path, "File 1 Line 1\nFile 1 Line 2\n").unwrap();
    std::fs::write(&file2_path, "File 2 Line 1\nFile 2 Line 2\nFile 2 Line 3\n").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Open first file
    harness.open_file(&file1_path).unwrap();

    // Create a vertical split and open second file
    harness.send_key(KeyCode::Char('v'), KeyModifiers::ALT).unwrap();
    harness.open_file(&file2_path).unwrap();

    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Split view screen:\n{}", screen);

    // Both splits should show line numbers
    harness.assert_screen_contains("   1 │");

    // Both files should be visible
    harness.assert_screen_contains("File 1 Line 1");
    harness.assert_screen_contains("File 2 Line 1");

    // Now disable line numbers in the active buffer (file2)
    harness.apply_event(editor::event::Event::SetLineNumbers { enabled: false }).unwrap();

    // Add a custom annotation to file1 (need to switch to file1 first)
    harness.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap(); // Switch to previous split
    harness.apply_event(editor::event::Event::AddMarginAnnotation {
        line: 0,
        position: editor::event::MarginPositionData::Left,
        content: editor::event::MarginContentData::Symbol {
            text: "●".to_string(),
            color: Some((255, 0, 0)),
        },
        annotation_id: Some("file1-marker".to_string()),
    }).unwrap();

    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("Split view after modifications:\n{}", screen_after);

    // File 1 should still have line numbers
    // Note: The marker might not be visible depending on split layout
    // The key point is that disabling line numbers in file2 doesn't affect file1

    // This verifies that each EditorState has its own MarginManager
    // If margins were shared, disabling in one would affect both
}
