use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test 1: Scrollbar handle fills entire height when buffer fits in viewport
/// This makes it obvious to the user that there's no scrolling possible
#[test]
fn test_scrollbar_fills_height_when_no_scrolling_needed() {
    // Create a small buffer that fits entirely within the viewport
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create content with only 5 lines (much less than the 24-line viewport)
    let content = "line1\nline2\nline3\nline4\nline5";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // The scrollbar is in the rightmost column (column 79 for 80-width terminal)
    // For a buffer that fits entirely in the viewport, the thumb should fill
    // the entire scrollbar height (all 24 rows minus 1 for tab bar)
    let scrollbar_col = 79;

    // Check that every row in the scrollbar column shows the thumb character (█)
    // not the track character (│).
    // Row 0 is tab bar, Row 23 is status bar, so check rows 1-22 (content area)
    for row in 1..23 {
        let cell_content = harness.get_cell(scrollbar_col, row);
        assert_eq!(
            cell_content.as_deref(),
            Some("█"),
            "Row {}: Expected scrollbar thumb (█), got {:?}. \
             When buffer fits in viewport, entire scrollbar should be filled.",
            row,
            cell_content
        );
    }
}

/// Test 2: Ctrl+Backspace deletes until previous word (delete word backward)
#[test]
fn test_ctrl_backspace_deletes_word_backward() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create buffer with some text
    let content = "hello world test";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // Move cursor to end of buffer
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Cursor should be at position 16 (after "test")
    assert_eq!(harness.cursor_position(), 16);
    harness.assert_buffer_content("hello world test");

    // Press Ctrl+Backspace to delete "test" (word backward)
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should have deleted "test", cursor now at position 12 (after "world ")
    assert_eq!(harness.cursor_position(), 12);
    harness.assert_buffer_content("hello world ");

    // Press Ctrl+Backspace again to delete "world"
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should have deleted "world", cursor now at position 6 (after "hello ")
    assert_eq!(harness.cursor_position(), 6);
    harness.assert_buffer_content("hello ");
}

/// Test 3: Cursor renders correctly when hitting Enter at end of last line
#[test]
fn test_cursor_visible_after_enter_at_end_of_file() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create buffer with a few lines
    let content = "line1\nline2\nline3";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // Move to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Cursor should be at end of "line3"
    let cursor_before_enter = harness.cursor_position();
    assert_eq!(cursor_before_enter, 17); // "line1\nline2\nline3" = 17 chars

    // Hit Enter to create a new line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Cursor should now be on the new empty line (position 18, after the newline)
    let cursor_after_enter = harness.cursor_position();
    assert_eq!(cursor_after_enter, 18);
    harness.assert_buffer_content("line1\nline2\nline3\n");

    // Check that cursor is visible on screen (not at 0,0 which would be the bug)
    let (screen_x, screen_y) = harness.screen_cursor_position();

    // Cursor should NOT be at (0, 0) - that's the bug we're fixing
    assert!(
        screen_y > 0 || screen_x > 0,
        "Cursor rendered at (0,0) - this is the bug! Expected cursor on line 4"
    );

    // Cursor should be on the 4th content line (row 4 accounting for tab bar at row 0)
    // Row 0: tab bar, Row 1: line1, Row 2: line2, Row 3: line3, Row 4: new empty line
    assert_eq!(
        screen_y, 4,
        "Cursor should be on row 4 (new line after line3), got row {}",
        screen_y
    );

    // Type a character to verify cursor is in the correct logical position
    harness.type_text("x").unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("line1\nline2\nline3\nx");
    assert_eq!(harness.cursor_position(), 19); // After the 'x'
}

/// Test 4: Cursor stays visible when moving down past visible area
#[test]
fn test_cursor_visible_when_scrolling_down_in_large_file() {
    let mut harness = EditorTestHarness::new(80, 10).unwrap();

    // Create a file with 30 lines (more than the 10-line viewport)
    let mut lines = Vec::new();
    for i in 1..=30 {
        lines.push(format!("line {}", i));
    }
    let content = lines.join("\n");
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Start at top
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Move down 15 times (past the visible area)
    for i in 0..15 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        // After each key press, cursor should be visible on screen
        // (not disappeared as the bug describes)
        let (screen_x, screen_y) = harness.screen_cursor_position();

        // Cursor should be visible (y should be within viewport height)
        assert!(
            screen_y < 10,
            "After {} down arrows, cursor disappeared off screen at y={}",
            i + 1,
            screen_y
        );
    }

    // Verify we're at line 16 (0-indexed as line 15)
    let cursor_pos = harness.cursor_position();
    let buffer_content = harness.get_buffer_content();
    let lines_before_cursor: Vec<&str> = buffer_content[..cursor_pos].split('\n').collect();
    assert_eq!(
        lines_before_cursor.len(),
        16,
        "Should be at line 16 (0-indexed), got line {}",
        lines_before_cursor.len()
    );
}
