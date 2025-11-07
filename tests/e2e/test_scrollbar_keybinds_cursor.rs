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

/// Test 5: Empty last line bug - margin should remain visible, cursor navigation should work
/// When cursor is on an empty last line and Delete is pressed, the line number margin/gutter
/// should remain visible, and cursor movement should work correctly
#[test]
fn test_empty_last_line_delete_preserves_margin() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a buffer without trailing newline
    let content = "line1\nline2";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // Move cursor to end of buffer
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Cursor should be at position 11 (end of "line2")
    let initial_cursor = harness.cursor_position();
    assert_eq!(initial_cursor, 11, "Cursor should be at end of line2");

    // Hit Enter to create an empty last line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now cursor should be at position 12 (on the new empty line)
    let cursor_on_empty_line = harness.cursor_position();
    assert_eq!(
        cursor_on_empty_line, 12,
        "Cursor should be on empty last line at position 12"
    );
    harness.assert_buffer_content("line1\nline2\n");

    // Get screen position before Delete
    let (initial_screen_x, initial_screen_y) = harness.screen_cursor_position();

    // Check that gutter/margin is visible before Delete
    // The line number "3" should be visible somewhere in the gutter (columns 0-6)
    // Column 0: indicator, Columns 1-4: line number (right-aligned), Column 5: separator
    let mut found_line_num_before = false;
    for col in 0..7 {
        if let Some(cell) = harness.get_cell(col, initial_screen_y) {
            if cell.contains('3') {
                found_line_num_before = true;
                break;
            }
        }
    }
    assert!(
        found_line_num_before,
        "Line number '3' should be visible in gutter before Delete key"
    );

    // Press Delete key on the empty last line (this should do nothing since there's nothing to delete)
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Get screen position after Delete
    let (_, screen_y_after_delete) = harness.screen_cursor_position();

    // BUG: The margin/gutter line number should still be visible after Delete
    let mut found_line_num_after = false;
    for col in 0..7 {
        if let Some(cell) = harness.get_cell(col, screen_y_after_delete) {
            if cell.contains('3') {
                found_line_num_after = true;
                break;
            }
        }
    }
    assert!(
        found_line_num_after,
        "BUG: Line number '3' disappeared after Delete on empty last line"
    );

    // Cursor should still be on the same line
    assert_eq!(
        screen_y_after_delete, initial_screen_y,
        "Cursor screen Y position should not change after Delete"
    );

    // Now test cursor movement: move left (should go to end of previous line)
    harness
        .send_key(KeyCode::Left, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let cursor_after_left = harness.cursor_position();
    assert_eq!(
        cursor_after_left, 11,
        "After moving left, cursor should be at end of line2 (position 11)"
    );

    // BUG: Try to move right back - should be able to return to position 12
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let cursor_after_right = harness.cursor_position();
    assert_eq!(
        cursor_after_right, 12,
        "BUG: After moving left then right, should return to position 12, got {}",
        cursor_after_right
    );
}

/// Test 6: Cursor X position should be 0 (leftmost column) after pressing Enter at end of line
#[test]
fn test_cursor_x_position_after_enter_at_end_of_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a buffer with text on the last line
    let content = "line1\nline2";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // Move cursor to end of buffer (end of "line2")
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Cursor should be at position 11
    assert_eq!(harness.cursor_position(), 11);

    // Press Enter to create a new line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Cursor should be at position 12 (on the new empty line)
    assert_eq!(harness.cursor_position(), 12);

    // Get screen cursor position
    let (screen_x, screen_y) = harness.screen_cursor_position();

    // The cursor should be at the leftmost column of the content area (after the gutter)
    // For a buffer with 3 lines (line1, line2, empty), the gutter width is:
    // 1 (indicator) + 4 (line number) + 3 (separator " │ ") = 8
    // So screen_x should be 8 (the first column after the gutter)
    let expected_x = 8; // gutter width
    assert_eq!(
        screen_x, expected_x,
        "BUG: Cursor X should be at leftmost column {} (after gutter), got {}",
        expected_x,
        screen_x
    );

    // Now type a character - it should appear in the correct place
    harness.type_text("x").unwrap();
    harness.render().unwrap();

    // Cursor should now be at position 13 (after the 'x')
    assert_eq!(harness.cursor_position(), 13);

    // Buffer should be "line1\nline2\nx"
    harness.assert_buffer_content("line1\nline2\nx");

    // Screen cursor should be at column 9 (gutter + 1 character)
    let (screen_x_after, _) = harness.screen_cursor_position();
    assert_eq!(
        screen_x_after,
        expected_x + 1,
        "After typing 'x', cursor should be at column {}",
        expected_x + 1
    );
}
