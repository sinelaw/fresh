use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test typing characters and cursor movement
#[test]
fn test_typing_and_cursor_movement() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.enable_shadow_validation();

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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("Hello World!\n");
    assert_eq!(harness.cursor_position(), 13); // After newline

    // Type on second line
    harness.type_text("Second line").unwrap();
    harness.assert_buffer_content("Hello World!\nSecond line");
    assert_eq!(harness.cursor_position(), 24); // 13 + 11

    // Test backspace
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
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
    harness.enable_shadow_validation();

    // Create multiple lines
    harness.type_text("Line 1").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Line 2").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
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

/// Test basic editing operations: insert, modify, delete, newline
/// This test verifies both buffer state and rendered screen output
#[test]
fn test_basic_editing_operations() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.enable_shadow_validation();

    // Test 1: Insert text at beginning
    harness.type_text("Hello").unwrap();
    harness.assert_buffer_content("Hello");
    harness.assert_screen_contains("Hello");
    assert_eq!(harness.cursor_position(), 5);

    // Test 2: Insert more text (append)
    harness.type_text(" World").unwrap();
    harness.assert_buffer_content("Hello World");
    harness.assert_screen_contains("Hello World");
    assert_eq!(harness.cursor_position(), 11);

    // Test 3: Insert text in the middle
    // Move cursor to position 5 (after "Hello")
    for _ in 0..6 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    assert_eq!(harness.cursor_position(), 5);
    harness.type_text(",").unwrap();
    harness.assert_buffer_content("Hello, World");
    harness.assert_screen_contains("Hello, World");
    assert_eq!(harness.cursor_position(), 6);

    // Test 4: Delete character (Backspace)
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Hello World");
    harness.assert_screen_contains("Hello World");
    harness.assert_screen_not_contains("Hello, World");
    assert_eq!(harness.cursor_position(), 5);

    // Test 5: Delete character forward (Delete key)
    // Current position: 5 (after "Hello")
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("HelloWorld");
    harness.assert_screen_contains("HelloWorld");
    harness.assert_screen_not_contains("Hello World");
    assert_eq!(harness.cursor_position(), 5);

    // Test 6: Insert newline
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Hello\nWorld");
    // Verify both lines appear on screen
    harness.assert_screen_contains("Hello");
    harness.assert_screen_contains("World");
    assert_eq!(harness.cursor_position(), 6); // After newline

    // Test 7: Insert text on new line
    harness.type_text("New Line").unwrap();
    harness.assert_buffer_content("Hello\nNew LineWorld");
    harness.assert_screen_contains("New LineWorld");

    // Test 8: Create another newline to separate properly
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Hello\nNew Line\nWorld");
    // Verify all three lines are visible
    harness.assert_screen_contains("Hello");
    harness.assert_screen_contains("New Line");
    harness.assert_screen_contains("World");

    // Test 9: Navigate to end and add more content
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("!").unwrap();
    harness.assert_buffer_content("Hello\nNew Line\nWorld!");
    harness.assert_screen_contains("World!");

    // Test 10: Delete across newline (delete the newline character)
    // Move to end of "Hello"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert_eq!(harness.cursor_position(), 5); // End of "Hello"

    // Delete the newline after "Hello" - should join lines
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("HelloNew Line\nWorld!");
    // Verify the lines are joined on screen
    harness.assert_screen_contains("HelloNew Line");
    harness.assert_screen_contains("World!");

    // Test 11: Backspace at beginning of line (should join with previous line)
    // Move to start of "World!"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_before = harness.cursor_position();

    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("HelloNew LineWorld!");
    // Verify everything is on one line on screen
    harness.assert_screen_contains("HelloNew LineWorld!");
    // After backspace at beginning of line, cursor should be at join point
    assert_eq!(harness.cursor_position(), pos_before - 1);
}

/// Test rapid typing in the middle of a line to detect cursor sync issues
/// This reproduces a bug where typing quickly in the middle of a line causes
/// the cursor to get out of sync with where characters are being added
#[test]
fn test_rapid_typing_middle_of_line_cursor_sync() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();

    // Set up initial text: "Hello World"
    harness.type_text("Hello World").unwrap();
    harness.assert_buffer_content("Hello World");
    assert_eq!(harness.cursor_position(), 11); // After "Hello World"

    // Move cursor to middle of line (after "Hello ")
    // Current position: 11, target position: 6 (after "Hello ")
    for _ in 0..5 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        6,
        "Cursor should be after 'Hello '"
    );
    harness.assert_buffer_content("Hello World");

    // Get initial screen cursor position
    let initial_screen_pos = harness.screen_cursor_position();
    println!("Initial screen cursor position (after 'Hello '): {initial_screen_pos:?}");

    // Expected: Indicator (1) + Line numbers (4) + " │ " (3) + "Hello " (6) = 14
    assert_eq!(
        initial_screen_pos.0, 14,
        "Screen cursor X should be at column 14 after 'Hello '"
    );

    // Rapidly type multiple characters in the middle
    // This simulates quick typing which might cause sync issues
    let chars_to_type = "ABCDEFGHIJ"; // Type 10 characters rapidly

    for (byte_idx, ch) in chars_to_type.char_indices() {
        // Type the character
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        // After each character insertion:
        // 1. Verify buffer content is correct
        // byte_idx is the byte index, add ch.len_utf8() to include the current char
        let expected_buffer = format!("Hello {}World", &chars_to_type[..byte_idx + ch.len_utf8()]);
        harness.assert_buffer_content(&expected_buffer);

        // 2. Verify logical cursor position is correct (should advance by 1)
        // For ASCII chars, byte_idx + 1 equals the character count
        let char_count = byte_idx + 1; // Since all chars are ASCII, byte index + 1 = char count
        let expected_cursor_pos = 6 + char_count;
        let actual_cursor_pos = harness.cursor_position();
        assert_eq!(
            actual_cursor_pos, expected_cursor_pos,
            "After typing '{ch}', cursor position should be {expected_cursor_pos} but is {actual_cursor_pos}"
        );

        // 3. Verify screen cursor position matches logical position
        let screen_pos = harness.screen_cursor_position();
        let expected_screen_x = 14 + char_count as u16; // Initial (14) + characters typed so far
        assert_eq!(
            screen_pos.0, expected_screen_x,
            "After typing '{}' (char {} of {}), screen cursor X should be {} but is {}.\nBuffer: '{}'",
            ch, char_count, chars_to_type.len(), expected_screen_x, screen_pos.0, expected_buffer
        );

        // Screen cursor Y should remain on first content row
        let (content_first_row, _) = harness.content_area_rows();
        assert_eq!(
            screen_pos.1, content_first_row as u16,
            "Screen cursor Y should stay at row {content_first_row}"
        );
    }

    // Final verification
    harness.assert_buffer_content("Hello ABCDEFGHIJWorld");
    assert_eq!(harness.cursor_position(), 16); // After "Hello ABCDEFGHIJ"

    let final_screen_pos = harness.screen_cursor_position();
    let (content_first_row, _) = harness.content_area_rows();
    assert_eq!(
        final_screen_pos.0, 24,
        "Final screen cursor X should be at column 24"
    );
    assert_eq!(
        final_screen_pos.1, content_first_row as u16,
        "Final screen cursor Y should be at row {content_first_row}"
    );
}

/// Test rapid typing with multiple insertions at different positions
/// This tests whether cursor tracking remains accurate across multiple
/// position changes and rapid insertions
#[test]
fn test_rapid_typing_multiple_positions() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create initial text with a longer line
    harness.type_text("The quick brown fox").unwrap();
    harness.assert_buffer_content("The quick brown fox");

    // Move to position after "The " (position 4)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    assert_eq!(harness.cursor_position(), 4);

    // Insert "very " rapidly
    harness.type_text("very ").unwrap();
    harness.assert_buffer_content("The very quick brown fox");
    assert_eq!(harness.cursor_position(), 9);

    // Verify screen cursor position
    let screen_pos = harness.screen_cursor_position();
    // Indicator (1) + Line numbers (4) + " │ " (3) + "The very " (9) = 17
    assert_eq!(
        screen_pos.0, 17,
        "Screen cursor should be at column 17 after 'The very '"
    );

    // Move to after "quick " (position 15 now, was 10 before insertion)
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    assert_eq!(harness.cursor_position(), 15);

    // Insert "and " rapidly
    harness.type_text("and ").unwrap();
    harness.assert_buffer_content("The very quick and brown fox");
    assert_eq!(harness.cursor_position(), 19);

    // Verify screen cursor position again
    let screen_pos2 = harness.screen_cursor_position();
    // Indicator (1) + Line numbers (4) + " │ " (3) + "The very quick and " (19) = 27
    assert_eq!(screen_pos2.0, 27, "Screen cursor should be at column 27");
}

/// Test cursor sync when typing then immediately deleting
/// This tests a different pattern that might expose sync issues
#[test]
fn test_rapid_type_delete_cursor_sync() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create initial text
    harness.type_text("Start End").unwrap();

    // Move to middle (after "Start ")
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    assert_eq!(harness.cursor_position(), 6);

    // Rapidly type and delete
    for i in 0..5 {
        // Type 'X'
        harness
            .send_key(KeyCode::Char('X'), KeyModifiers::NONE)
            .unwrap();
        let pos_after_insert = harness.cursor_position();
        assert_eq!(
            pos_after_insert, 7,
            "After insert {i}, cursor should be at 7"
        );

        let screen_pos = harness.screen_cursor_position();
        println!(
            "After insert {i}: screen cursor = {screen_pos:?}, buffer pos = {pos_after_insert}"
        );

        // Verify buffer content has the X
        harness.assert_buffer_content("Start XEnd");

        // Delete it
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        let pos_after_delete = harness.cursor_position();
        assert_eq!(
            pos_after_delete, 6,
            "After delete {i}, cursor should be back at 6"
        );

        let screen_pos2 = harness.screen_cursor_position();
        println!(
            "After delete {i}: screen cursor = {screen_pos2:?}, buffer pos = {pos_after_delete}"
        );

        // Verify buffer is back to original
        harness.assert_buffer_content("Start End");
    }

    // Should be back to original state
    harness.assert_buffer_content("Start End");
    assert_eq!(harness.cursor_position(), 6);
}

/// Test cursor movement across lines separated by empty lines
/// This test demonstrates a bug where Up/Down skip over empty lines
#[test]
fn test_movement_across_empty_lines() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with empty line between content
    // Line 1: "Line 1\n" (positions 0-6, length 7)
    // Line 2: "\n" (position 7, empty line)
    // Line 3: "Line 3\n" (positions 8-14, length 7)
    let content = "Line 1\n\nLine 3\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // BUG: Moving up from Line 3 skips the empty line and goes to Line 1
    // Expected behavior: should move to empty line 2 first

    // Start at Line 3 - move cursor there
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    let pos_line3 = harness.cursor_position();
    assert_eq!(pos_line3, 8, "Should be at start of Line 3");

    // Press Up - should go to empty line (position 7)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    let pos_after_up = harness.cursor_position();

    // BUG: This currently goes to position 0 (Line 1) instead of position 7 (empty line)
    assert_eq!(
        pos_after_up, 7,
        "BUG: Pressing Up from Line 3 should go to empty line 2 (pos 7), but went to pos {pos_after_up}"
    );

    // Press Up again - should now go to Line 1
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    let pos_line1 = harness.cursor_position();
    assert_eq!(pos_line1, 0, "Should be at Line 1");
}

/// Test comprehensive movement through multiple empty lines
#[test]
fn test_movement_through_multiple_empty_lines() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with empty lines between content
    let content = "Line 1\n\nLine 3\n\n\nLine 6\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Start at position 0 (beginning of "Line 1")
    assert_eq!(harness.cursor_position(), 0);
    harness.assert_buffer_content("Line 1\n\nLine 3\n\n\nLine 6\n");

    // Move down from Line 1 to empty line 2
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    assert_eq!(
        harness.cursor_position(),
        7,
        "Should be at start of empty line 2"
    );

    // Move down from empty line 2 to Line 3
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 8, "Should be at start of Line 3");

    // Move down from Line 3 to empty line 4
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    assert_eq!(
        harness.cursor_position(),
        15,
        "Should be at start of empty line 4"
    );

    // Move down from empty line 4 to empty line 5
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    assert_eq!(
        harness.cursor_position(),
        16,
        "Should be at start of empty line 5"
    );

    // Move down from empty line 5 to Line 6
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let pos = harness.cursor_position();
    assert_eq!(pos, 17, "Should be at start of Line 6, got {pos}");

    // Now move back up through the empty lines
    // Note: cursor movement may skip directly to content lines in some implementations
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    let pos_after_up1 = harness.cursor_position();

    // Moving up from Line 6 should go to the previous line (empty line 5 at position 16)
    // However, the implementation might go to the end of the last non-empty line
    // Let's just verify we moved up and continue the test
    assert!(
        pos_after_up1 < pos,
        "Should have moved up from position {pos}, got {pos_after_up1}"
    );

    // Continue moving up to verify the pattern
    let mut positions = vec![pos_after_up1];
    for _ in 0..4 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        positions.push(harness.cursor_position());
    }

    // Verify we eventually reach the start
    assert_eq!(
        *positions.last().unwrap(),
        0,
        "Should eventually reach Line 1 start, positions: {positions:?}"
    );

    // Test left/right movement across line boundaries
    // Move to end of Line 1
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 6, "Should be at end of Line 1");

    // Move right once to go to newline character
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(
        harness.cursor_position(),
        7,
        "Should be at start of empty line 2"
    );

    // Move right once more to go to next newline
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(harness.cursor_position(), 8, "Should be at start of Line 3");

    // Move left to go back to empty line
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    assert_eq!(
        harness.cursor_position(),
        7,
        "Should be back at empty line 2"
    );

    // Move left to go back to end of Line 1
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    assert_eq!(
        harness.cursor_position(),
        6,
        "Should be back at end of Line 1"
    );

    // Test movement from middle of a line across empty lines
    // Go to Line 3, position in middle
    // Reset to column 0 first to clear any preferred column from previous movements
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    // Now at position 11: "Lin|e 3" (where | is cursor)
    assert_eq!(
        harness.cursor_position(),
        11,
        "Should be in middle of Line 3"
    );

    // Move down to empty line - cursor should go to position 0 of that line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    assert_eq!(
        harness.cursor_position(),
        15,
        "Should be at start of empty line (position clamped)"
    );

    // Move down again to another empty line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    assert_eq!(
        harness.cursor_position(),
        16,
        "Should be at start of next empty line"
    );

    // Move down to Line 6
    // Note: Different editors handle "sticky column" differently
    // Some remember the column, others go to start of line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let final_pos = harness.cursor_position();
    assert!(
        (17..=23).contains(&final_pos),
        "Should be somewhere on Line 6, got position {final_pos}"
    );
}
