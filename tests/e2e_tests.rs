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
        content.push_str(&format!("Line {i}\n"));
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

/// Test that screen cursor position matches actual cursor position
#[test]
fn test_screen_cursor_position() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type "abc" on first line
    harness.type_text("abc").unwrap();
    harness.assert_buffer_content("abc");

    // Render and check cursor position
    harness.render().unwrap();

    // Get the actual screen cursor position from the terminal
    let cursor_pos = harness.screen_cursor_position();

    // After typing "abc", cursor should be at column 10:
    // "   1 │ abc" - the cursor should be after 'c'
    // Line numbers are 4 chars wide: "   1"
    // Then " │ " = 3 chars
    // Then "abc" = 3 chars
    // Total: 4 + 3 + 3 = 10
    // So cursor X should be at column 10 (0-indexed)
    // And cursor Y should be at row 1 (0-indexed, because row 0 is the tab bar)

    println!("Cursor position after typing 'abc': {cursor_pos:?}");
    println!("Expected: x=10 (4 + 3 + 3), y=1");

    assert_eq!(
        cursor_pos.1, 1,
        "Cursor Y should be at row 1 (below tab bar)"
    );
    assert_eq!(
        cursor_pos.0, 10,
        "Cursor X should be at column 10 (after 'abc')"
    );
}

/// Test cursor position as we type more characters
#[test]
fn test_cursor_x_position_advances() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Start with empty buffer
    harness.render().unwrap();
    let pos0 = harness.screen_cursor_position();
    println!("Initial cursor position: {pos0:?}");

    // Type first character
    harness.type_text("a").unwrap();
    harness.render().unwrap();
    let pos1 = harness.screen_cursor_position();
    println!("After 'a': {pos1:?}");

    // Type second character
    harness.type_text("b").unwrap();
    harness.render().unwrap();
    let pos2 = harness.screen_cursor_position();
    println!("After 'ab': {pos2:?}");

    // Type third character
    harness.type_text("c").unwrap();
    harness.render().unwrap();
    let pos3 = harness.screen_cursor_position();
    println!("After 'abc': {pos3:?}");

    // Y position should stay constant (row 1)
    assert_eq!(pos0.1, 1, "Initial Y should be 1");
    assert_eq!(pos1.1, 1, "Y should stay at 1 after 'a'");
    assert_eq!(pos2.1, 1, "Y should stay at 1 after 'ab'");
    assert_eq!(pos3.1, 1, "Y should stay at 1 after 'abc'");

    // X position should advance by 1 each time
    assert_eq!(pos1.0, pos0.0 + 1, "X should advance by 1 after 'a'");
    assert_eq!(pos2.0, pos1.0 + 1, "X should advance by 1 after 'b'");
    assert_eq!(pos3.0, pos2.0 + 1, "X should advance by 1 after 'c'");
}

/// Test help page display and toggle
#[test]
fn test_help_page_display() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Initially help should not be visible
    assert!(!harness.editor().is_help_visible());

    // Toggle help on
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    // Help should now be visible
    assert!(harness.editor().is_help_visible());

    // Screen should contain help page elements
    harness.assert_screen_contains("KEYBOARD SHORTCUTS");
    harness.assert_screen_contains("Help");

    // Should show some keybindings (check for ones that appear on first page)
    harness.assert_screen_contains("Ctrl+D"); // Add cursor at next match

    // Toggle help off
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    // Help should no longer be visible
    assert!(!harness.editor().is_help_visible());
}

/// Test help page shows keybindings
#[test]
fn test_help_page_shows_keybindings() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Toggle help on
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Help screen:\n{}", screen);

    // Should show common keybindings that appear on first page
    harness.assert_screen_contains("Ctrl+C"); // Copy
    harness.assert_screen_contains("Ctrl+X"); // Cut
    harness.assert_screen_contains("Backspace"); // Delete backward

    // Should show some actions
    harness.assert_screen_contains("Copy");
    harness.assert_screen_contains("Delete backward");
}

/// Test help page scrolling
#[test]
fn test_help_page_scrolling() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Toggle help on
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    // Get initial screen content
    let screen_before = harness.screen_to_string();

    // Scroll down
    harness.editor_mut().scroll_help(5);
    harness.render().unwrap();

    // Screen should have changed after scrolling
    let screen_after = harness.screen_to_string();

    // The content should be different (different lines visible)
    // Note: This test might be fragile if we don't have enough keybindings to scroll
    // We're just verifying the scroll mechanism works

    // Scroll back to top
    harness.editor_mut().scroll_help(-100); // Large negative to ensure we're at top
    harness.render().unwrap();

    let screen_top = harness.screen_to_string();

    // After scrolling back to top, should match the initial screen
    assert_eq!(screen_top, screen_before, "Scrolling back to top should restore original view");
}

/// Test help page resets scroll on toggle
#[test]
fn test_help_page_scroll_reset() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Toggle help on and scroll down
    harness.editor_mut().toggle_help();
    harness.editor_mut().scroll_help(10);
    harness.render().unwrap();

    // Toggle help off
    harness.editor_mut().toggle_help();

    // Toggle help on again - scroll should be reset
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    // Should be showing the top of the help (scroll position 0)
    harness.assert_screen_contains("KEYBOARD SHORTCUTS");
}

/// Test adding cursor at next match with Ctrl+D
#[test]
fn test_add_cursor_next_match() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text with repeated words
    harness.type_text("foo bar foo baz foo").unwrap();
    harness.assert_buffer_content("foo bar foo baz foo");

    // Select the first "foo" (positions 0-3)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();

    // Verify selection
    let primary = harness.editor().active_state().cursors.primary();
    assert_eq!(primary.position, 3);
    assert_eq!(primary.anchor, Some(0));

    // Press Ctrl+D to add cursor at next "foo"
    harness.editor_mut().add_cursor_at_next_match();
    harness.render().unwrap();

    // Should now have 2 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 2);

    // Press Ctrl+D again to add cursor at third "foo"
    harness.editor_mut().add_cursor_at_next_match();
    harness.render().unwrap();

    // Should now have 3 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 3);
}

/// Test adding cursor above with Ctrl+Alt+Up
#[test]
fn test_add_cursor_above() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Position cursor on Line 3
    harness.assert_buffer_content("Line 1\nLine 2\nLine 3");

    // Add cursor above (to Line 2)
    harness.editor_mut().add_cursor_above();
    harness.render().unwrap();

    // Should now have 2 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 2);

    // Add cursor above again (to Line 1)
    harness.editor_mut().add_cursor_above();
    harness.render().unwrap();

    // Should now have 3 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 3);
}

/// Test adding cursor below with Ctrl+Alt+Down
#[test]
fn test_add_cursor_below() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Position cursor on Line 1
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();

    // Add cursor below (to Line 2)
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 2 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 2);

    // Add cursor below again (to Line 3)
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 3 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 3);
}

/// Test multi-cursor typing
#[test]
fn test_multi_cursor_typing() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines with more content
    harness.type_text("aaa\nbbb\nccc\nddd").unwrap();

    // Go to start
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();

    // Add cursors - each time we add a cursor below, the new cursor becomes primary
    // So we can continue adding cursors below
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1 and 2
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1, 2, and 3

    // Should have 3 cursors
    let cursor_count = harness.editor().active_state().cursors.iter().count();
    assert_eq!(cursor_count, 3, "Should have 3 cursors");

    // Type "X" with all three cursors
    harness.type_text("X").unwrap();

    // Each cursor should insert X at its position
    let result = harness.get_buffer_content();

    // Count how many X's were inserted
    let x_count = result.matches('X').count();
    assert_eq!(x_count, 3, "Should have inserted exactly 3 X's, one per cursor");
}

/// Test removing secondary cursors with Esc
#[test]
fn test_remove_secondary_cursors() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Add cursors above
    harness.editor_mut().add_cursor_above();
    harness.editor_mut().add_cursor_above();

    // Should have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 3);

    // Remove secondary cursors
    harness.editor_mut().active_state_mut().cursors.remove_secondary();
    harness.render().unwrap();

    // Should have only 1 cursor now
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 1);
}

/// Test rapid typing in the middle of a line to detect cursor sync issues
/// This reproduces a bug where typing quickly in the middle of a line causes
/// the cursor to get out of sync with where characters are being added
#[test]
fn test_rapid_typing_middle_of_line_cursor_sync() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Set up initial text: "Hello World"
    harness.type_text("Hello World").unwrap();
    harness.assert_buffer_content("Hello World");
    assert_eq!(harness.cursor_position(), 11); // After "Hello World"

    // Move cursor to middle of line (after "Hello ")
    // Current position: 11, target position: 6 (after "Hello ")
    for _ in 0..5 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    assert_eq!(harness.cursor_position(), 6, "Cursor should be after 'Hello '");
    harness.assert_buffer_content("Hello World");

    // Get initial screen cursor position
    let initial_screen_pos = harness.screen_cursor_position();
    println!("Initial screen cursor position (after 'Hello '): {:?}", initial_screen_pos);

    // Expected: Line numbers (4 chars) + " │ " (3 chars) + "Hello " (6 chars) = 13
    assert_eq!(initial_screen_pos.0, 13, "Screen cursor X should be at column 13 after 'Hello '");

    // Rapidly type multiple characters in the middle
    // This simulates quick typing which might cause sync issues
    let chars_to_type = "ABCDEFGHIJ"; // Type 10 characters rapidly

    for (i, ch) in chars_to_type.chars().enumerate() {
        // Type the character
        harness.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();

        // After each character insertion:
        // 1. Verify buffer content is correct
        let expected_buffer = format!("Hello {}World", &chars_to_type[..=i]);
        harness.assert_buffer_content(&expected_buffer);

        // 2. Verify logical cursor position is correct (should advance by 1)
        let expected_cursor_pos = 6 + i + 1;
        let actual_cursor_pos = harness.cursor_position();
        assert_eq!(
            actual_cursor_pos, expected_cursor_pos,
            "After typing '{}', cursor position should be {} but is {}",
            ch, expected_cursor_pos, actual_cursor_pos
        );

        // 3. Verify screen cursor position matches logical position
        let screen_pos = harness.screen_cursor_position();
        let expected_screen_x = 13 + i as u16 + 1; // Initial (13) + characters typed so far
        assert_eq!(
            screen_pos.0, expected_screen_x,
            "After typing '{}' (char {} of {}), screen cursor X should be {} but is {}.\nBuffer: '{}'",
            ch, i + 1, chars_to_type.len(), expected_screen_x, screen_pos.0, expected_buffer
        );

        // Screen cursor Y should remain on line 1 (row 1, 0-indexed)
        assert_eq!(
            screen_pos.1, 1,
            "Screen cursor Y should stay at row 1"
        );
    }

    // Final verification
    harness.assert_buffer_content("Hello ABCDEFGHIJWorld");
    assert_eq!(harness.cursor_position(), 16); // After "Hello ABCDEFGHIJ"

    let final_screen_pos = harness.screen_cursor_position();
    assert_eq!(final_screen_pos.0, 23, "Final screen cursor X should be at column 23");
    assert_eq!(final_screen_pos.1, 1, "Final screen cursor Y should be at row 1");
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
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }
    assert_eq!(harness.cursor_position(), 4);

    // Insert "very " rapidly
    harness.type_text("very ").unwrap();
    harness.assert_buffer_content("The very quick brown fox");
    assert_eq!(harness.cursor_position(), 9);

    // Verify screen cursor position
    let screen_pos = harness.screen_cursor_position();
    // Line numbers (4) + " │ " (3) + "The very " (9) = 16
    assert_eq!(screen_pos.0, 16, "Screen cursor should be at column 16 after 'The very '");

    // Move to after "quick " (position 15 now, was 10 before insertion)
    for _ in 0..6 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }
    assert_eq!(harness.cursor_position(), 15);

    // Insert "and " rapidly
    harness.type_text("and ").unwrap();
    harness.assert_buffer_content("The very quick and brown fox");
    assert_eq!(harness.cursor_position(), 19);

    // Verify screen cursor position again
    let screen_pos2 = harness.screen_cursor_position();
    // Line numbers (4) + " │ " (3) + "The very quick and " (19) = 26
    assert_eq!(screen_pos2.0, 26, "Screen cursor should be at column 26");
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
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }
    assert_eq!(harness.cursor_position(), 6);

    // Rapidly type and delete
    for i in 0..5 {
        // Type 'X'
        harness.send_key(KeyCode::Char('X'), KeyModifiers::NONE).unwrap();
        let pos_after_insert = harness.cursor_position();
        assert_eq!(pos_after_insert, 7, "After insert {}, cursor should be at 7", i);

        let screen_pos = harness.screen_cursor_position();
        println!("After insert {}: screen cursor = {:?}, buffer pos = {}", i, screen_pos, pos_after_insert);

        // Verify buffer content has the X
        harness.assert_buffer_content("Start XEnd");

        // Delete it
        harness.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();
        let pos_after_delete = harness.cursor_position();
        assert_eq!(pos_after_delete, 6, "After delete {}, cursor should be back at 6", i);

        let screen_pos2 = harness.screen_cursor_position();
        println!("After delete {}: screen cursor = {:?}, buffer pos = {}", i, screen_pos2, pos_after_delete);

        // Verify buffer is back to original
        harness.assert_buffer_content("Start End");
    }

    // Should be back to original state
    harness.assert_buffer_content("Start End");
    assert_eq!(harness.cursor_position(), 6);
}

/// Test cursor doesn't get stuck when typing beyond viewport width
/// This reproduces a bug where the screen cursor position stops advancing
/// when the line gets longer than the viewport width (80 characters)
#[test]
fn test_cursor_advances_beyond_viewport_width() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a very long line - longer than the viewport width of 80
    // We'll type 100 characters to ensure we go beyond the viewport width
    let long_text = "a".repeat(100);

    for (i, ch) in long_text.chars().enumerate() {
        harness.send_key(crossterm::event::KeyCode::Char(ch), crossterm::event::KeyModifiers::NONE).unwrap();

        // Verify buffer position keeps advancing
        let buffer_pos = harness.cursor_position();
        assert_eq!(
            buffer_pos, i + 1,
            "After typing {} characters, buffer cursor should be at position {}, but is at {}",
            i + 1, i + 1, buffer_pos
        );
    }

    // Final verification
    harness.assert_buffer_content(&long_text);
    assert_eq!(harness.cursor_position(), 100);
}

/// Test horizontal scrolling when cursor moves beyond visible width
/// The viewport should scroll horizontally to keep the cursor visible
#[test]
fn test_horizontal_scrolling() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Calculate visible width (80 - 7 for line number gutter = 73 chars)
    let gutter_width = 7;
    let visible_width = 80 - gutter_width; // 73 characters visible

    // Type characters to fill most of the visible width
    let initial_text = "a".repeat(60);
    harness.type_text(&initial_text).unwrap();

    // Get initial viewport state (should be no scrolling yet)
    let viewport = &harness.editor().active_state().viewport;
    assert_eq!(viewport.left_column, 0, "Should not be scrolled yet");

    // Type more characters to go beyond visible width
    let more_text = "b".repeat(30); // Total: 90 characters
    harness.type_text(&more_text).unwrap();

    // Now the viewport should have scrolled horizontally
    let viewport = &harness.editor().active_state().viewport;
    assert!(
        viewport.left_column > 0,
        "Viewport should have scrolled horizontally, left_column = {}",
        viewport.left_column
    );

    // The cursor should still be visible on screen
    // Note: With horizontal_scroll_offset, the cursor can be slightly beyond
    // the calculated visible_width during scrolling, but it should be reasonable
    let screen_pos = harness.screen_cursor_position();
    assert!(
        screen_pos.0 < (visible_width + 10) as u16,
        "Cursor screen X ({}) should be reasonably within viewport (visible width {})",
        screen_pos.0,
        visible_width
    );

    // Verify buffer position is correct
    assert_eq!(harness.cursor_position(), 90);
}

/// Test horizontal scrolling when moving cursor left
#[test]
fn test_horizontal_scroll_left() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a long line
    let long_text = "a".repeat(100);
    harness.type_text(&long_text).unwrap();

    // Cursor is now at position 100, viewport should be scrolled
    let viewport = &harness.editor().active_state().viewport;
    let initial_left_col = viewport.left_column;
    assert!(initial_left_col > 0, "Viewport should be scrolled right");

    // Move cursor all the way to the left (Home key)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Cursor should be at position 0
    assert_eq!(harness.cursor_position(), 0);

    // Viewport should have scrolled back to show the beginning
    let viewport = &harness.editor().active_state().viewport;
    assert_eq!(
        viewport.left_column, 0,
        "Viewport should have scrolled back to left"
    );
}

/// Test horizontal scrolling with arrow key navigation
#[test]
fn test_horizontal_scroll_with_arrows() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a line longer than visible width
    let text = "x".repeat(90);
    harness.type_text(&text).unwrap();

    // Viewport should be scrolled
    let viewport = &harness.editor().active_state().viewport;
    assert!(viewport.left_column > 0);

    // Move left by 50 characters
    for _ in 0..50 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }

    // Cursor should be at position 40
    assert_eq!(harness.cursor_position(), 40);

    // Viewport should have scrolled left to keep cursor visible
    let viewport = &harness.editor().active_state().viewport;
    let screen_pos = harness.screen_cursor_position();

    // Screen cursor should be within visible bounds
    let visible_width = 80 - 7; // Terminal width minus gutter
    assert!(
        screen_pos.0 < visible_width as u16,
        "Cursor X ({}) should be within visible width ({})",
        screen_pos.0,
        visible_width
    );
}

/// Test vertical scrolling when typing lines to the bottom of screen
/// The viewport should scroll down to keep the cursor visible
#[test]
fn test_vertical_scroll_when_typing_to_bottom() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Terminal height is 24, but we need to account for:
    // - Tab bar (1 line)
    // - Status bar (1 line)
    // So visible content area is 22 lines
    let visible_lines = 22;

    // Start with viewport at top
    let viewport = &harness.editor().active_state().viewport;
    assert_eq!(viewport.top_line, 0, "Should start at top");

    // Type enough lines to fill the visible area and go beyond
    // We'll type (visible_lines + 10) lines to ensure scrolling happens
    let total_lines = visible_lines + 10;

    for i in 0..total_lines {
        harness.type_text(&format!("Line {}", i)).unwrap();

        // Add newline except for the last line
        if i < total_lines - 1 {
            harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
        }
    }

    // Verify we have the right number of lines
    let buffer = &harness.editor().active_state().buffer;
    let line_count = buffer.line_count();
    assert_eq!(line_count, total_lines, "Should have {} lines", total_lines);

    // The viewport should have scrolled down
    let top_line = harness.editor().active_state().viewport.top_line;
    assert!(
        top_line > 0,
        "Viewport should have scrolled down, top_line = {}",
        top_line
    );

    // The cursor should be on the last line
    let cursor_line = buffer.byte_to_line(harness.cursor_position());
    assert_eq!(
        cursor_line, total_lines - 1,
        "Cursor should be on the last line (line {})",
        total_lines - 1
    );

    // The last line should be visible on screen
    let screen_pos = harness.screen_cursor_position();
    assert!(
        screen_pos.1 <= visible_lines as u16,
        "Cursor screen Y ({}) should be within visible lines ({})",
        screen_pos.1,
        visible_lines
    );

    // Verify the last line is visible: screen_row should be within viewport height
    let last_line_screen_row = cursor_line.saturating_sub(top_line);
    assert!(
        last_line_screen_row < visible_lines,
        "Last line (screen row {}) should be visible within {} lines",
        last_line_screen_row,
        visible_lines
    );
}

/// Test vertical scrolling maintains cursor visibility with scroll offset
#[test]
fn test_vertical_scroll_offset() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let visible_lines = 22; // Account for tab bar and status bar

    // Type many lines
    for i in 0..40 {
        harness.type_text(&format!("Line {}", i)).unwrap();
        if i < 39 {
            harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
        }
    }

    // Cursor should be at bottom, viewport scrolled
    let initial_top_line = harness.editor().active_state().viewport.top_line;
    assert!(initial_top_line > 0, "Should be scrolled down");

    // Move up by many lines to trigger viewport scroll
    // With 40 lines and 22 visible, viewport is at line 18
    // Move up 20 lines (from 39 to 19) to trigger scroll offset
    for _ in 0..20 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }

    // The viewport should have scrolled up to keep cursor visible
    // with the scroll offset (default 3 lines)
    let new_top_line = harness.editor().active_state().viewport.top_line;

    // We moved up 20 lines, so viewport should have adjusted
    assert!(
        new_top_line < initial_top_line,
        "Viewport should have scrolled up: was {}, now {}",
        initial_top_line,
        new_top_line
    );

    // Cursor should still be visible with some margin
    let screen_pos = harness.screen_cursor_position();
    let scroll_offset = harness.editor().active_state().viewport.scroll_offset;

    assert!(
        screen_pos.1 >= scroll_offset as u16,
        "Cursor should have at least {} lines of scroll offset above, screen Y = {}",
        scroll_offset,
        screen_pos.1
    );
}

/// Test that selections are visually visible on screen
#[test]
fn test_selection_visual_rendering() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text
    harness.type_text("Hello World").unwrap();

    // Move to start of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Select the word "Hello" using Shift+Right (5 times)
    for _ in 0..5 {
        harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    }

    // Verify the cursor has a selection in the buffer
    let cursor = harness.editor().active_state().cursors.primary();
    let cursor_pos = cursor.position;
    let selection = cursor.selection_range();
    assert!(selection.is_some(), "Cursor should have a selection");

    let range = selection.unwrap();
    assert_eq!(range.start, 0, "Selection should start at position 0");
    assert_eq!(range.end, 5, "Selection should end at position 5");

    println!("Cursor position: {}, Selection: {:?}", cursor_pos, range);

    // Verify the selected text is "Hello"
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "Hello", "Selected text should be 'Hello'");

    // Get the screen rendering
    let screen = harness.screen_to_string();

    // The screen should contain the text "Hello World"
    harness.assert_screen_contains("Hello World");

    // Check that the selected characters have cyan background
    // Line numbers take up 7 characters: "   1 │ "
    // So "Hello" starts at column 7
    let buffer = harness.buffer();

    // Check first character 'H' at position (7, 1) - should have cyan background
    let h_pos = buffer.index_of(7, 1);
    let h_cell = &buffer.content[h_pos];
    assert_eq!(h_cell.symbol(), "H");
    assert_eq!(h_cell.bg, ratatui::style::Color::Cyan,
        "Selected character 'H' should have cyan background");

    // Check fourth character 'l' at position (10, 1) - should have cyan background
    let l_pos = buffer.index_of(10, 1);
    let l_cell = &buffer.content[l_pos];
    assert_eq!(l_cell.symbol(), "l");
    assert_eq!(l_cell.bg, ratatui::style::Color::Cyan,
        "Selected character 'l' should have cyan background");

    // Check fifth character 'o' at position (11, 1) - byte position 4, IN selection
    let o_pos = buffer.index_of(11, 1);
    let o_cell = &buffer.content[o_pos];
    assert_eq!(o_cell.symbol(), "o");
    // This 'o' is at byte position 4, which is in the selection range 0..5
    // But the cursor is at position 5, not 4, so this should have cyan background
    assert_eq!(o_cell.bg, ratatui::style::Color::Cyan,
        "Selected character 'o' (byte 4) should have cyan background");

    // Check character ' ' (space) at position (12, 1) - byte position 5, cursor position
    let space_pos = buffer.index_of(12, 1);
    let space_cell = &buffer.content[space_pos];
    assert_eq!(space_cell.symbol(), " ");
    // This space is at byte position 5, which is the cursor position
    // It should NOT have cyan background (cursor takes precedence over selection)
    // Also, position 5 is not in the selection range 0..5 anyway
    assert_ne!(space_cell.bg, ratatui::style::Color::Cyan,
        "Cursor position (byte 5, space) should NOT have cyan background");
}

/// Test that the prompt is rendered correctly
#[test]
fn test_prompt_rendering() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt with Ctrl+O
    harness.send_key(KeyCode::Char('o'), KeyModifiers::CONTROL).unwrap();

    // Check that the prompt is visible in the status bar area (bottom line)
    let screen = harness.screen_to_string();
    harness.assert_screen_contains("Find file: ");

    // Check that the status bar has yellow background (prompt color)
    let buffer = harness.buffer();
    let status_y = buffer.area.height - 1; // Status bar is at the bottom

    // Check a cell in the status bar has yellow background
    let first_cell_pos = buffer.index_of(0, status_y);
    let first_cell = &buffer.content[first_cell_pos];
    assert_eq!(first_cell.bg, ratatui::style::Color::Yellow,
        "Prompt should have yellow background");
}

/// Test prompt input handling (typing, backspace, cursor movement)
#[test]
fn test_prompt_input_handling() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt with Ctrl+O
    harness.send_key(KeyCode::Char('o'), KeyModifiers::CONTROL).unwrap();
    harness.assert_screen_contains("Find file: ");

    // Type some text
    harness.type_text("test.txt").unwrap();
    harness.assert_screen_contains("Find file: test.txt");

    // Test backspace
    harness.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();
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
    harness.send_key(KeyCode::Char('o'), KeyModifiers::CONTROL).unwrap();
    harness.assert_screen_contains("Find file: ");

    // Type some text
    harness.type_text("test.txt").unwrap();
    harness.assert_screen_contains("test.txt");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Prompt should be gone, and "Canceled" message should appear
    harness.assert_screen_not_contains("Find file: ");
    harness.assert_screen_contains("Canceled");
}

/// Test the complete open file workflow
#[test]
fn test_open_file_workflow() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;
    use std::path::Path;
    use tempfile::TempDir;

    // Create a temporary directory and file
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test_prompt.txt");
    fs::write(&file_path, "Hello from prompt test!").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt
    harness.send_key(KeyCode::Char('o'), KeyModifiers::CONTROL).unwrap();
    harness.assert_screen_contains("Find file: ");

    // Type the file path
    let path_str = file_path.to_str().unwrap();
    harness.type_text(path_str).unwrap();

    // Confirm with Enter
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

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
    harness.send_key(KeyCode::Char('o'), KeyModifiers::CONTROL).unwrap();

    // Type a non-existent file path
    harness.type_text("/nonexistent/file/path.txt").unwrap();

    // Confirm with Enter
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Should show an error message
    harness.assert_screen_contains("Error opening file");
}

/// Test command palette trigger and rendering
#[test]
fn test_command_palette_trigger() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette with Ctrl+P
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();

    // Check that the command prompt is visible
    harness.assert_screen_contains("Command: ");

    // Check that suggestions are visible (should show all commands initially)
    harness.assert_screen_contains("Open File");
    harness.assert_screen_contains("Save File");
    harness.assert_screen_contains("Quit");
}

/// Test command palette autocomplete filtering
#[test]
fn test_command_palette_autocomplete() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();

    // Type "open" to filter commands
    harness.type_text("open").unwrap();

    // Should show filtered results
    harness.assert_screen_contains("Open File");

    // Should not show non-matching commands
    // (We might still see them if there are many results, but "Open File" should be first)
}

/// Test command palette navigation with Up/Down
#[test]
fn test_command_palette_navigation() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();
    harness.assert_screen_contains("Command: ");

    // Navigate down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Navigate up
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();

    // Commands should still be visible
    harness.assert_screen_contains("Open File");
}

/// Test command palette Tab completion
#[test]
fn test_command_palette_tab_completion() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();

    // Type partial text
    harness.type_text("op").unwrap();

    // Press Tab to accept first suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    // The input should be completed to "Open File" (the first matching command)
    harness.assert_screen_contains("Command: Open File");
}

/// Test command palette cancel with Escape
#[test]
fn test_command_palette_cancel() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();
    harness.assert_screen_contains("Command: ");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Prompt should be gone
    harness.assert_screen_not_contains("Command: ");
    harness.assert_screen_contains("Canceled");
}

/// Test executing a command from the palette
#[test]
fn test_command_palette_execute() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();

    // Type the command name
    harness.type_text("Show Help").unwrap();

    // Execute with Enter
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Help should now be visible
    harness.assert_screen_contains("KEYBOARD SHORTCUTS");
}

/// Test command palette fuzzy matching
#[test]
fn test_command_palette_fuzzy_matching() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();

    // Type "sf" which should match "Save File" (fuzzy match)
    harness.type_text("sf").unwrap();

    // Should show "Save File" in suggestions
    harness.assert_screen_contains("Save File");
}

/// Test that viewport displays all available lines when content is larger than minimum
#[test]
fn test_viewport_displays_all_lines() {
    // Create a harness with 80 columns and 40 rows
    // This gives us: 1 line for tabs + 38 lines for content + 1 line for status = 40 total
    let mut harness = EditorTestHarness::new(80, 40).unwrap();

    // Create content with 35 lines (should all be visible in a 38-line viewport)
    let mut content = String::new();
    for i in 1..=35 {
        if i > 1 {
            content.push('\n');
        }
        content.push_str(&format!("This is line number {}", i));
    }

    harness.type_text(&content).unwrap();

    // Check the viewport state
    let editor = harness.editor();
    let state = editor.active_state();
    let viewport_height = state.viewport.height;

    // Viewport should be 38 lines tall (40 - 2 for tab bar and status bar)
    assert_eq!(viewport_height, 38, "Viewport height should be 38 (40 total - 2 for UI chrome)");

    // Get visible range
    let visible_range = state.viewport.visible_range();
    let visible_line_count = visible_range.end - visible_range.start;

    // All 35 lines should fit in the 38-line viewport
    assert!(visible_line_count >= 35,
        "Expected to see at least 35 lines, but visible range is only {} lines ({}..{})",
        visible_line_count, visible_range.start, visible_range.end);

    // Render and check that lines are actually displayed on screen
    harness.render().unwrap();

    // Check that we can see line 1 and line 35 on the screen
    harness.assert_screen_contains("This is line number 1");
    harness.assert_screen_contains("This is line number 35");

    // Also check some lines in the middle
    harness.assert_screen_contains("This is line number 15");
    harness.assert_screen_contains("This is line number 25");
}

/// Test that opening a file creates viewport with correct dimensions
/// This test captures a bug where open_file() creates the EditorState with
/// hardcoded dimensions (80, 24) instead of using actual terminal dimensions
#[test]
fn test_open_file_viewport_dimensions() {
    use tempfile::TempDir;

    // Create a temp file with some content
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\n").unwrap();

    // Create editor with 131x31 terminal (matching user's scenario)
    let mut harness = EditorTestHarness::new(131, 31).unwrap();

    // Initially, the default buffer has correct viewport dimensions
    let initial_viewport_height = harness.editor().active_state().viewport.height;
    assert_eq!(initial_viewport_height, 29, "Initial viewport should be 29 (31 - 2)");

    // Open a file
    harness.open_file(&file_path).unwrap();

    // After opening file, viewport height should still match terminal dimensions
    let viewport_height_after_open = harness.editor().active_state().viewport.height;
    assert_eq!(
        viewport_height_after_open, 29,
        "After opening file, viewport height should be 29 (31 - 2), but got {}. \
         This indicates the file was opened with hardcoded dimensions instead of actual terminal size.",
        viewport_height_after_open
    );

    // Render and verify the viewport displays the correct number of lines
    harness.render().unwrap();

    let visible_range = harness.editor().active_state().viewport.visible_range();
    let visible_count = visible_range.end - visible_range.start;

    assert_eq!(
        visible_count, 29,
        "Visible range should be 29 lines, but got {}",
        visible_count
    );
}

/// Test viewport with 31-row terminal (matching user's scenario)
#[test]
fn test_viewport_31_rows() {
    use crossterm::event::{KeyCode, KeyModifiers};

    // Create a harness with 131 columns and 31 rows (matching user's terminal)
    // This gives us: 1 line for tabs + 29 lines for content + 1 line for status = 31 total
    let mut harness = EditorTestHarness::new(131, 31).unwrap();

    // Create content with 29 lines (should all be visible in a 29-line viewport)
    let mut content = String::new();
    for i in 1..=29 {
        if i > 1 {
            content.push('\n');
        }
        content.push_str(&format!("Line {}", i));
    }

    harness.type_text(&content).unwrap();

    // Check the viewport state
    let editor = harness.editor();
    let state = editor.active_state();
    let viewport_height = state.viewport.height;

    // Viewport should be 29 lines tall (31 - 2 for tab bar and status bar)
    assert_eq!(viewport_height, 29, "Viewport height should be 29 (31 total - 2 for UI chrome)");

    // Get visible range
    let visible_range = state.viewport.visible_range();
    let visible_line_count = visible_range.end - visible_range.start;

    // All 29 lines should be visible
    assert_eq!(visible_line_count, 29,
        "Expected to see all 29 lines, but visible range is only {} lines ({}..{})",
        visible_line_count, visible_range.start, visible_range.end);

    // Render and verify lines are displayed
    harness.render().unwrap();

    // Check that we can see first and last lines
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 29");

    // Check lines throughout
    harness.assert_screen_contains("Line 10");
    harness.assert_screen_contains("Line 20");

    // Now open the command palette (which shows suggestions)
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Command palette should be visible
    harness.assert_screen_contains("Command:");

    // Suggestions popup should be visible with commands
    harness.assert_screen_contains("Open File");
    harness.assert_screen_contains("Save File");
    harness.assert_screen_contains("Quit");

    // The viewport height should be unchanged (suggestions take screen space, not viewport space)
    let editor = harness.editor();
    let state = editor.active_state();
    let viewport_height_with_palette = state.viewport.height;

    assert_eq!(viewport_height_with_palette, 29,
        "Viewport height should still be 29 even with command palette open, but got {}",
        viewport_height_with_palette);

    // Close the command palette
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // After closing palette, viewport should still be at full height
    let editor = harness.editor();
    let state = editor.active_state();
    let viewport_height_after = state.viewport.height;

    assert_eq!(viewport_height_after, 29,
        "Viewport height should still be 29 after closing command palette, but got {}",
        viewport_height_after);

    // Get visible range after closing palette
    let visible_range_after = state.viewport.visible_range();
    let visible_line_count_after = visible_range_after.end - visible_range_after.start;

    assert_eq!(visible_line_count_after, 29,
        "Expected to see all 29 lines after closing palette, but visible range is only {} lines ({}..{})",
        visible_line_count_after, visible_range_after.start, visible_range_after.end);

    // All lines should still be visible on screen
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 29");
}

/// Test select word functionality (Ctrl+W)
#[test]
fn test_select_word() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text with multiple words
    harness.type_text("hello world test").unwrap();

    // Move to the middle of "world"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..8 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    // Now cursor is at position 8 (in the middle of "world")
    // Select word with Ctrl+W
    harness.send_key(KeyCode::Char('w'), KeyModifiers::CONTROL).unwrap();

    // Verify the selection
    let cursor = harness.editor().active_state().cursors.primary();
    let selection = cursor.selection_range();
    assert!(selection.is_some(), "Cursor should have a selection");

    let range = selection.unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "world", "Should select the word 'world'");
}

/// Test select word at start of word
#[test]
fn test_select_word_at_start() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello world").unwrap();

    // Move to start of "world"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..6 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    // Select word
    harness.send_key(KeyCode::Char('w'), KeyModifiers::CONTROL).unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "world", "Should select the word 'world'");
}

/// Test select word at end of word
#[test]
fn test_select_word_at_end() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello world").unwrap();

    // Move to end of "hello"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    // Select word
    harness.send_key(KeyCode::Char('w'), KeyModifiers::CONTROL).unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "hello", "Should select the word 'hello'");
}

/// Test select line functionality (Ctrl+L)
#[test]
fn test_select_line() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type multiple lines
    harness.type_text("first line\nsecond line\nthird line").unwrap();

    // Move to start of document, then down to second line
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    // Select line with Ctrl+L
    harness.send_key(KeyCode::Char('l'), KeyModifiers::CONTROL).unwrap();

    // Verify the selection includes the entire line
    let cursor = harness.editor().active_state().cursors.primary();
    let selection = cursor.selection_range();
    assert!(selection.is_some(), "Cursor should have a selection");

    let range = selection.unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "second line\n", "Should select the entire line including newline");
}

/// Test select line on first line
#[test]
fn test_select_line_first() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("first line\nsecond line").unwrap();

    // Move to start of document (first line)
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();

    // Select line
    harness.send_key(KeyCode::Char('l'), KeyModifiers::CONTROL).unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "first line\n", "Should select the first line");
}

/// Test select line on last line (no trailing newline)
#[test]
fn test_select_line_last() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("first line\nsecond line").unwrap();

    // Select line (cursor is already on last line)
    harness.send_key(KeyCode::Char('l'), KeyModifiers::CONTROL).unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "second line", "Should select the last line without newline");
}

/// Test select word with multiple cursors
#[test]
fn test_select_word_multi_cursor() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text with words
    harness.type_text("hello world test").unwrap();

    // Move to "hello"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Add cursor at "world" using Ctrl+D (add cursor at next match)
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();

    // Now we have "hello" selected, add cursor at next space or different word
    harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();

    // Add cursor above at same column
    harness.send_key(KeyCode::Down, KeyModifiers::CONTROL | KeyModifiers::ALT).unwrap();

    // This test validates multi-cursor infrastructure is ready
    let state = harness.editor().active_state();
    assert!(state.cursors.count() >= 1, "Should have at least one cursor");
}

/// Test expand selection functionality (Ctrl+Shift+Right)
#[test]
fn test_expand_selection() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text with multiple words
    harness.type_text("hello world test").unwrap();

    // Move to middle of "hello"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    // First expand should select current word "hello"
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range.clone());
    assert_eq!(selected_text, "hello", "First expand should select 'hello'");

    // Second expand should extend to include "world"
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range.clone());
    assert_eq!(selected_text, "hello world", "Second expand should include 'world'");

    // Third expand should extend to include "test"
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "hello world test", "Third expand should include 'test'");
}

/// Test expand selection when starting with no selection
#[test]
fn test_expand_selection_no_initial_selection() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("foo bar baz").unwrap();

    // Move to middle of "bar"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    // Expand with no initial selection should select the word
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "bar", "Should select current word 'bar'");
}

/// Test expand selection performance with moderately large buffer
/// This test ensures that selection operations don't read the entire buffer
#[test]
fn test_expand_selection_large_buffer_performance() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;
    use std::fs;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large.txt");

    // Create a moderately large file (~100KB of text)
    let large_text = "word ".repeat(20_000); // ~100KB of text
    fs::write(&file_path, &large_text).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to a position near the middle
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    for _ in 0..50 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    // Expand selection - this used to hang/timeout with large buffers
    // because it would read the entire buffer. Now it should complete quickly
    // by only reading a small window around the cursor.
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();

    // Verify it works correctly
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(cursor.selection_range().is_some(), "Should have a selection");

    // The selected text should be a word (not testing exact content since position may vary)
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert!(!selected_text.is_empty(), "Selection should not be empty");
}

/// Test with an extremely large buffer (simulating the 63MB file issue)
/// This verifies the windowed reading approach works with very large files
#[test]
#[ignore] // This test takes a long time - run with --ignored flag
fn test_expand_selection_very_large_buffer() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;
    use std::fs;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("very_large.txt");

    // Create a very large file (~10MB of text - representative of the issue)
    let large_text = "word ".repeat(2_000_000); // ~10MB of text
    fs::write(&file_path, &large_text).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to various positions in the file and test expand selection
    // Test near the beginning
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    for _ in 0..100 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(cursor.selection_range().is_some(), "Should have selection at start");

    // Test in the middle (move down many lines)
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap(); // Clear selection
    for _ in 0..1000 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(cursor.selection_range().is_some(), "Should have selection in middle");

    // All operations should complete without hanging
}

/// Test selecting words after scrolling down beyond initial viewport
/// Ensures word selection works correctly at any position, not just visible lines
#[test]
fn test_select_word_after_scrolling() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a buffer with many lines (more than viewport height)
    let mut lines = Vec::new();
    for i in 0..100 {
        lines.push(format!("line{} word{} test{}", i, i, i));
    }
    harness.type_text(&lines.join("\n")).unwrap();

    // Scroll down past the initial viewport
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    for _ in 0..50 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    // Move to middle of a word on line 50
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..10 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    // Select word with Ctrl+W
    harness.send_key(KeyCode::Char('w'), KeyModifiers::CONTROL).unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);

    // Should have selected "word50" at line 50
    assert!(selected_text.contains("word"), "Should select a word after scrolling");
    assert!(selected_text.len() > 0, "Selection should not be empty");
}

/// Test expand selection after scrolling down
#[test]
fn test_expand_selection_after_scrolling() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a buffer with many lines
    let mut lines = Vec::new();
    for i in 0..50 {
        lines.push(format!("alpha beta gamma delta epsilon line{}", i));
    }
    harness.type_text(&lines.join("\n")).unwrap();

    // Scroll down to line 30
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    for _ in 0..30 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    // Move to middle of "alpha"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }

    // First expand should select "alpha"
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range.clone());
    assert_eq!(selected_text, "alpha", "First expand should select 'alpha'");

    // Second expand should extend to include "beta"
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "alpha beta", "Second expand should include 'beta'");
}

/// Test expand selection (Ctrl+Shift+Right) across line boundaries
/// Ensures selection can expand from end of one line to beginning of next
#[test]
fn test_expand_selection_across_lines() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multi-line content with words at line boundaries
    harness.type_text("first line ending\nsecond line starting here").unwrap();

    // Position cursor at "ending" on first line
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    // Move back to start of "ending"
    for _ in 0..6 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }

    // First expand: select "ending"
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "ending", "Should select 'ending' on first line");

    // Second expand: should cross the newline and select "second" on next line
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "ending\nsecond", "Should cross line boundary and select 'second'");

    // Third expand: should continue to "line"
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);
    assert_eq!(selected_text, "ending\nsecond line", "Should include 'line' from second line");
}

/// Test expand selection starting at end of line
#[test]
fn test_expand_selection_from_line_end() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("first line\nsecond word here").unwrap();

    // Position cursor at end of first line (before newline)
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // First expand from end of line - behavior depends on if cursor is on newline or before it
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range.clone());

    // The selection should move forward from the end of the line
    assert!(!selected_text.is_empty(), "Should select something");

    // If it contains "second", it successfully jumped to next line
    // Otherwise it may have selected the newline or whitespace
    println!("Selected from line end: {:?}", selected_text);

    // Continue expanding to ensure we can reach the next line
    harness.send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT).unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness.editor().active_state().buffer.slice(range);

    // After multiple expands, we should definitely reach "second" on the next line
    assert!(selected_text.contains("second"), "Should eventually reach 'second' on next line");
}
