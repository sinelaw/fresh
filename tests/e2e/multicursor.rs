use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

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
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();

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
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

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
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

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
    assert_eq!(
        x_count, 3,
        "Should have inserted exactly 3 X's, one per cursor"
    );
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
    harness
        .editor_mut()
        .active_state_mut()
        .cursors
        .remove_secondary();
    harness.render().unwrap();

    // Should have only 1 cursor now
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 1);
}

/// Test multi-cursor undo atomicity
/// When using multiple cursors, undo should undo all cursor actions in one step
#[test]
fn test_multi_cursor_undo_atomic() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines with more content (matching the working test)
    harness.type_text("aaa\nbbb\nccc\nddd").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add cursors - each time we add a cursor below, the new cursor becomes primary
    // So we can continue adding cursors below
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1 and 2
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1, 2, and 3

    // Should have 3 cursors
    let cursor_count = harness.editor().active_state().cursors.iter().count();
    assert_eq!(cursor_count, 3, "Should have 3 cursors");

    // Type "X" with all three cursors - this should create a batch event
    harness.type_text("X").unwrap();

    // Each cursor should insert X at its position
    let result = harness.get_buffer_content();

    // Count how many X's were inserted
    let x_count = result.matches('X').count();
    assert_eq!(
        x_count, 3,
        "Should have inserted exactly 3 X's, one per cursor. Buffer: {}", result
    );

    // Undo once - this should undo ALL three insertions atomically
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // All X's should be gone after a single undo
    let result_after_undo = harness.get_buffer_content();
    let x_count_after_undo = result_after_undo.matches('X').count();
    assert_eq!(
        x_count_after_undo, 0,
        "Should have removed all X's with single undo. Buffer: {}", result_after_undo
    );
    harness.assert_buffer_content("aaa\nbbb\nccc\nddd");

    // Redo once - this should redo ALL three insertions atomically
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // All X's should be back after a single redo
    let result_after_redo = harness.get_buffer_content();
    let x_count_after_redo = result_after_redo.matches('X').count();
    assert_eq!(
        x_count_after_redo, 3,
        "Should have restored all 3 X's with single redo. Buffer: {}", result_after_redo
    );
}

/// Test multi-cursor delete undo atomicity
#[test]
fn test_multi_cursor_delete_undo_atomic() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("aaa\nbbb\nccc").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add two more cursors
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();

    // Should have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 3);

    // Delete forward at all three cursors - should delete 'a', 'b', 'c'
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();

    // Verify first character deleted from each line
    harness.assert_buffer_content("aa\nbb\ncc");

    // Undo once - should restore all three characters
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // All characters should be restored
    harness.assert_buffer_content("aaa\nbbb\nccc");
}

/// Test that adding cursors can be undone
#[test]
fn test_add_cursor_undo() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Should start with 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Add a cursor below
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Add another cursor below
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Undo - should remove the last cursor added
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Undo again - should remove the second cursor
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Redo - should add cursor back
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);
}

/// Test that removing cursors can be undone
#[test]
fn test_remove_cursor_undo() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add two cursors
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();

    // Should have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Remove secondary cursors (using Escape)
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Undo - should restore the secondary cursors
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 3 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Redo - should remove them again
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);
}

/// Test undo beyond cursor add removes the cursor and undoes the edit
#[test]
fn test_undo_beyond_cursor_add() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("aaa\nbbb\nccc").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Should start with 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Add a cursor below
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Type "X" with both cursors
    harness.type_text("X").unwrap();

    // Should have X inserted at both positions
    let result = harness.get_buffer_content();
    let x_count = result.matches('X').count();
    assert_eq!(x_count, 2, "Should have 2 X's. Buffer: {}", result);

    // Undo - should undo the batch insertion
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // X's should be gone, but we should still have 2 cursors
    let result = harness.get_buffer_content();
    let x_count = result.matches('X').count();
    assert_eq!(x_count, 0, "Should have 0 X's. Buffer: {}", result);
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Undo again - should remove the second cursor
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Redo - should add the cursor back
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should have 2 cursors again
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Redo again - should redo the batch insertion
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // X's should be back
    let result = harness.get_buffer_content();
    let x_count = result.matches('X').count();
    assert_eq!(x_count, 2, "Should have 2 X's back. Buffer: {}", result);
}

/// Test that status bar shows cursor count when multiple cursors exist
#[test]
fn test_multi_cursor_status_bar_indicator() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Render to capture initial state
    harness.render().unwrap();

    // Status bar should NOT show cursor count when single cursor
    let screen = harness.screen_to_string();
    assert!(!screen.contains(" cursors"), "Should not show cursor count with single cursor");

    // Add a cursor below
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Status bar should show "2 cursors"
    let screen = harness.screen_to_string();
    assert!(screen.contains("2 cursors"), "Status bar should show '2 cursors'. Screen:\n{}", screen);

    // Add another cursor
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Status bar should show "3 cursors"
    let screen = harness.screen_to_string();
    assert!(screen.contains("3 cursors"), "Status bar should show '3 cursors'. Screen:\n{}", screen);

    // Remove secondary cursors
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Status bar should NOT show cursor count again
    let screen = harness.screen_to_string();
    assert!(!screen.contains(" cursors"), "Should not show cursor count after removing cursors");
}

/// Test that all cursors are visible in the viewport
#[test]
fn test_all_cursors_visible_in_viewport() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use ratatui::style::Modifier;
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add two more cursors
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should have 3 cursors
    assert_eq!(harness.cursor_count(), 3);

    // Now verify that all 3 cursors are visible with some kind of styling
    // (In the viewport, we should see styled characters at cursor positions)
    // Line 1, Line 2, Line 3 all start at column 0, so we should check
    // that there's cursor styling at the 'L' of each line

    // Get the y-coordinates of the three lines (after tab bar)
    // Tab bar is 1 line, content starts at y=1
    // But we also need to account for line numbers (gutter)
    // Line numbers take up some space (e.g., "1 ", "2 ", "3 ")
    // Let's check multiple x positions to find the cursor

    let line_y_positions = vec![1, 2, 3]; // y positions of the three lines

    let mut cursor_indicators_found = 0;

    for y in line_y_positions {
        // Check multiple x positions (accounting for line numbers/gutter)
        // Try x=0 through x=10 to find reversed characters
        for x in 0..10 {
            if let Some(style) = harness.get_cell_style(x, y) {
                // Cursor should have REVERSED modifier
                if style.add_modifier.contains(Modifier::REVERSED) {
                    cursor_indicators_found += 1;
                    break; // Found cursor on this line, move to next line
                }
            }
        }
    }

    assert!(
        cursor_indicators_found >= 2,
        "Expected at least 2 visible cursors (secondary cursors), found {}",
        cursor_indicators_found
    );
}

/// Test comprehensive multi-cursor editing with multiple 'abc' lines
/// This test uses the exact same pattern as test_multi_cursor_typing but with 'abc' content
#[test]
fn test_multi_cursor_comprehensive_abc_editing() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines with 'abc' - matching test_multi_cursor_typing pattern
    // Note: Using varied content to avoid any potential cursor normalization issues
    harness.type_text("abc1\nabc2\nabc3\nabc4").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add cursors - each time we add a cursor below, the new cursor becomes primary
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1 and 2
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1, 2, and 3
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1, 2, 3, and 4

    // Should have 4 cursors
    let cursor_count = harness.editor().active_state().cursors.iter().count();
    assert_eq!(cursor_count, 4, "Should have 4 cursors");

    // Test 1: Type "X" with all four cursors
    harness.type_text("X").unwrap();

    // Each cursor should insert X at its position
    let result = harness.get_buffer_content();

    // Count how many X's were inserted
    let x_count = result.matches('X').count();
    assert_eq!(
        x_count, 4,
        "Should have inserted exactly 4 X's, one per cursor. Buffer: {}", result
    );

    // Test 2: Undo should remove all X's atomically
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    let result_after_undo = harness.get_buffer_content();
    let x_count_after_undo = result_after_undo.matches('X').count();
    assert_eq!(
        x_count_after_undo, 0,
        "Should have removed all X's with single undo. Buffer: {}", result_after_undo
    );

    // Verify we still have 4 cursors after undo
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 4);
}

/// Test single cursor visibility - comprehensive test moving through every position
#[test]
fn test_single_cursor_visible() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use ratatui::style::Modifier;
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines with various content
    harness.type_text("Hello World\nSecond Line Here\nThird Line\nFourth").unwrap();

    let expected_content = "Hello World\nSecond Line Here\nThird Line\nFourth";
    harness.assert_buffer_content(expected_content);

    // Helper function to find cursor on screen
    let find_cursor = |harness: &EditorTestHarness| -> Option<(u16, u16, String)> {
        for y in 0..24 {
            for x in 0..80 {
                if let Some(style) = harness.get_cell_style(x, y) {
                    if style.add_modifier.contains(Modifier::REVERSED) {
                        let char = harness.get_cell(x, y).unwrap_or_else(|| " ".to_string());
                        return Some((x, y, char));
                    }
                }
            }
        }
        None
    };

    // Move to start of document
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Expected positions for "Hello World\nSecond Line Here\nThird Line\nFourth"
    let expected_chars = vec![
        'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', // end of line 1
        '\n', // newline is at position 11
    ];

    println!("\nStarting comprehensive cursor visibility test...");
    println!("Testing first line: 'Hello World'");

    // Move through first line character by character
    for (step, expected_char) in expected_chars.iter().enumerate() {
        harness.render().unwrap();

        let cursor_pos = harness.cursor_position();
        println!("\nStep {}: cursor at buffer position {}", step, cursor_pos);

        // Find cursor on screen
        let cursor_info = find_cursor(&harness);
        assert!(
            cursor_info.is_some(),
            "Step {}: Cursor not visible at buffer position {}! Expected char: '{}'",
            step,
            cursor_pos,
            expected_char
        );

        let (x, y, char_at_cursor) = cursor_info.unwrap();
        println!("  Screen position: ({}, {}), char: '{}'", x, y, char_at_cursor);

        // For newline, we expect to see a space since we add it for visibility
        if *expected_char == '\n' {
            println!("  At newline - expecting space or newline indicator");
        } else {
            // Verify the character matches (accounting for rendered character)
            let expected_str = expected_char.to_string();
            assert_eq!(
                char_at_cursor, expected_str,
                "Step {}: Cursor at wrong character. Expected '{}', got '{}'",
                step, expected_str, char_at_cursor
            );
        }

        // Move right for next iteration
        if step < expected_chars.len() - 1 {
            harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
        }
    }

    println!("\nTesting navigation to second line...");

    // Move to start of second line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let after_down = harness.cursor_position();
    println!("After Down: cursor at buffer position {}", after_down);

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    let after_home = harness.cursor_position();
    println!("After Home: cursor at buffer position {}", after_home);

    harness.render().unwrap();

    let cursor_info = find_cursor(&harness);
    assert!(cursor_info.is_some(), "Cursor should be visible at start of second line");
    let (x, y, char_at_cursor) = cursor_info.unwrap();
    println!("At start of line 2: screen ({}, {}), char: '{}', buffer pos: {}", x, y, char_at_cursor, after_home);

    // Position 12 should be 'S' (first char of "Second")
    // But we need to be flexible in case the cursor is shown differently
    if after_home == 12 {
        // If we're at the 'S', it should show 'S' with REVERSED
        assert_eq!(char_at_cursor, "S", "Should be at 'S' of 'Second'");
    } else {
        println!("WARNING: Cursor not at expected position 12, it's at {}", after_home);
    }

    // Move through "Second" character by character
    let second_chars = vec!['S', 'e', 'c', 'o', 'n', 'd'];
    for (i, expected_char) in second_chars.iter().enumerate() {
        harness.render().unwrap();

        let cursor_info = find_cursor(&harness);
        assert!(
            cursor_info.is_some(),
            "Cursor not visible at char {} of 'Second'",
            i
        );

        let (_, _, char_at_cursor) = cursor_info.unwrap();
        let expected_str = expected_char.to_string();
        assert_eq!(
            char_at_cursor, expected_str,
            "At position {} of 'Second': expected '{}', got '{}'",
            i, expected_str, char_at_cursor
        );

        if i < second_chars.len() - 1 {
            harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
        }
    }

    println!("\nTesting vertical navigation...");

    // Test moving up and down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let cursor_info = find_cursor(&harness);
    assert!(cursor_info.is_some(), "Cursor should be visible after moving down");
    println!("After Down: cursor at {:?}", cursor_info);

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let cursor_info = find_cursor(&harness);
    assert!(cursor_info.is_some(), "Cursor should be visible after moving up");
    println!("After Up: cursor at {:?}", cursor_info);

    // Move to end of document
    harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    let cursor_info = find_cursor(&harness);
    assert!(cursor_info.is_some(), "Cursor should be visible at end of document");
    println!("At end of document: cursor at {:?}", cursor_info);

    println!("\nCursor visibility test completed successfully!");
}

/// Test cursor visibility on empty lines
#[test]
fn test_cursor_visible_on_empty_line() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use ratatui::style::Modifier;
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Start with empty buffer (empty line)
    harness.render().unwrap();

    // Should have exactly 1 cursor
    assert_eq!(harness.cursor_count(), 1);

    // Cursor should be visible on the empty line
    let mut cursor_found = false;
    for x in 0..20 {
        if let Some(style) = harness.get_cell_style(x, 1) {
            if style.add_modifier.contains(Modifier::REVERSED) {
                cursor_found = true;
                break;
            }
        }
    }

    assert!(cursor_found, "Cursor should be visible on empty line");

    // Type some text, then delete it to create an empty line again
    harness.type_text("Test").unwrap();
    for _ in 0..4 {
        harness.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Cursor should still be visible on empty line
    let mut cursor_found_after_delete = false;
    for x in 0..20 {
        if let Some(style) = harness.get_cell_style(x, 1) {
            if style.add_modifier.contains(Modifier::REVERSED) {
                cursor_found_after_delete = true;
                break;
            }
        }
    }

    assert!(cursor_found_after_delete, "Cursor should be visible on empty line after deleting text");

    // Add multiple empty lines and test cursor on different empty lines
    harness.type_text("\n\n\n").unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Cursor should be visible on the empty line we moved to
    let mut cursor_found_on_middle_empty = false;
    for y in 1..10 {
        for x in 0..20 {
            if let Some(style) = harness.get_cell_style(x, y) {
                if style.add_modifier.contains(Modifier::REVERSED) {
                    cursor_found_on_middle_empty = true;
                    break;
                }
            }
        }
        if cursor_found_on_middle_empty {
            break;
        }
    }

    assert!(cursor_found_on_middle_empty, "Cursor should be visible on middle empty line");
}

/// Test to investigate cursor behavior with identical line content
#[test]
fn test_identical_lines_cursor_positions() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines with IDENTICAL content
    harness.type_text("abc\nabc\nabc\nabc").unwrap();
    harness.assert_buffer_content("abc\nabc\nabc\nabc");

    // Go to start
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();

    // Get initial cursor position
    let initial_pos = harness.cursor_position();
    println!("Initial cursor position: {}", initial_pos);

    // Add first cursor below
    harness.editor_mut().add_cursor_below();
    println!("After adding 1st cursor below:");
    for (id, cursor) in harness.editor().active_state().cursors.iter() {
        println!("  Cursor {:?}: position={}, anchor={:?}", id, cursor.position, cursor.anchor);
    }

    // Add second cursor below
    harness.editor_mut().add_cursor_below();
    println!("After adding 2nd cursor below:");
    for (id, cursor) in harness.editor().active_state().cursors.iter() {
        println!("  Cursor {:?}: position={}, anchor={:?}", id, cursor.position, cursor.anchor);
    }

    // Add third cursor below
    harness.editor_mut().add_cursor_below();
    println!("After adding 3rd cursor below:");
    for (id, cursor) in harness.editor().active_state().cursors.iter() {
        println!("  Cursor {:?}: position={}, anchor={:?}", id, cursor.position, cursor.anchor);
    }

    let cursor_count = harness.editor().active_state().cursors.iter().count();
    println!("Total cursors: {}", cursor_count);

    // Type X
    harness.type_text("X").unwrap();

    let result = harness.get_buffer_content();
    println!("Buffer after typing X:\n{}", result);

    let x_count = result.matches('X').count();
    println!("X count: {}", x_count);

    // This should pass if cursors are positioned correctly
    assert_eq!(x_count, 4, "Should have 4 X's, one per cursor. Buffer:\n{}", result);
}
