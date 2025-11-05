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

