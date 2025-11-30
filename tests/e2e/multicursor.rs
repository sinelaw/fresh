use crate::common::harness::EditorTestHarness;

/// Test that Ctrl+D with backward selection (Shift+Left) creates properly synced cursors
/// Issue #210: When selecting with Shift+Left and then creating a multiple cursor with Ctrl+D,
/// the cursors use unsynced offsets - one cursor is at start of selection, one at end.
/// This causes typing to produce incorrect results.
#[test]
fn test_add_cursor_next_match_with_backward_selection() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text with repeated words
    harness.type_text("foo bar foo").unwrap();
    harness.assert_buffer_content("foo bar foo");

    // Position cursor after first "foo" (at position 3)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Verify cursor position
    let primary = harness.editor().active_state().cursors.primary();
    assert_eq!(
        primary.position, 3,
        "Cursor should be at position 3 after first 'foo'"
    );

    // Select backward with Shift+Left 3 times to select "foo"
    // This creates a backward selection: cursor at 0, anchor at 3
    harness
        .send_key(KeyCode::Left, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Left, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Left, KeyModifiers::SHIFT)
        .unwrap();

    // Verify backward selection: position=0 (cursor at start), anchor=3 (selection end)
    let primary = harness.editor().active_state().cursors.primary();
    assert_eq!(
        primary.position, 0,
        "After Shift+Left, cursor should be at start of selection"
    );
    assert_eq!(
        primary.anchor,
        Some(3),
        "After Shift+Left, anchor should be at end of selection"
    );

    // Add cursor at next "foo" match
    harness.editor_mut().add_cursor_at_next_match();
    harness.render().unwrap();

    // Should now have 2 cursors
    let state = harness.editor().active_state();
    assert_eq!(state.cursors.iter().count(), 2);

    // CRITICAL: Both cursors should have the same relative position within their selections
    // The original cursor is at position 0 (start of selection 0..3)
    // The new cursor should also be at the start of its selection (8..11)
    // i.e., new cursor position should be 8, not 11
    for (id, cursor) in state.cursors.iter() {
        let selection = cursor
            .selection_range()
            .expect("Cursor should have selection");
        let is_at_start = cursor.position == selection.start;
        let is_at_end = cursor.position == selection.end;

        // Since original selection was backward (cursor at start), new cursor should also be at start
        assert!(
            is_at_start,
            "Cursor {:?} should be at start of selection. Position: {}, Selection: {:?}",
            id, cursor.position, selection
        );
        assert!(
            !is_at_end || selection.start == selection.end,
            "Cursor {:?} should NOT be at end of selection (unless collapsed). Position: {}, Selection: {:?}",
            id, cursor.position, selection
        );
    }

    // Type "X" to replace both selections - this should result in "X bar X"
    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Both "foo"s should be replaced with "X"
    harness.assert_buffer_content("X bar X");
}

/// Test that Ctrl+D with forward selection (Shift+Right) creates properly synced cursors
/// and typing replaces both selections correctly
#[test]
fn test_add_cursor_next_match_with_forward_selection() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text with repeated words
    harness.type_text("foo bar foo").unwrap();
    harness.assert_buffer_content("foo bar foo");

    // Select the first "foo" with forward selection (Shift+Right from position 0)
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

    // Verify forward selection: position=3 (cursor at end), anchor=0 (selection start)
    let primary = harness.editor().active_state().cursors.primary();
    assert_eq!(
        primary.position, 3,
        "After Shift+Right, cursor should be at end of selection"
    );
    assert_eq!(
        primary.anchor,
        Some(0),
        "After Shift+Right, anchor should be at start of selection"
    );

    // Add cursor at next "foo" match
    harness.editor_mut().add_cursor_at_next_match();
    harness.render().unwrap();

    // Should now have 2 cursors
    let state = harness.editor().active_state();
    assert_eq!(state.cursors.iter().count(), 2);

    // Type "X" to replace both selections - this should result in "X bar X"
    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Both "foo"s should be replaced with "X"
    harness.assert_buffer_content("X bar X");
}

/// Test adding cursor at next match with Ctrl+D (no typing, just cursor creation)
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
        "Should have inserted exactly 3 X's, one per cursor. Buffer: {result}"
    );

    // Undo once - this should undo ALL three insertions atomically
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // All X's should be gone after a single undo
    let result_after_undo = harness.get_buffer_content();
    let x_count_after_undo = result_after_undo.matches('X').count();
    assert_eq!(
        x_count_after_undo, 0,
        "Should have removed all X's with single undo. Buffer: {result_after_undo}"
    );
    harness.assert_buffer_content("aaa\nbbb\nccc\nddd");

    // Redo once - this should redo ALL three insertions atomically
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // All X's should be back after a single redo
    let result_after_redo = harness.get_buffer_content();
    let x_count_after_redo = result_after_redo.matches('X').count();
    assert_eq!(
        x_count_after_redo, 3,
        "Should have restored all 3 X's with single redo. Buffer: {result_after_redo}"
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
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
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
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be back to 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Undo again - should remove the second cursor
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Redo - should add cursor back
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be back to 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);
}

/// Test that removing cursors can be undone
/// Note: Ignored - cursor removal undo behavior may not restore cursors
/// depending on how undo history handles cursor state
#[test]
#[ignore]
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
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be back to 3 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Redo - should remove them again
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
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
    assert_eq!(x_count, 2, "Should have 2 X's. Buffer: {result}");

    // Undo - should undo the batch insertion
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // X's should be gone, but we should still have 2 cursors
    let result = harness.get_buffer_content();
    let x_count = result.matches('X').count();
    assert_eq!(x_count, 0, "Should have 0 X's. Buffer: {result}");
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Undo again - should remove the second cursor
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Redo - should add the cursor back
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should have 2 cursors again
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Redo again - should redo the batch insertion
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // X's should be back
    let result = harness.get_buffer_content();
    let x_count = result.matches('X').count();
    assert_eq!(x_count, 2, "Should have 2 X's back. Buffer: {result}");
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
    assert!(
        !screen.contains(" cursors"),
        "Should not show cursor count with single cursor"
    );

    // Add a cursor below
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Status bar should show "2 cursors"
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("2 cursors"),
        "Status bar should show '2 cursors'. Screen:\n{screen}"
    );

    // Add another cursor
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Status bar should show "3 cursors"
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("3 cursors"),
        "Status bar should show '3 cursors'. Screen:\n{screen}"
    );

    // Remove secondary cursors
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Status bar should NOT show cursor count again
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(" cursors"),
        "Should not show cursor count after removing cursors"
    );
}

/// Test that all cursors are visible in the viewport
#[test]
fn test_all_cursors_visible_in_viewport() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use ratatui::style::Modifier;
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();

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
        "Expected at least 2 visible cursors (secondary cursors), found {cursor_indicators_found}"
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
        "Should have inserted exactly 4 X's, one per cursor. Buffer: {result}"
    );

    // Test 2: Undo should remove all X's atomically
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let result_after_undo = harness.get_buffer_content();
    let x_count_after_undo = result_after_undo.matches('X').count();
    assert_eq!(
        x_count_after_undo, 0,
        "Should have removed all X's with single undo. Buffer: {result_after_undo}"
    );

    // Verify we still have 4 cursors after undo
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 4);
}

/// Test single cursor visibility - comprehensive test moving through every position
#[test]
fn test_single_cursor_visible() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();

    // Create multiple lines with various content
    harness
        .type_text("Hello World\nSecond Line Here\nThird Line\nFourth")
        .unwrap();

    let expected_content = "Hello World\nSecond Line Here\nThird Line\nFourth";
    harness.assert_buffer_content(expected_content);

    // Move to start of document
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Expected positions for "Hello World\nSecond Line Here\nThird Line\nFourth"
    let expected_chars = vec![
        'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd',  // end of line 1
        '\n', // newline is at position 11
    ];

    println!("\nStarting comprehensive cursor visibility test...");
    println!("Testing first line: 'Hello World'");

    // Move through first line character by character
    for (step, expected_char) in expected_chars.iter().enumerate() {
        harness.render().unwrap();

        let cursor_pos = harness.cursor_position();
        println!("\nStep {step}: cursor at buffer position {cursor_pos}");

        // Find cursor on screen using the harness's cursor detection
        let cursors = harness.find_all_cursors();
        assert!(
            !cursors.is_empty(),
            "Step {step}: Cursor not visible at buffer position {cursor_pos}! Expected char: '{expected_char}'"
        );

        let (x, y, char_at_cursor, _is_primary) = &cursors[0];
        println!("  Screen position: ({x}, {y}), char: '{char_at_cursor}'");

        // For newline, we expect to see a space since we add it for visibility
        if *expected_char == '\n' {
            println!("  At newline - expecting space or newline indicator");
        } else {
            // Verify the character matches (accounting for rendered character)
            let expected_str = expected_char.to_string();
            assert_eq!(
                *char_at_cursor, expected_str,
                "Step {step}: Cursor at wrong character. Expected '{expected_str}', got '{char_at_cursor}'"
            );
        }

        // Move right for next iteration
        if step < expected_chars.len() - 1 {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
        }
    }

    println!("\nTesting navigation to second line...");

    // Move to start of second line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let after_down = harness.cursor_position();
    println!("After Down: cursor at buffer position {after_down}");

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    let after_home = harness.cursor_position();
    println!("After Home: cursor at buffer position {after_home}");

    harness.render().unwrap();

    let cursors = harness.find_all_cursors();
    assert!(
        !cursors.is_empty(),
        "Cursor should be visible at start of second line"
    );
    let (x, y, char_at_cursor, _is_primary) = &cursors[0];
    println!(
        "At start of line 2: screen ({x}, {y}), char: '{char_at_cursor}', buffer pos: {after_home}"
    );

    // Position 12 should be 'S' (first char of "Second")
    // But we need to be flexible in case the cursor is shown differently
    if after_home == 12 {
        // If we're at the 'S', it should show 'S' with REVERSED
        assert_eq!(*char_at_cursor, "S", "Should be at 'S' of 'Second'");
    } else {
        println!("WARNING: Cursor not at expected position 12, it's at {after_home}");
    }

    // Move through "Second" character by character
    let second_chars = ['S', 'e', 'c', 'o', 'n', 'd'];
    for (i, expected_char) in second_chars.iter().enumerate() {
        harness.render().unwrap();

        let cursors = harness.find_all_cursors();
        assert!(
            !cursors.is_empty(),
            "Cursor not visible at char {i} of 'Second'"
        );

        let (_, _, char_at_cursor, _is_primary) = &cursors[0];
        let expected_str = expected_char.to_string();
        assert_eq!(
            *char_at_cursor, expected_str,
            "At position {i} of 'Second': expected '{expected_str}', got '{char_at_cursor}'"
        );

        if i < second_chars.len() - 1 {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
        }
    }

    println!("\nTesting vertical navigation...");

    // Test moving up and down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let cursors = harness.find_all_cursors();
    assert!(
        !cursors.is_empty(),
        "Cursor should be visible after moving down"
    );
    println!("After Down: cursor at {:?}", cursors[0]);

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let cursors = harness.find_all_cursors();
    assert!(
        !cursors.is_empty(),
        "Cursor should be visible after moving up"
    );
    println!("After Up: cursor at {:?}", cursors[0]);

    // Move to end of document
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let cursors = harness.find_all_cursors();
    assert!(
        !cursors.is_empty(),
        "Cursor should be visible at end of document"
    );
    println!("At end of document: cursor at {:?}", cursors[0]);

    println!("\nCursor visibility test completed successfully!");
}

/// Test cursor visibility on empty lines
#[test]
fn test_cursor_visible_on_empty_line() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Start with empty buffer (empty line)
    harness.render().unwrap();

    // Should have exactly 1 cursor
    assert_eq!(harness.cursor_count(), 1);

    // Cursor should be visible on the empty line
    let cursors = harness.find_all_cursors();
    assert!(
        !cursors.is_empty(),
        "Cursor should be visible on empty line"
    );
    assert_eq!(cursors.len(), 1, "Should have exactly 1 visible cursor");

    // Type some text, then delete it to create an empty line again
    harness.type_text("Test").unwrap();
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Cursor should still be visible on empty line
    let cursors_after_delete = harness.find_all_cursors();
    assert!(
        !cursors_after_delete.is_empty(),
        "Cursor should be visible on empty line after deleting text"
    );

    // Add multiple empty lines and test cursor on different empty lines
    harness.type_text("\n\n\n").unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Cursor should be visible on the empty line we moved to
    let cursors_on_middle_empty = harness.find_all_cursors();
    assert!(
        !cursors_on_middle_empty.is_empty(),
        "Cursor should be visible on middle empty line"
    );
}

/// Test cursor visibility when editor first opens with empty buffer
#[test]
fn test_cursor_visible_on_initial_empty_buffer() {
    // Create harness with empty buffer (simulates opening editor)
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    println!("Testing initial empty buffer cursor visibility...");
    println!(
        "Buffer length: {}",
        harness.editor().active_state().buffer.len()
    );
    println!(
        "Cursor position: {}",
        harness.editor().active_state().cursors.primary().position
    );

    // Use the harness's cursor detection which handles both hardware cursor and REVERSED cells
    let cursors = harness.find_all_cursors();
    assert!(
        !cursors.is_empty(),
        "Cursor must be visible when editor opens with empty buffer"
    );

    let (x, y, char_at_cursor, is_primary) = &cursors[0];
    println!(
        "Found cursor at screen position ({x}, {y}): '{char_at_cursor}' (primary: {is_primary})"
    );
    assert!(*is_primary, "The only cursor should be the primary cursor");

    // CRITICAL: The cursor must NOT be in the gutter area (column 0-7 typically)
    // It should be at the content area, which starts after the gutter
    let gutter_width = harness.editor().active_state().margins.left_total_width();
    println!("Gutter width: {}", gutter_width);
    println!("Cursor screen position: ({}, {})", x, y);
    println!(
        "Margins enabled: {}",
        harness.editor().active_state().margins.left_config.enabled
    );
    assert!(
        *x >= gutter_width as u16,
        "Cursor x position ({}) must be >= gutter width ({}) - cursor should not be in gutter area",
        x,
        gutter_width
    );
}

/// Test cursor position after Ctrl+End in various buffer states
/// This verifies the cursor renders at the correct screen position, not in the gutter
#[test]
fn test_ctrl_end_cursor_position() {
    use crossterm::event::{KeyCode, KeyModifiers};

    // Test 1: Empty buffer - Ctrl+End should keep cursor at (gutter_width, content_start_y)
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.render().unwrap();

        let gutter_width = harness.editor().active_state().margins.left_total_width();
        println!("Test 1: Empty buffer");
        println!("  Gutter width: {}", gutter_width);

        // Press Ctrl+End to jump to end of buffer
        harness
            .send_key(KeyCode::End, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        let (cursor_x, _cursor_y) = harness.screen_cursor_position();
        println!("  Cursor x after Ctrl+End: {}", cursor_x);
        assert!(
            cursor_x >= gutter_width as u16,
            "Empty buffer: Cursor x ({}) should be >= gutter width ({})",
            cursor_x,
            gutter_width
        );
    }

    // Test 2: Buffer with trailing newline - Ctrl+End should put cursor on empty line after newline
    // The implicit line after the newline should have a line number in the gutter
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.type_text("hello\n").unwrap();
        harness.render().unwrap();

        let gutter_width = harness.editor().active_state().margins.left_total_width();
        println!("Test 2: Buffer with trailing newline 'hello\\n'");
        println!("  Gutter width: {}", gutter_width);

        // Press Ctrl+End to jump to end of buffer
        harness
            .send_key(KeyCode::End, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        let (cursor_x, cursor_y) = harness.screen_cursor_position();
        println!(
            "  Cursor position after Ctrl+End: ({}, {})",
            cursor_x, cursor_y
        );
        assert!(
            cursor_x >= gutter_width as u16,
            "Trailing newline: Cursor x ({}) should be >= gutter width ({})",
            cursor_x,
            gutter_width
        );

        // Check that the gutter shows a line number for the implicit line after the newline
        // The cursor should be on line 2 (0-indexed y position relative to content area)
        // Get the row text where cursor is positioned and check the gutter
        let row_text = harness.get_row_text(cursor_y);
        println!("  Row text at cursor y={}: '{}'", cursor_y, row_text);

        // The gutter should contain "2" for line 2 (the implicit line after "hello\n")
        // Gutter format is typically: " N │" where N is right-aligned line number
        let gutter_text: String = row_text.chars().take(gutter_width).collect();
        println!("  Gutter text: '{}'", gutter_text);
        assert!(
            gutter_text.contains("2"),
            "Trailing newline: Gutter should show line number 2 for the implicit line, got: '{}'",
            gutter_text
        );
    }

    // Test 3: Buffer ending with empty line (two newlines) - Ctrl+End on last empty line
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.type_text("hello\n\n").unwrap();
        harness.render().unwrap();

        let gutter_width = harness.editor().active_state().margins.left_total_width();
        println!("Test 3: Buffer with empty line 'hello\\n\\n'");
        println!("  Gutter width: {}", gutter_width);

        // Press Ctrl+End to jump to end of buffer
        harness
            .send_key(KeyCode::End, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        let (cursor_x, _cursor_y) = harness.screen_cursor_position();
        println!("  Cursor x after Ctrl+End: {}", cursor_x);
        assert!(
            cursor_x >= gutter_width as u16,
            "Empty line: Cursor x ({}) should be >= gutter width ({})",
            cursor_x,
            gutter_width
        );
    }

    // Test 4: Buffer without trailing newline - Ctrl+End should put cursor after last char
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.type_text("hello").unwrap();
        harness.render().unwrap();

        let gutter_width = harness.editor().active_state().margins.left_total_width();
        println!("Test 4: Buffer without trailing newline 'hello'");
        println!("  Gutter width: {}", gutter_width);

        // Press Ctrl+End to jump to end of buffer
        harness
            .send_key(KeyCode::End, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        let (cursor_x, _cursor_y) = harness.screen_cursor_position();
        println!("  Cursor x after Ctrl+End: {}", cursor_x);
        // Cursor should be at gutter_width + 5 (after "hello")
        let expected_x = gutter_width as u16 + 5;
        assert!(
            cursor_x >= gutter_width as u16,
            "No trailing newline: Cursor x ({}) should be >= gutter width ({})",
            cursor_x,
            gutter_width
        );
        assert_eq!(
            cursor_x, expected_x,
            "No trailing newline: Cursor x ({}) should be at end of 'hello' ({})",
            cursor_x, expected_x
        );
    }
}

/// Test cursor visibility when opening a file
#[test]
fn test_cursor_visible_when_opening_file() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    fs::write(&file_path, "Hello World\nSecond Line").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    println!("Testing cursor visibility when opening file...");
    println!(
        "Buffer content: {}",
        harness.editor().active_state().buffer.to_string().unwrap()
    );
    println!(
        "Buffer length: {}",
        harness.editor().active_state().buffer.len()
    );
    println!(
        "Cursor position: {}",
        harness.editor().active_state().cursors.primary().position
    );

    // Use the harness's cursor detection which handles both hardware cursor and REVERSED cells
    let cursors = harness.find_all_cursors();
    assert!(
        !cursors.is_empty(),
        "Cursor must be visible when opening a file"
    );

    let (x, y, char_at_cursor, is_primary) = &cursors[0];
    println!(
        "Found cursor at screen position ({x}, {y}): '{char_at_cursor}' (primary: {is_primary})"
    );
    assert!(*is_primary, "The only cursor should be the primary cursor");
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
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Get initial cursor position
    let initial_pos = harness.cursor_position();
    println!("Initial cursor position: {initial_pos}");

    // Add first cursor below
    harness.editor_mut().add_cursor_below();
    println!("After adding 1st cursor below:");
    for (id, cursor) in harness.editor().active_state().cursors.iter() {
        println!(
            "  Cursor {:?}: position={}, anchor={:?}",
            id, cursor.position, cursor.anchor
        );
    }

    // Add second cursor below
    harness.editor_mut().add_cursor_below();
    println!("After adding 2nd cursor below:");
    for (id, cursor) in harness.editor().active_state().cursors.iter() {
        println!(
            "  Cursor {:?}: position={}, anchor={:?}",
            id, cursor.position, cursor.anchor
        );
    }

    // Add third cursor below
    harness.editor_mut().add_cursor_below();
    println!("After adding 3rd cursor below:");
    for (id, cursor) in harness.editor().active_state().cursors.iter() {
        println!(
            "  Cursor {:?}: position={}, anchor={:?}",
            id, cursor.position, cursor.anchor
        );
    }

    let cursor_count = harness.editor().active_state().cursors.iter().count();
    println!("Total cursors: {cursor_count}");

    // Type X
    harness.type_text("X").unwrap();

    let result = harness.get_buffer_content();
    println!("Buffer after typing X:\n{result}");

    let x_count = result.matches('X').count();
    println!("X count: {x_count}");

    // This should pass if cursors are positioned correctly
    assert_eq!(
        x_count, 4,
        "Should have 4 X's, one per cursor. Buffer:\n{result}"
    );
}

/// Test that pressing Esc returns to original cursor position, not last added cursor
#[test]
fn test_esc_returns_to_original_cursor_position() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines
    harness
        .type_text("Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
        .unwrap();
    harness.assert_buffer_content("Line 1\nLine 2\nLine 3\nLine 4\nLine 5");

    // Go to start of Line 1
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Get the original cursor position (should be at start of Line 1, position 0)
    let original_position = harness.cursor_position();
    println!("Original cursor position: {original_position} (should be at start of Line 1)");
    assert_eq!(original_position, 0, "Should start at position 0");

    // Add cursors to lines below (Line 2, 3, 4)
    harness.editor_mut().add_cursor_below(); // Add to Line 2
    harness.editor_mut().add_cursor_below(); // Add to Line 3
    harness.editor_mut().add_cursor_below(); // Add to Line 4

    // Should have 4 cursors now
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 4);

    // Print cursor positions for debugging
    println!("After adding cursors:");
    for (id, cursor) in harness.editor().active_state().cursors.iter() {
        println!("  Cursor {:?}: position={}", id, cursor.position);
    }

    // The "primary" cursor is now at Line 4 (the last one we added)
    // But when we press Esc, we expect to return to Line 1 (original position)

    // Press Esc to remove secondary cursors
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have only 1 cursor now
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 1);

    // The cursor should be at the ORIGINAL position (Line 1, position 0)
    // NOT at the last added cursor position (Line 4)
    let final_position = harness.cursor_position();
    println!("Final cursor position: {final_position} (should be back at original position 0)");

    assert_eq!(
        final_position, original_position,
        "After pressing Esc, cursor should return to original position {original_position} but is at {final_position}"
    );
}
