// Split View Behavioral Expectations Validation Tests
// These tests verify the expected behaviors documented in docs/TODO.md
// Key insight: Splits share the SAME buffer by default (Emacs-style)
// Each split has independent cursor and scroll positions

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Helper: Create a horizontal split via command palette
fn split_horizontal(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split horiz").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper: Create a vertical split via command palette
fn split_vertical(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper: Navigate to previous split via command palette
fn prev_split(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper: Close the active split via command palette
fn close_split(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("close split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test that horizontal split creates two views of the SAME buffer
#[test]
fn test_horizontal_split_shares_same_buffer() {
    let mut harness = EditorTestHarness::new(80, 40).unwrap();

    // Type text in first buffer
    harness.type_text("Original content").unwrap();
    harness.render().unwrap();

    let original_content = harness.get_buffer_content();
    assert_eq!(original_content, "Original content");

    // Split horizontally
    split_horizontal(&mut harness);

    // New split should show the SAME buffer content
    let new_split_content = harness.get_buffer_content();
    assert_eq!(
        new_split_content, "Original content",
        "New split should show the same buffer, got '{}'",
        new_split_content
    );

    // Screen should show "Split pane horizontally" message
    harness.assert_screen_contains("Split pane horizontally");

    // Verify separator line exists (horizontal split uses ─)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains('─'),
        "Horizontal split should show ─ separator"
    );
}

/// Test that vertical split creates two views of the SAME buffer
#[test]
fn test_vertical_split_shares_same_buffer() {
    let mut harness = EditorTestHarness::new(80, 40).unwrap();

    // Type text in first buffer
    harness.type_text("Buffer content").unwrap();
    harness.render().unwrap();

    // Split vertically (Alt+V)
    split_vertical(&mut harness);

    // New split should show the SAME buffer content
    let new_split_content = harness.get_buffer_content();
    assert_eq!(
        new_split_content, "Buffer content",
        "New split should show the same buffer"
    );

    // Verify separator line exists (vertical split uses │)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains('│'),
        "Vertical split should show │ separator"
    );
}

/// Test that typing in one split modifies the shared buffer
/// and changes are visible when switching to other split
#[test]
fn test_typing_modifies_shared_buffer() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type in first buffer
    harness.type_text("Hello").unwrap();

    // Create vertical split (both splits now show same buffer)
    split_vertical(&mut harness);

    // Verify new split shows same content
    assert_eq!(harness.get_buffer_content(), "Hello");

    // New split has independent cursor at position 0
    // Move cursor to end before typing
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Type more text in the second split (modifies shared buffer)
    harness.type_text(" World").unwrap();
    harness.render().unwrap();

    // Current buffer should now be "Hello World"
    assert_eq!(harness.get_buffer_content(), "Hello World");

    // Navigate back to first split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // First split should ALSO show "Hello World" (same buffer)
    assert_eq!(harness.get_buffer_content(), "Hello World");
}

/// Test that each split has independent cursor position for the same buffer
#[test]
fn test_independent_cursor_positions_same_buffer() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type text and position cursor at beginning
    harness.type_text("ABCDEFGHIJ").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    // Cursor in first split is at position 0

    // Create vertical split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // New split should show same buffer
    assert_eq!(harness.get_buffer_content(), "ABCDEFGHIJ");

    // Move cursor in second split to end
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    let cursor_in_second_split = harness.cursor_position();
    assert_eq!(cursor_in_second_split, 10); // End of "ABCDEFGHIJ"

    // Navigate back to first split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // First split cursor should still be at position 0 (independent)
    let cursor_in_first_split = harness.cursor_position();
    assert_eq!(
        cursor_in_first_split, 0,
        "First split cursor should remain at 0, not {}",
        cursor_in_first_split
    );
}

/// Test that each split has independent scroll position for the same buffer
#[test]
fn test_independent_scroll_positions_same_buffer() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Create long content (more than viewport height)
    let long_text = (1..=50)
        .map(|i| format!("Line {}", i))
        .collect::<Vec<_>>()
        .join("\n");
    harness.type_text(&long_text).unwrap();
    harness.render().unwrap();

    // Scroll to top in first split
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let top_byte_first_split = harness.top_byte();

    // Create vertical split (both show same buffer)
    split_vertical(&mut harness);

    // Scroll down in second split - use Ctrl+End to go to end of buffer
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let top_byte_second_split = harness.top_byte();

    // Second split should have scrolled
    assert_ne!(
        top_byte_second_split, top_byte_first_split,
        "Second split should have different scroll position"
    );

    // Navigate back to first split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // First split should still be at original scroll position
    assert_eq!(
        harness.top_byte(),
        top_byte_first_split,
        "First split scroll position should be preserved"
    );
}

/// Test next_split and prev_split circular navigation
#[test]
fn test_split_navigation_circular() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Create content
    harness.type_text("Shared buffer").unwrap();

    // Create second split
    split_vertical(&mut harness);

    // Create third split
    split_vertical(&mut harness);

    // Move cursor to unique position in third split
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    let cursor_third = harness.cursor_position();

    // Navigate to next split (should wrap to first)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // All splits show same buffer, but cursor positions differ
    assert_eq!(harness.get_buffer_content(), "Shared buffer");

    // Keep navigating to verify circular behavior
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // After 3 next operations, should be back to third split with cursor at end
    assert_eq!(harness.cursor_position(), cursor_third);
}

/// Test that closing a split expands the remaining split
#[test]
fn test_close_split_expands_remaining() {
    let mut harness = EditorTestHarness::new(80, 40).unwrap();

    // Create content
    harness.type_text("Buffer content").unwrap();

    // Create split
    split_vertical(&mut harness);

    // Verify separator exists
    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains('│'),
        "Should have vertical separator"
    );

    // Close current split via command palette
    close_split(&mut harness);

    // Should see success message
    harness.assert_screen_contains("Closed split");

    // Buffer content should still be accessible
    assert_eq!(harness.get_buffer_content(), "Buffer content");
}

/// Test that each split can open different files
#[test]
fn test_split_with_different_files() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");

    std::fs::write(&file1, "Content of file 1").unwrap();
    std::fs::write(&file2, "Content of file 2").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    assert_eq!(harness.get_buffer_content(), "Content of file 1");

    // Create vertical split (initially shows same buffer)
    split_vertical(&mut harness);
    assert_eq!(harness.get_buffer_content(), "Content of file 1");

    // Open different file in new split
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();
    assert_eq!(harness.get_buffer_content(), "Content of file 2");

    // Verify both file names appear in the UI
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");

    // Navigate back to first split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // First split should still show file1 content
    assert_eq!(harness.get_buffer_content(), "Content of file 1");
}

/// Test nested splits (3+ levels deep)
#[test]
fn test_nested_splits_maintain_hierarchy() {
    let mut harness = EditorTestHarness::new(160, 50).unwrap();

    // Create content
    harness.type_text("Base content").unwrap();

    // Vertical split
    split_vertical(&mut harness);

    // Horizontal split on second split
    split_horizontal(&mut harness);

    // Another vertical split
    split_vertical(&mut harness);

    // We should now have 4 splits, all showing same buffer
    assert_eq!(harness.get_buffer_content(), "Base content");

    // Navigate through all splits to verify they exist
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        harness.type_text("next split").unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        // All splits show same buffer
        assert_eq!(harness.get_buffer_content(), "Base content");
    }
}

/// Test that undo/redo affects the shared buffer (visible in all splits)
#[test]
fn test_undo_redo_affects_shared_buffer() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type text
    harness.type_text("Hello").unwrap();

    // Create split
    split_vertical(&mut harness);

    // Type more in second split (cursor starts at 0, move to end first)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" World").unwrap();
    harness.render().unwrap();
    assert_eq!(harness.get_buffer_content(), "Hello World");

    // Undo in second split (affects shared buffer)
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let after_undo = harness.get_buffer_content();

    // Navigate back to first split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // First split should also show the undone state (same buffer)
    assert_eq!(
        harness.get_buffer_content(),
        after_undo,
        "Both splits should show same buffer state after undo"
    );
}

/// Test status bar shows active split's buffer info
#[test]
fn test_status_bar_reflects_active_split() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("alpha.txt");
    let file2 = temp_dir.path().join("beta.txt");

    std::fs::write(&file1, "Alpha content").unwrap();
    std::fs::write(&file2, "Beta content").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();

    // Status bar should show alpha.txt
    harness.assert_screen_contains("alpha.txt");

    // Create split and open different file
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Status bar should now show beta.txt (active split)
    harness.assert_screen_contains("beta.txt");
}

/// Test that deleting text in one split is visible in other splits showing same buffer
#[test]
fn test_delete_visible_in_all_splits() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type text
    harness.type_text("ABCDE").unwrap();

    // Create split
    split_vertical(&mut harness);

    // Delete in second split (modifies shared buffer)
    // Move cursor to end first (cursor starts at 0 in new split)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(harness.get_buffer_content(), "ABC");

    // Navigate back to first split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // First split should also show "ABC" (same buffer)
    assert_eq!(harness.get_buffer_content(), "ABC");
}

/// Test that cursor movements don't affect other splits
#[test]
fn test_cursor_movement_isolated_to_active_split() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type multi-line content
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Position cursor at start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    let first_cursor_before = harness.cursor_position();
    assert_eq!(first_cursor_before, 0);

    // Create split (shares same buffer)
    split_vertical(&mut harness);

    // Move cursor around in second split
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    let second_cursor = harness.cursor_position();

    // Second split cursor should have moved
    assert_ne!(second_cursor, first_cursor_before);

    // Navigate back to first split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // First split cursor should be exactly where we left it
    assert_eq!(harness.cursor_position(), first_cursor_before);
}

/// Test creating a split with minimal height (edge case)
#[test]
fn test_split_with_minimal_height() {
    // Use a small terminal height
    let mut harness = EditorTestHarness::new(80, 12).unwrap();

    harness.type_text("Small terminal").unwrap();

    // Create horizontal split (splits the limited vertical space)
    split_horizontal(&mut harness);

    // Should succeed without panic
    harness.assert_screen_contains("Split pane horizontally");

    // Should be able to type in split (modifying shared buffer)
    harness.type_text(" - modified").unwrap();
    harness.render().unwrap();
    assert!(harness.get_buffer_content().contains("modified"));
}

/// Test that copy/paste works across splits (shared clipboard)
#[test]
fn test_clipboard_shared_across_splits() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type and select text in first buffer
    harness.type_text("CopyThis").unwrap();
    // Select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    // Copy
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Create split (shows same buffer)
    split_vertical(&mut harness);

    // Go to end and paste
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should have "CopyThis" + "CopyThis" = "CopyThisCopyThis"
    assert_eq!(harness.get_buffer_content(), "CopyThisCopyThis");
}

/// Test that cursor positions in other splits adjust when buffer is edited
/// This is an advanced feature that requires tracking cursor positions across splits
/// and adjusting them when the shared buffer is modified.
#[test]
fn test_cursor_adjustment_on_shared_buffer_edit() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type text
    harness.type_text("ABCDEFGHIJ").unwrap();

    // Create split
    split_vertical(&mut harness);

    // In second split, move cursor to position 5 (after "ABCDE")
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    assert_eq!(harness.cursor_position(), 5);

    // Navigate to first split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // In first split, insert text at beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("XXX").unwrap(); // Insert 3 chars
    harness.render().unwrap();

    // Buffer is now "XXXABCDEFGHIJ"
    assert_eq!(harness.get_buffer_content(), "XXXABCDEFGHIJ");

    // Navigate back to second split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Second split cursor should have adjusted from 5 to 8 (5 + 3 inserted chars)
    // This is a critical feature for shared buffer editing
    let adjusted_cursor = harness.cursor_position();
    assert_eq!(
        adjusted_cursor, 8,
        "Cursor in second split should adjust for insertion, got {}",
        adjusted_cursor
    );
}

/// Test that cursors in inactive splits are rendered (visible on screen)
/// Each split should show its cursor position even when not focused
#[test]
fn test_cursors_visible_in_all_splits() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Type some text
    harness.type_text("ABCDEFGHIJ").unwrap();
    harness.render().unwrap();

    // Create vertical split
    split_vertical(&mut harness);

    // Move cursor to different position in second split
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Second split cursor at position 5 (after "ABCDE")
    let second_split_cursor_pos = harness.cursor_position();
    assert_eq!(second_split_cursor_pos, 5);

    // Find all rendered cursors on screen
    let cursors = harness.find_all_cursors();

    // Should have at least 2 cursors visible (one for each split)
    // The active split has the hardware cursor, inactive split should have REVERSED cursor
    assert!(
        cursors.len() >= 2,
        "Should render cursors for both splits, found {} cursors: {:?}",
        cursors.len(),
        cursors
    );

    // Check that we have both a primary (hardware) cursor and a secondary (REVERSED) cursor
    let primary_cursors: Vec<_> = cursors
        .iter()
        .filter(|(_, _, _, is_primary)| *is_primary)
        .collect();
    let secondary_cursors: Vec<_> = cursors
        .iter()
        .filter(|(_, _, _, is_primary)| !*is_primary)
        .collect();

    assert_eq!(
        primary_cursors.len(),
        1,
        "Should have exactly one primary (hardware) cursor"
    );
    assert!(
        !secondary_cursors.is_empty(),
        "Should have at least one secondary cursor for inactive split"
    );
}

/// Test that cursor movement works after switching splits
#[test]
fn test_cursor_movement_after_split_switch() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Type some text
    harness.type_text("ABCDEFGHIJ").unwrap();
    harness.render().unwrap();

    // Cursor should be at end (position 10)
    let initial_pos = harness.cursor_position();
    assert_eq!(initial_pos, 10);

    // Create vertical split
    split_vertical(&mut harness);

    // New split should have cursor at position 0
    let second_split_pos = harness.cursor_position();
    assert_eq!(second_split_pos, 0);

    // Move cursor right in second split
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let after_right = harness.cursor_position();
    assert_eq!(after_right, 1);

    // Switch back to first split
    prev_split(&mut harness);

    // First split should have cursor at position 10 (where we left it)
    let first_split_pos = harness.cursor_position();
    assert_eq!(first_split_pos, 10);

    // Move cursor left in first split
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let after_left = harness.cursor_position();
    assert_eq!(after_left, 9, "Cursor should move left from 10 to 9");

    // Get screen position to verify visual cursor moved
    let (screen_x1, _screen_y1) = harness.screen_cursor_position();

    // Move right to go back
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let after_right2 = harness.cursor_position();
    assert_eq!(after_right2, 10);

    let (screen_x2, _screen_y2) = harness.screen_cursor_position();

    // Verify screen position changed
    assert_ne!(screen_x1, screen_x2, "Screen cursor X should have moved");
}
