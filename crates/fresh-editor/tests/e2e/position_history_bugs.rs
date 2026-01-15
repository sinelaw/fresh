use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that the actual buffer CONTENT (not just the tab) updates when navigating back/forward
#[test]
fn test_back_forward_updates_visible_content() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Buffer 1: Type unique content
    harness.type_text("FIRST_BUFFER_CONTENT").unwrap();

    // Create Buffer 2 with different content
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("SECOND_BUFFER_CONTENT").unwrap();

    // Create Buffer 3 with different content
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("THIRD_BUFFER_CONTENT").unwrap();

    // Now we're in buffer 3, verify the visible content
    harness.assert_buffer_content("THIRD_BUFFER_CONTENT");

    // Navigate back to buffer 2
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.render().unwrap(); // Force render

    // Check that the VISIBLE CONTENT actually changed, not just the tab
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("SECOND_BUFFER_CONTENT"),
        "Screen should show SECOND_BUFFER_CONTENT after navigating back, but shows: {screen}"
    );
    assert!(
        !screen.contains("THIRD_BUFFER_CONTENT"),
        "Screen should NOT show THIRD_BUFFER_CONTENT after navigating back"
    );

    // Navigate back again to buffer 1
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("FIRST_BUFFER_CONTENT"),
        "Screen should show FIRST_BUFFER_CONTENT after navigating back twice, but shows: {screen}"
    );
    assert!(
        !screen.contains("SECOND_BUFFER_CONTENT"),
        "Screen should NOT show SECOND_BUFFER_CONTENT"
    );

    // Navigate forward to buffer 2
    harness.send_key(KeyCode::Right, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("SECOND_BUFFER_CONTENT"),
        "Screen should show SECOND_BUFFER_CONTENT after navigating forward, but shows: {screen}"
    );
    assert!(
        !screen.contains("FIRST_BUFFER_CONTENT"),
        "Screen should NOT show FIRST_BUFFER_CONTENT"
    );
}

/// Test that cursor position is saved to history when moving around with arrow keys
#[test]
fn test_cursor_movement_tracked_in_history() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create content with multiple lines
    harness
        .type_text("Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
        .unwrap();

    // Move cursor to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(harness.cursor_position(), 0);

    // Move to end (this is a "large" movement that should be tracked)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    let end_pos = harness.cursor_position();
    assert!(end_pos > 0);

    // Create a new buffer (this will save position)
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Other buffer").unwrap();

    // Navigate back - cursor should return to the end position in buffer 1
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();

    // Verify we're in the first buffer AND cursor is at the saved position
    harness.assert_buffer_content("Line 1\nLine 2\nLine 3\nLine 4\nLine 5");
    assert_eq!(
        harness.cursor_position(),
        end_pos,
        "Cursor should be at position {} after navigating back, but is at {}",
        end_pos,
        harness.cursor_position()
    );
}

/// Test that moving cursor with arrow keys and then using back/forward actually moves the cursor
#[test]
fn test_small_cursor_movements_with_back_forward() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a line of text
    harness.type_text("0123456789").unwrap();
    assert_eq!(harness.cursor_position(), 10); // At end after typing

    // Move cursor to position 5 using left arrow
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Left, KeyModifiers::empty())
            .unwrap();
    }
    assert_eq!(harness.cursor_position(), 5);

    // Create new buffer (saves position at 5)
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Different content").unwrap();

    // Navigate back
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();

    // Should be back at position 5 in first buffer
    harness.assert_buffer_content("0123456789");
    assert_eq!(
        harness.cursor_position(),
        5,
        "After navigating back, cursor should be at position 5 where we left it"
    );
}

/// Test PageUp/PageDown movements are tracked
#[test]
fn test_page_movements_tracked_in_history() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create many lines of content (more than one screen)
    for i in 0..50 {
        harness.type_text(&format!("Line {i}\n")).unwrap();
    }

    // Cursor is at end, move to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(harness.cursor_position(), 0);

    // Page down a few times
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::empty())
        .unwrap();
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::empty())
        .unwrap();
    let middle_pos = harness.cursor_position();
    assert!(middle_pos > 0);

    // Create new buffer (saves position)
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Other").unwrap();

    // Navigate back
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();

    // Should be at the middle position where we paged down to
    assert_eq!(
        harness.cursor_position(),
        middle_pos,
        "After navigating back, cursor should be at position {middle_pos} where we paged down to"
    );
}

/// Test that cursor movements WITHIN A SINGLE BUFFER are tracked in history
/// This is the key feature of VS Code's position history - it tracks where you've been
/// even without switching files
#[test]
fn test_cursor_movements_within_single_buffer_tracked() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a multi-line document
    harness
        .type_text(
            "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10",
        )
        .unwrap();
    let end_pos = harness.cursor_position();

    // Position 1: Go to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    let pos1 = harness.cursor_position();
    assert_eq!(pos1, 0, "Should be at beginning");

    // Position 2: Jump to end (this is a "large" movement that should be tracked)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    let pos2 = harness.cursor_position();
    assert_eq!(pos2, end_pos, "Should be at end");

    // Position 3: Jump back to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    let pos3 = harness.cursor_position();
    assert_eq!(pos3, 0, "Should be at beginning again");

    // Now navigate back through the history
    // We should go: current(beginning) -> back to end -> back to beginning (before the jump to end)

    // First back: should go to position 2 (end)
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        pos2,
        "After first back, should be at end position (pos2={}), but at {}",
        pos2,
        harness.cursor_position()
    );

    // Second back: should go to position 1 (beginning before jump to end)
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        pos1,
        "After second back, should be at beginning position (pos1={}), but at {}",
        pos1,
        harness.cursor_position()
    );

    // Navigate forward: should go back to end
    harness.send_key(KeyCode::Right, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        pos2,
        "After forward, should be back at end position (pos2={}), but at {}",
        pos2,
        harness.cursor_position()
    );
}

/// Test that PageDown jumps within a buffer are tracked
#[test]
fn test_large_jumps_within_buffer_tracked() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create many lines of content
    for i in 0..100 {
        harness.type_text(&format!("Line {i}\n")).unwrap();
    }

    // Position 1: Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    let start_pos = harness.cursor_position();
    assert_eq!(start_pos, 0);

    // Position 2: PageDown once (significant movement)
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::empty())
        .unwrap();
    let page1_pos = harness.cursor_position();
    assert!(page1_pos > start_pos, "Should have moved down");

    // Position 3: PageDown again (another significant movement)
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::empty())
        .unwrap();
    let page2_pos = harness.cursor_position();
    assert!(page2_pos > page1_pos, "Should have moved further down");

    // Now navigate back through history
    // Should go: current(page2) -> page1 -> start

    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        page1_pos,
        "After back, should be at page1 position ({}), but at {}",
        page1_pos,
        harness.cursor_position()
    );

    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        start_pos,
        "After second back, should be at start position ({}), but at {}",
        start_pos,
        harness.cursor_position()
    );

    // Navigate forward
    harness.send_key(KeyCode::Right, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        page1_pos,
        "After forward, should be back at page1 position ({}), but at {}",
        page1_pos,
        harness.cursor_position()
    );
}
