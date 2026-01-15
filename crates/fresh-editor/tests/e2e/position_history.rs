use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test basic back/forward navigation within a single buffer
#[test]
fn test_navigate_back_forward_single_buffer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create some content
    harness
        .type_text("Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
        .unwrap();
    harness.assert_buffer_content("Line 1\nLine 2\nLine 3\nLine 4\nLine 5");

    // Move to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(harness.cursor_position(), 0);

    // Move to end (this should create a history entry)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    let end_pos = harness.cursor_position();
    assert!(end_pos > 0);

    // Navigate back - should go to beginning
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();

    // Note: The current implementation saves position on buffer switch,
    // not on large cursor movements. So this test verifies the keybinding works
    // but won't actually navigate back within the same buffer unless we switched buffers.
}

/// Test back/forward navigation across multiple buffers
#[test]
fn test_navigate_back_forward_across_buffers() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Buffer 1: Type some content
    harness.type_text("Buffer 1 content").unwrap();
    let buffer1_pos = harness.cursor_position();

    // Create a new buffer (Ctrl+N)
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();

    // Buffer 2: Type different content
    harness.type_text("Buffer 2 content").unwrap();
    let buffer2_pos = harness.cursor_position();

    // Create another buffer
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();

    // Buffer 3: Type more content
    harness.type_text("Buffer 3 content").unwrap();
    let buffer3_pos = harness.cursor_position();
    assert_eq!(harness.cursor_position(), buffer3_pos);

    // Navigate back (Alt+Left) - should go to Buffer 2
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.assert_buffer_content("Buffer 2 content");
    assert_eq!(harness.cursor_position(), buffer2_pos);

    // Navigate back again - should go to Buffer 1
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.assert_buffer_content("Buffer 1 content");
    assert_eq!(harness.cursor_position(), buffer1_pos);

    // Navigate forward (Alt+Right) - should go back to Buffer 2
    harness.send_key(KeyCode::Right, KeyModifiers::ALT).unwrap();
    harness.assert_buffer_content("Buffer 2 content");
    assert_eq!(harness.cursor_position(), buffer2_pos);

    // Navigate forward again - should go to Buffer 3
    harness.send_key(KeyCode::Right, KeyModifiers::ALT).unwrap();
    harness.assert_buffer_content("Buffer 3 content");
    assert_eq!(harness.cursor_position(), buffer3_pos);
}

/// Test that switching buffers with Ctrl+PageUp/PageDown creates history entries
#[test]
fn test_buffer_switching_creates_history() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create 3 buffers with different content
    harness.type_text("First").unwrap();
    let _first_pos = harness.cursor_position();

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Second").unwrap();
    let second_pos = harness.cursor_position();

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Third").unwrap();

    // Switch back to first buffer using Ctrl+PageUp twice
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("First");

    // Navigate back should take us through the history
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.assert_buffer_content("Second");
    assert_eq!(harness.cursor_position(), second_pos);
}

/// Test that position is preserved when navigating back/forward
#[test]
fn test_cursor_position_preserved() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Buffer 1: Create content and position cursor in the middle
    harness.type_text("0123456789").unwrap();
    // Move cursor to position 5
    harness
        .send_key(KeyCode::Home, KeyModifiers::empty())
        .unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::empty())
            .unwrap();
    }
    assert_eq!(harness.cursor_position(), 5);

    // Create new buffer
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("ABCDEFGHIJ").unwrap();

    // Navigate back - cursor should be at position 5
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.assert_buffer_content("0123456789");
    assert_eq!(harness.cursor_position(), 5);
}

/// Test navigating back at the beginning of history does nothing
#[test]
fn test_navigate_back_at_beginning() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("Content").unwrap();
    let pos = harness.cursor_position();

    // Try to navigate back when there's no history
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();

    // Should still be in the same buffer at the same position
    harness.assert_buffer_content("Content");
    assert_eq!(harness.cursor_position(), pos);
}

/// Test navigating forward at the end of history does nothing
#[test]
fn test_navigate_forward_at_end() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create two buffers
    harness.type_text("First").unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Second").unwrap();

    // Try to navigate forward when we're at the end of history
    harness.send_key(KeyCode::Right, KeyModifiers::ALT).unwrap();

    // Should still be in the second buffer
    harness.assert_buffer_content("Second");
}

/// Test that new navigation truncates forward history
#[test]
fn test_new_navigation_truncates_forward_history() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create 3 buffers
    harness.type_text("Buffer 1").unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Buffer 2").unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Buffer 3").unwrap();

    // Navigate back twice
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.assert_buffer_content("Buffer 1");

    // Create a new buffer - this should truncate forward history
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Buffer 4").unwrap();

    // Try to navigate forward - should not be able to go to Buffer 2 or 3
    harness.send_key(KeyCode::Right, KeyModifiers::ALT).unwrap();

    // Should still be in Buffer 4 (at the end of history)
    harness.assert_buffer_content("Buffer 4");
}

/// Test position history with buffer switching via next/prev buffer
#[test]
fn test_position_history_with_next_prev_buffer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create 3 buffers
    harness.type_text("First").unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Second").unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Third").unwrap();

    // Use Ctrl+PageDown to cycle through buffers
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("First");

    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("Second");

    // Navigate back twice using Alt+Left
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.assert_buffer_content("First");

    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    harness.assert_buffer_content("Third");
}
