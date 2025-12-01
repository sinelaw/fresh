use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that cursor position stays in sync when editing lines with non-ASCII characters
/// This reproduces the bug where visual cursor position drifts from actual position
/// when a line contains Unicode box-drawing characters or other multi-byte UTF-8 characters
#[test]
fn test_cursor_sync_with_non_ascii_box_drawing_chars() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Type a line with box-drawing characters like in the bug report
    // Example: │  ┌──────────────┐  ┌──────────────┐  ┌─────────────┐  │
    let text_with_boxes = "   17 │ │  ┌──────────────┐  ┌──────────────┐  ┌─────────────┐  │";
    harness.type_text(text_with_boxes).unwrap();
    harness.render().unwrap();

    // Verify buffer content is correct
    harness.assert_buffer_content(text_with_boxes);

    // Get the buffer position (should be at end)
    let buffer_pos = harness.cursor_position();
    let expected_buffer_pos = text_with_boxes.len();
    assert_eq!(
        buffer_pos, expected_buffer_pos,
        "Cursor should be at end of text (byte position {}), but is at {}",
        expected_buffer_pos, buffer_pos
    );

    // Move cursor to the beginning of the line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Cursor should now be at position 0
    let buffer_pos_after_home = harness.cursor_position();
    assert_eq!(
        buffer_pos_after_home, 0,
        "Cursor should be at position 0 after Home"
    );

    // Now move cursor right character by character and verify screen position matches
    // The key insight: when moving through multi-byte UTF-8 characters,
    // the buffer position advances by the number of bytes in the character,
    // but the screen column should advance by 1

    // First, let's move right 10 times (through "   17 │ │ ")
    for i in 1..=10 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();

        let buffer_pos = harness.cursor_position();
        let (screen_x, _screen_y) = harness.screen_cursor_position();

        // The screen cursor position depends on gutter width
        // For this test, we're mainly checking that the screen cursor advances properly
        // The gutter width varies based on line numbers, so we'll focus on relative movement

        println!(
            "After {} right arrows: buffer_pos={}, screen_x={}",
            i, buffer_pos, screen_x
        );
    }

    // Now test: type a character and verify it appears at the visual cursor position
    // Move to somewhere in the middle of the line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Move right 20 characters
    for _ in 0..20 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    let buffer_pos_before_insert = harness.cursor_position();
    let (screen_x_before, screen_y_before) = harness.screen_cursor_position();

    println!(
        "Before insert: buffer_pos={}, screen=({}, {})",
        buffer_pos_before_insert, screen_x_before, screen_y_before
    );

    // Insert a marker character 'X' at this position
    harness.type_text("X").unwrap();

    // Verify that 'X' appears at the expected position in the buffer
    let buffer_content_after = harness.get_buffer_content().unwrap();
    println!("Buffer after insert: {:?}", buffer_content_after);

    // The 'X' should be inserted at buffer_pos_before_insert
    // and should appear visually at screen_x_before

    // Get the screen position where 'X' appears
    harness.render().unwrap();

    // This is where the bug manifests: if cursor tracking is broken,
    // the 'X' will not appear at screen_x_before
}

/// Test cursor movement with simple multi-byte UTF-8 characters (emojis)
#[test]
fn test_cursor_sync_with_emoji() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a line with emojis
    let text = "Hello 😀 World 🌍";
    harness.type_text(text).unwrap();

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // The text has these characters:
    // H e l l o   😀   W o r l d   🌍
    // 0 1 2 3 4 5 [6-9] 10 11 12 13 14 15 [16-19]
    // Note: 😀 is 4 bytes (U+1F600), 🌍 is 4 bytes (U+1F30D)

    // Move right 7 times should position us after the emoji
    for _ in 0..7 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    let buffer_pos = harness.cursor_position();
    // "Hello " = 6 bytes, "😀" = 4 bytes, so position should be 10
    assert_eq!(
        buffer_pos, 10,
        "After moving through 'Hello 😀', cursor should be at byte 10"
    );

    // Type 'X' and verify it's inserted correctly
    harness.type_text("X").unwrap();
    let expected = "Hello 😀X World 🌍";
    harness.assert_buffer_content(expected);
}

/// Test that cursor position is correct when clicking on text with non-ASCII characters
#[test]
fn test_mouse_click_on_non_ascii_text() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Type a line with box-drawing characters
    let text = "│  ┌──────────────┐  ┌──────────────┐  │";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    // Now click on various positions in the line and verify cursor position

    // Get the gutter width first by checking where line 1 starts
    // The tab bar is at row 0, first line of text is at row 1
    let line_row = 1;

    // Click at the beginning of the text (after gutter)
    // We need to figure out where the gutter ends
    // Let's assume standard gutter of 8 chars for now: " " + "   1" + " │ "

    // This test may need adjustment based on actual gutter rendering
}
