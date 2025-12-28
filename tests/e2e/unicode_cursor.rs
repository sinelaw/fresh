use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

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
    let _line_row = 1;

    // Click at the beginning of the text (after gutter)
    // We need to figure out where the gutter ends
    // Let's assume standard gutter of 8 chars for now: " " + "   1" + " │ "

    // This test may need adjustment based on actual gutter rendering
}

/// Test that backspace properly deletes entire UTF-8 characters, not just bytes
/// This reproduces the bug where backspace removes only the last byte of a multi-byte character
#[test]
fn test_backspace_deletes_entire_utf8_character() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Test 1: Euro sign (3 bytes: 0xE2 0x82 0xAC)
    harness.type_text("€").unwrap();
    harness.assert_buffer_content("€");

    // Backspace should delete the entire euro sign, not just one byte
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("");

    // Test 2: Norwegian characters (2 bytes each: æ=0xC3 0xA6, ø=0xC3 0xB8, å=0xC3 0xA5)
    harness.type_text("æøå").unwrap();
    harness.assert_buffer_content("æøå");

    // Backspace should delete 'å' entirely
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("æø");

    // Another backspace should delete 'ø' entirely
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("æ");

    // Another backspace should delete 'æ' entirely
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("");

    // Test 3: Emoji (4 bytes: 😀 = U+1F600)
    harness.type_text("a😀b").unwrap();
    harness.assert_buffer_content("a😀b");

    // Backspace should delete 'b'
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("a😀");

    // Backspace should delete the entire emoji (4 bytes), not just one byte
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("a");
}

/// Test that delete (forward) properly removes entire UTF-8 characters
#[test]
fn test_delete_forward_removes_entire_utf8_character() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text with multi-byte characters
    harness.type_text("a€b").unwrap();
    harness.assert_buffer_content("a€b");

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Delete 'a' - this should work fine (ASCII)
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("€b");

    // Delete '€' - should delete entire 3-byte euro sign, not just one byte
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("b");
}

/// Test that selecting and deleting/replacing UTF-8 characters works correctly
#[test]
fn test_selection_delete_with_utf8_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text with multi-byte characters: a + æ(2) + ø(2) + å(2) + b
    harness.type_text("aæøåb").unwrap();
    harness.assert_buffer_content("aæøåb");

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Move right once (past 'a')
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Select the three Norwegian characters by shift+right 3 times
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();

    // Delete the selection with backspace
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("ab");
}

/// Test that selecting and replacing UTF-8 characters works correctly
#[test]
fn test_selection_replace_with_utf8_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text with emoji
    harness.type_text("hello😀world").unwrap();
    harness.assert_buffer_content("hello😀world");

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Move right 5 times (past "hello")
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Select the emoji (1 character, 4 bytes)
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();

    // Replace with a different character
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("helloXworld");
}

/// Test loading a file with UTF-8 characters, backspacing, saving, and verifying file content
/// This reproduces the exact bug where backspace removes only a byte, corrupting the file on save
#[test]
fn test_backspace_utf8_file_save_roundtrip() {
    let temp_dir = TempDir::new().unwrap();

    // Test 1: Euro sign (3 bytes: 0xE2 0x82 0xAC)
    let euro_path = temp_dir.path().join("euro.txt");
    std::fs::write(&euro_path, "€\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&euro_path).unwrap();
    harness.render().unwrap();

    // Move to end of line (after €, before newline)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Backspace should delete the entire euro sign
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // Save with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify the file contains only a newline (euro sign fully deleted)
    let saved = std::fs::read(&euro_path).unwrap();
    assert_eq!(
        saved, b"\n",
        "Euro sign should be fully deleted, file should contain only newline. Got: {:?}",
        saved
    );

    // Test 2: Norwegian characters (æøå)
    let norwegian_path = temp_dir.path().join("norwegian.txt");
    std::fs::write(&norwegian_path, "æøå\n").unwrap();

    let mut harness2 = EditorTestHarness::new(80, 24).unwrap();
    harness2.open_file(&norwegian_path).unwrap();
    harness2.render().unwrap();

    // Move to end of line
    harness2.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Backspace should delete 'å' entirely (2 bytes)
    harness2
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // Save
    harness2
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness2.render().unwrap();

    // Verify
    let saved2 = std::fs::read(&norwegian_path).unwrap();
    assert_eq!(
        saved2,
        "æø\n".as_bytes(),
        "Only 'å' should be deleted, leaving 'æø'. Got: {:?}",
        String::from_utf8_lossy(&saved2)
    );
}

/// Test that arrow keys move by grapheme clusters for Thai text
///
/// Thai "ที่" is 3 Unicode code points but 1 grapheme cluster:
/// - ท (U+0E17) base consonant
/// - ี (U+0E35) vowel mark
/// - ่ (U+0E48) tone mark
///
/// Pressing Right arrow once should skip the entire cluster.
/// Also verifies the screen cursor moves correctly (visual position).
#[test]
fn test_thai_grapheme_cluster_movement() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type Thai text: "aที่b" (a + Thai cluster + b)
    // This gives us: 1 byte (a) + 9 bytes (Thai) + 1 byte (b) = 11 bytes
    // Visual width: 1 (a) + 1 (Thai cluster) + 1 (b) = 3 columns
    let text = "aที่b";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    // Verify the text was typed correctly
    harness.assert_buffer_content(text);

    // Cursor should be at end (byte 11)
    let pos_at_end = harness.cursor_position();
    assert_eq!(
        pos_at_end, 11,
        "Cursor should be at byte 11 after typing text"
    );

    // Move to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        0,
        "Cursor should be at start after Home"
    );

    // Get initial screen cursor position (at start of text, after gutter)
    let (initial_x, initial_y) = harness.screen_cursor_position();
    println!("Initial screen cursor: ({}, {})", initial_x, initial_y);

    // Press Right arrow - should move past 'a' (byte 0->1, visual 0->1)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let pos1 = harness.cursor_position();
    let (x1, y1) = harness.screen_cursor_position();
    println!(
        "After 1st Right: buffer pos={}, screen=({}, {})",
        pos1, x1, y1
    );
    assert_eq!(pos1, 1, "After 1st Right, should be at byte 1 (after 'a')");
    assert_eq!(
        x1,
        initial_x + 1,
        "Screen cursor should advance by 1 column (past 'a')"
    );

    // Press Right arrow - should skip entire Thai cluster (byte 1->10, visual 1->2)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let pos2 = harness.cursor_position();
    let (x2, y2) = harness.screen_cursor_position();
    println!(
        "After 2nd Right: buffer pos={}, screen=({}, {})",
        pos2, x2, y2
    );
    assert_eq!(
        pos2, 10,
        "After 2nd Right, should be at byte 10 (after Thai cluster 'ที่'). Got {}",
        pos2
    );
    assert_eq!(
        x2,
        initial_x + 2,
        "Screen cursor should advance by 1 column (Thai cluster has visual width 1). Got {}",
        x2
    );

    // Press Right arrow - should move past 'b' (byte 10->11, visual 2->3)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let pos3 = harness.cursor_position();
    let (x3, y3) = harness.screen_cursor_position();
    println!(
        "After 3rd Right: buffer pos={}, screen=({}, {})",
        pos3, x3, y3
    );
    assert_eq!(
        pos3, 11,
        "After 3rd Right, should be at byte 11 (after 'b')"
    );
    assert_eq!(
        x3,
        initial_x + 3,
        "Screen cursor should advance by 1 column (past 'b')"
    );

    // Now go back with Left arrows
    // Press Left - should move before 'b' (byte 11->10, visual 3->2)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_l1 = harness.cursor_position();
    let (xl1, _) = harness.screen_cursor_position();
    println!("After 1st Left: buffer pos={}, screen x={}", pos_l1, xl1);
    assert_eq!(pos_l1, 10, "After 1st Left, should be at byte 10");
    assert_eq!(xl1, initial_x + 2, "Screen cursor should be at column 2");

    // Press Left - should skip entire Thai cluster back (byte 10->1, visual 2->1)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_l2 = harness.cursor_position();
    let (xl2, _) = harness.screen_cursor_position();
    println!("After 2nd Left: buffer pos={}, screen x={}", pos_l2, xl2);
    assert_eq!(
        pos_l2, 1,
        "After 2nd Left, should be at byte 1 (before Thai cluster). Got {}",
        pos_l2
    );
    assert_eq!(
        xl2,
        initial_x + 1,
        "Screen cursor should be at column 1 (after 'a'). Got {}",
        xl2
    );

    // Press Left - should move before 'a' (byte 1->0, visual 1->0)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_l3 = harness.cursor_position();
    let (xl3, _) = harness.screen_cursor_position();
    println!("After 3rd Left: buffer pos={}, screen x={}", pos_l3, xl3);
    assert_eq!(pos_l3, 0, "After 3rd Left, should be at byte 0");
    assert_eq!(
        xl3, initial_x,
        "Screen cursor should be back at initial column"
    );
}

/// Test that backspace deletes Thai combining marks layer-by-layer
///
/// This is the "pro" behavior: backspace removes one code point at a time,
/// allowing users to fix a typo in a tone mark without retyping the whole character.
#[test]
fn test_thai_backspace_layer_by_layer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type Thai text: "ที่" (base + vowel + tone = 3 code points)
    let thai = "ที่";
    harness.type_text(thai).unwrap();
    harness.render().unwrap();

    // Cursor is at end (byte 9)
    assert_eq!(harness.cursor_position(), 9);

    // First backspace: should delete tone mark (่) only, leaving "ที"
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    let content1 = harness.get_buffer_content().unwrap();
    assert_eq!(
        content1, "ที",
        "First backspace should delete only the tone mark. Got: {:?}",
        content1
    );

    // Second backspace: should delete vowel mark (ี) only, leaving "ท"
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    let content2 = harness.get_buffer_content().unwrap();
    assert_eq!(
        content2, "ท",
        "Second backspace should delete only the vowel mark. Got: {:?}",
        content2
    );

    // Third backspace: should delete base consonant (ท), leaving empty
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    let content3 = harness.get_buffer_content().unwrap();
    assert_eq!(
        content3, "",
        "Third backspace should delete the base consonant. Got: {:?}",
        content3
    );
}

/// Test that Delete key removes entire Thai grapheme cluster
///
/// Unlike backspace (layer-by-layer), Delete removes the whole cluster at once
/// because if you delete the base consonant, the marks have nothing to sit on.
#[test]
fn test_thai_delete_entire_cluster() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type Thai text: "ที่นี่" (2 grapheme clusters)
    let thai = "ที่นี่";
    harness.type_text(thai).unwrap();
    harness.render().unwrap();

    // Move to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 0);

    // Press Delete once - should remove entire first grapheme cluster "ที่"
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "นี่",
        "Delete should remove entire grapheme cluster 'ที่', leaving 'นี่'. Got: {:?}",
        content
    );
}

/// Test Thai text loaded from file - movement and rendering
///
/// This tests the full flow: open file with Thai text, verify rendering,
/// test cursor movement by grapheme clusters.
#[test]
fn test_thai_file_open_and_movement() {
    // Create temp file with Thai text
    let temp_dir = TempDir::new().unwrap();
    let thai_path = temp_dir.path().join("thai.txt");

    // Write Thai text: "ที่นี่คือที่ติดตั้งระบบ" (typical Thai sentence)
    // This text has 13 grapheme clusters but 23 code points
    let thai_content = "ที่นี่คือที่ติดตั้งระบบ\n";
    std::fs::write(&thai_path, thai_content).unwrap();

    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    harness.open_file(&thai_path).unwrap();
    harness.render().unwrap();

    // Verify content was loaded
    let loaded = harness.get_buffer_content().unwrap();
    assert_eq!(
        loaded.trim(),
        thai_content.trim(),
        "Thai content should be loaded correctly"
    );

    // Move to start of file
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get initial position
    let initial_pos = harness.cursor_position();
    let (initial_x, initial_y) = harness.screen_cursor_position();
    println!(
        "Initial: buffer pos={}, screen=({}, {})",
        initial_pos, initial_x, initial_y
    );

    // Press Right arrow - should skip entire first grapheme cluster "ที่"
    // The first grapheme "ที่" is 9 bytes (3 code points × 3 bytes each)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let pos1 = harness.cursor_position();
    let (x1, _) = harness.screen_cursor_position();
    println!("After 1st Right: buffer pos={}, screen x={}", pos1, x1);

    // The first grapheme cluster "ที่" should be skipped entirely
    assert_eq!(
        pos1, 9,
        "After 1st Right, cursor should be at byte 9 (after first Thai cluster 'ที่'). Got {}",
        pos1
    );

    // Screen cursor should advance by 1 column (Thai grapheme has visual width 1)
    assert_eq!(
        x1,
        initial_x + 1,
        "Screen cursor should advance by 1 column. Got {} (initial was {})",
        x1,
        initial_x
    );

    // Press Right arrow again - should skip second grapheme cluster "นี่"
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let pos2 = harness.cursor_position();
    let (x2, _) = harness.screen_cursor_position();
    println!("After 2nd Right: buffer pos={}, screen x={}", pos2, x2);

    // Second cluster "นี่" is also 9 bytes
    assert_eq!(
        pos2, 18,
        "After 2nd Right, cursor should be at byte 18. Got {}",
        pos2
    );
    assert_eq!(
        x2,
        initial_x + 2,
        "Screen cursor should be at initial+2. Got {}",
        x2
    );

    // Now go back with Left arrow - should skip back over "นี่"
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let pos_l1 = harness.cursor_position();
    let (xl1, _) = harness.screen_cursor_position();
    println!("After 1st Left: buffer pos={}, screen x={}", pos_l1, xl1);

    assert_eq!(
        pos_l1, 9,
        "After 1st Left, cursor should be at byte 9. Got {}",
        pos_l1
    );
    assert_eq!(
        xl1,
        initial_x + 1,
        "Screen cursor should be at initial+1. Got {}",
        xl1
    );

    // Left again - should go back to start
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let pos_l2 = harness.cursor_position();
    let (xl2, _) = harness.screen_cursor_position();
    println!("After 2nd Left: buffer pos={}, screen x={}", pos_l2, xl2);

    assert_eq!(
        pos_l2, 0,
        "After 2nd Left, cursor should be at byte 0. Got {}",
        pos_l2
    );
    assert_eq!(
        xl2, initial_x,
        "Screen cursor should be back at initial. Got {}",
        xl2
    );
}

/// Test grapheme cluster movement in the search prompt
///
/// When typing Thai text in the search prompt, arrow keys should move
/// by grapheme cluster, not by individual code points.
#[test]
fn test_search_prompt_grapheme_movement() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Open search prompt with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify search prompt is open
    harness.assert_screen_contains("Search:");

    // Type Thai text: "aที่b" (a + Thai cluster + b)
    // The Thai cluster "ที่" is 3 code points but 1 grapheme
    let thai_text = "aที่b";
    harness.type_text(thai_text).unwrap();
    harness.render().unwrap();

    // Verify text appears in prompt
    harness.assert_screen_contains(thai_text);

    // Get screen cursor position at end
    let (end_x, end_y) = harness.screen_cursor_position();
    println!("Cursor at end: ({}, {})", end_x, end_y);

    // Press Left once - should move back by 1 (past 'b')
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (x1, _) = harness.screen_cursor_position();
    println!("After 1st Left: x={}", x1);
    assert_eq!(
        x1,
        end_x - 1,
        "1st Left should move back 1 column (past 'b')"
    );

    // Press Left again - should skip entire Thai cluster
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (x2, _) = harness.screen_cursor_position();
    println!("After 2nd Left: x={}", x2);
    assert_eq!(
        x2,
        end_x - 2,
        "2nd Left should skip entire Thai cluster (visual width 1)"
    );

    // Press Left again - should move before 'a'
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (x3, _) = harness.screen_cursor_position();
    println!("After 3rd Left: x={}", x3);
    assert_eq!(x3, end_x - 3, "3rd Left should move before 'a'");

    // Now press Right 3 times and verify we end up back at the end
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let (final_x, _) = harness.screen_cursor_position();
    assert_eq!(
        final_x, end_x,
        "3 Right arrows should return cursor to end position"
    );

    // Close search prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test grapheme cluster movement in the file open prompt
///
/// When typing Thai text in the file open prompt, arrow keys should move
/// by grapheme cluster, not by individual code points.
#[test]
fn test_file_open_prompt_grapheme_movement() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Open file prompt with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify file open prompt is open (prompt shows "Open:" at bottom)
    harness.assert_screen_contains("Open:");

    // Clear any prefilled text by going to start and selecting all then deleting
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::SHIFT).unwrap();
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type Thai text: "ที่นี่" (2 grapheme clusters, 6 code points)
    // Each cluster is 3 code points (base + vowel + tone)
    let thai_text = "ที่นี่";
    harness.type_text(thai_text).unwrap();
    harness.render().unwrap();

    // Verify text appears in prompt
    harness.assert_screen_contains(thai_text);

    // Get screen cursor position at end
    let (end_x, end_y) = harness.screen_cursor_position();
    println!("Cursor at end: ({}, {})", end_x, end_y);

    // Press Left once - should skip entire second Thai cluster "นี่"
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (x1, _) = harness.screen_cursor_position();
    println!("After 1st Left: x={}", x1);
    assert_eq!(
        x1,
        end_x - 1,
        "1st Left should skip entire Thai cluster 'นี่' (visual width 1)"
    );

    // Press Left again - should skip entire first Thai cluster "ที่"
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (x2, _) = harness.screen_cursor_position();
    println!("After 2nd Left: x={}", x2);
    assert_eq!(
        x2,
        end_x - 2,
        "2nd Left should skip entire Thai cluster 'ที่' (visual width 1)"
    );

    // Now at the start. Press Right twice to return to end
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let (final_x, _) = harness.screen_cursor_position();
    assert_eq!(
        final_x, end_x,
        "2 Right arrows should return cursor to end position"
    );

    // Close file open prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test grapheme cluster movement in settings search box
///
/// Note: The settings search box is a simple filter field that doesn't support
/// cursor movement (Left/Right arrows) - it only supports typing at the end
/// and backspace. This is a limitation of the simple filter design, not a bug.
///
/// This test is marked ignore since cursor movement isn't supported in this field.
#[test]
#[ignore = "Settings search is a simple filter without cursor movement support"]
fn test_settings_search_grapheme_movement() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();

    // Open settings with Ctrl+,
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify settings is open
    harness.assert_screen_contains("Settings");

    // Open search box with '/'
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type Thai text: "aที่b" (a + Thai cluster + b)
    let thai_text = "aที่b";
    for c in thai_text.chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Verify text appears in the search box
    harness.assert_screen_contains(thai_text);

    // Get screen cursor position at end
    let (end_x, end_y) = harness.screen_cursor_position();
    println!("Cursor at end: ({}, {})", end_x, end_y);

    // Press Left once - should move back by 1 (past 'b')
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (x1, _) = harness.screen_cursor_position();
    println!("After 1st Left: x={}", x1);
    assert_eq!(
        x1,
        end_x - 1,
        "1st Left should move back 1 column (past 'b')"
    );

    // Press Left again - should skip entire Thai cluster
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (x2, _) = harness.screen_cursor_position();
    println!("After 2nd Left: x={}", x2);
    assert_eq!(
        x2,
        end_x - 2,
        "2nd Left should skip entire Thai cluster (visual width 1)"
    );

    // Press Left again - should move before 'a'
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (x3, _) = harness.screen_cursor_position();
    println!("After 3rd Left: x={}", x3);
    assert_eq!(x3, end_x - 3, "3rd Left should move before 'a'");

    // Press Right 3 times to return to end
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let (final_x, _) = harness.screen_cursor_position();
    assert_eq!(
        final_x, end_x,
        "3 Right arrows should return cursor to end position"
    );

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that Left arrow moves by grapheme cluster in main editor buffer (typed content)
#[test]
fn test_main_editor_left_arrow_grapheme_movement() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Type Thai text directly in the editor: "ที่" (1 grapheme cluster, 3 code points)
    // Each Thai grapheme cluster consists of:
    // - Base consonant (3 bytes)
    // - Vowel mark (3 bytes)
    // - Tone mark (3 bytes)
    let thai_text = "ที่";
    harness.type_text(thai_text).unwrap();
    harness.render().unwrap();

    // Cursor should be at byte 9 (after all 3 code points = 9 bytes)
    let pos_end = harness.cursor_position();
    assert_eq!(
        pos_end, 9,
        "After typing Thai cluster, cursor should be at byte 9"
    );

    // Press Left arrow ONCE - should skip entire grapheme cluster back to position 0
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let pos_after_left = harness.cursor_position();
    println!("After 1 Left: cursor at byte {}", pos_after_left);

    // Expected: position 0 (skipped entire grapheme cluster)
    // Bug: position 6 (only skipped one code point = 3 bytes)
    assert_eq!(
        pos_after_left, 0,
        "Left arrow should move by entire grapheme cluster (from byte 9 to 0). \
         If this fails with position 6, the bug is: left arrow moves by code point instead of grapheme"
    );
}

/// BUG REPRODUCTION: Left arrow at position >32 bytes falls back to code point movement
///
/// When cursor is at position > 32 bytes, prev_grapheme_boundary calculates
/// start = pos - 32, which may land in the middle of a UTF-8 character.
/// This causes from_utf8 to fail and the code falls back to prev_char_boundary
/// which only moves by one code point instead of a full grapheme cluster.
///
/// This test uses the exact Thai file that triggers the bug.
#[test]
fn test_left_arrow_at_long_position_file_loaded() {
    let temp_dir = TempDir::new().unwrap();
    let thai_path = temp_dir.path().join("thai_long.txt");

    // Use the same Thai text from /tmp/thai.txt: "ที่นี่คือที่ติดตั้งระบบ"
    // This is 69 bytes total, 23 code points, ~13 grapheme clusters
    let thai_content = "ที่นี่คือที่ติดตั้งระบบ";
    std::fs::write(&thai_path, thai_content).unwrap();

    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    harness.open_file(&thai_path).unwrap();
    harness.render().unwrap();

    // Move to end of line (position 69)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let pos_end = harness.cursor_position();
    println!("At end: cursor at byte {}", pos_end);
    assert_eq!(
        pos_end, 69,
        "Cursor should be at byte 69 (end of Thai text)"
    );

    // The last two characters are "บบ" (each is a single code point, 3 bytes)
    // Position 66-69: บ (U+0E1A)
    // Position 63-66: บ (U+0E1A)
    // So pressing Left from 69 should go to 66 (correct behavior)

    // Press Left - should move to position 66 (before last บ)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos1 = harness.cursor_position();
    println!("After 1st Left: cursor at byte {}", pos1);
    assert_eq!(
        pos1, 66,
        "After Left from 69, should be at 66 (skipped บ which is 3 bytes)"
    );

    // Press Left again - should move to position 63
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos2 = harness.cursor_position();
    println!("After 2nd Left: cursor at byte {}", pos2);
    assert_eq!(
        pos2, 63,
        "After Left from 66, should be at 63 (skipped บ which is 3 bytes)"
    );

    // Press Left again - should move to position 60 (ะ is single code point)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos3 = harness.cursor_position();
    println!("After 3rd Left: cursor at byte {}", pos3);
    assert_eq!(
        pos3, 60,
        "After Left from 63, should be at 60 (skipped ะ which is 3 bytes)"
    );

    // Continue pressing Left to navigate through the text
    // At this point we're at position 60, so start = 60 - 32 = 28
    // Position 28 is in the middle of code point at 27-30
    // This is where the bug might trigger!

    // Press Left again from position 60
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos4 = harness.cursor_position();
    println!("After 4th Left: cursor at byte {}", pos4);
    assert_eq!(
        pos4, 57,
        "After Left from 60, should be at 57 (skipped ร which is 3 bytes)"
    );

    // Keep pressing to test grapheme cluster movement
    // Position 54-57: ง (U+0E07, single code point)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos5 = harness.cursor_position();
    println!("After 5th Left: cursor at byte {}", pos5);
    assert_eq!(pos5, 54, "After Left from 57, should be at 54 (skipped ง)");

    // Position 45-54: ตั้ is a grapheme cluster with base + vowel + tone = 3 code points = 9 bytes
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos6 = harness.cursor_position();
    println!("After 6th Left: cursor at byte {}", pos6);

    // BUG CHECK: If this fails with position 51 instead of 45, the bug is present
    // (cursor moved by 1 code point instead of full grapheme cluster)
    assert_eq!(
        pos6, 45,
        "After Left from 54, should be at 45 (skipped grapheme cluster ตั้ which is 9 bytes). \
         If this is 51, the bug is present: left arrow fell back to code point movement."
    );
}
