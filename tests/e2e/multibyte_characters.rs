use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::primitives::display_width::char_width;
use std::path::PathBuf;
use tempfile::TempDir;

/// Test End key behavior with Chinese characters
/// Bug: End doesn't go to the actual end of the line
#[test]
fn test_end_key_with_chinese_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a line with Chinese characters
    // Each Chinese character is 3 bytes in UTF-8
    let text = "你好世界";
    harness.type_text(text).unwrap();
    harness.assert_buffer_content(text);

    // Cursor should be at end (12 bytes: 4 chars × 3 bytes each)
    let pos_at_end = harness.cursor_position();
    assert_eq!(
        pos_at_end,
        text.len(),
        "Cursor should be at byte position {} after typing, got {}",
        text.len(),
        pos_at_end
    );

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert_eq!(
        harness.cursor_position(),
        0,
        "Cursor should be at position 0 after Home"
    );

    // Press End - should go back to end of line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    let pos_after_end = harness.cursor_position();
    assert_eq!(
        pos_after_end,
        text.len(),
        "End key should move cursor to byte position {} (end of '{}'), but got {}",
        text.len(),
        text,
        pos_after_end
    );
}

/// Test End key with mixed ASCII and Chinese characters
#[test]
fn test_end_key_with_mixed_ascii_and_chinese() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Mix of ASCII and Chinese: "Hello 你好 World"
    let text = "Hello 你好 World";
    harness.type_text(text).unwrap();
    harness.assert_buffer_content(text);

    let expected_len = text.len(); // "Hello " (6) + "你好" (6) + " World" (6) = 18 bytes

    // Move to beginning and back to end
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    let pos = harness.cursor_position();
    assert_eq!(
        pos, expected_len,
        "End should move to byte {}, got {}",
        expected_len, pos
    );

    // Verify we can type at the end
    harness.type_text("!").unwrap();
    harness.assert_buffer_content("Hello 你好 World!");
}

/// Test cursor left movement with Chinese characters
/// Bug: Cursor moves to half-characters (byte boundaries) instead of character boundaries
#[test]
fn test_cursor_left_with_chinese_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Chinese text: "你好" (2 characters, 6 bytes)
    let text = "你好";
    harness.type_text(text).unwrap();

    // Cursor should be at end (position 6)
    assert_eq!(harness.cursor_position(), 6);

    // Move left once - should go to position 3 (before '好', after '你')
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    let pos = harness.cursor_position();
    assert_eq!(
        pos, 3,
        "After one Left from end, cursor should be at byte 3 (between 你 and 好), got {}",
        pos
    );

    // Move left again - should go to position 0 (before '你')
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    let pos = harness.cursor_position();
    assert_eq!(
        pos, 0,
        "After two Lefts from end, cursor should be at byte 0 (before 你), got {}",
        pos
    );

    // Verify we can type at the correct position
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("X你好");
}

/// Test cursor right movement with Chinese characters
/// Bug: Cursor moves to half-characters (byte boundaries) instead of character boundaries
#[test]
fn test_cursor_right_with_chinese_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Chinese text: "你好世界" (4 characters, 12 bytes)
    let text = "你好世界";
    harness.type_text(text).unwrap();

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 0);

    // Move right once - should skip the entire first character (3 bytes)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    let pos1 = harness.cursor_position();
    assert_eq!(
        pos1, 3,
        "After one Right, cursor should be at byte 3 (after 你), got {}",
        pos1
    );

    // Move right again - should be at byte 6 (after 好)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    let pos2 = harness.cursor_position();
    assert_eq!(
        pos2, 6,
        "After two Rights, cursor should be at byte 6 (after 好), got {}",
        pos2
    );

    // Move right again - should be at byte 9 (after 世)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    let pos3 = harness.cursor_position();
    assert_eq!(
        pos3, 9,
        "After three Rights, cursor should be at byte 9 (after 世), got {}",
        pos3
    );

    // Move right again - should be at byte 12 (after 界, at end)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    let pos4 = harness.cursor_position();
    assert_eq!(
        pos4, 12,
        "After four Rights, cursor should be at byte 12 (end), got {}",
        pos4
    );

    // Verify by inserting at each position
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("你好X世界");
}

/// Test cursor right/left with mixed ASCII and Chinese
#[test]
fn test_cursor_movement_mixed_content() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // "a你b好c" - alternating ASCII and Chinese
    let text = "a你b好c";
    harness.type_text(text).unwrap();
    // Byte positions: a=0, 你=1-3, b=4, 好=5-7, c=8
    // Character boundaries: 0, 1, 4, 5, 8, 9

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Move right through each character and verify positions
    let expected_positions = [1, 4, 5, 8, 9]; // after a, 你, b, 好, c

    for (i, expected) in expected_positions.iter().enumerate() {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        let pos = harness.cursor_position();
        assert_eq!(
            pos,
            *expected,
            "After {} right movements, cursor should be at byte {}, got {}",
            i + 1,
            expected,
            pos
        );
    }
}

/// Test cursor up/down maintains column position with Chinese characters
/// Bug: Cursor up/down does not move to the same visual column on adjacent lines
#[test]
fn test_cursor_up_down_column_position_with_chinese() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create two lines with Chinese characters
    // Note: Chinese characters are typically double-width on screen
    // Line 1: "你好世界" (4 chars, 8 columns visually, 12 bytes)
    // Line 2: "中文测试" (4 chars, 8 columns visually, 12 bytes)
    harness.type_text("你好世界").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("中文测试").unwrap();

    harness.assert_buffer_content("你好世界\n中文测试");

    // Position cursor in middle of line 2 (after "中文")
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    let pos_line2 = harness.cursor_position();
    // Should be at byte 13 + 6 = 19 (after newline at 12, then "中文" = 6 bytes)
    assert_eq!(
        pos_line2, 19,
        "Cursor should be at byte 19 (after 中文 on line 2), got {}",
        pos_line2
    );

    // Move up - should go to equivalent visual column on line 1
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    let pos_line1 = harness.cursor_position();

    // If column tracking is correct, cursor should be at visual column 4
    // which corresponds to byte position 6 (after "你好")
    assert_eq!(
        pos_line1, 6,
        "After Up from middle of line 2, cursor should be at byte 6 (after 你好 on line 1), got {}. \
         This indicates column tracking is broken for double-width characters.",
        pos_line1
    );

    // Move down - should return to same position on line 2
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let pos_back_line2 = harness.cursor_position();
    assert_eq!(
        pos_back_line2, 19,
        "After Down, cursor should return to byte 19 (after 中文 on line 2), got {}",
        pos_back_line2
    );
}

/// Test cursor up/down with mixed width characters
/// BUG: Cursor up/down can land in the middle of a multi-byte character (e.g., byte 5
/// which is inside "你" spanning bytes 3-5). This causes cursor corruption and
/// potential text corruption when editing.
/// This test documents this bug and will fail until the issue is fixed.
#[test]
fn test_cursor_up_down_mixed_width_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Line 1: "abc你好def" (9 chars: 3 ASCII + 2 Chinese + 3 ASCII)
    // Visual columns: a(1) b(2) c(3) 你(4-5) 好(6-7) d(8) e(9) f(10)
    // Byte positions: a=0, b=1, c=2, 你=3-5, 好=6-8, d=9, e=10, f=11
    harness.type_text("abc你好def").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Line 2: "1234567890" (10 ASCII chars)
    harness.type_text("1234567890").unwrap();

    harness.assert_buffer_content("abc你好def\n1234567890");

    // Position at middle of line 2 (after "12345", visual column 5)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    let pos_line2 = harness.cursor_position();
    // Line 1: "abc你好def" = 12 bytes, newline = 1 byte, so line 2 starts at byte 13
    // After 5 Right movements: 13 + 5 = 18
    assert_eq!(pos_line2, 18, "Should be at byte 18 (after '12345')");

    // Move up - visual column 5 should map to somewhere in or after "你"
    // Visual column 5 is in the middle of "你" (columns 4-5), so should clamp to after "你" (byte 6)
    // or possibly to after "c" (byte 3) depending on implementation
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    let pos_line1 = harness.cursor_position();

    // The exact position depends on how the editor handles double-width characters
    // It should NOT be at a byte boundary that splits a Chinese character
    // Valid positions: 0, 1, 2, 3, 6, 9, 10, 11, 12 (not 4, 5, 7, 8)
    let valid_positions = [0, 1, 2, 3, 6, 9, 10, 11, 12];
    assert!(
        valid_positions.contains(&pos_line1),
        "Cursor should be at a valid character boundary after Up, got byte {}. \
         Valid positions are: {:?}. Position {} may indicate cursor landed mid-character.",
        pos_line1,
        valid_positions,
        pos_line1
    );
}

/// Test selection with Chinese characters using Shift+Right
/// Bug: Selection of text goes haywire
#[test]
fn test_selection_shift_right_chinese() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let text = "你好世界";
    harness.type_text(text).unwrap();

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Select first character with Shift+Right
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();

    // Verify cursor moved by one character (3 bytes)
    let pos = harness.cursor_position();
    assert_eq!(
        pos, 3,
        "After Shift+Right, cursor should be at byte 3 (after 你), got {}",
        pos
    );

    // Delete the selection
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("好世界");
}

/// Test selection with Shift+Left on Chinese characters
#[test]
fn test_selection_shift_left_chinese() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let text = "你好世界";
    harness.type_text(text).unwrap();

    // Cursor is at end (byte 12)
    assert_eq!(harness.cursor_position(), 12);

    // Select last character with Shift+Left
    harness
        .send_key(KeyCode::Left, KeyModifiers::SHIFT)
        .unwrap();

    let pos = harness.cursor_position();
    assert_eq!(
        pos, 9,
        "After Shift+Left, cursor should be at byte 9 (before 界), got {}",
        pos
    );

    // Replace selection with 'X'
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("你好世X");
}

/// Test selecting multiple Chinese characters
#[test]
fn test_selection_multiple_chinese_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let text = "一二三四五";
    harness.type_text(text).unwrap();

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Select first 3 characters with Shift+Right x3
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    let pos = harness.cursor_position();
    assert_eq!(
        pos, 9,
        "After 3x Shift+Right, cursor should be at byte 9 (after 三), got {}",
        pos
    );

    // Delete selection
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("四五");
}

/// Test Shift+End selection with Chinese characters
#[test]
fn test_selection_shift_end_chinese() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let text = "你好世界";
    harness.type_text(text).unwrap();

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Move right once (after 你)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(harness.cursor_position(), 3);

    // Select to end with Shift+End
    harness.send_key(KeyCode::End, KeyModifiers::SHIFT).unwrap();

    let pos = harness.cursor_position();
    assert_eq!(
        pos, 12,
        "After Shift+End, cursor should be at byte 12 (end), got {}",
        pos
    );

    // Replace selection
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("你X");
}

/// Test Shift+Home selection with Chinese characters
#[test]
fn test_selection_shift_home_chinese() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let text = "你好世界";
    harness.type_text(text).unwrap();

    // Cursor at end (byte 12)
    // Move left once (before 界, at byte 9)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 9);

    // Select to beginning with Shift+Home
    harness
        .send_key(KeyCode::Home, KeyModifiers::SHIFT)
        .unwrap();

    let pos = harness.cursor_position();
    assert_eq!(
        pos, 0,
        "After Shift+Home, cursor should be at byte 0, got {}",
        pos
    );

    // Replace selection
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("X界");
}

/// Test backspace with Chinese characters
#[test]
fn test_backspace_chinese_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let text = "你好";
    harness.type_text(text).unwrap();
    harness.assert_buffer_content("你好");

    // Backspace should delete entire '好' character (3 bytes), not just 1 byte
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("你");

    // Backspace again should delete '你'
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("");
}

/// Test delete (forward) with Chinese characters
#[test]
fn test_delete_forward_chinese_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let text = "你好";
    harness.type_text(text).unwrap();

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Delete should remove entire '你' character
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("好");

    // Delete should remove '好'
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("");
}

/// Test backspace in middle of mixed ASCII and Chinese content
/// Ensures backspace deletes whole characters, not bytes
#[test]
fn test_backspace_middle_of_mixed_content() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // "abc你好def" - alternating ASCII and Chinese
    let text = "abc你好def";
    harness.type_text(text).unwrap();
    harness.assert_buffer_content(text);

    // Move to position after "abc你" (byte 6)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..4 {
        // a, b, c, 你
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    assert_eq!(harness.cursor_position(), 6, "Should be after 你");

    // Backspace should delete entire '你' (3 bytes), not just 1 byte
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("abc好def");
    assert_eq!(
        harness.cursor_position(),
        3,
        "Cursor should be at byte 3 after deleting 你"
    );

    // Verify the content is valid UTF-8 and contains expected characters
    let content = harness.get_buffer_content().unwrap();
    assert!(content.chars().count() == 7, "Should have 7 characters");
    assert!(
        content.is_char_boundary(3),
        "Position 3 should be a valid boundary"
    );
}

/// Test delete (forward) in middle of mixed ASCII and Chinese content
#[test]
fn test_delete_forward_middle_of_mixed_content() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // "abc你好def"
    let text = "abc你好def";
    harness.type_text(text).unwrap();

    // Move to position before "你" (byte 3)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        // a, b, c
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    assert_eq!(harness.cursor_position(), 3, "Should be before 你");

    // Delete should remove entire '你' (3 bytes), leaving cursor at same position
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("abc好def");
    assert_eq!(
        harness.cursor_position(),
        3,
        "Cursor should stay at byte 3 after forward delete"
    );

    // Delete again should remove '好'
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("abcdef");
    assert_eq!(harness.cursor_position(), 3);
}

/// Test that backspace/delete don't corrupt UTF-8 sequences
/// This is critical: partial byte deletion would create invalid UTF-8
#[test]
fn test_backspace_delete_never_corrupt_utf8() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Text with various multi-byte characters
    let text = "a中b文c字d";
    harness.type_text(text).unwrap();

    // Delete from end, one at a time, verifying buffer stays valid
    for _ in 0..7 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        let content = harness.get_buffer_content().unwrap();
        // This will panic if content is not valid UTF-8
        assert!(
            content.is_char_boundary(content.len()),
            "Content should be valid UTF-8: {:?}",
            content
        );
    }

    harness.assert_buffer_content("");

    // Now test forward delete
    harness.type_text(text).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    for _ in 0..7 {
        harness
            .send_key(KeyCode::Delete, KeyModifiers::NONE)
            .unwrap();
        let content = harness.get_buffer_content().unwrap();
        // This will panic if content is not valid UTF-8
        assert!(
            content.is_char_boundary(content.len()),
            "Content should be valid UTF-8 after forward delete: {:?}",
            content
        );
    }

    harness.assert_buffer_content("");
}

/// Test file roundtrip with Chinese characters
#[test]
fn test_chinese_file_save_roundtrip() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("chinese.txt");

    // Create file with Chinese content
    let content = "你好世界\n中文测试\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify content loaded correctly
    harness.assert_buffer_content(content);

    // Edit: go to end of first line and add text
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("！").unwrap(); // Chinese exclamation mark (3 bytes)

    // Save
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify file was saved correctly
    let saved = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        saved, "你好世界！\n中文测试\n",
        "File should contain edited Chinese text"
    );
}

/// Test mouse click positioning with double-width and multi-byte characters
/// Verifies clicks before, in the middle of, and after double-width characters
/// position the cursor correctly at valid character boundaries.
#[test]
fn test_mouse_click_double_width_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    // "你好" - two Chinese characters, each 3 bytes and 2 columns wide
    let text = "你好";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    // Get content row and gutter width
    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Valid byte boundaries: 0 (before 你), 3 (after 你), 6 (after 好)
    // Visual layout: [gutter][你][好]
    //                       0-1  2-3  (columns relative to gutter)

    // Test 1: Click before first character (column 0) -> byte 0
    harness.mouse_click(gutter_x, row).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        0,
        "Click at gutter edge should position at byte 0"
    );

    // Test 2: Click in first half of 你 (column 0) -> should snap to byte 0 or 3
    // Add delay to avoid double-click detection
    harness.sleep(double_click_delay);
    harness.mouse_click(gutter_x, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        pos == 0 || pos == 3,
        "Click in first column of 你 should snap to byte 0 or 3, got {}",
        pos
    );

    // Test 3: Click in second half of 你 (column 1) -> should snap to byte 3 (after 你)
    harness.sleep(double_click_delay);
    harness.mouse_click(gutter_x + 1, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        pos == 0 || pos == 3,
        "Click in second column of 你 should snap to byte 0 or 3, got {}",
        pos
    );

    // Test 4: Click at boundary between 你 and 好 (column 2) -> byte 3
    // Add delay to avoid double-click detection
    harness.sleep(double_click_delay);
    harness.mouse_click(gutter_x + 2, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        pos == 3 || pos == 6,
        "Click at boundary should snap to byte 3 or 6, got {}",
        pos
    );

    // Test 5: Click in 好 (columns 2-3) -> should snap to byte 3 or 6
    harness.mouse_click(gutter_x + 3, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        pos == 3 || pos == 6,
        "Click in 好 should snap to byte 3 or 6, got {}",
        pos
    );

    // Test 6: Click after 好 (column 4+) -> byte 6
    harness.mouse_click(gutter_x + 4, row).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        6,
        "Click after 好 should position at byte 6 (end)"
    );
}

/// Test mouse click on mixed ASCII and double-width content
/// Verifies cursor lands at correct byte positions, not mid-character
#[test]
fn test_mouse_click_mixed_ascii_and_double_width() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    // "a你b" - ASCII, Chinese (2 cols), ASCII
    // Bytes: a=0, 你=1-3, b=4
    // Visual columns: a=0, 你=1-2, b=3
    let text = "a你b";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Valid byte boundaries: 0, 1, 4, 5
    let valid_boundaries = [0, 1, 4, 5];

    // Click on 'a' (column 0) -> byte 0 or 1
    harness.mouse_click(gutter_x, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        valid_boundaries.contains(&pos),
        "Click on 'a' should land on valid boundary, got byte {}",
        pos
    );

    // Add delay to avoid double-click detection
    harness.sleep(double_click_delay);
    // Click in middle of 你 (column 1 or 2) -> should NOT be byte 2 or 3
    harness.mouse_click(gutter_x + 1, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        valid_boundaries.contains(&pos),
        "Click in middle of 你 should snap to valid boundary (0, 1, 4, or 5), got byte {} which is mid-character!",
        pos
    );

    // Add delay to avoid double-click detection
    harness.sleep(double_click_delay);
    harness.mouse_click(gutter_x + 2, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        valid_boundaries.contains(&pos),
        "Click on second column of 你 should snap to valid boundary, got byte {} which is mid-character!",
        pos
    );

    // Add delay to avoid double-click detection
    harness.sleep(double_click_delay);
    // Click on 'b' (column 3) -> byte 4 or 5
    harness.mouse_click(gutter_x + 3, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        pos == 4 || pos == 5,
        "Click on 'b' should position at byte 4 or 5, got {}",
        pos
    );

    // Add delay to avoid double-click detection
    harness.sleep(double_click_delay);
    // Click after 'b' (column 4+) -> byte 5
    harness.mouse_click(gutter_x + 4, row).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        5,
        "Click after content should position at end (byte 5)"
    );
}

/// Test mouse click with emoji (4-byte, double-width characters)
#[test]
fn test_mouse_click_emoji() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    // "🚀X" - rocket emoji (4 bytes, 2 cols) + ASCII
    let text = "🚀X";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Valid byte boundaries: 0, 4, 5
    // Visual: 🚀 at cols 0-1, X at col 2

    // Click in emoji (cols 0-1) should snap to byte 0 or 4, NOT 1, 2, or 3
    harness.mouse_click(gutter_x, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        pos == 0 || pos == 4,
        "Click in first column of 🚀 should snap to byte 0 or 4, got {} (mid-emoji!)",
        pos
    );

    // Add delay to avoid double-click detection
    harness.sleep(double_click_delay);
    harness.mouse_click(gutter_x + 1, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        pos == 0 || pos == 4,
        "Click in second column of 🚀 should snap to byte 0 or 4, got {} (mid-emoji!)",
        pos
    );

    // Add delay to avoid double-click detection
    harness.sleep(double_click_delay);
    // Click on X (col 2) -> byte 4 or 5
    harness.mouse_click(gutter_x + 2, row).unwrap();
    harness.render().unwrap();
    let pos = harness.cursor_position();
    assert!(
        pos == 4 || pos == 5,
        "Click on X should position at byte 4 or 5, got {}",
        pos
    );

    // Add delay to avoid double-click detection
    harness.sleep(double_click_delay);
    // Click after X (col 3+) -> byte 5
    harness.mouse_click(gutter_x + 3, row).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        5,
        "Click after X should position at byte 5"
    );
}

/// Test that mouse click never positions cursor at invalid UTF-8 boundary
#[test]
fn test_mouse_click_never_lands_mid_character() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Complex line with various multi-byte characters
    // "Hello你好🚀World"
    let text = "Hello你好🚀World";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Valid byte boundaries
    let valid_boundaries: Vec<usize> = text
        .char_indices()
        .map(|(i, _)| i)
        .chain(std::iter::once(text.len()))
        .collect();

    // Click at every visual column and verify cursor lands on valid boundary
    // Visual width: Hello(5) + 你(2) + 好(2) + 🚀(2) + World(5) = 16 columns
    for col in 0..20 {
        harness.mouse_click(gutter_x + col, row).unwrap();
        harness.render().unwrap();

        let pos = harness.cursor_position();
        assert!(
            valid_boundaries.contains(&pos),
            "Click at visual column {} landed at byte {}, which is NOT a valid character boundary! Valid: {:?}",
            col, pos, valid_boundaries
        );
    }
}

/// Test word movement (Ctrl+Left/Right) with Chinese characters
/// Chinese text typically doesn't have word boundaries like English
/// Note: Word movement behavior with Chinese may vary by implementation
#[test]
fn test_word_movement_chinese() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Mixed content with spaces
    let text = "Hello 你好 World 世界";
    harness.type_text(text).unwrap();

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    let start_pos = harness.cursor_position();
    assert_eq!(start_pos, 0);

    // Ctrl+Right should move forward by word
    // The exact stopping point depends on how the editor handles Chinese character boundaries
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL)
        .unwrap();
    let pos1 = harness.cursor_position();

    // Should have moved forward from position 0
    assert!(
        pos1 > 0,
        "Ctrl+Right should move cursor forward, got byte {}",
        pos1
    );

    // The cursor should land on a valid character boundary
    let valid_positions: Vec<usize> = text
        .char_indices()
        .map(|(i, _)| i)
        .chain(std::iter::once(text.len()))
        .collect();
    assert!(
        valid_positions.contains(&pos1),
        "Ctrl+Right should land on a character boundary, got byte {} which is invalid. Valid: {:?}",
        pos1,
        valid_positions
    );

    // Continue moving and verify we eventually reach the end
    let mut last_pos = pos1;
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::CONTROL)
            .unwrap();
        let new_pos = harness.cursor_position();
        if new_pos == last_pos {
            break; // Hit the end
        }
        assert!(
            new_pos > last_pos,
            "Ctrl+Right should advance cursor or stay at end"
        );
        assert!(
            valid_positions.contains(&new_pos),
            "Ctrl+Right landed on invalid position {} (mid-character)",
            new_pos
        );
        last_pos = new_pos;
    }
}

/// Test that cursor never lands on invalid UTF-8 boundaries
#[test]
fn test_cursor_never_splits_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Text with various multi-byte characters
    let text = "a你b好c世d界e";
    harness.type_text(text).unwrap();

    // Valid byte positions (character boundaries)
    let valid_positions: Vec<usize> = text
        .char_indices()
        .map(|(i, _)| i)
        .chain(std::iter::once(text.len()))
        .collect();

    // Move through entire text with Right arrow and verify each position
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    let mut positions_visited = vec![harness.cursor_position()];
    while harness.cursor_position() < text.len() {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        let pos = harness.cursor_position();
        positions_visited.push(pos);

        assert!(
            valid_positions.contains(&pos),
            "Cursor at byte {} is not a valid character boundary! Valid positions: {:?}",
            pos,
            valid_positions
        );
    }

    // Move back with Left arrow
    while harness.cursor_position() > 0 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
        let pos = harness.cursor_position();

        assert!(
            valid_positions.contains(&pos),
            "Cursor at byte {} is not a valid character boundary when moving left! Valid: {:?}",
            pos,
            valid_positions
        );
    }
}

/// Test that End key positions screen cursor after the last character visually
/// BUG: When a line contains double-width characters (CJK, emoji), pressing End
/// places the screen cursor at the wrong X position. The cursor appears before
/// the last character instead of after it.
///
/// Example: Line "🚀_Launch" (rocket emoji + "_Launch")
/// - Rocket emoji is 2 columns wide visually
/// - "_Launch" is 7 columns
/// - Expected cursor X after End: gutter + 9 columns
/// - Actual cursor X: gutter + 8 columns (1 short!)
#[test]
fn test_end_key_screen_cursor_position_double_width() {
    // Test cases: (content, expected_visual_width)
    let test_cases = [
        ("月", 2),        // Single CJK character (2 cols)
        ("🚀_Launch", 9), // Emoji (2) + ASCII (7) = 9 cols
        ("你好", 4),      // Two CJK chars = 4 cols
        ("Hello世界", 9), // ASCII (5) + CJK (4) = 9 cols
        ("a中b文c", 7),   // Mixed: 3 ASCII + 2 CJK (4) = 7 cols
    ];

    for (content, expected_width) in test_cases {
        let mut h = EditorTestHarness::new(80, 24).unwrap();
        h.type_text(content).unwrap();
        h.render().unwrap();

        // Get gutter width by going to Home
        h.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        let (gutter_x, _) = h.screen_cursor_position();

        // Go to End
        h.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        let (end_x, _) = h.screen_cursor_position();

        let actual_width = end_x as usize - gutter_x as usize;
        assert_eq!(
            actual_width, expected_width,
            "Content {:?}: Screen cursor at End should be {} columns from gutter, but is {}. \
             BUG: Double-width characters counted as single-width.",
            content, expected_width, actual_width
        );
    }
}

/// Test that Left/Right arrow keys move screen cursor by correct visual width
/// BUG: When moving past a double-width character (CJK, emoji), the screen cursor
/// only advances by 1 column instead of 2. This causes the cursor to drift left
/// of where it should be, getting progressively worse with more double-width chars.
///
/// Example: "🚀_Launch" starting at x=8 (gutter)
/// - After Right past 🚀: expected x=10, actual x=9 (1 behind!)
/// - After Right past _: expected x=11, actual x=10 (still 1 behind)
/// - ...cursor stays 1 column behind for rest of line
#[test]
fn test_cursor_left_right_screen_position_double_width() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // "🚀_Launch" - emoji (2 cols) + 7 ASCII chars
    let text = "🚀_Launch";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    // Go to Home to get gutter position
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Expected screen X positions after each Right:
    // Start: gutter_x (before 🚀)
    // After 🚀: gutter_x + 2 (emoji is 2 cols wide)
    // After _: gutter_x + 3
    // After L: gutter_x + 4
    // ... etc
    let expected_offsets = [0, 2, 3, 4, 5, 6, 7, 8, 9]; // cumulative visual widths
    let chars: Vec<char> = text.chars().collect();

    for (i, expected_offset) in expected_offsets.iter().enumerate() {
        let (actual_x, _) = harness.screen_cursor_position();
        let expected_x = gutter_x + *expected_offset as u16;

        let char_desc = if i == 0 {
            "start".to_string()
        } else {
            format!("after {:?}", chars[i - 1])
        };

        assert_eq!(
            actual_x, expected_x,
            "Position {}: Screen cursor X should be {} (gutter {} + offset {}), got {}. \
             BUG: Double-width char only advanced cursor by 1. ({})",
            i, expected_x, gutter_x, expected_offset, actual_x, char_desc
        );

        if i < expected_offsets.len() - 1 {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
            harness.render().unwrap();
        }
    }

    // Now test going back with Left - should also move by correct visual width
    for i in (0..expected_offsets.len() - 1).rev() {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let (actual_x, _) = harness.screen_cursor_position();
        let expected_x = gutter_x + expected_offsets[i] as u16;

        assert_eq!(
            actual_x,
            expected_x,
            "Left movement {}: Screen cursor X should be {}, got {}. \
             BUG: Double-width char only moved cursor by 1.",
            expected_offsets.len() - 1 - i,
            expected_x,
            actual_x
        );
    }
}

/// Test screen cursor position matches visual expectation with double-width chars
/// BUG: Chinese characters (which are double-width) only advance screen cursor by 1
/// instead of 2, causing visual desync between cursor position and actual character position.
/// This test documents this bug and will fail until the issue is fixed.
#[test]
fn test_screen_cursor_position_double_width() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let text = "你好";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (start_x, start_y) = harness.screen_cursor_position();

    // Move right once (past 你)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let (after_one_x, after_one_y) = harness.screen_cursor_position();

    // Chinese character is double-width, so screen X should increase by 2
    assert_eq!(after_one_y, start_y, "Y position should not change");
    assert_eq!(
        after_one_x,
        start_x + 2,
        "Screen X should increase by 2 for double-width character. Start: {}, After: {}",
        start_x,
        after_one_x
    );

    // Move right again (past 好)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let (after_two_x, _) = harness.screen_cursor_position();
    assert_eq!(
        after_two_x,
        start_x + 4,
        "Screen X should be start + 4 after two double-width chars. Start: {}, After: {}",
        start_x,
        after_two_x
    );
}

/// Comprehensive test that iterates over all lines in the multi-byte fixture file
/// and verifies cursor operations work correctly for each line.
///
/// Tests performed for each content line:
/// - End key goes to actual end of line
/// - Home key goes to start of line
/// - Left/Right navigation lands on valid character boundaries
/// - Cursor position after operations matches expected byte positions
/// - Screen cursor is visible and in correct position after render
#[test]
fn test_all_operations_on_multibyte_fixture() {
    let fixture_path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/multi-byte.txt");

    let fixture_content =
        std::fs::read_to_string(&fixture_path).expect("Failed to read multi-byte.txt fixture");

    // Extract content lines (skip comments and empty lines)
    let content_lines: Vec<&str> = fixture_content
        .lines()
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .collect();

    println!("Testing {} content lines from fixture", content_lines.len());

    for (line_idx, line) in content_lines.iter().enumerate() {
        println!("\n=== Testing line {}: {:?} ===", line_idx, line);

        // Skip lines that are just placeholders for invalid UTF-8
        if line.contains("Representative") {
            println!("  Skipping placeholder line");
            continue;
        }

        let mut harness = EditorTestHarness::new(120, 30).unwrap();

        // Type the line content
        harness.type_text(line).unwrap();
        harness.render().unwrap();

        // Verify buffer content matches
        harness.assert_buffer_content(line);

        let line_len = line.len();
        let char_count = line.chars().count();

        println!("  Line length: {} bytes, {} chars", line_len, char_count);

        // --- Test 1: End key goes to actual end ---
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let pos_after_end = harness.cursor_position();
        assert_eq!(
            pos_after_end, line_len,
            "Line {}: End key should move to byte {} but got {}. Line: {:?}",
            line_idx, line_len, pos_after_end, line
        );

        // Verify screen cursor is at expected position after End
        let (screen_x_end, screen_y_end) = harness.screen_cursor_position();
        let (content_start, _) = harness.content_area_rows();
        assert_eq!(
            screen_y_end, content_start as u16,
            "Line {}: Cursor should be on content row after End",
            line_idx
        );

        // Calculate expected visual width using char_width per character (same as production code)
        // Note: This sums individual character widths, which differs from str_width() for ZWJ sequences
        let expected_visual_width: usize = line.chars().map(|c| char_width(c)).sum();

        // Screen X = gutter_width + visual_content_width
        // Get gutter width by checking cursor X at Home position
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let (gutter_x, _) = harness.screen_cursor_position();
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let expected_screen_x = gutter_x as usize + expected_visual_width;
        assert_eq!(
            screen_x_end as usize, expected_screen_x,
            "Line {}: Screen cursor X at End should be {} (gutter {} + visual width {}), but got {}. \
             BUG: Double-width characters may be counted as single-width. Line: {:?}",
            line_idx, expected_screen_x, gutter_x, expected_visual_width, screen_x_end, line
        );

        println!(
            "  End: cursor at byte {}, screen ({}, {}), expected x={}",
            pos_after_end, screen_x_end, screen_y_end, expected_screen_x
        );

        // --- Test 2: Home key goes to start ---
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let pos_after_home = harness.cursor_position();
        assert_eq!(
            pos_after_home, 0,
            "Line {}: Home key should move to byte 0 but got {}",
            line_idx, pos_after_home
        );

        // --- Test 3: Right arrow traverses all characters, checking both byte AND screen position ---
        let valid_boundaries: Vec<usize> = line
            .char_indices()
            .map(|(i, _)| i)
            .chain(std::iter::once(line_len))
            .collect();

        // Calculate visual width for each character using char_width (same as production code)
        let char_widths: Vec<usize> = line.chars().map(|c| char_width(c)).collect();

        let mut positions_visited = vec![0usize];
        let mut prev_pos = 0;
        let mut cumulative_visual_width = 0usize;

        // Verify starting screen position
        harness.render().unwrap();
        let (start_screen_x, _) = harness.screen_cursor_position();
        assert_eq!(
            start_screen_x, gutter_x,
            "Line {}: At Home, screen X should be gutter ({}), got {}",
            line_idx, gutter_x, start_screen_x
        );

        for move_count in 1..=char_count {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
            harness.render().unwrap();
            let pos = harness.cursor_position();

            assert!(
                valid_boundaries.contains(&pos),
                "Line {}: After {} Right moves, cursor at byte {} is not a valid boundary. Valid: {:?}. Line: {:?}",
                line_idx, move_count, pos, valid_boundaries, line
            );

            assert!(
                pos > prev_pos || pos == line_len,
                "Line {}: Right should advance cursor. Was at {}, now at {}",
                line_idx,
                prev_pos,
                pos
            );

            // Check screen cursor position
            cumulative_visual_width += char_widths[move_count - 1];
            let expected_screen_x = gutter_x as usize + cumulative_visual_width;
            let (actual_screen_x, _) = harness.screen_cursor_position();

            assert_eq!(
                actual_screen_x as usize, expected_screen_x,
                "Line {}: After {} Right moves, screen X should be {} (gutter {} + visual {}), got {}. \
                 BUG: Double-width char only advanced cursor by 1. Line: {:?}",
                line_idx, move_count, expected_screen_x, gutter_x, cumulative_visual_width, actual_screen_x, line
            );

            positions_visited.push(pos);
            prev_pos = pos;
        }

        // Should have reached the end
        assert_eq!(
            harness.cursor_position(),
            line_len,
            "Line {}: After {} Right moves, should be at end (byte {})",
            line_idx,
            char_count,
            line_len
        );

        println!("  Right traversal: {:?}", positions_visited);

        // --- Test 4: Left arrow traverses back, landing on valid boundaries ---
        for move_count in 1..=char_count {
            harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
            let pos = harness.cursor_position();

            assert!(
                valid_boundaries.contains(&pos),
                "Line {}: After {} Left moves, cursor at byte {} is not a valid boundary. Valid: {:?}",
                line_idx, move_count, pos, valid_boundaries
            );
        }

        assert_eq!(
            harness.cursor_position(),
            0,
            "Line {}: After {} Left moves, should be at start (byte 0)",
            line_idx,
            char_count
        );

        // --- Test 5: Selection with Shift+Right selects whole characters ---
        if char_count >= 2 {
            harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
            harness
                .send_key(KeyCode::Right, KeyModifiers::SHIFT)
                .unwrap();

            let pos_after_shift_right = harness.cursor_position();
            assert!(
                valid_boundaries.contains(&pos_after_shift_right),
                "Line {}: Shift+Right should land on valid boundary, got {}",
                line_idx,
                pos_after_shift_right
            );

            // Type to replace selection
            harness.type_text("X").unwrap();
            let content_after_replace = harness.get_buffer_content().unwrap();

            // First character should be replaced with X
            assert!(
                content_after_replace.starts_with('X'),
                "Line {}: After Shift+Right then typing X, content should start with X. Got: {:?}",
                line_idx,
                content_after_replace
            );

            println!("  Selection replace: first char -> X, result starts with X: ✓");
        }

        // --- Test 6: Backspace deletes whole characters ---
        let mut harness2 = EditorTestHarness::new(120, 30).unwrap();
        harness2.type_text(line).unwrap();

        for del_count in 1..=char_count {
            let before_len = harness2.get_buffer_content().unwrap().len();
            harness2
                .send_key(KeyCode::Backspace, KeyModifiers::NONE)
                .unwrap();
            let after_content = harness2.get_buffer_content().unwrap();

            // Content should remain valid UTF-8 (this will panic if not)
            assert!(
                after_content.is_char_boundary(after_content.len()),
                "Line {}: After {} backspaces, content is invalid UTF-8",
                line_idx,
                del_count
            );

            // Length should have decreased
            assert!(
                after_content.len() < before_len,
                "Line {}: Backspace {} didn't reduce content length",
                line_idx,
                del_count
            );
        }

        harness2.assert_buffer_content("");
        println!("  Backspace: deleted {} chars one by one: ✓", char_count);

        // --- Test 7: Delete (forward) deletes whole characters ---
        let mut harness3 = EditorTestHarness::new(120, 30).unwrap();
        harness3.type_text(line).unwrap();
        harness3
            .send_key(KeyCode::Home, KeyModifiers::NONE)
            .unwrap();

        for del_count in 1..=char_count {
            let before_len = harness3.get_buffer_content().unwrap().len();
            harness3
                .send_key(KeyCode::Delete, KeyModifiers::NONE)
                .unwrap();
            let after_content = harness3.get_buffer_content().unwrap();

            // Content should remain valid UTF-8
            assert!(
                after_content.is_char_boundary(after_content.len()),
                "Line {}: After {} deletes, content is invalid UTF-8",
                line_idx,
                del_count
            );

            // Length should have decreased
            assert!(
                after_content.len() < before_len,
                "Line {}: Delete {} didn't reduce content length",
                line_idx,
                del_count
            );
        }

        harness3.assert_buffer_content("");
        println!("  Delete: deleted {} chars one by one: ✓", char_count);

        // --- Test 8: Screen cursor visibility after various operations ---
        let mut harness4 = EditorTestHarness::new(120, 30).unwrap();
        harness4.type_text(line).unwrap();
        harness4.render().unwrap();

        // After typing, cursor should be visible on screen
        let (end_x, end_y) = harness4.screen_cursor_position();
        let (content_start, content_end) = harness4.content_area_rows();

        assert!(
            (end_y as usize) >= content_start && (end_y as usize) <= content_end,
            "Line {}: Cursor Y {} should be in content area [{}, {}]",
            line_idx,
            end_y,
            content_start,
            content_end
        );

        // Move to middle and verify cursor still visible
        harness4
            .send_key(KeyCode::Home, KeyModifiers::NONE)
            .unwrap();
        for _ in 0..(char_count / 2) {
            harness4
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
        }
        harness4.render().unwrap();

        let (mid_x, mid_y) = harness4.screen_cursor_position();
        assert!(
            (mid_y as usize) >= content_start && (mid_y as usize) <= content_end,
            "Line {}: Cursor Y {} at middle should be in content area",
            line_idx,
            mid_y
        );

        println!(
            "  Screen cursor: end=({}, {}), mid=({}, {}): ✓",
            end_x, end_y, mid_x, mid_y
        );

        println!("  Line {} PASSED all operations", line_idx);
    }

    println!(
        "\n=== All {} lines passed all operations ===",
        content_lines.len()
    );
}

// ============================================================================
// Mouse Drag Selection Tests
// Verifies that mouse drag selection only captures entire valid character sequences
// ============================================================================

/// Test mouse drag selection on double-width Chinese characters
/// Selection boundaries must be at valid UTF-8 character boundaries
#[test]
fn test_mouse_select_double_width_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // "你好世界" - four Chinese characters, each 3 bytes
    // Bytes: 你=0-2, 好=3-5, 世=6-8, 界=9-11
    // Visual: 你(0-1), 好(2-3), 世(4-5), 界(6-7)
    let text = "你好世界";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Valid byte boundaries: 0, 3, 6, 9, 12
    let valid_boundaries: Vec<usize> = vec![0, 3, 6, 9, 12];

    // Test 1: Drag across first two characters (visual cols 0-3)
    // Should select "你好" (bytes 0-6)
    harness
        .mouse_drag(gutter_x, row, gutter_x + 4, row)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have selection after drag");

    let range = harness.get_selection_range();
    assert!(range.is_some(), "Should have selection range");
    let range = range.unwrap();

    // Both start and end must be at valid byte boundaries
    assert!(
        valid_boundaries.contains(&range.start),
        "Selection start {} must be at valid character boundary {:?}",
        range.start,
        valid_boundaries
    );
    assert!(
        valid_boundaries.contains(&range.end),
        "Selection end {} must be at valid character boundary {:?}",
        range.end,
        valid_boundaries
    );

    let selected = harness.get_selected_text();
    println!(
        "Selected '{}' (bytes {}-{})",
        selected, range.start, range.end
    );

    // The selected text must be valid UTF-8 (this will panic if not)
    assert!(
        selected.is_char_boundary(0) && selected.is_char_boundary(selected.len()),
        "Selected text must be valid UTF-8"
    );
}

/// Test mouse drag selection starting in the middle of a double-width character
/// Should snap to the nearest valid character boundary
#[test]
fn test_mouse_select_snaps_to_character_boundary() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // "你好" - two Chinese characters
    let text = "你好";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Drag starting from second column of 你 (middle of character)
    // to third column of 好 (middle of second character)
    // Visual: 你(col 0-1), 好(col 2-3)
    // Start at col 1 (middle of 你), end at col 3 (middle of 好)
    harness
        .mouse_drag(gutter_x + 1, row, gutter_x + 3, row)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have selection after drag");

    let range = harness.get_selection_range();
    assert!(range.is_some(), "Should have selection range");
    let range = range.unwrap();

    // Selection must snap to valid boundaries: 0, 3, or 6
    let valid_boundaries = [0, 3, 6];
    assert!(
        valid_boundaries.contains(&range.start),
        "BUG: Selection start {} is mid-character! Must snap to {:?}",
        range.start,
        valid_boundaries
    );
    assert!(
        valid_boundaries.contains(&range.end),
        "BUG: Selection end {} is mid-character! Must snap to {:?}",
        range.end,
        valid_boundaries
    );

    let selected = harness.get_selected_text();
    println!(
        "Drag mid-char to mid-char: selected '{}' (bytes {}-{})",
        selected, range.start, range.end
    );

    // Verify selected text is valid UTF-8
    for (i, _) in selected.char_indices() {
        assert!(
            selected.is_char_boundary(i),
            "Selected text has invalid UTF-8 at byte {}",
            i
        );
    }
}

/// Test mouse selection with emoji (4-byte characters)
#[test]
fn test_mouse_select_emoji() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // "🚀🎉" - two emoji, each 4 bytes and 2 columns
    let text = "🚀🎉";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Valid byte boundaries: 0, 4, 8
    let valid_boundaries = [0, 4, 8];

    // Drag across both emoji
    harness
        .mouse_drag(gutter_x, row, gutter_x + 4, row)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have selection");

    let range = harness.get_selection_range();
    assert!(range.is_some(), "Should have range");
    let range = range.unwrap();

    // Must be at valid 4-byte boundaries
    assert!(
        valid_boundaries.contains(&range.start),
        "BUG: Selection start {} not at emoji boundary! Valid: {:?}. This indicates mid-emoji selection.",
        range.start, valid_boundaries
    );
    assert!(
        valid_boundaries.contains(&range.end),
        "BUG: Selection end {} not at emoji boundary! Valid: {:?}. This indicates mid-emoji selection.",
        range.end, valid_boundaries
    );

    let selected = harness.get_selected_text();
    println!(
        "Selected emoji: '{}' (bytes {}-{})",
        selected, range.start, range.end
    );
}

/// Test mouse selection on mixed ASCII and multi-byte content
#[test]
fn test_mouse_select_mixed_content() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // "a你b" - ASCII, Chinese (3 bytes), ASCII
    // Bytes: a=0, 你=1-3, b=4
    let text = "a你b";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Valid byte boundaries: 0, 1, 4, 5
    let valid_boundaries = [0, 1, 4, 5];

    // Drag from 'a' to 'b' (should include 你)
    // Visual: a(col 0), 你(col 1-2), b(col 3)
    harness
        .mouse_drag(gutter_x, row, gutter_x + 4, row)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have selection");

    let range = harness.get_selection_range();
    assert!(range.is_some(), "Should have range");
    let range = range.unwrap();

    assert!(
        valid_boundaries.contains(&range.start),
        "BUG: Selection start {} is mid-character! Valid boundaries: {:?}",
        range.start,
        valid_boundaries
    );
    assert!(
        valid_boundaries.contains(&range.end),
        "BUG: Selection end {} is mid-character! Valid boundaries: {:?}",
        range.end,
        valid_boundaries
    );

    let selected = harness.get_selected_text();
    println!(
        "Mixed content selection: '{}' (bytes {}-{})",
        selected, range.start, range.end
    );
}

/// Test that dragging backwards (right to left) also respects character boundaries
#[test]
fn test_mouse_select_backwards() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let text = "你好";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Drag backwards: from end of 好 to start of 你
    harness
        .mouse_drag(gutter_x + 4, row, gutter_x, row)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have selection");

    let range = harness.get_selection_range();
    assert!(range.is_some(), "Should have range");
    let range = range.unwrap();

    let valid_boundaries = [0, 3, 6];
    assert!(
        valid_boundaries.contains(&range.start),
        "Backwards selection start {} must be at valid boundary {:?}",
        range.start,
        valid_boundaries
    );
    assert!(
        valid_boundaries.contains(&range.end),
        "Backwards selection end {} must be at valid boundary {:?}",
        range.end,
        valid_boundaries
    );
}

/// Test mouse selection across a line with complex multi-byte sequences
/// This test checks that selection never creates invalid UTF-8
#[test]
fn test_mouse_select_never_creates_invalid_utf8() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Delay to avoid double-click detection between consecutive drags
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    // Complex line: ASCII + Chinese + Emoji + ASCII
    // "Hello你好🚀World"
    let text = "Hello你好🚀World";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let row = content_start as u16;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Calculate valid byte boundaries
    let valid_boundaries: Vec<usize> = text
        .char_indices()
        .map(|(i, _)| i)
        .chain(std::iter::once(text.len()))
        .collect();

    println!("Testing text: '{}' (len={})", text, text.len());
    println!("Valid byte boundaries: {:?}", valid_boundaries);

    // Test selections starting and ending at various visual columns
    // Visual layout approximation:
    // H(0) e(1) l(2) l(3) o(4) 你(5-6) 好(7-8) 🚀(9-10) W(11) o(12) r(13) l(14) d(15)
    let test_positions: Vec<u16> = (0..20).collect();

    // Track previous start_col to add delay when same position is reused
    let mut prev_start_col: Option<u16> = None;

    for start_col in &test_positions {
        for end_col in &test_positions {
            if start_col == end_col {
                continue;
            }

            // Add delay when starting from same column as previous drag to avoid double-click
            if prev_start_col == Some(*start_col) {
                harness.sleep(double_click_delay);
            }
            prev_start_col = Some(*start_col);

            harness
                .mouse_drag(gutter_x + start_col, row, gutter_x + end_col, row)
                .unwrap();
            harness.render().unwrap();

            if !harness.has_selection() {
                continue;
            }

            let range = harness.get_selection_range();
            if range.is_none() {
                continue;
            }
            let range = range.unwrap();

            if range.start == range.end {
                continue;
            }

            // CRITICAL: Both boundaries must be valid UTF-8 character boundaries
            assert!(
                valid_boundaries.contains(&range.start),
                "BUG: Drag from col {} to {}: selection start byte {} is NOT a valid character boundary! \
                 Valid: {:?}. This will create invalid UTF-8 if the selection is copied/cut.",
                start_col, end_col, range.start, valid_boundaries
            );
            assert!(
                valid_boundaries.contains(&range.end),
                "BUG: Drag from col {} to {}: selection end byte {} is NOT a valid character boundary! \
                 Valid: {:?}. This will create invalid UTF-8 if the selection is copied/cut.",
                start_col, end_col, range.end, valid_boundaries
            );

            // Extra verification: actually get the selected text (this panics on invalid UTF-8)
            let selected = harness.get_selected_text();
            assert!(
                !selected.is_empty() || range.start == range.end,
                "Non-empty range should produce non-empty text"
            );
        }
    }

    println!("All selection combinations produce valid UTF-8 boundaries");
}

/// Test multi-line mouse selection with multi-byte characters
#[test]
fn test_mouse_select_multiline_multibyte() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Two lines with multi-byte characters
    let content = "你好\n世界";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_start, _) = harness.content_area_rows();
    let start_row = content_start as u16;
    let end_row = start_row + 1;

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (gutter_x, _) = harness.screen_cursor_position();

    // Drag from first line to second line
    harness
        .mouse_drag(gutter_x, start_row, gutter_x + 2, end_row)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have multi-line selection");

    let range = harness.get_selection_range();
    assert!(range.is_some(), "Should have range");
    let range = range.unwrap();

    // Get selected text - this will panic if selection created invalid UTF-8
    let selected = harness.get_selected_text();
    println!(
        "Multi-line selection: '{:?}' (bytes {}-{})",
        selected, range.start, range.end
    );

    // Verify the selection is valid UTF-8 by iterating over chars
    for (i, c) in selected.char_indices() {
        assert!(
            selected.is_char_boundary(i),
            "Multi-line selection has invalid UTF-8 at byte {}: char '{}'",
            i,
            c
        );
    }
}
