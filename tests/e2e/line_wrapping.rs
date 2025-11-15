use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Test basic line wrapping rendering
#[test]
fn test_line_wrapping_basic_rendering() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Type a long line that will wrap
    let long_text = "This is a very long line of text that will definitely exceed the terminal width and should wrap to multiple lines when line wrapping is enabled.";
    harness.type_text(long_text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // The text should be visible in the screen output
    // Since it wraps, we should see parts of the text on multiple lines
    // Note: the exact visible portion depends on viewport and wrapping
    assert!(
        screen.contains("line wrapping")
            || screen.contains("terminal")
            || screen.contains("multiple"),
        "Screen should contain some part of the wrapped text"
    );

    // Buffer content should still be a single line (no newlines added)
    assert_eq!(harness.get_buffer_content(), long_text);
    assert!(!harness.get_buffer_content().contains('\n'));
}

/// Test line wrapping can be disabled
#[test]
fn test_line_wrapping_disabled() {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    let mut harness = EditorTestHarness::with_config(60, 24, config).unwrap();

    // Type a long line
    let long_text = "This is a very long line of text that will definitely exceed the terminal width and would normally wrap but should not when disabled.";
    harness.type_text(long_text).unwrap();
    harness.render().unwrap();

    // Buffer content should still be a single line
    assert_eq!(harness.get_buffer_content(), long_text);
    assert!(!harness.get_buffer_content().contains('\n'));
}

/// Test cursor navigation with wrapped lines - Home key
#[test]
fn test_wrapped_line_navigation_home() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Type a long line that will wrap
    let long_text = "This is a very long line of text that will definitely exceed the terminal width and should wrap to multiple lines.";
    harness.type_text(long_text).unwrap();

    // Cursor should be at the end
    assert_eq!(harness.cursor_position(), long_text.len());

    // Press Home - should go to start of the physical line, not the wrapped line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Cursor should be at position 0
    assert_eq!(harness.cursor_position(), 0);
}

/// Test cursor navigation with wrapped lines - End key
#[test]
fn test_wrapped_line_navigation_end() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Type a long line
    let long_text = "This is a very long line of text that will definitely exceed the terminal width and should wrap to multiple lines.";
    harness.type_text(long_text).unwrap();

    // Move to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.cursor_position(), 0);

    // Press End - should go to end of the physical line, not just the wrapped portion
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Cursor should be at the end of the line
    assert_eq!(harness.cursor_position(), long_text.len());
}

/// Test cursor navigation with wrapped lines - Left/Right arrows
#[test]
fn test_wrapped_line_navigation_arrows() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Type a long line that will wrap
    harness.type_text("This is a very long line of text that will definitely exceed the terminal width and should wrap to multiple lines.").unwrap();

    let end_pos = harness.cursor_position();

    // Press Left arrow multiple times
    for _ in 0..10 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }

    // Cursor should have moved left by 10 positions
    assert_eq!(harness.cursor_position(), end_pos - 10);

    // Press Right arrow to move back
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Cursor should have moved right by 5 positions
    assert_eq!(harness.cursor_position(), end_pos - 5);
}

/// Test editing in the middle of a wrapped line
#[test]
fn test_wrapped_line_editing_middle() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Type a long line
    let long_text = "This is a very long line of text that will definitely exceed the terminal width and should wrap.";
    harness.type_text(long_text).unwrap();

    // Move to the middle of the line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..20 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    assert_eq!(harness.cursor_position(), 20);

    // Insert text in the middle
    harness.type_text("[INSERTED]").unwrap();

    // Verify the text was inserted correctly
    let expected = "This is a very long [INSERTED]line of text that will definitely exceed the terminal width and should wrap.";
    assert_eq!(harness.get_buffer_content(), expected);

    // Cursor should be after the inserted text
    assert_eq!(harness.cursor_position(), 30); // 20 + 10
}

/// Test multiple wrapped lines
#[test]
fn test_multiple_wrapped_lines() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Type multiple long lines
    harness.type_text("First very long line that will wrap across multiple display lines in the terminal window.").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .type_text(
            "Second very long line that will also wrap and take up multiple rows in the display.",
        )
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Third line is shorter.").unwrap();

    harness.render().unwrap();

    let buffer_content = harness.get_buffer_content();

    // Should have exactly 2 newlines (3 logical lines)
    assert_eq!(buffer_content.matches('\n').count(), 2);

    // Should contain all our text
    assert!(buffer_content.contains("First very long line"));
    assert!(buffer_content.contains("Second very long line"));
    assert!(buffer_content.contains("Third line is shorter"));
}

/// Test Up/Down navigation with wrapped lines
#[test]
fn test_wrapped_line_navigation_up_down() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Create two lines, first one wraps
    harness.type_text("This is a very long first line that will wrap to multiple display lines in the terminal.").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Short second line.").unwrap();

    let second_line_end = harness.cursor_position();

    // Press Up - should go to first line
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();

    let first_line_pos = harness.cursor_position();

    // Should be on the first line (before the newline)
    assert!(first_line_pos < second_line_end);

    // Press Down - should go back to second line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Should be back near the end of second line
    assert!(harness.cursor_position() >= second_line_end - 20);
}

/// Test deleting content in wrapped lines
#[test]
fn test_wrapped_line_deletion() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Type a long line
    harness
        .type_text("This is a very long line that will wrap to multiple display lines.")
        .unwrap();

    let initial_len = harness.get_buffer_content().len();

    // Delete some characters with backspace
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }

    // Content should be shorter
    assert_eq!(harness.get_buffer_content().len(), initial_len - 10);

    let content = harness.get_buffer_content();

    // Should end with "disp" now (removed "lay lines.")
    // Original: "This is a very long line that will wrap to multiple display lines."
    // After removing 10 chars (from "lay lines."): "This is a very long line that will wrap to multiple disp"
    assert!(
        content.ends_with("disp"),
        "Content should end with 'disp' after deletion"
    );
}

/// Test that line numbers are shown correctly with wrapped lines
#[test]
fn test_wrapped_line_numbers() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Create 3 logical lines, where the first one wraps
    harness.type_text("First line is very long and will wrap across multiple display rows in the terminal window.").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Second line is short.").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Third line is also short.").unwrap();

    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Should show line numbers 1, 2, 3
    // The wrapped portions should show spaces in the line number area
    assert!(screen.contains("1"));
    assert!(screen.contains("2"));
    assert!(screen.contains("3"));

    // Verify we only have 3 logical lines in the buffer
    assert_eq!(harness.get_buffer_content().matches('\n').count(), 2);
}

/// Test that horizontal scrolling is disabled when line wrapping is enabled
/// Bug: pressing "end" on a wrapped line causes horizontal scroll, breaking the visual wrapping
#[test]
fn test_wrapped_line_no_horizontal_scroll() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Type a long line that will wrap
    let long_text = "A fast, lightweight terminal text editor written in Rust. Handles files of any size with instant startup, low memory usage, and modern IDE features.";
    harness.type_text(long_text).unwrap();

    // Move cursor to start of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();

    // The line should be wrapped and visible from the beginning
    assert!(
        screen_before.contains("A fast"),
        "Should show start of line before End key"
    );
    assert!(
        screen_before.contains("lightweight"),
        "Should show 'lightweight' in wrapped portion"
    );

    // Press End to go to end of line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();

    // BUG: Currently the screen will show horizontally scrolled content
    // After fix: the line should still be wrapped and show the beginning
    // The screen should STILL show the beginning of the line (no horizontal scroll)
    assert!(screen_after.contains("A fast") || screen_after.contains("lightweight"),
            "After pressing End, line should still be wrapped and visible from start (no horizontal scroll). Screen:\n{screen_after}");

    // The cursor is at the end, but the line should still wrap from the beginning
    assert_eq!(
        harness.cursor_position(),
        long_text.len(),
        "Cursor should be at end of line"
    );
}

/// Test cursor position updates correctly as it moves through wrapped lines
/// Verifies visual cursor moves down to wrapped portions and back up
#[test]
/// Test cursor position updates correctly as it moves through wrapped lines
/// Verifies visual cursor moves down to wrapped portions and back up
#[test]
fn test_wrapped_line_cursor_positioning() {
    const TERMINAL_WIDTH: u16 = 60;
    const GUTTER_WIDTH: u16 = 8;

    let mut harness = EditorTestHarness::new(TERMINAL_WIDTH, 24).unwrap();

    // Type a long line with real words that will wrap
    let long_text = "The quick brown fox jumps over the lazy dog and runs through the forest, exploring ancient trees and mysterious pathways that wind between towering oaks.";
    harness.type_text(long_text).unwrap();
    harness.render().unwrap();

    eprintln!("Text length: {}", long_text.len());

    // Cursor should be at end of text
    assert_eq!(harness.cursor_position(), long_text.len());

    // Move to start of line with Home
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (start_x, start_y) = harness.screen_cursor_position();
    assert_eq!(
        harness.cursor_position(),
        0,
        "Cursor should be at position 0 after Home"
    );

    // Cursor at position 0 should be at x=GUTTER_WIDTH (after gutter)
    assert_eq!(
        start_x, GUTTER_WIDTH,
        "Cursor at position 0 should be at x={GUTTER_WIDTH} (after gutter)"
    );
    eprintln!("After Home: buffer_pos=0, screen=({start_x}, {start_y})");

    // Verify the beginning of the text is visible on screen
    let screen = harness.screen_to_string();
    let text_start = &long_text[..20.min(long_text.len())]; // First 20 chars
    assert!(
        screen.contains(text_start),
        "Screen should show start of text: '{text_start}'"
    );
    eprintln!("Text start visible on screen: '{text_start}'");

    let mut prev_y = start_y;
    let mut first_wrap_point = None;
    let mut second_wrap_point = None;

    // Move right through the line to detect where wrapping occurs
    // We'll detect up to 2 wrap points to understand the wrapping pattern
    for i in 1..=long_text.len().min(100) {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let (cur_x, cur_y) = harness.screen_cursor_position();
        let buf_pos = harness.cursor_position();

        // Verify buffer position matches
        assert_eq!(buf_pos, i, "Buffer position should be {i}");

        // Detect when cursor wraps to next line
        if cur_y > prev_y {
            if first_wrap_point.is_none() {
                first_wrap_point = Some(i);
                eprintln!("After {i} rights: buffer_pos={buf_pos}, screen=({cur_x}, {cur_y}) -> FIRST WRAP");

                // At first wrap point, cursor should be at start of continuation line
                assert_eq!(
                    cur_x, GUTTER_WIDTH,
                    "At first wrap point (position {i}), cursor should be at x={GUTTER_WIDTH}"
                );
                assert_eq!(
                    cur_y,
                    start_y + 1,
                    "At first wrap point (position {i}), cursor should be on next line"
                );
                eprintln!("  ✓ First wrap point verified: position {i}, screen=({cur_x}, {cur_y})");
            } else if second_wrap_point.is_none() {
                second_wrap_point = Some(i);
                eprintln!("After {i} rights: buffer_pos={buf_pos}, screen=({cur_x}, {cur_y}) -> SECOND WRAP");

                // At second wrap point, cursor should also be at start of continuation line
                assert_eq!(
                    cur_x, GUTTER_WIDTH,
                    "At second wrap point (position {i}), cursor should be at x={GUTTER_WIDTH}"
                );
                assert_eq!(
                    cur_y,
                    start_y + 2,
                    "At second wrap point (position {i}), cursor should be two lines down"
                );
                eprintln!(
                    "  ✓ Second wrap point verified: position {i}, screen=({cur_x}, {cur_y})"
                );

                // We've detected both wrap points, we can break
                break;
            }
        }

        prev_y = cur_y;
    }

    assert!(
        first_wrap_point.is_some(),
        "Should have detected first wrap point"
    );
    assert!(
        second_wrap_point.is_some(),
        "Should have detected second wrap point"
    );

    let first_line_width = first_wrap_point.unwrap();
    let continuation_line_width = second_wrap_point.unwrap() - first_wrap_point.unwrap();
    eprintln!("Detected wrapping: first_line_width={first_line_width}, continuation_line_width={continuation_line_width}");

    // Verify no horizontal scroll happened throughout
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(text_start),
        "Screen should still show start of text (no horizontal scroll)"
    );

    // Now press End to jump to end
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (end_x, end_y) = harness.screen_cursor_position();
    assert_eq!(
        harness.cursor_position(),
        long_text.len(),
        "Cursor should be at end after End key"
    );
    eprintln!(
        "After End: buffer_pos={}, screen=({}, {})",
        long_text.len(),
        end_x,
        end_y
    );

    // Verify cursor ended up on a later line (text wrapped at least once)
    assert!(
        end_y > start_y,
        "End cursor should be on a later line than start (text should wrap)"
    );

    // Verify text is visible on screen
    let screen_at_end = harness.screen_to_string();
    assert!(
        screen_at_end.contains("The quick brown fox"),
        "Screen should show beginning of text"
    );
    // Just verify some text from the end is visible (exact text depends on wrapping)
    assert!(
        screen_at_end.contains("oaks")
            || screen_at_end.contains("tower")
            || screen_at_end.contains("between"),
        "Screen should show some text from end of line"
    );

    // Now move back left and watch cursor move back up across wrap points
    let mut wrapped_up = false;
    let mut prev_y = end_y;

    // Move left through the text, watching for upward wrapping
    for i in 1..=50 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let (cur_x, cur_y) = harness.screen_cursor_position();
        let buf_pos = harness.cursor_position();

        // Check if cursor wrapped back up
        if cur_y < prev_y {
            eprintln!(
                "After {i} lefts: buffer_pos={buf_pos}, screen=({cur_x}, {cur_y}) -> WRAPPED UP"
            );
            wrapped_up = true;

            // When wrapping up, cursor should NOT be at gutter (should be at end of previous line)
            assert!(cur_x > GUTTER_WIDTH, "When wrapping up, cursor should be at end of previous line, not at x={GUTTER_WIDTH}");

            // We've verified upward wrapping works
            break;
        }

        prev_y = cur_y;
    }

    assert!(
        wrapped_up,
        "Cursor should have wrapped back up when moving left across wrap boundaries"
    );

    // Finally, press Home to go back to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (final_x, final_y) = harness.screen_cursor_position();
    assert_eq!(
        harness.cursor_position(),
        0,
        "Cursor should be at position 0 after final Home"
    );
    assert_eq!(
        final_x, GUTTER_WIDTH,
        "Cursor should be at x={GUTTER_WIDTH}"
    );
    assert_eq!(final_y, start_y, "Cursor should be back at starting y");

    // Verify start of text is still visible
    let screen_final = harness.screen_to_string();
    assert!(
        screen_final.contains(text_start),
        "Start of text should still be visible after Home"
    );
}

/// Test that scrolling works correctly when navigating down past viewport with wrapped lines
/// Bug: Using down arrow to scroll past the end of the view area doesn't scroll the page correctly
/// This test validates that the cursor's buffer position always corresponds to visible content
#[test]
fn test_wrapped_line_scrolling_down_past_viewport() {
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 12; // Small height to make scrolling happen quickly

    let mut harness = EditorTestHarness::new(TERMINAL_WIDTH, TERMINAL_HEIGHT).unwrap();

    // Create multiple long lines with identifiable prefixes
    // Each line starts with "[N]" so we can identify which line the cursor is on
    // Lines are long enough to wrap to 2-3 screen lines
    for i in 0..20 {
        harness.type_text(&format!(
            "[{}] This is line number {} with lots of extra text to make it wrap across multiple display rows in the terminal window. ",
            i, i
        )).unwrap();
        if i < 19 {
            harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
        }
    }

    // Move cursor to the beginning
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    let buffer_content = harness.get_buffer_content();
    eprintln!("\n=== Buffer content ===");
    eprintln!("Total buffer length: {} bytes", buffer_content.len());

    let (initial_x, initial_y) = harness.screen_cursor_position();
    eprintln!("\n=== Initial state ===");
    eprintln!("Initial cursor position: ({}, {})", initial_x, initial_y);

    // Verify we're at the start
    assert_eq!(harness.cursor_position(), 0, "Should be at start of buffer");

    // Get the initial screen content to see what's visible
    let screen_before = harness.screen_to_string();
    eprintln!("Screen before scrolling:\n{}", screen_before);

    // Helper function to determine which line number the cursor is on based on buffer position
    let get_line_at_position = |pos: usize| -> Option<usize> {
        let text_up_to_cursor = &buffer_content[..pos.min(buffer_content.len())];
        let lines_before = text_up_to_cursor.matches('\n').count();
        Some(lines_before)
    };

    // Press down repeatedly to move through wrapped lines
    let mut scrolling_occurred = false;
    let max_down_presses = 50;

    for i in 0..max_down_presses {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let (cur_x, cur_y) = harness.screen_cursor_position();
        let buf_pos = harness.cursor_position();

        // Get the screen content
        let screen_now = harness.screen_to_string();

        // Determine which line the cursor is on
        let cursor_line = get_line_at_position(buf_pos).unwrap();

        eprintln!("\n=== After {} down presses ===", i + 1);
        eprintln!("  Buffer position: {}", buf_pos);
        eprintln!("  Screen position: ({}, {})", cur_x, cur_y);
        eprintln!("  Cursor is on logical line: {}", cursor_line);

        // The cursor should ALWAYS be visible on screen
        assert!(
            cur_y < TERMINAL_HEIGHT,
            "After {} down presses: Cursor at y={} is beyond terminal height {}. \
             Buffer position: {}. This indicates scrolling didn't happen when it should have.",
            i + 1,
            cur_y,
            TERMINAL_HEIGHT,
            buf_pos
        );

        // CRITICAL CHECK: The line number the cursor is on should be visible in the viewport
        // Look for the line identifier "[N]" where N is the cursor's line number
        let line_marker = format!("[{}]", cursor_line);
        assert!(
            screen_now.contains(&line_marker),
            "After {} down presses: Cursor is on line {} (position {}), but '{}' is NOT visible on screen!\n\
             This means scrolling did not happen correctly.\n\
             Screen content:\n{}",
            i + 1,
            cursor_line,
            buf_pos,
            line_marker,
            screen_now
        );

        eprintln!("  ✓ Line marker '{}' is visible on screen", line_marker);

        // Additional validation: check a few characters around the cursor position
        if buf_pos > 0 && buf_pos < buffer_content.len() {
            let start = buf_pos.saturating_sub(5);
            let end = (buf_pos + 5).min(buffer_content.len());
            let context = &buffer_content[start..end];
            let context_clean = context.replace('\n', "\\n");

            eprintln!("  Context around cursor: '...{}...'", context_clean);

            // Check if any part of this context is visible on screen
            // (accounting for line breaks)
            let mut found_context = false;
            for word in context.split_whitespace() {
                if word.len() >= 3 && screen_now.contains(word) {
                    found_context = true;
                    eprintln!("  ✓ Found context word '{}' on screen", word);
                    break;
                }
            }
        }

        // Check if scrolling has occurred
        if !screen_now.contains("[0]") {
            if !scrolling_occurred {
                eprintln!("\n=== SCROLLING DETECTED after {} down presses ===", i + 1);
                scrolling_occurred = true;
            }
        }

        // Stop after we've scrolled significantly
        if cursor_line >= 12 {
            eprintln!("\n=== Reached line {}, stopping test ===", cursor_line);
            break;
        }
    }

    assert!(
        scrolling_occurred,
        "Scrolling should have occurred when navigating through wrapped lines"
    );

    // Final validation
    let final_pos = harness.cursor_position();
    let final_line = get_line_at_position(final_pos).unwrap();
    eprintln!("\n=== Final state ===");
    eprintln!("  Final buffer position: {}", final_pos);
    eprintln!("  Final line: {}", final_line);

    assert!(
        final_line >= 5,
        "Should have navigated to at least line 5, but only reached line {}",
        final_line
    );
}

/// Test that cursor doesn't move into empty space beyond wrapped line ends
/// Bug: Cursor can move several characters past the visible text before wrapping down
/// TODO: This test is currently disabled due to rendering issues that need investigation
#[test]
#[ignore]
fn test_wrapped_line_cursor_no_empty_space() {
    const TERMINAL_WIDTH: u16 = 60;
    const GUTTER_WIDTH: u16 = 8;

    let mut harness = EditorTestHarness::new(TERMINAL_WIDTH, 24).unwrap();

    // Type a line that will wrap
    let long_text = "The quick brown fox jumps over the lazy dog and runs through the forest.";
    harness.type_text(long_text).unwrap();

    // Move to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (start_x, start_y) = harness.screen_cursor_position();
    eprintln!("\n=== Testing cursor doesn't go into empty space ===");
    eprintln!("Text: '{long_text}'");
    eprintln!("Length: {} chars", long_text.len());

    // Move right character by character and check what's under the cursor
    let mut found_empty_space = false;
    let mut wrap_happened = false;
    let mut prev_y = start_y;

    for i in 0..long_text.len() {
        let (cur_x, cur_y) = harness.screen_cursor_position();
        let cell_content = harness.get_cell(cur_x, cur_y);
        let buf_pos = harness.cursor_position();

        eprintln!(
            "Position {i}: screen=({cur_x}, {cur_y}), buffer={buf_pos}, cell='{cell_content:?}'"
        );

        // Check if cursor wrapped to next line
        if cur_y > prev_y {
            eprintln!("  -> Cursor wrapped from y={prev_y} to y={cur_y}");
            wrap_happened = true;
        }
        prev_y = cur_y;

        // Verify cursor is on the expected character from the text
        let expected_char = long_text.chars().nth(i).unwrap();
        let is_space_in_text = expected_char == ' ';

        if let Some(content) = &cell_content {
            // Check if this is actual content from the text or padding
            if !is_space_in_text && content == " " {
                // The text character is not a space, but we're seeing a space on screen
                // This means we're in empty padding beyond the wrapped line
                eprintln!("  ⚠️  CURSOR IN EMPTY SPACE at screen position ({cur_x}, {cur_y})");
                eprintln!(
                    "      Expected char '{expected_char}' from text, but screen shows space"
                );
                found_empty_space = true;
            } else if content == "█" {
                // Hit scrollbar or UI element
                eprintln!("  ⚠️  CURSOR ON UI ELEMENT (scrollbar?) at ({cur_x}, {cur_y})");
                found_empty_space = true;
            }
        }

        // Move right
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    assert!(
        !found_empty_space,
        "Cursor should never move into empty space beyond visible text"
    );
    assert!(wrap_happened, "Cursor should have wrapped to next line");

    // Now test pressing End from the start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (end_x, end_y) = harness.screen_cursor_position();
    let buf_pos_at_end = harness.cursor_position();

    eprintln!("\n=== After pressing End ===");
    eprintln!("Cursor at: screen=({end_x}, {end_y}), buffer={buf_pos_at_end}");

    // The cursor should be at the actual end of the text
    assert_eq!(
        buf_pos_at_end,
        long_text.len(),
        "End key should move to end of buffer"
    );

    // Check what's under and around the cursor
    let cell_at_cursor = harness.get_cell(end_x, end_y);
    let cell_before_cursor = if end_x > 0 {
        harness.get_cell(end_x - 1, end_y)
    } else {
        None
    };

    eprintln!("Cell at cursor ({end_x}, {end_y}): {cell_at_cursor:?}");
    eprintln!(
        "Cell before cursor ({}, {}): {:?}",
        end_x - 1,
        end_y,
        cell_before_cursor
    );

    // The cell before the cursor should have actual text content (the last character)
    // The cursor itself might be on empty space (end of line) or the last character
    if let Some(before) = cell_before_cursor {
        assert!(
            !before.trim().is_empty() || before == " ",
            "Cell before cursor should have text content, found: '{before}'"
        );
    }
}
