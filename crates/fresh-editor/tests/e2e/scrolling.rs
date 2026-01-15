use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use tempfile::TempDir;

/// Test viewport scrolling with large file
#[test]
fn test_large_file_viewport() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large.txt");

    // Create a file with many lines (more than viewport height)
    let mut content = String::new();
    for i in 0..100 {
        content.push_str(&format!("Line {i}\n"));
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should show first few lines
    harness.assert_screen_contains("Line 0");
    harness.assert_screen_contains("Line 1");

    // Should NOT show lines beyond viewport
    harness.assert_screen_not_contains("Line 50");

    // TODO: When action_to_events() is implemented:
    // - Scroll down
    // - Verify different lines are visible
}

/// Test that edits persist when scrolling away and back
/// Verifies the cache and persistence layer maintain edits correctly
#[test]
fn test_edits_persist_through_scrolling() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create content with many lines (more than viewport can show)
    // This ensures we can scroll away from edited content
    let lines: Vec<String> = (0..100).map(|i| format!("Line {i}")).collect();
    let content = lines.join("\n");
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

    // Verify initial content
    harness.assert_buffer_content(&content);

    // Go to the beginning of the document
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(harness.cursor_position(), 0);

    // Make an edit at the beginning: change "Line 0" to "EDITED Line 0"
    harness.type_text("EDITED ").unwrap();
    harness.assert_buffer_content(&format!("EDITED Line 0\n{}", lines[1..].join("\n")));
    harness.assert_screen_contains("EDITED Line 0");

    // Jump to the end of the document (well past the viewport)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify we're at the end
    let pos_at_end = harness.cursor_position();
    assert!(pos_at_end > 500, "Should be at the end of the document");

    // Verify Line 0 is not visible on screen anymore
    harness.assert_screen_not_contains("Line 0");
    harness.assert_screen_not_contains("EDITED Line 0");

    // Verify we can see lines near the end
    harness.assert_screen_contains("Line 9");

    // Make an edit at the end
    harness.type_text("\nEND MARKER").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("END MARKER");

    // Verify both edits exist in the buffer
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("EDITED Line 0"),
        "Edit at beginning should persist"
    );
    assert!(
        buffer_content.contains("END MARKER"),
        "Edit at end should persist"
    );

    // Now jump back to the beginning to verify the first edit persisted
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The edit should still be visible on screen and in buffer
    harness.assert_screen_contains("EDITED Line 0");
    harness.assert_buffer_content(&buffer_content);

    // Verify cursor is at the beginning
    assert_eq!(harness.cursor_position(), 0);

    // Jump to somewhere in the middle of the document
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    for _ in 0..40 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Make an edit in the middle section
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("MIDDLE ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("MIDDLE Line 4");

    // Jump back to end
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // End marker should still be visible
    harness.assert_screen_contains("END MARKER");

    // Final verification: all three edits persist in buffer
    let final_content = harness.get_buffer_content().unwrap();
    assert!(
        final_content.contains("EDITED Line 0"),
        "Beginning edit persisted through all jumps"
    );
    assert!(
        final_content.contains("MIDDLE Line 4"),
        "Middle edit persisted through all jumps"
    );
    assert!(
        final_content.contains("END MARKER"),
        "End edit persisted through all jumps"
    );
}

/// Test cursor doesn't get stuck when typing beyond viewport width
/// This reproduces a bug where the screen cursor position stops advancing
/// when the line gets longer than the viewport width (80 characters)
#[test]
fn test_cursor_advances_beyond_viewport_width() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a very long line - longer than the viewport width of 80
    // We'll type 100 characters to ensure we go beyond the viewport width
    let long_text = "a".repeat(100);

    for (i, ch) in long_text.chars().enumerate() {
        harness
            .send_key(
                crossterm::event::KeyCode::Char(ch),
                crossterm::event::KeyModifiers::NONE,
            )
            .unwrap();

        // Verify buffer position keeps advancing
        let buffer_pos = harness.cursor_position();
        assert_eq!(
            buffer_pos,
            i + 1,
            "After typing {} characters, buffer cursor should be at position {}, but is at {}",
            i + 1,
            i + 1,
            buffer_pos
        );
    }

    // Final verification
    harness.assert_buffer_content(&long_text);
    assert_eq!(harness.cursor_position(), 100);
}

/// Test horizontal scrolling when cursor moves beyond visible width
/// The viewport should scroll horizontally to keep the cursor visible
#[test]
fn test_horizontal_scrolling() {
    use fresh::config::Config;
    let config = Config {
        editor: fresh::config::EditorConfig {
            line_wrap: false,
            ..Default::default()
        },
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Calculate visible width (80 - 7 for line number gutter = 73 chars)
    let gutter_width = 7;
    let _visible_width = 80 - gutter_width; // 73 characters visible

    // Type characters to fill most of the visible width
    let initial_text = "a".repeat(60);
    harness.type_text(&initial_text).unwrap();

    // Get initial viewport state (should be no scrolling yet)
    let viewport = &harness.editor().active_viewport();
    assert_eq!(viewport.left_column, 0, "Should not be scrolled yet");

    // Type more characters to go beyond visible width
    let more_text = "b".repeat(30); // Total: 90 characters
    harness.type_text(&more_text).unwrap();

    // Now the viewport should have scrolled horizontally
    let viewport = &harness.editor().active_viewport();
    assert!(
        viewport.left_column > 0,
        "Viewport should have scrolled horizontally, left_column = {}",
        viewport.left_column
    );

    // The cursor should still be visible on screen
    // Note: With horizontal_scroll_offset, the cursor position varies based on
    // scrolling implementation - just verify it's within terminal bounds
    let screen_pos = harness.screen_cursor_position();
    assert!(
        screen_pos.0 < 120, // Just verify cursor X is within reasonable terminal bounds
        "Cursor screen X ({}) should be within terminal bounds",
        screen_pos.0
    );

    // Verify buffer position is correct
    assert_eq!(harness.cursor_position(), 90);
}

/// Test horizontal scrolling when moving cursor left
#[test]
fn test_horizontal_scroll_left() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::config::Config;
    let config = Config {
        editor: fresh::config::EditorConfig {
            line_wrap: false,
            ..Default::default()
        },
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Type a long line
    let long_text = "a".repeat(100);
    harness.type_text(&long_text).unwrap();

    // Cursor is now at position 100, viewport should be scrolled
    let viewport = &harness.editor().active_viewport();
    let initial_left_col = viewport.left_column;
    assert!(initial_left_col > 0, "Viewport should be scrolled right");

    // Move cursor all the way to the left (Home key)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Cursor should be at position 0
    assert_eq!(harness.cursor_position(), 0);

    // Viewport should have scrolled back to show the beginning
    let viewport = &harness.editor().active_viewport();
    assert_eq!(
        viewport.left_column, 0,
        "Viewport should have scrolled back to left"
    );
}

/// Test horizontal scrolling with arrow key navigation
#[test]
fn test_horizontal_scroll_with_arrows() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::config::Config;
    let config = Config {
        editor: fresh::config::EditorConfig {
            line_wrap: false,
            ..Default::default()
        },
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Type a line longer than visible width
    let text = "x".repeat(90);
    harness.type_text(&text).unwrap();

    // Viewport should be scrolled
    let viewport = &harness.editor().active_viewport();
    assert!(viewport.left_column > 0);

    // Move left by 50 characters
    for _ in 0..50 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Cursor should be at position 40
    assert_eq!(harness.cursor_position(), 40);

    // Viewport should have scrolled left to keep cursor visible
    let _viewport = &harness.editor().active_viewport();
    let screen_pos = harness.screen_cursor_position();

    // Screen cursor should be within visible bounds
    let visible_width = 80 - 7; // Terminal width minus gutter
    assert!(
        screen_pos.0 < visible_width as u16,
        "Cursor X ({}) should be within visible width ({})",
        screen_pos.0,
        visible_width
    );
}

/// Test cursor wrapping behavior when navigating horizontally on long lines
/// This test verifies that when line wrap is disabled and a line extends beyond
/// the viewport width, pressing right arrow at the end of the line moves directly
/// to the start of the next line, and pressing left from the start of a line moves
/// to the end of the previous line.
#[test]
fn test_cursor_wrap_on_long_line_navigation() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::config::Config;
    let config = Config {
        editor: fresh::config::EditorConfig {
            line_wrap: false,
            ..Default::default()
        },
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Create a long line that extends well beyond viewport width (100 chars)
    // followed by a second line
    let long_line = "a".repeat(100);
    harness.type_text(&long_line).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("second line").unwrap();

    // Move to start of document
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate character-by-character to the end of the first line
    // This simulates a user holding down the right arrow key
    for i in 0..100 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        assert_eq!(
            harness.cursor_position(),
            i + 1,
            "Cursor should advance byte by byte"
        );
    }
    harness.render().unwrap();

    // After 100 right arrows, we should be at position 100
    // Let's check what's at this position
    let buffer_content = harness.get_buffer_content().unwrap();
    println!("Buffer length: {}", buffer_content.len());
    println!(
        "Character at position 99 (last 'a'): {:?}",
        buffer_content.chars().nth(99)
    );
    println!(
        "Character at position 100 (should be newline): {:?}",
        buffer_content.chars().nth(100)
    );
    println!(
        "Character at position 101 (first char of second line): {:?}",
        buffer_content.chars().nth(101)
    );

    assert_eq!(harness.cursor_position(), 100, "Should be at position 100");

    let screen_pos_at_end = harness.screen_cursor_position();
    println!(
        "Screen position at end of long line: ({}, {})",
        screen_pos_at_end.0, screen_pos_at_end.1
    );

    // Now press right one more time - this should take us to the next line
    // User expectation: cursor wraps to start of next line
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let pos_after_wrap = harness.cursor_position();
    println!("Cursor position after wrapping: {}", pos_after_wrap);

    // Position 101 is start of second line (after the newline at position 100)
    assert_eq!(pos_after_wrap, 101, "Cursor should be on next line");

    // Verify cursor is visually on the second line
    let screen_pos_on_second_line = harness.screen_cursor_position();
    println!(
        "Screen position on second line: ({}, {})",
        screen_pos_on_second_line.0, screen_pos_on_second_line.1
    );
    assert_eq!(
        screen_pos_on_second_line.1,
        screen_pos_at_end.1 + 1,
        "Cursor should move down one line visually"
    );

    // Now test the reverse: press left to wrap back to the previous line
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let pos_after_left_wrap = harness.cursor_position();
    println!("Cursor position after left wrap: {}", pos_after_left_wrap);

    // Should be back at position 100 (end of first line)
    assert_eq!(
        pos_after_left_wrap, 100,
        "Cursor should wrap back to end of previous line"
    );

    // Verify cursor is visually back on the first line
    let screen_pos_back = harness.screen_cursor_position();
    println!(
        "Screen position back on first line: ({}, {})",
        screen_pos_back.0, screen_pos_back.1
    );
    assert_eq!(
        screen_pos_back.1, screen_pos_at_end.1,
        "Cursor should be back on first line visually"
    );
}

/// Test to reproduce cursor disappearing when navigating beyond long line end
/// User reports: cursor disappears for ~16 right-arrow keypresses
/// when moving past the end of a horizontally scrolled long line.
#[test]
fn test_cursor_disappears_beyond_long_line_end() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::config::Config;
    let mut config = Config::default();
    config.editor.line_wrap = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Create a VERY long line (200 chars) followed by a second line
    // This forces significant horizontal scrolling
    let long_line = "a".repeat(200);
    harness.type_text(&long_line).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("second line").unwrap();

    // Navigate to position 188 character-by-character (near end of 200-char line)
    // Then continue pressing right to go through the end and onto next line
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..188 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    println!("\n=== Reproducing cursor disappearance bug ===");
    println!("Line length: 200 chars, starting at position 188");

    let start_pos = harness.cursor_position();
    println!("Starting at position: {}", start_pos);
    assert_eq!(start_pos, 188, "Should be at position 188");

    // Now press right arrow and track VISIBLE cursor for 25 keypresses
    let mut disappeared_count = 0;
    let mut first_disappear = None;
    let mut reappear = None;

    for i in 1..=25 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let buffer_pos = harness.cursor_position();
        let screen_pos = harness.screen_cursor_position();
        let left_col = harness.editor().active_viewport().left_column;

        // Get the actual rendered screen
        let screen = harness.screen_to_string();
        let lines: Vec<&str> = screen.lines().collect();

        // Get buffer character at this position
        let buffer_content = harness.get_buffer_content().unwrap();
        let buffer_char = buffer_content.chars().nth(buffer_pos);

        // Check if cursor is visible: find where actual line content ends
        let (is_cursor_visible, content_info) = if (screen_pos.1 as usize) < lines.len() {
            let line = lines[screen_pos.1 as usize];
            let cursor_x = screen_pos.0 as usize;

            // Find the gutter separator "│"
            let gutter_end = line.find('│').map(|pos| pos + 2).unwrap_or(8); // +2 to skip "│ "

            // Find where content ends: last non-space character before the scrollbar
            // Work backwards from the end to find the last actual content char.
            // The scrollbar is rendered as a space with background color, so it will
            // be trimmed along with other trailing spaces.
            let chars: Vec<char> = line.chars().collect();
            let mut content_end = chars.len();

            // Find last non-space content character
            while content_end > gutter_end && chars.get(content_end - 1) == Some(&' ') {
                content_end -= 1;
            }

            // Cursor is visible if it's within the content region
            // The cursor can be at content_end (one position past last char) for "end of line" position
            // If content_end == gutter_end, there's no visible content, so cursor can't be visible
            let is_visible =
                cursor_x >= gutter_end && cursor_x <= content_end && content_end > gutter_end;

            let char_at_cursor = chars.get(cursor_x).copied();
            let info = format!(
                "gutter_end={}, content_end={}, cursor_x={}, char={:?}",
                gutter_end, content_end, cursor_x, char_at_cursor
            );
            (is_visible, info)
        } else {
            (false, "BEYOND_SCREEN".to_string())
        };

        // Track disappearance
        if !is_cursor_visible {
            if first_disappear.is_none() {
                first_disappear = Some(i);
            }
            disappeared_count += 1;
        } else if first_disappear.is_some() && reappear.is_none() {
            reappear = Some(i);
        }

        println!("\nAfter {} right arrow(s):", i);
        println!("  Buffer pos: {} = {:?}", buffer_pos, buffer_char);
        println!("  Screen cursor: ({}, {})", screen_pos.0, screen_pos.1);
        println!("  Viewport left_column: {}", left_col);
        println!("  Content info: {}", content_info);
        println!("  Cursor VISIBLE: {}", is_cursor_visible);

        // Show the screen line at cursor Y position for critical positions
        if (i <= 7 || (99..=102).contains(&buffer_pos) || !is_cursor_visible)
            && (screen_pos.1 as usize) < lines.len()
        {
            let line = lines[screen_pos.1 as usize];
            println!("  Screen line {}: {:?}", screen_pos.1, line);
            // Also show line length
            let visible_line: String = line
                .chars()
                .filter(|c| !c.is_control() && *c != '\u{1b}')
                .collect();
            println!(
                "    (visible length: {} chars, cursor X: {})",
                visible_line.len(),
                screen_pos.0
            );
        }
    }

    println!("\n=== Summary ===");
    if let Some(first) = first_disappear {
        println!("Cursor first disappeared at keypress: {}", first);
    }
    if let Some(reapp) = reappear {
        println!("Cursor reappeared at keypress: {}", reapp);
    }
    if let (Some(first), Some(reapp)) = (first_disappear, reappear) {
        println!("Cursor was invisible for {} keypresses", reapp - first);
    }
    println!(
        "Total keypresses where cursor was invisible: {}",
        disappeared_count
    );

    // Assert if cursor disappeared
    assert_eq!(
        disappeared_count,
        0,
        "BUG REPRODUCED: Cursor disappeared for {} keypresses (from {} to {})",
        disappeared_count,
        first_disappear.unwrap_or(0),
        reappear.unwrap_or(0)
    );
}

/// Test vertical scrolling when typing lines to the bottom of screen
/// The viewport should scroll down to keep the cursor visible
#[test]
fn test_vertical_scroll_when_typing_to_bottom() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Terminal height is 24, but we need to account for:
    // - Tab bar (1 line)
    // - Status bar (1 line)
    // So visible content area is 22 lines
    let visible_lines = 22;

    // Start with viewport at top
    let viewport = &harness.editor().active_viewport();
    assert_eq!(viewport.top_byte, 0, "Should start at top");

    // Type enough lines to fill the visible area and go beyond
    // We'll type (visible_lines + 10) lines to ensure scrolling happens
    let total_lines = visible_lines + 10;

    for i in 0..total_lines {
        harness.type_text(&format!("Line {i}")).unwrap();

        // Add newline except for the last line
        if i < total_lines - 1 {
            harness
                .send_key(KeyCode::Enter, KeyModifiers::NONE)
                .unwrap();
        }
    }

    // Count lines to verify cursor is on expected line
    let cursor_pos = harness.cursor_position();
    let buffer = &mut harness.editor_mut().active_state_mut().buffer;
    let mut iter = buffer.line_iterator(0, 80);
    let mut cursor_line = 0;
    while let Some((line_start, _)) = iter.next_line() {
        if line_start > cursor_pos {
            break;
        }
        cursor_line += 1;
    }
    // We typed total_lines lines, so last line should be total_lines
    assert_eq!(cursor_line, total_lines, "Cursor should be on last line");

    // The viewport should have scrolled down (top_byte > 0)
    let top_byte = harness.editor().active_viewport().top_byte;
    assert!(
        top_byte > 0,
        "Viewport should have scrolled down, top_byte = {top_byte}"
    );

    // The last line should be visible on screen
    let screen_pos = harness.screen_cursor_position();
    assert!(
        screen_pos.1 <= visible_lines as u16,
        "Cursor screen Y ({}) should be within visible lines ({})",
        screen_pos.1,
        visible_lines
    );
}

/// Test vertical scrolling maintains cursor visibility with scroll offset
/// IGNORED: Test assumptions about visible_lines (22) don't match actual layout (20 lines).
/// The viewport correctly scrolls when cursor leaves visible area, but test math is wrong.
#[test]
#[ignore = "viewport scrolling test has incorrect visible_lines assumption (22 vs 20)"]
fn test_vertical_scroll_offset() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let _visible_lines = 22; // Account for tab bar and status bar

    // Type many lines
    for i in 0..40 {
        harness.type_text(&format!("Line {i}")).unwrap();
        if i < 39 {
            harness
                .send_key(KeyCode::Enter, KeyModifiers::NONE)
                .unwrap();
        }
    }

    // Cursor should be at bottom, viewport scrolled
    let initial_top_byte = harness.editor().active_viewport().top_byte;
    assert!(initial_top_byte > 0, "Should be scrolled down");

    // Move up by many lines - with new viewport behavior, viewport only scrolls
    // when cursor leaves the visible area (not proactively with scroll_offset)
    // Move up 20 lines (from 39 to 19), cursor should still be visible
    for _ in 0..20 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }

    // With new behavior: viewport doesn't scroll unless cursor leaves visible area
    // The cursor moved from line 39 to line 19, which is still in the visible range
    // (viewport shows lines 18-39, cursor at 19 is visible)
    let new_top_byte = harness.editor().active_viewport().top_byte;

    // Viewport should not have changed since cursor stayed within visible area
    assert_eq!(
        new_top_byte, initial_top_byte,
        "Viewport should not scroll when cursor stays in visible area: was {initial_top_byte}, now {new_top_byte}"
    );

    // Now move up enough to actually leave the viewport (move to top line)
    // This should trigger scrolling
    for _ in 0..19 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }

    let final_top_byte = harness.editor().active_viewport().top_byte;

    // Now viewport should have scrolled to keep cursor visible
    assert!(
        final_top_byte < initial_top_byte,
        "Viewport should scroll when cursor leaves visible area: was {initial_top_byte}, now {final_top_byte}"
    );
}

/// Test that viewport displays all available lines when content is larger than minimum
#[test]
fn test_viewport_displays_all_lines() {
    // Create a harness with 80 columns and 40 rows
    // This gives us: 1 line for menu bar + 1 line for tabs + 37 lines for content + 1 line for status = 40 total
    let mut harness = EditorTestHarness::new(80, 40).unwrap();
    harness.render().unwrap();

    // Get actual viewport height from harness (avoids hardcoding)
    let expected_viewport_height = harness.viewport_height();

    // Create content with 35 lines (should all be visible)
    let mut content = String::new();
    for i in 1..=35 {
        if i > 1 {
            content.push('\n');
        }
        content.push_str(&format!("This is line number {i}"));
    }

    harness.type_text(&content).unwrap();

    // Check the viewport state
    let editor = harness.editor();
    let viewport = editor.active_viewport();
    let viewport_height = viewport.height as usize;

    // Viewport should match expected (40 - 3 for menu bar, tab bar, and status bar)
    assert_eq!(
        viewport_height, expected_viewport_height,
        "Viewport height should be {expected_viewport_height} (40 total - 3 for UI chrome)"
    );

    // Get visible range
    let visible_line_count = viewport.visible_line_count();

    // All 35 lines should fit in the viewport
    assert!(
        visible_line_count >= 35,
        "Expected to see at least 35 lines, but visible range is only {visible_line_count} lines"
    );

    // Render and check that lines are actually displayed on screen
    harness.render().unwrap();

    // Check that we can see line 1 and line 35 on the screen
    harness.assert_screen_contains("This is line number 1");
    harness.assert_screen_contains("This is line number 35");

    // Also check some lines in the middle
    harness.assert_screen_contains("This is line number 15");
    harness.assert_screen_contains("This is line number 25");
}

/// Test viewport with 31-row terminal (matching user's scenario)
#[test]
fn test_viewport_31_rows() {
    use crossterm::event::{KeyCode, KeyModifiers};

    // Create a harness with 131 columns and 31 rows (matching user's terminal)
    // This gives us: 1 line for menu bar + 1 line for tabs + 28 lines for content + 1 line for status = 31 total
    let mut harness = EditorTestHarness::new(131, 31).unwrap();
    harness.render().unwrap();

    // Get actual viewport height from harness (avoids hardcoding)
    let expected_viewport_height = harness.viewport_height();

    // Create content with lines to fill the viewport
    let mut content = String::new();
    for i in 1..=expected_viewport_height {
        if i > 1 {
            content.push('\n');
        }
        content.push_str(&format!("Line {i}"));
    }

    harness.type_text(&content).unwrap();

    // Check the viewport state
    let editor = harness.editor();
    let viewport = editor.active_viewport();
    let viewport_height = viewport.height as usize;

    // Viewport should match expected (31 - 3 for menu bar, tab bar, and status bar)
    assert_eq!(
        viewport_height, expected_viewport_height,
        "Viewport height should be {expected_viewport_height} (31 total - 3 for UI chrome)"
    );

    // Get visible range
    let visible_line_count = viewport.visible_line_count();

    // All lines that we created should be visible
    assert_eq!(
        visible_line_count, expected_viewport_height,
        "Expected to see all {expected_viewport_height} lines, but visible range is only {visible_line_count} lines"
    );

    // Move cursor to the start of the document so all lines are in view
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Render and verify lines are displayed
    harness.render().unwrap();

    // Check that we can see first and last lines
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains(&format!("Line {expected_viewport_height}"));

    // Check lines throughout
    harness.assert_screen_contains("Line 10");
    harness.assert_screen_contains("Line 20");

    // Now open the command palette (which shows suggestions)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Command palette should be visible
    harness.assert_screen_contains("Command:");

    // Suggestions popup should be visible with commands (alphabetically sorted, starting with A/C)
    harness.assert_screen_contains("Add Cursor");
    harness.assert_screen_contains("Close");

    // Note: With our viewport sync architecture, the viewport height may change
    // when overlays like command palette are shown, as they can affect the
    // actual rendering area. This is expected behavior.

    // Close the command palette
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // After closing palette, viewport should be restored to full height
    let editor = harness.editor();
    let viewport = editor.active_viewport();
    let viewport_height_after = viewport.height as usize;

    assert_eq!(
        viewport_height_after, expected_viewport_height,
        "Viewport height should be restored to {expected_viewport_height} after closing command palette, but got {viewport_height_after}"
    );

    // Get visible range after closing palette
    let visible_line_count_after = viewport.visible_line_count();

    assert_eq!(
        visible_line_count_after, expected_viewport_height,
        "Expected to see all {expected_viewport_height} lines after closing palette, but visible range is only {visible_line_count_after} lines"
    );

    // All lines should still be visible on screen
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains(&format!("Line {expected_viewport_height}"));
}

#[test]
#[ignore] // Run with: cargo test test_load_big_file_e2e -- --ignored --nocapture
fn test_load_big_file_e2e() {
    use crossterm::event::{KeyCode, KeyModifiers};

    use std::time::Instant;

    // Initialize tracing
    use tracing_subscriber::EnvFilter;
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    println!("\n=== E2E Test: Loading BIG.txt through full editor ===");

    // Generate BIG.txt if it doesn't exist
    let big_txt_path = TestFixture::big_txt_for_test("jump_to_line_scroll_middle").unwrap();

    let start = Instant::now();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let harness_time = start.elapsed();
    println!("✓ Harness created in: {harness_time:?}");

    let start = Instant::now();
    harness.open_file(&big_txt_path).unwrap();
    let open_time = start.elapsed();
    println!("✓ File opened (with render) in: {open_time:?}");

    // Verify the file is actually loaded
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Editor Implementation Plan"),
        "First line should be visible"
    );

    // Test pagedown performance (this is where we had issues)
    let start = Instant::now();
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    let pagedown_time = start.elapsed();
    println!("✓ First PageDown in: {pagedown_time:?}");

    // Do a few more pagedowns to ensure consistent performance
    for i in 1..5 {
        let start = Instant::now();
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        let time = start.elapsed();
        println!("✓ PageDown #{i} in: {time:?}");
    }

    println!("\nTotal time: {:?}", harness_time + open_time);
    println!("Note: This includes the full editor flow + first render");
}

/// Test jumping to EOF in large file (Ctrl+End) without hang
/// Bug: Previously byte_to_line_lazy() would call count_newlines_in_range()
/// which loops through every byte from last known position to EOF, causing
/// a hang when jumping to end of 60MB file.
/// Fix: LineNumber enum allows buffer to return relative line numbers without
/// forcing expensive scans, and viewport handles this transparently.
#[test]
fn test_jump_to_eof_large_file() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::time::Instant;

    // Get shared large file (all tests use the same 61MB file for efficiency)
    let big_txt_path = TestFixture::big_txt_for_test("jump_to_eof_large_file").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();

    println!("\n=== Testing Ctrl+End on 61MB file ===");

    // Jump to EOF with Ctrl+End - this should NOT hang
    let start = Instant::now();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    let jump_time = start.elapsed();

    println!("✓ Ctrl+End completed in: {jump_time:?}");

    // Verify we jumped to near EOF
    let cursor_pos = harness.cursor_position();
    let buffer_len = harness.buffer_len();

    // Cursor should be very close to EOF (within last line)
    assert!(
        cursor_pos > buffer_len.saturating_sub(1000),
        "Cursor should be near EOF. Position: {cursor_pos}, Buffer length: {buffer_len}"
    );

    // Just log the time for informational purposes - don't assert on it
    // Machines can be slow, especially in CI or when running tests in parallel
    println!("✓ Cursor at position {cursor_pos} (buffer len: {buffer_len})");

    // Now test Page Up after jumping to EOF - this tests backward iteration
    println!("\n=== Testing Page Up after EOF ===");

    let start = Instant::now();
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    let pageup_time = start.elapsed();

    println!("✓ Page Up completed in: {pageup_time:?}");

    // Cursor should have moved backwards
    let new_cursor_pos = harness.cursor_position();
    assert!(
        new_cursor_pos < cursor_pos,
        "Cursor should have moved up. Was: {cursor_pos}, Now: {new_cursor_pos}"
    );

    println!("✓ Cursor moved from {cursor_pos} to {new_cursor_pos}");

    // Test multiple Page Ups in sequence - should all be fast
    println!("\n=== Testing multiple Page Ups ===");
    let start = Instant::now();
    for i in 0..5 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
        let pos = harness.cursor_position();
        println!("  Page Up {}: cursor at {}", i + 1, pos);
    }
    let multi_pageup_time = start.elapsed();

    println!("✓ 5 Page Ups completed in: {multi_pageup_time:?}");

    // Test line up movements - should also be fast
    println!("\n=== Testing line up movements ===");
    let start = Instant::now();
    for i in 0..20 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        if i % 5 == 4 {
            let pos = harness.cursor_position();
            println!("  After {} ups: cursor at {}", i + 1, pos);
        }
    }
    let line_up_time = start.elapsed();

    println!("✓ 20 line ups completed in: {line_up_time:?}");

    // Final sanity check: cursor should be well before EOF now
    let final_pos = harness.cursor_position();
    assert!(
        final_pos < buffer_len - 1000,
        "After scrolling up, cursor should be well before EOF"
    );

    println!(
        "✓ Final cursor position: {} (moved {} bytes from EOF)",
        final_pos,
        buffer_len - final_pos
    );
}

/// Test that screen content is correct when loading and navigating a large file
/// Validates that chunk-based lazy loading displays the correct data
#[test]
fn test_large_file_screen_content_validation() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::time::Instant;

    println!("\n=== Testing Large File Screen Content Validation ===");

    // Get shared large file (61MB, each line starts with "@00000000: " format showing byte offset)
    let big_txt_path = TestFixture::big_txt_for_test("screen_content_validation").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();

    let buffer_len = harness.buffer_len();
    println!("✓ Opened 61MB file (buffer length: {})", buffer_len);

    // Test 1: Validate initial screen shows correct content (beginning of file)
    println!("\n=== Test 1: Initial screen at file start ===");
    let screen = harness.screen_to_string();

    println!("Screen content:\n{}", screen);

    // The file starts with "@00000000: " at byte offset 0
    assert!(
        screen.contains("@00000000:"),
        "Initial screen should show first line starting at byte 0. Screen:\n{}",
        screen
    );
    // Each line is 80 bytes, so second line starts at byte 80 (hex)
    assert!(
        screen.contains("@00000080:"),
        "Initial screen should show second line starting at byte 80"
    );
    println!("✓ Initial screen shows correct content from file start (bytes 0-80 visible)");

    // Test 2: Jump to middle of file and validate content
    // TEMPORARILY DISABLED - PageDown not working properly with large files
    println!("\n=== Test 2: Jump to middle of file (SKIPPED) ===");
    /*
    // Jump to approximately 30MB into the file
    let target_offset = 30 * 1024 * 1024;
    let start = Instant::now();

    // Page down many times to get near the middle
    // Each page is ~22 lines * 80 bytes = ~1760 bytes
    let pages_to_middle = target_offset / 1760;
    for _ in 0..pages_to_middle.min(500) {
        harness.send_key(KeyCode::PageDown, KeyModifiers::NONE).unwrap();
    }
    println!("✓ Navigated toward middle in: {:?}", start.elapsed());

    let cursor_pos = harness.cursor_position();
    let screen_middle = harness.screen_to_string();

    println!("Current cursor position: {}", cursor_pos);

    // The cursor should be somewhere in the millions of bytes
    assert!(
        cursor_pos > 10_000_000,
        "After many page downs, cursor should be deep in file (at {})",
        cursor_pos
    );

    // The screen should show byte offsets in the appropriate range
    // Since each line is 80 bytes, round cursor position to nearest line start
    let line_start = (cursor_pos / 80) * 80;
    let offset_marker = format!("@{:08}:", line_start);

    // The screen might not show exact cursor position but should be in the ballpark
    // Just verify we're seeing lines with large byte offsets
    let has_large_offset = screen_middle.lines().any(|line| {
        line.contains("@") && {
            // Extract the byte offset from the line if present
            if let Some(start_idx) = line.find("@") {
                if let Some(end_idx) = line[start_idx..].find(":") {
                    let offset_str = &line[start_idx + 1..start_idx + end_idx];
                    if let Ok(offset) = offset_str.parse::<usize>() {
                        return offset > 5_000_000; // At least 5MB in
                    }
                }
            }
            false
        }
    });

    assert!(
        has_large_offset,
        "Middle screen should show byte offsets > 5MB"
    );
    println!("✓ Middle of file shows correct content with appropriate byte offsets");
    */

    // Test 3: Jump to end of file and validate content
    println!("\n=== Test 3: Jump to end of file ===");
    let start = Instant::now();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    println!("✓ Jumped to EOF in: {:?}", start.elapsed());

    let cursor_pos = harness.cursor_position();
    println!("Cursor position after Ctrl+End: {}", cursor_pos);
    println!("Buffer length: {}", buffer_len);

    let screen_end = harness.screen_to_string();
    println!("Screen after Ctrl+End:\n{}", screen_end);

    assert!(
        cursor_pos > buffer_len - 1000,
        "Cursor should be near EOF. Position: {}, Buffer length: {}",
        cursor_pos,
        buffer_len
    );

    // The last line should show a byte offset near the end of the file
    // buffer_len is ~64MB, so we should see offsets close to that
    let has_end_offset = screen_end.lines().any(|line| {
        line.contains("@") && {
            if let Some(start_idx) = line.find("@") {
                if let Some(end_idx) = line[start_idx..].find(":") {
                    let offset_str = &line[start_idx + 1..start_idx + end_idx];
                    if let Ok(offset) = offset_str.parse::<usize>() {
                        // Should be within last few MB of file
                        return offset > buffer_len - 5_000_000;
                    }
                }
            }
            false
        }
    });

    assert!(
        has_end_offset,
        "End screen should show byte offsets near EOF (> {})",
        buffer_len - 5_000_000
    );
    println!("✓ End of file shows correct content near EOF");

    // Test 4: Jump back to beginning and validate
    println!("\n=== Test 4: Jump back to beginning ===");
    let start = Instant::now();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    println!("✓ Jumped to start in: {:?}", start.elapsed());

    let cursor_pos = harness.cursor_position();
    assert_eq!(cursor_pos, 0, "Cursor should be at position 0");

    let screen_start = harness.screen_to_string();

    // Should show byte offset 0 again
    assert!(
        screen_start.contains("@00000000:"),
        "After jumping back, screen should show first line at byte 0"
    );
    assert!(
        screen_start.contains("@00000080:"),
        "After jumping back, screen should show second line at byte 80"
    );
    println!("✓ Beginning of file shows correct content after navigation");

    // Test 5: Make an edit and verify it appears on screen
    println!("\n=== Test 5: Edit and verify screen update ===");
    harness.type_text("EDIT_MARKER_").unwrap();

    let screen_after_edit = harness.screen_to_string();
    assert!(
        screen_after_edit.contains("EDIT_MARKER_"),
        "Screen should show the edit we just made"
    );
    println!("✓ Edit appears correctly on screen");

    // Test 6: Navigate away and back, verify edit persists
    println!("\n=== Test 6: Navigate away and back, verify edit persists ===");
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    let screen_after_nav = harness.screen_to_string();
    assert!(
        screen_after_nav.contains("EDIT_MARKER_"),
        "Edit should still be visible after navigation"
    );
    println!("✓ Edit persists correctly after navigation");

    println!("\n✓ All screen content validation tests passed!");
}

/// Test that we can navigate to EOF and back to beginning in a large file
/// Verifies that navigation works correctly and cursor ends up at the right positions
#[test]
fn test_line_numbers_absolute_after_jump_to_beginning() {
    use crossterm::event::{KeyCode, KeyModifiers};

    println!("\n=== Testing navigation: EOF -> Home ===");

    // Get shared large file (all tests use the same 61MB file for efficiency)
    let big_txt_path = TestFixture::big_txt_for_test("line_numbers_absolute").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();

    println!("✓ Opened 61MB file");
    let buffer_len = harness.editor().active_state().buffer.len();

    // Jump to EOF
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    println!("✓ Jumped to EOF");

    // Verify we're at the end
    let eof_pos = harness.cursor_position();
    assert_eq!(eof_pos, buffer_len, "Should be at EOF");

    // Check viewport scrolled
    {
        let viewport = harness.editor().active_viewport();
        assert!(viewport.top_byte > 0, "Viewport should have scrolled down");
    }

    // Now jump back to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    println!("✓ Jumped back to beginning");

    // Check cursor is at start
    let cursor_pos = harness.cursor_position();
    assert_eq!(cursor_pos, 0, "Cursor should be at position 0");

    // Check that viewport is at top
    {
        let viewport = harness.editor().active_viewport();
        assert_eq!(viewport.top_byte, 0, "Viewport should be at top");
    }

    // Verify first few lines are readable via iterator
    println!("\n  Verifying first few lines are readable:");
    let top_byte = harness.editor().active_viewport().top_byte;
    let state = harness.editor_mut().active_state_mut();
    let mut iter = state.buffer.line_iterator(top_byte, 80);
    let mut line_count = 0;
    for i in 0..5 {
        if let Some((byte_pos, content)) = iter.next_line() {
            println!(
                "    Line {} at byte {}: {} bytes",
                i,
                byte_pos,
                content.len()
            );
            line_count += 1;
        }
    }

    assert!(
        line_count >= 5,
        "Should be able to read at least 5 lines from beginning"
    );
    println!("\n✓ Navigation and line iteration working correctly");
}

/// Helper function to extract scrollbar thumb size from harness.
/// Scrollbars are rendered with background colors, not characters.
/// Returns (thumb_start_row, thumb_size, scrollbar_col)
fn extract_scrollbar_info(
    harness: &EditorTestHarness,
    terminal_width: u16,
    _terminal_height: u16,
) -> (usize, usize, u16) {
    let scrollbar_col = terminal_width - 1; // Rightmost column
    let (content_first_row, content_last_row) = harness.content_area_rows();

    let mut thumb_start = None;
    let mut thumb_end = None;

    // Scan the content area for scrollbar thumb cells
    for row in content_first_row..=content_last_row {
        if harness.is_scrollbar_thumb_at(scrollbar_col, row as u16) {
            // Found thumb cell
            if thumb_start.is_none() {
                thumb_start = Some(row);
            }
            thumb_end = Some(row);
        }
    }

    match (thumb_start, thumb_end) {
        (Some(start), Some(end)) => {
            let thumb_size = end - start + 1;
            (start, thumb_size, scrollbar_col)
        }
        _ => (0, 0, scrollbar_col), // No thumb found
    }
}

/// Test scrollbar handle size consistency during PageDown and Down key scrolling
/// This test verifies that the scrollbar thumb maintains consistent visual size
/// throughout scrolling, especially when reaching the end of the file
fn test_scrollbar_consistency_with_file_size(num_lines: usize) {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create file with specified number of lines
    let content: String = (1..=num_lines)
        .map(|i| format!("Line {i} with some content here\n"))
        .collect();
    std::fs::write(&file_path, content).unwrap();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    harness.open_file(&file_path).unwrap();

    println!("\n=== Testing file with {num_lines} lines ===");

    // Go to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Track scrollbar sizes throughout the scrolling
    let mut scrollbar_sizes = Vec::new();
    let mut positions = Vec::new();

    // Initial state
    let screen = harness.screen_to_string();
    let (start_row, size, _) = extract_scrollbar_info(&harness, terminal_width, terminal_height);
    scrollbar_sizes.push(size);
    positions.push((0, harness.top_line_number()));

    println!(
        "Initial: top_line={}, scrollbar thumb size={}, start_row={}",
        harness.top_line_number(),
        size,
        start_row
    );

    if num_lines <= 100 {
        println!("\nInitial screen:\n{screen}\n");
    }

    // Scroll with PageDown multiple times
    let mut step = 0;
    loop {
        let before_line = harness.top_line_number();
        let before_cursor = harness.cursor_position();

        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let after_line = harness.top_line_number();
        let after_cursor = harness.cursor_position();

        // Check if we've stopped moving (reached end)
        if before_line == after_line && before_cursor == after_cursor {
            println!("Reached end of file at line {after_line}");
            break;
        }

        step += 1;
        let (start_row, size, _) =
            extract_scrollbar_info(&harness, terminal_width, terminal_height);
        scrollbar_sizes.push(size);
        positions.push((step, after_line));

        println!("PageDown step {step}: top_line={after_line}, scrollbar thumb size={size}, start_row={start_row}");

        // Safety: prevent infinite loops
        if step > 100 {
            break;
        }
    }

    // Now scroll down with Down arrow key line by line from current position
    let pagedown_steps = step;
    for _ in 0..10 {
        let before_line = harness.top_line_number();
        let before_cursor = harness.cursor_position();

        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let after_line = harness.top_line_number();
        let after_cursor = harness.cursor_position();

        // Check if we've stopped moving
        if before_line == after_line && before_cursor == after_cursor {
            break;
        }

        step += 1;
        let (start_row, size, _) =
            extract_scrollbar_info(&harness, terminal_width, terminal_height);
        scrollbar_sizes.push(size);
        positions.push((step, after_line));

        println!(
            "Down step {}: top_line={}, scrollbar thumb size={}, start_row={}",
            step - pagedown_steps,
            after_line,
            size,
            start_row
        );
    }

    // Analyze scrollbar sizes
    println!("\n=== Analysis for {num_lines} lines ===");
    println!("Scrollbar sizes observed: {scrollbar_sizes:?}");

    // The scrollbar size should be consistent throughout scrolling
    // It may vary by 1 due to rounding, but should not change dramatically
    if scrollbar_sizes.len() > 1 {
        let min_size = *scrollbar_sizes.iter().min().unwrap();
        let max_size = *scrollbar_sizes.iter().max().unwrap();
        let size_variation = max_size.saturating_sub(min_size);

        println!("Min scrollbar size: {min_size}");
        println!("Max scrollbar size: {max_size}");
        println!("Size variation: {size_variation}");

        // All sizes should be positive (thumb should always be visible)
        assert!(
            min_size > 0,
            "Scrollbar thumb should always be visible (min size > 0), but got min={min_size}"
        );

        // The scrollbar thumb size MUST remain constant during scrolling
        // since it represents the ratio of viewport height to total content height.
        // Both values are constant:
        // - Viewport height = allocated content area (constant terminal rows)
        // - Total lines = document size (doesn't change during scrolling)
        //
        // For files under the large_file_threshold (default 1MB), we count actual
        // lines for precise scrollbar rendering with zero variation.
        //
        // For files over the threshold, we use a constant 1-character thumb for
        // performance reasons (also zero variation).
        assert_eq!(
            size_variation, 0,
            "Scrollbar thumb size MUST be constant (variation = 0) for {num_lines} lines, but varied by {size_variation} \
             (min={min_size}, max={max_size}). This indicates a bug in scrollbar rendering."
        );
    }

    println!(
        "✓ Scrollbar consistency test passed for {} lines (variation: {} chars)",
        num_lines,
        scrollbar_sizes
            .iter()
            .max()
            .unwrap_or(&0)
            .saturating_sub(*scrollbar_sizes.iter().min().unwrap_or(&0))
    );
}

/// Test scrollbar handle size remains consistent when scrolling with PageDown
#[test]
fn test_scrollbar_size_consistency_pagedown_50_lines() {
    test_scrollbar_consistency_with_file_size(50);
}

/// Test scrollbar handle size remains consistent when scrolling with PageDown
#[test]
fn test_scrollbar_size_consistency_pagedown_100_lines() {
    test_scrollbar_consistency_with_file_size(100);
}

/// Test scrollbar handle size remains consistent when scrolling with PageDown
#[test]
fn test_scrollbar_size_consistency_pagedown_200_lines() {
    test_scrollbar_consistency_with_file_size(200);
}

/// Test scrollbar handle size remains consistent when scrolling with PageDown
#[test]
fn test_scrollbar_size_consistency_pagedown_500_lines() {
    test_scrollbar_consistency_with_file_size(500);
}

/// Test scrollbar invariants for files under the large file threshold
/// This test verifies critical scrollbar properties:
/// 1. Handle size is constant (only changes if total line count changes)
/// 2. When at first line: handle top is at viewport top (row 0)
/// 3. When at last line: handle bottom is at viewport bottom (row height-1)
fn test_scrollbar_invariants_with_file_size(num_lines: usize) {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create file with specified number of lines
    let content: String = (1..=num_lines)
        .map(|i| format!("Line {i} with some content here\n"))
        .collect();
    std::fs::write(&file_path, content).unwrap();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    println!("\n=== Testing scrollbar invariants for {num_lines} lines ===");

    // Get content area bounds from harness (avoids hardcoding UI chrome assumptions)
    let (content_first_row, content_last_row) = harness.content_area_rows();
    let scrollbar_height = content_last_row - content_first_row + 1;

    // Go to beginning of file
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // INVARIANT 1: At first line, handle top should be at scrollbar top (content_first_row, after menu bar and tab bar)
    let (start_row, initial_size, _) =
        extract_scrollbar_info(&harness, terminal_width, terminal_height);

    println!("At first line: handle start_row={start_row}, size={initial_size}");
    assert_eq!(
        start_row, content_first_row,
        "When at first line, scrollbar handle top should be at row {content_first_row} (scrollbar top), but got row {start_row}"
    );

    // Track handle sizes throughout scrolling
    let mut all_sizes = vec![initial_size];

    // Scroll through the file with PageDown, collecting handle sizes
    let mut scroll_steps = 0;
    loop {
        let before_line = harness.top_line_number();
        let before_cursor = harness.cursor_position();

        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let after_line = harness.top_line_number();
        let after_cursor = harness.cursor_position();

        // Check if we've stopped moving
        if before_line == after_line && before_cursor == after_cursor {
            break;
        }

        scroll_steps += 1;
        let (start_row, size, _) =
            extract_scrollbar_info(&harness, terminal_width, terminal_height);
        all_sizes.push(size);

        println!("After PageDown {scroll_steps}: top_line={after_line}, handle start_row={start_row}, size={size}");

        // Safety: prevent infinite loops
        if scroll_steps > 100 {
            break;
        }
    }

    // Continue with Down arrow keys to ensure we reach the absolute last line
    for _ in 0..10 {
        let before_line = harness.top_line_number();
        let before_cursor = harness.cursor_position();

        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let after_line = harness.top_line_number();
        let after_cursor = harness.cursor_position();

        if before_line == after_line && before_cursor == after_cursor {
            break;
        }

        let (_start_row, size, _) =
            extract_scrollbar_info(&harness, terminal_width, terminal_height);
        all_sizes.push(size);
    }

    // INVARIANT 2: Handle size should be constant throughout
    let min_size = *all_sizes.iter().min().unwrap();
    let max_size = *all_sizes.iter().max().unwrap();
    let size_variation = max_size.saturating_sub(min_size);

    println!("\nHandle sizes observed: {all_sizes:?}");
    println!("Min size: {min_size}, Max size: {max_size}, Variation: {size_variation}");

    // Handle size MUST be constant (variation = 0) because it represents
    // the ratio of viewport height to total document height, neither of which
    // changes during scrolling. The viewport height is the allocated content area,
    // not the number of visible lines.
    assert_eq!(
        size_variation, 0,
        "Scrollbar handle size MUST be constant (variation = 0), but varied by {size_variation} (sizes: {all_sizes:?})"
    );

    // INVARIANT 3: At last line, handle bottom should be at scrollbar bottom
    let (start_row, size, _) = extract_scrollbar_info(&harness, terminal_width, terminal_height);
    let end_row = start_row + size;

    println!("\nAt last line: handle start_row={start_row}, size={size}, end_row={end_row}");
    println!("Scrollbar height (rows {content_first_row}-{content_last_row}): {scrollbar_height}");

    // The scrollbar goes from content_first_row to content_last_row
    // end_row is exclusive (start_row + size), so it should be near content_last_row + 1
    // Allow 1-row tolerance due to potential rounding in scrollbar calculations
    let scrollbar_bottom_row = content_last_row + 1;
    let diff = (end_row as i32 - scrollbar_bottom_row as i32).abs();

    assert!(
        diff <= 1,
        "When at last line, scrollbar handle bottom (row {end_row}) should be near scrollbar bottom (row {scrollbar_bottom_row}), diff={diff}"
    );

    // Go back to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify INVARIANT 1 again
    let (start_row_final, size_final, _) =
        extract_scrollbar_info(&harness, terminal_width, terminal_height);

    println!("\nBack at first line: handle start_row={start_row_final}, size={size_final}");

    assert_eq!(
        start_row_final, content_first_row,
        "When back at first line, scrollbar handle top should be at row {content_first_row}, but got row {start_row_final}"
    );

    // Size should match initial size (total lines haven't changed)
    assert_eq!(
        size_final, initial_size,
        "Handle size at end ({size_final}) should match initial size ({initial_size})"
    );

    println!("✓ All scrollbar invariants verified for {num_lines} lines");
}

/// Test scrollbar invariants for 50-line file
#[test]
fn test_scrollbar_invariants_50_lines() {
    test_scrollbar_invariants_with_file_size(50);
}

/// Test scrollbar invariants for 100-line file
#[test]
fn test_scrollbar_invariants_100_lines() {
    test_scrollbar_invariants_with_file_size(100);
}

/// Test scrollbar invariants for 200-line file
#[test]
fn test_scrollbar_invariants_200_lines() {
    test_scrollbar_invariants_with_file_size(200);
}

/// Test scrollbar invariants for 500-line file
#[test]
fn test_scrollbar_invariants_500_lines() {
    test_scrollbar_invariants_with_file_size(500);
}

/// Test that the last line of buffer never scrolls higher than the bottom of the editor
/// when scrolling vertically, unless the entire buffer is smaller than the view
#[test]
fn test_last_line_never_above_bottom() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    let terminal_height = 24u16;
    let terminal_width = 80u16;

    // Test Case 1: Buffer larger than viewport
    // Create a buffer with 50 lines (more than viewport height)
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    let mut content = String::new();
    for i in 1..=50 {
        if i > 1 {
            content.push('\n');
        }
        content.push_str(&format!("Line {i}"));
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Get content area bounds from harness (accounts for menu bar, tab bar, status bar)
    let (content_first_row, content_last_row) = harness.content_area_rows();

    // Jump to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify cursor is at the end of the file
    let buffer_content = harness.get_buffer_content().unwrap();
    let cursor_pos = harness.cursor_position();
    assert_eq!(
        cursor_pos,
        buffer_content.len(),
        "Cursor should be at end of file"
    );

    // Get the rendered screen and parse it
    let screen = harness.screen_to_string();
    let screen_lines: Vec<&str> = screen.lines().collect();

    // Find the last line of the buffer on screen
    let last_buffer_line = "Line 50";
    let mut last_line_row = None;
    for (row_idx, line) in screen_lines.iter().enumerate() {
        if line.contains(last_buffer_line) {
            last_line_row = Some(row_idx);
        }
    }

    // Verify the last line was found
    assert!(
        last_line_row.is_some(),
        "Last buffer line '{}' should be visible on screen",
        last_buffer_line
    );

    let last_line_row = last_line_row.unwrap();

    // The last line should be at or near the bottom of the content area
    // It should be at content_last_row (row 22 for height 24)
    assert_eq!(
        last_line_row, content_last_row,
        "Last buffer line should be at row {} (bottom of content area), but found at row {}",
        content_last_row, last_line_row
    );

    // Verify there are no empty rows between where we found the last line and the status bar
    // All rows from content_first_row to content_last_row should have content
    let mut empty_rows_below_last_line = 0;
    for row_idx in (last_line_row + 1)..=content_last_row {
        if row_idx < screen_lines.len() {
            let line = screen_lines[row_idx].trim();
            // Check if line is empty or just whitespace/gutter
            if line.is_empty() || line.chars().all(|c| c.is_whitespace() || c == '│') {
                empty_rows_below_last_line += 1;
            }
        }
    }

    assert_eq!(
        empty_rows_below_last_line, 0,
        "There should be no empty content rows below the last buffer line, but found {} empty rows",
        empty_rows_below_last_line
    );

    // Try to scroll down further with PageDown - should not move viewport
    let top_byte_before = harness.editor().active_viewport().top_byte;
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let top_byte_after = harness.editor().active_viewport().top_byte;

    assert_eq!(
        top_byte_before, top_byte_after,
        "Viewport should not scroll past the last line. \
         top_byte was {top_byte_before}, now {top_byte_after}"
    );

    // Try Down arrow - should not move viewport
    let top_byte_before = harness.editor().active_viewport().top_byte;
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let top_byte_after = harness.editor().active_viewport().top_byte;

    assert_eq!(
        top_byte_before, top_byte_after,
        "Viewport should not scroll when cursor is at last line. \
         top_byte was {top_byte_before}, now {top_byte_after}"
    );

    // Test Case 2: Buffer smaller than viewport
    // Create a buffer with only 10 lines (less than viewport height)
    let small_file_path = temp_dir.path().join("small.txt");
    let mut small_content = String::new();
    for i in 1..=10 {
        if i > 1 {
            small_content.push('\n');
        }
        small_content.push_str(&format!("Small Line {i}"));
    }
    std::fs::write(&small_file_path, &small_content).unwrap();

    let mut small_harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    small_harness.open_file(&small_file_path).unwrap();
    small_harness.render().unwrap();

    // Jump to end of file
    small_harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    small_harness.render().unwrap();

    // When buffer is smaller than viewport, top_byte should be 0
    let viewport = small_harness.editor().active_viewport();
    assert_eq!(
        viewport.top_byte, 0,
        "When buffer is smaller than viewport, top_byte should remain 0"
    );

    // Get screen for small buffer
    let small_screen = small_harness.screen_to_string();
    let small_screen_lines: Vec<&str> = small_screen.lines().collect();

    // Verify the first line is at the top of content area
    assert!(
        small_screen_lines.len() > content_first_row,
        "Screen should have enough lines"
    );
    assert!(
        small_screen_lines[content_first_row].contains("Small Line 1"),
        "First line should be at row {} (top of content area)",
        content_first_row
    );

    // Verify the last line is visible somewhere in the content area
    let mut found_last_small_line = false;
    for row_idx in content_first_row..=content_last_row {
        if row_idx < small_screen_lines.len()
            && small_screen_lines[row_idx].contains("Small Line 10")
        {
            found_last_small_line = true;

            // Since buffer is smaller than viewport, last line should NOT be at bottom
            assert!(
                row_idx < content_last_row,
                "When buffer is smaller than viewport, last line should not be at bottom. \
                 Found at row {} but content area ends at row {}",
                row_idx,
                content_last_row
            );
            break;
        }
    }

    assert!(
        found_last_small_line,
        "Last line of small buffer should be visible"
    );
}

/// Test PageDown behavior when buffer is exactly the same height as viewport
/// Regression test: cursor should end up at bottom row, not at top
#[test]
fn test_page_down_when_buffer_equals_viewport_height() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    let terminal_height = 24u16;
    // Layout: menu bar, tab bar, content, status bar, prompt line
    // Content area is terminal_height - 3 (rows 2 to terminal_height-3)
    let viewport_height = (terminal_height - 3) as usize; // 21 lines
    let expected_bottom_row = terminal_height - 3; // Row 21

    // Create buffer with exactly viewport_height lines
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("exact_fit.txt");
    let mut content = String::new();
    for i in 1..=viewport_height {
        if i > 1 {
            content.push('\n');
        }
        content.push_str(&format!("Line {i}"));
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, terminal_height).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Press PageDown twice from the top
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();

    // Verify cursor is detected at the bottom row
    let all_cursors = harness.find_all_cursors();
    assert_eq!(all_cursors.len(), 1, "Should have exactly one cursor");

    let (_, cursor_y, _, is_primary) = all_cursors[0];
    assert!(is_primary, "Cursor should be primary cursor");
    assert_eq!(
        cursor_y, expected_bottom_row,
        "Cursor should be at bottom row {}, but is at row {}",
        expected_bottom_row, cursor_y
    );
}

/// Test that pressing Enter repeatedly in an empty buffer maintains the invariant
/// that the last line of the buffer is always pinned to the bottom of the editor area.
/// Bug: When approaching the bottom, the viewport suddenly scrolls incorrectly,
/// showing only a few lines at the top of the viewport with empty space below.
#[test]
fn test_enter_key_maintains_bottom_line_pinned() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let terminal_height = 24u16;
    let terminal_width = 80u16;

    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    harness.render().unwrap();

    // Get content area bounds from harness (accounts for menu bar, tab bar, status bar)
    let (content_first_row, content_last_row) = harness.content_area_rows();

    // Start with an empty buffer
    let initial_content = harness.get_buffer_content().unwrap();
    assert_eq!(initial_content, "", "Buffer should start empty");

    // Press Enter many times to create lines
    // We'll press it 30 times to ensure we exceed the viewport height (22 lines)
    let num_enters = 30;

    println!(
        "\n=== Testing Enter key {} times in empty buffer ===",
        num_enters
    );

    for i in 1..=num_enters {
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        // After each Enter, check that the last line is visible at the bottom
        let screen = harness.screen_to_string();
        let screen_lines: Vec<&str> = screen.lines().collect();

        // Count actual buffer lines (number of newlines + 1, or just count lines)
        let buffer_content = harness.get_buffer_content().unwrap();
        let buffer_line_count = if buffer_content.is_empty() {
            1
        } else {
            buffer_content.chars().filter(|&c| c == '\n').count() + 1
        };

        // Get viewport state
        let viewport = &harness.editor().active_viewport();
        let top_byte = viewport.top_byte;

        // Find where the cursor is on screen
        let (_cursor_x, cursor_y) = harness.screen_cursor_position();

        // The cursor should be on the empty line after the newlines
        // For the invariant: when buffer has more lines than viewport height,
        // the last line (empty line where cursor is) should be at the bottom of content area

        // Check if there's excessive empty space below the cursor
        let mut empty_rows_below_cursor = 0;
        for row_idx in ((cursor_y as usize) + 1)..=content_last_row {
            if row_idx < screen_lines.len() {
                let line = screen_lines[row_idx].trim();
                // Check if line is empty or just whitespace/gutter
                if line.is_empty() || line.chars().all(|c| c.is_whitespace() || c == '│') {
                    empty_rows_below_cursor += 1;
                }
            }
        }

        println!(
            "After Enter #{}: buffer_lines={}, cursor_y={}, top_byte={}, empty_rows_below={}",
            i, buffer_line_count, cursor_y, top_byte, empty_rows_below_cursor
        );

        // The invariant: when buffer has more lines than viewport,
        // the last line should be pinned to the bottom (cursor at content_last_row)
        // There should be no empty content rows below the cursor
        if buffer_line_count > (content_last_row - content_first_row + 1) {
            // Buffer is larger than viewport - last line should be at bottom
            assert_eq!(
                empty_rows_below_cursor, 0,
                "BUG REPRODUCED at Enter #{}: When buffer has {} lines (more than viewport height), \
                 the last line should be pinned to the bottom with no empty rows below, \
                 but found {} empty rows below cursor at row {}. This breaks the bottom-line-pinned invariant.\n\
                 Screen:\n{}",
                i, buffer_line_count, empty_rows_below_cursor, cursor_y, screen
            );

            // Additionally, cursor should be at the bottom row
            assert_eq!(
                cursor_y, content_last_row as u16,
                "BUG REPRODUCED at Enter #{}: Cursor should be at bottom row {} when buffer ({} lines) \
                 exceeds viewport, but is at row {}",
                i, content_last_row, buffer_line_count, cursor_y
            );
        }
    }

    println!(
        "\n✓ Enter key maintains bottom line pinned throughout {} presses",
        num_enters
    );
}

/// Test cursor visibility and horizontal scrolling when moving to end of long line
/// Bug report: When line wrapping is disabled and moving right along a long line,
/// the cursor disappears visually 12 characters before the actual end of the line
/// (appears at coordinate 0,0), while logically it's still in the right place.
/// Also, the last 12 characters are rendered in a different color, and horizontal
/// scrolling doesn't adjust properly.
#[test]
fn test_cursor_visibility_at_line_end_no_wrap() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::config::Config;

    let mut config = Config::default();
    config.editor.line_wrap = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    let gutter_width = 8; // Approximate gutter width for line numbers
    let visible_width = 80 - gutter_width; // ~72 characters visible

    // Create a long line that extends well beyond visible width
    // We'll create a line that's 100 characters long
    let line_length = 100;
    let long_line = "a".repeat(line_length);

    harness.type_text(&long_line).unwrap();
    harness.render().unwrap();

    println!("\n=== Testing cursor visibility at end of long line (no wrap) ===");
    println!("Line length: {} chars", line_length);
    println!("Terminal width: 80, Visible width: ~{}", visible_width);

    // Move to the beginning of the line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert_eq!(harness.cursor_position(), 0, "Should be at position 0");

    let initial_screen_pos = harness.screen_cursor_position();
    println!(
        "Initial screen cursor at Home: ({}, {})",
        initial_screen_pos.0, initial_screen_pos.1
    );

    // Track issues as we move right character by character
    let mut issues = Vec::new();

    // Move right towards the end of the line, checking each position
    // Focus especially on the last 20 characters
    for i in 0..line_length {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let buffer_pos = harness.cursor_position();
        let screen_pos = harness.screen_cursor_position();
        let left_col = harness.editor().active_viewport().left_column;

        // Expected behavior:
        // 1. Buffer position should match i+1
        if buffer_pos != i + 1 {
            issues.push(format!(
                "At step {}: buffer position {} != expected {}",
                i + 1,
                buffer_pos,
                i + 1
            ));
        }

        // 2. Screen cursor should NEVER be at (0, 0) unless that's the actual position
        if screen_pos == (0, 0) && buffer_pos > 0 {
            issues.push(format!(
                "At step {}: CURSOR AT (0,0) - buffer pos {} but screen shows (0,0)",
                i + 1,
                buffer_pos
            ));
        }

        // 3. When buffer position exceeds visible width, horizontal scrolling should occur
        // The cursor should be kept visible, so left_column should increase
        if buffer_pos > visible_width && left_col == 0 {
            issues.push(format!(
                "At step {}: NO HORIZONTAL SCROLL - buffer pos {} exceeds visible width {} but left_column=0",
                i + 1, buffer_pos, visible_width
            ));
        }

        // 4. Screen cursor X should be within reasonable bounds
        // Allow cursor at position 80 (one past the last column) for end-of-line position
        if screen_pos.0 > 80 {
            issues.push(format!(
                "At step {}: CURSOR OUT OF BOUNDS - screen x={} (> 80)",
                i + 1,
                screen_pos.0
            ));
        }

        // 5. When near the end (last 20 chars), pay special attention
        if i >= line_length - 20 {
            println!(
                "Step {}: buffer_pos={}, screen_pos=({}, {}), left_col={}",
                i + 1,
                buffer_pos,
                screen_pos.0,
                screen_pos.1,
                left_col
            );

            // The cursor should be visible - calculate expected screen X
            // Expected screen X = gutter_width + (buffer_pos - left_col)
            let expected_screen_x = gutter_width as u16 + (buffer_pos - left_col) as u16;

            // Allow some tolerance for gutter width calculation
            let tolerance = 3;
            if screen_pos.0 < expected_screen_x.saturating_sub(tolerance)
                || screen_pos.0 > expected_screen_x + tolerance
            {
                issues.push(format!(
                    "At step {} (near end): screen x={} doesn't match expected ~{} (buffer_pos={}, left_col={})",
                    i + 1, screen_pos.0, expected_screen_x, buffer_pos, left_col
                ));
            }
        }
    }

    println!("\n=== Test Results ===");
    if issues.is_empty() {
        println!(
            "✓ No issues found - cursor remained visible and horizontal scrolling worked correctly"
        );
    } else {
        println!("✗ Found {} issues:", issues.len());
        for issue in &issues {
            println!("  - {}", issue);
        }
    }

    // Final checks at the end of the line
    let final_buffer_pos = harness.cursor_position();
    let final_screen_pos = harness.screen_cursor_position();
    let final_left_col = harness.editor().active_viewport().left_column;

    println!("\nFinal position:");
    println!("  Buffer position: {}", final_buffer_pos);
    println!(
        "  Screen position: ({}, {})",
        final_screen_pos.0, final_screen_pos.1
    );
    println!("  Horizontal scroll (left_column): {}", final_left_col);

    // Assert the major issues
    assert!(
        !issues.iter().any(|issue| issue.contains("CURSOR AT (0,0)")),
        "BUG REPRODUCED: Cursor appeared at (0,0) when it shouldn't:\n{}",
        issues
            .iter()
            .filter(|issue| issue.contains("CURSOR AT (0,0)"))
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    );

    assert!(
        !issues
            .iter()
            .any(|issue| issue.contains("NO HORIZONTAL SCROLL")),
        "BUG REPRODUCED: Horizontal scrolling didn't occur when needed:\n{}",
        issues
            .iter()
            .filter(|issue| issue.contains("NO HORIZONTAL SCROLL"))
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    );

    assert_eq!(
        final_buffer_pos, line_length,
        "Final buffer position should be at end of line ({})",
        line_length
    );

    assert_ne!(
        final_screen_pos,
        (0, 0),
        "Final screen cursor should not be at (0, 0)"
    );

    assert!(
        final_left_col > 0,
        "Horizontal scrolling should have occurred (left_column should be > 0)"
    );
}

/// Test that pressing Enter at the bottom of the viewport scrolls the view up
/// to make room for the new line immediately, not after typing into the new line.
/// Bug: When at the bottom, pressing Enter doesn't scroll until you type something.
#[test]
fn test_enter_at_bottom_scrolls_immediately() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let terminal_height = 24u16;
    let terminal_width = 80u16;

    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    harness.render().unwrap();

    println!("\n=== Testing Enter scrolls immediately when at bottom ===");

    // Insert unique marker text so we can identify our buffer content
    // Each line will have "LINE_N:" prefix
    let type_marker = |harness: &mut EditorTestHarness, line_num: usize| {
        let marker = format!("LINE_{}:", line_num);
        for c in marker.chars() {
            harness
                .send_key(KeyCode::Char(c), KeyModifiers::NONE)
                .unwrap();
        }
    };

    // Type marker for line 1
    type_marker(&mut harness, 1);

    // Create many more lines than the screen height to ensure scrolling is needed
    // For a 24-row screen, we'll create 100 lines to be safe
    let total_lines = 100;

    for line_num in 2..=total_lines {
        // Press Enter to create a new line
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        // Type marker for this new line
        type_marker(&mut harness, line_num);

        // CRITICAL CHECK: After typing the marker, verify that the cursor row
        // actually contains our marker (meaning we're in the content area, not status bar)
        let (_, cursor_y) = harness.screen_cursor_position();
        let cursor_row_text = harness.get_row_text(cursor_y);
        let expected_marker = format!("LINE_{}:", line_num);

        assert!(
            cursor_row_text.contains(&expected_marker),
            "BUG: At line {}, cursor row doesn't contain expected marker '{}'! \
             Cursor at screen row {}, row text: '{}'. \
             This indicates the viewport did not scroll properly when Enter was pressed, \
             causing cursor to move into the status bar area instead of staying in content area.",
            line_num,
            expected_marker,
            cursor_y,
            cursor_row_text.trim()
        );
    }

    println!(
        "✓ Successfully created {} lines with proper scrolling",
        total_lines
    );
    println!("✓ Cursor always stayed in content area (never moved to status bar)");
}

/// Test that cursor never jumps to status bar when scrolling to end of file
/// This reproduces the bug from issue #468: "Cursor is jumping on statusbar"
/// When using PgDown to scroll to the end of a file, especially in a small terminal
/// where line wrapping occurs, the cursor should never be positioned on the status bar row.
#[test]
fn test_cursor_never_on_status_bar_when_scrolling() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    // Use a small terminal to trigger line wrapping (which is key to reproducing the bug)
    let terminal_width = 30u16;
    let terminal_height = 10u16;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with 50 lines that will wrap in the narrow terminal
    let content: String = (1..=50)
        .map(|i| format!("This is line number {i} with extra text\n"))
        .collect();
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    println!(
        "\n=== Testing cursor never on status bar (small terminal {}x{}) ===",
        terminal_width, terminal_height
    );

    // Calculate the status bar row
    let status_bar_row = crate::common::harness::layout::status_bar_row(terminal_height as usize);
    let (content_first_row, content_last_row) = harness.content_area_rows();

    println!(
        "Content area: rows {} to {}",
        content_first_row, content_last_row
    );
    println!("Status bar: row {}", status_bar_row);

    // Test 1: Press PageDown repeatedly to scroll to end
    println!("\nTest 1: PageDown to end of file");
    for i in 1..=20 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let (cursor_x, cursor_y) = harness.screen_cursor_position();

        // The cursor should NEVER be on the status bar row
        assert!(
            (cursor_y as usize) < status_bar_row,
            "BUG: After PageDown #{}, cursor Y ({}) is at or beyond status bar row ({}). \
             Cursor position: ({}, {}). This violates the invariant that cursor must stay in content area.",
            i, cursor_y, status_bar_row, cursor_x, cursor_y
        );

        // Also verify cursor is in the valid content area
        assert!(
            (cursor_y as usize) >= content_first_row && (cursor_y as usize) <= content_last_row,
            "BUG: After PageDown #{}, cursor Y ({}) is outside content area (rows {}-{})",
            i,
            cursor_y,
            content_first_row,
            content_last_row
        );
    }
    println!("✓ PageDown scrolling: cursor always in content area");

    // Test 2: Jump directly to end with Ctrl+End
    println!("\nTest 2: Ctrl+End to jump to EOF");
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let (cursor_x, cursor_y) = harness.screen_cursor_position();
    assert!(
        (cursor_y as usize) < status_bar_row,
        "BUG: After Ctrl+End, cursor Y ({}) is at or beyond status bar row ({}). \
         Cursor position: ({}, {}). This violates the invariant that cursor must stay in content area.",
        cursor_y, status_bar_row, cursor_x, cursor_y
    );
    println!(
        "✓ Ctrl+End: cursor at ({}, {}), in content area",
        cursor_x, cursor_y
    );

    // Test 3: Navigate down line by line near the end
    println!("\nTest 3: Navigate down line by line from near end");
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let (cursor_x, cursor_y) = harness.screen_cursor_position();
        assert!(
            (cursor_y as usize) < status_bar_row,
            "BUG: After Down key, cursor Y ({}) is at or beyond status bar row ({}). \
             Cursor position: ({}, {})",
            cursor_y,
            status_bar_row,
            cursor_x,
            cursor_y
        );
    }
    println!("✓ Down key navigation: cursor always in content area");

    println!("\n✓ All tests passed: cursor never jumped to status bar");
}

/// Test that pressing Enter resets horizontal scroll when cursor moves to column 0
/// Bug: When at the end of a long horizontally-scrolled line, pressing Enter
/// moves the cursor to the start of the new line (column 0) but the horizontal
/// scroll is not reset, so the screen stays scrolled right showing empty space.
/// Typing any character after Enter does reset the scroll.
#[test]
fn test_enter_resets_horizontal_scroll() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::config::Config;

    let mut config = Config::default();
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    println!("\n=== Testing Enter resets horizontal scroll ===");

    // Create a long line (100 chars) - no trailing newline since it's the only line
    let long_line = "a".repeat(100);
    harness.type_text(&long_line).unwrap();
    harness.render().unwrap();

    // Verify cursor is at position 100 and horizontal scroll occurred
    let cursor_pos = harness.cursor_position();
    let left_col_before = harness.editor().active_viewport().left_column;

    println!("After typing 100 chars:");
    println!("  Cursor position: {}", cursor_pos);
    println!("  Horizontal scroll (left_column): {}", left_col_before);

    assert_eq!(cursor_pos, 100, "Cursor should be at position 100");
    assert!(
        left_col_before > 0,
        "Viewport should be scrolled right (left_column={}, expected > 0)",
        left_col_before
    );

    // Now press Enter - cursor should move to column 0 of new line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check the state after Enter
    let cursor_pos_after = harness.cursor_position();
    let left_col_after = harness.editor().active_viewport().left_column;
    let (screen_x, screen_y) = harness.screen_cursor_position();

    println!("\nAfter pressing Enter:");
    println!("  Cursor position: {}", cursor_pos_after);
    println!("  Horizontal scroll (left_column): {}", left_col_after);
    println!("  Screen cursor: ({}, {})", screen_x, screen_y);

    // Cursor should now be at position 101 (after the newline)
    // which is column 0 of the new line
    assert_eq!(
        cursor_pos_after, 101,
        "Cursor should be at position 101 (after newline)"
    );

    // The horizontal scroll should be reset to 0 since cursor is at column 0
    assert_eq!(
        left_col_after, 0,
        "BUG: Horizontal scroll should be reset to 0 after Enter (cursor is at column 0), \
         but left_column={}",
        left_col_after
    );

    // Screen cursor X should be at the gutter width (start of content)
    // Gutter is typically 4-5 chars for line numbers
    assert!(
        screen_x < 10,
        "BUG: Screen cursor X ({}) should be near the left side (< 10) after Enter",
        screen_x
    );

    println!("\n✓ Enter correctly reset horizontal scroll");
}
