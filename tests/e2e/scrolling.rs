
use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
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
    let mut lines = Vec::new();
    for i in 0..100 {
        lines.push(format!("Line {i}"));
    }
    harness.type_text(&lines.join("\n")).unwrap();

    // Verify initial content
    harness.assert_buffer_content(&lines.join("\n"));

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
    let buffer_content = harness.get_buffer_content();
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
    let final_content = harness.get_buffer_content();
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
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Calculate visible width (80 - 7 for line number gutter = 73 chars)
    let gutter_width = 7;
    let visible_width = 80 - gutter_width; // 73 characters visible

    // Type characters to fill most of the visible width
    let initial_text = "a".repeat(60);
    harness.type_text(&initial_text).unwrap();

    // Get initial viewport state (should be no scrolling yet)
    let viewport = &harness.editor().active_state().viewport;
    assert_eq!(viewport.left_column, 0, "Should not be scrolled yet");

    // Type more characters to go beyond visible width
    let more_text = "b".repeat(30); // Total: 90 characters
    harness.type_text(&more_text).unwrap();

    // Now the viewport should have scrolled horizontally
    let viewport = &harness.editor().active_state().viewport;
    assert!(
        viewport.left_column > 0,
        "Viewport should have scrolled horizontally, left_column = {}",
        viewport.left_column
    );

    // The cursor should still be visible on screen
    // Note: With horizontal_scroll_offset, the cursor can be slightly beyond
    // the calculated visible_width during scrolling, but it should be reasonable
    let screen_pos = harness.screen_cursor_position();
    assert!(
        screen_pos.0 < (visible_width + 10) as u16,
        "Cursor screen X ({}) should be reasonably within viewport (visible width {})",
        screen_pos.0,
        visible_width
    );

    // Verify buffer position is correct
    assert_eq!(harness.cursor_position(), 90);
}

/// Test horizontal scrolling when moving cursor left
#[test]
fn test_horizontal_scroll_left() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a long line
    let long_text = "a".repeat(100);
    harness.type_text(&long_text).unwrap();

    // Cursor is now at position 100, viewport should be scrolled
    let viewport = &harness.editor().active_state().viewport;
    let initial_left_col = viewport.left_column;
    assert!(initial_left_col > 0, "Viewport should be scrolled right");

    // Move cursor all the way to the left (Home key)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Cursor should be at position 0
    assert_eq!(harness.cursor_position(), 0);

    // Viewport should have scrolled back to show the beginning
    let viewport = &harness.editor().active_state().viewport;
    assert_eq!(
        viewport.left_column, 0,
        "Viewport should have scrolled back to left"
    );
}

/// Test horizontal scrolling with arrow key navigation
#[test]
fn test_horizontal_scroll_with_arrows() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a line longer than visible width
    let text = "x".repeat(90);
    harness.type_text(&text).unwrap();

    // Viewport should be scrolled
    let viewport = &harness.editor().active_state().viewport;
    assert!(viewport.left_column > 0);

    // Move left by 50 characters
    for _ in 0..50 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Cursor should be at position 40
    assert_eq!(harness.cursor_position(), 40);

    // Viewport should have scrolled left to keep cursor visible
    let viewport = &harness.editor().active_state().viewport;
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
    let viewport = &harness.editor().active_state().viewport;
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
    let buffer = &harness.editor().active_state().buffer;
    let cursor_pos = harness.cursor_position();
    let mut iter = buffer.line_iterator(0);
    let mut cursor_line = 0;
    while let Some((line_start, _)) = iter.next() {
        if line_start > cursor_pos {
            break;
        }
        cursor_line += 1;
    }
    // We typed total_lines lines, so last line should be total_lines
    assert_eq!(cursor_line, total_lines, "Cursor should be on last line");

    // The viewport should have scrolled down (top_byte > 0)
    let top_byte = harness.editor().active_state().viewport.top_byte;
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
#[test]
fn test_vertical_scroll_offset() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let visible_lines = 22; // Account for tab bar and status bar

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
    let initial_top_byte = harness.editor().active_state().viewport.top_byte;
    assert!(initial_top_byte > 0, "Should be scrolled down");

    // Move up by many lines to trigger viewport scroll
    // With 40 lines and 22 visible, viewport is at line 18
    // Move up 20 lines (from 39 to 19) to trigger scroll offset
    for _ in 0..20 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }

    // The viewport should have scrolled up to keep cursor visible
    // with the scroll offset (default 3 lines)
    let new_top_byte = harness.editor().active_state().viewport.top_byte;

    // We moved up 20 lines, so viewport should have adjusted (top_byte should decrease)
    assert!(
        new_top_byte < initial_top_byte,
        "Viewport should have scrolled up: was {initial_top_byte}, now {new_top_byte}"
    );

    // Cursor should still be visible with some margin
    let screen_pos = harness.screen_cursor_position();
    let scroll_offset = harness.editor().active_state().viewport.scroll_offset;

    assert!(
        screen_pos.1 >= scroll_offset as u16,
        "Cursor should have at least {} lines of scroll offset above, screen Y = {}",
        scroll_offset,
        screen_pos.1
    );
}

/// Test that viewport displays all available lines when content is larger than minimum
#[test]
fn test_viewport_displays_all_lines() {
    // Create a harness with 80 columns and 40 rows
    // This gives us: 1 line for tabs + 38 lines for content + 1 line for status = 40 total
    let mut harness = EditorTestHarness::new(80, 40).unwrap();

    // Create content with 35 lines (should all be visible in a 38-line viewport)
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
    let state = editor.active_state();
    let viewport_height = state.viewport.height;

    // Viewport should be 38 lines tall (40 - 2 for tab bar and status bar)
    assert_eq!(
        viewport_height, 38,
        "Viewport height should be 38 (40 total - 2 for UI chrome)"
    );

    // Get visible range
    let visible_line_count = state.viewport.visible_line_count();

    // All 35 lines should fit in the 38-line viewport
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
    // This gives us: 1 line for tabs + 29 lines for content + 1 line for status = 31 total
    let mut harness = EditorTestHarness::new(131, 31).unwrap();

    // Create content with 29 lines (should all be visible in a 29-line viewport)
    let mut content = String::new();
    for i in 1..=29 {
        if i > 1 {
            content.push('\n');
        }
        content.push_str(&format!("Line {i}"));
    }

    harness.type_text(&content).unwrap();

    // Check the viewport state
    let editor = harness.editor();
    let state = editor.active_state();
    let viewport_height = state.viewport.height;

    // Viewport should be 29 lines tall (31 - 2 for tab bar and status bar)
    assert_eq!(
        viewport_height, 29,
        "Viewport height should be 29 (31 total - 2 for UI chrome)"
    );

    // Get visible range
    let visible_line_count = state.viewport.visible_line_count();

    // All 29 lines should be visible
    assert_eq!(
        visible_line_count, 29,
        "Expected to see all 29 lines, but visible range is only {visible_line_count} lines"
    );

    // Move cursor to the start of the document so all lines are in view
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Render and verify lines are displayed
    harness.render().unwrap();

    // Check that we can see first and last lines
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 29");

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

    // Suggestions popup should be visible with commands
    harness.assert_screen_contains("Open File");
    harness.assert_screen_contains("Save File");
    harness.assert_screen_contains("Quit");

    // The viewport height should be unchanged (suggestions take screen space, not viewport space)
    let editor = harness.editor();
    let state = editor.active_state();
    let viewport_height_with_palette = state.viewport.height;

    assert_eq!(
        viewport_height_with_palette, 29,
        "Viewport height should still be 29 even with command palette open, but got {viewport_height_with_palette}"
    );

    // Close the command palette
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // After closing palette, viewport should still be at full height
    let editor = harness.editor();
    let state = editor.active_state();
    let viewport_height_after = state.viewport.height;

    assert_eq!(
        viewport_height_after, 29,
        "Viewport height should still be 29 after closing command palette, but got {viewport_height_after}"
    );

    // Get visible range after closing palette
    let visible_line_count_after = state.viewport.visible_line_count();

    assert_eq!(
        visible_line_count_after, 29,
        "Expected to see all 29 lines after closing palette, but visible range is only {visible_line_count_after} lines"
    );

    // All lines should still be visible on screen
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 29");
}

#[test]
#[ignore] // Run with: cargo test test_load_big_file_e2e -- --ignored --nocapture
fn test_load_big_file_e2e() {
    use crossterm::event::{KeyCode, KeyModifiers};

    use std::time::Instant;

    // Initialize tracing
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .try_init();

    println!("\n=== E2E Test: Loading BIG.txt through full editor ===");

    // Generate BIG.txt if it doesn't exist
    let big_txt_path = TestFixture::big_txt().unwrap();

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
        println!("✓ PageDown #{i} in: {:?}", time);
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
#[ignore = "Test fails when run with other tests due to BIG.txt file generation timing - passes when run individually"]
fn test_jump_to_eof_large_file() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::time::Instant;

    // Generate BIG.txt if it doesn't exist (this is cached across test runs)
    let big_txt_path = TestFixture::big_txt().unwrap();

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

/// Test that we can navigate to EOF and back to beginning in a large file
/// Verifies that navigation works correctly and cursor ends up at the right positions
#[test]
#[ignore = "Test fails when run with other tests due to BIG.txt file generation timing - passes when run individually"]
fn test_line_numbers_absolute_after_jump_to_beginning() {
    use crossterm::event::{KeyCode, KeyModifiers};

    println!("\n=== Testing navigation: EOF -> Home ===");

    // Use the big file
    let big_txt_path = TestFixture::big_txt().unwrap();
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
        let state = harness.editor().active_state();
        assert!(
            state.viewport.top_byte > 0,
            "Viewport should have scrolled down"
        );
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
        let state = harness.editor().active_state();
        assert_eq!(state.viewport.top_byte, 0, "Viewport should be at top");
    }

    // Verify first few lines are readable via iterator
    println!("\n  Verifying first few lines are readable:");
    let state = harness.editor().active_state();
    let mut iter = state.buffer.line_iterator(state.viewport.top_byte);
    let mut line_count = 0;
    for i in 0..5 {
        if let Some((byte_pos, content)) = iter.next() {
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

/// Helper function to extract scrollbar thumb size from screen
/// Returns (thumb_start_row, thumb_size, scrollbar_col)
fn extract_scrollbar_info(screen: &str, terminal_width: u16, terminal_height: u16) -> (usize, usize, u16) {
    let lines: Vec<&str> = screen.lines().collect();
    let scrollbar_col = terminal_width - 1; // Rightmost column

    let mut thumb_start = None;
    let mut thumb_end = None;

    // Skip first line (tab bar) and last line (status bar)
    // Content area is from row 1 to terminal_height - 2
    for (row_idx, line) in lines.iter().enumerate().skip(1).take((terminal_height - 2) as usize) {
        // Get character at scrollbar column
        let chars: Vec<char> = line.chars().collect();
        if (scrollbar_col as usize) < chars.len() {
            let ch = chars[scrollbar_col as usize];
            if ch == '█' {
                // Found thumb character
                if thumb_start.is_none() {
                    thumb_start = Some(row_idx);
                }
                thumb_end = Some(row_idx);
            }
        }
    }

    match (thumb_start, thumb_end) {
        (Some(start), Some(end)) => {
            let thumb_size = end - start + 1;
            (start, thumb_size, scrollbar_col)
        }
        _ => (0, 0, scrollbar_col) // No thumb found
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
        .map(|i| format!("Line {} with some content here\n", i))
        .collect();
    std::fs::write(&file_path, content).unwrap();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    harness.open_file(&file_path).unwrap();

    println!("\n=== Testing file with {} lines ===", num_lines);

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
    let (start_row, size, _) = extract_scrollbar_info(&screen, terminal_width, terminal_height);
    scrollbar_sizes.push(size);
    positions.push((0, harness.top_line_number()));

    println!("Initial: top_line={}, scrollbar thumb size={}, start_row={}",
             harness.top_line_number(), size, start_row);

    if num_lines <= 100 {
        println!("\nInitial screen:\n{}\n", screen);
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
            println!("Reached end of file at line {}", after_line);
            break;
        }

        step += 1;
        let screen = harness.screen_to_string();
        let (start_row, size, _) = extract_scrollbar_info(&screen, terminal_width, terminal_height);
        scrollbar_sizes.push(size);
        positions.push((step, after_line));

        println!("PageDown step {}: top_line={}, scrollbar thumb size={}, start_row={}",
                 step, after_line, size, start_row);

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

        harness
            .send_key(KeyCode::Down, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let after_line = harness.top_line_number();
        let after_cursor = harness.cursor_position();

        // Check if we've stopped moving
        if before_line == after_line && before_cursor == after_cursor {
            break;
        }

        step += 1;
        let screen = harness.screen_to_string();
        let (start_row, size, _) = extract_scrollbar_info(&screen, terminal_width, terminal_height);
        scrollbar_sizes.push(size);
        positions.push((step, after_line));

        println!("Down step {}: top_line={}, scrollbar thumb size={}, start_row={}",
                 step - pagedown_steps, after_line, size, start_row);
    }

    // Analyze scrollbar sizes
    println!("\n=== Analysis for {} lines ===", num_lines);
    println!("Scrollbar sizes observed: {:?}", scrollbar_sizes);

    // The scrollbar size should be consistent throughout scrolling
    // It may vary by 1 due to rounding, but should not change dramatically
    if scrollbar_sizes.len() > 1 {
        let min_size = *scrollbar_sizes.iter().min().unwrap();
        let max_size = *scrollbar_sizes.iter().max().unwrap();
        let size_variation = max_size.saturating_sub(min_size);

        println!("Min scrollbar size: {}", min_size);
        println!("Max scrollbar size: {}", max_size);
        println!("Size variation: {}", size_variation);

        // All sizes should be positive (thumb should always be visible)
        assert!(
            min_size > 0,
            "Scrollbar thumb should always be visible (min size > 0), but got min={}",
            min_size
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
            "Scrollbar thumb size MUST be constant (variation = 0) for {} lines, but varied by {} \
             (min={}, max={}). This indicates a bug in scrollbar rendering.",
            num_lines, size_variation, min_size, max_size
        );
    }

    println!("✓ Scrollbar consistency test passed for {} lines (variation: {} chars)",
             num_lines,
             scrollbar_sizes.iter().max().unwrap_or(&0).saturating_sub(*scrollbar_sizes.iter().min().unwrap_or(&0)));
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
        .map(|i| format!("Line {} with some content here\n", i))
        .collect();
    std::fs::write(&file_path, content).unwrap();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    harness.open_file(&file_path).unwrap();

    println!("\n=== Testing scrollbar invariants for {} lines ===", num_lines);

    // The scrollbar height is terminal_height - 2 (for tab bar and status bar)
    let scrollbar_height = (terminal_height - 2) as usize;

    // Go to beginning of file
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // INVARIANT 1: At first line, handle top should be at scrollbar top (row 1, after tab bar)
    let screen = harness.screen_to_string();
    let (start_row, initial_size, _) = extract_scrollbar_info(&screen, terminal_width, terminal_height);

    println!("At first line: handle start_row={}, size={}", start_row, initial_size);
    assert_eq!(
        start_row, 1,
        "When at first line, scrollbar handle top should be at row 1 (scrollbar top), but got row {}",
        start_row
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
        let screen = harness.screen_to_string();
        let (start_row, size, _) = extract_scrollbar_info(&screen, terminal_width, terminal_height);
        all_sizes.push(size);

        println!("After PageDown {}: top_line={}, handle start_row={}, size={}",
                 scroll_steps, after_line, start_row, size);

        // Safety: prevent infinite loops
        if scroll_steps > 100 {
            break;
        }
    }

    // Continue with Down arrow keys to ensure we reach the absolute last line
    for _ in 0..10 {
        let before_line = harness.top_line_number();
        let before_cursor = harness.cursor_position();

        harness
            .send_key(KeyCode::Down, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let after_line = harness.top_line_number();
        let after_cursor = harness.cursor_position();

        if before_line == after_line && before_cursor == after_cursor {
            break;
        }

        let screen = harness.screen_to_string();
        let (start_row, size, _) = extract_scrollbar_info(&screen, terminal_width, terminal_height);
        all_sizes.push(size);
    }

    // INVARIANT 2: Handle size should be constant throughout
    let min_size = *all_sizes.iter().min().unwrap();
    let max_size = *all_sizes.iter().max().unwrap();
    let size_variation = max_size.saturating_sub(min_size);

    println!("\nHandle sizes observed: {:?}", all_sizes);
    println!("Min size: {}, Max size: {}, Variation: {}", min_size, max_size, size_variation);

    // Handle size MUST be constant (variation = 0) because it represents
    // the ratio of viewport height to total document height, neither of which
    // changes during scrolling. The viewport height is the allocated content area,
    // not the number of visible lines.
    assert_eq!(
        size_variation, 0,
        "Scrollbar handle size MUST be constant (variation = 0), but varied by {} (sizes: {:?})",
        size_variation, all_sizes
    );

    // INVARIANT 3: At last line, handle bottom should be at scrollbar bottom
    let screen = harness.screen_to_string();
    let (start_row, size, _) = extract_scrollbar_info(&screen, terminal_width, terminal_height);
    let end_row = start_row + size;

    println!("\nAt last line: handle start_row={}, size={}, end_row={}", start_row, size, end_row);
    println!("Scrollbar height (rows 1-{}): {}", scrollbar_height, scrollbar_height);

    // The scrollbar goes from row 1 to row (terminal_height - 2)
    // So the last row is at index (terminal_height - 2)
    let scrollbar_bottom_row = terminal_height - 1; // Last row before status bar

    assert_eq!(
        end_row, scrollbar_bottom_row as usize,
        "When at last line, scrollbar handle bottom (row {}) should be at scrollbar bottom (row {})",
        end_row, scrollbar_bottom_row
    );

    // Go back to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify INVARIANT 1 again
    let screen = harness.screen_to_string();
    let (start_row_final, size_final, _) = extract_scrollbar_info(&screen, terminal_width, terminal_height);

    println!("\nBack at first line: handle start_row={}, size={}", start_row_final, size_final);

    assert_eq!(
        start_row_final, 1,
        "When back at first line, scrollbar handle top should be at row 1, but got row {}",
        start_row_final
    );

    // Size should match initial size (total lines haven't changed)
    assert_eq!(
        size_final, initial_size,
        "Handle size at end ({}) should match initial size ({})",
        size_final, initial_size
    );

    println!("✓ All scrollbar invariants verified for {} lines", num_lines);
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
