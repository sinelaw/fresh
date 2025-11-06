// End-to-end tests for mouse interactions and scrollbar functionality

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::io::Write;

/// Test scrollbar rendering in a single split
#[test]
fn test_scrollbar_renders() {
    // Initialize tracing
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env().add_directive(tracing::Level::TRACE.into()))
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type enough content to make the buffer scrollable
    for i in 1..=50 {
        harness
            .type_text(&format!("Line {} with some content\n", i))
            .unwrap();
    }

    harness.render().unwrap();

    // Check that scrollbar characters (│ or █) exist in the rendered output
    let screen = harness.screen_to_string();

    let has_track = screen.contains('│');
    let has_thumb = screen.contains('█');

    assert!(
        has_track || has_thumb,
        "Scrollbar should be visible (looking for │ or █ characters)"
    );
}

/// Test scrollbar rendering in multiple splits
#[test]
fn test_scrollbar_in_multiple_splits() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type content in first split
    for i in 1..=30 {
        harness
            .type_text(&format!("Left pane line {}\n", i))
            .unwrap();
    }

    // Create vertical split
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();

    // Type content in second split
    for i in 1..=30 {
        harness
            .type_text(&format!("Right pane line {}\n", i))
            .unwrap();
    }

    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Both splits should have scrollbars
    // With vertical split, each pane gets about half the width
    // Both should have scrollbars on their right edge
    assert!(
        screen.contains('│') || screen.contains('█'),
        "Scrollbars should be visible in split views"
    );
}

/// Test clicking on scrollbar to jump to position
#[test]
fn test_scrollbar_click_jump() {
    // Initialize tracing
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env().add_directive(tracing::Level::TRACE.into()))
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a long document
    for i in 1..=100 {
        harness
            .type_text(&format!("Line {} content here\n", i))
            .unwrap();
    }

    // Scroll to top using multiple PageUp presses
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness.send_key_repeat(KeyCode::PageUp, KeyModifiers::NONE, 10).unwrap();

    harness.render().unwrap();

    // Should now be at or near the top
    let initial_top_line = harness.top_line_number();

    // Click on scrollbar near the bottom (rightmost column, near bottom of screen)
    // Terminal is 80x24, scrollbar is at column 79, click at row 20
    harness.mouse_click(79, 20).unwrap();

    harness.render().unwrap();

    // Should have scrolled down
    let new_top_line = harness.top_line_number();
    assert!(
        new_top_line > initial_top_line + 10,
        "Clicking near bottom of scrollbar should scroll down significantly (was {}, now {})",
        initial_top_line,
        new_top_line
    );
}

/// Test dragging scrollbar to scroll
#[test]
fn test_scrollbar_drag() {
    // Initialize tracing
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env().add_directive(tracing::Level::TRACE.into()))
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a long document
    for i in 1..=100 {
        harness
            .type_text(&format!("Line {} with text\n", i))
            .unwrap();
    }

    // Scroll to top using multiple PageUp presses
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness.send_key_repeat(KeyCode::PageUp, KeyModifiers::NONE, 10).unwrap();

    harness.render().unwrap();

    // Should now be at or near the top
    let initial_top_line = harness.top_line_number();

    // Drag scrollbar from top to middle
    // Terminal is 80x24, scrollbar is at column 79
    // Drag from row 2 to row 12 (middle of content area)
    harness.mouse_drag(79, 2, 79, 12).unwrap();

    harness.render().unwrap();

    // Should have scrolled down
    let new_top_line = harness.top_line_number();
    assert!(
        new_top_line > initial_top_line + 10,
        "Dragging scrollbar should scroll content (was {}, now {})",
        initial_top_line,
        new_top_line
    );
}

/// Test mouse click in editor positions cursor
#[test]
fn test_mouse_click_positions_cursor() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some content
    harness.type_text("First line\n").unwrap();
    harness.type_text("Second line\n").unwrap();
    harness.type_text("Third line\n").unwrap();

    harness.render().unwrap();

    // Cursor should be at end
    let buffer_len = harness.buffer_len();
    assert_eq!(harness.cursor_position(), buffer_len);

    // Click on second line, near the beginning (accounting for line numbers gutter)
    // Line numbers take about 6 columns, so click at column 10 (in text area)
    // Row 2 is first line of content (after tabs header)
    harness.mouse_click(10, 2).unwrap();

    harness.render().unwrap();

    // Cursor should have moved to the clicked position
    // It should be somewhere in the first line now
    let new_pos = harness.cursor_position();
    assert!(
        new_pos < 15,
        "Cursor should be near start after clicking first line (position: {})",
        new_pos
    );
}

/// Test mouse click to switch focus between splits
#[test]
fn test_mouse_click_switches_split_focus() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type in first split
    harness.type_text("Left content").unwrap();
    let first_buffer_content = harness.get_buffer_content();

    // Create vertical split (Alt+v)
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();

    harness.render().unwrap();

    // Both splits show the same buffer initially, so type more to differentiate
    harness.type_text(" plus right").unwrap();
    harness.render().unwrap();

    let second_buffer_content = harness.get_buffer_content();

    // Verify they're different (second one has more content)
    assert!(second_buffer_content.contains("plus right"));
    assert!(!first_buffer_content.contains("plus right"));

    // Click in the left split area (accounting for split width)
    // With vertical split, left gets ~40 columns
    harness.mouse_click(10, 5).unwrap();
    harness.render().unwrap();

    // After clicking and typing, content should update in the clicked split
    // This is a basic test - just verify no crash
    let screen = harness.screen_to_string();
    assert!(!screen.is_empty(), "Editor should still be rendering after split click");
}

/// Test mouse interaction with file explorer
#[test]
fn test_mouse_click_file_explorer() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Create some test files
    let project_dir = harness.project_dir().unwrap();
    let test_file = project_dir.join("test.txt");
    fs::write(&test_file, "Test file content").unwrap();

    // Open file explorer (Ctrl+b)
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Click in the file explorer area
    // File explorer is on the left (30% of width)
    // Click at various positions in the explorer
    for row in 3..8 {
        harness.mouse_click(10, row).unwrap();
        harness.render().unwrap();
    }

    // Verify no crash
    let screen = harness.screen_to_string();
    assert!(
        !screen.is_empty(),
        "Editor should still be rendering after file explorer clicks"
    );
}

/// Test clicking in file explorer to open a file
#[test]
fn test_mouse_open_file_from_explorer() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Create a test file
    let project_dir = harness.project_dir().unwrap();
    let test_file = project_dir.join("clickme.txt");
    fs::write(&test_file, "I was opened by clicking!").unwrap();

    // Open file explorer (Ctrl+b)
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Try clicking in the file explorer area
    harness.mouse_click(10, 4).unwrap();
    harness.render().unwrap();

    // Verify no crash
    let screen = harness.screen_to_string();
    assert!(
        !screen.is_empty(),
        "Editor should still be functional after file explorer interaction"
    );
}

/// Test scrollbar visibility with small buffers
#[test]
fn test_scrollbar_with_small_buffer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type just a few lines (no scrolling needed)
    harness.type_text("Line 1\n").unwrap();
    harness.type_text("Line 2\n").unwrap();
    harness.type_text("Line 3\n").unwrap();

    harness.render().unwrap();

    // Scrollbar should still be rendered even with small content
    let screen = harness.screen_to_string();

    let has_track = screen.contains('│');
    let has_thumb = screen.contains('█');

    assert!(
        has_track || has_thumb,
        "Scrollbar should be visible even with small buffers"
    );
}

/// Test that clicking outside all interactive areas doesn't crash
#[test]
fn test_mouse_click_outside_areas() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("Some content").unwrap();
    harness.render().unwrap();

    // Click in various places
    // Status bar area (bottom row)
    harness.mouse_click(40, 23).unwrap();
    harness.render().unwrap();

    // Tab bar area (top row)
    harness.mouse_click(40, 0).unwrap();
    harness.render().unwrap();

    // Should not crash
    let screen = harness.screen_to_string();
    assert!(!screen.is_empty(), "Editor should still be functional");
}

/// Test scrollbar in horizontal split
#[test]
fn test_scrollbar_horizontal_split() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type content in first split
    for i in 1..=30 {
        harness
            .type_text(&format!("Top pane line {}\n", i))
            .unwrap();
    }

    // Create horizontal split (Alt+h)
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::ALT)
        .unwrap();

    // Type content in second split
    for i in 1..=30 {
        harness
            .type_text(&format!("Bottom pane line {}\n", i))
            .unwrap();
    }

    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Both splits should have scrollbars on their right edge
    // Check that scrollbar characters exist
    let scrollbar_chars = screen.matches('│').count() + screen.matches('█').count();
    assert!(
        scrollbar_chars > 10,
        "Should have scrollbar characters in horizontal splits"
    );
}

/// Test cursor positioning with horizontal scroll
#[test]
fn test_mouse_click_with_horizontal_scroll() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a very long line
    harness
        .type_text("This is a very long line that should extend beyond the visible width of the terminal and require horizontal scrolling to see all of it completely")
        .unwrap();

    harness.render().unwrap();

    // Scroll right to see more of the line
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness.send_key_repeat(KeyCode::Right, KeyModifiers::NONE, 10).unwrap();

    // Click somewhere in the visible area
    harness.mouse_click(40, 2).unwrap();
    harness.render().unwrap();

    // Should not crash and cursor should be positioned
    let pos = harness.cursor_position();
    assert!(
        pos < 200,
        "Cursor should be positioned in the line after click"
    );
}

/// Test clicking between line numbers (gutter) and text
#[test]
fn test_mouse_click_in_gutter() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("Line 1\n").unwrap();
    harness.type_text("Line 2\n").unwrap();
    harness.type_text("Line 3\n").unwrap();

    harness.render().unwrap();

    let initial_pos = harness.cursor_position();

    // Click in the gutter area (line numbers, around column 3)
    harness.mouse_click(3, 3).unwrap();
    harness.render().unwrap();

    // Clicking in gutter should not move cursor (or might, depending on implementation)
    // At minimum, it should not crash
    let screen = harness.screen_to_string();
    assert!(!screen.is_empty(), "Editor should still work after gutter click");
}

/// Test dragging scrollbar to top
#[test]
fn test_scrollbar_drag_to_top() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a long document
    for i in 1..=100 {
        harness
            .type_text(&format!("Line {}\n", i))
            .unwrap();
    }

    harness.render().unwrap();

    // Cursor is at bottom, so we're scrolled down
    // Get current position
    let scrolled_pos = harness.top_line_number();
    assert!(scrolled_pos > 70, "Should be scrolled down initially");

    // Drag scrollbar to top
    harness.mouse_drag(79, 12, 79, 2).unwrap();
    harness.render().unwrap();

    // Should have scrolled up
    let new_pos = harness.top_line_number();
    assert!(
        new_pos < scrolled_pos - 10,
        "Dragging up should scroll up (was {}, now {})",
        scrolled_pos,
        new_pos
    );
}

/// Test clicking in editor after using file explorer
#[test]
fn test_mouse_focus_after_file_explorer() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Type some content
    harness.type_text("Editor content").unwrap();
    harness.render().unwrap();

    // Open file explorer
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Click in the editor area (right side, not in file explorer)
    harness.mouse_click(50, 10).unwrap();
    harness.render().unwrap();

    // Verify no crash and editor still works
    let screen = harness.screen_to_string();
    assert!(
        !screen.is_empty() && screen.contains("Editor content"),
        "Editor should still be functional after clicking"
    );
}

/// Helper function to extract scrollbar thumb info from screen
/// Returns (thumb_start_row, thumb_end_row, thumb_size)
fn extract_scrollbar_thumb_info(screen: &str, terminal_width: u16, terminal_height: u16) -> (usize, usize, usize) {
    let lines: Vec<&str> = screen.lines().collect();
    let scrollbar_col = terminal_width - 1; // Rightmost column

    let mut thumb_start = None;
    let mut thumb_end = None;

    // Skip first line (tab bar) and last line (status bar)
    // Content area is from row 1 to terminal_height - 2
    for (row_idx, line) in lines.iter().enumerate().skip(1).take((terminal_height - 2) as usize) {
        let chars: Vec<char> = line.chars().collect();
        if (scrollbar_col as usize) < chars.len() {
            let ch = chars[scrollbar_col as usize];
            if ch == '█' {
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
            (start, end, thumb_size)
        }
        _ => (0, 0, 0)
    }
}

/// Test dragging scrollbar all the way to bottom to reproduce bug where:
/// 1. Scrollbar won't drag to absolute bottom (one char short)
/// 2. Cursor appears beyond EOF (on status bar)
/// 3. After typing, screen corrects itself
#[test]
fn test_scrollbar_drag_to_absolute_bottom() {
    // Initialize tracing
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env().add_directive(tracing::Level::TRACE.into()))
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a document with 100 lines
    for i in 1..=100 {
        harness
            .type_text(&format!("Line {} content\n", i))
            .unwrap();
    }

    // Scroll to top
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness.send_key_repeat(KeyCode::PageUp, KeyModifiers::NONE, 20).unwrap();

    harness.render().unwrap();

    let buffer_len = harness.buffer_len();
    println!("Buffer length: {} bytes", buffer_len);

    // Verify we're at the top
    let initial_top_line = harness.top_line_number();
    println!("Initial top line: {}", initial_top_line);
    assert!(initial_top_line <= 1, "Should be at top of document");

    // Terminal is 24 rows: row 0 = tab bar, rows 1-22 = content area (22 rows), row 23 = status bar
    // Scrollbar occupies rows 1-22 (22 rows total)
    let scrollbar_bottom_row = 22;

    // Drag scrollbar from top (row 1) to absolute bottom (row 22)
    println!("\nDragging scrollbar from row 1 to row {}", scrollbar_bottom_row);
    harness.mouse_drag(79, 1, 79, scrollbar_bottom_row as u16).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Extract scrollbar thumb information
    let (thumb_start, thumb_end, thumb_size) = extract_scrollbar_thumb_info(&screen, 80, 24);
    let top_line_after_drag = harness.top_line_number();

    println!("\nAfter drag to bottom:");
    println!("  Thumb start row: {}", thumb_start);
    println!("  Thumb end row: {}", thumb_end);
    println!("  Thumb size: {} chars", thumb_size);
    println!("  Scrollbar bottom row: {}", scrollbar_bottom_row);
    println!("  Top line number: {}", top_line_after_drag);
    println!("  Total lines in file: 100");
    println!("  Viewport height: 22 rows");
    println!("  Expected max top line: {} (100 - 22)", 100 - 22);

    // INVARIANT: When scrolled to EOF, thumb bottom should be at scrollbar bottom
    println!("\nChecking invariant: thumb_end ({}) should equal scrollbar_bottom_row ({})", thumb_end, scrollbar_bottom_row);

    // Check cursor position - it should not be beyond buffer
    let cursor_pos = harness.cursor_position();
    println!("Cursor position: {} bytes", cursor_pos);
    println!("Buffer length: {} bytes", buffer_len);

    // VERIFY FIX: Scrollbar should reach absolute bottom when dragged to row 22
    assert_eq!(
        thumb_end, scrollbar_bottom_row,
        "Scrollbar thumb should reach absolute bottom (row {}) when dragged to bottom, but ended at row {}",
        scrollbar_bottom_row, thumb_end
    );

    // VERIFY FIX: Viewport should be scrolled to maximum position
    assert_eq!(
        top_line_after_drag, 78,
        "Viewport should be scrolled to line 78 (100 - 22), but is at line {}",
        top_line_after_drag
    );

    assert!(
        cursor_pos <= buffer_len,
        "Cursor should not be beyond buffer end. Cursor at {}, buffer length {}",
        cursor_pos, buffer_len
    );
}
