// End-to-end tests for mouse interactions and scrollbar functionality

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test scrollbar rendering in a single split
#[test]
fn test_scrollbar_renders() {
    // Initialize tracing
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type enough content to make the buffer scrollable
    let content: String = (1..=50)
        .map(|i| format!("Line {i} with some content\n"))
        .collect();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

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
        harness.type_text(&format!("Left pane line {i}\n")).unwrap();
    }

    // Create vertical split
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();

    // Type content in second split
    for i in 1..=30 {
        harness
            .type_text(&format!("Right pane line {i}\n"))
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
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a long document
    let content: String = (1..=100)
        .map(|i| format!("Line {i} content here\n"))
        .collect();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

    // Scroll to top using multiple PageUp presses
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness
        .send_key_repeat(KeyCode::PageUp, KeyModifiers::NONE, 10)
        .unwrap();

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
        "Clicking near bottom of scrollbar should scroll down significantly (was {initial_top_line}, now {new_top_line})"
    );
}

/// Test dragging scrollbar to scroll
#[test]
fn test_scrollbar_drag() {
    // Initialize tracing
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a long document
    let content: String = (1..=100).map(|i| format!("Line {i} with text\n")).collect();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

    // Scroll to top using multiple PageUp presses
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness
        .send_key_repeat(KeyCode::PageUp, KeyModifiers::NONE, 10)
        .unwrap();

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
        "Dragging scrollbar should scroll content (was {initial_top_line}, now {new_top_line})"
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
        "Cursor should be near start after clicking first line (position: {new_pos})"
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
    assert!(
        !screen.is_empty(),
        "Editor should still be rendering after split click"
    );
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
        harness.type_text(&format!("Top pane line {i}\n")).unwrap();
    }

    // Create horizontal split (Alt+h)
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::ALT)
        .unwrap();

    // Type content in second split
    for i in 1..=30 {
        harness
            .type_text(&format!("Bottom pane line {i}\n"))
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
    harness
        .send_key_repeat(KeyCode::Right, KeyModifiers::NONE, 10)
        .unwrap();

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
    assert!(
        !screen.is_empty(),
        "Editor should still work after gutter click"
    );
}

/// Test dragging scrollbar to top
#[test]
fn test_scrollbar_drag_to_top() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a long document
    let content: String = (1..=100).map(|i| format!("Line {i}\n")).collect();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

    // Move cursor to end to scroll down (loading from file starts at beginning)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
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
        "Dragging up should scroll up (was {scrolled_pos}, now {new_pos})"
    );
}

/// Test scrollbar drag on large file (> 1MB)
/// This test ensures that dragging the scrollbar on large files doesn't hang
/// by iterating through the entire buffer to count lines.
///
/// Bug: Previously, calculate_max_scroll_position() would iterate through all lines
/// in the buffer even for large files, causing a complete hang on multi-GB files.
#[test]
fn test_scrollbar_drag_on_large_file() {
    use std::time::Instant;

    // Get shared large file (61MB)
    let big_txt_path = TestFixture::big_txt_for_test("scrollbar_drag_large_file").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    println!("\n=== Opening 61MB file for scrollbar drag test ===");
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    // Verify we're at the top
    let initial_top_line = harness.top_line_number();
    println!("Initial top line: {}", initial_top_line);

    // Drag scrollbar from near top to middle - this should be instant, not hang
    // Terminal is 80x24, scrollbar is at column 79
    // Drag from row 2 to row 12 (middle of content area)
    println!("\n=== Dragging scrollbar on 61MB file ===");
    let start = Instant::now();
    harness.mouse_drag(79, 2, 79, 12).unwrap();
    let drag_time = start.elapsed();

    harness.render().unwrap();

    println!("✓ Scrollbar drag completed in: {:?}", drag_time);

    // Should have scrolled down
    let new_top_line = harness.top_line_number();
    println!("New top line after drag: {}", new_top_line);

    assert!(
        new_top_line > initial_top_line,
        "Dragging scrollbar should scroll content down (was line {}, now line {})",
        initial_top_line,
        new_top_line
    );

    // The drag should have completed quickly (not hung)
    // We don't assert on time because CI can be slow, but log it for visibility
    println!("✓ Scrollbar drag on large file works without hang");

    // Test dragging back up
    println!("\n=== Dragging scrollbar back up ===");
    let start = Instant::now();
    harness.mouse_drag(79, 12, 79, 4).unwrap();
    let drag_back_time = start.elapsed();

    harness.render().unwrap();

    println!("✓ Scrollbar drag back completed in: {:?}", drag_back_time);

    let final_top_line = harness.top_line_number();
    println!("Final top line: {}", final_top_line);

    assert!(
        final_top_line < new_top_line,
        "Dragging scrollbar up should scroll content up (was line {}, now line {})",
        new_top_line,
        final_top_line
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
fn extract_scrollbar_thumb_info(
    screen: &str,
    terminal_width: u16,
    terminal_height: u16,
) -> (usize, usize, usize) {
    let lines: Vec<&str> = screen.lines().collect();
    let scrollbar_col = terminal_width - 1; // Rightmost column

    let mut thumb_start = None;
    let mut thumb_end = None;

    // Skip first line (tab bar) and last line (status bar)
    // Content area is from row 1 to terminal_height - 2
    for (row_idx, line) in lines
        .iter()
        .enumerate()
        .skip(1)
        .take((terminal_height - 2) as usize)
    {
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
        _ => (0, 0, 0),
    }
}

/// Test that dragging the scrollbar updates the cursor position
/// Bug: When dragging the scrollbar, the cursor stays at its old position
/// even though the viewport has scrolled. The cursor should be moved to
/// somewhere within the newly visible area.
#[test]
fn test_scrollbar_drag_updates_cursor_position() {
    // Initialize tracing
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a document with 100 lines
    let content: String = (1..=100).map(|i| format!("Line {i} content\n")).collect();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

    // Move cursor to the beginning of the document
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let initial_cursor_pos = harness.cursor_position();
    let initial_top_line = harness.top_line_number();

    println!("\nInitial state:");
    println!("  Cursor position: {initial_cursor_pos} bytes");
    println!("  Top line: {initial_top_line}");

    // Drag scrollbar from top to near bottom
    // This should scroll the viewport down significantly
    println!("\nDragging scrollbar from row 2 to row 18");
    harness.mouse_drag(79, 2, 79, 18).unwrap();
    harness.render().unwrap();

    let cursor_pos_after_drag = harness.cursor_position();
    let top_line_after_drag = harness.top_line_number();
    let top_byte_after_drag = harness.top_byte();

    println!("\nAfter scrollbar drag:");
    println!("  Cursor position: {cursor_pos_after_drag} bytes");
    println!("  Top line: {top_line_after_drag}");
    println!("  Top byte: {top_byte_after_drag}");
    println!("  Viewport scrolled by: {} lines", top_line_after_drag - initial_top_line);

    // VERIFY: Viewport should have scrolled down
    assert!(
        top_line_after_drag > initial_top_line + 20,
        "Viewport should have scrolled down significantly (was line {initial_top_line}, now line {top_line_after_drag})"
    );

    // VERIFY: Cursor should have moved to be within the visible area
    // The cursor should no longer be at the beginning of the file
    // It should be somewhere near the scrolled viewport position
    assert!(
        cursor_pos_after_drag > initial_cursor_pos,
        "Cursor should have moved from position {initial_cursor_pos} after scrollbar drag, but is still at {cursor_pos_after_drag}"
    );

    // VERIFY: Cursor should be at the top of the visible area (or close to it)
    // When scrollbar is dragged, the cursor is moved to top_byte
    assert_eq!(
        cursor_pos_after_drag, top_byte_after_drag,
        "Cursor position {cursor_pos_after_drag} should be at the top of the viewport (top_byte={top_byte_after_drag})"
    );
}

/// Test dragging scrollbar all the way to bottom to reproduce bug where:
/// 1. Scrollbar won't drag to absolute bottom (one char short)
/// 2. Cursor appears beyond EOF (on status bar)
/// 3. After typing, screen corrects itself
#[test]
fn test_scrollbar_drag_to_absolute_bottom() {
    // Initialize tracing
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a document with 100 lines
    let content: String = (1..=100).map(|i| format!("Line {i} content\n")).collect();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

    // Scroll to top
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness
        .send_key_repeat(KeyCode::PageUp, KeyModifiers::NONE, 20)
        .unwrap();

    harness.render().unwrap();

    let buffer_len = harness.buffer_len();
    println!("Buffer length: {buffer_len} bytes");

    // Verify we're at the top
    let initial_top_line = harness.top_line_number();
    println!("Initial top line: {initial_top_line}");
    assert!(initial_top_line <= 1, "Should be at top of document");

    // Terminal is 24 rows: row 0 = tab bar, rows 1-22 = content area (22 rows), row 23 = status bar
    // Scrollbar occupies rows 1-22 (22 rows total)
    let scrollbar_bottom_row = 22;

    // Drag scrollbar from top (row 1) to absolute bottom (row 22)
    println!("\nDragging scrollbar from row 1 to row {scrollbar_bottom_row}");
    harness
        .mouse_drag(79, 1, 79, scrollbar_bottom_row as u16)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Extract scrollbar thumb information
    let (thumb_start, thumb_end, thumb_size) = extract_scrollbar_thumb_info(&screen, 80, 24);
    let top_line_after_drag = harness.top_line_number();

    println!("\nAfter drag to bottom:");
    println!("  Thumb start row: {thumb_start}");
    println!("  Thumb end row: {thumb_end}");
    println!("  Thumb size: {thumb_size} chars");
    println!("  Scrollbar bottom row: {scrollbar_bottom_row}");
    println!("  Top line number: {top_line_after_drag}");
    println!("  Total lines in file: 100");
    println!("  Viewport height: 22 rows");
    println!("  Expected max top line: {} (100 - 22)", 100 - 22);

    // INVARIANT: When scrolled to EOF, thumb bottom should be at scrollbar bottom
    println!("\nChecking invariant: thumb_end ({thumb_end}) should equal scrollbar_bottom_row ({scrollbar_bottom_row})");

    // Check cursor position - it should not be beyond buffer
    let cursor_pos = harness.cursor_position();
    println!("Cursor position: {cursor_pos} bytes");
    println!("Buffer length: {buffer_len} bytes");

    // VERIFY FIX: Scrollbar should reach absolute bottom when dragged to row 22
    assert_eq!(
        thumb_end, scrollbar_bottom_row,
        "Scrollbar thumb should reach absolute bottom (row {scrollbar_bottom_row}) when dragged to bottom, but ended at row {thumb_end}"
    );

    // VERIFY FIX: Viewport should be scrolled to maximum position
    assert_eq!(
        top_line_after_drag, 78,
        "Viewport should be scrolled to line 78 (100 - 22), but is at line {top_line_after_drag}"
    );

    assert!(
        cursor_pos <= buffer_len,
        "Cursor should not be beyond buffer end. Cursor at {cursor_pos}, buffer length {buffer_len}"
    );
}
