// End-to-end tests for mouse interactions and scrollbar functionality

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test scrollbar rendering in a single split
#[test]
fn test_scrollbar_renders() {
    // Initialize tracing
    use tracing_subscriber::EnvFilter;
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

    // Check that scrollbar is visible (rendered with background colors in rightmost column)
    // For 80-width terminal, scrollbar is at column 79
    assert!(
        harness.has_scrollbar_at_column(79),
        "Scrollbar should be visible in rightmost column"
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

    // Both splits should have scrollbars
    // With vertical split, each pane gets about half the width
    // The rightmost split's scrollbar is at column 79
    assert!(
        harness.has_scrollbar_at_column(79),
        "Scrollbar should be visible in rightmost split"
    );
}

/// Test clicking on scrollbar to jump to position
#[test]
fn test_scrollbar_click_jump() {
    // Initialize tracing
    use tracing_subscriber::EnvFilter;
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
    use tracing_subscriber::EnvFilter;
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
    let first_buffer_content = harness.get_buffer_content().unwrap();

    // Create vertical split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.render().unwrap();

    // Both splits show the same buffer initially, so type more to differentiate
    harness.type_text(" plus right").unwrap();
    harness.render().unwrap();

    let second_buffer_content = harness.get_buffer_content().unwrap();

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

    // Open file explorer (Ctrl+E)
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
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

    // Open file explorer (Ctrl+E)
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
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
    // Check for scrollbar at rightmost column (79 for 80-width terminal)
    assert!(
        harness.has_scrollbar_at_column(79),
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

    // Both splits should have scrollbars on their right edge (column 79)
    assert!(
        harness.has_scrollbar_at_column(79),
        "Should have scrollbar in horizontal splits"
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

    let _initial_pos = harness.cursor_position();

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

/// Test clicking past end of line positions cursor at end of that line
#[test]
fn test_mouse_click_past_end_of_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a short line
    harness.type_text("hello\n").unwrap();
    harness.type_text("world\n").unwrap();

    harness.render().unwrap();

    // Cursor is at the end of buffer (after "world\n")
    let buffer_len = harness.buffer_len();
    assert_eq!(harness.cursor_position(), buffer_len);

    // Click way past the end of "hello" on the first content line (row 2)
    // Line numbers take about 6 columns, so click at column 50 (well past "hello" which ends around column 11)
    harness.mouse_click(50, 2).unwrap();
    harness.render().unwrap();

    // Cursor should be at position 5, which is the newline after "hello"
    // "hello\n" = positions 0-5, where 5 is the newline
    let new_pos = harness.cursor_position();
    assert_eq!(
        new_pos, 5,
        "Clicking past end of line should position cursor at end of that line (expected 5, got {new_pos})"
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
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
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

/// Helper function to extract scrollbar thumb info from harness.
/// Scrollbars are rendered with background colors, not characters.
/// Returns (thumb_start_row, thumb_end_row, thumb_size)
fn extract_scrollbar_thumb_info(
    harness: &EditorTestHarness,
    terminal_width: u16,
    _terminal_height: u16,
) -> (usize, usize, usize) {
    let scrollbar_col = terminal_width - 1; // Rightmost column
    let (content_first_row, content_last_row) = harness.content_area_rows();

    let mut thumb_start = None;
    let mut thumb_end = None;

    // Scan the content area for scrollbar thumb cells
    for row in content_first_row..=content_last_row {
        if harness.is_scrollbar_thumb_at(scrollbar_col, row as u16) {
            if thumb_start.is_none() {
                thumb_start = Some(row);
            }
            thumb_end = Some(row);
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
    use tracing_subscriber::EnvFilter;
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
    println!(
        "  Viewport scrolled by: {} lines",
        top_line_after_drag - initial_top_line
    );

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
    use tracing_subscriber::EnvFilter;
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

    // Get content area bounds from harness (accounts for menu bar, tab bar, status bar)
    let (content_first_row, content_last_row) = harness.content_area_rows();
    let viewport_height = harness.viewport_height();

    // Scrollbar occupies the content area rows
    let scrollbar_bottom_row = content_last_row;

    // Drag scrollbar from top to absolute bottom
    println!("\nDragging scrollbar from row {content_first_row} to row {scrollbar_bottom_row}");
    harness
        .mouse_drag(
            79,
            content_first_row as u16,
            79,
            scrollbar_bottom_row as u16,
        )
        .unwrap();
    harness.render().unwrap();

    // Extract scrollbar thumb information (using background color detection)
    let (thumb_start, thumb_end, thumb_size) = extract_scrollbar_thumb_info(&harness, 80, 24);
    let top_line_after_drag = harness.top_line_number();

    println!("\nAfter drag to bottom:");
    println!("  Thumb start row: {thumb_start}");
    println!("  Thumb end row: {thumb_end}");
    println!("  Thumb size: {thumb_size} chars");
    println!("  Scrollbar bottom row: {scrollbar_bottom_row}");
    println!("  Top line number: {top_line_after_drag}");
    println!("  Total lines in file: 100");
    println!("  Viewport height: {viewport_height} rows");
    // Max top line = total_lines - viewport_height + 1
    // With 100 lines and viewport 20: top_line 81 shows lines 81-100 (the last 20 lines)
    let expected_max_top_line = 100 - viewport_height + 1;
    println!("  Expected max top line: {expected_max_top_line} (100 - {viewport_height} + 1)");

    // INVARIANT: When scrolled to EOF, thumb bottom should be at scrollbar bottom
    println!("\nChecking invariant: thumb_end ({thumb_end}) should equal scrollbar_bottom_row ({scrollbar_bottom_row})");

    // Check cursor position - it should not be beyond buffer
    let cursor_pos = harness.cursor_position();
    println!("Cursor position: {cursor_pos} bytes");
    println!("Buffer length: {buffer_len} bytes");

    // VERIFY FIX: Scrollbar should reach near absolute bottom when dragged to bottom
    // Allow 1-row tolerance due to rounding in scrollbar calculations
    let diff = (thumb_end as i32 - scrollbar_bottom_row as i32).abs();
    assert!(
        diff <= 1,
        "Scrollbar thumb should reach near absolute bottom (row {scrollbar_bottom_row}) when dragged to bottom, but ended at row {thumb_end}"
    );

    // VERIFY FIX: Viewport should be scrolled to maximum position
    assert_eq!(
        top_line_after_drag, expected_max_top_line,
        "Viewport should be scrolled to line {expected_max_top_line} (100 - {viewport_height}), but is at line {top_line_after_drag}"
    );

    assert!(
        cursor_pos <= buffer_len,
        "Cursor should not be beyond buffer end. Cursor at {cursor_pos}, buffer length {buffer_len}"
    );
}

/// Test mouse drag on horizontal split separator to resize
#[test]
fn test_horizontal_split_separator_drag_resize() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    // Create horizontal split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split horiz").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Get the separator info - should have exactly one separator for horizontal split
    let separators = harness.editor().get_separator_areas().to_vec();
    assert_eq!(
        separators.len(),
        1,
        "Should have exactly one separator after creating horizontal split"
    );

    let (split_id, direction, sep_x, sep_y, sep_length) = separators[0];
    assert_eq!(
        direction,
        fresh::model::event::SplitDirection::Horizontal,
        "Should be a horizontal split"
    );

    // Get initial ratio
    let initial_ratio = harness.editor().get_split_ratio(split_id.into()).unwrap();
    assert!(
        (initial_ratio - 0.5).abs() < 0.1,
        "Initial ratio should be close to 0.5, got {initial_ratio}"
    );

    // Drag the separator down (increases top split size)
    // Start from the middle of the separator
    let start_col = sep_x + sep_length / 2;
    let start_row = sep_y;
    let end_row = sep_y + 3; // Drag down by 3 rows

    harness
        .mouse_drag(start_col, start_row, start_col, end_row)
        .unwrap();

    // Check that the ratio increased (top split got bigger)
    let new_ratio = harness.editor().get_split_ratio(split_id.into()).unwrap();
    assert!(
        new_ratio > initial_ratio,
        "Ratio should increase after dragging separator down. Was {initial_ratio}, now {new_ratio}"
    );

    // Wait to avoid double-click detection
    std::thread::sleep(double_click_delay);

    // Drag the separator up (decreases top split size)
    let separators_after = harness.editor().get_separator_areas().to_vec();
    let (_, _, sep_x_new, sep_y_new, sep_length_new) = separators_after[0];

    let start_col = sep_x_new + sep_length_new / 2;
    let start_row = sep_y_new;
    let end_row = sep_y_new.saturating_sub(5); // Drag up by 5 rows

    harness
        .mouse_drag(start_col, start_row, start_col, end_row)
        .unwrap();

    // Check that the ratio decreased (top split got smaller)
    let final_ratio = harness.editor().get_split_ratio(split_id.into()).unwrap();
    assert!(
        final_ratio < new_ratio,
        "Ratio should decrease after dragging separator up. Was {new_ratio}, now {final_ratio}"
    );
}

/// Test mouse drag on vertical split separator to resize
#[test]
fn test_vertical_split_separator_drag_resize() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    // Create vertical split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Get the separator info - should have exactly one separator for vertical split
    let separators = harness.editor().get_separator_areas().to_vec();
    assert_eq!(
        separators.len(),
        1,
        "Should have exactly one separator after creating vertical split"
    );

    let (split_id, direction, sep_x, sep_y, sep_length) = separators[0];
    assert_eq!(
        direction,
        fresh::model::event::SplitDirection::Vertical,
        "Should be a vertical split"
    );

    // Get initial ratio
    let initial_ratio = harness.editor().get_split_ratio(split_id.into()).unwrap();
    assert!(
        (initial_ratio - 0.5).abs() < 0.1,
        "Initial ratio should be close to 0.5, got {initial_ratio}"
    );

    // Drag the separator right (increases left split size)
    let start_col = sep_x;
    let start_row = sep_y + sep_length / 2;
    let end_col = sep_x + 10; // Drag right by 10 columns

    harness
        .mouse_drag(start_col, start_row, end_col, start_row)
        .unwrap();

    // Check that the ratio increased (left split got bigger)
    let new_ratio = harness.editor().get_split_ratio(split_id.into()).unwrap();
    assert!(
        new_ratio > initial_ratio,
        "Ratio should increase after dragging separator right. Was {initial_ratio}, now {new_ratio}"
    );

    // Wait to avoid double-click detection
    std::thread::sleep(double_click_delay);

    // Drag the separator left (decreases left split size)
    let separators_after = harness.editor().get_separator_areas().to_vec();
    let (_, _, sep_x_new, sep_y_new, sep_length_new) = separators_after[0];

    let start_col = sep_x_new;
    let start_row = sep_y_new + sep_length_new / 2;
    let end_col = sep_x_new.saturating_sub(15); // Drag left by 15 columns

    harness
        .mouse_drag(start_col, start_row, end_col, start_row)
        .unwrap();

    // Check that the ratio decreased (left split got smaller)
    let final_ratio = harness.editor().get_split_ratio(split_id.into()).unwrap();
    assert!(
        final_ratio < new_ratio,
        "Ratio should decrease after dragging separator left. Was {new_ratio}, now {final_ratio}"
    );
}

/// Test that separator drag respects minimum and maximum ratios
#[test]
fn test_split_separator_drag_respects_limits() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    // Create horizontal split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split horiz").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let separators = harness.editor().get_separator_areas().to_vec();
    let (split_id, _, sep_x, sep_y, sep_length) = separators[0];

    // Try to drag separator way beyond reasonable limits
    let start_col = sep_x + sep_length / 2;

    // Drag extremely far down (should clamp to max 0.9)
    harness
        .mouse_drag(start_col, sep_y, start_col, sep_y + 100)
        .unwrap();

    let max_ratio = harness.editor().get_split_ratio(split_id.into()).unwrap();
    assert!(
        max_ratio <= 0.9,
        "Ratio should not exceed 0.9, got {max_ratio}"
    );
    assert!(
        max_ratio >= 0.8,
        "Ratio should be close to maximum after extreme drag down, got {max_ratio}"
    );

    // Wait to avoid double-click detection
    std::thread::sleep(double_click_delay);

    // Drag extremely far up (should clamp to min 0.1)
    let separators_after = harness.editor().get_separator_areas().to_vec();
    let (_, _, _, sep_y_after, _) = separators_after[0];

    harness
        .mouse_drag(start_col, sep_y_after, start_col, 0)
        .unwrap();

    let min_ratio = harness.editor().get_split_ratio(split_id.into()).unwrap();
    assert!(
        min_ratio >= 0.1,
        "Ratio should not be less than 0.1, got {min_ratio}"
    );
    assert!(
        min_ratio <= 0.2,
        "Ratio should be close to minimum after extreme drag up, got {min_ratio}"
    );
}

/// Test that hovering over a tab close button changes its color
#[test]
fn test_tab_close_button_hover_changes_color() {
    use crate::common::harness::layout;
    use ratatui::style::Color;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a second buffer so we have two tabs
    harness.new_buffer().unwrap();
    harness.render().unwrap();

    // Get the tab row content to find the × position
    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    println!("Tab row content: '{}'", tab_row);
    println!("Tab row length: {}", tab_row.len());

    // Find the position of the first × in the tab bar
    let x_pos = tab_row
        .find('×')
        .expect("Could not find × close button in tab bar");
    println!("Found × at position: {}", x_pos);

    // Get the color of the × before hovering
    let style_before = harness.get_cell_style(x_pos as u16, layout::TAB_BAR_ROW as u16);
    let fg_before = style_before.and_then(|s| s.fg);
    println!("Color before hover: {:?}", fg_before);

    // Now hover over the × position
    harness
        .mouse_move(x_pos as u16, layout::TAB_BAR_ROW as u16)
        .unwrap();

    // Get the color after hovering
    let style_after = harness.get_cell_style(x_pos as u16, layout::TAB_BAR_ROW as u16);
    let fg_after = style_after.and_then(|s| s.fg);
    println!("Color after hover: {:?}", fg_after);

    // The hover color should be the tab_close_hover_fg (red-ish: RGB(255, 100, 100))
    // At minimum, the color should have changed
    assert_ne!(
        fg_before, fg_after,
        "Tab close button color should change on hover. Before: {:?}, After: {:?}",
        fg_before, fg_after
    );

    // Verify it's the expected hover color (red-ish)
    match fg_after {
        Some(Color::Rgb(r, g, b)) => {
            assert!(
                r > 200 && g < 150 && b < 150,
                "Expected red-ish hover color, got RGB({}, {}, {})",
                r,
                g,
                b
            );
        }
        other => panic!("Expected RGB color for hover, got {:?}", other),
    }
}

/// Test that tab hover detection matches the actual tab positions on screen
/// This test verifies that when you hover at various X positions in the tab bar,
/// the hover detection correctly identifies which tab (if any) is under the cursor
#[test]
fn test_tab_hover_position_accuracy() {
    use crate::common::harness::layout;
    use ratatui::style::Color;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create two buffers
    harness.new_buffer().unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    println!("Tab row: '{}'", tab_row);

    // Find all × positions (there should be 2, one for each tab)
    let x_positions: Vec<usize> = tab_row.match_indices('×').map(|(i, _)| i).collect();
    println!("× positions: {:?}", x_positions);
    assert_eq!(
        x_positions.len(),
        2,
        "Expected 2 close buttons (one per tab)"
    );

    // For each close button position, verify that hovering there:
    // 1. Changes the color at that position (hover is detected)
    // 2. Does NOT change the color at the other close button position (hover is position-specific)
    for (idx, &x_pos) in x_positions.iter().enumerate() {
        println!(
            "\n--- Testing close button {} at position {} ---",
            idx, x_pos
        );

        // Reset by moving mouse away
        harness.mouse_move(0, 0).unwrap();

        // Get colors of both × before hovering
        let colors_before: Vec<_> = x_positions
            .iter()
            .map(|&pos| {
                harness
                    .get_cell_style(pos as u16, layout::TAB_BAR_ROW as u16)
                    .and_then(|s| s.fg)
            })
            .collect();
        println!("Colors before hover: {:?}", colors_before);

        // Hover over the current close button
        harness
            .mouse_move(x_pos as u16, layout::TAB_BAR_ROW as u16)
            .unwrap();

        // Get colors of both × after hovering
        let colors_after: Vec<_> = x_positions
            .iter()
            .map(|&pos| {
                harness
                    .get_cell_style(pos as u16, layout::TAB_BAR_ROW as u16)
                    .and_then(|s| s.fg)
            })
            .collect();
        println!("Colors after hover: {:?}", colors_after);

        // The hovered button should have changed to red
        let hovered_color = colors_after[idx];
        match hovered_color {
            Some(Color::Rgb(r, g, b)) => {
                assert!(
                    r > 200 && g < 150 && b < 150,
                    "Close button {} at position {} should be red when hovered, got RGB({}, {}, {})",
                    idx, x_pos, r, g, b
                );
            }
            other => panic!(
                "Expected RGB color for hovered button {}, got {:?}",
                idx, other
            ),
        }

        // The other button should NOT have changed to red (should still be the original color)
        for (other_idx, &other_color) in colors_after.iter().enumerate() {
            if other_idx != idx {
                // It should NOT be the hover color
                if let Some(Color::Rgb(r, g, b)) = other_color {
                    assert!(
                        !(r > 200 && g < 150 && b < 150),
                        "Close button {} should NOT be red when button {} is hovered, but got RGB({}, {}, {})",
                        other_idx, idx, r, g, b
                    );
                }
            }
        }
    }
}

/// Test drag-to-select text in the editor
#[test]
fn test_drag_to_select_text() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Load a file with some content
    let content = "Hello World\nSecond line here\nThird line\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // Verify no initial selection
    assert!(
        !harness.has_selection(),
        "Should have no selection initially"
    );

    // Get content area info - first row of content after tab bar
    let (content_first_row, _) = harness.content_area_rows();

    // Drag from start of "Hello" to end of "World" on first line
    // Looking at the screen: "    1 │ Hello World" - the gutter is ~8 chars
    // Text starts at around column 8-9
    let start_col = 9;
    let end_col = 19;
    let row = content_first_row as u16;

    harness.mouse_drag(start_col, row, end_col, row).unwrap();
    harness.render().unwrap();

    // Should now have a selection
    assert!(harness.has_selection(), "Should have selection after drag");

    // Get the selected text
    let selected = harness.get_selected_text();
    println!("Selected text: '{}'", selected);

    // The selection should contain part of "Hello World"
    assert!(!selected.is_empty(), "Selected text should not be empty");

    // Verify the selection range exists
    let range = harness.get_selection_range();
    assert!(range.is_some(), "Should have a selection range");
    let range = range.unwrap();
    println!("Selection range: {} to {}", range.start, range.end);
    assert!(
        range.end > range.start,
        "Selection end ({}) should be greater than start ({})",
        range.end,
        range.start
    );
}

/// Test drag-to-select across multiple lines
#[test]
fn test_drag_to_select_multiple_lines() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Load content with multiple lines
    let content = "Line one\nLine two\nLine three\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();

    // Drag from first line to third line
    // The gutter is ~8 chars, so text starts at column 8-9
    let start_col = 9;
    let start_row = content_first_row as u16;
    let end_col = 14;
    let end_row = content_first_row as u16 + 2; // Third line

    println!(
        "Dragging from ({}, {}) to ({}, {})",
        start_col, start_row, end_col, end_row
    );
    harness
        .mouse_drag(start_col, start_row, end_col, end_row)
        .unwrap();
    harness.render().unwrap();

    // Should have selection
    assert!(
        harness.has_selection(),
        "Should have selection after multi-line drag"
    );

    // Selection should span multiple lines (contain newline)
    let selected = harness.get_selected_text();
    println!("Selected text: '{}'", selected);

    // The selection should span across lines
    let range = harness.get_selection_range();
    assert!(range.is_some(), "Should have selection range");
    let range = range.unwrap();
    println!("Selection range: {} to {}", range.start, range.end);

    // Multi-line selection should have a reasonable span
    assert!(
        range.end - range.start > 5,
        "Multi-line selection should span more than 5 bytes"
    );
}

/// Test that selection clears on mouse click
#[test]
fn test_click_clears_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    let content = "Some text to select\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    // Create a selection via drag (gutter is ~8 chars)
    harness.mouse_drag(9, row, 17, row).unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have selection after drag");

    // Wait to avoid double-click detection
    std::thread::sleep(double_click_delay);

    // Click somewhere else to clear selection
    harness.mouse_click(12, row).unwrap();
    harness.render().unwrap();

    // Selection should be cleared (anchor should equal cursor position)
    // Note: A single click sets both cursor and anchor to the same position
    let range = harness.get_selection_range();
    if let Some(range) = range {
        assert_eq!(
            range.start, range.end,
            "After click, selection should be empty (start={}, end={})",
            range.start, range.end
        );
    }
}

/// Test that shift+click extends selection
#[test]
fn test_shift_click_extends_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    let content = "hello world test content\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    let gutter_width = 8; // Approximate gutter width

    // Click to position cursor at start of "hello" (after gutter)
    harness.mouse_click(gutter_width, row).unwrap();
    harness.render().unwrap();

    let pos_after_click = harness.cursor_position();
    assert_eq!(
        pos_after_click, 0,
        "Cursor should be at start after clicking at gutter edge"
    );

    // Wait to avoid double-click detection
    std::thread::sleep(double_click_delay);

    // Shift+click at position 12 (around "world")
    harness.mouse_shift_click(gutter_width + 12, row).unwrap();
    harness.render().unwrap();

    // Should now have a selection from 0 to 12
    assert!(
        harness.has_selection(),
        "Should have selection after shift+click"
    );

    let selection_range = harness.get_selection_range();
    assert!(
        selection_range.is_some(),
        "Selection range should be available"
    );
    let range = selection_range.unwrap();
    assert_eq!(
        range.start, 0,
        "Selection should start at original click position"
    );
    assert!(
        range.end > range.start,
        "Selection should extend to shift+click position"
    );
}

/// Test that shift+click can shrink selection when clicking before anchor
#[test]
fn test_shift_click_can_shrink_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    let content = "hello world test content\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    let gutter_width = 8;

    // Create initial selection via drag from position 5 to 15
    harness
        .mouse_drag(gutter_width + 5, row, gutter_width + 15, row)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have selection after drag");
    let initial_range = harness.get_selection_range().unwrap();
    let initial_size = initial_range.end - initial_range.start;
    assert!(initial_size > 0, "Initial selection should have size > 0");

    std::thread::sleep(double_click_delay);

    // Shift+click at position 10 (within original selection) to shrink it
    harness.mouse_shift_click(gutter_width + 10, row).unwrap();
    harness.render().unwrap();

    // Still should have selection, but potentially different range
    assert!(
        harness.has_selection(),
        "Should still have selection after shift+click"
    );
}

/// Test tab hover with real files (which have line numbers, actual filenames, etc.)
/// This more closely matches real-world usage
#[test]
fn test_tab_hover_with_real_files() {
    use crate::common::harness::layout;
    use ratatui::style::Color;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Load real files (creates line numbers, actual filename in tab)
    let fixture1 = TestFixture::new("test1.txt", "Hello from file 1\nLine 2\n").unwrap();
    let fixture2 = TestFixture::new("test2.txt", "Hello from file 2\nLine 2\n").unwrap();

    harness.open_file(&fixture1.path).unwrap();
    harness.open_file(&fixture2.path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Full screen:\n{}", screen);

    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    println!("\nTab row: '{}'", tab_row);

    // Find all × positions
    let x_positions: Vec<usize> = tab_row.match_indices('×').map(|(i, _)| i).collect();
    println!("× positions: {:?}", x_positions);
    assert_eq!(x_positions.len(), 2, "Expected 2 close buttons");

    // Test hovering on the first close button
    let first_x = x_positions[0];
    println!("\nHovering at position {} (first ×)", first_x);

    // Get color before hover
    let style_before = harness.get_cell_style(first_x as u16, layout::TAB_BAR_ROW as u16);
    let fg_before = style_before.and_then(|s| s.fg);
    println!("Color before: {:?}", fg_before);

    // Hover
    harness
        .mouse_move(first_x as u16, layout::TAB_BAR_ROW as u16)
        .unwrap();

    // Get color after hover
    let style_after = harness.get_cell_style(first_x as u16, layout::TAB_BAR_ROW as u16);
    let fg_after = style_after.and_then(|s| s.fg);
    println!("Color after: {:?}", fg_after);

    // Should have changed to red
    assert_ne!(fg_before, fg_after, "Color should change on hover");
    match fg_after {
        Some(Color::Rgb(r, g, b)) => {
            assert!(
                r > 200 && g < 150 && b < 150,
                "Expected red-ish hover color, got RGB({}, {}, {})",
                r,
                g,
                b
            );
        }
        other => panic!("Expected RGB color, got {:?}", other),
    }
}

/// Test that mouse hover over editor text tracks the position
#[test]
fn test_mouse_hover_tracks_position() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Load some content
    let content = "fn main() {\n    let x = 42;\n}\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // Get content area info
    let (content_first_row, _) = harness.content_area_rows();

    // Initially no hover state
    assert!(
        harness.editor().get_mouse_hover_state().is_none(),
        "Should have no hover state initially"
    );

    // Move mouse over the text area (after gutter, which is ~8 chars)
    let text_col = 12; // Should be over "main"
    let text_row = content_first_row as u16;

    harness.mouse_move(text_col, text_row).unwrap();

    // Should now have hover state tracking the position
    let hover_state = harness.editor().get_mouse_hover_state();
    assert!(
        hover_state.is_some(),
        "Should have hover state after moving mouse over text"
    );

    let (byte_pos, screen_x, screen_y) = hover_state.unwrap();
    assert_eq!(screen_x, text_col, "Screen X should match mouse position");
    assert_eq!(screen_y, text_row, "Screen Y should match mouse position");
    assert!(
        byte_pos < content.len(),
        "Byte position {} should be within buffer (len {})",
        byte_pos,
        content.len()
    );
}

/// Test that mouse hover state is cleared when mouse moves away from editor
#[test]
fn test_mouse_hover_clears_when_leaving_editor() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "Hello World\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();

    // Move mouse over text
    harness.mouse_move(12, content_first_row as u16).unwrap();

    assert!(
        harness.editor().get_mouse_hover_state().is_some(),
        "Should have hover state over text"
    );

    // Move mouse to status bar (bottom row, outside editor content)
    harness.mouse_move(40, 23).unwrap();

    assert!(
        harness.editor().get_mouse_hover_state().is_none(),
        "Hover state should be cleared when mouse leaves editor content"
    );
}

/// Test that mouse hover state updates when position changes
#[test]
fn test_mouse_hover_updates_on_position_change() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "First line of text\nSecond line here\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();

    // Move to first position
    harness.mouse_move(12, content_first_row as u16).unwrap();

    let state1 = harness.editor().get_mouse_hover_state();
    assert!(state1.is_some(), "Should have hover state");
    let (pos1, _, _) = state1.unwrap();

    // Move to a different position (second line)
    harness
        .mouse_move(12, content_first_row as u16 + 1)
        .unwrap();

    let state2 = harness.editor().get_mouse_hover_state();
    assert!(state2.is_some(), "Should still have hover state");
    let (pos2, _, _) = state2.unwrap();

    // Position should have changed
    assert_ne!(
        pos1, pos2,
        "Byte position should change when moving to different line"
    );
}

/// Test that moving mouse to gutter clears hover state
#[test]
fn test_mouse_hover_clears_in_gutter() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "Some code here\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();

    // Move to text area first
    harness.mouse_move(15, content_first_row as u16).unwrap();

    assert!(
        harness.editor().get_mouse_hover_state().is_some(),
        "Should have hover state over text"
    );

    // Move to gutter (line numbers area, column 3)
    harness.mouse_move(3, content_first_row as u16).unwrap();

    assert!(
        harness.editor().get_mouse_hover_state().is_none(),
        "Hover state should be cleared when mouse is in gutter"
    );
}

/// Test that force_check_mouse_hover triggers hover request
#[test]
fn test_force_check_mouse_hover() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "let variable = 123;\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();

    // Move to text area
    harness.mouse_move(12, content_first_row as u16).unwrap();

    assert!(
        harness.editor().get_mouse_hover_state().is_some(),
        "Should have hover state"
    );

    // Force check should return true (would trigger hover if LSP was available)
    let triggered = harness.editor_mut().force_check_mouse_hover();
    assert!(
        triggered,
        "force_check_mouse_hover should return true when hover state exists"
    );

    // Second call should return false (already sent)
    let triggered_again = harness.editor_mut().force_check_mouse_hover();
    assert!(
        !triggered_again,
        "Second force_check should return false (already sent)"
    );
}

/// Test that hover state is preserved when staying at same position
#[test]
fn test_mouse_hover_same_position_preserves_state() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "test content\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let col = 12;
    let row = content_first_row as u16;

    // Move to position
    harness.mouse_move(col, row).unwrap();

    let state1 = harness.editor().get_mouse_hover_state();
    assert!(state1.is_some(), "Should have hover state");
    let (pos1, _, _) = state1.unwrap();

    // Move to same position again (should preserve state, not reset timer)
    harness.mouse_move(col, row).unwrap();

    let state2 = harness.editor().get_mouse_hover_state();
    assert!(state2.is_some(), "Should still have hover state");
    let (pos2, _, _) = state2.unwrap();

    assert_eq!(
        pos1, pos2,
        "Position should be preserved when staying at same spot"
    );
}

/// Test that clicking below the last line of a short file positions cursor
/// on the last line at the clicked column (or end of line), not at 0,0.
///
/// Bug fix: Issue #283 - clicking below the last visible line in a short file
/// was causing the cursor to jump to position 0,0 instead of staying on the
/// last line at an appropriate column.
#[test]
fn test_mouse_click_below_last_line_positions_on_last_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a short file with only 3 lines - this will have lots of empty
    // space below the content in the 24-row terminal
    let content = "First line\nSecond line\nThird line";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // Get content area bounds
    let (content_first_row, content_last_row) = harness.content_area_rows();
    println!(
        "Content area: rows {} to {}",
        content_first_row, content_last_row
    );

    // The file has 3 lines, so content occupies rows:
    // - content_first_row: "First line"
    // - content_first_row + 1: "Second line"
    // - content_first_row + 2: "Third line"
    // Rows below content_first_row + 2 are empty space below the file

    // Click well below the last line of content (e.g., row 15 in a 24-row terminal)
    // This is clicking in empty space below the file content
    // Use a column that would be in the middle of the text area (after the gutter)
    let click_row = content_first_row as u16 + 10; // Well below the 3 lines of content
    let click_col = 15; // In the text area, after the gutter (~8 chars)

    println!("Clicking at row {}, col {}", click_row, click_col);
    harness.mouse_click(click_col, click_row).unwrap();
    harness.render().unwrap();

    let cursor_pos = harness.cursor_position();
    println!("Cursor position after click: {}", cursor_pos);

    // The cursor should NOT be at position 0 (start of file)
    // It should be on the last line (which starts at byte position 24: "First line\n" = 11 + "Second line\n" = 12 + "Third" = 5)
    // "First line\n" = 11 bytes, "Second line\n" = 12 bytes, so third line starts at byte 23
    let third_line_start = 23; // "First line\n" (11) + "Second line\n" (12) = 23
    let content_len = content.len(); // 34 bytes total

    assert!(
        cursor_pos >= third_line_start,
        "Cursor should be on the last line (byte >= {}), but was at position {}. \
         Bug: clicking below last line should NOT jump to position 0.",
        third_line_start,
        cursor_pos
    );

    assert!(
        cursor_pos <= content_len,
        "Cursor position {} should be within buffer (len {})",
        cursor_pos,
        content_len
    );

    println!(
        "SUCCESS: Cursor is on the last line at position {}",
        cursor_pos
    );
}

/// Test that double-click is only detected when both clicks are at the same position.
/// If the user clicks at position A, then quickly clicks at position B (within the
/// double-click time threshold), it should NOT be treated as a double-click.
#[test]
fn test_double_click_requires_same_position() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Load content with multiple words on a line for double-click word selection
    let content = "hello world goodbye\nsecond line here\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    // Get gutter width so we know where text starts
    // Gutter is typically around 8 characters (line numbers + separator)
    let gutter_width = 8;

    // Position A: "hello" starts at column gutter_width
    let pos_a_col = gutter_width + 2; // Over "hello"

    // Position B: "world" starts around column gutter_width + 6
    let pos_b_col = gutter_width + 8; // Over "world"

    // First click at position A
    harness.mouse_click(pos_a_col, row).unwrap();
    harness.render().unwrap();

    // Verify no selection after first click (single click should just position cursor)
    assert!(
        !harness.has_selection() || {
            let range = harness.get_selection_range();
            range.is_none_or(|r| r.start == r.end)
        },
        "Single click should not create a selection"
    );

    // Second click at DIFFERENT position B (but within double-click time threshold)
    // This should NOT trigger double-click behavior (word selection)
    // Instead, it should just position the cursor at the new location
    harness.mouse_click(pos_b_col, row).unwrap();
    harness.render().unwrap();

    // If double-click was incorrectly triggered, it would select a word
    // We should NOT have a word selected since the clicks were at different positions
    let selected_text = harness.get_selected_text();
    println!(
        "Selected text after clicks at different positions: '{}'",
        selected_text
    );

    // There should be no selection (or empty selection) because:
    // - The clicks were at different positions
    // - Even though they were within the double-click time window
    // - This should NOT count as a double-click
    assert!(
        selected_text.is_empty() || selected_text.trim().is_empty(),
        "Clicks at different positions should NOT trigger double-click word selection. \
         Got selected text: '{}'. \
         Bug: Double-click is being detected even when clicks are at different positions.",
        selected_text
    );

    // Now verify that double-click at the SAME position DOES work
    // Wait for double-click timeout to reset (use 3x for safety under CPU load)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 3);
    std::thread::sleep(double_click_delay);

    // Double-click at position A (same position both times)
    harness.mouse_click(pos_a_col, row).unwrap();
    harness.mouse_click(pos_a_col, row).unwrap();
    harness.render().unwrap();

    // Now we SHOULD have word selection because both clicks were at the same position
    let selected_text_same_pos = harness.get_selected_text();
    println!(
        "Selected text after double-click at same position: '{}'",
        selected_text_same_pos
    );

    assert!(
        !selected_text_same_pos.is_empty(),
        "Double-click at same position SHOULD select a word, but got empty selection"
    );
}

/// Test that with blinking_bar cursor style, the first character of a selection
/// has the same background color as the rest of the selection.
///
/// Bug #614: When using blinking_bar cursor style and selecting text by dragging
/// upward and to the left, the first character in the selection is displayed with
/// a different background color than the rest of the selection.
///
/// This test should FAIL until the bug is fixed.
#[test]
fn test_blinking_bar_selection_first_char_color() {
    use fresh::config::{Config, CursorStyle};

    // Create config with blinking_bar cursor style
    let mut config = Config::default();
    config.editor.cursor_style = CursorStyle::BlinkingBar;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Load content with multiple lines
    let content = "Hello World\nThis is line 2\nThis is line 3\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();

    // Get the selection background color from the theme
    let theme = harness.editor().theme();
    let selection_bg = theme.selection_bg;

    // Drag from end of line 2 to start of line 1 (upward and to the left)
    // This reproduces the bug scenario described in issue #614
    // The gutter is ~8 chars, so:
    // - Line 1 "Hello World" starts at column 8
    // - Line 2 "This is line 2" starts at column 8
    let start_col = 22; // End of "This is line 2" (around column 8 + 14)
    let start_row = content_first_row as u16 + 1; // Line 2
    let end_col = 8; // Start of "Hello" (beginning of text after gutter)
    let end_row = content_first_row as u16; // Line 1

    println!(
        "Dragging from ({}, {}) to ({}, {})",
        start_col, start_row, end_col, end_row
    );
    harness
        .mouse_drag(start_col, start_row, end_col, end_row)
        .unwrap();
    harness.render().unwrap();

    // Verify we have a selection
    assert!(harness.has_selection(), "Should have selection after drag");
    let selected_text = harness.get_selected_text();
    println!("Selected text: '{}'", selected_text);

    // Now verify that ALL selected characters have the selection background color
    // The selection should include "Hello World\n" and part of "This is line 2"
    // But critically, the FIRST character 'H' should have the same bg as the rest

    let buffer = harness.buffer();

    // Check the first few characters of the selection (on line 1)
    // Line 1 content starts at column 8 (after gutter "   1 │ ")
    let first_char_col = 8; // 'H' in "Hello"
    let first_char_row = end_row;
    let second_char_col = 9; // 'e' in "Hello"
    let third_char_col = 10; // 'l' in "Hello"

    // Get background colors of the first few selected characters
    let first_char_idx = buffer.index_of(first_char_col, first_char_row);
    let second_char_idx = buffer.index_of(second_char_col, first_char_row);
    let third_char_idx = buffer.index_of(third_char_col, first_char_row);

    let first_char_cell = &buffer.content[first_char_idx];
    let second_char_cell = &buffer.content[second_char_idx];
    let third_char_cell = &buffer.content[third_char_idx];

    println!(
        "First char '{}' bg: {:?}",
        first_char_cell.symbol(),
        first_char_cell.bg
    );
    println!(
        "Second char '{}' bg: {:?}",
        second_char_cell.symbol(),
        second_char_cell.bg
    );
    println!(
        "Third char '{}' bg: {:?}",
        third_char_cell.symbol(),
        third_char_cell.bg
    );
    println!("Expected selection_bg: {:?}", selection_bg);

    // All selected characters should have the selection background color
    assert_eq!(
        first_char_cell.bg, selection_bg,
        "BUG #614: First character '{}' of selection has wrong background color {:?}, expected selection_bg {:?}. \
         With blinking_bar cursor style, the first character displays differently than the rest.",
        first_char_cell.symbol(),
        first_char_cell.bg,
        selection_bg
    );

    assert_eq!(
        second_char_cell.bg,
        selection_bg,
        "Second character '{}' should have selection background",
        second_char_cell.symbol()
    );

    assert_eq!(
        third_char_cell.bg,
        selection_bg,
        "Third character '{}' should have selection background",
        third_char_cell.symbol()
    );

    // Also verify that the first character has the SAME background as the second
    // (this is the core of the bug - they should be identical)
    assert_eq!(
        first_char_cell.bg,
        second_char_cell.bg,
        "BUG #614: First character '{}' bg {:?} differs from second character '{}' bg {:?}. \
         All selected characters should have identical background colors.",
        first_char_cell.symbol(),
        first_char_cell.bg,
        second_char_cell.symbol(),
        second_char_cell.bg
    );
}

/// Test that hover popup position accounts for file explorer width.
///
/// Bug #898: When the file explorer is open and a hover popup is shown,
/// the popup appears on the file explorer area instead of being positioned
/// under the cursor where the user is hovering.
///
/// This test verifies by examining the rendered output that the popup
/// appears at the correct screen position (in the editor area, not the
/// file explorer area).
#[test]
fn test_hover_popup_position_with_file_explorer() {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupKindHint, PopupPositionData,
    };
    use std::time::Duration;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Load some content
    let content = "fn main() {\n    let x = 42;\n}\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // Toggle file explorer ON and wait for it to load
    harness.editor_mut().toggle_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    assert!(
        harness.editor().file_explorer_visible(),
        "File explorer should be visible"
    );

    // Verify file explorer is actually visible in the rendered output
    harness.render().unwrap();
    let screen_before_popup = harness.screen_to_string();
    println!("Screen before popup:\n{}", screen_before_popup);

    // File explorer takes 30% of 80 columns = 24 columns (0-23)
    // Editor content starts at column 24
    let explorer_width = (80.0 * 0.3) as u16; // 24

    // Show a popup positioned BelowCursor - this simulates a hover popup
    // The popup content has a unique marker we can find in the rendered output
    let popup_marker = "HOVER_POPUP_MARKER_898";
    harness.apply_event(Event::ShowPopup {
        popup: PopupData {
            kind: PopupKindHint::Text,
            title: None,
            description: None,
            transient: false,
            content: PopupContentData::Text(vec![popup_marker.to_string()]),
            position: PopupPositionData::BelowCursor,
            width: 30,
            max_height: 5,
            bordered: true,
        },
    });
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with hover popup:\n{}", screen);

    // Find where the popup appears on screen by looking for our marker
    let mut popup_start_col: Option<usize> = None;

    for (row_idx, line) in screen.lines().enumerate() {
        if let Some(col) = line.find(popup_marker) {
            println!(
                "Found popup marker at row {}, col {} (explorer ends at col {})",
                row_idx, col, explorer_width
            );
            popup_start_col = Some(col);
            break;
        }
    }

    let popup_col = popup_start_col.expect("Popup marker should be visible on screen");

    // The popup should appear in the editor area (col >= explorer_width),
    // not in the file explorer area (col < explorer_width)
    assert!(
        popup_col >= explorer_width as usize,
        "BUG #898: Hover popup appears at column {} which is in the file explorer area (cols 0-{}). \
         The popup should be positioned under the cursor in the editor area (cols {}+). \
         Screen:\n{}",
        popup_col,
        explorer_width - 1,
        explorer_width,
        screen
    );
}
