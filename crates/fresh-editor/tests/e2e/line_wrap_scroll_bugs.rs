//! Tests for scrolling bugs when line wrapping is enabled.
//!
//! These tests reproduce two related bugs:
//! 1. Scrollbar appearance: When a file has long lines that wrap to many visual rows,
//!    the scrollbar incorrectly shows as "nothing to scroll" because it calculates
//!    based on logical line count (1) instead of visual row count (many).
//! 2. Mouse wheel scrolling: Scrolling doesn't work because scroll_up/scroll_down
//!    iterate through logical lines, not visual rows.
//!
//! Both issues stem from the same root cause: scrollbar and scroll calculations
//! use logical lines while rendering uses visual (wrapped) lines.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;

/// Helper to create a config with line wrapping enabled
fn config_with_line_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

/// Test that the scrollbar indicates scrollable content when a single long line wraps
/// to more visual rows than fit in the viewport.
///
/// Bug: When line wrapping is enabled and a file contains a single very long line,
/// the scrollbar thumb fills the entire track (indicating "nothing to scroll"),
/// even though the wrapped content extends beyond the viewport.
///
/// This happens because `scrollbar_line_counts()` calculates total_lines as 1
/// (the logical line count), but the visual content spans many more rows.
#[test]
fn test_scrollbar_shows_scrollable_content_with_wrapped_lines() {
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // Create a single line that's very long - it will wrap to many visual rows
    // With 60 width, ~8 gutter, 1 scrollbar = ~51 chars per visual line
    // 1000 chars should wrap to ~20 visual lines, exceeding viewport height
    let long_line = "X".repeat(1000);

    harness.type_text(&long_line).unwrap();
    harness.render().unwrap();

    // Verify it's a single logical line (no newlines)
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        !buffer_content.contains('\n'),
        "Should be a single logical line"
    );
    assert_eq!(buffer_content.len(), 1000, "Line should be 1000 chars");

    let screen = harness.screen_to_string();
    eprintln!("Screen:\n{}", screen);

    // The scrollbar is in the rightmost column
    let scrollbar_col = TERMINAL_WIDTH - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();

    eprintln!(
        "Content area: rows {} to {}, scrollbar at col {}",
        content_first_row, content_last_row, scrollbar_col
    );

    // Count scrollbar thumb and track cells by checking actual background colors
    // The scrollbar uses Gray for thumb (active) and DarkGray for track (active)
    let mut thumb_count = 0;
    let mut track_count = 0;
    let content_height = content_last_row - content_first_row + 1;

    for row in content_first_row..=content_last_row {
        if let Some(style) = harness.get_cell_style(scrollbar_col, row as u16) {
            // Check the actual background color to distinguish thumb from track
            // Thumb is Gray, Track is DarkGray (when active)
            match style.bg {
                Some(ratatui::style::Color::Gray) => thumb_count += 1,
                Some(ratatui::style::Color::DarkGray) => track_count += 1,
                _ => {}
            }
        }
    }

    eprintln!(
        "Scrollbar: thumb={} cells, track={} cells, content_height={}",
        thumb_count, track_count, content_height
    );

    // BUG CHECK: If the thumb fills the entire scrollbar (thumb_count == content_height),
    // that means the scrollbar is showing "nothing to scroll", which is incorrect.
    //
    // With 1000 chars wrapping to ~20 visual rows, and a viewport of ~16 content rows,
    // the thumb should be smaller than the full height to indicate scrollable content.
    assert!(
        thumb_count < content_height,
        "Scrollbar thumb ({} cells) should NOT fill the entire content area ({} rows). \
         This indicates the scrollbar incorrectly thinks there's nothing to scroll. \
         The file has 1000 chars that wrap to ~20 visual lines, which exceeds the viewport.",
        thumb_count,
        content_height
    );

    // Additionally, there should be some track visible (indicating scroll area)
    assert!(
        track_count > 0,
        "Scrollbar track should be visible, indicating there's content to scroll to"
    );
}

/// Test that mouse wheel scrolling actually changes the viewport when line wrapping
/// is enabled and content wraps beyond the viewport.
///
/// Bug: When scrolling with mouse wheel on a file with wrapped lines, the viewport
/// doesn't change because scroll_up/scroll_down move by logical lines (which is
/// just 1 line), not by visual rows.
#[test]
fn test_mouse_wheel_scrolls_wrapped_content() {
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // Create a single very long line that wraps to many visual rows
    // Use a pattern so we can identify which part is visible
    // Pattern: "AAA...BBB...CCC...DDD..." etc, where each letter repeats ~200 chars
    let mut long_line = String::new();
    for ch in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] {
        long_line.push_str(&ch.to_string().repeat(200));
    }

    harness.type_text(&long_line).unwrap();

    // Move cursor to the beginning so we start at top
    harness
        .send_key(
            crossterm::event::KeyCode::Home,
            crossterm::event::KeyModifiers::CONTROL,
        )
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    eprintln!("Screen before scroll:\n{}", screen_before);

    // The beginning of the file should show 'A' characters
    assert!(
        screen_before.contains("AAAA"),
        "Before scrolling, should see the beginning of the line (A chars)"
    );

    // The viewport should NOT show content from the end (like 'H' chars)
    // because that's way past what fits in the viewport
    let has_h_before = screen_before.contains("HHHH");
    eprintln!("Has 'HHHH' before scroll: {}", has_h_before);

    // Get the content area for scrolling
    let (content_first_row, _) = harness.content_area_rows();
    let scroll_col = TERMINAL_WIDTH / 2; // Middle of screen
    let scroll_row = content_first_row as u16 + 5; // A few rows into content

    // Scroll down multiple times with mouse wheel
    // Each scroll should move the viewport down by some visual rows
    for i in 0..20 {
        harness.mouse_scroll_down(scroll_col, scroll_row).unwrap();
        eprintln!("After scroll down #{}", i + 1);
    }

    let screen_after = harness.screen_to_string();
    eprintln!("Screen after scrolling down 20 times:\n{}", screen_after);

    // BUG CHECK: After scrolling down significantly, the visible content should change.
    // If scroll works correctly, we should see different content (later parts of the line).
    //
    // With correct visual-line scrolling:
    // - Scrolling 20 times should move us down ~20+ visual rows
    // - We should see content from later in the line (like 'B', 'C', or beyond)
    //
    // With the bug (logical line scrolling):
    // - Scrolling doesn't actually move the viewport (we're on 1 logical line)
    // - Screen content stays the same

    // Check if the content actually changed
    let content_changed = screen_before != screen_after;

    // Also check if we can see content from later in the line (any letter after A)
    let sees_later_content = screen_after.contains("BBBB")
        || screen_after.contains("CCCC")
        || screen_after.contains("DDDD")
        || screen_after.contains("EEEE")
        || screen_after.contains("FFFF")
        || screen_after.contains("GGGG")
        || screen_after.contains("HHHH");

    // The screen should have changed after scrolling
    assert!(
        content_changed,
        "Screen content should change after scrolling down with mouse wheel. \
         The viewport appears stuck, indicating scroll_down is not moving through \
         visual (wrapped) lines correctly."
    );

    // We should see later parts of the wrapped line
    assert!(
        sees_later_content,
        "After scrolling down, should see content from later in the wrapped line. \
         Screen still shows only the beginning, indicating scrolling is not working with wrapped lines.\n\
         Screen:\n{}", screen_after
    );
}

/// Test that scrollbar drag works correctly with wrapped lines.
///
/// Bug: Clicking on the scrollbar track to jump to a position doesn't work
/// correctly because the position calculation is based on logical lines.
#[test]
fn test_scrollbar_drag_with_wrapped_lines() {
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // Create a long line with identifiable sections
    let mut long_line = String::new();
    for ch in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] {
        long_line.push_str(&ch.to_string().repeat(200));
    }

    harness.type_text(&long_line).unwrap();
    harness
        .send_key(
            crossterm::event::KeyCode::Home,
            crossterm::event::KeyModifiers::CONTROL,
        )
        .unwrap();
    harness.render().unwrap();

    let screen_at_top = harness.screen_to_string();
    eprintln!("Screen at top:\n{}", screen_at_top);

    // Verify we're at the top (see A's)
    assert!(
        screen_at_top.contains("AAAA"),
        "Should start at top showing A characters"
    );

    // Get scrollbar position
    let scrollbar_col = TERMINAL_WIDTH - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();

    // Try to drag scrollbar to the middle/bottom
    let drag_start_row = content_first_row as u16 + 2;
    let drag_end_row = content_last_row as u16 - 2;

    eprintln!(
        "Dragging scrollbar from row {} to row {}",
        drag_start_row, drag_end_row
    );

    harness
        .mouse_drag(scrollbar_col, drag_start_row, scrollbar_col, drag_end_row)
        .unwrap();

    let screen_after_drag = harness.screen_to_string();
    eprintln!("Screen after scrollbar drag:\n{}", screen_after_drag);

    // After dragging to the bottom of the scrollbar, we should see later content
    let sees_later_content = screen_after_drag.contains("DDDD")
        || screen_after_drag.contains("EEEE")
        || screen_after_drag.contains("FFFF")
        || screen_after_drag.contains("GGGG")
        || screen_after_drag.contains("HHHH");

    assert!(
        sees_later_content || screen_at_top != screen_after_drag,
        "Scrollbar drag should move the viewport to show later content in the wrapped line. \
         The viewport didn't change, indicating scrollbar drag doesn't work with line wrapping."
    );
}

/// Test that Page Down/Up work correctly with wrapped lines to scroll by visual pages.
///
/// This tests the keyboard-based scrolling which should also respect visual lines.
#[test]
fn test_page_down_scrolls_visual_rows_with_wrapped_line() {
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // Create a single very long line
    let mut long_line = String::new();
    for ch in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] {
        long_line.push_str(&ch.to_string().repeat(200));
    }

    harness.type_text(&long_line).unwrap();
    harness
        .send_key(
            crossterm::event::KeyCode::Home,
            crossterm::event::KeyModifiers::CONTROL,
        )
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    eprintln!("Screen before PageDown:\n{}", screen_before);

    assert!(
        screen_before.contains("AAAA"),
        "Should start at top showing A characters"
    );

    // Press Page Down
    harness
        .send_key(
            crossterm::event::KeyCode::PageDown,
            crossterm::event::KeyModifiers::NONE,
        )
        .unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    eprintln!("Screen after PageDown:\n{}", screen_after);

    // Page Down should scroll by approximately one viewport height of visual rows
    // This means we should see different content
    let content_changed = screen_before != screen_after;

    assert!(
        content_changed,
        "PageDown should scroll the viewport through visual rows of the wrapped line. \
         The screen content didn't change, indicating PageDown is stuck on a single logical line."
    );
}

/// Test mouse wheel scrolling with a multi-line file where one line is very long.
/// This mimics files like ~/Downloads/zz.txt which have short lines plus one very long line.
#[test]
fn test_mouse_wheel_with_multiline_file_one_long_line() {
    const TERMINAL_WIDTH: u16 = 80;
    const TERMINAL_HEIGHT: u16 = 24;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // Create a file with structure similar to zz.txt:
    // - A few short lines at the start
    // - One very long line that wraps to many visual rows
    // - A couple short lines at the end
    let short_line1 = "Short line 1";
    let short_line2 = "Short line 2";
    let short_line3 = "Short line 3";
    // Long line that will wrap to ~30 visual rows (2000 chars at ~70 chars/row)
    let long_line = "X".repeat(2000);
    let short_line4 = "Short line 4";
    let short_line5 = "Short line 5";

    let content = format!(
        "{}\n{}\n{}\n{}\n{}\n{}",
        short_line1, short_line2, short_line3, long_line, short_line4, short_line5
    );

    for ch in content.chars() {
        if ch == '\n' {
            harness
                .send_key(
                    crossterm::event::KeyCode::Enter,
                    crossterm::event::KeyModifiers::NONE,
                )
                .unwrap();
        } else {
            harness.type_text(&ch.to_string()).unwrap();
        }
    }

    // Move cursor to the beginning
    harness
        .send_key(
            crossterm::event::KeyCode::Home,
            crossterm::event::KeyModifiers::CONTROL,
        )
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    eprintln!("Screen before scroll:\n{}", screen_before);

    // Verify we're at the top (see short lines)
    assert!(
        screen_before.contains("Short line 1"),
        "Should see Short line 1 at top"
    );

    // Get the content area for scrolling
    let (content_first_row, _) = harness.content_area_rows();

    // Scroll down with mouse wheel - should move through visual rows
    for i in 0..10 {
        harness
            .mouse_scroll_down(40, content_first_row as u16 + 5)
            .unwrap();
        eprintln!("After scroll down #{}", i + 1);
    }

    let screen_after = harness.screen_to_string();
    eprintln!("Screen after 10 scrolls:\n{}", screen_after);

    // After scrolling, the content should have changed
    let content_changed = screen_before != screen_after;

    assert!(
        content_changed,
        "Mouse wheel scroll should change viewport in multi-line file with wrapped content.\n\
         Before:\n{}\n\nAfter:\n{}",
        screen_before, screen_after
    );
}

/// Test scrollbar click with a multi-line file where one line is very long (like zz.txt).
/// This specifically tests the case where:
/// - Few logical lines (e.g., 6)
/// - One line wraps to many visual rows
/// - Total visual rows exceed viewport height
/// - But logical line count < viewport height
///
/// Bug: Scrollbar click doesn't work because the scrollbar handler calculates
/// max_scroll_line using logical lines, which results in 0 (no scrolling allowed).
#[test]
fn test_scrollbar_click_with_multiline_file_one_long_line() {
    const TERMINAL_WIDTH: u16 = 80;
    const TERMINAL_HEIGHT: u16 = 24;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // Create a file structure similar to zz.txt:
    // - Line 1: short
    // - Line 2: short
    // - Line 3: short
    // - Line 4: VERY long (wraps to 30+ visual rows)
    // - Line 5: short
    // - Line 6: short
    let short_line1 = "<p>Short line 1</p>";
    let short_line2 = "</p>";
    let short_line3 = "</div>";
    // Long line similar to zz.txt - about 2000 chars that will wrap
    let long_line = format!(
        "<div class=\"content\">{}</div>",
        "CONTENT_".repeat(250) // ~2000 chars
    );
    let short_line5 = "";
    let short_line6 = "";

    let content = format!(
        "{}\n{}\n{}\n{}\n{}\n{}",
        short_line1, short_line2, short_line3, long_line, short_line5, short_line6
    );

    // Type the content
    for ch in content.chars() {
        if ch == '\n' {
            harness
                .send_key(
                    crossterm::event::KeyCode::Enter,
                    crossterm::event::KeyModifiers::NONE,
                )
                .unwrap();
        } else {
            harness.type_text(&ch.to_string()).unwrap();
        }
    }

    // Move cursor to the beginning
    harness
        .send_key(
            crossterm::event::KeyCode::Home,
            crossterm::event::KeyModifiers::CONTROL,
        )
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    eprintln!("Screen before scrollbar click:\n{}", screen_before);

    // Verify we're at the top
    assert!(
        screen_before.contains("Short line 1"),
        "Should see Short line 1 at top"
    );

    // Get scrollbar position
    let scrollbar_col = TERMINAL_WIDTH - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();

    // Click in the lower half of the scrollbar to scroll down
    let click_row = content_last_row as u16 - 3;
    eprintln!(
        "Clicking scrollbar at col={}, row={} (content area: {}-{})",
        scrollbar_col, click_row, content_first_row, content_last_row
    );

    // Simulate clicking on the scrollbar track
    harness.mouse_click(scrollbar_col, click_row).unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    eprintln!("Screen after scrollbar click:\n{}", screen_after);

    // The content should have changed - we should see content from later in the file
    let content_changed = screen_before != screen_after;

    // Should see the long line content (CONTENT_) somewhere, possibly scrolled
    let sees_content_line = screen_after.contains("CONTENT_");

    assert!(
        content_changed,
        "Scrollbar click should change viewport in multi-line file with wrapped content.\n\
         Clicking at row {} should scroll down, but viewport didn't change.\n\
         This indicates scrollbar click is broken for files with few logical lines but many visual rows.\n\
         Before:\n{}\n\nAfter:\n{}",
        click_row, screen_before, screen_after
    );
}

/// Test scrollbar drag with a multi-line file where one line is very long.
#[test]
fn test_scrollbar_drag_with_multiline_file_one_long_line() {
    const TERMINAL_WIDTH: u16 = 80;
    const TERMINAL_HEIGHT: u16 = 24;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // Same file structure as above
    let short_line1 = "<p>Short line 1</p>";
    let short_line2 = "</p>";
    let short_line3 = "</div>";
    let long_line = format!("<div class=\"content\">{}</div>", "CONTENT_".repeat(250));
    let short_line5 = "";
    let short_line6 = "";

    let content = format!(
        "{}\n{}\n{}\n{}\n{}\n{}",
        short_line1, short_line2, short_line3, long_line, short_line5, short_line6
    );

    for ch in content.chars() {
        if ch == '\n' {
            harness
                .send_key(
                    crossterm::event::KeyCode::Enter,
                    crossterm::event::KeyModifiers::NONE,
                )
                .unwrap();
        } else {
            harness.type_text(&ch.to_string()).unwrap();
        }
    }

    harness
        .send_key(
            crossterm::event::KeyCode::Home,
            crossterm::event::KeyModifiers::CONTROL,
        )
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    eprintln!("Screen before scrollbar drag:\n{}", screen_before);

    assert!(
        screen_before.contains("Short line 1"),
        "Should see Short line 1 at top"
    );

    // Get scrollbar position
    let scrollbar_col = TERMINAL_WIDTH - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();

    // Drag scrollbar from top to bottom
    let drag_start_row = content_first_row as u16 + 2;
    let drag_end_row = content_last_row as u16 - 2;

    eprintln!(
        "Dragging scrollbar from row {} to row {}",
        drag_start_row, drag_end_row
    );

    harness
        .mouse_drag(scrollbar_col, drag_start_row, scrollbar_col, drag_end_row)
        .unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    eprintln!("Screen after scrollbar drag:\n{}", screen_after);

    let content_changed = screen_before != screen_after;

    assert!(
        content_changed,
        "Scrollbar drag should change viewport in multi-line file with wrapped content.\n\
         Dragging from row {} to {} should scroll, but viewport didn't change.\n\
         Before:\n{}\n\nAfter:\n{}",
        drag_start_row, drag_end_row, screen_before, screen_after
    );
}
