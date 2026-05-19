//! Migration of `tests/e2e/line_wrap_scroll_bugs.rs` — scrolling
//! bugs that surface when line wrapping is enabled.
//!
//! Two related root causes, eight regression-triggering scenarios
//! preserved here verbatim:
//!
//!   1. **Scrollbar appearance.** When a file has long lines that
//!      wrap to many visual rows, the scrollbar incorrectly shows
//!      as "nothing to scroll" because it calculates based on
//!      logical line count (1) instead of visual row count (many).
//!   2. **Mouse wheel / Page Down / scrollbar click+drag.** Scrolling
//!      doesn't work because the scroll routines iterate through
//!      logical lines, not visual rows.
//!   3. **Scrollbar thumb drag-start jump.** Clicking on the thumb
//!      (or dragging horizontally with the same row) must not jump
//!      the viewport — the thumb should move relative to where it
//!      was clicked, not center around the mouse.
//!
//! ## Harness-direct pattern
//!
//! Every test in this file probes surfaces with no `EditorTestApi`
//! projection:
//!   * `content_area_rows`, `get_cell_style`, `is_scrollbar_thumb_at`
//!     for scrollbar geometry,
//!   * `editor().theme()` for the scrollbar thumb/track colors,
//!   * `mouse_scroll_down`, `mouse_drag`, `mouse_click`, `send_mouse`
//!     for input plumbing,
//!   * `top_line_number` and `editor().get_split_areas()` for
//!     scroll-position observation.
//!
//! These tests therefore take the harness-direct path (the same
//! pattern `migrated_horizontal_scrollbar.rs` and
//! `migrated_line_wrap_parity.rs` use).
//!
//! Source: `tests/e2e/line_wrap_scroll_bugs.rs` (8 tests migrated;
//! no tests deferred).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Helper to create a config with line wrapping enabled.
fn config_with_line_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

#[test]
fn migrated_scrollbar_shows_scrollable_content_with_wrapped_lines() {
    // Original: `test_scrollbar_shows_scrollable_content_with_wrapped_lines`.
    // Bug: when line_wrap=true and the file is a single very long
    // line, the scrollbar thumb fills the entire track ("nothing to
    // scroll") because `scrollbar_line_counts()` uses logical lines
    // (1) instead of visual rows (~20 at width 60).
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // Single logical line, 1000 chars — wraps to ~20 visual rows at
    // width 60 (gutter ~8, scrollbar 1 → ~51 chars per row).
    let long_line = "X".repeat(1000);
    harness.type_text(&long_line).unwrap();
    harness.render().unwrap();

    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        !buffer_content.contains('\n'),
        "Should be a single logical line"
    );
    assert_eq!(buffer_content.len(), 1000, "Line should be 1000 chars");

    let scrollbar_col = TERMINAL_WIDTH - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();

    // Compare against the active theme's scrollbar colors (which the
    // renderer uses) to count thumb vs. track cells.
    let theme = harness.editor().theme();
    let thumb_bg = theme.scrollbar_thumb_fg;
    let track_bg = theme.scrollbar_track_fg;
    let mut thumb_count = 0;
    let mut track_count = 0;
    let content_height = content_last_row - content_first_row + 1;

    for row in content_first_row..=content_last_row {
        if let Some(style) = harness.get_cell_style(scrollbar_col, row as u16) {
            match style.bg {
                Some(c) if c == thumb_bg => thumb_count += 1,
                Some(c) if c == track_bg => track_count += 1,
                _ => {}
            }
        }
    }

    assert!(
        thumb_count < content_height,
        "Scrollbar thumb ({} cells) should NOT fill the entire content area ({} rows). \
         This indicates the scrollbar incorrectly thinks there's nothing to scroll. \
         The file has 1000 chars that wrap to ~20 visual lines, which exceeds the viewport.",
        thumb_count,
        content_height
    );
    assert!(
        track_count > 0,
        "Scrollbar track should be visible, indicating there's content to scroll to"
    );
}

#[test]
fn migrated_mouse_wheel_scrolls_wrapped_content() {
    // Original: `test_mouse_wheel_scrolls_wrapped_content`.
    // Bug: mouse wheel scrolling doesn't move the viewport because
    // scroll_up/scroll_down iterate logical lines (1), not visual
    // rows.
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // Pattern: "AAA...BBB...CCC...DDD..." — 200 chars per letter,
    // so we can identify which slice of the wrapped line is visible.
    let mut long_line = String::new();
    for ch in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] {
        long_line.push_str(&ch.to_string().repeat(200));
    }

    harness.type_text(&long_line).unwrap();
    // Move cursor to the beginning so we start at top.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains("AAAA"),
        "Before scrolling, should see the beginning of the line (A chars)"
    );

    let (content_first_row, _) = harness.content_area_rows();
    let scroll_col = TERMINAL_WIDTH / 2;
    let scroll_row = content_first_row as u16 + 5;

    // 20 mouse-wheel scrolls — should move the viewport down through
    // many visual rows.
    for _ in 0..20 {
        harness.mouse_scroll_down(scroll_col, scroll_row).unwrap();
    }

    let screen_after = harness.screen_to_string();
    let content_changed = screen_before != screen_after;
    let sees_later_content = screen_after.contains("BBBB")
        || screen_after.contains("CCCC")
        || screen_after.contains("DDDD")
        || screen_after.contains("EEEE")
        || screen_after.contains("FFFF")
        || screen_after.contains("GGGG")
        || screen_after.contains("HHHH");

    assert!(
        content_changed,
        "Screen content should change after scrolling down with mouse wheel. \
         The viewport appears stuck, indicating scroll_down is not moving through \
         visual (wrapped) lines correctly."
    );
    assert!(
        sees_later_content,
        "After scrolling down, should see content from later in the wrapped line. \
         Screen still shows only the beginning, indicating scrolling is not working with wrapped lines.\n\
         Screen:\n{}", screen_after
    );
}

#[test]
fn migrated_scrollbar_drag_with_wrapped_lines() {
    // Original: `test_scrollbar_drag_with_wrapped_lines`.
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    let mut long_line = String::new();
    for ch in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] {
        long_line.push_str(&ch.to_string().repeat(200));
    }

    harness.type_text(&long_line).unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen_at_top = harness.screen_to_string();
    assert!(
        screen_at_top.contains("AAAA"),
        "Should start at top showing A characters"
    );

    let scrollbar_col = TERMINAL_WIDTH - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();

    let drag_start_row = content_first_row as u16 + 2;
    let drag_end_row = content_last_row as u16 - 2;

    harness
        .mouse_drag(scrollbar_col, drag_start_row, scrollbar_col, drag_end_row)
        .unwrap();

    let screen_after_drag = harness.screen_to_string();
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

#[test]
fn migrated_page_down_scrolls_visual_rows_with_wrapped_line() {
    // Original: `test_page_down_scrolls_visual_rows_with_wrapped_line`.
    // Page Down should scroll by approximately one viewport height
    // of visual rows.
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    let mut long_line = String::new();
    for ch in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] {
        long_line.push_str(&ch.to_string().repeat(200));
    }

    harness.type_text(&long_line).unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains("AAAA"),
        "Should start at top showing A characters"
    );

    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    let content_changed = screen_before != screen_after;
    assert!(
        content_changed,
        "PageDown should scroll the viewport through visual rows of the wrapped line. \
         The screen content didn't change, indicating PageDown is stuck on a single logical line."
    );
}

#[test]
fn migrated_mouse_wheel_with_multiline_file_one_long_line() {
    // Original: `test_mouse_wheel_with_multiline_file_one_long_line`.
    // File structure mimics ~/Downloads/zz.txt: short lines + one
    // very long line + short lines.
    const TERMINAL_WIDTH: u16 = 80;
    const TERMINAL_HEIGHT: u16 = 24;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    let short_line1 = "Short line 1";
    let short_line2 = "Short line 2";
    let short_line3 = "Short line 3";
    let long_line = "X".repeat(2000);
    let short_line4 = "Short line 4";
    let short_line5 = "Short line 5";

    let content = format!(
        "{}\n{}\n{}\n{}\n{}\n{}",
        short_line1, short_line2, short_line3, long_line, short_line4, short_line5
    );

    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains("Short line 1"),
        "Should see Short line 1 at top"
    );

    let (content_first_row, _) = harness.content_area_rows();

    for _ in 0..10 {
        harness
            .mouse_scroll_down(40, content_first_row as u16 + 5)
            .unwrap();
    }

    let screen_after = harness.screen_to_string();
    let content_changed = screen_before != screen_after;

    assert!(
        content_changed,
        "Mouse wheel scroll should change viewport in multi-line file with wrapped content.\n\
         Before:\n{}\n\nAfter:\n{}",
        screen_before, screen_after
    );
}

#[test]
fn migrated_scrollbar_click_with_multiline_file_one_long_line() {
    // Original: `test_scrollbar_click_with_multiline_file_one_long_line`.
    // Bug: scrollbar click doesn't work because the handler
    // computes max_scroll_line using logical lines (here 6, less
    // than the viewport's 24 rows), so max_scroll_line is 0 and
    // no scrolling is allowed — even though the long line wraps to
    // 30+ visual rows.
    const TERMINAL_WIDTH: u16 = 80;
    const TERMINAL_HEIGHT: u16 = 24;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    let short_line1 = "<p>Short line 1</p>";
    let short_line2 = "</p>";
    let short_line3 = "</div>";
    let long_line = format!(
        "<div class=\"content\">{}</div>",
        "CONTENT_".repeat(250)
    );
    let short_line5 = "";
    let short_line6 = "";

    let content = format!(
        "{}\n{}\n{}\n{}\n{}\n{}",
        short_line1, short_line2, short_line3, long_line, short_line5, short_line6
    );

    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains("Short line 1"),
        "Should see Short line 1 at top"
    );

    let scrollbar_col = TERMINAL_WIDTH - 1;
    let (_content_first_row, content_last_row) = harness.content_area_rows();

    let click_row = content_last_row as u16 - 3;

    harness.mouse_click(scrollbar_col, click_row).unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    let content_changed = screen_before != screen_after;

    assert!(
        content_changed,
        "Scrollbar click should change viewport in multi-line file with wrapped content.\n\
         Clicking at row {} should scroll down, but viewport didn't change.\n\
         This indicates scrollbar click is broken for files with few logical lines but many visual rows.\n\
         Before:\n{}\n\nAfter:\n{}",
        click_row, screen_before, screen_after
    );
}

#[test]
fn migrated_scrollbar_drag_with_multiline_file_one_long_line() {
    // Original: `test_scrollbar_drag_with_multiline_file_one_long_line`.
    const TERMINAL_WIDTH: u16 = 80;
    const TERMINAL_HEIGHT: u16 = 24;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

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

    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains("Short line 1"),
        "Should see Short line 1 at top"
    );

    let scrollbar_col = TERMINAL_WIDTH - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();

    let drag_start_row = content_first_row as u16 + 2;
    let drag_end_row = content_last_row as u16 - 2;

    harness
        .mouse_drag(scrollbar_col, drag_start_row, scrollbar_col, drag_end_row)
        .unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    let content_changed = screen_before != screen_after;

    assert!(
        content_changed,
        "Scrollbar drag should change viewport in multi-line file with wrapped content.\n\
         Dragging from row {} to {} should scroll, but viewport didn't change.\n\
         Before:\n{}\n\nAfter:\n{}",
        drag_start_row, drag_end_row, screen_before, screen_after
    );
}

#[test]
fn migrated_scrollbar_thumb_drag_no_jump_on_start() {
    // Original: `test_scrollbar_thumb_drag_no_jump_on_start`.
    // Reproduction: click on the thumb, drag horizontally (same
    // row), release — should not scroll. The thumb must move
    // relative to where it was clicked, not center around the
    // mouse position.
    const TERMINAL_WIDTH: u16 = 80;
    const TERMINAL_HEIGHT: u16 = 24;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    // 100 short lines → scrollable.
    let content: String = (1..=100)
        .map(|i| format!("Line {} content here\n", i))
        .collect();

    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Scroll down 30 lines via keyboard so the thumb is mid-track.
    for _ in 0..30 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let top_line_before = harness.top_line_number();

    // Pull thumb position from the cached split-area layout.
    let scrollbar_col = TERMINAL_WIDTH - 1;
    let split_areas = harness.editor().get_split_areas().to_vec();
    let (_split_id, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) =
        split_areas[0];
    assert!(
        thumb_end > thumb_start,
        "Thumb should have nonzero size: start={}, end={}",
        thumb_start,
        thumb_end
    );
    let thumb_mid = (thumb_start + thumb_end) / 2;
    let thumb_row = scrollbar_rect.y + thumb_mid as u16;

    // Mouse down on the thumb starts the drag — must NOT change
    // scroll position.
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
    let down_event = MouseEvent {
        kind: MouseEventKind::Down(MouseButton::Left),
        column: scrollbar_col,
        row: thumb_row,
        modifiers: KeyModifiers::NONE,
    };
    harness.send_mouse(down_event).unwrap();
    harness.render().unwrap();

    let top_line_after_click = harness.top_line_number();
    assert_eq!(
        top_line_before, top_line_after_click,
        "Clicking on scrollbar thumb should not change scroll position.\n\
         Before click: line {}, After click: line {}",
        top_line_before, top_line_after_click
    );

    // Horizontal drag (same row, different col) — must NOT scroll.
    let drag_event = MouseEvent {
        kind: MouseEventKind::Drag(MouseButton::Left),
        column: scrollbar_col - 5,
        row: thumb_row,
        modifiers: KeyModifiers::NONE,
    };
    harness.send_mouse(drag_event).unwrap();
    harness.render().unwrap();

    let top_line_after_horizontal_drag = harness.top_line_number();
    assert_eq!(
        top_line_before, top_line_after_horizontal_drag,
        "Dragging scrollbar thumb horizontally (same row) should not change scroll position.\n\
         Before: line {}, After horizontal drag: line {}\n\
         This indicates the thumb is jumping to center around mouse position instead of using relative movement.",
        top_line_before,
        top_line_after_horizontal_drag
    );

    // Release — position should remain unchanged.
    let up_event = MouseEvent {
        kind: MouseEventKind::Up(MouseButton::Left),
        column: scrollbar_col - 5,
        row: thumb_row,
        modifiers: KeyModifiers::NONE,
    };
    harness.send_mouse(up_event).unwrap();
    harness.render().unwrap();

    let top_line_after_release = harness.top_line_number();
    assert_eq!(
        top_line_before, top_line_after_release,
        "After horizontal drag and release, scroll position should be unchanged.\n\
         Before: line {}, After release: line {}",
        top_line_before, top_line_after_release
    );
}

/// Anti-test: drop the 20 `mouse_scroll_down` calls. Without them,
/// the viewport stays at the top of the buffer and the screen
/// content must NOT change — proves that the positive
/// `migrated_mouse_wheel_scrolls_wrapped_content` claim depends on
/// the wheel events actually being delivered, not on the buffer
/// being typed.
#[test]
fn anti_mouse_wheel_without_scroll_leaves_screen_unchanged() {
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    let mut long_line = String::new();
    for ch in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] {
        long_line.push_str(&ch.to_string().repeat(200));
    }

    harness.type_text(&long_line).unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    // No mouse_scroll_down calls — that's the load-bearing step we
    // drop.
    harness.render().unwrap();
    let screen_after = harness.screen_to_string();

    assert_eq!(
        screen_before, screen_after,
        "anti: without any mouse_scroll_down calls, the viewport must \
         remain at the top of the wrapped line and the screen must be \
         unchanged after a second render. The positive mouse-wheel test \
         depends on the wheel events, not on harness construction."
    );
    // And we must still be at the beginning of the line.
    assert!(
        screen_after.contains("AAAA"),
        "anti: viewport must still show the start of the line (A chars) \
         when no scroll has been performed. Got:\n{}",
        screen_after
    );
}

/// Anti-test: drop the `PageDown` key. Without it, the screen
/// content must remain unchanged after a second render — proves
/// the positive `migrated_page_down_scrolls_visual_rows_with_wrapped_line`
/// claim depends on the keystroke actually being delivered.
#[test]
fn anti_page_down_without_keypress_leaves_screen_unchanged() {
    const TERMINAL_WIDTH: u16 = 60;
    const TERMINAL_HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(TERMINAL_WIDTH, TERMINAL_HEIGHT, config_with_line_wrap())
            .unwrap();

    let mut long_line = String::new();
    for ch in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] {
        long_line.push_str(&ch.to_string().repeat(200));
    }

    harness.type_text(&long_line).unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    // No PageDown press here.
    harness.render().unwrap();
    let screen_after = harness.screen_to_string();

    assert_eq!(
        screen_before, screen_after,
        "anti: without a PageDown press, the screen must remain unchanged. \
         The positive PageDown test depends on the keystroke itself, \
         not on the buffer being typed."
    );
}
