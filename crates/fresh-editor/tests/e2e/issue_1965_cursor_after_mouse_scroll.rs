//! End-to-end regression test for issue #1965: "Cursor issues while scrolling
//! down with mouse wheel".
//!
//! Bug: When the user scrolls the viewport with the mouse wheel so that the
//! cursor's actual buffer position scrolls off-screen, a phantom hardware
//! cursor visual appears on the first visible row of the new viewport.  The
//! cursor isn't really there — pressing an arrow key snaps the viewport back
//! to the real cursor location — but the visual is misleading.
//!
//! Expected: when the cursor's buffer position is outside the visible
//! viewport after a mouse-wheel scroll, the hardware cursor should not be
//! drawn anywhere in the viewport.

use crate::common::harness::EditorTestHarness;

/// Scroll down past the cursor with the mouse wheel and verify the hardware
/// cursor is not rendered at the top of the visible viewport.
#[test]
fn cursor_not_drawn_after_mouse_wheel_scrolls_past_it() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Buffer with many lines so we have plenty of room to scroll past the
    // cursor's logical position.
    let content: String = (0..200).map(|i| format!("Line {i}\n")).collect();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

    // Park the cursor on line 0 (cursor_position == 0).
    use crossterm::event::{KeyCode, KeyModifiers};
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    let cursor_before = harness.cursor_position();
    assert_eq!(
        cursor_before, 0,
        "test precondition: cursor should be at start of buffer"
    );

    // Render and confirm the cursor is visible at the top of the viewport.
    let baseline = harness.render_observing_cursor().unwrap();
    assert!(
        baseline.is_some(),
        "baseline: hardware cursor should be visible when the cursor is in-view"
    );

    // Scroll the viewport down many times.  Each ScrollDown moves the
    // viewport down by ~3 logical lines (the default wheel step), so after
    // 30 events we are well past line 0.
    let (content_first_row, _) = harness.content_area_rows();
    let scroll_col = 10u16;
    let scroll_row = content_first_row as u16 + 5;
    for _ in 0..30 {
        harness.mouse_scroll_down(scroll_col, scroll_row).unwrap();
    }

    // The mouse-wheel scroll must not move the actual cursor.
    assert_eq!(
        harness.cursor_position(),
        cursor_before,
        "mouse wheel scroll must not move the buffer cursor"
    );

    // The viewport must have actually scrolled (sanity: line 0 should be
    // gone from the visible content area).
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Line 0\n")
            && !screen
                .lines()
                .any(|l| l.trim_start().starts_with("Line 0 ")),
        "viewport should have scrolled away from Line 0 after wheel events.\nScreen:\n{screen}"
    );

    // The bug: a phantom hardware cursor shows at the top of the new
    // viewport even though the real cursor (at byte 0) is now offscreen.
    // The fix: hardware cursor should be hidden when the real cursor is
    // not in the visible range.
    let cursor_after = harness.render_observing_cursor().unwrap();
    assert!(
        cursor_after.is_none(),
        "hardware cursor should be hidden when the buffer cursor has scrolled out of view, \
         but it was rendered at {:?}",
        cursor_after
    );
}
