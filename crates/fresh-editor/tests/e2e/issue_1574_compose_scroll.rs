//! Tests for issue #1574: Weird scrolling in a buffer with heavy line wrapping.
//!
//! Root cause (pre-fix): `Viewport::ensure_visible` gates its decision to
//! scroll on `cursor_near_top` — if the cursor sits *near* the top of the
//! viewport the routine scrolls up one visual row, if *near* the bottom it
//! scrolls down one visual row. When the cursor is already inside the
//! viewport (the `cursor_is_visible` branch) the routine returns without
//! touching `top_byte`. So far, so good.
//!
//! But a second codepath kicks in for long wrapped lines: the
//! wrapped-counting branch (`line_wrap_enabled`) reports the cursor as
//! *not* visible whenever the cursor's column-within-wrap exceeds the
//! "bottom margin" (`viewport_lines - effective_offset`). That margin
//! exists to keep a scroll-margin around the cursor, but in a buffer with
//! heavy wrapping the cursor rides through the bottom margin on every
//! Down press, so `ensure_visible` happily scrolls once per key.
//! Moving Up again from the bottom does the same: cursor lands in the
//! bottom margin → scroll up → repeat.  The net effect is that the
//! viewport drifts by one visual row per arrow press even though the
//! cursor is still inside the visible area.
//!
//! The fix is in `Viewport::ensure_visible`: only apply the scroll
//! margin when the cursor is *actually* outside the viewport. A cursor
//! already inside the visible rows never triggers a scroll — the user's
//! configured scroll margin is for *approaching* the edge, not for
//! jostling the viewport when the cursor is already in the margin.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

fn long_wrapped_content() -> String {
    // Build a short document whose paragraphs each wrap to many visual rows
    // in an 80-column viewport, so the "heavy wrapping" precondition is met.
    let para = "This is a deliberately long paragraph that must wrap across many \
                visual rows so the scroll math is exercised. It continues for \
                a while so a single logical line becomes many visual rows.";
    let mut s = String::from("# Test\n\n");
    for i in 1..=6 {
        s.push_str(&format!("Paragraph {i}: {para}\n\n"));
    }
    s.push_str("End of file.\n");
    s
}

#[test]
fn test_issue_1574_up_does_not_scroll_when_cursor_not_at_top() {
    let mut harness = EditorTestHarness::with_config(80, 20, config_with_wrap()).unwrap();
    let content = long_wrapped_content();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Jump to the end of the file — the viewport scrolls to keep the cursor
    // visible and the cursor ends up at the bottom of the viewport.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let top_byte_at_end = harness.top_byte();
    let initial_cursor = harness.cursor_position();

    // Press Up a handful of times. The cursor starts at the bottom of the
    // viewport, so the first press moves it into the viewport's interior
    // (still visible, just one row higher). Subsequent presses continue to
    // move the cursor upward within the viewport. The viewport must NOT
    // scroll while the cursor remains inside the visible rows.
    //
    // With the bug, `ensure_visible` treats a cursor in the "bottom margin"
    // (which, by default `scroll_offset = 3`, is the bottom 3 rows) as
    // needing a scroll — so the first three Up presses each scroll the
    // viewport up one visual row.
    for i in 1..=3 {
        let before = harness.top_byte();
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let after = harness.top_byte();
        assert_eq!(
            before, after,
            "Up #{i}: top_byte drifted from {before} to {after} even though the cursor \
             is still inside the viewport (bug #1574: scroll follows cursor in wrapped buffers)",
        );
    }

    // Sanity: we started at the end, and top_byte must not have moved
    // despite several arrow presses.
    assert_eq!(
        harness.top_byte(),
        top_byte_at_end,
        "Viewport drifted in total from {top_byte_at_end} after arrow presses; \
         cursor moved from {initial_cursor} to {}",
        harness.cursor_position()
    );
}

#[test]
fn test_issue_1574_down_does_not_scroll_when_cursor_not_at_bottom() {
    let mut harness = EditorTestHarness::with_config(80, 20, config_with_wrap()).unwrap();
    let content = long_wrapped_content();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Start of file — cursor at top, viewport at top.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let top_byte_initial = harness.top_byte();

    // Press Down a handful of times. The cursor should march down through
    // the top of the viewport. As long as the cursor stays inside the
    // viewport, the top must not move.
    for i in 1..=3 {
        let before = harness.top_byte();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let after = harness.top_byte();
        assert_eq!(
            before, after,
            "Down #{i}: top_byte drifted from {before} to {after} even though \
             the cursor is still near the top of the viewport (bug #1574)",
        );
    }

    assert_eq!(harness.top_byte(), top_byte_initial);
}
