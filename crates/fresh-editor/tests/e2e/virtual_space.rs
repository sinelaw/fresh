//! E2E tests for virtual space (`editor.virtual_space`): the cursor may sit
//! past the end of a line. See docs/internal/virtual-space-scoping.md.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, VirtualSpaceMode};

fn harness_with_mode(mode: VirtualSpaceMode) -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.virtual_space = mode;
    EditorTestHarness::with_config(80, 24, config).unwrap()
}

/// With virtual space on, moving down onto a shorter line keeps the cursor
/// at its on-screen column instead of snapping to the line end.
#[test]
fn test_arrow_down_renders_cursor_past_eol() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("abcdef\nab\nabcdef").unwrap();

    let (x0, y0) = harness
        .find_text_on_screen("abcdef")
        .expect("first line visible");

    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!(
        (cx, cy),
        (x0 + 4, y0 + 1),
        "cursor floats at column 4, two columns past 'ab'"
    );
}

/// With virtual space off (the default), the same movement snaps the cursor
/// to the short line's end.
#[test]
fn test_arrow_down_snaps_to_eol_when_off() {
    let mut harness = harness_with_mode(VirtualSpaceMode::Off);
    harness.load_buffer_from_text("abcdef\nab\nabcdef").unwrap();

    let (x0, y0) = harness
        .find_text_on_screen("abcdef")
        .expect("first line visible");

    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!((cx, cy), (x0 + 2, y0 + 1), "cursor clamps to end of 'ab'");
}

/// With virtual space on, ArrowRight at end of line walks the cursor into
/// the empty space instead of wrapping to the next line.
#[test]
fn test_arrow_right_renders_cursor_past_eol() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab\nxyz").unwrap();

    let (x1, y1) = harness
        .find_text_on_screen("xyz")
        .expect("second line visible");
    let (x0, y0) = (x1, y1 - 1); // "ab" starts at the same column, one row up

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!(
        (cx, cy),
        (x0 + 5, y0),
        "cursor sits three columns past 'ab'"
    );

    // Left walks back through the virtual columns before bytes move.
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!((cx, cy), (x0 + 4, y0));
}

/// Vertical movement through a short line and back onto a long one restores
/// the original column (the goal column survives the virtual segment).
#[test]
fn test_column_survives_through_short_line() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("abcdef\nab\nabcdef").unwrap();

    let (x0, y0) = harness
        .find_text_on_screen("abcdef")
        .expect("first line visible");

    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!((cx, cy), (x0 + 5, y0 + 2), "column 5 restored on line 3");
}
