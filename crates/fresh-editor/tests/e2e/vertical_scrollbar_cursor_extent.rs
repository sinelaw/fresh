//! Renderer/layout regression coverage for cursor extent beside a vertical scrollbar.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

fn config(show_vertical_scrollbar: bool) -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.line_numbers = false;
    config.editor.show_vertical_scrollbar = show_vertical_scrollbar;
    config
}

fn fixture() -> String {
    // The suffix covers a double-width CJK grapheme, a combining sequence,
    // and a wide emoji before the final ASCII cell.
    let mut text = format!("{}界e\u{301}🙂Z", "x".repeat(32));
    for _ in 0..40 {
        text.push_str("\nshort");
    }
    text
}

fn assert_scrollbar_column(harness: &EditorTestHarness, column: u16, present: bool) {
    let (first_row, last_row) = harness.content_area_rows();
    for row in first_row..=last_row {
        let has_scrollbar = harness.is_scrollbar_thumb_at(column, row as u16)
            || harness.is_scrollbar_track_at(column, row as u16);
        assert_eq!(
            has_scrollbar, present,
            "scrollbar style at ({column}, {row}) should be {present}"
        );
    }
}

fn assert_left_cell(
    harness: &mut EditorTestHarness,
    expected_cell: &str,
    width_to_next: u16,
    next_x: u16,
    expected_y: u16,
) -> (u16, u16) {
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (x, y) = harness.screen_cursor_position();
    assert_eq!(harness.get_cell(x, y).as_deref(), Some(expected_cell));
    assert_eq!(x + width_to_next, next_x);
    assert_eq!(y, expected_y);
    (x, y)
}

fn assert_cursor_extent(show_vertical_scrollbar: bool) {
    let mut harness =
        EditorTestHarness::with_config(13, 12, config(show_vertical_scrollbar)).unwrap();
    harness.load_buffer_from_text(&fixture()).unwrap();

    // End may invoke a pre-render buffer-aware visibility pass; rendering then
    // invokes the renderer/layout-only pass with the renderer-synced content width.
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_width = harness
        .screen_to_string()
        .lines()
        .next()
        .expect("rendered screen has a first row")
        .chars()
        .count() as u16;
    let edge = screen_width.saturating_sub(1);
    let last_content_cell = if show_vertical_scrollbar {
        edge.saturating_sub(1)
    } else {
        edge
    };
    assert_scrollbar_column(&harness, edge, show_vertical_scrollbar);
    assert_eq!(harness.screen_cursor_position().0, last_content_cell);

    let eol_y = harness.screen_cursor_position().1;
    let (z_x, z_y) = assert_left_cell(&mut harness, "Z", 1, last_content_cell, eol_y);
    let (emoji_x, emoji_y) = assert_left_cell(&mut harness, "🙂", 2, z_x, z_y);
    let (combining_x, combining_y) =
        assert_left_cell(&mut harness, "e\u{301}", 1, emoji_x, emoji_y);
    let (_, cjk_y) = assert_left_cell(&mut harness, "界", 2, combining_x, combining_y);
    assert_eq!(cjk_y, z_y);
}

#[test]
fn end_uses_last_content_cell_when_vertical_scrollbar_is_shown() {
    assert_cursor_extent(true);
}

#[test]
fn end_uses_screen_edge_when_vertical_scrollbar_is_hidden() {
    assert_cursor_extent(false);
}
