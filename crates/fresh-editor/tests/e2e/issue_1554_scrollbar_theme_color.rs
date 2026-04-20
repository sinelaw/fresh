//! Regression test for issue #1554: scrollbar track/thumb colors ignored theme settings.
//!
//! Before the fix, `scrollbar_track_fg` and `scrollbar_thumb_fg` from the theme
//! were ignored for the editor split scrollbar (both vertical and horizontal),
//! because the render path used hardcoded `Color::DarkGray` / `Color::Black` /
//! `Color::Gray` values.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use ratatui::style::Color;

fn long_content(num_lines: usize) -> String {
    (0..num_lines)
        .map(|i| format!("line {i:04}: lorem ipsum dolor sit amet"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn scan_column_bgs(harness: &EditorTestHarness, col: u16) -> Vec<Color> {
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .filter_map(|row| {
            harness
                .get_cell_style(col, row as u16)
                .and_then(|s| s.bg)
                .filter(|c| *c != Color::Reset)
        })
        .collect()
}

#[test]
fn test_vertical_scrollbar_uses_theme_track_and_thumb_colors() {
    let config = Config {
        theme: "high-contrast".into(),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let theme = harness.editor().theme();
    let expected_track = theme.scrollbar_track_fg;
    let expected_thumb = theme.scrollbar_thumb_fg;

    harness.load_buffer_from_text(&long_content(200)).unwrap();
    harness.render().unwrap();

    let bgs = scan_column_bgs(&harness, 79);
    assert!(
        !bgs.is_empty(),
        "Rightmost column should contain scrollbar cells"
    );

    let saw_track = bgs.iter().any(|c| *c == expected_track);
    let saw_thumb = bgs.iter().any(|c| *c == expected_thumb);

    assert!(
        saw_track,
        "Scrollbar track should use theme.scrollbar_track_fg ({expected_track:?}); saw bgs {bgs:?}"
    );
    assert!(
        saw_thumb,
        "Scrollbar thumb should use theme.scrollbar_thumb_fg ({expected_thumb:?}); saw bgs {bgs:?}"
    );
}

#[test]
fn test_horizontal_scrollbar_uses_theme_track_color() {
    let mut config = Config {
        theme: "high-contrast".into(),
        ..Default::default()
    };
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = true;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let theme = harness.editor().theme();
    let expected_track = theme.scrollbar_track_fg;

    let long_line_content: String = (0..50)
        .map(|i| format!("line {i}: {}", "X".repeat(200)))
        .collect::<Vec<_>>()
        .join("\n");
    harness.load_buffer_from_text(&long_line_content).unwrap();
    harness.render().unwrap();

    let (_, last_content_row) = harness.content_area_rows();
    let row = last_content_row as u16;
    let width = harness.buffer().area.width;

    let bgs: Vec<Color> = (0..width)
        .filter_map(|c| {
            harness
                .get_cell_style(c, row)
                .and_then(|s| s.bg)
                .filter(|c| *c != Color::Reset)
        })
        .collect();

    assert!(
        bgs.iter().any(|c| *c == expected_track),
        "Horizontal scrollbar track should use theme.scrollbar_track_fg ({expected_track:?}); saw bgs {bgs:?}"
    );
}
