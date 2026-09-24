//! Regression test for issue #3294: with `wrap_column` set, arrowing down
//! through lines that wrap at that column left the cursor below the bottom
//! of the view.
//!
//! The renderer wrapped at `wrap_column`, but the viewport's scroll and
//! visibility math wrapped at the pane width, where the lines fit on one
//! row. It counted every two-row line as one row, so it believed the cursor
//! was on screen long after it had left it.
//!
//! Assertions read the rendered screen: the status bar's `Ln N` and the
//! gutter line numbers drawn in the pane.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// The `N` of the status bar's `Ln N, Col M`.
fn status_line(harness: &EditorTestHarness) -> usize {
    let status = harness.get_status_bar();
    let rest = &status[status
        .find("Ln ")
        .unwrap_or_else(|| panic!("no Ln in status bar: {status:?}"))
        + 3..];
    rest.chars()
        .take_while(|c| c.is_ascii_digit())
        .collect::<String>()
        .parse()
        .unwrap()
}

/// Every line number drawn in the gutter.
fn gutter_lines(harness: &EditorTestHarness) -> Vec<usize> {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .filter_map(|row| row.split_once('│')?.0.trim().parse().ok())
        .collect()
}

#[test]
fn test_cursor_stays_on_screen_when_lines_wrap_at_wrap_column() {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config.editor.wrap_column = Some(80);
    // Wide enough that the 76-character lines fit on one row at the pane
    // width; they wrap only because of `wrap_column`.
    let mut harness = EditorTestHarness::with_config(120, 40, config).unwrap();
    let content: Vec<String> = (1..=200)
        .map(|i| format!("{i:03} {}", "x".repeat(72)))
        .collect();
    let _fixture = harness.load_buffer_from_text(&content.join("\n")).unwrap();
    harness.render().unwrap();
    assert!(
        gutter_lines(&harness).len() < 30,
        "the lines should wrap at wrap_column, two rows each:\n{}",
        harness.screen_to_string()
    );

    for press in 1..=120 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        let cursor_line = status_line(&harness);
        let shown = gutter_lines(&harness);
        assert!(
            shown.contains(&cursor_line),
            "after {press} Downs the cursor is on line {cursor_line}, \
             but the view shows lines {:?}..={:?}:\n{}",
            shown.first(),
            shown.last(),
            harness.screen_to_string()
        );
    }
    assert!(
        status_line(&harness) > 50,
        "120 Downs over two-row lines should pass line 50"
    );
}
