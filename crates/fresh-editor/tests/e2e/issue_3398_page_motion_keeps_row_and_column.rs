//! Regression tests for issue #3398: after PageDown, the next Down or Right
//! scrolled the view back a few rows, and PageDown reset the column to 1.
//!
//! PageDown landed the caret on the first row of the new page, inside the
//! scroll-off margin, so the next ordinary motion scrolled the view to
//! restore the margin. It also placed the caret at the row's start. The
//! caret now keeps its screen row, kept out of the margins, and its goal
//! column, as Up and Down do. PageUp is checked the same way, with line wrap
//! on and off.
//!
//! Assertions read the rendered screen: the gutter's first line number (the
//! view's top), the status bar's `Ln`/`Col`, and the terminal cursor's cell.
//! The goal column is a visual one, so it is checked as the caret's screen
//! column: on a wrapped line it is a column of the visual row.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// The `(Ln, Col)` of the status bar.
fn status_position(harness: &EditorTestHarness) -> (usize, usize) {
    let status = harness.get_status_bar();
    let number_after = |label: &str| -> usize {
        let at = status
            .find(label)
            .unwrap_or_else(|| panic!("no {label:?} in status bar: {status:?}"))
            + label.len();
        status[at..]
            .chars()
            .take_while(|c| c.is_ascii_digit())
            .collect::<String>()
            .parse()
            .unwrap()
    };
    (number_after("Ln "), number_after("Col "))
}

/// The first line number drawn in the gutter: which line the view starts at.
fn top_line(harness: &EditorTestHarness) -> usize {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .find_map(|row| row.split_once('│')?.0.trim().parse().ok())
        .unwrap_or_else(|| panic!("no line numbers on screen:\n{screen}"))
}

fn press(harness: &mut EditorTestHarness, code: KeyCode) {
    harness.send_key(code, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

fn harness_with(
    line_wrap: bool,
    content: &str,
) -> (EditorTestHarness, crate::common::fixtures::TestFixture) {
    let mut config = Config::default();
    config.editor.line_wrap = line_wrap;
    let mut harness = EditorTestHarness::with_config(100, 30, config).unwrap();
    let fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();
    (harness, fixture)
}

/// 300 short numbered lines, every one wider than the goal column.
fn plain_lines() -> String {
    (1..=300)
        .map(|i| format!("{i:03} abcdefghijklmnopqrstuvwxyz"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Lines of three wrapped rows each at a 100-column pane.
fn wrapped_lines() -> String {
    (1..=150)
        .map(|i| format!("{i:03} {}", "word ".repeat(45)))
        .collect::<Vec<_>>()
        .join("\n")
}

/// PageDown from the first line, then the next Down / Right must not move
/// the view, and the column survives the page.
fn assert_page_down_then_motion_holds_view(line_wrap: bool, content: &str) {
    for next in [KeyCode::Down, KeyCode::Right] {
        let (mut harness, _fixture) = harness_with(line_wrap, content);
        for _ in 0..7 {
            press(&mut harness, KeyCode::Right);
        }
        assert_eq!(status_position(&harness), (1, 8));
        let caret_col = harness.screen_cursor_position().0;

        press(&mut harness, KeyCode::PageDown);
        let top_after_page = top_line(&harness);
        assert!(top_after_page > 1, "PageDown should scroll the view");
        assert_eq!(
            harness.screen_cursor_position().0,
            caret_col,
            "PageDown keeps the goal column (wrap: {line_wrap}):\n{}",
            harness.screen_to_string()
        );

        press(&mut harness, next);
        assert_eq!(
            top_line(&harness),
            top_after_page,
            "{next:?} after PageDown scrolled the view (wrap: {line_wrap}):\n{}",
            harness.screen_to_string()
        );
    }
}

#[test]
fn test_page_down_then_motion_does_not_rescroll_no_wrap() {
    assert_page_down_then_motion_holds_view(false, &plain_lines());
}

#[test]
fn test_page_down_then_motion_does_not_rescroll_wrapped() {
    assert_page_down_then_motion_holds_view(true, &wrapped_lines());
}

/// PageDown then PageUp: the caret keeps its screen row and column across
/// both, so the next Up / Down moves inside the page without scrolling.
fn assert_page_up_keeps_row_and_column(line_wrap: bool, content: &str) {
    let (mut harness, _fixture) = harness_with(line_wrap, content);
    for _ in 0..10 {
        press(&mut harness, KeyCode::Down);
    }
    for _ in 0..5 {
        press(&mut harness, KeyCode::Right);
    }
    let caret = harness.screen_cursor_position();
    press(&mut harness, KeyCode::PageDown);
    press(&mut harness, KeyCode::PageDown);

    press(&mut harness, KeyCode::PageUp);
    let top_after_page = top_line(&harness);
    assert_eq!(
        harness.screen_cursor_position(),
        caret,
        "PageUp keeps the caret's screen row and goal column (wrap: {line_wrap}):\n{}",
        harness.screen_to_string()
    );
    for next in [KeyCode::Up, KeyCode::Down] {
        press(&mut harness, next);
        assert_eq!(
            top_line(&harness),
            top_after_page,
            "{next:?} after PageUp scrolled the view (wrap: {line_wrap}):\n{}",
            harness.screen_to_string()
        );
    }
}

#[test]
fn test_page_up_keeps_row_and_column_no_wrap() {
    assert_page_up_keeps_row_and_column(false, &plain_lines());
}

#[test]
fn test_page_up_keeps_row_and_column_wrapped() {
    assert_page_up_keeps_row_and_column(true, &wrapped_lines());
}
