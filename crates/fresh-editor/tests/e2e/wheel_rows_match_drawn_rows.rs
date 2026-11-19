//! The rows the wheel steps through are the rows the pane draws.
//!
//! Every scroll path (the wheel, the scrollbar, placement, the dual view's
//! sync) counts a pane's rows in its wrap index, keyed by the width and gutter
//! the pane wraps at. When that key disagrees with the width and gutter the
//! renderer lays the pane out at, a line near the edge is one row on screen
//! and two in the index (or the other way round), and a wheel notch over it
//! moves nothing, or two rows.
//!
//! Each case scrolls a buffer of lines of many lengths a row a notch and
//! checks, on the rendered screen, that every notch moves the text up exactly
//! one row — under the settings that change the width or gutter a source pane
//! lays out at.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;

const WIDTH: u16 = 100;
const HEIGHT: u16 = 30;

/// Lines of every length from about half the pane to past it, so some fall
/// on the wrap edge whatever the gutter and width.
fn content() -> String {
    let mut lines: Vec<String> = (0..120)
        .map(|i| format!("L{i:03} {}", "x".repeat(40 + (i * 7) % 90)))
        .collect();
    lines.push("END".to_string());
    lines.join("\n")
}

/// The pane's text rows, without the scrollbar column (its thumb moves).
fn text_rows(h: &EditorTestHarness) -> Vec<String> {
    let screen = h.screen_to_string();
    let rows: Vec<&str> = screen.lines().collect();
    rows[2..rows.len() - 2]
        .iter()
        .map(|r| r.chars().take(WIDTH as usize - 2).collect())
        .collect()
}

fn harness(config: Config) -> EditorTestHarness {
    let mut config = config;
    config.editor.line_wrap = true;
    config.editor.mouse_wheel_scroll_lines = 1;
    let mut h = EditorTestHarness::with_config(WIDTH, HEIGHT, config).unwrap();
    let _ = h.load_buffer_from_text(&content()).unwrap();
    h.render().unwrap();
    h
}

/// Set the active buffer's page width in its split, as a plugin's layout
/// hints do.
fn set_page_width(h: &mut EditorTestHarness, width: u16) {
    let buffer = h.editor().active_buffer();
    let window = h.editor_mut().active_window_mut();
    let split = window.split_manager().active_split();
    if let Some(view_state) = window.split_view_states_mut().get_mut(&split) {
        view_state.ensure_buffer_state(buffer).compose_width = Some(width);
    }
    h.render().unwrap();
}

fn assert_each_notch_moves_one_row(h: &mut EditorTestHarness, case: &str) {
    for notch in 0..1000 {
        let before = text_rows(h);
        h.mouse_scroll_down(WIDTH / 2, HEIGHT / 2).unwrap();
        let after = text_rows(h);
        if after == before {
            assert!(
                h.screen_to_string().contains("END"),
                "{case}: notch {notch} moved nothing before the end:\n{}",
                h.screen_to_string()
            );
            return;
        }
        let n = before.len();
        assert_eq!(
            after[..n - 1],
            before[1..],
            "{case}: notch {notch} should move the text up one row\nbefore:\n{}\nafter:\n{}",
            before.join("\n"),
            after.join("\n")
        );
    }
    panic!("{case}: never reached the end");
}

#[test]
fn line_numbers_on() {
    let mut h = harness(Config::default());
    assert_each_notch_moves_one_row(&mut h, "line numbers on");
}

#[test]
fn line_numbers_off() {
    let mut config = Config::default();
    config.editor.line_numbers = false;
    let mut h = harness(config);
    assert_each_notch_moves_one_row(&mut h, "line numbers off");
}

#[test]
fn wrap_column() {
    let mut config = Config::default();
    config.editor.wrap_column = Some(70);
    let mut h = harness(config);
    assert_each_notch_moves_one_row(&mut h, "wrap_column 70");
}

#[test]
fn page_width_in_a_source_pane() {
    let mut h = harness(Config::default());
    set_page_width(&mut h, 70);
    assert_each_notch_moves_one_row(&mut h, "page width 70");
}

#[test]
fn page_width_with_line_numbers_off() {
    let mut config = Config::default();
    config.editor.line_numbers = false;
    let mut h = harness(config);
    set_page_width(&mut h, 70);
    assert_each_notch_moves_one_row(&mut h, "page width 70, line numbers off");
}
