//! Regression test for issue #779: UX enhancement — displaying lines after EOF.
//!
//! #779 asked for post-EOF rows to be shaded instead of being marked with a
//! `~`, and the fix threaded a theme color `editor.after_eof_bg` into the
//! row-fill path. The derived default (a shade off `editor.bg`) turned the
//! space below a short buffer into a grayed out strip that matched no color
//! in the active theme, so the default now follows `editor.bg`; a theme that
//! wants the shade names `after_eof_bg` explicitly.
//!
//! This test verifies:
//!
//! 1. The default `after_eof_bg` equals `editor.bg`, so post-EOF space
//!    stays on the theme's main background.
//! 2. A theme that names `after_eof_bg` still gets that color.
//! 3. Rows past end-of-file carry `after_eof_bg`, regardless of whether
//!    `show_tilde` is enabled.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use fresh::view::theme::Theme;
use ratatui::style::Color;

fn first_post_eof_row(harness: &EditorTestHarness, content_lines: usize) -> u16 {
    let (first, _last) = harness.content_area_rows();
    (first + content_lines) as u16
}

#[test]
fn default_theme_keeps_after_eof_bg_on_the_editor_background() {
    let harness = EditorTestHarness::with_config(80, 24, Config::default()).unwrap();
    let theme = harness.editor().theme();
    assert_eq!(
        theme.after_eof_bg, theme.editor_bg,
        "Without an explicit override, post-EOF rows must stay on the \
         theme's editor background rather than a derived shade"
    );
}

#[test]
fn explicit_after_eof_bg_is_honored() {
    let theme = Theme::from_json(
        r#"{
            "name": "post-eof-shade",
            "editor": { "bg": [30, 30, 30], "after_eof_bg": [60, 60, 60] }
        }"#,
    )
    .expect("theme fixture should parse");
    assert_eq!(theme.editor_bg, Color::Rgb(30, 30, 30));
    assert_eq!(
        theme.after_eof_bg,
        Color::Rgb(60, 60, 60),
        "a theme that names after_eof_bg still gets the post-EOF shade (#779)"
    );
}

#[test]
fn post_eof_rows_use_after_eof_bg_with_tilde_enabled() {
    let mut config = Config::default();
    config.editor.show_tilde = true;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let expected = harness.editor().theme().after_eof_bg;

    harness.load_buffer_from_text("one line").unwrap();
    harness.render().unwrap();

    let row = first_post_eof_row(&harness, 1);
    let gutter_width = 5; // sample a column well past the gutter
    let style = harness
        .get_cell_style(gutter_width, row)
        .expect("cell should exist in post-EOF row");
    assert_eq!(
        style.bg,
        Some(expected),
        "post-EOF row bg must be theme.after_eof_bg when tildes are shown (#779)"
    );
}

#[test]
fn post_eof_rows_use_after_eof_bg_with_tilde_disabled() {
    let mut config = Config::default();
    config.editor.show_tilde = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let expected = harness.editor().theme().after_eof_bg;

    harness.load_buffer_from_text("one line").unwrap();
    harness.render().unwrap();

    let row = first_post_eof_row(&harness, 1);
    // With tilde disabled, the leftmost content column should still carry
    // the post-EOF background shade (no `~` glyph is drawn).
    let style = harness
        .get_cell_style(5, row)
        .expect("cell should exist in post-EOF row");
    assert_eq!(
        style.bg,
        Some(expected),
        "post-EOF row bg must be theme.after_eof_bg when tildes are hidden (#779)"
    );
}
