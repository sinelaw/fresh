//! Regression test for issue #779: UX enhancement — displaying lines after EOF.
//!
//! Before the enhancement, rows past end-of-file were either marked with a
//! `~` character (matching the theme's editor background) or left blank.
//! The issue requested a subtle background shade that distinguishes post-EOF
//! space from buffer content.
//!
//! The fix threads a new theme color `editor.after_eof_bg` (with a computed
//! default derived from `editor.bg`) into the row-fill path in
//! `render_line.rs`. This test verifies:
//!
//! 1. The default `after_eof_bg` differs from `editor.bg` — themes now
//!    produce a visible shade automatically.
//! 2. Rows past end-of-file carry the `after_eof_bg` background, regardless
//!    of whether `show_tilde` is enabled.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;

fn first_post_eof_row(harness: &EditorTestHarness, content_lines: usize) -> u16 {
    let (first, _last) = harness.content_area_rows();
    (first + content_lines) as u16
}

#[test]
fn default_theme_derives_distinct_after_eof_bg() {
    let harness = EditorTestHarness::with_config(80, 24, Config::default()).unwrap();
    let theme = harness.editor().theme();
    assert_ne!(
        theme.after_eof_bg, theme.editor_bg,
        "Default after_eof_bg must differ from editor.bg so that post-EOF \
         rows are visually distinguishable from buffer content (#779)"
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
