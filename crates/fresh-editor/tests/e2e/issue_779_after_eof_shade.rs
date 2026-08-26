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
//! 4. A theme naming a *distinct* `after_eof_bg` paints it on those rows
//!    while the content rows stay on `editor.bg` — the assertions above
//!    cannot see that difference once the two colors are equal by default.
//! 5. With `editor.use_terminal_bg`, post-EOF rows follow the content rows
//!    onto the terminal's own background instead of painting an opaque
//!    `editor.bg` band over it.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::view::theme::Theme;
use ratatui::style::Color;
use std::fs;
use tempfile::TempDir;

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

/// A theme whose post-EOF rows are deliberately *not* the editor background.
const SHADED_THEME_JSON: &str = r#"{
    "name": "eof-shaded",
    "editor": {
        "bg": [30, 30, 30],
        "fg": [212, 212, 212],
        "after_eof_bg": [90, 20, 20]
    },
    "ui": {},
    "search": {},
    "diagnostic": {},
    "syntax": {}
}"#;

/// The two assertions above compare against `theme.after_eof_bg`, which now
/// equals `theme.editor_bg` for every theme that does not name it — so they
/// would still pass if the post-EOF rows stopped consulting the theme color
/// at all. This one separates the two colors so the rows have to pick the
/// right one.
#[test]
fn distinct_after_eof_bg_paints_only_the_post_eof_rows() {
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let themes_dir = temp_dir.path().join("config").join("themes");
    fs::create_dir_all(&themes_dir).unwrap();
    fs::write(themes_dir.join("eof-shaded.json"), SHADED_THEME_JSON).unwrap();

    let project_root = temp_dir.path().join("project_root");
    fs::create_dir_all(project_root.join("plugins")).unwrap();

    let mut config = Config::default();
    config.theme = "eof-shaded".to_string().into();
    config.editor.show_tilde = true;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_root)
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    // Two lines, so a content row can be sampled away from the cursor's own
    // line — the current-line highlight has a background of its own.
    harness.load_buffer_from_text("line one\nline two").unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.editor().theme().after_eof_bg,
        Color::Rgb(90, 20, 20),
        "the custom theme should be the active one"
    );

    let (first, _last) = harness.content_area_rows();
    let content_row = first as u16 + 1;
    let post_eof_row = first as u16 + 2;

    assert_eq!(
        harness.get_cell_style(5, content_row).and_then(|s| s.bg),
        Some(Color::Rgb(30, 30, 30)),
        "a content row must stay on editor.bg"
    );
    assert_eq!(
        harness.get_cell_style(5, post_eof_row).and_then(|s| s.bg),
        Some(Color::Rgb(90, 20, 20)),
        "a row past end-of-file must carry the theme's explicit after_eof_bg (#779)"
    );
}

/// `editor.use_terminal_bg` hands the buffer background to the terminal, so
/// content rows render on `Color::Reset`. Post-EOF rows resolve their color
/// from the theme, which knows nothing about that option: painting
/// `editor.bg` there put an opaque band over the terminal's own background,
/// exactly under the last line, for the users most likely to notice it.
#[test]
fn post_eof_rows_follow_use_terminal_bg() {
    let mut config = Config::default();
    config.editor.show_tilde = true;
    config.editor.use_terminal_bg = true;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Two lines, so the sampled content row is not the cursor's own line.
    harness.load_buffer_from_text("line one\nline two").unwrap();
    harness.render().unwrap();

    let (first, _last) = harness.content_area_rows();
    let content_bg = harness
        .get_cell_style(5, first as u16 + 1)
        .and_then(|s| s.bg)
        .expect("content row should exist");
    let post_eof_bg = harness
        .get_cell_style(5, first as u16 + 2)
        .and_then(|s| s.bg)
        .expect("post-EOF row should exist");

    assert_eq!(
        content_bg,
        Color::Reset,
        "use_terminal_bg should leave content rows on the terminal background"
    );
    assert_eq!(
        post_eof_bg,
        Color::Reset,
        "post-EOF rows must follow the content rows onto the terminal \
         background instead of painting an opaque editor.bg band"
    );
}
