//! End-to-end regression test for issue #2449: ANSI colors vanish on the
//! soft-wrapped continuation rows of a colored line.
//!
//! Terminal scrollback stores each terminal line as one *unwrapped* logical
//! line that the read-only view soft-wraps, so a long colored line occupies
//! several visual rows. The bug rendered only the *first* of those rows in
//! color; every wrapped continuation row fell back to the default foreground
//! (the colors "disappeared" the moment you entered scrollback). The same
//! buffer-level ANSI rendering powers viewing any colored log file, so this
//! test drives that flow directly: open an ANSI-colored file in a viewport
//! narrow enough to force wrapping and assert — from rendered output only —
//! that a wrapped continuation row keeps the color.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use ratatui::style::Color;
use tempfile::TempDir;

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

/// Foreground color of the first cell on `row` whose rendered character is
/// `ch`. Scans the whole row so the gutter (line numbers) is skipped
/// naturally — observes only what is painted on screen.
fn fg_of_first_char(harness: &EditorTestHarness, row: u16, ch: &str, width: u16) -> Option<Color> {
    (0..width)
        .find(|&x| harness.get_cell(x, row).as_deref() == Some(ch))
        .and_then(|x| harness.get_cell_style(x, row))
        .and_then(|style| style.fg)
}

#[test]
fn issue_2449_ansi_color_persists_across_wrapped_rows() {
    let temp_dir = TempDir::new().unwrap();
    let path = temp_dir.path().join("colored.log");

    // One logical line, colored truecolor red (`38;2;r;g;b` => Color::Rgb),
    // long enough to wrap several times at 40 columns. The 'X' marker recurs
    // every few characters so every wrapped row contains one.
    let red = "\x1b[38;2;220;50;47m";
    let reset = "\x1b[0m";
    let body = "REDX ".repeat(40); // 200 chars -> multiple wrapped rows
    std::fs::write(&path, format!("{red}{body}{reset}\n")).unwrap();

    let width = 40u16;
    let mut harness = EditorTestHarness::with_config(width, 24, config_with_wrap()).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    let expected = Color::Rgb(220, 50, 47);
    let (first_content_row, _) = harness.content_area_rows();
    let first_row = first_content_row as u16;
    let continuation_row = first_row + 1;

    // Precondition: the first row of the wrapped line is colored.
    assert_eq!(
        fg_of_first_char(&harness, first_row, "X", width),
        Some(expected),
        "precondition: first wrapped row should carry the ANSI color"
    );

    // Regression: the wrapped continuation row must ALSO carry the color.
    // Before the fix it rendered with the default foreground (color lost
    // after the soft-wrap).
    assert_eq!(
        fg_of_first_char(&harness, continuation_row, "X", width),
        Some(expected),
        "wrapped continuation row lost its ANSI color (issue #2449)"
    );
}
