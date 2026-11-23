//! End-to-end tests for how the built-in terminal draws a program's palette
//! colors (sinelaw/fresh#3210).
//!
//! A program that prints "red" (SGR 31) asks for the terminal's red, not for
//! a particular RGB value. Like tmux, Fresh hands that palette number on to
//! the host terminal, so the built-in terminal shows the user's own palette —
//! on the live screen and in the scrollback alike.
//!
//! Program output is fed straight into the grid, as in `terminal_link.rs`
//! (whose helpers these tests share).

use super::terminal_link::{enter_scrollback_showing, harness_or_skip, open_terminal_with_output};
use crate::common::harness::EditorTestHarness;
use ratatui::style::Color;

/// The colors the renderer gave the first cell of `text` on screen, as
/// (foreground, background).
fn colors_of(harness: &EditorTestHarness, text: &str) -> (Option<Color>, Option<Color>) {
    let (x, y) = harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{text:?} is not on screen:\n{}", harness.screen_to_string()));
    let style = harness.get_cell_style(x, y).unwrap_or_default();
    (style.fg, style.bg)
}

/// Program output using the palette: SGR 31 (red), SGR 91 (bright red),
/// SGR 38;5;123 (a 256-color entry) and SGR 44 (blue background).
const PALETTE_OUTPUT: &[u8] = b"\x1b[31mPALRED\x1b[0m \x1b[91mBRIGHTRED\x1b[0m \
\x1b[38;5;123mCUBE123\x1b[0m \x1b[44mBLUEBG\x1b[0m plain";

/// Assert that each marker in `PALETTE_OUTPUT` is drawn with the palette
/// number its program asked for; `view` names the view under test.
fn assert_palette_numbers(harness: &EditorTestHarness, view: &str) {
    assert_eq!(
        colors_of(harness, "PALRED").0,
        Some(Color::Indexed(1)),
        "{view}: SGR 31"
    );
    assert_eq!(
        colors_of(harness, "BRIGHTRED").0,
        Some(Color::Indexed(9)),
        "{view}: SGR 91"
    );
    assert_eq!(
        colors_of(harness, "CUBE123").0,
        Some(Color::Indexed(123)),
        "{view}: SGR 38;5;123"
    );
    assert_eq!(
        colors_of(harness, "BLUEBG").1,
        Some(Color::Indexed(4)),
        "{view}: SGR 44"
    );
}

/// Palette colors are drawn as palette numbers, for the host terminal's own
/// palette to resolve, on the live screen and once they are in the
/// scrollback.
#[test]
fn palette_colors_stay_palette_numbers_live_and_in_scrollback() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let Some(mut harness) = harness_or_skip(80, 24, temp_dir.path().to_path_buf()) else {
        return;
    };
    let (terminal_id, _) = open_terminal_with_output(&mut harness, PALETTE_OUTPUT, "BLUEBG");
    assert_palette_numbers(&harness, "live");

    enter_scrollback_showing(&mut harness, terminal_id, PALETTE_OUTPUT, "BLUEBG");
    assert_palette_numbers(&harness, "scrollback");
}
