//! Reproduction for issue #2888: `editor.use_terminal_bg = true` breaks
//! light themes.
//!
//! `use_terminal_bg` substitutes `Color::Reset` (the terminal's own default
//! background) for `theme.editor_bg` on buffer surfaces — see
//! `orchestration/render_buffer.rs` (`effective_editor_bg`) and
//! `orchestration/render_composite.rs`. Two things go wrong with a light
//! theme in a terminal whose own background is dark:
//!
//! 1. The *foreground* is not substituted along with the background. Buffer
//!    text keeps the light theme's near-black `editor_fg` but is now painted
//!    on the terminal's (dark) default background — black on black.
//! 2. The substitution only reaches plain buffer cells. The menu bar, the
//!    status bar and every cell that carries an explicit background (the
//!    current-line highlight, selections, plugin overlays, the gutter) keep
//!    their light-theme backgrounds, so the window becomes the patchwork of
//!    light and dark panels shown in the issue's screenshots.
//!
//! These tests assert the *broken* behaviour so the reproduction is
//! executable; the assertions marked `EXPECTED-FAIL-AFTER-FIX` are the ones a
//! fix should invalidate.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use ratatui::style::{Color, Style};

fn light_theme_config(use_terminal_bg: bool) -> Config {
    let mut config = Config {
        theme: "light".into(),
        ..Default::default()
    };
    config.editor.use_terminal_bg = use_terminal_bg;
    config.editor.animations = false;
    config
}

/// Type two lines so the cursor (and thus the current-line highlight, which
/// paints its own background) sits on line 2, leaving line 1 as a plain
/// buffer row.
fn type_two_lines(harness: &mut EditorTestHarness) {
    harness.type_text("PLAIN_ROW\nCURSOR_ROW").unwrap();
    harness.render().unwrap();
}

fn style_of_text(harness: &EditorTestHarness, needle: &str) -> Style {
    let (x, y) = harness.find_text_on_screen(needle).unwrap_or_else(|| {
        panic!(
            "expected {needle:?} on screen:\n{}",
            harness.screen_to_string()
        )
    });
    harness
        .get_cell_style(x, y)
        .expect("cell should have a style")
}

/// Core of the bug: buffer text keeps the light theme's dark foreground while
/// its background is reset to the terminal default.
#[test]
fn issue_2888_light_theme_text_is_dark_on_terminal_default_bg() {
    let mut harness = EditorTestHarness::with_config(120, 30, light_theme_config(true)).unwrap();
    let (theme_bg, theme_fg) = {
        let theme = harness.editor().theme();
        (theme.editor_bg, theme.editor_fg)
    };

    // Sanity: the "light" theme really is light (near-white bg, near-black fg).
    assert!(
        matches!(theme_bg, Color::Rgb(r, g, b) if r > 200 && g > 200 && b > 200),
        "light theme editor_bg should be near-white, got {theme_bg:?}"
    );
    assert!(
        matches!(theme_fg, Color::Rgb(r, g, b) if r < 80 && g < 80 && b < 80),
        "light theme editor_fg should be near-black, got {theme_fg:?}"
    );

    type_two_lines(&mut harness);
    let style = style_of_text(&harness, "PLAIN_ROW");

    // EXPECTED-FAIL-AFTER-FIX: a fix should either keep the theme bg here or
    // reset the fg alongside it, so this pair should stop being
    // (near-black fg, terminal-default bg).
    assert_eq!(
        style.bg,
        Some(Color::Reset),
        "with use_terminal_bg=true the buffer background is the terminal default. Screen:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        style.fg,
        Some(theme_fg),
        "but the foreground is still the light theme's near-black editor_fg — \
         unreadable on a dark terminal. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Control: with `use_terminal_bg = false` the same text is painted on the
/// theme's light background and stays readable.
#[test]
fn issue_2888_control_use_terminal_bg_false_is_readable() {
    let mut harness = EditorTestHarness::with_config(120, 30, light_theme_config(false)).unwrap();
    let (theme_bg, theme_fg) = {
        let theme = harness.editor().theme();
        (theme.editor_bg, theme.editor_fg)
    };

    type_two_lines(&mut harness);
    let style = style_of_text(&harness, "PLAIN_ROW");

    assert_eq!(
        style.bg,
        Some(theme_bg),
        "control: buffer text should sit on the theme's light background"
    );
    assert_eq!(style.fg, Some(theme_fg));
}

/// The patchwork: the substitution reaches plain buffer cells only, so the
/// chrome (menu bar, status bar) and every explicitly-backgrounded cell (here
/// the current-line highlight) keep the light theme's background on the same
/// screen as the reset rows.
#[test]
fn issue_2888_light_and_terminal_backgrounds_coexist_on_one_screen() {
    let mut harness = EditorTestHarness::with_config(120, 30, light_theme_config(true)).unwrap();
    let (menu_bg, status_bg, current_line_bg) = {
        let theme = harness.editor().theme();
        (theme.menu_bg, theme.status_bar_bg, theme.current_line_bg)
    };

    type_two_lines(&mut harness);

    let plain_row_bg = style_of_text(&harness, "PLAIN_ROW").bg;
    let cursor_row_bg = style_of_text(&harness, "CURSOR_ROW").bg;
    let menu_cell_bg = harness.get_cell_style(2, 0).expect("menu bar cell").bg;
    let status_row = harness.terminal_height() as u16 - 2;
    let status_cell_bg = harness
        .get_cell_style(2, status_row)
        .expect("status bar cell")
        .bg;

    assert_eq!(
        plain_row_bg,
        Some(Color::Reset),
        "plain buffer rows fall through to the terminal background"
    );
    assert_eq!(
        cursor_row_bg,
        Some(current_line_bg),
        "the current-line highlight keeps its light-theme background"
    );
    assert_eq!(
        menu_cell_bg,
        Some(menu_bg),
        "the menu bar keeps its light-theme background"
    );
    assert_eq!(
        status_cell_bg,
        Some(status_bg),
        "the status bar keeps its light-theme background"
    );

    // EXPECTED-FAIL-AFTER-FIX: the mixture itself is the defect — adjacent
    // rows of the same editor pane disagree about what the background is.
    assert_ne!(
        plain_row_bg, cursor_row_bg,
        "adjacent editor rows disagree about the background — this is the patchwork \
         in the issue's screenshots. Screen:\n{}",
        harness.screen_to_string()
    );
    assert_ne!(plain_row_bg, menu_cell_bg);
    assert_ne!(plain_row_bg, status_cell_bg);
}
