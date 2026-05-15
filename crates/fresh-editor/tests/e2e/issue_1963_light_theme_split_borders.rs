//! Regression test for issue #1963: split borders look bad in Light theme.
//!
//! Before the fix, `render_separator` (and the matching hover render path) set
//! only `fg` on separator cells, leaving the cell `bg` as the terminal default.
//! In the Light theme, where editor panes are white, the separator strips
//! between panes leaked the terminal's default background (typically dark),
//! producing harsh dark bands between white panels. The fix sets `bg` to the
//! theme's `editor_bg` so the separator integrates with surrounding panes.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use ratatui::style::Color;

fn split_vertical(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

fn split_horizontal(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split horiz").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

fn find_cell_with_symbol(harness: &EditorTestHarness, symbol: &str) -> Option<(u16, u16)> {
    let buffer = harness.buffer();
    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            let pos = buffer.index_of(x, y);
            if let Some(cell) = buffer.content.get(pos) {
                if cell.symbol() == symbol {
                    return Some((x, y));
                }
            }
        }
    }
    None
}

#[test]
fn test_light_theme_vertical_split_separator_has_editor_bg() {
    let config = Config {
        theme: "light".into(),
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(120, 30, config).unwrap();
    let (expected_fg, expected_bg) = {
        let theme = harness.editor().theme();
        (theme.split_separator_fg, theme.editor_bg)
    };

    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    split_vertical(&mut harness);

    let (sep_x, sep_y) = find_cell_with_symbol(&harness, "│")
        .expect("vertical split should render at least one │ separator cell");

    let style = harness
        .get_cell_style(sep_x, sep_y)
        .expect("separator cell should have a style");

    assert_eq!(
        style.fg,
        Some(expected_fg),
        "separator cell fg should be theme.split_separator_fg"
    );
    assert_eq!(
        style.bg,
        Some(expected_bg),
        "separator cell bg should be theme.editor_bg (got {:?}); otherwise the separator \
         leaks the terminal default bg and looks like a harsh dark strip on light themes",
        style.bg
    );
    assert_ne!(
        style.bg,
        Some(Color::Reset),
        "separator cell bg should not be Color::Reset"
    );
}

#[test]
fn test_light_theme_horizontal_split_separator_has_editor_bg() {
    let config = Config {
        theme: "light".into(),
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(120, 30, config).unwrap();
    let (expected_fg, expected_bg) = {
        let theme = harness.editor().theme();
        (theme.split_separator_fg, theme.editor_bg)
    };

    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    split_horizontal(&mut harness);

    let (sep_x, sep_y) = find_cell_with_symbol(&harness, "─")
        .expect("horizontal split should render at least one ─ separator cell");

    let style = harness
        .get_cell_style(sep_x, sep_y)
        .expect("separator cell should have a style");

    assert_eq!(
        style.fg,
        Some(expected_fg),
        "separator cell fg should be theme.split_separator_fg"
    );
    assert_eq!(
        style.bg,
        Some(expected_bg),
        "separator cell bg should be theme.editor_bg (got {:?})",
        style.bg
    );
}
