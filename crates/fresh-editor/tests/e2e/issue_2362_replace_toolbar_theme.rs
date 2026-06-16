//! Regression tests for issue #2362.
//!
//! Two distinct toolbar/theme-inspector bugs surfaced from the Replace
//! command's search-options toolbar:
//!
//! 1. **Checked checkbox invisible in Dracula.** The "active" (checked)
//!    option style painted `menu_highlight_fg` on `menu_dropdown_bg`. In
//!    themes where those two colors are equal (Dracula: both `[40,42,54]`)
//!    the checked checkbox label rendered fg-on-same-bg and vanished. The
//!    fix keeps the toolbar background (`menu_dropdown_bg`) and signals the
//!    checked state with the accent foreground `help_key_fg`, which every
//!    theme tunes to contrast with the panel/popup background.
//!
//! 2. **Theme inspector popup is empty + has a dead button on the toolbar.**
//!    The search-options toolbar doesn't record per-cell theme keys, so
//!    Ctrl+Right-clicking it produced a popup with a blank `Region:` and a
//!    "▶ Open in Theme Editor" button that did nothing. The inspector now
//!    shows an explanatory message and omits the non-functional button when
//!    no theme key is recorded for the clicked cell.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use fresh::config::Config;

/// Open the (non-interactive) Replace command, which shows the search-options
/// toolbar with the Case Sensitive / Whole Word / Regex / Confirm checkboxes.
fn open_replace(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    // The toolbar should now be on screen with Case Sensitive checked
    // (its default state).
    harness.assert_screen_contains("Case Sensitive");
}

#[test]
fn test_dracula_checked_option_is_visible() {
    let config = Config {
        theme: "dracula".into(),
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(120, 30, config).unwrap();

    let (accent_fg, toolbar_bg, unchecked_fg) = {
        let theme = harness.editor().theme();
        (
            theme.help_key_fg,
            theme.menu_dropdown_bg,
            theme.menu_dropdown_fg,
        )
    };

    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    open_replace(&mut harness);

    // Case Sensitive is checked by default, so its label is drawn with the
    // "active" style. Locate the label and inspect a cell within it.
    harness.assert_screen_contains("[x] Case Sensitive");
    let (label_col, label_row) = harness
        .find_text_on_screen("Case Sensitive")
        .expect("Case Sensitive label should be on the toolbar");

    let style = harness
        .get_cell_style(label_col, label_row)
        .expect("checked option label cell should have a style");

    // The core regression: a checked option must not be invisible
    // (foreground identical to background).
    assert_ne!(
        style.fg, style.bg,
        "checked Case Sensitive option must not render fg-on-same-bg (invisible); got fg={:?} bg={:?}",
        style.fg, style.bg
    );

    // The checked state keeps the toolbar background and differs only in the
    // foreground, which uses the accent `help_key_fg` (legible on every theme,
    // and distinct from the unchecked foreground).
    assert_eq!(
        style.bg,
        Some(toolbar_bg),
        "checked option should keep the toolbar bg (theme.menu_dropdown_bg)"
    );
    assert_eq!(
        style.fg,
        Some(accent_fg),
        "checked option fg should be the accent theme.help_key_fg"
    );
    assert_ne!(
        style.fg,
        Some(unchecked_fg),
        "checked option fg should be visually distinct from the unchecked fg"
    );
}

#[test]
fn test_theme_inspector_shows_message_instead_of_dead_button_on_toolbar() {
    let config = Config {
        theme: "dracula".into(),
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(120, 30, config).unwrap();

    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    open_replace(&mut harness);

    let (label_col, label_row) = harness
        .find_text_on_screen("Case Sensitive")
        .expect("Case Sensitive label should be on the toolbar");

    // Ctrl+Right-click the toolbar checkbox to open the theme inspector.
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Right),
            column: label_col,
            row: label_row,
            modifiers: KeyModifiers::CONTROL,
        })
        .unwrap();
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Right),
            column: label_col,
            row: label_row,
            modifiers: KeyModifiers::CONTROL,
        })
        .unwrap();
    harness.render().unwrap();

    // The toolbar has no recorded theme key, so the inspector must show a
    // clear message rather than a button that silently does nothing.
    harness.assert_screen_contains("No theme key recorded here.");
    harness.assert_screen_not_contains("Open in Theme Editor");
}
