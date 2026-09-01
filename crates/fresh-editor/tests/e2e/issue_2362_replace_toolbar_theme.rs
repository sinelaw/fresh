//! Regression tests for issue #2362.
//!
//! Two distinct toolbar/theme-inspector bugs surfaced from the Replace
//! command's search-options toolbar:
//!
//! 1. **Checked checkbox invisible in Dracula.** The "active" (checked)
//!    option style painted `menu_highlight_fg` on `menu_dropdown_bg`. In
//!    themes where those two colors are equal (Dracula: both `[40,42,54]`)
//!    the checked checkbox label rendered fg-on-same-bg and vanished. The
//!    fix uses the `menu_active_fg`/`menu_active_bg` designed pair (the same
//!    `menu_*` family the toolbar already uses for its base and hover states),
//!    which contrasts on every theme.
//!
//! 2. **Theme inspector popup is empty + has a dead button on the toolbar.**
//!    The search-options toolbar didn't record per-cell theme keys, so
//!    Ctrl+Right-clicking it produced a popup with a blank `Region:` and a
//!    "▶ Open in Theme Editor" button that did nothing. The inspector grew a
//!    fallback for that — an explanatory message, and no button — and this
//!    file asserted it here, on the toolbar.
//!
//!    **The toolbar is no longer that case.** It is a described surface now,
//!    and `FoldProvenance` files every described item's resolved key pair as
//!    the display list is folded (`app/render.rs`), so the cell under the
//!    pointer answers with the very keys test 1 pins: `ui.menu_active_fg` on
//!    `ui.menu_active_bg`. The button is live, and asserting the fallback
//!    here would be asserting that the provenance is still missing. So the
//!    second test asserts what the inspector reports instead. The fallback
//!    itself is unchanged and still covers a cell that genuinely records
//!    nothing — an item whose ink resolved to literal colours rather than to
//!    a key (defect F.2 in the retained-mode plan).

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

    let (active_fg, active_bg) = {
        let theme = harness.editor().theme();
        (theme.menu_active_fg, theme.menu_active_bg)
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

    // The checked state uses the theme-designed `menu_active_*` pair.
    assert_eq!(
        style.fg,
        Some(active_fg),
        "checked option fg should be theme.menu_active_fg"
    );
    assert_eq!(
        style.bg,
        Some(active_bg),
        "checked option bg should be theme.menu_active_bg"
    );
}

/// Ctrl+Right-click on the toolbar opens the theme inspector *on the
/// toolbar's own keys*: the checked option is painted from the
/// `menu_active_*` pair (test 1 above pins that), so that is what the popup
/// must name, and the "Open in Theme Editor" button has somewhere to go.
#[test]
fn test_theme_inspector_reports_the_toolbars_theme_keys() {
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

    // The clicked cell is the checked "Case Sensitive" option, which test 1
    // pins to the `menu_active_*` pair — so those are the keys the inspector
    // must report, and it must not fall back to the no-key message.
    harness.assert_screen_contains("ui.menu_active_fg");
    harness.assert_screen_contains("ui.menu_active_bg");
    harness.assert_screen_not_contains("No theme key recorded here.");
    // With a key to open, the button is offered again.
    harness.assert_screen_contains("Open in Theme Editor");
}
