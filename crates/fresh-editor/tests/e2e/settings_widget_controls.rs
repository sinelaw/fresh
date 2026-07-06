//! E2E coverage for the widget-rendered Settings controls — the user
//! flows fixed while unifying the Settings dialog onto the plugin widget
//! framework. Each test drives only keyboard events and asserts on
//! rendered output (per CONTRIBUTING.md, "E2E Tests Observe, Not
//! Inspect").
//!
//! - ObjectArray list navigation (Env › Detectors): Up/Down moves the
//!   focused row; the `[+] Add new` sentinel highlights when focused.
//! - Narrow terminal keeps a form toggle's `[v]` chip on-screen.
//! - An entry dialog's text field (a language's Grammar) aligns its
//!   value cell with the sibling toggles' chips.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Open Settings and step the sidebar down until the Env section's
/// Detectors list is on screen, then Tab into the content pane so the
/// ObjectArray control is focused.
fn focus_env_detectors(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();
    for _ in 0..40 {
        if harness.screen_to_string().contains("Detectors:") {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(
        harness.screen_to_string().contains("Detectors:"),
        "should reach the Env section's Detectors list; screen:\n{}",
        harness.screen_to_string()
    );
    // Move focus from the category sidebar into the content pane.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// The `>` focus indicator sits before whichever detector row is
/// focused; return that row's name (the token after `> `) if any.
fn focused_detector(harness: &EditorTestHarness) -> Option<String> {
    let names = [".venv", "venv", "direnv", "mise", "pipenv", "poetry"];
    for line in harness.screen_to_string().lines() {
        if let Some(pos) = line.find("> ") {
            let after = line[pos + 2..].trim_start();
            for n in names {
                if after.starts_with(n) {
                    return Some(n.to_string());
                }
            }
        }
    }
    None
}

/// A1: Up/Down navigates the entries of an ObjectArray list. Before the
/// fix `select_next`/`select_prev` only routed into `Map` controls, so
/// the focused row never moved off the first entry.
#[test]
fn object_array_list_navigates_entries() {
    let config = Config::default();
    assert!(
        config.env.detectors.len() >= 3,
        "precondition: several default detectors"
    );
    let mut harness = EditorTestHarness::with_config(120, 40, config).unwrap();
    harness.render().unwrap();
    focus_env_detectors(&mut harness);

    // Entering from above focuses the first entry.
    assert_eq!(
        focused_detector(&harness).as_deref(),
        Some(".venv"),
        "Tab into the list focuses the first detector; screen:\n{}",
        harness.screen_to_string()
    );

    // Down walks to a distinct, later entry. `direnv` is the 3rd default
    // detector — unreachable unless navigation actually advances.
    let mut reached = None;
    for _ in 0..6 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        if focused_detector(&harness).as_deref() == Some("direnv") {
            reached = Some("direnv");
            break;
        }
    }
    assert_eq!(
        reached,
        Some("direnv"),
        "Down should move focus through the entries to `direnv`; screen:\n{}",
        harness.screen_to_string()
    );

    // Up returns toward the first entry.
    for _ in 0..6 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        if focused_detector(&harness).as_deref() == Some(".venv") {
            break;
        }
    }
    assert_eq!(
        focused_detector(&harness).as_deref(),
        Some(".venv"),
        "Up should walk back to the first detector; screen:\n{}",
        harness.screen_to_string()
    );
}

/// A2: the `[+] Add new` sentinel highlights (selection background) when
/// it is the focused sub-row. Before the fix the add row rendered with
/// no highlight, so a keyboard user couldn't tell it was selected. This
/// asserts on the *rendered* cell background, not model state.
#[test]
fn object_array_add_new_row_highlights_when_focused() {
    let mut harness = EditorTestHarness::with_config(120, 40, Config::default()).unwrap();
    harness.render().unwrap();
    focus_env_detectors(&mut harness);

    // Locate the `[+] Add new` row and a plain-background reference cell
    // on it (a column in the trailing padding, past the label text) while
    // it is NOT focused.
    let add_row = (0..harness.buffer().area.height)
        .find(|&y| harness.get_row_text(y).contains("[+] Add new"))
        .expect("add-new row on screen");
    // A column well past "[+] Add new" — in the row's padding.
    let probe_x = {
        let line = harness.get_row_text(add_row);
        let start = line.find("[+] Add new").unwrap() as u16;
        start + 20
    };
    let bg_unfocused = harness.get_cell_style(probe_x, add_row).and_then(|s| s.bg);

    // Walk Down until the add-new row is the focused sub-row (past the
    // last entry). Bounded so a regression fails instead of looping.
    let mut focused_add = false;
    for _ in 0..12 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        // The add row is focused once no detector name carries the `>`.
        if focused_detector(&harness).is_none()
            && harness.screen_to_string().contains("[+] Add new")
        {
            focused_add = true;
            break;
        }
    }
    assert!(
        focused_add,
        "Down should reach the add-new sentinel; screen:\n{}",
        harness.screen_to_string()
    );

    let add_row_f = (0..harness.buffer().area.height)
        .find(|&y| harness.get_row_text(y).contains("[+] Add new"))
        .expect("add-new row still on screen");
    let bg_focused = harness
        .get_cell_style(probe_x, add_row_f)
        .and_then(|s| s.bg);
    assert!(
        bg_focused.is_some() && bg_focused != bg_unfocused,
        "the focused add-new row must paint a selection background \
         (was {bg_unfocused:?}, focused {bg_focused:?}); screen:\n{}",
        harness.screen_to_string()
    );
}

/// B1: on a narrow terminal a form toggle's `[v]`/`[ ]` chip stays
/// on-screen. Before the fix the label was padded to the page-wide
/// column, overflowing the control box and clipping the chip off the
/// right edge.
#[test]
fn narrow_terminal_keeps_toggle_chip_visible() {
    // Narrow width: the Editor page's long toggle labels + a wide label
    // column would push the chip past the box edge without the clamp.
    let mut harness = EditorTestHarness::with_config(80, 40, Config::default()).unwrap();
    harness.render().unwrap();
    harness.open_settings().unwrap();

    // Step the sidebar to the Editor section (its first control is the
    // "Highlight Matching Brackets" toggle).
    for _ in 0..40 {
        if harness
            .screen_to_string()
            .contains("Highlight Matching Brackets")
        {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    let screen = harness.screen_to_string();
    let row = screen
        .lines()
        .find(|l| l.contains("Highlight Matching Brackets"))
        .unwrap_or_else(|| panic!("Editor toggle row should be on screen; screen:\n{screen}"));
    // The chip must render on the same row as the label.
    assert!(
        row.contains("[v]") || row.contains("[ ]") || row.contains("[-]"),
        "the toggle chip must stay on-screen on a narrow terminal; row was:\n{row:?}"
    );
}

/// B2: an entry dialog's single-line text field aligns its value cell
/// with the sibling toggles' chips. Before the fix the Text widget
/// rendered `Grammar [value]` (cell right after the label) instead of
/// padding to the shared label column like the toggles do.
#[test]
fn language_entry_text_field_aligns_with_toggles() {
    // A config with a single language keeps the map to one row so the
    // dialog is reached deterministically.
    let mut config = Config::default();
    let keep = if config.languages.contains_key("html") {
        "html"
    } else {
        // Fall back to whatever the first language is.
        config
            .languages
            .keys()
            .next()
            .cloned()
            .expect("at least one language")
            .leak()
    };
    config.languages.retain(|name, _| name == keep);

    let mut harness = EditorTestHarness::with_config(120, 40, config).unwrap();
    harness.render().unwrap();
    harness.open_settings().unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // Walk to the language map row (its `[Enter to edit]` affordance) and
    // open the entry dialog.
    for _ in 0..40 {
        if harness.screen_to_string().contains("[Enter to edit]") {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("Grammar"),
        "language entry dialog should show the Grammar field; screen:\n{}",
        harness.screen_to_string()
    );

    // The Grammar field's `[` must open at the same column as a toggle's
    // chip `[`. Compare the Grammar row's bracket column to a toggle
    // row's (any `: [` form row on the dialog).
    let screen = harness.screen_to_string();
    let bracket_col = |needle: &str| -> Option<usize> {
        let line = screen.lines().find(|l| l.contains(needle))?;
        // The value cell's `[` is the first `[` after the `: ` column.
        let colon = line.find(": [")?;
        Some(colon + 2)
    };
    let grammar_col = bracket_col("Grammar").unwrap_or_else(|| {
        panic!("Grammar row should render an aligned `: [` cell; screen:\n{screen}")
    });
    // "Auto Indent" is a plain toggle in the language dialog.
    let toggle_col = bracket_col("Auto Indent")
        .unwrap_or_else(|| panic!("expected an `Auto Indent` toggle row; screen:\n{screen}"));
    assert_eq!(
        grammar_col, toggle_col,
        "the Grammar text field's value cell should align with the toggle chips \
         (grammar `[` at {grammar_col}, toggle `[` at {toggle_col}); screen:\n{screen}"
    );
}
