//! Reproducers for issue #623 (a suggestion dropdown that scrolls, but gives
//! no sign that entries are hidden — "at first glance, I thought only these")
//! and the remaining item of issue #1593 (the command palette rendered no
//! scrollbar at all). Both lists go through the same `SuggestionsRenderer`,
//! so one fix covers both: when the list overflows the dropdown, the shared
//! scrollbar widget is drawn over the popup's right border, and it responds
//! to clicks like every other scrollbar in the editor.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// The popup's suggestion rows: full-width popup rows start with the `│`
/// left border glyph (the top/bottom borders start with `┌`/`└`).
fn suggestion_rows(harness: &EditorTestHarness, height: u16) -> Vec<u16> {
    (0..height)
        .filter(|&y| harness.get_row_text(y).starts_with('│'))
        .collect()
}

/// Issue #1593: the command palette lists far more commands than fit, so
/// every suggestion row must carry a scrollbar cell on the popup's right
/// border. Before the fix that column was a plain `│` frame line, so nothing
/// signalled that the list continued.
#[test]
fn test_command_palette_overflow_draws_scrollbar() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Cursor Above");

    let rows = suggestion_rows(&harness, 24);
    assert!(!rows.is_empty(), "command palette must render suggestions");

    let scrollbar_col = 99;
    let scrollbar_cells = rows
        .iter()
        .filter(|&&y| {
            harness.is_scrollbar_thumb_at(scrollbar_col, y)
                || harness.is_scrollbar_track_at(scrollbar_col, y)
        })
        .count();
    assert_eq!(
        scrollbar_cells,
        rows.len(),
        "every suggestion row must carry a scrollbar track/thumb cell on the right border"
    );
}

/// Issue #623 verbatim: the Select Locale dropdown shows 10 of the 14
/// locales. It scrolled even before the fix — what was missing was any
/// indication that more entries existed, which is now the right-border
/// scrollbar.
#[test]
fn test_select_locale_dropdown_shows_scrollbar_for_hidden_entries() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Select Locale").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The locale list is open: 14 locales, 10 rows of room, so 4 are hidden.
    let rows = suggestion_rows(&harness, 24);
    assert_eq!(
        rows.len(),
        10,
        "locale dropdown must fill its 10 rows with the 14 locales"
    );

    let scrollbar_col = 99;
    let scrollbar_cells = rows
        .iter()
        .filter(|&&y| {
            harness.is_scrollbar_thumb_at(scrollbar_col, y)
                || harness.is_scrollbar_track_at(scrollbar_col, y)
        })
        .count();
    assert_eq!(
        scrollbar_cells,
        rows.len(),
        "the hidden locales must be signalled by a scrollbar on the right border"
    );
}

/// A list that fits the dropdown keeps its plain `│` right border — the
/// indicator only appears when entries are actually hidden.
#[test]
fn test_short_suggestion_list_shows_no_scrollbar() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Toggle Line Wrap").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Toggle Line Wrap");

    let rows = suggestion_rows(&harness, 24);
    assert!(
        !rows.is_empty() && rows.len() < 10,
        "filtered list should fit the dropdown, got {} rows",
        rows.len()
    );
    for &y in &rows {
        assert!(
            harness.get_row_text(y).trim_end().ends_with('│'),
            "row {y} must keep its plain right border when the list fits"
        );
    }
}

/// Issue #1593 asked for a palette scrollbar that behaves like the editor's:
/// clicking low on the track must jump the list there and keep it there
/// (the renderer's keep-selection-visible pass must not yank it back).
#[test]
fn test_clicking_palette_scrollbar_scrolls_the_list() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Cursor Above");

    let rows = suggestion_rows(&harness, 24);
    let bottom_row = *rows.last().expect("popup must have suggestion rows");

    // Click the bottom of the scrollbar track: the list must jump towards
    // the end, so the alphabetically-first command scrolls out of view.
    harness.mouse_click(99, bottom_row).unwrap();
    harness.render().unwrap();

    harness.assert_screen_not_contains("Add Cursor Above");
}
