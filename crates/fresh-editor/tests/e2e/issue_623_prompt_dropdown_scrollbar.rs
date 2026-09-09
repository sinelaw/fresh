//! Reproducers for issue #623 (a suggestion dropdown that scrolls, but gives
//! no sign that entries are hidden — "at first glance, I thought only these")
//! and the remaining item of issue #1593 (the command palette rendered no
//! scrollbar at all). Both lists go through the same `SuggestionsRenderer`,
//! so one fix covers both: when the list overflows the dropdown, the shared
//! scrollbar widget is drawn over the popup's right border, and it responds
//! to clicks like every other scrollbar in the editor.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

/// The scrollbar sits on the popup's right border, i.e. the last column.
const SCROLLBAR_COL: u16 = 99;

/// The popup's suggestion rows: full-width popup rows start with the `│`
/// left border glyph (the top/bottom borders start with `┌`/`└`).
fn suggestion_rows(harness: &EditorTestHarness, height: u16) -> Vec<u16> {
    (0..height)
        .filter(|&y| harness.get_row_text(y).starts_with('│'))
        .collect()
}

/// Where the scrollbar thumb is drawn, as `(top, size)` in rows relative to
/// the top of the track. Read from the rendered cells only — the thumb is a
/// run of background-coloured cells inside the track.
fn thumb_span(harness: &EditorTestHarness, rows: &[u16]) -> (usize, usize) {
    let thumb: Vec<usize> = rows
        .iter()
        .enumerate()
        .filter(|(_, &y)| harness.is_scrollbar_thumb_at(SCROLLBAR_COL, y))
        .map(|(i, _)| i)
        .collect();
    assert!(
        !thumb.is_empty(),
        "expected a scrollbar thumb on the popup border:\n{}",
        harness.screen_to_string()
    );
    let top = thumb[0];
    assert_eq!(
        thumb.last().copied(),
        Some(top + thumb.len() - 1),
        "thumb rows must be contiguous, got {thumb:?}"
    );
    (top, thumb.len())
}

/// A dropdown row's text, with runs of padding collapsed: the column widths
/// are computed from the *visible* entries, so scrolling reflows them and
/// only the words identify the entry.
fn entry_text(harness: &EditorTestHarness, y: u16) -> String {
    harness
        .get_row_text(y)
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

/// Text of the highlighted suggestion row, or `None` when the selection has
/// scrolled out of the viewport. The selection is a background colour rather
/// than a glyph, so it's found as the one row whose background differs from
/// the rest of the list.
fn selected_row_text(harness: &EditorTestHarness, rows: &[u16]) -> Option<String> {
    use std::collections::HashMap;
    let bg_at = |y: u16| harness.get_cell_style(2, y).and_then(|s| s.bg);
    let mut counts: HashMap<_, usize> = HashMap::new();
    for &y in rows {
        *counts.entry(bg_at(y)).or_default() += 1;
    }
    let unselected = counts.into_iter().max_by_key(|&(_, n)| n)?.0;
    rows.iter()
        .copied()
        .find(|&y| bg_at(y) != unselected)
        .map(|y| entry_text(harness, y))
}

/// Open a prompt whose suggestion list overflows the 10-row dropdown, and
/// return its suggestion rows. `command` is run through the palette; passing
/// `None` leaves the palette itself open.
fn open_overflowing_dropdown(harness: &mut EditorTestHarness, command: Option<&str>) -> Vec<u16> {
    match command {
        Some(name) => harness.run_palette_command(name).unwrap(),
        None => harness
            .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap(),
    }
    harness.render().unwrap();
    let rows = suggestion_rows(harness, 24);
    assert_eq!(
        rows.len(),
        10,
        "dropdown must be full for the list to overflow:\n{}",
        harness.screen_to_string()
    );
    rows
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

/// Clicking track row R must leave the thumb's top *on* row R — the thumb
/// goes where you point it. The mapping used to divide the click position by
/// the full track height instead of by the thumb's actual travel, so every
/// click landed the thumb a row (or more) above the row clicked, and the
/// bottom row of the track could never reach the end of the list.
#[test]
fn test_clicking_palette_scrollbar_lands_thumb_on_the_clicked_row() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    let rows = open_overflowing_dropdown(&mut harness, None);

    let (_, thumb_size) = thumb_span(&harness, &rows);
    // Rows below `max_thumb_top` can't hold the thumb's top — the thumb
    // would hang off the end of the track — so they clamp to the bottom.
    let max_thumb_top = rows.len() - thumb_size;

    for target in 0..=max_thumb_top {
        harness.mouse_click(SCROLLBAR_COL, rows[target]).unwrap();
        let (top, size) = thumb_span(&harness, &rows);
        assert_eq!(
            top,
            target,
            "clicking track row {target} put the thumb at row {top}:\n{}",
            harness.screen_to_string()
        );
        assert_eq!(
            size, thumb_size,
            "thumb size must not change while scrolling"
        );
    }

    // Both extremes, explicitly: the last row of the track scrolls to the
    // very end of the list, the first row back to the very start.
    harness
        .mouse_click(SCROLLBAR_COL, *rows.last().unwrap())
        .unwrap();
    let (top, size) = thumb_span(&harness, &rows);
    assert_eq!(
        top + size,
        rows.len(),
        "clicking the last track row must park the thumb at the bottom:\n{}",
        harness.screen_to_string()
    );
    harness.assert_screen_not_contains("Add Cursor Above");

    harness.mouse_click(SCROLLBAR_COL, rows[0]).unwrap();
    assert_eq!(
        thumb_span(&harness, &rows).0,
        0,
        "clicking the first track row must return to the top of the list"
    );
    harness.assert_screen_contains("Add Cursor Above");
}

/// The drag follow-up uses the same mapping as the press, so the thumb keeps
/// tracking the cursor row-for-row while the button is held. Driven as one
/// continuous gesture — press once, walk the cursor down the track and back
/// up — so it exercises the drag handler rather than a series of clicks.
#[test]
fn test_dragging_palette_scrollbar_tracks_the_cursor_row() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    let rows = open_overflowing_dropdown(&mut harness, None);

    let (_, thumb_size) = thumb_span(&harness, &rows);
    let max_thumb_top = rows.len() - thumb_size;

    let press = |h: &mut EditorTestHarness, y: u16| {
        h.send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: SCROLLBAR_COL,
            row: y,
            modifiers: KeyModifiers::empty(),
        })
        .unwrap();
    };
    let drag_to = |h: &mut EditorTestHarness, y: u16| {
        h.send_mouse(MouseEvent {
            kind: MouseEventKind::Drag(MouseButton::Left),
            column: SCROLLBAR_COL,
            row: y,
            modifiers: KeyModifiers::empty(),
        })
        .unwrap();
        h.render().unwrap();
    };
    let release = |h: &mut EditorTestHarness, y: u16| {
        h.send_mouse(MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: SCROLLBAR_COL,
            row: y,
            modifiers: KeyModifiers::empty(),
        })
        .unwrap();
        h.render().unwrap();
    };

    press(&mut harness, rows[0]);
    let sweep: Vec<usize> = (0..=max_thumb_top)
        .chain((0..max_thumb_top).rev())
        .collect();
    for target in sweep {
        drag_to(&mut harness, rows[target]);
        assert_eq!(
            thumb_span(&harness, &rows).0,
            target,
            "dragging to track row {target} left the thumb elsewhere:\n{}",
            harness.screen_to_string()
        );
    }
    release(&mut harness, rows[0]);
}

/// The mouse wheel scrolls the VIEW only: it must never move the selection.
///
/// Wheeling used to walk `selected_suggestion` instead, which rewrote the
/// prompt input under the user and — once a scrollbar click could pin the
/// viewport — made the list visibly jump, because the wheel released that
/// pin and the renderer snapped back to a selection that had not moved on
/// screen. Asserted on the palette, which is the surface the report used.
#[test]
fn test_wheel_over_palette_scrolls_view_without_moving_selection() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    let rows = open_overflowing_dropdown(&mut harness, None);

    // Put the selection a few rows down so it stays on screen after the
    // wheel — that's what makes "the same entry is still highlighted" an
    // observation rather than a vacuous absence.
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    let selected_before = selected_row_text(&harness, &rows).expect("an entry must be highlighted");
    let first_before = entry_text(&harness, rows[0]);

    harness.mouse_scroll_down(50, rows[5]).unwrap();

    assert_ne!(
        entry_text(&harness, rows[0]),
        first_before,
        "the wheel must scroll the list:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_row_text(&harness, &rows).as_deref(),
        Some(selected_before.as_str()),
        "the wheel must leave the selection on the same entry:\n{}",
        harness.screen_to_string()
    );

    // Scrolling back restores the view, selection still untouched.
    harness.mouse_scroll_up(50, rows[5]).unwrap();
    assert_eq!(entry_text(&harness, rows[0]), first_before);
    assert_eq!(
        selected_row_text(&harness, &rows).as_deref(),
        Some(selected_before.as_str())
    );
}

/// Same rule on the Select Locale picker (issue #623's own dropdown): a
/// different `PromptType`, the same shared suggestions renderer.
#[test]
fn test_wheel_over_select_locale_does_not_move_selection() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    let rows = open_overflowing_dropdown(&mut harness, Some("Select Locale"));

    for _ in 0..2 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    let selected_before = selected_row_text(&harness, &rows).expect("a locale must be highlighted");
    let first_before = entry_text(&harness, rows[0]);

    harness.mouse_scroll_down(50, rows[5]).unwrap();

    assert_ne!(
        entry_text(&harness, rows[0]),
        first_before,
        "the wheel must scroll the locale list:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_row_text(&harness, &rows).as_deref(),
        Some(selected_before.as_str()),
        "the wheel must leave the selected locale alone:\n{}",
        harness.screen_to_string()
    );
}

/// And on a third prompt-driven list — Set Language, whose suggestions come
/// from the grammar catalogue rather than from the command registry.
#[test]
fn test_wheel_over_set_language_does_not_move_selection() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    let rows = open_overflowing_dropdown(&mut harness, Some("Set Language"));

    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    let selected_before =
        selected_row_text(&harness, &rows).expect("a language must be highlighted");
    let first_before = entry_text(&harness, rows[0]);

    harness.mouse_scroll_down(50, rows[5]).unwrap();

    assert_ne!(
        entry_text(&harness, rows[0]),
        first_before,
        "the wheel must scroll the language list:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_row_text(&harness, &rows).as_deref(),
        Some(selected_before.as_str()),
        "the wheel must leave the selected language alone:\n{}",
        harness.screen_to_string()
    );
}

/// The wheel may scroll the selection clean off the list — that's correct
/// (VS Code does the same), and the selection must survive it: scrolling
/// back brings the very same entry back, still highlighted.
#[test]
fn test_wheel_may_scroll_selection_out_of_view_without_losing_it() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    let rows = open_overflowing_dropdown(&mut harness, None);

    let selected_before = selected_row_text(&harness, &rows).expect("an entry must be highlighted");

    // Four notches of 3 rows: the first entry is far off the top now.
    for _ in 0..4 {
        harness.mouse_scroll_down(50, rows[5]).unwrap();
    }
    assert_eq!(
        selected_row_text(&harness, &rows),
        None,
        "the selection scrolled out of view, so no row is highlighted:\n{}",
        harness.screen_to_string()
    );

    for _ in 0..4 {
        harness.mouse_scroll_up(50, rows[5]).unwrap();
    }
    assert_eq!(
        selected_row_text(&harness, &rows).as_deref(),
        Some(selected_before.as_str()),
        "scrolling back must reveal the same selected entry:\n{}",
        harness.screen_to_string()
    );
}

/// Keyboard navigation still re-engages keep-the-selection-visible
/// scrolling: after wheeling the selection off screen, an arrow key brings
/// the view back to it. This is what keeps the manual-scroll latch honest
/// now that the wheel no longer touches the selection.
#[test]
fn test_arrow_key_after_wheel_brings_the_selection_back_into_view() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    let rows = open_overflowing_dropdown(&mut harness, None);

    for _ in 0..4 {
        harness.mouse_scroll_down(50, rows[5]).unwrap();
    }
    assert_eq!(selected_row_text(&harness, &rows), None);

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        selected_row_text(&harness, &rows).is_some(),
        "an arrow key must scroll the selection back into view:\n{}",
        harness.screen_to_string()
    );
}
