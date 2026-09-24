//! Regression tests for issue #3329: a drag along a row near the top (or
//! bottom) of the viewport scrolled the view on every motion, so the
//! selection ran away from the pointer — backwards from the anchor along
//! the top rows.
//!
//! Each drag event let the render pass apply the scroll-off margin
//! (`editor.scroll_offset`, 3 by default) to the selection head. A head on
//! one of the first three rows scrolled the view up three lines, and the
//! next motion at the same screen row then named a line three higher. The
//! anchor line ended up with only its prefix selected and the lines above
//! it fully selected.
//!
//! A drag *past* the edge still scrolls (issue #3006, covered by
//! `issue_3006_drag_beyond_text_area`); inside the text area the head is the
//! cell under the pointer, so nothing needs to scroll.
//!
//! Assertions read rendered output only: the gutter's line numbers and the
//! cells painted with the selection background.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

const LINE: &str = "this is a text";

fn mouse(harness: &mut EditorTestHarness, kind: MouseEventKind, col: u16, row: u16) {
    harness
        .send_mouse(MouseEvent {
            kind,
            column: col,
            row,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
}

/// The gutter's line number on screen `row`, if the row shows a line.
fn gutter_line(harness: &EditorTestHarness, row: u16) -> Option<u32> {
    harness
        .screen_row_text(row)
        .split_once('│')
        .and_then(|(gutter, _)| gutter.trim().parse().ok())
}

/// The characters painted with the selection background on `row` (the
/// in-selection whitespace marker folded back to a space).
fn selected_text(harness: &EditorTestHarness, row: u16) -> String {
    let bg = harness.editor().theme().selection_bg;
    (0..harness.buffer().area.width)
        .filter(|&col| harness.get_cell_style(col, row).and_then(|s| s.bg) == Some(bg))
        .filter_map(|col| harness.get_cell(col, row))
        .collect::<String>()
        .replace('·', " ")
}

/// Every content row, with its gutter line number and selected text, for
/// failure messages and for "nothing else is selected" checks.
fn selected_rows(harness: &EditorTestHarness) -> Vec<(u32, String)> {
    (0..harness.buffer().area.height)
        .filter_map(|row| Some((gutter_line(harness, row)?, selected_text(harness, row))))
        .filter(|(_, sel)| !sel.is_empty())
        .collect()
}

/// Open 50 copies of `LINE`, wheel down so the view is well away from line
/// 1, and return the harness, fixture and the screen rows of the first and
/// last visible text rows plus the column where `LINE` starts.
fn scrolled_fixture() -> (
    EditorTestHarness,
    crate::common::fixtures::TestFixture,
    u16,
    u16,
    u16,
) {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text(&format!("{LINE}\n").repeat(50))
        .unwrap();
    harness.render().unwrap();
    for _ in 0..5 {
        harness.mouse_scroll_down(30, 8).unwrap();
    }
    harness.render().unwrap();

    let (text_col, first_row) = harness.find_text_on_screen(LINE).unwrap();
    let last_row = (first_row..harness.buffer().area.height)
        .take_while(|&row| gutter_line(&harness, row).is_some())
        .last()
        .unwrap();
    assert!(
        gutter_line(&harness, first_row).unwrap() > 5,
        "the wheel should have scrolled well away from line 1:\n{}",
        harness.screen_to_string()
    );
    (harness, fixture, first_row, last_row, text_col)
}

/// Press on the `a` of `row`'s `this is a text` and drag right along the
/// same row to past the end of the line, a frame between each motion.
/// Returns the line the row showed before the press.
fn drag_along_row(harness: &mut EditorTestHarness, row: u16, text_col: u16) -> u32 {
    let line = gutter_line(harness, row).unwrap();
    let a_col = text_col + 8;
    mouse(harness, MouseEventKind::Down(MouseButton::Left), a_col, row);
    for col in [a_col + 3, a_col + 5, a_col + 6, a_col + 10, a_col + 12] {
        mouse(harness, MouseEventKind::Drag(MouseButton::Left), col, row);
    }
    mouse(
        harness,
        MouseEventKind::Up(MouseButton::Left),
        a_col + 12,
        row,
    );
    line
}

#[test]
fn test_drag_along_top_row_selects_suffix_without_scrolling() {
    let (mut harness, _fixture, first_row, _, text_col) = scrolled_fixture();
    let before = harness.screen_to_string();

    let line = drag_along_row(&mut harness, first_row, text_col);

    assert_eq!(
        gutter_line(&harness, first_row),
        Some(line),
        "a drag inside the text area must not scroll the view.\nBefore:\n{before}\nAfter:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_rows(&harness),
        vec![(line, "a text".to_string())],
        "only the suffix of the dragged line is selected.\nAfter:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn test_drag_along_bottom_row_selects_suffix_without_scrolling() {
    let (mut harness, _fixture, first_row, last_row, text_col) = scrolled_fixture();
    let top = gutter_line(&harness, first_row);
    let before = harness.screen_to_string();

    let line = drag_along_row(&mut harness, last_row, text_col);

    assert_eq!(
        gutter_line(&harness, first_row),
        top,
        "a drag inside the text area must not scroll the view.\nBefore:\n{before}\nAfter:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_rows(&harness),
        vec![(line, "a text".to_string())],
        "only the suffix of the dragged line is selected.\nAfter:\n{}",
        harness.screen_to_string()
    );
}
