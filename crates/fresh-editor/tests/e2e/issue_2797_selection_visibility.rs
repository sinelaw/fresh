//! Issue #2797: a selection that covers empty lines (or line breaks in
//! general) left no trace on screen — an all-empty-line selection looked
//! exactly like no selection at all.
//!
//! Two rendered signals cover it, both observed here only through the
//! rendered cells:
//!
//! * every selected line break paints one highlighted column at the
//!   position the break occupies (column 0 on an empty line), and
//! * whitespace inside a selection draws its `·` / `→` indicators even
//!   when the whitespace indicators are otherwise off
//!   (`editor.whitespace_in_selection`, on by default).

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Screen row of buffer line `line_idx` (0-based) and the first content
/// column, for a harness whose viewport is at the top of the buffer.
fn content_origin(harness: &EditorTestHarness) -> (u16, u16) {
    let gutter_width = harness.editor().active_state().margins.left_total_width() as u16;
    let (content_first_row, _) = harness.content_area_rows();
    (gutter_width, content_first_row as u16)
}

#[test]
fn selected_empty_lines_show_a_highlighted_column() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // Lines (0-based): 0 "aaa", 1 "bbb", 2 "", 3 "", 4 "", 5 "ccc"
    harness
        .load_buffer_from_text("aaa\nbbb\n\n\n\nccc\n")
        .unwrap();

    // Cursor to the start of line 2, then select lines 2 and 3 (both empty).
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..2 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    let selection_bg = Some(harness.editor().theme().selection_bg);
    let (col0, first_row) = content_origin(&harness);

    for line_idx in [2u16, 3] {
        let style = harness
            .get_cell_style(col0, first_row + line_idx)
            .expect("cell inside the viewport");
        assert_eq!(
            style.bg,
            selection_bg,
            "empty line {line_idx} is inside the selection, so its line break \
             should paint a highlighted column at the start of the line\n{}",
            harness.screen_to_string()
        );
    }

    // Line 4 is empty too but outside the selection — it must stay clean.
    let style = harness
        .get_cell_style(col0, first_row + 4)
        .expect("cell inside the viewport");
    assert_ne!(
        style.bg,
        selection_bg,
        "empty line 4 is outside the selection and must not be highlighted\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn selected_line_break_shows_a_highlighted_column_past_line_end() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.load_buffer_from_text("aaa\nbbb\n").unwrap();

    // Select line 0 including its line break.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let selection_bg = Some(harness.editor().theme().selection_bg);
    let (col0, first_row) = content_origin(&harness);

    let style = harness
        .get_cell_style(col0 + 3, first_row)
        .expect("cell inside the viewport");
    assert_eq!(
        style.bg,
        selection_bg,
        "the selected line break after \"aaa\" should paint one highlighted column\n{}",
        harness.screen_to_string()
    );

    // One column further is past the line break — not selected.
    let style = harness
        .get_cell_style(col0 + 4, first_row)
        .expect("cell inside the viewport");
    assert_ne!(
        style.bg, selection_bg,
        "only the line break itself is highlighted, not the rest of the row"
    );
}

#[test]
fn whitespace_inside_selection_shows_indicators_by_default() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.load_buffer_from_text("ab  cd  ef\n").unwrap();
    assert!(
        !harness.config().editor.whitespace_spaces_inner,
        "the space indicators are off by default; this test is about the \
         in-selection override"
    );

    // Select "ab  cd" — the first pair of spaces is inside the selection,
    // the second pair is not.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    let (col0, row) = content_origin(&harness);
    let screen = harness.screen_to_string();
    for offset in [2u16, 3] {
        assert_eq!(
            harness.get_cell(col0 + offset, row).as_deref(),
            Some("·"),
            "space at column {offset} is inside the selection, so it should \
             render a whitespace indicator\n{screen}"
        );
    }
    for offset in [6u16, 7] {
        assert_eq!(
            harness.get_cell(col0 + offset, row).as_deref(),
            Some(" "),
            "space at column {offset} is outside the selection and the \
             indicators are off, so it should stay blank\n{screen}"
        );
    }
}

#[test]
fn whitespace_in_selection_can_be_turned_off() {
    let mut config = Config::default();
    config.editor.whitespace_in_selection = false;
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.load_buffer_from_text("ab  cd  ef\n").unwrap();

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    let (col0, row) = content_origin(&harness);
    for offset in [2u16, 3] {
        assert_eq!(
            harness.get_cell(col0 + offset, row).as_deref(),
            Some(" "),
            "with editor.whitespace_in_selection off, selected spaces keep \
             rendering blank\n{}",
            harness.screen_to_string()
        );
    }
}
