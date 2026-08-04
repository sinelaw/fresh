//! The gutter bar for unsaved changes (`diff_since_saved`), asserted on
//! rendered cells in the indicator column.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Color;

/// Cornflower blue: the unsaved-change decoration on both the gutter and the
/// scrollbar.
const UNSAVED_BLUE: Color = Color::Rgb(100, 149, 237);

fn content(lines: usize) -> String {
    (0..lines)
        .map(|i| format!("line {i:04}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Screen rows whose indicator column carries the unsaved-change bar.
fn barred_rows(harness: &EditorTestHarness) -> Vec<usize> {
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .filter(|row| {
            harness.get_cell(0, *row as u16).as_deref() == Some("│")
                && harness
                    .get_cell_style(0, *row as u16)
                    .and_then(|s| s.fg)
                    .is_some_and(|fg| fg == UNSAVED_BLUE)
        })
        .collect()
}

/// Inserting whole lines marks exactly the inserted lines.
///
/// The diff's byte ranges are half-open, and inserting whole lines produces a
/// range that ends exactly on a line boundary. Counting that boundary as a
/// marked line put a bar on the first *untouched* line after the insertion —
/// visible as a bar that disappeared on save, once git reported the real line
/// set for the same edit.
#[test]
fn inserting_lines_marks_only_the_inserted_lines() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.load_buffer_from_text(&content(400)).unwrap();
    harness.render().unwrap();
    assert!(
        barred_rows(&harness).is_empty(),
        "an unmodified buffer has no unsaved-change bars"
    );

    // Three whole new lines at the start of line 5 (row 5 of the viewport).
    for _ in 0..4 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.type_text("NEW A\nNEW B\nNEW C\n").unwrap();
    harness.render().unwrap();

    let rows = barred_rows(&harness);
    let screen = harness.screen_to_string();
    assert_eq!(
        rows.len(),
        3,
        "three lines were inserted, so three lines are changed; \
         got {} bars\n{screen}",
        rows.len()
    );

    // And they are the three rows showing the new text.
    let text_rows: Vec<usize> = rows
        .iter()
        .map(|r| harness.get_screen_row(*r))
        .filter(|line| line.contains("NEW "))
        .enumerate()
        .map(|(i, _)| i)
        .collect();
    assert_eq!(
        text_rows.len(),
        3,
        "each bar should sit on one of the inserted lines\n{screen}"
    );
}

/// Editing within a single line marks that line only.
#[test]
fn editing_one_line_marks_one_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.load_buffer_from_text(&content(400)).unwrap();
    harness.render().unwrap();

    for _ in 0..9 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.type_text("EDIT").unwrap();
    harness.render().unwrap();

    let rows = barred_rows(&harness);
    assert_eq!(
        rows.len(),
        1,
        "one line was edited, so one bar is expected; got {rows:?}\n{}",
        harness.screen_to_string()
    );
    assert!(
        harness.get_screen_row(rows[0]).contains("EDIT"),
        "the bar should sit on the edited line"
    );
}
