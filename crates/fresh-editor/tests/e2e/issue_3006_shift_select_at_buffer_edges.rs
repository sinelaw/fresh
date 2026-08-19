//! Issue #3006: Shift+Up on the first line and Shift+Down on the last line did
//! nothing at all — no cursor movement and no selection — instead of extending
//! the selection to the start/end of the buffer like VSCode and Sublime do.
//!
//! These tests drive real key events and assert only on what ends up on screen:
//! the selection background of individual content cells and the `Ln n, Col m`
//! readout in the status bar.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

const LINE1: &str = "first line of the file";
const LINE4: &str = "last line of the file";

/// The fixture deliberately has *no* trailing newline, so line 4 really is the
/// last line of the buffer.
fn open_fixture(harness: &mut EditorTestHarness, temp_dir: &TempDir) {
    let file_path = temp_dir.path().join("sel.txt");
    std::fs::write(
        &file_path,
        format!("{LINE1}\nsecond line here\nthird line here\n{LINE4}"),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
}

/// Locate `line` on screen, folding the in-selection whitespace indicators
/// (`editor.whitespace_in_selection` draws `·` over a selected space) back to
/// plain spaces first — a partly selected line renders a mix of the two, so a
/// literal match would miss it. Each indicator occupies exactly one cell, so
/// the column arithmetic is unaffected.
fn find_line_on_screen(harness: &EditorTestHarness, line: &str) -> Option<(u16, u16)> {
    harness
        .screen_to_string()
        .lines()
        .enumerate()
        .find_map(|(row, text)| {
            let plain = text.replace('·', " ");
            plain
                .find(line)
                .map(|byte_idx| (plain[..byte_idx].chars().count() as u16, row as u16))
        })
}

/// Is the cell `offset` columns into the on-screen rendering of `line` painted
/// with the theme's selection background?
fn cell_is_selected(harness: &EditorTestHarness, line: &str, offset: u16) -> bool {
    let selection_bg = harness.editor().theme().selection_bg;
    let (col, row) = find_line_on_screen(harness, line).unwrap_or_else(|| {
        panic!(
            "line {line:?} not on screen:\n{}",
            harness.screen_to_string()
        )
    });
    harness
        .get_cell_style(col + offset, row)
        .map(|style| style.bg == Some(selection_bg))
        .unwrap_or(false)
}

/// Column offsets of `line` that are rendered with the selection background.
fn selected_offsets(harness: &EditorTestHarness, line: &str) -> Vec<u16> {
    (0..line.chars().count() as u16)
        .filter(|&offset| cell_is_selected(harness, line, offset))
        .collect()
}

/// Shift+Up with the cursor on the very first line must extend the selection
/// from the cursor back to the start of the buffer.
#[test]
fn shift_up_on_first_line_selects_to_buffer_start() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    open_fixture(&mut harness, &temp_dir);

    // Put the cursor on line 1, column 11 (just after "first line").
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    assert!(
        harness.get_status_bar().contains("Ln 1, Col 11"),
        "precondition: cursor on line 1 column 11, status bar says: {}",
        harness.get_status_bar()
    );
    assert_eq!(
        selected_offsets(&harness, LINE1),
        Vec::<u16>::new(),
        "precondition: nothing selected yet"
    );

    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    assert_eq!(
        selected_offsets(&harness, LINE1),
        (0..10).collect::<Vec<u16>>(),
        "Shift+Up on the first line should highlight everything from the buffer \
         start up to the cursor:\n{}",
        harness.screen_to_string()
    );
    assert!(
        harness.get_status_bar().contains("Ln 1, Col 1"),
        "Shift+Up on the first line should move the cursor to column 1, status bar says: {}",
        harness.get_status_bar()
    );
}

/// Shift+Down with the cursor on the very last line must extend the selection
/// from the cursor to the end of the buffer.
#[test]
fn shift_down_on_last_line_selects_to_buffer_end() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    open_fixture(&mut harness, &temp_dir);

    // Put the cursor on line 4 (the last line), column 11.
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    assert!(
        harness.get_status_bar().contains("Ln 4, Col 11"),
        "precondition: cursor on line 4 column 11, status bar says: {}",
        harness.get_status_bar()
    );
    assert_eq!(
        selected_offsets(&harness, LINE4),
        Vec::<u16>::new(),
        "precondition: nothing selected yet"
    );

    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        selected_offsets(&harness, LINE4),
        (10..LINE4.len() as u16).collect::<Vec<u16>>(),
        "Shift+Down on the last line should highlight everything from the cursor \
         to the end of the buffer:\n{}",
        harness.screen_to_string()
    );
    assert!(
        harness
            .get_status_bar()
            .contains(&format!("Ln 4, Col {}", LINE4.len() + 1)),
        "Shift+Down on the last line should move the cursor past the last character, \
         status bar says: {}",
        harness.get_status_bar()
    );
}

/// An already-active selection whose head has arrived on the first line must
/// keep growing to the buffer start, and pressing Shift+Up once more when the
/// head is already there must change nothing on screen.
#[test]
fn shift_up_extends_existing_selection_to_buffer_start_then_stops() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    open_fixture(&mut harness, &temp_dir);

    // Cursor on line 2, column 11, then select up onto line 1.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        selected_offsets(&harness, LINE1),
        (10..LINE1.len() as u16).collect::<Vec<u16>>(),
        "precondition: selection reaches from line 1 column 11 down into line 2:\n{}",
        harness.screen_to_string()
    );

    // Second Shift+Up: the head is on line 1, so it must run to the buffer start.
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        selected_offsets(&harness, LINE1),
        (0..LINE1.len() as u16).collect::<Vec<u16>>(),
        "Shift+Up on the first line should extend the existing selection to the \
         buffer start:\n{}",
        harness.screen_to_string()
    );
    assert!(
        harness.get_status_bar().contains("Ln 1, Col 1"),
        "status bar says: {}",
        harness.get_status_bar()
    );

    // Third Shift+Up: the head is already at the buffer start, nothing may change.
    let before = harness.screen_to_string();
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.screen_to_string(),
        before,
        "Shift+Up with the head already at the buffer start should be a no-op"
    );
}
