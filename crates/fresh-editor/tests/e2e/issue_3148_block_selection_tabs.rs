//! Issue #3148 — a block selection's rectangle is painted where the cursor is.
//!
//! The rectangle's columns are byte offsets within a line, but the painter
//! tested them against each cell's index into the *view* row, which counts a
//! tab as its whole expansion. With one leading tab and `tab_size: 4` the
//! highlight stopped `tab_size - 1` cells to the left of the cursor — and the
//! cursor is the one telling the truth about what a block copy takes.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Drive the reported gesture: Alt+Shift+Down ×2, Alt+Shift+Right ×3.
fn block_select(harness: &mut EditorTestHarness) {
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
    }
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();
}

/// The screen columns of `row` carrying the selection background.
fn selected_columns(harness: &EditorTestHarness, row: u16) -> Vec<u16> {
    let selection_bg = harness.editor().theme().selection_bg;
    let width = harness.buffer().area.width;
    (0..width)
        .filter(|&x| {
            harness
                .get_cell_style(x, row)
                .is_some_and(|s| s.bg == Some(selection_bg))
        })
        .collect()
}

fn harness_for(contents: &str, name: &str) -> (EditorTestHarness, TempDir) {
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join(name);
    std::fs::write(&file, contents).unwrap();
    let mut config = Config::default();
    config.editor.tab_size = 4;
    let mut harness =
        EditorTestHarness::create(100, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();
    (harness, temp_dir)
}

#[test]
fn block_selection_ends_at_the_cursor_with_a_leading_tab() {
    let (mut harness, _dir) = harness_for("\tabcdefgh\n".repeat(3).as_str(), "blk.txt");
    block_select(&mut harness);

    let (cursor_x, cursor_y) = harness.screen_cursor_position();
    let selected = selected_columns(&harness, cursor_y);

    assert!(
        !selected.is_empty(),
        "the block should paint something on the cursor's row\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected.last().copied(),
        Some(cursor_x),
        "the rectangle's right edge is the cursor's cell, not {} cells left of it\n{}",
        cursor_x.saturating_sub(selected.last().copied().unwrap_or(0)),
        harness.screen_to_string()
    );
    // Tab expansion (4 cells) + `abc`: the tab is wholly in or wholly out,
    // which is what a block copy of column range 0..=3 takes.
    assert_eq!(
        selected.len(),
        7,
        "expected the tab's four cells plus `abc`\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        harness.get_cell(cursor_x, cursor_y).as_deref(),
        Some("c"),
        "the cursor sits on the last character the block covers\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn block_selection_without_a_tab_is_unchanged() {
    let (mut harness, _dir) = harness_for("Zabcdefgh\n".repeat(3).as_str(), "blk2.txt");
    block_select(&mut harness);

    let (cursor_x, cursor_y) = harness.screen_cursor_position();
    let selected = selected_columns(&harness, cursor_y);

    assert_eq!(selected.last().copied(), Some(cursor_x));
    assert_eq!(
        selected.len(),
        4,
        "`Zabc` — one cell per character\n{}",
        harness.screen_to_string()
    );
}
