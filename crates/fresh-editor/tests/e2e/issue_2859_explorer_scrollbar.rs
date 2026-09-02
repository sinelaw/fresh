//! Issue #2859 — the file explorer draws a scrollbar when the tree overflows.
//!
//! The panel gave no sign that a tree was longer than its body: its right
//! edge was a plain border on every row, at any scroll offset. The editor's
//! own bar is a split-pane feature and the reusable widget was wired only
//! into the settings UI, so the explorer had no bar code at all.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Color;
use std::fs;

/// The panel's last inner column — the bar's lane.
///
/// Found from the chrome rather than from a constant: the top border ends at
/// the panel's right edge, and the column before it is the one a bar takes.
fn bar_column(harness: &EditorTestHarness) -> u16 {
    let area = harness.buffer().area;
    for y in 0..area.height {
        for x in 0..area.width {
            if harness.get_cell(x, y).as_deref() == Some("┐") {
                return x - 1;
            }
        }
    }
    panic!(
        "no explorer panel on screen\n{}",
        harness.screen_to_string()
    );
}

/// The rows of `column` painted in `want`.
fn rows_with_bg(harness: &EditorTestHarness, column: u16, want: Color) -> Vec<u16> {
    let area = harness.buffer().area;
    (0..area.height)
        .filter(|&y| {
            harness
                .get_cell_style(column, y)
                .is_some_and(|s| s.bg == Some(want))
        })
        .collect()
}

fn open_explorer(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();
}

#[test]
fn an_overflowing_tree_draws_a_scrollbar_that_follows_the_scroll() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    let root = harness.project_dir().unwrap();
    // Comfortably more entries than the panel's body rows.
    for i in 0..60 {
        fs::write(root.join(format!("file_{i:02}.txt")), "x").unwrap();
    }
    open_explorer(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("file_00.txt"))
        .unwrap();

    let column = bar_column(&harness);
    let thumb_fg = harness.editor().theme().scrollbar_thumb_fg;
    let track_fg = harness.editor().theme().scrollbar_track_fg;
    let thumb = rows_with_bg(&harness, column, thumb_fg);
    let track = rows_with_bg(&harness, column, track_fg);
    assert!(
        !thumb.is_empty(),
        "an overflowing tree must draw a thumb\n{}",
        harness.screen_to_string()
    );
    assert!(
        !track.is_empty(),
        "and the track it slides along\n{}",
        harness.screen_to_string()
    );
    let top_before = *thumb.first().unwrap();
    assert!(
        track.iter().all(|y| *y > *thumb.last().unwrap()),
        "unscrolled, the thumb is at the top of the track\nthumb {thumb:?} track {track:?}"
    );

    // Scroll to the end of the tree: the thumb follows.
    harness.editor_mut().focus_file_explorer();
    if let Some(view) = harness.editor_mut().file_explorer_mut() {
        view.select_last();
        view.update_scroll_for_selection();
    }
    harness.render().unwrap();

    let thumb_after = rows_with_bg(&harness, column, thumb_fg);
    let track_after = rows_with_bg(&harness, column, track_fg);
    let top_after = *thumb_after.first().unwrap();
    assert!(
        top_after > top_before,
        "the thumb must move down as the tree scrolls ({top_before} -> {top_after})\n{}",
        harness.screen_to_string()
    );
    assert!(
        track_after.iter().all(|y| *y < top_after),
        "fully scrolled, the thumb sits at the track's end\nthumb {thumb_after:?} track {track_after:?}"
    );
}

#[test]
fn a_tree_that_fits_draws_no_scrollbar() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    let root = harness.project_dir().unwrap();
    fs::write(root.join("only.txt"), "x").unwrap();
    open_explorer(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("only.txt"))
        .unwrap();

    let column = bar_column(&harness);
    let thumb_fg = harness.editor().theme().scrollbar_thumb_fg;
    let track_fg = harness.editor().theme().scrollbar_track_fg;
    assert!(
        rows_with_bg(&harness, column, thumb_fg).is_empty()
            && rows_with_bg(&harness, column, track_fg).is_empty(),
        "a tree that fits needs no bar\n{}",
        harness.screen_to_string()
    );
}
