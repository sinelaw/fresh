//! The file explorer's window is the library's: its bar is a declared
//! `viewport`'s, so the bar can be *dragged*, and the ceiling the thumb
//! reaches is the one that puts the tree's last row on screen with the
//! root still pinned above it.
//!
//! Every assertion is on the rendered grid: which file names are on screen,
//! and which cells of the bar's lane carry the thumb's colour.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Color;
use std::fs;

/// The panel's last inner column — the bar's lane, found from the chrome.
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

/// The `file_NN.txt` names on the panel's rows, top to bottom.
fn files_on_screen(harness: &EditorTestHarness) -> Vec<String> {
    harness
        .screen_to_string()
        .lines()
        .filter(|l| l.starts_with('│'))
        .filter_map(|l| {
            l.split_whitespace()
                .find(|w| w.starts_with("file_") && w.ends_with(".txt"))
                .map(str::to_string)
        })
        .collect()
}

fn open_explorer_over(harness: &mut EditorTestHarness, files: usize) {
    let root = harness.project_dir().unwrap();
    for i in 0..files {
        fs::write(root.join(format!("file_{i:02}.txt")), "x").unwrap();
    }
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("file_00.txt"))
        .unwrap();
}

/// **Dragging the thumb scrolls the tree.** The hand-built bar was a column
/// of coloured cells the pointer fell through; the declared one is the
/// library's, and a press on its track followed by a drag drives the window
/// — reported to the model, which is where the offset lives, so the rows
/// under the bar are the model's rows for the offset the bar was dragged to.
#[test]
fn dragging_the_bar_scrolls_the_tree() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    open_explorer_over(&mut harness, 60);
    let column = bar_column(&harness);
    let thumb_fg = harness.editor().theme().scrollbar_thumb_fg;
    let before = files_on_screen(&harness);
    assert_eq!(before.first().map(String::as_str), Some("file_00.txt"));
    let thumb = rows_with_bg(&harness, column, thumb_fg);
    let top = *thumb.first().expect("a thumb");

    // Pick the thumb up and drag it a third of the way down the track.
    let track_end = *rows_with_bg(
        &harness,
        column,
        harness.editor().theme().scrollbar_track_fg,
    )
    .last()
    .expect("a track");
    let to = top + (track_end - top) / 3;
    harness.mouse_drag(column, top, column, to).unwrap();
    harness.render().unwrap();

    let after = files_on_screen(&harness);
    assert_ne!(
        after.first(),
        before.first(),
        "the rows moved with the thumb\n{}",
        harness.screen_to_string()
    );
    assert!(
        after.first().is_some_and(|f| f.as_str() > "file_00.txt"),
        "and moved *down*: {after:?}"
    );
    let thumb_after = rows_with_bg(&harness, column, thumb_fg);
    assert!(
        thumb_after.first().is_some_and(|y| *y > top),
        "the thumb is where it was dragged to: {thumb:?} -> {thumb_after:?}"
    );
    assert!(
        !harness.screen_to_string().contains("file_59.txt"),
        "a third of the way is not the end"
    );
}

/// **The end of the track is the end of the tree, root pinned.** Scrolled,
/// the project root stays pinned above the files, which costs the window a
/// row — so the last offset is one past `count - rows`, and a bar drawn
/// against the naive ceiling reached the end of its track while the last
/// file was still below the window. The thumb reaching the end and the last
/// file being on screen have to be the same moment.
#[test]
fn the_thumb_reaches_the_end_when_the_last_row_is_on_screen_under_the_pinned_root() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    open_explorer_over(&mut harness, 60);
    let column = bar_column(&harness);
    let thumb_fg = harness.editor().theme().scrollbar_thumb_fg;
    let track_fg = harness.editor().theme().scrollbar_track_fg;
    let body_row = harness
        .find_text_on_screen("file_00.txt")
        .expect("a tree row on screen")
        .1;

    // Notch by notch, until the thumb is flush with the end of the track.
    let mut notches = 0;
    loop {
        harness.mouse_scroll_down(column / 2, body_row).unwrap();
        harness.render().unwrap();
        notches += 1;
        assert!(notches < 200, "the thumb never reached the end");
        let thumb = rows_with_bg(&harness, column, thumb_fg);
        let track = rows_with_bg(&harness, column, track_fg);
        let flush = track.iter().all(|y| thumb.first().is_some_and(|t| y < t));
        if flush {
            break;
        }
        assert!(
            !harness.screen_to_string().contains("file_59.txt"),
            "the last file is on screen while the thumb is still short of \
             the end (after {notches} notches)\n{}",
            harness.screen_to_string()
        );
    }
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("file_59.txt"),
        "at the track's end the last file is on screen\n{screen}"
    );
    assert!(
        screen
            .lines()
            .any(|l| l.starts_with('│') && l.contains("project_root")),
        "and the root is still pinned above it\n{screen}"
    );
    // One more notch moves nothing: that was the ceiling.
    harness.mouse_scroll_down(column / 2, body_row).unwrap();
    harness.render().unwrap();
    assert_eq!(harness.screen_to_string(), screen);
}
