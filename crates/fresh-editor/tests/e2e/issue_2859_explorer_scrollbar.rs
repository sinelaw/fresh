//! Regression tests for issue #2859: a scrollable File Explorer drew no
//! scrollbar, even though the tree itself could be scrolled.
//!
//! Everything here is read back from rendered output (CONTRIBUTING Testing 2):
//! the bar shows up as themed cell backgrounds in its reserved lane, the tree
//! as screen text. No model, view, or context accessors — the same rule
//! `issue_2119_wheel_scroll.rs` documents, whose token helpers are reused here.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, ExplorerWidth};
use ratatui::style::Color;
use std::fs;

/// A fixed-width explorer on the left spans columns `0..EXPLORER_COLS`. Its
/// resize border is the last column, so the scrollbar's lane is the one before
/// it — the column the row renderer keeps free.
const EXPLORER_COLS: u16 = 30;
const SCROLLBAR_COL: u16 = EXPLORER_COLS - 2;

fn explorer_config() -> Config {
    let mut config = Config {
        theme: "high-contrast".into(),
        ..Default::default()
    };
    config.file_explorer.width = ExplorerWidth::Columns(EXPLORER_COLS);
    config
}

/// Open the explorer on a temp project holding `files` numbered entries and
/// wait until the tree has landed.
fn harness_with_files(files: usize) -> EditorTestHarness {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, explorer_config()).unwrap();
    let project_root = harness.project_dir().unwrap();
    for i in 0..files {
        fs::write(project_root.join(format!("file_{i:02}.txt")), "x").unwrap();
    }

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("file_00.txt").unwrap();
    harness.render().unwrap();
    harness
}

/// The first non-whitespace token at/after `prefix` on the line, e.g. for
/// prefix "file_" on "│▌   file_11.txt  ●" returns "file_11.txt".
fn token_after(line: &str, prefix: &str) -> Option<String> {
    let idx = line.find(prefix)?;
    let tok: String = line[idx..]
        .chars()
        .take_while(|c| !c.is_whitespace())
        .collect();
    Some(tok)
}

/// The `prefix`-token on the first screen line that also contains `marker`.
fn token_on_line_with(screen: &str, marker: &str, prefix: &str) -> Option<String> {
    screen
        .lines()
        .find(|l| l.contains(marker))
        .and_then(|l| token_after(l, prefix))
}

/// The `prefix`-token on the first *file-explorer body* row (those start with
/// the explorer's left border `│`), so we read the top tree entry rather than
/// a tab title or status-bar mention of the same name.
fn first_explorer_token(screen: &str, prefix: &str) -> Option<String> {
    screen
        .lines()
        .filter(|l| l.starts_with('│'))
        .find_map(|l| token_after(l, prefix))
}

/// Screen rows of the explorer body: the ones between its top and bottom
/// border, found by the corner glyphs the panel draws in column 0. This is
/// exactly the span the scrollbar's track occupies.
fn explorer_body_rows(harness: &EditorTestHarness) -> Vec<u16> {
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let top = lines
        .iter()
        .position(|l| l.starts_with('┌'))
        .unwrap_or_else(|| panic!("the explorer's top border should be drawn.\nScreen:\n{screen}"));
    let bottom = lines
        .iter()
        .rposition(|l| l.starts_with('└'))
        .unwrap_or_else(|| {
            panic!("the explorer's bottom border should be drawn.\nScreen:\n{screen}")
        });
    ((top as u16 + 1)..bottom as u16).collect()
}

fn cell_bg(harness: &EditorTestHarness, col: u16, row: u16) -> Option<Color> {
    harness.get_cell_style(col, row).and_then(|style| style.bg)
}

/// Body rows whose scrollbar cell carries the themed thumb colour.
fn thumb_rows(harness: &EditorTestHarness) -> Vec<u16> {
    let thumb = harness.editor().theme().scrollbar_thumb_fg;
    explorer_body_rows(harness)
        .into_iter()
        .filter(|&row| cell_bg(harness, SCROLLBAR_COL, row) == Some(thumb))
        .collect()
}

/// An overflowing tree paints a themed bar down its reserved lane, and the
/// thumb reports where the viewport is: pinned to the top of the track while
/// the tree is at its start, to the bottom once it is scrolled to its end.
#[test]
fn scrollable_file_explorer_draws_a_thumb_that_tracks_the_viewport() {
    let mut harness = harness_with_files(80);

    let body = explorer_body_rows(&harness);
    assert!(
        body.len() > 4,
        "the explorer should have a body to scroll. Screen:\n{}",
        harness.screen_to_string()
    );
    let track_top = body[0];
    let track_bottom = *body.last().unwrap();

    let track_color = harness.editor().theme().scrollbar_track_fg;
    let thumb_color = harness.editor().theme().scrollbar_thumb_fg;

    // Every row of the lane belongs to the bar — thumb or track, nothing else.
    for &row in &body {
        let bg = cell_bg(&harness, SCROLLBAR_COL, row);
        assert!(
            bg == Some(track_color) || bg == Some(thumb_color),
            "column {SCROLLBAR_COL}, row {row} should carry the Explorer scrollbar, \
             saw {bg:?}.\nScreen:\n{}",
            harness.screen_to_string()
        );
    }

    let at_top = thumb_rows(&harness);
    assert!(
        !at_top.is_empty(),
        "the scrollbar should paint a thumb.\nScreen:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        at_top[0],
        track_top,
        "at the start of the tree the thumb belongs at the top of the track.\nScreen:\n{}",
        harness.screen_to_string()
    );
    assert!(
        *at_top.last().unwrap() < track_bottom,
        "an overflowing tree's thumb must be shorter than its track.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Wheel to the end of the tree; the thumb has to follow.
    for _ in 0..60 {
        harness.mouse_scroll_down(3, body[1]).unwrap();
    }
    harness.render().unwrap();

    let at_bottom = thumb_rows(&harness);
    assert!(
        !at_bottom.is_empty(),
        "the thumb should still be painted after scrolling.\nScreen:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        *at_bottom.last().unwrap(),
        track_bottom,
        "at the end of the tree the thumb belongs at the bottom of the track.\nScreen:\n{}",
        harness.screen_to_string()
    );
    assert!(
        at_bottom[0] > track_top,
        "the thumb should have travelled away from the top of the track.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// The bar is draggable, and dragging it only moves the viewport: the entry
/// under the pointer is not selected, and the round trip back to the top of
/// the track restores exactly the view (and selection) we started from.
#[test]
fn file_explorer_scrollbar_thumb_drag_scrolls_without_moving_the_selection() {
    let mut harness = harness_with_files(80);

    // Put the selection well inside the first screenful so it is visible both
    // before the drag and after the round trip.
    for _ in 0..12 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let before = harness.screen_to_string();
    let selected = token_on_line_with(&before, "▌", "file_")
        .unwrap_or_else(|| panic!("a file should be selected (▌ marker).\nScreen:\n{before}"));
    let top_before = first_explorer_token(&before, "file_")
        .unwrap_or_else(|| panic!("the tree should show entries.\nScreen:\n{before}"));
    assert!(
        !before.contains("file_79.txt"),
        "precondition: the end of the tree is off-screen.\nScreen:\n{before}"
    );

    let body = explorer_body_rows(&harness);
    let track_top = body[0];
    let track_bottom = *body.last().unwrap();

    harness
        .mouse_drag(SCROLLBAR_COL, track_top, SCROLLBAR_COL, track_bottom)
        .unwrap();

    let dragged = harness.screen_to_string();
    assert!(
        dragged.contains("file_79.txt"),
        "dragging the thumb to the bottom of the track should reveal the end \
         of the tree.\nScreen:\n{dragged}"
    );

    harness
        .mouse_drag(SCROLLBAR_COL, track_bottom, SCROLLBAR_COL, track_top)
        .unwrap();

    let back = harness.screen_to_string();
    assert_eq!(
        first_explorer_token(&back, "file_").as_deref(),
        Some(top_before.as_str()),
        "dragging back to the top of the track should restore the original \
         view.\nScreen:\n{back}"
    );
    assert_eq!(
        token_on_line_with(&back, "▌", "file_").as_deref(),
        Some(selected.as_str()),
        "a scrollbar drag must not select the tree row under the pointer.\nScreen:\n{back}"
    );
}

/// The bar is a scroll affordance, not decoration: a tree that fits leaves its
/// lane blank.
#[test]
fn file_explorer_that_fits_paints_no_scrollbar() {
    let harness = harness_with_files(3);

    let track_color = harness.editor().theme().scrollbar_track_fg;
    let thumb_color = harness.editor().theme().scrollbar_thumb_fg;

    let body = explorer_body_rows(&harness);
    assert!(
        !body.is_empty(),
        "the explorer should be on screen.\nScreen:\n{}",
        harness.screen_to_string()
    );
    for row in body {
        let bg = cell_bg(&harness, SCROLLBAR_COL, row);
        assert!(
            bg != Some(track_color) && bg != Some(thumb_color),
            "a tree that fits must not paint a scrollbar, but column \
             {SCROLLBAR_COL}, row {row} carries {bg:?}.\nScreen:\n{}",
            harness.screen_to_string()
        );
    }
}

/// The trailing status glyph is hit-tested where it is painted. The scrollbar
/// lane is not part of a row's content, so hovering the bar reveals nothing
/// about the file behind it.
#[test]
fn status_indicator_hover_follows_the_glyph_not_the_scrollbar() {
    let mut harness = harness_with_files(80);

    // An unsaved buffer decorates its explorer row with "●" and a tooltip.
    let project_root = harness.project_dir().unwrap();
    harness
        .open_file(&project_root.join("file_00.txt"))
        .unwrap();
    harness.editor_mut().active_window_mut().focus_editor();
    harness.type_text("x").unwrap();
    harness.editor_mut().focus_file_explorer();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .lines()
                .any(|l| l.starts_with('│') && l.contains("file_00.txt") && l.contains('●'))
        })
        .unwrap();
    // Refresh the layout cache (explorer rect, trailing-slot bounds) the
    // hit-test reads.
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let (row, line) = screen
        .lines()
        .enumerate()
        .find(|(_, l)| l.starts_with('│') && l.contains("file_00.txt") && l.contains('●'))
        .unwrap_or_else(|| panic!("the unsaved file should carry ● .\nScreen:\n{screen}"));
    let row = row as u16;
    // Count cells, not bytes, so the multi-byte border/marker glyphs don't
    // skew the column.
    let glyph_col = line[..line.find('●').unwrap()].chars().count() as u16;

    assert!(
        glyph_col < SCROLLBAR_COL,
        "the status glyph belongs in the row's content lane, left of the \
         scrollbar at column {SCROLLBAR_COL}, but is painted at {glyph_col}.\nScreen:\n{screen}"
    );

    harness.mouse_move(glyph_col, row).unwrap();
    assert!(
        harness
            .screen_to_string()
            .contains("Unsaved changes in editor"),
        "hovering the status glyph should show its tooltip.\nScreen:\n{}",
        harness.screen_to_string()
    );

    harness.mouse_move(SCROLLBAR_COL, row).unwrap();
    assert!(
        !harness
            .screen_to_string()
            .contains("Unsaved changes in editor"),
        "hovering the scrollbar must not show the status tooltip of the file \
         behind it.\nScreen:\n{}",
        harness.screen_to_string()
    );
}
