//! Regression tests for issue #2859: a scrollable File Explorer drew no
//! scrollbar, even though the tree itself could be scrolled.
//!
//! Everything here is read back from rendered output (CONTRIBUTING Testing 2):
//! the bar shows up as themed cell backgrounds in its reserved lane, the tree
//! as screen text. No model, view, or context accessors — the same rule
//! `issue_2119_wheel_scroll.rs` documents, whose token readers these tests
//! share from `common::explorer`.

use crate::common::explorer::{first_explorer_token, token_on_line_with};
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
/// A column well inside the editor split to the explorer's right.
const EDITOR_COL: u16 = EXPLORER_COLS + 20;

fn explorer_config() -> Config {
    let mut config = Config {
        theme: "high-contrast".into(),
        ..Default::default()
    };
    config.file_explorer.width = ExplorerWidth::Columns(EXPLORER_COLS);
    config
}

/// Open the explorer on a temp project holding `files` numbered entries plus
/// any `extra` names, and wait until the tree has landed.
fn harness_with_files(files: usize, extra: &[&str]) -> EditorTestHarness {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, explorer_config()).unwrap();
    let project_root = harness.project_dir().unwrap();
    for i in 0..files {
        fs::write(project_root.join(format!("file_{i:02}.txt")), "x").unwrap();
    }
    for name in extra {
        fs::write(project_root.join(name), "x").unwrap();
    }

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("file_00.txt").unwrap();
    harness.render().unwrap();
    harness
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

/// Body rows whose scrollbar cell carries a themed bar colour, track or thumb.
fn scrollbar_rows(harness: &EditorTestHarness) -> Vec<u16> {
    let track = harness.editor().theme().scrollbar_track_fg;
    let thumb = harness.editor().theme().scrollbar_thumb_fg;
    explorer_body_rows(harness)
        .into_iter()
        .filter(|&row| {
            cell_bg(harness, SCROLLBAR_COL, row).is_some_and(|bg| bg == track || bg == thumb)
        })
        .collect()
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
    let mut harness = harness_with_files(80, &[]);

    let body = explorer_body_rows(&harness);
    assert!(
        body.len() > 4,
        "the explorer should have a body to scroll. Screen:\n{}",
        harness.screen_to_string()
    );
    let track_top = body[0];
    let track_bottom = *body.last().unwrap();

    // Every row of the lane belongs to the bar — thumb or track, nothing else.
    assert_eq!(
        scrollbar_rows(&harness),
        body,
        "the whole of column {SCROLLBAR_COL} beside an overflowing tree should \
         carry the Explorer scrollbar.\nScreen:\n{}",
        harness.screen_to_string()
    );

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
    let mut harness = harness_with_files(80, &[]);

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

/// The bar is a scroll affordance, not decoration: it appears while the tree
/// overflows and goes away when it stops. Both halves are asserted against the
/// same panel in the same run, so the "no bar" half cannot pass by pointing at
/// a column that was never the bar's.
#[test]
fn file_explorer_scrollbar_appears_only_while_the_tree_overflows() {
    let mut harness = harness_with_files(80, &[]);

    assert!(
        !scrollbar_rows(&harness).is_empty(),
        "an overflowing tree should paint the scrollbar — without this half the \
         negative one below would pass against any column at all.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Enter on the selected root collapses it, leaving a one-row tree.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let collapsed = harness.screen_to_string();
    assert!(
        !collapsed.contains("file_00.txt"),
        "precondition: collapsing the root should hide its children, leaving a \
         tree that fits.\nScreen:\n{collapsed}"
    );

    assert_eq!(
        scrollbar_rows(&harness),
        Vec::<u16>::new(),
        "a tree that fits must leave the scrollbar's column blank.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// The trailing status glyph is hit-tested where it is painted. The scrollbar
/// lane is not part of a row's content, so hovering the bar reveals nothing
/// about the file behind it.
#[test]
fn status_indicator_hover_follows_the_glyph_not_the_scrollbar() {
    let mut harness = harness_with_files(80, &[]);

    // An unsaved buffer decorates its explorer row with "●" and a tooltip.
    // Canonicalize: on platforms where the temp dir is reached through a
    // symlink, an uncanonicalized path keys the buffer differently from the
    // explorer's node and the marker never appears.
    let project_root = harness.project_dir().unwrap().canonicalize().unwrap();
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
    let glyph_col = line.chars().position(|c| c == '●').unwrap() as u16;

    // Pin the column, not just "left of the bar": the marker is right-aligned
    // into the last column of the row's content, which is the one immediately
    // before the scrollbar. Anything else means the bar has pushed the row's
    // layout around, which is what it must not do.
    assert_eq!(
        glyph_col,
        SCROLLBAR_COL - 1,
        "the status glyph belongs in the last content column, immediately \
         left of the scrollbar at column {SCROLLBAR_COL}.\nScreen:\n{screen}"
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

/// The other end of the same rule: a row too wide for its content lane has no
/// room left to right-align its status marker, so the marker is laid out in
/// the scrollbar's column and painted straight over. An invisible marker must
/// not answer the pointer either.
#[test]
fn overflowing_row_offers_no_status_tooltip_under_the_scrollbar() {
    // Size the name so the slot lands exactly on the bar - the sharp case.
    // Once a row overflows, its padding collapses to a single space, so the
    // slot sits at `content_start_x (1) + row prefix + name + gap (1)`.
    const ROW_PREFIX: usize = 4; // one indent level (2) + tree indicator (2)
    let name_len = SCROLLBAR_COL as usize - 1 - ROW_PREFIX - 1;
    // Sorts ahead of the `file_NN` entries, so the row is on screen unscrolled.
    let long_name = format!("a{}.txt", "b".repeat(name_len - 5));
    assert_eq!(long_name.chars().count(), name_len);
    assert!(
        ROW_PREFIX + name_len + 2 >= EXPLORER_COLS as usize - 3,
        "the fixture name has to overflow the row's content lane, or this test \
         proves nothing about the overflow path"
    );

    let mut harness = harness_with_files(80, &[long_name.as_str()]);

    // Canonicalize for the same reason the hover test does: a symlinked temp
    // dir would key the buffer differently from the explorer's node.
    let project_root = harness.project_dir().unwrap().canonicalize().unwrap();
    harness.open_file(&project_root.join(&long_name)).unwrap();
    harness.type_text("x").unwrap();
    harness.editor_mut().focus_file_explorer();
    // Wait on one fact only - the row being on screen - and assert the rest,
    // so a wrong expectation fails with a screen dump instead of hanging.
    harness
        .wait_until(|h| h.screen_to_string().contains(long_name.as_str()))
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let (row, line) = screen
        .lines()
        .enumerate()
        .find(|(_, l)| l.starts_with('│') && l.contains(long_name.as_str()))
        .unwrap_or_else(|| {
            panic!("the overflowing file should have an explorer row.\nScreen:\n{screen}")
        });
    let row = row as u16;

    // Count cells, not bytes: the row carries multi-byte border and tree
    // glyphs before the panel's right edge.
    let explorer_row: String = line.chars().take(EXPLORER_COLS as usize).collect();
    assert!(
        !explorer_row.contains('●'),
        "the marker on an overflowing row is painted over by the scrollbar, so \
         it should not be on screen at all; row reads {explorer_row:?}.\nScreen:\n{screen}"
    );

    // Nothing anywhere on that row - the scrollbar's column above all - may
    // offer the tooltip of a marker nobody can see.
    for col in 0..EXPLORER_COLS {
        harness.mouse_move(col, row).unwrap();
        assert!(
            !harness
                .screen_to_string()
                .contains("Unsaved changes in editor"),
            "column {col} of the overflowing row claimed a status marker that is \
             painted under the scrollbar.\nScreen:\n{}",
            harness.screen_to_string()
        );
    }
}

/// Letting go of the button ends the scrollbar grab. The Explorer's grab is
/// consulted before the terminal and text-selection ones, so a grab that
/// outlived its button would capture every later drag anywhere on screen and
/// steer the tree from it.
#[test]
fn file_explorer_scrollbar_grab_ends_with_the_button() {
    let mut harness = harness_with_files(80, &[]);

    let body = explorer_body_rows(&harness);
    let track_top = body[0];
    let track_bottom = *body.last().unwrap();
    // Leave the thumb in the upper part of the track, so a grab that survived
    // would have somewhere to drag the tree to.
    let part_way = track_top + (track_bottom - track_top) / 3;

    let top_before =
        first_explorer_token(&harness.screen_to_string(), "file_").unwrap_or_else(|| {
            panic!(
                "the tree should show entries.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });

    harness
        .mouse_drag(SCROLLBAR_COL, track_top, SCROLLBAR_COL, part_way)
        .unwrap();
    let scrolled = harness.screen_to_string();
    let top_scrolled = first_explorer_token(&scrolled, "file_")
        .unwrap_or_else(|| panic!("the tree should still show entries.\nScreen:\n{scrolled}"));
    assert_ne!(
        top_scrolled, top_before,
        "precondition: the thumb drag should have scrolled the tree.\nScreen:\n{scrolled}"
    );

    // Now drag in the editor pane, ending far below where the thumb was left.
    // A leaked grab reads only the row, so this would drag the tree to its end.
    harness
        .mouse_drag(EDITOR_COL, track_top, EDITOR_COL, track_bottom)
        .unwrap();

    let after = harness.screen_to_string();
    assert_eq!(
        first_explorer_token(&after, "file_").as_deref(),
        Some(top_scrolled.as_str()),
        "a drag in the editor must not scroll the File Explorer — the scrollbar \
         grab should have ended with the button that started it.\nScreen:\n{after}"
    );
}
