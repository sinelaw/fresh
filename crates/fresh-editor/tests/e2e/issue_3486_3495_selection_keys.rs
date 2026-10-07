//! Issue #3486: a vertical move with nowhere to go did nothing at all — Up on
//! a selection whose start was on the first line left both the cursor and the
//! selection exactly where they were, however often it was pressed, and Down
//! did the same on a selection reaching the last line. The collapse rode on the
//! same `MoveCursor` event as the line step, so when there was no line to step
//! to, neither happened.
//!
//! Issue #3495: `Ctrl+L` re-anchored to the cursor's line on every press, so
//! repeating it selected exactly one line and marched it down the file instead
//! of taking in another line each time.
//!
//! These pin the boundary semantics the way #1566 and #3006 pinned theirs, and
//! they assert only on what ends up on screen: the selection background of
//! individual content cells and the `Ln n, Col m` readout in the status bar.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

const LINE1: &str = "first line of the file";
const LINE2: &str = "second line here";
const LINE3: &str = "third line here";
const LINE4: &str = "last line of the file";

/// No trailing newline, so line 4 really is the last line of the buffer.
fn open_fixture(harness: &mut EditorTestHarness, temp_dir: &TempDir) {
    let file_path = temp_dir.path().join("sel.txt");
    std::fs::write(&file_path, format!("{LINE1}\n{LINE2}\n{LINE3}\n{LINE4}")).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
}

/// Locate `line` on screen, folding the in-selection whitespace indicators
/// (`editor.whitespace_in_selection` draws `·` over a selected space) back to
/// plain spaces first — a partly selected line renders a mix of the two. Each
/// indicator occupies one cell, so the column arithmetic is unaffected.
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

/// Column offsets of `line` that are rendered with the selection background.
fn selected_offsets(harness: &EditorTestHarness, line: &str) -> Vec<u16> {
    let selection_bg = harness.editor().theme().selection_bg;
    let (col, row) = find_line_on_screen(harness, line).unwrap_or_else(|| {
        panic!(
            "line {line:?} not on screen:\n{}",
            harness.screen_to_string()
        )
    });
    (0..line.chars().count() as u16)
        .filter(|&offset| {
            harness
                .get_cell_style(col + offset, row)
                .map(|style| style.bg == Some(selection_bg))
                .unwrap_or(false)
        })
        .collect()
}

fn whole(line: &str) -> Vec<u16> {
    (0..line.chars().count() as u16).collect()
}

fn nothing() -> Vec<u16> {
    Vec::new()
}

/// Up cancels a selection that begins on the first line, and lands on its top
/// edge, instead of leaving the key dead.
#[test]
fn up_cancels_a_selection_anchored_on_the_first_line() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    open_fixture(&mut harness, &temp_dir);

    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        selected_offsets(&harness, LINE1),
        whole(LINE1),
        "precondition: Shift+Down selects the whole first line"
    );

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert_eq!(
        selected_offsets(&harness, LINE1),
        nothing(),
        "Up should cancel the selection:\n{}",
        harness.screen_to_string()
    );
    assert!(
        harness.get_status_bar().contains("Ln 1, Col 1"),
        "Up should land on the selection's top edge, status bar says: {}",
        harness.get_status_bar()
    );
}

/// The mirror: Down cancels a selection that reaches the last line.
#[test]
fn down_cancels_a_selection_reaching_the_last_line() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    open_fixture(&mut harness, &temp_dir);

    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        selected_offsets(&harness, LINE4),
        whole(LINE4),
        "precondition: Shift+Down on the last line selects to the buffer end"
    );

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert_eq!(
        selected_offsets(&harness, LINE4),
        nothing(),
        "Down should cancel the selection:\n{}",
        harness.screen_to_string()
    );
}

/// Every cursor's selection goes, not just the ones that had somewhere to move.
///
/// The cached-layout mover runs before the byte-based one and used to skip a
/// cursor it could not move. With a second cursor that *could* move, its events
/// came back non-empty, nothing fell through, and the cursor at the buffer's
/// edge kept its selection — #3486 surviving under multi-cursor.
#[test]
fn up_cancels_every_cursors_selection_not_only_the_ones_that_can_move() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    open_fixture(&mut harness, &temp_dir);

    harness
        .send_key(KeyCode::Down, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        selected_offsets(&harness, LINE1),
        whole(LINE1),
        "precondition: the first cursor selected line 1"
    );
    // All but the first cell: the first cursor's head rests on line 2's first
    // byte, and a cell under a caret renders as the caret rather than as
    // selection background.
    assert_eq!(
        selected_offsets(&harness, LINE2),
        (1..LINE2.chars().count() as u16).collect::<Vec<u16>>(),
        "precondition: the second cursor selected line 2:\n{}",
        harness.screen_to_string()
    );

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert_eq!(
        selected_offsets(&harness, LINE1),
        nothing(),
        "one Up should cancel the boundary cursor's selection too:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_offsets(&harness, LINE2),
        nothing(),
        "and the selection of the cursor that could move:\n{}",
        harness.screen_to_string()
    );
}

/// Ctrl+L takes in another line on every press instead of moving a one-line
/// selection down the file.
#[test]
fn repeated_ctrl_l_takes_in_another_line_each_press() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    open_fixture(&mut harness, &temp_dir);

    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        selected_offsets(&harness, LINE1),
        whole(LINE1),
        "the first press selects the current line"
    );
    assert_eq!(
        selected_offsets(&harness, LINE2),
        nothing(),
        "and only that line"
    );

    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        selected_offsets(&harness, LINE1),
        whole(LINE1),
        "the second press keeps what was already selected:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_offsets(&harness, LINE2),
        whole(LINE2),
        "and adds the line below:\n{}",
        harness.screen_to_string()
    );

    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        selected_offsets(&harness, LINE3),
        whole(LINE3),
        "and so does the third:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_offsets(&harness, LINE1),
        whole(LINE1),
        "without dropping the first line:\n{}",
        harness.screen_to_string()
    );
}

/// An upward selection squares off over the lines it already covers rather than
/// shrinking to the head's line.
#[test]
fn ctrl_l_on_an_upward_selection_squares_it_off_without_shrinking() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    open_fixture(&mut harness, &temp_dir);

    for _ in 0..2 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();
    assert_eq!(
        selected_offsets(&harness, LINE2),
        whole(LINE2),
        "precondition: line 2 selected upwards, head above the anchor"
    );

    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        selected_offsets(&harness, LINE2),
        whole(LINE2),
        "the lines already covered stay covered:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_offsets(&harness, LINE1),
        nothing(),
        "and the selection does not grow upwards:\n{}",
        harness.screen_to_string()
    );
}
