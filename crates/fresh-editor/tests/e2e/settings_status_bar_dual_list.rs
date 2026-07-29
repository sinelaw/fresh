//! E2E coverage for the status-bar element picker in Settings
//! (Editor › Status Bar › Left / Right) — the two-column
//! Available/Included `DualList`.
//!
//! Before the fix the control rendered no cursor at all: every row in
//! both columns painted identically, nothing marked which column had
//! the keyboard, and nothing said how to move an item. Arrow keys
//! walked an invisible selection and Enter moved whichever entry it
//! happened to be sitting on. These tests drive keyboard and mouse
//! events and assert only on rendered output (per CONTRIBUTING.md,
//! "E2E Tests Observe, Not Inspect").

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Cursor glyph on the column the keyboard is driving.
const ACTIVE: char = '▸';
/// Cursor glyph parked in the other column.
const IDLE: char = '▹';
/// Marker under the active column's header.
const COLUMN: char = '▾';

/// Open Settings, walk the sidebar to Editor › Status Bar, then Tab
/// into the content pane so the `Left` picker is the selected control.
fn focus_status_bar_left(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();
    // Expand the Editor category, then step down to its Status Bar
    // subsection. Stop as soon as the picker's columns are on screen.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    for _ in 0..40 {
        let screen = harness.screen_to_string();
        if screen.contains("Available") && screen.contains("Included") {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Available") && screen.contains("Included"),
        "should reach the Status Bar element picker; screen:\n{screen}"
    );
    // Move focus from the category sidebar into the content pane.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// The label carried by the row holding `marker`, with the marker and
/// surrounding padding stripped — e.g. `▸ Filename` → `Filename`.
/// Multiple cells can sit on one screen row (the two columns), so the
/// text is cut at the run of spaces that separates them.
fn cell_at_marker(harness: &EditorTestHarness, marker: char) -> Option<String> {
    let screen = harness.screen_to_string();
    let (pos, line) = screen.lines().find_map(|line| {
        line.char_indices()
            .find(|&(_, c)| c == marker)
            .map(|(i, _)| (i, line))
    })?;
    let after = &line[pos + marker.len_utf8()..];
    let label = after.trim_start().split("  ").next()?.trim();
    (!label.is_empty()).then(|| label.to_string())
}

/// Screen column (not byte offset) where `needle` starts on a rendered
/// row. Rows are full of box-drawing glyphs, so the two differ.
fn char_index_of(line: &[char], needle: &str) -> Option<usize> {
    let needle: Vec<char> = needle.chars().collect();
    line.windows(needle.len())
        .position(|w| w == needle.as_slice())
}

/// Enter starts editing the picker: the cursor appears in the Available
/// column, the other column's parked cursor is marked distinctly, and
/// the header points at the column that has the keyboard.
///
/// Without the fix the control painted no markers in any state — the
/// spec it projected carried neither focus nor cursor — so every one of
/// these assertions fails.
#[test]
fn dual_list_shows_cursor_and_active_column_once_editing() {
    let mut harness = EditorTestHarness::new(140, 44).unwrap();
    harness.render().unwrap();
    focus_status_bar_left(&mut harness);

    // Selected but not editing: arrows still walk the settings list, so
    // no in-column cursor is drawn — just the invitation to edit.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(ACTIVE) && !screen.contains(COLUMN),
        "no cursor should be painted before editing; screen:\n{screen}"
    );
    assert!(
        screen.contains("Enter to edit"),
        "selected picker should say how to start; screen:\n{screen}"
    );

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(&format!("{COLUMN} Available")),
        "the active column's header should be marked; screen:\n{screen}"
    );
    assert!(
        !screen.contains(&format!("{COLUMN} Included")),
        "only one column may read as active; screen:\n{screen}"
    );
    assert!(
        cell_at_marker(&harness, ACTIVE).is_some(),
        "the active column should show a cursor; screen:\n{screen}"
    );
    assert!(
        cell_at_marker(&harness, IDLE).is_some(),
        "the idle column's parked cursor should be shown too; screen:\n{screen}"
    );
    // The keys that move items are not guessable from the control's
    // shape, so they are spelled out while editing.
    assert!(
        screen.contains("Shift+←→") && screen.contains("Shift+↑↓"),
        "editing should list the move/reorder keys; screen:\n{screen}"
    );
}

/// Down moves the cursor to a visibly different entry, and Right hands
/// the keyboard to the Included column — both observable on screen.
///
/// Without the fix these keys mutated an invisible cursor: the rendered
/// output was byte-identical before and after.
#[test]
fn dual_list_cursor_moves_and_switches_columns_visibly() {
    let mut harness = EditorTestHarness::new(140, 44).unwrap();
    harness.render().unwrap();
    focus_status_bar_left(&mut harness);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let first = cell_at_marker(&harness, ACTIVE).expect("cursor on entry");

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let second = cell_at_marker(&harness, ACTIVE).expect("cursor after Down");
    assert_ne!(
        first,
        second,
        "Down should visibly move the cursor; screen:\n{}",
        harness.screen_to_string()
    );

    // Right hands the keyboard to the Included column: its header takes
    // the marker and the filled cursor crosses over.
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(&format!("{COLUMN} Included")),
        "Right should mark the Included column active; screen:\n{screen}"
    );
    // The Available cursor stays put, now drawn as the parked one.
    assert_eq!(
        cell_at_marker(&harness, IDLE).as_deref(),
        Some(second.as_str()),
        "the Available cursor should be parked where it was; screen:\n{screen}"
    );
}

/// Shift+Right moves the cursor's entry into the Included column, and
/// the cursor follows it — the whole point of the control, and until
/// now impossible to aim because the cursor was invisible.
#[test]
fn dual_list_shift_right_moves_the_entry_under_the_cursor() {
    let mut harness = EditorTestHarness::new(140, 44).unwrap();
    harness.render().unwrap();
    focus_status_bar_left(&mut harness);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let picked = cell_at_marker(&harness, ACTIVE).expect("cursor on entry");

    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(&format!("{COLUMN} Included")),
        "focus should follow the moved entry into Included; screen:\n{screen}"
    );
    assert_eq!(
        cell_at_marker(&harness, ACTIVE).as_deref(),
        Some(picked.as_str()),
        "the cursor should land on the entry it moved; screen:\n{screen}"
    );
}

/// Clicking a cell selects that row. The picker previously published no
/// click geometry at all, so clicks inside the columns did nothing.
#[test]
fn dual_list_click_selects_the_clicked_entry() {
    let mut harness = EditorTestHarness::new(140, 44).unwrap();
    harness.render().unwrap();
    focus_status_bar_left(&mut harness);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let before = cell_at_marker(&harness, ACTIVE).expect("cursor on entry");

    // Aim at the Available column by its header's screen column, so the
    // click can't land in the category sidebar that shares these rows.
    // Screen rows carry box-drawing glyphs, so index by chars, not bytes.
    let screen = harness.screen_to_string();
    let header_row = screen
        .lines()
        .position(|l| l.contains("Available") && l.contains("Included"))
        .expect("column header on screen");
    let header: Vec<char> = screen.lines().nth(header_row).unwrap().chars().collect();
    let avail_col = char_index_of(&header, "Available").expect("Available header");
    // Three rows below the header, well clear of the entry the cursor
    // entered on.
    let target_row = header_row + 3;
    let line: Vec<char> = screen
        .lines()
        .nth(target_row)
        .expect("target row on screen")
        .chars()
        .collect();
    let target: String = line[avail_col..]
        .iter()
        .collect::<String>()
        .split("  ")
        .next()
        .unwrap()
        .trim()
        .to_string();
    assert!(
        !target.is_empty(),
        "precondition: an entry sits at the click target; screen:\n{screen}"
    );
    assert_ne!(target, before, "precondition: clicking a different entry");

    harness
        .mouse_click(avail_col as u16, target_row as u16)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        cell_at_marker(&harness, ACTIVE).as_deref(),
        Some(target.as_str()),
        "clicking a cell should move the cursor to it; screen:\n{}",
        harness.screen_to_string()
    );
}
