//! Issue #3150 — the block-selection highlight and the block copy disagreed
//! by one column.
//!
//! The rectangle's column span is half-open, `min_col..max_col`: that is what
//! `copy_block_selection_text` takes from each line and what
//! `convert_block_selection_to_cursors` gives each line as a selection. The
//! painter tested `col <= end_col`, so it lit one column more than either of
//! them touched — the user saw `abcd` highlighted and pasted `abc`.
//!
//! Two consequences are pinned here besides the widths matching:
//!
//! * Extending the block rightward leaves the cursor just past the
//!   rectangle's right edge, so a one-column block reads as one column.
//! * A zero-width block (Alt+Shift+Down with no horizontal movement) is a
//!   column of cursors, not a selection: it paints nothing and copies
//!   nothing. It used to paint one column — indistinguishable from a
//!   one-column block — and copy a bare newline per line, which a later
//!   paste inserted as blank lines.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

const LINES: usize = 3;

/// `abcdef` on three lines, cursor parked at 1:1.
fn harness() -> EditorTestHarness {
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    harness
        .load_buffer_from_text(&"abcdef\n".repeat(LINES))
        .unwrap();
    harness.editor_mut().set_clipboard_for_test(String::new());
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
}

fn press(harness: &mut EditorTestHarness, code: KeyCode, times: usize) {
    for _ in 0..times {
        harness
            .send_key(code, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();
}

/// The characters on `row` whose cell carries the selection background.
fn highlighted(harness: &EditorTestHarness, row: u16) -> String {
    let selection_bg = harness.editor().theme().selection_bg;
    let width = harness.buffer().area.width;
    (0..width)
        .filter(|&x| {
            harness
                .get_cell_style(x, row)
                .is_some_and(|s| s.bg == Some(selection_bg))
        })
        .filter_map(|x| harness.get_cell(x, row))
        .collect()
}

/// The highlighted text of each of the buffer's `LINES` rows.
fn highlighted_rows(harness: &EditorTestHarness) -> Vec<String> {
    let (first, _) = harness.content_area_rows();
    (0..LINES)
        .map(|i| highlighted(harness, first as u16 + i as u16))
        .collect()
}

fn copy(harness: &mut EditorTestHarness) -> String {
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.editor_mut().clipboard_content_for_test()
}

/// Case 1 from the report: Alt+Shift+Right ×3 from 1:1 highlighted `abcd`
/// but copied `abc`.
#[test]
fn highlight_width_matches_copy_width() {
    let mut harness = harness();
    press(&mut harness, KeyCode::Down, 2);
    press(&mut harness, KeyCode::Right, 3);

    let rows = highlighted_rows(&harness);
    assert_eq!(
        rows,
        vec!["abc".to_string(); LINES],
        "three Alt+Shift+Rights from column 0 cover columns 0..3\n{}",
        harness.screen_to_string()
    );

    let clipboard = copy(&mut harness);
    assert_eq!(clipboard, "abc\nabc\nabc");
    for (row, copied) in rows.iter().zip(clipboard.lines()) {
        assert_eq!(row, copied, "the highlight is what the copy takes");
    }
}

/// A single-line block behaves the same — this is the exact gesture in the
/// report (no vertical movement at all).
#[test]
fn single_line_highlight_matches_copy() {
    let mut harness = harness();
    press(&mut harness, KeyCode::Right, 3);

    let (first, _) = harness.content_area_rows();
    assert_eq!(highlighted(&harness, first as u16), "abc");
    assert_eq!(copy(&mut harness), "abc");
}

/// The reporter's suggestion: once the block is at least one column wide the
/// cursor sits outside the rectangle, so one column of highlight means one
/// column of text.
#[test]
fn cursor_sits_just_past_the_rectangle() {
    let mut harness = harness();
    press(&mut harness, KeyCode::Right, 1);

    let (cursor_x, cursor_y) = harness.screen_cursor_position();
    assert_eq!(
        highlighted(&harness, cursor_y),
        "a",
        "one Alt+Shift+Right covers exactly column 0\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        harness.get_cell(cursor_x, cursor_y).as_deref(),
        Some("b"),
        "the cursor is outside the rectangle, on the first cell past it\n{}",
        harness.screen_to_string()
    );
    assert_eq!(copy(&mut harness), "a");
}

/// Extending leftward puts the cursor on the rectangle's left edge, which is
/// inside it — the span is `cursor_col..anchor_col` there.
#[test]
fn extending_left_keeps_the_widths_matching() {
    let mut harness = harness();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    press(&mut harness, KeyCode::Left, 3);

    let (cursor_x, cursor_y) = harness.screen_cursor_position();
    assert_eq!(highlighted(&harness, cursor_y), "abc");
    assert_eq!(
        harness.get_cell(cursor_x, cursor_y).as_deref(),
        Some("a"),
        "extending left, the cursor is the rectangle's left edge\n{}",
        harness.screen_to_string()
    );
    assert_eq!(copy(&mut harness), "abc");
}

/// Case 2 from the report: a zero-width block painted column 0 on every line,
/// which looked exactly like a one-column-wide block.
#[test]
fn zero_width_block_paints_nothing() {
    let mut harness = harness();
    press(&mut harness, KeyCode::Down, 2);

    assert_eq!(
        highlighted_rows(&harness),
        vec![String::new(); LINES],
        "a zero-width block is a column of cursors, not a selection\n{}",
        harness.screen_to_string()
    );
}

/// …and copying it took a bare newline per line, which pasting turned into
/// blank lines. It must take nothing and leave the clipboard alone.
#[test]
fn zero_width_block_copies_nothing() {
    let mut harness = harness();
    harness
        .editor_mut()
        .set_clipboard_for_test("untouched".to_string());
    press(&mut harness, KeyCode::Down, 2);

    assert_eq!(
        copy(&mut harness),
        "untouched",
        "a zero-width block selects no text, so Ctrl+C is a no-op"
    );
}

/// A zero-width block reached by going right and back again is still
/// zero-width: nothing painted, nothing copied.
#[test]
fn block_collapsed_back_to_zero_width_paints_nothing() {
    let mut harness = harness();
    press(&mut harness, KeyCode::Down, 1);
    press(&mut harness, KeyCode::Right, 2);
    assert_eq!(highlighted_rows(&harness)[0], "ab");

    press(&mut harness, KeyCode::Left, 2);
    assert_eq!(
        highlighted_rows(&harness),
        vec![String::new(); LINES],
        "back to zero width\n{}",
        harness.screen_to_string()
    );
    assert_eq!(copy(&mut harness), "");
}
