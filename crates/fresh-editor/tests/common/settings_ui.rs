//! Driving the Settings dialog from a test.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Move the Settings category selection onto `name`.
///
/// The categories are a list and the arrow keys are the only way along it, so
/// the keys pressed are the same however this is done. What is not the same is
/// reading the screen back and redrawing it after every one of them: the list
/// is already on screen, so the distance can be read off it once and walked in
/// a single burst, with the result checked at the end. Stepping and re-scanning
/// a row at a time measured at 587ms of a 1.23s test.
///
/// The one-at-a-time walk stays as the fallback, for when the target is not on
/// screen (the pane scrolls) or the burst does not land where the arithmetic
/// said it would. It re-reads the distance each step and moves either way, so
/// an overshoot corrects itself instead of walking to the end of the list.
pub fn focus_category(h: &mut EditorTestHarness, name: &str) {
    if let Some(distance) = rows_to_category(h, name) {
        let key = if distance < 0 {
            KeyCode::Up
        } else {
            KeyCode::Down
        };
        for _ in 0..distance.unsigned_abs() {
            h.send_key(key, KeyModifiers::NONE).unwrap();
        }
        h.render().unwrap();
        if category_is_selected(h, name) {
            return;
        }
    }

    for _ in 0..40 {
        if category_is_selected(h, name) {
            return;
        }
        // Off screen, or the burst landed wrong: step towards it, either way.
        let key = match rows_to_category(h, name) {
            Some(d) if d < 0 => KeyCode::Up,
            _ => KeyCode::Down,
        };
        h.send_key(key, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    panic!(
        "category {:?} never became selected. Screen:\n{}",
        name,
        h.screen_to_string()
    );
}

/// Whether `name`'s row in the category list is the selected one.
pub fn category_is_selected(h: &EditorTestHarness, name: &str) -> bool {
    let screen = h.screen_to_string();
    let rows: Vec<&str> = screen.lines().collect();
    let Some(pane) = CategoryPane::find(&rows) else {
        return false;
    };
    pane.row_of(&rows, name) == Some(pane.selected_row)
}

/// The category list, located on a rendered Settings dialog.
///
/// Two things on this screen make the obvious reading wrong, and both have to
/// be ruled out before a row index means anything:
///
/// - `>` is drawn in two places. The category cursor sits at the start of a
///   category row (`view/shell/settings.rs`, `cat_row`); the selected *card*
///   has a `>` in its gutter in the right-hand pane (`gutter`), and that one
///   is not gated on focus. Taking the first row containing a `>` finds
///   whichever is higher up the screen.
/// - The selected category's name is *also* the right-hand pane's title, so
///   it appears twice, on different rows. Taking the first row containing the
///   name finds the title, which is never the selected row.
///
/// And the dialog does not own the screen: the editor behind it is still
/// drawn, so a `>` may appear left of the dialog entirely (the File Explorer
/// marks an expanded directory with one).
///
/// All three are settled by column: the list is bounded on the left by the
/// cursor column -- itself inside the dialog's own left border -- and on the
/// right by the divider between the panes. Only what lies between them is the
/// category list.
struct CategoryPane {
    selected_row: usize,
    cursor_col: usize,
    divider_col: usize,
}

impl CategoryPane {
    fn find(rows: &[&str]) -> Option<Self> {
        // Everything is relative to the dialog, because the rest of the
        // editor is still on screen behind it -- the File Explorer draws its
        // own `>` on an expanded directory, further left than anything in the
        // dialog, and taking the leftmost `>` on the *screen* finds that.
        let dialog_left = rows
            .iter()
            .find(|line| line.contains("Settings ["))
            .and_then(|line| col_of(line, "\u{256d}"))?;
        // Within the dialog, the category cursor is the leftmost `>`: the card
        // gutter's is in the right-hand pane, so it is always further right.
        let (selected_row, cursor_col) = rows
            .iter()
            .enumerate()
            .filter_map(|(row, line)| col_of(line, ">").map(|col| (row, col)))
            .filter(|(_, col)| *col > dialog_left)
            .min_by_key(|(_, col)| *col)?;
        // The pane divider is the first box-drawing rule right of the cursor.
        let divider_col = col_of_after(rows[selected_row], "\u{2502}", cursor_col)?;
        Some(Self {
            selected_row,
            cursor_col,
            divider_col,
        })
    }

    /// The row `name` is drawn on *in the category list*, ignoring the copy in
    /// the right-hand pane's title.
    fn row_of(&self, rows: &[&str], name: &str) -> Option<usize> {
        rows.iter().enumerate().find_map(|(row, line)| {
            col_of(line, name)
                .filter(|col| *col > self.cursor_col && *col < self.divider_col)
                .map(|_| row)
        })
    }
}

/// The column `needle` starts at, counted in cells rather than bytes -- the
/// dialog is full of multi-byte box-drawing and icon characters, and a byte
/// offset would not be comparable between one row and the next.
fn col_of(line: &str, needle: &str) -> Option<usize> {
    line.find(needle).map(|byte| line[..byte].chars().count())
}

/// As [`col_of`], but for the first occurrence after `col`.
fn col_of_after(line: &str, needle: &str, col: usize) -> Option<usize> {
    let from: usize = line
        .char_indices()
        .nth(col)
        .map(|(byte, _)| byte)
        .unwrap_or(line.len());
    line[from..]
        .find(needle)
        .map(|byte| line[..from + byte].chars().count())
}

/// How many rows separate the selected category from `name` -- negative when
/// `name` is above it. `None` when either is not on screen, which is the
/// caller's cue to walk rather than guess.
fn rows_to_category(h: &EditorTestHarness, name: &str) -> Option<isize> {
    let screen = h.screen_to_string();
    let rows: Vec<&str> = screen.lines().collect();
    let pane = CategoryPane::find(&rows)?;
    let target = pane.row_of(&rows, name)?;
    Some(target as isize - pane.selected_row as isize)
}
