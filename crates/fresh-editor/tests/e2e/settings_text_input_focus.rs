//! E2E tests for Settings UI text input focus/edit visuals.
//!
//! Reproduces two bugs seen when navigating to a text field (e.g. Terminal ->
//! Command) in the Settings UI:
//!
//! 1. The current item's label text uses `theme.selection_bg` as its
//!    foreground colour, which renders as dark-on-dark (unreadable) on the
//!    high-contrast theme (and is semantically wrong on any theme — a
//!    background colour has no reason to be used as a foreground).
//! 2. The text-input cursor (a reversed-video cell inside the brackets)
//!    is rendered as soon as the row is navigated-to, before the user
//!    has actually entered edit mode by pressing Enter. There is thus no
//!    visual indication that a second keypress is needed to start typing.
//!
//! The fix introduces an explicit `editing` flag on `TextInputState`:
//! * the cursor is only drawn when `editing == true`;
//! * in the selected-but-not-editing state the row's highlight background
//!   provides the "you are here" cue, while the label/brackets retain
//!   readable foreground colours.
//!
//! These tests verify both behaviours and would have caught the regression.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Modifier;

/// Navigate to Terminal -> Command by typing the explicit field path
/// into the settings search. Using the slash-prefixed path avoids the
/// fuzzy-matcher tie-breaks that show up when the query happens to be
/// a non-trivial substring of unrelated long descriptions.
fn open_terminal_command(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();

    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("/terminal/shell/command").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command");

    // Walk up then down looking for the focus indicator on the Command
    // label row (search may have dropped focus on a different item).
    for _ in 0..30 {
        if find_command_row(harness).is_some() {
            return;
        }
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    for _ in 0..30 {
        if find_command_row(harness).is_some() {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    panic!(
        "Could not focus the Terminal -> Command row. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Return the (row_index, line) where the focus indicator ">" sits on the
/// `Command` text-input label. Walks the row by character to accommodate
/// multibyte box-drawing characters.
fn find_command_row(harness: &EditorTestHarness) -> Option<(u16, String)> {
    let height = harness.buffer().area.height;
    for y in 0..height {
        let line = harness.get_row_text(y);
        let chars: Vec<char> = line.chars().collect();
        // Find "Command" followed (on same line) by '['.
        let needle: Vec<char> = "Command".chars().collect();
        let Some(cmd_col) = (0..chars.len().saturating_sub(needle.len() - 1))
            .find(|&i| chars[i..i + needle.len()] == needle[..])
        else {
            continue;
        };
        if !chars[cmd_col..].contains(&'[') {
            continue;
        }
        // Focus indicator ">" sits a few cells before the label.
        if let Some(arrow_col) = chars[..cmd_col].iter().rposition(|&c| c == '>') {
            if cmd_col - arrow_col <= 6 {
                return Some((y, line));
            }
        }
    }
    None
}

/// Locate the x-coordinate of the opening `[` of the text input on the
/// Command row. Walks the row character-by-character because the screen
/// contains multibyte box-drawing characters (`│` is 3 bytes / 1 column).
fn find_bracket_open(harness: &EditorTestHarness, row: u16) -> Option<u16> {
    let line = harness.get_row_text(row);
    let chars: Vec<char> = line.chars().collect();
    let needle = "Command";
    let needle_chars: Vec<char> = needle.chars().collect();
    // Find "Command" by character column.
    let mut label_col: Option<usize> = None;
    for start in 0..chars.len().saturating_sub(needle_chars.len() - 1) {
        if chars[start..start + needle_chars.len()] == needle_chars[..] {
            label_col = Some(start);
            break;
        }
    }
    let label_col = label_col?;
    let bracket_offset = chars[label_col..].iter().position(|&c| c == '[')?;
    Some((label_col + bracket_offset) as u16)
}

/// Return true if any cell in [x..x+width) on the given row has the
/// REVERSED modifier — which is how the text-input cursor is drawn.
fn has_cursor_cell(harness: &EditorTestHarness, x: u16, y: u16, width: u16) -> bool {
    for dx in 0..width {
        if let Some(style) = harness.get_cell_style(x + dx, y) {
            if style.add_modifier.contains(Modifier::REVERSED) {
                return true;
            }
        }
    }
    false
}

/// Bug: the cursor (REVERSED cell) is rendered as soon as the text input
/// is navigated to, before the user has pressed Enter to enter edit mode.
/// The cursor should only appear once the user has explicitly started
/// editing.
#[test]
fn test_command_cursor_hidden_until_editing() {
    let mut harness = EditorTestHarness::new(140, 40).unwrap();
    harness.render().unwrap();
    open_terminal_command(&mut harness);

    let (row, _line) = find_command_row(&harness).expect("Command row not focused");
    let bracket_x = find_bracket_open(&harness, row).expect("Expected '[' on Command row");

    // When the row is merely selected (not in edit mode) we should not
    // render a cursor inside the brackets.
    assert!(
        !has_cursor_cell(&harness, bracket_x, row, 30),
        "BUG: Settings UI renders a REVERSED cursor cell inside the Command \
         input even before the user has pressed Enter to start editing. \
         This makes it impossible to tell whether the field is highlighted \
         for navigation or actively being edited.\nRow: {}\nScreen:\n{}",
        harness.get_row_text(row),
        harness.screen_to_string()
    );

    // Enter edit mode — now the cursor SHOULD be visible.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let (row, _line) = find_command_row(&harness).expect("Command row still focused");
    let bracket_x = find_bracket_open(&harness, row).expect("Expected '[' on Command row");

    assert!(
        has_cursor_cell(&harness, bracket_x, row, 30),
        "After pressing Enter the Command text input should show a cursor \
         inside the brackets.\nRow: {}\nScreen:\n{}",
        harness.get_row_text(row),
        harness.screen_to_string()
    );
}

/// Bug: the Command label foreground uses `theme.selection_bg`, which is a
/// dark colour (RGB 50,60,90 in high-contrast). On the dark row-highlight
/// background it renders as unreadable dark-on-dark.
///
/// The fix must use a readable colour (e.g. `settings_selected_fg`) for the
/// focused label. This test guards against the specific regression by
/// asserting the foreground colour is not `theme.selection_bg`.
#[test]
fn test_command_label_not_selection_bg_color() {
    use ratatui::style::Color;

    let mut harness = EditorTestHarness::new(140, 40).unwrap();
    harness.render().unwrap();
    open_terminal_command(&mut harness);

    let (row, _line) = find_command_row(&harness).expect("Command row not focused");
    // The row text may contain multibyte box-drawing chars; locate the
    // "Command" column via char iteration.
    let row_text = harness.get_row_text(row);
    let chars: Vec<char> = row_text.chars().collect();
    let needle: Vec<char> = "Command".chars().collect();
    let label_col = (0..chars.len().saturating_sub(needle.len() - 1))
        .find(|&i| chars[i..i + needle.len()] == needle[..])
        .expect("Command label should be present") as u16;

    // Grab the foreground colour of the first cell of the label.
    let style = harness
        .get_cell_style(label_col, row)
        .expect("label cell should have a style");
    let fg = style.fg.expect("label should have an fg colour");

    // High-contrast theme's selection_bg is dark blue.
    let forbidden = Color::Rgb(50, 60, 90);
    assert_ne!(
        fg,
        forbidden,
        "BUG: focused Settings text-input label is rendered with \
         theme.selection_bg ({:?}) as its foreground colour, which is dark-on-dark \
         and unreadable in the high-contrast theme. It must use a readable \
         foreground (e.g. settings_selected_fg / editor_fg).\nRow: {}\nScreen:\n{}",
        forbidden,
        harness.get_row_text(row),
        harness.screen_to_string()
    );
}
