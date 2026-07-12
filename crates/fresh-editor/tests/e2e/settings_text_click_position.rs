//! E2E: clicking inside a Settings text field moves the caret to the
//! clicked position (#2573), like any GUI text input — instead of leaving
//! it at the end (or wiping the value via the "replace on first keystroke"
//! affordance that arms when a field enters edit mode).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Navigate to Terminal -> Command (a single-line text field) and leave it
/// focused. Mirrors the helper in `settings_text_input_focus`.
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
        "Could not focus the Terminal -> Command row.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Row index of the focused `Command` text-input label (walks by char to
/// step over multibyte box-drawing glyphs).
fn find_command_row(harness: &EditorTestHarness) -> Option<u16> {
    let height = harness.buffer().area.height;
    for y in 0..height {
        let line = harness.get_row_text(y);
        let chars: Vec<char> = line.chars().collect();
        let needle: Vec<char> = "Command".chars().collect();
        let Some(cmd_col) = (0..chars.len().saturating_sub(needle.len().saturating_sub(1)))
            .find(|&i| chars[i..i + needle.len()] == needle[..])
        else {
            continue;
        };
        if !chars[cmd_col..].contains(&'[') {
            continue;
        }
        if let Some(arrow_col) = chars[..cmd_col].iter().rposition(|&c| c == '>') {
            if cmd_col - arrow_col <= 6 {
                return Some(y);
            }
        }
    }
    None
}

#[test]
fn test_settings_text_field_click_positions_cursor() {
    let mut harness = EditorTestHarness::new(140, 40).unwrap();
    harness.render().unwrap();
    open_terminal_command(&mut harness);

    // Enter edit mode, type a value with all-distinct characters, and
    // commit it (Enter) so the field holds a saved value — the way a real
    // setting would when you later click into it.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("abcXYZ").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("[abcXYZ");

    // Click on the "X" cell (value index 3): the caret should land before
    // the X. Clicking also re-enters edit mode and re-arms replace-on-type;
    // positioning the caret from the click must cancel that arm.
    let (vcol, vrow) = harness
        .find_text_on_screen("abcXYZ")
        .expect("the committed value should be visible in the field");
    harness.mouse_click(vcol + 3, vrow).unwrap();
    harness.render().unwrap();

    // Type a sentinel. Fixed: caret at the click, arm cancelled → "abc!XYZ".
    // Regressions this guards:
    //   * caret left at end           → "abcXYZ!"
    //   * replace-on-type still armed  → "!"
    harness.type_text("!").unwrap();
    harness.render().unwrap();

    let s = harness.screen_to_string();
    assert!(
        s.contains("[abc!XYZ") && !s.contains("[abcXYZ!"),
        "expected the caret to split the value at the click (abc!XYZ).\nScreen:\n{}",
        s
    );
}

/// Screen (col, row) of the first `[+] Add new` affordance, scanning by
/// character to step over multibyte box-drawing glyphs.
fn find_add_new(harness: &EditorTestHarness) -> Option<(u16, u16)> {
    let height = harness.buffer().area.height;
    for y in 0..height {
        let chars: Vec<char> = harness.get_row_text(y).chars().collect();
        let needle: Vec<char> = "[+] Add new".chars().collect();
        if let Some(c) = (0..chars.len().saturating_sub(needle.len().saturating_sub(1)))
            .find(|&i| chars[i..i + needle.len()] == needle[..])
        {
            return Some((c as u16 + 1, y)); // +1 → on the '+' cell
        }
    }
    None
}

/// The same fix on the entry (add/edit) sub-dialog: clicking a text field
/// inside the "Add Value" dialog positions the caret at the click.
#[test]
fn test_settings_entry_dialog_text_click_positions_cursor() {
    let mut harness = EditorTestHarness::new(150, 45).unwrap();
    harness.render().unwrap();
    harness.open_settings().unwrap();

    // Jump to the Keybindings settings, whose "Keybinding Maps" list has an
    // "[+] Add new" that opens an "Add Value" dialog with an editable "Key"
    // text field.
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("keybinding maps").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let (ac, ar) = find_add_new(&harness).unwrap_or_else(|| {
        panic!(
            "no '[+] Add new' on the Keybindings page.\nScreen:\n{}",
            harness.screen_to_string()
        )
    });
    harness.mouse_click(ac, ar).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Key");

    // The dialog's Key field is focused; type a distinct value into it.
    harness.type_text("abcXYZ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("[abcXYZ");

    // Click on the "X" cell (value index 3) and type a sentinel.
    let (vcol, vrow) = harness
        .find_text_on_screen("abcXYZ")
        .expect("the typed value should be visible in the dialog's Key field");
    harness.mouse_click(vcol + 3, vrow).unwrap();
    harness.render().unwrap();
    harness.type_text("!").unwrap();
    harness.render().unwrap();

    let s = harness.screen_to_string();
    assert!(
        s.contains("[abc!XYZ") && !s.contains("[abcXYZ!"),
        "expected the caret to split the dialog value at the click (abc!XYZ).\nScreen:\n{}",
        s
    );
}
