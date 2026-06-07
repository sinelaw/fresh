//! E2E test for the tab bar's trailing "+" new-tab button.
//!
//! Clicking the "+" tab at the end of the tab bar opens a small popup
//! menu offering "New Terminal" and "New File". Selecting "New File"
//! creates a new empty buffer (a second tab appears).

use crate::common::harness::EditorTestHarness;

/// Locate the 0-based cell column of `needle` on the given (0-based) screen row.
fn col_of_char_on_row(screen: &str, row: usize, needle: char) -> Option<u16> {
    let line = screen.lines().nth(row)?;
    line.chars().position(|c| c == needle).map(|p| p as u16)
}

/// Locate the (col, row) of the first cell of `needle` substring anywhere on screen.
fn pos_of_substr(screen: &str, needle: &str) -> Option<(u16, u16)> {
    for (row, line) in screen.lines().enumerate() {
        if let Some(byte_idx) = line.find(needle) {
            // Convert byte index to a cell/column index (count chars before it).
            let col = line[..byte_idx].chars().count() as u16;
            return Some((col, row as u16));
        }
    }
    None
}

#[test]
fn plus_button_opens_menu_and_new_file_creates_buffer() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    harness.render().unwrap();

    // The "+" button is rendered as a trailing tab on the tab row (row 1:
    // row 0 = menu bar, row 1 = tab bar).
    let screen = harness.screen_to_string();
    let plus_col = col_of_char_on_row(&screen, 1, '+').unwrap_or_else(|| {
        panic!("expected a '+' new-tab button on the tab row. Screen:\n{screen}")
    });

    // Click the "+" button — the popup should appear.
    harness.mouse_click(plus_col, 1).unwrap();
    harness.assert_screen_contains("New Terminal");
    harness.assert_screen_contains("New File");

    // Click the "New File" item in the popup.
    let screen = harness.screen_to_string();
    let (nf_col, nf_row) = pos_of_substr(&screen, "New File")
        .unwrap_or_else(|| panic!("expected 'New File' item in popup. Screen:\n{screen}"));
    harness.mouse_click(nf_col + 1, nf_row).unwrap();

    // The popup closes and a second buffer exists: with two unnamed
    // buffers the tabs are disambiguated as "[No Name] 1" / "[No Name] 2".
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("New Terminal"),
        "popup should be dismissed after selecting an item. Screen:\n{screen}"
    );
    harness.assert_screen_contains("[No Name] 2");
}

#[test]
fn plus_button_menu_dismisses_on_outside_click() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let plus_col = col_of_char_on_row(&screen, 1, '+').unwrap_or_else(|| {
        panic!("expected a '+' new-tab button on the tab row. Screen:\n{screen}")
    });

    harness.mouse_click(plus_col, 1).unwrap();
    harness.assert_screen_contains("New Terminal");

    // Click far away in the editor content area — the popup should close
    // without creating a new buffer.
    harness.mouse_click(2, 10).unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("New Terminal"),
        "popup should be dismissed by an outside click. Screen:\n{screen}"
    );
    assert!(
        !screen.contains("[No Name] 2"),
        "outside click should not create a new buffer. Screen:\n{screen}"
    );
}
