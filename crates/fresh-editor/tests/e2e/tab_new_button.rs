//! E2E test for the tab bar's trailing "+" new-tab button.
//!
//! Clicking the "+" tab at the end of the tab bar opens a small popup
//! menu offering "New Terminal" and "New File". Selecting "New File"
//! creates a new empty buffer (a second tab appears).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

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
fn plus_button_pins_to_right_edge_on_overflow() {
    // A narrow bar with many tabs forces the buffer tabs to overflow.
    let width: u16 = 50;
    let mut harness = EditorTestHarness::new(width, 24).unwrap();
    for _ in 0..8 {
        harness.new_buffer().unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let plus_col = col_of_char_on_row(&screen, 1, '+').unwrap_or_else(|| {
        panic!("expected a pinned '+' button on the tab row. Screen:\n{screen}")
    });

    // Pinned: the "+" cell occupies the rightmost columns of the bar
    // (" + " => '+' sits one column in from the right edge).
    assert!(
        plus_col >= width - 3,
        "expected '+' pinned near the right edge (col >= {}), got {plus_col}. Screen:\n{screen}",
        width - 3
    );

    // It is still interactive: clicking the pinned button opens the popup.
    harness.mouse_click(plus_col, 1).unwrap();
    harness.assert_screen_contains("New Terminal");
    harness.assert_screen_contains("New File");
}

/// Near the right edge the popup clamps left so it fits on screen — and its
/// click hit-test must clamp identically, or a click on the *drawn* item lands
/// outside the menu's (unclamped) region and is treated as an outside-click.
/// Regression: render clamped `menu.position` while the hit-tests used the raw
/// position, so clicking a visible item near the edge silently dismissed the
/// menu instead of activating it.
#[test]
fn plus_button_menu_near_right_edge_clicks_land_on_drawn_items() {
    // Narrow bar + overflow pins the "+" (and thus the popup anchor) hard
    // against the right edge, where `NEW_TAB_MENU_WIDTH` (18) overflows and
    // forces the clamp.
    let width: u16 = 50;
    let mut harness = EditorTestHarness::new(width, 24).unwrap();
    for _ in 0..8 {
        harness.new_buffer().unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let plus_col = col_of_char_on_row(&screen, 1, '+').unwrap_or_else(|| {
        panic!("expected a pinned '+' button on the tab row. Screen:\n{screen}")
    });
    assert!(
        plus_col >= width - 3,
        "precondition: '+' pinned near the right edge, got {plus_col}. Screen:\n{screen}"
    );

    harness.mouse_click(plus_col, 1).unwrap();
    harness.assert_screen_contains("New File");

    let buffers_before = harness.editor().active_window().buffers.len();

    // Click "New File" where it is actually drawn (its clamped position). With
    // the raw-position hit-test this click fell outside the menu → dismissed it
    // with no new buffer; with the shared clamp it activates the item.
    let screen = harness.screen_to_string();
    let (nf_col, nf_row) = pos_of_substr(&screen, "New File")
        .unwrap_or_else(|| panic!("expected 'New File' item in popup. Screen:\n{screen}"));
    harness.mouse_click(nf_col + 1, nf_row).unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("New Terminal"),
        "popup should be dismissed after selecting an item. Screen:\n{screen}"
    );
    assert_eq!(
        harness.editor().active_window().buffers.len(),
        buffers_before + 1,
        "clicking the drawn 'New File' near the edge must create a buffer, \
         not miss the item. Screen:\n{screen}"
    );
}

#[test]
fn plus_button_menu_captures_keyboard_and_filters_keys() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let plus_col = col_of_char_on_row(&screen, 1, '+').unwrap_or_else(|| {
        panic!("expected a '+' new-tab button on the tab row. Screen:\n{screen}")
    });
    harness.mouse_click(plus_col, 1).unwrap();
    harness.assert_screen_contains("New Terminal");

    // The popup is modal: typing while it's open must be swallowed, not
    // inserted into the active buffer underneath. Observed on screen: the
    // typed text never appears, and the status bar's cursor stays put.
    harness.type_text("ZZZ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("ZZZ");
    harness.assert_screen_contains("Ln 1, Col 1");
    harness.assert_screen_contains("New Terminal");

    // Keyboard navigation drives the popup: Down highlights "New File",
    // Enter activates it (creating a second buffer) and closes the popup.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("New Terminal"),
        "popup should be dismissed after Enter. Screen:\n{screen}"
    );
    harness.assert_screen_contains("[No Name] 2");
}

#[test]
fn plus_button_menu_dismisses_on_escape() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let plus_col = col_of_char_on_row(&screen, 1, '+').unwrap_or_else(|| {
        panic!("expected a '+' new-tab button on the tab row. Screen:\n{screen}")
    });
    harness.mouse_click(plus_col, 1).unwrap();
    harness.assert_screen_contains("New Terminal");

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("New Terminal"),
        "Esc should dismiss the '+' popup. Screen:\n{screen}"
    );
    assert!(
        !screen.contains("[No Name] 2"),
        "Esc should not create a new buffer. Screen:\n{screen}"
    );
}

#[test]
fn tab_context_menu_captures_keyboard_and_filters_keys() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    harness.render().unwrap();

    // Right-click the (only) tab to open its context menu.
    harness.mouse_right_click(2, 1).unwrap();
    harness.assert_screen_contains("Close Others");

    // Modal: typing must not leak into the buffer underneath. The menu
    // overlaps the text area, so observe the status bar's cursor instead —
    // it stays at "Ln 1, Col 1" rather than advancing to "Col 4".
    harness.type_text("QQQ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Ln 1, Col 1");
    harness.assert_screen_contains("Close Others");

    // Esc dismisses the menu without taking any action.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Close Others"),
        "Esc should dismiss the tab context menu. Screen:\n{screen}"
    );
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
