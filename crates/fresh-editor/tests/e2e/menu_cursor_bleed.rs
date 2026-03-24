//! E2E test for #1114: cursor styling bleeds through dropdown menus.
//!
//! When the user clicks on text in the document (positioning the cursor) and
//! then opens a dropdown menu, the cursor's REVERSED cell styling should not
//! be visible through the menu overlay. The dropdown menu should fully occlude
//! the editor cursor styling.

use crate::common::harness::{layout, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Modifier;

/// Test that the cursor cell styling does not bleed through an open dropdown menu.
///
/// Reproduction:
/// 1. Type enough text so the cursor is at a position that will be covered by a menu dropdown
/// 2. Click on the text to ensure cursor is placed there
/// 3. Open a dropdown menu (File menu) that covers the cursor position
/// 4. Verify: no cell within the dropdown area has REVERSED modifier (cursor styling)
#[test]
fn test_cursor_does_not_bleed_through_dropdown_menu() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();
    // Use software-cursor-only mode so the cursor cell gets REVERSED styling.
    // Without this, the hardware cursor path skips REVERSED and there is
    // nothing to bleed through — making the test vacuous.
    harness.set_software_cursor_only(true);

    // Type several lines of text so there's content under where the File menu will drop down.
    harness.type_text("Line one").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Line two").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Line three").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Line four").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Line five").unwrap();

    // Click on "Line two" to position the cursor there.
    // Row 0 = menu bar, row 1 = tab bar, row 2+ = content
    let cursor_row = layout::CONTENT_START_ROW as u16 + 1; // line 2 in content area
    let cursor_col = 10;
    harness.mouse_click(cursor_col, cursor_row).unwrap();

    // Verify cursor is now on line 2
    let status = harness.get_status_bar();
    assert!(
        status.contains("Ln 2"),
        "Expected cursor on line 2, got status: {}",
        status
    );

    // Verify the cursor cell has REVERSED modifier before opening the menu
    let cursor_style_before = harness.get_cell_style(cursor_col, cursor_row);
    assert!(
        cursor_style_before
            .map(|s| s.add_modifier.contains(Modifier::REVERSED))
            .unwrap_or(false),
        "Cursor cell should have REVERSED modifier before menu opens"
    );

    // Now open the File menu (Alt+F)
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Verify the menu is open
    harness.assert_screen_contains("New File");
    harness.assert_screen_contains("Save");

    let screen = harness.screen_to_string();

    // Find the menu dropdown bounds by locating box-drawing border chars
    let mut menu_top = None;
    let mut menu_bottom = None;
    let mut menu_left = None;
    let mut menu_right = None;
    for row in 0..harness.terminal_height() as u16 {
        let row_text = harness.get_row_text(row);
        let chars: Vec<char> = row_text.chars().collect();
        if let Some(left) = chars.iter().position(|&c| c == '┌') {
            if let Some(right) = chars.iter().rposition(|&c| c == '┐') {
                menu_top = Some(row);
                menu_left = Some(left as u16);
                menu_right = Some(right as u16);
            }
        }
        if chars.iter().any(|&c| c == '└') {
            menu_bottom = Some(row);
        }
    }

    let menu_top = menu_top.expect("no menu top border found");
    let menu_bottom = menu_bottom.expect("no menu bottom border found");
    let menu_left = menu_left.unwrap();
    let menu_right = menu_right.unwrap();
    eprintln!(
        "Menu dropdown: rows {}..{}, cols {}..{}",
        menu_top, menu_bottom, menu_left, menu_right
    );

    // The cursor was at (cursor_col, cursor_row). Verify it's within the dropdown area.
    assert!(
        cursor_row >= menu_top && cursor_row <= menu_bottom,
        "Cursor row {} should be within menu rows {}..{}",
        cursor_row,
        menu_top,
        menu_bottom
    );

    // Scan all cells within the dropdown area for REVERSED modifier.
    // No cell should have REVERSED — that would mean the cursor styling leaked through.
    let mut reversed_cells = Vec::new();
    for row in menu_top..=menu_bottom {
        for col in menu_left..=menu_right {
            if let Some(style) = harness.get_cell_style(col, row) {
                if style.add_modifier.contains(Modifier::REVERSED) {
                    reversed_cells.push((col, row));
                }
            }
        }
    }

    assert!(
        reversed_cells.is_empty(),
        "Bug #1114: Cursor REVERSED styling bleeds through the dropdown menu at {:?}. \
         The dropdown should fully overwrite cursor styling. Screen:\n{}",
        reversed_cells,
        screen
    );
}
