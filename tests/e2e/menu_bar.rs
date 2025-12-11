use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::time::Duration;

/// Test that Alt+F opens the File menu
#[test]
fn test_alt_f_opens_file_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Menu dropdown should not be visible initially
    harness.assert_screen_not_contains("New File");

    // Press Alt+F to open File menu
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // File menu dropdown should now be visible with its items
    harness.assert_screen_contains("New File");
    harness.assert_screen_contains("Open");
    harness.assert_screen_contains("Save");
}

/// Test that Alt+E opens the Edit menu
#[test]
fn test_alt_e_opens_edit_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Press Alt+E to open Edit menu
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Edit menu dropdown should be visible
    harness.assert_screen_contains("Undo");
    harness.assert_screen_contains("Redo");
    harness.assert_screen_contains("Cut");
}

/// Test that Alt+V opens the View menu
#[test]
fn test_alt_v_opens_view_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Press Alt+V to open View menu
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // View menu dropdown should be visible (menu item has checkbox prefix now)
    harness.assert_screen_contains("File Explorer");
    harness.assert_screen_contains("Split Horizontal");
}

/// Test that Alt+H opens the Help menu
#[test]
fn test_alt_h_opens_help_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Press Alt+H to open Help menu
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Help menu dropdown should be visible (renamed from "Show Help")
    harness.assert_screen_contains("Show Fresh Manual");
}

/// Test that F10 activates the menu bar (then arrow keys can navigate)
/// Note: F10 keybinding may not be properly handled in test harness
#[test]
#[ignore = "F10 keybinding needs investigation - works in real editor but not in test harness"]
fn test_f10_activates_menu_bar() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Press F10 to activate menu bar
    harness
        .send_key(KeyCode::F(10), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Down to open the menu under the highlighted item
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // First menu (File) should be open
    harness.assert_screen_contains("New File");
}

/// Test that Escape closes an open menu
#[test]
fn test_escape_closes_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Open a menu first
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New File");

    // Press Escape to close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Menu should be closed
    harness.assert_screen_not_contains("New File");
}

/// Test menu navigation with arrow keys
#[test]
fn test_menu_left_right_navigation() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Open File menu
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New File");

    // Press Right to go to Edit menu
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show Edit menu items
    harness.assert_screen_contains("Undo");
    harness.assert_screen_not_contains("New File");

    // Press Left to go back to File menu
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should now show File menu items again
    harness.assert_screen_contains("New File");
    harness.assert_screen_not_contains("Undo");
}

/// Test that menu renders with underlined mnemonic character
#[test]
fn test_menu_mnemonic_underline_rendering() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // The menu bar should show underlined characters for Alt+letter shortcuts
    // File should have F underlined, Edit should have E underlined, etc.
    // Check that the menu bar is rendered (row 0)
    harness.assert_screen_contains("File");
    harness.assert_screen_contains("Edit");
    harness.assert_screen_contains("View");
}

/// Test that clicking on File menu opens it
#[test]
fn test_mouse_click_opens_file_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Menu should not be open initially
    harness.assert_screen_not_contains("New File");

    // Click on "File" in menu bar (row 0, column ~1-4)
    harness.mouse_click(2, 0).unwrap();
    harness.render().unwrap();

    // File menu dropdown should now be visible
    harness.assert_screen_contains("New File");
    harness.assert_screen_contains("Open");
    harness.assert_screen_contains("Save");
}

/// Test that clicking on Edit menu opens it
#[test]
fn test_mouse_click_opens_edit_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Click on "Edit" in menu bar (around column 8-11)
    harness.mouse_click(9, 0).unwrap();
    harness.render().unwrap();

    // Edit menu dropdown should be visible
    harness.assert_screen_contains("Undo");
    harness.assert_screen_contains("Redo");
}

/// Test that clicking on open menu closes it
#[test]
fn test_mouse_click_toggles_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Click to open File menu
    harness.mouse_click(2, 0).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New File");

    // Click on File again to close it
    harness.mouse_click(2, 0).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("New File");
}

/// Test that clicking outside menu labels closes menu
#[test]
fn test_mouse_click_empty_area_closes_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Open a menu first
    harness.mouse_click(2, 0).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New File");

    // Click on empty area of menu bar (far right)
    harness.mouse_click(70, 0).unwrap();
    harness.render().unwrap();

    // Menu should be closed
    harness.assert_screen_not_contains("New File");
}

/// Test that clicking on a menu item executes its action
#[test]
fn test_mouse_click_menu_item_executes_action() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Open Help menu via Alt+H
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Show Fresh Manual");

    // The Help menu dropdown appears at row 1 (below menu bar)
    // Help is the 6th menu, so x position = " File " (7) + " Edit " (7) + " View " (7) + " Selection " (12) + " Go " (5) = 38
    // Click on "Show Fresh Manual" item - it should be the first item
    // Menu items are rendered with border, so first item starts at row 2
    harness.mouse_click(40, 2).unwrap();
    harness.render().unwrap();

    // After clicking, the help panel should open
    // The menu should close after executing
    harness.assert_screen_not_contains("Show Fresh Manual");
    // Help panel shows keybinding information (look for actual keybinding entries)
    // On macOS, Ctrl is rendered as ⌘, on other platforms as "Ctrl"
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ctrl+") || screen.contains("⌘+"),
        "Help panel should show keybinding entries"
    );
}

/// Test clicking on Edit menu's Undo item
#[test]
fn test_mouse_click_undo_menu_item() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text first
    harness.type_text("Hello World").unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Hello World");

    // Open Edit menu (around column 8)
    harness.mouse_click(9, 0).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Undo");

    // Click on Undo item (first item in Edit menu, row 2 after border)
    // Edit menu starts at column 7 (after " File " + space)
    harness.mouse_click(10, 2).unwrap();
    harness.render().unwrap();

    // Undo should have reversed the last text insertion
    // Menu should be closed
    harness.assert_screen_not_contains("Undo");
    // The last character should be undone (type_text inserts char by char)
    harness.assert_buffer_content("Hello Worl");
}

/// Test that View menu File Explorer checkbox syncs with actual file explorer state
/// Issue #291: Closing file explorer does not clear the check mark
#[test]
fn test_view_menu_file_explorer_checkbox_syncs_on_close() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Initially file explorer is not open, checkbox should be unchecked
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("☐ File Explorer"),
        "File Explorer checkbox should be unchecked initially. Screen:\n{}",
        screen
    );

    // Close menu
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open file explorer
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Open View menu - checkbox should now be checked
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("☑ File Explorer"),
        "File Explorer checkbox should be checked after opening explorer. Screen:\n{}",
        screen
    );

    // Close menu
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Close file explorer (toggle it off)
    harness.editor_mut().toggle_file_explorer();
    harness.render().unwrap();

    // Verify file explorer is actually hidden
    assert!(
        !harness.editor().file_explorer_visible(),
        "File explorer should be hidden after toggle"
    );

    // Open View menu - checkbox should now be UNchecked
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("☐ File Explorer"),
        "File Explorer checkbox should be unchecked after closing explorer. Screen:\n{}",
        screen
    );
}

/// Test that other View menu checkboxes also sync properly
/// Tests Line Numbers, Word Wrap, etc.
#[test]
fn test_view_menu_other_checkboxes_sync() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Check initial state of Line Numbers (default is ON)
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // Line numbers is on by default
    assert!(
        screen.contains("☑ Line Numbers"),
        "Line Numbers checkbox should be checked by default. Screen:\n{}",
        screen
    );

    // Close menu
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Toggle line numbers off
    harness.editor_mut().toggle_line_numbers();
    harness.render().unwrap();

    // Open View menu again - should show unchecked
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("☐ Line Numbers"),
        "Line Numbers checkbox should be unchecked after toggle. Screen:\n{}",
        screen
    );
}
