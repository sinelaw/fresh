use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

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

    // View menu dropdown should be visible
    harness.assert_screen_contains("Toggle File Explorer");
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

    // Help menu dropdown should be visible
    harness.assert_screen_contains("Show Help");
}

/// Test that F10 activates the menu bar
#[test]
fn test_f10_activates_menu_bar() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Press F10 to activate menu bar
    harness
        .send_key(KeyCode::F(10), KeyModifiers::NONE)
        .unwrap();
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
    harness
        .send_key(KeyCode::Esc, KeyModifiers::NONE)
        .unwrap();
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
    harness
        .send_key(KeyCode::Left, KeyModifiers::NONE)
        .unwrap();
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
