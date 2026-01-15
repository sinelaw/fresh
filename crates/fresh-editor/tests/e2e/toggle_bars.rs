use crate::common::harness::{layout, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Test that the tab bar is visible by default
#[test]
fn test_tab_bar_visible_by_default() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Tab bar should be visible at row 1 (after menu bar)
    // Check that tab bar area shows the default buffer name "[No Name]"
    let tab_bar_row = harness.get_tab_bar();
    assert!(
        tab_bar_row.contains("[No Name]") || tab_bar_row.contains("untitled"),
        "Tab bar should show buffer name at row {}. Got: {}",
        layout::TAB_BAR_ROW,
        tab_bar_row
    );
}

/// Test that the menu bar is visible by default
#[test]
fn test_menu_bar_visible_by_default() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Menu bar should be visible at row 0
    let menu_bar_row = harness.get_menu_bar();
    assert!(
        menu_bar_row.contains("File") && menu_bar_row.contains("Edit"),
        "Menu bar should show File and Edit menus at row {}. Got: {}",
        layout::MENU_BAR_ROW,
        menu_bar_row
    );
}

/// Test that toggling tab bar via command palette hides and shows it
#[test]
fn test_toggle_tab_bar_via_command_palette() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Verify tab bar is visible initially (shows "[No Name]" for new buffer)
    harness.assert_screen_contains("[No Name]");

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command:");

    // Type "toggle tab bar" to find the command
    harness.type_text("Toggle Tab Bar").unwrap();
    harness.render().unwrap();

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab bar should now be hidden - the status message should appear
    harness.assert_screen_contains("Tab bar hidden");

    // Toggle back - open command palette again
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Toggle Tab Bar").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab bar should be visible again
    harness.assert_screen_contains("Tab bar shown");
}

/// Test that toggling menu bar via command palette hides and shows it
#[test]
fn test_toggle_menu_bar_via_command_palette() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Verify menu bar is visible initially
    let menu_bar = harness.get_menu_bar();
    assert!(
        menu_bar.contains("File"),
        "Menu bar should be visible initially"
    );

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type "toggle menu bar" to find the command
    harness.type_text("Toggle Menu Bar").unwrap();
    harness.render().unwrap();

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Menu bar should now be hidden
    harness.assert_screen_contains("Menu bar hidden");

    // The row that was menu bar should no longer contain "File"
    let menu_bar = harness.get_screen_row(layout::MENU_BAR_ROW);
    assert!(
        !menu_bar.contains("File"),
        "Menu bar should be hidden after toggle"
    );

    // Toggle back
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Toggle Menu Bar").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Menu bar should be visible again
    harness.assert_screen_contains("Menu bar shown");
}

/// Test that config option show_tab_bar: false hides tab bar on startup
#[test]
fn test_config_show_tab_bar_false() {
    let mut config = Config::default();
    config.editor.show_tab_bar = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    // The menu bar (row 0) should still show File/Edit
    let menu_bar = harness.get_menu_bar();
    assert!(
        menu_bar.contains("File"),
        "Menu bar should still be visible"
    );

    // The tab bar toggle getter should return false
    assert!(!harness.editor().tab_bar_visible());
}

/// Test that config option show_menu_bar: false hides menu bar on startup
#[test]
fn test_config_show_menu_bar_false() {
    let mut config = Config::default();
    config.editor.show_menu_bar = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    // Menu bar should be hidden
    let row0 = harness.get_screen_row(0);
    assert!(
        !row0.contains("File"),
        "Menu bar should be hidden when show_menu_bar is false. Got: {}",
        row0
    );
}

/// Test that both bars can be hidden simultaneously
#[test]
fn test_both_bars_hidden() {
    let mut config = Config::default();
    config.editor.show_menu_bar = false;
    config.editor.show_tab_bar = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    // Neither bar should be visible
    let row0 = harness.get_screen_row(0);
    assert!(!row0.contains("File"), "Menu bar should be hidden");

    // Content should start at row 0 or close to it
    // Since both bars are hidden, more screen real estate is available
    assert!(!harness.editor().tab_bar_visible());
}

/// Test that tab bar toggle works correctly when opening multiple files
#[test]
fn test_tab_bar_toggle_with_multiple_buffers() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Create test files
    let project_dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(project_dir.join("file1.txt"), "content 1").unwrap();
    std::fs::write(project_dir.join("file2.txt"), "content 2").unwrap();

    // Open first file
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("file1.txt").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open second file
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("file2.txt").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Both files should be in tab bar
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");

    // Hide tab bar
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Toggle Tab Bar").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab bar should be hidden
    harness.assert_screen_contains("Tab bar hidden");
    assert!(!harness.editor().tab_bar_visible());

    // Show tab bar again
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Toggle Tab Bar").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab bar should be visible with both files
    harness.assert_screen_contains("Tab bar shown");
    assert!(harness.editor().tab_bar_visible());
}
