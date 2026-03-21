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
    harness.assert_screen_contains(">command");

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
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();

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

/// Test that status bar is visible by default
#[test]
fn test_status_bar_visible_by_default() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Status bar should show cursor position info (Ln/Col) at the expected row
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("Ln") && status_bar.contains("Col"),
        "Status bar should show cursor position. Got: {}",
        status_bar
    );
}

/// Test that toggling status bar via command palette hides and shows it
#[test]
fn test_toggle_status_bar_via_command_palette() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Status bar should be visible initially
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("Ln"),
        "Status bar should be visible initially. Got: {}",
        status_bar
    );

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type "toggle status bar" to find the command
    harness.type_text("Toggle Status Bar").unwrap();
    harness.render().unwrap();

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Status bar row should no longer show cursor position info
    let status_bar = harness.get_status_bar();
    assert!(
        !status_bar.contains("Ln"),
        "Status bar should be hidden after toggle. Got: {}",
        status_bar
    );

    // Toggle back - open command palette again
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Toggle Status Bar").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Status bar should be visible again with "Status bar shown" message
    harness.assert_screen_contains("Status bar shown");
}

/// Test that config option show_status_bar: false hides status bar on startup
#[test]
fn test_config_show_status_bar_false() {
    let mut config = Config::default();
    config.editor.show_status_bar = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    // The status bar row should not show cursor position info
    let status_bar_row = harness.get_screen_row(layout::status_bar_row(24));
    assert!(
        !status_bar_row.contains("Ln"),
        "Status bar should be hidden when show_status_bar is false. Got: {}",
        status_bar_row
    );
}

/// Test that all three bars can be hidden simultaneously
#[test]
fn test_all_bars_hidden() {
    let mut config = Config::default();
    config.editor.show_menu_bar = false;
    config.editor.show_tab_bar = false;
    config.editor.show_status_bar = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    // Menu bar row should not contain menu items
    let row0 = harness.get_screen_row(0);
    assert!(!row0.contains("File"), "Menu bar should be hidden");

    // Status bar row should not contain cursor position info
    let status_bar_row = harness.get_screen_row(layout::status_bar_row(24));
    assert!(
        !status_bar_row.contains("Ln"),
        "Status bar should be hidden. Got: {}",
        status_bar_row
    );
}

/// Test that the prompt line is visible by default
#[test]
fn test_prompt_line_visible_by_default() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    assert!(
        harness.editor().prompt_line_visible(),
        "Prompt line should be visible by default"
    );
}

/// Test that config option show_prompt_line: false hides prompt line on startup
#[test]
fn test_config_show_prompt_line_false() {
    let mut config = Config::default();
    config.editor.show_prompt_line = false;

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    assert!(
        !harness.editor().prompt_line_visible(),
        "Prompt line should be hidden when show_prompt_line is false"
    );
}

/// Test that changing show_prompt_line via the Settings UI takes effect immediately
#[test]
fn test_settings_show_prompt_line_applies_immediately() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();
    harness.render().unwrap();

    // Prompt line should be visible initially
    assert!(harness.editor().prompt_line_visible());

    // Open settings
    harness.open_settings().unwrap();

    // Search for "show_prompt_line"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("show_prompt").unwrap();
    harness.render().unwrap();

    // Jump to result and toggle it off
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Save with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Settings should be closed
    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after Ctrl+S"
    );

    // Prompt line should now be hidden (applied immediately, not requiring restart)
    assert!(
        !harness.editor().prompt_line_visible(),
        "Prompt line should be hidden after toggling show_prompt_line off via Settings UI"
    );
}

/// Test that toggling prompt line via command palette hides and shows it
#[test]
fn test_toggle_prompt_line_via_command_palette() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Prompt line should be visible initially
    assert!(harness.editor().prompt_line_visible());

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type "toggle prompt line" to find the command
    harness.type_text("Toggle Prompt Line").unwrap();
    harness.render().unwrap();

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Prompt line should now be hidden
    harness.assert_screen_contains("Prompt line hidden");
    assert!(!harness.editor().prompt_line_visible());

    // Toggle back - open command palette again
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Toggle Prompt Line").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Prompt line should be visible again
    harness.assert_screen_contains("Prompt line shown");
    assert!(harness.editor().prompt_line_visible());
}
