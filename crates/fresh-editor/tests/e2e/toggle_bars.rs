use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

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
    // 120×24 instead of 80×24: with `{remote}` on the
    // default status bar, the trailing Messages element
    // gets ellipsis-truncated at 80. The widening keeps
    // 'Status bar shown' / 'Menu bar hidden' / etc.
    // readable for the assertions below.
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
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
    // 120×24 instead of 80×24: with `{remote}` on the
    // default status bar, the trailing Messages element
    // gets ellipsis-truncated at 80. The widening keeps
    // 'Status bar shown' / 'Menu bar hidden' / etc.
    // readable for the assertions below.
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
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
    // 120×24 instead of 80×24: with `{remote}` on the
    // default status bar, the trailing Messages element
    // gets ellipsis-truncated at 80. The widening keeps
    // 'Status bar shown' / 'Menu bar hidden' / etc.
    // readable for the assertions below.
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
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

/// Test that toggling the prompt line off at runtime hides it.
/// (The config-load path with show_prompt_line=false is covered by
/// `test_settings_show_prompt_line_applies_immediately`; the harness
/// always forces show_prompt_line=true so layout-sensitive tests stay
/// stable, hence this test exercises the runtime toggle instead.)
#[test]
fn test_toggle_prompt_line_off_hides_it() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    assert!(harness.editor().prompt_line_visible());
    harness.editor_mut().toggle_prompt_line();
    assert!(
        !harness.editor().prompt_line_visible(),
        "Prompt line should be hidden after toggling it off"
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
    // 120×24 instead of 80×24: with `{remote}` on the
    // default status bar, the trailing Messages element
    // gets ellipsis-truncated at 80. The widening keeps
    // 'Status bar shown' / 'Menu bar hidden' / etc.
    // readable for the assertions below.
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
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

/// Regression test for issue #1156: toggling the menu bar via the View menu /
/// command palette should persist to the global user config so the change is
/// truly global (every workspace picks it up on next launch), not a per-
/// workspace override.
#[test]
fn test_toggle_menu_bar_persists_to_global_config() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open the editor and toggle the menu bar off via the runtime
    // action (same code path used by the View menu and the "Toggle Menu Bar"
    // command-palette entry).
    {
        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(Config::default())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.render().unwrap();

        // Sanity: the global default is "show menu bar" = true.
        assert!(harness.editor().config().editor.show_menu_bar);

        harness.editor_mut().toggle_menu_bar();

        // After toggling, the runtime config must reflect the new value.
        assert!(
            !harness.editor().config().editor.show_menu_bar,
            "toggle_menu_bar should update show_menu_bar in the global config"
        );
    }

    // Session 2: a different working directory using the same user config
    // dir. Loading the config layers from disk must reflect the persisted
    // change — otherwise the toggle was per-workspace, not global.
    let other_project = temp_dir.path().join("other_project");
    std::fs::create_dir(&other_project).unwrap();
    let loaded = Config::load_with_layers(&dir_context, &other_project);
    assert!(
        !loaded.editor.show_menu_bar,
        "Toggle should persist to user config so unrelated workspaces inherit it"
    );
}

/// Regression test for issue #1156: a stale `menu_bar_hidden` workspace
/// override from an older Fresh release must not silently win over the
/// current global `editor.show_menu_bar` setting. The setting is global,
/// so the override is treated as legacy and ignored on restore.
#[test]
fn test_workspace_override_does_not_shadow_global_show_menu_bar() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: hide the menu bar (toggle persists to global user config)
    // and save the workspace. Older builds also wrote a per-workspace
    // `menu_bar_hidden=true` override here.
    {
        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(Config::default())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.render().unwrap();
        harness.editor_mut().toggle_menu_bar();
        harness.shutdown(true).unwrap();
    }

    // Session 2: the user re-enables the menu bar globally before reopening
    // (e.g. via the Settings UI on a different machine, or by editing the
    // config file). Now reopen the same workspace — the global setting
    // must win.
    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config({
                let mut c = Config::default();
                c.editor.show_menu_bar = true;
                c
            })
            .with_working_dir(project_dir.clone())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.editor_mut().try_restore_workspace().unwrap();
    harness.render().unwrap();

    let menu_bar_row = harness.get_screen_row(0);
    assert!(
        menu_bar_row.contains("File"),
        "Global show_menu_bar=true must take precedence over a stale \
         workspace override. Got row 0: {:?}",
        menu_bar_row
    );
}
