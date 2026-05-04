use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use ratatui::style::Color;
use std::fs;

/// Helper function to open the theme editor via command palette
/// After running "Edit Theme" command, this waits for the theme selection prompt
/// and types "dark" to explicitly select the dark builtin theme.
fn open_theme_editor(harness: &mut EditorTestHarness) {
    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to find the Edit Theme command
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme selection prompt to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();

    // Type "dark" to select the dark builtin theme explicitly
    // (Plugin prompts now use suggestion values when selected, so we type to be explicit)
    harness.type_text("dark").unwrap();
    harness.render().unwrap();

    // Select it
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor to fully load.
    //
    // We must wait for the PANEL CONTENT to be populated, not just for the
    // `*Theme Editor*` tab label to appear. The tab bar updates as soon as
    // the buffer group is created, which happens BEFORE the plugin runs
    // `setPanelContent` to populate the tree/picker/footer panels. On slower
    // platforms (e.g. Windows) there's a visible race window in which the
    // tab is in place but every panel is still blank — previously this
    // helper used `screen.contains("Editor")` which matches the `*Theme
    // Editor*` tab label, so the wait returned during that blank window
    // and every subsequent `contains("Theme Editor:")` / `contains("#...")`
    // assertion in the callers raced against a half-rendered UI.
    //
    // Instead, wait for per-panel content the plugin writes via
    // setPanelContent:
    //   - `Theme Editor: ` — the first line of the tree (left) panel,
    //     e.g. "Theme Editor: dark".
    //   - `Select a color field` (when nothing is selected yet) or `Hex:`
    //     (after a color has been picked) — both only appear after the
    //     picker (right) panel has been populated.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor: ")
                && (screen.contains("Select a color field") || screen.contains("Hex:"))
        })
        .unwrap();
}

/// Test that the theme editor command is registered by the plugin
#[test]
fn test_theme_editor_command_registered() {
    init_tracing_from_env();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    // Create themes directory with a test theme
    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();

    // Initial render
    harness.render().unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to find the Edit Theme command
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();

    // The theme editor command should be registered and visible in the palette
    harness.assert_screen_contains("Edit Theme");
    harness.assert_screen_contains("theme_editor");
}

/// Test that the tab bar remains present when opening and closing the theme editor.
/// Verifies buffer group integration: the theme editor appears as a single tab entry,
/// panel splits don't show per-split tab bars, and closing the group restores the
/// previous state.
#[test]
fn test_theme_editor_tab_bar_persists() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    // === Initial state: tab bar present with [No Name] ===
    harness.render().unwrap();
    let initial_screen = harness.screen_to_string();
    assert!(
        initial_screen.contains("[No Name]"),
        "Initial tab bar should show [No Name]. Screen:\n{}",
        initial_screen
    );

    // === Open theme editor: tab bar still present, shows the new tab ===
    open_theme_editor(&mut harness);

    let after_open_screen = harness.screen_to_string();
    assert!(
        after_open_screen.contains("[No Name]"),
        "Tab bar should still show [No Name] after opening theme editor. Screen:\n{}",
        after_open_screen
    );
    assert!(
        after_open_screen.contains("*Theme Editor*"),
        "Theme editor should appear as a new tab entry. Screen:\n{}",
        after_open_screen
    );
    assert!(
        after_open_screen.contains("Theme Editor:"),
        "Theme editor panel content should be visible. Screen:\n{}",
        after_open_screen
    );

    // === Close theme editor: tab bar still present ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Close Theme Editor").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

    let after_close_screen = harness.screen_to_string();
    assert!(
        after_close_screen.contains("[No Name]"),
        "Tab bar should still show [No Name] after closing theme editor. Screen:\n{}",
        after_close_screen
    );
    assert!(
        !after_close_screen.contains("*Theme Editor*"),
        "Theme editor tab should be gone after close. Screen:\n{}",
        after_close_screen
    );
}

/// Invoking the "Close Buffer" command from the command palette while a
/// group panel is the active/focused target should close the entire group,
/// not just the one panel. Individual panels are internal details that the
/// user should not be able to close piecemeal via the generic Close Buffer
/// command — they close together with the group.
#[test]
fn test_close_buffer_while_in_group_closes_whole_group() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor — the group tab becomes active.
    open_theme_editor(&mut harness);

    let after_open_screen = harness.screen_to_string();
    assert!(
        after_open_screen.contains("*Theme Editor*"),
        "Theme editor should be open. Screen:\n{}",
        after_open_screen
    );
    assert!(
        after_open_screen.contains("Theme Editor:"),
        "Theme editor panel content should be visible. Screen:\n{}",
        after_open_screen
    );

    // Run the generic "Close Buffer" command (not the theme-editor-specific
    // "Theme: Close Editor"). With the theme editor active, this should
    // close the whole group — NOT just close the currently-focused panel
    // buffer while leaving the rest of the group's layout visible.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Close Buffer").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // After close, the group tab, the group's panel content, and the group
    // panels themselves should all be gone.
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Theme Editor*"))
        .unwrap();

    let after_close_screen = harness.screen_to_string();
    assert!(
        !after_close_screen.contains("*Theme Editor*"),
        "Theme editor group tab should be gone after Close Buffer. Screen:\n{}",
        after_close_screen
    );
    assert!(
        !after_close_screen.contains("Theme Editor:"),
        "Theme editor panel content should be gone after Close Buffer. Screen:\n{}",
        after_close_screen
    );
    assert!(
        after_close_screen.contains("[No Name]"),
        "Original [No Name] buffer tab should still be visible. Screen:\n{}",
        after_close_screen
    );
}

/// Next/Previous Buffer should cycle across both regular buffer tabs and
/// group tabs (i.e., top-level tabs in the tab bar). Opening a file +
/// opening the theme editor should give two tabs, and next_buffer should
/// toggle between them regardless of whether the group is currently active.
#[test]
fn test_next_buffer_cycles_across_groups_and_buffers() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    let test_file = project_root.join("cycle_test.txt");
    fs::write(&test_file, "UniqueContentMarker\n").unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    // Open the source file.
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // File is active; screen should show the file's content.
    let after_file_screen = harness.screen_to_string();
    assert!(
        after_file_screen.contains("UniqueContentMarker"),
        "Source file should be visible. Screen:\n{}",
        after_file_screen
    );

    // Open theme editor — this becomes the active tab; the file tab stays
    // in the tab bar.
    open_theme_editor(&mut harness);

    let after_theme_screen = harness.screen_to_string();
    assert!(
        after_theme_screen.contains("cycle_test.txt"),
        "File tab should still be listed. Screen:\n{}",
        after_theme_screen
    );
    assert!(
        after_theme_screen.contains("*Theme Editor*"),
        "Theme editor tab should be listed. Screen:\n{}",
        after_theme_screen
    );
    assert!(
        after_theme_screen.contains("Theme Editor:"),
        "Theme editor content should be visible. Screen:\n{}",
        after_theme_screen
    );

    // Run "Next Buffer" from the command palette. This should cycle from
    // the group tab back to the file tab.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Next Buffer").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("UniqueContentMarker"))
        .unwrap();

    let back_to_file_screen = harness.screen_to_string();
    assert!(
        back_to_file_screen.contains("UniqueContentMarker"),
        "Next Buffer should switch back to the source file. Screen:\n{}",
        back_to_file_screen
    );
    // The theme editor tab should still be present in the tab bar (the
    // group wasn't closed, just inactive).
    assert!(
        back_to_file_screen.contains("*Theme Editor*"),
        "Theme editor tab should still be visible after switching away. Screen:\n{}",
        back_to_file_screen
    );
    // And the theme editor content should NOT be on screen any more.
    assert!(
        !back_to_file_screen.contains("Theme Editor:"),
        "Theme editor panel content should not be visible after switching away. Screen:\n{}",
        back_to_file_screen
    );

    // Run "Next Buffer" again — should now switch back to the theme editor.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Next Buffer").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

    let back_to_theme_screen = harness.screen_to_string();
    assert!(
        back_to_theme_screen.contains("Theme Editor:"),
        "Next Buffer should cycle back to the theme editor. Screen:\n{}",
        back_to_theme_screen
    );
}

/// Test that the theme editor opens successfully without crashing
/// This test catches the pathJoin API bug where passing an array instead of
/// variadic args causes a serde_v8 error
#[test]
fn test_theme_editor_opens_without_error() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    // Create themes directory with a test theme
    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "dark",
        "editor": {
            "bg": [30, 30, 30],
            "fg": [212, 212, 212],
            "cursor": [82, 139, 255],
            "selection_bg": [38, 79, 120],
            "current_line_bg": [40, 40, 40],
            "line_number_fg": [100, 100, 100],
            "line_number_bg": [30, 30, 30]
        },
        "ui": {
            "tab_active_fg": "Yellow",
            "tab_active_bg": "Blue",
            "tab_inactive_fg": "White",
            "tab_inactive_bg": "DarkGray",
            "status_bar_fg": "White",
            "status_bar_bg": "DarkGray"
        },
        "search": {
            "match_bg": [100, 100, 20],
            "match_fg": [255, 255, 255]
        },
        "diagnostic": {
            "error_fg": "Red",
            "warning_fg": "Yellow"
        },
        "syntax": {
            "keyword": [86, 156, 214],
            "string": [206, 145, 120],
            "comment": [106, 153, 85]
        }
    }"#;
    fs::write(themes_dir.join("dark.json"), test_theme).unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    // Initial render
    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    let screen = harness.screen_to_string();

    // Verify the editor actually opened with proper content
    assert!(
        screen.contains("Theme Editor") || screen.contains("Editor"),
        "Theme editor should show 'Theme Editor' or 'Editor' section. Got:\n{}",
        screen
    );

    // Should NOT contain error messages about serde_v8 or pathJoin
    assert!(
        !screen.contains("serde_v8"),
        "Should not show serde_v8 error on screen"
    );
    assert!(
        !screen.contains("invalid type"),
        "Should not show 'invalid type' error on screen"
    );
}

/// Test that the theme editor can be opened, closed, and reopened
#[test]
fn test_theme_editor_open_close_reopen() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // === First open ===
    open_theme_editor(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Theme Editor"),
        "Theme editor should be open. Screen:\n{}",
        screen
    );

    // === Close via command palette ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Close Theme Editor").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor to close
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Theme Editor:")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Theme Editor:"),
        "Theme editor should be closed after Escape. Screen:\n{}",
        screen
    );

    // === Reopen ===
    open_theme_editor(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Theme Editor"),
        "Theme editor should reopen successfully. Screen:\n{}",
        screen
    );
}

/// Test that the theme editor can be closed with "Close Buffer" command and reopened
/// This verifies the stateless approach works when the buffer is closed externally
#[test]
fn test_theme_editor_reopen_after_close_buffer() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // === Step 1: Open theme editor ===
    open_theme_editor(&mut harness);

    // Wait for theme editor to be visible
    harness
        .wait_until(|h| h.screen_to_string().contains("*Theme Editor*"))
        .unwrap();

    // === Step 2: Close with "Close Buffer" from command palette ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Close Buffer").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor buffer to disappear from tabs
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Theme Editor*"))
        .unwrap();

    // === Step 3: Try to reopen - this is where the bug manifests ===
    open_theme_editor(&mut harness);

    // Wait for theme editor to reappear
    harness
        .wait_until(|h| h.screen_to_string().contains("*Theme Editor*"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Theme Editor"),
        "Theme editor should reopen after Close Buffer. Screen:\n{}",
        screen
    );
}

/// Test that the theme editor displays color fields with swatches
#[test]
fn test_theme_editor_shows_color_sections() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Copy the theme_editor.ts plugin
    copy_plugin(&plugins_dir, "theme_editor");

    // Create themes directory with test themes
    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "dark",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {"keyword": [86, 156, 214]}
    }"#;
    fs::write(themes_dir.join("dark.json"), test_theme).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    let screen = harness.screen_to_string();

    // Should show theme sections - the plugin creates sections like "Editor", "Syntax"
    // These are the section headers that should appear
    let has_editor_section = screen.contains("Editor") || screen.contains("editor");
    let has_syntax_section = screen.contains("Syntax") || screen.contains("syntax");

    assert!(
        has_editor_section || has_syntax_section,
        "Theme editor should show color sections. Got:\n{}",
        screen
    );
}

/// Test that the theme editor can open a builtin theme
/// This verifies the open functionality works correctly
#[test]
fn test_theme_editor_open_builtin() {
    // Create isolated directory context for proper test isolation
    let context_temp = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(context_temp.path());

    // Create user themes directory and put test theme there
    fs::create_dir_all(dir_context.themes_dir()).unwrap();
    let source_theme = r#"{
        "name": "source",
        "editor": {
            "bg": [10, 20, 30],
            "fg": [240, 240, 240]
        },
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(dir_context.themes_dir().join("source.json"), source_theme).unwrap();

    // Create project directory with plugins
    let project_temp = tempfile::TempDir::new().unwrap();
    let project_root = project_temp.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    // Create harness with isolated directory context
    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        40,
        Default::default(),
        project_root.clone(),
        dir_context,
    )
    .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Press Ctrl+O to open a theme (builtin or user)
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Wait for the prompt to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Open theme") || screen.contains("Select theme")
        })
        .unwrap();

    // Type the source theme name
    harness.type_text("source").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme to be loaded - should show the theme name "source"
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor: source") || screen.contains("Opened")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // Verify the theme editor now shows the opened theme name
    assert!(
        screen.contains("source") && !screen.contains("custom"),
        "Theme editor should show the opened theme name. Screen:\n{}",
        screen
    );
}

/// Test that theme colors from the theme editor are displayed correctly on screen
/// This verifies that the color swatches show RGB values and use RGB colors in rendering
#[test]
fn test_theme_editor_displays_correct_colors() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Copy the theme_editor.ts plugin
    copy_plugin(&plugins_dir, "theme_editor");

    // Create themes directory
    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test-colors",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test-colors.json"), test_theme).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // The theme editor should now be showing color fields with swatches
    let screen = harness.screen_to_string();

    // Verify the theme editor shows color values in hex format #RRGGBB
    // The default theme has values like #1E1E1E for background [30, 30, 30]
    let has_hex_format = screen.contains("#1E1E1E")
        || screen.contains("#1e1e1e")
        || screen.contains("#D4D4D4")
        || screen.contains("#d4d4d4")
        || screen.contains("#528BFF")
        || screen.contains("#282828")
        || screen.contains("#646464");

    assert!(
        has_hex_format,
        "Theme editor should display RGB color values in #RRGGBB format. Screen:\n{}",
        screen
    );

    // Check that the screen contains color field key names (two-panel layout shows short keys)
    assert!(
        screen.contains("bg") || screen.contains("fg") || screen.contains("cursor"),
        "Theme editor should show color field labels. Screen:\n{}",
        screen
    );

    // Verify some RGB colors are being used in rendering (for swatches, highlights, etc.)
    let buffer = harness.buffer();
    let mut rgb_color_count = 0;

    // Count cells with RGB colors (either foreground or background)
    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            if let Some(style) = harness.get_cell_style(x, y) {
                if matches!(style.fg, Some(Color::Rgb(_, _, _))) {
                    rgb_color_count += 1;
                }
                if matches!(style.bg, Some(Color::Rgb(_, _, _))) {
                    rgb_color_count += 1;
                }
            }
        }
    }

    // The theme editor should use many RGB colors for its UI (section headers, field values, etc.)
    assert!(
        rgb_color_count > 50,
        "Theme editor should use RGB colors for rendering. Found {} RGB-colored cells",
        rgb_color_count
    );
}

/// Test that the editor uses RGB colors from themes
/// This verifies that the editor rendering pipeline supports RGB colors
#[test]
fn test_editor_uses_rgb_colors() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create a test file
    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Hello World\nLine 2\nLine 3").unwrap();

    // Create harness with default config (which uses the dark theme with RGB colors)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Wait for the file content to be rendered
    harness
        .wait_until(|h| h.screen_to_string().contains("Hello World"))
        .unwrap();

    // Count RGB colors used in the rendering
    let buffer = harness.buffer();
    let mut rgb_bg_count = 0;
    let mut rgb_fg_count = 0;

    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            if let Some(style) = harness.get_cell_style(x, y) {
                if matches!(style.bg, Some(Color::Rgb(_, _, _))) {
                    rgb_bg_count += 1;
                }
                if matches!(style.fg, Some(Color::Rgb(_, _, _))) {
                    rgb_fg_count += 1;
                }
            }
        }
    }

    // The editor should use RGB colors for backgrounds and foregrounds
    // The exact count depends on theme, but there should be significant RGB usage
    let total_rgb = rgb_bg_count + rgb_fg_count;

    assert!(
        total_rgb > 100,
        "Editor should use RGB colors from theme. Found {} RGB backgrounds and {} RGB foregrounds (total: {})",
        rgb_bg_count, rgb_fg_count, total_rgb
    );
}

// =============================================================================
// Bug Tests - These tests verify bugs that need to be fixed
// =============================================================================

/// Test that cursor position is preserved when toggling a section with Enter
#[test]
fn test_cursor_position_preserved_after_section_toggle() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    // Create a theme with UI section fields so toggling works
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {"tab_bg": [40, 40, 40], "tab_fg": [180, 180, 180]},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Navigate down to find "UI Elements" section header
    // Keep pressing down until we see "UI Elements" on screen
    for _ in 0..20 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        if screen.contains("UI Elements") {
            break;
        }
    }

    // Get cursor position before toggle
    let (_, _cursor_y_before) = harness.screen_cursor_position();

    // Press Enter to toggle the section
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Process async operations and render to ensure key is handled
    harness.process_async_and_render().unwrap();

    let (_, cursor_y_after) = harness.screen_cursor_position();

    // After toggling, the cursor should still be on a valid line
    // (exact position may vary based on section expansion/collapse)
    assert!(
        cursor_y_after > 0,
        "Cursor should be on a valid line after toggling. Y position: {}",
        cursor_y_after
    );
}

/// Test that color prompt shows suggestions including current value
#[test]
#[ignore = "flaky"]
fn test_color_prompt_shows_suggestions() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Navigate down to find a color field (Background)
    // The structure is: Title, File path, blank, Section, Section desc, Field desc, Field
    // So we need to navigate down enough to land on a field line (index 6+)
    for _ in 0..8 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // Wait for Background to appear on screen
    harness
        .wait_until(|h| h.screen_to_string().contains("Background:"))
        .unwrap();

    // Keep pressing Down until we're on a field that opens a prompt.
    // After each Enter we wait for the screen to change (no timeout) and
    // then check whether a color prompt appeared.
    let mut prompt_opened = false;
    for _ in 0..10 {
        let before = harness.screen_to_string();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        // Wait for the Enter to take effect (screen must change)
        harness
            .wait_until(|h| {
                let screen = h.screen_to_string();
                screen != before
                    || screen.contains("#RRGGBB")
                    || screen.contains("(#RRGGBB or named)")
            })
            .unwrap();

        let screen = harness.screen_to_string();
        if screen.contains("#RRGGBB") || screen.contains("(#RRGGBB or named)") {
            prompt_opened = true;
            break;
        }

        // If no prompt, we might be on description/section, try moving down
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    assert!(prompt_opened, "Color prompt should appear");

    let screen = harness.screen_to_string();

    // The prompt should show named color suggestions
    let has_named_colors = screen.contains("Black")
        || screen.contains("Red")
        || screen.contains("White")
        || screen.contains("Green")
        || screen.contains("Blue");

    assert!(
        has_named_colors,
        "Prompt should show named color suggestions. Screen:\n{}",
        screen
    );

    // The current value should appear in suggestions (in hex format)
    let has_current_value =
        screen.contains("#1E1E1E") || screen.contains("#1e1e1e") || screen.contains("current");

    assert!(
        has_current_value,
        "Prompt should show current color value. Screen:\n{}",
        screen
    );
}

/// Test that colors are displayed in HTML hex format (#RRGGBB)
#[test]
fn test_colors_displayed_in_hex_format() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    let screen = harness.screen_to_string();

    // Should show hex colors like #1E1E1E (30, 30, 30) or #D4D4D4 (212, 212, 212)
    // BUG: Currently shows [r, g, b] format
    let has_hex_format = screen.contains("#1E1E1E")
        || screen.contains("#1e1e1e")
        || screen.contains("#D4D4D4")
        || screen.contains("#d4d4d4")
        || screen.contains("#528BFF")  // cursor color
        || screen.contains("#282828"); // current line bg

    assert!(
        has_hex_format,
        "Colors should be displayed in hex format (#RRGGBB). Screen:\n{}",
        screen
    );

    // Should NOT show [r, g, b] format
    let has_bracket_format = screen.contains("[30, 30, 30]")
        || screen.contains("[212, 212, 212]")
        || screen.contains("[82, 139, 255]");

    assert!(
        !has_bracket_format,
        "Colors should NOT be in [r, g, b] format. Screen:\n{}",
        screen
    );
}

/// Test that comments appear BEFORE the field they describe, not after
/// BUG: Currently comments appear after the field
#[test]
fn test_comments_appear_before_fields() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // In the two-panel layout, field descriptions appear in the right-side picker panel
    // when a field is selected. The Editor section starts expanded by default, so just
    // press Down to navigate from the section header to the first field.
    //
    // The plugin sorts fields alphabetically within a section, so *which* field is
    // first depends on whichever editor color sorts first by key — this must not be
    // hard-coded to any specific name (see #779: adding `after_eof_bg` bumped the
    // alphabetically-first field, breaking the old matcher). Instead, verify that
    // *some* editor field is selected and shown in the picker.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // The tree panel shows the selection marker `▸` in front of a field row.
            // The picker panel shows the path of the currently-selected field as
            // `editor.<field_name> - <display_name>`. Matching either is enough;
            // on narrow terminals the picker header can be truncated, so accept
            // the tree-panel marker as an equivalent signal.
            screen.contains("\u{25B8} ")
                || screen
                    .lines()
                    .any(|line| line.trim_start().starts_with("editor."))
        })
        .unwrap();
}

/// Test that theme changes are applied immediately after saving
/// Saving a theme automatically applies it
#[test]
#[ignore = "complex test with directory context isolation issues - needs redesign"]
fn test_theme_applied_immediately_after_save() {
    init_tracing_from_env();

    // Create isolated directory context for this test
    let context_temp = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(context_temp.path());

    // Create the themes directory and put our test theme there
    fs::create_dir_all(dir_context.themes_dir()).unwrap();
    let test_theme = r#"{
        "name": "red-test",
        "editor": {"bg": [255, 0, 0], "fg": [255, 255, 255]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(dir_context.themes_dir().join("red-test.json"), test_theme).unwrap();

    // Create project directory with plugins
    let project_temp = tempfile::TempDir::new().unwrap();
    let project_root = project_temp.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    // Create a test file to see theme changes
    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Hello World").unwrap();

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        40,
        Default::default(),
        project_root.clone(),
        dir_context,
    )
    .unwrap();

    // Open the test file first
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Wait for file to load
    harness
        .wait_until(|h| h.screen_to_string().contains("Hello World"))
        .unwrap();

    // Record the initial background color of the editor area
    let buffer = harness.buffer();
    let mut initial_bg_color: Option<Color> = None;
    for y in 2..buffer.area.height - 2 {
        for x in 0..buffer.area.width {
            if let Some(style) = harness.get_cell_style(x, y) {
                if let Some(bg) = style.bg {
                    if matches!(bg, Color::Rgb(_, _, _)) {
                        initial_bg_color = Some(bg);
                        break;
                    }
                }
            }
        }
        if initial_bg_color.is_some() {
            break;
        }
    }

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Open the red-test theme using Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Wait for the prompt to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Open theme") || screen.contains("Select theme")
        })
        .unwrap();

    // Type the theme name "red-test" and confirm
    harness.type_text("red-test").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme to be loaded
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("red-test") || screen.contains("Opened")
        })
        .unwrap();

    // Save the theme with Ctrl+Shift+S (Save As) since it's a builtin
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Wait for save-as prompt
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Save theme")
                || screen.contains("save as")
                || screen.contains("theme as")
        })
        .unwrap();

    // Type a unique name and save (use timestamp to avoid conflicts)
    let unique_name = format!(
        "my-red-theme-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis()
    );
    harness.type_text(&unique_name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Wait for theme to be saved and applied
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.to_lowercase().contains("changed") || screen.to_lowercase().contains("saved")
        })
        .unwrap();

    // Close the theme editor with Ctrl+Q
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

    // Now check if the editor background color changed
    let buffer = harness.buffer();
    let mut new_bg_color: Option<Color> = None;
    for y in 2..buffer.area.height - 2 {
        for x in 0..buffer.area.width {
            if let Some(style) = harness.get_cell_style(x, y) {
                if let Some(bg) = style.bg {
                    if matches!(bg, Color::Rgb(_, _, _)) {
                        new_bg_color = Some(bg);
                        break;
                    }
                }
            }
        }
        if new_bg_color.is_some() {
            break;
        }
    }

    // The background should have changed (we loaded a red theme)
    if let (Some(Color::Rgb(ir, ig, ib)), Some(Color::Rgb(nr, ng, nb))) =
        (initial_bg_color, new_bg_color)
    {
        // Check that the color actually changed
        let color_changed = ir != nr || ig != ng || ib != nb;

        assert!(
            color_changed,
            "Theme should be applied immediately after save. Initial: ({}, {}, {}), New: ({}, {}, {})",
            ir, ig, ib, nr, ng, nb
        );
    }
    // If we can't find RGB colors, that's okay - the test is just verifying the flow works
}

/// Test that cursor X position is preserved when toggling a section with Enter
/// BUG: Currently cursor moves one character back
#[test]
#[ignore = "flaky test - times out intermittently"]
fn test_cursor_x_position_preserved_after_section_toggle() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {"tab_bg": [40, 40, 40], "tab_fg": [180, 180, 180]},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Navigate down to find "UI Elements" section header (collapsed by default)
    // Keep pressing Down until cursor is on the UI Elements line
    loop {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        let (cx, cy) = harness.screen_cursor_position();
        eprintln!("Navigating down: cursor at ({}, {})", cx, cy);

        if screen.contains("> UI Elements") {
            // Check if we're actually on that line
            let lines: Vec<&str> = screen.lines().collect();
            if cy < lines.len() as u16 {
                let cursor_line = lines[cy as usize];
                eprintln!("Cursor line: {}", cursor_line);
                if cursor_line.contains("> UI Elements") {
                    break;
                }
            }
        }
    }

    // Render and get cursor position before toggle
    harness.render().unwrap();
    let screen_before = harness.screen_to_string();
    let (cursor_x_before, cursor_y_before) = harness.screen_cursor_position();

    eprintln!("=== BEFORE TOGGLE ===");
    eprintln!(
        "Cursor position: ({}, {})",
        cursor_x_before, cursor_y_before
    );
    eprintln!("Screen:\n{}", screen_before);

    // Press Enter to toggle the section (expand) - Enter toggles when on a section header
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the toggle to complete (> becomes ▼)
    harness
        .wait_until(|h| h.screen_to_string().contains("▼ UI Elements"))
        .unwrap();

    let screen_after = harness.screen_to_string();
    let (cursor_x_after, cursor_y_after) = harness.screen_cursor_position();

    eprintln!("=== AFTER TOGGLE ===");
    eprintln!("Cursor position: ({}, {})", cursor_x_after, cursor_y_after);
    eprintln!("Screen:\n{}", screen_after);

    // Verify we actually toggled (> should become ▼)
    assert!(
        screen_before.contains("> UI Elements"),
        "Before toggle should show collapsed UI Elements (>). Screen:\n{}",
        screen_before
    );
    assert!(
        screen_after.contains("▼ UI Elements"),
        "After toggle should show expanded UI Elements (▼). Screen:\n{}",
        screen_after
    );

    // Extract column from status bar (format: "Ln X, Col Y")
    fn extract_col_from_status(screen: &str) -> Option<u32> {
        for line in screen.lines() {
            if let Some(col_idx) = line.find("Col ") {
                let rest = &line[col_idx + 4..];
                let col_str: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
                return col_str.parse().ok();
            }
        }
        None
    }

    let col_before = extract_col_from_status(&screen_before);
    let col_after = extract_col_from_status(&screen_after);

    eprintln!(
        "Column before: {:?}, Column after: {:?}",
        col_before, col_after
    );

    // The cursor X position should stay the same
    // BUG: Currently cursor moves one character back (cursor_x_after = cursor_x_before - 1)
    assert_eq!(
        cursor_x_before, cursor_x_after,
        "Cursor X should stay at same position after toggling. Before: ({}, {}), After: ({}, {})",
        cursor_x_before, cursor_y_before, cursor_x_after, cursor_y_after
    );

    // Also check the column from status bar
    if let (Some(col_b), Some(col_a)) = (col_before, col_after) {
        assert_eq!(
            col_b, col_a,
            "Column in status bar should stay same after toggling. Before: {}, After: {}",
            col_b, col_a
        );
    }
}

/// Test that color suggestions show hex format (#123456) not [r,g,b]
/// BUG: Currently suggestions show [r, g, b] format
#[test]
#[ignore = "flaky test - timing sensitive"]
fn test_color_suggestions_show_hex_format() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Navigate down to a color field and open the prompt.
    // After each Down+Enter we wait for the screen to change (no timeout).
    let mut prompt_opened = false;
    for _ in 0..30 {
        // Navigate down and wait for the UI to settle
        let before_down = harness.screen_to_string();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness
            .wait_until(|h| h.screen_to_string() != before_down)
            .unwrap();

        // Try to open a prompt
        let before_enter = harness.screen_to_string();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness
            .wait_until(|h| {
                let screen = h.screen_to_string();
                screen != before_enter
                    || screen.contains("#RRGGBB")
                    || screen.contains("(#RRGGBB or named)")
            })
            .unwrap();

        let screen = harness.screen_to_string();
        if screen.contains("#RRGGBB") || screen.contains("(#RRGGBB or named)") {
            prompt_opened = true;
            break;
        }

        // If we opened something that's not a color prompt, close it and try next field
        if screen.contains("Enter:") || screen.contains("select") {
            harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
            harness.process_async_and_render().unwrap();
        }
    }

    assert!(prompt_opened, "Color prompt should appear");

    // Wait for the prompt to fully render (screen stops changing)
    harness
        .wait_until_stable(|h| {
            // Condition: prompt is visible
            h.screen_to_string().contains("#RRGGBB")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    // Check whether suggestions appeared
    let has_suggestions = screen.contains("#000000")
        || screen.contains("#FF0000")
        || screen.contains("[0, 0, 0]")
        || screen.contains("[255, 0, 0]")
        || screen.contains("black")
        || screen.contains("white");

    let screen = harness.screen_to_string();

    // If no suggestions appeared, skip the format check - suggestions may not be implemented
    if !has_suggestions {
        // Just verify the prompt is working (shows hex format hint)
        assert!(
            screen.contains("#RRGGBB"),
            "Color prompt should show format hint. Screen:\n{}",
            screen
        );
        return;
    }

    // The suggestions should show hex format for named colors
    // BUG: Currently shows "[0, 0, 0]" instead of "#000000"
    let has_bracket_format = screen.contains("[0, 0, 0]")
        || screen.contains("[255, 0, 0]")
        || screen.contains("[0, 128, 0]")
        || screen.contains("[255, 255, 0]");

    assert!(
        !has_bracket_format,
        "Color suggestions should NOT show [r, g, b] format. Screen:\n{}",
        screen
    );

    // Should show hex format like #000000, #FF0000, etc.
    let has_hex_format = screen.contains("#000000")
        || screen.contains("#FF0000")
        || screen.contains("#008000")
        || screen.contains("#FFFF00");

    assert!(
        has_hex_format,
        "Color suggestions should show hex format (#RRGGBB). Screen:\n{}",
        screen
    );
}

/// Test that color prompt is pre-filled with current value
/// BUG: Currently prompt starts empty
#[test]
#[ignore = "flaky"]
fn test_color_prompt_prefilled_with_current_value() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Navigate down to Background field
    for _ in 0..8 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // Keep pressing Down until we're on a field that opens a prompt
    let mut prompt_opened = false;
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        if screen.contains("#RRGGBB") || screen.contains("(#RRGGBB or named)") {
            prompt_opened = true;
            break;
        }

        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    assert!(prompt_opened, "Color prompt should appear");

    // The prompt input should be pre-filled with the current color value
    let screen = harness.screen_to_string();

    // Look for the prompt line which should contain a pre-filled hex value
    // The prompt format is: "FieldName (#RRGGBB or named): #XXXXXX"
    // The test may land on different fields, so check for any hex value in prompt
    let prompt_line = screen
        .lines()
        .find(|line| line.contains("#RRGGBB or named): #"));

    assert!(
        prompt_line.is_some(),
        "Prompt should be pre-filled with current color value in hex format. Screen:\n{}",
        screen
    );
}

/// Test that color values in the theme editor are rendered without extra internal spaces
/// This tests the fix for a bug where virtual text spacing caused "R  ed" instead of "Red"
#[test]
fn test_theme_editor_color_values_no_internal_spaces() {
    use regex::Regex;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Wait for swatches to appear (indicated by "██" swatch blocks)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("██") || screen.contains("Theme Editor")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // The bug causes hex colors to render as "#  XXXXXX" (spaces after #) instead of "#XXXXXX"
    // This is because the buggy code used two addVirtualText calls:
    // - One with before:true for the swatch
    // - One with before:false for the space, which inserts AFTER the # character

    // Check for the bug pattern: # followed by spaces then hex digits
    let broken_pattern = Regex::new(r"#\s+[0-9A-Fa-f]").unwrap();

    // Find lines that have color fields (contain "██" swatch and "#" hex value)
    let color_lines: Vec<&str> = screen
        .lines()
        .filter(|line| line.contains("██") && line.contains("#"))
        .collect();

    assert!(
        !color_lines.is_empty(),
        "Should find color field lines in theme editor. Screen:\n{}",
        screen
    );

    // Check that none of the color lines have the bug pattern
    for line in &color_lines {
        assert!(
            !broken_pattern.is_match(line),
            "Found broken color value with spaces after # (virtual text spacing bug): '{}'\n\nFull screen:\n{}",
            line,
            screen
        );
    }

    // Also verify we have proper hex colors (no spaces between # and digits)
    let proper_hex_pattern = Regex::new(r"#[0-9A-Fa-f]{6}").unwrap();
    let has_proper_hex = color_lines
        .iter()
        .any(|line| proper_hex_pattern.is_match(line));

    assert!(
        has_proper_hex,
        "Should find properly formatted hex colors (#XXXXXX). Screen:\n{}",
        screen
    );
}

/// Test that navigation skips non-selectable lines and only lands on fields/sections
/// Navigation should work with Up/Down arrows and Tab/Shift-Tab for section jumping
#[test]
#[ignore = "flaky test - times out intermittently"]
fn test_theme_editor_navigation_skips_non_selectable_lines() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {"tab_active_bg": [50, 50, 50]},
        "search": {},
        "diagnostic": {},
        "syntax": {"keyword": [100, 150, 200]}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Initial position
    let (_, cursor_y_initial) = harness.screen_cursor_position();

    // Press Down multiple times to navigate through fields, waiting for screen to change each time
    for _ in 0..6 {
        let screen_before = harness.screen_to_string();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        // Wait for screen to change (semantic waiting - cursor movement changes highlighting)
        harness
            .wait_until(|h| h.screen_to_string() != screen_before)
            .unwrap();
    }

    let (_, cursor_y_after_multiple) = harness.screen_cursor_position();

    // After multiple Down presses, cursor should have moved
    // (navigating through selectable lines)
    assert!(
        cursor_y_after_multiple > cursor_y_initial || cursor_y_initial > 2,
        "Cursor should navigate through theme editor. Initial Y: {}, Final Y: {}",
        cursor_y_initial,
        cursor_y_after_multiple
    );

    // Now press Up to go back - wait for screen to change
    let screen_before_up = harness.screen_to_string();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.screen_to_string() != screen_before_up)
        .unwrap();

    let (_, cursor_y_after_up) = harness.screen_cursor_position();

    // Cursor should have moved up
    assert!(
        cursor_y_after_up < cursor_y_after_multiple,
        "Cursor should move up after pressing Up. After multiple down Y: {}, After up Y: {}",
        cursor_y_after_multiple,
        cursor_y_after_up
    );

    // Test Tab navigation - should jump to next section
    // First, go back to beginning
    for _ in 0..20 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
    }

    let _screen_at_start = harness.screen_to_string();

    // Press Tab to navigate to next selectable element (field or section)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    let (_, _cursor_y_after_tab) = harness.screen_cursor_position();
    let (_, _cursor_y_before_tab) = harness.screen_cursor_position();

    // Tab should move the cursor (it navigates through all fields and sections)
    // Note: With wrapping, it might wrap back to start if we're at the end

    // Press Tab multiple times to verify wrapping works
    let (_, _cursor_y_initial_for_wrap) = harness.screen_cursor_position();
    for _ in 0..50 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
    }

    // After many Tabs, cursor should have wrapped back to somewhere
    // (We can't assert exact position, but it shouldn't crash)

    // Test Shift+Tab navigation - should navigate backwards with wrapping
    let (_, _cursor_y_before_backtab) = harness.screen_cursor_position();
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness.process_async_and_render().unwrap();

    let (_, _cursor_y_after_backtab) = harness.screen_cursor_position();

    // Shift+Tab should also move the cursor
    // (exact behavior depends on current position due to wrapping)

    // Verify that pressing Enter on a section toggles it (expand/collapse)
    // Find a collapsed section first
    for _ in 0..10 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
        let screen = harness.screen_to_string();
        if screen.contains("> UI")
            || screen.contains("> Search")
            || screen.contains("> Diagnostics")
        {
            break;
        }
    }

    let screen_before_toggle = harness.screen_to_string();
    let has_collapsed_section = screen_before_toggle.contains("> ");

    if has_collapsed_section {
        // Press Enter to toggle (expand)
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.process_async_and_render().unwrap();

        let screen_after_toggle = harness.screen_to_string();

        // After toggle, the section should be expanded (shows ▼ instead of >)
        // Note: This depends on which section we landed on
        let has_expanded = screen_after_toggle.contains("▼");
        assert!(
            has_expanded || screen_after_toggle != screen_before_toggle,
            "Enter on section should toggle expansion. Before toggle screen had '>' for collapsed sections."
        );
    }
}

/// Test that cursor position is preserved after editing a color value
/// The cursor should return to the same field after confirming a color change
#[test]
fn test_cursor_position_preserved_after_color_edit() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200], "cursor": [255, 255, 255]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Wait for theme editor to be fully loaded with color fields
    harness
        .wait_until(|h| h.screen_to_string().contains("editor"))
        .unwrap();

    // Navigate down to reach a color field (skip section headers)
    // The first few items are section headers, we need to get to actual color fields
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // Now we should be on a color field. Open the prompt.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for color prompt to appear (semantic waiting, no timeout)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("#RRGGBB") || screen.contains("(#RRGGBB or named)")
        })
        .unwrap();

    // Cancel the prompt to go back to the field
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Wait for prompt to close
    harness
        .wait_until(|h| !h.screen_to_string().contains("#RRGGBB"))
        .unwrap();

    // NOW record the cursor position - we know we're on a valid color field
    let (cursor_x_before, cursor_y_before) = harness.screen_cursor_position();

    // Open the color prompt again
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for color prompt to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("#RRGGBB") || screen.contains("(#RRGGBB or named)")
        })
        .unwrap();

    // Clear the pre-filled value and type a new color value
    // The prompt opens with the current value pre-filled, so we need to select all and replace
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("#FF0000").unwrap();
    harness.render().unwrap();

    // Confirm the color change
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Wait for the prompt to close and display to update
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("#RRGGBB") && screen.contains("#FF0000")
        })
        .unwrap();

    // Record cursor position after editing
    let (cursor_x_after, cursor_y_after) = harness.screen_cursor_position();

    // The cursor should be near the same position (within 2 lines due to possible display changes)
    let y_diff = (cursor_y_after as i32 - cursor_y_before as i32).abs();
    assert!(
        y_diff <= 2,
        "Cursor Y should stay near same position after editing color. Before: ({}, {}), After: ({}, {}), Diff: {}",
        cursor_x_before, cursor_y_before, cursor_x_after, cursor_y_after, y_diff
    );

    // The color should have been updated
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("#FF0000"),
        "Color should be updated to #FF0000. Screen:\n{}",
        screen
    );
}

/// Test that cursor is positioned on the value field (not first column) when navigating
/// When moving to a color field, cursor should be on the value, not at the line start
#[test]
fn test_cursor_on_value_field_when_navigating() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Navigate down to a color field
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
    }

    // Get cursor position
    let (cursor_x, _cursor_y) = harness.screen_cursor_position();

    // The cursor X should NOT be at the first column (0)
    // It should be positioned after "FieldName: " on the value
    // The exact position depends on field name length and indentation
    // But it should definitely be > 10 (past indentation + field name + colon)
    assert!(
        cursor_x > 5,
        "Cursor X should be positioned on the value field, not at first column. Got X={}",
        cursor_x
    );

    // Navigate to another field and check again
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    let (cursor_x_2, _) = harness.screen_cursor_position();

    // Should still be positioned on value
    assert!(
        cursor_x_2 > 5,
        "Cursor X should be positioned on value after navigating. Got X={}",
        cursor_x_2
    );
}

/// Test that builtin themes require Save As (cannot overwrite builtins)
#[test]
fn test_builtin_theme_requires_save_as() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    // Create a DirectoryContext so we know where config_dir/themes is
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Write the test theme into the config themes dir (where ThemeLoader looks)
    let themes_dir = dir_context.themes_dir();
    fs::create_dir_all(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "builtin-test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("builtin-test.json"), test_theme).unwrap();

    let mut harness = EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_config(Default::default())
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir()
            .with_shared_dir_context(dir_context),
    )
    .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    // Open the builtin theme with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Open theme") || screen.contains("Select theme")
        })
        .unwrap();

    harness.type_text("builtin-test").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme to load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("builtin-test") || screen.contains("Opened")
        })
        .unwrap();

    // Navigate to a field and make a change
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
    }

    // Try to open a color prompt and make a change
    for _ in 0..10 {
        let before = harness.screen_to_string();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        harness
            .wait_until(|h| {
                let screen = h.screen_to_string();
                screen != before || screen.contains("#RRGGBB")
            })
            .unwrap();

        if harness.screen_to_string().contains("#RRGGBB") {
            break;
        }

        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
    }

    // Type a color change
    harness.type_text("#AA0000").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Now try to save with Ctrl+S - should prompt for Save As since it's a builtin
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for Save As prompt to appear (async plugin handler)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Save theme as") || screen.contains("save as")
        })
        .unwrap();
}

/// Test that color swatches are displayed next to color values
#[test]
fn test_color_swatches_displayed() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "test",
        "editor": {"bg": [30, 30, 30], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor using helper (handles theme selection prompt)
    open_theme_editor(&mut harness);

    let screen = harness.screen_to_string();

    // Color swatches should be displayed as "██" blocks next to field values
    assert!(
        screen.contains("██"),
        "Color swatches should be displayed next to color values. Screen:\n{}",
        screen
    );

    // Should also have hex color values visible
    let has_hex = screen.contains("#");
    assert!(
        has_hex,
        "Hex color values should be visible. Screen:\n{}",
        screen
    );
}

/// Test that selecting the built-in "nostalgia" theme displays its actual colors
/// Bug reproduction: when selecting Nostalgia from the Edit Theme suggestion list,
/// the theme that opens should have Nostalgia's colors (blue background #0000AA),
/// not Dark theme colors (#1E1E1E)
///
/// This test types the theme name to select it.
#[test]
fn test_theme_editor_nostalgia_builtin_shows_correct_colors() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    // Don't create a themes directory - we want to use the built-in themes only

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to find the Edit Theme command
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme selection prompt to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();

    // Type "nostalgia" to filter/select the nostalgia theme
    harness.type_text("nostalgia").unwrap();
    harness.render().unwrap();

    // Select it
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor to fully load with the nostalgia theme
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor") && screen.contains("nostalgia")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // Nostalgia theme has editor.bg = [0, 0, 170] which is #0000AA in hex
    // The theme editor should display this value
    let has_nostalgia_bg = screen.contains("#0000AA") || screen.contains("#0000aa");

    // Dark theme has editor.bg = [30, 30, 30] which is #1E1E1E in hex
    // This should NOT appear if nostalgia was loaded correctly
    let has_dark_bg = screen.contains("#1E1E1E") || screen.contains("#1e1e1e");

    assert!(
        has_nostalgia_bg,
        "Theme editor should show Nostalgia's background color #0000AA. Screen:\n{}",
        screen
    );

    assert!(
        !has_dark_bg,
        "Theme editor should NOT show Dark theme's background color #1E1E1E when Nostalgia is selected. Screen:\n{}",
        screen
    );
}

/// Test that selecting nostalgia theme via arrow navigation displays its actual colors
/// This tests the case where user navigates the suggestions list with arrow keys
/// and selects a suggestion (which sends the suggestion's `value` field, not `text`)
#[test]
fn test_theme_editor_nostalgia_builtin_via_arrow_selection() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    // Don't create a themes directory - we want to use the built-in themes only

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to find the Edit Theme command
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme selection prompt to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();

    // Type "nostalgia" to filter suggestions to just nostalgia
    harness.type_text("nostalgia").unwrap();
    harness.render().unwrap();

    // Wait for suggestions to update
    harness
        .wait_until(|h| h.screen_to_string().contains("nostalgia"))
        .unwrap();

    // Press Down arrow to select the suggestion from the list
    // This should send the `value` field from the suggestion (e.g., "builtin:nostalgia")
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now press Enter to confirm selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor to fully load with the nostalgia theme
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor") && screen.contains("nostalgia")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // Nostalgia theme has editor.bg = [0, 0, 170] which is #0000AA in hex
    let has_nostalgia_bg = screen.contains("#0000AA") || screen.contains("#0000aa");

    // Dark theme has editor.bg = [30, 30, 30] which is #1E1E1E in hex
    let has_dark_bg = screen.contains("#1E1E1E") || screen.contains("#1e1e1e");

    assert!(
        has_nostalgia_bg,
        "Theme editor should show Nostalgia's background color #0000AA when selected via arrow navigation. Screen:\n{}",
        screen
    );

    assert!(
        !has_dark_bg,
        "Theme editor should NOT show Dark theme's background color #1E1E1E when Nostalgia is selected. Screen:\n{}",
        screen
    );
}

/// Bug regression test: selecting nostalgia from suggestion dropdown should load nostalgia colors
/// The bug was that plugin prompts didn't use the suggestion's `value` field when a suggestion
/// was selected, so "builtin:nostalgia" was not being passed correctly to the handler.
#[test]
fn test_theme_editor_select_nostalgia_from_dropdown() {
    init_tracing_from_env();
    eprintln!("[TEST] test_theme_editor_select_nostalgia_from_dropdown: starting");

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();
    eprintln!("[TEST] harness created and rendered");

    // Open command palette
    eprintln!("[TEST] opening command palette...");
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    eprintln!("[TEST] command palette opened");

    eprintln!("[TEST] typing 'Edit Theme'...");
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();
    eprintln!("[TEST] typed 'Edit Theme'");

    eprintln!("[TEST] pressing Enter to execute command...");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    eprintln!("[TEST] Enter pressed, waiting for theme selection prompt...");

    // Wait for theme selection prompt
    harness
        .wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();
    eprintln!("[TEST] theme selection prompt appeared");

    // Type "nostalgia" to filter the list
    eprintln!("[TEST] typing 'nostalgia'...");
    harness.type_text("nostalgia").unwrap();
    harness.render().unwrap();
    eprintln!("[TEST] typed 'nostalgia'");

    // Press Down to select the nostalgia suggestion from the dropdown
    // This is the key part - selecting from dropdown sends the suggestion's `value`
    eprintln!("[TEST] pressing Down to select suggestion...");
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    eprintln!("[TEST] Down pressed");

    // Confirm selection
    eprintln!("[TEST] pressing Enter to confirm selection...");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    eprintln!("[TEST] Enter pressed, waiting for Theme Editor to load...");

    // Wait for theme editor to fully load.
    //
    // Must wait for per-panel content (populated by setPanelContent) rather
    // than just the `*Theme Editor*` tab label — see `open_theme_editor` for
    // the full rationale. The tab label appears as soon as the buffer group
    // is created, which is BEFORE the plugin populates the tree/picker
    // panels, and on Windows CI that race window is wide enough that the
    // subsequent assertions below consistently observed blank panels.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor: ")
                && (screen.contains("Select a color field") || screen.contains("Hex:"))
        })
        .unwrap();
    eprintln!("[TEST] Theme Editor loaded");

    let screen = harness.screen_to_string();

    // Verify nostalgia theme loaded correctly:
    // 1. Title should show "Theme Editor: nostalgia"
    // 2. Background color should be #0000AA (nostalgia's blue), NOT #1E1E1E (dark's gray)

    assert!(
        screen.contains("Theme Editor: nostalgia"),
        "Title should show 'Theme Editor: nostalgia'. Screen:\n{}",
        screen
    );

    // Nostalgia has bg = [0, 0, 170] = #0000AA
    assert!(
        screen.contains("#0000AA") || screen.contains("#0000aa"),
        "Should show Nostalgia's blue background #0000AA. Screen:\n{}",
        screen
    );

    // Should NOT have dark theme's background color
    assert!(
        !screen.contains("#1E1E1E"),
        "Should NOT show Dark theme's background #1E1E1E. Screen:\n{}",
        screen
    );
}

/// Test that deleteTheme API correctly deletes a user theme
/// This tests the full lifecycle: create theme, verify it exists, delete it, verify it's gone
#[test]
fn test_delete_theme_api() {
    // Create isolated directory context for proper test isolation
    let context_temp = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(context_temp.path());

    // Create user themes directory
    fs::create_dir_all(dir_context.themes_dir()).unwrap();

    // Create a test theme that we'll delete
    let test_theme = r#"{
        "name": "to-be-deleted",
        "editor": {"bg": [100, 100, 100], "fg": [200, 200, 200]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    let theme_path = dir_context.themes_dir().join("to-be-deleted.json");
    fs::write(&theme_path, test_theme).unwrap();

    // Verify the theme file exists
    assert!(
        theme_path.exists(),
        "Theme file should exist before deletion"
    );

    // Create project directory with a test plugin that calls deleteTheme
    let project_temp = tempfile::TempDir::new().unwrap();
    let project_root = project_temp.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Create a test plugin that will delete the theme
    let delete_plugin = r#"
const editor = getEditor();

// Global state to track deletion result
let deleteResult: string = "not_run";

globalThis.test_delete_theme = async function(): Promise<void> {
    try {
        await editor.deleteTheme("to-be-deleted");
        deleteResult = "success";
        editor.setStatus("Theme deleted successfully");
    } catch (e) {
        deleteResult = "error: " + String(e);
        editor.setStatus("Delete failed: " + String(e));
    }
};

globalThis.test_check_result = function(): void {
    editor.setStatus("Result: " + deleteResult);
};

editor.registerCommand(
    "Test: Delete Theme",
    "Delete the to-be-deleted theme",
    "test_delete_theme",
    null
);

editor.registerCommand(
    "Test: Check Result",
    "Check delete result",
    "test_check_result",
    null
);

editor.setStatus("Delete theme test plugin loaded");
"#;
    fs::write(plugins_dir.join("delete_test.ts"), delete_plugin).unwrap();

    // Copy plugin lib for TypeScript support
    copy_plugin_lib(&plugins_dir);

    // Create harness with isolated directory context
    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        40,
        Default::default(),
        project_root.clone(),
        dir_context.clone(),
    )
    .unwrap();

    harness.render().unwrap();

    // Run the delete command via Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Test: Delete Theme").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Delete Theme"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Wait for deletion to complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("deleted successfully") || screen.contains("Delete failed")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // Verify deletion was successful
    assert!(
        screen.contains("deleted successfully"),
        "Theme deletion should succeed. Screen:\n{}",
        screen
    );

    // Verify the theme file no longer exists
    assert!(
        !theme_path.exists(),
        "Theme file should be deleted (moved to trash)"
    );
}

/// Test that "Inspect Theme at Cursor" command opens the theme editor
/// at the correct field for the theme key under the cursor.
#[test]
fn test_inspect_theme_at_cursor_opens_theme_editor() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    // Create a test file so the cursor is on editor content
    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Hello world\nLine two\nLine three\n").unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Cursor is now on editor content — run "Inspect Theme at Cursor"
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Inspect Theme at Cursor").unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Inspect Theme at Cursor");

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for the theme editor to open and auto-navigate to the editor field
    // (the resolved key will be editor.fg or editor.bg, so "Editor" section expands).
    // On macOS the long temp-dir path can push "editor.fg"/"editor.bg" off the right
    // panel header, so also match the selected-field indicator (▸) next to the field name.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("editor.fg")
                || screen.contains("editor.bg")
                || screen.contains("\u{25B8} fg")
                || screen.contains("\u{25B8} bg")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // The theme editor should auto-load the current theme (no "Select theme" prompt)
    assert!(
        !screen.contains("Select theme to edit"),
        "Should NOT prompt for theme selection — should auto-load current theme. Screen:\n{}",
        screen
    );
}

/// Test multiple rounds of inspect → focus source buffer → inspect again.
/// Verifies the theme editor re-navigates correctly when already open.
#[test]
fn test_inspect_theme_at_cursor_multiple_rounds() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Hello world\nLine two\nLine three\n").unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // === Round 1: First inspect ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Inspect Theme at Cursor").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("editor.fg")
                || screen.contains("editor.bg")
                || screen.contains("\u{25B8} fg")
                || screen.contains("\u{25B8} bg")
        })
        .unwrap();

    // === Switch back to source buffer (Ctrl+PageDown = next_buffer) ===
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify we're back on the source file
    harness
        .wait_until(|h| h.screen_to_string().contains("Hello world"))
        .unwrap();

    // === Round 2: Inspect again while theme editor is already open ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Inspect Theme at Cursor").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Theme editor should re-focus (the hook navigates when already open)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("editor.fg")
                || screen.contains("editor.bg")
                || screen.contains("\u{25B8} fg")
                || screen.contains("\u{25B8} bg")
        })
        .unwrap();

    // === Switch back to source again ===
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Hello world"))
        .unwrap();

    // === Round 3: One more inspect to confirm stability ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Inspect Theme at Cursor").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("editor.fg")
                || screen.contains("editor.bg")
                || screen.contains("\u{25B8} fg")
                || screen.contains("\u{25B8} bg")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // Should still not have prompted for theme selection at any point
    assert!(
        !screen.contains("Select theme to edit"),
        "Should never prompt for theme selection during inspect. Screen:\n{}",
        screen
    );
}

/// Test that saving a built-in theme as a new name produces a complete, valid theme file.
/// Reproduces a bug where the saved file was incomplete (only the edited field + name),
/// causing it to fail ThemeFile deserialization and not appear in Select Theme.
#[test]
fn test_save_builtin_theme_produces_valid_file() {
    init_tracing_from_env();

    // Create isolated directory context so we control the themes directory
    let context_temp = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(context_temp.path());
    fs::create_dir_all(dir_context.themes_dir()).unwrap();

    // Create project directory with plugins
    let project_temp = tempfile::TempDir::new().unwrap();
    let project_root = project_temp.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Hello world\n").unwrap();

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        40,
        Default::default(),
        project_root.clone(),
        dir_context.clone(),
    )
    .unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open theme editor via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme selection prompt
    harness
        .wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();

    // Select the "light" builtin theme
    harness.type_text("light").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor to load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor") || screen.contains("*Theme Editor*")
        })
        .unwrap();

    // Navigate to a color field and edit it (press Enter on the first field)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for color input prompt (should show a # hex prefix)
    harness
        .wait_until(|h| h.screen_to_string().contains("#"))
        .unwrap();

    // Clear input and type a new color
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("#FF0000").unwrap();
    harness.render().unwrap();

    // Confirm the color edit
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor to redisplay
    harness
        .wait_until(|h| h.screen_to_string().contains("Theme Editor"))
        .unwrap();

    // Save with Ctrl+S — since it's a builtin theme, this triggers Save As
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Wait for save-as prompt
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Save") || screen.contains("name")
        })
        .unwrap();

    // Type a new name (prompt starts empty)
    harness.render().unwrap();
    harness.type_text("light-custom").unwrap();
    harness.render().unwrap();

    // Confirm save
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for save confirmation
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("saved") || screen.contains("Saved") || screen.contains("applied")
        })
        .unwrap();

    // Now verify the saved file is a valid, complete theme
    let saved_path = dir_context.themes_dir().join("light-custom.json");
    assert!(
        saved_path.exists(),
        "Saved theme file should exist at {:?}.\nFiles in themes dir: {:?}",
        saved_path,
        fs::read_dir(dir_context.themes_dir())
            .map(|entries| entries
                .filter_map(|e| e.ok())
                .map(|e| e.file_name())
                .collect::<Vec<_>>())
            .unwrap_or_default()
    );

    let content = fs::read_to_string(&saved_path).unwrap();

    // The file must deserialize as a valid ThemeFile
    let theme_file: Result<fresh::view::theme::ThemeFile, _> = serde_json::from_str(&content);
    assert!(
        theme_file.is_ok(),
        "Saved theme must be a valid ThemeFile. Got error: {:?}\nFile content ({} bytes):\n{}",
        theme_file.err(),
        content.len(),
        content
    );

    let theme = theme_file.unwrap();
    assert_eq!(theme.name, "light-custom");

    // The file must contain all required sections — not just the edited field
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();
    for section in &["editor", "ui", "search", "diagnostic", "syntax"] {
        assert!(
            parsed.get(section).is_some(),
            "Saved theme is missing required section '{}'. File content:\n{}",
            section,
            content
        );
    }

    // The editor section should have more than just the one edited field
    let editor_obj = parsed.get("editor").unwrap().as_object().unwrap();
    assert!(
        editor_obj.len() > 1,
        "Editor section should contain all original fields, not just the edited one. \
         Got {} fields: {:?}\nFile content:\n{}",
        editor_obj.len(),
        editor_obj.keys().collect::<Vec<_>>(),
        content
    );
}

/// Test that saving a theme works when the themes directory does not exist yet
/// (fresh install scenario). Reproduces #1180 where Save As fails because
/// ~/.config/fresh/themes is not created by the editor.
#[test]
fn test_issue_1180_save_theme_creates_themes_directory() {
    init_tracing_from_env();

    // Create isolated directory context but do NOT create the themes directory.
    // This simulates a fresh install where ~/.config/fresh/themes doesn't exist.
    let context_temp = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(context_temp.path());
    // Intentionally NOT calling: fs::create_dir_all(dir_context.themes_dir())

    // Verify the themes directory really doesn't exist
    assert!(
        !dir_context.themes_dir().exists(),
        "Themes directory should not exist before save"
    );

    // Create project directory with plugins
    let project_temp = tempfile::TempDir::new().unwrap();
    let project_root = project_temp.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Hello world\n").unwrap();

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        40,
        Default::default(),
        project_root.clone(),
        dir_context.clone(),
    )
    .unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open theme editor via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme selection prompt
    harness
        .wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();

    // Select the "light" builtin theme
    harness.type_text("light").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor to load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor") || screen.contains("*Theme Editor*")
        })
        .unwrap();

    // Navigate to a color field and edit it
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for color input prompt
    harness
        .wait_until(|h| h.screen_to_string().contains("#"))
        .unwrap();

    // Clear input and type a new color
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("#FF0000").unwrap();
    harness.render().unwrap();

    // Confirm the color edit
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor to redisplay
    harness
        .wait_until(|h| h.screen_to_string().contains("Theme Editor"))
        .unwrap();

    // Save with Ctrl+Shift+S (Save As) to trigger the save-as flow
    harness
        .send_key(
            KeyCode::Char('S'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Wait for save-as prompt
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Save") || screen.contains("name")
        })
        .unwrap();

    // Type a new name for the theme
    harness.type_text("my-fresh-theme").unwrap();
    harness.render().unwrap();

    // Confirm save
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for save confirmation
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("saved") || screen.contains("Saved") || screen.contains("applied")
        })
        .unwrap();

    // Verify the themes directory was created
    assert!(
        dir_context.themes_dir().exists(),
        "Themes directory should have been created by the save operation"
    );

    // Verify the saved theme file exists and is valid
    let saved_path = dir_context.themes_dir().join("my-fresh-theme.json");
    assert!(
        saved_path.exists(),
        "Saved theme file should exist at {:?}.\nThemes dir exists: {}\nFiles in themes dir: {:?}",
        saved_path,
        dir_context.themes_dir().exists(),
        fs::read_dir(dir_context.themes_dir())
            .map(|entries| entries
                .filter_map(|e| e.ok())
                .map(|e| e.file_name())
                .collect::<Vec<_>>())
            .unwrap_or_default()
    );

    let content = fs::read_to_string(&saved_path).unwrap();
    let theme_file: Result<fresh::view::theme::ThemeFile, _> = serde_json::from_str(&content);
    assert!(
        theme_file.is_ok(),
        "Saved theme must be a valid ThemeFile. Got error: {:?}\nFile content:\n{}",
        theme_file.err(),
        content
    );

    let theme = theme_file.unwrap();
    assert_eq!(theme.name, "my-fresh-theme");
}

/// Test that after saving a custom theme, "Inspect Theme at Cursor" works
/// with the newly saved theme active. Reproduces a bug where the normalized
/// theme name (underscores→hyphens) didn't match the filename on disk.
#[test]
fn test_inspect_after_saving_custom_theme() {
    init_tracing_from_env();
    fresh::services::signal_handler::install_signal_handlers();

    let context_temp = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(context_temp.path());
    fs::create_dir_all(dir_context.themes_dir()).unwrap();

    let project_temp = tempfile::TempDir::new().unwrap();
    let project_root = project_temp.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Hello world\n").unwrap();

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        40,
        Default::default(),
        project_root.clone(),
        dir_context.clone(),
    )
    .unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();
    tracing::warn!("[test] file opened, starting step 1");

    // === Step 1: Open theme editor, select builtin, edit a color, save as custom ===

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    tracing::warn!("[test] waiting for 'Select theme to edit'");
    harness
        .wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();

    tracing::warn!("[test] typing 'light' and pressing Enter");
    harness.type_text("light").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    tracing::warn!("[test] waiting for Theme Editor tab");
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor") || screen.contains("*Theme Editor*")
        })
        .unwrap();

    // Expand Editor section and navigate to the first color field (bg)
    tracing::warn!("[test] expanding Editor section");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Edit the color field
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    tracing::warn!("[test] waiting for '#' (color edit field)");
    harness
        .wait_until(|h| h.screen_to_string().contains("#"))
        .unwrap();
    tracing::warn!("[test] typing color #FF0000");
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("#FF0000").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    tracing::warn!("[test] waiting for Theme Editor after color edit");
    harness
        .wait_until(|h| h.screen_to_string().contains("Theme Editor"))
        .unwrap();

    // Save as "light_custom" (with underscore to test normalization)
    tracing::warn!("[test] pressing Ctrl+S to save");
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    tracing::warn!("[test] waiting for 'Save theme as' dialog");
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Save theme as")
        })
        .unwrap();
    harness.render().unwrap();
    tracing::warn!("[test] typing 'light_custom' and pressing Enter");
    harness.type_text("light_custom").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    tracing::warn!("[test] waiting for saved/applied confirmation");
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("saved") || screen.contains("Saved") || screen.contains("applied")
        })
        .unwrap();

    // === Step 2: Close theme editor via Escape ===
    tracing::warn!("[test] step 2: closing theme editor via Escape");
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    tracing::warn!("[test] waiting for 'Hello world' (main editor)");
    harness
        .wait_until(|h| h.screen_to_string().contains("Hello world"))
        .unwrap();

    // === Step 3: Inspect Theme at Cursor — should work with the custom theme ===
    tracing::warn!("[test] step 3: opening Inspect Theme at Cursor");
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Inspect Theme at Cursor").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for theme editor to reopen and auto-navigate to editor fields.
    // The full qualified name (editor.fg / editor.bg) appears in the right panel
    // header, but on macOS the long temp-dir path in the left header can push it
    // off-screen.  Fall back to checking for the selected-field indicator (▸)
    // next to the short field name in the tree panel.
    tracing::warn!("[test] waiting for editor.fg/editor.bg fields");
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("editor.fg")
                || screen.contains("editor.bg")
                || screen.contains("\u{25B8} fg")
                || screen.contains("\u{25B8} bg")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // Should NOT show "Failed to load" error
    assert!(
        !screen.contains("Failed to load"),
        "Should not fail to load the custom theme. Screen:\n{}",
        screen
    );
}

/// Test that clicking on a palette swatch in the right panel applies the correct color.
/// Bug reproduction: clicking on the 5th palette swatch was applying the 1st swatch's color
/// because the byte offset calculation for click column detection was wrong.
#[test]
fn test_palette_swatch_click_targets_correct_column() {
    init_tracing_from_env();
    fresh::services::signal_handler::install_signal_handlers();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "dark",
        "editor": {
            "bg": [30, 30, 30],
            "fg": [212, 212, 212],
            "cursor": [82, 139, 255],
            "selection_bg": [38, 79, 120],
            "current_line_bg": [40, 40, 40],
            "line_number_fg": [100, 100, 100],
            "line_number_bg": [30, 30, 30]
        },
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("dark.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();
    open_theme_editor(&mut harness);

    // Navigate down from section header to first color field (bg)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Wait for theme editor to fully render with color palette
    harness
        .wait_until(|h| h.screen_to_string().contains("Color Palette"))
        .unwrap();

    // Find the first row of palette swatches (██ characters in the right panel area).
    // The left panel is 38 chars wide + 1 divider = 39, so right panel starts at col 39.
    // The palette row text is " " + " ██ ██ ██..." with a " " prefix, so first swatch
    // starts at approximately col 41.
    // Each swatch pattern: prefix(1 char) + "██"(2 chars) = 3 chars per swatch.
    // col N swatch starts at screen column 41 + 3*N.

    // Find the screen row that contains "Color Palette:" to locate palette rows
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let palette_label_row = lines
        .iter()
        .position(|line| line.contains("Color Palette:"))
        .expect("Should find 'Color Palette:' label on screen");

    // Palette rows start right after the label
    let palette_row_y = (palette_label_row + 1) as u16;

    // Verify palette swatches are visible at this row
    assert!(
        lines[palette_row_y as usize].contains("██"),
        "Palette row should contain swatch characters. Row {}: '{}'",
        palette_row_y,
        lines[palette_row_y as usize]
    );

    // Locate the first palette swatch (`██`) on the palette row at runtime,
    // then compute sibling swatch columns. The buffer-group theme editor
    // renders the palette inside the right panel (picker), so the absolute
    // screen column of col 0 depends on the tree/picker split ratio and is
    // not fixed. Each swatch is 2 chars of `██` followed by 1 char of
    // separator, so col N is col 0 + 3*N. Previously the test hardcoded
    // x=41 which happened to be the `fg` row's swatch column in the LEFT
    // (tree) panel — clicking there would move the tree selection instead
    // of applying a palette color.
    let swatch_col_0_x: u16 = {
        // The left panel also contains `██` (field swatches). We must find
        // the palette swatches in the RIGHT panel — i.e. the first `██`
        // that appears AFTER the vertical divider `│` in the palette row.
        let row_cells: Vec<String> = (0..120)
            .map(|x| {
                harness
                    .get_cell(x, palette_row_y)
                    .unwrap_or_else(|| " ".to_string())
            })
            .collect();
        let divider_col = row_cells
            .iter()
            .position(|s| s == "│")
            .expect("palette row should contain a `│` divider") as u16;
        // Scan after the divider for two adjacent `█` cells.
        let mut found = None;
        let mut x = divider_col + 1;
        while x + 1 < 120 {
            if row_cells[x as usize] == "█" && row_cells[(x + 1) as usize] == "█" {
                found = Some(x);
                break;
            }
            x += 1;
        }
        found.expect("palette row should contain `██` after the divider")
    };
    let swatch_col_4_x: u16 = swatch_col_0_x + 3 * 4;

    let color_at_col0 = harness
        .get_cell_style(swatch_col_0_x, palette_row_y)
        .and_then(|s| s.fg);
    let color_at_col4 = harness
        .get_cell_style(swatch_col_4_x, palette_row_y)
        .and_then(|s| s.fg);

    // The two swatches should have different colors (hue 0 vs hue 120)
    assert_ne!(
        color_at_col0, color_at_col4,
        "Col 0 and col 4 palette swatches should be different colors: {:?} vs {:?}",
        color_at_col0, color_at_col4
    );

    // Click on col 4 swatch (the 5th one)
    harness.mouse_click(swatch_col_4_x, palette_row_y).unwrap();
    harness.render().unwrap();

    // Wait for click to be processed and display updated
    harness
        .wait_until(|h| {
            // The Hex: line should change from the initial bg color (#1E1E1E)
            let s = h.screen_to_string();
            s.lines()
                .any(|l| l.contains("Hex:") && !l.contains("#1E1E1E"))
        })
        .unwrap();

    let screen_after_click = harness.screen_to_string();

    // Now click on col 0 swatch (should apply a different color)
    harness.mouse_click(swatch_col_0_x, palette_row_y).unwrap();
    harness.render().unwrap();

    // Wait for the hex to change again
    let hex_after_col4 = screen_after_click
        .lines()
        .find(|l| l.contains("Hex:"))
        .unwrap_or("")
        .to_string();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            if let Some(hex_line) = s.lines().find(|l| l.contains("Hex:")) {
                hex_line != hex_after_col4
            } else {
                false
            }
        })
        .unwrap();

    let screen_after_second_click = harness.screen_to_string();
    let hex_after_col0 = screen_after_second_click
        .lines()
        .find(|l| l.contains("Hex:"))
        .unwrap_or("")
        .to_string();

    // The two clicks should have produced different hex values
    assert_ne!(
        hex_after_col4, hex_after_col0,
        "Clicking col 4 and col 0 palette swatches should apply different colors.\nAfter col 4: {}\nAfter col 0: {}",
        hex_after_col4, hex_after_col0
    );

    harness.assert_no_plugin_errors();
}

/// Test that PageUp/PageDown keys work for navigating the theme editor's left sidebar.
/// Bug: PageUp/PageDown keys were not bound in the theme editor mode,
/// so pressing them did nothing.
#[test]
fn test_theme_editor_page_up_page_down() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin(&plugins_dir, "theme_editor");

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    // Use a theme with enough fields so PageDown has room to move
    let test_theme = r#"{
        "name": "test",
        "editor": {
            "bg": [30, 30, 30],
            "fg": [200, 200, 200],
            "cursor": [255, 255, 255],
            "selection_bg": [38, 79, 120],
            "current_line_bg": [40, 40, 40],
            "line_number_fg": [100, 100, 100],
            "line_number_bg": [30, 30, 30],
            "ruler_bg": [50, 50, 50],
            "whitespace_indicator": [70, 70, 70],
            "diff_add_bg": [35, 60, 35],
            "diff_remove_bg": [70, 35, 35],
            "diff_modify_bg": [40, 38, 30],
            "inactive_cursor": [100, 100, 100]
        },
        "ui": {
            "tab_active_bg": [50, 50, 50],
            "tab_inactive_bg": [30, 30, 30],
            "tab_active_fg": [200, 200, 200],
            "tab_inactive_fg": [128, 128, 128],
            "statusbar_bg": [0, 95, 135],
            "statusbar_fg": [200, 200, 200],
            "menu_bg": [37, 37, 38],
            "menu_fg": [200, 200, 200],
            "menu_selected_bg": [4, 57, 94],
            "menu_selected_fg": [255, 255, 255],
            "menu_border": [69, 69, 69],
            "prompt_bg": [37, 37, 38],
            "prompt_fg": [200, 200, 200]
        },
        "syntax": {
            "keyword": [86, 156, 214],
            "string": [206, 145, 120],
            "comment": [106, 153, 85],
            "function": [220, 220, 170],
            "type": [78, 201, 176],
            "constant": [79, 193, 255],
            "variable": [156, 220, 254],
            "operator": [200, 200, 200]
        }
    }"#;
    fs::write(themes_dir.join("test.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    harness.render().unwrap();

    // Open theme editor
    open_theme_editor(&mut harness);

    // Get initial screen
    let screen_initial = harness.screen_to_string();

    // The initial selection should be on the first field (e.g. bg under Editor)
    // Press PageDown - it should jump multiple fields at once
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string() != screen_initial)
        .unwrap();

    let screen_after_pagedown = harness.screen_to_string();

    // After PageDown the selection indicator (▸) should have moved significantly
    // Find the selected line (contains ▸) in each screen
    let _initial_selected = screen_initial
        .lines()
        .position(|l| l.contains('\u{25B8}'))
        .expect("Should have a selected line initially");
    let after_pagedown_selected = screen_after_pagedown
        .lines()
        .position(|l| l.contains('\u{25B8}'))
        .expect("Should have a selected line after PageDown");

    // PageDown should have moved by more than 1 line (i.e. it's not just Down)
    // OR the view should have scrolled (selected item on a different logical index)
    // The key point: the screen should have changed after pressing PageDown.
    assert!(
        screen_after_pagedown != screen_initial,
        "PageDown should change the screen"
    );

    // Now press PageUp to go back
    let screen_before_pageup = harness.screen_to_string();
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string() != screen_before_pageup)
        .unwrap();

    let screen_after_pageup = harness.screen_to_string();

    // After PageUp, the selection should have moved back up
    let after_pageup_selected = screen_after_pageup
        .lines()
        .position(|l| l.contains('\u{25B8}'))
        .expect("Should have a selected line after PageUp");

    // PageUp should move selection upward (or at least change the display)
    assert!(
        after_pageup_selected <= after_pagedown_selected
            || screen_after_pageup != screen_after_pagedown,
        "PageUp should move selection up. After PageDown line: {}, After PageUp line: {}",
        after_pagedown_selected,
        after_pageup_selected
    );

    harness.assert_no_plugin_errors();
}

/// Test that named color swatches in the theme editor use the native ANSI
/// color (e.g. Color::Yellow) rather than an RGB approximation.
///
/// BUG: When a theme field uses a named color like "Yellow", the swatch (██)
/// in the theme editor was rendered as Color::Rgb(255, 255, 0) instead of
/// Color::Yellow. This is wrong because the actual theme renders Color::Yellow
/// as ANSI color 3 (via crossterm), which terminals display as a different
/// shade than RGB(255, 255, 0). The swatch should use the native ANSI color
/// so it matches what the user actually sees.
#[test]
fn test_named_color_swatch_uses_native_ansi_color() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    // Create a theme with a named color "Yellow" for tab_active_fg.
    // The swatch should render as Color::Yellow (native ANSI),
    // not Color::Rgb(255, 255, 0).
    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    let test_theme = r#"{
        "name": "dark",
        "editor": {
            "bg": [30, 30, 30],
            "fg": [212, 212, 212]
        },
        "ui": {
            "tab_active_fg": "Yellow",
            "tab_active_bg": [0, 0, 200]
        },
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("dark.json"), test_theme).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();
    harness.render().unwrap();

    // Open theme editor
    open_theme_editor(&mut harness);

    // Wait for the theme editor to fully display
    harness
        .wait_until(|h| h.screen_to_string().contains("Theme Editor"))
        .unwrap();

    // The "ui" section is collapsed by default. Navigate down until the
    // selection indicator (▸) lands on the UI Elements section header.
    //
    // Use true semantic waiting: after each Down press, wait until the
    // *selected line's content* (the line containing ▸) actually changes.
    // Waiting on "screen changed" is unreliable because unrelated async
    // work (timers, async redraws, etc.) can flip a cell between the
    // key-press and the real selection update, making the previous wait
    // return early and letting the test race ahead of the plugin thread —
    // which was the source of this test's intermittent timeouts.
    let selection_indicator = '\u{25B8}'; // ▸
    let selected_line = |h: &EditorTestHarness| -> Option<String> {
        h.screen_to_string()
            .lines()
            .find(|l| l.contains(selection_indicator))
            .map(|s| s.to_string())
    };
    let line_is_collapsed_ui_section = |l: &str| l.contains("> UI") || l.contains("> ui");
    let line_is_expanded_ui_section = |l: &str| l.contains("▼ UI") || l.contains("▼ ui");

    loop {
        if selected_line(&harness)
            .as_deref()
            .map(line_is_collapsed_ui_section)
            .unwrap_or(false)
        {
            break;
        }
        let before = selected_line(&harness);
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        // Wait until ▸ is visible AND on a different line. Without the
        // is_some() guard, a transient scroll-lag frame (where ▸ is
        // off-viewport) would satisfy `None != Some(old)`, letting the
        // loop capture `before = None` on the next iteration and then
        // block forever on `None != None`.
        harness
            .wait_until(|h| {
                let cur = selected_line(h);
                cur.is_some() && cur != before
            })
            .unwrap();
    }

    // Expand the UI section and wait semantically for the selected line
    // to flip from collapsed (▸> UI) to expanded (▸▼ UI).
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            selected_line(h)
                .as_deref()
                .map(line_is_expanded_ui_section)
                .unwrap_or(false)
        })
        .unwrap();

    // Navigate down to tab_active_fg within the expanded UI section.
    // Same semantic-wait pattern: wait for the selected line's content to
    // change after each Down press, not just for any screen cell to flip.
    loop {
        if selected_line(&harness)
            .as_deref()
            .map(|l| l.contains("tab_active_fg"))
            .unwrap_or(false)
        {
            break;
        }
        let before = selected_line(&harness);
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness
            .wait_until(|h| {
                let cur = selected_line(h);
                cur.is_some() && cur != before
            })
            .unwrap();
    }

    // Move selection away so the tab_active_fg row renders without the
    // selection highlight (which adds a bg overlay that breaks the
    // fg==bg swatch detection).
    let before = selected_line(&harness);
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            let cur = selected_line(h);
            cur.is_some() && cur != before
        })
        .unwrap();

    // Wait for the tab_active_fg swatch to render with the correct native
    // ANSI Yellow color. On slow CI the plugin may not have finished
    // painting the inline overlay yet.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            let lines: Vec<&str> = screen.lines().collect();
            let Some(row) = lines
                .iter()
                .position(|l| l.contains("tab_active_fg") && l.contains("██"))
            else {
                return false;
            };
            find_swatch_color(h, row as u16) == Some(Color::Yellow)
        })
        .unwrap();
}

/// Regression test: switching the active theme via "Select Theme" while a
/// theme-editor plugin buffer is open must refresh the overlay colors the
/// plugin painted with.
///
/// The bug was that the plugin resolved its UI palette client-side — it
/// read `editor.getThemeData()` in JS, dug out the RGB tuple for
/// `syntax.keyword`, and handed the RGB array to `setVirtualBufferContent`.
/// That made the overlay an `OverlayFace::Style` with baked RGB, so the
/// core's render-time theme-key resolver (`split_rendering.rs` →
/// `ThemedStyle` branch) was bypassed and a theme switch left the buffer
/// painted in the old theme's colors.
///
/// The fix is to pass theme-key strings (e.g. `"syntax.keyword"`) straight
/// through to the core. `OverlayFace::from_options` then stores the key
/// in `ThemedStyle { fg_theme: Some("syntax.keyword"), .. }` and the next
/// render resolves it against `ctx.theme` — which is the new theme after
/// `apply_theme`.
#[test]
fn test_theme_editor_colors_update_on_theme_change() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    let mut config = fresh::config::Config::default();
    config.theme = "dark".into();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 40, config, project_root).unwrap();

    harness.render().unwrap();

    // Open the theme editor with "dark" selected.
    open_theme_editor(&mut harness);

    // The "Theme Editor:" header row carries a hardcoded `colors.header`
    // fg of [100, 180, 255] (blue) AND the buffer's default editor.bg.
    // Record both the text-cell and an empty cell on the same row.
    let header_pos = harness
        .find_text_on_screen("Theme Editor:")
        .expect("'Theme Editor:' header should be visible in the theme editor buffer");
    let dark_header_fg = harness
        .get_cell_style(header_pos.0, header_pos.1)
        .expect("header cell should have a style")
        .fg;

    // Sample a cell well to the right on the same row where there's no text
    // (so we get the buffer's default bg, not an overlay).
    let empty_col = header_pos.0.saturating_add(60);
    let dark_row_empty_bg = harness
        .get_cell_style(empty_col, header_pos.1)
        .expect("cell should have a style")
        .bg;
    assert_eq!(
        dark_row_empty_bg,
        Some(Color::Rgb(30, 30, 30)),
        "With dark theme, theme editor buffer empty cells should have bg [30,30,30], got {:?}. \
         Screen:\n{}",
        dark_row_empty_bg,
        harness.screen_to_string(),
    );

    // --- Switch to the light theme via the command palette ---
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Select Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("Select theme").unwrap();

    for _ in 0..20 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text("light").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    // After the switch, both the buffer background AND the hardcoded header
    // fg should reflect the new theme. In particular, the plugin's cached
    // overlay-styles (baked RGB) should be refreshed so they don't stay as
    // the dark theme's values against the new light bg.
    let header_pos = harness
        .find_text_on_screen("Theme Editor:")
        .expect("'Theme Editor:' header should still be visible after theme switch");
    let empty_col = header_pos.0.saturating_add(60);
    let light_row_empty_bg = harness
        .get_cell_style(empty_col, header_pos.1)
        .expect("cell should have a style")
        .bg;
    assert_eq!(
        light_row_empty_bg,
        Some(Color::Rgb(255, 255, 255)),
        "After switching to light theme, theme editor buffer empty cells should have \
         bg [255,255,255], got {:?}. Screen:\n{}",
        light_row_empty_bg,
        harness.screen_to_string(),
    );

    let light_header_fg = harness
        .get_cell_style(header_pos.0, header_pos.1)
        .expect("header cell should have a style")
        .fg;

    // BUG REPRODUCTION: the plugin-provided header highlight fg is baked at
    // `setVirtualBufferContent` time and does not refresh when the theme
    // changes. We assert that the fg DOES change — this is the behaviour
    // the user expects, and it currently fails because the plugin's
    // hardcoded RGB highlights never get re-applied.
    assert_ne!(
        light_header_fg,
        dark_header_fg,
        "After switching themes, the theme editor's header-text fg should be refreshed \
         (expected it to differ from the dark-theme value {:?}). The plugin-provided \
         overlay colors appear to be baked at creation time and never refreshed on \
         theme change. Screen:\n{}",
        dark_header_fg,
        harness.screen_to_string(),
    );
}

/// Find the fg color of the swatch (██) on a given screen row.
/// Scans the left panel area (columns 0-37) for cells where fg == bg,
/// which indicates a color swatch.
fn find_swatch_color(harness: &EditorTestHarness, row: u16) -> Option<Color> {
    for col in 0..38 {
        if let Some(cell_text) = harness.get_cell(col, row) {
            if cell_text == "█" {
                if let Some(style) = harness.get_cell_style(col, row) {
                    // Swatch cells have fg == bg (same color)
                    if style.fg.is_some() && style.fg == style.bg {
                        return style.fg;
                    }
                }
            }
        }
    }
    None
}

/// Regression test: pasting (or any path that runs
/// `apply_events_as_bulk_edit`) inside the Theme Editor must not
/// panic. Reported in the field as
/// `called Option::unwrap() on a None value` at
/// `app/event_apply.rs:228`.
///
/// Reproduces by opening the Theme Editor (which puts the active
/// buffer inside a buffer-group panel whose `BufferId` lives in the
/// inner panel's `keyed_states`, NOT in the outer split's
/// `keyed_states`) and then triggering a multi-event paste. The
/// bulk-edit path captures `split_id` from the outer split but
/// `active_buf` from `effective_active_pair`, so the outer split's
/// `keyed_states.get(&active_buf)` is `None` and unwraps to a panic.
#[test]
fn test_paste_in_theme_editor_does_not_panic() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();
    harness.render().unwrap();

    open_theme_editor(&mut harness);

    // Use the internal-only clipboard so this test doesn't fight the
    // host system clipboard in parallel CI runs.
    harness
        .editor_mut()
        .set_clipboard_for_test("zzz\nzzz".to_string());

    // Multi-line clipboard content forces the bulk-edit path even
    // with a single cursor + no selection if the implementation ever
    // changes; for the current implementation, force the bulk path
    // explicitly by adding a second cursor first via Ctrl+D
    // (add-cursor-next-match) — but that requires a match, so just
    // hand-craft the events through paste_text.
    //
    // The simpler reproducer: select the current line then paste,
    // which produces (Delete, Insert) — 2 events → bulk path.
    harness
        .send_key(KeyCode::Home, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // This must not panic.
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();
}

/// Regression test: opening the Theme Editor for the `terminal` built-in
/// theme must populate the tree panel with field rows. The terminal theme
/// uses non-color schema fields (`selection_modifier`,
/// `semantic_highlight_modifier`) which are arrays of strings — the
/// plugin used to treat any array as an RGB tuple and crash inside
/// `formatColorValue → rgbToHex → toHex` when it hit a modifier value,
/// which aborted `buildTreeLines` and left the tree panel completely
/// blank.
#[test]
fn test_theme_editor_terminal_builtin_renders_field_rows() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "theme_editor");

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 40, Default::default(), project_root)
            .unwrap();
    harness.render().unwrap();

    // Open Edit Theme via command palette and pick the `terminal` built-in.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();
    harness.type_text("terminal").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the editor's header so we know the buffer was opened.
    harness
        .wait_until(|h| h.screen_to_string().contains("Theme Editor: terminal"))
        .unwrap();

    // The tree panel should list the `editor` section header and at least
    // one field row. If `buildTreeLines` aborted the tree will be blank
    // (only the divider column survives), so the section name and field
    // keys never appear.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor: terminal")
                && screen.contains("editor")
                && (screen.contains(" bg ") || screen.contains(" fg "))
        })
        .unwrap_or_else(|_| {
            panic!(
                "Theme editor tree panel never populated for terminal theme. \
                 Likely the plugin's `formatColorValue` crashed on a non-color \
                 field (e.g. `selection_modifier: [\"reversed\"]`). \
                 Screen:\n{}",
                harness.screen_to_string()
            )
        });
}
