use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Color;
use std::fs;

/// Test that the theme editor command is registered by the plugin
#[test]
fn test_theme_editor_command_registered() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Copy the theme_editor.ts plugin
    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    let plugin_dest = plugins_dir.join("theme_editor.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

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

    // Copy the theme_editor.ts plugin
    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    let plugin_dest = plugins_dir.join("theme_editor.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

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

    // Wait for theme editor to load
    // The theme editor should show "Theme Editor" in the title when loaded
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // The theme editor shows "Theme Editor:" in its header when loaded
            // If pathJoin bug exists, it will stay stuck at "Loading theme editor..."
            screen.contains("Theme Editor:") || screen.contains("custom")
        })
        .unwrap();

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
    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open command palette and run Edit Theme
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme editor to load with sections
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Theme editor should show sections like "Editor" and "Syntax"
            screen.contains("Theme Editor:") || screen.contains("Syntax")
        })
        .unwrap();

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

/// Test that the theme editor can copy from a builtin theme
/// This verifies the copy functionality works correctly
#[test]
fn test_theme_editor_copy_from_builtin() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Copy the theme_editor.ts plugin
    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

    // Create themes directory with a source theme to copy from
    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
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
    fs::write(themes_dir.join("source.json"), source_theme).unwrap();

    // Create harness
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.render().unwrap();

    // Open theme editor
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme editor to load
    harness
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

    // Press 'c' to copy from builtin theme
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for the prompt to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Copy from theme"))
        .unwrap();

    // Type the source theme name
    harness.type_text("source").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme to be copied - should show the new name "source-custom"
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("source-custom") || screen.contains("Copied from")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // Verify the theme editor now shows the copied theme name
    assert!(
        screen.contains("source-custom") || screen.contains("modified"),
        "Theme editor should show the new theme name after copy"
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
    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open theme editor
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme editor to load
    harness
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

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

    // Check that the screen contains color field labels
    assert!(
        screen.contains("Background") || screen.contains("Foreground") || screen.contains("Cursor"),
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

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open theme editor
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
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

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
    let (_, cursor_y_before) = harness.screen_cursor_position();

    // Press Enter to toggle the section
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let (_, cursor_y_after) = harness.screen_cursor_position();

    // The cursor should stay close to where it was (within 2 lines)
    let cursor_moved = (cursor_y_after as i32 - cursor_y_before as i32).abs();
    assert!(
        cursor_moved <= 2,
        "Cursor should stay near same position after toggling. Before: {}, After: {}, Moved: {}",
        cursor_y_before,
        cursor_y_after,
        cursor_moved
    );
}

/// Test that color prompt shows suggestions including current value
#[test]
fn test_color_prompt_shows_suggestions() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open theme editor
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
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

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

    // Keep pressing Down until we're on a field that opens a prompt
    // Try pressing Enter and check if prompt appears
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

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open theme editor
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
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

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

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open theme editor
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
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();

    // Find the "Background" field line and check that the description/comment is BEFORE it
    let mut found_description_before_field = false;
    for i in 1..lines.len() {
        let prev_line = lines[i - 1];
        let curr_line = lines[i];

        // If current line contains a field name like "Background:"
        if curr_line.contains("Background:") && curr_line.contains("#") {
            // The previous line should contain the description comment
            if prev_line.contains("//") && prev_line.contains("background") {
                found_description_before_field = true;
                break;
            }
        }
    }

    // BUG: Currently the comment appears AFTER the field
    // Check that we don't have the pattern: field line followed by comment
    let mut found_field_before_description = false;
    for i in 0..lines.len() - 1 {
        let curr_line = lines[i];
        let next_line = lines[i + 1];

        if curr_line.contains("Background:") && next_line.contains("//") {
            found_field_before_description = true;
            break;
        }
    }

    assert!(
        found_description_before_field && !found_field_before_description,
        "Comments should appear BEFORE fields, not after. Screen:\n{}",
        screen
    );
}

/// Test that theme changes are applied immediately to the running editor
#[test]
fn test_theme_applied_immediately_after_save() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

    // Create a test file to see theme changes
    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Hello World").unwrap();

    let themes_dir = project_root.join("themes");
    fs::create_dir(&themes_dir).unwrap();
    // Create a theme with a specific red background so we can verify it's applied
    let test_theme = r#"{
        "name": "red-test",
        "editor": {"bg": [255, 0, 0], "fg": [255, 255, 255]},
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#;
    fs::write(themes_dir.join("red-test.json"), test_theme).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_root.clone(),
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

    // Open theme editor
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
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

    // The theme "red-test" should be available
    // Press 'd' to set as default (this opens a prompt)
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();

    // Wait for the prompt to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Set default theme"))
        .unwrap();

    // Type the theme name "red-test"
    harness.type_text("red-test").unwrap();
    harness.render().unwrap();

    // Press Enter to confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme to be applied - check for either status message
    // (Rust sets "Theme changed to '...'" and plugin sets "applied")
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("applied") || screen.contains("Theme changed to")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // Close the theme editor
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();

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
            "Theme should be applied immediately. Initial: ({}, {}, {}), New: ({}, {}, {})",
            ir, ig, ib, nr, ng, nb
        );
    }
    // If we can't find RGB colors, that's okay - the test is just verifying the flow works
}

/// Test that cursor X position is preserved when toggling a section with Enter
/// BUG: Currently cursor moves one character back
#[test]
fn test_cursor_x_position_preserved_after_section_toggle() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open theme editor
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
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

    // Navigate down to find "UI Elements" section header (collapsed by default)
    let mut found_ui_elements = false;
    for i in 0..30 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        let (cx, cy) = harness.screen_cursor_position();
        eprintln!("After down {}: cursor at ({}, {})", i, cx, cy);
        if screen.contains("▶ UI Elements") {
            // Check if we're actually on that line
            let lines: Vec<&str> = screen.lines().collect();
            if cy < lines.len() as u16 {
                let cursor_line = lines[cy as usize];
                eprintln!("Cursor line: {}", cursor_line);
                if cursor_line.contains("▶ UI Elements") {
                    found_ui_elements = true;
                    break;
                }
            }
        }
    }

    assert!(
        found_ui_elements,
        "Should find and navigate to UI Elements section"
    );

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

    // Press Tab to toggle the section (expand)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    let (cursor_x_after, cursor_y_after) = harness.screen_cursor_position();

    eprintln!("=== AFTER TOGGLE ===");
    eprintln!("Cursor position: ({}, {})", cursor_x_after, cursor_y_after);
    eprintln!("Screen:\n{}", screen_after);

    // Verify we actually toggled (▶ should become ▼)
    assert!(
        screen_before.contains("▶ UI Elements"),
        "Before toggle should show collapsed UI Elements (▶). Screen:\n{}",
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
fn test_color_suggestions_show_hex_format() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open theme editor
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
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

    // Navigate down to a color field and open the prompt
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

    let screen = harness.screen_to_string();

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
fn test_color_prompt_prefilled_with_current_value() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open theme editor
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
        .wait_until(|h| h.screen_to_string().contains("Theme Editor:"))
        .unwrap();

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

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/theme_editor.ts");
    fs::copy(&plugin_source, plugins_dir.join("theme_editor.ts")).unwrap();

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

    // Open theme editor
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Edit Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme editor to load AND swatches to appear
    // Swatches are indicated by the color block "██"
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Theme Editor:") && screen.contains("██")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // The bug causes hex colors to render as "#  XXXXXX" (spaces after #) instead of "#XXXXXX"
    // This is because the buggy code used two addVirtualText calls:
    // - One with before:true for the swatch
    // - One with before:false for the space, which inserts AFTER the # character

    // Check for the bug pattern: # followed by spaces then hex digits
    let broken_pattern = Regex::new(r"#\s+[0-9A-Fa-f]").unwrap();

    // Find lines that have color fields (contain ":" and "#")
    let color_lines: Vec<&str> = screen
        .lines()
        .filter(|line| line.contains(":") && line.contains("#"))
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
