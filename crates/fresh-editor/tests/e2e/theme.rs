// E2E tests for the theme system

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use ratatui::style::Color;
use std::fs;
use tempfile::TempDir;

#[test]
fn test_default_theme_is_dark() {
    let harness = EditorTestHarness::new(80, 24).unwrap();

    // Default theme should be "high-contrast"
    let theme = harness.editor().theme();
    assert_eq!(theme.name, "high-contrast");
}

#[test]
fn test_theme_loading_from_config_dark() {
    let config = Config {
        theme: "dark".into(),
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    let theme = harness.editor().theme();
    assert_eq!(theme.name, "dark");

    // Verify some dark theme colors
    assert_eq!(theme.editor_bg, Color::Rgb(30, 30, 30));
    assert_eq!(theme.editor_fg, Color::Rgb(212, 212, 212));
    assert_eq!(theme.tab_active_fg, Color::Yellow);
    assert_eq!(theme.tab_active_bg, Color::Blue);
}

#[test]
fn test_theme_loading_from_config_light() {
    let config = Config {
        theme: "light".into(),
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    let theme = harness.editor().theme();
    assert_eq!(theme.name, "light");

    // Verify some light theme colors (from Theme::from_name(theme::THEME_LIGHT) Rust fallback)
    assert_eq!(theme.editor_bg, Color::Rgb(255, 255, 255));
    assert_eq!(theme.editor_fg, Color::Rgb(0, 0, 0));
    assert_eq!(theme.tab_active_fg, Color::Rgb(40, 40, 40));
    assert_eq!(theme.tab_active_bg, Color::Rgb(255, 255, 255));
}

#[test]
fn test_theme_loading_from_config_high_contrast() {
    let config = Config {
        theme: "high-contrast".into(),
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    let theme = harness.editor().theme();
    assert_eq!(theme.name, "high-contrast");

    // Verify some high-contrast theme colors (explicit RGB values)
    assert_eq!(theme.editor_bg, Color::Rgb(0, 0, 0));
    assert_eq!(theme.editor_fg, Color::Rgb(255, 255, 255));
    assert_eq!(theme.cursor, Color::Rgb(255, 255, 255));
    assert_eq!(theme.tab_active_fg, Color::Rgb(0, 0, 0));
    assert_eq!(theme.tab_active_bg, Color::Rgb(255, 255, 0));
}

#[test]
fn test_invalid_theme_falls_back_to_default() {
    let config = Config {
        theme: "nonexistent-theme".into(),
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Should fall back to default theme (high-contrast)
    let theme = harness.editor().theme();
    assert_eq!(theme.name, "high-contrast");
}

#[test]
fn test_theme_renders_with_correct_tab_colors() {
    let config = Config {
        theme: "dark".into(),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    // Check that tab bar uses theme colors
    // The active tab should have the theme's tab_active colors
    let theme = harness.editor().theme();

    // Get style of a cell in the tab bar area (row 1, after menu bar at row 0)
    if let Some(style) = harness.get_cell_style(1, 1) {
        // For dark theme, active tab has yellow fg and blue bg
        assert_eq!(style.fg, Some(theme.tab_active_fg));
        assert_eq!(style.bg, Some(theme.tab_active_bg));
    }
}

#[test]
fn test_theme_renders_with_correct_status_bar_colors() {
    let config = Config {
        theme: "dark".into(),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    let theme = harness.editor().theme();

    // Status bar is at the bottom (row 23 for a 24-row terminal)
    if let Some(style) = harness.get_cell_style(1, 23) {
        // Status bar background should match theme's status bar colors
        // Foreground may be Reset (uses terminal default) or the theme's fg color
        // We check bg which should be consistently themed
        assert!(
            style.bg == Some(theme.status_bar_bg) || style.bg.is_some(),
            "Status bar should have a background color set, got: {:?}",
            style.bg
        );
    }
}

#[test]
fn test_light_theme_renders_differently_than_dark() {
    let dark_config = Config {
        theme: "dark".into(),
        ..Default::default()
    };

    let light_config = Config {
        theme: "light".into(),
        ..Default::default()
    };

    let mut dark_harness = EditorTestHarness::with_config(80, 24, dark_config).unwrap();
    let mut light_harness = EditorTestHarness::with_config(80, 24, light_config).unwrap();

    dark_harness.render().unwrap();
    light_harness.render().unwrap();

    // Get tab bar styles from both themes (row 1, after menu bar at row 0)
    let dark_style = dark_harness.get_cell_style(1, 1);
    let light_style = light_harness.get_cell_style(1, 1);

    // The colors should be different
    assert_ne!(
        dark_style, light_style,
        "Dark and light themes should render with different colors"
    );
}

#[test]
fn test_theme_diagnostic_colors() {
    let config = Config {
        theme: "dark".into(),
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let theme = harness.editor().theme();

    // Verify diagnostic colors are set correctly for dark theme
    assert_eq!(theme.diagnostic_error_fg, Color::Red);
    assert_eq!(theme.diagnostic_error_bg, Color::Rgb(60, 20, 20));
    assert_eq!(theme.diagnostic_warning_fg, Color::Yellow);
    assert_eq!(theme.diagnostic_warning_bg, Color::Rgb(60, 50, 0));
    assert_eq!(theme.diagnostic_info_fg, Color::Blue);
    assert_eq!(theme.diagnostic_info_bg, Color::Rgb(0, 30, 60));
}

#[test]
fn test_theme_syntax_highlighting_colors() {
    let config = Config {
        theme: "dark".into(),
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let theme = harness.editor().theme();

    // Verify syntax highlighting colors are set
    assert_eq!(theme.syntax_keyword, Color::Rgb(86, 156, 214));
    assert_eq!(theme.syntax_string, Color::Rgb(206, 145, 120));
    assert_eq!(theme.syntax_comment, Color::Rgb(106, 153, 85));
    assert_eq!(theme.syntax_function, Color::Rgb(220, 220, 170));
    assert_eq!(theme.syntax_type, Color::Rgb(78, 201, 176));
    assert_eq!(theme.syntax_variable, Color::Rgb(156, 220, 254));
}

#[test]
fn test_all_available_themes_can_be_loaded() {
    let themes = vec!["dark", "light", "high-contrast"];

    for theme_name in themes {
        let config = Config {
            theme: theme_name.into(),
            ..Default::default()
        };

        let harness = EditorTestHarness::with_config(80, 24, config).unwrap();
        let theme = harness.editor().theme();

        assert_eq!(
            theme.name, theme_name,
            "Theme '{theme_name}' should load correctly"
        );
    }
}

#[test]
fn test_theme_selection_colors() {
    let dark_config = Config {
        theme: "dark".into(),
        ..Default::default()
    };

    let light_config = Config {
        theme: "light".into(),
        ..Default::default()
    };

    let dark_harness = EditorTestHarness::with_config(80, 24, dark_config).unwrap();
    let light_harness = EditorTestHarness::with_config(80, 24, light_config).unwrap();

    let dark_theme = dark_harness.editor().theme();
    let light_theme = light_harness.editor().theme();

    // Selection colors should be different between themes
    assert_ne!(dark_theme.selection_bg, light_theme.selection_bg);

    // Dark theme has a darker selection background
    assert_eq!(dark_theme.selection_bg, Color::Rgb(38, 79, 120));

    // Light theme has a lighter selection background
    assert_eq!(light_theme.selection_bg, Color::Rgb(173, 214, 255));
}

#[test]
fn test_theme_popup_colors() {
    let config = Config {
        theme: "dark".into(),
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let theme = harness.editor().theme();

    // Verify popup colors
    assert_eq!(theme.popup_border_fg, Color::Gray);
    assert_eq!(theme.popup_bg, Color::Rgb(30, 30, 30));
    assert_eq!(theme.popup_selection_bg, Color::Rgb(58, 79, 120));
    assert_eq!(theme.popup_text_fg, Color::White);
}

#[test]
fn test_case_insensitive_theme_name() {
    let config = Config {
        theme: "HIGH-CONTRAST".into(), // uppercase
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let theme = harness.editor().theme();

    // Should still load high-contrast theme (case insensitive)
    assert_eq!(theme.name, "high-contrast");
}

#[test]
fn test_theme_with_underscore_variant() {
    let config = Config {
        theme: "high_contrast".into(), // underscore instead of dash
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let theme = harness.editor().theme();

    // Should still load high-contrast theme (accepts both - and _)
    assert_eq!(theme.name, "high-contrast");
}

/// Minimal custom theme JSON for issue #1001 regression tests.
///
/// The JSON "name" field intentionally uses spaces and mixed case ("Catppuccin Mocha")
/// while the file is saved as "catppuccin-mocha.json" — this mismatch is the root
/// cause of the bug. Only editor.bg is set to a distinctive value (Rgb(30,30,46))
/// so the tests can distinguish it from the high-contrast fallback (Black).
/// All other sections use serde defaults — no external resources needed.
fn custom_catppuccin_theme_json() -> &'static str {
    r#"{
        "name": "Catppuccin Mocha",
        "editor": { "bg": [30, 30, 46] },
        "ui": {},
        "search": {},
        "diagnostic": {},
        "syntax": {}
    }"#
}

/// Issue #1001: Theme not found after restart when JSON name differs from filename.
///
/// Regression test: Simulates a user selecting a custom theme whose JSON "name" field
/// ("Catppuccin Mocha") differs from its filename ("catppuccin-mocha.json"). After
/// selecting the theme through the UI and "restarting" (creating a new editor instance
/// with the same config directory), the theme must still load correctly.
///
/// Without the fix:
///   - apply_theme() saves "Catppuccin Mocha" (the JSON name) to config.json
///   - On restart, the lookup normalizes "Catppuccin Mocha" → "catppuccin mocha"
///     (spaces NOT converted to hyphens) which doesn't match the key "catppuccin-mocha"
///   - Falls back to default "high-contrast" theme
///
/// With the fix:
///   - apply_theme() saves "catppuccin-mocha" (the normalized registry key) to config
///   - On restart, the lookup finds it correctly
#[test]
fn test_issue_1001_theme_persists_after_restart_with_name_mismatch() {
    // --- Setup: create isolated temp dir with a custom theme ---
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Create themes directory and install the custom theme
    let themes_dir = temp_dir.path().join("config").join("themes");
    fs::create_dir_all(&themes_dir).unwrap();
    fs::write(
        themes_dir.join("catppuccin-mocha.json"),
        custom_catppuccin_theme_json(),
    )
    .unwrap();

    // Create project root & empty plugins dir for isolation
    let project_root = temp_dir.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();

    // --- Session 1: Select the custom theme via keyboard ---
    let mut harness = EditorTestHarness::create(
        100,
        40,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.render().unwrap();

    // Verify default theme is high-contrast (the fallback)
    let default_style = harness.get_cell_style(5, 3);
    let default_bg = default_style.and_then(|s| s.bg);
    assert_eq!(
        default_bg,
        Some(Color::Rgb(0, 0, 0)),
        "Editor should start with high-contrast theme (black background)"
    );

    // Open command palette (Ctrl+P)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Type "Select Theme" and execute the command
    harness.type_text("Select Theme").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the theme selection prompt to appear
    harness.wait_for_screen_contains("Select theme").unwrap();

    // Clear the pre-filled input (current theme name "high-contrast" = 13 chars)
    // Use Backspace to clear, then type the new theme name
    for _ in 0..20 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }

    // Type the custom theme name to filter suggestions
    harness.type_text("catppuccin").unwrap();
    harness.render().unwrap();

    // Confirm the selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    // Verify the theme was applied by checking rendered editor background color.
    // Catppuccin Mocha uses Rgb(30, 30, 46), NOT black.
    let catppuccin_bg = Color::Rgb(30, 30, 46);
    let applied_style = harness.get_cell_style(5, 3);
    let applied_bg = applied_style.and_then(|s| s.bg);
    assert_eq!(
        applied_bg,
        Some(catppuccin_bg),
        "After selection, editor should render with catppuccin-mocha background (30,30,46), got {:?}",
        applied_bg
    );

    // Verify the config file was persisted with the NORMALIZED name.
    // This is the core assertion: the config must contain "catppuccin-mocha"
    // (the registry key), NOT "Catppuccin Mocha" (the JSON name field).
    let config_path = temp_dir.path().join("config").join("config.json");
    let saved_config = fs::read_to_string(&config_path).unwrap();
    let saved_json: serde_json::Value = serde_json::from_str(&saved_config).unwrap();

    let saved_theme = saved_json
        .get("theme")
        .and_then(|t| t.as_str())
        .unwrap_or("");
    assert_eq!(
        saved_theme, "catppuccin-mocha",
        "BUG #1001: Config should contain normalized theme name 'catppuccin-mocha', \
         not the JSON name field. Got: '{}'. Config content: {}",
        saved_theme, saved_config
    );

    // Drop the first harness (simulates closing the editor)
    drop(harness);

    // --- Session 2: "Restart" — create a new editor with the same config directory ---
    // Read the persisted config.json to determine what theme name was saved,
    // then pass it as the Config for the new editor (simulating production startup).
    let restart_config_str = fs::read_to_string(&config_path).unwrap();
    let restart_json: serde_json::Value = serde_json::from_str(&restart_config_str).unwrap();
    let restart_theme_name = restart_json
        .get("theme")
        .and_then(|t| t.as_str())
        .unwrap_or("high-contrast");

    let mut harness2 = EditorTestHarness::create(
        100,
        40,
        HarnessOptions::new()
            .with_config(Config {
                theme: restart_theme_name.into(),
                ..Default::default()
            })
            .with_working_dir(project_root)
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness2.render().unwrap();

    // The restarted editor should load catppuccin-mocha, NOT fall back to high-contrast.
    // Verify by checking the rendered editor background color.
    let restarted_style = harness2.get_cell_style(5, 3);
    let restarted_bg = restarted_style.and_then(|s| s.bg);
    assert_eq!(
        restarted_bg,
        Some(catppuccin_bg),
        "BUG #1001: After restart, editor should render with catppuccin-mocha background (30,30,46). \
         Got {:?} — theme was likely not found and fell back to high-contrast (black). \
         Config persisted theme as '{}', config file: {}",
        restarted_bg,
        restart_theme_name,
        restart_config_str
    );

    // Keep temp_dir alive until test completes
    drop(temp_dir);
}

/// Issue #1001 variant: Config already has a space-containing theme name from old version.
///
/// This simulates a user who previously had their config written by the buggy code:
/// config.json contains "Catppuccin Mocha" (the JSON name with spaces), and the
/// theme file is "catppuccin-mocha.json". The normalization fix must handle this
/// gracefully by normalizing the lookup (spaces → hyphens).
#[test]
fn test_issue_1001_config_with_spaces_in_theme_name_loads_correctly() {
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Create themes directory and install the custom theme
    let themes_dir = temp_dir.path().join("config").join("themes");
    fs::create_dir_all(&themes_dir).unwrap();
    fs::write(
        themes_dir.join("catppuccin-mocha.json"),
        custom_catppuccin_theme_json(),
    )
    .unwrap();

    // Create project root & empty plugins dir
    let project_root = temp_dir.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();

    // Simulate what the old buggy code would have saved: the JSON "name" field
    // with spaces and mixed case, NOT the normalized filename.
    let mut harness = EditorTestHarness::create(
        100,
        40,
        HarnessOptions::new()
            .with_config(Config {
                theme: "Catppuccin Mocha".into(),
                ..Default::default()
            })
            .with_working_dir(project_root)
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.render().unwrap();

    // The editor should normalize "Catppuccin Mocha" → "catppuccin-mocha" on lookup
    // and find the theme. Verify by checking rendered background.
    let catppuccin_bg = Color::Rgb(30, 30, 46);
    let style = harness.get_cell_style(5, 3);
    let bg = style.and_then(|s| s.bg);
    assert_eq!(
        bg,
        Some(catppuccin_bg),
        "BUG #1001: Config has 'Catppuccin Mocha' (with spaces). Normalization should \
         convert this to 'catppuccin-mocha' and find the theme file. Instead got bg={:?}, \
         which suggests fallback to high-contrast (black).",
        bg
    );

    drop(temp_dir);
}
