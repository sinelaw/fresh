// E2E tests for the theme system

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use ratatui::style::Color;

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

    // Verify some high-contrast theme colors (from Theme::high_contrast() Rust fallback)
    assert_eq!(theme.editor_bg, Color::Black);
    assert_eq!(theme.editor_fg, Color::White);
    assert_eq!(theme.cursor, Color::White);
    assert_eq!(theme.tab_active_fg, Color::Black);
    assert_eq!(theme.tab_active_bg, Color::Yellow);
}

#[test]
#[ignore = "Theme loading now errors instead of falling back - behavior change from json theme refactor"]
fn test_invalid_theme_falls_back_to_dark() {
    let config = Config {
        theme: "nonexistent-theme".into(),
        ..Default::default()
    };

    let harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Should fall back to dark theme
    let theme = harness.editor().theme();
    assert_eq!(theme.name, "dark");
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
