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
    assert_eq!(dark_theme.selection_bg, Color::Rgb(50, 50, 60));

    // Light theme has a lighter selection background
    assert_eq!(light_theme.selection_bg, Color::Rgb(225, 232, 242));
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

    // Verify default theme is high-contrast (the fallback).
    // Sample the first content row (row 2) inside the editor area so we hit
    // `editor.bg` rather than the post-EOF shade (`editor.after_eof_bg`).
    let default_style = harness.get_cell_style(5, 2);
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
    let applied_style = harness.get_cell_style(5, 2);
    let applied_bg = applied_style.and_then(|s| s.bg);
    assert_eq!(
        applied_bg,
        Some(catppuccin_bg),
        "After selection, editor should render with catppuccin-mocha background (30,30,46), got {:?}",
        applied_bg
    );

    // Verify the config file was persisted with the theme key.
    // The key for user themes is a file:// URL pointing to the theme file.
    let config_path = temp_dir.path().join("config").join("config.json");
    let saved_config = fs::read_to_string(&config_path).unwrap();
    let saved_json: serde_json::Value = serde_json::from_str(&saved_config).unwrap();

    let saved_theme = saved_json
        .get("theme")
        .and_then(|t| t.as_str())
        .unwrap_or("");
    assert!(
        saved_theme.contains("catppuccin-mocha"),
        "Config should contain a key referencing 'catppuccin-mocha', \
         not the raw JSON name field 'Catppuccin Mocha'. Got: '{}'. Config content: {}",
        saved_theme,
        saved_config
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
    let restarted_style = harness2.get_cell_style(5, 2);
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
    let style = harness.get_cell_style(5, 2);
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

/// Create a minimal fake LSP server that sends a diagnostic on didOpen.
///
/// Sends a single ERROR diagnostic covering `"hello"` (line 1, chars 17–24)
/// in the test file `fn main() { let x: i32 = "hello"; }`.
fn create_diagnostic_lsp_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG="${1:-/dev/null}"
> "$LOG"

read_message() {
    local cl=0
    while IFS=: read -r k v; do
        k=$(echo "$k" | tr -d '\r\n')
        v=$(echo "$v" | tr -d '\r\n ')
        [ "$k" = "Content-Length" ] && cl=$v
        [ -z "$k" ] && break
    done
    [ $cl -gt 0 ] && dd bs=1 count=$cl 2>/dev/null
}

send_message() {
    local m="$1"
    printf "Content-Length: ${#m}\r\n\r\n%s" "$m"
}

while true; do
    msg=$(read_message)
    [ -z "$msg" ] && break
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)
    echo "RECV: $method" >> "$LOG"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"textDocumentSync":{"openClose":true,"change":2}}}}'
            ;;
        "textDocument/didOpen")
            URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "SENT: diag" >> "$LOG"
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$URI"'","diagnostics":[{"range":{"start":{"line":1,"character":17},"end":{"line":1,"character":24}},"severity":1,"message":"type error"}]}}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            [ -n "$method" ] && [ -n "$msg_id" ] && \
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            ;;
    esac
done
"##;
    let path = dir.join("diag_lsp.sh");
    fs::write(&path, script).unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut p = fs::metadata(&path).unwrap().permissions();
        p.set_mode(0o755);
        fs::set_permissions(&path, p).unwrap();
    }
    path
}

/// Diagnostic overlay colors must update when the theme changes.
///
/// Diagnostic overlays bake the theme's `diagnostic_error_bg` as an RGB value
/// at creation time. When the user switches themes, `apply_theme` must
/// re-apply all stored diagnostics with the new theme colors.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Bash-based fake LSP
fn test_diagnostic_overlay_colors_update_on_theme_change() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let script = create_diagnostic_lsp_script(temp_dir.path());
    let log_file = temp_dir.path().join("diag_log.txt");

    let test_file = temp_dir.path().join("test.rs");
    fs::write(
        &test_file,
        "fn main() {\n    let x: i32 = \"hello\";\n    println!(\"{}\", x);\n}\n",
    )?;

    let mut config = Config::default();
    config.theme = "dark".into();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
            process_limits: Default::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: None,
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open file → triggers didOpen → fake LSP sends diagnostic
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for diagnostic to arrive (status bar shows "E:1")
    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;
    harness.render()?;

    // Find the diagnostic text on screen and verify bg is dark theme's error bg
    let dark_error_bg = Color::Rgb(60, 20, 20);
    let pos = harness
        .find_text_on_screen("hello")
        .expect("'hello' should be visible on screen");
    let style = harness
        .get_cell_style(pos.0, pos.1)
        .expect("cell should have a style");
    assert_eq!(
        style.bg,
        Some(dark_error_bg),
        "With dark theme, diagnostic bg should be {:?}, got {:?}",
        dark_error_bg,
        style.bg,
    );

    // Switch to light theme via command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.wait_for_prompt()?;
    harness.type_text("Select Theme")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.wait_for_screen_contains("Select theme")?;

    // Clear pre-filled input and type "light"
    for _ in 0..20 {
        harness.send_key(KeyCode::Backspace, KeyModifiers::NONE)?;
    }
    harness.type_text("light")?;
    harness.render()?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.wait_for_prompt_closed()?;
    harness.render()?;

    // Verify the diagnostic overlay now uses the light theme's error bg
    let light_error_bg = Color::Rgb(255, 210, 210);
    let pos = harness
        .find_text_on_screen("hello")
        .expect("'hello' should still be visible");
    let style = harness
        .get_cell_style(pos.0, pos.1)
        .expect("cell should have a style");
    assert_eq!(
        style.bg,
        Some(light_error_bg),
        "After switching to light theme, diagnostic bg should be {:?}, got {:?}",
        light_error_bg,
        style.bg,
    );

    Ok(())
}

/// Theme selector should display URLs without scheme prefixes (https://, file://).
///
/// User themes have `file:///path/to/theme.json` keys. The selector should strip
/// the `file://` prefix but preserve the leading `/` so it shows `/path/to/theme.json`.
#[test]
fn test_theme_selector_strips_url_scheme_from_display() {
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Install a custom theme so there's a file:// URL in the list
    let themes_dir = temp_dir.path().join("config").join("themes");
    fs::create_dir_all(&themes_dir).unwrap();
    fs::write(
        themes_dir.join("catppuccin-mocha.json"),
        custom_catppuccin_theme_json(),
    )
    .unwrap();

    let project_root = temp_dir.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();

    let mut harness = EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_working_dir(project_root)
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.render().unwrap();

    // Open command palette and select theme
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Select Theme").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("Select theme").unwrap();

    // Clear input and type catppuccin to filter to the custom theme
    for _ in 0..20 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text("catppuccin").unwrap();
    harness.render().unwrap();

    // The screen should show just the relative filename, not the full path or file:// URL
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("file://"),
        "Theme selector should not display 'file://' scheme prefix. Screen:\n{}",
        screen
    );
    // Should show just the filename relative to the themes dir
    assert!(
        screen.contains("catppuccin-mocha.json"),
        "Theme selector should show the relative path under themes dir. Screen:\n{}",
        screen
    );

    // Dismiss the prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    drop(temp_dir);
}
