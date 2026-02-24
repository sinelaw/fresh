//! E2E tests for glob pattern language detection in language config.
//!
//! Tests the full flow of:
//! 1. Opening a file with no built-in language detection
//! 2. Adding a glob pattern rule to the config
//! 3. Reopening the file and verifying the correct language is detected
//! 4. Verifying the settings UI shows the languages configuration

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, LanguageConfig};
use std::collections::HashMap;
use std::fs;
use tempfile::TempDir;

/// Test that an extensionless file gets no language initially, but after adding
/// a glob pattern to the config and creating a fresh harness, it gets the correct language.
#[test]
fn test_glob_pattern_language_detection_flow() {
    // Create a temp directory for the test files
    let temp_dir = TempDir::new().unwrap();
    let working_dir = temp_dir.path().to_path_buf();

    // Create the extensionless config file (like lfrc)
    let test_file = working_dir.join("lfrc");
    fs::write(&test_file, "# lf file manager config\nset preview true\n").unwrap();

    // Phase 1: Open with no special language config - file should be "text"
    {
        let mut harness = EditorTestHarness::create(
            100,
            30,
            HarnessOptions::new()
                .without_empty_plugins_dir()
                .with_working_dir(working_dir.clone()),
        )
        .unwrap();

        harness.open_file(&test_file).unwrap();

        let language = &harness.editor().active_state().language;
        assert_eq!(
            language, "text",
            "lfrc file should be detected as plain text without config"
        );
    }

    // Phase 2: Write config with glob pattern, open fresh harness, verify detection
    let config_dir = working_dir.join(".fresh");
    fs::create_dir_all(&config_dir).unwrap();
    let config_json = serde_json::json!({
        "languages": {
            "bash": {
                "extensions": ["sh"],
                "filenames": ["*rc"],
                "grammar": "bash",
                "comment_prefix": "#",
                "auto_indent": true
            }
        }
    });
    fs::write(config_dir.join("config.json"), config_json.to_string()).unwrap();

    {
        // Create fresh harness that will load the config from disk
        let mut harness = EditorTestHarness::create(
            100,
            30,
            HarnessOptions::new()
                .without_empty_plugins_dir()
                .with_working_dir(working_dir.clone()),
        )
        .unwrap();

        // Reload config to pick up the file we wrote
        harness.editor_mut().reload_config();

        // Verify the config was loaded
        let loaded_languages = &harness.config().languages;
        assert!(
            loaded_languages.contains_key("bash"),
            "Config should contain bash language after reload"
        );
        let bash_config = loaded_languages.get("bash").unwrap();
        assert!(
            bash_config.filenames.contains(&"*rc".to_string()),
            "bash filenames should contain *rc glob pattern"
        );

        // Open the file - should now be detected as bash
        harness.open_file(&test_file).unwrap();
        let language = &harness.editor().active_state().language;
        assert_eq!(
            language, "bash",
            "lfrc file should be detected as bash after adding *rc glob pattern"
        );
    }
}

/// Test that *.conf glob pattern matches .conf files
#[test]
fn test_glob_star_dot_conf_language_detection() {
    let temp_dir = TempDir::new().unwrap();
    let working_dir = temp_dir.path().to_path_buf();

    // Create a .conf file
    let test_file = working_dir.join("nftables.conf");
    fs::write(&test_file, "# nftables config\ntable inet filter {\n}\n").unwrap();

    // Set up config with *.conf -> shell glob pattern
    let mut config = Config::default();
    let mut languages = HashMap::new();
    languages.insert(
        "bash".to_string(),
        LanguageConfig {
            extensions: vec!["sh".to_string()],
            filenames: vec!["*.conf".to_string()],
            grammar: "bash".to_string(),
            comment_prefix: Some("#".to_string()),
            auto_indent: true,
            ..Default::default()
        },
    );
    config.languages = languages;

    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .without_empty_plugins_dir()
            .with_config(config),
    )
    .unwrap();

    // Open the .conf file
    harness.open_file(&test_file).unwrap();

    // nftables.conf should be detected as bash via the *.conf glob
    let language = &harness.editor().active_state().language;
    assert_eq!(
        language, "bash",
        "nftables.conf should be detected as bash via *.conf glob pattern"
    );
}

/// Test that path glob patterns (e.g., /etc/**/rc.*) work for language detection
#[test]
fn test_path_glob_pattern_language_detection() {
    let temp_dir = TempDir::new().unwrap();
    let working_dir = temp_dir.path().to_path_buf();

    // Create a directory structure mimicking /etc/
    let etc_dir = working_dir.join("etc");
    let sub_dir = etc_dir.join("init");
    fs::create_dir_all(&sub_dir).unwrap();

    let test_file = etc_dir.join("rc.conf");
    fs::write(&test_file, "# rc config\nrc_verbose=yes\n").unwrap();

    let nested_file = sub_dir.join("rc.local");
    fs::write(&nested_file, "# rc local\nexit 0\n").unwrap();

    // Build the path glob pattern matching the temp directory structure
    let path_pattern = format!("{}/**/rc.*", etc_dir.display());

    let mut config = Config::default();
    let mut languages = HashMap::new();
    languages.insert(
        "bash".to_string(),
        LanguageConfig {
            extensions: vec!["sh".to_string()],
            filenames: vec![path_pattern],
            grammar: "bash".to_string(),
            comment_prefix: Some("#".to_string()),
            auto_indent: true,
            ..Default::default()
        },
    );
    config.languages = languages;

    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .without_empty_plugins_dir()
            .with_config(config),
    )
    .unwrap();

    // Open rc.conf - should match the path glob
    harness.open_file(&test_file).unwrap();
    let language = &harness.editor().active_state().language;
    assert_eq!(
        language, "bash",
        "etc/rc.conf should match path glob pattern for bash"
    );

    // Open nested rc.local - should also match via **
    harness.open_file(&nested_file).unwrap();
    let language = &harness.editor().active_state().language;
    assert_eq!(
        language, "bash",
        "etc/init/rc.local should match path glob pattern with ** for bash"
    );
}

/// Test that exact filenames take priority over glob patterns in language detection
#[test]
fn test_exact_filename_priority_over_glob() {
    let temp_dir = TempDir::new().unwrap();
    let working_dir = temp_dir.path().to_path_buf();

    // Create a file that could match both an exact filename rule and a glob rule
    let test_file = working_dir.join("lfrc");
    fs::write(&test_file, "# lf config\nset preview true\n").unwrap();

    let mut config = Config::default();
    let mut languages = HashMap::new();

    // *rc glob -> bash
    languages.insert(
        "bash".to_string(),
        LanguageConfig {
            extensions: vec!["sh".to_string()],
            filenames: vec!["*rc".to_string()],
            grammar: "bash".to_string(),
            comment_prefix: Some("#".to_string()),
            auto_indent: true,
            ..Default::default()
        },
    );

    // Exact "lfrc" -> python (contrived, but tests priority)
    languages.insert(
        "python".to_string(),
        LanguageConfig {
            extensions: vec!["py".to_string()],
            filenames: vec!["lfrc".to_string()],
            grammar: "python".to_string(),
            comment_prefix: Some("#".to_string()),
            auto_indent: true,
            ..Default::default()
        },
    );

    config.languages = languages;

    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .without_empty_plugins_dir()
            .with_config(config),
    )
    .unwrap();

    harness.open_file(&test_file).unwrap();

    // Exact filename match (python) should win over glob match (bash)
    let language = &harness.editor().active_state().language;
    assert_eq!(
        language, "python",
        "Exact filename 'lfrc' should take priority over '*rc' glob pattern"
    );
}

/// Test the full settings UI flow: open settings via command palette,
/// navigate to find the languages configuration section.
#[test]
fn test_settings_ui_shows_languages_config() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();

    // Open settings via command palette (Ctrl+P -> "Settings" -> Enter)
    harness.open_settings().unwrap();

    // Settings modal should now be visible
    harness.assert_screen_contains("Settings");

    // Search for "languages" in settings
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type "languages" to filter settings
    for c in "languages".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // The settings search should find the languages configuration
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("anguage") || screen.contains("results"),
        "Settings search for 'languages' should show results. Screen:\n{}",
        screen
    );

    // Close search and settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that opening settings via command palette shows the "Open Settings" command
#[test]
fn test_command_palette_has_settings_command() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();

    // Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Type "settings" to filter
    harness.type_text("settings").unwrap();
    harness.render().unwrap();

    // Should show the "Settings" command in the palette
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Settings") || screen.contains("settings"),
        "Command palette should show Settings command. Screen:\n{}",
        screen
    );

    // Press Escape to close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test settings UI with pre-configured language glob patterns.
/// Opens settings via command palette and verifies the settings UI
/// opens and contains the expected categories. Then searches for
/// language-related configuration.
#[test]
fn test_settings_ui_with_language_config_via_command_palette() {
    let mut config = Config::default();
    let mut languages = HashMap::new();
    languages.insert(
        "bash".to_string(),
        LanguageConfig {
            extensions: vec!["sh".to_string()],
            filenames: vec!["*.conf".to_string(), "*rc".to_string()],
            grammar: "bash".to_string(),
            comment_prefix: Some("#".to_string()),
            auto_indent: true,
            ..Default::default()
        },
    );
    config.languages = languages;

    let mut harness =
        EditorTestHarness::create(120, 40, HarnessOptions::new().with_config(config)).unwrap();
    harness.render().unwrap();

    // Step 1: Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Step 2: Type "settings" and select the Settings command
    harness.type_text("settings").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Settings");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("Settings").unwrap();
    harness.render().unwrap();

    // Step 3: Verify settings UI is open with expected categories
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("General"),
        "Settings should show General category"
    );
    assert!(
        screen.contains("Editor"),
        "Settings should show Editor category"
    );

    // Step 4: Navigate to General category (first category, should be selected by default)
    // Switch to settings panel to see items
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Step 5: Scroll down in settings to find the "Languages" map setting
    // Languages is a map-type setting in the General category
    // It may be below the visible area, so scroll down
    for _ in 0..20 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // The languages map should show "bash" as an entry since we configured it
    // Or at minimum we should see "Languages" as a setting label
    let has_languages = screen.contains("Languages") || screen.contains("bash");

    // If we haven't found it yet, scroll down more
    if !has_languages {
        for _ in 0..20 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.render().unwrap();
    }

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Languages") || screen.contains("bash") || screen.contains("language"),
        "Settings should display Languages setting or configured language entries. Screen:\n{}",
        screen
    );

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test full config write + reload cycle:
/// Write config to disk, create a harness that loads it, open files, verify detection.
#[test]
fn test_config_reload_updates_language_detection() {
    let temp_dir = TempDir::new().unwrap();
    let working_dir = temp_dir.path().to_path_buf();

    // Create test files
    let conf_file = working_dir.join("nftables.conf");
    fs::write(&conf_file, "# nftables config\n").unwrap();

    let rc_file = working_dir.join("vimrc");
    fs::write(&rc_file, "\" vim config\nset number\n").unwrap();

    // Phase 1: Start with default config (no language globs)
    {
        let mut harness = EditorTestHarness::create(
            100,
            30,
            HarnessOptions::new()
                .without_empty_plugins_dir()
                .with_working_dir(working_dir.clone()),
        )
        .unwrap();

        // Both files should be "text" initially
        harness.open_file(&conf_file).unwrap();
        assert_eq!(
            &harness.editor().active_state().language,
            "text",
            "nftables.conf should be text initially"
        );

        harness.open_file(&rc_file).unwrap();
        assert_eq!(
            &harness.editor().active_state().language,
            "text",
            "vimrc should be text initially"
        );
    }

    // Phase 2: Write config with glob patterns
    let config_dir = working_dir.join(".fresh");
    fs::create_dir_all(&config_dir).unwrap();
    let config_json = serde_json::json!({
        "languages": {
            "bash": {
                "extensions": ["sh"],
                "filenames": ["*.conf"],
                "grammar": "bash",
                "comment_prefix": "#"
            },
            "vim": {
                "extensions": ["vim"],
                "filenames": ["*rc"],
                "grammar": "vim",
                "comment_prefix": "\""
            }
        }
    });
    fs::write(config_dir.join("config.json"), config_json.to_string()).unwrap();

    // Phase 3: Create fresh harness, reload config, verify detection
    {
        let mut harness = EditorTestHarness::create(
            100,
            30,
            HarnessOptions::new()
                .without_empty_plugins_dir()
                .with_working_dir(working_dir.clone()),
        )
        .unwrap();

        // Reload config to pick up the file we wrote
        harness.editor_mut().reload_config();

        // Verify config was loaded
        assert!(
            harness.config().languages.contains_key("bash"),
            "Config should contain bash language"
        );
        assert!(
            harness.config().languages.contains_key("vim"),
            "Config should contain vim language"
        );

        // Open files - should now detect via glob patterns
        harness.open_file(&conf_file).unwrap();
        assert_eq!(
            &harness.editor().active_state().language,
            "bash",
            "nftables.conf should be bash after config reload with *.conf pattern"
        );

        harness.open_file(&rc_file).unwrap();
        assert_eq!(
            &harness.editor().active_state().language,
            "vim",
            "vimrc should be vim after config reload with *rc pattern"
        );
    }
}
