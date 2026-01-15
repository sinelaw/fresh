//! E2E tests for on-save actions (formatters, linters, etc.)
//!
//! These tests verify:
//! - Formatters execute when format_on_save is enabled
//! - On-save actions (linters) execute when files are saved
//! - stdin mode passes buffer content to command stdin
//! - $FILE placeholder substitution
//! - Timeout handling
//! - Action failure handling

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, FormatterConfig, LanguageConfig, OnSaveAction};
use tempfile::TempDir;

/// Test format_on_save with formatter (replaces buffer content)
#[test]
#[cfg_attr(not(unix), ignore = "On-save actions require Unix-like environment")]
fn test_format_on_save() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("unsorted.txt");
    std::fs::write(&file_path, "cherry\napple\nbanana\n").unwrap();

    // Configure formatter: sort the file content (stdin -> stdout)
    let formatter = FormatterConfig {
        command: "sort".to_string(),
        args: vec![],
        stdin: true,
        timeout_ms: 5000,
    };

    // Create config for "plaintext" language (matches .txt files)
    let mut config = Config::default();
    config.languages.insert(
        "plaintext".to_string(),
        LanguageConfig {
            extensions: vec!["txt".to_string()],
            filenames: vec![],
            grammar: "plaintext".to_string(),
            comment_prefix: None,
            auto_indent: false,
            highlighter: Default::default(),
            textmate_grammar: None,
            show_whitespace_tabs: true,
            use_tabs: false,
            tab_size: None,
            formatter: Some(formatter),
            format_on_save: true,
            on_save: vec![],
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_dir).unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify initial content
    harness.assert_buffer_content("cherry\napple\nbanana\n");

    // Save the file (triggers format_on_save)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Buffer content should be sorted by the formatter
    harness.assert_buffer_content("apple\nbanana\ncherry\n");

    // Status should indicate save
    harness.assert_screen_contains("Saved");
}

/// Test on-save action (linter-style, just runs the command)
#[test]
#[cfg_attr(not(unix), ignore = "On-save actions require Unix-like environment")]
fn test_on_save_linter_style() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("test.txt");
    std::fs::write(&file_path, "original content\n").unwrap();

    // Configure on-save action: just run true (succeeds without modifying)
    let action = OnSaveAction {
        command: "true".to_string(),
        args: vec![],
        working_dir: None,
        stdin: false,
        timeout_ms: 5000,
        enabled: true,
    };

    let mut config = Config::default();
    config.languages.insert(
        "plaintext".to_string(),
        LanguageConfig {
            extensions: vec!["txt".to_string()],
            filenames: vec![],
            grammar: "plaintext".to_string(),
            comment_prefix: None,
            auto_indent: false,
            highlighter: Default::default(),
            textmate_grammar: None,
            show_whitespace_tabs: true,
            use_tabs: false,
            tab_size: None,
            formatter: None,
            format_on_save: false,
            on_save: vec![action],
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_dir).unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify initial content
    harness.assert_buffer_content("original content\n");

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Content should be unchanged (linter doesn't modify)
    harness.assert_buffer_content("original content\n");

    // Save should succeed
    harness.assert_screen_contains("Saved");
}

/// Test on-save action failure handling
#[test]
#[cfg_attr(not(unix), ignore = "On-save actions require Unix-like environment")]
fn test_on_save_action_failure() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("test.txt");
    std::fs::write(&file_path, "content\n").unwrap();

    // Configure on-save action that will fail
    let action = OnSaveAction {
        command: "false".to_string(), // Always exits with code 1
        args: vec![],
        working_dir: None,
        stdin: false,
        timeout_ms: 5000,
        enabled: true,
    };

    let mut config = Config::default();
    config.languages.insert(
        "plaintext".to_string(),
        LanguageConfig {
            extensions: vec!["txt".to_string()],
            filenames: vec![],
            grammar: "plaintext".to_string(),
            comment_prefix: None,
            auto_indent: false,
            highlighter: Default::default(),
            textmate_grammar: None,
            show_whitespace_tabs: true,
            use_tabs: false,
            tab_size: None,
            formatter: None,
            format_on_save: false,
            on_save: vec![action],
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_dir).unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add some content to make the buffer modified
    harness.type_text("extra").unwrap();
    harness.render().unwrap();

    // Save the file (will fail due to on-save action)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Status should show error
    harness.assert_screen_contains("failed");

    // But the file itself should still be saved (on-save failure doesn't prevent save)
    let disk_content = std::fs::read_to_string(&file_path).unwrap();
    assert!(
        disk_content.contains("extra"),
        "File should be saved despite on-save action failure"
    );
}

/// Test on-save action with $FILE placeholder
/// The $FILE placeholder is substituted by the on-save system before shell execution
#[test]
#[cfg_attr(not(unix), ignore = "On-save actions require Unix-like environment")]
fn test_on_save_file_placeholder() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("test.txt");
    std::fs::write(&file_path, "original\n").unwrap();

    // Create a marker file that the on-save action will create
    let marker_path = project_dir.join("marker.txt");

    // Configure on-save action that uses $FILE
    let action = OnSaveAction {
        command: "cp".to_string(),
        args: vec!["$FILE".to_string(), marker_path.display().to_string()],
        working_dir: None,
        stdin: false,
        timeout_ms: 5000,
        enabled: true,
    };

    let mut config = Config::default();
    config.languages.insert(
        "plaintext".to_string(),
        LanguageConfig {
            extensions: vec!["txt".to_string()],
            filenames: vec![],
            grammar: "plaintext".to_string(),
            comment_prefix: None,
            auto_indent: false,
            highlighter: Default::default(),
            textmate_grammar: None,
            show_whitespace_tabs: true,
            use_tabs: false,
            tab_size: None,
            formatter: None,
            format_on_save: false,
            on_save: vec![action],
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_dir).unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify buffer to trigger save
    harness.type_text("x").unwrap();
    harness.render().unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that marker file was created by copying the source file
    assert!(marker_path.exists(), "Marker file should be created");
    let marker_content = std::fs::read_to_string(&marker_path).unwrap();
    // The marker should contain the modified content from the saved file
    assert!(
        marker_content.contains("original") || marker_content.contains("x"),
        "Marker should contain content from the file: {}",
        marker_content
    );
}

/// Test formatter with stdin mode (passes buffer content as stdin)
#[test]
#[cfg_attr(not(unix), ignore = "On-save actions require Unix-like environment")]
fn test_formatter_stdin_mode() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("uppercase.txt");
    std::fs::write(&file_path, "hello world\n").unwrap();

    // Configure formatter: convert to uppercase via stdin
    let formatter = FormatterConfig {
        command: "tr".to_string(),
        args: vec!["a-z".to_string(), "A-Z".to_string()],
        stdin: true,
        timeout_ms: 5000,
    };

    let mut config = Config::default();
    config.languages.insert(
        "plaintext".to_string(),
        LanguageConfig {
            extensions: vec!["txt".to_string()],
            filenames: vec![],
            grammar: "plaintext".to_string(),
            comment_prefix: None,
            auto_indent: false,
            highlighter: Default::default(),
            textmate_grammar: None,
            show_whitespace_tabs: true,
            use_tabs: false,
            tab_size: None,
            formatter: Some(formatter),
            format_on_save: true,
            on_save: vec![],
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_dir).unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify initial content
    harness.assert_buffer_content("hello world\n");

    // Save the file (triggers formatter)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Buffer content should be uppercase
    harness.assert_buffer_content("HELLO WORLD\n");
}

/// Test that on-save action failure stops subsequent actions
#[test]
#[cfg_attr(not(unix), ignore = "On-save actions require Unix-like environment")]
fn test_on_save_stops_on_failure() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("test.txt");
    std::fs::write(&file_path, "hello\n").unwrap();

    let marker_path = project_dir.join("should_not_exist.txt");

    // Configure multiple on-save actions:
    // 1. Fail
    // 2. Create marker (should NOT run due to failure above)
    let action1 = OnSaveAction {
        command: "false".to_string(),
        args: vec![],
        working_dir: None,
        stdin: false,
        timeout_ms: 5000,
        enabled: true,
    };

    let action2 = OnSaveAction {
        command: "touch".to_string(),
        args: vec![marker_path.display().to_string()],
        working_dir: None,
        stdin: false,
        timeout_ms: 5000,
        enabled: true,
    };

    let mut config = Config::default();
    config.languages.insert(
        "plaintext".to_string(),
        LanguageConfig {
            extensions: vec!["txt".to_string()],
            filenames: vec![],
            grammar: "plaintext".to_string(),
            comment_prefix: None,
            auto_indent: false,
            highlighter: Default::default(),
            textmate_grammar: None,
            show_whitespace_tabs: true,
            use_tabs: false,
            tab_size: None,
            formatter: None,
            format_on_save: false,
            on_save: vec![action1, action2],
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_dir).unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify buffer
    harness.type_text("x").unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The second action should NOT have run (marker file should not exist)
    assert!(
        !marker_path.exists(),
        "Marker file should NOT be created because first action failed"
    );
}

/// Test with no actions configured (should just save normally)
#[test]
fn test_on_save_no_actions_configured() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("test.rs");
    std::fs::write(&file_path, "fn main() {}\n").unwrap();

    // Use default config (format_on_save is disabled by default)
    let config = Config::default();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_dir).unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify buffer
    harness.type_text("// comment\n").unwrap();
    harness.render().unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should save normally without any issues
    harness.assert_screen_contains("Saved");
}

/// Test formatter not found shows helpful message
#[test]
#[cfg_attr(not(unix), ignore = "On-save actions require Unix-like environment")]
fn test_formatter_not_found_shows_message() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("test.txt");
    std::fs::write(&file_path, "content\n").unwrap();

    // Configure a formatter with a non-existent command
    let formatter = FormatterConfig {
        command: "nonexistent_formatter_xyz_12345".to_string(),
        args: vec![],
        stdin: true,
        timeout_ms: 5000,
    };

    let mut config = Config::default();
    config.languages.insert(
        "plaintext".to_string(),
        LanguageConfig {
            extensions: vec!["txt".to_string()],
            filenames: vec![],
            grammar: "plaintext".to_string(),
            comment_prefix: None,
            auto_indent: false,
            highlighter: Default::default(),
            textmate_grammar: None,
            show_whitespace_tabs: true,
            use_tabs: false,
            tab_size: None,
            formatter: Some(formatter),
            format_on_save: true,
            on_save: vec![],
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_dir).unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify buffer
    harness.type_text("x").unwrap();
    harness.render().unwrap();

    // Save the file - should show a message about missing formatter
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Content should be unchanged (formatter didn't run)
    harness.assert_buffer_content("xcontent\n");

    // Should show a message about missing formatter
    harness.assert_screen_contains("Formatter");
}
