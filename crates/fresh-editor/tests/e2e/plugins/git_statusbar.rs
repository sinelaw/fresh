//! E2E tests for the git_statusbar plugin
//!
//! These tests verify that the status bar can be configured to show the
//! git branch element, which is registered by the git_statusbar plugin.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, StatusBarConfig, StatusBarElement};
use std::fs;

#[test]
fn test_status_bar_shows_custom_branch_token() {
    let mut config = Config::default();
    config.editor.status_bar = StatusBarConfig {
        left: vec![
            StatusBarElement::Filename,
            StatusBarElement::CustomToken("git_statusbar:branch".to_string()),
        ],
        right: vec![StatusBarElement::Encoding, StatusBarElement::Language],
    };

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir_all(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "git_statusbar");
    copy_plugin_lib(&plugins_dir);

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .with_config(config),
    )
    .unwrap();

    // Wait for plugins to load by checking if any commands are registered
    // This ensures the plugin has at least started executing
    harness
        .wait_until(|h| {
            let commands = h.editor().command_registry().read().unwrap().get_all();
            !commands.is_empty()
        })
        .unwrap();

    // Wait for plugin to register custom status bar token
    harness
        .wait_until(|h| {
            h.editor()
                .get_status_bar_elements()
                .iter()
                .any(|(k, _)| k == "{git_statusbar:branch}")
        })
        .unwrap();

    // Verify the custom token is registered by the plugin
    let tokens = harness.editor().get_status_bar_elements();
    assert!(
        tokens
            .iter()
            .any(|(k, t)| k == "{git_statusbar:branch}" && t == "Git: branch"),
        "Custom token should be registered by plugin. Got: {:?}",
        tokens
    );

    // Open settings and verify the custom token appears in status bar config
    harness.open_settings().unwrap();
    // Navigate: General -> Clipboard -> Editor (2 Downs)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // Right to expand Editor section
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    // Navigate down to Status Bar section (12 total from Editor expanded)
    for _ in 0..12 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    // Right to expand Status Bar
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    // Look for the custom token "Git: branch" in the settings body
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Git: branch"),
        "Settings should show custom token 'Git: branch'. Got:\n{}",
        screen
    );

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "test content\n").unwrap();

    harness.open_file(&test_file).unwrap();

    // Move cursor to trigger cursor_moved event
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // The branch token value is published asynchronously by the plugin
    // (after_file_open → spawnProcess("git") → setStatusBarValue). Wait
    // for it to land in the rendered status bar rather than snapshotting
    // once and racing the round-trip.
    harness
        .wait_until(|h| h.get_status_bar().contains("Not in git"))
        .unwrap();
}
