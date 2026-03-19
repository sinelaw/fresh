//! E2E tests for plugin-registered commands appearing in the keybinding editor.
//!
//! Verifies that commands registered via `registerCommand()` show up as
//! bindable actions in the keybinding editor and its autocomplete.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::time::Duration;

/// Helper: write a plugin file, open it, and load it from buffer.
/// Returns the harness after the plugin is loaded.
fn load_plugin_from_buffer(harness: &mut EditorTestHarness, filename: &str, source: &str) {
    let project_dir = harness.project_dir().unwrap();
    let plugin_file = project_dir.join(filename);
    std::fs::write(&plugin_file, source).unwrap();
    harness.open_file(&plugin_file).unwrap();
    harness.render().unwrap();

    // Trigger "Load Plugin from Buffer" via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Load Plugin from Buffer").unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for plugin to load
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    harness.assert_no_plugin_errors();
}

/// Test that a plugin-registered command appears in the keybinding editor
/// when searching by action name.
#[test]
fn test_plugin_command_appears_in_keybinding_editor() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();

    let plugin_source = r#"
const editor = getEditor();
editor.registerCommand(
    "Zephyr Quasar Widget",
    "A unique test command for keybinding editor",
    "zephyr_quasar_widget",
    null
);
editor.setStatus("zqw-plugin loaded");
"#;

    load_plugin_from_buffer(&mut harness, "zqw_plugin.ts", plugin_source);

    // Open the keybinding editor
    harness.editor_mut().open_keybinding_editor();
    harness.render().unwrap();
    harness.assert_screen_contains("Keybinding Editor");

    // Search for the plugin action name
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("zephyr_quasar").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("zephyr_quasar_widget"),
        "Plugin action should appear in keybinding editor search results. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Zephyr Quasar Widget"),
        "Plugin command display name should appear as the description. Screen:\n{}",
        screen
    );
}

/// Test that a plugin-registered command's action name appears in the
/// autocomplete popup when adding a new keybinding.
#[test]
fn test_plugin_command_in_keybinding_editor_autocomplete() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();

    let plugin_source = r#"
const editor = getEditor();
editor.registerCommand(
    "Nexus Prism Beacon",
    "Unique command for autocomplete test",
    "nexus_prism_beacon",
    null
);
editor.setStatus("npb-plugin loaded");
"#;

    load_plugin_from_buffer(&mut harness, "npb_plugin.ts", plugin_source);

    // Open the keybinding editor
    harness.editor_mut().open_keybinding_editor();
    harness.render().unwrap();

    // Press 'a' to add a new binding
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab to the action field
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Type a prefix of the plugin action
    harness.type_text("nexus_prism").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("nexus_prism_beacon"),
        "Plugin action should appear in autocomplete suggestions. Screen:\n{}",
        screen
    );
}

/// Test that the plugin command is grouped under its plugin's section in the
/// keybinding editor (not under Builtin).
#[test]
fn test_plugin_command_grouped_under_plugin_section() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();

    let plugin_source = r#"
const editor = getEditor();
editor.registerCommand(
    "Coral Reef Dolphin",
    "Test plugin section grouping",
    "coral_reef_dolphin",
    null
);
editor.setStatus("crd-plugin loaded");
"#;

    load_plugin_from_buffer(&mut harness, "crd_plugin.ts", plugin_source);

    // Open the keybinding editor
    harness.editor_mut().open_keybinding_editor();
    harness.render().unwrap();

    // Search for the action to filter down to just this plugin
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("coral_reef").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // The plugin section header should show the plugin filename
    assert!(
        screen.contains("crd_plugin.ts"),
        "Plugin section should be named after the plugin file. Screen:\n{}",
        screen
    );
    // And should NOT be under Builtin
    assert!(
        !screen.contains("Builtin"),
        "Plugin command should not appear under Builtin section. Screen:\n{}",
        screen
    );
}
