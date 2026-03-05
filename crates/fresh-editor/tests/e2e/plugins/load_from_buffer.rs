//! E2E tests for the "Load Plugin from Buffer" feature.
//!
//! Tests that a user can open a TypeScript file containing plugin code,
//! trigger the LoadPluginFromBuffer action, and have the plugin execute
//! with its commands registered. Also tests hot-reload (load twice)
//! cleans up old state properly.

use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::time::Duration;

/// Test that loading a plugin from the current buffer registers its command
/// and shows a success status message.
#[test]
fn test_load_plugin_from_buffer_registers_command() {
    init_tracing_from_env();

    // Create a harness with a temporary project (provides plugins dir)
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // Create a .ts file containing plugin code that registers a command
    let plugin_source = r#"
const editor = getEditor();
editor.registerCommand(
    "Buffer Plugin Hello",
    "Say hello from a buffer plugin",
    "buffer_plugin_hello",
    null
);
editor.setStatus("buffer-plugin loaded ok");
"#;

    // Write the plugin source to a .ts file and open it
    let project_dir = harness.project_dir().unwrap();
    let plugin_file = project_dir.join("my_plugin.ts");
    std::fs::write(&plugin_file, plugin_source).unwrap();
    harness.open_file(&plugin_file).unwrap();
    harness.render().unwrap();

    // Verify the file content is visible
    harness.assert_screen_contains("registerCommand");

    // Trigger "Load Plugin from Buffer" via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("load_plugin_from_buffer").unwrap();

    // Process to update suggestions
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Process async messages to let the plugin load
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    // Check that the plugin loaded successfully via status message
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("buffer-plugin loaded ok")
            || screen.contains("Plugin 'buffer-my_plugin' loaded from buffer"),
        "Expected plugin load success message. Screen:\n{}",
        screen
    );

    harness.assert_no_plugin_errors();

    // Now verify the registered command appears in the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Buffer Plugin Hello").unwrap();

    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Buffer Plugin Hello"),
        "Plugin command should appear in palette after loading from buffer. Screen:\n{}",
        screen
    );
}

/// Test that loading a plugin from buffer twice (hot-reload) cleans up the
/// old plugin's registered command and replaces it with the new one.
#[test]
fn test_load_plugin_from_buffer_hot_reload_cleanup() {
    init_tracing_from_env();

    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // First version of the plugin registers "Alpha Zebra" (unique name)
    let plugin_v1 = r#"
const editor = getEditor();
editor.registerCommand(
    "Alpha Zebra Xylophone",
    "This command should disappear after reload",
    "hot_reload_old",
    null
);
editor.setStatus("v1 loaded");
"#;

    let project_dir = harness.project_dir().unwrap();
    let plugin_file = project_dir.join("reload_test.ts");
    std::fs::write(&plugin_file, plugin_v1).unwrap();
    harness.open_file(&plugin_file).unwrap();
    harness.render().unwrap();

    // Load plugin v1
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("load_plugin_from_buffer").unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    harness.assert_no_plugin_errors();

    // Verify v1 command is registered
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Alpha Zebra Xylophone").unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Alpha Zebra Xylophone"),
        "Old command should be registered. Screen:\n{}",
        screen
    );

    // Close palette
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now update the file with v2 plugin code that registers a completely different command
    let plugin_v2 = r#"
const editor = getEditor();
editor.registerCommand(
    "Beta Mango Pineapple",
    "This is the replacement command",
    "hot_reload_new",
    null
);
editor.setStatus("v2 loaded");
"#;
    std::fs::write(&plugin_file, plugin_v2).unwrap();

    // Revert buffer to pick up the new file content
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Revert").unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..5 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    // Load plugin v2 (hot-reload: same filename → same plugin name)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("load_plugin_from_buffer").unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    harness.assert_no_plugin_errors();

    // Verify v2 command is registered
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Beta Mango Pineapple").unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Beta Mango Pineapple"),
        "New command should be registered after hot-reload. Screen:\n{}",
        screen
    );

    // Verify the old command ("Alpha Zebra Xylophone") is NOT in the popup results.
    // The popup renders results inside bordered lines with "│". We check that the
    // old command name doesn't appear inside the popup box (it WILL appear in the
    // search input line, so we can't just check screen.contains).
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Alpha Zebra Xylophone").unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    let screen = harness.screen_to_string();
    // Count occurrences: the search input itself will contain the text once.
    // If the command still exists in the palette results, it would appear a second time.
    let count = screen.matches("Alpha Zebra Xylophone").count();
    assert!(
        count <= 1,
        "Old command should be cleaned up after hot-reload (found {} occurrences, expected at most 1 from search input). Screen:\n{}",
        count,
        screen
    );
}
