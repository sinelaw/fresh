//! E2E test reproducing the bug where keybindings bound to plugin-registered
//! commands do not actually execute the command when pressed.
//!
//! The command palette path works because it uses pre-constructed
//! `Action::PluginAction` objects from the `CommandRegistry`.  However, when
//! keybindings are loaded from config, `Action::from_str()` returns `None` for
//! unknown (plugin) action strings, silently discarding the binding.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::time::Duration;

/// Regression test: a custom keybinding for a plugin command must actually
/// execute the command when the key is pressed.
///
/// The plugin action inserts a unique marker string into the editor buffer.
/// We open a blank file (not the plugin source) and press the keybinding,
/// then verify the marker appears in the buffer content on screen.
#[test]
fn test_plugin_command_executes_via_keybinding() {
    // Configure a keybinding that maps Alt+M to our plugin action
    let mut config = fresh::config::Config::default();
    config.keybindings.push(fresh::config::Keybinding {
        key: "m".to_string(),
        modifiers: vec!["alt".to_string()],
        keys: vec![],
        action: "marker_insert_action".to_string(),
        args: std::collections::HashMap::new(),
        when: None,
    });

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 40, config).unwrap();

    // Plugin registers a command with a callback that inserts text into the buffer
    let plugin_source = r#"
const editor = getEditor();

globalThis.marker_insert_action = function(): void {
    const bufferId = editor.getActiveBufferId();
    if (bufferId !== null && bufferId !== undefined) {
        editor.insertText(bufferId, 0, "XYZZY_KEYBIND_MARKER");
    }
};

editor.registerCommand(
    "Insert Marker",
    "Insert a marker string at the start of the buffer",
    "marker_insert_action",
    null
);

editor.setStatus("marker-plugin-loaded");
"#;

    // Write and load the plugin
    let project_dir = harness.project_dir().unwrap();
    let plugin_file = project_dir.join("marker_plugin.ts");
    std::fs::write(&plugin_file, plugin_source).unwrap();
    harness.open_file(&plugin_file).unwrap();
    harness.render().unwrap();

    // Load plugin from buffer via command palette
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

    // Now open a blank test file (so we're not looking at the plugin source)
    let test_file = project_dir.join("test_target.txt");
    std::fs::write(&test_file, "hello world\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Verify we're on the test file and it doesn't contain our marker yet
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("XYZZY_KEYBIND_MARKER"),
        "Marker should not be present before pressing keybinding. Screen:\n{}",
        screen
    );

    // Press Alt+M — the keybinding configured for our plugin command
    harness
        .send_key(KeyCode::Char('m'), KeyModifiers::ALT)
        .unwrap();

    // Process async messages so the plugin action executes
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("XYZZY_KEYBIND_MARKER"),
        "Pressing Alt+M should have executed the plugin command via keybinding, \
         inserting 'XYZZY_KEYBIND_MARKER' into the buffer. Screen:\n{}",
        screen
    );
}
