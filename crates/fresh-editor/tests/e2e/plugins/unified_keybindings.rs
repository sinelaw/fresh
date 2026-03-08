//! E2E tests for unified plugin keybindings
//!
//! Verifies that plugin mode bindings are dual-registered into KeybindingResolver,
//! appear in the keybinding editor with "Plugin" source, and mode context is visible.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test the full unified keybindings flow:
/// 1. An ad-hoc plugin defines a mode with keybindings
/// 2. The keybinding editor shows them with "Plugin" source filter
/// 3. The mode context "mode:tst-unikeys" is visible
#[test]
fn test_plugin_mode_bindings_in_keybinding_editor() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(project_root.clone()).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    // Ad-hoc plugin that defines a mode with keybindings and activates it
    let test_plugin = r###"
const editor = getEditor();

editor.defineMode("tst-unikeys", null, [
    ["Enter", "test_confirm_action"],
    ["d", "test_delete_action"],
    ["q", "test_quit_action"],
], true, false);

editor.setStatus("Unified keybinding test ready");
"###;

    let plugin_path = plugins_dir.join("test_unified_keys.ts");
    fs::write(&plugin_path, test_plugin).unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "hello world\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Wait for the plugin to load
    harness
        .wait_for_screen_contains("Unified keybinding test ready")
        .unwrap();

    // Open the keybinding editor
    harness.editor_mut().open_keybinding_editor();
    harness.render().unwrap();
    harness.assert_screen_contains("Keybinding Editor");

    // Cycle source filter to "Plugin" (All -> Custom -> Keymap -> Plugin)
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Should show "Plugin" as the current source filter
    harness.assert_screen_contains("Plugin");

    // Should show our plugin mode bindings with the mode context
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("mode:tst-unikeys"),
        "Keybinding editor should show the mode context. Screen:\n{}",
        screen
    );

    // Should show at least one of the action names from our plugin
    assert!(
        screen.contains("test_confirm_action")
            || screen.contains("test_delete_action")
            || screen.contains("test_quit_action"),
        "Keybinding editor should show plugin action names. Screen:\n{}",
        screen
    );
}
