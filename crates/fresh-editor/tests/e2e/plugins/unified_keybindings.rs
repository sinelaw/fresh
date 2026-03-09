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

/// Test expanding a collapsed plugin section, editing a binding, saving,
/// and verifying the list renders correctly with section headers intact.
#[test]
fn test_expand_plugin_section_edit_and_save() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(project_root.clone()).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    let test_plugin = r###"
const editor = getEditor();

editor.defineMode("tst-sections", null, [
    ["Enter", "sect_confirm"],
    ["d", "sect_delete"],
    ["q", "sect_quit"],
], true, false);

editor.setStatus("Section test ready");
"###;

    let plugin_path = plugins_dir.join("test_sections.ts");
    fs::write(&plugin_path, test_plugin).unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "test content\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();
    harness
        .wait_for_screen_contains("Section test ready")
        .unwrap();

    // Open the keybinding editor
    harness.editor_mut().open_keybinding_editor();
    harness.render().unwrap();
    harness.assert_screen_contains("Keybinding Editor");

    // The Builtin section should be expanded, plugin section collapsed
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Builtin"),
        "Should show Builtin section header"
    );
    // Plugin bindings should NOT be visible (collapsed)
    assert!(
        !screen.contains("sect_confirm"),
        "Plugin bindings should be hidden when collapsed. Screen:\n{}",
        screen
    );

    // Navigate to the plugin section header (End jumps to last row)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate up until we find the test_sections header selected
    for _ in 0..5 {
        let s = harness.screen_to_string();
        if s.lines()
            .any(|l| l.contains(">") && l.contains("test_sections"))
        {
            break;
        }
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // Press Enter to expand the section
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Navigate down to the first binding in the expanded section
    // (this also scrolls the viewport to show the binding)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Plugin bindings should now be visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("sect_confirm")
            || screen.contains("sect_delete")
            || screen.contains("sect_quit"),
        "Plugin bindings should be visible after expanding. Screen:\n{}",
        screen
    );

    // Press Enter to open the edit dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Keybinding");

    // Tab to Save button (Key -> Action -> Context -> Save = 3 Tabs)
    for _ in 0..3 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Press Enter to save (saves the binding as-is, creating a custom override)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Edit dialog should be closed
    harness.assert_screen_not_contains("Edit Keybinding");
    // Keybinding editor should still be open
    harness.assert_screen_contains("Keybinding Editor");

    // Collapse all sections by going Home and pressing Enter on Builtin header
    // to collapse it, so all section headers fit on screen for inspection
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    // Collapse Builtin section (selected at top)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // With all sections collapsed, the full header list should be visible
    let screen = harness.screen_to_string();

    // BUG CHECK: After editing a plugin binding and saving, the edited binding
    // should stay in its plugin section. It must NOT create a duplicate Builtin
    // section (which happens when apply_edit_dialog sets plugin_name: None).
    let builtin_count = screen.lines().filter(|l| l.contains("Builtin")).count();
    assert!(
        builtin_count <= 1,
        "There should be at most one Builtin section header, but found {}. \
         Editing a plugin binding must not create a duplicate Builtin section. Screen:\n{}",
        builtin_count,
        screen
    );

    // The plugin section should appear exactly once
    let plugin_count = screen
        .lines()
        .filter(|l| l.contains("test_sections"))
        .count();
    assert_eq!(
        plugin_count, 1,
        "There should be exactly one test_sections section header, but found {}. Screen:\n{}",
        plugin_count, screen
    );
}
