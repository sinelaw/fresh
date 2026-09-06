//! E2E tests for setFileExplorerSlots plugin API.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

#[test]
fn test_file_explorer_slots_leading_override_renders() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();
    fs::write(project_root.join("foo.txt"), "hello").unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    let test_plugin = r###"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

editor.on("editor_initialized", () => {
  const path = editor.pathJoin(editor.getCwd(), "foo.txt");
  editor.setFileExplorerSlots("test-slots", [{
    path,
    leading: {
      text: "◆",
      color: "ui.syntax.keyword",
      minWidth: 2,
    },
    priority: 99,
  }]);
});
"###;
    fs::write(plugins_dir.join("file_explorer_slots_test.ts"), test_plugin).unwrap();

    let mut config = Config::default();
    config.plugins.insert(
        "file_explorer_slots_test".to_string(),
        fresh_core::config::PluginConfig {
            enabled: true,
            path: Some(plugins_dir.join("file_explorer_slots_test.ts")),
            ..Default::default()
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, project_root.clone())
            .unwrap();

    // Drain any plugin commands queued during `editor_initialized` (for
    // example `setFileExplorerSlots`) before we open the explorer.
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("File Explorer").unwrap();
    harness.wait_for_screen_contains("foo.txt").unwrap();
    harness
        .wait_until_stable(|h| {
            h.screen_to_string()
                .lines()
                .any(|line| line.contains("foo.txt") && line.contains('◆'))
        })
        .unwrap();
}

#[test]
fn test_path_independent_leading_rules_render_and_exact_paths_win() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();
    fs::write(project_root.join("rule.rs"), "").unwrap();
    fs::write(project_root.join("override.rs"), "").unwrap();
    fs::write(project_root.join("suppressed.rs"), "").unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    let test_plugin = r###"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

editor.on("editor_initialized", () => {
  editor.setFileExplorerLeadingSlotRules("rules", {
    extensions: {
      rs: { text: "◇", color: { source: "filename" }, minWidth: 1 },
    },
    fallbackFile: { text: "F", color: "syntax.string" },
  });
  editor.setFileExplorerSlots("exact", [
    {
      path: editor.pathJoin(editor.getCwd(), "override.rs"),
      leading: { text: "◆", color: "syntax.keyword", minWidth: 1 },
      priority: -100,
    },
    {
      path: editor.pathJoin(editor.getCwd(), "suppressed.rs"),
      suppressLeading: true,
      priority: -100,
    },
  ]);
});
"###;
    let plugin_path = plugins_dir.join("file_explorer_rules_test.ts");
    fs::write(&plugin_path, test_plugin).unwrap();

    let mut config = Config::default();
    config.plugins.insert(
        "file_explorer_rules_test".to_string(),
        fresh_core::config::PluginConfig {
            enabled: true,
            path: Some(plugin_path),
            ..Default::default()
        },
    );
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, project_root).unwrap();
    harness.editor_mut().process_async_messages();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("suppressed.rs").unwrap();

    let rows: Vec<_> = harness
        .screen_to_string()
        .lines()
        .map(str::to_owned)
        .collect();
    let row = |name: &str| rows.iter().find(|row| row.contains(name)).unwrap();
    assert!(row("rule.rs").contains('◇'));
    assert!(row("override.rs").contains('◆'));
    assert!(!row("override.rs").contains('◇'));
    assert!(!row("suppressed.rs").contains('◇'));
}

#[test]
fn test_bundled_file_icons_plugin_honors_nerd_font_setting() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();
    fs::write(project_root.join("main.rs"), "fn main() {}\n").unwrap();
    fs::create_dir(project_root.join("src")).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "file_icons");

    let mut config = Config::default();
    config.editor.nerd_font_icons = true;
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, project_root).unwrap();
    harness.editor_mut().process_async_messages();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("main.rs").unwrap();

    let rows: Vec<_> = harness
        .screen_to_string()
        .lines()
        .map(str::to_owned)
        .collect();
    let row = |name: &str| rows.iter().find(|row| row.contains(name)).unwrap();
    assert!(row("main.rs").contains('\u{e7a8}'));
    assert!(row("src").contains('\u{f07b}'));
}

#[test]
fn test_bundled_file_icons_plugin_preserves_default_output_when_disabled() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();
    fs::write(project_root.join("main.rs"), "").unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "file_icons");

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), project_root)
            .unwrap();
    harness.editor_mut().process_async_messages();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("main.rs").unwrap();

    let row = harness
        .screen_to_string()
        .lines()
        .find(|row| row.contains("main.rs"))
        .unwrap()
        .to_string();
    assert!(!row.contains('\u{e7a8}'));
}

#[test]
fn test_leading_rules_apply_to_windows_created_after_registration() {
    let project_a = tempfile::TempDir::new().unwrap();
    let project_b = tempfile::TempDir::new().unwrap();
    fs::write(project_b.path().join("future.rs"), "").unwrap();

    let plugins_dir = project_a.path().join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    let plugin_path = plugins_dir.join("global_rules_test.ts");
    fs::write(
        &plugin_path,
        r###"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
editor.on("editor_initialized", () => {
  editor.setFileExplorerLeadingSlotRules("global", {
    extensions: { rs: { text: "G", color: "syntax.keyword", minWidth: 1 } },
  });
});
"###,
    )
    .unwrap();

    let mut config = Config::default();
    config.plugins.insert(
        "global_rules_test".to_string(),
        fresh_core::config::PluginConfig {
            enabled: true,
            path: Some(plugin_path),
            ..Default::default()
        },
    );
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        config,
        project_a.path().to_path_buf(),
    )
    .unwrap();
    harness.editor_mut().process_async_messages();

    let future = harness
        .editor_mut()
        .create_window_at(project_b.path().to_path_buf(), "future".into());
    harness.editor_mut().set_active_window(future);
    harness.editor_mut().toggle_file_explorer();
    harness.wait_for_screen_contains("future.rs").unwrap();

    assert!(harness
        .screen_to_string()
        .lines()
        .any(|row| row.contains("future.rs") && row.contains('G')));
}
