//! E2E tests for setFileExplorerSlots plugin API.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
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

    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("File Explorer").unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .lines()
                .any(|line| line.contains("foo.txt") && line.contains('◆'))
        })
        .unwrap();
}
