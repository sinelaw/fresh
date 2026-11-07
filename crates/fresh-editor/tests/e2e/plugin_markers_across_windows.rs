//! Plugin markers survive a window switch.
//!
//! Plugin markers live in the plugin state snapshot, keyed by buffer id —
//! unique across the editor. The snapshot refresh used to prune them to the
//! *active window's* buffers, meaning "open" and "in the active window" were
//! the same thing to it: the first refresh after switching windows deleted
//! every marker on the other windows' buffers. A plugin that anchored
//! something in window A (Search/Replace's match anchors, #3704) found it gone
//! when the user came back. Markers of a buffer that is closed are still
//! dropped.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

const MARKER_PLUGIN: &str = r#"
const editor = getEditor();
let bufferId = 0;
registerHandler("mk_set", () => {
  bufferId = editor.getActiveBufferId();
  editor.createMarker(bufferId, "probe", 2, 4, null);
  editor.setStatus("MARKER-SET");
});
registerHandler("mk_check", () => {
  const m = editor.getMarker(bufferId, "probe");
  editor.setStatus(m ? `MARKER-AT-${m.start}` : "MARKER-GONE");
});
editor.registerCommand("Marker Probe: Set", "test", "mk_set");
editor.registerCommand("Marker Probe: Check", "test", "mk_check");
"#;

fn run_command(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(command).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn plugin_marker_survives_a_window_switch() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_a = temp_dir.path().join("a");
    let plugins_dir = project_a.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("marker_probe.ts"), MARKER_PLUGIN).unwrap();
    fs::write(project_a.join("a.txt"), "hello marker\n").unwrap();
    let project_b = temp_dir.path().join("b");
    fs::create_dir_all(&project_b).unwrap();
    fs::write(project_b.join("b.txt"), "other window\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        24,
        Default::default(),
        project_a.clone(),
    )
    .unwrap();
    harness.open_file(&project_a.join("a.txt")).unwrap();
    harness.render().unwrap();
    let window_a = harness.editor().active_window_id();

    run_command(&mut harness, "Marker Probe: Set");
    harness
        .wait_until(|h| h.screen_to_string().contains("MARKER-SET"))
        .unwrap();

    // Spend some frames in another window: each refreshes the snapshot.
    let window_b = harness
        .editor_mut()
        .create_window_at(project_b.clone(), "b".to_string());
    harness.editor_mut().set_active_window(window_b);
    harness.open_file(&project_b.join("b.txt")).unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
    }

    harness.editor_mut().set_active_window(window_a);
    harness.render().unwrap();
    run_command(&mut harness, "Marker Probe: Check");
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("MARKER-AT-2") || s.contains("MARKER-GONE")
        })
        .unwrap();
    harness.assert_screen_contains("MARKER-AT-2");
}
