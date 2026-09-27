//! The plugin state snapshot is rebuilt when something happens, not on every
//! pass of the event loop, and plugins still read current state.
//!
//! Counted with `PerfCounters` rather than timed: an idle pass must add no
//! rebuild and no environment probe, whatever the machine.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::time::{Duration, Instant};

/// Records what the plugin API reports each time the cursor moves or text
/// is inserted.
const PROBE_PLUGIN: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

function record(): void {
  const id = editor.getActiveBufferId();
  const info = editor.getBufferInfo(id);
  editor.setGlobalState("seen", {
    cursor: editor.getCursorPosition(),
    length: info ? info.length : -1,
    env: editor.detectedEnv(),
  });
}
editor.on("cursor_moved", record);
editor.on("after_insert", record);
"#;

struct Setup {
    dir: tempfile::TempDir,
    root: std::path::PathBuf,
    harness: EditorTestHarness,
}

fn setup() -> Setup {
    let dir = tempfile::TempDir::new().unwrap();
    let root = dir.path().canonicalize().unwrap();
    fs::write(root.join("notes.txt"), "alpha\nbeta\ngamma\n").unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("snapshot_probe.ts"), PROBE_PLUGIN).unwrap();

    let mut config = Config::default();
    config.plugins.insert(
        "snapshot_probe".to_string(),
        fresh_core::config::PluginConfig {
            enabled: true,
            path: Some(plugins_dir.join("snapshot_probe.ts")),
            ..Default::default()
        },
    );
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, config, root.clone()).unwrap();
    harness.open_file(&root.join("notes.txt")).unwrap();
    harness.render().unwrap();
    Setup { dir, root, harness }
}

/// What the probe plugin last recorded, once it has recorded `cursor`.
fn seen_at(harness: &mut EditorTestHarness, cursor: usize) -> serde_json::Value {
    for _ in 0..500 {
        harness.tick_and_render().unwrap();
        if let Some(v) = seen(harness).filter(|v| v["cursor"].as_u64() == Some(cursor as u64)) {
            return v;
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    panic!(
        "the probe never saw cursor {cursor}; last saw {:?}",
        seen(harness)
    );
}

fn seen(harness: &EditorTestHarness) -> Option<serde_json::Value> {
    harness
        .editor()
        .plugin_global_state()
        .get("snapshot_probe")
        .and_then(|m| m.get("seen"))
        .cloned()
}

/// Loop passes as the TUI, GUI and web loops run them between frames:
/// `editor_tick`, then a render whenever it asks for one.
fn idle_for(harness: &mut EditorTestHarness, duration: Duration) {
    let until = Instant::now() + duration;
    while Instant::now() < until {
        if fresh::app::editor_tick(harness.editor_mut(), || Ok(())).unwrap() {
            harness.render().unwrap();
        }
        std::thread::sleep(Duration::from_millis(5));
    }
}

#[test]
fn idle_loop_passes_leave_the_plugin_snapshot_alone() {
    let Setup {
        dir: _dir,
        mut harness,
        root,
    } = setup();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    seen_at(&mut harness, 1);

    // Settle whatever the key, its hook and the file's own watch left in
    // flight: quiet for a whole window.
    let mut settled = harness.editor().perf_counters();
    for _ in 0..20 {
        idle_for(&mut harness, Duration::from_millis(300));
        let now = harness.editor().perf_counters();
        if now.plugin_snapshot_rebuilds == settled.plugin_snapshot_rebuilds {
            break;
        }
        settled = now;
    }

    // Activity at the root that cannot change the environment, like a log
    // a dev server keeps appending to.
    for i in 0..5 {
        fs::write(root.join("server.log"), format!("line {i}\n")).unwrap();
        idle_for(&mut harness, Duration::from_millis(50));
    }
    idle_for(&mut harness, Duration::from_millis(300));

    let after = harness.editor().perf_counters();
    assert_eq!(
        after.plugin_snapshot_rebuilds, settled.plugin_snapshot_rebuilds,
        "idle loop passes rebuilt the plugin snapshot"
    );
    assert_eq!(
        after.env_detections, settled.env_detections,
        "idle loop passes probed the workspace environment"
    );
    assert!(
        settled.env_detections > 0,
        "the environment was never detected, so this test measures nothing"
    );
}

#[test]
fn plugins_read_current_state_after_moves_and_edits() {
    let Setup {
        dir: _dir,
        mut harness,
        ..
    } = setup();
    let before = harness.editor().perf_counters();

    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    let seen = seen_at(&mut harness, 3);
    assert_eq!(seen["length"], 17, "{seen}");

    harness.type_text("XY").unwrap();
    assert_eq!(harness.cursor_position(), 5);
    let seen = seen_at(&mut harness, 5);
    assert_eq!(
        seen["length"], 19,
        "an edit must reach getBufferInfo: {seen}"
    );

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let cursor = harness.cursor_position();
    seen_at(&mut harness, cursor);

    // Rebuilds happened for every change, but the environment was probed
    // once: nothing at the root changed.
    let after = harness.editor().perf_counters();
    assert!(after.plugin_snapshot_rebuilds > before.plugin_snapshot_rebuilds);
    assert!(
        after.env_detections - before.env_detections <= 1,
        "detect_env re-ran on unrelated changes: {} probes",
        after.env_detections - before.env_detections
    );
}

#[test]
fn a_new_marker_at_the_root_reaches_detected_env() {
    let Setup {
        dir: _dir,
        mut harness,
        root,
    } = setup();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(seen_at(&mut harness, 1)["env"], "");

    fs::write(root.join(".envrc"), "export FOO=1\n").unwrap();

    // The root watch reports the file off-thread; nudge the cursor so the
    // probe reads `detectedEnv()` again until the report has landed.
    for step in 0..400 {
        let key = if step % 2 == 0 {
            KeyCode::Right
        } else {
            KeyCode::Left
        };
        harness.send_key(key, KeyModifiers::NONE).unwrap();
        let cursor = harness.cursor_position();
        let seen = seen_at(&mut harness, cursor);
        if seen["env"].as_str().is_some_and(|e| e.contains("direnv")) {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(25));
    }
    panic!("detectedEnv() never reported the new .envrc");
}
