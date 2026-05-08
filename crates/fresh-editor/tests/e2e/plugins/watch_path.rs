//! E2E tests for the `watchPath` plugin API.
//!
//! Drives the dispatcher directly (rather than loading a JS plugin)
//! so the test is fast and deterministic — the JS-side wiring is
//! exercised by the dts-roundtrip tests in fresh-plugin-runtime.
//! Here we verify the editor-side semantics:
//!
//! - WatchPath returns a numeric handle via WatchPathRegistered.
//! - Filesystem changes under the watched directory produce
//!   AsyncMessage::PathChanged events that the main loop forwards
//!   to the `path_changed` plugin hook.

use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use fresh_core::api::PluginCommand;
use std::time::Duration;

#[test]
fn watch_path_round_trip_registers_and_fires() {
    init_tracing_from_env();
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let watched = project_dir.join("watched");
    std::fs::create_dir_all(&watched).unwrap();

    let request_id = 8001;
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::WatchPath {
            path: watched.clone(),
            recursive: true,
            request_id,
        })
        .unwrap();

    let handle = harness
        .editor()
        .last_watch_response_for_test()
        .expect("WatchPathRegistered should be captured immediately by the dispatcher")
        .1
        .clone()
        .expect("watchPath should succeed for a fresh tmp directory");
    assert!(handle > 0, "handle should be a positive opaque id");

    // Create a file inside the watched directory. notify reports
    // create + (often) modify; we just need at least one
    // PathChanged to surface to the editor.
    let f = watched.join("trigger.txt");
    std::fs::write(&f, "hello").unwrap();

    let mut got_event = false;
    for _ in 0..120 {
        harness.process_async_and_render().unwrap();
        if harness
            .editor()
            .last_path_change_for_test()
            .map(|(_h, p, _k)| p.ends_with("trigger.txt"))
            .unwrap_or(false)
        {
            got_event = true;
            break;
        }
        harness.sleep(Duration::from_millis(25));
    }
    assert!(
        got_event,
        "creating a file under the watched dir should produce a path_changed event; \
         last seen: {:?}",
        harness.editor().last_path_change_for_test()
    );

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::UnwatchPath { handle })
        .unwrap();
}
