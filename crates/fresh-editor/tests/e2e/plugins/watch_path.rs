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

    // Create a file inside the watched directory. notify's backend decides
    // whether the surfaced event names the created file or the watched
    // directory itself — Linux inotify reports the directory (kind "other") —
    // and how many events coalesce. So the only stable contract is the one the
    // plugin hook guarantees: *some* path_changed fires for our handle under
    // the watched dir. Wait indefinitely (semantic wait); nextest bounds it
    // externally. A fixed timeout here is the source of the CI flake.
    let f = watched.join("trigger.txt");
    std::fs::write(&f, "hello").unwrap();

    harness
        .wait_until(|h| {
            h.editor()
                .path_changes_for_test()
                .iter()
                .any(|(evt_handle, path, _kind)| {
                    *evt_handle == handle && path.starts_with(&watched)
                })
        })
        .unwrap();

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::UnwatchPath { handle })
        .unwrap();
}
