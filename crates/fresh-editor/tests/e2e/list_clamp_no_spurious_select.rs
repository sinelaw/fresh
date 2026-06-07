//! A List widget's keyboard navigation must NOT fire a `select`
//! `widget_event` for a move that is clamped at the list's top/bottom
//! edge — i.e. when the selection doesn't actually change.
//!
//! Regression for the host bug in
//! `widget_runtime::handle_widget_select_move_for_key`: it fired a
//! `select` event on every Up/Down, including no-op moves at a list
//! boundary. Holding ↓ against the bottom (or ↑ against the top) then
//! spammed the plugin with same-index selections; in the Orchestrator
//! dock each spurious event scheduled a redundant live-switch (the
//! `fromEdge=null` "heartbeat" seen in the dock-switch traces). The Tree
//! handler already guarded this ("No change → bail"); the List handler
//! did not.
//!
//! The `test_list_clamp_select.ts` plugin mounts a 3-item focusable List
//! and a `SELECTS=<n>` counter that ticks once per received `select`
//! event. We arrow to the bottom, keep pressing ↓ into the boundary, and
//! assert the counter stops climbing — a screen-observable proxy for "no
//! spurious select fired" (CONTRIBUTING §2: drive keys, assert on
//! rendered output).

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Install the clamp-select test plugin into the project's plugin dir.
fn install_plugin(project_root: &std::path::Path) {
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin_lib(&plugins_dir);

    const SRC: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/plugins/test_list_clamp_select.ts"
    ));
    let dst = plugins_dir.join("test_list_clamp_select.ts");
    fs::write(&dst, SRC).unwrap_or_else(|e| panic!("Failed to write test plugin to {dst:?}: {e}"));
}

#[test]
fn list_boundary_keyrepeat_fires_no_spurious_select() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    install_plugin(&project_root);

    let mut h =
        EditorTestHarness::with_config_and_working_dir(100, 32, Default::default(), project_root)
            .unwrap();
    h.render().unwrap();

    // Mount the focusable 3-item list (selection starts at index 0).
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("TestSel: Mount").unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("SELECTS=0"))
        .unwrap();

    // Two Downs move 0→1→2 (the last item), firing one `select` each.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("SELECTS=1"))
        .unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("SELECTS=2"))
        .unwrap();

    // Now hammer Down against the bottom edge. Each press is a clamped,
    // no-op move — with the fix it fires NO `select`, so the counter
    // stays at 2. With the bug each press ticked it (SELECTS=3,4,5…).
    for _ in 0..4 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    // Give the plugin event queue ample time to deliver any (buggy)
    // spurious selects before asserting they did not arrive.
    h.wait_until_stable(|_| true).unwrap();

    let screen = h.screen_to_string();
    assert!(
        screen.contains("SELECTS=2"),
        "after 2 real moves + 4 clamped Downs the counter must read \
         SELECTS=2 (no spurious select at the boundary).\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("SELECTS=3"),
        "a clamped Down at the list's bottom edge must not fire a \
         `select` event — the counter climbed past 2.\nScreen:\n{screen}"
    );
}
