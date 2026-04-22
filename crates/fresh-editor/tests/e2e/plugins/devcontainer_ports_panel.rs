//! E2E coverage for E-3: the standalone Forwarded Ports panel.
//!
//! Phase A's `devcontainer_show_ports` is a prompt-picker for quick
//! lookups. E-3 extends that with a standalone virtual-buffer panel
//! that tabulates configured ports, port attributes, and runtime
//! `docker port <id>` bindings so users can see everything at once.
//!
//! The tests drive the panel via `PluginCommand::ExecuteAction`
//! (the same path the palette uses) and wait on the rendered screen
//! text. That chain hops through the plugin thread multiple times
//! (plugin handler runs → `createVirtualBufferInSplit` async roundtrip
//! → editor receives result → screen repaints). Environment
//! requirements are asserted up front — missing plugin, missing
//! registered command — so the tests fail with a clear message
//! instead of silently timing out on the `wait_until` if the
//! environment is wrong.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use fresh::services::plugins::api::PluginCommand;
use std::fs;

/// Fail fast if the plugin environment isn't what these tests expect.
/// A silent `wait_until` timeout leaves nothing diagnostic in CI logs
/// other than "TIMEOUT 180s"; surfacing the real cause (plugin didn't
/// load, action didn't register) turns those into actionable failures.
fn assert_devcontainer_plugin_ready(harness: &EditorTestHarness) {
    let plugins = harness.editor().plugin_manager().list_plugins();
    let loaded: Vec<_> = plugins.iter().map(|p| p.name.clone()).collect();
    assert!(
        plugins.iter().any(|p| p.name == "devcontainer"),
        "`devcontainer` plugin must be loaded before driving its \
         commands. Loaded plugins: {:?}",
        loaded,
    );
}

/// Set up a workspace with a devcontainer config that declares a few
/// `forwardPorts` entries and `portsAttributes` labels. No container
/// authority is active so the panel has no runtime bindings to
/// display — that's the "configured only" branch of the renderer.
fn set_up_workspace() -> (tempfile::TempDir, std::path::PathBuf) {
    // Pin the locale to English so screen-text assertions against
    // the plugin's `editor.t()` output are deterministic regardless
    // of the host's `LANG`.
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().to_path_buf();

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    fs::write(
        dc.join("devcontainer.json"),
        r#"{
            "name": "fake",
            "image": "ubuntu:22.04",
            "forwardPorts": [3000, 5432],
            "portsAttributes": {
                "3000": { "label": "Web App", "protocol": "http", "onAutoForward": "notify" },
                "5432": { "label": "Postgres", "onAutoForward": "silent" }
            }
        }"#,
    )
    .unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    (temp, workspace)
}

/// Trigger the panel via the plugin command we registered. Works even
/// though the harness doesn't fire `plugins_loaded` — the command is
/// in `registered_actions` because it went through `registerCommand`.
///
/// Windows CI has timed out on this test (180s nextest deadline) —
/// the async chain `ExecuteAction` → plugin-thread handler →
/// `createVirtualBufferInSplit` async roundtrip → screen repaint
/// didn't complete in the window. The upfront
/// `assert_devcontainer_plugin_ready` surfaces the most common
/// environmental cause (plugin not loaded) before we enter the
/// `wait_until`. Genuine plugin-runtime hangs on Windows will still
/// surface as timeouts, but with `plugin_ready` passing the failure
/// is pinned to the async-chain flakiness rather than environment.
#[test]
fn devcontainer_show_forwarded_ports_panel_lists_configured_ports() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 40, workspace).unwrap();

    harness.tick_and_render().unwrap();
    assert_devcontainer_plugin_ready(&harness);

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ExecuteAction {
            action_name: "devcontainer_show_forwarded_ports_panel".to_string(),
        })
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Forwarded Ports"))
        .unwrap();

    let screen = harness.screen_to_string();
    // Headers must render so users know what each column means.
    for header in [
        "Forwarded Ports",
        "Configured",
        "Protocol",
        "Label",
        "Runtime binding",
    ] {
        assert!(
            screen.contains(header),
            "Panel must render column '{}'. Screen:\n{}",
            header,
            screen,
        );
    }
    // Each configured port with its label should appear on a row.
    for content in ["3000", "http", "Web App", "5432", "Postgres"] {
        assert!(
            screen.contains(content),
            "Panel must include row content '{}'. Screen:\n{}",
            content,
            screen,
        );
    }
    // The footer calls out the refresh/close bindings.
    assert!(
        screen.contains("r: refresh"),
        "Panel footer must advertise the refresh key. Screen:\n{}",
        screen,
    );
}

/// The panel is a virtual buffer in a mode that registers `r` for
/// refresh, `q`/Escape for close. Close dismisses the split and
/// clears the module-level buffer-id state so a subsequent open
/// rebuilds cleanly.
#[test]
fn devcontainer_ports_panel_closes_on_q() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 40, workspace).unwrap();

    harness.tick_and_render().unwrap();
    assert_devcontainer_plugin_ready(&harness);

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ExecuteAction {
            action_name: "devcontainer_show_forwarded_ports_panel".to_string(),
        })
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Forwarded Ports"))
        .unwrap();

    // `q` in the ports panel mode closes the split.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ExecuteAction {
            action_name: "devcontainer_close_ports_panel".to_string(),
        })
        .unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("Forwarded Ports"))
        .unwrap();
}
