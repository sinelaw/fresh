//! E2E coverage for E-3: the standalone Forwarded Ports panel.
//!
//! Phase A's `devcontainer_show_ports` is a prompt-picker for quick
//! lookups. E-3 extends that with a standalone virtual-buffer panel
//! that tabulates configured ports, port attributes, and runtime
//! `docker port <id>` bindings so users can see everything at once.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use fresh::services::plugins::api::PluginCommand;
use std::fs;

/// Set up a workspace with a devcontainer config that declares a few
/// `forwardPorts` entries and `portsAttributes` labels. No container
/// authority is active so the panel has no runtime bindings to
/// display — that's the "configured only" branch of the renderer.
fn set_up_workspace() -> (tempfile::TempDir, std::path::PathBuf) {
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
#[test]
fn devcontainer_show_forwarded_ports_panel_lists_configured_ports() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 40, workspace).unwrap();

    harness.tick_and_render().unwrap();

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
