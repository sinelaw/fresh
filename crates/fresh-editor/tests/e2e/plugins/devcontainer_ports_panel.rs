//! E2E coverage for E-3: the standalone Forwarded Ports panel.
//!
//! Phase A's `devcontainer_show_ports` is a prompt-picker for quick
//! lookups. E-3 extends that with a standalone virtual-buffer panel
//! that tabulates configured ports, port attributes, and runtime
//! `docker port <id>` bindings so users can see everything at once.
//!
//! These tests drive the panel the same way a user does: keyboard
//! input through the command palette, keyboard input to close. No
//! internal plugin-command dispatch, no poking at editor state —
//! the assertions are against the rendered screen.
//!
//! Progress prints (`eprintln!`) before each wait so a CI timeout
//! surfaces the last step the test reached rather than a silent
//! 180s hang. nextest captures stderr into its failure report.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Set up a workspace with a devcontainer config that declares a few
/// `forwardPorts` entries and `portsAttributes` labels. No container
/// authority is active so the panel has no runtime bindings to
/// display — that's the "configured only" branch of the renderer.
fn set_up_workspace() -> (tempfile::TempDir, std::path::PathBuf) {
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

/// Walk the command palette to the "Show Forwarded Ports" entry and
/// activate it. Same keyboard sequence a user would type.
fn open_ports_panel_via_palette(harness: &mut EditorTestHarness) {
    eprintln!("[ports_panel] send Ctrl+P");
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    eprintln!("[ports_panel] wait for palette prompt");
    harness.wait_for_prompt().unwrap();
    eprintln!("[ports_panel] type 'Show Forwarded Ports'");
    harness.type_text("Show Forwarded Ports").unwrap();
    eprintln!("[ports_panel] wait for palette suggestion 'Dev Container: Show Forwarded Ports'");
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Dev Container: Show Forwarded Ports")
        })
        .unwrap();
    eprintln!("[ports_panel] send Enter to activate command");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// User invokes `Dev Container: Show Forwarded Ports` from the
/// palette; the panel renders below with the four column headers
/// and a row per configured port.
#[test]
fn devcontainer_show_forwarded_ports_panel_lists_configured_ports() {
    eprintln!("[ports_panel] setting up workspace");
    let (_temp, workspace) = set_up_workspace();
    eprintln!(
        "[ports_panel] constructing harness with workspace={:?}",
        workspace
    );
    let mut harness = EditorTestHarness::with_working_dir(160, 40, workspace).unwrap();
    eprintln!("[ports_panel] initial tick_and_render");
    harness.tick_and_render().unwrap();

    let plugin_names: Vec<_> = harness
        .editor()
        .plugin_manager()
        .list_plugins()
        .into_iter()
        .map(|p| p.name)
        .collect();
    eprintln!("[ports_panel] loaded plugins: {:?}", plugin_names);
    assert!(
        plugin_names.iter().any(|n| n == "devcontainer"),
        "`devcontainer` plugin must be loaded. Loaded: {:?}",
        plugin_names
    );

    open_ports_panel_via_palette(&mut harness);

    eprintln!("[ports_panel] wait for 'Forwarded Ports' on screen");
    harness
        .wait_until(|h| h.screen_to_string().contains("Forwarded Ports"))
        .unwrap();

    eprintln!("[ports_panel] verifying panel content");
    let screen = harness.screen_to_string();
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
    for content in ["3000", "http", "Web App", "5432", "Postgres"] {
        assert!(
            screen.contains(content),
            "Panel must include row content '{}'. Screen:\n{}",
            content,
            screen,
        );
    }
    assert!(
        screen.contains("r: refresh"),
        "Panel footer must advertise the refresh key. Screen:\n{}",
        screen,
    );
    eprintln!("[ports_panel] done");
}

/// The panel's mode binds `q` to close. User presses `q`, the
/// panel disappears from the screen.
#[test]
fn devcontainer_ports_panel_closes_on_q() {
    eprintln!("[ports_panel_close] setting up workspace");
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 40, workspace).unwrap();
    eprintln!("[ports_panel_close] initial tick_and_render");
    harness.tick_and_render().unwrap();

    let plugin_names: Vec<_> = harness
        .editor()
        .plugin_manager()
        .list_plugins()
        .into_iter()
        .map(|p| p.name)
        .collect();
    eprintln!("[ports_panel_close] loaded plugins: {:?}", plugin_names);
    assert!(
        plugin_names.iter().any(|n| n == "devcontainer"),
        "`devcontainer` plugin must be loaded. Loaded: {:?}",
        plugin_names
    );

    open_ports_panel_via_palette(&mut harness);

    eprintln!("[ports_panel_close] wait for 'Forwarded Ports' on screen");
    harness
        .wait_until(|h| h.screen_to_string().contains("Forwarded Ports"))
        .unwrap();

    eprintln!("[ports_panel_close] send 'q' to close");
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();

    eprintln!("[ports_panel_close] wait for panel to close");
    harness
        .wait_until(|h| !h.screen_to_string().contains("Forwarded Ports"))
        .unwrap();
    eprintln!("[ports_panel_close] done");
}
