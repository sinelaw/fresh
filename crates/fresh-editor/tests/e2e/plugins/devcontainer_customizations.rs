//! E2E coverage for E-1: the `customizations.fresh.plugins` extension
//! point.
//!
//! Spec §7 asks for tool-specific customization namespaces. VS Code's
//! `customizations.vscode.extensions` doesn't apply to Fresh — different
//! plugin model — so we mirror the shape under `customizations.fresh.plugins`
//! so devcontainer authors can ship plugins scoped to the attached
//! container. The test sets up a workspace whose devcontainer.json
//! references a sibling plugin that registers a distinctively-named
//! command, installs a container authority, fires the `plugins_loaded`
//! hook, and asserts the plugin's command appears in the palette.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::authority::{
    Authority, AuthorityPayload, FilesystemSpec, SpawnerSpec, TerminalWrapperSpec,
};
use std::fs;

fn container_authority(label: &str) -> Authority {
    // Minimal docker-exec style spawner that doesn't require a real
    // Docker daemon — the authority label is what the plugin's
    // "attached?" check reads.
    Authority::from_plugin_payload(AuthorityPayload {
        filesystem: FilesystemSpec::Local,
        spawner: SpawnerSpec::Local,
        terminal_wrapper: TerminalWrapperSpec::HostShell,
        display_label: label.to_string(),
    })
    .unwrap()
}

const CUSTOMIZATION_PLUGIN_SRC: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

globalThis.devcontainer_customization_marker = function(): void {
    editor.setStatus("customization-marker-ran");
};

editor.registerCommand(
    "Dev Container Customization Marker",
    "Marker command registered by a customization plugin (test fixture).",
    "devcontainer_customization_marker",
    null,
);

editor.debug("devcontainer-customization-marker loaded");
"#;

/// A workspace configured with `customizations.fresh.plugins` pointing
/// at a sibling plugin file. On container attach, the devcontainer
/// plugin must load that sibling so its registered command shows up in
/// the palette.
#[test]
fn devcontainer_customizations_fresh_plugins_loads_entries_on_attach() {
    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().to_path_buf();

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    fs::write(
        dc.join("devcontainer.json"),
        r#"{
            "name": "fake",
            "image": "ubuntu:22.04",
            "customizations": {
                "fresh": { "plugins": ["./customization-marker.ts"] }
            }
        }"#,
    )
    .unwrap();

    // The customization plugin lives at the workspace root; the relative
    // path in devcontainer.json resolves against the workspace cwd.
    fs::write(
        workspace.join("customization-marker.ts"),
        CUSTOMIZATION_PLUGIN_SRC,
    )
    .unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    let mut harness = EditorTestHarness::with_working_dir(140, 40, workspace.clone()).unwrap();

    // Install a container authority the same way
    // `main.rs` does after `devcontainer up` succeeds. Gates the E-1
    // handler on the "attached to container" check.
    harness
        .editor_mut()
        .set_boot_authority(container_authority("Container:deadbeef"));

    // Tick so the authority snapshot update propagates to the plugin
    // runtime before `plugins_loaded` reads `getAuthorityLabel()`.
    harness.tick_and_render().unwrap();

    // The harness doesn't run `main.rs`'s boot sequence, so the
    // `plugins_loaded` hook never fires automatically. The devcontainer
    // plugin's E-1 handler is subscribed to it.
    harness.editor().fire_plugins_loaded_hook();

    // Wait for the customization plugin's command to show up in the
    // palette — that's our semantic signal that `loadPlugin` succeeded
    // and the plugin registered.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text("Dev Container Customization Marker")
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Dev Container Customization Marker")
        })
        .unwrap();
}
