//! E2E: a local workspace that was still being created when the editor quit
//! is restored on the next launch. The non-blocking New-Workspace flow
//! persists in-flight *local* specs (a remote connect is instead recovered by
//! the host's own dormant-session persistence), and the orchestrator replays
//! them on the `ready` lifecycle hook as paused, resumable dock rows — so an
//! interrupted worktree create is never silently lost.
//!
//! We seed the persisted pending-workspace state exactly as a prior session
//! would have left it, then fire `ready` and assert the row comes back. This
//! drives the real recovery path (`recoverPendingWorkspaces`) and asserts on
//! rendered output; without it, firing `ready` leaves nothing on screen.
//!
//! Single test in this binary: `isolated_dir_context` sets the process-global
//! `XDG_DATA_HOME`, keeping all persistence inside the per-test temp tree.
#![cfg(all(target_os = "linux", feature = "plugins"))]

mod common;

use common::dormant_ssh::isolated_dir_context;
use common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use fresh_core::api::PluginCommand;
use serde_json::json;

#[test]
fn interrupted_local_workspace_is_restored_paused_on_launch() {
    fresh::i18n::set_locale("en");
    let base = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(base.path());
    let project = base.path().join("project");
    std::fs::create_dir_all(&project).unwrap();
    let project = project.canonicalize().unwrap();

    let plugins_dir = project.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    let mut h = EditorTestHarness::create(
        160,
        50,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .with_shared_dir_context(dir_context),
    )
    .unwrap();
    h.tick_and_render().unwrap();
    h.wait_until(|h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all()
            .iter()
            .any(|c| c.get_localized_name() == "Orchestrator: New Workspace")
    })
    .unwrap();

    // The state a previous session left behind: a local workspace that was
    // still being created (its worktree not yet made) when the editor quit,
    // under the orchestrator's pending-workspace key.
    let target = project.join("recovered-ws");
    let target_str = target.to_string_lossy().to_string();
    let pending = json!([{
        "spec": {
            "backend": "local",
            "projectPath": target_str,
            "name": "",
            "cmd": "",
            "branch": "",
            "createWorktree": false,
            "displayLabel": "recovered-ws",
            "displayProject": target_str,
        },
        "label": "recovered-ws",
    }]);
    h.editor_mut()
        .handle_plugin_command(PluginCommand::SetGlobalState {
            plugin_name: "orchestrator".to_string(),
            key: "orchestrator.pending".to_string(),
            value: Some(pending),
        });

    // The `ready` lifecycle hook replays persisted pending specs (this is the
    // "editor just launched" signal).
    h.editor_mut().fire_ready_hook();

    // The interrupted workspace comes back — paused and resumable — in the
    // dock, labelled as it was.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("recovered-ws") && s.contains("Interrupted")
    })
    .unwrap();
}
