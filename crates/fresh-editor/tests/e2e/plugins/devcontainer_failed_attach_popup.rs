//! E2E coverage for D-4: the proactive failed-attach action popup.
//!
//! Spec §8 says a build failure should surface a user-prompted Retry /
//! Reopen Locally action popup so the user doesn't have to notice the
//! red Remote Indicator and click it to reach Retry.
//!
//! Driving the full `runDevcontainerUp` failure path needs a fake
//! `devcontainer` CLI on `PATH`, which isn't safe to mutate inside the
//! test binary (tests share the process env). Instead we reuse the
//! plugin's own "post-restart recovery" branch: seeding an
//! `attach-attempt:<cwd>` breadcrumb in plugin global state combined
//! with no active container authority routes through
//! `devcontainer_maybe_show_attach_prompt` → `enterFailedAttach`.
//! That path emits the same popup as every in-flight failure branch.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::plugins::api::PluginCommand;
use fresh::view::ui::status_bar::RemoteIndicatorOverride;
use std::fs;

fn set_up_workspace() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().to_path_buf();
    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    fs::write(
        dc.join("devcontainer.json"),
        r#"{ "name": "fake", "image": "ubuntu:22.04" }"#,
    )
    .unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    (temp, workspace)
}

/// Seed the attach-attempt breadcrumb and then fire the plugin's
/// recovery handler so `enterFailedAttach` runs without needing a real
/// `devcontainer` CLI on `PATH`. The harness doesn't run `main.rs`'s
/// boot sequence, so `plugins_loaded` never fires automatically — we
/// dispatch the handler by name ourselves.
fn drive_failed_attach(harness: &mut EditorTestHarness, workspace: &std::path::Path) {
    let cwd = workspace.to_string_lossy().to_string();
    let breadcrumb_key = format!("attach-attempt:{}", cwd);
    let now_ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis();
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetGlobalState {
            plugin_name: "devcontainer".to_string(),
            key: breadcrumb_key,
            value: Some(serde_json::json!(now_ms.to_string())),
        })
        .unwrap();

    // `handle_set_global_state` updates `Editor.plugin_global_state`
    // but not the plugin state snapshot the JS runtime reads. Tick +
    // render runs the async dispatch that merges into the snapshot.
    harness.tick_and_render().unwrap();

    // `PluginCommand::ExecuteAction` only reaches `registered_actions`
    // entries (handlers that were also surfaced via `registerCommand`).
    // `devcontainer_maybe_show_attach_prompt` is subscribed to the
    // `plugins_loaded` hook instead, so fire that hook directly.
    harness.editor().fire_plugins_loaded_hook();
}

/// After a failed attach the plugin must surface a global action popup
/// with Retry / Show Build Logs / Reopen Locally / Dismiss rows so the
/// user doesn't have to hunt for the Remote Indicator — spec §8.
#[test]
fn devcontainer_failed_attach_surfaces_action_popup() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(140, 40, workspace.clone()).unwrap();

    // Tick so plugin-load async messages drain before we dispatch on it.
    harness.tick_and_render().unwrap();

    drive_failed_attach(&mut harness, &workspace);

    harness
        .wait_until(|h| h.screen_to_string().contains("Dev Container Attach Failed"))
        .unwrap();

    let screen = harness.screen_to_string();
    for row in ["Retry", "Show Build Logs", "Reopen Locally", "Dismiss"] {
        assert!(
            screen.contains(row),
            "Failed-attach popup must offer '{}' row. Screen:\n{}",
            row,
            screen,
        );
    }
}

/// Picking "Reopen Locally" from the failed-attach popup must drop the
/// FailedAttach override (no authority was ever installed, so there's
/// nothing to detach — `clearRemoteIndicatorState` handles it). This
/// locks in that the handler is actually wired, not just rendering
/// inert rows.
#[test]
fn devcontainer_failed_attach_popup_reopen_local_clears_override() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(140, 40, workspace.clone()).unwrap();

    harness.tick_and_render().unwrap();
    drive_failed_attach(&mut harness, &workspace);

    harness
        .wait_until(|h| h.screen_to_string().contains("Dev Container Attach Failed"))
        .unwrap();

    assert!(
        matches!(
            harness.editor().remote_indicator_override,
            Some(RemoteIndicatorOverride::FailedAttach { .. })
        ),
        "enterFailedAttach must set a FailedAttach override before the popup action clears it"
    );

    // Rows render in the order Retry (selected), Show Build Logs,
    // Reopen Locally, Dismiss. Arrow down twice to land on Reopen
    // Locally, then Enter to fire action_popup_result.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.editor().remote_indicator_override.is_none())
        .unwrap();
}
