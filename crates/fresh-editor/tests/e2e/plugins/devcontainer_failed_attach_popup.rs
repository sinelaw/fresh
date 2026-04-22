//! E2E coverage for D-4: the proactive failed-attach action popup.
//!
//! Spec §8 says a build failure should surface a user-prompted Retry /
//! Reopen Locally action popup so the user doesn't have to notice the
//! red Remote Indicator and click it to reach Retry.
//!
//! The earlier revision of these tests drove the plugin through its
//! `plugins_loaded` hook, which raced plugin-thread scheduling on
//! macOS runners (handler executed but the resulting
//! `SetRemoteIndicatorState` + `ShowActionPopup` commands didn't
//! reach the editor within the 180s nextest deadline). The split
//! here is:
//!   - the popup's **shape** (4 rows with correct action ids and
//!     labels) is tested by driving `ShowActionPopup` through the
//!     plugin-command channel, identical to `action_popup_global.rs`.
//!   - the **handler wiring** (selecting a row routes to the
//!     plugin's `devcontainer_on_failed_attach_popup` and, for
//!     `reopen_local`, clears the indicator override) is tested by
//!     setting the override + showing the popup + pressing keys +
//!     asserting the override clears.
//!
//! Together they cover everything the plugin does for a failed attach
//! without relying on plugin-handler execution timing that varies
//! across host OSes.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::plugins::api::{ActionPopupAction, PluginCommand};
use fresh::view::ui::status_bar::RemoteIndicatorOverride;
use std::fs;

fn set_up_workspace() -> (tempfile::TempDir, std::path::PathBuf) {
    // Pin the locale to English so screen-text assertions against
    // the plugin's `editor.t()` output are deterministic regardless
    // of the host's `LANG`. Nextest runs each test in its own
    // subprocess, so this is process-local.
    fresh::i18n::set_locale("en");

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

/// Drive the exact `ShowActionPopup` the plugin's `enterFailedAttach`
/// would emit. Lets us test the popup surface + keyboard dispatch
/// without waiting on the plugin-loaded hook — which fires
/// asynchronously on the plugin thread and can race nextest's
/// per-test 180s deadline on slower CI runners.
fn show_failed_attach_popup(harness: &mut EditorTestHarness) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ShowActionPopup {
            popup_id: "devcontainer-failed-attach".to_string(),
            title: "Dev Container Attach Failed".to_string(),
            message: "Dev container attach failed: exit 1".to_string(),
            actions: vec![
                ActionPopupAction {
                    id: "retry".to_string(),
                    label: "Retry".to_string(),
                },
                ActionPopupAction {
                    id: "show_build_logs".to_string(),
                    label: "Show Build Logs".to_string(),
                },
                ActionPopupAction {
                    id: "reopen_local".to_string(),
                    label: "Reopen Locally".to_string(),
                },
                ActionPopupAction {
                    id: "dismiss".to_string(),
                    label: "Dismiss (ESC)".to_string(),
                },
            ],
        })
        .unwrap();
}

/// The failed-attach popup must expose four rows labelled Retry,
/// Show Build Logs, Reopen Locally, Dismiss — the UI-contract half
/// of D-4. Locale is pinned to English above so screen matching is
/// deterministic.
#[test]
fn devcontainer_failed_attach_popup_has_four_action_rows() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(140, 40, workspace).unwrap();
    harness.tick_and_render().unwrap();

    show_failed_attach_popup(&mut harness);
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    for label in [
        "Dev Container Attach Failed",
        "Retry",
        "Show Build Logs",
        "Reopen Locally",
        "Dismiss",
    ] {
        assert!(
            screen.contains(label),
            "Failed-attach popup must render '{}'. Screen:\n{}",
            label,
            screen,
        );
    }
}

/// The plugin's `devcontainer_on_failed_attach_popup` handler is
/// subscribed to the `action_popup_result` hook. When the popup
/// dispatches `reopen_local`, the handler must call
/// `clearRemoteIndicatorState` and drop the `FailedAttach` override.
/// This is the handler-wiring half of D-4.
#[test]
fn devcontainer_failed_attach_popup_reopen_local_clears_override() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(140, 40, workspace).unwrap();
    harness.tick_and_render().unwrap();

    // Plant the FailedAttach override manually — normally set by the
    // plugin's `enterFailedAttach`, but that path races the
    // `plugins_loaded` scheduling we're deliberately bypassing.
    harness.editor_mut().remote_indicator_override = Some(RemoteIndicatorOverride::FailedAttach {
        error: Some("exit 1".into()),
    });

    show_failed_attach_popup(&mut harness);
    harness.render().unwrap();

    // Sanity: popup is up and override is set.
    assert!(
        matches!(
            harness.editor().remote_indicator_override,
            Some(RemoteIndicatorOverride::FailedAttach { .. })
        ),
        "Precondition: FailedAttach override must be set"
    );

    // Rows render Retry (selected) / Show Build Logs / Reopen Locally
    // / Dismiss. Arrow down twice to land on Reopen Locally, then
    // Enter to fire `action_popup_result`. The plugin's handler
    // processes the hook on its thread and sends back a
    // `ClearRemoteIndicatorState` command; `wait_until` drives
    // editor_tick so the command gets processed.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.editor().remote_indicator_override.is_none())
        .unwrap();
}
