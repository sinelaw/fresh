//! Tests for the Remote Indicator status-bar popup (`show_remote_indicator_popup`).
//!
//! The helper that drives "Reopen in Container" vs the disabled
//! "No dev container config detected" row probes the workspace's
//! filesystem. These tests lock in that the probe goes through the
//! active authority's `FileSystem` trait (per `CONTRIBUTING.md`
//! guideline 4) by driving the happy path through `StdFileSystem`
//! end-to-end: a sibling `.devcontainer/devcontainer.json` flips the
//! popup from the disabled hint to the actionable row.
//!
//! Phase B adds popup branches keyed off
//! `Editor::remote_indicator_override` (Connecting / FailedAttach);
//! we exercise those by setting the override directly, the same
//! plumbing the `setRemoteIndicatorState` plugin op drives.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::view::ui::status_bar::RemoteIndicatorOverride;
use std::fs;

fn popup_item_texts(harness: &EditorTestHarness) -> Vec<String> {
    harness
        .editor()
        .active_state()
        .popups
        .top()
        .map(|p| match &p.content {
            fresh::view::popup::PopupContent::List { items, .. } => {
                items.iter().map(|i| i.text.clone()).collect()
            }
            _ => Vec::new(),
        })
        .unwrap_or_default()
}

#[test]
fn test_remote_indicator_popup_local_with_devcontainer_offers_reopen() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let dc = temp.path().join(".devcontainer");
    fs::create_dir_all(&dc)?;
    fs::write(
        dc.join("devcontainer.json"),
        r#"{ "name": "test", "image": "ubuntu:22.04" }"#,
    )?;

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.editor_mut().show_remote_indicator_popup();
    harness.render()?;

    let items = popup_item_texts(&harness);
    assert!(
        items.iter().any(|t| t.contains("Reopen in Container")),
        "Popup should offer 'Reopen in Container' when .devcontainer/devcontainer.json \
         is visible via the authority filesystem. Items: {:#?}",
        items
    );
    Ok(())
}

#[test]
fn test_remote_indicator_popup_local_without_devcontainer_shows_hint() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    // Deliberately no .devcontainer files.

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.editor_mut().show_remote_indicator_popup();
    harness.render()?;

    let items = popup_item_texts(&harness);
    assert!(
        items
            .iter()
            .any(|t| t.contains("Create Dev Container Config")),
        "Popup should offer the scaffold row when no config is detectable. \
         Items: {:#?}",
        items
    );
    assert!(
        !items.iter().any(|t| t.contains("Reopen in Container")),
        "Popup should not offer 'Reopen in Container' without a config. \
         Items: {:#?}",
        items
    );
    Ok(())
}

#[test]
fn test_remote_indicator_popup_connecting_offers_cancel_and_logs() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    // Drive the editor into the Connecting state the way the plugin
    // would via setRemoteIndicatorState — bypassing the plugin
    // command channel keeps the test hermetic.
    harness.editor_mut().remote_indicator_override = Some(RemoteIndicatorOverride::Connecting {
        label: Some("Building".into()),
    });

    harness.editor_mut().show_remote_indicator_popup();
    harness.render()?;

    let items = popup_item_texts(&harness);
    assert!(
        items.iter().any(|t| t.contains("Cancel Startup")),
        "Connecting popup should offer a Cancel Startup row. Items: {:#?}",
        items
    );
    assert!(
        items
            .iter()
            .any(|t| t.contains("Show Logs") && !t.contains("Container")),
        "Connecting popup should offer a Show Logs row. Items: {:#?}",
        items
    );
    assert!(
        !items.iter().any(|t| t.contains("Reopen in Container")),
        "Connecting popup must not dispatch a second attach. Items: {:#?}",
        items
    );
    Ok(())
}

#[test]
fn test_remote_indicator_popup_failed_attach_offers_retry() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.editor_mut().remote_indicator_override = Some(RemoteIndicatorOverride::FailedAttach {
        error: Some("exit 1".into()),
    });

    harness.editor_mut().show_remote_indicator_popup();
    harness.render()?;

    let items = popup_item_texts(&harness);
    assert!(
        items.iter().any(|t| t.contains("Retry")),
        "FailedAttach popup should offer a Retry row. Items: {:#?}",
        items
    );
    assert!(
        items.iter().any(|t| t.contains("Reopen Locally")),
        "FailedAttach popup should offer a Reopen Locally row. Items: {:#?}",
        items
    );
    assert!(
        items.iter().any(|t| t.contains("Show Build Logs")),
        "FailedAttach popup should offer a Show Build Logs row (stub until Phase D). \
         Items: {:#?}",
        items
    );
    Ok(())
}
