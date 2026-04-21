//! Tests for the Remote Indicator status-bar popup (`show_remote_indicator_popup`).
//!
//! The helper that drives "Reopen in Container" vs the disabled
//! "No dev container config detected" row probes the workspace's
//! filesystem. These tests lock in that the probe goes through the
//! active authority's `FileSystem` trait (per `CONTRIBUTING.md`
//! guideline 4) by driving the happy path through `StdFileSystem`
//! end-to-end: a sibling `.devcontainer/devcontainer.json` flips the
//! popup from the disabled hint to the actionable row.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
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
