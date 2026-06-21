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
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchCursor, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch, StdFileSystem,
};
use fresh::services::authority::{
    Authority, AuthorityPayload, FilesystemSpec, RemoteAgentSpec, RemoteTransportSpec,
    SessionAuthoritySpec, SpawnerSpec, TerminalWrapperSpec,
};
use fresh::view::ui::status_bar::RemoteIndicatorOverride;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// A filesystem that reports as a *remote* backend whose link is down — it
/// delegates real I/O to `StdFileSystem` but advertises a connection string and
/// answers `is_remote_connected() == false`, so `connection_display_string()`
/// renders the "(Disconnected)" suffix the Remote Indicator's Disconnected
/// branch keys off. Lets the popup test drive that state with no network.
struct DisconnectedRemoteFs {
    inner: StdFileSystem,
}

impl DisconnectedRemoteFs {
    fn new() -> Self {
        Self {
            inner: StdFileSystem,
        }
    }
}

impl FileSystem for DisconnectedRemoteFs {
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.inner.read_file(path)
    }
    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.inner.read_range(path, offset, len)
    }
    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.inner.write_file(path, data)
    }
    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_file(path)
    }
    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        self.inner.open_file(path)
    }
    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_write(path)
    }
    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_append(path)
    }
    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
        self.inner.set_file_length(path, len)
    }
    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.inner.rename(from, to)
    }
    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        self.inner.copy(from, to)
    }
    fn remove_file(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_file(path)
    }
    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_dir(path)
    }
    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.metadata(path)
    }
    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.symlink_metadata(path)
    }
    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_dir(path)
    }
    fn is_file(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_file(path)
    }
    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
        self.inner.set_permissions(path, permissions)
    }
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        self.inner.read_dir(path)
    }
    fn create_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir(path)
    }
    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir_all(path)
    }
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.inner.canonicalize(path)
    }
    fn current_uid(&self) -> u32 {
        self.inner.current_uid()
    }
    fn search_file(
        &self,
        path: &Path,
        pattern: &str,
        opts: &FileSearchOptions,
        cursor: &mut FileSearchCursor,
    ) -> io::Result<Vec<SearchMatch>> {
        self.inner.search_file(path, pattern, opts, cursor)
    }
    fn sudo_write(
        &self,
        path: &Path,
        data: &[u8],
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> io::Result<()> {
        self.inner.sudo_write(path, data, mode, uid, gid)
    }
    fn walk_files(
        &self,
        root: &Path,
        skip_dirs: &[&str],
        cancel: &std::sync::atomic::AtomicBool,
        on_file: &mut dyn FnMut(&Path, &str) -> bool,
    ) -> io::Result<()> {
        self.inner.walk_files(root, skip_dirs, cancel, on_file)
    }
    fn remote_connection_info(&self) -> Option<&str> {
        Some("root@127.0.0.1")
    }
    fn is_remote_connected(&self) -> bool {
        false
    }
}

/// A remote-agent (SSH) backend spec, the kind a live SSH window persists so a
/// reconnect can rebuild it.
fn ssh_agent_spec() -> SessionAuthoritySpec {
    SessionAuthoritySpec::RemoteAgent(RemoteAgentSpec {
        transport: RemoteTransportSpec::Ssh {
            user: Some("root".into()),
            host: "127.0.0.1".into(),
            port: Some(2222),
            identity_file: None,
            remote_path: None,
            extra_args: Vec::new(),
        },
        base_env: Vec::new(),
        window: true,
        label: None,
        command: None,
    })
}

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

/// Pair of (label, data, disabled) for each row — lets a test assert
/// both "row is visible" and "row dispatches the right action and is
/// not disabled." Prevents regressions where a row quietly loses its
/// action (reverted to a `.disabled()` stub).
fn popup_item_rows(harness: &EditorTestHarness) -> Vec<(String, Option<String>, bool)> {
    harness
        .editor()
        .active_state()
        .popups
        .top()
        .map(|p| match &p.content {
            fresh::view::popup::PopupContent::List { items, .. } => items
                .iter()
                .map(|i| (i.text.clone(), i.data.clone(), i.disabled))
                .collect(),
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

    let rows = popup_item_rows(&harness);
    let cancel = rows
        .iter()
        .find(|(t, _, _)| t.contains("Cancel Startup"))
        .unwrap_or_else(|| panic!("Connecting popup lacks a Cancel Startup row. Rows: {rows:#?}"));
    assert_eq!(
        cancel.1.as_deref(),
        Some("plugin:devcontainer_cancel_attach"),
        "Cancel Startup must dispatch the plugin cancel handler. Row: {cancel:?}"
    );
    assert!(
        !cancel.2,
        "Cancel Startup must not be disabled. Row: {cancel:?}"
    );

    let logs = rows
        .iter()
        .find(|(t, _, _)| t.contains("Show Logs") && !t.contains("Container"))
        .unwrap_or_else(|| panic!("Connecting popup lacks a Show Logs row. Rows: {rows:#?}"));
    assert_eq!(
        logs.1.as_deref(),
        Some("plugin:devcontainer_show_build_logs"),
        "Show Logs must dispatch the plugin show-build-logs handler. Row: {logs:?}"
    );
    assert!(!logs.2, "Show Logs must not be disabled. Row: {logs:?}");

    assert!(
        !rows
            .iter()
            .any(|(t, _, _)| t.contains("Reopen in Container")),
        "Connecting popup must not dispatch a second attach. Rows: {rows:#?}"
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

    let rows = popup_item_rows(&harness);
    let retry = rows
        .iter()
        .find(|(t, _, _)| t.contains("Retry"))
        .unwrap_or_else(|| panic!("FailedAttach popup lacks a Retry row. Rows: {rows:#?}"));
    assert_eq!(retry.1.as_deref(), Some("plugin:devcontainer_retry_attach"));
    assert!(!retry.2);

    let reopen = rows
        .iter()
        .find(|(t, _, _)| t.contains("Reopen Locally"))
        .unwrap_or_else(|| {
            panic!("FailedAttach popup lacks a Reopen Locally row. Rows: {rows:#?}")
        });
    assert_eq!(reopen.1.as_deref(), Some("clear_override"));
    assert!(!reopen.2);

    let logs = rows
        .iter()
        .find(|(t, _, _)| t.contains("Show Build Logs"))
        .unwrap_or_else(|| {
            panic!("FailedAttach popup lacks a Show Build Logs row. Rows: {rows:#?}")
        });
    assert_eq!(
        logs.1.as_deref(),
        Some("plugin:devcontainer_show_build_logs"),
        "Show Build Logs must dispatch the plugin show-build-logs handler. Row: {logs:?}"
    );
    assert!(
        !logs.2,
        "Show Build Logs must not be disabled. Row: {logs:?}"
    );
    Ok(())
}

/// A *live* remote-agent (SSH/kube) window whose backend dropped its carrier
/// must offer a working **Reconnect** in the Remote Indicator popup — the
/// status-bar surface for rebuilding the link (re-point authority, respawn the
/// dead terminal). The row dispatches the core reconnect (`reconnect`), distinct
/// from "Go Local" (`detach`).
#[test]
fn test_remote_indicator_popup_disconnected_remote_agent_offers_reconnect() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    // A filesystem that presents as a disconnected remote: the popup's
    // `connection_display_string()` gets a "(Disconnected)" suffix, driving the
    // Disconnected branch with no real SSH.
    let fs = Arc::new(DisconnectedRemoteFs::new());
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_filesystem(fs)
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    // Mark the active window as a remote-agent session — its backend can be
    // rebuilt from the stored spec, which is what gates the Reconnect row.
    let active = harness.editor().active_window_id();
    harness
        .editor_mut()
        .set_session_authority_spec(active, ssh_agent_spec());

    harness.editor_mut().show_remote_indicator_popup();
    harness.render()?;

    let rows = popup_item_rows(&harness);
    let reconnect = rows
        .iter()
        .find(|(t, _, _)| t.contains("Reconnect"))
        .unwrap_or_else(|| {
            panic!("Disconnected remote-agent popup lacks a Reconnect row. Rows: {rows:#?}")
        });
    assert_eq!(
        reconnect.1.as_deref(),
        Some("reconnect"),
        "Reconnect must dispatch the core reconnect action. Row: {reconnect:?}"
    );
    assert!(
        !reconnect.2,
        "Reconnect must not be disabled. Row: {reconnect:?}"
    );

    // "Go Local" is still offered as the escape hatch.
    assert!(
        rows.iter()
            .any(|(t, d, _)| t.contains("Go Local") && d.as_deref() == Some("detach")),
        "Disconnected popup must still offer 'Go Local'. Rows: {rows:#?}"
    );
    Ok(())
}

/// A local session's Remote Indicator popup must NOT offer Reconnect — there is
/// no remote backend to rebuild. Guards against the Reconnect row leaking into
/// non-remote (or container) windows.
#[test]
fn test_remote_indicator_popup_local_does_not_offer_reconnect() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.editor_mut().show_remote_indicator_popup();
    harness.render()?;

    let rows = popup_item_rows(&harness);
    assert!(
        !rows.iter().any(|(t, _, _)| t.contains("Reconnect")),
        "Local popup must not offer a Reconnect row. Rows: {rows:#?}"
    );
    Ok(())
}

/// Once the user is attached to a container, the Remote Indicator
/// popup must surface a "Show Build Logs" row so they can revisit the
/// `devcontainer up` log without hunting through `.fresh-cache/`. The
/// row dispatches the same plugin handler as the Connecting /
/// FailedAttach branches.
#[test]
fn test_remote_indicator_popup_connected_container_offers_show_build_logs() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    // Drop the editor into the Connected/container state by installing
    // a container authority — same path main.rs takes after a
    // `devcontainer up` succeeds. Display label is what the popup
    // branch keys off (`is_container = label.starts_with("Container:")`).
    let authority = Authority::from_plugin_payload(
        AuthorityPayload {
            filesystem: FilesystemSpec::Local,
            spawner: SpawnerSpec::Local,
            terminal_wrapper: TerminalWrapperSpec::HostShell,
            display_label: "Container:deadbeef".to_string(),
            path_translation: None,
        },
        std::sync::Arc::new(fresh::services::workspace_trust::WorkspaceTrust::permissive()),
        std::sync::Arc::new(fresh::services::env_provider::EnvProvider::inactive()),
    )?;
    harness.editor_mut().set_boot_authority(authority);

    harness.editor_mut().show_remote_indicator_popup();
    harness.render()?;

    let rows = popup_item_rows(&harness);
    let logs = rows
        .iter()
        .find(|(t, _, _)| t.contains("Show Build Logs"))
        .unwrap_or_else(|| {
            panic!("Connected/container popup lacks a Show Build Logs row. Rows: {rows:#?}")
        });
    assert_eq!(
        logs.1.as_deref(),
        Some("plugin:devcontainer_show_build_logs"),
        "Show Build Logs must dispatch the plugin show-build-logs handler. Row: {logs:?}"
    );
    assert!(
        !logs.2,
        "Show Build Logs must not be disabled. Row: {logs:?}"
    );
    Ok(())
}
