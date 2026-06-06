//! Per-session authority infrastructure (`Editor::set_session_authority`).
//!
//! The activation primitive the per-session authority model is built on
//! (see `docs/internal/AUTHORITY_DESIGN.md` §"Evolution: per-session
//! authority"). Unlike `set_boot_authority`, which fans one authority
//! across every window at boot, `set_session_authority` swaps a *single*
//! window's authority and only mirrors into the editor-wide cache when
//! that window is the active one.
//!
//! These tests lock in:
//!   1. swapping the active window's authority updates both the window's
//!      own `authority()` and the editor-wide `authority()` the rest of
//!      the editor reads;
//!   2. targeting a non-active / unknown window does not disturb the
//!      active authority — the guard that keeps distinct sessions from
//!      stomping each other once multi-session is live.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::services::authority::{
    Authority, AuthorityPayload, FilesystemSpec, SpawnerSpec, TerminalWrapperSpec,
};

fn container_authority(label: &str) -> Authority {
    Authority::from_plugin_payload(
        AuthorityPayload {
            filesystem: FilesystemSpec::Local,
            spawner: SpawnerSpec::Local,
            terminal_wrapper: TerminalWrapperSpec::HostShell,
            display_label: label.to_string(),
            path_translation: None,
        },
        std::sync::Arc::new(fresh::services::workspace_trust::WorkspaceTrust::permissive()),
        std::sync::Arc::new(fresh::services::env_provider::EnvProvider::inactive()),
    )
    .expect("local-backed payload is valid")
}

#[test]
fn set_session_authority_on_active_window_updates_window_and_editor() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    // Boots local — empty display label.
    assert_eq!(harness.editor_mut().authority().display_label, "");

    let active = harness.editor_mut().active_window_id();
    harness
        .editor_mut()
        .set_session_authority(active, container_authority("Container:abc"));

    // The editor-wide cache (read by the 100+ `self.authority` call sites)
    // reflects the swap…
    assert_eq!(
        harness.editor_mut().authority().display_label,
        "Container:abc"
    );
    // …and so does the window's own per-session handle.
    assert_eq!(
        harness
            .editor_mut()
            .active_window()
            .authority()
            .display_label,
        "Container:abc"
    );
    Ok(())
}

#[test]
fn install_authority_with_keepalive_queues_both_and_requests_restart() -> anyhow::Result<()> {
    // The path the `attachRemoteAgent` op lands on once its async connect
    // succeeds: a connection-backed authority is queued *alongside* its
    // keepalive, and a restart is requested so both restart loops adopt them
    // before the old editor is dropped.
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    // A real attach would pass an `KubeKeepalive`; the slot is opaque
    // `Box<dyn Any + Send>`, so any owned value exercises the wiring.
    let keepalive: Box<dyn std::any::Any + Send> = Box::new(());
    let remote_root = std::path::PathBuf::from("/workspace");
    harness.editor_mut().install_authority_with_keepalive(
        container_authority("Container:ka"),
        keepalive,
        remote_root.clone(),
    );

    // The authority is queued…
    let pending = harness.editor_mut().take_pending_authority();
    assert_eq!(
        pending.expect("authority queued").display_label,
        "Container:ka"
    );
    // …so is the keepalive…
    assert!(
        harness.editor_mut().take_pending_keepalive().is_some(),
        "keepalive queued alongside the authority"
    );
    // …and a restart was requested that re-roots the editor at the *remote*
    // workspace (not the local working dir) — the fix for the explorer /
    // quick-open / open-file all pointing at a host path absent in the pod.
    assert_eq!(
        harness.editor_mut().take_restart_dir(),
        Some(remote_root),
        "restart re-roots at the remote workspace"
    );
    Ok(())
}

#[test]
fn active_authority_follows_the_active_window_on_switch() -> anyhow::Result<()> {
    // Gap A: `set_active_window` must re-point the editor-wide authority at
    // the window it switches to, so a per-session remote/cloud backend
    // actually takes effect when its window becomes active (and the local
    // backend is restored when switching back). Without this, switching
    // windows moved the active *pointer* but left every filesystem/spawn call
    // site reading the previous window's authority.
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    let local = harness.editor_mut().active_window_id();
    // Boots local — empty display label.
    assert_eq!(harness.editor_mut().authority().display_label, "");

    // A second window (at a distinct root so it doesn't collapse onto the
    // first) carrying a container-labelled authority, set while it is *not*
    // active: the editor-wide authority must stay local.
    let remote_root = temp.path().join("remote");
    std::fs::create_dir_all(&remote_root)?;
    let remote = harness
        .editor_mut()
        .create_window_at(remote_root, "remote".to_string());
    harness
        .editor_mut()
        .set_session_authority(remote, container_authority("Container:remote"));
    assert_eq!(
        harness.editor_mut().authority().display_label,
        "",
        "setting a non-active window's authority must not change the active one"
    );

    // Switching to it adopts its authority editor-wide…
    harness.editor_mut().set_active_window(remote);
    assert_eq!(
        harness.editor_mut().authority().display_label,
        "Container:remote",
        "active authority follows the switched-to window"
    );

    // …and switching back restores the local authority.
    harness.editor_mut().set_active_window(local);
    assert_eq!(
        harness.editor_mut().authority().display_label,
        "",
        "switching back to the local window restores the local authority"
    );
    Ok(())
}

#[test]
fn set_session_authority_on_other_window_leaves_active_untouched() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    // Establish a known active authority first.
    let active = harness.editor_mut().active_window_id();
    harness
        .editor_mut()
        .set_session_authority(active, container_authority("Container:active"));
    assert_eq!(
        harness.editor_mut().authority().display_label,
        "Container:active"
    );

    // Target a window that isn't active (and, here, doesn't exist): the
    // editor-wide active authority must be left alone. This is the guard
    // that lets a background session's authority be swapped without
    // disturbing the foreground one once multi-session is live.
    let bogus = fresh_core::WindowId(9999);
    assert_ne!(bogus, active);
    harness
        .editor_mut()
        .set_session_authority(bogus, container_authority("Container:background"));

    assert_eq!(
        harness.editor_mut().authority().display_label,
        "Container:active",
        "swapping a non-active window must not change the active authority"
    );
    Ok(())
}
