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

/// PTY availability guard — `create_window_with_terminal` spawns a real
/// shell, which the CI sandbox occasionally can't allocate.
fn pty_available() -> bool {
    portable_pty::native_pty_system()
        .openpty(portable_pty::PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

#[test]
fn new_local_session_is_born_with_its_own_local_authority() -> anyhow::Result<()> {
    // Repro for: open a devcontainer project + attach, then create a new
    // session for *another* project via the Orchestrator dock — the new
    // session kept acting through the devcontainer authority (its terminal
    // ran `docker exec` into the container, the status bar showed
    // `Container:…`) instead of its own local backend.
    //
    // Root cause: `create_window_with_terminal` built the new window from
    // `window_resources()`, which clones the *active* window's authority.
    // A new local session must be born under its own local authority.
    if !pty_available() {
        eprintln!("Skipping new-session authority test: PTY not available");
        return Ok(());
    }

    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;
    harness.tick_and_render()?;

    // Attach: the active (working-dir) session now runs under a container
    // authority — exactly the state after `editor.setAuthority(...)` +
    // restart, which `set_boot_authority` mirrors inline.
    let owner = harness.editor_mut().active_window_id();
    harness
        .editor_mut()
        .set_boot_authority(container_authority("Container:dc"));
    assert_eq!(
        harness.editor_mut().authority().display_label,
        "Container:dc"
    );

    // Orchestrator "New Session (Local)" for a different project — the
    // dispatcher hands `create_window_with_terminal` a fresh local
    // authority, so the window is born local.
    let proj_b = temp.path().join("projB");
    std::fs::create_dir_all(&proj_b)?;
    let born_authority = harness.editor().local_session_authority(&proj_b);
    let (new_win, _terminal, _buffer) = harness
        .editor_mut()
        .create_window_with_terminal(
            proj_b.clone(),
            "projB".into(),
            Some(proj_b.clone()),
            Some(vec!["sh".into(), "-c".into(), "sleep 60".into()]),
            Some("agent".into()),
            born_authority,
            None,
        )
        .map_err(anyhow::Error::msg)?;

    // The new session is active and runs under its OWN local authority,
    // not the devcontainer's.
    assert_eq!(harness.editor().active_window_id(), new_win);
    assert_eq!(
        harness.editor().authority().display_label,
        "",
        "a new local session must not inherit the devcontainer authority"
    );
    assert_eq!(
        harness
            .editor()
            .session(new_win)
            .unwrap()
            .authority()
            .display_label,
        "",
        "the new window's own authority must be local"
    );

    // The original devcontainer session still owns the container backend —
    // the fix scopes the authority per-session, it doesn't drop it.
    harness.editor_mut().set_active_window(owner);
    assert_eq!(
        harness.editor().authority().display_label,
        "Container:dc",
        "switching back to the devcontainer session restores its authority"
    );
    Ok(())
}

#[test]
fn attach_does_not_leak_authority_onto_background_windows() -> anyhow::Result<()> {
    // Repro for the pre-existing-session variant: with another project
    // already open as a background session, attaching a devcontainer to the
    // active project leaked the container authority onto *every* window, so
    // switching to the background project via the dock kept acting through
    // the devcontainer.
    //
    // `set_boot_authority` is the inline stand-in for the `install_authority`
    // restart (the same call every devcontainer e2e test uses); it must give
    // the active/owning window the new authority and leave background windows
    // on their own local backend.
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    let owner = harness.editor_mut().active_window_id();
    let bg_root = temp.path().join("projB");
    std::fs::create_dir_all(&bg_root)?;
    let background = harness
        .editor_mut()
        .create_window_at(bg_root, "projB".into());

    // Attach a devcontainer to the active project.
    harness
        .editor_mut()
        .set_boot_authority(container_authority("Container:dc"));

    // The owner runs under the container; the background project does not.
    assert_eq!(
        harness
            .editor()
            .session(owner)
            .unwrap()
            .authority()
            .display_label,
        "Container:dc",
        "the attaching session owns the container authority"
    );
    assert_eq!(
        harness
            .editor()
            .session(background)
            .unwrap()
            .authority()
            .display_label,
        "",
        "a background project must not inherit the devcontainer authority"
    );

    // Switching to the background project must use ITS authority (local),
    // not keep routing through the devcontainer — the reported bug.
    harness.editor_mut().set_active_window(background);
    assert_eq!(
        harness.editor().authority().display_label,
        "",
        "the switched-to project must use its own local authority"
    );
    Ok(())
}

#[test]
fn switching_to_a_dormant_remote_session_starts_reconnect() -> anyhow::Result<()> {
    // A remote session restored from disk comes back *dormant*: its backend
    // spec is remote but its live authority is still the local placeholder
    // (no keepalive). Switching to it must kick off a reconnect — the
    // per-window activation the per-session design calls for — surfaced
    // synchronously as a "Connecting…" status before the async connect runs.
    use fresh::services::authority::{RemoteAgentSpec, RemoteTransportSpec, SessionAuthoritySpec};

    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;

    // A background session, made dormant-remote: a known SSH backend spec but
    // no live connection (no keepalive), exactly the restored-from-disk state.
    let bg_root = temp.path().join("remoteproj");
    std::fs::create_dir_all(&bg_root)?;
    let dormant = harness
        .editor_mut()
        .create_window_at(bg_root, "remoteproj".into());
    harness.editor_mut().set_session_authority_spec(
        dormant,
        SessionAuthoritySpec::RemoteAgent(RemoteAgentSpec {
            transport: RemoteTransportSpec::Ssh {
                user: None,
                host: "dormant-host.invalid".into(),
                port: None,
                identity_file: None,
                remote_path: None,
                extra_args: Vec::new(),
            },
            base_env: Vec::new(),
            window: true,
            label: None,
            command: None,
        }),
    );

    // Switching to it starts the reconnect (the connect itself runs async on
    // the editor runtime and will fail against the bogus host — we assert the
    // synchronous initiation, not the outcome).
    harness.editor_mut().set_active_window(dormant);
    harness.render()?;
    harness.assert_screen_contains("Connecting");

    Ok(())
}

#[test]
fn trusting_one_session_does_not_change_another() -> anyhow::Result<()> {
    // Per-session trust: each open session owns its own WorkspaceTrust scoped
    // to its root, so a trust decision in one project never changes the live
    // trust level another open session's spawns are gated against (issue
    // #2280). Before this, every session shared one trust `Arc`, so trusting
    // one folder trusted them all.
    use fresh::services::workspace_trust::TrustLevel;

    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;
    let session_a = harness.editor_mut().active_window_id();
    let root_b = temp.path().join("projB");
    std::fs::create_dir_all(&root_b)?;
    let session_b = harness
        .editor_mut()
        .create_window_at(root_b, "projB".into());

    // Record distinct decisions in the two sessions.
    harness
        .editor()
        .session(session_a)
        .unwrap()
        .authority()
        .workspace_trust
        .set_level(TrustLevel::Trusted);
    harness
        .editor()
        .session(session_b)
        .unwrap()
        .authority()
        .workspace_trust
        .set_level(TrustLevel::Restricted);

    // Neither decision leaked: a shared handle would have left both at the
    // last value set (Restricted).
    assert_eq!(
        harness
            .editor()
            .session(session_a)
            .unwrap()
            .authority()
            .workspace_trust
            .level(),
        TrustLevel::Trusted,
        "session A keeps its own Trusted decision when B is set Restricted"
    );
    assert_eq!(
        harness
            .editor()
            .session(session_b)
            .unwrap()
            .authority()
            .workspace_trust
            .level(),
        TrustLevel::Restricted,
        "session B keeps its own Restricted decision"
    );
    Ok(())
}

#[test]
fn activating_env_in_one_session_does_not_affect_another() -> anyhow::Result<()> {
    // Per-session env: each session owns its own EnvProvider, so activating
    // an env (venv/direnv/mise) in one project never activates it for another
    // open session (issue #2280). Before this, every session shared one
    // EnvProvider handle.
    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;
    let session_a = harness.editor_mut().active_window_id();
    let root_b = temp.path().join("projB");
    std::fs::create_dir_all(&root_b)?;
    let session_b = harness
        .editor_mut()
        .create_window_at(root_b, "projB".into());

    // Activate an env in session A.
    harness
        .editor()
        .session(session_a)
        .unwrap()
        .authority()
        .env_provider
        .set("export FRESH_TEST=1".into(), None);

    assert!(
        harness
            .editor()
            .session(session_a)
            .unwrap()
            .authority()
            .env_provider
            .is_active(),
        "session A's env is active after activation"
    );
    assert!(
        !harness
            .editor()
            .session(session_b)
            .unwrap()
            .authority()
            .env_provider
            .is_active(),
        "activating an env in session A must not activate session B's env"
    );
    Ok(())
}

/// Regression (orchestrator "Trust restarted everything"): activating an env
/// via `setEnv` — the path the env-manager drives on `trust_changed` for a
/// shell/venv project — must NOT request a process-wide editor restart.
///
/// The `EnvProvider` is updated in place and every *new* spawn inherits it, so
/// no rebuild is needed. The old `handle_set_env` called `request_restart`,
/// which rebuilds the whole editor process — dropping every window's
/// `terminal_manager` (all orchestrator sessions' PTYs) and reconstructing the
/// orchestrator plugin (closing the dock). Asserting `!should_restart()` after
/// activation locks that out. Drives `PluginCommand::SetEnv` directly (what
/// `editor.setEnv` dispatches) rather than poking the provider, so it covers
/// the actual restart decision.
#[cfg(feature = "plugins")]
#[test]
fn activating_env_does_not_restart_the_editor() -> anyhow::Result<()> {
    use fresh::services::workspace_trust::TrustLevel;
    use fresh_core::api::PluginCommand;

    let temp = tempfile::tempdir()?;
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new().with_working_dir(temp.path().to_path_buf()),
    )?;
    let active = harness.editor_mut().active_window_id();
    // Env activation only applies in a Trusted workspace (`handle_set_env`).
    harness
        .editor()
        .session(active)
        .unwrap()
        .authority()
        .workspace_trust
        .set_level(TrustLevel::Trusted);

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetEnv {
            snippet: "export FRESH_TEST=1".into(),
            dir: None,
        })?;

    assert!(
        harness
            .editor()
            .session(active)
            .unwrap()
            .authority()
            .env_provider
            .is_active(),
        "env activated in place"
    );
    assert!(
        !harness.editor().should_restart(),
        "activating an env must not request a process-wide editor restart \
         (a rebuild tears down other sessions' terminals and closes the dock)"
    );
    Ok(())
}
