//! Shared scaffolding for the issue-#2570 dormant-SSH-workspace reproducers
//! (`orchestrator_dock_failed_reconnect.rs`,
//! `orchestrator_dock_dormant_ssh_badge.rs`).
//!
//! Persistence isolation: `Workspace::save`/`load` live under the *global*
//! `$XDG_DATA_HOME/fresh`, while the harness's boot discovery reads the
//! `DirectoryContext` it is built with. [`isolated_dir_context`] points BOTH
//! at the same per-test temp tree so phase-1 saves are what phase-2 discovery
//! finds — and nothing touches the real user data dir. Setting the env var is
//! process-global, so each test binary using this module must hold a single
//! test (the same constraint `remote_restore_terminal_e2e.rs` documents).

use std::path::{Path, PathBuf};
use std::sync::Once;

use fresh::config_io::DirectoryContext;
use fresh::services::authority::{RemoteAgentSpec, RemoteTransportSpec, SessionAuthoritySpec};

use super::harness::{EditorTestHarness, HarnessOptions};

static PATH_INIT: Once = Once::new();

/// Prepend the fake-ssh fixtures dir to PATH once, so `Command::new("ssh")`
/// resolves to the always-failing shim (`tests/fixtures/fake-ssh`) — a
/// deterministic "unreachable host" with no network involved. `Once` keeps
/// the process-global env mutation from racing across tests in a binary.
pub fn ensure_fake_ssh_on_path() {
    PATH_INIT.call_once(|| {
        let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fake-ssh");
        assert!(
            dir.join("ssh").exists(),
            "fake ssh shim missing at {}",
            dir.display()
        );
        let old = std::env::var("PATH").unwrap_or_default();
        std::env::set_var("PATH", format!("{}:{old}", dir.display()));
    });
}

/// Isolate ALL editor persistence into `base`: `$XDG_DATA_HOME/fresh` is
/// where workspace save/load live, and the returned `DirectoryContext`'s
/// `data_dir` is the SAME path — so phase-1 saves, boot discovery, and any
/// later save all agree, inside the test's temp tree.
pub fn isolated_dir_context(base: &Path) -> DirectoryContext {
    let xdg_data = base.join("xdg-data");
    std::fs::create_dir_all(&xdg_data).unwrap();
    std::env::set_var("XDG_DATA_HOME", &xdg_data);
    DirectoryContext {
        data_dir: xdg_data.join("fresh"),
        config_dir: base.join("config"),
        home_dir: Some(base.join("home")),
        documents_dir: None,
        downloads_dir: None,
    }
}

/// An SSH `authority_spec` for a host the fake shim "fails to reach".
pub fn dead_ssh_spec(remote_path: &Path) -> SessionAuthoritySpec {
    SessionAuthoritySpec::RemoteAgent(RemoteAgentSpec {
        transport: RemoteTransportSpec::Ssh {
            user: Some("root".to_string()),
            host: "dead-host".to_string(),
            port: Some(2222),
            identity_file: None,
            remote_path: Some(remote_path.to_string_lossy().into_owned()),
            extra_args: Vec::new(),
        },
        base_env: Vec::new(),
        window: true,
        label: Some("ssh-dead".to_string()),
        command: None,
    })
}

pub fn canonical_mkdir(base: &Path, name: &str) -> PathBuf {
    let p = base.join(name);
    std::fs::create_dir_all(&p).unwrap();
    p.canonicalize().unwrap_or(p)
}

/// The "previous session" both reproducers restart from: it leaves behind a
/// local project workspace (with `local_marker.txt` open) and a persisted
/// SSH workspace labelled `ssh-dead` (with `remote_notes.txt` open and a
/// `RemoteAgent` backend spec).
pub fn persist_previous_session(
    dir_context: &DirectoryContext,
    project: &Path,
    remote_root: &Path,
    with_plugins: bool,
) {
    let mut opts = HarnessOptions::new()
        .with_working_dir(project.to_path_buf())
        .with_shared_dir_context(dir_context.clone());
    if !with_plugins {
        opts = opts.with_empty_plugins_dir();
    }
    let mut h = EditorTestHarness::create(120, 36, opts).unwrap();

    std::fs::write(project.join("local_marker.txt"), "LOCAL MARKER\n").unwrap();
    h.open_file(&project.join("local_marker.txt")).unwrap();

    // The remote session: give it real content so its on-disk workspace has
    // something a stray save could clobber, then tag it remote. The spec is
    // set *after* the content so nothing tries to connect in this phase.
    std::fs::write(remote_root.join("remote_notes.txt"), "REMOTE NOTES\n").unwrap();
    let a = h
        .editor_mut()
        .create_window_at(remote_root.to_path_buf(), "ssh-dead".to_string());
    h.editor_mut().set_active_window(a);
    h.open_file(&remote_root.join("remote_notes.txt")).unwrap();
    h.editor_mut()
        .set_session_authority_spec(a, dead_ssh_spec(remote_root));

    h.editor_mut().save_all_windows_workspaces().unwrap();
}
