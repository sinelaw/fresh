//! Shared scaffolding for the issue-#2570 dormant-SSH-workspace reproducers
//! (`orchestrator_dock_failed_reconnect.rs`,
//! `orchestrator_dock_dormant_ssh_badge.rs`).
//!
//! Persistence isolation: `Workspace::save`/`load` resolve their files through
//! `get_data_dir`, while the harness's boot discovery reads the
//! `DirectoryContext` it is built with. [`isolated_dir_context`] points BOTH
//! at the same per-test temp tree so phase-1 saves are what phase-2 discovery
//! finds — and nothing touches the real user data dir. It does that with the
//! thread-local `global_state::pin_data_dir`, so the isolation is private to the calling
//! test; the `std::env::set_var("XDG_DATA_HOME", …)` it used to use was
//! process-global and, once every root was folded into one `all_tests` binary,
//! moved every concurrent test's workspace store as well as this one's.

use std::path::{Path, PathBuf};

use fresh::config_io::DirectoryContext;
use fresh::services::authority::{RemoteAgentSpec, RemoteTransportSpec, SessionAuthoritySpec};

use super::global_state::{pin_path_with_dir_first, PathPin};
use super::harness::{EditorTestHarness, HarnessOptions};

/// Put the always-failing fake `ssh` (`tests/fixtures/fake-ssh`) at the front
/// of `$PATH` for the life of the returned guard — a deterministic
/// "unreachable host" with no network involved.
///
/// Hold the guard for the whole test: the three shims all provide a program
/// called `ssh` and only one can be in front at a time, which is why this is a
/// scoped pin rather than the process-lifetime `Once` it used to be (see
/// [`pin_path_with_dir_first`]).
#[must_use = "the shim leaves $PATH as soon as the guard is dropped"]
pub fn fake_ssh_on_path() -> PathPin {
    pin_shim_dir("tests/fixtures/fake-ssh")
}

/// Like [`fake_ssh_on_path`], but the shim **hangs** instead of failing
/// (`tests/fixtures/fake-ssh-hang`): a host that accepts the TCP connection
/// and never completes the SSH handshake, so the connect stays in-flight for
/// the whole test — the "shut-down host that drops packets" shape, which never
/// produces a prompt failure.
#[must_use = "the shim leaves $PATH as soon as the guard is dropped"]
pub fn hanging_fake_ssh_on_path() -> PathPin {
    pin_shim_dir("tests/fixtures/fake-ssh-hang")
}

/// Like [`fake_ssh_on_path`], but the shim **completes the connection slowly**
/// (`tests/fixtures/fake-ssh-slow`): it bootstraps the real agent locally so
/// file ops actually work, but throttles selected responses so the channel
/// becomes "connected but slow" — the bandwidth-throttled remote shape
/// (cf. `ProxyCommand ... | pv -qL 20k`). This is the state that stalls a
/// *synchronous* filesystem call, unlike the hang shim (which never
/// establishes the channel at all).
///
/// Behaviour is tuned per-test through the `FAKE_SSH_SLOW_*` variables the
/// shim reads; set them through [`PathPin::set_env`] so they are unset again
/// with the shim. Leaving them behind pointed a later test's shim at a gate
/// file inside a temp directory that had already been deleted.
#[must_use = "the shim leaves $PATH as soon as the guard is dropped"]
pub fn slow_fake_ssh_on_path() -> PathPin {
    pin_shim_dir("tests/fixtures/fake-ssh-slow")
}

fn pin_shim_dir(rel: &str) -> PathPin {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(rel);
    assert!(
        dir.join("ssh").exists(),
        "fake ssh shim missing at {}",
        dir.display()
    );
    pin_path_with_dir_first(&dir)
}

/// Re-exported from [`super::global_state`], where the five other roots that
/// need the same thing now find it too.
pub use super::global_state::isolated_dir_context;

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
