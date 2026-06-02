//! Authority — the single backend slot for "where does the editor act?"
//!
//! Every primitive the editor exposes — file I/O, integrated terminal,
//! plugin `spawnProcess`, formatter, LSP server spawn, file watcher,
//! find-in-files, save, recovery — routes through the active `Authority`.
//! There is exactly one authority per `Editor` at any moment.
//!
//! Transitions are atomic and destructive: `Editor::install_authority`
//! queues the replacement, then piggy-backs on the existing
//! `request_restart` flow so the whole `Editor` is dropped and rebuilt
//! around the new authority. Every cached `Arc<dyn FileSystem>`, LSP
//! handle, terminal PTY, plugin state, and in-flight task goes away
//! with the old `Editor`; there is no in-place swap and no half-
//! transitioned window. See `docs/internal/AUTHORITY_DESIGN.md`.
//!
//! Authority is opaque to core code. The four fields below are the
//! entire contract; nothing else inspects whether the backend is local,
//! SSH, a container, or something a plugin invented.
//!
//! ## Construction
//!
//! - `Authority::local(trust, env)` — host filesystem + host spawner + host
//!   shell. Always available; the editor boots with this. Trust + env are
//!   mandatory shared handles.
//! - `Authority::ssh(filesystem, spawner, long_running, params, remote_dir,
//!   trust, env)` — used by the `fresh user@host:path` startup flow. `params`
//!   and `remote_dir` build the `ssh -t …` terminal wrapper so the integrated
//!   terminal opens on the remote host.
//! - `Authority::from_plugin_payload(payload, trust, env)` — built from the
//!   `editor.setAuthority(...)` plugin op. The payload is a tagged shape
//!   (filesystem kind + spawner kind + terminal wrapper + label); it stays
//!   small and additive so we can grow new kinds without breaking the
//!   plugin contract.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::model::filesystem::{FileSystem, StdFileSystem};
use crate::services::remote::{
    build_eks_terminal_args, build_ssh_terminal_args, ConnectionParams, EksTarget,
    LocalLongRunningSpawner, LocalProcessSpawner, LongRunningSpawner, ProcessSpawner,
};
use crate::services::workspace_trust::WorkspaceTrust;

/// Plugin-supplied form of the host↔remote workspace mapping. Plugins
/// build this from their own knowledge (e.g. the devcontainer plugin
/// already has `editor.getCwd()` for the host root and
/// `result.remoteWorkspaceFolder` for the in-container root). Strings
/// because the wire format is JSON; paths get parsed in
/// `Authority::from_plugin_payload`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathTranslationSpec {
    pub host_root: String,
    pub remote_root: String,
}

/// Symmetric path translation between host and remote workspace
/// roots. Owned by the active [`Authority`] when the backend lives in
/// a container (or any other place where the workspace is mounted at a
/// different path than its on-host location). Local authorities and
/// SSH leave this field unset.
///
/// LSP URIs are the primary consumer: the editor's buffer file paths
/// are host-side, but the LSP server is on the other side of the
/// mount and only knows the remote-side path. We translate at the
/// boundary so the editor can keep using host paths internally and
/// the LSP keeps seeing the paths it expects.
#[derive(Debug, Clone)]
pub struct PathTranslation {
    pub host_root: PathBuf,
    pub remote_root: PathBuf,
}

impl PathTranslation {
    /// Map a host-side path under `host_root` to its remote-side
    /// counterpart. Returns `None` for paths outside the workspace
    /// (e.g. system headers, library sources) — those are passed
    /// through unchanged so the caller can decide whether to forward
    /// them as-is or drop them.
    pub fn host_to_remote(&self, host: &Path) -> Option<PathBuf> {
        let rel = host.strip_prefix(&self.host_root).ok()?;
        Some(self.remote_root.join(rel))
    }

    /// Map a remote-side path under `remote_root` back to its
    /// host-side counterpart. Same outside-the-workspace caveat as
    /// [`Self::host_to_remote`].
    pub fn remote_to_host(&self, remote: &Path) -> Option<PathBuf> {
        let rel = remote.strip_prefix(&self.remote_root).ok()?;
        Some(self.host_root.join(rel))
    }
}

/// How the integrated terminal is launched under this authority.
///
/// The terminal manager unconditionally honours this — there is no
/// "no wrapper" branch.  For local authority, the wrapper command is the
/// detected host shell with no extra args; `manages_cwd` is false so the
/// terminal manager calls `CommandBuilder::cwd()` itself.  Authorities
/// that re-parent the shell (e.g. `docker exec -w <workspace>`) set
/// `manages_cwd = true` so cwd is left to the wrapper's args.
#[derive(Debug, Clone)]
pub struct TerminalWrapper {
    /// Command to execute (e.g. the host shell, `"docker"`, `"ssh"`).
    pub command: String,
    /// Arguments passed before any user input — usually the flags that
    /// drop the user into an interactive shell at the right place.
    pub args: Vec<String>,
    /// If true, `args` already establishes the working directory and the
    /// terminal manager must skip `CommandBuilder::cwd()`. For local
    /// authorities this is false so the host shell honours the per-
    /// terminal cwd the editor passes in.
    pub manages_cwd: bool,
}

impl TerminalWrapper {
    /// Wrap the detected host shell with no extra args. Cwd is set by
    /// the terminal manager from the spawn call.
    pub fn host_shell() -> Self {
        Self {
            command: crate::services::terminal::manager::detect_shell(),
            args: Vec::new(),
            manages_cwd: false,
        }
    }

    /// Re-parent the integrated terminal onto the remote host over SSH:
    /// `ssh -t … user@host 'cd <dir>; exec $SHELL -l'`. Like the
    /// `docker exec -w …` wrapper, this pins cwd through its own args
    /// (`cd <dir>`), so `manages_cwd` is true and the terminal manager
    /// must not call `CommandBuilder::cwd()` with a remote path the local
    /// PTY can't honour. Without this, SSH authorities would fall back to
    /// the local host shell and the embedded terminal would run on the
    /// *local* machine instead of the remote host.
    pub fn ssh(params: &ConnectionParams, remote_dir: Option<&str>) -> Self {
        Self {
            command: "ssh".to_string(),
            args: build_ssh_terminal_args(params, remote_dir),
            manages_cwd: true,
        }
    }

    /// Re-parent the integrated terminal into an EKS pod via
    /// `kubectl exec -it … -- sh -lc 'cd <ws>; exec $SHELL -l'`. Same
    /// re-parenting contract as [`Self::ssh`] / the `docker exec -w …`
    /// wrapper: cwd is pinned through the wrapper's own args, so
    /// `manages_cwd` is true and the terminal manager must not hand the
    /// local PTY a pod-side cwd it can't honour.
    pub fn eks(target: &EksTarget) -> Self {
        Self {
            command: "kubectl".to_string(),
            args: build_eks_terminal_args(target),
            manages_cwd: true,
        }
    }

    /// Apply the user's `terminal.shell` config override on top of this
    /// wrapper. The override replaces `command` and `args` only when the
    /// wrapper leaves cwd management to the terminal manager
    /// (`manages_cwd == false`) — that is, for the host-shell wrapper.
    /// Authorities that re-parent the shell (e.g. `docker exec -w …`,
    /// `ssh …`) pin cwd through their own args and are left untouched so
    /// the re-parenting stays intact.
    pub fn with_user_shell_override(
        mut self,
        shell: Option<&crate::config::TerminalShellConfig>,
    ) -> Self {
        if let Some(shell) = shell {
            if !self.manages_cwd {
                self.command = shell.command.clone();
                self.args = shell.args.clone();
            }
        }
        self
    }
}

/// Tagged payload describing how to build an authority from a plugin.
///
/// Kept intentionally small and explicit. Adding a new spawner or
/// filesystem kind means adding a new variant here and a constructor in
/// `Authority::from_plugin_payload`. Plugins consuming the API see only
/// the `kind` discriminator and the kind-specific params, so old payloads
/// keep working as new kinds are added.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthorityPayload {
    pub filesystem: FilesystemSpec,
    pub spawner: SpawnerSpec,
    pub terminal_wrapper: TerminalWrapperSpec,
    /// Status-bar / explorer label. Empty = no label rendered.
    #[serde(default)]
    pub display_label: String,
    /// Optional host↔remote workspace path mapping. Devcontainer-style
    /// authorities supply both the host workspace path (the editor's
    /// `cwd` at attach time) and the in-container `remoteWorkspaceFolder`
    /// so URIs traveling to/from the LSP get translated symmetrically.
    /// SSH and local authorities leave this unset.
    #[serde(default)]
    pub path_translation: Option<PathTranslationSpec>,
}

/// Filesystem kind chosen by a plugin payload.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum FilesystemSpec {
    /// Use the host filesystem. Devcontainers fall here because the
    /// workspace is mounted into the container, so file paths translate
    /// 1:1 between host and container.
    Local,
}

/// Process-spawner kind chosen by a plugin payload.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum SpawnerSpec {
    /// Spawn on the host. Equivalent to `LocalProcessSpawner`.
    ///
    /// Environment-manager activation is *not* expressed here — env is a live
    /// provider set via `editor.setEnv` (see `services::env_provider`), not a
    /// backend rebuilt from a payload. `SpawnerSpec` is only for choosing the
    /// backend (local vs container).
    Local,
    /// Run via `docker exec` against a long-lived container. The plugin
    /// manages the container lifecycle (e.g. via `editor.spawnHostProcess`
    /// to invoke `devcontainer up`) and hands us the container id once it
    /// is ready.
    ///
    /// `env` is the captured `userEnvProbe` snapshot from inside the
    /// container — typically `PATH`, `HOME`, `LANG`, and any vars the
    /// user's profile exports. It's applied to every `docker exec`
    /// (one-shot spawns, LSP/long-running, `command_exists` probes)
    /// so plugins-installed binaries on `~/.local/bin` (or any
    /// shell-only PATH) actually resolve. Empty when `userEnvProbe`
    /// is `none` or the probe fails.
    DockerExec {
        container_id: String,
        #[serde(default)]
        user: Option<String>,
        #[serde(default)]
        workspace: Option<String>,
        /// Captured `userEnvProbe` env. Order is preserved so
        /// per-call `env` can layer over it deterministically; the
        /// list of pairs (rather than a HashMap) keeps `docker exec
        /// -e` ordering explicit.
        #[serde(default)]
        env: Vec<(String, String)>,
    },
}

/// Terminal-wrapper kind chosen by a plugin payload.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum TerminalWrapperSpec {
    /// Use the detected host shell.
    HostShell,
    /// Use an explicit command + args (e.g. `docker exec -it -u <user>
    /// -w <workspace> <id> bash -l`). `manages_cwd` defaults to true
    /// because that is the only sensible choice for re-parented shells.
    Explicit {
        command: String,
        args: Vec<String>,
        #[serde(default = "default_true")]
        manages_cwd: bool,
    },
}

fn default_true() -> bool {
    true
}

/// The single backend slot. Replaces the old quartet of `filesystem`,
/// `process_spawner`, `terminal_wrapper`, and `authority_display_string`
/// fields on `Editor`. Cloned cheaply via `Arc`s.
#[derive(Clone)]
pub struct Authority {
    pub filesystem: Arc<dyn FileSystem + Send + Sync>,
    pub process_spawner: Arc<dyn ProcessSpawner>,
    /// Spawner for long-lived stdio processes — LSP servers today, tool
    /// agents tomorrow. Container authorities wire this to a
    /// `docker exec -i` variant so servers run inside the container
    /// rather than on the host. Without it, LSP bypasses the authority
    /// entirely (see `AUTHORITY_DESIGN.md` principle 2).
    pub long_running_spawner: Arc<dyn LongRunningSpawner>,
    pub terminal_wrapper: TerminalWrapper,
    /// Status-bar / file-explorer label. Empty means render nothing.
    /// SSH leaves this empty and lets the status bar fall back to the
    /// filesystem's `remote_connection_info()` so disconnect annotations
    /// stay in one place.
    pub display_label: String,
    /// Host↔remote workspace path mapping for backends where the
    /// workspace is mounted at a different path than its on-host
    /// location. The dev-container authority populates this so LSP
    /// URIs translate at the host/container boundary; local and SSH
    /// authorities leave it `None` and URIs flow through unchanged.
    pub path_translation: Option<PathTranslation>,
    /// Workspace Trust state gating execution under this authority — mandatory,
    /// passed into every constructor and held by each spawner (no optional, no
    /// post-hoc wrapping). It's the same `Arc` the server owns, so the command
    /// palette / prompt mutate the level through it and every spawner sees it
    /// live. A spawner literally cannot be built without it.
    pub workspace_trust: Arc<WorkspaceTrust>,
    /// Live environment provider (the activated venv/direnv/mise recipe) gating
    /// what env every spawn carries — shared, mutated in place via the
    /// `setEnv`/`clearEnv` plugin ops, never a stored snapshot. Same `Arc` the
    /// server owns; born in `main.rs` alongside trust.
    pub env_provider: Arc<crate::services::env_provider::EnvProvider>,
}

impl Authority {
    /// Default boot-time authority: host filesystem, host process
    /// spawner, host shell wrapper, gated by `trust`. The editor starts
    /// here on every startup; SSH or plugin-installed authorities replace
    /// it later (carrying the same `trust`).
    pub fn local(
        trust: Arc<WorkspaceTrust>,
        env: Arc<crate::services::env_provider::EnvProvider>,
    ) -> Self {
        Self {
            filesystem: Arc::new(StdFileSystem),
            process_spawner: Arc::new(LocalProcessSpawner::new(
                Arc::clone(&env),
                Arc::clone(&trust),
            )),
            long_running_spawner: Arc::new(LocalLongRunningSpawner::new(
                Arc::clone(&env),
                Arc::clone(&trust),
            )),
            terminal_wrapper: TerminalWrapper::host_shell(),
            display_label: String::new(),
            path_translation: None,
            workspace_trust: trust,
            env_provider: env,
        }
    }

    /// Build an SSH authority. The caller already holds the connection
    /// (and its keepalive resources) so we just wire the parts in. Label
    /// is left empty — the status bar falls back to the filesystem's own
    /// `remote_connection_info()` which knows how to annotate disconnect.
    ///
    /// `long_running_spawner` is an SSH-routed spawner
    /// ([`RemoteLongRunningSpawner`](crate::services::remote::RemoteLongRunningSpawner)),
    /// so LSP servers run on the remote host rather than the host-local
    /// fallback that earlier versions used.
    ///
    /// `params` / `remote_dir` build the integrated-terminal wrapper so the
    /// embedded terminal opens a shell *on the remote host* (`ssh -t …`)
    /// rooted at the workspace, matching where the filesystem and spawners
    /// already act. Earlier versions hardcoded the local host shell here,
    /// so terminals silently ran on the local machine.
    pub fn ssh(
        filesystem: Arc<dyn FileSystem + Send + Sync>,
        process_spawner: Arc<dyn ProcessSpawner>,
        long_running_spawner: Arc<dyn LongRunningSpawner>,
        params: &ConnectionParams,
        remote_dir: Option<&str>,
        trust: Arc<WorkspaceTrust>,
        env: Arc<crate::services::env_provider::EnvProvider>,
    ) -> Self {
        Self {
            filesystem,
            process_spawner,
            long_running_spawner,
            terminal_wrapper: TerminalWrapper::ssh(params, remote_dir),
            display_label: String::new(),
            path_translation: None,
            workspace_trust: trust,
            env_provider: env,
        }
    }

    /// Build an EKS authority: the remote-agent stack (filesystem + spawners,
    /// already wired to the pod's `kubectl exec` agent channel) plus a
    /// terminal wrapper that opens a shell *inside the pod*.
    ///
    /// Mirrors [`Self::ssh`] — the caller owns the connection and its
    /// keepalive resources and threads the parts in. Unlike SSH, the label
    /// is set from the target (there is no filesystem-side
    /// `remote_connection_info()` that knows about an EKS pod, so identity
    /// lives in the label per `AUTHORITY_DESIGN.md` principle 9). Path
    /// translation is unset: the editor operates directly in the pod's path
    /// space, exactly as SSH does.
    pub fn eks(
        filesystem: Arc<dyn FileSystem + Send + Sync>,
        process_spawner: Arc<dyn ProcessSpawner>,
        long_running_spawner: Arc<dyn LongRunningSpawner>,
        target: &EksTarget,
        trust: Arc<WorkspaceTrust>,
        env: Arc<crate::services::env_provider::EnvProvider>,
    ) -> Self {
        Self {
            filesystem,
            process_spawner,
            long_running_spawner,
            terminal_wrapper: TerminalWrapper::eks(target),
            display_label: target.display(),
            path_translation: None,
            workspace_trust: trust,
            env_provider: env,
        }
    }

    /// Build an authority from a plugin payload (the data carried by the
    /// `editor.setAuthority(...)` op), gated by `trust` (the editor passes its
    /// live trust handle so the new authority shares it). All translation from
    /// "kind + params" to concrete `Arc<dyn …>` lives here and nowhere else.
    pub fn from_plugin_payload(
        payload: AuthorityPayload,
        trust: Arc<WorkspaceTrust>,
        env: Arc<crate::services::env_provider::EnvProvider>,
    ) -> Result<Self, AuthorityPayloadError> {
        let filesystem: Arc<dyn FileSystem + Send + Sync> = match payload.filesystem {
            FilesystemSpec::Local => Arc::new(StdFileSystem),
        };

        // Both spawner traits need the docker-exec params when the
        // payload is a container, so destructure once and reuse.
        let (process_spawner, long_running_spawner): (
            Arc<dyn ProcessSpawner>,
            Arc<dyn LongRunningSpawner>,
        ) = match payload.spawner {
            SpawnerSpec::Local => (
                Arc::new(LocalProcessSpawner::new(
                    Arc::clone(&env),
                    Arc::clone(&trust),
                )),
                Arc::new(LocalLongRunningSpawner::new(
                    Arc::clone(&env),
                    Arc::clone(&trust),
                )),
            ),
            SpawnerSpec::DockerExec {
                container_id,
                user,
                workspace,
                env,
            } => (
                Arc::new(
                    crate::services::authority::docker_spawner::DockerExecSpawner::with_env(
                        container_id.clone(),
                        user.clone(),
                        workspace.clone(),
                        env.clone(),
                        Arc::clone(&trust),
                    ),
                ),
                Arc::new(
                    crate::services::authority::docker_spawner::DockerLongRunningSpawner::with_env(
                        container_id,
                        user,
                        workspace,
                        env,
                        Arc::clone(&trust),
                    ),
                ),
            ),
        };

        let terminal_wrapper = match payload.terminal_wrapper {
            TerminalWrapperSpec::HostShell => TerminalWrapper::host_shell(),
            TerminalWrapperSpec::Explicit {
                command,
                args,
                manages_cwd,
            } => TerminalWrapper {
                command,
                args,
                manages_cwd,
            },
        };

        let path_translation = payload.path_translation.map(|spec| PathTranslation {
            host_root: PathBuf::from(spec.host_root),
            remote_root: PathBuf::from(spec.remote_root),
        });

        Ok(Self {
            filesystem,
            process_spawner,
            long_running_spawner,
            terminal_wrapper,
            display_label: payload.display_label,
            path_translation,
            workspace_trust: trust,
            env_provider: env,
        })
    }
}

/// Error from translating a plugin payload into a live authority.
/// Reserved for future kinds that might fail to construct (e.g. invalid
/// connection parameters); local-only payloads currently never fail.
#[derive(Debug, thiserror::Error)]
pub enum AuthorityPayloadError {
    #[error("invalid authority payload: {0}")]
    Invalid(String),
}

mod docker_spawner;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_authority_uses_host_shell_with_no_args() {
        let auth = Authority::local(
            Arc::new(WorkspaceTrust::permissive()),
            Arc::new(crate::services::env_provider::EnvProvider::inactive()),
        );
        assert!(!auth.terminal_wrapper.command.is_empty());
        assert!(auth.terminal_wrapper.args.is_empty());
        assert!(!auth.terminal_wrapper.manages_cwd);
        assert_eq!(auth.display_label, "");
    }

    #[test]
    fn eks_terminal_wrapper_reparents_into_pod() {
        let target = EksTarget {
            context: Some("prod".into()),
            namespace: "dev".into(),
            pod: "pod-1".into(),
            container: None,
            workspace: Some("/workspace".into()),
        };
        let wrapper = TerminalWrapper::eks(&target);
        assert_eq!(wrapper.command, "kubectl");
        // Re-parented shell must pin cwd through its own args.
        assert!(wrapper.manages_cwd);
        assert_eq!(wrapper.args[0], "--context");
        assert!(wrapper.args.iter().any(|a| a == "-it"));
        assert!(wrapper.args.iter().any(|a| a == "pod-1"));
        // User shell override is a no-op for a cwd-managing wrapper, so the
        // re-parenting into the pod stays intact.
        let override_shell = crate::config::TerminalShellConfig {
            command: "/usr/local/bin/fish".into(),
            args: vec![],
        };
        let after = wrapper
            .clone()
            .with_user_shell_override(Some(&override_shell));
        assert_eq!(after.command, "kubectl");
    }

    #[test]
    fn from_plugin_payload_local_yields_host_shell() {
        let payload = AuthorityPayload {
            filesystem: FilesystemSpec::Local,
            spawner: SpawnerSpec::Local,
            terminal_wrapper: TerminalWrapperSpec::HostShell,
            display_label: String::new(),
            path_translation: None,
        };
        let auth = Authority::from_plugin_payload(
            payload,
            Arc::new(WorkspaceTrust::permissive()),
            Arc::new(crate::services::env_provider::EnvProvider::inactive()),
        )
        .expect("local payload is valid");
        assert!(!auth.terminal_wrapper.command.is_empty());
        assert!(auth.terminal_wrapper.args.is_empty());
    }

    #[test]
    fn payload_roundtrips_through_serde_json() {
        // The plugin op carries the payload as opaque JSON through
        // `fresh-core`; this test nails down the wire shape so we
        // don't silently break plugins when the struct evolves.
        let json = serde_json::json!({
            "filesystem": { "kind": "local" },
            "spawner": {
                "kind": "docker-exec",
                "container_id": "abc123",
                "user": "vscode",
                "workspace": "/workspaces/proj"
            },
            "terminal_wrapper": {
                "kind": "explicit",
                "command": "docker",
                "args": ["exec", "-it", "abc123", "bash", "-l"],
                "manages_cwd": true
            },
            "display_label": "Container:abc123"
        });
        let payload: AuthorityPayload =
            serde_json::from_value(json).expect("json matches payload schema");
        let auth = Authority::from_plugin_payload(
            payload,
            Arc::new(WorkspaceTrust::permissive()),
            Arc::new(crate::services::env_provider::EnvProvider::inactive()),
        )
        .expect("docker payload is valid");
        assert_eq!(auth.terminal_wrapper.command, "docker");
        assert!(auth.terminal_wrapper.manages_cwd);
        assert_eq!(auth.display_label, "Container:abc123");
    }

    #[test]
    fn payload_accepts_docker_exec_env_pairs() {
        // The captured `userEnvProbe` env carries the in-container
        // `PATH`/`HOME`/etc. so LSP `command_exists` can find binaries
        // that live in shell-only PATHs (e.g. `~/.local/bin`).
        let json = serde_json::json!({
            "filesystem": { "kind": "local" },
            "spawner": {
                "kind": "docker-exec",
                "container_id": "abc123",
                "user": "vscode",
                "workspace": "/workspaces/proj",
                "env": [
                    ["PATH", "/home/vscode/.local/bin:/usr/bin"],
                    ["LANG", "C.UTF-8"]
                ]
            },
            "terminal_wrapper": { "kind": "host-shell" }
        });
        let payload: AuthorityPayload =
            serde_json::from_value(json).expect("env field is accepted");
        if let SpawnerSpec::DockerExec { env, .. } = &payload.spawner {
            assert_eq!(env.len(), 2);
            assert_eq!(
                env[0],
                ("PATH".into(), "/home/vscode/.local/bin:/usr/bin".into())
            );
            assert_eq!(env[1], ("LANG".into(), "C.UTF-8".into()));
        } else {
            panic!("expected docker-exec spawner");
        }
        // And the omitted-field form still parses (the field defaults
        // to empty), so older plugins that don't populate it stay
        // wire-compatible.
        let json_no_env = serde_json::json!({
            "filesystem": { "kind": "local" },
            "spawner": {
                "kind": "docker-exec",
                "container_id": "abc123"
            },
            "terminal_wrapper": { "kind": "host-shell" }
        });
        let payload2: AuthorityPayload =
            serde_json::from_value(json_no_env).expect("env is optional");
        if let SpawnerSpec::DockerExec { env, .. } = payload2.spawner {
            assert!(env.is_empty());
        } else {
            panic!("expected docker-exec spawner");
        }
    }

    #[test]
    fn payload_defaults_manages_cwd_to_true_for_explicit_wrapper() {
        // Per the schema, `manages_cwd` is optional in the JSON and
        // defaults to true because re-parented shells almost always
        // want it that way.
        let json = serde_json::json!({
            "filesystem": { "kind": "local" },
            "spawner": { "kind": "local" },
            "terminal_wrapper": {
                "kind": "explicit",
                "command": "bash",
                "args": []
            }
        });
        let payload: AuthorityPayload =
            serde_json::from_value(json).expect("manages_cwd is optional");
        let auth = Authority::from_plugin_payload(
            payload,
            Arc::new(WorkspaceTrust::permissive()),
            Arc::new(crate::services::env_provider::EnvProvider::inactive()),
        )
        .expect("payload is valid");
        assert!(auth.terminal_wrapper.manages_cwd);
        assert_eq!(auth.display_label, "");
    }

    #[test]
    fn user_shell_override_replaces_host_shell_wrapper() {
        let override_shell = crate::config::TerminalShellConfig {
            command: "/usr/local/bin/fish".into(),
            args: vec!["-l".into(), "-i".into()],
        };
        let wrapper = TerminalWrapper::host_shell().with_user_shell_override(Some(&override_shell));
        assert_eq!(wrapper.command, "/usr/local/bin/fish");
        assert_eq!(wrapper.args, vec!["-l".to_string(), "-i".to_string()]);
        assert!(!wrapper.manages_cwd);
    }

    #[test]
    fn user_shell_override_is_noop_when_wrapper_manages_cwd() {
        // Docker/SSH-style wrappers set `manages_cwd = true`; replacing
        // their command would drop the re-parenting args and spawn the
        // user's shell on the host, defeating the authority.
        let docker = TerminalWrapper {
            command: "docker".into(),
            args: vec![
                "exec".into(),
                "-w".into(),
                "/workspaces/proj".into(),
                "abc123".into(),
                "bash".into(),
            ],
            manages_cwd: true,
        };
        let override_shell = crate::config::TerminalShellConfig {
            command: "/usr/local/bin/fish".into(),
            args: vec![],
        };
        let wrapper = docker
            .clone()
            .with_user_shell_override(Some(&override_shell));
        assert_eq!(wrapper.command, docker.command);
        assert_eq!(wrapper.args, docker.args);
        assert!(wrapper.manages_cwd);
    }

    #[test]
    fn user_shell_override_none_leaves_wrapper_unchanged() {
        let original = TerminalWrapper::host_shell();
        let wrapper = original.clone().with_user_shell_override(None);
        assert_eq!(wrapper.command, original.command);
        assert_eq!(wrapper.args, original.args);
        assert_eq!(wrapper.manages_cwd, original.manages_cwd);
    }

    #[test]
    fn from_plugin_payload_docker_exec_carries_label() {
        let payload = AuthorityPayload {
            filesystem: FilesystemSpec::Local,
            spawner: SpawnerSpec::DockerExec {
                container_id: "abc123".into(),
                user: Some("vscode".into()),
                workspace: Some("/workspaces/proj".into()),
                env: Vec::new(),
            },
            terminal_wrapper: TerminalWrapperSpec::Explicit {
                command: "docker".into(),
                args: vec![
                    "exec".into(),
                    "-it".into(),
                    "-u".into(),
                    "vscode".into(),
                    "-w".into(),
                    "/workspaces/proj".into(),
                    "abc123".into(),
                    "bash".into(),
                    "-l".into(),
                ],
                manages_cwd: true,
            },
            display_label: "Container:abc123".into(),
            path_translation: None,
        };
        let auth = Authority::from_plugin_payload(
            payload,
            Arc::new(WorkspaceTrust::permissive()),
            Arc::new(crate::services::env_provider::EnvProvider::inactive()),
        )
        .expect("docker payload is valid");
        assert_eq!(auth.terminal_wrapper.command, "docker");
        assert!(auth.terminal_wrapper.manages_cwd);
        assert_eq!(auth.display_label, "Container:abc123");
    }

    #[test]
    fn path_translation_round_trips_under_workspace() {
        let pt = PathTranslation {
            host_root: PathBuf::from("/tmp/.tmpA1B2"),
            remote_root: PathBuf::from("/workspaces/proj"),
        };
        let host = Path::new("/tmp/.tmpA1B2/src/util.py");
        let remote = pt.host_to_remote(host).expect("host under host_root");
        assert_eq!(remote, PathBuf::from("/workspaces/proj/src/util.py"));
        assert_eq!(
            pt.remote_to_host(&remote)
                .expect("remote under remote_root"),
            host.to_path_buf(),
        );
    }

    #[test]
    fn path_translation_returns_none_outside_root() {
        // Library / system paths sit outside the workspace mapping —
        // callers decide what to do with them. The translator just
        // says "not mine".
        let pt = PathTranslation {
            host_root: PathBuf::from("/host/proj"),
            remote_root: PathBuf::from("/workspaces/proj"),
        };
        assert!(pt
            .host_to_remote(Path::new("/usr/include/stdio.h"))
            .is_none());
        assert!(pt
            .remote_to_host(Path::new("/usr/include/stdio.h"))
            .is_none());
    }

    #[test]
    fn from_plugin_payload_with_path_translation_round_trips() {
        // Plugins (the devcontainer one in particular) supply both
        // workspace roots so LSP URIs translate at the boundary. The
        // wire shape uses strings so it survives JSON; the constructed
        // authority parses them into `PathBuf`.
        let json = serde_json::json!({
            "filesystem": { "kind": "local" },
            "spawner": {
                "kind": "docker-exec",
                "container_id": "abc123",
                "workspace": "/workspaces/proj"
            },
            "terminal_wrapper": { "kind": "host-shell" },
            "path_translation": {
                "host_root": "/tmp/.tmpA1B2",
                "remote_root": "/workspaces/proj"
            }
        });
        let payload: AuthorityPayload =
            serde_json::from_value(json).expect("path_translation is accepted");
        let auth = Authority::from_plugin_payload(
            payload,
            Arc::new(WorkspaceTrust::permissive()),
            Arc::new(crate::services::env_provider::EnvProvider::inactive()),
        )
        .expect("payload with translation is valid");
        let pt = auth
            .path_translation
            .expect("authority carries the translation");
        assert_eq!(pt.host_root, PathBuf::from("/tmp/.tmpA1B2"));
        assert_eq!(pt.remote_root, PathBuf::from("/workspaces/proj"));
    }
}
