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
//! - `Authority::local()` — host filesystem + host spawner + host shell
//!   wrapped without args. Always available; the editor boots with this.
//! - `Authority::ssh(filesystem, spawner, display_label)` — used by the
//!   `fresh user@host:path` startup flow.
//! - `Authority::from_plugin_payload(payload)` — built from the
//!   `editor.setAuthority(...)` plugin op. The payload is a tagged shape
//!   (filesystem kind + spawner kind + terminal wrapper + label); it stays
//!   small and additive so we can grow new kinds without breaking the
//!   plugin contract.

use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::model::filesystem::{FileSystem, StdFileSystem};
use crate::services::remote::{
    LocalLongRunningSpawner, LocalProcessSpawner, LongRunningSpawner, ProcessSpawner,
};

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
}

impl Authority {
    /// Default boot-time authority: host filesystem, host process
    /// spawner, host shell wrapper. The editor starts here on every
    /// startup; SSH or plugin-installed authorities replace it later.
    pub fn local() -> Self {
        Self {
            filesystem: Arc::new(StdFileSystem),
            process_spawner: Arc::new(LocalProcessSpawner),
            long_running_spawner: Arc::new(LocalLongRunningSpawner),
            terminal_wrapper: TerminalWrapper::host_shell(),
            display_label: String::new(),
        }
    }

    /// Build an SSH authority. The caller already holds the connection
    /// (and its keepalive resources) so we just wire the parts in. Label
    /// is left empty — the status bar falls back to the filesystem's own
    /// `remote_connection_info()` which knows how to annotate disconnect.
    ///
    /// `long_running_spawner` defaults to the local implementation for
    /// now; Phase L of the dev-container gap plan adds an SSH-routed
    /// variant so LSP runs on the remote host. Until then, LSP over SSH
    /// still spawns on the host — a pre-existing limitation the plan
    /// documents but defers.
    pub fn ssh(
        filesystem: Arc<dyn FileSystem + Send + Sync>,
        process_spawner: Arc<dyn ProcessSpawner>,
    ) -> Self {
        Self {
            filesystem,
            process_spawner,
            long_running_spawner: Arc::new(LocalLongRunningSpawner),
            terminal_wrapper: TerminalWrapper::host_shell(),
            display_label: String::new(),
        }
    }

    /// Build an authority from a plugin payload (the data carried by the
    /// `editor.setAuthority(...)` op). All translation from "kind +
    /// params" to concrete `Arc<dyn …>` lives here and nowhere else.
    pub fn from_plugin_payload(payload: AuthorityPayload) -> Result<Self, AuthorityPayloadError> {
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
                Arc::new(LocalProcessSpawner),
                Arc::new(LocalLongRunningSpawner),
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
                    ),
                ),
                Arc::new(
                    crate::services::authority::docker_spawner::DockerLongRunningSpawner::with_env(
                        container_id,
                        user,
                        workspace,
                        env,
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

        Ok(Self {
            filesystem,
            process_spawner,
            long_running_spawner,
            terminal_wrapper,
            display_label: payload.display_label,
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
        let auth = Authority::local();
        assert!(!auth.terminal_wrapper.command.is_empty());
        assert!(auth.terminal_wrapper.args.is_empty());
        assert!(!auth.terminal_wrapper.manages_cwd);
        assert_eq!(auth.display_label, "");
    }

    #[test]
    fn from_plugin_payload_local_yields_host_shell() {
        let payload = AuthorityPayload {
            filesystem: FilesystemSpec::Local,
            spawner: SpawnerSpec::Local,
            terminal_wrapper: TerminalWrapperSpec::HostShell,
            display_label: String::new(),
        };
        let auth = Authority::from_plugin_payload(payload).expect("local payload is valid");
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
        let auth = Authority::from_plugin_payload(payload).expect("docker payload is valid");
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
        let auth = Authority::from_plugin_payload(payload).expect("payload is valid");
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
        };
        let auth = Authority::from_plugin_payload(payload).expect("docker payload is valid");
        assert_eq!(auth.terminal_wrapper.command, "docker");
        assert!(auth.terminal_wrapper.manages_cwd);
        assert_eq!(auth.display_label, "Container:abc123");
    }
}
