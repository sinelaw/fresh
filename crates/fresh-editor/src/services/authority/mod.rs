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
    build_kube_agent_terminal_args, build_kube_terminal_args, build_ssh_agent_terminal_args,
    build_ssh_terminal_args, spawn_kube_reconnect_task, spawn_reconnect_task, ConnectionParams,
    KubeConnection, KubeTarget, LocalLongRunningSpawner, LocalProcessSpawner, LongRunningSpawner,
    ProcessSpawner, RemoteFileSystem, RemoteLongRunningSpawner, RemoteProcessSpawner,
    SshConnection, SshError, TransportError,
};
use crate::services::workspace_trust::WorkspaceTrust;

/// Plugin-supplied form of the host↔remote workspace mapping. Plugins
/// build this from their own knowledge (e.g. the devcontainer plugin
/// already has `editor.getCwd()` for the host root and
/// `result.remoteWorkspaceFolder` for the in-container root). Strings
/// because the wire format is JSON; paths get parsed in
/// `Authority::from_plugin_payload`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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

    /// Re-parent the integrated terminal into a K8s pod via
    /// `kubectl exec -it … -- sh -lc 'cd <ws>; exec $SHELL -l'`. Same
    /// re-parenting contract as [`Self::ssh`] / the `docker exec -w …`
    /// wrapper: cwd is pinned through the wrapper's own args, so
    /// `manages_cwd` is true and the terminal manager must not hand the
    /// local PTY a pod-side cwd it can't honour.
    pub fn kube(target: &KubeTarget, base_env: &[(String, String)]) -> Self {
        Self {
            command: "kubectl".to_string(),
            args: build_kube_terminal_args(target, base_env),
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum FilesystemSpec {
    /// Use the host filesystem. Devcontainers fall here because the
    /// workspace is mounted into the container, so file paths translate
    /// 1:1 between host and container.
    Local,
}

/// Process-spawner kind chosen by a plugin payload.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
/// fields on `Editor`. **Not `Clone`**: an `Authority` is owned by exactly
/// one `Window`, so a session's backend/trust/env cannot be shared into
/// another window — that isolation is enforced by the type system, not a
/// runtime check (issue #2280). It is *moved* between slots
/// (`set_session_authority`, `set_boot_authority`, restore), never copied.
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
    /// How an arbitrary *interactive* argv (an agent like `claude --resume
    /// <id>`, or a plain command) is turned into a PTY command that runs
    /// **inside** this backend, rooted at the session's workspace dir. Used by
    /// [`Self::terminal_command`]. This is the seam where the per-session
    /// backend and the agent-resume terminal command compose — see
    /// `docs/internal/PER_SESSION_BACKENDS_DESIGN.md`.
    pub command_wrap: CommandWrap,
}

/// How [`Authority::terminal_command`] composes an interactive argv with the
/// authority's backend. Each backend pins the argv's working directory in the
/// way that backend supports: a `-w` flag for `docker exec`, or a `cd <dir>`
/// shell hop for `ssh` / `kubectl` (which have no cwd flag).
#[derive(Debug, Clone)]
pub enum CommandWrap {
    /// Local: the argv is the PTY child directly; the terminal sets cwd.
    Direct,
    /// Argv-pure exec prefix (`docker exec -it [-u][-w][-e] <id>`): the agent
    /// argv is appended verbatim — never a shell string — and cwd is pinned by
    /// the prefix's own `-w` flag. The prefix is non-empty (`prefix[0]` is the
    /// program, e.g. `docker`).
    Prefix(Vec<String>),
    /// SSH: re-exec the argv on the remote host rooted at `remote_dir`. `ssh`
    /// has no cwd flag, so cwd is pinned via a `cd <dir>` shell hop and the
    /// argv is shell-quoted (not appended raw). Without this an agent argv ran
    /// on the **local** host instead of the remote.
    Ssh {
        params: ConnectionParams,
        remote_dir: Option<String>,
    },
    /// Kubernetes: re-exec the argv inside the pod rooted at the target's
    /// workspace, same shell-hop contract as SSH. `base_env` is the captured
    /// in-pod env probe, exported before the agent so its `PATH` resolves.
    Kube {
        target: KubeTarget,
        base_env: Vec<(String, String)>,
    },
}

/// A session's **execution scope**: the trust gate (*may it run?*) and env
/// overlay (*with what env?*) for one session, paired so they are always
/// minted and handed around together.
///
/// This is the one blessed way to obtain per-session `trust` + `env`:
/// [`SessionScope::for_root`] mints **fresh** handles owned by exactly one
/// session, so trusting/activating in one window can never leak into another
/// (issue #2280). It is move-only (not `Clone`) and consumed by
/// `Authority::local_scoped`, and the [`Authority`] it builds is itself
/// non-`Clone` and owned by a single `Window` — so the isolation is enforced
/// by the type system at construction, not a runtime check.
pub struct SessionScope {
    pub trust: Arc<WorkspaceTrust>,
    pub env: Arc<crate::services::env_provider::EnvProvider>,
}

impl SessionScope {
    /// Mint a fresh scope for a session rooted at `root`: a per-root trust
    /// (backed by that project's store, adopting its recorded level) and a
    /// per-session env backed by that project's recipe store. When the session
    /// is trusted, a previously-activated recipe is restored so the session
    /// boots already in its env (no auto-activation restart); otherwise it
    /// starts inactive. Each call yields handles owned by exactly one session.
    pub fn for_root(root: &Path, project_state_dir: &Path) -> Self {
        let trust = WorkspaceTrust::for_session(root, project_state_dir);
        let trusted = trust.level() == crate::services::workspace_trust::TrustLevel::Trusted;
        Self {
            trust,
            env: Arc::new(crate::services::env_provider::EnvProvider::for_session(
                project_state_dir,
                trusted,
            )),
        }
    }
}

impl Authority {
    /// Build a local authority from a per-session [`SessionScope`] — the
    /// canonical per-session local constructor. Equivalent to
    /// [`Self::local`] but takes the scope as a unit so callers can't pass a
    /// trust from one window and an env from another.
    pub fn local_scoped(scope: SessionScope) -> Self {
        Self::local(scope.trust, scope.env)
    }

    /// The persistable backend descriptor for this *live* authority, derived
    /// from its [`CommandWrap`]. This is the single source of truth a window's
    /// `authority_spec` should reflect: it makes a plain `fresh ssh://…` launch
    /// carry a real `RemoteAgent` spec (so persistence, the dormancy model, and
    /// the manual-reconnect rebuild in `start_remote_reconnect` all work)
    /// instead of the historical `Local` default that left those paths inert.
    ///
    /// `Direct` (local) and `Prefix` (container — its spec is owned by the
    /// plugin that built it) map to `Local`; the SSH and Kube wraps reconstruct
    /// their transport spec from the connection params they already hold.
    pub fn session_spec(&self) -> SessionAuthoritySpec {
        match &self.command_wrap {
            CommandWrap::Ssh { params, remote_dir } => {
                SessionAuthoritySpec::RemoteAgent(RemoteAgentSpec {
                    transport: RemoteTransportSpec::Ssh {
                        user: params.user.clone(),
                        host: params.host.clone(),
                        port: params.port,
                        identity_file: params
                            .identity_file
                            .as_ref()
                            .map(|p| p.to_string_lossy().into_owned()),
                        remote_path: remote_dir.clone(),
                        extra_args: params.extra_args.clone(),
                    },
                    base_env: Vec::new(),
                    window: false,
                    label: None,
                    command: None,
                })
            }
            CommandWrap::Kube { target, base_env } => {
                SessionAuthoritySpec::RemoteAgent(RemoteAgentSpec {
                    transport: RemoteTransportSpec::KubectlExec {
                        context: target.context.clone(),
                        namespace: target.namespace.clone(),
                        pod: target.pod.clone(),
                        container: target.container.clone(),
                        workspace: target.workspace.clone(),
                    },
                    base_env: base_env.clone(),
                    window: false,
                    label: None,
                    command: None,
                })
            }
            CommandWrap::Direct | CommandWrap::Prefix(_) => SessionAuthoritySpec::Local,
        }
    }

    /// Build a [`TerminalWrapper`] that runs `argv` as an interactive PTY
    /// child **inside this authority's backend**, rooted at the session's
    /// workspace dir. Local runs it directly; container/remote authorities wrap
    /// it per [`CommandWrap`] so it runs in the backend (a container exec, an
    /// `ssh`/`kubectl` shell hop) rather than on the host. An empty `argv`
    /// falls back to the authority's interactive shell wrapper.
    ///
    /// This is what the Orchestrator agent-resume path spawns through, so a
    /// `claude --resume <id>` restored in a devcontainer session runs as
    /// `docker exec -it … <id> claude --resume <id>`, and an SSH session runs
    /// it as `ssh -t … 'cd <dir>; exec <argv>'` on the remote host — never on
    /// the local machine.
    pub fn terminal_command(&self, argv: &[String]) -> TerminalWrapper {
        let Some((cmd, rest)) = argv.split_first() else {
            return self.terminal_wrapper.clone();
        };
        match &self.command_wrap {
            CommandWrap::Direct => {
                // Local: the command is the PTY child; cwd is the terminal's.
                TerminalWrapper {
                    command: cmd.clone(),
                    args: rest.to_vec(),
                    manages_cwd: false,
                }
            }
            CommandWrap::Prefix(prefix) => {
                // Container: `<exec-prefix> <argv>`, argv-pure. The prefix pins
                // the backend (and its cwd via `-w`), so cwd is managed by the
                // wrapper's own args.
                let mut args = prefix[1..].to_vec();
                args.extend(argv.iter().cloned());
                TerminalWrapper {
                    command: prefix[0].clone(),
                    args,
                    manages_cwd: true,
                }
            }
            CommandWrap::Ssh { params, remote_dir } => {
                // SSH: run the argv on the remote host rooted at `remote_dir`
                // via a `cd <dir>; exec <argv>` shell hop. cwd is pinned by the
                // wrapper's own args.
                TerminalWrapper {
                    command: "ssh".to_string(),
                    args: build_ssh_agent_terminal_args(params, remote_dir.as_deref(), argv),
                    manages_cwd: true,
                }
            }
            CommandWrap::Kube { target, base_env } => {
                // Kubernetes: run the argv inside the pod rooted at the target's
                // workspace via the same shell hop. cwd is pinned by the
                // wrapper's own args.
                TerminalWrapper {
                    command: "kubectl".to_string(),
                    args: build_kube_agent_terminal_args(target, base_env, argv),
                    manages_cwd: true,
                }
            }
        }
    }

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
            // Local: commands run directly as the PTY child, no backend wrap.
            command_wrap: CommandWrap::Direct,
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
            // An agent argv (`claude --resume <id>`) runs on the *remote* host
            // rooted at `remote_dir` via a `cd <dir>; exec <argv>` shell hop —
            // not on the local machine. See [`CommandWrap::Ssh`].
            command_wrap: CommandWrap::Ssh {
                params: params.clone(),
                remote_dir: remote_dir.map(str::to_string),
            },
        }
    }

    /// Build a K8s authority: the remote-agent stack (filesystem + spawners,
    /// already wired to the pod's `kubectl exec` agent channel) plus a
    /// terminal wrapper that opens a shell *inside the pod*.
    ///
    /// Mirrors [`Self::ssh`] — the caller owns the connection and its
    /// keepalive resources and threads the parts in. Unlike SSH, the label
    /// is set from the target (there is no filesystem-side
    /// `remote_connection_info()` that knows about a K8s pod, so identity
    /// lives in the label per `AUTHORITY_DESIGN.md` principle 9). Path
    /// translation is unset: the editor operates directly in the pod's path
    /// space, exactly as SSH does.
    pub fn kube(
        filesystem: Arc<dyn FileSystem + Send + Sync>,
        process_spawner: Arc<dyn ProcessSpawner>,
        long_running_spawner: Arc<dyn LongRunningSpawner>,
        target: &KubeTarget,
        base_env: &[(String, String)],
        trust: Arc<WorkspaceTrust>,
        env: Arc<crate::services::env_provider::EnvProvider>,
    ) -> Self {
        Self {
            filesystem,
            process_spawner,
            long_running_spawner,
            terminal_wrapper: TerminalWrapper::kube(target, base_env),
            display_label: target.display(),
            path_translation: None,
            workspace_trust: trust,
            env_provider: env,
            // An agent argv runs *inside the pod* rooted at the target's
            // workspace via a `cd <ws>; exec <argv>` shell hop — not on the
            // local machine. See [`CommandWrap::Kube`].
            command_wrap: CommandWrap::Kube {
                target: target.clone(),
                base_env: base_env.to_vec(),
            },
        }
    }

    /// Assemble a full K8s authority from a live [`KubeConnection`].
    ///
    /// The high-level counterpart to [`Self::k8s`]: wires the filesystem and
    /// one-shot spawner onto the connection's agent channel
    /// ([`RemoteFileSystem`] / [`RemoteProcessSpawner`], reused verbatim from
    /// the SSH stack) and the long-running (LSP) spawner onto a per-server
    /// `kubectl exec` ([`KubectlLongRunningSpawner`]). `base_env` is the
    /// captured in-pod env probe applied to LSP spawns and `command_exists`.
    ///
    /// The caller must keep the `KubeConnection` alive (in the session
    /// keepalive bundle) — dropping it kills the carrier and tears down the
    /// channel the returned authority rides on, exactly as SSH holds its
    /// `SshConnection`.
    pub fn kube_from_connection(
        connection: &KubeConnection,
        target: KubeTarget,
        base_env: Vec<(String, String)>,
        trust: Arc<WorkspaceTrust>,
        env: Arc<crate::services::env_provider::EnvProvider>,
    ) -> Self {
        let channel = connection.channel();
        let filesystem: Arc<dyn FileSystem + Send + Sync> = Arc::new(RemoteFileSystem::new(
            channel.clone(),
            connection.connection_string().to_string(),
        ));
        let process_spawner: Arc<dyn ProcessSpawner> = Arc::new(RemoteProcessSpawner::new(
            channel,
            Arc::clone(&env),
            Arc::clone(&trust),
        ));
        let long_running_spawner: Arc<dyn LongRunningSpawner> =
            Arc::new(KubectlLongRunningSpawner::with_env(
                target.clone(),
                base_env.clone(),
                Arc::clone(&trust),
            ));
        Self::kube(
            filesystem,
            process_spawner,
            long_running_spawner,
            &target,
            &base_env,
            trust,
            env,
        )
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
        let (process_spawner, long_running_spawner, command_wrap): (
            Arc<dyn ProcessSpawner>,
            Arc<dyn LongRunningSpawner>,
            CommandWrap,
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
                // Host-local spawner: commands run directly, no backend wrap.
                CommandWrap::Direct,
            ),
            SpawnerSpec::DockerExec {
                container_id,
                user,
                workspace,
                env: docker_env,
            } => {
                // The interactive exec prefix so an agent terminal runs
                // *inside* the container (`docker exec -it … <id> <argv>`),
                // mirroring the spawner's one-shot `docker exec` invocation
                // (and the captured `userEnvProbe` env so the agent's PATH
                // resolves the same binaries).
                let command_prefix = build_docker_exec_prefix(
                    &container_id,
                    user.as_deref(),
                    workspace.as_deref(),
                    &docker_env,
                );
                (
                    Arc::new(
                        crate::services::authority::docker_spawner::DockerExecSpawner::with_env(
                            container_id.clone(),
                            user.clone(),
                            workspace.clone(),
                            docker_env.clone(),
                            Arc::clone(&trust),
                        ),
                    ),
                    Arc::new(
                        crate::services::authority::docker_spawner::DockerLongRunningSpawner::with_env(
                            container_id,
                            user,
                            workspace,
                            docker_env,
                            Arc::clone(&trust),
                        ),
                    ),
                    CommandWrap::Prefix(command_prefix),
                )
            }
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
            command_wrap,
        })
    }
}

/// Build the `docker exec -it …` argv prefix that runs an interactive command
/// inside a container, mirroring [`DockerExecSpawner`]'s one-shot exec args.
/// A following agent argv is appended verbatim (argv-pure — no shell string).
/// `-t` (added here, alongside the spawner's `-i`) allocates the PTY the
/// integrated terminal needs.
fn build_docker_exec_prefix(
    container_id: &str,
    user: Option<&str>,
    workspace: Option<&str>,
    env: &[(String, String)],
) -> Vec<String> {
    let mut prefix: Vec<String> = vec!["docker".into(), "exec".into(), "-it".into()];
    if let Some(user) = user {
        prefix.push("-u".into());
        prefix.push(user.to_string());
    }
    if let Some(ws) = workspace {
        prefix.push("-w".into());
        prefix.push(ws.to_string());
    }
    for (k, v) in env {
        prefix.push("-e".into());
        prefix.push(format!("{k}={v}"));
    }
    prefix.push(container_id.to_string());
    prefix
}

/// Declarative description of *how to rebuild* a session's backend — the
/// persisted, source-of-truth counterpart to the live [`Authority`]. A
/// session stores this (in its per-dir workspace file) so that after an
/// editor restart or a cold relaunch its backend can be reconstructed
/// (`Local`) or reconnected (`Plugin` / `RemoteAgent`) instead of silently
/// degrading to local. See `docs/internal/PER_SESSION_BACKENDS_DESIGN.md`.
///
/// Reuses the existing creation payloads ([`AuthorityPayload`],
/// [`RemoteAgentSpec`]) verbatim so there is no new backend vocabulary and
/// `fresh-core` stays backend-opaque. Externally tagged so it round-trips
/// through JSON robustly and additively.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub enum SessionAuthoritySpec {
    /// Host-local backend. The default for a brand-new session and for any
    /// session with no persisted spec (back-compat).
    #[default]
    Local,
    /// A backend installed via `editor.setAuthority(...)` — devcontainer /
    /// docker. Reconnecting it is the owning plugin's job (only it can run
    /// `devcontainer up`).
    Plugin(AuthorityPayload),
    /// A born-attached remote agent (SSH / Kubernetes). Reconnectable from
    /// core via `connect_ssh_authority` / `connect_kube_authority`.
    RemoteAgent(RemoteAgentSpec),
}

impl SessionAuthoritySpec {
    /// Whether this session's backend is anything other than plain local —
    /// i.e. one that must be reconnected (not just rebuilt) on restore.
    pub fn is_remote(&self) -> bool {
        !matches!(self, Self::Local)
    }

    /// Reduce this backend spec to the [`fresh_core::api::RemoteBackendInfo`]
    /// facet the dock and the dormant-shell page render (`None` for local
    /// sessions, and for plugin-managed backends whose facet the owning
    /// plugin supplies itself). The `kind` strings match the plugin API's
    /// `SessionBackend` values.
    pub fn remote_backend_info(
        &self,
        connected: bool,
    ) -> Option<fresh_core::api::RemoteBackendInfo> {
        let Self::RemoteAgent(agent) = self else {
            return None;
        };
        let (kind, detail) = match &agent.transport {
            RemoteTransportSpec::Ssh { user, host, .. } => (
                "ssh",
                match user.as_deref().filter(|u| !u.is_empty()) {
                    Some(user) => format!("{user}@{host}"),
                    None => host.clone(),
                },
            ),
            RemoteTransportSpec::KubectlExec { namespace, pod, .. } => {
                ("kubernetes", format!("{namespace}/{pod}"))
            }
        };
        Some(fresh_core::api::RemoteBackendInfo {
            kind: kind.to_string(),
            detail,
            connected,
        })
    }
}

/// Plugin payload for `editor.attachRemoteAgent(...)`. Names a transport
/// that needs a live connection plus the captured in-pod env probe.
/// Opaque JSON at the fresh-core boundary; parsed here.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct RemoteAgentSpec {
    pub transport: RemoteTransportSpec,
    /// Captured in-pod env (PATH/HOME/LANG/…) applied to LSP spawns and
    /// `command_exists`. Empty when no probe ran.
    #[serde(default)]
    pub base_env: Vec<(String, String)>,
    /// When true, attach as a **new window** (born-attached, coexisting with
    /// existing windows) instead of the default global restart. The
    /// Orchestrator sets this so a cloud session is a real session row rather
    /// than retargeting the whole editor.
    #[serde(default)]
    pub window: bool,
    /// Window label (used only when `window` is true). Empty falls back to the
    /// transport's display.
    #[serde(default)]
    pub label: Option<String>,
    /// Optional agent argv for the new window's seed terminal (window mode).
    #[serde(default)]
    pub command: Option<Vec<String>>,
}

/// Transport kind for [`RemoteAgentSpec`]. Tagged + additive so new
/// carriers slot in without breaking the plugin contract.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum RemoteTransportSpec {
    /// Exec into a pod on a K8s (or any kube) cluster.
    KubectlExec {
        #[serde(default)]
        context: Option<String>,
        namespace: String,
        pod: String,
        #[serde(default)]
        container: Option<String>,
        #[serde(default)]
        workspace: Option<String>,
    },
    /// SSH into a remote host: the same remote-agent stack as the boot-time
    /// `fresh user@host:path` flow, exposed at runtime so the Orchestrator can
    /// open an SSH session as a born-attached window.
    Ssh {
        /// Login user. Optional — omit for `host` / `ssh://host`, letting ssh
        /// resolve the user from its own config or the current local user.
        #[serde(default)]
        user: Option<String>,
        host: String,
        #[serde(default)]
        port: Option<u16>,
        #[serde(default)]
        identity_file: Option<String>,
        /// Remote directory to root the session at (terminal `cd` target).
        #[serde(default)]
        remote_path: Option<String>,
        /// Extra `ssh` arguments (e.g. `-J jump`, `-o ProxyCommand=…`) applied
        /// to every ssh invocation for this session.
        #[serde(default)]
        extra_args: Vec<String>,
    },
}

impl RemoteAgentSpec {
    /// Resolve a kubectl-exec spec into the pod target and the captured env.
    /// Only valid for the `KubectlExec` transport (the caller dispatches on
    /// `transport` first); panics otherwise.
    pub fn into_kube_target(self) -> (KubeTarget, Vec<(String, String)>) {
        match self.transport {
            RemoteTransportSpec::KubectlExec {
                context,
                namespace,
                pod,
                container,
                workspace,
            } => (
                KubeTarget {
                    context,
                    namespace,
                    pod,
                    container,
                    workspace,
                },
                self.base_env,
            ),
            RemoteTransportSpec::Ssh { .. } => {
                unreachable!("into_kube_target called on a non-kube transport")
            }
        }
    }
}

/// Resources that must outlive a K8s [`Authority`]: the carrier
/// connection (its `kubectl exec` child + heartbeat task) and the
/// reconnect task. The editor parks this in its session-keepalive slot —
/// the same one SSH uses for its `SshConnection` — so the agent channel
/// survives the editor rebuild on attach. Dropping it tears the session
/// down (reconnect aborted, then the connection's carrier killed).
pub struct KubeKeepalive {
    // Drop runs the explicit `Drop` below first (aborting reconnect), then
    // fields drop in declaration order: the connection (kills the carrier),
    // then the runtime (shuts down its now-idle workers).
    reconnect: tokio::task::JoinHandle<()>,
    _connection: KubeConnection,
    // The load-bearing field: the dedicated runtime the agent channel +
    // heartbeat + reconnect tasks run on. Owned here so they survive the editor
    // restart the attach triggers — the editor's per-instance runtime is
    // dropped during that rebuild, and if the channel rode *that* runtime its
    // I/O tasks would die the instant the attach completed ("Channel closed"
    // on every file op). SSH's `RemoteSession._runtime` does exactly this.
    _runtime: tokio::runtime::Runtime,
}

impl Drop for KubeKeepalive {
    fn drop(&mut self) {
        self.reconnect.abort();
    }
}

/// Connect to a K8s pod and assemble its [`Authority`] plus the
/// [`KubeKeepalive`] that must be parked to keep it alive.
///
/// The K8s counterpart to SSH's `connect_remote`: bootstraps the agent
/// ([`KubeConnection::connect`]) and the reconnect/heartbeat tasks **on a
/// dedicated runtime owned by the returned keepalive** — *not* the caller's
/// runtime. Installing the authority restarts the editor, dropping the
/// editor's per-instance runtime; binding the channel there would kill it the
/// moment the attach completes (regression: `agent_channel_survives_dropping_
/// the_attach_runtime`). `base_env` is the captured in-pod env probe applied to
/// LSP spawns and `command_exists`.
///
/// Stays `async` for callers, but the bootstrap needs `block_on` (which can't
/// run inside the caller's async context), so it happens on a short-lived
/// helper thread; the live runtime — with its channel/heartbeat/reconnect
/// tasks — is handed back and parked in the keepalive.
pub async fn connect_kube_authority(
    target: KubeTarget,
    base_env: Vec<(String, String)>,
    trust: Arc<WorkspaceTrust>,
    env: Arc<crate::services::env_provider::EnvProvider>,
    cancel: Option<tokio::sync::oneshot::Receiver<()>>,
) -> Result<(Authority, KubeKeepalive), TransportError> {
    type Built = Result<
        (
            KubeConnection,
            tokio::task::JoinHandle<()>,
            tokio::runtime::Runtime,
        ),
        TransportError,
    >;

    let (tx, rx) = tokio::sync::oneshot::channel::<Built>();
    let bootstrap_target = target.clone();
    std::thread::Builder::new()
        .name("kube-connect".to_string())
        .spawn(move || {
            let built: Built = (|| {
                let runtime = tokio::runtime::Builder::new_multi_thread()
                    .worker_threads(2)
                    .thread_name("kube-agent")
                    .enable_all()
                    .build()
                    .map_err(|e| TransportError::AgentStartFailed(format!("runtime: {e}")))?;
                // `block_on` drives the bootstrap; the channel/heartbeat/reconnect
                // tasks it spawns live on `runtime`'s worker threads, which keep
                // running after `block_on` returns and after this helper thread
                // exits — until the `runtime` (moved into the keepalive) drops.
                let (connection, reconnect) = runtime.block_on(async {
                    // Race the connect against the cancel signal so a slow/hung
                    // kubectl bootstrap can be aborted; dropping the connect
                    // future drops the in-flight child. No signal → await it.
                    let connection = match cancel {
                        Some(cancel) => tokio::select! {
                            biased;
                            _ = cancel => {
                                return Err(TransportError::AgentStartFailed(
                                    "cancelled".to_string(),
                                ));
                            }
                            res = KubeConnection::connect(bootstrap_target.clone()) => res?,
                        },
                        None => KubeConnection::connect(bootstrap_target.clone()).await?,
                    };
                    let reconnect =
                        spawn_kube_reconnect_task(&connection.channel(), bootstrap_target.clone());
                    Ok::<_, TransportError>((connection, reconnect))
                })?;
                Ok((connection, reconnect, runtime))
            })();
            #[allow(clippy::let_underscore_must_use)]
            let _ = tx.send(built);
        })
        .map_err(|e| TransportError::AgentStartFailed(format!("connect thread: {e}")))?;

    let (connection, reconnect, runtime) = rx
        .await
        .map_err(|_| TransportError::AgentStartFailed("connect thread vanished".to_string()))??;

    let authority = Authority::kube_from_connection(&connection, target, base_env, trust, env);
    Ok((
        authority,
        KubeKeepalive {
            reconnect,
            _connection: connection,
            _runtime: runtime,
        },
    ))
}

/// Resources that must outlive an SSH [`Authority`]: the `SshConnection`
/// (its `ssh …` child), the reconnect task, and the dedicated runtime the
/// agent channel rides on. The runtime-owned analogue of `main.rs`'s
/// boot-time `RemoteSession`; parked per-window by the Orchestrator's
/// born-attached SSH sessions (and droppable on window close).
pub struct SshKeepalive {
    reconnect: tokio::task::JoinHandle<()>,
    _connection: SshConnection,
    _runtime: tokio::runtime::Runtime,
}

impl Drop for SshKeepalive {
    fn drop(&mut self) {
        self.reconnect.abort();
    }
}

/// Connect to a remote host over SSH and assemble its [`Authority`] plus the
/// [`SshKeepalive`] that must be parked to keep it alive — the runtime-owned,
/// reusable counterpart to `main.rs`'s boot-time `connect_remote`, mirroring
/// [`connect_kube_authority`]. The agent channel + reconnect run on a dedicated
/// runtime returned in the keepalive so they survive editor rebuilds.
///
/// `remote_dir` is the directory the integrated terminal roots at (the
/// `ssh -t … 'cd <dir>; …'` wrapper); filesystem/process ops carry absolute
/// paths and don't need it.
pub async fn connect_ssh_authority(
    params: ConnectionParams,
    remote_dir: Option<String>,
    trust: Arc<WorkspaceTrust>,
    env: Arc<crate::services::env_provider::EnvProvider>,
    cancel: Option<tokio::sync::oneshot::Receiver<()>>,
) -> Result<(Authority, SshKeepalive), SshError> {
    type Built = Result<
        (
            SshConnection,
            tokio::task::JoinHandle<()>,
            tokio::runtime::Runtime,
        ),
        SshError,
    >;

    let (tx, rx) = tokio::sync::oneshot::channel::<Built>();
    let bootstrap_params = params.clone();
    std::thread::Builder::new()
        .name("ssh-connect".to_string())
        .spawn(move || {
            let built: Built = (|| {
                let runtime = tokio::runtime::Builder::new_multi_thread()
                    .worker_threads(2)
                    .thread_name("ssh-agent")
                    .enable_all()
                    .build()
                    .map_err(|e| SshError::AgentStartFailed(format!("runtime: {e}")))?;
                // The channel/reconnect tasks spawned here live on `runtime`'s
                // workers, surviving after this helper thread exits — until the
                // `runtime` (moved into the keepalive) drops.
                let (connection, reconnect) = runtime.block_on(async {
                    // Race the connect against the cancel signal. On cancel the
                    // connect future is dropped, which drops the in-flight ssh
                    // child (spawned kill-on-drop) so a hung handshake leaves no
                    // orphaned process. No cancel signal → just await connect.
                    let connection = match cancel {
                        Some(cancel) => tokio::select! {
                            biased;
                            _ = cancel => {
                                return Err(SshError::AgentStartFailed("cancelled".to_string()));
                            }
                            res = SshConnection::connect(bootstrap_params.clone()) => res?,
                        },
                        None => SshConnection::connect(bootstrap_params.clone()).await?,
                    };
                    let reconnect =
                        spawn_reconnect_task(connection.channel(), connection.params().clone());
                    Ok::<_, SshError>((connection, reconnect))
                })?;
                Ok((connection, reconnect, runtime))
            })();
            #[allow(clippy::let_underscore_must_use)]
            let _ = tx.send(built);
        })
        .map_err(|e| SshError::AgentStartFailed(format!("connect thread: {e}")))?;

    let (connection, reconnect, runtime) = rx
        .await
        .map_err(|_| SshError::AgentStartFailed("connect thread vanished".to_string()))??;

    let channel = connection.channel();
    let connection_string = connection.connection_string().to_string();
    let reconnect_params = connection.params().clone();
    let filesystem: Arc<dyn FileSystem + Send + Sync> =
        Arc::new(RemoteFileSystem::new(channel.clone(), connection_string));
    let process_spawner: Arc<dyn ProcessSpawner> = Arc::new(RemoteProcessSpawner::new(
        channel.clone(),
        Arc::clone(&env),
        Arc::clone(&trust),
    ));
    let long_running_spawner: Arc<dyn LongRunningSpawner> =
        Arc::new(RemoteLongRunningSpawner::new(
            reconnect_params.clone(),
            Arc::clone(&env),
            Arc::clone(&trust),
        ));
    let authority = Authority::ssh(
        filesystem,
        process_spawner,
        long_running_spawner,
        &reconnect_params,
        remote_dir.as_deref(),
        trust,
        env,
    );
    Ok((
        authority,
        SshKeepalive {
            reconnect,
            _connection: connection,
            _runtime: runtime,
        },
    ))
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
mod kube_spawner;

pub(crate) use kube_spawner::KubectlLongRunningSpawner;

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
    fn kube_terminal_wrapper_reparents_into_pod() {
        let target = KubeTarget {
            context: Some("prod".into()),
            namespace: "dev".into(),
            pod: "pod-1".into(),
            container: None,
            workspace: Some("/workspace".into()),
        };
        let wrapper = TerminalWrapper::kube(&target, &[]);
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
    fn remote_agent_spec_parses_plugin_payload() {
        // The exact JSON shape `editor.attachRemoteAgent(...)` carries and
        // `handle_attach_remote_agent` parses (opaque at the fresh-core
        // boundary). Pins the plugin↔core wire contract.
        let json = serde_json::json!({
            "transport": {
                "kind": "kubectl-exec",
                "context": "k3d-dev",
                "namespace": "dev",
                "pod": "fresh-7c9f",
                "container": "app",
                "workspace": "/workspace"
            },
            "base_env": [
                ["PATH", "/home/dev/.local/bin:/usr/bin"],
                ["LANG", "C.UTF-8"]
            ]
        });
        let spec: RemoteAgentSpec = serde_json::from_value(json).expect("spec parses");
        let (target, base_env) = spec.into_kube_target();
        assert_eq!(target.context.as_deref(), Some("k3d-dev"));
        assert_eq!(target.namespace, "dev");
        assert_eq!(target.pod, "fresh-7c9f");
        assert_eq!(target.container.as_deref(), Some("app"));
        assert_eq!(target.workspace.as_deref(), Some("/workspace"));
        assert_eq!(base_env.len(), 2);
        assert_eq!(
            base_env[0],
            (
                "PATH".to_string(),
                "/home/dev/.local/bin:/usr/bin".to_string()
            )
        );

        // Minimal payload (only namespace + pod) parses too: context,
        // container, workspace, and base_env are all optional.
        let minimal = serde_json::json!({
            "transport": { "kind": "kubectl-exec", "namespace": "dev", "pod": "p" }
        });
        let spec2: RemoteAgentSpec = serde_json::from_value(minimal).expect("minimal parses");
        let (t2, env2) = spec2.into_kube_target();
        assert!(t2.context.is_none() && t2.container.is_none() && t2.workspace.is_none());
        assert!(env2.is_empty());
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
    fn terminal_command_runs_local_argv_directly() {
        let auth = Authority::local(
            Arc::new(WorkspaceTrust::permissive()),
            Arc::new(crate::services::env_provider::EnvProvider::inactive()),
        );
        // Local backend: the command is the PTY child, no exec prefix, cwd
        // managed by the terminal (not the wrapper).
        let w = auth.terminal_command(&["claude".into(), "--resume".into(), "u-1".into()]);
        assert_eq!(w.command, "claude");
        assert_eq!(w.args, vec!["--resume".to_string(), "u-1".to_string()]);
        assert!(!w.manages_cwd);
        // Empty argv falls back to the interactive shell wrapper.
        let shell = auth.terminal_command(&[]);
        assert_eq!(shell.command, auth.terminal_wrapper.command);
    }

    #[test]
    fn terminal_command_wraps_argv_into_container_exec() {
        // A container authority must run an agent argv *inside* the container
        // (`docker exec -it … <id> <argv>`) with the argv intact — never on
        // the host, never as a shell string. This is the seam where the
        // per-session backend and the agent-resume command compose.
        let payload = AuthorityPayload {
            filesystem: FilesystemSpec::Local,
            spawner: SpawnerSpec::DockerExec {
                container_id: "abc123".into(),
                user: Some("vscode".into()),
                workspace: Some("/workspaces/proj".into()),
                env: vec![("PATH".into(), "/home/vscode/.local/bin:/usr/bin".into())],
            },
            terminal_wrapper: TerminalWrapperSpec::HostShell,
            display_label: "Container:abc123".into(),
            path_translation: None,
        };
        let auth = Authority::from_plugin_payload(
            payload,
            Arc::new(WorkspaceTrust::permissive()),
            Arc::new(crate::services::env_provider::EnvProvider::inactive()),
        )
        .expect("docker payload is valid");

        let w = auth.terminal_command(&["claude".into(), "--resume".into(), "u-1".into()]);
        assert_eq!(w.command, "docker");
        // Re-parented into the container, so cwd is pinned by the wrapper.
        assert!(w.manages_cwd);
        // The exec prefix (with -u/-w/-e env and the container id) precedes
        // the agent argv, which appears verbatim as the trailing elements.
        assert_eq!(
            w.args,
            vec![
                "exec",
                "-it",
                "-u",
                "vscode",
                "-w",
                "/workspaces/proj",
                "-e",
                "PATH=/home/vscode/.local/bin:/usr/bin",
                "abc123",
                "claude",
                "--resume",
                "u-1",
            ]
        );
    }

    /// Build a minimal SSH authority for the terminal_command tests. The
    /// spawners are local stubs — `terminal_command` only consults
    /// `command_wrap` (built from `params` + `remote_dir`), never the spawners.
    fn ssh_authority(params: &ConnectionParams, remote_dir: Option<&str>) -> Authority {
        let trust = Arc::new(WorkspaceTrust::permissive());
        let env = Arc::new(crate::services::env_provider::EnvProvider::inactive());
        Authority::ssh(
            Arc::new(StdFileSystem),
            Arc::new(LocalProcessSpawner::new(
                Arc::clone(&env),
                Arc::clone(&trust),
            )),
            Arc::new(LocalLongRunningSpawner::new(
                Arc::clone(&env),
                Arc::clone(&trust),
            )),
            params,
            remote_dir,
            trust,
            env,
        )
    }

    #[test]
    fn ssh_terminal_command_runs_agent_on_remote_in_workspace() {
        // Regression: launching an agent (e.g. `claude`) in an SSH session ran
        // it on the *local* host (command == "claude", manages_cwd == false),
        // ignoring the session's remote path. It must run through `ssh` on the
        // remote host, rooted at the provided remote workspace dir.
        let params = ConnectionParams {
            user: Some("u".into()),
            host: "h".into(),
            port: Some(2222),
            identity_file: None,
            extra_args: Vec::new(),
        };
        let auth = ssh_authority(&params, Some("/srv/proj"));

        let w = auth.terminal_command(&["claude".into(), "--resume".into(), "u-1".into()]);
        assert_eq!(
            w.command, "ssh",
            "agent must launch through ssh, not locally"
        );
        assert!(
            w.manages_cwd,
            "ssh re-parents the agent, so it manages its own cwd"
        );
        // Target precedes the single remote-command argument.
        assert!(w.args.contains(&"u@h".to_string()));
        let remote_cmd = w.args.last().expect("ssh passes a remote command");
        assert!(
            remote_cmd.contains("/srv/proj") && remote_cmd.contains("cd "),
            "agent must be rooted at the remote workspace dir: {remote_cmd}"
        );
        assert!(
            remote_cmd.contains("claude"),
            "agent argv must be present in the remote command: {remote_cmd}"
        );
    }

    #[test]
    fn ssh_terminal_command_empty_argv_falls_back_to_shell_wrapper() {
        // Empty argv = "spawn a bare remote shell": still the ssh login-shell
        // wrapper, not an empty/garbage command.
        let params = ConnectionParams {
            user: None,
            host: "h".into(),
            port: None,
            identity_file: None,
            extra_args: Vec::new(),
        };
        let auth = ssh_authority(&params, Some("/srv/proj"));
        let w = auth.terminal_command(&[]);
        assert_eq!(w.command, auth.terminal_wrapper.command);
        assert_eq!(w.args, auth.terminal_wrapper.args);
    }

    #[test]
    fn kube_terminal_command_runs_agent_in_pod_workspace() {
        // Same regression as SSH, but for a Kubernetes-backed session: the
        // agent must run inside the pod (`kubectl exec … -- sh -lc 'cd <ws>;
        // exec <argv>'`), rooted at the pod-side workspace, not on the host.
        let target = KubeTarget {
            context: None,
            namespace: "dev".into(),
            pod: "pod-1".into(),
            container: None,
            workspace: Some("/workspace".into()),
        };
        let trust = Arc::new(WorkspaceTrust::permissive());
        let env = Arc::new(crate::services::env_provider::EnvProvider::inactive());
        let auth = Authority::kube(
            Arc::new(StdFileSystem),
            Arc::new(LocalProcessSpawner::new(
                Arc::clone(&env),
                Arc::clone(&trust),
            )),
            Arc::new(LocalLongRunningSpawner::new(
                Arc::clone(&env),
                Arc::clone(&trust),
            )),
            &target,
            &[],
            trust,
            env,
        );

        let w = auth.terminal_command(&["claude".into(), "--resume".into(), "u-1".into()]);
        assert_eq!(w.command, "kubectl", "agent must launch through kubectl");
        assert!(w.manages_cwd);
        let remote_cmd = w.args.last().expect("kubectl passes a remote command");
        assert!(
            remote_cmd.contains("/workspace") && remote_cmd.contains("cd "),
            "agent must be rooted at the pod workspace dir: {remote_cmd}"
        );
        assert!(
            remote_cmd.contains("claude"),
            "agent argv must be present in the pod command: {remote_cmd}"
        );
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
