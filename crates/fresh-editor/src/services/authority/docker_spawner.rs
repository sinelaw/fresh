//! Docker-exec process spawner.
//!
//! Used by container authorities. Plugins build this via the
//! `editor.setAuthority({ spawner: { kind: "docker-exec", … } })` op
//! after they have brought a container up. Core never names "docker"
//! anywhere outside this file — the spawner is just one more
//! `dyn ProcessSpawner` / `dyn LongRunningSpawner` implementation as
//! far as the rest of the editor is concerned.

use std::path::Path;
use std::process::Stdio;

use async_trait::async_trait;
use tokio::process::Command;

use crate::services::remote::{
    LongRunningSpawner, ProcessSpawner, SpawnError, SpawnResult, StdioChild,
};

/// Spawn processes inside a long-lived Docker container via `docker exec`.
///
/// `base_env` carries env vars that get injected via `docker exec -e
/// KEY=VAL` on every spawn — the plugin populates this with the
/// container's `userEnvProbe` capture (notably `PATH`, which differs
/// between an interactive login shell and a bare `docker exec`). LSP
/// servers and `command_exists` probes both go through this so
/// `pylsp` installed by a `postCreateCommand` into `~/.local/bin`
/// is actually discoverable when the editor goes to spawn it.
pub(crate) struct DockerExecSpawner {
    container_id: String,
    user: Option<String>,
    workspace: Option<String>,
    base_env: Vec<(String, String)>,
}

impl DockerExecSpawner {
    pub(crate) fn with_env(
        container_id: String,
        user: Option<String>,
        workspace: Option<String>,
        base_env: Vec<(String, String)>,
    ) -> Self {
        Self {
            container_id,
            user,
            workspace,
            base_env,
        }
    }

    /// Test helper — `with_env` with an empty base env. Production
    /// always knows whether it has a `userEnvProbe` capture or not, so
    /// the explicit form is what `from_plugin_payload` calls.
    #[cfg(test)]
    pub(crate) fn new(
        container_id: String,
        user: Option<String>,
        workspace: Option<String>,
    ) -> Self {
        Self::with_env(container_id, user, workspace, Vec::new())
    }
}

impl DockerExecSpawner {
    /// Compose the `docker` CLI arguments for invoking `command` with
    /// `args` inside the container. Shared between the one-shot
    /// `ProcessSpawner` impl and the long-running variant so both
    /// paths honour `-u <user>` / `-w <cwd-or-workspace>` consistently.
    ///
    /// `extra_env` is merged after the spawner's own `base_env` (so
    /// per-call entries override the captured probe). Both flatten to
    /// `-e KEY=VALUE` flags placed before the container id, matching
    /// `docker`'s flag-parsing rules.
    fn build_exec_args(
        &self,
        command: &str,
        args: &[String],
        cwd: Option<&Path>,
        interactive: bool,
        extra_env: &[(String, String)],
    ) -> Vec<String> {
        let env_capacity = (self.base_env.len() + extra_env.len()) * 2;
        let mut docker_args: Vec<String> = Vec::with_capacity(args.len() + env_capacity + 8);
        docker_args.push("exec".into());
        if interactive {
            // `-i` keeps stdin open so JSON-RPC clients can write to
            // the in-container process; LSP needs this.
            docker_args.push("-i".into());
        }
        if let Some(user) = self.user.as_ref() {
            docker_args.push("-u".into());
            docker_args.push(user.clone());
        }
        let chosen_cwd: Option<String> = cwd
            .map(|p| p.to_string_lossy().into_owned())
            .or_else(|| self.workspace.clone());
        if let Some(dir) = chosen_cwd {
            docker_args.push("-w".into());
            docker_args.push(dir);
        }
        for (k, v) in &self.base_env {
            docker_args.push("-e".into());
            docker_args.push(format!("{}={}", k, v));
        }
        for (k, v) in extra_env {
            docker_args.push("-e".into());
            docker_args.push(format!("{}={}", k, v));
        }
        docker_args.push(self.container_id.clone());
        docker_args.push(command.to_string());
        docker_args.extend(args.iter().cloned());
        docker_args
    }
}

#[async_trait]
impl ProcessSpawner for DockerExecSpawner {
    async fn spawn(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
    ) -> Result<SpawnResult, SpawnError> {
        let cwd_path = cwd.as_deref().map(Path::new);
        let docker_args = self.build_exec_args(&command, &args, cwd_path, false, &[]);

        let output = Command::new("docker")
            .args(&docker_args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .map_err(|e| SpawnError::Process(e.to_string()))?;

        Ok(SpawnResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            exit_code: output.status.code().unwrap_or(-1),
        })
    }
}

/// Long-running spawner for container authorities — wraps the `docker
/// exec -i` invocation into a `StdioChild` so LSP servers (and future
/// tool agents) run inside the container with piped JSON-RPC.
///
/// `spawned_locally = false` on the returned child so host-only resource
/// controls (cgroups, rlimits) skip themselves — their PID would be the
/// `docker` CLI wrapper, not the server running inside the container,
/// and applying a cgroup to the wrapper accomplishes nothing useful.
///
/// `command_exists` runs `docker exec <id> sh -c 'command -v <cmd>'` so
/// the LSP popup's binary-presence probe reflects the container's PATH
/// rather than the host's — which is the whole point of routing through
/// the authority.
pub(crate) struct DockerLongRunningSpawner {
    inner: DockerExecSpawner,
}

impl DockerLongRunningSpawner {
    pub(crate) fn with_env(
        container_id: String,
        user: Option<String>,
        workspace: Option<String>,
        base_env: Vec<(String, String)>,
    ) -> Self {
        Self {
            inner: DockerExecSpawner::with_env(container_id, user, workspace, base_env),
        }
    }

    /// Test helper — see [`DockerExecSpawner::new`].
    #[cfg(test)]
    pub(crate) fn new(
        container_id: String,
        user: Option<String>,
        workspace: Option<String>,
    ) -> Self {
        Self::with_env(container_id, user, workspace, Vec::new())
    }
}

#[async_trait]
impl LongRunningSpawner for DockerLongRunningSpawner {
    async fn spawn_stdio(
        &self,
        command: &str,
        args: &[String],
        env: Vec<(String, String)>,
        cwd: Option<&Path>,
        limits: Option<&crate::types::ProcessLimits>,
    ) -> Result<StdioChild, SpawnError> {
        // Docker authorities can't meaningfully enforce host-side
        // resource limits: a cgroup attached to the `docker` CLI PID
        // doesn't govern the container-side server, and `setrlimit`
        // applied via `pre_exec` in the host-side `docker` process
        // propagates nowhere. Log when limits are set so users don't
        // silently wonder why their cap isn't enforced.
        if let Some(lim) = limits {
            if lim.enabled && (lim.max_memory_percent.is_some() || lim.max_cpu_percent.is_some()) {
                tracing::debug!(
                    "DockerLongRunningSpawner: ignoring process_limits — host-side \
                     cgroups/rlimits don't reach into containers (memory={:?}%, cpu={:?}%)",
                    lim.max_memory_percent,
                    lim.max_cpu_percent
                );
            }
        }

        // `-e KEY=VAL` entries (the spawner's captured `userEnvProbe`
        // base env plus this call's per-spawn `env`) are injected
        // *before* the container id so `docker exec` applies them to
        // the server process. `build_exec_args` handles both layers in
        // one pass.
        let docker_args = self.inner.build_exec_args(command, args, cwd, true, &env);

        let child = Command::new("docker")
            .args(&docker_args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .map_err(|e| SpawnError::Process(e.to_string()))?;

        Ok(StdioChild::from_tokio_child(child, false))
    }

    async fn command_exists(&self, command: &str) -> bool {
        // `command -v` is POSIX-standard and honours shell builtins,
        // functions, and `$PATH` lookups — the same semantics
        // `which::which` gives on the host, minus `which`'s non-
        // ubiquity inside minimal container images.
        //
        // The probe has to see the same `$PATH` the LSP child will see
        // when it actually spawns; otherwise the editor declines to
        // launch a server that's perfectly installed (e.g. `pylsp` in
        // `~/.local/bin`, on PATH for an interactive login shell but
        // not for the bare exec env). `build_exec_args` includes
        // `base_env` here for free.
        let probe = format!("command -v {}", shell_quote(command));
        let sh_args = vec!["-c".to_string(), probe];
        let docker_args = self.inner.build_exec_args("sh", &sh_args, None, false, &[]);

        match Command::new("docker")
            .args(&docker_args)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .await
        {
            Ok(status) => status.success(),
            Err(_) => false,
        }
    }
}

/// Quote a single argument for POSIX `sh -c`. `command -v` only takes a
/// bare command name in practice (no spaces, rare weird chars), but
/// quoting defensively keeps future callers safe.
fn shell_quote(s: &str) -> String {
    if s.chars()
        .all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '.' | '/' | '+' | ':' | '@'))
    {
        s.to_string()
    } else {
        // Wrap in single quotes; escape any embedded single quotes
        // with the usual POSIX `'\''` sequence.
        let escaped = s.replace('\'', "'\\''");
        format!("'{}'", escaped)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_exec_args_non_interactive_places_flags_correctly() {
        let sp = DockerExecSpawner::new(
            "abc123".into(),
            Some("vscode".into()),
            Some("/workspaces/proj".into()),
        );
        let args = sp.build_exec_args("rust-analyzer", &[], None, false, &[]);
        // ["exec", "-u", "vscode", "-w", "/workspaces/proj", "abc123", "rust-analyzer"]
        assert_eq!(args[0], "exec");
        assert_eq!(args[1], "-u");
        assert_eq!(args[2], "vscode");
        assert_eq!(args[3], "-w");
        assert_eq!(args[4], "/workspaces/proj");
        assert_eq!(args[5], "abc123");
        assert_eq!(args[6], "rust-analyzer");
        assert_eq!(args.len(), 7);
    }

    #[test]
    fn build_exec_args_interactive_inserts_dash_i() {
        let sp = DockerExecSpawner::new("abc".into(), None, None);
        let args = sp.build_exec_args("bash", &[], None, true, &[]);
        assert_eq!(&args[..3], &["exec", "-i", "abc"]);
    }

    #[test]
    fn build_exec_args_cwd_override_wins_over_workspace() {
        let sp = DockerExecSpawner::new("abc".into(), None, Some("/default".into()));
        let args = sp.build_exec_args("ls", &[], Some(Path::new("/override")), false, &[]);
        // The cwd slot must carry the per-call override, not the default
        let w_pos = args.iter().position(|a| a == "-w").expect("-w present");
        assert_eq!(args[w_pos + 1], "/override");
    }

    #[test]
    fn build_exec_args_base_env_lands_before_container_id() {
        // `userEnvProbe`-captured PATH (and any other env the plugin
        // hands the spawner constructor) must reach the in-container
        // command. `docker exec` parses `-e` only before the container
        // id; anything after is treated as the command/args.
        let sp = DockerExecSpawner::with_env(
            "abc".into(),
            None,
            None,
            vec![
                ("PATH".into(), "/home/vscode/.local/bin:/usr/bin".into()),
                ("LANG".into(), "C.UTF-8".into()),
            ],
        );
        let args = sp.build_exec_args("pylsp", &[], None, false, &[]);
        let abc_pos = args
            .iter()
            .position(|a| a == "abc")
            .expect("container id present");
        let path_pos = args
            .iter()
            .position(|a| a == "PATH=/home/vscode/.local/bin:/usr/bin")
            .expect("PATH env injected");
        let lang_pos = args
            .iter()
            .position(|a| a == "LANG=C.UTF-8")
            .expect("LANG env injected");
        let pylsp_pos = args
            .iter()
            .position(|a| a == "pylsp")
            .expect("command present");
        assert!(path_pos < abc_pos, "PATH must precede container id");
        assert!(lang_pos < abc_pos, "LANG must precede container id");
        assert!(abc_pos < pylsp_pos, "container id must precede command");
        // Each env value must be preceded by a `-e` flag.
        assert_eq!(args[path_pos - 1], "-e");
        assert_eq!(args[lang_pos - 1], "-e");
    }

    #[test]
    fn build_exec_args_extra_env_appended_after_base_env() {
        // Per-call `env` should compose with the base env so callers
        // can override individual keys; the call-site env is appended
        // last, which means later `-e KEY=VAL` wins under `docker exec`'s
        // last-flag-wins semantics.
        let sp = DockerExecSpawner::with_env(
            "abc".into(),
            None,
            None,
            vec![("PATH".into(), "/base".into())],
        );
        let extra = vec![("PATH".into(), "/override".into())];
        let args = sp.build_exec_args("ls", &[], None, false, &extra);
        let base_idx = args.iter().position(|a| a == "PATH=/base").unwrap();
        let override_idx = args.iter().position(|a| a == "PATH=/override").unwrap();
        assert!(
            base_idx < override_idx,
            "base env precedes per-call env so the call-site value wins"
        );
    }

    #[test]
    fn docker_long_running_spawn_injects_env_before_container_id() {
        // The command composition itself is deterministic — it's what
        // the process will see. Verify that the env flags land between
        // the flag block and the container id, not after the command.
        let sp =
            DockerLongRunningSpawner::new("abc".into(), Some("vscode".into()), Some("/ws".into()));
        let env: Vec<(String, String)> = vec![("RUST_LOG".into(), "debug".into())];
        let out = sp
            .inner
            .build_exec_args("rust-analyzer", &[], None, true, &env);
        let e_pos = out.iter().position(|a| a == "-e").unwrap();
        let abc_pos = out.iter().position(|a| a == "abc").unwrap();
        let ra_pos = out.iter().position(|a| a == "rust-analyzer").unwrap();
        assert!(e_pos < abc_pos);
        assert!(abc_pos < ra_pos);
        assert_eq!(out[e_pos + 1], "RUST_LOG=debug");
    }

    #[test]
    fn shell_quote_passes_simple_names_through() {
        assert_eq!(shell_quote("rust-analyzer"), "rust-analyzer");
        assert_eq!(shell_quote("/usr/bin/env"), "/usr/bin/env");
    }

    #[test]
    fn shell_quote_escapes_weird_characters() {
        assert_eq!(shell_quote("has space"), "'has space'");
        assert_eq!(shell_quote("it's"), "'it'\\''s'");
    }
}
