//! Long-running (stdio) spawner for K8s authorities.
//!
//! The container analogue of `docker_spawner.rs`: each LSP server (or tool
//! agent) gets its own `kubectl exec -i … -- <server>` subprocess whose piped
//! stdio *is* the in-pod process's stdio, so the LSP I/O layer talks to
//! ordinary `ChildStdin`/`ChildStdout` with no awareness it's remote — the
//! same trick the SSH and Docker spawners use.
//!
//! Why a separate spawner rather than the agent channel: the agent's one-shot
//! `exec` can't keep a process alive with writable stdin, and one-shot
//! commands / the filesystem already route through the channel
//! (`RemoteProcessSpawner` / `RemoteFileSystem`). Only the long-running path
//! needs its own carrier.
//!
//! Two wrinkles distinguish this from Docker, both because `kubectl exec` has
//! no flags for them and execs the post-`--` argv directly (no remote shell):
//!
//! - **cwd** — there is no `-w`. We wrap in `sh -lc 'cd <dir>; exec … "$0"
//!   "$@"' <cmd> <args…>` when a working directory is requested.
//! - **env** — there is no `-e KEY=VAL`. We inject via the same wrapper's
//!   `env KEY=VAL …` prefix. `base_env` carries the captured in-pod
//!   `userEnvProbe` (notably `PATH`) so a server installed on a shell-only
//!   PATH (e.g. `~/.local/bin`) actually resolves; per-call `env` layers on
//!   top (last wins under `env`).

use std::path::Path;
use std::process::Stdio;
use std::sync::Arc;

use async_trait::async_trait;
use tokio::process::Command;

use crate::services::process_hidden::HideWindow;
use crate::services::remote::{
    kubectl_exec_argv, KubeTarget, LongRunningSpawner, SpawnError, StdioChild,
};
use crate::services::workspace_trust::{gate, WorkspaceTrust};

pub(crate) struct KubectlLongRunningSpawner {
    target: KubeTarget,
    /// Captured in-pod env probe (PATH/HOME/LANG/…). Applied to every server
    /// and to `command_exists` so binary discovery matches what the server
    /// will see. Empty when no probe ran.
    base_env: Vec<(String, String)>,
    trust: Arc<WorkspaceTrust>,
}

impl KubectlLongRunningSpawner {
    pub(crate) fn with_env(
        target: KubeTarget,
        base_env: Vec<(String, String)>,
        trust: Arc<WorkspaceTrust>,
    ) -> Self {
        Self {
            target,
            base_env,
            trust,
        }
    }

    /// Test helper — empty base env, permissive trust. See
    /// [`super::docker_spawner`] for the rationale (production always knows
    /// whether it captured a probe).
    #[cfg(test)]
    pub(crate) fn new(target: KubeTarget) -> Self {
        Self::with_env(target, Vec::new(), Arc::new(WorkspaceTrust::permissive()))
    }

    /// Compose the `(command, args)` to hand `kubectl exec` so the in-pod
    /// process runs with `cwd` and the merged env applied. Returns the bare
    /// `(command, args)` when neither is needed (no wrapper shell), otherwise
    /// an `sh -lc '…'` wrapper. `base_env` is laid down first so per-call
    /// `env` overrides it (`env` is last-assignment-wins).
    fn compose(
        &self,
        command: &str,
        args: &[String],
        env: &[(String, String)],
        cwd: Option<&Path>,
    ) -> (String, Vec<String>) {
        let mut merged: Vec<(String, String)> = self.base_env.clone();
        merged.extend(env.iter().cloned());

        if cwd.is_none() && merged.is_empty() {
            return (command.to_string(), args.to_vec());
        }

        let mut script = String::new();
        if let Some(dir) = cwd {
            script.push_str(&format!("cd {}; ", shell_quote(&dir.to_string_lossy())));
        }
        script.push_str("exec ");
        if !merged.is_empty() {
            script.push_str("env ");
            for (k, v) in &merged {
                script.push_str(&format!("{}={} ", k, shell_quote(v)));
            }
        }
        // "$0" is the real command, "$@" its args — passed positionally after
        // the script so no quoting of the user's argv is required.
        script.push_str("\"$0\" \"$@\"");

        // `-c`, not `-lc`: env is injected explicitly via `env K=V` (from the
        // captured probe), so a *login* shell is unwanted — it would source
        // profile scripts that add startup latency and can leak noise onto the
        // server's stdout. Same stance as `docker exec` (no login shell).
        let mut a = Vec::with_capacity(args.len() + 3);
        a.push("-c".to_string());
        a.push(script);
        a.push(command.to_string());
        a.extend(args.iter().cloned());
        ("sh".to_string(), a)
    }
}

#[async_trait]
impl LongRunningSpawner for KubectlLongRunningSpawner {
    async fn spawn_stdio(
        &self,
        command: &str,
        args: &[String],
        env: Vec<(String, String)>,
        cwd: Option<&Path>,
        limits: Option<&crate::types::ProcessLimits>,
    ) -> Result<StdioChild, SpawnError> {
        gate(
            &self.trust,
            command,
            cwd.map(|p| p.to_string_lossy()).as_deref(),
        )?;

        // Like the Docker spawner: a cgroup/rlimit on the host-side `kubectl`
        // PID doesn't reach the in-pod server, so host resource limits can't
        // be honoured here. Pod-spec requests/limits are the right lever, set
        // at schedule time. Log so a set cap isn't silently ignored.
        if let Some(lim) = limits {
            if lim.enabled && (lim.max_memory_percent.is_some() || lim.max_cpu_percent.is_some()) {
                tracing::debug!(
                    "KubectlLongRunningSpawner: ignoring process_limits — host-side \
                     cgroups/rlimits don't reach into pods (memory={:?}%, cpu={:?}%)",
                    lim.max_memory_percent,
                    lim.max_cpu_percent
                );
            }
        }

        let (cmd, cargs) = self.compose(command, args, &env, cwd);
        let argv = kubectl_exec_argv(&self.target, &["-i"], &cmd, &cargs);

        let child = Command::new("kubectl")
            .args(&argv)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .hide_window()
            .kill_on_drop(true)
            .spawn()
            .map_err(|e| SpawnError::Process(e.to_string()))?;

        // `spawned_locally = false`: the host-only resource-control path must
        // skip itself — the child PID is the `kubectl` wrapper, not the server.
        Ok(StdioChild::from_tokio_child(child, false))
    }

    async fn command_exists(&self, command: &str) -> bool {
        // `command -v` (POSIX) honours builtins / functions / `$PATH`. The
        // probe must see the same PATH the server will, so we replay
        // `base_env` via `export` before probing — otherwise the editor
        // declines to launch a server that's installed but only on a
        // shell-/login-PATH (e.g. `pylsp` in `~/.local/bin`).
        let mut script = String::new();
        for (k, v) in &self.base_env {
            script.push_str(&format!("export {}={}; ", k, shell_quote(v)));
        }
        script.push_str(&format!("command -v {}", shell_quote(command)));

        // `-c` (non-login): base_env is replayed via explicit `export`s above,
        // so the probe sees the same PATH the server will without sourcing
        // login profiles. Matches the spawn_stdio wrapper.
        let argv = kubectl_exec_argv(&self.target, &[], "sh", &["-c".to_string(), script]);

        match Command::new("kubectl")
            .args(&argv)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .hide_window()
            .status()
            .await
        {
            Ok(status) => status.success(),
            Err(_) => false,
        }
    }
}

/// Quote a single argument for POSIX `sh`. Bare when it's an obviously-safe
/// token (matching the Docker spawner's allow-list, plus `=` for `K=V`
/// env assignments), otherwise single-quoted with embedded quotes escaped.
fn shell_quote(s: &str) -> String {
    if !s.is_empty()
        && s.chars().all(|c| {
            c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '.' | '/' | '+' | ':' | '@' | '=')
        })
    {
        s.to_string()
    } else {
        let escaped = s.replace('\'', "'\\''");
        format!("'{}'", escaped)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn target() -> KubeTarget {
        KubeTarget {
            context: None,
            namespace: "dev".into(),
            pod: "pod-1".into(),
            container: None,
            workspace: None,
        }
    }

    #[test]
    fn compose_bare_when_no_cwd_or_env() {
        let sp = KubectlLongRunningSpawner::new(target());
        let (cmd, args) = sp.compose("rust-analyzer", &["--stdio".into()], &[], None);
        assert_eq!(cmd, "rust-analyzer");
        assert_eq!(args, vec!["--stdio".to_string()]);
    }

    #[test]
    fn compose_wraps_cwd_and_env_in_sh() {
        let sp = KubectlLongRunningSpawner::with_env(
            target(),
            vec![("PATH".into(), "/home/dev/.local/bin:/usr/bin".into())],
            Arc::new(WorkspaceTrust::permissive()),
        );
        let (cmd, args) = sp.compose(
            "pylsp",
            &["-v".into()],
            &[("RUST_LOG".into(), "debug".into())],
            Some(Path::new("/workspace")),
        );
        assert_eq!(cmd, "sh");
        assert_eq!(args[0], "-c");
        // cd first, then env (base then per-call), then exec the real argv.
        assert_eq!(
            args[1],
            "cd /workspace; exec env PATH=/home/dev/.local/bin:/usr/bin RUST_LOG=debug \"$0\" \"$@\""
        );
        assert_eq!(args[2], "pylsp");
        assert_eq!(args[3], "-v");
    }

    #[test]
    fn compose_env_only_skips_cd() {
        let sp = KubectlLongRunningSpawner::with_env(
            target(),
            vec![("LANG".into(), "C.UTF-8".into())],
            Arc::new(WorkspaceTrust::permissive()),
        );
        let (cmd, args) = sp.compose("gopls", &[], &[], None);
        assert_eq!(cmd, "sh");
        assert_eq!(args[1], "exec env LANG=C.UTF-8 \"$0\" \"$@\"");
        assert_eq!(args[2], "gopls");
    }

    #[test]
    fn compose_quotes_paths_with_spaces() {
        let sp = KubectlLongRunningSpawner::new(target());
        let (_cmd, args) = sp.compose("ls", &[], &[], Some(Path::new("/work space")));
        assert_eq!(args[1], "cd '/work space'; exec \"$0\" \"$@\"");
    }

    #[test]
    fn spawn_argv_is_interactive_kubectl_exec() {
        // The composed argv must be `kubectl exec -i … -- sh -lc … cmd args`.
        let sp = KubectlLongRunningSpawner::with_env(
            target(),
            vec![("PATH".into(), "/usr/bin".into())],
            Arc::new(WorkspaceTrust::permissive()),
        );
        let (cmd, cargs) = sp.compose("gopls", &[], &[], Some(Path::new("/workspace")));
        let argv = kubectl_exec_argv(&target(), &["-i"], &cmd, &cargs);
        assert_eq!(&argv[..5], &["exec", "-i", "-n", "dev", "pod-1"]);
        assert_eq!(argv[5], "--");
        assert_eq!(argv[6], "sh");
        assert!(argv.contains(&"gopls".to_string()));
    }
}
