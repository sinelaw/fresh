//! Process spawner abstraction
//!
//! Provides a trait for spawning processes that works transparently on both
//! local and remote hosts. Used by the Editor's SpawnProcess handler (for
//! plugins like git_grep) and by FileProvider (for `git ls-files`).
//!
//! Two orthogonal traits live here:
//!
//! - [`ProcessSpawner`] — one-shot "run and collect" commands. Callers get
//!   `{stdout, stderr, exit_code}` back once the child exits. Used by
//!   plugin `spawnProcess`, find-in-files, `git ls-files`, etc.
//! - [`LongRunningSpawner`] — long-lived stdio processes (LSP servers,
//!   future tool agents). Callers get a [`StdioChild`] they can talk to
//!   via piped stdin/stdout/stderr and kill explicitly. LSP servers route
//!   through this so an authority pointing at a container runs the server
//!   inside the container (via `docker exec -i`) instead of on the host.

use crate::services::process_hidden::HideWindow;
use crate::services::process_limits::PostSpawnAction;
use crate::services::remote::channel::{AgentChannel, ChannelError};
use crate::services::remote::protocol::{decode_base64, exec_params};
use crate::types::ProcessLimits;
use std::path::Path;
use std::process::ExitStatus;
use std::sync::Arc;
use tokio::process::{ChildStderr, ChildStdin, ChildStdout};

/// Result of spawning a process
#[derive(Debug, Clone)]
pub struct SpawnResult {
    pub stdout: String,
    pub stderr: String,
    pub exit_code: i32,
}

/// Error from spawning a process
#[derive(Debug, thiserror::Error)]
pub enum SpawnError {
    #[error("Channel error: {0}")]
    Channel(#[from] ChannelError),

    #[error("Process error: {0}")]
    Process(String),

    #[error("Decode error: {0}")]
    Decode(String),
}

/// Trait for spawning processes (local or remote)
///
/// This abstraction allows plugins and core features (like file discovery)
/// to spawn processes transparently on either local or remote filesystems.
#[async_trait::async_trait]
pub trait ProcessSpawner: Send + Sync {
    /// Spawn a process and wait for completion
    async fn spawn(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
    ) -> Result<SpawnResult, SpawnError>;
}

/// Local process spawner using tokio
///
/// Used for local file editing (the default).
pub struct LocalProcessSpawner;

#[async_trait::async_trait]
impl ProcessSpawner for LocalProcessSpawner {
    async fn spawn(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
    ) -> Result<SpawnResult, SpawnError> {
        let mut cmd = tokio::process::Command::new(&command);
        cmd.args(&args);
        cmd.hide_window();

        if let Some(ref dir) = cwd {
            cmd.current_dir(dir);
        }

        let output = cmd
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

/// Remote process spawner via SSH agent
pub struct RemoteProcessSpawner {
    channel: Arc<AgentChannel>,
}

impl RemoteProcessSpawner {
    /// Create a new remote process spawner
    pub fn new(channel: Arc<AgentChannel>) -> Self {
        Self { channel }
    }
}

#[async_trait::async_trait]
impl ProcessSpawner for RemoteProcessSpawner {
    async fn spawn(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
    ) -> Result<SpawnResult, SpawnError> {
        let params = exec_params(&command, &args, cwd.as_deref());

        // Use streaming request to get live output
        let (mut data_rx, result_rx) = self.channel.request_streaming("exec", params).await?;

        let mut stdout = Vec::new();
        let mut stderr = Vec::new();

        // Collect streaming output
        while let Some(data) = data_rx.recv().await {
            if let Some(out) = data.get("out").and_then(|v| v.as_str()) {
                if let Ok(decoded) = decode_base64(out) {
                    stdout.extend_from_slice(&decoded);
                }
            }
            if let Some(err) = data.get("err").and_then(|v| v.as_str()) {
                if let Ok(decoded) = decode_base64(err) {
                    stderr.extend_from_slice(&decoded);
                }
            }
        }

        // Get final result
        let result = result_rx
            .await
            .map_err(|_| SpawnError::Channel(ChannelError::ChannelClosed))?
            .map_err(SpawnError::Process)?;

        let exit_code = result
            .get("code")
            .and_then(|v| v.as_i64())
            .map(|c| c as i32)
            .unwrap_or(-1);

        Ok(SpawnResult {
            stdout: String::from_utf8_lossy(&stdout).to_string(),
            stderr: String::from_utf8_lossy(&stderr).to_string(),
            exit_code,
        })
    }
}

/// A long-lived child process with piped stdio streams.
///
/// Wraps [`tokio::process::Child`] so the LSP code (and future callers
/// like plugin-managed tool agents) doesn't reach into concrete process
/// types — that way a container authority can transparently run the
/// child through `docker exec -i` while the caller keeps talking to an
/// ordinary stdin/stdout pair.
///
/// Streams are `Option`-wrapped so callers can [`Self::take_stdin`] /
/// [`Self::take_stdout`] / [`Self::take_stderr`] into their own reader
/// and writer tasks. After all streams are taken, the `StdioChild` is
/// still useful for lifecycle control via [`Self::kill`] and
/// [`Self::wait`].
///
/// `spawned_locally` tells callers whether `id()` names the real child
/// process (true for local spawns) or an intermediate like `docker` /
/// `ssh` (false). LSP's cgroup-attachment step keys off this — applying
/// a cgroup to the `docker` CLI PID doesn't constrain the container-
/// side server it exec'd.
pub struct StdioChild {
    inner: tokio::process::Child,
    stdin: Option<ChildStdin>,
    stdout: Option<ChildStdout>,
    stderr: Option<ChildStderr>,
    spawned_locally: bool,
}

impl StdioChild {
    /// Construct a `StdioChild` from an already-spawned
    /// `tokio::process::Child`. Pulls the piped streams out of the
    /// child so callers can take them individually later.
    ///
    /// This constructor is for spawners that don't participate in
    /// host-side resource limiting (the Docker variant is the
    /// canonical example). Local spawners should prefer
    /// [`Self::from_local_tokio_child`] so a `PostSpawnAction` produced
    /// by [`ProcessLimits::apply_to_command`] is applied to the child's
    /// PID before the spawner returns.
    pub fn from_tokio_child(mut child: tokio::process::Child, spawned_locally: bool) -> Self {
        let stdin = child.stdin.take();
        let stdout = child.stdout.take();
        let stderr = child.stderr.take();
        Self {
            inner: child,
            stdin,
            stdout,
            stderr,
            spawned_locally,
        }
    }

    /// Construct a `StdioChild` for a locally-spawned child while
    /// applying any host-side `PostSpawnAction` (cgroup attachment)
    /// returned by [`ProcessLimits::apply_to_command`]. Best-effort:
    /// failure to attach logs a warning but doesn't fail the spawn,
    /// matching the pre-refactor behavior.
    pub fn from_local_tokio_child(
        child: tokio::process::Child,
        post_spawn: PostSpawnAction,
    ) -> Self {
        let out = Self::from_tokio_child(child, true);
        if let Some(pid) = out.inner.id() {
            post_spawn.apply_to_child(pid);
        }
        out
    }

    /// Take the stdin stream. Returns `None` after the first call.
    pub fn take_stdin(&mut self) -> Option<ChildStdin> {
        self.stdin.take()
    }

    /// Take the stdout stream. Returns `None` after the first call.
    pub fn take_stdout(&mut self) -> Option<ChildStdout> {
        self.stdout.take()
    }

    /// Take the stderr stream. Returns `None` after the first call.
    pub fn take_stderr(&mut self) -> Option<ChildStderr> {
        self.stderr.take()
    }

    /// PID of the immediate child process. For local spawns this is
    /// the LSP server itself; for docker/ssh this is the CLI wrapper.
    /// Use [`Self::spawned_locally`] to tell which.
    pub fn id(&self) -> Option<u32> {
        self.inner.id()
    }

    /// `true` when the child PID names the real target process. Callers
    /// that only apply host-side resource controls (cgroups, rlimits)
    /// should skip their application when this is `false`.
    pub fn spawned_locally(&self) -> bool {
        self.spawned_locally
    }

    /// Request termination. Forwards to [`tokio::process::Child::kill`].
    pub async fn kill(&mut self) -> std::io::Result<()> {
        self.inner.kill().await
    }

    /// Await exit. Forwards to [`tokio::process::Child::wait`].
    pub async fn wait(&mut self) -> std::io::Result<ExitStatus> {
        self.inner.wait().await
    }
}

/// Spawner for long-lived stdio processes (LSP servers, tool agents).
///
/// Separate from [`ProcessSpawner`] because the APIs diverge in two
/// ways that don't compose: [`ProcessSpawner::spawn`] awaits
/// completion and returns collected output; callers of
/// `LongRunningSpawner` need a live child they can read from and
/// write to over time.
///
/// Authorities expose one of these alongside their filesystem and
/// one-shot spawner. Routing LSP spawning through it is what gives
/// container authorities in-container LSP without a special-cased
/// branch in `LspHandle`.
///
/// Callers pass an optional [`ProcessLimits`] block so local spawners
/// can honor host-side memory / CPU limits. Non-local variants (docker,
/// ssh) don't have a meaningful way to impose host limits on their
/// child — cgroups attached to the `docker` CLI PID don't reach into
/// the container — and are expected to ignore them.
#[async_trait::async_trait]
pub trait LongRunningSpawner: Send + Sync {
    /// Spawn `command` with `args` as a long-lived stdio child under
    /// this authority. Stdin/stdout/stderr are piped so the caller can
    /// hand them to dedicated reader/writer tasks. `limits`, when
    /// provided, lets local spawners attach cgroups or `setrlimit`;
    /// remote spawners are expected to ignore it (see trait docs).
    async fn spawn_stdio(
        &self,
        command: &str,
        args: &[String],
        env: Vec<(String, String)>,
        cwd: Option<&Path>,
        limits: Option<&ProcessLimits>,
    ) -> Result<StdioChild, SpawnError>;

    /// Check whether `command` resolves to an executable under this
    /// authority. Routed through the same spawner so an SSH authority
    /// probes the remote `$PATH` and a container authority probes the
    /// container's `$PATH` — unlike `which::which` which only ever sees
    /// the host.
    async fn command_exists(&self, command: &str) -> bool;
}

/// Local long-running spawner using `tokio::process::Command` directly.
///
/// Functionally equivalent to how `LspHandle::spawn` works today, but
/// exposed through the trait so non-local authorities can substitute
/// their own implementation without any LSP-side awareness. Applies
/// any `ProcessLimits` passed in via the same machinery the
/// pre-refactor LSP code used (`apply_to_command` + `apply_to_child`).
pub struct LocalLongRunningSpawner;

#[async_trait::async_trait]
impl LongRunningSpawner for LocalLongRunningSpawner {
    async fn spawn_stdio(
        &self,
        command: &str,
        args: &[String],
        env: Vec<(String, String)>,
        cwd: Option<&Path>,
        limits: Option<&ProcessLimits>,
    ) -> Result<StdioChild, SpawnError> {
        let mut cmd = tokio::process::Command::new(command);
        cmd.args(args)
            .envs(env)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .hide_window()
            .kill_on_drop(true);
        if let Some(dir) = cwd {
            cmd.current_dir(dir);
        }

        // Apply pre-spawn hooks (cgroup path selection, setrlimit
        // via `pre_exec`). Errors bubble up so callers see
        // configuration problems early — matches the pre-refactor
        // LSP behavior.
        let post_spawn = match limits {
            Some(lim) => lim
                .apply_to_command(&mut cmd)
                .map_err(|e| SpawnError::Process(format!("Failed to apply process limits: {e}")))?,
            None => PostSpawnAction::default(),
        };

        let child = cmd
            .spawn()
            .map_err(|e| SpawnError::Process(e.to_string()))?;
        Ok(StdioChild::from_local_tokio_child(child, post_spawn))
    }

    async fn command_exists(&self, command: &str) -> bool {
        which::which(command).is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::AsyncReadExt;

    #[tokio::test]
    async fn test_local_spawner() {
        let spawner = LocalProcessSpawner;
        let result = spawner
            .spawn("echo".to_string(), vec!["hello".to_string()], None)
            .await
            .unwrap();

        assert_eq!(result.exit_code, 0);
        assert!(result.stdout.trim() == "hello");
    }

    #[tokio::test]
    async fn local_long_running_spawn_stdio_pipes_output() {
        let spawner = LocalLongRunningSpawner;
        let mut child = spawner
            .spawn_stdio(
                "sh",
                &["-c".into(), "echo hi".into()],
                Vec::new(),
                None,
                None,
            )
            .await
            .expect("spawn succeeds");

        let mut stdout = child.take_stdout().expect("stdout piped");
        let mut buf = String::new();
        stdout.read_to_string(&mut buf).await.unwrap();
        assert_eq!(buf.trim(), "hi");

        let status = child.wait().await.unwrap();
        assert!(status.success());
        assert!(child.spawned_locally());
    }

    #[tokio::test]
    async fn local_long_running_command_exists_for_sh() {
        let spawner = LocalLongRunningSpawner;
        assert!(spawner.command_exists("sh").await);
        assert!(
            !spawner
                .command_exists("fresh-unlikely-binary-name-ygzu9")
                .await
        );
    }
}
