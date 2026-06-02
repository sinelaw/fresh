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

use crate::services::env_provider::EnvProvider;
use crate::services::process_hidden::HideWindow;
use crate::services::process_limits::PostSpawnAction;
use crate::services::remote::channel::{AgentChannel, ChannelError};
use crate::services::remote::protocol::{decode_base64, exec_params};
use crate::services::workspace_trust::{gate, WorkspaceTrust};
use crate::types::ProcessLimits;

/// Capture the active environment for a *local* host: run the provider's
/// capture script through `$SHELL -lc …` as a raw subprocess (no env applied —
/// this is how the env is established, so it must not recurse). Returns the
/// captured `KEY=VALUE` pairs, or empty when inactive / on failure.
async fn local_captured_env(provider: &EnvProvider) -> Vec<(String, String)> {
    provider
        .current(|script| async move {
            let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string());
            let output = tokio::process::Command::new(&shell)
                .arg("-lc")
                .arg(&script)
                .hide_window()
                .output()
                .await
                .ok()?;
            Some(String::from_utf8_lossy(&output.stdout).into_owned())
        })
        .await
}
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

    /// Spawn a process, piping stdout directly to a file instead of
    /// buffering it in memory. Default impl buffers and writes; concrete
    /// implementations should override when a streaming path exists.
    ///
    /// `SpawnResult.stdout` is empty on success — the bytes are on disk
    /// at `stdout_to` instead. `stderr` and `exit_code` work as usual.
    async fn spawn_to_file(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        stdout_to: std::path::PathBuf,
    ) -> Result<SpawnResult, SpawnError> {
        // Fallback: collect in memory then write. Concrete impls override
        // to pipe directly.
        let result = self.spawn(command, args, cwd).await?;
        if result.exit_code == 0 || !result.stdout.is_empty() {
            std::fs::write(&stdout_to, result.stdout.as_bytes())
                .map_err(|e| SpawnError::Process(format!("write {:?}: {}", stdout_to, e)))?;
        }
        Ok(SpawnResult {
            stdout: String::new(),
            stderr: result.stderr,
            exit_code: result.exit_code,
        })
    }

    /// Spawn a process that can be cancelled mid-flight via a oneshot
    /// receiver. When `stdout_to` is `Some`, stdout streams to the file;
    /// when `None`, it's buffered into `SpawnResult.stdout`.
    ///
    /// If `kill_rx` fires before the child exits, the child is killed and
    /// the result reflects the killed exit status.
    ///
    /// Default impl ignores `kill_rx` (no true cancellation for backends
    /// that buffer in memory). Local override implements real kill.
    async fn spawn_cancellable(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        stdout_to: Option<std::path::PathBuf>,
        _kill_rx: tokio::sync::oneshot::Receiver<()>,
    ) -> Result<SpawnResult, SpawnError> {
        match stdout_to {
            Some(p) => self.spawn_to_file(command, args, cwd, p).await,
            None => self.spawn(command, args, cwd).await,
        }
    }
}

/// Local process spawner using tokio.
///
/// Used for local file editing (the default). The optional `env` is layered
/// onto every child's environment (under the inherited process env) — this is
/// how an activated environment manager (venv / direnv / mise) injects its
/// captured variables into one-shot spawns. Empty for the plain default.
pub struct LocalProcessSpawner {
    env: Arc<EnvProvider>,
    trust: Arc<WorkspaceTrust>,
}

impl LocalProcessSpawner {
    /// Local spawner gated by `trust`, applying the live `env` provider's
    /// captured environment to every child.
    pub fn new(env: Arc<EnvProvider>, trust: Arc<WorkspaceTrust>) -> Self {
        Self { env, trust }
    }

    async fn apply_env(&self, cmd: &mut tokio::process::Command) {
        let env = local_captured_env(&self.env).await;
        if !env.is_empty() {
            cmd.envs(env.iter().map(|(k, v)| (k.as_str(), v.as_str())));
        }
    }
}

#[async_trait::async_trait]
impl ProcessSpawner for LocalProcessSpawner {
    async fn spawn(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
    ) -> Result<SpawnResult, SpawnError> {
        gate(&self.trust, &command, cwd.as_deref())?;
        let mut cmd = tokio::process::Command::new(&command);
        cmd.args(&args);
        self.apply_env(&mut cmd).await;
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

    /// Cancellable streaming spawn. Handles both `stdout_to = Some(path)`
    /// (pipe stdout to file) and `stdout_to = None` (buffer in memory),
    /// with kill support via `kill_rx`.
    async fn spawn_cancellable(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        stdout_to: Option<std::path::PathBuf>,
        kill_rx: tokio::sync::oneshot::Receiver<()>,
    ) -> Result<SpawnResult, SpawnError> {
        use std::process::Stdio;
        use tokio::io::AsyncReadExt;

        gate(&self.trust, &command, cwd.as_deref())?;
        let mut cmd = tokio::process::Command::new(&command);
        cmd.args(&args);
        self.apply_env(&mut cmd).await;
        cmd.hide_window();
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        if let Some(ref dir) = cwd {
            cmd.current_dir(dir);
        }

        // For file-output mode, ensure parent dir exists. Surface the
        // failure as a SpawnError rather than silently dropping — if we
        // can't make the dir, the File::create below would just fail
        // with a less informative error.
        if let Some(ref path) = stdout_to {
            if let Some(parent) = path.parent() {
                if !parent.as_os_str().is_empty() {
                    tokio::fs::create_dir_all(parent).await.map_err(|e| {
                        SpawnError::Process(format!("create_dir_all {:?}: {}", parent, e))
                    })?;
                }
            }
        }

        let mut child = cmd
            .spawn()
            .map_err(|e| SpawnError::Process(e.to_string()))?;

        let mut child_stdout = child
            .stdout
            .take()
            .ok_or_else(|| SpawnError::Process("child stdout missing".to_string()))?;
        let mut child_stderr = child
            .stderr
            .take()
            .ok_or_else(|| SpawnError::Process("child stderr missing".to_string()))?;

        // Drain stdout (to file or buffer) and stderr concurrently —
        // both must be drained or the child can stall on a full pipe.
        let stdout_task: tokio::task::JoinHandle<std::io::Result<Vec<u8>>> = match stdout_to {
            Some(path) => tokio::spawn(async move {
                let mut file = tokio::fs::File::create(&path).await?;
                tokio::io::copy(&mut child_stdout, &mut file).await?;
                use tokio::io::AsyncWriteExt;
                // flush + sync are best-effort durability so a reader
                // opening the file right after spawn resolves sees all
                // bytes. The actual write happened in `copy` above; a
                // flush error here only loses the durability hint.
                if let Err(e) = file.flush().await {
                    tracing::warn!("spawn_cancellable: file flush failed: {}", e);
                }
                if let Err(e) = file.sync_all().await {
                    tracing::warn!("spawn_cancellable: file sync_all failed: {}", e);
                }
                Ok(Vec::new())
            }),
            None => tokio::spawn(async move {
                let mut buf = Vec::new();
                child_stdout.read_to_end(&mut buf).await?;
                Ok(buf)
            }),
        };
        let stderr_task: tokio::task::JoinHandle<std::io::Result<Vec<u8>>> =
            tokio::spawn(async move {
                let mut buf = Vec::new();
                child_stderr.read_to_end(&mut buf).await?;
                Ok(buf)
            });

        // Race child.wait() against kill_rx so the dispatcher can kill
        // mid-stream (e.g. user scrolled past the commit before git
        // finished).
        let exit_code = tokio::select! {
            status = child.wait() => status
                .map(|s| s.code().unwrap_or(-1))
                .unwrap_or(-1),
            _ = kill_rx => {
                // start_kill fails only when the process has already
                // exited — and we're about to `wait()` to reap either
                // way, so the failure path collapses with the success
                // path. Log at debug for diagnostic visibility.
                if let Err(e) = child.start_kill() {
                    tracing::debug!("spawn_cancellable: start_kill (already exited?): {}", e);
                }
                child.wait().await.map(|s| s.code().unwrap_or(-1)).unwrap_or(-1)
            }
        };

        // Both drain tasks must finish; on kill they get EOF when the
        // child's pipes close.
        let stdout_bytes = stdout_task
            .await
            .map_err(|e| SpawnError::Process(format!("stdout task: {}", e)))?
            .map_err(|e| SpawnError::Process(format!("stdout drain: {}", e)))?;
        let stderr_bytes = stderr_task
            .await
            .map_err(|e| SpawnError::Process(format!("stderr task: {}", e)))?
            .map_err(|e| SpawnError::Process(format!("stderr drain: {}", e)))?;

        Ok(SpawnResult {
            stdout: String::from_utf8_lossy(&stdout_bytes).to_string(),
            stderr: String::from_utf8_lossy(&stderr_bytes).to_string(),
            exit_code,
        })
    }

    /// Streaming override: pipe child stdout straight into `stdout_to`
    /// via `tokio::io::copy`. The 43 MB stdout of `git show` for the
    /// bun-rust-rewrite commit never lands in a single `String`.
    async fn spawn_to_file(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        stdout_to: std::path::PathBuf,
    ) -> Result<SpawnResult, SpawnError> {
        use std::process::Stdio;
        use tokio::io::AsyncWriteExt;

        gate(&self.trust, &command, cwd.as_deref())?;
        let mut cmd = tokio::process::Command::new(&command);
        cmd.args(&args);
        self.apply_env(&mut cmd).await;
        cmd.hide_window();
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        if let Some(ref dir) = cwd {
            cmd.current_dir(dir);
        }

        // Ensure the parent dir exists so the open below doesn't ENOENT.
        // Surface failures rather than letting File::create error with a
        // less informative message.
        if let Some(parent) = stdout_to.parent() {
            if !parent.as_os_str().is_empty() {
                tokio::fs::create_dir_all(parent).await.map_err(|e| {
                    SpawnError::Process(format!("create_dir_all {:?}: {}", parent, e))
                })?;
            }
        }

        let mut file = tokio::fs::File::create(&stdout_to)
            .await
            .map_err(|e| SpawnError::Process(format!("create {:?}: {}", stdout_to, e)))?;

        let mut child = cmd
            .spawn()
            .map_err(|e| SpawnError::Process(e.to_string()))?;

        let mut child_stdout = child
            .stdout
            .take()
            .ok_or_else(|| SpawnError::Process("child stdout missing".to_string()))?;
        let mut child_stderr = child
            .stderr
            .take()
            .ok_or_else(|| SpawnError::Process("child stderr missing".to_string()))?;

        // Copy stdout to file and drain stderr concurrently. Both ends
        // must be drained or the child can stall on a full pipe.
        let stdout_task = tokio::spawn(async move {
            let res = tokio::io::copy(&mut child_stdout, &mut file).await;
            // flush + sync are best-effort durability so a reader
            // opening the file right after spawn resolves sees all
            // bytes. The data was already written in `copy` above; a
            // flush error here only loses the durability hint.
            if let Err(e) = file.flush().await {
                tracing::warn!("spawn_to_file: file flush failed: {}", e);
            }
            if let Err(e) = file.sync_all().await {
                tracing::warn!("spawn_to_file: file sync_all failed: {}", e);
            }
            res
        });
        let stderr_task = tokio::spawn(async move {
            let mut buf = Vec::new();
            let res = tokio::io::copy(&mut child_stderr, &mut buf).await;
            res.map(|_| buf)
        });

        let status = child
            .wait()
            .await
            .map_err(|e| SpawnError::Process(format!("wait: {}", e)))?;

        // Drop the empty Vec from the streaming task — its only signal
        // is success/failure of the io::copy and flush, propagated via
        // the `?` operator.
        stdout_task
            .await
            .map_err(|e| SpawnError::Process(format!("stdout task: {}", e)))?
            .map_err(|e| SpawnError::Process(format!("stdout copy: {}", e)))?;
        let stderr_bytes = stderr_task
            .await
            .map_err(|e| SpawnError::Process(format!("stderr task: {}", e)))?
            .map_err(|e| SpawnError::Process(format!("stderr drain: {}", e)))?;

        Ok(SpawnResult {
            stdout: String::new(),
            stderr: String::from_utf8_lossy(&stderr_bytes).to_string(),
            exit_code: status.code().unwrap_or(-1),
        })
    }
}

/// Wrap `(command, args)` so the captured `env` is applied on a backend that
/// passes an argv array (SSH agent / docker) rather than a shell string:
/// `env K=V … command args…`. Empty env ⇒ unchanged. (`command` must not
/// contain `=`, which `env` would mistake for an assignment — true for any
/// program name.)
fn env_wrap(env: &[(String, String)], command: &str, args: &[String]) -> (String, Vec<String>) {
    if env.is_empty() {
        return (command.to_string(), args.to_vec());
    }
    let mut wrapped = Vec::with_capacity(env.len() + 1 + args.len());
    for (k, v) in env {
        wrapped.push(format!("{k}={v}"));
    }
    wrapped.push(command.to_string());
    wrapped.extend(args.iter().cloned());
    ("env".to_string(), wrapped)
}

/// Remote process spawner via SSH agent
pub struct RemoteProcessSpawner {
    channel: Arc<AgentChannel>,
    env: Arc<EnvProvider>,
    trust: Arc<WorkspaceTrust>,
}

impl RemoteProcessSpawner {
    /// Create a new remote process spawner gated by `trust`, applying the live
    /// `env` provider (captured on the remote host) to every spawn.
    pub fn new(
        channel: Arc<AgentChannel>,
        env: Arc<EnvProvider>,
        trust: Arc<WorkspaceTrust>,
    ) -> Self {
        Self {
            channel,
            env,
            trust,
        }
    }

    /// Capture the active env on the *remote* host by running the provider's
    /// script through the agent's raw `exec` (no env applied — recursion-free).
    async fn captured_env(&self) -> Vec<(String, String)> {
        let channel = self.channel.clone();
        self.env
            .current(move |script| async move {
                let params = exec_params("sh", &["-lc".to_string(), script], None);
                let (mut data_rx, _result) =
                    channel.request_streaming("exec", params).await.ok()?;
                let mut stdout = Vec::new();
                while let Some(d) = data_rx.recv().await {
                    if let Some(out) = d.get("out").and_then(|v| v.as_str()) {
                        if let Ok(b) = decode_base64(out) {
                            stdout.extend_from_slice(&b);
                        }
                    }
                }
                Some(String::from_utf8_lossy(&stdout).into_owned())
            })
            .await
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
        gate(&self.trust, &command, cwd.as_deref())?;
        let captured = self.captured_env().await;
        let (eff_cmd, eff_args) = env_wrap(&captured, &command, &args);
        let params = exec_params(&eff_cmd, &eff_args, cwd.as_deref());

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

    async fn spawn_to_file(
        &self,
        _command: String,
        _args: Vec<String>,
        _cwd: Option<String>,
        _stdout_to: std::path::PathBuf,
    ) -> Result<SpawnResult, SpawnError> {
        Err(SpawnError::Process(
            "stdoutTo is not supported for remote processes".to_string(),
        ))
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
pub struct LocalLongRunningSpawner {
    env: Arc<EnvProvider>,
    trust: Arc<WorkspaceTrust>,
}

impl LocalLongRunningSpawner {
    /// Local long-running spawner gated by `trust`, applying the live `env`
    /// provider's captured environment under each child's environment.
    pub fn new(env: Arc<EnvProvider>, trust: Arc<WorkspaceTrust>) -> Self {
        Self { env, trust }
    }
}

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
        gate(
            &self.trust,
            command,
            cwd.map(|p| p.to_string_lossy()).as_deref(),
        )?;
        let captured = local_captured_env(&self.env).await;
        let mut cmd = tokio::process::Command::new(command);
        cmd.args(args)
            // Provider env first, then the per-call env so the caller wins.
            .envs(captured.iter().map(|(k, v)| (k.as_str(), v.as_str())))
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
        // Honor the active env's PATH (e.g. a venv's `bin/`) so the existence
        // probe searches the same place `spawn_stdio` will — otherwise a
        // repo-local `pyright`/`ruff` looks missing and the server never
        // starts. Falls back to the process PATH when no env is active.
        let captured = local_captured_env(&self.env).await;
        if let Some((_, path)) = captured.iter().find(|(k, _)| k == "PATH") {
            let cwd = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("."));
            return which::which_in(command, Some(path), &cwd).is_ok();
        }
        which::which(command).is_ok()
    }
}

/// POSIX shell single-quote: wrap in `'…'`, escaping embedded quotes as
/// `'\''`. Safe to splice into a remote command string.
fn shell_quote(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('\'');
    for c in s.chars() {
        if c == '\'' {
            out.push_str("'\\''");
        } else {
            out.push(c);
        }
    }
    out.push('\'');
    out
}

/// Build the remote shell command for a long-running process:
/// `[cd <cwd> && ]exec env K=V… <command> <args…>` (all shell-quoted).
/// `exec` replaces the login shell so the server *is* the SSH channel's
/// process — EOF/kill propagate to it directly. `env` applies the injected
/// environment then execs the real binary.
fn build_remote_exec(
    env: &[(String, String)],
    cwd: Option<&str>,
    command: &str,
    args: &[String],
) -> String {
    let mut s = String::new();
    if let Some(dir) = cwd {
        s.push_str("cd ");
        s.push_str(&shell_quote(dir));
        s.push_str(" && ");
    }
    s.push_str("exec ");
    if !env.is_empty() {
        s.push_str("env ");
        for (k, v) in env {
            s.push_str(k);
            s.push('=');
            s.push_str(&shell_quote(v));
            s.push(' ');
        }
    }
    s.push_str(&shell_quote(command));
    for a in args {
        s.push(' ');
        s.push_str(&shell_quote(a));
    }
    s
}

/// Build the remote command for an existence probe. `command -v` is a shell
/// builtin (not a binary), so the env is applied via `export` assignments
/// rather than the `env` binary.
fn build_remote_command_exists(env: &[(String, String)], command: &str) -> String {
    let mut s = String::new();
    for (k, v) in env {
        s.push_str("export ");
        s.push_str(k);
        s.push('=');
        s.push_str(&shell_quote(v));
        s.push_str("; ");
    }
    s.push_str("command -v ");
    s.push_str(&shell_quote(command));
    s.push_str(" >/dev/null 2>&1");
    s
}

/// Assemble the `ssh` argv: connection options, `user@host`, then the single
/// remote command string (ssh concatenates trailing args with spaces, so the
/// command must already be one shell-quoted string). Mirrors the options the
/// agent connection uses.
fn build_ssh_args(
    params: &crate::services::remote::ConnectionParams,
    remote_cmd: &str,
) -> Vec<String> {
    let mut a = vec![
        "-o".to_string(),
        "StrictHostKeyChecking=accept-new".to_string(),
        "-o".to_string(),
        "BatchMode=yes".to_string(),
    ];
    if let Some(port) = params.port {
        a.push("-p".to_string());
        a.push(port.to_string());
    }
    if let Some(ref identity) = params.identity_file {
        a.push("-i".to_string());
        a.push(identity.to_string_lossy().into_owned());
    }
    a.push(format!("{}@{}", params.user, params.host));
    a.push(remote_cmd.to_string());
    a
}

/// Assemble the `ssh` argv for an *interactive terminal* under a remote
/// authority. Unlike [`build_ssh_args`] (one-shot, non-interactive LSP /
/// probe spawns) this:
///
/// * forces remote PTY allocation with `-t` so the remote shell behaves
///   interactively (job control, line editing, a real prompt);
/// * omits `BatchMode=yes` so auth prompts (key passphrase, password,
///   2FA) can surface *inside* the embedded terminal rather than failing;
/// * runs an interactive login shell (`exec ${SHELL:-/bin/sh} -l`) after
///   `cd`-ing into the workspace, so the user lands where the editor is
///   rooted with their normal remote environment.
///
/// Returned as the argv *after* the leading `ssh` program name; the caller
/// (the SSH terminal wrapper) sets `command = "ssh"` and these as `args`.
pub fn build_ssh_terminal_args(
    params: &crate::services::remote::ConnectionParams,
    remote_dir: Option<&str>,
) -> Vec<String> {
    let mut a = vec![
        "-t".to_string(),
        "-o".to_string(),
        "StrictHostKeyChecking=accept-new".to_string(),
    ];
    if let Some(port) = params.port {
        a.push("-p".to_string());
        a.push(port.to_string());
    }
    if let Some(ref identity) = params.identity_file {
        a.push("-i".to_string());
        a.push(identity.to_string_lossy().into_owned());
    }
    a.push(format!("{}@{}", params.user, params.host));

    // Land in the workspace (when known), then hand control to the user's
    // login shell. `remote_dir` is whatever path the URL pointed at, which
    // may be a *file* (`fresh ssh://host/proj/main.rs`) — so fall back to
    // its parent dir, and treat a failed `cd` as non-fatal so the shell
    // always starts. `exec` replaces the ssh-side shell so closing the
    // terminal tears the session down cleanly.
    let mut remote_cmd = String::new();
    if let Some(dir) = remote_dir.filter(|d| !d.is_empty()) {
        let quoted = shell_quote(dir);
        remote_cmd.push_str(&format!(
            "d={quoted}; [ -d \"$d\" ] || d=$(dirname \"$d\"); cd \"$d\" 2>/dev/null; "
        ));
    }
    remote_cmd.push_str("exec ${SHELL:-/bin/sh} -l");
    a.push(remote_cmd);
    a
}

/// Build the `kubectl` argv that opens the integrated terminal as an
/// interactive login shell *inside the pod*:
///
/// ```text
/// [--context CTX] exec -it -n NS [-c C] POD -- sh -lc 'cd WS; exec "$SHELL" -l'
/// ```
///
/// The EKS analogue of [`build_ssh_terminal_args`]. `-it` allocates a TTY (so
/// resize / curses apps work — `kubectl` itself implements the resize
/// protocol), and the `sh -lc` wrapper pins cwd, so the authority's terminal
/// wrapper sets `manages_cwd = true`. A failed `cd` is non-fatal so the shell
/// always starts; `exec` replaces the wrapper shell so closing the terminal
/// tears the exec session down cleanly.
pub fn build_eks_terminal_args(target: &crate::services::remote::EksTarget) -> Vec<String> {
    let mut remote_cmd = String::new();
    if let Some(dir) = target.workspace.as_deref().filter(|d| !d.is_empty()) {
        let quoted = shell_quote(dir);
        remote_cmd.push_str(&format!(
            "d={quoted}; [ -d \"$d\" ] || d=$(dirname \"$d\"); cd \"$d\" 2>/dev/null; "
        ));
    }
    remote_cmd.push_str("exec ${SHELL:-/bin/sh} -l");
    crate::services::remote::transport::kubectl_exec_argv(
        target,
        &["-it"],
        "sh",
        &["-lc".to_string(), remote_cmd],
    )
}

/// Long-running spawner over SSH: each LSP server (or tool agent) gets its own
/// `ssh user@host <remote-cmd>` subprocess, whose piped stdio *is* the remote
/// process's stdio. Returning a real local [`tokio::process::Child`] (the ssh
/// client) means the LSP I/O layer talks to ordinary `ChildStdin`/`ChildStdout`
/// with no awareness it's remote — the same trick the Docker spawner uses with
/// the local `docker` CLI.
///
/// This opens a separate SSH connection per server rather than multiplexing
/// through the agent: the agent's one-shot `exec` can't keep a process alive
/// with writable stdin, and abstracting `StdioChild` / the whole LSP I/O layer
/// over the agent channel would be a far larger change. The tradeoff is extra
/// SSH connections; the win is LSP that actually runs on the remote host
/// instead of the host-local fallback.
pub struct RemoteLongRunningSpawner {
    params: crate::services::remote::ConnectionParams,
    env: Arc<EnvProvider>,
    trust: Arc<WorkspaceTrust>,
}

impl RemoteLongRunningSpawner {
    /// Spawner for `params`, gated by `trust`, applying the live `env` provider
    /// (captured on the remote host) to every server it launches.
    pub fn new(
        params: crate::services::remote::ConnectionParams,
        env: Arc<EnvProvider>,
        trust: Arc<WorkspaceTrust>,
    ) -> Self {
        Self { params, env, trust }
    }

    /// Capture the active env on the *remote* host: run the provider's script
    /// through a one-shot `ssh … <script>` (raw — no env applied).
    async fn captured_env(&self) -> Vec<(String, String)> {
        let params = self.params.clone();
        self.env
            .current(move |script| async move {
                let ssh_args = build_ssh_args(&params, &script);
                let output = tokio::process::Command::new("ssh")
                    .args(&ssh_args)
                    .hide_window()
                    .output()
                    .await
                    .ok()?;
                Some(String::from_utf8_lossy(&output.stdout).into_owned())
            })
            .await
    }
}

#[async_trait::async_trait]
impl LongRunningSpawner for RemoteLongRunningSpawner {
    async fn spawn_stdio(
        &self,
        command: &str,
        args: &[String],
        env: Vec<(String, String)>,
        cwd: Option<&Path>,
        _limits: Option<&ProcessLimits>,
    ) -> Result<StdioChild, SpawnError> {
        // Host-side process limits don't reach a remote process (the local
        // PID is the ssh client), so `_limits` is ignored — same as Docker.
        let cwd_str = cwd.map(|p| p.to_string_lossy().into_owned());
        gate(&self.trust, command, cwd_str.as_deref())?;

        // Captured (provider) env first, then the per-call env so the caller
        // wins on conflict (mirrors the local layering).
        let mut merged = self.captured_env().await;
        merged.extend(env);

        let remote = build_remote_exec(&merged, cwd_str.as_deref(), command, args);
        let ssh_args = build_ssh_args(&self.params, &remote);

        let mut cmd = tokio::process::Command::new("ssh");
        cmd.args(&ssh_args)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .hide_window()
            .kill_on_drop(true);

        let child = cmd
            .spawn()
            .map_err(|e| SpawnError::Process(e.to_string()))?;
        // `spawned_locally = false`: the local PID is the ssh client, not the
        // remote server, so host-only resource controls skip themselves.
        Ok(StdioChild::from_tokio_child(child, false))
    }

    async fn command_exists(&self, command: &str) -> bool {
        let captured = self.captured_env().await;
        let remote = build_remote_command_exists(&captured, command);
        let ssh_args = build_ssh_args(&self.params, &remote);
        match tokio::process::Command::new("ssh")
            .args(&ssh_args)
            .hide_window()
            .output()
            .await
        {
            Ok(output) => output.status.success(),
            Err(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::AsyncReadExt;

    #[tokio::test]
    async fn test_local_spawner() {
        let spawner = LocalProcessSpawner::new(
            Arc::new(EnvProvider::inactive()),
            Arc::new(WorkspaceTrust::permissive()),
        );
        let result = spawner
            .spawn("echo".to_string(), vec!["hello".to_string()], None)
            .await
            .unwrap();

        assert_eq!(result.exit_code, 0);
        assert!(result.stdout.trim() == "hello");
    }

    #[tokio::test]
    async fn test_local_spawner_stdout_to_file() {
        let spawner = LocalProcessSpawner::new(
            Arc::new(EnvProvider::inactive()),
            Arc::new(WorkspaceTrust::permissive()),
        );
        let tmp =
            std::env::temp_dir().join(format!("fresh-spawner-test-{}.out", std::process::id()));
        // Best-effort cleanup of any leftover from a previous run.
        // Failure (e.g. NotFound) is fine — the spawn below will
        // create the file fresh.
        #[allow(clippy::let_underscore_must_use)]
        let _ = std::fs::remove_file(&tmp);
        let result = spawner
            .spawn_to_file(
                "echo".to_string(),
                vec!["hello-from-disk".to_string()],
                None,
                tmp.clone(),
            )
            .await
            .unwrap();

        assert_eq!(result.exit_code, 0);
        assert!(
            result.stdout.is_empty(),
            "stdout should be empty when streaming"
        );
        let contents = std::fs::read_to_string(&tmp).expect("output file should exist");
        assert_eq!(contents.trim(), "hello-from-disk");
        // Best-effort cleanup — leaving a temp file behind on
        // failure is acceptable and the next run's pre-cleanup
        // handles it.
        #[allow(clippy::let_underscore_must_use)]
        let _ = std::fs::remove_file(&tmp);
    }

    #[tokio::test]
    async fn test_local_spawner_cancellable_kill() {
        let spawner = LocalProcessSpawner::new(
            Arc::new(EnvProvider::inactive()),
            Arc::new(WorkspaceTrust::permissive()),
        );
        let (kill_tx, kill_rx) = tokio::sync::oneshot::channel::<()>();

        // Start a sleep that would take 30s normally; fire kill after 100ms.
        let task = tokio::spawn(async move {
            spawner
                .spawn_cancellable(
                    "sleep".to_string(),
                    vec!["30".to_string()],
                    None,
                    None,
                    kill_rx,
                )
                .await
        });

        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        // Fire the kill. Err means the receiver was dropped (task
        // already finished), which would mean the 30s sleep returned
        // promptly on its own — impossible in this test window, but
        // not worth a panic either way; the subsequent task.await
        // surfaces any real problem.
        #[allow(clippy::let_underscore_must_use)]
        let _ = kill_tx.send(());

        let start = std::time::Instant::now();
        let result = task.await.unwrap().unwrap();
        let elapsed = start.elapsed();

        // SIGKILL'd sleep on Unix returns exit_code 137 or -1 (no code).
        // The point is we returned promptly, not after 30s.
        assert!(
            elapsed < std::time::Duration::from_secs(5),
            "kill should be prompt, took {:?}",
            elapsed
        );
        assert_ne!(result.exit_code, 0, "killed process shouldn't be exit 0");
    }

    #[tokio::test]
    async fn local_long_running_spawn_stdio_pipes_output() {
        let spawner = LocalLongRunningSpawner::new(
            Arc::new(EnvProvider::inactive()),
            Arc::new(WorkspaceTrust::permissive()),
        );
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
        let spawner = LocalLongRunningSpawner::new(
            Arc::new(EnvProvider::inactive()),
            Arc::new(WorkspaceTrust::permissive()),
        );
        assert!(spawner.command_exists("sh").await);
        assert!(
            !spawner
                .command_exists("fresh-unlikely-binary-name-ygzu9")
                .await
        );
    }

    // Unix-only: the local env capture runs the recipe through a POSIX login
    // shell (`$SHELL -lc`, falling back to `/bin/sh`). On Windows there is no
    // such shell, so capture intentionally no-ops — there's nothing to assert.
    #[cfg(unix)]
    #[tokio::test]
    async fn local_spawner_applies_active_env_provider() {
        // Full local path: an active provider whose snippet exports a var →
        // captured via the login shell → injected into the spawned child.
        let env = Arc::new(EnvProvider::inactive());
        env.set("export FRESH_ENV_TEST=hi-from-provider".into(), None);
        let spawner = LocalProcessSpawner::new(env, Arc::new(WorkspaceTrust::permissive()));
        let result = spawner
            .spawn(
                "sh".into(),
                vec!["-c".into(), "printf %s \"$FRESH_ENV_TEST\"".into()],
                None,
            )
            .await
            .unwrap();
        assert_eq!(result.exit_code, 0);
        assert_eq!(result.stdout, "hi-from-provider");
    }

    #[tokio::test]
    async fn local_spawner_inactive_provider_injects_nothing() {
        let spawner = LocalProcessSpawner::new(
            Arc::new(EnvProvider::inactive()),
            Arc::new(WorkspaceTrust::permissive()),
        );
        let result = spawner
            .spawn(
                "sh".into(),
                vec!["-c".into(), "printf %s \"${FRESH_ENV_TEST:-unset}\"".into()],
                None,
            )
            .await
            .unwrap();
        assert_eq!(result.stdout, "unset");
    }

    // --- RemoteLongRunningSpawner command builders (pure, no SSH needed) ---

    #[test]
    fn shell_quote_wraps_and_escapes() {
        assert_eq!(shell_quote("abc"), "'abc'");
        assert_eq!(shell_quote("a b/c"), "'a b/c'");
        assert_eq!(shell_quote("a'b"), "'a'\\''b'");
    }

    #[test]
    fn build_remote_exec_with_cwd_and_env() {
        let env = vec![("VIRTUAL_ENV".to_string(), "/proj/.venv".to_string())];
        let s = build_remote_exec(&env, Some("/proj dir"), "python", &["x.py".to_string()]);
        assert_eq!(
            s,
            "cd '/proj dir' && exec env VIRTUAL_ENV='/proj/.venv' 'python' 'x.py'"
        );
    }

    #[test]
    fn build_remote_exec_minimal() {
        assert_eq!(build_remote_exec(&[], None, "gopls", &[]), "exec 'gopls'");
    }

    #[test]
    fn build_remote_command_exists_exports_env() {
        let env = vec![("PATH".to_string(), "/proj/.venv/bin:/usr/bin".to_string())];
        assert_eq!(
            build_remote_command_exists(&env, "pyright"),
            "export PATH='/proj/.venv/bin:/usr/bin'; command -v 'pyright' >/dev/null 2>&1"
        );
    }

    #[test]
    fn build_ssh_args_full() {
        let params = crate::services::remote::ConnectionParams {
            user: "u".into(),
            host: "h".into(),
            port: Some(2222),
            identity_file: Some(std::path::PathBuf::from("/k")),
        };
        let a = build_ssh_args(&params, "echo hi");
        let expected: Vec<String> = [
            "-o",
            "StrictHostKeyChecking=accept-new",
            "-o",
            "BatchMode=yes",
            "-p",
            "2222",
            "-i",
            "/k",
            "u@h",
            "echo hi",
        ]
        .into_iter()
        .map(String::from)
        .collect();
        assert_eq!(a, expected);
    }

    #[test]
    fn build_ssh_terminal_args_forces_tty_and_login_shell() {
        let params = crate::services::remote::ConnectionParams {
            user: "u".into(),
            host: "h".into(),
            port: Some(2222),
            identity_file: Some(std::path::PathBuf::from("/k")),
        };
        let a = build_ssh_terminal_args(&params, Some("/proj dir"));
        let expected: Vec<String> = [
            "-t",
            "-o",
            "StrictHostKeyChecking=accept-new",
            "-p",
            "2222",
            "-i",
            "/k",
            "u@h",
            "d='/proj dir'; [ -d \"$d\" ] || d=$(dirname \"$d\"); cd \"$d\" 2>/dev/null; exec ${SHELL:-/bin/sh} -l",
        ]
        .into_iter()
        .map(String::from)
        .collect();
        assert_eq!(a, expected);
        // No BatchMode — interactive auth must be able to prompt in the PTY.
        assert!(!a.iter().any(|s| s == "BatchMode=yes"));
    }

    #[test]
    fn build_ssh_terminal_args_without_dir_skips_cd() {
        let params = crate::services::remote::ConnectionParams {
            user: "u".into(),
            host: "h".into(),
            port: None,
            identity_file: None,
        };
        let a = build_ssh_terminal_args(&params, None);
        assert_eq!(
            a,
            vec![
                "-t",
                "-o",
                "StrictHostKeyChecking=accept-new",
                "u@h",
                "exec ${SHELL:-/bin/sh} -l",
            ]
        );
        // Empty dir is treated the same as no dir.
        assert_eq!(build_ssh_terminal_args(&params, Some("")), a);
    }

    #[test]
    fn build_eks_terminal_args_allocates_tty_and_pins_cwd() {
        let target = crate::services::remote::EksTarget {
            context: Some("prod".into()),
            namespace: "dev".into(),
            pod: "pod-1".into(),
            container: Some("app".into()),
            workspace: Some("/workspace".into()),
        };
        let a = build_eks_terminal_args(&target);
        let expected: Vec<String> = [
            "--context",
            "prod",
            "exec",
            "-it",
            "-n",
            "dev",
            "-c",
            "app",
            "pod-1",
            "--",
            "sh",
            "-lc",
            "d='/workspace'; [ -d \"$d\" ] || d=$(dirname \"$d\"); cd \"$d\" 2>/dev/null; exec ${SHELL:-/bin/sh} -l",
        ]
        .into_iter()
        .map(String::from)
        .collect();
        assert_eq!(a, expected);
    }

    #[test]
    fn build_eks_terminal_args_without_workspace_skips_cd() {
        let target = crate::services::remote::EksTarget {
            context: None,
            namespace: "dev".into(),
            pod: "pod-1".into(),
            container: None,
            workspace: None,
        };
        let a = build_eks_terminal_args(&target);
        assert_eq!(
            a,
            vec![
                "exec",
                "-it",
                "-n",
                "dev",
                "pod-1",
                "--",
                "sh",
                "-lc",
                "exec ${SHELL:-/bin/sh} -l",
            ]
        );
    }
}
