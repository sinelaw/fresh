//! SSH connection management
//!
//! Handles spawning SSH process and bootstrapping the Python agent.

use crate::services::process_hidden::HideWindow;
use crate::services::remote::channel::AgentChannel;
use crate::services::remote::protocol::AgentResponse;
use crate::services::remote::AGENT_SOURCE;
use std::path::PathBuf;
use std::process::Stdio;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStderr, Command};

/// Error type for SSH connection
#[derive(Debug, thiserror::Error)]
pub enum SshError {
    #[error("Failed to spawn SSH process ({0}). Is the `ssh` command installed and in your PATH?")]
    SpawnFailed(#[from] std::io::Error),

    #[error("Agent failed to start: {0}")]
    AgentStartFailed(String),

    #[error("Protocol version mismatch: expected {expected}, got {got}")]
    VersionMismatch { expected: u32, got: u32 },

    #[error("Connection closed")]
    ConnectionClosed,

    #[error("Authentication failed")]
    AuthenticationFailed,
}

/// SSH connection parameters
#[derive(Debug, Clone)]
pub struct ConnectionParams {
    /// SSH login user. `None` lets ssh pick the user (its config / the current
    /// local user), so `host` and `ssh://host` work without a `user@`.
    pub user: Option<String>,
    pub host: String,
    pub port: Option<u16>,
    pub identity_file: Option<PathBuf>,
    /// Extra `ssh` arguments inserted verbatim before the target on every ssh
    /// invocation (agent connect, reconnect, interactive terminal, LSP/probe
    /// spawns), so options like `-J jump` or `-o ProxyCommand=…` apply end to
    /// end rather than only to the initial connect.
    pub extra_args: Vec<String>,
}

impl ConnectionParams {
    /// Parse a connection string like `host`, `user@host`, or `user@host:port`
    /// (a leading `ssh://` is tolerated). The user is optional.
    pub fn parse(s: &str) -> Option<Self> {
        let s = s.strip_prefix("ssh://").unwrap_or(s);
        let (user_host, port) = if let Some((uh, p)) = s.rsplit_once(':') {
            if let Ok(port) = p.parse::<u16>() {
                (uh, Some(port))
            } else {
                (s, None)
            }
        } else {
            (s, None)
        };

        let (user, host) = match user_host.split_once('@') {
            Some((u, h)) => (Some(u.to_string()), h),
            None => (None, user_host),
        };
        if host.is_empty() || user.as_deref() == Some("") {
            return None;
        }

        Some(Self {
            user,
            host: host.to_string(),
            port,
            identity_file: None,
            extra_args: Vec::new(),
        })
    }

    /// The ssh target argument: `user@host` when a user is set, else bare
    /// `host` (ssh then resolves the user itself).
    pub fn ssh_target(&self) -> String {
        match &self.user {
            Some(user) if !user.is_empty() => format!("{user}@{}", self.host),
            _ => self.host.clone(),
        }
    }
}

impl std::fmt::Display for ConnectionParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.port {
            Some(port) => write!(f, "{}:{}", self.ssh_target(), port),
            None => write!(f, "{}", self.ssh_target()),
        }
    }
}

/// Active SSH connection with bootstrapped agent
pub struct SshConnection {
    /// SSH child process
    process: Child,
    /// Communication channel with agent (wrapped in Arc for sharing)
    channel: std::sync::Arc<AgentChannel>,
    /// Connection parameters
    params: ConnectionParams,
}

impl SshConnection {
    /// Establish a new SSH connection and bootstrap the agent
    pub async fn connect(params: ConnectionParams) -> Result<Self, SshError> {
        let mut cmd = Command::new("ssh");

        // Don't check host key strictly for ease of use
        cmd.arg("-o").arg("StrictHostKeyChecking=accept-new");

        if let Some(port) = params.port {
            cmd.arg("-p").arg(port.to_string());
        }

        if let Some(ref identity) = params.identity_file {
            cmd.arg("-i").arg(identity);
        }

        cmd.args(&params.extra_args);
        cmd.arg(params.ssh_target());

        // Bootstrap the agent using Python itself to read the exact byte count.
        // This avoids requiring bash or other shell utilities on the remote.
        // Python reads exactly N bytes (the agent code), execs it, and the agent
        // then continues reading from stdin for protocol messages.
        //
        // Note: SSH passes the remote command through a shell, so we need to
        // properly quote the Python code. We use double quotes for the outer
        // shell and avoid problematic characters in the Python code.
        let agent_len = AGENT_SOURCE.len();
        let bootstrap = format!(
            "python3 -u -c \"import sys;exec(sys.stdin.read({}))\"",
            agent_len
        );
        cmd.arg(bootstrap);

        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        // Capture ssh's stderr instead of inheriting it. The editor runs a
        // full-screen ratatui UI on the alternate screen; an inherited stderr
        // lets ssh scribble its diagnostics ("Could not resolve hostname …")
        // straight over the rendered UI. ratatui has no idea those cells
        // changed, so the garbage persists until the next full repaint — the
        // "corrupted window" users see after a bad host. We pipe stderr and
        // fold its message into the connection error instead (see
        // `ssh_eof_error`), so a failed connect becomes a clean status line.
        cmd.stderr(Stdio::piped());
        // Kill the ssh process if this connect future is dropped before it
        // finishes (e.g. the New-Session dialog's Cancel aborts the connect
        // task while the handshake is still hanging). Without this a hung
        // connect would orphan the ssh child until it timed out on its own.
        // For an established carrier `SshConnection`'s Drop also kills it; this
        // covers the window before the connection object exists.
        cmd.kill_on_drop(true);
        cmd.hide_window();

        tracing::debug!(target = %params.ssh_target(), "ssh connect: spawning ssh child");
        let mut child = cmd.spawn()?;
        tracing::debug!("ssh connect: ssh child spawned");

        // Get handles
        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| SshError::AgentStartFailed("failed to get stdin".to_string()))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| SshError::AgentStartFailed("failed to get stdout".to_string()))?;
        let stderr = child.stderr.take();

        // Send the agent code (exact byte count). If the carrier already died
        // (a failed connect — e.g. the host was unreachable), this write/flush
        // races the child's exit and can fail with a broken pipe. That pipe
        // error isn't the actionable reason; the carrier's own stderr is. Fall
        // through to the same EOF path so we surface "ssh: …" rather than a bare
        // `SpawnFailed`, regardless of which side loses the race.
        tracing::debug!(agent_len, "ssh connect: sending agent bootstrap to stdin");
        if stdin.write_all(AGENT_SOURCE.as_bytes()).await.is_err() || stdin.flush().await.is_err() {
            return Err(ssh_eof_error(&mut child, &params, stderr).await);
        }

        // Create buffered reader for stdout
        let mut reader = BufReader::new(stdout);

        // Wait for ready message from agent
        // No timeout needed - all failure modes (auth failure, network issues, etc.)
        // result in SSH exiting and us getting EOF. User can Ctrl+C if needed.
        tracing::debug!("ssh connect: awaiting agent ready line (blocks on handshake/auth)");
        let mut ready_line = String::new();
        match reader.read_line(&mut ready_line).await {
            Ok(0) => {
                return Err(ssh_eof_error(&mut child, &params, stderr).await);
            }
            Ok(_) => {}
            Err(e) => return Err(SshError::AgentStartFailed(format!("read error: {}", e))),
        }
        tracing::debug!("ssh connect: agent ready line received");

        // Connected. Drain ssh's stderr for the life of the connection so the
        // occasional later diagnostic (host-key warnings, etc.) is discarded
        // rather than filling the pipe or — if we'd inherited it — landing on
        // the editor's screen.
        if let Some(mut stderr) = stderr {
            tokio::spawn(async move {
                let mut sink = tokio::io::sink();
                // Best-effort drain; the byte count / EOF error is irrelevant
                // since we're discarding ssh's stderr for the session.
                #[allow(clippy::let_underscore_must_use)]
                let _ = tokio::io::copy(&mut stderr, &mut sink).await;
            });
        }

        let ready: AgentResponse = serde_json::from_str(&ready_line).map_err(|e| {
            SshError::AgentStartFailed(format!(
                "invalid ready message '{}': {}",
                ready_line.trim(),
                e
            ))
        })?;

        if !ready.is_ready() {
            return Err(SshError::AgentStartFailed(
                "agent did not send ready message".to_string(),
            ));
        }

        // Check protocol version
        let version = ready.version.unwrap_or(0);
        if version != crate::services::remote::protocol::PROTOCOL_VERSION {
            return Err(SshError::VersionMismatch {
                expected: crate::services::remote::protocol::PROTOCOL_VERSION,
                got: version,
            });
        }

        tracing::debug!(version, "ssh connect: agent ready, protocol version ok");

        // Create channel (takes ownership of stdin for writing)
        let channel = std::sync::Arc::new(AgentChannel::new(reader, stdin));

        Ok(Self {
            process: child,
            channel,
            params,
        })
    }

    /// Get the communication channel as an Arc for sharing
    pub fn channel(&self) -> std::sync::Arc<AgentChannel> {
        self.channel.clone()
    }

    /// Get connection parameters
    pub fn params(&self) -> &ConnectionParams {
        &self.params
    }

    /// Check if the connection is still alive
    pub fn is_connected(&self) -> bool {
        self.channel.is_connected()
    }

    /// Get the connection string for display
    pub fn connection_string(&self) -> String {
        self.params.to_string()
    }
}

impl Drop for SshConnection {
    fn drop(&mut self) {
        // Best-effort kill of the SSH process during cleanup.
        // If it fails (process already exited, permission error, etc.)
        // there's nothing we can do in a Drop impl — the OS will clean
        // up the zombie when our process exits.
        if let Ok(()) = self.process.start_kill() {}
    }
}

/// Default interval between reconnection attempts.
const DEFAULT_RECONNECT_INTERVAL: std::time::Duration = std::time::Duration::from_secs(5);

/// Configuration for the reconnect task.
pub struct ReconnectConfig {
    /// How long to wait between reconnection attempts.
    pub interval: std::time::Duration,
}

impl Default for ReconnectConfig {
    fn default() -> Self {
        Self {
            interval: DEFAULT_RECONNECT_INTERVAL,
        }
    }
}

/// Spawn a background task that automatically reconnects when the channel
/// disconnects.
///
/// The task monitors `channel.is_connected()` and, when false, attempts to
/// establish a new SSH connection using the given `params`. On success, it
/// calls `channel.replace_transport()` to hot-swap the underlying reader/writer.
///
/// The task runs until the channel is dropped (write_tx closed) or the
/// returned `tokio::task::JoinHandle` is aborted.
pub fn spawn_reconnect_task(
    channel: std::sync::Arc<AgentChannel>,
    params: ConnectionParams,
) -> tokio::task::JoinHandle<()> {
    let connect_fn = move || {
        let params = params.clone();
        async move {
            let (reader, writer, _child) = establish_ssh_transport(&params).await?;
            // Box the reader/writer so they have a uniform type
            let reader: Box<dyn tokio::io::AsyncBufRead + Unpin + Send> = Box::new(reader);
            let writer: Box<dyn tokio::io::AsyncWrite + Unpin + Send> = Box::new(writer);
            Ok::<_, SshError>((reader, writer))
        }
    };

    spawn_reconnect_task_with(
        channel,
        connect_fn,
        ReconnectConfig::default(),
        "SSH remote",
    )
}

/// Spawn a reconnect task with a custom connection factory.
///
/// This is the generic version used by both production (via `spawn_reconnect_task`)
/// and tests (with a fake connection factory). The `connect_fn` is called each
/// time a reconnection attempt is made. It should return a `(reader, writer)` pair
/// on success.
pub fn spawn_reconnect_task_with<F, Fut>(
    channel: std::sync::Arc<AgentChannel>,
    connect_fn: F,
    config: ReconnectConfig,
    label: &'static str,
) -> tokio::task::JoinHandle<()>
where
    F: Fn() -> Fut + Send + 'static,
    Fut: std::future::Future<
            Output = Result<
                (
                    Box<dyn tokio::io::AsyncBufRead + Unpin + Send>,
                    Box<dyn tokio::io::AsyncWrite + Unpin + Send>,
                ),
                SshError,
            >,
        > + Send,
{
    tokio::spawn(async move {
        loop {
            // Wait until disconnected
            while channel.is_connected() {
                tokio::time::sleep(config.interval).await;
            }

            tracing::info!("{label}: connection lost, attempting reconnection...");

            // Retry loop
            loop {
                tokio::time::sleep(config.interval).await;

                // Check if channel was dropped (write_tx gone)
                if !channel.is_connected() {
                    // Still disconnected — try to reconnect
                } else {
                    // Something else reconnected us (e.g., manual replace_transport)
                    break;
                }

                match (connect_fn)().await {
                    Ok((reader, writer)) => {
                        tracing::info!("{label}: reconnected successfully");
                        channel.replace_transport(reader, writer).await;
                        break;
                    }
                    Err(e) => {
                        tracing::debug!("{label}: reconnection attempt failed: {e}");
                    }
                }
            }
        }
    })
}

/// Default heartbeat interval. Comfortably under the smallest common
/// load-balancer / NAT idle timeout (~5 min) so an otherwise-idle agent
/// stream keeps generating traffic and isn't silently dropped.
pub const DEFAULT_HEARTBEAT_INTERVAL: std::time::Duration = std::time::Duration::from_secs(60);

/// Spawn a background task that pings the agent periodically so an idle
/// connection's stream keeps producing traffic.
///
/// Long-lived agent streams that sit idle (no edits, no LSP chatter) get
/// silently dropped by ELB / NAT idle timers after a few minutes — the
/// client never sees a FIN, so the *next* request just hangs until it
/// times out and the UI appears frozen. A cheap periodic `info` request
/// keeps the NAT state-table entry warm. Shared by every agent transport
/// (SSH and `kubectl exec` alike); `info` is already handled by every
/// agent version, so no protocol bump is needed.
///
/// Holds only a `Weak` reference, so the task terminates on its own once
/// the last owner of the channel is dropped — no JoinHandle bookkeeping
/// is required to avoid a leak (callers may still `abort()` it to stop
/// pinging immediately when the carrier dies). Pinging while disconnected
/// is skipped; the reconnect task owns re-establishment.
pub fn spawn_heartbeat_task(
    channel: &std::sync::Arc<AgentChannel>,
    interval: std::time::Duration,
) -> tokio::task::JoinHandle<()> {
    let weak = std::sync::Arc::downgrade(channel);
    tokio::spawn(async move {
        loop {
            tokio::time::sleep(interval).await;
            let Some(channel) = weak.upgrade() else {
                break;
            };
            if channel.is_connected() {
                // Outcome ignored on purpose: a failed/timed-out ping
                // already marks the channel disconnected (see `request`),
                // and the reconnect task owns recovery from there. Bound
                // to a named `_` to satisfy `deny(let_underscore_must_use)`.
                let _ping = channel.request("info", serde_json::json!({})).await;
            }
        }
    })
}

/// Establish a new SSH connection and return the raw transport + child process.
///
/// Build a descriptive error when the SSH process closes stdout (EOF) without
/// sending a ready message. We wait for the SSH process to exit and inspect its
/// exit code to give the user a more actionable message than a generic
/// "connection closed".
async fn ssh_eof_error(
    child: &mut Child,
    params: &ConnectionParams,
    stderr: Option<ChildStderr>,
) -> SshError {
    // Give SSH a moment to finish so we can read its exit code.
    let status = tokio::time::timeout(std::time::Duration::from_secs(5), child.wait()).await;

    let hint = match status {
        Ok(Ok(status)) => {
            match status.code() {
                // 255 is SSH's conventional exit code for connection errors
                // (host unreachable, connection refused, DNS failure, auth
                // failure, etc.).
                Some(255) => format!(
                    "SSH could not connect to {}. Check that the host is \
                     reachable, the hostname is correct, and your SSH \
                     credentials are valid (exit code 255)",
                    params
                ),
                // 127 is the shell's "command not found" — for our bootstrap
                // that means `python3` is missing on the remote. Fresh's remote
                // backend (agent + the integrated terminal's env launcher) runs
                // on python3, so name the requirement and the fix plainly.
                Some(127) => format!(
                    "Python 3 was not found on the remote host {}. \
                     Fresh's remote support requires python3 on the remote — \
                     install it there, then reconnect",
                    params
                ),
                Some(code) => format!(
                    "SSH process exited with code {} while connecting to {}",
                    code, params
                ),
                None => format!(
                    "SSH process was killed by a signal while connecting to {}",
                    params
                ),
            }
        }
        Ok(Err(e)) => format!("failed to get SSH exit status: {}", e),
        Err(_) => {
            // Timed out waiting for exit — kill it so we don't leak.
            if let Err(e) = child.start_kill() {
                tracing::warn!("Failed to kill timed-out SSH process: {}", e);
            }
            format!(
                "SSH process did not exit in time while connecting to {}",
                params
            )
        }
    };

    // ssh writes the actionable reason ("Could not resolve hostname",
    // "Permission denied", "Connection refused", …) to stderr. We piped it
    // (rather than letting it corrupt the editor's screen), so fold the most
    // specific line into the error for the status bar.
    match read_ssh_stderr(stderr).await {
        Some(detail) => SshError::AgentStartFailed(format!("{hint}: {detail}")),
        None => SshError::AgentStartFailed(hint),
    }
}

/// Read whatever a failed ssh process wrote to stderr and return its most
/// specific (last non-empty) line. ssh has closed stdout by the time we call
/// this and is exiting, so the read is bounded; we still cap the wait so a
/// wedged pipe can't hang the error path.
async fn read_ssh_stderr(stderr: Option<ChildStderr>) -> Option<String> {
    let mut stderr = stderr?;
    let mut buf = String::new();
    #[allow(clippy::let_underscore_must_use)]
    let _ = tokio::time::timeout(
        std::time::Duration::from_secs(2),
        stderr.read_to_string(&mut buf),
    )
    .await;
    buf.trim()
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .next_back()
        .map(str::to_string)
}

/// This is the lower-level function used by both `SshConnection::connect` and
/// the reconnect task. It spawns an SSH process, bootstraps the Python agent,
/// and returns the reader/writer pair ready for use with `AgentChannel`.
async fn establish_ssh_transport(
    params: &ConnectionParams,
) -> Result<
    (
        BufReader<tokio::process::ChildStdout>,
        tokio::process::ChildStdin,
        Child,
    ),
    SshError,
> {
    let mut cmd = Command::new("ssh");

    cmd.arg("-o").arg("StrictHostKeyChecking=accept-new");
    // Disable password prompts for reconnection (non-interactive)
    cmd.arg("-o").arg("BatchMode=yes");

    if let Some(port) = params.port {
        cmd.arg("-p").arg(port.to_string());
    }

    if let Some(ref identity) = params.identity_file {
        cmd.arg("-i").arg(identity);
    }

    cmd.args(&params.extra_args);
    cmd.arg(params.ssh_target());

    let agent_len = AGENT_SOURCE.len();
    let bootstrap = format!(
        "python3 -u -c \"import sys;exec(sys.stdin.read({}))\"",
        agent_len
    );
    cmd.arg(bootstrap);

    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::null()); // No terminal for reconnection
    cmd.hide_window();

    let mut child = cmd.spawn()?;

    let mut stdin = child
        .stdin
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdin".to_string()))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdout".to_string()))?;

    // Send the agent code
    stdin.write_all(AGENT_SOURCE.as_bytes()).await?;
    stdin.flush().await?;

    let mut reader = BufReader::new(stdout);

    // Wait for ready message
    let mut ready_line = String::new();
    match reader.read_line(&mut ready_line).await {
        Ok(0) => {
            // Reconnect spawns with `stderr(Stdio::null())`, so there is no
            // captured stderr to attach here.
            return Err(ssh_eof_error(&mut child, params, None).await);
        }
        Ok(_) => {}
        Err(e) => return Err(SshError::AgentStartFailed(format!("read error: {}", e))),
    }

    let ready: AgentResponse = serde_json::from_str(&ready_line).map_err(|e| {
        SshError::AgentStartFailed(format!(
            "invalid ready message '{}': {}",
            ready_line.trim(),
            e
        ))
    })?;

    if !ready.is_ready() {
        return Err(SshError::AgentStartFailed(
            "agent did not send ready message".to_string(),
        ));
    }

    let version = ready.version.unwrap_or(0);
    if version != crate::services::remote::protocol::PROTOCOL_VERSION {
        return Err(SshError::VersionMismatch {
            expected: crate::services::remote::protocol::PROTOCOL_VERSION,
            got: version,
        });
    }

    Ok((reader, stdin, child))
}

/// Spawn a local agent process for testing (no SSH)
///
/// This is used by integration tests to test the full stack without SSH.
/// Not intended for production use.
#[doc(hidden)]
pub async fn spawn_local_agent() -> Result<std::sync::Arc<AgentChannel>, SshError> {
    use tokio::process::Command as TokioCommand;

    let mut child = TokioCommand::new("python3")
        .arg("-u")
        .arg("-c")
        .arg(AGENT_SOURCE)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .hide_window()
        .spawn()?;

    let stdin = child
        .stdin
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdin".to_string()))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdout".to_string()))?;

    let mut reader = BufReader::new(stdout);

    // Wait for ready message
    let mut ready_line = String::new();
    reader.read_line(&mut ready_line).await?;

    let ready: AgentResponse = serde_json::from_str(&ready_line)
        .map_err(|e| SshError::AgentStartFailed(format!("invalid ready message: {}", e)))?;

    if !ready.is_ready() {
        return Err(SshError::AgentStartFailed(
            "agent did not send ready message".to_string(),
        ));
    }

    Ok(std::sync::Arc::new(AgentChannel::new(reader, stdin)))
}

/// Spawn a local Python agent with a custom data channel capacity.
///
/// Same as `spawn_local_agent` but allows overriding the channel capacity
/// for stress-testing backpressure handling.
#[doc(hidden)]
pub async fn spawn_local_agent_with_capacity(
    data_channel_capacity: usize,
) -> Result<std::sync::Arc<AgentChannel>, SshError> {
    use tokio::process::Command as TokioCommand;

    let mut child = TokioCommand::new("python3")
        .arg("-u")
        .arg("-c")
        .arg(AGENT_SOURCE)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .hide_window()
        .spawn()?;

    let stdin = child
        .stdin
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdin".to_string()))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdout".to_string()))?;

    let mut reader = BufReader::new(stdout);

    // Wait for ready message
    let mut ready_line = String::new();
    reader.read_line(&mut ready_line).await?;

    let ready: AgentResponse = serde_json::from_str(&ready_line)
        .map_err(|e| SshError::AgentStartFailed(format!("invalid ready message: {}", e)))?;

    if !ready.is_ready() {
        return Err(SshError::AgentStartFailed(
            "agent did not send ready message".to_string(),
        ));
    }

    Ok(std::sync::Arc::new(AgentChannel::with_capacity(
        reader,
        stdin,
        data_channel_capacity,
    )))
}

/// Spawn a local Python agent and return the raw reader/writer transport.
///
/// Unlike `spawn_local_agent`, this does NOT create an `AgentChannel`. It
/// returns the ready-to-use reader and writer so callers can feed them to
/// `AgentChannel::replace_transport()` for reconnection testing.
#[doc(hidden)]
pub async fn spawn_local_agent_transport() -> Result<
    (
        tokio::io::BufReader<tokio::process::ChildStdout>,
        tokio::process::ChildStdin,
    ),
    SshError,
> {
    use tokio::process::Command as TokioCommand;

    let mut child = TokioCommand::new("python3")
        .arg("-u")
        .arg("-c")
        .arg(AGENT_SOURCE)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .hide_window()
        .spawn()?;

    let stdin = child
        .stdin
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdin".to_string()))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdout".to_string()))?;

    let mut reader = BufReader::new(stdout);

    // Wait for ready message
    let mut ready_line = String::new();
    reader.read_line(&mut ready_line).await?;

    let ready: AgentResponse = serde_json::from_str(&ready_line)
        .map_err(|e| SshError::AgentStartFailed(format!("invalid ready message: {}", e)))?;

    if !ready.is_ready() {
        return Err(SshError::AgentStartFailed(
            "agent did not send ready message".to_string(),
        ));
    }

    Ok((reader, stdin))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_connection_params() {
        let params = ConnectionParams::parse("user@host").unwrap();
        assert_eq!(params.user.as_deref(), Some("user"));
        assert_eq!(params.host, "host");
        assert_eq!(params.port, None);

        let params = ConnectionParams::parse("user@host:22").unwrap();
        assert_eq!(params.user.as_deref(), Some("user"));
        assert_eq!(params.host, "host");
        assert_eq!(params.port, Some(22));

        // User is optional: bare host and ssh:// both parse, user = None.
        let params = ConnectionParams::parse("hostonly").unwrap();
        assert_eq!(params.user, None);
        assert_eq!(params.host, "hostonly");
        assert_eq!(params.ssh_target(), "hostonly");

        let params = ConnectionParams::parse("ssh://example.com:2222").unwrap();
        assert_eq!(params.user, None);
        assert_eq!(params.host, "example.com");
        assert_eq!(params.port, Some(2222));

        // Empty user / empty host are still rejected.
        assert!(ConnectionParams::parse("@host").is_none());
        assert!(ConnectionParams::parse("user@").is_none());
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn heartbeat_keeps_channel_warm_and_exits_on_drop() {
        // Real agent over local stdio — no SSH/kubectl, same channel.
        let channel = spawn_local_agent().await.expect("spawn local agent");
        let handle = spawn_heartbeat_task(&channel, std::time::Duration::from_millis(30));

        // Let several heartbeats fire; the channel must stay healthy.
        tokio::time::sleep(std::time::Duration::from_millis(150)).await;
        assert!(
            channel.is_connected(),
            "channel stays connected while heartbeat pings"
        );
        assert!(
            channel.request("info", serde_json::json!({})).await.is_ok(),
            "agent still answers after heartbeats"
        );

        // Dropping the last strong ref lets the Weak-based task terminate
        // on its own — proving it can't leak past the connection's life.
        drop(channel);
        tokio::time::timeout(std::time::Duration::from_secs(3), handle)
            .await
            .expect("heartbeat task exits after the channel is dropped")
            .expect("heartbeat task did not panic");
    }

    #[test]
    fn test_connection_string() {
        let params = ConnectionParams {
            user: Some("alice".to_string()),
            host: "example.com".to_string(),
            port: None,
            identity_file: None,
            extra_args: Vec::new(),
        };
        assert_eq!(params.to_string(), "alice@example.com");

        let params = ConnectionParams {
            user: Some("bob".to_string()),
            host: "server.local".to_string(),
            port: Some(2222),
            identity_file: None,
            extra_args: Vec::new(),
        };
        assert_eq!(params.to_string(), "bob@server.local:2222");

        // No user: the target (and display) is the bare host.
        let params = ConnectionParams {
            user: None,
            host: "server.local".to_string(),
            port: Some(2222),
            identity_file: None,
            extra_args: Vec::new(),
        };
        assert_eq!(params.to_string(), "server.local:2222");
        assert_eq!(params.ssh_target(), "server.local");
    }
}
