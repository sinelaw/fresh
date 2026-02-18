//! SSH connection management
//!
//! Handles spawning SSH process and bootstrapping the Python agent.

use crate::services::remote::channel::AgentChannel;
use crate::services::remote::protocol::AgentResponse;
use crate::services::remote::AGENT_SOURCE;
use std::path::PathBuf;
use std::process::Stdio;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, Command};

/// Error type for SSH connection
#[derive(Debug, thiserror::Error)]
pub enum SshError {
    #[error("Failed to spawn SSH process: {0}")]
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
    pub user: String,
    pub host: String,
    pub port: Option<u16>,
    pub identity_file: Option<PathBuf>,
}

impl ConnectionParams {
    /// Parse a connection string like "user@host" or "user@host:port"
    pub fn parse(s: &str) -> Option<Self> {
        let (user_host, port) = if let Some((uh, p)) = s.rsplit_once(':') {
            if let Ok(port) = p.parse::<u16>() {
                (uh, Some(port))
            } else {
                (s, None)
            }
        } else {
            (s, None)
        };

        let (user, host) = user_host.split_once('@')?;
        if user.is_empty() || host.is_empty() {
            return None;
        }

        Some(Self {
            user: user.to_string(),
            host: host.to_string(),
            port,
            identity_file: None,
        })
    }

    /// Format as connection string
    pub fn to_string(&self) -> String {
        if let Some(port) = self.port {
            format!("{}@{}:{}", self.user, self.host, port)
        } else {
            format!("{}@{}", self.user, self.host)
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
        // Allow password prompts - SSH will use the terminal for this
        // Note: We inherit stderr so SSH can prompt for password if needed

        if let Some(port) = params.port {
            cmd.arg("-p").arg(port.to_string());
        }

        if let Some(ref identity) = params.identity_file {
            cmd.arg("-i").arg(identity);
        }

        cmd.arg(format!("{}@{}", params.user, params.host));

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
        // Inherit stderr so SSH can prompt for password on the terminal
        cmd.stderr(Stdio::inherit());

        let mut child = cmd.spawn()?;

        // Get handles
        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| SshError::AgentStartFailed("failed to get stdin".to_string()))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| SshError::AgentStartFailed("failed to get stdout".to_string()))?;
        // Note: stderr is inherited so SSH can prompt for password on the terminal

        // Send the agent code (exact byte count)
        stdin.write_all(AGENT_SOURCE.as_bytes()).await?;
        stdin.flush().await?;

        // Create buffered reader for stdout
        let mut reader = BufReader::new(stdout);

        // Wait for ready message from agent
        // No timeout needed - all failure modes (auth failure, network issues, etc.)
        // result in SSH exiting and us getting EOF. User can Ctrl+C if needed.
        let mut ready_line = String::new();
        match reader.read_line(&mut ready_line).await {
            Ok(0) => {
                return Err(SshError::AgentStartFailed(
                    "connection closed (check terminal for SSH errors)".to_string(),
                ));
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

        // Check protocol version
        let version = ready.version.unwrap_or(0);
        if version != crate::services::remote::protocol::PROTOCOL_VERSION {
            return Err(SshError::VersionMismatch {
                expected: crate::services::remote::protocol::PROTOCOL_VERSION,
                got: version,
            });
        }

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
        match self.process.start_kill() {
            Ok(()) => {}
            Err(_) => {}
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_connection_params() {
        let params = ConnectionParams::parse("user@host").unwrap();
        assert_eq!(params.user, "user");
        assert_eq!(params.host, "host");
        assert_eq!(params.port, None);

        let params = ConnectionParams::parse("user@host:22").unwrap();
        assert_eq!(params.user, "user");
        assert_eq!(params.host, "host");
        assert_eq!(params.port, Some(22));

        assert!(ConnectionParams::parse("hostonly").is_none());
        assert!(ConnectionParams::parse("@host").is_none());
        assert!(ConnectionParams::parse("user@").is_none());
    }

    #[test]
    fn test_connection_string() {
        let params = ConnectionParams {
            user: "alice".to_string(),
            host: "example.com".to_string(),
            port: None,
            identity_file: None,
        };
        assert_eq!(params.to_string(), "alice@example.com");

        let params = ConnectionParams {
            user: "bob".to_string(),
            host: "server.local".to_string(),
            port: Some(2222),
            identity_file: None,
        };
        assert_eq!(params.to_string(), "bob@server.local:2222");
    }
}
