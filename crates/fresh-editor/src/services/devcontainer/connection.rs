//! Container connection management
//!
//! Establishes a connection to a running devcontainer by spawning the
//! Python agent inside the container via `docker exec`, reusing the
//! same `AgentChannel` infrastructure as SSH remote editing.

use crate::services::remote::{
    spawn_reconnect_task_with, AgentChannel, AgentResponse, ReconnectConfig, SshError,
    AGENT_SOURCE, PROTOCOL_VERSION,
};
use std::path::PathBuf;
use std::process::Stdio;
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, Command};

/// Active connection to a devcontainer with a bootstrapped agent
pub struct ContainerConnection {
    /// The docker exec child process
    process: Child,
    /// Communication channel with agent
    channel: Arc<AgentChannel>,
    /// Docker container ID
    container_id: String,
    /// Workspace folder inside the container
    workspace_folder: PathBuf,
}

impl ContainerConnection {
    /// Establish a new connection to a running container by bootstrapping
    /// the Python agent via `docker exec`.
    pub async fn connect(
        container_id: String,
        workspace_folder: PathBuf,
    ) -> Result<Self, SshError> {
        let (reader, writer, process) = establish_docker_transport(&container_id).await?;

        let channel = Arc::new(AgentChannel::new(reader, writer));

        Ok(Self {
            process,
            channel,
            container_id,
            workspace_folder,
        })
    }

    /// Get the communication channel as an Arc for sharing
    pub fn channel(&self) -> Arc<AgentChannel> {
        self.channel.clone()
    }

    /// Get the container ID
    pub fn container_id(&self) -> &str {
        &self.container_id
    }

    /// Get the workspace folder inside the container
    pub fn workspace_folder(&self) -> &Path {
        &self.workspace_folder
    }

    /// Check if the connection is still alive
    pub fn is_connected(&self) -> bool {
        self.channel.is_connected()
    }

    /// Get the connection string for display (used in status bar)
    pub fn connection_string(&self) -> String {
        // Show short container ID (first 12 chars like Docker does)
        let short_id = if self.container_id.len() > 12 {
            &self.container_id[..12]
        } else {
            &self.container_id
        };
        format!("Container:{}", short_id)
    }

    /// Spawn a background reconnection task for this container.
    ///
    /// Reuses the generic `spawn_reconnect_task_with` from the remote module,
    /// providing a Docker-specific connect function.
    pub fn spawn_reconnect_task(&self) -> tokio::task::JoinHandle<()> {
        let channel = self.channel.clone();
        let container_id = self.container_id.clone();

        let connect_fn = move || {
            let container_id = container_id.clone();
            async move {
                let (reader, writer, _child) =
                    establish_docker_transport(&container_id).await?;
                let reader: Box<dyn tokio::io::AsyncBufRead + Unpin + Send> = Box::new(reader);
                let writer: Box<dyn tokio::io::AsyncWrite + Unpin + Send> = Box::new(writer);
                Ok::<_, SshError>((reader, writer))
            }
        };

        spawn_reconnect_task_with(
            channel,
            connect_fn,
            ReconnectConfig::default(),
            "Devcontainer",
        )
    }
}

impl Drop for ContainerConnection {
    fn drop(&mut self) {
        // Best-effort kill of the docker exec process.
        // The container itself continues running (it's detached).
        if let Ok(()) = self.process.start_kill() {}
    }
}

use std::path::Path;

/// Establish a transport to a container by running `docker exec -i` with
/// the Python agent bootstrap, mirroring the SSH transport pattern.
async fn establish_docker_transport(
    container_id: &str,
) -> Result<
    (
        BufReader<tokio::process::ChildStdout>,
        tokio::process::ChildStdin,
        Child,
    ),
    SshError,
> {
    let agent_len = AGENT_SOURCE.len();
    let bootstrap = format!(
        "python3 -u -c \"import sys;exec(sys.stdin.read({}))\"",
        agent_len
    );

    let mut cmd = Command::new("docker");
    cmd.args(["exec", "-i", container_id, "sh", "-c", &bootstrap]);
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::null());

    let mut child = cmd.spawn()?;

    let mut stdin = child
        .stdin
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdin".to_string()))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| SshError::AgentStartFailed("failed to get stdout".to_string()))?;

    // Send the agent code (exact byte count, same as SSH path)
    stdin.write_all(AGENT_SOURCE.as_bytes()).await?;
    stdin.flush().await?;

    let mut reader = BufReader::new(stdout);

    // Wait for ready message from agent
    let mut ready_line = String::new();
    match reader.read_line(&mut ready_line).await {
        Ok(0) => {
            return Err(docker_eof_error(&mut child, container_id).await);
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
    if version != PROTOCOL_VERSION {
        return Err(SshError::VersionMismatch {
            expected: PROTOCOL_VERSION,
            got: version,
        });
    }

    Ok((reader, stdin, child))
}

/// Build a descriptive error when docker exec closes stdout without a ready message.
async fn docker_eof_error(child: &mut Child, container_id: &str) -> SshError {
    let status = tokio::time::timeout(std::time::Duration::from_secs(5), child.wait()).await;

    let hint = match status {
        Ok(Ok(status)) => match status.code() {
            Some(127) => format!(
                "python3 was not found in container {}. \
                 Ensure Python 3 is installed in the devcontainer",
                container_id
            ),
            Some(code) => format!(
                "docker exec exited with code {} for container {}",
                code, container_id
            ),
            None => format!(
                "docker exec was killed by a signal for container {}",
                container_id
            ),
        },
        Ok(Err(e)) => format!("failed to get docker exec exit status: {}", e),
        Err(_) => {
            if let Err(e) = child.start_kill() {
                tracing::warn!("Failed to kill timed-out docker exec process: {}", e);
            }
            format!(
                "docker exec did not exit in time for container {}",
                container_id
            )
        }
    };

    SshError::AgentStartFailed(hint)
}
