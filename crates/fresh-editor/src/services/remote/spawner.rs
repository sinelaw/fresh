//! Process spawner abstraction
//!
//! Provides a trait for spawning processes that works transparently on both
//! local and remote hosts. Used by the Editor's SpawnProcess handler (for
//! plugins like git_grep) and by FileProvider (for `git ls-files`).

use crate::services::remote::channel::{AgentChannel, ChannelError};
use crate::services::remote::protocol::{decode_base64, exec_params};
use std::sync::Arc;

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

#[cfg(test)]
mod tests {
    use super::*;

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
}
