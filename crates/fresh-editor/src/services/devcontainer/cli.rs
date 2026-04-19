//! Devcontainer CLI wrapper
//!
//! Wraps the `devcontainer` CLI (`@devcontainers/cli`) for container
//! lifecycle management: building, starting, stopping, and querying containers.

use std::path::Path;
use std::process::Stdio;
use tokio::process::Command;

/// Error type for devcontainer CLI operations
#[derive(Debug, thiserror::Error)]
pub enum DevcontainerError {
    #[error("Failed to spawn devcontainer CLI ({0}). Is `devcontainer` installed and in your PATH? Install with: npm install -g @devcontainers/cli")]
    SpawnFailed(#[from] std::io::Error),

    #[error("devcontainer CLI failed (exit code {code}): {stderr}")]
    CliFailed { code: i32, stderr: String },

    #[error("Failed to parse devcontainer CLI output: {0}")]
    ParseError(String),

    #[error("Container not running for workspace: {0}")]
    ContainerNotRunning(String),

    #[error("Docker is not available: {0}")]
    DockerNotAvailable(String),
}

/// Result of `devcontainer up`
#[derive(Debug, Clone)]
pub struct UpResult {
    /// The Docker container ID
    pub container_id: String,
    /// Workspace folder inside the container
    pub remote_workspace_folder: String,
    /// Remote user inside the container
    pub remote_user: Option<String>,
}

/// Wrapper around the `devcontainer` CLI
pub struct DevcontainerCli {
    /// Path to the devcontainer CLI executable
    cli_path: String,
}

impl DevcontainerCli {
    /// Create a new CLI wrapper with default binary name
    pub fn new() -> Self {
        Self {
            cli_path: "devcontainer".to_string(),
        }
    }

    /// Create a new CLI wrapper with a custom binary path
    pub fn with_path(cli_path: String) -> Self {
        Self { cli_path }
    }

    /// Check if the devcontainer CLI is available
    pub async fn is_available(&self) -> bool {
        Command::new(&self.cli_path)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .await
            .map(|s| s.success())
            .unwrap_or(false)
    }

    /// Check if Docker is available and running
    pub async fn check_docker(&self) -> Result<(), DevcontainerError> {
        let output = Command::new("docker")
            .args(["version", "--format", "{{.Server.Version}}"])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .map_err(|e| DevcontainerError::DockerNotAvailable(e.to_string()))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(DevcontainerError::DockerNotAvailable(stderr.to_string()));
        }

        Ok(())
    }

    /// Build and start the devcontainer.
    ///
    /// Runs `devcontainer up --workspace-folder <path>` and parses the JSON
    /// output to extract the container ID and workspace folder.
    pub async fn up(&self, workspace_folder: &Path) -> Result<UpResult, DevcontainerError> {
        let output = Command::new(&self.cli_path)
            .args([
                "up",
                "--workspace-folder",
                &workspace_folder.to_string_lossy(),
            ])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(DevcontainerError::CliFailed {
                code: output.status.code().unwrap_or(-1),
                stderr: stderr.to_string(),
            });
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        self.parse_up_output(&stdout)
    }

    /// Parse the JSON output from `devcontainer up`.
    ///
    /// The CLI outputs JSON like:
    /// ```json
    /// {"outcome":"success","containerId":"abc123...","remoteUser":"vscode","remoteWorkspaceFolder":"/workspaces/project"}
    /// ```
    fn parse_up_output(&self, output: &str) -> Result<UpResult, DevcontainerError> {
        // devcontainer CLI may output multiple lines; the last JSON line is the result
        let json_line = output
            .lines()
            .rev()
            .find(|line| line.trim_start().starts_with('{'))
            .ok_or_else(|| {
                DevcontainerError::ParseError(format!(
                    "No JSON output from devcontainer up: {}",
                    output
                ))
            })?;

        let value: serde_json::Value = serde_json::from_str(json_line)
            .map_err(|e| DevcontainerError::ParseError(format!("{}: {}", e, json_line)))?;

        let outcome = value
            .get("outcome")
            .and_then(|v| v.as_str())
            .unwrap_or("");

        if outcome != "success" {
            return Err(DevcontainerError::ParseError(format!(
                "devcontainer up did not succeed: outcome={}",
                outcome
            )));
        }

        let container_id = value
            .get("containerId")
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                DevcontainerError::ParseError("missing containerId in output".to_string())
            })?
            .to_string();

        let remote_workspace_folder = value
            .get("remoteWorkspaceFolder")
            .and_then(|v| v.as_str())
            .unwrap_or("/workspaces")
            .to_string();

        let remote_user = value
            .get("remoteUser")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());

        Ok(UpResult {
            container_id,
            remote_workspace_folder,
            remote_user,
        })
    }

}

impl Default for DevcontainerCli {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_up_output_success() {
        let cli = DevcontainerCli::new();
        let output = r#"{"outcome":"success","containerId":"abc123def456","remoteUser":"vscode","remoteWorkspaceFolder":"/workspaces/myproject"}"#;

        let result = cli.parse_up_output(output).unwrap();
        assert_eq!(result.container_id, "abc123def456");
        assert_eq!(result.remote_workspace_folder, "/workspaces/myproject");
        assert_eq!(result.remote_user.as_deref(), Some("vscode"));
    }

    #[test]
    fn test_parse_up_output_multiline() {
        let cli = DevcontainerCli::new();
        let output = "some log line\nanother log\n{\"outcome\":\"success\",\"containerId\":\"abc123\",\"remoteWorkspaceFolder\":\"/workspaces/proj\"}\n";

        let result = cli.parse_up_output(output).unwrap();
        assert_eq!(result.container_id, "abc123");
    }

    #[test]
    fn test_parse_up_output_no_remote_user() {
        let cli = DevcontainerCli::new();
        let output =
            r#"{"outcome":"success","containerId":"abc123","remoteWorkspaceFolder":"/workspaces/proj"}"#;

        let result = cli.parse_up_output(output).unwrap();
        assert!(result.remote_user.is_none());
    }

    #[test]
    fn test_parse_up_output_failure() {
        let cli = DevcontainerCli::new();
        let output = r#"{"outcome":"error","message":"build failed"}"#;

        assert!(cli.parse_up_output(output).is_err());
    }

    #[test]
    fn test_parse_up_output_no_json() {
        let cli = DevcontainerCli::new();
        let output = "just some log output\nno json here\n";

        assert!(cli.parse_up_output(output).is_err());
    }
}
