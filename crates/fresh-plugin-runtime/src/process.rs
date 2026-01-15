//! Plugin Process Spawning: Async process execution for plugins
//!
//! This module enables plugins to spawn external processes asynchronously,
//! capturing stdout/stderr and notifying via callbacks when complete.

use fresh_core::api::PluginAsyncMessage as AsyncMessage;
use std::process::Stdio;
use std::sync::mpsc;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;

/// Spawn an external process for a plugin
///
/// This function:
/// 1. Spawns the process asynchronously
/// 2. Captures all stdout and stderr
/// 3. Waits for process completion
/// 4. Sends results back via AsyncBridge with process_id for callback matching
///
/// # Arguments
/// * `process_id` - Unique ID to match with callback
/// * `command` - Command to execute (e.g., "git")
/// * `args` - Command arguments
/// * `cwd` - Optional working directory
/// * `sender` - Channel to send results back to main loop
pub async fn spawn_plugin_process(
    process_id: u64,
    command: String,
    args: Vec<String>,
    cwd: Option<String>,
    sender: mpsc::Sender<AsyncMessage>,
) {
    // Build the command
    let mut cmd = Command::new(&command);
    cmd.args(&args);
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    // Set working directory if provided
    if let Some(ref dir) = cwd {
        cmd.current_dir(dir);
    }

    // Spawn the process
    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(e) => {
            // Failed to spawn - send error result
            let _ = sender.send(AsyncMessage::ProcessOutput {
                process_id,
                stdout: String::new(),
                stderr: format!("Failed to spawn process: {}", e),
                exit_code: -1,
            });
            return;
        }
    };

    // Capture stdout and stderr
    let stdout_handle = child.stdout.take();
    let stderr_handle = child.stderr.take();

    // Read stdout
    let stdout_future = async {
        if let Some(stdout) = stdout_handle {
            let reader = BufReader::new(stdout);
            let mut lines = reader.lines();
            let mut output = String::new();

            while let Ok(Some(line)) = lines.next_line().await {
                output.push_str(&line);
                output.push('\n');
            }
            output
        } else {
            String::new()
        }
    };

    // Read stderr
    let stderr_future = async {
        if let Some(stderr) = stderr_handle {
            let reader = BufReader::new(stderr);
            let mut lines = reader.lines();
            let mut output = String::new();

            while let Ok(Some(line)) = lines.next_line().await {
                output.push_str(&line);
                output.push('\n');
            }
            output
        } else {
            String::new()
        }
    };

    // Wait for both outputs concurrently
    let (stdout, stderr) = tokio::join!(stdout_future, stderr_future);

    // Wait for process to complete
    let exit_code = match child.wait().await {
        Ok(status) => status.code().unwrap_or(-1),
        Err(_) => -1,
    };

    // Send results back to main loop
    let _ = sender.send(AsyncMessage::ProcessOutput {
        process_id,
        stdout,
        stderr,
        exit_code,
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_spawn_simple_command() {
        let (sender, receiver) = mpsc::channel();

        spawn_plugin_process(
            1,
            "echo".to_string(),
            vec!["hello".to_string()],
            None,
            sender,
        )
        .await;

        let msg = receiver.recv().unwrap();
        match msg {
            AsyncMessage::ProcessOutput {
                process_id,
                stdout,
                stderr,
                exit_code,
            } => {
                assert_eq!(process_id, 1);
                assert!(stdout.contains("hello"));
                assert_eq!(stderr, "");
                assert_eq!(exit_code, 0);
            }
            _ => panic!("Expected PluginProcessOutput"),
        }
    }

    #[tokio::test]
    async fn test_spawn_with_args() {
        let (sender, receiver) = mpsc::channel();

        spawn_plugin_process(
            2,
            "printf".to_string(),
            vec![
                "%s %s".to_string(),
                "hello".to_string(),
                "world".to_string(),
            ],
            None,
            sender,
        )
        .await;

        let msg = receiver.recv().unwrap();
        match msg {
            AsyncMessage::ProcessOutput {
                process_id,
                stdout,
                exit_code,
                ..
            } => {
                assert_eq!(process_id, 2);
                assert!(stdout.contains("hello world"));
                assert_eq!(exit_code, 0);
            }
            _ => panic!("Expected PluginProcessOutput"),
        }
    }

    #[tokio::test]
    async fn test_spawn_nonexistent_command() {
        let (sender, receiver) = mpsc::channel();

        spawn_plugin_process(
            3,
            "this_command_does_not_exist_12345".to_string(),
            vec![],
            None,
            sender,
        )
        .await;

        let msg = receiver.recv().unwrap();
        match msg {
            AsyncMessage::ProcessOutput {
                process_id,
                stdout,
                stderr,
                exit_code,
            } => {
                assert_eq!(process_id, 3);
                assert_eq!(stdout, "");
                assert!(stderr.contains("Failed to spawn"));
                assert_eq!(exit_code, -1);
            }
            _ => panic!("Expected PluginProcessOutput"),
        }
    }

    #[tokio::test]
    async fn test_spawn_failing_command() {
        let (sender, receiver) = mpsc::channel();

        // Use a command that will fail
        spawn_plugin_process(
            4,
            "sh".to_string(),
            vec!["-c".to_string(), "exit 42".to_string()],
            None,
            sender,
        )
        .await;

        let msg = receiver.recv().unwrap();
        match msg {
            AsyncMessage::ProcessOutput {
                process_id,
                exit_code,
                ..
            } => {
                assert_eq!(process_id, 4);
                assert_eq!(exit_code, 42);
            }
            _ => panic!("Expected PluginProcessOutput"),
        }
    }
}
