//! On-save action execution.
//!
//! This module handles running configured actions when files are saved,
//! such as formatters, linters, and other tools.

use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Duration;

use super::Editor;
use crate::config::OnSaveAction;
use crate::model::event::Event;
use crate::services::lsp::manager::detect_language;

/// Result of running a single on-save action
enum OnSaveResult {
    /// Action ran successfully, contains output
    Success(String),
    /// Command not found (for optional actions)
    CommandNotFound(String),
    /// Action failed with error
    Error(String),
}

impl Editor {
    /// Run on-save actions for the active buffer after a successful save.
    /// Returns Ok(true) if actions ran successfully, Ok(false) if no actions,
    /// or Err with an error message.
    pub fn run_on_save_actions(&mut self) -> Result<bool, String> {
        let path = match self.active_state().buffer.file_path() {
            Some(p) => p.to_path_buf(),
            None => return Ok(false),
        };

        // Detect language for this file
        let language = match detect_language(&path, &self.config.languages) {
            Some(lang) => lang,
            None => return Ok(false),
        };

        // Get on_save actions for this language
        let on_save_actions: Vec<OnSaveAction> = self
            .config
            .languages
            .get(&language)
            .map(|lang_config| lang_config.on_save.clone())
            .unwrap_or_default();

        if on_save_actions.is_empty() {
            return Ok(false);
        }

        // Get project root for working directory
        let project_root = std::env::current_dir()
            .unwrap_or_else(|_| path.parent().unwrap_or(Path::new(".")).to_path_buf());

        // Track missing optional commands to show in status
        let mut missing_commands: Vec<String> = Vec::new();
        let mut ran_any_action = false;

        // Run each enabled action in order
        for action in &on_save_actions {
            // Skip disabled actions
            if !action.enabled {
                continue;
            }

            match self.run_single_on_save_action(action, &path, &project_root) {
                OnSaveResult::Success(output) => {
                    ran_any_action = true;
                    if action.replace_buffer {
                        // Replace buffer content with the output
                        self.replace_buffer_with_output(&output)?;
                        // Re-save after replacement
                        if let Err(e) = self.active_state_mut().buffer.save() {
                            return Err(format!("Failed to re-save after format: {}", e));
                        }
                        // Mark event log as saved again
                        self.active_event_log_mut().mark_saved();
                    }
                }
                OnSaveResult::CommandNotFound(cmd) => {
                    // For optional actions, just track the missing command
                    missing_commands.push(cmd);
                }
                OnSaveResult::Error(e) => {
                    // Stop on first failure
                    return Err(e);
                }
            }
        }

        // If some optional commands were missing, show a helpful message
        if !missing_commands.is_empty() {
            let msg = if missing_commands.len() == 1 {
                format!(
                    "Formatter '{}' not found (install it for auto-formatting)",
                    missing_commands[0]
                )
            } else {
                format!(
                    "Formatters not found: {} (install for auto-formatting)",
                    missing_commands.join(", ")
                )
            };
            self.status_message = Some(msg);
        }

        Ok(ran_any_action || !missing_commands.is_empty())
    }

    /// Run a single on-save action.
    fn run_single_on_save_action(
        &mut self,
        action: &OnSaveAction,
        file_path: &Path,
        project_root: &Path,
    ) -> OnSaveResult {
        let file_path_str = file_path.display().to_string();

        // Check if command exists
        if !command_exists(&action.command) {
            if action.optional {
                return OnSaveResult::CommandNotFound(action.command.clone());
            } else {
                return OnSaveResult::Error(format!(
                    "On-save action '{}' failed: command not found",
                    action.command
                ));
            }
        }

        // Build the command
        let shell = detect_shell();

        // Build the full command string with arguments
        let mut cmd_parts = vec![action.command.clone()];
        for arg in &action.args {
            // Replace $FILE placeholder
            cmd_parts.push(arg.replace("$FILE", &file_path_str));
        }

        // If no arguments contain $FILE, append the file path
        let has_file_arg = action.args.iter().any(|a| a.contains("$FILE"));
        if !has_file_arg && !action.stdin {
            cmd_parts.push(file_path_str.clone());
        }

        let full_command = cmd_parts.join(" ");

        // Determine working directory
        let working_dir = action
            .working_dir
            .as_ref()
            .map(|wd| {
                let expanded = wd.replace("$FILE", &file_path_str);
                Path::new(&expanded).to_path_buf()
            })
            .unwrap_or_else(|| project_root.to_path_buf());

        // Set up the command
        let mut cmd = Command::new(&shell);
        cmd.args(["-c", &full_command])
            .current_dir(&working_dir)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        // Set up stdin if needed
        if action.stdin {
            cmd.stdin(Stdio::piped());
        } else {
            cmd.stdin(Stdio::null());
        }

        // Spawn the process
        let mut child = match cmd.spawn() {
            Ok(c) => c,
            Err(e) => {
                // If optional and spawn failed (likely command not found), report it
                if action.optional {
                    return OnSaveResult::CommandNotFound(action.command.clone());
                }
                return OnSaveResult::Error(format!("Failed to run '{}': {}", action.command, e));
            }
        };

        // Write buffer content to stdin if configured
        if action.stdin {
            let content = self.active_state().buffer.to_string().unwrap_or_default();
            if let Some(mut stdin) = child.stdin.take() {
                if let Err(e) = stdin.write_all(content.as_bytes()) {
                    return OnSaveResult::Error(format!("Failed to write to stdin: {}", e));
                }
            }
        }

        // Wait for the process with timeout
        let timeout = Duration::from_millis(action.timeout_ms);
        let start = std::time::Instant::now();

        loop {
            match child.try_wait() {
                Ok(Some(status)) => {
                    // Process finished
                    let output = match child.wait_with_output() {
                        Ok(o) => o,
                        Err(e) => {
                            return OnSaveResult::Error(format!("Failed to get output: {}", e))
                        }
                    };

                    if status.success() {
                        return match String::from_utf8(output.stdout) {
                            Ok(s) => OnSaveResult::Success(s),
                            Err(e) => {
                                OnSaveResult::Error(format!("Invalid UTF-8 in output: {}", e))
                            }
                        };
                    } else {
                        let stderr = String::from_utf8_lossy(&output.stderr);
                        let stdout = String::from_utf8_lossy(&output.stdout);
                        let error_output = if !stderr.is_empty() {
                            stderr.trim().to_string()
                        } else if !stdout.is_empty() {
                            stdout.trim().to_string()
                        } else {
                            format!("exit code {:?}", status.code())
                        };
                        return OnSaveResult::Error(format!(
                            "On-save action '{}' failed: {}",
                            action.command, error_output
                        ));
                    }
                }
                Ok(None) => {
                    // Still running
                    if start.elapsed() > timeout {
                        let _ = child.kill();
                        return OnSaveResult::Error(format!(
                            "On-save action '{}' timed out after {}ms",
                            action.command, action.timeout_ms
                        ));
                    }
                    std::thread::sleep(Duration::from_millis(10));
                }
                Err(e) => {
                    return OnSaveResult::Error(format!(
                        "Failed to wait for '{}': {}",
                        action.command, e
                    ));
                }
            }
        }
    }

    /// Replace the active buffer's content with new output.
    fn replace_buffer_with_output(&mut self, output: &str) -> Result<(), String> {
        let cursor_id = self.active_state().cursors.primary_id();

        // Get current buffer content
        let buffer_content = self.active_state().buffer.to_string().unwrap_or_default();

        // Only replace if content is different
        if buffer_content == output {
            return Ok(());
        }

        let buffer_len = buffer_content.len();

        // Capture cursor position and selection state before replacement
        let old_cursor_pos = self.active_state().cursors.primary().position;
        let old_anchor = self.active_state().cursors.primary().anchor;
        let old_sticky_column = self.active_state().cursors.primary().sticky_column;

        // Delete all content and insert new
        let delete_event = Event::Delete {
            range: 0..buffer_len,
            deleted_text: buffer_content,
            cursor_id,
        };
        let insert_event = Event::Insert {
            position: 0,
            text: output.to_string(),
            cursor_id,
        };

        // After delete+insert, cursor will be at output.len()
        // Restore cursor to original position (or clamp to new buffer length)
        let new_buffer_len = output.len();
        let new_cursor_pos = old_cursor_pos.min(new_buffer_len);

        // Only add MoveCursor event if position actually changes
        let mut events = vec![delete_event, insert_event];
        if new_cursor_pos != new_buffer_len {
            let move_cursor_event = Event::MoveCursor {
                cursor_id,
                old_position: new_buffer_len, // Where cursor is after insert
                new_position: new_cursor_pos,
                old_anchor: None,
                new_anchor: old_anchor.map(|a| a.min(new_buffer_len)),
                old_sticky_column: 0,
                new_sticky_column: old_sticky_column,
            };
            events.push(move_cursor_event);
        }

        // Apply as a batch for atomic undo
        let batch = Event::Batch {
            events,
            description: "On-save format".to_string(),
        };
        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);

        Ok(())
    }
}

/// Check if a command exists in the system PATH.
fn command_exists(command: &str) -> bool {
    // Use 'which' on Unix or 'where' on Windows to check if command exists
    #[cfg(unix)]
    {
        Command::new("which")
            .arg(command)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    }

    #[cfg(windows)]
    {
        Command::new("where")
            .arg(command)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    }

    #[cfg(not(any(unix, windows)))]
    {
        // On other platforms, assume command exists and let it fail at runtime
        true
    }
}

/// Detect the shell to use for executing commands.
fn detect_shell() -> String {
    // Try SHELL environment variable first
    if let Ok(shell) = std::env::var("SHELL") {
        if !shell.is_empty() {
            return shell;
        }
    }

    // Fall back to common shells
    #[cfg(unix)]
    {
        if std::path::Path::new("/bin/bash").exists() {
            return "/bin/bash".to_string();
        }
        if std::path::Path::new("/bin/sh").exists() {
            return "/bin/sh".to_string();
        }
    }

    #[cfg(windows)]
    {
        if let Ok(comspec) = std::env::var("COMSPEC") {
            return comspec;
        }
        return "cmd.exe".to_string();
    }

    // Last resort
    "sh".to_string()
}
