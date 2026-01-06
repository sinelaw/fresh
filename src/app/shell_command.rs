//! Shell command execution on buffer/region content.
//!
//! This module provides functionality to:
//! - Run shell commands with buffer or selection content as stdin
//! - Output results to a new buffer or replace the input content

use std::io::Write;
use std::process::{Command, Stdio};

use super::Editor;
use crate::model::event::Event;
use crate::view::prompt::PromptType;
use rust_i18n::t;

impl Editor {
    /// Start a shell command prompt.
    /// If `replace` is true, the output will replace the buffer/selection.
    /// If `replace` is false, the output goes to a new buffer.
    pub fn start_shell_command_prompt(&mut self, replace: bool) {
        let prompt_msg = if replace {
            t!("shell.command_replace_prompt").to_string()
        } else {
            t!("shell.command_prompt").to_string()
        };
        self.start_prompt(prompt_msg, PromptType::ShellCommand { replace });
    }

    /// Execute a shell command with the current buffer/selection as stdin.
    /// Returns Ok(output) on success, Err(error_message) on failure.
    pub fn execute_shell_command(&mut self, command: &str) -> Result<String, String> {
        // Get the input text (selection or entire buffer)
        let input = self.get_shell_input();

        // Detect the shell to use
        let shell = detect_shell();

        // Execute the command
        let mut child = Command::new(&shell)
            .args(["-c", command])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("Failed to spawn shell: {}", e))?;

        // Write input to stdin
        if let Some(mut stdin) = child.stdin.take() {
            stdin
                .write_all(input.as_bytes())
                .map_err(|e| format!("Failed to write to stdin: {}", e))?;
        }

        // Wait for the command to complete
        let output = child
            .wait_with_output()
            .map_err(|e| format!("Failed to wait for command: {}", e))?;

        if output.status.success() {
            String::from_utf8(output.stdout).map_err(|e| format!("Invalid UTF-8 in output: {}", e))
        } else {
            // Include stderr in error message
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            if !stderr.is_empty() {
                Err(format!("Command failed: {}", stderr.trim()))
            } else if !stdout.is_empty() {
                // Some commands output errors to stdout
                Err(format!("Command failed: {}", stdout.trim()))
            } else {
                Err(format!(
                    "Command failed with exit code: {:?}",
                    output.status.code()
                ))
            }
        }
    }

    /// Get the input for shell command (selection or entire buffer).
    fn get_shell_input(&mut self) -> String {
        // First get selection range
        let selection_range = {
            let state = self.active_state();
            state.cursors.primary().selection_range()
        };

        // Check if there's a selection
        if let Some(selection) = selection_range {
            let start = selection.start.min(selection.end);
            let end = selection.start.max(selection.end);
            self.active_state_mut().get_text_range(start, end)
        } else {
            // Use entire buffer
            self.active_state().buffer.to_string().unwrap_or_default()
        }
    }

    /// Handle shell command execution after prompt confirmation.
    /// If `replace` is true, replaces the selection/buffer with output.
    /// If `replace` is false, creates a new buffer with the output.
    pub fn handle_shell_command(&mut self, command: &str, replace: bool) {
        // Capture selection range first
        let selection_range = {
            let state = self.active_state();
            let primary = state.cursors.primary();
            primary.selection_range().map(|sel| {
                let start = sel.start.min(sel.end);
                let end = sel.start.max(sel.end);
                (start, end)
            })
        };

        // Now get the deleted text if there's a selection
        let selection_info = if let Some((start, end)) = selection_range {
            let deleted_text = self.active_state_mut().get_text_range(start, end);
            Some((start, end, deleted_text))
        } else {
            None
        };
        let has_selection = selection_info.is_some();

        match self.execute_shell_command(command) {
            Ok(output) => {
                if replace {
                    self.replace_with_shell_output(&output, has_selection, selection_info);
                } else {
                    self.create_shell_output_buffer(command, &output);
                }
            }
            Err(err) => {
                self.set_status_message(err);
            }
        }
    }

    /// Replace the current selection or buffer with shell output.
    fn replace_with_shell_output(
        &mut self,
        output: &str,
        has_selection: bool,
        selection_info: Option<(usize, usize, String)>,
    ) {
        let cursor_id = self.active_state().cursors.primary_id();

        // Capture cursor position and selection state before replacement
        let old_cursor_pos = self.active_state().cursors.primary().position;
        let old_anchor = self.active_state().cursors.primary().anchor;
        let old_sticky_column = self.active_state().cursors.primary().sticky_column;

        if has_selection {
            // Replace selection with output
            if let Some((start, end, deleted_text)) = selection_info {
                // Create delete and insert events
                let delete_event = Event::Delete {
                    range: start..end,
                    deleted_text,
                    cursor_id,
                };
                let insert_event = Event::Insert {
                    position: start,
                    text: output.to_string(),
                    cursor_id,
                };

                // After insert, cursor will be at start + output.len()
                // For selection replacement, keep cursor at end of insertion (default behavior)
                // Apply as a batch for atomic undo
                let batch = Event::Batch {
                    events: vec![delete_event, insert_event],
                    description: "Shell command replace".to_string(),
                };
                self.active_event_log_mut().append(batch.clone());
                self.apply_event_to_active_buffer(&batch);
            }
        } else {
            // Replace entire buffer
            let buffer_content = self.active_state().buffer.to_string().unwrap_or_default();
            let buffer_len = buffer_content.len();

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
                description: "Shell command replace buffer".to_string(),
            };
            self.active_event_log_mut().append(batch.clone());
            self.apply_event_to_active_buffer(&batch);
        }

        self.set_status_message(t!("status.shell_command_completed").to_string());
    }

    /// Create a new buffer with the shell command output.
    fn create_shell_output_buffer(&mut self, command: &str, output: &str) {
        // Create a new buffer for the output
        let buffer_name = format!("*Shell: {}*", truncate_command(command, 30));
        let buffer_id = self.new_buffer();

        // Switch to the new buffer first
        self.switch_buffer(buffer_id);

        // Insert the output content
        let cursor_id = self.active_state().cursors.primary_id();
        let insert_event = Event::Insert {
            position: 0,
            text: output.to_string(),
            cursor_id,
        };
        self.apply_event_to_active_buffer(&insert_event);

        // Update metadata with a virtual name
        if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
            metadata.display_name = buffer_name.clone();
        }

        self.set_status_message(t!("shell.output_in", buffer = buffer_name).to_string());
    }

    /// Execute a shell command blocking the UI.
    /// This is used for commands like `sudo` where we might need to wait for completion.
    pub(crate) fn run_shell_command_blocking(&mut self, command: &str) -> anyhow::Result<()> {
        use crossterm::terminal::{
            disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
        };
        use crossterm::ExecutableCommand;
        use std::io::stdout;

        // Suspend TUI
        let _ = disable_raw_mode();
        let _ = stdout().execute(LeaveAlternateScreen);

        let shell = detect_shell();
        let mut child = Command::new(&shell)
            .args(["-c", command])
            .spawn()
            .map_err(|e| anyhow::anyhow!("Failed to spawn shell: {}", e))?;

        let status = child
            .wait()
            .map_err(|e| anyhow::anyhow!("Failed to wait for command: {}", e))?;

        // Resume TUI
        let _ = stdout().execute(EnterAlternateScreen);
        let _ = enable_raw_mode();

        // Request a full hard redraw to clear any ghost text from the external command
        self.request_full_redraw();

        if status.success() {
            Ok(())
        } else {
            anyhow::bail!("Command failed with exit code: {:?}", status.code())
        }
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

/// Truncate a command string for display purposes.
fn truncate_command(command: &str, max_len: usize) -> String {
    let trimmed = command.trim();
    if trimmed.len() <= max_len {
        trimmed.to_string()
    } else {
        format!("{}...", &trimmed[..max_len - 3])
    }
}
