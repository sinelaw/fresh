//! On-save action execution.
//!
//! This module handles running configured actions when files are saved,
//! such as formatters, linters, and other tools.

use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Duration;

use super::Editor;
use crate::config::{FormatterConfig, OnSaveAction};
use crate::model::event::Event;
use crate::services::lsp::manager::detect_language;
use rust_i18n::t;

/// Result of running a formatter or on-save action
enum ActionResult {
    /// Action ran successfully, contains output
    Success(String),
    /// Command not found
    CommandNotFound(String),
    /// Action failed with error
    Error(String),
}

impl Editor {
    /// Run on-save actions for the active buffer after a successful save.
    /// This includes format-on-save (if enabled) and any on_save actions.
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

        let lang_config = match self.config.languages.get(&language) {
            Some(lc) => lc.clone(),
            None => return Ok(false),
        };

        let mut ran_any_action = false;

        // Run formatter if format_on_save is enabled
        if lang_config.format_on_save {
            if let Some(ref formatter) = lang_config.formatter {
                match self.run_formatter(formatter, &path) {
                    ActionResult::Success(output) => {
                        self.replace_buffer_with_output(&output)?;
                        // Re-save after formatting
                        if let Err(e) = self.active_state_mut().buffer.save() {
                            return Err(format!("Failed to re-save after format: {}", e));
                        }
                        self.active_event_log_mut().mark_saved();
                        ran_any_action = true;
                    }
                    ActionResult::CommandNotFound(cmd) => {
                        self.status_message = Some(format!(
                            "Formatter '{}' not found (install it for auto-formatting)",
                            cmd
                        ));
                    }
                    ActionResult::Error(e) => {
                        return Err(e);
                    }
                }
            }
        }

        // Run on_save actions (linters, etc.)
        let project_root = std::env::current_dir()
            .unwrap_or_else(|_| path.parent().unwrap_or(Path::new(".")).to_path_buf());

        for action in &lang_config.on_save {
            if !action.enabled {
                continue;
            }

            match self.run_on_save_action(action, &path, &project_root) {
                ActionResult::Success(_) => {
                    ran_any_action = true;
                }
                ActionResult::CommandNotFound(_) => {
                    // Skip missing optional commands silently
                }
                ActionResult::Error(e) => {
                    return Err(e);
                }
            }
        }

        Ok(ran_any_action)
    }

    /// Format the current buffer using the configured formatter.
    /// Returns Ok(()) if formatting succeeded, or Err with an error message.
    pub fn format_buffer(&mut self) -> Result<(), String> {
        let path = match self.active_state().buffer.file_path() {
            Some(p) => p.to_path_buf(),
            None => {
                return Err(
                    "Cannot format unsaved buffer (save first to detect language)".to_string(),
                )
            }
        };

        // Detect language for this file
        let language = match detect_language(&path, &self.config.languages) {
            Some(lang) => lang,
            None => return Err("No language detected for this file".to_string()),
        };

        // Get formatter for this language
        let formatter = self
            .config
            .languages
            .get(&language)
            .and_then(|lc| lc.formatter.clone());

        let formatter = match formatter {
            Some(f) => f,
            None => return Err(format!("No formatter configured for {}", language)),
        };

        match self.run_formatter(&formatter, &path) {
            ActionResult::Success(output) => {
                self.replace_buffer_with_output(&output)?;
                self.set_status_message(
                    t!(
                        "format.formatted_with",
                        formatter = formatter.command.clone()
                    )
                    .to_string(),
                );
                Ok(())
            }
            ActionResult::CommandNotFound(cmd) => Err(format!("Formatter '{}' not found", cmd)),
            ActionResult::Error(e) => Err(e),
        }
    }

    /// Run a formatter on the current buffer content.
    fn run_formatter(&mut self, formatter: &FormatterConfig, file_path: &Path) -> ActionResult {
        let file_path_str = file_path.display().to_string();

        // Check if command exists
        if !command_exists(&formatter.command) {
            return ActionResult::CommandNotFound(formatter.command.clone());
        }

        // Build the command
        let shell = detect_shell();

        // Build the full command string with arguments
        let mut cmd_parts = vec![formatter.command.clone()];
        for arg in &formatter.args {
            cmd_parts.push(arg.replace("$FILE", &file_path_str));
        }

        let full_command = cmd_parts.join(" ");

        // Get project root for working directory
        let project_root = std::env::current_dir()
            .unwrap_or_else(|_| file_path.parent().unwrap_or(Path::new(".")).to_path_buf());

        // Set up the command
        let mut cmd = Command::new(&shell);
        cmd.args(["-c", &full_command])
            .current_dir(&project_root)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        if formatter.stdin {
            cmd.stdin(Stdio::piped());
        } else {
            cmd.stdin(Stdio::null());
        }

        // Spawn the process
        let mut child = match cmd.spawn() {
            Ok(c) => c,
            Err(e) => {
                return ActionResult::Error(format!(
                    "Failed to run '{}': {}",
                    formatter.command, e
                ));
            }
        };

        // Write buffer content to stdin if configured
        if formatter.stdin {
            let content = self.active_state().buffer.to_string().unwrap_or_default();
            if let Some(mut stdin) = child.stdin.take() {
                if let Err(e) = stdin.write_all(content.as_bytes()) {
                    return ActionResult::Error(format!("Failed to write to stdin: {}", e));
                }
            }
        }

        // Wait for the process with timeout
        let timeout = Duration::from_millis(formatter.timeout_ms);
        let start = std::time::Instant::now();

        loop {
            match child.try_wait() {
                Ok(Some(status)) => {
                    let output = match child.wait_with_output() {
                        Ok(o) => o,
                        Err(e) => {
                            return ActionResult::Error(format!("Failed to get output: {}", e))
                        }
                    };

                    if status.success() {
                        return match String::from_utf8(output.stdout) {
                            Ok(s) => ActionResult::Success(s),
                            Err(e) => {
                                ActionResult::Error(format!("Invalid UTF-8 in output: {}", e))
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
                        return ActionResult::Error(format!(
                            "Formatter '{}' failed: {}",
                            formatter.command, error_output
                        ));
                    }
                }
                Ok(None) => {
                    if start.elapsed() > timeout {
                        let _ = child.kill();
                        return ActionResult::Error(format!(
                            "Formatter '{}' timed out after {}ms",
                            formatter.command, formatter.timeout_ms
                        ));
                    }
                    std::thread::sleep(Duration::from_millis(10));
                }
                Err(e) => {
                    return ActionResult::Error(format!(
                        "Failed to wait for '{}': {}",
                        formatter.command, e
                    ));
                }
            }
        }
    }

    /// Run a single on-save action (linter, etc.).
    fn run_on_save_action(
        &mut self,
        action: &OnSaveAction,
        file_path: &Path,
        project_root: &Path,
    ) -> ActionResult {
        let file_path_str = file_path.display().to_string();

        // Check if command exists
        if !command_exists(&action.command) {
            return ActionResult::CommandNotFound(action.command.clone());
        }

        // Build the command
        let shell = detect_shell();

        let mut cmd_parts = vec![action.command.clone()];
        for arg in &action.args {
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

        if action.stdin {
            cmd.stdin(Stdio::piped());
        } else {
            cmd.stdin(Stdio::null());
        }

        // Spawn the process
        let mut child = match cmd.spawn() {
            Ok(c) => c,
            Err(e) => {
                return ActionResult::Error(format!("Failed to run '{}': {}", action.command, e));
            }
        };

        // Write buffer content to stdin if configured
        if action.stdin {
            let content = self.active_state().buffer.to_string().unwrap_or_default();
            if let Some(mut stdin) = child.stdin.take() {
                if let Err(e) = stdin.write_all(content.as_bytes()) {
                    return ActionResult::Error(format!("Failed to write to stdin: {}", e));
                }
            }
        }

        // Wait for the process with timeout
        let timeout = Duration::from_millis(action.timeout_ms);
        let start = std::time::Instant::now();

        loop {
            match child.try_wait() {
                Ok(Some(status)) => {
                    let output = match child.wait_with_output() {
                        Ok(o) => o,
                        Err(e) => {
                            return ActionResult::Error(format!("Failed to get output: {}", e))
                        }
                    };

                    if status.success() {
                        return match String::from_utf8(output.stdout) {
                            Ok(s) => ActionResult::Success(s),
                            Err(e) => {
                                ActionResult::Error(format!("Invalid UTF-8 in output: {}", e))
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
                        return ActionResult::Error(format!(
                            "On-save action '{}' failed: {}",
                            action.command, error_output
                        ));
                    }
                }
                Ok(None) => {
                    if start.elapsed() > timeout {
                        let _ = child.kill();
                        return ActionResult::Error(format!(
                            "On-save action '{}' timed out after {}ms",
                            action.command, action.timeout_ms
                        ));
                    }
                    std::thread::sleep(Duration::from_millis(10));
                }
                Err(e) => {
                    return ActionResult::Error(format!(
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
