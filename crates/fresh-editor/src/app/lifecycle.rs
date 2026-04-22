//! Editor-lifecycle methods: quit, restart, session/detach control,
//! focus/resize hooks, theme/settings queries, escape-sequence + clipboard
//! piping, and the should_quit confirmation flow that walks modified buffers.

use super::*;

impl Editor {
    /// Check if the editor should quit
    pub fn should_quit(&self) -> bool {
        self.should_quit
    }

    /// Check if the client should detach (keep server running)
    pub fn should_detach(&self) -> bool {
        self.should_detach
    }

    /// Clear the detach flag (after processing)
    pub fn clear_detach(&mut self) {
        self.should_detach = false;
    }

    /// Set session mode (use hardware cursor only, no REVERSED style for software cursor)
    pub fn set_session_mode(&mut self, session_mode: bool) {
        self.session_mode = session_mode;
        self.clipboard.set_session_mode(session_mode);
        // Also set custom context for command palette filtering
        if session_mode {
            self.active_custom_contexts
                .insert(crate::types::context_keys::SESSION_MODE.to_string());
        } else {
            self.active_custom_contexts
                .remove(crate::types::context_keys::SESSION_MODE);
        }
    }

    /// Check if running in session mode
    pub fn is_session_mode(&self) -> bool {
        self.session_mode
    }

    /// Mark that the backend does not render a hardware cursor.
    /// When set, the renderer always draws a software cursor indicator.
    pub fn set_software_cursor_only(&mut self, enabled: bool) {
        self.software_cursor_only = enabled;
    }

    /// Set the session name for display in status bar.
    ///
    /// When a session name is set, the recovery service is reinitialized
    /// to use a session-scoped recovery directory so each named session's
    /// recovery data is isolated.
    pub fn set_session_name(&mut self, name: Option<String>) {
        if let Some(ref session_name) = name {
            let base_recovery_dir = self.dir_context.recovery_dir();
            let scope = crate::services::recovery::RecoveryScope::Session {
                name: session_name.clone(),
            };
            let recovery_config = RecoveryConfig {
                enabled: self.recovery_service.is_enabled(),
                ..RecoveryConfig::default()
            };
            self.recovery_service =
                RecoveryService::with_scope(recovery_config, &base_recovery_dir, &scope);
        }
        self.session_name = name;
    }

    /// Get the session name (for status bar display)
    pub fn session_name(&self) -> Option<&str> {
        self.session_name.as_deref()
    }

    /// Queue escape sequences to be sent to the client (session mode only)
    pub fn queue_escape_sequences(&mut self, sequences: &[u8]) {
        self.pending_escape_sequences.extend_from_slice(sequences);
    }

    /// Take pending escape sequences, clearing the queue
    pub fn take_pending_escape_sequences(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.pending_escape_sequences)
    }

    /// Take pending clipboard data queued in session mode, clearing the request
    pub fn take_pending_clipboard(
        &mut self,
    ) -> Option<crate::services::clipboard::PendingClipboard> {
        self.clipboard.take_pending_clipboard()
    }

    /// Check if the editor should restart with a new working directory
    pub fn should_restart(&self) -> bool {
        self.restart_with_dir.is_some()
    }

    /// Take the restart directory, clearing the restart request
    /// Returns the new working directory if a restart was requested
    pub fn take_restart_dir(&mut self) -> Option<PathBuf> {
        self.restart_with_dir.take()
    }

    /// Request the editor to restart with a new working directory
    /// This triggers a clean shutdown and restart with the new project root
    /// Request a full hardware terminal clear and redraw on the next frame.
    /// Used after external commands have messed up the terminal state.
    pub fn request_full_redraw(&mut self) {
        self.full_redraw_requested = true;
    }

    /// Check if a full redraw was requested, and clear the flag.
    pub fn take_full_redraw_request(&mut self) -> bool {
        let requested = self.full_redraw_requested;
        self.full_redraw_requested = false;
        requested
    }

    /// Request the event loop to suspend the editor process (SIGTSTP on Unix).
    /// The loop tears down terminal modes, raises the signal, then re-enables
    /// modes once the shell sends SIGCONT (e.g. via `fg`).
    pub fn request_suspend(&mut self) {
        self.suspend_requested = true;
    }

    /// Check if a suspend was requested, and clear the flag.
    pub fn take_suspend_request(&mut self) -> bool {
        let requested = self.suspend_requested;
        self.suspend_requested = false;
        requested
    }

    pub fn request_restart(&mut self, new_working_dir: PathBuf) {
        tracing::info!(
            "Restart requested with new working directory: {}",
            new_working_dir.display()
        );
        self.restart_with_dir = Some(new_working_dir);
        // Also signal quit so the event loop exits
        self.should_quit = true;
    }

    /// Get the active theme
    pub fn theme(&self) -> &crate::view::theme::Theme {
        &self.theme
    }

    /// Check if the settings dialog is open and visible
    pub fn is_settings_open(&self) -> bool {
        self.settings_state.as_ref().is_some_and(|s| s.visible)
    }

    /// Request the editor to quit
    pub fn quit(&mut self) {
        // Check for unsaved buffers (all are auto-persisted when hot_exit is enabled)
        let modified_count = self.count_modified_buffers_needing_prompt();
        if modified_count > 0 {
            let save_key = t!("prompt.key.save").to_string();
            let cancel_key = t!("prompt.key.cancel").to_string();
            let hot_exit = self.config.editor.hot_exit;

            let msg = if hot_exit {
                // With hot exit: offer save, quit-without-saving (recoverable), or cancel
                let quit_key = t!("prompt.key.quit").to_string();
                if modified_count == 1 {
                    t!(
                        "prompt.quit_modified_hot_one",
                        save_key = save_key,
                        quit_key = quit_key,
                        cancel_key = cancel_key
                    )
                    .to_string()
                } else {
                    t!(
                        "prompt.quit_modified_hot_many",
                        count = modified_count,
                        save_key = save_key,
                        quit_key = quit_key,
                        cancel_key = cancel_key
                    )
                    .to_string()
                }
            } else {
                // Without hot exit: offer save, discard, or cancel
                let discard_key = t!("prompt.key.discard").to_string();
                if modified_count == 1 {
                    t!(
                        "prompt.quit_modified_one",
                        save_key = save_key,
                        discard_key = discard_key,
                        cancel_key = cancel_key
                    )
                    .to_string()
                } else {
                    t!(
                        "prompt.quit_modified_many",
                        count = modified_count,
                        save_key = save_key,
                        discard_key = discard_key,
                        cancel_key = cancel_key
                    )
                    .to_string()
                }
            };
            self.start_prompt(msg, PromptType::ConfirmQuitWithModified);
        } else {
            self.should_quit = true;
        }
    }

    /// Count modified buffers that would require a save prompt on quit.
    ///
    /// When `hot_exit` is enabled, unnamed buffers are excluded (they are
    /// automatically recovered across sessions), but file-backed modified
    /// buffers still trigger a prompt with a "recoverable" option.
    /// When `auto_save_enabled` is true, file-backed buffers are excluded
    /// (they will be saved to disk on exit).
    fn count_modified_buffers_needing_prompt(&self) -> usize {
        let hot_exit = self.config.editor.hot_exit;
        let auto_save = self.config.editor.auto_save_enabled;

        self.buffers
            .iter()
            .filter(|(buffer_id, state)| {
                if !state.buffer.is_modified() {
                    return false;
                }
                if let Some(meta) = self.buffer_metadata.get(buffer_id) {
                    if let Some(path) = meta.file_path() {
                        let is_unnamed = path.as_os_str().is_empty();
                        if is_unnamed && hot_exit {
                            return false; // unnamed buffer, auto-recovered via hot exit
                        }
                        if !is_unnamed && auto_save {
                            return false; // file-backed, will be auto-saved on exit
                        }
                    }
                }
                true
            })
            .count()
    }

    /// Handle terminal focus gained event
    pub fn focus_gained(&mut self) {
        self.plugin_manager.run_hook(
            "focus_gained",
            crate::services::plugins::hooks::HookArgs::FocusGained,
        );
    }

    /// Resize all buffers to match new terminal size
    pub fn resize(&mut self, width: u16, height: u16) {
        // Update terminal dimensions for future buffer creation
        self.terminal_width = width;
        self.terminal_height = height;

        // Resize all SplitViewState viewports (viewport is now owned by SplitViewState)
        for view_state in self.split_view_states.values_mut() {
            view_state.viewport.resize(width, height);
        }

        // Resize visible terminal PTYs to match new dimensions
        self.resize_visible_terminals();

        // Notify plugins of the resize so they can adjust layouts
        self.plugin_manager.run_hook(
            "resize",
            fresh_core::hooks::HookArgs::Resize { width, height },
        );
    }
}
