//! Terminal integration for the Editor
//!
//! This module provides methods for the Editor to interact with the terminal system:
//! - Opening new terminal sessions
//! - Closing terminals
//! - Rendering terminal content
//! - Handling terminal input
//!
//! # Role in Incremental Streaming Architecture
//!
//! This module handles mode switching between terminal and scrollback modes.
//! See `crate::services::terminal` for the full architecture diagram.
//!
//! ## Mode Switching Methods
//!
//! - [`Editor::sync_terminal_to_buffer`]: Terminal → Scrollback mode
//!   - Appends visible screen (~50 lines) to backing file
//!   - Loads backing file as read-only buffer
//!   - Performance: O(screen_size) ≈ 5ms
//!
//! - [`Editor::enter_terminal_mode`]: Scrollback → Terminal mode
//!   - Truncates backing file to remove visible screen tail
//!   - Resumes live terminal rendering
//!   - Performance: O(1) ≈ 1ms

use super::{BufferId, BufferMetadata, Editor};
use crate::services::terminal::TerminalId;
use crate::state::EditorState;
use rust_i18n::t;

impl Editor {
    /// Open a new terminal in the current split
    pub fn open_terminal(&mut self) {
        // Get the current split dimensions for the terminal size
        let (cols, rows) = self.get_terminal_dimensions();

        // Set up async bridge for terminal manager if not already done
        if let Some(ref bridge) = self.async_bridge {
            self.terminal_manager.set_async_bridge(bridge.clone());
        }

        // Prepare persistent storage paths under the user's data directory
        let terminal_root = self.dir_context.terminal_dir_for(&self.working_dir);
        let _ = self.filesystem.create_dir_all(&terminal_root);
        // Precompute paths using the next terminal ID so we capture from the first byte
        let predicted_terminal_id = self.terminal_manager.next_terminal_id();
        let log_path =
            terminal_root.join(format!("fresh-terminal-{}.log", predicted_terminal_id.0));
        let backing_path =
            terminal_root.join(format!("fresh-terminal-{}.txt", predicted_terminal_id.0));
        // Stash backing path now so buffer creation can reuse it
        self.terminal_backing_files
            .insert(predicted_terminal_id, backing_path);

        // Spawn terminal with incremental scrollback streaming
        let backing_path_for_spawn = self
            .terminal_backing_files
            .get(&predicted_terminal_id)
            .cloned();
        match self.terminal_manager.spawn(
            cols,
            rows,
            Some(self.working_dir.clone()),
            Some(log_path.clone()),
            backing_path_for_spawn,
        ) {
            Ok(terminal_id) => {
                // Track log file path (use actual ID in case it differs)
                let actual_log_path = log_path.clone();
                self.terminal_log_files
                    .insert(terminal_id, actual_log_path.clone());
                // If predicted differs, move backing path entry
                if terminal_id != predicted_terminal_id {
                    self.terminal_backing_files.remove(&predicted_terminal_id);
                    let backing_path =
                        terminal_root.join(format!("fresh-terminal-{}.txt", terminal_id.0));
                    self.terminal_backing_files
                        .insert(terminal_id, backing_path);
                }

                // Create a buffer for this terminal
                let buffer_id = self.create_terminal_buffer_attached(
                    terminal_id,
                    self.split_manager.active_split(),
                );

                // Switch to the terminal buffer
                self.set_active_buffer(buffer_id);

                // Enable terminal mode
                self.terminal_mode = true;
                self.key_context = crate::input::keybindings::KeyContext::Terminal;

                // Resize terminal to match actual split content area
                self.resize_visible_terminals();

                // Get the terminal escape keybinding dynamically
                let exit_key = self
                    .keybindings
                    .find_keybinding_for_action(
                        "terminal_escape",
                        crate::input::keybindings::KeyContext::Terminal,
                    )
                    .unwrap_or_else(|| "Ctrl+Space".to_string());
                self.set_status_message(
                    t!("terminal.opened", id = terminal_id.0, exit_key = exit_key).to_string(),
                );
                tracing::info!(
                    "Opened terminal {:?} with buffer {:?}",
                    terminal_id,
                    buffer_id
                );
            }
            Err(e) => {
                self.set_status_message(
                    t!("terminal.failed_to_open", error = e.to_string()).to_string(),
                );
                tracing::error!("Failed to open terminal: {}", e);
            }
        }
    }

    /// Create a buffer for a terminal session
    pub(crate) fn create_terminal_buffer_attached(
        &mut self,
        terminal_id: TerminalId,
        split_id: crate::model::event::SplitId,
    ) -> BufferId {
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        // Get config values
        let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;

        // Rendered backing file for scrollback view (reuse if already recorded)
        let backing_file = self
            .terminal_backing_files
            .get(&terminal_id)
            .cloned()
            .unwrap_or_else(|| {
                let root = self.dir_context.terminal_dir_for(&self.working_dir);
                let _ = self.filesystem.create_dir_all(&root);
                root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
            });

        // Ensure the file exists - but DON'T truncate if it already has content
        // The PTY read loop may have already started writing scrollback
        if !self.filesystem.exists(&backing_file) {
            if let Err(e) = self.filesystem.write_file(&backing_file, &[]) {
                tracing::warn!("Failed to create terminal backing file: {}", e);
            }
        }

        // Store the backing file path
        self.terminal_backing_files
            .insert(terminal_id, backing_file.clone());

        // Create editor state with the backing file
        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            large_file_threshold,
            std::sync::Arc::clone(&self.filesystem),
        );
        state.buffer.set_file_path(backing_file.clone());
        // Terminal buffers should never show line numbers
        state.margins.configure_for_line_numbers(false);
        self.buffers.insert(buffer_id, state);

        // Use virtual metadata so the tab shows "*Terminal N*" and LSP stays off.
        // The backing file is still tracked separately for syncing scrollback.
        let metadata = BufferMetadata::virtual_buffer(
            format!("*Terminal {}*", terminal_id.0),
            "terminal".into(),
            false,
        );
        self.buffer_metadata.insert(buffer_id, metadata);

        // Map buffer to terminal
        self.terminal_buffers.insert(buffer_id, terminal_id);

        // Initialize event log for undo/redo
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Set up split view state
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            view_state.open_buffers.push(buffer_id);
            // Terminal buffers should not wrap lines so escape sequences stay intact
            view_state.viewport.line_wrap_enabled = false;
        }

        buffer_id
    }

    /// Create a terminal buffer without attaching it to any split (used during session restore).
    pub(crate) fn create_terminal_buffer_detached(&mut self, terminal_id: TerminalId) -> BufferId {
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        // Get config values
        let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;

        let backing_file = self
            .terminal_backing_files
            .get(&terminal_id)
            .cloned()
            .unwrap_or_else(|| {
                let root = self.dir_context.terminal_dir_for(&self.working_dir);
                let _ = self.filesystem.create_dir_all(&root);
                root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
            });

        // Create the file only if it doesn't exist (preserve existing scrollback for restore)
        if !self.filesystem.exists(&backing_file) {
            if let Err(e) = self.filesystem.write_file(&backing_file, &[]) {
                tracing::warn!("Failed to create terminal backing file: {}", e);
            }
        }

        // Create editor state with the backing file
        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            large_file_threshold,
            std::sync::Arc::clone(&self.filesystem),
        );
        state.buffer.set_file_path(backing_file.clone());
        state.margins.configure_for_line_numbers(false);
        self.buffers.insert(buffer_id, state);

        let metadata = BufferMetadata::virtual_buffer(
            format!("*Terminal {}*", terminal_id.0),
            "terminal".into(),
            false,
        );
        self.buffer_metadata.insert(buffer_id, metadata);
        self.terminal_buffers.insert(buffer_id, terminal_id);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        buffer_id
    }

    /// Close the current terminal (if viewing a terminal buffer)
    pub fn close_terminal(&mut self) {
        let buffer_id = self.active_buffer();

        if let Some(&terminal_id) = self.terminal_buffers.get(&buffer_id) {
            // Close the terminal
            self.terminal_manager.close(terminal_id);
            self.terminal_buffers.remove(&buffer_id);

            // Clean up backing/rendering file
            let backing_file = self.terminal_backing_files.remove(&terminal_id);
            if let Some(ref path) = backing_file {
                let _ = self.filesystem.remove_file(path);
            }
            // Clean up raw log file
            if let Some(log_file) = self.terminal_log_files.remove(&terminal_id) {
                if backing_file.as_ref() != Some(&log_file) {
                    let _ = self.filesystem.remove_file(&log_file);
                }
            }

            // Exit terminal mode
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;

            // Close the buffer
            let _ = self.close_buffer(buffer_id);

            self.set_status_message(t!("terminal.closed", id = terminal_id.0).to_string());
        } else {
            self.set_status_message(t!("status.not_viewing_terminal").to_string());
        }
    }

    /// Check if a buffer is a terminal buffer
    pub fn is_terminal_buffer(&self, buffer_id: BufferId) -> bool {
        self.terminal_buffers.contains_key(&buffer_id)
    }

    /// Get the terminal ID for a buffer (if it's a terminal buffer)
    pub fn get_terminal_id(&self, buffer_id: BufferId) -> Option<TerminalId> {
        self.terminal_buffers.get(&buffer_id).copied()
    }

    /// Get the terminal state for the active buffer (if it's a terminal buffer)
    pub fn get_active_terminal_state(
        &self,
    ) -> Option<std::sync::MutexGuard<'_, crate::services::terminal::TerminalState>> {
        let terminal_id = self.terminal_buffers.get(&self.active_buffer())?;
        let handle = self.terminal_manager.get(*terminal_id)?;
        handle.state.lock().ok()
    }

    /// Send input to the active terminal
    pub fn send_terminal_input(&mut self, data: &[u8]) {
        if let Some(&terminal_id) = self.terminal_buffers.get(&self.active_buffer()) {
            if let Some(handle) = self.terminal_manager.get(terminal_id) {
                handle.write(data);
            }
        }
    }

    /// Send a key event to the active terminal
    pub fn send_terminal_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) {
        if let Some(bytes) = crate::services::terminal::pty::key_to_pty_bytes(code, modifiers) {
            self.send_terminal_input(&bytes);
        }
    }

    /// Send a mouse event to the active terminal
    pub fn send_terminal_mouse(
        &mut self,
        col: u16,
        row: u16,
        kind: crate::input::handler::TerminalMouseEventKind,
        modifiers: crossterm::event::KeyModifiers,
    ) {
        use crate::input::handler::TerminalMouseEventKind;

        // Check if terminal uses SGR mouse encoding
        let use_sgr = self
            .get_active_terminal_state()
            .map(|s| s.uses_sgr_mouse())
            .unwrap_or(true); // Default to SGR as it's the modern standard

        // For alternate scroll mode, convert scroll to arrow keys
        let uses_alt_scroll = self
            .get_active_terminal_state()
            .map(|s| s.uses_alternate_scroll())
            .unwrap_or(false);

        if uses_alt_scroll {
            match kind {
                TerminalMouseEventKind::ScrollUp => {
                    // Send up arrow 3 times (typical scroll amount)
                    for _ in 0..3 {
                        self.send_terminal_input(b"\x1b[A");
                    }
                    return;
                }
                TerminalMouseEventKind::ScrollDown => {
                    // Send down arrow 3 times
                    for _ in 0..3 {
                        self.send_terminal_input(b"\x1b[B");
                    }
                    return;
                }
                _ => {}
            }
        }

        // Encode mouse event for terminal
        let bytes = if use_sgr {
            encode_sgr_mouse(col, row, kind, modifiers)
        } else {
            encode_x10_mouse(col, row, kind, modifiers)
        };

        if let Some(bytes) = bytes {
            self.send_terminal_input(&bytes);
        }
    }

    /// Check if the active terminal buffer is in alternate screen mode.
    /// Programs like vim, less, htop use alternate screen mode.
    pub fn is_terminal_in_alternate_screen(&self, buffer_id: BufferId) -> bool {
        if let Some(&terminal_id) = self.terminal_buffers.get(&buffer_id) {
            if let Some(handle) = self.terminal_manager.get(terminal_id) {
                if let Ok(state) = handle.state.lock() {
                    return state.is_alternate_screen();
                }
            }
        }
        false
    }

    /// Get terminal dimensions based on split size
    pub(crate) fn get_terminal_dimensions(&self) -> (u16, u16) {
        // Use the visible area of the current split
        // Subtract 1 for status bar, tab bar, etc.
        let cols = self.terminal_width.saturating_sub(2).max(40);
        let rows = self.terminal_height.saturating_sub(4).max(10);
        (cols, rows)
    }

    /// Resize terminal to match split dimensions
    pub fn resize_terminal(&mut self, buffer_id: BufferId, cols: u16, rows: u16) {
        if let Some(&terminal_id) = self.terminal_buffers.get(&buffer_id) {
            if let Some(handle) = self.terminal_manager.get_mut(terminal_id) {
                handle.resize(cols, rows);
            }
        }
    }

    /// Resize all visible terminal PTYs to match their current split dimensions.
    /// Call this after operations that change split layout (maximize, resize, etc.)
    pub fn resize_visible_terminals(&mut self) {
        // Get the content area excluding file explorer
        let file_explorer_width = if self.file_explorer_visible {
            (self.terminal_width as f32 * self.file_explorer_width_percent) as u16
        } else {
            0
        };
        let editor_width = self.terminal_width.saturating_sub(file_explorer_width);
        let editor_area = ratatui::layout::Rect::new(
            file_explorer_width,
            1, // menu bar
            editor_width,
            self.terminal_height.saturating_sub(2), // menu bar + status bar
        );

        // Get visible buffers with their areas
        let visible_buffers = self.split_manager.get_visible_buffers(editor_area);

        // Resize each terminal buffer to match its split content area
        for (_split_id, buffer_id, split_area) in visible_buffers {
            if self.terminal_buffers.contains_key(&buffer_id) {
                // Calculate content dimensions (accounting for tab bar and borders)
                // Tab bar takes 1 row, and we leave 1 for scrollbar width on right
                let content_height = split_area.height.saturating_sub(2);
                let content_width = split_area.width.saturating_sub(2);

                if content_width > 0 && content_height > 0 {
                    self.resize_terminal(buffer_id, content_width, content_height);
                }
            }
        }
    }

    /// Handle terminal input when in terminal mode
    pub fn handle_terminal_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> bool {
        // Check for escape sequences to exit terminal mode
        // Ctrl+Space, Ctrl+], or Ctrl+` to exit (Ctrl+\ sends SIGQUIT on Unix)
        if modifiers.contains(crossterm::event::KeyModifiers::CONTROL) {
            match code {
                crossterm::event::KeyCode::Char(' ')
                | crossterm::event::KeyCode::Char(']')
                | crossterm::event::KeyCode::Char('`') => {
                    // Exit terminal mode and sync buffer
                    self.terminal_mode = false;
                    self.key_context = crate::input::keybindings::KeyContext::Normal;
                    self.sync_terminal_to_buffer(self.active_buffer());
                    self.set_status_message(
                        "Terminal mode disabled - read only (Ctrl+Space to resume)".to_string(),
                    );
                    return true;
                }
                _ => {}
            }
        }

        // Send the key to the terminal
        self.send_terminal_key(code, modifiers);
        true
    }

    /// Sync terminal content to the text buffer for read-only viewing/selection
    ///
    /// This uses the incremental streaming architecture:
    /// 1. Scrollback has already been streamed to the backing file during PTY reads
    /// 2. We just append the visible screen (~50 lines) to the backing file
    /// 3. Reload the buffer from the backing file (lazy load for large files)
    ///
    /// Performance: O(screen_size) instead of O(total_history)
    pub fn sync_terminal_to_buffer(&mut self, buffer_id: BufferId) {
        if let Some(&terminal_id) = self.terminal_buffers.get(&buffer_id) {
            // Get the backing file path
            let backing_file = match self.terminal_backing_files.get(&terminal_id) {
                Some(path) => path.clone(),
                None => return,
            };

            // Append visible screen to backing file
            // The scrollback has already been incrementally streamed by the PTY read loop
            if let Some(handle) = self.terminal_manager.get(terminal_id) {
                if let Ok(mut state) = handle.state.lock() {
                    // Record the current file size as the history end point
                    // (before appending visible screen) so we can truncate back to it
                    if let Ok(metadata) = self.filesystem.metadata(&backing_file) {
                        state.set_backing_file_history_end(metadata.size);
                    }

                    // Open backing file in append mode to add visible screen
                    if let Ok(mut file) = self.filesystem.open_file_for_append(&backing_file) {
                        use std::io::BufWriter;
                        let mut writer = BufWriter::new(&mut *file);
                        if let Err(e) = state.append_visible_screen(&mut writer) {
                            tracing::error!(
                                "Failed to append visible screen to backing file: {}",
                                e
                            );
                        }
                    }
                }
            }

            // Reload buffer from the backing file (reusing existing file loading)
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;
            if let Ok(new_state) = EditorState::from_file_with_languages(
                &backing_file,
                self.terminal_width,
                self.terminal_height,
                large_file_threshold,
                &self.grammar_registry,
                &self.config.languages,
                std::sync::Arc::clone(&self.filesystem),
            ) {
                // Replace buffer state
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let total_bytes = new_state.buffer.total_bytes();
                    *state = new_state;
                    // Terminal buffers should never be considered "modified"
                    state.buffer.set_modified(false);
                    // Move cursor to end of buffer in SplitViewState
                    if let Some(view_state) = self
                        .split_view_states
                        .get_mut(&self.split_manager.active_split())
                    {
                        view_state.cursors.primary_mut().position = total_bytes;
                    }
                }
            }

            // Mark buffer as editing-disabled while in non-terminal mode
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.editing_disabled = true;
                state.margins.configure_for_line_numbers(false);
            }

            // In read-only view, keep line wrapping disabled for terminal buffers
            // Also scroll viewport to show the end of the buffer where the cursor is
            if let Some(view_state) = self
                .split_view_states
                .get_mut(&self.split_manager.active_split())
            {
                view_state.viewport.line_wrap_enabled = false;

                // Clear skip_ensure_visible flag so the viewport scrolls to cursor
                // This fixes the bug where re-entering scrollback mode would jump to the
                // previous scroll position because the flag was still set from scrolling
                view_state.viewport.clear_skip_ensure_visible();

                // Scroll viewport to make cursor visible at the end of buffer
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let cursor = *view_state.cursors.primary();
                    view_state
                        .viewport
                        .ensure_visible(&mut state.buffer, &cursor);
                }
            }
        }
    }

    /// Re-enter terminal mode from read-only buffer view
    ///
    /// This truncates the backing file to remove the visible screen tail
    /// that was appended when we exited terminal mode, leaving only the
    /// incrementally-streamed scrollback history.
    pub fn enter_terminal_mode(&mut self) {
        if self.is_terminal_buffer(self.active_buffer()) {
            self.terminal_mode = true;
            self.key_context = crate::input::keybindings::KeyContext::Terminal;

            // Re-enable editing when in terminal mode (input goes to PTY)
            if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
                state.editing_disabled = false;
                state.margins.configure_for_line_numbers(false);
            }
            if let Some(view_state) = self
                .split_view_states
                .get_mut(&self.split_manager.active_split())
            {
                view_state.viewport.line_wrap_enabled = false;
            }

            // Truncate backing file to remove visible screen tail and scroll to bottom
            if let Some(&terminal_id) = self.terminal_buffers.get(&self.active_buffer()) {
                // Truncate backing file to remove visible screen that was appended
                if let Some(backing_path) = self.terminal_backing_files.get(&terminal_id) {
                    if let Some(handle) = self.terminal_manager.get(terminal_id) {
                        if let Ok(state) = handle.state.lock() {
                            let truncate_pos = state.backing_file_history_end();
                            // Always truncate to remove appended visible screen
                            // (even if truncate_pos is 0, meaning no scrollback yet)
                            if let Err(e) =
                                self.filesystem.set_file_length(backing_path, truncate_pos)
                            {
                                tracing::warn!("Failed to truncate terminal backing file: {}", e);
                            }
                        }
                    }
                }

                // Scroll terminal to bottom when re-entering
                if let Some(handle) = self.terminal_manager.get(terminal_id) {
                    if let Ok(mut state) = handle.state.lock() {
                        state.scroll_to_bottom();
                    }
                }
            }

            // Ensure terminal PTY is sized correctly for current split dimensions
            self.resize_visible_terminals();

            self.set_status_message(t!("status.terminal_mode_enabled").to_string());
        }
    }

    /// Get terminal content for rendering
    pub fn get_terminal_content(
        &self,
        buffer_id: BufferId,
    ) -> Option<Vec<Vec<crate::services::terminal::TerminalCell>>> {
        let terminal_id = self.terminal_buffers.get(&buffer_id)?;
        let handle = self.terminal_manager.get(*terminal_id)?;
        let state = handle.state.lock().ok()?;

        let (_, rows) = state.size();
        let mut content = Vec::with_capacity(rows as usize);

        for row in 0..rows {
            content.push(state.get_line(row));
        }

        Some(content)
    }
}

impl Editor {
    /// Check if terminal mode is active (for testing)
    pub fn is_terminal_mode(&self) -> bool {
        self.terminal_mode
    }

    /// Check if a buffer is in terminal_mode_resume set (for testing/debugging)
    pub fn is_in_terminal_mode_resume(&self, buffer_id: BufferId) -> bool {
        self.terminal_mode_resume.contains(&buffer_id)
    }

    /// Check if keyboard capture is enabled in terminal mode (for testing)
    pub fn is_keyboard_capture(&self) -> bool {
        self.keyboard_capture
    }

    /// Set terminal jump_to_end_on_output config option (for testing)
    pub fn set_terminal_jump_to_end_on_output(&mut self, value: bool) {
        self.config.terminal.jump_to_end_on_output = value;
    }

    /// Get read-only access to the terminal manager (for testing)
    pub fn terminal_manager(&self) -> &crate::services::terminal::TerminalManager {
        &self.terminal_manager
    }

    /// Get read-only access to terminal backing files map (for testing)
    pub fn terminal_backing_files(
        &self,
    ) -> &std::collections::HashMap<crate::services::terminal::TerminalId, std::path::PathBuf> {
        &self.terminal_backing_files
    }

    /// Get the currently active buffer ID
    pub fn active_buffer_id(&self) -> BufferId {
        self.active_buffer()
    }

    /// Get buffer content as a string (for testing)
    pub fn get_buffer_content(&self, buffer_id: BufferId) -> Option<String> {
        self.buffers
            .get(&buffer_id)
            .and_then(|state| state.buffer.to_string())
    }

    /// Get cursor position for a buffer (for testing)
    pub fn get_cursor_position(&self, buffer_id: BufferId) -> Option<usize> {
        // Find cursor from any split view state that has this buffer
        self.split_view_states
            .values()
            .find_map(|vs| {
                if vs.keyed_states.contains_key(&buffer_id) {
                    Some(vs.keyed_states.get(&buffer_id)?.cursors.primary().position)
                } else {
                    None
                }
            })
            .or_else(|| {
                // Fallback: check active cursors
                self.split_view_states
                    .values()
                    .find_map(|vs| Some(vs.cursors.primary().position))
            })
    }

    /// Render terminal content for all terminal buffers in split areas
    ///
    /// Renders all visible terminal buffers from their live terminal state.
    /// This ensures terminals continue updating even when not focused, as long
    /// as they remain visible in a split.
    pub fn render_terminal_splits(
        &self,
        frame: &mut ratatui::Frame,
        split_areas: &[(
            crate::model::event::SplitId,
            BufferId,
            ratatui::layout::Rect,
            ratatui::layout::Rect,
            usize,
            usize,
        )],
    ) {
        for (_split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            split_areas
        {
            // Only render terminal buffers - skip regular file buffers
            if let Some(&terminal_id) = self.terminal_buffers.get(buffer_id) {
                // Only render from live terminal state if in terminal mode OR if not the active buffer
                // (when it's the active buffer but not in terminal mode, we're in read-only scrollback mode
                // and should show the synced buffer content instead)
                let is_active = *buffer_id == self.active_buffer();
                if is_active && !self.terminal_mode {
                    // Active buffer in read-only mode - let normal buffer rendering handle it
                    continue;
                }
                // Get terminal content and cursor info
                if let Some(handle) = self.terminal_manager.get(terminal_id) {
                    if let Ok(state) = handle.state.lock() {
                        let cursor_pos = state.cursor_position();
                        // Only show cursor for the active terminal in terminal mode
                        let cursor_visible =
                            state.cursor_visible() && is_active && self.terminal_mode;
                        let (_, rows) = state.size();

                        // Collect content
                        let mut content = Vec::with_capacity(rows as usize);
                        for row in 0..rows {
                            content.push(state.get_line(row));
                        }

                        // Clear the content area first
                        frame.render_widget(ratatui::widgets::Clear, *content_rect);

                        // Render terminal content with theme colors
                        render::render_terminal_content(
                            &content,
                            cursor_pos,
                            cursor_visible,
                            *content_rect,
                            frame.buffer_mut(),
                            self.theme.terminal_fg,
                            self.theme.terminal_bg,
                        );
                    }
                }
            }
        }
    }
}

/// Terminal rendering utilities
pub mod render {
    use crate::services::terminal::TerminalCell;
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;
    use ratatui::style::{Color, Modifier, Style};

    /// Render terminal content to a ratatui buffer
    pub fn render_terminal_content(
        content: &[Vec<TerminalCell>],
        cursor_pos: (u16, u16),
        cursor_visible: bool,
        area: Rect,
        buf: &mut Buffer,
        default_fg: Color,
        default_bg: Color,
    ) {
        for (row_idx, row) in content.iter().enumerate() {
            if row_idx as u16 >= area.height {
                break;
            }

            let y = area.y + row_idx as u16;

            for (col_idx, cell) in row.iter().enumerate() {
                if col_idx as u16 >= area.width {
                    break;
                }

                let x = area.x + col_idx as u16;

                // Build style from cell attributes, using theme defaults
                let mut style = Style::default().fg(default_fg).bg(default_bg);

                // Override with cell-specific colors if present
                if let Some((r, g, b)) = cell.fg {
                    style = style.fg(Color::Rgb(r, g, b));
                }

                if let Some((r, g, b)) = cell.bg {
                    style = style.bg(Color::Rgb(r, g, b));
                }

                // Apply modifiers
                if cell.bold {
                    style = style.add_modifier(Modifier::BOLD);
                }
                if cell.italic {
                    style = style.add_modifier(Modifier::ITALIC);
                }
                if cell.underline {
                    style = style.add_modifier(Modifier::UNDERLINED);
                }
                if cell.inverse {
                    style = style.add_modifier(Modifier::REVERSED);
                }

                // Check if this is the cursor position
                if cursor_visible
                    && row_idx as u16 == cursor_pos.1
                    && col_idx as u16 == cursor_pos.0
                {
                    style = style.add_modifier(Modifier::REVERSED);
                }

                buf.set_string(x, y, cell.c.to_string(), style);
            }
        }
    }
}

/// Encode a mouse event in SGR format (modern protocol).
/// Format: CSI < Cb ; Cx ; Cy M (press) or CSI < Cb ; Cx ; Cy m (release)
fn encode_sgr_mouse(
    col: u16,
    row: u16,
    kind: crate::input::handler::TerminalMouseEventKind,
    modifiers: crossterm::event::KeyModifiers,
) -> Option<Vec<u8>> {
    use crate::input::handler::{TerminalMouseButton, TerminalMouseEventKind};

    // SGR uses 1-based coordinates
    let cx = col + 1;
    let cy = row + 1;

    // Build button code
    let (button_code, is_release) = match kind {
        TerminalMouseEventKind::Down(btn) => {
            let code = match btn {
                TerminalMouseButton::Left => 0,
                TerminalMouseButton::Middle => 1,
                TerminalMouseButton::Right => 2,
            };
            (code, false)
        }
        TerminalMouseEventKind::Up(btn) => {
            let code = match btn {
                TerminalMouseButton::Left => 0,
                TerminalMouseButton::Middle => 1,
                TerminalMouseButton::Right => 2,
            };
            (code, true)
        }
        TerminalMouseEventKind::Drag(btn) => {
            let code = match btn {
                TerminalMouseButton::Left => 32,   // 0 + 32 (motion flag)
                TerminalMouseButton::Middle => 33, // 1 + 32
                TerminalMouseButton::Right => 34,  // 2 + 32
            };
            (code, false)
        }
        TerminalMouseEventKind::Moved => (35, false), // 3 + 32 (no button + motion)
        TerminalMouseEventKind::ScrollUp => (64, false),
        TerminalMouseEventKind::ScrollDown => (65, false),
    };

    // Add modifier flags
    let mut cb = button_code;
    if modifiers.contains(crossterm::event::KeyModifiers::SHIFT) {
        cb += 4;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::ALT) {
        cb += 8;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::CONTROL) {
        cb += 16;
    }

    // Build escape sequence
    let terminator = if is_release { 'm' } else { 'M' };
    Some(format!("\x1b[<{};{};{}{}", cb, cx, cy, terminator).into_bytes())
}

/// Encode a mouse event in X10/normal format (legacy protocol).
/// Format: CSI M Cb Cx Cy (with 32 added to all values for ASCII safety)
fn encode_x10_mouse(
    col: u16,
    row: u16,
    kind: crate::input::handler::TerminalMouseEventKind,
    modifiers: crossterm::event::KeyModifiers,
) -> Option<Vec<u8>> {
    use crate::input::handler::{TerminalMouseButton, TerminalMouseEventKind};

    // X10 uses 1-based coordinates with 32 offset for ASCII safety
    // Maximum coordinate is 223 (255 - 32)
    let cx = (col.min(222) + 1 + 32) as u8;
    let cy = (row.min(222) + 1 + 32) as u8;

    // Build button code
    let button_code: u8 = match kind {
        TerminalMouseEventKind::Down(btn) | TerminalMouseEventKind::Drag(btn) => match btn {
            TerminalMouseButton::Left => 0,
            TerminalMouseButton::Middle => 1,
            TerminalMouseButton::Right => 2,
        },
        TerminalMouseEventKind::Up(_) => 3, // Release is button 3 in X10
        TerminalMouseEventKind::Moved => 3 + 32,
        TerminalMouseEventKind::ScrollUp => 64,
        TerminalMouseEventKind::ScrollDown => 65,
    };

    // Add modifier flags and motion flag for drag
    let mut cb = button_code;
    if matches!(kind, TerminalMouseEventKind::Drag(_)) {
        cb += 32; // Motion flag
    }
    if modifiers.contains(crossterm::event::KeyModifiers::SHIFT) {
        cb += 4;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::ALT) {
        cb += 8;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::CONTROL) {
        cb += 16;
    }

    // Add 32 offset for ASCII safety
    let cb = cb + 32;

    Some(vec![0x1b, b'[', b'M', cb, cx, cy])
}
