//! Terminal integration for the Editor
//!
//! This module provides methods for the Editor to interact with the terminal system:
//! - Opening new terminal sessions
//! - Closing terminals
//! - Rendering terminal content
//! - Handling terminal input

use super::{BufferId, BufferMetadata, Editor};
use crate::services::terminal::TerminalId;
use crate::state::EditorState;

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
        let _ = std::fs::create_dir_all(&terminal_root);
        // Precompute paths using the next terminal ID so we capture from the first byte
        let predicted_terminal_id = self.terminal_manager.next_terminal_id();
        let log_path =
            terminal_root.join(format!("fresh-terminal-{}.log", predicted_terminal_id.0));
        let backing_path =
            terminal_root.join(format!("fresh-terminal-{}.txt", predicted_terminal_id.0));
        // Stash backing path now so buffer creation can reuse it
        self.terminal_backing_files
            .insert(predicted_terminal_id, backing_path);

        // Spawn terminal
        match self.terminal_manager.spawn(
            cols,
            rows,
            Some(self.working_dir.clone()),
            Some(log_path.clone()),
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

                self.set_status_message(format!(
                    "Terminal {} opened (Ctrl+Space to exit)",
                    terminal_id
                ));
                tracing::info!(
                    "Opened terminal {:?} with buffer {:?}",
                    terminal_id,
                    buffer_id
                );
            }
            Err(e) => {
                self.set_status_message(format!("Failed to open terminal: {}", e));
                tracing::error!("Failed to open terminal: {}", e);
            }
        }
    }

    /// Create a buffer for a terminal session
    fn create_terminal_buffer_attached(
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
                let _ = std::fs::create_dir_all(&root);
                root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
            });

        // Ensure the file exists
        if let Err(e) = std::fs::write(&backing_file, "") {
            tracing::warn!("Failed to create terminal backing file: {}", e);
        }

        // Store the backing file path
        self.terminal_backing_files
            .insert(terminal_id, backing_file.clone());

        // Create editor state with the backing file
        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            large_file_threshold,
        );
        state.buffer.set_file_path(backing_file.clone());
        // Terminal buffers should never show line numbers
        state.margins.set_line_numbers(false);
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
                let _ = std::fs::create_dir_all(&root);
                root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
            });

        // Ensure the file exists
        if let Err(e) = std::fs::write(&backing_file, "") {
            tracing::warn!("Failed to create terminal backing file: {}", e);
        }

        // Create editor state with the backing file
        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            large_file_threshold,
        );
        state.buffer.set_file_path(backing_file.clone());
        state.margins.set_line_numbers(false);
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
                let _ = std::fs::remove_file(path);
            }
            // Clean up raw log file
            if let Some(log_file) = self.terminal_log_files.remove(&terminal_id) {
                if backing_file.as_ref() != Some(&log_file) {
                    let _ = std::fs::remove_file(&log_file);
                }
            }

            // Exit terminal mode
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;

            // Close the buffer
            let _ = self.close_buffer(buffer_id);

            self.set_status_message(format!("Terminal {} closed", terminal_id));
        } else {
            self.set_status_message("Not viewing a terminal buffer".to_string());
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

    /// Get terminal dimensions based on split size
    fn get_terminal_dimensions(&self) -> (u16, u16) {
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
    pub fn sync_terminal_to_buffer(&mut self, buffer_id: BufferId) {
        if let Some(&terminal_id) = self.terminal_buffers.get(&buffer_id) {
            // Get the backing (rendered) file path and raw log path
            let backing_file = match self.terminal_backing_files.get(&terminal_id) {
                Some(path) => path.clone(),
                None => return,
            };
            let log_file = self.terminal_log_files.get(&terminal_id).cloned();

            // Render content either from the raw log (preferred) or the live emulator state
            let content = if let (Some(log_path), Some(handle)) =
                (log_file, self.terminal_manager.get(terminal_id))
            {
                // Replay the raw log through a fresh terminal state to capture full history
                let (cols, rows) = handle.size();
                let mut state = crate::services::terminal::TerminalState::new(cols, rows);
                if let Ok(mut file) = std::fs::File::open(&log_path) {
                    use std::io::Read;
                    let mut buf = [0u8; 4096];
                    while let Ok(n) = file.read(&mut buf) {
                        if n == 0 {
                            break;
                        }
                        state.process_output(&buf[..n]);
                    }
                    Some(state.full_content_string())
                } else {
                    None
                }
            } else {
                None
            }
            .or_else(|| {
                // Fallback: use current emulator state
                self.terminal_manager
                    .get(terminal_id)
                    .and_then(|handle| handle.state.lock().ok())
                    .map(|state| state.full_content_string())
            });

            // Write rendered content to the backing file if we have it
            if let Some(content) = content {
                if let Err(e) = std::fs::write(&backing_file, &content) {
                    tracing::error!("Failed to write terminal content to backing file: {}", e);
                    return;
                }
            }

            // Reload buffer from the backing file (reusing existing file loading)
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;
            if let Ok(new_state) = EditorState::from_file(
                &backing_file,
                self.terminal_width,
                self.terminal_height,
                large_file_threshold,
                &self.grammar_registry,
            ) {
                // Replace buffer state
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    *state = new_state;
                    // Move cursor to end of buffer
                    let total = state.buffer.total_bytes();
                    state.primary_cursor_mut().position = total;
                    // Terminal buffers should never be considered "modified"
                    state.buffer.set_modified(false);
                }
            }

            // Mark buffer as editing-disabled while in non-terminal mode
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.editing_disabled = true;
                state.margins.set_line_numbers(false);
            }

            // In read-only view, keep line wrapping disabled for terminal buffers
            if let Some(view_state) = self
                .split_view_states
                .get_mut(&self.split_manager.active_split())
            {
                view_state.viewport.line_wrap_enabled = false;
            }
        }
    }

    /// Re-enter terminal mode from read-only buffer view
    pub fn enter_terminal_mode(&mut self) {
        if self.is_terminal_buffer(self.active_buffer()) {
            self.terminal_mode = true;
            self.key_context = crate::input::keybindings::KeyContext::Terminal;

            // Re-enable editing when in terminal mode (input goes to PTY)
            if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
                state.editing_disabled = false;
                state.margins.set_line_numbers(false);
            }
            if let Some(view_state) = self
                .split_view_states
                .get_mut(&self.split_manager.active_split())
            {
                view_state.viewport.line_wrap_enabled = false;
            }

            // Scroll terminal to bottom when re-entering
            if let Some(&terminal_id) = self.terminal_buffers.get(&self.active_buffer()) {
                if let Some(handle) = self.terminal_manager.get(terminal_id) {
                    if let Ok(mut state) = handle.state.lock() {
                        state.scroll_to_bottom();
                    }
                }
            }

            self.set_status_message("Terminal mode enabled".to_string());
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

    /// Get read-only access to the terminal manager (for testing)
    pub fn terminal_manager(&self) -> &crate::services::terminal::TerminalManager {
        &self.terminal_manager
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
        self.buffers
            .get(&buffer_id)
            .map(|state| state.primary_cursor().position)
    }

    /// Render terminal content for all terminal buffers in split areas
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
        // Only render terminal content when in terminal mode
        // In read-only mode, the buffer content is rendered by normal buffer rendering
        if !self.terminal_mode {
            return;
        }

        for (_split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            split_areas
        {
            // Check if this buffer is the active terminal buffer
            if *buffer_id != self.active_buffer() {
                continue;
            }

            if let Some(&terminal_id) = self.terminal_buffers.get(buffer_id) {
                // Get terminal content and cursor info
                if let Some(handle) = self.terminal_manager.get(terminal_id) {
                    if let Ok(state) = handle.state.lock() {
                        let cursor_pos = state.cursor_position();
                        let cursor_visible = state.cursor_visible();
                        let (_, rows) = state.size();

                        // Collect content
                        let mut content = Vec::with_capacity(rows as usize);
                        for row in 0..rows {
                            content.push(state.get_line(row));
                        }

                        // Clear the content area first
                        frame.render_widget(ratatui::widgets::Clear, *content_rect);

                        // Render terminal content
                        render::render_terminal_content(
                            &content,
                            cursor_pos,
                            cursor_visible,
                            *content_rect,
                            frame.buffer_mut(),
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

                // Build style from cell attributes
                let mut style = Style::default();

                // Set foreground color
                if let Some((r, g, b)) = cell.fg {
                    style = style.fg(Color::Rgb(r, g, b));
                }

                // Set background color
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
