//! Buffer management operations for the Editor.
//!
//! This module contains all methods related to buffer lifecycle and navigation:
//! - Opening files (with and without focus)
//! - Creating new buffers (regular and virtual)
//! - Closing buffers and tabs
//! - Switching between buffers
//! - Navigate back/forward in position history
//! - Buffer state persistence

use rust_i18n::t;
use std::io;
use std::path::Path;

use crate::app::warning_domains::WarningDomain;
use crate::model::event::{BufferId, Event, SplitId};
use crate::services::lsp::manager::detect_language;
use crate::state::EditorState;
use crate::view::prompt::PromptType;
use crate::view::split::SplitViewState;

use super::help;
use super::Editor;

impl Editor {
    /// Open a file and return its buffer ID
    ///
    /// If the file doesn't exist, creates an unsaved buffer with that filename.
    /// Saving the buffer will create the file.
    pub fn open_file(&mut self, path: &Path) -> io::Result<BufferId> {
        let buffer_id = self.open_file_no_focus(path)?;

        // Check if this was an already-open buffer or a new one
        // For already-open buffers, just switch to them
        // For new buffers, record position history before switching
        let is_new_buffer = self.active_buffer() != buffer_id;

        if is_new_buffer {
            // Save current position before switching to new buffer
            self.position_history.commit_pending_movement();

            // Explicitly record current position before switching
            let current_state = self.active_state();
            let position = current_state.cursors.primary().position;
            let anchor = current_state.cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();
        }

        self.set_active_buffer(buffer_id);

        // Use display_name from metadata for relative path display
        let display_name = self
            .buffer_metadata
            .get(&buffer_id)
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| path.display().to_string());

        // Check if buffer is binary for status message
        let is_binary = self
            .buffers
            .get(&buffer_id)
            .map(|s| s.buffer.is_binary())
            .unwrap_or(false);

        // Show appropriate status message for binary vs regular files
        if is_binary {
            self.status_message = Some(t!("buffer.opened_binary", name = display_name).to_string());
        } else {
            self.status_message = Some(t!("buffer.opened", name = display_name).to_string());
        }

        Ok(buffer_id)
    }

    /// Open a file without switching focus to it
    ///
    /// Creates a new buffer for the file (or returns existing buffer ID if already open)
    /// but does not change the active buffer. Useful for opening files in background tabs.
    ///
    /// If the file doesn't exist, creates an unsaved buffer with that filename.
    pub fn open_file_no_focus(&mut self, path: &Path) -> io::Result<BufferId> {
        // Resolve relative paths against working_dir, not process current directory
        let resolved_path = if path.is_relative() {
            self.working_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Determine if we're opening a non-existent file (for creating new files)
        let file_exists = resolved_path.exists();

        // Canonicalize the path to resolve symlinks and normalize path components
        // This ensures consistent path representation throughout the editor
        // For non-existent files, we need to canonicalize the parent directory and append the filename
        let canonical_path = if file_exists {
            resolved_path
                .canonicalize()
                .unwrap_or_else(|_| resolved_path.clone())
        } else {
            // For non-existent files, canonicalize parent dir and append filename
            if let Some(parent) = resolved_path.parent() {
                let canonical_parent = if parent.as_os_str().is_empty() {
                    // No parent means just a filename, use working dir
                    self.working_dir.clone()
                } else {
                    parent
                        .canonicalize()
                        .unwrap_or_else(|_| parent.to_path_buf())
                };
                if let Some(filename) = resolved_path.file_name() {
                    canonical_parent.join(filename)
                } else {
                    resolved_path
                }
            } else {
                resolved_path
            }
        };
        let path = canonical_path.as_path();

        // Check if file is already open - return existing buffer without switching
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            return Ok(id);
        }

        // If the current buffer is empty and unmodified, replace it instead of creating a new one
        let replace_current = {
            let current_state = self.buffers.get(&self.active_buffer()).unwrap();
            current_state.buffer.is_empty()
                && !current_state.buffer.is_modified()
                && current_state.buffer.file_path().is_none()
        };

        let buffer_id = if replace_current {
            // Reuse the current empty buffer
            self.active_buffer()
        } else {
            // Create new buffer for this file
            let id = BufferId(self.next_buffer_id);
            self.next_buffer_id += 1;
            id
        };

        // Create the editor state - either load from file or create empty buffer
        let mut state = if file_exists {
            EditorState::from_file_with_languages(
                path,
                self.terminal_width,
                self.terminal_height,
                self.config.editor.large_file_threshold_bytes as usize,
                &self.grammar_registry,
                &self.config.languages,
            )?
        } else {
            // File doesn't exist - create empty buffer with the file path set
            let mut new_state = EditorState::new(
                self.terminal_width,
                self.terminal_height,
                self.config.editor.large_file_threshold_bytes as usize,
            );
            // Set the file path so saving will create the file
            new_state.buffer.set_file_path(path.to_path_buf());
            new_state
        };
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created

        // Check if the buffer contains binary content
        let is_binary = state.buffer.is_binary();
        if is_binary {
            // Make binary buffers read-only
            state.editing_disabled = true;
            tracing::info!("Detected binary file: {}", path.display());
        }

        // Set show_whitespace_tabs, use_tabs, and tab_size based on language config
        // with fallback to global editor config for tab_size
        if let Some(language) = detect_language(path, &self.config.languages) {
            if let Some(lang_config) = self.config.languages.get(&language) {
                state.show_whitespace_tabs = lang_config.show_whitespace_tabs;
                state.use_tabs = lang_config.use_tabs;
                // Use language-specific tab_size if set, otherwise fall back to global
                state.tab_size = lang_config.tab_size.unwrap_or(self.config.editor.tab_size);
            } else {
                state.tab_size = self.config.editor.tab_size;
            }
        } else {
            state.tab_size = self.config.editor.tab_size;
        }

        // Apply line_numbers default from config
        state
            .margins
            .set_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata for this buffer
        let mut metadata =
            super::types::BufferMetadata::with_file(path.to_path_buf(), &self.working_dir);

        // Mark binary files in metadata and disable LSP
        if is_binary {
            metadata.binary = true;
            metadata.read_only = true;
            metadata.disable_lsp(t!("buffer.binary_file").to_string());
        }

        // Notify LSP about the newly opened file (skip for binary files)
        if !is_binary {
            self.notify_lsp_file_opened(path, buffer_id, &mut metadata);
        }

        // Store metadata for this buffer
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add buffer to the active split's tabs (but don't switch to it)
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
            // Apply line_wrap default from config (per-view setting, applies to split)
            view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
        }

        // Restore global file state (scroll/cursor position) if available
        // This persists file positions across projects and editor instances
        self.restore_global_file_state(buffer_id, path, active_split);

        // Emit control event
        self.emit_event(
            crate::model::control_event::events::FILE_OPENED.name,
            serde_json::json!({
                "path": path.display().to_string(),
                "buffer_id": buffer_id.0
            }),
        );

        // Track file for auto-revert and conflict detection
        self.watch_file(path);

        // Fire AfterFileOpen hook for plugins
        self.plugin_manager.run_hook(
            "after_file_open",
            crate::services::plugins::hooks::HookArgs::AfterFileOpen {
                buffer_id,
                path: path.to_path_buf(),
            },
        );

        Ok(buffer_id)
    }

    /// Restore global file state (cursor and scroll position) for a newly opened file
    ///
    /// This looks up the file's saved state from the global file states store
    /// and applies it to both the EditorState (cursor) and SplitViewState (viewport).
    fn restore_global_file_state(&mut self, buffer_id: BufferId, path: &Path, split_id: SplitId) {
        use crate::session::PersistedFileSession;

        // Load the per-file session for this path (lazy load from disk)
        let file_state = match PersistedFileSession::load(path) {
            Some(state) => state,
            None => return, // No saved state for this file
        };

        // Get the buffer to validate positions
        let max_pos = match self.buffers.get(&buffer_id) {
            Some(buffer) => buffer.buffer.len(),
            None => return,
        };

        // Apply cursor position to EditorState (authoritative cursor)
        if let Some(editor_state) = self.buffers.get_mut(&buffer_id) {
            let cursor_pos = file_state.cursor.position.min(max_pos);
            editor_state.cursors.primary_mut().position = cursor_pos;
            editor_state.cursors.primary_mut().anchor =
                file_state.cursor.anchor.map(|a| a.min(max_pos));
        }

        // Apply viewport (scroll) state to SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            view_state.viewport.top_byte = file_state.scroll.top_byte;
            view_state.viewport.left_column = file_state.scroll.left_column;
        }
    }

    /// Save file state when a buffer is closed (for per-file session persistence)
    fn save_file_state_on_close(&self, buffer_id: BufferId) {
        use crate::session::{
            PersistedFileSession, SerializedCursor, SerializedFileState, SerializedScroll,
        };

        // Get the file path for this buffer
        let abs_path = match self.buffer_metadata.get(&buffer_id) {
            Some(metadata) => match metadata.file_path() {
                Some(path) => path.to_path_buf(),
                None => return, // Not a file buffer
            },
            None => return,
        };

        // Find a split that has this buffer open to get the view state
        let view_state = self
            .split_view_states
            .values()
            .find(|vs| vs.has_buffer(buffer_id));

        let view_state = match view_state {
            Some(vs) => vs,
            None => return, // No split has this buffer
        };

        // Capture the current state
        let primary_cursor = view_state.cursors.primary();
        let file_state = SerializedFileState {
            cursor: SerializedCursor {
                position: primary_cursor.position,
                anchor: primary_cursor.anchor,
                sticky_column: primary_cursor.sticky_column,
            },
            additional_cursors: view_state
                .cursors
                .iter()
                .skip(1)
                .map(|(_, cursor)| SerializedCursor {
                    position: cursor.position,
                    anchor: cursor.anchor,
                    sticky_column: cursor.sticky_column,
                })
                .collect(),
            scroll: SerializedScroll {
                top_byte: view_state.viewport.top_byte,
                top_view_line_offset: view_state.viewport.top_view_line_offset,
                left_column: view_state.viewport.left_column,
            },
        };

        // Save to disk
        PersistedFileSession::save(&abs_path, file_state);
        tracing::debug!("Saved file state on close for {:?}", abs_path);
    }

    /// Navigate to a specific line and column in the active buffer.
    ///
    /// Line and column are 1-indexed (matching typical editor conventions).
    /// If the line is out of bounds, navigates to the last line.
    /// If the column is out of bounds, navigates to the end of the line.
    pub fn goto_line_col(&mut self, line: usize, column: Option<usize>) {
        if line == 0 {
            return; // Line numbers are 1-indexed
        }

        let buffer_id = self.active_buffer();
        let estimated_line_length = self.config.editor.estimated_line_length;

        if let Some(state) = self.buffers.get(&buffer_id) {
            let cursor_id = state.cursors.primary_id();
            let old_position = state.cursors.primary().position;
            let old_anchor = state.cursors.primary().anchor;
            let old_sticky_column = state.cursors.primary().sticky_column;
            let is_large_file = state.buffer.line_count().is_none();
            let buffer_len = state.buffer.len();

            // Convert 1-indexed line to 0-indexed
            let target_line = line.saturating_sub(1);
            // Column is also 1-indexed, convert to 0-indexed
            let target_col = column.map(|c| c.saturating_sub(1)).unwrap_or(0);

            let position = if is_large_file {
                // Large file mode: estimate byte offset based on line number
                let estimated_offset = target_line * estimated_line_length;
                let clamped_offset = estimated_offset.min(buffer_len);

                // Use LineIterator to find the actual line start at the estimated position
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let iter = state
                        .buffer
                        .line_iterator(clamped_offset, estimated_line_length);
                    let line_start = iter.current_position();
                    // Add column offset, clamped to buffer length
                    (line_start + target_col).min(buffer_len)
                } else {
                    clamped_offset
                }
            } else {
                // Small file mode: use exact line position
                let max_line = state.buffer.line_count().unwrap_or(1).saturating_sub(1);
                let actual_line = target_line.min(max_line);
                state.buffer.line_col_to_position(actual_line, target_col)
            };

            let event = Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: position,
                old_anchor,
                new_anchor: None,
                old_sticky_column,
                new_sticky_column: target_col,
            };

            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.apply(&event);
            }
        }
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> BufferId {
        // Save current position before switching to new buffer
        self.position_history.commit_pending_movement();

        // Explicitly record current position before switching
        let current_state = self.active_state();
        let position = current_state.cursors.primary().position;
        let anchor = current_state.cursors.primary().anchor;
        self.position_history
            .record_movement(self.active_buffer(), position, anchor);
        self.position_history.commit_pending_movement();

        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
        );
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created
        state
            .margins
            .set_line_numbers(self.config.editor.line_numbers);
        // Set default line ending for new buffers from config
        state
            .buffer
            .set_default_line_ending(self.config.editor.default_line_ending.to_line_ending());
        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Apply line_wrap default from config to the active split
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
        }

        self.set_active_buffer(buffer_id);
        self.status_message = Some(t!("buffer.new").to_string());

        buffer_id
    }

    /// Create a new buffer from stdin content stored in a temp file
    ///
    /// Uses lazy chunk loading for efficient handling of large stdin inputs.
    /// The buffer is unnamed (no file path for save) - saving will prompt for a filename.
    /// The temp file path is preserved internally for lazy loading to work.
    ///
    /// # Arguments
    /// * `temp_path` - Path to temp file where stdin content is being written
    /// * `thread_handle` - Optional handle to background thread streaming stdin to temp file
    pub fn open_stdin_buffer(
        &mut self,
        temp_path: &Path,
        thread_handle: Option<std::thread::JoinHandle<std::io::Result<()>>>,
    ) -> io::Result<BufferId> {
        // Save current position before switching to new buffer
        self.position_history.commit_pending_movement();

        // Explicitly record current position before switching
        let current_state = self.active_state();
        let position = current_state.cursors.primary().position;
        let anchor = current_state.cursors.primary().anchor;
        self.position_history
            .record_movement(self.active_buffer(), position, anchor);
        self.position_history.commit_pending_movement();

        // If the current buffer is empty and unmodified, replace it instead of creating a new one
        let replace_current = {
            let current_state = self.buffers.get(&self.active_buffer()).unwrap();
            current_state.buffer.is_empty()
                && !current_state.buffer.is_modified()
                && current_state.buffer.file_path().is_none()
        };

        let buffer_id = if replace_current {
            // Reuse the current empty buffer
            self.active_buffer()
        } else {
            // Create new buffer ID
            let id = BufferId(self.next_buffer_id);
            self.next_buffer_id += 1;
            id
        };

        // Get file size for status message before loading
        let file_size = std::fs::metadata(temp_path)?.len() as usize;

        // Load from temp file using EditorState::from_file_with_languages
        // This enables lazy chunk loading for large inputs (>100MB by default)
        let mut state = EditorState::from_file_with_languages(
            temp_path,
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            &self.grammar_registry,
            &self.config.languages,
        )?;

        // Clear the file path so the buffer is "unnamed" for save purposes
        // The Unloaded chunks still reference the temp file for lazy loading
        state.buffer.clear_file_path();
        // Clear modified flag - content is "fresh" from stdin (vim behavior)
        state.buffer.clear_modified();

        // Set tab size from config
        state.tab_size = self.config.editor.tab_size;

        // Apply line_numbers default from config
        state
            .margins
            .set_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata for this buffer (no file path)
        let metadata = super::types::BufferMetadata::new_unnamed(t!("stdin.display_name").to_string());
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add buffer to the active split's tabs
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
            // Apply line_wrap default from config
            view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
        }

        self.set_active_buffer(buffer_id);

        // Set up stdin streaming state for polling
        // If no thread handle, it means data is already complete (testing scenario)
        let complete = thread_handle.is_none();
        self.stdin_streaming = Some(super::StdinStreamingState {
            temp_path: temp_path.to_path_buf(),
            buffer_id,
            last_known_size: file_size,
            complete,
            thread_handle,
        });

        // Status will be updated by poll_stdin_streaming
        self.status_message = Some(t!("stdin.streaming").to_string());

        Ok(buffer_id)
    }

    /// Poll stdin streaming state and extend buffer if file grew.
    /// Returns true if the status changed (needs render).
    pub fn poll_stdin_streaming(&mut self) -> bool {
        let Some(ref mut stream_state) = self.stdin_streaming else {
            return false;
        };

        if stream_state.complete {
            return false;
        }

        let mut changed = false;

        // Check current file size
        let current_size = std::fs::metadata(&stream_state.temp_path)
            .map(|m| m.len() as usize)
            .unwrap_or(stream_state.last_known_size);

        // If file grew, extend the buffer
        if current_size > stream_state.last_known_size {
            if let Some(editor_state) = self.buffers.get_mut(&stream_state.buffer_id) {
                editor_state
                    .buffer
                    .extend_streaming(&stream_state.temp_path, current_size);
            }
            stream_state.last_known_size = current_size;

            // Update status message with current progress
            self.status_message = Some(t!("stdin.streaming_bytes", bytes = current_size).to_string());
            changed = true;
        }

        // Check if background thread has finished
        let thread_finished = stream_state
            .thread_handle
            .as_ref()
            .map(|h| h.is_finished())
            .unwrap_or(true);

        if thread_finished {
            // Take ownership of handle to join it
            if let Some(handle) = stream_state.thread_handle.take() {
                match handle.join() {
                    Ok(Ok(())) => {
                        tracing::info!("Stdin streaming completed successfully");
                    }
                    Ok(Err(e)) => {
                        tracing::warn!("Stdin streaming error: {}", e);
                        self.status_message = Some(t!("stdin.read_error", error = e.to_string()).to_string());
                    }
                    Err(_) => {
                        tracing::warn!("Stdin streaming thread panicked");
                        self.status_message = Some(t!("stdin.read_error_panic").to_string());
                    }
                }
            }
            self.complete_stdin_streaming();
            changed = true;
        }

        changed
    }

    /// Mark stdin streaming as complete.
    /// Called when the background thread finishes.
    pub fn complete_stdin_streaming(&mut self) {
        if let Some(ref mut stream_state) = self.stdin_streaming {
            stream_state.complete = true;

            // Final poll to get any remaining data
            let final_size = std::fs::metadata(&stream_state.temp_path)
                .map(|m| m.len() as usize)
                .unwrap_or(stream_state.last_known_size);

            if final_size > stream_state.last_known_size {
                if let Some(editor_state) = self.buffers.get_mut(&stream_state.buffer_id) {
                    editor_state
                        .buffer
                        .extend_streaming(&stream_state.temp_path, final_size);
                }
                stream_state.last_known_size = final_size;
            }

            self.status_message = Some(t!("stdin.read_complete", bytes = stream_state.last_known_size).to_string());
        }
    }

    /// Check if stdin streaming is active (not complete).
    pub fn is_stdin_streaming(&self) -> bool {
        self.stdin_streaming
            .as_ref()
            .map(|s| !s.complete)
            .unwrap_or(false)
    }

    /// Create a new virtual buffer (not backed by a file)
    ///
    /// # Arguments
    /// * `name` - Display name (e.g., "*Diagnostics*")
    /// * `mode` - Buffer mode for keybindings (e.g., "diagnostics-list")
    /// * `read_only` - Whether the buffer should be read-only
    ///
    /// # Returns
    /// The BufferId of the created virtual buffer
    pub fn create_virtual_buffer(
        &mut self,
        name: String,
        mode: String,
        read_only: bool,
    ) -> BufferId {
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
        );
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created

        // Set syntax highlighting based on buffer name (e.g., "*OURS*.c" will get C highlighting)
        state.set_language_from_name(&name, &self.grammar_registry);

        // Apply line_numbers default from config
        state
            .margins
            .set_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Set virtual buffer metadata
        let metadata = super::types::BufferMetadata::virtual_buffer(name, mode, read_only);
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add buffer to the active split's open_buffers (tabs)
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
        } else {
            // Create view state if it doesn't exist
            let mut view_state =
                SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id);
            view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            self.split_view_states.insert(active_split, view_state);
        }

        buffer_id
    }

    /// Set the content of a virtual buffer with text properties
    ///
    /// # Arguments
    /// * `buffer_id` - The virtual buffer to update
    /// * `entries` - Text entries with embedded properties
    pub fn set_virtual_buffer_content(
        &mut self,
        buffer_id: BufferId,
        entries: Vec<crate::primitives::text_property::TextPropertyEntry>,
    ) -> Result<(), String> {
        let state = self
            .buffers
            .get_mut(&buffer_id)
            .ok_or_else(|| "Buffer not found".to_string())?;

        // Save current cursor position to preserve it after content update
        let old_cursor_pos = state.cursors.primary().position;

        // Build text and properties from entries
        let (text, properties) =
            crate::primitives::text_property::TextPropertyManager::from_entries(entries);

        // Replace buffer content
        let current_len = state.buffer.len();
        if current_len > 0 {
            state.buffer.delete_bytes(0, current_len);
        }
        state.buffer.insert(0, &text);

        // Clear modified flag since this is virtual buffer content setting, not user edits
        state.buffer.clear_modified();

        // Set text properties
        state.text_properties = properties;

        // Preserve cursor position (clamped to new content length and snapped to char boundary)
        let new_len = state.buffer.len();
        let clamped_pos = old_cursor_pos.min(new_len);
        // Ensure cursor is at a valid UTF-8 character boundary (without moving if already valid)
        let new_cursor_pos = state.buffer.snap_to_char_boundary(clamped_pos);
        state.cursors.primary_mut().position = new_cursor_pos;
        state.cursors.primary_mut().anchor = None;

        Ok(())
    }

    /// Open the built-in help manual in a read-only buffer
    ///
    /// If a help manual buffer already exists, switch to it instead of creating a new one.
    pub fn open_help_manual(&mut self) {
        // Check if help buffer already exists
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::HELP_MANUAL_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            // Switch to existing help buffer
            self.set_active_buffer(buffer_id);
            return;
        }

        // Create new help buffer with "special" mode (has 'q' to close)
        let buffer_id = self.create_virtual_buffer(
            help::HELP_MANUAL_BUFFER_NAME.to_string(),
            "special".to_string(),
            true,
        );

        // Set the content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, help::HELP_MANUAL_CONTENT);
            state.buffer.clear_modified();
            state.editing_disabled = true;

            // Disable line numbers for cleaner display
            state.margins.set_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }

    /// Open the keyboard shortcuts viewer in a read-only buffer
    ///
    /// If a keyboard shortcuts buffer already exists, switch to it instead of creating a new one.
    /// The shortcuts are dynamically generated from the current keybindings configuration.
    pub fn open_keyboard_shortcuts(&mut self) {
        // Check if keyboard shortcuts buffer already exists
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::KEYBOARD_SHORTCUTS_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            // Switch to existing buffer
            self.set_active_buffer(buffer_id);
            return;
        }

        // Get all keybindings
        let bindings = self.keybindings.get_all_bindings();

        // Format the keybindings as readable text
        let mut content = String::from("Keyboard Shortcuts\n");
        content.push_str("==================\n\n");
        content.push_str("Press 'q' to close this buffer.\n\n");

        // Group bindings by context (Normal, Prompt, etc.)
        let mut current_context = String::new();
        for (key, action) in &bindings {
            // Check if action starts with a context prefix like "[Prompt] "
            let (context, action_name) = if let Some(bracket_end) = action.find("] ") {
                let ctx = &action[1..bracket_end];
                let name = &action[bracket_end + 2..];
                (ctx.to_string(), name.to_string())
            } else {
                ("Normal".to_string(), action.clone())
            };

            // Print context header when it changes
            if context != current_context {
                if !current_context.is_empty() {
                    content.push('\n');
                }
                content.push_str(&format!("── {} Mode ──\n\n", context));
                current_context = context;
            }

            // Format: "  Ctrl+S          Save"
            content.push_str(&format!("  {:20} {}\n", key, action_name));
        }

        // Create new keyboard shortcuts buffer with "special" mode (has 'q' to close)
        let buffer_id = self.create_virtual_buffer(
            help::KEYBOARD_SHORTCUTS_BUFFER_NAME.to_string(),
            "special".to_string(),
            true,
        );

        // Set the content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, &content);
            state.buffer.clear_modified();
            state.editing_disabled = true;

            // Disable line numbers for cleaner display
            state.margins.set_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }

    /// Show warnings by opening the warning log file directly
    ///
    /// If there are no warnings, shows a brief status message.
    /// Otherwise, opens the warning log file for the user to view.
    pub fn show_warnings_popup(&mut self) {
        if !self.warning_domains.has_any_warnings() {
            self.status_message = Some(t!("warnings.none").to_string());
            return;
        }

        // Open the warning log file directly
        self.open_warning_log();
    }

    /// Show LSP status - opens the warning log file if there are LSP warnings,
    /// otherwise shows a brief status message.
    pub fn show_lsp_status_popup(&mut self) {
        let has_error = self.warning_domains.lsp.level() == crate::app::WarningLevel::Error;

        // Use the language from the LSP error state if available, otherwise detect from buffer.
        // This ensures clicking the status indicator works regardless of which buffer is focused.
        let language = self
            .warning_domains
            .lsp
            .language
            .clone()
            .unwrap_or_else(|| {
                self.buffer_metadata
                    .get(&self.active_buffer())
                    .and_then(|m| m.file_path())
                    .and_then(|path| detect_language(path, &self.config.languages))
                    .unwrap_or_else(|| "unknown".to_string())
            });

        tracing::info!(
            "show_lsp_status_popup: language={}, has_error={}, has_warnings={}",
            language,
            has_error,
            self.warning_domains.lsp.has_warnings()
        );

        // Fire the LspStatusClicked hook for plugins
        self.plugin_manager.run_hook(
            "lsp_status_clicked",
            crate::services::plugins::hooks::HookArgs::LspStatusClicked {
                language: language.clone(),
                has_error,
            },
        );
        tracing::info!("show_lsp_status_popup: hook fired");

        if !self.warning_domains.lsp.has_warnings() {
            if self.lsp_status.is_empty() {
                self.status_message = Some(t!("lsp.no_server_active").to_string());
            } else {
                self.status_message = Some(t!("lsp.status", status = &self.lsp_status).to_string());
            }
            return;
        }

        // If there's an LSP error AND a plugin is handling the status click, don't open the
        // warning log which would switch focus and break language detection for subsequent clicks.
        // Only suppress if a plugin has registered to handle the hook.
        if has_error && self.plugin_manager.has_hook_handlers("lsp_status_clicked") {
            tracing::info!(
                "show_lsp_status_popup: has_error=true and plugin registered, skipping warning log"
            );
            return;
        }

        // Open the warning log file directly (same as warnings popup)
        self.open_warning_log();
    }

    /// Get text properties at the cursor position in the active buffer
    pub fn get_text_properties_at_cursor(
        &self,
    ) -> Option<Vec<&crate::primitives::text_property::TextProperty>> {
        let state = self.buffers.get(&self.active_buffer())?;
        let cursor_pos = state.cursors.primary().position;
        Some(state.text_properties.get_at(cursor_pos))
    }

    /// Close the given buffer
    pub fn close_buffer(&mut self, id: BufferId) -> io::Result<()> {
        // Check for unsaved changes
        if let Some(state) = self.buffers.get(&id) {
            if state.buffer.is_modified() {
                return Err(io::Error::other("Buffer has unsaved changes"));
            }
        }
        self.close_buffer_internal(id)
    }

    /// Force close the given buffer without checking for unsaved changes
    /// Use this when the user has already confirmed they want to discard changes
    pub fn force_close_buffer(&mut self, id: BufferId) -> io::Result<()> {
        self.close_buffer_internal(id)
    }

    /// Internal helper to close a buffer (shared by close_buffer and force_close_buffer)
    fn close_buffer_internal(&mut self, id: BufferId) -> io::Result<()> {
        // Save file state before closing (for per-file session persistence)
        self.save_file_state_on_close(id);

        // If closing a terminal buffer while in terminal mode, exit terminal mode
        if self.terminal_mode && self.is_terminal_buffer(id) {
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // If it's the last buffer, create a new empty buffer and focus file explorer
        let is_last_buffer = self.buffers.len() == 1;
        let replacement_buffer = if is_last_buffer {
            self.new_buffer()
        } else {
            // Find a replacement buffer (any buffer that's not the one being closed)
            *self.buffers.keys().find(|&&bid| bid != id).unwrap()
        };

        // Update all splits that are showing this buffer to show the replacement
        let splits_to_update = self.split_manager.splits_for_buffer(id);
        for split_id in splits_to_update {
            let _ = self
                .split_manager
                .set_split_buffer(split_id, replacement_buffer);
        }

        self.buffers.remove(&id);
        self.event_logs.remove(&id);
        self.seen_byte_ranges.remove(&id);
        self.buffer_metadata.remove(&id);

        // Remove buffer from panel_ids mapping if it was a panel buffer
        // This prevents stale entries when the same panel_id is reused later
        self.panel_ids.retain(|_, &mut buf_id| buf_id != id);

        // Remove buffer from all splits' open_buffers lists
        for view_state in self.split_view_states.values_mut() {
            view_state.remove_buffer(id);
        }

        // Switch to another buffer if we closed the active one
        if self.active_buffer() == id {
            self.set_active_buffer(replacement_buffer);
        }

        // If this was the last buffer, focus file explorer
        if is_last_buffer {
            self.focus_file_explorer();
        }

        Ok(())
    }

    /// Switch to the given buffer
    pub fn switch_buffer(&mut self, id: BufferId) {
        if self.buffers.contains_key(&id) && id != self.active_buffer() {
            // Save current position before switching buffers
            self.position_history.commit_pending_movement();

            // Also explicitly record current position (in case there was no pending movement)
            let current_state = self.active_state();
            let position = current_state.cursors.primary().position;
            let anchor = current_state.cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();

            self.set_active_buffer(id);
        }
    }

    /// Close the current tab in the current split view.
    /// If the tab is the last viewport of the underlying buffer, do the same as close_buffer
    /// (including triggering the save/discard prompt for modified buffers).
    pub fn close_tab(&mut self) {
        let buffer_id = self.active_buffer();
        let active_split = self.split_manager.active_split();

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .split_view_states
            .iter()
            .filter(|(&split_id, view_state)| {
                split_id != active_split && view_state.has_buffer(buffer_id)
            })
            .count();

        // Get current split's open buffers
        let current_split_tabs = self
            .split_view_states
            .get(&active_split)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        // If this is the only tab in this split and there are no other splits with this buffer,
        // this is the last viewport - behave like close_buffer
        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // Last viewport of this buffer - close the buffer entirely
            if self.active_state().buffer.is_modified() {
                // Buffer has unsaved changes - prompt for confirmation
                let name = self.get_buffer_display_name(buffer_id);
                let save_key = t!("prompt.key.save").to_string();
                let discard_key = t!("prompt.key.discard").to_string();
                let cancel_key = t!("prompt.key.cancel").to_string();
                self.start_prompt(
                    t!(
                        "prompt.buffer_modified",
                        name = name,
                        save_key = save_key,
                        discard_key = discard_key,
                        cancel_key = cancel_key
                    )
                    .to_string(),
                    PromptType::ConfirmCloseBuffer { buffer_id },
                );
            } else if let Err(e) = self.close_buffer(buffer_id) {
                self.set_status_message(t!("file.cannot_close", error = e.to_string()).to_string());
            } else {
                self.set_status_message(t!("buffer.tab_closed").to_string());
            }
        } else {
            // There are other viewports of this buffer - just remove from current split's tabs
            if current_split_tabs.len() <= 1 {
                // This is the only tab in this split - close the split
                self.close_active_split();
                return;
            }

            // Find replacement buffer for this split
            let current_idx = current_split_tabs
                .iter()
                .position(|&id| id == buffer_id)
                .unwrap_or(0);
            let replacement_idx = if current_idx > 0 { current_idx - 1 } else { 1 };
            let replacement_buffer = current_split_tabs[replacement_idx];

            // Remove buffer from this split's tabs
            if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                view_state.remove_buffer(buffer_id);
            }

            // Update the split to show the replacement buffer
            let _ = self
                .split_manager
                .set_split_buffer(active_split, replacement_buffer);

            self.set_status_message(t!("buffer.tab_closed").to_string());
        }
    }

    /// Close a specific tab (buffer) in a specific split.
    /// Used by mouse click handler on tab close button.
    /// Returns true if the tab was closed without needing a prompt.
    pub fn close_tab_in_split(&mut self, buffer_id: BufferId, split_id: SplitId) -> bool {
        // If closing a terminal buffer while in terminal mode, exit terminal mode
        if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .split_view_states
            .iter()
            .filter(|(&sid, view_state)| sid != split_id && view_state.has_buffer(buffer_id))
            .count();

        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // Last viewport of this buffer - need to close buffer entirely
            if let Some(state) = self.buffers.get(&buffer_id) {
                if state.buffer.is_modified() {
                    // Buffer has unsaved changes - prompt for confirmation
                    let name = self.get_buffer_display_name(buffer_id);
                    let save_key = t!("prompt.key.save").to_string();
                    let discard_key = t!("prompt.key.discard").to_string();
                    let cancel_key = t!("prompt.key.cancel").to_string();
                    self.start_prompt(
                        t!(
                            "prompt.buffer_modified",
                            name = name,
                            save_key = save_key,
                            discard_key = discard_key,
                            cancel_key = cancel_key
                        )
                        .to_string(),
                        PromptType::ConfirmCloseBuffer { buffer_id },
                    );
                    return false;
                }
            }
            if let Err(e) = self.close_buffer(buffer_id) {
                self.set_status_message(t!("file.cannot_close", error = e.to_string()).to_string());
            } else {
                self.set_status_message(t!("buffer.tab_closed").to_string());
            }
        } else {
            // There are other viewports of this buffer - just remove from this split's tabs
            if split_tabs.len() <= 1 {
                // This is the only tab in this split - close the split
                self.handle_close_split(split_id);
                return true;
            }

            // Find replacement buffer for this split
            let current_idx = split_tabs
                .iter()
                .position(|&id| id == buffer_id)
                .unwrap_or(0);
            let replacement_idx = if current_idx > 0 { current_idx - 1 } else { 1 };
            let replacement_buffer = split_tabs[replacement_idx];

            // Remove buffer from this split's tabs
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.remove_buffer(buffer_id);
            }

            // Update the split to show the replacement buffer
            let _ = self
                .split_manager
                .set_split_buffer(split_id, replacement_buffer);

            self.set_status_message(t!("buffer.tab_closed").to_string());
        }
        true
    }

    /// Close all other tabs in a split, keeping only the specified buffer
    pub fn close_other_tabs_in_split(&mut self, keep_buffer_id: BufferId, split_id: SplitId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        // Close all tabs except the one we want to keep
        let tabs_to_close: Vec<_> = split_tabs
            .iter()
            .filter(|&&id| id != keep_buffer_id)
            .copied()
            .collect();

        let mut closed = 0;
        let mut skipped_modified = 0;
        for buffer_id in tabs_to_close {
            if self.close_tab_in_split_silent(buffer_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        // Make sure the kept buffer is active
        let _ = self
            .split_manager
            .set_split_buffer(split_id, keep_buffer_id);

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close tabs to the right of the specified buffer in a split
    pub fn close_tabs_to_right_in_split(&mut self, buffer_id: BufferId, split_id: SplitId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        // Find the index of the target buffer
        let Some(target_idx) = split_tabs.iter().position(|&id| id == buffer_id) else {
            return;
        };

        // Close all tabs after the target
        let tabs_to_close: Vec<_> = split_tabs.iter().skip(target_idx + 1).copied().collect();

        let mut closed = 0;
        let mut skipped_modified = 0;
        for buf_id in tabs_to_close {
            if self.close_tab_in_split_silent(buf_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close tabs to the left of the specified buffer in a split
    pub fn close_tabs_to_left_in_split(&mut self, buffer_id: BufferId, split_id: SplitId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        // Find the index of the target buffer
        let Some(target_idx) = split_tabs.iter().position(|&id| id == buffer_id) else {
            return;
        };

        // Close all tabs before the target
        let tabs_to_close: Vec<_> = split_tabs.iter().take(target_idx).copied().collect();

        let mut closed = 0;
        let mut skipped_modified = 0;
        for buf_id in tabs_to_close {
            if self.close_tab_in_split_silent(buf_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close all tabs in a split
    pub fn close_all_tabs_in_split(&mut self, split_id: SplitId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        let mut closed = 0;
        let mut skipped_modified = 0;

        // Close all tabs (this will eventually close the split when empty)
        for buffer_id in split_tabs {
            if self.close_tab_in_split_silent(buffer_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Set status message for batch close operations
    fn set_batch_close_status_message(&mut self, closed: usize, skipped_modified: usize) {
        let message = match (closed, skipped_modified) {
            (0, 0) => t!("buffer.no_tabs_to_close").to_string(),
            (0, n) => t!("buffer.skipped_modified", count = n).to_string(),
            (n, 0) => t!("buffer.closed_tabs", count = n).to_string(),
            (c, s) => t!("buffer.closed_tabs_skipped", closed = c, skipped = s).to_string(),
        };
        self.set_status_message(message);
    }

    /// Close a tab silently (without setting status message)
    /// Used internally by batch close operations
    /// Returns true if the tab was closed, false if it was skipped (e.g., modified buffer)
    fn close_tab_in_split_silent(&mut self, buffer_id: BufferId, split_id: SplitId) -> bool {
        // If closing a terminal buffer while in terminal mode, exit terminal mode
        if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .split_view_states
            .iter()
            .filter(|(&sid, view_state)| sid != split_id && view_state.has_buffer(buffer_id))
            .count();

        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // Last viewport of this buffer - need to close buffer entirely
            // Skip modified buffers to avoid prompting during batch operations
            if let Some(state) = self.buffers.get(&buffer_id) {
                if state.buffer.is_modified() {
                    // Skip modified buffers - don't close them
                    return false;
                }
            }
            let _ = self.close_buffer(buffer_id);
            true
        } else {
            // There are other viewports of this buffer - just remove from this split's tabs
            if split_tabs.len() <= 1 {
                // This is the only tab in this split - close the split
                self.handle_close_split(split_id);
                return true;
            }

            // Find replacement buffer for this split
            let current_idx = split_tabs
                .iter()
                .position(|&id| id == buffer_id)
                .unwrap_or(0);
            let replacement_idx = if current_idx > 0 { current_idx - 1 } else { 1 };
            let replacement_buffer = split_tabs.get(replacement_idx).copied();

            // Remove buffer from this split's tabs
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.remove_buffer(buffer_id);
            }

            // Update the split to show the replacement buffer
            if let Some(replacement) = replacement_buffer {
                let _ = self.split_manager.set_split_buffer(split_id, replacement);
            }
            true
        }
    }

    /// Switch to next buffer in current split's tabs
    pub fn next_buffer(&mut self) {
        // Get the current split's open buffers
        let active_split = self.split_manager.active_split();
        let ids = if let Some(view_state) = self.split_view_states.get(&active_split) {
            view_state.open_buffers.clone()
        } else {
            // Fallback to all buffers if no view state
            let mut all_ids: Vec<_> = self.buffers.keys().copied().collect();
            all_ids.sort_by_key(|id| id.0);
            all_ids
        };

        if ids.is_empty() {
            return;
        }

        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer()) {
            let next_idx = (idx + 1) % ids.len();
            if ids[next_idx] != self.active_buffer() {
                // Save current position before switching
                self.position_history.commit_pending_movement();

                // Also explicitly record current position
                let current_state = self.active_state();
                let position = current_state.cursors.primary().position;
                let anchor = current_state.cursors.primary().anchor;
                self.position_history
                    .record_movement(self.active_buffer(), position, anchor);
                self.position_history.commit_pending_movement();

                self.set_active_buffer(ids[next_idx]);
            }
        }
    }

    /// Switch to previous buffer in current split's tabs
    pub fn prev_buffer(&mut self) {
        // Get the current split's open buffers
        let active_split = self.split_manager.active_split();
        let ids = if let Some(view_state) = self.split_view_states.get(&active_split) {
            view_state.open_buffers.clone()
        } else {
            // Fallback to all buffers if no view state
            let mut all_ids: Vec<_> = self.buffers.keys().copied().collect();
            all_ids.sort_by_key(|id| id.0);
            all_ids
        };

        if ids.is_empty() {
            return;
        }

        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer()) {
            let prev_idx = if idx == 0 { ids.len() - 1 } else { idx - 1 };
            if ids[prev_idx] != self.active_buffer() {
                // Save current position before switching
                self.position_history.commit_pending_movement();

                // Also explicitly record current position
                let current_state = self.active_state();
                let position = current_state.cursors.primary().position;
                let anchor = current_state.cursors.primary().anchor;
                self.position_history
                    .record_movement(self.active_buffer(), position, anchor);
                self.position_history.commit_pending_movement();

                self.set_active_buffer(ids[prev_idx]);
            }
        }
    }

    /// Navigate back in position history
    pub fn navigate_back(&mut self) {
        // Set flag to prevent recording this navigation movement
        self.in_navigation = true;

        // Commit any pending movement
        self.position_history.commit_pending_movement();

        // If we're at the end of history (haven't used back yet), save current position
        // so we can navigate forward to it later
        if self.position_history.can_go_back() && !self.position_history.can_go_forward() {
            let current_state = self.active_state();
            let position = current_state.cursors.primary().position;
            let anchor = current_state.cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();
        }

        // Navigate to the previous position
        if let Some(entry) = self.position_history.back() {
            let target_buffer = entry.buffer_id;
            let target_position = entry.position;
            let target_anchor = entry.anchor;

            // Switch to the target buffer
            if self.buffers.contains_key(&target_buffer) {
                self.set_active_buffer(target_buffer);

                // Move cursor to the saved position
                let state = self.active_state_mut();
                let cursor_id = state.cursors.primary_id();
                let old_position = state.cursors.primary().position;
                let old_anchor = state.cursors.primary().anchor;
                let old_sticky_column = state.cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                state.apply(&event);
            }
        }

        // Clear the flag
        self.in_navigation = false;
    }

    /// Navigate forward in position history
    pub fn navigate_forward(&mut self) {
        // Set flag to prevent recording this navigation movement
        self.in_navigation = true;

        if let Some(entry) = self.position_history.forward() {
            let target_buffer = entry.buffer_id;
            let target_position = entry.position;
            let target_anchor = entry.anchor;

            // Switch to the target buffer
            if self.buffers.contains_key(&target_buffer) {
                self.set_active_buffer(target_buffer);

                // Move cursor to the saved position
                let state = self.active_state_mut();
                let cursor_id = state.cursors.primary_id();
                let old_position = state.cursors.primary().position;
                let old_anchor = state.cursors.primary().anchor;
                let old_sticky_column = state.cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                state.apply(&event);
            }
        }

        // Clear the flag
        self.in_navigation = false;
    }

    /// Get the current mouse hover state for testing
    /// Returns Some((byte_position, screen_x, screen_y)) if hovering over text
    pub fn get_mouse_hover_state(&self) -> Option<(usize, u16, u16)> {
        self.mouse_state
            .lsp_hover_state
            .map(|(pos, _, x, y)| (pos, x, y))
    }

    /// Check if a transient popup (hover/signature help) is currently visible
    pub fn has_transient_popup(&self) -> bool {
        self.active_state()
            .popups
            .top()
            .is_some_and(|p| p.transient)
    }

    /// Force check the mouse hover timer (for testing)
    /// This bypasses the normal 500ms delay
    pub fn force_check_mouse_hover(&mut self) -> bool {
        // Temporarily mark the hover as ready by checking if state exists
        if let Some((byte_pos, _, screen_x, screen_y)) = self.mouse_state.lsp_hover_state {
            if !self.mouse_state.lsp_hover_request_sent {
                self.mouse_state.lsp_hover_request_sent = true;
                self.mouse_hover_screen_position = Some((screen_x, screen_y));
                if let Err(e) = self.request_hover_at_position(byte_pos) {
                    tracing::debug!("Failed to request hover: {}", e);
                    return false;
                }
                return true;
            }
        }
        false
    }
}
