//! Buffer management operations for the Editor.
//!
//! This module contains all methods related to buffer lifecycle and navigation:
//! - Opening files (with and without focus)
//! - Creating new buffers (regular and virtual)
//! - Closing buffers and tabs
//! - Switching between buffers
//! - Navigate back/forward in position history
//! - Buffer state persistence

use anyhow::Result as AnyhowResult;
use rust_i18n::t;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::app::warning_domains::WarningDomain;
use crate::model::event::{BufferId, Event, SplitId};
use crate::state::EditorState;
use crate::view::prompt::PromptType;
use crate::view::split::SplitViewState;

use super::help;
use super::Editor;

impl Editor {
    /// Get the preferred split for opening a file.
    /// If the active split has no label, use it (normal case).
    /// Otherwise find an unlabeled leaf so files don't open in labeled splits (e.g., sidebars).
    fn preferred_split_for_file(&self) -> SplitId {
        let active = self.split_manager.active_split();
        if self.split_manager.get_label(active).is_none() {
            return active;
        }
        self.split_manager.find_unlabeled_leaf().unwrap_or(active)
    }

    /// Open a file and return its buffer ID
    ///
    /// If the file doesn't exist, creates an unsaved buffer with that filename.
    /// Saving the buffer will create the file.
    pub fn open_file(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        let buffer_id = self.open_file_no_focus(path)?;

        // Check if this was an already-open buffer or a new one
        // For already-open buffers, just switch to them
        // For new buffers, record position history before switching
        let is_new_buffer = self.active_buffer() != buffer_id;

        if is_new_buffer {
            // Save current position before switching to new buffer
            self.position_history.commit_pending_movement();

            // Explicitly record current position before switching
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
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
    pub fn open_file_no_focus(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Resolve relative paths against appropriate base directory
        // For remote mode, use the remote home directory; for local, use working_dir
        let base_dir = if self.filesystem.remote_connection_info().is_some() {
            self.filesystem
                .home_dir()
                .unwrap_or_else(|_| self.working_dir.clone())
        } else {
            self.working_dir.clone()
        };

        let resolved_path = if path.is_relative() {
            base_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Determine if we're opening a non-existent file (for creating new files)
        // Use filesystem trait method to support remote files
        let file_exists = self.filesystem.exists(&resolved_path);

        // Canonicalize the path to resolve symlinks and normalize path components
        // This ensures consistent path representation throughout the editor
        // For non-existent files, we need to canonicalize the parent directory and append the filename
        let canonical_path = if file_exists {
            self.filesystem
                .canonicalize(&resolved_path)
                .unwrap_or_else(|_| resolved_path.clone())
        } else {
            // For non-existent files, canonicalize parent dir and append filename
            if let Some(parent) = resolved_path.parent() {
                let canonical_parent = if parent.as_os_str().is_empty() {
                    // No parent means just a filename, use base dir
                    base_dir.clone()
                } else {
                    self.filesystem
                        .canonicalize(parent)
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

        // Check if the path is a directory (after following symlinks via canonicalize)
        // Directories cannot be opened as files in the editor
        // Use filesystem trait method to support remote files
        if self.filesystem.is_dir(path).unwrap_or(false) {
            anyhow::bail!(t!("buffer.cannot_open_directory"));
        }

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
        // Note: Don't replace composite buffers (they appear empty but are special views)
        let replace_current = {
            let current_state = self.buffers.get(&self.active_buffer()).unwrap();
            !current_state.is_composite_buffer
                && current_state.buffer.is_empty()
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
        tracing::info!(
            "[SYNTAX DEBUG] open_file_no_focus: path={:?}, extension={:?}, registry_syntaxes={}, user_extensions={:?}",
            path,
            path.extension(),
            self.grammar_registry.available_syntaxes().len(),
            self.grammar_registry.user_extensions_debug()
        );
        let mut state = if file_exists {
            EditorState::from_file_with_languages(
                path,
                self.terminal_width,
                self.terminal_height,
                self.config.editor.large_file_threshold_bytes as usize,
                &self.grammar_registry,
                &self.config.languages,
                Arc::clone(&self.filesystem),
            )?
        } else {
            // File doesn't exist - create empty buffer with the file path set
            let mut new_state = EditorState::new(
                self.terminal_width,
                self.terminal_height,
                self.config.editor.large_file_threshold_bytes as usize,
                Arc::clone(&self.filesystem),
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
        // Use the buffer's stored language (already set by from_file_with_languages)
        if let Some(lang_config) = self.config.languages.get(&state.language) {
            state.buffer_settings.show_whitespace_tabs = lang_config.show_whitespace_tabs;
            state.buffer_settings.use_tabs = lang_config.use_tabs;
            // Use language-specific tab_size if set, otherwise fall back to global
            state.buffer_settings.tab_size =
                lang_config.tab_size.unwrap_or(self.config.editor.tab_size);
        } else {
            state.buffer_settings.tab_size = self.config.editor.tab_size;
        }

        // Apply line_numbers default from config
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

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

        // Add buffer to the preferred split's tabs (but don't switch to it)
        // Uses preferred_split_for_file() to avoid opening in labeled splits (e.g., sidebars)
        let target_split = self.preferred_split_for_file();
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            // Initialize per-buffer view state for the new buffer with config defaults
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            buf_state.rulers = self.config.editor.rulers.clone();
        }

        // Restore global file state (scroll/cursor position) if available
        // This persists file positions across projects and editor instances
        self.restore_global_file_state(buffer_id, path, target_split);

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

    /// Open a local file (always uses local filesystem, not remote)
    ///
    /// This is used for opening local files like log files when in remote mode.
    /// Unlike `open_file`, this always uses the local filesystem even when
    /// the editor is connected to a remote server.
    pub fn open_local_file(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Resolve relative paths against working_dir
        let resolved_path = if path.is_relative() {
            self.working_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Canonicalize the path
        let canonical_path = resolved_path
            .canonicalize()
            .unwrap_or_else(|_| resolved_path.clone());
        let path = canonical_path.as_path();

        // Check if already open
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            self.set_active_buffer(id);
            return Ok(id);
        }

        // Create new buffer
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        // Create editor state using LOCAL filesystem
        let state = EditorState::from_file_with_languages(
            path,
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            &self.grammar_registry,
            &self.config.languages,
            Arc::clone(&self.local_filesystem),
        )?;

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata
        let metadata =
            super::types::BufferMetadata::with_file(path.to_path_buf(), &self.working_dir);
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.preferred_split_for_file();
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            buf_state.rulers = self.config.editor.rulers.clone();
        }

        self.set_active_buffer(buffer_id);

        let display_name = path.display().to_string();
        self.status_message = Some(t!("buffer.opened", name = display_name).to_string());

        Ok(buffer_id)
    }

    /// Open a file with a specific encoding (no auto-detection).
    ///
    /// Used when the user disables auto-detection in the file browser
    /// and selects a specific encoding to use.
    pub fn open_file_with_encoding(
        &mut self,
        path: &Path,
        encoding: crate::model::buffer::Encoding,
    ) -> anyhow::Result<BufferId> {
        // Use the same base directory logic as open_file
        let base_dir = self.working_dir.clone();

        let resolved_path = if path.is_relative() {
            base_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Canonicalize the path
        let canonical_path = self
            .filesystem
            .canonicalize(&resolved_path)
            .unwrap_or_else(|_| resolved_path.clone());
        let path = canonical_path.as_path();

        // Check if already open
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            // File is already open - update its encoding and reload
            if let Some(state) = self.buffers.get_mut(&id) {
                state.buffer.set_encoding(encoding);
            }
            self.set_active_buffer(id);
            return Ok(id);
        }

        // Create new buffer with specified encoding
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        // Load buffer with the specified encoding
        let buffer = crate::model::buffer::Buffer::load_from_file_with_encoding(
            path,
            encoding,
            Arc::clone(&self.filesystem),
        )?;

        // Create editor state with the buffer
        let highlighter =
            crate::primitives::highlight_engine::HighlightEngine::for_file_with_languages(
                path,
                &self.grammar_registry,
                &self.config.languages,
            );

        let language = crate::primitives::highlighter::Language::from_path(path);
        let language_name = if let Some(lang) = &language {
            lang.to_string()
        } else {
            crate::services::lsp::manager::detect_language(path, &self.config.languages)
                .unwrap_or_else(|| "text".to_string())
        };

        let mut state =
            EditorState::from_buffer_with_highlighter(buffer, highlighter, language_name, language);

        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let metadata =
            super::types::BufferMetadata::with_file(path.to_path_buf(), &self.working_dir);
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.preferred_split_for_file();
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            buf_state.rulers = self.config.editor.rulers.clone();
        }

        self.set_active_buffer(buffer_id);

        Ok(buffer_id)
    }

    /// Reload the current file with a specific encoding.
    ///
    /// Requires the buffer to have no unsaved modifications.
    pub fn reload_with_encoding(
        &mut self,
        encoding: crate::model::buffer::Encoding,
    ) -> anyhow::Result<()> {
        let buffer_id = self.active_buffer();

        // Get the file path
        let path = self
            .buffers
            .get(&buffer_id)
            .and_then(|s| s.buffer.file_path().map(|p| p.to_path_buf()))
            .ok_or_else(|| anyhow::anyhow!("Buffer has no file path"))?;

        // Check for unsaved modifications
        if let Some(state) = self.buffers.get(&buffer_id) {
            if state.buffer.is_modified() {
                anyhow::bail!("Cannot reload: buffer has unsaved modifications");
            }
        }

        // Reload the buffer with the new encoding
        let new_buffer = crate::model::buffer::Buffer::load_from_file_with_encoding(
            &path,
            encoding,
            Arc::clone(&self.filesystem),
        )?;

        // Update the buffer in the editor state
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer = new_buffer;
            // Invalidate highlighting
            state.highlighter.invalidate_all();
        }

        // Reset cursor to start in the split view state
        let split_id = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                buf_state.cursors = crate::model::cursor::Cursors::new();
            }
        }

        Ok(())
    }

    /// Open a large file with confirmed full loading for non-resynchronizable encoding.
    ///
    /// Called after user confirms they want to load a large file with an encoding like
    /// GB18030, GBK, Shift-JIS, or EUC-KR that requires loading the entire file into memory.
    pub fn open_file_large_encoding_confirmed(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Use the same base directory logic as open_file
        let base_dir = self.working_dir.clone();

        let resolved_path = if path.is_relative() {
            base_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Canonicalize the path
        let canonical_path = self
            .filesystem
            .canonicalize(&resolved_path)
            .unwrap_or_else(|_| resolved_path.clone());
        let path = canonical_path.as_path();

        // Check if already open
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            self.set_active_buffer(id);
            return Ok(id);
        }

        // Create new buffer with forced full loading
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        // Load buffer with forced full loading (bypasses the large file encoding check)
        let buffer = crate::model::buffer::Buffer::load_large_file_confirmed(
            path,
            Arc::clone(&self.filesystem),
        )?;

        // Create editor state with the buffer
        let highlighter =
            crate::primitives::highlight_engine::HighlightEngine::for_file_with_languages(
                path,
                &self.grammar_registry,
                &self.config.languages,
            );

        let language = crate::primitives::highlighter::Language::from_path(path);
        let language_name = if let Some(lang) = &language {
            lang.to_string()
        } else {
            crate::services::lsp::manager::detect_language(path, &self.config.languages)
                .unwrap_or_else(|| "text".to_string())
        };

        let mut state =
            EditorState::from_buffer_with_highlighter(buffer, highlighter, language_name, language);

        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let metadata =
            super::types::BufferMetadata::with_file(path.to_path_buf(), &self.working_dir);
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.preferred_split_for_file();
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            buf_state.rulers = self.config.editor.rulers.clone();
        }

        self.set_active_buffer(buffer_id);

        // Use display_name from metadata for relative path display
        let display_name = self
            .buffer_metadata
            .get(&buffer_id)
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| path.display().to_string());

        self.status_message = Some(t!("buffer.opened", name = display_name).to_string());

        Ok(buffer_id)
    }

    /// Restore global file state (cursor and scroll position) for a newly opened file
    ///
    /// This looks up the file's saved state from the global file states store
    /// and applies it to both the EditorState (cursor) and SplitViewState (viewport).
    fn restore_global_file_state(&mut self, buffer_id: BufferId, path: &Path, split_id: SplitId) {
        use crate::workspace::PersistedFileWorkspace;

        // Load the per-file state for this path (lazy load from disk)
        let file_state = match PersistedFileWorkspace::load(path) {
            Some(state) => state,
            None => return, // No saved state for this file
        };

        // Get the buffer to validate positions
        let max_pos = match self.buffers.get(&buffer_id) {
            Some(buffer) => buffer.buffer.len(),
            None => return,
        };

        // Apply cursor position and viewport (scroll) state to SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                let cursor_pos = file_state.cursor.position.min(max_pos);
                buf_state.cursors.primary_mut().position = cursor_pos;
                buf_state.cursors.primary_mut().anchor =
                    file_state.cursor.anchor.map(|a| a.min(max_pos));
            }
            view_state.viewport.top_byte = file_state.scroll.top_byte;
            view_state.viewport.left_column = file_state.scroll.left_column;
        }
    }

    /// Save file state when a buffer is closed (for per-file session persistence)
    fn save_file_state_on_close(&self, buffer_id: BufferId) {
        use crate::workspace::{
            PersistedFileWorkspace, SerializedCursor, SerializedFileState, SerializedScroll,
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

        // Get the per-buffer view state (not necessarily the active buffer in this split)
        let buf_state = match view_state.keyed_states.get(&buffer_id) {
            Some(bs) => bs,
            None => return,
        };

        // Capture the current state
        let primary_cursor = buf_state.cursors.primary();
        let file_state = SerializedFileState {
            cursor: SerializedCursor {
                position: primary_cursor.position,
                anchor: primary_cursor.anchor,
                sticky_column: primary_cursor.sticky_column,
            },
            additional_cursors: buf_state
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
                top_byte: buf_state.viewport.top_byte,
                top_view_line_offset: buf_state.viewport.top_view_line_offset,
                left_column: buf_state.viewport.left_column,
            },
            view_mode: Default::default(),
            compose_width: None,
            plugin_state: std::collections::HashMap::new(),
        };

        // Save to disk
        PersistedFileWorkspace::save(&abs_path, file_state);
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

        // Read cursor state from split view state
        let cursors = self.active_cursors();
        let cursor_id = cursors.primary_id();
        let old_position = cursors.primary().position;
        let old_anchor = cursors.primary().anchor;
        let old_sticky_column = cursors.primary().sticky_column;

        if let Some(state) = self.buffers.get(&buffer_id) {
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

            let split_id = self.split_manager.active_split();
            let state = self.buffers.get_mut(&buffer_id).unwrap();
            let view_state = self.split_view_states.get_mut(&split_id).unwrap();
            state.apply(&mut view_state.cursors, &event);
        }
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> BufferId {
        // Save current position before switching to new buffer
        self.position_history.commit_pending_movement();

        // Explicitly record current position before switching
        let cursors = self.active_cursors();
        let position = cursors.primary().position;
        let anchor = cursors.primary().anchor;
        self.position_history
            .record_movement(self.active_buffer(), position, anchor);
        self.position_history.commit_pending_movement();

        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            Arc::clone(&self.filesystem),
        );
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);
        // Set default line ending for new buffers from config
        state
            .buffer
            .set_default_line_ending(self.config.editor.default_line_ending.to_line_ending());
        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());
        self.buffer_metadata
            .insert(buffer_id, crate::app::types::BufferMetadata::new());

        // Initialize per-buffer view state with config defaults
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            buf_state.rulers = self.config.editor.rulers.clone();
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
        thread_handle: Option<std::thread::JoinHandle<anyhow::Result<()>>>,
    ) -> AnyhowResult<BufferId> {
        // Save current position before switching to new buffer
        self.position_history.commit_pending_movement();

        // Explicitly record current position before switching
        let cursors = self.active_cursors();
        let position = cursors.primary().position;
        let anchor = cursors.primary().anchor;
        self.position_history
            .record_movement(self.active_buffer(), position, anchor);
        self.position_history.commit_pending_movement();

        // If the current buffer is empty and unmodified, replace it instead of creating a new one
        // Note: Don't replace composite buffers (they appear empty but are special views)
        let replace_current = {
            let current_state = self.buffers.get(&self.active_buffer()).unwrap();
            !current_state.is_composite_buffer
                && current_state.buffer.is_empty()
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
        let file_size = self.filesystem.metadata(temp_path)?.size as usize;

        // Load from temp file using EditorState::from_file_with_languages
        // This enables lazy chunk loading for large inputs (>100MB by default)
        let mut state = EditorState::from_file_with_languages(
            temp_path,
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            &self.grammar_registry,
            &self.config.languages,
            Arc::clone(&self.filesystem),
        )?;

        // Clear the file path so the buffer is "unnamed" for save purposes
        // The Unloaded chunks still reference the temp file for lazy loading
        state.buffer.clear_file_path();
        // Clear modified flag - content is "fresh" from stdin (vim behavior)
        state.buffer.clear_modified();

        // Set tab size from config
        state.buffer_settings.tab_size = self.config.editor.tab_size;

        // Apply line_numbers default from config
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata for this buffer (no file path)
        let metadata =
            super::types::BufferMetadata::new_unnamed(t!("stdin.display_name").to_string());
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add buffer to the active split's tabs
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            buf_state.rulers = self.config.editor.rulers.clone();
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
        let current_size = self
            .filesystem
            .metadata(&stream_state.temp_path)
            .map(|m| m.size as usize)
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
            self.status_message =
                Some(t!("stdin.streaming_bytes", bytes = current_size).to_string());
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
                        self.status_message =
                            Some(t!("stdin.read_error", error = e.to_string()).to_string());
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
            let final_size = self
                .filesystem
                .metadata(&stream_state.temp_path)
                .map(|m| m.size as usize)
                .unwrap_or(stream_state.last_known_size);

            if final_size > stream_state.last_known_size {
                if let Some(editor_state) = self.buffers.get_mut(&stream_state.buffer_id) {
                    editor_state
                        .buffer
                        .extend_streaming(&stream_state.temp_path, final_size);
                }
                stream_state.last_known_size = final_size;
            }

            self.status_message =
                Some(t!("stdin.read_complete", bytes = stream_state.last_known_size).to_string());
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
            Arc::clone(&self.filesystem),
        );
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created

        // Set syntax highlighting based on buffer name (e.g., "*OURS*.c" will get C highlighting)
        state.set_language_from_name(&name, &self.grammar_registry);

        // Apply line_numbers default from config
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

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
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            buf_state.rulers = self.config.editor.rulers.clone();
        } else {
            // Create view state if it doesn't exist
            let mut view_state =
                SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id);
            view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            view_state.rulers = self.config.editor.rulers.clone();
            view_state.show_line_numbers = self.config.editor.line_numbers;
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
        // Save current cursor position from split view state to preserve it after content update
        let old_cursor_pos = self
            .split_view_states
            .values()
            .find(|vs| vs.has_buffer(buffer_id))
            .and_then(|vs| vs.keyed_states.get(&buffer_id))
            .map(|bs| bs.cursors.primary().position)
            .unwrap_or(0);

        let state = self
            .buffers
            .get_mut(&buffer_id)
            .ok_or_else(|| "Buffer not found".to_string())?;

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

        // Update cursor in the split view state that has this buffer
        for view_state in self.split_view_states.values_mut() {
            if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                buf_state.cursors.primary_mut().position = new_cursor_pos;
                buf_state.cursors.primary_mut().anchor = None;
            }
        }

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
            state.margins.configure_for_line_numbers(false);
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
            state.margins.configure_for_line_numbers(false);
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
                // Use buffer's stored language
                self.buffers
                    .get(&self.active_buffer())
                    .map(|s| s.language.clone())
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
        let cursor_pos = self.active_cursors().primary().position;
        Some(state.text_properties.get_at(cursor_pos))
    }

    /// Close the given buffer
    pub fn close_buffer(&mut self, id: BufferId) -> anyhow::Result<()> {
        // Check for unsaved changes
        if let Some(state) = self.buffers.get(&id) {
            if state.buffer.is_modified() {
                return Err(anyhow::anyhow!("Buffer has unsaved changes"));
            }
        }
        self.close_buffer_internal(id)
    }

    /// Force close the given buffer without checking for unsaved changes
    /// Use this when the user has already confirmed they want to discard changes
    pub fn force_close_buffer(&mut self, id: BufferId) -> anyhow::Result<()> {
        self.close_buffer_internal(id)
    }

    /// Internal helper to close a buffer (shared by close_buffer and force_close_buffer)
    fn close_buffer_internal(&mut self, id: BufferId) -> anyhow::Result<()> {
        // Save file state before closing (for per-file session persistence)
        self.save_file_state_on_close(id);

        // If closing a terminal buffer, clean up terminal-related data structures
        if let Some(terminal_id) = self.terminal_buffers.remove(&id) {
            // Close the terminal process
            self.terminal_manager.close(terminal_id);

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

            // Remove from terminal_mode_resume to prevent stale entries
            self.terminal_mode_resume.remove(&id);

            // Exit terminal mode if we were in it
            if self.terminal_mode {
                self.terminal_mode = false;
                self.key_context = crate::input::keybindings::KeyContext::Normal;
            }
        }

        // Find a replacement buffer, preferring the most recently focused one
        // First try focus history, then fall back to any visible buffer
        let active_split = self.split_manager.active_split();
        let replacement_from_history = self.split_view_states.get(&active_split).and_then(|vs| {
            // Find the most recently focused buffer that's still open and visible
            vs.focus_history
                .iter()
                .rev()
                .find(|&&bid| {
                    bid != id
                        && self.buffers.contains_key(&bid)
                        && !self
                            .buffer_metadata
                            .get(&bid)
                            .map(|m| m.hidden_from_tabs)
                            .unwrap_or(false)
                })
                .copied()
        });

        // Fall back to any visible buffer if no history match
        let visible_replacement = replacement_from_history.or_else(|| {
            self.buffers
                .keys()
                .find(|&&bid| {
                    bid != id
                        && !self
                            .buffer_metadata
                            .get(&bid)
                            .map(|m| m.hidden_from_tabs)
                            .unwrap_or(false)
                })
                .copied()
        });

        let is_last_visible_buffer = visible_replacement.is_none();
        let replacement_buffer = if is_last_visible_buffer {
            self.new_buffer()
        } else {
            visible_replacement.unwrap()
        };

        // Switch to replacement buffer BEFORE updating splits.
        // This is important because set_active_buffer returns early if the buffer
        // is already active, and updating splits changes what active_buffer() returns.
        // We need set_active_buffer to run its terminal_mode_resume logic.
        if self.active_buffer() == id {
            self.set_active_buffer(replacement_buffer);
        }

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
        if let Some((request_id, _, _)) = self.semantic_tokens_in_flight.remove(&id) {
            self.pending_semantic_token_requests.remove(&request_id);
        }
        if let Some((request_id, _, _, _)) = self.semantic_tokens_range_in_flight.remove(&id) {
            self.pending_semantic_token_range_requests
                .remove(&request_id);
        }
        self.semantic_tokens_range_last_request.remove(&id);
        self.semantic_tokens_range_applied.remove(&id);
        self.semantic_tokens_full_debounce.remove(&id);

        // Remove buffer from panel_ids mapping if it was a panel buffer
        // This prevents stale entries when the same panel_id is reused later
        self.panel_ids.retain(|_, &mut buf_id| buf_id != id);

        // Remove buffer from all splits' open_buffers lists and focus history
        for view_state in self.split_view_states.values_mut() {
            view_state.remove_buffer(id);
            view_state.remove_from_history(id);
        }

        // If this was the last visible buffer, focus file explorer
        if is_last_visible_buffer {
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
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
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
            // If this is the only buffer in this split AND there are other splits,
            // close the split instead of the buffer (don't create an empty replacement)
            let has_other_splits = self.split_manager.root().count_leaves() > 1;
            if current_split_tabs.len() <= 1 && has_other_splits {
                // Check for unsaved changes first
                if self.active_state().buffer.is_modified() {
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
                    return;
                }
                // Close the buffer first, then the split
                let _ = self.close_buffer(buffer_id);
                self.close_active_split();
                return;
            }

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
                // If we're closing a terminal buffer while in terminal mode, exit terminal mode
                if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
                    self.terminal_mode = false;
                    self.key_context = crate::input::keybindings::KeyContext::Normal;
                }
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

            // If we're closing a terminal buffer while in terminal mode, exit terminal mode
            if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
                self.terminal_mode = false;
                self.key_context = crate::input::keybindings::KeyContext::Normal;
            }

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

    /// Get visible (non-hidden) buffers for the current split.
    /// This filters out buffers with hidden_from_tabs=true.
    fn visible_buffers_for_active_split(&self) -> Vec<BufferId> {
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get(&active_split) {
            view_state
                .open_buffers
                .iter()
                .copied()
                .filter(|id| {
                    !self
                        .buffer_metadata
                        .get(id)
                        .map(|m| m.hidden_from_tabs)
                        .unwrap_or(false)
                })
                .collect()
        } else {
            // Fallback to all visible buffers if no view state
            let mut all_ids: Vec<_> = self
                .buffers
                .keys()
                .copied()
                .filter(|id| {
                    !self
                        .buffer_metadata
                        .get(id)
                        .map(|m| m.hidden_from_tabs)
                        .unwrap_or(false)
                })
                .collect();
            all_ids.sort_by_key(|id| id.0);
            all_ids
        }
    }

    /// Switch to next buffer in current split's tabs
    pub fn next_buffer(&mut self) {
        let ids = self.visible_buffers_for_active_split();

        if ids.is_empty() {
            return;
        }

        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer()) {
            let next_idx = (idx + 1) % ids.len();
            if ids[next_idx] != self.active_buffer() {
                // Save current position before switching
                self.position_history.commit_pending_movement();

                // Also explicitly record current position
                let cursors = self.active_cursors();
                let position = cursors.primary().position;
                let anchor = cursors.primary().anchor;
                self.position_history
                    .record_movement(self.active_buffer(), position, anchor);
                self.position_history.commit_pending_movement();

                self.set_active_buffer(ids[next_idx]);
            }
        }
    }

    /// Switch to previous buffer in current split's tabs
    pub fn prev_buffer(&mut self) {
        let ids = self.visible_buffers_for_active_split();

        if ids.is_empty() {
            return;
        }

        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer()) {
            let prev_idx = if idx == 0 { ids.len() - 1 } else { idx - 1 };
            if ids[prev_idx] != self.active_buffer() {
                // Save current position before switching
                self.position_history.commit_pending_movement();

                // Also explicitly record current position
                let cursors = self.active_cursors();
                let position = cursors.primary().position;
                let anchor = cursors.primary().anchor;
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
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
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
                let cursors = self.active_cursors();
                let cursor_id = cursors.primary_id();
                let old_position = cursors.primary().position;
                let old_anchor = cursors.primary().anchor;
                let old_sticky_column = cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                let split_id = self.split_manager.active_split();
                let state = self.buffers.get_mut(&target_buffer).unwrap();
                let view_state = self.split_view_states.get_mut(&split_id).unwrap();
                state.apply(&mut view_state.cursors, &event);
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
                let cursors = self.active_cursors();
                let cursor_id = cursors.primary_id();
                let old_position = cursors.primary().position;
                let old_anchor = cursors.primary().anchor;
                let old_sticky_column = cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                let split_id = self.split_manager.active_split();
                let state = self.buffers.get_mut(&target_buffer).unwrap();
                let view_state = self.split_view_states.get_mut(&split_id).unwrap();
                state.apply(&mut view_state.cursors, &event);
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

    /// Queue a file to be opened after the TUI starts.
    ///
    /// This is used for CLI file arguments to ensure they go through the same
    /// code path as interactive file opens, providing consistent error handling
    /// (e.g., encoding confirmation prompts are shown in the UI instead of crashing).
    pub fn queue_file_open(&mut self, path: PathBuf, line: Option<usize>, column: Option<usize>) {
        self.pending_file_opens
            .push(super::PendingFileOpen { path, line, column });
    }

    /// Process pending file opens (called from the event loop).
    ///
    /// Opens files that were queued during startup, using the same error handling
    /// as interactive file opens. Returns true if any files were processed.
    pub fn process_pending_file_opens(&mut self) -> bool {
        if self.pending_file_opens.is_empty() {
            return false;
        }

        // Take all pending files to process
        let pending = std::mem::take(&mut self.pending_file_opens);
        let mut processed_any = false;

        for pending_file in pending {
            tracing::info!(
                "[SYNTAX DEBUG] Processing pending file open: {:?}",
                pending_file.path
            );

            match self.open_file(&pending_file.path) {
                Ok(_) => {
                    // Navigate to line/column if specified
                    if let Some(line) = pending_file.line {
                        self.goto_line_col(line, pending_file.column);
                    }
                    processed_any = true;
                }
                Err(e) => {
                    // Check if this is a large file encoding confirmation error
                    // Show prompt instead of crashing
                    if let Some(confirmation) =
                        e.downcast_ref::<crate::model::buffer::LargeFileEncodingConfirmation>()
                    {
                        self.start_large_file_encoding_confirmation(confirmation);
                    } else {
                        // For other errors, show status message (consistent with file browser)
                        self.set_status_message(
                            t!("file.error_opening", error = e.to_string()).to_string(),
                        );
                    }
                    processed_any = true;
                }
            }
        }

        processed_any
    }
}
