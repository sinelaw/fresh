//! File-open orchestrators on `Editor`.
//!
//! The `open_file` family — open_file, open_file_no_focus,
//! open_local_file, open_file_with_encoding, reload_with_encoding,
//! open_file_large_encoding_confirmed — and supporting helpers
//! restore_global_file_state and save_file_state_on_close.
//!
//! Opening a file in this editor coordinates: detecting the file type,
//! choosing or creating a buffer, registering with the LSP, parsing
//! grammar, restoring per-file UI state (cursor position, scroll), and
//! deciding which split to focus. Each variant differs only in how it
//! handles encoding errors, focus, and "no file at this path yet" cases.

use std::path::Path;
use std::sync::Arc;

use rust_i18n::t;

use crate::model::event::{BufferId, LeafId};
use crate::state::EditorState;

use super::Editor;

impl Editor {
    /// Open a file and return its buffer ID
    ///
    /// If the file doesn't exist, creates an unsaved buffer with that filename.
    /// Saving the buffer will create the file.
    pub fn open_file(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Check whether the active buffer had a file path before loading.
        // If it didn't, open_file_no_focus may replace the empty initial buffer
        // in-place (same buffer ID, new content), and we need to notify plugins.
        let active_had_path = self
            .buffers
            .get(&self.active_buffer())
            .and_then(|s| s.buffer.file_path())
            .is_some();

        let buffer_id = self.open_file_no_focus(path)?;

        // Check if this was an already-open buffer or a new one
        // For already-open buffers, just switch to them
        // For new buffers, record position history before switching
        let is_new_buffer = self.active_buffer() != buffer_id;

        if is_new_buffer && !self.suppress_position_history_once {
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

        // If the initial empty buffer was replaced in-place with file content,
        // set_active_buffer is a no-op (same buffer ID). Fire buffer_activated
        // explicitly so plugins see the newly loaded file.
        // Skip this when re-opening an already-active file (active_had_path),
        // as nothing changed and the extra hook would cause spurious refreshes
        // in plugins like the diagnostics panel.
        if !is_new_buffer && !active_had_path {
            #[cfg(feature = "plugins")]
            self.update_plugin_state_snapshot();

            self.plugin_manager.run_hook(
                "buffer_activated",
                crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
            );
        }

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
        // Fail fast if the remote connection is down — don't attempt I/O that
        // would either timeout or return confusing errors.
        if !self.authority.filesystem.is_remote_connected() {
            anyhow::bail!(
                "Cannot open file: remote connection lost ({})",
                self.authority
                    .filesystem
                    .remote_connection_info()
                    .unwrap_or("unknown host")
            );
        }

        // Resolve relative paths against appropriate base directory
        // For remote mode, use the remote home directory; for local, use working_dir
        let base_dir = if self.authority.filesystem.remote_connection_info().is_some() {
            self.authority
                .filesystem
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
        let file_exists = self.authority.filesystem.exists(&resolved_path);

        // Save the user-visible (non-canonicalized) path for language detection.
        // Glob patterns in language config should match the path as the user sees it,
        // not the canonical path (e.g., on macOS /var -> /private/var symlinks).
        let display_path = resolved_path.clone();

        // Canonicalize the path to resolve symlinks and normalize path components
        // This ensures consistent path representation throughout the editor
        // For non-existent files, we need to canonicalize the parent directory and append the filename
        let canonical_path = if file_exists {
            self.authority
                .filesystem
                .canonicalize(&resolved_path)
                .unwrap_or_else(|_| resolved_path.clone())
        } else {
            // For non-existent files, canonicalize parent dir and append filename
            if let Some(parent) = resolved_path.parent() {
                let canonical_parent = if parent.as_os_str().is_empty() {
                    // No parent means just a filename, use base dir
                    base_dir.clone()
                } else {
                    self.authority
                        .filesystem
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
        if self.authority.filesystem.is_dir(path).unwrap_or(false) {
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
            "[SYNTAX DEBUG] open_file_no_focus: path={:?}, extension={:?}, catalog={}",
            path,
            path.extension(),
            self.grammar_registry.catalog().len(),
        );
        let mut state = if file_exists {
            // Load from canonical path (for I/O and dedup), detect language from
            // display path (for glob pattern matching against user-visible names).
            let buffer = crate::model::buffer::Buffer::load_from_file(
                &canonical_path,
                self.config.editor.large_file_threshold_bytes as usize,
                Arc::clone(&self.authority.filesystem),
            )?;
            let first_line = buffer.first_line_lossy();
            let detected =
                crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                    &display_path,
                    first_line.as_deref(),
                    &self.grammar_registry,
                    &self.config.languages,
                    self.config.default_language.as_deref(),
                );
            EditorState::from_buffer_with_language(buffer, detected)
        } else {
            // File doesn't exist - create empty buffer with the file path set
            EditorState::new_with_path(
                self.config.editor.large_file_threshold_bytes as usize,
                Arc::clone(&self.authority.filesystem),
                path.to_path_buf(),
            )
        };
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created

        // Check if the buffer contains binary content
        let is_binary = state.buffer.is_binary();
        if is_binary {
            // Make binary buffers read-only
            state.editing_disabled = true;
            tracing::info!("Detected binary file: {}", path.display());
        }

        // Set whitespace visibility, use_tabs, and tab_size based on language config
        // with fallback to global editor config for tab_size
        // Use the buffer's stored language (already set by from_file_with_languages)
        let mut whitespace =
            crate::config::WhitespaceVisibility::from_editor_config(&self.config.editor);
        state.buffer_settings.auto_close = self.config.editor.auto_close;
        state.buffer_settings.auto_surround = self.config.editor.auto_surround;
        if let Some(lang_config) = self.config.languages.get(&state.language) {
            whitespace = whitespace.with_language_tab_override(lang_config.show_whitespace_tabs);
            state.buffer_settings.use_tabs =
                lang_config.use_tabs.unwrap_or(self.config.editor.use_tabs);
            // Use language-specific tab_size if set, otherwise fall back to global
            state.buffer_settings.tab_size =
                lang_config.tab_size.unwrap_or(self.config.editor.tab_size);
            // Auto close: language override (only if globally enabled)
            if state.buffer_settings.auto_close {
                if let Some(lang_auto_close) = lang_config.auto_close {
                    state.buffer_settings.auto_close = lang_auto_close;
                }
            }
            // Auto surround: language override (only if globally enabled)
            if state.buffer_settings.auto_surround {
                if let Some(lang_auto_surround) = lang_config.auto_surround {
                    state.buffer_settings.auto_surround = lang_auto_surround;
                }
            }
        } else {
            state.buffer_settings.tab_size = self.config.editor.tab_size;
            state.buffer_settings.use_tabs = self.config.editor.use_tabs;
        }
        state.buffer_settings.whitespace = whitespace;

        // Apply line_numbers default from config
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata for this buffer
        let mut metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.working_dir,
            self.authority.path_translation.as_ref(),
        );

        // Mark binary files in metadata and disable LSP
        if is_binary {
            metadata.binary = true;
            metadata.read_only = true;
            metadata.disable_lsp(t!("buffer.binary_file").to_string());
        }

        // Check if the file is read-only on disk (filesystem permissions)
        if file_exists && !metadata.read_only && !self.authority.filesystem.is_writable(path) {
            metadata.read_only = true;
        }

        // Mark read-only files (library, binary, or filesystem-readonly) as editing-disabled
        if metadata.read_only {
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.editing_disabled = true;
            }
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
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        let page_view = self.resolve_page_view_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            // Initialize per-buffer view state for the new buffer with config defaults
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
            // Auto-activate page view if configured for this language
            if let Some(page_width) = page_view {
                buf_state.activate_page_view(page_width);
            }
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

        // Save user-visible path for language detection before canonicalizing
        let display_path = resolved_path.clone();

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

        // Load from canonical path (for I/O and dedup), detect language from
        // display path (for glob pattern matching against user-visible names).
        let buffer = crate::model::buffer::Buffer::load_from_file(
            &canonical_path,
            self.config.editor.large_file_threshold_bytes as usize,
            Arc::clone(&self.local_filesystem),
        )?;
        let first_line = buffer.first_line_lossy();
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &display_path,
                first_line.as_deref(),
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );
        let state = EditorState::from_buffer_with_language(buffer, detected);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata
        let metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.working_dir,
            self.authority.path_translation.as_ref(),
        );
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.preferred_split_for_file();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
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

        // Save user-visible path for language detection before canonicalizing
        let display_path = resolved_path.clone();

        // Canonicalize the path
        let canonical_path = self
            .authority
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

        // Load buffer with the specified encoding (use canonical path for I/O)
        let buffer = crate::model::buffer::Buffer::load_from_file_with_encoding(
            path,
            encoding,
            Arc::clone(&self.authority.filesystem),
            crate::model::buffer::BufferConfig {
                estimated_line_length: self.config.editor.estimated_line_length,
            },
        )?;
        let first_line = buffer.first_line_lossy();
        // Create editor state with the buffer
        // Use display_path for language detection (glob patterns match user-visible paths)
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &display_path,
                first_line.as_deref(),
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );

        let mut state = EditorState::from_buffer_with_language(buffer, detected);

        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.working_dir,
            self.authority.path_translation.as_ref(),
        );
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.preferred_split_for_file();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
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
            Arc::clone(&self.authority.filesystem),
            crate::model::buffer::BufferConfig {
                estimated_line_length: self.config.editor.estimated_line_length,
            },
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

        // Save user-visible path for language detection before canonicalizing
        let display_path = resolved_path.clone();

        // Canonicalize the path
        let canonical_path = self
            .authority
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
            Arc::clone(&self.authority.filesystem),
        )?;
        let first_line = buffer.first_line_lossy();
        // Create editor state with the buffer
        // Use display_path for language detection (glob patterns match user-visible paths)
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &display_path,
                first_line.as_deref(),
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );

        let mut state = EditorState::from_buffer_with_language(buffer, detected);

        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.working_dir,
            self.authority.path_translation.as_ref(),
        );
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.preferred_split_for_file();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
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
    fn restore_global_file_state(&mut self, buffer_id: BufferId, path: &Path, split_id: LeafId) {
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

        // Apply cursor position and viewport (scroll) state to the
        // *loaded buffer's* keyed view state. This must NOT go through
        // `view_state.viewport` / `view_state.cursors` — those Deref
        // through to the split's *active* buffer's view, which for
        // `open_file_no_focus` is still the previously-active buffer.
        // Writing through the Deref would scroll the unrelated active
        // buffer (visible to the user as: switching to a result in a
        // different file inside the Live Grep overlay scrolls the
        // background buffer by one line — issue surfaced by the
        // preview-pane file-load path).
        let view_state_opt = self.split_view_states.get_mut(&split_id);
        let buffer_state_opt = self.buffers.get_mut(&buffer_id);
        if let (Some(view_state), Some(buffer_state)) = (view_state_opt, buffer_state_opt) {
            if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                let cursor_pos = file_state.cursor.position.min(max_pos);
                buf_state.cursors.primary_mut().position = cursor_pos;
                buf_state.cursors.primary_mut().anchor =
                    file_state.cursor.anchor.map(|a| a.min(max_pos));
                buf_state.viewport.top_byte = file_state.scroll.top_byte;
                buf_state.viewport.left_column = file_state.scroll.left_column;
                // Saved cursor and saved viewport are written from
                // independent fields and may be out of sync (e.g.
                // cursor moved off-screen before save). Reconcile so
                // the restored view always shows the cursor — without
                // this, arrow keys in wrap mode can't bring the
                // viewport back because of the
                // `top_view_line_offset > 0` early return in
                // `viewport.rs::ensure_visible` (#1689 follow-up).
                super::navigation::reconcile_restored_buffer_view(
                    buf_state,
                    &mut buffer_state.buffer,
                );
            }
        }
    }

    /// Save file state when a buffer is closed (for per-file session persistence)
    pub(super) fn save_file_state_on_close(&self, buffer_id: BufferId) {
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
            folds: Vec::new(),
        };

        // Save to disk
        PersistedFileWorkspace::save(&abs_path, file_state);
        tracing::debug!("Saved file state on close for {:?}", abs_path);
    }

    /// Open the file an LSP response URI points at, handling the three
    /// cases the goto-def / references / workspace-edit handlers all
    /// have to think about:
    ///
    ///   * **on-host file** (the workspace bind mount, or a local
    ///     authority): host-translate the URI and open the host file
    ///     normally — exactly what the editor has always done.
    ///   * **container-only file** (devcontainer attach with the
    ///     target outside the workspace mount, e.g. a pip-installed
    ///     `~/.local/.../site-packages/flask/app.py`): fetch the file
    ///     bytes via the authority's process spawner
    ///     (`docker exec <id> cat <path>`) and open them as a
    ///     read-only buffer at the in-container path.
    ///   * **unreachable** (no file at the host path; container fetch
    ///     failed or no container authority): return `Err` so the
    ///     caller can surface a user-visible status message instead
    ///     of silently opening a phantom buffer.
    ///
    /// Cursor placement, focus, and any post-open hook firing are the
    /// caller's job (this method just resolves "URI → BufferId").
    pub(crate) fn open_lsp_uri_target(
        &mut self,
        uri: &crate::app::types::LspUri,
    ) -> anyhow::Result<BufferId> {
        let translation = self.authority.path_translation.clone();
        let host_path = uri
            .to_host_path(translation.as_ref())
            .ok_or_else(|| anyhow::anyhow!("URI is not a file path"))?;

        // Case 1: file is reachable on the host filesystem (either
        // local authority, or workspace-mounted on a devcontainer).
        // `open_file` focuses, which is what callers (goto-def,
        // workspace edits) expect — they want the cursor to land in
        // the destination buffer afterward.
        if self.authority.filesystem.exists(&host_path) {
            return self.open_file(&host_path);
        }

        // Case 2: container-only fetch. Only meaningful when the
        // active authority can route a `cat` through to the
        // container — `path_translation` being set is the proxy for
        // "this is a container authority". Local + SSH authorities
        // skip straight to the error case.
        if translation.is_some() {
            // The container-side path is the URI's raw path. Calling
            // `to_host_path` with `None` returns the wire-side path
            // verbatim (no translation applied) — exactly what we
            // need for `cat <path>` inside the container.
            let container_path = uri.to_host_path(None).ok_or_else(|| {
                anyhow::anyhow!("URI is not a file path (container-side decode failed)")
            })?;
            let buffer_id = self.fetch_and_open_container_file(container_path, uri.clone())?;
            // Match `open_file`'s focus behaviour so the cursor
            // assertion in callers (goto-def's `MoveCursor` event)
            // applies to the right buffer.
            self.set_active_buffer(buffer_id);
            return Ok(buffer_id);
        }

        // Case 3: nothing we can open.
        Err(anyhow::anyhow!(
            "could not open {}: file not found",
            host_path.display()
        ))
    }

    /// Run `cat <container_path>` through the active authority's
    /// process spawner and open the result as a read-only buffer
    /// tagged with the wire URI. Helper for [`Self::open_lsp_uri_target`].
    ///
    /// On `cat` exit-code 0 the bytes become the buffer's contents.
    /// On any error (no tokio runtime, spawner failure, non-zero
    /// exit) we return `Err` with a message that includes the
    /// container path and stderr's first line — enough for the
    /// caller's status-line surface.
    fn fetch_and_open_container_file(
        &mut self,
        container_path: std::path::PathBuf,
        uri: crate::app::types::LspUri,
    ) -> anyhow::Result<BufferId> {
        let runtime = self.tokio_runtime.as_ref().ok_or_else(|| {
            anyhow::anyhow!(
                "could not open {}: no tokio runtime available for container fetch",
                container_path.display()
            )
        })?;

        let spawner = self.authority.process_spawner.clone();
        let path_arg = container_path.to_string_lossy().into_owned();
        let result = runtime
            .block_on(spawner.spawn("cat".into(), vec![path_arg], None))
            .map_err(|e| {
                anyhow::anyhow!(
                    "could not open {} from container: {}",
                    container_path.display(),
                    e
                )
            })?;

        if result.exit_code != 0 {
            let first_stderr_line = result
                .stderr
                .lines()
                .next()
                .unwrap_or("(no error message)")
                .trim();
            anyhow::bail!(
                "could not open {} from container: {}",
                container_path.display(),
                first_stderr_line
            );
        }

        self.open_container_only_file(container_path, uri, result.stdout.into_bytes())
    }

    /// Build a buffer from already-fetched container content. The
    /// buffer's `file_path` is the in-container path (so further LSP
    /// requests carry the right URI) and the buffer is read-only —
    /// there is no host writeback path for files that exist only
    /// inside the container. LSP stays enabled so a follow-up
    /// goto-def from the fetched buffer works.
    pub(crate) fn open_container_only_file(
        &mut self,
        container_path: std::path::PathBuf,
        uri: crate::app::types::LspUri,
        content: Vec<u8>,
    ) -> anyhow::Result<BufferId> {
        // Don't double-open. The file_path matches by container path,
        // since that's what we set after build.
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(container_path.as_path()))
            .map(|(id, _)| *id);
        if let Some(id) = already_open {
            return Ok(id);
        }

        // Build the buffer from the fetched bytes and pin its
        // file_path to the container path. The host filesystem ref
        // here is mostly cosmetic — the buffer is read-only so save
        // never runs through it.
        let mut buffer = crate::model::buffer::Buffer::from_bytes(
            content,
            Arc::clone(&self.authority.filesystem),
        );
        buffer.rename_file_path(container_path.clone());

        // Detect language from the container path (the basename's
        // extension is what matters; the directory tree is
        // container-side and won't match host-relative globs anyway).
        let first_line = buffer.first_line_lossy();
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &container_path,
                first_line.as_deref(),
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );
        let mut state = EditorState::from_buffer_with_language(buffer, detected);
        state.editing_disabled = true;

        // Whitespace / tab settings — same shape as `open_file_no_focus`
        // so the rendered look is consistent. Container-fetched
        // buffers should obey the user's editor config like any other
        // read-only buffer.
        let mut whitespace =
            crate::config::WhitespaceVisibility::from_editor_config(&self.config.editor);
        if let Some(lang_config) = self.config.languages.get(&state.language) {
            whitespace = whitespace.with_language_tab_override(lang_config.show_whitespace_tabs);
            state.buffer_settings.use_tabs =
                lang_config.use_tabs.unwrap_or(self.config.editor.use_tabs);
            state.buffer_settings.tab_size =
                lang_config.tab_size.unwrap_or(self.config.editor.tab_size);
        } else {
            state.buffer_settings.tab_size = self.config.editor.tab_size;
            state.buffer_settings.use_tabs = self.config.editor.use_tabs;
        }
        state.buffer_settings.whitespace = whitespace;
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;
        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let mut metadata =
            super::types::BufferMetadata::with_container_file(container_path.clone(), uri);
        // Notify the LSP servers about the newly opened file so
        // hover / further goto-def in the fetched buffer works. The
        // URI we cached is already the wire-form URI, so the LSP
        // sees the right path.
        self.notify_lsp_file_opened(&container_path, buffer_id, &mut metadata);
        self.buffer_metadata.insert(buffer_id, metadata);

        // Wire the buffer into a tab on the preferred split, mirroring
        // the host-file path. Skip `watch_file` — there's no host
        // file to inotify, and the spawned-fetch is one-shot.
        let target_split = self.preferred_split_for_file();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
        }

        Ok(buffer_id)
    }
}
