//! Stdin streaming and virtual buffer creation on `Editor`.
//!
//! - open_stdin_buffer / poll_stdin_streaming / complete_stdin_streaming /
//!   is_stdin_streaming: drive the StdinStream subsystem (extracted in
//!   phase 2e), translating its outcomes into buffer extensions and
//!   status messages.
//! - create_virtual_buffer / set_virtual_buffer_content: helpers for
//!   creating buffers backed by virtual content (LSP help text, plugin
//!   panels, search results, etc.).

use std::path::Path;
use std::sync::Arc;

use anyhow::Result as AnyhowResult;
use rust_i18n::t;

use crate::model::event::BufferId;
use crate::state::EditorState;
use crate::view::split::SplitViewState;

use super::Editor;

impl Editor {
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
        let file_size = self.authority.filesystem.metadata(temp_path)?.size as usize;

        // Load from temp file using EditorState::from_file_with_languages
        // This enables lazy chunk loading for large inputs (>100MB by default)
        let mut state = EditorState::from_file_with_languages(
            temp_path,
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            &self.grammar_registry,
            &self.config.languages,
            Arc::clone(&self.authority.filesystem),
        )?;

        // Clear the file path so the buffer is "unnamed" for save purposes
        // The Unloaded chunks still reference the temp file for lazy loading
        state.buffer.clear_file_path();
        // Clear modified flag - content is "fresh" from stdin (vim behavior)
        state.buffer.clear_modified();

        // Set tab size, auto_close, and auto_surround from config
        state.buffer_settings.tab_size = self.config.editor.tab_size;
        state.buffer_settings.auto_close = self.config.editor.auto_close;
        state.buffer_settings.auto_surround = self.config.editor.auto_surround;

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
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
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

        // Set up stdin streaming state for polling.
        // If no thread handle, the subsystem starts already-complete — used
        // by tests and the "stdin was fully drained before we started" case.
        self.stdin_stream
            .start(temp_path.to_path_buf(), buffer_id, file_size, thread_handle);

        // Status will be updated by poll_stdin_streaming
        self.status_message = Some(t!("stdin.streaming").to_string());

        Ok(buffer_id)
    }

    /// Poll stdin streaming state and extend buffer if file grew.
    /// Returns true if the status changed (needs render).
    pub fn poll_stdin_streaming(&mut self) -> bool {
        use super::stdin_stream::ThreadOutcome;

        if !self.stdin_stream.is_active() {
            return false;
        }

        let Some(buffer_id) = self.stdin_stream.buffer_id() else {
            return false;
        };
        let temp_path = self.stdin_stream.temp_path().unwrap().to_path_buf();
        let last_known = self.stdin_stream.last_known_size();

        let mut changed = false;

        // Check current file size
        let current_size = self
            .authority
            .filesystem
            .metadata(&temp_path)
            .map(|m| m.size as usize)
            .unwrap_or(last_known);

        // If file grew, extend the buffer
        if self.stdin_stream.record_growth(current_size) {
            if let Some(editor_state) = self.buffers.get_mut(&buffer_id) {
                editor_state
                    .buffer
                    .extend_streaming(&temp_path, current_size);
            }
            self.status_message =
                Some(t!("stdin.streaming_bytes", bytes = current_size).to_string());
            changed = true;
        }

        // Drain a just-finished thread and surface its outcome to the user.
        if let Some(outcome) = self.stdin_stream.take_finished_thread_outcome() {
            match outcome {
                ThreadOutcome::Success => {
                    tracing::info!("Stdin streaming completed successfully");
                }
                ThreadOutcome::Error(msg) => {
                    tracing::warn!("Stdin streaming error: {}", msg);
                    self.status_message = Some(t!("stdin.read_error", error = msg).to_string());
                }
                ThreadOutcome::Panic => {
                    tracing::warn!("Stdin streaming thread panicked");
                    self.status_message = Some(t!("stdin.read_error_panic").to_string());
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
        let Some(buffer_id) = self.stdin_stream.buffer_id() else {
            return;
        };
        let Some(temp_path) = self.stdin_stream.temp_path().map(Path::to_path_buf) else {
            return;
        };

        self.stdin_stream.mark_complete();

        // Final poll to get any remaining data
        let final_size = self
            .authority
            .filesystem
            .metadata(&temp_path)
            .map(|m| m.size as usize)
            .unwrap_or(self.stdin_stream.last_known_size());

        if self.stdin_stream.record_growth(final_size) {
            if let Some(editor_state) = self.buffers.get_mut(&buffer_id) {
                editor_state.buffer.extend_streaming(&temp_path, final_size);
            }
        }

        self.status_message = Some(
            t!(
                "stdin.read_complete",
                bytes = self.stdin_stream.last_known_size()
            )
            .to_string(),
        );
    }

    /// Check if stdin streaming is active (not complete).
    pub fn is_stdin_streaming(&self) -> bool {
        self.stdin_stream.is_active()
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
    ///
    /// Like [`Self::create_virtual_buffer`] but does **not** add the
    /// new buffer to any split's tab list. Use this when the caller
    /// is going to seed a freshly-created split (e.g. the Utility
    /// Dock leaf) with the new buffer directly — without it, the
    /// buffer would briefly appear as a phantom tab in whatever the
    /// previously-active split was, requiring a separate cleanup
    /// pass to remove it.
    pub fn create_virtual_buffer_detached(
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
            Arc::clone(&self.authority.filesystem),
        );
        // Set syntax highlighting based on buffer name (e.g., "*OURS*.c"
        // gets C highlighting). Mirrors create_virtual_buffer.
        state.set_language_from_name(&name, &self.grammar_registry);
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Set virtual buffer metadata
        let metadata = super::types::BufferMetadata::virtual_buffer(name, mode, read_only);
        self.buffer_metadata.insert(buffer_id, metadata);

        buffer_id
    }

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
            Arc::clone(&self.authority.filesystem),
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
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
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
        } else {
            // Create view state if it doesn't exist
            let mut view_state =
                SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id);
            view_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
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

        // Build text and properties from entries
        let (text, properties, collected_overlays) =
            crate::primitives::text_property::TextPropertyManager::from_entries(entries);

        // Replace buffer content
        // Note: we use buffer.delete_bytes/insert directly (not state.delete_range/insert_text_at)
        // which bypasses marker_list adjustment. Clear ALL overlays first so no stale markers
        // remain pointing at invalid positions in the new content.
        state.overlays.clear(&mut state.marker_list);

        let current_len = state.buffer.len();
        if current_len > 0 {
            state.buffer.delete_bytes(0, current_len);
        }
        state.buffer.insert(0, &text);

        // Clear modified flag since this is virtual buffer content setting, not user edits
        state.buffer.clear_modified();

        // Set text properties
        state.text_properties = properties;

        // Create inline overlays for the new content. Build the full vec
        // first and bulk-add it so the OverlayManager sorts exactly once;
        // a per-overlay `add` re-sorts every time and is O(n² log n) for
        // N entries (a big git-show diff can be ~500k overlays).
        {
            use crate::view::overlay::{Overlay, OverlayFace};
            use fresh_core::overlay::OverlayNamespace;

            let inline_ns = OverlayNamespace::from_string("_inline".to_string());
            let mut new_overlays = Vec::with_capacity(collected_overlays.len());

            for co in collected_overlays {
                let face = OverlayFace::from_options(&co.options);
                let mut overlay = Overlay::with_namespace(
                    &mut state.marker_list,
                    co.range,
                    face,
                    inline_ns.clone(),
                );
                overlay.extend_to_line_end = co.options.extend_to_line_end;
                if let Some(url) = co.options.url {
                    overlay.url = Some(url);
                }
                new_overlays.push(overlay);
            }
            state.overlays.extend(new_overlays);
        }

        // Each split keeps its own cursor; just clamp anything that fell
        // past the new buffer end and snap to a char boundary. Don't read
        // one split's cursor and write it into the others.
        let new_len = state.buffer.len();
        // `state` is no longer used past this point — re-borrow `self.buffers`
        // immutably for the snap and `self.split_view_states` mutably for the
        // write. These are disjoint fields of `self`.
        let buffer = &self
            .buffers
            .get(&buffer_id)
            .expect("buffer still present")
            .buffer;
        for view_state in self.split_view_states.values_mut() {
            let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) else {
                continue;
            };
            buf_state.cursors.map(|cursor| {
                let pos = cursor.position.min(new_len);
                cursor.position = buffer.snap_to_char_boundary(pos);
                if let Some(anchor) = cursor.anchor {
                    let clamped = anchor.min(new_len);
                    cursor.anchor = Some(buffer.snap_to_char_boundary(clamped));
                }
            });
        }
        Ok(())
    }
}
