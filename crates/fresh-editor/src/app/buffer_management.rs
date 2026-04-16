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
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::app::warning_domains::WarningDomain;
use crate::model::event::{BufferId, Event, LeafId};
use crate::state::EditorState;
use crate::view::prompt::PromptType;
use crate::view::split::SplitViewState;

use super::buffer_config_resolve;
use super::help;
use super::Editor;

impl Editor {
    /// Resolve the effective line_wrap setting for a buffer, considering language overrides.
    pub(super) fn resolve_line_wrap_for_buffer(&self, buffer_id: BufferId) -> bool {
        match self.buffers.get(&buffer_id) {
            Some(state) => buffer_config_resolve::line_wrap(&state.language, &self.config),
            None => self.config.editor.line_wrap,
        }
    }

    /// Resolve page view settings for a buffer from its language config.
    pub(super) fn resolve_page_view_for_buffer(
        &self,
        buffer_id: BufferId,
    ) -> Option<Option<usize>> {
        let state = self.buffers.get(&buffer_id)?;
        buffer_config_resolve::page_view(&state.language, &self.config)
    }

    /// Resolve the effective wrap_column for a buffer, considering language overrides.
    pub(super) fn resolve_wrap_column_for_buffer(&self, buffer_id: BufferId) -> Option<usize> {
        match self.buffers.get(&buffer_id) {
            Some(state) => buffer_config_resolve::wrap_column(&state.language, &self.config),
            None => self.config.editor.wrap_column,
        }
    }

    /// Get the preferred split for opening a file.
    /// If the active split has no label, use it (normal case).
    /// Otherwise find an unlabeled leaf so files don't open in labeled splits (e.g., sidebars).
    pub(super) fn preferred_split_for_file(&self) -> LeafId {
        let active = self.split_manager.active_split();
        if self.split_manager.get_label(active.into()).is_none() {
            return active;
        }
        self.split_manager.find_unlabeled_leaf().unwrap_or(active)
    }

    /// Open a file in "preview" (ephemeral) mode and return its buffer ID.
    ///
    /// Used for exploratory single-click opens from the file explorer. If the
    /// `file_explorer.preview_tabs` setting is disabled, this is equivalent to
    /// `open_file`.
    ///
    /// Semantics (see `Editor::preview` for the full invariants):
    /// - Preview is anchored to a specific split. At most one preview exists
    ///   editor-wide.
    /// - If the file is already open (deduped by canonical path, including
    ///   symlinks and relative paths, by delegating to `open_file_no_focus`),
    ///   just switch to it. No preview-state changes in either direction.
    /// - Otherwise, if there's an existing preview in the **same** target
    ///   split, close it and replace it. If it's in a **different** split,
    ///   promote it (walking away is commitment) and start a fresh preview
    ///   in the target split.
    /// - Skips writing to position history, so a string of exploratory
    ///   clicks doesn't flood back/forward navigation with stale entries.
    ///
    /// TODO(perf): Each preview swap today triggers LSP didClose + didOpen.
    /// For heavy language servers (rust-analyzer, tsserver) that's wasteful
    /// on rapid browsing. A future optimization is to keep the LSP session
    /// for the outgoing buffer until the user commits to the new one.
    pub fn open_file_preview(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Feature gate — fall back to normal open when preview tabs are off.
        if !self.config.file_explorer.preview_tabs {
            return self.open_file(path);
        }

        // Decide target split up-front. `open_file_no_focus` will target
        // the same one (it calls `preferred_split_for_file` internally),
        // so this mirrors its logic. If that invariant ever drifts we'd
        // open the preview in one split and track it in another.
        let target_split = self.preferred_split_for_file();

        // Snapshot the buffer IDs that already back a real file, so we can
        // tell "opened a previously-unknown file" from "switched to one
        // that was already open". We delegate the symlink/relative-path
        // dedup to `open_file_no_focus` (which canonicalizes) — any buffer
        // with a non-empty file path is a candidate match. Note: the
        // initial empty buffer has a `BufferKind::File` with an empty
        // `PathBuf`, and we deliberately exclude it here because
        // `open_file_no_focus` may *repurpose* that buffer (same ID, new
        // content) for the newly-opened file.
        let previously_file_backed: HashSet<BufferId> = self
            .buffers
            .iter()
            .filter_map(|(id, state)| {
                state.buffer.file_path().and_then(|p| {
                    if p.as_os_str().is_empty() {
                        None
                    } else {
                        Some(*id)
                    }
                })
            })
            .collect();

        // Route through `open_file` with position-history suppression.
        // Using the regular `open_file` path keeps all cross-cutting concerns
        // (LSP, language detection, split targeting, status message, plugin
        // hooks) consistent with a normal open.
        self.suppress_position_history_once = true;
        let open_result = self.open_file(path);
        self.suppress_position_history_once = false;
        let buffer_id = open_result?;
        let is_new = !previously_file_backed.contains(&buffer_id);

        // Already-open buffer: leave preview state untouched. A previously-
        // committed tab must not be demoted back to preview, and the existing
        // preview (if any, in whichever split) is still valid.
        if !is_new {
            return Ok(buffer_id);
        }

        // New buffer. Resolve the existing preview (if any) relative to the
        // target split.
        match self.preview.take() {
            Some((prev_split, old_id)) if prev_split == target_split => {
                // Same split: close the old preview so the new one takes its
                // place. If close fails (modified buffer — shouldn't happen
                // because edits promote, but defend in depth), demote the
                // orphan to a permanent tab rather than leaving behind an
                // italic "(preview)" tab that will never be replaced.
                if let Err(e) = self.close_buffer(old_id) {
                    tracing::warn!(
                        "preview: could not replace stale preview buffer {:?}, demoting to permanent: {}",
                        old_id,
                        e
                    );
                    if let Some(m) = self.buffer_metadata.get_mut(&old_id) {
                        m.is_preview = false;
                    }
                }
            }
            Some((_other_split, old_id)) => {
                // Different split: user walked away from the old preview
                // before this click. Promote it to permanent — their focus
                // moving to another split was the commitment signal.
                if let Some(m) = self.buffer_metadata.get_mut(&old_id) {
                    m.is_preview = false;
                }
            }
            None => {}
        }

        // Mark the new buffer as the preview, anchored to its split.
        if let Some(meta) = self.buffer_metadata.get_mut(&buffer_id) {
            meta.is_preview = true;
        }
        self.preview = Some((target_split, buffer_id));

        Ok(buffer_id)
    }

    /// Promote a specific buffer from preview to permanent, if it was in
    /// preview mode. No-op if the buffer is not currently a preview.
    pub(crate) fn promote_buffer_from_preview(&mut self, buffer_id: BufferId) {
        if let Some(m) = self.buffer_metadata.get_mut(&buffer_id) {
            m.is_preview = false;
        }
        if let Some((_, id)) = self.preview {
            if id == buffer_id {
                self.preview = None;
            }
        }
    }

    /// Promote the active buffer from preview to permanent, if applicable.
    /// Called on any buffer mutation so that touching a preview buffer
    /// commits it to a permanent tab.
    pub(crate) fn promote_active_buffer_from_preview(&mut self) {
        let id = self.active_buffer();
        self.promote_buffer_from_preview(id);
    }

    /// Promote the current preview, regardless of which buffer it points at.
    /// Used before layout changes (split, close-split, move-tab) where the
    /// preview invariant ("anchored to a specific split") would otherwise
    /// be broken by the operation itself.
    pub(crate) fn promote_current_preview(&mut self) {
        if let Some((_, id)) = self.preview.take() {
            if let Some(m) = self.buffer_metadata.get_mut(&id) {
                m.is_preview = false;
            }
        }
    }

    /// Promote the current preview if it belongs to a split other than
    /// `new_split`. Called from split-focus-change paths so that moving
    /// focus away from the preview's pane commits it.
    pub(crate) fn promote_preview_if_not_in_split(&mut self, new_split: LeafId) {
        if let Some((preview_split, _)) = self.preview {
            if preview_split != new_split {
                self.promote_current_preview();
            }
        }
    }

    /// Whether the given buffer is currently in preview (ephemeral) mode.
    /// Primarily for tests; production code should use `self.preview`.
    pub fn is_buffer_preview(&self, buffer_id: BufferId) -> bool {
        self.buffer_metadata
            .get(&buffer_id)
            .map(|m| m.is_preview)
            .unwrap_or(false)
    }

    /// Number of open buffers (including hidden/virtual buffers).
    /// Intended for tests that verify preview tabs don't accumulate.
    pub fn open_buffer_count(&self) -> usize {
        self.buffers.len()
    }

    /// The (split, buffer) tuple of the current preview tab, if any.
    /// Intended for tests that verify preview anchoring semantics.
    pub fn current_preview(&self) -> Option<(LeafId, BufferId)> {
        self.preview
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

        // Read cursor state from split view state
        let cursors = self.active_cursors();
        let cursor_id = cursors.primary_id();
        let old_position = cursors.primary().position;
        let old_anchor = cursors.primary().anchor;
        let old_sticky_column = cursors.primary().sticky_column;

        if let Some(state) = self.buffers.get(&buffer_id) {
            let has_line_index = state.buffer.line_count().is_some();
            let has_line_scan = state.buffer.has_line_feed_scan();
            let buffer_len = state.buffer.len();

            // Convert 1-indexed line to 0-indexed
            let target_line = line.saturating_sub(1);
            // Column is also 1-indexed, convert to 0-indexed
            let target_col = column.map(|c| c.saturating_sub(1)).unwrap_or(0);

            // Track the known exact line number for scanned large files,
            // since offset_to_position may not be able to reverse-resolve it accurately.
            let mut known_line: Option<usize> = None;

            let position = if has_line_scan && has_line_index {
                // Scanned large file: use tree metadata to find exact line offset
                let max_line = state.buffer.line_count().unwrap_or(1).saturating_sub(1);
                let actual_line = target_line.min(max_line);
                known_line = Some(actual_line);
                // Need mutable access to potentially read chunk data from disk
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state
                        .buffer
                        .resolve_line_byte_offset(actual_line)
                        .map(|offset| (offset + target_col).min(buffer_len))
                        .unwrap_or(0)
                } else {
                    0
                }
            } else {
                // Small file with full line starts or no line index:
                // use exact line position
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

            // For scanned large files, override the line number with the known exact value
            // since offset_to_position may fall back to proportional estimation.
            if let Some(line) = known_line {
                state.primary_cursor_line_number = crate::model::buffer::LineNumber::Absolute(line);
            }
        }
    }

    /// Select a range in the active buffer. Lines/columns are 1-indexed.
    /// The cursor moves to the end of the range and the anchor is set to the
    /// start, producing a visual selection.
    pub fn select_range(
        &mut self,
        start_line: usize,
        start_col: Option<usize>,
        end_line: usize,
        end_col: Option<usize>,
    ) {
        if start_line == 0 || end_line == 0 {
            return;
        }

        let buffer_id = self.active_buffer();

        let cursors = self.active_cursors();
        let cursor_id = cursors.primary_id();
        let old_position = cursors.primary().position;
        let old_anchor = cursors.primary().anchor;
        let old_sticky_column = cursors.primary().sticky_column;

        if let Some(state) = self.buffers.get(&buffer_id) {
            let buffer_len = state.buffer.len();

            // Convert 1-indexed to 0-indexed
            let start_line_0 = start_line.saturating_sub(1);
            let start_col_0 = start_col.map(|c| c.saturating_sub(1)).unwrap_or(0);
            let end_line_0 = end_line.saturating_sub(1);
            let end_col_0 = end_col.map(|c| c.saturating_sub(1)).unwrap_or(0);

            let max_line = state.buffer.line_count().unwrap_or(1).saturating_sub(1);

            let start_pos = state
                .buffer
                .line_col_to_position(start_line_0.min(max_line), start_col_0)
                .min(buffer_len);
            let end_pos = state
                .buffer
                .line_col_to_position(end_line_0.min(max_line), end_col_0)
                .min(buffer_len);

            let event = Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: end_pos,
                old_anchor,
                new_anchor: Some(start_pos),
                old_sticky_column,
                new_sticky_column: end_col_0,
            };

            let split_id = self.split_manager.active_split();
            let state = self.buffers.get_mut(&buffer_id).unwrap();
            let view_state = self.split_view_states.get_mut(&split_id).unwrap();
            state.apply(&mut view_state.cursors, &event);
        }
    }

    /// Go to an exact byte offset in the buffer (used in byte-offset mode for large files)
    pub fn goto_byte_offset(&mut self, offset: usize) {
        let buffer_id = self.active_buffer();

        let cursors = self.active_cursors();
        let cursor_id = cursors.primary_id();
        let old_position = cursors.primary().position;
        let old_anchor = cursors.primary().anchor;
        let old_sticky_column = cursors.primary().sticky_column;

        if let Some(state) = self.buffers.get(&buffer_id) {
            let buffer_len = state.buffer.len();
            let position = offset.min(buffer_len);

            let event = Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: position,
                old_anchor,
                new_anchor: None,
                old_sticky_column,
                new_sticky_column: 0,
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

        self.set_active_buffer(buffer_id);

        // Initialize per-buffer view state with config defaults.
        // Must happen AFTER set_active_buffer, because switch_buffer creates
        // the new BufferViewState with defaults (show_line_numbers=true).
        let active_split = self.split_manager.active_split();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
        }

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
        let bindings = self.keybindings.read().unwrap().get_all_bindings();

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
        if let Some((byte_pos, _, screen_x, screen_y)) = self.mouse_state.lsp_hover_state {
            if !self.mouse_state.lsp_hover_request_sent {
                self.hover.set_screen_position((screen_x, screen_y));
                match self.request_hover_at_position(byte_pos) {
                    Ok(true) => {
                        self.mouse_state.lsp_hover_request_sent = true;
                        return true;
                    }
                    Ok(false) => return false, // no server ready, retry later
                    Err(e) => {
                        tracing::debug!("Failed to request hover: {}", e);
                        return false;
                    }
                }
            }
        }
        false
    }

    /// Queue a file to be opened after the TUI starts.
    ///
    /// This is used for CLI file arguments to ensure they go through the same
    /// code path as interactive file opens, providing consistent error handling
    /// (e.g., encoding confirmation prompts are shown in the UI instead of crashing).
    /// Schedule hot exit recovery to run after the next batch of pending file opens.
    pub fn schedule_hot_exit_recovery(&mut self) {
        if self.config.editor.hot_exit {
            self.pending_hot_exit_recovery = true;
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn queue_file_open(
        &mut self,
        path: PathBuf,
        line: Option<usize>,
        column: Option<usize>,
        end_line: Option<usize>,
        end_column: Option<usize>,
        message: Option<String>,
        wait_id: Option<u64>,
    ) {
        self.pending_file_opens.push(super::PendingFileOpen {
            path,
            line,
            column,
            end_line,
            end_column,
            message,
            wait_id,
        });
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
                    // Navigate to line/column or select range if specified
                    if let (Some(line), Some(end_line)) = (pending_file.line, pending_file.end_line)
                    {
                        self.select_range(
                            line,
                            pending_file.column,
                            end_line,
                            pending_file.end_column,
                        );
                    } else if let Some(line) = pending_file.line {
                        self.goto_line_col(line, pending_file.column);
                    }
                    // Show hover message popup if specified
                    let has_popup = pending_file.message.is_some();
                    if let Some(ref msg) = pending_file.message {
                        self.show_file_message_popup(msg);
                    }
                    // Track wait ID for --wait support
                    if let Some(wait_id) = pending_file.wait_id {
                        let buffer_id = self.active_buffer();
                        self.wait_tracking.insert(buffer_id, (wait_id, has_popup));
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

        // Apply hot exit recovery if flagged (one-shot after CLI files are opened)
        if processed_any && self.pending_hot_exit_recovery {
            self.pending_hot_exit_recovery = false;
            match self.apply_hot_exit_recovery() {
                Ok(count) if count > 0 => {
                    tracing::info!("Hot exit: restored unsaved changes for {} buffer(s)", count);
                }
                Ok(_) => {}
                Err(e) => {
                    tracing::warn!("Failed to apply hot exit recovery: {}", e);
                }
            }
        }

        processed_any
    }

    /// Take and return completed wait IDs (for --wait support).
    pub fn take_completed_waits(&mut self) -> Vec<u64> {
        std::mem::take(&mut self.completed_waits)
    }

    /// Remove wait tracking for a given wait_id (e.g., when waiting client disconnects).
    pub fn remove_wait_tracking(&mut self, wait_id: u64) {
        self.wait_tracking.retain(|_, (wid, _)| *wid != wait_id);
    }
}
