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

}
