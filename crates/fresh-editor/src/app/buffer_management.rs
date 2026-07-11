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
use std::collections::HashSet;
use std::path::Path;
use std::sync::Arc;

use crate::model::event::{BufferId, Event, LeafId};
use crate::state::EditorState;

use super::buffer_config_resolve;
use super::Editor;

impl crate::app::window::Window {
    /// Resolve the effective line_wrap setting for a buffer, considering language overrides.
    pub fn resolve_line_wrap_for_buffer(&self, buffer_id: BufferId) -> bool {
        // Terminal buffers never wrap. Their content is column-formatted (ANSI,
        // aligned output), so wrapping is wrong — and wrapping a large scrollback
        // makes the scrollbar's visual-row index an O(all-lines) scan on every
        // frame, freezing the UI (fresh#2608). Forced off here so no global
        // toggle, language override, or per-buffer pin can turn it on.
        if self.is_terminal_buffer(buffer_id) {
            return false;
        }
        match self.buffers.get(&buffer_id) {
            Some(state) => buffer_config_resolve::line_wrap(&state.language, self.config()),
            None => self.config().editor.line_wrap,
        }
    }

    /// Resolve page view settings for a buffer from its language config.
    pub(crate) fn resolve_page_view_for_buffer(
        &self,
        buffer_id: BufferId,
    ) -> Option<Option<usize>> {
        let state = self.buffers.get(&buffer_id)?;
        buffer_config_resolve::page_view(&state.language, self.config())
    }

    /// Resolve the effective wrap_column for a buffer, considering language overrides.
    pub(crate) fn resolve_wrap_column_for_buffer(&self, buffer_id: BufferId) -> Option<usize> {
        match self.buffers.get(&buffer_id) {
            Some(state) => buffer_config_resolve::wrap_column(&state.language, self.config()),
            None => self.config().editor.wrap_column,
        }
    }

    /// Get the preferred split for opening a file.
    /// If the active split has no label, use it (normal case).
    /// Otherwise find an unlabeled leaf so files don't open in labeled splits (e.g., sidebars).
    pub(crate) fn preferred_split_for_file(&self) -> LeafId {
        let (mgr, _) = self
            .buffers
            .splits()
            .expect("active window must have a populated split layout");
        let active = mgr.active_split();
        if mgr.get_label(active.into()).is_none() {
            return active;
        }
        mgr.find_unlabeled_leaf().unwrap_or(active)
    }
}

impl Editor {
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
        // Dismiss any popup on the buffer being left. The explorer's preview
        // gesture (mouse single-click *and* keyboard arrow nav both route
        // through this function) is a focus shift away from the editor pane;
        // an LSP popup anchored to the previous buffer's cursor must not
        // follow the user across previews. Doing the cleanup here is the
        // single dedup point — both input paths get it for free, and the
        // popup is gone in the next render so a subsequent re-preview of the
        // same file doesn't resurrect it.
        if self.active_state().popups.is_visible() {
            self.clear_popups();
        }

        // Feature gate — fall back to normal open when preview tabs are off.
        if !self.config.file_explorer.preview_tabs {
            return self.open_file(path);
        }

        // Decide target split up-front. `open_file_no_focus` will target
        // the same one (it calls `preferred_split_for_file` internally),
        // so this mirrors its logic. If that invariant ever drifts we'd
        // open the preview in one split and track it in another.
        let target_split = self.active_window().preferred_split_for_file();

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
            .buffers()
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
        //
        // `OpenKind::Preview` additionally tells the open path that this is a
        // browse, not a deliberate open, so the `after_file_open` plugin hook
        // is deferred — otherwise plugins raise intrusive UI (e.g. the
        // asm-lsp `.asm-lsp.toml` config-offer popup) over each file the user
        // arrows past. The hook fires later if/when this preview is escalated
        // to a permanent tab.
        self.active_window_mut().suppress_position_history_once = true;
        let open_result =
            self.open_file_with_kind(path, super::file_open_orchestrators::OpenKind::Preview);
        self.active_window_mut().suppress_position_history_once = false;
        let buffer_id = open_result?;
        let is_new = !previously_file_backed.contains(&buffer_id);

        // Already-open buffer: leave preview state untouched. A previously-
        // committed tab must not be demoted back to preview, and the existing
        // preview (if any, in whichever split) is still valid.
        if !is_new {
            return Ok(buffer_id);
        }

        // New buffer. Resolve the existing preview (if any) relative to the
        // target split. `preview.take()` clears the single source of truth;
        // each arm below decides whether the displaced buffer was *committed*
        // (escalated → fire its deferred `after_file_open`) or merely
        // *discarded* (closed → no hook).
        match self.active_window_mut().preview.take() {
            Some((prev_split, old_id)) if prev_split == target_split => {
                // Same split: close the old preview so the new one takes its
                // place. Replacement is not a commitment, so no hook. If close
                // fails (modified buffer — shouldn't happen because edits
                // promote, but defend in depth), the buffer stays open as a
                // permanent tab, which *is* a commitment → fire its deferred
                // hook.
                if let Err(e) = self.close_buffer(old_id) {
                    tracing::warn!(
                        "preview: could not replace stale preview buffer {:?}, demoting to permanent: {}",
                        old_id,
                        e
                    );
                    self.active_window().fire_deferred_after_file_open(old_id);
                }
            }
            Some((_other_split, old_id)) => {
                // Different split: user walked away from the old preview
                // before this click. Their focus moving to another split was
                // the commitment signal → escalate it and fire the hook.
                self.active_window().fire_deferred_after_file_open(old_id);
            }
            None => {}
        }

        // Anchor the new buffer as the preview — the single source of truth.
        self.active_window_mut().preview = Some((target_split, buffer_id));

        Ok(buffer_id)
    }

    // `promote_buffer_from_preview`, `promote_active_buffer_from_preview`,
    // `promote_current_preview`, `promote_preview_if_not_in_split`,
    // `is_buffer_preview`, `current_preview` moved to `impl Window`
    // (in `window.rs`). Editor callers reach them via
    // `self.active_window_mut().X(...)`.

    /// Re-point every buffer whose file path sits at or under `old_root`
    /// to the equivalent location under `new_root`. Returns the ids of
    /// the buffers that were actually relocated.
    ///
    /// Handles three shapes of path change uniformly:
    ///
    /// - Single-file rename: `old_root = /a/foo.txt`, `new_root = /a/bar.txt`
    ///   → the buffer for foo.txt re-points to bar.txt.
    /// - Directory rename: `old_root = /a/dir`, `new_root = /a/renamed`
    ///   → every buffer for a file inside `dir` (e.g. `/a/dir/x.txt`)
    ///   re-points under `/a/renamed` (`/a/renamed/x.txt`).
    /// - Cut+paste move: `old_root = /a/foo.txt`, `new_root = /b/foo.txt`
    ///   → the buffer for the moved file re-points to its new home.
    ///
    /// For each affected buffer we update the persistence path on the
    /// Buffer itself, rebuild the `BufferMetadata::kind` (new path + new
    /// LSP URI), and recompute the display name. Without this, a save
    /// on the buffer would write to the old (now gone or stale) path
    /// and silently resurrect / duplicate the file.
    pub(crate) fn relocate_buffers_for_rename(
        &mut self,
        old_root: &std::path::Path,
        new_root: &std::path::Path,
    ) -> Vec<BufferId> {
        let affected = self.buffer_ids_under_path(old_root);
        for &id in &affected {
            let Some(state) = self
                .windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .get(&id)
            else {
                continue;
            };
            let Some(current) = state.buffer.file_path().map(|p| p.to_path_buf()) else {
                continue;
            };
            // For buffers equal to old_root, the new path is simply
            // new_root. For buffers under old_root (directory case),
            // strip the old prefix and re-root under new_root.
            let new_path = if current == old_root {
                new_root.to_path_buf()
            } else if let Ok(relative) = current.strip_prefix(old_root) {
                new_root.join(relative)
            } else {
                // Defensive: buffer_ids_under_path already filtered, so
                // this shouldn't happen. Skip rather than corrupt state.
                continue;
            };

            if let Some(state) = self
                .windows
                .get_mut(&self.active_window)
                .map(|w| &mut w.buffers)
                .expect("active window present")
                .get_mut(&id)
            {
                state.buffer.rename_file_path(new_path.clone());
            }
            let file_uri = super::types::LspUri::from_host_path(
                &new_path,
                self.authority().path_translation.as_ref(),
            );
            let display_name =
                super::BufferMetadata::display_name_for_path(&new_path, self.working_dir());
            if let Some(metadata) = self.active_window_mut().buffer_metadata.get_mut(&id) {
                metadata.kind = super::BufferKind::File {
                    path: new_path.clone(),
                    uri: file_uri,
                };
                metadata.display_name = display_name;
            }
        }
        affected
    }

    // `promote_current_preview`, `promote_preview_if_not_in_split`,
    // `is_buffer_preview`, `current_preview` moved to `impl Window`.

    /// Number of open buffers (including hidden/virtual buffers).
    /// Intended for tests that verify preview tabs don't accumulate.
    pub fn open_buffer_count(&self) -> usize {
        self.active_window().buffers.len()
    }

    /// Whether the active buffer has a full line index available.
    ///
    /// Always true for normal files; false for large files opened in
    /// byte-offset mode before a line-index scan has run — in that state
    /// there is no line→offset mapping, so line-based navigation (`Ctrl+G`,
    /// Quick Open `:N`) must first offer to scan the file.
    pub(crate) fn active_buffer_has_line_index(&self) -> bool {
        self.buffers()
            .get(&self.active_buffer())
            .is_none_or(|s| s.buffer.line_count().is_some())
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
        let old_cursor = *cursors.primary();
        let cursor_id = cursors.primary_id();
        let old_position = cursors.primary().position;
        let old_anchor = cursors.primary().anchor;
        let old_sticky_column = cursors.primary().sticky_column;

        if let Some(state) = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&buffer_id)
        {
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
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&buffer_id)
                {
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

            // Preserve anchor if deselect_on_move is false (Emacs mark mode)
            let new_anchor = if old_cursor.deselect_on_move {
                None
            } else {
                old_cursor.anchor
            };

            let event = Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: position,
                old_anchor,
                new_anchor,
                old_sticky_column,
                new_sticky_column: Some(target_col),
            };

            let split_id = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split();
            self.active_window_mut()
                .apply_event_to_buffer(buffer_id, split_id, &event);

            // For scanned large files, override the line number with the known exact value
            // since offset_to_position may fall back to proportional estimation.
            if let Some(line) = known_line {
                if let Some(state) = self.active_window_mut().buffers.get_mut(&buffer_id) {
                    state.primary_cursor_line_number =
                        crate::model::buffer::LineNumber::Absolute(line);
                }
            }

            // Center the target line in the viewport. The default
            // `ensure_visible` behavior only scrolls just enough to reveal
            // the cursor, which pins a forward jump to the bottom row — and
            // for live-preview jumps (Quick Open `:N`, Goto Line prompt) the
            // suggestion/prompt popup overlays the bottom of the screen,
            // obscuring the very line the user is navigating to. Recentering
            // puts the target in the middle so it stays visible.
            self.apply_event_to_active_buffer(&Event::Recenter);
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

        if let Some(state) = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&buffer_id)
        {
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
                new_sticky_column: Some(end_col_0),
            };

            let split_id = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split();
            self.active_window_mut()
                .apply_event_to_buffer(buffer_id, split_id, &event);
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

        if let Some(state) = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&buffer_id)
        {
            let buffer_len = state.buffer.len();
            let position = offset.min(buffer_len);

            let event = Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: position,
                old_anchor,
                new_anchor: None,
                old_sticky_column,
                new_sticky_column: None,
            };

            let split_id = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split();
            self.active_window_mut()
                .apply_event_to_buffer(buffer_id, split_id, &event);
        }
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> BufferId {
        // Save current position before switching to new buffer
        self.active_window_mut()
            .position_history
            .commit_pending_movement();

        // Explicitly record current position before switching
        let cursors = self.active_cursors();
        let position = cursors.primary().position;
        let anchor = cursors.primary().anchor;
        let active_buffer_id = self.active_buffer();
        let ph = &mut self.active_window_mut().position_history;
        ph.record_movement(active_buffer_id, position, anchor);
        ph.commit_pending_movement();

        let buffer_id = self.alloc_buffer_id();

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            Arc::clone(&self.authority().filesystem),
        );
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);
        // Set default line ending for new buffers from config
        state
            .buffer
            .set_default_line_ending(self.config.editor.default_line_ending.to_line_ending());
        state.reference_highlight_overlay.enabled = self.config.editor.highlight_occurrences;
        // Whitespace indicator visibility: resolve from the user's editor config
        // so a brand-new buffer shows the same indicators as an opened file.
        // Without this the buffer kept `WhitespaceVisibility::default()` (tabs on
        // / spaces off), so configured space indicators never showed in a new
        // file (issue #2580).
        let mut whitespace =
            crate::config::WhitespaceVisibility::from_editor_config(&self.config.editor);
        if let Some(lang_config) = self.config.languages.get(&state.language) {
            whitespace = whitespace.with_language_tab_override(lang_config.show_whitespace_tabs);
        }
        state.buffer_settings.whitespace = whitespace;
        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .insert(buffer_id, state);
        self.active_window_mut()
            .event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());
        self.active_window_mut()
            .buffer_metadata
            .insert(buffer_id, crate::app::types::BufferMetadata::new());

        self.set_active_buffer(buffer_id);

        // Initialize per-buffer view state with config defaults.
        // Must happen AFTER set_active_buffer, because switch_buffer creates
        // the new BufferViewState with defaults (show_line_numbers=true).
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        let line_wrap = self.active_window().resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self
            .active_window()
            .resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&active_split)
        {
            view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                line_numbers: self.config.editor.line_numbers,
                highlight_current_line: self.config.editor.highlight_current_line,
                line_wrap,
                wrap_indent: self.config.editor.wrap_indent,
                wrap_column,
                rulers: self.config.editor.rulers.clone(),
                scroll_offset: self.config.editor.scroll_offset,
            });
        }

        self.active_window_mut().status_message = Some(t!("buffer.new").to_string());

        buffer_id
    }

    /// Get the current mouse hover state for testing
    /// Returns Some((byte_position, screen_x, screen_y)) if hovering over text
    pub fn get_mouse_hover_state(&self) -> Option<(usize, u16, u16)> {
        self.active_window()
            .mouse_state
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
        if let Some((byte_pos, _, screen_x, screen_y)) =
            self.active_window_mut().mouse_state.lsp_hover_state
        {
            if !self.active_window_mut().mouse_state.lsp_hover_request_sent {
                self.active_window_mut()
                    .hover
                    .set_screen_position((screen_x, screen_y));
                match self.request_hover_at_position(byte_pos) {
                    Ok(true) => {
                        self.active_window_mut().mouse_state.lsp_hover_request_sent = true;
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
