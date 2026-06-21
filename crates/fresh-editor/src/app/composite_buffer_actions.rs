//! Composite buffer management actions
//!
//! This module handles creating, managing, and closing composite buffers
//! which display multiple source buffers in a single tab.
//!
//! ## Cursor and Selection Handling
//!
//! Composite buffers re-implement cursor movement and selection rather than routing
//! to the underlying source buffers. This is a deliberate trade-off because:
//!
//! - Composite buffers use a display-row coordinate system with alignment rows that
//!   may not have 1:1 mapping to source lines (e.g., padding rows for deleted lines)
//! - The cursor position is shared across all panes but each pane may have different
//!   content at the same display row
//! - Horizontal scroll must sync across panes for side-by-side comparison
//!
//! Routing to underlying buffers was considered but would require complex coordinate
//! translation and wouldn't handle padding rows or synced scrolling naturally.

use crate::app::types::BufferMetadata;
use crate::app::Editor;
use crate::model::composite_buffer::{CompositeBuffer, CompositeLayout, LineAlignment, SourcePane};
use crate::model::event::{BufferId, LeafId};
use crate::view::composite_view::CompositeViewState;
use anyhow::Result as AnyhowResult;
use unicode_segmentation::UnicodeSegmentation;

/// Information about the current cursor line needed for movement operations
struct CursorLineInfo {
    content: String,
    length: usize,
    pane_width: usize,
}

/// Direction for cursor movement
#[derive(Clone, Copy)]
enum CursorMovement {
    Up,
    Down,
    Left,
    Right,
    LineStart,
    LineEnd,
    WordLeft,
    WordRight,
    WordEnd, // Move to end of current word
}

/// Find the previous word boundary position in a line
fn find_word_boundary_left(line: &str, from_column: usize) -> usize {
    let graphemes: Vec<&str> = line.graphemes(true).collect();
    let mut pos = from_column;
    // Skip spaces going left
    while pos > 0
        && graphemes
            .get(pos.saturating_sub(1))
            .is_some_and(|g| g.chars().all(|c| c.is_whitespace()))
    {
        pos -= 1;
    }
    // Skip word chars going left
    while pos > 0
        && graphemes
            .get(pos.saturating_sub(1))
            .is_some_and(|g| !g.chars().all(|c| c.is_whitespace()))
    {
        pos -= 1;
    }
    pos
}

/// Find the next word boundary position in a line
fn find_word_boundary_right(line: &str, from_column: usize, line_length: usize) -> usize {
    let graphemes: Vec<&str> = line.graphemes(true).collect();
    let mut pos = from_column;
    // Skip word chars going right
    while pos < graphemes.len() && !graphemes[pos].chars().all(|c| c.is_whitespace()) {
        pos += 1;
    }
    // Skip spaces going right
    while pos < graphemes.len() && graphemes[pos].chars().all(|c| c.is_whitespace()) {
        pos += 1;
    }
    pos.min(line_length)
}

/// Find the end of the current word (or end of next word if on whitespace)
fn find_word_end_right(line: &str, from_column: usize, line_length: usize) -> usize {
    let graphemes: Vec<&str> = line.graphemes(true).collect();
    let mut pos = from_column;

    // If on whitespace, skip it first
    while pos < graphemes.len() && graphemes[pos].chars().all(|c| c.is_whitespace()) {
        pos += 1;
    }

    // If on punctuation, consume it
    if pos < graphemes.len()
        && !graphemes[pos]
            .chars()
            .any(|c| c.is_alphanumeric() || c == '_')
        && !graphemes[pos].chars().all(|c| c.is_whitespace())
    {
        while pos < graphemes.len()
            && !graphemes[pos]
                .chars()
                .any(|c| c.is_alphanumeric() || c == '_')
            && !graphemes[pos].chars().all(|c| c.is_whitespace())
        {
            pos += 1;
        }
    } else {
        // Skip word chars (alphanumeric + underscore)
        while pos < graphemes.len()
            && graphemes[pos]
                .chars()
                .any(|c| c.is_alphanumeric() || c == '_')
        {
            pos += 1;
        }
    }

    pos.min(line_length)
}

impl crate::app::window::Window {
    /// Check if a buffer is a composite buffer
    pub fn is_composite_buffer(&self, buffer_id: BufferId) -> bool {
        self.composite_buffers.contains_key(&buffer_id)
    }

    /// Get a composite buffer by ID
    pub fn get_composite(&self, buffer_id: BufferId) -> Option<&CompositeBuffer> {
        self.composite_buffers.get(&buffer_id)
    }

    /// Get a mutable composite buffer by ID
    pub fn get_composite_mut(&mut self, buffer_id: BufferId) -> Option<&mut CompositeBuffer> {
        self.composite_buffers.get_mut(&buffer_id)
    }

    /// Cursor info for the active composite (diff-view) buffer.
    ///
    /// Returns `None` when the active buffer is not a composite buffer.
    /// Otherwise yields `(focused_pane, pane_count, per_pane_source_line)`
    /// where each entry of `per_pane_source_line` is the 0-indexed source
    /// line shown in that pane on the cursor's aligned row (`None` where the
    /// pane has no content on that row — e.g. the blank side of an insertion
    /// or deletion). Plugins use this to map a side-by-side cursor back to a
    /// concrete (file version, line) so they can open it on disk.
    ///
    /// The cursor can rest on a hunk-header / spacer alignment row that has no
    /// source content on any pane (e.g. at the very top of the diff, before
    /// the first context line). From such a row "open the file" should still
    /// target a real line, so the cursor row is first resolved to the nearest
    /// row that carries content — scanning down (into the hunk the header
    /// introduces) before up. Rows that already have content on at least one
    /// pane are used as-is, preserving per-pane `None` for the blank side of
    /// an add/delete.
    pub fn active_composite_cursor_info(&self) -> Option<(usize, usize, Vec<Option<usize>>)> {
        let (split_id, buffer_id) = self.effective_active_pair();
        if !self.is_composite_buffer(buffer_id) {
            return None;
        }
        let composite = self.composite_buffers.get(&buffer_id)?;
        let view_state = self.composite_view_states.get(&(split_id, buffer_id))?;
        let pane_count = composite.sources.len();
        let row_count = composite.alignment.row_count();

        let row_has_content = |r: usize| -> bool {
            composite
                .alignment
                .get_row(r)
                .map(|row| (0..pane_count).any(|i| row.get_pane_line(i).is_some()))
                .unwrap_or(false)
        };

        let start = view_state.cursor_row;
        let resolved_row = if row_has_content(start) {
            Some(start)
        } else {
            let mut found = None;
            let mut delta = 1;
            while delta < row_count {
                if start + delta < row_count && row_has_content(start + delta) {
                    found = Some(start + delta);
                    break;
                }
                if start >= delta && row_has_content(start - delta) {
                    found = Some(start - delta);
                    break;
                }
                delta += 1;
            }
            found
        };

        let row = resolved_row.and_then(|r| composite.alignment.get_row(r));
        let lines: Vec<Option<usize>> = (0..pane_count)
            .map(|i| {
                row.and_then(|r| r.get_pane_line(i))
                    .map(|line_ref| line_ref.line)
            })
            .collect();
        Some((view_state.focused_pane, pane_count, lines))
    }

    /// Set the line alignment for a composite buffer
    pub fn set_composite_alignment(&mut self, buffer_id: BufferId, alignment: LineAlignment) {
        if let Some(composite) = self.composite_buffers.get_mut(&buffer_id) {
            composite.set_alignment(alignment);
        }
    }

    /// Close a composite buffer and clean up associated state
    pub fn close_composite_buffer(&mut self, buffer_id: BufferId) {
        self.composite_buffers.remove(&buffer_id);
        self.buffer_metadata.remove(&buffer_id);
        self.composite_view_states
            .retain(|(_, bid), _| *bid != buffer_id);
    }

    /// Switch focus to the next pane in a composite buffer
    pub fn composite_focus_next(&mut self, split_id: LeafId, buffer_id: BufferId) {
        if let Some(composite) = self.composite_buffers.get_mut(&buffer_id) {
            composite.focus_next();
        }
        if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id)) {
            view_state.focus_next_pane();
        }
    }

    /// Switch focus to the previous pane in a composite buffer
    pub fn composite_focus_prev(&mut self, split_id: LeafId, buffer_id: BufferId) {
        if let Some(composite) = self.composite_buffers.get_mut(&buffer_id) {
            composite.focus_prev();
        }
        if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id)) {
            view_state.focus_prev_pane();
        }
    }

    /// Effective viewport height for composite-buffer scrolling on
    /// `split_id`. Subtracts the one-row composite header from the
    /// raw split viewport. Falls back to a 24-row default when the
    /// split doesn't yet have a viewport (pre-first-render).
    fn get_composite_viewport_height(&self, split_id: LeafId) -> usize {
        const COMPOSITE_HEADER_HEIGHT: u16 = 1;
        const DEFAULT_VIEWPORT_HEIGHT: usize = 24;

        self.buffers
            .splits()
            .map(|(_, vs)| vs)
            .expect("window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.viewport.height.saturating_sub(COMPOSITE_HEADER_HEIGHT) as usize)
            .unwrap_or(DEFAULT_VIEWPORT_HEIGHT)
    }

    /// Mirror the composite view's cursor row/column back onto the
    /// underlying buffer's `EditorState` and the active split's view
    /// state. Called from hunk-navigation handlers so the status-bar
    /// `Ln/Col` reflects the new alignment row instead of stale
    /// pre-jump data.
    fn sync_editor_cursor_from_composite(&mut self, split_id: LeafId, buffer_id: BufferId) {
        let (cursor_row, cursor_column, focused_pane) = self
            .composite_view_states
            .get(&(split_id, buffer_id))
            .map(|vs| (vs.cursor_row, vs.cursor_column, vs.focused_pane))
            .unwrap_or((0, 0, 0));

        let display_line = self
            .composite_buffers
            .get(&buffer_id)
            .and_then(|composite| composite.alignment.get_row(cursor_row))
            .and_then(|row| row.get_pane_line(focused_pane))
            .map(|line_ref| line_ref.line)
            .unwrap_or(cursor_row);

        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.primary_cursor_line_number =
                crate::model::buffer::LineNumber::Absolute(display_line);
        }

        // Write the cursor column into the same leaf the composite is keyed
        // under (`split_id`), not a recomputed `active_split()` — for the
        // Review Diff group tab those differ and the status-bar column would
        // land on the wrong leaf's view state.
        if let Some((_, vs_map)) = self.buffers.splits_mut() {
            if let Some(view_state) = vs_map.get_mut(&split_id) {
                view_state.cursors.primary_mut().position = cursor_column;
            }
        }
    }

    /// Navigate to the next hunk (composite-buffer diff view) on
    /// `split_id`. Centers the new hunk header roughly a third of the
    /// way down the viewport and syncs the editor cursor so the status
    /// bar's Ln/Col follows. Returns `true` iff a next hunk existed.
    pub fn composite_next_hunk(&mut self, split_id: LeafId, buffer_id: BufferId) -> bool {
        let viewport_height = self.get_composite_viewport_height(split_id);
        let moved = if let (Some(composite), Some(view_state)) = (
            self.composite_buffers.get(&buffer_id),
            self.composite_view_states.get_mut(&(split_id, buffer_id)),
        ) {
            if let Some(next_row) = composite.alignment.next_hunk_row(view_state.cursor_row) {
                view_state.cursor_row = next_row;
                let context_above = viewport_height / 3;
                view_state.scroll_row = next_row.saturating_sub(context_above);
                true
            } else {
                false
            }
        } else {
            false
        };
        if moved {
            self.sync_editor_cursor_from_composite(split_id, buffer_id);
        }
        moved
    }

    /// Navigate to the previous hunk in a composite-buffer diff view.
    /// See `composite_next_hunk` for behaviour.
    pub fn composite_prev_hunk(&mut self, split_id: LeafId, buffer_id: BufferId) -> bool {
        let viewport_height = self.get_composite_viewport_height(split_id);
        let moved = if let (Some(composite), Some(view_state)) = (
            self.composite_buffers.get(&buffer_id),
            self.composite_view_states.get_mut(&(split_id, buffer_id)),
        ) {
            if let Some(prev_row) = composite.alignment.prev_hunk_row(view_state.cursor_row) {
                view_state.cursor_row = prev_row;
                let context_above = viewport_height / 3;
                view_state.scroll_row = prev_row.saturating_sub(context_above);
                true
            } else {
                false
            }
        } else {
            false
        };
        if moved {
            self.sync_editor_cursor_from_composite(split_id, buffer_id);
        }
        moved
    }

    /// Hunk navigation entry point that resolves `split_id` from this
    /// window's active split. Used by keybinding handlers that don't
    /// carry a split id.
    pub fn composite_next_hunk_active(&mut self, buffer_id: BufferId) -> bool {
        // Inner group leaf (see `handle_composite_action`): the composite
        // lives in the focused group leaf, not the outer active split.
        let split_id = self.effective_active_pair().0;
        self.composite_next_hunk(split_id, buffer_id)
    }

    /// `composite_prev_hunk` flavour for the active split.
    pub fn composite_prev_hunk_active(&mut self, buffer_id: BufferId) -> bool {
        let split_id = self.effective_active_pair().0;
        self.composite_prev_hunk(split_id, buffer_id)
    }

    /// Scroll a composite-buffer view by `delta` rows, clamped to the
    /// composite's row count. No-op if the buffer or view state is
    /// missing.
    pub fn composite_scroll(&mut self, split_id: LeafId, buffer_id: BufferId, delta: isize) {
        if let (Some(composite), Some(view_state)) = (
            self.composite_buffers.get(&buffer_id),
            self.composite_view_states.get_mut(&(split_id, buffer_id)),
        ) {
            let max_row = composite.row_count().saturating_sub(1);
            view_state.scroll(delta, max_row);
        }
    }

    /// Scroll a composite-buffer view to absolute row `row`, clamped.
    pub fn composite_scroll_to(&mut self, split_id: LeafId, buffer_id: BufferId, row: usize) {
        if let (Some(composite), Some(view_state)) = (
            self.composite_buffers.get(&buffer_id),
            self.composite_view_states.get_mut(&(split_id, buffer_id)),
        ) {
            let max_row = composite.row_count().saturating_sub(1);
            view_state.set_scroll_row(row, max_row);
        }
    }
}

impl Editor {
    // =========================================================================
    // Layout Flush (synchronous state materialization)
    // =========================================================================

    /// Force-materialize render-dependent state for all visible splits.
    ///
    /// This is the editor's equivalent of iOS `layoutIfNeeded()` or browser
    /// forced reflow. It ensures that `CompositeViewState` entries exist for
    /// any visible composite buffer, using the split's current viewport
    /// dimensions. After calling this, commands like `compositeNextHunk` can
    /// safely read and modify view state that would otherwise only exist after
    /// the next render cycle.
    pub fn flush_layout(&mut self) {
        use crate::view::composite_view::CompositeViewState;

        let visible = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .get_visible_buffers(ratatui::layout::Rect {
                x: 0,
                y: 0,
                width: self.terminal_width,
                height: self.terminal_height,
            });

        for (split_id, buffer_id, _area) in &visible {
            // Only process composite buffers
            if let Some(composite) = self.active_window().composite_buffers.get(buffer_id) {
                let pane_count = composite.pane_count();
                self.active_window_mut()
                    .composite_view_states
                    .entry((*split_id, *buffer_id))
                    .or_insert_with(|| CompositeViewState::new(*buffer_id, pane_count));
            }
        }
    }

    // =========================================================================
    // Composite Buffer Methods
    // =========================================================================
    //
    // The simple read/write helpers (`is_composite_buffer`, `get_composite`,
    // `get_composite_mut`, `set_composite_alignment`, `close_composite_buffer`,
    // `composite_focus_next`, `composite_focus_prev`) live on `impl Window`
    // above — call them via `self.active_window().X` /
    // `self.active_window_mut().X` from `impl Editor`. The remaining
    // methods below stay on `impl Editor` because they read editor-global
    // state (`terminal_width`/`height`, plugin manager, status messages)
    // alongside their window-scoped work.

    /// Get or create composite view state for a split
    pub fn get_composite_view_state(
        &mut self,
        split_id: LeafId,
        buffer_id: BufferId,
    ) -> Option<&mut CompositeViewState> {
        if !self
            .active_window()
            .composite_buffers
            .contains_key(&buffer_id)
        {
            return None;
        }

        let pane_count = self
            .active_window()
            .composite_buffers
            .get(&buffer_id)?
            .pane_count();

        Some(
            self.active_window_mut()
                .composite_view_states
                .entry((split_id, buffer_id))
                .or_insert_with(|| CompositeViewState::new(buffer_id, pane_count)),
        )
    }

    /// Create a new composite buffer
    ///
    /// # Arguments
    /// * `name` - Display name for the composite buffer (shown in tab)
    /// * `mode` - Mode for keybindings (e.g., "diff-view")
    /// * `layout` - How panes are arranged (side-by-side, stacked, unified)
    /// * `sources` - Source panes to display
    ///
    /// # Returns
    /// The ID of the newly created composite buffer
    pub fn create_composite_buffer(
        &mut self,
        name: String,
        mode: String,
        layout: CompositeLayout,
        sources: Vec<SourcePane>,
    ) -> BufferId {
        let buffer_id = self.alloc_buffer_id();

        let composite =
            CompositeBuffer::new(buffer_id, name.clone(), mode.clone(), layout, sources);
        self.active_window_mut()
            .composite_buffers
            .insert(buffer_id, composite);

        // Add metadata for display
        // Note: We use virtual_buffer() but override hidden_from_tabs since composite buffers
        // should be visible in tabs (unlike their hidden source panes)
        let mut metadata = BufferMetadata::virtual_buffer(name.clone(), mode.clone(), true);
        metadata.hidden_from_tabs = false;
        self.active_window_mut()
            .buffer_metadata
            .insert(buffer_id, metadata);

        // Create an EditorState entry so the buffer can be shown in tabs and via showBuffer()
        // The actual content rendering is handled by the composite renderer
        let mut state = crate::state::EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            std::sync::Arc::clone(&self.authority().filesystem),
        );
        state.is_composite_buffer = true;
        state.editing_disabled = true;
        state.mode = mode;
        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .insert(buffer_id, state);
        // Create an event log entry (required for many editor operations)
        self.active_window_mut()
            .event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // NOTE: the composite is intentionally NOT auto-attached to the
        // active split here. Callers place it explicitly — `showBuffer` for a
        // standalone drill-down (which adds it to the active split via
        // `set_active_buffer`), or `setBufferGroupPanelBuffer` for an in-panel
        // review center. Attaching here too left the composite as a stray tab
        // in the main split, spawning "[No Name]" tabs when it was later
        // closed on a file switch.
        buffer_id
    }

    // `set_composite_alignment`, `close_composite_buffer`,
    // `composite_focus_next`, `composite_focus_prev` moved to
    // `impl Window` above. Editor callers reach them via
    // `self.active_window_mut().X(...)`.

    // =========================================================================
    // Action Handling for Composite Buffers
    // =========================================================================

    /// Get information about the line at the cursor position
    fn get_cursor_line_info(&self, split_id: LeafId, buffer_id: BufferId) -> CursorLineInfo {
        let composite = self.active_window().composite_buffers.get(&buffer_id);
        let view_state = self
            .active_window()
            .composite_view_states
            .get(&(split_id, buffer_id));

        if let (Some(composite), Some(view_state)) = (composite, view_state) {
            let pane_line = composite
                .alignment
                .get_row(view_state.cursor_row)
                .and_then(|row| row.get_pane_line(view_state.focused_pane));

            tracing::debug!(
                "get_cursor_line_info: cursor_row={}, focused_pane={}, pane_line={:?}",
                view_state.cursor_row,
                view_state.focused_pane,
                pane_line
            );

            let line_bytes = pane_line.and_then(|line_ref| {
                let source = composite.sources.get(view_state.focused_pane)?;
                self.windows
                    .get(&self.active_window)
                    .map(|w| &w.buffers)
                    .expect("active window present")
                    .get(&source.buffer_id)?
                    .buffer
                    .get_line(line_ref.line)
            });

            let content = line_bytes
                .as_ref()
                .map(|b| {
                    let s = String::from_utf8_lossy(b).to_string();
                    // Strip trailing newline - cursor shouldn't go past end of visible content
                    s.trim_end_matches('\n').trim_end_matches('\r').to_string()
                })
                .unwrap_or_default();
            let length = content.graphemes(true).count();
            let pane_width = view_state
                .pane_widths
                .get(view_state.focused_pane)
                .copied()
                .unwrap_or(40) as usize;

            CursorLineInfo {
                content,
                length,
                pane_width,
            }
        } else {
            CursorLineInfo {
                content: String::new(),
                length: 0,
                pane_width: 40,
            }
        }
    }

    /// Apply a cursor movement to a composite view state
    fn apply_cursor_movement(
        &mut self,
        split_id: LeafId,
        buffer_id: BufferId,
        movement: CursorMovement,
        line_info: &CursorLineInfo,
        viewport_height: usize,
    ) {
        let max_row = self
            .active_window_mut()
            .composite_buffers
            .get(&buffer_id)
            .map(|c| c.row_count().saturating_sub(1))
            .unwrap_or(0);

        let is_vertical = matches!(movement, CursorMovement::Up | CursorMovement::Down);
        let mut wrapped_to_new_line = false;

        // Get alignment reference for wrap checks
        let win = self.active_window_mut();
        let composite = win.composite_buffers.get(&buffer_id);

        if let Some(view_state) = win.composite_view_states.get_mut(&(split_id, buffer_id)) {
            match movement {
                CursorMovement::Down => {
                    view_state.move_cursor_down(max_row, viewport_height);
                }
                CursorMovement::Up => {
                    view_state.move_cursor_up(viewport_height);
                }
                CursorMovement::Left => {
                    if view_state.cursor_column > 0 {
                        view_state.move_cursor_left();
                    } else if view_state.cursor_row > 0 {
                        // Try to wrap to end of previous line - find a row with content
                        if let Some(composite) = composite {
                            let focused_pane = view_state.focused_pane;
                            let mut target_row = view_state.cursor_row - 1;
                            while target_row > 0 {
                                if let Some(row) = composite.alignment.get_row(target_row) {
                                    if row.get_pane_line(focused_pane).is_some() {
                                        break;
                                    }
                                }
                                target_row -= 1;
                            }
                            // Only wrap if target row has content
                            if let Some(row) = composite.alignment.get_row(target_row) {
                                if row.get_pane_line(focused_pane).is_some() {
                                    view_state.cursor_row = target_row;
                                    if view_state.cursor_row < view_state.scroll_row {
                                        view_state.scroll_row = view_state.cursor_row;
                                    }
                                    wrapped_to_new_line = true;
                                }
                            }
                        }
                    }
                }
                CursorMovement::Right => {
                    if view_state.cursor_column < line_info.length {
                        view_state.move_cursor_right(line_info.length, line_info.pane_width);
                    } else if view_state.cursor_row < max_row {
                        // Try to wrap to start of next line - find a row with content
                        if let Some(composite) = composite {
                            let focused_pane = view_state.focused_pane;
                            let mut target_row = view_state.cursor_row + 1;
                            while target_row < max_row {
                                if let Some(row) = composite.alignment.get_row(target_row) {
                                    if row.get_pane_line(focused_pane).is_some() {
                                        break;
                                    }
                                }
                                target_row += 1;
                            }
                            // Only wrap if target row has content
                            if let Some(row) = composite.alignment.get_row(target_row) {
                                if row.get_pane_line(focused_pane).is_some() {
                                    view_state.cursor_row = target_row;
                                    view_state.cursor_column = 0;
                                    view_state.sticky_column = 0;
                                    if view_state.cursor_row
                                        >= view_state.scroll_row + viewport_height
                                    {
                                        view_state.scroll_row = view_state
                                            .cursor_row
                                            .saturating_sub(viewport_height - 1);
                                    }
                                    // Reset horizontal scroll for ALL panes
                                    for viewport in &mut view_state.pane_viewports {
                                        viewport.left_column = 0;
                                    }
                                }
                            }
                        }
                    }
                }
                CursorMovement::LineStart => {
                    view_state.move_cursor_to_line_start();
                }
                CursorMovement::LineEnd => {
                    view_state.move_cursor_to_line_end(line_info.length, line_info.pane_width);
                }
                CursorMovement::WordLeft => {
                    let new_col =
                        find_word_boundary_left(&line_info.content, view_state.cursor_column);
                    if new_col < view_state.cursor_column {
                        view_state.cursor_column = new_col;
                        view_state.sticky_column = new_col;
                        // Update horizontal scroll for ALL panes to keep cursor visible
                        let current_left = view_state
                            .pane_viewports
                            .get(view_state.focused_pane)
                            .map(|v| v.left_column)
                            .unwrap_or(0);
                        if view_state.cursor_column < current_left {
                            for viewport in &mut view_state.pane_viewports {
                                viewport.left_column = view_state.cursor_column;
                            }
                        }
                    } else if view_state.cursor_row > 0 {
                        // At start of line, wrap to end of previous line - find a row with content
                        if let Some(composite) = composite {
                            let focused_pane = view_state.focused_pane;
                            let mut target_row = view_state.cursor_row - 1;
                            while target_row > 0 {
                                if let Some(row) = composite.alignment.get_row(target_row) {
                                    if row.get_pane_line(focused_pane).is_some() {
                                        break;
                                    }
                                }
                                target_row -= 1;
                            }
                            // Only wrap if target row has content
                            if let Some(row) = composite.alignment.get_row(target_row) {
                                if row.get_pane_line(focused_pane).is_some() {
                                    view_state.cursor_row = target_row;
                                    if view_state.cursor_row < view_state.scroll_row {
                                        view_state.scroll_row = view_state.cursor_row;
                                    }
                                    wrapped_to_new_line = true;
                                }
                            }
                        }
                    }
                }
                CursorMovement::WordRight => {
                    let new_col = find_word_boundary_right(
                        &line_info.content,
                        view_state.cursor_column,
                        line_info.length,
                    );
                    if new_col > view_state.cursor_column {
                        view_state.cursor_column = new_col;
                        view_state.sticky_column = new_col;
                        // Update horizontal scroll for ALL panes to keep cursor visible
                        let visible_width = line_info.pane_width.saturating_sub(4);
                        let current_left = view_state
                            .pane_viewports
                            .get(view_state.focused_pane)
                            .map(|v| v.left_column)
                            .unwrap_or(0);
                        if visible_width > 0
                            && view_state.cursor_column >= current_left + visible_width
                        {
                            let new_left = view_state
                                .cursor_column
                                .saturating_sub(visible_width.saturating_sub(1));
                            for viewport in &mut view_state.pane_viewports {
                                viewport.left_column = new_left;
                            }
                        }
                    } else if view_state.cursor_row < max_row {
                        // At end of line, wrap to start of next line - find a row with content
                        if let Some(composite) = composite {
                            let focused_pane = view_state.focused_pane;
                            let mut target_row = view_state.cursor_row + 1;
                            while target_row < max_row {
                                if let Some(row) = composite.alignment.get_row(target_row) {
                                    if row.get_pane_line(focused_pane).is_some() {
                                        break;
                                    }
                                }
                                target_row += 1;
                            }
                            // Only wrap if target row has content
                            if let Some(row) = composite.alignment.get_row(target_row) {
                                if row.get_pane_line(focused_pane).is_some() {
                                    view_state.cursor_row = target_row;
                                    view_state.cursor_column = 0;
                                    view_state.sticky_column = 0;
                                    if view_state.cursor_row
                                        >= view_state.scroll_row + viewport_height
                                    {
                                        view_state.scroll_row = view_state
                                            .cursor_row
                                            .saturating_sub(viewport_height - 1);
                                    }
                                    // Reset horizontal scroll for ALL panes
                                    for viewport in &mut view_state.pane_viewports {
                                        viewport.left_column = 0;
                                    }
                                }
                            }
                        }
                    }
                }
                CursorMovement::WordEnd => {
                    let new_col = find_word_end_right(
                        &line_info.content,
                        view_state.cursor_column,
                        line_info.length,
                    );
                    if new_col > view_state.cursor_column {
                        view_state.cursor_column = new_col;
                        view_state.sticky_column = new_col;
                        // Update horizontal scroll for ALL panes to keep cursor visible
                        let visible_width = line_info.pane_width.saturating_sub(4);
                        let current_left = view_state
                            .pane_viewports
                            .get(view_state.focused_pane)
                            .map(|v| v.left_column)
                            .unwrap_or(0);
                        if visible_width > 0
                            && view_state.cursor_column >= current_left + visible_width
                        {
                            let new_left = view_state
                                .cursor_column
                                .saturating_sub(visible_width.saturating_sub(1));
                            for viewport in &mut view_state.pane_viewports {
                                viewport.left_column = new_left;
                            }
                        }
                    } else if view_state.cursor_row < max_row {
                        // At end of line, wrap to start of next line - find a row with content
                        if let Some(composite) = composite {
                            let focused_pane = view_state.focused_pane;
                            let mut target_row = view_state.cursor_row + 1;
                            while target_row < max_row {
                                if let Some(row) = composite.alignment.get_row(target_row) {
                                    if row.get_pane_line(focused_pane).is_some() {
                                        break;
                                    }
                                }
                                target_row += 1;
                            }
                            // Only wrap if target row has content
                            if let Some(row) = composite.alignment.get_row(target_row) {
                                if row.get_pane_line(focused_pane).is_some() {
                                    view_state.cursor_row = target_row;
                                    view_state.cursor_column = 0;
                                    view_state.sticky_column = 0;
                                    if view_state.cursor_row
                                        >= view_state.scroll_row + viewport_height
                                    {
                                        view_state.scroll_row = view_state
                                            .cursor_row
                                            .saturating_sub(viewport_height - 1);
                                    }
                                    // Reset horizontal scroll for ALL panes
                                    for viewport in &mut view_state.pane_viewports {
                                        viewport.left_column = 0;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // For vertical movement or line wrap, get line info for the NEW row and clamp/set cursor column
        if is_vertical || wrapped_to_new_line {
            let new_line_info = self.get_cursor_line_info(split_id, buffer_id);
            if let Some(view_state) = self
                .active_window_mut()
                .composite_view_states
                .get_mut(&(split_id, buffer_id))
            {
                if wrapped_to_new_line
                    && matches!(movement, CursorMovement::Left | CursorMovement::WordLeft)
                {
                    // Wrapping left goes to end of previous line
                    tracing::debug!(
                        "Wrap left to row {}, setting column to line length {}",
                        view_state.cursor_row,
                        new_line_info.length
                    );
                    view_state.cursor_column = new_line_info.length;
                    view_state.sticky_column = new_line_info.length;
                    // Scroll ALL panes horizontally to show cursor at end of line
                    let visible_width = new_line_info.pane_width.saturating_sub(4);
                    if visible_width > 0 && view_state.cursor_column >= visible_width {
                        let new_left = view_state
                            .cursor_column
                            .saturating_sub(visible_width.saturating_sub(1));
                        for viewport in &mut view_state.pane_viewports {
                            viewport.left_column = new_left;
                        }
                    }
                } else {
                    view_state.clamp_cursor_to_line(new_line_info.length);
                }
            }
        }
    }

    /// Handle cursor movement actions (both Move and Select variants)
    fn handle_cursor_movement_action(
        &mut self,
        split_id: LeafId,
        buffer_id: BufferId,
        movement: CursorMovement,
        extend_selection: bool,
    ) -> Option<bool> {
        let viewport_height = self.active_window().get_composite_viewport_height(split_id);

        let line_info = self.get_cursor_line_info(split_id, buffer_id);

        if extend_selection {
            // Start visual selection if extending and not already in visual mode
            if let Some(view_state) = self
                .active_window_mut()
                .composite_view_states
                .get_mut(&(split_id, buffer_id))
            {
                if !view_state.visual_mode {
                    view_state.start_visual_selection();
                }
            }
        } else {
            // Clear selection when moving without shift
            if let Some(view_state) = self
                .active_window_mut()
                .composite_view_states
                .get_mut(&(split_id, buffer_id))
            {
                if view_state.visual_mode {
                    view_state.clear_selection();
                }
            }
        }

        self.apply_cursor_movement(split_id, buffer_id, movement, &line_info, viewport_height);
        self.active_window_mut()
            .sync_editor_cursor_from_composite(split_id, buffer_id);

        Some(true)
    }

    /// Handle an action for a composite buffer.
    ///
    /// For navigation and selection actions, this forwards to the focused source buffer
    /// and syncs scroll between panes. Returns Some(true) if handled, None to fall through
    /// to normal buffer handling.
    pub fn handle_composite_action(
        &mut self,
        buffer_id: BufferId,
        action: &crate::input::keybindings::Action,
    ) -> Option<bool> {
        use crate::input::keybindings::Action;

        // Resolve the leaf the composite actually lives in. The Review Diff
        // panels are a group tab, so the focused composite is in an *inner*
        // group leaf, not the outer `active_split()`. `effective_active_pair`
        // descends into the focused group leaf (and matches how the renderer
        // keys `composite_view_states`), so the lookups below hit. Using the
        // outer `active_split()` here made every keyboard movement a no-op.
        let split_id = self.active_window().effective_active_pair().0;

        // Verify this is a valid composite buffer
        let _composite = self.active_window().composite_buffers.get(&buffer_id)?;
        let _view_state = self
            .active_window()
            .composite_view_states
            .get(&(split_id, buffer_id))?;

        match action {
            // Tab switches between panes
            Action::InsertTab => {
                self.active_window_mut()
                    .composite_focus_next(split_id, buffer_id);
                Some(true)
            }

            // Copy from the focused pane
            Action::Copy => {
                self.handle_composite_copy(split_id, buffer_id);
                Some(true)
            }

            // Cursor movement (without selection)
            Action::MoveDown => {
                self.handle_cursor_movement_action(split_id, buffer_id, CursorMovement::Down, false)
            }
            Action::MoveUp => {
                self.handle_cursor_movement_action(split_id, buffer_id, CursorMovement::Up, false)
            }
            Action::MoveLeft => {
                self.handle_cursor_movement_action(split_id, buffer_id, CursorMovement::Left, false)
            }
            Action::MoveRight => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::Right,
                false,
            ),
            Action::MoveLineStart | Action::SmartHome => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::LineStart,
                false,
            ),
            Action::MoveLineEnd => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::LineEnd,
                false,
            ),
            Action::MoveWordLeft => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::WordLeft,
                false,
            ),
            Action::MoveWordRight => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::WordRight,
                false,
            ),
            Action::MoveWordEnd | Action::ViMoveWordEnd => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::WordEnd,
                false,
            ),
            Action::MoveLeftInLine => {
                self.handle_cursor_movement_action(split_id, buffer_id, CursorMovement::Left, false)
            }
            Action::MoveRightInLine => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::Right,
                false,
            ),

            // Cursor movement with selection
            Action::SelectDown => {
                self.handle_cursor_movement_action(split_id, buffer_id, CursorMovement::Down, true)
            }
            Action::SelectUp => {
                self.handle_cursor_movement_action(split_id, buffer_id, CursorMovement::Up, true)
            }
            Action::SelectLeft => {
                self.handle_cursor_movement_action(split_id, buffer_id, CursorMovement::Left, true)
            }
            Action::SelectRight => {
                self.handle_cursor_movement_action(split_id, buffer_id, CursorMovement::Right, true)
            }
            Action::SelectLineStart => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::LineStart,
                true,
            ),
            Action::SelectLineEnd => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::LineEnd,
                true,
            ),
            Action::SelectWordLeft => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::WordLeft,
                true,
            ),
            Action::SelectWordRight => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::WordRight,
                true,
            ),
            Action::SelectWordEnd | Action::ViSelectWordEnd => self.handle_cursor_movement_action(
                split_id,
                buffer_id,
                CursorMovement::WordEnd,
                true,
            ),

            // Page navigation
            Action::MovePageDown | Action::MovePageUp => {
                let viewport_height = self.active_window().get_composite_viewport_height(split_id);
                let win = self.active_window_mut();
                if let Some(view_state) = win.composite_view_states.get_mut(&(split_id, buffer_id))
                {
                    if matches!(action, Action::MovePageDown) {
                        if let Some(composite) = win.composite_buffers.get(&buffer_id) {
                            let max_row = composite.row_count().saturating_sub(1);
                            view_state.page_down(viewport_height, max_row);
                            view_state.cursor_row = view_state.scroll_row;
                        }
                    } else {
                        view_state.page_up(viewport_height);
                        view_state.cursor_row = view_state.scroll_row;
                    }
                }
                self.active_window_mut()
                    .sync_editor_cursor_from_composite(split_id, buffer_id);
                Some(true)
            }

            // Document start/end
            Action::MoveDocumentStart | Action::MoveDocumentEnd => {
                let viewport_height = self.active_window().get_composite_viewport_height(split_id);
                let win = self.active_window_mut();
                if let Some(view_state) = win.composite_view_states.get_mut(&(split_id, buffer_id))
                {
                    if matches!(action, Action::MoveDocumentStart) {
                        view_state.move_cursor_to_top();
                    } else if let Some(composite) = win.composite_buffers.get(&buffer_id) {
                        let max_row = composite.row_count().saturating_sub(1);
                        view_state.move_cursor_to_bottom(max_row, viewport_height);
                    }
                }
                self.active_window_mut()
                    .sync_editor_cursor_from_composite(split_id, buffer_id);
                Some(true)
            }

            // Scroll without moving cursor
            Action::ScrollDown | Action::ScrollUp => {
                let delta = if matches!(action, Action::ScrollDown) {
                    1
                } else {
                    -1
                };
                self.active_window_mut()
                    .composite_scroll(split_id, buffer_id, delta);
                Some(true)
            }

            // For other actions, return None to fall through to normal handling
            _ => None,
        }
    }

    /// Handle copy action for composite buffer
    fn handle_composite_copy(&mut self, split_id: LeafId, buffer_id: BufferId) {
        let text = {
            let composite = match self.active_window().composite_buffers.get(&buffer_id) {
                Some(c) => c,
                None => return,
            };
            let view_state = match self
                .active_window()
                .composite_view_states
                .get(&(split_id, buffer_id))
            {
                Some(vs) => vs,
                None => return,
            };

            let (start_row, end_row) = match view_state.selection_row_range() {
                Some(range) => range,
                None => return,
            };

            let source = match composite.sources.get(view_state.focused_pane) {
                Some(s) => s,
                None => return,
            };

            let source_state = match self
                .windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .get(&source.buffer_id)
            {
                Some(s) => s,
                None => return,
            };

            // Collect text from selected rows
            let mut text = String::new();
            for row in start_row..=end_row {
                if let Some(aligned_row) = composite.alignment.rows.get(row) {
                    if let Some(line_ref) = aligned_row.get_pane_line(view_state.focused_pane) {
                        if let Some(line_bytes) = source_state.buffer.get_line(line_ref.line) {
                            if !text.is_empty() {
                                text.push('\n');
                            }
                            // Strip trailing newline from line content to avoid double newlines
                            let line_str = String::from_utf8_lossy(&line_bytes);
                            let line_trimmed = line_str.trim_end_matches(&['\n', '\r'][..]);
                            text.push_str(line_trimmed);
                        }
                    }
                }
            }
            text
        };

        // Strip ANSI escape codes so the plain copy carries the visible text
        // rather than the raw control codes that ANSI-aware panes render as styling.
        let text = crate::primitives::ansi::strip_ansi_codes(&text);
        if !text.is_empty() {
            self.clipboard.copy(text);
        }

        // Don't clear selection after copy - user may want to continue working with it
    }

    // =========================================================================
    // Plugin Command Handlers
    // =========================================================================

    /// Handle the CreateCompositeBuffer plugin command
    #[cfg(feature = "plugins")]
    pub(crate) fn handle_create_composite_buffer(
        &mut self,
        name: String,
        mode: String,
        layout_config: fresh_core::api::CompositeLayoutConfig,
        source_configs: Vec<fresh_core::api::CompositeSourceConfig>,
        hunks: Option<Vec<fresh_core::api::CompositeHunk>>,
        initial_focus_hunk: Option<usize>,
        _request_id: Option<u64>,
    ) {
        use crate::model::composite_buffer::{
            CompositeLayout, DiffHunk, GutterStyle, LineAlignment, PaneStyle, SourcePane,
        };

        // Convert layout config
        let layout = match layout_config.layout_type.as_str() {
            "stacked" => CompositeLayout::Stacked {
                spacing: layout_config.spacing.unwrap_or(1),
            },
            "unified" => CompositeLayout::Unified,
            _ => CompositeLayout::SideBySide {
                ratios: layout_config.ratios.unwrap_or_else(|| vec![0.5, 0.5]),
                show_separator: layout_config.show_separator,
            },
        };

        // Convert source configs
        let sources: Vec<SourcePane> = source_configs
            .into_iter()
            .map(|src| {
                let mut pane = SourcePane::new(BufferId(src.buffer_id), src.label, src.editable);
                if let Some(style_config) = src.style {
                    let gutter_style = match style_config.gutter_style.as_deref() {
                        Some("diff-markers") => GutterStyle::DiffMarkers,
                        Some("both") => GutterStyle::Both,
                        Some("none") => GutterStyle::None,
                        _ => GutterStyle::LineNumbers,
                    };
                    // Convert [u8; 3] arrays to (u8, u8, u8) tuples
                    let to_tuple = |arr: [u8; 3]| (arr[0], arr[1], arr[2]);
                    pane.style = PaneStyle {
                        add_bg: style_config.add_bg.map(to_tuple),
                        remove_bg: style_config.remove_bg.map(to_tuple),
                        modify_bg: style_config.modify_bg.map(to_tuple),
                        gutter_style,
                    };
                }
                pane
            })
            .collect();

        // Create the composite buffer
        let buffer_id = self.create_composite_buffer(name.clone(), mode.clone(), layout, sources);

        // Set alignment from hunks if provided
        if let Some(hunk_configs) = hunks {
            let diff_hunks: Vec<DiffHunk> = hunk_configs
                .into_iter()
                .map(|h| {
                    DiffHunk::new(h.old_start, h.old_count, h.new_start, h.new_count)
                        .with_ops(h.ops)
                })
                .collect();

            // Get line counts from source buffers
            let old_line_count = self
                .buffers()
                .get(
                    &self
                        .active_window()
                        .composite_buffers
                        .get(&buffer_id)
                        .unwrap()
                        .sources[0]
                        .buffer_id,
                )
                .and_then(|s| s.buffer.line_count())
                .unwrap_or(0);
            let new_line_count = self
                .buffers()
                .get(
                    &self
                        .active_window()
                        .composite_buffers
                        .get(&buffer_id)
                        .unwrap()
                        .sources[1]
                        .buffer_id,
                )
                .and_then(|s| s.buffer.line_count())
                .unwrap_or(0);

            let alignment = LineAlignment::from_hunks(&diff_hunks, old_line_count, new_line_count);
            self.active_window_mut()
                .set_composite_alignment(buffer_id, alignment);
        }

        // Store initial focus hunk for the first render to apply
        if initial_focus_hunk.is_some() {
            if let Some(composite) = self
                .active_window_mut()
                .composite_buffers
                .get_mut(&buffer_id)
            {
                composite.initial_focus_hunk = initial_focus_hunk;
            }
        }

        tracing::info!(
            "Created composite buffer '{}' with mode '{}' (id={:?})",
            name,
            mode,
            buffer_id
        );

        // Resolve callback with buffer_id if request_id is provided
        if let Some(req_id) = _request_id {
            // Return just the buffer ID as a number (consistent with createVirtualBuffer)
            let result = buffer_id.0.to_string();
            self.plugin_manager
                .read()
                .unwrap()
                .resolve_callback(fresh_core::api::JsCallbackId::from(req_id), result);
            tracing::info!(
                "CreateCompositeBuffer: resolve_callback sent for request_id={}",
                req_id
            );
        }
    }

    /// Handle the UpdateCompositeAlignment plugin command
    #[cfg(feature = "plugins")]
    pub(crate) fn handle_update_composite_alignment(
        &mut self,
        buffer_id: BufferId,
        hunk_configs: Vec<fresh_core::api::CompositeHunk>,
    ) {
        use crate::model::composite_buffer::{DiffHunk, LineAlignment};

        if let Some(composite) = self.active_window().composite_buffers.get(&buffer_id) {
            let diff_hunks: Vec<DiffHunk> = hunk_configs
                .into_iter()
                .map(|h| {
                    DiffHunk::new(h.old_start, h.old_count, h.new_start, h.new_count)
                        .with_ops(h.ops)
                })
                .collect();

            // Get line counts from source buffers
            let old_line_count = self
                .buffers()
                .get(&composite.sources[0].buffer_id)
                .and_then(|s| s.buffer.line_count())
                .unwrap_or(0);
            let new_line_count = self
                .buffers()
                .get(&composite.sources[1].buffer_id)
                .and_then(|s| s.buffer.line_count())
                .unwrap_or(0);

            let alignment = LineAlignment::from_hunks(&diff_hunks, old_line_count, new_line_count);
            self.active_window_mut()
                .set_composite_alignment(buffer_id, alignment);
        }
    }

    /// Handle a mouse click in a composite buffer view
    pub(crate) fn handle_composite_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        // Calculate which pane was clicked based on x coordinate
        let pane_idx = if let Some(view_state) = self
            .active_window()
            .composite_view_states
            .get(&(split_id, buffer_id))
        {
            let mut x = content_rect.x;
            let mut found_pane = 0;
            for (i, &width) in view_state.pane_widths.iter().enumerate() {
                if col >= x && col < x + width {
                    found_pane = i;
                    break;
                }
                x += width + 1; // +1 for separator
            }
            found_pane
        } else {
            0
        };

        // Calculate the clicked row (relative to scroll position)
        // Subtract 1 for the header row ("OLD (HEAD)" / "NEW (Working)")
        let content_row = row.saturating_sub(content_rect.y).saturating_sub(1) as usize;

        // Calculate column within the pane (accounting for gutter and horizontal scroll)
        let (pane_start_x, left_column) = if let Some(view_state) = self
            .active_window()
            .composite_view_states
            .get(&(split_id, buffer_id))
        {
            let mut x = content_rect.x;
            for (i, &width) in view_state.pane_widths.iter().enumerate() {
                if i == pane_idx {
                    break;
                }
                x += width + 1;
            }
            let left_col = view_state
                .pane_viewports
                .get(pane_idx)
                .map(|vp| vp.left_column)
                .unwrap_or(0);
            (x, left_col)
        } else {
            (content_rect.x, 0)
        };
        let gutter_width = 4; // Line number width
        let visual_col = col
            .saturating_sub(pane_start_x)
            .saturating_sub(gutter_width) as usize;
        // Convert visual column to actual column by adding horizontal scroll offset
        let click_col = left_column + visual_col;

        // Get line length to clamp cursor position
        let display_row = if let Some(view_state) = self
            .active_window()
            .composite_view_states
            .get(&(split_id, buffer_id))
        {
            view_state.scroll_row + content_row
        } else {
            content_row
        };

        let line_length =
            if let Some(composite) = self.active_window().composite_buffers.get(&buffer_id) {
                composite
                    .alignment
                    .get_row(display_row)
                    .and_then(|row| row.get_pane_line(pane_idx))
                    .and_then(|line_ref| {
                        let source = composite.sources.get(pane_idx)?;
                        self.windows
                            .get(&self.active_window)
                            .map(|w| &w.buffers)
                            .expect("active window present")
                            .get(&source.buffer_id)?
                            .buffer
                            .get_line(line_ref.line)
                    })
                    .map(|bytes| {
                        let s = String::from_utf8_lossy(&bytes);
                        // Strip trailing newline - cursor shouldn't go past end of visible content
                        let trimmed = s.trim_end_matches('\n').trim_end_matches('\r');
                        trimmed.graphemes(true).count()
                    })
                    .unwrap_or(0)
            } else {
                0
            };

        // Clamp click column to line length
        let clamped_col = click_col.min(line_length);

        // Update composite buffer's active pane
        if let Some(composite) = self
            .active_window_mut()
            .composite_buffers
            .get_mut(&buffer_id)
        {
            composite.active_pane = pane_idx;
        }

        // Update composite view state with click position
        if let Some(view_state) = self
            .active_window_mut()
            .composite_view_states
            .get_mut(&(split_id, buffer_id))
        {
            view_state.focused_pane = pane_idx;
            view_state.cursor_row = display_row;
            view_state.cursor_column = clamped_col;
            view_state.sticky_column = clamped_col;

            // Clear selection on click (will start fresh selection on drag)
            view_state.clear_selection();
        }

        // Store state for potential text selection drag
        self.active_window_mut().mouse_state.dragging_text_selection = false; // Disable regular text selection for composite
        self.active_window_mut().mouse_state.drag_selection_split = Some(split_id);

        // Sync cursor position to EditorState for status bar display
        self.active_window_mut()
            .sync_editor_cursor_from_composite(split_id, buffer_id);

        Ok(())
    }
}
