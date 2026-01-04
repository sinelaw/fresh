//! Composite buffer management actions
//!
//! This module handles creating, managing, and closing composite buffers
//! which display multiple source buffers in a single tab.

use crate::app::types::BufferMetadata;
use crate::app::Editor;
use crate::model::composite_buffer::{CompositeBuffer, CompositeLayout, LineAlignment, SourcePane};
use crate::model::event::{BufferId, SplitId};
use crate::view::composite_view::CompositeViewState;
use unicode_segmentation::UnicodeSegmentation;

impl Editor {
    // =========================================================================
    // Composite Buffer Methods
    // =========================================================================

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

    /// Get or create composite view state for a split
    pub fn get_composite_view_state(
        &mut self,
        split_id: SplitId,
        buffer_id: BufferId,
    ) -> Option<&mut CompositeViewState> {
        if !self.composite_buffers.contains_key(&buffer_id) {
            return None;
        }

        let pane_count = self.composite_buffers.get(&buffer_id)?.pane_count();

        Some(
            self.composite_view_states
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
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let composite =
            CompositeBuffer::new(buffer_id, name.clone(), mode.clone(), layout, sources);
        self.composite_buffers.insert(buffer_id, composite);

        // Add metadata for display
        // Note: We use virtual_buffer() but override hidden_from_tabs since composite buffers
        // should be visible in tabs (unlike their hidden source panes)
        let mut metadata = BufferMetadata::virtual_buffer(name.clone(), mode.clone(), true);
        metadata.hidden_from_tabs = false;
        self.buffer_metadata.insert(buffer_id, metadata);

        // Create an EditorState entry so the buffer can be shown in tabs and via showBuffer()
        // The actual content rendering is handled by the composite renderer
        let mut state = crate::state::EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
        );
        state.is_composite_buffer = true;
        state.editing_disabled = true;
        state.mode = mode;
        self.buffers.insert(buffer_id, state);

        // Create an event log entry (required for many editor operations)
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Register with the active split so it appears in tabs
        let split_id = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            view_state.add_buffer(buffer_id);
        }

        buffer_id
    }

    /// Set the line alignment for a composite buffer
    ///
    /// The alignment determines how lines from different source buffers
    /// are paired up for display (important for diff views).
    pub fn set_composite_alignment(&mut self, buffer_id: BufferId, alignment: LineAlignment) {
        if let Some(composite) = self.composite_buffers.get_mut(&buffer_id) {
            composite.set_alignment(alignment);
        }
    }

    /// Close a composite buffer and clean up associated state
    pub fn close_composite_buffer(&mut self, buffer_id: BufferId) {
        self.composite_buffers.remove(&buffer_id);
        self.buffer_metadata.remove(&buffer_id);

        // Remove all view states for this buffer
        self.composite_view_states
            .retain(|(_, bid), _| *bid != buffer_id);
    }

    /// Switch focus to the next pane in a composite buffer
    pub fn composite_focus_next(&mut self, split_id: SplitId, buffer_id: BufferId) {
        if let Some(composite) = self.composite_buffers.get_mut(&buffer_id) {
            composite.focus_next();
        }
        // Also update the view state's focused_pane (used by renderer)
        if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id)) {
            view_state.focus_next_pane();
        }
    }

    /// Switch focus to the previous pane in a composite buffer
    pub fn composite_focus_prev(&mut self, split_id: SplitId, buffer_id: BufferId) {
        if let Some(composite) = self.composite_buffers.get_mut(&buffer_id) {
            composite.focus_prev();
        }
        // Also update the view state's focused_pane (used by renderer)
        if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id)) {
            view_state.focus_prev_pane();
        }
    }

    /// Navigate to the next hunk in a composite buffer's diff view
    pub fn composite_next_hunk(&mut self, split_id: SplitId, buffer_id: BufferId) -> bool {
        if let (Some(composite), Some(view_state)) = (
            self.composite_buffers.get(&buffer_id),
            self.composite_view_states.get_mut(&(split_id, buffer_id)),
        ) {
            if let Some(next_row) = composite.alignment.next_hunk_row(view_state.scroll_row) {
                view_state.scroll_row = next_row;
                return true;
            }
        }
        false
    }

    /// Navigate to the previous hunk in a composite buffer's diff view
    pub fn composite_prev_hunk(&mut self, split_id: SplitId, buffer_id: BufferId) -> bool {
        if let (Some(composite), Some(view_state)) = (
            self.composite_buffers.get(&buffer_id),
            self.composite_view_states.get_mut(&(split_id, buffer_id)),
        ) {
            if let Some(prev_row) = composite.alignment.prev_hunk_row(view_state.scroll_row) {
                view_state.scroll_row = prev_row;
                return true;
            }
        }
        false
    }

    /// Scroll a composite buffer view
    pub fn composite_scroll(&mut self, split_id: SplitId, buffer_id: BufferId, delta: isize) {
        if let (Some(composite), Some(view_state)) = (
            self.composite_buffers.get(&buffer_id),
            self.composite_view_states.get_mut(&(split_id, buffer_id)),
        ) {
            let max_row = composite.row_count().saturating_sub(1);
            view_state.scroll(delta, max_row);
        }
    }

    /// Scroll composite buffer to a specific row
    pub fn composite_scroll_to(&mut self, split_id: SplitId, buffer_id: BufferId, row: usize) {
        if let (Some(composite), Some(view_state)) = (
            self.composite_buffers.get(&buffer_id),
            self.composite_view_states.get_mut(&(split_id, buffer_id)),
        ) {
            let max_row = composite.row_count().saturating_sub(1);
            view_state.set_scroll_row(row, max_row);
        }
    }

    // =========================================================================
    // Action Handling for Composite Buffers
    // =========================================================================

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

        let split_id = self.split_manager.active_split();

        // Get the focused source buffer for forwarding actions
        let (focused_buffer_id, focused_pane_idx, other_buffer_id) = {
            let composite = self.composite_buffers.get(&buffer_id)?;
            let view_state = self.composite_view_states.get(&(split_id, buffer_id))?;
            let focused = composite.sources.get(view_state.focused_pane)?.buffer_id;
            let other_pane = if view_state.focused_pane == 0 { 1 } else { 0 };
            let other = composite.sources.get(other_pane).map(|s| s.buffer_id);
            (focused, view_state.focused_pane, other)
        };
        let _ = focused_pane_idx; // Used for Copy action mapping

        // Actions that need special composite handling
        match action {
            // Tab switches between panes in composite buffer
            Action::InsertTab => {
                self.composite_focus_next(split_id, buffer_id);
                Some(true)
            }

            // Copy from the focused pane (need to map aligned rows to source lines)
            Action::Copy => {
                if let (Some(composite), Some(view_state)) = (
                    self.composite_buffers.get(&buffer_id),
                    self.composite_view_states.get(&(split_id, buffer_id)),
                ) {
                    if let Some((start_row, end_row)) = view_state.selection_row_range() {
                        // Get the source buffer for the focused pane
                        if let Some(source) = composite.sources.get(view_state.focused_pane) {
                            if let Some(source_state) = self.buffers.get(&source.buffer_id) {
                                // Collect text from selected rows
                                let mut text = String::new();
                                for row in start_row..=end_row {
                                    // Map display row to source line
                                    if let Some(aligned_row) = composite.alignment.rows.get(row) {
                                        if let Some(line_ref) =
                                            aligned_row.get_pane_line(view_state.focused_pane)
                                        {
                                            if let Some(line_bytes) =
                                                source_state.buffer.get_line(line_ref.line)
                                            {
                                                if !text.is_empty() {
                                                    text.push('\n');
                                                }
                                                text.push_str(&String::from_utf8_lossy(
                                                    &line_bytes,
                                                ));
                                            }
                                        }
                                    }
                                }
                                // Copy to clipboard
                                if !text.is_empty() {
                                    self.clipboard.copy(text);
                                }
                            }
                        }
                        // Clear selection after copy
                        if let Some(view_state) =
                            self.composite_view_states.get_mut(&(split_id, buffer_id))
                        {
                            view_state.clear_selection();
                        }
                    }
                }
                Some(true)
            }

            // Navigation: Update composite view's cursor/scroll position
            // These operate on the aligned view, not the underlying source buffers
            Action::MoveDown
            | Action::MoveUp
            | Action::MoveLeft
            | Action::MoveRight
            | Action::MoveLineStart
            | Action::MoveLineEnd
            | Action::MoveWordLeft
            | Action::MoveWordRight => {
                let viewport_height = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| vs.viewport.height as usize)
                    .unwrap_or(24);

                let new_cursor_row;
                let new_cursor_column;

                // Get line content, length and pane width for cursor bounds
                let (line_content, line_length, pane_width) = {
                    let composite = self.composite_buffers.get(&buffer_id);
                    let view_state = self.composite_view_states.get(&(split_id, buffer_id));

                    if let (Some(composite), Some(view_state)) = (composite, view_state) {
                        // Get the source line for the focused pane at cursor row
                        let line_bytes = composite
                            .alignment
                            .get_row(view_state.cursor_row)
                            .and_then(|row| row.get_pane_line(view_state.focused_pane))
                            .and_then(|line_ref| {
                                let source = composite.sources.get(view_state.focused_pane)?;
                                self.buffers
                                    .get(&source.buffer_id)?
                                    .buffer
                                    .get_line(line_ref.line)
                            });

                        let line_str = line_bytes
                            .as_ref()
                            .map(|b| String::from_utf8_lossy(b).to_string())
                            .unwrap_or_default();
                        // Use grapheme count for proper unicode handling
                        let line_len = line_str.graphemes(true).count();

                        let width = view_state
                            .pane_widths
                            .get(view_state.focused_pane)
                            .copied()
                            .unwrap_or(40) as usize;

                        (line_str, line_len, width)
                    } else {
                        (String::new(), 0, 40)
                    }
                };

                if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id))
                {
                    match action {
                        Action::MoveDown => {
                            if let Some(composite) = self.composite_buffers.get(&buffer_id) {
                                let max_row = composite.row_count().saturating_sub(1);
                                view_state.move_cursor_down(max_row, viewport_height);
                            }
                        }
                        Action::MoveUp => view_state.move_cursor_up(),
                        Action::MoveLeft => {
                            view_state.move_cursor_left();
                        }
                        Action::MoveRight => {
                            view_state.move_cursor_right(line_length, pane_width);
                        }
                        Action::MoveLineStart => {
                            view_state.move_cursor_to_line_start();
                        }
                        Action::MoveLineEnd => {
                            view_state.move_cursor_to_line_end(line_length, pane_width);
                        }
                        Action::MoveWordLeft => {
                            // Find previous word boundary using grapheme clusters
                            let graphemes: Vec<&str> = line_content.graphemes(true).collect();
                            let mut pos = view_state.cursor_column;
                            // Skip spaces going left
                            while pos > 0
                                && graphemes
                                    .get(pos.saturating_sub(1))
                                    .map_or(false, |g| g.chars().all(|c| c.is_whitespace()))
                            {
                                pos -= 1;
                            }
                            // Skip word chars going left
                            while pos > 0
                                && graphemes
                                    .get(pos.saturating_sub(1))
                                    .map_or(false, |g| !g.chars().all(|c| c.is_whitespace()))
                            {
                                pos -= 1;
                            }
                            view_state.cursor_column = pos;
                            // Update horizontal scroll
                            if let Some(viewport) =
                                view_state.pane_viewports.get_mut(view_state.focused_pane)
                            {
                                if view_state.cursor_column < viewport.left_column {
                                    viewport.left_column = view_state.cursor_column;
                                }
                            }
                        }
                        Action::MoveWordRight => {
                            // Find next word boundary using grapheme clusters
                            let graphemes: Vec<&str> = line_content.graphemes(true).collect();
                            let mut pos = view_state.cursor_column;
                            // Skip word chars going right
                            while pos < graphemes.len()
                                && !graphemes[pos].chars().all(|c| c.is_whitespace())
                            {
                                pos += 1;
                            }
                            // Skip spaces going right
                            while pos < graphemes.len()
                                && graphemes[pos].chars().all(|c| c.is_whitespace())
                            {
                                pos += 1;
                            }
                            view_state.cursor_column = pos.min(line_length);
                            // Update horizontal scroll
                            if let Some(viewport) =
                                view_state.pane_viewports.get_mut(view_state.focused_pane)
                            {
                                let visible_width = pane_width.saturating_sub(4);
                                if view_state.cursor_column >= viewport.left_column + visible_width
                                {
                                    viewport.left_column =
                                        view_state.cursor_column.saturating_sub(visible_width.saturating_sub(1));
                                }
                            }
                        }
                        _ => {}
                    }
                    new_cursor_row = view_state.cursor_row;
                    new_cursor_column = view_state.cursor_column;
                } else {
                    new_cursor_row = 0;
                    new_cursor_column = 0;
                }

                // Sync the fake EditorState's cursor with CompositeViewState
                // This makes the status bar show the correct position
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.primary_cursor_line_number =
                        crate::model::buffer::LineNumber::Absolute(new_cursor_row);
                    state.cursors.primary_mut().position = new_cursor_column;
                }

                let _ = (focused_buffer_id, other_buffer_id);
                Some(true)
            }

            // Page navigation
            Action::MovePageDown | Action::MovePageUp => {
                let viewport_height = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| vs.viewport.height as usize)
                    .unwrap_or(24);

                if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id))
                {
                    if matches!(action, Action::MovePageDown) {
                        if let Some(composite) = self.composite_buffers.get(&buffer_id) {
                            let max_row = composite.row_count().saturating_sub(1);
                            view_state.page_down(viewport_height, max_row);
                            view_state.cursor_row = view_state.scroll_row;
                        }
                    } else {
                        view_state.page_up(viewport_height);
                        view_state.cursor_row = view_state.scroll_row;
                    }
                }

                let _ = (focused_buffer_id, other_buffer_id);
                Some(true)
            }

            // Document start/end
            Action::MoveDocumentStart | Action::MoveDocumentEnd => {
                let viewport_height = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| vs.viewport.height as usize)
                    .unwrap_or(24);

                if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id))
                {
                    if matches!(action, Action::MoveDocumentStart) {
                        view_state.move_cursor_to_top();
                    } else if let Some(composite) = self.composite_buffers.get(&buffer_id) {
                        let max_row = composite.row_count().saturating_sub(1);
                        view_state.move_cursor_to_bottom(max_row, viewport_height);
                    }
                }

                let _ = (focused_buffer_id, other_buffer_id);
                Some(true)
            }

            // Scroll without moving cursor
            Action::ScrollDown | Action::ScrollUp => {
                let delta = if matches!(action, Action::ScrollDown) {
                    1
                } else {
                    -1
                };
                self.composite_scroll(split_id, buffer_id, delta);

                let _ = (focused_buffer_id, other_buffer_id);
                Some(true)
            }

            // Selection: Start visual mode and extend
            Action::SelectDown
            | Action::SelectUp
            | Action::SelectLeft
            | Action::SelectRight
            | Action::SelectLineStart
            | Action::SelectLineEnd
            | Action::SelectWordLeft
            | Action::SelectWordRight => {
                let viewport_height = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| vs.viewport.height as usize)
                    .unwrap_or(24);

                let new_cursor_row;
                let new_cursor_column;

                // Get line content, length and pane width for cursor bounds
                let (line_content, line_length, pane_width) = {
                    let composite = self.composite_buffers.get(&buffer_id);
                    let view_state = self.composite_view_states.get(&(split_id, buffer_id));

                    if let (Some(composite), Some(view_state)) = (composite, view_state) {
                        let line_bytes = composite
                            .alignment
                            .get_row(view_state.cursor_row)
                            .and_then(|row| row.get_pane_line(view_state.focused_pane))
                            .and_then(|line_ref| {
                                let source = composite.sources.get(view_state.focused_pane)?;
                                self.buffers
                                    .get(&source.buffer_id)?
                                    .buffer
                                    .get_line(line_ref.line)
                            });

                        let line_str = line_bytes
                            .as_ref()
                            .map(|b| String::from_utf8_lossy(b).to_string())
                            .unwrap_or_default();
                        // Use grapheme count for proper unicode handling
                        let line_len = line_str.graphemes(true).count();

                        let width = view_state
                            .pane_widths
                            .get(view_state.focused_pane)
                            .copied()
                            .unwrap_or(40) as usize;

                        (line_str, line_len, width)
                    } else {
                        (String::new(), 0, 40)
                    }
                };

                if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id))
                {
                    if !view_state.visual_mode {
                        view_state.start_visual_selection();
                    }
                    match action {
                        Action::SelectDown => {
                            if let Some(composite) = self.composite_buffers.get(&buffer_id) {
                                let max_row = composite.row_count().saturating_sub(1);
                                view_state.move_cursor_down(max_row, viewport_height);
                            }
                        }
                        Action::SelectUp => view_state.move_cursor_up(),
                        Action::SelectLeft => {
                            view_state.move_cursor_left();
                        }
                        Action::SelectRight => {
                            view_state.move_cursor_right(line_length, pane_width);
                        }
                        Action::SelectLineStart => {
                            view_state.move_cursor_to_line_start();
                        }
                        Action::SelectLineEnd => {
                            view_state.move_cursor_to_line_end(line_length, pane_width);
                        }
                        Action::SelectWordLeft => {
                            // Use grapheme clusters for proper unicode handling
                            let graphemes: Vec<&str> = line_content.graphemes(true).collect();
                            let mut pos = view_state.cursor_column;
                            while pos > 0
                                && graphemes
                                    .get(pos.saturating_sub(1))
                                    .map_or(false, |g| g.chars().all(|c| c.is_whitespace()))
                            {
                                pos -= 1;
                            }
                            while pos > 0
                                && graphemes
                                    .get(pos.saturating_sub(1))
                                    .map_or(false, |g| !g.chars().all(|c| c.is_whitespace()))
                            {
                                pos -= 1;
                            }
                            view_state.cursor_column = pos;
                            if let Some(viewport) =
                                view_state.pane_viewports.get_mut(view_state.focused_pane)
                            {
                                if view_state.cursor_column < viewport.left_column {
                                    viewport.left_column = view_state.cursor_column;
                                }
                            }
                        }
                        Action::SelectWordRight => {
                            // Use grapheme clusters for proper unicode handling
                            let graphemes: Vec<&str> = line_content.graphemes(true).collect();
                            let mut pos = view_state.cursor_column;
                            while pos < graphemes.len()
                                && !graphemes[pos].chars().all(|c| c.is_whitespace())
                            {
                                pos += 1;
                            }
                            while pos < graphemes.len()
                                && graphemes[pos].chars().all(|c| c.is_whitespace())
                            {
                                pos += 1;
                            }
                            view_state.cursor_column = pos.min(line_length);
                            if let Some(viewport) =
                                view_state.pane_viewports.get_mut(view_state.focused_pane)
                            {
                                let visible_width = pane_width.saturating_sub(4);
                                if view_state.cursor_column >= viewport.left_column + visible_width
                                {
                                    viewport.left_column =
                                        view_state.cursor_column.saturating_sub(visible_width.saturating_sub(1));
                                }
                            }
                        }
                        _ => {}
                    }
                    new_cursor_row = view_state.cursor_row;
                    new_cursor_column = view_state.cursor_column;
                } else {
                    new_cursor_row = 0;
                    new_cursor_column = 0;
                }

                // Sync the fake EditorState's cursor with CompositeViewState
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.primary_cursor_line_number =
                        crate::model::buffer::LineNumber::Absolute(new_cursor_row);
                    state.cursors.primary_mut().position = new_cursor_column;
                }

                let _ = (focused_buffer_id, other_buffer_id);
                Some(true)
            }

            // For other actions, return None to fall through to normal handling
            _ => None,
        }
    }

    // =========================================================================
    // Plugin Command Handlers
    // =========================================================================

    /// Handle the CreateCompositeBuffer plugin command
    pub(crate) fn handle_create_composite_buffer(
        &mut self,
        name: String,
        mode: String,
        layout_config: crate::services::plugins::api::CompositeLayoutConfig,
        source_configs: Vec<crate::services::plugins::api::CompositeSourceConfig>,
        hunks: Option<Vec<crate::services::plugins::api::CompositeHunk>>,
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
                    pane.style = PaneStyle {
                        add_bg: style_config.add_bg,
                        remove_bg: style_config.remove_bg,
                        modify_bg: style_config.modify_bg,
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
                .map(|h| DiffHunk::new(h.old_start, h.old_count, h.new_start, h.new_count))
                .collect();

            // Get line counts from source buffers
            let old_line_count = self
                .buffers
                .get(&self.composite_buffers.get(&buffer_id).unwrap().sources[0].buffer_id)
                .and_then(|s| s.buffer.line_count())
                .unwrap_or(0);
            let new_line_count = self
                .buffers
                .get(&self.composite_buffers.get(&buffer_id).unwrap().sources[1].buffer_id)
                .and_then(|s| s.buffer.line_count())
                .unwrap_or(0);

            let alignment = LineAlignment::from_hunks(&diff_hunks, old_line_count, new_line_count);
            self.set_composite_alignment(buffer_id, alignment);
        }

        tracing::info!(
            "Created composite buffer '{}' with mode '{}' (id={:?})",
            name,
            mode,
            buffer_id
        );

        // Send response with buffer_id if request_id is provided
        if let Some(req_id) = _request_id {
            self.send_plugin_response(
                crate::services::plugins::api::PluginResponse::CompositeBufferCreated {
                    request_id: req_id,
                    buffer_id,
                },
            );
        }
    }

    /// Handle the UpdateCompositeAlignment plugin command
    pub(crate) fn handle_update_composite_alignment(
        &mut self,
        buffer_id: BufferId,
        hunk_configs: Vec<crate::services::plugins::api::CompositeHunk>,
    ) {
        use crate::model::composite_buffer::{DiffHunk, LineAlignment};

        if let Some(composite) = self.composite_buffers.get(&buffer_id) {
            let diff_hunks: Vec<DiffHunk> = hunk_configs
                .into_iter()
                .map(|h| DiffHunk::new(h.old_start, h.old_count, h.new_start, h.new_count))
                .collect();

            // Get line counts from source buffers
            let old_line_count = self
                .buffers
                .get(&composite.sources[0].buffer_id)
                .and_then(|s| s.buffer.line_count())
                .unwrap_or(0);
            let new_line_count = self
                .buffers
                .get(&composite.sources[1].buffer_id)
                .and_then(|s| s.buffer.line_count())
                .unwrap_or(0);

            let alignment = LineAlignment::from_hunks(&diff_hunks, old_line_count, new_line_count);
            self.set_composite_alignment(buffer_id, alignment);
        }
    }

    /// Handle a mouse click in a composite buffer view
    pub(crate) fn handle_composite_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: SplitId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        // Calculate which pane was clicked based on x coordinate
        let pane_idx =
            if let Some(view_state) = self.composite_view_states.get(&(split_id, buffer_id)) {
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
        let content_row = row.saturating_sub(content_rect.y) as usize;

        // Calculate column within the pane (accounting for gutter and horizontal scroll)
        let (pane_start_x, left_column) =
            if let Some(view_state) = self.composite_view_states.get(&(split_id, buffer_id)) {
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
        let display_row = if let Some(view_state) =
            self.composite_view_states.get(&(split_id, buffer_id))
        {
            view_state.scroll_row + content_row
        } else {
            content_row
        };

        let line_length = if let Some(composite) = self.composite_buffers.get(&buffer_id) {
            composite
                .alignment
                .get_row(display_row)
                .and_then(|row| row.get_pane_line(pane_idx))
                .and_then(|line_ref| {
                    let source = composite.sources.get(pane_idx)?;
                    self.buffers.get(&source.buffer_id)?.buffer.get_line(line_ref.line)
                })
                .map(|bytes| String::from_utf8_lossy(&bytes).chars().count())
                .unwrap_or(0)
        } else {
            0
        };

        // Clamp click column to line length
        let clamped_col = click_col.min(line_length);

        // Update composite buffer's active pane
        if let Some(composite) = self.composite_buffers.get_mut(&buffer_id) {
            composite.active_pane = pane_idx;
        }

        // Update composite view state with click position
        if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id)) {
            view_state.focused_pane = pane_idx;
            view_state.cursor_row = display_row;
            view_state.cursor_column = clamped_col;

            // Clear selection on click (will start fresh selection on drag)
            view_state.clear_selection();
        }

        // Store state for potential text selection drag
        self.mouse_state.dragging_text_selection = false; // Disable regular text selection for composite
        self.mouse_state.drag_selection_split = Some(split_id);

        Ok(())
    }
}
