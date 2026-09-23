//! View state for composite buffers
//!
//! Manages viewport, cursor, and focus state for composite buffer rendering.

use crate::model::cursor::Cursors;
use crate::model::event::BufferId;

/// View state for a composite buffer in a split
#[derive(Debug, Clone)]
pub struct CompositeViewState {
    /// The composite buffer being displayed
    pub composite_id: BufferId,

    /// Independent viewport per pane
    pub pane_viewports: Vec<PaneViewport>,

    /// Which pane has focus (0-indexed)
    pub focused_pane: usize,

    /// Single scroll position (display row)
    /// All panes scroll together via alignment
    pub scroll_row: usize,

    /// Current cursor row (for navigation highlighting)
    pub cursor_row: usize,

    /// Current cursor column within the focused pane
    pub cursor_column: usize,

    /// Desired column for vertical navigation (sticky column)
    /// When moving up/down, the cursor tries to return to this column
    pub sticky_column: usize,

    /// Cursor positions per pane (for editing)
    pub pane_cursors: Vec<Cursors>,

    /// Width of each pane (computed during render)
    pub pane_widths: Vec<u16>,

    /// Whether visual selection mode is active
    pub visual_mode: bool,

    /// Selection anchor row (where selection started)
    pub selection_anchor_row: usize,

    /// Selection anchor column (where selection started)
    pub selection_anchor_column: usize,
}

impl CompositeViewState {
    /// Create a new composite view state for the given buffer
    pub fn new(composite_id: BufferId, pane_count: usize) -> Self {
        Self {
            composite_id,
            pane_viewports: (0..pane_count).map(|_| PaneViewport::default()).collect(),
            focused_pane: 0,
            scroll_row: 0,
            cursor_row: 0,
            cursor_column: 0,
            sticky_column: 0,
            pane_cursors: (0..pane_count).map(|_| Cursors::new()).collect(),
            pane_widths: vec![0; pane_count],
            visual_mode: false,
            selection_anchor_row: 0,
            selection_anchor_column: 0,
        }
    }

    /// Start visual selection at current cursor position
    pub fn start_visual_selection(&mut self) {
        self.visual_mode = true;
        self.selection_anchor_row = self.cursor_row;
        self.selection_anchor_column = self.cursor_column;
    }

    /// Clear visual selection
    pub fn clear_selection(&mut self) {
        self.visual_mode = false;
    }

    /// Get selection row range (start_row, end_row) inclusive
    /// Returns None if not in visual mode
    pub fn selection_row_range(&self) -> Option<(usize, usize)> {
        if !self.visual_mode {
            return None;
        }
        let start = self.selection_anchor_row.min(self.cursor_row);
        let end = self.selection_anchor_row.max(self.cursor_row);
        Some((start, end))
    }

    /// Get the column range that is selected for a given row
    /// Returns (start_col, end_col) where end_col is exclusive
    /// Returns None if row is not in selection
    pub fn selection_column_range(&self, row: usize) -> Option<(usize, usize)> {
        if !self.visual_mode {
            return None;
        }

        let (start_row, end_row) = self.selection_row_range()?;
        if row < start_row || row > end_row {
            return None;
        }

        // Determine which position is "start" and which is "end"
        let (sel_start_row, sel_start_col, sel_end_row, sel_end_col) = if self.selection_anchor_row
            < self.cursor_row
            || (self.selection_anchor_row == self.cursor_row
                && self.selection_anchor_column <= self.cursor_column)
        {
            (
                self.selection_anchor_row,
                self.selection_anchor_column,
                self.cursor_row,
                self.cursor_column,
            )
        } else {
            (
                self.cursor_row,
                self.cursor_column,
                self.selection_anchor_row,
                self.selection_anchor_column,
            )
        };

        // For multi-row selection:
        // - First row: from start_col to end of line (usize::MAX)
        // - Middle rows: entire line (0 to usize::MAX)
        // - Last row: from 0 to end_col
        // For single-row selection: from start_col to end_col
        if sel_start_row == sel_end_row {
            // Single row selection
            Some((sel_start_col, sel_end_col))
        } else if row == sel_start_row {
            // First row of multi-row selection
            Some((sel_start_col, usize::MAX))
        } else if row == sel_end_row {
            // Last row of multi-row selection
            Some((0, sel_end_col))
        } else {
            // Middle row - entire line selected
            Some((0, usize::MAX))
        }
    }

    /// Move cursor down, auto-scrolling if needed.
    /// Keeps cursor at least SCROLL_MARGIN lines from the bottom edge of the viewport.
    pub fn move_cursor_down(&mut self, max_row: usize, viewport_height: usize) {
        const SCROLL_MARGIN: usize = 3;
        if self.cursor_row < max_row {
            self.cursor_row += 1;
            let margin = SCROLL_MARGIN.min(viewport_height.saturating_sub(1) / 2);
            if self.cursor_row + margin >= self.scroll_row + viewport_height {
                self.scroll_row += 1;
            }
        }
    }

    /// Move cursor up, auto-scrolling if needed.
    /// Keeps cursor at least SCROLL_MARGIN lines from the top edge of the viewport.
    pub fn move_cursor_up(&mut self, viewport_height: usize) {
        const SCROLL_MARGIN: usize = 3;
        if self.cursor_row > 0 {
            self.cursor_row -= 1;
            let margin = SCROLL_MARGIN.min(viewport_height.saturating_sub(1) / 2);
            if self.cursor_row < self.scroll_row + margin && self.scroll_row > 0 {
                self.scroll_row -= 1;
            }
        }
    }

    /// Move cursor to top
    pub fn move_cursor_to_top(&mut self) {
        self.cursor_row = 0;
        self.scroll_row = 0;
    }

    /// Move cursor to bottom
    pub fn move_cursor_to_bottom(&mut self, max_row: usize, viewport_height: usize) {
        self.cursor_row = max_row;
        self.scroll_row = max_row.saturating_sub(viewport_height.saturating_sub(1));
    }

    /// Move cursor left by one column
    pub fn move_cursor_left(&mut self) {
        if self.cursor_column > 0 {
            self.cursor_column -= 1;
            self.sticky_column = self.cursor_column;
            // Auto-scroll horizontally all panes together
            let current_left = self
                .pane_viewports
                .get(self.focused_pane)
                .map(|v| v.left_column)
                .unwrap_or(0);
            if self.cursor_column < current_left {
                for viewport in &mut self.pane_viewports {
                    viewport.left_column = self.cursor_column;
                }
            }
        }
    }

    /// Move cursor right by one column
    pub fn move_cursor_right(&mut self, max_column: usize, pane_width: usize) {
        if self.cursor_column < max_column {
            self.cursor_column += 1;
            self.sticky_column = self.cursor_column;
            // Auto-scroll horizontally all panes together
            let visible_width = pane_width.saturating_sub(4); // minus gutter
            let current_left = self
                .pane_viewports
                .get(self.focused_pane)
                .map(|v| v.left_column)
                .unwrap_or(0);
            if visible_width > 0 && self.cursor_column >= current_left + visible_width {
                let new_left = self
                    .cursor_column
                    .saturating_sub(visible_width.saturating_sub(1));
                for viewport in &mut self.pane_viewports {
                    viewport.left_column = new_left;
                }
            }
        }
    }

    /// Move cursor to start of line
    pub fn move_cursor_to_line_start(&mut self) {
        self.cursor_column = 0;
        self.sticky_column = 0;
        // Reset horizontal scroll for all panes
        for viewport in &mut self.pane_viewports {
            viewport.left_column = 0;
        }
    }

    /// Move cursor to end of line
    pub fn move_cursor_to_line_end(&mut self, line_length: usize, pane_width: usize) {
        self.cursor_column = line_length;
        self.sticky_column = line_length;
        // Auto-scroll all panes to show cursor
        let visible_width = pane_width.saturating_sub(4); // minus gutter
        let current_left = self
            .pane_viewports
            .get(self.focused_pane)
            .map(|v| v.left_column)
            .unwrap_or(0);
        if visible_width > 0 && self.cursor_column >= current_left + visible_width {
            let new_left = self
                .cursor_column
                .saturating_sub(visible_width.saturating_sub(1));
            for viewport in &mut self.pane_viewports {
                viewport.left_column = new_left;
            }
        }
    }

    /// Clamp cursor column to line length, using sticky column if possible
    /// Call this after vertical movement to adjust cursor to new line's length
    pub fn clamp_cursor_to_line(&mut self, line_length: usize) {
        // Try to use sticky column, but clamp to line length
        self.cursor_column = self.sticky_column.min(line_length);
    }

    /// Scroll all panes together by delta lines
    pub fn scroll(&mut self, delta: isize, max_row: usize) {
        if delta >= 0 {
            self.scroll_row = self.scroll_row.saturating_add(delta as usize).min(max_row);
        } else {
            self.scroll_row = self.scroll_row.saturating_sub(delta.unsigned_abs());
        }
    }

    /// Set scroll to a specific row
    pub fn set_scroll_row(&mut self, row: usize, max_row: usize) {
        self.scroll_row = row.min(max_row);
    }

    /// Scroll to bottom
    pub fn scroll_to_bottom(&mut self, total_rows: usize, viewport_height: usize) {
        self.scroll_row = total_rows.saturating_sub(viewport_height);
    }

    /// Page down
    pub fn page_down(&mut self, viewport_height: usize, max_row: usize) {
        self.scroll_row = self.scroll_row.saturating_add(viewport_height).min(max_row);
    }

    /// Page up
    pub fn page_up(&mut self, viewport_height: usize) {
        self.scroll_row = self.scroll_row.saturating_sub(viewport_height);
    }

    /// Switch focus to the next pane
    pub fn focus_next_pane(&mut self) {
        if !self.pane_viewports.is_empty() {
            self.focused_pane = (self.focused_pane + 1) % self.pane_viewports.len();
        }
    }

    /// Switch focus to the previous pane
    pub fn focus_prev_pane(&mut self) {
        let count = self.pane_viewports.len();
        if count > 0 {
            self.focused_pane = (self.focused_pane + count - 1) % count;
        }
    }

    /// Get the viewport for a specific pane
    pub fn get_pane_viewport(&self, pane_index: usize) -> Option<&PaneViewport> {
        self.pane_viewports.get(pane_index)
    }
}

/// Viewport state for a single pane within a composite
#[derive(Debug, Clone, Default)]
pub struct PaneViewport {
    /// Horizontal scroll offset for this pane
    pub left_column: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_composite_view_scroll() {
        let mut view = CompositeViewState::new(BufferId(1), 2);
        assert_eq!(view.scroll_row, 0);

        view.scroll(10, 100);
        assert_eq!(view.scroll_row, 10);

        view.scroll(-5, 100);
        assert_eq!(view.scroll_row, 5);

        view.scroll(-10, 100);
        assert_eq!(view.scroll_row, 0); // Doesn't go negative
    }

    #[test]
    fn test_composite_view_focus() {
        let mut view = CompositeViewState::new(BufferId(1), 3);
        assert_eq!(view.focused_pane, 0);

        view.focus_next_pane();
        assert_eq!(view.focused_pane, 1);

        view.focus_next_pane();
        assert_eq!(view.focused_pane, 2);

        view.focus_next_pane();
        assert_eq!(view.focused_pane, 0); // Wraps around

        view.focus_prev_pane();
        assert_eq!(view.focused_pane, 2);
    }
}
