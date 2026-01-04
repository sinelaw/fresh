//! View state for composite buffers
//!
//! Manages viewport, cursor, and focus state for composite buffer rendering.

use crate::model::cursor::Cursors;
use crate::model::event::BufferId;
use ratatui::layout::Rect;

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

    /// Check if a row is within the selection
    pub fn is_row_selected(&self, row: usize) -> bool {
        if !self.visual_mode {
            return false;
        }
        let (start, end) = self.selection_row_range().unwrap();
        row >= start && row <= end
    }

    /// Move cursor down, auto-scrolling if needed
    pub fn move_cursor_down(&mut self, max_row: usize, viewport_height: usize) {
        if self.cursor_row < max_row {
            self.cursor_row += 1;
            // Auto-scroll if cursor goes below viewport
            if self.cursor_row >= self.scroll_row + viewport_height {
                self.scroll_row = self.cursor_row.saturating_sub(viewport_height - 1);
            }
        }
    }

    /// Move cursor up, auto-scrolling if needed
    pub fn move_cursor_up(&mut self) {
        if self.cursor_row > 0 {
            self.cursor_row -= 1;
            // Auto-scroll if cursor goes above viewport
            if self.cursor_row < self.scroll_row {
                self.scroll_row = self.cursor_row;
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
            // Auto-scroll horizontally if needed
            if let Some(viewport) = self.pane_viewports.get_mut(self.focused_pane) {
                if self.cursor_column < viewport.left_column {
                    viewport.left_column = self.cursor_column;
                }
            }
        }
    }

    /// Move cursor right by one column
    pub fn move_cursor_right(&mut self, max_column: usize, pane_width: usize) {
        if self.cursor_column < max_column {
            self.cursor_column += 1;
            // Auto-scroll horizontally if needed
            if let Some(viewport) = self.pane_viewports.get_mut(self.focused_pane) {
                let visible_width = pane_width.saturating_sub(4); // minus gutter
                if visible_width > 0 && self.cursor_column >= viewport.left_column + visible_width {
                    viewport.left_column = self.cursor_column.saturating_sub(visible_width.saturating_sub(1));
                }
            }
        }
    }

    /// Move cursor to start of line
    pub fn move_cursor_to_line_start(&mut self) {
        self.cursor_column = 0;
        if let Some(viewport) = self.pane_viewports.get_mut(self.focused_pane) {
            viewport.left_column = 0;
        }
    }

    /// Move cursor to end of line
    pub fn move_cursor_to_line_end(&mut self, line_length: usize, pane_width: usize) {
        self.cursor_column = line_length;
        // Auto-scroll to show cursor
        if let Some(viewport) = self.pane_viewports.get_mut(self.focused_pane) {
            let visible_width = pane_width.saturating_sub(4); // minus gutter
            if self.cursor_column >= viewport.left_column + visible_width {
                viewport.left_column = self.cursor_column.saturating_sub(visible_width.saturating_sub(1));
            }
        }
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

    /// Scroll to top
    pub fn scroll_to_top(&mut self) {
        self.scroll_row = 0;
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

    /// Set focus to a specific pane
    pub fn set_focused_pane(&mut self, pane_index: usize) {
        if pane_index < self.pane_viewports.len() {
            self.focused_pane = pane_index;
        }
    }

    /// Get the viewport for a specific pane
    pub fn get_pane_viewport(&self, pane_index: usize) -> Option<&PaneViewport> {
        self.pane_viewports.get(pane_index)
    }

    /// Get mutable viewport for a specific pane
    pub fn get_pane_viewport_mut(&mut self, pane_index: usize) -> Option<&mut PaneViewport> {
        self.pane_viewports.get_mut(pane_index)
    }

    /// Get the cursor for a specific pane
    pub fn get_pane_cursor(&self, pane_index: usize) -> Option<&Cursors> {
        self.pane_cursors.get(pane_index)
    }

    /// Get mutable cursor for a specific pane
    pub fn get_pane_cursor_mut(&mut self, pane_index: usize) -> Option<&mut Cursors> {
        self.pane_cursors.get_mut(pane_index)
    }

    /// Get the focused pane's cursor
    pub fn focused_cursor(&self) -> Option<&Cursors> {
        self.pane_cursors.get(self.focused_pane)
    }

    /// Get mutable reference to the focused pane's cursor
    pub fn focused_cursor_mut(&mut self) -> Option<&mut Cursors> {
        self.pane_cursors.get_mut(self.focused_pane)
    }

    /// Update pane widths based on layout ratios and total width
    pub fn update_pane_widths(&mut self, total_width: u16, ratios: &[f32], separator_width: u16) {
        let separator_count = if self.pane_viewports.len() > 1 {
            self.pane_viewports.len() - 1
        } else {
            0
        };
        let available_width = total_width.saturating_sub(separator_count as u16 * separator_width);

        self.pane_widths.clear();
        for ratio in ratios {
            let width = (available_width as f32 * ratio).round() as u16;
            self.pane_widths.push(width);
        }

        // Adjust last pane to account for rounding
        let total: u16 = self.pane_widths.iter().sum();
        if total < available_width {
            if let Some(last) = self.pane_widths.last_mut() {
                *last += available_width - total;
            }
        } else if total > available_width {
            if let Some(last) = self.pane_widths.last_mut() {
                *last = last.saturating_sub(total - available_width);
            }
        }
    }

    /// Compute rects for each pane given the total area
    pub fn compute_pane_rects(&self, area: Rect, separator_width: u16) -> Vec<Rect> {
        let mut rects = Vec::with_capacity(self.pane_widths.len());
        let mut x = area.x;

        for (i, &width) in self.pane_widths.iter().enumerate() {
            rects.push(Rect {
                x,
                y: area.y,
                width,
                height: area.height,
            });
            x += width;
            if i < self.pane_widths.len() - 1 {
                x += separator_width;
            }
        }

        rects
    }
}

/// Viewport state for a single pane within a composite
#[derive(Debug, Clone, Default)]
pub struct PaneViewport {
    /// Computed rect for this pane (set during render)
    pub rect: Rect,
    /// Horizontal scroll offset for this pane
    pub left_column: usize,
}

impl PaneViewport {
    /// Create a new pane viewport
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the rect for this pane
    pub fn set_rect(&mut self, rect: Rect) {
        self.rect = rect;
    }

    /// Scroll horizontally
    pub fn scroll_horizontal(&mut self, delta: isize, max_column: usize) {
        if delta >= 0 {
            self.left_column = self
                .left_column
                .saturating_add(delta as usize)
                .min(max_column);
        } else {
            self.left_column = self.left_column.saturating_sub(delta.unsigned_abs());
        }
    }

    /// Reset horizontal scroll
    pub fn reset_horizontal_scroll(&mut self) {
        self.left_column = 0;
    }
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

    #[test]
    fn test_pane_width_calculation() {
        let mut view = CompositeViewState::new(BufferId(1), 2);
        view.update_pane_widths(100, &[0.5, 0.5], 1);

        assert_eq!(view.pane_widths.len(), 2);
        // 100 - 1 (separator) = 99, 99 * 0.5 = 49.5 ≈ 50
        assert!(view.pane_widths[0] + view.pane_widths[1] == 99);
    }

    #[test]
    fn test_compute_pane_rects() {
        let mut view = CompositeViewState::new(BufferId(1), 2);
        view.update_pane_widths(101, &[0.5, 0.5], 1);

        let area = Rect {
            x: 0,
            y: 0,
            width: 101,
            height: 50,
        };
        let rects = view.compute_pane_rects(area, 1);

        assert_eq!(rects.len(), 2);
        assert_eq!(rects[0].x, 0);
        assert_eq!(rects[1].x, rects[0].width + 1); // After separator
        assert_eq!(rects[0].height, 50);
        assert_eq!(rects[1].height, 50);
    }
}
