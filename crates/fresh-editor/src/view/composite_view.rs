//! View state for composite buffers
//!
//! Manages viewport, cursor, and focus state for composite buffer rendering.

use crate::model::composite_buffer::{CompositeBuffer, CompositeLayout};
use crate::model::cursor::Cursors;
use crate::model::event::BufferId;

/// The line-number gutter each composite pane draws before its text.
pub const PANE_GUTTER_WIDTH: u16 = 4;

/// Where each pane of a composite view sits across the width it is drawn in.
///
/// **A function of the composite and the width, not a record of the last
/// paint.** The painter used to compute the widths and store them on the
/// view state (`pane_widths`) for the event-time readers, which then
/// walked them with their own idea of the separator: a click added one
/// column per pane whether or not the layout drew a separator, so in a
/// layout without one every pane after the first was hit one column off.
/// Now the painter and each reader build the same value from the same two
/// inputs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PaneLayout {
    /// Each pane's width, gutter included.
    pub widths: Vec<u16>,
    /// The columns drawn between two panes: one for a side-by-side layout
    /// that shows a separator, none otherwise.
    pub separator_width: u16,
}

impl PaneLayout {
    pub fn new(composite: &CompositeBuffer, width: u16) -> Self {
        let pane_count = composite.sources.len();
        if pane_count == 0 {
            return Self {
                widths: Vec::new(),
                separator_width: 0,
            };
        }
        let separator_width = match &composite.layout {
            CompositeLayout::SideBySide { show_separator, .. } => u16::from(*show_separator),
            _ => 0,
        };
        let available = width.saturating_sub((pane_count as u16 - 1) * separator_width);
        let widths = match &composite.layout {
            CompositeLayout::SideBySide { ratios, .. } => {
                let default_ratio = 1.0 / pane_count as f32;
                ratios
                    .iter()
                    .chain(std::iter::repeat(&default_ratio))
                    .take(pane_count)
                    .map(|r| (available as f32 * r).round() as u16)
                    .collect()
            }
            _ => vec![available / pane_count as u16; pane_count],
        };
        Self {
            widths,
            separator_width,
        }
    }

    pub fn pane_count(&self) -> usize {
        self.widths.len()
    }

    /// The column pane `index` starts at, from the left of the view.
    pub fn pane_x(&self, index: usize) -> u16 {
        self.widths
            .iter()
            .take(index)
            .map(|w| w + self.separator_width)
            .sum()
    }

    /// The pane under column `x` (from the left of the view). A separator
    /// belongs to the pane on its left, and a column past the last pane to
    /// the last pane.
    pub fn pane_at(&self, x: u16) -> usize {
        let mut right = 0u16;
        for (i, w) in self.widths.iter().enumerate() {
            right += w + self.separator_width;
            if x < right {
                return i;
            }
        }
        self.pane_count().saturating_sub(1)
    }

    /// How many columns of text pane `index` shows beside its gutter.
    pub fn text_width(&self, index: usize) -> usize {
        usize::from(
            self.widths
                .get(index)
                .copied()
                .unwrap_or(0)
                .saturating_sub(PANE_GUTTER_WIDTH),
        )
    }
}

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
            // A step left can only leave the view on its left, which needs
            // no width to correct.
            self.reveal_cursor_column(0);
        }
    }

    /// Move cursor right by one column
    pub fn move_cursor_right(&mut self, max_column: usize, text_width: usize) {
        if self.cursor_column < max_column {
            self.cursor_column += 1;
            self.sticky_column = self.cursor_column;
            self.reveal_cursor_column(text_width);
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
    pub fn move_cursor_to_line_end(&mut self, line_length: usize, text_width: usize) {
        self.cursor_column = line_length;
        self.sticky_column = line_length;
        self.reveal_cursor_column(text_width);
    }

    /// Scroll every pane sideways, together, so the cursor's column is among
    /// the `text_width` columns each shows: to the cursor when it is left of
    /// the view, and so it sits at the right edge when it is past it.
    pub fn reveal_cursor_column(&mut self, text_width: usize) {
        let left = self
            .pane_viewports
            .get(self.focused_pane)
            .map_or(0, |v| v.left_column);
        let new_left = if self.cursor_column < left {
            self.cursor_column
        } else if text_width > 0 && self.cursor_column >= left + text_width {
            self.cursor_column + 1 - text_width
        } else {
            return;
        };
        for viewport in &mut self.pane_viewports {
            viewport.left_column = new_left;
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

    fn side_by_side(show_separator: bool) -> CompositeBuffer {
        use crate::model::composite_buffer::SourcePane;
        let pane = |label: &str| SourcePane::new(BufferId(1), label, false);
        CompositeBuffer::new(
            BufferId(9),
            "diff".into(),
            "diff-view".into(),
            CompositeLayout::SideBySide {
                ratios: vec![0.5, 0.5],
                show_separator,
            },
            vec![pane("OLD"), pane("NEW")],
        )
    }

    #[test]
    fn a_separator_takes_a_column_between_panes() {
        let layout = PaneLayout::new(&side_by_side(true), 81);
        assert_eq!(layout.widths, vec![40, 40]);
        assert_eq!(layout.pane_x(1), 41);
        assert_eq!(layout.pane_at(39), 0);
        assert_eq!(layout.pane_at(40), 0, "the separator is the left pane's");
        assert_eq!(layout.pane_at(41), 1);
    }

    #[test]
    fn without_a_separator_the_second_pane_starts_where_the_first_ends() {
        let layout = PaneLayout::new(&side_by_side(false), 80);
        assert_eq!(layout.widths, vec![40, 40]);
        assert_eq!(layout.pane_x(1), 40);
        assert_eq!(layout.pane_at(39), 0);
        assert_eq!(layout.pane_at(40), 1);
        assert_eq!(layout.text_width(1), 36);
    }
}
