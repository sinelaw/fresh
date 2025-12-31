//! Reusable scrollbar widget for lists and content areas
//!
//! This module provides a scrollbar that can be used with any scrollable content,
//! not just the editor buffer. It's extracted from the split_rendering module
//! to enable reuse in file browsers, popups, and other scrollable UI elements.

use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

/// State needed to render and interact with a scrollbar
#[derive(Debug, Clone, Copy)]
pub struct ScrollbarState {
    /// Total number of items/lines
    pub total_items: usize,
    /// Number of items visible in the viewport
    pub visible_items: usize,
    /// Current scroll offset (first visible item index)
    pub scroll_offset: usize,
}

impl ScrollbarState {
    /// Create a new scrollbar state
    pub fn new(total_items: usize, visible_items: usize, scroll_offset: usize) -> Self {
        Self {
            total_items,
            visible_items,
            scroll_offset,
        }
    }

    /// Calculate thumb position and size for a given track height
    ///
    /// Returns (thumb_start, thumb_size) in rows
    pub fn thumb_geometry(&self, track_height: usize) -> (usize, usize) {
        if track_height == 0 || self.total_items == 0 {
            return (0, 0);
        }

        // Calculate the maximum scroll position
        let max_scroll = self.total_items.saturating_sub(self.visible_items);

        // When content fits entirely in viewport, fill the entire scrollbar
        if max_scroll == 0 {
            return (0, track_height);
        }

        // Calculate thumb size based on viewport ratio
        let thumb_size_raw = ((self.visible_items as f64 / self.total_items as f64)
            * track_height as f64)
            .ceil() as usize;

        // Cap thumb size: minimum 1, maximum 80% of track height
        let max_thumb_size = (track_height as f64 * 0.8).floor() as usize;
        let thumb_size = thumb_size_raw.max(1).min(max_thumb_size).min(track_height);

        // Calculate thumb position using linear mapping
        let scroll_ratio = self.scroll_offset.min(max_scroll) as f64 / max_scroll as f64;
        let max_thumb_start = track_height.saturating_sub(thumb_size);
        let thumb_start = (scroll_ratio * max_thumb_start as f64) as usize;

        (thumb_start, thumb_size)
    }

    /// Convert a click position on the track to a scroll offset
    ///
    /// # Arguments
    /// * `track_height` - Height of the scrollbar track in rows
    /// * `click_row` - Row within the track that was clicked (0-indexed)
    ///
    /// # Returns
    /// The scroll offset that would position the thumb at the click location
    pub fn click_to_offset(&self, track_height: usize, click_row: usize) -> usize {
        if track_height == 0 || self.total_items == 0 {
            return 0;
        }

        let max_scroll = self.total_items.saturating_sub(self.visible_items);
        if max_scroll == 0 {
            return 0;
        }

        // Map click position to scroll offset
        let click_ratio = click_row as f64 / track_height as f64;
        let offset = (click_ratio * max_scroll as f64) as usize;

        offset.min(max_scroll)
    }

    /// Check if a row is within the thumb area
    pub fn is_thumb_row(&self, track_height: usize, row: usize) -> bool {
        let (thumb_start, thumb_size) = self.thumb_geometry(track_height);
        row >= thumb_start && row < thumb_start + thumb_size
    }
}

/// Colors for the scrollbar
#[derive(Debug, Clone, Copy)]
pub struct ScrollbarColors {
    pub track: Color,
    pub thumb: Color,
}

impl Default for ScrollbarColors {
    fn default() -> Self {
        Self {
            track: Color::DarkGray,
            thumb: Color::Gray,
        }
    }
}

impl ScrollbarColors {
    /// Colors for an active/focused scrollbar
    pub fn active() -> Self {
        Self {
            track: Color::DarkGray,
            thumb: Color::Gray,
        }
    }

    /// Colors for an inactive/unfocused scrollbar
    pub fn inactive() -> Self {
        Self {
            track: Color::Black,
            thumb: Color::DarkGray,
        }
    }

    /// Create from theme colors
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            track: theme.scrollbar_track_fg,
            thumb: theme.scrollbar_thumb_fg,
        }
    }

    /// Create from theme colors with hover
    pub fn from_theme_hover(theme: &crate::view::theme::Theme) -> Self {
        Self {
            track: theme.scrollbar_track_hover_fg,
            thumb: theme.scrollbar_thumb_hover_fg,
        }
    }
}

/// Render a vertical scrollbar
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - A 1-column wide rectangle for the scrollbar
/// * `state` - The scrollbar state (total items, visible items, offset)
/// * `colors` - Colors for track and thumb
///
/// # Returns
/// (thumb_start, thumb_end) in row coordinates relative to the area
pub fn render_scrollbar(
    frame: &mut Frame,
    area: Rect,
    state: &ScrollbarState,
    colors: &ScrollbarColors,
) -> (usize, usize) {
    let height = area.height as usize;
    if height == 0 || area.width == 0 {
        return (0, 0);
    }

    let (thumb_start, thumb_size) = state.thumb_geometry(height);
    let thumb_end = thumb_start + thumb_size;

    // Render as background fills to avoid gaps with box-drawing glyphs in some terminals.
    for row in 0..height {
        let cell_area = Rect::new(area.x, area.y + row as u16, 1, 1);

        let style = if row >= thumb_start && row < thumb_end {
            Style::default().bg(colors.thumb)
        } else {
            Style::default().bg(colors.track)
        };

        let paragraph = Paragraph::new(" ").style(style);
        frame.render_widget(paragraph, cell_area);
    }

    (thumb_start, thumb_end)
}

/// Render a scrollbar with mouse hover highlight
///
/// Same as `render_scrollbar` but highlights the thumb if hovered
pub fn render_scrollbar_with_hover(
    frame: &mut Frame,
    area: Rect,
    state: &ScrollbarState,
    colors: &ScrollbarColors,
    is_thumb_hovered: bool,
) -> (usize, usize) {
    let height = area.height as usize;
    if height == 0 || area.width == 0 {
        return (0, 0);
    }

    let (thumb_start, thumb_size) = state.thumb_geometry(height);
    let thumb_end = thumb_start + thumb_size;

    // Highlight thumb when hovered
    let thumb_color = if is_thumb_hovered {
        Color::White
    } else {
        colors.thumb
    };

    for row in 0..height {
        let cell_area = Rect::new(area.x, area.y + row as u16, 1, 1);

        let style = if row >= thumb_start && row < thumb_end {
            Style::default().bg(thumb_color)
        } else {
            Style::default().bg(colors.track)
        };

        let paragraph = Paragraph::new(" ").style(style);
        frame.render_widget(paragraph, cell_area);
    }

    (thumb_start, thumb_end)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_thumb_geometry_full_content_visible() {
        // When all content fits in viewport, thumb fills entire track
        let state = ScrollbarState::new(10, 20, 0); // 10 items, 20 visible
        let (start, size) = state.thumb_geometry(10);
        assert_eq!(start, 0);
        assert_eq!(size, 10); // Fills entire track
    }

    #[test]
    fn test_thumb_geometry_at_top() {
        let state = ScrollbarState::new(100, 20, 0);
        let (start, _size) = state.thumb_geometry(10);
        assert_eq!(start, 0);
    }

    #[test]
    fn test_thumb_geometry_at_bottom() {
        let state = ScrollbarState::new(100, 20, 80); // Scrolled to max
        let (start, size) = state.thumb_geometry(10);
        assert_eq!(start + size, 10); // Thumb should be at bottom
    }

    #[test]
    fn test_thumb_geometry_middle() {
        let state = ScrollbarState::new(100, 20, 40); // Halfway
        let (start, size) = state.thumb_geometry(10);
        // Thumb should be roughly in the middle
        assert!(start > 0);
        assert!(start + size < 10);
    }

    #[test]
    fn test_click_to_offset_top() {
        let state = ScrollbarState::new(100, 20, 0);
        let offset = state.click_to_offset(10, 0);
        assert_eq!(offset, 0);
    }

    #[test]
    fn test_click_to_offset_bottom() {
        let state = ScrollbarState::new(100, 20, 0);
        let offset = state.click_to_offset(10, 10);
        assert_eq!(offset, 80); // max scroll
    }

    #[test]
    fn test_click_to_offset_middle() {
        let state = ScrollbarState::new(100, 20, 0);
        let offset = state.click_to_offset(10, 5);
        assert_eq!(offset, 40); // Half of max scroll (80)
    }

    #[test]
    fn test_is_thumb_row() {
        let state = ScrollbarState::new(100, 20, 0);
        let (start, size) = state.thumb_geometry(10);

        // Rows in thumb area should return true
        for row in start..(start + size) {
            assert!(state.is_thumb_row(10, row));
        }

        // Rows outside should return false (if any)
        if start > 0 {
            assert!(!state.is_thumb_row(10, 0));
        }
    }
}
