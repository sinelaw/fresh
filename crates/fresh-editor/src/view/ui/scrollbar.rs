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

    /// Inverse of [`thumb_geometry`]: compute the scroll offset that
    /// places the thumb's top at (or as close as possible to)
    /// `target_thumb_top`. Use this — not `click_to_offset` — when a
    /// caller needs the thumb to land at a specific row on the track
    /// (e.g. press on the track to recentre the thumb under the cursor):
    /// `click_to_offset` divides by `track_height` rather than the actual
    /// `max_thumb_top`, so its result drifts above the intended row by a
    /// factor of `thumb_size / track_height`.
    pub fn offset_for_thumb_top(&self, track_height: usize, target_thumb_top: usize) -> usize {
        let max_scroll = self.total_items.saturating_sub(self.visible_items);
        if track_height == 0 || max_scroll == 0 {
            return 0;
        }
        let (_, thumb_size) = self.thumb_geometry(track_height);
        let max_thumb_top = track_height.saturating_sub(thumb_size);
        if max_thumb_top == 0 {
            return 0;
        }
        let clamped = target_thumb_top.min(max_thumb_top);
        let ratio = clamped as f64 / max_thumb_top as f64;
        ((ratio * max_scroll as f64).round() as usize).min(max_scroll)
    }

    /// Compute the scroll offset for a drag that preserves the cursor's
    /// position within the thumb.
    ///
    /// When the user presses on the thumb itself, the thumb shouldn't jump
    /// so its top aligns with the cursor — the cursor should stay pinned to
    /// the same spot on the thumb. Callers capture the press position
    /// (`drag_start_row`) and the scroll offset at that moment
    /// (`drag_start_offset`), then call this on every subsequent drag event.
    ///
    /// # Arguments
    /// * `track_height` — height of the scrollbar track in rows
    /// * `drag_start_row` — track-relative row where the drag started
    /// * `drag_start_offset` — scroll offset at the time of the press
    /// * `current_row` — track-relative row of the cursor now
    pub fn drag_to_offset(
        &self,
        track_height: usize,
        drag_start_row: usize,
        drag_start_offset: usize,
        current_row: usize,
    ) -> usize {
        let max_scroll = self.total_items.saturating_sub(self.visible_items);
        if track_height == 0 || max_scroll == 0 {
            return drag_start_offset.min(max_scroll);
        }

        // Compute by cursor delta so the round-trip through thumb_geometry
        // can't drift the offset on a zero-movement drag — pressing on the
        // thumb without moving must leave the viewport untouched.
        let delta_rows = current_row as i64 - drag_start_row as i64;
        if delta_rows == 0 {
            return drag_start_offset.min(max_scroll);
        }

        // Thumb geometry the thumb had at drag start. `max_thumb_top` is
        // the denominator that maps thumb rows to scroll offsets.
        let start = Self::new(self.total_items, self.visible_items, drag_start_offset);
        let (_, thumb_size) = start.thumb_geometry(track_height);
        let max_thumb_top = track_height.saturating_sub(thumb_size);
        if max_thumb_top == 0 {
            return drag_start_offset.min(max_scroll);
        }

        // delta_rows on the track ↦ delta_rows × (max_scroll / max_thumb_top).
        let offset_delta = delta_rows as f64 * (max_scroll as f64 / max_thumb_top as f64);
        let new_offset = (drag_start_offset as f64 + offset_delta).round();
        new_offset.clamp(0.0, max_scroll as f64) as usize
    }
}

/// In-flight scrollbar drag state captured on press. `start_row` is in
/// track coordinates (0 = top of the track).
#[derive(Debug, Clone, Copy)]
pub struct ScrollbarDrag {
    pub start_row: usize,
    pub start_offset: usize,
}

/// Shared press/drag/release state for a modal scrollbar. Owners hold one
/// of these alongside their scroll state and forward mouse events to it
/// via [`press`](Self::press), [`drag`](Self::drag), [`release`](Self::release).
///
/// All three methods return `Some(new_offset)` when the caller should
/// update its scroll position, or `None` when the event isn't ours to
/// handle (press outside the track, drag without a prior press, etc.).
#[derive(Debug, Clone, Copy, Default)]
pub struct ScrollbarMouse {
    pub drag: Option<ScrollbarDrag>,
}

impl ScrollbarMouse {
    /// Handle a left-button press. Returns `Some(new_offset)` when the
    /// press lands inside `track`. A press on the thumb captures the
    /// anchor without moving the viewport; a press on the track outside
    /// the thumb recentres the thumb on the cursor before capturing.
    pub fn press(
        &mut self,
        state: ScrollbarState,
        track: Rect,
        col: u16,
        row: u16,
    ) -> Option<usize> {
        if !super::point_in_rect(track, col, row) {
            return None;
        }
        let track_height = track.height as usize;
        let click_row = (row.saturating_sub(track.y) as usize).min(track_height);

        let new_offset = if state.is_thumb_row(track_height, click_row) {
            state.scroll_offset
        } else {
            let (_, thumb_size) = state.thumb_geometry(track_height);
            let aim_top = click_row.saturating_sub(thumb_size / 2);
            state.offset_for_thumb_top(track_height, aim_top)
        };

        self.drag = Some(ScrollbarDrag {
            start_row: click_row,
            start_offset: new_offset,
        });
        Some(new_offset)
    }

    /// Handle a left-button drag. Returns `Some(new_offset)` if a drag is
    /// active (i.e. there was a prior `press`), preserving the cursor's
    /// position within the thumb.
    pub fn drag(&mut self, state: ScrollbarState, track: Rect, row: u16) -> Option<usize> {
        let drag = self.drag?;
        let track_height = track.height as usize;
        let current_row = (row.saturating_sub(track.y) as usize).min(track_height);
        Some(state.drag_to_offset(track_height, drag.start_row, drag.start_offset, current_row))
    }

    /// Handle a left-button release. Ends any active drag.
    pub fn release(&mut self) {
        self.drag = None;
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

    #[test]
    fn test_drag_to_offset_no_movement_keeps_offset() {
        // Press on the thumb and don't move — offset must stay put.
        let state = ScrollbarState::new(100, 20, 40);
        let track = 20;
        let (thumb_top, _) = state.thumb_geometry(track);
        // Click in the middle of the thumb.
        let click_row = thumb_top + 1;
        let new_offset = state.drag_to_offset(track, click_row, 40, click_row);
        assert_eq!(new_offset, 40);
    }

    #[test]
    fn test_drag_to_offset_press_anywhere_on_thumb_no_jump() {
        // Pressing on a non-top row of the thumb must not jump the
        // viewport — the cursor stays pinned to that thumb position.
        let state = ScrollbarState::new(200, 50, 75);
        let track = 20;
        let (thumb_top, thumb_size) = state.thumb_geometry(track);
        assert!(thumb_size >= 2, "test needs thumb at least 2 rows tall");
        for row_in_thumb in thumb_top..(thumb_top + thumb_size) {
            let new_offset = state.drag_to_offset(track, row_in_thumb, 75, row_in_thumb);
            assert_eq!(
                new_offset, 75,
                "press at thumb row {row_in_thumb} should not move the viewport"
            );
        }
    }

    #[test]
    fn test_drag_to_offset_follows_cursor_down() {
        // Press at the top of the thumb when scrolled to the start, then
        // drag the cursor down — the offset must move down accordingly.
        let state = ScrollbarState::new(100, 20, 0);
        let track = 20;
        let (thumb_top, _) = state.thumb_geometry(track);
        let start_row = thumb_top;
        let down_row = start_row + 5;
        let dragged = state.drag_to_offset(track, start_row, 0, down_row);
        assert!(
            dragged > 0,
            "drag down should increase offset, got {dragged}"
        );
    }

    #[test]
    fn test_drag_to_offset_clamps_at_bottom() {
        let state = ScrollbarState::new(100, 20, 0);
        let track = 20;
        let dragged = state.drag_to_offset(track, 0, 0, 1000);
        let max_scroll = 100 - 20;
        assert_eq!(dragged, max_scroll);
    }

    #[test]
    fn test_drag_to_offset_no_overflow_when_fits() {
        // Content shorter than viewport — drag is a no-op.
        let state = ScrollbarState::new(10, 20, 0);
        assert_eq!(state.drag_to_offset(20, 0, 0, 5), 0);
    }

    #[test]
    fn test_offset_for_thumb_top_round_trip() {
        // For every reachable thumb row, `offset_for_thumb_top` must
        // produce an offset whose rendered thumb top matches that row —
        // i.e. it really is the inverse of `thumb_geometry`.
        let cases = [
            (200_usize, 50_usize, 20_usize),
            (1000, 30, 25),
            (50, 10, 15),
        ];
        for (total, visible, track) in cases {
            let probe = ScrollbarState::new(total, visible, 0);
            let (_, thumb_size) = probe.thumb_geometry(track);
            let max_thumb_top = track.saturating_sub(thumb_size);
            for target in 0..=max_thumb_top {
                let offset = probe.offset_for_thumb_top(track, target);
                let placed = ScrollbarState::new(total, visible, offset);
                let (got_top, _) = placed.thumb_geometry(track);
                assert!(
                    got_top.abs_diff(target) <= 1,
                    "thumb landed at {got_top}, expected {target} (total={total} visible={visible} track={track})"
                );
            }
        }
    }

    #[test]
    fn test_offset_for_thumb_top_clamps_to_max() {
        let state = ScrollbarState::new(200, 50, 0);
        let track = 20;
        let (_, thumb_size) = state.thumb_geometry(track);
        let max_thumb_top = track - thumb_size;
        // Asking for a row past the bottom must clamp to max_scroll, not
        // wrap or overshoot.
        assert_eq!(
            state.offset_for_thumb_top(track, max_thumb_top + 100),
            200 - 50
        );
    }

    fn track_rect(height: u16) -> Rect {
        Rect::new(50, 10, 1, height)
    }

    #[test]
    fn test_mouse_press_outside_track_returns_none() {
        let mut mouse = ScrollbarMouse::default();
        let state = ScrollbarState::new(200, 50, 75);
        let track = track_rect(20);
        // x outside
        assert_eq!(mouse.press(state, track, 0, 15), None);
        // y above
        assert_eq!(mouse.press(state, track, 50, 0), None);
        // y below (track is rows 10..30)
        assert_eq!(mouse.press(state, track, 50, 30), None);
        assert!(mouse.drag.is_none());
    }

    #[test]
    fn test_mouse_press_on_thumb_does_not_jump() {
        let mut mouse = ScrollbarMouse::default();
        let state = ScrollbarState::new(200, 50, 75);
        let track = track_rect(20);
        let (thumb_top, _) = state.thumb_geometry(track.height as usize);
        let press_screen_row = track.y + thumb_top as u16 + 1;
        let returned = mouse.press(state, track, track.x, press_screen_row);
        assert_eq!(returned, Some(75), "press on thumb must not move offset");
        let drag = mouse.drag.expect("anchor captured");
        assert_eq!(drag.start_offset, 75);
    }

    #[test]
    fn test_mouse_press_on_track_recenters_thumb() {
        let mut mouse = ScrollbarMouse::default();
        let state = ScrollbarState::new(200, 50, 0); // thumb at top
        let track = track_rect(20);
        // Click way down the track (outside the thumb).
        let returned = mouse.press(state, track, track.x, track.y + 18).unwrap();
        // The new offset should place the thumb so its centre is near
        // row 18: that means the new thumb_top is at 18 - thumb_size/2.
        let placed = ScrollbarState::new(200, 50, returned);
        let (got_top, thumb_size) = placed.thumb_geometry(track.height as usize);
        let want_top = (18_usize).saturating_sub(thumb_size / 2);
        assert!(
            got_top.abs_diff(want_top) <= 1,
            "thumb landed at {got_top}, expected ~{want_top}"
        );
    }

    #[test]
    fn test_mouse_drag_without_press_returns_none() {
        let mut mouse = ScrollbarMouse::default();
        let state = ScrollbarState::new(200, 50, 0);
        assert_eq!(mouse.drag(state, track_rect(20), 15), None);
    }

    #[test]
    fn test_mouse_drag_after_press_follows_cursor() {
        let mut mouse = ScrollbarMouse::default();
        let state = ScrollbarState::new(200, 50, 0);
        let track = track_rect(20);
        // Press at the thumb top.
        let _ = mouse.press(state, track, track.x, track.y);
        // Drag down a few rows.
        let new_offset = mouse.drag(state, track, track.y + 5).unwrap();
        assert!(new_offset > 0, "drag down should increase offset");
    }

    #[test]
    fn test_mouse_release_clears_drag() {
        let mut mouse = ScrollbarMouse::default();
        let state = ScrollbarState::new(200, 50, 0);
        let track = track_rect(20);
        let _ = mouse.press(state, track, track.x, track.y);
        assert!(mouse.drag.is_some());
        mouse.release();
        assert!(mouse.drag.is_none());
    }
}
