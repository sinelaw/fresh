//! Popup mouse handling.
//!
//! This module contains mouse event handling for popups including:
//! - Hit testing (determining what was clicked)
//! - Hover target computation
//! - Layout info conversion

use ratatui::layout::Rect;

use super::popup::{Popup, PopupContent, PopupManager};

/// Result of a popup click hit test
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PopupClickResult {
    /// Clicked on a link in the popup content
    Link { url: String },
    /// Clicked on a list item (for list popups)
    ListItem { popup_idx: usize, item_idx: usize },
    /// Clicked on text content (for text/markdown popups) - start selection
    TextContent {
        popup_idx: usize,
        line: usize,
        col: usize,
    },
    /// Clicked on scrollbar
    Scrollbar {
        popup_idx: usize,
        target_scroll: i32,
    },
    /// Click was inside popup but not on any interactive element
    Background,
    /// Click was outside all popups
    Outside,
}

/// Cached layout information for a single popup used in hit testing
#[derive(Debug, Clone)]
pub struct PopupLayoutInfo {
    pub popup_idx: usize,
    pub outer_rect: Rect,
    pub inner_rect: Rect,
    pub scroll_offset: usize,
    pub num_items: usize,
    pub scrollbar_rect: Option<Rect>,
    pub total_lines: usize,
}

/// Helper struct for popup mouse hit testing
pub struct PopupHitTester<'a> {
    layouts: &'a [PopupLayoutInfo],
    popups: &'a PopupManager,
}

impl<'a> PopupHitTester<'a> {
    pub fn new(layouts: &'a [PopupLayoutInfo], popups: &'a PopupManager) -> Self {
        Self { layouts, popups }
    }

    /// Check if a point is over any popup
    pub fn is_over_popup(&self, col: u16, row: u16) -> bool {
        if !self.popups.is_visible() {
            return false;
        }
        self.layouts.iter().any(|layout| {
            col >= layout.outer_rect.x
                && col < layout.outer_rect.x + layout.outer_rect.width
                && row >= layout.outer_rect.y
                && row < layout.outer_rect.y + layout.outer_rect.height
        })
    }

    /// Check if a point is over a transient popup
    pub fn is_over_transient_popup(&self, col: u16, row: u16) -> bool {
        let has_transient = self.popups.top().is_some_and(|p| p.transient);
        if !has_transient {
            return false;
        }
        self.is_over_popup(col, row)
    }

    /// Perform hit test for a click at the given position
    /// Returns what was clicked and any associated data
    pub fn hit_test_click(&self, col: u16, row: u16) -> PopupClickResult {
        // Check popups from top to bottom (reverse order)
        for layout in self.layouts.iter().rev() {
            // Check scrollbar first
            if let Some(sb_rect) = &layout.scrollbar_rect {
                if col >= sb_rect.x
                    && col < sb_rect.x + sb_rect.width
                    && row >= sb_rect.y
                    && row < sb_rect.y + sb_rect.height
                {
                    let track_height = sb_rect.height as usize;
                    let visible_lines = layout.inner_rect.height as usize;

                    if track_height > 0 && layout.total_lines > visible_lines {
                        let relative_row = (row - sb_rect.y) as usize;
                        let max_scroll = layout.total_lines.saturating_sub(visible_lines);
                        let target_scroll = if track_height > 1 {
                            ((relative_row * max_scroll) / (track_height - 1)) as i32
                        } else {
                            0
                        };
                        return PopupClickResult::Scrollbar {
                            popup_idx: layout.popup_idx,
                            target_scroll,
                        };
                    }
                }
            }

            // Check inner content area
            if col >= layout.inner_rect.x
                && col < layout.inner_rect.x + layout.inner_rect.width
                && row >= layout.inner_rect.y
                && row < layout.inner_rect.y + layout.inner_rect.height
            {
                let relative_col = (col - layout.inner_rect.x) as usize;
                let relative_row = (row - layout.inner_rect.y) as usize;

                // Check for link click in markdown popup
                if let Some(popup) = self.popups.get(layout.popup_idx) {
                    if let Some(url) = popup.link_at_position(relative_col, relative_row) {
                        return PopupClickResult::Link { url };
                    }
                }

                // Check for list item click
                if layout.num_items > 0 {
                    let item_idx = layout.scroll_offset + relative_row;
                    if item_idx < layout.num_items {
                        return PopupClickResult::ListItem {
                            popup_idx: layout.popup_idx,
                            item_idx,
                        };
                    }
                }

                // Check for text/markdown content click (for selection)
                if let Some(popup) = self.popups.get(layout.popup_idx) {
                    if matches!(
                        popup.content,
                        PopupContent::Text(_) | PopupContent::Markdown(_)
                    ) {
                        return PopupClickResult::TextContent {
                            popup_idx: layout.popup_idx,
                            line: layout.scroll_offset + relative_row,
                            col: relative_col,
                        };
                    }
                }

                return PopupClickResult::Background;
            }

            // Check outer rect (borders, etc.)
            if col >= layout.outer_rect.x
                && col < layout.outer_rect.x + layout.outer_rect.width
                && row >= layout.outer_rect.y
                && row < layout.outer_rect.y + layout.outer_rect.height
            {
                return PopupClickResult::Background;
            }
        }

        PopupClickResult::Outside
    }

    /// Get the hover target for a position (for list items)
    /// Returns (popup_idx, item_idx) if hovering over a list item
    pub fn hover_target(&self, col: u16, row: u16) -> Option<(usize, usize)> {
        for layout in self.layouts.iter().rev() {
            if col >= layout.inner_rect.x
                && col < layout.inner_rect.x + layout.inner_rect.width
                && row >= layout.inner_rect.y
                && row < layout.inner_rect.y + layout.inner_rect.height
                && layout.num_items > 0
            {
                let relative_row = (row - layout.inner_rect.y) as usize;
                let item_idx = layout.scroll_offset + relative_row;
                if item_idx < layout.num_items {
                    return Some((layout.popup_idx, item_idx));
                }
            }
        }
        None
    }

    /// Get the position within a popup's content area
    /// Returns (popup_idx, line, col) if the position is inside a popup's content
    pub fn content_position(&self, col: u16, row: u16) -> Option<(usize, usize, usize)> {
        for layout in self.layouts.iter().rev() {
            if col >= layout.inner_rect.x
                && col < layout.inner_rect.x + layout.inner_rect.width
                && row >= layout.inner_rect.y
                && row < layout.inner_rect.y + layout.inner_rect.height
            {
                let relative_col = (col - layout.inner_rect.x) as usize;
                let relative_row = (row - layout.inner_rect.y) as usize;
                let line = layout.scroll_offset + relative_row;
                return Some((layout.popup_idx, line, relative_col));
            }
        }
        None
    }
}

/// Convert cached popup areas to PopupLayoutInfo for hit testing
/// This function bridges the gap between the cached layout format and our hit testing API
pub fn popup_areas_to_layout_info(
    popup_areas: &[(usize, Rect, Rect, usize, usize, Option<Rect>, usize)],
) -> Vec<PopupLayoutInfo> {
    popup_areas
        .iter()
        .map(
            |(
                popup_idx,
                outer_rect,
                inner_rect,
                scroll_offset,
                num_items,
                scrollbar_rect,
                total_lines,
            )| {
                PopupLayoutInfo {
                    popup_idx: *popup_idx,
                    outer_rect: *outer_rect,
                    inner_rect: *inner_rect,
                    scroll_offset: *scroll_offset,
                    num_items: *num_items,
                    scrollbar_rect: *scrollbar_rect,
                    total_lines: *total_lines,
                }
            },
        )
        .collect()
}

/// Mouse drag handler for popup text selection
pub fn handle_popup_selection_drag(
    popup: &mut Popup,
    layout: &PopupLayoutInfo,
    col: u16,
    row: u16,
) {
    // Check if mouse is within the popup inner area
    if col >= layout.inner_rect.x
        && col < layout.inner_rect.x + layout.inner_rect.width
        && row >= layout.inner_rect.y
        && row < layout.inner_rect.y + layout.inner_rect.height
    {
        let relative_col = (col - layout.inner_rect.x) as usize;
        let relative_row = (row - layout.inner_rect.y) as usize;
        let line = layout.scroll_offset + relative_row;
        popup.extend_selection(line, relative_col);
    }
}
