//! Popup geometry predicates for the mouse pipeline.
//!
//! What survives here is deliberately small: the cached-layout
//! conversion and the two "is the pointer over a popup?" predicates
//! that `Editor::is_mouse_over_any_popup` / the transient keep-alive
//! consult (the plan-acknowledged parallel rect query — blocking-safe
//! by construction). The old click/hover/drag dispatch half that used
//! to live beside them (`PopupClickResult`, `hit_test_click`,
//! `hover_target`, `content_position`, `handle_popup_selection_drag`)
//! was replaced by the chrome Popups component's handlers and the
//! `PopupSelect` pointer grab, and had already diverged from them (the
//! component click path knows the `[×]` close button; the dead
//! hit-test didn't) — deleted, not kept as a second encoding.

use ratatui::layout::Rect;

use super::popup::PopupManager;

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
}

/// Convert cached popup areas to PopupLayoutInfo for hit testing
/// This function bridges the gap between the cached layout format and our hit testing API
#[allow(clippy::type_complexity)]
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
