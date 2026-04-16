//! Pure coordinate conversion between screen cells and buffer byte offsets.
//!
//! None of these functions need an `Editor`: they take only the screen
//! coordinates, the content rectangle, the cached view-line mappings, and
//! (for the fold helper) a direct `&EditorState`. They were previously
//! associated functions on `Editor` (`Self::screen_to_buffer_position(...)`)
//! purely by convention — this module makes that decoupling explicit.
//!
//! See `docs/internal/editor-modules-refactor-plan.md` phase 1.

use std::collections::BTreeMap;

use ratatui::layout::Rect;

use crate::app::types::ViewLineMapping;
use crate::state::EditorState;

/// Adjust a content rectangle for compose-mode centering.
///
/// When `compose_width` is `Some(cw)` and `cw < content_rect.width`, the
/// content is centered inside `content_rect` with left/right padding; this
/// returns the rectangle actually occupied by drawn text. Otherwise returns
/// `content_rect` unchanged.
pub(crate) fn adjust_content_rect_for_compose(
    content_rect: Rect,
    compose_width: Option<u16>,
) -> Rect {
    if let Some(cw) = compose_width {
        let clamped = cw.min(content_rect.width).max(1);
        if clamped < content_rect.width {
            let pad_total = content_rect.width - clamped;
            let left_pad = pad_total / 2;
            return Rect::new(
                content_rect.x + left_pad,
                content_rect.y,
                clamped,
                content_rect.height,
            );
        }
    }
    content_rect
}

/// Calculate buffer byte position from screen coordinates.
///
/// When `compose_width` is set and narrower than the content area, the
/// content is centered with left padding. View-line mappings are built
/// relative to that compose render area, so the same offset must be
/// applied here when converting screen coordinates.
///
/// Returns `None` if the position cannot be determined (e.g. a click in the
/// gutter when `allow_gutter_click` is false).
#[allow(clippy::too_many_arguments)]
pub(crate) fn screen_to_buffer_position(
    col: u16,
    row: u16,
    content_rect: Rect,
    gutter_width: u16,
    cached_mappings: &Option<Vec<ViewLineMapping>>,
    fallback_position: usize,
    allow_gutter_click: bool,
    compose_width: Option<u16>,
) -> Option<usize> {
    let content_rect = adjust_content_rect_for_compose(content_rect, compose_width);

    // Calculate relative position in content area
    let content_col = col.saturating_sub(content_rect.x);
    let content_row = row.saturating_sub(content_rect.y);

    tracing::trace!(
        col,
        row,
        ?content_rect,
        gutter_width,
        content_col,
        content_row,
        num_mappings = cached_mappings.as_ref().map(|m| m.len()),
        "screen_to_buffer_position"
    );

    // Handle gutter clicks
    let text_col = if content_col < gutter_width {
        if !allow_gutter_click {
            return None; // Click handler skips gutter clicks
        }
        0 // Drag handler uses position 0 of the line
    } else {
        content_col.saturating_sub(gutter_width) as usize
    };

    // Use cached view line mappings for accurate position lookup
    let visual_row = content_row as usize;

    // Helper to get position from a line mapping at a given visual column
    let position_from_mapping = |line_mapping: &ViewLineMapping, col: usize| -> usize {
        if col < line_mapping.visual_to_char.len() {
            // Use O(1) lookup: visual column -> char index -> source byte
            if let Some(byte_pos) = line_mapping.source_byte_at_visual_col(col) {
                return byte_pos;
            }
            // Column maps to virtual/injected content - find nearest real position
            for c in (0..col).rev() {
                if let Some(byte_pos) = line_mapping.source_byte_at_visual_col(c) {
                    return byte_pos;
                }
            }
            line_mapping.line_end_byte
        } else {
            // Click is past end of visible content.
            // For empty lines (only a newline), return the line start position
            // to keep cursor on this line rather than jumping to the next line.
            if line_mapping.visual_to_char.len() <= 1 {
                if let Some(Some(first_byte)) = line_mapping.char_source_bytes.first() {
                    return *first_byte;
                }
            }
            line_mapping.line_end_byte
        }
    };

    let position = cached_mappings
        .as_ref()
        .and_then(|mappings| {
            if let Some(line_mapping) = mappings.get(visual_row) {
                // Click is on a visible line
                Some(position_from_mapping(line_mapping, text_col))
            } else if !mappings.is_empty() {
                // Click is below last visible line — use the last line at the clicked column
                let last_mapping = mappings.last().unwrap();
                Some(position_from_mapping(last_mapping, text_col))
            } else {
                None
            }
        })
        .unwrap_or(fallback_position);

    Some(position)
}

/// Check whether a gutter click at `target_position` should toggle a fold.
///
/// Returns `Some(target_position)` (the byte to fold at) or `None` when the
/// click was not in the gutter or no fold exists at that line.
pub(crate) fn fold_toggle_byte_from_position(
    state: &EditorState,
    collapsed_header_bytes: &BTreeMap<usize, Option<String>>,
    target_position: usize,
    content_col: u16,
    gutter_width: u16,
) -> Option<usize> {
    if content_col >= gutter_width {
        return None;
    }

    use crate::view::folding::indent_folding;
    let line_start = indent_folding::find_line_start_byte(&state.buffer, target_position);

    // Already collapsed → allow toggling (unfold)
    if collapsed_header_bytes.contains_key(&line_start) {
        return Some(target_position);
    }

    // Check LSP folding ranges first (line-based comparison unavoidable).
    // Resolve markers to current line numbers post-edit.
    if !state.folding_ranges.is_empty() {
        let line = state.buffer.get_line_number(target_position);
        let resolved = state
            .folding_ranges
            .resolved(&state.buffer, &state.marker_list);
        let has_lsp_fold = resolved.iter().any(|range| {
            let start_line = range.start_line as usize;
            let end_line = range.end_line as usize;
            start_line == line && end_line > start_line
        });
        if has_lsp_fold {
            return Some(target_position);
        }
    }

    // Fallback: indent-based foldable detection on bytes when LSP ranges are empty
    if state.folding_ranges.is_empty() {
        let tab_size = state.buffer_settings.tab_size;
        let max_scan = crate::config::INDENT_FOLD_INDICATOR_MAX_SCAN;
        let max_bytes = max_scan * state.buffer.estimated_line_length();
        if indent_folding::indent_fold_end_byte(&state.buffer, line_start, tab_size, max_bytes)
            .is_some()
        {
            return Some(target_position);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adjust_for_compose_passes_through_when_compose_disabled() {
        let r = Rect::new(10, 5, 100, 20);
        assert_eq!(adjust_content_rect_for_compose(r, None), r);
    }

    #[test]
    fn adjust_for_compose_passes_through_when_compose_wider_than_content() {
        let r = Rect::new(10, 5, 100, 20);
        // compose_width >= content width: nothing to do.
        assert_eq!(adjust_content_rect_for_compose(r, Some(120)), r);
        assert_eq!(adjust_content_rect_for_compose(r, Some(100)), r);
    }

    #[test]
    fn adjust_for_compose_centers_narrower_content() {
        let r = Rect::new(10, 5, 100, 20);
        let adjusted = adjust_content_rect_for_compose(r, Some(60));
        // 100 - 60 = 40 slack, half (=20) on the left:
        assert_eq!(adjusted.x, 30);
        assert_eq!(adjusted.y, 5);
        assert_eq!(adjusted.width, 60);
        assert_eq!(adjusted.height, 20);
    }

    #[test]
    fn adjust_for_compose_handles_odd_slack() {
        // 100 - 63 = 37 slack, left = 18, right = 19.
        let r = Rect::new(0, 0, 100, 20);
        let adjusted = adjust_content_rect_for_compose(r, Some(63));
        assert_eq!(adjusted.x, 18);
        assert_eq!(adjusted.width, 63);
    }

    #[test]
    fn adjust_for_compose_clamps_width_to_minimum_of_one() {
        let r = Rect::new(0, 0, 10, 2);
        let adjusted = adjust_content_rect_for_compose(r, Some(0));
        // compose_width = 0 gets clamped to 1.
        assert_eq!(adjusted.width, 1);
    }

    #[test]
    fn screen_to_buffer_position_returns_fallback_when_no_mappings() {
        let r = Rect::new(0, 0, 100, 20);
        let pos = screen_to_buffer_position(
            /* col */ 5, /* row */ 5, /* content_rect */ r,
            /* gutter_width */ 3, /* cached_mappings */ &None,
            /* fallback_position */ 42, /* allow_gutter_click */ true,
            /* compose_width */ None,
        );
        assert_eq!(pos, Some(42));
    }

    #[test]
    fn screen_to_buffer_position_rejects_gutter_click_when_not_allowed() {
        let r = Rect::new(0, 0, 100, 20);
        // col 1 is inside the 3-wide gutter.
        let pos = screen_to_buffer_position(1, 2, r, 3, &None, 0, false, None);
        assert_eq!(pos, None);
    }
}
