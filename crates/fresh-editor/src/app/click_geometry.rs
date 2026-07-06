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
    screen_to_buffer_position_with_overshoot(
        col,
        row,
        content_rect,
        gutter_width,
        cached_mappings,
        fallback_position,
        allow_gutter_click,
        compose_width,
    )
    .map(|target| target.position)
}

/// A click target resolved by [`screen_to_buffer_position_with_overshoot`].
#[derive(Debug, Clone, Copy)]
pub(crate) struct ClickTarget {
    /// Buffer byte position the click resolves to (clipped to content).
    pub position: usize,
    /// Screen cells past the end of the clicked row's rendered content
    /// (0 when the click hit a real cell).
    pub col_overshoot: usize,
    /// Visual rows below the last rendered row (0 when the click hit a
    /// rendered row). Vertical virtual space uses this to place the cursor
    /// on lines below the end of the buffer.
    pub row_overshoot: usize,
    /// The clicked column within the content area (viewport-relative,
    /// after the gutter).
    pub text_col: usize,
}

/// Like [`screen_to_buffer_position`], but also reports how far past the
/// rendered content the click landed (columns past the row's content and
/// rows below the last rendered row). Virtual-space mouse placement uses
/// the overshoots to derive the clicked position beyond the line/buffer
/// end.
#[allow(clippy::too_many_arguments)]
pub(crate) fn screen_to_buffer_position_with_overshoot(
    col: u16,
    row: u16,
    content_rect: Rect,
    gutter_width: u16,
    cached_mappings: &Option<Vec<ViewLineMapping>>,
    fallback_position: usize,
    allow_gutter_click: bool,
    compose_width: Option<u16>,
) -> Option<ClickTarget> {
    let orig_content_rect = content_rect;
    let mut content_rect = adjust_content_rect_for_compose(content_rect, compose_width);

    // Mirror the render-time gutter reclaim (issue #2146): in compose mode the
    // indicator gutter is drawn in the reclaimed left margin, so the view-line
    // mappings' origin sits `gutter_width` columns left of the centered paper.
    // Apply the same shift here (only when there was margin room to reclaim,
    // matching `calculate_compose_layout` + the reclaim in render_buffer).
    if compose_width.is_some() && gutter_width > 0 {
        let left_pad = content_rect.x.saturating_sub(orig_content_rect.x);
        if left_pad >= gutter_width {
            content_rect.x -= gutter_width;
            content_rect.width += gutter_width;
        }
    }

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

    // Helper to get position (and cells past the rendered content) from a
    // line mapping at a given visual column.
    let position_from_mapping = |line_mapping: &ViewLineMapping, col: usize| -> (usize, usize) {
        // Column of the cell just past the last *content* cell: the last
        // source-backed cell whose byte is before `line_end_byte` (the
        // newline cell and trailing decoration-only cells don't count).
        // A click at or beyond this column is a click past the line's
        // content; the difference is the virtual-space overshoot.
        let content_end_col = line_mapping
            .visual_to_char
            .iter()
            .enumerate()
            .rev()
            .find(|(_, &char_idx)| {
                line_mapping
                    .char_source_bytes
                    .get(char_idx)
                    .is_some_and(|b| b.is_some_and(|b| b < line_mapping.line_end_byte))
            })
            .map(|(visual_col, _)| visual_col + 1)
            .unwrap_or(0);

        if col < content_end_col {
            // Use O(1) lookup: visual column -> char index -> source byte
            if let Some(byte_pos) = line_mapping.source_byte_at_visual_col(col) {
                return (byte_pos, 0);
            }
            // Column maps to virtual/injected content - find nearest real position
            for c in (0..col).rev() {
                if let Some(byte_pos) = line_mapping.source_byte_at_visual_col(c) {
                    return (byte_pos, 0);
                }
            }
            (line_mapping.line_end_byte, 0)
        } else {
            // Click is past end of visible content.
            let overshoot = col - content_end_col;
            // For empty lines (only a newline), return the line start position
            // to keep cursor on this line rather than jumping to the next line.
            if line_mapping.visual_to_char.len() <= 1 {
                if let Some(Some(first_byte)) = line_mapping.char_source_bytes.first() {
                    return (*first_byte, overshoot);
                }
            }
            (line_mapping.line_end_byte, overshoot)
        }
    };

    let (position, col_overshoot, row_overshoot) = cached_mappings
        .as_ref()
        .and_then(|mappings| {
            if let Some(line_mapping) = mappings.get(visual_row) {
                // Click is on a visible line
                let (position, col_overshoot) = position_from_mapping(line_mapping, text_col);
                Some((position, col_overshoot, 0))
            } else if !mappings.is_empty() {
                // Click is below last visible line — use the last line at the clicked column
                let last_mapping = mappings.last().unwrap();
                let (position, col_overshoot) = position_from_mapping(last_mapping, text_col);
                Some((position, col_overshoot, visual_row - (mappings.len() - 1)))
            } else {
                None
            }
        })
        .unwrap_or((fallback_position, 0, 0));

    Some(ClickTarget {
        position,
        col_overshoot,
        row_overshoot,
        text_col,
    })
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
