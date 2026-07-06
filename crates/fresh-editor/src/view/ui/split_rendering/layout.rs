//! Layout & geometry helpers for split panes.
//!
//! Everything in this module deals with rectangles, view anchors, viewport
//! bounds, view preferences, and per-split tab configuration. Nothing here
//! depends on any shared render-time "mega struct".

use crate::model::buffer::Buffer;
use crate::model::cursor::Cursors;
use crate::model::event::{BufferId, LeafId, SplitDirection};
use crate::state::{EditorState, ViewMode};
use crate::view::split::{SplitViewState, TabTarget};
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::ViewLine;
use crate::view::viewport::Viewport;
use fresh_core::api::ViewTransformPayload;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::widgets::Paragraph;
use ratatui::widgets::Widget;
use std::collections::HashMap;

/// Anchor describing where to start rendering within a slice of view lines.
pub(super) struct ViewAnchor {
    pub start_line_idx: usize,
}

/// Layout for compose (centered page) mode: the effective render area and
/// the side paddings.
pub(super) struct ComposeLayout {
    pub render_area: Rect,
    pub left_pad: u16,
    pub right_pad: u16,
}

/// Rectangle partitioning for one split: tabs, content, vertical scrollbar,
/// horizontal scrollbar.
pub(super) struct SplitLayout {
    pub tabs_rect: Rect,
    pub content_rect: Rect,
    pub scrollbar_rect: Rect,
    pub horizontal_scrollbar_rect: Rect,
}

/// View-level preferences resolved from `SplitViewState` (with fallback
/// defaults).
pub(super) struct ViewPreferences {
    pub view_mode: ViewMode,
    pub compose_width: Option<u16>,
    pub compose_column_guides: Option<Vec<u16>>,
    pub view_transform: Option<ViewTransformPayload>,
    pub rulers: Vec<usize>,
    /// Per-split line number visibility (from BufferViewState).
    pub show_line_numbers: bool,
    /// Per-split current line highlight visibility (from BufferViewState).
    pub highlight_current_line: bool,
}

/// Partition a split area into tabs / content / scrollbar rectangles.
pub(super) fn split_layout(
    split_area: Rect,
    tab_bar_visible: bool,
    show_vertical_scrollbar: bool,
    show_horizontal_scrollbar: bool,
) -> SplitLayout {
    let tabs_height = if tab_bar_visible { 1u16 } else { 0u16 };
    let scrollbar_width = if show_vertical_scrollbar { 1u16 } else { 0u16 };
    let hscrollbar_height = if show_horizontal_scrollbar {
        1u16
    } else {
        0u16
    };

    let tabs_rect = Rect::new(split_area.x, split_area.y, split_area.width, tabs_height);
    let content_rect = Rect::new(
        split_area.x,
        split_area.y + tabs_height,
        split_area.width.saturating_sub(scrollbar_width),
        split_area
            .height
            .saturating_sub(tabs_height)
            .saturating_sub(hscrollbar_height),
    );
    let scrollbar_rect = Rect::new(
        split_area.x + split_area.width.saturating_sub(scrollbar_width),
        split_area.y + tabs_height,
        scrollbar_width,
        split_area
            .height
            .saturating_sub(tabs_height)
            .saturating_sub(hscrollbar_height),
    );
    let horizontal_scrollbar_rect = Rect::new(
        split_area.x,
        split_area.y + split_area.height.saturating_sub(hscrollbar_height),
        split_area.width.saturating_sub(scrollbar_width),
        hscrollbar_height,
    );

    SplitLayout {
        tabs_rect,
        content_rect,
        scrollbar_rect,
        horizontal_scrollbar_rect,
    }
}

/// Return the open-buffer list and tab scroll offset for a split.
pub(super) fn split_buffers_for_tabs(
    split_view_states: Option<&HashMap<LeafId, SplitViewState>>,
    split_id: LeafId,
    buffer_id: BufferId,
) -> (Vec<TabTarget>, usize) {
    if let Some(view_states) = split_view_states {
        if let Some(view_state) = view_states.get(&split_id) {
            return (
                view_state.open_buffers.clone(),
                view_state.tab_scroll_offset,
            );
        }
    }
    (vec![TabTarget::Buffer(buffer_id)], 0)
}

/// Sync viewport width/height to `content_rect`, and ensure the primary
/// cursor is visible (unless `pin_to_top` keeps a self-managing panel
/// anchored at the top).
#[allow(clippy::too_many_arguments)]
pub(super) fn sync_viewport_to_content(
    viewport: &mut Viewport,
    buffer: &mut Buffer,
    cursors: &Cursors,
    content_rect: Rect,
    hidden_ranges: &[(usize, usize)],
    compose_width: Option<u16>,
    show_line_numbers: bool,
    pin_to_top: bool,
    virtual_space: crate::config::VirtualSpaceMode,
) {
    let size_changed =
        viewport.width != content_rect.width || viewport.height != content_rect.height;

    if size_changed {
        viewport.resize(content_rect.width, content_rect.height);
    }

    // Mirror per-split state into the viewport so its scroll math
    // sees the renderer's effective wrap width / gutter.  Without
    // this, on a wide terminal with `compose_width` set, scroll math
    // counts visual rows at the raw split width while the renderer
    // wraps at the compose-clamped width — `max_scroll_row` ends up
    // too small and the user can't reach the buffer's tail.
    viewport.compose_width = compose_width;
    viewport.show_line_numbers = show_line_numbers;

    // A pinned (non-scrollable) panel owns its own content window (its
    // List/Tree scrolls internally), so the buffer viewport must stay
    // anchored at the top. `ensure_visible` would otherwise chase the
    // hidden cursor and push the panel's header chrome off-screen
    // (issue #2434 follow-up).
    if pin_to_top {
        viewport.top_byte = 0;
        viewport.top_view_line_offset = 0;
        viewport.left_column = 0;
        return;
    }

    let primary = *cursors.primary();
    let virtual_columns =
        crate::model::virtual_space::cursor_virtual_columns(virtual_space, buffer, &primary);
    viewport.ensure_visible_with_virtual(buffer, &primary, hidden_ranges, virtual_columns);
}

/// Pull per-split view preferences from `SplitViewState`, falling back to
/// reasonable defaults when one is not available.
pub(super) fn resolve_view_preferences(
    _state: &EditorState,
    split_view_states: Option<&HashMap<LeafId, SplitViewState>>,
    split_id: LeafId,
) -> ViewPreferences {
    if let Some(view_states) = split_view_states {
        if let Some(view_state) = view_states.get(&split_id) {
            return ViewPreferences {
                view_mode: view_state.view_mode.clone(),
                compose_width: view_state.compose_width,
                compose_column_guides: view_state.compose_column_guides.clone(),
                view_transform: view_state.view_transform.clone(),
                rulers: view_state.rulers.clone(),
                show_line_numbers: view_state.show_line_numbers,
                highlight_current_line: view_state.highlight_current_line,
            };
        }
    }

    ViewPreferences {
        view_mode: ViewMode::Source,
        compose_width: None,
        compose_column_guides: None,
        view_transform: None,
        rulers: Vec::new(),
        show_line_numbers: true,
        highlight_current_line: true,
    }
}

/// Resolve the first line index for rendering, given the viewport's top byte.
pub(super) fn calculate_view_anchor(view_lines: &[ViewLine], top_byte: usize) -> ViewAnchor {
    for (idx, line) in view_lines.iter().enumerate() {
        if let Some(first_source) = line.char_source_bytes.iter().find_map(|m| *m) {
            if first_source >= top_byte {
                // Found a line with source >= top_byte, but we may need to
                // include previous lines if they're injected headers.
                let mut start_idx = idx;
                while start_idx > 0 {
                    let prev_line = &view_lines[start_idx - 1];
                    let prev_has_source = prev_line.char_source_bytes.iter().any(|m| m.is_some());
                    if !prev_has_source {
                        start_idx -= 1;
                    } else {
                        break;
                    }
                }
                return ViewAnchor {
                    start_line_idx: start_idx,
                };
            }
        }
    }

    ViewAnchor { start_line_idx: 0 }
}

/// Compute the compose-mode layout for a given area.
pub(super) fn calculate_compose_layout(
    area: Rect,
    view_mode: &ViewMode,
    compose_width: Option<u16>,
) -> ComposeLayout {
    // Enable centering/margins if:
    // 1. View mode is explicitly Compose, OR
    // 2. compose_width is set (plugin-driven compose mode)
    let should_compose = view_mode == &ViewMode::PageView || compose_width.is_some();

    if !should_compose {
        return ComposeLayout {
            render_area: area,
            left_pad: 0,
            right_pad: 0,
        };
    }

    let target_width = compose_width.unwrap_or(area.width);
    let clamped_width = target_width.min(area.width).max(1);
    if clamped_width >= area.width {
        return ComposeLayout {
            render_area: area,
            left_pad: 0,
            right_pad: 0,
        };
    }

    let pad_total = area.width - clamped_width;
    let left_pad = pad_total / 2;
    let right_pad = pad_total - left_pad;

    ComposeLayout {
        render_area: Rect::new(area.x + left_pad, area.y, clamped_width, area.height),
        left_pad,
        right_pad,
    }
}

/// A line whose content reaches this many bytes is treated as "over-long": it
/// already fills (far past) the visible row, and with horizontal scrolling
/// (line-wrap off) it occupies a single screen row. `LineIterator` also splits
/// such lines into chunks of `MAX_LINE_BYTES` (100 KB) and yields each chunk as
/// a separate "line", so without a guard `calculate_viewport_end` would walk
/// chunk-by-chunk hundreds of KB into the line. Kept in step with that limit.
const OVERLONG_LINE_BYTES: usize = 100_000;

/// Compute the byte offset just past the last visible line of the viewport.
///
/// The returned `viewport_start..viewport_end` span is what the decoration pass
/// (syntax highlight, reference highlight, bracket/rainbow overlay) scans every
/// frame. For an over-long line — e.g. a minified SVG/JSON on a single line —
/// the line can be hundreds of KB while only `viewport_width` columns are
/// actually on screen, so each line's contribution is clamped to the visible
/// horizontal window (`left_column + viewport_width`, with slack for multibyte
/// content) and the walk stops at the first over-long line. Without this every
/// cursor move re-scans the whole line, which is the per-keystroke lag in
/// issue #2529.
pub(super) fn calculate_viewport_end(
    state: &mut EditorState,
    viewport_start: usize,
    estimated_line_length: usize,
    visible_count: usize,
    left_column: usize,
    viewport_width: usize,
) -> usize {
    // Upper bound, in bytes, on how far past a line's start the visible window
    // can reach. One displayed column maps to at most ~4 source bytes (a 4-byte
    // UTF-8 scalar), so `(left_column + viewport_width) * 4` safely covers the
    // visible cells while keeping the scanned span proportional to the screen,
    // not the line. `0` width (only seen in degenerate callers) disables the
    // clamp so we keep the previous full-line behavior.
    let visible_byte_budget = left_column.saturating_add(viewport_width).saturating_mul(4);

    let mut iter_temp = state
        .buffer
        .line_iterator(viewport_start, estimated_line_length);
    let mut viewport_end = viewport_start;
    for _ in 0..visible_count {
        let Some((line_start, line_content)) = iter_temp.next_line() else {
            break;
        };
        let line_len = line_content.len();
        let line_end = line_start + line_len;

        if visible_byte_budget == 0 {
            viewport_end = line_end;
            continue;
        }

        let clamped_end = line_end.min(line_start.saturating_add(visible_byte_budget));
        viewport_end = clamped_end;

        // An over-long line fills its single (unwrapped) screen row and may be
        // yielded as multiple 100 KB chunks; the clamp above already covers the
        // visible window, so stop rather than walking the rest of the line.
        if line_len >= OVERLONG_LINE_BYTES {
            break;
        }
    }
    viewport_end
}

/// Draw the separator line between two splits.
pub(super) fn render_separator(
    buf: &mut ratatui::buffer::Buffer,
    direction: SplitDirection,
    x: u16,
    y: u16,
    length: u16,
    theme: &Theme,
) {
    let style = Style::default()
        .fg(theme.split_separator_fg)
        .bg(theme.editor_bg);
    match direction {
        SplitDirection::Horizontal => {
            let line_area = Rect::new(x, y, length, 1);
            let line_text = "─".repeat(length as usize);
            let paragraph = Paragraph::new(line_text).style(style);
            paragraph.render(line_area, buf);
        }
        SplitDirection::Vertical => {
            for offset in 0..length {
                let cell_area = Rect::new(x, y + offset, 1, 1);
                let paragraph = Paragraph::new("│").style(style);
                paragraph.render(cell_area, buf);
            }
        }
    }
}
