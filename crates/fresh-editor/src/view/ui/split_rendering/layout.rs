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
use ratatui::Frame;
use std::collections::HashMap;

/// Anchor describing where to start rendering within a slice of view lines.
pub(super) struct ViewAnchor {
    pub start_line_idx: usize,
    pub start_line_skip: usize,
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
/// cursor is visible.
pub(super) fn sync_viewport_to_content(
    viewport: &mut Viewport,
    buffer: &mut Buffer,
    cursors: &Cursors,
    content_rect: Rect,
    hidden_ranges: &[(usize, usize)],
) {
    let size_changed =
        viewport.width != content_rect.width || viewport.height != content_rect.height;

    if size_changed {
        viewport.resize(content_rect.width, content_rect.height);
    }

    let primary = *cursors.primary();
    viewport.ensure_visible(buffer, &primary, hidden_ranges);
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

/// Resolve the first line index and skip count for rendering, given the
/// viewport's top byte.
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
                    start_line_skip: 0,
                };
            }
        }
    }

    ViewAnchor {
        start_line_idx: 0,
        start_line_skip: 0,
    }
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

/// Compute the byte offset just past the last visible line of the viewport.
pub(super) fn calculate_viewport_end(
    state: &mut EditorState,
    viewport_start: usize,
    estimated_line_length: usize,
    visible_count: usize,
) -> usize {
    let mut iter_temp = state
        .buffer
        .line_iterator(viewport_start, estimated_line_length);
    let mut viewport_end = viewport_start;
    for _ in 0..visible_count {
        if let Some((line_start, line_content)) = iter_temp.next_line() {
            viewport_end = line_start + line_content.len();
        } else {
            break;
        }
    }
    viewport_end
}

/// Draw the separator line between two splits.
pub(super) fn render_separator(
    frame: &mut Frame,
    direction: SplitDirection,
    x: u16,
    y: u16,
    length: u16,
    theme: &Theme,
) {
    match direction {
        SplitDirection::Horizontal => {
            let line_area = Rect::new(x, y, length, 1);
            let line_text = "─".repeat(length as usize);
            let paragraph =
                Paragraph::new(line_text).style(Style::default().fg(theme.split_separator_fg));
            frame.render_widget(paragraph, line_area);
        }
        SplitDirection::Vertical => {
            for offset in 0..length {
                let cell_area = Rect::new(x, y + offset, 1, 1);
                let paragraph =
                    Paragraph::new("│").style(Style::default().fg(theme.split_separator_fg));
                frame.render_widget(paragraph, cell_area);
            }
        }
    }
}
