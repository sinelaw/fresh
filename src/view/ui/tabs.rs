//! Tab bar rendering for multiple buffers

use crate::app::BufferMetadata;
use crate::model::event::BufferId;
use crate::primitives::display_width::str_width;
use crate::state::EditorState;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Paragraph};
use ratatui::Frame;
use std::collections::HashMap;

/// Renders the tab bar showing open buffers
pub struct TabsRenderer;

/// Compute a scroll offset that keeps the active tab fully visible.
/// `tab_widths` should include separators; `active_idx` refers to the tab index (not counting separators).
pub fn compute_tab_scroll_offset(
    tab_widths: &[usize],
    active_idx: usize,
    max_width: usize,
    current_offset: usize,
    padding_between_tabs: usize,
) -> usize {
    if tab_widths.is_empty() || max_width == 0 {
        return 0;
    }

    let total_width: usize = tab_widths.iter().sum::<usize>()
        + padding_between_tabs.saturating_mul(tab_widths.len().saturating_sub(1));
    let mut tab_start = 0usize;
    let mut tab_end = 0usize;

    // Walk through widths to locate active tab boundaries.
    for (tab_counter, w) in tab_widths.iter().enumerate() {
        let next = tab_start + *w;
        if tab_counter == active_idx {
            tab_end = next;
            break;
        }
        tab_start = next + padding_between_tabs;
    }

    // If we didn't find the tab, keep current offset.
    if tab_end == 0 {
        return current_offset.min(total_width.saturating_sub(max_width));
    }

    // Basic rule: stick the active tab into view, prefer left-aligned start unless it overflows.
    let mut offset = tab_start;
    if tab_end.saturating_sub(offset) > max_width {
        offset = tab_end.saturating_sub(max_width);
    }

    offset.min(total_width.saturating_sub(max_width))
}

#[cfg(test)]
mod tests {
    use super::compute_tab_scroll_offset;

    #[test]
    fn offset_clamped_to_zero_when_active_first() {
        let widths = vec![5, 5, 5]; // tab widths
        let offset = compute_tab_scroll_offset(&widths, 0, 6, 10, 1);
        assert_eq!(offset, 0);
    }

    #[test]
    fn offset_moves_to_show_active_tab() {
        let widths = vec![5, 8, 6]; // active is the middle tab (index 1)
        let offset = compute_tab_scroll_offset(&widths, 1, 6, 0, 1);
        // Active tab width 8 cannot fully fit into width 6; expect it to right-align within view.
        assert_eq!(offset, 8);
    }

    #[test]
    fn offset_respects_total_width_bounds() {
        let widths = vec![3, 3, 3, 3];
        let offset = compute_tab_scroll_offset(&widths, 3, 4, 100, 1);
        let total: usize = widths.iter().sum();
        let total_with_padding = total + 3; // three gaps of width 1
        assert!(offset <= total_with_padding.saturating_sub(4));
    }
}

impl TabsRenderer {
    /// Render the tab bar for a specific split showing only its open buffers
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render the tabs in
    /// * `split_buffers` - List of buffer IDs open in this split (in order)
    /// * `buffers` - All open buffers (for accessing state/metadata)
    /// * `buffer_metadata` - Metadata for buffers (contains display names for virtual buffers)
    /// * `active_buffer` - The currently active buffer ID for this split
    /// * `theme` - The active theme for colors
    /// * `is_active_split` - Whether this split is the active one
    /// * `hovered_tab` - Optional (buffer_id, is_close_button) if a tab is being hovered
    ///
    /// # Returns
    /// Vec of (buffer_id, tab_start_col, tab_end_col, close_start_col) for each visible tab.
    /// These are absolute screen column positions for hit testing.
    #[allow(clippy::too_many_arguments)]
    pub fn render_for_split(
        frame: &mut Frame,
        area: Rect,
        split_buffers: &[BufferId],
        buffers: &HashMap<BufferId, EditorState>,
        buffer_metadata: &HashMap<BufferId, BufferMetadata>,
        composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
        active_buffer: BufferId,
        theme: &crate::view::theme::Theme,
        is_active_split: bool,
        tab_scroll_offset: usize,
        hovered_tab: Option<(BufferId, bool)>, // (buffer_id, is_close_button)
    ) -> Vec<(BufferId, u16, u16, u16)> {
        const SCROLL_INDICATOR_LEFT: &str = "<";
        const SCROLL_INDICATOR_RIGHT: &str = ">";
        const SCROLL_INDICATOR_WIDTH: usize = 1; // Width of "<" or ">"

        let mut all_tab_spans: Vec<(Span, usize)> = Vec::new(); // Store (Span, display_width)
        let mut tab_ranges: Vec<(usize, usize, usize)> = Vec::new(); // (start, end, close_start) positions for each tab
        let mut rendered_buffer_ids: Vec<BufferId> = Vec::new(); // Track which buffers actually got rendered

        // First, build all spans and calculate their display widths
        for id in split_buffers.iter() {
            // Check if this is a regular buffer or a composite buffer
            let is_regular_buffer = buffers.contains_key(id);
            let is_composite_buffer = composite_buffers.contains_key(id);

            if !is_regular_buffer && !is_composite_buffer {
                continue;
            }

            // Skip buffers that are marked as hidden from tabs (e.g., composite source buffers)
            if let Some(meta) = buffer_metadata.get(id) {
                if meta.hidden_from_tabs {
                    continue;
                }
            }
            rendered_buffer_ids.push(*id);

            let meta = buffer_metadata.get(id);
            let is_terminal = meta
                .and_then(|m| m.virtual_mode())
                .map(|mode| mode == "terminal")
                .unwrap_or(false);

            // For composite buffers, use display_name from metadata
            // For regular buffers, try file_path first, then display_name
            let name = if is_composite_buffer {
                meta.map(|m| m.display_name.as_str())
            } else if is_terminal {
                meta.map(|m| m.display_name.as_str())
            } else {
                buffers
                    .get(id)
                    .and_then(|state| state.buffer.file_path())
                    .and_then(|p| p.file_name())
                    .and_then(|n| n.to_str())
                    .or_else(|| meta.map(|m| m.display_name.as_str()))
            }
            .unwrap_or("[No Name]");

            // For composite buffers, never show as modified (they're read-only views)
            let modified = if is_composite_buffer {
                ""
            } else if let Some(state) = buffers.get(id) {
                if state.buffer.is_modified() {
                    "*"
                } else {
                    ""
                }
            } else {
                ""
            };
            let binary_indicator = if buffer_metadata.get(id).map(|m| m.binary).unwrap_or(false) {
                " [BIN]"
            } else {
                ""
            };

            let is_active = *id == active_buffer;

            // Check hover state for this tab
            let (is_hovered_name, is_hovered_close) = match hovered_tab {
                Some((hover_buf, is_close)) if hover_buf == *id => (!is_close, is_close),
                _ => (false, false),
            };

            // Determine base style
            let base_style = if is_active {
                if is_active_split {
                    Style::default()
                        .fg(theme.tab_active_fg)
                        .bg(theme.tab_active_bg)
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default()
                        .fg(theme.tab_active_fg)
                        .bg(theme.tab_inactive_bg)
                        .add_modifier(Modifier::BOLD)
                }
            } else if is_hovered_name {
                // Non-active tab with name hovered - use hover background
                Style::default()
                    .fg(theme.tab_inactive_fg)
                    .bg(theme.tab_hover_bg)
            } else {
                Style::default()
                    .fg(theme.tab_inactive_fg)
                    .bg(theme.tab_inactive_bg)
            };

            // Style for the close button
            let close_style = if is_hovered_close {
                // Close button hovered - use hover color
                base_style.fg(theme.tab_close_hover_fg)
            } else {
                base_style
            };

            // Build tab content: " {name}{modified}{binary_indicator} "
            let tab_name_text = format!(" {name}{modified}{binary_indicator} ");
            let tab_name_width = str_width(&tab_name_text);

            // Close button: "× "
            let close_text = "× ";
            let close_width = str_width(close_text);

            let total_width = tab_name_width + close_width;

            let start_pos: usize = all_tab_spans.iter().map(|(_, w)| w).sum();
            let close_start_pos = start_pos + tab_name_width;
            let end_pos = start_pos + total_width;
            tab_ranges.push((start_pos, end_pos, close_start_pos));

            // Add name span
            all_tab_spans.push((Span::styled(tab_name_text, base_style), tab_name_width));
            // Add close button span (can have different style when hovered)
            all_tab_spans.push((
                Span::styled(close_text.to_string(), close_style),
                close_width,
            ));
        }

        // Add separators between tabs (we do this after the loop to handle hidden buffers correctly)
        // We'll rebuild all_tab_spans with separators inserted, and fix up tab_ranges
        // to account for the separator widths
        let mut final_spans: Vec<(Span<'static>, usize)> = Vec::new();
        let mut separator_offset = 0usize;
        let spans_per_tab = 2; // name + close button
        for (tab_idx, chunk) in all_tab_spans.chunks(spans_per_tab).enumerate() {
            // Adjust tab_ranges for this tab to account for separators before it
            if separator_offset > 0 {
                let (start, end, close_start) = tab_ranges[tab_idx];
                tab_ranges[tab_idx] = (
                    start + separator_offset,
                    end + separator_offset,
                    close_start + separator_offset,
                );
            }

            for span in chunk {
                final_spans.push(span.clone());
            }
            // Add separator if not the last tab
            if tab_idx < rendered_buffer_ids.len().saturating_sub(1) {
                final_spans.push((
                    Span::styled(" ", Style::default().bg(theme.tab_separator_bg)),
                    1,
                ));
                separator_offset += 1;
            }
        }
        #[allow(clippy::let_and_return)]
        let all_tab_spans = final_spans;

        let mut current_spans: Vec<Span> = Vec::new();
        let max_width = area.width as usize;

        let total_width: usize = all_tab_spans.iter().map(|(_, w)| w).sum();
        // Use rendered_buffer_ids (not split_buffers) to find active index,
        // since some buffers may have been skipped if not in buffers HashMap
        let active_tab_idx = rendered_buffer_ids
            .iter()
            .position(|id| *id == active_buffer);

        let mut tab_widths: Vec<usize> = Vec::new();
        for (start, end, _close_start) in &tab_ranges {
            tab_widths.push(end.saturating_sub(*start));
        }

        let mut offset = tab_scroll_offset.min(total_width.saturating_sub(max_width));
        if let Some(active_idx) = active_tab_idx {
            offset = compute_tab_scroll_offset(
                &tab_widths,
                active_idx,
                max_width,
                tab_scroll_offset,
                1, // separator width between tabs
            );
        }

        // Indicators reserve space; adjust once so the active tab still fits.
        let mut show_left = offset > 0;
        let mut show_right = total_width.saturating_sub(offset) > max_width;
        let mut available = max_width
            .saturating_sub((show_left as usize + show_right as usize) * SCROLL_INDICATOR_WIDTH);

        if let Some(active_idx) = active_tab_idx {
            let (start, end, _close_start) = tab_ranges[active_idx];
            let active_width = end.saturating_sub(start);
            if start == 0 && active_width >= max_width {
                show_left = false;
                show_right = false;
                available = max_width;
            }

            if end.saturating_sub(offset) > available {
                offset = end.saturating_sub(available);
                offset = offset.min(total_width.saturating_sub(available));
                show_left = offset > 0;
                show_right = total_width.saturating_sub(offset) > available;
                available = max_width.saturating_sub(
                    (show_left as usize + show_right as usize) * SCROLL_INDICATOR_WIDTH,
                );
            }
            if start < offset {
                offset = start;
                show_left = offset > 0;
                show_right = total_width.saturating_sub(offset) > available;
            }
        }

        let mut rendered_width = 0;
        let mut skip_chars_count = offset;

        if show_left {
            current_spans.push(Span::styled(
                SCROLL_INDICATOR_LEFT,
                Style::default().bg(theme.tab_separator_bg),
            ));
            rendered_width += SCROLL_INDICATOR_WIDTH;
        }

        for (mut span, width) in all_tab_spans.into_iter() {
            if skip_chars_count >= width {
                skip_chars_count -= width;
                continue;
            }

            let visible_chars_in_span = width - skip_chars_count;
            if rendered_width + visible_chars_in_span
                > max_width.saturating_sub(if show_right {
                    SCROLL_INDICATOR_WIDTH
                } else {
                    0
                })
            {
                let remaining_width =
                    max_width
                        .saturating_sub(rendered_width)
                        .saturating_sub(if show_right {
                            SCROLL_INDICATOR_WIDTH
                        } else {
                            0
                        });
                let truncated_content = span
                    .content
                    .chars()
                    .skip(skip_chars_count)
                    .take(remaining_width)
                    .collect::<String>();
                span.content = std::borrow::Cow::Owned(truncated_content);
                current_spans.push(span);
                rendered_width += remaining_width;
                break;
            } else {
                let visible_content = span
                    .content
                    .chars()
                    .skip(skip_chars_count)
                    .collect::<String>();
                span.content = std::borrow::Cow::Owned(visible_content);
                current_spans.push(span);
                rendered_width += visible_chars_in_span;
                skip_chars_count = 0;
            }
        }

        if show_right && rendered_width < max_width {
            current_spans.push(Span::styled(
                SCROLL_INDICATOR_RIGHT,
                Style::default().bg(theme.tab_separator_bg),
            ));
            rendered_width += SCROLL_INDICATOR_WIDTH;
        }

        if rendered_width < max_width {
            current_spans.push(Span::styled(
                " ".repeat(max_width.saturating_sub(rendered_width)),
                Style::default().bg(theme.tab_separator_bg),
            ));
        }

        let line = Line::from(current_spans);
        let block = Block::default().style(Style::default().bg(theme.tab_separator_bg));
        let paragraph = Paragraph::new(line).block(block);
        frame.render_widget(paragraph, area);

        // Compute and return hit areas for mouse interaction
        // We need to map the logical tab positions to screen positions accounting for:
        // 1. The scroll offset
        // 2. The left scroll indicator (if shown)
        // 3. The base area.x position
        let left_indicator_offset = if show_left { SCROLL_INDICATOR_WIDTH } else { 0 };

        let mut hit_areas = Vec::new();
        for (idx, buffer_id) in rendered_buffer_ids.iter().enumerate() {
            let (logical_start, logical_end, logical_close_start) = tab_ranges[idx];

            // Convert logical positions to screen positions
            // Screen position = area.x + left_indicator_offset + (logical_pos - scroll_offset)
            // But we need to clamp to visible area
            let visible_start = offset;
            let visible_end = offset + available;

            // Skip tabs that are completely scrolled out of view
            if logical_end <= visible_start || logical_start >= visible_end {
                continue;
            }

            // Calculate visible portion of this tab
            let screen_start = if logical_start >= visible_start {
                area.x + left_indicator_offset as u16 + (logical_start - visible_start) as u16
            } else {
                area.x + left_indicator_offset as u16
            };

            let screen_end = if logical_end <= visible_end {
                area.x + left_indicator_offset as u16 + (logical_end - visible_start) as u16
            } else {
                area.x + left_indicator_offset as u16 + available as u16
            };

            // Close button position (if visible)
            let screen_close_start = if logical_close_start >= visible_start
                && logical_close_start < visible_end
            {
                area.x + left_indicator_offset as u16 + (logical_close_start - visible_start) as u16
            } else if logical_close_start < visible_start {
                // Close button is partially/fully scrolled off left - use screen_start
                screen_start
            } else {
                // Close button is scrolled off right
                screen_end
            };

            hit_areas.push((*buffer_id, screen_start, screen_end, screen_close_start));
        }

        hit_areas
    }

    /// Legacy render function for backward compatibility
    /// Renders all buffers as tabs (used during transition)
    #[allow(dead_code)]
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        buffers: &HashMap<BufferId, EditorState>,
        buffer_metadata: &HashMap<BufferId, BufferMetadata>,
        composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
        active_buffer: BufferId,
        theme: &crate::view::theme::Theme,
    ) {
        // Sort buffer IDs to ensure consistent tab order
        let mut buffer_ids: Vec<_> = buffers.keys().copied().collect();
        buffer_ids.sort_by_key(|id| id.0);

        Self::render_for_split(
            frame,
            area,
            &buffer_ids,
            buffers,
            buffer_metadata,
            composite_buffers,
            active_buffer,
            theme,
            true, // Legacy behavior: always treat as active
            0,    // Default tab_scroll_offset for legacy render
            None, // No hover state for legacy render
        );
    }
}
