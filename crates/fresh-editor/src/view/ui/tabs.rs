//! Tab bar rendering for multiple buffers

use crate::app::BufferMetadata;
use crate::model::event::BufferId;
use crate::primitives::display_width::str_width;
use crate::state::EditorState;
use crate::view::ui::layout::point_in_rect;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Paragraph};
use ratatui::Frame;
use std::collections::HashMap;

/// Hit area for a single tab
#[derive(Debug, Clone)]
pub struct TabHitArea {
    /// The buffer ID this tab represents
    pub buffer_id: BufferId,
    /// The area covering the tab name (clickable to switch to buffer)
    pub tab_area: Rect,
    /// The area covering the close button
    pub close_area: Rect,
}

/// Layout information for hit testing tab interactions
///
/// Returned by `TabsRenderer::render_for_split()` to enable mouse hit testing
/// without duplicating position calculations.
#[derive(Debug, Clone, Default)]
pub struct TabLayout {
    /// Hit areas for each visible tab
    pub tabs: Vec<TabHitArea>,
    /// The full tab bar area
    pub bar_area: Rect,
    /// Hit area for the left scroll button (if shown)
    pub left_scroll_area: Option<Rect>,
    /// Hit area for the right scroll button (if shown)
    pub right_scroll_area: Option<Rect>,
}

/// Hit test result for tab interactions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TabHit {
    /// Hit the tab name area (click to switch buffer)
    TabName(BufferId),
    /// Hit the close button area
    CloseButton(BufferId),
    /// Hit the tab bar background
    BarBackground,
    /// Hit the left scroll button
    ScrollLeft,
    /// Hit the right scroll button
    ScrollRight,
}

impl TabLayout {
    /// Create a new empty layout
    pub fn new(bar_area: Rect) -> Self {
        Self {
            tabs: Vec::new(),
            bar_area,
            left_scroll_area: None,
            right_scroll_area: None,
        }
    }

    /// Perform a hit test to determine what element is at the given position
    pub fn hit_test(&self, x: u16, y: u16) -> Option<TabHit> {
        // Check scroll buttons first (they're at the edges)
        if let Some(left_area) = self.left_scroll_area {
            tracing::debug!(
                "Tab hit_test: checking left_scroll_area {:?} against ({}, {})",
                left_area,
                x,
                y
            );
            if point_in_rect(left_area, x, y) {
                tracing::debug!("Tab hit_test: HIT ScrollLeft");
                return Some(TabHit::ScrollLeft);
            }
        }
        if let Some(right_area) = self.right_scroll_area {
            tracing::debug!(
                "Tab hit_test: checking right_scroll_area {:?} against ({}, {})",
                right_area,
                x,
                y
            );
            if point_in_rect(right_area, x, y) {
                tracing::debug!("Tab hit_test: HIT ScrollRight");
                return Some(TabHit::ScrollRight);
            }
        }

        for tab in &self.tabs {
            // Check close button first (it's inside the tab area)
            if point_in_rect(tab.close_area, x, y) {
                return Some(TabHit::CloseButton(tab.buffer_id));
            }
            // Check tab area
            if point_in_rect(tab.tab_area, x, y) {
                return Some(TabHit::TabName(tab.buffer_id));
            }
        }

        // Check bar background
        if point_in_rect(self.bar_area, x, y) {
            return Some(TabHit::BarBackground);
        }

        None
    }
}

/// Renders the tab bar showing open buffers
pub struct TabsRenderer;

/// Compute scroll offset to bring the active tab into view.
/// Always scrolls to put the active tab at a comfortable position.
/// `tab_widths` includes separators between tabs.
pub fn scroll_to_show_tab(
    tab_widths: &[usize],
    active_idx: usize,
    _current_offset: usize,
    max_width: usize,
) -> usize {
    if tab_widths.is_empty() || max_width == 0 || active_idx >= tab_widths.len() {
        return 0;
    }

    let total_width: usize = tab_widths.iter().sum();
    let tab_start: usize = tab_widths[..active_idx].iter().sum();
    let tab_width = tab_widths[active_idx];
    let tab_end = tab_start + tab_width;

    // Try to put the active tab about 1/4 from the left edge
    let preferred_position = max_width / 4;
    let target_offset = tab_start.saturating_sub(preferred_position);

    // Clamp to valid range (0 to max_offset)
    let max_offset = total_width.saturating_sub(max_width);
    let mut result = target_offset.min(max_offset);

    // But ensure the tab is fully visible - if clamping pushed the tab off screen,
    // adjust to show at least the tab
    if tab_end > result + max_width {
        // Tab is past right edge, scroll right to show it
        result = tab_end.saturating_sub(max_width);
    }

    tracing::debug!(
        "scroll_to_show_tab: idx={}, tab={}..{}, target={}, result={}, total={}, max_width={}, max_offset={}",
        active_idx, tab_start, tab_end, target_offset, result, total_width, max_width, max_offset
    );

    result
}

/// Calculate tab widths for scroll offset calculations.
/// Returns (tab_widths, rendered_buffer_ids) where tab_widths includes separators.
/// This uses the same logic as render_for_split to ensure consistency.
pub fn calculate_tab_widths(
    split_buffers: &[BufferId],
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
) -> (Vec<usize>, Vec<BufferId>) {
    let mut tab_widths: Vec<usize> = Vec::new();
    let mut rendered_buffer_ids: Vec<BufferId> = Vec::new();

    for id in split_buffers.iter() {
        // Check if this is a regular buffer or a composite buffer
        let is_regular_buffer = buffers.contains_key(id);
        let is_composite_buffer = composite_buffers.contains_key(id);

        if !is_regular_buffer && !is_composite_buffer {
            continue;
        }

        // Skip buffers that are marked as hidden from tabs
        if let Some(meta) = buffer_metadata.get(id) {
            if meta.hidden_from_tabs {
                continue;
            }
        }

        let meta = buffer_metadata.get(id);
        let is_terminal = meta
            .and_then(|m| m.virtual_mode())
            .map(|mode| mode == "terminal")
            .unwrap_or(false);

        // Use same name resolution logic as render_for_split
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

        // Calculate modified indicator
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

        // Same format as render_for_split: " {name}{modified}{binary_indicator} " + "× "
        let tab_name_text = format!(" {name}{modified}{binary_indicator} ");
        let close_text = "× ";
        let tab_width = str_width(&tab_name_text) + str_width(close_text);

        // Add separator if not first tab
        if !rendered_buffer_ids.is_empty() {
            tab_widths.push(1); // separator
        }

        tab_widths.push(tab_width);
        rendered_buffer_ids.push(*id);
    }

    (tab_widths, rendered_buffer_ids)
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
    /// `TabLayout` containing hit areas for mouse interaction.
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
    ) -> TabLayout {
        let mut layout = TabLayout::new(area);
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

        // Use the scroll offset directly - ensure_active_tab_visible handles the calculation
        // Only clamp to prevent negative or extreme values
        let max_offset = total_width.saturating_sub(max_width);
        let offset = tab_scroll_offset.min(total_width);
        tracing::debug!(
            "render_for_split: tab_scroll_offset={}, max_offset={}, offset={}, total={}, max_width={}",
            tab_scroll_offset, max_offset, offset, total_width, max_width
        );

        // Indicators reserve space based on scroll position
        let show_left = offset > 0;
        let show_right = total_width.saturating_sub(offset) > max_width;
        let available = max_width
            .saturating_sub((show_left as usize + show_right as usize) * SCROLL_INDICATOR_WIDTH);

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

        // Track where the right indicator will be rendered (before adding it)
        let right_indicator_x = if show_right && rendered_width < max_width {
            Some(area.x + rendered_width as u16)
        } else {
            None
        };

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

        // Set scroll button areas if shown
        if show_left {
            layout.left_scroll_area =
                Some(Rect::new(area.x, area.y, SCROLL_INDICATOR_WIDTH as u16, 1));
        }
        if let Some(right_x) = right_indicator_x {
            // Right scroll button is at the position where it was actually rendered
            layout.right_scroll_area =
                Some(Rect::new(right_x, area.y, SCROLL_INDICATOR_WIDTH as u16, 1));
        }

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

            // Build tab hit area using Rects
            let tab_width = screen_end.saturating_sub(screen_start);
            let close_width = screen_end.saturating_sub(screen_close_start);

            layout.tabs.push(TabHitArea {
                buffer_id: *buffer_id,
                tab_area: Rect::new(screen_start, area.y, tab_width, 1),
                close_area: Rect::new(screen_close_start, area.y, close_width, 1),
            });
        }

        layout
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::BufferId;

    #[test]
    fn scroll_to_show_active_first_tab() {
        // Active is first tab, should scroll left to show it
        let widths = vec![5, 5, 5];
        let offset = scroll_to_show_tab(&widths, 0, 10, 20);
        // First tab starts at 0, should scroll to show it
        assert_eq!(offset, 0);
    }

    #[test]
    fn scroll_to_show_tab_already_visible() {
        // Tab is already visible, offset should stay the same
        let widths = vec![5, 5, 5];
        let offset = scroll_to_show_tab(&widths, 1, 0, 20);
        // Tab 1 starts at 5, ends at 10, visible in 0..20
        assert_eq!(offset, 0);
    }

    #[test]
    fn scroll_to_show_tab_on_right() {
        // Tab is to the right, need to scroll right
        let widths = vec![10, 10, 10];
        let offset = scroll_to_show_tab(&widths, 2, 0, 15);
        // Tab 2 starts at 20, ends at 30; need to scroll to show it
        assert!(offset > 0);
    }

    #[test]
    fn test_tab_layout_hit_test() {
        let bar_area = Rect::new(0, 0, 80, 1);
        let mut layout = TabLayout::new(bar_area);

        let buf1 = BufferId(1);

        layout.tabs.push(TabHitArea {
            buffer_id: buf1,
            tab_area: Rect::new(0, 0, 16, 1),
            close_area: Rect::new(12, 0, 4, 1),
        });

        // Hit tab name
        assert_eq!(layout.hit_test(5, 0), Some(TabHit::TabName(buf1)));

        // Hit close button
        assert_eq!(layout.hit_test(13, 0), Some(TabHit::CloseButton(buf1)));

        // Hit bar background
        assert_eq!(layout.hit_test(50, 0), Some(TabHit::BarBackground));

        // Outside everything
        assert_eq!(layout.hit_test(50, 5), None);
    }
}
