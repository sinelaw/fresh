//! Tab bar rendering for multiple buffers

use crate::editor::BufferMetadata;
use crate::event::BufferId;
use crate::state::EditorState;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Paragraph};
use ratatui::Frame;
use std::collections::HashMap;

/// Renders the tab bar showing open buffers
pub struct TabsRenderer;

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
    pub fn render_for_split(
        frame: &mut Frame,
        area: Rect,
        split_buffers: &[BufferId],
        buffers: &HashMap<BufferId, EditorState>,
        buffer_metadata: &HashMap<BufferId, BufferMetadata>,
        active_buffer: BufferId,
        theme: &crate::theme::Theme,
        is_active_split: bool,
    ) {
        // Build spans for each tab with individual background colors
        let mut spans = Vec::new();

        for (idx, id) in split_buffers.iter().enumerate() {
            // Skip if buffer doesn't exist (shouldn't happen but be safe)
            let Some(state) = buffers.get(id) else {
                continue;
            };

            // Use display_name from metadata if available, otherwise fall back to file path
            let name = if let Some(metadata) = buffer_metadata.get(id) {
                metadata.display_name.as_str()
            } else {
                state
                    .buffer
                    .file_path()
                    .and_then(|p| p.file_name())
                    .and_then(|n| n.to_str())
                    .unwrap_or("[No Name]")
            };

            let modified = if state.buffer.is_modified() { "*" } else { "" };
            let tab_text = format!(" {name}{modified} ");

            let is_active = *id == active_buffer;

            // Active tab: theme colors with bold (only fully highlighted if split is active)
            // Inactive tabs: theme colors
            let style = if is_active {
                if is_active_split {
                    Style::default()
                        .fg(theme.tab_active_fg)
                        .bg(theme.tab_active_bg)
                        .add_modifier(Modifier::BOLD)
                } else {
                    // Active tab in inactive split - slightly dimmed
                    Style::default()
                        .fg(theme.tab_active_fg)
                        .bg(theme.tab_inactive_bg)
                        .add_modifier(Modifier::BOLD)
                }
            } else {
                Style::default()
                    .fg(theme.tab_inactive_fg)
                    .bg(theme.tab_inactive_bg)
            };

            spans.push(Span::styled(tab_text, style));

            // Add a small separator between tabs
            if idx < split_buffers.len() - 1 {
                spans.push(Span::styled(" ", Style::default().bg(theme.tab_separator_bg)));
            }
        }

        let line = Line::from(spans);
        let block = Block::default().style(Style::default().bg(theme.tab_separator_bg));
        let paragraph = Paragraph::new(line).block(block);
        frame.render_widget(paragraph, area);
    }

    /// Legacy render function for backward compatibility
    /// Renders all buffers as tabs (used during transition)
    #[allow(dead_code)]
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        buffers: &HashMap<BufferId, EditorState>,
        buffer_metadata: &HashMap<BufferId, BufferMetadata>,
        active_buffer: BufferId,
        theme: &crate::theme::Theme,
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
            active_buffer,
            theme,
            true, // Legacy behavior: always treat as active
        );
    }
}
