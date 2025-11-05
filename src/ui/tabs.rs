//! Tab bar rendering for multiple buffers

use crate::event::BufferId;
use crate::state::EditorState;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;
use std::collections::HashMap;

/// Renders the tab bar showing all open buffers
pub struct TabsRenderer;

impl TabsRenderer {
    /// Render the tab bar showing all open buffers
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render the tabs in
    /// * `buffers` - All open buffers
    /// * `active_buffer` - The currently active buffer ID
    /// * `theme` - The active theme for colors
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        buffers: &HashMap<BufferId, EditorState>,
        active_buffer: BufferId,
        theme: &crate::theme::Theme,
    ) {
        // Build spans for each tab with individual background colors
        let mut spans = Vec::new();

        // Sort buffer IDs to ensure consistent tab order
        let mut buffer_ids: Vec<_> = buffers.keys().copied().collect();
        buffer_ids.sort_by_key(|id| id.0);

        for (idx, id) in buffer_ids.iter().enumerate() {
            let state = &buffers[id];
            let name = state
                .buffer
                .file_path()
                .and_then(|p| p.file_name())
                .and_then(|n| n.to_str())
                .unwrap_or("[No Name]");

            let modified = if state.buffer.is_modified() { "*" } else { "" };
            let tab_text = format!(" {name}{modified} ");

            let is_active = *id == active_buffer;

            // Active tab: theme colors with bold
            // Inactive tabs: theme colors
            let style = if is_active {
                Style::default()
                    .fg(theme.tab_active_fg)
                    .bg(theme.tab_active_bg)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
                    .fg(theme.tab_inactive_fg)
                    .bg(theme.tab_inactive_bg)
            };

            spans.push(Span::styled(tab_text, style));

            // Add a small separator between tabs (single space with no background)
            if idx < buffers.len() - 1 {
                spans.push(Span::raw(" "));
            }
        }

        let line = Line::from(spans);
        let paragraph = Paragraph::new(line).style(Style::default().bg(theme.tab_separator_bg));
        frame.render_widget(paragraph, area);
    }
}
