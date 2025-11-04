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
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        buffers: &HashMap<BufferId, EditorState>,
        active_buffer: BufferId,
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

            // Active tab: bright yellow text on blue background with bold
            // Inactive tabs: white text on dark gray background
            let style = if is_active {
                Style::default()
                    .fg(Color::Yellow)
                    .bg(Color::Blue)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
                    .fg(Color::White)
                    .bg(Color::DarkGray)
            };

            spans.push(Span::styled(tab_text, style));

            // Add a small separator between tabs (single space with no background)
            if idx < buffers.len() - 1 {
                spans.push(Span::raw(" "));
            }
        }

        let line = Line::from(spans);
        let paragraph = Paragraph::new(line).style(Style::default().bg(Color::Black));
        frame.render_widget(paragraph, area);
    }
}
