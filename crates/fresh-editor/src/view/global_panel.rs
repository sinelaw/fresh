use ratatui::{
    layout::Rect,
    style::{Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, Paragraph},
    Frame,
};

use fresh_core::api::GlobalPanelRow;

/// A bottom-anchored, full-width overlay panel with no interactive model.
/// All key handling is done externally via `defineMode`; the panel is display-only.
pub struct GlobalPanel {
    pub id: String,
    pub rows: Vec<GlobalPanelRow>,
}

impl GlobalPanel {
    pub fn new(id: String, rows: Vec<GlobalPanelRow>) -> Self {
        Self { id, rows }
    }

    /// Height of the panel including the border (1 top + 1 bottom).
    pub fn total_height(&self) -> u16 {
        (self.rows.len() as u16).saturating_add(2)
    }

    /// Compute the area for this panel given the full frame area and the
    /// height of the status bar row(s) sitting below it.
    pub fn calculate_area(&self, frame_area: Rect, status_bar_height: u16) -> Rect {
        let height = self.total_height().min(frame_area.height);
        let y = frame_area
            .height
            .saturating_sub(height)
            .saturating_sub(status_bar_height);
        Rect {
            x: 0,
            y,
            width: frame_area.width,
            height,
        }
    }

    pub fn render(&self, frame: &mut Frame, area: Rect, theme: &crate::view::theme::Theme) {
        // Clamp to frame bounds to avoid ratatui panic on out-of-range rects.
        let frame_area = frame.area();
        let area = Rect {
            x: area.x.min(frame_area.width.saturating_sub(1)),
            y: area.y.min(frame_area.height.saturating_sub(1)),
            width: area.width.min(frame_area.width.saturating_sub(area.x)),
            height: area.height.min(frame_area.height.saturating_sub(area.y)),
        };
        if area.width == 0 || area.height == 0 {
            return;
        }

        frame.render_widget(Clear, area);

        let border_style = Style::default().fg(theme.popup_border_fg);
        let bg_style = Style::default().bg(theme.popup_bg);

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(border_style)
            .style(bg_style);
        frame.render_widget(block, area);

        // Inner area (inside the border).
        let inner = Rect {
            x: area.x + 1,
            y: area.y + 1,
            width: area.width.saturating_sub(2),
            height: area.height.saturating_sub(2),
        };
        if inner.width == 0 || inner.height == 0 {
            return;
        }

        let title_style = Style::default()
            .fg(theme.popup_border_fg)
            .add_modifier(Modifier::BOLD);
        let header_style = Style::default()
            .fg(theme.help_separator_fg)
            .add_modifier(Modifier::BOLD);
        let normal_style = Style::default().fg(theme.popup_text_fg);
        let hint_style = Style::default().fg(theme.help_separator_fg);

        let lines: Vec<Line> = self
            .rows
            .iter()
            .take(inner.height as usize)
            .map(|row| {
                let style = match row.style.as_deref() {
                    Some("title") => title_style,
                    Some("group-header") => header_style,
                    Some("hint") => hint_style,
                    _ => normal_style,
                };
                Line::from(Span::styled(row.text.clone(), style))
            })
            .collect();

        frame.render_widget(Paragraph::new(lines), inner);
    }
}

pub struct GlobalPanelManager {
    panels: Vec<GlobalPanel>,
}

impl GlobalPanelManager {
    pub fn new() -> Self {
        Self { panels: Vec::new() }
    }

    /// Insert or replace a panel with the given id.
    pub fn show(&mut self, panel: GlobalPanel) {
        if let Some(pos) = self.panels.iter().position(|p| p.id == panel.id) {
            self.panels[pos] = panel;
        } else {
            self.panels.push(panel);
        }
    }

    /// Update rows of the panel with the given id (no-op if not found).
    pub fn update(&mut self, id: &str, rows: Vec<GlobalPanelRow>) {
        if let Some(panel) = self.panels.iter_mut().find(|p| p.id == id) {
            panel.rows = rows;
        }
    }

    /// Remove the panel with the given id.
    pub fn close(&mut self, id: &str) {
        self.panels.retain(|p| p.id != id);
    }

    /// The topmost (last added) panel, used for rendering.
    pub fn top(&self) -> Option<&GlobalPanel> {
        self.panels.last()
    }

    pub fn is_visible(&self) -> bool {
        !self.panels.is_empty()
    }
}

impl Default for GlobalPanelManager {
    fn default() -> Self {
        Self::new()
    }
}
