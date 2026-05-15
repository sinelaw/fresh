use ratatui::{
    layout::Rect,
    style::{Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, Paragraph},
    Frame,
};

use fresh_core::api::{GlobalPanelAnchor, GlobalPanelRow};

/// A bottom-anchored, full-width overlay panel with no interactive model.
/// All key handling is done externally via `defineMode`; the panel is display-only.
pub struct GlobalPanel {
    pub id: String,
    pub rows: Vec<GlobalPanelRow>,
    /// Which screen edge this panel is anchored to.
    pub anchor: GlobalPanelAnchor,
    /// Maximum width cap in terminal columns (left/right anchors).
    pub max_width: Option<u16>,
    /// Maximum height cap in terminal rows (top/bottom anchors).
    pub max_height: Option<u16>,
}

impl GlobalPanel {
    pub fn new(
        id: String,
        rows: Vec<GlobalPanelRow>,
        anchor: GlobalPanelAnchor,
        max_width: Option<u16>,
        max_height: Option<u16>,
    ) -> Self {
        Self { id, rows, anchor, max_width, max_height }
    }

    /// Height of the panel including the border (1 top + 1 bottom), capped by max_height.
    pub fn total_height(&self) -> u16 {
        let natural = (self.rows.len() as u16).saturating_add(2);
        match self.max_height {
            Some(cap) => natural.min(cap),
            None => natural,
        }
    }

    /// Width of the panel including the border (1 left + 1 right), capped by max_width.
    /// Used for left/right anchors.
    pub fn total_width(&self) -> u16 {
        let natural = self
            .rows
            .iter()
            .map(|r| r.text.chars().count() as u16)
            .max()
            .unwrap_or(0)
            .saturating_add(2);
        match self.max_width {
            Some(cap) => natural.min(cap),
            None => natural,
        }
    }

    /// Compute the area for this panel given the full frame area and the
    /// heights/widths of the chrome rows/columns adjacent to each edge.
    ///
    /// `top_reserved`    — rows consumed by menu bar + tab bar (for `Top` anchor)
    /// `bottom_reserved` — rows consumed by status bar (for `Bottom` anchor)
    /// `content_y`       — first content row (same as `top_reserved`)
    /// `content_height`  — usable content rows (for `Left`/`Right` anchors)
    pub fn calculate_area(
        &self,
        frame_area: Rect,
        top_reserved: u16,
        bottom_reserved: u16,
    ) -> Rect {
        let content_y = top_reserved;
        let content_height = frame_area
            .height
            .saturating_sub(top_reserved)
            .saturating_sub(bottom_reserved);

        match self.anchor {
            GlobalPanelAnchor::Bottom => {
                let height = self.total_height().min(frame_area.height);
                let y = frame_area
                    .height
                    .saturating_sub(height)
                    .saturating_sub(bottom_reserved);
                Rect {
                    x: 0,
                    y,
                    width: frame_area.width,
                    height,
                }
            }
            GlobalPanelAnchor::Top => {
                let height = self.total_height().min(frame_area.height);
                Rect {
                    x: 0,
                    y: content_y,
                    width: frame_area.width,
                    height,
                }
            }
            GlobalPanelAnchor::Left => {
                let width = self.total_width().min(frame_area.width);
                let height = match self.max_height {
                    Some(cap) => content_height.min(cap),
                    None => content_height,
                };
                Rect {
                    x: 0,
                    y: content_y,
                    width,
                    height,
                }
            }
            GlobalPanelAnchor::Right => {
                let width = self.total_width().min(frame_area.width);
                let height = match self.max_height {
                    Some(cap) => content_height.min(cap),
                    None => content_height,
                };
                Rect {
                    x: frame_area.width.saturating_sub(width),
                    y: content_y,
                    width,
                    height,
                }
            }
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
