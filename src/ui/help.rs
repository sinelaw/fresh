//! Help page rendering and management

use crate::keybindings::KeybindingResolver;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph};
use ratatui::Frame;

/// Manages help page state and rendering
pub struct HelpRenderer {
    /// Current scroll offset
    scroll: usize,
    /// Is help visible?
    visible: bool,
}

impl HelpRenderer {
    /// Create a new help renderer
    pub fn new() -> Self {
        Self {
            scroll: 0,
            visible: false,
        }
    }

    /// Toggle help page visibility
    pub fn toggle(&mut self) {
        self.visible = !self.visible;
        self.scroll = 0; // Reset scroll when toggling
    }

    /// Check if help page is visible
    pub fn is_visible(&self) -> bool {
        self.visible
    }

    /// Scroll the help page
    ///
    /// # Arguments
    /// * `delta` - The amount to scroll (positive = down, negative = up)
    /// * `keybindings` - The keybinding resolver to get total binding count
    pub fn scroll(&mut self, delta: isize, keybindings: &KeybindingResolver) {
        let bindings = keybindings.get_all_bindings();
        let max_scroll = bindings.len().saturating_sub(1);

        if delta > 0 {
            self.scroll = (self.scroll + delta as usize).min(max_scroll);
        } else {
            self.scroll = self.scroll.saturating_sub(delta.unsigned_abs());
        }
    }

    /// Render the help page
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render in
    /// * `keybindings` - The keybinding resolver to get bindings from
    /// * `theme` - The active theme for colors
    pub fn render(&self, frame: &mut Frame, area: Rect, keybindings: &KeybindingResolver, theme: &crate::theme::Theme) {
        // Get all keybindings
        let bindings = keybindings.get_all_bindings();

        // Calculate visible range based on scroll
        let visible_height = area.height.saturating_sub(4) as usize; // Leave space for header and footer
        let start_idx = self.scroll;
        let end_idx = (start_idx + visible_height).min(bindings.len());

        // Build help text
        let mut lines = vec![];

        // Header
        lines.push(Line::from(vec![Span::styled(
            " KEYBOARD SHORTCUTS ",
            Style::default()
                .fg(theme.prompt_fg)
                .bg(theme.prompt_bg)
                .add_modifier(Modifier::BOLD),
        )]));
        lines.push(Line::from(""));

        // Find max key width for alignment
        let max_key_width = bindings
            .iter()
            .map(|(key, _)| key.len())
            .max()
            .unwrap_or(20);

        // Render visible bindings
        for (key, action) in bindings.iter().skip(start_idx).take(end_idx - start_idx) {
            let line_text = format!("  {key:<max_key_width$}  {action}");
            lines.push(Line::from(line_text));
        }

        // Footer
        lines.push(Line::from(""));
        lines.push(Line::from(vec![Span::styled(
            format!(
                " Showing {}-{} of {} | Use Up/Down to scroll | Press Ctrl+H or Esc to close ",
                start_idx + 1,
                end_idx,
                bindings.len()
            ),
            Style::default().fg(theme.status_bar_fg).bg(theme.status_bar_bg),
        )]));

        let help = Paragraph::new(lines)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(theme.help_separator_fg))
                    .title(" Help ")
                    .title_style(
                        Style::default()
                            .fg(theme.help_key_fg)
                            .add_modifier(Modifier::BOLD),
                    ),
            )
            .wrap(ratatui::widgets::Wrap { trim: true });

        frame.render_widget(help, area);
    }
}

impl Default for HelpRenderer {
    fn default() -> Self {
        Self::new()
    }
}
