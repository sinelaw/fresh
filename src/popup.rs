use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph},
    Frame,
};

/// Position of a popup relative to a point in the buffer
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PopupPosition {
    /// At cursor position
    AtCursor,
    /// Below cursor position
    BelowCursor,
    /// Above cursor position
    AboveCursor,
    /// Fixed screen coordinates (x, y)
    Fixed { x: u16, y: u16 },
    /// Centered on screen
    Centered,
}

/// Content of a popup window
#[derive(Debug, Clone, PartialEq)]
pub enum PopupContent {
    /// Simple text content
    Text(Vec<String>),
    /// List of selectable items
    List {
        items: Vec<PopupListItem>,
        selected: usize,
    },
    /// Custom rendered content (just store strings for now)
    Custom(Vec<String>),
}

/// A single item in a popup list
#[derive(Debug, Clone, PartialEq)]
pub struct PopupListItem {
    /// Main text to display
    pub text: String,
    /// Optional secondary text (description, type info, etc.)
    pub detail: Option<String>,
    /// Optional icon or prefix
    pub icon: Option<String>,
    /// User data associated with this item (for completion, etc.)
    pub data: Option<String>,
}

impl PopupListItem {
    pub fn new(text: String) -> Self {
        Self {
            text,
            detail: None,
            icon: None,
            data: None,
        }
    }

    pub fn with_detail(mut self, detail: String) -> Self {
        self.detail = Some(detail);
        self
    }

    pub fn with_icon(mut self, icon: String) -> Self {
        self.icon = Some(icon);
        self
    }

    pub fn with_data(mut self, data: String) -> Self {
        self.data = Some(data);
        self
    }
}

/// A popup/floating window
/// This is a general-purpose UI primitive that can be used for:
/// - Completion menus
/// - Hover documentation
/// - Command palette
/// - File picker
/// - Diagnostic messages
/// - Quick fixes / code actions
#[derive(Debug, Clone, PartialEq)]
pub struct Popup {
    /// Title of the popup (optional)
    pub title: Option<String>,

    /// Content to display
    pub content: PopupContent,

    /// Position strategy
    pub position: PopupPosition,

    /// Width of popup (in columns)
    pub width: u16,

    /// Maximum height (will be clamped to available space)
    pub max_height: u16,

    /// Whether to show borders
    pub bordered: bool,

    /// Border style
    pub border_style: Style,

    /// Background style
    pub background_style: Style,

    /// Scroll offset for content (for scrolling through long lists)
    pub scroll_offset: usize,
}

impl Popup {
    /// Create a new popup with text content using theme colors
    pub fn text(content: Vec<String>, theme: &crate::theme::Theme) -> Self {
        Self {
            title: None,
            content: PopupContent::Text(content),
            position: PopupPosition::AtCursor,
            width: 50,
            max_height: 15,
            bordered: true,
            border_style: Style::default().fg(theme.popup_border_fg),
            background_style: Style::default().bg(theme.popup_bg),
            scroll_offset: 0,
        }
    }

    /// Create a new popup with a list of items using theme colors
    pub fn list(items: Vec<PopupListItem>, theme: &crate::theme::Theme) -> Self {
        Self {
            title: None,
            content: PopupContent::List { items, selected: 0 },
            position: PopupPosition::AtCursor,
            width: 50,
            max_height: 15,
            bordered: true,
            border_style: Style::default().fg(theme.popup_border_fg),
            background_style: Style::default().bg(theme.popup_bg),
            scroll_offset: 0,
        }
    }

    /// Set the title
    pub fn with_title(mut self, title: String) -> Self {
        self.title = Some(title);
        self
    }

    /// Set the position
    pub fn with_position(mut self, position: PopupPosition) -> Self {
        self.position = position;
        self
    }

    /// Set the width
    pub fn with_width(mut self, width: u16) -> Self {
        self.width = width;
        self
    }

    /// Set the max height
    pub fn with_max_height(mut self, max_height: u16) -> Self {
        self.max_height = max_height;
        self
    }

    /// Set border style
    pub fn with_border_style(mut self, style: Style) -> Self {
        self.border_style = style;
        self
    }

    /// Get the currently selected item (if this is a list popup)
    pub fn selected_item(&self) -> Option<&PopupListItem> {
        match &self.content {
            PopupContent::List { items, selected } => items.get(*selected),
            _ => None,
        }
    }

    /// Move selection down (for list popups)
    pub fn select_next(&mut self) {
        if let PopupContent::List { items, selected } = &mut self.content {
            if *selected < items.len().saturating_sub(1) {
                *selected += 1;
                // Adjust scroll if needed
                if *selected >= self.scroll_offset + self.max_height as usize {
                    self.scroll_offset = (*selected + 1).saturating_sub(self.max_height as usize);
                }
            }
        }
    }

    /// Move selection up (for list popups)
    pub fn select_prev(&mut self) {
        if let PopupContent::List { items, selected } = &mut self.content {
            if *selected > 0 {
                *selected -= 1;
                // Adjust scroll if needed
                if *selected < self.scroll_offset {
                    self.scroll_offset = *selected;
                }
            }
        }
    }

    /// Scroll down by one page
    pub fn page_down(&mut self) {
        if let PopupContent::List { items, selected } = &mut self.content {
            let page_size = self.max_height as usize;
            *selected = (*selected + page_size).min(items.len().saturating_sub(1));
            self.scroll_offset = (*selected + 1).saturating_sub(page_size);
        } else {
            self.scroll_offset += self.max_height as usize;
        }
    }

    /// Scroll up by one page
    pub fn page_up(&mut self) {
        if let PopupContent::List { items, selected } = &mut self.content {
            let page_size = self.max_height as usize;
            *selected = selected.saturating_sub(page_size);
            self.scroll_offset = *selected;
        } else {
            self.scroll_offset = self.scroll_offset.saturating_sub(self.max_height as usize);
        }
    }

    /// Calculate the actual content height based on the popup content
    fn content_height(&self) -> u16 {
        let content_lines = match &self.content {
            PopupContent::Text(lines) => lines.len() as u16,
            PopupContent::List { items, .. } => items.len() as u16,
            PopupContent::Custom(lines) => lines.len() as u16,
        };

        // Add border lines if bordered
        let border_height = if self.bordered { 2 } else { 0 };

        content_lines + border_height
    }

    /// Calculate the area where this popup should be rendered
    pub fn calculate_area(&self, terminal_area: Rect, cursor_pos: Option<(u16, u16)>) -> Rect {
        match self.position {
            PopupPosition::AtCursor | PopupPosition::BelowCursor | PopupPosition::AboveCursor => {
                let (cursor_x, cursor_y) = cursor_pos.unwrap_or((terminal_area.width / 2, terminal_area.height / 2));

                let width = self.width.min(terminal_area.width);
                // Use the minimum of max_height, actual content height, and terminal height
                let height = self.content_height().min(self.max_height).min(terminal_area.height);

                let x = if cursor_x + width > terminal_area.width {
                    terminal_area.width.saturating_sub(width)
                } else {
                    cursor_x
                };

                let y = match self.position {
                    PopupPosition::AtCursor => cursor_y,
                    PopupPosition::BelowCursor => {
                        if cursor_y + 1 + height > terminal_area.height {
                            // Not enough space below, put above
                            cursor_y.saturating_sub(height)
                        } else {
                            cursor_y + 1
                        }
                    }
                    PopupPosition::AboveCursor => {
                        cursor_y.saturating_sub(height)
                    }
                    _ => cursor_y,
                };

                Rect {
                    x,
                    y,
                    width,
                    height,
                }
            }
            PopupPosition::Fixed { x, y } => {
                let width = self.width.min(terminal_area.width);
                let height = self.content_height().min(self.max_height).min(terminal_area.height);
                Rect { x, y, width, height }
            }
            PopupPosition::Centered => {
                let width = self.width.min(terminal_area.width);
                let height = self.content_height().min(self.max_height).min(terminal_area.height);
                let x = (terminal_area.width.saturating_sub(width)) / 2;
                let y = (terminal_area.height.saturating_sub(height)) / 2;
                Rect { x, y, width, height }
            }
        }
    }

    /// Render the popup to the frame
    pub fn render(&self, frame: &mut Frame, area: Rect, theme: &crate::theme::Theme) {
        let block = if self.bordered {
            let mut block = Block::default()
                .borders(Borders::ALL)
                .border_style(self.border_style)
                .style(self.background_style);

            if let Some(title) = &self.title {
                block = block.title(title.as_str());
            }

            block
        } else {
            Block::default().style(self.background_style)
        };

        let inner_area = block.inner(area);
        frame.render_widget(block, area);

        match &self.content {
            PopupContent::Text(lines) => {
                let visible_lines: Vec<Line> = lines
                    .iter()
                    .skip(self.scroll_offset)
                    .take(inner_area.height as usize)
                    .map(|line| Line::from(line.as_str()))
                    .collect();

                let paragraph = Paragraph::new(visible_lines);
                frame.render_widget(paragraph, inner_area);
            }
            PopupContent::List { items, selected } => {
                let list_items: Vec<ListItem> = items
                    .iter()
                    .enumerate()
                    .skip(self.scroll_offset)
                    .take(inner_area.height as usize)
                    .map(|(idx, item)| {
                        let mut spans = Vec::new();

                        // Add icon if present
                        if let Some(icon) = &item.icon {
                            spans.push(Span::raw(format!("{} ", icon)));
                        }

                        // Add main text
                        spans.push(Span::raw(&item.text));

                        // Add detail if present
                        if let Some(detail) = &item.detail {
                            spans.push(Span::styled(
                                format!(" {}", detail),
                                Style::default().fg(theme.help_separator_fg),
                            ));
                        }

                        let style = if idx == *selected {
                            Style::default()
                                .bg(theme.popup_selection_bg)
                                .add_modifier(Modifier::BOLD)
                        } else {
                            Style::default()
                        };

                        ListItem::new(Line::from(spans)).style(style)
                    })
                    .collect();

                let list = List::new(list_items);
                frame.render_widget(list, inner_area);
            }
            PopupContent::Custom(lines) => {
                let visible_lines: Vec<Line> = lines
                    .iter()
                    .skip(self.scroll_offset)
                    .take(inner_area.height as usize)
                    .map(|line| Line::from(line.as_str()))
                    .collect();

                let paragraph = Paragraph::new(visible_lines);
                frame.render_widget(paragraph, inner_area);
            }
        }
    }
}

/// Manager for popups - can show multiple popups with z-ordering
#[derive(Debug, Clone)]
pub struct PopupManager {
    /// Stack of active popups (top of stack = topmost popup)
    popups: Vec<Popup>,
}

impl PopupManager {
    pub fn new() -> Self {
        Self {
            popups: Vec::new(),
        }
    }

    /// Show a popup (adds to top of stack)
    pub fn show(&mut self, popup: Popup) {
        self.popups.push(popup);
    }

    /// Hide the topmost popup
    pub fn hide(&mut self) -> Option<Popup> {
        self.popups.pop()
    }

    /// Clear all popups
    pub fn clear(&mut self) {
        self.popups.clear();
    }

    /// Get the topmost popup
    pub fn top(&self) -> Option<&Popup> {
        self.popups.last()
    }

    /// Get mutable reference to topmost popup
    pub fn top_mut(&mut self) -> Option<&mut Popup> {
        self.popups.last_mut()
    }

    /// Check if any popups are visible
    pub fn is_visible(&self) -> bool {
        !self.popups.is_empty()
    }

    /// Get all popups (for rendering)
    pub fn all(&self) -> &[Popup] {
        &self.popups
    }
}

impl Default for PopupManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_popup_list_item() {
        let item = PopupListItem::new("test".to_string())
            .with_detail("detail".to_string())
            .with_icon("📄".to_string());

        assert_eq!(item.text, "test");
        assert_eq!(item.detail, Some("detail".to_string()));
        assert_eq!(item.icon, Some("📄".to_string()));
    }

    #[test]
    fn test_popup_selection() {
        let theme = crate::theme::Theme::dark();
        let items = vec![
            PopupListItem::new("item1".to_string()),
            PopupListItem::new("item2".to_string()),
            PopupListItem::new("item3".to_string()),
        ];

        let mut popup = Popup::list(items, &theme);

        assert_eq!(popup.selected_item().unwrap().text, "item1");

        popup.select_next();
        assert_eq!(popup.selected_item().unwrap().text, "item2");

        popup.select_next();
        assert_eq!(popup.selected_item().unwrap().text, "item3");

        popup.select_next(); // Should stay at last item
        assert_eq!(popup.selected_item().unwrap().text, "item3");

        popup.select_prev();
        assert_eq!(popup.selected_item().unwrap().text, "item2");

        popup.select_prev();
        assert_eq!(popup.selected_item().unwrap().text, "item1");

        popup.select_prev(); // Should stay at first item
        assert_eq!(popup.selected_item().unwrap().text, "item1");
    }

    #[test]
    fn test_popup_manager() {
        let theme = crate::theme::Theme::dark();
        let mut manager = PopupManager::new();

        assert!(!manager.is_visible());
        assert_eq!(manager.top(), None);

        let popup1 = Popup::text(vec!["test1".to_string()], &theme);
        manager.show(popup1);

        assert!(manager.is_visible());
        assert_eq!(manager.all().len(), 1);

        let popup2 = Popup::text(vec!["test2".to_string()], &theme);
        manager.show(popup2);

        assert_eq!(manager.all().len(), 2);

        manager.hide();
        assert_eq!(manager.all().len(), 1);

        manager.clear();
        assert!(!manager.is_visible());
        assert_eq!(manager.all().len(), 0);
    }

    #[test]
    fn test_popup_area_calculation() {
        let theme = crate::theme::Theme::dark();
        let terminal_area = Rect {
            x: 0,
            y: 0,
            width: 100,
            height: 50,
        };

        let popup = Popup::text(vec!["test".to_string()], &theme)
            .with_width(30)
            .with_max_height(10);

        // Centered
        let popup_centered = popup.clone().with_position(PopupPosition::Centered);
        let area = popup_centered.calculate_area(terminal_area, None);
        assert_eq!(area.width, 30);
        assert_eq!(area.height, 10);
        assert_eq!(area.x, (100 - 30) / 2);
        assert_eq!(area.y, (50 - 10) / 2);

        // Below cursor
        let popup_below = popup.clone().with_position(PopupPosition::BelowCursor);
        let area = popup_below.calculate_area(terminal_area, Some((20, 10)));
        assert_eq!(area.x, 20);
        assert_eq!(area.y, 11); // One row below cursor
    }
}
