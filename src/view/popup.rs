use ratatui::{
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, List, ListItem, Paragraph},
    Frame,
};

use pulldown_cmark::{Event, Options, Parser, Tag, TagEnd};

use super::ui::scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};

/// Clamp a rectangle to fit within bounds, preventing out-of-bounds rendering panics.
/// Returns a rectangle that is guaranteed to be fully contained within `bounds`.
fn clamp_rect_to_bounds(rect: Rect, bounds: Rect) -> Rect {
    // Clamp x to be within bounds
    let x = rect.x.min(bounds.x + bounds.width.saturating_sub(1));
    // Clamp y to be within bounds
    let y = rect.y.min(bounds.y + bounds.height.saturating_sub(1));

    // Calculate maximum possible width/height from the clamped position
    let max_width = (bounds.x + bounds.width).saturating_sub(x);
    let max_height = (bounds.y + bounds.height).saturating_sub(y);

    Rect {
        x,
        y,
        width: rect.width.min(max_width),
        height: rect.height.min(max_height),
    }
}

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

/// A styled span for markdown rendering
#[derive(Debug, Clone, PartialEq)]
pub struct StyledSpan {
    pub text: String,
    pub style: Style,
}

/// A line of styled spans for markdown rendering
#[derive(Debug, Clone, PartialEq)]
pub struct StyledLine {
    pub spans: Vec<StyledSpan>,
}

impl StyledLine {
    pub fn new() -> Self {
        Self { spans: Vec::new() }
    }

    pub fn push(&mut self, text: String, style: Style) {
        self.spans.push(StyledSpan { text, style });
    }
}

impl Default for StyledLine {
    fn default() -> Self {
        Self::new()
    }
}

/// Content of a popup window
#[derive(Debug, Clone, PartialEq)]
pub enum PopupContent {
    /// Simple text content
    Text(Vec<String>),
    /// Markdown content with styling
    Markdown(Vec<StyledLine>),
    /// List of selectable items
    List {
        items: Vec<PopupListItem>,
        selected: usize,
    },
    /// Custom rendered content (just store strings for now)
    Custom(Vec<String>),
}

/// Parse markdown text into styled lines for terminal rendering
pub fn parse_markdown(text: &str, theme: &crate::view::theme::Theme) -> Vec<StyledLine> {
    let mut options = Options::empty();
    options.insert(Options::ENABLE_STRIKETHROUGH);

    let parser = Parser::new_ext(text, options);
    let mut lines: Vec<StyledLine> = vec![StyledLine::new()];

    // Style stack for nested formatting
    let mut style_stack: Vec<Style> = vec![Style::default()];
    let mut in_code_block = false;
    let mut code_block_lang = String::new();

    for event in parser {
        match event {
            Event::Start(tag) => {
                match tag {
                    Tag::Strong => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(current.add_modifier(Modifier::BOLD));
                    }
                    Tag::Emphasis => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(current.add_modifier(Modifier::ITALIC));
                    }
                    Tag::Strikethrough => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(current.add_modifier(Modifier::CROSSED_OUT));
                    }
                    Tag::CodeBlock(kind) => {
                        in_code_block = true;
                        code_block_lang = match kind {
                            pulldown_cmark::CodeBlockKind::Fenced(lang) => lang.to_string(),
                            pulldown_cmark::CodeBlockKind::Indented => String::new(),
                        };
                        // Start new line for code block
                        if !lines.last().map(|l| l.spans.is_empty()).unwrap_or(true) {
                            lines.push(StyledLine::new());
                        }
                    }
                    Tag::Heading { .. } => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack
                            .push(current.add_modifier(Modifier::BOLD).fg(theme.help_key_fg));
                    }
                    Tag::Link { .. } | Tag::Image { .. } => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack
                            .push(current.add_modifier(Modifier::UNDERLINED).fg(Color::Cyan));
                    }
                    Tag::List(_) | Tag::Item => {
                        // Start list items on new line
                        if !lines.last().map(|l| l.spans.is_empty()).unwrap_or(true) {
                            lines.push(StyledLine::new());
                        }
                    }
                    Tag::Paragraph => {
                        // Start paragraphs on new line if we have content
                        if !lines.last().map(|l| l.spans.is_empty()).unwrap_or(true) {
                            lines.push(StyledLine::new());
                        }
                    }
                    _ => {}
                }
            }
            Event::End(tag_end) => {
                match tag_end {
                    TagEnd::Strong
                    | TagEnd::Emphasis
                    | TagEnd::Strikethrough
                    | TagEnd::Heading(_)
                    | TagEnd::Link
                    | TagEnd::Image => {
                        style_stack.pop();
                    }
                    TagEnd::CodeBlock => {
                        in_code_block = false;
                        code_block_lang.clear();
                        // End code block with new line
                        lines.push(StyledLine::new());
                    }
                    TagEnd::Paragraph => {
                        // Add blank line after paragraph
                        lines.push(StyledLine::new());
                    }
                    TagEnd::Item => {
                        // Items end naturally
                    }
                    _ => {}
                }
            }
            Event::Text(text) => {
                let current_style = if in_code_block {
                    Style::default()
                        .fg(theme.help_key_fg)
                        .bg(theme.inline_code_bg)
                } else {
                    *style_stack.last().unwrap_or(&Style::default())
                };

                // Split text by newlines and add to lines
                for (i, part) in text.split('\n').enumerate() {
                    if i > 0 {
                        lines.push(StyledLine::new());
                    }
                    if !part.is_empty() {
                        if let Some(line) = lines.last_mut() {
                            line.push(part.to_string(), current_style);
                        }
                    }
                }
            }
            Event::Code(code) => {
                // Inline code
                let style = Style::default()
                    .fg(theme.help_key_fg)
                    .bg(theme.inline_code_bg);
                if let Some(line) = lines.last_mut() {
                    line.push(format!("`{}`", code), style);
                }
            }
            Event::SoftBreak => {
                // Soft break - add space
                if let Some(line) = lines.last_mut() {
                    line.push(" ".to_string(), Style::default());
                }
            }
            Event::HardBreak => {
                // Hard break - new line
                lines.push(StyledLine::new());
            }
            Event::Rule => {
                // Horizontal rule
                lines.push(StyledLine::new());
                if let Some(line) = lines.last_mut() {
                    line.push("─".repeat(40), Style::default().fg(Color::DarkGray));
                }
                lines.push(StyledLine::new());
            }
            _ => {}
        }
    }

    // Remove trailing empty lines
    while lines.last().map(|l| l.spans.is_empty()).unwrap_or(false) {
        lines.pop();
    }

    lines
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

    /// Whether this popup is transient (dismissed on focus loss, e.g. hover, signature help)
    pub transient: bool,

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
    pub fn text(content: Vec<String>, theme: &crate::view::theme::Theme) -> Self {
        Self {
            title: None,
            transient: false,
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

    /// Create a new popup with markdown content using theme colors
    pub fn markdown(markdown_text: &str, theme: &crate::view::theme::Theme) -> Self {
        let styled_lines = parse_markdown(markdown_text, theme);
        Self {
            title: None,
            transient: false,
            content: PopupContent::Markdown(styled_lines),
            position: PopupPosition::AtCursor,
            width: 60,      // Wider for markdown content
            max_height: 20, // Taller for documentation
            bordered: true,
            border_style: Style::default().fg(theme.popup_border_fg),
            background_style: Style::default().bg(theme.popup_bg),
            scroll_offset: 0,
        }
    }

    /// Create a new popup with a list of items using theme colors
    pub fn list(items: Vec<PopupListItem>, theme: &crate::view::theme::Theme) -> Self {
        Self {
            title: None,
            transient: false,
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

    /// Mark this popup as transient (will be dismissed on focus loss)
    pub fn with_transient(mut self, transient: bool) -> Self {
        self.transient = transient;
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
        if let PopupContent::List { items: _, selected } = &mut self.content {
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
        if let PopupContent::List { items: _, selected } = &mut self.content {
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
            PopupContent::Markdown(lines) => lines.len() as u16,
            PopupContent::List { items, .. } => items.len() as u16,
            PopupContent::Custom(lines) => lines.len() as u16,
        };

        // Add border lines if bordered
        let border_height = if self.bordered { 2 } else { 0 };

        content_lines + border_height
    }

    /// Get the number of content lines (without borders)
    pub fn content_line_count(&self) -> usize {
        match &self.content {
            PopupContent::Text(lines) => lines.len(),
            PopupContent::Markdown(lines) => lines.len(),
            PopupContent::List { items, .. } => items.len(),
            PopupContent::Custom(lines) => lines.len(),
        }
    }

    /// Check if content needs scrolling (has more lines than visible area)
    pub fn needs_scrollbar(&self, visible_height: u16) -> bool {
        self.content_line_count() > visible_height as usize
    }

    /// Calculate the area where this popup should be rendered
    pub fn calculate_area(&self, terminal_area: Rect, cursor_pos: Option<(u16, u16)>) -> Rect {
        match self.position {
            PopupPosition::AtCursor | PopupPosition::BelowCursor | PopupPosition::AboveCursor => {
                let (cursor_x, cursor_y) =
                    cursor_pos.unwrap_or((terminal_area.width / 2, terminal_area.height / 2));

                let width = self.width.min(terminal_area.width);
                // Use the minimum of max_height, actual content height, and terminal height
                let height = self
                    .content_height()
                    .min(self.max_height)
                    .min(terminal_area.height);

                let x = if cursor_x + width > terminal_area.width {
                    terminal_area.width.saturating_sub(width)
                } else {
                    cursor_x
                };

                let y = match self.position {
                    PopupPosition::AtCursor => cursor_y,
                    PopupPosition::BelowCursor => {
                        if cursor_y + 2 + height > terminal_area.height {
                            // Not enough space below, put above cursor
                            // Position so bottom of popup is one row above cursor
                            (cursor_y + 1).saturating_sub(height)
                        } else {
                            // Below cursor with two row gap
                            cursor_y + 2
                        }
                    }
                    PopupPosition::AboveCursor => {
                        // Position so bottom of popup is one row above cursor
                        (cursor_y + 1).saturating_sub(height)
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
                let height = self
                    .content_height()
                    .min(self.max_height)
                    .min(terminal_area.height);
                // Clamp x and y to ensure popup stays within terminal bounds
                let x = if x + width > terminal_area.width {
                    terminal_area.width.saturating_sub(width)
                } else {
                    x
                };
                let y = if y + height > terminal_area.height {
                    terminal_area.height.saturating_sub(height)
                } else {
                    y
                };
                Rect {
                    x,
                    y,
                    width,
                    height,
                }
            }
            PopupPosition::Centered => {
                let width = self.width.min(terminal_area.width);
                let height = self
                    .content_height()
                    .min(self.max_height)
                    .min(terminal_area.height);
                let x = (terminal_area.width.saturating_sub(width)) / 2;
                let y = (terminal_area.height.saturating_sub(height)) / 2;
                Rect {
                    x,
                    y,
                    width,
                    height,
                }
            }
        }
    }

    /// Render the popup to the frame
    pub fn render(&self, frame: &mut Frame, area: Rect, theme: &crate::view::theme::Theme) {
        self.render_with_hover(frame, area, theme, None);
    }

    /// Render the popup to the frame with hover highlighting
    pub fn render_with_hover(
        &self,
        frame: &mut Frame,
        area: Rect,
        theme: &crate::view::theme::Theme,
        hover_target: Option<&crate::app::HoverTarget>,
    ) {
        // Defensive bounds checking: clamp area to frame bounds to prevent panic
        let frame_area = frame.area();
        let area = clamp_rect_to_bounds(area, frame_area);

        // Skip rendering if area is empty after clamping
        if area.width == 0 || area.height == 0 {
            return;
        }

        // Clear the area behind the popup first to hide underlying text
        frame.render_widget(Clear, area);

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

        // Check if we need a scrollbar (content exceeds visible area)
        let total_lines = self.content_line_count();
        let visible_lines_count = inner_area.height as usize;
        let needs_scrollbar = total_lines > visible_lines_count && inner_area.width > 2;

        // Adjust content area to leave room for scrollbar if needed
        let content_area = if needs_scrollbar {
            Rect {
                x: inner_area.x,
                y: inner_area.y,
                width: inner_area.width.saturating_sub(2), // 1 for scrollbar + 1 for spacing
                height: inner_area.height,
            }
        } else {
            inner_area
        };

        match &self.content {
            PopupContent::Text(lines) => {
                let visible_lines: Vec<Line> = lines
                    .iter()
                    .skip(self.scroll_offset)
                    .take(content_area.height as usize)
                    .map(|line| Line::from(line.as_str()))
                    .collect();

                let paragraph = Paragraph::new(visible_lines);
                frame.render_widget(paragraph, content_area);
            }
            PopupContent::Markdown(styled_lines) => {
                let visible_lines: Vec<Line> = styled_lines
                    .iter()
                    .skip(self.scroll_offset)
                    .take(content_area.height as usize)
                    .map(|styled_line| {
                        let spans: Vec<Span> = styled_line
                            .spans
                            .iter()
                            .map(|s| Span::styled(s.text.clone(), s.style))
                            .collect();
                        Line::from(spans)
                    })
                    .collect();

                let paragraph = Paragraph::new(visible_lines);
                frame.render_widget(paragraph, content_area);
            }
            PopupContent::List { items, selected } => {
                let list_items: Vec<ListItem> = items
                    .iter()
                    .enumerate()
                    .skip(self.scroll_offset)
                    .take(content_area.height as usize)
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

                        // Check if this item is hovered
                        let is_hovered = matches!(
                            hover_target,
                            Some(crate::app::HoverTarget::PopupListItem(_, hovered_idx)) if *hovered_idx == idx
                        );

                        let style = if idx == *selected {
                            Style::default()
                                .bg(theme.popup_selection_bg)
                                .add_modifier(Modifier::BOLD)
                        } else if is_hovered {
                            Style::default()
                                .bg(theme.menu_hover_bg)
                                .fg(theme.menu_hover_fg)
                        } else {
                            Style::default()
                        };

                        ListItem::new(Line::from(spans)).style(style)
                    })
                    .collect();

                let list = List::new(list_items);
                frame.render_widget(list, content_area);
            }
            PopupContent::Custom(lines) => {
                let visible_lines: Vec<Line> = lines
                    .iter()
                    .skip(self.scroll_offset)
                    .take(content_area.height as usize)
                    .map(|line| Line::from(line.as_str()))
                    .collect();

                let paragraph = Paragraph::new(visible_lines);
                frame.render_widget(paragraph, content_area);
            }
        }

        // Render scrollbar if needed
        if needs_scrollbar {
            let scrollbar_area = Rect {
                x: inner_area.x + inner_area.width - 1,
                y: inner_area.y,
                width: 1,
                height: inner_area.height,
            };

            let scrollbar_state = ScrollbarState::new(
                total_lines,
                visible_lines_count,
                self.scroll_offset,
            );
            let scrollbar_colors = ScrollbarColors::from_theme(theme);
            render_scrollbar(frame, scrollbar_area, &scrollbar_state, &scrollbar_colors);
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
        Self { popups: Vec::new() }
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

    /// Dismiss transient popups if present at the top.
    /// These popups should be dismissed when the buffer loses focus.
    /// Returns true if a popup was dismissed.
    pub fn dismiss_transient(&mut self) -> bool {
        let is_transient = self.popups.last().is_some_and(|p| p.transient);

        if is_transient {
            self.popups.pop();
            true
        } else {
            false
        }
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
        let theme = crate::view::theme::Theme::dark();
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
        let theme = crate::view::theme::Theme::dark();
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
        let theme = crate::view::theme::Theme::dark();
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
        // Height is now based on content: 1 text line + 2 border lines = 3
        assert_eq!(area.height, 3);
        assert_eq!(area.x, (100 - 30) / 2);
        assert_eq!(area.y, (50 - 3) / 2);

        // Below cursor
        let popup_below = popup.clone().with_position(PopupPosition::BelowCursor);
        let area = popup_below.calculate_area(terminal_area, Some((20, 10)));
        assert_eq!(area.x, 20);
        assert_eq!(area.y, 12); // Two rows below cursor (allows space for cursor line)
    }

    #[test]
    fn test_popup_fixed_position_clamping() {
        let theme = crate::view::theme::Theme::dark();
        let terminal_area = Rect {
            x: 0,
            y: 0,
            width: 100,
            height: 50,
        };

        let popup = Popup::text(vec!["test".to_string()], &theme)
            .with_width(30)
            .with_max_height(10);

        // Fixed position within bounds - should stay as specified
        let popup_fixed = popup
            .clone()
            .with_position(PopupPosition::Fixed { x: 10, y: 20 });
        let area = popup_fixed.calculate_area(terminal_area, None);
        assert_eq!(area.x, 10);
        assert_eq!(area.y, 20);

        // Fixed position at right edge - x should be clamped
        let popup_right_edge = popup
            .clone()
            .with_position(PopupPosition::Fixed { x: 99, y: 20 });
        let area = popup_right_edge.calculate_area(terminal_area, None);
        // x=99 + width=30 > 100, so x should be clamped to 100-30=70
        assert_eq!(area.x, 70);
        assert_eq!(area.y, 20);

        // Fixed position beyond right edge - x should be clamped
        let popup_beyond = popup
            .clone()
            .with_position(PopupPosition::Fixed { x: 199, y: 20 });
        let area = popup_beyond.calculate_area(terminal_area, None);
        // x=199 + width=30 > 100, so x should be clamped to 100-30=70
        assert_eq!(area.x, 70);
        assert_eq!(area.y, 20);

        // Fixed position at bottom edge - y should be clamped
        let popup_bottom = popup
            .clone()
            .with_position(PopupPosition::Fixed { x: 10, y: 49 });
        let area = popup_bottom.calculate_area(terminal_area, None);
        assert_eq!(area.x, 10);
        // y=49 + height=3 > 50, so y should be clamped to 50-3=47
        assert_eq!(area.y, 47);
    }

    #[test]
    fn test_clamp_rect_to_bounds() {
        let bounds = Rect {
            x: 0,
            y: 0,
            width: 100,
            height: 50,
        };

        // Rect within bounds - unchanged
        let rect = Rect {
            x: 10,
            y: 20,
            width: 30,
            height: 10,
        };
        let clamped = super::clamp_rect_to_bounds(rect, bounds);
        assert_eq!(clamped, rect);

        // Rect at exact right edge of bounds
        let rect = Rect {
            x: 99,
            y: 20,
            width: 30,
            height: 10,
        };
        let clamped = super::clamp_rect_to_bounds(rect, bounds);
        assert_eq!(clamped.x, 99); // x is within bounds
        assert_eq!(clamped.width, 1); // width clamped to fit

        // Rect beyond bounds
        let rect = Rect {
            x: 199,
            y: 60,
            width: 30,
            height: 10,
        };
        let clamped = super::clamp_rect_to_bounds(rect, bounds);
        assert_eq!(clamped.x, 99); // x clamped to last valid position
        assert_eq!(clamped.y, 49); // y clamped to last valid position
        assert_eq!(clamped.width, 1); // width clamped to fit
        assert_eq!(clamped.height, 1); // height clamped to fit
    }
}
