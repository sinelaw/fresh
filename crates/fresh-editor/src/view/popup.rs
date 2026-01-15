use ratatui::{
    layout::Rect,
    style::{Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, List, ListItem, Paragraph},
    Frame,
};

use super::markdown::{parse_markdown, wrap_styled_lines, wrap_text_lines, StyledLine};
use super::ui::scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};
use crate::primitives::grammar_registry::GrammarRegistry;

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
    /// Bottom right corner (above status bar)
    BottomRight,
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

    /// Description text shown below title, above content (optional)
    pub description: Option<String>,

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
            description: None,
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
    ///
    /// If `registry` is provided, code blocks will have syntax highlighting
    /// for ~150+ languages via syntect.
    pub fn markdown(
        markdown_text: &str,
        theme: &crate::view::theme::Theme,
        registry: Option<&GrammarRegistry>,
    ) -> Self {
        let styled_lines = parse_markdown(markdown_text, theme, registry);
        Self {
            title: None,
            description: None,
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
            description: None,
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

    /// Get the actual visible content height (accounting for borders)
    fn visible_height(&self) -> usize {
        let border_offset = if self.bordered { 2 } else { 0 };
        (self.max_height as usize).saturating_sub(border_offset)
    }

    /// Move selection down (for list popups)
    pub fn select_next(&mut self) {
        let visible = self.visible_height();
        if let PopupContent::List { items, selected } = &mut self.content {
            if *selected < items.len().saturating_sub(1) {
                *selected += 1;
                // Adjust scroll if needed (use visible_height to account for borders)
                if *selected >= self.scroll_offset + visible {
                    self.scroll_offset = (*selected + 1).saturating_sub(visible);
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
        let visible = self.visible_height();
        if let PopupContent::List { items, selected } = &mut self.content {
            *selected = (*selected + visible).min(items.len().saturating_sub(1));
            self.scroll_offset = (*selected + 1).saturating_sub(visible);
        } else {
            self.scroll_offset += visible;
        }
    }

    /// Scroll up by one page
    pub fn page_up(&mut self) {
        let visible = self.visible_height();
        if let PopupContent::List { items: _, selected } = &mut self.content {
            *selected = selected.saturating_sub(visible);
            self.scroll_offset = *selected;
        } else {
            self.scroll_offset = self.scroll_offset.saturating_sub(visible);
        }
    }

    /// Select the first item (for list popups)
    pub fn select_first(&mut self) {
        if let PopupContent::List { items: _, selected } = &mut self.content {
            *selected = 0;
            self.scroll_offset = 0;
        } else {
            self.scroll_offset = 0;
        }
    }

    /// Select the last item (for list popups)
    pub fn select_last(&mut self) {
        let visible = self.visible_height();
        if let PopupContent::List { items, selected } = &mut self.content {
            *selected = items.len().saturating_sub(1);
            // Ensure the last item is visible
            if *selected >= visible {
                self.scroll_offset = (*selected + 1).saturating_sub(visible);
            }
        } else {
            // For non-list content, scroll to the end
            let content_height = self.item_count();
            if content_height > visible {
                self.scroll_offset = content_height.saturating_sub(visible);
            }
        }
    }

    /// Scroll by a delta amount (positive = down, negative = up)
    /// Used for mouse wheel scrolling
    pub fn scroll_by(&mut self, delta: i32) {
        let content_len = self.item_count();
        let visible = self.visible_height();
        let max_scroll = content_len.saturating_sub(visible);

        if delta < 0 {
            // Scroll up
            self.scroll_offset = self.scroll_offset.saturating_sub((-delta) as usize);
        } else {
            // Scroll down
            self.scroll_offset = (self.scroll_offset + delta as usize).min(max_scroll);
        }

        // For list popups, adjust selection to stay visible
        if let PopupContent::List { items, selected } = &mut self.content {
            let visible_start = self.scroll_offset;
            let visible_end = (self.scroll_offset + visible).min(items.len());

            if *selected < visible_start {
                *selected = visible_start;
            } else if *selected >= visible_end {
                *selected = visible_end.saturating_sub(1);
            }
        }
    }

    /// Get the total number of items/lines in the popup
    pub fn item_count(&self) -> usize {
        match &self.content {
            PopupContent::Text(lines) => lines.len(),
            PopupContent::Markdown(lines) => lines.len(),
            PopupContent::List { items, .. } => items.len(),
            PopupContent::Custom(lines) => lines.len(),
        }
    }

    /// Check if the popup needs a scrollbar (content exceeds visible area)
    pub fn needs_scrollbar(&self) -> bool {
        self.item_count() > self.visible_height()
    }

    /// Get scroll state for scrollbar rendering
    pub fn scroll_state(&self) -> (usize, usize, usize) {
        let total = self.item_count();
        let visible = self.visible_height();
        (total, visible, self.scroll_offset)
    }

    /// Find the link URL at a given relative position within the popup content area.
    /// `relative_col` and `relative_row` are relative to the inner content area (after borders).
    /// Returns None if:
    /// - The popup doesn't contain markdown content
    /// - The position doesn't have a link
    pub fn link_at_position(&self, relative_col: usize, relative_row: usize) -> Option<String> {
        let PopupContent::Markdown(styled_lines) = &self.content else {
            return None;
        };

        // Calculate the content width for wrapping
        let border_width = if self.bordered { 2 } else { 0 };
        let scrollbar_reserved = 2;
        let content_width = self
            .width
            .saturating_sub(border_width)
            .saturating_sub(scrollbar_reserved) as usize;

        // Wrap the styled lines
        let wrapped_lines = wrap_styled_lines(styled_lines, content_width);

        // Account for scroll offset
        let line_index = self.scroll_offset + relative_row;

        // Get the line at this position
        let line = wrapped_lines.get(line_index)?;

        // Find the link at the column position
        line.link_at_column(relative_col).map(|s| s.to_string())
    }

    /// Get the height of the description area (including blank line separator)
    /// Returns 0 if there is no description.
    pub fn description_height(&self) -> u16 {
        if let Some(desc) = &self.description {
            let border_width = if self.bordered { 2 } else { 0 };
            let scrollbar_reserved = 2;
            let content_width = self
                .width
                .saturating_sub(border_width)
                .saturating_sub(scrollbar_reserved) as usize;
            let desc_vec = vec![desc.clone()];
            let wrapped = wrap_text_lines(&desc_vec, content_width.saturating_sub(2));
            wrapped.len() as u16 + 1 // +1 for blank line after description
        } else {
            0
        }
    }

    /// Calculate the actual content height based on the popup content
    fn content_height(&self) -> u16 {
        // Use the popup's configured width for wrapping calculation
        self.content_height_for_width(self.width)
    }

    /// Calculate content height for a specific width, accounting for word wrapping
    fn content_height_for_width(&self, popup_width: u16) -> u16 {
        // Calculate the effective content width (accounting for borders and scrollbar)
        let border_width = if self.bordered { 2 } else { 0 };
        let scrollbar_reserved = 2; // Reserve space for potential scrollbar
        let content_width = popup_width
            .saturating_sub(border_width)
            .saturating_sub(scrollbar_reserved) as usize;

        // Calculate description height if present
        let description_lines = if let Some(desc) = &self.description {
            let desc_vec = vec![desc.clone()];
            let wrapped = wrap_text_lines(&desc_vec, content_width.saturating_sub(2));
            wrapped.len() as u16 + 1 // +1 for blank line after description
        } else {
            0
        };

        let content_lines = match &self.content {
            PopupContent::Text(lines) => {
                // Count wrapped lines
                wrap_text_lines(lines, content_width).len() as u16
            }
            PopupContent::Markdown(styled_lines) => {
                // Count wrapped styled lines
                wrap_styled_lines(styled_lines, content_width).len() as u16
            }
            PopupContent::List { items, .. } => items.len() as u16,
            PopupContent::Custom(lines) => lines.len() as u16,
        };

        // Add border lines if bordered
        let border_height = if self.bordered { 2 } else { 0 };

        description_lines + content_lines + border_height
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
            PopupPosition::BottomRight => {
                let width = self.width.min(terminal_area.width);
                let height = self
                    .content_height()
                    .min(self.max_height)
                    .min(terminal_area.height);
                // Position in bottom right, leaving 2 rows for status bar
                let x = terminal_area.width.saturating_sub(width);
                let y = terminal_area
                    .height
                    .saturating_sub(height)
                    .saturating_sub(2);
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

        // Render description if present, and adjust content area
        let content_start_y;
        if let Some(desc) = &self.description {
            // Word-wrap description to fit inner width
            let desc_wrap_width = inner_area.width.saturating_sub(2) as usize; // Leave some padding
            let desc_vec = vec![desc.clone()];
            let wrapped_desc = wrap_text_lines(&desc_vec, desc_wrap_width);
            let desc_lines: usize = wrapped_desc.len();

            // Render each description line
            for (i, line) in wrapped_desc.iter().enumerate() {
                if i >= inner_area.height as usize {
                    break;
                }
                let line_area = Rect {
                    x: inner_area.x,
                    y: inner_area.y + i as u16,
                    width: inner_area.width,
                    height: 1,
                };
                let desc_style = Style::default().fg(theme.help_separator_fg);
                frame.render_widget(Paragraph::new(line.as_str()).style(desc_style), line_area);
            }

            // Add blank line after description
            content_start_y = inner_area.y + (desc_lines as u16).min(inner_area.height) + 1;
        } else {
            content_start_y = inner_area.y;
        }

        // Adjust inner_area to start after description
        let inner_area = Rect {
            x: inner_area.x,
            y: content_start_y,
            width: inner_area.width,
            height: inner_area
                .height
                .saturating_sub(content_start_y - area.y - if self.bordered { 1 } else { 0 }),
        };

        // For text and markdown content, we need to wrap first to determine if scrollbar is needed.
        // We wrap to the width that would be available if scrollbar is shown (conservative approach).
        let scrollbar_reserved_width = 2; // 1 for scrollbar + 1 for spacing
        let wrap_width = inner_area.width.saturating_sub(scrollbar_reserved_width) as usize;
        let visible_lines_count = inner_area.height as usize;

        // Calculate wrapped line count and determine if scrollbar is needed
        let (wrapped_total_lines, needs_scrollbar) = match &self.content {
            PopupContent::Text(lines) => {
                let wrapped = wrap_text_lines(lines, wrap_width);
                let count = wrapped.len();
                (
                    count,
                    count > visible_lines_count && inner_area.width > scrollbar_reserved_width,
                )
            }
            PopupContent::Markdown(styled_lines) => {
                let wrapped = wrap_styled_lines(styled_lines, wrap_width);
                let count = wrapped.len();
                (
                    count,
                    count > visible_lines_count && inner_area.width > scrollbar_reserved_width,
                )
            }
            PopupContent::List { items, .. } => {
                let count = items.len();
                (
                    count,
                    count > visible_lines_count && inner_area.width > scrollbar_reserved_width,
                )
            }
            PopupContent::Custom(lines) => {
                let count = lines.len();
                (
                    count,
                    count > visible_lines_count && inner_area.width > scrollbar_reserved_width,
                )
            }
        };

        // Adjust content area to leave room for scrollbar if needed
        let content_area = if needs_scrollbar {
            Rect {
                x: inner_area.x,
                y: inner_area.y,
                width: inner_area.width.saturating_sub(scrollbar_reserved_width),
                height: inner_area.height,
            }
        } else {
            inner_area
        };

        match &self.content {
            PopupContent::Text(lines) => {
                // Word-wrap lines to fit content area width
                let wrapped_lines = wrap_text_lines(lines, content_area.width as usize);
                let visible_lines: Vec<Line> = wrapped_lines
                    .iter()
                    .skip(self.scroll_offset)
                    .take(content_area.height as usize)
                    .map(|line| Line::from(line.as_str()))
                    .collect();

                let paragraph = Paragraph::new(visible_lines);
                frame.render_widget(paragraph, content_area);
            }
            PopupContent::Markdown(styled_lines) => {
                // Word-wrap styled lines to fit content area width
                let wrapped_lines = wrap_styled_lines(styled_lines, content_area.width as usize);

                // Collect link overlay info for OSC 8 rendering after the main draw
                // Each entry: (visible_line_idx, start_column, link_text, url)
                let mut link_overlays: Vec<(usize, usize, String, String)> = Vec::new();

                let visible_lines: Vec<Line> = wrapped_lines
                    .iter()
                    .skip(self.scroll_offset)
                    .take(content_area.height as usize)
                    .enumerate()
                    .map(|(line_idx, styled_line)| {
                        let mut col = 0usize;
                        let spans: Vec<Span> = styled_line
                            .spans
                            .iter()
                            .map(|s| {
                                let span_width =
                                    unicode_width::UnicodeWidthStr::width(s.text.as_str());
                                if let Some(url) = &s.link_url {
                                    link_overlays.push((
                                        line_idx,
                                        col,
                                        s.text.clone(),
                                        url.clone(),
                                    ));
                                }
                                col += span_width;
                                Span::styled(s.text.clone(), s.style)
                            })
                            .collect();
                        Line::from(spans)
                    })
                    .collect();

                let paragraph = Paragraph::new(visible_lines);
                frame.render_widget(paragraph, content_area);

                // Apply OSC 8 hyperlinks following Ratatui's official workaround
                let buffer = frame.buffer_mut();
                let max_x = content_area.x + content_area.width;
                for (line_idx, col_start, text, url) in link_overlays {
                    let y = content_area.y + line_idx as u16;
                    if y >= content_area.y + content_area.height {
                        continue;
                    }
                    let start_x = content_area.x + col_start as u16;
                    apply_hyperlink_overlay(buffer, start_x, y, max_x, &text, &url);
                }
            }
            PopupContent::List { items, selected } => {
                let list_items: Vec<ListItem> = items
                    .iter()
                    .enumerate()
                    .skip(self.scroll_offset)
                    .take(content_area.height as usize)
                    .map(|(idx, item)| {
                        // Check if this item is hovered or selected
                        let is_hovered = matches!(
                            hover_target,
                            Some(crate::app::HoverTarget::PopupListItem(_, hovered_idx)) if *hovered_idx == idx
                        );
                        let is_selected = idx == *selected;

                        let mut spans = Vec::new();

                        // Add icon if present
                        if let Some(icon) = &item.icon {
                            spans.push(Span::raw(format!("{} ", icon)));
                        }

                        // Add main text with underline for clickable items
                        let text_style = if is_selected {
                            Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
                        } else {
                            Style::default().add_modifier(Modifier::UNDERLINED)
                        };
                        spans.push(Span::styled(&item.text, text_style));

                        // Add detail if present
                        if let Some(detail) = &item.detail {
                            spans.push(Span::styled(
                                format!(" {}", detail),
                                Style::default().fg(theme.help_separator_fg),
                            ));
                        }

                        // Row style (background only, no underline)
                        let row_style = if is_selected {
                            Style::default().bg(theme.popup_selection_bg)
                        } else if is_hovered {
                            Style::default()
                                .bg(theme.menu_hover_bg)
                                .fg(theme.menu_hover_fg)
                        } else {
                            Style::default()
                        };

                        ListItem::new(Line::from(spans)).style(row_style)
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

            let scrollbar_state =
                ScrollbarState::new(wrapped_total_lines, visible_lines_count, self.scroll_offset);
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

    /// Get reference to popup by index
    pub fn get(&self, index: usize) -> Option<&Popup> {
        self.popups.get(index)
    }

    /// Get mutable reference to popup by index
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Popup> {
        self.popups.get_mut(index)
    }

    /// Check if any popups are visible
    pub fn is_visible(&self) -> bool {
        !self.popups.is_empty()
    }

    /// Check if the topmost popup is a completion popup (supports type-to-filter)
    pub fn is_completion_popup(&self) -> bool {
        self.top()
            .and_then(|p| p.title.as_ref())
            .map(|title| title == "Completion")
            .unwrap_or(false)
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

/// Overlay OSC 8 hyperlinks in 2-character chunks to keep text layout aligned.
///
/// This mirrors the approach used in Ratatui's official hyperlink example to
/// work around Crossterm width accounting bugs for OSC sequences.
fn apply_hyperlink_overlay(
    buffer: &mut ratatui::buffer::Buffer,
    start_x: u16,
    y: u16,
    max_x: u16,
    text: &str,
    url: &str,
) {
    let mut chunk_index = 0u16;
    let mut chars = text.chars();

    loop {
        let mut chunk = String::new();
        for _ in 0..2 {
            if let Some(ch) = chars.next() {
                chunk.push(ch);
            } else {
                break;
            }
        }

        if chunk.is_empty() {
            break;
        }

        let x = start_x + chunk_index * 2;
        if x >= max_x {
            break;
        }

        let hyperlink = format!("\x1B]8;;{}\x07{}\x1B]8;;\x07", url, chunk);
        buffer[(x, y)].set_symbol(&hyperlink);

        chunk_index += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::theme;

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
        let theme = crate::view::theme::Theme::from_name(theme::THEME_DARK).unwrap();
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
        let theme = crate::view::theme::Theme::from_name(theme::THEME_DARK).unwrap();
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
        let theme = crate::view::theme::Theme::from_name(theme::THEME_DARK).unwrap();
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
        let theme = crate::view::theme::Theme::from_name(theme::THEME_DARK).unwrap();
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

    #[test]
    fn hyperlink_overlay_chunks_pairs() {
        use ratatui::{buffer::Buffer, layout::Rect};

        let mut buffer = Buffer::empty(Rect::new(0, 0, 10, 1));
        buffer[(0, 0)].set_symbol("P");
        buffer[(1, 0)].set_symbol("l");
        buffer[(2, 0)].set_symbol("a");
        buffer[(3, 0)].set_symbol("y");

        apply_hyperlink_overlay(&mut buffer, 0, 0, 10, "Play", "https://example.com");

        let first = buffer[(0, 0)].symbol().to_string();
        let second = buffer[(2, 0)].symbol().to_string();

        assert!(
            first.contains("Pl"),
            "first chunk should contain 'Pl', got {first:?}"
        );
        assert!(
            second.contains("ay"),
            "second chunk should contain 'ay', got {second:?}"
        );
    }
}
