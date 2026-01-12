//! Text list control for managing lists of strings
//!
//! Renders as a list with add/remove buttons:
//! ```text
//! Label:
//!   [item one                ] [x]
//!   [item two                ] [x]
//!   [                        ] [+]
//! ```
//!
//! This module provides a complete text list component with:
//! - State management (`TextListState`)
//! - Rendering (`render_text_list`)
//! - Input handling (`TextListState::handle_mouse`, `handle_key`)
//! - Layout/hit testing (`TextListLayout`)

mod input;
mod render;

use ratatui::layout::Rect;
use ratatui::style::Color;

pub use input::TextListEvent;
pub use render::render_text_list;

use super::FocusState;

/// State for a text list control
#[derive(Debug, Clone)]
pub struct TextListState {
    /// List of items
    pub items: Vec<String>,
    /// Currently focused item index (None = add new item field)
    pub focused_item: Option<usize>,
    /// Cursor position within the focused item
    pub cursor: usize,
    /// Text in the "add new" field
    pub new_item_text: String,
    /// Label displayed above the list
    pub label: String,
    /// Focus state
    pub focus: FocusState,
}

impl TextListState {
    /// Create a new text list state
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            items: Vec::new(),
            focused_item: None,
            cursor: 0,
            new_item_text: String::new(),
            label: label.into(),
            focus: FocusState::Normal,
        }
    }

    /// Set the initial items
    pub fn with_items(mut self, items: Vec<String>) -> Self {
        self.items = items;
        self
    }

    /// Set the focus state
    pub fn with_focus(mut self, focus: FocusState) -> Self {
        self.focus = focus;
        self
    }

    /// Check if the control is enabled
    pub fn is_enabled(&self) -> bool {
        self.focus != FocusState::Disabled
    }

    /// Add a new item from the new_item_text field
    pub fn add_item(&mut self) {
        if !self.is_enabled() || self.new_item_text.is_empty() {
            return;
        }
        self.items.push(std::mem::take(&mut self.new_item_text));
        self.cursor = 0;
    }

    /// Insert a string at the cursor position
    pub fn insert_str(&mut self, s: &str) {
        if !self.is_enabled() {
            return;
        }
        if let Some(index) = self.focused_item {
            if let Some(item) = self.items.get_mut(index) {
                if self.cursor <= item.len() {
                    item.insert_str(self.cursor, s);
                    self.cursor += s.len();
                }
            }
        } else if self.cursor <= self.new_item_text.len() {
            self.new_item_text.insert_str(self.cursor, s);
            self.cursor += s.len();
        }
    }

    /// Remove an item by index
    pub fn remove_item(&mut self, index: usize) {
        if !self.is_enabled() || index >= self.items.len() {
            return;
        }
        self.items.remove(index);
        if let Some(focused) = self.focused_item {
            if focused >= self.items.len() {
                self.focused_item = if self.items.is_empty() {
                    None
                } else {
                    Some(self.items.len() - 1)
                };
            }
        }
    }

    /// Focus on an item for editing
    pub fn focus_item(&mut self, index: usize) {
        if index < self.items.len() {
            self.focused_item = Some(index);
            self.cursor = self.items[index].len();
        }
    }

    /// Focus on the new item field
    pub fn focus_new_item(&mut self) {
        self.focused_item = None;
        self.cursor = self.new_item_text.len();
    }

    /// Insert a character in the focused field
    pub fn insert(&mut self, c: char) {
        if !self.is_enabled() {
            return;
        }
        match self.focused_item {
            Some(idx) if idx < self.items.len() => {
                self.items[idx].insert(self.cursor, c);
                self.cursor += 1;
            }
            None => {
                self.new_item_text.insert(self.cursor, c);
                self.cursor += 1;
            }
            _ => {}
        }
    }

    /// Backspace in the focused field
    pub fn backspace(&mut self) {
        if !self.is_enabled() || self.cursor == 0 {
            return;
        }
        self.cursor -= 1;
        match self.focused_item {
            Some(idx) if idx < self.items.len() => {
                self.items[idx].remove(self.cursor);
            }
            None => {
                self.new_item_text.remove(self.cursor);
            }
            _ => {}
        }
    }

    /// Move cursor left
    pub fn move_left(&mut self) {
        if self.cursor > 0 {
            self.cursor -= 1;
        }
    }

    /// Move cursor right
    pub fn move_right(&mut self) {
        let max = match self.focused_item {
            Some(idx) if idx < self.items.len() => self.items[idx].len(),
            None => self.new_item_text.len(),
            _ => 0,
        };
        if self.cursor < max {
            self.cursor += 1;
        }
    }

    /// Move cursor to beginning of current field
    pub fn move_home(&mut self) {
        self.cursor = 0;
    }

    /// Move cursor to end of current field
    pub fn move_end(&mut self) {
        self.cursor = match self.focused_item {
            Some(idx) if idx < self.items.len() => self.items[idx].len(),
            None => self.new_item_text.len(),
            _ => 0,
        };
    }

    /// Delete character at cursor (forward delete)
    pub fn delete(&mut self) {
        if !self.is_enabled() {
            return;
        }
        let max = match self.focused_item {
            Some(idx) if idx < self.items.len() => self.items[idx].len(),
            None => self.new_item_text.len(),
            _ => return,
        };
        if self.cursor >= max {
            return;
        }
        match self.focused_item {
            Some(idx) if idx < self.items.len() => {
                self.items[idx].remove(self.cursor);
            }
            None => {
                self.new_item_text.remove(self.cursor);
            }
            _ => {}
        }
    }

    /// Move focus to previous item
    pub fn focus_prev(&mut self) {
        match self.focused_item {
            Some(0) => {}
            Some(idx) => {
                self.focused_item = Some(idx - 1);
                self.cursor = self.items[idx - 1].len();
            }
            None if !self.items.is_empty() => {
                self.focused_item = Some(self.items.len() - 1);
                self.cursor = self.items.last().map(|s| s.len()).unwrap_or(0);
            }
            None => {}
        }
    }

    /// Move focus to next item
    pub fn focus_next(&mut self) {
        match self.focused_item {
            Some(idx) if idx + 1 < self.items.len() => {
                self.focused_item = Some(idx + 1);
                self.cursor = self.items[idx + 1].len();
            }
            Some(_) => {
                self.focused_item = None;
                self.cursor = self.new_item_text.len();
            }
            None => {}
        }
    }
}

/// Colors for the text list control
#[derive(Debug, Clone, Copy)]
pub struct TextListColors {
    /// Label color
    pub label: Color,
    /// Item text color
    pub text: Color,
    /// Border/bracket color
    pub border: Color,
    /// Remove button color
    pub remove_button: Color,
    /// Add button color
    pub add_button: Color,
    /// Focused item highlight
    pub focused: Color,
    /// Cursor color
    pub cursor: Color,
    /// Disabled color
    pub disabled: Color,
}

impl Default for TextListColors {
    fn default() -> Self {
        Self {
            label: Color::White,
            text: Color::White,
            border: Color::Gray,
            remove_button: Color::Red,
            add_button: Color::Green,
            focused: Color::Cyan,
            cursor: Color::Yellow,
            disabled: Color::DarkGray,
        }
    }
}

impl TextListColors {
    /// Create colors from theme
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            label: theme.editor_fg,
            text: theme.editor_fg,
            border: theme.line_number_fg,
            remove_button: theme.diagnostic_error_fg,
            add_button: theme.diagnostic_info_fg,
            focused: theme.selection_bg,
            cursor: theme.cursor,
            disabled: theme.line_number_fg,
        }
    }
}

/// Hit area for a text list row
#[derive(Debug, Clone, Copy)]
pub struct TextListRowLayout {
    /// The text field area
    pub text_area: Rect,
    /// The button area (remove or add)
    pub button_area: Rect,
    /// Index of this row (None for add-new row)
    pub index: Option<usize>,
}

/// Layout information returned after rendering for hit testing
#[derive(Debug, Clone, Default)]
pub struct TextListLayout {
    /// Layout for each row
    pub rows: Vec<TextListRowLayout>,
    /// The full control area
    pub full_area: Rect,
}

impl TextListLayout {
    /// Find which row and component was clicked
    pub fn hit_test(&self, x: u16, y: u16) -> Option<TextListHit> {
        for row in &self.rows {
            if y >= row.text_area.y
                && y < row.text_area.y + row.text_area.height
                && x >= row.button_area.x
                && x < row.button_area.x + row.button_area.width
            {
                return Some(TextListHit::Button(row.index));
            }
            if y >= row.text_area.y
                && y < row.text_area.y + row.text_area.height
                && x >= row.text_area.x
                && x < row.text_area.x + row.text_area.width
            {
                return Some(TextListHit::TextField(row.index));
            }
        }
        None
    }
}

/// Result of hit testing on a text list
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextListHit {
    /// Clicked on a text field (None = add-new field)
    TextField(Option<usize>),
    /// Clicked on a button (None = add button, Some = remove button)
    Button(Option<usize>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::backend::TestBackend;
    use ratatui::Terminal;

    fn test_frame<F>(width: u16, height: u16, f: F)
    where
        F: FnOnce(&mut ratatui::Frame, Rect),
    {
        let backend = TestBackend::new(width, height);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal
            .draw(|frame| {
                let area = Rect::new(0, 0, width, height);
                f(frame, area);
            })
            .unwrap();
    }

    #[test]
    fn test_text_list_empty() {
        test_frame(40, 5, |frame, area| {
            let state = TextListState::new("Items");
            let colors = TextListColors::default();
            let layout = render_text_list(frame, area, &state, &colors, 20);

            assert_eq!(layout.rows.len(), 1);
            assert!(layout.rows[0].index.is_none());
        });
    }

    #[test]
    fn test_text_list_with_items() {
        test_frame(40, 5, |frame, area| {
            let state =
                TextListState::new("Items").with_items(vec!["one".to_string(), "two".to_string()]);
            let colors = TextListColors::default();
            let layout = render_text_list(frame, area, &state, &colors, 20);

            assert_eq!(layout.rows.len(), 3);
            assert_eq!(layout.rows[0].index, Some(0));
            assert_eq!(layout.rows[1].index, Some(1));
            assert!(layout.rows[2].index.is_none());
        });
    }

    #[test]
    fn test_text_list_add_item() {
        let mut state = TextListState::new("Items");
        state.new_item_text = "new item".to_string();
        state.add_item();

        assert_eq!(state.items.len(), 1);
        assert_eq!(state.items[0], "new item");
        assert!(state.new_item_text.is_empty());
    }

    #[test]
    fn test_text_list_remove_item() {
        let mut state =
            TextListState::new("Items").with_items(vec!["a".to_string(), "b".to_string()]);
        state.remove_item(0);

        assert_eq!(state.items.len(), 1);
        assert_eq!(state.items[0], "b");
    }

    #[test]
    fn test_text_list_edit_item() {
        let mut state = TextListState::new("Items").with_items(vec!["hello".to_string()]);
        state.focus_item(0);
        state.insert('!');

        assert_eq!(state.items[0], "hello!");
    }

    #[test]
    fn test_text_list_navigation() {
        let mut state = TextListState::new("Items")
            .with_items(vec!["a".to_string(), "b".to_string()])
            .with_focus(FocusState::Focused);

        assert!(state.focused_item.is_none());

        state.focus_prev();
        assert_eq!(state.focused_item, Some(1));

        state.focus_prev();
        assert_eq!(state.focused_item, Some(0));

        state.focus_prev();
        assert_eq!(state.focused_item, Some(0));

        state.focus_next();
        assert_eq!(state.focused_item, Some(1));

        state.focus_next();
        assert!(state.focused_item.is_none());
    }

    #[test]
    fn test_text_list_hit_test() {
        test_frame(40, 5, |frame, area| {
            let state = TextListState::new("Items").with_items(vec!["one".to_string()]);
            let colors = TextListColors::default();
            let layout = render_text_list(frame, area, &state, &colors, 20);

            let btn = &layout.rows[0].button_area;
            let hit = layout.hit_test(btn.x, btn.y);
            assert_eq!(hit, Some(TextListHit::Button(Some(0))));

            let add_btn = &layout.rows[1].button_area;
            let hit = layout.hit_test(add_btn.x, add_btn.y);
            assert_eq!(hit, Some(TextListHit::Button(None)));
        });
    }
}
