//! Single-line text input control
//!
//! Renders as: `Label: [text content     ]`
//!
//! This module provides a complete text input component with:
//! - State management (`TextInputState`)
//! - Rendering (`render_text_input`, `render_text_input_aligned`)
//! - Input handling (`TextInputState::handle_mouse`, `handle_key`)
//! - Layout/hit testing (`TextInputLayout`)

mod input;
mod render;

use crate::primitives::grapheme;
use ratatui::layout::Rect;
use ratatui::style::Color;

pub use input::TextInputEvent;
pub use render::{render_text_input, render_text_input_aligned};

use super::FocusState;

/// State for a text input control
#[derive(Debug, Clone)]
pub struct TextInputState {
    /// Current text value
    pub value: String,
    /// Cursor position (character index)
    pub cursor: usize,
    /// Label displayed before the input
    pub label: String,
    /// Placeholder text when empty
    pub placeholder: String,
    /// Focus state
    pub focus: FocusState,
    /// If true, validate that value is valid JSON before allowing exit
    pub validate_json: bool,
}

impl TextInputState {
    /// Create a new text input state
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            value: String::new(),
            cursor: 0,
            label: label.into(),
            placeholder: String::new(),
            focus: FocusState::Normal,
            validate_json: false,
        }
    }

    /// Set JSON validation mode
    pub fn with_json_validation(mut self) -> Self {
        self.validate_json = true;
        self
    }

    /// Check if the current value is valid (valid JSON if validate_json is set)
    pub fn is_valid(&self) -> bool {
        if self.validate_json {
            serde_json::from_str::<serde_json::Value>(&self.value).is_ok()
        } else {
            true
        }
    }

    /// Set the initial value
    pub fn with_value(mut self, value: impl Into<String>) -> Self {
        self.value = value.into();
        self.cursor = self.value.len();
        self
    }

    /// Set the placeholder text
    pub fn with_placeholder(mut self, placeholder: impl Into<String>) -> Self {
        self.placeholder = placeholder.into();
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

    /// Insert a character at the cursor position
    pub fn insert(&mut self, c: char) {
        if !self.is_enabled() {
            return;
        }
        self.value.insert(self.cursor, c);
        self.cursor += c.len_utf8();
    }

    /// Insert a string at the cursor position
    pub fn insert_str(&mut self, s: &str) {
        if !self.is_enabled() {
            return;
        }
        self.value.insert_str(self.cursor, s);
        self.cursor += s.len();
    }

    /// Delete the character before the cursor (backspace)
    pub fn backspace(&mut self) {
        if !self.is_enabled() || self.cursor == 0 {
            return;
        }
        // Find the previous character boundary
        let prev_boundary = self.value[..self.cursor]
            .char_indices()
            .next_back()
            .map(|(i, _)| i)
            .unwrap_or(0);
        self.value.remove(prev_boundary);
        self.cursor = prev_boundary;
    }

    /// Delete the grapheme cluster at the cursor (delete key)
    ///
    /// Deletes the entire grapheme cluster, handling combining characters properly.
    pub fn delete(&mut self) {
        if !self.is_enabled() || self.cursor >= self.value.len() {
            return;
        }
        let next_boundary = grapheme::next_grapheme_boundary(&self.value, self.cursor);
        self.value.drain(self.cursor..next_boundary);
    }

    /// Move cursor left (to previous grapheme cluster boundary)
    ///
    /// Uses grapheme cluster boundaries for proper handling of combining characters
    /// like Thai diacritics, emoji with modifiers, etc.
    pub fn move_left(&mut self) {
        if self.cursor > 0 {
            self.cursor = grapheme::prev_grapheme_boundary(&self.value, self.cursor);
        }
    }

    /// Move cursor right (to next grapheme cluster boundary)
    ///
    /// Uses grapheme cluster boundaries for proper handling of combining characters
    /// like Thai diacritics, emoji with modifiers, etc.
    pub fn move_right(&mut self) {
        if self.cursor < self.value.len() {
            self.cursor = grapheme::next_grapheme_boundary(&self.value, self.cursor);
        }
    }

    /// Move cursor to start
    pub fn move_home(&mut self) {
        self.cursor = 0;
    }

    /// Move cursor to end
    pub fn move_end(&mut self) {
        self.cursor = self.value.len();
    }

    /// Clear the input
    pub fn clear(&mut self) {
        if self.is_enabled() {
            self.value.clear();
            self.cursor = 0;
        }
    }

    /// Set the value directly
    pub fn set_value(&mut self, value: impl Into<String>) {
        if self.is_enabled() {
            self.value = value.into();
            self.cursor = self.value.len();
        }
    }
}

/// Colors for the text input control
#[derive(Debug, Clone, Copy)]
pub struct TextInputColors {
    /// Label color
    pub label: Color,
    /// Input text color
    pub text: Color,
    /// Border/bracket color
    pub border: Color,
    /// Placeholder text color
    pub placeholder: Color,
    /// Cursor color
    pub cursor: Color,
    /// Focused highlight color
    pub focused: Color,
    /// Disabled color
    pub disabled: Color,
}

impl Default for TextInputColors {
    fn default() -> Self {
        Self {
            label: Color::White,
            text: Color::White,
            border: Color::Gray,
            placeholder: Color::DarkGray,
            cursor: Color::Yellow,
            focused: Color::Cyan,
            disabled: Color::DarkGray,
        }
    }
}

impl TextInputColors {
    /// Create colors from theme
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            label: theme.editor_fg,
            text: theme.editor_fg,
            border: theme.line_number_fg,
            placeholder: theme.line_number_fg,
            cursor: theme.cursor,
            focused: theme.selection_bg,
            disabled: theme.line_number_fg,
        }
    }
}

/// Layout information returned after rendering for hit testing
#[derive(Debug, Clone, Copy, Default)]
pub struct TextInputLayout {
    /// The text input field area
    pub input_area: Rect,
    /// The full control area including label
    pub full_area: Rect,
    /// Cursor position in screen coordinates (if focused)
    pub cursor_pos: Option<(u16, u16)>,
}

impl TextInputLayout {
    /// Check if a point is within the input area
    pub fn is_input(&self, x: u16, y: u16) -> bool {
        x >= self.input_area.x
            && x < self.input_area.x + self.input_area.width
            && y >= self.input_area.y
            && y < self.input_area.y + self.input_area.height
    }

    /// Check if a point is within the full control area
    pub fn contains(&self, x: u16, y: u16) -> bool {
        x >= self.full_area.x
            && x < self.full_area.x + self.full_area.width
            && y >= self.full_area.y
            && y < self.full_area.y + self.full_area.height
    }
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
    fn test_text_input_renders() {
        test_frame(40, 1, |frame, area| {
            let state = TextInputState::new("Name").with_value("John");
            let colors = TextInputColors::default();
            let layout = render_text_input(frame, area, &state, &colors, 20);

            assert!(layout.input_area.width > 0);
        });
    }

    #[test]
    fn test_text_input_insert() {
        let mut state = TextInputState::new("Test");
        state.insert('a');
        state.insert('b');
        state.insert('c');
        assert_eq!(state.value, "abc");
        assert_eq!(state.cursor, 3);
    }

    #[test]
    fn test_text_input_backspace() {
        let mut state = TextInputState::new("Test").with_value("abc");
        state.backspace();
        assert_eq!(state.value, "ab");
        assert_eq!(state.cursor, 2);
    }

    #[test]
    fn test_text_input_cursor_movement() {
        let mut state = TextInputState::new("Test").with_value("hello");
        assert_eq!(state.cursor, 5);

        state.move_left();
        assert_eq!(state.cursor, 4);

        state.move_home();
        assert_eq!(state.cursor, 0);

        state.move_right();
        assert_eq!(state.cursor, 1);

        state.move_end();
        assert_eq!(state.cursor, 5);
    }

    #[test]
    fn test_text_input_delete() {
        let mut state = TextInputState::new("Test").with_value("abc");
        state.move_home();
        state.delete();
        assert_eq!(state.value, "bc");
        assert_eq!(state.cursor, 0);
    }

    #[test]
    fn test_text_input_disabled() {
        let mut state = TextInputState::new("Test").with_focus(FocusState::Disabled);
        state.insert('a');
        assert_eq!(state.value, "");
    }

    #[test]
    fn test_text_input_clear() {
        let mut state = TextInputState::new("Test").with_value("hello");
        state.clear();
        assert_eq!(state.value, "");
        assert_eq!(state.cursor, 0);
    }

    #[test]
    fn test_text_input_multibyte_insert_and_backspace() {
        // Regression test for issue #466: panic when backspacing multi-byte chars
        let mut state = TextInputState::new("Test");
        // © is 2 bytes in UTF-8
        state.insert('©');
        assert_eq!(state.value, "©");
        assert_eq!(state.cursor, 2); // byte position, not char position

        // Backspace should delete the whole character, not cause a panic
        state.backspace();
        assert_eq!(state.value, "");
        assert_eq!(state.cursor, 0);
    }

    #[test]
    fn test_text_input_multibyte_cursor_movement() {
        let mut state = TextInputState::new("Test").with_value("日本語");
        // Each Japanese character is 3 bytes
        assert_eq!(state.cursor, 9);

        state.move_left();
        assert_eq!(state.cursor, 6); // moved back by one character (3 bytes)

        state.move_left();
        assert_eq!(state.cursor, 3);

        state.move_right();
        assert_eq!(state.cursor, 6);

        state.move_home();
        assert_eq!(state.cursor, 0);

        state.move_right();
        assert_eq!(state.cursor, 3); // moved forward by one character (3 bytes)
    }

    #[test]
    fn test_text_input_multibyte_delete() {
        let mut state = TextInputState::new("Test").with_value("a日b");
        // 'a' is 1 byte, '日' is 3 bytes, 'b' is 1 byte = 5 bytes total
        assert_eq!(state.cursor, 5);

        state.move_home();
        state.move_right(); // cursor now at byte 1 (after 'a', before '日')
        assert_eq!(state.cursor, 1);

        state.delete(); // delete '日'
        assert_eq!(state.value, "ab");
        assert_eq!(state.cursor, 1);
    }

    #[test]
    fn test_text_input_insert_between_multibyte() {
        let mut state = TextInputState::new("Test").with_value("日語");
        state.move_home();
        state.move_right(); // cursor after first character
        assert_eq!(state.cursor, 3);

        state.insert('本');
        assert_eq!(state.value, "日本語");
        assert_eq!(state.cursor, 6);
    }
}
