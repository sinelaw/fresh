//! Number input control with increment/decrement
//!
//! Renders as: `Label: [  42  ] [-] [+]`
//!
//! This module provides a complete number input component with:
//! - State management (`NumberInputState`)
//! - Rendering (`render_number_input`, `render_number_input_aligned`)
//! - Input handling (`NumberInputState::handle_mouse`, `handle_key`)
//! - Layout/hit testing (`NumberInputLayout`)

mod input;
mod render;

use ratatui::layout::Rect;
use ratatui::style::Color;

pub use input::NumberInputEvent;
pub use render::{render_number_input, render_number_input_aligned};

use super::FocusState;
use crate::view::ui::text_edit::TextEdit;

/// State for a number input control
#[derive(Debug, Clone)]
pub struct NumberInputState {
    /// Current value
    pub value: i64,
    /// Minimum allowed value
    pub min: Option<i64>,
    /// Maximum allowed value
    pub max: Option<i64>,
    /// Step amount for increment/decrement
    pub step: i64,
    /// Label displayed before the input
    pub label: String,
    /// Focus state
    pub focus: FocusState,
    /// Text editor for editing mode (None when not editing)
    pub editor: Option<TextEdit>,
    /// Whether this value is a percentage (float value * 100 for display)
    /// When true, the value should be divided by 100 when converting back to JSON
    pub is_percentage: bool,
}

impl NumberInputState {
    /// Create a new number input state
    pub fn new(value: i64, label: impl Into<String>) -> Self {
        Self {
            value,
            min: None,
            max: None,
            step: 1,
            label: label.into(),
            focus: FocusState::Normal,
            editor: None,
            is_percentage: false,
        }
    }

    /// Check if currently editing
    pub fn editing(&self) -> bool {
        self.editor.is_some()
    }

    /// Set the minimum value
    pub fn with_min(mut self, min: i64) -> Self {
        self.min = Some(min);
        self
    }

    /// Set the maximum value
    pub fn with_max(mut self, max: i64) -> Self {
        self.max = Some(max);
        self
    }

    /// Set the step amount
    pub fn with_step(mut self, step: i64) -> Self {
        self.step = step;
        self
    }

    /// Set the focus state
    pub fn with_focus(mut self, focus: FocusState) -> Self {
        self.focus = focus;
        self
    }

    /// Mark this value as a percentage (float * 100 for display)
    pub fn with_percentage(mut self) -> Self {
        self.is_percentage = true;
        self
    }

    /// Check if the control is enabled
    pub fn is_enabled(&self) -> bool {
        self.focus != FocusState::Disabled
    }

    /// Increment the value by step
    pub fn increment(&mut self) {
        if !self.is_enabled() {
            return;
        }
        let new_value = self.value.saturating_add(self.step);
        self.value = match self.max {
            Some(max) => new_value.min(max),
            None => new_value,
        };
    }

    /// Decrement the value by step
    pub fn decrement(&mut self) {
        if !self.is_enabled() {
            return;
        }
        let new_value = self.value.saturating_sub(self.step);
        self.value = match self.min {
            Some(min) => new_value.max(min),
            None => new_value,
        };
    }

    /// Set the value directly, respecting min/max
    pub fn set_value(&mut self, value: i64) {
        if !self.is_enabled() {
            return;
        }
        let mut v = value;
        if let Some(min) = self.min {
            v = v.max(min);
        }
        if let Some(max) = self.max {
            v = v.min(max);
        }
        self.value = v;
    }

    /// Start editing mode
    pub fn start_editing(&mut self) {
        if !self.is_enabled() {
            return;
        }
        let mut editor = TextEdit::single_line();
        editor.set_value(&self.value.to_string());
        // Select all text so typing replaces the value
        editor.select_all();
        self.editor = Some(editor);
    }

    /// Cancel editing and restore original value
    pub fn cancel_editing(&mut self) {
        self.editor = None;
    }

    /// Confirm editing and apply the new value
    pub fn confirm_editing(&mut self) {
        if let Some(editor) = self.editor.take() {
            if let Ok(new_value) = editor.value().parse::<i64>() {
                self.set_value(new_value);
            }
        }
    }

    /// Insert a character while editing
    /// Allows digits, minus sign, and decimal point for number input
    pub fn insert_char(&mut self, c: char) {
        if let Some(editor) = &mut self.editor {
            // Allow digits, minus sign, and decimal point
            if c.is_ascii_digit() || c == '-' || c == '.' {
                editor.insert_char(c);
            }
        }
    }

    /// Backspace while editing
    pub fn backspace(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.backspace();
        }
    }

    /// Delete character at cursor
    pub fn delete(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.delete();
        }
    }

    /// Move cursor left
    pub fn move_left(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_left();
        }
    }

    /// Move cursor right
    pub fn move_right(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_right();
        }
    }

    /// Move cursor to start of text
    pub fn move_home(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_home();
        }
    }

    /// Move cursor to end of text
    pub fn move_end(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_end();
        }
    }

    /// Move cursor left by word (Ctrl+Left)
    pub fn move_word_left(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_word_left();
        }
    }

    /// Move cursor right by word (Ctrl+Right)
    pub fn move_word_right(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_word_right();
        }
    }

    /// Move cursor left with selection (Shift+Left)
    pub fn move_left_selecting(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_left_selecting();
        }
    }

    /// Move cursor right with selection (Shift+Right)
    pub fn move_right_selecting(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_right_selecting();
        }
    }

    /// Move to start with selection (Shift+Home)
    pub fn move_home_selecting(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_home_selecting();
        }
    }

    /// Move to end with selection (Shift+End)
    pub fn move_end_selecting(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_end_selecting();
        }
    }

    /// Move word left with selection (Ctrl+Shift+Left)
    pub fn move_word_left_selecting(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_word_left_selecting();
        }
    }

    /// Move word right with selection (Ctrl+Shift+Right)
    pub fn move_word_right_selecting(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.move_word_right_selecting();
        }
    }

    /// Select all text (Ctrl+A)
    pub fn select_all(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.select_all();
        }
    }

    /// Delete from cursor to end of word (Ctrl+Delete)
    pub fn delete_word_forward(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.delete_word_forward();
        }
    }

    /// Delete from start of word to cursor (Ctrl+Backspace)
    pub fn delete_word_backward(&mut self) {
        if let Some(editor) = &mut self.editor {
            editor.delete_word_backward();
        }
    }

    /// Get selected text for copy
    pub fn selected_text(&self) -> Option<String> {
        self.editor.as_ref().and_then(|e| e.selected_text())
    }

    /// Delete selection and return deleted text (for cut)
    pub fn delete_selection(&mut self) -> Option<String> {
        self.editor.as_mut().and_then(|e| e.delete_selection())
    }

    /// Insert string at cursor (for paste)
    pub fn insert_str(&mut self, text: &str) {
        if let Some(editor) = &mut self.editor {
            // Filter to only allow valid number characters
            let filtered: String = text
                .chars()
                .filter(|c| c.is_ascii_digit() || *c == '-' || *c == '.')
                .collect();
            editor.insert_str(&filtered);
        }
    }

    /// Get the display text (edit text when editing, value otherwise)
    pub fn display_text(&self) -> String {
        if let Some(editor) = &self.editor {
            editor.value()
        } else {
            self.value.to_string()
        }
    }

    /// Get cursor position when editing (column in single-line text)
    pub fn cursor_col(&self) -> usize {
        self.editor.as_ref().map(|e| e.cursor_col).unwrap_or(0)
    }

    /// Check if there's an active selection
    pub fn has_selection(&self) -> bool {
        self.editor
            .as_ref()
            .map(|e| e.has_selection())
            .unwrap_or(false)
    }

    /// Get selection range as (start, end) column positions
    pub fn selection_range(&self) -> Option<(usize, usize)> {
        self.editor.as_ref().and_then(|e| {
            e.selection_range()
                .map(|((_, start_col), (_, end_col))| (start_col, end_col))
        })
    }
}

/// Colors for the number input control
#[derive(Debug, Clone, Copy)]
pub struct NumberInputColors {
    /// Label color
    pub label: Color,
    /// Value text color
    pub value: Color,
    /// Border/bracket color
    pub border: Color,
    /// Button color (increment/decrement)
    pub button: Color,
    /// Focused highlight color
    pub focused: Color,
    /// Disabled color
    pub disabled: Color,
}

impl Default for NumberInputColors {
    fn default() -> Self {
        Self {
            label: Color::White,
            value: Color::Yellow,
            border: Color::Gray,
            button: Color::Cyan,
            focused: Color::Cyan,
            disabled: Color::DarkGray,
        }
    }
}

impl NumberInputColors {
    /// Create colors from theme
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            label: theme.editor_fg,
            value: theme.help_key_fg,
            border: theme.line_number_fg,
            button: theme.menu_active_fg,
            focused: theme.selection_bg,
            disabled: theme.line_number_fg,
        }
    }
}

/// Layout information returned after rendering for hit testing
#[derive(Debug, Clone, Copy, Default)]
pub struct NumberInputLayout {
    /// The value display area
    pub value_area: Rect,
    /// The decrement button area
    pub decrement_area: Rect,
    /// The increment button area
    pub increment_area: Rect,
    /// The full control area
    pub full_area: Rect,
}

impl NumberInputLayout {
    /// Check if a point is on the decrement button
    pub fn is_decrement(&self, x: u16, y: u16) -> bool {
        x >= self.decrement_area.x
            && x < self.decrement_area.x + self.decrement_area.width
            && y >= self.decrement_area.y
            && y < self.decrement_area.y + self.decrement_area.height
    }

    /// Check if a point is on the increment button
    pub fn is_increment(&self, x: u16, y: u16) -> bool {
        x >= self.increment_area.x
            && x < self.increment_area.x + self.increment_area.width
            && y >= self.increment_area.y
            && y < self.increment_area.y + self.increment_area.height
    }

    /// Check if a point is on the value area
    pub fn is_value(&self, x: u16, y: u16) -> bool {
        x >= self.value_area.x
            && x < self.value_area.x + self.value_area.width
            && y >= self.value_area.y
            && y < self.value_area.y + self.value_area.height
    }

    /// Check if a point is within any part of the control
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
    fn test_number_input_renders() {
        test_frame(40, 1, |frame, area| {
            let state = NumberInputState::new(42, "Count");
            let colors = NumberInputColors::default();
            let layout = render_number_input(frame, area, &state, &colors);

            assert!(layout.value_area.width > 0);
            assert!(layout.decrement_area.width > 0);
            assert!(layout.increment_area.width > 0);
        });
    }

    #[test]
    fn test_number_input_increment() {
        let mut state = NumberInputState::new(5, "Value");
        state.increment();
        assert_eq!(state.value, 6);
    }

    #[test]
    fn test_number_input_decrement() {
        let mut state = NumberInputState::new(5, "Value");
        state.decrement();
        assert_eq!(state.value, 4);
    }

    #[test]
    fn test_number_input_min_max() {
        let mut state = NumberInputState::new(5, "Value").with_min(0).with_max(10);

        state.set_value(-5);
        assert_eq!(state.value, 0);

        state.set_value(20);
        assert_eq!(state.value, 10);
    }

    #[test]
    fn test_number_input_step() {
        let mut state = NumberInputState::new(0, "Value").with_step(5);
        state.increment();
        assert_eq!(state.value, 5);
        state.increment();
        assert_eq!(state.value, 10);
    }

    #[test]
    fn test_number_input_disabled() {
        let mut state = NumberInputState::new(5, "Value").with_focus(FocusState::Disabled);
        state.increment();
        assert_eq!(state.value, 5);
    }

    #[test]
    fn test_number_input_hit_detection() {
        test_frame(40, 1, |frame, area| {
            let state = NumberInputState::new(42, "Count");
            let colors = NumberInputColors::default();
            let layout = render_number_input(frame, area, &state, &colors);

            let dec_x = layout.decrement_area.x;
            assert!(layout.is_decrement(dec_x, 0));
            assert!(!layout.is_increment(dec_x, 0));

            let inc_x = layout.increment_area.x;
            assert!(layout.is_increment(inc_x, 0));
            assert!(!layout.is_decrement(inc_x, 0));
        });
    }

    #[test]
    fn test_number_input_start_editing() {
        let mut state = NumberInputState::new(42, "Value");
        assert!(!state.editing());
        assert_eq!(state.display_text(), "42");

        state.start_editing();
        assert!(state.editing());
        assert_eq!(state.display_text(), "42");
    }

    #[test]
    fn test_number_input_cancel_editing() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        // After start_editing, text is selected so typing replaces it
        state.insert_char('1');
        state.insert_char('0');
        state.insert_char('0');
        assert_eq!(state.display_text(), "100");

        state.cancel_editing();
        assert!(!state.editing());
        assert_eq!(state.display_text(), "42");
        assert_eq!(state.value, 42);
    }

    #[test]
    fn test_number_input_confirm_editing() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        // Clear and type new value
        state.select_all();
        state.insert_str("100");

        state.confirm_editing();
        assert!(!state.editing());
        assert_eq!(state.value, 100);
    }

    #[test]
    fn test_number_input_confirm_invalid_resets() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        // Type invalid text - only valid chars will be inserted
        state.select_all();
        state.insert_str("abc"); // This will be filtered to empty

        state.confirm_editing();
        assert!(!state.editing());
        // Value remains unchanged since empty string can't be parsed
        assert_eq!(state.value, 42);
    }

    #[test]
    fn test_number_input_insert_char() {
        let mut state = NumberInputState::new(0, "Value");
        state.start_editing();
        // Clear and insert new chars
        state.select_all();
        state.insert_char('1');
        state.insert_char('2');
        state.insert_char('3');
        assert_eq!(state.display_text(), "123");

        let mut state2 = NumberInputState::new(0, "Value");
        state2.start_editing();
        state2.select_all();
        state2.insert_char('-');
        assert_eq!(state2.display_text(), "-");
        state2.insert_char('-'); // Multiple minus signs allowed by TextEdit
        state2.insert_char('5');
        assert_eq!(state2.display_text(), "--5");
    }

    #[test]
    fn test_number_input_backspace() {
        let mut state = NumberInputState::new(123, "Value");
        state.start_editing();
        assert_eq!(state.display_text(), "123");

        // After start_editing, text is selected. Move to end to deselect.
        state.move_end();

        state.backspace();
        assert_eq!(state.display_text(), "12");
        state.backspace();
        assert_eq!(state.display_text(), "1");
        state.backspace();
        assert_eq!(state.display_text(), "");
        state.backspace();
        assert_eq!(state.display_text(), "");
    }

    #[test]
    fn test_number_input_display_text() {
        let mut state = NumberInputState::new(42, "Value");

        assert_eq!(state.display_text(), "42");

        state.start_editing();
        assert_eq!(state.display_text(), "42");
        // After start_editing, text is selected. Move to end to append.
        state.move_end();
        state.insert_char('0');
        assert_eq!(state.display_text(), "420");
    }

    #[test]
    fn test_number_input_editing_respects_minmax() {
        let mut state = NumberInputState::new(50, "Value").with_min(0).with_max(100);
        state.start_editing();
        state.select_all();
        state.insert_str("200");

        state.confirm_editing();
        assert_eq!(state.value, 100);
    }

    #[test]
    fn test_number_input_disabled_no_editing() {
        let mut state = NumberInputState::new(42, "Value").with_focus(FocusState::Disabled);
        state.start_editing();
        assert!(!state.editing());
    }

    #[test]
    fn test_number_input_decimal_point() {
        let mut state = NumberInputState::new(0, "Value");
        state.start_editing();
        state.select_all();
        state.insert_str("0.25");
        assert_eq!(state.display_text(), "0.25");

        // Confirm won't parse as i64, so value stays at 0
        state.confirm_editing();
        assert_eq!(state.value, 0);
    }

    #[test]
    fn test_number_input_selection() {
        let mut state = NumberInputState::new(12345, "Value");
        state.start_editing();
        assert_eq!(state.display_text(), "12345");

        // Select all and replace
        state.select_all();
        assert!(state.has_selection());
        state.insert_char('9');
        assert_eq!(state.display_text(), "9");
    }

    #[test]
    fn test_number_input_cursor_navigation() {
        let mut state = NumberInputState::new(123, "Value");
        state.start_editing();
        // Cursor starts at end
        assert_eq!(state.cursor_col(), 3);

        state.move_left();
        assert_eq!(state.cursor_col(), 2);

        state.move_home();
        assert_eq!(state.cursor_col(), 0);

        state.move_end();
        assert_eq!(state.cursor_col(), 3);
    }
}
