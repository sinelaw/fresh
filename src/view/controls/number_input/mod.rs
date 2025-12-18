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
    /// Whether currently editing the text value
    pub editing: bool,
    /// Text being edited (when editing=true)
    pub edit_text: String,
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
            editing: false,
            edit_text: String::new(),
        }
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
        self.editing = true;
        self.edit_text = self.value.to_string();
    }

    /// Cancel editing and restore original value
    pub fn cancel_editing(&mut self) {
        self.editing = false;
        self.edit_text.clear();
    }

    /// Confirm editing and apply the new value
    pub fn confirm_editing(&mut self) {
        if self.editing {
            if let Ok(new_value) = self.edit_text.parse::<i64>() {
                self.set_value(new_value);
            }
            self.editing = false;
            self.edit_text.clear();
        }
    }

    /// Insert a character while editing (only digits and minus sign)
    pub fn insert_char(&mut self, c: char) {
        if !self.editing {
            return;
        }
        // Allow digits and minus sign at the start
        if c.is_ascii_digit() || (c == '-' && self.edit_text.is_empty()) {
            self.edit_text.push(c);
        }
    }

    /// Backspace while editing
    pub fn backspace(&mut self) {
        if self.editing {
            self.edit_text.pop();
        }
    }

    /// Get the display text (edit text when editing, value otherwise)
    pub fn display_text(&self) -> String {
        if self.editing {
            self.edit_text.clone()
        } else {
            self.value.to_string()
        }
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
        assert!(!state.editing);
        assert!(state.edit_text.is_empty());

        state.start_editing();
        assert!(state.editing);
        assert_eq!(state.edit_text, "42");
    }

    #[test]
    fn test_number_input_cancel_editing() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        state.insert_char('1');
        state.insert_char('0');
        state.insert_char('0');
        assert_eq!(state.edit_text, "42100");

        state.cancel_editing();
        assert!(!state.editing);
        assert!(state.edit_text.is_empty());
        assert_eq!(state.value, 42);
    }

    #[test]
    fn test_number_input_confirm_editing() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        state.edit_text = "100".to_string();

        state.confirm_editing();
        assert!(!state.editing);
        assert!(state.edit_text.is_empty());
        assert_eq!(state.value, 100);
    }

    #[test]
    fn test_number_input_confirm_invalid_resets() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        state.edit_text = "abc".to_string();

        state.confirm_editing();
        assert!(!state.editing);
        assert_eq!(state.value, 42);
    }

    #[test]
    fn test_number_input_insert_char() {
        let mut state = NumberInputState::new(0, "Value");
        state.start_editing();
        state.edit_text.clear();

        state.insert_char('1');
        state.insert_char('2');
        state.insert_char('3');
        assert_eq!(state.edit_text, "123");

        let mut state2 = NumberInputState::new(0, "Value");
        state2.start_editing();
        state2.edit_text.clear();
        state2.insert_char('-');
        assert_eq!(state2.edit_text, "-");
        state2.insert_char('-');
        assert_eq!(state2.edit_text, "-");
        state2.insert_char('5');
        assert_eq!(state2.edit_text, "-5");
    }

    #[test]
    fn test_number_input_backspace() {
        let mut state = NumberInputState::new(123, "Value");
        state.start_editing();
        assert_eq!(state.edit_text, "123");

        state.backspace();
        assert_eq!(state.edit_text, "12");
        state.backspace();
        assert_eq!(state.edit_text, "1");
        state.backspace();
        assert_eq!(state.edit_text, "");
        state.backspace();
        assert_eq!(state.edit_text, "");
    }

    #[test]
    fn test_number_input_display_text() {
        let mut state = NumberInputState::new(42, "Value");

        assert_eq!(state.display_text(), "42");

        state.start_editing();
        assert_eq!(state.display_text(), "42");
        state.insert_char('0');
        assert_eq!(state.display_text(), "420");
    }

    #[test]
    fn test_number_input_editing_respects_minmax() {
        let mut state = NumberInputState::new(50, "Value").with_min(0).with_max(100);
        state.start_editing();
        state.edit_text = "200".to_string();

        state.confirm_editing();
        assert_eq!(state.value, 100);
    }

    #[test]
    fn test_number_input_disabled_no_editing() {
        let mut state = NumberInputState::new(42, "Value").with_focus(FocusState::Disabled);
        state.start_editing();
        assert!(!state.editing);
    }
}
