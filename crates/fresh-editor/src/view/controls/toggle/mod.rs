//! Toggle (checkbox) control for boolean values
//!
//! Renders as: `Label: [x]` or `Label: [ ]`
//!
//! This module provides a complete toggle component with:
//! - State management (`ToggleState`)
//! - Rendering (`render_toggle`, `render_toggle_aligned`)
//! - Input handling (`ToggleState::handle_mouse`, `handle_key`)
//! - Layout/hit testing (`ToggleLayout`)

mod input;
mod render;

use ratatui::layout::Rect;
use ratatui::style::Color;

pub use input::ToggleEvent;
pub use render::{render_toggle, render_toggle_aligned};

use super::FocusState;

/// State for a toggle control
#[derive(Debug, Clone)]
pub struct ToggleState {
    /// Current value
    pub checked: bool,
    /// Label displayed next to the toggle
    pub label: String,
    /// Focus state
    pub focus: FocusState,
}

impl ToggleState {
    /// Create a new toggle state
    pub fn new(checked: bool, label: impl Into<String>) -> Self {
        Self {
            checked,
            label: label.into(),
            focus: FocusState::Normal,
        }
    }

    /// Set the focus state
    pub fn with_focus(mut self, focus: FocusState) -> Self {
        self.focus = focus;
        self
    }

    /// Check if the toggle is enabled
    pub fn is_enabled(&self) -> bool {
        self.focus != FocusState::Disabled
    }

    /// Toggle the value
    pub fn toggle(&mut self) {
        if self.is_enabled() {
            self.checked = !self.checked;
        }
    }
}

/// Colors for the toggle control
#[derive(Debug, Clone, Copy)]
pub struct ToggleColors {
    /// Checkbox bracket color
    pub bracket: Color,
    /// Checkmark color when checked
    pub checkmark: Color,
    /// Label text color
    pub label: Color,
    /// Focused highlight color
    pub focused: Color,
    /// Disabled color
    pub disabled: Color,
}

impl Default for ToggleColors {
    fn default() -> Self {
        Self {
            bracket: Color::Gray,
            checkmark: Color::Green,
            label: Color::White,
            focused: Color::Cyan,
            disabled: Color::DarkGray,
        }
    }
}

impl ToggleColors {
    /// Create colors from theme
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            bracket: theme.line_number_fg,
            checkmark: theme.diagnostic_info_fg,
            label: theme.editor_fg,
            focused: theme.selection_bg,
            disabled: theme.line_number_fg,
        }
    }
}

/// Layout information returned after rendering for hit testing
#[derive(Debug, Clone, Copy, Default)]
pub struct ToggleLayout {
    /// The checkbox area (clickable)
    pub checkbox_area: Rect,
    /// The full control area including label
    pub full_area: Rect,
}

impl ToggleLayout {
    /// Check if a point is within the clickable area
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
    fn test_toggle_checked() {
        test_frame(20, 1, |frame, area| {
            let state = ToggleState::new(true, "Enable");
            let colors = ToggleColors::default();
            let layout = render_toggle(frame, area, &state, &colors);

            assert_eq!(layout.checkbox_area.width, 3);
            assert_eq!(layout.full_area.width, 11); // "Enable: [x]"
        });
    }

    #[test]
    fn test_toggle_unchecked() {
        test_frame(20, 1, |frame, area| {
            let state = ToggleState::new(false, "Enable");
            let colors = ToggleColors::default();
            let layout = render_toggle(frame, area, &state, &colors);

            assert_eq!(layout.checkbox_area.width, 3);
        });
    }

    #[test]
    fn test_toggle_click_detection() {
        test_frame(20, 1, |frame, area| {
            let state = ToggleState::new(true, "Enable");
            let colors = ToggleColors::default();
            let layout = render_toggle(frame, area, &state, &colors);

            // Click on checkbox
            assert!(layout.contains(0, 0));
            assert!(layout.contains(2, 0));

            // Click on label
            assert!(layout.contains(5, 0));

            // Click outside
            assert!(!layout.contains(15, 0));
        });
    }

    #[test]
    fn test_toggle_state_toggle() {
        let mut state = ToggleState::new(false, "Test");
        assert!(!state.checked);

        state.toggle();
        assert!(state.checked);

        state.toggle();
        assert!(!state.checked);
    }

    #[test]
    fn test_toggle_disabled_no_toggle() {
        let mut state = ToggleState::new(false, "Test").with_focus(FocusState::Disabled);
        state.toggle();
        assert!(!state.checked); // Should not change
    }

    #[test]
    fn test_toggle_narrow_area() {
        test_frame(2, 1, |frame, area| {
            let state = ToggleState::new(true, "Enable");
            let colors = ToggleColors::default();
            let layout = render_toggle(frame, area, &state, &colors);

            // Should still have some layout even if truncated
            assert!(layout.full_area.width <= area.width);
        });
    }
}
