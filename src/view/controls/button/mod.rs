//! Button control for triggering actions
//!
//! Renders as: `[ Button Text ]`
//!
//! This module provides a complete button component with:
//! - State management (`ButtonState`)
//! - Rendering (`render_button`, `render_button_row`)
//! - Input handling (`ButtonState::handle_mouse`)
//! - Layout/hit testing (`ButtonLayout`)

mod input;
mod render;

use ratatui::layout::Rect;
use ratatui::style::Color;

pub use input::ButtonEvent;
pub use render::{render_button, render_button_row};

use super::FocusState;

/// State for a button control
#[derive(Debug, Clone)]
pub struct ButtonState {
    /// Button label text
    pub label: String,
    /// Focus state
    pub focus: FocusState,
    /// Whether the button is currently pressed (for visual feedback)
    pub pressed: bool,
}

impl ButtonState {
    /// Create a new button state
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            label: label.into(),
            focus: FocusState::Normal,
            pressed: false,
        }
    }

    /// Set the focus state
    pub fn with_focus(mut self, focus: FocusState) -> Self {
        self.focus = focus;
        self
    }

    /// Check if the button can be activated
    pub fn is_enabled(&self) -> bool {
        self.focus != FocusState::Disabled
    }

    /// Set pressed state (for visual feedback)
    pub fn set_pressed(&mut self, pressed: bool) {
        self.pressed = pressed;
    }
}

/// Colors for the button control
#[derive(Debug, Clone, Copy)]
pub struct ButtonColors {
    /// Button text color
    pub text: Color,
    /// Border color
    pub border: Color,
    /// Background color (when pressed)
    pub pressed_bg: Color,
    /// Focused highlight color
    pub focused: Color,
    /// Hovered highlight color
    pub hovered: Color,
    /// Disabled color
    pub disabled: Color,
}

impl Default for ButtonColors {
    fn default() -> Self {
        Self {
            text: Color::White,
            border: Color::Gray,
            pressed_bg: Color::DarkGray,
            focused: Color::Cyan,
            hovered: Color::Blue,
            disabled: Color::DarkGray,
        }
    }
}

impl ButtonColors {
    /// Create colors from theme
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            text: theme.editor_fg,
            border: theme.line_number_fg,
            pressed_bg: theme.selection_bg,
            focused: theme.selection_bg,
            hovered: theme.menu_hover_bg,
            disabled: theme.line_number_fg,
        }
    }

    /// Create a primary/accent button style
    pub fn primary() -> Self {
        Self {
            text: Color::Black,
            border: Color::Cyan,
            pressed_bg: Color::LightCyan,
            focused: Color::Cyan,
            hovered: Color::LightCyan,
            disabled: Color::DarkGray,
        }
    }

    /// Create a danger/destructive button style
    pub fn danger() -> Self {
        Self {
            text: Color::White,
            border: Color::Red,
            pressed_bg: Color::LightRed,
            focused: Color::Red,
            hovered: Color::LightRed,
            disabled: Color::DarkGray,
        }
    }
}

/// Layout information returned after rendering for hit testing
#[derive(Debug, Clone, Copy, Default)]
pub struct ButtonLayout {
    /// The clickable button area
    pub button_area: Rect,
}

impl ButtonLayout {
    /// Check if a point is within the button
    pub fn contains(&self, x: u16, y: u16) -> bool {
        x >= self.button_area.x
            && x < self.button_area.x + self.button_area.width
            && y >= self.button_area.y
            && y < self.button_area.y + self.button_area.height
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
    fn test_button_renders() {
        test_frame(20, 1, |frame, area| {
            let state = ButtonState::new("OK");
            let colors = ButtonColors::default();
            let layout = render_button(frame, area, &state, &colors);

            assert_eq!(layout.button_area.width, 6); // "[ OK ]"
        });
    }

    #[test]
    fn test_button_hit_detection() {
        test_frame(20, 1, |frame, area| {
            let state = ButtonState::new("Click");
            let colors = ButtonColors::default();
            let layout = render_button(frame, area, &state, &colors);

            // Inside button
            assert!(layout.contains(0, 0));
            assert!(layout.contains(5, 0));

            // Outside button
            assert!(!layout.contains(15, 0));
        });
    }

    #[test]
    fn test_button_row() {
        test_frame(40, 1, |frame, area| {
            let ok = ButtonState::new("OK");
            let cancel = ButtonState::new("Cancel");
            let colors = ButtonColors::default();

            let layouts = render_button_row(frame, area, &[(&ok, &colors), (&cancel, &colors)], 2);

            assert_eq!(layouts.len(), 2);
            assert!(layouts[0].button_area.x < layouts[1].button_area.x);
        });
    }

    #[test]
    fn test_button_disabled() {
        let state = ButtonState::new("Save").with_focus(FocusState::Disabled);
        assert!(!state.is_enabled());
    }

    #[test]
    fn test_button_pressed_state() {
        let mut state = ButtonState::new("Submit");
        assert!(!state.pressed);

        state.set_pressed(true);
        assert!(state.pressed);
    }

    #[test]
    fn test_button_truncation() {
        test_frame(8, 1, |frame, area| {
            let state = ButtonState::new("Very Long Button Text");
            let colors = ButtonColors::default();
            let layout = render_button(frame, area, &state, &colors);

            // Button should be truncated to fit
            assert!(layout.button_area.width <= area.width);
        });
    }
}
