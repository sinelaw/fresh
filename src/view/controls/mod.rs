//! Reusable form controls for settings and dialogs
//!
//! This module provides a set of form controls that can be used across
//! the application for settings screens, dialogs, and other interactive UI.
//!
//! ## Available Controls
//! - `Toggle` - Boolean on/off switch
//! - `NumberInput` - Numeric input with increment/decrement
//! - `Dropdown` - Selection from a list of options
//! - `TextInput` - Single-line text entry
//! - `TextList` - List of strings with add/remove
//! - `MapInput` - Key-value map with expandable entries
//! - `Button` - Clickable action button
//!
//! ## Pattern
//! Each control follows a consistent pattern:
//! - `*State` struct containing the control's data
//! - `*Colors` struct for theming
//! - `render_*` function that renders to a frame and returns hit areas

pub mod button;
pub mod dropdown;
pub mod keybinding_list;
pub mod map_input;
pub mod number_input;
pub mod text_input;
pub mod text_list;
pub mod toggle;

pub use button::{
    render_button, render_button_row, ButtonColors, ButtonEvent, ButtonLayout, ButtonState,
};
pub use dropdown::{
    render_dropdown, render_dropdown_aligned, DropdownColors, DropdownLayout, DropdownState,
};
pub use keybinding_list::{
    render_keybinding_list, KeybindingListColors, KeybindingListLayout, KeybindingListState,
};
pub use map_input::{render_map, MapColors, MapLayout, MapState};
pub use number_input::{
    render_number_input, render_number_input_aligned, NumberInputColors, NumberInputLayout,
    NumberInputState,
};
pub use text_input::{
    render_text_input, render_text_input_aligned, TextInputColors, TextInputLayout, TextInputState,
};
pub use text_list::{render_text_list, TextListColors, TextListLayout, TextListState};
pub use toggle::{
    render_toggle, render_toggle_aligned, ToggleColors, ToggleEvent, ToggleLayout, ToggleState,
};

use ratatui::style::Color;

/// Common colors shared across controls
#[derive(Debug, Clone, Copy)]
pub struct ControlColors {
    /// Background color
    pub bg: Color,
    /// Foreground/text color
    pub fg: Color,
    /// Border color
    pub border: Color,
    /// Focused/active accent color
    pub accent: Color,
    /// Disabled text color
    pub disabled: Color,
}

impl Default for ControlColors {
    fn default() -> Self {
        Self {
            bg: Color::Black,
            fg: Color::White,
            border: Color::DarkGray,
            accent: Color::Cyan,
            disabled: Color::DarkGray,
        }
    }
}

impl ControlColors {
    /// Create control colors from a theme
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            bg: theme.editor_bg,
            fg: theme.editor_fg,
            border: theme.split_separator_fg,
            accent: theme.selection_bg,
            disabled: theme.line_number_fg,
        }
    }
}

/// Focus state for controls
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FocusState {
    #[default]
    Normal,
    Focused,
    Hovered,
    Disabled,
}
