//! Pure theme types without I/O operations.
//!
//! This module contains all theme-related data structures that can be used
//! without filesystem access. This enables WASM compatibility and easier testing.

use ratatui::style::Color;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

pub const THEME_DARK: &str = "dark";
pub const THEME_LIGHT: &str = "light";
pub const THEME_HIGH_CONTRAST: &str = "high-contrast";
pub const THEME_NOSTALGIA: &str = "nostalgia";
pub const THEME_DRACULA: &str = "dracula";
pub const THEME_NORD: &str = "nord";
pub const THEME_SOLARIZED_DARK: &str = "solarized-dark";

/// A builtin theme with its name and embedded JSON content.
pub struct BuiltinTheme {
    pub name: &'static str,
    pub json: &'static str,
}

/// All builtin themes embedded at compile time.
pub const BUILTIN_THEMES: &[BuiltinTheme] = &[
    BuiltinTheme {
        name: THEME_DARK,
        json: include_str!("../../../themes/dark.json"),
    },
    BuiltinTheme {
        name: THEME_LIGHT,
        json: include_str!("../../../themes/light.json"),
    },
    BuiltinTheme {
        name: THEME_HIGH_CONTRAST,
        json: include_str!("../../../themes/high-contrast.json"),
    },
    BuiltinTheme {
        name: THEME_NOSTALGIA,
        json: include_str!("../../../themes/nostalgia.json"),
    },
    BuiltinTheme {
        name: THEME_DRACULA,
        json: include_str!("../../../themes/dracula.json"),
    },
    BuiltinTheme {
        name: THEME_NORD,
        json: include_str!("../../../themes/nord.json"),
    },
    BuiltinTheme {
        name: THEME_SOLARIZED_DARK,
        json: include_str!("../../../themes/solarized-dark.json"),
    },
];

/// Convert a ratatui Color to RGB values.
/// Returns None for Reset or Indexed colors.
pub fn color_to_rgb(color: Color) -> Option<(u8, u8, u8)> {
    match color {
        Color::Rgb(r, g, b) => Some((r, g, b)),
        Color::White => Some((255, 255, 255)),
        Color::Black => Some((0, 0, 0)),
        Color::Red => Some((205, 0, 0)),
        Color::Green => Some((0, 205, 0)),
        Color::Blue => Some((0, 0, 238)),
        Color::Yellow => Some((205, 205, 0)),
        Color::Magenta => Some((205, 0, 205)),
        Color::Cyan => Some((0, 205, 205)),
        Color::Gray => Some((229, 229, 229)),
        Color::DarkGray => Some((127, 127, 127)),
        Color::LightRed => Some((255, 0, 0)),
        Color::LightGreen => Some((0, 255, 0)),
        Color::LightBlue => Some((92, 92, 255)),
        Color::LightYellow => Some((255, 255, 0)),
        Color::LightMagenta => Some((255, 0, 255)),
        Color::LightCyan => Some((0, 255, 255)),
        Color::Reset | Color::Indexed(_) => None,
    }
}

/// Brighten a color by adding an amount to each RGB component.
/// Clamps values to 255.
pub fn brighten_color(color: Color, amount: u8) -> Color {
    if let Some((r, g, b)) = color_to_rgb(color) {
        Color::Rgb(
            r.saturating_add(amount),
            g.saturating_add(amount),
            b.saturating_add(amount),
        )
    } else {
        color
    }
}

/// Serializable color representation
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ColorDef {
    /// RGB color as [r, g, b]
    Rgb(u8, u8, u8),
    /// Named color
    Named(String),
}

impl From<ColorDef> for Color {
    fn from(def: ColorDef) -> Self {
        match def {
            ColorDef::Rgb(r, g, b) => Color::Rgb(r, g, b),
            ColorDef::Named(name) => match name.as_str() {
                "Black" => Color::Black,
                "Red" => Color::Red,
                "Green" => Color::Green,
                "Yellow" => Color::Yellow,
                "Blue" => Color::Blue,
                "Magenta" => Color::Magenta,
                "Cyan" => Color::Cyan,
                "Gray" => Color::Gray,
                "DarkGray" => Color::DarkGray,
                "LightRed" => Color::LightRed,
                "LightGreen" => Color::LightGreen,
                "LightYellow" => Color::LightYellow,
                "LightBlue" => Color::LightBlue,
                "LightMagenta" => Color::LightMagenta,
                "LightCyan" => Color::LightCyan,
                "White" => Color::White,
                // Default/Reset uses the terminal's default color (preserves transparency)
                "Default" | "Reset" => Color::Reset,
                _ => Color::White, // Default fallback
            },
        }
    }
}

impl From<Color> for ColorDef {
    fn from(color: Color) -> Self {
        match color {
            Color::Rgb(r, g, b) => ColorDef::Rgb(r, g, b),
            Color::White => ColorDef::Named("White".to_string()),
            Color::Black => ColorDef::Named("Black".to_string()),
            Color::Red => ColorDef::Named("Red".to_string()),
            Color::Green => ColorDef::Named("Green".to_string()),
            Color::Blue => ColorDef::Named("Blue".to_string()),
            Color::Yellow => ColorDef::Named("Yellow".to_string()),
            Color::Magenta => ColorDef::Named("Magenta".to_string()),
            Color::Cyan => ColorDef::Named("Cyan".to_string()),
            Color::Gray => ColorDef::Named("Gray".to_string()),
            Color::DarkGray => ColorDef::Named("DarkGray".to_string()),
            Color::LightRed => ColorDef::Named("LightRed".to_string()),
            Color::LightGreen => ColorDef::Named("LightGreen".to_string()),
            Color::LightBlue => ColorDef::Named("LightBlue".to_string()),
            Color::LightYellow => ColorDef::Named("LightYellow".to_string()),
            Color::LightMagenta => ColorDef::Named("LightMagenta".to_string()),
            Color::LightCyan => ColorDef::Named("LightCyan".to_string()),
            Color::Reset => ColorDef::Named("Default".to_string()),
            Color::Indexed(_) => {
                // Fallback for indexed colors
                if let Some((r, g, b)) = color_to_rgb(color) {
                    ColorDef::Rgb(r, g, b)
                } else {
                    ColorDef::Named("Default".to_string())
                }
            }
        }
    }
}

/// Serializable theme definition (matches JSON structure)
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ThemeFile {
    /// Theme name
    pub name: String,
    /// Editor area colors
    pub editor: EditorColors,
    /// UI element colors (tabs, menus, status bar, etc.)
    pub ui: UiColors,
    /// Search result highlighting colors
    pub search: SearchColors,
    /// LSP diagnostic colors (errors, warnings, etc.)
    pub diagnostic: DiagnosticColors,
    /// Syntax highlighting colors
    pub syntax: SyntaxColors,
}

/// Editor area colors
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct EditorColors {
    /// Editor background color
    #[serde(default = "default_editor_bg")]
    pub bg: ColorDef,
    /// Default text color
    #[serde(default = "default_editor_fg")]
    pub fg: ColorDef,
    /// Cursor color
    #[serde(default = "default_cursor")]
    pub cursor: ColorDef,
    /// Cursor color in unfocused splits
    #[serde(default = "default_inactive_cursor")]
    pub inactive_cursor: ColorDef,
    /// Selected text background
    #[serde(default = "default_selection_bg")]
    pub selection_bg: ColorDef,
    /// Background of the line containing cursor
    #[serde(default = "default_current_line_bg")]
    pub current_line_bg: ColorDef,
    /// Line number text color
    #[serde(default = "default_line_number_fg")]
    pub line_number_fg: ColorDef,
    /// Line number gutter background
    #[serde(default = "default_line_number_bg")]
    pub line_number_bg: ColorDef,
    /// Diff added line background
    #[serde(default = "default_diff_add_bg")]
    pub diff_add_bg: ColorDef,
    /// Diff removed line background
    #[serde(default = "default_diff_remove_bg")]
    pub diff_remove_bg: ColorDef,
    /// Diff modified line background
    #[serde(default = "default_diff_modify_bg")]
    pub diff_modify_bg: ColorDef,
}

// Default editor colors (for minimal themes)
fn default_editor_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}
fn default_editor_fg() -> ColorDef {
    ColorDef::Rgb(212, 212, 212)
}
fn default_cursor() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}
fn default_inactive_cursor() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}
fn default_selection_bg() -> ColorDef {
    ColorDef::Rgb(38, 79, 120)
}
fn default_current_line_bg() -> ColorDef {
    ColorDef::Rgb(40, 40, 40)
}
fn default_line_number_fg() -> ColorDef {
    ColorDef::Rgb(100, 100, 100)
}
fn default_line_number_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}
fn default_diff_add_bg() -> ColorDef {
    ColorDef::Rgb(35, 60, 35) // Dark green
}
fn default_diff_remove_bg() -> ColorDef {
    ColorDef::Rgb(70, 35, 35) // Dark red
}
fn default_diff_modify_bg() -> ColorDef {
    ColorDef::Rgb(40, 38, 30) // Very subtle yellow tint, close to dark bg
}

/// UI element colors (tabs, menus, status bar, etc.)
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct UiColors {
    /// Active tab text color
    #[serde(default = "default_tab_active_fg")]
    pub tab_active_fg: ColorDef,
    /// Active tab background color
    #[serde(default = "default_tab_active_bg")]
    pub tab_active_bg: ColorDef,
    /// Inactive tab text color
    #[serde(default = "default_tab_inactive_fg")]
    pub tab_inactive_fg: ColorDef,
    /// Inactive tab background color
    #[serde(default = "default_tab_inactive_bg")]
    pub tab_inactive_bg: ColorDef,
    /// Tab bar separator color
    #[serde(default = "default_tab_separator_bg")]
    pub tab_separator_bg: ColorDef,
    /// Tab close button hover color
    #[serde(default = "default_tab_close_hover_fg")]
    pub tab_close_hover_fg: ColorDef,
    /// Tab hover background color
    #[serde(default = "default_tab_hover_bg")]
    pub tab_hover_bg: ColorDef,
    /// Menu bar background
    #[serde(default = "default_menu_bg")]
    pub menu_bg: ColorDef,
    /// Menu bar text color
    #[serde(default = "default_menu_fg")]
    pub menu_fg: ColorDef,
    /// Active menu item background
    #[serde(default = "default_menu_active_bg")]
    pub menu_active_bg: ColorDef,
    /// Active menu item text color
    #[serde(default = "default_menu_active_fg")]
    pub menu_active_fg: ColorDef,
    /// Dropdown menu background
    #[serde(default = "default_menu_dropdown_bg")]
    pub menu_dropdown_bg: ColorDef,
    /// Dropdown menu text color
    #[serde(default = "default_menu_dropdown_fg")]
    pub menu_dropdown_fg: ColorDef,
    /// Highlighted menu item background
    #[serde(default = "default_menu_highlight_bg")]
    pub menu_highlight_bg: ColorDef,
    /// Highlighted menu item text color
    #[serde(default = "default_menu_highlight_fg")]
    pub menu_highlight_fg: ColorDef,
    /// Menu border color
    #[serde(default = "default_menu_border_fg")]
    pub menu_border_fg: ColorDef,
    /// Menu separator line color
    #[serde(default = "default_menu_separator_fg")]
    pub menu_separator_fg: ColorDef,
    /// Menu item hover background
    #[serde(default = "default_menu_hover_bg")]
    pub menu_hover_bg: ColorDef,
    /// Menu item hover text color
    #[serde(default = "default_menu_hover_fg")]
    pub menu_hover_fg: ColorDef,
    /// Disabled menu item text color
    #[serde(default = "default_menu_disabled_fg")]
    pub menu_disabled_fg: ColorDef,
    /// Disabled menu item background
    #[serde(default = "default_menu_disabled_bg")]
    pub menu_disabled_bg: ColorDef,
    /// Status bar text color
    #[serde(default = "default_status_bar_fg")]
    pub status_bar_fg: ColorDef,
    /// Status bar background color
    #[serde(default = "default_status_bar_bg")]
    pub status_bar_bg: ColorDef,
    /// Command prompt text color
    #[serde(default = "default_prompt_fg")]
    pub prompt_fg: ColorDef,
    /// Command prompt background
    #[serde(default = "default_prompt_bg")]
    pub prompt_bg: ColorDef,
    /// Prompt selected text color
    #[serde(default = "default_prompt_selection_fg")]
    pub prompt_selection_fg: ColorDef,
    /// Prompt selection background
    #[serde(default = "default_prompt_selection_bg")]
    pub prompt_selection_bg: ColorDef,
    /// Popup window border color
    #[serde(default = "default_popup_border_fg")]
    pub popup_border_fg: ColorDef,
    /// Popup window background
    #[serde(default = "default_popup_bg")]
    pub popup_bg: ColorDef,
    /// Popup selected item background
    #[serde(default = "default_popup_selection_bg")]
    pub popup_selection_bg: ColorDef,
    /// Popup window text color
    #[serde(default = "default_popup_text_fg")]
    pub popup_text_fg: ColorDef,
    /// Autocomplete suggestion background
    #[serde(default = "default_suggestion_bg")]
    pub suggestion_bg: ColorDef,
    /// Selected suggestion background
    #[serde(default = "default_suggestion_selected_bg")]
    pub suggestion_selected_bg: ColorDef,
    /// Help panel background
    #[serde(default = "default_help_bg")]
    pub help_bg: ColorDef,
    /// Help panel text color
    #[serde(default = "default_help_fg")]
    pub help_fg: ColorDef,
    /// Help keybinding text color
    #[serde(default = "default_help_key_fg")]
    pub help_key_fg: ColorDef,
    /// Help panel separator color
    #[serde(default = "default_help_separator_fg")]
    pub help_separator_fg: ColorDef,
    /// Help indicator text color
    #[serde(default = "default_help_indicator_fg")]
    pub help_indicator_fg: ColorDef,
    /// Help indicator background
    #[serde(default = "default_help_indicator_bg")]
    pub help_indicator_bg: ColorDef,
    /// Inline code block background
    #[serde(default = "default_inline_code_bg")]
    pub inline_code_bg: ColorDef,
    /// Split pane separator color
    #[serde(default = "default_split_separator_fg")]
    pub split_separator_fg: ColorDef,
    /// Split separator hover color
    #[serde(default = "default_split_separator_hover_fg")]
    pub split_separator_hover_fg: ColorDef,
    /// Scrollbar track color
    #[serde(default = "default_scrollbar_track_fg")]
    pub scrollbar_track_fg: ColorDef,
    /// Scrollbar thumb color
    #[serde(default = "default_scrollbar_thumb_fg")]
    pub scrollbar_thumb_fg: ColorDef,
    /// Scrollbar track hover color
    #[serde(default = "default_scrollbar_track_hover_fg")]
    pub scrollbar_track_hover_fg: ColorDef,
    /// Scrollbar thumb hover color
    #[serde(default = "default_scrollbar_thumb_hover_fg")]
    pub scrollbar_thumb_hover_fg: ColorDef,
    /// Compose mode margin background
    #[serde(default = "default_compose_margin_bg")]
    pub compose_margin_bg: ColorDef,
    /// Word under cursor highlight
    #[serde(default = "default_semantic_highlight_bg")]
    pub semantic_highlight_bg: ColorDef,
    /// Embedded terminal background (use Default for transparency)
    #[serde(default = "default_terminal_bg")]
    pub terminal_bg: ColorDef,
    /// Embedded terminal default text color
    #[serde(default = "default_terminal_fg")]
    pub terminal_fg: ColorDef,
    /// Warning indicator background in status bar
    #[serde(default = "default_status_warning_indicator_bg")]
    pub status_warning_indicator_bg: ColorDef,
    /// Warning indicator text color in status bar
    #[serde(default = "default_status_warning_indicator_fg")]
    pub status_warning_indicator_fg: ColorDef,
    /// Error indicator background in status bar
    #[serde(default = "default_status_error_indicator_bg")]
    pub status_error_indicator_bg: ColorDef,
    /// Error indicator text color in status bar
    #[serde(default = "default_status_error_indicator_fg")]
    pub status_error_indicator_fg: ColorDef,
    /// Warning indicator hover background
    #[serde(default = "default_status_warning_indicator_hover_bg")]
    pub status_warning_indicator_hover_bg: ColorDef,
    /// Warning indicator hover text color
    #[serde(default = "default_status_warning_indicator_hover_fg")]
    pub status_warning_indicator_hover_fg: ColorDef,
    /// Error indicator hover background
    #[serde(default = "default_status_error_indicator_hover_bg")]
    pub status_error_indicator_hover_bg: ColorDef,
    /// Error indicator hover text color
    #[serde(default = "default_status_error_indicator_hover_fg")]
    pub status_error_indicator_hover_fg: ColorDef,
    /// Tab drop zone background during drag
    #[serde(default = "default_tab_drop_zone_bg")]
    pub tab_drop_zone_bg: ColorDef,
    /// Tab drop zone border during drag
    #[serde(default = "default_tab_drop_zone_border")]
    pub tab_drop_zone_border: ColorDef,
}

// Default tab close hover color (for backward compatibility with existing themes)
// Default tab colors (for minimal themes)
fn default_tab_active_fg() -> ColorDef {
    ColorDef::Named("Yellow".to_string())
}
fn default_tab_active_bg() -> ColorDef {
    ColorDef::Named("Blue".to_string())
}
fn default_tab_inactive_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_tab_inactive_bg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}
fn default_tab_separator_bg() -> ColorDef {
    ColorDef::Named("Black".to_string())
}
fn default_tab_close_hover_fg() -> ColorDef {
    ColorDef::Rgb(255, 100, 100) // Red-ish color for close button hover
}
fn default_tab_hover_bg() -> ColorDef {
    ColorDef::Rgb(70, 70, 75) // Slightly lighter than inactive tab bg for hover
}

// Default menu colors (for backward compatibility with existing themes)
fn default_menu_bg() -> ColorDef {
    ColorDef::Rgb(60, 60, 65)
}
fn default_menu_fg() -> ColorDef {
    ColorDef::Rgb(220, 220, 220)
}
fn default_menu_active_bg() -> ColorDef {
    ColorDef::Rgb(60, 60, 60)
}
fn default_menu_active_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}
fn default_menu_dropdown_bg() -> ColorDef {
    ColorDef::Rgb(50, 50, 50)
}
fn default_menu_dropdown_fg() -> ColorDef {
    ColorDef::Rgb(220, 220, 220)
}
fn default_menu_highlight_bg() -> ColorDef {
    ColorDef::Rgb(70, 130, 180)
}
fn default_menu_highlight_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}
fn default_menu_border_fg() -> ColorDef {
    ColorDef::Rgb(100, 100, 100)
}
fn default_menu_separator_fg() -> ColorDef {
    ColorDef::Rgb(80, 80, 80)
}
fn default_menu_hover_bg() -> ColorDef {
    ColorDef::Rgb(55, 55, 55)
}
fn default_menu_hover_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}
fn default_menu_disabled_fg() -> ColorDef {
    ColorDef::Rgb(100, 100, 100) // Gray for disabled items
}
fn default_menu_disabled_bg() -> ColorDef {
    ColorDef::Rgb(50, 50, 50) // Same as dropdown bg
}
// Default status bar colors
fn default_status_bar_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_status_bar_bg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}

// Default prompt colors
fn default_prompt_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_prompt_bg() -> ColorDef {
    ColorDef::Named("Black".to_string())
}
fn default_prompt_selection_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_prompt_selection_bg() -> ColorDef {
    ColorDef::Rgb(58, 79, 120)
}

// Default popup colors
fn default_popup_border_fg() -> ColorDef {
    ColorDef::Named("Gray".to_string())
}
fn default_popup_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}
fn default_popup_selection_bg() -> ColorDef {
    ColorDef::Rgb(58, 79, 120)
}
fn default_popup_text_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}

// Default suggestion colors
fn default_suggestion_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}
fn default_suggestion_selected_bg() -> ColorDef {
    ColorDef::Rgb(58, 79, 120)
}

// Default help colors
fn default_help_bg() -> ColorDef {
    ColorDef::Named("Black".to_string())
}
fn default_help_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_help_key_fg() -> ColorDef {
    ColorDef::Named("Cyan".to_string())
}
fn default_help_separator_fg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}
fn default_help_indicator_fg() -> ColorDef {
    ColorDef::Named("Red".to_string())
}
fn default_help_indicator_bg() -> ColorDef {
    ColorDef::Named("Black".to_string())
}

fn default_inline_code_bg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}

// Default split separator colors
fn default_split_separator_fg() -> ColorDef {
    ColorDef::Rgb(100, 100, 100)
}
fn default_split_separator_hover_fg() -> ColorDef {
    ColorDef::Rgb(100, 149, 237) // Cornflower blue for visibility
}
fn default_scrollbar_track_fg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}
fn default_scrollbar_thumb_fg() -> ColorDef {
    ColorDef::Named("Gray".to_string())
}
fn default_scrollbar_track_hover_fg() -> ColorDef {
    ColorDef::Named("Gray".to_string())
}
fn default_scrollbar_thumb_hover_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_compose_margin_bg() -> ColorDef {
    ColorDef::Rgb(18, 18, 18) // Darker than editor_bg for "desk" effect
}
fn default_semantic_highlight_bg() -> ColorDef {
    ColorDef::Rgb(60, 60, 80) // Subtle dark highlight for word occurrences
}
fn default_terminal_bg() -> ColorDef {
    ColorDef::Named("Default".to_string()) // Use terminal's default background (preserves transparency)
}
fn default_terminal_fg() -> ColorDef {
    ColorDef::Named("Default".to_string()) // Use terminal's default foreground
}
fn default_status_warning_indicator_bg() -> ColorDef {
    ColorDef::Rgb(181, 137, 0) // Solarized yellow/amber - noticeable but not harsh
}
fn default_status_warning_indicator_fg() -> ColorDef {
    ColorDef::Rgb(0, 0, 0) // Black text on amber background
}
fn default_status_error_indicator_bg() -> ColorDef {
    ColorDef::Rgb(220, 50, 47) // Solarized red - clearly an error
}
fn default_status_error_indicator_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255) // White text on red background
}
fn default_status_warning_indicator_hover_bg() -> ColorDef {
    ColorDef::Rgb(211, 167, 30) // Lighter amber for hover
}
fn default_status_warning_indicator_hover_fg() -> ColorDef {
    ColorDef::Rgb(0, 0, 0) // Black text on hover
}
fn default_status_error_indicator_hover_bg() -> ColorDef {
    ColorDef::Rgb(250, 80, 77) // Lighter red for hover
}
fn default_status_error_indicator_hover_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255) // White text on hover
}
fn default_tab_drop_zone_bg() -> ColorDef {
    ColorDef::Rgb(70, 130, 180) // Steel blue with transparency effect
}
fn default_tab_drop_zone_border() -> ColorDef {
    ColorDef::Rgb(100, 149, 237) // Cornflower blue for border
}

/// Search result highlighting colors
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SearchColors {
    /// Search match background color
    #[serde(default = "default_search_match_bg")]
    pub match_bg: ColorDef,
    /// Search match text color
    #[serde(default = "default_search_match_fg")]
    pub match_fg: ColorDef,
}

// Default search colors
fn default_search_match_bg() -> ColorDef {
    ColorDef::Rgb(100, 100, 20)
}
fn default_search_match_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}

/// LSP diagnostic colors (errors, warnings, etc.)
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct DiagnosticColors {
    /// Error message text color
    #[serde(default = "default_diagnostic_error_fg")]
    pub error_fg: ColorDef,
    /// Error highlight background
    #[serde(default = "default_diagnostic_error_bg")]
    pub error_bg: ColorDef,
    /// Warning message text color
    #[serde(default = "default_diagnostic_warning_fg")]
    pub warning_fg: ColorDef,
    /// Warning highlight background
    #[serde(default = "default_diagnostic_warning_bg")]
    pub warning_bg: ColorDef,
    /// Info message text color
    #[serde(default = "default_diagnostic_info_fg")]
    pub info_fg: ColorDef,
    /// Info highlight background
    #[serde(default = "default_diagnostic_info_bg")]
    pub info_bg: ColorDef,
    /// Hint message text color
    #[serde(default = "default_diagnostic_hint_fg")]
    pub hint_fg: ColorDef,
    /// Hint highlight background
    #[serde(default = "default_diagnostic_hint_bg")]
    pub hint_bg: ColorDef,
}

// Default diagnostic colors
fn default_diagnostic_error_fg() -> ColorDef {
    ColorDef::Named("Red".to_string())
}
fn default_diagnostic_error_bg() -> ColorDef {
    ColorDef::Rgb(60, 20, 20)
}
fn default_diagnostic_warning_fg() -> ColorDef {
    ColorDef::Named("Yellow".to_string())
}
fn default_diagnostic_warning_bg() -> ColorDef {
    ColorDef::Rgb(60, 50, 0)
}
fn default_diagnostic_info_fg() -> ColorDef {
    ColorDef::Named("Blue".to_string())
}
fn default_diagnostic_info_bg() -> ColorDef {
    ColorDef::Rgb(0, 30, 60)
}
fn default_diagnostic_hint_fg() -> ColorDef {
    ColorDef::Named("Gray".to_string())
}
fn default_diagnostic_hint_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}

/// Syntax highlighting colors
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SyntaxColors {
    /// Language keywords (if, for, fn, etc.)
    #[serde(default = "default_syntax_keyword")]
    pub keyword: ColorDef,
    /// String literals
    #[serde(default = "default_syntax_string")]
    pub string: ColorDef,
    /// Code comments
    #[serde(default = "default_syntax_comment")]
    pub comment: ColorDef,
    /// Function names
    #[serde(default = "default_syntax_function")]
    pub function: ColorDef,
    /// Type names
    #[serde(rename = "type", default = "default_syntax_type")]
    pub type_: ColorDef,
    /// Variable names
    #[serde(default = "default_syntax_variable")]
    pub variable: ColorDef,
    /// Constants and literals
    #[serde(default = "default_syntax_constant")]
    pub constant: ColorDef,
    /// Operators (+, -, =, etc.)
    #[serde(default = "default_syntax_operator")]
    pub operator: ColorDef,
}

// Default syntax colors (VSCode Dark+ inspired)
fn default_syntax_keyword() -> ColorDef {
    ColorDef::Rgb(86, 156, 214)
}
fn default_syntax_string() -> ColorDef {
    ColorDef::Rgb(206, 145, 120)
}
fn default_syntax_comment() -> ColorDef {
    ColorDef::Rgb(106, 153, 85)
}
fn default_syntax_function() -> ColorDef {
    ColorDef::Rgb(220, 220, 170)
}
fn default_syntax_type() -> ColorDef {
    ColorDef::Rgb(78, 201, 176)
}
fn default_syntax_variable() -> ColorDef {
    ColorDef::Rgb(156, 220, 254)
}
fn default_syntax_constant() -> ColorDef {
    ColorDef::Rgb(79, 193, 255)
}
fn default_syntax_operator() -> ColorDef {
    ColorDef::Rgb(212, 212, 212)
}

/// Comprehensive theme structure with all UI colors
#[derive(Debug, Clone)]
pub struct Theme {
    /// Theme name (e.g., "dark", "light", "high-contrast")
    pub name: String,

    // Editor colors
    pub editor_bg: Color,
    pub editor_fg: Color,
    pub cursor: Color,
    pub inactive_cursor: Color,
    pub selection_bg: Color,
    pub current_line_bg: Color,
    pub line_number_fg: Color,
    pub line_number_bg: Color,

    // Diff highlighting colors
    pub diff_add_bg: Color,
    pub diff_remove_bg: Color,
    pub diff_modify_bg: Color,
    /// Brighter background for inline diff highlighting on added content
    pub diff_add_highlight_bg: Color,
    /// Brighter background for inline diff highlighting on removed content
    pub diff_remove_highlight_bg: Color,

    // UI element colors
    pub tab_active_fg: Color,
    pub tab_active_bg: Color,
    pub tab_inactive_fg: Color,
    pub tab_inactive_bg: Color,
    pub tab_separator_bg: Color,
    pub tab_close_hover_fg: Color,
    pub tab_hover_bg: Color,

    // Menu bar colors
    pub menu_bg: Color,
    pub menu_fg: Color,
    pub menu_active_bg: Color,
    pub menu_active_fg: Color,
    pub menu_dropdown_bg: Color,
    pub menu_dropdown_fg: Color,
    pub menu_highlight_bg: Color,
    pub menu_highlight_fg: Color,
    pub menu_border_fg: Color,
    pub menu_separator_fg: Color,
    pub menu_hover_bg: Color,
    pub menu_hover_fg: Color,
    pub menu_disabled_fg: Color,
    pub menu_disabled_bg: Color,

    pub status_bar_fg: Color,
    pub status_bar_bg: Color,
    pub prompt_fg: Color,
    pub prompt_bg: Color,
    pub prompt_selection_fg: Color,
    pub prompt_selection_bg: Color,

    pub popup_border_fg: Color,
    pub popup_bg: Color,
    pub popup_selection_bg: Color,
    pub popup_text_fg: Color,

    pub suggestion_bg: Color,
    pub suggestion_selected_bg: Color,

    pub help_bg: Color,
    pub help_fg: Color,
    pub help_key_fg: Color,
    pub help_separator_fg: Color,

    pub help_indicator_fg: Color,
    pub help_indicator_bg: Color,

    /// Background color for inline code in help popups
    pub inline_code_bg: Color,

    pub split_separator_fg: Color,
    pub split_separator_hover_fg: Color,

    // Scrollbar colors
    pub scrollbar_track_fg: Color,
    pub scrollbar_thumb_fg: Color,
    pub scrollbar_track_hover_fg: Color,
    pub scrollbar_thumb_hover_fg: Color,

    // Compose mode colors
    pub compose_margin_bg: Color,

    // Semantic highlighting (word under cursor)
    pub semantic_highlight_bg: Color,

    // Terminal colors (for embedded terminal buffers)
    pub terminal_bg: Color,
    pub terminal_fg: Color,

    // Status bar warning/error indicator colors
    pub status_warning_indicator_bg: Color,
    pub status_warning_indicator_fg: Color,
    pub status_error_indicator_bg: Color,
    pub status_error_indicator_fg: Color,
    pub status_warning_indicator_hover_bg: Color,
    pub status_warning_indicator_hover_fg: Color,
    pub status_error_indicator_hover_bg: Color,
    pub status_error_indicator_hover_fg: Color,

    // Tab drag-and-drop colors
    pub tab_drop_zone_bg: Color,
    pub tab_drop_zone_border: Color,

    // Search colors
    pub search_match_bg: Color,
    pub search_match_fg: Color,

    // Diagnostic colors
    pub diagnostic_error_fg: Color,
    pub diagnostic_error_bg: Color,
    pub diagnostic_warning_fg: Color,
    pub diagnostic_warning_bg: Color,
    pub diagnostic_info_fg: Color,
    pub diagnostic_info_bg: Color,
    pub diagnostic_hint_fg: Color,
    pub diagnostic_hint_bg: Color,

    // Syntax highlighting colors
    pub syntax_keyword: Color,
    pub syntax_string: Color,
    pub syntax_comment: Color,
    pub syntax_function: Color,
    pub syntax_type: Color,
    pub syntax_variable: Color,
    pub syntax_constant: Color,
    pub syntax_operator: Color,
}

impl From<ThemeFile> for Theme {
    fn from(file: ThemeFile) -> Self {
        Self {
            name: file.name,
            editor_bg: file.editor.bg.into(),
            editor_fg: file.editor.fg.into(),
            cursor: file.editor.cursor.into(),
            inactive_cursor: file.editor.inactive_cursor.into(),
            selection_bg: file.editor.selection_bg.into(),
            current_line_bg: file.editor.current_line_bg.into(),
            line_number_fg: file.editor.line_number_fg.into(),
            line_number_bg: file.editor.line_number_bg.into(),
            diff_add_bg: file.editor.diff_add_bg.clone().into(),
            diff_remove_bg: file.editor.diff_remove_bg.clone().into(),
            diff_modify_bg: file.editor.diff_modify_bg.into(),
            // Compute brighter highlight colors from base diff colors
            diff_add_highlight_bg: brighten_color(file.editor.diff_add_bg.into(), 40),
            diff_remove_highlight_bg: brighten_color(file.editor.diff_remove_bg.into(), 40),
            tab_active_fg: file.ui.tab_active_fg.into(),
            tab_active_bg: file.ui.tab_active_bg.into(),
            tab_inactive_fg: file.ui.tab_inactive_fg.into(),
            tab_inactive_bg: file.ui.tab_inactive_bg.into(),
            tab_separator_bg: file.ui.tab_separator_bg.into(),
            tab_close_hover_fg: file.ui.tab_close_hover_fg.into(),
            tab_hover_bg: file.ui.tab_hover_bg.into(),
            menu_bg: file.ui.menu_bg.into(),
            menu_fg: file.ui.menu_fg.into(),
            menu_active_bg: file.ui.menu_active_bg.into(),
            menu_active_fg: file.ui.menu_active_fg.into(),
            menu_dropdown_bg: file.ui.menu_dropdown_bg.into(),
            menu_dropdown_fg: file.ui.menu_dropdown_fg.into(),
            menu_highlight_bg: file.ui.menu_highlight_bg.into(),
            menu_highlight_fg: file.ui.menu_highlight_fg.into(),
            menu_border_fg: file.ui.menu_border_fg.into(),
            menu_separator_fg: file.ui.menu_separator_fg.into(),
            menu_hover_bg: file.ui.menu_hover_bg.into(),
            menu_hover_fg: file.ui.menu_hover_fg.into(),
            menu_disabled_fg: file.ui.menu_disabled_fg.into(),
            menu_disabled_bg: file.ui.menu_disabled_bg.into(),
            status_bar_fg: file.ui.status_bar_fg.into(),
            status_bar_bg: file.ui.status_bar_bg.into(),
            prompt_fg: file.ui.prompt_fg.into(),
            prompt_bg: file.ui.prompt_bg.into(),
            prompt_selection_fg: file.ui.prompt_selection_fg.into(),
            prompt_selection_bg: file.ui.prompt_selection_bg.into(),
            popup_border_fg: file.ui.popup_border_fg.into(),
            popup_bg: file.ui.popup_bg.into(),
            popup_selection_bg: file.ui.popup_selection_bg.into(),
            popup_text_fg: file.ui.popup_text_fg.into(),
            suggestion_bg: file.ui.suggestion_bg.into(),
            suggestion_selected_bg: file.ui.suggestion_selected_bg.into(),
            help_bg: file.ui.help_bg.into(),
            help_fg: file.ui.help_fg.into(),
            help_key_fg: file.ui.help_key_fg.into(),
            help_separator_fg: file.ui.help_separator_fg.into(),
            help_indicator_fg: file.ui.help_indicator_fg.into(),
            help_indicator_bg: file.ui.help_indicator_bg.into(),
            inline_code_bg: file.ui.inline_code_bg.into(),
            split_separator_fg: file.ui.split_separator_fg.into(),
            split_separator_hover_fg: file.ui.split_separator_hover_fg.into(),
            scrollbar_track_fg: file.ui.scrollbar_track_fg.into(),
            scrollbar_thumb_fg: file.ui.scrollbar_thumb_fg.into(),
            scrollbar_track_hover_fg: file.ui.scrollbar_track_hover_fg.into(),
            scrollbar_thumb_hover_fg: file.ui.scrollbar_thumb_hover_fg.into(),
            compose_margin_bg: file.ui.compose_margin_bg.into(),
            semantic_highlight_bg: file.ui.semantic_highlight_bg.into(),
            terminal_bg: file.ui.terminal_bg.into(),
            terminal_fg: file.ui.terminal_fg.into(),
            status_warning_indicator_bg: file.ui.status_warning_indicator_bg.into(),
            status_warning_indicator_fg: file.ui.status_warning_indicator_fg.into(),
            status_error_indicator_bg: file.ui.status_error_indicator_bg.into(),
            status_error_indicator_fg: file.ui.status_error_indicator_fg.into(),
            status_warning_indicator_hover_bg: file.ui.status_warning_indicator_hover_bg.into(),
            status_warning_indicator_hover_fg: file.ui.status_warning_indicator_hover_fg.into(),
            status_error_indicator_hover_bg: file.ui.status_error_indicator_hover_bg.into(),
            status_error_indicator_hover_fg: file.ui.status_error_indicator_hover_fg.into(),
            tab_drop_zone_bg: file.ui.tab_drop_zone_bg.into(),
            tab_drop_zone_border: file.ui.tab_drop_zone_border.into(),
            search_match_bg: file.search.match_bg.into(),
            search_match_fg: file.search.match_fg.into(),
            diagnostic_error_fg: file.diagnostic.error_fg.into(),
            diagnostic_error_bg: file.diagnostic.error_bg.into(),
            diagnostic_warning_fg: file.diagnostic.warning_fg.into(),
            diagnostic_warning_bg: file.diagnostic.warning_bg.into(),
            diagnostic_info_fg: file.diagnostic.info_fg.into(),
            diagnostic_info_bg: file.diagnostic.info_bg.into(),
            diagnostic_hint_fg: file.diagnostic.hint_fg.into(),
            diagnostic_hint_bg: file.diagnostic.hint_bg.into(),
            syntax_keyword: file.syntax.keyword.into(),
            syntax_string: file.syntax.string.into(),
            syntax_comment: file.syntax.comment.into(),
            syntax_function: file.syntax.function.into(),
            syntax_type: file.syntax.type_.into(),
            syntax_variable: file.syntax.variable.into(),
            syntax_constant: file.syntax.constant.into(),
            syntax_operator: file.syntax.operator.into(),
        }
    }
}

impl From<Theme> for ThemeFile {
    fn from(theme: Theme) -> Self {
        Self {
            name: theme.name,
            editor: EditorColors {
                bg: theme.editor_bg.into(),
                fg: theme.editor_fg.into(),
                cursor: theme.cursor.into(),
                inactive_cursor: theme.inactive_cursor.into(),
                selection_bg: theme.selection_bg.into(),
                current_line_bg: theme.current_line_bg.into(),
                line_number_fg: theme.line_number_fg.into(),
                line_number_bg: theme.line_number_bg.into(),
                diff_add_bg: theme.diff_add_bg.into(),
                diff_remove_bg: theme.diff_remove_bg.into(),
                diff_modify_bg: theme.diff_modify_bg.into(),
            },
            ui: UiColors {
                tab_active_fg: theme.tab_active_fg.into(),
                tab_active_bg: theme.tab_active_bg.into(),
                tab_inactive_fg: theme.tab_inactive_fg.into(),
                tab_inactive_bg: theme.tab_inactive_bg.into(),
                tab_separator_bg: theme.tab_separator_bg.into(),
                tab_close_hover_fg: theme.tab_close_hover_fg.into(),
                tab_hover_bg: theme.tab_hover_bg.into(),
                menu_bg: theme.menu_bg.into(),
                menu_fg: theme.menu_fg.into(),
                menu_active_bg: theme.menu_active_bg.into(),
                menu_active_fg: theme.menu_active_fg.into(),
                menu_dropdown_bg: theme.menu_dropdown_bg.into(),
                menu_dropdown_fg: theme.menu_dropdown_fg.into(),
                menu_highlight_bg: theme.menu_highlight_bg.into(),
                menu_highlight_fg: theme.menu_highlight_fg.into(),
                menu_border_fg: theme.menu_border_fg.into(),
                menu_separator_fg: theme.menu_separator_fg.into(),
                menu_hover_bg: theme.menu_hover_bg.into(),
                menu_hover_fg: theme.menu_hover_fg.into(),
                menu_disabled_fg: theme.menu_disabled_fg.into(),
                menu_disabled_bg: theme.menu_disabled_bg.into(),
                status_bar_fg: theme.status_bar_fg.into(),
                status_bar_bg: theme.status_bar_bg.into(),
                prompt_fg: theme.prompt_fg.into(),
                prompt_bg: theme.prompt_bg.into(),
                prompt_selection_fg: theme.prompt_selection_fg.into(),
                prompt_selection_bg: theme.prompt_selection_bg.into(),
                popup_border_fg: theme.popup_border_fg.into(),
                popup_bg: theme.popup_bg.into(),
                popup_selection_bg: theme.popup_selection_bg.into(),
                popup_text_fg: theme.popup_text_fg.into(),
                suggestion_bg: theme.suggestion_bg.into(),
                suggestion_selected_bg: theme.suggestion_selected_bg.into(),
                help_bg: theme.help_bg.into(),
                help_fg: theme.help_fg.into(),
                help_key_fg: theme.help_key_fg.into(),
                help_separator_fg: theme.help_separator_fg.into(),
                help_indicator_fg: theme.help_indicator_fg.into(),
                help_indicator_bg: theme.help_indicator_bg.into(),
                inline_code_bg: theme.inline_code_bg.into(),
                split_separator_fg: theme.split_separator_fg.into(),
                split_separator_hover_fg: theme.split_separator_hover_fg.into(),
                scrollbar_track_fg: theme.scrollbar_track_fg.into(),
                scrollbar_thumb_fg: theme.scrollbar_thumb_fg.into(),
                scrollbar_track_hover_fg: theme.scrollbar_track_hover_fg.into(),
                scrollbar_thumb_hover_fg: theme.scrollbar_thumb_hover_fg.into(),
                compose_margin_bg: theme.compose_margin_bg.into(),
                semantic_highlight_bg: theme.semantic_highlight_bg.into(),
                terminal_bg: theme.terminal_bg.into(),
                terminal_fg: theme.terminal_fg.into(),
                status_warning_indicator_bg: theme.status_warning_indicator_bg.into(),
                status_warning_indicator_fg: theme.status_warning_indicator_fg.into(),
                status_error_indicator_bg: theme.status_error_indicator_bg.into(),
                status_error_indicator_fg: theme.status_error_indicator_fg.into(),
                status_warning_indicator_hover_bg: theme.status_warning_indicator_hover_bg.into(),
                status_warning_indicator_hover_fg: theme.status_warning_indicator_hover_fg.into(),
                status_error_indicator_hover_bg: theme.status_error_indicator_hover_bg.into(),
                status_error_indicator_hover_fg: theme.status_error_indicator_hover_fg.into(),
                tab_drop_zone_bg: theme.tab_drop_zone_bg.into(),
                tab_drop_zone_border: theme.tab_drop_zone_border.into(),
            },
            search: SearchColors {
                match_bg: theme.search_match_bg.into(),
                match_fg: theme.search_match_fg.into(),
            },
            diagnostic: DiagnosticColors {
                error_fg: theme.diagnostic_error_fg.into(),
                error_bg: theme.diagnostic_error_bg.into(),
                warning_fg: theme.diagnostic_warning_fg.into(),
                warning_bg: theme.diagnostic_warning_bg.into(),
                info_fg: theme.diagnostic_info_fg.into(),
                info_bg: theme.diagnostic_info_bg.into(),
                hint_fg: theme.diagnostic_hint_fg.into(),
                hint_bg: theme.diagnostic_hint_bg.into(),
            },
            syntax: SyntaxColors {
                keyword: theme.syntax_keyword.into(),
                string: theme.syntax_string.into(),
                comment: theme.syntax_comment.into(),
                function: theme.syntax_function.into(),
                type_: theme.syntax_type.into(),
                variable: theme.syntax_variable.into(),
                constant: theme.syntax_constant.into(),
                operator: theme.syntax_operator.into(),
            },
        }
    }
}

impl Theme {
    /// Load a builtin theme by name (no I/O, uses embedded JSON).
    pub fn load_builtin(name: &str) -> Option<Self> {
        BUILTIN_THEMES
            .iter()
            .find(|t| t.name == name)
            .and_then(|t| serde_json::from_str::<ThemeFile>(t.json).ok())
            .map(|tf| tf.into())
    }

    /// Parse theme from JSON string (no I/O).
    pub fn from_json(json: &str) -> Result<Self, String> {
        let theme_file: ThemeFile =
            serde_json::from_str(json).map_err(|e| format!("Failed to parse theme JSON: {}", e))?;
        Ok(theme_file.into())
    }
}

// =============================================================================
// Theme Schema Generation for Plugin API
// =============================================================================

/// Returns the raw JSON Schema for ThemeFile, generated by schemars.
/// The schema uses standard JSON Schema format with $ref for type references.
/// Plugins are responsible for parsing and resolving $ref references.
pub fn get_theme_schema() -> serde_json::Value {
    use schemars::schema_for;
    let schema = schema_for!(ThemeFile);
    serde_json::to_value(&schema).unwrap_or_default()
}

/// Returns a map of built-in theme names to their JSON content.
pub fn get_builtin_themes() -> serde_json::Value {
    let mut map = serde_json::Map::new();
    for theme in BUILTIN_THEMES {
        map.insert(
            theme.name.to_string(),
            serde_json::Value::String(theme.json.to_string()),
        );
    }
    serde_json::Value::Object(map)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_builtin_theme() {
        let dark = Theme::load_builtin(THEME_DARK).expect("Dark theme must exist");
        assert_eq!(dark.name, THEME_DARK);

        let light = Theme::load_builtin(THEME_LIGHT).expect("Light theme must exist");
        assert_eq!(light.name, THEME_LIGHT);

        let high_contrast =
            Theme::load_builtin(THEME_HIGH_CONTRAST).expect("High contrast theme must exist");
        assert_eq!(high_contrast.name, THEME_HIGH_CONTRAST);
    }

    #[test]
    fn test_builtin_themes_match_schema() {
        for theme in BUILTIN_THEMES {
            let _: ThemeFile = serde_json::from_str(theme.json)
                .unwrap_or_else(|_| panic!("Theme '{}' does not match schema", theme.name));
        }
    }

    #[test]
    fn test_from_json() {
        let json = r#"{"name":"test","editor":{},"ui":{},"search":{},"diagnostic":{},"syntax":{}}"#;
        let theme = Theme::from_json(json).expect("Should parse minimal theme");
        assert_eq!(theme.name, "test");
    }

    #[test]
    fn test_default_reset_color() {
        // Test that "Default" maps to Color::Reset
        let color: Color = ColorDef::Named("Default".to_string()).into();
        assert_eq!(color, Color::Reset);

        // Test that "Reset" also maps to Color::Reset
        let color: Color = ColorDef::Named("Reset".to_string()).into();
        assert_eq!(color, Color::Reset);
    }
}
