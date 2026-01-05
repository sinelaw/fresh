use ratatui::style::Color;
use serde::{Deserialize, Serialize};
use std::path::Path;

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
fn brighten_color(color: Color, amount: u8) -> Color {
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
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum ColorDef {
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

/// Serializable theme definition (matches JSON structure)
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ThemeFile {
    name: String,
    editor: EditorColors,
    ui: UiColors,
    search: SearchColors,
    diagnostic: DiagnosticColors,
    syntax: SyntaxColors,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct EditorColors {
    #[serde(default = "default_editor_bg")]
    bg: ColorDef,
    #[serde(default = "default_editor_fg")]
    fg: ColorDef,
    #[serde(default = "default_cursor")]
    cursor: ColorDef,
    #[serde(default = "default_inactive_cursor")]
    inactive_cursor: ColorDef,
    #[serde(default = "default_selection_bg")]
    selection_bg: ColorDef,
    #[serde(default = "default_current_line_bg")]
    current_line_bg: ColorDef,
    #[serde(default = "default_line_number_fg")]
    line_number_fg: ColorDef,
    #[serde(default = "default_line_number_bg")]
    line_number_bg: ColorDef,
    #[serde(default = "default_diff_add_bg")]
    diff_add_bg: ColorDef,
    #[serde(default = "default_diff_remove_bg")]
    diff_remove_bg: ColorDef,
    #[serde(default = "default_diff_modify_bg")]
    diff_modify_bg: ColorDef,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
struct UiColors {
    #[serde(default = "default_tab_active_fg")]
    tab_active_fg: ColorDef,
    #[serde(default = "default_tab_active_bg")]
    tab_active_bg: ColorDef,
    #[serde(default = "default_tab_inactive_fg")]
    tab_inactive_fg: ColorDef,
    #[serde(default = "default_tab_inactive_bg")]
    tab_inactive_bg: ColorDef,
    #[serde(default = "default_tab_separator_bg")]
    tab_separator_bg: ColorDef,
    #[serde(default = "default_tab_close_hover_fg")]
    tab_close_hover_fg: ColorDef,
    #[serde(default = "default_tab_hover_bg")]
    tab_hover_bg: ColorDef,
    #[serde(default = "default_menu_bg")]
    menu_bg: ColorDef,
    #[serde(default = "default_menu_fg")]
    menu_fg: ColorDef,
    #[serde(default = "default_menu_active_bg")]
    menu_active_bg: ColorDef,
    #[serde(default = "default_menu_active_fg")]
    menu_active_fg: ColorDef,
    #[serde(default = "default_menu_dropdown_bg")]
    menu_dropdown_bg: ColorDef,
    #[serde(default = "default_menu_dropdown_fg")]
    menu_dropdown_fg: ColorDef,
    #[serde(default = "default_menu_highlight_bg")]
    menu_highlight_bg: ColorDef,
    #[serde(default = "default_menu_highlight_fg")]
    menu_highlight_fg: ColorDef,
    #[serde(default = "default_menu_border_fg")]
    menu_border_fg: ColorDef,
    #[serde(default = "default_menu_separator_fg")]
    menu_separator_fg: ColorDef,
    #[serde(default = "default_menu_hover_bg")]
    menu_hover_bg: ColorDef,
    #[serde(default = "default_menu_hover_fg")]
    menu_hover_fg: ColorDef,
    #[serde(default = "default_menu_disabled_fg")]
    menu_disabled_fg: ColorDef,
    #[serde(default = "default_menu_disabled_bg")]
    menu_disabled_bg: ColorDef,
    #[serde(default = "default_status_bar_fg")]
    status_bar_fg: ColorDef,
    #[serde(default = "default_status_bar_bg")]
    status_bar_bg: ColorDef,
    #[serde(default = "default_prompt_fg")]
    prompt_fg: ColorDef,
    #[serde(default = "default_prompt_bg")]
    prompt_bg: ColorDef,
    #[serde(default = "default_prompt_selection_fg")]
    prompt_selection_fg: ColorDef,
    #[serde(default = "default_prompt_selection_bg")]
    prompt_selection_bg: ColorDef,
    #[serde(default = "default_popup_border_fg")]
    popup_border_fg: ColorDef,
    #[serde(default = "default_popup_bg")]
    popup_bg: ColorDef,
    #[serde(default = "default_popup_selection_bg")]
    popup_selection_bg: ColorDef,
    #[serde(default = "default_popup_text_fg")]
    popup_text_fg: ColorDef,
    #[serde(default = "default_suggestion_bg")]
    suggestion_bg: ColorDef,
    #[serde(default = "default_suggestion_selected_bg")]
    suggestion_selected_bg: ColorDef,
    #[serde(default = "default_help_bg")]
    help_bg: ColorDef,
    #[serde(default = "default_help_fg")]
    help_fg: ColorDef,
    #[serde(default = "default_help_key_fg")]
    help_key_fg: ColorDef,
    #[serde(default = "default_help_separator_fg")]
    help_separator_fg: ColorDef,
    #[serde(default = "default_help_indicator_fg")]
    help_indicator_fg: ColorDef,
    #[serde(default = "default_help_indicator_bg")]
    help_indicator_bg: ColorDef,
    #[serde(default = "default_inline_code_bg")]
    inline_code_bg: ColorDef,
    #[serde(default = "default_split_separator_fg")]
    split_separator_fg: ColorDef,
    #[serde(default = "default_split_separator_hover_fg")]
    split_separator_hover_fg: ColorDef,
    #[serde(default = "default_scrollbar_track_fg")]
    scrollbar_track_fg: ColorDef,
    #[serde(default = "default_scrollbar_thumb_fg")]
    scrollbar_thumb_fg: ColorDef,
    #[serde(default = "default_scrollbar_track_hover_fg")]
    scrollbar_track_hover_fg: ColorDef,
    #[serde(default = "default_scrollbar_thumb_hover_fg")]
    scrollbar_thumb_hover_fg: ColorDef,
    #[serde(default = "default_compose_margin_bg")]
    compose_margin_bg: ColorDef,
    #[serde(default = "default_semantic_highlight_bg")]
    semantic_highlight_bg: ColorDef,
    #[serde(default = "default_terminal_bg")]
    terminal_bg: ColorDef,
    #[serde(default = "default_terminal_fg")]
    terminal_fg: ColorDef,
    #[serde(default = "default_status_warning_indicator_bg")]
    status_warning_indicator_bg: ColorDef,
    #[serde(default = "default_status_warning_indicator_fg")]
    status_warning_indicator_fg: ColorDef,
    #[serde(default = "default_status_error_indicator_bg")]
    status_error_indicator_bg: ColorDef,
    #[serde(default = "default_status_error_indicator_fg")]
    status_error_indicator_fg: ColorDef,
    #[serde(default = "default_status_warning_indicator_hover_bg")]
    status_warning_indicator_hover_bg: ColorDef,
    #[serde(default = "default_status_warning_indicator_hover_fg")]
    status_warning_indicator_hover_fg: ColorDef,
    #[serde(default = "default_status_error_indicator_hover_bg")]
    status_error_indicator_hover_bg: ColorDef,
    #[serde(default = "default_status_error_indicator_hover_fg")]
    status_error_indicator_hover_fg: ColorDef,
    #[serde(default = "default_tab_drop_zone_bg")]
    tab_drop_zone_bg: ColorDef,
    #[serde(default = "default_tab_drop_zone_border")]
    tab_drop_zone_border: ColorDef,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SearchColors {
    #[serde(default = "default_search_match_bg")]
    match_bg: ColorDef,
    #[serde(default = "default_search_match_fg")]
    match_fg: ColorDef,
}

// Default search colors
fn default_search_match_bg() -> ColorDef {
    ColorDef::Rgb(100, 100, 20)
}
fn default_search_match_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct DiagnosticColors {
    #[serde(default = "default_diagnostic_error_fg")]
    error_fg: ColorDef,
    #[serde(default = "default_diagnostic_error_bg")]
    error_bg: ColorDef,
    #[serde(default = "default_diagnostic_warning_fg")]
    warning_fg: ColorDef,
    #[serde(default = "default_diagnostic_warning_bg")]
    warning_bg: ColorDef,
    #[serde(default = "default_diagnostic_info_fg")]
    info_fg: ColorDef,
    #[serde(default = "default_diagnostic_info_bg")]
    info_bg: ColorDef,
    #[serde(default = "default_diagnostic_hint_fg")]
    hint_fg: ColorDef,
    #[serde(default = "default_diagnostic_hint_bg")]
    hint_bg: ColorDef,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SyntaxColors {
    #[serde(default = "default_syntax_keyword")]
    keyword: ColorDef,
    #[serde(default = "default_syntax_string")]
    string: ColorDef,
    #[serde(default = "default_syntax_comment")]
    comment: ColorDef,
    #[serde(default = "default_syntax_function")]
    function: ColorDef,
    #[serde(rename = "type", default = "default_syntax_type")]
    type_: ColorDef,
    #[serde(default = "default_syntax_variable")]
    variable: ColorDef,
    #[serde(default = "default_syntax_constant")]
    constant: ColorDef,
    #[serde(default = "default_syntax_operator")]
    operator: ColorDef,
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

impl Theme {
    /// Load theme from a JSON file
    fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read theme file: {}", e))?;
        let theme_file: ThemeFile = serde_json::from_str(&content)
            .map_err(|e| format!("Failed to parse theme file: {}", e))?;
        Ok(theme_file.into())
    }

    /// Load builtin theme from the themes directory
    fn load_builtin_theme(name: &str) -> Option<Self> {
        // Build list of paths to search
        let mut theme_paths = vec![
            format!("themes/{}.json", name),
            format!("../themes/{}.json", name),
            format!("../../themes/{}.json", name),
        ];

        // Also check user config themes directory
        if let Some(config_dir) = dirs::config_dir() {
            let user_theme_path = config_dir
                .join("fresh")
                .join("themes")
                .join(format!("{}.json", name));
            theme_paths.insert(0, user_theme_path.to_string_lossy().to_string());
        }

        for path in &theme_paths {
            if let Ok(theme) = Self::from_file(path) {
                return Some(theme);
            }
        }

        None
    }

    /// Default dark theme (VSCode Dark+ inspired)
    /// Fallback if JSON file cannot be loaded
    pub fn dark() -> Self {
        Self {
            name: "dark".to_string(),

            // Editor colors
            editor_bg: Color::Rgb(30, 30, 30),
            editor_fg: Color::Rgb(212, 212, 212),
            cursor: Color::Rgb(255, 255, 255),
            inactive_cursor: Color::Rgb(100, 100, 100),
            selection_bg: Color::Rgb(38, 79, 120),
            current_line_bg: Color::Rgb(40, 40, 40),
            line_number_fg: Color::Rgb(100, 100, 100),
            line_number_bg: Color::Rgb(30, 30, 30),

            // Diff highlighting colors
            diff_add_bg: Color::Rgb(35, 60, 35),    // Dark green
            diff_remove_bg: Color::Rgb(70, 35, 35), // Dark red
            diff_modify_bg: Color::Rgb(40, 38, 30), // Subtle yellow tint, close to editor_bg
            diff_add_highlight_bg: Color::Rgb(60, 110, 60), // Brighter green for inline
            diff_remove_highlight_bg: Color::Rgb(120, 50, 50), // Brighter red for inline

            // UI element colors
            tab_active_fg: Color::Yellow,
            tab_active_bg: Color::Blue,
            tab_inactive_fg: Color::White,
            tab_inactive_bg: Color::DarkGray,
            tab_separator_bg: Color::Rgb(45, 45, 48),
            tab_close_hover_fg: Color::Rgb(255, 100, 100),
            tab_hover_bg: Color::Rgb(70, 70, 75),

            // Menu bar colors
            menu_bg: Color::Rgb(60, 60, 65),
            menu_fg: Color::Rgb(220, 220, 220),
            menu_active_bg: Color::Rgb(60, 60, 60),
            menu_active_fg: Color::Rgb(255, 255, 255),
            menu_dropdown_bg: Color::Rgb(50, 50, 50),
            menu_dropdown_fg: Color::Rgb(220, 220, 220),
            menu_highlight_bg: Color::Rgb(70, 130, 180),
            menu_highlight_fg: Color::Rgb(255, 255, 255),
            menu_border_fg: Color::Rgb(100, 100, 100),
            menu_separator_fg: Color::Rgb(80, 80, 80),
            menu_hover_bg: Color::Rgb(55, 55, 55),
            menu_hover_fg: Color::Rgb(255, 255, 255),
            menu_disabled_fg: Color::Rgb(100, 100, 100), // Gray for disabled items
            menu_disabled_bg: Color::Rgb(50, 50, 50),

            status_bar_fg: Color::White,
            status_bar_bg: Color::Rgb(30, 30, 30), // Darker than DarkGray
            prompt_fg: Color::White,
            prompt_bg: Color::Rgb(20, 20, 20), // Very dark
            prompt_selection_fg: Color::White,
            prompt_selection_bg: Color::Rgb(58, 79, 120), // Blue selection

            popup_border_fg: Color::Gray,
            popup_bg: Color::Rgb(30, 30, 30),
            popup_selection_bg: Color::Rgb(58, 79, 120),
            popup_text_fg: Color::White,

            suggestion_bg: Color::Rgb(30, 30, 30),
            suggestion_selected_bg: Color::Rgb(58, 79, 120),

            help_bg: Color::Black,
            help_fg: Color::White,
            help_key_fg: Color::Cyan,
            help_separator_fg: Color::DarkGray,

            help_indicator_fg: Color::Red,
            help_indicator_bg: Color::Black,

            inline_code_bg: Color::Rgb(60, 60, 60), // Dark gray for code blocks

            split_separator_fg: Color::Rgb(100, 100, 100),
            split_separator_hover_fg: Color::Rgb(100, 149, 237), // Cornflower blue

            // Scrollbar colors
            scrollbar_track_fg: Color::DarkGray,
            scrollbar_thumb_fg: Color::Gray,
            scrollbar_track_hover_fg: Color::Gray,
            scrollbar_thumb_hover_fg: Color::White,

            // Compose mode colors
            compose_margin_bg: Color::Rgb(18, 18, 18), // Darker than editor_bg for "desk" effect

            // Semantic highlighting (word under cursor)
            semantic_highlight_bg: Color::Rgb(60, 60, 80), // Subtle dark highlight

            // Terminal colors (use terminal's default colors to preserve transparency)
            terminal_bg: Color::Reset,
            terminal_fg: Color::Reset,

            // Status bar warning/error indicator colors
            status_warning_indicator_bg: Color::Rgb(181, 137, 0), // Solarized amber
            status_warning_indicator_fg: Color::Rgb(0, 0, 0),     // Black text
            status_error_indicator_bg: Color::Rgb(220, 50, 47),   // Solarized red
            status_error_indicator_fg: Color::Rgb(255, 255, 255), // White text
            status_warning_indicator_hover_bg: Color::Rgb(211, 167, 30), // Lighter amber
            status_warning_indicator_hover_fg: Color::Rgb(0, 0, 0),
            status_error_indicator_hover_bg: Color::Rgb(250, 80, 77), // Lighter red
            status_error_indicator_hover_fg: Color::Rgb(255, 255, 255),

            // Tab drag-and-drop colors
            tab_drop_zone_bg: Color::Rgb(70, 130, 180), // Steel blue
            tab_drop_zone_border: Color::Rgb(100, 149, 237), // Cornflower blue

            // Search colors
            search_match_bg: Color::Rgb(100, 100, 20), // Yellow-brown highlight
            search_match_fg: Color::Rgb(255, 255, 255),

            // Diagnostic colors
            diagnostic_error_fg: Color::Red,
            diagnostic_error_bg: Color::Rgb(60, 20, 20),
            diagnostic_warning_fg: Color::Yellow,
            diagnostic_warning_bg: Color::Rgb(60, 50, 0),
            diagnostic_info_fg: Color::Blue,
            diagnostic_info_bg: Color::Rgb(0, 30, 60),
            diagnostic_hint_fg: Color::Gray,
            diagnostic_hint_bg: Color::Rgb(30, 30, 30),

            // Syntax highlighting colors (VSCode Dark+ palette)
            syntax_keyword: Color::Rgb(86, 156, 214),
            syntax_string: Color::Rgb(206, 145, 120),
            syntax_comment: Color::Rgb(106, 153, 85),
            syntax_function: Color::Rgb(220, 220, 170),
            syntax_type: Color::Rgb(78, 201, 176),
            syntax_variable: Color::Rgb(156, 220, 254),
            syntax_constant: Color::Rgb(79, 193, 255),
            syntax_operator: Color::Rgb(212, 212, 212),
        }
    }

    /// Light theme (VSCode Light+ inspired)
    pub fn light() -> Self {
        Self {
            name: "light".to_string(),

            // Editor colors
            editor_bg: Color::Rgb(255, 255, 255),
            editor_fg: Color::Rgb(0, 0, 0),
            cursor: Color::Rgb(0, 0, 0),
            inactive_cursor: Color::Rgb(180, 180, 180),
            selection_bg: Color::Rgb(173, 214, 255),
            current_line_bg: Color::Rgb(245, 245, 245),
            line_number_fg: Color::Rgb(140, 140, 140),
            line_number_bg: Color::Rgb(255, 255, 255),

            // Diff highlighting colors
            diff_add_bg: Color::Rgb(200, 255, 200), // Light green
            diff_remove_bg: Color::Rgb(255, 200, 200), // Light red
            diff_modify_bg: Color::Rgb(255, 252, 240), // Subtle cream, close to white
            diff_add_highlight_bg: Color::Rgb(140, 220, 140), // Darker green for inline (contrast on light bg)
            diff_remove_highlight_bg: Color::Rgb(220, 140, 140), // Darker red for inline

            // UI element colors
            tab_active_fg: Color::Rgb(40, 40, 40),
            tab_active_bg: Color::Rgb(255, 255, 255),
            tab_inactive_fg: Color::Rgb(100, 100, 100),
            tab_inactive_bg: Color::Rgb(230, 230, 230),
            tab_separator_bg: Color::Rgb(200, 200, 200),
            tab_close_hover_fg: Color::Rgb(220, 50, 50),
            tab_hover_bg: Color::Rgb(210, 210, 210),

            // Menu bar colors - dark text on light backgrounds
            menu_bg: Color::Rgb(245, 245, 245),
            menu_fg: Color::Rgb(30, 30, 30),
            menu_active_bg: Color::Rgb(225, 225, 225),
            menu_active_fg: Color::Rgb(0, 0, 0),
            menu_dropdown_bg: Color::Rgb(248, 248, 248),
            menu_dropdown_fg: Color::Rgb(30, 30, 30),
            menu_highlight_bg: Color::Rgb(209, 226, 243), // Light blue highlight
            menu_highlight_fg: Color::Rgb(0, 0, 0),       // Dark text on light highlight
            menu_border_fg: Color::Rgb(180, 180, 180),
            menu_separator_fg: Color::Rgb(210, 210, 210),
            menu_hover_bg: Color::Rgb(230, 235, 240),
            menu_hover_fg: Color::Rgb(0, 0, 0),
            menu_disabled_fg: Color::Rgb(160, 160, 160), // Gray for disabled items
            menu_disabled_bg: Color::Rgb(248, 248, 248),

            status_bar_fg: Color::Black,
            status_bar_bg: Color::Rgb(220, 220, 220), // Light grey
            prompt_fg: Color::Black,
            prompt_bg: Color::Rgb(230, 240, 250), // Very light blue
            prompt_selection_fg: Color::Black,
            prompt_selection_bg: Color::Rgb(173, 214, 255), // Light blue selection

            popup_border_fg: Color::Rgb(180, 180, 180),
            popup_bg: Color::Rgb(232, 238, 245), // Light blue-gray
            popup_selection_bg: Color::Rgb(209, 226, 243),
            popup_text_fg: Color::Rgb(30, 30, 30),

            suggestion_bg: Color::Rgb(232, 238, 245), // Light blue-gray
            suggestion_selected_bg: Color::Rgb(209, 226, 243),

            help_bg: Color::White,
            help_fg: Color::Black,
            help_key_fg: Color::Blue,
            help_separator_fg: Color::Gray,

            help_indicator_fg: Color::Red,
            help_indicator_bg: Color::White,

            inline_code_bg: Color::Rgb(230, 230, 235), // Light gray for code blocks

            split_separator_fg: Color::Rgb(140, 140, 140),
            split_separator_hover_fg: Color::Rgb(70, 130, 180), // Steel blue

            // Scrollbar colors
            scrollbar_track_fg: Color::Rgb(220, 220, 220),
            scrollbar_thumb_fg: Color::Rgb(180, 180, 180),
            scrollbar_track_hover_fg: Color::Rgb(200, 200, 200),
            scrollbar_thumb_hover_fg: Color::Rgb(140, 140, 140),

            // Compose mode colors
            compose_margin_bg: Color::Rgb(220, 220, 225), // Slightly darker than white for "desk" effect

            // Semantic highlighting (word under cursor)
            semantic_highlight_bg: Color::Rgb(220, 230, 240), // Subtle light blue highlight

            // Terminal colors (use terminal's default colors to preserve transparency)
            terminal_bg: Color::Reset,
            terminal_fg: Color::Reset,

            // Status bar warning/error indicator colors (darker for light theme)
            status_warning_indicator_bg: Color::Rgb(202, 145, 0), // Darker amber for light bg
            status_warning_indicator_fg: Color::Rgb(0, 0, 0),     // Black text
            status_error_indicator_bg: Color::Rgb(200, 40, 40),   // Darker red for light bg
            status_error_indicator_fg: Color::Rgb(255, 255, 255), // White text
            status_warning_indicator_hover_bg: Color::Rgb(232, 175, 30), // Lighter amber
            status_warning_indicator_hover_fg: Color::Rgb(0, 0, 0),
            status_error_indicator_hover_bg: Color::Rgb(230, 70, 70), // Lighter red
            status_error_indicator_hover_fg: Color::Rgb(255, 255, 255),

            // Tab drag-and-drop colors (lighter for light theme)
            tab_drop_zone_bg: Color::Rgb(173, 214, 255), // Light blue
            tab_drop_zone_border: Color::Rgb(70, 130, 180), // Steel blue

            // Search colors
            search_match_bg: Color::Rgb(255, 255, 150), // Light yellow highlight
            search_match_fg: Color::Rgb(0, 0, 0),

            // Diagnostic colors
            diagnostic_error_fg: Color::Red,
            diagnostic_error_bg: Color::Rgb(255, 220, 220),
            diagnostic_warning_fg: Color::Rgb(128, 128, 0),
            diagnostic_warning_bg: Color::Rgb(255, 255, 200),
            diagnostic_info_fg: Color::Blue,
            diagnostic_info_bg: Color::Rgb(220, 240, 255),
            diagnostic_hint_fg: Color::DarkGray,
            diagnostic_hint_bg: Color::Rgb(240, 240, 240),

            // Syntax highlighting colors (improved light theme palette)
            syntax_keyword: Color::Rgb(175, 0, 219), // Purple keywords
            syntax_string: Color::Rgb(163, 21, 21),  // Dark red strings
            syntax_comment: Color::Rgb(0, 128, 0),   // Green comments
            syntax_function: Color::Rgb(121, 94, 38), // Brown functions
            syntax_type: Color::Rgb(0, 128, 128),    // Teal types
            syntax_variable: Color::Rgb(0, 16, 128), // Dark blue variables
            syntax_constant: Color::Rgb(0, 112, 193), // Blue constants
            syntax_operator: Color::Rgb(0, 0, 0),    // Black operators
        }
    }

    /// High contrast theme for accessibility
    pub fn high_contrast() -> Self {
        Self {
            name: "high-contrast".to_string(),

            // Editor colors
            editor_bg: Color::Black,
            editor_fg: Color::White,
            cursor: Color::White,
            inactive_cursor: Color::DarkGray,
            selection_bg: Color::Rgb(0, 100, 200),
            current_line_bg: Color::Rgb(20, 20, 20),
            line_number_fg: Color::Rgb(140, 140, 140),
            line_number_bg: Color::Black,

            // Diff highlighting colors
            diff_add_bg: Color::Rgb(0, 80, 0),     // Dark green
            diff_remove_bg: Color::Rgb(100, 0, 0), // Dark red
            diff_modify_bg: Color::Rgb(25, 22, 0), // Subtle yellow, close to black
            diff_add_highlight_bg: Color::Rgb(0, 140, 0), // Brighter green
            diff_remove_highlight_bg: Color::Rgb(180, 0, 0), // Brighter red

            // UI element colors
            tab_active_fg: Color::Black,
            tab_active_bg: Color::Yellow,
            tab_inactive_fg: Color::White,
            tab_inactive_bg: Color::Black,
            tab_separator_bg: Color::Rgb(30, 30, 35),
            tab_close_hover_fg: Color::Rgb(249, 38, 114), // Monokai pink for hover
            tab_hover_bg: Color::Rgb(50, 50, 55),

            // Menu bar colors
            menu_bg: Color::Rgb(50, 50, 55),
            menu_fg: Color::White,
            menu_active_bg: Color::Yellow,
            menu_active_fg: Color::Black,
            menu_dropdown_bg: Color::Rgb(20, 20, 20),
            menu_dropdown_fg: Color::White,
            menu_highlight_bg: Color::Rgb(0, 100, 200),
            menu_highlight_fg: Color::White,
            menu_border_fg: Color::Yellow,
            menu_separator_fg: Color::White,
            menu_hover_bg: Color::Rgb(50, 50, 50),
            menu_hover_fg: Color::Yellow,
            menu_disabled_fg: Color::DarkGray, // Low contrast gray for disabled
            menu_disabled_bg: Color::Rgb(20, 20, 20),

            status_bar_fg: Color::White,
            status_bar_bg: Color::Rgb(20, 20, 20), // Darker for high contrast
            prompt_fg: Color::White,
            prompt_bg: Color::Rgb(10, 10, 10), // Very dark
            prompt_selection_fg: Color::White,
            prompt_selection_bg: Color::Rgb(0, 100, 200), // Blue selection

            popup_border_fg: Color::LightCyan,
            popup_bg: Color::Black,
            popup_selection_bg: Color::Rgb(0, 100, 200),
            popup_text_fg: Color::White,

            suggestion_bg: Color::Black,
            suggestion_selected_bg: Color::Rgb(0, 100, 200),

            help_bg: Color::Black,
            help_fg: Color::White,
            help_key_fg: Color::LightCyan,
            help_separator_fg: Color::White,

            help_indicator_fg: Color::Red,
            help_indicator_bg: Color::Black,

            inline_code_bg: Color::Rgb(40, 40, 40), // Dark gray for code blocks

            split_separator_fg: Color::Rgb(140, 140, 140),
            split_separator_hover_fg: Color::Yellow,

            // Scrollbar colors
            scrollbar_track_fg: Color::White,
            scrollbar_thumb_fg: Color::Yellow,
            scrollbar_track_hover_fg: Color::Yellow,
            scrollbar_thumb_hover_fg: Color::Cyan,

            // Compose mode colors
            compose_margin_bg: Color::Rgb(10, 10, 10), // Very dark for high contrast "desk" effect

            // Semantic highlighting (word under cursor)
            semantic_highlight_bg: Color::Rgb(0, 60, 100), // Bright blue highlight for visibility

            // Terminal colors (use terminal's default colors to preserve transparency)
            terminal_bg: Color::Reset,
            terminal_fg: Color::Reset,

            // Status bar warning/error indicator colors (high visibility)
            status_warning_indicator_bg: Color::Yellow, // Bright yellow
            status_warning_indicator_fg: Color::Black,  // Black text
            status_error_indicator_bg: Color::Red,      // Bright red
            status_error_indicator_fg: Color::White,    // White text
            status_warning_indicator_hover_bg: Color::LightYellow, // Lighter yellow
            status_warning_indicator_hover_fg: Color::Black,
            status_error_indicator_hover_bg: Color::LightRed, // Lighter red
            status_error_indicator_hover_fg: Color::White,

            // Tab drag-and-drop colors (high visibility)
            tab_drop_zone_bg: Color::Rgb(0, 100, 200), // Bright blue
            tab_drop_zone_border: Color::Yellow,       // Yellow border for visibility

            // Search colors
            search_match_bg: Color::Yellow,
            search_match_fg: Color::Black,

            // Diagnostic colors
            diagnostic_error_fg: Color::Red,
            diagnostic_error_bg: Color::Rgb(100, 0, 0),
            diagnostic_warning_fg: Color::Yellow,
            diagnostic_warning_bg: Color::Rgb(100, 100, 0),
            diagnostic_info_fg: Color::Cyan,
            diagnostic_info_bg: Color::Rgb(0, 50, 100),
            diagnostic_hint_fg: Color::White,
            diagnostic_hint_bg: Color::Rgb(50, 50, 50),

            // Syntax highlighting colors (high contrast)
            syntax_keyword: Color::Cyan,
            syntax_string: Color::Green,
            syntax_comment: Color::Gray,
            syntax_function: Color::Yellow,
            syntax_type: Color::Magenta,
            syntax_variable: Color::White,
            syntax_constant: Color::LightBlue,
            syntax_operator: Color::White,
        }
    }

    /// Get a theme by name, defaults to dark if not found
    /// Tries to load from JSON file first, falls back to hardcoded themes
    pub fn from_name(name: &str) -> Self {
        let normalized_name = name.to_lowercase().replace('_', "-");

        // Try to load from JSON file first
        if let Some(theme) = Self::load_builtin_theme(&normalized_name) {
            return theme;
        }

        // Fall back to hardcoded themes
        match normalized_name.as_str() {
            "light" => Self::light(),
            "high-contrast" => Self::high_contrast(),
            "nostalgia" => Self::nostalgia(),
            _ => Self::dark(),
        }
    }

    /// Get all available theme names (builtin + user themes)
    pub fn available_themes() -> Vec<String> {
        let mut themes: Vec<String> = vec![
            "dark".to_string(),
            "light".to_string(),
            "high-contrast".to_string(),
            "nostalgia".to_string(),
        ];

        // Scan built-in themes directory (themes/*.json in the project)
        if let Ok(entries) = std::fs::read_dir("themes") {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().is_some_and(|ext| ext == "json") {
                    if let Some(stem) = path.file_stem() {
                        let name = stem.to_string_lossy().to_string();
                        // Avoid duplicates
                        if !themes.iter().any(|t| t == &name) {
                            themes.push(name);
                        }
                    }
                }
            }
        }

        // Scan user themes directory (user themes can override built-ins)
        if let Some(config_dir) = dirs::config_dir() {
            let user_themes_dir = config_dir.join("fresh").join("themes");
            if let Ok(entries) = std::fs::read_dir(&user_themes_dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.extension().is_some_and(|ext| ext == "json") {
                        if let Some(stem) = path.file_stem() {
                            let name = stem.to_string_lossy().to_string();
                            // Avoid duplicates (user theme overriding builtin)
                            if !themes.iter().any(|t| t == &name) {
                                themes.push(name);
                            }
                        }
                    }
                }
            }
        }

        themes
    }

    /// Nostalgia theme (Turbo Pascal 5 / WordPerfect 5 inspired)
    pub fn nostalgia() -> Self {
        Self {
            name: "nostalgia".to_string(),

            // Editor colors - classic blue background with yellow/white text
            editor_bg: Color::Rgb(0, 0, 170),    // Classic DOS blue
            editor_fg: Color::Rgb(255, 255, 85), // Bright yellow
            cursor: Color::Rgb(255, 255, 255),   // White block cursor
            inactive_cursor: Color::Rgb(170, 170, 170),
            selection_bg: Color::Rgb(170, 170, 170), // Gray selection
            current_line_bg: Color::Rgb(0, 0, 128),  // Slightly darker blue
            line_number_fg: Color::Rgb(85, 255, 255), // Cyan
            line_number_bg: Color::Rgb(0, 0, 170),

            // Diff highlighting colors
            diff_add_bg: Color::Rgb(0, 100, 0),      // DOS green
            diff_remove_bg: Color::Rgb(170, 0, 0),   // DOS red
            diff_modify_bg: Color::Rgb(20, 20, 140), // Subtle purple-blue, close to DOS blue bg
            diff_add_highlight_bg: Color::Rgb(0, 170, 0), // Brighter DOS green
            diff_remove_highlight_bg: Color::Rgb(255, 0, 0), // Brighter DOS red

            // UI element colors
            tab_active_fg: Color::Rgb(0, 0, 0),
            tab_active_bg: Color::Rgb(170, 170, 170),
            tab_inactive_fg: Color::Rgb(255, 255, 85),
            tab_inactive_bg: Color::Rgb(0, 0, 128),
            tab_separator_bg: Color::Rgb(0, 0, 170),
            tab_close_hover_fg: Color::Rgb(255, 85, 85), // Bright red for close hover
            tab_hover_bg: Color::Rgb(0, 0, 200),         // Slightly brighter blue for hover

            // Menu bar colors - classic DOS menu style
            menu_bg: Color::Rgb(170, 170, 170),
            menu_fg: Color::Rgb(0, 0, 0),
            menu_active_bg: Color::Rgb(0, 170, 0),
            menu_active_fg: Color::Rgb(255, 255, 255),
            menu_dropdown_bg: Color::Rgb(170, 170, 170),
            menu_dropdown_fg: Color::Rgb(0, 0, 0),
            menu_highlight_bg: Color::Rgb(0, 170, 0), // Green highlight
            menu_highlight_fg: Color::Rgb(255, 255, 255),
            menu_border_fg: Color::Rgb(0, 0, 0),
            menu_separator_fg: Color::Rgb(85, 85, 85),
            menu_hover_bg: Color::Rgb(0, 170, 0),
            menu_hover_fg: Color::Rgb(255, 255, 255),
            menu_disabled_fg: Color::Rgb(85, 85, 85), // Dark gray for disabled
            menu_disabled_bg: Color::Rgb(170, 170, 170),

            status_bar_fg: Color::Rgb(0, 0, 0),
            status_bar_bg: Color::Rgb(0, 170, 170), // Cyan status bar
            prompt_fg: Color::Rgb(255, 255, 85),    // Yellow text
            prompt_bg: Color::Rgb(0, 0, 170),       // Blue background
            prompt_selection_fg: Color::Rgb(0, 0, 0),
            prompt_selection_bg: Color::Rgb(170, 170, 170),

            popup_border_fg: Color::Rgb(255, 255, 255),
            popup_bg: Color::Rgb(0, 0, 170),
            popup_selection_bg: Color::Rgb(0, 170, 0),
            popup_text_fg: Color::Rgb(255, 255, 85),

            suggestion_bg: Color::Rgb(0, 0, 170),
            suggestion_selected_bg: Color::Rgb(0, 170, 0),

            help_bg: Color::Rgb(0, 0, 170),
            help_fg: Color::Rgb(255, 255, 85),
            help_key_fg: Color::Rgb(85, 255, 255),
            help_separator_fg: Color::Rgb(170, 170, 170),

            help_indicator_fg: Color::Rgb(255, 85, 85),
            help_indicator_bg: Color::Rgb(0, 0, 170),

            inline_code_bg: Color::Rgb(0, 0, 85), // Darker blue for code blocks

            split_separator_fg: Color::Rgb(85, 255, 255),
            split_separator_hover_fg: Color::Rgb(255, 255, 255),

            // Scrollbar colors
            scrollbar_track_fg: Color::Rgb(0, 0, 128),
            scrollbar_thumb_fg: Color::Rgb(170, 170, 170),
            scrollbar_track_hover_fg: Color::Rgb(0, 0, 128),
            scrollbar_thumb_hover_fg: Color::Rgb(255, 255, 255),

            // Compose mode colors
            compose_margin_bg: Color::Rgb(0, 0, 128), // Darker blue for margins

            // Semantic highlighting (word under cursor)
            semantic_highlight_bg: Color::Rgb(0, 85, 170), // Lighter blue highlight

            // Terminal colors (Turbo Pascal style - blue background, yellow text)
            terminal_bg: Color::Rgb(0, 0, 170), // Classic DOS blue
            terminal_fg: Color::Rgb(255, 255, 85), // Bright yellow

            // Status bar warning/error indicator colors (DOS style)
            status_warning_indicator_bg: Color::Rgb(170, 85, 0), // Brown/orange (DOS warning)
            status_warning_indicator_fg: Color::Rgb(255, 255, 255), // White text
            status_error_indicator_bg: Color::Rgb(170, 0, 0),    // DOS red
            status_error_indicator_fg: Color::Rgb(255, 255, 255), // White text
            status_warning_indicator_hover_bg: Color::Rgb(200, 115, 30), // Lighter brown
            status_warning_indicator_hover_fg: Color::Rgb(255, 255, 255),
            status_error_indicator_hover_bg: Color::Rgb(200, 30, 30), // Lighter red
            status_error_indicator_hover_fg: Color::Rgb(255, 255, 255),

            // Tab drag-and-drop colors (DOS style)
            tab_drop_zone_bg: Color::Rgb(0, 170, 170), // Cyan (DOS style)
            tab_drop_zone_border: Color::Rgb(255, 255, 255), // White border

            // Search colors
            search_match_bg: Color::Rgb(170, 85, 0), // Orange/brown
            search_match_fg: Color::Rgb(255, 255, 255),

            // Diagnostic colors
            diagnostic_error_fg: Color::Rgb(255, 85, 85),
            diagnostic_error_bg: Color::Rgb(128, 0, 0),
            diagnostic_warning_fg: Color::Rgb(255, 255, 85),
            diagnostic_warning_bg: Color::Rgb(128, 128, 0),
            diagnostic_info_fg: Color::Rgb(85, 255, 255),
            diagnostic_info_bg: Color::Rgb(0, 85, 128),
            diagnostic_hint_fg: Color::Rgb(170, 170, 170),
            diagnostic_hint_bg: Color::Rgb(0, 0, 128),

            // Syntax highlighting colors (Turbo Pascal / Borland style)
            syntax_keyword: Color::Rgb(255, 255, 255), // Bright white keywords
            syntax_string: Color::Rgb(0, 255, 255),    // Bright cyan strings
            syntax_comment: Color::Rgb(128, 128, 128), // Dark gray comments
            syntax_function: Color::Rgb(255, 255, 0),  // Bright yellow functions
            syntax_type: Color::Rgb(0, 255, 0),        // Bright green types
            syntax_variable: Color::Rgb(255, 255, 85), // Yellow variables
            syntax_constant: Color::Rgb(255, 0, 255),  // Bright magenta constants
            syntax_operator: Color::Rgb(170, 170, 170), // Light gray operators
        }
    }

    /// Set the terminal cursor color using OSC 12 escape sequence.
    /// This makes the hardware cursor visible on any background.
    pub fn set_terminal_cursor_color(&self) {
        use std::io::Write;
        if let Some((r, g, b)) = color_to_rgb(self.cursor) {
            // OSC 12 sets cursor color: \x1b]12;#RRGGBB\x07
            let _ = write!(
                std::io::stdout(),
                "\x1b]12;#{:02x}{:02x}{:02x}\x07",
                r,
                g,
                b
            );
            let _ = std::io::stdout().flush();
        }
    }

    /// Reset the terminal cursor color to default.
    pub fn reset_terminal_cursor_color() {
        use std::io::Write;
        // OSC 112 resets cursor color to default
        let _ = write!(std::io::stdout(), "\x1b]112\x07");
        let _ = std::io::stdout().flush();
    }
}

impl Default for Theme {
    fn default() -> Self {
        Self::high_contrast()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_theme_creation() {
        let dark = Theme::dark();
        assert_eq!(dark.name, "dark");

        let light = Theme::light();
        assert_eq!(light.name, "light");

        let high_contrast = Theme::high_contrast();
        assert_eq!(high_contrast.name, "high-contrast");
    }

    #[test]
    fn test_theme_from_name() {
        let theme = Theme::from_name("light");
        assert_eq!(theme.name, "light");

        let theme = Theme::from_name("high-contrast");
        assert_eq!(theme.name, "high-contrast");

        let theme = Theme::from_name("unknown");
        assert_eq!(theme.name, "dark");
    }

    #[test]
    fn test_available_themes() {
        let themes = Theme::available_themes();
        // At minimum, should have the 4 builtin themes
        assert!(themes.len() >= 4);
        assert!(themes.contains(&"dark".to_string()));
        assert!(themes.contains(&"light".to_string()));
        assert!(themes.contains(&"high-contrast".to_string()));
        assert!(themes.contains(&"nostalgia".to_string()));
    }

    #[test]
    fn test_default_theme() {
        let theme = Theme::default();
        assert_eq!(theme.name, "high-contrast");
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
