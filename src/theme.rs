use ratatui::style::Color;
use serde::{Deserialize, Serialize};
use std::path::Path;

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
    bg: ColorDef,
    fg: ColorDef,
    cursor: ColorDef,
    #[serde(default = "default_inactive_cursor")]
    inactive_cursor: ColorDef,
    selection_bg: ColorDef,
    current_line_bg: ColorDef,
    line_number_fg: ColorDef,
    line_number_bg: ColorDef,
}

fn default_inactive_cursor() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct UiColors {
    tab_active_fg: ColorDef,
    tab_active_bg: ColorDef,
    tab_inactive_fg: ColorDef,
    tab_inactive_bg: ColorDef,
    tab_separator_bg: ColorDef,
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
    status_bar_fg: ColorDef,
    status_bar_bg: ColorDef,
    prompt_fg: ColorDef,
    prompt_bg: ColorDef,
    prompt_selection_fg: ColorDef,
    prompt_selection_bg: ColorDef,
    popup_border_fg: ColorDef,
    popup_bg: ColorDef,
    popup_selection_bg: ColorDef,
    popup_text_fg: ColorDef,
    suggestion_bg: ColorDef,
    suggestion_selected_bg: ColorDef,
    help_bg: ColorDef,
    help_fg: ColorDef,
    help_key_fg: ColorDef,
    help_separator_fg: ColorDef,
    help_indicator_fg: ColorDef,
    help_indicator_bg: ColorDef,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SearchColors {
    match_bg: ColorDef,
    match_fg: ColorDef,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct DiagnosticColors {
    error_fg: ColorDef,
    error_bg: ColorDef,
    warning_fg: ColorDef,
    warning_bg: ColorDef,
    info_fg: ColorDef,
    info_bg: ColorDef,
    hint_fg: ColorDef,
    hint_bg: ColorDef,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SyntaxColors {
    keyword: ColorDef,
    string: ColorDef,
    comment: ColorDef,
    function: ColorDef,
    #[serde(rename = "type")]
    type_: ColorDef,
    variable: ColorDef,
    constant: ColorDef,
    operator: ColorDef,
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

    // UI element colors
    pub tab_active_fg: Color,
    pub tab_active_bg: Color,
    pub tab_inactive_fg: Color,
    pub tab_inactive_bg: Color,
    pub tab_separator_bg: Color,

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

    pub split_separator_fg: Color,
    pub split_separator_hover_fg: Color,

    // Scrollbar colors
    pub scrollbar_track_fg: Color,
    pub scrollbar_thumb_fg: Color,
    pub scrollbar_track_hover_fg: Color,
    pub scrollbar_thumb_hover_fg: Color,

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
            tab_active_fg: file.ui.tab_active_fg.into(),
            tab_active_bg: file.ui.tab_active_bg.into(),
            tab_inactive_fg: file.ui.tab_inactive_fg.into(),
            tab_inactive_bg: file.ui.tab_inactive_bg.into(),
            tab_separator_bg: file.ui.tab_separator_bg.into(),
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
            split_separator_fg: file.ui.split_separator_fg.into(),
            split_separator_hover_fg: file.ui.split_separator_hover_fg.into(),
            scrollbar_track_fg: file.ui.scrollbar_track_fg.into(),
            scrollbar_thumb_fg: file.ui.scrollbar_thumb_fg.into(),
            scrollbar_track_hover_fg: file.ui.scrollbar_track_hover_fg.into(),
            scrollbar_thumb_hover_fg: file.ui.scrollbar_thumb_hover_fg.into(),
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
        // Try to load from the themes directory in the project root
        let theme_paths = [
            format!("themes/{}.json", name),
            format!("../themes/{}.json", name),
            format!("../../themes/{}.json", name),
        ];

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
            cursor: Color::Rgb(82, 139, 255),
            inactive_cursor: Color::Rgb(100, 100, 100),
            selection_bg: Color::Rgb(38, 79, 120),
            current_line_bg: Color::Rgb(40, 40, 40),
            line_number_fg: Color::Rgb(133, 133, 133),
            line_number_bg: Color::Rgb(30, 30, 30),

            // UI element colors
            tab_active_fg: Color::Yellow,
            tab_active_bg: Color::Blue,
            tab_inactive_fg: Color::White,
            tab_inactive_bg: Color::DarkGray,
            tab_separator_bg: Color::Rgb(45, 45, 48),

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

            status_bar_fg: Color::Black,
            status_bar_bg: Color::White,
            prompt_fg: Color::Black,
            prompt_bg: Color::Yellow,
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

            split_separator_fg: Color::DarkGray,
            split_separator_hover_fg: Color::Rgb(100, 149, 237), // Cornflower blue

            // Scrollbar colors
            scrollbar_track_fg: Color::DarkGray,
            scrollbar_thumb_fg: Color::Gray,
            scrollbar_track_hover_fg: Color::Gray,
            scrollbar_thumb_hover_fg: Color::White,

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
            cursor: Color::Rgb(0, 0, 255),
            inactive_cursor: Color::Rgb(180, 180, 180),
            selection_bg: Color::Rgb(173, 214, 255),
            current_line_bg: Color::Rgb(245, 245, 245),
            line_number_fg: Color::Rgb(133, 133, 133),
            line_number_bg: Color::Rgb(255, 255, 255),

            // UI element colors
            tab_active_fg: Color::Black,
            tab_active_bg: Color::Cyan,
            tab_inactive_fg: Color::Black,
            tab_inactive_bg: Color::Gray,
            tab_separator_bg: Color::White,

            // Menu bar colors
            menu_bg: Color::Rgb(240, 240, 240),
            menu_fg: Color::Rgb(40, 40, 40),
            menu_active_bg: Color::Rgb(200, 200, 200),
            menu_active_fg: Color::Rgb(0, 0, 0),
            menu_dropdown_bg: Color::Rgb(250, 250, 250),
            menu_dropdown_fg: Color::Rgb(40, 40, 40),
            menu_highlight_bg: Color::Rgb(70, 130, 180),
            menu_highlight_fg: Color::Rgb(255, 255, 255),
            menu_border_fg: Color::Rgb(150, 150, 150),
            menu_separator_fg: Color::Rgb(200, 200, 200),
            menu_hover_bg: Color::Rgb(220, 220, 220),
            menu_hover_fg: Color::Rgb(0, 0, 0),

            status_bar_fg: Color::White,
            status_bar_bg: Color::Blue,
            prompt_fg: Color::Black,
            prompt_bg: Color::Yellow,
            prompt_selection_fg: Color::Black,
            prompt_selection_bg: Color::Rgb(173, 214, 255), // Light blue selection

            popup_border_fg: Color::DarkGray,
            popup_bg: Color::Rgb(255, 255, 255),
            popup_selection_bg: Color::Rgb(173, 214, 255),
            popup_text_fg: Color::Black,

            suggestion_bg: Color::Rgb(255, 255, 255),
            suggestion_selected_bg: Color::Rgb(173, 214, 255),

            help_bg: Color::White,
            help_fg: Color::Black,
            help_key_fg: Color::Blue,
            help_separator_fg: Color::Gray,

            help_indicator_fg: Color::Red,
            help_indicator_bg: Color::White,

            split_separator_fg: Color::Gray,
            split_separator_hover_fg: Color::Rgb(70, 130, 180), // Steel blue

            // Scrollbar colors
            scrollbar_track_fg: Color::Gray,
            scrollbar_thumb_fg: Color::DarkGray,
            scrollbar_track_hover_fg: Color::DarkGray,
            scrollbar_thumb_hover_fg: Color::Black,

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

            // Syntax highlighting colors (VSCode Light+ palette)
            syntax_keyword: Color::Rgb(0, 0, 255),
            syntax_string: Color::Rgb(163, 21, 21),
            syntax_comment: Color::Rgb(0, 128, 0),
            syntax_function: Color::Rgb(121, 94, 38),
            syntax_type: Color::Rgb(38, 127, 153),
            syntax_variable: Color::Rgb(0, 0, 0),
            syntax_constant: Color::Rgb(0, 112, 193),
            syntax_operator: Color::Rgb(0, 0, 0),
        }
    }

    /// High contrast theme for accessibility
    pub fn high_contrast() -> Self {
        Self {
            name: "high-contrast".to_string(),

            // Editor colors
            editor_bg: Color::Black,
            editor_fg: Color::White,
            cursor: Color::Yellow,
            inactive_cursor: Color::DarkGray,
            selection_bg: Color::Rgb(0, 100, 200),
            current_line_bg: Color::Rgb(20, 20, 20),
            line_number_fg: Color::Yellow,
            line_number_bg: Color::Black,

            // UI element colors
            tab_active_fg: Color::Black,
            tab_active_bg: Color::Yellow,
            tab_inactive_fg: Color::White,
            tab_inactive_bg: Color::Black,
            tab_separator_bg: Color::Rgb(30, 30, 35),

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

            status_bar_fg: Color::Black,
            status_bar_bg: Color::Yellow,
            prompt_fg: Color::Black,
            prompt_bg: Color::Cyan,
            prompt_selection_fg: Color::White,
            prompt_selection_bg: Color::Rgb(0, 100, 200), // Blue selection

            popup_border_fg: Color::Yellow,
            popup_bg: Color::Black,
            popup_selection_bg: Color::Rgb(0, 100, 200),
            popup_text_fg: Color::White,

            suggestion_bg: Color::Black,
            suggestion_selected_bg: Color::Rgb(0, 100, 200),

            help_bg: Color::Black,
            help_fg: Color::White,
            help_key_fg: Color::Yellow,
            help_separator_fg: Color::White,

            help_indicator_fg: Color::Red,
            help_indicator_bg: Color::Black,

            split_separator_fg: Color::White,
            split_separator_hover_fg: Color::Yellow,

            // Scrollbar colors
            scrollbar_track_fg: Color::White,
            scrollbar_thumb_fg: Color::Yellow,
            scrollbar_track_hover_fg: Color::Yellow,
            scrollbar_thumb_hover_fg: Color::Cyan,

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
            _ => Self::dark(),
        }
    }

    /// Get all available theme names
    pub fn available_themes() -> Vec<&'static str> {
        vec!["dark", "light", "high-contrast", "nord", "dracula", "solarized-dark"]
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
        assert_eq!(themes.len(), 6);
        assert!(themes.contains(&"dark"));
        assert!(themes.contains(&"light"));
        assert!(themes.contains(&"high-contrast"));
        assert!(themes.contains(&"nord"));
        assert!(themes.contains(&"dracula"));
        assert!(themes.contains(&"solarized-dark"));
    }

    #[test]
    fn test_default_theme() {
        let theme = Theme::default();
        assert_eq!(theme.name, "high-contrast");
    }
}
