use ratatui::style::Color;

/// Comprehensive theme structure with all UI colors
#[derive(Debug, Clone)]
pub struct Theme {
    /// Theme name (e.g., "dark", "light", "high-contrast")
    pub name: String,

    // Editor colors
    pub editor_bg: Color,
    pub editor_fg: Color,
    pub cursor: Color,
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

impl Theme {
    /// Default dark theme (VSCode Dark+ inspired)
    pub fn dark() -> Self {
        Self {
            name: "dark".to_string(),

            // Editor colors
            editor_bg: Color::Rgb(30, 30, 30),
            editor_fg: Color::Rgb(212, 212, 212),
            cursor: Color::Rgb(82, 139, 255),
            selection_bg: Color::Rgb(38, 79, 120),
            current_line_bg: Color::Rgb(40, 40, 40),
            line_number_fg: Color::Rgb(133, 133, 133),
            line_number_bg: Color::Rgb(30, 30, 30),

            // UI element colors
            tab_active_fg: Color::Yellow,
            tab_active_bg: Color::Blue,
            tab_inactive_fg: Color::White,
            tab_inactive_bg: Color::DarkGray,
            tab_separator_bg: Color::Black,

            status_bar_fg: Color::Black,
            status_bar_bg: Color::White,
            prompt_fg: Color::Black,
            prompt_bg: Color::Yellow,
            prompt_selection_fg: Color::White,
            prompt_selection_bg: Color::Rgb(58, 79, 120),  // Blue selection

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

            status_bar_fg: Color::White,
            status_bar_bg: Color::Blue,
            prompt_fg: Color::Black,
            prompt_bg: Color::Yellow,
            prompt_selection_fg: Color::Black,
            prompt_selection_bg: Color::Rgb(173, 214, 255),  // Light blue selection

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
            selection_bg: Color::Rgb(0, 100, 200),
            current_line_bg: Color::Rgb(20, 20, 20),
            line_number_fg: Color::Yellow,
            line_number_bg: Color::Black,

            // UI element colors
            tab_active_fg: Color::Black,
            tab_active_bg: Color::Yellow,
            tab_inactive_fg: Color::White,
            tab_inactive_bg: Color::Black,
            tab_separator_bg: Color::Black,

            status_bar_fg: Color::Black,
            status_bar_bg: Color::Yellow,
            prompt_fg: Color::Black,
            prompt_bg: Color::Cyan,
            prompt_selection_fg: Color::White,
            prompt_selection_bg: Color::Rgb(0, 100, 200),  // Blue selection

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
    pub fn from_name(name: &str) -> Self {
        match name.to_lowercase().as_str() {
            "light" => Self::light(),
            "high-contrast" | "high_contrast" => Self::high_contrast(),
            _ => Self::dark(),
        }
    }

    /// Get all available theme names
    pub fn available_themes() -> Vec<&'static str> {
        vec!["dark", "light", "high-contrast"]
    }
}

impl Default for Theme {
    fn default() -> Self {
        Self::dark()
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
        assert_eq!(themes.len(), 3);
        assert!(themes.contains(&"dark"));
        assert!(themes.contains(&"light"));
        assert!(themes.contains(&"high-contrast"));
    }

    #[test]
    fn test_default_theme() {
        let theme = Theme::default();
        assert_eq!(theme.name, "dark");
    }
}
