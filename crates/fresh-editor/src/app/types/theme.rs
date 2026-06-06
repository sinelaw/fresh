/// Lightweight per-cell theme key provenance recorded during rendering.
/// Stored in `ChromeLayout::cell_theme_map` so the theme inspector popup
/// can look up the exact keys used for any screen position.
#[derive(Debug, Clone, Default)]
pub struct CellThemeInfo {
    /// Foreground theme key (e.g. "syntax.keyword", "editor.fg")
    pub fg_key: Option<&'static str>,
    /// Background theme key (e.g. "editor.bg", "diagnostic.warning_bg")
    pub bg_key: Option<&'static str>,
    /// Short region label (e.g. "Line Numbers", "Editor Content")
    pub region: &'static str,
    /// Dynamic region suffix (e.g. syntax category display name appended to "Syntax: ")
    pub syntax_category: Option<&'static str>,
}

/// Information about which theme key(s) style a specific screen position.
/// Used by the Ctrl+Right-Click theme inspector popup.
#[derive(Debug, Clone)]
pub struct ThemeKeyInfo {
    /// The foreground theme key path (e.g., "syntax.keyword", "editor.fg")
    pub fg_key: Option<String>,
    /// The background theme key path (e.g., "editor.bg", "editor.selection_bg")
    pub bg_key: Option<String>,
    /// Human-readable description of the UI region
    pub region: String,
    /// The actual foreground color value currently applied
    pub fg_color: Option<ratatui::style::Color>,
    /// The actual background color value currently applied
    pub bg_color: Option<ratatui::style::Color>,
    /// For syntax highlights: the HighlightCategory display name
    pub syntax_category: Option<String>,
}

/// State for the theme inspector popup (Ctrl+Right-Click)
#[derive(Debug, Clone)]
pub struct ThemeInfoPopup {
    /// Screen position where popup appears (x, y)
    pub position: (u16, u16),
    /// Resolved theme key information
    pub info: ThemeKeyInfo,
    /// Whether the "Open in Theme Editor" button is highlighted (mouse hover)
    pub button_highlighted: bool,
}
