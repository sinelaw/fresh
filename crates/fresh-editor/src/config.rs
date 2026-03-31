use crate::types::{context_keys, LspLanguageConfig, LspServerConfig, ProcessLimits};

use rust_i18n::t;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;
use std::path::Path;

/// Newtype for theme name that generates proper JSON Schema with enum options
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ThemeName(pub String);

impl ThemeName {
    /// Built-in theme options shown in the settings dropdown
    pub const BUILTIN_OPTIONS: &'static [&'static str] =
        &["dark", "light", "high-contrast", "nostalgia"];
}

impl Deref for ThemeName {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<String> for ThemeName {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for ThemeName {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl PartialEq<str> for ThemeName {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<ThemeName> for str {
    fn eq(&self, other: &ThemeName) -> bool {
        self == other.0
    }
}

impl JsonSchema for ThemeName {
    fn schema_name() -> Cow<'static, str> {
        Cow::Borrowed("ThemeOptions")
    }

    fn json_schema(_gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "description": "Available color themes",
            "type": "string",
            "enum": Self::BUILTIN_OPTIONS
        })
    }
}

/// Newtype for locale name that generates proper JSON Schema with enum options
/// Wraps Option<String> to allow null for auto-detection from environment
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(transparent)]
pub struct LocaleName(pub Option<String>);

// Include the generated locale options from build.rs
include!(concat!(env!("OUT_DIR"), "/locale_options.rs"));

impl LocaleName {
    /// Available locale options shown in the settings dropdown
    /// null means auto-detect from environment
    /// This is auto-generated from the locales/*.json files by build.rs
    pub const LOCALE_OPTIONS: &'static [Option<&'static str>] = GENERATED_LOCALE_OPTIONS;

    /// Get the inner value as Option<&str>
    pub fn as_option(&self) -> Option<&str> {
        self.0.as_deref()
    }
}

impl From<Option<String>> for LocaleName {
    fn from(s: Option<String>) -> Self {
        Self(s)
    }
}

impl From<Option<&str>> for LocaleName {
    fn from(s: Option<&str>) -> Self {
        Self(s.map(|s| s.to_string()))
    }
}

impl JsonSchema for LocaleName {
    fn schema_name() -> Cow<'static, str> {
        Cow::Borrowed("LocaleOptions")
    }

    fn json_schema(_gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "description": "UI locale (language). Use null for auto-detection from environment.",
            "enum": Self::LOCALE_OPTIONS
        })
    }
}

/// Cursor style options for the terminal cursor
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum CursorStyle {
    /// Use the terminal's default cursor style
    #[default]
    Default,
    /// Blinking block cursor (█)
    BlinkingBlock,
    /// Solid block cursor (█)
    SteadyBlock,
    /// Blinking vertical bar cursor (│)
    BlinkingBar,
    /// Solid vertical bar cursor (│)
    SteadyBar,
    /// Blinking underline cursor (_)
    BlinkingUnderline,
    /// Solid underline cursor (_)
    SteadyUnderline,
}

impl CursorStyle {
    /// All available cursor style options
    pub const OPTIONS: &'static [&'static str] = &[
        "default",
        "blinking_block",
        "steady_block",
        "blinking_bar",
        "steady_bar",
        "blinking_underline",
        "steady_underline",
    ];

    /// Human-readable descriptions for each cursor style
    pub const DESCRIPTIONS: &'static [&'static str] = &[
        "Terminal default",
        "█ Blinking block",
        "█ Solid block",
        "│ Blinking bar",
        "│ Solid bar",
        "_ Blinking underline",
        "_ Solid underline",
    ];

    /// Convert to crossterm cursor style (runtime only)
    #[cfg(feature = "runtime")]
    pub fn to_crossterm_style(self) -> crossterm::cursor::SetCursorStyle {
        use crossterm::cursor::SetCursorStyle;
        match self {
            Self::Default => SetCursorStyle::DefaultUserShape,
            Self::BlinkingBlock => SetCursorStyle::BlinkingBlock,
            Self::SteadyBlock => SetCursorStyle::SteadyBlock,
            Self::BlinkingBar => SetCursorStyle::BlinkingBar,
            Self::SteadyBar => SetCursorStyle::SteadyBar,
            Self::BlinkingUnderline => SetCursorStyle::BlinkingUnderScore,
            Self::SteadyUnderline => SetCursorStyle::SteadyUnderScore,
        }
    }

    /// Get the ANSI escape sequence for this cursor style (DECSCUSR)
    /// Used for session mode where we can't write directly to terminal
    pub fn to_escape_sequence(self) -> &'static [u8] {
        match self {
            Self::Default => b"\x1b[0 q",
            Self::BlinkingBlock => b"\x1b[1 q",
            Self::SteadyBlock => b"\x1b[2 q",
            Self::BlinkingUnderline => b"\x1b[3 q",
            Self::SteadyUnderline => b"\x1b[4 q",
            Self::BlinkingBar => b"\x1b[5 q",
            Self::SteadyBar => b"\x1b[6 q",
        }
    }

    /// Parse from string (for command palette)
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "default" => Some(CursorStyle::Default),
            "blinking_block" => Some(CursorStyle::BlinkingBlock),
            "steady_block" => Some(CursorStyle::SteadyBlock),
            "blinking_bar" => Some(CursorStyle::BlinkingBar),
            "steady_bar" => Some(CursorStyle::SteadyBar),
            "blinking_underline" => Some(CursorStyle::BlinkingUnderline),
            "steady_underline" => Some(CursorStyle::SteadyUnderline),
            _ => None,
        }
    }

    /// Convert to string representation
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Default => "default",
            Self::BlinkingBlock => "blinking_block",
            Self::SteadyBlock => "steady_block",
            Self::BlinkingBar => "blinking_bar",
            Self::SteadyBar => "steady_bar",
            Self::BlinkingUnderline => "blinking_underline",
            Self::SteadyUnderline => "steady_underline",
        }
    }
}

impl JsonSchema for CursorStyle {
    fn schema_name() -> Cow<'static, str> {
        Cow::Borrowed("CursorStyle")
    }

    fn json_schema(_gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "description": "Terminal cursor style",
            "type": "string",
            "enum": Self::OPTIONS
        })
    }
}

/// Newtype for keybinding map name that generates proper JSON Schema with enum options
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct KeybindingMapName(pub String);

impl KeybindingMapName {
    /// Built-in keybinding map options shown in the settings dropdown
    pub const BUILTIN_OPTIONS: &'static [&'static str] =
        &["default", "emacs", "vscode", "macos", "macos-gui"];
}

impl Deref for KeybindingMapName {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<String> for KeybindingMapName {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for KeybindingMapName {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl PartialEq<str> for KeybindingMapName {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

/// Line ending format for new files
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LineEndingOption {
    /// Unix/Linux/macOS format (LF)
    #[default]
    Lf,
    /// Windows format (CRLF)
    Crlf,
    /// Classic Mac format (CR) - rare
    Cr,
}

impl LineEndingOption {
    /// Convert to the buffer's LineEnding type
    pub fn to_line_ending(&self) -> crate::model::buffer::LineEnding {
        match self {
            Self::Lf => crate::model::buffer::LineEnding::LF,
            Self::Crlf => crate::model::buffer::LineEnding::CRLF,
            Self::Cr => crate::model::buffer::LineEnding::CR,
        }
    }
}

impl JsonSchema for LineEndingOption {
    fn schema_name() -> Cow<'static, str> {
        Cow::Borrowed("LineEndingOption")
    }

    fn json_schema(_gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "description": "Default line ending format for new files",
            "type": "string",
            "enum": ["lf", "crlf", "cr"],
            "default": "lf"
        })
    }
}

impl PartialEq<KeybindingMapName> for str {
    fn eq(&self, other: &KeybindingMapName) -> bool {
        self == other.0
    }
}

impl JsonSchema for KeybindingMapName {
    fn schema_name() -> Cow<'static, str> {
        Cow::Borrowed("KeybindingMapOptions")
    }

    fn json_schema(_gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "description": "Available keybinding maps",
            "type": "string",
            "enum": Self::BUILTIN_OPTIONS
        })
    }
}

/// Main configuration structure
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Config {
    /// Configuration version (for migration support)
    /// Configs without this field are treated as version 0
    #[serde(default)]
    pub version: u32,

    /// Color theme name
    #[serde(default = "default_theme_name")]
    pub theme: ThemeName,

    /// UI locale (language) for translations
    /// If not set, auto-detected from environment (LC_ALL, LC_MESSAGES, LANG)
    #[serde(default)]
    pub locale: LocaleName,

    /// Check for new versions on startup (default: true).
    /// When enabled, also sends basic anonymous telemetry (version, OS, terminal type).
    #[serde(default = "default_true")]
    pub check_for_updates: bool,

    /// Editor behavior settings (indentation, line numbers, wrapping, etc.)
    #[serde(default)]
    pub editor: EditorConfig,

    /// File explorer panel settings
    #[serde(default)]
    pub file_explorer: FileExplorerConfig,

    /// File browser settings (Open File dialog)
    #[serde(default)]
    pub file_browser: FileBrowserConfig,

    /// Clipboard settings (which clipboard methods to use)
    #[serde(default)]
    pub clipboard: ClipboardConfig,

    /// Terminal settings
    #[serde(default)]
    pub terminal: TerminalConfig,

    /// Custom keybindings (overrides for the active map)
    #[serde(default)]
    pub keybindings: Vec<Keybinding>,

    /// Named keybinding maps (user can define custom maps here)
    /// Each map can optionally inherit from another map
    #[serde(default)]
    pub keybinding_maps: HashMap<String, KeymapConfig>,

    /// Active keybinding map name
    #[serde(default = "default_keybinding_map_name")]
    pub active_keybinding_map: KeybindingMapName,

    /// Per-language configuration overrides (tab size, formatters, etc.)
    #[serde(default)]
    pub languages: HashMap<String, LanguageConfig>,

    /// Fallback configuration for files whose type cannot be detected.
    /// Applied when no extension, filename, glob, or built-in detection matches.
    /// Useful for setting a default grammar (e.g., "bash") and comment_prefix
    /// for unrecognized .conf, .rc, .rules, etc. files.
    #[serde(default)]
    pub fallback: Option<LanguageConfig>,

    /// LSP server configurations by language.
    /// Each language maps to one or more server configs (multi-LSP support).
    /// Accepts both single-object and array forms for backwards compatibility.
    #[serde(default)]
    pub lsp: HashMap<String, LspLanguageConfig>,

    /// Warning notification settings
    #[serde(default)]
    pub warnings: WarningsConfig,

    /// Plugin configurations by plugin name
    /// Plugins are auto-discovered from the plugins directory.
    /// Use this to enable/disable specific plugins.
    #[serde(default)]
    #[schemars(extend("x-standalone-category" = true, "x-no-add" = true))]
    pub plugins: HashMap<String, PluginConfig>,

    /// Package manager settings for plugin/theme installation
    #[serde(default)]
    pub packages: PackagesConfig,
}

fn default_keybinding_map_name() -> KeybindingMapName {
    // On macOS, default to the macOS keymap which has Mac-specific bindings
    // (Ctrl+A/E for Home/End, Ctrl+Shift+Z for redo, etc.)
    if cfg!(target_os = "macos") {
        KeybindingMapName("macos".to_string())
    } else {
        KeybindingMapName("default".to_string())
    }
}

fn default_theme_name() -> ThemeName {
    ThemeName("high-contrast".to_string())
}

/// Resolved whitespace indicator visibility for a buffer.
///
/// These are the final resolved flags after applying master toggle,
/// global config, and per-language overrides. Used directly by the renderer.
#[derive(Debug, Clone, Copy)]
pub struct WhitespaceVisibility {
    pub spaces_leading: bool,
    pub spaces_inner: bool,
    pub spaces_trailing: bool,
    pub tabs_leading: bool,
    pub tabs_inner: bool,
    pub tabs_trailing: bool,
}

impl Default for WhitespaceVisibility {
    fn default() -> Self {
        // Match EditorConfig defaults: tabs all on, spaces all off
        Self {
            spaces_leading: false,
            spaces_inner: false,
            spaces_trailing: false,
            tabs_leading: true,
            tabs_inner: true,
            tabs_trailing: true,
        }
    }
}

impl WhitespaceVisibility {
    /// Resolve from EditorConfig flat fields (applying master toggle)
    pub fn from_editor_config(editor: &EditorConfig) -> Self {
        if !editor.whitespace_show {
            return Self {
                spaces_leading: false,
                spaces_inner: false,
                spaces_trailing: false,
                tabs_leading: false,
                tabs_inner: false,
                tabs_trailing: false,
            };
        }
        Self {
            spaces_leading: editor.whitespace_spaces_leading,
            spaces_inner: editor.whitespace_spaces_inner,
            spaces_trailing: editor.whitespace_spaces_trailing,
            tabs_leading: editor.whitespace_tabs_leading,
            tabs_inner: editor.whitespace_tabs_inner,
            tabs_trailing: editor.whitespace_tabs_trailing,
        }
    }

    /// Apply a language-level override for tab visibility.
    /// When the language sets `show_whitespace_tabs: false`, all tab positions are disabled.
    pub fn with_language_tab_override(mut self, show_whitespace_tabs: bool) -> Self {
        if !show_whitespace_tabs {
            self.tabs_leading = false;
            self.tabs_inner = false;
            self.tabs_trailing = false;
        }
        self
    }

    /// Returns true if any space indicator is enabled
    pub fn any_spaces(&self) -> bool {
        self.spaces_leading || self.spaces_inner || self.spaces_trailing
    }

    /// Returns true if any tab indicator is enabled
    pub fn any_tabs(&self) -> bool {
        self.tabs_leading || self.tabs_inner || self.tabs_trailing
    }

    /// Returns true if any indicator (space or tab) is enabled
    pub fn any_visible(&self) -> bool {
        self.any_spaces() || self.any_tabs()
    }

    /// Toggle all whitespace indicators on/off (master switch).
    /// When turning off, all positions are disabled.
    /// When turning on, restores to default visibility (tabs all on, spaces all off).
    pub fn toggle_all(&mut self) {
        if self.any_visible() {
            *self = Self {
                spaces_leading: false,
                spaces_inner: false,
                spaces_trailing: false,
                tabs_leading: false,
                tabs_inner: false,
                tabs_trailing: false,
            };
        } else {
            *self = Self::default();
        }
    }
}

/// Editor behavior configuration
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct EditorConfig {
    // ===== Display =====
    /// Show line numbers in the gutter (default for new buffers)
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub line_numbers: bool,

    /// Show line numbers relative to cursor position
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Display"))]
    pub relative_line_numbers: bool,

    /// Highlight the line containing the cursor
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub highlight_current_line: bool,

    /// Wrap long lines to fit the window width (default for new views)
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub line_wrap: bool,

    /// Indent wrapped continuation lines to match the leading whitespace of the original line
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub wrap_indent: bool,

    /// Column at which to wrap lines when line wrapping is enabled.
    /// If not specified (`null`), lines wrap at the viewport edge (default behavior).
    /// Example: `80` wraps at column 80. The actual wrap column is clamped to the
    /// viewport width (lines can't wrap beyond the visible area).
    #[serde(default)]
    #[schemars(extend("x-section" = "Display"))]
    pub wrap_column: Option<usize>,

    /// Width of the page in page view mode (in columns).
    /// Controls the content width when page view is active, with centering margins.
    /// Defaults to 80. Set to `null` to use the full viewport width.
    #[serde(default = "default_page_width")]
    #[schemars(extend("x-section" = "Display"))]
    pub page_width: Option<usize>,

    /// Enable syntax highlighting for code files
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub syntax_highlighting: bool,

    /// Whether the menu bar is visible by default.
    /// The menu bar provides access to menus (File, Edit, View, etc.) at the top of the screen.
    /// Can be toggled at runtime via command palette or keybinding.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub show_menu_bar: bool,

    /// Whether menu bar mnemonics (Alt+letter shortcuts) are enabled.
    /// When enabled, pressing Alt+F opens the File menu, Alt+E opens Edit, etc.
    /// Disabling this frees up Alt+letter keybindings for other actions.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub menu_bar_mnemonics: bool,

    /// Whether the tab bar is visible by default.
    /// The tab bar shows open files in each split pane.
    /// Can be toggled at runtime via command palette or keybinding.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub show_tab_bar: bool,

    /// Whether the status bar is visible by default.
    /// The status bar shows file info, cursor position, and editor status at the bottom of the screen.
    /// Can be toggled at runtime via command palette or keybinding.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub show_status_bar: bool,

    /// Whether the prompt line is visible by default.
    /// The prompt line is the bottom-most line used for command input, search, file open, etc.
    /// When hidden, the prompt line only appears when a prompt is active.
    /// Can be toggled at runtime via command palette or keybinding.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub show_prompt_line: bool,

    /// Whether the vertical scrollbar is visible in each split pane.
    /// Can be toggled at runtime via command palette or keybinding.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub show_vertical_scrollbar: bool,

    /// Whether the horizontal scrollbar is visible in each split pane.
    /// The horizontal scrollbar appears when line wrap is disabled and content extends beyond the viewport.
    /// Can be toggled at runtime via command palette or keybinding.
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Display"))]
    pub show_horizontal_scrollbar: bool,

    /// Show tilde (~) markers on lines after the end of the file.
    /// These vim-style markers indicate lines that are not part of the file content.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Display"))]
    pub show_tilde: bool,

    /// Use the terminal's default background color instead of the theme's editor background.
    /// When enabled, the editor background inherits from the terminal emulator,
    /// allowing transparency or custom terminal backgrounds to show through.
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Display"))]
    pub use_terminal_bg: bool,

    /// Cursor style for the terminal cursor.
    /// Options: blinking_block, steady_block, blinking_bar, steady_bar, blinking_underline, steady_underline
    /// Default: blinking_block
    #[serde(default)]
    #[schemars(extend("x-section" = "Display"))]
    pub cursor_style: CursorStyle,

    /// Vertical ruler lines at specific column positions.
    /// Draws subtle vertical lines to help with line length conventions.
    /// Example: [80, 120] draws rulers at columns 80 and 120.
    /// Default: [] (no rulers)
    #[serde(default)]
    #[schemars(extend("x-section" = "Display"))]
    pub rulers: Vec<usize>,

    // ===== Whitespace =====
    /// Master toggle for whitespace indicator visibility.
    /// When disabled, no whitespace indicators (·, →) are shown regardless
    /// of the per-position settings below.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Whitespace"))]
    pub whitespace_show: bool,

    /// Show space indicators (·) for leading whitespace (indentation).
    /// Leading whitespace is everything before the first non-space character on a line.
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Whitespace"))]
    pub whitespace_spaces_leading: bool,

    /// Show space indicators (·) for inner whitespace (between words/tokens).
    /// Inner whitespace is spaces between the first and last non-space characters.
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Whitespace"))]
    pub whitespace_spaces_inner: bool,

    /// Show space indicators (·) for trailing whitespace.
    /// Trailing whitespace is everything after the last non-space character on a line.
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Whitespace"))]
    pub whitespace_spaces_trailing: bool,

    /// Show tab indicators (→) for leading tabs (indentation).
    /// Can be overridden per-language via `show_whitespace_tabs` in language config.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Whitespace"))]
    pub whitespace_tabs_leading: bool,

    /// Show tab indicators (→) for inner tabs (between words/tokens).
    /// Can be overridden per-language via `show_whitespace_tabs` in language config.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Whitespace"))]
    pub whitespace_tabs_inner: bool,

    /// Show tab indicators (→) for trailing tabs.
    /// Can be overridden per-language via `show_whitespace_tabs` in language config.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Whitespace"))]
    pub whitespace_tabs_trailing: bool,

    // ===== Editing =====
    /// Whether pressing Tab inserts a tab character instead of spaces.
    /// This is the global default; individual languages can override it
    /// via their own `use_tabs` setting.
    /// Default: false (insert spaces)
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Editing"))]
    pub use_tabs: bool,

    /// Number of spaces per tab character
    #[serde(default = "default_tab_size")]
    #[schemars(extend("x-section" = "Editing"))]
    pub tab_size: usize,

    /// Automatically indent new lines based on the previous line
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Editing"))]
    pub auto_indent: bool,

    /// Automatically close brackets, parentheses, and quotes when typing.
    /// When enabled, typing an opening delimiter like `(`, `[`, `{`, `"`, `'`, or `` ` ``
    /// will automatically insert the matching closing delimiter.
    /// Also enables skip-over (moving past existing closing delimiters) and
    /// pair deletion (deleting both delimiters when backspacing between them).
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Editing"))]
    pub auto_close: bool,

    /// Automatically surround selected text with matching pairs when typing
    /// an opening delimiter. When enabled and text is selected, typing `(`, `[`,
    /// `{`, `"`, `'`, or `` ` `` wraps the selection instead of replacing it.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Editing"))]
    pub auto_surround: bool,

    /// Minimum lines to keep visible above/below cursor when scrolling
    #[serde(default = "default_scroll_offset")]
    #[schemars(extend("x-section" = "Editing"))]
    pub scroll_offset: usize,

    /// Default line ending format for new files.
    /// Files loaded from disk will use their detected line ending format.
    /// Options: "lf" (Unix/Linux/macOS), "crlf" (Windows), "cr" (Classic Mac)
    /// Default: "lf"
    #[serde(default)]
    #[schemars(extend("x-section" = "Editing"))]
    pub default_line_ending: LineEndingOption,

    /// Remove trailing whitespace from lines when saving.
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Editing"))]
    pub trim_trailing_whitespace_on_save: bool,

    /// Ensure files end with a newline when saving.
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Editing"))]
    pub ensure_final_newline_on_save: bool,

    // ===== Bracket Matching =====
    /// Highlight matching bracket pairs when cursor is on a bracket.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Bracket Matching"))]
    pub highlight_matching_brackets: bool,

    /// Use rainbow colors for nested brackets based on nesting depth.
    /// Requires highlight_matching_brackets to be enabled.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Bracket Matching"))]
    pub rainbow_brackets: bool,

    // ===== Completion =====
    /// Enable quick suggestions (VS Code-like behavior).
    /// When enabled, completion suggestions appear automatically while typing,
    /// not just on trigger characters (like `.` or `::`).
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Completion"))]
    pub quick_suggestions: bool,

    /// Delay in milliseconds before showing completion suggestions.
    /// Lower values (10-50ms) feel more responsive but may be distracting.
    /// Higher values (100-500ms) reduce noise while typing.
    /// Trigger characters (like `.`) bypass this delay.
    /// Default: 150
    #[serde(default = "default_quick_suggestions_delay")]
    #[schemars(extend("x-section" = "Completion"))]
    pub quick_suggestions_delay_ms: u64,

    /// Whether trigger characters (like `.`, `::`, `->`) immediately show completions.
    /// When true, typing a trigger character bypasses quick_suggestions_delay_ms.
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Completion"))]
    pub suggest_on_trigger_characters: bool,

    // ===== LSP =====
    /// Whether to enable LSP inlay hints (type hints, parameter hints, etc.)
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "LSP"))]
    pub enable_inlay_hints: bool,

    /// Whether to request full-document LSP semantic tokens.
    /// Range requests are still used when supported.
    /// Default: false (range-only to avoid heavy full refreshes).
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "LSP"))]
    pub enable_semantic_tokens_full: bool,

    /// Whether to show inline diagnostic text at the end of lines with errors/warnings.
    /// When enabled, the highest-severity diagnostic message is rendered after the
    /// source code on each affected line.
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Diagnostics"))]
    pub diagnostics_inline_text: bool,

    // ===== Mouse =====
    /// Whether mouse hover triggers LSP hover requests.
    /// When enabled, hovering over code with the mouse will show documentation.
    /// On Windows, this also controls the mouse tracking mode: when disabled,
    /// the editor uses xterm mode 1002 (cell motion — click, drag, release only);
    /// when enabled, it uses mode 1003 (all motion — full mouse movement tracking).
    /// Mode 1003 generates high event volume on Windows and may cause input
    /// corruption on some systems. On macOS and Linux this setting only controls
    /// LSP hover; the mouse tracking mode is always full motion.
    /// Default: true (macOS/Linux), false (Windows)
    #[serde(default = "default_mouse_hover_enabled")]
    #[schemars(extend("x-section" = "Mouse"))]
    pub mouse_hover_enabled: bool,

    /// Delay in milliseconds before a mouse hover triggers an LSP hover request.
    /// Lower values show hover info faster but may cause more LSP server load.
    /// Default: 500ms
    #[serde(default = "default_mouse_hover_delay")]
    #[schemars(extend("x-section" = "Mouse"))]
    pub mouse_hover_delay_ms: u64,

    /// Time window in milliseconds for detecting double-clicks.
    /// Two clicks within this time are treated as a double-click (word selection).
    /// Default: 500ms
    #[serde(default = "default_double_click_time")]
    #[schemars(extend("x-section" = "Mouse"))]
    pub double_click_time_ms: u64,

    /// Whether to enable persistent auto-save (save to original file on disk).
    /// When enabled, modified buffers are saved to their original file path
    /// at a configurable interval.
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Recovery"))]
    pub auto_save_enabled: bool,

    /// Interval in seconds for persistent auto-save.
    /// Modified buffers are saved to their original file at this interval.
    /// Only effective when auto_save_enabled is true.
    /// Default: 30 seconds
    #[serde(default = "default_auto_save_interval")]
    #[schemars(extend("x-section" = "Recovery"))]
    pub auto_save_interval_secs: u32,

    /// Whether to preserve unsaved changes in all buffers (file-backed and
    /// unnamed) across editor sessions (VS Code "hot exit" behavior).
    /// When enabled, modified buffers are backed up on clean exit and their
    /// unsaved changes are restored on next startup.  Unnamed (scratch)
    /// buffers are also persisted (Sublime Text / Notepad++ behavior).
    /// Default: true
    #[serde(default = "default_true", alias = "persist_unnamed_buffers")]
    #[schemars(extend("x-section" = "Recovery"))]
    pub hot_exit: bool,

    // ===== Recovery =====
    /// Whether to enable file recovery (Emacs-style auto-save)
    /// When enabled, buffers are periodically saved to recovery files
    /// so they can be recovered if the editor crashes.
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Recovery"))]
    pub recovery_enabled: bool,

    /// Interval in seconds for auto-recovery-save.
    /// Modified buffers are saved to recovery files at this interval.
    /// Only effective when recovery_enabled is true.
    /// Default: 2 seconds
    #[serde(default = "default_auto_recovery_save_interval")]
    #[schemars(extend("x-section" = "Recovery"))]
    pub auto_recovery_save_interval_secs: u32,

    /// Poll interval in milliseconds for auto-reverting open buffers.
    /// When auto-revert is enabled, file modification times are checked at this interval.
    /// Lower values detect external changes faster but use more CPU.
    /// Default: 2000ms (2 seconds)
    #[serde(default = "default_auto_revert_poll_interval")]
    #[schemars(extend("x-section" = "Recovery"))]
    pub auto_revert_poll_interval_ms: u64,

    // ===== Keyboard =====
    /// Enable keyboard enhancement: disambiguate escape codes using CSI-u sequences.
    /// This allows unambiguous reading of Escape and modified keys.
    /// Requires terminal support (kitty keyboard protocol).
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Keyboard"))]
    pub keyboard_disambiguate_escape_codes: bool,

    /// Enable keyboard enhancement: report key event types (repeat/release).
    /// Adds extra events when keys are autorepeated or released.
    /// Requires terminal support (kitty keyboard protocol).
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Keyboard"))]
    pub keyboard_report_event_types: bool,

    /// Enable keyboard enhancement: report alternate keycodes.
    /// Sends alternate keycodes in addition to the base keycode.
    /// Requires terminal support (kitty keyboard protocol).
    /// Default: true
    #[serde(default = "default_true")]
    #[schemars(extend("x-section" = "Keyboard"))]
    pub keyboard_report_alternate_keys: bool,

    /// Enable keyboard enhancement: report all keys as escape codes.
    /// Represents all keyboard events as CSI-u sequences.
    /// Required for repeat/release events on plain-text keys.
    /// Requires terminal support (kitty keyboard protocol).
    /// Default: false
    #[serde(default = "default_false")]
    #[schemars(extend("x-section" = "Keyboard"))]
    pub keyboard_report_all_keys_as_escape_codes: bool,

    // ===== Performance =====
    /// Maximum time in milliseconds for syntax highlighting per frame
    #[serde(default = "default_highlight_timeout")]
    #[schemars(extend("x-section" = "Performance"))]
    pub highlight_timeout_ms: u64,

    /// Undo history snapshot interval (number of edits between snapshots)
    #[serde(default = "default_snapshot_interval")]
    #[schemars(extend("x-section" = "Performance"))]
    pub snapshot_interval: usize,

    /// Number of bytes to look back/forward from the viewport for syntax highlighting context.
    /// Larger values improve accuracy for multi-line constructs (strings, comments, nested blocks)
    /// but may slow down highlighting for very large files.
    /// Default: 10KB (10000 bytes)
    #[serde(default = "default_highlight_context_bytes")]
    #[schemars(extend("x-section" = "Performance"))]
    pub highlight_context_bytes: usize,

    /// File size threshold in bytes for "large file" behavior
    /// Files larger than this will:
    /// - Skip LSP features
    /// - Use constant-size scrollbar thumb (1 char)
    ///
    /// Files smaller will count actual lines for accurate scrollbar rendering
    #[serde(default = "default_large_file_threshold")]
    #[schemars(extend("x-section" = "Performance"))]
    pub large_file_threshold_bytes: u64,

    /// Estimated average line length in bytes (used for large file line estimation)
    /// This is used by LineIterator to estimate line positions in large files
    /// without line metadata. Typical values: 80-120 bytes.
    #[serde(default = "default_estimated_line_length")]
    #[schemars(extend("x-section" = "Performance"))]
    pub estimated_line_length: usize,

    /// Maximum number of concurrent filesystem read requests.
    /// Used during line-feed scanning and other bulk I/O operations.
    /// Higher values improve throughput, especially for remote filesystems.
    /// Default: 64
    #[serde(default = "default_read_concurrency")]
    #[schemars(extend("x-section" = "Performance"))]
    pub read_concurrency: usize,

    /// Poll interval in milliseconds for refreshing expanded directories in the file explorer.
    /// Directory modification times are checked at this interval to detect new/deleted files.
    /// Lower values detect changes faster but use more CPU.
    /// Default: 3000ms (3 seconds)
    #[serde(default = "default_file_tree_poll_interval")]
    #[schemars(extend("x-section" = "Performance"))]
    pub file_tree_poll_interval_ms: u64,
}

fn default_tab_size() -> usize {
    4
}

/// Large file threshold in bytes
/// Files larger than this will use optimized algorithms (estimation, viewport-only parsing)
/// Files smaller will use exact algorithms (full line tracking, complete parsing)
pub const LARGE_FILE_THRESHOLD_BYTES: u64 = 1024 * 1024; // 1MB

fn default_large_file_threshold() -> u64 {
    LARGE_FILE_THRESHOLD_BYTES
}

/// Maximum lines to scan forward when computing indent-based fold end
/// for the fold toggle action (user-triggered, infrequent).
pub const INDENT_FOLD_MAX_SCAN_LINES: usize = 10_000;

/// Maximum lines to scan forward when checking foldability for gutter
/// indicators or click detection (called per-viewport-line during render).
pub const INDENT_FOLD_INDICATOR_MAX_SCAN: usize = 50;

/// Maximum lines to walk backward when searching for a fold header
/// that contains the cursor (in the fold toggle action).
pub const INDENT_FOLD_MAX_UPWARD_SCAN: usize = 200;

fn default_read_concurrency() -> usize {
    64
}

fn default_true() -> bool {
    true
}

fn default_false() -> bool {
    false
}

fn default_quick_suggestions_delay() -> u64 {
    150 // 150ms — fast enough to feel responsive, slow enough to not interrupt typing
}

fn default_scroll_offset() -> usize {
    3
}

fn default_highlight_timeout() -> u64 {
    5
}

fn default_snapshot_interval() -> usize {
    100
}

fn default_estimated_line_length() -> usize {
    80
}

fn default_auto_save_interval() -> u32 {
    30 // 30 seconds between persistent auto-saves
}

fn default_auto_recovery_save_interval() -> u32 {
    2 // 2 seconds between recovery saves
}

fn default_highlight_context_bytes() -> usize {
    10_000 // 10KB context for accurate syntax highlighting
}

fn default_mouse_hover_enabled() -> bool {
    !cfg!(windows)
}

fn default_mouse_hover_delay() -> u64 {
    500 // 500ms delay before showing hover info
}

fn default_double_click_time() -> u64 {
    500 // 500ms window for detecting double-clicks
}

fn default_auto_revert_poll_interval() -> u64 {
    2000 // 2 seconds between file mtime checks
}

fn default_file_tree_poll_interval() -> u64 {
    3000 // 3 seconds between directory mtime checks
}

impl Default for EditorConfig {
    fn default() -> Self {
        Self {
            use_tabs: false,
            tab_size: default_tab_size(),
            auto_indent: true,
            auto_close: true,
            auto_surround: true,
            line_numbers: true,
            relative_line_numbers: false,
            scroll_offset: default_scroll_offset(),
            syntax_highlighting: true,
            highlight_current_line: true,
            line_wrap: true,
            wrap_indent: true,
            wrap_column: None,
            page_width: default_page_width(),
            highlight_timeout_ms: default_highlight_timeout(),
            snapshot_interval: default_snapshot_interval(),
            large_file_threshold_bytes: default_large_file_threshold(),
            estimated_line_length: default_estimated_line_length(),
            enable_inlay_hints: true,
            enable_semantic_tokens_full: false,
            diagnostics_inline_text: false,
            auto_save_enabled: false,
            auto_save_interval_secs: default_auto_save_interval(),
            hot_exit: true,
            recovery_enabled: true,
            auto_recovery_save_interval_secs: default_auto_recovery_save_interval(),
            highlight_context_bytes: default_highlight_context_bytes(),
            mouse_hover_enabled: default_mouse_hover_enabled(),
            mouse_hover_delay_ms: default_mouse_hover_delay(),
            double_click_time_ms: default_double_click_time(),
            auto_revert_poll_interval_ms: default_auto_revert_poll_interval(),
            read_concurrency: default_read_concurrency(),
            file_tree_poll_interval_ms: default_file_tree_poll_interval(),
            default_line_ending: LineEndingOption::default(),
            trim_trailing_whitespace_on_save: false,
            ensure_final_newline_on_save: false,
            highlight_matching_brackets: true,
            rainbow_brackets: true,
            cursor_style: CursorStyle::default(),
            keyboard_disambiguate_escape_codes: true,
            keyboard_report_event_types: false,
            keyboard_report_alternate_keys: true,
            keyboard_report_all_keys_as_escape_codes: false,
            quick_suggestions: true,
            quick_suggestions_delay_ms: default_quick_suggestions_delay(),
            suggest_on_trigger_characters: true,
            show_menu_bar: true,
            menu_bar_mnemonics: true,
            show_tab_bar: true,
            show_status_bar: true,
            show_prompt_line: true,
            show_vertical_scrollbar: true,
            show_horizontal_scrollbar: false,
            show_tilde: true,
            use_terminal_bg: false,
            rulers: Vec::new(),
            whitespace_show: true,
            whitespace_spaces_leading: false,
            whitespace_spaces_inner: false,
            whitespace_spaces_trailing: false,
            whitespace_tabs_leading: true,
            whitespace_tabs_inner: true,
            whitespace_tabs_trailing: true,
        }
    }
}

/// File explorer configuration
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FileExplorerConfig {
    /// Whether to respect .gitignore files
    #[serde(default = "default_true")]
    pub respect_gitignore: bool,

    /// Whether to show hidden files (starting with .) by default
    #[serde(default = "default_false")]
    pub show_hidden: bool,

    /// Whether to show gitignored files by default
    #[serde(default = "default_false")]
    pub show_gitignored: bool,

    /// Custom patterns to ignore (in addition to .gitignore)
    #[serde(default)]
    pub custom_ignore_patterns: Vec<String>,

    /// Width of file explorer as percentage (0.0 to 1.0)
    #[serde(default = "default_explorer_width")]
    pub width: f32,
}

fn default_explorer_width() -> f32 {
    0.3 // 30% of screen width
}

/// Clipboard configuration
///
/// Controls which clipboard methods are used for copy/paste operations.
/// By default, all methods are enabled and the editor tries them in order:
/// 1. OSC 52 escape sequences (works in modern terminals like Kitty, Alacritty, Wezterm)
/// 2. System clipboard via X11/Wayland APIs (works in Gnome Console, XFCE Terminal, etc.)
/// 3. Internal clipboard (always available as fallback)
///
/// If you experience hangs or issues (e.g., when using PuTTY or certain SSH setups),
/// you can disable specific methods.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ClipboardConfig {
    /// Enable OSC 52 escape sequences for clipboard access (default: true)
    /// Disable this if your terminal doesn't support OSC 52 or if it causes hangs
    #[serde(default = "default_true")]
    pub use_osc52: bool,

    /// Enable system clipboard access via X11/Wayland APIs (default: true)
    /// Disable this if you don't have a display server or it causes issues
    #[serde(default = "default_true")]
    pub use_system_clipboard: bool,
}

impl Default for ClipboardConfig {
    fn default() -> Self {
        Self {
            use_osc52: true,
            use_system_clipboard: true,
        }
    }
}

/// Terminal configuration
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TerminalConfig {
    /// When viewing terminal scrollback and new output arrives,
    /// automatically jump back to terminal mode (default: true)
    #[serde(default = "default_true")]
    pub jump_to_end_on_output: bool,
}

impl Default for TerminalConfig {
    fn default() -> Self {
        Self {
            jump_to_end_on_output: true,
        }
    }
}

/// Warning notification configuration
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct WarningsConfig {
    /// Show warning/error indicators in the status bar (default: true)
    /// When enabled, displays a colored indicator for LSP errors and other warnings
    #[serde(default = "default_true")]
    pub show_status_indicator: bool,
}

impl Default for WarningsConfig {
    fn default() -> Self {
        Self {
            show_status_indicator: true,
        }
    }
}

/// Package manager configuration for plugins and themes
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct PackagesConfig {
    /// Registry sources (git repository URLs containing plugin/theme indices)
    /// Default: ["https://github.com/sinelaw/fresh-plugins-registry"]
    #[serde(default = "default_package_sources")]
    pub sources: Vec<String>,
}

fn default_package_sources() -> Vec<String> {
    vec!["https://github.com/sinelaw/fresh-plugins-registry".to_string()]
}

impl Default for PackagesConfig {
    fn default() -> Self {
        Self {
            sources: default_package_sources(),
        }
    }
}

// Re-export PluginConfig from fresh-core for shared type usage
pub use fresh_core::config::PluginConfig;

impl Default for FileExplorerConfig {
    fn default() -> Self {
        Self {
            respect_gitignore: true,
            show_hidden: false,
            show_gitignored: false,
            custom_ignore_patterns: Vec::new(),
            width: default_explorer_width(),
        }
    }
}

/// File browser configuration (for Open File dialog)
#[derive(Debug, Clone, Default, Serialize, Deserialize, JsonSchema)]
pub struct FileBrowserConfig {
    /// Whether to show hidden files (starting with .) by default in Open File dialog
    #[serde(default = "default_false")]
    pub show_hidden: bool,
}

/// A single key in a sequence
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct KeyPress {
    /// Key name (e.g., "a", "Enter", "F1")
    pub key: String,
    /// Modifiers (e.g., ["ctrl"], ["ctrl", "shift"])
    #[serde(default)]
    pub modifiers: Vec<String>,
}

/// Keybinding definition
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/action"))]
pub struct Keybinding {
    /// Key name (e.g., "a", "Enter", "F1") - for single-key bindings
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub key: String,

    /// Modifiers (e.g., ["ctrl"], ["ctrl", "shift"]) - for single-key bindings
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub modifiers: Vec<String>,

    /// Key sequence for chord bindings (e.g., [{"key": "x", "modifiers": ["ctrl"]}, {"key": "s", "modifiers": ["ctrl"]}])
    /// If present, takes precedence over key + modifiers
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub keys: Vec<KeyPress>,

    /// Action to perform (e.g., "insert_char", "move_left")
    pub action: String,

    /// Optional arguments for the action
    #[serde(default)]
    pub args: HashMap<String, serde_json::Value>,

    /// Optional condition (e.g., "mode == insert")
    #[serde(default)]
    pub when: Option<String>,
}

/// Keymap configuration (for built-in and user-defined keymaps)
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/inherits"))]
pub struct KeymapConfig {
    /// Optional parent keymap to inherit from
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inherits: Option<String>,

    /// Keybindings defined in this keymap
    #[serde(default)]
    pub bindings: Vec<Keybinding>,
}

/// Formatter configuration for a language
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/command"))]
pub struct FormatterConfig {
    /// The formatter command to run (e.g., "rustfmt", "prettier")
    pub command: String,

    /// Arguments to pass to the formatter
    /// Use "$FILE" to include the file path
    #[serde(default)]
    pub args: Vec<String>,

    /// Whether to pass buffer content via stdin (default: true)
    /// Most formatters read from stdin and write to stdout
    #[serde(default = "default_true")]
    pub stdin: bool,

    /// Timeout in milliseconds (default: 10000)
    #[serde(default = "default_on_save_timeout")]
    pub timeout_ms: u64,
}

/// Action to run when a file is saved (for linters, etc.)
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/command"))]
pub struct OnSaveAction {
    /// The shell command to run
    /// The file path is available as $FILE or as an argument
    pub command: String,

    /// Arguments to pass to the command
    /// Use "$FILE" to include the file path
    #[serde(default)]
    pub args: Vec<String>,

    /// Working directory for the command (defaults to project root)
    #[serde(default)]
    pub working_dir: Option<String>,

    /// Whether to use the buffer content as stdin
    #[serde(default)]
    pub stdin: bool,

    /// Timeout in milliseconds (default: 10000)
    #[serde(default = "default_on_save_timeout")]
    pub timeout_ms: u64,

    /// Whether this action is enabled (default: true)
    /// Set to false to disable an action without removing it from config
    #[serde(default = "default_true")]
    pub enabled: bool,
}

fn default_on_save_timeout() -> u64 {
    10000
}

fn default_page_width() -> Option<usize> {
    Some(80)
}

/// Language-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/grammar"))]
pub struct LanguageConfig {
    /// File extensions for this language (e.g., ["rs"] for Rust)
    #[serde(default)]
    pub extensions: Vec<String>,

    /// Exact filenames for this language (e.g., ["Makefile", "GNUmakefile"])
    #[serde(default)]
    pub filenames: Vec<String>,

    /// Tree-sitter grammar name
    #[serde(default)]
    pub grammar: String,

    /// Comment prefix
    #[serde(default)]
    pub comment_prefix: Option<String>,

    /// Whether to auto-indent
    #[serde(default = "default_true")]
    pub auto_indent: bool,

    /// Whether to auto-close brackets, parentheses, and quotes for this language.
    /// If not specified (`null`), falls back to the global `editor.auto_close` setting.
    #[serde(default)]
    pub auto_close: Option<bool>,

    /// Whether to auto-surround selected text with matching pairs for this language.
    /// If not specified (`null`), falls back to the global `editor.auto_surround` setting.
    #[serde(default)]
    pub auto_surround: Option<bool>,

    /// Preferred highlighter backend (auto, tree-sitter, or textmate)
    #[serde(default)]
    pub highlighter: HighlighterPreference,

    /// Path to custom TextMate grammar file (optional)
    /// If specified, this grammar will be used when highlighter is "textmate"
    #[serde(default)]
    pub textmate_grammar: Option<std::path::PathBuf>,

    /// Whether to show whitespace tab indicators (→) for this language
    /// Defaults to true. Set to false for languages like Go that use tabs for indentation.
    #[serde(default = "default_true")]
    pub show_whitespace_tabs: bool,

    /// Whether to enable line wrapping for this language.
    /// If not specified (`null`), falls back to the global `editor.line_wrap` setting.
    /// Useful for prose-heavy languages like Markdown where wrapping is desirable
    /// even if globally disabled.
    #[serde(default)]
    pub line_wrap: Option<bool>,

    /// Column at which to wrap lines for this language.
    /// If not specified (`null`), falls back to the global `editor.wrap_column` setting.
    #[serde(default)]
    pub wrap_column: Option<usize>,

    /// Whether to automatically enable page view (compose mode) for this language.
    /// Page view provides a document-style layout with centered content,
    /// concealed formatting markers, and intelligent word wrapping.
    /// If not specified (`null`), page view is not auto-activated.
    #[serde(default)]
    pub page_view: Option<bool>,

    /// Width of the page in page view mode (in columns).
    /// Controls the content width when page view is active, with centering margins.
    /// If not specified (`null`), falls back to the global `editor.page_width` setting.
    #[serde(default)]
    pub page_width: Option<usize>,

    /// Whether pressing Tab should insert a tab character instead of spaces.
    /// If not specified (`null`), falls back to the global `editor.use_tabs` setting.
    /// Set to true for languages like Go and Makefile that require tabs.
    #[serde(default)]
    pub use_tabs: Option<bool>,

    /// Tab size (number of spaces per tab) for this language.
    /// If not specified, falls back to the global editor.tab_size setting.
    #[serde(default)]
    pub tab_size: Option<usize>,

    /// The formatter for this language (used by format_buffer command)
    #[serde(default)]
    pub formatter: Option<FormatterConfig>,

    /// Whether to automatically format on save (uses the formatter above)
    #[serde(default)]
    pub format_on_save: bool,

    /// Actions to run when a file of this language is saved (linters, etc.)
    /// Actions are run in order; if any fails (non-zero exit), subsequent actions don't run
    /// Note: Use `formatter` + `format_on_save` for formatting, not on_save
    #[serde(default)]
    pub on_save: Vec<OnSaveAction>,

    /// Extra characters (beyond alphanumeric and `_`) considered part of
    /// identifiers for this language. Used by dabbrev and buffer-word
    /// completion to correctly tokenise language-specific naming conventions.
    ///
    /// Examples:
    /// - Lisp/Clojure/CSS: `"-"` (kebab-case identifiers)
    /// - PHP/Bash: `"$"` (variable sigils)
    /// - Ruby: `"?!"` (predicate/bang methods)
    /// - Rust (default): `""` (standard alphanumeric + underscore)
    #[serde(default)]
    pub word_characters: Option<String>,
}

/// Resolved editor configuration for a specific buffer.
///
/// This struct contains the effective settings for a buffer after applying
/// language-specific overrides on top of the global editor config.
///
/// Use `BufferConfig::resolve()` to create one from a Config and optional language ID.
#[derive(Debug, Clone)]
pub struct BufferConfig {
    /// Number of spaces per tab character
    pub tab_size: usize,

    /// Whether to insert a tab character (true) or spaces (false) when pressing Tab
    pub use_tabs: bool,

    /// Whether to auto-indent new lines
    pub auto_indent: bool,

    /// Whether to auto-close brackets, parentheses, and quotes
    pub auto_close: bool,

    /// Whether to surround selected text with matching pairs
    pub auto_surround: bool,

    /// Whether line wrapping is enabled for this buffer
    pub line_wrap: bool,

    /// Column at which to wrap lines (None = viewport width)
    pub wrap_column: Option<usize>,

    /// Resolved whitespace indicator visibility
    pub whitespace: WhitespaceVisibility,

    /// Formatter command for this buffer
    pub formatter: Option<FormatterConfig>,

    /// Whether to format on save
    pub format_on_save: bool,

    /// Actions to run when saving
    pub on_save: Vec<OnSaveAction>,

    /// Preferred highlighter backend
    pub highlighter: HighlighterPreference,

    /// Path to custom TextMate grammar (if any)
    pub textmate_grammar: Option<std::path::PathBuf>,

    /// Extra word-constituent characters for this language (for completion).
    /// Empty string means standard alphanumeric + underscore only.
    pub word_characters: String,
}

impl BufferConfig {
    /// Resolve the effective configuration for a buffer given its language.
    ///
    /// This merges the global editor settings with any language-specific overrides
    /// from `Config.languages`.
    ///
    /// # Arguments
    /// * `global_config` - The resolved global configuration
    /// * `language_id` - Optional language identifier (e.g., "rust", "python")
    pub fn resolve(global_config: &Config, language_id: Option<&str>) -> Self {
        let editor = &global_config.editor;

        // Start with global editor settings
        let mut whitespace = WhitespaceVisibility::from_editor_config(editor);
        let mut config = BufferConfig {
            tab_size: editor.tab_size,
            use_tabs: editor.use_tabs,
            auto_indent: editor.auto_indent,
            auto_close: editor.auto_close,
            auto_surround: editor.auto_surround,
            line_wrap: editor.line_wrap,
            wrap_column: editor.wrap_column,
            whitespace,
            formatter: None,
            format_on_save: false,
            on_save: Vec::new(),
            highlighter: HighlighterPreference::Auto,
            textmate_grammar: None,
            word_characters: String::new(),
        };

        // Apply language-specific overrides if available.
        // If no language config matches and the language is "text" (undetected),
        // try the fallback config (#1219).
        let lang_config_ref = language_id
            .and_then(|id| global_config.languages.get(id))
            .or({
                // Apply fallback only when language is unknown ("text" or None)
                match language_id {
                    None | Some("text") => global_config.fallback.as_ref(),
                    _ => None,
                }
            });
        if let Some(lang_config) = lang_config_ref {
            // Tab size: use language setting if specified, else global
            if let Some(ts) = lang_config.tab_size {
                config.tab_size = ts;
            }

            // Use tabs: language override (only if explicitly set)
            if let Some(use_tabs) = lang_config.use_tabs {
                config.use_tabs = use_tabs;
            }

            // Line wrap: language override (only if explicitly set)
            if let Some(line_wrap) = lang_config.line_wrap {
                config.line_wrap = line_wrap;
            }

            // Wrap column: language override (only if explicitly set)
            if lang_config.wrap_column.is_some() {
                config.wrap_column = lang_config.wrap_column;
            }

            // Auto indent: language override
            config.auto_indent = lang_config.auto_indent;

            // Auto close: language override (only if globally enabled)
            if config.auto_close {
                if let Some(lang_auto_close) = lang_config.auto_close {
                    config.auto_close = lang_auto_close;
                }
            }

            // Auto surround: language override (only if globally enabled)
            if config.auto_surround {
                if let Some(lang_auto_surround) = lang_config.auto_surround {
                    config.auto_surround = lang_auto_surround;
                }
            }

            // Whitespace tabs: language override can disable tab indicators
            whitespace = whitespace.with_language_tab_override(lang_config.show_whitespace_tabs);
            config.whitespace = whitespace;

            // Formatter: from language config
            config.formatter = lang_config.formatter.clone();

            // Format on save: from language config
            config.format_on_save = lang_config.format_on_save;

            // On save actions: from language config
            config.on_save = lang_config.on_save.clone();

            // Highlighter preference: from language config
            config.highlighter = lang_config.highlighter;

            // TextMate grammar path: from language config
            config.textmate_grammar = lang_config.textmate_grammar.clone();

            // Word characters: from language config
            if let Some(ref wc) = lang_config.word_characters {
                config.word_characters = wc.clone();
            }
        }

        config
    }

    /// Get the effective indentation string for this buffer.
    ///
    /// Returns a tab character if `use_tabs` is true, otherwise returns
    /// `tab_size` spaces.
    pub fn indent_string(&self) -> String {
        if self.use_tabs {
            "\t".to_string()
        } else {
            " ".repeat(self.tab_size)
        }
    }
}

/// Preference for which syntax highlighting backend to use
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "lowercase")]
pub enum HighlighterPreference {
    /// Use tree-sitter if available, fall back to TextMate
    #[default]
    Auto,
    /// Force tree-sitter only (no highlighting if unavailable)
    #[serde(rename = "tree-sitter")]
    TreeSitter,
    /// Force TextMate grammar (skip tree-sitter even if available)
    #[serde(rename = "textmate")]
    TextMate,
}

/// Menu bar configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize, JsonSchema)]
pub struct MenuConfig {
    /// List of top-level menus in the menu bar
    #[serde(default)]
    pub menus: Vec<Menu>,
}

// Re-export Menu and MenuItem from fresh-core for shared type usage
pub use fresh_core::menu::{Menu, MenuItem};

/// Extension trait for Menu with editor-specific functionality
pub trait MenuExt {
    /// Get the identifier for matching (id if set, otherwise label).
    /// This is used for keybinding matching and should be stable across translations.
    fn match_id(&self) -> &str;

    /// Expand all DynamicSubmenu items in this menu to regular Submenu items
    /// This should be called before the menu is used for rendering/navigation
    fn expand_dynamic_items(&mut self, themes_dir: &std::path::Path);
}

impl MenuExt for Menu {
    fn match_id(&self) -> &str {
        self.id.as_deref().unwrap_or(&self.label)
    }

    fn expand_dynamic_items(&mut self, themes_dir: &std::path::Path) {
        self.items = self
            .items
            .iter()
            .map(|item| item.expand_dynamic(themes_dir))
            .collect();
    }
}

/// Extension trait for MenuItem with editor-specific functionality
pub trait MenuItemExt {
    /// Expand a DynamicSubmenu into a regular Submenu with generated items.
    /// Returns the original item if not a DynamicSubmenu.
    fn expand_dynamic(&self, themes_dir: &std::path::Path) -> MenuItem;
}

impl MenuItemExt for MenuItem {
    fn expand_dynamic(&self, themes_dir: &std::path::Path) -> MenuItem {
        match self {
            MenuItem::DynamicSubmenu { label, source } => {
                let items = generate_dynamic_items(source, themes_dir);
                MenuItem::Submenu {
                    label: label.clone(),
                    items,
                }
            }
            other => other.clone(),
        }
    }
}

/// Generate menu items for a dynamic source (runtime only - requires view::theme)
#[cfg(feature = "runtime")]
pub fn generate_dynamic_items(source: &str, themes_dir: &std::path::Path) -> Vec<MenuItem> {
    match source {
        "copy_with_theme" => {
            // Generate theme options from available themes
            let loader = crate::view::theme::ThemeLoader::new(themes_dir.to_path_buf());
            let registry = loader.load_all(&[]);
            registry
                .list()
                .iter()
                .map(|info| {
                    let mut args = HashMap::new();
                    args.insert("theme".to_string(), serde_json::json!(info.name));
                    MenuItem::Action {
                        label: info.name.clone(),
                        action: "copy_with_theme".to_string(),
                        args,
                        when: Some(context_keys::HAS_SELECTION.to_string()),
                        checkbox: None,
                    }
                })
                .collect()
        }
        _ => vec![MenuItem::Label {
            info: format!("Unknown source: {}", source),
        }],
    }
}

/// Generate menu items for a dynamic source (WASM stub - returns empty)
#[cfg(not(feature = "runtime"))]
pub fn generate_dynamic_items(_source: &str, _themes_dir: &std::path::Path) -> Vec<MenuItem> {
    // Theme loading not available in WASM builds
    vec![]
}

impl Default for Config {
    fn default() -> Self {
        Self {
            version: 0,
            theme: default_theme_name(),
            locale: LocaleName::default(),
            check_for_updates: true,
            editor: EditorConfig::default(),
            file_explorer: FileExplorerConfig::default(),
            file_browser: FileBrowserConfig::default(),
            clipboard: ClipboardConfig::default(),
            terminal: TerminalConfig::default(),
            keybindings: vec![], // User customizations only; defaults come from active_keybinding_map
            keybinding_maps: HashMap::new(), // User-defined maps go here
            active_keybinding_map: default_keybinding_map_name(),
            languages: Self::default_languages(),
            fallback: None,
            lsp: Self::default_lsp_config(),
            warnings: WarningsConfig::default(),
            plugins: HashMap::new(), // Populated when scanning for plugins
            packages: PackagesConfig::default(),
        }
    }
}

impl MenuConfig {
    /// Create a MenuConfig with translated menus using the current locale
    pub fn translated() -> Self {
        Self {
            menus: Self::translated_menus(),
        }
    }

    /// Create default menu bar configuration with translated labels.
    ///
    /// This is the single source of truth for the editor's menu structure.
    /// Both the built-in TUI menu bar and the native GUI menu bar (e.g. macOS)
    /// are built from this definition.
    pub fn translated_menus() -> Vec<Menu> {
        vec![
            // File menu
            Menu {
                id: Some("File".to_string()),
                label: t!("menu.file").to_string(),
                when: None,
                items: vec![
                    MenuItem::Action {
                        label: t!("menu.file.new_file").to_string(),
                        action: "new".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.file.open_file").to_string(),
                        action: "open".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.file.save").to_string(),
                        action: "save".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.file.save_as").to_string(),
                        action: "save_as".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.file.revert").to_string(),
                        action: "revert".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.file.reload_with_encoding").to_string(),
                        action: "reload_with_encoding".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.file.close_buffer").to_string(),
                        action: "close".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.file.switch_project").to_string(),
                        action: "switch_project".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.file.detach").to_string(),
                        action: "detach".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::SESSION_MODE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.file.quit").to_string(),
                        action: "quit".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // Edit menu
            Menu {
                id: Some("Edit".to_string()),
                label: t!("menu.edit").to_string(),
                when: None,
                items: vec![
                    MenuItem::Action {
                        label: t!("menu.edit.undo").to_string(),
                        action: "undo".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.edit.redo").to_string(),
                        action: "redo".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.edit.cut").to_string(),
                        action: "cut".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::HAS_SELECTION.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.edit.copy").to_string(),
                        action: "copy".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::HAS_SELECTION.to_string()),
                        checkbox: None,
                    },
                    MenuItem::DynamicSubmenu {
                        label: t!("menu.edit.copy_with_formatting").to_string(),
                        source: "copy_with_theme".to_string(),
                    },
                    MenuItem::Action {
                        label: t!("menu.edit.paste").to_string(),
                        action: "paste".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.edit.select_all").to_string(),
                        action: "select_all".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.edit.find").to_string(),
                        action: "search".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.edit.find_in_selection").to_string(),
                        action: "find_in_selection".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::HAS_SELECTION.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.edit.find_next").to_string(),
                        action: "find_next".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.edit.find_previous").to_string(),
                        action: "find_previous".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.edit.replace").to_string(),
                        action: "query_replace".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.edit.delete_line").to_string(),
                        action: "delete_line".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.edit.format_buffer").to_string(),
                        action: "format_buffer".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FORMATTER_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.edit.settings").to_string(),
                        action: "open_settings".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.edit.keybinding_editor").to_string(),
                        action: "open_keybinding_editor".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // View menu
            Menu {
                id: Some("View".to_string()),
                label: t!("menu.view").to_string(),
                when: None,
                items: vec![
                    MenuItem::Action {
                        label: t!("menu.view.file_explorer").to_string(),
                        action: "toggle_file_explorer".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::FILE_EXPLORER.to_string()),
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.view.line_numbers").to_string(),
                        action: "toggle_line_numbers".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::LINE_NUMBERS.to_string()),
                    },
                    MenuItem::Action {
                        label: t!("menu.view.line_wrap").to_string(),
                        action: "toggle_line_wrap".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::LINE_WRAP.to_string()),
                    },
                    MenuItem::Action {
                        label: t!("menu.view.mouse_support").to_string(),
                        action: "toggle_mouse_capture".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::MOUSE_CAPTURE.to_string()),
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.view.vertical_scrollbar").to_string(),
                        action: "toggle_vertical_scrollbar".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::VERTICAL_SCROLLBAR.to_string()),
                    },
                    MenuItem::Action {
                        label: t!("menu.view.horizontal_scrollbar").to_string(),
                        action: "toggle_horizontal_scrollbar".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::HORIZONTAL_SCROLLBAR.to_string()),
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.view.set_background").to_string(),
                        action: "set_background".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.set_background_blend").to_string(),
                        action: "set_background_blend".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.set_page_width").to_string(),
                        action: "set_page_width".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.view.select_theme").to_string(),
                        action: "select_theme".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.select_locale").to_string(),
                        action: "select_locale".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.settings").to_string(),
                        action: "open_settings".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.calibrate_input").to_string(),
                        action: "calibrate_input".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.view.split_horizontal").to_string(),
                        action: "split_horizontal".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.split_vertical").to_string(),
                        action: "split_vertical".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.close_split").to_string(),
                        action: "close_split".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.scroll_sync").to_string(),
                        action: "toggle_scroll_sync".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::HAS_SAME_BUFFER_SPLITS.to_string()),
                        checkbox: Some(context_keys::SCROLL_SYNC.to_string()),
                    },
                    MenuItem::Action {
                        label: t!("menu.view.focus_next_split").to_string(),
                        action: "next_split".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.focus_prev_split").to_string(),
                        action: "prev_split".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.view.toggle_maximize_split").to_string(),
                        action: "toggle_maximize_split".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Submenu {
                        label: t!("menu.terminal").to_string(),
                        items: vec![
                            MenuItem::Action {
                                label: t!("menu.terminal.open").to_string(),
                                action: "open_terminal".to_string(),
                                args: HashMap::new(),
                                when: None,
                                checkbox: None,
                            },
                            MenuItem::Action {
                                label: t!("menu.terminal.close").to_string(),
                                action: "close_terminal".to_string(),
                                args: HashMap::new(),
                                when: None,
                                checkbox: None,
                            },
                            MenuItem::Separator { separator: true },
                            MenuItem::Action {
                                label: t!("menu.terminal.toggle_keyboard_capture").to_string(),
                                action: "toggle_keyboard_capture".to_string(),
                                args: HashMap::new(),
                                when: None,
                                checkbox: None,
                            },
                        ],
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Submenu {
                        label: t!("menu.view.keybinding_style").to_string(),
                        items: vec![
                            MenuItem::Action {
                                label: t!("menu.view.keybinding_default").to_string(),
                                action: "switch_keybinding_map".to_string(),
                                args: {
                                    let mut map = HashMap::new();
                                    map.insert("map".to_string(), serde_json::json!("default"));
                                    map
                                },
                                when: None,
                                checkbox: Some(context_keys::KEYMAP_DEFAULT.to_string()),
                            },
                            MenuItem::Action {
                                label: t!("menu.view.keybinding_emacs").to_string(),
                                action: "switch_keybinding_map".to_string(),
                                args: {
                                    let mut map = HashMap::new();
                                    map.insert("map".to_string(), serde_json::json!("emacs"));
                                    map
                                },
                                when: None,
                                checkbox: Some(context_keys::KEYMAP_EMACS.to_string()),
                            },
                            MenuItem::Action {
                                label: t!("menu.view.keybinding_vscode").to_string(),
                                action: "switch_keybinding_map".to_string(),
                                args: {
                                    let mut map = HashMap::new();
                                    map.insert("map".to_string(), serde_json::json!("vscode"));
                                    map
                                },
                                when: None,
                                checkbox: Some(context_keys::KEYMAP_VSCODE.to_string()),
                            },
                            MenuItem::Action {
                                label: "macOS GUI (⌘)".to_string(),
                                action: "switch_keybinding_map".to_string(),
                                args: {
                                    let mut map = HashMap::new();
                                    map.insert("map".to_string(), serde_json::json!("macos-gui"));
                                    map
                                },
                                when: None,
                                checkbox: Some(context_keys::KEYMAP_MACOS_GUI.to_string()),
                            },
                        ],
                    },
                ],
            },
            // Selection menu
            Menu {
                id: Some("Selection".to_string()),
                label: t!("menu.selection").to_string(),
                when: None,
                items: vec![
                    MenuItem::Action {
                        label: t!("menu.selection.select_all").to_string(),
                        action: "select_all".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.selection.select_word").to_string(),
                        action: "select_word".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.selection.select_line").to_string(),
                        action: "select_line".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.selection.expand_selection").to_string(),
                        action: "expand_selection".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.selection.add_cursor_above").to_string(),
                        action: "add_cursor_above".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.selection.add_cursor_below").to_string(),
                        action: "add_cursor_below".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.selection.add_cursor_next_match").to_string(),
                        action: "add_cursor_next_match".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.selection.remove_secondary_cursors").to_string(),
                        action: "remove_secondary_cursors".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // Go menu
            Menu {
                id: Some("Go".to_string()),
                label: t!("menu.go").to_string(),
                when: None,
                items: vec![
                    MenuItem::Action {
                        label: t!("menu.go.goto_line").to_string(),
                        action: "goto_line".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.go.goto_definition").to_string(),
                        action: "lsp_goto_definition".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.go.find_references").to_string(),
                        action: "lsp_references".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.go.next_buffer").to_string(),
                        action: "next_buffer".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.go.prev_buffer").to_string(),
                        action: "prev_buffer".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.go.command_palette").to_string(),
                        action: "command_palette".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // LSP menu
            Menu {
                id: Some("LSP".to_string()),
                label: t!("menu.lsp").to_string(),
                when: None,
                items: vec![
                    MenuItem::Action {
                        label: t!("menu.lsp.show_hover").to_string(),
                        action: "lsp_hover".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.lsp.goto_definition").to_string(),
                        action: "lsp_goto_definition".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.lsp.find_references").to_string(),
                        action: "lsp_references".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.lsp.rename_symbol").to_string(),
                        action: "lsp_rename".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.lsp.show_completions").to_string(),
                        action: "lsp_completion".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.lsp.show_signature").to_string(),
                        action: "lsp_signature_help".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.lsp.code_actions").to_string(),
                        action: "lsp_code_actions".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.lsp.toggle_inlay_hints").to_string(),
                        action: "toggle_inlay_hints".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: Some(context_keys::INLAY_HINTS.to_string()),
                    },
                    MenuItem::Action {
                        label: t!("menu.lsp.toggle_mouse_hover").to_string(),
                        action: "toggle_mouse_hover".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::MOUSE_HOVER.to_string()),
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.lsp.restart_server").to_string(),
                        action: "lsp_restart".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.lsp.stop_server").to_string(),
                        action: "lsp_stop".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.lsp.toggle_for_buffer").to_string(),
                        action: "lsp_toggle_for_buffer".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // Explorer menu (only visible when file explorer is focused)
            Menu {
                id: Some("Explorer".to_string()),
                label: t!("menu.explorer").to_string(),
                when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                items: vec![
                    MenuItem::Action {
                        label: t!("menu.explorer.new_file").to_string(),
                        action: "file_explorer_new_file".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.explorer.new_folder").to_string(),
                        action: "file_explorer_new_directory".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.explorer.open").to_string(),
                        action: "file_explorer_open".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.explorer.rename").to_string(),
                        action: "file_explorer_rename".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.explorer.delete").to_string(),
                        action: "file_explorer_delete".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.explorer.refresh").to_string(),
                        action: "file_explorer_refresh".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.explorer.show_hidden").to_string(),
                        action: "file_explorer_toggle_hidden".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER.to_string()),
                        checkbox: Some(context_keys::FILE_EXPLORER_SHOW_HIDDEN.to_string()),
                    },
                    MenuItem::Action {
                        label: t!("menu.explorer.show_gitignored").to_string(),
                        action: "file_explorer_toggle_gitignored".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER.to_string()),
                        checkbox: Some(context_keys::FILE_EXPLORER_SHOW_GITIGNORED.to_string()),
                    },
                ],
            },
            // Help menu
            Menu {
                id: Some("Help".to_string()),
                label: t!("menu.help").to_string(),
                when: None,
                items: vec![
                    MenuItem::Label {
                        info: format!("Fresh v{}", env!("CARGO_PKG_VERSION")),
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.help.show_manual").to_string(),
                        action: "show_help".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: t!("menu.help.keyboard_shortcuts").to_string(),
                        action: "keyboard_shortcuts".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: t!("menu.help.event_debug").to_string(),
                        action: "event_debug".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
        ]
    }
}

impl Config {
    /// The config filename used throughout the application
    pub(crate) const FILENAME: &'static str = "config.json";

    /// Get the local config path (in the working directory)
    pub(crate) fn local_config_path(working_dir: &Path) -> std::path::PathBuf {
        working_dir.join(Self::FILENAME)
    }

    /// Load configuration from a JSON file
    ///
    /// This deserializes the user's config file as a partial config and resolves
    /// it with system defaults. For HashMap fields like `lsp` and `languages`,
    /// entries from the user config are merged with the default entries.
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> Result<Self, ConfigError> {
        let contents = std::fs::read_to_string(path.as_ref())
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        // Deserialize as PartialConfig first, then resolve with defaults
        let partial: crate::partial_config::PartialConfig =
            serde_json::from_str(&contents).map_err(|e| ConfigError::ParseError(e.to_string()))?;

        Ok(partial.resolve())
    }

    /// Load a built-in keymap from embedded JSON
    fn load_builtin_keymap(name: &str) -> Option<KeymapConfig> {
        let json_content = match name {
            "default" => include_str!("../keymaps/default.json"),
            "emacs" => include_str!("../keymaps/emacs.json"),
            "vscode" => include_str!("../keymaps/vscode.json"),
            "macos" => include_str!("../keymaps/macos.json"),
            "macos-gui" => include_str!("../keymaps/macos-gui.json"),
            _ => return None,
        };

        match serde_json::from_str(json_content) {
            Ok(config) => Some(config),
            Err(e) => {
                eprintln!("Failed to parse builtin keymap '{}': {}", name, e);
                None
            }
        }
    }

    /// Resolve a keymap with inheritance
    /// Returns all bindings from the keymap and its parent chain
    pub fn resolve_keymap(&self, map_name: &str) -> Vec<Keybinding> {
        let mut visited = std::collections::HashSet::new();
        self.resolve_keymap_recursive(map_name, &mut visited)
    }

    /// Recursive helper for resolve_keymap
    fn resolve_keymap_recursive(
        &self,
        map_name: &str,
        visited: &mut std::collections::HashSet<String>,
    ) -> Vec<Keybinding> {
        // Prevent infinite loops
        if visited.contains(map_name) {
            eprintln!(
                "Warning: Circular inheritance detected in keymap '{}'",
                map_name
            );
            return Vec::new();
        }
        visited.insert(map_name.to_string());

        // Try to load the keymap (user-defined or built-in)
        let keymap = self
            .keybinding_maps
            .get(map_name)
            .cloned()
            .or_else(|| Self::load_builtin_keymap(map_name));

        let Some(keymap) = keymap else {
            return Vec::new();
        };

        // Start with parent bindings (if any)
        let mut all_bindings = if let Some(ref parent_name) = keymap.inherits {
            self.resolve_keymap_recursive(parent_name, visited)
        } else {
            Vec::new()
        };

        // Add this keymap's bindings (they override parent bindings)
        all_bindings.extend(keymap.bindings);

        all_bindings
    }
    /// Create default language configurations
    fn default_languages() -> HashMap<String, LanguageConfig> {
        let mut languages = HashMap::new();

        languages.insert(
            "rust".to_string(),
            LanguageConfig {
                extensions: vec!["rs".to_string()],
                filenames: vec![],
                grammar: "rust".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "rustfmt".to_string(),
                    args: vec!["--edition".to_string(), "2021".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "javascript".to_string(),
            LanguageConfig {
                extensions: vec!["js".to_string(), "jsx".to_string(), "mjs".to_string()],
                filenames: vec![],
                grammar: "javascript".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "prettier".to_string(),
                    args: vec!["--stdin-filepath".to_string(), "$FILE".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "typescript".to_string(),
            LanguageConfig {
                extensions: vec!["ts".to_string(), "tsx".to_string(), "mts".to_string()],
                filenames: vec![],
                grammar: "typescript".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "prettier".to_string(),
                    args: vec!["--stdin-filepath".to_string(), "$FILE".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "python".to_string(),
            LanguageConfig {
                extensions: vec!["py".to_string(), "pyi".to_string()],
                filenames: vec![],
                grammar: "python".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "ruff".to_string(),
                    args: vec![
                        "format".to_string(),
                        "--stdin-filename".to_string(),
                        "$FILE".to_string(),
                    ],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "c".to_string(),
            LanguageConfig {
                extensions: vec!["c".to_string(), "h".to_string()],
                filenames: vec![],
                grammar: "c".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "clang-format".to_string(),
                    args: vec![],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "cpp".to_string(),
            LanguageConfig {
                extensions: vec![
                    "cpp".to_string(),
                    "cc".to_string(),
                    "cxx".to_string(),
                    "hpp".to_string(),
                    "hh".to_string(),
                    "hxx".to_string(),
                ],
                filenames: vec![],
                grammar: "cpp".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "clang-format".to_string(),
                    args: vec![],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "csharp".to_string(),
            LanguageConfig {
                extensions: vec!["cs".to_string()],
                filenames: vec![],
                grammar: "C#".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "bash".to_string(),
            LanguageConfig {
                extensions: vec!["sh".to_string(), "bash".to_string()],
                filenames: vec![
                    ".bash_aliases".to_string(),
                    ".bash_logout".to_string(),
                    ".bash_profile".to_string(),
                    ".bashrc".to_string(),
                    ".env".to_string(),
                    ".profile".to_string(),
                    ".zlogin".to_string(),
                    ".zlogout".to_string(),
                    ".zprofile".to_string(),
                    ".zshenv".to_string(),
                    ".zshrc".to_string(),
                    // Common shell script files without extensions
                    "PKGBUILD".to_string(),
                    "APKBUILD".to_string(),
                ],
                grammar: "bash".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "makefile".to_string(),
            LanguageConfig {
                extensions: vec!["mk".to_string()],
                filenames: vec![
                    "Makefile".to_string(),
                    "makefile".to_string(),
                    "GNUmakefile".to_string(),
                ],
                grammar: "Makefile".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: false,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: Some(true), // Makefiles require tabs for recipes
                tab_size: Some(8),    // Makefiles traditionally use 8-space tabs
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "dockerfile".to_string(),
            LanguageConfig {
                extensions: vec!["dockerfile".to_string()],
                filenames: vec!["Dockerfile".to_string(), "Containerfile".to_string()],
                grammar: "dockerfile".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "json".to_string(),
            LanguageConfig {
                extensions: vec!["json".to_string(), "jsonc".to_string()],
                filenames: vec![],
                grammar: "json".to_string(),
                comment_prefix: None,
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "prettier".to_string(),
                    args: vec!["--stdin-filepath".to_string(), "$FILE".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "toml".to_string(),
            LanguageConfig {
                extensions: vec!["toml".to_string()],
                filenames: vec!["Cargo.lock".to_string()],
                grammar: "toml".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "yaml".to_string(),
            LanguageConfig {
                extensions: vec!["yml".to_string(), "yaml".to_string()],
                filenames: vec![],
                grammar: "yaml".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "prettier".to_string(),
                    args: vec!["--stdin-filepath".to_string(), "$FILE".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "markdown".to_string(),
            LanguageConfig {
                extensions: vec!["md".to_string(), "markdown".to_string()],
                filenames: vec!["README".to_string()],
                grammar: "markdown".to_string(),
                comment_prefix: None,
                auto_indent: false,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        // Go uses tabs for indentation by convention, so hide tab indicators and use tabs
        languages.insert(
            "go".to_string(),
            LanguageConfig {
                extensions: vec!["go".to_string()],
                filenames: vec![],
                grammar: "go".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: false,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: Some(true), // Go convention is to use tabs
                tab_size: Some(8),    // Go convention is 8-space tab width
                formatter: Some(FormatterConfig {
                    command: "gofmt".to_string(),
                    args: vec![],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "odin".to_string(),
            LanguageConfig {
                extensions: vec!["odin".to_string()],
                filenames: vec![],
                grammar: "odin".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: false,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: Some(true),
                tab_size: Some(8),
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "zig".to_string(),
            LanguageConfig {
                extensions: vec!["zig".to_string(), "zon".to_string()],
                filenames: vec![],
                grammar: "zig".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "java".to_string(),
            LanguageConfig {
                extensions: vec!["java".to_string()],
                filenames: vec![],
                grammar: "java".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "latex".to_string(),
            LanguageConfig {
                extensions: vec![
                    "tex".to_string(),
                    "latex".to_string(),
                    "ltx".to_string(),
                    "sty".to_string(),
                    "cls".to_string(),
                    "bib".to_string(),
                ],
                filenames: vec![],
                grammar: "latex".to_string(),
                comment_prefix: Some("%".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "templ".to_string(),
            LanguageConfig {
                extensions: vec!["templ".to_string()],
                filenames: vec![],
                grammar: "go".to_string(), // Templ uses Go-like syntax
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        // Git-related file types
        languages.insert(
            "git-rebase".to_string(),
            LanguageConfig {
                extensions: vec![],
                filenames: vec!["git-rebase-todo".to_string()],
                grammar: "Git Rebase Todo".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: false,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "git-commit".to_string(),
            LanguageConfig {
                extensions: vec![],
                filenames: vec![
                    "COMMIT_EDITMSG".to_string(),
                    "MERGE_MSG".to_string(),
                    "SQUASH_MSG".to_string(),
                    "TAG_EDITMSG".to_string(),
                ],
                grammar: "Git Commit Message".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: false,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "gitignore".to_string(),
            LanguageConfig {
                extensions: vec!["gitignore".to_string()],
                filenames: vec![
                    ".gitignore".to_string(),
                    ".dockerignore".to_string(),
                    ".npmignore".to_string(),
                    ".hgignore".to_string(),
                ],
                grammar: "Gitignore".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: false,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "gitconfig".to_string(),
            LanguageConfig {
                extensions: vec!["gitconfig".to_string()],
                filenames: vec![".gitconfig".to_string(), ".gitmodules".to_string()],
                grammar: "Git Config".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "gitattributes".to_string(),
            LanguageConfig {
                extensions: vec!["gitattributes".to_string()],
                filenames: vec![".gitattributes".to_string()],
                grammar: "Git Attributes".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: false,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "typst".to_string(),
            LanguageConfig {
                extensions: vec!["typ".to_string()],
                filenames: vec![],
                grammar: "Typst".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        // --- Languages added for LSP support ---
        // These entries ensure detect_language() maps file extensions to language
        // names that match the LSP config keys in default_lsp_config().

        languages.insert(
            "kotlin".to_string(),
            LanguageConfig {
                extensions: vec!["kt".to_string(), "kts".to_string()],
                filenames: vec![],
                grammar: "Kotlin".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "swift".to_string(),
            LanguageConfig {
                extensions: vec!["swift".to_string()],
                filenames: vec![],
                grammar: "Swift".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "scala".to_string(),
            LanguageConfig {
                extensions: vec!["scala".to_string(), "sc".to_string()],
                filenames: vec![],
                grammar: "Scala".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "dart".to_string(),
            LanguageConfig {
                extensions: vec!["dart".to_string()],
                filenames: vec![],
                grammar: "Dart".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "elixir".to_string(),
            LanguageConfig {
                extensions: vec!["ex".to_string(), "exs".to_string()],
                filenames: vec![],
                grammar: "Elixir".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "erlang".to_string(),
            LanguageConfig {
                extensions: vec!["erl".to_string(), "hrl".to_string()],
                filenames: vec![],
                grammar: "Erlang".to_string(),
                comment_prefix: Some("%".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "haskell".to_string(),
            LanguageConfig {
                extensions: vec!["hs".to_string(), "lhs".to_string()],
                filenames: vec![],
                grammar: "Haskell".to_string(),
                comment_prefix: Some("--".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "ocaml".to_string(),
            LanguageConfig {
                extensions: vec!["ml".to_string(), "mli".to_string()],
                filenames: vec![],
                grammar: "OCaml".to_string(),
                comment_prefix: None,
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "clojure".to_string(),
            LanguageConfig {
                extensions: vec![
                    "clj".to_string(),
                    "cljs".to_string(),
                    "cljc".to_string(),
                    "edn".to_string(),
                ],
                filenames: vec![],
                grammar: "Clojure".to_string(),
                comment_prefix: Some(";".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "r".to_string(),
            LanguageConfig {
                extensions: vec!["r".to_string(), "R".to_string(), "rmd".to_string()],
                filenames: vec![],
                grammar: "R".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "julia".to_string(),
            LanguageConfig {
                extensions: vec!["jl".to_string()],
                filenames: vec![],
                grammar: "Julia".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "perl".to_string(),
            LanguageConfig {
                extensions: vec!["pl".to_string(), "pm".to_string(), "t".to_string()],
                filenames: vec![],
                grammar: "Perl".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "nim".to_string(),
            LanguageConfig {
                extensions: vec!["nim".to_string(), "nims".to_string(), "nimble".to_string()],
                filenames: vec![],
                grammar: "Nim".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "gleam".to_string(),
            LanguageConfig {
                extensions: vec!["gleam".to_string()],
                filenames: vec![],
                grammar: "Gleam".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "fsharp".to_string(),
            LanguageConfig {
                extensions: vec!["fs".to_string(), "fsi".to_string(), "fsx".to_string()],
                filenames: vec![],
                grammar: "FSharp".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "nix".to_string(),
            LanguageConfig {
                extensions: vec!["nix".to_string()],
                filenames: vec![],
                grammar: "Nix".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "nushell".to_string(),
            LanguageConfig {
                extensions: vec!["nu".to_string()],
                filenames: vec![],
                grammar: "Nushell".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "solidity".to_string(),
            LanguageConfig {
                extensions: vec!["sol".to_string()],
                filenames: vec![],
                grammar: "Solidity".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "ruby".to_string(),
            LanguageConfig {
                extensions: vec!["rb".to_string(), "rake".to_string(), "gemspec".to_string()],
                filenames: vec![
                    "Gemfile".to_string(),
                    "Rakefile".to_string(),
                    "Guardfile".to_string(),
                ],
                grammar: "Ruby".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "php".to_string(),
            LanguageConfig {
                extensions: vec!["php".to_string(), "phtml".to_string()],
                filenames: vec![],
                grammar: "PHP".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "lua".to_string(),
            LanguageConfig {
                extensions: vec!["lua".to_string()],
                filenames: vec![],
                grammar: "Lua".to_string(),
                comment_prefix: Some("--".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "html".to_string(),
            LanguageConfig {
                extensions: vec!["html".to_string(), "htm".to_string()],
                filenames: vec![],
                grammar: "HTML".to_string(),
                comment_prefix: None,
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "css".to_string(),
            LanguageConfig {
                extensions: vec!["css".to_string()],
                filenames: vec![],
                grammar: "CSS".to_string(),
                comment_prefix: None,
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "sql".to_string(),
            LanguageConfig {
                extensions: vec!["sql".to_string()],
                filenames: vec![],
                grammar: "SQL".to_string(),
                comment_prefix: Some("--".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "graphql".to_string(),
            LanguageConfig {
                extensions: vec!["graphql".to_string(), "gql".to_string()],
                filenames: vec![],
                grammar: "GraphQL".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "protobuf".to_string(),
            LanguageConfig {
                extensions: vec!["proto".to_string()],
                filenames: vec![],
                grammar: "Protocol Buffers".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "cmake".to_string(),
            LanguageConfig {
                extensions: vec!["cmake".to_string()],
                filenames: vec!["CMakeLists.txt".to_string()],
                grammar: "CMake".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "terraform".to_string(),
            LanguageConfig {
                extensions: vec!["tf".to_string(), "tfvars".to_string(), "hcl".to_string()],
                filenames: vec![],
                grammar: "HCL".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "vue".to_string(),
            LanguageConfig {
                extensions: vec!["vue".to_string()],
                filenames: vec![],
                grammar: "Vue".to_string(),
                comment_prefix: None,
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "svelte".to_string(),
            LanguageConfig {
                extensions: vec!["svelte".to_string()],
                filenames: vec![],
                grammar: "Svelte".to_string(),
                comment_prefix: None,
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "astro".to_string(),
            LanguageConfig {
                extensions: vec!["astro".to_string()],
                filenames: vec![],
                grammar: "Astro".to_string(),
                comment_prefix: None,
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        // --- Languages for embedded grammars (syntax highlighting only) ---

        languages.insert(
            "scss".to_string(),
            LanguageConfig {
                extensions: vec!["scss".to_string()],
                filenames: vec![],
                grammar: "SCSS".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "less".to_string(),
            LanguageConfig {
                extensions: vec!["less".to_string()],
                filenames: vec![],
                grammar: "LESS".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "powershell".to_string(),
            LanguageConfig {
                extensions: vec!["ps1".to_string(), "psm1".to_string(), "psd1".to_string()],
                filenames: vec![],
                grammar: "PowerShell".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "kdl".to_string(),
            LanguageConfig {
                extensions: vec!["kdl".to_string()],
                filenames: vec![],
                grammar: "KDL".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "starlark".to_string(),
            LanguageConfig {
                extensions: vec!["bzl".to_string(), "star".to_string()],
                filenames: vec!["BUILD".to_string(), "WORKSPACE".to_string()],
                grammar: "Starlark".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "justfile".to_string(),
            LanguageConfig {
                extensions: vec![],
                filenames: vec![
                    "justfile".to_string(),
                    "Justfile".to_string(),
                    ".justfile".to_string(),
                ],
                grammar: "Justfile".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: Some(true),
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "earthfile".to_string(),
            LanguageConfig {
                extensions: vec!["earth".to_string()],
                filenames: vec!["Earthfile".to_string()],
                grammar: "Earthfile".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "gomod".to_string(),
            LanguageConfig {
                extensions: vec![],
                filenames: vec!["go.mod".to_string(), "go.sum".to_string()],
                grammar: "Go Module".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: Some(true),
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "vlang".to_string(),
            LanguageConfig {
                extensions: vec!["v".to_string(), "vv".to_string()],
                filenames: vec![],
                grammar: "V".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "ini".to_string(),
            LanguageConfig {
                extensions: vec!["ini".to_string(), "cfg".to_string()],
                filenames: vec![],
                grammar: "INI".to_string(),
                comment_prefix: Some(";".to_string()),
                auto_indent: false,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages.insert(
            "hyprlang".to_string(),
            LanguageConfig {
                extensions: vec!["hl".to_string()],
                filenames: vec!["hyprland.conf".to_string()],
                grammar: "Hyprlang".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        languages
    }

    /// Create default LSP configurations
    #[cfg(feature = "runtime")]
    fn default_lsp_config() -> HashMap<String, LspLanguageConfig> {
        let mut lsp = HashMap::new();

        // rust-analyzer (installed via rustup or package manager)
        // Enable logging to help debug LSP issues (stored in XDG state directory)
        let ra_log_path = crate::services::log_dirs::lsp_log_path("rust-analyzer")
            .to_string_lossy()
            .to_string();

        Self::populate_lsp_config(&mut lsp, ra_log_path);
        lsp
    }

    /// Create empty LSP configurations for WASM builds
    #[cfg(not(feature = "runtime"))]
    fn default_lsp_config() -> HashMap<String, LspLanguageConfig> {
        // LSP is not available in WASM builds
        HashMap::new()
    }

    #[cfg(feature = "runtime")]
    fn populate_lsp_config(lsp: &mut HashMap<String, LspLanguageConfig>, ra_log_path: String) {
        // rust-analyzer: full mode by default (no init param restrictions, no process limits).
        // Users can switch to reduced-memory mode via the "Rust LSP: Reduced Memory Mode"
        // command palette command (provided by the rust-lsp plugin).
        lsp.insert(
            "rust".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "rust-analyzer".to_string(),
                args: vec!["--log-file".to_string(), ra_log_path],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::unlimited(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "Cargo.toml".to_string(),
                    "rust-project.json".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // pylsp (installed via pip)
        lsp.insert(
            "python".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "pylsp".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "pyproject.toml".to_string(),
                    "setup.py".to_string(),
                    "setup.cfg".to_string(),
                    "pyrightconfig.json".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // typescript-language-server (installed via npm)
        // Alternative: use "deno lsp" with initialization_options: {"enable": true}
        lsp.insert(
            "javascript".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "typescript-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: HashMap::from([(
                    "jsx".to_string(),
                    "javascriptreact".to_string(),
                )]),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "tsconfig.json".to_string(),
                    "jsconfig.json".to_string(),
                    "package.json".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );
        lsp.insert(
            "typescript".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "typescript-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: HashMap::from([(
                    "tsx".to_string(),
                    "typescriptreact".to_string(),
                )]),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "tsconfig.json".to_string(),
                    "jsconfig.json".to_string(),
                    "package.json".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // vscode-html-language-server (installed via npm install -g vscode-langservers-extracted)
        lsp.insert(
            "html".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "vscode-html-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // vscode-css-language-server (installed via npm install -g vscode-langservers-extracted)
        lsp.insert(
            "css".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "vscode-css-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // clangd (installed via package manager)
        lsp.insert(
            "c".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "clangd".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "compile_commands.json".to_string(),
                    "CMakeLists.txt".to_string(),
                    "Makefile".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );
        lsp.insert(
            "cpp".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "clangd".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "compile_commands.json".to_string(),
                    "CMakeLists.txt".to_string(),
                    "Makefile".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // gopls (installed via go install)
        lsp.insert(
            "go".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "gopls".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "go.mod".to_string(),
                    "go.work".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // vscode-json-language-server (installed via npm install -g vscode-langservers-extracted)
        lsp.insert(
            "json".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "vscode-json-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // csharp-language-server (installed via dotnet tool install -g csharp-ls)
        lsp.insert(
            "csharp".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "csharp-ls".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "*.csproj".to_string(),
                    "*.sln".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // ols - Odin Language Server (https://github.com/DanielGavin/ols)
        // Build from source: cd ols && ./build.sh (Linux/macOS) or ./build.bat (Windows)
        lsp.insert(
            "odin".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "ols".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // zls - Zig Language Server (https://github.com/zigtools/zls)
        // Install via package manager or download from releases
        lsp.insert(
            "zig".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "zls".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // jdtls - Eclipse JDT Language Server for Java
        // Install via package manager or download from Eclipse
        lsp.insert(
            "java".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "jdtls".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "pom.xml".to_string(),
                    "build.gradle".to_string(),
                    "build.gradle.kts".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // texlab - LaTeX Language Server (https://github.com/latex-lsp/texlab)
        // Install via cargo install texlab or package manager
        lsp.insert(
            "latex".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "texlab".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // marksman - Markdown Language Server (https://github.com/artempyanykh/marksman)
        // Install via package manager or download from releases
        lsp.insert(
            "markdown".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "marksman".to_string(),
                args: vec!["server".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // templ - Templ Language Server (https://templ.guide)
        // Install via go install github.com/a-h/templ/cmd/templ@latest
        lsp.insert(
            "templ".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "templ".to_string(),
                args: vec!["lsp".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // tinymist - Typst Language Server (https://github.com/Myriad-Dreamin/tinymist)
        // Install via cargo install tinymist or download from releases
        lsp.insert(
            "typst".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "tinymist".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // bash-language-server (installed via npm install -g bash-language-server)
        lsp.insert(
            "bash".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "bash-language-server".to_string(),
                args: vec!["start".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // lua-language-server (https://github.com/LuaLS/lua-language-server)
        // Install via package manager or download from releases
        lsp.insert(
            "lua".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "lua-language-server".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    ".luarc.json".to_string(),
                    ".luarc.jsonc".to_string(),
                    ".luacheckrc".to_string(),
                    ".stylua.toml".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // solargraph - Ruby Language Server (installed via gem install solargraph)
        lsp.insert(
            "ruby".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "solargraph".to_string(),
                args: vec!["stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "Gemfile".to_string(),
                    ".ruby-version".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // phpactor - PHP Language Server (https://phpactor.readthedocs.io)
        // Install via composer global require phpactor/phpactor
        lsp.insert(
            "php".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "phpactor".to_string(),
                args: vec!["language-server".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec!["composer.json".to_string(), ".git".to_string()],
            }]),
        );

        // yaml-language-server (installed via npm install -g yaml-language-server)
        lsp.insert(
            "yaml".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "yaml-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // taplo - TOML Language Server (https://taplo.tamasfe.dev)
        // Install via cargo install taplo-cli or npm install -g @taplo/cli
        lsp.insert(
            "toml".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "taplo".to_string(),
                args: vec!["lsp".to_string(), "stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // dart - Dart Language Server (#1252)
        // Included with the Dart SDK
        lsp.insert(
            "dart".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "dart".to_string(),
                args: vec!["language-server".to_string(), "--protocol=lsp".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec!["pubspec.yaml".to_string(), ".git".to_string()],
            }]),
        );

        // nu - Nushell Language Server (#1031)
        // Built into the Nushell binary
        lsp.insert(
            "nushell".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "nu".to_string(),
                args: vec!["--lsp".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // solc - Solidity Language Server (#857)
        // Install via npm install -g @nomicfoundation/solidity-language-server
        lsp.insert(
            "solidity".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "nomicfoundation-solidity-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // --- DevOps / infrastructure LSP servers ---

        // terraform-ls - Terraform Language Server (https://github.com/hashicorp/terraform-ls)
        // Install via package manager or download from releases
        lsp.insert(
            "terraform".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "terraform-ls".to_string(),
                args: vec!["serve".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec![
                    "*.tf".to_string(),
                    ".terraform".to_string(),
                    ".git".to_string(),
                ],
            }]),
        );

        // cmake-language-server (https://github.com/regen100/cmake-language-server)
        // Install via pip: pip install cmake-language-server
        lsp.insert(
            "cmake".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "cmake-language-server".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: vec!["CMakeLists.txt".to_string(), ".git".to_string()],
            }]),
        );

        // buf - Protobuf Language Server (https://buf.build)
        // Install via package manager or curl
        lsp.insert(
            "protobuf".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "buf".to_string(),
                args: vec!["beta".to_string(), "lsp".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // graphql-lsp (https://github.com/graphql/graphiql/tree/main/packages/graphql-language-service-cli)
        // Install via npm: npm install -g graphql-language-service-cli
        lsp.insert(
            "graphql".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "graphql-lsp".to_string(),
                args: vec!["server".to_string(), "-m".to_string(), "stream".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // sqls - SQL Language Server (https://github.com/sqls-server/sqls)
        // Install via go: go install github.com/sqls-server/sqls@latest
        lsp.insert(
            "sql".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "sqls".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // --- Web framework LSP servers ---

        // vue-language-server (installed via npm install -g @vue/language-server)
        lsp.insert(
            "vue".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "vue-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // svelte-language-server (installed via npm install -g svelte-language-server)
        lsp.insert(
            "svelte".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "svelteserver".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // astro-ls - Astro Language Server (installed via npm install -g @astrojs/language-server)
        lsp.insert(
            "astro".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "astro-ls".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // tailwindcss-language-server (installed via npm install -g @tailwindcss/language-server)
        lsp.insert(
            "tailwindcss".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "tailwindcss-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // --- Programming language LSP servers ---

        // nil - Nix Language Server (https://github.com/oxalica/nil)
        // Install via nix profile install github:oxalica/nil
        lsp.insert(
            "nix".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "nil".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // kotlin-language-server (https://github.com/fwcd/kotlin-language-server)
        // Install via package manager or build from source
        lsp.insert(
            "kotlin".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "kotlin-language-server".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // sourcekit-lsp - Swift Language Server (included with Swift toolchain)
        lsp.insert(
            "swift".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "sourcekit-lsp".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // metals - Scala Language Server (https://scalameta.org/metals/)
        // Install via coursier: cs install metals
        lsp.insert(
            "scala".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "metals".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // elixir-ls - Elixir Language Server (https://github.com/elixir-lsp/elixir-ls)
        // Install via mix: mix escript.install hex elixir_ls
        lsp.insert(
            "elixir".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "elixir-ls".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // erlang_ls - Erlang Language Server (https://github.com/erlang-ls/erlang_ls)
        lsp.insert(
            "erlang".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "erlang_ls".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // haskell-language-server (https://github.com/haskell/haskell-language-server)
        // Install via ghcup: ghcup install hls
        lsp.insert(
            "haskell".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "haskell-language-server-wrapper".to_string(),
                args: vec!["--lsp".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // ocamllsp - OCaml Language Server (https://github.com/ocaml/ocaml-lsp)
        // Install via opam: opam install ocaml-lsp-server
        lsp.insert(
            "ocaml".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "ocamllsp".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // clojure-lsp (https://github.com/clojure-lsp/clojure-lsp)
        // Install via package manager or download from releases
        lsp.insert(
            "clojure".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "clojure-lsp".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // r-languageserver (https://github.com/REditorSupport/languageserver)
        // Install via R: install.packages("languageserver")
        lsp.insert(
            "r".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "R".to_string(),
                args: vec![
                    "--vanilla".to_string(),
                    "-e".to_string(),
                    "languageserver::run()".to_string(),
                ],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // julia LanguageServer.jl (https://github.com/julia-vscode/LanguageServer.jl)
        // Install via Julia: using Pkg; Pkg.add("LanguageServer")
        lsp.insert(
            "julia".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "julia".to_string(),
                args: vec![
                    "--startup-file=no".to_string(),
                    "--history-file=no".to_string(),
                    "-e".to_string(),
                    "using LanguageServer; runserver()".to_string(),
                ],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // PerlNavigator (https://github.com/bscan/PerlNavigator)
        // Install via npm: npm install -g perlnavigator-server
        lsp.insert(
            "perl".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "perlnavigator".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // nimlangserver - Nim Language Server (https://github.com/nim-lang/langserver)
        // Install via nimble: nimble install nimlangserver
        lsp.insert(
            "nim".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "nimlangserver".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // gleam lsp - Gleam Language Server (built into the gleam binary)
        lsp.insert(
            "gleam".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "gleam".to_string(),
                args: vec!["lsp".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );

        // fsharp - F# Language Server (https://github.com/fsharp/FsAutoComplete)
        // Install via dotnet: dotnet tool install -g fsautocomplete
        lsp.insert(
            "fsharp".to_string(),
            LspLanguageConfig::Multi(vec![LspServerConfig {
                command: "fsautocomplete".to_string(),
                args: vec!["--adaptive-lsp-server-enabled".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            }]),
        );
    }
    pub fn validate(&self) -> Result<(), ConfigError> {
        // Validate tab size
        if self.editor.tab_size == 0 {
            return Err(ConfigError::ValidationError(
                "tab_size must be greater than 0".to_string(),
            ));
        }

        // Validate scroll offset
        if self.editor.scroll_offset > 100 {
            return Err(ConfigError::ValidationError(
                "scroll_offset must be <= 100".to_string(),
            ));
        }

        // Validate keybindings
        for binding in &self.keybindings {
            if binding.key.is_empty() {
                return Err(ConfigError::ValidationError(
                    "keybinding key cannot be empty".to_string(),
                ));
            }
            if binding.action.is_empty() {
                return Err(ConfigError::ValidationError(
                    "keybinding action cannot be empty".to_string(),
                ));
            }
        }

        Ok(())
    }
}

/// Configuration error types
#[derive(Debug)]
pub enum ConfigError {
    IoError(String),
    ParseError(String),
    SerializeError(String),
    ValidationError(String),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(msg) => write!(f, "IO error: {msg}"),
            Self::ParseError(msg) => write!(f, "Parse error: {msg}"),
            Self::SerializeError(msg) => write!(f, "Serialize error: {msg}"),
            Self::ValidationError(msg) => write!(f, "Validation error: {msg}"),
        }
    }
}

impl std::error::Error for ConfigError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert_eq!(config.editor.tab_size, 4);
        assert!(config.editor.line_numbers);
        assert!(config.editor.syntax_highlighting);
        // keybindings is empty by design - it's for user customizations only
        // The actual keybindings come from resolve_keymap(active_keybinding_map)
        assert!(config.keybindings.is_empty());
        // But the resolved keymap should have bindings
        let resolved = config.resolve_keymap(&config.active_keybinding_map);
        assert!(!resolved.is_empty());
    }

    #[test]
    fn test_all_builtin_keymaps_loadable() {
        for name in KeybindingMapName::BUILTIN_OPTIONS {
            let keymap = Config::load_builtin_keymap(name);
            assert!(keymap.is_some(), "Failed to load builtin keymap '{}'", name);
        }
    }

    #[test]
    fn test_config_validation() {
        let mut config = Config::default();
        assert!(config.validate().is_ok());

        config.editor.tab_size = 0;
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_macos_keymap_inherits_enter_bindings() {
        let config = Config::default();
        let bindings = config.resolve_keymap("macos");

        let enter_bindings: Vec<_> = bindings.iter().filter(|b| b.key == "Enter").collect();
        assert!(
            !enter_bindings.is_empty(),
            "macos keymap should inherit Enter bindings from default, got {} Enter bindings",
            enter_bindings.len()
        );
        // Should have at least insert_newline for normal mode
        let has_insert_newline = enter_bindings.iter().any(|b| b.action == "insert_newline");
        assert!(
            has_insert_newline,
            "macos keymap should have insert_newline action for Enter key"
        );
    }

    #[test]
    fn test_config_serialize_deserialize() {
        // Test that Config can be serialized and deserialized correctly
        let config = Config::default();

        // Serialize to JSON
        let json = serde_json::to_string_pretty(&config).unwrap();

        // Deserialize back
        let loaded: Config = serde_json::from_str(&json).unwrap();

        assert_eq!(config.editor.tab_size, loaded.editor.tab_size);
        assert_eq!(config.theme, loaded.theme);
    }

    #[test]
    fn test_config_with_custom_keybinding() {
        let json = r#"{
            "editor": {
                "tab_size": 2
            },
            "keybindings": [
                {
                    "key": "x",
                    "modifiers": ["ctrl", "shift"],
                    "action": "custom_action",
                    "args": {},
                    "when": null
                }
            ]
        }"#;

        let config: Config = serde_json::from_str(json).unwrap();
        assert_eq!(config.editor.tab_size, 2);
        assert_eq!(config.keybindings.len(), 1);
        assert_eq!(config.keybindings[0].key, "x");
        assert_eq!(config.keybindings[0].modifiers.len(), 2);
    }

    #[test]
    fn test_sparse_config_merges_with_defaults() {
        // User config that only specifies one LSP server
        let temp_dir = tempfile::tempdir().unwrap();
        let config_path = temp_dir.path().join("config.json");

        // Write a sparse config - only overriding rust LSP
        let sparse_config = r#"{
            "lsp": {
                "rust": {
                    "command": "custom-rust-analyzer",
                    "args": ["--custom-arg"]
                }
            }
        }"#;
        std::fs::write(&config_path, sparse_config).unwrap();

        // Load the config - should merge with defaults
        let loaded = Config::load_from_file(&config_path).unwrap();

        // User's rust override should be present
        assert!(loaded.lsp.contains_key("rust"));
        assert_eq!(
            loaded.lsp["rust"].as_slice()[0].command,
            "custom-rust-analyzer".to_string()
        );

        // Default LSP servers should also be present (merged from defaults)
        assert!(
            loaded.lsp.contains_key("python"),
            "python LSP should be merged from defaults"
        );
        assert!(
            loaded.lsp.contains_key("typescript"),
            "typescript LSP should be merged from defaults"
        );
        assert!(
            loaded.lsp.contains_key("javascript"),
            "javascript LSP should be merged from defaults"
        );

        // Default language configs should also be present
        assert!(loaded.languages.contains_key("rust"));
        assert!(loaded.languages.contains_key("python"));
        assert!(loaded.languages.contains_key("typescript"));
    }

    #[test]
    fn test_empty_config_gets_all_defaults() {
        let temp_dir = tempfile::tempdir().unwrap();
        let config_path = temp_dir.path().join("config.json");

        // Write an empty config
        std::fs::write(&config_path, "{}").unwrap();

        let loaded = Config::load_from_file(&config_path).unwrap();
        let defaults = Config::default();

        // Should have all default LSP servers
        assert_eq!(loaded.lsp.len(), defaults.lsp.len());

        // Should have all default languages
        assert_eq!(loaded.languages.len(), defaults.languages.len());
    }

    #[test]
    fn test_dynamic_submenu_expansion() {
        // Test that DynamicSubmenu expands to Submenu with generated items
        let temp_dir = tempfile::tempdir().unwrap();
        let themes_dir = temp_dir.path().to_path_buf();

        let dynamic = MenuItem::DynamicSubmenu {
            label: "Test".to_string(),
            source: "copy_with_theme".to_string(),
        };

        let expanded = dynamic.expand_dynamic(&themes_dir);

        // Should expand to a Submenu
        match expanded {
            MenuItem::Submenu { label, items } => {
                assert_eq!(label, "Test");
                // Should have items for each available theme (embedded themes only, no user themes in temp dir)
                let loader = crate::view::theme::ThemeLoader::embedded_only();
                let registry = loader.load_all(&[]);
                assert_eq!(items.len(), registry.len());

                // Each item should be an Action with copy_with_theme
                for (item, theme_info) in items.iter().zip(registry.list().iter()) {
                    match item {
                        MenuItem::Action {
                            label,
                            action,
                            args,
                            ..
                        } => {
                            assert_eq!(label, &theme_info.name);
                            assert_eq!(action, "copy_with_theme");
                            assert_eq!(
                                args.get("theme").and_then(|v| v.as_str()),
                                Some(theme_info.name.as_str())
                            );
                        }
                        _ => panic!("Expected Action item"),
                    }
                }
            }
            _ => panic!("Expected Submenu after expansion"),
        }
    }

    #[test]
    fn test_non_dynamic_item_unchanged() {
        // Non-DynamicSubmenu items should be unchanged by expand_dynamic
        let temp_dir = tempfile::tempdir().unwrap();
        let themes_dir = temp_dir.path();

        let action = MenuItem::Action {
            label: "Test".to_string(),
            action: "test".to_string(),
            args: HashMap::new(),
            when: None,
            checkbox: None,
        };

        let expanded = action.expand_dynamic(themes_dir);
        match expanded {
            MenuItem::Action { label, action, .. } => {
                assert_eq!(label, "Test");
                assert_eq!(action, "test");
            }
            _ => panic!("Action should remain Action after expand_dynamic"),
        }
    }

    #[test]
    fn test_buffer_config_uses_global_defaults() {
        let config = Config::default();
        let buffer_config = BufferConfig::resolve(&config, None);

        assert_eq!(buffer_config.tab_size, config.editor.tab_size);
        assert_eq!(buffer_config.auto_indent, config.editor.auto_indent);
        assert!(!buffer_config.use_tabs); // Default is spaces
        assert!(buffer_config.whitespace.any_tabs()); // Tabs visible by default
        assert!(buffer_config.formatter.is_none());
        assert!(!buffer_config.format_on_save);
    }

    #[test]
    fn test_buffer_config_applies_language_overrides() {
        let mut config = Config::default();

        // Add a language config with custom settings
        config.languages.insert(
            "go".to_string(),
            LanguageConfig {
                extensions: vec!["go".to_string()],
                filenames: vec![],
                grammar: "go".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: false, // Go hides tab indicators
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: Some(true), // Go uses tabs
                tab_size: Some(8),    // Go uses 8-space tabs
                formatter: Some(FormatterConfig {
                    command: "gofmt".to_string(),
                    args: vec![],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: true,
                on_save: vec![],
                word_characters: None,
            },
        );

        let buffer_config = BufferConfig::resolve(&config, Some("go"));

        assert_eq!(buffer_config.tab_size, 8);
        assert!(buffer_config.use_tabs);
        assert!(!buffer_config.whitespace.any_tabs()); // Go disables tab indicators
        assert!(buffer_config.format_on_save);
        assert!(buffer_config.formatter.is_some());
        assert_eq!(buffer_config.formatter.as_ref().unwrap().command, "gofmt");
    }

    #[test]
    fn test_buffer_config_unknown_language_uses_global() {
        let config = Config::default();
        let buffer_config = BufferConfig::resolve(&config, Some("unknown_lang"));

        // Should fall back to global settings
        assert_eq!(buffer_config.tab_size, config.editor.tab_size);
        assert!(!buffer_config.use_tabs);
    }

    #[test]
    fn test_buffer_config_per_language_line_wrap() {
        let mut config = Config::default();
        config.editor.line_wrap = false;

        // Add markdown with line_wrap override
        config.languages.insert(
            "markdown".to_string(),
            LanguageConfig {
                extensions: vec!["md".to_string()],
                line_wrap: Some(true),
                ..Default::default()
            },
        );

        // Markdown should override global line_wrap=false
        let md_config = BufferConfig::resolve(&config, Some("markdown"));
        assert!(md_config.line_wrap, "Markdown should have line_wrap=true");

        // Other languages should use global default (false)
        let other_config = BufferConfig::resolve(&config, Some("rust"));
        assert!(
            !other_config.line_wrap,
            "Non-configured languages should use global line_wrap=false"
        );

        // No language should use global default
        let no_lang_config = BufferConfig::resolve(&config, None);
        assert!(
            !no_lang_config.line_wrap,
            "No language should use global line_wrap=false"
        );
    }

    #[test]
    fn test_buffer_config_per_language_wrap_column() {
        let mut config = Config::default();
        config.editor.wrap_column = Some(120);

        // Add markdown with wrap_column override
        config.languages.insert(
            "markdown".to_string(),
            LanguageConfig {
                extensions: vec!["md".to_string()],
                wrap_column: Some(80),
                ..Default::default()
            },
        );

        // Markdown should use its own wrap_column
        let md_config = BufferConfig::resolve(&config, Some("markdown"));
        assert_eq!(md_config.wrap_column, Some(80));

        // Other languages should use global wrap_column
        let other_config = BufferConfig::resolve(&config, Some("rust"));
        assert_eq!(other_config.wrap_column, Some(120));

        // No language should use global wrap_column
        let no_lang_config = BufferConfig::resolve(&config, None);
        assert_eq!(no_lang_config.wrap_column, Some(120));
    }

    #[test]
    fn test_buffer_config_indent_string() {
        let config = Config::default();

        // Spaces indent
        let spaces_config = BufferConfig::resolve(&config, None);
        assert_eq!(spaces_config.indent_string(), "    "); // 4 spaces

        // Tabs indent - create a language that uses tabs
        let mut config_with_tabs = Config::default();
        config_with_tabs.languages.insert(
            "makefile".to_string(),
            LanguageConfig {
                use_tabs: Some(true),
                tab_size: Some(8),
                ..Default::default()
            },
        );
        let tabs_config = BufferConfig::resolve(&config_with_tabs, Some("makefile"));
        assert_eq!(tabs_config.indent_string(), "\t");
    }

    #[test]
    fn test_buffer_config_global_use_tabs_inherited() {
        // When editor.use_tabs is true, buffers without a language-specific
        // override should inherit the global setting.
        let mut config = Config::default();
        config.editor.use_tabs = true;

        // Unknown language inherits global
        let buffer_config = BufferConfig::resolve(&config, Some("unknown_lang"));
        assert!(buffer_config.use_tabs);

        // No language inherits global
        let buffer_config = BufferConfig::resolve(&config, None);
        assert!(buffer_config.use_tabs);

        // Language with explicit use_tabs: Some(false) overrides global
        config.languages.insert(
            "python".to_string(),
            LanguageConfig {
                use_tabs: Some(false),
                ..Default::default()
            },
        );
        let buffer_config = BufferConfig::resolve(&config, Some("python"));
        assert!(!buffer_config.use_tabs);

        // Language with use_tabs: None inherits global true
        config.languages.insert(
            "rust".to_string(),
            LanguageConfig {
                use_tabs: None,
                ..Default::default()
            },
        );
        let buffer_config = BufferConfig::resolve(&config, Some("rust"));
        assert!(buffer_config.use_tabs);
    }

    /// Verify that every LSP config key has a matching entry in default_languages().
    /// Without this, detect_language() won't map file extensions to the language name,
    /// causing "No LSP server configured for this file type" even though the LSP config
    /// exists. The only exception is "tailwindcss" which attaches to CSS/HTML/JS files
    /// rather than having its own file type.
    #[test]
    #[cfg(feature = "runtime")]
    fn test_lsp_languages_have_language_config() {
        let config = Config::default();
        let exceptions = ["tailwindcss"];
        for lsp_key in config.lsp.keys() {
            if exceptions.contains(&lsp_key.as_str()) {
                continue;
            }
            assert!(
                config.languages.contains_key(lsp_key),
                "LSP config key '{}' has no matching entry in default_languages(). \
                 Add a LanguageConfig with the correct file extensions so detect_language() \
                 can map files to this language.",
                lsp_key
            );
        }
    }
}
