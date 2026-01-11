use crate::types::{context_keys, LspServerConfig, ProcessLimits};

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

    /// Convert to crossterm cursor style
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

    /// Parse from string (for command palette)
    pub fn from_str(s: &str) -> Option<Self> {
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
    pub const BUILTIN_OPTIONS: &'static [&'static str] = &["default", "emacs", "vscode", "macos"];
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LineEndingOption {
    /// Unix/Linux/macOS format (LF)
    Lf,
    /// Windows format (CRLF)
    Crlf,
    /// Classic Mac format (CR) - rare
    Cr,
}

impl Default for LineEndingOption {
    fn default() -> Self {
        Self::Lf
    }
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

    /// Check for new versions on quit (default: true)
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

    /// LSP server configurations by language
    #[serde(default)]
    pub lsp: HashMap<String, LspServerConfig>,

    /// Warning notification settings
    #[serde(default)]
    pub warnings: WarningsConfig,

    /// Plugin configurations by plugin name
    /// Plugins are auto-discovered from the plugins directory.
    /// Use this to enable/disable specific plugins.
    #[serde(default)]
    #[schemars(extend("x-standalone-category" = true, "x-no-add" = true))]
    pub plugins: HashMap<String, PluginConfig>,
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

/// Editor behavior configuration
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct EditorConfig {
    /// Number of spaces per tab character
    #[serde(default = "default_tab_size")]
    pub tab_size: usize,

    /// Automatically indent new lines based on the previous line
    #[serde(default = "default_true")]
    pub auto_indent: bool,

    /// Show line numbers in the gutter (default for new buffers)
    #[serde(default = "default_true")]
    pub line_numbers: bool,

    /// Show line numbers relative to cursor position
    #[serde(default = "default_false")]
    pub relative_line_numbers: bool,

    /// Minimum lines to keep visible above/below cursor when scrolling
    #[serde(default = "default_scroll_offset")]
    pub scroll_offset: usize,

    /// Enable syntax highlighting for code files
    #[serde(default = "default_true")]
    pub syntax_highlighting: bool,

    /// Wrap long lines to fit the window width (default for new views)
    #[serde(default = "default_true")]
    pub line_wrap: bool,

    /// Maximum time in milliseconds for syntax highlighting per frame
    #[serde(default = "default_highlight_timeout")]
    pub highlight_timeout_ms: u64,

    /// Undo history snapshot interval (number of edits between snapshots)
    #[serde(default = "default_snapshot_interval")]
    pub snapshot_interval: usize,

    /// File size threshold in bytes for "large file" behavior
    /// Files larger than this will:
    /// - Skip LSP features
    /// - Use constant-size scrollbar thumb (1 char)
    /// Files smaller will count actual lines for accurate scrollbar rendering
    #[serde(default = "default_large_file_threshold")]
    pub large_file_threshold_bytes: u64,

    /// Estimated average line length in bytes (used for large file line estimation)
    /// This is used by LineIterator to estimate line positions in large files
    /// without line metadata. Typical values: 80-120 bytes.
    #[serde(default = "default_estimated_line_length")]
    pub estimated_line_length: usize,

    /// Whether to enable LSP inlay hints (type hints, parameter hints, etc.)
    #[serde(default = "default_true")]
    pub enable_inlay_hints: bool,

    /// Whether to enable file recovery (Emacs-style auto-save)
    /// When enabled, buffers are periodically saved to recovery files
    /// so they can be recovered if the editor crashes.
    #[serde(default = "default_true")]
    pub recovery_enabled: bool,

    /// Auto-save interval in seconds for file recovery
    /// Modified buffers are saved to recovery files at this interval.
    /// Default: 2 seconds for fast recovery with minimal data loss.
    /// Set to 0 to disable periodic auto-save (manual recovery only).
    #[serde(default = "default_auto_save_interval")]
    pub auto_save_interval_secs: u32,

    /// Number of bytes to look back/forward from the viewport for syntax highlighting context.
    /// Larger values improve accuracy for multi-line constructs (strings, comments, nested blocks)
    /// but may slow down highlighting for very large files.
    /// Default: 10KB (10000 bytes)
    #[serde(default = "default_highlight_context_bytes")]
    pub highlight_context_bytes: usize,

    /// Whether mouse hover triggers LSP hover requests.
    /// When enabled, hovering over code with the mouse will show documentation.
    /// Default: true
    #[serde(default = "default_true")]
    pub mouse_hover_enabled: bool,

    /// Delay in milliseconds before a mouse hover triggers an LSP hover request.
    /// Lower values show hover info faster but may cause more LSP server load.
    /// Default: 500ms
    #[serde(default = "default_mouse_hover_delay")]
    pub mouse_hover_delay_ms: u64,

    /// Time window in milliseconds for detecting double-clicks.
    /// Two clicks within this time are treated as a double-click (word selection).
    /// Default: 500ms
    #[serde(default = "default_double_click_time")]
    pub double_click_time_ms: u64,

    /// Poll interval in milliseconds for auto-reverting open buffers.
    /// When auto-revert is enabled, file modification times are checked at this interval.
    /// Lower values detect external changes faster but use more CPU.
    /// Default: 2000ms (2 seconds)
    #[serde(default = "default_auto_revert_poll_interval")]
    pub auto_revert_poll_interval_ms: u64,

    /// Poll interval in milliseconds for refreshing expanded directories in the file explorer.
    /// Directory modification times are checked at this interval to detect new/deleted files.
    /// Lower values detect changes faster but use more CPU.
    /// Default: 3000ms (3 seconds)
    #[serde(default = "default_file_tree_poll_interval")]
    pub file_tree_poll_interval_ms: u64,

    /// Default line ending format for new files.
    /// Files loaded from disk will use their detected line ending format.
    /// Options: "lf" (Unix/Linux/macOS), "crlf" (Windows), "cr" (Classic Mac)
    /// Default: "lf"
    #[serde(default)]
    pub default_line_ending: LineEndingOption,

    /// Cursor style for the terminal cursor.
    /// Options: blinking_block, steady_block, blinking_bar, steady_bar, blinking_underline, steady_underline
    /// Default: blinking_block
    #[serde(default)]
    pub cursor_style: CursorStyle,

    /// Enable quick suggestions (VS Code-like behavior).
    /// When enabled, completion suggestions appear automatically while typing,
    /// not just on trigger characters (like `.` or `::`).
    /// Default: true
    #[serde(default = "default_true")]
    pub quick_suggestions: bool,

    /// Whether the menu bar is visible by default.
    /// The menu bar provides access to menus (File, Edit, View, etc.) at the top of the screen.
    /// Can be toggled at runtime via command palette or keybinding.
    /// Default: true
    #[serde(default = "default_true")]
    pub show_menu_bar: bool,

    /// Whether the tab bar is visible by default.
    /// The tab bar shows open files in each split pane.
    /// Can be toggled at runtime via command palette or keybinding.
    /// Default: true
    #[serde(default = "default_true")]
    pub show_tab_bar: bool,
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

fn default_true() -> bool {
    true
}

fn default_false() -> bool {
    false
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
    2 // Auto-save every 2 seconds for fast recovery
}

fn default_highlight_context_bytes() -> usize {
    10_000 // 10KB context for accurate syntax highlighting
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
            tab_size: default_tab_size(),
            auto_indent: true,
            line_numbers: true,
            relative_line_numbers: false,
            scroll_offset: default_scroll_offset(),
            syntax_highlighting: true,
            line_wrap: true,
            highlight_timeout_ms: default_highlight_timeout(),
            snapshot_interval: default_snapshot_interval(),
            large_file_threshold_bytes: default_large_file_threshold(),
            estimated_line_length: default_estimated_line_length(),
            enable_inlay_hints: true,
            recovery_enabled: true,
            auto_save_interval_secs: default_auto_save_interval(),
            highlight_context_bytes: default_highlight_context_bytes(),
            mouse_hover_enabled: true,
            mouse_hover_delay_ms: default_mouse_hover_delay(),
            double_click_time_ms: default_double_click_time(),
            auto_revert_poll_interval_ms: default_auto_revert_poll_interval(),
            file_tree_poll_interval_ms: default_file_tree_poll_interval(),
            default_line_ending: LineEndingOption::default(),
            cursor_style: CursorStyle::default(),
            quick_suggestions: true,
            show_menu_bar: true,
            show_tab_bar: true,
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

/// Configuration for an individual plugin
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/enabled"))]
pub struct PluginConfig {
    /// Whether this plugin is enabled (default: true)
    /// When disabled, the plugin will not be loaded or executed.
    #[serde(default = "default_true")]
    pub enabled: bool,

    /// Path to the plugin file (populated automatically when scanning)
    /// This is filled in by the plugin system and should not be set manually.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[schemars(extend("readOnly" = true))]
    pub path: Option<std::path::PathBuf>,
}

impl Default for PluginConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            path: None,
        }
    }
}

impl PluginConfig {
    /// Create a new plugin config with the given path
    pub fn new_with_path(path: std::path::PathBuf) -> Self {
        Self {
            enabled: true,
            path: Some(path),
        }
    }
}

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
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FileBrowserConfig {
    /// Whether to show hidden files (starting with .) by default in Open File dialog
    #[serde(default = "default_false")]
    pub show_hidden: bool,
}

impl Default for FileBrowserConfig {
    fn default() -> Self {
        Self { show_hidden: false }
    }
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

    /// Whether pressing Tab should insert a tab character instead of spaces.
    /// Defaults to false (insert spaces based on tab_size).
    /// Set to true for languages like Go and Makefile that require tabs.
    #[serde(default = "default_false")]
    pub use_tabs: bool,

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

    /// Whether to show whitespace tab indicators (→)
    pub show_whitespace_tabs: bool,

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
        let mut config = BufferConfig {
            tab_size: editor.tab_size,
            use_tabs: false, // Global default is spaces
            auto_indent: editor.auto_indent,
            show_whitespace_tabs: true, // Global default
            formatter: None,
            format_on_save: false,
            on_save: Vec::new(),
            highlighter: HighlighterPreference::Auto,
            textmate_grammar: None,
        };

        // Apply language-specific overrides if available
        if let Some(lang_id) = language_id {
            if let Some(lang_config) = global_config.languages.get(lang_id) {
                // Tab size: use language setting if specified, else global
                if let Some(ts) = lang_config.tab_size {
                    config.tab_size = ts;
                }

                // Use tabs: language override
                config.use_tabs = lang_config.use_tabs;

                // Auto indent: language override
                config.auto_indent = lang_config.auto_indent;

                // Show whitespace tabs: language override
                config.show_whitespace_tabs = lang_config.show_whitespace_tabs;

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
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MenuConfig {
    /// List of top-level menus in the menu bar
    #[serde(default)]
    pub menus: Vec<Menu>,
}

/// A top-level menu in the menu bar
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Menu {
    /// Internal identifier for the menu (used for keybinding matching).
    /// This should NOT be translated - use English names like "File", "Edit".
    /// If not set, the label is used for matching (for backward compatibility).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    /// Display label for the menu (can be translated)
    pub label: String,
    /// Menu items (actions, separators, or submenus)
    pub items: Vec<MenuItem>,
}

impl Menu {
    /// Get the identifier for matching (id if set, otherwise label).
    /// This is used for keybinding matching and should be stable across translations.
    pub fn match_id(&self) -> &str {
        self.id.as_deref().unwrap_or(&self.label)
    }

    /// Expand all DynamicSubmenu items in this menu to regular Submenu items
    /// This should be called before the menu is used for rendering/navigation
    pub fn expand_dynamic_items(&mut self) {
        self.items = self
            .items
            .iter()
            .map(|item| item.expand_dynamic())
            .collect();
    }
}

/// A menu item (action, separator, or submenu)
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum MenuItem {
    /// A separator line
    Separator { separator: bool },
    /// An action item
    Action {
        label: String,
        action: String,
        #[serde(default)]
        args: HashMap<String, serde_json::Value>,
        #[serde(default)]
        when: Option<String>,
        /// Checkbox state condition (e.g., "line_numbers", "line_wrap")
        #[serde(default)]
        checkbox: Option<String>,
    },
    /// A submenu (for future extensibility)
    Submenu { label: String, items: Vec<Self> },
    /// A dynamic submenu whose items are generated at runtime
    /// The `source` field specifies what to generate (e.g., "themes")
    DynamicSubmenu { label: String, source: String },
    /// A disabled info label (no action)
    Label { info: String },
}

impl MenuItem {
    /// Expand a DynamicSubmenu into a regular Submenu with generated items.
    /// Returns the original item if not a DynamicSubmenu.
    pub fn expand_dynamic(&self) -> Self {
        match self {
            Self::DynamicSubmenu { label, source } => {
                let items = Self::generate_dynamic_items(source);
                Self::Submenu {
                    label: label.clone(),
                    items,
                }
            }
            other => other.clone(),
        }
    }

    /// Generate menu items for a dynamic source
    pub fn generate_dynamic_items(source: &str) -> Vec<Self> {
        match source {
            "copy_with_theme" => {
                // Generate theme options from available themes
                crate::view::theme::Theme::available_themes()
                    .into_iter()
                    .map(|theme_name| {
                        let mut args = HashMap::new();
                        args.insert("theme".to_string(), serde_json::json!(theme_name));
                        Self::Action {
                            label: theme_name.to_string(),
                            action: "copy_with_theme".to_string(),
                            args,
                            when: Some(context_keys::HAS_SELECTION.to_string()),
                            checkbox: None,
                        }
                    })
                    .collect()
            }
            _ => vec![Self::Label {
                info: format!("Unknown source: {}", source),
            }],
        }
    }
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
            terminal: TerminalConfig::default(),
            keybindings: vec![], // User customizations only; defaults come from active_keybinding_map
            keybinding_maps: HashMap::new(), // User-defined maps go here
            active_keybinding_map: default_keybinding_map_name(),
            languages: Self::default_languages(),
            lsp: Self::default_lsp_config(),
            warnings: WarningsConfig::default(),
            plugins: HashMap::new(), // Populated when scanning for plugins
        }
    }
}

impl Default for MenuConfig {
    fn default() -> Self {
        Self { menus: vec![] }
    }
}

impl MenuConfig {
    /// Create a MenuConfig with translated menus using the current locale
    pub fn translated() -> Self {
        Self {
            menus: Self::translated_menus(),
        }
    }

    /// Create default menu bar configuration with translated labels
    fn translated_menus() -> Vec<Menu> {
        vec![
            // File menu
            Menu {
                id: Some("File".to_string()),
                label: t!("menu.file").to_string(),
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
                ],
            },
            // View menu
            Menu {
                id: Some("View".to_string()),
                label: t!("menu.view").to_string(),
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
                        label: t!("menu.view.set_compose_width").to_string(),
                        action: "set_compose_width".to_string(),
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
                                checkbox: None,
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
                                checkbox: None,
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
                                checkbox: None,
                            },
                        ],
                    },
                ],
            },
            // Selection menu
            Menu {
                id: Some("Selection".to_string()),
                label: t!("menu.selection").to_string(),
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
                ],
            },
            // Explorer menu
            Menu {
                id: Some("Explorer".to_string()),
                label: t!("menu.explorer").to_string(),
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "rustfmt".to_string(),
                    args: vec!["--edition".to_string(), "2021".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "prettier".to_string(),
                    args: vec!["--stdin-filepath".to_string(), "$FILE".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "prettier".to_string(),
                    args: vec!["--stdin-filepath".to_string(), "$FILE".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "clang-format".to_string(),
                    args: vec![],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "clang-format".to_string(),
                    args: vec![],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
            },
        );

        languages.insert(
            "csharp".to_string(),
            LanguageConfig {
                extensions: vec!["cs".to_string()],
                filenames: vec![],
                grammar: "c_sharp".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
                grammar: "make".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: false,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: true,    // Makefiles require tabs for recipes
                tab_size: Some(8), // Makefiles traditionally use 8-space tabs
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "prettier".to_string(),
                    args: vec!["--stdin-filepath".to_string(), "$FILE".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: Some(FormatterConfig {
                    command: "prettier".to_string(),
                    args: vec!["--stdin-filepath".to_string(), "$FILE".to_string()],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: false,
                use_tabs: true,    // Go convention is to use tabs
                tab_size: Some(8), // Go convention is 8-space tab width
                formatter: Some(FormatterConfig {
                    command: "gofmt".to_string(),
                    args: vec![],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: false,
                on_save: vec![],
            },
        );

        languages
    }

    /// Create default LSP configurations
    fn default_lsp_config() -> HashMap<String, LspServerConfig> {
        let mut lsp = HashMap::new();

        // rust-analyzer (installed via rustup or package manager)
        // Enable logging to help debug LSP issues (stored in XDG state directory)
        let ra_log_path = crate::services::log_dirs::lsp_log_path("rust-analyzer")
            .to_string_lossy()
            .to_string();

        // Minimal performance config for rust-analyzer:
        // - checkOnSave: false - disables cargo check on every save (the #1 cause of slowdowns)
        // - cachePriming.enable: false - disables background indexing of entire crate graph
        // - procMacro.enable: false - disables proc-macro expansion (saves CPU/RAM)
        // - cargo.buildScripts.enable: false - prevents running build.rs automatically
        // - cargo.autoreload: false - only reload manually
        // - diagnostics.enable: true - keeps basic syntax error reporting
        // - files.watcher: "server" - more efficient than editor-side watchers
        let ra_init_options = serde_json::json!({
            "checkOnSave": false,
            "cachePriming": { "enable": false },
            "procMacro": { "enable": false },
            "cargo": {
                "buildScripts": { "enable": false },
                "autoreload": false
            },
            "diagnostics": { "enable": true },
            "files": { "watcher": "server" }
        });

        lsp.insert(
            "rust".to_string(),
            LspServerConfig {
                command: "rust-analyzer".to_string(),
                args: vec!["--log-file".to_string(), ra_log_path],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: Some(ra_init_options),
            },
        );

        // pylsp (installed via pip)
        lsp.insert(
            "python".to_string(),
            LspServerConfig {
                command: "pylsp".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
            },
        );

        // typescript-language-server (installed via npm)
        // Alternative: use "deno lsp" with initialization_options: {"enable": true}
        let ts_lsp = LspServerConfig {
            command: "typescript-language-server".to_string(),
            args: vec!["--stdio".to_string()],
            enabled: true,
            auto_start: false,
            process_limits: ProcessLimits::default(),
            initialization_options: None,
        };
        lsp.insert("javascript".to_string(), ts_lsp.clone());
        lsp.insert("typescript".to_string(), ts_lsp);

        // vscode-html-language-server (installed via npm install -g vscode-langservers-extracted)
        lsp.insert(
            "html".to_string(),
            LspServerConfig {
                command: "vscode-html-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
            },
        );

        // vscode-css-language-server (installed via npm install -g vscode-langservers-extracted)
        lsp.insert(
            "css".to_string(),
            LspServerConfig {
                command: "vscode-css-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
            },
        );

        // clangd (installed via package manager)
        lsp.insert(
            "c".to_string(),
            LspServerConfig {
                command: "clangd".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
            },
        );
        lsp.insert(
            "cpp".to_string(),
            LspServerConfig {
                command: "clangd".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
            },
        );

        // gopls (installed via go install)
        lsp.insert(
            "go".to_string(),
            LspServerConfig {
                command: "gopls".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
            },
        );

        // vscode-json-language-server (installed via npm install -g vscode-langservers-extracted)
        lsp.insert(
            "json".to_string(),
            LspServerConfig {
                command: "vscode-json-language-server".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
            },
        );

        // csharp-language-server (installed via dotnet tool install -g csharp-ls)
        lsp.insert(
            "csharp".to_string(),
            LspServerConfig {
                command: "csharp-ls".to_string(),
                args: vec![],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
            },
        );

        lsp
    }

    /// Validate the configuration
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
            loaded.lsp["rust"].command,
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
        let dynamic = MenuItem::DynamicSubmenu {
            label: "Test".to_string(),
            source: "copy_with_theme".to_string(),
        };

        let expanded = dynamic.expand_dynamic();

        // Should expand to a Submenu
        match expanded {
            MenuItem::Submenu { label, items } => {
                assert_eq!(label, "Test");
                // Should have items for each available theme
                let themes = crate::view::theme::Theme::available_themes();
                assert_eq!(items.len(), themes.len());

                // Each item should be an Action with copy_with_theme
                for (item, theme_name) in items.iter().zip(themes.iter()) {
                    match item {
                        MenuItem::Action {
                            label,
                            action,
                            args,
                            ..
                        } => {
                            assert_eq!(label, theme_name);
                            assert_eq!(action, "copy_with_theme");
                            assert_eq!(
                                args.get("theme").and_then(|v| v.as_str()),
                                Some(theme_name.as_str())
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
        let action = MenuItem::Action {
            label: "Test".to_string(),
            action: "test".to_string(),
            args: HashMap::new(),
            when: None,
            checkbox: None,
        };

        let expanded = action.expand_dynamic();
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
        assert!(buffer_config.show_whitespace_tabs);
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
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: false, // Go hides tab indicators
                use_tabs: true,              // Go uses tabs
                tab_size: Some(8),           // Go uses 8-space tabs
                formatter: Some(FormatterConfig {
                    command: "gofmt".to_string(),
                    args: vec![],
                    stdin: true,
                    timeout_ms: 10000,
                }),
                format_on_save: true,
                on_save: vec![],
            },
        );

        let buffer_config = BufferConfig::resolve(&config, Some("go"));

        assert_eq!(buffer_config.tab_size, 8);
        assert!(buffer_config.use_tabs);
        assert!(!buffer_config.show_whitespace_tabs);
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
                use_tabs: true,
                tab_size: Some(8),
                ..Default::default()
            },
        );
        let tabs_config = BufferConfig::resolve(&config_with_tabs, Some("makefile"));
        assert_eq!(tabs_config.indent_string(), "\t");
    }
}
