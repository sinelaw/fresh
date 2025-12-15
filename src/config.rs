use crate::types::{context_keys, LspServerConfig, ProcessLimits};

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

/// Newtype for keybinding map name that generates proper JSON Schema with enum options
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct KeybindingMapName(pub String);

impl KeybindingMapName {
    /// Built-in keybinding map options shown in the settings dropdown
    pub const BUILTIN_OPTIONS: &'static [&'static str] = &["default", "emacs", "vscode"];
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
    /// Color theme name
    #[serde(default = "default_theme_name")]
    pub theme: ThemeName,

    /// Check for new versions on quit (default: true)
    #[serde(default = "default_true")]
    pub check_for_updates: bool,

    /// Editor behavior settings (indentation, line numbers, wrapping, etc.)
    #[serde(default)]
    pub editor: EditorConfig,

    /// File explorer panel settings
    #[serde(default)]
    pub file_explorer: FileExplorerConfig,

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

    /// Menu bar configuration
    #[serde(default)]
    pub menu: MenuConfig,
}

fn default_keybinding_map_name() -> KeybindingMapName {
    KeybindingMapName("default".to_string())
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

    /// Show line numbers in the gutter
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

    /// Wrap long lines to fit the window width
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
pub struct KeymapConfig {
    /// Optional parent keymap to inherit from
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inherits: Option<String>,

    /// Keybindings defined in this keymap
    #[serde(default)]
    pub bindings: Vec<Keybinding>,
}

/// Language-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct LanguageConfig {
    /// File extensions for this language
    #[serde(default)]
    pub extensions: Vec<String>,

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
    /// Display label for the menu (e.g., "File", "Edit")
    pub label: String,
    /// Menu items (actions, separators, or submenus)
    pub items: Vec<MenuItem>,
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
    Submenu { label: String, items: Vec<MenuItem> },
}

impl Default for Config {
    fn default() -> Self {
        Self {
            theme: default_theme_name(),
            check_for_updates: true,
            editor: EditorConfig::default(),
            file_explorer: FileExplorerConfig::default(),
            terminal: TerminalConfig::default(),
            keybindings: vec![], // User customizations only; defaults come from active_keybinding_map
            keybinding_maps: HashMap::new(), // User-defined maps go here
            active_keybinding_map: default_keybinding_map_name(),
            languages: Self::default_languages(),
            lsp: Self::default_lsp_config(),
            menu: MenuConfig::default(),
        }
    }
}

impl Default for MenuConfig {
    fn default() -> Self {
        Self {
            menus: Config::default_menus(),
        }
    }
}

impl Config {
    /// Load configuration from a JSON file
    ///
    /// This deserializes the user's config file and merges it with defaults.
    /// For HashMap fields like `lsp` and `languages`, entries from the user config
    /// are merged with (and override) the default entries. This allows users to
    /// customize a single LSP server without losing the defaults for others.
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> Result<Self, ConfigError> {
        let contents = std::fs::read_to_string(path.as_ref())
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        let mut config: Config =
            serde_json::from_str(&contents).map_err(|e| ConfigError::ParseError(e.to_string()))?;

        // Merge with defaults for HashMap fields
        config.merge_defaults_for_maps();

        Ok(config)
    }

    /// Save configuration to a JSON file
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), ConfigError> {
        let contents = serde_json::to_string_pretty(self)
            .map_err(|e| ConfigError::SerializeError(e.to_string()))?;

        std::fs::write(path.as_ref(), contents).map_err(|e| ConfigError::IoError(e.to_string()))?;

        Ok(())
    }

    /// Merge default values for HashMap fields that should combine user entries with defaults.
    ///
    /// This is called after deserializing user config to ensure that:
    /// - Default LSP servers are present even if user only customizes one
    /// - Default language configs are present even if user only customizes one
    ///
    /// User entries override defaults when keys collide.
    pub(crate) fn merge_defaults_for_maps(&mut self) {
        let defaults = Self::default();

        // Merge LSP configs: start with defaults, overlay user entries
        let user_lsp = std::mem::take(&mut self.lsp);
        self.lsp = defaults.lsp;
        for (key, value) in user_lsp {
            self.lsp.insert(key, value);
        }

        // Merge language configs: start with defaults, overlay user entries
        let user_languages = std::mem::take(&mut self.languages);
        self.languages = defaults.languages;
        for (key, value) in user_languages {
            self.languages.insert(key, value);
        }

        // Note: keybinding_maps is NOT merged - user defines their own complete maps
        // Note: keybindings Vec is NOT merged - it's user customizations only
        // Note: menu is NOT merged - user can completely override the menu structure
    }

    /// Load a built-in keymap from embedded JSON
    fn load_builtin_keymap(name: &str) -> Option<KeymapConfig> {
        let json_content = match name {
            "default" => include_str!("../keymaps/default.json"),
            "emacs" => include_str!("../keymaps/emacs.json"),
            "vscode" => include_str!("../keymaps/vscode.json"),
            _ => return None,
        };

        serde_json::from_str(json_content).ok()
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
                grammar: "rust".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
            },
        );

        languages.insert(
            "javascript".to_string(),
            LanguageConfig {
                extensions: vec!["js".to_string(), "jsx".to_string()],
                grammar: "javascript".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
            },
        );

        languages.insert(
            "typescript".to_string(),
            LanguageConfig {
                extensions: vec!["ts".to_string(), "tsx".to_string()],
                grammar: "typescript".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
            },
        );

        languages.insert(
            "python".to_string(),
            LanguageConfig {
                extensions: vec!["py".to_string()],
                grammar: "python".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
            },
        );

        languages.insert(
            "c".to_string(),
            LanguageConfig {
                extensions: vec!["c".to_string(), "h".to_string()],
                grammar: "c".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
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
                grammar: "cpp".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
            },
        );

        languages.insert(
            "csharp".to_string(),
            LanguageConfig {
                extensions: vec!["cs".to_string()],
                grammar: "c_sharp".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                highlighter: HighlighterPreference::Auto,
                textmate_grammar: None,
            },
        );

        languages
    }

    /// Create default LSP configurations
    fn default_lsp_config() -> HashMap<String, LspServerConfig> {
        let mut lsp = HashMap::new();

        // rust-analyzer (installed via rustup or package manager)
        // Enable logging to help debug LSP issues (cross-platform temp directory)
        let ra_log_path = std::env::temp_dir()
            .join(format!("rust-analyzer-{}.log", std::process::id()))
            .to_string_lossy()
            .to_string();

        lsp.insert(
            "rust".to_string(),
            LspServerConfig {
                command: "rust-analyzer".to_string(),
                args: vec!["--log-file".to_string(), ra_log_path],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
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

        // vscode-html-languageserver-bin (installed via npm)
        lsp.insert(
            "html".to_string(),
            LspServerConfig {
                command: "vscode-html-languageserver-bin".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: None,
            },
        );

        // vscode-css-languageserver-bin (installed via npm)
        lsp.insert(
            "css".to_string(),
            LspServerConfig {
                command: "vscode-css-languageserver-bin".to_string(),
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

        // vscode-json-languageserver (installed via npm)
        lsp.insert(
            "json".to_string(),
            LspServerConfig {
                command: "vscode-json-languageserver".to_string(),
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

    /// Create default menu bar configuration
    fn default_menus() -> Vec<Menu> {
        vec![
            // File menu
            Menu {
                label: "File".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "New File".to_string(),
                        action: "new".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Open File...".to_string(),
                        action: "open".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Save".to_string(),
                        action: "save".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Save As...".to_string(),
                        action: "save_as".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Revert".to_string(),
                        action: "revert".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Close Buffer".to_string(),
                        action: "close".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Switch Project...".to_string(),
                        action: "switch_project".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Quit".to_string(),
                        action: "quit".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // Edit menu
            Menu {
                label: "Edit".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "Undo".to_string(),
                        action: "undo".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Redo".to_string(),
                        action: "redo".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Cut".to_string(),
                        action: "cut".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Copy".to_string(),
                        action: "copy".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Paste".to_string(),
                        action: "paste".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Select All".to_string(),
                        action: "select_all".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Find...".to_string(),
                        action: "search".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Find in Selection".to_string(),
                        action: "find_in_selection".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::HAS_SELECTION.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Find Next".to_string(),
                        action: "find_next".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Find Previous".to_string(),
                        action: "find_previous".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Replace...".to_string(),
                        action: "query_replace".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Delete Line".to_string(),
                        action: "delete_line".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // View menu
            Menu {
                label: "View".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "File Explorer".to_string(),
                        action: "toggle_file_explorer".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::FILE_EXPLORER.to_string()),
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Line Numbers".to_string(),
                        action: "toggle_line_numbers".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::LINE_NUMBERS.to_string()),
                    },
                    MenuItem::Action {
                        label: "Line Wrap".to_string(),
                        action: "toggle_line_wrap".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::LINE_WRAP.to_string()),
                    },
                    MenuItem::Action {
                        label: "Mouse Support".to_string(),
                        action: "toggle_mouse_capture".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::MOUSE_CAPTURE.to_string()),
                    },
                    // Note: Compose Mode removed from menu - markdown_compose plugin provides this
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Set Background...".to_string(),
                        action: "set_background".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Set Background Blend...".to_string(),
                        action: "set_background_blend".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Set Compose Width...".to_string(),
                        action: "set_compose_width".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Select Theme...".to_string(),
                        action: "select_theme".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Settings...".to_string(),
                        action: "open_settings".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Split Horizontal".to_string(),
                        action: "split_horizontal".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Split Vertical".to_string(),
                        action: "split_vertical".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Close Split".to_string(),
                        action: "close_split".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Focus Next Split".to_string(),
                        action: "next_split".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Focus Previous Split".to_string(),
                        action: "prev_split".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Toggle Maximize Split".to_string(),
                        action: "toggle_maximize_split".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Submenu {
                        label: "Terminal".to_string(),
                        items: vec![
                            MenuItem::Action {
                                label: "Open Terminal".to_string(),
                                action: "open_terminal".to_string(),
                                args: HashMap::new(),
                                when: None,
                                checkbox: None,
                            },
                            MenuItem::Action {
                                label: "Close Terminal".to_string(),
                                action: "close_terminal".to_string(),
                                args: HashMap::new(),
                                when: None,
                                checkbox: None,
                            },
                            MenuItem::Separator { separator: true },
                            MenuItem::Action {
                                label: "Toggle Keyboard Capture".to_string(),
                                action: "toggle_keyboard_capture".to_string(),
                                args: HashMap::new(),
                                when: None,
                                checkbox: None,
                            },
                        ],
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Submenu {
                        label: "Keybinding Style".to_string(),
                        items: vec![
                            MenuItem::Action {
                                label: "Default".to_string(),
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
                                label: "Emacs".to_string(),
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
                                label: "VSCode".to_string(),
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
                label: "Selection".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "Select All".to_string(),
                        action: "select_all".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Select Word".to_string(),
                        action: "select_word".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Select Line".to_string(),
                        action: "select_line".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Expand Selection".to_string(),
                        action: "expand_selection".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Add Cursor Above".to_string(),
                        action: "add_cursor_above".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Add Cursor Below".to_string(),
                        action: "add_cursor_below".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Add Cursor at Next Match".to_string(),
                        action: "add_cursor_next_match".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Remove Secondary Cursors".to_string(),
                        action: "remove_secondary_cursors".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // Go menu
            Menu {
                label: "Go".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "Go to Line...".to_string(),
                        action: "goto_line".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Go to Definition".to_string(),
                        action: "lsp_goto_definition".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Find References".to_string(),
                        action: "lsp_references".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Next Buffer".to_string(),
                        action: "next_buffer".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Previous Buffer".to_string(),
                        action: "prev_buffer".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Command Palette...".to_string(),
                        action: "command_palette".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // LSP menu (Language Server Protocol operations)
            Menu {
                label: "LSP".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "Show Hover Info".to_string(),
                        action: "lsp_hover".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Go to Definition".to_string(),
                        action: "lsp_goto_definition".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Find References".to_string(),
                        action: "lsp_references".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Rename Symbol".to_string(),
                        action: "lsp_rename".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Show Completions".to_string(),
                        action: "lsp_completion".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Show Signature Help".to_string(),
                        action: "lsp_signature_help".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Code Actions".to_string(),
                        action: "lsp_code_actions".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Toggle Inlay Hints".to_string(),
                        action: "toggle_inlay_hints".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::LSP_AVAILABLE.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Toggle Mouse Hover".to_string(),
                        action: "toggle_mouse_hover".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some(context_keys::MOUSE_HOVER.to_string()),
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Restart Server".to_string(),
                        action: "lsp_restart".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Stop Server".to_string(),
                        action: "lsp_stop".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
            // Explorer menu (file explorer operations)
            Menu {
                label: "Explorer".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "New File".to_string(),
                        action: "file_explorer_new_file".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "New Folder".to_string(),
                        action: "file_explorer_new_directory".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Open".to_string(),
                        action: "file_explorer_open".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Rename".to_string(),
                        action: "file_explorer_rename".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Delete".to_string(),
                        action: "file_explorer_delete".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Refresh".to_string(),
                        action: "file_explorer_refresh".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER_FOCUSED.to_string()),
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Show Hidden Files".to_string(),
                        action: "file_explorer_toggle_hidden".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER.to_string()),
                        checkbox: Some(context_keys::FILE_EXPLORER_SHOW_HIDDEN.to_string()),
                    },
                    MenuItem::Action {
                        label: "Show Gitignored Files".to_string(),
                        action: "file_explorer_toggle_gitignored".to_string(),
                        args: HashMap::new(),
                        when: Some(context_keys::FILE_EXPLORER.to_string()),
                        checkbox: Some(context_keys::FILE_EXPLORER_SHOW_GITIGNORED.to_string()),
                    },
                ],
            },
            // Help menu
            Menu {
                label: "Help".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "Show Fresh Manual".to_string(),
                        action: "show_help".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Keyboard Shortcuts".to_string(),
                        action: "keyboard_shortcuts".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
            },
        ]
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
            ConfigError::IoError(msg) => write!(f, "IO error: {msg}"),
            ConfigError::ParseError(msg) => write!(f, "Parse error: {msg}"),
            ConfigError::SerializeError(msg) => write!(f, "Serialize error: {msg}"),
            ConfigError::ValidationError(msg) => write!(f, "Validation error: {msg}"),
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
    fn test_config_validation() {
        let mut config = Config::default();
        assert!(config.validate().is_ok());

        config.editor.tab_size = 0;
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_config_save_load() {
        let temp_dir = tempfile::tempdir().unwrap();
        let config_path = temp_dir.path().join("config.json");

        let config = Config::default();
        config.save_to_file(&config_path).unwrap();

        let loaded = Config::load_from_file(&config_path).unwrap();
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
        assert_eq!(loaded.lsp["rust"].command, "custom-rust-analyzer");

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
}
