use crate::lsp::LspServerConfig;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// Main configuration structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    #[serde(default = "default_theme_name")]
    pub theme: String,

    #[serde(default)]
    pub editor: EditorConfig,

    #[serde(default)]
    pub file_explorer: FileExplorerConfig,

    #[serde(default)]
    pub keybindings: Vec<Keybinding>,

    #[serde(default)]
    pub languages: HashMap<String, LanguageConfig>,

    #[serde(default)]
    pub lsp: HashMap<String, LspServerConfig>,

    #[serde(default)]
    pub menu: MenuConfig,
}

fn default_theme_name() -> String {
    "high-contrast".to_string()
}

/// Editor behavior configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EditorConfig {
    #[serde(default = "default_tab_size")]
    pub tab_size: usize,

    #[serde(default = "default_true")]
    pub auto_indent: bool,

    #[serde(default = "default_true")]
    pub line_numbers: bool,

    #[serde(default = "default_false")]
    pub relative_line_numbers: bool,

    #[serde(default = "default_scroll_offset")]
    pub scroll_offset: usize,

    #[serde(default = "default_true")]
    pub syntax_highlighting: bool,

    #[serde(default = "default_true")]
    pub line_wrap: bool,

    #[serde(default = "default_highlight_timeout")]
    pub highlight_timeout_ms: u64,

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
        }
    }
}

/// File explorer configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
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

/// Keybinding definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Keybinding {
    /// Key name (e.g., "a", "Enter", "F1")
    pub key: String,

    /// Modifiers (e.g., ["ctrl"], ["ctrl", "shift"])
    #[serde(default)]
    pub modifiers: Vec<String>,

    /// Action to perform (e.g., "insert_char", "move_left")
    pub action: String,

    /// Optional arguments for the action
    #[serde(default)]
    pub args: HashMap<String, serde_json::Value>,

    /// Optional condition (e.g., "mode == insert")
    #[serde(default)]
    pub when: Option<String>,
}

/// Language-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageConfig {
    /// File extensions for this language
    pub extensions: Vec<String>,

    /// Tree-sitter grammar name
    pub grammar: String,

    /// Comment prefix
    #[serde(default)]
    pub comment_prefix: Option<String>,

    /// Whether to auto-indent
    #[serde(default = "default_true")]
    pub auto_indent: bool,
}

/// Menu bar configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MenuConfig {
    #[serde(default)]
    pub menus: Vec<Menu>,
}

/// A top-level menu in the menu bar
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Menu {
    pub label: String,
    pub items: Vec<MenuItem>,
}

/// A menu item (action, separator, or submenu)
#[derive(Debug, Clone, Serialize, Deserialize)]
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
            editor: EditorConfig::default(),
            file_explorer: FileExplorerConfig::default(),
            keybindings: Self::default_keybindings(),
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
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> Result<Self, ConfigError> {
        let contents = std::fs::read_to_string(path.as_ref())
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        let config: Config =
            serde_json::from_str(&contents).map_err(|e| ConfigError::ParseError(e.to_string()))?;

        Ok(config)
    }

    /// Save configuration to a JSON file
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), ConfigError> {
        let contents = serde_json::to_string_pretty(self)
            .map_err(|e| ConfigError::SerializeError(e.to_string()))?;

        std::fs::write(path.as_ref(), contents).map_err(|e| ConfigError::IoError(e.to_string()))?;

        Ok(())
    }

    /// Create a new config with default keybindings
    fn default_keybindings() -> Vec<Keybinding> {
        vec![
            // Basic movement
            Keybinding {
                key: "Left".to_string(),
                modifiers: vec![],
                action: "move_left".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "Right".to_string(),
                modifiers: vec![],
                action: "move_right".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "Up".to_string(),
                modifiers: vec![],
                action: "move_up".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "Down".to_string(),
                modifiers: vec![],
                action: "move_down".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // Editing
            Keybinding {
                key: "Backspace".to_string(),
                modifiers: vec![],
                action: "delete_backward".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "Delete".to_string(),
                modifiers: vec![],
                action: "delete_forward".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "Enter".to_string(),
                modifiers: vec![],
                action: "insert_newline".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // File operations
            Keybinding {
                key: "s".to_string(),
                modifiers: vec!["ctrl".to_string()],
                action: "save".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "q".to_string(),
                modifiers: vec!["ctrl".to_string()],
                action: "quit".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // Undo/redo
            Keybinding {
                key: "z".to_string(),
                modifiers: vec!["ctrl".to_string()],
                action: "undo".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "y".to_string(),
                modifiers: vec!["ctrl".to_string()],
                action: "redo".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // Multi-cursor
            Keybinding {
                key: "d".to_string(),
                modifiers: vec!["ctrl".to_string()],
                action: "add_cursor_next_match".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "Up".to_string(),
                modifiers: vec!["ctrl".to_string(), "alt".to_string()],
                action: "add_cursor_above".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "Down".to_string(),
                modifiers: vec!["ctrl".to_string(), "alt".to_string()],
                action: "add_cursor_below".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // Split view operations
            // Using Alt+letter combinations for maximum terminal compatibility
            Keybinding {
                key: "h".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "split_horizontal".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "v".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "split_vertical".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "x".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "close_split".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "o".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "next_split".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "o".to_string(),
                modifiers: vec!["alt".to_string(), "shift".to_string()],
                action: "prev_split".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "=".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "increase_split_size".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "-".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "decrease_split_size".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // LSP features
            Keybinding {
                key: " ".to_string(), // Space key
                modifiers: vec!["ctrl".to_string()],
                action: "lsp_completion".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "F12".to_string(),
                modifiers: vec![],
                action: "lsp_goto_definition".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "k".to_string(),
                modifiers: vec!["ctrl".to_string(), "shift".to_string()],
                action: "lsp_hover".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // LSP signature help (also auto-triggered on '(' and ',')
            Keybinding {
                key: "space".to_string(),
                modifiers: vec!["ctrl".to_string(), "shift".to_string()],
                action: "lsp_signature_help".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // LSP code actions (quick fixes, refactorings)
            // Note: Ctrl+. doesn't work in many terminals, so we use Ctrl+Shift+A
            Keybinding {
                key: "a".to_string(),
                modifiers: vec!["ctrl".to_string(), "shift".to_string()],
                action: "lsp_code_actions".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // File Explorer - Toggle and focus
            Keybinding {
                key: "b".to_string(),
                modifiers: vec!["ctrl".to_string()],
                action: "toggle_file_explorer".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "e".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "focus_file_explorer".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "Escape".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "focus_editor".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // File Explorer - Navigation
            // Note: j/k navigation would need context-aware keybindings
            // For now, using arrow keys which work in both contexts
            Keybinding {
                key: "j".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "file_explorer_down".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "k".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "file_explorer_up".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // File Explorer - Operations
            Keybinding {
                key: "Enter".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "file_explorer_open".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "l".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "file_explorer_expand".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "h".to_string(),
                modifiers: vec!["alt".to_string(), "shift".to_string()],
                action: "file_explorer_collapse".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "r".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "file_explorer_refresh".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // File Explorer - File Operations
            Keybinding {
                key: "n".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "file_explorer_new_file".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "n".to_string(),
                modifiers: vec!["alt".to_string(), "shift".to_string()],
                action: "file_explorer_new_directory".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "d".to_string(),
                modifiers: vec!["alt".to_string(), "shift".to_string()],
                action: "file_explorer_delete".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "F2".to_string(),
                modifiers: vec![],
                action: "file_explorer_rename".to_string(),
                args: HashMap::new(),
                when: None,
            },
            // File Explorer - Toggles
            Keybinding {
                key: ".".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "file_explorer_toggle_hidden".to_string(),
                args: HashMap::new(),
                when: None,
            },
            Keybinding {
                key: "i".to_string(),
                modifiers: vec!["alt".to_string()],
                action: "file_explorer_toggle_gitignored".to_string(),
                args: HashMap::new(),
                when: None,
            },
        ]
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
            },
        );

        languages.insert(
            "javascript".to_string(),
            LanguageConfig {
                extensions: vec!["js".to_string(), "jsx".to_string()],
                grammar: "javascript".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
            },
        );

        languages.insert(
            "typescript".to_string(),
            LanguageConfig {
                extensions: vec!["ts".to_string(), "tsx".to_string()],
                grammar: "typescript".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
            },
        );

        languages.insert(
            "python".to_string(),
            LanguageConfig {
                extensions: vec!["py".to_string()],
                grammar: "python".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
            },
        );

        languages
    }

    /// Create default LSP configurations
    fn default_lsp_config() -> HashMap<String, LspServerConfig> {
        let mut lsp = HashMap::new();

        // rust-analyzer (installed via rustup or package manager)
        // Enable logging to help debug LSP issues
        let ra_log_path = format!("/tmp/rust-analyzer-{}.log", std::process::id());
        tracing::info!("rust-analyzer will log to: {}", ra_log_path);

        lsp.insert(
            "rust".to_string(),
            LspServerConfig {
                command: "rust-analyzer".to_string(),
                args: vec!["--log-file".to_string(), ra_log_path],
                enabled: true,
                process_limits: crate::process_limits::ProcessLimits::default(),
            },
        );

        // pylsp (installed via pip)
        lsp.insert(
            "python".to_string(),
            LspServerConfig {
                command: "pylsp".to_string(),
                args: vec![],
                enabled: true,
                process_limits: crate::process_limits::ProcessLimits::default(),
            },
        );

        // typescript-language-server (installed via npm)
        let ts_lsp = LspServerConfig {
            command: "typescript-language-server".to_string(),
            args: vec!["--stdio".to_string()],
            enabled: true,
            process_limits: crate::process_limits::ProcessLimits::default(),
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
                process_limits: crate::process_limits::ProcessLimits::default(),
            },
        );

        // vscode-css-languageserver-bin (installed via npm)
        lsp.insert(
            "css".to_string(),
            LspServerConfig {
                command: "vscode-css-languageserver-bin".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                process_limits: crate::process_limits::ProcessLimits::default(),
            },
        );

        // clangd (installed via package manager)
        lsp.insert(
            "c".to_string(),
            LspServerConfig {
                command: "clangd".to_string(),
                args: vec![],
                enabled: true,
                process_limits: crate::process_limits::ProcessLimits::default(),
            },
        );
        lsp.insert(
            "cpp".to_string(),
            LspServerConfig {
                command: "clangd".to_string(),
                args: vec![],
                enabled: true,
                process_limits: crate::process_limits::ProcessLimits::default(),
            },
        );

        // gopls (installed via go install)
        lsp.insert(
            "go".to_string(),
            LspServerConfig {
                command: "gopls".to_string(),
                args: vec![],
                enabled: true,
                process_limits: crate::process_limits::ProcessLimits::default(),
            },
        );

        // vscode-json-languageserver (installed via npm)
        lsp.insert(
            "json".to_string(),
            LspServerConfig {
                command: "vscode-json-languageserver".to_string(),
                args: vec!["--stdio".to_string()],
                enabled: true,
                process_limits: crate::process_limits::ProcessLimits::default(),
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
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Close Buffer".to_string(),
                        action: "close".to_string(),
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
                        when: Some("has_selection".to_string()),
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
                        checkbox: Some("file_explorer".to_string()),
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Line Numbers".to_string(),
                        action: "toggle_line_numbers".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some("line_numbers".to_string()),
                    },
                    MenuItem::Action {
                        label: "Line Wrap".to_string(),
                        action: "toggle_line_wrap".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some("line_wrap".to_string()),
                    },
                    MenuItem::Action {
                        label: "Compose Mode".to_string(),
                        action: "toggle_compose_mode".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: Some("compose_mode".to_string()),
                    },
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
        assert!(!config.keybindings.is_empty());
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
}
