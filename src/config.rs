use crate::lsp::LspServerConfig;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// Main configuration structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub theme: ThemeConfig,

    #[serde(default)]
    pub editor: EditorConfig,

    #[serde(default)]
    pub keybindings: Vec<Keybinding>,

    #[serde(default)]
    pub languages: HashMap<String, LanguageConfig>,

    #[serde(default)]
    pub lsp: HashMap<String, LspServerConfig>,
}

/// Theme configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThemeConfig {
    #[serde(default = "default_foreground")]
    pub foreground: String,

    #[serde(default = "default_background")]
    pub background: String,

    #[serde(default = "default_cursor")]
    pub cursor: String,

    #[serde(default = "default_selection")]
    pub selection: String,

    #[serde(default)]
    pub syntax: HashMap<String, String>,
}

fn default_foreground() -> String {
    "#ffffff".to_string()
}

fn default_background() -> String {
    "#1e1e1e".to_string()
}

fn default_cursor() -> String {
    "#528bff".to_string()
}

fn default_selection() -> String {
    "#264f78".to_string()
}

impl Default for ThemeConfig {
    fn default() -> Self {
        let mut syntax = HashMap::new();
        syntax.insert("keyword".to_string(), "#569cd6".to_string());
        syntax.insert("string".to_string(), "#ce9178".to_string());
        syntax.insert("comment".to_string(), "#6a9955".to_string());
        syntax.insert("function".to_string(), "#dcdcaa".to_string());
        syntax.insert("type".to_string(), "#4ec9b0".to_string());
        syntax.insert("variable".to_string(), "#9cdcfe".to_string());

        Self {
            foreground: default_foreground(),
            background: default_background(),
            cursor: default_cursor(),
            selection: default_selection(),
            syntax,
        }
    }
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

    #[serde(default = "default_highlight_timeout")]
    pub highlight_timeout_ms: u64,

    #[serde(default = "default_snapshot_interval")]
    pub snapshot_interval: usize,
}

fn default_tab_size() -> usize {
    4
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

impl Default for EditorConfig {
    fn default() -> Self {
        Self {
            tab_size: default_tab_size(),
            auto_indent: true,
            line_numbers: true,
            relative_line_numbers: false,
            scroll_offset: default_scroll_offset(),
            syntax_highlighting: true,
            highlight_timeout_ms: default_highlight_timeout(),
            snapshot_interval: default_snapshot_interval(),
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

impl Default for Config {
    fn default() -> Self {
        Self {
            theme: ThemeConfig::default(),
            editor: EditorConfig::default(),
            keybindings: Self::default_keybindings(),
            languages: Self::default_languages(),
            lsp: Self::default_lsp_config(),
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
        lsp.insert(
            "rust".to_string(),
            LspServerConfig {
                command: "rust-analyzer".to_string(),
                args: vec![],
                enabled: true,
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
        assert_eq!(config.theme.foreground, loaded.theme.foreground);
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
