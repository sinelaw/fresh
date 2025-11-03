# Configuration System - JSON-Based with Schema

## Configuration File Location

```
~/.config/editor/config.json  (Linux/Mac)
%APPDATA%\editor\config.json  (Windows)
```

## Configuration Schema

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "Editor Configuration",
  "type": "object",
  "properties": {
    "theme": {
      "type": "object",
      "properties": {
        "background": { "type": "string", "pattern": "^#[0-9a-fA-F]{6}$" },
        "foreground": { "type": "string", "pattern": "^#[0-9a-fA-F]{6}$" },
        "cursor": { "type": "string", "pattern": "^#[0-9a-fA-F]{6}$" },
        "selection": { "type": "string", "pattern": "^#[0-9a-fA-F]{6}$" },
        "line_number": { "type": "string", "pattern": "^#[0-9a-fA-F]{6}$" },
        "syntax": {
          "type": "object",
          "properties": {
            "comment": { "type": "string" },
            "keyword": { "type": "string" },
            "string": { "type": "string" },
            "number": { "type": "string" },
            "function": { "type": "string" },
            "type": { "type": "string" },
            "variable": { "type": "string" }
          }
        }
      }
    },
    "editor": {
      "type": "object",
      "properties": {
        "tab_size": { "type": "integer", "minimum": 1, "maximum": 16 },
        "insert_spaces": { "type": "boolean" },
        "line_numbers": { "type": "boolean" },
        "highlight_current_line": { "type": "boolean" },
        "word_wrap": { "type": "boolean" },
        "scroll_speed": { "type": "integer", "minimum": 1, "maximum": 10 },
        "smooth_scroll": { "type": "boolean" }
      }
    },
    "keybindings": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "key": { "type": "string" },
          "modifiers": {
            "type": "array",
            "items": {
              "type": "string",
              "enum": ["ctrl", "alt", "shift", "super"]
            }
          },
          "action": { "type": "string" },
          "args": { "type": "object" },
          "when": { "type": "string" }
        },
        "required": ["key", "action"]
      }
    },
    "languages": {
      "type": "object",
      "additionalProperties": {
        "type": "object",
        "properties": {
          "extensions": {
            "type": "array",
            "items": { "type": "string" }
          },
          "tree_sitter_grammar": { "type": "string" },
          "comment_token": { "type": "string" }
        }
      }
    }
  }
}
```

## Example Configuration File

```json
{
  "theme": {
    "background": "#1e1e1e",
    "foreground": "#d4d4d4",
    "cursor": "#ffffff",
    "selection": "#264f78",
    "line_number": "#858585",
    "syntax": {
      "comment": "#6a9955",
      "keyword": "#569cd6",
      "string": "#ce9178",
      "number": "#b5cea8",
      "function": "#dcdcaa",
      "type": "#4ec9b0",
      "variable": "#9cdcfe"
    }
  },
  "editor": {
    "tab_size": 4,
    "insert_spaces": true,
    "line_numbers": true,
    "highlight_current_line": true,
    "word_wrap": false,
    "scroll_speed": 3,
    "smooth_scroll": true
  },
  "keybindings": [
    {
      "key": "s",
      "modifiers": ["ctrl"],
      "action": "save"
    },
    {
      "key": "z",
      "modifiers": ["ctrl"],
      "action": "undo"
    },
    {
      "key": "z",
      "modifiers": ["ctrl", "shift"],
      "action": "redo"
    },
    {
      "key": "d",
      "modifiers": ["ctrl"],
      "action": "select_next_occurrence"
    },
    {
      "key": "a",
      "modifiers": ["ctrl"],
      "action": "select_all"
    },
    {
      "key": "f",
      "modifiers": ["ctrl"],
      "action": "find"
    },
    {
      "key": "g",
      "modifiers": ["ctrl"],
      "action": "goto_line"
    },
    {
      "key": "/",
      "modifiers": ["ctrl"],
      "action": "toggle_comment"
    },
    {
      "key": "up",
      "modifiers": ["alt"],
      "action": "move_line_up"
    },
    {
      "key": "down",
      "modifiers": ["alt"],
      "action": "move_line_down"
    },
    {
      "key": "enter",
      "modifiers": ["ctrl"],
      "action": "insert_line_below"
    },
    {
      "key": "enter",
      "modifiers": ["ctrl", "shift"],
      "action": "insert_line_above"
    },
    {
      "key": "q",
      "modifiers": ["ctrl"],
      "action": "quit"
    }
  ],
  "languages": {
    "rust": {
      "extensions": [".rs"],
      "tree_sitter_grammar": "rust",
      "comment_token": "//"
    },
    "javascript": {
      "extensions": [".js", ".jsx", ".mjs"],
      "tree_sitter_grammar": "javascript",
      "comment_token": "//"
    },
    "python": {
      "extensions": [".py"],
      "tree_sitter_grammar": "python",
      "comment_token": "#"
    },
    "json": {
      "extensions": [".json"],
      "tree_sitter_grammar": "json",
      "comment_token": null
    }
  }
}
```

## Rust Implementation

### Config Struct

```rust
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThemeConfig {
    #[serde(default = "default_bg")]
    pub background: String,

    #[serde(default = "default_fg")]
    pub foreground: String,

    #[serde(default = "default_cursor")]
    pub cursor: String,

    #[serde(default = "default_selection")]
    pub selection: String,

    #[serde(default = "default_line_number")]
    pub line_number: String,

    #[serde(default)]
    pub syntax: SyntaxTheme,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SyntaxTheme {
    #[serde(default = "default_comment")]
    pub comment: String,

    #[serde(default = "default_keyword")]
    pub keyword: String,

    #[serde(default = "default_string")]
    pub string: String,

    #[serde(default = "default_number")]
    pub number: String,

    #[serde(default = "default_function")]
    pub function: String,

    #[serde(default = "default_type")]
    pub r#type: String,

    #[serde(default = "default_variable")]
    pub variable: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EditorConfig {
    #[serde(default = "default_tab_size")]
    pub tab_size: usize,

    #[serde(default = "default_true")]
    pub insert_spaces: bool,

    #[serde(default = "default_true")]
    pub line_numbers: bool,

    #[serde(default = "default_true")]
    pub highlight_current_line: bool,

    #[serde(default)]
    pub word_wrap: bool,

    #[serde(default = "default_scroll_speed")]
    pub scroll_speed: usize,

    #[serde(default = "default_true")]
    pub smooth_scroll: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Keybinding {
    pub key: String,

    #[serde(default)]
    pub modifiers: Vec<String>,

    pub action: String,

    #[serde(default)]
    pub args: HashMap<String, serde_json::Value>,

    /// Context condition (e.g., "editorTextFocus && !editorReadonly")
    #[serde(default)]
    pub when: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageConfig {
    pub extensions: Vec<String>,
    pub tree_sitter_grammar: String,
    pub comment_token: Option<String>,
}

// Default value functions
fn default_bg() -> String { "#1e1e1e".to_string() }
fn default_fg() -> String { "#d4d4d4".to_string() }
fn default_cursor() -> String { "#ffffff".to_string() }
fn default_selection() -> String { "#264f78".to_string() }
fn default_line_number() -> String { "#858585".to_string() }
fn default_comment() -> String { "#6a9955".to_string() }
fn default_keyword() -> String { "#569cd6".to_string() }
fn default_string() -> String { "#ce9178".to_string() }
fn default_number() -> String { "#b5cea8".to_string() }
fn default_function() -> String { "#dcdcaa".to_string() }
fn default_type() -> String { "#4ec9b0".to_string() }
fn default_variable() -> String { "#9cdcfe".to_string() }
fn default_tab_size() -> usize { 4 }
fn default_scroll_speed() -> usize { 3 }
fn default_true() -> bool { true }

impl Default for Config {
    fn default() -> Self {
        Config {
            theme: ThemeConfig::default(),
            editor: EditorConfig::default(),
            keybindings: default_keybindings(),
            languages: default_languages(),
        }
    }
}

impl Default for ThemeConfig {
    fn default() -> Self {
        ThemeConfig {
            background: default_bg(),
            foreground: default_fg(),
            cursor: default_cursor(),
            selection: default_selection(),
            line_number: default_line_number(),
            syntax: SyntaxTheme::default(),
        }
    }
}

impl Default for EditorConfig {
    fn default() -> Self {
        EditorConfig {
            tab_size: default_tab_size(),
            insert_spaces: true,
            line_numbers: true,
            highlight_current_line: true,
            word_wrap: false,
            scroll_speed: default_scroll_speed(),
            smooth_scroll: true,
        }
    }
}

fn default_keybindings() -> Vec<Keybinding> {
    vec![
        Keybinding {
            key: "s".to_string(),
            modifiers: vec!["ctrl".to_string()],
            action: "save".to_string(),
            args: HashMap::new(),
            when: None,
        },
        Keybinding {
            key: "z".to_string(),
            modifiers: vec!["ctrl".to_string()],
            action: "undo".to_string(),
            args: HashMap::new(),
            when: None,
        },
        // ... more defaults
    ]
}

fn default_languages() -> HashMap<String, LanguageConfig> {
    let mut langs = HashMap::new();
    langs.insert("rust".to_string(), LanguageConfig {
        extensions: vec![".rs".to_string()],
        tree_sitter_grammar: "rust".to_string(),
        comment_token: Some("//".to_string()),
    });
    // ... more languages
    langs
}
```

### Config Loading

```rust
impl Config {
    /// Load config from standard location
    pub fn load() -> Result<Self> {
        let path = Self::config_path()?;

        if !path.exists() {
            // Create default config
            let config = Config::default();
            config.save()?;
            return Ok(config);
        }

        let contents = std::fs::read_to_string(&path)?;
        let config: Config = serde_json::from_str(&contents)
            .map_err(|e| anyhow!("Failed to parse config: {}", e))?;

        // Validate against schema
        config.validate()?;

        Ok(config)
    }

    /// Save config to standard location
    pub fn save(&self) -> Result<()> {
        let path = Self::config_path()?;

        // Create parent directory if needed
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(&path, json)?;

        Ok(())
    }

    /// Get standard config path
    fn config_path() -> Result<PathBuf> {
        let config_dir = if cfg!(target_os = "windows") {
            std::env::var("APPDATA")?
        } else {
            format!("{}/.config", std::env::var("HOME")?)
        };

        Ok(PathBuf::from(config_dir).join("editor").join("config.json"))
    }

    /// Validate config against schema
    fn validate(&self) -> Result<()> {
        // Check hex colors
        for color in [
            &self.theme.background,
            &self.theme.foreground,
            &self.theme.cursor,
            &self.theme.selection,
        ] {
            if !color.starts_with('#') || color.len() != 7 {
                return Err(anyhow!("Invalid color format: {}", color));
            }
        }

        // Check tab size
        if self.editor.tab_size == 0 || self.editor.tab_size > 16 {
            return Err(anyhow!("Invalid tab_size: {}", self.editor.tab_size));
        }

        // Validate keybindings
        for kb in &self.keybindings {
            if kb.key.is_empty() {
                return Err(anyhow!("Keybinding key cannot be empty"));
            }
            if kb.action.is_empty() {
                return Err(anyhow!("Keybinding action cannot be empty"));
            }
        }

        Ok(())
    }
}
```

### Keybinding System

```rust
/// Action that can be performed in the editor
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    // Editing
    Insert { text: String },
    Delete,
    Backspace,
    Undo,
    Redo,

    // Navigation
    MoveLeft,
    MoveRight,
    MoveUp,
    MoveDown,
    MoveWordLeft,
    MoveWordRight,
    MoveLineStart,
    MoveLineEnd,
    MoveFileStart,
    MoveFileEnd,
    PageUp,
    PageDown,

    // Selection
    SelectLeft,
    SelectRight,
    SelectUp,
    SelectDown,
    SelectAll,
    SelectNextOccurrence,

    // Multi-cursor
    AddCursorAbove,
    AddCursorBelow,
    AddCursorAtMouse { position: usize },

    // File operations
    Save,
    SaveAs,
    Open,
    Close,
    Quit,

    // Search
    Find,
    FindNext,
    FindPrevious,
    Replace,

    // Line operations
    InsertLineAbove,
    InsertLineBelow,
    DeleteLine,
    DuplicateLine,
    MoveLineUp,
    MoveLineDown,
    ToggleComment,

    // View
    ZoomIn,
    ZoomOut,
    ToggleLineNumbers,

    // Custom actions (from config)
    Custom { name: String, args: HashMap<String, serde_json::Value> },
}

pub struct KeybindingResolver {
    /// Map from (key, modifiers) to Action
    bindings: HashMap<(String, Vec<String>), Action>,

    /// Config reference
    config: Config,
}

impl KeybindingResolver {
    pub fn new(config: &Config) -> Self {
        let mut resolver = KeybindingResolver {
            bindings: HashMap::new(),
            config: config.clone(),
        };

        // Build binding map from config
        for kb in &config.keybindings {
            let action = Self::parse_action(&kb.action, &kb.args);
            let key = (kb.key.clone(), kb.modifiers.clone());
            resolver.bindings.insert(key, action);
        }

        resolver
    }

    /// Convert key event to action
    pub fn resolve(&self, key: KeyEvent) -> Option<Action> {
        let key_str = match key.code {
            KeyCode::Char(c) => c.to_string(),
            KeyCode::Enter => "enter".to_string(),
            KeyCode::Backspace => "backspace".to_string(),
            KeyCode::Left => "left".to_string(),
            KeyCode::Right => "right".to_string(),
            KeyCode::Up => "up".to_string(),
            KeyCode::Down => "down".to_string(),
            KeyCode::Tab => "tab".to_string(),
            KeyCode::Esc => "escape".to_string(),
            _ => return None,
        };

        let mut modifiers = Vec::new();
        if key.modifiers.contains(KeyModifiers::CONTROL) {
            modifiers.push("ctrl".to_string());
        }
        if key.modifiers.contains(KeyModifiers::ALT) {
            modifiers.push("alt".to_string());
        }
        if key.modifiers.contains(KeyModifiers::SHIFT) {
            modifiers.push("shift".to_string());
        }

        self.bindings.get(&(key_str, modifiers)).cloned()
    }

    /// Parse action string to Action enum
    fn parse_action(action: &str, args: &HashMap<String, serde_json::Value>) -> Action {
        match action {
            "save" => Action::Save,
            "undo" => Action::Undo,
            "redo" => Action::Redo,
            "select_next_occurrence" => Action::SelectNextOccurrence,
            "select_all" => Action::SelectAll,
            "find" => Action::Find,
            "quit" => Action::Quit,
            "move_line_up" => Action::MoveLineUp,
            "move_line_down" => Action::MoveLineDown,
            "toggle_comment" => Action::ToggleComment,
            _ => Action::Custom {
                name: action.to_string(),
                args: args.clone(),
            },
        }
    }
}
```

### Integration with Editor

```rust
impl Editor {
    pub fn new(file: Option<PathBuf>) -> Result<Self> {
        // Load config
        let config = Config::load()?;

        // Create keybinding resolver
        let keybindings = KeybindingResolver::new(&config);

        // Create editor with config
        let mut editor = Editor {
            state: EditorState::new(),
            log: EventLog::new(),
            highlighter: Highlighter::new(&config),
            terminal: Terminal::new()?,
            config: config.clone(),
            keybindings,
        };

        // Apply theme
        editor.apply_theme(&config.theme)?;

        // Load file if provided
        if let Some(path) = file {
            editor.load_file(&path)?;
        }

        Ok(editor)
    }

    pub fn handle_key_event(&mut self, key: KeyEvent) -> Result<bool> {
        // Resolve keybinding to action
        let action = if let Some(action) = self.keybindings.resolve(key) {
            action
        } else if let KeyCode::Char(c) = key.code {
            // Default: insert character
            Action::Insert { text: c.to_string() }
        } else {
            return Ok(true);
        };

        // Convert action to events
        let events = self.action_to_events(&action)?;

        // Record and apply events
        for event in events {
            let id = self.log.record(event.clone());
            self.state.apply(&event);
        }

        // Special handling for quit
        if matches!(action, Action::Quit) {
            return Ok(false);
        }

        Ok(true)
    }

    fn action_to_events(&self, action: &Action) -> Result<Vec<Event>> {
        match action {
            Action::Insert { text } => {
                // Generate insert event for each cursor
                Ok(self.state.cursors.iter()
                    .map(|(id, cursor)| Event::Insert {
                        position: cursor.position,
                        text: text.clone(),
                        cursor_id: *id,
                    })
                    .collect())
            }

            Action::Undo => {
                // Undo is handled specially
                Ok(vec![])
            }

            Action::Save => {
                self.state.buffer.save()?;
                Ok(vec![])
            }

            // ... more actions
            _ => Ok(vec![]),
        }
    }
}
```

## Hot Reload Configuration

```rust
use notify::{Watcher, RecursiveMode, watcher};
use std::sync::mpsc::channel;
use std::time::Duration;

impl Editor {
    /// Watch config file for changes and reload
    pub fn watch_config(&mut self) -> Result<()> {
        let (tx, rx) = channel();
        let mut watcher = watcher(tx, Duration::from_secs(1))?;

        let config_path = Config::config_path()?;
        watcher.watch(&config_path, RecursiveMode::NonRecursive)?;

        // Store watcher to keep it alive
        self.config_watcher = Some(watcher);
        self.config_reload_rx = Some(rx);

        Ok(())
    }

    /// Check for config changes
    pub fn check_config_reload(&mut self) -> Result<()> {
        if let Some(rx) = &self.config_reload_rx {
            if let Ok(_event) = rx.try_recv() {
                // Config file changed, reload
                self.reload_config()?;
            }
        }
        Ok(())
    }

    fn reload_config(&mut self) -> Result<()> {
        let new_config = Config::load()?;

        // Update config
        self.config = new_config.clone();

        // Rebuild keybindings
        self.keybindings = KeybindingResolver::new(&new_config);

        // Reapply theme
        self.apply_theme(&new_config.theme)?;

        // Show notification
        self.show_status("Configuration reloaded", 2000);

        Ok(())
    }
}
```

## Schema Validation Tool

```bash
# Generate schema file
$ editor --generate-schema > ~/.config/editor/schema.json

# Validate config against schema
$ editor --validate-config ~/.config/editor/config.json
✓ Configuration is valid

# Show current config
$ editor --show-config
{
  "theme": { ... },
  ...
}
```

## Updated File Structure

```
src/
├── main.rs           # Entry point, CLI arg parsing
├── editor.rs         # Editor struct, event loop
├── event.rs          # Event enum, EventLog
├── state.rs          # EditorState, apply() method
├── buffer.rs         # Buffer struct, line cache
├── cursor.rs         # Cursor struct, CursorId
├── viewport.rs       # Viewport, smart scrolling
├── highlighter.rs    # Syntax highlighting
├── chunk_tree.rs     # Rope implementation
├── config.rs         # Config struct, loading/saving  (NEW)
├── keybindings.rs    # KeybindingResolver, Action    (NEW)
├── theme.rs          # Theme application              (NEW)
└── render.rs         # Terminal rendering
```

## Benefits

1. **User-friendly**: JSON is easy to edit
2. **Validated**: Schema prevents invalid configs
3. **Hot reload**: Changes apply immediately
4. **Extensible**: Easy to add new options
5. **Discoverable**: Schema shows all options
6. **VSCode-like**: Familiar keybinding format

Ready to implement this configuration system?
