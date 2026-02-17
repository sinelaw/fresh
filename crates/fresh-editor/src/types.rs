//! Shared configuration types used by both schema generation and runtime.
//!
//! These types are kept in a separate module so that the schema generator
//! can import them without pulling in heavy runtime dependencies.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Constants for menu context state keys
/// These are used both in menu item `when` conditions and `checkbox` states
pub mod context_keys {
    pub const LINE_NUMBERS: &str = "line_numbers";
    pub const LINE_WRAP: &str = "line_wrap";
    pub const COMPOSE_MODE: &str = "compose_mode";
    pub const FILE_EXPLORER: &str = "file_explorer";
    pub const MENU_BAR: &str = "menu_bar";
    pub const FILE_EXPLORER_FOCUSED: &str = "file_explorer_focused";
    pub const MOUSE_CAPTURE: &str = "mouse_capture";
    pub const MOUSE_HOVER: &str = "mouse_hover";
    pub const LSP_AVAILABLE: &str = "lsp_available";
    pub const FILE_EXPLORER_SHOW_HIDDEN: &str = "file_explorer_show_hidden";
    pub const FILE_EXPLORER_SHOW_GITIGNORED: &str = "file_explorer_show_gitignored";
    pub const HAS_SELECTION: &str = "has_selection";
    pub const FORMATTER_AVAILABLE: &str = "formatter_available";
    pub const INLAY_HINTS: &str = "inlay_hints";
    pub const SESSION_MODE: &str = "session_mode";
    pub const VERTICAL_SCROLLBAR: &str = "vertical_scrollbar";
    pub const HORIZONTAL_SCROLLBAR: &str = "horizontal_scrollbar";
    pub const SCROLL_SYNC: &str = "scroll_sync";
    pub const HAS_SAME_BUFFER_SPLITS: &str = "has_same_buffer_splits";
}

/// Configuration for process resource limits
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, JsonSchema)]
pub struct ProcessLimits {
    /// Maximum memory usage as percentage of total system memory (None = no limit)
    /// Default is 50% of total system memory
    #[serde(default)]
    pub max_memory_percent: Option<u32>,

    /// Maximum CPU usage as percentage of total CPU (None = no limit)
    /// For multi-core systems, 100% = 1 core, 200% = 2 cores, etc.
    #[serde(default)]
    pub max_cpu_percent: Option<u32>,

    /// Enable resource limiting (can be disabled per-platform)
    #[serde(default = "default_true")]
    pub enabled: bool,
}

fn default_true() -> bool {
    true
}

impl Default for ProcessLimits {
    fn default() -> Self {
        Self {
            max_memory_percent: Some(50),       // 50% of total memory
            max_cpu_percent: Some(90),          // 90% of total CPU
            enabled: cfg!(target_os = "linux"), // Only enabled on Linux by default
        }
    }
}

impl ProcessLimits {
    /// Create a new ProcessLimits with no restrictions
    pub fn unlimited() -> Self {
        Self {
            max_memory_percent: None,
            max_cpu_percent: None,
            enabled: false,
        }
    }

    /// Get the default CPU limit (90% of total CPU)
    pub fn default_cpu_limit_percent() -> u32 {
        90
    }
}

/// LSP server configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/command"))]
pub struct LspServerConfig {
    /// Command to spawn the server.
    /// Required when enabled=true, ignored when enabled=false.
    #[serde(default)]
    pub command: String,

    /// Arguments to pass to the server
    #[serde(default)]
    pub args: Vec<String>,

    /// Whether the server is enabled
    #[serde(default = "default_true")]
    pub enabled: bool,

    /// Whether to auto-start this LSP server when opening matching files
    /// If false (default), the server must be started manually via command palette
    #[serde(default)]
    pub auto_start: bool,

    /// Process resource limits (memory and CPU)
    #[serde(default)]
    pub process_limits: ProcessLimits,

    /// Custom initialization options to send to the server
    /// These are passed in the `initializationOptions` field of the LSP Initialize request
    #[serde(default)]
    pub initialization_options: Option<serde_json::Value>,
}

impl LspServerConfig {
    /// Merge this config with defaults, using default values for empty/unset fields.
    ///
    /// This is used when loading configs where fields like `command` may be empty
    /// (serde's default) because they weren't specified in the user's config file.
    pub fn merge_with_defaults(self, defaults: &LspServerConfig) -> LspServerConfig {
        LspServerConfig {
            command: if self.command.is_empty() {
                defaults.command.clone()
            } else {
                self.command
            },
            args: if self.args.is_empty() {
                defaults.args.clone()
            } else {
                self.args
            },
            enabled: self.enabled,
            auto_start: self.auto_start,
            process_limits: self.process_limits,
            initialization_options: self
                .initialization_options
                .or_else(|| defaults.initialization_options.clone()),
        }
    }
}
