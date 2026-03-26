//! Shared configuration types used by both schema generation and runtime.
//!
//! These types are kept in a separate module so that the schema generator
//! can import them without pulling in heavy runtime dependencies.

use std::collections::HashMap;
use std::collections::HashSet;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Constants for menu context state keys
/// These are used both in menu item `when` conditions and `checkbox` states
pub mod context_keys {
    pub const LINE_NUMBERS: &str = "line_numbers";
    pub const LINE_WRAP: &str = "line_wrap";
    pub const PAGE_VIEW: &str = "page_view";
    /// Backward-compatible alias for PAGE_VIEW
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
    pub const KEYMAP_DEFAULT: &str = "keymap_default";
    pub const KEYMAP_EMACS: &str = "keymap_emacs";
    pub const KEYMAP_VSCODE: &str = "keymap_vscode";
    pub const KEYMAP_MACOS_GUI: &str = "keymap_macos_gui";
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

/// LSP features that can be routed to specific servers in a multi-server setup.
///
/// Features are classified as either "merged" (results from all servers are combined)
/// or "exclusive" (first eligible server wins). This classification is used by the
/// dispatch layer, not by this enum itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum LspFeature {
    /// Diagnostics (merged: combined from all servers)
    Diagnostics,
    /// Code completion (merged: combined from all servers)
    Completion,
    /// Code actions / quick fixes (merged: combined from all servers)
    CodeAction,
    /// Document symbols (merged: combined from all servers)
    DocumentSymbols,
    /// Workspace symbols (merged: combined from all servers)
    WorkspaceSymbols,
    /// Hover information (exclusive: first eligible server wins)
    Hover,
    /// Go to definition, declaration, type definition, implementation (exclusive)
    Definition,
    /// Find references (exclusive)
    References,
    /// Document formatting and range formatting (exclusive)
    Format,
    /// Rename and prepare rename (exclusive)
    Rename,
    /// Signature help (exclusive)
    SignatureHelp,
    /// Inlay hints (exclusive)
    InlayHints,
    /// Folding ranges (exclusive)
    FoldingRange,
    /// Semantic tokens (exclusive)
    SemanticTokens,
    /// Document highlight (exclusive)
    DocumentHighlight,
}

impl LspFeature {
    /// Whether this feature produces merged results from all eligible servers.
    /// Merged features send requests to all servers and combine the results.
    /// Non-merged (exclusive) features use only the first eligible server.
    pub fn is_merged(&self) -> bool {
        matches!(
            self,
            LspFeature::Diagnostics
                | LspFeature::Completion
                | LspFeature::CodeAction
                | LspFeature::DocumentSymbols
                | LspFeature::WorkspaceSymbols
        )
    }
}

/// Feature filter for an LSP server, controlling which features it handles.
///
/// - `All`: The server handles all features (default).
/// - `Only(set)`: The server handles only the listed features.
/// - `Except(set)`: The server handles all features except the listed ones.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FeatureFilter {
    All,
    Only(HashSet<LspFeature>),
    Except(HashSet<LspFeature>),
}

impl Default for FeatureFilter {
    fn default() -> Self {
        FeatureFilter::All
    }
}

impl FeatureFilter {
    /// Check if this filter allows a given feature.
    pub fn allows(&self, feature: LspFeature) -> bool {
        match self {
            FeatureFilter::All => true,
            FeatureFilter::Only(set) => set.contains(&feature),
            FeatureFilter::Except(set) => !set.contains(&feature),
        }
    }

    /// Build a FeatureFilter from the only_features/except_features config fields.
    pub fn from_config(
        only: &Option<Vec<LspFeature>>,
        except: &Option<Vec<LspFeature>>,
    ) -> FeatureFilter {
        match (only, except) {
            (Some(only), _) => FeatureFilter::Only(only.iter().copied().collect()),
            (_, Some(except)) => FeatureFilter::Except(except.iter().copied().collect()),
            _ => FeatureFilter::All,
        }
    }
}

/// Wrapper for deserializing a per-language LSP config that can be either
/// a single server object or an array of server objects.
///
/// ```json
/// { "lsp": { "rust": { "command": "rust-analyzer" } } }          // single
/// { "lsp": { "python": [{ "command": "pyright" }, { "command": "ruff" }] } }  // multi
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum LspLanguageConfig {
    /// Multiple servers for this language (array form)
    Multi(Vec<LspServerConfig>),
    /// A single server for this language (object form)
    Single(LspServerConfig),
}

impl LspLanguageConfig {
    /// Convert to a Vec of server configs.
    pub fn into_vec(self) -> Vec<LspServerConfig> {
        match self {
            LspLanguageConfig::Single(c) => vec![c],
            LspLanguageConfig::Multi(v) => v,
        }
    }

    /// Get a reference as a slice of server configs.
    pub fn as_slice(&self) -> &[LspServerConfig] {
        match self {
            LspLanguageConfig::Single(c) => std::slice::from_ref(c),
            LspLanguageConfig::Multi(v) => v,
        }
    }
}

impl Default for LspLanguageConfig {
    fn default() -> Self {
        LspLanguageConfig::Single(LspServerConfig::default())
    }
}

/// LSP server configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/command"))]
pub struct LspServerConfig {
    /// Display name for this server (e.g., "tsserver", "eslint").
    /// Defaults to the command basename if not specified.
    #[serde(default)]
    pub name: Option<String>,

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

    /// Environment variables to set for the LSP server process.
    /// These are added to (or override) the inherited parent environment.
    #[serde(default)]
    pub env: HashMap<String, String>,

    /// Override the LSP languageId sent in textDocument/didOpen based on file extension.
    /// Maps file extension (without dot) to LSP language ID string.
    /// For example: `{"tsx": "typescriptreact", "jsx": "javascriptreact"}`
    #[serde(default)]
    pub language_id_overrides: HashMap<String, String>,

    /// Restrict this server to only handle the listed features.
    /// Mutually exclusive with `except_features`. If neither is set, all features are handled.
    #[serde(default)]
    pub only_features: Option<Vec<LspFeature>>,

    /// Exclude the listed features from this server.
    /// Mutually exclusive with `only_features`. If neither is set, all features are handled.
    #[serde(default)]
    pub except_features: Option<Vec<LspFeature>>,

    /// File/directory names to search for when detecting the workspace root.
    /// The editor walks upward from the opened file's directory looking for
    /// any of these markers. The first directory containing a match becomes
    /// the workspace root sent to the LSP server.
    ///
    /// If empty, falls back to `[".git"]` as a universal marker.
    /// If the walk reaches a filesystem boundary without a match, uses the
    /// file's parent directory (never cwd or $HOME).
    #[serde(default)]
    pub root_markers: Vec<String>,
}

impl LspServerConfig {
    /// Merge this config with defaults, using default values for empty/unset fields.
    ///
    /// This is used when loading configs where fields like `command` may be empty
    /// (serde's default) because they weren't specified in the user's config file.
    /// Resolve the display name for this server.
    /// Returns the explicit name if set, otherwise the basename of the command.
    pub fn display_name(&self) -> String {
        if let Some(ref name) = self.name {
            return name.clone();
        }
        // Use command basename
        std::path::Path::new(&self.command)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(&self.command)
            .to_string()
    }

    /// Build the FeatureFilter for this server config.
    pub fn feature_filter(&self) -> FeatureFilter {
        FeatureFilter::from_config(&self.only_features, &self.except_features)
    }

    /// Merge this config with defaults, using default values for empty/unset fields.
    ///
    /// This is used when loading configs where fields like `command` may be empty
    /// (serde's default) because they weren't specified in the user's config file.
    pub fn merge_with_defaults(self, defaults: &LspServerConfig) -> LspServerConfig {
        LspServerConfig {
            name: self.name.or_else(|| defaults.name.clone()),
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
            only_features: self
                .only_features
                .or_else(|| defaults.only_features.clone()),
            except_features: self
                .except_features
                .or_else(|| defaults.except_features.clone()),
            initialization_options: self
                .initialization_options
                .or_else(|| defaults.initialization_options.clone()),
            env: {
                let mut merged = defaults.env.clone();
                merged.extend(self.env);
                merged
            },
            language_id_overrides: {
                let mut merged = defaults.language_id_overrides.clone();
                merged.extend(self.language_id_overrides);
                merged
            },
            root_markers: if self.root_markers.is_empty() {
                defaults.root_markers.clone()
            } else {
                self.root_markers
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn test_lsp_feature_is_merged() {
        assert!(LspFeature::Diagnostics.is_merged());
        assert!(LspFeature::Completion.is_merged());
        assert!(LspFeature::CodeAction.is_merged());
        assert!(LspFeature::DocumentSymbols.is_merged());
        assert!(LspFeature::WorkspaceSymbols.is_merged());

        assert!(!LspFeature::Hover.is_merged());
        assert!(!LspFeature::Definition.is_merged());
        assert!(!LspFeature::References.is_merged());
        assert!(!LspFeature::Format.is_merged());
        assert!(!LspFeature::Rename.is_merged());
        assert!(!LspFeature::SignatureHelp.is_merged());
        assert!(!LspFeature::InlayHints.is_merged());
        assert!(!LspFeature::FoldingRange.is_merged());
        assert!(!LspFeature::SemanticTokens.is_merged());
        assert!(!LspFeature::DocumentHighlight.is_merged());
    }

    #[test]
    fn test_feature_filter_all() {
        let filter = FeatureFilter::All;
        assert!(filter.allows(LspFeature::Hover));
        assert!(filter.allows(LspFeature::Diagnostics));
        assert!(filter.allows(LspFeature::Completion));
        assert!(filter.allows(LspFeature::Rename));
    }

    #[test]
    fn test_feature_filter_only() {
        let mut set = HashSet::new();
        set.insert(LspFeature::Diagnostics);
        set.insert(LspFeature::Completion);
        let filter = FeatureFilter::Only(set);

        assert!(filter.allows(LspFeature::Diagnostics));
        assert!(filter.allows(LspFeature::Completion));
        assert!(!filter.allows(LspFeature::Hover));
        assert!(!filter.allows(LspFeature::Definition));
    }

    #[test]
    fn test_feature_filter_except() {
        let mut set = HashSet::new();
        set.insert(LspFeature::Format);
        set.insert(LspFeature::Rename);
        let filter = FeatureFilter::Except(set);

        assert!(filter.allows(LspFeature::Hover));
        assert!(filter.allows(LspFeature::Diagnostics));
        assert!(!filter.allows(LspFeature::Format));
        assert!(!filter.allows(LspFeature::Rename));
    }

    #[test]
    fn test_feature_filter_from_config_none() {
        let filter = FeatureFilter::from_config(&None, &None);
        assert!(matches!(filter, FeatureFilter::All));
    }

    #[test]
    fn test_feature_filter_from_config_only() {
        let only = Some(vec![LspFeature::Diagnostics, LspFeature::Completion]);
        let filter = FeatureFilter::from_config(&only, &None);
        assert!(filter.allows(LspFeature::Diagnostics));
        assert!(filter.allows(LspFeature::Completion));
        assert!(!filter.allows(LspFeature::Hover));
    }

    #[test]
    fn test_feature_filter_from_config_except() {
        let except = Some(vec![LspFeature::Format]);
        let filter = FeatureFilter::from_config(&None, &except);
        assert!(filter.allows(LspFeature::Hover));
        assert!(!filter.allows(LspFeature::Format));
    }

    #[test]
    fn test_feature_filter_default() {
        let filter = FeatureFilter::default();
        assert!(matches!(filter, FeatureFilter::All));
    }
}
