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
    /// True when the active buffer is a real, user-visible buffer.
    /// False when it's the synthesized placeholder kept alive by the
    /// close path with `auto_create_empty_buffer_on_last_buffer_close`
    /// disabled. Buffer-specific menu items gate on this so they don't
    /// pretend to operate on a non-existent buffer.
    pub const HAS_BUFFER: &str = "has_buffer";
    /// True when the active buffer is a real buffer *and* holds editable text
    /// — i.e. not a terminal. A terminal buffer satisfies `HAS_BUFFER`, so
    /// items gated on that alone were offered for it: File → Save wrote the
    /// terminal's scrollback transcript back over its own backing file and
    /// reported "Saved", and could raise a "File changed on disk" conflict
    /// for a file only the terminal had ever written. Items that save, revert
    /// or edit the active buffer's text gate on this instead. Reading a
    /// terminal (select, copy, search its scrollback) still gates on
    /// `HAS_BUFFER`.
    pub const HAS_TEXT_BUFFER: &str = "has_text_buffer";
    /// True when Save has something to write: a text buffer with unsaved
    /// changes. `HAS_BUFFER` alone offered Save for an untouched buffer, where
    /// it rewrites the same bytes and reports success.
    pub const CAN_SAVE: &str = "can_save";
    /// True when any buffer in the window has unsaved changes — what Save All
    /// acts on.
    pub const CAN_SAVE_ALL: &str = "can_save_all";
    /// True when the active buffer has a file behind it to re-read, which
    /// Revert and "reload with encoding" need and a never-saved scratch buffer
    /// lacks.
    pub const CAN_REVERT: &str = "can_revert";
    /// True when the active buffer accepts edits — the same `editing_disabled`
    /// flag the handlers check, so read-only buffers and plugin panels don't
    /// advertise Undo, Delete Line or Replace.
    pub const CAN_EDIT: &str = "can_edit";
    pub const LINE_NUMBERS: &str = "line_numbers";
    pub const LINE_WRAP: &str = "line_wrap";
    pub const PAGE_VIEW: &str = "page_view";
    /// Backward-compatible alias for PAGE_VIEW
    pub const COMPOSE_MODE: &str = "compose_mode";
    pub const FILE_EXPLORER: &str = "file_explorer";
    /// True while a plugin dock panel (`PanelSlot::Dock` — the left
    /// companion column, e.g. the Orchestrator's) is mounted, whether it
    /// holds the keyboard or is merely visible. Lets a plugin's menu row
    /// carry a checkmark that tracks its own panel without the core
    /// knowing which plugin owns the dock.
    pub const DOCK: &str = "dock";
    pub const MENU_BAR: &str = "menu_bar";
    pub const FILE_EXPLORER_FOCUSED: &str = "file_explorer_focused";
    pub const MOUSE_CAPTURE: &str = "mouse_capture";
    pub const MOUSE_HOVER: &str = "mouse_hover";
    pub const LSP_AVAILABLE: &str = "lsp_available";
    pub const FILE_EXPLORER_SHOW_HIDDEN: &str = "file_explorer_show_hidden";
    pub const FILE_EXPLORER_SHOW_GITIGNORED: &str = "file_explorer_show_gitignored";
    pub const HAS_SELECTION: &str = "has_selection";
    pub const CAN_COPY: &str = "can_copy";
    pub const CAN_PASTE: &str = "can_paste";
    /// Like [`CAN_COPY`], but for the destructive half: in the editor it also
    /// requires a buffer whose text is the user's to remove, so Cut is not
    /// offered for a terminal's scrollback.
    pub const CAN_CUT: &str = "can_cut";
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

/// Resource limits for a process.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, JsonSchema)]
pub struct ProcessLimits {
    /// Max memory as a percent of system memory (default: 50). `null` means no limit.
    #[serde(default)]
    pub max_memory_percent: Option<u32>,

    /// Max CPU as a percent, where 100 = one core, 200 = two cores. `null` means no limit.
    #[serde(default)]
    pub max_cpu_percent: Option<u32>,

    /// Apply these limits. Default: true (the built-in config turns them on
    /// only on Linux).
    #[serde(default = "default_true")]
    pub enabled: bool,
}

fn default_true() -> bool {
    true
}

/// Schema-stable default for nested `process_limits` objects.
/// `ProcessLimits::default()` is platform-specific; JSON Schema must not vary by OS.
fn process_limits_schema_default() -> ProcessLimits {
    ProcessLimits {
        max_memory_percent: Some(50),
        max_cpu_percent: Some(90),
        enabled: true,
    }
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

/// Language server feature, for routing features to servers when a language has
/// several. "Merged" features combine results from all servers; "exclusive" ones use the first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum LspFeature {
    /// Diagnostics (merged)
    Diagnostics,
    /// Code completion (merged)
    Completion,
    /// Code actions / quick fixes (merged)
    CodeAction,
    /// Document symbols (merged)
    DocumentSymbols,
    /// Workspace symbols (merged)
    WorkspaceSymbols,
    /// Hover information (exclusive)
    Hover,
    /// Go to definition, declaration, type definition (exclusive)
    Definition,
    /// Go to implementation (exclusive)
    Implementation,
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
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum FeatureFilter {
    #[default]
    All,
    Only(HashSet<LspFeature>),
    Except(HashSet<LspFeature>),
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
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LspLanguageConfig {
    /// Multiple servers for this language (array form)
    Multi(Vec<LspServerConfig>),
    /// A single server for this language (object form)
    Single(Box<LspServerConfig>),
}

/// Custom JsonSchema: always advertise the canonical array form so the settings
/// UI renders a structured array editor. The `#[serde(untagged)]` on the enum
/// still accepts both single-object and array forms during deserialization.
impl JsonSchema for LspLanguageConfig {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("LspLanguageConfig")
    }

    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "description": "One or more LSP server configs for this language.\nAccepts both a single object and an array for backwards compatibility.",
            "type": "array",
            "items": generator.subschema_for::<LspServerConfig>()
        })
    }
}

impl LspLanguageConfig {
    /// Convert to a Vec of server configs.
    pub fn into_vec(self) -> Vec<LspServerConfig> {
        match self {
            LspLanguageConfig::Single(c) => vec![*c],
            LspLanguageConfig::Multi(v) => v,
        }
    }

    /// Get a reference as a slice of server configs.
    pub fn as_slice(&self) -> &[LspServerConfig] {
        match self {
            LspLanguageConfig::Single(c) => std::slice::from_ref(c.as_ref()),
            LspLanguageConfig::Multi(v) => v,
        }
    }

    /// Get a mutable reference as a slice of server configs.
    pub fn as_mut_slice(&mut self) -> &mut [LspServerConfig] {
        match self {
            LspLanguageConfig::Single(c) => std::slice::from_mut(c),
            LspLanguageConfig::Multi(v) => v,
        }
    }
}

impl Default for LspLanguageConfig {
    fn default() -> Self {
        LspLanguageConfig::Single(Box::default())
    }
}

/// Language server settings.
#[derive(Debug, Clone, Default, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/command"))]
pub struct LspServerConfig {
    /// Command that starts the server. Required when enabled.
    #[serde(default)]
    #[schemars(extend("x-order" = 1))]
    pub command: String,

    /// Enable this server.
    #[serde(default = "default_true")]
    #[schemars(extend("x-order" = 2))]
    pub enabled: bool,

    /// Display name (e.g. "tsserver"). Defaults to the command's file name.
    #[serde(default)]
    #[schemars(extend("x-order" = 3))]
    pub name: Option<String>,

    /// Arguments for the server. If omitted, the default server's arguments are
    /// used; any list, even `[]`, replaces them.
    #[serde(default)]
    #[schemars(extend("x-order" = 4))]
    pub args: Option<Vec<String>>,

    /// Start the server when a matching file opens (default: true). When off,
    /// start it from the command palette.
    #[serde(default = "default_true")]
    #[schemars(extend("x-order" = 5))]
    pub auto_start: bool,

    /// Files or folders that mark the project root: the nearest folder above the
    /// file that contains one is used. Empty means `[".git"]`. With no match, the
    /// file's own folder is used.
    #[serde(default)]
    #[schemars(extend("x-order" = 6))]
    pub root_markers: Vec<String>,

    /// Extra environment variables for the server (override inherited ones).
    #[serde(default)]
    #[schemars(extend("x-section" = "Advanced", "x-order" = 10))]
    pub env: HashMap<String, String>,

    /// Language ID to send to the server per file extension (no dot),
    /// e.g. `{"tsx": "typescriptreact"}`.
    #[serde(default)]
    #[schemars(extend("x-section" = "Advanced", "x-order" = 11))]
    pub language_id_overrides: HashMap<String, String>,

    /// Server-specific `initializationOptions` sent at startup.
    #[serde(default)]
    #[schemars(extend("x-section" = "Advanced", "x-order" = 12))]
    pub initialization_options: Option<serde_json::Value>,

    /// Use this server only for these features. Don't combine with
    /// `except_features`; if neither is set, it handles everything.
    #[serde(default)]
    #[schemars(extend("x-section" = "Advanced", "x-order" = 13))]
    pub only_features: Option<Vec<LspFeature>>,

    /// Use this server for everything except these features. Don't combine with
    /// `only_features`.
    #[serde(default)]
    #[schemars(extend("x-section" = "Advanced", "x-order" = 14))]
    pub except_features: Option<Vec<LspFeature>>,

    /// Memory and CPU limits for the server.
    #[serde(default)]
    #[schemars(
        default = "process_limits_schema_default",
        extend("x-section" = "Advanced", "x-order" = 15)
    )]
    pub process_limits: ProcessLimits,
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

    /// The arguments to launch this server with, treating an omitted
    /// (`None`) list as no arguments. Use this at spawn time; use the
    /// `args` field directly only when the unset-vs-empty distinction
    /// matters (e.g. merging user config with defaults).
    pub fn args(&self) -> &[String] {
        self.args.as_deref().unwrap_or(&[])
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
            // Inherit the default args only when the user omitted the field
            // entirely (`None`). An explicit list — including an empty `[]` —
            // is respected as-is, so a replacement server can opt out of the
            // default's arguments.
            args: self.args.or_else(|| defaults.args.clone()),
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

    /// A default server (e.g. `marksman server`) that a user replaces with a
    /// command taking a different set of args.
    fn marksman_default() -> LspServerConfig {
        LspServerConfig {
            command: "marksman".to_string(),
            args: Some(vec!["server".to_string()]),
            ..Default::default()
        }
    }

    #[test]
    fn test_merge_omitted_args_inherits_default() {
        // User omits `args` entirely (deserializes to None): inherit the
        // default server's args.
        let user = LspServerConfig {
            command: "marksman".to_string(),
            args: None,
            ..Default::default()
        };
        let merged = user.merge_with_defaults(&marksman_default());
        assert_eq!(merged.args, Some(vec!["server".to_string()]));
        assert_eq!(merged.args(), ["server".to_string()]);
    }

    #[test]
    fn test_merge_explicit_empty_args_overrides_default() {
        // Regression for #2549: a user replacing marksman with a server that
        // takes no arguments sets `"args": []`. This must NOT fall back to the
        // default `["server"]` — the server should be launched with no args.
        let user = LspServerConfig {
            command: "markdown-oxide".to_string(),
            args: Some(vec![]),
            ..Default::default()
        };
        let merged = user.merge_with_defaults(&marksman_default());
        assert_eq!(merged.command, "markdown-oxide");
        assert_eq!(merged.args, Some(vec![]));
        assert!(merged.args().is_empty());
    }

    #[test]
    fn test_merge_explicit_args_replaces_default() {
        let user = LspServerConfig {
            command: "markdown-oxide".to_string(),
            args: Some(vec!["--stdio".to_string()]),
            ..Default::default()
        };
        let merged = user.merge_with_defaults(&marksman_default());
        assert_eq!(merged.args, Some(vec!["--stdio".to_string()]));
    }

    #[test]
    fn test_args_deserializes_missing_as_none_and_empty_as_some() {
        // The unset-vs-empty distinction hinges on serde: an omitted field is
        // None, while an explicit empty array is Some([]).
        let omitted: LspServerConfig =
            serde_json::from_str(r#"{"command":"markdown-oxide"}"#).unwrap();
        assert_eq!(omitted.args, None);

        let empty: LspServerConfig =
            serde_json::from_str(r#"{"command":"markdown-oxide","args":[]}"#).unwrap();
        assert_eq!(empty.args, Some(vec![]));
    }
}
