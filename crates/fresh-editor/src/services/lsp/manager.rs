//! LSP Manager - manages multiple language servers using async I/O
//!
//! This module provides a manager for multiple LSP servers that:
//! - Spawns one server per language
//! - Uses async LspHandle for non-blocking I/O
//! - Routes requests to appropriate servers
//! - Configured via config.json

use crate::services::async_bridge::AsyncBridge;
use crate::services::lsp::async_handler::LspHandle;
use crate::types::{FeatureFilter, LspFeature, LspServerConfig};
use lsp_types::{SemanticTokensLegend, Uri};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::time::{Duration, Instant};

/// Result of attempting to spawn an LSP server
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LspSpawnResult {
    /// Server was spawned or already running
    Spawned,
    /// Server is not configured for auto-start
    /// The server can still be started manually via command palette
    NotAutoStart,
    /// No LSP server is configured for this language
    NotConfigured,
    /// Server spawn failed or is disabled
    Failed,
}

/// Constants for restart behavior
const MAX_RESTARTS_IN_WINDOW: usize = 5;
const RESTART_WINDOW_SECS: u64 = 180; // 3 minutes
const RESTART_BACKOFF_BASE_MS: u64 = 1000; // 1s, 2s, 4s, 8s...

/// Convert a directory path to an LSP `file://` URI without the `url` crate.
fn path_to_uri(path: &Path) -> Option<Uri> {
    let abs = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir().ok()?.join(path)
    };
    // Percent-encode each path component for RFC 3986 compliance
    let encoded: String = abs
        .components()
        .filter_map(|c| match c {
            std::path::Component::RootDir => None, // handled by leading '/' in Normal
            std::path::Component::Normal(s) => {
                let s = s.to_str()?;
                let mut out = String::with_capacity(s.len() + 1);
                out.push('/');
                for b in s.bytes() {
                    if b.is_ascii_alphanumeric()
                        || matches!(
                            b,
                            b'-' | b'.'
                                | b'_'
                                | b'~'
                                | b'@'
                                | b'!'
                                | b'$'
                                | b'&'
                                | b'\''
                                | b'('
                                | b')'
                                | b'+'
                                | b','
                                | b';'
                                | b'='
                        )
                    {
                        out.push(b as char);
                    } else {
                        out.push_str(&format!("%{:02X}", b));
                    }
                }
                Some(out)
            }
            _ => None,
        })
        .collect();
    format!("file://{}", encoded).parse().ok()
}

/// Detect workspace root by walking upward from a file looking for marker files/directories.
///
/// Returns the first directory containing any of the markers, or the file's parent
/// directory if no marker is found.
pub fn detect_workspace_root(file_path: &Path, root_markers: &[String]) -> std::path::PathBuf {
    let file_dir = file_path.parent().unwrap_or(file_path).to_path_buf();

    if root_markers.is_empty() {
        return file_dir;
    }

    let mut dir = Some(file_dir.as_path());
    while let Some(d) = dir {
        for marker in root_markers {
            if d.join(marker).exists() {
                return d.to_path_buf();
            }
        }
        dir = d.parent();
    }

    file_dir
}

/// Summary of capabilities reported by an LSP server during initialization.
///
/// This is extracted from `ServerCapabilities` in the `initialize` response
/// and stored per-server so that requests are only sent to servers that
/// actually support them. Follows the LSP 3.17 specification.
///
#[derive(Debug, Clone, Default)]
pub struct ServerCapabilitySummary {
    /// Whether capabilities have been received from the server.
    /// When false, `has_capability()` defers to the handle's readiness state.
    pub initialized: bool,
    pub hover: bool,
    pub completion: bool,
    pub completion_resolve: bool,
    pub completion_trigger_characters: Vec<String>,
    pub definition: bool,
    pub references: bool,
    pub document_formatting: bool,
    pub document_range_formatting: bool,
    pub rename: bool,
    pub signature_help: bool,
    pub inlay_hints: bool,
    pub folding_ranges: bool,
    pub semantic_tokens_full: bool,
    pub semantic_tokens_full_delta: bool,
    pub semantic_tokens_range: bool,
    pub semantic_tokens_legend: Option<SemanticTokensLegend>,
    pub document_highlight: bool,
    pub code_action: bool,
    pub code_action_resolve: bool,
    pub document_symbols: bool,
    pub workspace_symbols: bool,
    pub diagnostics: bool,
}

/// A named LSP handle with feature filter metadata and per-server capabilities.
/// Wraps an LspHandle with the server's display name, feature routing filter,
/// and the capabilities reported by this specific server during initialization.
pub struct ServerHandle {
    /// Display name for this server (e.g., "rust-analyzer", "eslint")
    pub name: String,
    /// The underlying LSP handle
    pub handle: LspHandle,
    /// Feature filter controlling which LSP features this server handles
    pub feature_filter: FeatureFilter,
    /// Capabilities reported by this server during initialization.
    pub capabilities: ServerCapabilitySummary,
}

impl ServerHandle {
    /// Check if this server has the actual capability for a feature.
    ///
    /// Checks the server's reported capabilities (from the `initialize` response).
    /// Before initialization completes (capabilities not yet received), returns
    /// `false` — the main loop must not route feature requests to servers whose
    /// capabilities are unknown. Callers handle `None` from `handle_for_feature_mut`
    /// by relying on existing retry mechanisms (render-cycle polling, timer retries,
    /// or explicit re-requests from the `LspInitialized` handler).
    pub fn has_capability(&self, feature: LspFeature) -> bool {
        if !self.capabilities.initialized {
            return false;
        }
        match feature {
            LspFeature::Hover => self.capabilities.hover,
            LspFeature::Completion => self.capabilities.completion,
            LspFeature::Definition => self.capabilities.definition,
            LspFeature::References => self.capabilities.references,
            LspFeature::Format => {
                self.capabilities.document_formatting || self.capabilities.document_range_formatting
            }
            LspFeature::Rename => self.capabilities.rename,
            LspFeature::SignatureHelp => self.capabilities.signature_help,
            LspFeature::InlayHints => self.capabilities.inlay_hints,
            LspFeature::FoldingRange => self.capabilities.folding_ranges,
            LspFeature::SemanticTokens => {
                self.capabilities.semantic_tokens_full || self.capabilities.semantic_tokens_range
            }
            LspFeature::DocumentHighlight => self.capabilities.document_highlight,
            LspFeature::CodeAction => self.capabilities.code_action,
            LspFeature::DocumentSymbols => self.capabilities.document_symbols,
            LspFeature::WorkspaceSymbols => self.capabilities.workspace_symbols,
            LspFeature::Diagnostics => self.capabilities.diagnostics,
        }
    }
}

/// Manager for multiple language servers (async version)
pub struct LspManager {
    /// Map from language ID to LSP handles (supports multiple servers per language)
    handles: HashMap<String, Vec<ServerHandle>>,

    /// Configuration for each language (supports multiple servers per language)
    config: HashMap<String, Vec<LspServerConfig>>,

    /// Default root URI for workspace (used if no per-language root is set)
    root_uri: Option<Uri>,

    /// Per-language root URIs (allows plugins to specify project roots)
    per_language_root_uris: HashMap<String, Uri>,

    /// Tokio runtime reference
    runtime: Option<tokio::runtime::Handle>,

    /// Async bridge for communication
    async_bridge: Option<AsyncBridge>,

    /// Restart attempt timestamps per language (for tracking restart frequency)
    restart_attempts: HashMap<String, Vec<Instant>>,

    /// Languages currently in restart cooldown (gave up after too many restarts)
    restart_cooldown: HashSet<String>,

    /// Scheduled restart times (language -> when to restart)
    pending_restarts: HashMap<String, Instant>,

    /// Languages that have been manually started by the user
    /// If a language is in this set, it will spawn even if auto_start=false in config
    allowed_languages: HashSet<String>,

    /// Languages that have been explicitly disabled/stopped by the user
    /// These will not auto-restart until user manually restarts them
    disabled_languages: HashSet<String>,
}

impl LspManager {
    /// Create a new LSP manager
    pub fn new(root_uri: Option<Uri>) -> Self {
        Self {
            handles: HashMap::new(),
            config: HashMap::new(),
            root_uri,
            per_language_root_uris: HashMap::new(),
            runtime: None,
            async_bridge: None,
            restart_attempts: HashMap::new(),
            restart_cooldown: HashSet::new(),
            pending_restarts: HashMap::new(),
            allowed_languages: HashSet::new(),
            disabled_languages: HashSet::new(),
        }
    }

    /// Check if a language has been manually enabled (allowing spawn even if auto_start=false)
    pub fn is_language_allowed(&self, language: &str) -> bool {
        self.allowed_languages.contains(language)
    }

    /// Allow a language to spawn LSP server (used by manual start command)
    pub fn allow_language(&mut self, language: &str) {
        self.allowed_languages.insert(language.to_string());
        tracing::info!("LSP language '{}' manually enabled", language);
    }

    /// Get the set of manually enabled languages
    pub fn allowed_languages(&self) -> &HashSet<String> {
        &self.allowed_languages
    }

    /// Get the configurations for a specific language (one or more servers).
    pub fn get_configs(&self, language: &str) -> Option<&[LspServerConfig]> {
        self.config.get(language).map(|v| v.as_slice())
    }

    /// Get the primary (first) configuration for a specific language.
    pub fn get_config(&self, language: &str) -> Option<&LspServerConfig> {
        self.config.get(language).and_then(|v| v.first())
    }

    /// Store capabilities on the specific server handle identified by server_name.
    pub fn set_server_capabilities(
        &mut self,
        language: &str,
        server_name: &str,
        mut capabilities: ServerCapabilitySummary,
    ) {
        capabilities.initialized = true;
        if let Some(handles) = self.handles.get_mut(language) {
            if let Some(sh) = handles.iter_mut().find(|sh| sh.name == server_name) {
                sh.capabilities = capabilities;
            }
        }
    }

    /// Get the semantic token legend for a language from the first eligible server.
    pub fn semantic_tokens_legend(&self, language: &str) -> Option<&SemanticTokensLegend> {
        self.handles.get(language)?.iter().find_map(|sh| {
            if sh.feature_filter.allows(LspFeature::SemanticTokens)
                && sh.has_capability(LspFeature::SemanticTokens)
            {
                sh.capabilities.semantic_tokens_legend.as_ref()
            } else {
                None
            }
        })
    }

    /// Check if any eligible server for the language supports full semantic tokens.
    pub fn semantic_tokens_full_supported(&self, language: &str) -> bool {
        self.handles.get(language).is_some_and(|handles| {
            handles.iter().any(|sh| {
                sh.feature_filter.allows(LspFeature::SemanticTokens)
                    && sh.capabilities.semantic_tokens_full
            })
        })
    }

    /// Check if any eligible server for the language supports full semantic token deltas.
    pub fn semantic_tokens_full_delta_supported(&self, language: &str) -> bool {
        self.handles.get(language).is_some_and(|handles| {
            handles.iter().any(|sh| {
                sh.feature_filter.allows(LspFeature::SemanticTokens)
                    && sh.capabilities.semantic_tokens_full_delta
            })
        })
    }

    /// Check if any eligible server for the language supports range semantic tokens.
    pub fn semantic_tokens_range_supported(&self, language: &str) -> bool {
        self.handles.get(language).is_some_and(|handles| {
            handles.iter().any(|sh| {
                sh.feature_filter.allows(LspFeature::SemanticTokens)
                    && sh.capabilities.semantic_tokens_range
            })
        })
    }

    /// Check if any eligible server for the language supports folding ranges.
    pub fn folding_ranges_supported(&self, language: &str) -> bool {
        self.handles.get(language).is_some_and(|handles| {
            handles.iter().any(|sh| {
                sh.feature_filter.allows(LspFeature::FoldingRange) && sh.capabilities.folding_ranges
            })
        })
    }

    /// Check if a character is a completion trigger for any running language server.
    pub fn is_completion_trigger_char(&self, ch: char, language: &str) -> bool {
        let ch_str = ch.to_string();
        self.handles.get(language).is_some_and(|handles| {
            handles.iter().any(|sh| {
                sh.feature_filter.allows(LspFeature::Completion)
                    && sh
                        .capabilities
                        .completion_trigger_characters
                        .contains(&ch_str)
            })
        })
    }

    /// Try to spawn an LSP server, checking auto_start configuration
    ///
    /// This is the main entry point for spawning LSP servers on file open.
    /// It returns:
    /// - `LspSpawnResult::Spawned` if the server was spawned or already running
    /// - `LspSpawnResult::NotAutoStart` if auto_start is false and not manually allowed
    /// - `LspSpawnResult::NotConfigured` if no LSP server is configured for the language
    /// - `LspSpawnResult::Failed` if spawn failed or language is disabled
    ///
    /// The `file_path` is used for workspace root detection via `root_markers`.
    ///
    /// IMPORTANT: Callers should only call this when there is at least one buffer
    /// with a matching language. Do not call for languages with no open files.
    pub fn try_spawn(&mut self, language: &str, file_path: Option<&Path>) -> LspSpawnResult {
        // If handles already exist for this language, return success
        if self.handles.get(language).is_some_and(|v| !v.is_empty()) {
            return LspSpawnResult::Spawned;
        }

        // Check if language is configured
        let configs = match self.config.get(language) {
            Some(configs) if !configs.is_empty() => configs,
            _ => return LspSpawnResult::NotConfigured,
        };

        // Check if any config is enabled
        if !configs.iter().any(|c| c.enabled) {
            return LspSpawnResult::Failed;
        }

        // Check if we have runtime and bridge
        if self.runtime.is_none() || self.async_bridge.is_none() {
            return LspSpawnResult::Failed;
        }

        // Check if auto_start is enabled (on any config) or language was manually allowed
        let any_auto_start = configs.iter().any(|c| c.auto_start && c.enabled);
        if !any_auto_start && !self.allowed_languages.contains(language) {
            return LspSpawnResult::NotAutoStart;
        }

        // Spawn all enabled servers for this language
        if self.force_spawn(language, file_path).is_some() {
            LspSpawnResult::Spawned
        } else {
            LspSpawnResult::Failed
        }
    }

    /// Set the Tokio runtime and async bridge
    ///
    /// Must be called before spawning any servers
    pub fn set_runtime(&mut self, runtime: tokio::runtime::Handle, async_bridge: AsyncBridge) {
        self.runtime = Some(runtime);
        self.async_bridge = Some(async_bridge);
    }

    /// Set configuration for a language (single server).
    pub fn set_language_config(&mut self, language: String, config: LspServerConfig) {
        self.config.insert(language, vec![config]);
    }

    /// Set configurations for a language (one or more servers).
    pub fn set_language_configs(&mut self, language: String, configs: Vec<LspServerConfig>) {
        self.config.insert(language, configs);
    }

    /// Append additional server configs to an existing language entry.
    pub fn append_language_configs(&mut self, language: String, configs: Vec<LspServerConfig>) {
        self.config.entry(language).or_default().extend(configs);
    }

    /// Return the list of currently configured language keys.
    pub fn configured_languages(&self) -> Vec<String> {
        self.config.keys().cloned().collect()
    }

    /// Set a new root URI for the workspace
    ///
    /// This should be called after shutting down all servers when switching projects.
    /// Servers spawned after this will use the new root URI.
    pub fn set_root_uri(&mut self, root_uri: Option<Uri>) {
        self.root_uri = root_uri;
    }

    /// Set a language-specific root URI
    ///
    /// This allows plugins to specify project roots for specific languages.
    /// For example, a C# plugin can set the root to the directory containing .csproj.
    /// Returns true if an existing server was restarted with the new root.
    pub fn set_language_root_uri(&mut self, language: &str, uri: Uri) -> bool {
        tracing::info!("Setting root URI for {}: {}", language, uri.as_str());
        self.per_language_root_uris
            .insert(language.to_string(), uri.clone());

        // If there's an existing server for this language, restart it with the new root
        if self.handles.contains_key(language) {
            tracing::info!(
                "Restarting {} LSP server with new root: {}",
                language,
                uri.as_str()
            );
            self.shutdown_server(language);
            // The server will be respawned on next request with the new root
            return true;
        }
        false
    }

    /// Resolve the root URI for a language, using root_markers for detection.
    ///
    /// Priority:
    /// 1. Plugin-set per-language root (per_language_root_uris)
    /// 2. Walk upward from file_path using config's root_markers
    /// 3. File's parent directory
    pub fn resolve_root_uri(&self, language: &str, file_path: Option<&Path>) -> Option<Uri> {
        // 1. Plugin-set root takes priority
        if let Some(uri) = self.per_language_root_uris.get(language) {
            return Some(uri.clone());
        }

        // 2. Use root_markers to detect workspace root from file path
        if let Some(path) = file_path {
            let markers = self
                .config
                .get(language)
                .and_then(|configs| configs.first())
                .map(|c| c.root_markers.as_slice())
                .unwrap_or(&[]);
            let root = detect_workspace_root(path, markers);
            if let Some(uri) = path_to_uri(&root) {
                return Some(uri);
            }
        }

        // 3. No file path available — use the global root_uri
        self.root_uri.clone()
    }

    /// Get the effective root URI for a language (legacy, without file-based detection)
    ///
    /// Returns the language-specific root if set, otherwise the default root.
    pub fn get_effective_root_uri(&self, language: &str) -> Option<Uri> {
        self.resolve_root_uri(language, None)
    }

    /// Reset the manager for a new project
    ///
    /// This shuts down all servers and clears state, preparing for a fresh start.
    /// The configuration is preserved but servers will need to be respawned.
    pub fn reset_for_new_project(&mut self, new_root_uri: Option<Uri>) {
        // Shutdown all servers
        self.shutdown_all();

        // Update root URI
        self.root_uri = new_root_uri;

        // Clear restart tracking state (fresh start)
        self.restart_attempts.clear();
        self.restart_cooldown.clear();
        self.pending_restarts.clear();

        // Keep allowed_languages and disabled_languages as user preferences
        // Keep config as it's not project-specific

        tracing::info!(
            "LSP manager reset for new project: {:?}",
            self.root_uri.as_ref().map(|u| u.as_str())
        );
    }

    /// Get the primary (first) existing LSP handle for a language (no spawning).
    pub fn get_handle(&self, language: &str) -> Option<&LspHandle> {
        self.handles
            .get(language)
            .and_then(|v| v.first())
            .map(|sh| &sh.handle)
    }

    /// Get the primary (first) mutable existing LSP handle for a language (no spawning).
    pub fn get_handle_mut(&mut self, language: &str) -> Option<&mut LspHandle> {
        self.handles
            .get_mut(language)
            .and_then(|v| v.first_mut())
            .map(|sh| &mut sh.handle)
    }

    /// Get all handles for a language (for broadcasting operations like didOpen/didChange).
    pub fn get_handles(&self, language: &str) -> &[ServerHandle] {
        self.handles
            .get(language)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Check if a server with the given name exists (across all languages).
    pub fn has_server_named(&self, server_name: &str) -> bool {
        self.handles
            .values()
            .any(|handles| handles.iter().any(|sh| sh.name == server_name))
    }

    /// Get all mutable handles for a language.
    pub fn get_handles_mut(&mut self, language: &str) -> &mut [ServerHandle] {
        self.handles
            .get_mut(language)
            .map(|v| v.as_mut_slice())
            .unwrap_or(&mut [])
    }

    /// Get the first handle for a language that allows a given feature (for exclusive features).
    /// For capability-gated features (semantic tokens, folding ranges), this also checks
    /// that the server actually reported the capability during initialization.
    /// Returns `None` if no handle matches.
    pub fn handle_for_feature(&self, language: &str, feature: LspFeature) -> Option<&ServerHandle> {
        self.handles
            .get(language)?
            .iter()
            .find(|sh| sh.feature_filter.allows(feature) && sh.has_capability(feature))
    }

    /// Get the first mutable handle for a language that allows a given feature.
    /// For capability-gated features, this also checks the server's actual capabilities.
    pub fn handle_for_feature_mut(
        &mut self,
        language: &str,
        feature: LspFeature,
    ) -> Option<&mut ServerHandle> {
        self.handles
            .get_mut(language)?
            .iter_mut()
            .find(|sh| sh.feature_filter.allows(feature) && sh.has_capability(feature))
    }

    /// Get all handles for a language that allow a given feature (for merged features).
    /// Like `handle_for_feature`, also checks per-server capabilities.
    pub fn handles_for_feature(&self, language: &str, feature: LspFeature) -> Vec<&ServerHandle> {
        self.handles
            .get(language)
            .map(|v| {
                v.iter()
                    .filter(|sh| sh.feature_filter.allows(feature) && sh.has_capability(feature))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get all mutable handles for a language that allow a given feature.
    /// Like `handle_for_feature_mut`, also checks per-server capabilities.
    pub fn handles_for_feature_mut(
        &mut self,
        language: &str,
        feature: LspFeature,
    ) -> Vec<&mut ServerHandle> {
        self.handles
            .get_mut(language)
            .map(|v| {
                v.iter_mut()
                    .filter(|sh| sh.feature_filter.allows(feature) && sh.has_capability(feature))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Force spawn LSP server(s) for a language.
    ///
    /// Spawns servers configured for the language, filtered as follows:
    /// - If the language is in `allowed_languages` (the user explicitly
    ///   started or approved this language via a manual command), spawns
    ///   every configured server regardless of its `enabled` / `auto_start`
    ///   flags. This is the "manual" path used by the command palette's
    ///   Start / Restart LSP commands and the LSP confirmation popup.
    /// - Otherwise (the auto-start path, reached via `try_spawn` on buffer
    ///   load or by crash recovery), spawns only servers that have both
    ///   `enabled=true` AND `auto_start=true`. Each config's own
    ///   `auto_start` flag is honoured individually, so configuring one
    ///   auto-start server alongside an opt-in manual server no longer
    ///   drags the manual one along for the ride.
    ///
    /// Returns a mutable reference to the primary (first) handle if any
    /// were spawned. The `file_path` is used for workspace root detection
    /// via `root_markers`.
    pub fn force_spawn(
        &mut self,
        language: &str,
        file_path: Option<&Path>,
    ) -> Option<&mut LspHandle> {
        tracing::debug!("force_spawn called for language: {}", language);

        // Return existing handle if available
        if self.handles.get(language).is_some_and(|v| !v.is_empty()) {
            tracing::debug!("force_spawn: returning existing handle for {}", language);
            return self
                .handles
                .get_mut(language)
                .and_then(|v| v.first_mut())
                .map(|sh| &mut sh.handle);
        }

        // Check if language was explicitly disabled by user (via stop command)
        if self.disabled_languages.contains(language) {
            tracing::debug!(
                "LSP for {} is disabled, not spawning (use manual restart to re-enable)",
                language
            );
            return None;
        }

        // Get configs for this language
        let configs = match self.config.get(language) {
            Some(configs) if !configs.is_empty() => configs.clone(),
            _ => {
                tracing::warn!(
                    "force_spawn: no config found for language '{}', available configs: {:?}",
                    language,
                    self.config.keys().collect::<Vec<_>>()
                );
                return None;
            }
        };

        // Check we have runtime and bridge
        let runtime = match self.runtime.as_ref() {
            Some(r) => r.clone(),
            None => {
                tracing::error!("force_spawn: no tokio runtime available for {}", language);
                return None;
            }
        };
        let async_bridge = match self.async_bridge.as_ref() {
            Some(b) => b.clone(),
            None => {
                tracing::error!("force_spawn: no async bridge available for {}", language);
                return None;
            }
        };

        let mut spawned_handles = Vec::new();
        let manually_allowed = self.allowed_languages.contains(language);

        for config in &configs {
            if manually_allowed {
                // User explicitly started this language via command palette:
                // spawn every configured server, even if individually
                // disabled or marked not-auto-start.
            } else {
                // Auto-start path: only spawn servers that the user has
                // opted into both via `enabled=true` AND `auto_start=true`.
                // This honours each config's flags independently so that
                // e.g. configuring rust-auto (auto_start=true) alongside
                // rust-manual (auto_start=false) does not spawn both.
                if !config.enabled || !config.auto_start {
                    continue;
                }
            }

            if config.command.is_empty() {
                tracing::warn!(
                    "force_spawn: LSP command is empty for {} server '{}'",
                    language,
                    config.display_name()
                );
                continue;
            }

            let server_name = config.display_name();
            tracing::info!(
                "Spawning LSP server '{}' for language: {}",
                server_name,
                language
            );

            match LspHandle::spawn(
                &runtime,
                &config.command,
                &config.args,
                config.env.clone(),
                language.to_string(),
                server_name.clone(),
                &async_bridge,
                config.process_limits.clone(),
                config.language_id_overrides.clone(),
            ) {
                Ok(handle) => {
                    let effective_root = self.resolve_root_uri(language, file_path);
                    if let Err(e) =
                        handle.initialize(effective_root, config.initialization_options.clone())
                    {
                        tracing::error!(
                            "Failed to send initialize command for {} ({}): {}",
                            language,
                            server_name,
                            e
                        );
                        continue;
                    }

                    tracing::info!(
                        "LSP initialization started for {} ({}), will be ready asynchronously",
                        language,
                        server_name
                    );

                    spawned_handles.push(ServerHandle {
                        name: server_name,
                        handle,
                        feature_filter: config.feature_filter(),
                        capabilities: ServerCapabilitySummary::default(),
                    });
                }
                Err(e) => {
                    tracing::error!(
                        "Failed to spawn LSP handle for {} ({}): {}",
                        language,
                        server_name,
                        e
                    );
                }
            }
        }

        if spawned_handles.is_empty() {
            return None;
        }

        self.handles.insert(language.to_string(), spawned_handles);
        self.handles
            .get_mut(language)
            .and_then(|v| v.first_mut())
            .map(|sh| &mut sh.handle)
    }

    /// Handle a server crash by scheduling a restart with exponential backoff
    ///
    /// Returns a message describing the action taken (for UI notification)
    #[allow(clippy::let_underscore_must_use)] // shutdown() is best-effort cleanup of a crashed server
    pub fn handle_server_crash(&mut self, language: &str) -> String {
        // Remove all handles for this language
        if let Some(handles) = self.handles.remove(language) {
            for sh in handles {
                let _ = sh.handle.shutdown(); // Best-effort cleanup
            }
        }

        // Check if server was explicitly disabled by user (via stop command)
        // Don't auto-restart disabled servers
        if self.disabled_languages.contains(language) {
            return format!(
                "LSP server for {} stopped. Use 'Restart LSP Server' command to start it again.",
                language
            );
        }

        // Check if we're in cooldown
        if self.restart_cooldown.contains(language) {
            return format!(
                "LSP server for {} crashed. Too many restarts - use 'Restart LSP Server' command to retry.",
                language
            );
        }

        // Clean up old restart attempts outside the window
        let now = Instant::now();
        let window = Duration::from_secs(RESTART_WINDOW_SECS);
        let attempts = self
            .restart_attempts
            .entry(language.to_string())
            .or_default();
        attempts.retain(|t| now.duration_since(*t) < window);

        // Check if we've exceeded max restarts
        if attempts.len() >= MAX_RESTARTS_IN_WINDOW {
            self.restart_cooldown.insert(language.to_string());
            tracing::warn!(
                "LSP server for {} has crashed {} times in {} minutes, entering cooldown",
                language,
                MAX_RESTARTS_IN_WINDOW,
                RESTART_WINDOW_SECS / 60
            );
            return format!(
                "LSP server for {} has crashed too many times ({} in {} min). Use 'Restart LSP Server' command to manually restart.",
                language,
                MAX_RESTARTS_IN_WINDOW,
                RESTART_WINDOW_SECS / 60
            );
        }

        // Calculate exponential backoff delay
        let attempt_number = attempts.len();
        let delay_ms = RESTART_BACKOFF_BASE_MS * (1 << attempt_number); // 1s, 2s, 4s, 8s
        let restart_time = now + Duration::from_millis(delay_ms);

        // Schedule the restart
        self.pending_restarts
            .insert(language.to_string(), restart_time);

        tracing::info!(
            "LSP server for {} crashed (attempt {}/{}), will restart in {}ms",
            language,
            attempt_number + 1,
            MAX_RESTARTS_IN_WINDOW,
            delay_ms
        );

        format!(
            "LSP server for {} crashed (attempt {}/{}), restarting in {}s...",
            language,
            attempt_number + 1,
            MAX_RESTARTS_IN_WINDOW,
            delay_ms / 1000
        )
    }

    /// Check and process any pending restarts that are due
    ///
    /// Returns list of (language, success, message) for each restart attempted
    pub fn process_pending_restarts(&mut self) -> Vec<(String, bool, String)> {
        let now = Instant::now();
        let mut results = Vec::new();

        // Find restarts that are due
        let due_restarts: Vec<String> = self
            .pending_restarts
            .iter()
            .filter(|(_, time)| **time <= now)
            .map(|(lang, _)| lang.clone())
            .collect();

        for language in due_restarts {
            self.pending_restarts.remove(&language);

            // Record this restart attempt
            self.restart_attempts
                .entry(language.clone())
                .or_default()
                .push(now);

            // Attempt to spawn the server (bypassing auto_start for crash recovery)
            if self.force_spawn(&language, None).is_some() {
                let message = format!("LSP server for {} restarted successfully", language);
                tracing::info!("{}", message);
                results.push((language, true, message));
            } else {
                let message = format!("Failed to restart LSP server for {}", language);
                tracing::error!("{}", message);
                results.push((language, false, message));
            }
        }

        results
    }

    /// Check if a language server is in restart cooldown
    pub fn is_in_cooldown(&self, language: &str) -> bool {
        self.restart_cooldown.contains(language)
    }

    /// Check if a language server has a pending restart
    pub fn has_pending_restart(&self, language: &str) -> bool {
        self.pending_restarts.contains_key(language)
    }

    /// Clear cooldown for a language and allow manual restart
    pub fn clear_cooldown(&mut self, language: &str) {
        self.restart_cooldown.remove(language);
        self.restart_attempts.remove(language);
        self.pending_restarts.remove(language);
        tracing::info!("Cleared restart cooldown for {}", language);
    }

    /// Manually restart/start a language server (bypasses cooldown and auto_start check)
    ///
    /// This is used both to restart a crashed server and to manually start a server
    /// that has auto_start=false in its configuration.
    ///
    /// Returns (success, message) tuple
    #[allow(clippy::let_underscore_must_use)] // shutdown() is best-effort cleanup before restart
    pub fn manual_restart(&mut self, language: &str, file_path: Option<&Path>) -> (bool, String) {
        // Clear any existing state
        self.clear_cooldown(language);

        // Re-enable the language (remove from disabled set)
        self.disabled_languages.remove(language);

        // Add to allowed languages so it stays active even if auto_start=false
        self.allowed_languages.insert(language.to_string());

        // Remove existing handles
        if let Some(handles) = self.handles.remove(language) {
            for sh in handles {
                let _ = sh.handle.shutdown();
            }
        }

        // Spawn new server (bypassing auto_start for user-initiated restart)
        if self.force_spawn(language, file_path).is_some() {
            let message = format!("LSP server for {} started", language);
            tracing::info!("{}", message);
            (true, message)
        } else {
            let message = format!("Failed to start LSP server for {}", language);
            tracing::error!("{}", message);
            (false, message)
        }
    }

    /// Restart a single server by name for a specific language.
    ///
    /// Shuts down just that server and re-spawns it from config.
    /// Returns (success, message) tuple.
    #[allow(clippy::let_underscore_must_use)]
    pub fn manual_restart_server(
        &mut self,
        language: &str,
        server_name: &str,
        file_path: Option<&Path>,
    ) -> (bool, String) {
        self.clear_cooldown(language);
        self.disabled_languages.remove(language);
        self.allowed_languages.insert(language.to_string());

        // Find and shut down just the named server
        if let Some(handles) = self.handles.get_mut(language) {
            if let Some(idx) = handles.iter().position(|sh| sh.name == server_name) {
                let sh = handles.remove(idx);
                let _ = sh.handle.shutdown();
            }
        }

        // Find the matching config and spawn just that server
        let config = self
            .config
            .get(language)
            .and_then(|configs| configs.iter().find(|c| c.display_name() == server_name))
            .cloned();

        let Some(config) = config else {
            let message = format!(
                "No config found for server '{}' ({})",
                server_name, language
            );
            tracing::error!("{}", message);
            return (false, message);
        };

        if config.command.is_empty() {
            let message = format!(
                "LSP command is empty for {} server '{}'",
                language, server_name
            );
            tracing::error!("{}", message);
            return (false, message);
        }

        let runtime = match self.runtime.as_ref() {
            Some(r) => r.clone(),
            None => return (false, "No tokio runtime available".to_string()),
        };
        let async_bridge = match self.async_bridge.as_ref() {
            Some(b) => b.clone(),
            None => return (false, "No async bridge available".to_string()),
        };

        match LspHandle::spawn(
            &runtime,
            &config.command,
            &config.args,
            config.env.clone(),
            language.to_string(),
            server_name.to_string(),
            &async_bridge,
            config.process_limits.clone(),
            config.language_id_overrides.clone(),
        ) {
            Ok(handle) => {
                let effective_root = self.resolve_root_uri(language, file_path);
                if let Err(e) =
                    handle.initialize(effective_root, config.initialization_options.clone())
                {
                    let message = format!(
                        "Failed to initialize LSP server '{}' for {}: {}",
                        server_name, language, e
                    );
                    tracing::error!("{}", message);
                    return (false, message);
                }

                let sh = ServerHandle {
                    name: server_name.to_string(),
                    handle,
                    feature_filter: config.feature_filter(),
                    capabilities: ServerCapabilitySummary::default(),
                };

                self.handles
                    .entry(language.to_string())
                    .or_default()
                    .push(sh);

                let message = format!("LSP server '{}' for {} started", server_name, language);
                tracing::info!("{}", message);
                (true, message)
            }
            Err(e) => {
                let message = format!(
                    "Failed to start LSP server '{}' for {}: {}",
                    server_name, language, e
                );
                tracing::error!("{}", message);
                (false, message)
            }
        }
    }

    /// Get the number of recent restart attempts for a language
    pub fn restart_attempt_count(&self, language: &str) -> usize {
        let now = Instant::now();
        let window = Duration::from_secs(RESTART_WINDOW_SECS);
        self.restart_attempts
            .get(language)
            .map(|attempts| {
                attempts
                    .iter()
                    .filter(|t| now.duration_since(**t) < window)
                    .count()
            })
            .unwrap_or(0)
    }

    /// Get a list of currently running LSP server languages
    pub fn running_servers(&self) -> Vec<String> {
        self.handles.keys().cloned().collect()
    }

    /// Get the names of all running servers for a given language
    pub fn server_names_for_language(&self, language: &str) -> Vec<String> {
        self.handles
            .get(language)
            .map(|handles| handles.iter().map(|sh| sh.name.clone()).collect())
            .unwrap_or_default()
    }

    /// Check if any LSP server for a language is running and ready to serve requests
    pub fn is_server_ready(&self, language: &str) -> bool {
        self.handles
            .get(language)
            .map(|handles| {
                handles
                    .iter()
                    .any(|sh| sh.handle.state().can_send_requests())
            })
            .unwrap_or(false)
    }

    /// Shutdown a single server by name for a specific language.
    ///
    /// Returns true if the server was found and shut down.
    /// If this was the last server for the language, marks the language as disabled.
    #[allow(clippy::let_underscore_must_use)]
    pub fn shutdown_server_by_name(&mut self, language: &str, server_name: &str) -> bool {
        let Some(handles) = self.handles.get_mut(language) else {
            tracing::warn!("No running LSP servers found for {}", language);
            return false;
        };

        let pos = handles.iter().position(|sh| sh.name == server_name);
        let Some(idx) = pos else {
            tracing::warn!(
                "No running LSP server named '{}' found for {}",
                server_name,
                language
            );
            return false;
        };

        let sh = handles.remove(idx);
        tracing::info!(
            "Shutting down LSP server '{}' for {} (disabled until manual restart)",
            sh.name,
            language
        );
        let _ = sh.handle.shutdown();

        if handles.is_empty() {
            // Last server for this language — clean up like shutdown_server
            self.handles.remove(language);
            self.disabled_languages.insert(language.to_string());
            self.pending_restarts.remove(language);
            self.restart_cooldown.remove(language);
            self.allowed_languages.remove(language);
        }

        true
    }

    /// Shutdown all servers for a specific language.
    ///
    /// This marks the language as disabled, preventing auto-restart until the user
    /// explicitly restarts it using the restart command.
    #[allow(clippy::let_underscore_must_use)]
    pub fn shutdown_server(&mut self, language: &str) -> bool {
        if let Some(handles) = self.handles.remove(language) {
            for sh in &handles {
                tracing::info!(
                    "Shutting down LSP server '{}' for {} (disabled until manual restart)",
                    sh.name,
                    language
                );
                let _ = sh.handle.shutdown();
            }
            self.disabled_languages.insert(language.to_string());
            self.pending_restarts.remove(language);
            self.restart_cooldown.remove(language);
            self.allowed_languages.remove(language);
            !handles.is_empty()
        } else {
            tracing::warn!("No running LSP server found for {}", language);
            false
        }
    }

    /// Shutdown all language servers
    #[allow(clippy::let_underscore_must_use)]
    pub fn shutdown_all(&mut self) {
        for (language, handles) in self.handles.iter() {
            for sh in handles {
                tracing::info!("Shutting down LSP server '{}' for {}", sh.name, language);
                let _ = sh.handle.shutdown();
            }
        }
        self.handles.clear();
    }
}

impl Drop for LspManager {
    fn drop(&mut self) {
        self.shutdown_all();
    }
}

/// Helper function to detect language from file path using the config's languages section.
///
/// Priority order matches the grammar registry (`find_syntax_for_file_with_languages`):
/// 1. Exact filename match against `filenames` (highest priority)
/// 2. Glob pattern match against `filenames` entries containing wildcards
/// 3. File extension match against `extensions` (lowest config-based priority)
pub fn detect_language(
    path: &std::path::Path,
    languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
) -> Option<String> {
    use crate::primitives::glob_match::{
        filename_glob_matches, is_glob_pattern, is_path_pattern, path_glob_matches,
    };

    if let Some(filename) = path.file_name().and_then(|f| f.to_str()) {
        // 1. Exact filename match (highest priority)
        for (language_name, lang_config) in languages {
            if lang_config
                .filenames
                .iter()
                .any(|f| !is_glob_pattern(f) && f == filename)
            {
                return Some(language_name.clone());
            }
        }

        // 2. Glob pattern match
        // Path patterns (containing `/`) match against the full path;
        // filename-only patterns match against just the filename.
        let path_str = path.to_str().unwrap_or("");
        for (language_name, lang_config) in languages {
            if lang_config.filenames.iter().any(|f| {
                if !is_glob_pattern(f) {
                    return false;
                }
                if is_path_pattern(f) {
                    path_glob_matches(f, path_str)
                } else {
                    filename_glob_matches(f, filename)
                }
            }) {
                return Some(language_name.clone());
            }
        }
    }

    // 3. Extension match (lowest priority among config-based detection)
    if let Some(extension) = path.extension().and_then(|e| e.to_str()) {
        for (language_name, lang_config) in languages {
            if lang_config.extensions.iter().any(|ext| ext == extension) {
                return Some(language_name.clone());
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_lsp_manager_new() {
        let root_uri: Option<Uri> = "file:///test".parse().ok();
        let manager = LspManager::new(root_uri.clone());

        // Manager should start with no handles
        assert_eq!(manager.handles.len(), 0);
        assert_eq!(manager.config.len(), 0);
        assert!(manager.root_uri.is_some());
        assert!(manager.runtime.is_none());
        assert!(manager.async_bridge.is_none());
    }

    #[test]
    fn test_lsp_manager_set_language_config() {
        let mut manager = LspManager::new(None);

        let config = LspServerConfig {
            enabled: true,
            command: "rust-analyzer".to_string(),
            args: vec![],
            process_limits: crate::services::process_limits::ProcessLimits::unlimited(),
            auto_start: false,
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            name: None,
            only_features: None,
            except_features: None,
            root_markers: Default::default(),
        };

        manager.set_language_config("rust".to_string(), config);

        assert_eq!(manager.config.len(), 1);
        assert!(manager.config.contains_key("rust"));
        assert!(manager.config.get("rust").unwrap().first().unwrap().enabled);
    }

    #[test]
    fn test_lsp_manager_force_spawn_no_runtime() {
        let mut manager = LspManager::new(None);

        // Add config for rust
        manager.set_language_config(
            "rust".to_string(),
            LspServerConfig {
                enabled: true,
                command: "rust-analyzer".to_string(),
                args: vec![],
                process_limits: crate::services::process_limits::ProcessLimits::unlimited(),
                auto_start: false,
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            },
        );

        // force_spawn should return None without runtime
        let result = manager.force_spawn("rust", None);
        assert!(result.is_none());
    }

    #[test]
    fn test_lsp_manager_force_spawn_no_config() {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let mut manager = LspManager::new(None);
        let async_bridge = AsyncBridge::new();

        manager.set_runtime(rt.handle().clone(), async_bridge);

        // force_spawn should return None for unconfigured language
        let result = manager.force_spawn("rust", None);
        assert!(result.is_none());
    }

    #[test]
    fn test_lsp_manager_force_spawn_disabled_language() {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let mut manager = LspManager::new(None);
        let async_bridge = AsyncBridge::new();

        manager.set_runtime(rt.handle().clone(), async_bridge);

        // Add disabled config (command is optional when disabled)
        manager.set_language_config(
            "rust".to_string(),
            LspServerConfig {
                enabled: false,
                command: String::new(), // command not required when disabled
                args: vec![],
                process_limits: crate::services::process_limits::ProcessLimits::unlimited(),
                auto_start: false,
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                name: None,
                only_features: None,
                except_features: None,
                root_markers: Default::default(),
            },
        );

        // force_spawn should return None for disabled language
        let result = manager.force_spawn("rust", None);
        assert!(result.is_none());
    }

    #[test]
    fn test_lsp_manager_shutdown_all() {
        let mut manager = LspManager::new(None);

        // shutdown_all should not panic even with no handles
        manager.shutdown_all();
        assert_eq!(manager.handles.len(), 0);
    }

    fn test_languages() -> std::collections::HashMap<String, crate::config::LanguageConfig> {
        let mut languages = std::collections::HashMap::new();
        languages.insert(
            "rust".to_string(),
            crate::config::LanguageConfig {
                extensions: vec!["rs".to_string()],
                filenames: vec![],
                grammar: "rust".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                textmate_grammar: None,
                show_whitespace_tabs: false,
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
            "javascript".to_string(),
            crate::config::LanguageConfig {
                extensions: vec!["js".to_string(), "jsx".to_string()],
                filenames: vec![],
                grammar: "javascript".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                textmate_grammar: None,
                show_whitespace_tabs: false,
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
            "csharp".to_string(),
            crate::config::LanguageConfig {
                extensions: vec!["cs".to_string()],
                filenames: vec![],
                grammar: "c_sharp".to_string(),
                comment_prefix: Some("//".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                textmate_grammar: None,
                show_whitespace_tabs: false,
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

    #[test]
    fn test_detect_language_from_config() {
        let languages = test_languages();

        // Test configured languages
        assert_eq!(
            detect_language(Path::new("main.rs"), &languages),
            Some("rust".to_string())
        );
        assert_eq!(
            detect_language(Path::new("index.js"), &languages),
            Some("javascript".to_string())
        );
        assert_eq!(
            detect_language(Path::new("App.jsx"), &languages),
            Some("javascript".to_string())
        );
        assert_eq!(
            detect_language(Path::new("Program.cs"), &languages),
            Some("csharp".to_string())
        );

        // Test unconfigured extensions return None
        assert_eq!(detect_language(Path::new("main.py"), &languages), None);
        assert_eq!(detect_language(Path::new("file.xyz"), &languages), None);
        assert_eq!(detect_language(Path::new("file"), &languages), None);
    }

    #[test]
    fn test_detect_language_no_extension() {
        let languages = test_languages();
        assert_eq!(detect_language(Path::new("README"), &languages), None);
        assert_eq!(detect_language(Path::new("Makefile"), &languages), None);
    }

    #[test]
    fn test_detect_language_path_glob() {
        let mut languages = test_languages();
        languages.insert(
            "shell".to_string(),
            crate::config::LanguageConfig {
                extensions: vec!["sh".to_string()],
                filenames: vec!["/etc/**/rc.*".to_string(), "*rc".to_string()],
                grammar: "bash".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                textmate_grammar: None,
                show_whitespace_tabs: false,
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

        // Path glob: /etc/**/rc.* should match
        assert_eq!(
            detect_language(Path::new("/etc/rc.conf"), &languages),
            Some("shell".to_string())
        );
        assert_eq!(
            detect_language(Path::new("/etc/init/rc.local"), &languages),
            Some("shell".to_string())
        );
        // Path glob should NOT match different root
        assert_eq!(detect_language(Path::new("/var/rc.conf"), &languages), None);

        // Filename glob: *rc should still work
        assert_eq!(
            detect_language(Path::new("lfrc"), &languages),
            Some("shell".to_string())
        );
    }

    #[test]
    fn test_detect_workspace_root_finds_marker_in_parent() {
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("myproject");
        let src = project.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(project.join("Cargo.toml"), "").unwrap();
        let file = src.join("main.rs");
        std::fs::write(&file, "").unwrap();

        let root = detect_workspace_root(&file, &["Cargo.toml".to_string(), ".git".to_string()]);
        assert_eq!(root, project);
    }

    #[test]
    fn test_detect_workspace_root_finds_marker_two_levels_up() {
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("myproject");
        let deep = project.join("src").join("nested");
        std::fs::create_dir_all(&deep).unwrap();
        std::fs::write(project.join("Cargo.toml"), "").unwrap();
        let file = deep.join("lib.rs");
        std::fs::write(&file, "").unwrap();

        let root = detect_workspace_root(&file, &["Cargo.toml".to_string()]);
        assert_eq!(root, project);
    }

    #[test]
    fn test_detect_workspace_root_no_marker_returns_parent() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path().join("somedir");
        std::fs::create_dir_all(&dir).unwrap();
        let file = dir.join("file.txt");
        std::fs::write(&file, "").unwrap();

        let root = detect_workspace_root(&file, &["nonexistent_marker".to_string()]);
        assert_eq!(root, dir);
    }

    #[test]
    fn test_detect_workspace_root_empty_markers_returns_parent() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path().join("somedir");
        std::fs::create_dir_all(&dir).unwrap();
        let file = dir.join("file.txt");
        std::fs::write(&file, "").unwrap();

        let root = detect_workspace_root(&file, &[]);
        assert_eq!(root, dir);
    }

    #[test]
    fn test_detect_workspace_root_directory_marker() {
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("myproject");
        let src = project.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::create_dir_all(project.join(".git")).unwrap();
        let file = src.join("main.rs");
        std::fs::write(&file, "").unwrap();

        let root = detect_workspace_root(&file, &[".git".to_string()]);
        assert_eq!(root, project);
    }

    #[test]
    fn test_path_to_uri_basic() {
        let uri = path_to_uri(Path::new("/tmp/test")).unwrap();
        assert_eq!(uri.as_str(), "file:///tmp/test");
    }

    #[test]
    fn test_path_to_uri_with_spaces() {
        let uri = path_to_uri(Path::new("/tmp/my project/src")).unwrap();
        assert_eq!(uri.as_str(), "file:///tmp/my%20project/src");
    }
}
