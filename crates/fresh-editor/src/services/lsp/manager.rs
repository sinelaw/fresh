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
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::Path;
use std::time::{Duration, Instant};

/// Consume and discard a `Result` from a fire-and-forget operation.
///
/// Use for best-effort cleanup where failure is expected and non-actionable,
/// e.g. shutting down an LSP server that may have already exited.
fn fire_and_forget<E: std::fmt::Debug>(result: Result<(), E>) {
    if let Err(e) = result {
        tracing::trace!(error = ?e, "fire-and-forget operation failed");
    }
}

/// Which languages an LSP server handles.
///
/// Empty means the server is universal (accepts all languages).
/// Non-empty lists only the accepted languages.
#[derive(Debug, Clone)]
pub struct LanguageScope(Vec<String>);

impl LanguageScope {
    /// Universal scope — accepts all languages.
    pub fn all() -> Self {
        Self(Vec::new())
    }

    /// Scope for a single language.
    pub fn single(language: impl Into<String>) -> Self {
        Self(vec![language.into()])
    }

    /// Whether this scope accepts documents of the given language.
    pub fn accepts(&self, language: &str) -> bool {
        self.0.is_empty() || self.0.iter().any(|l| l == language)
    }

    /// Whether this is a universal scope (accepts all languages).
    pub fn is_universal(&self) -> bool {
        self.0.is_empty()
    }

    /// The language list. Empty means all.
    pub fn languages(&self) -> &[String] {
        &self.0
    }

    /// A display label for logging and status messages.
    pub fn label(&self) -> &str {
        self.0.first().map(|s| s.as_str()).unwrap_or("universal")
    }
}

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

/// Outcome of consulting the spawn gate for a language.
///
/// The gate is the single throttle point for process spawns — every
/// path that ultimately forks an LSP child (user activity via
/// `try_spawn` → `force_spawn`, scheduled restarts via
/// `process_pending_restarts`, crash recovery, manual restarts) goes
/// through it. Previously, only `handle_server_crash` tracked restart
/// attempts, which meant a fast-crashing server respawned on every
/// edit via `force_spawn` and flooded the log (see #1612).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SpawnDecision {
    /// A handle already exists — return the existing one, don't spawn.
    Existing,
    /// Spawn is permitted; the attempt has been recorded.
    Allow,
    /// A restart is already scheduled via exponential backoff — do
    /// not double-spawn. The scheduled restart will fire later.
    PendingBackoff,
    /// The language hit the crash cap and is in cooldown until the
    /// user manually re-enables it.
    CooledDown,
}

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
    /// All running LSP server handles. Each handle's `LanguageScope` determines
    /// which languages it serves. Universal servers have `LanguageScope::all()`.
    handles: Vec<ServerHandle>,

    /// Configuration for each language (supports multiple servers per language)
    config: HashMap<String, Vec<LspServerConfig>>,

    /// Universal (global) LSP server configs — spawned once per project.
    universal_configs: Vec<LspServerConfig>,

    /// Default root URI for workspace (used if no per-language root is set)
    root_uri: Option<Uri>,

    /// Per-language root URIs (allows plugins to specify project roots)
    per_language_root_uris: HashMap<String, Uri>,

    /// Tokio runtime reference
    runtime: Option<tokio::runtime::Handle>,

    /// Async bridge for communication
    async_bridge: Option<AsyncBridge>,

    /// Long-running stdio spawner from the active authority. Used by
    /// `force_spawn` and friends to route LSP child processes through
    /// the right backend (local Command, `docker exec -i`, SSH). Set
    /// by `set_long_running_spawner` from `Editor::set_boot_authority`
    /// before any LSP spawn can happen.
    long_running_spawner: Option<std::sync::Arc<dyn crate::services::remote::LongRunningSpawner>>,

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
            handles: Vec::new(),
            config: HashMap::new(),
            universal_configs: Vec::new(),
            root_uri,
            per_language_root_uris: HashMap::new(),
            runtime: None,
            async_bridge: None,
            long_running_spawner: None,
            restart_attempts: HashMap::new(),
            restart_cooldown: HashSet::new(),
            pending_restarts: HashMap::new(),
            allowed_languages: HashSet::new(),
            disabled_languages: HashSet::new(),
        }
    }

    /// Wire the long-running spawner from the active `Authority`.
    ///
    /// Called from `Editor::set_boot_authority` so every LSP server
    /// spawned after this point runs under the right backend — local
    /// host, `docker exec -i` for containers, or SSH-tunneled.
    /// Authority transitions destroy and rebuild the editor (and
    /// therefore `LspManager`), so this is a one-shot wiring call per
    /// editor instance.
    pub fn set_long_running_spawner(
        &mut self,
        spawner: std::sync::Arc<dyn crate::services::remote::LongRunningSpawner>,
    ) {
        self.long_running_spawner = Some(spawner);
    }

    /// Blocking variant of the authority-routed command probe used by
    /// the LSP status popup (which runs on the main thread and needs a
    /// synchronous answer). Blocks on the tokio runtime to drive the
    /// async trait method; the local spawner resolves immediately via
    /// `which::which`, the docker spawner runs a short
    /// `docker exec <id> sh -c 'command -v <cmd>'`.
    ///
    /// Falls back to the module-level host probe when the spawner or
    /// runtime hasn't been wired yet (e.g. during early editor boot
    /// before `set_boot_authority` runs). The fallback is only
    /// reachable in test harnesses and a vanishingly small window
    /// around startup, so routing through the authority is the
    /// effective behavior in production.
    pub fn command_exists_via_authority(&self, command: &str) -> bool {
        if command.is_empty() {
            return false;
        }
        let (Some(runtime), Some(spawner)) =
            (self.runtime.as_ref(), self.long_running_spawner.as_ref())
        else {
            return crate::services::lsp::command_exists(command);
        };
        runtime.block_on(spawner.command_exists(command))
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
        _language: &str,
        server_name: &str,
        mut capabilities: ServerCapabilitySummary,
    ) {
        capabilities.initialized = true;

        if let Some(sh) = self.handles.iter_mut().find(|sh| sh.name == server_name) {
            sh.capabilities = capabilities;
        }
    }

    /// Get the semantic token legend for a language from the first eligible server.
    pub fn semantic_tokens_legend(&self, language: &str) -> Option<&SemanticTokensLegend> {
        self.get_handles(language).into_iter().find_map(|sh| {
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
        self.get_handles(language).iter().any(|sh| {
            sh.feature_filter.allows(LspFeature::SemanticTokens)
                && sh.capabilities.semantic_tokens_full
        })
    }

    /// Check if any eligible server for the language supports full semantic token deltas.
    pub fn semantic_tokens_full_delta_supported(&self, language: &str) -> bool {
        self.get_handles(language).iter().any(|sh| {
            sh.feature_filter.allows(LspFeature::SemanticTokens)
                && sh.capabilities.semantic_tokens_full_delta
        })
    }

    /// Check if any eligible server for the language supports range semantic tokens.
    pub fn semantic_tokens_range_supported(&self, language: &str) -> bool {
        self.get_handles(language).iter().any(|sh| {
            sh.feature_filter.allows(LspFeature::SemanticTokens)
                && sh.capabilities.semantic_tokens_range
        })
    }

    /// Check if any eligible server for the language supports folding ranges.
    pub fn folding_ranges_supported(&self, language: &str) -> bool {
        self.get_handles(language).iter().any(|sh| {
            sh.feature_filter.allows(LspFeature::FoldingRange) && sh.capabilities.folding_ranges
        })
    }

    /// Check if a character is a completion trigger for any running language server.
    pub fn is_completion_trigger_char(&self, ch: char, language: &str) -> bool {
        let ch_str = ch.to_string();
        self.get_handles(language).iter().any(|sh| {
            sh.feature_filter.allows(LspFeature::Completion)
                && sh
                    .capabilities
                    .completion_trigger_characters
                    .contains(&ch_str)
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
        // If handles already exist for this language, just ensure universals are running too
        if self
            .handles
            .iter()
            .any(|sh| sh.handle.scope().accepts(language))
        {
            self.ensure_universal_servers_running(file_path);
            return LspSpawnResult::Spawned;
        }

        // Check if we have runtime and bridge
        if self.runtime.is_none() || self.async_bridge.is_none() {
            return LspSpawnResult::Failed;
        }

        // Always try to start universal servers (they manage their own auto_start check)
        self.ensure_universal_servers_running(file_path);

        // Check if language is configured
        let configs = match self.config.get(language) {
            Some(configs) if !configs.is_empty() => configs,
            _ => {
                // No per-language config, but universal servers may be running
                if self
                    .handles
                    .iter()
                    .any(|sh| sh.handle.scope().is_universal())
                {
                    return LspSpawnResult::Spawned;
                }
                return LspSpawnResult::NotConfigured;
            }
        };

        // Check if any per-language config is enabled
        if !configs.iter().any(|c| c.enabled) {
            if self
                .handles
                .iter()
                .any(|sh| sh.handle.scope().is_universal())
            {
                return LspSpawnResult::Spawned;
            }
            return LspSpawnResult::Failed;
        }

        // Check if auto_start is enabled (on any per-language config) or language was manually allowed
        let any_auto_start = configs.iter().any(|c| c.auto_start && c.enabled);
        if !any_auto_start && !self.allowed_languages.contains(language) {
            if self
                .handles
                .iter()
                .any(|sh| sh.handle.scope().is_universal())
            {
                return LspSpawnResult::Spawned;
            }
            return LspSpawnResult::NotAutoStart;
        }

        // Spawn per-language servers
        let spawned = self.force_spawn(language, file_path).is_some();

        if spawned
            || self
                .handles
                .iter()
                .any(|sh| sh.handle.scope().is_universal())
        {
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

    /// Set universal (global) LSP server configs.
    ///
    /// Universal servers are spawned once per project and shared across all
    /// languages, rather than being duplicated into each language's config list.
    pub fn set_universal_configs(&mut self, configs: Vec<LspServerConfig>) {
        self.universal_configs = configs;
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
        if self
            .handles
            .iter()
            .any(|sh| sh.handle.scope().accepts(language))
        {
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
    /// Checks language-specific handles first, then universal handles.
    pub fn get_handle(&self, language: &str) -> Option<&LspHandle> {
        self.handles
            .iter()
            .find(|sh| sh.handle.scope().accepts(language))
            .map(|sh| &sh.handle)
    }

    /// Get the primary (first) mutable existing LSP handle for a language (no spawning).
    /// Checks language-specific handles first, then universal handles.
    pub fn get_handle_mut(&mut self, language: &str) -> Option<&mut LspHandle> {
        self.handles
            .iter_mut()
            .find(|sh| sh.handle.scope().accepts(language))
            .map(|sh| &mut sh.handle)
    }

    /// Get all handles that accept a language (both language-specific and universal).
    pub fn get_handles(&self, language: &str) -> Vec<&ServerHandle> {
        self.handles
            .iter()
            .filter(|sh| sh.handle.scope().accepts(language))
            .collect()
    }

    /// Get all mutable handles that accept a language (both language-specific and universal).
    pub fn get_handles_mut(&mut self, language: &str) -> Vec<&mut ServerHandle> {
        self.handles
            .iter_mut()
            .filter(|sh| sh.handle.scope().accepts(language))
            .collect()
    }

    /// Get the language scope for a server by name.
    ///
    /// Returns `None` if the server is not found.
    pub fn server_scope(&self, server_name: &str) -> Option<&LanguageScope> {
        self.handles
            .iter()
            .find(|sh| sh.name == server_name)
            .map(|sh| sh.handle.scope())
    }

    /// Check if any handles (language-specific or universal) exist for a language.
    pub fn has_handles(&self, language: &str) -> bool {
        self.handles
            .iter()
            .any(|sh| sh.handle.scope().accepts(language))
    }

    /// Count all handles that accept a language.
    pub fn handle_count(&self, language: &str) -> usize {
        self.handles
            .iter()
            .filter(|sh| sh.handle.scope().accepts(language))
            .count()
    }

    /// Check if a server with the given name exists.
    pub fn has_server_named(&self, server_name: &str) -> bool {
        self.handles.iter().any(|sh| sh.name == server_name)
    }

    /// Get the first handle for a language that allows a given feature (for exclusive features).
    /// For capability-gated features (semantic tokens, folding ranges), this also checks
    /// that the server actually reported the capability during initialization.
    /// Checks per-language handles first, then universal handles.
    /// Returns `None` if no handle matches.
    pub fn handle_for_feature(&self, language: &str, feature: LspFeature) -> Option<&ServerHandle> {
        self.handles
            .iter()
            .filter(|sh| sh.handle.scope().accepts(language))
            .find(|sh| sh.feature_filter.allows(feature) && sh.has_capability(feature))
    }

    /// Get the first mutable handle for a language that allows a given feature.
    /// For capability-gated features, this also checks the server's actual capabilities.
    /// Checks per-language handles first, then universal handles.
    pub fn handle_for_feature_mut(
        &mut self,
        language: &str,
        feature: LspFeature,
    ) -> Option<&mut ServerHandle> {
        self.handles
            .iter_mut()
            .filter(|sh| sh.handle.scope().accepts(language))
            .find(|sh| sh.feature_filter.allows(feature) && sh.has_capability(feature))
    }

    /// Get all handles for a language that allow a given feature (for merged features).
    /// Like `handle_for_feature`, also checks per-server capabilities.
    /// Includes both per-language and universal handles.
    pub fn handles_for_feature(&self, language: &str, feature: LspFeature) -> Vec<&ServerHandle> {
        self.handles
            .iter()
            .filter(|sh| sh.handle.scope().accepts(language))
            .filter(|sh| sh.feature_filter.allows(feature) && sh.has_capability(feature))
            .collect()
    }

    /// Get all mutable handles for a language that allow a given feature.
    /// Like `handle_for_feature_mut`, also checks per-server capabilities.
    /// Includes both per-language and universal handles.
    pub fn handles_for_feature_mut(
        &mut self,
        language: &str,
        feature: LspFeature,
    ) -> Vec<&mut ServerHandle> {
        self.handles
            .iter_mut()
            .filter(|sh| sh.handle.scope().accepts(language))
            .filter(|sh| sh.feature_filter.allows(feature) && sh.has_capability(feature))
            .collect()
    }

    /// Consult the spawn throttle for `language` and, on `Allow`, record
    /// the attempt.
    ///
    /// This is the single source of truth for "are we allowed to spawn
    /// another LSP child right now?" — every path that actually spawns
    /// must call it.  The caller should propagate the decision (return
    /// None / refuse to spawn) on anything other than `Allow`.
    fn spawn_decision(&mut self, language: &str) -> SpawnDecision {
        if self
            .handles
            .iter()
            .any(|sh| sh.handle.scope().accepts(language))
        {
            return SpawnDecision::Existing;
        }
        if self.restart_cooldown.contains(language) {
            return SpawnDecision::CooledDown;
        }
        if self.pending_restarts.contains_key(language) {
            return SpawnDecision::PendingBackoff;
        }

        let now = Instant::now();
        let window = Duration::from_secs(RESTART_WINDOW_SECS);
        let attempts = self
            .restart_attempts
            .entry(language.to_string())
            .or_default();
        attempts.retain(|t| now.duration_since(*t) < window);

        if attempts.len() >= MAX_RESTARTS_IN_WINDOW {
            self.restart_cooldown.insert(language.to_string());
            tracing::warn!(
                "LSP server for {} has spawned {} times in {} minutes, entering cooldown",
                language,
                MAX_RESTARTS_IN_WINDOW,
                RESTART_WINDOW_SECS / 60
            );
            return SpawnDecision::CooledDown;
        }

        attempts.push(now);
        SpawnDecision::Allow
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
        if self
            .handles
            .iter()
            .any(|sh| sh.handle.scope().accepts(language))
        {
            tracing::debug!("force_spawn: returning existing handle for {}", language);
            return self
                .handles
                .iter_mut()
                .find(|sh| sh.handle.scope().accepts(language))
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

        // Consult the spawn gate. This is the single point that enforces
        // the restart throttle across *all* spawn entry points — user
        // activity (try_spawn), scheduled backoff, manual restart. See
        // #1612: previously only handle_server_crash tracked attempts,
        // so a fast-crashing server got respawned on every edit.
        match self.spawn_decision(language) {
            SpawnDecision::Existing => {
                // Existing-handle case is already short-circuited above,
                // but handle it defensively.
                return self
                    .handles
                    .iter_mut()
                    .find(|sh| sh.handle.scope().accepts(language))
                    .map(|sh| &mut sh.handle);
            }
            SpawnDecision::CooledDown => {
                tracing::debug!(
                    "force_spawn: {} is in cooldown, refusing spawn (use Restart LSP command)",
                    language
                );
                return None;
            }
            SpawnDecision::PendingBackoff => {
                tracing::debug!(
                    "force_spawn: {} has a pending restart scheduled, not double-spawning",
                    language
                );
                return None;
            }
            SpawnDecision::Allow => {}
        }

        // Check we have runtime, bridge, and the authority's spawner.
        // All three are wired at editor construction; a missing one is
        // a configuration error worth surfacing rather than silently
        // degrading to a host-only spawn.
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
        // Default to the local spawner when nothing's been wired. The
        // editor's `set_boot_authority` wires this as part of normal
        // construction; tests and other call sites that construct an
        // LspManager directly without going through Editor get the
        // pre-Phase-L behavior (host-only spawn) automatically. Passing
        // through the warning keeps the failure loud enough to catch
        // regressions in Editor-side wiring.
        let long_running_spawner = match self.long_running_spawner.as_ref() {
            Some(s) => s.clone(),
            None => {
                tracing::warn!(
                    "force_spawn: long-running spawner not wired for {} — \
                     falling back to host-local spawn (normal for tests \
                     that skip set_boot_authority)",
                    language
                );
                std::sync::Arc::new(crate::services::remote::LocalLongRunningSpawner)
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
                LanguageScope::single(language),
                server_name.clone(),
                &async_bridge,
                config.process_limits.clone(),
                config.language_id_overrides.clone(),
                long_running_spawner.clone(),
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

        self.handles.extend(spawned_handles);
        self.handles
            .iter_mut()
            .rev()
            .find(|sh| sh.handle.scope().accepts(language))
            .map(|sh| &mut sh.handle)
    }

    /// Spawn universal LSP servers if they aren't already running.
    ///
    /// Called from `try_spawn` — universal servers are spawned once and shared
    /// across all languages. Only servers with `enabled=true` and
    /// `auto_start=true` are started automatically.
    fn ensure_universal_servers_running(&mut self, file_path: Option<&Path>) {
        if self
            .handles
            .iter()
            .any(|sh| sh.handle.scope().is_universal())
            || self.universal_configs.is_empty()
        {
            return;
        }

        let runtime = match self.runtime.as_ref() {
            Some(r) => r.clone(),
            None => return,
        };
        let async_bridge = match self.async_bridge.as_ref() {
            Some(b) => b.clone(),
            None => return,
        };
        let long_running_spawner =
            self.long_running_spawner
                .as_ref()
                .cloned()
                .unwrap_or_else(|| {
                    std::sync::Arc::new(crate::services::remote::LocalLongRunningSpawner)
                });

        let mut spawned = Vec::new();
        for config in &self.universal_configs {
            if !config.enabled || !config.auto_start {
                continue;
            }
            if config.command.is_empty() {
                continue;
            }

            let server_name = config.display_name();
            tracing::info!("Spawning universal LSP server '{}'", server_name);

            match LspHandle::spawn(
                &runtime,
                &config.command,
                &config.args,
                config.env.clone(),
                LanguageScope::all(),
                server_name.clone(),
                &async_bridge,
                config.process_limits.clone(),
                config.language_id_overrides.clone(),
                long_running_spawner.clone(),
            ) {
                Ok(handle) => {
                    let effective_root = file_path
                        .map(|p| {
                            let root = detect_workspace_root(p, &config.root_markers);
                            path_to_uri(&root)
                        })
                        .flatten()
                        .or_else(|| self.root_uri.clone());
                    if let Err(e) =
                        handle.initialize(effective_root, config.initialization_options.clone())
                    {
                        tracing::error!(
                            "Failed to initialize universal LSP server '{}': {}",
                            server_name,
                            e
                        );
                        continue;
                    }
                    tracing::info!(
                        "Universal LSP server '{}' initialization started",
                        server_name
                    );
                    spawned.push(ServerHandle {
                        name: server_name,
                        handle,
                        feature_filter: config.feature_filter(),
                        capabilities: ServerCapabilitySummary::default(),
                    });
                }
                Err(e) => {
                    tracing::error!(
                        "Failed to spawn universal LSP server '{}': {}",
                        server_name,
                        e
                    );
                }
            }
        }

        self.handles.extend(spawned);
    }

    /// Handle a server crash by scheduling a restart with exponential backoff
    ///
    /// Returns a message describing the action taken (for UI notification)
    pub fn handle_server_crash(&mut self, language: &str, server_name: &str) -> String {
        // Check if the crashed server is a universal handle
        if self
            .handles
            .iter()
            .any(|sh| sh.name == server_name && sh.handle.scope().is_universal())
        {
            // Drain all universal handles and shut them down
            let universals: Vec<ServerHandle> = {
                let mut drained = Vec::new();
                let mut i = 0;
                while i < self.handles.len() {
                    if self.handles[i].handle.scope().is_universal() {
                        drained.push(self.handles.remove(i));
                    } else {
                        i += 1;
                    }
                }
                drained
            };
            for sh in universals {
                fire_and_forget(sh.handle.shutdown());
            }
            // Universal servers will be re-spawned on next try_spawn call
            return "Universal LSP server crashed. It will restart on next file open.".to_string();
        }

        // Remove all handles that accept this language (but not universal ones)
        {
            let mut i = 0;
            while i < self.handles.len() {
                if !self.handles[i].handle.scope().is_universal()
                    && self.handles[i].handle.scope().accepts(language)
                {
                    let sh = self.handles.remove(i);
                    fire_and_forget(sh.handle.shutdown());
                } else {
                    i += 1;
                }
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

        // Attempt-counting and the cap are owned by `spawn_decision`
        // (called at every real spawn site). Here we only schedule the
        // next restart with exponential backoff; the gate will decide
        // whether it actually proceeds when the pending restart fires.
        let now = Instant::now();
        let attempt_number = self
            .restart_attempts
            .get(language)
            .map(|v| v.len())
            .unwrap_or(0);

        let delay_ms = RESTART_BACKOFF_BASE_MS * (1 << attempt_number); // 1s, 2s, 4s, 8s
        let restart_time = now + Duration::from_millis(delay_ms);

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

            // Attempt to spawn the server (bypassing auto_start for
            // crash recovery). The attempt is recorded by the spawn
            // gate inside force_spawn — no need to push here.
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
    pub fn manual_restart(&mut self, language: &str, file_path: Option<&Path>) -> (bool, String) {
        // Clear any existing state
        self.clear_cooldown(language);

        // Re-enable the language (remove from disabled set)
        self.disabled_languages.remove(language);

        // Add to allowed languages so it stays active even if auto_start=false
        self.allowed_languages.insert(language.to_string());

        // Remove existing handles for this language (non-universal)
        {
            let mut i = 0;
            while i < self.handles.len() {
                if !self.handles[i].handle.scope().is_universal()
                    && self.handles[i].handle.scope().accepts(language)
                {
                    let sh = self.handles.remove(i);
                    fire_and_forget(sh.handle.shutdown());
                } else {
                    i += 1;
                }
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
        if let Some(idx) = self.handles.iter().position(|sh| sh.name == server_name) {
            let sh = self.handles.remove(idx);
            fire_and_forget(sh.handle.shutdown());
        }

        // Find the matching config (check per-language first, then universal)
        let is_universal = self
            .universal_configs
            .iter()
            .any(|c| c.display_name() == server_name);
        let config = if is_universal {
            self.universal_configs
                .iter()
                .find(|c| c.display_name() == server_name)
                .cloned()
        } else {
            self.config
                .get(language)
                .and_then(|configs| configs.iter().find(|c| c.display_name() == server_name))
                .cloned()
        };

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
        let long_running_spawner =
            self.long_running_spawner
                .as_ref()
                .cloned()
                .unwrap_or_else(|| {
                    std::sync::Arc::new(crate::services::remote::LocalLongRunningSpawner)
                });

        let scope = if is_universal {
            LanguageScope::all()
        } else {
            LanguageScope::single(language)
        };

        match LspHandle::spawn(
            &runtime,
            &config.command,
            &config.args,
            config.env.clone(),
            scope,
            server_name.to_string(),
            &async_bridge,
            config.process_limits.clone(),
            config.language_id_overrides.clone(),
            long_running_spawner,
        ) {
            Ok(handle) => {
                let effective_root = if is_universal {
                    file_path
                        .map(|p| {
                            let root = detect_workspace_root(p, &config.root_markers);
                            path_to_uri(&root)
                        })
                        .flatten()
                        .or_else(|| self.root_uri.clone())
                } else {
                    self.resolve_root_uri(language, file_path)
                };
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

                self.handles.push(sh);

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

    /// Get a list of currently running LSP server language labels (deduplicated).
    pub fn running_servers(&self) -> Vec<String> {
        let mut labels: Vec<String> = self
            .handles
            .iter()
            .map(|sh| sh.handle.scope().label().to_string())
            .collect();
        labels.sort();
        labels.dedup();
        labels
    }

    /// Get the names of all running servers for a given language
    pub fn server_names_for_language(&self, language: &str) -> Vec<String> {
        self.handles
            .iter()
            .filter(|sh| sh.handle.scope().accepts(language))
            .map(|sh| sh.name.clone())
            .collect()
    }

    /// Check if any LSP server for a language is running and ready to serve requests
    pub fn is_server_ready(&self, language: &str) -> bool {
        self.handles
            .iter()
            .filter(|sh| sh.handle.scope().accepts(language))
            .any(|sh| sh.handle.state().can_send_requests())
    }

    /// Shutdown a single server by name for a specific language.
    ///
    /// Returns true if the server was found and shut down.
    /// If this was the last server for the language, marks the language as disabled.
    pub fn shutdown_server_by_name(&mut self, language: &str, server_name: &str) -> bool {
        let Some(idx) = self.handles.iter().position(|sh| sh.name == server_name) else {
            tracing::warn!(
                "No running LSP server named '{}' found for {}",
                server_name,
                language
            );
            return false;
        };

        let sh = self.handles.remove(idx);
        tracing::info!(
            "Shutting down LSP server '{}' for {} (disabled until manual restart)",
            sh.name,
            language
        );
        fire_and_forget(sh.handle.shutdown());

        // If no more non-universal handles remain for this language, mark it disabled
        let has_remaining = self
            .handles
            .iter()
            .any(|sh| !sh.handle.scope().is_universal() && sh.handle.scope().accepts(language));
        if !has_remaining {
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
    pub fn shutdown_server(&mut self, language: &str) -> bool {
        let mut found = false;
        let mut i = 0;
        while i < self.handles.len() {
            if !self.handles[i].handle.scope().is_universal()
                && self.handles[i].handle.scope().accepts(language)
            {
                let sh = self.handles.remove(i);
                tracing::info!(
                    "Shutting down LSP server '{}' for {} (disabled until manual restart)",
                    sh.name,
                    language
                );
                fire_and_forget(sh.handle.shutdown());
                found = true;
            } else {
                i += 1;
            }
        }

        if found {
            self.disabled_languages.insert(language.to_string());
            self.pending_restarts.remove(language);
            self.restart_cooldown.remove(language);
            self.allowed_languages.remove(language);
        } else {
            tracing::warn!("No running LSP server found for {}", language);
        }

        found
    }

    /// Shutdown all language servers (including universal servers)
    pub fn shutdown_all(&mut self) {
        for sh in &self.handles {
            tracing::info!(
                "Shutting down LSP server '{}' ({})",
                sh.name,
                sh.handle.scope().label()
            );
            fire_and_forget(sh.handle.shutdown());
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
/// Priority order matches `GrammarRegistry::find_by_path`:
/// 1. Exact filename match against `filenames` (highest priority)
/// 2. Glob pattern match against `filenames` entries containing wildcards
/// 3. File extension match against `extensions` (lowest config-based priority)
///
/// Kept separate from `find_by_path` because this returns the user's
/// config **key** (`[languages.mylang]` → `"mylang"`) rather than the
/// catalog entry's `language_id`, which is needed for LSP routing when a
/// user aliases an existing grammar.
pub fn detect_language(
    path: &std::path::Path,
    languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
) -> Option<String> {
    let detected = detect_language_by_config(path, languages);

    // `.h` headers: the default config maps the extension to C, but in C++
    // projects the header is still C++ and must route to clangd in C++ mode.
    // If the detected language is `c`, the file is `.h`, and the surrounding
    // tree smells like C++ (sibling C++ sources or an ancestor
    // `compile_commands.json`), promote to `cpp` so the LSP binding is right.
    if detected.as_deref() == Some("c")
        && path.extension().and_then(|e| e.to_str()) == Some("h")
        && languages.contains_key("cpp")
        && header_in_cpp_tree(path)
    {
        return Some("cpp".to_string());
    }

    detected
}

/// Pure config/path-based language detection without filesystem probing.
fn detect_language_by_config(
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

/// Filesystem probe: does this header sit inside something that looks like
/// a C++ project? Two signals, both conservative:
///
///   * The file's own directory contains any C++ source or C++-specific
///     header (`.cc`, `.cpp`, `.cxx`, `.C`, `.c++`, `.hpp`, `.hh`, `.hxx`).
///     Decisive — if the siblings are C++, the header is too.
///   * An ancestor up to 10 levels deep contains a `compile_commands.json`
///     whose content carries a C++ marker. The mere presence of the file
///     is not enough: CMake emits `compile_commands.json` for pure-C
///     builds as well, so we peek inside and only promote when the
///     payload mentions a C++-specific compiler, flag, or source
///     extension (`c++`, `.cpp`, `.cc`, `.cxx`, `.C` ). This still covers
///     the fmt / Chromium / LLVM / Qt-style layouts where the header
///     lives deep under `include/` while sources sit in `src/` at the
///     project root.
///
/// Bounded by depth (10), by a single shallow `read_dir` at the start,
/// and by a capped 1 MiB read of `compile_commands.json`, so the cost is
/// a handful of `stat`s plus at most one bounded read on file open.
/// Silent on any I/O error — if we can't see the filesystem we fall back
/// to the default config answer (C), which is the pre-fix behavior.
///
/// NOTE(remote-fs): Uses `std::fs` directly, matching the pre-existing
/// `detect_workspace_root` in this module. On SSH sessions the probe
/// sees the local filesystem, so the promotion silently becomes a no-op
/// (returns `false`, falls back to `c`). Fixing this requires threading
/// `&dyn FileSystem` through `detect_language` and
/// `DetectedLanguage::from_path` — a cross-cutting refactor that should
/// be done alongside the same fix for `detect_workspace_root`.
fn header_in_cpp_tree(path: &std::path::Path) -> bool {
    let Some(start_dir) = path.parent() else {
        return false;
    };

    // 1. Sibling scan in the header's own directory.
    if let Ok(entries) = std::fs::read_dir(start_dir) {
        for entry in entries.flatten() {
            let p = entry.path();
            let Some(ext) = p.extension().and_then(|e| e.to_str()) else {
                continue;
            };
            if matches!(
                ext,
                "cc" | "cpp" | "cxx" | "C" | "c++" | "hpp" | "hh" | "hxx"
            ) {
                return true;
            }
        }
    }

    // 2. Walk ancestors for compile_commands.json, and only promote if
    //    the file actually carries a C++ marker — CMake emits it for
    //    pure-C builds too.
    let mut current = Some(start_dir);
    let mut depth = 0u32;
    while let Some(dir) = current {
        let cc = dir.join("compile_commands.json");
        if cc.is_file() && compile_commands_has_cpp_marker(&cc) {
            return true;
        }
        if depth >= 10 {
            break;
        }
        depth += 1;
        current = dir.parent();
    }

    false
}

/// Returns true when `compile_commands.json` contains a C++ marker —
/// either the literal substring `c++` (covers `-std=c++17`, `clang++`,
/// `g++`, the `c++` compiler name) or a C++ source extension in a
/// context where it cannot be confused with an adjacent header path
/// (`.cpp`, `.cc`, `.cxx`). Reads at most 1 MiB so multi-megabyte
/// compile DBs from large monorepos don't block file open; a valid CMake
/// entry fits comfortably in that window.
fn compile_commands_has_cpp_marker(path: &std::path::Path) -> bool {
    use std::io::Read;
    const MAX_READ: u64 = 1_048_576;

    let Ok(file) = std::fs::File::open(path) else {
        return false;
    };
    let mut buf = Vec::with_capacity(64 * 1024);
    if file.take(MAX_READ).read_to_end(&mut buf).is_err() {
        return false;
    }
    let Ok(text) = std::str::from_utf8(&buf) else {
        return false;
    };

    // Strongest single marker: literal "c++" appears in -std=c++NN,
    // clang++, g++, and the "c++" compiler name — never in a pure-C
    // compilation invocation.
    if text.contains("c++") {
        return true;
    }
    // Secondary markers: any mention of a C++ source extension in the
    // compile DB implies at least one C++ translation unit in the tree.
    text.contains(".cpp") || text.contains(".cxx") || text.contains(".cc\"")
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

    /// Returns a languages map mirroring the default config's `c` + `cpp`
    /// entries: `.h` maps to `c`, and `.cpp/.cc/.cxx/.hpp/.hh/.hxx` map to
    /// `cpp`. Matches `config.rs:3010` and `:3040-3047` so the promotion
    /// logic is exercised under realistic config.
    fn c_cpp_languages() -> std::collections::HashMap<String, crate::config::LanguageConfig> {
        use crate::config::LanguageConfig;
        let mut languages = std::collections::HashMap::new();
        let base = LanguageConfig {
            extensions: vec![],
            filenames: vec![],
            grammar: String::new(),
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
        };
        languages.insert(
            "c".to_string(),
            LanguageConfig {
                extensions: vec!["c".to_string(), "h".to_string()],
                grammar: "c".to_string(),
                ..base.clone()
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
                ..base
            },
        );
        languages
    }

    #[test]
    fn test_detect_language_h_stays_c_without_cpp_signals() {
        // No filesystem context — plain `Path::new("foo.h")` doesn't exist,
        // so sibling scan + compile_commands walk both return false and the
        // default-config answer (`c`) survives.
        let languages = c_cpp_languages();
        assert_eq!(
            detect_language(Path::new("foo.h"), &languages),
            Some("c".to_string())
        );
    }

    #[test]
    fn test_detect_language_h_promotes_to_cpp_with_sibling_cpp_source() {
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("proj");
        std::fs::create_dir_all(&project).unwrap();
        let header = project.join("widget.h");
        std::fs::write(&header, "").unwrap();
        // Sibling .cpp source — the decisive C++ signal.
        std::fs::write(project.join("widget.cpp"), "").unwrap();

        let languages = c_cpp_languages();
        assert_eq!(
            detect_language(&header, &languages),
            Some("cpp".to_string())
        );
    }

    #[test]
    fn test_detect_language_h_promotes_to_cpp_with_sibling_hpp() {
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("proj");
        std::fs::create_dir_all(&project).unwrap();
        let header = project.join("a.h");
        std::fs::write(&header, "").unwrap();
        // A `.hpp` sibling is also a C++-specific signal.
        std::fs::write(project.join("b.hpp"), "").unwrap();

        let languages = c_cpp_languages();
        assert_eq!(
            detect_language(&header, &languages),
            Some("cpp".to_string())
        );
    }

    #[test]
    fn test_detect_language_h_promotes_to_cpp_with_ancestor_compile_commands() {
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("proj");
        let include = project.join("include").join("fmt");
        std::fs::create_dir_all(&include).unwrap();
        // Compile DB two levels above the header — the fmt-style layout.
        // Realistic CMake output: the compile command references clang++
        // and a C++ source, which is the C++ marker we key on.
        std::fs::write(
            project.join("compile_commands.json"),
            r#"[{"directory":"/proj","command":"/usr/bin/clang++ -std=c++17 -c src/format.cc","file":"src/format.cc"}]"#,
        ).unwrap();
        let header = include.join("format.h");
        std::fs::write(&header, "").unwrap();

        let languages = c_cpp_languages();
        assert_eq!(
            detect_language(&header, &languages),
            Some("cpp".to_string())
        );
    }

    #[test]
    fn test_detect_language_h_stays_c_with_pure_c_compile_commands() {
        // A compile_commands.json generated for a pure-C project (gcc,
        // -std=c11, .c sources only) must NOT promote .h to cpp.
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("cproj");
        let include = project.join("include");
        std::fs::create_dir_all(&include).unwrap();
        std::fs::write(
            project.join("compile_commands.json"),
            r#"[{"directory":"/cproj","command":"/usr/bin/gcc -std=c11 -c src/lib.c","file":"src/lib.c"}]"#,
        )
        .unwrap();
        let header = include.join("lib.h");
        std::fs::write(&header, "").unwrap();

        let languages = c_cpp_languages();
        assert_eq!(detect_language(&header, &languages), Some("c".to_string()));
    }

    #[test]
    fn test_detect_language_h_stays_c_in_pure_c_tree() {
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("cproj");
        std::fs::create_dir_all(&project).unwrap();
        let header = project.join("lib.h");
        std::fs::write(&header, "").unwrap();
        // Only `.c` siblings — no C++ signal, no compile_commands.json.
        std::fs::write(project.join("lib.c"), "").unwrap();

        let languages = c_cpp_languages();
        assert_eq!(detect_language(&header, &languages), Some("c".to_string()));
    }

    #[test]
    fn test_detect_language_h_stays_c_with_empty_compile_commands() {
        // Empty / minimal compile_commands.json carries no C++ marker,
        // so we stay conservative and leave the header as C.
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("proj");
        std::fs::create_dir_all(&project).unwrap();
        std::fs::write(project.join("compile_commands.json"), "[]").unwrap();
        let header = project.join("foo.h");
        std::fs::write(&header, "").unwrap();

        let languages = c_cpp_languages();
        assert_eq!(detect_language(&header, &languages), Some("c".to_string()));
    }

    #[test]
    fn test_detect_language_h_promotes_on_cpp_std_flag_alone() {
        // `-std=c++20` with no other C++ extension is still conclusive.
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("proj");
        let include = project.join("include");
        std::fs::create_dir_all(&include).unwrap();
        std::fs::write(
            project.join("compile_commands.json"),
            // A contrived entry using the `.C` (capital) source extension
            // with the c++20 flag — tests that the "c++" substring alone
            // is sufficient even when our `.cpp/.cc/.cxx` scan would miss.
            r#"[{"directory":"/proj","command":"/usr/bin/clang -std=c++20 -c src/x.C","file":"src/x.C"}]"#,
        )
        .unwrap();
        let header = include.join("x.h");
        std::fs::write(&header, "").unwrap();

        let languages = c_cpp_languages();
        assert_eq!(
            detect_language(&header, &languages),
            Some("cpp".to_string())
        );
    }

    #[test]
    fn test_detect_language_c_source_never_promoted() {
        // `.c` files should stay `c` even in a C++ tree.
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("proj");
        std::fs::create_dir_all(&project).unwrap();
        let source = project.join("legacy.c");
        std::fs::write(&source, "").unwrap();
        std::fs::write(project.join("main.cpp"), "").unwrap();

        let languages = c_cpp_languages();
        assert_eq!(detect_language(&source, &languages), Some("c".to_string()));
    }

    #[test]
    fn test_detect_language_h_no_promotion_without_cpp_config() {
        // If the user hasn't configured `cpp`, we have nowhere to promote to
        // — stay with the base detection rather than inventing a language.
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("proj");
        std::fs::create_dir_all(&project).unwrap();
        let header = project.join("widget.h");
        std::fs::write(&header, "").unwrap();
        std::fs::write(project.join("widget.cpp"), "").unwrap();

        let mut languages = c_cpp_languages();
        languages.remove("cpp");
        assert_eq!(detect_language(&header, &languages), Some("c".to_string()));
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
