//! LSP Manager - manages multiple language servers using async I/O
//!
//! This module provides a manager for multiple LSP servers that:
//! - Spawns one server per language
//! - Uses async LspHandle for non-blocking I/O
//! - Routes requests to appropriate servers
//! - Configured via config.json

use crate::services::async_bridge::AsyncBridge;
use crate::services::lsp::async_handler::LspHandle;
use crate::types::LspServerConfig;
use lsp_types::{SemanticTokensLegend, Uri};
use std::collections::{HashMap, HashSet};
use std::time::{Duration, Instant};

/// Result of attempting to spawn an LSP server
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LspSpawnResult {
    /// Server was spawned or already running
    Spawned,
    /// Server is not configured for auto-start
    /// The server can still be started manually via command palette
    NotAutoStart,
    /// Server spawn failed or is disabled
    Failed,
}

/// Constants for restart behavior
const MAX_RESTARTS_IN_WINDOW: usize = 5;
const RESTART_WINDOW_SECS: u64 = 180; // 3 minutes
const RESTART_BACKOFF_BASE_MS: u64 = 1000; // 1s, 2s, 4s, 8s...

/// Manager for multiple language servers (async version)
pub struct LspManager {
    /// Map from language ID to LSP handle
    handles: HashMap<String, LspHandle>,

    /// Configuration for each language
    config: HashMap<String, LspServerConfig>,

    /// Root URI for workspace
    root_uri: Option<Uri>,

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

    /// Completion trigger characters per language (from server capabilities)
    completion_trigger_characters: HashMap<String, Vec<String>>,

    /// Semantic token legends per language (from server capabilities)
    semantic_token_legends: HashMap<String, SemanticTokensLegend>,

    /// Whether a language supports full document semantic tokens
    semantic_tokens_full_support: HashMap<String, bool>,

    /// Whether a language supports full document semantic token deltas
    semantic_tokens_full_delta_support: HashMap<String, bool>,

    /// Whether a language supports range semantic tokens
    semantic_tokens_range_support: HashMap<String, bool>,
}

impl LspManager {
    /// Create a new LSP manager
    pub fn new(root_uri: Option<Uri>) -> Self {
        Self {
            handles: HashMap::new(),
            config: HashMap::new(),
            root_uri,
            runtime: None,
            async_bridge: None,
            restart_attempts: HashMap::new(),
            restart_cooldown: HashSet::new(),
            pending_restarts: HashMap::new(),
            allowed_languages: HashSet::new(),
            disabled_languages: HashSet::new(),
            completion_trigger_characters: HashMap::new(),
            semantic_token_legends: HashMap::new(),
            semantic_tokens_full_support: HashMap::new(),
            semantic_tokens_full_delta_support: HashMap::new(),
            semantic_tokens_range_support: HashMap::new(),
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

    /// Get the configuration for a specific language
    pub fn get_config(&self, language: &str) -> Option<&LspServerConfig> {
        self.config.get(language)
    }

    /// Set completion trigger characters for a language
    pub fn set_completion_trigger_characters(&mut self, language: &str, chars: Vec<String>) {
        self.completion_trigger_characters
            .insert(language.to_string(), chars);
    }

    /// Get completion trigger characters for a language
    pub fn get_completion_trigger_characters(&self, language: &str) -> Option<&Vec<String>> {
        self.completion_trigger_characters.get(language)
    }

    /// Store semantic token capability information for a language
    pub fn set_semantic_tokens_capabilities(
        &mut self,
        language: &str,
        legend: Option<SemanticTokensLegend>,
        full_support: bool,
        full_delta_support: bool,
        range_support: bool,
    ) {
        if let Some(legend) = legend {
            self.semantic_token_legends
                .insert(language.to_string(), legend);
        } else {
            self.semantic_token_legends.remove(language);
        }
        self.semantic_tokens_full_support
            .insert(language.to_string(), full_support);
        self.semantic_tokens_full_delta_support
            .insert(language.to_string(), full_delta_support);
        self.semantic_tokens_range_support
            .insert(language.to_string(), range_support);
    }

    /// Get the semantic token legend for a language (if provided by server)
    pub fn semantic_tokens_legend(&self, language: &str) -> Option<&SemanticTokensLegend> {
        self.semantic_token_legends.get(language)
    }

    /// Check if the language supports full semantic tokens
    pub fn semantic_tokens_full_supported(&self, language: &str) -> bool {
        *self
            .semantic_tokens_full_support
            .get(language)
            .unwrap_or(&false)
    }

    /// Check if the language supports full semantic token deltas
    pub fn semantic_tokens_full_delta_supported(&self, language: &str) -> bool {
        *self
            .semantic_tokens_full_delta_support
            .get(language)
            .unwrap_or(&false)
    }

    /// Check if the language supports range semantic tokens
    pub fn semantic_tokens_range_supported(&self, language: &str) -> bool {
        *self
            .semantic_tokens_range_support
            .get(language)
            .unwrap_or(&false)
    }

    /// Check if a character is a completion trigger for any running language server
    pub fn is_completion_trigger_char(&self, ch: char, language: &str) -> bool {
        let ch_str = ch.to_string();
        self.completion_trigger_characters
            .get(language)
            .map(|chars| chars.contains(&ch_str))
            .unwrap_or(false)
    }

    /// Try to spawn an LSP server, checking auto_start configuration
    ///
    /// This is the main entry point for spawning LSP servers on file open.
    /// It returns:
    /// - `LspSpawnResult::Spawned` if the server was spawned or already running
    /// - `LspSpawnResult::NotAutoStart` if auto_start is false and not manually allowed
    /// - `LspSpawnResult::Failed` if spawn failed or language is disabled
    pub fn try_spawn(&mut self, language: &str) -> LspSpawnResult {
        // If handle already exists, return success
        if self.handles.contains_key(language) {
            return LspSpawnResult::Spawned;
        }

        // Check if language is configured and enabled
        let config = match self.config.get(language) {
            Some(c) if c.enabled => c,
            Some(_) => return LspSpawnResult::Failed, // Disabled
            None => return LspSpawnResult::Failed,    // Not configured
        };

        // Check if we have runtime and bridge
        if self.runtime.is_none() || self.async_bridge.is_none() {
            return LspSpawnResult::Failed;
        }

        // Check if auto_start is enabled or language was manually allowed
        if !config.auto_start && !self.allowed_languages.contains(language) {
            return LspSpawnResult::NotAutoStart;
        }

        // Spawn the server (using force_spawn since we've already checked auto_start)
        if self.force_spawn(language).is_some() {
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

    /// Set configuration for a language
    pub fn set_language_config(&mut self, language: String, config: LspServerConfig) {
        self.config.insert(language, config);
    }

    /// Set a new root URI for the workspace
    ///
    /// This should be called after shutting down all servers when switching projects.
    /// Servers spawned after this will use the new root URI.
    pub fn set_root_uri(&mut self, root_uri: Option<Uri>) {
        self.root_uri = root_uri;
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

    /// Get an existing LSP handle for a language (no spawning)
    ///
    /// This is the safe way to get a handle - it only returns an existing handle
    /// and never spawns a new server. Use this after calling `try_spawn()`.
    pub fn get_handle(&self, language: &str) -> Option<&LspHandle> {
        self.handles.get(language)
    }

    /// Get a mutable existing LSP handle for a language (no spawning)
    ///
    /// This is the safe way to get a handle - it only returns an existing handle
    /// and never spawns a new server. Use this after calling `try_spawn()`.
    pub fn get_handle_mut(&mut self, language: &str) -> Option<&mut LspHandle> {
        self.handles.get_mut(language)
    }

    /// Force spawn an LSP handle, bypassing auto_start checks
    ///
    /// **WARNING**: This bypasses the auto_start setting! Only use this when:
    /// - User has explicitly confirmed they want to start the LSP (via popup)
    /// - Manually restarting a server (via command palette)
    /// - Internal operations that need to guarantee spawn (like retry_crashed_servers)
    ///
    /// For normal operations, use `try_spawn()` + `get_handle_mut()` instead.
    pub fn force_spawn(&mut self, language: &str) -> Option<&mut LspHandle> {
        // Return existing handle if available
        if self.handles.contains_key(language) {
            return self.handles.get_mut(language);
        }

        // Check if language was explicitly disabled by user (via stop command)
        // Don't auto-spawn disabled languages
        if self.disabled_languages.contains(language) {
            tracing::debug!(
                "LSP for {} is disabled, not spawning (use manual restart to re-enable)",
                language
            );
            return None;
        }

        // Get config for this language
        let config = self.config.get(language)?;

        if !config.enabled {
            return None;
        }

        // Check command is specified (required when enabled)
        if config.command.is_empty() {
            return None;
        }
        let command = &config.command;

        // Check we have runtime and bridge
        let runtime = self.runtime.as_ref()?;
        let async_bridge = self.async_bridge.as_ref()?;

        // Spawn new handle
        tracing::info!("Spawning async LSP server for language: {}", language);

        match LspHandle::spawn(
            runtime,
            command,
            &config.args,
            language.to_string(),
            async_bridge,
            config.process_limits.clone(),
        ) {
            Ok(handle) => {
                // Initialize the handle (non-blocking)
                // The handle will become ready asynchronously
                if let Err(e) =
                    handle.initialize(self.root_uri.clone(), config.initialization_options.clone())
                {
                    tracing::error!("Failed to send initialize command for {}: {}", language, e);
                    return None;
                }

                tracing::info!(
                    "LSP initialization started for {}, will be ready asynchronously",
                    language
                );
                self.handles.insert(language.to_string(), handle);
                self.handles.get_mut(language)
            }
            Err(e) => {
                tracing::error!("Failed to spawn LSP handle for {}: {}", language, e);
                None
            }
        }
    }

    /// Handle a server crash by scheduling a restart with exponential backoff
    ///
    /// Returns a message describing the action taken (for UI notification)
    pub fn handle_server_crash(&mut self, language: &str) -> String {
        // Remove the crashed handle
        if let Some(handle) = self.handles.remove(language) {
            let _ = handle.shutdown(); // Best-effort cleanup
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
            if self.force_spawn(&language).is_some() {
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
    pub fn manual_restart(&mut self, language: &str) -> (bool, String) {
        // Clear any existing state
        self.clear_cooldown(language);

        // Re-enable the language (remove from disabled set)
        self.disabled_languages.remove(language);

        // Add to allowed languages so it stays active even if auto_start=false
        self.allowed_languages.insert(language.to_string());

        // Remove existing handle
        if let Some(handle) = self.handles.remove(language) {
            let _ = handle.shutdown();
        }

        // Spawn new server (bypassing auto_start for user-initiated restart)
        if self.force_spawn(language).is_some() {
            let message = format!("LSP server for {} started", language);
            tracing::info!("{}", message);
            (true, message)
        } else {
            let message = format!("Failed to start LSP server for {}", language);
            tracing::error!("{}", message);
            (false, message)
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

    /// Check if an LSP server for a language is running and ready to serve requests
    pub fn is_server_ready(&self, language: &str) -> bool {
        self.handles
            .get(language)
            .map(|handle| handle.state().can_send_requests())
            .unwrap_or(false)
    }

    /// Shutdown a specific language server
    ///
    /// This marks the server as disabled, preventing auto-restart until the user
    /// explicitly restarts it using the restart command.
    ///
    /// Returns true if the server was found and shutdown, false otherwise
    pub fn shutdown_server(&mut self, language: &str) -> bool {
        if let Some(handle) = self.handles.remove(language) {
            tracing::info!(
                "Shutting down LSP server for {} (disabled until manual restart)",
                language
            );
            let _ = handle.shutdown();
            // Mark as disabled to prevent auto-restart
            self.disabled_languages.insert(language.to_string());
            // Cancel any pending restarts
            self.pending_restarts.remove(language);
            // Remove from restart cooldown
            self.restart_cooldown.remove(language);
            // Also remove from allowed languages so it will require confirmation again
            // if user tries to start it later
            self.allowed_languages.remove(language);
            true
        } else {
            tracing::warn!("No running LSP server found for {}", language);
            false
        }
    }

    /// Shutdown all language servers
    pub fn shutdown_all(&mut self) {
        for (language, handle) in self.handles.iter() {
            tracing::info!("Shutting down LSP server for {}", language);
            let _ = handle.shutdown();
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
/// Checks in order:
/// 1. File extension against `extensions`
/// 2. Exact filename against `filenames`
pub fn detect_language(
    path: &std::path::Path,
    languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
) -> Option<String> {
    // Try extension first
    if let Some(extension) = path.extension().and_then(|e| e.to_str()) {
        for (language_name, lang_config) in languages {
            if lang_config.extensions.iter().any(|ext| ext == extension) {
                return Some(language_name.clone());
            }
        }
    }

    // Try exact filename match
    if let Some(filename) = path.file_name().and_then(|f| f.to_str()) {
        for (language_name, lang_config) in languages {
            if lang_config.filenames.iter().any(|f| f == filename) {
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
        };

        manager.set_language_config("rust".to_string(), config);

        assert_eq!(manager.config.len(), 1);
        assert!(manager.config.contains_key("rust"));
        assert!(manager.config.get("rust").unwrap().enabled);
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
            },
        );

        // force_spawn should return None without runtime
        let result = manager.force_spawn("rust");
        assert!(result.is_none());
    }

    #[test]
    fn test_lsp_manager_force_spawn_no_config() {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let mut manager = LspManager::new(None);
        let async_bridge = AsyncBridge::new();

        manager.set_runtime(rt.handle().clone(), async_bridge);

        // force_spawn should return None for unconfigured language
        let result = manager.force_spawn("rust");
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
            },
        );

        // force_spawn should return None for disabled language
        let result = manager.force_spawn("rust");
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
                highlighter: crate::config::HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: false,
                use_tabs: false,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
                highlighter: crate::config::HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: false,
                use_tabs: false,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
                highlighter: crate::config::HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: false,
                use_tabs: false,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
}
