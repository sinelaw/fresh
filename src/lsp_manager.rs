//! LSP Manager - manages multiple language servers using async I/O
//!
//! This module provides a manager for multiple LSP servers that:
//! - Spawns one server per language
//! - Uses async LspHandle for non-blocking I/O
//! - Routes requests to appropriate servers
//! - Configured via config.json

use crate::async_bridge::AsyncBridge;
use crate::lsp::LspServerConfig;
use crate::lsp_async::LspHandle;
use lsp_types::Uri;
use std::collections::{HashMap, HashSet};
use std::time::{Duration, Instant};

/// Result of attempting to spawn an LSP server
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LspSpawnResult {
    /// Server was spawned or already running
    Spawned,
    /// User confirmation is required before spawning
    /// Contains the language name that needs confirmation
    NeedsConfirmation(String),
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

    /// Languages that have been approved for auto-spawning by the user
    /// If a language is in this set, it will spawn without confirmation
    allowed_languages: HashSet<String>,

    /// Whether to require confirmation before spawning LSP servers
    /// When true, LSP servers won't auto-spawn until user approves
    require_confirmation: bool,

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
            runtime: None,
            async_bridge: None,
            restart_attempts: HashMap::new(),
            restart_cooldown: HashSet::new(),
            pending_restarts: HashMap::new(),
            allowed_languages: HashSet::new(),
            require_confirmation: true, // Require confirmation by default
            disabled_languages: HashSet::new(),
        }
    }

    /// Enable or disable LSP spawn confirmation requirement
    pub fn set_require_confirmation(&mut self, require: bool) {
        self.require_confirmation = require;
    }

    /// Check if LSP spawn confirmation is required
    pub fn requires_confirmation(&self) -> bool {
        self.require_confirmation
    }

    /// Check if a language is allowed to spawn without confirmation
    pub fn is_language_allowed(&self, language: &str) -> bool {
        !self.require_confirmation || self.allowed_languages.contains(language)
    }

    /// Allow a language to spawn LSP server (permanently for this session)
    pub fn allow_language(&mut self, language: &str) {
        self.allowed_languages.insert(language.to_string());
        tracing::info!("LSP language '{}' allowed for auto-spawn", language);
    }

    /// Get the set of allowed languages
    pub fn allowed_languages(&self) -> &HashSet<String> {
        &self.allowed_languages
    }

    /// Get the configuration for a specific language
    pub fn get_config(&self, language: &str) -> Option<&LspServerConfig> {
        self.config.get(language)
    }

    /// Try to spawn an LSP server, returning whether confirmation is needed
    ///
    /// This is the main entry point for spawning LSP servers when confirmation
    /// might be required. It returns:
    /// - `LspSpawnResult::Spawned` if the server was spawned or already running
    /// - `LspSpawnResult::NeedsConfirmation(language)` if user confirmation is needed
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

        // Check if confirmation is required
        if self.require_confirmation && !self.allowed_languages.contains(language) {
            return LspSpawnResult::NeedsConfirmation(language.to_string());
        }

        // Spawn the server
        if self.get_or_spawn(language).is_some() {
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

    /// Get or spawn an LSP handle for a language
    pub fn get_or_spawn(&mut self, language: &str) -> Option<&mut LspHandle> {
        // Return existing handle if available
        if self.handles.contains_key(language) {
            return self.handles.get_mut(language);
        }

        // Get config for this language
        let config = self.config.get(language)?;

        if !config.enabled {
            return None;
        }

        // Check we have runtime and bridge
        let runtime = self.runtime.as_ref()?;
        let async_bridge = self.async_bridge.as_ref()?;

        // Spawn new handle
        tracing::info!("Spawning async LSP server for language: {}", language);

        match LspHandle::spawn(
            runtime,
            &config.command,
            &config.args,
            language.to_string(),
            async_bridge,
            config.process_limits.clone(),
        ) {
            Ok(handle) => {
                // Initialize the handle (non-blocking)
                // The handle will become ready asynchronously
                if let Err(e) = handle.initialize(self.root_uri.clone()) {
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

            // Attempt to spawn the server
            if self.get_or_spawn(&language).is_some() {
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

    /// Manually restart a language server (bypasses cooldown and re-enables auto-restart)
    ///
    /// Returns (success, message) tuple
    pub fn manual_restart(&mut self, language: &str) -> (bool, String) {
        // Clear any existing state
        self.clear_cooldown(language);

        // Re-enable the language (remove from disabled set)
        self.disabled_languages.remove(language);

        // Remove existing handle
        if let Some(handle) = self.handles.remove(language) {
            let _ = handle.shutdown();
        }

        // Spawn new server
        if self.get_or_spawn(language).is_some() {
            let message = format!("LSP server for {} restarted manually", language);
            tracing::info!("{}", message);
            (true, message)
        } else {
            let message = format!("Failed to manually restart LSP server for {}", language);
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

    /// Shutdown a specific language server
    ///
    /// This marks the server as disabled, preventing auto-restart until the user
    /// explicitly restarts it using the restart command.
    ///
    /// Returns true if the server was found and shutdown, false otherwise
    pub fn shutdown_server(&mut self, language: &str) -> bool {
        if let Some(handle) = self.handles.remove(language) {
            tracing::info!("Shutting down LSP server for {} (disabled until manual restart)", language);
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

/// Helper function to detect language from file extension
pub fn detect_language(path: &std::path::Path) -> Option<String> {
    let extension = path.extension()?.to_str()?;

    match extension {
        "rs" => Some("rust".to_string()),
        "js" | "jsx" | "mjs" | "cjs" => Some("javascript".to_string()),
        "ts" | "tsx" => Some("typescript".to_string()),
        "py" | "pyi" => Some("python".to_string()),
        "json" => Some("json".to_string()),
        "md" => Some("markdown".to_string()),
        "toml" => Some("toml".to_string()),
        "yaml" | "yml" => Some("yaml".to_string()),
        "html" => Some("html".to_string()),
        "css" => Some("css".to_string()),
        "go" => Some("go".to_string()),
        "c" | "h" => Some("c".to_string()),
        "cpp" | "cc" | "cxx" | "hpp" | "hxx" => Some("cpp".to_string()),
        _ => None,
    }
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
            process_limits: crate::process_limits::ProcessLimits::unlimited(),
        };

        manager.set_language_config("rust".to_string(), config);

        assert_eq!(manager.config.len(), 1);
        assert!(manager.config.contains_key("rust"));
        assert!(manager.config.get("rust").unwrap().enabled);
    }

    #[test]
    fn test_lsp_manager_get_or_spawn_no_runtime() {
        let mut manager = LspManager::new(None);

        // Add config for rust
        manager.set_language_config(
            "rust".to_string(),
            LspServerConfig {
                enabled: true,
                command: "rust-analyzer".to_string(),
                args: vec![],
                process_limits: crate::process_limits::ProcessLimits::unlimited(),
            },
        );

        // get_or_spawn should return None without runtime
        let result = manager.get_or_spawn("rust");
        assert!(result.is_none());
    }

    #[test]
    fn test_lsp_manager_get_or_spawn_no_config() {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let mut manager = LspManager::new(None);
        let async_bridge = AsyncBridge::new();

        manager.set_runtime(rt.handle().clone(), async_bridge);

        // get_or_spawn should return None for unconfigured language
        let result = manager.get_or_spawn("rust");
        assert!(result.is_none());
    }

    #[test]
    fn test_lsp_manager_get_or_spawn_disabled_language() {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let mut manager = LspManager::new(None);
        let async_bridge = AsyncBridge::new();

        manager.set_runtime(rt.handle().clone(), async_bridge);

        // Add disabled config
        manager.set_language_config(
            "rust".to_string(),
            LspServerConfig {
                enabled: false,
                command: "rust-analyzer".to_string(),
                args: vec![],
                process_limits: crate::process_limits::ProcessLimits::unlimited(),
            },
        );

        // get_or_spawn should return None for disabled language
        let result = manager.get_or_spawn("rust");
        assert!(result.is_none());
    }

    #[test]
    fn test_lsp_manager_shutdown_all() {
        let mut manager = LspManager::new(None);

        // shutdown_all should not panic even with no handles
        manager.shutdown_all();
        assert_eq!(manager.handles.len(), 0);
    }

    #[test]
    fn test_detect_language_rust() {
        assert_eq!(
            detect_language(Path::new("main.rs")),
            Some("rust".to_string())
        );
        assert_eq!(
            detect_language(Path::new("lib.rs")),
            Some("rust".to_string())
        );
        assert_eq!(
            detect_language(Path::new("/path/to/file.rs")),
            Some("rust".to_string())
        );
    }

    #[test]
    fn test_detect_language_javascript() {
        assert_eq!(
            detect_language(Path::new("index.js")),
            Some("javascript".to_string())
        );
        assert_eq!(
            detect_language(Path::new("App.jsx")),
            Some("javascript".to_string())
        );
        assert_eq!(
            detect_language(Path::new("module.mjs")),
            Some("javascript".to_string())
        );
        assert_eq!(
            detect_language(Path::new("config.cjs")),
            Some("javascript".to_string())
        );
    }

    #[test]
    fn test_detect_language_typescript() {
        assert_eq!(
            detect_language(Path::new("index.ts")),
            Some("typescript".to_string())
        );
        assert_eq!(
            detect_language(Path::new("App.tsx")),
            Some("typescript".to_string())
        );
    }

    #[test]
    fn test_detect_language_python() {
        assert_eq!(
            detect_language(Path::new("main.py")),
            Some("python".to_string())
        );
        assert_eq!(
            detect_language(Path::new("types.pyi")),
            Some("python".to_string())
        );
    }

    #[test]
    fn test_detect_language_c_cpp() {
        assert_eq!(detect_language(Path::new("main.c")), Some("c".to_string()));
        assert_eq!(
            detect_language(Path::new("header.h")),
            Some("c".to_string())
        );
        assert_eq!(
            detect_language(Path::new("main.cpp")),
            Some("cpp".to_string())
        );
        assert_eq!(
            detect_language(Path::new("main.cc")),
            Some("cpp".to_string())
        );
        assert_eq!(
            detect_language(Path::new("main.cxx")),
            Some("cpp".to_string())
        );
        assert_eq!(
            detect_language(Path::new("header.hpp")),
            Some("cpp".to_string())
        );
        assert_eq!(
            detect_language(Path::new("header.hxx")),
            Some("cpp".to_string())
        );
    }

    #[test]
    fn test_detect_language_markup() {
        assert_eq!(
            detect_language(Path::new("README.md")),
            Some("markdown".to_string())
        );
        assert_eq!(
            detect_language(Path::new("index.html")),
            Some("html".to_string())
        );
        assert_eq!(
            detect_language(Path::new("styles.css")),
            Some("css".to_string())
        );
    }

    #[test]
    fn test_detect_language_config_files() {
        assert_eq!(
            detect_language(Path::new("Cargo.toml")),
            Some("toml".to_string())
        );
        assert_eq!(
            detect_language(Path::new("config.yaml")),
            Some("yaml".to_string())
        );
        assert_eq!(
            detect_language(Path::new("config.yml")),
            Some("yaml".to_string())
        );
        assert_eq!(
            detect_language(Path::new("package.json")),
            Some("json".to_string())
        );
    }

    #[test]
    fn test_detect_language_go() {
        assert_eq!(
            detect_language(Path::new("main.go")),
            Some("go".to_string())
        );
    }

    #[test]
    fn test_detect_language_unknown() {
        assert_eq!(detect_language(Path::new("file.xyz")), None);
        assert_eq!(detect_language(Path::new("file.unknown")), None);
        assert_eq!(detect_language(Path::new("file")), None); // No extension
    }

    #[test]
    fn test_detect_language_no_extension() {
        assert_eq!(detect_language(Path::new("README")), None);
        assert_eq!(detect_language(Path::new("Makefile")), None);
    }
}
