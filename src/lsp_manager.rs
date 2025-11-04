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
use lsp_types::{TextDocumentContentChangeEvent, Url};
use std::collections::HashMap;

/// Manager for multiple language servers (async version)
pub struct LspManager {
    /// Map from language ID to LSP handle
    handles: HashMap<String, LspHandle>,

    /// Configuration for each language
    config: HashMap<String, LspServerConfig>,

    /// Root URI for workspace
    root_uri: Option<Url>,

    /// Tokio runtime reference
    runtime: Option<tokio::runtime::Handle>,

    /// Async bridge for communication
    async_bridge: Option<AsyncBridge>,
}

impl LspManager {
    /// Create a new LSP manager
    pub fn new(root_uri: Option<Url>) -> Self {
        Self {
            handles: HashMap::new(),
            config: HashMap::new(),
            root_uri,
            runtime: None,
            async_bridge: None,
        }
    }

    /// Set the Tokio runtime and async bridge
    ///
    /// Must be called before spawning any servers
    pub fn set_runtime(
        &mut self,
        runtime: tokio::runtime::Handle,
        async_bridge: AsyncBridge,
    ) {
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
        ) {
            Ok(handle) => {
                // Initialize the handle
                match handle.initialize(self.root_uri.clone()) {
                    Ok(_) => {
                        self.handles.insert(language.to_string(), handle);
                        self.handles.get_mut(language)
                    }
                    Err(e) => {
                        tracing::error!("Failed to initialize LSP server for {}: {}", language, e);
                        None
                    }
                }
            }
            Err(e) => {
                tracing::error!("Failed to spawn LSP handle for {}: {}", language, e);
                None
            }
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
