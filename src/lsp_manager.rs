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
use std::collections::HashMap;

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
