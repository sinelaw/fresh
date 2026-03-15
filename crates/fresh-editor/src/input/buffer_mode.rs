//! Buffer mode system for buffer-local metadata
//!
//! Modes define per-buffer behavior: read-only state, text input handling,
//! and plugin attribution. Keybinding resolution is handled by KeybindingResolver.

use std::collections::HashMap;

/// A buffer mode that defines behavior for a type of buffer
#[derive(Debug, Clone)]
pub struct BufferMode {
    /// Name of this mode (e.g., "special", "diagnostics-list")
    pub name: String,

    /// Whether buffers with this mode are read-only by default
    pub read_only: bool,

    /// When true, unbound character keys in a read-only mode are dispatched as
    /// `PluginAction("mode_text_input:<char>")` instead of being silently dropped.
    /// This allows plugins to handle inline text editing (e.g. search fields)
    /// without registering individual bindings for every character.
    pub allow_text_input: bool,

    /// Name of the plugin that registered this mode (for attribution in keybinding editor)
    pub plugin_name: Option<String>,
}

impl BufferMode {
    /// Create a new buffer mode
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            read_only: false,
            allow_text_input: false,
            plugin_name: None,
        }
    }

    /// Set whether this mode is read-only by default
    pub fn with_read_only(mut self, read_only: bool) -> Self {
        self.read_only = read_only;
        self
    }

    /// Set the plugin name for attribution
    pub fn with_plugin_name(mut self, plugin_name: Option<String>) -> Self {
        self.plugin_name = plugin_name;
        self
    }

    /// Set whether unbound character keys should be dispatched as text input events
    pub fn with_allow_text_input(mut self, allow: bool) -> Self {
        self.allow_text_input = allow;
        self
    }
}

/// Registry for buffer modes — stores metadata only.
///
/// Keybinding resolution is handled by KeybindingResolver with Mode contexts.
#[derive(Debug, Clone)]
pub struct ModeRegistry {
    /// All registered modes
    modes: HashMap<String, BufferMode>,
}

impl ModeRegistry {
    /// Create a new mode registry
    pub fn new() -> Self {
        Self {
            modes: HashMap::new(),
        }
    }

    /// Register a new mode
    pub fn register(&mut self, mode: BufferMode) {
        self.modes.insert(mode.name.clone(), mode);
    }

    /// Get a mode by name
    pub fn get(&self, name: &str) -> Option<&BufferMode> {
        self.modes.get(name)
    }

    /// Check if a mode is read-only
    pub fn is_read_only(&self, mode_name: &str) -> bool {
        self.modes
            .get(mode_name)
            .map(|m| m.read_only)
            .unwrap_or(false)
    }

    /// Check if a mode allows text input passthrough
    pub fn allows_text_input(&self, mode_name: &str) -> bool {
        self.modes
            .get(mode_name)
            .map(|m| m.allow_text_input)
            .unwrap_or(false)
    }

    /// List all registered mode names
    pub fn list_modes(&self) -> Vec<String> {
        self.modes.keys().cloned().collect()
    }

    /// Check if a mode exists
    pub fn has_mode(&self, name: &str) -> bool {
        self.modes.contains_key(name)
    }
}

impl Default for ModeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mode_metadata() {
        let mut registry = ModeRegistry::new();

        let mode = BufferMode::new("test-mode")
            .with_read_only(true)
            .with_allow_text_input(true)
            .with_plugin_name(Some("test-plugin".to_string()));

        registry.register(mode);

        assert!(registry.has_mode("test-mode"));
        assert!(registry.is_read_only("test-mode"));
        assert!(registry.allows_text_input("test-mode"));
        assert_eq!(
            registry.get("test-mode").unwrap().plugin_name,
            Some("test-plugin".to_string())
        );
    }

    #[test]
    fn test_mode_defaults() {
        let registry = ModeRegistry::new();
        assert!(!registry.is_read_only("nonexistent"));
        assert!(!registry.allows_text_input("nonexistent"));
    }
}
