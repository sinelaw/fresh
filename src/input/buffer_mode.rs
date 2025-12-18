//! Buffer mode system for buffer-local keybindings
//!
//! This module implements an Emacs-style major mode system where each buffer
//! can have its own mode that defines keybindings. Modes support inheritance,
//! allowing derived modes to extend parent modes.

use crossterm::event::{KeyCode, KeyModifiers};
use std::collections::HashMap;

/// A buffer mode that defines keybindings and behavior for a type of buffer
#[derive(Debug, Clone)]
pub struct BufferMode {
    /// Name of this mode (e.g., "special", "diagnostics-list")
    pub name: String,

    /// Parent mode name for inheritance (e.g., "special" is parent of "diagnostics-list")
    pub parent: Option<String>,

    /// Keybindings specific to this mode (key → command name)
    pub keybindings: HashMap<(KeyCode, KeyModifiers), String>,

    /// Whether buffers with this mode are read-only by default
    pub read_only: bool,
}

impl BufferMode {
    /// Create a new buffer mode
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            parent: None,
            keybindings: HashMap::new(),
            read_only: false,
        }
    }

    /// Set the parent mode for inheritance
    pub fn with_parent(mut self, parent: impl Into<String>) -> Self {
        self.parent = Some(parent.into());
        self
    }

    /// Add a keybinding to this mode
    pub fn with_binding(
        mut self,
        code: KeyCode,
        modifiers: KeyModifiers,
        command: impl Into<String>,
    ) -> Self {
        self.keybindings.insert((code, modifiers), command.into());
        self
    }

    /// Set whether this mode is read-only by default
    pub fn with_read_only(mut self, read_only: bool) -> Self {
        self.read_only = read_only;
        self
    }

    /// Add multiple keybindings at once
    pub fn with_bindings(mut self, bindings: Vec<(KeyCode, KeyModifiers, String)>) -> Self {
        for (code, modifiers, command) in bindings {
            self.keybindings.insert((code, modifiers), command);
        }
        self
    }
}

/// Registry for buffer modes
///
/// Manages all available modes and provides lookup functionality with inheritance.
#[derive(Debug, Clone)]
pub struct ModeRegistry {
    /// All registered modes
    modes: HashMap<String, BufferMode>,
}

impl ModeRegistry {
    /// Create a new mode registry with built-in modes
    pub fn new() -> Self {
        let mut registry = Self {
            modes: HashMap::new(),
        };

        // Register built-in "special" mode (base for all special buffers)
        // This is like Emacs' special-mode
        // Keybindings map to Action names (see Action::from_str)
        let special_mode = BufferMode::new("special")
            .with_read_only(true)
            .with_binding(KeyCode::Char('q'), KeyModifiers::NONE, "close")
            .with_binding(KeyCode::Char('g'), KeyModifiers::NONE, "revert");

        registry.register(special_mode);

        registry
    }

    /// Register a new mode
    pub fn register(&mut self, mode: BufferMode) {
        self.modes.insert(mode.name.clone(), mode);
    }

    /// Get a mode by name
    pub fn get(&self, name: &str) -> Option<&BufferMode> {
        self.modes.get(name)
    }

    /// Resolve a keybinding for a mode, following inheritance chain
    ///
    /// Returns the command name if a binding is found in this mode or any parent.
    pub fn resolve_keybinding(
        &self,
        mode_name: &str,
        code: KeyCode,
        modifiers: KeyModifiers,
    ) -> Option<String> {
        let mut current_mode_name = Some(mode_name);

        // Walk up the inheritance chain
        while let Some(name) = current_mode_name {
            if let Some(mode) = self.modes.get(name) {
                // Check if this mode has the keybinding
                if let Some(command) = mode.keybindings.get(&(code, modifiers)) {
                    return Some(command.clone());
                }

                // Move to parent mode
                current_mode_name = mode.parent.as_deref();
            } else {
                // Mode not found, stop searching
                break;
            }
        }

        None
    }

    /// Check if a mode is read-only (checking inheritance)
    pub fn is_read_only(&self, mode_name: &str) -> bool {
        let mut current_mode_name = Some(mode_name);

        // Walk up the inheritance chain
        while let Some(name) = current_mode_name {
            if let Some(mode) = self.modes.get(name) {
                if mode.read_only {
                    return true;
                }
                current_mode_name = mode.parent.as_deref();
            } else {
                break;
            }
        }

        false
    }

    /// List all registered mode names
    pub fn list_modes(&self) -> Vec<String> {
        self.modes.keys().cloned().collect()
    }

    /// Check if a mode exists
    pub fn has_mode(&self, name: &str) -> bool {
        self.modes.contains_key(name)
    }

    /// Get all keybindings for a mode (including inherited ones)
    ///
    /// Returns bindings from most specific (this mode) to least specific (root parent).
    /// Later bindings override earlier ones.
    pub fn get_all_keybindings(&self, mode_name: &str) -> HashMap<(KeyCode, KeyModifiers), String> {
        let mut result = HashMap::new();
        let mut chain = Vec::new();

        // Build inheritance chain (root first)
        let mut current = Some(mode_name);
        while let Some(name) = current {
            if let Some(mode) = self.modes.get(name) {
                chain.push(mode);
                current = mode.parent.as_deref();
            } else {
                break;
            }
        }

        // Apply bindings from root to leaf (so leaf overrides)
        for mode in chain.into_iter().rev() {
            result.extend(mode.keybindings.clone());
        }

        result
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
    fn test_special_mode_exists() {
        let registry = ModeRegistry::new();
        assert!(registry.has_mode("special"));
    }

    #[test]
    fn test_special_mode_keybindings() {
        let registry = ModeRegistry::new();
        let mode = registry.get("special").unwrap();

        assert_eq!(
            mode.keybindings
                .get(&(KeyCode::Char('q'), KeyModifiers::NONE)),
            Some(&"close".to_string())
        );
        assert_eq!(
            mode.keybindings
                .get(&(KeyCode::Char('g'), KeyModifiers::NONE)),
            Some(&"revert".to_string())
        );
    }

    #[test]
    fn test_mode_inheritance() {
        let mut registry = ModeRegistry::new();

        // Create a child mode that inherits from special
        let diagnostics_mode = BufferMode::new("diagnostics-list")
            .with_parent("special")
            .with_binding(KeyCode::Enter, KeyModifiers::NONE, "diagnostics:goto")
            .with_binding(KeyCode::Char('n'), KeyModifiers::NONE, "next-line");

        registry.register(diagnostics_mode);

        // Should find direct binding
        assert_eq!(
            registry.resolve_keybinding("diagnostics-list", KeyCode::Enter, KeyModifiers::NONE),
            Some("diagnostics:goto".to_string())
        );

        // Should find inherited binding from special mode
        assert_eq!(
            registry.resolve_keybinding("diagnostics-list", KeyCode::Char('q'), KeyModifiers::NONE),
            Some("close".to_string())
        );

        // Should not find non-existent binding
        assert_eq!(
            registry.resolve_keybinding("diagnostics-list", KeyCode::Char('x'), KeyModifiers::NONE),
            None
        );
    }

    #[test]
    fn test_mode_read_only_inheritance() {
        let mut registry = ModeRegistry::new();

        // Special mode is read-only
        assert!(registry.is_read_only("special"));

        // Child mode inherits read-only
        let child_mode = BufferMode::new("child").with_parent("special");
        registry.register(child_mode);
        assert!(registry.is_read_only("child"));

        // Non-special mode is not read-only
        let editable_mode = BufferMode::new("editable");
        registry.register(editable_mode);
        assert!(!registry.is_read_only("editable"));
    }

    #[test]
    fn test_get_all_keybindings() {
        let mut registry = ModeRegistry::new();

        let child_mode = BufferMode::new("child")
            .with_parent("special")
            .with_binding(KeyCode::Enter, KeyModifiers::NONE, "child:action")
            // Override parent binding
            .with_binding(KeyCode::Char('q'), KeyModifiers::NONE, "child:quit");

        registry.register(child_mode);

        let all_bindings = registry.get_all_keybindings("child");

        // Should have overridden 'q'
        assert_eq!(
            all_bindings.get(&(KeyCode::Char('q'), KeyModifiers::NONE)),
            Some(&"child:quit".to_string())
        );

        // Should have inherited 'g'
        assert_eq!(
            all_bindings.get(&(KeyCode::Char('g'), KeyModifiers::NONE)),
            Some(&"revert".to_string())
        );

        // Should have child-specific binding
        assert_eq!(
            all_bindings.get(&(KeyCode::Enter, KeyModifiers::NONE)),
            Some(&"child:action".to_string())
        );
    }
}
