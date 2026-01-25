//! Quick Open Provider System
//!
//! A unified prompt system with prefix-based routing to different providers.
//! Inspired by VSCode's Quick Open (Ctrl+P) which supports:
//! - Empty prefix: file finder
//! - `>`: command palette
//! - `#`: symbol finder (future)
//! - `@`: go to symbol in file (future)
//! - `:`: go to line
//!
//! Providers are registered with a prefix and handle suggestion generation
//! and selection for their domain.

pub mod providers;

pub use providers::{BufferProvider, CommandProvider, FileProvider, GotoLineProvider};

use crate::input::commands::Suggestion;
use crate::input::keybindings::Action;
use std::collections::HashMap;

/// Result of confirming a selection in a provider
#[derive(Debug, Clone)]
pub enum QuickOpenResult {
    /// Execute an editor action
    ExecuteAction(Action),
    /// Open a file at optional line/column
    OpenFile {
        path: String,
        line: Option<usize>,
        column: Option<usize>,
    },
    /// Show a buffer by ID
    ShowBuffer(usize),
    /// Go to a line in the current buffer
    GotoLine(usize),
    /// Do nothing (provider handled it internally)
    None,
    /// Show an error message
    Error(String),
}

/// Context provided to providers when generating suggestions
#[derive(Debug, Clone)]
pub struct QuickOpenContext {
    /// Current working directory
    pub cwd: String,
    /// List of open buffer paths
    pub open_buffers: Vec<BufferInfo>,
    /// Active buffer ID
    pub active_buffer_id: usize,
    /// Active buffer path (if any)
    pub active_buffer_path: Option<String>,
    /// Whether there's an active selection
    pub has_selection: bool,
    /// Current key context
    pub key_context: crate::input::keybindings::KeyContext,
    /// Active custom contexts (for command filtering)
    pub custom_contexts: std::collections::HashSet<String>,
    /// Active buffer mode (e.g., "vi_normal")
    pub buffer_mode: Option<String>,
}

/// Information about an open buffer
#[derive(Debug, Clone)]
pub struct BufferInfo {
    pub id: usize,
    pub path: String,
    pub name: String,
    pub modified: bool,
}

/// Trait for quick open providers
///
/// Each provider handles a specific prefix and provides suggestions
/// for that domain (files, commands, buffers, etc.)
pub trait QuickOpenProvider: Send + Sync {
    /// The prefix that triggers this provider (e.g., ">", "#", ":")
    /// Empty string means this is the default provider (no prefix)
    fn prefix(&self) -> &str;

    /// Human-readable name for this provider
    fn name(&self) -> &str;

    /// Short hint shown in the status bar (e.g., ">  Commands")
    fn hint(&self) -> &str;

    /// Generate suggestions for the given query
    ///
    /// The query has already had the prefix stripped.
    fn suggestions(&self, query: &str, context: &QuickOpenContext) -> Vec<Suggestion>;

    /// Handle selection of a suggestion
    ///
    /// `selected_index` is the index into the suggestions array returned by `suggestions()`.
    /// `query` is the original query (without prefix).
    fn on_select(
        &self,
        selected_index: Option<usize>,
        query: &str,
        context: &QuickOpenContext,
    ) -> QuickOpenResult;

    /// Optional: provide a preview for the selected suggestion
    /// Returns a file path and optional line number for preview
    fn preview(
        &self,
        _selected_index: usize,
        _context: &QuickOpenContext,
    ) -> Option<(String, Option<usize>)> {
        None
    }
}

/// Registry for quick open providers
pub struct QuickOpenRegistry {
    /// Providers indexed by their prefix
    providers: HashMap<String, Box<dyn QuickOpenProvider>>,
    /// Ordered list of prefixes for hint display
    prefix_order: Vec<String>,
}

impl QuickOpenRegistry {
    pub fn new() -> Self {
        Self {
            providers: HashMap::new(),
            prefix_order: Vec::new(),
        }
    }

    /// Register a provider
    ///
    /// If a provider with the same prefix exists, it will be replaced.
    pub fn register(&mut self, provider: Box<dyn QuickOpenProvider>) {
        let prefix = provider.prefix().to_string();
        if !self.prefix_order.contains(&prefix) {
            self.prefix_order.push(prefix.clone());
        }
        self.providers.insert(prefix, provider);
    }

    /// Get the provider for a given input
    ///
    /// Returns (provider, query_without_prefix)
    pub fn get_provider_for_input<'a>(
        &'a self,
        input: &'a str,
    ) -> Option<(&'a dyn QuickOpenProvider, &'a str)> {
        // Try prefixes in order (longest first to handle overlapping prefixes)
        let mut prefixes: Vec<_> = self.providers.keys().collect();
        prefixes.sort_by(|a, b| b.len().cmp(&a.len()));

        for prefix in prefixes {
            if prefix.is_empty() {
                continue; // Handle default provider last
            }
            if input.starts_with(prefix.as_str()) {
                let query = &input[prefix.len()..];
                return self.providers.get(prefix).map(|p| (p.as_ref(), query));
            }
        }

        // Fall back to default provider (empty prefix)
        self.providers.get("").map(|p| (p.as_ref(), input))
    }

    /// Get the default provider (empty prefix)
    pub fn get_default_provider(&self) -> Option<&dyn QuickOpenProvider> {
        self.providers.get("").map(|p| p.as_ref())
    }

    /// Get hints string for status bar display
    pub fn get_hints(&self) -> String {
        self.prefix_order
            .iter()
            .filter_map(|prefix| self.providers.get(prefix).map(|p| p.hint()))
            .collect::<Vec<_>>()
            .join("   ")
    }

    /// Get all registered prefixes
    pub fn prefixes(&self) -> Vec<&str> {
        self.providers.keys().map(|s| s.as_str()).collect()
    }
}

impl Default for QuickOpenRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestProvider {
        prefix: String,
    }

    impl QuickOpenProvider for TestProvider {
        fn prefix(&self) -> &str {
            &self.prefix
        }

        fn name(&self) -> &str {
            "Test"
        }

        fn hint(&self) -> &str {
            "Test hint"
        }

        fn suggestions(&self, _query: &str, _context: &QuickOpenContext) -> Vec<Suggestion> {
            vec![]
        }

        fn on_select(
            &self,
            _selected_index: Option<usize>,
            _query: &str,
            _context: &QuickOpenContext,
        ) -> QuickOpenResult {
            QuickOpenResult::None
        }
    }

    #[test]
    fn test_provider_routing() {
        let mut registry = QuickOpenRegistry::new();

        registry.register(Box::new(TestProvider {
            prefix: "".to_string(),
        }));
        registry.register(Box::new(TestProvider {
            prefix: ">".to_string(),
        }));
        registry.register(Box::new(TestProvider {
            prefix: "#".to_string(),
        }));

        // Default provider for no prefix
        let (provider, query) = registry.get_provider_for_input("hello").unwrap();
        assert_eq!(provider.prefix(), "");
        assert_eq!(query, "hello");

        // Command provider
        let (provider, query) = registry.get_provider_for_input(">save").unwrap();
        assert_eq!(provider.prefix(), ">");
        assert_eq!(query, "save");

        // Buffer provider
        let (provider, query) = registry.get_provider_for_input("#main").unwrap();
        assert_eq!(provider.prefix(), "#");
        assert_eq!(query, "main");
    }
}
