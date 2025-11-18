//! Dynamic command registry for plugins and extensions
//!
//! This module allows plugins to register custom commands dynamically
//! while maintaining the built-in command set.

use crate::commands::{get_all_commands, Command, Suggestion};
use crate::keybindings::KeyContext;
use std::sync::{Arc, RwLock};

/// Registry for managing editor commands
///
/// Supports both built-in commands and dynamically registered plugin commands.
/// Thread-safe for use across multiple threads (e.g., from async tasks).
pub struct CommandRegistry {
    /// Built-in commands (loaded once at startup)
    builtin_commands: Vec<Command>,

    /// Plugin-registered commands (dynamically added/removed)
    plugin_commands: Arc<RwLock<Vec<Command>>>,

    /// Command usage history (most recent first)
    /// Used to sort command palette suggestions by recency
    command_history: Vec<String>,
}

impl CommandRegistry {
    /// Maximum number of commands to keep in history
    const MAX_HISTORY_SIZE: usize = 50;

    /// Create a new command registry with built-in commands
    pub fn new() -> Self {
        Self {
            builtin_commands: get_all_commands(),
            plugin_commands: Arc::new(RwLock::new(Vec::new())),
            command_history: Vec::new(),
        }
    }

    /// Record that a command was used (for history/sorting)
    ///
    /// This moves the command to the front of the history list.
    /// Recently used commands appear first in suggestions.
    pub fn record_usage(&mut self, command_name: &str) {
        // Remove existing entry if present
        self.command_history.retain(|name| name != command_name);

        // Add to front (most recent)
        self.command_history.insert(0, command_name.to_string());

        // Trim to max size
        if self.command_history.len() > Self::MAX_HISTORY_SIZE {
            self.command_history.truncate(Self::MAX_HISTORY_SIZE);
        }
    }

    /// Get the position of a command in history (0 = most recent)
    /// Returns None if command is not in history
    fn history_position(&self, command_name: &str) -> Option<usize> {
        self.command_history
            .iter()
            .position(|name| name == command_name)
    }

    /// Register a new command (typically from a plugin)
    ///
    /// If a command with the same name already exists, it will be replaced.
    /// This allows plugins to override built-in commands.
    pub fn register(&self, command: Command) {
        let mut commands = self.plugin_commands.write().unwrap();

        // Remove existing command with same name
        commands.retain(|c| c.name != command.name);

        // Add new command
        commands.push(command);
    }

    /// Unregister a command by name
    pub fn unregister(&self, name: &str) {
        let mut commands = self.plugin_commands.write().unwrap();
        commands.retain(|c| c.name != name);
    }

    /// Unregister all commands registered by a specific plugin
    pub fn unregister_by_prefix(&self, prefix: &str) {
        let mut commands = self.plugin_commands.write().unwrap();
        commands.retain(|c| !c.name.starts_with(prefix));
    }

    /// Get all commands (built-in + plugin)
    pub fn get_all(&self) -> Vec<Command> {
        let mut all_commands = self.builtin_commands.clone();

        let plugin_commands = self.plugin_commands.read().unwrap();
        all_commands.extend(plugin_commands.iter().cloned());

        all_commands
    }

    /// Filter commands by fuzzy matching query with context awareness
    ///
    /// When query is empty, commands are sorted by recency (most recently used first).
    /// When query is not empty, commands are sorted by match quality with recency as tiebreaker.
    /// Disabled commands always appear after enabled ones.
    pub fn filter(
        &self,
        query: &str,
        current_context: KeyContext,
        keybinding_resolver: &crate::keybindings::KeybindingResolver,
    ) -> Vec<Suggestion> {
        let query_lower = query.to_lowercase();
        let commands = self.get_all();

        // Helper function to check if command is available in current context
        let is_available = |cmd: &Command| -> bool {
            // Empty contexts means available in all contexts
            cmd.contexts.is_empty() || cmd.contexts.contains(&current_context)
        };

        // Helper function for fuzzy matching
        let matches_query = |cmd: &Command| -> bool {
            if query.is_empty() {
                return true;
            }

            let name_lower = cmd.name.to_lowercase();
            let mut query_chars = query_lower.chars();
            let mut current_char = query_chars.next();

            for name_char in name_lower.chars() {
                if let Some(qc) = current_char {
                    if qc == name_char {
                        current_char = query_chars.next();
                    }
                } else {
                    break;
                }
            }

            current_char.is_none() // All query characters matched
        };

        // Filter and convert to suggestions with history position
        let mut suggestions: Vec<(Suggestion, Option<usize>)> = commands
            .into_iter()
            .filter(|cmd| matches_query(cmd))
            .map(|cmd| {
                let available = is_available(&cmd);
                let keybinding =
                    keybinding_resolver.get_keybinding_for_action(&cmd.action, current_context);
                let history_pos = self.history_position(&cmd.name);
                let suggestion = Suggestion::with_all(
                    cmd.name.clone(),
                    Some(cmd.description),
                    !available,
                    keybinding,
                );
                (suggestion, history_pos)
            })
            .collect();

        // Sort by:
        // 1. Disabled status (enabled first)
        // 2. History position (recent first, then never-used alphabetically)
        suggestions.sort_by(|(a, a_hist), (b, b_hist)| {
            // First sort by disabled status
            match a.disabled.cmp(&b.disabled) {
                std::cmp::Ordering::Equal => {}
                other => return other,
            }

            // Then sort by history position (lower = more recent = better)
            match (a_hist, b_hist) {
                (Some(a_pos), Some(b_pos)) => a_pos.cmp(b_pos),
                (Some(_), None) => std::cmp::Ordering::Less, // In history beats not in history
                (None, Some(_)) => std::cmp::Ordering::Greater,
                (None, None) => a.text.cmp(&b.text), // Alphabetical for never-used commands
            }
        });

        // Extract just the suggestions
        suggestions.into_iter().map(|(s, _)| s).collect()
    }

    /// Get count of registered plugin commands
    pub fn plugin_command_count(&self) -> usize {
        self.plugin_commands.read().unwrap().len()
    }

    /// Get count of total commands (built-in + plugin)
    pub fn total_command_count(&self) -> usize {
        self.builtin_commands.len() + self.plugin_command_count()
    }

    /// Find a command by exact name match
    pub fn find_by_name(&self, name: &str) -> Option<Command> {
        // Check plugin commands first (they can override built-in)
        {
            let plugin_commands = self.plugin_commands.read().unwrap();
            if let Some(cmd) = plugin_commands.iter().find(|c| c.name == name) {
                return Some(cmd.clone());
            }
        }

        // Then check built-in commands
        self.builtin_commands
            .iter()
            .find(|c| c.name == name)
            .cloned()
    }
}

impl Default for CommandRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keybindings::Action;

    #[test]
    fn test_command_registry_creation() {
        let registry = CommandRegistry::new();
        assert!(registry.total_command_count() > 0); // Has built-in commands
        assert_eq!(registry.plugin_command_count(), 0); // No plugin commands yet
    }

    #[test]
    fn test_register_command() {
        let registry = CommandRegistry::new();

        let custom_command = Command {
            name: "Test Command".to_string(),
            description: "A test command".to_string(),
            action: Action::None,
            contexts: vec![],
        };

        registry.register(custom_command.clone());
        assert_eq!(registry.plugin_command_count(), 1);

        let found = registry.find_by_name("Test Command");
        assert!(found.is_some());
        assert_eq!(found.unwrap().description, "A test command");
    }

    #[test]
    fn test_unregister_command() {
        let registry = CommandRegistry::new();

        let custom_command = Command {
            name: "Test Command".to_string(),
            description: "A test command".to_string(),
            action: Action::None,
            contexts: vec![],
        };

        registry.register(custom_command);
        assert_eq!(registry.plugin_command_count(), 1);

        registry.unregister("Test Command");
        assert_eq!(registry.plugin_command_count(), 0);
    }

    #[test]
    fn test_register_replaces_existing() {
        let registry = CommandRegistry::new();

        let command1 = Command {
            name: "Test Command".to_string(),
            description: "First version".to_string(),
            action: Action::None,
            contexts: vec![],
        };

        let command2 = Command {
            name: "Test Command".to_string(),
            description: "Second version".to_string(),
            action: Action::None,
            contexts: vec![],
        };

        registry.register(command1);
        assert_eq!(registry.plugin_command_count(), 1);

        registry.register(command2);
        assert_eq!(registry.plugin_command_count(), 1); // Still just one

        let found = registry.find_by_name("Test Command").unwrap();
        assert_eq!(found.description, "Second version");
    }

    #[test]
    fn test_unregister_by_prefix() {
        let registry = CommandRegistry::new();

        registry.register(Command {
            name: "Plugin A: Command 1".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
        });

        registry.register(Command {
            name: "Plugin A: Command 2".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
        });

        registry.register(Command {
            name: "Plugin B: Command".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
        });

        assert_eq!(registry.plugin_command_count(), 3);

        registry.unregister_by_prefix("Plugin A:");
        assert_eq!(registry.plugin_command_count(), 1);

        let remaining = registry.find_by_name("Plugin B: Command");
        assert!(remaining.is_some());
    }

    #[test]
    fn test_filter_commands() {
        use crate::config::Config;
        use crate::keybindings::KeybindingResolver;

        let registry = CommandRegistry::new();
        let config = Config::default();
        let keybindings = KeybindingResolver::new(&config);

        registry.register(Command {
            name: "Test Save".to_string(),
            description: "Test save command".to_string(),
            action: Action::None,
            contexts: vec![KeyContext::Normal],
        });

        let results = registry.filter("save", KeyContext::Normal, &keybindings);
        assert!(results.len() >= 2); // At least "Save File" + "Test Save"

        // Check that both built-in and custom commands appear
        let names: Vec<String> = results.iter().map(|s| s.text.clone()).collect();
        assert!(names.iter().any(|n| n.contains("Save")));
    }

    #[test]
    fn test_context_filtering() {
        use crate::config::Config;
        use crate::keybindings::KeybindingResolver;

        let registry = CommandRegistry::new();
        let config = Config::default();
        let keybindings = KeybindingResolver::new(&config);

        registry.register(Command {
            name: "Normal Only".to_string(),
            description: "Available only in normal context".to_string(),
            action: Action::None,
            contexts: vec![KeyContext::Normal],
        });

        registry.register(Command {
            name: "Help Only".to_string(),
            description: "Available only in help context".to_string(),
            action: Action::None,
            contexts: vec![KeyContext::Help],
        });

        // In normal context, "Help Only" should be disabled
        let results = registry.filter("", KeyContext::Normal, &keybindings);
        let help_only = results.iter().find(|s| s.text == "Help Only");
        assert!(help_only.is_some());
        assert!(help_only.unwrap().disabled);

        // In help context, "Normal Only" should be disabled
        let results = registry.filter("", KeyContext::Help, &keybindings);
        let normal_only = results.iter().find(|s| s.text == "Normal Only");
        assert!(normal_only.is_some());
        assert!(normal_only.unwrap().disabled);
    }

    #[test]
    fn test_get_all_merges_commands() {
        let registry = CommandRegistry::new();
        let initial_count = registry.total_command_count();

        registry.register(Command {
            name: "Custom 1".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
        });

        registry.register(Command {
            name: "Custom 2".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
        });

        let all = registry.get_all();
        assert_eq!(all.len(), initial_count + 2);
    }

    #[test]
    fn test_plugin_command_overrides_builtin() {
        let registry = CommandRegistry::new();

        // Check a built-in command exists
        let builtin = registry.find_by_name("Save File");
        assert!(builtin.is_some());
        let original_desc = builtin.unwrap().description;

        // Override it with a plugin command
        registry.register(Command {
            name: "Save File".to_string(),
            description: "Custom save implementation".to_string(),
            action: Action::None,
            contexts: vec![],
        });

        // Should now find the custom version
        let custom = registry.find_by_name("Save File").unwrap();
        assert_eq!(custom.description, "Custom save implementation");
        assert_ne!(custom.description, original_desc);
    }

    #[test]
    fn test_record_usage() {
        let mut registry = CommandRegistry::new();

        registry.record_usage("Save File");
        assert_eq!(registry.history_position("Save File"), Some(0));

        registry.record_usage("Open File");
        assert_eq!(registry.history_position("Open File"), Some(0));
        assert_eq!(registry.history_position("Save File"), Some(1));

        // Using Save File again should move it to front
        registry.record_usage("Save File");
        assert_eq!(registry.history_position("Save File"), Some(0));
        assert_eq!(registry.history_position("Open File"), Some(1));
    }

    #[test]
    fn test_history_sorting() {
        use crate::config::Config;
        use crate::keybindings::KeybindingResolver;

        let mut registry = CommandRegistry::new();
        let config = Config::default();
        let keybindings = KeybindingResolver::new(&config);

        // Record some commands
        registry.record_usage("Quit");
        registry.record_usage("Save File");
        registry.record_usage("Open File");

        // Filter with empty query should return history-sorted results
        let results = registry.filter("", KeyContext::Normal, &keybindings);

        // Find positions of our test commands in results
        let open_pos = results.iter().position(|s| s.text == "Open File").unwrap();
        let save_pos = results.iter().position(|s| s.text == "Save File").unwrap();
        let quit_pos = results.iter().position(|s| s.text == "Quit").unwrap();

        // Most recently used should be first
        assert!(open_pos < save_pos, "Open File should come before Save File");
        assert!(save_pos < quit_pos, "Save File should come before Quit");
    }

    #[test]
    fn test_history_max_size() {
        let mut registry = CommandRegistry::new();

        // Add more than MAX_HISTORY_SIZE commands
        for i in 0..60 {
            registry.record_usage(&format!("Command {}", i));
        }

        // Should be trimmed to MAX_HISTORY_SIZE
        assert_eq!(registry.command_history.len(), CommandRegistry::MAX_HISTORY_SIZE);

        // Most recent should still be at front
        assert_eq!(registry.history_position("Command 59"), Some(0));

        // Oldest should be trimmed
        assert_eq!(registry.history_position("Command 0"), None);
    }

    #[test]
    fn test_unused_commands_alphabetical() {
        use crate::config::Config;
        use crate::keybindings::KeybindingResolver;

        let mut registry = CommandRegistry::new();
        let config = Config::default();
        let keybindings = KeybindingResolver::new(&config);

        // Register some custom commands (never used)
        registry.register(Command {
            name: "Zebra Command".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
        });

        registry.register(Command {
            name: "Alpha Command".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
        });

        // Use one built-in command
        registry.record_usage("Save File");

        let results = registry.filter("", KeyContext::Normal, &keybindings);

        let save_pos = results.iter().position(|s| s.text == "Save File").unwrap();
        let alpha_pos = results.iter().position(|s| s.text == "Alpha Command").unwrap();
        let zebra_pos = results.iter().position(|s| s.text == "Zebra Command").unwrap();

        // Used command should be first
        assert!(save_pos < alpha_pos, "Save File should come before Alpha Command");
        // Unused commands should be alphabetical
        assert!(alpha_pos < zebra_pos, "Alpha Command should come before Zebra Command");
    }
}
