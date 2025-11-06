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
}

impl CommandRegistry {
    /// Create a new command registry with built-in commands
    pub fn new() -> Self {
        Self {
            builtin_commands: get_all_commands(),
            plugin_commands: Arc::new(RwLock::new(Vec::new())),
        }
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
    pub fn filter(&self, query: &str, current_context: KeyContext) -> Vec<Suggestion> {
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

        // Filter and convert to suggestions
        let mut suggestions: Vec<Suggestion> = commands
            .into_iter()
            .filter(|cmd| matches_query(cmd))
            .map(|cmd| {
                let available = is_available(&cmd);
                Suggestion::with_description_and_disabled(
                    cmd.name.clone(),
                    cmd.description,
                    !available,
                )
            })
            .collect();

        // Sort: available commands first, then disabled ones
        suggestions.sort_by_key(|s| s.disabled);

        suggestions
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
        let registry = CommandRegistry::new();

        registry.register(Command {
            name: "Test Save".to_string(),
            description: "Test save command".to_string(),
            action: Action::None,
            contexts: vec![KeyContext::Normal],
        });

        let results = registry.filter("save", KeyContext::Normal);
        assert!(results.len() >= 2); // At least "Save File" + "Test Save"

        // Check that both built-in and custom commands appear
        let names: Vec<String> = results.iter().map(|s| s.text.clone()).collect();
        assert!(names.iter().any(|n| n.contains("Save")));
    }

    #[test]
    fn test_context_filtering() {
        let registry = CommandRegistry::new();

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
        let results = registry.filter("", KeyContext::Normal);
        let help_only = results.iter().find(|s| s.text == "Help Only");
        assert!(help_only.is_some());
        assert!(help_only.unwrap().disabled);

        // In help context, "Normal Only" should be disabled
        let results = registry.filter("", KeyContext::Help);
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
}
