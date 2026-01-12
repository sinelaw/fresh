//! Dynamic command registry for plugins and extensions
//!
//! This module allows plugins to register custom commands dynamically
//! while maintaining the built-in command set.

use crate::input::commands::{get_all_commands, Command, Suggestion};
use crate::input::fuzzy::fuzzy_match;
use crate::input::keybindings::Action;
use crate::input::keybindings::KeyContext;
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

    /// Refresh built-in commands (e.g. after locale change)
    pub fn refresh_builtin_commands(&mut self) {
        self.builtin_commands = get_all_commands();
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
    /// When query is not empty, commands are sorted by match quality (fzf-style scoring)
    /// with recency as tiebreaker for equal scores.
    /// Disabled commands always appear after enabled ones.
    pub fn filter(
        &self,
        query: &str,
        current_context: KeyContext,
        keybinding_resolver: &crate::input::keybindings::KeybindingResolver,
        selection_active: bool,
        active_custom_contexts: &std::collections::HashSet<String>,
        active_buffer_mode: Option<&str>,
    ) -> Vec<Suggestion> {
        let commands = self.get_all();

        // Helper function to check if command should be visible (custom context check)
        // Commands with unmet custom contexts are completely hidden, not just disabled
        // A custom context is satisfied if:
        // 1. It's in the global active_custom_contexts set, OR
        // 2. It matches the focused buffer's mode (for buffer-scoped commands)
        let is_visible = |cmd: &Command| -> bool {
            cmd.custom_contexts.is_empty()
                || cmd.custom_contexts.iter().all(|ctx| {
                    active_custom_contexts.contains(ctx)
                        || active_buffer_mode.is_some_and(|mode| mode == ctx)
                })
        };

        // Helper function to check if command is available in current context
        let is_available = |cmd: &Command| -> bool {
            // Global commands are always available
            if cmd.contexts.contains(&KeyContext::Global) {
                return true;
            }

            // Check built-in contexts
            cmd.contexts.is_empty() || cmd.contexts.contains(&current_context)
        };

        // Helper to create a suggestion from a command
        let make_suggestion =
            |cmd: &Command, score: i32, localized_name: String, localized_desc: String| {
                let mut available = is_available(cmd);
                if cmd.action == Action::FindInSelection && !selection_active {
                    available = false;
                }
                let keybinding =
                    keybinding_resolver.get_keybinding_for_action(&cmd.action, current_context);
                let history_pos = self.history_position(&cmd.name);

                let suggestion = Suggestion::with_source(
                    localized_name,
                    Some(localized_desc),
                    !available,
                    keybinding,
                    Some(cmd.source.clone()),
                );
                (suggestion, history_pos, score)
            };

        // First, try to match by name only
        // Commands with unmet custom contexts are completely hidden
        let mut suggestions: Vec<(Suggestion, Option<usize>, i32)> = commands
            .iter()
            .filter(|cmd| is_visible(cmd))
            .filter_map(|cmd| {
                let localized_name = cmd.get_localized_name();
                let name_result = fuzzy_match(query, &localized_name);
                if name_result.matched {
                    let localized_desc = cmd.get_localized_description();
                    Some(make_suggestion(
                        cmd,
                        name_result.score,
                        localized_name,
                        localized_desc,
                    ))
                } else {
                    None
                }
            })
            .collect();

        // If no name matches found, try description matching as a fallback
        if suggestions.is_empty() && !query.is_empty() {
            suggestions = commands
                .iter()
                .filter(|cmd| is_visible(cmd))
                .filter_map(|cmd| {
                    let localized_desc = cmd.get_localized_description();
                    let desc_result = fuzzy_match(query, &localized_desc);
                    if desc_result.matched {
                        let localized_name = cmd.get_localized_name();
                        // Description matches get reduced score
                        Some(make_suggestion(
                            cmd,
                            desc_result.score.saturating_sub(50),
                            localized_name,
                            localized_desc,
                        ))
                    } else {
                        None
                    }
                })
                .collect();
        }

        // Sort by:
        // 1. Disabled status (enabled first)
        // 2. Fuzzy match score (higher is better) - only when query is not empty
        // 3. History position (recent first, then never-used alphabetically)
        let has_query = !query.is_empty();
        suggestions.sort_by(|(a, a_hist, a_score), (b, b_hist, b_score)| {
            // First sort by disabled status
            match a.disabled.cmp(&b.disabled) {
                std::cmp::Ordering::Equal => {}
                other => return other,
            }

            // When there's a query, sort by fuzzy score (higher is better)
            if has_query {
                match b_score.cmp(a_score) {
                    std::cmp::Ordering::Equal => {}
                    other => return other,
                }
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
        suggestions.into_iter().map(|(s, _, _)| s).collect()
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
    use crate::input::commands::CommandSource;
    use crate::input::keybindings::Action;

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
            custom_contexts: vec![],
            source: CommandSource::Builtin,
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
            custom_contexts: vec![],
            source: CommandSource::Builtin,
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
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        };

        let command2 = Command {
            name: "Test Command".to_string(),
            description: "Second version".to_string(),
            action: Action::None,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
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
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        });

        registry.register(Command {
            name: "Plugin A: Command 2".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        });

        registry.register(Command {
            name: "Plugin B: Command".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
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
        use crate::input::keybindings::KeybindingResolver;

        let registry = CommandRegistry::new();
        let config = Config::default();
        let keybindings = KeybindingResolver::new(&config);

        registry.register(Command {
            name: "Test Save".to_string(),
            description: "Test save command".to_string(),
            action: Action::None,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        });

        let empty_contexts = std::collections::HashSet::new();
        let results = registry.filter(
            "save",
            KeyContext::Normal,
            &keybindings,
            false,
            &empty_contexts,
            None,
        );
        assert!(results.len() >= 2); // At least "Save File" + "Test Save"

        // Check that both built-in and custom commands appear
        let names: Vec<String> = results.iter().map(|s| s.text.clone()).collect();
        assert!(names.iter().any(|n| n.contains("Save")));
    }

    #[test]
    fn test_context_filtering() {
        use crate::config::Config;
        use crate::input::keybindings::KeybindingResolver;

        let registry = CommandRegistry::new();
        let config = Config::default();
        let keybindings = KeybindingResolver::new(&config);

        registry.register(Command {
            name: "Normal Only".to_string(),
            description: "Available only in normal context".to_string(),
            action: Action::None,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        });

        registry.register(Command {
            name: "Popup Only".to_string(),
            description: "Available only in popup context".to_string(),
            action: Action::None,
            contexts: vec![KeyContext::Popup],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        });

        // In normal context, "Popup Only" should be disabled
        let empty_contexts = std::collections::HashSet::new();
        let results = registry.filter(
            "",
            KeyContext::Normal,
            &keybindings,
            false,
            &empty_contexts,
            None,
        );
        let popup_only = results.iter().find(|s| s.text == "Popup Only");
        assert!(popup_only.is_some());
        assert!(popup_only.unwrap().disabled);

        // In popup context, "Normal Only" should be disabled
        let results = registry.filter(
            "",
            KeyContext::Popup,
            &keybindings,
            false,
            &empty_contexts,
            None,
        );
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
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        });

        registry.register(Command {
            name: "Custom 2".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
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
            custom_contexts: vec![],
            source: CommandSource::Builtin,
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
        use crate::input::keybindings::KeybindingResolver;

        let mut registry = CommandRegistry::new();
        let config = Config::default();
        let keybindings = KeybindingResolver::new(&config);

        // Record some commands
        registry.record_usage("Quit");
        registry.record_usage("Save File");
        registry.record_usage("Open File");

        // Filter with empty query should return history-sorted results
        let empty_contexts = std::collections::HashSet::new();
        let results = registry.filter(
            "",
            KeyContext::Normal,
            &keybindings,
            false,
            &empty_contexts,
            None,
        );

        // Find positions of our test commands in results
        let open_pos = results.iter().position(|s| s.text == "Open File").unwrap();
        let save_pos = results.iter().position(|s| s.text == "Save File").unwrap();
        let quit_pos = results.iter().position(|s| s.text == "Quit").unwrap();

        // Most recently used should be first
        assert!(
            open_pos < save_pos,
            "Open File should come before Save File"
        );
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
        assert_eq!(
            registry.command_history.len(),
            CommandRegistry::MAX_HISTORY_SIZE
        );

        // Most recent should still be at front
        assert_eq!(registry.history_position("Command 59"), Some(0));

        // Oldest should be trimmed
        assert_eq!(registry.history_position("Command 0"), None);
    }

    #[test]
    fn test_unused_commands_alphabetical() {
        use crate::config::Config;
        use crate::input::keybindings::KeybindingResolver;

        let mut registry = CommandRegistry::new();
        let config = Config::default();
        let keybindings = KeybindingResolver::new(&config);

        // Register some custom commands (never used)
        registry.register(Command {
            name: "Zebra Command".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        });

        registry.register(Command {
            name: "Alpha Command".to_string(),
            description: "".to_string(),
            action: Action::None,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        });

        // Use one built-in command
        registry.record_usage("Save File");

        let empty_contexts = std::collections::HashSet::new();
        let results = registry.filter(
            "",
            KeyContext::Normal,
            &keybindings,
            false,
            &empty_contexts,
            None,
        );

        let save_pos = results.iter().position(|s| s.text == "Save File").unwrap();
        let alpha_pos = results
            .iter()
            .position(|s| s.text == "Alpha Command")
            .unwrap();
        let zebra_pos = results
            .iter()
            .position(|s| s.text == "Zebra Command")
            .unwrap();

        // Used command should be first
        assert!(
            save_pos < alpha_pos,
            "Save File should come before Alpha Command"
        );
        // Unused commands should be alphabetical
        assert!(
            alpha_pos < zebra_pos,
            "Alpha Command should come before Zebra Command"
        );
    }

    #[test]
    fn test_required_commands_exist() {
        // This test ensures that all required command palette entries exist.
        // If this test fails, it means a command was removed or renamed.
        crate::i18n::set_locale("en");
        let registry = CommandRegistry::new();

        let required_commands = [
            // LSP commands
            ("Show Completions", Action::LspCompletion),
            ("Go to Definition", Action::LspGotoDefinition),
            ("Show Hover Info", Action::LspHover),
            ("Find References", Action::LspReferences),
            // Help commands
            ("Show Manual", Action::ShowHelp),
            ("Show Keyboard Shortcuts", Action::ShowKeyboardShortcuts),
            // Scroll commands
            ("Scroll Up", Action::ScrollUp),
            ("Scroll Down", Action::ScrollDown),
            ("Scroll Tabs Left", Action::ScrollTabsLeft),
            ("Scroll Tabs Right", Action::ScrollTabsRight),
            // Navigation commands
            ("Smart Home", Action::SmartHome),
            // Delete commands
            ("Delete Word Backward", Action::DeleteWordBackward),
            ("Delete Word Forward", Action::DeleteWordForward),
            ("Delete to End of Line", Action::DeleteToLineEnd),
        ];

        for (name, expected_action) in required_commands {
            let cmd = registry.find_by_name(name);
            assert!(
                cmd.is_some(),
                "Command '{}' should exist in command palette",
                name
            );
            assert_eq!(
                cmd.unwrap().action,
                expected_action,
                "Command '{}' should have action {:?}",
                name,
                expected_action
            );
        }
    }
}
