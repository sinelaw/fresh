//! Plugin API: Safe interface for plugins to interact with the editor
//!
//! This module provides a safe, controlled API for plugins (Lua, WASM, etc.)
//! to interact with the editor without direct access to internal state.

use crate::command_registry::CommandRegistry;
use crate::commands::Command;
use crate::event::{BufferId, Event, OverlayFace, UnderlineStyle};
use crate::hooks::{HookArgs, HookCallback, HookRegistry};
use crate::keybindings::Action;
use std::ops::Range;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

/// Plugin command - allows plugins to send commands to the editor
#[derive(Debug, Clone)]
pub enum PluginCommand {
    /// Insert text at a position in a buffer
    InsertText {
        buffer_id: BufferId,
        position: usize,
        text: String,
    },

    /// Delete a range of text from a buffer
    DeleteRange {
        buffer_id: BufferId,
        range: Range<usize>,
    },

    /// Add an overlay to a buffer
    AddOverlay {
        buffer_id: BufferId,
        overlay_id: String,
        range: Range<usize>,
        color: (u8, u8, u8),
        underline: bool,
    },

    /// Remove an overlay from a buffer
    RemoveOverlay {
        buffer_id: BufferId,
        overlay_id: String,
    },

    /// Set status message
    SetStatus { message: String },

    /// Register a custom command
    RegisterCommand { command: Command },

    /// Unregister a command by name
    UnregisterCommand { name: String },
}

/// Plugin API context - provides safe access to editor functionality
pub struct PluginApi {
    /// Hook registry (shared with editor)
    hooks: Arc<RwLock<HookRegistry>>,

    /// Command registry (shared with editor)
    commands: Arc<RwLock<CommandRegistry>>,

    /// Command queue for sending commands to editor
    command_sender: std::sync::mpsc::Sender<PluginCommand>,
}

impl PluginApi {
    /// Create a new plugin API context
    pub fn new(
        hooks: Arc<RwLock<HookRegistry>>,
        commands: Arc<RwLock<CommandRegistry>>,
        command_sender: std::sync::mpsc::Sender<PluginCommand>,
    ) -> Self {
        Self {
            hooks,
            commands,
            command_sender,
        }
    }

    /// Register a hook callback
    pub fn register_hook(&self, hook_name: &str, callback: HookCallback) {
        let mut hooks = self.hooks.write().unwrap();
        hooks.add_hook(hook_name, callback);
    }

    /// Remove all hooks for a specific name
    pub fn unregister_hooks(&self, hook_name: &str) {
        let mut hooks = self.hooks.write().unwrap();
        hooks.remove_hooks(hook_name);
    }

    /// Register a command
    pub fn register_command(&self, command: Command) {
        let commands = self.commands.read().unwrap();
        commands.register(command);
    }

    /// Unregister a command by name
    pub fn unregister_command(&self, name: &str) {
        let commands = self.commands.read().unwrap();
        commands.unregister(name);
    }

    /// Send a command to the editor (async/non-blocking)
    pub fn send_command(&self, command: PluginCommand) -> Result<(), String> {
        self.command_sender
            .send(command)
            .map_err(|e| format!("Failed to send command: {}", e))
    }

    /// Insert text at a position in a buffer
    pub fn insert_text(
        &self,
        buffer_id: BufferId,
        position: usize,
        text: String,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::InsertText {
            buffer_id,
            position,
            text,
        })
    }

    /// Delete a range of text from a buffer
    pub fn delete_range(
        &self,
        buffer_id: BufferId,
        range: Range<usize>,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::DeleteRange { buffer_id, range })
    }

    /// Add an overlay (decoration) to a buffer
    pub fn add_overlay(
        &self,
        buffer_id: BufferId,
        overlay_id: String,
        range: Range<usize>,
        color: (u8, u8, u8),
        underline: bool,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::AddOverlay {
            buffer_id,
            overlay_id,
            range,
            color,
            underline,
        })
    }

    /// Remove an overlay from a buffer
    pub fn remove_overlay(
        &self,
        buffer_id: BufferId,
        overlay_id: String,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::RemoveOverlay {
            buffer_id,
            overlay_id,
        })
    }

    /// Set the status message
    pub fn set_status(&self, message: String) -> Result<(), String> {
        self.send_command(PluginCommand::SetStatus { message })
    }
}

impl Clone for PluginApi {
    fn clone(&self) -> Self {
        Self {
            hooks: Arc::clone(&self.hooks),
            commands: Arc::clone(&self.commands),
            command_sender: self.command_sender.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keybindings::KeyContext;

    #[test]
    fn test_plugin_api_creation() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();

        let api = PluginApi::new(hooks, commands, tx);

        // Should not panic
        let _clone = api.clone();
    }

    #[test]
    fn test_register_hook() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();

        let api = PluginApi::new(hooks.clone(), commands, tx);

        api.register_hook("test-hook", Box::new(|_| true));

        let hook_registry = hooks.read().unwrap();
        assert_eq!(hook_registry.hook_count("test-hook"), 1);
    }

    #[test]
    fn test_register_command() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();

        let api = PluginApi::new(hooks, commands.clone(), tx);

        let command = Command {
            name: "Test Command".to_string(),
            description: "A test".to_string(),
            action: Action::None,
            contexts: vec![],
        };

        api.register_command(command);

        let cmd_registry = commands.read().unwrap();
        assert_eq!(cmd_registry.plugin_command_count(), 1);
    }

    #[test]
    fn test_send_command() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, rx) = std::sync::mpsc::channel();

        let api = PluginApi::new(hooks, commands, tx);

        let result = api.insert_text(BufferId(1), 0, "test".to_string());
        assert!(result.is_ok());

        // Verify command was sent
        let received = rx.try_recv();
        assert!(received.is_ok());

        match received.unwrap() {
            PluginCommand::InsertText {
                buffer_id,
                position,
                text,
            } => {
                assert_eq!(buffer_id.0, 1);
                assert_eq!(position, 0);
                assert_eq!(text, "test");
            }
            _ => panic!("Wrong command type"),
        }
    }

    #[test]
    fn test_add_overlay_command() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, rx) = std::sync::mpsc::channel();

        let api = PluginApi::new(hooks, commands, tx);

        let result = api.add_overlay(
            BufferId(1),
            "test-overlay".to_string(),
            0..10,
            (255, 0, 0),
            true,
        );
        assert!(result.is_ok());

        let received = rx.try_recv().unwrap();
        match received {
            PluginCommand::AddOverlay {
                buffer_id,
                overlay_id,
                range,
                color,
                underline,
            } => {
                assert_eq!(buffer_id.0, 1);
                assert_eq!(overlay_id, "test-overlay");
                assert_eq!(range, 0..10);
                assert_eq!(color, (255, 0, 0));
                assert!(underline);
            }
            _ => panic!("Wrong command type"),
        }
    }

    #[test]
    fn test_set_status_command() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, rx) = std::sync::mpsc::channel();

        let api = PluginApi::new(hooks, commands, tx);

        let result = api.set_status("Test status".to_string());
        assert!(result.is_ok());

        let received = rx.try_recv().unwrap();
        match received {
            PluginCommand::SetStatus { message } => {
                assert_eq!(message, "Test status");
            }
            _ => panic!("Wrong command type"),
        }
    }
}
