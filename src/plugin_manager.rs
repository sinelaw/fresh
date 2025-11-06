//! Plugin Manager: Lua plugin loading and lifecycle management
//!
//! This module handles:
//! - Plugin discovery and loading
//! - Lua runtime management
//! - Plugin lifecycle (load/unload/reload)
//! - FFI bindings between Lua and Rust

use crate::command_registry::CommandRegistry;
use crate::commands::Command;
use crate::event::BufferId;
use crate::hooks::{HookArgs, HookRegistry};
use crate::keybindings::{Action, KeyContext};
use crate::plugin_api::{PluginApi, PluginCommand};
use mlua::{Lua, Table};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

/// Information about a loaded plugin
#[derive(Debug, Clone)]
pub struct PluginInfo {
    /// Plugin name
    pub name: String,
    /// Plugin file path
    pub path: PathBuf,
    /// Whether the plugin is enabled
    pub enabled: bool,
}

/// Plugin manager - handles loading and managing plugins
pub struct PluginManager {
    /// Lua runtime
    lua: Lua,

    /// Loaded plugins
    plugins: HashMap<String, PluginInfo>,

    /// Hook registry (shared with editor)
    hooks: Arc<RwLock<HookRegistry>>,

    /// Command registry (shared with editor)
    commands: Arc<RwLock<CommandRegistry>>,

    /// Plugin API for Lua bindings
    plugin_api: PluginApi,

    /// Command receiver (to get commands from plugins)
    command_receiver: std::sync::mpsc::Receiver<PluginCommand>,

    /// Action callbacks (action_name -> Lua registry key)
    action_callbacks: HashMap<String, mlua::RegistryKey>,
}

impl PluginManager {
    /// Create a new plugin manager
    pub fn new(
        hooks: Arc<RwLock<HookRegistry>>,
        commands: Arc<RwLock<CommandRegistry>>,
    ) -> Result<Self, mlua::Error> {
        let lua = Lua::new();

        // Create channel for plugin commands
        let (command_sender, command_receiver) = std::sync::mpsc::channel();

        // Create plugin API
        let plugin_api = PluginApi::new(
            Arc::clone(&hooks),
            Arc::clone(&commands),
            command_sender,
        );

        // Set up Lua globals and bindings
        Self::setup_lua_bindings(&lua, &plugin_api)?;

        // Create global table for storing callbacks
        lua.globals().set("_plugin_callbacks", lua.create_table()?)?;

        Ok(Self {
            lua,
            plugins: HashMap::new(),
            hooks,
            commands,
            plugin_api,
            command_receiver,
            action_callbacks: HashMap::new(),
        })
    }

    /// Set up Lua global functions and bindings
    fn setup_lua_bindings(lua: &Lua, api: &PluginApi) -> Result<(), mlua::Error> {
        let globals = lua.globals();

        // Create editor API table
        let editor = lua.create_table()?;

        // Clone API for closures
        let api_clone = api.clone();

        // editor.register_command(command_table)
        let register_command = lua.create_function(move |lua, table: Table| {
            let name: String = table.get("name")?;
            let description: String = table.get("description")?;
            let action_name: String = table.get("action")?;

            // Check if there's a callback function
            let callback: Option<mlua::Function> = table.get("callback").ok();

            // If there's a callback, store it in the global callbacks table
            if let Some(cb) = callback {
                let callbacks: Table = lua.globals().get("_plugin_callbacks")?;
                callbacks.set(action_name.clone(), cb)?;
            }

            // Parse action from string
            let action = match action_name.as_str() {
                "save" => Action::Save,
                "quit" => Action::Quit,
                "open" => Action::Open,
                "show_help" => Action::ShowHelp,
                "command_palette" => Action::CommandPalette,
                "undo" => Action::Undo,
                "redo" => Action::Redo,
                "none" => Action::None,
                _ => {
                    // For custom actions, use PluginAction variant
                    Action::PluginAction(action_name.clone())
                }
            };

            // Parse contexts
            let contexts: Vec<String> = table.get("contexts").unwrap_or_else(|_| Vec::new());
            let parsed_contexts: Vec<KeyContext> = contexts
                .iter()
                .filter_map(|c| match c.as_str() {
                    "normal" => Some(KeyContext::Normal),
                    "help" => Some(KeyContext::Help),
                    "prompt" => Some(KeyContext::Prompt),
                    "popup" => Some(KeyContext::Popup),
                    "file_explorer" => Some(KeyContext::FileExplorer),
                    _ => None,
                })
                .collect();

            let command = Command {
                name,
                description,
                action,
                contexts: parsed_contexts,
            };

            api_clone.register_command(command);
            Ok(())
        })?;
        editor.set("register_command", register_command)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.insert_text(buffer_id, position, text)
        let insert_text = lua.create_function(move |_, (buffer_id, position, text): (usize, usize, String)| {
            api_clone
                .insert_text(BufferId(buffer_id), position, text)
                .map_err(|e| mlua::Error::RuntimeError(e))
        })?;
        editor.set("insert_text", insert_text)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.add_overlay(buffer_id, overlay_id, start, end, r, g, b, underline)
        let add_overlay = lua.create_function(
            move |_,
                  (buffer_id, overlay_id, start, end, r, g, b, underline): (
                usize,
                String,
                usize,
                usize,
                u8,
                u8,
                u8,
                bool,
            )| {
                api_clone
                    .add_overlay(BufferId(buffer_id), overlay_id, start..end, (r, g, b), underline)
                    .map_err(|e| mlua::Error::RuntimeError(e))
            },
        )?;
        editor.set("add_overlay", add_overlay)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.set_status(message)
        let set_status = lua.create_function(move |_, message: String| {
            api_clone
                .set_status(message)
                .map_err(|e| mlua::Error::RuntimeError(e))
        })?;
        editor.set("set_status", set_status)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.on(hook_name, callback)
        let on_hook = lua.create_function(move |lua, (hook_name, callback): (String, mlua::Function)| {
            // Store callback in registry to keep it alive
            let registry_key = lua.create_registry_value(callback)?;

            // Create Rust callback that calls Lua function
            // Note: This is a simplified version - real implementation would need
            // to handle the registry key lifetime properly
            let hook_callback = Box::new(move |_args: &HookArgs| -> bool {
                // In real implementation, we'd call the Lua function here
                // For now, just return true
                true
            });

            api_clone.register_hook(&hook_name, hook_callback);
            Ok(())
        })?;
        editor.set("on", on_hook)?;

        // Set the editor table as a global
        globals.set("editor", editor)?;

        Ok(())
    }

    /// Load a plugin from a file
    pub fn load_plugin(&mut self, path: &Path) -> Result<(), String> {
        let plugin_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| "Invalid plugin filename".to_string())?
            .to_string();

        tracing::info!("Loading plugin: {} from {:?}", plugin_name, path);

        // Read plugin file
        let code = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read plugin file: {}", e))?;

        // Execute plugin code
        self.lua
            .load(&code)
            .set_name(plugin_name.as_str())
            .exec()
            .map_err(|e| format!("Failed to execute plugin: {}", e))?;

        // Store plugin info
        self.plugins.insert(
            plugin_name.clone(),
            PluginInfo {
                name: plugin_name,
                path: path.to_path_buf(),
                enabled: true,
            },
        );

        Ok(())
    }

    /// Unload a plugin
    pub fn unload_plugin(&mut self, name: &str) -> Result<(), String> {
        if let Some(plugin) = self.plugins.remove(name) {
            tracing::info!("Unloading plugin: {}", name);

            // Remove plugin's commands (assuming they're prefixed with plugin name)
            let prefix = format!("{}:", name);
            self.commands.read().unwrap().unregister_by_prefix(&prefix);

            // TODO: Remove plugin's hooks (need to track which hooks belong to which plugin)

            Ok(())
        } else {
            Err(format!("Plugin '{}' not found", name))
        }
    }

    /// Reload a plugin
    pub fn reload_plugin(&mut self, name: &str) -> Result<(), String> {
        let path = self
            .plugins
            .get(name)
            .ok_or_else(|| format!("Plugin '{}' not found", name))?
            .path
            .clone();

        self.unload_plugin(name)?;
        self.load_plugin(&path)?;

        Ok(())
    }

    /// Load all plugins from a directory
    pub fn load_plugins_from_dir(&mut self, dir: &Path) -> Vec<String> {
        let mut errors = Vec::new();

        if !dir.exists() {
            tracing::warn!("Plugin directory does not exist: {:?}", dir);
            return errors;
        }

        // Scan directory for .lua files
        match std::fs::read_dir(dir) {
            Ok(entries) => {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.extension().and_then(|s| s.to_str()) == Some("lua") {
                        if let Err(e) = self.load_plugin(&path) {
                            let err = format!("Failed to load {:?}: {}", path, e);
                            tracing::error!("{}", err);
                            errors.push(err);
                        }
                    }
                }
            }
            Err(e) => {
                let err = format!("Failed to read plugin directory: {}", e);
                tracing::error!("{}", err);
                errors.push(err);
            }
        }

        errors
    }

    /// Get list of loaded plugins
    pub fn list_plugins(&self) -> Vec<PluginInfo> {
        self.plugins.values().cloned().collect()
    }

    /// Process plugin commands (should be called in main loop)
    pub fn process_commands(&mut self) -> Vec<PluginCommand> {
        let mut commands = Vec::new();
        while let Ok(cmd) = self.command_receiver.try_recv() {
            commands.push(cmd);
        }
        commands
    }

    /// Execute a plugin action callback by name
    pub fn execute_action(&self, action_name: &str) -> Result<(), String> {
        tracing::info!("Executing plugin action: {}", action_name);

        // Get the callbacks table
        let callbacks: mlua::Table = self.lua.globals()
            .get("_plugin_callbacks")
            .map_err(|e| format!("Failed to get callbacks table: {}", e))?;

        // Get the callback function
        let callback: Option<mlua::Function> = callbacks.get(action_name).ok();

        if let Some(cb) = callback {
            // Call the callback
            cb.call::<_, ()>(())
                .map_err(|e| format!("Plugin callback error: {}", e))?;
            tracing::info!("Plugin action '{}' executed successfully", action_name);
            Ok(())
        } else {
            Err(format!("No callback registered for action: {}", action_name))
        }
    }

    /// Run a Lua snippet (for testing/debugging)
    pub fn eval(&self, code: &str) -> Result<String, String> {
        self.lua
            .load(code)
            .eval::<mlua::Value>()
            .map(|v| format!("{:?}", v))
            .map_err(|e| format!("{}", e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plugin_manager_creation() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let manager = PluginManager::new(hooks, commands);
        assert!(manager.is_ok());
    }

    #[test]
    fn test_eval_lua() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let manager = PluginManager::new(hooks, commands).unwrap();

        let result = manager.eval("return 1 + 1");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("2"));
    }

    #[test]
    fn test_lua_globals() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let manager = PluginManager::new(hooks, commands).unwrap();

        // Test that editor global is available
        let result = manager.eval("return editor ~= nil");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("true"));
    }

    #[test]
    fn test_register_command_from_lua() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let initial_count = commands.read().unwrap().plugin_command_count();

        let manager = PluginManager::new(hooks.clone(), commands.clone()).unwrap();

        let lua_code = r#"
            editor.register_command({
                name = "Test Command",
                description = "A test command from Lua",
                action = "none",
                contexts = {"normal"}
            })
        "#;

        let result = manager.eval(lua_code);
        assert!(result.is_ok(), "Failed to register command: {:?}", result);

        // Check that command was registered
        let new_count = commands.read().unwrap().plugin_command_count();
        assert_eq!(new_count, initial_count + 1);
    }

    #[test]
    fn test_set_status_from_lua() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = PluginManager::new(hooks, commands).unwrap();

        let lua_code = r#"editor.set_status("Hello from Lua")"#;

        let result = manager.eval(lua_code);
        assert!(result.is_ok());

        // Check that command was sent
        let plugin_commands = manager.process_commands();
        assert_eq!(plugin_commands.len(), 1);

        match &plugin_commands[0] {
            PluginCommand::SetStatus { message } => {
                assert_eq!(message, "Hello from Lua");
            }
            _ => panic!("Wrong command type"),
        }
    }

    #[test]
    fn test_insert_text_from_lua() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = PluginManager::new(hooks, commands).unwrap();

        let lua_code = r#"editor.insert_text(1, 0, "test")"#;

        let result = manager.eval(lua_code);
        assert!(result.is_ok());

        // Check that command was sent
        let plugin_commands = manager.process_commands();
        assert_eq!(plugin_commands.len(), 1);

        match &plugin_commands[0] {
            PluginCommand::InsertText {
                buffer_id,
                position,
                text,
            } => {
                assert_eq!(buffer_id.0, 1);
                assert_eq!(*position, 0);
                assert_eq!(text, "test");
            }
            _ => panic!("Wrong command type"),
        }
    }

    #[test]
    fn test_add_overlay_from_lua() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = PluginManager::new(hooks, commands).unwrap();

        let lua_code = r#"editor.add_overlay(1, "test-id", 0, 10, 255, 0, 0, true)"#;

        let result = manager.eval(lua_code);
        assert!(result.is_ok());

        // Check that command was sent
        let plugin_commands = manager.process_commands();
        assert_eq!(plugin_commands.len(), 1);

        match &plugin_commands[0] {
            PluginCommand::AddOverlay {
                buffer_id,
                overlay_id,
                range,
                color,
                underline,
            } => {
                assert_eq!(buffer_id.0, 1);
                assert_eq!(overlay_id, "test-id");
                assert_eq!(range.clone(), 0..10);
                assert_eq!(*color, (255, 0, 0));
                assert!(underline);
            }
            _ => panic!("Wrong command type"),
        }
    }

    #[test]
    fn test_load_plugin_file() {
        use std::io::Write;
        use tempfile::NamedTempFile;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = PluginManager::new(hooks, commands).unwrap();

        // Create a temporary Lua file
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(
            temp_file,
            r#"
            editor.register_command({{
                name = "Test Plugin Command",
                description = "From file",
                action = "none",
                contexts = {{"normal"}}
            }})
        "#
        )
        .unwrap();

        let result = manager.load_plugin(temp_file.path());
        assert!(result.is_ok(), "Failed to load plugin: {:?}", result);

        // Check plugin is in list
        let plugins = manager.list_plugins();
        assert_eq!(plugins.len(), 1);
    }

    #[test]
    fn test_unload_plugin() {
        use std::io::Write;
        use tempfile::NamedTempFile;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = PluginManager::new(hooks, commands).unwrap();

        // Create and load a temporary plugin
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, r#"-- test plugin"#).unwrap();

        manager.load_plugin(temp_file.path()).unwrap();

        let plugin_name = temp_file
            .path()
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        assert_eq!(manager.list_plugins().len(), 1);

        // Unload it
        let result = manager.unload_plugin(&plugin_name);
        assert!(result.is_ok());

        assert_eq!(manager.list_plugins().len(), 0);
    }
}
