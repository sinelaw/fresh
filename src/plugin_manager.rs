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
use crate::plugin_api::{EditorStateSnapshot, PluginApi, PluginCommand};
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

    /// Command registry (shared with editor)
    commands: Arc<RwLock<CommandRegistry>>,

    /// Plugin API for Lua bindings
    plugin_api: PluginApi,

    /// Command receiver (to get commands from plugins)
    command_receiver: std::sync::mpsc::Receiver<PluginCommand>,

    /// Async bridge sender (for spawning processes)
    async_sender: Option<std::sync::mpsc::Sender<crate::async_bridge::AsyncMessage>>,
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

        // Create debug log file in temp directory
        let debug_log_path =
            std::env::temp_dir().join(format!("editor_plugin_debug_{}.log", std::process::id()));

        // Create or truncate the debug log file
        std::fs::write(&debug_log_path, "=== Plugin Debug Log ===\n")
            .map_err(|e| mlua::Error::RuntimeError(format!("Failed to create debug log: {}", e)))?;

        // Create editor state snapshot for query API
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Create plugin API
        let plugin_api = PluginApi::new(
            Arc::clone(&hooks),
            Arc::clone(&commands),
            command_sender,
            Arc::clone(&state_snapshot),
        );

        // Set up Lua globals and bindings
        Self::setup_lua_bindings(&lua, &plugin_api, &debug_log_path)?;

        // Create global table for storing callbacks
        lua.globals()
            .set("_plugin_callbacks", lua.create_table()?)?;

        // Create global table for storing spawn callbacks
        lua.globals().set("_spawn_callbacks", lua.create_table()?)?;

        // Create global table for storing hook callbacks
        lua.globals().set("_hook_callbacks", lua.create_table()?)?;

        tracing::info!("Plugin debug log: {:?}", debug_log_path);

        Ok(Self {
            lua,
            plugins: HashMap::new(),
            commands,
            plugin_api,
            command_receiver,
            async_sender: None,
        })
    }

    /// Set the async bridge sender (called by editor after construction)
    pub fn set_async_sender(
        &mut self,
        sender: std::sync::mpsc::Sender<crate::async_bridge::AsyncMessage>,
    ) {
        self.async_sender = Some(sender);
    }

    /// Set up Lua global functions and bindings
    fn setup_lua_bindings(
        lua: &Lua,
        api: &PluginApi,
        debug_log_path: &PathBuf,
    ) -> Result<(), mlua::Error> {
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
        let insert_text = lua.create_function(
            move |_, (buffer_id, position, text): (usize, usize, String)| {
                api_clone
                    .insert_text(BufferId(buffer_id), position, text)
                    .map_err(|e| mlua::Error::RuntimeError(e))
            },
        )?;
        editor.set("insert_text", insert_text)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.insert(text) - insert at current cursor position in active buffer
        let insert = lua.create_function(move |_, text: String| {
            api_clone
                .send_command(PluginCommand::InsertAtCursor { text })
                .map_err(|e| mlua::Error::RuntimeError(e))
        })?;
        editor.set("insert", insert)?;

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
                    .add_overlay(
                        BufferId(buffer_id),
                        overlay_id,
                        start..end,
                        (r, g, b),
                        underline,
                    )
                    .map_err(|e| mlua::Error::RuntimeError(e))
            },
        )?;
        editor.set("add_overlay", add_overlay)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.remove_overlay(buffer_id, overlay_id)
        let remove_overlay =
            lua.create_function(move |_, (buffer_id, overlay_id): (usize, String)| {
                api_clone
                    .remove_overlay(BufferId(buffer_id), overlay_id)
                    .map_err(|e| mlua::Error::RuntimeError(e))
            })?;
        editor.set("remove_overlay", remove_overlay)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.set_status(message)
        let set_status = lua.create_function(move |_, message: String| {
            api_clone
                .set_status(message)
                .map_err(|e| mlua::Error::RuntimeError(e))
        })?;
        editor.set("set_status", set_status)?;

        // editor.on(hook_name, callback)
        // We can't directly create a closure that captures Lua state across threads,
        // so we store the callback and invoke it later in run_hook()
        let on_hook = lua.create_function(
            move |lua, (hook_name, callback): (String, mlua::Function)| {
                // Store callback in a global table so we can find it later
                let hooks_table: mlua::Table = lua.globals().get("_hook_callbacks")?;

                // Get or create array for this hook name
                let hook_array: mlua::Table = if hooks_table.contains_key(hook_name.as_str())? {
                    hooks_table.get(hook_name.as_str())?
                } else {
                    let new_array = lua.create_table()?;
                    hooks_table.set(hook_name.as_str(), &new_array)?;
                    new_array
                };

                // Append callback to array
                let len = hook_array.len()?;
                hook_array.set(len + 1, callback)?;

                Ok(())
            },
        )?;
        editor.set("on", on_hook)?;

        // Clone API for query functions
        let api_clone = api.clone();

        // editor.get_active_buffer_id() - Get the ID of the currently active buffer
        let get_active_buffer_id = lua.create_function(move |_, ()| {
            let buffer_id = api_clone.get_active_buffer_id();
            Ok(buffer_id.0)
        })?;
        editor.set("get_active_buffer_id", get_active_buffer_id)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.list_buffers() - Get info about all open buffers
        let list_buffers = lua.create_function(move |lua, ()| {
            let buffers = api_clone.list_buffers();
            let result = lua.create_table()?;
            for (idx, buf_info) in buffers.iter().enumerate() {
                let buf_table = lua.create_table()?;
                buf_table.set("id", buf_info.id.0)?;
                buf_table.set(
                    "path",
                    buf_info
                        .path
                        .as_ref()
                        .and_then(|p| p.to_str())
                        .unwrap_or(""),
                )?;
                buf_table.set("modified", buf_info.modified)?;
                buf_table.set("length", buf_info.length)?;
                result.set(idx + 1, buf_table)?;
            }
            Ok(result)
        })?;
        editor.set("list_buffers", list_buffers)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.get_buffer_info(buffer_id) - Get info about a specific buffer
        let get_buffer_info = lua.create_function(move |lua, buffer_id: usize| {
            if let Some(buf_info) = api_clone.get_buffer_info(BufferId(buffer_id)) {
                let buf_table = lua.create_table()?;
                buf_table.set("id", buf_info.id.0)?;
                buf_table.set(
                    "path",
                    buf_info
                        .path
                        .as_ref()
                        .and_then(|p| p.to_str())
                        .unwrap_or(""),
                )?;
                buf_table.set("modified", buf_info.modified)?;
                buf_table.set("length", buf_info.length)?;
                Ok(Some(buf_table))
            } else {
                Ok(None)
            }
        })?;
        editor.set("get_buffer_info", get_buffer_info)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.get_primary_cursor() - Get primary cursor info for active buffer
        let get_primary_cursor = lua.create_function(move |lua, ()| {
            if let Some(cursor) = api_clone.get_primary_cursor() {
                let cursor_table = lua.create_table()?;
                cursor_table.set("position", cursor.position)?;
                if let Some(sel) = cursor.selection {
                    let sel_table = lua.create_table()?;
                    sel_table.set("start", sel.start)?;
                    sel_table.set("end", sel.end)?;
                    cursor_table.set("selection", sel_table)?;
                } else {
                    cursor_table.set("selection", mlua::Value::Nil)?;
                }
                Ok(Some(cursor_table))
            } else {
                Ok(None)
            }
        })?;
        editor.set("get_primary_cursor", get_primary_cursor)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.get_all_cursors() - Get all cursor info for active buffer
        let get_all_cursors = lua.create_function(move |lua, ()| {
            let cursors = api_clone.get_all_cursors();
            let result = lua.create_table()?;
            for (idx, cursor) in cursors.iter().enumerate() {
                let cursor_table = lua.create_table()?;
                cursor_table.set("position", cursor.position)?;
                if let Some(sel) = &cursor.selection {
                    let sel_table = lua.create_table()?;
                    sel_table.set("start", sel.start)?;
                    sel_table.set("end", sel.end)?;
                    cursor_table.set("selection", sel_table)?;
                } else {
                    cursor_table.set("selection", mlua::Value::Nil)?;
                }
                result.set(idx + 1, cursor_table)?;
            }
            Ok(result)
        })?;
        editor.set("get_all_cursors", get_all_cursors)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.get_viewport() - Get viewport information for active buffer
        let get_viewport = lua.create_function(move |lua, ()| {
            if let Some(viewport) = api_clone.get_viewport() {
                let vp_table = lua.create_table()?;
                vp_table.set("top_byte", viewport.top_byte)?;
                vp_table.set("left_column", viewport.left_column)?;
                vp_table.set("width", viewport.width)?;
                vp_table.set("height", viewport.height)?;
                Ok(Some(vp_table))
            } else {
                Ok(None)
            }
        })?;
        editor.set("get_viewport", get_viewport)?;

        // Clone API for spawn function
        let api_clone = api.clone();

        // editor.spawn(command, args, callback) or editor.spawn(command, args, options, callback)
        // where options = {cwd = "/path"}
        let spawn = lua.create_function(move |lua, args: mlua::Variadic<mlua::Value>| {
            let args_vec: Vec<mlua::Value> = args.into_iter().collect();

            if args_vec.len() < 3 {
                return Err(mlua::Error::RuntimeError(
                    "spawn requires at least 3 arguments: command, args, callback".to_string(),
                ));
            }

            // Parse command
            let command: String = lua.unpack(args_vec[0].clone())?;

            // Parse args array
            let args_table: mlua::Table = lua.unpack(args_vec[1].clone())?;
            let mut command_args = Vec::new();
            for pair in args_table.pairs::<mlua::Value, String>() {
                let (_, arg) = pair?;
                command_args.push(arg);
            }

            // Check if we have 3 or 4 arguments
            let (cwd, callback) = if args_vec.len() == 4 {
                // Format: spawn(command, args, options, callback)
                let options: mlua::Table = lua.unpack(args_vec[2].clone())?;
                let cwd: Option<String> = options.get("cwd").ok();
                let callback: mlua::Function = lua.unpack(args_vec[3].clone())?;
                (cwd, callback)
            } else {
                // Format: spawn(command, args, callback)
                let callback: mlua::Function = lua.unpack(args_vec[2].clone())?;
                (None, callback)
            };

            // Get next callback ID from global counter
            let spawn_callbacks: mlua::Table = lua.globals().get("_spawn_callbacks")?;
            let callback_id: u64 = spawn_callbacks.raw_len() as u64 + 1;

            // Store callback in _spawn_callbacks table
            spawn_callbacks.set(callback_id, callback)?;

            // Send spawn command via plugin API
            api_clone
                .send_command(PluginCommand::SpawnProcess {
                    command,
                    args: command_args,
                    cwd,
                    callback_id,
                })
                .map_err(|e| mlua::Error::RuntimeError(e))?;

            Ok(())
        })?;
        editor.set("spawn", spawn)?;

        // Clone API for clear overlays functions
        let api_clone = api.clone();

        // editor.clear_all_overlays(buffer_id) - Remove all overlays from a buffer
        let clear_all_overlays = lua.create_function(move |_, buffer_id: usize| {
            api_clone
                .send_command(PluginCommand::ClearAllOverlays {
                    buffer_id: BufferId(buffer_id),
                })
                .map_err(|e| mlua::Error::RuntimeError(e))
        })?;
        editor.set("clear_all_overlays", clear_all_overlays)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.remove_overlays_by_prefix(buffer_id, prefix) - Remove overlays by ID prefix
        let remove_overlays_by_prefix =
            lua.create_function(move |_, (buffer_id, prefix): (usize, String)| {
                api_clone
                    .send_command(PluginCommand::RemoveOverlaysByPrefix {
                        buffer_id: BufferId(buffer_id),
                        prefix,
                    })
                    .map_err(|e| mlua::Error::RuntimeError(e))
            })?;
        editor.set("remove_overlays_by_prefix", remove_overlays_by_prefix)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.open_file({path = "file.txt", line = 10, column = 5})
        // or editor.open_file("file.txt") -- just open without jumping
        // Line and column are 1-indexed to match git grep output
        let open_file = lua.create_function(move |_lua, args: mlua::Value| {
            // Handle both table and string arguments
            let (path, line, column) = if let mlua::Value::Table(table) = args {
                // Table format: {path = "...", line = 10, column = 5}
                let path: String = table.get("path")?;
                let line: Option<usize> = table.get("line").ok();
                let column: Option<usize> = table.get("column").ok();
                (path, line, column)
            } else if let mlua::Value::String(s) = args {
                // String format: just the path
                (s.to_str()?.to_string(), None, None)
            } else {
                return Err(mlua::Error::RuntimeError(
                    "open_file requires a table {path, line, column} or a string path".to_string(),
                ));
            };

            api_clone
                .open_file_at_location(
                    std::path::PathBuf::from(path),
                    line,
                    column,
                )
                .map_err(|e| mlua::Error::RuntimeError(e))
        })?;
        editor.set("open_file", open_file)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.start_prompt({label = "Prompt: ", prompt_type = "my-prompt"})
        // Starts a plugin-controlled prompt
        let start_prompt = lua.create_function(move |_lua, args: mlua::Table| {
            let label: String = args.get("label")?;
            let prompt_type: String = args.get("prompt_type")?;

            api_clone
                .start_prompt(label, prompt_type)
                .map_err(|e| mlua::Error::RuntimeError(e))
        })?;
        editor.set("start_prompt", start_prompt)?;

        // Clone API for next closure
        let api_clone = api.clone();

        // editor.set_prompt_suggestions({...})
        // Updates the suggestions list for the current prompt
        // Each suggestion should be a table with: {text = "...", description = "...", value = "...", disabled = false, keybinding = "..."}
        let set_prompt_suggestions = lua.create_function(move |_lua, suggestions: mlua::Table| {
            use crate::commands::Suggestion;

            let mut result_suggestions = Vec::new();

            // Iterate through the Lua table (1-indexed)
            for pair in suggestions.pairs::<usize, mlua::Table>() {
                let (_, sugg_table) = pair?;

                let text: String = sugg_table.get("text")?;
                let description: Option<String> = sugg_table.get("description").ok();
                let value: Option<String> = sugg_table.get("value").ok();
                let disabled: Option<bool> = sugg_table.get("disabled").ok();
                let keybinding: Option<String> = sugg_table.get("keybinding").ok();

                result_suggestions.push(Suggestion {
                    text,
                    description,
                    value,
                    disabled: disabled.unwrap_or(false),
                    keybinding,
                });
            }

            api_clone
                .set_prompt_suggestions(result_suggestions)
                .map_err(|e| mlua::Error::RuntimeError(e))
        })?;
        editor.set("set_prompt_suggestions", set_prompt_suggestions)?;

        // NOTE: We intentionally do NOT provide a get_buffer_content() API
        // because it would materialize the entire buffer into memory, which
        // defeats the editor's streaming architecture for huge files (GB+).
        //
        // Instead, plugins should use:
        // 1. The render-line hook to access visible line content efficiently
        // 2. Buffer metadata via get_buffer_info() for file path, length, etc.
        //
        // The example plugin buffer_query_demo.lua should be updated to reflect this.

        // Set the editor table as a global
        globals.set("editor", editor)?;

        // Create debug() global function (not part of editor table)
        let debug_log_path_clone = debug_log_path.clone();
        let debug = lua.create_function(move |_, message: String| {
            use std::io::Write;

            // Append to debug log file
            let mut file = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&debug_log_path_clone)
                .map_err(|e| {
                    mlua::Error::RuntimeError(format!("Failed to open debug log: {}", e))
                })?;

            writeln!(file, "{}", message).map_err(|e| {
                mlua::Error::RuntimeError(format!("Failed to write to debug log: {}", e))
            })?;

            // Debug messages are written to the log file but don't automatically open it
            // Users can manually open the debug log if needed

            Ok(())
        })?;
        globals.set("debug", debug)?;

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
        if let Some(_plugin) = self.plugins.remove(name) {
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
        let callbacks: mlua::Table = self
            .lua
            .globals()
            .get("_plugin_callbacks")
            .map_err(|e| format!("Failed to get callbacks table: {}", e))?;

        // Get the callback function
        let callback: Option<mlua::Function> = callbacks.get(action_name).ok();

        if let Some(cb) = callback {
            // Call the callback
            cb.call::<()>(())
                .map_err(|e| format!("Plugin callback error: {}", e))?;
            tracing::info!("Plugin action '{}' executed successfully", action_name);
            Ok(())
        } else {
            Err(format!(
                "No callback registered for action: {}",
                action_name
            ))
        }
    }

    /// Run plugin hooks for a given event
    /// This allows Lua plugins to respond to editor events
    pub fn run_hook(&self, hook_name: &str, args: &HookArgs) -> Result<(), String> {
        // Get the hooks table
        let hooks_table: mlua::Table = self
            .lua
            .globals()
            .get("_hook_callbacks")
            .map_err(|e| format!("Failed to get hooks table: {}", e))?;

        // Get the array of callbacks for this hook
        let hook_array: Option<mlua::Table> = hooks_table.get(hook_name).ok();

        if let Some(array) = hook_array {
            // Convert HookArgs to Lua table
            let args_table = self
                .hook_args_to_lua(args)
                .map_err(|e| format!("Failed to convert hook args: {}", e))?;

            // Call each callback
            let len = array
                .len()
                .map_err(|e| format!("Failed to get hook array length: {}", e))?;

            for i in 1..=len {
                let callback: Option<mlua::Function> = array.get(i).ok();
                if let Some(cb) = callback {
                    // Call the callback with args
                    // The return value can be anything (boolean, table, nil, etc.)
                    let _result: mlua::Value = cb
                        .call(args_table.clone())
                        .map_err(|e| format!("Plugin hook callback error: {}", e))?;
                    // We ignore the return value for now
                    // Future: could use it to cancel operations (if false is returned)
                }
            }
        }

        Ok(())
    }

    /// Convert HookArgs to a Lua table
    fn hook_args_to_lua(&self, args: &HookArgs) -> Result<mlua::Table, mlua::Error> {
        let table = self.lua.create_table()?;

        match args {
            HookArgs::RenderLine {
                buffer_id,
                line_number,
                byte_start,
                byte_end,
                content,
            } => {
                table.set("buffer_id", buffer_id.0)?;
                table.set("line_number", *line_number)?;
                table.set("byte_start", *byte_start)?;
                table.set("byte_end", *byte_end)?;
                table.set("content", content.as_str())?;
            }
            HookArgs::BufferActivated { buffer_id } => {
                table.set("buffer_id", buffer_id.0)?;
            }
            HookArgs::BufferDeactivated { buffer_id } => {
                table.set("buffer_id", buffer_id.0)?;
            }
            HookArgs::BufferClosed { buffer_id } => {
                table.set("buffer_id", buffer_id.0)?;
            }
            HookArgs::CursorMoved {
                buffer_id,
                cursor_id,
                old_position,
                new_position,
            } => {
                table.set("buffer_id", buffer_id.0)?;
                table.set("cursor_id", cursor_id.0)?;
                table.set("old_position", *old_position)?;
                table.set("new_position", *new_position)?;
            }
            HookArgs::BeforeInsert {
                buffer_id,
                position,
                text,
            } => {
                table.set("buffer_id", buffer_id.0)?;
                table.set("position", *position)?;
                table.set("text", text.as_str())?;
            }
            HookArgs::AfterInsert {
                buffer_id,
                position,
                text,
            } => {
                table.set("buffer_id", buffer_id.0)?;
                table.set("position", *position)?;
                table.set("text", text.as_str())?;
            }
            HookArgs::BeforeDelete { buffer_id, range } => {
                table.set("buffer_id", buffer_id.0)?;
                table.set("start", range.start)?;
                table.set("end", range.end)?;
            }
            HookArgs::AfterDelete {
                buffer_id,
                range,
                deleted_text,
            } => {
                table.set("buffer_id", buffer_id.0)?;
                table.set("start", range.start)?;
                table.set("end", range.end)?;
                table.set("deleted_text", deleted_text.as_str())?;
            }
            HookArgs::BeforeFileOpen { path } => {
                table.set("path", path.to_string_lossy().as_ref())?;
            }
            HookArgs::AfterFileOpen { path, buffer_id } => {
                table.set("path", path.to_string_lossy().as_ref())?;
                table.set("buffer_id", buffer_id.0)?;
            }
            HookArgs::BeforeFileSave { path, buffer_id } => {
                table.set("path", path.to_string_lossy().as_ref())?;
                table.set("buffer_id", buffer_id.0)?;
            }
            HookArgs::AfterFileSave { path, buffer_id } => {
                table.set("path", path.to_string_lossy().as_ref())?;
                table.set("buffer_id", buffer_id.0)?;
            }
            HookArgs::PreCommand { action } => {
                table.set("action", format!("{:?}", action))?;
            }
            HookArgs::PostCommand { action } => {
                table.set("action", format!("{:?}", action))?;
            }
            HookArgs::Idle { milliseconds } => {
                table.set("milliseconds", *milliseconds)?;
            }
            HookArgs::EditorInitialized => {
                // No args for EditorInitialized
            }
            HookArgs::PromptChanged { prompt_type, input } => {
                table.set("prompt_type", prompt_type.as_str())?;
                table.set("input", input.as_str())?;
            }
            HookArgs::PromptConfirmed {
                prompt_type,
                input,
                selected_index,
            } => {
                table.set("prompt_type", prompt_type.as_str())?;
                table.set("input", input.as_str())?;
                if let Some(idx) = selected_index {
                    table.set("selected_index", *idx)?;
                }
            }
            HookArgs::PromptCancelled { prompt_type, input } => {
                table.set("prompt_type", prompt_type.as_str())?;
                table.set("input", input.as_str())?;
            }
        }

        Ok(table)
    }

    /// Spawn an async process for a plugin
    ///
    /// This method:
    /// 1. Uses the provided callback_id to identify the callback
    /// 2. Spawns the process asynchronously via tokio
    /// 3. Returns the callback_id as process_id for tracking
    pub fn spawn_process(
        &mut self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        callback_id: u64,
    ) -> Result<u64, String> {
        // Get the async sender
        let sender = self
            .async_sender
            .as_ref()
            .ok_or_else(|| "Async bridge not initialized".to_string())?
            .clone();

        // Use callback_id as process_id (they're the same thing)
        let process_id = callback_id;

        // Spawn the process asynchronously
        tokio::spawn(crate::plugin_process::spawn_plugin_process(
            process_id, command, args, cwd, sender,
        ));

        Ok(process_id)
    }

    /// Execute a process callback when the process completes
    pub fn execute_process_callback(
        &mut self,
        callback_id: u64,
        stdout: String,
        stderr: String,
        exit_code: i32,
    ) -> Result<(), String> {
        // Get the spawn callbacks table
        let spawn_callbacks: mlua::Table = self
            .lua
            .globals()
            .get("_spawn_callbacks")
            .map_err(|e| format!("Failed to get _spawn_callbacks table: {}", e))?;

        // Get and remove the callback function
        let callback: mlua::Function = spawn_callbacks
            .get(callback_id)
            .map_err(|e| format!("No callback registered for process {}: {}", callback_id, e))?;

        // Remove from table to prevent memory leak
        spawn_callbacks
            .set(callback_id, mlua::Value::Nil)
            .map_err(|e| format!("Failed to remove callback: {}", e))?;

        // Call the callback with results
        callback
            .call::<()>((stdout, stderr, exit_code))
            .map_err(|e| format!("Process callback error: {}", e))?;

        Ok(())
    }

    /// Run a Lua snippet (for testing/debugging)
    pub fn eval(&self, code: &str) -> Result<String, String> {
        self.lua
            .load(code)
            .eval::<mlua::Value>()
            .map(|v| format!("{:?}", v))
            .map_err(|e| format!("{}", e))
    }

    /// Get access to the state snapshot for updating (used by Editor)
    pub fn state_snapshot_handle(&self) -> Arc<RwLock<EditorStateSnapshot>> {
        self.plugin_api.state_snapshot_handle()
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

    #[test]
    fn test_load_todo_highlighter() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let mut manager = PluginManager::new(hooks, commands.clone()).unwrap();

        // Load the TODO highlighter plugin
        let plugin_path = Path::new("plugins/todo_highlighter.lua");
        if plugin_path.exists() {
            let result = manager.load_plugin(plugin_path);
            assert!(
                result.is_ok(),
                "Failed to load TODO highlighter: {:?}",
                result
            );

            // Verify it's loaded
            assert!(manager.plugins.contains_key("todo_highlighter"));

            // Verify commands were registered
            let registry = commands.read().unwrap();
            assert!(registry.find_by_name("TODO Highlighter: Enable").is_some());
            assert!(registry.find_by_name("TODO Highlighter: Disable").is_some());
            assert!(registry.find_by_name("TODO Highlighter: Toggle").is_some());
            assert!(registry
                .find_by_name("TODO Highlighter: Show Keywords")
                .is_some());
        } else {
            // Skip test if plugin file doesn't exist
            println!("Skipping test - todo_highlighter.lua not found");
        }
    }

    #[test]
    fn test_get_active_buffer_id_from_lua() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let manager = PluginManager::new(hooks, commands).unwrap();

        // Set up state snapshot
        {
            let snapshot_handle = manager.state_snapshot_handle();
            let mut snapshot = snapshot_handle.write().unwrap();
            snapshot.active_buffer_id = BufferId(42);
        }

        let result = manager.eval("return editor.get_active_buffer_id()");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("42"));
    }

    #[test]
    fn test_list_buffers_from_lua() {
        use crate::plugin_api::BufferInfo;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let manager = PluginManager::new(hooks, commands).unwrap();

        // Set up state snapshot
        {
            let snapshot_handle = manager.state_snapshot_handle();
            let mut snapshot = snapshot_handle.write().unwrap();
            snapshot.buffers.insert(
                BufferId(1),
                BufferInfo {
                    id: BufferId(1),
                    path: Some(std::path::PathBuf::from("/file1.txt")),
                    modified: false,
                    length: 100,
                },
            );
            snapshot.buffers.insert(
                BufferId(2),
                BufferInfo {
                    id: BufferId(2),
                    path: Some(std::path::PathBuf::from("/file2.txt")),
                    modified: true,
                    length: 200,
                },
            );
        }

        // Test list_buffers
        let result = manager.eval("local buffers = editor.list_buffers(); return #buffers");
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.contains("2"), "Output: {}", output);

        // Test accessing buffer properties
        let result = manager.eval("local buffers = editor.list_buffers(); return buffers[1].id");
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_buffer_info_from_lua() {
        use crate::plugin_api::BufferInfo;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let manager = PluginManager::new(hooks, commands).unwrap();

        // Set up state snapshot
        {
            let snapshot_handle = manager.state_snapshot_handle();
            let mut snapshot = snapshot_handle.write().unwrap();
            snapshot.buffers.insert(
                BufferId(5),
                BufferInfo {
                    id: BufferId(5),
                    path: Some(std::path::PathBuf::from("/test.txt")),
                    modified: true,
                    length: 150,
                },
            );
        }

        // Test get_buffer_info
        let result = manager.eval("local info = editor.get_buffer_info(5); return info.id");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("5"));

        let result = manager.eval("local info = editor.get_buffer_info(5); return info.modified");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("true"));

        let result = manager.eval("local info = editor.get_buffer_info(5); return info.length");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("150"));
    }

    #[test]
    fn test_get_primary_cursor_from_lua() {
        use crate::plugin_api::CursorInfo;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let manager = PluginManager::new(hooks, commands).unwrap();

        // Set up state snapshot
        {
            let snapshot_handle = manager.state_snapshot_handle();
            let mut snapshot = snapshot_handle.write().unwrap();
            snapshot.primary_cursor = Some(CursorInfo {
                position: 123,
                selection: Some(100..123),
            });
        }

        // Test get_primary_cursor
        let result =
            manager.eval("local cursor = editor.get_primary_cursor(); return cursor.position");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("123"));

        let result = manager
            .eval("local cursor = editor.get_primary_cursor(); return cursor.selection.start");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("100"));
    }

    #[test]
    fn test_get_all_cursors_from_lua() {
        use crate::plugin_api::CursorInfo;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let manager = PluginManager::new(hooks, commands).unwrap();

        // Set up state snapshot
        {
            let snapshot_handle = manager.state_snapshot_handle();
            let mut snapshot = snapshot_handle.write().unwrap();
            snapshot.all_cursors = vec![
                CursorInfo {
                    position: 10,
                    selection: None,
                },
                CursorInfo {
                    position: 20,
                    selection: Some(15..20),
                },
                CursorInfo {
                    position: 30,
                    selection: None,
                },
            ];
        }

        // Test get_all_cursors
        let result = manager.eval("local cursors = editor.get_all_cursors(); return #cursors");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("3"));

        let result =
            manager.eval("local cursors = editor.get_all_cursors(); return cursors[2].position");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("20"));
    }

    #[test]
    fn test_get_viewport_from_lua() {
        use crate::plugin_api::ViewportInfo;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let manager = PluginManager::new(hooks, commands).unwrap();

        // Set up state snapshot
        {
            let snapshot_handle = manager.state_snapshot_handle();
            let mut snapshot = snapshot_handle.write().unwrap();
            snapshot.viewport = Some(ViewportInfo {
                top_byte: 500,
                left_column: 10,
                width: 120,
                height: 40,
            });
        }

        // Test get_viewport
        let result = manager.eval("local vp = editor.get_viewport(); return vp.top_byte");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("500"));

        let result = manager.eval("local vp = editor.get_viewport(); return vp.width");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("120"));

        let result = manager.eval("local vp = editor.get_viewport(); return vp.height");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("40"));
    }

    #[test]
    fn test_spawn_from_lua() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let mut manager = PluginManager::new(hooks, commands).unwrap();

        // Set up async bridge (normally done by editor)
        let (sender, _receiver) = std::sync::mpsc::channel();
        manager.set_async_sender(sender);

        // Test that spawn function exists and can be called
        let result = manager.eval(
            r#"
            local callback_called = false
            editor.spawn("echo", {"hello"}, function(stdout, stderr, exit_code)
                callback_called = true
            end)
            return "spawn_called"
        "#,
        );

        assert!(result.is_ok());
        assert!(result.unwrap().contains("spawn_called"));

        // Check that the callback was stored in _spawn_callbacks
        let result = manager.eval(
            r#"
            local count = 0
            for _ in pairs(_spawn_callbacks) do count = count + 1 end
            return count
        "#,
        );

        assert!(result.is_ok());
        assert!(result.unwrap().contains("1"), "Expected 1 callback stored");
    }

    #[test]
    fn test_spawn_with_cwd_from_lua() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let mut manager = PluginManager::new(hooks, commands).unwrap();

        // Set up async bridge
        let (sender, _receiver) = std::sync::mpsc::channel();
        manager.set_async_sender(sender);

        // Test spawn with working directory option
        let result = manager.eval(
            r#"
            editor.spawn("pwd", {}, {cwd = "/tmp"}, function(stdout, stderr, exit_code)
                -- callback
            end)
            return "spawn_with_cwd_called"
        "#,
        );

        assert!(result.is_ok());
        assert!(result.unwrap().contains("spawn_with_cwd_called"));
    }
}
