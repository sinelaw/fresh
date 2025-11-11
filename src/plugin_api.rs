//! Plugin API: Safe interface for plugins to interact with the editor
//!
//! This module provides a safe, controlled API for plugins (Lua, WASM, etc.)
//! to interact with the editor without direct access to internal state.

use crate::command_registry::CommandRegistry;
use crate::commands::Command;
use crate::event::BufferId;
use crate::hooks::{HookCallback, HookRegistry};
use std::collections::HashMap;
use std::ops::Range;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

/// Information about a cursor in the editor
#[derive(Debug, Clone)]
pub struct CursorInfo {
    /// Byte position of the cursor
    pub position: usize,
    /// Selection range (if any)
    pub selection: Option<Range<usize>>,
}

/// Information about a buffer
#[derive(Debug, Clone)]
pub struct BufferInfo {
    /// Buffer ID
    pub id: BufferId,
    /// File path (if any)
    pub path: Option<PathBuf>,
    /// Whether the buffer has been modified
    pub modified: bool,
    /// Length of buffer in bytes
    pub length: usize,
}

/// Information about the viewport
#[derive(Debug, Clone)]
pub struct ViewportInfo {
    /// Byte position of the first visible line
    pub top_byte: usize,
    /// Left column offset (horizontal scroll)
    pub left_column: usize,
    /// Viewport width
    pub width: u16,
    /// Viewport height
    pub height: u16,
}

/// Snapshot of editor state for plugin queries
/// This is updated by the editor on each loop iteration
#[derive(Debug, Clone)]
pub struct EditorStateSnapshot {
    /// Currently active buffer ID
    pub active_buffer_id: BufferId,
    /// Information about all open buffers
    pub buffers: HashMap<BufferId, BufferInfo>,
    /// Primary cursor position for the active buffer
    pub primary_cursor: Option<CursorInfo>,
    /// All cursor positions for the active buffer
    pub all_cursors: Vec<CursorInfo>,
    /// Viewport information for the active buffer
    pub viewport: Option<ViewportInfo>,
}

impl EditorStateSnapshot {
    pub fn new() -> Self {
        Self {
            active_buffer_id: BufferId(0),
            buffers: HashMap::new(),
            primary_cursor: None,
            all_cursors: Vec::new(),
            viewport: None,
        }
    }
}

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

    /// Open a file in the editor (in background, without switching focus)
    OpenFileInBackground { path: PathBuf },

    /// Insert text at the current cursor position in the active buffer
    InsertAtCursor { text: String },

    /// Spawn an async process
    SpawnProcess {
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        callback_id: u64, // ID to look up callback in _spawn_callbacks Lua table
    },

    /// Remove all overlays from a buffer
    ClearAllOverlays { buffer_id: BufferId },

    /// Remove overlays whose ID starts with the given prefix
    RemoveOverlaysByPrefix { buffer_id: BufferId, prefix: String },
}

/// Plugin API context - provides safe access to editor functionality
pub struct PluginApi {
    /// Hook registry (shared with editor)
    hooks: Arc<RwLock<HookRegistry>>,

    /// Command registry (shared with editor)
    commands: Arc<RwLock<CommandRegistry>>,

    /// Command queue for sending commands to editor
    command_sender: std::sync::mpsc::Sender<PluginCommand>,

    /// Snapshot of editor state (read-only for plugins)
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
}

impl PluginApi {
    /// Create a new plugin API context
    pub fn new(
        hooks: Arc<RwLock<HookRegistry>>,
        commands: Arc<RwLock<CommandRegistry>>,
        command_sender: std::sync::mpsc::Sender<PluginCommand>,
        state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
    ) -> Self {
        Self {
            hooks,
            commands,
            command_sender,
            state_snapshot,
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
    pub fn delete_range(&self, buffer_id: BufferId, range: Range<usize>) -> Result<(), String> {
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
    pub fn remove_overlay(&self, buffer_id: BufferId, overlay_id: String) -> Result<(), String> {
        self.send_command(PluginCommand::RemoveOverlay {
            buffer_id,
            overlay_id,
        })
    }

    /// Set the status message
    pub fn set_status(&self, message: String) -> Result<(), String> {
        self.send_command(PluginCommand::SetStatus { message })
    }

    // === Query Methods ===

    /// Get the currently active buffer ID
    pub fn get_active_buffer_id(&self) -> BufferId {
        let snapshot = self.state_snapshot.read().unwrap();
        snapshot.active_buffer_id
    }

    /// Get information about a specific buffer
    pub fn get_buffer_info(&self, buffer_id: BufferId) -> Option<BufferInfo> {
        let snapshot = self.state_snapshot.read().unwrap();
        snapshot.buffers.get(&buffer_id).cloned()
    }

    /// Get all buffer IDs
    pub fn list_buffers(&self) -> Vec<BufferInfo> {
        let snapshot = self.state_snapshot.read().unwrap();
        snapshot.buffers.values().cloned().collect()
    }

    /// Get primary cursor information for the active buffer
    pub fn get_primary_cursor(&self) -> Option<CursorInfo> {
        let snapshot = self.state_snapshot.read().unwrap();
        snapshot.primary_cursor.clone()
    }

    /// Get all cursor information for the active buffer
    pub fn get_all_cursors(&self) -> Vec<CursorInfo> {
        let snapshot = self.state_snapshot.read().unwrap();
        snapshot.all_cursors.clone()
    }

    /// Get viewport information for the active buffer
    pub fn get_viewport(&self) -> Option<ViewportInfo> {
        let snapshot = self.state_snapshot.read().unwrap();
        snapshot.viewport.clone()
    }

    /// Get access to the state snapshot Arc (for internal use)
    pub fn state_snapshot_handle(&self) -> Arc<RwLock<EditorStateSnapshot>> {
        Arc::clone(&self.state_snapshot)
    }
}

impl Clone for PluginApi {
    fn clone(&self) -> Self {
        Self {
            hooks: Arc::clone(&self.hooks),
            commands: Arc::clone(&self.commands),
            command_sender: self.command_sender.clone(),
            state_snapshot: Arc::clone(&self.state_snapshot),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plugin_api_creation() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

        // Should not panic
        let _clone = api.clone();
    }

    #[test]
    fn test_register_hook() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        let api = PluginApi::new(hooks.clone(), commands, tx, state_snapshot);

        api.register_hook("test-hook", Box::new(|_| true));

        let hook_registry = hooks.read().unwrap();
        assert_eq!(hook_registry.hook_count("test-hook"), 1);
    }

    #[test]
    fn test_send_command() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

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
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

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
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

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

    #[test]
    fn test_get_active_buffer_id() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Set active buffer to 5
        {
            let mut snapshot = state_snapshot.write().unwrap();
            snapshot.active_buffer_id = BufferId(5);
        }

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

        let active_id = api.get_active_buffer_id();
        assert_eq!(active_id.0, 5);
    }

    #[test]
    fn test_get_buffer_info() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Add buffer info
        {
            let mut snapshot = state_snapshot.write().unwrap();
            let buffer_info = BufferInfo {
                id: BufferId(1),
                path: Some(std::path::PathBuf::from("/test/file.txt")),
                modified: true,
                length: 100,
            };
            snapshot.buffers.insert(BufferId(1), buffer_info);
        }

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

        let info = api.get_buffer_info(BufferId(1));
        assert!(info.is_some());
        let info = info.unwrap();
        assert_eq!(info.id.0, 1);
        assert_eq!(
            info.path.as_ref().unwrap().to_str().unwrap(),
            "/test/file.txt"
        );
        assert!(info.modified);
        assert_eq!(info.length, 100);

        // Non-existent buffer
        let no_info = api.get_buffer_info(BufferId(999));
        assert!(no_info.is_none());
    }

    #[test]
    fn test_list_buffers() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Add multiple buffers
        {
            let mut snapshot = state_snapshot.write().unwrap();
            snapshot.buffers.insert(
                BufferId(1),
                BufferInfo {
                    id: BufferId(1),
                    path: Some(std::path::PathBuf::from("/file1.txt")),
                    modified: false,
                    length: 50,
                },
            );
            snapshot.buffers.insert(
                BufferId(2),
                BufferInfo {
                    id: BufferId(2),
                    path: Some(std::path::PathBuf::from("/file2.txt")),
                    modified: true,
                    length: 100,
                },
            );
            snapshot.buffers.insert(
                BufferId(3),
                BufferInfo {
                    id: BufferId(3),
                    path: None,
                    modified: false,
                    length: 0,
                },
            );
        }

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

        let buffers = api.list_buffers();
        assert_eq!(buffers.len(), 3);

        // Verify all buffers are present
        assert!(buffers.iter().any(|b| b.id.0 == 1));
        assert!(buffers.iter().any(|b| b.id.0 == 2));
        assert!(buffers.iter().any(|b| b.id.0 == 3));
    }

    #[test]
    fn test_get_primary_cursor() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Add cursor info
        {
            let mut snapshot = state_snapshot.write().unwrap();
            snapshot.primary_cursor = Some(CursorInfo {
                position: 42,
                selection: Some(10..42),
            });
        }

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

        let cursor = api.get_primary_cursor();
        assert!(cursor.is_some());
        let cursor = cursor.unwrap();
        assert_eq!(cursor.position, 42);
        assert_eq!(cursor.selection, Some(10..42));
    }

    #[test]
    fn test_get_all_cursors() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Add multiple cursors
        {
            let mut snapshot = state_snapshot.write().unwrap();
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
                    selection: Some(25..30),
                },
            ];
        }

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

        let cursors = api.get_all_cursors();
        assert_eq!(cursors.len(), 3);
        assert_eq!(cursors[0].position, 10);
        assert_eq!(cursors[0].selection, None);
        assert_eq!(cursors[1].position, 20);
        assert_eq!(cursors[1].selection, Some(15..20));
        assert_eq!(cursors[2].position, 30);
        assert_eq!(cursors[2].selection, Some(25..30));
    }

    #[test]
    fn test_get_viewport() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Add viewport info
        {
            let mut snapshot = state_snapshot.write().unwrap();
            snapshot.viewport = Some(ViewportInfo {
                top_byte: 100,
                left_column: 5,
                width: 80,
                height: 24,
            });
        }

        let api = PluginApi::new(hooks, commands, tx, state_snapshot);

        let viewport = api.get_viewport();
        assert!(viewport.is_some());
        let viewport = viewport.unwrap();
        assert_eq!(viewport.top_byte, 100);
        assert_eq!(viewport.left_column, 5);
        assert_eq!(viewport.width, 80);
        assert_eq!(viewport.height, 24);
    }
}
