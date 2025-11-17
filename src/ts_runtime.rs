//! TypeScript Plugin Runtime
//!
//! This module provides a TypeScript/JavaScript runtime for plugins using deno_core.
//! It enables native async/await support, solving the async command execution problem
//! that existed with the Lua plugin system.

use crate::event::BufferId;
use crate::plugin_api::{EditorStateSnapshot, PluginCommand};
use anyhow::{anyhow, Result};
use deno_core::{extension, op2, OpState, FastString, JsRuntime, RuntimeOptions};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, RwLock};

/// Shared state accessible from ops
struct TsRuntimeState {
    /// Editor state snapshot (read-only access)
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
    /// Command sender for write operations
    command_sender: std::sync::mpsc::Sender<PluginCommand>,
}

/// Custom ops for the Fresh editor API
#[op2(fast)]
fn op_fresh_set_status(state: &mut OpState, #[string] message: String) {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let _ = runtime_state.command_sender.send(PluginCommand::SetStatus {
            message: message.clone(),
        });
    }
    tracing::info!("TypeScript plugin set_status: {}", message);
}

#[op2(fast)]
fn op_fresh_debug(#[string] message: String) {
    tracing::debug!("TypeScript plugin: {}", message);
}

#[op2(fast)]
fn op_fresh_get_active_buffer_id(state: &mut OpState) -> u32 {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot.active_buffer_id.0 as u32;
        };
    }
    0
}

#[op2(fast)]
fn op_fresh_get_cursor_position(state: &mut OpState) -> u32 {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(ref cursor) = snapshot.primary_cursor {
                return cursor.position as u32;
            }
        };
    }
    0
}

#[op2]
#[string]
fn op_fresh_get_buffer_path(state: &mut OpState, buffer_id: u32) -> String {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(info) = snapshot.buffers.get(&BufferId(buffer_id as usize)) {
                if let Some(ref path) = info.path {
                    return path.to_string_lossy().to_string();
                }
            }
        };
    }
    String::new()
}

#[op2(fast)]
fn op_fresh_get_buffer_length(state: &mut OpState, buffer_id: u32) -> u32 {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(info) = snapshot.buffers.get(&BufferId(buffer_id as usize)) {
                return info.length as u32;
            }
        };
    }
    0
}

#[op2(fast)]
fn op_fresh_is_buffer_modified(state: &mut OpState, buffer_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(info) = snapshot.buffers.get(&BufferId(buffer_id as usize)) {
                return info.modified;
            }
        };
    }
    false
}

#[op2(fast)]
fn op_fresh_insert_text(
    state: &mut OpState,
    buffer_id: u32,
    position: u32,
    #[string] text: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state.command_sender.send(PluginCommand::InsertText {
            buffer_id: BufferId(buffer_id as usize),
            position: position as usize,
            text,
        });
        return result.is_ok();
    }
    false
}

#[op2(fast)]
fn op_fresh_delete_range(
    state: &mut OpState,
    buffer_id: u32,
    start: u32,
    end: u32,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state.command_sender.send(PluginCommand::DeleteRange {
            buffer_id: BufferId(buffer_id as usize),
            range: (start as usize)..(end as usize),
        });
        return result.is_ok();
    }
    false
}

#[op2(fast)]
fn op_fresh_add_overlay(
    state: &mut OpState,
    buffer_id: u32,
    #[string] overlay_id: String,
    start: u32,
    end: u32,
    r: u8,
    g: u8,
    b: u8,
    underline: bool,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state.command_sender.send(PluginCommand::AddOverlay {
            buffer_id: BufferId(buffer_id as usize),
            overlay_id,
            range: (start as usize)..(end as usize),
            color: (r, g, b),
            underline,
        });
        return result.is_ok();
    }
    false
}

#[op2(fast)]
fn op_fresh_remove_overlay(
    state: &mut OpState,
    buffer_id: u32,
    #[string] overlay_id: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state.command_sender.send(PluginCommand::RemoveOverlay {
            buffer_id: BufferId(buffer_id as usize),
            overlay_id,
        });
        return result.is_ok();
    }
    false
}

#[op2(fast)]
fn op_fresh_remove_overlays_by_prefix(
    state: &mut OpState,
    buffer_id: u32,
    #[string] prefix: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::RemoveOverlaysByPrefix {
                buffer_id: BufferId(buffer_id as usize),
                prefix,
            });
        return result.is_ok();
    }
    false
}

#[op2(fast)]
fn op_fresh_clear_all_overlays(state: &mut OpState, buffer_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ClearAllOverlays {
                buffer_id: BufferId(buffer_id as usize),
            });
        return result.is_ok();
    }
    false
}

#[op2(fast)]
fn op_fresh_insert_at_cursor(state: &mut OpState, #[string] text: String) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::InsertAtCursor { text });
        return result.is_ok();
    }
    false
}

#[op2(fast)]
fn op_fresh_register_command(
    state: &mut OpState,
    #[string] name: String,
    #[string] description: String,
    #[string] action: String,
    #[string] contexts: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();

        // Parse contexts string (comma-separated, e.g., "normal,prompt,popup")
        let context_list: Vec<crate::keybindings::KeyContext> = if contexts.trim().is_empty() {
            vec![] // Empty = available in all contexts
        } else {
            contexts
                .split(',')
                .filter_map(|s| match s.trim().to_lowercase().as_str() {
                    "global" => Some(crate::keybindings::KeyContext::Global),
                    "normal" => Some(crate::keybindings::KeyContext::Normal),
                    "help" => Some(crate::keybindings::KeyContext::Help),
                    "prompt" => Some(crate::keybindings::KeyContext::Prompt),
                    "popup" => Some(crate::keybindings::KeyContext::Popup),
                    "fileexplorer" | "file_explorer" => {
                        Some(crate::keybindings::KeyContext::FileExplorer)
                    }
                    "menu" => Some(crate::keybindings::KeyContext::Menu),
                    _ => None,
                })
                .collect()
        };

        let command = crate::commands::Command {
            name: name.clone(),
            description,
            action: crate::keybindings::Action::PluginAction(action),
            contexts: context_list,
        };

        let result = runtime_state
            .command_sender
            .send(PluginCommand::RegisterCommand { command });
        return result.is_ok();
    }
    false
}

#[op2(fast)]
fn op_fresh_open_file(
    state: &mut OpState,
    #[string] path: String,
    line: u32,
    column: u32,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::OpenFileAtLocation {
                path: std::path::PathBuf::from(path),
                line: if line == 0 { None } else { Some(line as usize) },
                column: if column == 0 {
                    None
                } else {
                    Some(column as usize)
                },
            });
        return result.is_ok();
    }
    false
}

#[op2(fast)]
fn op_fresh_get_active_split_id(state: &mut OpState) -> u32 {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot.active_split_id as u32;
        };
    }
    0
}

#[op2(fast)]
fn op_fresh_open_file_in_split(
    state: &mut OpState,
    split_id: u32,
    #[string] path: String,
    line: u32,
    column: u32,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::OpenFileInSplit {
                split_id: split_id as usize,
                path: std::path::PathBuf::from(path),
                line: if line == 0 { None } else { Some(line as usize) },
                column: if column == 0 {
                    None
                } else {
                    Some(column as usize)
                },
            });
        return result.is_ok();
    }
    false
}

/// Result of spawning a process
#[derive(serde::Serialize)]
struct SpawnResult {
    stdout: String,
    stderr: String,
    exit_code: i32,
}

/// Async op for spawning external processes
/// This is the key async op that enables TypeScript plugins to run shell commands
#[op2(async)]
#[serde]
async fn op_fresh_spawn_process(
    #[string] command: String,
    #[serde] args: Vec<String>,
    #[string] cwd: Option<String>,
) -> Result<SpawnResult, deno_core::error::AnyError> {
    use std::process::Stdio;
    use tokio::io::{AsyncBufReadExt, BufReader};
    use tokio::process::Command;

    // Build the command
    let mut cmd = Command::new(&command);
    cmd.args(&args);
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    // Set working directory if provided
    if let Some(ref dir) = cwd {
        cmd.current_dir(dir);
    }

    // Spawn the process
    let mut child = cmd
        .spawn()
        .map_err(|e| deno_core::error::generic_error(format!("Failed to spawn process: {}", e)))?;

    // Capture stdout and stderr
    let stdout_handle = child.stdout.take();
    let stderr_handle = child.stderr.take();

    // Read stdout
    let stdout_future = async {
        if let Some(stdout) = stdout_handle {
            let reader = BufReader::new(stdout);
            let mut lines = reader.lines();
            let mut output = String::new();

            while let Ok(Some(line)) = lines.next_line().await {
                output.push_str(&line);
                output.push('\n');
            }
            output
        } else {
            String::new()
        }
    };

    // Read stderr
    let stderr_future = async {
        if let Some(stderr) = stderr_handle {
            let reader = BufReader::new(stderr);
            let mut lines = reader.lines();
            let mut output = String::new();

            while let Ok(Some(line)) = lines.next_line().await {
                output.push_str(&line);
                output.push('\n');
            }
            output
        } else {
            String::new()
        }
    };

    // Wait for both outputs concurrently
    let (stdout, stderr) = tokio::join!(stdout_future, stderr_future);

    // Wait for process to complete
    let exit_code = match child.wait().await {
        Ok(status) => status.code().unwrap_or(-1),
        Err(_) => -1,
    };

    Ok(SpawnResult {
        stdout,
        stderr,
        exit_code,
    })
}

// Define the extension with our ops
extension!(
    fresh_runtime,
    ops = [
        op_fresh_set_status,
        op_fresh_debug,
        op_fresh_get_active_buffer_id,
        op_fresh_get_cursor_position,
        op_fresh_get_buffer_path,
        op_fresh_get_buffer_length,
        op_fresh_is_buffer_modified,
        op_fresh_insert_text,
        op_fresh_delete_range,
        op_fresh_add_overlay,
        op_fresh_remove_overlay,
        op_fresh_remove_overlays_by_prefix,
        op_fresh_clear_all_overlays,
        op_fresh_insert_at_cursor,
        op_fresh_register_command,
        op_fresh_open_file,
        op_fresh_get_active_split_id,
        op_fresh_open_file_in_split,
        op_fresh_spawn_process,
    ],
);

/// TypeScript plugin runtime
pub struct TypeScriptRuntime {
    js_runtime: JsRuntime,
}

impl TypeScriptRuntime {
    /// Create a new TypeScript runtime (standalone, for testing)
    pub fn new() -> Result<Self> {
        // Create dummy state for standalone testing
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        Self::with_state(state_snapshot, tx)
    }

    /// Create a new TypeScript runtime with editor state
    pub fn with_state(
        state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
        command_sender: std::sync::mpsc::Sender<PluginCommand>,
    ) -> Result<Self> {
        let runtime_state = Rc::new(RefCell::new(TsRuntimeState {
            state_snapshot,
            command_sender,
        }));

        let mut js_runtime = JsRuntime::new(RuntimeOptions {
            module_loader: Some(Rc::new(deno_core::FsModuleLoader)),
            extensions: vec![fresh_runtime::init_ops()],
            ..Default::default()
        });

        // Store the runtime state in the op state
        js_runtime.op_state().borrow_mut().put(runtime_state);

        // Set up the global editor API
        js_runtime
            .execute_script(
                "<fresh_bootstrap>",
                r#"
                const core = Deno.core;

                // Create the editor API object
                const editor = {
                    // Status and logging
                    setStatus(message) {
                        core.ops.op_fresh_set_status(message);
                    },
                    debug(message) {
                        core.ops.op_fresh_debug(message);
                    },

                    // Buffer queries
                    getActiveBufferId() {
                        return core.ops.op_fresh_get_active_buffer_id();
                    },
                    getCursorPosition() {
                        return core.ops.op_fresh_get_cursor_position();
                    },
                    getBufferPath(bufferId) {
                        return core.ops.op_fresh_get_buffer_path(bufferId);
                    },
                    getBufferLength(bufferId) {
                        return core.ops.op_fresh_get_buffer_length(bufferId);
                    },
                    isBufferModified(bufferId) {
                        return core.ops.op_fresh_is_buffer_modified(bufferId);
                    },

                    // Buffer mutations
                    insertText(bufferId, position, text) {
                        return core.ops.op_fresh_insert_text(bufferId, position, text);
                    },
                    deleteRange(bufferId, start, end) {
                        return core.ops.op_fresh_delete_range(bufferId, start, end);
                    },

                    // Overlays
                    addOverlay(bufferId, overlayId, start, end, r, g, b, underline) {
                        return core.ops.op_fresh_add_overlay(bufferId, overlayId, start, end, r, g, b, underline);
                    },
                    removeOverlay(bufferId, overlayId) {
                        return core.ops.op_fresh_remove_overlay(bufferId, overlayId);
                    },
                    removeOverlaysByPrefix(bufferId, prefix) {
                        return core.ops.op_fresh_remove_overlays_by_prefix(bufferId, prefix);
                    },
                    clearAllOverlays(bufferId) {
                        return core.ops.op_fresh_clear_all_overlays(bufferId);
                    },

                    // Convenience
                    insertAtCursor(text) {
                        return core.ops.op_fresh_insert_at_cursor(text);
                    },

                    // Command registration
                    registerCommand(name, description, action, contexts = "") {
                        return core.ops.op_fresh_register_command(name, description, action, contexts);
                    },

                    // File operations
                    openFile(path, line = 0, column = 0) {
                        return core.ops.op_fresh_open_file(path, line, column);
                    },

                    // Split operations
                    getActiveSplitId() {
                        return core.ops.op_fresh_get_active_split_id();
                    },
                    openFileInSplit(splitId, path, line = 0, column = 0) {
                        return core.ops.op_fresh_open_file_in_split(splitId, path, line, column);
                    },

                    // Async operations
                    spawnProcess(command, args = [], cwd = null) {
                        return core.ops.op_fresh_spawn_process(command, args, cwd);
                    },
                };

                // Make editor globally available
                globalThis.editor = editor;
                "#
                .to_string(),
            )
            .map_err(|e| anyhow!("Failed to initialize editor API: {}", e))?;

        Ok(Self { js_runtime })
    }

    /// Execute JavaScript code directly
    pub async fn execute_script(&mut self, name: &'static str, code: &str) -> Result<()> {
        // Code needs to be FastString for the IntoModuleCodeString trait
        let code_static: FastString = code.to_string().into();
        self.js_runtime
            .execute_script(name, code_static)
            .map_err(|e| anyhow!("Failed to execute script '{}': {}", name, e))?;

        // Run the event loop to process any pending async operations
        self.js_runtime
            .run_event_loop(Default::default())
            .await
            .map_err(|e| anyhow!("Event loop error: {}", e))?;

        Ok(())
    }

    /// Load and execute a TypeScript/JavaScript module file
    pub async fn load_module(&mut self, path: &str) -> Result<()> {
        let main_module = deno_core::resolve_path(
            path,
            &std::env::current_dir().map_err(|e| anyhow!("Failed to get cwd: {}", e))?,
        )
        .map_err(|e| anyhow!("Failed to resolve module path '{}': {}", path, e))?;

        let mod_id = self
            .js_runtime
            .load_main_es_module(&main_module)
            .await
            .map_err(|e| anyhow!("Failed to load module '{}': {}", path, e))?;

        let result = self.js_runtime.mod_evaluate(mod_id);

        self.js_runtime
            .run_event_loop(Default::default())
            .await
            .map_err(|e| anyhow!("Event loop error while loading module: {}", e))?;

        result
            .await
            .map_err(|e| anyhow!("Module evaluation error: {}", e))?;

        Ok(())
    }

    /// Execute a global function by name (for plugin actions)
    pub async fn execute_action(&mut self, action_name: &str) -> Result<()> {
        let code = format!(
            r#"
            (async () => {{
                if (typeof globalThis.{} === 'function') {{
                    const result = globalThis.{}();
                    if (result instanceof Promise) {{
                        await result;
                    }}
                }} else {{
                    throw new Error('Action "{}" is not defined as a global function');
                }}
            }})();
            "#,
            action_name, action_name, action_name
        );

        self.execute_script("<action>", &code).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_typescript_runtime_creation() {
        let runtime = TypeScriptRuntime::new();
        assert!(runtime.is_ok(), "Failed to create TypeScript runtime");
    }

    #[tokio::test]
    async fn test_execute_simple_script() {
        let mut runtime = TypeScriptRuntime::new().unwrap();
        let result = runtime
            .execute_script("<test>", "const x = 1 + 1; console.log('Result:', x);")
            .await;
        assert!(result.is_ok(), "Failed to execute simple script: {:?}", result);
    }

    #[tokio::test]
    async fn test_call_fresh_ops() {
        let mut runtime = TypeScriptRuntime::new().unwrap();
        let result = runtime
            .execute_script(
                "<test_ops>",
                r#"
                Deno.core.ops.op_fresh_set_status("Hello from TypeScript!");
                Deno.core.ops.op_fresh_debug("Debug message");
                const bufferId = Deno.core.ops.op_fresh_get_active_buffer_id();
                console.log("Buffer ID:", bufferId);
                "#,
            )
            .await;
        assert!(result.is_ok(), "Failed to call Fresh ops: {:?}", result);
    }

    #[tokio::test]
    async fn test_async_await() {
        let mut runtime = TypeScriptRuntime::new().unwrap();
        let result = runtime
            .execute_script(
                "<test_async>",
                r#"
                async function testAsync() {
                    const result = await Promise.resolve(42);
                    console.log("Async result:", result);
                    return result;
                }
                testAsync();
                "#,
            )
            .await;
        assert!(result.is_ok(), "Failed to execute async code: {:?}", result);
    }

    #[tokio::test]
    async fn test_execute_action() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Define a global function
        runtime
            .execute_script(
                "<define_action>",
                r#"
                globalThis.my_test_action = function() {
                    Deno.core.ops.op_fresh_set_status("Action executed!");
                };
                "#,
            )
            .await
            .unwrap();

        // Execute the action
        let result = runtime.execute_action("my_test_action").await;
        assert!(result.is_ok(), "Failed to execute action: {:?}", result);
    }

    #[tokio::test]
    async fn test_execute_async_action() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Define an async global function (using Promise.resolve instead of setTimeout)
        runtime
            .execute_script(
                "<define_async_action>",
                r#"
                globalThis.my_async_action = async function() {
                    const result = await Promise.resolve("async data");
                    Deno.core.ops.op_fresh_set_status("Async action completed with: " + result);
                };
                "#,
            )
            .await
            .unwrap();

        // Execute the async action
        let result = runtime.execute_action("my_async_action").await;
        assert!(result.is_ok(), "Failed to execute async action: {:?}", result);
    }

    #[tokio::test]
    async fn test_with_editor_state() {
        use crate::plugin_api::{BufferInfo, CursorInfo};
        use std::path::PathBuf;

        // Create shared state
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Populate state with test data
        {
            let mut snapshot = state_snapshot.write().unwrap();
            snapshot.active_buffer_id = BufferId(42);
            snapshot.buffers.insert(
                BufferId(42),
                BufferInfo {
                    id: BufferId(42),
                    path: Some(PathBuf::from("/test/file.rs")),
                    modified: true,
                    length: 1000,
                },
            );
            snapshot.primary_cursor = Some(CursorInfo {
                position: 100,
                selection: None,
            });
        }

        // Create runtime with state
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot.clone(), tx).unwrap();

        // Test querying state from TypeScript
        let result = runtime
            .execute_script(
                "<test_state>",
                r#"
                // Test buffer queries
                const bufferId = editor.getActiveBufferId();
                if (bufferId !== 42) {
                    throw new Error(`Expected buffer ID 42, got ${bufferId}`);
                }

                const path = editor.getBufferPath(bufferId);
                if (path !== "/test/file.rs") {
                    throw new Error(`Expected path /test/file.rs, got ${path}`);
                }

                const length = editor.getBufferLength(bufferId);
                if (length !== 1000) {
                    throw new Error(`Expected length 1000, got ${length}`);
                }

                const modified = editor.isBufferModified(bufferId);
                if (!modified) {
                    throw new Error("Expected buffer to be modified");
                }

                const cursorPos = editor.getCursorPosition();
                if (cursorPos !== 100) {
                    throw new Error(`Expected cursor at 100, got ${cursorPos}`);
                }

                console.log("All state queries passed!");
                "#,
            )
            .await;
        assert!(result.is_ok(), "State query test failed: {:?}", result);

        // Test sending commands from TypeScript
        let result = runtime
            .execute_script(
                "<test_commands>",
                r#"
                // Test status command
                editor.setStatus("Test status from TypeScript");

                // Test insert text
                const insertSuccess = editor.insertText(42, 50, "Hello, World!");
                if (!insertSuccess) {
                    throw new Error("Insert text failed");
                }

                // Test delete range
                const deleteSuccess = editor.deleteRange(42, 10, 20);
                if (!deleteSuccess) {
                    throw new Error("Delete range failed");
                }

                // Test overlay
                const overlaySuccess = editor.addOverlay(42, "test-overlay", 0, 50, 255, 0, 0, true);
                if (!overlaySuccess) {
                    throw new Error("Add overlay failed");
                }

                const removeSuccess = editor.removeOverlay(42, "test-overlay");
                if (!removeSuccess) {
                    throw new Error("Remove overlay failed");
                }

                console.log("All commands sent successfully!");
                "#,
            )
            .await;
        assert!(result.is_ok(), "Command test failed: {:?}", result);

        // Verify commands were received
        let commands: Vec<_> = rx.try_iter().collect();
        assert_eq!(commands.len(), 5, "Expected 5 commands");

        // Check command types
        match &commands[0] {
            PluginCommand::SetStatus { message } => {
                assert_eq!(message, "Test status from TypeScript");
            }
            _ => panic!("Expected SetStatus command"),
        }

        match &commands[1] {
            PluginCommand::InsertText {
                buffer_id,
                position,
                text,
            } => {
                assert_eq!(buffer_id.0, 42);
                assert_eq!(*position, 50);
                assert_eq!(text, "Hello, World!");
            }
            _ => panic!("Expected InsertText command"),
        }

        match &commands[2] {
            PluginCommand::DeleteRange { buffer_id, range } => {
                assert_eq!(buffer_id.0, 42);
                assert_eq!(range.start, 10);
                assert_eq!(range.end, 20);
            }
            _ => panic!("Expected DeleteRange command"),
        }

        match &commands[3] {
            PluginCommand::AddOverlay {
                buffer_id,
                overlay_id,
                range,
                color,
                underline,
            } => {
                assert_eq!(buffer_id.0, 42);
                assert_eq!(overlay_id, "test-overlay");
                assert_eq!(range.start, 0);
                assert_eq!(range.end, 50);
                assert_eq!(*color, (255, 0, 0));
                assert!(*underline);
            }
            _ => panic!("Expected AddOverlay command"),
        }

        match &commands[4] {
            PluginCommand::RemoveOverlay {
                buffer_id,
                overlay_id,
            } => {
                assert_eq!(buffer_id.0, 42);
                assert_eq!(overlay_id, "test-overlay");
            }
            _ => panic!("Expected RemoveOverlay command"),
        }
    }

    #[tokio::test]
    async fn test_editor_api_accessible() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Test that the editor API is accessible and well-formed
        let result = runtime
            .execute_script(
                "<test_api>",
                r#"
                // Verify all API methods exist
                const methods = [
                    'setStatus', 'debug', 'getActiveBufferId', 'getCursorPosition',
                    'getBufferPath', 'getBufferLength', 'isBufferModified',
                    'insertText', 'deleteRange', 'addOverlay', 'removeOverlay'
                ];

                for (const method of methods) {
                    if (typeof editor[method] !== 'function') {
                        throw new Error(`editor.${method} is not a function`);
                    }
                }

                console.log("All editor API methods are present!");
                "#,
            )
            .await;
        assert!(result.is_ok(), "API accessibility test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_new_ops() {
        use std::path::PathBuf;

        // Create shared state
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Populate state with test data including split ID
        {
            let mut snapshot = state_snapshot.write().unwrap();
            snapshot.active_buffer_id = BufferId(1);
            snapshot.active_split_id = 5;
        }

        // Create runtime with state
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot.clone(), tx).unwrap();

        // Test new ops from TypeScript
        let result = runtime
            .execute_script(
                "<test_new_ops>",
                r#"
                // Test getActiveSplitId
                const splitId = editor.getActiveSplitId();
                if (splitId !== 5) {
                    throw new Error(`Expected split ID 5, got ${splitId}`);
                }

                // Test registerCommand
                const regSuccess = editor.registerCommand(
                    "My Plugin Command",
                    "A test command from TypeScript",
                    "my_plugin_action",
                    "normal,prompt"
                );
                if (!regSuccess) {
                    throw new Error("Register command failed");
                }

                // Test openFile
                const openSuccess = editor.openFile("/test/file.rs", 42, 10);
                if (!openSuccess) {
                    throw new Error("Open file failed");
                }

                // Test openFileInSplit
                const splitOpenSuccess = editor.openFileInSplit(3, "/test/other.rs", 100, 5);
                if (!splitOpenSuccess) {
                    throw new Error("Open file in split failed");
                }

                console.log("All new ops work correctly!");
                "#,
            )
            .await;
        assert!(result.is_ok(), "New ops test failed: {:?}", result);

        // Verify commands were received
        let commands: Vec<_> = rx.try_iter().collect();
        assert_eq!(commands.len(), 3, "Expected 3 commands");

        // Check RegisterCommand
        match &commands[0] {
            PluginCommand::RegisterCommand { command } => {
                assert_eq!(command.name, "My Plugin Command");
                assert_eq!(command.description, "A test command from TypeScript");
                match &command.action {
                    crate::keybindings::Action::PluginAction(name) => {
                        assert_eq!(name, "my_plugin_action");
                    }
                    _ => panic!("Expected PluginAction"),
                }
                assert_eq!(command.contexts.len(), 2);
            }
            _ => panic!("Expected RegisterCommand"),
        }

        // Check OpenFileAtLocation
        match &commands[1] {
            PluginCommand::OpenFileAtLocation { path, line, column } => {
                assert_eq!(path, &PathBuf::from("/test/file.rs"));
                assert_eq!(*line, Some(42));
                assert_eq!(*column, Some(10));
            }
            _ => panic!("Expected OpenFileAtLocation"),
        }

        // Check OpenFileInSplit
        match &commands[2] {
            PluginCommand::OpenFileInSplit {
                split_id,
                path,
                line,
                column,
            } => {
                assert_eq!(*split_id, 3);
                assert_eq!(path, &PathBuf::from("/test/other.rs"));
                assert_eq!(*line, Some(100));
                assert_eq!(*column, Some(5));
            }
            _ => panic!("Expected OpenFileInSplit"),
        }
    }

    #[tokio::test]
    async fn test_register_command_empty_contexts() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx).unwrap();

        // Register command with empty contexts (available everywhere)
        let result = runtime
            .execute_script(
                "<test_empty_contexts>",
                r#"
                editor.registerCommand("Global Command", "Available everywhere", "global_action", "");
                "#,
            )
            .await;
        assert!(result.is_ok());

        let commands: Vec<_> = rx.try_iter().collect();
        assert_eq!(commands.len(), 1);

        match &commands[0] {
            PluginCommand::RegisterCommand { command } => {
                assert_eq!(command.name, "Global Command");
                assert!(command.contexts.is_empty(), "Empty string should result in empty contexts");
            }
            _ => panic!("Expected RegisterCommand"),
        }
    }

    #[tokio::test]
    async fn test_register_command_all_contexts() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx).unwrap();

        // Test all valid context types
        let result = runtime
            .execute_script(
                "<test_all_contexts>",
                r#"
                editor.registerCommand(
                    "All Contexts",
                    "Test all context types",
                    "test_action",
                    "global, normal, help, prompt, popup, fileexplorer, menu"
                );
                "#,
            )
            .await;
        assert!(result.is_ok());

        let commands: Vec<_> = rx.try_iter().collect();
        match &commands[0] {
            PluginCommand::RegisterCommand { command } => {
                assert_eq!(command.contexts.len(), 7);
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Global));
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Normal));
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Help));
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Prompt));
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Popup));
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::FileExplorer));
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Menu));
            }
            _ => panic!("Expected RegisterCommand"),
        }
    }

    #[tokio::test]
    async fn test_register_command_invalid_contexts_ignored() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx).unwrap();

        // Invalid contexts should be silently ignored
        let result = runtime
            .execute_script(
                "<test_invalid_contexts>",
                r#"
                editor.registerCommand(
                    "Partial Contexts",
                    "Some invalid",
                    "test_action",
                    "normal, invalid_context, popup, unknown"
                );
                "#,
            )
            .await;
        assert!(result.is_ok());

        let commands: Vec<_> = rx.try_iter().collect();
        match &commands[0] {
            PluginCommand::RegisterCommand { command } => {
                // Only normal and popup should be recognized
                assert_eq!(command.contexts.len(), 2);
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Normal));
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Popup));
            }
            _ => panic!("Expected RegisterCommand"),
        }
    }

    #[tokio::test]
    async fn test_open_file_with_zero_values() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx).unwrap();

        // Zero values should translate to None (file opening without positioning)
        let result = runtime
            .execute_script(
                "<test_zero_values>",
                r#"
                editor.openFile("/test/file.txt", 0, 0);
                "#,
            )
            .await;
        assert!(result.is_ok());

        let commands: Vec<_> = rx.try_iter().collect();
        match &commands[0] {
            PluginCommand::OpenFileAtLocation { path, line, column } => {
                assert_eq!(path.to_str().unwrap(), "/test/file.txt");
                assert_eq!(*line, None, "0 should translate to None");
                assert_eq!(*column, None, "0 should translate to None");
            }
            _ => panic!("Expected OpenFileAtLocation"),
        }
    }

    #[tokio::test]
    async fn test_open_file_with_default_params() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx).unwrap();

        // Test that JavaScript default parameters work
        let result = runtime
            .execute_script(
                "<test_default_params>",
                r#"
                // Call with just path (line and column default to 0)
                editor.openFile("/test/file.txt");
                "#,
            )
            .await;
        assert!(result.is_ok());

        let commands: Vec<_> = rx.try_iter().collect();
        match &commands[0] {
            PluginCommand::OpenFileAtLocation { path, line, column } => {
                assert_eq!(path.to_str().unwrap(), "/test/file.txt");
                assert_eq!(*line, None);
                assert_eq!(*column, None);
            }
            _ => panic!("Expected OpenFileAtLocation"),
        }
    }

    #[tokio::test]
    async fn test_open_file_with_line_only() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx).unwrap();

        // Open file at specific line but no column
        let result = runtime
            .execute_script(
                "<test_line_only>",
                r#"
                editor.openFile("/test/file.txt", 50);
                "#,
            )
            .await;
        assert!(result.is_ok());

        let commands: Vec<_> = rx.try_iter().collect();
        match &commands[0] {
            PluginCommand::OpenFileAtLocation { line, column, .. } => {
                assert_eq!(*line, Some(50));
                assert_eq!(*column, None, "Column should be None when not specified");
            }
            _ => panic!("Expected OpenFileAtLocation"),
        }
    }

    #[tokio::test]
    async fn test_register_command_case_insensitive_contexts() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx).unwrap();

        // Context names should be case-insensitive
        let result = runtime
            .execute_script(
                "<test_case_insensitive>",
                r#"
                editor.registerCommand(
                    "Case Test",
                    "Test case insensitivity",
                    "test_action",
                    "NORMAL, Popup, FileExplorer"
                );
                "#,
            )
            .await;
        assert!(result.is_ok());

        let commands: Vec<_> = rx.try_iter().collect();
        match &commands[0] {
            PluginCommand::RegisterCommand { command } => {
                assert_eq!(command.contexts.len(), 3);
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Normal));
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::Popup));
                assert!(command.contexts.contains(&crate::keybindings::KeyContext::FileExplorer));
            }
            _ => panic!("Expected RegisterCommand"),
        }
    }

    #[tokio::test]
    async fn test_spawn_process_simple() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Test spawning a simple echo command
        let result = runtime
            .execute_script(
                "<test_spawn>",
                r#"
                (async () => {
                    const result = await editor.spawnProcess("echo", ["hello", "world"]);
                    if (!result.stdout.includes("hello world")) {
                        throw new Error(`Expected 'hello world' in stdout, got: ${result.stdout}`);
                    }
                    if (result.exit_code !== 0) {
                        throw new Error(`Expected exit code 0, got: ${result.exit_code}`);
                    }
                    console.log("Spawn process test passed!");
                })()
                "#,
            )
            .await;
        assert!(result.is_ok(), "Spawn process test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_spawn_process_with_stderr() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Test spawning a command that writes to stderr
        let result = runtime
            .execute_script(
                "<test_spawn_stderr>",
                r#"
                (async () => {
                    const result = await editor.spawnProcess("sh", ["-c", "echo error >&2"]);
                    if (!result.stderr.includes("error")) {
                        throw new Error(`Expected 'error' in stderr, got: ${result.stderr}`);
                    }
                    console.log("Spawn stderr test passed!");
                })()
                "#,
            )
            .await;
        assert!(result.is_ok(), "Spawn stderr test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_spawn_process_nonzero_exit() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Test spawning a command that exits with non-zero
        let result = runtime
            .execute_script(
                "<test_spawn_exit>",
                r#"
                (async () => {
                    const result = await editor.spawnProcess("sh", ["-c", "exit 42"]);
                    if (result.exit_code !== 42) {
                        throw new Error(`Expected exit code 42, got: ${result.exit_code}`);
                    }
                    console.log("Non-zero exit test passed!");
                })()
                "#,
            )
            .await;
        assert!(result.is_ok(), "Non-zero exit test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_spawn_process_git_example() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Test a realistic example: git version
        let result = runtime
            .execute_script(
                "<test_git>",
                r#"
                (async () => {
                    const result = await editor.spawnProcess("git", ["--version"]);
                    if (!result.stdout.includes("git version")) {
                        throw new Error(`Expected 'git version' in output, got: ${result.stdout}`);
                    }
                    editor.setStatus(`Git version: ${result.stdout.trim()}`);
                    console.log("Git version test passed!");
                })()
                "#,
            )
            .await;
        assert!(result.is_ok(), "Git example test failed: {:?}", result);
    }
}



