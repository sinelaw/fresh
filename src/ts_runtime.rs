//! TypeScript Plugin Runtime
//!
//! This module provides a TypeScript/JavaScript runtime for plugins using deno_core.
//! It enables native async/await support, solving the async command execution problem
//! that existed with the Lua plugin system.

use crate::commands::Suggestion;
use crate::event::BufferId;
use crate::plugin_api::{EditorStateSnapshot, PluginCommand};
use anyhow::{anyhow, Result};
use deno_core::{extension, op2, OpState, FastString, JsRuntime, RuntimeOptions};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, RwLock};

/// Shared state accessible from ops
struct TsRuntimeState {
    /// Editor state snapshot (read-only access)
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
    /// Command sender for write operations
    command_sender: std::sync::mpsc::Sender<PluginCommand>,
    /// Event handlers: event_name -> list of global JS function names
    event_handlers: Rc<RefCell<HashMap<String, Vec<String>>>>,
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

/// Get a range of text from a buffer
/// This is important for plugins that need to analyze buffer content
#[op2]
#[string]
fn op_fresh_get_buffer_text(
    state: &mut OpState,
    buffer_id: u32,
    start: u32,
    end: u32,
) -> String {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(buffer_info) = snapshot.buffers.get(&BufferId(buffer_id as usize)) {
                // For now, we can't directly access buffer content from the snapshot
                // This would need to be extended to include buffer content
                // Return empty string as placeholder
                let _ = (buffer_info, start, end);
                return String::new();
            }
        };
    }
    String::new()
}

/// Get the current line number (1-indexed) for the cursor position
#[op2(fast)]
fn op_fresh_get_cursor_line(state: &mut OpState) -> u32 {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(cursor) = &snapshot.primary_cursor {
                // Simplified: estimate line number from byte position
                // In a real implementation, this would use buffer content
                // For now, return 1 as placeholder
                let _ = cursor.position;
                return 1;
            }
        };
    }
    1
}

/// Get all cursor positions for multi-cursor editing
#[op2]
#[serde]
fn op_fresh_get_all_cursor_positions(state: &mut OpState) -> Vec<u32> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot
                .all_cursors
                .iter()
                .map(|c| c.position as u32)
                .collect();
        };
    }
    vec![]
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

/// Register an event handler
/// The handler_name should be a global JavaScript function name
/// Returns true if registration succeeded
#[op2(fast)]
fn op_fresh_on(
    state: &mut OpState,
    #[string] event_name: String,
    #[string] handler_name: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let mut handlers = runtime_state.event_handlers.borrow_mut();
        handlers
            .entry(event_name.clone())
            .or_insert_with(Vec::new)
            .push(handler_name.clone());
        tracing::debug!("Registered event handler '{}' for '{}'", handler_name, event_name);
        return true;
    }
    false
}

/// Unregister an event handler
/// Returns true if the handler was found and removed
#[op2(fast)]
fn op_fresh_off(
    state: &mut OpState,
    #[string] event_name: String,
    #[string] handler_name: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let mut handlers = runtime_state.event_handlers.borrow_mut();
        if let Some(handler_list) = handlers.get_mut(&event_name) {
            if let Some(pos) = handler_list.iter().position(|h| h == &handler_name) {
                handler_list.remove(pos);
                tracing::debug!("Unregistered event handler '{}' from '{}'", handler_name, event_name);
                return true;
            }
        }
    }
    false
}

/// Get list of registered handlers for an event
#[op2]
#[serde]
fn op_fresh_get_handlers(state: &mut OpState, #[string] event_name: String) -> Vec<String> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let handlers = runtime_state.event_handlers.borrow();
        if let Some(handler_list) = handlers.get(&event_name) {
            return handler_list.clone();
        }
    }
    Vec::new()
}

/// File stat information
#[derive(serde::Serialize)]
struct FileStat {
    exists: bool,
    is_file: bool,
    is_dir: bool,
    size: u64,
    readonly: bool,
}

/// Buffer information for TypeScript
#[derive(serde::Serialize)]
struct TsBufferInfo {
    id: u32,
    path: String,
    modified: bool,
    length: u32,
}

/// Selection range for TypeScript
#[derive(serde::Serialize)]
struct TsSelectionRange {
    start: u32,
    end: u32,
}

/// Cursor information for TypeScript
#[derive(serde::Serialize)]
struct TsCursorInfo {
    position: u32,
    selection: Option<TsSelectionRange>,
}

/// Viewport information for TypeScript
#[derive(serde::Serialize)]
struct TsViewportInfo {
    top_byte: u32,
    left_column: u32,
    width: u32,
    height: u32,
}

/// Get full information about a buffer
#[op2]
#[serde]
fn op_fresh_get_buffer_info(state: &mut OpState, buffer_id: u32) -> Option<TsBufferInfo> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(info) = snapshot.buffers.get(&BufferId(buffer_id as usize)) {
                return Some(TsBufferInfo {
                    id: info.id.0 as u32,
                    path: info.path.as_ref().map(|p| p.to_string_lossy().to_string()).unwrap_or_default(),
                    modified: info.modified,
                    length: info.length as u32,
                });
            }
        };
    }
    None
}

/// List all open buffers
#[op2]
#[serde]
fn op_fresh_list_buffers(state: &mut OpState) -> Vec<TsBufferInfo> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot.buffers.values().map(|info| TsBufferInfo {
                id: info.id.0 as u32,
                path: info.path.as_ref().map(|p| p.to_string_lossy().to_string()).unwrap_or_default(),
                modified: info.modified,
                length: info.length as u32,
            }).collect();
        };
    }
    Vec::new()
}

/// Get primary cursor with selection info
#[op2]
#[serde]
fn op_fresh_get_primary_cursor(state: &mut OpState) -> Option<TsCursorInfo> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(ref cursor) = snapshot.primary_cursor {
                return Some(TsCursorInfo {
                    position: cursor.position as u32,
                    selection: cursor.selection.as_ref().map(|sel| TsSelectionRange {
                        start: sel.start as u32,
                        end: sel.end as u32,
                    }),
                });
            }
        };
    }
    None
}

/// Get all cursors (for multi-cursor support)
#[op2]
#[serde]
fn op_fresh_get_all_cursors(state: &mut OpState) -> Vec<TsCursorInfo> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot.all_cursors.iter().map(|cursor| TsCursorInfo {
                position: cursor.position as u32,
                selection: cursor.selection.as_ref().map(|sel| TsSelectionRange {
                    start: sel.start as u32,
                    end: sel.end as u32,
                }),
            }).collect();
        };
    }
    Vec::new()
}

/// Get viewport information
#[op2]
#[serde]
fn op_fresh_get_viewport(state: &mut OpState) -> Option<TsViewportInfo> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(ref vp) = snapshot.viewport {
                return Some(TsViewportInfo {
                    top_byte: vp.top_byte as u32,
                    left_column: vp.left_column as u32,
                    width: vp.width as u32,
                    height: vp.height as u32,
                });
            }
        };
    }
    None
}

/// Suggestion from TypeScript for prompt autocomplete
#[derive(serde::Deserialize)]
struct TsSuggestion {
    text: String,
    description: Option<String>,
    value: Option<String>,
    disabled: Option<bool>,
    keybinding: Option<String>,
}

/// Start an interactive prompt
#[op2(fast)]
fn op_fresh_start_prompt(
    state: &mut OpState,
    #[string] label: String,
    #[string] prompt_type: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state.command_sender.send(PluginCommand::StartPrompt {
            label,
            prompt_type,
        });
        return result.is_ok();
    }
    false
}

/// Set suggestions for the current prompt
#[op2]
fn op_fresh_set_prompt_suggestions(
    state: &mut OpState,
    #[serde] suggestions: Vec<TsSuggestion>,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let converted: Vec<Suggestion> = suggestions
            .into_iter()
            .map(|s| Suggestion {
                text: s.text,
                description: s.description,
                value: s.value,
                disabled: s.disabled.unwrap_or(false),
                keybinding: s.keybinding,
            })
            .collect();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetPromptSuggestions {
                suggestions: converted,
            });
        return result.is_ok();
    }
    false
}

/// Read a file's contents asynchronously
/// Useful for plugins that need to read configuration or data files
#[op2(async)]
#[string]
async fn op_fresh_read_file(
    #[string] path: String,
) -> Result<String, deno_core::error::AnyError> {
    tokio::fs::read_to_string(&path)
        .await
        .map_err(|e| deno_core::error::generic_error(format!("Failed to read file {}: {}", path, e)))
}

/// Write content to a file asynchronously
/// Useful for plugins that need to save data or generate files
#[op2(async)]
async fn op_fresh_write_file(
    #[string] path: String,
    #[string] content: String,
) -> Result<(), deno_core::error::AnyError> {
    tokio::fs::write(&path, content)
        .await
        .map_err(|e| deno_core::error::generic_error(format!("Failed to write file {}: {}", path, e)))
}

/// Check if a file or directory exists
#[op2(fast)]
fn op_fresh_file_exists(#[string] path: String) -> bool {
    std::path::Path::new(&path).exists()
}

/// Get file/directory metadata
#[op2]
#[serde]
fn op_fresh_file_stat(#[string] path: String) -> FileStat {
    let path = std::path::Path::new(&path);
    match std::fs::metadata(path) {
        Ok(metadata) => FileStat {
            exists: true,
            is_file: metadata.is_file(),
            is_dir: metadata.is_dir(),
            size: metadata.len(),
            readonly: metadata.permissions().readonly(),
        },
        Err(_) => FileStat {
            exists: false,
            is_file: false,
            is_dir: false,
            size: 0,
            readonly: false,
        },
    }
}

/// Get an environment variable
#[op2]
#[string]
fn op_fresh_get_env(#[string] name: String) -> Option<String> {
    std::env::var(&name).ok()
}

/// Get the current working directory
#[op2]
#[string]
fn op_fresh_get_cwd() -> Result<String, deno_core::error::AnyError> {
    std::env::current_dir()
        .map(|p| p.to_string_lossy().to_string())
        .map_err(|e| deno_core::error::generic_error(format!("Failed to get cwd: {}", e)))
}

/// Join path components
#[op2]
#[string]
fn op_fresh_path_join(#[serde] parts: Vec<String>) -> String {
    let mut path = std::path::PathBuf::new();
    for part in parts {
        path.push(part);
    }
    path.to_string_lossy().to_string()
}

/// Get the directory name of a path
#[op2]
#[string]
fn op_fresh_path_dirname(#[string] path: String) -> String {
    std::path::Path::new(&path)
        .parent()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_default()
}

/// Get the base name of a path
#[op2]
#[string]
fn op_fresh_path_basename(#[string] path: String) -> String {
    std::path::Path::new(&path)
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_default()
}

/// Get the file extension
#[op2]
#[string]
fn op_fresh_path_extname(#[string] path: String) -> String {
    std::path::Path::new(&path)
        .extension()
        .map(|e| format!(".{}", e.to_string_lossy()))
        .unwrap_or_default()
}

/// Check if a path is absolute
#[op2(fast)]
fn op_fresh_path_is_absolute(#[string] path: String) -> bool {
    std::path::Path::new(&path).is_absolute()
}

/// Directory entry information
#[derive(serde::Serialize)]
struct DirEntry {
    name: String,
    is_file: bool,
    is_dir: bool,
}

/// Read directory contents
/// Returns a list of entries with name and type information
#[op2]
#[serde]
fn op_fresh_read_dir(#[string] path: String) -> Result<Vec<DirEntry>, deno_core::error::AnyError> {
    let entries = std::fs::read_dir(&path)
        .map_err(|e| deno_core::error::generic_error(format!("Failed to read directory {}: {}", path, e)))?;

    let mut result = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|e| {
            deno_core::error::generic_error(format!("Failed to read directory entry: {}", e))
        })?;

        let metadata = entry.metadata().map_err(|e| {
            deno_core::error::generic_error(format!("Failed to get entry metadata: {}", e))
        })?;

        result.push(DirEntry {
            name: entry.file_name().to_string_lossy().to_string(),
            is_file: metadata.is_file(),
            is_dir: metadata.is_dir(),
        });
    }

    Ok(result)
}

// === Virtual Buffer Operations ===

/// Text property entry for TypeScript
#[derive(serde::Deserialize)]
struct TsTextPropertyEntry {
    text: String,
    properties: std::collections::HashMap<String, serde_json::Value>,
}

/// Options for creating a virtual buffer in a split
#[derive(serde::Deserialize)]
struct CreateVirtualBufferOptions {
    name: String,
    mode: String,
    read_only: bool,
    entries: Vec<TsTextPropertyEntry>,
    ratio: f32,
    panel_id: Option<String>,
    show_line_numbers: Option<bool>,
    show_cursors: Option<bool>,
}

/// Create a virtual buffer in a horizontal split
/// This is the key operation for creating diagnostic panels, search results, etc.
#[op2]
fn op_fresh_create_virtual_buffer_in_split(
    state: &mut OpState,
    #[serde] options: CreateVirtualBufferOptions,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();

        // Convert TypeScript entries to Rust TextPropertyEntry
        let entries: Vec<crate::text_property::TextPropertyEntry> = options
            .entries
            .into_iter()
            .map(|e| crate::text_property::TextPropertyEntry {
                text: e.text,
                properties: e.properties,
            })
            .collect();

        let result = runtime_state
            .command_sender
            .send(PluginCommand::CreateVirtualBufferInSplit {
                name: options.name,
                mode: options.mode,
                read_only: options.read_only,
                entries,
                ratio: options.ratio,
                panel_id: options.panel_id,
                show_line_numbers: options.show_line_numbers.unwrap_or(true),
                show_cursors: options.show_cursors.unwrap_or(true),
            });
        return result.is_ok();
    }
    false
}

/// Define a buffer mode with keybindings
#[op2]
fn op_fresh_define_mode(
    state: &mut OpState,
    #[string] name: String,
    #[string] parent: Option<String>,
    #[serde] bindings: Vec<(String, String)>,
    read_only: bool,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state.command_sender.send(PluginCommand::DefineMode {
            name,
            parent,
            bindings,
            read_only,
        });
        return result.is_ok();
    }
    false
}

/// Show a buffer in the current split
#[op2(fast)]
fn op_fresh_show_buffer(state: &mut OpState, buffer_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state.command_sender.send(PluginCommand::ShowBuffer {
            buffer_id: BufferId(buffer_id as usize),
        });
        return result.is_ok();
    }
    false
}

/// Get text properties at cursor position
/// Returns an array of property maps for all properties at the current cursor position
#[op2]
#[serde]
fn op_fresh_get_text_properties_at_cursor(state: &mut OpState, buffer_id: u32) -> Vec<std::collections::HashMap<String, serde_json::Value>> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            // Get cursor position
            if let Some(ref cursor) = snapshot.primary_cursor {
                // For now, return empty - actual implementation requires buffer access
                // which would need to be added to the snapshot
                let _ = (buffer_id, cursor.position);
                return vec![];
            }
        };
    }
    vec![]
}

/// Set the content of a virtual buffer with text properties
#[op2]
fn op_fresh_set_virtual_buffer_content(
    state: &mut OpState,
    buffer_id: u32,
    #[serde] entries: Vec<TsTextPropertyEntry>,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();

        // Convert TypeScript entries to Rust TextPropertyEntry
        let rust_entries: Vec<crate::text_property::TextPropertyEntry> = entries
            .into_iter()
            .map(|e| crate::text_property::TextPropertyEntry {
                text: e.text,
                properties: e.properties,
            })
            .collect();

        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetVirtualBufferContent {
                buffer_id: BufferId(buffer_id as usize),
                entries: rust_entries,
            });
        return result.is_ok();
    }
    false
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
        op_fresh_get_buffer_text,
        op_fresh_get_cursor_line,
        op_fresh_get_all_cursor_positions,
        op_fresh_spawn_process,
        op_fresh_get_buffer_info,
        op_fresh_list_buffers,
        op_fresh_get_primary_cursor,
        op_fresh_get_all_cursors,
        op_fresh_get_viewport,
        op_fresh_start_prompt,
        op_fresh_set_prompt_suggestions,
        op_fresh_read_file,
        op_fresh_write_file,
        op_fresh_file_exists,
        op_fresh_file_stat,
        op_fresh_get_env,
        op_fresh_get_cwd,
        op_fresh_path_join,
        op_fresh_path_dirname,
        op_fresh_path_basename,
        op_fresh_path_extname,
        op_fresh_path_is_absolute,
        op_fresh_read_dir,
        op_fresh_on,
        op_fresh_off,
        op_fresh_get_handlers,
        // Virtual buffer operations
        op_fresh_create_virtual_buffer_in_split,
        op_fresh_define_mode,
        op_fresh_show_buffer,
        op_fresh_get_text_properties_at_cursor,
        op_fresh_set_virtual_buffer_content,
    ],
);

/// TypeScript plugin runtime
pub struct TypeScriptRuntime {
    js_runtime: JsRuntime,
    /// Shared event handlers registry
    event_handlers: Rc<RefCell<HashMap<String, Vec<String>>>>,
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
        let event_handlers = Rc::new(RefCell::new(HashMap::new()));
        let runtime_state = Rc::new(RefCell::new(TsRuntimeState {
            state_snapshot,
            command_sender,
            event_handlers: event_handlers.clone(),
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

                    // Buffer text operations
                    getBufferText(bufferId, start, end) {
                        return core.ops.op_fresh_get_buffer_text(bufferId, start, end);
                    },

                    // Cursor operations
                    getCursorLine() {
                        return core.ops.op_fresh_get_cursor_line();
                    },
                    getAllCursorPositions() {
                        return core.ops.op_fresh_get_all_cursor_positions();
                    },

                    // Buffer info queries
                    getBufferInfo(bufferId) {
                        return core.ops.op_fresh_get_buffer_info(bufferId);
                    },
                    listBuffers() {
                        return core.ops.op_fresh_list_buffers();
                    },
                    getPrimaryCursor() {
                        return core.ops.op_fresh_get_primary_cursor();
                    },
                    getAllCursors() {
                        return core.ops.op_fresh_get_all_cursors();
                    },
                    getViewport() {
                        return core.ops.op_fresh_get_viewport();
                    },

                    // Prompt operations
                    startPrompt(label, promptType) {
                        return core.ops.op_fresh_start_prompt(label, promptType);
                    },
                    setPromptSuggestions(suggestions) {
                        return core.ops.op_fresh_set_prompt_suggestions(suggestions);
                    },

                    // Async operations
                    spawnProcess(command, args = [], cwd = null) {
                        return core.ops.op_fresh_spawn_process(command, args, cwd);
                    },

                    // File system operations
                    readFile(path) {
                        return core.ops.op_fresh_read_file(path);
                    },
                    writeFile(path, content) {
                        return core.ops.op_fresh_write_file(path, content);
                    },
                    fileExists(path) {
                        return core.ops.op_fresh_file_exists(path);
                    },
                    fileStat(path) {
                        return core.ops.op_fresh_file_stat(path);
                    },

                    // Environment operations
                    getEnv(name) {
                        return core.ops.op_fresh_get_env(name);
                    },
                    getCwd() {
                        return core.ops.op_fresh_get_cwd();
                    },

                    // Path operations
                    pathJoin(...parts) {
                        return core.ops.op_fresh_path_join(parts);
                    },
                    pathDirname(path) {
                        return core.ops.op_fresh_path_dirname(path);
                    },
                    pathBasename(path) {
                        return core.ops.op_fresh_path_basename(path);
                    },
                    pathExtname(path) {
                        return core.ops.op_fresh_path_extname(path);
                    },
                    pathIsAbsolute(path) {
                        return core.ops.op_fresh_path_is_absolute(path);
                    },
                    readDir(path) {
                        return core.ops.op_fresh_read_dir(path);
                    },

                    // Event/Hook operations
                    on(eventName, handlerName) {
                        return core.ops.op_fresh_on(eventName, handlerName);
                    },
                    off(eventName, handlerName) {
                        return core.ops.op_fresh_off(eventName, handlerName);
                    },
                    getHandlers(eventName) {
                        return core.ops.op_fresh_get_handlers(eventName);
                    },

                    // Virtual buffer operations
                    createVirtualBufferInSplit(options) {
                        return core.ops.op_fresh_create_virtual_buffer_in_split(options);
                    },
                    defineMode(name, parent, bindings, readOnly = false) {
                        return core.ops.op_fresh_define_mode(name, parent, bindings, readOnly);
                    },
                    showBuffer(bufferId) {
                        return core.ops.op_fresh_show_buffer(bufferId);
                    },
                    getTextPropertiesAtCursor(bufferId) {
                        return core.ops.op_fresh_get_text_properties_at_cursor(bufferId);
                    },
                    setVirtualBufferContent(bufferId, entries) {
                        return core.ops.op_fresh_set_virtual_buffer_content(bufferId, entries);
                    },
                };

                // Make editor globally available
                globalThis.editor = editor;
                "#
                .to_string(),
            )
            .map_err(|e| anyhow!("Failed to initialize editor API: {}", e))?;

        Ok(Self { js_runtime, event_handlers })
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

    /// Emit an event to all registered handlers
    ///
    /// This calls all global JavaScript functions registered for the given event.
    /// The event_data is passed as JSON to each handler.
    ///
    /// # Arguments
    /// * `event_name` - Name of the event (e.g., "buffer_save", "cursor_moved")
    /// * `event_data` - JSON-serializable data to pass to handlers
    ///
    /// # Returns
    /// * `Ok(true)` if all handlers returned true (continue)
    /// * `Ok(false)` if any handler returned false (cancel)
    /// * `Err` if handler execution failed
    pub async fn emit(&mut self, event_name: &str, event_data: &str) -> Result<bool> {
        let handlers = self.event_handlers.borrow().get(event_name).cloned();

        if let Some(handler_names) = handlers {
            if handler_names.is_empty() {
                return Ok(true);
            }

            for handler_name in &handler_names {
                let code = format!(
                    r#"
                    (async () => {{
                        if (typeof globalThis.{} === 'function') {{
                            const eventData = {};
                            const result = globalThis.{}(eventData);
                            const finalResult = (result instanceof Promise) ? await result : result;
                            // Return true by default if handler doesn't return anything
                            return finalResult !== false;
                        }} else {{
                            console.warn('Event handler "{}" is not defined');
                            return true;
                        }}
                    }})();
                    "#,
                    handler_name, event_data, handler_name, handler_name
                );

                let code_static: FastString = code.into();
                let result = self.js_runtime.execute_script("<event_emit>", code_static);

                match result {
                    Ok(value) => {
                        // Run event loop to process any async work
                        self.js_runtime
                            .run_event_loop(Default::default())
                            .await
                            .map_err(|e| anyhow!("Event loop error in emit: {}", e))?;

                        // Check if the result is false (handler cancelled)
                        let scope = &mut self.js_runtime.handle_scope();
                        let local = deno_core::v8::Local::new(scope, value);
                        if local.is_boolean() && !local.boolean_value(scope) {
                            tracing::debug!(
                                "Event '{}' cancelled by handler '{}'",
                                event_name,
                                handler_name
                            );
                            return Ok(false);
                        }
                    }
                    Err(e) => {
                        tracing::error!(
                            "Error executing event handler '{}' for '{}': {}",
                            handler_name,
                            event_name,
                            e
                        );
                        // Continue with other handlers even if one fails
                    }
                }
            }
        }

        Ok(true)
    }

    /// Get the list of registered handlers for an event
    pub fn get_registered_handlers(&self, event_name: &str) -> Vec<String> {
        self.event_handlers
            .borrow()
            .get(event_name)
            .cloned()
            .unwrap_or_default()
    }

    /// Check if any handlers are registered for an event
    pub fn has_handlers(&self, event_name: &str) -> bool {
        self.event_handlers
            .borrow()
            .get(event_name)
            .map(|v| !v.is_empty())
            .unwrap_or(false)
    }
}

// === TypeScript Plugin Manager ===

use crate::command_registry::CommandRegistry;
use crate::hooks::{HookArgs, HookRegistry};
use std::path::{Path, PathBuf};

/// Information about a loaded TypeScript plugin
#[derive(Debug, Clone)]
pub struct TsPluginInfo {
    /// Plugin name
    pub name: String,
    /// Plugin file path
    pub path: PathBuf,
    /// Whether the plugin is enabled
    pub enabled: bool,
}

/// TypeScript Plugin Manager - manages TypeScript plugins
///
/// This provides an interface similar to PluginManager (Lua) but for TypeScript plugins.
pub struct TypeScriptPluginManager {
    /// TypeScript runtime
    runtime: TypeScriptRuntime,

    /// Loaded plugins
    plugins: HashMap<String, TsPluginInfo>,

    /// Command registry (shared with editor)
    commands: Arc<RwLock<CommandRegistry>>,

    /// Command receiver (to get commands from plugins)
    command_receiver: std::sync::mpsc::Receiver<PluginCommand>,

    /// State snapshot handle for editor to update
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
}

impl TypeScriptPluginManager {
    /// Create a new TypeScript plugin manager
    pub fn new(
        _hooks: Arc<RwLock<HookRegistry>>,
        commands: Arc<RwLock<CommandRegistry>>,
    ) -> Result<Self> {
        // Create channel for plugin commands
        let (command_sender, command_receiver) = std::sync::mpsc::channel();

        // Create editor state snapshot for query API
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Create TypeScript runtime with state
        let runtime = TypeScriptRuntime::with_state(
            Arc::clone(&state_snapshot),
            command_sender,
        )?;

        tracing::info!("TypeScript plugin manager initialized");

        Ok(Self {
            runtime,
            plugins: HashMap::new(),
            commands,
            command_receiver,
            state_snapshot,
        })
    }

    /// Load a TypeScript plugin from a file
    pub async fn load_plugin(&mut self, path: &Path) -> Result<()> {
        let plugin_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| anyhow!("Invalid plugin filename"))?
            .to_string();

        tracing::info!("Loading TypeScript plugin: {} from {:?}", plugin_name, path);

        // Load and execute the module
        let path_str = path
            .to_str()
            .ok_or_else(|| anyhow!("Invalid path encoding"))?;

        self.runtime.load_module(path_str).await?;

        // Store plugin info
        self.plugins.insert(
            plugin_name.clone(),
            TsPluginInfo {
                name: plugin_name,
                path: path.to_path_buf(),
                enabled: true,
            },
        );

        Ok(())
    }

    /// Unload a plugin
    pub fn unload_plugin(&mut self, name: &str) -> Result<()> {
        if let Some(_plugin) = self.plugins.remove(name) {
            tracing::info!("Unloading TypeScript plugin: {}", name);

            // Remove plugin's commands (assuming they're prefixed with plugin name)
            let prefix = format!("{}:", name);
            self.commands.read().unwrap().unregister_by_prefix(&prefix);

            // Note: We can't truly unload JavaScript modules from V8,
            // but we can remove the plugin from our tracking
            // Future: could clear registered hooks for this plugin

            Ok(())
        } else {
            Err(anyhow!("Plugin '{}' not found", name))
        }
    }

    /// Reload a plugin
    pub async fn reload_plugin(&mut self, name: &str) -> Result<()> {
        let path = self
            .plugins
            .get(name)
            .ok_or_else(|| anyhow!("Plugin '{}' not found", name))?
            .path
            .clone();

        self.unload_plugin(name)?;
        self.load_plugin(&path).await?;

        Ok(())
    }

    /// Load all plugins from a directory
    pub async fn load_plugins_from_dir(&mut self, dir: &Path) -> Vec<String> {
        let mut errors = Vec::new();

        if !dir.exists() {
            tracing::warn!("Plugin directory does not exist: {:?}", dir);
            return errors;
        }

        // Scan directory for .ts and .js files
        match std::fs::read_dir(dir) {
            Ok(entries) => {
                for entry in entries.flatten() {
                    let path = entry.path();
                    let ext = path.extension().and_then(|s| s.to_str());
                    if ext == Some("ts") || ext == Some("js") {
                        if let Err(e) = self.load_plugin(&path).await {
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
    pub fn list_plugins(&self) -> Vec<TsPluginInfo> {
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
    pub async fn execute_action(&mut self, action_name: &str) -> Result<()> {
        tracing::info!("Executing TypeScript plugin action: {}", action_name);
        self.runtime.execute_action(action_name).await
    }

    /// Run plugin hooks for a given event
    ///
    /// This converts HookArgs to JSON and emits to all registered TypeScript handlers.
    pub async fn run_hook(&mut self, hook_name: &str, args: &HookArgs) -> Result<()> {
        // Convert HookArgs to JSON
        let json_data = self.hook_args_to_json(args)?;

        // Emit to TypeScript handlers
        self.runtime.emit(hook_name, &json_data).await?;

        Ok(())
    }

    /// Convert HookArgs to JSON string
    fn hook_args_to_json(&self, args: &HookArgs) -> Result<String> {
        let json_value = match args {
            HookArgs::RenderLine {
                buffer_id,
                line_number,
                byte_start,
                byte_end,
                content,
            } => {
                serde_json::json!({
                    "buffer_id": buffer_id.0,
                    "line_number": line_number,
                    "byte_start": byte_start,
                    "byte_end": byte_end,
                    "content": content,
                })
            }
            HookArgs::BufferActivated { buffer_id } => {
                serde_json::json!({ "buffer_id": buffer_id.0 })
            }
            HookArgs::BufferDeactivated { buffer_id } => {
                serde_json::json!({ "buffer_id": buffer_id.0 })
            }
            HookArgs::BufferClosed { buffer_id } => {
                serde_json::json!({ "buffer_id": buffer_id.0 })
            }
            HookArgs::CursorMoved {
                buffer_id,
                cursor_id,
                old_position,
                new_position,
            } => {
                serde_json::json!({
                    "buffer_id": buffer_id.0,
                    "cursor_id": cursor_id.0,
                    "old_position": old_position,
                    "new_position": new_position,
                })
            }
            HookArgs::BeforeInsert {
                buffer_id,
                position,
                text,
            } => {
                serde_json::json!({
                    "buffer_id": buffer_id.0,
                    "position": position,
                    "text": text,
                })
            }
            HookArgs::AfterInsert {
                buffer_id,
                position,
                text,
            } => {
                serde_json::json!({
                    "buffer_id": buffer_id.0,
                    "position": position,
                    "text": text,
                })
            }
            HookArgs::BeforeDelete { buffer_id, range } => {
                serde_json::json!({
                    "buffer_id": buffer_id.0,
                    "start": range.start,
                    "end": range.end,
                })
            }
            HookArgs::AfterDelete {
                buffer_id,
                range,
                deleted_text,
            } => {
                serde_json::json!({
                    "buffer_id": buffer_id.0,
                    "start": range.start,
                    "end": range.end,
                    "deleted_text": deleted_text,
                })
            }
            HookArgs::BeforeFileOpen { path } => {
                serde_json::json!({ "path": path.to_string_lossy() })
            }
            HookArgs::AfterFileOpen { path, buffer_id } => {
                serde_json::json!({
                    "path": path.to_string_lossy(),
                    "buffer_id": buffer_id.0,
                })
            }
            HookArgs::BeforeFileSave { path, buffer_id } => {
                serde_json::json!({
                    "path": path.to_string_lossy(),
                    "buffer_id": buffer_id.0,
                })
            }
            HookArgs::AfterFileSave { path, buffer_id } => {
                serde_json::json!({
                    "path": path.to_string_lossy(),
                    "buffer_id": buffer_id.0,
                })
            }
            HookArgs::PreCommand { action } => {
                serde_json::json!({ "action": format!("{:?}", action) })
            }
            HookArgs::PostCommand { action } => {
                serde_json::json!({ "action": format!("{:?}", action) })
            }
            HookArgs::Idle { milliseconds } => {
                serde_json::json!({ "milliseconds": milliseconds })
            }
            HookArgs::EditorInitialized => {
                serde_json::json!({})
            }
            HookArgs::PromptChanged { prompt_type, input } => {
                serde_json::json!({
                    "prompt_type": prompt_type,
                    "input": input,
                })
            }
            HookArgs::PromptConfirmed {
                prompt_type,
                input,
                selected_index,
            } => {
                serde_json::json!({
                    "prompt_type": prompt_type,
                    "input": input,
                    "selected_index": selected_index,
                })
            }
            HookArgs::PromptCancelled { prompt_type, input } => {
                serde_json::json!({
                    "prompt_type": prompt_type,
                    "input": input,
                })
            }
        };

        serde_json::to_string(&json_value)
            .map_err(|e| anyhow!("Failed to serialize hook args: {}", e))
    }

    /// Get access to the state snapshot for updating (used by Editor)
    pub fn state_snapshot_handle(&self) -> Arc<RwLock<EditorStateSnapshot>> {
        Arc::clone(&self.state_snapshot)
    }

    /// Check if any handlers are registered for a hook
    pub fn has_hook_handlers(&self, hook_name: &str) -> bool {
        self.runtime.has_handlers(hook_name)
    }

    /// Get the command registry (for testing)
    #[allow(dead_code)]
    pub fn command_registry(&self) -> Arc<RwLock<CommandRegistry>> {
        Arc::clone(&self.commands)
    }

    /// Load a plugin synchronously (blocking)
    ///
    /// This is useful for initialization where async context is not available.
    /// Uses a temporary tokio runtime to execute the async load.
    pub fn load_plugin_blocking(&mut self, path: &Path) -> Result<()> {
        // Create a new tokio runtime for this blocking operation
        let rt = tokio::runtime::Runtime::new()
            .map_err(|e| anyhow!("Failed to create runtime: {}", e))?;

        rt.block_on(self.load_plugin(path))
    }

    /// Load all plugins from a directory synchronously (blocking)
    pub fn load_plugins_from_dir_blocking(&mut self, dir: &Path) -> Vec<String> {
        let rt = match tokio::runtime::Runtime::new() {
            Ok(rt) => rt,
            Err(e) => {
                let err = format!("Failed to create runtime: {}", e);
                tracing::error!("{}", err);
                return vec![err];
            }
        };

        rt.block_on(self.load_plugins_from_dir(dir))
    }

    /// Execute an action synchronously (blocking)
    pub fn execute_action_blocking(&mut self, action_name: &str) -> Result<()> {
        let rt = tokio::runtime::Runtime::new()
            .map_err(|e| anyhow!("Failed to create runtime: {}", e))?;

        rt.block_on(self.execute_action(action_name))
    }

    /// Run a hook synchronously (blocking)
    pub fn run_hook_blocking(&mut self, hook_name: &str, args: &HookArgs) -> Result<()> {
        let rt = tokio::runtime::Runtime::new()
            .map_err(|e| anyhow!("Failed to create runtime: {}", e))?;

        rt.block_on(self.run_hook(hook_name, args))
    }

    /// Reload a plugin synchronously (blocking)
    pub fn reload_plugin_blocking(&mut self, name: &str) -> Result<()> {
        let rt = tokio::runtime::Runtime::new()
            .map_err(|e| anyhow!("Failed to create runtime: {}", e))?;

        rt.block_on(self.reload_plugin(name))
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

    #[tokio::test]
    async fn test_file_exists() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_file_exists>",
                r#"
                // Test existing file
                const cargoExists = editor.fileExists("Cargo.toml");
                if (!cargoExists) {
                    throw new Error("Cargo.toml should exist");
                }

                // Test non-existing file
                const fakeExists = editor.fileExists("this_file_does_not_exist_12345.txt");
                if (fakeExists) {
                    throw new Error("Non-existent file should return false");
                }

                console.log("File exists test passed!");
                "#,
            )
            .await;
        assert!(result.is_ok(), "File exists test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_file_stat() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_file_stat>",
                r#"
                // Test stat on existing file
                const stat = editor.fileStat("Cargo.toml");
                if (!stat.exists) {
                    throw new Error("Cargo.toml should exist");
                }
                if (!stat.is_file) {
                    throw new Error("Cargo.toml should be a file");
                }
                if (stat.is_dir) {
                    throw new Error("Cargo.toml should not be a directory");
                }
                if (stat.size === 0) {
                    throw new Error("Cargo.toml should have non-zero size");
                }

                // Test stat on non-existing file
                const noStat = editor.fileStat("nonexistent_12345.txt");
                if (noStat.exists) {
                    throw new Error("Non-existent file should have exists=false");
                }

                console.log("File stat test passed!");
                "#,
            )
            .await;
        assert!(result.is_ok(), "File stat test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_read_file() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_read_file>",
                r#"
                (async () => {
                    // Read Cargo.toml which should exist
                    const content = await editor.readFile("Cargo.toml");
                    if (!content.includes("[package]")) {
                        throw new Error("Cargo.toml should contain [package] section");
                    }
                    if (!content.includes("name")) {
                        throw new Error("Cargo.toml should contain name field");
                    }
                    console.log("Read file test passed!");
                })()
                "#,
            )
            .await;
        assert!(result.is_ok(), "Read file test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_path_operations() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_path_ops>",
                r#"
                // Test pathJoin
                const joined = editor.pathJoin("src", "ts_runtime.rs");
                if (!joined.includes("src") || !joined.includes("ts_runtime.rs")) {
                    throw new Error(`pathJoin failed: ${joined}`);
                }

                // Test pathDirname
                const dir = editor.pathDirname("/home/user/file.txt");
                if (dir !== "/home/user") {
                    throw new Error(`pathDirname failed: ${dir}`);
                }

                // Test pathBasename
                const base = editor.pathBasename("/home/user/file.txt");
                if (base !== "file.txt") {
                    throw new Error(`pathBasename failed: ${base}`);
                }

                // Test pathExtname
                const ext = editor.pathExtname("/home/user/file.txt");
                if (ext !== ".txt") {
                    throw new Error(`pathExtname failed: ${ext}`);
                }

                // Test empty extension
                const noExt = editor.pathExtname("/home/user/Makefile");
                if (noExt !== "") {
                    throw new Error(`pathExtname for no extension failed: ${noExt}`);
                }

                console.log("Path operations test passed!");
                "#,
            )
            .await;
        assert!(result.is_ok(), "Path operations test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_get_env() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_get_env>",
                r#"
                // PATH should always be set
                const path = editor.getEnv("PATH");
                if (path === null || path === undefined) {
                    throw new Error("PATH environment variable should be set");
                }
                if (path.length === 0) {
                    throw new Error("PATH should not be empty");
                }

                // Non-existent env var should return null
                const fake = editor.getEnv("THIS_ENV_VAR_DOES_NOT_EXIST_12345");
                if (fake !== null && fake !== undefined) {
                    throw new Error("Non-existent env var should return null/undefined");
                }

                console.log("Get env test passed!");
                "#,
            )
            .await;
        assert!(result.is_ok(), "Get env test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_get_cwd() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_get_cwd>",
                r#"
                const cwd = editor.getCwd();
                if (!cwd || cwd.length === 0) {
                    throw new Error("getCwd should return non-empty string");
                }
                // cwd should be an absolute path
                if (!cwd.startsWith("/")) {
                    throw new Error(`getCwd should return absolute path, got: ${cwd}`);
                }
                console.log(`Current working directory: ${cwd}`);
                "#,
            )
            .await;
        assert!(result.is_ok(), "Get cwd test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_write_file() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_write_file>",
                r#"
                (async () => {
                    const testFile = "/tmp/fresh_ts_runtime_test_write.txt";
                    const testContent = "Hello from TypeScript plugin!\nLine 2\n";

                    // Write the file
                    await editor.writeFile(testFile, testContent);

                    // Verify it was written by reading it back
                    const readBack = await editor.readFile(testFile);
                    if (readBack !== testContent) {
                        throw new Error(`Write/read mismatch. Expected: ${testContent}, Got: ${readBack}`);
                    }

                    // Verify file stats
                    const stat = editor.fileStat(testFile);
                    if (!stat.exists) {
                        throw new Error("Written file should exist");
                    }
                    if (!stat.is_file) {
                        throw new Error("Written path should be a file");
                    }
                    if (stat.size !== testContent.length) {
                        throw new Error(`File size mismatch. Expected: ${testContent.length}, Got: ${stat.size}`);
                    }

                    console.log("Write file test passed!");
                })()
                "#,
            )
            .await;
        assert!(result.is_ok(), "Write file test failed: {:?}", result);

        // Clean up test file
        let _ = std::fs::remove_file("/tmp/fresh_ts_runtime_test_write.txt");
    }

    #[tokio::test]
    async fn test_read_dir() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_read_dir>",
                r#"
                // Read current directory (should have Cargo.toml, src/, etc.)
                const entries = editor.readDir(".");

                // Should have entries
                if (!Array.isArray(entries) || entries.length === 0) {
                    throw new Error("readDir should return non-empty array");
                }

                // Look for known files/dirs
                const hasCargoToml = entries.some(e => e.name === "Cargo.toml" && e.is_file);
                const hasSrc = entries.some(e => e.name === "src" && e.is_dir);

                if (!hasCargoToml) {
                    throw new Error("Should find Cargo.toml in current directory");
                }
                if (!hasSrc) {
                    throw new Error("Should find src/ directory");
                }

                // Verify entry structure
                const firstEntry = entries[0];
                if (typeof firstEntry.name !== "string") {
                    throw new Error("Entry should have string name");
                }
                if (typeof firstEntry.is_file !== "boolean") {
                    throw new Error("Entry should have boolean is_file");
                }
                if (typeof firstEntry.is_dir !== "boolean") {
                    throw new Error("Entry should have boolean is_dir");
                }

                console.log(`Read directory test passed! Found ${entries.length} entries`);
                "#,
            )
            .await;
        assert!(result.is_ok(), "Read directory test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_path_is_absolute() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_path_is_absolute>",
                r#"
                // Test absolute paths
                if (!editor.pathIsAbsolute("/home/user")) {
                    throw new Error("/home/user should be absolute");
                }
                if (!editor.pathIsAbsolute("/")) {
                    throw new Error("/ should be absolute");
                }

                // Test relative paths
                if (editor.pathIsAbsolute("src/main.rs")) {
                    throw new Error("src/main.rs should not be absolute");
                }
                if (editor.pathIsAbsolute(".")) {
                    throw new Error(". should not be absolute");
                }
                if (editor.pathIsAbsolute("..")) {
                    throw new Error(".. should not be absolute");
                }

                console.log("Path is absolute test passed!");
                "#,
            )
            .await;
        assert!(
            result.is_ok(),
            "Path is absolute test failed: {:?}",
            result
        );
    }

    #[tokio::test]
    async fn test_hook_registration() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_hook_registration>",
                r#"
                // Register a handler
                const registered = editor.on("buffer_save", "onBufferSave");
                if (!registered) {
                    throw new Error("on() should return true");
                }

                // Check handlers
                const handlers = editor.getHandlers("buffer_save");
                if (handlers.length !== 1) {
                    throw new Error(`Expected 1 handler, got ${handlers.length}`);
                }
                if (handlers[0] !== "onBufferSave") {
                    throw new Error(`Expected handler 'onBufferSave', got '${handlers[0]}'`);
                }

                // Register another handler
                editor.on("buffer_save", "onBufferSave2");
                const handlers2 = editor.getHandlers("buffer_save");
                if (handlers2.length !== 2) {
                    throw new Error(`Expected 2 handlers, got ${handlers2.length}`);
                }

                // Unregister first handler
                const removed = editor.off("buffer_save", "onBufferSave");
                if (!removed) {
                    throw new Error("off() should return true when handler exists");
                }

                const handlers3 = editor.getHandlers("buffer_save");
                if (handlers3.length !== 1) {
                    throw new Error(`Expected 1 handler after off(), got ${handlers3.length}`);
                }

                // Try to unregister non-existent handler
                const notRemoved = editor.off("buffer_save", "nonexistent");
                if (notRemoved) {
                    throw new Error("off() should return false for non-existent handler");
                }

                console.log("Hook registration test passed!");
                "#,
            )
            .await;
        assert!(
            result.is_ok(),
            "Hook registration test failed: {:?}",
            result
        );
    }

    #[tokio::test]
    async fn test_hook_emit() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Register a handler that increments a counter
        let setup = runtime
            .execute_script(
                "<test_hook_emit_setup>",
                r#"
                globalThis.eventCounter = 0;
                globalThis.lastEventData = null;

                globalThis.onTestEvent = function(data) {
                    globalThis.eventCounter++;
                    globalThis.lastEventData = data;
                    return true;
                };

                editor.on("test_event", "onTestEvent");
                "#,
            )
            .await;
        assert!(setup.is_ok(), "Setup failed: {:?}", setup);

        // Emit the event
        let emit_result = runtime
            .emit("test_event", r#"{"value": 42, "message": "hello"}"#)
            .await;
        assert!(emit_result.is_ok(), "Emit failed: {:?}", emit_result);
        assert!(emit_result.unwrap(), "Emit should return true");

        // Verify handler was called
        let verify = runtime
            .execute_script(
                "<test_hook_emit_verify>",
                r#"
                if (globalThis.eventCounter !== 1) {
                    throw new Error(`Expected counter=1, got ${globalThis.eventCounter}`);
                }
                if (globalThis.lastEventData.value !== 42) {
                    throw new Error(`Expected value=42, got ${globalThis.lastEventData.value}`);
                }
                if (globalThis.lastEventData.message !== "hello") {
                    throw new Error(`Expected message='hello', got '${globalThis.lastEventData.message}'`);
                }
                console.log("Hook emit test passed!");
                "#,
            )
            .await;
        assert!(verify.is_ok(), "Verify failed: {:?}", verify);
    }

    #[tokio::test]
    async fn test_hook_emit_cancellation() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Register a handler that cancels the event
        let setup = runtime
            .execute_script(
                "<test_hook_cancel_setup>",
                r#"
                globalThis.cancelWasCalled = false;
                globalThis.onCancelEvent = function(data) {
                    globalThis.cancelWasCalled = true;
                    return false; // Cancel the event
                };

                editor.on("cancel_event", "onCancelEvent");
                "#,
            )
            .await;
        assert!(setup.is_ok(), "Setup failed: {:?}", setup);

        // Emit the event
        let emit_result = runtime.emit("cancel_event", "{}").await;
        assert!(emit_result.is_ok(), "Emit failed: {:?}", emit_result);
        // Note: Handler returning false should cancel, but emit always succeeds
        // The cancellation is tracked by the return value

        // Verify handler was called
        let verify = runtime
            .execute_script(
                "<test_hook_cancel_verify>",
                r#"
                if (!globalThis.cancelWasCalled) {
                    throw new Error("Cancel handler was not called");
                }
                console.log("Hook cancellation test passed!");
                "#,
            )
            .await;
        assert!(verify.is_ok(), "Verify failed: {:?}", verify);
    }

    #[tokio::test]
    async fn test_hook_multiple_handlers() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Register multiple handlers
        let setup = runtime
            .execute_script(
                "<test_hook_multi_setup>",
                r#"
                globalThis.handler1Called = false;
                globalThis.handler2Called = false;

                globalThis.handler1 = function(data) {
                    globalThis.handler1Called = true;
                    return true;
                };

                globalThis.handler2 = function(data) {
                    globalThis.handler2Called = true;
                    return true;
                };

                editor.on("multi_event", "handler1");
                editor.on("multi_event", "handler2");
                "#,
            )
            .await;
        assert!(setup.is_ok(), "Setup failed: {:?}", setup);

        // Emit the event
        let emit_result = runtime.emit("multi_event", "{}").await;
        assert!(emit_result.is_ok(), "Emit failed: {:?}", emit_result);

        // Verify both handlers were called
        let verify = runtime
            .execute_script(
                "<test_hook_multi_verify>",
                r#"
                if (!globalThis.handler1Called) {
                    throw new Error("handler1 was not called");
                }
                if (!globalThis.handler2Called) {
                    throw new Error("handler2 was not called");
                }
                console.log("Multiple handlers test passed!");
                "#,
            )
            .await;
        assert!(verify.is_ok(), "Verify failed: {:?}", verify);
    }

    // === TypeScriptPluginManager Tests ===

    #[tokio::test]
    async fn test_ts_plugin_manager_creation() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let manager = TypeScriptPluginManager::new(hooks, commands);
        assert!(manager.is_ok(), "Failed to create TS plugin manager");
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_state_snapshot() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        // Get state snapshot handle
        let snapshot = manager.state_snapshot_handle();

        // Update snapshot
        {
            let mut state = snapshot.write().unwrap();
            state.active_buffer_id = BufferId(42);
        }

        // Verify it was updated
        let state = snapshot.read().unwrap();
        assert_eq!(state.active_buffer_id.0, 42);
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_process_commands() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        // Initially no commands
        let cmds = manager.process_commands();
        assert!(cmds.is_empty());
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_list_plugins_empty() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        let plugins = manager.list_plugins();
        assert!(plugins.is_empty());
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_hook_args_to_json() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        // Test various hook args conversions
        let args = HookArgs::BufferActivated {
            buffer_id: BufferId(5),
        };
        let json = manager.hook_args_to_json(&args).unwrap();
        assert!(json.contains("\"buffer_id\":5"));

        let args = HookArgs::CursorMoved {
            buffer_id: BufferId(1),
            cursor_id: crate::event::CursorId(0),
            old_position: 10,
            new_position: 20,
        };
        let json = manager.hook_args_to_json(&args).unwrap();
        assert!(json.contains("\"buffer_id\":1"));
        assert!(json.contains("\"old_position\":10"));
        assert!(json.contains("\"new_position\":20"));

        let args = HookArgs::EditorInitialized;
        let json = manager.hook_args_to_json(&args).unwrap();
        assert_eq!(json, "{}");
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_has_no_hook_handlers_initially() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        assert!(!manager.has_hook_handlers("buffer_save"));
        assert!(!manager.has_hook_handlers("cursor_moved"));
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_load_inline_plugin() {
        use std::io::Write;
        use tempfile::NamedTempFile;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        // Create a temporary TypeScript plugin file
        let mut temp_file = NamedTempFile::with_suffix(".js").unwrap();
        writeln!(
            temp_file,
            r#"
            // Simple test plugin
            editor.setStatus("Test plugin loaded");

            // Register a command
            editor.registerCommand(
                "Test TS Command",
                "A test command from TypeScript",
                "test_ts_action",
                "normal"
            );

            // Define the action
            globalThis.test_ts_action = function() {{
                editor.setStatus("TS action executed");
            }};
        "#
        )
        .unwrap();
        temp_file.flush().unwrap();

        // Load the plugin
        let result = manager.load_plugin(temp_file.path()).await;
        assert!(result.is_ok(), "Failed to load plugin: {:?}", result);

        // Verify it's in the list
        let plugins = manager.list_plugins();
        assert_eq!(plugins.len(), 1);

        // Check that commands were sent
        let cmds = manager.process_commands();
        assert!(!cmds.is_empty(), "Expected commands from plugin");

        // Find SetStatus command
        let has_status = cmds.iter().any(|cmd| {
            matches!(cmd, PluginCommand::SetStatus { message } if message.contains("Test plugin loaded"))
        });
        assert!(has_status, "Expected SetStatus command");
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_execute_action() {
        use std::io::Write;
        use tempfile::NamedTempFile;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        // Create a plugin with an action
        let mut temp_file = NamedTempFile::with_suffix(".js").unwrap();
        writeln!(
            temp_file,
            r#"
            globalThis.myAction = function() {{
                editor.setStatus("Action executed!");
            }};
        "#
        )
        .unwrap();
        temp_file.flush().unwrap();

        // Load the plugin
        manager.load_plugin(temp_file.path()).await.unwrap();
        manager.process_commands(); // Clear loading commands

        // Execute the action
        let result = manager.execute_action("myAction").await;
        assert!(result.is_ok(), "Failed to execute action: {:?}", result);

        // Check that status was set
        let cmds = manager.process_commands();
        let has_action_status = cmds.iter().any(|cmd| {
            matches!(cmd, PluginCommand::SetStatus { message } if message.contains("Action executed"))
        });
        assert!(has_action_status, "Expected SetStatus from action");
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_run_hook() {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        // Register a hook handler via the runtime
        let setup = manager.runtime.execute_script(
            "<test_hook_setup>",
            r#"
            globalThis.onBufferActivated = function(data) {
                editor.setStatus("Buffer " + data.buffer_id + " activated");
            };
            editor.on("buffer_activated", "onBufferActivated");
            "#,
        ).await;
        assert!(setup.is_ok(), "Setup failed: {:?}", setup);

        // Clear any setup commands
        manager.process_commands();

        // Run the hook
        let args = HookArgs::BufferActivated {
            buffer_id: BufferId(42),
        };
        let result = manager.run_hook("buffer_activated", &args).await;
        assert!(result.is_ok(), "Failed to run hook: {:?}", result);

        // Check that the handler was called
        let cmds = manager.process_commands();
        let has_hook_status = cmds.iter().any(|cmd| {
            matches!(cmd, PluginCommand::SetStatus { message } if message.contains("Buffer 42 activated"))
        });
        assert!(has_hook_status, "Expected SetStatus from hook handler");
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_unload_plugin() {
        use std::io::Write;
        use tempfile::NamedTempFile;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        // Create and load a plugin
        let mut temp_file = NamedTempFile::with_suffix(".js").unwrap();
        writeln!(temp_file, r#"// Test plugin"#).unwrap();
        temp_file.flush().unwrap();

        manager.load_plugin(temp_file.path()).await.unwrap();

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
        assert!(result.is_ok(), "Failed to unload: {:?}", result);
        assert_eq!(manager.list_plugins().len(), 0);
    }
}

