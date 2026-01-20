//! QuickJS JavaScript runtime backend for TypeScript plugins
//!
//! This module provides a JavaScript runtime using QuickJS for executing
//! TypeScript plugins. TypeScript is transpiled to JavaScript using oxc.

use anyhow::{anyhow, Result};
use fresh_core::api::{
    ActionPopupAction, ActionSpec, BufferInfo, EditorStateSnapshot, JsCallbackId, PluginCommand,
    PluginResponse,
};
use fresh_core::command::Command;
use fresh_core::overlay::OverlayNamespace;
use fresh_core::text_property::TextPropertyEntry;
use fresh_core::{BufferId, SplitId};
use fresh_parser_js::{
    bundle_module, has_es_imports, has_es_module_syntax, strip_imports_and_exports,
    transpile_typescript,
};
use fresh_plugin_api_macros::{plugin_api, plugin_api_impl};
use rquickjs::{Context, Function, Object, Runtime, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{mpsc, Arc, RwLock};

/// Convert a QuickJS Value to serde_json::Value
fn js_to_json(ctx: &rquickjs::Ctx<'_>, val: Value<'_>) -> serde_json::Value {
    use rquickjs::Type;
    match val.type_of() {
        Type::Null | Type::Undefined | Type::Uninitialized => serde_json::Value::Null,
        Type::Bool => val
            .as_bool()
            .map(serde_json::Value::Bool)
            .unwrap_or(serde_json::Value::Null),
        Type::Int => val
            .as_int()
            .map(|n| serde_json::Value::Number(n.into()))
            .unwrap_or(serde_json::Value::Null),
        Type::Float => val
            .as_float()
            .and_then(|f| serde_json::Number::from_f64(f))
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        Type::String => val
            .as_string()
            .and_then(|s| s.to_string().ok())
            .map(serde_json::Value::String)
            .unwrap_or(serde_json::Value::Null),
        Type::Array => {
            if let Some(arr) = val.as_array() {
                let items: Vec<serde_json::Value> = arr
                    .iter()
                    .filter_map(|item| item.ok())
                    .map(|item| js_to_json(ctx, item))
                    .collect();
                serde_json::Value::Array(items)
            } else {
                serde_json::Value::Null
            }
        }
        Type::Object | Type::Constructor | Type::Function => {
            if let Some(obj) = val.as_object() {
                let mut map = serde_json::Map::new();
                for key in obj.keys::<String>().flatten() {
                    if let Ok(v) = obj.get::<_, Value>(&key) {
                        map.insert(key, js_to_json(ctx, v));
                    }
                }
                serde_json::Value::Object(map)
            } else {
                serde_json::Value::Null
            }
        }
        _ => serde_json::Value::Null,
    }
}

/// Get text properties at cursor position
fn get_text_properties_at_cursor_typed(
    snapshot: &Arc<RwLock<EditorStateSnapshot>>,
    buffer_id: u32,
) -> fresh_core::api::TextPropertiesAtCursor {
    use fresh_core::api::TextPropertiesAtCursor;

    let snap = match snapshot.read() {
        Ok(s) => s,
        Err(_) => return TextPropertiesAtCursor(Vec::new()),
    };
    let buffer_id_typed = BufferId(buffer_id as usize);
    let cursor_pos = match snap
        .buffer_cursor_positions
        .get(&buffer_id_typed)
        .copied()
        .or_else(|| {
            if snap.active_buffer_id == buffer_id_typed {
                snap.primary_cursor.as_ref().map(|c| c.position)
            } else {
                None
            }
        }) {
        Some(pos) => pos,
        None => return TextPropertiesAtCursor(Vec::new()),
    };

    let properties = match snap.buffer_text_properties.get(&buffer_id_typed) {
        Some(p) => p,
        None => return TextPropertiesAtCursor(Vec::new()),
    };

    // Find all properties at cursor position
    let result: Vec<_> = properties
        .iter()
        .filter(|prop| prop.start <= cursor_pos && cursor_pos < prop.end)
        .map(|prop| prop.properties.clone())
        .collect();

    TextPropertiesAtCursor(result)
}

/// Convert a JavaScript value to a string representation for console output
fn js_value_to_string(ctx: &rquickjs::Ctx<'_>, val: &Value<'_>) -> String {
    use rquickjs::Type;
    match val.type_of() {
        Type::Null => "null".to_string(),
        Type::Undefined => "undefined".to_string(),
        Type::Bool => val.as_bool().map(|b| b.to_string()).unwrap_or_default(),
        Type::Int => val.as_int().map(|n| n.to_string()).unwrap_or_default(),
        Type::Float => val.as_float().map(|f| f.to_string()).unwrap_or_default(),
        Type::String => val
            .as_string()
            .and_then(|s| s.to_string().ok())
            .unwrap_or_default(),
        Type::Object | Type::Exception => {
            // Check if this is an Error object (has message/stack properties)
            if let Some(obj) = val.as_object() {
                // Try to get error properties
                let name: Option<String> = obj.get("name").ok();
                let message: Option<String> = obj.get("message").ok();
                let stack: Option<String> = obj.get("stack").ok();

                if message.is_some() || name.is_some() {
                    // This looks like an Error object
                    let name = name.unwrap_or_else(|| "Error".to_string());
                    let message = message.unwrap_or_default();
                    if let Some(stack) = stack {
                        return format!("{}: {}\n{}", name, message, stack);
                    } else {
                        return format!("{}: {}", name, message);
                    }
                }

                // Regular object - convert to JSON
                let json = js_to_json(ctx, val.clone());
                serde_json::to_string(&json).unwrap_or_else(|_| "[object]".to_string())
            } else {
                "[object]".to_string()
            }
        }
        Type::Array => {
            let json = js_to_json(ctx, val.clone());
            serde_json::to_string(&json).unwrap_or_else(|_| "[array]".to_string())
        }
        Type::Function | Type::Constructor => "[function]".to_string(),
        Type::Symbol => "[symbol]".to_string(),
        Type::BigInt => val
            .as_big_int()
            .and_then(|b| b.clone().to_i64().ok())
            .map(|n| n.to_string())
            .unwrap_or_else(|| "[bigint]".to_string()),
        _ => format!("[{}]", val.type_name()),
    }
}

/// Format a JavaScript error with full details including stack trace
fn format_js_error(
    ctx: &rquickjs::Ctx<'_>,
    err: rquickjs::Error,
    source_name: &str,
) -> anyhow::Error {
    // Check if this is an exception that we can catch for more details
    if err.is_exception() {
        // Try to catch the exception to get the full error object
        let exc = ctx.catch();
        if !exc.is_undefined() && !exc.is_null() {
            // Try to get error message and stack from the exception object
            if let Some(exc_obj) = exc.as_object() {
                let message: String = exc_obj
                    .get::<_, String>("message")
                    .unwrap_or_else(|_| "Unknown error".to_string());
                let stack: String = exc_obj.get::<_, String>("stack").unwrap_or_default();
                let name: String = exc_obj
                    .get::<_, String>("name")
                    .unwrap_or_else(|_| "Error".to_string());

                if !stack.is_empty() {
                    return anyhow::anyhow!(
                        "JS error in {}: {}: {}\nStack trace:\n{}",
                        source_name,
                        name,
                        message,
                        stack
                    );
                } else {
                    return anyhow::anyhow!("JS error in {}: {}: {}", source_name, name, message);
                }
            } else {
                // Exception is not an object, try to convert to string
                let exc_str: String = exc
                    .as_string()
                    .and_then(|s: &rquickjs::String| s.to_string().ok())
                    .unwrap_or_else(|| format!("{:?}", exc));
                return anyhow::anyhow!("JS error in {}: {}", source_name, exc_str);
            }
        }
    }

    // Fall back to the basic error message
    anyhow::anyhow!("JS error in {}: {}", source_name, err)
}

/// Log a JavaScript error with full details
/// If panic_on_js_errors is enabled, this will panic to surface JS errors immediately
fn log_js_error(ctx: &rquickjs::Ctx<'_>, err: rquickjs::Error, context: &str) {
    let error = format_js_error(ctx, err, context);
    tracing::error!("{}", error);

    // When enabled, panic on JS errors to make them visible and fail fast
    if should_panic_on_js_errors() {
        panic!("JavaScript error in {}: {}", context, error);
    }
}

/// Global flag to panic on JS errors (enabled during testing)
static PANIC_ON_JS_ERRORS: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Enable panicking on JS errors (call this from test setup)
pub fn set_panic_on_js_errors(enabled: bool) {
    PANIC_ON_JS_ERRORS.store(enabled, std::sync::atomic::Ordering::SeqCst);
}

/// Check if panic on JS errors is enabled
fn should_panic_on_js_errors() -> bool {
    PANIC_ON_JS_ERRORS.load(std::sync::atomic::Ordering::SeqCst)
}

/// Global flag indicating a fatal JS error occurred that should terminate the plugin thread.
/// This is used because panicking inside rquickjs callbacks (FFI boundary) gets caught by
/// rquickjs's catch_unwind, so we need an alternative mechanism to signal errors.
static FATAL_JS_ERROR: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Storage for the fatal error message
static FATAL_JS_ERROR_MSG: std::sync::RwLock<Option<String>> = std::sync::RwLock::new(None);

/// Set a fatal JS error - call this instead of panicking inside FFI callbacks
fn set_fatal_js_error(msg: String) {
    if let Ok(mut guard) = FATAL_JS_ERROR_MSG.write() {
        if guard.is_none() {
            // Only store the first error
            *guard = Some(msg);
        }
    }
    FATAL_JS_ERROR.store(true, std::sync::atomic::Ordering::SeqCst);
}

/// Check if a fatal JS error has occurred
pub fn has_fatal_js_error() -> bool {
    FATAL_JS_ERROR.load(std::sync::atomic::Ordering::SeqCst)
}

/// Get and clear the fatal JS error message (returns None if no error)
pub fn take_fatal_js_error() -> Option<String> {
    if !FATAL_JS_ERROR.swap(false, std::sync::atomic::Ordering::SeqCst) {
        return None;
    }
    if let Ok(mut guard) = FATAL_JS_ERROR_MSG.write() {
        guard.take()
    } else {
        Some("Fatal JS error (message unavailable)".to_string())
    }
}

/// Run all pending jobs and check for unhandled exceptions
/// If panic_on_js_errors is enabled, this will panic on unhandled exceptions
fn run_pending_jobs_checked(ctx: &rquickjs::Ctx<'_>, context: &str) -> usize {
    let mut count = 0;
    loop {
        // Check for unhandled exception before running more jobs
        let exc: rquickjs::Value = ctx.catch();
        // Only treat it as an exception if it's actually an Error object
        if exc.is_exception() {
            let error_msg = if let Some(err) = exc.as_exception() {
                format!(
                    "{}: {}",
                    err.message().unwrap_or_default(),
                    err.stack().unwrap_or_default()
                )
            } else {
                format!("{:?}", exc)
            };
            tracing::error!("Unhandled JS exception during {}: {}", context, error_msg);
            if should_panic_on_js_errors() {
                panic!("Unhandled JS exception during {}: {}", context, error_msg);
            }
        }

        if !ctx.execute_pending_job() {
            break;
        }
        count += 1;
    }

    // Final check for exceptions after all jobs completed
    let exc: rquickjs::Value = ctx.catch();
    if exc.is_exception() {
        let error_msg = if let Some(err) = exc.as_exception() {
            format!(
                "{}: {}",
                err.message().unwrap_or_default(),
                err.stack().unwrap_or_default()
            )
        } else {
            format!("{:?}", exc)
        };
        tracing::error!(
            "Unhandled JS exception after running jobs in {}: {}",
            context,
            error_msg
        );
        if should_panic_on_js_errors() {
            panic!(
                "Unhandled JS exception after running jobs in {}: {}",
                context, error_msg
            );
        }
    }

    count
}

/// Parse a TextPropertyEntry from a JS Object
fn parse_text_property_entry(
    ctx: &rquickjs::Ctx<'_>,
    obj: &Object<'_>,
) -> Option<TextPropertyEntry> {
    let text: String = obj.get("text").ok()?;
    let properties: HashMap<String, serde_json::Value> = obj
        .get::<_, Object>("properties")
        .ok()
        .map(|props_obj| {
            let mut map = HashMap::new();
            for key in props_obj.keys::<String>().flatten() {
                if let Ok(v) = props_obj.get::<_, Value>(&key) {
                    map.insert(key, js_to_json(ctx, v));
                }
            }
            map
        })
        .unwrap_or_default();
    Some(TextPropertyEntry { text, properties })
}

/// Pending response senders type alias
pub type PendingResponses =
    Arc<std::sync::Mutex<HashMap<u64, tokio::sync::oneshot::Sender<PluginResponse>>>>;

/// Information about a loaded plugin
#[derive(Debug, Clone)]
pub struct TsPluginInfo {
    pub name: String,
    pub path: PathBuf,
    pub enabled: bool,
}

/// Handler information for events and actions
#[derive(Debug, Clone)]
pub struct PluginHandler {
    pub plugin_name: String,
    pub handler_name: String,
}

/// JavaScript-exposed Editor API using rquickjs class system
/// This allows proper lifetime handling for methods returning JS values
#[derive(rquickjs::class::Trace, rquickjs::JsLifetime)]
#[rquickjs::class]
pub struct JsEditorApi {
    #[qjs(skip_trace)]
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
    #[qjs(skip_trace)]
    command_sender: mpsc::Sender<PluginCommand>,
    #[qjs(skip_trace)]
    registered_actions: Rc<RefCell<HashMap<String, PluginHandler>>>,
    #[qjs(skip_trace)]
    event_handlers: Rc<RefCell<HashMap<String, Vec<PluginHandler>>>>,
    #[qjs(skip_trace)]
    next_request_id: Rc<RefCell<u64>>,
    #[qjs(skip_trace)]
    callback_contexts: Rc<RefCell<HashMap<u64, String>>>,
    #[qjs(skip_trace)]
    services: Arc<dyn fresh_core::services::PluginServiceBridge>,
    pub plugin_name: String,
}

#[plugin_api_impl]
#[rquickjs::methods(rename_all = "camelCase")]
impl JsEditorApi {
    // === Buffer Queries ===

    /// Get the active buffer ID (0 if none)
    pub fn get_active_buffer_id(&self) -> u32 {
        self.state_snapshot
            .read()
            .map(|s| s.active_buffer_id.0 as u32)
            .unwrap_or(0)
    }

    /// Get the active split ID
    pub fn get_active_split_id(&self) -> u32 {
        self.state_snapshot
            .read()
            .map(|s| s.active_split_id as u32)
            .unwrap_or(0)
    }

    /// List all open buffers - returns array of BufferInfo objects
    pub fn list_buffers<'js>(&self, ctx: rquickjs::Ctx<'js>) -> rquickjs::Result<Value<'js>> {
        let buffers: Vec<BufferInfo> = if let Ok(s) = self.state_snapshot.read() {
            s.buffers.values().cloned().collect()
        } else {
            Vec::new()
        };
        rquickjs_serde::to_value(ctx, &buffers)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    // === Logging ===

    pub fn debug(&self, msg: String) {
        tracing::info!("Plugin.debug: {}", msg);
    }

    pub fn info(&self, msg: String) {
        tracing::info!("Plugin: {}", msg);
    }

    pub fn warn(&self, msg: String) {
        tracing::warn!("Plugin: {}", msg);
    }

    pub fn error(&self, msg: String) {
        tracing::error!("Plugin: {}", msg);
    }

    // === Status ===

    pub fn set_status(&self, msg: String) {
        let _ = self
            .command_sender
            .send(PluginCommand::SetStatus { message: msg });
    }

    // === Clipboard ===

    pub fn copy_to_clipboard(&self, text: String) {
        let _ = self
            .command_sender
            .send(PluginCommand::SetClipboard { text });
    }

    pub fn set_clipboard(&self, text: String) {
        let _ = self
            .command_sender
            .send(PluginCommand::SetClipboard { text: text });
    }

    // === Command Registration ===

    /// Register a command - reads plugin name from __pluginName__ global
    /// context is optional - can be omitted, null, undefined, or a string
    pub fn register_command<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        name: String,
        description: String,
        handler_name: String,
        context: rquickjs::function::Opt<rquickjs::Value<'js>>,
    ) -> rquickjs::Result<bool> {
        // Use stored plugin name instead of global lookup
        let plugin_name = self.plugin_name.clone();
        // Extract context string - handle null, undefined, or missing
        let context_str: Option<String> = context.0.and_then(|v| {
            if v.is_null() || v.is_undefined() {
                None
            } else {
                v.as_string().and_then(|s| s.to_string().ok())
            }
        });

        tracing::debug!(
            "registerCommand: plugin='{}', name='{}', handler='{}'",
            plugin_name,
            name,
            handler_name
        );

        // Store action handler mapping with its plugin name
        self.registered_actions.borrow_mut().insert(
            handler_name.clone(),
            PluginHandler {
                plugin_name: self.plugin_name.clone(),
                handler_name: handler_name.clone(),
            },
        );

        // Register with editor
        let command = Command {
            name: name.clone(),
            description,
            action_name: handler_name,
            plugin_name,
            custom_contexts: context_str.into_iter().collect(),
        };

        Ok(self
            .command_sender
            .send(PluginCommand::RegisterCommand { command })
            .is_ok())
    }

    /// Unregister a command by name
    pub fn unregister_command(&self, name: String) -> bool {
        self.command_sender
            .send(PluginCommand::UnregisterCommand { name })
            .is_ok()
    }

    /// Set a context (for keybinding conditions)
    pub fn set_context(&self, name: String, active: bool) -> bool {
        self.command_sender
            .send(PluginCommand::SetContext { name, active })
            .is_ok()
    }

    /// Execute a built-in action
    pub fn execute_action(&self, action_name: String) -> bool {
        self.command_sender
            .send(PluginCommand::ExecuteAction { action_name })
            .is_ok()
    }

    // === Translation ===

    /// Translate a string - reads plugin name from __pluginName__ global
    /// Args is optional - can be omitted, undefined, null, or an object
    pub fn t<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        key: String,
        args: rquickjs::function::Rest<Value<'js>>,
    ) -> String {
        // Use stored plugin name instead of global lookup
        let plugin_name = self.plugin_name.clone();
        // Convert args to HashMap - args.0 is a Vec of the rest arguments
        let args_map: HashMap<String, String> = if let Some(first_arg) = args.0.first() {
            if let Some(obj) = first_arg.as_object() {
                let mut map = HashMap::new();
                for key_result in obj.keys::<String>() {
                    if let Ok(k) = key_result {
                        if let Ok(v) = obj.get::<_, String>(&k) {
                            map.insert(k, v);
                        }
                    }
                }
                map
            } else {
                HashMap::new()
            }
        } else {
            HashMap::new()
        };
        let res = self.services.translate(&plugin_name, &key, &args_map);

        tracing::info!(
            "Translating: key={}, plugin={}, args={:?} => res='{}'",
            key,
            plugin_name,
            args_map,
            res
        );
        res
    }

    // === Buffer Queries (additional) ===

    /// Get cursor position in active buffer
    pub fn get_cursor_position(&self) -> u32 {
        self.state_snapshot
            .read()
            .ok()
            .and_then(|s| s.primary_cursor.as_ref().map(|c| c.position as u32))
            .unwrap_or(0)
    }

    /// Get file path for a buffer
    pub fn get_buffer_path(&self, buffer_id: u32) -> String {
        if let Ok(s) = self.state_snapshot.read() {
            if let Some(b) = s.buffers.get(&BufferId(buffer_id as usize)) {
                if let Some(p) = &b.path {
                    return p.to_string_lossy().to_string();
                }
            }
        }
        String::new()
    }

    /// Get buffer length in bytes
    pub fn get_buffer_length(&self, buffer_id: u32) -> u32 {
        if let Ok(s) = self.state_snapshot.read() {
            if let Some(b) = s.buffers.get(&BufferId(buffer_id as usize)) {
                return b.length as u32;
            }
        }
        0
    }

    /// Check if buffer has unsaved changes
    pub fn is_buffer_modified(&self, buffer_id: u32) -> bool {
        if let Ok(s) = self.state_snapshot.read() {
            if let Some(b) = s.buffers.get(&BufferId(buffer_id as usize)) {
                return b.modified;
            }
        }
        false
    }

    /// Save a buffer to a specific file path
    /// Used by :w filename to save unnamed buffers or save-as
    pub fn save_buffer_to_path(&self, buffer_id: u32, path: String) -> bool {
        self.command_sender
            .send(PluginCommand::SaveBufferToPath {
                buffer_id: BufferId(buffer_id as usize),
                path: std::path::PathBuf::from(path),
            })
            .is_ok()
    }

    /// Get buffer info by ID
    pub fn get_buffer_info<'js>(
        &self,
        ctx: rquickjs::Ctx<'js>,
        buffer_id: u32,
    ) -> rquickjs::Result<Value<'js>> {
        let info = if let Ok(s) = self.state_snapshot.read() {
            s.buffers.get(&BufferId(buffer_id as usize)).cloned()
        } else {
            None
        };
        rquickjs_serde::to_value(ctx, &info)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Get primary cursor info for active buffer
    pub fn get_primary_cursor<'js>(&self, ctx: rquickjs::Ctx<'js>) -> rquickjs::Result<Value<'js>> {
        let cursor = if let Ok(s) = self.state_snapshot.read() {
            s.primary_cursor.clone()
        } else {
            None
        };
        rquickjs_serde::to_value(ctx, &cursor)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Get all cursors for active buffer
    pub fn get_all_cursors<'js>(&self, ctx: rquickjs::Ctx<'js>) -> rquickjs::Result<Value<'js>> {
        let cursors = if let Ok(s) = self.state_snapshot.read() {
            s.all_cursors.clone()
        } else {
            Vec::new()
        };
        rquickjs_serde::to_value(ctx, &cursors)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Get all cursor positions as byte offsets
    pub fn get_all_cursor_positions<'js>(
        &self,
        ctx: rquickjs::Ctx<'js>,
    ) -> rquickjs::Result<Value<'js>> {
        let positions: Vec<u32> = if let Ok(s) = self.state_snapshot.read() {
            s.all_cursors.iter().map(|c| c.position as u32).collect()
        } else {
            Vec::new()
        };
        rquickjs_serde::to_value(ctx, &positions)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Get viewport info for active buffer
    pub fn get_viewport<'js>(&self, ctx: rquickjs::Ctx<'js>) -> rquickjs::Result<Value<'js>> {
        let viewport = if let Ok(s) = self.state_snapshot.read() {
            s.viewport.clone()
        } else {
            None
        };
        rquickjs_serde::to_value(ctx, &viewport)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Get the line number (0-indexed) of the primary cursor
    pub fn get_cursor_line(&self) -> u32 {
        // This would require line counting from the buffer
        // For now, return 0 - proper implementation needs buffer access
        // TODO: Add line number tracking to EditorStateSnapshot
        0
    }

    /// Find buffer by file path, returns buffer ID or 0 if not found
    pub fn find_buffer_by_path(&self, path: String) -> u32 {
        let path_buf = std::path::PathBuf::from(&path);
        if let Ok(s) = self.state_snapshot.read() {
            for (id, info) in &s.buffers {
                if let Some(buf_path) = &info.path {
                    if buf_path == &path_buf {
                        return id.0 as u32;
                    }
                }
            }
        }
        0
    }

    /// Get diff between buffer content and last saved version
    #[plugin_api(ts_return = "BufferSavedDiff | null")]
    pub fn get_buffer_saved_diff<'js>(
        &self,
        ctx: rquickjs::Ctx<'js>,
        buffer_id: u32,
    ) -> rquickjs::Result<Value<'js>> {
        let diff = if let Ok(s) = self.state_snapshot.read() {
            s.buffer_saved_diffs
                .get(&BufferId(buffer_id as usize))
                .cloned()
        } else {
            None
        };
        rquickjs_serde::to_value(ctx, &diff)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    // === Text Editing ===

    /// Insert text at a position in a buffer
    pub fn insert_text(&self, buffer_id: u32, position: u32, text: String) -> bool {
        self.command_sender
            .send(PluginCommand::InsertText {
                buffer_id: BufferId(buffer_id as usize),
                position: position as usize,
                text,
            })
            .is_ok()
    }

    /// Delete a range from a buffer
    pub fn delete_range(&self, buffer_id: u32, start: u32, end: u32) -> bool {
        self.command_sender
            .send(PluginCommand::DeleteRange {
                buffer_id: BufferId(buffer_id as usize),
                range: (start as usize)..(end as usize),
            })
            .is_ok()
    }

    /// Insert text at cursor position in active buffer
    pub fn insert_at_cursor(&self, text: String) -> bool {
        self.command_sender
            .send(PluginCommand::InsertAtCursor { text })
            .is_ok()
    }

    // === File Operations ===

    /// Open a file, optionally at a specific line/column
    pub fn open_file(&self, path: String, line: Option<u32>, column: Option<u32>) -> bool {
        self.command_sender
            .send(PluginCommand::OpenFileAtLocation {
                path: PathBuf::from(path),
                line: line.map(|l| l as usize),
                column: column.map(|c| c as usize),
            })
            .is_ok()
    }

    /// Open a file in a specific split
    pub fn open_file_in_split(&self, split_id: u32, path: String, line: u32, column: u32) -> bool {
        self.command_sender
            .send(PluginCommand::OpenFileInSplit {
                split_id: split_id as usize,
                path: PathBuf::from(path),
                line: Some(line as usize),
                column: Some(column as usize),
            })
            .is_ok()
    }

    /// Show a buffer in the current split
    pub fn show_buffer(&self, buffer_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::ShowBuffer {
                buffer_id: BufferId(buffer_id as usize),
            })
            .is_ok()
    }

    /// Close a buffer
    pub fn close_buffer(&self, buffer_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::CloseBuffer {
                buffer_id: BufferId(buffer_id as usize),
            })
            .is_ok()
    }

    // === Event Handling ===

    /// Subscribe to an editor event
    pub fn on<'js>(&self, _ctx: rquickjs::Ctx<'js>, event_name: String, handler_name: String) {
        self.event_handlers
            .borrow_mut()
            .entry(event_name)
            .or_default()
            .push(PluginHandler {
                plugin_name: self.plugin_name.clone(),
                handler_name,
            });
    }

    /// Unsubscribe from an event
    pub fn off(&self, event_name: String, handler_name: String) {
        if let Some(list) = self.event_handlers.borrow_mut().get_mut(&event_name) {
            list.retain(|h| h.handler_name != handler_name);
        }
    }

    // === Environment ===

    /// Get an environment variable
    pub fn get_env(&self, name: String) -> Option<String> {
        std::env::var(&name).ok()
    }

    /// Get current working directory
    pub fn get_cwd(&self) -> String {
        self.state_snapshot
            .read()
            .map(|s| s.working_dir.to_string_lossy().to_string())
            .unwrap_or_else(|_| ".".to_string())
    }

    // === Path Operations ===

    /// Join path components (variadic - accepts multiple string arguments)
    /// Always uses forward slashes for cross-platform consistency (like Node.js path.posix.join)
    pub fn path_join(&self, parts: rquickjs::function::Rest<String>) -> String {
        let mut result_parts: Vec<String> = Vec::new();
        let mut has_leading_slash = false;

        for part in &parts.0 {
            // Normalize separators to forward slashes
            let normalized = part.replace('\\', "/");

            // Check if this is an absolute path (starts with / or has drive letter like C:/)
            let is_absolute = normalized.starts_with('/')
                || (normalized.len() >= 2
                    && normalized
                        .chars()
                        .next()
                        .map(|c| c.is_ascii_alphabetic())
                        .unwrap_or(false)
                    && normalized.chars().nth(1) == Some(':'));

            if is_absolute {
                // Reset for absolute paths
                result_parts.clear();
                has_leading_slash = normalized.starts_with('/');
            }

            // Split and add non-empty parts
            for segment in normalized.split('/') {
                if !segment.is_empty() && segment != "." {
                    if segment == ".." {
                        result_parts.pop();
                    } else {
                        result_parts.push(segment.to_string());
                    }
                }
            }
        }

        // Reconstruct with forward slashes
        let joined = result_parts.join("/");

        // Preserve leading slash for Unix absolute paths
        if has_leading_slash && !joined.is_empty() {
            format!("/{}", joined)
        } else {
            joined
        }
    }

    /// Get directory name from path
    pub fn path_dirname(&self, path: String) -> String {
        Path::new(&path)
            .parent()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default()
    }

    /// Get file name from path
    pub fn path_basename(&self, path: String) -> String {
        Path::new(&path)
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_default()
    }

    /// Get file extension
    pub fn path_extname(&self, path: String) -> String {
        Path::new(&path)
            .extension()
            .map(|s| format!(".{}", s.to_string_lossy()))
            .unwrap_or_default()
    }

    /// Check if path is absolute
    pub fn path_is_absolute(&self, path: String) -> bool {
        Path::new(&path).is_absolute()
    }

    // === File System ===

    /// Check if file exists
    pub fn file_exists(&self, path: String) -> bool {
        Path::new(&path).exists()
    }

    /// Read file contents
    pub fn read_file(&self, path: String) -> Option<String> {
        std::fs::read_to_string(&path).ok()
    }

    /// Write file contents
    pub fn write_file(&self, path: String, content: String) -> bool {
        std::fs::write(&path, content).is_ok()
    }

    /// Read directory contents (returns array of {name, is_file, is_dir})
    pub fn read_dir<'js>(
        &self,
        ctx: rquickjs::Ctx<'js>,
        path: String,
    ) -> rquickjs::Result<Value<'js>> {
        #[derive(serde::Serialize)]
        struct DirEntry {
            name: String,
            is_file: bool,
            is_dir: bool,
        }

        let entries: Vec<DirEntry> = match std::fs::read_dir(&path) {
            Ok(entries) => entries
                .filter_map(|e| e.ok())
                .map(|entry| {
                    let file_type = entry.file_type().ok();
                    DirEntry {
                        name: entry.file_name().to_string_lossy().to_string(),
                        is_file: file_type.map(|ft| ft.is_file()).unwrap_or(false),
                        is_dir: file_type.map(|ft| ft.is_dir()).unwrap_or(false),
                    }
                })
                .collect(),
            Err(e) => {
                tracing::warn!("readDir failed for '{}': {}", path, e);
                Vec::new()
            }
        };

        rquickjs_serde::to_value(ctx, &entries)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    // === Config ===

    /// Get current config as JS object
    pub fn get_config<'js>(&self, ctx: rquickjs::Ctx<'js>) -> rquickjs::Result<Value<'js>> {
        let config: serde_json::Value = self
            .state_snapshot
            .read()
            .map(|s| s.config.clone())
            .unwrap_or_else(|_| serde_json::json!({}));

        rquickjs_serde::to_value(ctx, &config)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Get user config as JS object
    pub fn get_user_config<'js>(&self, ctx: rquickjs::Ctx<'js>) -> rquickjs::Result<Value<'js>> {
        let config: serde_json::Value = self
            .state_snapshot
            .read()
            .map(|s| s.user_config.clone())
            .unwrap_or_else(|_| serde_json::json!({}));

        rquickjs_serde::to_value(ctx, &config)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Reload configuration from file
    pub fn reload_config(&self) {
        let _ = self.command_sender.send(PluginCommand::ReloadConfig);
    }

    /// Get config directory path
    pub fn get_config_dir(&self) -> String {
        self.services.config_dir().to_string_lossy().to_string()
    }

    /// Get themes directory path
    pub fn get_themes_dir(&self) -> String {
        self.services
            .config_dir()
            .join("themes")
            .to_string_lossy()
            .to_string()
    }

    /// Apply a theme by name
    pub fn apply_theme(&self, theme_name: String) -> bool {
        self.command_sender
            .send(PluginCommand::ApplyTheme { theme_name })
            .is_ok()
    }

    /// Get theme schema as JS object
    pub fn get_theme_schema<'js>(&self, ctx: rquickjs::Ctx<'js>) -> rquickjs::Result<Value<'js>> {
        let schema = self.services.get_theme_schema();
        rquickjs_serde::to_value(ctx, &schema)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Get list of builtin themes as JS object
    pub fn get_builtin_themes<'js>(&self, ctx: rquickjs::Ctx<'js>) -> rquickjs::Result<Value<'js>> {
        let themes = self.services.get_builtin_themes();
        rquickjs_serde::to_value(ctx, &themes)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Delete a custom theme file (sync)
    #[qjs(rename = "_deleteThemeSync")]
    pub fn delete_theme_sync(&self, name: String) -> bool {
        // Security: only allow deleting from the themes directory
        let themes_dir = self.services.config_dir().join("themes");
        let theme_path = themes_dir.join(format!("{}.json", name));

        // Verify the file is actually in the themes directory (prevent path traversal)
        if let Ok(canonical) = theme_path.canonicalize() {
            if let Ok(themes_canonical) = themes_dir.canonicalize() {
                if canonical.starts_with(&themes_canonical) {
                    return std::fs::remove_file(&canonical).is_ok();
                }
            }
        }
        false
    }

    /// Delete a custom theme (alias for deleteThemeSync)
    pub fn delete_theme(&self, name: String) -> bool {
        self.delete_theme_sync(name)
    }

    // === File Stats ===

    /// Get file stat information
    pub fn file_stat<'js>(
        &self,
        ctx: rquickjs::Ctx<'js>,
        path: String,
    ) -> rquickjs::Result<Value<'js>> {
        let metadata = std::fs::metadata(&path).ok();
        let stat = metadata.map(|m| {
            serde_json::json!({
                "isFile": m.is_file(),
                "isDir": m.is_dir(),
                "size": m.len(),
                "readonly": m.permissions().readonly(),
            })
        });
        rquickjs_serde::to_value(ctx, &stat)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    // === Process Management ===

    /// Check if a background process is still running
    pub fn is_process_running(&self, _process_id: u64) -> bool {
        // This would need to check against tracked processes
        // For now, return false - proper implementation needs process tracking
        false
    }

    /// Kill a process by ID (alias for killBackgroundProcess)
    pub fn kill_process(&self, process_id: u64) -> bool {
        self.command_sender
            .send(PluginCommand::KillBackgroundProcess { process_id })
            .is_ok()
    }

    // === Translation ===

    /// Translate a key for a specific plugin
    pub fn plugin_translate<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        plugin_name: String,
        key: String,
        args: rquickjs::function::Opt<rquickjs::Object<'js>>,
    ) -> String {
        let args_map: HashMap<String, String> = args
            .0
            .map(|obj| {
                let mut map = HashMap::new();
                for result in obj.props::<String, String>() {
                    if let Ok((k, v)) = result {
                        map.insert(k, v);
                    }
                }
                map
            })
            .unwrap_or_default();

        self.services.translate(&plugin_name, &key, &args_map)
    }

    // === Composite Buffers ===

    /// Create a composite buffer (async)
    ///
    /// Uses serde deserialization with `deny_unknown_fields` to validate:
    /// - All required fields are present
    /// - No unknown/misspelled fields are passed
    #[plugin_api(async_promise, js_name = "createCompositeBuffer", ts_return = "number")]
    #[qjs(rename = "_createCompositeBufferStart")]
    pub fn create_composite_buffer_start<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        opts: rquickjs::Value<'js>,
    ) -> rquickjs::Result<u64> {
        use fresh_core::api::CreateCompositeBufferOptions;

        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };

        // Deserialize by going through serde_json::Value first.
        // Direct deserialization with rquickjs_serde has issues with complex nested structures,
        // but deserializing to Value works fine, so we use a two-step approach.
        let json: serde_json::Value = rquickjs_serde::from_value(opts).map_err(|e| {
            rquickjs::Error::new_from_js_message(
                "createCompositeBuffer",
                "opts",
                &format!("Failed to convert to JSON: {}", e),
            )
        })?;

        let parsed: CreateCompositeBufferOptions = serde_json::from_value(json).map_err(|e| {
            rquickjs::Error::new_from_js_message("createCompositeBuffer", "opts", &e.to_string())
        })?;

        let _ = self
            .command_sender
            .send(PluginCommand::CreateCompositeBuffer {
                name: parsed.name,
                mode: parsed.mode,
                layout: parsed.layout,
                sources: parsed.sources,
                hunks: parsed.hunks,
                request_id: Some(id),
            });

        Ok(id)
    }

    /// Update alignment hunks for a composite buffer
    ///
    /// Uses serde deserialization with `deny_unknown_fields` to validate:
    /// - All required fields are present
    /// - No unknown/misspelled fields are passed
    pub fn update_composite_alignment<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        buffer_id: u32,
        hunks: rquickjs::Value<'js>,
    ) -> rquickjs::Result<bool> {
        use fresh_core::api::CompositeHunk;

        // Deserialize using serde - validates required fields and rejects unknown fields
        let parsed_hunks: Vec<CompositeHunk> = rquickjs_serde::from_value(hunks).map_err(|e| {
            rquickjs::Error::new_from_js_message(
                "updateCompositeAlignment",
                "hunks",
                &e.to_string(),
            )
        })?;

        Ok(self
            .command_sender
            .send(PluginCommand::UpdateCompositeAlignment {
                buffer_id: BufferId(buffer_id as usize),
                hunks: parsed_hunks,
            })
            .is_ok())
    }

    /// Close a composite buffer
    pub fn close_composite_buffer(&self, buffer_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::CloseCompositeBuffer {
                buffer_id: BufferId(buffer_id as usize),
            })
            .is_ok()
    }

    // === Highlights ===

    /// Request syntax highlights for a buffer range (async)
    #[plugin_api(
        async_promise,
        js_name = "getHighlights",
        ts_return = "TsHighlightSpan[]"
    )]
    #[qjs(rename = "_getHighlightsStart")]
    pub fn get_highlights_start<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        buffer_id: u32,
        start: u32,
        end: u32,
    ) -> rquickjs::Result<u64> {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record plugin name for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };

        let _ = self.command_sender.send(PluginCommand::RequestHighlights {
            buffer_id: BufferId(buffer_id as usize),
            range: (start as usize)..(end as usize),
            request_id: id,
        });

        Ok(id)
    }

    // === Overlays ===

    /// Add an overlay with styling
    #[allow(clippy::too_many_arguments)]
    pub fn add_overlay(
        &self,
        buffer_id: u32,
        namespace: String,
        start: u32,
        end: u32,
        r: i32,
        g: i32,
        b: i32,
        underline: rquickjs::function::Opt<bool>,
        bold: rquickjs::function::Opt<bool>,
        italic: rquickjs::function::Opt<bool>,
        bg_r: rquickjs::function::Opt<i32>,
        bg_g: rquickjs::function::Opt<i32>,
        bg_b: rquickjs::function::Opt<i32>,
        extend_to_line_end: rquickjs::function::Opt<bool>,
    ) -> bool {
        // -1 means use default color (white)
        let color = if r >= 0 && g >= 0 && b >= 0 {
            (r as u8, g as u8, b as u8)
        } else {
            (255, 255, 255)
        };

        // -1 for bg means no background, also None if not provided
        let bg_r = bg_r.0.unwrap_or(-1);
        let bg_g = bg_g.0.unwrap_or(-1);
        let bg_b = bg_b.0.unwrap_or(-1);
        let bg_color = if bg_r >= 0 && bg_g >= 0 && bg_b >= 0 {
            Some((bg_r as u8, bg_g as u8, bg_b as u8))
        } else {
            None
        };

        self.command_sender
            .send(PluginCommand::AddOverlay {
                buffer_id: BufferId(buffer_id as usize),
                namespace: Some(OverlayNamespace::from_string(namespace)),
                range: (start as usize)..(end as usize),
                color,
                bg_color,
                underline: underline.0.unwrap_or(false),
                bold: bold.0.unwrap_or(false),
                italic: italic.0.unwrap_or(false),
                extend_to_line_end: extend_to_line_end.0.unwrap_or(false),
            })
            .is_ok()
    }

    /// Clear all overlays in a namespace
    pub fn clear_namespace(&self, buffer_id: u32, namespace: String) -> bool {
        self.command_sender
            .send(PluginCommand::ClearNamespace {
                buffer_id: BufferId(buffer_id as usize),
                namespace: OverlayNamespace::from_string(namespace),
            })
            .is_ok()
    }

    /// Clear all overlays from a buffer
    pub fn clear_all_overlays(&self, buffer_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::ClearAllOverlays {
                buffer_id: BufferId(buffer_id as usize),
            })
            .is_ok()
    }

    /// Clear all overlays that overlap with a byte range
    pub fn clear_overlays_in_range(&self, buffer_id: u32, start: u32, end: u32) -> bool {
        self.command_sender
            .send(PluginCommand::ClearOverlaysInRange {
                buffer_id: BufferId(buffer_id as usize),
                start: start as usize,
                end: end as usize,
            })
            .is_ok()
    }

    /// Remove an overlay by its handle
    pub fn remove_overlay(&self, buffer_id: u32, handle: String) -> bool {
        use fresh_core::overlay::OverlayHandle;
        self.command_sender
            .send(PluginCommand::RemoveOverlay {
                buffer_id: BufferId(buffer_id as usize),
                handle: OverlayHandle(handle),
            })
            .is_ok()
    }

    // === View Transform ===

    /// Submit a view transform for a buffer/split
    #[allow(clippy::too_many_arguments)]
    pub fn submit_view_transform<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        buffer_id: u32,
        split_id: Option<u32>,
        start: u32,
        end: u32,
        tokens: Vec<rquickjs::Object<'js>>,
        _layout_hints: rquickjs::function::Opt<rquickjs::Object<'js>>,
    ) -> rquickjs::Result<bool> {
        use fresh_core::api::{
            ViewTokenStyle, ViewTokenWire, ViewTokenWireKind, ViewTransformPayload,
        };

        let tokens: Vec<ViewTokenWire> = tokens
            .into_iter()
            .map(|obj| {
                let kind_str: String = obj.get("kind").unwrap_or_default();
                let text: String = obj.get("text").unwrap_or_default();
                let source_offset: Option<usize> = obj.get("sourceOffset").ok();

                let kind = match kind_str.as_str() {
                    "text" => ViewTokenWireKind::Text(text),
                    "newline" => ViewTokenWireKind::Newline,
                    "space" => ViewTokenWireKind::Space,
                    "break" => ViewTokenWireKind::Break,
                    _ => ViewTokenWireKind::Text(text),
                };

                let style = obj.get::<_, rquickjs::Object>("style").ok().map(|s| {
                    let fg: Option<Vec<u8>> = s.get("fg").ok();
                    let bg: Option<Vec<u8>> = s.get("bg").ok();
                    ViewTokenStyle {
                        fg: fg.and_then(|c| {
                            if c.len() >= 3 {
                                Some((c[0], c[1], c[2]))
                            } else {
                                None
                            }
                        }),
                        bg: bg.and_then(|c| {
                            if c.len() >= 3 {
                                Some((c[0], c[1], c[2]))
                            } else {
                                None
                            }
                        }),
                        bold: s.get("bold").unwrap_or(false),
                        italic: s.get("italic").unwrap_or(false),
                    }
                });

                ViewTokenWire {
                    source_offset,
                    kind,
                    style,
                }
            })
            .collect();

        let payload = ViewTransformPayload {
            range: (start as usize)..(end as usize),
            tokens,
            layout_hints: None,
        };

        Ok(self
            .command_sender
            .send(PluginCommand::SubmitViewTransform {
                buffer_id: BufferId(buffer_id as usize),
                split_id: split_id.map(|id| SplitId(id as usize)),
                payload,
            })
            .is_ok())
    }

    /// Clear view transform for a buffer/split
    pub fn clear_view_transform(&self, buffer_id: u32, split_id: Option<u32>) -> bool {
        self.command_sender
            .send(PluginCommand::ClearViewTransform {
                buffer_id: BufferId(buffer_id as usize),
                split_id: split_id.map(|id| SplitId(id as usize)),
            })
            .is_ok()
    }

    // === File Explorer ===

    /// Set file explorer decorations for a namespace
    pub fn set_file_explorer_decorations<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        namespace: String,
        decorations: Vec<rquickjs::Object<'js>>,
    ) -> rquickjs::Result<bool> {
        use fresh_core::file_explorer::FileExplorerDecoration;

        let decorations: Vec<FileExplorerDecoration> = decorations
            .into_iter()
            .map(|obj| {
                let color: Vec<u8> = obj.get("color").unwrap_or_else(|_| vec![128, 128, 128]);
                FileExplorerDecoration {
                    path: std::path::PathBuf::from(
                        obj.get::<_, String>("path").unwrap_or_default(),
                    ),
                    symbol: obj.get("symbol").unwrap_or_default(),
                    color: if color.len() >= 3 {
                        (color[0], color[1], color[2])
                    } else {
                        (128, 128, 128)
                    },
                    priority: obj.get("priority").unwrap_or(0),
                }
            })
            .collect();

        Ok(self
            .command_sender
            .send(PluginCommand::SetFileExplorerDecorations {
                namespace,
                decorations,
            })
            .is_ok())
    }

    /// Clear file explorer decorations for a namespace
    pub fn clear_file_explorer_decorations(&self, namespace: String) -> bool {
        self.command_sender
            .send(PluginCommand::ClearFileExplorerDecorations { namespace })
            .is_ok()
    }

    // === Virtual Text ===

    /// Add virtual text (inline text that doesn't exist in the buffer)
    #[allow(clippy::too_many_arguments)]
    pub fn add_virtual_text(
        &self,
        buffer_id: u32,
        virtual_text_id: String,
        position: u32,
        text: String,
        r: u8,
        g: u8,
        b: u8,
        before: bool,
        use_bg: bool,
    ) -> bool {
        self.command_sender
            .send(PluginCommand::AddVirtualText {
                buffer_id: BufferId(buffer_id as usize),
                virtual_text_id,
                position: position as usize,
                text,
                color: (r, g, b),
                use_bg,
                before,
            })
            .is_ok()
    }

    /// Remove a virtual text by ID
    pub fn remove_virtual_text(&self, buffer_id: u32, virtual_text_id: String) -> bool {
        self.command_sender
            .send(PluginCommand::RemoveVirtualText {
                buffer_id: BufferId(buffer_id as usize),
                virtual_text_id,
            })
            .is_ok()
    }

    /// Remove virtual texts whose ID starts with the given prefix
    pub fn remove_virtual_texts_by_prefix(&self, buffer_id: u32, prefix: String) -> bool {
        self.command_sender
            .send(PluginCommand::RemoveVirtualTextsByPrefix {
                buffer_id: BufferId(buffer_id as usize),
                prefix,
            })
            .is_ok()
    }

    /// Clear all virtual texts from a buffer
    pub fn clear_virtual_texts(&self, buffer_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::ClearVirtualTexts {
                buffer_id: BufferId(buffer_id as usize),
            })
            .is_ok()
    }

    /// Clear all virtual texts in a namespace
    pub fn clear_virtual_text_namespace(&self, buffer_id: u32, namespace: String) -> bool {
        self.command_sender
            .send(PluginCommand::ClearVirtualTextNamespace {
                buffer_id: BufferId(buffer_id as usize),
                namespace,
            })
            .is_ok()
    }

    /// Add a virtual line (full line above/below a position)
    #[allow(clippy::too_many_arguments)]
    pub fn add_virtual_line(
        &self,
        buffer_id: u32,
        position: u32,
        text: String,
        fg_r: u8,
        fg_g: u8,
        fg_b: u8,
        bg_r: u8,
        bg_g: u8,
        bg_b: u8,
        above: bool,
        namespace: String,
        priority: i32,
    ) -> bool {
        self.command_sender
            .send(PluginCommand::AddVirtualLine {
                buffer_id: BufferId(buffer_id as usize),
                position: position as usize,
                text,
                fg_color: (fg_r, fg_g, fg_b),
                bg_color: Some((bg_r, bg_g, bg_b)),
                above,
                namespace,
                priority,
            })
            .is_ok()
    }

    // === Prompts ===

    /// Start an interactive prompt
    pub fn start_prompt(&self, label: String, prompt_type: String) -> bool {
        self.command_sender
            .send(PluginCommand::StartPrompt { label, prompt_type })
            .is_ok()
    }

    /// Start a prompt with initial value
    pub fn start_prompt_with_initial(
        &self,
        label: String,
        prompt_type: String,
        initial_value: String,
    ) -> bool {
        self.command_sender
            .send(PluginCommand::StartPromptWithInitial {
                label,
                prompt_type,
                initial_value,
            })
            .is_ok()
    }

    /// Set suggestions for the current prompt (takes array of suggestion objects)
    pub fn set_prompt_suggestions<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        suggestions_arr: Vec<rquickjs::Object<'js>>,
    ) -> rquickjs::Result<bool> {
        let suggestions: Vec<fresh_core::command::Suggestion> = suggestions_arr
            .into_iter()
            .map(|obj| fresh_core::command::Suggestion {
                text: obj.get("text").unwrap_or_default(),
                description: obj.get("description").ok(),
                value: obj.get("value").ok(),
                disabled: obj.get("disabled").unwrap_or(false),
                keybinding: obj.get("keybinding").ok(),
                source: None,
            })
            .collect();
        Ok(self
            .command_sender
            .send(PluginCommand::SetPromptSuggestions { suggestions })
            .is_ok())
    }

    // === Modes ===

    /// Define a buffer mode (takes bindings as array of [key, command] pairs)
    pub fn define_mode(
        &self,
        name: String,
        parent: Option<String>,
        bindings_arr: Vec<Vec<String>>,
        read_only: rquickjs::function::Opt<bool>,
    ) -> bool {
        let bindings: Vec<(String, String)> = bindings_arr
            .into_iter()
            .filter_map(|arr| {
                if arr.len() >= 2 {
                    Some((arr[0].clone(), arr[1].clone()))
                } else {
                    None
                }
            })
            .collect();

        // Register commands associated with this mode so start_action can find them
        // and execute them in the correct plugin context
        {
            let mut registered = self.registered_actions.borrow_mut();
            for (_, cmd_name) in &bindings {
                registered.insert(
                    cmd_name.clone(),
                    PluginHandler {
                        plugin_name: self.plugin_name.clone(),
                        handler_name: cmd_name.clone(),
                    },
                );
            }
        }

        self.command_sender
            .send(PluginCommand::DefineMode {
                name,
                parent,
                bindings,
                read_only: read_only.0.unwrap_or(false),
            })
            .is_ok()
    }

    /// Set the global editor mode
    pub fn set_editor_mode(&self, mode: Option<String>) -> bool {
        self.command_sender
            .send(PluginCommand::SetEditorMode { mode })
            .is_ok()
    }

    /// Get the current editor mode
    pub fn get_editor_mode(&self) -> Option<String> {
        self.state_snapshot
            .read()
            .ok()
            .and_then(|s| s.editor_mode.clone())
    }

    // === Splits ===

    /// Close a split
    pub fn close_split(&self, split_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::CloseSplit {
                split_id: SplitId(split_id as usize),
            })
            .is_ok()
    }

    /// Set the buffer displayed in a split
    pub fn set_split_buffer(&self, split_id: u32, buffer_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::SetSplitBuffer {
                split_id: SplitId(split_id as usize),
                buffer_id: BufferId(buffer_id as usize),
            })
            .is_ok()
    }

    /// Focus a specific split
    pub fn focus_split(&self, split_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::FocusSplit {
                split_id: SplitId(split_id as usize),
            })
            .is_ok()
    }

    /// Set scroll position of a split
    pub fn set_split_scroll(&self, split_id: u32, top_byte: u32) -> bool {
        self.command_sender
            .send(PluginCommand::SetSplitScroll {
                split_id: SplitId(split_id as usize),
                top_byte: top_byte as usize,
            })
            .is_ok()
    }

    /// Set the ratio of a split (0.0 to 1.0, 0.5 = equal)
    pub fn set_split_ratio(&self, split_id: u32, ratio: f32) -> bool {
        self.command_sender
            .send(PluginCommand::SetSplitRatio {
                split_id: SplitId(split_id as usize),
                ratio,
            })
            .is_ok()
    }

    /// Distribute all splits evenly
    pub fn distribute_splits_evenly(&self) -> bool {
        // Get all split IDs - for now send empty vec (app will handle)
        self.command_sender
            .send(PluginCommand::DistributeSplitsEvenly { split_ids: vec![] })
            .is_ok()
    }

    /// Set cursor position in a buffer
    pub fn set_buffer_cursor(&self, buffer_id: u32, position: u32) -> bool {
        self.command_sender
            .send(PluginCommand::SetBufferCursor {
                buffer_id: BufferId(buffer_id as usize),
                position: position as usize,
            })
            .is_ok()
    }

    // === Line Indicators ===

    /// Set a line indicator in the gutter
    #[allow(clippy::too_many_arguments)]
    pub fn set_line_indicator(
        &self,
        buffer_id: u32,
        line: u32,
        namespace: String,
        symbol: String,
        r: u8,
        g: u8,
        b: u8,
        priority: i32,
    ) -> bool {
        self.command_sender
            .send(PluginCommand::SetLineIndicator {
                buffer_id: BufferId(buffer_id as usize),
                line: line as usize,
                namespace,
                symbol,
                color: (r, g, b),
                priority,
            })
            .is_ok()
    }

    /// Clear line indicators in a namespace
    pub fn clear_line_indicators(&self, buffer_id: u32, namespace: String) -> bool {
        self.command_sender
            .send(PluginCommand::ClearLineIndicators {
                buffer_id: BufferId(buffer_id as usize),
                namespace,
            })
            .is_ok()
    }

    /// Enable or disable line numbers for a buffer
    pub fn set_line_numbers(&self, buffer_id: u32, enabled: bool) -> bool {
        self.command_sender
            .send(PluginCommand::SetLineNumbers {
                buffer_id: BufferId(buffer_id as usize),
                enabled,
            })
            .is_ok()
    }

    // === Scroll Sync ===

    /// Create a scroll sync group for anchor-based synchronized scrolling
    pub fn create_scroll_sync_group(
        &self,
        group_id: u32,
        left_split: u32,
        right_split: u32,
    ) -> bool {
        self.command_sender
            .send(PluginCommand::CreateScrollSyncGroup {
                group_id,
                left_split: SplitId(left_split as usize),
                right_split: SplitId(right_split as usize),
            })
            .is_ok()
    }

    /// Set sync anchors for a scroll sync group
    pub fn set_scroll_sync_anchors<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        group_id: u32,
        anchors: Vec<Vec<u32>>,
    ) -> bool {
        let anchors: Vec<(usize, usize)> = anchors
            .into_iter()
            .filter_map(|pair| {
                if pair.len() >= 2 {
                    Some((pair[0] as usize, pair[1] as usize))
                } else {
                    None
                }
            })
            .collect();
        self.command_sender
            .send(PluginCommand::SetScrollSyncAnchors { group_id, anchors })
            .is_ok()
    }

    /// Remove a scroll sync group
    pub fn remove_scroll_sync_group(&self, group_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::RemoveScrollSyncGroup { group_id })
            .is_ok()
    }

    // === Actions ===

    /// Execute multiple actions in sequence
    pub fn execute_actions<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        actions: Vec<rquickjs::Object<'js>>,
    ) -> rquickjs::Result<bool> {
        let specs: Vec<ActionSpec> = actions
            .into_iter()
            .map(|obj| ActionSpec {
                action: obj.get("action").unwrap_or_default(),
                count: obj.get("count").unwrap_or(1),
            })
            .collect();
        Ok(self
            .command_sender
            .send(PluginCommand::ExecuteActions { actions: specs })
            .is_ok())
    }

    /// Show an action popup
    pub fn show_action_popup<'js>(
        &self,
        _ctx: rquickjs::Ctx<'js>,
        opts: rquickjs::Object<'js>,
    ) -> rquickjs::Result<bool> {
        let popup_id: String = opts.get("popupId").unwrap_or_default();
        let title: String = opts.get("title").unwrap_or_default();
        let message: String = opts.get("message").unwrap_or_default();
        let actions_arr: Vec<rquickjs::Object> = opts.get("actions").unwrap_or_default();

        let actions: Vec<ActionPopupAction> = actions_arr
            .into_iter()
            .map(|obj| ActionPopupAction {
                id: obj.get("id").unwrap_or_default(),
                label: obj.get("label").unwrap_or_default(),
            })
            .collect();

        Ok(self
            .command_sender
            .send(PluginCommand::ShowActionPopup {
                popup_id,
                title,
                message,
                actions,
            })
            .is_ok())
    }

    /// Disable LSP for a specific language
    pub fn disable_lsp_for_language(&self, language: String) -> bool {
        self.command_sender
            .send(PluginCommand::DisableLspForLanguage { language })
            .is_ok()
    }

    /// Get all diagnostics from LSP
    pub fn get_all_diagnostics<'js>(
        &self,
        ctx: rquickjs::Ctx<'js>,
    ) -> rquickjs::Result<Value<'js>> {
        let diagnostics = if let Ok(s) = self.state_snapshot.read() {
            // Convert to a simpler format for JS
            let mut result: Vec<serde_json::Value> = Vec::new();
            for (uri, diags) in &s.diagnostics {
                for diag in diags {
                    result.push(serde_json::json!({
                        "uri": uri,
                        "message": diag.message,
                        "severity": diag.severity.map(|s| match s {
                            lsp_types::DiagnosticSeverity::ERROR => 1,
                            lsp_types::DiagnosticSeverity::WARNING => 2,
                            lsp_types::DiagnosticSeverity::INFORMATION => 3,
                            lsp_types::DiagnosticSeverity::HINT => 4,
                            _ => 0,
                        }),
                        "range": {
                            "start": {"line": diag.range.start.line, "character": diag.range.start.character},
                            "end": {"line": diag.range.end.line, "character": diag.range.end.character}
                        }
                    }));
                }
            }
            result
        } else {
            Vec::new()
        };
        rquickjs_serde::to_value(ctx, &diagnostics)
            .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
    }

    /// Get registered event handlers for an event
    pub fn get_handlers(&self, event_name: String) -> Vec<String> {
        self.event_handlers
            .borrow()
            .get(&event_name)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|h| h.handler_name)
            .collect()
    }

    // === Virtual Buffers ===

    /// Create a virtual buffer in current split (async, returns buffer ID)
    #[plugin_api(async_promise, js_name = "createVirtualBuffer", ts_return = "number")]
    #[qjs(rename = "_createVirtualBufferStart")]
    pub fn create_virtual_buffer_start(
        &self,
        _ctx: rquickjs::Ctx<'_>,
        opts: fresh_core::api::CreateVirtualBufferOptions,
    ) -> rquickjs::Result<u64> {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };

        // Convert JsTextPropertyEntry to TextPropertyEntry
        let entries: Vec<TextPropertyEntry> = opts
            .entries
            .unwrap_or_default()
            .into_iter()
            .map(|e| TextPropertyEntry {
                text: e.text,
                properties: e.properties.unwrap_or_default(),
            })
            .collect();

        tracing::debug!(
            "_createVirtualBufferStart: sending CreateVirtualBufferWithContent command, request_id={}",
            id
        );
        let _ = self
            .command_sender
            .send(PluginCommand::CreateVirtualBufferWithContent {
                name: opts.name,
                mode: opts.mode.unwrap_or_default(),
                read_only: opts.read_only.unwrap_or(false),
                entries,
                show_line_numbers: opts.show_line_numbers.unwrap_or(false),
                show_cursors: opts.show_cursors.unwrap_or(true),
                editing_disabled: opts.editing_disabled.unwrap_or(false),
                hidden_from_tabs: opts.hidden_from_tabs.unwrap_or(false),
                request_id: Some(id),
            });
        Ok(id)
    }

    /// Create a virtual buffer in a new split (async, returns request_id)
    #[plugin_api(
        async_promise,
        js_name = "createVirtualBufferInSplit",
        ts_return = "number"
    )]
    #[qjs(rename = "_createVirtualBufferInSplitStart")]
    pub fn create_virtual_buffer_in_split_start(
        &self,
        _ctx: rquickjs::Ctx<'_>,
        opts: fresh_core::api::CreateVirtualBufferInSplitOptions,
    ) -> rquickjs::Result<u64> {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };

        // Convert JsTextPropertyEntry to TextPropertyEntry
        let entries: Vec<TextPropertyEntry> = opts
            .entries
            .unwrap_or_default()
            .into_iter()
            .map(|e| TextPropertyEntry {
                text: e.text,
                properties: e.properties.unwrap_or_default(),
            })
            .collect();

        let _ = self
            .command_sender
            .send(PluginCommand::CreateVirtualBufferInSplit {
                name: opts.name,
                mode: opts.mode.unwrap_or_default(),
                read_only: opts.read_only.unwrap_or(false),
                entries,
                ratio: opts.ratio.unwrap_or(0.5),
                direction: opts.direction,
                panel_id: opts.panel_id,
                show_line_numbers: opts.show_line_numbers.unwrap_or(true),
                show_cursors: opts.show_cursors.unwrap_or(true),
                editing_disabled: opts.editing_disabled.unwrap_or(false),
                line_wrap: opts.line_wrap,
                request_id: Some(id),
            });
        Ok(id)
    }

    /// Create a virtual buffer in an existing split (async, returns request_id)
    #[plugin_api(
        async_promise,
        js_name = "createVirtualBufferInExistingSplit",
        ts_return = "number"
    )]
    #[qjs(rename = "_createVirtualBufferInExistingSplitStart")]
    pub fn create_virtual_buffer_in_existing_split_start(
        &self,
        _ctx: rquickjs::Ctx<'_>,
        opts: fresh_core::api::CreateVirtualBufferInExistingSplitOptions,
    ) -> rquickjs::Result<u64> {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };

        // Convert JsTextPropertyEntry to TextPropertyEntry
        let entries: Vec<TextPropertyEntry> = opts
            .entries
            .unwrap_or_default()
            .into_iter()
            .map(|e| TextPropertyEntry {
                text: e.text,
                properties: e.properties.unwrap_or_default(),
            })
            .collect();

        let _ = self
            .command_sender
            .send(PluginCommand::CreateVirtualBufferInExistingSplit {
                name: opts.name,
                mode: opts.mode.unwrap_or_default(),
                read_only: opts.read_only.unwrap_or(false),
                entries,
                split_id: SplitId(opts.split_id),
                show_line_numbers: opts.show_line_numbers.unwrap_or(true),
                show_cursors: opts.show_cursors.unwrap_or(true),
                editing_disabled: opts.editing_disabled.unwrap_or(false),
                line_wrap: opts.line_wrap,
                request_id: Some(id),
            });
        Ok(id)
    }

    /// Set virtual buffer content (takes array of entry objects)
    pub fn set_virtual_buffer_content<'js>(
        &self,
        ctx: rquickjs::Ctx<'js>,
        buffer_id: u32,
        entries_arr: Vec<rquickjs::Object<'js>>,
    ) -> rquickjs::Result<bool> {
        let entries: Vec<TextPropertyEntry> = entries_arr
            .iter()
            .filter_map(|obj| parse_text_property_entry(&ctx, obj))
            .collect();
        Ok(self
            .command_sender
            .send(PluginCommand::SetVirtualBufferContent {
                buffer_id: BufferId(buffer_id as usize),
                entries,
            })
            .is_ok())
    }

    /// Get text properties at cursor position (returns JS array)
    pub fn get_text_properties_at_cursor(
        &self,
        buffer_id: u32,
    ) -> fresh_core::api::TextPropertiesAtCursor {
        get_text_properties_at_cursor_typed(&self.state_snapshot, buffer_id)
    }

    // === Async Operations ===

    /// Spawn a process (async, returns request_id)
    #[plugin_api(async_thenable, js_name = "spawnProcess", ts_return = "SpawnResult")]
    #[qjs(rename = "_spawnProcessStart")]
    pub fn spawn_process_start(
        &self,
        _ctx: rquickjs::Ctx<'_>,
        command: String,
        args: Vec<String>,
        cwd: rquickjs::function::Opt<String>,
    ) -> u64 {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };
        // Use provided cwd, or fall back to snapshot's working_dir
        let effective_cwd = cwd.0.or_else(|| {
            self.state_snapshot
                .read()
                .ok()
                .map(|s| s.working_dir.to_string_lossy().to_string())
        });
        tracing::info!(
            "spawn_process_start: command='{}', args={:?}, cwd={:?}, callback_id={}",
            command,
            args,
            effective_cwd,
            id
        );
        let _ = self.command_sender.send(PluginCommand::SpawnProcess {
            callback_id: JsCallbackId::new(id),
            command,
            args,
            cwd: effective_cwd,
        });
        id
    }

    /// Wait for a process to complete and get its result (async)
    #[plugin_api(async_promise, js_name = "spawnProcessWait", ts_return = "SpawnResult")]
    #[qjs(rename = "_spawnProcessWaitStart")]
    pub fn spawn_process_wait_start(&self, _ctx: rquickjs::Ctx<'_>, process_id: u64) -> u64 {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };
        let _ = self.command_sender.send(PluginCommand::SpawnProcessWait {
            process_id,
            callback_id: JsCallbackId::new(id),
        });
        id
    }

    /// Get buffer text range (async, returns request_id)
    #[plugin_api(async_promise, js_name = "getBufferText", ts_return = "string")]
    #[qjs(rename = "_getBufferTextStart")]
    pub fn get_buffer_text_start(
        &self,
        _ctx: rquickjs::Ctx<'_>,
        buffer_id: u32,
        start: u32,
        end: u32,
    ) -> u64 {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };
        let _ = self.command_sender.send(PluginCommand::GetBufferText {
            buffer_id: BufferId(buffer_id as usize),
            start: start as usize,
            end: end as usize,
            request_id: id,
        });
        id
    }

    /// Delay/sleep (async, returns request_id)
    #[plugin_api(async_promise, js_name = "delay", ts_return = "void")]
    #[qjs(rename = "_delayStart")]
    pub fn delay_start(&self, _ctx: rquickjs::Ctx<'_>, duration_ms: u64) -> u64 {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };
        let _ = self.command_sender.send(PluginCommand::Delay {
            callback_id: JsCallbackId::new(id),
            duration_ms,
        });
        id
    }

    /// Send LSP request (async, returns request_id)
    #[plugin_api(async_promise, js_name = "sendLspRequest", ts_return = "unknown")]
    #[qjs(rename = "_sendLspRequestStart")]
    pub fn send_lsp_request_start<'js>(
        &self,
        ctx: rquickjs::Ctx<'js>,
        language: String,
        method: String,
        params: Option<rquickjs::Object<'js>>,
    ) -> rquickjs::Result<u64> {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };
        // Convert params object to serde_json::Value
        let params_json: Option<serde_json::Value> = params.map(|obj| {
            let val = obj.into_value();
            js_to_json(&ctx, val)
        });
        let _ = self.command_sender.send(PluginCommand::SendLspRequest {
            request_id: id,
            language,
            method,
            params: params_json,
        });
        Ok(id)
    }

    /// Spawn a background process (async, returns request_id which is also process_id)
    #[plugin_api(
        async_thenable,
        js_name = "spawnBackgroundProcess",
        ts_return = "BackgroundProcessResult"
    )]
    #[qjs(rename = "_spawnBackgroundProcessStart")]
    pub fn spawn_background_process_start(
        &self,
        _ctx: rquickjs::Ctx<'_>,
        command: String,
        args: Vec<String>,
        cwd: rquickjs::function::Opt<String>,
    ) -> u64 {
        let id = {
            let mut id_ref = self.next_request_id.borrow_mut();
            let id = *id_ref;
            *id_ref += 1;
            // Record context for this callback
            self.callback_contexts
                .borrow_mut()
                .insert(id, self.plugin_name.clone());
            id
        };
        // Use id as process_id for simplicity
        let process_id = id;
        let _ = self
            .command_sender
            .send(PluginCommand::SpawnBackgroundProcess {
                process_id,
                command,
                args,
                cwd: cwd.0,
                callback_id: JsCallbackId::new(id),
            });
        id
    }

    /// Kill a background process
    pub fn kill_background_process(&self, process_id: u64) -> bool {
        self.command_sender
            .send(PluginCommand::KillBackgroundProcess { process_id })
            .is_ok()
    }

    // === Misc ===

    /// Force refresh of line display
    pub fn refresh_lines(&self, buffer_id: u32) -> bool {
        self.command_sender
            .send(PluginCommand::RefreshLines {
                buffer_id: BufferId(buffer_id as usize),
            })
            .is_ok()
    }

    /// Get the current locale
    pub fn get_current_locale(&self) -> String {
        self.services.current_locale()
    }
}

/// QuickJS-based JavaScript runtime for plugins
pub struct QuickJsBackend {
    runtime: Runtime,
    /// Main context for shared/internal operations
    main_context: Context,
    /// Plugin-specific contexts: plugin_name -> Context
    plugin_contexts: Rc<RefCell<HashMap<String, Context>>>,
    /// Event handlers: event_name -> list of PluginHandler
    event_handlers: Rc<RefCell<HashMap<String, Vec<PluginHandler>>>>,
    /// Registered actions: action_name -> PluginHandler
    registered_actions: Rc<RefCell<HashMap<String, PluginHandler>>>,
    /// Editor state snapshot (read-only access)
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
    /// Command sender for write operations
    command_sender: mpsc::Sender<PluginCommand>,
    /// Pending response senders for async operations (held to keep Arc alive)
    #[allow(dead_code)]
    pending_responses: PendingResponses,
    /// Next request ID for async operations
    next_request_id: Rc<RefCell<u64>>,
    /// Plugin name for each pending callback ID
    callback_contexts: Rc<RefCell<HashMap<u64, String>>>,
    /// Bridge for editor services (i18n, theme, etc.)
    pub services: Arc<dyn fresh_core::services::PluginServiceBridge>,
}

impl QuickJsBackend {
    /// Create a new QuickJS backend (standalone, for testing)
    pub fn new() -> Result<Self> {
        let (tx, _rx) = mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let services = Arc::new(fresh_core::services::NoopServiceBridge);
        Self::with_state(state_snapshot, tx, services)
    }

    /// Create a new QuickJS backend with editor state
    pub fn with_state(
        state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
        command_sender: mpsc::Sender<PluginCommand>,
        services: Arc<dyn fresh_core::services::PluginServiceBridge>,
    ) -> Result<Self> {
        let pending_responses: PendingResponses = Arc::new(std::sync::Mutex::new(HashMap::new()));
        Self::with_state_and_responses(state_snapshot, command_sender, pending_responses, services)
    }

    /// Create a new QuickJS backend with editor state and shared pending responses
    pub fn with_state_and_responses(
        state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
        command_sender: mpsc::Sender<PluginCommand>,
        pending_responses: PendingResponses,
        services: Arc<dyn fresh_core::services::PluginServiceBridge>,
    ) -> Result<Self> {
        tracing::debug!("QuickJsBackend::new: creating QuickJS runtime");

        let runtime =
            Runtime::new().map_err(|e| anyhow!("Failed to create QuickJS runtime: {}", e))?;

        // Set up promise rejection tracker to catch unhandled rejections
        runtime.set_host_promise_rejection_tracker(Some(Box::new(
            |_ctx, _promise, reason, is_handled| {
                if !is_handled {
                    // Format the rejection reason
                    let error_msg = if let Some(exc) = reason.as_exception() {
                        format!(
                            "{}: {}",
                            exc.message().unwrap_or_default(),
                            exc.stack().unwrap_or_default()
                        )
                    } else {
                        format!("{:?}", reason)
                    };

                    tracing::error!("Unhandled Promise rejection: {}", error_msg);

                    if should_panic_on_js_errors() {
                        // Don't panic here - we're inside an FFI callback and rquickjs catches panics.
                        // Instead, set a fatal error flag that the plugin thread loop will check.
                        let full_msg = format!("Unhandled Promise rejection: {}", error_msg);
                        set_fatal_js_error(full_msg);
                    }
                }
            },
        )));

        let main_context = Context::full(&runtime)
            .map_err(|e| anyhow!("Failed to create QuickJS context: {}", e))?;

        let plugin_contexts = Rc::new(RefCell::new(HashMap::new()));
        let event_handlers = Rc::new(RefCell::new(HashMap::new()));
        let registered_actions = Rc::new(RefCell::new(HashMap::new()));
        let next_request_id = Rc::new(RefCell::new(1u64));
        let callback_contexts = Rc::new(RefCell::new(HashMap::new()));

        let backend = Self {
            runtime,
            main_context,
            plugin_contexts,
            event_handlers,
            registered_actions,
            state_snapshot,
            command_sender,
            pending_responses,
            next_request_id,
            callback_contexts,
            services,
        };

        // Initialize main context (for internal utilities if needed)
        backend.setup_context_api(&backend.main_context.clone(), "internal")?;

        tracing::debug!("QuickJsBackend::new: runtime created successfully");
        Ok(backend)
    }

    /// Set up the editor API in a specific JavaScript context
    fn setup_context_api(&self, context: &Context, plugin_name: &str) -> Result<()> {
        let state_snapshot = Arc::clone(&self.state_snapshot);
        let command_sender = self.command_sender.clone();
        let event_handlers = Rc::clone(&self.event_handlers);
        let registered_actions = Rc::clone(&self.registered_actions);
        let next_request_id = Rc::clone(&self.next_request_id);

        context.with(|ctx| {
            let globals = ctx.globals();

            // Set the plugin name global
            globals.set("__pluginName__", plugin_name)?;

            // Create the editor object using JsEditorApi class
            // This provides proper lifetime handling for methods returning JS values
            let js_api = JsEditorApi {
                state_snapshot: Arc::clone(&state_snapshot),
                command_sender: command_sender.clone(),
                registered_actions: Rc::clone(&registered_actions),
                event_handlers: Rc::clone(&event_handlers),
                next_request_id: Rc::clone(&next_request_id),
                callback_contexts: Rc::clone(&self.callback_contexts),
                services: self.services.clone(),
                plugin_name: plugin_name.to_string(),
            };
            let editor = rquickjs::Class::<JsEditorApi>::instance(ctx.clone(), js_api)?;

            // All methods are now in JsEditorApi - export editor as global
            globals.set("editor", editor)?;

            // Define getEditor() globally
            ctx.eval::<(), _>("globalThis.getEditor = function() { return editor; };")?;

            // Provide console.log for debugging
            // Use Rest<T> to handle variadic arguments like console.log('a', 'b', obj)
            let console = Object::new(ctx.clone())?;
            console.set("log", Function::new(ctx.clone(), |ctx: rquickjs::Ctx, args: rquickjs::function::Rest<rquickjs::Value>| {
                let parts: Vec<String> = args.0.iter().map(|v| js_value_to_string(&ctx, v)).collect();
                tracing::info!("console.log: {}", parts.join(" "));
            })?)?;
            console.set("warn", Function::new(ctx.clone(), |ctx: rquickjs::Ctx, args: rquickjs::function::Rest<rquickjs::Value>| {
                let parts: Vec<String> = args.0.iter().map(|v| js_value_to_string(&ctx, v)).collect();
                tracing::warn!("console.warn: {}", parts.join(" "));
            })?)?;
            console.set("error", Function::new(ctx.clone(), |ctx: rquickjs::Ctx, args: rquickjs::function::Rest<rquickjs::Value>| {
                let parts: Vec<String> = args.0.iter().map(|v| js_value_to_string(&ctx, v)).collect();
                tracing::error!("console.error: {}", parts.join(" "));
            })?)?;
            globals.set("console", console)?;

            // Bootstrap: Promise infrastructure (getEditor is defined per-plugin in execute_js)
            ctx.eval::<(), _>(r#"
                // Pending promise callbacks: callbackId -> { resolve, reject }
                globalThis._pendingCallbacks = new Map();

                // Resolve a pending callback (called from Rust)
                globalThis._resolveCallback = function(callbackId, result) {
                    console.log('[JS] _resolveCallback called with callbackId=' + callbackId + ', pendingCallbacks.size=' + globalThis._pendingCallbacks.size);
                    const cb = globalThis._pendingCallbacks.get(callbackId);
                    if (cb) {
                        console.log('[JS] _resolveCallback: found callback, calling resolve()');
                        globalThis._pendingCallbacks.delete(callbackId);
                        cb.resolve(result);
                        console.log('[JS] _resolveCallback: resolve() called');
                    } else {
                        console.log('[JS] _resolveCallback: NO callback found for id=' + callbackId);
                    }
                };

                // Reject a pending callback (called from Rust)
                globalThis._rejectCallback = function(callbackId, error) {
                    const cb = globalThis._pendingCallbacks.get(callbackId);
                    if (cb) {
                        globalThis._pendingCallbacks.delete(callbackId);
                        cb.reject(new Error(error));
                    }
                };

                // Generic async wrapper decorator
                // Wraps a function that returns a callbackId into a promise-returning function
                // Usage: editor.foo = _wrapAsync("_fooStart", "foo");
                // NOTE: We pass the method name as a string and call via bracket notation
                // to preserve rquickjs's automatic Ctx injection for methods
                globalThis._wrapAsync = function(methodName, fnName) {
                    const startFn = editor[methodName];
                    if (typeof startFn !== 'function') {
                        // Return a function that always throws - catches missing implementations
                        return function(...args) {
                            const error = new Error(`editor.${fnName || 'unknown'} is not implemented (missing ${methodName})`);
                            editor.debug(`[ASYNC ERROR] ${error.message}`);
                            throw error;
                        };
                    }
                    return function(...args) {
                        // Call via bracket notation to preserve method binding and Ctx injection
                        const callbackId = editor[methodName](...args);
                        return new Promise((resolve, reject) => {
                            // NOTE: setTimeout not available in QuickJS - timeout disabled for now
                            // TODO: Implement setTimeout polyfill using editor.delay() or similar
                            globalThis._pendingCallbacks.set(callbackId, { resolve, reject });
                        });
                    };
                };

                // Async wrapper that returns a thenable object (for APIs like spawnProcess)
                // The returned object has .result promise and is itself thenable
                globalThis._wrapAsyncThenable = function(methodName, fnName) {
                    const startFn = editor[methodName];
                    if (typeof startFn !== 'function') {
                        // Return a function that always throws - catches missing implementations
                        return function(...args) {
                            const error = new Error(`editor.${fnName || 'unknown'} is not implemented (missing ${methodName})`);
                            editor.debug(`[ASYNC ERROR] ${error.message}`);
                            throw error;
                        };
                    }
                    return function(...args) {
                        // Call via bracket notation to preserve method binding and Ctx injection
                        const callbackId = editor[methodName](...args);
                        const resultPromise = new Promise((resolve, reject) => {
                            // NOTE: setTimeout not available in QuickJS - timeout disabled for now
                            globalThis._pendingCallbacks.set(callbackId, { resolve, reject });
                        });
                        return {
                            get result() { return resultPromise; },
                            then(onFulfilled, onRejected) {
                                return resultPromise.then(onFulfilled, onRejected);
                            },
                            catch(onRejected) {
                                return resultPromise.catch(onRejected);
                            }
                        };
                    };
                };

                // Apply wrappers to async functions on editor
                editor.spawnProcess = _wrapAsyncThenable("_spawnProcessStart", "spawnProcess");
                editor.delay = _wrapAsync("_delayStart", "delay");
                editor.createVirtualBuffer = _wrapAsync("_createVirtualBufferStart", "createVirtualBuffer");
                editor.createVirtualBufferInSplit = _wrapAsync("_createVirtualBufferInSplitStart", "createVirtualBufferInSplit");
                editor.createVirtualBufferInExistingSplit = _wrapAsync("_createVirtualBufferInExistingSplitStart", "createVirtualBufferInExistingSplit");
                editor.sendLspRequest = _wrapAsync("_sendLspRequestStart", "sendLspRequest");
                editor.spawnBackgroundProcess = _wrapAsyncThenable("_spawnBackgroundProcessStart", "spawnBackgroundProcess");
                editor.spawnProcessWait = _wrapAsync("_spawnProcessWaitStart", "spawnProcessWait");
                editor.getBufferText = _wrapAsync("_getBufferTextStart", "getBufferText");
                editor.createCompositeBuffer = _wrapAsync("_createCompositeBufferStart", "createCompositeBuffer");
                editor.getHighlights = _wrapAsync("_getHighlightsStart", "getHighlights");

                // Wrapper for deleteTheme - wraps sync function in Promise
                editor.deleteTheme = function(name) {
                    return new Promise(function(resolve, reject) {
                        const success = editor._deleteThemeSync(name);
                        if (success) {
                            resolve();
                        } else {
                            reject(new Error("Failed to delete theme: " + name));
                        }
                    });
                };
            "#.as_bytes())?;

            Ok::<_, rquickjs::Error>(())
        }).map_err(|e| anyhow!("Failed to set up global API: {}", e))?;

        Ok(())
    }

    /// Load and execute a TypeScript/JavaScript plugin from a file path
    pub async fn load_module_with_source(
        &mut self,
        path: &str,
        _plugin_source: &str,
    ) -> Result<()> {
        let path_buf = PathBuf::from(path);
        let source = std::fs::read_to_string(&path_buf)
            .map_err(|e| anyhow!("Failed to read plugin {}: {}", path, e))?;

        let filename = path_buf
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("plugin.ts");

        // Check for ES imports - these need bundling to resolve dependencies
        if has_es_imports(&source) {
            // Try to bundle (this also strips imports and exports)
            match bundle_module(&path_buf) {
                Ok(bundled) => {
                    self.execute_js(&bundled, path)?;
                }
                Err(e) => {
                    tracing::warn!(
                        "Plugin {} uses ES imports but bundling failed: {}. Skipping.",
                        path,
                        e
                    );
                    return Ok(()); // Skip plugins with unresolvable imports
                }
            }
        } else if has_es_module_syntax(&source) {
            // Has exports but no imports - strip exports and transpile
            let stripped = strip_imports_and_exports(&source);
            let js_code = if filename.ends_with(".ts") {
                transpile_typescript(&stripped, filename)?
            } else {
                stripped
            };
            self.execute_js(&js_code, path)?;
        } else {
            // Plain code - just transpile if TypeScript
            let js_code = if filename.ends_with(".ts") {
                transpile_typescript(&source, filename)?
            } else {
                source
            };
            self.execute_js(&js_code, path)?;
        }

        Ok(())
    }

    /// Execute JavaScript code in the context
    fn execute_js(&mut self, code: &str, source_name: &str) -> Result<()> {
        // Extract plugin name from path (filename without extension)
        let plugin_name = Path::new(source_name)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        tracing::debug!(
            "execute_js: starting for plugin '{}' from '{}'",
            plugin_name,
            source_name
        );

        // Get or create context for this plugin
        let context = {
            let mut contexts = self.plugin_contexts.borrow_mut();
            if let Some(ctx) = contexts.get(plugin_name) {
                ctx.clone()
            } else {
                let ctx = Context::full(&self.runtime).map_err(|e| {
                    anyhow!(
                        "Failed to create QuickJS context for plugin {}: {}",
                        plugin_name,
                        e
                    )
                })?;
                self.setup_context_api(&ctx, plugin_name)?;
                contexts.insert(plugin_name.to_string(), ctx.clone());
                ctx
            }
        };

        // Wrap plugin code in IIFE to prevent TDZ errors and scope pollution
        // This is critical for plugins like vi_mode that declare `const editor = ...`
        // which shadows the global `editor` causing TDZ if not wrapped.
        let wrapped_code = format!("(function() {{ {} }})();", code);
        let wrapped = wrapped_code.as_str();

        context.with(|ctx| {
            tracing::debug!("execute_js: executing plugin code for '{}'", plugin_name);

            // Execute the plugin code with filename for better stack traces
            let mut eval_options = rquickjs::context::EvalOptions::default();
            eval_options.global = true;
            eval_options.filename = Some(source_name.to_string());
            let result = ctx
                .eval_with_options::<(), _>(wrapped.as_bytes(), eval_options)
                .map_err(|e| format_js_error(&ctx, e, source_name));

            tracing::debug!(
                "execute_js: plugin code execution finished for '{}', result: {:?}",
                plugin_name,
                result.is_ok()
            );

            result
        })
    }

    /// Emit an event to all registered handlers
    pub async fn emit(&mut self, event_name: &str, event_data: &serde_json::Value) -> Result<bool> {
        let _event_data_str = event_data.to_string();
        tracing::debug!("emit: event '{}' with data: {:?}", event_name, event_data);

        // Track execution state for signal handler debugging
        self.services
            .set_js_execution_state(format!("hook '{}'", event_name));

        let handlers = self.event_handlers.borrow().get(event_name).cloned();

        if let Some(handler_pairs) = handlers {
            if handler_pairs.is_empty() {
                self.services.clear_js_execution_state();
                return Ok(true);
            }

            let plugin_contexts = self.plugin_contexts.borrow();
            for handler in handler_pairs {
                let context_opt = plugin_contexts.get(&handler.plugin_name);
                if let Some(context) = context_opt {
                    let handler_name = &handler.handler_name;
                    // Call the handler and properly handle both sync and async errors
                    // Async handlers return Promises - we attach .catch() to surface rejections
                    // Double-encode the JSON to produce a valid JavaScript string literal:
                    // event_data = {"path": "/test"} -> first to_string = {"path": "/test"}
                    // -> second to_string = "{\"path\": \"/test\"}" (properly quoted for JS)
                    let json_string = serde_json::to_string(event_data)?;
                    let js_string_literal = serde_json::to_string(&json_string)?;
                    let code = format!(
                        r#"
                        (function() {{
                            try {{
                                const data = JSON.parse({});
                                if (typeof globalThis["{}"] === 'function') {{
                                    const result = globalThis["{}"](data);
                                    // If handler returns a Promise, catch rejections
                                    if (result && typeof result.then === 'function') {{
                                        result.catch(function(e) {{
                                            console.error('Handler {} async error:', e);
                                            // Re-throw to make it an unhandled rejection for the runtime to catch
                                            throw e;
                                        }});
                                    }}
                                }}
                            }} catch (e) {{
                                console.error('Handler {} sync error:', e);
                                throw e;
                            }}
                        }})();
                        "#,
                        js_string_literal, handler_name, handler_name, handler_name, handler_name
                    );

                    context.with(|ctx| {
                        if let Err(e) = ctx.eval::<(), _>(code.as_bytes()) {
                            log_js_error(&ctx, e, &format!("handler {}", handler_name));
                        }
                        // Run pending jobs to process any Promise continuations and catch errors
                        run_pending_jobs_checked(&ctx, &format!("emit handler {}", handler_name));
                    });
                }
            }
        }

        self.services.clear_js_execution_state();
        Ok(true)
    }

    /// Check if any handlers are registered for an event
    pub fn has_handlers(&self, event_name: &str) -> bool {
        self.event_handlers
            .borrow()
            .get(event_name)
            .map(|v| !v.is_empty())
            .unwrap_or(false)
    }

    /// Start an action without waiting for async operations to complete.
    /// This is useful when the calling thread needs to continue processing
    /// ResolveCallback requests that the action may be waiting for.
    pub fn start_action(&mut self, action_name: &str) -> Result<()> {
        let pair = self.registered_actions.borrow().get(action_name).cloned();
        let (plugin_name, function_name) = match pair {
            Some(handler) => (handler.plugin_name, handler.handler_name),
            None => ("main".to_string(), action_name.to_string()),
        };

        let plugin_contexts = self.plugin_contexts.borrow();
        let context = plugin_contexts
            .get(&plugin_name)
            .unwrap_or(&self.main_context);

        // Track execution state for signal handler debugging
        self.services
            .set_js_execution_state(format!("action '{}' (fn: {})", action_name, function_name));

        tracing::info!(
            "start_action: BEGIN '{}' -> function '{}'",
            action_name,
            function_name
        );

        // Just call the function - don't try to await or drive Promises
        let code = format!(
            r#"
            (function() {{
                console.log('[JS] start_action: calling {fn}');
                try {{
                    if (typeof globalThis.{fn} === 'function') {{
                        console.log('[JS] start_action: {fn} is a function, invoking...');
                        globalThis.{fn}();
                        console.log('[JS] start_action: {fn} invoked (may be async)');
                    }} else {{
                        console.error('[JS] Action {action} is not defined as a global function');
                    }}
                }} catch (e) {{
                    console.error('[JS] Action {action} error:', e);
                }}
            }})();
            "#,
            fn = function_name,
            action = action_name
        );

        tracing::info!("start_action: evaluating JS code");
        context.with(|ctx| {
            if let Err(e) = ctx.eval::<rquickjs::Value, _>(code.as_bytes()) {
                log_js_error(&ctx, e, &format!("action {}", action_name));
            }
            tracing::info!("start_action: running pending microtasks");
            // Run any immediate microtasks
            let count = run_pending_jobs_checked(&ctx, &format!("start_action {}", action_name));
            tracing::info!("start_action: executed {} pending jobs", count);
        });

        tracing::info!("start_action: END '{}'", action_name);

        // Clear execution state (action started, may still be running async)
        self.services.clear_js_execution_state();

        Ok(())
    }

    /// Execute a registered action by name
    pub async fn execute_action(&mut self, action_name: &str) -> Result<()> {
        // First check if there's a registered command mapping
        let pair = self.registered_actions.borrow().get(action_name).cloned();
        let (plugin_name, function_name) = match pair {
            Some(handler) => (handler.plugin_name, handler.handler_name),
            None => ("main".to_string(), action_name.to_string()),
        };

        let plugin_contexts = self.plugin_contexts.borrow();
        let context = plugin_contexts
            .get(&plugin_name)
            .unwrap_or(&self.main_context);

        tracing::debug!(
            "execute_action: '{}' -> function '{}'",
            action_name,
            function_name
        );

        // Call the function and await if it returns a Promise
        // We use a global _executeActionResult to pass the result back
        let code = format!(
            r#"
            (async function() {{
                try {{
                    if (typeof globalThis.{fn} === 'function') {{
                        const result = globalThis.{fn}();
                        // If it's a Promise, await it
                        if (result && typeof result.then === 'function') {{
                            await result;
                        }}
                    }} else {{
                        console.error('Action {action} is not defined as a global function');
                    }}
                }} catch (e) {{
                    console.error('Action {action} error:', e);
                }}
            }})();
            "#,
            fn = function_name,
            action = action_name
        );

        context.with(|ctx| {
            // Eval returns a Promise for the async IIFE, which we need to drive
            match ctx.eval::<rquickjs::Value, _>(code.as_bytes()) {
                Ok(value) => {
                    // If it's a Promise, we need to drive the runtime to completion
                    if value.is_object() {
                        if let Some(obj) = value.as_object() {
                            // Check if it's a Promise by looking for 'then' method
                            if obj.get::<_, rquickjs::Function>("then").is_ok() {
                                // Drive the runtime to process the promise
                                // QuickJS processes promises synchronously when we call execute_pending_job
                                run_pending_jobs_checked(
                                    &ctx,
                                    &format!("execute_action {} promise", action_name),
                                );
                            }
                        }
                    }
                }
                Err(e) => {
                    log_js_error(&ctx, e, &format!("action {}", action_name));
                }
            }
        });

        Ok(())
    }

    /// Poll the event loop once to run any pending microtasks
    pub fn poll_event_loop_once(&mut self) -> bool {
        let mut had_work = false;

        // Poll main context
        self.main_context.with(|ctx| {
            let count = run_pending_jobs_checked(&ctx, "poll_event_loop main");
            if count > 0 {
                had_work = true;
            }
        });

        // Poll all plugin contexts
        let contexts = self.plugin_contexts.borrow().clone();
        for (name, context) in contexts {
            context.with(|ctx| {
                let count = run_pending_jobs_checked(&ctx, &format!("poll_event_loop {}", name));
                if count > 0 {
                    had_work = true;
                }
            });
        }
        had_work
    }

    /// Send a status message to the editor
    pub fn send_status(&self, message: String) {
        let _ = self
            .command_sender
            .send(PluginCommand::SetStatus { message });
    }

    /// Resolve a pending async callback with a result (called from Rust when async op completes)
    ///
    /// Takes a JSON string which is parsed and converted to a proper JS value.
    /// This avoids string interpolation with eval for better type safety.
    pub fn resolve_callback(
        &mut self,
        callback_id: fresh_core::api::JsCallbackId,
        result_json: &str,
    ) {
        let id = callback_id.as_u64();
        tracing::debug!("resolve_callback: starting for callback_id={}", id);

        // Find the plugin name and then context for this callback
        let plugin_name = {
            let mut contexts = self.callback_contexts.borrow_mut();
            contexts.remove(&id)
        };

        let Some(name) = plugin_name else {
            tracing::warn!("resolve_callback: No plugin found for callback_id={}", id);
            return;
        };

        let plugin_contexts = self.plugin_contexts.borrow();
        let Some(context) = plugin_contexts.get(&name) else {
            tracing::warn!("resolve_callback: Context lost for plugin {}", name);
            return;
        };

        context.with(|ctx| {
            // Parse JSON string to serde_json::Value
            let json_value: serde_json::Value = match serde_json::from_str(result_json) {
                Ok(v) => v,
                Err(e) => {
                    tracing::error!(
                        "resolve_callback: failed to parse JSON for callback_id={}: {}",
                        id,
                        e
                    );
                    return;
                }
            };

            // Convert to JS value using rquickjs_serde
            let js_value = match rquickjs_serde::to_value(ctx.clone(), &json_value) {
                Ok(v) => v,
                Err(e) => {
                    tracing::error!(
                        "resolve_callback: failed to convert to JS value for callback_id={}: {}",
                        id,
                        e
                    );
                    return;
                }
            };

            // Get _resolveCallback function from globalThis
            let globals = ctx.globals();
            let resolve_fn: rquickjs::Function = match globals.get("_resolveCallback") {
                Ok(f) => f,
                Err(e) => {
                    tracing::error!(
                        "resolve_callback: _resolveCallback not found for callback_id={}: {:?}",
                        id,
                        e
                    );
                    return;
                }
            };

            // Call the function with callback_id (as u64) and the JS value
            if let Err(e) = resolve_fn.call::<_, ()>((id, js_value)) {
                log_js_error(&ctx, e, &format!("resolving callback {}", id));
            }

            // IMPORTANT: Run pending jobs to process Promise continuations
            let job_count = run_pending_jobs_checked(&ctx, &format!("resolve_callback {}", id));
            tracing::info!(
                "resolve_callback: executed {} pending jobs for callback_id={}",
                job_count,
                id
            );
        });
    }

    /// Reject a pending async callback with an error (called from Rust when async op fails)
    pub fn reject_callback(&mut self, callback_id: fresh_core::api::JsCallbackId, error: &str) {
        let id = callback_id.as_u64();

        // Find the plugin name and then context for this callback
        let plugin_name = {
            let mut contexts = self.callback_contexts.borrow_mut();
            contexts.remove(&id)
        };

        let Some(name) = plugin_name else {
            tracing::warn!("reject_callback: No plugin found for callback_id={}", id);
            return;
        };

        let plugin_contexts = self.plugin_contexts.borrow();
        let Some(context) = plugin_contexts.get(&name) else {
            tracing::warn!("reject_callback: Context lost for plugin {}", name);
            return;
        };

        context.with(|ctx| {
            // Get _rejectCallback function from globalThis
            let globals = ctx.globals();
            let reject_fn: rquickjs::Function = match globals.get("_rejectCallback") {
                Ok(f) => f,
                Err(e) => {
                    tracing::error!(
                        "reject_callback: _rejectCallback not found for callback_id={}: {:?}",
                        id,
                        e
                    );
                    return;
                }
            };

            // Call the function with callback_id (as u64) and error string
            if let Err(e) = reject_fn.call::<_, ()>((id, error)) {
                log_js_error(&ctx, e, &format!("rejecting callback {}", id));
            }

            // IMPORTANT: Run pending jobs to process Promise continuations
            run_pending_jobs_checked(&ctx, &format!("reject_callback {}", id));
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_core::api::{BufferInfo, CursorInfo};
    use std::sync::mpsc;

    /// Helper to create a backend with a command receiver for testing
    fn create_test_backend() -> (QuickJsBackend, mpsc::Receiver<PluginCommand>) {
        let (tx, rx) = mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let services = Arc::new(TestServiceBridge::new());
        let backend = QuickJsBackend::with_state(state_snapshot, tx, services).unwrap();
        (backend, rx)
    }

    struct TestServiceBridge {
        en_strings: std::sync::Mutex<HashMap<String, String>>,
    }

    impl TestServiceBridge {
        fn new() -> Self {
            Self {
                en_strings: std::sync::Mutex::new(HashMap::new()),
            }
        }
    }

    impl fresh_core::services::PluginServiceBridge for TestServiceBridge {
        fn as_any(&self) -> &dyn std::any::Any {
            self
        }
        fn translate(
            &self,
            _plugin_name: &str,
            key: &str,
            _args: &HashMap<String, String>,
        ) -> String {
            self.en_strings
                .lock()
                .unwrap()
                .get(key)
                .cloned()
                .unwrap_or_else(|| key.to_string())
        }
        fn current_locale(&self) -> String {
            "en".to_string()
        }
        fn set_js_execution_state(&self, _state: String) {}
        fn clear_js_execution_state(&self) {}
        fn get_theme_schema(&self) -> serde_json::Value {
            serde_json::json!({})
        }
        fn get_builtin_themes(&self) -> serde_json::Value {
            serde_json::json!([])
        }
        fn register_command(&self, _command: fresh_core::command::Command) {}
        fn unregister_command(&self, _name: &str) {}
        fn unregister_commands_by_prefix(&self, _prefix: &str) {}
        fn plugins_dir(&self) -> std::path::PathBuf {
            std::path::PathBuf::from("/tmp/plugins")
        }
        fn config_dir(&self) -> std::path::PathBuf {
            std::path::PathBuf::from("/tmp/config")
        }
    }

    #[test]
    fn test_quickjs_backend_creation() {
        let backend = QuickJsBackend::new();
        assert!(backend.is_ok());
    }

    #[test]
    fn test_execute_simple_js() {
        let mut backend = QuickJsBackend::new().unwrap();
        let result = backend.execute_js("const x = 1 + 2;", "test.js");
        assert!(result.is_ok());
    }

    #[test]
    fn test_event_handler_registration() {
        let backend = QuickJsBackend::new().unwrap();

        // Initially no handlers
        assert!(!backend.has_handlers("test_event"));

        // Register a handler
        backend
            .event_handlers
            .borrow_mut()
            .entry("test_event".to_string())
            .or_default()
            .push(PluginHandler {
                plugin_name: "test".to_string(),
                handler_name: "testHandler".to_string(),
            });

        // Now has handlers
        assert!(backend.has_handlers("test_event"));
    }

    // ==================== API Tests ====================

    #[test]
    fn test_api_set_status() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.setStatus("Hello from test");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetStatus { message } => {
                assert_eq!(message, "Hello from test");
            }
            _ => panic!("Expected SetStatus command, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_register_command() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis.myTestHandler = function() { };
            editor.registerCommand("Test Command", "A test command", "myTestHandler", null);
        "#,
                "test_plugin.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::RegisterCommand { command } => {
                assert_eq!(command.name, "Test Command");
                assert_eq!(command.description, "A test command");
                // Check that plugin_name contains the plugin name (derived from filename)
                assert_eq!(command.plugin_name, "test_plugin");
            }
            _ => panic!("Expected RegisterCommand, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_define_mode() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.defineMode("test-mode", null, [
                ["a", "action_a"],
                ["b", "action_b"]
            ]);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::DefineMode {
                name,
                parent,
                bindings,
                read_only,
            } => {
                assert_eq!(name, "test-mode");
                assert!(parent.is_none());
                assert_eq!(bindings.len(), 2);
                assert_eq!(bindings[0], ("a".to_string(), "action_a".to_string()));
                assert_eq!(bindings[1], ("b".to_string(), "action_b".to_string()));
                assert!(!read_only);
            }
            _ => panic!("Expected DefineMode, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_set_editor_mode() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.setEditorMode("vi-normal");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetEditorMode { mode } => {
                assert_eq!(mode, Some("vi-normal".to_string()));
            }
            _ => panic!("Expected SetEditorMode, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_clear_editor_mode() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.setEditorMode(null);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetEditorMode { mode } => {
                assert!(mode.is_none());
            }
            _ => panic!("Expected SetEditorMode with None, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_insert_at_cursor() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.insertAtCursor("Hello, World!");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::InsertAtCursor { text } => {
                assert_eq!(text, "Hello, World!");
            }
            _ => panic!("Expected InsertAtCursor, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_set_context() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.setContext("myContext", true);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetContext { name, active } => {
                assert_eq!(name, "myContext");
                assert!(active);
            }
            _ => panic!("Expected SetContext, got {:?}", cmd),
        }
    }

    #[tokio::test]
    async fn test_execute_action_sync_function() {
        let (mut backend, rx) = create_test_backend();

        // Register the action explicitly so it knows to look in "test" plugin
        backend.registered_actions.borrow_mut().insert(
            "my_sync_action".to_string(),
            PluginHandler {
                plugin_name: "test".to_string(),
                handler_name: "my_sync_action".to_string(),
            },
        );

        // Define a sync function and register it
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis.my_sync_action = function() {
                editor.setStatus("sync action executed");
            };
        "#,
                "test.js",
            )
            .unwrap();

        // Drain any setup commands
        while rx.try_recv().is_ok() {}

        // Execute the action
        backend.execute_action("my_sync_action").await.unwrap();

        // Check the command was sent
        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetStatus { message } => {
                assert_eq!(message, "sync action executed");
            }
            _ => panic!("Expected SetStatus from action, got {:?}", cmd),
        }
    }

    #[tokio::test]
    async fn test_execute_action_async_function() {
        let (mut backend, rx) = create_test_backend();

        // Register the action explicitly
        backend.registered_actions.borrow_mut().insert(
            "my_async_action".to_string(),
            PluginHandler {
                plugin_name: "test".to_string(),
                handler_name: "my_async_action".to_string(),
            },
        );

        // Define an async function
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis.my_async_action = async function() {
                await Promise.resolve();
                editor.setStatus("async action executed");
            };
        "#,
                "test.js",
            )
            .unwrap();

        // Drain any setup commands
        while rx.try_recv().is_ok() {}

        // Execute the action
        backend.execute_action("my_async_action").await.unwrap();

        // Check the command was sent (async should complete)
        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetStatus { message } => {
                assert_eq!(message, "async action executed");
            }
            _ => panic!("Expected SetStatus from async action, got {:?}", cmd),
        }
    }

    #[tokio::test]
    async fn test_execute_action_with_registered_handler() {
        let (mut backend, rx) = create_test_backend();

        // Register an action with a different handler name
        backend.registered_actions.borrow_mut().insert(
            "my_action".to_string(),
            PluginHandler {
                plugin_name: "test".to_string(),
                handler_name: "actual_handler_function".to_string(),
            },
        );

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis.actual_handler_function = function() {
                editor.setStatus("handler executed");
            };
        "#,
                "test.js",
            )
            .unwrap();

        // Drain any setup commands
        while rx.try_recv().is_ok() {}

        // Execute the action by name (should resolve to handler)
        backend.execute_action("my_action").await.unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetStatus { message } => {
                assert_eq!(message, "handler executed");
            }
            _ => panic!("Expected SetStatus, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_on_event_registration() {
        let (mut backend, _rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis.myEventHandler = function() { };
            editor.on("bufferSave", "myEventHandler");
        "#,
                "test.js",
            )
            .unwrap();

        assert!(backend.has_handlers("bufferSave"));
    }

    #[test]
    fn test_api_off_event_unregistration() {
        let (mut backend, _rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis.myEventHandler = function() { };
            editor.on("bufferSave", "myEventHandler");
            editor.off("bufferSave", "myEventHandler");
        "#,
                "test.js",
            )
            .unwrap();

        // Handler should be removed
        assert!(!backend.has_handlers("bufferSave"));
    }

    #[tokio::test]
    async fn test_emit_event() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis.onSaveHandler = function(data) {
                editor.setStatus("saved: " + JSON.stringify(data));
            };
            editor.on("bufferSave", "onSaveHandler");
        "#,
                "test.js",
            )
            .unwrap();

        // Drain setup commands
        while rx.try_recv().is_ok() {}

        // Emit the event
        let event_data: serde_json::Value = serde_json::json!({"path": "/test.txt"});
        backend.emit("bufferSave", &event_data).await.unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetStatus { message } => {
                assert!(message.contains("/test.txt"));
            }
            _ => panic!("Expected SetStatus from event handler, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_copy_to_clipboard() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.copyToClipboard("clipboard text");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetClipboard { text } => {
                assert_eq!(text, "clipboard text");
            }
            _ => panic!("Expected SetClipboard, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_open_file() {
        let (mut backend, rx) = create_test_backend();

        // openFile takes (path, line?, column?)
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.openFile("/path/to/file.txt", null, null);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::OpenFileAtLocation { path, line, column } => {
                assert_eq!(path.to_str().unwrap(), "/path/to/file.txt");
                assert!(line.is_none());
                assert!(column.is_none());
            }
            _ => panic!("Expected OpenFileAtLocation, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_delete_range() {
        let (mut backend, rx) = create_test_backend();

        // deleteRange takes (buffer_id, start, end)
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.deleteRange(0, 10, 20);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::DeleteRange { range, .. } => {
                assert_eq!(range.start, 10);
                assert_eq!(range.end, 20);
            }
            _ => panic!("Expected DeleteRange, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_insert_text() {
        let (mut backend, rx) = create_test_backend();

        // insertText takes (buffer_id, position, text)
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.insertText(0, 5, "inserted");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::InsertText { position, text, .. } => {
                assert_eq!(position, 5);
                assert_eq!(text, "inserted");
            }
            _ => panic!("Expected InsertText, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_set_buffer_cursor() {
        let (mut backend, rx) = create_test_backend();

        // setBufferCursor takes (buffer_id, position)
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.setBufferCursor(0, 100);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetBufferCursor { position, .. } => {
                assert_eq!(position, 100);
            }
            _ => panic!("Expected SetBufferCursor, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_get_cursor_position_from_state() {
        let (tx, _rx) = mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Set up cursor position in state
        {
            let mut state = state_snapshot.write().unwrap();
            state.primary_cursor = Some(CursorInfo {
                position: 42,
                selection: None,
            });
        }

        let services = Arc::new(fresh_core::services::NoopServiceBridge);
        let mut backend = QuickJsBackend::with_state(state_snapshot, tx, services).unwrap();

        // Execute JS that reads and stores cursor position
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            const pos = editor.getCursorPosition();
            globalThis._testResult = pos;
        "#,
                "test.js",
            )
            .unwrap();

        // Verify by reading back - getCursorPosition returns byte offset as u32
        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let result: u32 = global.get("_testResult").unwrap();
                assert_eq!(result, 42);
            });
    }

    #[test]
    fn test_api_path_functions() {
        let (mut backend, _rx) = create_test_backend();

        // Use platform-appropriate absolute path for isAbsolute test
        // Note: On Windows, backslashes need to be escaped for JavaScript string literals
        #[cfg(windows)]
        let absolute_path = r#"C:\\foo\\bar"#;
        #[cfg(not(windows))]
        let absolute_path = "/foo/bar";

        // pathJoin takes an array of path parts
        let js_code = format!(
            r#"
            const editor = getEditor();
            globalThis._dirname = editor.pathDirname("/foo/bar/baz.txt");
            globalThis._basename = editor.pathBasename("/foo/bar/baz.txt");
            globalThis._extname = editor.pathExtname("/foo/bar/baz.txt");
            globalThis._isAbsolute = editor.pathIsAbsolute("{}");
            globalThis._isRelative = editor.pathIsAbsolute("foo/bar");
            globalThis._joined = editor.pathJoin("/foo", "bar", "baz");
        "#,
            absolute_path
        );
        backend.execute_js(&js_code, "test.js").unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                assert_eq!(global.get::<_, String>("_dirname").unwrap(), "/foo/bar");
                assert_eq!(global.get::<_, String>("_basename").unwrap(), "baz.txt");
                assert_eq!(global.get::<_, String>("_extname").unwrap(), ".txt");
                assert!(global.get::<_, bool>("_isAbsolute").unwrap());
                assert!(!global.get::<_, bool>("_isRelative").unwrap());
                assert_eq!(global.get::<_, String>("_joined").unwrap(), "/foo/bar/baz");
            });
    }

    #[test]
    fn test_typescript_transpilation() {
        use fresh_parser_js::transpile_typescript;

        let (mut backend, rx) = create_test_backend();

        // TypeScript code with type annotations
        let ts_code = r#"
            const editor = getEditor();
            function greet(name: string): string {
                return "Hello, " + name;
            }
            editor.setStatus(greet("TypeScript"));
        "#;

        // Transpile to JavaScript first
        let js_code = transpile_typescript(ts_code, "test.ts").unwrap();

        // Execute the transpiled JavaScript
        backend.execute_js(&js_code, "test.js").unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetStatus { message } => {
                assert_eq!(message, "Hello, TypeScript");
            }
            _ => panic!("Expected SetStatus, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_get_buffer_text_sends_command() {
        let (mut backend, rx) = create_test_backend();

        // Call getBufferText - this returns a Promise and sends the command
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            // Store the promise for later
            globalThis._textPromise = editor.getBufferText(0, 10, 20);
        "#,
                "test.js",
            )
            .unwrap();

        // Verify the GetBufferText command was sent
        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::GetBufferText {
                buffer_id,
                start,
                end,
                request_id,
            } => {
                assert_eq!(buffer_id.0, 0);
                assert_eq!(start, 10);
                assert_eq!(end, 20);
                assert!(request_id > 0); // Should have a valid request ID
            }
            _ => panic!("Expected GetBufferText, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_get_buffer_text_resolves_callback() {
        let (mut backend, rx) = create_test_backend();

        // Call getBufferText and set up a handler for when it resolves
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis._resolvedText = null;
            editor.getBufferText(0, 0, 100).then(text => {
                globalThis._resolvedText = text;
            });
        "#,
                "test.js",
            )
            .unwrap();

        // Get the request_id from the command
        let request_id = match rx.try_recv().unwrap() {
            PluginCommand::GetBufferText { request_id, .. } => request_id,
            cmd => panic!("Expected GetBufferText, got {:?}", cmd),
        };

        // Simulate the editor responding with the text
        backend.resolve_callback(JsCallbackId::from(request_id), "\"hello world\"");

        // Drive the Promise to completion
        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                run_pending_jobs_checked(&ctx, "test async getText");
            });

        // Verify the Promise resolved with the text
        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let result: String = global.get("_resolvedText").unwrap();
                assert_eq!(result, "hello world");
            });
    }

    #[test]
    fn test_plugin_translation() {
        let (mut backend, _rx) = create_test_backend();

        // The t() function should work (returns key if translation not found)
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis._translated = editor.t("test.key");
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                // Without actual translations, it returns the key
                let result: String = global.get("_translated").unwrap();
                assert_eq!(result, "test.key");
            });
    }

    #[test]
    fn test_plugin_translation_with_registered_strings() {
        let (mut backend, _rx) = create_test_backend();

        // Register translations for the test plugin
        let mut en_strings = std::collections::HashMap::new();
        en_strings.insert("greeting".to_string(), "Hello, World!".to_string());
        en_strings.insert("prompt.find_file".to_string(), "Find file: ".to_string());

        let mut strings = std::collections::HashMap::new();
        strings.insert("en".to_string(), en_strings);

        // Register for "test" plugin
        if let Some(bridge) = backend
            .services
            .as_any()
            .downcast_ref::<TestServiceBridge>()
        {
            let mut en = bridge.en_strings.lock().unwrap();
            en.insert("greeting".to_string(), "Hello, World!".to_string());
            en.insert("prompt.find_file".to_string(), "Find file: ".to_string());
        }

        // Test translation
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis._greeting = editor.t("greeting");
            globalThis._prompt = editor.t("prompt.find_file");
            globalThis._missing = editor.t("nonexistent.key");
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let greeting: String = global.get("_greeting").unwrap();
                assert_eq!(greeting, "Hello, World!");

                let prompt: String = global.get("_prompt").unwrap();
                assert_eq!(prompt, "Find file: ");

                // Missing key should return the key itself
                let missing: String = global.get("_missing").unwrap();
                assert_eq!(missing, "nonexistent.key");
            });
    }

    // ==================== Line Indicator Tests ====================

    #[test]
    fn test_api_set_line_indicator() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.setLineIndicator(1, 5, "test-ns", "●", 255, 0, 0, 10);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetLineIndicator {
                buffer_id,
                line,
                namespace,
                symbol,
                color,
                priority,
            } => {
                assert_eq!(buffer_id.0, 1);
                assert_eq!(line, 5);
                assert_eq!(namespace, "test-ns");
                assert_eq!(symbol, "●");
                assert_eq!(color, (255, 0, 0));
                assert_eq!(priority, 10);
            }
            _ => panic!("Expected SetLineIndicator, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_clear_line_indicators() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.clearLineIndicators(1, "test-ns");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::ClearLineIndicators {
                buffer_id,
                namespace,
            } => {
                assert_eq!(buffer_id.0, 1);
                assert_eq!(namespace, "test-ns");
            }
            _ => panic!("Expected ClearLineIndicators, got {:?}", cmd),
        }
    }

    // ==================== Virtual Buffer Tests ====================

    #[test]
    fn test_api_create_virtual_buffer_sends_command() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.createVirtualBuffer({
                name: "*Test Buffer*",
                mode: "test-mode",
                readOnly: true,
                entries: [
                    { text: "Line 1\n", properties: { type: "header" } },
                    { text: "Line 2\n", properties: { type: "content" } }
                ],
                showLineNumbers: false,
                showCursors: true,
                editingDisabled: true
            });
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::CreateVirtualBufferWithContent {
                name,
                mode,
                read_only,
                entries,
                show_line_numbers,
                show_cursors,
                editing_disabled,
                ..
            } => {
                assert_eq!(name, "*Test Buffer*");
                assert_eq!(mode, "test-mode");
                assert!(read_only);
                assert_eq!(entries.len(), 2);
                assert_eq!(entries[0].text, "Line 1\n");
                assert!(!show_line_numbers);
                assert!(show_cursors);
                assert!(editing_disabled);
            }
            _ => panic!("Expected CreateVirtualBufferWithContent, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_set_virtual_buffer_content() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.setVirtualBufferContent(5, [
                { text: "New content\n", properties: { type: "updated" } }
            ]);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetVirtualBufferContent { buffer_id, entries } => {
                assert_eq!(buffer_id.0, 5);
                assert_eq!(entries.len(), 1);
                assert_eq!(entries[0].text, "New content\n");
            }
            _ => panic!("Expected SetVirtualBufferContent, got {:?}", cmd),
        }
    }

    // ==================== Overlay Tests ====================

    #[test]
    fn test_api_add_overlay() {
        let (mut backend, rx) = create_test_backend();

        backend.execute_js(r#"
            const editor = getEditor();
            editor.addOverlay(1, "highlight", 10, 20, 255, 128, 0, false, true, false, 50, 50, 50, false);
        "#, "test.js").unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::AddOverlay {
                buffer_id,
                namespace,
                range,
                color,
                bg_color,
                underline,
                bold,
                italic,
                extend_to_line_end,
            } => {
                assert_eq!(buffer_id.0, 1);
                assert!(namespace.is_some());
                assert_eq!(namespace.unwrap().as_str(), "highlight");
                assert_eq!(range, 10..20);
                assert_eq!(color, (255, 128, 0));
                assert_eq!(bg_color, Some((50, 50, 50)));
                assert!(!underline);
                assert!(bold);
                assert!(!italic);
                assert!(!extend_to_line_end);
            }
            _ => panic!("Expected AddOverlay, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_clear_namespace() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.clearNamespace(1, "highlight");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::ClearNamespace {
                buffer_id,
                namespace,
            } => {
                assert_eq!(buffer_id.0, 1);
                assert_eq!(namespace.as_str(), "highlight");
            }
            _ => panic!("Expected ClearNamespace, got {:?}", cmd),
        }
    }

    // ==================== Theme Tests ====================

    #[test]
    fn test_api_get_theme_schema() {
        let (mut backend, _rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            const schema = editor.getThemeSchema();
            globalThis._isObject = typeof schema === 'object' && schema !== null;
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let is_object: bool = global.get("_isObject").unwrap();
                // getThemeSchema should return an object
                assert!(is_object);
            });
    }

    #[test]
    fn test_api_get_builtin_themes() {
        let (mut backend, _rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            const themes = editor.getBuiltinThemes();
            globalThis._isObject = typeof themes === 'object' && themes !== null;
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let is_object: bool = global.get("_isObject").unwrap();
                // getBuiltinThemes should return an object
                assert!(is_object);
            });
    }

    #[test]
    fn test_api_apply_theme() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.applyTheme("dark");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::ApplyTheme { theme_name } => {
                assert_eq!(theme_name, "dark");
            }
            _ => panic!("Expected ApplyTheme, got {:?}", cmd),
        }
    }

    // ==================== Buffer Operations Tests ====================

    #[test]
    fn test_api_close_buffer() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.closeBuffer(3);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::CloseBuffer { buffer_id } => {
                assert_eq!(buffer_id.0, 3);
            }
            _ => panic!("Expected CloseBuffer, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_focus_split() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.focusSplit(2);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::FocusSplit { split_id } => {
                assert_eq!(split_id.0, 2);
            }
            _ => panic!("Expected FocusSplit, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_list_buffers() {
        let (tx, _rx) = mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Add some buffers to state
        {
            let mut state = state_snapshot.write().unwrap();
            state.buffers.insert(
                BufferId(0),
                BufferInfo {
                    id: BufferId(0),
                    path: Some(PathBuf::from("/test1.txt")),
                    modified: false,
                    length: 100,
                },
            );
            state.buffers.insert(
                BufferId(1),
                BufferInfo {
                    id: BufferId(1),
                    path: Some(PathBuf::from("/test2.txt")),
                    modified: true,
                    length: 200,
                },
            );
        }

        let services = Arc::new(fresh_core::services::NoopServiceBridge);
        let mut backend = QuickJsBackend::with_state(state_snapshot, tx, services).unwrap();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            const buffers = editor.listBuffers();
            globalThis._isArray = Array.isArray(buffers);
            globalThis._length = buffers.length;
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let is_array: bool = global.get("_isArray").unwrap();
                let length: u32 = global.get("_length").unwrap();
                assert!(is_array);
                assert_eq!(length, 2);
            });
    }

    // ==================== Prompt Tests ====================

    #[test]
    fn test_api_start_prompt() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.startPrompt("Enter value:", "test-prompt");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::StartPrompt { label, prompt_type } => {
                assert_eq!(label, "Enter value:");
                assert_eq!(prompt_type, "test-prompt");
            }
            _ => panic!("Expected StartPrompt, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_start_prompt_with_initial() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.startPromptWithInitial("Enter value:", "test-prompt", "default");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::StartPromptWithInitial {
                label,
                prompt_type,
                initial_value,
            } => {
                assert_eq!(label, "Enter value:");
                assert_eq!(prompt_type, "test-prompt");
                assert_eq!(initial_value, "default");
            }
            _ => panic!("Expected StartPromptWithInitial, got {:?}", cmd),
        }
    }

    #[test]
    fn test_api_set_prompt_suggestions() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.setPromptSuggestions([
                { text: "Option 1", value: "opt1" },
                { text: "Option 2", value: "opt2" }
            ]);
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::SetPromptSuggestions { suggestions } => {
                assert_eq!(suggestions.len(), 2);
                assert_eq!(suggestions[0].text, "Option 1");
                assert_eq!(suggestions[0].value, Some("opt1".to_string()));
            }
            _ => panic!("Expected SetPromptSuggestions, got {:?}", cmd),
        }
    }

    // ==================== State Query Tests ====================

    #[test]
    fn test_api_get_active_buffer_id() {
        let (tx, _rx) = mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        {
            let mut state = state_snapshot.write().unwrap();
            state.active_buffer_id = BufferId(42);
        }

        let services = Arc::new(fresh_core::services::NoopServiceBridge);
        let mut backend = QuickJsBackend::with_state(state_snapshot, tx, services).unwrap();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis._activeId = editor.getActiveBufferId();
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let result: u32 = global.get("_activeId").unwrap();
                assert_eq!(result, 42);
            });
    }

    #[test]
    fn test_api_get_active_split_id() {
        let (tx, _rx) = mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        {
            let mut state = state_snapshot.write().unwrap();
            state.active_split_id = 7;
        }

        let services = Arc::new(fresh_core::services::NoopServiceBridge);
        let mut backend = QuickJsBackend::with_state(state_snapshot, tx, services).unwrap();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis._splitId = editor.getActiveSplitId();
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let result: u32 = global.get("_splitId").unwrap();
                assert_eq!(result, 7);
            });
    }

    // ==================== File System Tests ====================

    #[test]
    fn test_api_file_exists() {
        let (mut backend, _rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            // Test with a path that definitely exists
            globalThis._exists = editor.fileExists("/");
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let result: bool = global.get("_exists").unwrap();
                assert!(result);
            });
    }

    #[test]
    fn test_api_get_cwd() {
        let (mut backend, _rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis._cwd = editor.getCwd();
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let result: String = global.get("_cwd").unwrap();
                // Should return some path
                assert!(!result.is_empty());
            });
    }

    #[test]
    fn test_api_get_env() {
        let (mut backend, _rx) = create_test_backend();

        // Set a test environment variable
        std::env::set_var("TEST_PLUGIN_VAR", "test_value");

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis._envVal = editor.getEnv("TEST_PLUGIN_VAR");
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let result: Option<String> = global.get("_envVal").unwrap();
                assert_eq!(result, Some("test_value".to_string()));
            });

        std::env::remove_var("TEST_PLUGIN_VAR");
    }

    #[test]
    fn test_api_get_config() {
        let (mut backend, _rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            const config = editor.getConfig();
            globalThis._isObject = typeof config === 'object';
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let is_object: bool = global.get("_isObject").unwrap();
                // getConfig should return an object, not a string
                assert!(is_object);
            });
    }

    #[test]
    fn test_api_get_themes_dir() {
        let (mut backend, _rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            globalThis._themesDir = editor.getThemesDir();
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let result: String = global.get("_themesDir").unwrap();
                // Should return some path
                assert!(!result.is_empty());
            });
    }

    // ==================== Read Dir Test ====================

    #[test]
    fn test_api_read_dir() {
        let (mut backend, _rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            const entries = editor.readDir("/tmp");
            globalThis._isArray = Array.isArray(entries);
            globalThis._length = entries.length;
        "#,
                "test.js",
            )
            .unwrap();

        backend
            .plugin_contexts
            .borrow()
            .get("test")
            .unwrap()
            .clone()
            .with(|ctx| {
                let global = ctx.globals();
                let is_array: bool = global.get("_isArray").unwrap();
                let length: u32 = global.get("_length").unwrap();
                // /tmp should exist and readDir should return an array
                assert!(is_array);
                // Length is valid (u32 always >= 0)
                let _ = length;
            });
    }

    // ==================== Execute Action Test ====================

    #[test]
    fn test_api_execute_action() {
        let (mut backend, rx) = create_test_backend();

        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.executeAction("move_cursor_up");
        "#,
                "test.js",
            )
            .unwrap();

        let cmd = rx.try_recv().unwrap();
        match cmd {
            PluginCommand::ExecuteAction { action_name } => {
                assert_eq!(action_name, "move_cursor_up");
            }
            _ => panic!("Expected ExecuteAction, got {:?}", cmd),
        }
    }

    // ==================== Debug Test ====================

    #[test]
    fn test_api_debug() {
        let (mut backend, _rx) = create_test_backend();

        // debug() should not panic and should work with any input
        backend
            .execute_js(
                r#"
            const editor = getEditor();
            editor.debug("Test debug message");
            editor.debug("Another message with special chars: <>&\"'");
        "#,
                "test.js",
            )
            .unwrap();
        // If we get here without panic, the test passes
    }

    // ==================== TypeScript Definitions Test ====================

    #[test]
    fn test_typescript_preamble_generated() {
        // Check that the TypeScript preamble constant exists and has content
        assert!(!JSEDITORAPI_TS_PREAMBLE.is_empty());
        assert!(JSEDITORAPI_TS_PREAMBLE.contains("declare function getEditor()"));
        assert!(JSEDITORAPI_TS_PREAMBLE.contains("ProcessHandle"));
        println!(
            "Generated {} bytes of TypeScript preamble",
            JSEDITORAPI_TS_PREAMBLE.len()
        );
    }

    #[test]
    fn test_typescript_editor_api_generated() {
        // Check that the EditorAPI interface is generated
        assert!(!JSEDITORAPI_TS_EDITOR_API.is_empty());
        assert!(JSEDITORAPI_TS_EDITOR_API.contains("interface EditorAPI"));
        println!(
            "Generated {} bytes of EditorAPI interface",
            JSEDITORAPI_TS_EDITOR_API.len()
        );
    }

    #[test]
    fn test_js_methods_list() {
        // Check that the JS methods list is generated
        assert!(!JSEDITORAPI_JS_METHODS.is_empty());
        println!("Generated {} API methods", JSEDITORAPI_JS_METHODS.len());
        // Print first 20 methods
        for (i, method) in JSEDITORAPI_JS_METHODS.iter().enumerate() {
            if i < 20 {
                println!("  - {}", method);
            }
        }
        if JSEDITORAPI_JS_METHODS.len() > 20 {
            println!("  ... and {} more", JSEDITORAPI_JS_METHODS.len() - 20);
        }
    }
}
