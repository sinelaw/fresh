//! TypeScript Plugin Runtime
//!
//! This module provides a TypeScript/JavaScript runtime for plugins using deno_core.
//! It enables native async/await support, solving the async command execution problem
//! that existed with the Lua plugin system.
//!
//! # Core Concepts
//!
//! ## Buffers
//! A buffer holds text content and may or may not be associated with a file.
//! Each buffer has a unique numeric ID that persists for the editor session.
//! Buffers track their content, modification state, cursor positions, and path.
//! All text operations (insert, delete, read) use byte offsets, not character indices.
//!
//! ## Splits
//! A split is a viewport pane that displays a buffer. The editor can have multiple
//! splits arranged in a tree layout. Each split shows exactly one buffer, but the
//! same buffer can be displayed in multiple splits. Use split IDs to control which
//! pane displays which buffer.
//!
//! ## Virtual Buffers
//! Special buffers created by plugins to display structured data like search results,
//! diagnostics, or git logs. Virtual buffers support text properties (metadata attached
//! to text ranges) that plugins can query when the user selects a line. Unlike normal
//! buffers, virtual buffers are typically read-only and not backed by files.
//!
//! ## Text Properties
//! Metadata attached to text ranges in virtual buffers. Each entry has text content
//! and a properties object with arbitrary key-value pairs. Use getTextPropertiesAtCursor
//! to retrieve properties at the cursor position (e.g., to get file/line info for "go to").
//!
//! ## Overlays
//! Visual decorations applied to buffer text without modifying content. Overlays can
//! change text color and add underlines. Use overlay IDs to manage them; prefix IDs
//! enable batch removal (e.g., "lint:" prefix for all linter highlights).
//!
//! ## Modes
//! Keybinding contexts that determine how keypresses are interpreted. Each buffer has
//! a mode (e.g., "normal", "insert", "special"). Custom modes can inherit from parents
//! and define buffer-local keybindings. Virtual buffers typically use custom modes.

use crate::input::commands::Suggestion;
use crate::model::event::BufferId;
use crate::model::event::SplitId;
use crate::services::plugins::api::{
    ActionPopupAction, ActionSpec, EditorStateSnapshot, LayoutHints, PluginCommand, ViewTokenWire,
};
use anyhow::{anyhow, Result};
use deno_core::{
    error::ModuleLoaderError, extension, op2, FastString, JsRuntime, ModuleLoadOptions,
    ModuleLoadReferrer, ModuleLoadResponse, ModuleSource, ModuleSourceCode, ModuleSpecifier,
    ModuleType, OpState, ResolutionKind, RuntimeOptions,
};
use deno_error::JsErrorBox;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, RwLock};

// LayoutHints and ViewTransform are defined in plugin_api

/// Custom module loader that transpiles TypeScript to JavaScript
struct TypeScriptModuleLoader;

impl deno_core::ModuleLoader for TypeScriptModuleLoader {
    fn resolve(
        &self,
        specifier: &str,
        referrer: &str,
        _kind: ResolutionKind,
    ) -> Result<ModuleSpecifier, ModuleLoaderError> {
        deno_core::resolve_import(specifier, referrer)
            .map_err(|e| JsErrorBox::generic(e.to_string()))
    }

    fn load(
        &self,
        module_specifier: &ModuleSpecifier,
        _maybe_referrer: Option<&ModuleLoadReferrer>,
        _options: ModuleLoadOptions,
    ) -> ModuleLoadResponse {
        let specifier = module_specifier.clone();
        let module_load = async move {
            let path = specifier
                .to_file_path()
                .map_err(|_| JsErrorBox::generic(format!("Invalid file URL: {}", specifier)))?;

            let code = std::fs::read_to_string(&path).map_err(|e| {
                JsErrorBox::generic(format!("Failed to read {}: {}", path.display(), e))
            })?;

            // Check if we need to transpile TypeScript
            let (code, module_type) = if path.extension().and_then(|s| s.to_str()) == Some("ts") {
                // Transpile TypeScript to JavaScript
                let transpiled = transpile_typescript(&code, &specifier)?;
                (transpiled, ModuleType::JavaScript)
            } else {
                (code, ModuleType::JavaScript)
            };

            let module_source = ModuleSource::new(
                module_type,
                ModuleSourceCode::String(code.into()),
                &specifier,
                None,
            );

            Ok(module_source)
        };

        ModuleLoadResponse::Async(Box::pin(module_load))
    }
}

/// Transpile TypeScript to JavaScript using deno_ast
fn transpile_typescript(source: &str, specifier: &ModuleSpecifier) -> Result<String, JsErrorBox> {
    use deno_ast::{EmitOptions, MediaType, ParseParams, TranspileOptions};

    let parsed = deno_ast::parse_module(ParseParams {
        specifier: specifier.clone(),
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        scope_analysis: false,
        maybe_syntax: None,
    })
    .map_err(|e| JsErrorBox::generic(format!("TypeScript parse error: {}", e)))?;

    let transpiled = parsed
        .transpile(
            &TranspileOptions::default(),
            &Default::default(),
            &EmitOptions::default(),
        )
        .map_err(|e| JsErrorBox::generic(format!("TypeScript transpile error: {}", e)))?;

    Ok(transpiled.into_source().text.to_string())
}

/// A cancellable process with pending output collection
struct CancellableProcess {
    /// The child process handle (for killing)
    child: tokio::process::Child,
    /// Receiver for the collected output (stdout, stderr)
    output_rx: tokio::sync::oneshot::Receiver<(String, String)>,
}

/// Shared state accessible from ops
struct TsRuntimeState {
    /// Editor state snapshot (read-only access)
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
    /// Command sender for write operations
    command_sender: std::sync::mpsc::Sender<PluginCommand>,
    /// Event handlers: event_name -> list of global JS function names
    event_handlers: Rc<RefCell<HashMap<String, Vec<String>>>>,
    /// Pending response senders for async operations (request_id -> sender)
    pending_responses: Arc<
        std::sync::Mutex<
            HashMap<
                u64,
                tokio::sync::oneshot::Sender<crate::services::plugins::api::PluginResponse>,
            >,
        >,
    >,
    /// Next request ID for async operations
    next_request_id: Rc<RefCell<u64>>,
    /// Background processes: process_id -> Child handle
    background_processes: Rc<RefCell<HashMap<u64, tokio::process::Child>>>,
    /// Cancellable processes: process_id -> CancellableProcess
    cancellable_processes: Rc<RefCell<HashMap<u64, CancellableProcess>>>,
    /// Process PIDs: process_id -> OS PID (for killing processes that are being waited on)
    process_pids: Rc<RefCell<HashMap<u64, u32>>>,
    /// Next process ID for background processes
    next_process_id: Rc<RefCell<u64>>,
    /// Directory context for system paths
    dir_context: crate::config_io::DirectoryContext,
}

/// Display a transient message in the editor's status bar
///
/// The message will be shown until the next status update or user action.
/// Use for feedback on completed operations (e.g., "File saved", "2 matches found").
/// @param message - Text to display; keep short (status bar has limited width)
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

/// Apply a theme by name
///
/// Loads and applies the specified theme immediately. The theme can be a built-in
/// theme name or a custom theme from the themes directory.
/// @param theme_name - Name of the theme to apply (e.g., "dark", "light", "my-custom-theme")
#[op2(fast)]
fn op_fresh_apply_theme(state: &mut OpState, #[string] theme_name: String) {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let _ = runtime_state
            .command_sender
            .send(PluginCommand::ApplyTheme {
                theme_name: theme_name.clone(),
            });
    }
    tracing::info!("TypeScript plugin apply_theme: {}", theme_name);
}

/// Get the theme JSON Schema for the theme editor
///
/// Returns the raw JSON Schema generated by schemars for ThemeFile.
/// The schema uses standard JSON Schema format with $ref for type references.
/// Plugins are responsible for parsing the schema and resolving $ref references.
/// @returns JSON Schema object
#[op2]
#[serde]
fn op_fresh_get_theme_schema() -> serde_json::Value {
    use crate::view::theme::get_theme_schema;
    get_theme_schema()
}

#[op2]
#[serde]
fn op_fresh_get_builtin_themes() -> serde_json::Value {
    use crate::view::theme::get_builtin_themes;
    get_builtin_themes()
}

/// Reload configuration from file
///
/// After a plugin saves config changes to the config file, call this to reload
/// the editor's in-memory configuration. This ensures the editor and plugins
/// stay in sync with the saved config.
#[op2(fast)]
fn op_fresh_reload_config(state: &mut OpState) {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let _ = runtime_state
            .command_sender
            .send(PluginCommand::ReloadConfig);
    }
    tracing::debug!("TypeScript plugin: reloading config");
}

/// Get the current editor configuration
///
/// Returns the merged configuration (user config file + compiled-in defaults).
/// This is the runtime config that the editor is actually using, including
/// all default values for LSP servers, languages, keybindings, etc.
/// @returns Configuration object
#[op2]
#[serde]
fn op_fresh_get_config(state: &mut OpState) -> serde_json::Value {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot.config.clone();
        };
    }
    // Return empty object if config not available
    serde_json::Value::Object(serde_json::Map::new())
}

/// Get the user's configuration (only explicitly set values)
///
/// Returns only the configuration from the user's config file.
/// Fields not present here are using default values.
/// Use this with getConfig() to determine which values are defaults.
/// @returns User configuration object (sparse - only explicitly set values)
#[op2]
#[serde]
fn op_fresh_get_user_config(state: &mut OpState) -> serde_json::Value {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot.user_config.clone();
        };
    }
    // Return empty object if config not available
    serde_json::Value::Object(serde_json::Map::new())
}

/// Log an error message from a plugin
///
/// Messages appear in log file when running with RUST_LOG=error.
/// Use for critical errors that need attention.
/// @param message - Error message
#[op2(fast)]
fn op_fresh_error(#[string] message: String) {
    tracing::error!("TypeScript plugin: {}", message);
}

/// Log a warning message from a plugin
///
/// Messages appear in log file when running with RUST_LOG=warn.
/// Use for warnings that don't prevent operation but indicate issues.
/// @param message - Warning message
#[op2(fast)]
fn op_fresh_warn(#[string] message: String) {
    tracing::warn!("TypeScript plugin: {}", message);
}

/// Log an info message from a plugin
///
/// Messages appear in log file when running with RUST_LOG=info.
/// Use for important operational messages.
/// @param message - Info message
#[op2(fast)]
fn op_fresh_info(#[string] message: String) {
    tracing::info!("TypeScript plugin: {}", message);
}

/// Log a debug message from a plugin
///
/// Messages appear in log file when running with RUST_LOG=debug.
/// Useful for plugin development and troubleshooting.
/// @param message - Debug message; include context like function name and relevant values
#[op2(fast)]
fn op_fresh_debug(#[string] message: String) {
    tracing::debug!("TypeScript plugin: {}", message);
}

/// Copy text to the system clipboard
///
/// Copies the provided text to both the internal and system clipboard.
/// Uses OSC 52 and arboard for cross-platform compatibility.
/// @param text - Text to copy to clipboard
#[op2(fast)]
fn op_fresh_set_clipboard(state: &mut OpState, #[string] text: String) {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let _ = runtime_state
            .command_sender
            .send(PluginCommand::SetClipboard { text: text.clone() });
    }
    tracing::debug!("TypeScript plugin set_clipboard: {} chars", text.len());
}

/// Get the user configuration directory path
///
/// Returns the absolute path to the directory where user config and themes are stored.
/// e.g. ~/.config/fresh/ on Linux or ~/Library/Application Support/fresh/ on macOS.
#[op2]
#[string]
fn op_fresh_get_config_dir(state: &mut OpState) -> String {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        return runtime_state
            .dir_context
            .config_dir
            .to_string_lossy()
            .to_string();
    }
    String::new()
}

/// Get the user themes directory path
///
/// Returns the absolute path to the directory where user themes are stored.
/// e.g. ~/.config/fresh/themes/
#[op2]
#[string]
fn op_fresh_get_themes_dir(state: &mut OpState) -> String {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        return runtime_state
            .dir_context
            .themes_dir()
            .to_string_lossy()
            .to_string();
    }
    String::new()
}

/// Get the buffer ID of the focused editor pane
///
/// Returns 0 if no buffer is active (rare edge case).
/// Use this ID with other buffer operations like insertText.
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

/// Get the byte offset of the primary cursor in the active buffer
///
/// Returns 0 if no cursor exists. For multi-cursor scenarios, use getAllCursors
/// to get all cursor positions with selection info.
/// Note: This is a byte offset, not a character index (UTF-8 matters).
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

/// Get the absolute file path for a buffer
///
/// Returns empty string for unsaved buffers or virtual buffers.
/// The path is always absolute. Use this to determine file type,
/// construct related paths, or display to the user.
/// @param buffer_id - Target buffer ID
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

/// Get the total byte length of a buffer's content
///
/// Returns 0 if buffer doesn't exist.
/// @param buffer_id - Target buffer ID
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

/// Check if a buffer has been modified since last save
///
/// Returns false if buffer doesn't exist or has never been saved.
/// Virtual buffers are never considered modified.
/// @param buffer_id - Target buffer ID
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

/// Insert text at a byte position in a buffer
///
/// Text is inserted before the byte at position. Position must be valid
/// (0 to buffer length). Insertion shifts all text after position.
/// Operation is asynchronous; returns true if command was sent successfully.
/// @param buffer_id - Target buffer ID
/// @param position - Byte offset where text will be inserted (must be at char boundary)
/// @param text - UTF-8 text to insert
#[op2(fast)]
fn op_fresh_insert_text(
    state: &mut OpState,
    buffer_id: u32,
    position: u32,
    #[string] text: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::InsertText {
                buffer_id: BufferId(buffer_id as usize),
                position: position as usize,
                text,
            });
        return result.is_ok();
    }
    false
}

/// Delete a byte range from a buffer
///
/// Deletes bytes from start (inclusive) to end (exclusive).
/// Both positions must be at valid UTF-8 char boundaries.
/// Operation is asynchronous; returns true if command was sent successfully.
/// @param buffer_id - Target buffer ID
/// @param start - Start byte offset (inclusive)
/// @param end - End byte offset (exclusive)
#[op2(fast)]
fn op_fresh_delete_range(state: &mut OpState, buffer_id: u32, start: u32, end: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::DeleteRange {
                buffer_id: BufferId(buffer_id as usize),
                range: (start as usize)..(end as usize),
            });
        return result.is_ok();
    }
    false
}

/// Add a colored highlight overlay to text without modifying content
///
/// Overlays are visual decorations that persist until explicitly removed.
/// Add an overlay (visual decoration) to a buffer
/// Use namespaces for easy batch removal (e.g., "spell", "todo").
/// Multiple overlays can apply to the same range; colors blend.
/// @param buffer_id - Target buffer ID
/// @param namespace - Optional namespace for grouping (use clearNamespace for batch removal)
/// @param start - Start byte offset
/// @param end - End byte offset
/// @param r - Red (0-255)
/// @param g - Green (0-255)
/// @param b - Blue (0-255)
/// @param underline - Add underline decoration
/// @param bold - Use bold text
/// @param italic - Use italic text
/// @param extend_to_line_end - Extend background to end of visual line
/// @returns true if overlay was added
#[op2(fast)]
#[allow(clippy::too_many_arguments)]
fn op_fresh_add_overlay(
    state: &mut OpState,
    buffer_id: u32,
    #[string] namespace: String,
    start: u32,
    end: u32,
    r: u8,
    g: u8,
    b: u8,
    bg_r: i16,
    bg_g: i16,
    bg_b: i16,
    underline: bool,
    bold: bool,
    italic: bool,
    extend_to_line_end: bool,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let ns = if namespace.is_empty() {
            None
        } else {
            Some(crate::view::overlay::OverlayNamespace::from_string(
                namespace,
            ))
        };

        let bg_color = if bg_r >= 0 && bg_g >= 0 && bg_b >= 0 {
            Some((bg_r as u8, bg_g as u8, bg_b as u8))
        } else {
            None
        };

        let result = runtime_state
            .command_sender
            .send(PluginCommand::AddOverlay {
                buffer_id: BufferId(buffer_id as usize),
                namespace: ns,
                range: (start as usize)..(end as usize),
                color: (r, g, b),
                bg_color,
                underline,
                bold,
                italic,
                extend_to_line_end,
            });
        return result.is_ok();
    }
    false
}

/// Remove a specific overlay by its handle
/// @param buffer_id - The buffer ID
/// @param handle - The overlay handle to remove
/// @returns true if overlay was removed
#[op2(fast)]
fn op_fresh_remove_overlay(state: &mut OpState, buffer_id: u32, #[string] handle: String) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::RemoveOverlay {
                buffer_id: BufferId(buffer_id as usize),
                handle: crate::view::overlay::OverlayHandle::from_string(handle),
            });
        return result.is_ok();
    }
    false
}

/// Clear all overlays in a namespace
/// @param buffer_id - The buffer ID
/// @param namespace - The namespace to clear
/// @returns true if successful
#[op2(fast)]
fn op_fresh_clear_namespace(
    state: &mut OpState,
    buffer_id: u32,
    #[string] namespace: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ClearNamespace {
                buffer_id: BufferId(buffer_id as usize),
                namespace: crate::view::overlay::OverlayNamespace::from_string(namespace),
            });
        return result.is_ok();
    }
    false
}

/// Clear all overlays that overlap with a byte range
/// @param buffer_id - The buffer ID
/// @param start - Start byte position (inclusive)
/// @param end - End byte position (exclusive)
/// @returns true if successful
#[op2(fast)]
fn op_fresh_clear_overlays_in_range(
    state: &mut OpState,
    buffer_id: u32,
    start: u32,
    end: u32,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ClearOverlaysInRange {
                buffer_id: BufferId(buffer_id as usize),
                start: start as usize,
                end: end as usize,
            });
        return result.is_ok();
    }
    false
}

/// Enable/disable line numbers for a buffer
/// @param buffer_id - The buffer ID
/// @param enabled - Whether to show line numbers
/// @returns true if successful
#[op2(fast)]
fn op_fresh_set_line_numbers(state: &mut OpState, buffer_id: u32, enabled: bool) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetLineNumbers {
                buffer_id: BufferId(buffer_id as usize),
                enabled,
            });
        return result.is_ok();
    }
    false
}

/// Remove all overlays from a buffer
/// @param buffer_id - The buffer ID
/// @returns true if overlays were cleared
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

/// Add virtual text (inline decoration) at a position
/// @param buffer_id - The buffer ID
/// @param virtual_text_id - Unique identifier for this virtual text
/// @param position - Byte position to insert at
/// @param text - The virtual text to display
/// @param r - Red color component (0-255)
/// @param g - Green color component (0-255)
/// @param b - Blue color component (0-255)
/// @param before - Whether to insert before (true) or after (false) the position
/// @param use_bg - Whether to use the color as background (true) or foreground (false)
/// @returns true if virtual text was added
#[op2(fast)]
#[allow(clippy::too_many_arguments)]
fn op_fresh_add_virtual_text(
    state: &mut OpState,
    buffer_id: u32,
    #[string] virtual_text_id: String,
    position: u32,
    #[string] text: String,
    r: u8,
    g: u8,
    b: u8,
    before: bool,
    use_bg: bool,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::AddVirtualText {
                buffer_id: BufferId(buffer_id as usize),
                virtual_text_id,
                position: position as usize,
                text,
                color: (r, g, b),
                use_bg,
                before,
            });
        return result.is_ok();
    }
    false
}

/// Remove virtual text by ID
/// @param buffer_id - The buffer ID
/// @param virtual_text_id - The virtual text ID to remove
/// @returns true if virtual text was removed
#[op2(fast)]
fn op_fresh_remove_virtual_text(
    state: &mut OpState,
    buffer_id: u32,
    #[string] virtual_text_id: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::RemoveVirtualText {
                buffer_id: BufferId(buffer_id as usize),
                virtual_text_id,
            });
        return result.is_ok();
    }
    false
}

/// Remove all virtual texts with IDs starting with a prefix
/// @param buffer_id - The buffer ID
/// @param prefix - The prefix to match virtual text IDs against
/// @returns true if any virtual texts were removed
#[op2(fast)]
fn op_fresh_remove_virtual_texts_by_prefix(
    state: &mut OpState,
    buffer_id: u32,
    #[string] prefix: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::RemoveVirtualTextsByPrefix {
                buffer_id: BufferId(buffer_id as usize),
                prefix,
            });
        return result.is_ok();
    }
    false
}

/// Remove all virtual texts from a buffer
/// @param buffer_id - The buffer ID
/// @returns true if virtual texts were cleared
#[op2(fast)]
fn op_fresh_clear_virtual_texts(state: &mut OpState, buffer_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ClearVirtualTexts {
                buffer_id: BufferId(buffer_id as usize),
            });
        return result.is_ok();
    }
    false
}

/// Add a virtual line above or below a source line
/// @param buffer_id - The buffer ID
/// @param position - Byte position to anchor the virtual line to
/// @param text - The text content of the virtual line
/// @param fg_r - Foreground red color component (0-255)
/// @param fg_g - Foreground green color component (0-255)
/// @param fg_b - Foreground blue color component (0-255)
/// @param bg_r - Background red color component (0-255), -1 for transparent
/// @param bg_g - Background green color component (0-255), -1 for transparent
/// @param bg_b - Background blue color component (0-255), -1 for transparent
/// @param above - Whether to insert above (true) or below (false) the line
/// @param namespace - Namespace for bulk removal (e.g., "git-blame")
/// @param priority - Priority for ordering multiple lines at same position
/// @returns true if virtual line was added
#[op2(fast)]
#[allow(clippy::too_many_arguments)]
fn op_fresh_add_virtual_line(
    state: &mut OpState,
    buffer_id: u32,
    position: u32,
    #[string] text: String,
    fg_r: u8,
    fg_g: u8,
    fg_b: u8,
    bg_r: i16,
    bg_g: i16,
    bg_b: i16,
    above: bool,
    #[string] namespace: String,
    priority: i32,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();

        // Convert background color: -1 means transparent (None)
        let bg_color = if bg_r >= 0 && bg_g >= 0 && bg_b >= 0 {
            Some((bg_r as u8, bg_g as u8, bg_b as u8))
        } else {
            None
        };

        let result = runtime_state
            .command_sender
            .send(PluginCommand::AddVirtualLine {
                buffer_id: BufferId(buffer_id as usize),
                position: position as usize,
                text,
                fg_color: (fg_r, fg_g, fg_b),
                bg_color,
                above,
                namespace,
                priority,
            });
        return result.is_ok();
    }
    false
}

/// Clear all virtual texts in a namespace
/// @param buffer_id - The buffer ID
/// @param namespace - The namespace to clear (e.g., "git-blame")
/// @returns true if namespace was cleared
#[op2(fast)]
fn op_fresh_clear_virtual_text_namespace(
    state: &mut OpState,
    buffer_id: u32,
    #[string] namespace: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ClearVirtualTextNamespace {
                buffer_id: BufferId(buffer_id as usize),
                namespace,
            });
        return result.is_ok();
    }
    false
}

/// Force a refresh of line display for a buffer
/// @param buffer_id - The buffer ID
/// @returns true if refresh was triggered
#[op2(fast)]
fn op_fresh_refresh_lines(state: &mut OpState, buffer_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::RefreshLines {
                buffer_id: BufferId(buffer_id as usize),
            });
        return result.is_ok();
    }
    false
}

/// Set a line indicator in the gutter's indicator column
/// @param buffer_id - The buffer ID
/// @param line - Line number (0-indexed)
/// @param namespace - Namespace for grouping (e.g., "git-gutter", "breakpoints")
/// @param symbol - Symbol to display (e.g., "│", "●", "★")
/// @param r - Red color component (0-255)
/// @param g - Green color component (0-255)
/// @param b - Blue color component (0-255)
/// @param priority - Priority for display when multiple indicators exist (higher wins)
/// @returns true if indicator was set
#[op2(fast)]
#[allow(clippy::too_many_arguments)]
fn op_fresh_set_line_indicator(
    state: &mut OpState,
    buffer_id: u32,
    line: u32,
    #[string] namespace: String,
    #[string] symbol: String,
    r: u8,
    g: u8,
    b: u8,
    priority: i32,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetLineIndicator {
                buffer_id: BufferId(buffer_id as usize),
                line: line as usize,
                namespace,
                symbol,
                color: (r, g, b),
                priority,
            });
        return result.is_ok();
    }
    false
}

/// Clear all line indicators for a specific namespace
/// @param buffer_id - The buffer ID
/// @param namespace - Namespace to clear (e.g., "git-gutter")
/// @returns true if indicators were cleared
#[op2(fast)]
fn op_fresh_clear_line_indicators(
    state: &mut OpState,
    buffer_id: u32,
    #[string] namespace: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ClearLineIndicators {
                buffer_id: BufferId(buffer_id as usize),
                namespace,
            });
        return result.is_ok();
    }
    false
}

/// File explorer decoration entry provided by plugins
#[derive(serde::Deserialize)]
struct FileExplorerDecoration {
    /// Absolute or workspace-relative path to decorate
    path: String,
    /// Symbol to display (single character recommended)
    symbol: Option<String>,
    /// RGB color for the symbol
    color: Option<[u8; 3]>,
    /// Priority for resolving conflicts (higher wins)
    priority: Option<i32>,
}

/// Set file explorer decorations for a namespace
/// @param namespace - Namespace for grouping (e.g., "git-status")
/// @param decorations - Decoration entries
/// @returns true if decorations were accepted
#[op2]
fn op_fresh_set_file_explorer_decorations(
    state: &mut OpState,
    #[string] namespace: String,
    #[serde] decorations: Vec<FileExplorerDecoration>,
) -> bool {
    use crate::view::file_tree::FileExplorerDecoration as ExplorerDecoration;
    use std::path::PathBuf;

    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let decorations: Vec<ExplorerDecoration> = decorations
            .into_iter()
            .filter_map(|entry| {
                if entry.path.is_empty() {
                    return None;
                }
                let symbol = entry.symbol.unwrap_or_else(|| "●".to_string());
                let color = entry.color.unwrap_or([255, 184, 108]);
                let priority = entry.priority.unwrap_or(0);
                Some(ExplorerDecoration {
                    path: PathBuf::from(entry.path),
                    symbol,
                    color: (color[0], color[1], color[2]),
                    priority,
                })
            })
            .collect();

        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetFileExplorerDecorations {
                namespace,
                decorations,
            });
        return result.is_ok();
    }
    false
}

/// Clear file explorer decorations for a namespace
/// @param namespace - Namespace to clear (e.g., "git-status")
/// @returns true if decorations were cleared
#[op2(fast)]
fn op_fresh_clear_file_explorer_decorations(
    state: &mut OpState,
    #[string] namespace: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ClearFileExplorerDecorations { namespace });
        return result.is_ok();
    }
    false
}

/// Submit a transformed view stream for a viewport
/// @param buffer_id - Buffer to apply the transform to
/// @param start - Viewport start byte
/// @param end - Viewport end byte
/// @param tokens - Array of tokens with source offsets
/// @param source_map - Array of source offsets (null for injected)
/// @param layout_hints - Optional layout hints (compose width, column guides)
#[op2]
fn op_fresh_submit_view_transform(
    state: &mut OpState,
    buffer_id: u32,
    split_id: Option<u32>,
    start: u32,
    end: u32,
    #[serde] tokens: Vec<ViewTokenWire>,
    #[serde] layout_hints: Option<LayoutHints>,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let hints = layout_hints.unwrap_or(LayoutHints {
            compose_width: None,
            column_guides: None,
        });
        let split_id = split_id.map(|id| SplitId(id as usize));
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetLayoutHints {
                buffer_id: BufferId(buffer_id as usize),
                split_id,
                range: start as usize..end as usize,
                hints: hints.clone(),
            });
        // Also send full view transform payload for renderer consumption
        let _ = runtime_state
            .command_sender
            .send(PluginCommand::SubmitViewTransform {
                buffer_id: BufferId(buffer_id as usize),
                split_id,
                payload: crate::services::plugins::api::ViewTransformPayload {
                    range: start as usize..end as usize,
                    tokens,
                    layout_hints: Some(hints),
                },
            });
        return result.is_ok();
    }
    false
}

/// Clear view transform for a buffer/split (returns to normal rendering)
/// @param buffer_id - Buffer ID
/// @param split_id - Optional split ID (uses active split if not specified)
/// @returns true if clear succeeded
#[op2]
fn op_fresh_clear_view_transform(
    state: &mut OpState,
    buffer_id: u32,
    split_id: Option<u32>,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let split_id = split_id.map(|id| SplitId(id as usize));
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ClearViewTransform {
                buffer_id: BufferId(buffer_id as usize),
                split_id,
            });
        return result.is_ok();
    }
    false
}

/// Insert text at the current cursor position in the active buffer
/// @param text - The text to insert
/// @returns true if insertion succeeded
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

/// Get the currently active locale
#[op2]
#[string]
fn op_fresh_get_current_locale() -> String {
    crate::i18n::current_locale()
}

/// Translate a string for a plugin using the current locale
#[op2]
#[string]
fn op_fresh_plugin_translate(
    #[string] plugin_name: String,
    #[string] key: String,
    #[serde] args: std::collections::HashMap<String, String>,
) -> String {
    crate::i18n::translate_plugin_string(&plugin_name, &key, &args)
}

/// Register a custom command that can be triggered by keybindings or the command palette
#[op2(fast)]
fn op_fresh_register_command(
    state: &mut OpState,
    #[string] name: String,
    #[string] description: String,
    #[string] action: String,
    #[string] contexts: String,
    #[string] source: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();

        // Parse contexts string - separate into built-in KeyContext values and custom contexts
        let mut context_list: Vec<crate::input::keybindings::KeyContext> = Vec::new();
        let mut custom_context_list: Vec<String> = Vec::new();

        if !contexts.trim().is_empty() {
            for ctx in contexts.split(',').map(|s| s.trim()) {
                if ctx.is_empty() {
                    continue;
                }
                // Try to parse as built-in context
                match ctx.to_lowercase().as_str() {
                    "global" => context_list.push(crate::input::keybindings::KeyContext::Global),
                    "normal" => context_list.push(crate::input::keybindings::KeyContext::Normal),
                    "prompt" => context_list.push(crate::input::keybindings::KeyContext::Prompt),
                    "popup" => context_list.push(crate::input::keybindings::KeyContext::Popup),
                    "fileexplorer" | "file_explorer" => {
                        context_list.push(crate::input::keybindings::KeyContext::FileExplorer)
                    }
                    "menu" => context_list.push(crate::input::keybindings::KeyContext::Menu),
                    _ => {
                        // Not a built-in context, treat as custom context
                        custom_context_list.push(ctx.to_string());
                    }
                }
            }
        }

        // Use the explicit source parameter
        let command_source = if source.is_empty() {
            crate::input::commands::CommandSource::Builtin
        } else {
            crate::input::commands::CommandSource::Plugin(source.clone())
        };

        let command = crate::input::commands::Command {
            name: name.clone(),
            description,
            action: crate::input::keybindings::Action::PluginAction(action),
            contexts: context_list,
            custom_contexts: custom_context_list,
            source: command_source,
        };

        let result = runtime_state
            .command_sender
            .send(PluginCommand::RegisterCommand { command });
        return result.is_ok();
    }
    false
}

/// Unregister a custom command by name
/// @param name - The name of the command to unregister
/// @returns true if the command was successfully unregistered
#[op2(fast)]
fn op_fresh_unregister_command(state: &mut OpState, #[string] name: String) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::UnregisterCommand { name });
        return result.is_ok();
    }
    false
}

/// Set or unset a custom context for command visibility
/// Custom contexts allow plugins to control when their commands are available.
/// For example, setting "config-editor" context makes config editor commands visible.
/// @param name - Context name (e.g., "config-editor")
/// @param active - Whether the context is active (true = set, false = unset)
/// @returns true if the context was updated
#[op2(fast)]
fn op_fresh_set_context(state: &mut OpState, #[string] name: String, active: bool) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetContext { name, active });
        return result.is_ok();
    }
    false
}

/// Open a file in the editor, optionally at a specific location
/// @param path - File path to open
/// @param line - Line number to jump to (0 for no jump)
/// @param column - Column number to jump to (0 for no jump)
/// @returns true if file was opened
#[op2(fast)]
fn op_fresh_open_file(state: &mut OpState, #[string] path: String, line: u32, column: u32) -> bool {
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

/// Get the ID of the focused split pane
///
/// Use with focusSplit, setSplitBuffer, or createVirtualBufferInExistingSplit
/// to manage split layouts.
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

/// Get the line number of the primary cursor (1-indexed)
///
/// Line numbers start at 1. Returns 1 if no cursor exists.
/// For byte offset use getCursorPosition instead.
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

/// Get byte offsets of all cursors (multi-cursor support)
///
/// Returns array of positions; empty if no cursors. Primary cursor
/// is typically first. For selection info use getAllCursors instead.
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

/// Open a file in a specific split pane
/// @param split_id - The split ID to open the file in
/// @param path - File path to open
/// @param line - Line number to jump to (0 for no jump)
/// @param column - Column number to jump to (0 for no jump)
/// @returns true if file was opened
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

/// Result from spawnProcess
#[derive(serde::Serialize)]
struct SpawnResult {
    /// Complete stdout as string. Newlines preserved; trailing newline included.
    stdout: String,
    /// Complete stderr as string. Contains error messages and warnings.
    stderr: String,
    /// Process exit code. 0 usually means success; -1 if process was killed.
    exit_code: i32,
}

/// Result from spawnBackgroundProcess - just the process ID
#[derive(serde::Serialize)]
struct BackgroundProcessResult {
    /// Unique process ID for later reference (kill, status check)
    process_id: u64,
}

/// Spawn a long-running background process
///
/// Unlike spawnProcess which waits for completion, this starts a process
/// in the background and returns immediately with a process ID.
/// Use killProcess(id) to terminate the process later.
/// Use isProcessRunning(id) to check if it's still running.
///
/// @param command - Program name (searched in PATH) or absolute path
/// @param args - Command arguments (each array element is one argument)
/// @param cwd - Working directory; null uses editor's cwd
/// @returns Object with process_id for later reference
/// @example
/// const proc = await editor.spawnBackgroundProcess("asciinema", ["rec", "output.cast"]);
/// // Later...
/// await editor.killProcess(proc.process_id);
#[op2(async)]
#[serde]
async fn op_fresh_spawn_background_process(
    state: Rc<RefCell<OpState>>,
    #[string] command: String,
    #[serde] args: Vec<String>,
    #[string] cwd: Option<String>,
) -> Result<BackgroundProcessResult, JsErrorBox> {
    use std::process::Stdio;
    use tokio::process::Command;

    // Build the command
    let mut cmd = Command::new(&command);
    cmd.args(&args);
    // For background processes, we don't capture output (it runs independently)
    cmd.stdout(Stdio::null());
    cmd.stderr(Stdio::null());
    cmd.stdin(Stdio::null());

    // Set working directory if provided
    if let Some(ref dir) = cwd {
        cmd.current_dir(dir);
    }

    // Spawn the process
    let child = cmd
        .spawn()
        .map_err(|e| JsErrorBox::generic(format!("Failed to spawn process: {}", e)))?;

    // Get process ID and store the child handle
    let process_id = {
        let op_state = state.borrow();
        if let Some(runtime_state) = op_state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
            let runtime_state = runtime_state.borrow();
            let mut id = runtime_state.next_process_id.borrow_mut();
            let process_id = *id;
            *id += 1;
            drop(id);

            runtime_state
                .background_processes
                .borrow_mut()
                .insert(process_id, child);
            process_id
        } else {
            return Err(JsErrorBox::generic("Runtime state not available"));
        }
    };

    Ok(BackgroundProcessResult { process_id })
}

/// Kill a background or cancellable process by ID
///
/// Sends SIGTERM to gracefully terminate the process.
/// Returns true if the process was found and killed, false if not found.
///
/// @param process_id - ID returned from spawnBackgroundProcess or spawnProcessStart
/// @returns true if process was killed, false if not found
#[op2(async)]
async fn op_fresh_kill_process(
    state: Rc<RefCell<OpState>>,
    #[bigint] process_id: u64,
) -> Result<bool, JsErrorBox> {
    // Try to find and remove from either background_processes or cancellable_processes
    let (bg_child, cancellable, os_pid) = {
        let op_state = state.borrow();
        if let Some(runtime_state) = op_state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
            let runtime_state = runtime_state.borrow();
            let bg = runtime_state
                .background_processes
                .borrow_mut()
                .remove(&process_id);
            let cancellable = runtime_state
                .cancellable_processes
                .borrow_mut()
                .remove(&process_id);
            // Also get OS PID for fallback kill-by-pid
            let os_pid = runtime_state.process_pids.borrow_mut().remove(&process_id);
            (bg, cancellable, os_pid)
        } else {
            return Ok(false);
        }
    };

    // Kill whichever one we found
    if let Some(mut child) = bg_child {
        let _ = child.kill().await;
        Ok(true)
    } else if let Some(mut process) = cancellable {
        let _ = process.child.kill().await;
        Ok(true)
    } else if let Some(pid) = os_pid {
        // Fallback: kill by OS PID when spawn_process_wait has taken ownership
        // This happens when await-ing the process while trying to kill it
        tracing::trace!(process_id, pid, "killing process by OS PID (fallback)");
        #[cfg(unix)]
        {
            use nix::sys::signal::{kill, Signal};
            use nix::unistd::Pid;
            let _ = kill(Pid::from_raw(pid as i32), Signal::SIGKILL);
        }
        #[cfg(not(unix))]
        {
            // On non-Unix, try using taskkill
            let _ = std::process::Command::new("taskkill")
                .args(["/F", "/PID", &pid.to_string()])
                .status();
        }
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Check if a background process is still running
///
/// @param process_id - ID returned from spawnBackgroundProcess
/// @returns true if process is running, false if not found or exited
#[op2(fast)]
#[allow(clippy::result_unit_err)]
fn op_fresh_is_process_running(state: &mut OpState, #[bigint] process_id: u64) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let mut processes = runtime_state.background_processes.borrow_mut();

        if let Some(child) = processes.get_mut(&process_id) {
            // Try to check if the process has exited
            match child.try_wait() {
                Ok(Some(_)) => {
                    // Process has exited, remove it
                    processes.remove(&process_id);
                    false
                }
                Ok(None) => {
                    // Process is still running
                    true
                }
                Err(_) => {
                    // Error checking status, assume not running
                    processes.remove(&process_id);
                    false
                }
            }
        } else {
            false
        }
    } else {
        false
    }
}

/// Start a cancellable process and return its ID immediately
///
/// Unlike spawnProcess which waits for completion, this starts output collection
/// in the background and returns immediately with a process ID.
/// Use spawnProcessWait(id) to get the result, or killProcess(id) to cancel.
///
/// @param command - Program name (searched in PATH) or absolute path
/// @param args - Command arguments (each array element is one argument)
/// @param cwd - Working directory; null uses editor's cwd
/// @returns Process ID for later reference
#[op2(async)]
#[bigint]
async fn op_fresh_spawn_process_start(
    state: Rc<RefCell<OpState>>,
    #[string] command: String,
    #[serde] args: Vec<String>,
    #[string] cwd: Option<String>,
) -> Result<u64, JsErrorBox> {
    use std::process::Stdio;
    use tokio::io::{AsyncBufReadExt, BufReader};
    use tokio::process::Command;

    let spawn_start = std::time::Instant::now();
    tracing::trace!(
        command = %command,
        args = ?args,
        cwd = ?cwd,
        "spawn_process_start called"
    );

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
        .map_err(|e| JsErrorBox::generic(format!("Failed to spawn process: {}", e)))?;

    // Get the OS PID for kill-by-pid (needed because spawn_process_wait takes ownership)
    let os_pid = child.id();

    tracing::trace!(
        command = %command,
        os_pid = ?os_pid,
        spawn_ms = spawn_start.elapsed().as_micros(),
        "process spawned"
    );

    // Take stdout and stderr handles
    let stdout_handle = child.stdout.take();
    let stderr_handle = child.stderr.take();

    // Create a oneshot channel for the output
    let (tx, rx) = tokio::sync::oneshot::channel();

    // Spawn a task to collect output
    tokio::spawn(async move {
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

        let (stdout, stderr) = tokio::join!(stdout_future, stderr_future);
        let _ = tx.send((stdout, stderr));
    });

    // Store the process and get its ID
    let process_id = {
        let op_state = state.borrow();
        if let Some(runtime_state) = op_state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
            let runtime_state = runtime_state.borrow();
            let mut id = runtime_state.next_process_id.borrow_mut();
            let process_id = *id;
            *id += 1;
            drop(id);

            runtime_state.cancellable_processes.borrow_mut().insert(
                process_id,
                CancellableProcess {
                    child,
                    output_rx: rx,
                },
            );

            // Store OS PID separately for kill-by-pid
            // (needed because spawn_process_wait takes ownership of Child)
            if let Some(pid) = os_pid {
                runtime_state
                    .process_pids
                    .borrow_mut()
                    .insert(process_id, pid);
            }

            process_id
        } else {
            return Err(JsErrorBox::generic("Runtime state not available"));
        }
    };

    Ok(process_id)
}

/// Wait for a cancellable process to complete and get its result
///
/// @param process_id - ID returned from spawnProcessStart
/// @returns SpawnResult with stdout, stderr, and exit_code
#[op2(async)]
#[serde]
async fn op_fresh_spawn_process_wait(
    state: Rc<RefCell<OpState>>,
    #[bigint] process_id: u64,
) -> Result<SpawnResult, JsErrorBox> {
    let wait_start = std::time::Instant::now();
    tracing::trace!(process_id, "spawn_process_wait called");

    // Take the process from the map
    let process_opt = {
        let op_state = state.borrow();
        if let Some(runtime_state) = op_state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
            let runtime_state = runtime_state.borrow();
            let result = runtime_state
                .cancellable_processes
                .borrow_mut()
                .remove(&process_id);
            result
        } else {
            return Err(JsErrorBox::generic("Runtime state not available"));
        }
    };

    let Some(mut process) = process_opt else {
        tracing::trace!(process_id, "process not found");
        return Err(JsErrorBox::generic(format!(
            "Process {} not found (already completed or killed)",
            process_id
        )));
    };

    // Wait for the process to complete
    tracing::trace!(process_id, "waiting for process...");
    let exit_code = match process.child.wait().await {
        Ok(status) => status.code().unwrap_or(-1),
        Err(_) => -1,
    };
    tracing::trace!(
        process_id,
        exit_code,
        wait_ms = wait_start.elapsed().as_millis(),
        "process exited"
    );

    // Get the collected output
    let (stdout, stderr) = process
        .output_rx
        .await
        .unwrap_or_else(|_| (String::new(), String::new()));

    // Clean up process_pids entry (if kill_process hasn't already)
    {
        let op_state = state.borrow();
        if let Some(runtime_state) = op_state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
            let runtime_state = runtime_state.borrow();
            runtime_state.process_pids.borrow_mut().remove(&process_id);
        }
    }

    tracing::trace!(
        process_id,
        exit_code,
        stdout_len = stdout.len(),
        stderr_len = stderr.len(),
        total_ms = wait_start.elapsed().as_millis(),
        "spawn_process_wait completed"
    );

    Ok(SpawnResult {
        stdout,
        stderr,
        exit_code,
    })
}

/// Delay execution for a specified number of milliseconds
///
/// Useful for debouncing user input or adding delays between operations.
/// @param ms - Number of milliseconds to delay
/// @example
/// await editor.delay(100);  // Wait 100ms
#[op2(async)]
async fn op_fresh_delay(#[bigint] ms: u64) -> Result<(), JsErrorBox> {
    tokio::time::sleep(std::time::Duration::from_millis(ms)).await;
    Ok(())
}

/// Subscribe to an editor event
///
/// Handler must be a global function name (not a closure).
/// Multiple handlers can be registered for the same event.
/// Events: "buffer_save", "cursor_moved", "buffer_modified", etc.
/// @param event_name - Event to subscribe to
/// @param handler_name - Name of globalThis function to call with event data
/// @example
/// globalThis.onSave = (data) => {
///   editor.setStatus(`Saved: ${data.path}`);
/// };
/// editor.on("buffer_save", "onSave");
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
        tracing::debug!(
            "Registered event handler '{}' for '{}'",
            handler_name,
            event_name
        );
        return true;
    }
    false
}

/// Unregister an event handler
/// @param event_name - Name of the event
/// @param handler_name - Name of the handler to remove
/// @returns true if handler was found and removed
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
                tracing::debug!(
                    "Unregistered event handler '{}' from '{}'",
                    handler_name,
                    event_name
                );
                return true;
            }
        }
    }
    false
}

/// Get list of registered handlers for an event
/// @param event_name - Name of the event
/// @returns Array of handler function names
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
    /// Whether the path exists
    exists: bool,
    /// Whether the path is a file
    is_file: bool,
    /// Whether the path is a directory
    is_dir: bool,
    /// File size in bytes
    size: u64,
    /// Whether the file is read-only
    readonly: bool,
}

/// Buffer information
#[derive(serde::Serialize)]
struct TsBufferInfo {
    /// Unique buffer ID
    id: u32,
    /// File path (empty string if no path)
    path: String,
    /// Whether buffer has unsaved changes
    modified: bool,
    /// Buffer length in bytes
    length: u32,
}

/// Diff vs last save for a buffer
#[derive(serde::Serialize)]
struct TsBufferSavedDiff {
    equal: bool,
    byte_ranges: Vec<(u32, u32)>,
    line_ranges: Option<Vec<(u32, u32)>>,
}

// Re-export TsHighlightSpan from api module for backwards compatibility
pub use crate::services::plugins::api::TsHighlightSpan;

/// Compute syntax highlighting for a buffer range
#[op2(async)]
#[serde]
async fn op_fresh_get_highlights(
    state: Rc<RefCell<OpState>>,
    buffer_id: u32,
    start: u32,
    end: u32,
) -> Result<Vec<TsHighlightSpan>, JsErrorBox> {
    let (tx, rx) = tokio::sync::oneshot::channel();
    let request_id = {
        let op_state = state.borrow();
        let runtime_state = op_state.borrow::<Rc<RefCell<TsRuntimeState>>>().borrow();
        let mut id_ref = runtime_state.next_request_id.borrow_mut();
        let id = *id_ref;
        *id_ref += 1;

        runtime_state
            .pending_responses
            .lock()
            .unwrap()
            .insert(id, tx);

        let _ = runtime_state
            .command_sender
            .send(PluginCommand::RequestHighlights {
                buffer_id: BufferId(buffer_id as usize),
                range: (start as usize)..(end as usize),
                request_id: id,
            });
        id
    };

    match rx.await {
        Ok(crate::services::plugins::api::PluginResponse::HighlightsComputed { spans, .. }) => {
            Ok(spans)
        }
        _ => Err(JsErrorBox::generic(format!(
            "Failed to get highlights for request {}",
            request_id
        ))),
    }
}

/// Find a buffer ID by its file path
#[op2(fast)]
fn op_fresh_find_buffer_by_path(state: &mut OpState, #[string] path: String) -> u32 {
    let runtime_state = state.borrow::<Rc<RefCell<TsRuntimeState>>>().borrow();
    if let Ok(snapshot) = runtime_state.state_snapshot.read() {
        let target_path = std::path::PathBuf::from(path);
        for (id, info) in &snapshot.buffers {
            if let Some(ref buffer_path) = info.path {
                if *buffer_path == target_path {
                    return id.0 as u32;
                }
            }
        }
    }
    0
}

/// Selection range
#[derive(serde::Serialize)]
struct TsSelectionRange {
    /// Start byte position
    start: u32,
    /// End byte position
    end: u32,
}

/// Cursor information with optional selection
#[derive(serde::Serialize)]
struct TsCursorInfo {
    /// Byte position of the cursor
    position: u32,
    /// Selection range if text is selected, null otherwise
    selection: Option<TsSelectionRange>,
}

/// LSP diagnostic position
#[derive(serde::Serialize)]
struct TsDiagnosticPosition {
    line: u32,
    character: u32,
}

/// LSP diagnostic range
#[derive(serde::Serialize)]
struct TsDiagnosticRange {
    start: TsDiagnosticPosition,
    end: TsDiagnosticPosition,
}

/// LSP diagnostic item for TypeScript plugins
#[derive(serde::Serialize)]
struct TsDiagnostic {
    /// File URI (e.g., "file:///path/to/file.rs")
    uri: String,
    /// Diagnostic severity: 1=Error, 2=Warning, 3=Info, 4=Hint
    severity: u8,
    /// Diagnostic message
    message: String,
    /// Source of the diagnostic (e.g., "rust-analyzer")
    source: Option<String>,
    /// Location range in the file
    range: TsDiagnosticRange,
}

/// Viewport information
#[derive(serde::Serialize)]
struct TsViewportInfo {
    /// Byte offset of the top-left visible position
    top_byte: u32,
    /// Column offset for horizontal scrolling
    left_column: u32,
    /// Viewport width in columns
    width: u32,
    /// Viewport height in rows
    height: u32,
}

/// Get full information about a buffer
/// @param buffer_id - Buffer ID
/// @returns BufferInfo object or null if buffer not found
#[op2]
#[serde]
fn op_fresh_get_buffer_info(state: &mut OpState, buffer_id: u32) -> Option<TsBufferInfo> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            if let Some(info) = snapshot.buffers.get(&BufferId(buffer_id as usize)) {
                return Some(TsBufferInfo {
                    id: info.id.0 as u32,
                    path: info
                        .path
                        .as_ref()
                        .map(|p| p.to_string_lossy().to_string())
                        .unwrap_or_default(),
                    modified: info.modified,
                    length: info.length as u32,
                });
            }
        };
    }
    None
}

/// Get diff vs last saved snapshot for a buffer
#[op2]
#[serde]
fn op_fresh_get_buffer_saved_diff(
    state: &mut OpState,
    buffer_id: u32,
) -> Option<TsBufferSavedDiff> {
    let runtime_state = state
        .try_borrow::<Rc<RefCell<TsRuntimeState>>>()?
        .borrow()
        .state_snapshot
        .read()
        .ok()?
        .buffer_saved_diffs
        .get(&BufferId(buffer_id as usize))
        .cloned()?;

    Some(TsBufferSavedDiff {
        equal: runtime_state.equal,
        byte_ranges: runtime_state
            .byte_ranges
            .iter()
            .map(|r| (r.start as u32, r.end as u32))
            .collect(),
        line_ranges: runtime_state.line_ranges.as_ref().map(|ranges| {
            ranges
                .iter()
                .map(|r| (r.start as u32, r.end as u32))
                .collect()
        }),
    })
}

/// List all open buffers
/// @returns Array of BufferInfo objects
#[op2]
#[serde]
fn op_fresh_list_buffers(state: &mut OpState) -> Vec<TsBufferInfo> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot
                .buffers
                .values()
                .map(|info| TsBufferInfo {
                    id: info.id.0 as u32,
                    path: info
                        .path
                        .as_ref()
                        .map(|p| p.to_string_lossy().to_string())
                        .unwrap_or_default(),
                    modified: info.modified,
                    length: info.length as u32,
                })
                .collect();
        };
    }
    Vec::new()
}

/// Get all LSP diagnostics across all files
/// @returns Array of Diagnostic objects with file URI, severity, message, and range
#[op2]
#[serde]
fn op_fresh_get_all_diagnostics(state: &mut OpState) -> Vec<TsDiagnostic> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            let mut result = Vec::new();
            for (uri, diagnostics) in &snapshot.diagnostics {
                for diag in diagnostics {
                    let severity = match diag.severity {
                        Some(lsp_types::DiagnosticSeverity::ERROR) => 1,
                        Some(lsp_types::DiagnosticSeverity::WARNING) => 2,
                        Some(lsp_types::DiagnosticSeverity::INFORMATION) => 3,
                        Some(lsp_types::DiagnosticSeverity::HINT) => 4,
                        _ => 0,
                    };
                    result.push(TsDiagnostic {
                        uri: uri.clone(),
                        severity,
                        message: diag.message.clone(),
                        source: diag.source.clone(),
                        range: TsDiagnosticRange {
                            start: TsDiagnosticPosition {
                                line: diag.range.start.line,
                                character: diag.range.start.character,
                            },
                            end: TsDiagnosticPosition {
                                line: diag.range.end.line,
                                character: diag.range.end.character,
                            },
                        },
                    });
                }
            }
            return result;
        };
    }
    Vec::new()
}

/// Get primary cursor with selection info
/// @returns CursorInfo object or null if no cursor
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
/// @returns Array of CursorInfo objects
#[op2]
#[serde]
fn op_fresh_get_all_cursors(state: &mut OpState) -> Vec<TsCursorInfo> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot
                .all_cursors
                .iter()
                .map(|cursor| TsCursorInfo {
                    position: cursor.position as u32,
                    selection: cursor.selection.as_ref().map(|sel| TsSelectionRange {
                        start: sel.start as u32,
                        end: sel.end as u32,
                    }),
                })
                .collect();
        };
    }
    Vec::new()
}

/// Get viewport information
/// @returns ViewportInfo object or null if no viewport
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

/// Suggestion for prompt autocomplete
#[derive(serde::Deserialize)]
struct TsSuggestion {
    /// Display text for the suggestion
    text: String,
    /// Optional description shown alongside
    description: Option<String>,
    /// Optional value to use instead of text when selected
    value: Option<String>,
    /// Whether the suggestion is disabled
    disabled: Option<bool>,
    /// Optional keybinding hint
    keybinding: Option<String>,
}

/// Start an interactive prompt
/// @param label - Label to display (e.g., "Git grep: ")
/// @param prompt_type - Type identifier (e.g., "git-grep")
/// @returns true if prompt was started successfully
#[op2(fast)]
fn op_fresh_start_prompt(
    state: &mut OpState,
    #[string] label: String,
    #[string] prompt_type: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::StartPrompt { label, prompt_type });
        return result.is_ok();
    }
    false
}

/// Start a prompt with pre-filled initial value
/// @param label - Label to display (e.g., "Git grep: ")
/// @param prompt_type - Type identifier (e.g., "git-grep")
/// @param initial_value - Initial text to pre-fill in the prompt
/// @returns true if prompt was started successfully
#[op2(fast)]
fn op_fresh_start_prompt_with_initial(
    state: &mut OpState,
    #[string] label: String,
    #[string] prompt_type: String,
    #[string] initial_value: String,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::StartPromptWithInitial {
                label,
                prompt_type,
                initial_value,
            });
        return result.is_ok();
    }
    false
}

/// Set suggestions for the current prompt
/// @param suggestions - Array of suggestions to display
/// @returns true if suggestions were set successfully
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
                source: None,
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

/// Read entire file contents as UTF-8 string
///
/// Throws if file doesn't exist, isn't readable, or isn't valid UTF-8.
/// For binary files, this will fail. For large files, consider memory usage.
/// @param path - File path (absolute or relative to cwd)
#[op2(async)]
#[string]
async fn op_fresh_read_file(#[string] path: String) -> Result<String, JsErrorBox> {
    tokio::fs::read_to_string(&path)
        .await
        .map_err(|e| JsErrorBox::generic(format!("Failed to read file {}: {}", path, e)))
}

/// Write string content to a NEW file (fails if file exists)
///
/// Creates a new file with the given content. Fails if the file already exists
/// to prevent plugins from accidentally overwriting user data.
/// @param path - Destination path (absolute or relative to cwd)
/// @param content - UTF-8 string to write
#[op2(async)]
async fn op_fresh_write_file(
    #[string] path: String,
    #[string] content: String,
) -> Result<(), JsErrorBox> {
    use tokio::fs::OpenOptions;
    use tokio::io::AsyncWriteExt;

    // Check if file already exists
    if std::path::Path::new(&path).exists() {
        return Err(JsErrorBox::generic(format!(
            "File already exists: {}. Use a different path or delete the file first.",
            path
        )));
    }

    // Create parent directories if needed
    if let Some(parent) = std::path::Path::new(&path).parent() {
        if !parent.exists() {
            tokio::fs::create_dir_all(parent).await.map_err(|e| {
                JsErrorBox::generic(format!("Failed to create parent directories: {}", e))
            })?;
        }
    }

    // Create and write the file (create_new ensures atomic check)
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .await
        .map_err(|e| JsErrorBox::generic(format!("Failed to create file {}: {}", path, e)))?;

    file.write_all(content.as_bytes())
        .await
        .map_err(|e| JsErrorBox::generic(format!("Failed to write file {}: {}", path, e)))
}

/// Delete a theme file by name
///
/// Only deletes files from the user's themes directory.
/// This is a safe operation that prevents plugins from deleting arbitrary files.
/// @param name - Theme name (without .json extension)
#[op2(async)]
async fn op_fresh_delete_theme(
    state: Rc<RefCell<OpState>>,
    #[string] name: String,
) -> Result<(), JsErrorBox> {
    // Get the themes directory from runtime state
    let themes_dir = {
        let state = state.borrow();
        if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
            let runtime_state = runtime_state.borrow();
            runtime_state.dir_context.themes_dir()
        } else {
            return Err(JsErrorBox::generic("Failed to get themes directory"));
        }
    };

    // Construct the theme file path
    let theme_path = themes_dir.join(format!("{}.json", name));

    // Check if theme file exists first
    if !theme_path.exists() {
        return Err(JsErrorBox::generic(format!(
            "Theme '{}' does not exist",
            name
        )));
    }

    // Security check: ensure the resolved path is actually inside the themes directory
    let canonical_themes_dir = themes_dir
        .canonicalize()
        .map_err(|e| JsErrorBox::generic(format!("Failed to resolve themes directory: {}", e)))?;

    let canonical_theme_path = theme_path
        .canonicalize()
        .map_err(|e| JsErrorBox::generic(format!("Failed to resolve theme path: {}", e)))?;

    if !canonical_theme_path.starts_with(&canonical_themes_dir) {
        return Err(JsErrorBox::generic(
            "Security error: theme path is outside themes directory".to_string(),
        ));
    }

    // Move the theme file to trash (safer than permanent deletion)
    let theme_name = name.clone();
    tokio::task::spawn_blocking(move || {
        trash::delete(&canonical_theme_path).map_err(|e| {
            JsErrorBox::generic(format!("Failed to delete theme '{}': {}", theme_name, e))
        })
    })
    .await
    .map_err(|e| JsErrorBox::generic(format!("Failed to delete theme '{}': {}", name, e)))?
}

/// Check if a path exists (file, directory, or symlink)
///
/// Does not follow symlinks; returns true for broken symlinks.
/// Use fileStat for more detailed information.
/// @param path - Path to check (absolute or relative to cwd)
#[op2(fast)]
fn op_fresh_file_exists(#[string] path: String) -> bool {
    std::path::Path::new(&path).exists()
}

/// Get metadata about a file or directory
///
/// Follows symlinks. Returns exists=false for non-existent paths
/// rather than throwing. Size is in bytes; directories may report 0.
/// @param path - Path to stat (absolute or relative to cwd)
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
/// @param name - Name of environment variable
/// @returns Value if set, null if not set
#[op2]
#[string]
fn op_fresh_get_env(#[string] name: String) -> Option<String> {
    std::env::var(&name).ok()
}

/// Get the editor's current working directory
///
/// Returns the editor's working directory (set when the editor was started).
/// Use as base for resolving relative paths and spawning processes.
/// Note: This returns the editor's stored working_dir, not process CWD,
/// which is important for test isolation.
#[op2]
#[string]
fn op_fresh_get_cwd(state: &mut OpState) -> String {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot.working_dir.to_string_lossy().to_string();
        };
    }
    // Fallback to process cwd if state not available (should not happen in practice)
    std::env::current_dir()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| ".".to_string())
}

/// Join path segments using the OS path separator
///
/// Handles empty segments and normalizes separators.
/// If a segment is absolute, previous segments are discarded.
/// @param parts - Path segments to join
/// @example
/// pathJoin("/home", "user", "file.txt") // "/home/user/file.txt"
/// pathJoin("relative", "/absolute") // "/absolute"
#[op2]
#[string]
fn op_fresh_path_join(#[serde] parts: Vec<String>) -> String {
    let mut path = std::path::PathBuf::new();
    for part in parts {
        path.push(part);
    }
    path.to_string_lossy().to_string()
}

/// Get the parent directory of a path
///
/// Returns empty string for root paths or paths without parent.
/// Does not resolve symlinks or check existence.
/// @param path - File or directory path
/// @example
/// pathDirname("/home/user/file.txt") // "/home/user"
/// pathDirname("/") // ""
#[op2]
#[string]
fn op_fresh_path_dirname(#[string] path: String) -> String {
    std::path::Path::new(&path)
        .parent()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_default()
}

/// Get the final component of a path
///
/// Returns empty string for root paths.
/// Does not strip file extension; use pathExtname for that.
/// @param path - File or directory path
/// @example
/// pathBasename("/home/user/file.txt") // "file.txt"
/// pathBasename("/home/user/") // "user"
#[op2]
#[string]
fn op_fresh_path_basename(#[string] path: String) -> String {
    std::path::Path::new(&path)
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_default()
}

/// Get the file extension including the dot
///
/// Returns empty string if no extension. Only returns the last extension
/// for files like "archive.tar.gz" (returns ".gz").
/// @param path - File path
/// @example
/// pathExtname("file.txt") // ".txt"
/// pathExtname("archive.tar.gz") // ".gz"
/// pathExtname("Makefile") // ""
#[op2]
#[string]
fn op_fresh_path_extname(#[string] path: String) -> String {
    std::path::Path::new(&path)
        .extension()
        .map(|e| format!(".{}", e.to_string_lossy()))
        .unwrap_or_default()
}

/// Check if a path is absolute
///
/// On Unix: starts with "/". On Windows: starts with drive letter or UNC path.
/// @param path - Path to check
#[op2(fast)]
fn op_fresh_path_is_absolute(#[string] path: String) -> bool {
    std::path::Path::new(&path).is_absolute()
}

/// Directory entry from readDir
#[derive(serde::Serialize)]
struct DirEntry {
    /// Entry name only (not full path). Join with parent path to get absolute path.
    name: String,
    /// True if entry is a regular file
    is_file: bool,
    /// True if entry is a directory. Note: symlinks report the target type.
    is_dir: bool,
}

/// List directory contents
///
/// Returns unsorted entries with type info. Entry names are relative
/// to the directory (use pathJoin to construct full paths).
/// Throws on permission errors or if path is not a directory.
/// @param path - Directory path (absolute or relative to cwd)
/// @example
/// const entries = editor.readDir("/home/user");
/// for (const e of entries) {
///   const fullPath = editor.pathJoin("/home/user", e.name);
/// }
#[op2]
#[serde]
fn op_fresh_read_dir(
    state: &mut OpState,
    #[string] path: String,
) -> Result<Vec<DirEntry>, JsErrorBox> {
    // Resolve relative paths against the editor's working directory
    let resolved_path = if std::path::Path::new(&path).is_absolute() {
        std::path::PathBuf::from(&path)
    } else {
        // Try to get the working directory from the editor state
        let working_dir =
            state
                .try_borrow::<Rc<RefCell<TsRuntimeState>>>()
                .and_then(|runtime_state| {
                    let runtime_state = runtime_state.borrow();
                    runtime_state
                        .state_snapshot
                        .read()
                        .ok()
                        .map(|snapshot| snapshot.working_dir.clone())
                });

        if let Some(wd) = working_dir {
            wd.join(&path)
        } else {
            std::path::PathBuf::from(&path)
        }
    };

    let entries = std::fs::read_dir(&resolved_path)
        .map_err(|e| JsErrorBox::generic(format!("Failed to read directory {}: {}", path, e)))?;

    let mut result = Vec::new();
    for entry in entries {
        let entry = entry
            .map_err(|e| JsErrorBox::generic(format!("Failed to read directory entry: {}", e)))?;

        let metadata = entry
            .metadata()
            .map_err(|e| JsErrorBox::generic(format!("Failed to get entry metadata: {}", e)))?;

        result.push(DirEntry {
            name: entry.file_name().to_string_lossy().to_string(),
            is_file: metadata.is_file(),
            is_dir: metadata.is_dir(),
        });
    }

    Ok(result)
}

// === Virtual Buffer Operations ===

/// Entry for virtual buffer content with embedded metadata
#[derive(serde::Deserialize)]
struct TsTextPropertyEntry {
    /// Text to display. Include trailing newline for separate lines.
    text: String,
    /// Arbitrary metadata queryable via getTextPropertiesAtCursor.
    /// Common: { file: string, line: number, type: string }
    properties: std::collections::HashMap<String, serde_json::Value>,
}

/// Result from createVirtualBufferInSplit
#[derive(serde::Serialize)]
struct CreateVirtualBufferResult {
    buffer_id: u32,
    split_id: Option<u32>,
}

/// Configuration for createVirtualBufferInSplit
#[derive(serde::Deserialize)]
struct CreateVirtualBufferOptions {
    /// Buffer name shown in status bar (convention: "*Name*")
    name: String,
    /// Mode for keybindings; define with defineMode first
    mode: String,
    /// Prevent text modifications
    read_only: bool,
    /// Content with embedded metadata
    entries: Vec<TsTextPropertyEntry>,
    /// Split ratio (0.3 = new pane gets 30% of space)
    ratio: f32,
    /// Split direction: "horizontal" (below) or "vertical" (side-by-side). Default: horizontal
    direction: Option<String>,
    /// If set and panel exists, update content instead of creating new buffer
    panel_id: Option<String>,
    /// Show line numbers gutter (default: true)
    show_line_numbers: Option<bool>,
    /// Show cursor in buffer (default: true)
    show_cursors: Option<bool>,
    /// Disable all editing commands (default: false)
    editing_disabled: Option<bool>,
    /// Enable/disable line wrapping (None = use global setting)
    line_wrap: Option<bool>,
}

/// Create a virtual buffer in a new horizontal split below current pane
///
/// Use for results panels, diagnostics, logs, etc. The panel_id enables
/// idempotent updates: if a panel with that ID exists, its content is replaced
/// instead of creating a new split. Define the mode with defineMode first.
/// @param options - Buffer configuration
/// @example
/// // First define the mode with keybindings
/// editor.defineMode("search-results", "special", [
///   ["Return", "search_goto"],
///   ["q", "close_buffer"]
/// ], true);
///
/// // Then create the buffer
/// const id = await editor.createVirtualBufferInSplit({
///   name: "*Search*",
///   mode: "search-results",
///   read_only: true,
///   entries: [
///     { text: "src/main.rs:42: match\n", properties: { file: "src/main.rs", line: 42 } }
///   ],
///   ratio: 0.3,
///   panel_id: "search"
/// });
#[op2(async)]
#[serde]
async fn op_fresh_create_virtual_buffer_in_split(
    state: Rc<RefCell<OpState>>,
    #[serde] options: CreateVirtualBufferOptions,
) -> Result<CreateVirtualBufferResult, JsErrorBox> {
    // Get runtime state and create oneshot channel
    let receiver = {
        let state = state.borrow();
        let runtime_state = state
            .try_borrow::<Rc<RefCell<TsRuntimeState>>>()
            .ok_or_else(|| JsErrorBox::generic("Failed to get runtime state"))?;
        let runtime_state = runtime_state.borrow();

        // Allocate request ID
        let request_id = {
            let mut id = runtime_state.next_request_id.borrow_mut();
            let current = *id;
            *id += 1;
            current
        };

        // Create oneshot channel for response
        let (tx, rx) = tokio::sync::oneshot::channel();

        // Store the sender
        {
            let mut pending = runtime_state.pending_responses.lock().unwrap();
            pending.insert(request_id, tx);
        }

        // Convert TypeScript entries to Rust TextPropertyEntry
        let entries: Vec<crate::primitives::text_property::TextPropertyEntry> = options
            .entries
            .into_iter()
            .map(|e| crate::primitives::text_property::TextPropertyEntry {
                text: e.text,
                properties: e.properties,
            })
            .collect();

        // Send command with request_id
        tracing::trace!(
            "op_create_virtual_buffer_in_split: sending command with request_id={}",
            request_id
        );
        runtime_state
            .command_sender
            .send(PluginCommand::CreateVirtualBufferInSplit {
                name: options.name,
                mode: options.mode,
                read_only: options.read_only,
                entries,
                ratio: options.ratio,
                direction: options.direction,
                panel_id: options.panel_id,
                show_line_numbers: options.show_line_numbers.unwrap_or(true),
                show_cursors: options.show_cursors.unwrap_or(true),
                editing_disabled: options.editing_disabled.unwrap_or(false),
                line_wrap: options.line_wrap,
                request_id: Some(request_id),
            })
            .map_err(|_| JsErrorBox::generic("Failed to send command"))?;
        tracing::trace!("op_create_virtual_buffer_in_split: command sent, waiting for response");

        rx
    };

    // Wait for response
    let response = receiver
        .await
        .map_err(|_| JsErrorBox::generic("Response channel closed"))?;

    // Extract buffer ID and split ID from response
    match response {
        crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
            buffer_id,
            split_id,
            ..
        } => Ok(CreateVirtualBufferResult {
            buffer_id: buffer_id.0 as u32,
            split_id: split_id.map(|s| s.0 as u32),
        }),
        _ => Err(JsErrorBox::generic(
            "Unexpected plugin response for virtual buffer creation",
        )),
    }
}

/// Options for creating a virtual buffer in an existing split
#[derive(serde::Deserialize)]
struct CreateVirtualBufferInExistingSplitOptions {
    /// Display name (e.g., "*Commit Details*")
    name: String,
    /// Mode name for buffer-local keybindings
    mode: String,
    /// Whether the buffer is read-only
    read_only: bool,
    /// Entries with text and embedded properties
    entries: Vec<TsTextPropertyEntry>,
    /// Target split ID where the buffer should be displayed
    split_id: u32,
    /// Whether to show line numbers in the buffer (default true)
    show_line_numbers: Option<bool>,
    /// Whether to show cursors in the buffer (default true)
    show_cursors: Option<bool>,
    /// Whether editing is disabled for this buffer (default false)
    editing_disabled: Option<bool>,
    /// Enable/disable line wrapping (None = use global setting)
    line_wrap: Option<bool>,
}

/// Create a virtual buffer in an existing split
/// @param options - Configuration for the virtual buffer
/// @returns Promise resolving to the buffer ID of the created virtual buffer
#[op2(async)]
async fn op_fresh_create_virtual_buffer_in_existing_split(
    state: Rc<RefCell<OpState>>,
    #[serde] options: CreateVirtualBufferInExistingSplitOptions,
) -> Result<u32, JsErrorBox> {
    // Get runtime state and create oneshot channel
    let receiver = {
        let state = state.borrow();
        let runtime_state = state
            .try_borrow::<Rc<RefCell<TsRuntimeState>>>()
            .ok_or_else(|| JsErrorBox::generic("Failed to get runtime state"))?;
        let runtime_state = runtime_state.borrow();

        // Allocate request ID
        let request_id = {
            let mut id = runtime_state.next_request_id.borrow_mut();
            let current = *id;
            *id += 1;
            current
        };

        // Create oneshot channel for response
        let (tx, rx) = tokio::sync::oneshot::channel();

        // Store the sender
        {
            let mut pending = runtime_state.pending_responses.lock().unwrap();
            pending.insert(request_id, tx);
        }

        // Convert TypeScript entries to Rust TextPropertyEntry
        let entries: Vec<crate::primitives::text_property::TextPropertyEntry> = options
            .entries
            .into_iter()
            .map(|e| crate::primitives::text_property::TextPropertyEntry {
                text: e.text,
                properties: e.properties,
            })
            .collect();

        // Send command with request_id
        runtime_state
            .command_sender
            .send(PluginCommand::CreateVirtualBufferInExistingSplit {
                name: options.name,
                mode: options.mode,
                read_only: options.read_only,
                entries,
                split_id: crate::model::event::SplitId(options.split_id as usize),
                show_line_numbers: options.show_line_numbers.unwrap_or(true),
                show_cursors: options.show_cursors.unwrap_or(true),
                editing_disabled: options.editing_disabled.unwrap_or(false),
                line_wrap: options.line_wrap,
                request_id: Some(request_id),
            })
            .map_err(|_| JsErrorBox::generic("Failed to send command"))?;

        rx
    };

    // Wait for response
    let response = receiver
        .await
        .map_err(|_| JsErrorBox::generic("Response channel closed"))?;

    // Extract buffer ID from response
    match response {
        crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
            buffer_id, ..
        } => Ok(buffer_id.0 as u32),
        _ => Err(JsErrorBox::generic(
            "Unexpected plugin response for virtual buffer creation",
        )),
    }
}

/// Options for creating a virtual buffer in the current split as a new tab
#[derive(serde::Deserialize)]
struct CreateVirtualBufferInCurrentSplitOptions {
    /// Display name (e.g., "*Help*")
    name: String,
    /// Mode name for buffer-local keybindings
    mode: String,
    /// Whether the buffer is read-only
    read_only: bool,
    /// Entries with text and embedded properties
    entries: Vec<TsTextPropertyEntry>,
    /// Whether to show line numbers in the buffer (default false for help/docs)
    show_line_numbers: Option<bool>,
    /// Whether to show cursors in the buffer (default true)
    show_cursors: Option<bool>,
    /// Whether editing is disabled for this buffer (default false)
    editing_disabled: Option<bool>,
    /// Whether this buffer should be hidden from tabs (for composite source buffers)
    hidden_from_tabs: Option<bool>,
}

/// Create a virtual buffer in the current split as a new tab
/// This is useful for help panels, documentation, etc. that should open
/// alongside other buffers rather than in a separate split.
/// @param options - Configuration for the virtual buffer
/// @returns Promise resolving to the buffer ID of the created virtual buffer
#[op2(async)]
async fn op_fresh_create_virtual_buffer(
    state: Rc<RefCell<OpState>>,
    #[serde] options: CreateVirtualBufferInCurrentSplitOptions,
) -> Result<u32, JsErrorBox> {
    // Get runtime state and create oneshot channel
    let receiver = {
        let state = state.borrow();
        let runtime_state = state
            .try_borrow::<Rc<RefCell<TsRuntimeState>>>()
            .ok_or_else(|| JsErrorBox::generic("Failed to get runtime state"))?;
        let runtime_state = runtime_state.borrow();

        // Allocate request ID
        let request_id = {
            let mut id = runtime_state.next_request_id.borrow_mut();
            let current = *id;
            *id += 1;
            current
        };

        // Create oneshot channel for response
        let (tx, rx) = tokio::sync::oneshot::channel();

        // Store the sender
        {
            let mut pending = runtime_state.pending_responses.lock().unwrap();
            pending.insert(request_id, tx);
        }

        // Convert TypeScript entries to Rust TextPropertyEntry
        let entries: Vec<crate::primitives::text_property::TextPropertyEntry> = options
            .entries
            .into_iter()
            .map(|e| crate::primitives::text_property::TextPropertyEntry {
                text: e.text,
                properties: e.properties,
            })
            .collect();

        // Send command with request_id
        runtime_state
            .command_sender
            .send(PluginCommand::CreateVirtualBufferWithContent {
                name: options.name,
                mode: options.mode,
                read_only: options.read_only,
                entries,
                show_line_numbers: options.show_line_numbers.unwrap_or(false),
                show_cursors: options.show_cursors.unwrap_or(true),
                editing_disabled: options.editing_disabled.unwrap_or(false),
                hidden_from_tabs: options.hidden_from_tabs.unwrap_or(false),
                request_id: Some(request_id),
            })
            .map_err(|_| JsErrorBox::generic("Failed to send command"))?;

        rx
    };

    // Wait for response
    let response = receiver
        .await
        .map_err(|_| JsErrorBox::generic("Response channel closed"))?;

    // Extract buffer ID from response
    match response {
        crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
            buffer_id, ..
        } => Ok(buffer_id.0 as u32),
        _ => Err(JsErrorBox::generic(
            "Unexpected plugin response for virtual buffer creation",
        )),
    }
}

// =============================================================================
// Composite Buffer Operations
// =============================================================================

/// Layout configuration for composite buffers
#[derive(serde::Deserialize)]
struct TsCompositeLayoutConfig {
    /// Layout type: "side-by-side", "stacked", or "unified"
    layout_type: String,
    /// Relative widths for side-by-side layout (e.g., [0.5, 0.5])
    ratios: Option<Vec<f32>>,
    /// Show separator between panes
    show_separator: Option<bool>,
    /// Spacing between stacked panes
    spacing: Option<u16>,
}

/// Pane style configuration
#[derive(serde::Deserialize)]
struct TsCompositePaneStyle {
    /// Background color for added lines (RGB tuple)
    add_bg: Option<(u8, u8, u8)>,
    /// Background color for removed lines (RGB tuple)
    remove_bg: Option<(u8, u8, u8)>,
    /// Background color for modified lines (RGB tuple)
    modify_bg: Option<(u8, u8, u8)>,
    /// Gutter style: "line-numbers", "diff-markers", "both", "none"
    gutter_style: Option<String>,
}

/// Source pane configuration for composite buffers
#[derive(serde::Deserialize)]
struct TsCompositeSourceConfig {
    /// Buffer ID to display in this pane
    buffer_id: u32,
    /// Label for the pane (shown in header)
    label: Option<String>,
    /// Whether the pane is editable
    editable: bool,
    /// Pane styling options
    style: Option<TsCompositePaneStyle>,
}

/// Diff hunk configuration
#[derive(serde::Deserialize)]
struct TsCompositeHunk {
    /// Start line in old file (0-indexed)
    old_start: usize,
    /// Number of lines in old file
    old_count: usize,
    /// Start line in new file (0-indexed)
    new_start: usize,
    /// Number of lines in new file
    new_count: usize,
}

/// Options for creating a composite buffer
#[derive(serde::Deserialize)]
struct CreateCompositeBufferOptions {
    /// Display name for the composite buffer (shown in tab)
    name: String,
    /// Mode for keybindings (e.g., "diff-view")
    mode: String,
    /// Layout configuration
    layout: TsCompositeLayoutConfig,
    /// Source panes to display
    sources: Vec<TsCompositeSourceConfig>,
    /// Optional diff hunks for line alignment
    hunks: Option<Vec<TsCompositeHunk>>,
}

/// Create a composite buffer that displays multiple source buffers
///
/// Composite buffers allow displaying multiple underlying buffers in a single
/// tab/view area with custom layouts (side-by-side, stacked, unified).
/// This is useful for diff views, merge conflict resolution, etc.
/// @param options - Configuration for the composite buffer
/// @returns Promise resolving to the buffer ID of the created composite buffer
#[op2(async)]
async fn op_fresh_create_composite_buffer(
    state: Rc<RefCell<OpState>>,
    #[serde] options: CreateCompositeBufferOptions,
) -> Result<u32, JsErrorBox> {
    let receiver = {
        let state = state.borrow();
        let runtime_state = state
            .try_borrow::<Rc<RefCell<TsRuntimeState>>>()
            .ok_or_else(|| JsErrorBox::generic("Failed to get runtime state"))?;
        let runtime_state = runtime_state.borrow();

        // Allocate request ID
        let request_id = {
            let mut id = runtime_state.next_request_id.borrow_mut();
            let current = *id;
            *id += 1;
            current
        };

        // Create oneshot channel for response
        let (tx, rx) = tokio::sync::oneshot::channel();

        // Store the sender
        {
            let mut pending = runtime_state.pending_responses.lock().unwrap();
            pending.insert(request_id, tx);
        }

        // Convert TypeScript config to plugin API config
        let layout_config = crate::services::plugins::api::CompositeLayoutConfig {
            layout_type: options.layout.layout_type,
            ratios: options.layout.ratios,
            show_separator: options.layout.show_separator.unwrap_or(true),
            spacing: options.layout.spacing,
        };

        let source_configs: Vec<crate::services::plugins::api::CompositeSourceConfig> = options
            .sources
            .into_iter()
            .map(|src| crate::services::plugins::api::CompositeSourceConfig {
                buffer_id: src.buffer_id as usize,
                label: src.label.unwrap_or_default(),
                editable: src.editable,
                style: src
                    .style
                    .map(|s| crate::services::plugins::api::CompositePaneStyle {
                        add_bg: s.add_bg,
                        remove_bg: s.remove_bg,
                        modify_bg: s.modify_bg,
                        gutter_style: s.gutter_style,
                    }),
            })
            .collect();

        let hunks: Option<Vec<crate::services::plugins::api::CompositeHunk>> =
            options.hunks.map(|h| {
                h.into_iter()
                    .map(|hunk| crate::services::plugins::api::CompositeHunk {
                        old_start: hunk.old_start,
                        old_count: hunk.old_count,
                        new_start: hunk.new_start,
                        new_count: hunk.new_count,
                    })
                    .collect()
            });

        // Send command
        runtime_state
            .command_sender
            .send(PluginCommand::CreateCompositeBuffer {
                name: options.name,
                mode: options.mode,
                layout: layout_config,
                sources: source_configs,
                hunks,
                request_id: Some(request_id),
            })
            .map_err(|_| JsErrorBox::generic("Failed to send command"))?;

        rx
    };

    // Wait for response
    let response = receiver
        .await
        .map_err(|_| JsErrorBox::generic("Response channel closed"))?;

    // Extract buffer ID from response
    match response {
        crate::services::plugins::api::PluginResponse::CompositeBufferCreated {
            buffer_id, ..
        } => Ok(buffer_id.0 as u32),
        _ => Err(JsErrorBox::generic(
            "Unexpected plugin response for composite buffer creation",
        )),
    }
}

/// Update line alignment for a composite buffer
/// @param buffer_id - The composite buffer ID
/// @param hunks - New diff hunks for alignment
#[op2]
fn op_fresh_update_composite_alignment(
    state: &mut OpState,
    buffer_id: u32,
    #[serde] hunks: Vec<TsCompositeHunk>,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();

        let hunk_configs: Vec<crate::services::plugins::api::CompositeHunk> = hunks
            .into_iter()
            .map(|h| crate::services::plugins::api::CompositeHunk {
                old_start: h.old_start,
                old_count: h.old_count,
                new_start: h.new_start,
                new_count: h.new_count,
            })
            .collect();

        let _ = runtime_state
            .command_sender
            .send(PluginCommand::UpdateCompositeAlignment {
                buffer_id: crate::model::event::BufferId(buffer_id as usize),
                hunks: hunk_configs,
            });
        true
    } else {
        false
    }
}

/// Close a composite buffer
/// @param buffer_id - The composite buffer ID to close
#[op2(fast)]
fn op_fresh_close_composite_buffer(state: &mut OpState, buffer_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let _ = runtime_state
            .command_sender
            .send(PluginCommand::CloseCompositeBuffer {
                buffer_id: crate::model::event::BufferId(buffer_id as usize),
            });
        true
    } else {
        false
    }
}

/// Send an arbitrary LSP request and receive the raw JSON response
/// @param language - Language ID (e.g., "cpp")
/// @param method - Full LSP method (e.g., "textDocument/switchSourceHeader")
/// @param params - Optional request payload
/// @returns Promise resolving to the JSON response value
#[op2(async)]
#[serde]
async fn op_fresh_send_lsp_request(
    state: Rc<RefCell<OpState>>,
    #[string] language: String,
    #[string] method: String,
    #[serde] params: Option<serde_json::Value>,
) -> Result<serde_json::Value, JsErrorBox> {
    let receiver = {
        let state = state.borrow();
        let runtime_state = state
            .try_borrow::<Rc<RefCell<TsRuntimeState>>>()
            .ok_or_else(|| JsErrorBox::generic("Failed to get runtime state"))?;
        let runtime_state = runtime_state.borrow();

        let request_id = {
            let mut id = runtime_state.next_request_id.borrow_mut();
            let current = *id;
            *id += 1;
            current
        };

        let (tx, rx) = tokio::sync::oneshot::channel();
        {
            let mut pending = runtime_state.pending_responses.lock().unwrap();
            pending.insert(request_id, tx);
        }

        if runtime_state
            .command_sender
            .send(
                crate::services::plugins::api::PluginCommand::SendLspRequest {
                    language,
                    method,
                    params,
                    request_id,
                },
            )
            .is_err()
        {
            let mut pending = runtime_state.pending_responses.lock().unwrap();
            pending.remove(&request_id);
            return Err(JsErrorBox::generic("Failed to send plugin LSP request"));
        }

        rx
    };

    let response = receiver
        .await
        .map_err(|_| JsErrorBox::generic("Plugin LSP request cancelled"))?;

    match response {
        crate::services::plugins::api::PluginResponse::LspRequest { result, .. } => match result {
            Ok(value) => Ok(value),
            Err(err) => Err(JsErrorBox::generic(err)),
        },
        _ => Err(JsErrorBox::generic(
            "Unexpected plugin response for LSP request",
        )),
    }
}

/// Define a buffer mode with keybindings
/// @param name - Mode name (e.g., "diagnostics-list")
/// @param parent - Parent mode name for inheritance (e.g., "special"), or null
/// @param bindings - Array of [key_string, command_name] pairs
/// @param read_only - Whether buffers in this mode are read-only
/// @returns true if mode was defined successfully
/// @example
/// editor.defineMode("diagnostics-list", "special", [
///   ["Return", "diagnostics_goto"],
///   ["q", "close_buffer"]
/// ], true);
#[op2]
fn op_fresh_define_mode(
    state: &mut OpState,
    #[string] name: String,
    #[string] parent: String,
    #[serde] bindings: Vec<(String, String)>,
    read_only: bool,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        // Convert empty string to None for parent
        let parent = if parent.is_empty() {
            None
        } else {
            Some(parent)
        };
        let result = runtime_state
            .command_sender
            .send(PluginCommand::DefineMode {
                name,
                parent,
                bindings,
                read_only,
            });
        return result.is_ok();
    }
    false
}

/// Switch the current split to display a buffer
/// @param buffer_id - ID of the buffer to show
/// @returns true if buffer was shown successfully
#[op2(fast)]
fn op_fresh_show_buffer(state: &mut OpState, buffer_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ShowBuffer {
                buffer_id: BufferId(buffer_id as usize),
            });
        return result.is_ok();
    }
    false
}

/// Close a buffer and remove it from all splits
/// @param buffer_id - ID of the buffer to close
/// @returns true if buffer was closed successfully
#[op2(fast)]
fn op_fresh_close_buffer(state: &mut OpState, buffer_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::CloseBuffer {
                buffer_id: BufferId(buffer_id as usize),
            });
        return result.is_ok();
    }
    false
}

/// Focus a specific split
/// @param split_id - ID of the split to focus
/// @returns true if split was focused successfully
#[op2(fast)]
fn op_fresh_focus_split(state: &mut OpState, split_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::FocusSplit {
                split_id: crate::model::event::SplitId(split_id as usize),
            });
        return result.is_ok();
    }
    false
}

/// Set the buffer displayed in a specific split
/// @param split_id - ID of the split
/// @param buffer_id - ID of the buffer to display in the split
/// @returns true if the buffer was set successfully
#[op2(fast)]
fn op_fresh_set_split_buffer(state: &mut OpState, split_id: u32, buffer_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetSplitBuffer {
                split_id: crate::model::event::SplitId(split_id as usize),
                buffer_id: BufferId(buffer_id as usize),
            });
        return result.is_ok();
    }
    false
}

/// Set the scroll position of a specific split
/// @param split_id - The split ID
/// @param top_byte - The byte offset of the top visible line
/// @returns true if successful
#[op2(fast)]
fn op_fresh_set_split_scroll(state: &mut OpState, split_id: u32, top_byte: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetSplitScroll {
                split_id: crate::model::event::SplitId(split_id as usize),
                top_byte: top_byte as usize,
            });
        return result.is_ok();
    }
    false
}

/// Close a split (if not the last one)
/// @param split_id - ID of the split to close
/// @returns true if the split was closed successfully
#[op2(fast)]
fn op_fresh_close_split(state: &mut OpState, split_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::CloseSplit {
                split_id: crate::model::event::SplitId(split_id as usize),
            });
        return result.is_ok();
    }
    false
}

/// Set the ratio of a split container
/// @param split_id - ID of the split
/// @param ratio - Ratio between 0.0 and 1.0 (0.5 = equal split)
/// @returns true if the ratio was set successfully
#[op2(fast)]
fn op_fresh_set_split_ratio(state: &mut OpState, split_id: u32, ratio: f64) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetSplitRatio {
                split_id: crate::model::event::SplitId(split_id as usize),
                ratio: ratio as f32,
            });
        return result.is_ok();
    }
    false
}

/// Distribute all visible splits evenly
/// This adjusts the ratios of all container splits so each leaf split gets equal space
/// @returns true if the command was sent successfully
#[op2(fast)]
fn op_fresh_distribute_splits_evenly(state: &mut OpState) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::DistributeSplitsEvenly {
                split_ids: vec![], // Currently ignored - distributes all splits
            });
        return result.is_ok();
    }
    false
}

/// Set cursor position in a buffer (also scrolls viewport to show cursor)
/// @param buffer_id - ID of the buffer
/// @param position - Byte offset position for the cursor
/// @returns true if the command was sent successfully
#[op2(fast)]
fn op_fresh_set_buffer_cursor(state: &mut OpState, buffer_id: u32, position: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetBufferCursor {
                buffer_id: crate::model::event::BufferId(buffer_id as usize),
                position: position as usize,
            });
        return result.is_ok();
    }
    false
}

/// Get text properties at the cursor position in a buffer
/// @param buffer_id - ID of the buffer to query
/// @returns Array of property objects for text ranges containing the cursor
/// @example
/// const props = editor.getTextPropertiesAtCursor(bufferId);
/// if (props.length > 0 && props[0].location) {
///   editor.openFile(props[0].location.file, props[0].location.line, 0);
/// }
#[op2]
#[serde]
fn op_fresh_get_text_properties_at_cursor(
    state: &mut OpState,
    buffer_id: u32,
) -> Vec<std::collections::HashMap<String, serde_json::Value>> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            let buffer_id_key = BufferId(buffer_id as usize);

            // Get cursor position for this buffer
            let cursor_pos = match snapshot.buffer_cursor_positions.get(&buffer_id_key) {
                Some(pos) => *pos,
                None => return vec![],
            };

            // Get text properties for this buffer
            let properties = match snapshot.buffer_text_properties.get(&buffer_id_key) {
                Some(props) => props,
                None => return vec![],
            };

            // Find all properties that contain the cursor position
            return properties
                .iter()
                .filter(|prop| prop.contains(cursor_pos))
                .map(|prop| prop.properties.clone())
                .collect();
        };
    }
    vec![]
}

/// Set the content of a virtual buffer with text properties
/// @param buffer_id - ID of the virtual buffer
/// @param entries - Array of text entries with properties
/// @returns true if content was set successfully
#[op2]
fn op_fresh_set_virtual_buffer_content(
    state: &mut OpState,
    buffer_id: u32,
    #[serde] entries: Vec<TsTextPropertyEntry>,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();

        // Convert TypeScript entries to Rust TextPropertyEntry
        let rust_entries: Vec<crate::primitives::text_property::TextPropertyEntry> = entries
            .into_iter()
            .map(|e| crate::primitives::text_property::TextPropertyEntry {
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

/// Execute a built-in editor action by name
///
/// This is used by vi mode plugin to run motions and then check cursor position.
/// For example, to implement "dw" (delete word), the plugin:
/// 1. Saves current cursor position
/// 2. Calls executeAction("move_word_right") - cursor moves
/// 3. Gets new cursor position
/// 4. Deletes from old to new position
///
/// @param action_name - Action name (e.g., "move_word_right", "move_line_end")
/// @returns true if action was sent successfully
#[op2(fast)]
fn op_fresh_execute_action(state: &mut OpState, #[string] action_name: String) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ExecuteAction { action_name });
        return result.is_ok();
    }
    false
}

/// Execute multiple actions in sequence, each with an optional repeat count
///
/// Used by vi mode for count prefix (e.g., "3dw" = delete 3 words).
/// All actions execute atomically with no plugin roundtrips between them.
///
/// @param actions - Array of {action: string, count?: number} objects
/// @returns true if actions were sent successfully
#[op2]
fn op_fresh_execute_actions(state: &mut OpState, #[serde] actions: Vec<ActionSpecJs>) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let action_specs: Vec<ActionSpec> = actions
            .into_iter()
            .map(|a| ActionSpec {
                action: a.action,
                count: a.count.unwrap_or(1),
            })
            .collect();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::ExecuteActions {
                actions: action_specs,
            });
        return result.is_ok();
    }
    false
}

/// JavaScript representation of ActionSpec (with optional count)
#[derive(Debug, serde::Deserialize)]
struct ActionSpecJs {
    action: String,
    #[serde(default)]
    count: Option<u32>,
}

/// Get text from a buffer range
///
/// Used by vi mode plugin for yank operations - reads text without deleting.
/// @param buffer_id - Buffer ID
/// @param start - Start byte offset
/// @param end - End byte offset
/// @returns Text content of the range, or empty string on error
#[op2(async)]
#[string]
async fn op_fresh_get_buffer_text(
    state: Rc<RefCell<OpState>>,
    buffer_id: u32,
    start: u32,
    end: u32,
) -> Result<String, JsErrorBox> {
    let receiver = {
        let state = state.borrow();
        let runtime_state = state
            .try_borrow::<Rc<RefCell<TsRuntimeState>>>()
            .ok_or_else(|| JsErrorBox::generic("Failed to get runtime state"))?;
        let runtime_state = runtime_state.borrow();

        // Allocate request ID
        let request_id = {
            let mut id = runtime_state.next_request_id.borrow_mut();
            let current = *id;
            *id += 1;
            current
        };

        // Create oneshot channel for response
        let (tx, rx) = tokio::sync::oneshot::channel();

        // Store the sender
        {
            let mut pending = runtime_state.pending_responses.lock().unwrap();
            pending.insert(request_id, tx);
        }

        // Send command
        runtime_state
            .command_sender
            .send(PluginCommand::GetBufferText {
                buffer_id: BufferId(buffer_id as usize),
                start: start as usize,
                end: end as usize,
                request_id,
            })
            .map_err(|_| JsErrorBox::generic("Failed to send GetBufferText command"))?;

        rx
    };

    // Wait for response
    let response = receiver
        .await
        .map_err(|_| JsErrorBox::generic("Response channel closed"))?;

    match response {
        crate::services::plugins::api::PluginResponse::BufferText { text, .. } => {
            text.map_err(JsErrorBox::generic)
        }
        _ => Err(JsErrorBox::generic("Unexpected response type")),
    }
}

/// Set the global editor mode (for modal editing like vi mode)
///
/// When a mode is set, its keybindings take precedence over normal key handling.
/// Pass null/undefined to clear the mode and return to normal editing.
///
/// @param mode - Mode name (e.g., "vi-normal") or null to clear
/// @returns true if command was sent successfully
#[op2]
fn op_fresh_set_editor_mode(state: &mut OpState, #[string] mode: Option<String>) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetEditorMode { mode });
        return result.is_ok();
    }
    false
}

/// Get the current global editor mode
///
/// @returns Current mode name or null if no mode is active
#[op2]
#[string]
fn op_fresh_get_editor_mode(state: &mut OpState) -> Option<String> {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        if let Ok(snapshot) = runtime_state.state_snapshot.read() {
            return snapshot.editor_mode.clone();
        };
    }
    None
}

/// TypeScript struct for action popup action
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TsActionPopupAction {
    pub id: String,
    pub label: String,
}

/// TypeScript struct for action popup options
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TsActionPopupOptions {
    pub id: String,
    pub title: String,
    pub message: String,
    pub actions: Vec<TsActionPopupAction>,
}

/// Show an action popup with buttons for user interaction
///
/// When the user selects an action, the ActionPopupResult hook is fired.
/// @param options - Popup configuration with id, title, message, and actions
#[op2]
fn op_fresh_show_action_popup(state: &mut OpState, #[serde] options: TsActionPopupOptions) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();

        let actions: Vec<ActionPopupAction> = options
            .actions
            .into_iter()
            .map(|a| ActionPopupAction {
                id: a.id,
                label: a.label,
            })
            .collect();

        let result = runtime_state
            .command_sender
            .send(PluginCommand::ShowActionPopup {
                popup_id: options.id,
                title: options.title,
                message: options.message,
                actions,
            });
        return result.is_ok();
    }
    false
}

/// Disable LSP for a specific language and persist to config
///
/// This is used by LSP helper plugins to let users disable LSP for languages
/// where the server is not available or not working.
/// @param language - The language to disable LSP for (e.g., "python", "rust")
#[op2(fast)]
fn op_fresh_disable_lsp_for_language(state: &mut OpState, #[string] language: String) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::DisableLspForLanguage { language });
        return result.is_ok();
    }
    false
}

/// Create a scroll sync group for anchor-based synchronized scrolling
///
/// Used for side-by-side diff views where two panes need to scroll together.
/// The plugin provides the group ID (must be unique per plugin).
#[op2(fast)]
fn op_fresh_create_scroll_sync_group(
    state: &mut OpState,
    group_id: u32,
    left_split: u32,
    right_split: u32,
) -> bool {
    use crate::model::event::SplitId;

    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::CreateScrollSyncGroup {
                group_id,
                left_split: SplitId(left_split as usize),
                right_split: SplitId(right_split as usize),
            });
        return result.is_ok();
    }
    false
}

/// Set sync anchors for a scroll sync group
///
/// Anchors map corresponding line numbers between left and right buffers.
/// Each anchor is a tuple of (left_line, right_line).
#[op2]
fn op_fresh_set_scroll_sync_anchors(
    state: &mut OpState,
    group_id: u32,
    #[serde] anchors: Vec<(usize, usize)>,
) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::SetScrollSyncAnchors { group_id, anchors });
        return result.is_ok();
    }
    false
}

/// Remove a scroll sync group
#[op2(fast)]
fn op_fresh_remove_scroll_sync_group(state: &mut OpState, group_id: u32) -> bool {
    if let Some(runtime_state) = state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
        let runtime_state = runtime_state.borrow();
        let result = runtime_state
            .command_sender
            .send(PluginCommand::RemoveScrollSyncGroup { group_id });
        return result.is_ok();
    }
    false
}

// Define the extension with our ops
extension!(
    fresh_runtime,
    ops = [
        op_fresh_set_status,
        op_fresh_apply_theme,
        op_fresh_get_theme_schema,
        op_fresh_get_builtin_themes,
        op_fresh_reload_config,
        op_fresh_get_config,
        op_fresh_get_user_config,
        op_fresh_get_current_locale,
        op_fresh_plugin_translate,
        op_fresh_error,
        op_fresh_warn,
        op_fresh_info,
        op_fresh_debug,
        op_fresh_set_clipboard,
        op_fresh_get_config_dir,
        op_fresh_get_themes_dir,
        op_fresh_get_active_buffer_id,
        op_fresh_get_cursor_position,
        op_fresh_get_buffer_path,
        op_fresh_get_buffer_length,
        op_fresh_get_buffer_saved_diff,
        op_fresh_get_highlights,
        op_fresh_find_buffer_by_path,
        op_fresh_is_buffer_modified,
        op_fresh_insert_text,
        op_fresh_delete_range,
        op_fresh_add_overlay,
        op_fresh_remove_overlay,
        op_fresh_clear_namespace,
        op_fresh_clear_overlays_in_range,
        op_fresh_set_line_numbers,
        op_fresh_clear_all_overlays,
        op_fresh_add_virtual_text,
        op_fresh_remove_virtual_text,
        op_fresh_remove_virtual_texts_by_prefix,
        op_fresh_clear_virtual_texts,
        op_fresh_add_virtual_line,
        op_fresh_clear_virtual_text_namespace,
        op_fresh_submit_view_transform,
        op_fresh_clear_view_transform,
        op_fresh_refresh_lines,
        op_fresh_set_line_indicator,
        op_fresh_clear_line_indicators,
        op_fresh_set_file_explorer_decorations,
        op_fresh_clear_file_explorer_decorations,
        op_fresh_insert_at_cursor,
        op_fresh_register_command,
        op_fresh_unregister_command,
        op_fresh_set_context,
        op_fresh_open_file,
        op_fresh_get_active_split_id,
        op_fresh_open_file_in_split,
        op_fresh_get_cursor_line,
        op_fresh_get_all_cursor_positions,
        op_fresh_spawn_process_start,
        op_fresh_spawn_process_wait,
        op_fresh_delay,
        op_fresh_spawn_background_process,
        op_fresh_kill_process,
        op_fresh_is_process_running,
        op_fresh_get_buffer_info,
        op_fresh_list_buffers,
        op_fresh_get_all_diagnostics,
        op_fresh_get_primary_cursor,
        op_fresh_get_all_cursors,
        op_fresh_get_viewport,
        op_fresh_start_prompt,
        op_fresh_start_prompt_with_initial,
        op_fresh_set_prompt_suggestions,
        op_fresh_read_file,
        op_fresh_write_file,
        op_fresh_delete_theme,
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
        op_fresh_create_virtual_buffer_in_existing_split,
        op_fresh_create_virtual_buffer,
        // Composite buffer operations
        op_fresh_create_composite_buffer,
        op_fresh_update_composite_alignment,
        op_fresh_close_composite_buffer,
        op_fresh_send_lsp_request,
        op_fresh_define_mode,
        op_fresh_show_buffer,
        op_fresh_close_buffer,
        op_fresh_set_split_buffer,
        op_fresh_set_split_scroll,
        op_fresh_close_split,
        op_fresh_focus_split,
        op_fresh_set_split_ratio,
        op_fresh_distribute_splits_evenly,
        op_fresh_set_buffer_cursor,
        op_fresh_get_text_properties_at_cursor,
        op_fresh_set_virtual_buffer_content,
        // Vi mode support operations
        op_fresh_execute_action,
        op_fresh_execute_actions,
        op_fresh_get_buffer_text,
        op_fresh_set_editor_mode,
        op_fresh_get_editor_mode,
        // LSP helper operations
        op_fresh_show_action_popup,
        op_fresh_disable_lsp_for_language,
        // Scroll sync operations
        op_fresh_create_scroll_sync_group,
        op_fresh_set_scroll_sync_anchors,
        op_fresh_remove_scroll_sync_group,
    ],
);

/// Pending response senders type alias for convenience
pub type PendingResponses = Arc<
    std::sync::Mutex<
        HashMap<u64, tokio::sync::oneshot::Sender<crate::services::plugins::api::PluginResponse>>,
    >,
>;

/// TypeScript plugin runtime
pub struct TypeScriptRuntime {
    js_runtime: JsRuntime,
    /// Shared event handlers registry
    event_handlers: Rc<RefCell<HashMap<String, Vec<String>>>>,
    /// Pending response senders (shared with runtime state for delivering responses)
    pending_responses: PendingResponses,
}

impl TypeScriptRuntime {
    /// Create a new TypeScript runtime (standalone, for testing)
    pub fn new() -> Result<Self> {
        // Create dummy state for standalone testing
        let (tx, _rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let dir_context =
            crate::config_io::DirectoryContext::for_testing(std::path::Path::new("/tmp"));
        Self::with_state(state_snapshot, tx, dir_context)
    }

    /// Create a new TypeScript runtime with editor state
    pub fn with_state(
        state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
        command_sender: std::sync::mpsc::Sender<PluginCommand>,
        dir_context: crate::config_io::DirectoryContext,
    ) -> Result<Self> {
        let pending_responses: PendingResponses = Arc::new(std::sync::Mutex::new(HashMap::new()));
        Self::with_state_and_responses(
            state_snapshot,
            command_sender,
            pending_responses,
            dir_context,
        )
    }

    /// Create a new TypeScript runtime with editor state and shared pending responses
    pub fn with_state_and_responses(
        state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
        command_sender: std::sync::mpsc::Sender<PluginCommand>,
        pending_responses: PendingResponses,
        dir_context: crate::config_io::DirectoryContext,
    ) -> Result<Self> {
        tracing::debug!("TypeScriptRuntime::with_state_and_responses: initializing V8 platform");
        // Initialize V8 platform before creating JsRuntime
        crate::v8_init::init();
        tracing::debug!("TypeScriptRuntime::with_state_and_responses: V8 platform initialized");

        tracing::debug!("TypeScriptRuntime::with_state_and_responses: creating runtime state");
        let event_handlers = Rc::new(RefCell::new(HashMap::new()));
        let runtime_state = Rc::new(RefCell::new(TsRuntimeState {
            state_snapshot,
            command_sender,
            event_handlers: event_handlers.clone(),
            pending_responses: Arc::clone(&pending_responses),
            next_request_id: Rc::new(RefCell::new(1)),
            background_processes: Rc::new(RefCell::new(HashMap::new())),
            cancellable_processes: Rc::new(RefCell::new(HashMap::new())),
            process_pids: Rc::new(RefCell::new(HashMap::new())),
            next_process_id: Rc::new(RefCell::new(1)),
            dir_context,
        }));

        tracing::debug!(
            "TypeScriptRuntime::with_state_and_responses: creating JsRuntime with deno_core"
        );
        let js_runtime_start = std::time::Instant::now();
        let mut js_runtime = JsRuntime::new(RuntimeOptions {
            module_loader: Some(Rc::new(TypeScriptModuleLoader)),
            extensions: vec![fresh_runtime::init()],
            ..Default::default()
        });
        tracing::debug!(
            "TypeScriptRuntime::with_state_and_responses: JsRuntime created in {:?}",
            js_runtime_start.elapsed()
        );

        // Store the runtime state in the op state
        js_runtime.op_state().borrow_mut().put(runtime_state);

        // Set up the global editor API
        js_runtime
            .execute_script(
                "<fresh_bootstrap>",
                r#"
                const core = Deno.core;

                // Core editor operations (plugin-agnostic)
                // These are shared by all plugin-scoped editor objects
                const _editorCore = {
                    // Status
                    setStatus(message) {
                        core.ops.op_fresh_set_status(message);
                    },

                    // Theme operations
                    applyTheme(themeName) {
                        return core.ops.op_fresh_apply_theme(themeName);
                    },
                    getThemeSchema() {
                        return core.ops.op_fresh_get_theme_schema();
                    },
                    getBuiltinThemes() {
                        return core.ops.op_fresh_get_builtin_themes();
                    },

                    // Config operations
                    reloadConfig() {
                        core.ops.op_fresh_reload_config();
                    },
                    getConfig() {
                        return core.ops.op_fresh_get_config();
                    },
                    getUserConfig() {
                        return core.ops.op_fresh_get_user_config();
                    },
                    getConfigDir() {
                        return core.ops.op_fresh_get_config_dir();
                    },
                    getThemesDir() {
                        return core.ops.op_fresh_get_themes_dir();
                    },

                    // Clipboard
                    copyToClipboard(text) {
                        core.ops.op_fresh_set_clipboard(text);
                    },
                    setClipboard(text) {
                        core.ops.op_fresh_set_clipboard(text);
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
                    getBufferSavedDiff(bufferId) {
                        return core.ops.op_fresh_get_buffer_saved_diff(bufferId);
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
                    addOverlay(bufferId, namespace, start, end, r, g, b, underline, bold = false, italic = false, bg_r = -1, bg_g = -1, bg_b = -1, extend_to_line_end = false) {
                        return core.ops.op_fresh_add_overlay(bufferId, namespace, start, end, r, g, b, bg_r, bg_g, bg_b, underline, bold, italic, extend_to_line_end);
                    },
                    removeOverlay(bufferId, handle) {
                        return core.ops.op_fresh_remove_overlay(bufferId, handle);
                    },
                    clearNamespace(bufferId, namespace) {
                        return core.ops.op_fresh_clear_namespace(bufferId, namespace);
                    },
                    clearOverlaysInRange(bufferId, start, end) {
                        return core.ops.op_fresh_clear_overlays_in_range(bufferId, start, end);
                    },
                    clearAllOverlays(bufferId) {
                        return core.ops.op_fresh_clear_all_overlays(bufferId);
                    },

                    // Line numbers
                    setLineNumbers(bufferId, enabled) {
                        return core.ops.op_fresh_set_line_numbers(bufferId, enabled);
                    },

                    // Virtual text
                    addVirtualText(bufferId, virtualTextId, position, text, r, g, b, before, useBg = false) {
                        return core.ops.op_fresh_add_virtual_text(bufferId, virtualTextId, position, text, r, g, b, before, useBg);
                    },
                    removeVirtualText(bufferId, virtualTextId) {
                        return core.ops.op_fresh_remove_virtual_text(bufferId, virtualTextId);
                    },
                    removeVirtualTextsByPrefix(bufferId, prefix) {
                        return core.ops.op_fresh_remove_virtual_texts_by_prefix(bufferId, prefix);
                    },
                    clearVirtualTexts(bufferId) {
                        return core.ops.op_fresh_clear_virtual_texts(bufferId);
                    },

                    // Virtual lines
                    addVirtualLine(bufferId, position, text, fgR, fgG, fgB, bgR, bgG, bgB, above, namespace, priority = 0) {
                        return core.ops.op_fresh_add_virtual_line(bufferId, position, text, fgR, fgG, fgB, bgR, bgG, bgB, above, namespace, priority);
                    },
                    clearVirtualTextNamespace(bufferId, namespace) {
                        return core.ops.op_fresh_clear_virtual_text_namespace(bufferId, namespace);
                    },

                    // View transforms
                    submitViewTransform(bufferId, splitId, start, end, tokens, layoutHints) {
                        return core.ops.op_fresh_submit_view_transform(bufferId, splitId, start, end, tokens, layoutHints);
                    },
                    clearViewTransform(bufferId, splitId = null) {
                        return core.ops.op_fresh_clear_view_transform(bufferId, splitId);
                    },

                    refreshLines(bufferId) {
                        return core.ops.op_fresh_refresh_lines(bufferId);
                    },

                    // Line indicators
                    setLineIndicator(bufferId, line, namespace, symbol, r, g, b, priority) {
                        return core.ops.op_fresh_set_line_indicator(bufferId, line, namespace, symbol, r, g, b, priority);
                    },
                    clearLineIndicators(bufferId, namespace) {
                        return core.ops.op_fresh_clear_line_indicators(bufferId, namespace);
                    },
                    setFileExplorerDecorations(namespace, decorations) {
                        return core.ops.op_fresh_set_file_explorer_decorations(namespace, decorations);
                    },
                    clearFileExplorerDecorations(namespace) {
                        return core.ops.op_fresh_clear_file_explorer_decorations(namespace);
                    },

                    insertAtCursor(text) {
                        return core.ops.op_fresh_insert_at_cursor(text);
                    },

                    unregisterCommand(name) {
                        return core.ops.op_fresh_unregister_command(name);
                    },

                    setContext(name, active) {
                        return core.ops.op_fresh_set_context(name, active);
                    },

                    openFile(path, line = 0, column = 0) {
                        return core.ops.op_fresh_open_file(path, line, column);
                    },

                    getActiveSplitId() {
                        return core.ops.op_fresh_get_active_split_id();
                    },
                    openFileInSplit(splitId, path, line = 0, column = 0) {
                        return core.ops.op_fresh_open_file_in_split(splitId, path, line, column);
                    },

                    getCursorLine() {
                        return core.ops.op_fresh_get_cursor_line();
                    },
                    getAllCursorPositions() {
                        return core.ops.op_fresh_get_all_cursor_positions();
                    },

                    getBufferInfo(bufferId) {
                        return core.ops.op_fresh_get_buffer_info(bufferId);
                    },
                    listBuffers() {
                        return core.ops.op_fresh_list_buffers();
                    },
                    getAllDiagnostics() {
                        return core.ops.op_fresh_get_all_diagnostics();
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

                    startPrompt(label, promptType) {
                        return core.ops.op_fresh_start_prompt(label, promptType);
                    },
                    startPromptWithInitial(label, promptType, initialValue) {
                        return core.ops.op_fresh_start_prompt_with_initial(label, promptType, initialValue);
                    },
                    setPromptSuggestions(suggestions) {
                        return core.ops.op_fresh_set_prompt_suggestions(suggestions);
                    },

                    spawnProcess(command, args = [], cwd = null) {
                        const effectiveCwd = cwd ?? core.ops.op_fresh_get_cwd();
                        const processId = core.ops.op_fresh_spawn_process_start(command, args, effectiveCwd);
                        const resultPromise = processId.then(id => core.ops.op_fresh_spawn_process_wait(id));
                        return {
                            get processId() { return processId; },
                            get result() { return resultPromise; },
                            kill: async () => {
                                const id = await processId;
                                return core.ops.op_fresh_kill_process(id);
                            },
                            then(onFulfilled, onRejected) {
                                return resultPromise.then(onFulfilled, onRejected);
                            },
                            catch(onRejected) {
                                return resultPromise.catch(onRejected);
                            }
                        };
                    },
                    delay(ms) {
                        return core.ops.op_fresh_delay(ms);
                    },
                    spawnBackgroundProcess(command, args = [], cwd = null) {
                        const effectiveCwd = cwd ?? core.ops.op_fresh_get_cwd();
                        return core.ops.op_fresh_spawn_background_process(command, args, effectiveCwd);
                    },
                    killProcess(processId) {
                        return core.ops.op_fresh_kill_process(processId);
                    },
                    isProcessRunning(processId) {
                        return core.ops.op_fresh_is_process_running(processId);
                    },
                    sendLspRequest(language, method, params = null) {
                        return core.ops.op_fresh_send_lsp_request(language, method, params);
                    },

                    readFile(path) {
                        return core.ops.op_fresh_read_file(path);
                    },
                    writeFile(path, content) {
                        return core.ops.op_fresh_write_file(path, content);
                    },
                    deleteTheme(name) {
                        return core.ops.op_fresh_delete_theme(name);
                    },
                    fileExists(path) {
                        return core.ops.op_fresh_file_exists(path);
                    },
                    fileStat(path) {
                        return core.ops.op_fresh_file_stat(path);
                    },

                    getEnv(name) {
                        return core.ops.op_fresh_get_env(name);
                    },
                    getCwd() {
                        return core.ops.op_fresh_get_cwd();
                    },

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

                    on(eventName, handlerName) {
                        return core.ops.op_fresh_on(eventName, handlerName);
                    },
                    off(eventName, handlerName) {
                        return core.ops.op_fresh_off(eventName, handlerName);
                    },
                    getHandlers(eventName) {
                        return core.ops.op_fresh_get_handlers(eventName);
                    },

                    createVirtualBufferInSplit(options) {
                        return core.ops.op_fresh_create_virtual_buffer_in_split(options);
                    },
                    createVirtualBufferInExistingSplit(options) {
                        return core.ops.op_fresh_create_virtual_buffer_in_existing_split(options);
                    },
                    createVirtualBuffer(options) {
                        return core.ops.op_fresh_create_virtual_buffer(options);
                    },

                    // Composite buffer operations
                    createCompositeBuffer(options) {
                        return core.ops.op_fresh_create_composite_buffer(options);
                    },
                    updateCompositeAlignment(bufferId, hunks) {
                        return core.ops.op_fresh_update_composite_alignment(bufferId, hunks);
                    },
                    closeCompositeBuffer(bufferId) {
                        return core.ops.op_fresh_close_composite_buffer(bufferId);
                    },

                    defineMode(name, parent, bindings, readOnly = false) {
                        const parentStr = parent != null ? parent : "";
                        return core.ops.op_fresh_define_mode(name, parentStr, bindings, readOnly);
                    },
                    showBuffer(bufferId) {
                        return core.ops.op_fresh_show_buffer(bufferId);
                    },
                    closeBuffer(bufferId) {
                        return core.ops.op_fresh_close_buffer(bufferId);
                    },
                    focusSplit(splitId) {
                        return core.ops.op_fresh_focus_split(splitId);
                    },
                    setSplitBuffer(splitId, bufferId) {
                        return core.ops.op_fresh_set_split_buffer(splitId, bufferId);
                    },
                    closeSplit(splitId) {
                        return core.ops.op_fresh_close_split(splitId);
                    },
                    setSplitRatio(splitId, ratio) {
                        return core.ops.op_fresh_set_split_ratio(splitId, ratio);
                    },
                    setSplitScroll(splitId, topByte) {
                        return core.ops.op_fresh_set_split_scroll(splitId, topByte);
                    },
                    distributeSplitsEvenly() {
                        return core.ops.op_fresh_distribute_splits_evenly();
                    },
                    setBufferCursor(bufferId, position) {
                        return core.ops.op_fresh_set_buffer_cursor(bufferId, position);
                    },
                    getTextPropertiesAtCursor(bufferId) {
                        return core.ops.op_fresh_get_text_properties_at_cursor(bufferId);
                    },
                    setVirtualBufferContent(bufferId, entries) {
                        return core.ops.op_fresh_set_virtual_buffer_content(bufferId, entries);
                    },

                    executeAction(actionName) {
                        return core.ops.op_fresh_execute_action(actionName);
                    },
                    executeActions(actions) {
                        return core.ops.op_fresh_execute_actions(actions);
                    },
                    getBufferText(bufferId, start, end) {
                        return core.ops.op_fresh_get_buffer_text(bufferId, start, end);
                    },
                    setEditorMode(mode) {
                        return core.ops.op_fresh_set_editor_mode(mode);
                    },
                    getEditorMode() {
                        return core.ops.op_fresh_get_editor_mode();
                    },

                    showActionPopup(options) {
                        return core.ops.op_fresh_show_action_popup(options);
                    },
                    disableLspForLanguage(language) {
                        return core.ops.op_fresh_disable_lsp_for_language(language);
                    },

                    createScrollSyncGroup(groupId, leftSplit, rightSplit) {
                        return core.ops.op_fresh_create_scroll_sync_group(groupId, leftSplit, rightSplit);
                    },
                    setScrollSyncAnchors(groupId, anchors) {
                        return core.ops.op_fresh_set_scroll_sync_anchors(groupId, anchors);
                    },
                    removeScrollSyncGroup(groupId) {
                        return core.ops.op_fresh_remove_scroll_sync_group(groupId);
                    },

                    getCurrentLocale() {
                        return core.ops.op_fresh_get_current_locale();
                    },
                };

                // Create a plugin-scoped editor object
                // Each plugin gets its own editor with bound plugin name for i18n and logging
                globalThis._createPluginEditor = function(pluginName) {
                    const pluginEditor = {
                        // Include all core methods
                        ..._editorCore,

                        // Plugin name for reference
                        _pluginName: pluginName,

                        // Plugin-specific logging (prefixed with plugin name)
                        error(message) {
                            core.ops.op_fresh_error(`[${pluginName}] ${message}`);
                        },
                        warn(message) {
                            core.ops.op_fresh_warn(`[${pluginName}] ${message}`);
                        },
                        info(message) {
                            core.ops.op_fresh_info(`[${pluginName}] ${message}`);
                        },
                        debug(message) {
                            core.ops.op_fresh_debug(`[${pluginName}] ${message}`);
                        },

                        // Plugin-specific command registration
                        registerCommand(name, description, action, contexts = "") {
                            return core.ops.op_fresh_register_command(name, description, action, contexts, pluginName);
                        },

                        // Plugin-specific translation
                        t(key, args = {}) {
                            return core.ops.op_fresh_plugin_translate(pluginName, key, args);
                        },

                        // For compatibility - returns self since t() is already bound
                        getL10n() {
                            return { t: (key, args = {}) => this.t(key, args) };
                        },
                    };
                    return pluginEditor;
                };

                // Plugins MUST call getEditor() at the top of their file to get their scoped editor.
                // This captures the editor instance during plugin initialization.
                // The editor is stored in __pendingEditor during plugin load and cleared after getEditor() is called.
                globalThis.__pendingEditor = null;
                globalThis.getEditor = function() {
                    const editor = globalThis.__pendingEditor;
                    if (!editor) {
                        throw new Error('getEditor() must be called at the top of the plugin file during initialization');
                    }
                    globalThis.__pendingEditor = null; // Clear after use
                    return editor;
                };

                // Default editor for system use (console logging)
                const defaultEditor = globalThis._createPluginEditor('system');
                globalThis.console = {
                    log: (...args) => defaultEditor.info(args.map(a => String(a)).join(' ')),
                    warn: (...args) => defaultEditor.warn(args.map(a => String(a)).join(' ')),
                    error: (...args) => defaultEditor.error(args.map(a => String(a)).join(' ')),
                    info: (...args) => defaultEditor.info(args.map(a => String(a)).join(' ')),
                    debug: (...args) => defaultEditor.debug(args.map(a => String(a)).join(' ')),
                };

                // Pre-compiled event dispatcher for performance
                globalThis.__eventDispatcher = async function(handlerName, eventData) {
                    const handler = globalThis[handlerName];
                    if (typeof handler === 'function') {
                        const result = handler(eventData);
                        const finalResult = (result instanceof Promise) ? await result : result;
                        return finalResult !== false;
                    } else {
                        defaultEditor.debug('Event handler "' + handlerName + '" is not defined');
                        return true;
                    }
                };
                "#
                .to_string(),
            )
            .map_err(|e| anyhow!("Failed to initialize editor API: {}", e))?;

        tracing::debug!(
            "TypeScriptRuntime::with_state_and_responses: bootstrap script executed, runtime ready"
        );

        Ok(Self {
            js_runtime,
            event_handlers,
            pending_responses,
        })
    }

    /// Deliver a response to a pending async operation
    ///
    /// This is called by the editor after processing a command that requires a response.
    pub fn deliver_response(&self, response: crate::services::plugins::api::PluginResponse) {
        let request_id = match &response {
            crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
                request_id,
                ..
            } => *request_id,
            crate::services::plugins::api::PluginResponse::LspRequest { request_id, .. } => {
                *request_id
            }
            crate::services::plugins::api::PluginResponse::HighlightsComputed {
                request_id,
                ..
            } => *request_id,
            crate::services::plugins::api::PluginResponse::BufferText { request_id, .. } => {
                *request_id
            }
            crate::services::plugins::api::PluginResponse::CompositeBufferCreated {
                request_id,
                ..
            } => *request_id,
        };

        let sender = {
            let mut pending = self.pending_responses.lock().unwrap();
            pending.remove(&request_id)
        };

        if let Some(tx) = sender {
            let _ = tx.send(response);
        } else {
            tracing::warn!("No pending response sender for request_id {}", request_id);
        }
    }

    /// Get a reference to pending responses for external delivery
    pub fn pending_responses(&self) -> &PendingResponses {
        &self.pending_responses
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
        self.load_module_with_source(path, "").await
    }

    /// Load and execute a TypeScript/JavaScript module file with explicit plugin source
    pub async fn load_module_with_source(&mut self, path: &str, plugin_source: &str) -> Result<()> {
        // Create a plugin-scoped editor and store it in __pendingEditor.
        // Plugins must call getEditor() at the top of their file to retrieve it.
        let set_editor: FastString = format!(
            "globalThis.__pendingEditor = globalThis._createPluginEditor(\"{}\");",
            plugin_source
        )
        .into();
        self.js_runtime
            .execute_script("<set_pending_editor>", set_editor)
            .map_err(|e| anyhow!("Failed to set pending editor: {}", e))?;

        let module_specifier = deno_core::resolve_path(
            path,
            &std::env::current_dir().map_err(|e| anyhow!("Failed to get cwd: {}", e))?,
        )
        .map_err(|e| anyhow!("Failed to resolve module path '{}': {}", path, e))?;

        // Use load_side_es_module for plugins (allows multiple modules to be loaded)
        let mod_id = self
            .js_runtime
            .load_side_es_module(&module_specifier)
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

        // Check if the plugin forgot to call getEditor()
        let check_editor: FastString = r#"
            if (globalThis.__pendingEditor !== null) {
                const pluginName = globalThis.__pendingEditor._pluginName;
                globalThis.__pendingEditor = null;
                throw new Error(`Plugin '${pluginName}' must call getEditor() at the top of the file`);
            }
        "#.to_string().into();
        self.js_runtime
            .execute_script("<check_pending_editor>", check_editor)
            .map_err(|e| anyhow!("Plugin initialization error: {}", e))?;

        Ok(())
    }

    /// Execute a global function by name (for plugin actions)
    pub async fn execute_action(&mut self, action_name: &str) -> Result<()> {
        // Action functions are defined by plugins and have captured their scoped
        // editor reference in closures, so no context setup is needed here.
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
        let emit_start = std::time::Instant::now();
        let handlers = self.event_handlers.borrow().get(event_name).cloned();

        if let Some(handler_names) = handlers {
            if handler_names.is_empty() {
                return Ok(true);
            }

            for handler_name in &handler_names {
                let call_start = std::time::Instant::now();

                // Use execute_script to call the event dispatcher
                // This escapes the handler name and embeds the event data as JSON
                let script = format!(
                    "__eventDispatcher({}, {})",
                    serde_json::to_string(handler_name).unwrap_or_else(|_| "\"\"".to_string()),
                    event_data
                );

                match self.js_runtime.execute_script("<emit>", script) {
                    Ok(_) => {
                        let call_elapsed = call_start.elapsed();
                        // Don't poll event loop here - the plugin thread's main loop
                        // will poll it periodically to allow long-running promises
                        // (like process spawns) to make progress.
                        tracing::trace!(
                            event = event_name,
                            handler = handler_name,
                            call_us = call_elapsed.as_micros(),
                            "emit handler timing"
                        );
                    }
                    Err(e) => {
                        tracing::error!(
                            "Failed to call event handler '{}' for '{}': {:?}",
                            handler_name,
                            event_name,
                            e
                        );
                    }
                }
            }
        }

        let emit_elapsed = emit_start.elapsed();
        tracing::trace!(
            event = event_name,
            total_us = emit_elapsed.as_micros(),
            "emit total timing"
        );

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

    /// Poll the event loop once to make progress on pending promises.
    /// Returns true if there's still pending work, false if all work is done.
    pub fn poll_event_loop_once(&mut self) -> bool {
        let waker = std::task::Waker::noop();
        let mut cx = std::task::Context::from_waker(waker);
        match self.js_runtime.poll_event_loop(&mut cx, Default::default()) {
            std::task::Poll::Ready(result) => {
                if let Err(e) = result {
                    tracing::warn!("Event loop error: {}", e);
                }
                false // No more pending work
            }
            std::task::Poll::Pending => true, // More work pending
        }
    }

    /// Send a status message to the editor UI
    pub fn send_status(&mut self, message: String) {
        let op_state = self.js_runtime.op_state();
        let op_state = op_state.borrow();
        if let Some(runtime_state) = op_state.try_borrow::<Rc<RefCell<TsRuntimeState>>>() {
            let runtime_state = runtime_state.borrow();
            let _ = runtime_state
                .command_sender
                .send(PluginCommand::SetStatus { message });
        }
    }
}

// === TypeScript Plugin Manager ===

use crate::input::command_registry::CommandRegistry;
use crate::services::plugins::hooks::{hook_args_to_json, HookArgs, HookRegistry};
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

        // Create directory context (use system defaults or temp for testing)
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap_or_else(|_| {
            crate::config_io::DirectoryContext::for_testing(std::path::Path::new("/tmp"))
        });

        // Create TypeScript runtime with state
        let runtime = TypeScriptRuntime::with_state(
            Arc::clone(&state_snapshot),
            command_sender,
            dir_context,
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

        // Load and execute the module, passing plugin name for command registration
        let path_str = path
            .to_str()
            .ok_or_else(|| anyhow!("Invalid path encoding"))?;

        self.runtime
            .load_module_with_source(path_str, &plugin_name)
            .await?;

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
        let json_data = hook_args_to_json(args)?;

        // Emit to TypeScript handlers
        self.runtime.emit(hook_name, &json_data).await?;

        Ok(())
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
        // Create a new tokio current_thread runtime for this blocking operation
        // deno_core requires current_thread runtime for async ops
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(|e| anyhow!("Failed to create runtime: {}", e))?;

        rt.block_on(self.load_plugin(path))
    }

    /// Load all plugins from a directory synchronously (blocking)
    pub fn load_plugins_from_dir_blocking(&mut self, dir: &Path) -> Vec<String> {
        // deno_core requires current_thread runtime for async ops
        let rt = match tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
        {
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
        // deno_core requires current_thread runtime for async ops
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(|e| anyhow!("Failed to create runtime: {}", e))?;

        rt.block_on(self.execute_action(action_name))
    }

    /// Run a hook synchronously (blocking)
    pub fn run_hook_blocking(&mut self, hook_name: &str, args: &HookArgs) -> Result<()> {
        // deno_core requires a current_thread runtime for async ops
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(|e| anyhow!("Failed to create runtime: {}", e))?;

        rt.block_on(self.run_hook(hook_name, args))
    }

    /// Reload a plugin synchronously (blocking)
    pub fn reload_plugin_blocking(&mut self, name: &str) -> Result<()> {
        // deno_core requires current_thread runtime for async ops
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
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
        assert!(
            result.is_ok(),
            "Failed to execute simple script: {:?}",
            result
        );
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
        assert!(
            result.is_ok(),
            "Failed to execute async action: {:?}",
            result
        );
    }

    #[tokio::test]
    async fn test_with_editor_state() {
        use crate::services::plugins::api::{BufferInfo, CursorInfo};
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
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        let mut runtime =
            TypeScriptRuntime::with_state(state_snapshot.clone(), tx, dir_context).unwrap();

        // Test querying state from TypeScript
        let result = runtime
            .execute_script(
                "<test_state>",
                r#"
                var editor = globalThis._createPluginEditor('test');
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
                var editor = globalThis._createPluginEditor('test');
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
                namespace,
                range,
                color,
                bg_color,
                underline,
                bold,
                italic,
                extend_to_line_end,
            } => {
                assert_eq!(buffer_id.0, 42);
                assert_eq!(namespace.as_ref().map(|n| n.as_str()), Some("test-overlay"));
                assert_eq!(range.start, 0);
                assert_eq!(range.end, 50);
                assert_eq!(*color, (255, 0, 0));
                assert_eq!(*bg_color, None);
                assert!(*underline);
                assert!(!*bold);
                assert!(!*italic);
                assert!(!*extend_to_line_end);
            }
            _ => panic!("Expected AddOverlay command"),
        }

        match &commands[4] {
            PluginCommand::RemoveOverlay { buffer_id, handle } => {
                assert_eq!(buffer_id.0, 42);
                assert_eq!(handle.as_str(), "test-overlay");
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
                const editor = globalThis._createPluginEditor('test');
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
        assert!(
            result.is_ok(),
            "API accessibility test failed: {:?}",
            result
        );
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
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        let mut runtime =
            TypeScriptRuntime::with_state(state_snapshot.clone(), tx, dir_context).unwrap();

        // Test new ops from TypeScript
        let result = runtime
            .execute_script(
                "<test_new_ops>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
                    crate::input::keybindings::Action::PluginAction(name) => {
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
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx, dir_context).unwrap();

        // Register command with empty contexts (available everywhere)
        let result = runtime
            .execute_script(
                "<test_empty_contexts>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
                assert!(
                    command.contexts.is_empty(),
                    "Empty string should result in empty contexts"
                );
            }
            _ => panic!("Expected RegisterCommand"),
        }
    }

    #[tokio::test]
    async fn test_register_command_all_contexts() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx, dir_context).unwrap();

        // Test all valid context types
        let result = runtime
            .execute_script(
                "<test_all_contexts>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
                assert_eq!(command.contexts.len(), 6);
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::Global));
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::Normal));
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::Prompt));
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::Popup));
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::FileExplorer));
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::Menu));
            }
            _ => panic!("Expected RegisterCommand"),
        }
    }

    #[tokio::test]
    async fn test_register_command_invalid_contexts_ignored() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx, dir_context).unwrap();

        // Invalid contexts should be silently ignored
        let result = runtime
            .execute_script(
                "<test_invalid_contexts>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::Normal));
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::Popup));
            }
            _ => panic!("Expected RegisterCommand"),
        }
    }

    #[tokio::test]
    async fn test_open_file_with_zero_values() {
        let (tx, rx) = std::sync::mpsc::channel();
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx, dir_context).unwrap();

        // Zero values should translate to None (file opening without positioning)
        let result = runtime
            .execute_script(
                "<test_zero_values>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx, dir_context).unwrap();

        // Test that JavaScript default parameters work
        let result = runtime
            .execute_script(
                "<test_default_params>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx, dir_context).unwrap();

        // Open file at specific line but no column
        let result = runtime
            .execute_script(
                "<test_line_only>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        let mut runtime = TypeScriptRuntime::with_state(state_snapshot, tx, dir_context).unwrap();

        // Context names should be case-insensitive
        let result = runtime
            .execute_script(
                "<test_case_insensitive>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::Normal));
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::Popup));
                assert!(command
                    .contexts
                    .contains(&crate::input::keybindings::KeyContext::FileExplorer));
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
                const cwd = editor.getCwd();
                if (!cwd || cwd.length === 0) {
                    throw new Error("getCwd should return non-empty string");
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

        // Use platform-appropriate temp directory
        let temp_file = std::env::temp_dir().join("fresh_ts_runtime_test_write.txt");
        let temp_file_str = temp_file.to_string_lossy().replace('\\', "/"); // Normalize path separators for JS

        let result = runtime
            .execute_script(
                "<test_write_file>",
                &format!(
                    r#"
                const editor = globalThis._createPluginEditor('test');
                (async () => {{
                    const testFile = "{temp_file_str}";
                    const testContent = "Hello from TypeScript plugin!\nLine 2\n";

                    // Write the file
                    await editor.writeFile(testFile, testContent);

                    // Verify it was written by reading it back
                    const readBack = await editor.readFile(testFile);
                    if (readBack !== testContent) {{
                        throw new Error(`Write/read mismatch. Expected: ${{testContent}}, Got: ${{readBack}}`);
                    }}

                    // Verify file stats
                    const stat = editor.fileStat(testFile);
                    if (!stat.exists) {{
                        throw new Error("Written file should exist");
                    }}
                    if (!stat.is_file) {{
                        throw new Error("Written path should be a file");
                    }}
                    if (stat.size !== testContent.length) {{
                        throw new Error(`File size mismatch. Expected: ${{testContent.length}}, Got: ${{stat.size}}`);
                    }}

                    console.log("Write file test passed!");
                }})()
                "#
                ),
            )
            .await;
        assert!(result.is_ok(), "Write file test failed: {:?}", result);

        // Clean up test file
        let _ = std::fs::remove_file(&temp_file);
    }

    #[tokio::test]
    async fn test_read_dir() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_read_dir>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
    #[cfg(unix)]
    async fn test_path_is_absolute() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_path_is_absolute>",
                r#"
                const editor = globalThis._createPluginEditor('test');
                // Test absolute paths (Unix)
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
        assert!(result.is_ok(), "Path is absolute test failed: {:?}", result);
    }

    #[tokio::test]
    #[cfg(windows)]
    async fn test_path_is_absolute() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_path_is_absolute>",
                r#"
                const editor = globalThis._createPluginEditor('test');
                // Test absolute paths (Windows)
                if (!editor.pathIsAbsolute("C:\\Users\\test")) {
                    throw new Error("C:\\Users\\test should be absolute");
                }
                if (!editor.pathIsAbsolute("C:/Users/test")) {
                    throw new Error("C:/Users/test should be absolute");
                }
                if (!editor.pathIsAbsolute("D:\\")) {
                    throw new Error("D:\\ should be absolute");
                }

                // Test relative paths
                if (editor.pathIsAbsolute("src\\main.rs")) {
                    throw new Error("src\\main.rs should not be absolute");
                }
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
        assert!(result.is_ok(), "Path is absolute test failed: {:?}", result);
    }

    #[tokio::test]
    async fn test_hook_registration() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        let result = runtime
            .execute_script(
                "<test_hook_registration>",
                r#"
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
                const editor = globalThis._createPluginEditor('test');
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
        // Test various hook args conversions using the shared function
        let args = HookArgs::BufferActivated {
            buffer_id: BufferId(5),
        };
        let json = hook_args_to_json(&args).unwrap();
        assert!(json.contains("\"buffer_id\":5"));

        let args = HookArgs::CursorMoved {
            buffer_id: BufferId(1),
            cursor_id: crate::model::event::CursorId(0),
            old_position: 10,
            new_position: 20,
            line: 5,
        };
        let json = hook_args_to_json(&args).unwrap();
        assert!(json.contains("\"buffer_id\":1"));
        assert!(json.contains("\"old_position\":10"));
        assert!(json.contains("\"new_position\":20"));
        assert!(json.contains("\"line\":5"));

        let args = HookArgs::EditorInitialized;
        let json = hook_args_to_json(&args).unwrap();
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
            const editor = getEditor();
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
            const editor = getEditor();
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
        let setup = manager
            .runtime
            .execute_script(
                "<test_hook_setup>",
                r#"
            const editor = globalThis._createPluginEditor('test');
            globalThis.onBufferActivated = function(data) {
                editor.setStatus("Buffer " + data.buffer_id + " activated");
            };
            editor.on("buffer_activated", "onBufferActivated");
            "#,
            )
            .await;
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
        writeln!(temp_file, r#"const editor = getEditor(); // Test plugin"#).unwrap();
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

    #[tokio::test]
    async fn test_emit_performance() {
        use std::time::Instant;

        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Register an event handler that does minimal work (like TODO highlighter)
        runtime
            .execute_script(
                "<setup_handler>",
                r#"
                const editor = globalThis._createPluginEditor('test');
                globalThis.onRenderLine = function(data) {
                    // Simulate TODO highlighter: check if line contains keyword
                    if (data.content && data.content.includes("TODO")) {
                        // Would add overlay here
                    }
                    return true;
                };
                editor.on("render_line", "onRenderLine");
                "#,
            )
            .await
            .unwrap();

        // Simulate rendering many lines for accurate measurement
        const NUM_LINES: usize = 100;
        const NUM_ITERATIONS: usize = 100;

        let mut total_duration = std::time::Duration::ZERO;

        for _ in 0..NUM_ITERATIONS {
            let start = Instant::now();

            for line_num in 0..NUM_LINES {
                let event_data = format!(
                    r#"{{"buffer_id": 1, "line_number": {}, "byte_start": {}, "byte_end": {}, "content": "    let x = 42; // TODO: optimize this"}}"#,
                    line_num,
                    line_num * 50,
                    (line_num + 1) * 50
                );

                let result = runtime.emit("render_line", &event_data).await;
                assert!(result.is_ok(), "Emit failed: {:?}", result);
            }

            total_duration += start.elapsed();
        }

        let avg_duration = total_duration / NUM_ITERATIONS as u32;
        let per_line_us = avg_duration.as_micros() / NUM_LINES as u128;

        println!("\n=== EMIT PERFORMANCE BENCHMARK ===");
        println!("Lines per iteration: {}", NUM_LINES);
        println!("Iterations: {}", NUM_ITERATIONS);
        println!("Average time per iteration: {:?}", avg_duration);
        println!("Average time per line: {} µs", per_line_us);
        println!("===================================\n");

        // Performance assertion: each emit should take less than 1ms (1000µs)
        // With the fix, we expect < 100µs per line
        // Without the fix, it might be > 500µs per line due to recompilation
        assert!(
            per_line_us < 1000,
            "Emit is too slow: {} µs per line (should be < 1000 µs)",
            per_line_us
        );
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_load_plugin_with_import_error() {
        // Initialize tracing subscriber for detailed logging
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        // Use the actual plugins directory which has the lib folder
        let plugins_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("plugins");
        let plugin_path = plugins_dir.join("test_import_plugin.ts");

        // Create a test plugin that imports from the lib (which exists)
        std::fs::write(
            &plugin_path,
            r#"
            const editor = getEditor();
            // Import from the actual lib folder
            import { PanelManager } from "./lib/index.ts";

            // Use the imported value
            editor.setStatus("Plugin loaded with PanelManager");
            editor.debug("PanelManager type: " + typeof PanelManager);
            "#,
        )
        .unwrap();

        // Load the plugin - this should work (or fail with an error, not hang)
        let result = manager.load_plugin(&plugin_path).await;

        // Clean up test file
        let _ = std::fs::remove_file(&plugin_path);

        // If imports work correctly, this should succeed
        // If they don't work, it should fail with an error (not hang)
        match result {
            Ok(()) => {
                // Success - check that the plugin was loaded
                let cmds = manager.process_commands();
                let has_status = cmds.iter().any(|cmd| {
                    matches!(cmd, PluginCommand::SetStatus { message } if message.contains("PanelManager"))
                });
                assert!(has_status, "Expected SetStatus with PanelManager mention");
            }
            Err(e) => {
                // If it errors, that's also acceptable (not a hang)
                // Log the error for debugging
                eprintln!("Import test failed with error: {}", e);
            }
        }
    }

    #[tokio::test]
    async fn test_ts_plugin_manager_load_plugin_with_valid_import() {
        use tempfile::TempDir;

        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut manager = TypeScriptPluginManager::new(hooks, commands).unwrap();

        // Create a temporary directory for the plugin and its import
        let temp_dir = TempDir::new().unwrap();
        let lib_path = temp_dir.path().join("lib.ts");
        let plugin_path = temp_dir.path().join("test_plugin.ts");

        // Create the library module
        std::fs::write(
            &lib_path,
            r#"
            export const MESSAGE = "Hello from lib";
            export function greet(name: string): string {
                return `Hello, ${name}!`;
            }
            "#,
        )
        .unwrap();

        // Create the plugin that imports from lib
        std::fs::write(
            &plugin_path,
            r#"
            const editor = getEditor();
            import { MESSAGE, greet } from "./lib.ts";

            editor.setStatus(MESSAGE);
            editor.debug(greet("World"));
            "#,
        )
        .unwrap();

        // Load the plugin - this should succeed
        let result = manager.load_plugin(&plugin_path).await;
        assert!(
            result.is_ok(),
            "Failed to load plugin with valid import: {:?}",
            result
        );

        // Check that the status was set with the imported message
        let cmds = manager.process_commands();
        let has_status = cmds.iter().any(|cmd| {
            matches!(cmd, PluginCommand::SetStatus { message } if message.contains("Hello from lib"))
        });
        assert!(has_status, "Expected SetStatus with imported MESSAGE");
    }

    #[test]
    fn test_plugin_thread_load_plugin_with_import() {
        use crate::services::plugins::thread::PluginThreadHandle;

        // Initialize tracing subscriber for detailed logging
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        // Create dir context
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        // Spawn the plugin thread
        let mut handle = PluginThreadHandle::spawn(commands, dir_context).unwrap();

        // Use the actual plugins directory which has the lib folder
        let plugins_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("plugins");
        let plugin_path = plugins_dir.join("test_thread_import_plugin.ts");

        // Create a test plugin that imports from the lib (which exists)
        std::fs::write(
            &plugin_path,
            r#"
            const editor = getEditor();
            // Import from the actual lib folder
            import { PanelManager } from "./lib/index.ts";

            // Use the imported value
            editor.setStatus("Plugin thread test: PanelManager loaded");
            editor.debug("PanelManager type: " + typeof PanelManager);
            "#,
        )
        .unwrap();

        // Load the plugin through the plugin thread (this could hang!)
        let result = handle.load_plugin(&plugin_path);

        // Clean up test file
        let _ = std::fs::remove_file(&plugin_path);

        // Check result
        match result {
            Ok(()) => {
                // Success - check that the plugin was loaded
                let cmds = handle.process_commands();
                let has_status = cmds.iter().any(|cmd| {
                    matches!(cmd, PluginCommand::SetStatus { message } if message.contains("PanelManager"))
                });
                assert!(has_status, "Expected SetStatus with PanelManager mention");
            }
            Err(e) => {
                // If it errors, that's also acceptable (not a hang)
                eprintln!("Plugin thread import test failed with error: {}", e);
            }
        }

        // Shutdown
        handle.shutdown();
    }

    #[test]
    fn test_plugin_thread_load_git_log_plugin() {
        use crate::services::plugins::thread::PluginThreadHandle;

        // Initialize tracing subscriber for detailed logging
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        // Create dir context
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        // Spawn the plugin thread
        let mut handle = PluginThreadHandle::spawn(commands, dir_context).unwrap();

        // Load the actual git_log.ts plugin
        let plugins_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("plugins");
        let plugin_path = plugins_dir.join("git_log.ts");

        // Load the plugin through the plugin thread (this could hang!)
        let result = handle.load_plugin(&plugin_path);

        // Check result
        match result {
            Ok(()) => {
                eprintln!("Git log plugin loaded successfully");
                // Check that the plugin was loaded
                let cmds = handle.process_commands();
                eprintln!("Commands after load: {:?}", cmds.len());
            }
            Err(e) => {
                eprintln!("Git log plugin failed with error: {}", e);
            }
        }

        // Shutdown
        handle.shutdown();
    }

    #[test]
    fn test_plugin_thread_load_vi_mode_plugin() {
        use crate::services::plugins::thread::PluginThreadHandle;

        // Initialize tracing subscriber for detailed logging
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        // Create dir context
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        // Spawn the plugin thread
        let mut handle = PluginThreadHandle::spawn(commands, dir_context).unwrap();

        // Load the vi_mode.ts plugin
        let plugins_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("plugins");
        let plugin_path = plugins_dir.join("vi_mode.ts");

        // Load the plugin through the plugin thread
        let result = handle.load_plugin(&plugin_path);

        // Check result
        match result {
            Ok(()) => {
                eprintln!("Vi mode plugin loaded successfully");
                // Check that the plugin was loaded and registered commands
                let cmds = handle.process_commands();
                eprintln!("Commands after load: {:?}", cmds.len());
                // The vi mode plugin should register the "Toggle Vi mode" command
                assert!(cmds.len() > 0, "Vi mode plugin should register commands");
            }
            Err(e) => {
                panic!("Vi mode plugin failed to load: {}", e);
            }
        }

        // Shutdown
        handle.shutdown();
    }

    #[test]
    #[ignore]
    fn test_plugin_thread_execute_git_log_action() {
        use crate::model::event::BufferId;
        use crate::services::plugins::api::PluginCommand;
        use crate::services::plugins::thread::PluginThreadHandle;

        // Initialize tracing subscriber for detailed logging
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        // Create dir context
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        // Spawn the plugin thread
        let mut handle = PluginThreadHandle::spawn(commands, dir_context).unwrap();

        // Load the actual git_log.ts plugin
        let plugins_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("plugins");
        let plugin_path = plugins_dir.join("git_log.ts");

        // Load the plugin
        let result = handle.load_plugin(&plugin_path);
        assert!(
            result.is_ok(),
            "Failed to load git_log plugin: {:?}",
            result
        );

        eprintln!("Git log plugin loaded, now executing show_git_log action...");

        // Execute the show_git_log action using async pattern
        let receiver = handle.execute_action_async("show_git_log").unwrap();

        // Simulate editor event loop: process commands while action runs
        // Note: No internal timeout - cargo nextest handles external timeout per README guidelines
        let mut completed = false;

        while !completed {
            // Process any commands from the plugin
            let cmds = handle.process_commands();
            for cmd in cmds {
                match cmd {
                    PluginCommand::CreateVirtualBufferInSplit {
                        request_id: Some(req_id),
                        ..
                    } => {
                        eprintln!(
                            "Received CreateVirtualBufferInSplit with request_id={}",
                            req_id
                        );
                        let response =
                            crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
                                request_id: req_id,
                                buffer_id: BufferId(100),
                                split_id: Some(crate::model::event::SplitId(1)),
                            };
                        handle.deliver_response(response);
                        eprintln!("Delivered response for request_id={}", req_id);
                    }
                    PluginCommand::SetStatus { message } => {
                        eprintln!("Plugin status: {}", message);
                    }
                    _ => {}
                }
            }

            // Check if action completed
            match receiver.try_recv() {
                Ok(result) => {
                    completed = true;
                    match result {
                        Ok(()) => eprintln!("show_git_log executed successfully!"),
                        Err(e) => eprintln!("show_git_log failed: {}", e),
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                    panic!("Action receiver disconnected");
                }
            }
        }

        // Shutdown
        handle.shutdown();
    }

    #[test]
    fn test_plugin_thread_spawn_process_simple() {
        use crate::services::plugins::thread::PluginThreadHandle;
        use tempfile::TempDir;

        // Initialize tracing subscriber for detailed logging
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        // Create dir context
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        // Spawn the plugin thread
        let mut handle = PluginThreadHandle::spawn(commands, dir_context).unwrap();

        // Create a simple plugin that spawns a process
        let temp_dir = TempDir::new().unwrap();
        let plugin_path = temp_dir.path().join("spawn_test.ts");

        std::fs::write(
            &plugin_path,
            r#"
            const editor = getEditor();
            globalThis.test_spawn = async function(): Promise<void> {
                editor.setStatus("About to spawn echo...");

                const result = await editor.spawnProcess("echo", ["hello", "world"]);

                editor.setStatus("Spawn completed: exit=" + result.exit_code);
                editor.debug("stdout: " + result.stdout);
            };

            editor.setStatus("Spawn test plugin loaded");
            "#,
        )
        .unwrap();

        // Load the plugin
        let result = handle.load_plugin(&plugin_path);
        assert!(
            result.is_ok(),
            "Failed to load spawn test plugin: {:?}",
            result
        );

        eprintln!("Spawn test plugin loaded, now executing test_spawn action...");

        // Execute the test_spawn action using async pattern
        let receiver = handle.execute_action_async("test_spawn").unwrap();

        // Wait for completion while processing commands
        // Note: No internal timeout - cargo nextest handles external timeout per README guidelines
        let mut completed = false;

        while !completed {
            let _cmds = handle.process_commands();
            match receiver.try_recv() {
                Ok(result) => {
                    completed = true;
                    match result {
                        Ok(()) => eprintln!("test_spawn executed successfully"),
                        Err(e) => eprintln!("test_spawn failed: {}", e),
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                    panic!("Action receiver disconnected");
                }
            }
        }

        // Shutdown
        handle.shutdown();
    }

    #[test]
    fn test_plugin_thread_spawn_git_log() {
        use crate::services::plugins::thread::PluginThreadHandle;
        use tempfile::TempDir;

        // Initialize tracing subscriber for detailed logging
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        // Create dir context
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        // Spawn the plugin thread
        let mut handle = PluginThreadHandle::spawn(commands, dir_context).unwrap();

        // Create a plugin that runs git log like the git_log plugin does
        let temp_dir = TempDir::new().unwrap();
        let plugin_path = temp_dir.path().join("git_test.ts");

        std::fs::write(
            &plugin_path,
            r#"
            const editor = getEditor();
            globalThis.test_git = async function(): Promise<void> {
                editor.setStatus("About to run git log...");

                const format = "%H%x00%h%x00%an%x00%ae%x00%ai%x00%ar%x00%d%x00%s%x00%b%x1e";
                const args = ["log", `--format=${format}`, "-n10"];

                const result = await editor.spawnProcess("git", args);

                editor.setStatus("Git log completed: exit=" + result.exit_code + ", lines=" + result.stdout.split("\n").length);
            };

            editor.setStatus("Git test plugin loaded");
            "#,
        )
        .unwrap();

        // Load the plugin
        let result = handle.load_plugin(&plugin_path);
        assert!(
            result.is_ok(),
            "Failed to load git test plugin: {:?}",
            result
        );

        eprintln!("Git test plugin loaded, now executing test_git action...");

        // Execute the test_git action using async pattern
        let receiver = handle.execute_action_async("test_git").unwrap();

        // Wait for completion while processing commands
        // Note: No internal timeout - cargo nextest handles external timeout per README guidelines
        let mut completed = false;

        while !completed {
            let _cmds = handle.process_commands();
            match receiver.try_recv() {
                Ok(result) => {
                    completed = true;
                    match result {
                        Ok(()) => eprintln!("test_git executed successfully"),
                        Err(e) => eprintln!("test_git failed: {}", e),
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                    panic!("Action receiver disconnected");
                }
            }
        }

        // Shutdown
        handle.shutdown();
    }

    #[test]
    fn test_plugin_thread_create_virtual_buffer_async() {
        use crate::model::event::BufferId;
        use crate::services::plugins::api::PluginCommand;
        use crate::services::plugins::thread::PluginThreadHandle;
        use tempfile::TempDir;

        // Initialize tracing subscriber for detailed logging
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        let commands = Arc::new(RwLock::new(CommandRegistry::new()));

        // Create dir context
        let dir_context = crate::config_io::DirectoryContext::from_system().unwrap();
        // Spawn the plugin thread
        let mut handle = PluginThreadHandle::spawn(commands, dir_context).unwrap();

        // Create a plugin that mimics git_log with debug logs
        let temp_dir = TempDir::new().unwrap();
        let plugin_path = temp_dir.path().join("vbuf_test.ts");

        std::fs::write(
            &plugin_path,
            r#"
            const editor = getEditor();
            globalThis.test_vbuf = async function(): Promise<void> {
                editor.debug("Step 1: About to spawn process...");

                const result = await editor.spawnProcess("echo", ["test"]);
                editor.debug("Step 2: Spawn completed with exit=" + result.exit_code);

                editor.debug("Step 3: About to create virtual buffer...");

                // This should hang because no editor is processing responses
                const bufferId = await editor.createVirtualBufferInSplit({
                    name: "*Test*",
                    mode: "normal",
                    read_only: true,
                    entries: [{ text: "test content\n", properties: { type: "test" } }],
                    ratio: 0.5,
                });

                editor.debug("Step 4: Virtual buffer created with id=" + bufferId);
            };

            editor.setStatus("VBuf test plugin loaded");
            "#,
        )
        .unwrap();

        // Load the plugin
        let result = handle.load_plugin(&plugin_path);
        assert!(
            result.is_ok(),
            "Failed to load vbuf test plugin: {:?}",
            result
        );

        eprintln!("VBuf test plugin loaded, now executing test_vbuf action...");
        eprintln!("Using async pattern to avoid deadlock");

        // Execute the action using async pattern (non-blocking)
        let receiver = handle.execute_action_async("test_vbuf").unwrap();

        // Simulate editor event loop: process commands while action runs
        // Note: No internal timeout - cargo nextest handles external timeout per README guidelines
        let mut completed = false;

        while !completed {
            // Process any commands from the plugin
            let cmds = handle.process_commands();
            for cmd in cmds {
                match cmd {
                    PluginCommand::CreateVirtualBufferInSplit {
                        request_id: Some(req_id),
                        ..
                    } => {
                        eprintln!(
                            "Received CreateVirtualBufferInSplit with request_id={}",
                            req_id
                        );
                        // Deliver a fake response (in real editor, this would be the actual buffer_id)
                        let response =
                            crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
                                request_id: req_id,
                                buffer_id: BufferId(100), // Fake buffer ID
                                split_id: Some(crate::model::event::SplitId(1)),
                            };
                        handle.deliver_response(response);
                        eprintln!("Delivered response for request_id={}", req_id);
                    }
                    PluginCommand::SetStatus { message } => {
                        eprintln!("Plugin status: {}", message);
                    }
                    _ => {
                        // Other commands (debug goes to tracing, not here)
                    }
                }
            }

            // Check if action completed
            match receiver.try_recv() {
                Ok(result) => {
                    completed = true;
                    match result {
                        Ok(()) => eprintln!("test_vbuf executed successfully!"),
                        Err(e) => eprintln!("test_vbuf failed: {}", e),
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {
                    // Action still running, continue processing
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                    panic!("Action receiver disconnected");
                }
            }
        }

        // Shutdown
        handle.shutdown();
    }
}
