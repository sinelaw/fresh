//! Hook System: Event subscription and notification for plugins
//!
//! Hooks allow plugins to subscribe to editor events and react to them.
//! This is inspired by Emacs' hook system.

use anyhow::{anyhow, Result};

use crate::input::keybindings::Action;
use crate::model::event::{BufferId, CursorId, SplitId};
use crate::services::plugins::api::{ViewTokenWire, ViewTokenWireKind};
use std::collections::HashMap;
use std::ops::Range;
use std::path::PathBuf;

/// Arguments passed to hook callbacks
#[derive(Debug, Clone)]
pub enum HookArgs {
    /// Before a file is opened
    BeforeFileOpen { path: PathBuf },

    /// After a file is successfully opened
    AfterFileOpen { buffer_id: BufferId, path: PathBuf },

    /// Before a buffer is saved to disk
    BeforeFileSave { buffer_id: BufferId, path: PathBuf },

    /// After a buffer is successfully saved
    AfterFileSave { buffer_id: BufferId, path: PathBuf },

    /// A buffer was closed
    BufferClosed { buffer_id: BufferId },

    /// Before text is inserted
    BeforeInsert {
        buffer_id: BufferId,
        position: usize,
        text: String,
    },

    /// After text was inserted
    AfterInsert {
        buffer_id: BufferId,
        position: usize,
        text: String,
        /// Byte position where the affected range starts
        affected_start: usize,
        /// Byte position where the affected range ends (after the inserted text)
        affected_end: usize,
        /// Line number where insertion occurred (0-indexed)
        start_line: usize,
        /// Line number where insertion ended (0-indexed)
        end_line: usize,
        /// Number of lines added by this insertion
        lines_added: usize,
    },

    /// Before text is deleted
    BeforeDelete {
        buffer_id: BufferId,
        range: Range<usize>,
    },

    /// After text was deleted
    AfterDelete {
        buffer_id: BufferId,
        range: Range<usize>,
        deleted_text: String,
        /// Byte position where the deletion occurred
        affected_start: usize,
        /// Length of the deleted content in bytes
        deleted_len: usize,
        /// Line number where deletion started (0-indexed)
        start_line: usize,
        /// Line number where deletion ended (0-indexed, in original buffer)
        end_line: usize,
        /// Number of lines removed by this deletion
        lines_removed: usize,
    },

    /// Cursor moved to a new position
    CursorMoved {
        buffer_id: BufferId,
        cursor_id: CursorId,
        old_position: usize,
        new_position: usize,
        /// Line number at new position (1-indexed)
        line: usize,
    },

    /// Buffer became active
    BufferActivated { buffer_id: BufferId },

    /// Buffer was deactivated
    BufferDeactivated { buffer_id: BufferId },

    /// LSP diagnostics were updated for a file
    DiagnosticsUpdated {
        /// The URI of the file that was updated
        uri: String,
        /// Number of diagnostics in the update
        count: usize,
    },

    /// Before a command/action is executed
    PreCommand { action: Action },

    /// After a command/action was executed
    PostCommand { action: Action },

    /// Editor has been idle for N milliseconds (no input)
    Idle { milliseconds: u64 },

    /// Editor is initializing
    EditorInitialized,

    /// Rendering is starting for a buffer (called once per buffer before render_line hooks)
    /// Plugins can use this to clear overlays before they get recreated
    RenderStart { buffer_id: BufferId },

    /// A line is being rendered (called during the rendering pass)
    /// This hook fires once per visible line during each frame
    /// Plugins can inspect content and add overlays without additional traversal
    RenderLine {
        buffer_id: BufferId,
        line_number: usize,
        byte_start: usize,
        byte_end: usize,
        content: String,
    },

    /// Lines have changed and need processing (batched for efficiency)
    /// This hook fires when:
    /// - Lines become visible for the first time (viewport scroll)
    /// - Line content changes (insert/delete)
    ///
    /// Plugins should use this instead of RenderLine for better performance
    LinesChanged {
        buffer_id: BufferId,
        lines: Vec<LineInfo>,
    },

    /// Prompt input changed (user typed/edited)
    PromptChanged { prompt_type: String, input: String },

    /// Prompt was confirmed (user pressed Enter)
    PromptConfirmed {
        prompt_type: String,
        input: String,
        selected_index: Option<usize>,
    },

    /// Prompt was cancelled (user pressed Escape/Ctrl+G)
    PromptCancelled { prompt_type: String, input: String },

    /// Prompt suggestion selection changed (user navigated with Up/Down)
    PromptSelectionChanged {
        prompt_type: String,
        selected_index: usize,
    },

    /// Request keyboard shortcuts data (key, action) for the help buffer
    KeyboardShortcuts { bindings: Vec<(String, String)> },

    /// LSP find references response received
    LspReferences {
        /// The symbol name being queried
        symbol: String,
        /// The locations where the symbol is referenced
        locations: Vec<LspLocation>,
    },

    /// View transform request - core pushes base tokens to plugins for transformation
    /// Plugins receive the tokenized viewport content and can transform it
    /// (e.g., converting newlines to soft breaks for markdown compose mode).
    /// Plugin should call submitViewTransform() with transformed tokens.
    ViewTransformRequest {
        buffer_id: BufferId,
        split_id: SplitId,
        /// Byte offset of the viewport start
        viewport_start: usize,
        /// Byte offset of the viewport end
        viewport_end: usize,
        /// Base tokens (Text, Newline, Space) from the source
        tokens: Vec<ViewTokenWire>,
    },

    /// Mouse click event - fired when user clicks in the editor viewport
    /// Plugins can use this to implement clickable UI elements
    MouseClick {
        /// Column (x coordinate) in screen cells
        column: u16,
        /// Row (y coordinate) in screen cells
        row: u16,
        /// Mouse button: "left", "right", "middle"
        button: String,
        /// Modifier keys: "shift", "ctrl", "alt", or combinations like "shift+ctrl"
        modifiers: String,
        /// Content area X offset (where the buffer content starts on screen)
        content_x: u16,
        /// Content area Y offset (where the buffer content starts on screen)
        content_y: u16,
    },

    /// Mouse move/hover event - fired when mouse moves in the editor viewport
    /// Plugins can use this to implement hover effects
    MouseMove {
        /// Column (x coordinate) in screen cells
        column: u16,
        /// Row (y coordinate) in screen cells
        row: u16,
        /// Content area X offset (where the buffer content starts on screen)
        content_x: u16,
        /// Content area Y offset (where the buffer content starts on screen)
        content_y: u16,
    },

    /// LSP server request (server -> client)
    /// This hook fires when the LSP server sends a custom request method
    /// that isn't handled by the core. Plugins can use this to handle
    /// language-specific extension methods.
    LspServerRequest {
        /// The language/server that sent the request (e.g., "csharp", "rust")
        language: String,
        /// The JSON-RPC method name (e.g., "workspace/_roslyn_projectNeedsRestore")
        method: String,
        /// The server command that was used to spawn this LSP (e.g., "csharp-language-server")
        server_command: String,
        /// The request parameters as a JSON string (may be null)
        params: Option<String>,
    },

    /// Viewport changed (scrolled or resized)
    ViewportChanged {
        split_id: SplitId,
        buffer_id: BufferId,
        top_byte: usize,
        width: u16,
        height: u16,
    },

    /// LSP server failed to start or crashed
    /// This hook fires when an LSP server encounters an error during startup
    /// or unexpectedly terminates. Plugins can use this to offer installation
    /// instructions or disable LSP for the affected language.
    LspServerError {
        /// The language that failed (e.g., "python", "rust")
        language: String,
        /// The server command that failed (e.g., "pylsp", "rust-analyzer")
        server_command: String,
        /// Error type: "not_found", "spawn_failed", "timeout", "crash"
        error_type: String,
        /// Human-readable error message
        message: String,
    },

    /// User clicked the LSP status indicator in the status bar
    /// This hook fires when the user clicks on the LSP status area,
    /// allowing plugins to show contextual help or actions.
    LspStatusClicked {
        /// The language of the current buffer
        language: String,
        /// Whether there's an active error for this language
        has_error: bool,
    },

    /// User selected an action from an action popup
    /// This hook fires when the user selects an action or dismisses a popup
    /// created with showActionPopup.
    ActionPopupResult {
        /// The popup ID (set when showing popup)
        popup_id: String,
        /// The action ID selected, or "dismissed" if closed without selection
        action_id: String,
    },
}

/// Information about a single line for the LinesChanged hook
#[derive(Debug, Clone)]
pub struct LineInfo {
    /// Line number (0-based)
    pub line_number: usize,
    /// Byte offset where the line starts in the buffer
    pub byte_start: usize,
    /// Byte offset where the line ends (exclusive)
    pub byte_end: usize,
    /// The content of the line
    pub content: String,
}

/// Location information for LSP references
#[derive(Debug, Clone)]
pub struct LspLocation {
    /// File path
    pub file: String,
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub column: u32,
}

/// Type for hook callbacks
/// Returns `true` to continue execution, `false` to cancel the operation
pub type HookCallback = Box<dyn Fn(&HookArgs) -> bool + Send + Sync>;

/// Registry for managing hooks
pub struct HookRegistry {
    /// Map from hook name to list of callbacks
    hooks: HashMap<String, Vec<HookCallback>>,
}

impl HookRegistry {
    /// Create a new hook registry
    pub fn new() -> Self {
        Self {
            hooks: HashMap::new(),
        }
    }

    /// Add a hook callback for a specific hook name
    ///
    /// # Arguments
    /// * `name` - Name of the hook (e.g., "after-file-save")
    /// * `callback` - Callback function to invoke when hook is triggered
    pub fn add_hook(&mut self, name: &str, callback: HookCallback) {
        self.hooks
            .entry(name.to_string())
            .or_default()
            .push(callback);
    }

    /// Remove all hooks for a specific name
    pub fn remove_hooks(&mut self, name: &str) {
        self.hooks.remove(name);
    }

    /// Run all hooks for a specific name
    ///
    /// Returns `true` if all hooks returned true (continue execution)
    /// Returns `false` if any hook returned false (cancel operation)
    pub fn run_hooks(&self, name: &str, args: &HookArgs) -> bool {
        if let Some(hooks) = self.hooks.get(name) {
            for callback in hooks {
                if !callback(args) {
                    tracing::debug!("Hook '{}' cancelled operation", name);
                    return false; // Hook cancelled operation
                }
            }
        }
        true
    }

    /// Run hooks with timeout protection
    ///
    /// Returns `true` if all hooks completed successfully within timeout
    pub fn run_hooks_with_timeout(
        &self,
        name: &str,
        args: &HookArgs,
        timeout: std::time::Duration,
    ) -> bool {
        use std::time::Instant;

        let start = Instant::now();

        if let Some(hooks) = self.hooks.get(name) {
            for (i, callback) in hooks.iter().enumerate() {
                if start.elapsed() > timeout {
                    tracing::warn!(
                        "Hook '{}' timeout exceeded at callback {} ({:?})",
                        name,
                        i,
                        start.elapsed()
                    );
                    return true; // Continue but warn
                }

                if !callback(args) {
                    return false; // Hook cancelled
                }
            }
        }
        true
    }

    /// Get count of registered callbacks for a hook
    pub fn hook_count(&self, name: &str) -> usize {
        self.hooks.get(name).map(|v| v.len()).unwrap_or(0)
    }

    /// Get all registered hook names
    pub fn hook_names(&self) -> Vec<String> {
        self.hooks.keys().cloned().collect()
    }
}

impl Default for HookRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert HookArgs to JSON string for plugin communication
pub fn hook_args_to_json(args: &HookArgs) -> Result<String> {
    let json_value = match args {
        HookArgs::RenderStart { buffer_id } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
            })
        }
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
        HookArgs::DiagnosticsUpdated { uri, count } => {
            serde_json::json!({
                "uri": uri,
                "count": count,
            })
        }
        HookArgs::BufferClosed { buffer_id } => {
            serde_json::json!({ "buffer_id": buffer_id.0 })
        }
        HookArgs::CursorMoved {
            buffer_id,
            cursor_id,
            old_position,
            new_position,
            line,
        } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "cursor_id": cursor_id.0,
                "old_position": old_position,
                "new_position": new_position,
                "line": line,
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
            affected_start,
            affected_end,
            start_line,
            end_line,
            lines_added,
        } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "position": position,
                "text": text,
                "affected_start": affected_start,
                "affected_end": affected_end,
                "start_line": start_line,
                "end_line": end_line,
                "lines_added": lines_added,
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
            affected_start,
            deleted_len,
            start_line,
            end_line,
            lines_removed,
        } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "start": range.start,
                "end": range.end,
                "deleted_text": deleted_text,
                "affected_start": affected_start,
                "deleted_len": deleted_len,
                "start_line": start_line,
                "end_line": end_line,
                "lines_removed": lines_removed,
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
        HookArgs::PromptSelectionChanged {
            prompt_type,
            selected_index,
        } => {
            serde_json::json!({
                "prompt_type": prompt_type,
                "selected_index": selected_index,
            })
        }
        HookArgs::KeyboardShortcuts { bindings } => {
            let entries: Vec<serde_json::Value> = bindings
                .iter()
                .map(|(key, action)| serde_json::json!({ "key": key, "action": action }))
                .collect();
            serde_json::json!({ "bindings": entries })
        }
        HookArgs::LspReferences { symbol, locations } => {
            let locs: Vec<serde_json::Value> = locations
                .iter()
                .map(|loc| {
                    serde_json::json!({
                        "file": loc.file,
                        "line": loc.line,
                        "column": loc.column,
                    })
                })
                .collect();
            serde_json::json!({ "symbol": symbol, "locations": locs })
        }
        HookArgs::LinesChanged { buffer_id, lines } => {
            let lines_json: Vec<serde_json::Value> = lines
                .iter()
                .map(|line| {
                    serde_json::json!({
                        "line_number": line.line_number,
                        "byte_start": line.byte_start,
                        "byte_end": line.byte_end,
                        "content": line.content,
                    })
                })
                .collect();
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "lines": lines_json,
            })
        }
        HookArgs::ViewTransformRequest {
            buffer_id,
            split_id,
            viewport_start,
            viewport_end,
            tokens,
        } => {
            let tokens_json: Vec<serde_json::Value> = tokens
                .iter()
                .map(|token| {
                    let kind_json = match &token.kind {
                        ViewTokenWireKind::Text(s) => serde_json::json!({ "Text": s }),
                        ViewTokenWireKind::Newline => serde_json::json!("Newline"),
                        ViewTokenWireKind::Space => serde_json::json!("Space"),
                        ViewTokenWireKind::Break => serde_json::json!("Break"),
                        ViewTokenWireKind::BinaryByte(b) => serde_json::json!({ "BinaryByte": b }),
                    };
                    serde_json::json!({
                        "source_offset": token.source_offset,
                        "kind": kind_json,
                    })
                })
                .collect();
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "split_id": split_id.0,
                "viewport_start": viewport_start,
                "viewport_end": viewport_end,
                "tokens": tokens_json,
            })
        }
        HookArgs::MouseClick {
            column,
            row,
            button,
            modifiers,
            content_x,
            content_y,
        } => {
            serde_json::json!({
                "column": column,
                "row": row,
                "button": button,
                "modifiers": modifiers,
                "content_x": content_x,
                "content_y": content_y,
            })
        }
        HookArgs::MouseMove {
            column,
            row,
            content_x,
            content_y,
        } => {
            serde_json::json!({
                "column": column,
                "row": row,
                "content_x": content_x,
                "content_y": content_y,
            })
        }
        HookArgs::LspServerRequest {
            language,
            method,
            server_command,
            params,
        } => {
            serde_json::json!({
                "language": language,
                "method": method,
                "server_command": server_command,
                "params": params,
            })
        }
        HookArgs::ViewportChanged {
            split_id,
            buffer_id,
            top_byte,
            width,
            height,
        } => {
            serde_json::json!({
                "split_id": split_id.0,
                "buffer_id": buffer_id.0,
                "top_byte": top_byte,
                "width": width,
                "height": height,
            })
        }
        HookArgs::LspServerError {
            language,
            server_command,
            error_type,
            message,
        } => {
            serde_json::json!({
                "language": language,
                "server_command": server_command,
                "error_type": error_type,
                "message": message,
            })
        }
        HookArgs::LspStatusClicked {
            language,
            has_error,
        } => {
            serde_json::json!({
                "language": language,
                "has_error": has_error,
            })
        }
        HookArgs::ActionPopupResult {
            popup_id,
            action_id,
        } => {
            serde_json::json!({
                "popup_id": popup_id,
                "action_id": action_id,
            })
        }
    };

    serde_json::to_string(&json_value).map_err(|e| anyhow!("Failed to serialize hook args: {}", e))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hook_registry_creation() {
        let registry = HookRegistry::new();
        assert_eq!(registry.hook_count("any-hook"), 0);
        assert_eq!(registry.hook_names().len(), 0);
    }

    #[test]
    fn test_add_and_run_hook() {
        let mut registry = HookRegistry::new();
        let _called = false;

        // Can't capture mutable reference in Send callback, so use a different approach
        registry.add_hook(
            "test-hook",
            Box::new(|_args| {
                // Hook was called
                true
            }),
        );

        assert_eq!(registry.hook_count("test-hook"), 1);

        let args = HookArgs::EditorInitialized;
        let result = registry.run_hooks("test-hook", &args);
        assert!(result);
    }

    #[test]
    fn test_hook_cancellation() {
        let mut registry = HookRegistry::new();

        // First hook returns false (cancels)
        registry.add_hook("cancel-test", Box::new(|_args| false));

        // Second hook should not be called
        registry.add_hook(
            "cancel-test",
            Box::new(|_args| {
                panic!("Should not be called after cancellation");
            }),
        );

        let args = HookArgs::EditorInitialized;
        let result = registry.run_hooks("cancel-test", &args);
        assert!(!result);
    }

    #[test]
    fn test_multiple_hooks() {
        let mut registry = HookRegistry::new();

        registry.add_hook("multi-test", Box::new(|_args| true));
        registry.add_hook("multi-test", Box::new(|_args| true));
        registry.add_hook("multi-test", Box::new(|_args| true));

        assert_eq!(registry.hook_count("multi-test"), 3);

        let args = HookArgs::EditorInitialized;
        let result = registry.run_hooks("multi-test", &args);
        assert!(result);
    }

    #[test]
    fn test_remove_hooks() {
        let mut registry = HookRegistry::new();

        registry.add_hook("remove-test", Box::new(|_args| true));
        assert_eq!(registry.hook_count("remove-test"), 1);

        registry.remove_hooks("remove-test");
        assert_eq!(registry.hook_count("remove-test"), 0);
    }

    #[test]
    fn test_run_nonexistent_hook() {
        let registry = HookRegistry::new();
        let args = HookArgs::EditorInitialized;
        let result = registry.run_hooks("nonexistent", &args);
        assert!(result); // Should succeed (no hooks to fail)
    }

    #[test]
    fn test_hook_args_variants() {
        let registry = HookRegistry::new();

        // Test different hook arg variants
        let test_cases = vec![
            HookArgs::BeforeFileOpen {
                path: PathBuf::from("/test.txt"),
            },
            HookArgs::AfterFileOpen {
                buffer_id: BufferId(1),
                path: PathBuf::from("/test.txt"),
            },
            HookArgs::BeforeFileSave {
                buffer_id: BufferId(1),
                path: PathBuf::from("/test.txt"),
            },
            HookArgs::AfterFileSave {
                buffer_id: BufferId(1),
                path: PathBuf::from("/test.txt"),
            },
            HookArgs::BufferClosed {
                buffer_id: BufferId(1),
            },
            HookArgs::BeforeInsert {
                buffer_id: BufferId(1),
                position: 0,
                text: "test".to_string(),
            },
            HookArgs::AfterInsert {
                buffer_id: BufferId(1),
                position: 0,
                text: "test".to_string(),
                affected_start: 0,
                affected_end: 4,
                start_line: 0,
                end_line: 0,
                lines_added: 0,
            },
            HookArgs::BeforeDelete {
                buffer_id: BufferId(1),
                range: 0..5,
            },
            HookArgs::AfterDelete {
                buffer_id: BufferId(1),
                range: 0..5,
                deleted_text: "test".to_string(),
                affected_start: 0,
                deleted_len: 4,
                start_line: 0,
                end_line: 0,
                lines_removed: 0,
            },
            HookArgs::CursorMoved {
                buffer_id: BufferId(1),
                cursor_id: CursorId(0),
                old_position: 0,
                new_position: 5,
                line: 1,
            },
            HookArgs::BufferActivated {
                buffer_id: BufferId(1),
            },
            HookArgs::BufferDeactivated {
                buffer_id: BufferId(1),
            },
            HookArgs::PreCommand {
                action: Action::Save,
            },
            HookArgs::PostCommand {
                action: Action::Save,
            },
            HookArgs::Idle { milliseconds: 500 },
            HookArgs::EditorInitialized,
            HookArgs::LspServerError {
                language: "python".to_string(),
                server_command: "pylsp".to_string(),
                error_type: "not_found".to_string(),
                message: "Server not found".to_string(),
            },
            HookArgs::LspStatusClicked {
                language: "rust".to_string(),
                has_error: true,
            },
            HookArgs::ActionPopupResult {
                popup_id: "test-popup".to_string(),
                action_id: "copy_pip".to_string(),
            },
        ];

        // All should run without panicking
        for args in test_cases {
            let result = registry.run_hooks("test", &args);
            assert!(result);
        }
    }

    #[test]
    fn test_lsp_hook_args_serialization() {
        // Test LspServerError serialization
        let args = HookArgs::LspServerError {
            language: "python".to_string(),
            server_command: "pylsp".to_string(),
            error_type: "not_found".to_string(),
            message: "Server 'pylsp' not found in PATH".to_string(),
        };
        let json = hook_args_to_json(&args).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["language"], "python");
        assert_eq!(parsed["server_command"], "pylsp");
        assert_eq!(parsed["error_type"], "not_found");
        assert!(parsed["message"].as_str().unwrap().contains("pylsp"));

        // Test LspStatusClicked serialization
        let args = HookArgs::LspStatusClicked {
            language: "rust".to_string(),
            has_error: true,
        };
        let json = hook_args_to_json(&args).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["language"], "rust");
        assert_eq!(parsed["has_error"], true);

        // Test ActionPopupResult serialization
        let args = HookArgs::ActionPopupResult {
            popup_id: "python-lsp-help".to_string(),
            action_id: "copy_pip".to_string(),
        };
        let json = hook_args_to_json(&args).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["popup_id"], "python-lsp-help");
        assert_eq!(parsed["action_id"], "copy_pip");
    }

    #[test]
    fn test_hook_timeout() {
        use std::time::Duration;

        let mut registry = HookRegistry::new();

        // Add a slow hook (simulated)
        registry.add_hook(
            "timeout-test",
            Box::new(|_args| {
                // In real scenario, this would be a long-running operation
                true
            }),
        );

        let args = HookArgs::EditorInitialized;
        let result =
            registry.run_hooks_with_timeout("timeout-test", &args, Duration::from_millis(10));
        assert!(result); // Should complete quickly
    }
}
