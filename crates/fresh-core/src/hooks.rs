//! Hook System: Event subscription and notification for plugins
//!
//! Hooks allow plugins to subscribe to editor events and react to them.

use anyhow::Result;
use std::collections::HashMap;
use std::path::PathBuf;

use crate::action::Action;
use crate::api::ViewTokenWire;
use crate::{BufferId, CursorId, SplitId};

/// Arguments passed to hook callbacks
#[derive(Debug, Clone, serde::Serialize)]
#[serde(untagged)]
pub enum HookArgs {
    /// Before a file is opened
    BeforeFileOpen { path: PathBuf },

    /// After a file is successfully opened
    AfterFileOpen { buffer_id: BufferId, path: PathBuf },

    /// Before a buffer is saved to disk
    BeforeFileSave { buffer_id: BufferId, path: PathBuf },

    /// After a buffer is successfully saved
    AfterFileSave { buffer_id: BufferId, path: PathBuf },

    /// The file explorer mutated the filesystem (paste, duplicate, ...)
    /// without going through a buffer save. Plugins that surface
    /// filesystem-derived state (git status decorations, etc.) use this
    /// to re-scan after explorer-driven changes that wouldn't otherwise
    /// fire `BeforeFileSave`/`AfterFileSave`. `path` is one of the
    /// affected paths; for batch operations (multi-paste) the hook
    /// fires once per refresh, not once per file.
    AfterFileExplorerChange { path: PathBuf },

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
        start: usize,
        end: usize,
    },

    /// After text was deleted
    AfterDelete {
        buffer_id: BufferId,
        start: usize,
        end: usize,
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
        /// Text properties at the new cursor position
        text_properties: Vec<std::collections::HashMap<String, serde_json::Value>>,
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
    EditorInitialized {},

    /// All plugin packages + init.ts have been loaded. Fires after the
    /// plugin discovery loop and before session restore — the lifecycle
    /// hook for code that wants to configure a plugin via its
    /// getPluginApi(...) surface. See design §3.3 (phase 2).
    PluginsLoaded {},

    /// Editor has completed startup: plugins are loaded, session is
    /// restored, and the active buffer exists. Design §3.3 (phase 3).
    Ready {},

    /// The editor's active authority changed (e.g. local → container,
    /// container → local). Fires after the new authority is in place
    /// and the plugin state snapshot has been refreshed, so handlers
    /// can read the new label via `editor.getAuthorityLabel()`.
    /// Plugins use this to re-register state-dependent commands
    /// that should only appear in one authority mode (e.g. dev
    /// container `Detach` only when attached). In production a
    /// transition triggers a full editor restart that re-runs plugin
    /// init from scratch; this hook lets plugins react inline
    /// without that, which keeps the harness in sync too.
    AuthorityChanged { label: String },

    /// Rendering is starting for a buffer (called once per buffer before render_line hooks)
    RenderStart { buffer_id: BufferId },

    /// A line is being rendered (called during the rendering pass)
    RenderLine {
        buffer_id: BufferId,
        line_number: usize,
        byte_start: usize,
        byte_end: usize,
        content: String,
    },

    /// Lines have changed and need processing (batched for efficiency)
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

    /// View transform request
    ViewTransformRequest {
        buffer_id: BufferId,
        split_id: SplitId,
        /// Byte offset of the viewport start
        viewport_start: usize,
        /// Byte offset of the viewport end
        viewport_end: usize,
        /// Base tokens (Text, Newline, Space) from the source
        tokens: Vec<ViewTokenWire>,
        /// Byte positions of all cursors in this buffer
        cursor_positions: Vec<usize>,
    },

    /// Mouse click event
    MouseClick {
        /// Column (x coordinate) in screen cells
        column: u16,
        /// Row (y coordinate) in screen cells
        row: u16,
        /// Mouse button: "left", "right", "middle"
        button: String,
        /// Modifier keys
        modifiers: String,
        /// Content area X offset
        content_x: u16,
        /// Content area Y offset
        content_y: u16,
        /// Buffer under the click (None when the click is outside any
        /// buffer panel).
        buffer_id: Option<u64>,
        /// 0-indexed buffer row (line number) of the click, accounting
        /// for scroll. None when the click is outside any buffer.
        buffer_row: Option<u32>,
        /// 0-indexed byte column inside the buffer row. None when the
        /// click is outside any buffer.
        buffer_col: Option<u32>,
    },

    /// Mouse move/hover event
    MouseMove {
        /// Column (x coordinate) in screen cells
        column: u16,
        /// Row (y coordinate) in screen cells
        row: u16,
        /// Content area X offset
        content_x: u16,
        /// Content area Y offset
        content_y: u16,
    },

    /// LSP server request (server -> client)
    LspServerRequest {
        /// The language/server that sent the request
        language: String,
        /// The JSON-RPC method name
        method: String,
        /// The server command used to spawn this LSP
        server_command: String,
        /// The request parameters as a JSON string
        params: Option<String>,
    },

    /// Viewport changed (scrolled or resized)
    ViewportChanged {
        split_id: SplitId,
        buffer_id: BufferId,
        top_byte: usize,
        top_line: Option<usize>,
        width: u16,
        height: u16,
    },

    /// LSP server failed to start or crashed
    LspServerError {
        /// The language that failed
        language: String,
        /// The server command that failed
        server_command: String,
        /// Error type: "not_found", "spawn_failed", "timeout", "crash"
        error_type: String,
        /// Human-readable error message
        message: String,
    },

    /// User clicked the LSP status indicator
    LspStatusClicked {
        /// The language of the current buffer
        language: String,
        /// Whether there's an active error
        has_error: bool,
        /// Commands of configured servers whose binaries are not on `$PATH`
        /// (or absolute-path equivalents). Empty when every configured
        /// server is installed. Plugins can inspect this to show tailored
        /// install hints without waiting for a failed spawn.
        missing_servers: Vec<String>,
        /// Whether the user previously dismissed the LSP pill for this
        /// language (via the popup's "Disable" action). Plugins seeing
        /// this as `true` should offer "Enable" / "Install" rather than
        /// "Start".
        user_dismissed: bool,
    },

    /// User selected an action from an action popup
    ActionPopupResult {
        /// The popup ID
        popup_id: String,
        /// The action ID selected, or "dismissed"
        action_id: String,
    },

    /// Background process output (streaming)
    ProcessOutput {
        /// The process ID
        process_id: u64,
        /// The output data
        data: String,
    },

    /// Buffer language was changed (e.g. via "Set Language" command or Save-As)
    LanguageChanged {
        buffer_id: BufferId,
        /// The new language identifier (e.g., "markdown", "rust", "text")
        language: String,
    },

    /// Request to inspect a theme key in the theme editor
    ThemeInspectKey {
        /// The name of the current theme
        theme_name: String,
        /// The theme key to inspect (e.g. "editor.bg")
        key: String,
    },

    /// Mouse scroll event (wheel up/down)
    MouseScroll {
        buffer_id: BufferId,
        /// Scroll delta: negative = up, positive = down (typically ±3)
        delta: i32,
        /// Mouse column (0-based, terminal origin top-left)
        col: u16,
        /// Mouse row (0-based, terminal origin top-left)
        row: u16,
    },

    /// Terminal was resized
    Resize { width: u16, height: u16 },

    /// Terminal focus was gained (e.g. user switched back to the editor)
    FocusGained {},
}

/// Information about a single line for the LinesChanged hook
#[derive(Debug, Clone, serde::Serialize)]
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
#[derive(Debug, Clone, serde::Serialize)]
pub struct LspLocation {
    /// File path
    pub file: String,
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub column: u32,
}

/// Type for hook callbacks
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
    pub fn run_hooks(&self, name: &str, args: &HookArgs) -> bool {
        if let Some(hooks) = self.hooks.get(name) {
            for callback in hooks {
                if !callback(args) {
                    return false;
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

/// Convert HookArgs to a serde_json::Value for plugin communication.
///
/// `HookArgs` is `#[serde(untagged)]`, so each variant serializes as its
/// fields only — no discriminant wrapper. Empty struct variants (`{}`) produce
/// an empty JSON object rather than `null`.
pub fn hook_args_to_json(args: &HookArgs) -> Result<serde_json::Value> {
    Ok(serde_json::to_value(args)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    fn noop_true() -> HookCallback {
        Box::new(|_| true)
    }

    /// Adding, listing, counting, and removing hooks behave consistently:
    /// counts match the number added, names reflect the keys, and removal
    /// purges all callbacks for that key.
    #[test]
    fn add_count_list_remove_round_trip() {
        let mut reg = HookRegistry::new();
        assert_eq!(reg.hook_count("a"), 0);
        assert!(reg.hook_names().is_empty());

        reg.add_hook("a", noop_true());
        reg.add_hook("a", noop_true());
        reg.add_hook("b", noop_true());

        assert_eq!(reg.hook_count("a"), 2);
        assert_eq!(reg.hook_count("b"), 1);
        assert_eq!(reg.hook_count("missing"), 0);

        let mut names = reg.hook_names();
        names.sort();
        assert_eq!(names, vec!["a".to_string(), "b".to_string()]);

        reg.remove_hooks("a");
        assert_eq!(reg.hook_count("a"), 0);
        assert_eq!(reg.hook_count("b"), 1);
        assert_eq!(reg.hook_names(), vec!["b".to_string()]);
    }

    /// `run_hooks` returns true iff every callback returned true, short-circuits
    /// on the first `false`, and returns true for hook names with no callbacks.
    #[test]
    fn run_hooks_all_true_and_short_circuits_on_false() {
        let mut reg = HookRegistry::new();
        let args = HookArgs::EditorInitialized {};

        // Unknown hook: treated as "no callbacks" → true.
        assert!(reg.run_hooks("unknown", &args));

        // All-true chain returns true and calls every callback.
        let calls = Arc::new(AtomicUsize::new(0));
        for _ in 0..3 {
            let c = calls.clone();
            reg.add_hook(
                "all_true",
                Box::new(move |_| {
                    c.fetch_add(1, Ordering::SeqCst);
                    true
                }),
            );
        }
        assert!(reg.run_hooks("all_true", &args));
        assert_eq!(calls.load(Ordering::SeqCst), 3);

        // Short-circuits on the first `false` — the second callback must not run.
        let calls = Arc::new(AtomicUsize::new(0));
        let c1 = calls.clone();
        reg.add_hook(
            "short",
            Box::new(move |_| {
                c1.fetch_add(1, Ordering::SeqCst);
                false
            }),
        );
        let c2 = calls.clone();
        reg.add_hook(
            "short",
            Box::new(move |_| {
                c2.fetch_add(1, Ordering::SeqCst);
                true
            }),
        );
        assert!(!reg.run_hooks("short", &args));
        assert_eq!(calls.load(Ordering::SeqCst), 1);
    }

    /// `hook_args_to_json` produces an object with the expected field for
    /// a representative variant — ensuring the function actually serializes
    /// the payload instead of returning a default (null) value.
    #[test]
    fn hook_args_to_json_serializes_payload_fields() {
        let json = hook_args_to_json(&HookArgs::DiagnosticsUpdated {
            uri: "file:///x.rs".into(),
            count: 7,
        })
        .unwrap();
        assert_eq!(json["uri"], "file:///x.rs");
        assert_eq!(json["count"], 7);
    }

    #[test]
    fn hook_args_to_json_empty_variants_produce_empty_object() {
        for args in [
            HookArgs::EditorInitialized {},
            HookArgs::PluginsLoaded {},
            HookArgs::Ready {},
            HookArgs::FocusGained {},
        ] {
            let json = hook_args_to_json(&args).unwrap();
            assert_eq!(
                json,
                serde_json::json!({}),
                "variant should serialize as {{}}"
            );
        }
    }

    #[test]
    fn hook_args_to_json_delete_fields_are_flat() {
        let json = hook_args_to_json(&HookArgs::BeforeDelete {
            buffer_id: crate::BufferId(1),
            start: 10,
            end: 20,
        })
        .unwrap();
        assert_eq!(json["start"], 10);
        assert_eq!(json["end"], 20);
        assert!(json.get("range").is_none(), "range must not be nested");
    }
}
