//! Plugin API: Safe interface for plugins to interact with the editor
//!
//! This module provides a safe, controlled API for plugins (Lua, WASM, etc.)
//! to interact with the editor without direct access to internal state.

use crate::command::{Command, Suggestion};
use crate::file_explorer::FileExplorerDecoration;
use crate::hooks::{HookCallback, HookRegistry};
use crate::menu::{Menu, MenuItem};
use crate::overlay::{OverlayHandle, OverlayNamespace};
use crate::text_property::{TextProperty, TextPropertyEntry};
use crate::BufferId;
use crate::SplitId;
use lsp_types;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::ops::Range;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use ts_rs::TS;

/// Minimal command registry for PluginApi.
/// This is a stub that provides basic command storage for plugin use.
/// The editor's full CommandRegistry lives in fresh-editor.
pub struct CommandRegistry {
    commands: std::sync::RwLock<Vec<Command>>,
}

impl CommandRegistry {
    /// Create a new empty command registry
    pub fn new() -> Self {
        Self {
            commands: std::sync::RwLock::new(Vec::new()),
        }
    }

    /// Register a command
    pub fn register(&self, command: Command) {
        let mut commands = self.commands.write().unwrap();
        commands.retain(|c| c.name != command.name);
        commands.push(command);
    }

    /// Unregister a command by name  
    pub fn unregister(&self, name: &str) {
        let mut commands = self.commands.write().unwrap();
        commands.retain(|c| c.name != name);
    }
}

impl Default for CommandRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// A callback ID for JavaScript promises in the plugin runtime.
///
/// This newtype distinguishes JS promise callbacks (resolved via `resolve_callback`)
/// from Rust oneshot channel IDs (resolved via `send_plugin_response`).
/// Using a newtype prevents accidentally mixing up these two callback mechanisms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct JsCallbackId(pub u64);

impl JsCallbackId {
    /// Create a new JS callback ID
    pub fn new(id: u64) -> Self {
        Self(id)
    }

    /// Get the underlying u64 value
    pub fn as_u64(self) -> u64 {
        self.0
    }
}

impl From<u64> for JsCallbackId {
    fn from(id: u64) -> Self {
        Self(id)
    }
}

impl From<JsCallbackId> for u64 {
    fn from(id: JsCallbackId) -> u64 {
        id.0
    }
}

impl std::fmt::Display for JsCallbackId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Result of creating a virtual buffer
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct VirtualBufferResult {
    /// The created buffer ID
    #[ts(type = "number")]
    pub buffer_id: u64,
    /// The split ID (if created in a new split)
    #[ts(type = "number | null")]
    pub split_id: Option<u64>,
}

/// Response from the editor for async plugin operations
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum PluginResponse {
    /// Response to CreateVirtualBufferInSplit with the created buffer ID and split ID
    VirtualBufferCreated {
        request_id: u64,
        buffer_id: BufferId,
        split_id: Option<SplitId>,
    },
    /// Response to a plugin-initiated LSP request
    LspRequest {
        request_id: u64,
        #[ts(type = "any")]
        result: Result<JsonValue, String>,
    },
    /// Response to RequestHighlights
    HighlightsComputed {
        request_id: u64,
        spans: Vec<TsHighlightSpan>,
    },
    /// Response to GetBufferText with the text content
    BufferText {
        request_id: u64,
        text: Result<String, String>,
    },
    /// Response to CreateCompositeBuffer with the created buffer ID
    CompositeBufferCreated {
        request_id: u64,
        buffer_id: BufferId,
    },
}

/// Messages sent from async plugin tasks to the synchronous main loop
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum PluginAsyncMessage {
    /// Plugin process completed with output
    ProcessOutput {
        /// Unique ID for this process
        process_id: u64,
        /// Standard output
        stdout: String,
        /// Standard error
        stderr: String,
        /// Exit code
        exit_code: i32,
    },
    /// Plugin delay/timer completed
    DelayComplete {
        /// Callback ID to resolve
        callback_id: u64,
    },
    /// Background process stdout data
    ProcessStdout { process_id: u64, data: String },
    /// Background process stderr data
    ProcessStderr { process_id: u64, data: String },
    /// Background process exited
    ProcessExit {
        process_id: u64,
        callback_id: u64,
        exit_code: i32,
    },
    /// Response for a plugin-initiated LSP request
    LspResponse {
        language: String,
        request_id: u64,
        #[ts(type = "any")]
        result: Result<JsonValue, String>,
    },
    /// Generic plugin response (e.g., GetBufferText result)
    PluginResponse(crate::api::PluginResponse),
}

/// Information about a cursor in the editor
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct CursorInfo {
    /// Byte position of the cursor
    pub position: usize,
    /// Selection range (if any)
    #[cfg_attr(
        feature = "plugins",
        ts(type = "{ start: number; end: number } | null")
    )]
    pub selection: Option<Range<usize>>,
}

/// Specification for an action to execute, with optional repeat count
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct ActionSpec {
    /// Action name (e.g., "move_word_right", "delete_line")
    pub action: String,
    /// Number of times to repeat the action (default 1)
    pub count: u32,
}

/// Information about a buffer
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct BufferInfo {
    /// Buffer ID
    #[ts(type = "number")]
    pub id: BufferId,
    /// File path (if any)
    #[serde(serialize_with = "serialize_path")]
    #[ts(type = "string")]
    pub path: Option<PathBuf>,
    /// Whether the buffer has been modified
    pub modified: bool,
    /// Length of buffer in bytes
    pub length: usize,
}

fn serialize_path<S: serde::Serializer>(path: &Option<PathBuf>, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str(
        &path
            .as_ref()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default(),
    )
}

/// Serialize ranges as [start, end] tuples for JS compatibility
fn serialize_ranges_as_tuples<S>(ranges: &[Range<usize>], serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    use serde::ser::SerializeSeq;
    let mut seq = serializer.serialize_seq(Some(ranges.len()))?;
    for range in ranges {
        seq.serialize_element(&(range.start, range.end))?;
    }
    seq.end()
}

/// Serialize optional ranges as [start, end] tuples for JS compatibility
fn serialize_opt_ranges_as_tuples<S>(
    ranges: &Option<Vec<Range<usize>>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    match ranges {
        Some(ranges) => {
            use serde::ser::SerializeSeq;
            let mut seq = serializer.serialize_seq(Some(ranges.len()))?;
            for range in ranges {
                seq.serialize_element(&(range.start, range.end))?;
            }
            seq.end()
        }
        None => serializer.serialize_none(),
    }
}

/// Diff between current buffer content and last saved snapshot
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct BufferSavedDiff {
    pub equal: bool,
    #[serde(serialize_with = "serialize_ranges_as_tuples")]
    #[ts(type = "Array<[number, number]>")]
    pub byte_ranges: Vec<Range<usize>>,
    #[serde(serialize_with = "serialize_opt_ranges_as_tuples")]
    #[ts(type = "Array<[number, number]> | null")]
    pub line_ranges: Option<Vec<Range<usize>>>,
}

/// Information about the viewport
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
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

/// Layout hints supplied by plugins (e.g., Compose mode)
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct LayoutHints {
    /// Optional compose width for centering/wrapping
    pub compose_width: Option<u16>,
    /// Optional column guides for aligned tables
    pub column_guides: Option<Vec<u16>>,
}

// ============================================================================
// Composite Buffer Configuration (for multi-buffer single-tab views)
// ============================================================================

/// Layout configuration for composite buffers
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export, rename = "TsCompositeLayoutConfig")]
pub struct CompositeLayoutConfig {
    /// Layout type: "side-by-side", "stacked", or "unified"
    #[serde(rename = "type")]
    #[ts(rename = "type")]
    pub layout_type: String,
    /// Width ratios for side-by-side (e.g., [0.5, 0.5])
    #[serde(default)]
    pub ratios: Option<Vec<f32>>,
    /// Show separator between panes
    #[serde(default = "default_true", rename = "showSeparator")]
    #[ts(rename = "showSeparator")]
    pub show_separator: bool,
    /// Spacing for stacked layout
    #[serde(default)]
    pub spacing: Option<u16>,
}

fn default_true() -> bool {
    true
}

/// Source pane configuration for composite buffers
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export, rename = "TsCompositeSourceConfig")]
pub struct CompositeSourceConfig {
    /// Buffer ID of the source buffer (required)
    #[serde(rename = "bufferId")]
    #[ts(rename = "bufferId")]
    pub buffer_id: usize,
    /// Label for this pane (e.g., "OLD", "NEW")
    pub label: String,
    /// Whether this pane is editable
    #[serde(default)]
    pub editable: bool,
    /// Style configuration
    #[serde(default)]
    pub style: Option<CompositePaneStyle>,
}

/// Style configuration for a composite pane
#[derive(Debug, Clone, Serialize, Deserialize, Default, TS)]
#[serde(deny_unknown_fields)]
#[ts(export, rename = "TsCompositePaneStyle")]
pub struct CompositePaneStyle {
    /// Background color for added lines (RGB)
    /// Using [u8; 3] instead of (u8, u8, u8) for better rquickjs_serde compatibility
    #[serde(default, rename = "addBg")]
    #[ts(rename = "addBg", type = "[number, number, number] | null")]
    pub add_bg: Option<[u8; 3]>,
    /// Background color for removed lines (RGB)
    #[serde(default, rename = "removeBg")]
    #[ts(rename = "removeBg", type = "[number, number, number] | null")]
    pub remove_bg: Option<[u8; 3]>,
    /// Background color for modified lines (RGB)
    #[serde(default, rename = "modifyBg")]
    #[ts(rename = "modifyBg", type = "[number, number, number] | null")]
    pub modify_bg: Option<[u8; 3]>,
    /// Gutter style: "line-numbers", "diff-markers", "both", or "none"
    #[serde(default, rename = "gutterStyle")]
    #[ts(rename = "gutterStyle")]
    pub gutter_style: Option<String>,
}

/// Diff hunk for composite buffer alignment
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export, rename = "TsCompositeHunk")]
pub struct CompositeHunk {
    /// Starting line in old buffer (0-indexed)
    #[serde(rename = "oldStart")]
    #[ts(rename = "oldStart")]
    pub old_start: usize,
    /// Number of lines in old buffer
    #[serde(rename = "oldCount")]
    #[ts(rename = "oldCount")]
    pub old_count: usize,
    /// Starting line in new buffer (0-indexed)
    #[serde(rename = "newStart")]
    #[ts(rename = "newStart")]
    pub new_start: usize,
    /// Number of lines in new buffer
    #[serde(rename = "newCount")]
    #[ts(rename = "newCount")]
    pub new_count: usize,
}

/// Options for creating a composite buffer (used by plugin API)
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export, rename = "TsCreateCompositeBufferOptions")]
pub struct CreateCompositeBufferOptions {
    /// Buffer name (displayed in tabs/title)
    #[serde(default)]
    pub name: String,
    /// Mode for keybindings
    #[serde(default)]
    pub mode: String,
    /// Layout configuration
    pub layout: CompositeLayoutConfig,
    /// Source pane configurations
    pub sources: Vec<CompositeSourceConfig>,
    /// Diff hunks for alignment (optional)
    #[serde(default)]
    pub hunks: Option<Vec<CompositeHunk>>,
}

/// Wire-format view token kind (serialized for plugin transforms)
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum ViewTokenWireKind {
    Text(String),
    Newline,
    Space,
    /// Visual line break inserted by wrapping (not from source)
    /// Always has source_offset: None
    Break,
    /// A single binary byte that should be rendered as <XX>
    /// Used in binary file mode to ensure cursor positioning works correctly
    /// (all 4 display chars of <XX> map to the same source byte)
    BinaryByte(u8),
}

/// Styling for view tokens (used for injected annotations)
///
/// This allows plugins to specify styling for tokens that don't have a source
/// mapping (source_offset: None), such as annotation headers in git blame.
/// For tokens with source_offset: Some(_), syntax highlighting is applied instead.
#[derive(Debug, Clone, Serialize, Deserialize, Default, TS)]
#[ts(export)]
pub struct ViewTokenStyle {
    /// Foreground color as RGB tuple
    #[serde(default)]
    #[ts(type = "[number, number, number] | null")]
    pub fg: Option<(u8, u8, u8)>,
    /// Background color as RGB tuple
    #[serde(default)]
    #[ts(type = "[number, number, number] | null")]
    pub bg: Option<(u8, u8, u8)>,
    /// Whether to render in bold
    #[serde(default)]
    pub bold: bool,
    /// Whether to render in italic
    #[serde(default)]
    pub italic: bool,
}

/// Wire-format view token with optional source mapping and styling
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct ViewTokenWire {
    /// Source byte offset in the buffer. None for injected content (annotations).
    pub source_offset: Option<usize>,
    /// The token content
    pub kind: ViewTokenWireKind,
    /// Optional styling for injected content (only used when source_offset is None)
    #[serde(default)]
    #[ts(optional)]
    pub style: Option<ViewTokenStyle>,
}

/// Transformed view stream payload (plugin-provided)
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct ViewTransformPayload {
    /// Byte range this transform applies to (viewport)
    pub range: Range<usize>,
    /// Tokens in wire format
    pub tokens: Vec<ViewTokenWire>,
    /// Layout hints
    pub layout_hints: Option<LayoutHints>,
}

/// Snapshot of editor state for plugin queries
/// This is updated by the editor on each loop iteration
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct EditorStateSnapshot {
    /// Currently active buffer ID
    pub active_buffer_id: BufferId,
    /// Currently active split ID
    pub active_split_id: usize,
    /// Information about all open buffers
    pub buffers: HashMap<BufferId, BufferInfo>,
    /// Diff vs last saved snapshot for each buffer (line counts may be unknown)
    pub buffer_saved_diffs: HashMap<BufferId, BufferSavedDiff>,
    /// Primary cursor position for the active buffer
    pub primary_cursor: Option<CursorInfo>,
    /// All cursor positions for the active buffer
    pub all_cursors: Vec<CursorInfo>,
    /// Viewport information for the active buffer
    pub viewport: Option<ViewportInfo>,
    /// Cursor positions per buffer (for buffers other than active)
    pub buffer_cursor_positions: HashMap<BufferId, usize>,
    /// Text properties per buffer (for virtual buffers with properties)
    pub buffer_text_properties: HashMap<BufferId, Vec<TextProperty>>,
    /// Selected text from the primary cursor (if any selection exists)
    /// This is populated on each update to avoid needing full buffer access
    pub selected_text: Option<String>,
    /// Internal clipboard content (for plugins that need clipboard access)
    pub clipboard: String,
    /// Editor's working directory (for file operations and spawning processes)
    pub working_dir: PathBuf,
    /// LSP diagnostics per file URI
    /// Maps file URI string to Vec of diagnostics for that file
    #[ts(type = "any")]
    pub diagnostics: HashMap<String, Vec<lsp_types::Diagnostic>>,
    /// Runtime config as serde_json::Value (merged user config + defaults)
    /// This is the runtime config, not just the user's config file
    #[ts(type = "any")]
    pub config: serde_json::Value,
    /// User config as serde_json::Value (only what's in the user's config file)
    /// Fields not present here are using default values
    #[ts(type = "any")]
    pub user_config: serde_json::Value,
    /// Global editor mode for modal editing (e.g., "vi-normal", "vi-insert")
    /// When set, this mode's keybindings take precedence over normal key handling
    pub editor_mode: Option<String>,
}

impl EditorStateSnapshot {
    pub fn new() -> Self {
        Self {
            active_buffer_id: BufferId(0),
            active_split_id: 0,
            buffers: HashMap::new(),
            buffer_saved_diffs: HashMap::new(),
            primary_cursor: None,
            all_cursors: Vec::new(),
            viewport: None,
            buffer_cursor_positions: HashMap::new(),
            buffer_text_properties: HashMap::new(),
            selected_text: None,
            clipboard: String::new(),
            working_dir: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
            diagnostics: HashMap::new(),
            config: serde_json::Value::Null,
            user_config: serde_json::Value::Null,
            editor_mode: None,
        }
    }
}

impl Default for EditorStateSnapshot {
    fn default() -> Self {
        Self::new()
    }
}

/// Position for inserting menu items or menus
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum MenuPosition {
    /// Add at the beginning
    Top,
    /// Add at the end
    Bottom,
    /// Add before a specific label
    Before(String),
    /// Add after a specific label
    After(String),
}

/// Plugin command - allows plugins to send commands to the editor
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
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

    /// Add an overlay to a buffer, returns handle via response channel
    AddOverlay {
        buffer_id: BufferId,
        namespace: Option<OverlayNamespace>,
        range: Range<usize>,
        color: (u8, u8, u8),
        bg_color: Option<(u8, u8, u8)>,
        underline: bool,
        bold: bool,
        italic: bool,
        extend_to_line_end: bool,
    },

    /// Remove an overlay by its opaque handle
    RemoveOverlay {
        buffer_id: BufferId,
        handle: OverlayHandle,
    },

    /// Set status message
    SetStatus { message: String },

    /// Apply a theme by name
    ApplyTheme { theme_name: String },

    /// Reload configuration from file
    /// After a plugin saves config changes, it should call this to reload the config
    ReloadConfig,

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
        callback_id: JsCallbackId,
    },

    /// Delay/sleep for a duration (async, resolves callback when done)
    Delay {
        callback_id: JsCallbackId,
        duration_ms: u64,
    },

    /// Spawn a long-running background process
    /// Unlike SpawnProcess, this returns immediately with a process handle
    /// and provides streaming output via hooks
    SpawnBackgroundProcess {
        /// Unique ID for this process (generated by plugin runtime)
        process_id: u64,
        /// Command to execute
        command: String,
        /// Arguments to pass
        args: Vec<String>,
        /// Working directory (optional)
        cwd: Option<String>,
        /// Callback ID to call when process exits
        callback_id: JsCallbackId,
    },

    /// Kill a background process by ID
    KillBackgroundProcess { process_id: u64 },

    /// Wait for a process to complete and get its result
    /// Used with processes started via SpawnProcess
    SpawnProcessWait {
        /// Process ID to wait for
        process_id: u64,
        /// Callback ID for async response
        callback_id: JsCallbackId,
    },

    /// Set layout hints for a buffer/viewport
    SetLayoutHints {
        buffer_id: BufferId,
        split_id: Option<SplitId>,
        range: Range<usize>,
        hints: LayoutHints,
    },

    /// Enable/disable line numbers for a buffer
    SetLineNumbers { buffer_id: BufferId, enabled: bool },

    /// Submit a transformed view stream for a viewport
    SubmitViewTransform {
        buffer_id: BufferId,
        split_id: Option<SplitId>,
        payload: ViewTransformPayload,
    },

    /// Clear view transform for a buffer/split (returns to normal rendering)
    ClearViewTransform {
        buffer_id: BufferId,
        split_id: Option<SplitId>,
    },

    /// Remove all overlays from a buffer
    ClearAllOverlays { buffer_id: BufferId },

    /// Remove all overlays in a namespace
    ClearNamespace {
        buffer_id: BufferId,
        namespace: OverlayNamespace,
    },

    /// Remove all overlays that overlap with a byte range
    /// Used for targeted invalidation when content in a range changes
    ClearOverlaysInRange {
        buffer_id: BufferId,
        start: usize,
        end: usize,
    },

    /// Add virtual text (inline text that doesn't exist in the buffer)
    /// Used for color swatches, type hints, parameter hints, etc.
    AddVirtualText {
        buffer_id: BufferId,
        virtual_text_id: String,
        position: usize,
        text: String,
        color: (u8, u8, u8),
        use_bg: bool, // true = use color as background, false = use as foreground
        before: bool, // true = before char, false = after char
    },

    /// Remove a virtual text by ID
    RemoveVirtualText {
        buffer_id: BufferId,
        virtual_text_id: String,
    },

    /// Remove virtual texts whose ID starts with the given prefix
    RemoveVirtualTextsByPrefix { buffer_id: BufferId, prefix: String },

    /// Clear all virtual texts from a buffer
    ClearVirtualTexts { buffer_id: BufferId },

    /// Add a virtual LINE (full line above/below a position)
    /// Used for git blame headers, code coverage, inline documentation, etc.
    /// These lines do NOT show line numbers in the gutter.
    AddVirtualLine {
        buffer_id: BufferId,
        /// Byte position to anchor the line to
        position: usize,
        /// Full line content to display
        text: String,
        /// Foreground color (RGB)
        fg_color: (u8, u8, u8),
        /// Background color (RGB), None = transparent
        bg_color: Option<(u8, u8, u8)>,
        /// true = above the line containing position, false = below
        above: bool,
        /// Namespace for bulk removal (e.g., "git-blame")
        namespace: String,
        /// Priority for ordering multiple lines at same position (higher = later)
        priority: i32,
    },

    /// Clear all virtual texts in a namespace
    /// This is the primary way to remove a plugin's virtual lines before updating them.
    ClearVirtualTextNamespace {
        buffer_id: BufferId,
        namespace: String,
    },

    /// Refresh lines for a buffer (clear seen_lines cache to re-trigger lines_changed hook)
    RefreshLines { buffer_id: BufferId },

    /// Set a line indicator in the gutter's indicator column
    /// Used for git gutter, breakpoints, bookmarks, etc.
    SetLineIndicator {
        buffer_id: BufferId,
        /// Line number (0-indexed)
        line: usize,
        /// Namespace for grouping (e.g., "git-gutter", "breakpoints")
        namespace: String,
        /// Symbol to display (e.g., "│", "●", "★")
        symbol: String,
        /// Color as RGB tuple
        color: (u8, u8, u8),
        /// Priority for display when multiple indicators exist (higher wins)
        priority: i32,
    },

    /// Clear all line indicators for a specific namespace
    ClearLineIndicators {
        buffer_id: BufferId,
        /// Namespace to clear (e.g., "git-gutter")
        namespace: String,
    },

    /// Set file explorer decorations for a namespace
    SetFileExplorerDecorations {
        /// Namespace for grouping (e.g., "git-status")
        namespace: String,
        /// Decorations to apply
        decorations: Vec<FileExplorerDecoration>,
    },

    /// Clear file explorer decorations for a namespace
    ClearFileExplorerDecorations {
        /// Namespace to clear (e.g., "git-status")
        namespace: String,
    },

    /// Open a file at a specific line and column
    /// Line and column are 1-indexed to match git grep output
    OpenFileAtLocation {
        path: PathBuf,
        line: Option<usize>,   // 1-indexed, None = go to start
        column: Option<usize>, // 1-indexed, None = go to line start
    },

    /// Open a file in a specific split at a given line and column
    /// Line and column are 1-indexed to match git grep output
    OpenFileInSplit {
        split_id: usize,
        path: PathBuf,
        line: Option<usize>,   // 1-indexed, None = go to start
        column: Option<usize>, // 1-indexed, None = go to line start
    },

    /// Start a prompt (minibuffer) with a custom type identifier
    /// This allows plugins to create interactive prompts
    StartPrompt {
        label: String,
        prompt_type: String, // e.g., "git-grep", "git-find-file"
    },

    /// Start a prompt with pre-filled initial value
    StartPromptWithInitial {
        label: String,
        prompt_type: String,
        initial_value: String,
    },

    /// Update the suggestions list for the current prompt
    /// Uses the editor's Suggestion type
    SetPromptSuggestions { suggestions: Vec<Suggestion> },

    /// Add a menu item to an existing menu
    /// Add a menu item to an existing menu
    AddMenuItem {
        menu_label: String,
        item: MenuItem,
        position: MenuPosition,
    },

    /// Add a new top-level menu
    AddMenu { menu: Menu, position: MenuPosition },

    /// Remove a menu item from a menu
    RemoveMenuItem {
        menu_label: String,
        item_label: String,
    },

    /// Remove a top-level menu
    RemoveMenu { menu_label: String },

    /// Create a new virtual buffer (not backed by a file)
    CreateVirtualBuffer {
        /// Display name (e.g., "*Diagnostics*")
        name: String,
        /// Mode name for buffer-local keybindings (e.g., "diagnostics-list")
        mode: String,
        /// Whether the buffer is read-only
        read_only: bool,
    },

    /// Create a virtual buffer and set its content in one operation
    /// This is preferred over CreateVirtualBuffer + SetVirtualBufferContent
    /// because it doesn't require tracking the buffer ID
    CreateVirtualBufferWithContent {
        /// Display name (e.g., "*Diagnostics*")
        name: String,
        /// Mode name for buffer-local keybindings (e.g., "diagnostics-list")
        mode: String,
        /// Whether the buffer is read-only
        read_only: bool,
        /// Entries with text and embedded properties
        entries: Vec<TextPropertyEntry>,
        /// Whether to show line numbers in the gutter
        show_line_numbers: bool,
        /// Whether to show cursors in the buffer
        show_cursors: bool,
        /// Whether editing is disabled (blocks editing commands)
        editing_disabled: bool,
        /// Whether this buffer should be hidden from tabs (for composite source buffers)
        hidden_from_tabs: bool,
        /// Optional request ID for async response
        request_id: Option<u64>,
    },

    /// Create a virtual buffer in a horizontal split
    /// Opens the buffer in a new pane below the current one
    CreateVirtualBufferInSplit {
        /// Display name (e.g., "*Diagnostics*")
        name: String,
        /// Mode name for buffer-local keybindings (e.g., "diagnostics-list")
        mode: String,
        /// Whether the buffer is read-only
        read_only: bool,
        /// Entries with text and embedded properties
        entries: Vec<TextPropertyEntry>,
        /// Split ratio (0.0 to 1.0, where 0.5 = equal split)
        ratio: f32,
        /// Split direction ("horizontal" or "vertical"), default horizontal
        direction: Option<String>,
        /// Optional panel ID for idempotent operations (if panel exists, update content)
        panel_id: Option<String>,
        /// Whether to show line numbers in the buffer (default true)
        show_line_numbers: bool,
        /// Whether to show cursors in the buffer (default true)
        show_cursors: bool,
        /// Whether editing is disabled for this buffer (default false)
        editing_disabled: bool,
        /// Whether line wrapping is enabled for this split (None = use global setting)
        line_wrap: Option<bool>,
        /// Optional request ID for async response (if set, editor will send back buffer ID)
        request_id: Option<u64>,
    },

    /// Set the content of a virtual buffer with text properties
    SetVirtualBufferContent {
        buffer_id: BufferId,
        /// Entries with text and embedded properties
        entries: Vec<TextPropertyEntry>,
    },

    /// Get text properties at the cursor position in a buffer
    GetTextPropertiesAtCursor { buffer_id: BufferId },

    /// Define a buffer mode with keybindings
    DefineMode {
        name: String,
        parent: Option<String>,
        bindings: Vec<(String, String)>, // (key_string, command_name)
        read_only: bool,
    },

    /// Switch the current split to display a buffer
    ShowBuffer { buffer_id: BufferId },

    /// Create a virtual buffer in an existing split (replaces current buffer in that split)
    CreateVirtualBufferInExistingSplit {
        /// Display name (e.g., "*Commit Details*")
        name: String,
        /// Mode name for buffer-local keybindings
        mode: String,
        /// Whether the buffer is read-only
        read_only: bool,
        /// Entries with text and embedded properties
        entries: Vec<TextPropertyEntry>,
        /// Target split ID where the buffer should be displayed
        split_id: SplitId,
        /// Whether to show line numbers in the buffer (default true)
        show_line_numbers: bool,
        /// Whether to show cursors in the buffer (default true)
        show_cursors: bool,
        /// Whether editing is disabled for this buffer (default false)
        editing_disabled: bool,
        /// Whether line wrapping is enabled for this split (None = use global setting)
        line_wrap: Option<bool>,
        /// Optional request ID for async response
        request_id: Option<u64>,
    },

    /// Close a buffer and remove it from all splits
    CloseBuffer { buffer_id: BufferId },

    /// Create a composite buffer that displays multiple source buffers
    /// Used for side-by-side diff, unified diff, and 3-way merge views
    CreateCompositeBuffer {
        /// Display name (shown in tab bar)
        name: String,
        /// Mode name for keybindings (e.g., "diff-view")
        mode: String,
        /// Layout configuration
        layout: CompositeLayoutConfig,
        /// Source pane configurations
        sources: Vec<CompositeSourceConfig>,
        /// Diff hunks for line alignment (optional)
        hunks: Option<Vec<CompositeHunk>>,
        /// Request ID for async response
        request_id: Option<u64>,
    },

    /// Update alignment for a composite buffer (e.g., after source edit)
    UpdateCompositeAlignment {
        buffer_id: BufferId,
        hunks: Vec<CompositeHunk>,
    },

    /// Close a composite buffer
    CloseCompositeBuffer { buffer_id: BufferId },

    /// Focus a specific split
    FocusSplit { split_id: SplitId },

    /// Set the buffer displayed in a specific split
    SetSplitBuffer {
        split_id: SplitId,
        buffer_id: BufferId,
    },

    /// Set the scroll position of a specific split
    SetSplitScroll { split_id: SplitId, top_byte: usize },

    /// Request syntax highlights for a buffer range
    RequestHighlights {
        buffer_id: BufferId,
        range: Range<usize>,
        request_id: u64,
    },

    /// Close a split (if not the last one)
    CloseSplit { split_id: SplitId },

    /// Set the ratio of a split container
    SetSplitRatio {
        split_id: SplitId,
        /// Ratio between 0.0 and 1.0 (0.5 = equal split)
        ratio: f32,
    },

    /// Distribute splits evenly - make all given splits equal size
    DistributeSplitsEvenly {
        /// Split IDs to distribute evenly
        split_ids: Vec<SplitId>,
    },

    /// Set cursor position in a buffer (also scrolls viewport to show cursor)
    SetBufferCursor {
        buffer_id: BufferId,
        /// Byte offset position for the cursor
        position: usize,
    },

    /// Send an arbitrary LSP request and return the raw JSON response
    SendLspRequest {
        language: String,
        method: String,
        #[ts(type = "any")]
        params: Option<JsonValue>,
        request_id: u64,
    },

    /// Set the internal clipboard content
    SetClipboard { text: String },

    /// Delete the current selection in the active buffer
    /// This deletes all selected text across all cursors
    DeleteSelection,

    /// Set or unset a custom context
    /// Custom contexts are plugin-defined states that can be used to control command visibility
    /// For example, "config-editor" context could make config editor commands available
    SetContext {
        /// Context name (e.g., "config-editor")
        name: String,
        /// Whether the context is active
        active: bool,
    },

    /// Set the hunks for the Review Diff tool
    SetReviewDiffHunks { hunks: Vec<ReviewHunk> },

    /// Execute an editor action by name (e.g., "move_word_right", "delete_line")
    /// Used by vi mode plugin to run motions and calculate cursor ranges
    ExecuteAction {
        /// Action name (e.g., "move_word_right", "move_line_end")
        action_name: String,
    },

    /// Execute multiple actions in sequence, each with an optional repeat count
    /// Used by vi mode for count prefix (e.g., "3dw" = delete 3 words)
    /// All actions execute atomically with no plugin roundtrips between them
    ExecuteActions {
        /// List of actions to execute in sequence
        actions: Vec<ActionSpec>,
    },

    /// Get text from a buffer range (for yank operations)
    GetBufferText {
        /// Buffer ID
        buffer_id: BufferId,
        /// Start byte offset
        start: usize,
        /// End byte offset
        end: usize,
        /// Request ID for async response
        request_id: u64,
    },

    /// Set the global editor mode (for modal editing like vi mode)
    /// When set, the mode's keybindings take precedence over normal editing
    SetEditorMode {
        /// Mode name (e.g., "vi-normal", "vi-insert") or None to clear
        mode: Option<String>,
    },

    /// Show an action popup with buttons for user interaction
    /// When the user selects an action, the ActionPopupResult hook is fired
    ShowActionPopup {
        /// Unique identifier for the popup (used in ActionPopupResult)
        popup_id: String,
        /// Title text for the popup
        title: String,
        /// Body message (supports basic formatting)
        message: String,
        /// Action buttons to display
        actions: Vec<ActionPopupAction>,
    },

    /// Disable LSP for a specific language and persist to config
    DisableLspForLanguage {
        /// The language to disable LSP for (e.g., "python", "rust")
        language: String,
    },

    /// Create a scroll sync group for anchor-based synchronized scrolling
    /// Used for side-by-side diff views where two panes need to scroll together
    /// The plugin provides the group ID (must be unique per plugin)
    CreateScrollSyncGroup {
        /// Plugin-assigned group ID
        group_id: u32,
        /// The left (primary) split - scroll position is tracked in this split's line space
        left_split: SplitId,
        /// The right (secondary) split - position is derived from anchors
        right_split: SplitId,
    },

    /// Set sync anchors for a scroll sync group
    /// Anchors map corresponding line numbers between left and right buffers
    SetScrollSyncAnchors {
        /// The group ID returned by CreateScrollSyncGroup
        group_id: u32,
        /// List of (left_line, right_line) pairs marking corresponding positions
        anchors: Vec<(usize, usize)>,
    },

    /// Remove a scroll sync group
    RemoveScrollSyncGroup {
        /// The group ID returned by CreateScrollSyncGroup
        group_id: u32,
    },

    /// Save a buffer to a specific file path
    /// Used by :w filename command to save unnamed buffers or save-as
    SaveBufferToPath {
        /// Buffer ID to save
        buffer_id: BufferId,
        /// Path to save to
        path: PathBuf,
    },
}

/// Hunk status for Review Diff
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, TS)]
#[ts(export)]
pub enum HunkStatus {
    Pending,
    Staged,
    Discarded,
}

/// A high-level hunk directive for the Review Diff tool
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct ReviewHunk {
    pub id: String,
    pub file: String,
    pub context_header: String,
    pub status: HunkStatus,
    /// 0-indexed line range in the base (HEAD) version
    pub base_range: Option<(usize, usize)>,
    /// 0-indexed line range in the modified (Working) version
    pub modified_range: Option<(usize, usize)>,
}

/// Action button for action popups
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export, rename = "TsActionPopupAction")]
pub struct ActionPopupAction {
    /// Unique action identifier (returned in ActionPopupResult)
    pub id: String,
    /// Display text for the button (can include command hints)
    pub label: String,
}

/// Syntax highlight span for a buffer range
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct TsHighlightSpan {
    pub start: u32,
    pub end: u32,
    #[ts(type = "[number, number, number]")]
    pub color: (u8, u8, u8),
    pub bold: bool,
    pub italic: bool,
}

/// Result from spawning a process with spawnProcess
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct SpawnResult {
    /// Complete stdout as string
    pub stdout: String,
    /// Complete stderr as string
    pub stderr: String,
    /// Process exit code (0 usually means success, -1 if killed)
    pub exit_code: i32,
}

/// Result from spawning a background process
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct BackgroundProcessResult {
    /// Unique process ID for later reference
    #[ts(type = "number")]
    pub process_id: u64,
    /// Process exit code (0 usually means success, -1 if killed)
    /// Only present when the process has exited
    pub exit_code: i32,
}

/// Entry for virtual buffer content with optional text properties (JS API version)
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export, rename = "TextPropertyEntry")]
pub struct JsTextPropertyEntry {
    /// Text content for this entry
    pub text: String,
    /// Optional properties attached to this text (e.g., file path, line number)
    #[serde(default)]
    #[ts(optional, type = "Record<string, unknown>")]
    pub properties: Option<HashMap<String, JsonValue>>,
}

/// Directory entry returned by readDir
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct DirEntry {
    /// File/directory name
    pub name: String,
    /// True if this is a file
    pub is_file: bool,
    /// True if this is a directory
    pub is_dir: bool,
}

/// Options for createVirtualBuffer
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct CreateVirtualBufferOptions {
    /// Buffer name (displayed in tabs/title)
    pub name: String,
    /// Mode for keybindings (e.g., "git-log", "search-results")
    #[serde(default)]
    #[ts(optional)]
    pub mode: Option<String>,
    /// Whether buffer is read-only (default: false)
    #[serde(default, rename = "readOnly")]
    #[ts(optional, rename = "readOnly")]
    pub read_only: Option<bool>,
    /// Show line numbers in gutter (default: false)
    #[serde(default, rename = "showLineNumbers")]
    #[ts(optional, rename = "showLineNumbers")]
    pub show_line_numbers: Option<bool>,
    /// Show cursor (default: true)
    #[serde(default, rename = "showCursors")]
    #[ts(optional, rename = "showCursors")]
    pub show_cursors: Option<bool>,
    /// Disable text editing (default: false)
    #[serde(default, rename = "editingDisabled")]
    #[ts(optional, rename = "editingDisabled")]
    pub editing_disabled: Option<bool>,
    /// Hide from tab bar (default: false)
    #[serde(default, rename = "hiddenFromTabs")]
    #[ts(optional, rename = "hiddenFromTabs")]
    pub hidden_from_tabs: Option<bool>,
    /// Initial content entries with optional properties
    #[serde(default)]
    #[ts(optional)]
    pub entries: Option<Vec<JsTextPropertyEntry>>,
}

/// Options for createVirtualBufferInSplit
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct CreateVirtualBufferInSplitOptions {
    /// Buffer name (displayed in tabs/title)
    pub name: String,
    /// Mode for keybindings (e.g., "git-log", "search-results")
    #[serde(default)]
    #[ts(optional)]
    pub mode: Option<String>,
    /// Whether buffer is read-only (default: false)
    #[serde(default, rename = "readOnly")]
    #[ts(optional, rename = "readOnly")]
    pub read_only: Option<bool>,
    /// Split ratio 0.0-1.0 (default: 0.5)
    #[serde(default)]
    #[ts(optional)]
    pub ratio: Option<f32>,
    /// Split direction: "horizontal" or "vertical"
    #[serde(default)]
    #[ts(optional)]
    pub direction: Option<String>,
    /// Panel ID to split from
    #[serde(default, rename = "panelId")]
    #[ts(optional, rename = "panelId")]
    pub panel_id: Option<String>,
    /// Show line numbers in gutter (default: true)
    #[serde(default, rename = "showLineNumbers")]
    #[ts(optional, rename = "showLineNumbers")]
    pub show_line_numbers: Option<bool>,
    /// Show cursor (default: true)
    #[serde(default, rename = "showCursors")]
    #[ts(optional, rename = "showCursors")]
    pub show_cursors: Option<bool>,
    /// Disable text editing (default: false)
    #[serde(default, rename = "editingDisabled")]
    #[ts(optional, rename = "editingDisabled")]
    pub editing_disabled: Option<bool>,
    /// Enable line wrapping
    #[serde(default, rename = "lineWrap")]
    #[ts(optional, rename = "lineWrap")]
    pub line_wrap: Option<bool>,
    /// Initial content entries with optional properties
    #[serde(default)]
    #[ts(optional)]
    pub entries: Option<Vec<JsTextPropertyEntry>>,
}

/// Options for createVirtualBufferInExistingSplit
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct CreateVirtualBufferInExistingSplitOptions {
    /// Buffer name (displayed in tabs/title)
    pub name: String,
    /// Target split ID (required)
    #[serde(rename = "splitId")]
    #[ts(rename = "splitId")]
    pub split_id: usize,
    /// Mode for keybindings (e.g., "git-log", "search-results")
    #[serde(default)]
    #[ts(optional)]
    pub mode: Option<String>,
    /// Whether buffer is read-only (default: false)
    #[serde(default, rename = "readOnly")]
    #[ts(optional, rename = "readOnly")]
    pub read_only: Option<bool>,
    /// Show line numbers in gutter (default: true)
    #[serde(default, rename = "showLineNumbers")]
    #[ts(optional, rename = "showLineNumbers")]
    pub show_line_numbers: Option<bool>,
    /// Show cursor (default: true)
    #[serde(default, rename = "showCursors")]
    #[ts(optional, rename = "showCursors")]
    pub show_cursors: Option<bool>,
    /// Disable text editing (default: false)
    #[serde(default, rename = "editingDisabled")]
    #[ts(optional, rename = "editingDisabled")]
    pub editing_disabled: Option<bool>,
    /// Enable line wrapping
    #[serde(default, rename = "lineWrap")]
    #[ts(optional, rename = "lineWrap")]
    pub line_wrap: Option<bool>,
    /// Initial content entries with optional properties
    #[serde(default)]
    #[ts(optional)]
    pub entries: Option<Vec<JsTextPropertyEntry>>,
}

/// Result of getTextPropertiesAtCursor - array of property objects
///
/// Each element contains the properties from a text property span that overlaps
/// with the cursor position. Properties are dynamic key-value pairs set by plugins.
#[derive(Debug, Clone, Serialize, TS)]
#[ts(export, type = "Array<Record<string, unknown>>")]
pub struct TextPropertiesAtCursor(pub Vec<HashMap<String, JsonValue>>);

// Implement FromJs for option types using rquickjs_serde
#[cfg(feature = "plugins")]
mod fromjs_impls {
    use super::*;
    use rquickjs::{Ctx, FromJs, Value};

    impl<'js> FromJs<'js> for JsTextPropertyEntry {
        fn from_js(_ctx: &Ctx<'js>, value: Value<'js>) -> rquickjs::Result<Self> {
            rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
                from: "object",
                to: "JsTextPropertyEntry",
                message: Some(e.to_string()),
            })
        }
    }

    impl<'js> FromJs<'js> for CreateVirtualBufferOptions {
        fn from_js(_ctx: &Ctx<'js>, value: Value<'js>) -> rquickjs::Result<Self> {
            rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
                from: "object",
                to: "CreateVirtualBufferOptions",
                message: Some(e.to_string()),
            })
        }
    }

    impl<'js> FromJs<'js> for CreateVirtualBufferInSplitOptions {
        fn from_js(_ctx: &Ctx<'js>, value: Value<'js>) -> rquickjs::Result<Self> {
            rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
                from: "object",
                to: "CreateVirtualBufferInSplitOptions",
                message: Some(e.to_string()),
            })
        }
    }

    impl<'js> FromJs<'js> for CreateVirtualBufferInExistingSplitOptions {
        fn from_js(_ctx: &Ctx<'js>, value: Value<'js>) -> rquickjs::Result<Self> {
            rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
                from: "object",
                to: "CreateVirtualBufferInExistingSplitOptions",
                message: Some(e.to_string()),
            })
        }
    }

    impl<'js> rquickjs::IntoJs<'js> for TextPropertiesAtCursor {
        fn into_js(self, ctx: &Ctx<'js>) -> rquickjs::Result<Value<'js>> {
            rquickjs_serde::to_value(ctx.clone(), &self.0)
                .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
        }
    }
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
    /// Returns an opaque handle that can be used to remove the overlay later
    #[allow(clippy::too_many_arguments)]
    pub fn add_overlay(
        &self,
        buffer_id: BufferId,
        namespace: Option<String>,
        range: Range<usize>,
        color: (u8, u8, u8),
        bg_color: Option<(u8, u8, u8)>,
        underline: bool,
        bold: bool,
        italic: bool,
        extend_to_line_end: bool,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::AddOverlay {
            buffer_id,
            namespace: namespace.map(OverlayNamespace::from_string),
            range,
            color,
            bg_color,
            underline,
            bold,
            italic,
            extend_to_line_end,
        })
    }

    /// Remove an overlay from a buffer by its handle
    pub fn remove_overlay(&self, buffer_id: BufferId, handle: String) -> Result<(), String> {
        self.send_command(PluginCommand::RemoveOverlay {
            buffer_id,
            handle: OverlayHandle::from_string(handle),
        })
    }

    /// Clear all overlays in a namespace from a buffer
    pub fn clear_namespace(&self, buffer_id: BufferId, namespace: String) -> Result<(), String> {
        self.send_command(PluginCommand::ClearNamespace {
            buffer_id,
            namespace: OverlayNamespace::from_string(namespace),
        })
    }

    /// Clear all overlays that overlap with a byte range
    /// Used for targeted invalidation when content changes
    pub fn clear_overlays_in_range(
        &self,
        buffer_id: BufferId,
        start: usize,
        end: usize,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::ClearOverlaysInRange {
            buffer_id,
            start,
            end,
        })
    }

    /// Set the status message
    pub fn set_status(&self, message: String) -> Result<(), String> {
        self.send_command(PluginCommand::SetStatus { message })
    }

    /// Open a file at a specific line and column (1-indexed)
    /// This is useful for jumping to locations from git grep, LSP definitions, etc.
    pub fn open_file_at_location(
        &self,
        path: PathBuf,
        line: Option<usize>,
        column: Option<usize>,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::OpenFileAtLocation { path, line, column })
    }

    /// Open a file in a specific split at a line and column
    ///
    /// Similar to open_file_at_location but targets a specific split pane.
    /// The split_id is the ID of the split pane to open the file in.
    pub fn open_file_in_split(
        &self,
        split_id: usize,
        path: PathBuf,
        line: Option<usize>,
        column: Option<usize>,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::OpenFileInSplit {
            split_id,
            path,
            line,
            column,
        })
    }

    /// Start a prompt (minibuffer) with a custom type identifier
    /// The prompt_type is used to filter hooks in plugin code
    pub fn start_prompt(&self, label: String, prompt_type: String) -> Result<(), String> {
        self.send_command(PluginCommand::StartPrompt { label, prompt_type })
    }

    /// Set the suggestions for the current prompt
    /// This updates the prompt's autocomplete/selection list
    pub fn set_prompt_suggestions(&self, suggestions: Vec<Suggestion>) -> Result<(), String> {
        self.send_command(PluginCommand::SetPromptSuggestions { suggestions })
    }

    /// Add a menu item to an existing menu
    pub fn add_menu_item(
        &self,
        menu_label: String,
        item: MenuItem,
        position: MenuPosition,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::AddMenuItem {
            menu_label,
            item,
            position,
        })
    }

    /// Add a new top-level menu
    pub fn add_menu(&self, menu: Menu, position: MenuPosition) -> Result<(), String> {
        self.send_command(PluginCommand::AddMenu { menu, position })
    }

    /// Remove a menu item from a menu
    pub fn remove_menu_item(&self, menu_label: String, item_label: String) -> Result<(), String> {
        self.send_command(PluginCommand::RemoveMenuItem {
            menu_label,
            item_label,
        })
    }

    /// Remove a top-level menu
    pub fn remove_menu(&self, menu_label: String) -> Result<(), String> {
        self.send_command(PluginCommand::RemoveMenu { menu_label })
    }

    // === Virtual Buffer Methods ===

    /// Create a new virtual buffer (not backed by a file)
    ///
    /// Virtual buffers are used for special displays like diagnostic lists,
    /// search results, etc. They have their own mode for keybindings.
    pub fn create_virtual_buffer(
        &self,
        name: String,
        mode: String,
        read_only: bool,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::CreateVirtualBuffer {
            name,
            mode,
            read_only,
        })
    }

    /// Create a virtual buffer and set its content in one operation
    ///
    /// This is the preferred way to create virtual buffers since it doesn't
    /// require tracking the buffer ID. The buffer is created and populated
    /// atomically.
    pub fn create_virtual_buffer_with_content(
        &self,
        name: String,
        mode: String,
        read_only: bool,
        entries: Vec<TextPropertyEntry>,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::CreateVirtualBufferWithContent {
            name,
            mode,
            read_only,
            entries,
            show_line_numbers: true,
            show_cursors: true,
            editing_disabled: false,
            hidden_from_tabs: false,
            request_id: None,
        })
    }

    /// Set the content of a virtual buffer with text properties
    ///
    /// Each entry contains text and metadata properties (e.g., source location).
    pub fn set_virtual_buffer_content(
        &self,
        buffer_id: BufferId,
        entries: Vec<TextPropertyEntry>,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::SetVirtualBufferContent { buffer_id, entries })
    }

    /// Get text properties at cursor position in a buffer
    ///
    /// This triggers a command that will make properties available to plugins.
    pub fn get_text_properties_at_cursor(&self, buffer_id: BufferId) -> Result<(), String> {
        self.send_command(PluginCommand::GetTextPropertiesAtCursor { buffer_id })
    }

    /// Define a buffer mode with keybindings
    ///
    /// Modes can inherit from parent modes (e.g., "diagnostics-list" inherits from "special").
    /// Bindings are specified as (key_string, command_name) pairs.
    pub fn define_mode(
        &self,
        name: String,
        parent: Option<String>,
        bindings: Vec<(String, String)>,
        read_only: bool,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::DefineMode {
            name,
            parent,
            bindings,
            read_only,
        })
    }

    /// Switch the current split to display a buffer
    pub fn show_buffer(&self, buffer_id: BufferId) -> Result<(), String> {
        self.send_command(PluginCommand::ShowBuffer { buffer_id })
    }

    /// Set the scroll position of a specific split
    pub fn set_split_scroll(&self, split_id: usize, top_byte: usize) -> Result<(), String> {
        self.send_command(PluginCommand::SetSplitScroll {
            split_id: SplitId(split_id),
            top_byte,
        })
    }

    /// Request syntax highlights for a buffer range
    pub fn get_highlights(
        &self,
        buffer_id: BufferId,
        range: Range<usize>,
        request_id: u64,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::RequestHighlights {
            buffer_id,
            range,
            request_id,
        })
    }

    // === Query Methods ===

    /// Get the currently active buffer ID
    pub fn get_active_buffer_id(&self) -> BufferId {
        let snapshot = self.state_snapshot.read().unwrap();
        snapshot.active_buffer_id
    }

    /// Get the currently active split ID
    pub fn get_active_split_id(&self) -> usize {
        let snapshot = self.state_snapshot.read().unwrap();
        snapshot.active_split_id
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
            Some("test-overlay".to_string()),
            0..10,
            (255, 0, 0),
            None,
            true,
            false,
            false,
            false,
        );
        assert!(result.is_ok());

        let received = rx.try_recv().unwrap();
        match received {
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
                assert_eq!(namespace.as_ref().map(|n| n.as_str()), Some("test-overlay"));
                assert_eq!(range, 0..10);
                assert_eq!(color, (255, 0, 0));
                assert_eq!(bg_color, None);
                assert!(underline);
                assert!(!bold);
                assert!(!italic);
                assert!(!extend_to_line_end);
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

    #[test]
    fn test_composite_buffer_options_rejects_unknown_fields() {
        // Valid JSON with correct field names
        let valid_json = r#"{
            "name": "test",
            "mode": "diff",
            "layout": {"type": "side-by-side", "ratios": [0.5, 0.5], "showSeparator": true},
            "sources": [{"bufferId": 1, "label": "old"}]
        }"#;
        let result: Result<CreateCompositeBufferOptions, _> = serde_json::from_str(valid_json);
        assert!(
            result.is_ok(),
            "Valid JSON should parse: {:?}",
            result.err()
        );

        // Invalid JSON with unknown field (buffer_id instead of bufferId)
        let invalid_json = r#"{
            "name": "test",
            "mode": "diff",
            "layout": {"type": "side-by-side", "ratios": [0.5, 0.5], "showSeparator": true},
            "sources": [{"buffer_id": 1, "label": "old"}]
        }"#;
        let result: Result<CreateCompositeBufferOptions, _> = serde_json::from_str(invalid_json);
        assert!(
            result.is_err(),
            "JSON with unknown field should fail to parse"
        );
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("unknown field") || err.contains("buffer_id"),
            "Error should mention unknown field: {}",
            err
        );
    }

    #[test]
    fn test_composite_hunk_rejects_unknown_fields() {
        // Valid JSON with correct field names
        let valid_json = r#"{"oldStart": 0, "oldCount": 5, "newStart": 0, "newCount": 7}"#;
        let result: Result<CompositeHunk, _> = serde_json::from_str(valid_json);
        assert!(
            result.is_ok(),
            "Valid JSON should parse: {:?}",
            result.err()
        );

        // Invalid JSON with unknown field (old_start instead of oldStart)
        let invalid_json = r#"{"old_start": 0, "oldCount": 5, "newStart": 0, "newCount": 7}"#;
        let result: Result<CompositeHunk, _> = serde_json::from_str(invalid_json);
        assert!(
            result.is_err(),
            "JSON with unknown field should fail to parse"
        );
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("unknown field") || err.contains("old_start"),
            "Error should mention unknown field: {}",
            err
        );
    }
}
