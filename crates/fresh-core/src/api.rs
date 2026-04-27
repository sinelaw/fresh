//! Plugin API: Safe interface for plugins to interact with the editor
//!
//! This module provides a safe, controlled API for plugins (Lua, WASM, etc.)
//! to interact with the editor without direct access to internal state.
//!
//! # Type Safety Architecture
//!
//! Rust structs in this module serve as the **single source of truth** for the
//! TypeScript plugin API. The type safety system works as follows:
//!
//! ```text
//! Rust struct                  Generated TypeScript
//! ───────────                  ────────────────────
//! #[derive(TS, Deserialize)]   type ActionPopupOptions = {
//! #[serde(deny_unknown_fields)]    id: string;
//! struct ActionPopupOptions {      title: string;
//!     id: String,                  message: string;
//!     title: String,               actions: TsActionPopupAction[];
//!     ...                      };
//! }
//! ```
//!
//! ## Key Patterns
//!
//! 1. **`#[derive(TS)]`** - Generates TypeScript type definitions via ts-rs
//! 2. **`#[serde(deny_unknown_fields)]`** - Rejects typos/unknown fields at runtime
//! 3. **`impl FromJs`** - Bridges rquickjs values to typed Rust structs
//!
//! ## Validation Layers
//!
//! | Layer                  | What it catches                          |
//! |------------------------|------------------------------------------|
//! | TypeScript compile     | Wrong field names, missing required fields |
//! | Rust runtime (serde)   | Typos like `popup_id` instead of `id`    |
//! | Rust compile           | Type mismatches in method signatures     |
//!
//! ## Limitations & Tradeoffs
//!
//! - **Manual parsing for complex types**: Some methods (e.g., `submitViewTransform`)
//!   still use manual object parsing due to enum serialization complexity
//! - **Two-step deserialization**: Complex nested structs may need
//!   `rquickjs::Value → serde_json::Value → typed struct` due to rquickjs_serde limits
//! - **Duplicate attributes**: Both `#[serde(...)]` and `#[ts(...)]` needed since
//!   they control different things (runtime serialization vs compile-time codegen)

use crate::command::{Command, Suggestion};
use crate::file_explorer::FileExplorerDecoration;
use crate::hooks::{HookCallback, HookRegistry};
use crate::menu::{Menu, MenuItem};
use crate::overlay::{OverlayHandle, OverlayNamespace};
use crate::text_property::{TextProperty, TextPropertyEntry};
use crate::BufferId;
use crate::SplitId;
use crate::TerminalId;
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

/// Result of creating a terminal
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct TerminalResult {
    /// The created buffer ID (for use with setSplitBuffer, etc.)
    #[ts(type = "number")]
    pub buffer_id: u64,
    /// The terminal ID (for use with sendTerminalInput, closeTerminal)
    #[ts(type = "number")]
    pub terminal_id: u64,
    /// The split ID (if created in a new split)
    #[ts(type = "number | null")]
    pub split_id: Option<u64>,
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

/// A rectangular region, in cells. Used by the animation plugin API so
/// callers can target arbitrary screen regions without going through a
/// virtual buffer.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct AnimationRect {
    pub x: u16,
    pub y: u16,
    pub width: u16,
    pub height: u16,
}

/// Edge a slide-in effect enters from.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub enum PluginAnimationEdge {
    Top,
    Bottom,
    Left,
    Right,
}

/// Plugin-facing animation description. Tagged by `kind`. Additional
/// variants can be added later; plugins must handle the `kind` they send.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, TS)]
#[serde(tag = "kind", rename_all = "camelCase")]
#[ts(export)]
pub enum PluginAnimationKind {
    #[serde(rename_all = "camelCase")]
    SlideIn {
        from: PluginAnimationEdge,
        duration_ms: u32,
        delay_ms: u32,
    },
}

/// Result of creating a buffer group
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct BufferGroupResult {
    /// The group ID
    #[ts(type = "number")]
    pub group_id: u64,
    /// Panel buffer IDs, keyed by panel name
    #[ts(type = "Record<string, number>")]
    pub panels: HashMap<String, u64>,
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
    /// Response to CreateTerminal with the created buffer, terminal, and split IDs
    TerminalCreated {
        request_id: u64,
        buffer_id: BufferId,
        terminal_id: TerminalId,
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
    /// Response to GetLineStartPosition with the byte offset
    LineStartPosition {
        request_id: u64,
        /// None if line is out of range, Some(offset) for valid line
        position: Option<usize>,
    },
    /// Response to GetLineEndPosition with the byte offset
    LineEndPosition {
        request_id: u64,
        /// None if line is out of range, Some(offset) for valid line
        position: Option<usize>,
    },
    /// Response to GetBufferLineCount with the total number of lines
    BufferLineCount {
        request_id: u64,
        /// None if buffer not found, Some(count) for valid buffer
        count: Option<usize>,
    },
    /// Response to CreateCompositeBuffer with the created buffer ID
    CompositeBufferCreated {
        request_id: u64,
        buffer_id: BufferId,
    },
    /// Response to GetSplitByLabel with the found split ID (if any)
    SplitByLabel {
        request_id: u64,
        split_id: Option<SplitId>,
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

    /// Streaming grep: partial results for one file
    GrepStreamingProgress {
        /// Search ID to route to the correct progress callback
        search_id: u64,
        /// Matches from a single file
        matches_json: String,
    },

    /// Streaming grep: search complete
    GrepStreamingComplete {
        /// Search ID
        search_id: u64,
        /// Callback ID for the completion promise
        callback_id: u64,
        /// Total number of matches found
        total_matches: usize,
        /// Whether the search was stopped early due to reaching max_results
        truncated: bool,
    },
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
#[serde(deny_unknown_fields)]
#[ts(export)]
pub struct ActionSpec {
    /// Action name (e.g., "move_word_right", "delete_line")
    pub action: String,
    /// Number of times to repeat the action (default 1)
    #[serde(default = "default_action_count")]
    pub count: u32,
}

fn default_action_count() -> u32 {
    1
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
    /// Whether this is a virtual buffer (not backed by a file)
    pub is_virtual: bool,
    /// Current view mode of the active split: "source" or "compose"
    pub view_mode: String,
    /// True if any split showing this buffer has compose mode enabled.
    /// Plugins should use this (not `view_mode`) to decide whether to maintain
    /// decorations, since decorations live on the buffer and are filtered
    /// per-split at render time.
    pub is_composing_in_any_split: bool,
    /// Compose width (if set), from the active split's view state
    pub compose_width: Option<u16>,
    /// The detected language for this buffer (e.g., "rust", "markdown", "text")
    pub language: String,
    /// Whether this tab was opened in "preview" (ephemeral) mode — true when
    /// opened via single-click in the file explorer and not yet committed
    /// (no edit, no double-click, no tab-click, no layout change). Plugins
    /// that react to buffer lifecycle events should generally treat preview
    /// buffers as transient; e.g. a diagnostics panel may want to skip
    /// refreshing itself for a preview tab.
    #[serde(default)]
    pub is_preview: bool,
    /// Split ids that currently hold this buffer (empty when the buffer is
    /// open but not visible in any split — e.g. background-opened tabs
    /// that haven't been focused). Lets plugins implement "focus existing
    /// buffer if visible, else open new" without having to track split
    /// ids across editor restarts (which reassign them). The list is a
    /// snapshot at the last `update_plugin_state_snapshot` tick.
    #[serde(default)]
    #[ts(type = "number[]")]
    pub splits: Vec<SplitId>,
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

/// Diff between current buffer content and last saved snapshot
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct BufferSavedDiff {
    pub equal: bool,
    #[serde(serialize_with = "serialize_ranges_as_tuples")]
    #[ts(type = "Array<[number, number]>")]
    pub byte_ranges: Vec<Range<usize>>,
}

/// Information about the viewport
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct ViewportInfo {
    /// Byte position of the first visible line
    pub top_byte: usize,
    /// Line number of the first visible line (None when line index unavailable, e.g. large file before scan)
    pub top_line: Option<usize>,
    /// Left column offset (horizontal scroll)
    pub left_column: usize,
    /// Viewport width
    pub width: u16,
    /// Viewport height
    pub height: u16,
}

/// Per-split state surfaced to plugins via `editor.listSplits()`.
///
/// Plugins that need to operate on every visible buffer (multi-split
/// flash labels, syncing decorations across panes, ...) can iterate
/// this list rather than only seeing the active split's `getViewport()`.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct SplitSnapshot {
    /// Stable split identifier; matches the values used by
    /// `setSplitBuffer`, `focusSplit`, `getSplitByLabel`, etc.
    pub split_id: usize,
    /// Buffer currently shown in this split.
    pub buffer_id: BufferId,
    /// Viewport (top byte / dimensions) for this split's active buffer.
    pub viewport: ViewportInfo,
}

/// Payload delivered to a plugin's `editor.getNextKey()` Promise when
/// the next keypress arrives in the editor's input dispatch.
///
/// `key` uses the same naming as `defineMode` bindings: lowercase
/// names like `"escape"`, `"enter"`, `"tab"`, `"space"`, `"left"`,
/// `"f1"`–`"f12"`, or a single character (e.g. `"a"`, `"!"`).
/// Modifier flags are reported separately so plugins can recognise
/// chord variants without parsing.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct KeyEventPayload {
    /// Key name (e.g. `"a"`, `"escape"`, `"f1"`).
    pub key: String,
    /// Ctrl held.
    pub ctrl: bool,
    /// Alt held.
    pub alt: bool,
    /// Shift held (only meaningful for non-character keys; for
    /// printable characters the case is already encoded in `key`).
    pub shift: bool,
    /// Super / Cmd / Meta held.
    pub meta: bool,
}

/// Layout hints supplied by plugins (e.g., Compose mode)
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct LayoutHints {
    /// Optional compose width for centering/wrapping
    #[ts(optional)]
    pub compose_width: Option<u16>,
    /// Optional column guides for aligned tables
    #[ts(optional)]
    pub column_guides: Option<Vec<u16>>,
}

// ============================================================================
// Overlay Types with Theme Support
// ============================================================================

/// Color specification that can be either RGB values or a theme key.
///
/// Theme keys reference colors from the current theme, e.g.:
/// - "ui.status_bar_bg" - UI status bar background
/// - "editor.selection_bg" - Editor selection background
/// - "syntax.keyword" - Syntax highlighting for keywords
/// - "diagnostic.error" - Error diagnostic color
///
/// When a theme key is used, the color is resolved at render time,
/// so overlays automatically update when the theme changes.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(untagged)]
#[ts(export)]
pub enum OverlayColorSpec {
    /// RGB color as [r, g, b] array
    #[ts(type = "[number, number, number]")]
    Rgb(u8, u8, u8),
    /// Theme key reference (e.g., "ui.status_bar_bg")
    ThemeKey(String),
}

impl OverlayColorSpec {
    /// Create an RGB color spec
    pub fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self::Rgb(r, g, b)
    }

    /// Create a theme key color spec
    pub fn theme_key(key: impl Into<String>) -> Self {
        Self::ThemeKey(key.into())
    }

    /// Convert to RGB if this is an RGB spec, None if it's a theme key
    pub fn as_rgb(&self) -> Option<(u8, u8, u8)> {
        match self {
            Self::Rgb(r, g, b) => Some((*r, *g, *b)),
            Self::ThemeKey(_) => None,
        }
    }

    /// Get the theme key if this is a theme key spec
    pub fn as_theme_key(&self) -> Option<&str> {
        match self {
            Self::ThemeKey(key) => Some(key),
            Self::Rgb(_, _, _) => None,
        }
    }
}

/// Options for adding an overlay with theme support.
///
/// This struct provides a type-safe way to specify overlay styling
/// with optional theme key references for colors.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
#[derive(Default)]
pub struct OverlayOptions {
    /// Foreground color - RGB array or theme key string
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fg: Option<OverlayColorSpec>,

    /// Background color - RGB array or theme key string
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bg: Option<OverlayColorSpec>,

    /// Whether to render with underline
    #[serde(default)]
    pub underline: bool,

    /// Whether to render in bold
    #[serde(default)]
    pub bold: bool,

    /// Whether to render in italic
    #[serde(default)]
    pub italic: bool,

    /// Whether to render with strikethrough
    #[serde(default)]
    pub strikethrough: bool,

    /// Whether to extend background color to end of line
    #[serde(default)]
    pub extend_to_line_end: bool,

    /// Optional URL for OSC 8 terminal hyperlinks.
    /// When set, the overlay text becomes a clickable hyperlink in terminals
    /// that support OSC 8 escape sequences.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
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
    #[ts(optional)]
    pub ratios: Option<Vec<f32>>,
    /// Show separator between panes
    #[serde(default = "default_true", rename = "showSeparator")]
    #[ts(rename = "showSeparator")]
    pub show_separator: bool,
    /// Spacing for stacked layout
    #[serde(default)]
    #[ts(optional)]
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
    #[ts(optional, rename = "addBg", type = "[number, number, number]")]
    pub add_bg: Option<[u8; 3]>,
    /// Background color for removed lines (RGB)
    #[serde(default, rename = "removeBg")]
    #[ts(optional, rename = "removeBg", type = "[number, number, number]")]
    pub remove_bg: Option<[u8; 3]>,
    /// Background color for modified lines (RGB)
    #[serde(default, rename = "modifyBg")]
    #[ts(optional, rename = "modifyBg", type = "[number, number, number]")]
    pub modify_bg: Option<[u8; 3]>,
    /// Gutter style: "line-numbers", "diff-markers", "both", or "none"
    #[serde(default, rename = "gutterStyle")]
    #[ts(optional, rename = "gutterStyle")]
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
    /// When set, the first render will scroll to center the Nth hunk (0-indexed).
    /// This avoids timing issues with imperative scroll commands that depend on
    /// render-created state (viewport dimensions, view state).
    #[serde(default, rename = "initialFocusHunk")]
    #[ts(optional, rename = "initialFocusHunk")]
    pub initial_focus_hunk: Option<usize>,
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
/// mapping (sourceOffset: None), such as annotation headers in git blame.
/// For tokens with sourceOffset: Some(_), syntax highlighting is applied instead.
#[derive(Debug, Clone, Serialize, Deserialize, Default, TS)]
#[serde(deny_unknown_fields)]
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
#[serde(deny_unknown_fields)]
#[ts(export)]
pub struct ViewTokenWire {
    /// Source byte offset in the buffer. None for injected content (annotations).
    #[ts(type = "number | null")]
    pub source_offset: Option<usize>,
    /// The token content
    pub kind: ViewTokenWireKind,
    /// Optional styling for injected content (only used when source_offset is None)
    #[serde(default, skip_serializing_if = "Option::is_none")]
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
    /// Per-split snapshots: split id, buffer shown, viewport.
    /// Includes the active split.  Order is unspecified.
    #[serde(default)]
    pub splits: Vec<SplitSnapshot>,
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
    /// Status-bar / explorer label for the active authority.
    ///
    /// Empty = the local (default) authority with nothing to render.
    /// Non-empty means a non-local authority is installed (e.g.
    /// `"Container:abc123def456"` for a devcontainer). Plugins can
    /// read this via `editor.getAuthorityLabel()` to detect "already
    /// attached" without having to track state across editor restarts.
    #[serde(default)]
    pub authority_label: String,
    /// LSP diagnostics per file URI.
    /// Maps file URI string to Vec of diagnostics for that file.
    ///
    /// Wrapped in `Arc` so snapshot refresh is a refcount bump rather than
    /// a deep clone. The editor only mutates its own map through
    /// `Arc::make_mut`, which CoW-clones while this snapshot still holds
    /// a reference — a reader can never observe an in-place mutation.
    ///
    /// `#[serde(skip)]`: serde out-of-the-box can't serialize `Arc<T>`
    /// (behind the `rc` cargo feature we don't enable). We never serialize
    /// the snapshot as a whole — plugin readers pull out these Arcs and
    /// serialize the *inner* value directly (e.g. `get_all_diagnostics`).
    #[serde(skip)]
    #[ts(type = "any")]
    pub diagnostics: Arc<HashMap<String, Vec<lsp_types::Diagnostic>>>,
    /// LSP folding ranges per file URI.
    /// Maps file URI string to Vec of folding ranges for that file.
    /// Arc-wrapped for the same CoW invariant as `diagnostics`; see that
    /// field for why this is `#[serde(skip)]`.
    #[serde(skip)]
    #[ts(type = "any")]
    pub folding_ranges: Arc<HashMap<String, Vec<lsp_types::FoldingRange>>>,
    /// Runtime config as serde_json::Value (merged user config + defaults).
    /// This is the runtime config, not just the user's config file.
    ///
    /// Wrapped in `Arc` so the snapshot update is a refcount bump. The
    /// editor reserializes its source `Config` only when the underlying
    /// `Arc<Config>` pointer has moved (i.e., after a real mutation), and
    /// swaps the whole `Arc<Value>` atomically — callers never see a
    /// partially-updated blob. `#[serde(skip)]` for the same reason as
    /// `diagnostics`.
    #[serde(skip)]
    #[ts(type = "any")]
    pub config: Arc<serde_json::Value>,
    /// User config as serde_json::Value (only what's in the user's config file).
    /// Fields not present here are using default values.
    /// Arc-wrapped; swapped as a whole when the user's file is reloaded.
    /// `#[serde(skip)]` for the same reason as `diagnostics`.
    #[serde(skip)]
    #[ts(type = "any")]
    pub user_config: Arc<serde_json::Value>,
    /// Available grammars with provenance info, updated when grammar registry changes
    #[ts(type = "GrammarInfo[]")]
    pub available_grammars: Vec<GrammarInfoSnapshot>,
    /// Global editor mode for modal editing (e.g., "vi-normal", "vi-insert")
    /// When set, this mode's keybindings take precedence over normal key handling
    pub editor_mode: Option<String>,

    /// Plugin-managed per-buffer view state for the active split.
    /// Updated from BufferViewState.plugin_state during snapshot updates.
    /// Also written directly by JS plugins via setViewState for immediate read-back.
    #[ts(type = "any")]
    pub plugin_view_states: HashMap<BufferId, HashMap<String, serde_json::Value>>,

    /// Tracks which split was active when plugin_view_states was last populated.
    /// When the active split changes, plugin_view_states is fully repopulated.
    #[serde(skip)]
    #[ts(skip)]
    pub plugin_view_states_split: usize,

    /// Keybinding labels for plugin modes, keyed by "action\0mode" for fast lookup.
    /// Updated when modes are registered via defineMode().
    #[serde(skip)]
    #[ts(skip)]
    pub keybinding_labels: HashMap<String, String>,

    /// Plugin-managed global state, isolated per plugin.
    /// Outer key is plugin name, inner key is the state key set by the plugin.
    /// TODO: Need to think about plugin isolation / namespacing strategy for these APIs.
    /// Currently we isolate by plugin name, but we may want a more robust approach
    /// (e.g. preventing plugins from reading each other's state, or providing
    /// explicit cross-plugin state sharing APIs).
    #[ts(type = "any")]
    pub plugin_global_states: HashMap<String, HashMap<String, serde_json::Value>>,
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
            splits: Vec::new(),
            buffer_cursor_positions: HashMap::new(),
            buffer_text_properties: HashMap::new(),
            selected_text: None,
            clipboard: String::new(),
            working_dir: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
            authority_label: String::new(),
            diagnostics: Arc::new(HashMap::new()),
            folding_ranges: Arc::new(HashMap::new()),
            config: Arc::new(serde_json::Value::Null),
            user_config: Arc::new(serde_json::Value::Null),
            available_grammars: Vec::new(),
            editor_mode: None,
            plugin_view_states: HashMap::new(),
            plugin_view_states_split: 0,
            keybinding_labels: HashMap::new(),
            plugin_global_states: HashMap::new(),
        }
    }
}

impl Default for EditorStateSnapshot {
    fn default() -> Self {
        Self::new()
    }
}

/// Grammar info exposed to plugins, mirroring the editor's grammar provenance tracking.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct GrammarInfoSnapshot {
    /// The grammar name as used in config files (case-insensitive matching)
    pub name: String,
    /// Where this grammar was loaded from (e.g. "built-in", "plugin (myplugin)")
    pub source: String,
    /// File extensions associated with this grammar
    pub file_extensions: Vec<String>,
    /// Optional short name alias (e.g., "bash" for "Bourne Again Shell (bash)")
    pub short_name: Option<String>,
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
    ///
    /// Colors can be specified as RGB tuples or theme keys. When theme keys
    /// are provided, they take precedence and are resolved at render time.
    AddOverlay {
        buffer_id: BufferId,
        namespace: Option<OverlayNamespace>,
        range: Range<usize>,
        /// Overlay styling options (colors, modifiers, etc.)
        options: OverlayOptions,
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

    /// Override specific theme color keys in-memory for the running session.
    /// Keys are the same `section.field` strings accepted by
    /// `Theme::resolve_theme_key` (e.g. `"editor.bg"`, `"ui.status_bar_fg"`).
    /// Values are `[r, g, b]` triplets. Unknown keys are silently dropped so
    /// a typo in a fast animation loop doesn't blow up the caller; the
    /// return channel isn't used — plugins can do a dry-run look-up via
    /// `getThemeSchema` if they want compile-time safety. Overrides are
    /// reset the next time the caller (or anyone else) invokes
    /// `applyTheme`, because that replaces the whole `Theme` from the
    /// registry.
    OverrideThemeColors { overrides: HashMap<String, [u8; 3]> },

    /// Reload configuration from file
    /// After a plugin saves config changes, it should call this to reload the config
    ReloadConfig,

    /// Write a single setting to the runtime overlay for this session.
    /// `path` is dot-separated (e.g. "editor.tab_size"). Last write wins.
    SetSetting {
        plugin_name: String,
        path: String,
        #[ts(type = "unknown")]
        value: JsonValue,
    },

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

    /// Set the view mode for a buffer ("source" or "compose")
    SetViewMode { buffer_id: BufferId, mode: String },

    /// Enable/disable line wrapping for a buffer
    SetLineWrap {
        buffer_id: BufferId,
        split_id: Option<SplitId>,
        enabled: bool,
    },

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

    /// Set plugin-managed view state for a buffer in the active split.
    /// Stored in BufferViewState.plugin_state and persisted across sessions.
    SetViewState {
        buffer_id: BufferId,
        key: String,
        #[ts(type = "any")]
        value: Option<serde_json::Value>,
    },

    /// Set plugin-managed global state (not tied to any buffer or split).
    /// Isolated per plugin by plugin_name.
    /// TODO: Need to think about plugin isolation / namespacing strategy for these APIs.
    SetGlobalState {
        plugin_name: String,
        key: String,
        #[ts(type = "any")]
        value: Option<serde_json::Value>,
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

    /// Add virtual text with full styling — fg/bg can be RGB or theme
    /// keys (resolved at render time so theme changes apply live).
    /// This is the richer form of `AddVirtualText` that lets plugins
    /// produce themed labels (flash jump, type hints with semantic
    /// colours, …) without hard-coding RGB values.
    AddVirtualTextStyled {
        buffer_id: BufferId,
        virtual_text_id: String,
        position: usize,
        text: String,
        fg: Option<OverlayColorSpec>,
        bg: Option<OverlayColorSpec>,
        bold: bool,
        italic: bool,
        before: bool,
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
        /// Foreground color — RGB tuple or theme key string (e.g.
        /// `"editor.line_number_fg"`).  Resolved at render time so the line
        /// follows theme changes.
        fg_color: Option<OverlayColorSpec>,
        /// Background color — RGB tuple or theme key string.  None =
        /// transparent (inherits from underlying viewport background).
        bg_color: Option<OverlayColorSpec>,
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

    /// Add a conceal range that hides or replaces a byte range during rendering.
    /// Used for Typora-style seamless markdown: hiding syntax markers like `**`, `[](url)`, etc.
    AddConceal {
        buffer_id: BufferId,
        /// Namespace for bulk removal (shared with overlay namespace system)
        namespace: OverlayNamespace,
        /// Byte range to conceal
        start: usize,
        end: usize,
        /// Optional replacement text to show instead. None = hide completely.
        replacement: Option<String>,
    },

    /// Clear all conceal ranges in a namespace
    ClearConcealNamespace {
        buffer_id: BufferId,
        namespace: OverlayNamespace,
    },

    /// Remove all conceal ranges that overlap with a byte range
    /// Used for targeted invalidation when content in a range changes
    ClearConcealsInRange {
        buffer_id: BufferId,
        start: usize,
        end: usize,
    },

    /// Add a collapsed fold range. Hides the byte range
    /// `[start, end)` from rendering — the line containing `start - 1`
    /// (the fold's "header") stays visible while the lines covered by
    /// the range are skipped. Used by plugins that want to expose
    /// outline-style collapse without rebuilding buffer content.
    AddFold {
        buffer_id: BufferId,
        start: usize,
        end: usize,
        /// Optional placeholder text to show on the header line
        /// (currently unused by the renderer; reserved for future use).
        placeholder: Option<String>,
    },

    /// Clear every collapsed fold range on the buffer.
    ClearFolds { buffer_id: BufferId },

    /// Add a soft break point for marker-based line wrapping.
    /// The break is stored as a marker that auto-adjusts on buffer edits,
    /// eliminating the flicker caused by async view_transform round-trips.
    AddSoftBreak {
        buffer_id: BufferId,
        /// Namespace for bulk removal (shared with overlay namespace system)
        namespace: OverlayNamespace,
        /// Byte offset where the break should be injected
        position: usize,
        /// Number of hanging indent spaces after the break
        indent: u16,
    },

    /// Clear all soft breaks in a namespace
    ClearSoftBreakNamespace {
        buffer_id: BufferId,
        namespace: OverlayNamespace,
    },

    /// Remove all soft breaks that fall within a byte range
    ClearSoftBreaksInRange {
        buffer_id: BufferId,
        start: usize,
        end: usize,
    },

    /// Refresh lines for a buffer (clear seen_lines cache to re-trigger lines_changed hook)
    RefreshLines { buffer_id: BufferId },

    /// Refresh lines for ALL buffers (clear entire seen_lines cache)
    /// Sent when a plugin registers for the lines_changed hook to handle the race
    /// where render marks lines as "seen" before the plugin has registered.
    RefreshAllLines,

    /// Sentinel sent by the plugin thread after a hook has been fully processed.
    /// Used by the render loop to wait deterministically for plugin responses
    /// (e.g., conceal commands from `lines_changed`) instead of polling.
    HookCompleted { hook_name: String },

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

    /// Batch set line indicators in the gutter's indicator column
    /// Optimized for setting many lines with the same namespace/symbol/color/priority
    SetLineIndicators {
        buffer_id: BufferId,
        /// Line numbers (0-indexed)
        lines: Vec<usize>,
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

    /// Start an async prompt that returns result via callback
    /// The callback_id is used to resolve the promise when the prompt is confirmed or cancelled
    StartPromptAsync {
        label: String,
        initial_value: String,
        callback_id: JsCallbackId,
    },

    /// Request the next keypress for the calling plugin.
    ///
    /// The editor enqueues `callback_id` and resolves it with a
    /// `KeyEventPayload` JSON value the next time a key arrives in
    /// `Editor::handle_key`. Multiple pending requests are FIFO.
    /// While at least one request is pending, the next key is consumed
    /// by the resolution and does not propagate to mode bindings or
    /// other dispatch — this is the primitive that lets a plugin run a
    /// short input loop (flash labels, vi find-char, replace-char,
    /// etc.) without binding every printable key in `defineMode`.
    AwaitNextKey { callback_id: JsCallbackId },

    /// Begin or end "key capture" mode for the calling plugin.
    ///
    /// Without this, a plugin running a `getNextKey()` loop has a
    /// race: keys typed by the user (or pasted, or auto-repeated)
    /// can arrive between two consecutive `getNextKey()` calls while
    /// the plugin is still mid-redraw, and would otherwise fall
    /// through to the editor's normal dispatch (inserting into the
    /// buffer, etc.).
    ///
    /// While capture is active, every key arriving in
    /// `Editor::handle_key` (after terminal-input dispatch) is
    /// either resolved against a pending `AwaitNextKey` callback
    /// (existing behaviour) or, if no callback is pending, *buffered*
    /// in a FIFO queue.  When the next `AwaitNextKey` is processed,
    /// the queue is drained first.  This gives plugins lossless,
    /// in-order delivery of every key the user typed regardless of
    /// timing.
    ///
    /// `EndKeyCapture` clears any unconsumed buffered keys; they do
    /// NOT replay into the editor's normal dispatch path (that would
    /// be surprising — the user's intent was for the plugin to
    /// consume them).
    SetKeyCaptureActive { active: bool },

    /// Update the suggestions list for the current prompt
    /// Uses the editor's Suggestion type
    SetPromptSuggestions { suggestions: Vec<Suggestion> },

    /// When enabled, navigating suggestions updates the prompt input text
    SetPromptInputSync { sync: bool },

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
        /// Place the new buffer before (left/top of) the existing content (default: false/after)
        before: bool,
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

    /// Create a buffer group: multiple panels appearing as one tab.
    /// Each panel is a real buffer with its own scrollbar and viewport.
    CreateBufferGroup {
        /// Display name (shown in tab bar)
        name: String,
        /// Mode for keybindings
        mode: String,
        /// Layout tree as JSON string (parsed by the handler)
        layout_json: String,
        /// Optional request ID for async response
        request_id: Option<u64>,
    },

    /// Set the content of a panel within a buffer group.
    SetPanelContent {
        /// Group ID
        group_id: usize,
        /// Panel name (e.g., "tree", "picker")
        panel_name: String,
        /// Content entries
        entries: Vec<TextPropertyEntry>,
    },

    /// Close a buffer group (closes all panels and splits)
    CloseBufferGroup { group_id: usize },

    /// Focus a specific panel within a buffer group
    FocusPanel { group_id: usize, panel_name: String },

    /// Define a buffer mode with keybindings
    DefineMode {
        name: String,
        bindings: Vec<(String, String)>, // (key_string, command_name)
        read_only: bool,
        /// When true, unbound character keys dispatch as `mode_text_input:<char>`.
        allow_text_input: bool,
        /// When true, keys not bound by this mode fall through to the Normal
        /// context (motion, selection, copy) instead of being dropped.
        inherit_normal_bindings: bool,
        /// Name of the plugin that defined this mode (for attribution)
        plugin_name: Option<String>,
    },

    /// Switch the current split to display a buffer
    ShowBuffer { buffer_id: BufferId },

    /// Start a frame-buffer animation over a given screen region. The `id`
    /// is allocated on the plugin side so the JS call can return it
    /// synchronously; the editor uses it verbatim.
    StartAnimationArea {
        id: u64,
        rect: AnimationRect,
        kind: PluginAnimationKind,
    },

    /// Start an animation over the on-screen Rect currently occupied by a
    /// virtual buffer. If the buffer is not visible, the editor ignores
    /// the command.
    StartAnimationVirtualBuffer {
        id: u64,
        buffer_id: BufferId,
        kind: PluginAnimationKind,
    },

    /// Cancel an animation by the ID returned from `animateArea` /
    /// `animateVirtualBuffer`. No-op if the ID is unknown or already done.
    CancelAnimation { id: u64 },

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
        /// When set, first render scrolls to center this hunk (0-indexed)
        initial_focus_hunk: Option<usize>,
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

    /// Force-materialize render-dependent state (like `layoutIfNeeded` in UIKit).
    ///
    /// Creates `CompositeViewState` for any visible composite buffer that doesn't
    /// have one, and syncs viewport dimensions from split layout. This ensures
    /// subsequent commands can read/modify view state that is normally created
    /// lazily during the render cycle.
    FlushLayout,

    /// Navigate to the next hunk in a composite buffer
    CompositeNextHunk { buffer_id: BufferId },

    /// Navigate to the previous hunk in a composite buffer
    CompositePrevHunk { buffer_id: BufferId },

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

    /// Set a label on a leaf split (e.g., "sidebar")
    SetSplitLabel { split_id: SplitId, label: String },

    /// Remove a label from a split
    ClearSplitLabel { split_id: SplitId },

    /// Find a split by its label (async)
    GetSplitByLabel { label: String, request_id: u64 },

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

    /// Toggle whether the editor draws a native caret for this buffer.
    ///
    /// Buffer-group panel buffers default to `show_cursors = false`, which not
    /// only hides the caret but also blocks all movement actions in
    /// `action_to_events`. Plugins that want native cursor motion in a panel
    /// buffer (e.g. for magit-style row navigation) flip this to `true` after
    /// `createBufferGroup` returns.
    SetBufferShowCursors { buffer_id: BufferId, show: bool },

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

    /// Get byte offset of the start of a line (async)
    /// Line is 0-indexed (0 = first line)
    GetLineStartPosition {
        /// Buffer ID (0 for active buffer)
        buffer_id: BufferId,
        /// Line number (0-indexed)
        line: u32,
        /// Request ID for async response
        request_id: u64,
    },

    /// Get byte offset of the end of a line (async)
    /// Line is 0-indexed (0 = first line)
    /// Returns the byte offset after the last character of the line (before newline)
    GetLineEndPosition {
        /// Buffer ID (0 for active buffer)
        buffer_id: BufferId,
        /// Line number (0-indexed)
        line: u32,
        /// Request ID for async response
        request_id: u64,
    },

    /// Get the total number of lines in a buffer (async)
    GetBufferLineCount {
        /// Buffer ID (0 for active buffer)
        buffer_id: BufferId,
        /// Request ID for async response
        request_id: u64,
    },

    /// Scroll a split to center a specific line in the viewport
    /// Line is 0-indexed (0 = first line)
    ScrollToLineCenter {
        /// Split ID to scroll
        split_id: SplitId,
        /// Buffer ID containing the line
        buffer_id: BufferId,
        /// Line number to center (0-indexed)
        line: usize,
    },

    /// Scroll any split/panel that displays `buffer_id` so the given
    /// line is visible in the viewport. Unlike `ScrollToLineCenter` this
    /// does not require a split id — it walks all splits (including
    /// inner panels of a buffer group) and updates every viewport that
    /// shows this buffer. Line is 0-indexed.
    ScrollBufferToLine {
        /// Buffer ID to scroll
        buffer_id: BufferId,
        /// Line number to bring into view (0-indexed)
        line: usize,
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

    /// Restart LSP server for a specific language
    RestartLspForLanguage {
        /// The language to restart LSP for (e.g., "python", "rust")
        language: String,
    },

    /// Set the workspace root URI for a specific language's LSP server
    /// This allows plugins to specify project roots (e.g., directory containing .csproj)
    /// If the LSP is already running, it will be restarted with the new root
    SetLspRootUri {
        /// The language to set root URI for (e.g., "csharp", "rust")
        language: String,
        /// The root URI (file:// URL format)
        uri: String,
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

    /// Load a plugin from a file path
    /// The plugin will be initialized and start receiving events
    LoadPlugin {
        /// Path to the plugin file (.ts or .js)
        path: PathBuf,
        /// Callback ID for async response (success/failure)
        callback_id: JsCallbackId,
    },

    /// Unload a plugin by name
    /// The plugin will stop receiving events and be removed from memory
    UnloadPlugin {
        /// Plugin name (as registered)
        name: String,
        /// Callback ID for async response (success/failure)
        callback_id: JsCallbackId,
    },

    /// Reload a plugin by name (unload + load)
    /// Useful for development when plugin code changes
    ReloadPlugin {
        /// Plugin name (as registered)
        name: String,
        /// Callback ID for async response (success/failure)
        callback_id: JsCallbackId,
    },

    /// List all loaded plugins
    /// Returns plugin info (name, path, enabled) for all loaded plugins
    ListPlugins {
        /// Callback ID for async response (JSON array of plugin info)
        callback_id: JsCallbackId,
    },

    /// Reload the theme registry from disk
    /// Call this after installing a theme package or saving a new theme.
    /// If `apply_theme` is set, apply that theme immediately after reloading.
    ReloadThemes { apply_theme: Option<String> },

    /// Register a TextMate grammar file for a language
    /// The grammar will be added to pending_grammars until ReloadGrammars is called
    RegisterGrammar {
        /// Language identifier (e.g., "elixir", "zig")
        language: String,
        /// Path to the grammar file (.sublime-syntax or .tmLanguage)
        grammar_path: String,
        /// File extensions to associate with this grammar (e.g., ["ex", "exs"])
        extensions: Vec<String>,
    },

    /// Register language configuration (comment prefix, indentation, formatter)
    /// This is applied immediately to the runtime config
    RegisterLanguageConfig {
        /// Language identifier (e.g., "elixir")
        language: String,
        /// Language configuration
        config: LanguagePackConfig,
    },

    /// Register an LSP server for a language
    /// This is applied immediately to the LSP manager and runtime config
    RegisterLspServer {
        /// Language identifier (e.g., "elixir")
        language: String,
        /// LSP server configuration
        config: LspServerPackConfig,
    },

    /// Reload the grammar registry to apply registered grammars (async)
    /// Call this after registering one or more grammars to rebuild the syntax set.
    /// The callback is resolved when the background grammar build completes.
    ReloadGrammars { callback_id: JsCallbackId },

    // ==================== Terminal Commands ====================
    /// Create a new terminal in a split (async, returns TerminalResult)
    /// This spawns a PTY-backed terminal that plugins can write to and read from.
    CreateTerminal {
        /// Working directory for the terminal (defaults to editor cwd)
        cwd: Option<String>,
        /// Split direction ("horizontal" or "vertical"), default vertical
        direction: Option<String>,
        /// Split ratio (0.0 to 1.0), default 0.5
        ratio: Option<f32>,
        /// Whether to focus the new terminal split (default true)
        focus: Option<bool>,
        /// Whether this terminal survives editor restarts. When false, the
        /// terminal is excluded from workspace serialization and its backing
        /// file is kept unique-per-spawn so no scrollback from a prior run
        /// leaks in. Plugin-created terminals default to `false` since they
        /// are typically one-off tool UIs (rebuilds, exec shells, etc.).
        persistent: bool,
        /// Callback ID for async response
        request_id: u64,
    },

    /// Send input data to a terminal by its terminal ID
    SendTerminalInput {
        /// The terminal ID (from TerminalResult)
        terminal_id: TerminalId,
        /// Data to write to the terminal PTY (UTF-8 string, may include escape sequences)
        data: String,
    },

    /// Close a terminal by its terminal ID
    CloseTerminal {
        /// The terminal ID to close
        terminal_id: TerminalId,
    },

    /// Project-wide grep search (async)
    /// Searches all project files via FileSystem trait, respecting .gitignore.
    /// For open buffers with dirty edits, searches the buffer's piece tree.
    GrepProject {
        /// Search pattern (literal string)
        pattern: String,
        /// Whether the pattern is a fixed string (true) or regex (false)
        fixed_string: bool,
        /// Whether the search is case-sensitive
        case_sensitive: bool,
        /// Maximum number of results to return
        max_results: usize,
        /// Whether to match whole words only
        whole_words: bool,
        /// Callback ID for async response
        callback_id: JsCallbackId,
    },

    /// Project-wide streaming grep search (async, parallel)
    /// Like GrepProject but streams results incrementally via progress callback.
    /// Searches files in parallel using tokio tasks, sending per-file results
    /// back to the plugin as they complete.
    GrepProjectStreaming {
        /// Search pattern
        pattern: String,
        /// Whether the pattern is a fixed string (true) or regex (false)
        fixed_string: bool,
        /// Whether the search is case-sensitive
        case_sensitive: bool,
        /// Maximum number of results to return
        max_results: usize,
        /// Whether to match whole words only
        whole_words: bool,
        /// Search ID — used to route progress callbacks and for cancellation
        search_id: u64,
        /// Callback ID for the completion promise
        callback_id: JsCallbackId,
    },

    /// Replace matches in a buffer (async)
    /// Opens the file if not already open, applies edits through the buffer model,
    /// groups as a single undo action, and saves via FileSystem trait.
    ReplaceInBuffer {
        /// File path to edit (will open if not already in a buffer)
        file_path: PathBuf,
        /// Matches to replace, each is (byte_offset, length)
        matches: Vec<(usize, usize)>,
        /// Replacement text
        replacement: String,
        /// Callback ID for async response
        callback_id: JsCallbackId,
    },

    /// Install a new authority.
    ///
    /// Authority is opaque to core. The payload is a tagged JSON object
    /// (filesystem kind + spawner kind + terminal wrapper + display
    /// label) that `fresh-editor` deserializes into its concrete
    /// `AuthorityPayload` type. Using `serde_json::Value` here keeps
    /// fresh-core from growing backend-specific knowledge; see
    /// `crates/fresh-editor/src/services/authority/mod.rs` for the
    /// canonical schema.
    ///
    /// Fire-and-forget: the transition piggy-backs on the existing
    /// editor restart flow, so the plugin that sent this command will
    /// be re-loaded as part of the restart. Any follow-up work the
    /// plugin wants to do after the switch belongs in its post-restart
    /// init code, not in a callback here.
    SetAuthority {
        #[ts(type = "unknown")]
        payload: JsonValue,
    },

    /// Restore the default local authority. Same semantics as
    /// `SetAuthority` with a local payload — triggers an editor
    /// restart.
    ClearAuthority,

    /// Override the Remote Indicator's displayed state for the rest
    /// of the current editor session (until a restart, or until the
    /// plugin sends another override / `ClearRemoteIndicatorState`).
    ///
    /// The derived state — computed from the active authority's
    /// connection info — keeps running underneath and is what the
    /// indicator shows whenever an override is not in effect.
    /// Plugins use this to surface lifecycle states that have no
    /// authority-level truth yet (e.g. "Connecting" during
    /// `devcontainer up`, "FailedAttach" after a non-zero exit).
    ///
    /// `state` is a tagged enum keyed by `kind`:
    ///   - `{ "kind": "local" }`
    ///   - `{ "kind": "connecting", "label": "..." }`
    ///   - `{ "kind": "connected", "label": "..." }`
    ///   - `{ "kind": "failed_attach", "error": "..." }`
    ///   - `{ "kind": "disconnected", "label": "..." }`
    ///
    /// The exact schema lives in
    /// `crates/fresh-editor/src/view/ui/status_bar.rs`; fresh-core
    /// takes it opaquely so new variants can land without touching
    /// core plumbing.
    SetRemoteIndicatorState {
        #[ts(type = "unknown")]
        state: JsonValue,
    },

    /// Drop any active Remote Indicator override and fall back to
    /// the authority-derived state. Safe to call without a prior
    /// `SetRemoteIndicatorState`.
    ClearRemoteIndicatorState,

    /// Spawn a process on the host, regardless of the currently
    /// installed authority.
    ///
    /// Intended for plugin internals that must run host-side work
    /// (e.g. `devcontainer up`) before installing an authority that
    /// would otherwise route the spawn elsewhere. Behaves like
    /// `SpawnProcess` but always uses `LocalProcessSpawner`.
    ///
    /// The TS-side handle exposes `.kill()` on the returned
    /// `ProcessHandle`, serviced by `KillHostProcess` below — this
    /// lets callers abort a long-running host spawn (e.g.
    /// `devcontainer up`) via a user action like "Cancel Startup".
    SpawnHostProcess {
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        callback_id: JsCallbackId,
    },

    /// Cancel a host-side process previously started via
    /// `SpawnHostProcess`. `process_id` is the callback id returned
    /// by `spawnHostProcess` (the TS handle stores it and forwards
    /// when the caller invokes `.kill()`).
    ///
    /// No-op when the id is unknown — the process may have already
    /// exited, or the caller may hold a stale handle. SIGKILL on
    /// Unix per `tokio::process::Child::start_kill`; children of the
    /// killed process may leak (see Q-C2 in
    /// `DEVCONTAINER_SPEC_GAP_PLAN.md`).
    KillHostProcess { process_id: u64 },
}

impl PluginCommand {
    /// Extract the enum variant name from the Debug representation.
    pub fn debug_variant_name(&self) -> String {
        let dbg = format!("{:?}", self);
        dbg.split([' ', '{', '(']).next().unwrap_or("?").to_string()
    }
}

// =============================================================================
// Language Pack Configuration Types
// =============================================================================

/// Language configuration for language packs
///
/// This is a simplified version of the full LanguageConfig, containing only
/// the fields that can be set via the plugin API.
#[derive(Debug, Clone, Serialize, Deserialize, Default, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LanguagePackConfig {
    /// Comment prefix for line comments (e.g., "//" or "#")
    #[serde(default)]
    pub comment_prefix: Option<String>,

    /// Block comment start marker (e.g., slash-star)
    #[serde(default)]
    pub block_comment_start: Option<String>,

    /// Block comment end marker (e.g., star-slash)
    #[serde(default)]
    pub block_comment_end: Option<String>,

    /// Whether to use tabs instead of spaces for indentation
    #[serde(default)]
    pub use_tabs: Option<bool>,

    /// Tab size (number of spaces per tab level)
    #[serde(default)]
    pub tab_size: Option<usize>,

    /// Whether auto-indent is enabled
    #[serde(default)]
    pub auto_indent: Option<bool>,

    /// Whether to show whitespace tab indicators (→) for this language
    /// Defaults to true. Set to false for languages like Go/Hare that use tabs for indentation.
    #[serde(default)]
    pub show_whitespace_tabs: Option<bool>,

    /// Formatter configuration
    #[serde(default)]
    pub formatter: Option<FormatterPackConfig>,
}

/// Formatter configuration for language packs
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct FormatterPackConfig {
    /// Command to run (e.g., "prettier", "rustfmt")
    pub command: String,

    /// Arguments to pass to the formatter
    #[serde(default)]
    pub args: Vec<String>,
}

/// Process resource limits for LSP servers
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ProcessLimitsPackConfig {
    /// Maximum memory usage as percentage of total system memory (null = no limit)
    #[serde(default)]
    pub max_memory_percent: Option<u32>,

    /// Maximum CPU usage as percentage of total CPU (null = no limit)
    #[serde(default)]
    pub max_cpu_percent: Option<u32>,

    /// Enable resource limiting
    #[serde(default)]
    pub enabled: Option<bool>,
}

/// LSP server configuration for language packs
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LspServerPackConfig {
    /// Command to start the LSP server
    pub command: String,

    /// Arguments to pass to the command
    #[serde(default)]
    pub args: Vec<String>,

    /// Whether to auto-start the server when a matching file is opened
    #[serde(default)]
    pub auto_start: Option<bool>,

    /// LSP initialization options
    #[serde(default)]
    #[ts(type = "Record<string, unknown> | null")]
    pub initialization_options: Option<JsonValue>,

    /// Process resource limits (memory and CPU)
    #[serde(default)]
    pub process_limits: Option<ProcessLimitsPackConfig>,
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
#[serde(deny_unknown_fields)]
#[ts(export, rename = "TsActionPopupAction")]
pub struct ActionPopupAction {
    /// Unique action identifier (returned in ActionPopupResult)
    pub id: String,
    /// Display text for the button (can include command hints)
    pub label: String,
}

/// Options for showActionPopup
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export)]
pub struct ActionPopupOptions {
    /// Unique identifier for the popup (used in ActionPopupResult)
    pub id: String,
    /// Title text for the popup
    pub title: String,
    /// Body message (supports basic formatting)
    pub message: String,
    /// Action buttons to display
    pub actions: Vec<ActionPopupAction>,
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

/// A single match from project-wide grep
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct GrepMatch {
    /// Absolute file path
    pub file: String,
    /// Buffer ID if the file is open (0 if not)
    #[ts(type = "number")]
    pub buffer_id: usize,
    /// Byte offset of match start in the file/buffer content
    #[ts(type = "number")]
    pub byte_offset: usize,
    /// Match length in bytes
    #[ts(type = "number")]
    pub length: usize,
    /// 1-indexed line number
    #[ts(type = "number")]
    pub line: usize,
    /// 1-indexed column number
    #[ts(type = "number")]
    pub column: usize,
    /// The matched line content (for display)
    pub context: String,
}

/// Result from replacing matches in a buffer
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct ReplaceResult {
    /// Number of replacements made
    #[ts(type = "number")]
    pub replacements: usize,
    /// Buffer ID of the edited buffer
    #[ts(type = "number")]
    pub buffer_id: usize,
}

/// Entry for virtual buffer content with optional text properties (JS API version)
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
#[ts(export, rename = "TextPropertyEntry", rename_all = "camelCase")]
pub struct JsTextPropertyEntry {
    /// Text content for this entry
    pub text: String,
    /// Optional properties attached to this text (e.g., file path, line number)
    #[serde(default)]
    #[ts(optional, type = "Record<string, unknown>")]
    pub properties: Option<HashMap<String, JsonValue>>,
    /// Optional whole-entry styling
    #[serde(default)]
    #[ts(optional, type = "Partial<OverlayOptions>")]
    pub style: Option<OverlayOptions>,
    /// Optional sub-range styling within this entry
    #[serde(default)]
    #[ts(optional)]
    pub inline_overlays: Option<Vec<crate::text_property::InlineOverlay>>,
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

/// Position in a document (line and character)
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct JsPosition {
    /// Zero-indexed line number
    pub line: u32,
    /// Zero-indexed character offset
    pub character: u32,
}

/// Range in a document (start and end positions)
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct JsRange {
    /// Start position
    pub start: JsPosition,
    /// End position
    pub end: JsPosition,
}

/// Diagnostic from LSP
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct JsDiagnostic {
    /// Document URI
    pub uri: String,
    /// Diagnostic message
    pub message: String,
    /// Severity: 1=Error, 2=Warning, 3=Info, 4=Hint, null=unknown
    pub severity: Option<u8>,
    /// Range in the document
    pub range: JsRange,
    /// Source of the diagnostic (e.g., "typescript", "eslint")
    #[ts(optional)]
    pub source: Option<String>,
}

/// Options for createVirtualBuffer
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
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
#[serde(deny_unknown_fields)]
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
    /// Place the new buffer before (left/top of) the existing content (default: false)
    #[serde(default)]
    #[ts(optional)]
    pub before: Option<bool>,
    /// Initial content entries with optional properties
    #[serde(default)]
    #[ts(optional)]
    pub entries: Option<Vec<JsTextPropertyEntry>>,
}

/// Options for createVirtualBufferInExistingSplit
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
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

/// Options for createTerminal
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export)]
pub struct CreateTerminalOptions {
    /// Working directory for the terminal (defaults to editor cwd)
    #[serde(default)]
    #[ts(optional)]
    pub cwd: Option<String>,
    /// Split direction: "horizontal" or "vertical" (default: "vertical")
    #[serde(default)]
    #[ts(optional)]
    pub direction: Option<String>,
    /// Split ratio 0.0-1.0 (default: 0.5)
    #[serde(default)]
    #[ts(optional)]
    pub ratio: Option<f32>,
    /// Whether to focus the new terminal split (default: true)
    #[serde(default)]
    #[ts(optional)]
    pub focus: Option<bool>,
    /// Whether this terminal is part of the user's persisted workspace.
    /// Defaults to `false` for plugin-created terminals — they are typically
    /// one-off tool UIs (rebuilds, exec shells, build output) and should
    /// start with empty scrollback on each invocation. Set to `true` only
    /// when the plugin owns a terminal that the user should see restored
    /// across editor restarts.
    #[serde(default)]
    #[ts(optional)]
    pub persistent: Option<bool>,
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

    // All types that deserialize from a JS value via rquickjs_serde follow
    // the same 8-line pattern differing only in the type name. This macro
    // expands that pattern once so adding a new plugin-API type costs one line
    // here instead of a copy-pasted block.
    macro_rules! impl_from_js_via_serde {
        ($($T:ty),+ $(,)?) => {
            $(
                impl<'js> FromJs<'js> for $T {
                    fn from_js(_ctx: &Ctx<'js>, value: Value<'js>) -> rquickjs::Result<Self> {
                        rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
                            from: "object",
                            to: stringify!($T),
                            message: Some(e.to_string()),
                        })
                    }
                }
            )+
        };
    }

    impl_from_js_via_serde!(
        JsTextPropertyEntry,
        CreateVirtualBufferOptions,
        CreateVirtualBufferInSplitOptions,
        CreateVirtualBufferInExistingSplitOptions,
        ActionSpec,
        ActionPopupAction,
        ActionPopupOptions,
        ViewTokenWire,
        ViewTokenStyle,
        LayoutHints,
        CompositeHunk,
        LanguagePackConfig,
        LspServerPackConfig,
        ProcessLimitsPackConfig,
        CreateTerminalOptions,
    );

    impl<'js> rquickjs::IntoJs<'js> for TextPropertiesAtCursor {
        fn into_js(self, ctx: &Ctx<'js>) -> rquickjs::Result<Value<'js>> {
            rquickjs_serde::to_value(ctx.clone(), &self.0)
                .map_err(|e| rquickjs::Error::new_from_js_message("serialize", "", &e.to_string()))
        }
    }

    impl<'js> FromJs<'js> for CreateCompositeBufferOptions {
        fn from_js(_ctx: &Ctx<'js>, value: Value<'js>) -> rquickjs::Result<Self> {
            // Two-step deserialization: rquickjs_serde cannot handle the nested
            // enums in this struct directly, so go via serde_json as an intermediary.
            let json: serde_json::Value =
                rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
                    from: "object",
                    to: "CreateCompositeBufferOptions (json)",
                    message: Some(e.to_string()),
                })?;
            serde_json::from_value(json).map_err(|e| rquickjs::Error::FromJs {
                from: "json",
                to: "CreateCompositeBufferOptions",
                message: Some(e.to_string()),
            })
        }
    }

    // ── Tests for FromJs / IntoJs impls ────────────────────────────────────
    //
    // Each impl is a one-liner that delegates to `rquickjs_serde`. A mutant
    // that replaces the body with `Ok(Default::default())` drops the
    // decoded payload on the floor. Every test below asserts a
    // non-defaultable field value, so the mutant cannot pass.
    //
    // Note: many of the target structs do not implement `Default`, making
    // those mutants unviable (they fail to compile) — cargo-mutants still
    // lists them as candidates. The tests below serve double-duty as
    // behavioural regression protection for the JS → Rust conversion layer.
    #[cfg(test)]
    mod tests {
        use super::*;
        use rquickjs::{Context, Runtime};

        /// Run a closure within a fresh QuickJS context so that `FromJs`
        /// impls can be exercised end-to-end.
        fn with_js<R>(f: impl for<'js> FnOnce(Ctx<'js>) -> R) -> R {
            let rt = Runtime::new().expect("create rquickjs runtime");
            let ctx = Context::full(&rt).expect("create rquickjs context");
            ctx.with(f)
        }

        /// Evaluate a JS object literal and decode it as `T` via `FromJs`.
        fn eval_as<T>(src: &str) -> T
        where
            for<'js> T: rquickjs::FromJs<'js>,
        {
            with_js(|ctx| {
                let value: Value = ctx
                    .eval::<Value, _>(src.as_bytes())
                    .expect("eval JS source");
                T::from_js(&ctx, value).expect("from_js decode")
            })
        }

        #[test]
        fn js_text_property_entry_decodes_text_and_properties() {
            let got: JsTextPropertyEntry =
                eval_as("({text: 'hello', properties: {file: '/x.rs'}})");
            assert_eq!(got.text, "hello");
            let props = got.properties.expect("properties present");
            assert_eq!(props.get("file").and_then(|v| v.as_str()), Some("/x.rs"));
        }

        #[test]
        fn create_virtual_buffer_options_decodes_name() {
            let got: CreateVirtualBufferOptions = eval_as("({name: 'logs', readOnly: true})");
            assert_eq!(got.name, "logs");
            assert_eq!(got.read_only, Some(true));
        }

        #[test]
        fn create_virtual_buffer_in_split_options_decodes_ratio() {
            let got: CreateVirtualBufferInSplitOptions =
                eval_as("({name: 'diag', ratio: 0.25, direction: 'horizontal'})");
            assert_eq!(got.name, "diag");
            assert!(matches!(got.ratio, Some(r) if (r - 0.25).abs() < 1e-6));
            assert_eq!(got.direction.as_deref(), Some("horizontal"));
        }

        #[test]
        fn create_virtual_buffer_in_existing_split_options_decodes_splitid() {
            let got: CreateVirtualBufferInExistingSplitOptions =
                eval_as("({name: 'n', splitId: 7})");
            assert_eq!(got.name, "n");
            assert_eq!(got.split_id, 7);
        }

        #[test]
        fn create_terminal_options_decodes_cwd_and_focus() {
            let got: CreateTerminalOptions =
                eval_as("({cwd: '/tmp', direction: 'vertical', focus: false})");
            assert_eq!(got.cwd.as_deref(), Some("/tmp"));
            assert_eq!(got.direction.as_deref(), Some("vertical"));
            assert_eq!(got.focus, Some(false));
        }

        #[test]
        fn action_spec_decodes_action_and_count() {
            let got: ActionSpec = eval_as("({action: 'move_word_right', count: 5})");
            assert_eq!(got.action, "move_word_right");
            assert_eq!(got.count, 5);
        }

        #[test]
        fn action_popup_action_decodes_id_and_label() {
            let got: ActionPopupAction = eval_as("({id: 'ok', label: 'OK'})");
            assert_eq!(got.id, "ok");
            assert_eq!(got.label, "OK");
        }

        #[test]
        fn action_popup_options_decodes_actions_list() {
            let got: ActionPopupOptions = eval_as(
                "({id: 'p', title: 't', message: 'm', \
                   actions: [{id: 'ok', label: 'OK'}]})",
            );
            assert_eq!(got.id, "p");
            assert_eq!(got.title, "t");
            assert_eq!(got.message, "m");
            assert_eq!(got.actions.len(), 1);
            assert_eq!(got.actions[0].id, "ok");
        }

        #[test]
        fn view_token_wire_decodes_offset_and_kind() {
            // Using `Newline` (a unit variant) avoids the tuple-variant
            // wire-format ambiguity in rquickjs_serde while still exercising
            // the `FromJs` impl end-to-end.
            let got: ViewTokenWire = eval_as("({source_offset: 42, kind: 'Newline'})");
            assert_eq!(got.source_offset, Some(42));
            assert!(matches!(got.kind, ViewTokenWireKind::Newline));
        }

        #[test]
        fn view_token_style_decodes_boolean_flags() {
            // `fg`/`bg` are `Option<(u8, u8, u8)>` which rquickjs_serde does
            // not decode from plain JS arrays, so we pin down the boolean
            // flags — enough to prove the body actually ran.
            let got: ViewTokenStyle = eval_as("({bold: true, italic: true})");
            assert!(got.bold);
            assert!(got.italic);
            assert!(got.fg.is_none());
        }

        #[test]
        fn layout_hints_decodes_compose_width() {
            let got: LayoutHints = eval_as("({composeWidth: 120})");
            assert_eq!(got.compose_width, Some(120));
            assert!(got.column_guides.is_none());
        }

        #[test]
        fn create_composite_buffer_options_decodes_name_and_sources() {
            let got: CreateCompositeBufferOptions = eval_as(
                "({name: 'diff', mode: 'm', \
                   layout: {type: 'side-by-side', ratios: [0.5, 0.5], showSeparator: true}, \
                   sources: [{bufferId: 3, label: 'OLD'}]})",
            );
            assert_eq!(got.name, "diff");
            assert_eq!(got.layout.layout_type, "side-by-side");
            assert_eq!(got.sources.len(), 1);
            assert_eq!(got.sources[0].buffer_id, 3);
            assert_eq!(got.sources[0].label, "OLD");
        }

        #[test]
        fn composite_hunk_decodes_all_fields() {
            let got: CompositeHunk =
                eval_as("({oldStart: 1, oldCount: 2, newStart: 3, newCount: 4})");
            assert_eq!(got.old_start, 1);
            assert_eq!(got.old_count, 2);
            assert_eq!(got.new_start, 3);
            assert_eq!(got.new_count, 4);
        }

        #[test]
        fn language_pack_config_decodes_comment_prefix_and_tab_size() {
            let got: LanguagePackConfig =
                eval_as("({commentPrefix: '//', tabSize: 7, useTabs: true})");
            assert_eq!(got.comment_prefix.as_deref(), Some("//"));
            assert_eq!(got.tab_size, Some(7));
            assert_eq!(got.use_tabs, Some(true));
        }

        #[test]
        fn lsp_server_pack_config_decodes_command_and_args() {
            let got: LspServerPackConfig =
                eval_as("({command: 'rust-analyzer', args: ['--log'], autoStart: true})");
            assert_eq!(got.command, "rust-analyzer");
            assert_eq!(got.args, vec!["--log".to_string()]);
            assert_eq!(got.auto_start, Some(true));
        }

        #[test]
        fn process_limits_pack_config_decodes_percentages() {
            let got: ProcessLimitsPackConfig =
                eval_as("({maxMemoryPercent: 75, maxCpuPercent: 50, enabled: true})");
            assert_eq!(got.max_memory_percent, Some(75));
            assert_eq!(got.max_cpu_percent, Some(50));
            assert_eq!(got.enabled, Some(true));
        }

        /// `TextPropertiesAtCursor::into_js` must serialise the inner vector
        /// into a JS array whose length matches the payload. A mutant that
        /// returns a default (`undefined` / empty) value would fail either
        /// the array check or the length check.
        #[test]
        fn text_properties_at_cursor_into_js_preserves_length() {
            use rquickjs::IntoJs;
            with_js(|ctx| {
                let mut entry = std::collections::HashMap::new();
                entry.insert("k".to_string(), serde_json::json!("v"));
                let payload = TextPropertiesAtCursor(vec![entry.clone(), entry]);

                let v = payload.into_js(&ctx).expect("into_js");
                let arr = v.as_array().expect("expected JS array");
                assert_eq!(arr.len(), 2);
            });
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
    /// Add an overlay to a buffer with styling options
    ///
    /// Returns an opaque handle that can be used to remove the overlay later.
    ///
    /// Colors can be specified as RGB arrays or theme key strings.
    /// Theme keys are resolved at render time, so overlays update with theme changes.
    pub fn add_overlay(
        &self,
        buffer_id: BufferId,
        namespace: Option<String>,
        range: Range<usize>,
        options: OverlayOptions,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::AddOverlay {
            buffer_id,
            namespace: namespace.map(OverlayNamespace::from_string),
            range,
            options,
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

    /// Enable/disable syncing prompt input text when navigating suggestions
    pub fn set_prompt_input_sync(&self, sync: bool) -> Result<(), String> {
        self.send_command(PluginCommand::SetPromptInputSync { sync })
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
    /// Bindings are specified as (key_string, command_name) pairs.
    pub fn define_mode(
        &self,
        name: String,
        bindings: Vec<(String, String)>,
        read_only: bool,
        allow_text_input: bool,
    ) -> Result<(), String> {
        self.send_command(PluginCommand::DefineMode {
            name,
            bindings,
            read_only,
            allow_text_input,
            inherit_normal_bindings: false,
            plugin_name: None,
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

// ============================================================================
// Pluggable Completion Service — TypeScript Plugin API Types
// ============================================================================
//
// These types are the bridge between the Rust `CompletionService` and
// TypeScript plugins that want to provide completion candidates.  They are
// serialised to/from JSON via serde and generate TypeScript definitions via
// ts-rs so that the plugin API stays in sync automatically.

/// A completion candidate produced by a TypeScript plugin provider.
///
/// This mirrors `CompletionCandidate` in the Rust `completion::provider`
/// module but uses serde-friendly primitives for the JS ↔ Rust boundary.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export, rename_all = "camelCase")]
pub struct TsCompletionCandidate {
    /// Display text shown in the completion popup.
    pub label: String,

    /// Text to insert when accepted. Falls back to `label` if omitted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub insert_text: Option<String>,

    /// Short detail string shown next to the label.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detail: Option<String>,

    /// Single-character icon hint (e.g. `"λ"`, `"v"`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon: Option<String>,

    /// Provider-assigned relevance score (higher = better).
    #[serde(default)]
    pub score: i64,

    /// Whether `insert_text` uses LSP snippet syntax (`$0`, `${1:ph}`, …).
    #[serde(default)]
    pub is_snippet: bool,

    /// Opaque data carried through to the `completionAccepted` hook.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider_data: Option<String>,
}

/// Context sent to a TypeScript plugin's `provideCompletions` handler.
///
/// Plugins receive this as a read-only snapshot so they never need direct
/// buffer access (which would be unsafe for huge files).
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct TsCompletionContext {
    /// The word prefix typed so far.
    pub prefix: String,

    /// Byte offset of the cursor.
    pub cursor_byte: usize,

    /// Byte offset of the word start (for replacement range).
    pub word_start_byte: usize,

    /// Total buffer size in bytes.
    pub buffer_len: usize,

    /// Whether the buffer is a lazily-loaded huge file.
    pub is_large_file: bool,

    /// A text excerpt around the cursor (the contents of the safe scan window).
    /// Plugins should search only this string, not request the full buffer.
    pub text_around_cursor: String,

    /// Byte offset within `text_around_cursor` that corresponds to the cursor.
    pub cursor_offset_in_text: usize,

    /// File language id (e.g. `"rust"`, `"typescript"`), if known.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub language_id: Option<String>,
}

/// Registration payload sent by a plugin to register a completion provider.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export, rename_all = "camelCase")]
pub struct TsCompletionProviderRegistration {
    /// Unique id for this provider (e.g., `"my-snippets"`).
    pub id: String,

    /// Human-readable name shown in status/debug UI.
    pub display_name: String,

    /// Priority tier (lower = higher priority). Convention:
    /// 0 = LSP, 10 = ctags, 20 = buffer words, 30 = dabbrev, 50 = plugin.
    #[serde(default = "default_plugin_provider_priority")]
    pub priority: u32,

    /// Optional list of language ids this provider is active for.
    /// If empty/omitted, the provider is active for all languages.
    #[serde(default)]
    pub language_ids: Vec<String>,
}

fn default_plugin_provider_priority() -> u32 {
    50
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
            OverlayOptions {
                fg: Some(OverlayColorSpec::ThemeKey("ui.status_bar_fg".to_string())),
                bg: None,
                underline: true,
                bold: false,
                italic: false,
                strikethrough: false,
                extend_to_line_end: false,
                url: None,
            },
        );
        assert!(result.is_ok());

        let received = rx.try_recv().unwrap();
        match received {
            PluginCommand::AddOverlay {
                buffer_id,
                namespace,
                range,
                options,
            } => {
                assert_eq!(buffer_id.0, 1);
                assert_eq!(namespace.as_ref().map(|n| n.as_str()), Some("test-overlay"));
                assert_eq!(range, 0..10);
                assert!(matches!(
                    options.fg,
                    Some(OverlayColorSpec::ThemeKey(ref k)) if k == "ui.status_bar_fg"
                ));
                assert!(options.bg.is_none());
                assert!(options.underline);
                assert!(!options.bold);
                assert!(!options.italic);
                assert!(!options.extend_to_line_end);
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
                is_virtual: false,
                view_mode: "source".to_string(),
                is_composing_in_any_split: false,
                compose_width: None,
                language: "text".to_string(),
                is_preview: false,
                splits: Vec::new(),
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
                    is_virtual: false,
                    view_mode: "source".to_string(),
                    is_composing_in_any_split: false,
                    compose_width: None,
                    language: "text".to_string(),
                    is_preview: false,
                    splits: Vec::new(),
                },
            );
            snapshot.buffers.insert(
                BufferId(2),
                BufferInfo {
                    id: BufferId(2),
                    path: Some(std::path::PathBuf::from("/file2.txt")),
                    modified: true,
                    length: 100,
                    is_virtual: false,
                    view_mode: "source".to_string(),
                    is_composing_in_any_split: false,
                    compose_width: None,
                    language: "text".to_string(),
                    is_preview: false,
                    splits: Vec::new(),
                },
            );
            snapshot.buffers.insert(
                BufferId(3),
                BufferInfo {
                    id: BufferId(3),
                    path: None,
                    modified: false,
                    length: 0,
                    is_virtual: true,
                    view_mode: "source".to_string(),
                    is_composing_in_any_split: false,
                    compose_width: None,
                    language: "text".to_string(),
                    is_preview: false,
                    splits: Vec::new(),
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
                top_line: Some(5),
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

    #[test]
    fn test_plugin_response_line_end_position() {
        let response = PluginResponse::LineEndPosition {
            request_id: 42,
            position: Some(100),
        };
        let json = serde_json::to_string(&response).unwrap();
        assert!(json.contains("LineEndPosition"));
        assert!(json.contains("42"));
        assert!(json.contains("100"));

        // Test None case
        let response_none = PluginResponse::LineEndPosition {
            request_id: 1,
            position: None,
        };
        let json_none = serde_json::to_string(&response_none).unwrap();
        assert!(json_none.contains("null"));
    }

    #[test]
    fn test_plugin_response_buffer_line_count() {
        let response = PluginResponse::BufferLineCount {
            request_id: 99,
            count: Some(500),
        };
        let json = serde_json::to_string(&response).unwrap();
        assert!(json.contains("BufferLineCount"));
        assert!(json.contains("99"));
        assert!(json.contains("500"));
    }

    #[test]
    fn test_plugin_command_get_line_end_position() {
        let command = PluginCommand::GetLineEndPosition {
            buffer_id: BufferId(1),
            line: 10,
            request_id: 123,
        };
        let json = serde_json::to_string(&command).unwrap();
        assert!(json.contains("GetLineEndPosition"));
        assert!(json.contains("10"));
    }

    #[test]
    fn test_plugin_command_get_buffer_line_count() {
        let command = PluginCommand::GetBufferLineCount {
            buffer_id: BufferId(0),
            request_id: 456,
        };
        let json = serde_json::to_string(&command).unwrap();
        assert!(json.contains("GetBufferLineCount"));
        assert!(json.contains("456"));
    }

    #[test]
    fn test_plugin_command_scroll_to_line_center() {
        let command = PluginCommand::ScrollToLineCenter {
            split_id: SplitId(1),
            buffer_id: BufferId(2),
            line: 50,
        };
        let json = serde_json::to_string(&command).unwrap();
        assert!(json.contains("ScrollToLineCenter"));
        assert!(json.contains("50"));
    }

    /// `JsCallbackId` round-trips through `u64` via `new` / `as_u64` / `From`
    /// and renders as its underlying integer via `Display`.
    #[test]
    fn js_callback_id_conversions_and_display() {
        for raw in [0u64, 1, 42, u64::MAX] {
            let id = JsCallbackId::new(raw);
            assert_eq!(id.as_u64(), raw);
            assert_eq!(u64::from(id), raw);
            assert_eq!(JsCallbackId::from(raw), id);
            assert_eq!(id.to_string(), raw.to_string());
        }
    }

    /// Serde `default = ...` helpers fire when the field is omitted and are
    /// overridden by explicit values. One test per struct pins each helper
    /// to its documented default.
    #[test]
    fn serde_defaults_fire_when_fields_are_omitted() {
        // default_action_count → 1
        let spec: ActionSpec = serde_json::from_str(r#"{"action": "move_left"}"#).unwrap();
        assert_eq!(spec.count, 1);
        let spec: ActionSpec =
            serde_json::from_str(r#"{"action": "move_left", "count": 5}"#).unwrap();
        assert_eq!(spec.count, 5);

        // default_true → showSeparator = true
        let layout: CompositeLayoutConfig =
            serde_json::from_str(r#"{"type": "side-by-side"}"#).unwrap();
        assert!(layout.show_separator);
        let layout: CompositeLayoutConfig =
            serde_json::from_str(r#"{"type": "side-by-side", "showSeparator": false}"#).unwrap();
        assert!(!layout.show_separator);

        // default_plugin_provider_priority → 50
        let reg: TsCompletionProviderRegistration =
            serde_json::from_str(r#"{"id": "p", "displayName": "P"}"#).unwrap();
        assert_eq!(reg.priority, 50);
        let reg: TsCompletionProviderRegistration =
            serde_json::from_str(r#"{"id": "p", "displayName": "P", "priority": 3}"#).unwrap();
        assert_eq!(reg.priority, 3);
    }

    // ── Behavioural tests added to kill the mutants reported by cargo-mutants ──
    //
    // These tests pin down observable behaviour for tiny methods whose bodies
    // were replaceable with a constant (e.g. `()`, `Ok(())`, `None`, or a
    // default value) without any existing test noticing.

    /// Helper: build a minimal `Command` with a given name.
    fn mk_cmd(name: &str) -> Command {
        Command {
            name: name.to_string(),
            description: String::new(),
            action_name: String::new(),
            plugin_name: String::new(),
            custom_contexts: Vec::new(),
        }
    }

    /// `CommandRegistry::register` appends new commands and replaces any
    /// existing entry with the same name; `unregister` removes exactly the
    /// matching entry and is a no-op for unknown names.
    ///
    /// Kills: replace register with `()`; `!= → ==` in register;
    ///        replace unregister with `()`; `!= → ==` in unregister.
    #[test]
    fn command_registry_register_and_unregister_semantics() {
        let r = CommandRegistry::new();

        r.register(mk_cmd("a"));
        r.register(mk_cmd("b"));
        assert_eq!(r.commands.read().unwrap().len(), 2);

        // Re-registering "a" must keep "b" (retain filters by `!=`); the
        // `== → !=` mutant would drop "b" and leave two copies of "a".
        r.register(mk_cmd("a"));
        let names: Vec<String> = r
            .commands
            .read()
            .unwrap()
            .iter()
            .map(|c| c.name.clone())
            .collect();
        assert_eq!(names, vec!["b".to_string(), "a".to_string()]);

        // Unregister must remove exactly "a" and preserve "b"; the `== → !=`
        // mutant would keep "a" and drop "b".
        r.unregister("a");
        let names: Vec<String> = r
            .commands
            .read()
            .unwrap()
            .iter()
            .map(|c| c.name.clone())
            .collect();
        assert_eq!(names, vec!["b".to_string()]);

        // Unregistering an unknown name is a no-op.
        r.unregister("nope");
        assert_eq!(r.commands.read().unwrap().len(), 1);
    }

    /// `OverlayColorSpec::as_rgb` returns the exact stored tuple for the RGB
    /// variant and `None` for the theme-key variant; `as_theme_key` is the
    /// dual. Uses a triple with no zero or one components and a theme key
    /// that is neither empty nor `"xyzzy"` to kill every constant-return
    /// mutant reported by cargo-mutants at once.
    #[test]
    fn overlay_color_spec_accessors_are_variant_specific() {
        let rgb = OverlayColorSpec::rgb(12, 34, 56);
        assert_eq!(rgb.as_rgb(), Some((12, 34, 56)));
        assert_eq!(rgb.as_theme_key(), None);

        let tk = OverlayColorSpec::theme_key("ui.status_bar_bg");
        assert_eq!(tk.as_rgb(), None);
        assert_eq!(tk.as_theme_key(), Some("ui.status_bar_bg"));
    }

    /// `PluginCommand::debug_variant_name` returns the actual variant name
    /// derived from the `Debug` impl, not an empty or hard-coded string.
    #[test]
    fn plugin_command_debug_variant_name_returns_real_variant() {
        let c = PluginCommand::SetStatus {
            message: "hi".into(),
        };
        assert_eq!(c.debug_variant_name(), "SetStatus");

        let c2 = PluginCommand::InsertText {
            buffer_id: BufferId(1),
            position: 0,
            text: String::new(),
        };
        assert_eq!(c2.debug_variant_name(), "InsertText");
    }

    // ── PluginApi dispatch / mutation tests ────────────────────────────────
    //
    // Each `PluginApi` method is a one-liner that either pushes a
    // `PluginCommand` onto the channel or mutates a shared registry. The
    // mutants replace the body with `Ok(())` / `()`, i.e. the side effect
    // disappears. One assertion per method ties the side effect down.

    fn mk_api() -> (
        PluginApi,
        std::sync::mpsc::Receiver<PluginCommand>,
        Arc<RwLock<HookRegistry>>,
        Arc<RwLock<CommandRegistry>>,
        Arc<RwLock<EditorStateSnapshot>>,
    ) {
        let hooks = Arc::new(RwLock::new(HookRegistry::new()));
        let commands = Arc::new(RwLock::new(CommandRegistry::new()));
        let (tx, rx) = std::sync::mpsc::channel();
        let snap = Arc::new(RwLock::new(EditorStateSnapshot::new()));
        let api = PluginApi::new(hooks.clone(), commands.clone(), tx, snap.clone());
        (api, rx, hooks, commands, snap)
    }

    /// `unregister_hooks` must actually clear hooks registered under the
    /// same name; replacing the body with `()` leaves the count at 1.
    #[test]
    fn plugin_api_unregister_hooks_clears_registry() {
        let (api, _rx, hooks, _cmds, _snap) = mk_api();
        api.register_hook("h", Box::new(|_| true));
        assert_eq!(hooks.read().unwrap().hook_count("h"), 1);
        api.unregister_hooks("h");
        assert_eq!(hooks.read().unwrap().hook_count("h"), 0);
    }

    /// `register_command` / `unregister_command` must actually write through
    /// to the shared `CommandRegistry`.
    #[test]
    fn plugin_api_register_and_unregister_command_write_through() {
        let (api, _rx, _hooks, cmds, _snap) = mk_api();

        api.register_command(mk_cmd("x"));
        assert_eq!(cmds.read().unwrap().commands.read().unwrap().len(), 1);

        api.unregister_command("x");
        assert_eq!(cmds.read().unwrap().commands.read().unwrap().len(), 0);
    }

    /// Macro: assert that calling `$call` on a fresh `PluginApi` produces
    /// exactly one `PluginCommand` matching `$pattern` with the additional
    /// invariants in `$guard`.
    macro_rules! assert_dispatches {
        ($call:expr, $pattern:pat $(if $guard:expr)?) => {{
            let (api, rx, _h, _c, _s) = mk_api();
            let _ = $call(&api);
            match rx.try_recv().expect("no command sent") {
                $pattern $(if $guard)? => {}
                other => panic!("unexpected command variant: {:?}", other),
            }
        }};
    }

    /// Every simple `send_command`-based method on `PluginApi` translates
    /// its arguments into the documented `PluginCommand` variant with the
    /// expected fields.
    #[test]
    fn plugin_api_send_command_methods_dispatch_correctly() {
        // delete_range
        assert_dispatches!(
            |a: &PluginApi| a.delete_range(BufferId(7), 3..9),
            PluginCommand::DeleteRange { buffer_id, range }
                if buffer_id == BufferId(7) && range == (3..9)
        );

        // remove_overlay
        assert_dispatches!(
            |a: &PluginApi| a.remove_overlay(BufferId(2), "h-1".into()),
            PluginCommand::RemoveOverlay { buffer_id, handle }
                if buffer_id == BufferId(2) && handle.as_str() == "h-1"
        );

        // clear_namespace
        assert_dispatches!(
            |a: &PluginApi| a.clear_namespace(BufferId(3), "diag".into()),
            PluginCommand::ClearNamespace { buffer_id, namespace }
                if buffer_id == BufferId(3) && namespace.as_str() == "diag"
        );

        // clear_overlays_in_range
        assert_dispatches!(
            |a: &PluginApi| a.clear_overlays_in_range(BufferId(4), 10, 20),
            PluginCommand::ClearOverlaysInRange { buffer_id, start, end }
                if buffer_id == BufferId(4) && start == 10 && end == 20
        );

        // open_file_at_location
        assert_dispatches!(
            |a: &PluginApi| a.open_file_at_location(
                PathBuf::from("/tmp/x.rs"), Some(4), Some(8)
            ),
            PluginCommand::OpenFileAtLocation { path, line, column }
                if path == PathBuf::from("/tmp/x.rs")
                    && line == Some(4)
                    && column == Some(8)
        );

        // open_file_in_split
        assert_dispatches!(
            |a: &PluginApi| a.open_file_in_split(
                2, PathBuf::from("/tmp/y.rs"), Some(5), None
            ),
            PluginCommand::OpenFileInSplit { split_id, path, line, column }
                if split_id == 2
                    && path == PathBuf::from("/tmp/y.rs")
                    && line == Some(5)
                    && column.is_none()
        );

        // start_prompt
        assert_dispatches!(
            |a: &PluginApi| a.start_prompt("label".into(), "cmd".into()),
            PluginCommand::StartPrompt { label, prompt_type }
                if label == "label" && prompt_type == "cmd"
        );

        // set_prompt_suggestions
        assert_dispatches!(
            |a: &PluginApi| a.set_prompt_suggestions(vec![
                Suggestion::new("one".into()),
                Suggestion::new("two".into()),
            ]),
            PluginCommand::SetPromptSuggestions { suggestions }
                if suggestions.len() == 2
                    && suggestions[0].text == "one"
                    && suggestions[1].text == "two"
        );

        // set_prompt_input_sync
        assert_dispatches!(
            |a: &PluginApi| a.set_prompt_input_sync(true),
            PluginCommand::SetPromptInputSync { sync } if sync
        );
        assert_dispatches!(
            |a: &PluginApi| a.set_prompt_input_sync(false),
            PluginCommand::SetPromptInputSync { sync } if !sync
        );

        // add_menu_item
        assert_dispatches!(
            |a: &PluginApi| a.add_menu_item(
                "File".into(),
                MenuItem::Label { info: "info".into() },
                MenuPosition::Bottom,
            ),
            PluginCommand::AddMenuItem { menu_label, item, position }
                if menu_label == "File"
                    && matches!(item, MenuItem::Label { ref info } if info == "info")
                    && matches!(position, MenuPosition::Bottom)
        );

        // add_menu
        assert_dispatches!(
            |a: &PluginApi| a.add_menu(
                Menu {
                    id: None,
                    label: "Help".into(),
                    items: vec![],
                    when: None,
                },
                MenuPosition::After("Edit".into()),
            ),
            PluginCommand::AddMenu { menu, position }
                if menu.label == "Help"
                    && matches!(position, MenuPosition::After(ref s) if s == "Edit")
        );

        // remove_menu_item
        assert_dispatches!(
            |a: &PluginApi| a.remove_menu_item("File".into(), "Open".into()),
            PluginCommand::RemoveMenuItem { menu_label, item_label }
                if menu_label == "File" && item_label == "Open"
        );

        // remove_menu
        assert_dispatches!(
            |a: &PluginApi| a.remove_menu("File".into()),
            PluginCommand::RemoveMenu { menu_label } if menu_label == "File"
        );

        // create_virtual_buffer
        assert_dispatches!(
            |a: &PluginApi| a.create_virtual_buffer("buf".into(), "mode".into(), true),
            PluginCommand::CreateVirtualBuffer { name, mode, read_only }
                if name == "buf" && mode == "mode" && read_only
        );

        // create_virtual_buffer_with_content
        assert_dispatches!(
            |a: &PluginApi| a.create_virtual_buffer_with_content(
                "n".into(), "m".into(), false, vec![]
            ),
            PluginCommand::CreateVirtualBufferWithContent {
                name, mode, read_only, show_line_numbers, show_cursors,
                editing_disabled, hidden_from_tabs, request_id, ..
            }
                if name == "n" && mode == "m" && !read_only
                    && show_line_numbers && show_cursors
                    && !editing_disabled && !hidden_from_tabs
                    && request_id.is_none()
        );

        // set_virtual_buffer_content
        assert_dispatches!(
            |a: &PluginApi| a.set_virtual_buffer_content(BufferId(9), vec![]),
            PluginCommand::SetVirtualBufferContent { buffer_id, entries }
                if buffer_id == BufferId(9) && entries.is_empty()
        );

        // get_text_properties_at_cursor
        assert_dispatches!(
            |a: &PluginApi| a.get_text_properties_at_cursor(BufferId(11)),
            PluginCommand::GetTextPropertiesAtCursor { buffer_id }
                if buffer_id == BufferId(11)
        );

        // define_mode
        assert_dispatches!(
            |a: &PluginApi| a.define_mode(
                "m".into(),
                vec![("j".into(), "move_down".into())],
                true,
                false,
            ),
            PluginCommand::DefineMode {
                name, bindings, read_only, allow_text_input, inherit_normal_bindings, plugin_name
            }
                if name == "m"
                    && bindings.len() == 1
                    && bindings[0].0 == "j"
                    && bindings[0].1 == "move_down"
                    && read_only
                    && !allow_text_input
                    && !inherit_normal_bindings
                    && plugin_name.is_none()
        );

        // show_buffer
        assert_dispatches!(
            |a: &PluginApi| a.show_buffer(BufferId(77)),
            PluginCommand::ShowBuffer { buffer_id } if buffer_id == BufferId(77)
        );

        // set_split_scroll
        assert_dispatches!(
            |a: &PluginApi| a.set_split_scroll(5, 128),
            PluginCommand::SetSplitScroll { split_id, top_byte }
                if split_id == SplitId(5) && top_byte == 128
        );

        // get_highlights
        assert_dispatches!(
            |a: &PluginApi| a.get_highlights(BufferId(1), 0..10, 7),
            PluginCommand::RequestHighlights { buffer_id, range, request_id }
                if buffer_id == BufferId(1) && range == (0..10) && request_id == 7
        );
    }

    /// `get_active_split_id` reads the snapshot verbatim; a non-{0,1}
    /// sentinel value kills both the `0` and `1` constant-return mutants.
    #[test]
    fn plugin_api_get_active_split_id_reads_snapshot() {
        let (api, _rx, _h, _c, snap) = mk_api();
        snap.write().unwrap().active_split_id = 42;
        assert_eq!(api.get_active_split_id(), 42);
    }

    /// `state_snapshot_handle` returns a clone of the same `Arc`, not a
    /// freshly-defaulted snapshot. A distinguishing field value on the
    /// original state proves that the handle sees it.
    #[test]
    fn plugin_api_state_snapshot_handle_shares_underlying_arc() {
        let (api, _rx, _h, _c, snap) = mk_api();
        snap.write().unwrap().active_buffer_id = BufferId(42);

        let h = api.state_snapshot_handle();
        assert_eq!(h.read().unwrap().active_buffer_id, BufferId(42));
        assert!(Arc::ptr_eq(&h, &snap));
    }

    /// `KillHostProcess` survives a round-trip through serde: the
    /// `process_id` field stays identified by name and the variant
    /// retains its tag shape. If a future contributor renames the
    /// field or splits it into a tuple, the plugin-runtime TS side
    /// (which hand-builds the command JSON for the dispatcher) would
    /// silently break — this test pins the wire format.
    #[test]
    fn plugin_command_kill_host_process_serde_round_trip() {
        let cmd = PluginCommand::KillHostProcess { process_id: 1234 };
        let json = serde_json::to_value(&cmd).unwrap();
        assert_eq!(json["KillHostProcess"]["process_id"], 1234);
        let decoded: PluginCommand = serde_json::from_value(json).unwrap();
        match decoded {
            PluginCommand::KillHostProcess { process_id } => assert_eq!(process_id, 1234),
            other => panic!("expected KillHostProcess, got {:?}", other),
        }
    }
}
