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
use crate::WindowId;
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
    /// Response to `WatchPath`. `handle` is the editor's stable
    /// id for this watcher, used both as the cancellation token
    /// for `UnwatchPath` and as the routing key in
    /// `path_changed` event payloads. `Err` indicates the watcher
    /// could not be installed (path missing, kernel limit, etc.).
    WatchPathRegistered {
        request_id: u64,
        result: Result<u64, String>,
    },
}

impl PluginResponse {
    pub fn request_id(&self) -> u64 {
        match self {
            Self::VirtualBufferCreated { request_id, .. }
            | Self::TerminalCreated { request_id, .. }
            | Self::LspRequest { request_id, .. }
            | Self::HighlightsComputed { request_id, .. }
            | Self::BufferText { request_id, .. }
            | Self::LineStartPosition { request_id, .. }
            | Self::LineEndPosition { request_id, .. }
            | Self::BufferLineCount { request_id, .. }
            | Self::CompositeBufferCreated { request_id, .. }
            | Self::SplitByLabel { request_id, .. }
            | Self::WatchPathRegistered { request_id, .. } => *request_id,
        }
    }
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

/// `serde(default)` fallback for `EditorStateSnapshot.active_window_id`
/// — old serialized snapshots predate the field. Falls back to the
/// always-present base session (id 1).
fn default_window_id() -> WindowId {
    WindowId(1)
}

/// Information about an editor session (plugin-visible). Returned
/// by `editor.listWindows()` and carried in the snapshot. Mirrors
/// the editor-side `Session` struct — see
/// `crates/fresh-editor/src/app/session.rs` and
/// `docs/internal/orchestrator-sessions-design.md`.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct WindowInfo {
    /// Stable session id. The base session is always `1`.
    #[ts(type = "number")]
    pub id: WindowId,
    /// User-visible label (defaults to root basename).
    pub label: String,
    /// Absolute project root.
    #[ts(type = "string")]
    pub root: PathBuf,
    /// Project this session belongs to — the canonical repo
    /// root (or arbitrary directory) the user pointed the
    /// new-session form at. `null` for legacy sessions that
    /// predate the Project Path field. The Orchestrator Open
    /// dialog filters by this so the "this project's sessions"
    /// view is one keystroke away from the all-projects view.
    #[ts(type = "string | null")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub project_path: Option<PathBuf>,
    /// `true` when the session shares its working tree with
    /// other sessions (worktree-creation was off at session
    /// time, or the session lives in a non-git directory).
    /// Persistence-only field; defaults to `false` and isn't
    /// emitted when false.
    #[ts(type = "boolean")]
    #[serde(skip_serializing_if = "is_false_field", default)]
    pub shared_worktree: bool,
}

fn is_false_field(b: &bool) -> bool {
    !b
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

/// Modifier-only overlay applied to a byte range within a virtual line's
/// text. Used by plugins (live-diff) to bold + underline removed words on
/// a deletion virtual line without varying the line's overall fg/bg.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct VirtualLineTextOverlay {
    /// Inclusive byte offset within the virtual line's `text`.
    pub start: u32,
    /// Exclusive byte offset within the virtual line's `text`.
    pub end: u32,
    #[serde(default)]
    pub bold: bool,
    #[serde(default)]
    pub underline: bool,
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

/// A run of text with optional styling. `style` reuses
/// [`OverlayOptions`] — the same primitive plugins use for virtual
/// text — so a hint is just `{ text: "Alt+P cycle", style: { fg:
/// "ui.help_key_fg" } }`. `None` style means "no styling override";
/// each consumer applies its own default (e.g. the floating-prompt
/// title uses `prompt_fg` + bold).
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct StyledText {
    pub text: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional, type = "Partial<OverlayOptions>")]
    pub style: Option<OverlayOptions>,
}

#[cfg(feature = "plugins")]
impl<'js> rquickjs::FromJs<'js> for StyledText {
    fn from_js(_ctx: &rquickjs::Ctx<'js>, value: rquickjs::Value<'js>) -> rquickjs::Result<Self> {
        rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
            from: "object",
            to: "StyledText",
            message: Some(e.to_string()),
        })
    }
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

/// Color carried by a `ViewTokenStyle`. Untagged so JSON plugins can
/// keep passing `[r, g, b]` arrays, while richer themes can use named
/// ANSI colors (`"Red"`, `"LightGreen"`, `"Default"`) or theme keys
/// (`"editor.diff_remove_bg"`). The renderer resolves named/theme
/// strings against the active theme at draw time; unknown strings
/// fall through to the terminal's default color.
///
/// `Color::Indexed(N)` round-trips through the `"Indexed:N"` form so
/// 256-color values from a ratatui `Color` survive the
/// `ViewTokenStyle` boundary.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(untagged)]
#[ts(export)]
pub enum TokenColor {
    /// RGB color as [r, g, b] array
    #[ts(type = "[number, number, number]")]
    Rgb(u8, u8, u8),
    /// Named ANSI color, `"Default"`, `"Indexed:N"`, or a theme key.
    Named(String),
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
    /// Foreground color. Either `[r, g, b]` or a named/theme string —
    /// see [`TokenColor`].
    #[serde(default)]
    pub fg: Option<TokenColor>,
    /// Background color. Either `[r, g, b]` or a named/theme string —
    /// see [`TokenColor`].
    #[serde(default)]
    pub bg: Option<TokenColor>,
    /// Whether to render in bold
    #[serde(default)]
    pub bold: bool,
    /// Whether to render in italic
    #[serde(default)]
    pub italic: bool,
    /// Whether to render with underline
    #[serde(default)]
    pub underline: bool,
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
    /// Editor's working directory (for file operations and spawning processes).
    ///
    /// Equal to `sessions[i].root` where `sessions[i].id == active_window_id`.
    /// Plugins that just need "where am I" can read this directly; plugins
    /// orchestrating multiple sessions (Orchestrator) iterate `sessions`.
    pub working_dir: PathBuf,
    /// All editor sessions, in id order. Always non-empty (the base
    /// session is `id == 1`). Updated when sessions are
    /// created/closed or relabelled.
    #[serde(default)]
    pub windows: Vec<WindowInfo>,
    /// Id of the currently active session. Always present in
    /// `sessions`. Read by plugins via `editor.activeWindow()`.
    #[serde(default = "default_window_id")]
    pub active_window_id: WindowId,
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
    /// Last-seen grammar registry generation. The state-snapshot updater
    /// rebuilds `available_grammars` only when this disagrees with the
    /// registry's current `catalog_gen()`. `#[serde(skip)]` because the
    /// counter is a host-side detail not exposed to plugins.
    #[serde(skip)]
    #[ts(skip)]
    pub last_grammar_gen: u64,
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

    /// Plugin-managed per-session state, snapshotted as the
    /// **active** session's plugin_state map. Updated wholesale
    /// on `setActiveWindow` (alongside the rest of the
    /// per-session state) — plugins that read this via
    /// `editor.getWindowState(key)` see the active session's
    /// values without crossing the IPC boundary on every read.
    /// Outer key is plugin name, inner is the plugin-defined key.
    #[serde(default)]
    #[ts(type = "any")]
    pub active_session_plugin_states: HashMap<String, HashMap<String, serde_json::Value>>,

    /// Total terminal dimensions in cells. Refreshed on every
    /// resize event. Plugins read this via `editor.getScreenSize()`
    /// when they need to size floating overlays against the whole
    /// terminal — `getViewport()` only reports the active split,
    /// which is smaller than the screen whenever splits exist.
    #[serde(default)]
    pub terminal_width: u16,
    #[serde(default)]
    pub terminal_height: u16,
}

/// Total terminal size in cells. Returned by `editor.getScreenSize()`.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct ScreenSize {
    pub width: u16,
    pub height: u16,
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
            windows: Vec::new(),
            active_window_id: WindowId(1),
            authority_label: String::new(),
            diagnostics: Arc::new(HashMap::new()),
            folding_ranges: Arc::new(HashMap::new()),
            config: Arc::new(serde_json::Value::Null),
            user_config: Arc::new(serde_json::Value::Null),
            available_grammars: Vec::new(),
            last_grammar_gen: 0,
            editor_mode: None,
            plugin_view_states: HashMap::new(),
            plugin_view_states_split: 0,
            keybinding_labels: HashMap::new(),
            plugin_global_states: HashMap::new(),
            active_session_plugin_states: HashMap::new(),
            terminal_width: 0,
            terminal_height: 0,
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

// ===========================================================================
// Widget library — plugin-facing declarative UI.
//
// Plugins describe a widget tree as a `WidgetSpec`; the host reconciles the
// tree against the previous spec for the same panel and produces rendered
// output. This is the foundation laid out in
// `docs/internal/plugin-widget-library-design.md`.
//
// The set of widget kinds is intentionally narrow at v1 (`HintBar` and the
// `Row`/`Col`/`Raw` composition primitives). Additional kinds (`Toggle`,
// `Button`, `TextInput`, `List`, `Tree`, `Layer`, `Transient`, `Table`)
// extend the enum without changing the `MountWidgetPanel`/`UpdateWidgetPanel`
// IPC shape.
// ===========================================================================

/// One entry in a `HintBar` — a key chord plus its label.
/// Renders as `<keys> <label>` with the key portion styled by the
/// `ui.help_key_fg` theme key.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct HintEntry {
    /// The key chord, e.g. `"Tab"`, `"Alt+P"`, `"Esc"`.
    pub keys: String,
    /// The human-readable label for the action.
    pub label: String,
}

/// Default for `TextInput::cursor_byte` when the plugin doesn't
/// supply one. -1 ⇒ "no cursor visible" (the field is unfocused
/// or read-only).
fn default_cursor_byte() -> i32 {
    -1
}

/// Default for `List::selected_index` when the plugin doesn't
/// supply one. -1 ⇒ "no selection".
fn default_list_selected() -> i32 {
    -1
}

/// Default visible-rows for a `List` when the plugin doesn't supply
/// one. 20 is a reasonable terminal-panel default.
fn default_list_visible_rows() -> u32 {
    20
}

/// Default for `Tree::selected_index`. -1 ⇒ "no selection".
fn default_tree_selected() -> i32 {
    -1
}

/// Default visible-rows for a `Tree`. Same default as `List`.
fn default_tree_visible_rows() -> u32 {
    20
}

/// Default `rows` for a `Text` widget — `1` ⇒ single-line. Plugins
/// opt into multi-line by setting `rows >= 2`.
fn default_text_rows() -> u32 {
    1
}

/// One node in a `Tree` widget's flat-list spec. The plugin walks
/// its hierarchy depth-first and emits one `TreeNode` per node;
/// `depth` controls indent, `has_children` controls whether the
/// disclosure glyph (and its hit area) is rendered. The host filters
/// the visible window — descendants of collapsed nodes are skipped.
///
/// `text` is the pre-rendered row content. The host prepends the
/// indent + disclosure glyph at render time and shifts the entry's
/// inline overlays accordingly; plugins emit `text` (and overlays)
/// in the row's own coordinate space, starting at column 0.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct TreeNode {
    /// The pre-rendered row content (text + per-row overlays).
    /// The host renders this verbatim after the indent + disclosure
    /// prefix; plugin overlays are byte-shifted by the prefix
    /// length.
    pub text: crate::text_property::TextPropertyEntry,
    /// 0-based depth — controls leading indent (`depth * 2` spaces).
    #[serde(default)]
    pub depth: u32,
    /// When true, render a disclosure glyph (`▶` collapsed / `▼`
    /// expanded) and emit a hit area over it that fires the `expand`
    /// event. Leaf nodes (`false`) get no glyph and no expand hit;
    /// the row width occupies the full row.
    #[serde(default)]
    pub has_children: bool,
    /// Per-node checkbox state. Only rendered when the parent
    /// `Tree` has `checkable: true`. `None` = no checkbox glyph;
    /// `Some(true)` = `[v]`; `Some(false)` = `[ ]`. The plugin
    /// owns the truth — the host fires `widget_event { event_type:
    /// "toggle" }` and the plugin pushes the new state back via
    /// `WidgetMutation::SetCheckedKeys`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub checked: Option<bool>,
}

/// Visual role for a `Button`. Maps to theme keys at render time —
/// plugins describe intent, not colors. See §7 of the design doc.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, TS, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub enum ButtonKind {
    /// A regular action button — no special emphasis.
    #[default]
    Normal,
    /// The primary affirmative action (e.g. "Submit", "Replace All").
    /// Rendered with bold weight; the focused state uses the active
    /// menu/selection theme keys.
    Primary,
    /// A destructive action (e.g. "Delete"). Rendered with the
    /// theme's error/warning palette.
    Danger,
}

/// Declarative widget tree. Each variant is one node; nested
/// composition is via `Row { children }` / `Col { children }`.
///
/// `key` is the stable identifier used by the reconciler to match a
/// node across `MountWidgetPanel` / `UpdateWidgetPanel` calls — when
/// the plugin re-emits a Spec, instance state (cursor offset, scroll,
/// expanded keys, hover) is preserved on nodes whose `key` matches.
/// Plugins should provide stable keys for any widget that owns
/// instance state; stateless widgets (`HintBar`, `Toggle`, `Button`,
/// `Spacer`) can omit it.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(
    tag = "kind",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
#[ts(export, rename_all = "camelCase")]
pub enum WidgetSpec {
    /// Horizontal layout: children laid out left-to-right.
    Row {
        children: Vec<WidgetSpec>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Vertical layout: children stacked top-to-bottom.
    Col {
        children: Vec<WidgetSpec>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Keyboard-hint footer (one row, comma-separated `<keys> <label>` items).
    HintBar {
        entries: Vec<HintEntry>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Boolean toggle, rendered as `[v] label` / `[ ] label`. The
    /// `focused` flag controls the focus-styling overlay; the host
    /// will own focus once the keymap layer is wired (today the
    /// plugin passes it explicitly per render).
    Toggle {
        checked: bool,
        label: String,
        #[serde(default)]
        focused: bool,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Action button, rendered as `[ Label ]` (or `[ Label ]` with
    /// emphasized styling for `Primary`/`Danger`). Focused buttons
    /// flip foreground/background using the active menu theme keys.
    ///
    /// `intent` is the button's visual role (`Normal` / `Primary` /
    /// `Danger`); the field is named `intent` rather than `kind`
    /// because `kind` is the discriminator for the outer `WidgetSpec`
    /// tag.
    Button {
        label: String,
        #[serde(default)]
        focused: bool,
        #[serde(default)]
        intent: ButtonKind,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Horizontal whitespace eater. In a `Row`, produces `cols`
    /// spaces (or fills remaining width if `flex: true`); in a
    /// `Col`, produces `cols` blank lines (`flex` is ignored).
    ///
    /// `flex: true` distributes the row's leftover width — `panel
    /// width - sum(non-flex child widths)` — across flex spacers.
    /// With multiple flex spacers in one row the leftover splits
    /// evenly. With no leftover (children already exceed panel
    /// width), the flex spacer collapses to zero.
    Spacer {
        #[serde(default)]
        cols: u32,
        #[serde(default)]
        flex: bool,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Vertical list of pre-rendered rows with host-managed
    /// selection styling, click routing, **and virtual scrolling**.
    ///
    /// The plugin passes the *full dataset* of items + a
    /// `visible_rows` count (typically the panel's available
    /// height). The host owns the scroll offset as widget instance
    /// state, keyed by the spec's `key` — so a `key` is required for
    /// any List that should preserve scroll across re-renders. The
    /// scroll offset auto-clamps to keep `selected_index` in view;
    /// plugins never compute scroll math.
    ///
    /// Each item is one rendered row (`TextPropertyEntry`).
    /// `item_keys` is a parallel array of stable per-item identifiers
    /// the plugin uses to map a click event back to its model
    /// (e.g. `"file:5/match:23"`); the array length must match
    /// `items.len()`. Missing keys default to empty string.
    ///
    /// `selected_index` is the *absolute* index into `items`
    /// (`-1` for no selection); the host paints the selected row
    /// with `ui.menu_active_bg` extended to line end. Clicks fire
    /// `widget_event { event_type: "select",
    ///                payload: { index, key } }`
    /// where `index` is the absolute (not visible-window) index.
    List {
        items: Vec<crate::text_property::TextPropertyEntry>,
        #[serde(default)]
        item_keys: Vec<String>,
        #[serde(default = "default_list_selected")]
        selected_index: i32,
        /// Number of rows of the panel's available height the list
        /// should occupy. Plugin computes from its viewport. The
        /// host shows up to this many items per render.
        #[serde(default = "default_list_visible_rows")]
        visible_rows: u32,
        /// Whether `Tab` / `Shift+Tab` will land focus on this
        /// list. Defaults to `true` (lists are normal tabbable
        /// widgets). Picker-style usage typically sets this to
        /// `false` so Tab moves between the filter input and
        /// the action buttons, while Up/Down on the focused
        /// filter still forwards to the list via host smart-key
        /// dispatch.
        #[serde(default = "default_true")]
        focusable: bool,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Hierarchical list with host-managed expand/collapse, selection
    /// styling, click routing, and virtual scrolling.
    ///
    /// The plugin emits its tree as a depth-first flat list of
    /// `TreeNode`s (each carrying a `depth` and `has_children` flag)
    /// plus a parallel `item_keys` array. The host filters out
    /// descendants of collapsed nodes when rendering the visible
    /// window, so the plugin always emits the *full* tree — toggling
    /// expansion is host-owned (instance state) rather than the
    /// plugin re-emitting on every `▶`/`▼` press.
    ///
    /// `expanded_keys` is initial-only (seeded into instance state
    /// on first render); subsequent expansion changes flow through
    /// `WidgetCommand::Key` (Right/Left) or click on the disclosure
    /// glyph — neither requires the plugin to re-emit. Plugins that
    /// need to react to expansion changes listen for
    /// `widget_event { event_type: "expand" }`.
    ///
    /// `selected_index` is the *absolute* index into `nodes`
    /// (initial-only; instance state takes over). Click on a row
    /// fires `widget_event { event_type: "select", payload: { index,
    /// key } }`; click on the disclosure column fires
    /// `widget_event { event_type: "expand", payload: { index, key,
    /// expanded } }`. Enter/Space on the focused tree fires
    /// `widget_event { event_type: "activate", payload: { index, key } }`.
    Tree {
        nodes: Vec<TreeNode>,
        #[serde(default)]
        item_keys: Vec<String>,
        #[serde(default = "default_tree_selected")]
        selected_index: i32,
        #[serde(default = "default_tree_visible_rows")]
        visible_rows: u32,
        /// Initial-only set of expanded item keys. Once the widget
        /// has rendered, the host's instance-state `expanded_keys`
        /// is authoritative; updating this field on subsequent specs
        /// has no effect (use `WidgetMutation::SetExpandedKeys` to
        /// override host state).
        #[serde(default)]
        expanded_keys: Vec<String>,
        /// When true, every node with `checked: Some(_)` renders a
        /// `[v]` / `[ ]` glyph and emits a `toggle` hit area over
        /// the glyph. Click on the glyph fires `widget_event {
        /// event_type: "toggle", payload: { key, checked: <new> } }`;
        /// the plugin updates its model and pushes the new state
        /// back via `WidgetMutation::SetCheckedKeys`.
        #[serde(default)]
        checkable: bool,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Single-line text input, rendered as `[value]` with a cursor
    /// highlight at the byte position given by `cursor_byte` (when
    /// `cursor_byte >= 0`). When `value` is empty and the input is
    /// not focused, `placeholder` (if set) is shown instead.
    ///
    /// v1 is a *render-only* widget: the host owns visual cursor
    /// styling and theme-keyed focus, but the plugin still owns the
    /// value string and cursor position. Keystrokes (Backspace,
    /// arrows, character input) flow through the plugin's existing
    /// `defineMode` + `mode_text_input` plumbing; the plugin re-emits
    /// the spec on every change. The keymap-routing layer (host
    /// claims widget keys before the plugin sees them) lands in a
    /// later commit.
    /// Text input — single-line (`rows == 1`, default) or multi-line
    /// (`rows > 1`). The host owns `value` and `cursor_byte` as
    /// instance state once the widget renders for the first time;
    /// the spec's values are initial-only.
    ///
    /// Single-line vs multi-line behaviour is selected by `rows`:
    /// * `rows == 1` — renders as `[value]` with the cursor pinned
    ///   to a constant `field_width` (head-truncate when the value
    ///   exceeds it). `Enter` advances focus (form-like UX).
    ///   `Up`/`Down` are no-ops. `Home`/`End` jump to the start /
    ///   end of the whole value.
    /// * `rows > 1` — renders as `rows` lines tall (padded with
    ///   blanks when `value` is shorter). `Enter` inserts a newline
    ///   at the cursor. `Up`/`Down` move between lines (clamped to
    ///   each line's column count). `Home`/`End` jump within the
    ///   current line. The host auto-scrolls vertically to keep
    ///   the cursor's line visible.
    ///
    /// Smart-key dispatch (`WidgetCommand::Key`) selects the right
    /// behaviour from `rows`. Plugins that want a different `Enter`
    /// binding intercept the key in their own mode binding before
    /// dispatching it through the smart-key router.
    ///
    /// `label` (when non-empty) renders inline before `[` for
    /// single-line, and as a row above the editing region for
    /// multi-line. `placeholder` shows when `value` is empty and
    /// the field is unfocused (first row only for multi-line).
    /// `field_width` controls visible column width: `0` = auto-fit
    /// (single-line) or panel width (multi-line). `max_visible_chars`
    /// is a single-line soft cap applied after the field-width pad
    /// (`0` = no cap; ignored when `rows > 1`).
    Text {
        /// Initial text. Spec value is read at first render only;
        /// instance state takes over thereafter.
        #[serde(default)]
        value: String,
        /// Initial byte-offset cursor within `value`. Negative
        /// (encoded as `i32` in JSON) means "no cursor" — clamped
        /// to `[0, value.len()]` host-side.
        #[serde(default = "default_cursor_byte")]
        cursor_byte: i32,
        /// Whether this widget has visual focus.
        #[serde(default)]
        focused: bool,
        /// Optional label rendered before / above the editing
        /// region. Empty = omitted.
        #[serde(default, skip_serializing_if = "String::is_empty")]
        label: String,
        /// Placeholder shown when unfocused and `value` is empty.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        placeholder: Option<String>,
        /// Number of visible rows of editing region. `0` falls back
        /// to `1` (single-line). `1` = single-line behaviour;
        /// `>= 2` = multi-line behaviour. See the type-level doc
        /// for the per-mode semantics.
        #[serde(default = "default_text_rows")]
        rows: u32,
        /// Visible column width. `0` = auto-fit (single-line) or
        /// panel width (multi-line). When set, single-line
        /// head-truncates with `…` and multi-line tail-truncates
        /// per-line.
        #[serde(default)]
        field_width: u32,
        /// Single-line soft cap on visible chars after the
        /// `field_width` pad. `0` = no cap. Ignored when `rows > 1`.
        #[serde(default)]
        max_visible_chars: u32,
        /// Stretch the visible field to fill the available
        /// width of the enclosing container. Overrides
        /// `field_width` when set: the renderer computes
        /// `panel_width - label_overhead - bracket_overhead` as
        /// the effective visible width. Multi-line widgets
        /// already fill the panel width by default; this flag is
        /// most useful for single-line inputs inside a
        /// `LabeledSection` or a flexible row.
        #[serde(default)]
        full_width: bool,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Visual grouping container: renders a rounded thin border
    /// around a single child widget, with `label` printed as a
    /// top-left legend overlapping the border (HTML `<fieldset>`
    /// semantics).
    ///
    /// Layout (border drawn with `╭─╮│╰─╯`):
    /// ```text
    /// ╭─ Label ──────────────────╮
    /// │ <child rendered content> │
    /// ╰──────────────────────────╯
    /// ```
    ///
    /// Width: the section always occupies the full `panel_width`
    /// passed down by its parent container. The child is rendered
    /// with `panel_width - 4` (two border columns + two padding
    /// columns) so widgets that honour `full_width` size
    /// themselves to the inner area.
    ///
    /// The child can be any single `WidgetSpec` — typically a
    /// `Text` input, but a `Toggle`/`Button`/nested `Col` also
    /// works. Focus, hit areas and cursor positions bubble up
    /// from the child unchanged, shifted by the section's border
    /// offset (1 row down, 2 columns in).
    LabeledSection {
        /// Legend text printed in the top border. Empty = no
        /// legend (the top border becomes one unbroken line).
        #[serde(default)]
        label: String,
        /// The single wrapped widget. Boxed because `WidgetSpec`
        /// is recursive.
        child: Box<WidgetSpec>,
        /// When this section is a Block child of a Row, request
        /// `width_pct` percent of the row's `panel_width` instead
        /// of the equal-split default. Multiple siblings with
        /// `width_pct` set sum to ≤ 100; the remainder splits
        /// equally among siblings without an explicit width.
        /// Out-of-range values (0 or > 100) fall back to the
        /// equal-split path.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        width_pct: Option<u32>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Reserve a rectangle in the widget layout for the host to
    /// natively paint the editor `Window` identified by
    /// `window_id`. The widget itself renders only blank lines
    /// so subsequent passes (split tree, terminal grids, syntax
    /// highlighting, decorations) can be drawn into the
    /// reserved cells by the existing per-window render path.
    ///
    /// `rows` controls the embed's height. Width is whatever
    /// the parent container allocates (`panel_width` for a
    /// direct Col child; the block's `column_width` inside a
    /// Row's horizontal-zip path). Used by Orchestrator's open
    /// dialog so the preview pane shows a live render of the
    /// highlighted session.
    WindowEmbed {
        /// Numeric editor-window id, matching `WindowId(N).0`.
        /// `0` (or any unknown id) renders empty placeholder
        /// rows without dispatching the per-window render.
        /// `u32` rather than `u64` to keep the TS binding a
        /// plain `number`; window ids never exceed 4B in
        /// practice.
        window_id: u32,
        /// Number of visible rows the embed should occupy.
        rows: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Imperative-virtual-buffer escape hatch. The plugin supplies
    /// `TextPropertyEntry[]` exactly as it would for
    /// `setVirtualBufferContent`; the host inlines those entries into
    /// the rendered panel without further interpretation. Used during
    /// migration to wrap existing hand-rolled rendering inside a new
    /// widget panel.
    Raw {
        entries: Vec<crate::text_property::TextPropertyEntry>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
    /// Float `child` over the rest of the layout instead of
    /// consuming vertical space. Placed inside a `Col`, the
    /// overlay anchors at the row it would have occupied if it
    /// were a regular child — but the rows below it DO NOT
    /// shift down. At paint time the overlay is drawn last,
    /// over whatever's beneath it, like a tooltip / popup.
    ///
    /// Use case: dropdown completions, hover popups, transient
    /// hints that should appear right next to the focused
    /// widget without reflowing the rest of the panel each
    /// time they show / hide.
    ///
    /// Hit testing: overlays paint on top, so clicks inside an
    /// overlay's region go to the overlay (not whatever's
    /// underneath). Tab cycle: the host's `collect_tabbable`
    /// walks into the overlay's child like any other widget;
    /// give the child a `key` if you want it focusable, or
    /// leave it keyless to keep it out of the cycle.
    Overlay {
        child: Box<WidgetSpec>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        key: Option<String>,
    },
}

impl WidgetSpec {
    /// Iterate this widget's immediate child specs in declaration
    /// order. Container kinds (`Row`, `Col`, `LabeledSection`)
    /// return their nested children; leaf kinds return an empty
    /// iterator.
    ///
    /// Generic tree walkers (focus dispatch, hit-area lookup,
    /// scrollable-widget detection, instance-state mutation) call
    /// this instead of pattern-matching every container variant
    /// by hand, so adding a new container kind is a single update
    /// here rather than touching every walker. The box is the
    /// price for returning an iterator whose type depends on the
    /// variant; the allocation is single-digit-byte and dwarfed
    /// by everything else in the dispatch path.
    pub fn children(&self) -> Box<dyn Iterator<Item = &WidgetSpec> + '_> {
        match self {
            WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
                Box::new(children.iter())
            }
            WidgetSpec::LabeledSection { child, .. } | WidgetSpec::Overlay { child, .. } => {
                Box::new(std::iter::once(child.as_ref()))
            }
            _ => Box::new(std::iter::empty()),
        }
    }

    /// Mutable counterpart of [`children`]. Same set of container
    /// kinds, same semantics — the iterator yields exclusive
    /// references so walkers that mutate (e.g. `set_*_in_spec`)
    /// can recurse generically.
    pub fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut WidgetSpec> + '_> {
        match self {
            WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
                Box::new(children.iter_mut())
            }
            WidgetSpec::LabeledSection { child, .. } | WidgetSpec::Overlay { child, .. } => {
                Box::new(std::iter::once(child.as_mut()))
            }
            _ => Box::new(std::iter::empty()),
        }
    }
}

/// Action a plugin can request the widget runtime to perform on a
/// mounted panel. Bundled into a single `WidgetCommand` PluginCommand
/// so the plugin's TypeScript layer exposes one routing method
/// (`editor.widgetCommand(panel_id, action)`) rather than a fanout
/// of per-key IPC.
///
/// All actions target the panel's currently focused widget (the host
/// tracks focus per panel). They are fired by the plugin's mode
/// bindings — Tab → `FocusAdvance{+1}`, Enter → `Activate`,
/// Up/Down → `SelectMove{±1}`, Backspace → `TextInputKey{"Backspace"}`,
/// printable chars (via `mode_text_input`) → `TextInputChar{"x"}`.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(
    tag = "kind",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
#[ts(export, rename_all = "camelCase")]
pub enum WidgetAction {
    /// Cycle focus to the next (`delta=+1`) or previous (`delta=-1`)
    /// tabbable widget in declaration order. Wraps at the ends.
    FocusAdvance { delta: i32 },
    /// "Activate" the focused widget — fires a semantic event
    /// keyed on widget kind: `Button` → `widget_event { event_type:
    /// "activate" }`; `Toggle` → `widget_event { event_type:
    /// "toggle", payload: { checked: !old } }`. No-op for other
    /// kinds.
    Activate,
    /// Move the focused `List`'s selection by `delta`. Plugins
    /// listen for `widget_event { event_type: "select" }` to mirror
    /// the new index into their model. No-op when the focused
    /// widget isn't a List.
    SelectMove { delta: i32 },
    /// Apply a non-printable editing key to the focused
    /// `TextInput`: `"Backspace"`, `"Delete"`, `"Left"`, `"Right"`,
    /// `"Home"`, `"End"`. Host computes the new value/cursor and
    /// fires `widget_event { event_type: "change", payload: { value,
    /// cursorByte } }`. No-op when the focused widget isn't a
    /// TextInput or the key isn't recognised.
    TextInputKey { key: String },
    /// Append printable text to the focused `TextInput` at the
    /// current cursor position. Used for the `mode_text_input`
    /// fall-through path. Fires `widget_event` as for `TextInputKey`.
    TextInputChar { text: String },
    /// "Smart" keystroke dispatch — the host routes based on the
    /// focused widget's kind without the plugin needing to know
    /// what's focused. This is the recommended path for plugin
    /// mode bindings: bind every relevant key to one handler that
    /// calls `editor.widgetCommand(panel_id, key("Tab"))` etc.
    ///
    /// Dispatch table:
    ///
    /// | Key                                   | TextInput   | TextArea          | Toggle / Button | List       | Tree                | (no focus) |
    /// |---------------------------------------|-------------|-------------------|-----------------|------------|---------------------|------------|
    /// | `Tab`                                 | focus +1    | focus +1          | focus +1        | focus +1   | focus +1            | no-op      |
    /// | `Shift+Tab`                           | focus -1    | focus -1          | focus -1        | focus -1   | focus -1            | no-op      |
    /// | `Backspace` / `Delete`                | text-edit   | text-edit         | no-op           | no-op      | no-op               | no-op      |
    /// | `Home` / `End`                        | text-edit   | line-start / -end | no-op           | no-op      | no-op               | no-op      |
    /// | `Left`                                | text-edit   | text-edit         | no-op           | no-op      | collapse / parent   | no-op      |
    /// | `Right`                               | text-edit   | text-edit         | no-op           | no-op      | expand              | no-op      |
    /// | `Up`                                  | no-op       | line up           | no-op           | select -1  | select -1 (visible) | no-op      |
    /// | `Down`                                | no-op       | line down         | no-op           | select +1  | select +1 (visible) | no-op      |
    /// | `Enter`                               | focus +1    | insert `\n`       | activate        | activate   | activate            | no-op      |
    /// | `Space`                               | char " "    | char " "          | activate        | activate   | activate            | no-op      |
    /// | (anything else)                       | no-op       | no-op             | no-op           | no-op      | no-op               | no-op      |
    ///
    /// "no-op" still returns successfully — plugins can rely on the
    /// command not erroring when the focused widget can't handle the
    /// key. Plugins that want to fall back to their own behaviour
    /// when the widget doesn't claim a key should bind those keys
    /// to plugin-specific handlers instead.
    Key { key: String },
}

/// Targeted in-place mutation of a mounted widget panel — the
/// IPC fast path. Plugins use these when the model change touches
/// one widget; the host applies the mutation directly to the
/// panel's spec / instance state and re-renders without
/// re-transmitting the full spec.
///
/// `UpdateWidgetPanel` remains the right tool for structural
/// changes (adding/removing widgets, restructuring layout). Both
/// paths preserve instance state via widget keys.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(
    tag = "kind",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
#[ts(export, rename_all = "camelCase")]
pub enum WidgetMutation {
    /// Set a `TextInput`'s value and (optionally) cursor byte.
    /// Mutates instance state directly.
    SetValue {
        widget_key: String,
        value: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        cursor_byte: Option<i32>,
    },
    /// Set a `Toggle`'s checked state. Mutates the Toggle's
    /// `checked` field in the spec.
    SetChecked { widget_key: String, checked: bool },
    /// Set a `List`'s selected index (instance state).
    SetSelectedIndex { widget_key: String, index: i32 },
    /// Replace a `List`'s items + parallel `item_keys`. Mutates
    /// the List in the spec.
    SetItems {
        widget_key: String,
        items: Vec<crate::text_property::TextPropertyEntry>,
        #[serde(default)]
        item_keys: Vec<String>,
    },
    /// Replace a `Tree`'s expanded-keys instance state. Plugins use
    /// this when a non-user action needs to drive expansion (e.g.
    /// "expand all", reveal-on-search). `Right`/`Left` arrow keys
    /// and disclosure clicks already mutate expansion host-side
    /// without the plugin's involvement.
    SetExpandedKeys {
        widget_key: String,
        keys: Vec<String>,
    },
    /// Set `checked` to the given value on every node in `keys`.
    /// Mutates the Tree's nodes in the spec; the renderer sees
    /// the change on the next paint without a full spec re-emit.
    /// Used by the `toggle` event handler in plugins to push the
    /// new checkbox state back after a user click or `x` keypress.
    SetCheckedKeys {
        widget_key: String,
        checked: bool,
        keys: Vec<String>,
    },
    /// Append `new_nodes` (and parallel `new_item_keys`) to an
    /// existing Tree's node list. Streaming-friendly counterpart to
    /// `SetItems`: a plugin streaming thousands of results sends only
    /// the per-batch delta instead of re-transmitting the entire tree
    /// on every batch. Existing selection / scroll / expansion state
    /// is preserved across the append.
    AppendTreeNodes {
        widget_key: String,
        new_nodes: Vec<crate::api::TreeNode>,
        #[serde(default)]
        new_item_keys: Vec<String>,
    },
    /// Replace a `Raw` widget's entries in place. The streaming search
    /// pump uses this to update small bits of chrome (the matchStats
    /// label, the separator "Matches (N in M files)" header) without
    /// re-emitting the full panel spec — the latter would force a
    /// `js_to_json` walk of every node in a 5 000-row tree once
    /// streaming finishes, blocking the JS thread for ~1 second.
    SetRawEntries {
        widget_key: String,
        entries: Vec<crate::text_property::TextPropertyEntry>,
    },
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

    /// Register a custom statusbar token
    RegisterStatusBarElement {
        plugin_name: String,
        token_name: String,
        title: String,
    },

    /// Set the value of a status-bar token for a specific buffer.
    /// `key` is the full "plugin_name:token_name" form.
    SetStatusBarValue {
        buffer_id: u64,
        key: String,
        value: String,
    },

    /// Unregister a command by name
    UnregisterCommand { name: String },

    /// Create a new editor session rooted at `root`.
    ///
    /// `root` must be an absolute path; relative paths are rejected
    /// rather than silently joined onto the active session's root —
    /// that ambiguity would leak the wrong cwd into agent processes.
    /// `label` may be empty; the editor falls back to the basename
    /// of `root` (matching `Session::new`).
    ///
    /// The new session's id is assigned by the editor and reported
    /// back via the `session_created` plugin hook (id, label, root
    /// in payload). Sessions are not made active on creation;
    /// follow up with `SetActiveWindow` to dive.
    CreateWindow { root: PathBuf, label: String },

    /// Make `id` the active session. No-op if `id` is already
    /// active. Fires `active_session_changed` on transition.
    /// Errors (id not found) are logged via tracing rather than
    /// surfaced to the plugin — the plugin can verify by reading
    /// `editor.activeWindow()` after.
    SetActiveWindow { id: WindowId },

    /// Close a session and drop its associated state. Refuses to
    /// close the currently active session — the caller must switch
    /// first. Fires `session_closed` on success.
    CloseWindow { id: WindowId },

    /// Eagerly initialise an inactive session's per-session state
    /// (file tree walk, ignore matcher, etc.) without diving. The
    /// only thing that's actually pre-warmed today is the file
    /// explorer's root walk; LSP servers boot on first buffer
    /// open and watcher setup happens on first `watchPath` call,
    /// so those are unaffected. No-op for the active session
    /// (already warm) or unknown id.
    PrewarmWindow { id: WindowId },

    /// Register a filesystem path watcher. The editor returns the
    /// allocated `handle` via the async response so the plugin can
    /// match `path_changed` events back to the call. `recursive`
    /// follows `notify::RecursiveMode`; non-recursive watches
    /// cover only the named path itself (or its direct children
    /// for directories on macOS — kqueue limitation).
    WatchPath {
        path: PathBuf,
        recursive: bool,
        request_id: u64,
    },

    /// Drop a path watcher previously registered via
    /// `WatchPath`. Unknown handles are silently ignored — the
    /// editor's view of "what's still watched" can drift if a
    /// plugin reloads, and the design doesn't make plugins
    /// reconcile.
    UnwatchPath { handle: u64 },

    /// Tell the editor that the floating-overlay prompt's
    /// preview pane should render the **entire** split tree of
    /// session `id` (Primitive #1 in
    /// `docs/internal/orchestrator-sessions-design.md`). `None`
    /// clears the override and falls back to the existing
    /// path-based phantom-leaf preview.
    ///
    /// Orchestrator sets this when the user navigates the session
    /// list so the right-hand pane shows the highlighted
    /// session's full editor UI live (splits, terminals,
    /// syntax highlighting, decorations) — rendered natively
    /// by reusing the editor's existing render_content path
    /// against the previewed session's stashed split tree.
    PreviewWindowInRect { id: Option<WindowId> },

    /// Open a file in the editor (in background, without switching focus).
    ///
    /// `window_id` defaults to the active session at dispatch
    /// time. When set to an inactive session, the file's buffer
    /// is loaded as usual but attached to that session's
    /// membership and split tree — the active session's UI is
    /// undisturbed.
    OpenFileInBackground {
        path: PathBuf,
        #[serde(default)]
        window_id: Option<WindowId>,
    },

    /// Insert text at the current cursor position in the active buffer
    InsertAtCursor { text: String },

    /// Spawn an async process
    ///
    /// When `stdout_to` is `Some(path)`, the child's stdout is piped
    /// directly into that file on disk (via `tokio::io::copy`) rather
    /// than being buffered in memory. The resulting `SpawnResult.stdout`
    /// is empty in that case; `stderr` and `exit_code` are populated as
    /// usual. This lets large outputs (e.g. `git show` for a big commit)
    /// stay on disk and be opened as a file-backed buffer without ever
    /// crossing the JS bridge.
    SpawnProcess {
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        #[serde(default)]
        stdout_to: Option<PathBuf>,
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

    /// Plugin-managed per-session state. Writes to the **currently
    /// active** session's `plugin_state` map keyed by
    /// `(plugin_name, key)`. Other sessions' state is unaffected.
    /// `None` means delete (matches `SetGlobalState` semantics).
    SetWindowState {
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
        /// Optional gutter glyph rendered in the line-number column on
        /// the first visual row of this virtual line. Used by diff
        /// plugins to put a "-" directly on the deletion line itself
        /// instead of the source line that follows it.
        gutter_glyph: Option<String>,
        /// Color for `gutter_glyph` (RGB or theme key). Falls back to
        /// `theme.line_number_fg` when `None`.
        gutter_color: Option<OverlayColorSpec>,
        /// Per-range modifier overlays applied on top of the base fg/bg.
        /// Offsets are byte offsets within `text`, not buffer bytes.
        /// Used e.g. by live-diff to bold + underline removed words on
        /// a deletion virtual line.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        text_overlays: Vec<VirtualLineTextOverlay>,
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

    /// Publish a set of fold ranges on the buffer in the same shape
    /// LSP `textDocument/foldingRange` populates. The ranges are
    /// stored as **toggleable** — the standard `toggle_fold` keybinding
    /// finds them via `state.folding_ranges` and collapses/expands on
    /// demand. Unlike `AddFold`, this does not pre-collapse anything.
    ///
    /// Designed for plugins that derive structural folds from buffer
    /// content (e.g. git-log's per-file / per-hunk diff structure)
    /// without driving an LSP. Replacing call replaces the prior set.
    SetFoldingRanges {
        buffer_id: BufferId,
        #[ts(type = "any")]
        ranges: Vec<lsp_types::FoldingRange>,
    },

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
        /// When true, the prompt renders as a centred floating
        /// overlay rather than a bottom-row minibuffer. Used for
        /// Live Grep (issue #1796). Defaults to false at the wire
        /// level via `#[serde(default)]`.
        #[serde(default)]
        floating_overlay: bool,
    },

    /// Start a prompt with pre-filled initial value
    StartPromptWithInitial {
        label: String,
        prompt_type: String,
        initial_value: String,
        /// See `StartPrompt::floating_overlay`.
        #[serde(default)]
        floating_overlay: bool,
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

    /// Set the title shown in a floating-overlay prompt's frame
    /// header (issue #1796) as styled segments. Each segment carries
    /// optional `OverlayOptions`, so plugins can theme keybinding
    /// hints with `fg: "ui.help_key_fg"`, separators with
    /// `fg: "ui.popup_border_fg"`, etc. An empty vec clears the
    /// title and falls back to the prompt-type default. Has no
    /// visible effect on non-overlay prompts.
    SetPromptTitle { title: Vec<StyledText> },

    /// Plugin-supplied footer chrome rendered along the bottom
    /// row of the floating-overlay's results pane (Primitive #2
    /// chrome region in
    /// `docs/internal/orchestrator-sessions-design.md`). Orchestrator
    /// uses this for hotkey-hint rows. Empty vec clears the
    /// footer. Has no visible effect on non-overlay prompts.
    SetPromptFooter { footer: Vec<StyledText> },

    /// Override the currently-highlighted suggestion row in the
    /// open prompt. Clamped to the suggestion list's bounds; out-
    /// of-range indices snap to the last row. No-op when there is
    /// no open prompt or the list is empty. The renderer scrolls
    /// the selection into view on the next frame.
    SetPromptSelectedIndex { index: u32 },

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
        /// Optional split role tag. When `Some("utility_dock")`, the
        /// dispatcher routes the buffer to the existing dock leaf if
        /// one exists; otherwise it seeds a new dock leaf with the
        /// requested direction/ratio.
        role: Option<String>,
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

    /// Close all buffers in the split except the specified one
    CloseOtherBuffersInSplit {
        buffer_id: BufferId,
        split_id: SplitId,
    },

    /// Close all buffers in the split
    CloseAllBuffersInSplit { split_id: SplitId },

    /// Close all buffers to the right of the specified buffer in the split
    CloseBuffersToRightInSplit {
        buffer_id: BufferId,
        split_id: SplitId,
    },

    /// Close all buffers to the left of the specified buffer in the split
    CloseBuffersToLeftInSplit {
        buffer_id: BufferId,
        split_id: SplitId,
    },

    /// Move the active tab one position to the left within its split
    MoveTabLeft,

    /// Move the active tab one position to the right within its split
    MoveTabRight,

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

    /// Open `path` as a regular buffer in forced large-file (file-backed)
    /// mode regardless of file size. Designed for buffers whose backing
    /// file will grow under them (e.g. a temp file fed by `spawnProcess`
    /// with `stdoutTo`). Resolves with the new buffer's id.
    ///
    /// Pair with `RefreshBufferFromDisk` to grow the buffer as the file
    /// is written.
    OpenFileStreaming {
        /// Path to open. May not yet exist or may be empty.
        path: PathBuf,
        /// Request ID for async response (the buffer_id).
        request_id: u64,
    },

    /// Re-stat the file backing `buffer_id` and extend the buffer if
    /// the file has grown. No-op if the buffer has no file path or the
    /// file didn't grow. Resolves with the new total byte length.
    RefreshBufferFromDisk {
        buffer_id: BufferId,
        /// Request ID for async response.
        request_id: u64,
    },

    /// Re-point a buffer-group's panel at a different buffer id.
    /// Used by streaming plugins (e.g. git-log) to swap one
    /// file-backed buffer for another when the user navigates to a
    /// new commit, without rebuilding the group layout. Both
    /// `group.panel_buffers[panel_name]` and the corresponding
    /// `SplitViewState.active_buffer` are updated; layout is marked
    /// dirty for the next render.
    ///
    /// Resolves with `true` on success, `false` if the group or panel
    /// is missing.
    SetBufferGroupPanelBuffer {
        group_id: usize,
        panel_name: String,
        buffer_id: BufferId,
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

    /// Contribute (or replace, or clear) a set of menu rows for the
    /// LSP-Servers popup (the popup opened by clicking the LSP
    /// indicator). Each plugin owns its own slice keyed by
    /// `plugin_id`; passing an empty `items` clears that slice.
    ///
    /// Rationale: previously plugins reacting to `lsp_status_clicked`
    /// pushed their own separate action popup via `ShowActionPopup`,
    /// which stacked over the built-in LSP-Servers popup and created
    /// the UX conflict in PR #1941. This command lets plugins
    /// contribute rows that merge into the existing popup instead.
    /// Selecting a contributed row fires `action_popup_result` with
    /// `popup_id = "lsp_status"` and `action_id =
    /// "{plugin_id}|{id}"`.
    SetLspMenuContributions {
        /// Stable plugin identifier used both as the namespace for
        /// this slice of contributions and as the prefix of the
        /// resulting `action_popup_result.action_id`.
        plugin_id: String,
        /// Language whose LSP-Servers popup should display these
        /// rows (e.g. "rust", "python").
        language: String,
        /// The rows to install. Empty clears any previous
        /// contribution from this `plugin_id` for this `language`.
        items: Vec<LspMenuItem>,
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
        /// Optional session id to attach the new terminal buffer to.
        /// `None` (default) attaches to the active session at creation
        /// time — the historical behaviour. `Some(id)` lets Orchestrator
        /// (and any plugin spawning agents in worktrees) attach the
        /// terminal to its target session without diving first; the
        /// terminal's split is created in that session's stashed split
        /// tree, and the buffer is added to the target session's
        /// `Session.buffers` membership rather than the active one's.
        /// Falls back to active session if the id is unknown.
        #[serde(default)]
        window_id: Option<WindowId>,
        /// Argv to spawn directly in the PTY in lieu of the host's
        /// configured shell. See `CreateTerminalOptions::command` for
        /// the full semantics — `None` keeps the shell-and-type
        /// behaviour, `Some(argv)` runs `argv` as the PTY child.
        #[serde(default)]
        command: Option<Vec<String>>,
        /// Tab title override. Defaults to `command[0]` (when
        /// `command` is set) or `"Terminal N"` (when it isn't).
        /// See `CreateTerminalOptions::title`.
        #[serde(default)]
        title: Option<String>,
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

    /// Send `signal` to every process group tracked by the
    /// window `id`. `signal` is one of `"SIGTERM"` / `"SIGKILL"`
    /// / `"SIGINT"` / `"SIGHUP"`; the window's authority
    /// determines the actual delivery mechanism (local
    /// `kill(-pgid, …)` on host, `docker exec kill …` for
    /// container authorities, SSH agent for remote ones —
    /// see `app/window/process_group.rs`). Idempotent across
    /// already-exited groups: callers can retry safely.
    SignalWindow { id: WindowId, signal: String },

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

    /// Project-wide streaming search using a pull-based handle.
    ///
    /// The plugin allocates `handle_id` and registers an `Arc<SearchHandleState>`
    /// in the shared `SearchHandleRegistry` before sending this command. The
    /// editor's searcher tasks look up the same entry and write matches
    /// directly into its `pending` vec — no per-chunk JS dispatch. The plugin
    /// drains state via `editor._searchHandleTake(handle_id)` at its own pace.
    BeginSearch {
        /// Search pattern
        pattern: String,
        /// Whether the pattern is a fixed string (true) or regex (false)
        fixed_string: bool,
        /// Whether the search is case-sensitive
        case_sensitive: bool,
        /// Maximum number of results before the search self-truncates
        max_results: usize,
        /// Whether to match whole words only
        whole_words: bool,
        /// Handle ID — key into the shared `SearchHandleRegistry`
        handle_id: u64,
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

    /// Mount a declarative widget panel inside an existing virtual
    /// buffer. The host renders the `WidgetSpec` and writes the
    /// resulting text-property entries into the buffer. The
    /// `panel_id` is plugin-allocated (any unique u64 for that
    /// plugin) and is used to address the panel for later
    /// `UpdateWidgetPanel` / `UnmountWidgetPanel` calls.
    ///
    /// See `docs/internal/plugin-widget-library-design.md`.
    MountWidgetPanel {
        panel_id: u64,
        buffer_id: BufferId,
        spec: WidgetSpec,
    },

    /// Replace the spec of a previously-mounted widget panel.
    /// The reconciler diffs against the previous spec and applies
    /// the minimum mutation; widget instance state is preserved on
    /// nodes whose `key` matches.
    UpdateWidgetPanel { panel_id: u64, spec: WidgetSpec },

    /// Tear down a widget panel. Subsequent `UpdateWidgetPanel`
    /// calls for the same `panel_id` are no-ops.
    UnmountWidgetPanel { panel_id: u64 },

    /// Route a keystroke / nav action to the panel's currently
    /// focused widget. The plugin's `defineMode` bindings dispatch
    /// here for keys that should be handled by the widget layer
    /// (Tab cycle, Enter to activate, Up/Down to navigate a List,
    /// Backspace / arrows / printable input to edit a TextInput).
    /// See `WidgetAction` for the action shapes.
    WidgetCommand { panel_id: u64, action: WidgetAction },

    /// Apply a targeted mutation to a mounted widget panel
    /// without re-transmitting the full spec. The IPC fast path
    /// for hot-path updates (typing, selection moves, partial
    /// list refreshes). See `WidgetMutation` for the shapes.
    WidgetMutate {
        panel_id: u64,
        mutation: WidgetMutation,
    },

    /// Mount a declarative widget panel as a centered floating
    /// overlay rather than into a virtual buffer. `width_pct` and
    /// `height_pct` size the overlay rect relative to the terminal
    /// (clamped 1..=100). Only one floating widget panel may be
    /// mounted at a time; a second `MountFloatingWidget` replaces
    /// any existing one.
    MountFloatingWidget {
        panel_id: u64,
        spec: WidgetSpec,
        width_pct: u8,
        height_pct: u8,
    },

    /// Replace the spec of the currently-mounted floating widget
    /// panel. No-op when no floating panel is mounted, or when the
    /// `panel_id` doesn't match the mounted one.
    UpdateFloatingWidget { panel_id: u64, spec: WidgetSpec },

    /// Tear down the floating widget panel. No-op when no floating
    /// panel is mounted, or when the `panel_id` doesn't match.
    UnmountFloatingWidget { panel_id: u64 },
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

/// Plugin-contributed row in the LSP-Servers popup.
/// See `PluginCommand::SetLspMenuContributions`.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export, rename = "TsLspMenuItem")]
pub struct LspMenuItem {
    /// Stable identifier used as the `action_id` in the resulting
    /// `action_popup_result` event (prefixed by `{plugin_id}|`).
    pub id: String,
    /// Display label shown in the popup row.
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

/// Per-call result from `SearchHandle.take()` — the matches accumulated since
/// the previous call plus terminal-state flags.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct SearchTakeResult {
    /// Matches discovered since the previous take()
    pub matches: Vec<GrepMatch>,
    /// Whether the producer has finished (no more matches will arrive)
    pub done: bool,
    /// Total number of matches the producer has emitted across all batches
    /// (including ones already drained on prior take() calls)
    #[ts(type = "number")]
    pub total_seen: usize,
    /// Whether the producer stopped early because it hit `maxResults`
    pub truncated: bool,
    /// Producer error, if any (e.g., invalid regex). When set, `done` is also true.
    #[ts(optional, type = "string | null")]
    pub error: Option<String>,
}

/// Inner state of a streaming search, written by the host's parallel
/// searchers and drained by the plugin via `SearchHandle.take()`. The plugin
/// observes deltas (`mem::take` on `pending`) at its own cadence; producers
/// write at full speed without per-chunk dispatches.
#[derive(Debug, Default)]
pub struct SearchState {
    /// Matches accumulated since the consumer's last drain
    pub pending: Vec<GrepMatch>,
    /// Total matches the producer has emitted across the search's lifetime
    pub total_seen: usize,
    /// Set when the producer stopped early due to hitting max_results
    pub truncated: bool,
    /// Set when the producer is fully done — no more writes will occur
    pub done: bool,
    /// Producer error, if any (final state)
    pub error: Option<String>,
}

/// A search handle's shared state plus its cancellation flag. Owned by an
/// `Arc` so producers (host searcher tasks) and consumers (the JS plugin via
/// the registry) can both reference it.
#[derive(Debug)]
pub struct SearchHandleState {
    pub state: std::sync::Mutex<SearchState>,
    pub cancel: std::sync::atomic::AtomicBool,
}

impl SearchHandleState {
    pub fn new() -> Self {
        Self {
            state: std::sync::Mutex::new(SearchState::default()),
            cancel: std::sync::atomic::AtomicBool::new(false),
        }
    }
}

impl Default for SearchHandleState {
    fn default() -> Self {
        Self::new()
    }
}

/// Registry mapping a handle ID to its shared `SearchHandleState`. Shared
/// between the JS thread (where `JsEditorApi` registers handles and serves
/// `take()`/`cancel()`) and the editor thread (where the host's searcher
/// tasks write into the same state).
pub type SearchHandleRegistry = Arc<std::sync::Mutex<HashMap<u64, Arc<SearchHandleState>>>>;

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
    /// See `TextPropertyEntry::pad_to_chars`.
    #[serde(default)]
    #[ts(optional)]
    pub pad_to_chars: Option<u32>,
    /// See `TextPropertyEntry::truncate_to_chars`.
    #[serde(default)]
    #[ts(optional)]
    pub truncate_to_chars: Option<u32>,
    /// See `TextPropertyEntry::segments`.
    #[serde(default)]
    #[ts(optional)]
    pub segments: Option<Vec<crate::text_property::StyledSegment>>,
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
    /// Split role tag. When set to `"utility_dock"`, the dispatcher
    /// routes this buffer to the existing dock leaf if one exists,
    /// instead of creating a new split. See
    /// `docs/internal/tui-editor-layout-design.md` Section 2.
    #[serde(default)]
    #[ts(optional)]
    pub role: Option<String>,
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
    /// Optional session id to attach the new terminal buffer to.
    /// Defaults to the active session at creation time. Setting this
    /// lets Orchestrator and similar plugins spawn a terminal *into* an
    /// inactive session (e.g. an agent in a worktree the user hasn't
    /// dived into yet). The terminal's split is created in that
    /// session's stashed split tree; the buffer is attached to the
    /// target session's membership set rather than the active one's.
    #[serde(default, rename = "windowId")]
    #[ts(optional, rename = "windowId")]
    pub window_id: Option<WindowId>,
    /// Argv to spawn directly inside the PTY instead of the host's
    /// configured shell. `None` (default) keeps the historical
    /// behaviour: spawn the user's shell and let the caller type into
    /// it via `sendTerminalInput`. `Some([cmd, ...args])` runs that
    /// exact command as the PTY child — no shell middleman, so the
    /// process exits cleanly when the agent does and the
    /// terminal-buffer's `terminal_exit` plugin hook reflects the
    /// agent's real exit status. Used by Orchestrator so a session
    /// with agent `python3` is just python3 in the PTY rather than
    /// bash-running-python3-as-a-subshell-command.
    #[serde(default)]
    #[ts(optional)]
    pub command: Option<Vec<String>>,
    /// Tab title for the terminal buffer. Defaults to `command[0]`
    /// (when `command` is set) or `"Terminal N"` (the historical
    /// auto-numbered title). If another terminal in the same window
    /// already uses the requested title, the host appends `" (k)"`
    /// to disambiguate. Empty string is treated the same as `None`.
    #[serde(default)]
    #[ts(optional)]
    pub title: Option<String>,
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
        LspMenuItem,
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
            // `fg`/`bg` are `Option<TokenColor>` (untagged: RGB array or
            // named string). rquickjs_serde struggles with the untagged
            // variant from a plain JS array, so we pin down the boolean
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
        self.send_command(PluginCommand::StartPrompt {
            label,
            prompt_type,
            floating_overlay: false,
        })
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

    /// Set the floating-overlay prompt's title (issue #1796) as
    /// styled segments. An empty vec clears the title and falls
    /// back to the prompt-type default.
    pub fn set_prompt_title(&self, title: Vec<StyledText>) -> Result<(), String> {
        self.send_command(PluginCommand::SetPromptTitle { title })
    }

    /// Set the floating-overlay prompt's footer chrome row.
    /// Plugins use this for hotkey hints / footer banners along
    /// the bottom of the results pane. Empty vec clears.
    pub fn set_prompt_footer(&self, footer: Vec<StyledText>) -> Result<(), String> {
        self.send_command(PluginCommand::SetPromptFooter { footer })
    }

    /// Override the currently-highlighted suggestion row in the
    /// open prompt. Useful when re-opening a picker and wanting
    /// the previously-active entry to come up pre-selected
    /// (e.g. Orchestrator highlighting the active session). The
    /// editor clamps `index` to the list's bounds.
    pub fn set_prompt_selected_index(&self, index: u32) -> Result<(), String> {
        self.send_command(PluginCommand::SetPromptSelectedIndex { index })
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

    /// Get total terminal dimensions.
    pub fn get_screen_size(&self) -> ScreenSize {
        let snapshot = self.state_snapshot.read().unwrap();
        ScreenSize {
            width: snapshot.terminal_width,
            height: snapshot.terminal_height,
        }
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
    use std::path::Path;

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
            terminal_bypass: false,
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

    type MkApi = (
        PluginApi,
        std::sync::mpsc::Receiver<PluginCommand>,
        Arc<RwLock<HookRegistry>>,
        Arc<RwLock<CommandRegistry>>,
        Arc<RwLock<EditorStateSnapshot>>,
    );

    fn mk_api() -> MkApi {
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
                if path == Path::new("/tmp/x.rs")
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
                    && path == Path::new("/tmp/y.rs")
                    && line == Some(5)
                    && column.is_none()
        );

        // start_prompt
        assert_dispatches!(
            |a: &PluginApi| a.start_prompt("label".into(), "cmd".into()),
            PluginCommand::StartPrompt { label, prompt_type, floating_overlay }
                if label == "label" && prompt_type == "cmd" && !floating_overlay
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

    // ==================== SearchHandle behavior ====================

    fn dummy_match(line: usize) -> GrepMatch {
        GrepMatch {
            file: "fixture.rs".to_string(),
            buffer_id: 0,
            byte_offset: 0,
            length: 4,
            line,
            column: 1,
            context: "match".to_string(),
        }
    }

    /// Pull-based handle batches matches between drains: a producer that
    /// pushes N matches across multiple writes hands them to the consumer
    /// in a single take(), and a follow-up take() with no new writes
    /// returns an empty batch — proving the architectural property the
    /// new API was built around (no per-chunk dispatch).
    #[test]
    fn search_handle_batches_between_takes() {
        let handle = Arc::new(SearchHandleState::new());

        // Three independent writer batches simulate three searcher tasks
        // pushing into the shared state.
        for chunk in [vec![dummy_match(1), dummy_match(2)], vec![dummy_match(3)]] {
            let count = chunk.len();
            let mut state = handle.state.lock().unwrap();
            state.pending.extend(chunk);
            state.total_seen += count;
        }

        // First take drains everything written so far.
        let drained: Vec<_> = {
            let mut s = handle.state.lock().unwrap();
            std::mem::take(&mut s.pending)
        };
        assert_eq!(drained.len(), 3);
        assert_eq!(handle.state.lock().unwrap().total_seen, 3);

        // Second take with no producer activity yields an empty batch.
        let empty: Vec<_> = {
            let mut s = handle.state.lock().unwrap();
            std::mem::take(&mut s.pending)
        };
        assert!(empty.is_empty());
    }

    /// `cancel` is a one-way latch visible to producers and consumers.
    /// Setting it does not implicitly mark `done` — completion is the
    /// producer's responsibility — but a producer observing the flag
    /// should stop pushing.
    #[test]
    fn search_handle_cancel_is_observable() {
        let handle = Arc::new(SearchHandleState::new());
        assert!(!handle.cancel.load(std::sync::atomic::Ordering::Relaxed));

        handle
            .cancel
            .store(true, std::sync::atomic::Ordering::Relaxed);

        assert!(handle.cancel.load(std::sync::atomic::Ordering::Relaxed));
        assert!(!handle.state.lock().unwrap().done);
    }

    /// The terminal state transition: producers flip `done = true` once
    /// no more matches will arrive, with `truncated` reflecting whether
    /// the search hit `max_results`. Consumers learn the search is
    /// finished from the same `take()` that drains the final batch.
    #[test]
    fn search_handle_done_transition_is_visible_to_consumer() {
        let handle = Arc::new(SearchHandleState::new());

        // Producer pushes a final batch, then marks done.
        {
            let mut s = handle.state.lock().unwrap();
            s.pending.push(dummy_match(7));
            s.total_seen += 1;
            s.truncated = true;
            s.done = true;
        }

        let (matches, done, truncated) = {
            let mut s = handle.state.lock().unwrap();
            (std::mem::take(&mut s.pending), s.done, s.truncated)
        };

        assert_eq!(matches.len(), 1);
        assert!(done);
        assert!(truncated);
    }

    /// Producers and consumers must be able to interleave without
    /// blocking each other longer than a `mem::take` swap. This test
    /// drives writes from a worker thread while the main thread drains;
    /// it asserts the consumer eventually sees every match. With a
    /// per-chunk dispatch model an analogous test would deadlock or
    /// drop matches; with the pull model it converges.
    #[test]
    fn search_handle_concurrent_producer_consumer() {
        let handle = Arc::new(SearchHandleState::new());
        let producer = Arc::clone(&handle);
        let writer = std::thread::spawn(move || {
            for line in 1..=200 {
                let mut s = producer.state.lock().unwrap();
                s.pending.push(dummy_match(line));
                s.total_seen += 1;
            }
            producer.state.lock().unwrap().done = true;
        });

        let mut drained: Vec<GrepMatch> = Vec::new();
        loop {
            let (mut batch, done) = {
                let mut s = handle.state.lock().unwrap();
                (std::mem::take(&mut s.pending), s.done)
            };
            drained.append(&mut batch);
            if done {
                let mut tail = handle.state.lock().unwrap();
                drained.append(&mut std::mem::take(&mut tail.pending));
                break;
            }
            std::thread::yield_now();
        }
        writer.join().unwrap();
        assert_eq!(drained.len(), 200);
    }
}
