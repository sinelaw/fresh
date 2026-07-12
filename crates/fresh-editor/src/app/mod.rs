mod action_events;
mod active_focus;
mod async_dispatch;
mod async_messages;
mod bookmark_actions;
mod bookmarks;
mod buffer_close;
mod buffer_config_resolve;
mod buffer_groups;
mod buffer_management;
mod calibration_actions;
pub mod calibration_wizard;
mod click_geometry;
mod click_handlers;
mod clipboard;
mod composite_buffer_actions;
mod dabbrev_actions;
mod diagnostic_jumps;
mod editor_accessors;
mod editor_init;
mod event_apply;
pub mod event_debug;
mod event_debug_actions;
mod file_explorer;
pub mod file_open;
mod file_open_input;
mod file_open_orchestrators;
mod file_open_queue;
mod file_operations;
mod git_index;
mod help;
mod help_actions;
mod hover;
mod input;
mod input_dispatch;
mod input_helpers;
pub mod keybinding_editor;
mod keybinding_editor_actions;
mod lifecycle;
mod line_scan;
mod lsp_actions;
mod lsp_event_notify;
mod lsp_requests;
mod lsp_status;
mod macro_actions;
mod macro_codegen;
mod macros;
mod menu_actions;
mod menu_context;
mod mouse_input;
mod navigation;
mod on_save_actions;
mod orchestrator_persistence;
mod overlay;
mod path_utils;
#[cfg(feature = "plugins")]
mod plugin_commands;
#[cfg(feature = "plugins")]
mod plugin_dispatch;
mod popup_actions;
mod popup_dialogs;
mod popup_overlay_actions;
mod prompt_actions;
mod prompt_lifecycle;
mod recovery_actions;
mod regex_replace;
pub(crate) mod render;
mod scan_orchestrators;
mod scroll_sync;
mod scrollbar_input;
mod scrollbar_math;
mod search_ops;
mod search_scan;
mod settings_actions;
mod settings_prompts;
mod shell_command;
mod smart_home;
mod split_actions;
mod stdin_stream;
mod tab_drag;
mod terminal;
pub use terminal::PluginTerminalSpec;
mod terminal_input;
mod terminal_link;
mod terminal_mouse;
mod text_ops;
mod theme_inspect;
mod toggle_actions;
pub mod types;
mod undo_actions;
mod view_actions;
mod virtual_buffers;
pub mod warning_domains;
mod widget_runtime;
pub mod window;
mod window_actions;
pub mod window_resources;
pub mod workspace;

use anyhow::Result as AnyhowResult;
use rust_i18n::t;

/// Shared per-tick housekeeping: process async messages, check timers, auto-save, etc.
/// Returns true if a render is needed. The `clear_terminal` callback handles full-redraw
/// requests (terminal clears the screen; GUI can ignore or handle differently).
/// Used by both the terminal event loop and the GUI event loop.
pub fn editor_tick(
    editor: &mut Editor,
    mut clear_terminal: impl FnMut() -> AnyhowResult<()>,
) -> AnyhowResult<bool> {
    let mut needs_render = false;

    // Run the paste-deadline check *before* draining async messages
    // so a deadline-fired fallback and a real result that came in
    // the same tick are both handled in a single pass (the first to
    // touch `paste_pending` wins; the other is silently dropped).
    if editor.check_paste_deadline() {
        needs_render = true;
    }

    let async_messages = {
        let _s = tracing::info_span!("process_async_messages").entered();
        editor.process_async_messages()
    };
    if async_messages {
        needs_render = true;
    }
    // Keep the status-bar remote indicator in sync with a background
    // reconnect that never routes through an input event or async message.
    if editor.poll_remote_connection_changes() {
        needs_render = true;
    }
    let pending_file_opens = {
        let _s = tracing::info_span!("process_pending_file_opens").entered();
        editor.process_pending_file_opens()
    };
    if pending_file_opens {
        needs_render = true;
    }
    if editor.process_line_scan() {
        needs_render = true;
    }
    let search_scan = {
        let _s = tracing::info_span!("process_search_scan").entered();
        editor.process_search_scan()
    };
    if search_scan {
        needs_render = true;
    }
    let search_overlay_refresh = {
        let _s = tracing::info_span!("check_search_overlay_refresh").entered();
        editor.check_search_overlay_refresh()
    };
    if search_overlay_refresh {
        needs_render = true;
    }
    if editor.check_mouse_hover_timer() {
        needs_render = true;
    }
    if editor.active_window().check_semantic_highlight_timer() {
        needs_render = true;
    }
    if editor.check_completion_trigger_timer() {
        needs_render = true;
    }
    editor.active_window_mut().check_diagnostic_pull_timer();
    if editor.check_warning_log() {
        needs_render = true;
    }
    if editor.poll_stdin_streaming() {
        needs_render = true;
    }

    if let Err(e) = editor.auto_recovery_save_dirty_buffers() {
        tracing::debug!("Auto-recovery-save error: {}", e);
    }
    if let Err(e) = editor.auto_save_persistent_buffers() {
        tracing::debug!("Auto-save (disk) error: {}", e);
    }

    if editor.take_full_redraw_request() {
        clear_terminal()?;
        needs_render = true;
    }

    Ok(needs_render)
}

pub(crate) use path_utils::{
    explorer_path_under_root, normalize_explorer_plugin_path, normalize_path,
};

use self::types::{
    LspMenuItem, LspMessageEntry, LspProgressInfo, SearchState, TabContextMenu,
    DEFAULT_BACKGROUND_FILE,
};
use crate::config::Config;
use crate::config_io::DirectoryContext;
use crate::input::buffer_mode::ModeRegistry;
use crate::input::command_registry::CommandRegistry;
use crate::input::keybindings::{Action, KeyContext, KeybindingResolver};
use crate::input::quick_open::{
    BufferProvider, CommandProvider, FileProvider, GotoLineProvider, QuickOpenRegistry,
};
use crate::model::cursor::Cursors;
use crate::model::event::{Event, EventLog, LeafId, SplitDirection};
use crate::model::filesystem::FileSystem;
use crate::services::async_bridge::AsyncBridge;
use crate::services::fs::FsManager;
use crate::services::plugins::PluginManager;
use crate::services::recovery::{RecoveryConfig, RecoveryService};
use crate::services::time_source::{RealTimeSource, SharedTimeSource};
use crate::state::EditorState;
use crate::types::{LspLanguageConfig, LspServerConfig};
use crate::view::file_tree::{FileTree, FileTreeView};
use crate::view::prompt::PromptType;
use crate::view::split::{SplitManager, SplitViewState};
use crate::view::ui::{
    ExplorerDecorations, FileExplorerRenderer, SplitRenderer, StatusBarRenderer,
    SuggestionsRenderer,
};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::{
    layout::{Constraint, Direction, Layout},
    Frame,
};
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock};
use std::time::Instant;

// Re-export BufferId from event module for backward compatibility
pub use self::types::{BufferKind, BufferMetadata, HoverTarget};
pub use self::warning_domains::{
    GeneralWarningDomain, LspWarningDomain, WarningAction, WarningActionId, WarningDomain,
    WarningDomainRegistry, WarningLevel, WarningPopupContent,
};
pub use crate::model::event::BufferId;

/// Decode a wire-side LSP URI to a host path. Thin wrapper over
/// [`LspUri::to_host_path`](crate::app::types::LspUri::to_host_path)
/// that produces a `Result` for call sites that prefer the
/// error-string form. Editor code that owns a raw `lsp_types::Uri`
/// from a third-party type (e.g. `lsp_types::Location.uri`) wraps it
/// via [`LspUri::from_wire`](crate::app::types::LspUri::from_wire)
/// and then calls this — that's the only path from a wire URI to a
/// host `PathBuf`, by construction.
fn lsp_uri_to_host_path(
    uri: &crate::app::types::LspUri,
    translation: Option<&crate::services::authority::PathTranslation>,
) -> Result<PathBuf, String> {
    uri.to_host_path(translation)
        .ok_or_else(|| "URI is not a file path".to_string())
}

/// A pending grammar registration waiting for reload_grammars() to apply
#[derive(Clone, Debug)]
pub struct PendingGrammar {
    /// Language identifier (e.g., "elixir")
    pub language: String,
    /// Path to the grammar file (.sublime-syntax or .tmLanguage)
    pub grammar_path: String,
    /// File extensions to associate with this grammar
    pub extensions: Vec<String>,
}

/// Track an in-flight semantic token range request.
#[derive(Clone, Debug)]
pub(crate) struct SemanticTokenRangeRequest {
    pub(crate) buffer_id: BufferId,
    pub(crate) version: u64,
    pub(crate) range: Range<usize>,
    pub(crate) start_line: usize,
    pub(crate) end_line: usize,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum SemanticTokensFullRequestKind {
    Full,
    FullDelta,
}

#[derive(Clone, Debug)]
pub(crate) struct SemanticTokenFullRequest {
    pub(crate) buffer_id: BufferId,
    pub(crate) version: u64,
    pub(crate) kind: SemanticTokensFullRequestKind,
}

#[derive(Clone, Debug)]
pub(crate) struct FoldingRangeRequest {
    pub(crate) buffer_id: BufferId,
    pub(crate) version: u64,
}

#[derive(Clone, Debug)]
pub(crate) struct InlayHintsRequest {
    pub(crate) buffer_id: BufferId,
    pub(crate) version: u64,
}

/// State for the dabbrev cycling session (Alt+/ style).
///
/// When the user presses Alt+/ repeatedly, we cycle through candidates
/// in proximity order without showing a popup. The session is reset when
/// any other action is taken (typing, moving, etc.).
#[derive(Debug, Clone)]
pub struct DabbrevCycleState {
    /// The original prefix the user typed before the first expansion.
    pub original_prefix: String,
    /// Byte position where the prefix starts.
    pub word_start: usize,
    /// The list of candidates (ordered by proximity).
    pub candidates: Vec<String>,
    /// Current index into `candidates`.
    pub index: usize,
}

/// Snapshot of cursor and viewport state used to restore the original position
/// when a goto-line preview is abandoned (cancel, or the user edits the input
/// so it no longer targets a line).
///
/// Shared between Quick Open's `:N` syntax and the standalone `Goto Line`
/// prompt — both flows save a snapshot on the first preview jump and restore
/// it if the user cancels or clears the target.
///
/// `last_jump_position` is the byte offset the most recent preview jump put the
/// cursor at; the restore path only applies the snapshot when the cursor is
/// still exactly there. If anything else moved the cursor (mouse click, an
/// async buffer edit shifting positions via `adjust_for_edit`, …) the snapshot
/// is considered stale and simply dropped. This is the single staleness check
/// that replaces per-site invalidation across many call paths.
#[derive(Debug, Clone)]
pub(crate) struct GotoLinePreviewSnapshot {
    pub buffer_id: BufferId,
    pub split_id: LeafId,
    pub cursor_id: crate::model::event::CursorId,
    pub position: usize,
    pub anchor: Option<usize>,
    pub sticky_column: Option<usize>,
    pub viewport_top_byte: usize,
    pub viewport_top_view_line_offset: usize,
    pub viewport_left_column: usize,
    pub last_jump_position: usize,
}

/// The main editor struct - manages multiple buffers, clipboard, and rendering
pub struct Editor {
    // Buffers moved onto `Window` (Step 0c). Each window owns its
    // own buffer storage; opening the same file in two windows
    // produces two independent buffers. Access through
    // `Editor::buffers()` / `buffers_mut()` (active window) or by
    // direct `self.windows.get_mut(&id).unwrap().buffers` for
    // cross-window iteration.

    // NOTE: There is no `active_buffer` field. The active buffer is derived from
    // `split_manager.active_buffer_id()` to maintain a single source of truth.
    // Use `self.active_buffer()` to get the active buffer ID.
    // event_logs moved onto `Window` (Step 0e). Undo logs follow the
    // buffer storage, so they live alongside the buffer they describe.
    /// Next buffer ID to assign.
    ///
    /// **Use [`Self::alloc_buffer_id`] instead of direct mutation.**
    /// Direct `self.next_buffer_id += 1` works only on `impl Editor`
    /// — methods that run on `impl Window` allocate via the shared
    /// `Arc<BufferIdAllocator>` in `WindowResources`. Keeping this
    /// counter as the canonical source means a handler on `Editor`
    /// can still increment locally; the allocator's atomic stays in
    /// sync because both go through `alloc_buffer_id`.
    next_buffer_id: usize,

    /// Globally-unique buffer-id allocator shared by `Arc` clone with
    /// every `Window` (via `WindowResources`). Handlers on
    /// `impl Window` call `Window::alloc_buffer_id()` which delegates
    /// here; handlers on `impl Editor` call `Editor::alloc_buffer_id()`
    /// which does the same plus advances the local `next_buffer_id`
    /// snapshot. Both routes produce the same monotonic id sequence.
    pub(crate) buffer_id_alloc: crate::app::window_resources::BufferIdAllocator,

    /// Configuration.
    ///
    /// Stored as `Arc<Config>` so that mutations go through `Arc::make_mut`
    /// (via `config_mut()`), which clone-on-writes when any other holder
    /// references the same value. `Arc<T>` has no `DerefMut`, so direct
    /// field assignment through `self.config` is a compile error — every
    /// mutation must route through the CoW-aware accessor.
    ///
    /// Effective value is `base_config_json` + `runtime_overlay` (design
    /// §3.4): init.ts and plugins may layer per-session writes via
    /// `editor.setSetting(path, value)`. The overlay is merged into
    /// `base_config_json`, the result is deserialised into this field,
    /// and mutations go through `Arc::make_mut`.
    ///
    /// **Freshness invariant**: `config_snapshot_anchor` below is set to
    /// `Arc::clone(&self.config)` on every plugin-snapshot refresh. That
    /// guarantees the first `Arc::make_mut(&mut self.config)` after each
    /// refresh *always* CoW-clones (strong count ≥ 2), so `self.config`
    /// moves to a new pointer and stops being `ptr_eq` with the anchor.
    config: Arc<Config>,

    /// Clone of `config` captured at the last plugin-snapshot refresh.
    config_snapshot_anchor: Arc<Config>,

    /// Serialized JSON of `*self.config` as of the last time
    /// `ptr_eq(&self.config, &self.config_snapshot_anchor)` was false.
    config_cached_json: Arc<serde_json::Value>,

    /// Cached raw user config (for plugins, avoids re-reading file on every frame).
    user_config_raw: Arc<serde_json::Value>,

    /// Directory context for editor state paths
    dir_context: DirectoryContext,

    /// Grammar registry for TextMate syntax highlighting
    grammar_registry: std::sync::Arc<crate::primitives::grammar::GrammarRegistry>,

    /// Pending grammars registered by plugins, waiting for reload_grammars() to apply
    pending_grammars: Vec<PendingGrammar>,

    /// Whether a grammar reload has been requested but not yet flushed.
    /// This allows batching multiple RegisterGrammar+ReloadGrammars sequences
    /// into a single rebuild.
    grammar_reload_pending: bool,

    /// Whether a background grammar build is in progress.
    /// When true, `flush_pending_grammars()` defers work until the build completes.
    grammar_build_in_progress: bool,

    /// Whether the initial full grammar build (user grammars + language packs)
    /// still needs to happen. Deferred from construction so that plugin-registered
    /// grammars from the first event-loop tick are included in a single build.
    needs_full_grammar_build: bool,

    /// Plugin callback IDs waiting for the grammar build to complete.
    /// Multiple reloadGrammars() calls may accumulate here; all are resolved
    /// when the background build finishes.
    pending_grammar_callbacks: Vec<fresh_core::api::JsCallbackId>,

    /// Active theme
    /// Active resolved theme, shared with windows via WindowResources.
    /// Wrapped in `Arc<RwLock<>>` so a theme reload propagates to every
    /// window without copying.
    pub(crate) theme: Arc<RwLock<crate::view::theme::Theme>>,

    /// All loaded themes (embedded + user). Held as `Arc` so
    /// `expanded_menus_cache` can detect a registry swap via `Arc::ptr_eq`.
    theme_registry: Arc<crate::view::theme::ThemeRegistry>,

    /// Memoised `MenuConfig` with `DynamicSubmenu` items expanded against
    /// the current theme registry.
    expanded_menus_cache: crate::view::ui::ExpandedMenusCache,

    /// Shared theme data cache for plugin access (name → JSON value)
    theme_cache: Arc<RwLock<HashMap<String, serde_json::Value>>>,

    /// Optional ANSI background image
    ansi_background: Option<crate::primitives::ansi_background::AnsiBackground>,

    /// Source path for the currently loaded ANSI background
    ansi_background_path: Option<PathBuf>,

    /// Blend amount for the ANSI background (0..1)
    background_fade: f32,

    /// Keybinding resolver (shared with Quick Open CommandProvider)
    keybindings: Arc<RwLock<KeybindingResolver>>,

    /// Shared clipboard (handles both internal and system clipboard)
    clipboard: crate::services::clipboard::Clipboard,

    /// Should the editor quit?
    should_quit: bool,

    /// Whether the workspace-trust prompt currently on screen was opened
    /// voluntarily (command palette) rather than as the mandatory open-time
    /// gate. When `true` its secondary action is "Cancel" (just close); when
    /// `false` it's "Quit" (exit the editor) and Escape is inert.
    workspace_trust_prompt_cancellable: bool,

    /// The marker files/dirs that triggered the workspace-trust prompt (e.g.
    /// `.envrc`, `.venv`, `App.sln`), captured when the prompt is shown so the
    /// dialog can tell the user why it appeared.
    workspace_trust_markers: Vec<String>,

    /// Scroll offset (in rows) for the workspace-trust dialog when it's too
    /// tall for the terminal. Driven by the mouse wheel; clamped in render.
    workspace_trust_scroll: u16,

    /// Should the client detach (keep server running)?
    should_detach: bool,

    /// Running in session/server mode (use hardware cursor only, no REVERSED style)
    session_mode: bool,

    /// Backend does not render a hardware cursor — always use software cursor indicators.
    software_cursor_only: bool,

    /// Session name for display in status bar (session mode only)
    session_name: Option<String>,

    /// Pending escape sequences to send to client (session mode only)
    /// These get prepended to the next render output
    pending_escape_sequences: Vec<u8>,

    /// If set, the editor should restart with this new working directory
    /// This is used by Open Folder to do a clean context switch
    restart_with_dir: Option<PathBuf>,

    // status_message, plugin_status_message, prompt moved onto
    // `Window` (Step 0k phase 3) — each window has its own chrome,
    // and the active window's chrome is what renders.
    /// Last terminal window title written via OSC 2. Used so we only write
    /// the escape sequence when the title would actually change, rather
    /// than on every frame.
    last_window_title: Option<String>,

    // `plugin_errors` moved onto `Window`.
    /// Terminal dimensions (for creating new buffers)
    terminal_width: u16,
    terminal_height: u16,

    /// Last layout signature the plugin `resize` hook fired for, used
    /// by `Editor::relayout` to dedupe notifications. The tuple is
    /// `(terminal_width, terminal_height, dock_cols, file_explorer_cols)`
    /// — the content geometry plugins observe. Deduping is what keeps
    /// the orchestrator's resize→`dock_width`→relayout reaction from
    /// looping: once the dock width settles, the signature stops
    /// changing and the hook stops re-firing. `None` until the first
    /// relayout.
    last_layout_signature: Option<(u16, u16, u16, u16)>,

    // LSP manager moved onto `Window`. Access via
    // `Editor::lsp()` / `lsp_mut()` — each window has its own
    // LspManager rooted at its project root.
    // buffer_metadata moved onto `Window` (Step 0l). Access via
    // `self.active_window().buffer_metadata` (or
    // `_mut()`) — each window owns the metadata for its own
    // buffers, alongside the buffer storage itself.
    /// Buffer mode registry (for buffer-local keybindings)
    mode_registry: ModeRegistry,

    /// Tokio runtime for async I/O tasks
    tokio_runtime: Option<Arc<tokio::runtime::Runtime>>,

    /// Bridge for async messages from tokio tasks to main loop
    async_bridge: Option<AsyncBridge>,

    /// Connection ids (`AgentChannel::id`) for which a reconnect-forwarder task
    /// has already been spawned. The forwarder awaits each channel's
    /// `reconnect_notify` and turns a hot-swap into an
    /// `AsyncMessage::RemoteReconnected`, so reconnect handling is event-driven
    /// rather than polled. `ensure_remote_reconnect_forwarders` registers one
    /// lazily per remote window; this set makes that idempotent.
    remote_reconnect_forwarders: std::collections::HashSet<u64>,

    /// Last-seen remote-connection state per window, so a per-tick poll can
    /// force a re-render when a link drops or comes back. The background
    /// reconnect task flips `is_remote_connected()` without any input event or
    /// (on a plain drop) async message, so without this poll the status-bar
    /// remote indicator would stay stale — showing "connected" after a drop, or
    /// "(Disconnected)" after the link settled back — until the user happens to
    /// press a key. Only windows with a remote authority are tracked.
    remote_connected_cache: HashMap<fresh_core::WindowId, bool>,

    /// In-flight async system-clipboard reads, keyed by `request_id`.
    /// Each entry owns an anchor (`VirtualText("▍")`) in some buffer
    /// that floats with edits via the marker tree; when the matching
    /// `ClipboardPasteResult` arrives, the pasted text is inserted at
    /// the anchor's *current* position (which may have moved since the
    /// user pressed Ctrl+V). The editor stays fully interactive while
    /// pastes are pending — multiple Ctrl+V presses simply add more
    /// entries, each capturing the OS clipboard contents at the moment
    /// its own background thread starts.
    pub(super) paste_pending: std::collections::HashMap<u64, crate::app::clipboard::PendingPaste>,

    /// Set by `paste()` when it took the slow placeholder path. The
    /// input dispatch reads and clears this to suppress the
    /// otherwise-automatic render for the Ctrl+V keystroke, and the
    /// main loop also reads `paste_render_suppress_until` (below)
    /// to actively veto frame rendering until that deadline.
    pub(super) paste_slow_path_just_armed: bool,

    /// While `Some` and the instant is in the future, the main loop
    /// holds off on rendering even when `needs_render` is set. Used
    /// by the async paste path: when arboard doesn't return inside
    /// the inline budget we plant a placeholder, but on a slow
    /// renderer (LSP-active, remote terminal, dense diagnostics)
    /// each `terminal.draw` is hundreds of ms, so rendering the
    /// placeholder before the paste itself resolves *doubles* the
    /// user-visible latency. Setting this suppresses renders until
    /// the paste deadline; the paste resolve clears it and triggers
    /// a single render with the final text.
    pub(super) paste_render_suppress_until: Option<std::time::Instant>,

    /// Test-only override for the system-clipboard reader used by the
    /// async paste path. `None` in production (the real
    /// `services::clipboard::read_system_clipboard` is used). Tests set
    /// this to a deterministic stub — e.g. `|| None` to simulate a
    /// host with no usable system clipboard (Termux, headless TTY) —
    /// so the internal-clipboard fallback can be exercised in isolation
    /// without touching the real host clipboard.
    pub(super) system_clipboard_reader: Option<fn() -> Option<String>>,

    // split_manager and split_view_states moved onto `Window`. Access
    // via `Editor::split_manager()` / `split_manager_mut()` and
    // `Editor::split_view_states()` / `split_view_states_mut()`.
    // Each window owns its own split tree + per-leaf view state.
    // `previous_viewports` moved onto `Window` (per-leaf state — each
    // window has its own splits, so its own viewport-change tracker).
    // `scroll_sync_manager` moved onto `Window` — pairs splits, which
    // are per-window.

    // file_explorer moved onto `Window`. Access via
    // `Editor::file_explorer()` / `file_explorer_mut()` —
    // each window has its own tree view.
    // `preview` (per-window preview-tab tracker) moved onto `Window`.
    // Each window has its own preview slot.

    // suppress_position_history_once moved onto `Window` (Step 0f).
    // The file explorer and Open File browser ride per-window fs_managers
    // (`WindowResources::fs_manager`, derived from each window's authority), so
    // the editor no longer holds a global one that could go stale against the
    // active authority.
    // The editor's active backend is *not* a field here — it lives on the
    // active `Window` (owned, non-`Clone`), read via `Editor::authority()`.
    // Keeping it single-owned per window is what makes a session's
    // backend/trust/env impossible to share into another window by
    // construction (issue #2280).
    /// Authority queued by `install_authority`, picked up by `main.rs`
    /// right before dropping this editor on restart. `None` in the
    /// steady state. Not durable state — restarts from `main.rs`'s
    /// restart-dir path leave this `None`, and the main loop carries
    /// the authority over through its own channel.
    pending_authority: Option<crate::services::authority::Authority>,

    /// Keepalive bundle queued alongside `pending_authority` for a
    /// connection-backed authority (remote agent / K8s), parked by the
    /// restart loop so the live carrier + reconnect/heartbeat tasks
    /// survive the rebuild. `None` for synchronously-constructible
    /// authorities (local, docker). See
    /// [`Editor::install_authority_with_keepalive`].
    pending_keepalive: Option<Box<dyn std::any::Any + Send>>,

    /// Plugin-supplied override for the Remote Indicator. Takes
    /// precedence over the authority-derived state at render time.
    /// Cleared on editor restart (plugins must reassert the state
    /// after `setAuthority`). See
    /// `PluginCommand::SetRemoteIndicatorState`.
    pub remote_indicator_override: Option<crate::view::ui::status_bar::RemoteIndicatorOverride>,

    /// Local filesystem for editor-internal files (log files, status
    /// log). Stays separate from `authority` because these are the
    /// editor's own private state — they live on the host disk
    /// regardless of where the user is editing.
    local_filesystem: Arc<dyn FileSystem + Send + Sync>,

    // `file_explorer_visible`, `file_explorer_sync_in_progress`,
    // `file_explorer_width`, `file_explorer_side`,
    // `pending_file_explorer_show_*`, `file_explorer_decorations`,
    // `file_explorer_decoration_cache` all moved onto `Window`. The
    // file-explorer view itself was already per-window since Step 0b;
    // these chrome flags follow.
    /// File explorer clipboard for cut/copy/paste of files and directories
    // `file_explorer_clipboard` moved onto `Window`.

    // `menu_bar_visible`, `menu_bar_auto_shown`, `tab_bar_visible`,
    // `status_bar_visible`, `prompt_line_visible`, `mouse_enabled`
    // moved onto `Window` — per-window UI toggles.

    // `same_buffer_scroll_sync` moved onto `Window` — per-window UX
    // toggle, since the split tree it controls is per-window.
    /// Mouse cursor position (for GPM software cursor rendering)
    /// When GPM is active, we need to draw our own cursor since GPM can't
    /// draw on the alternate screen buffer used by TUI applications.
    // `mouse_cursor_position`, `gpm_active`, `key_context` moved onto `Window`.

    /// Menu state (active menu, highlighted item)
    menu_state: crate::view::ui::MenuState,

    /// Menu configuration (built-in menus with i18n support)
    menus: crate::config::MenuConfig,

    /// All editor sessions, keyed by id. The active window's `root` is
    /// the editor's working directory — see `working_dir()`, which
    /// derives it. There is deliberately no separate `working_dir`
    /// field: it cannot drift out of sync with the active window
    /// (issue #2056). Holds exactly one session (`WindowId(1)`, the
    /// "base") until the orchestrator adds more.
    pub(crate) windows: HashMap<fresh_core::WindowId, crate::app::window::Window>,

    /// Connection keepalives for born-attached remote windows, keyed by
    /// `WindowId`. A remote (Kubernetes / SSH / …) window's carrier process +
    /// reconnect/heartbeat tasks + dedicated runtime live in this opaque
    /// bundle; it must outlive the `Editor` rebuilds that *don't* drop the
    /// window and is torn down when the window is closed (`close_window`).
    /// Local windows have no entry. This is the per-window analogue of the
    /// process-level keepalive the restart-based attach parks.
    pub(crate) session_keepalives: HashMap<fresh_core::WindowId, Box<dyn std::any::Any + Send>>,

    /// Request ids of `attachRemoteAgent` connects currently in flight (added
    /// when the connect is spawned, removed when it settles). Lets a plugin
    /// cancel a pending connect (the New-Session dialog's Cancel).
    pub(crate) remote_attach_inflight: std::collections::HashSet<u64>,
    /// Request ids of in-flight attaches the plugin asked to cancel. When the
    /// connect later resolves, the result (authority + carrier keepalive) is
    /// dropped instead of installed — so no window is created and the carrier
    /// is torn down — and a failure is ignored.
    pub(crate) remote_attach_cancelled: std::collections::HashSet<u64>,
    /// Cancellation senders for the background connect threads, keyed by
    /// request id. The connect runs on a detached thread (so the carrier's
    /// runtime outlives it); signalling here makes that thread's `select!` drop
    /// the in-flight connect future, which drops the ssh child (spawned
    /// kill-on-drop) — so even a hung handshake leaves no orphaned process. The
    /// thread then finishes and its result is discarded. Cleared on settle.
    pub(crate) remote_attach_cancels:
        std::collections::HashMap<u64, tokio::sync::oneshot::Sender<()>>,

    /// Id of the currently active session. Always `WindowId(1)` for
    /// now; multi-session support arrives in a follow-up commit.
    pub(crate) active_window: fresh_core::WindowId,

    /// Windows whose persisted workspace (files/splits) has not yet
    /// been restored from disk. Startup materializes only the
    /// foreground (CLI-dir) window eagerly; every other discovered
    /// session is seeded with an empty layout and listed here, then
    /// restored lazily on first dive or preview via
    /// `materialize_window`.
    pub(crate) materialize_pending: std::collections::HashSet<fresh_core::WindowId>,

    /// Persisted **remote** sessions (SSH / kube) discovered at boot but not
    /// yet connected — there is deliberately **no `Window`** for them, because a
    /// `Window` must always own its session's *real* authority and a remote
    /// session's authority does not exist until it connects. Representing them
    /// as `Window`s would force a dummy local-placeholder authority (the old
    /// "shell"), which is exactly the "local before, remote later" pattern that
    /// silently ran restored terminals on the local host. Instead they live
    /// here as authority-less descriptors: listed in the dock via the
    /// `WindowInfo` snapshot, and promoted to a real `Window` (born with the
    /// connected SSH/kube authority, restoring their workspace through it) only
    /// when the user dives in — see `bring_dormant_remote_online`. Keyed by the
    /// id they will adopt as a `Window`.
    pub(crate) dormant_remote: std::collections::HashMap<
        fresh_core::WindowId,
        crate::app::orchestrator_persistence::PersistedWindow,
    >,

    /// Monotonic counter for the next session id. The base session
    /// uses 1; new sessions take 2, 3, …. Closing a session does
    /// not free its id (per design, ids are stable within a process).
    /// Unused until `createWindow` lands in the next migration step.
    #[allow(dead_code)]
    pub(crate) next_window_id: u64,

    /// Optional plugin-provided order/subset for the Next/Prev Window
    /// commands. When `Some`, those commands cycle through exactly these
    /// window ids in this order (ids no longer open are skipped); when
    /// `None`, the default (every window, by id) applies. Set by the
    /// orchestrator dock so paging windows visits exactly the sessions
    /// the dock currently shows. See `cycle_active_window`.
    pub(crate) window_cycle_order: Option<Vec<fresh_core::WindowId>>,

    // LSP request-tracking state (next_lsp_request_id,
    // pending_*_requests, *_in_flight, completion_items,
    // dabbrev_state, etc.) all moved onto `Window` in Step 0k.
    // `completion_service` moved onto `Window` — orchestrates this
    // window's per-window completion providers (dabbrev, buffer words,
    // LSP, plugin providers).
    // `lsp_diagnostic_namespace` moved onto `Window` — overlay
    // namespace key for diagnostic overlays, which are buffer overlays
    // and follow buffers onto the window.
    // `interactive_replace_state` moved onto `Window` — per-window
    // search-and-replace session state.
    // `mouse_state` moved onto `Window` — drag targets reference per-window LeafIds.
    // `tab_context_menu`, `file_explorer_context_menu`, `theme_info_popup`
    // moved onto `Window`.
    // `chrome_layout` moved onto `Window` — each window has its own
    // status bar, menu, prompt overlay, and popups.
    /// Command registry for dynamic commands
    command_registry: Arc<RwLock<CommandRegistry>>,

    /// Quick Open registry for unified prompt providers
    quick_open_registry: QuickOpenRegistry,

    /// URI schemes a plugin has claimed via `registerLspUriScheme`. When an
    /// LSP navigation (e.g. go-to-definition) resolves to a non-`file://`
    /// URI whose scheme is in this set, the core fires the
    /// `lsp_open_external_uri` hook and lets the plugin fetch/open the
    /// target instead of showing the "external location" fallback message.
    /// Keeps synthetic-document handling (slangd's `slang-synth://`, jdtls's
    /// `jdt://`, …) out of the core: the core only dispatches by scheme.
    pub(crate) lsp_uri_schemes: std::collections::HashSet<String>,

    /// Plugin manager (handles both enabled and disabled cases)
    /// Plugin manager, wrapped in `Arc<RwLock<>>` so windows can fire
    /// hooks (`run_hook`) via WindowResources without holding an
    /// `&mut Editor` reference. `&self` methods (run_hook,
    /// deliver_response, has_hook_handlers, …) take a read lock; the
    /// few `&mut self` methods (process_commands, check_thread_health,
    /// test_inject_command) take a write lock.
    plugin_manager: std::rc::Rc<RwLock<PluginManager>>,

    // `plugin_dev_workspaces` moved onto `Window` — keyed by `BufferId`,
    // and buffers are per-window, so the workspace map follows.
    /// Registry of status-bar tokens contributed by plugins.
    /// Key: "plugin_name:token_name" (e.g., "git_statusbar:branch"); value: display title.
    /// Global and lifetime-checked. Per-buffer values live on each `Window`.
    status_bar_token_registry: Mutex<HashMap<String, String>>,

    /// Registry of plugin-provided config schemas, keyed by plugin name.
    /// Each value is the validated JSON schema describing
    /// `plugins.<name>.settings.*`. Populated at startup from
    /// `<plugin_name>.schema.json` sidecar files discovered next to plugin
    /// `.ts`/`.js` files; the Settings UI reads this to render a
    /// per-plugin sub-category under "Plugin Settings".
    pub(crate) plugin_schemas:
        std::sync::Arc<std::sync::RwLock<HashMap<String, serde_json::Value>>>,

    // `seen_byte_ranges` moved onto `Window` — keyed by `BufferId`
    // which lives on `Window`, so the tracker follows the buffers.

    // panel_ids moved onto `Window`. Access via
    // `Editor::panel_ids()` / `panel_ids_mut()` — those resolve to
    // the active window's dock occupancy. Each window owns its own
    // utility-dock; switching windows doesn't share dock state.
    // `buffer_groups`, `buffer_to_group`, `next_buffer_group_id`
    // moved onto `Window` — each window has its own buffer groups.

    // grouped_subtrees moved onto `Window` — each window owns its
    // own buffer-group subtrees (a window with a Live Grep panel
    // open doesn't share the panel state with sibling windows).
    /// Background process abort handles for cancellation
    /// Maps process_id to abort handle
    background_process_handles: HashMap<u64, tokio::task::AbortHandle>,

    /// Cancellation senders for host-side processes spawned via
    /// `spawnHostProcess`. Firing the sender (or dropping it) triggers
    /// an in-task `child.start_kill()` so the process is reaped, not
    /// just orphaned. Entries are removed when the spawn task sends
    /// its terminal `PluginProcessOutput`.
    host_process_handles: HashMap<u64, tokio::sync::oneshot::Sender<()>>,
    /// FIFO queue of plugin `editor.getNextKey()` callbacks awaiting a
    /// keypress. While non-empty, the next key arriving in
    /// `handle_key` is consumed by resolving the front-most callback
    /// rather than dispatching to mode bindings or other handlers.
    // `pending_next_key_callbacks`, `key_capture_active`,
    // `pending_key_capture_buffer` moved onto `Window`.
    // `lsp_progress`, `lsp_server_statuses`, `lsp_window_messages`,
    // `lsp_log_messages` moved onto `Window` — each window has its own
    // LspManager, so the progress/status/message streams describe that
    // manager's servers, not the editor's.

    // `diagnostic_result_ids` moved onto `Window` — each window has its
    // own LspManager and therefore its own per-URI result_id stream.
    // `stored_push_diagnostics`, `stored_pull_diagnostics`,
    // `stored_diagnostics`, `stored_folding_ranges` moved onto
    // `Window` — URI-keyed but each URI maps to a buffer in a specific
    // window's LSP set, so the maps describe one window's diagnostics.
    /// Event broadcaster for control events (observable by external systems)
    event_broadcaster: crate::model::control_event::EventBroadcaster,

    // bookmarks moved onto `Window` (Step 0f).
    /// Macro record/playback subsystem (owns `macros`, `recording`,
    /// `last_register`, and the `playing` guard flag).
    // `macros` moved onto `Window`.

    /// Pending plugin action receivers (for async action execution)
    #[cfg(feature = "plugins")]
    pending_plugin_actions: Vec<(
        String,
        crate::services::plugins::thread::oneshot::Receiver<anyhow::Result<()>>,
    )>,

    /// Flag set by plugin commands that need a render (e.g., RefreshLines)
    #[cfg(feature = "plugins")]
    plugin_render_requested: bool,

    /// Pending chord sequence for multi-key bindings (e.g., C-x C-s in Emacs)
    /// Stores the keys pressed so far in a chord sequence
    // `chord_state` moved onto `Window`.

    // (Historical `pending_lsp_confirmation` and `pending_lsp_status_popup`
    // fields moved onto `Popup::resolver` — each popup carries its own
    // "how do I confirm?" identity, so `handle_popup_confirm` dispatches
    // by matching the focused popup's resolver instead of racing through
    // a precedence cascade of side-channel `Option`s that a second
    // simultaneously-open popup could steal.)
    /// Languages the user has interactively dismissed from the LSP popup.
    ///
    /// Pending Save-As queue for the "save and quit" flow.
    ///
    // `last_auto_revert_poll`, `last_file_tree_poll`, `git_index_resolved`,
    // `dir_mod_times`, `pending_file_poll_rx`, `pending_dir_poll_rx`
    // all moved onto `Window` — auto-revert and file-tree change
    // detection are per-window, paired with the already-per-window
    // `file_mod_times`.
    /// File open dialog state (when PromptType::OpenFile is active)
    // `file_open_state` moved onto `Window`.

    /// Cached layout for file browser (for mouse hit testing)
    // `file_browser_layout` moved onto `Window`.

    /// Recovery service for auto-recovery-save and crash recovery.
    /// `Arc<Mutex>` because it is shared into every `Window` via
    /// `WindowResources` so per-window restore / auto-save can reach it
    /// without an active-window flip.
    recovery_service: std::sync::Arc<std::sync::Mutex<RecoveryService>>,

    /// Request a full terminal clear and redraw on the next frame
    full_redraw_requested: bool,

    /// When true, the render pipeline computes chrome *layout* (menu, dropdown,
    /// command palette / suggestions) and records it on the layout caches as
    /// usual, but SKIPS drawing those chrome layers into the cell buffer. Hosts
    /// that render chrome from the semantic model instead of cells (the web /
    /// Tauri frontends) set this so they get pane-only cells with no chrome to
    /// hide. The TUI/GUI leave it `false` and draw chrome to cells as before.
    /// See docs/internal/web-ui.md.
    pub(crate) suppress_chrome_cells: bool,

    /// Request the event loop to suspend the process (SIGTSTP on Unix).
    /// Consumed by the outer event loop after the current action returns.
    suspend_requested: bool,

    /// Time source for testable time operations
    time_source: SharedTimeSource,

    /// Last auto-recovery-save time for rate limiting
    // `last_auto_recovery_save`, `last_persistent_auto_save` moved onto `Window`.

    /// Active custom contexts for command visibility
    /// Plugin-defined contexts like "config-editor" that control command availability
    // `active_custom_contexts` moved onto `Window`.

    /// Plugin-managed global state, isolated per plugin name.
    /// Outer key is plugin name, inner key is the state key set by the plugin.
    plugin_global_state: HashMap<String, HashMap<String, serde_json::Value>>,
    /// Warning log receiver and path (for tracking warnings)
    warning_log: Option<(std::sync::mpsc::Receiver<()>, PathBuf)>,

    /// Status message log path (for viewing full status history)
    status_log_path: Option<PathBuf>,

    /// Warning domain registry for extensible warning indicators
    /// Contains LSP warnings, general warnings, and can be extended by plugins
    // `warning_domains` moved onto `Window`.

    /// Periodic update checker (checks for new releases every hour)
    update_checker: Option<crate::services::release_checker::PeriodicUpdateChecker>,

    // Terminal subsystem moved onto `Window` (Step 0d). PTYs and
    // their backing files belong to the window that spawned them, so
    // closeWindow joins the threads. Access through methods on Window
    // (called via `self.windows.get_mut(&id).unwrap().method(...)`),
    // not via accessors on Editor.
    /// Plugin-driven filesystem watchers (lazily constructed —
    /// the underlying notify backend spawns a thread, so it's
    /// nicer to defer until the first `watchPath` call). See
    /// `services/file_watcher.rs`.
    #[cfg(feature = "plugins")]
    file_watcher_manager: crate::services::file_watcher::FileWatcherManager,

    /// Test-only log of `path_changed` plugin events. Captured by
    /// `async_dispatch` whenever a PathChanged AsyncMessage arrives,
    /// so e2e tests can assert filesystem events reached the editor
    /// without standing up a JS plugin. Production builds never read
    /// this.
    pub(crate) path_changes_for_test: Vec<(u64, std::path::PathBuf, &'static str)>,

    /// Test-only sink for the most-recent `WatchPathRegistered`
    /// plugin response, keyed by request_id. Used by
    /// `watch_path` e2e tests to read back the allocated handle.
    pub(crate) last_watch_response_for_test: Option<(u64, Result<u64, String>)>,

    /// Plugin-driven session preview override. When `Some(sid)`
    /// and the floating-overlay prompt is open, the overlay's
    /// preview pane renders the *entire* split tree of session
    /// `sid` natively — Primitive #1 in
    /// `docs/internal/orchestrator-sessions-design.md` §
    /// "Rich Control Room rendering".
    pub(crate) preview_window_id: Option<fresh_core::WindowId>,

    // terminal_buffers / terminal_backing_files / terminal_log_files
    // moved onto `Window` (Step 0d).
    // `ephemeral_terminals` moved onto `Window` — TerminalManager and
    // its terminals are per-window (Step 0d), so the ephemeral set
    // follows.

    // `terminal_mode` moved onto `Window`. Per-window because each
    // window has its own active terminal buffer.
    /// Whether keyboard capture is enabled in terminal mode.
    /// When true, ALL keys go to the terminal (except Ctrl+` to toggle).
    /// When false, UI keybindings (split nav, palette, etc.) are processed first.
    // `keyboard_capture` moved onto `Window`.

    // A terminal buffer's remembered live/scrollback interaction mode is
    // part of its per-window `Window::terminal_buffers` record
    // (`TerminalBuffer::mode`).
    /// Timestamp of the previous mouse click (for multi-click detection)
    // `previous_click_time`, `previous_click_position`, `click_count` moved onto `Window`.

    /// Settings UI state (when settings modal is open)
    pub(crate) settings_state: Option<crate::view::settings::SettingsState>,

    /// Calibration wizard state (when calibration modal is open)
    pub(crate) calibration_wizard: Option<calibration_wizard::CalibrationWizard>,

    // `event_debug` modal state moved to `Window` — each window has its
    // own debug overlay (the dialog records keystrokes destined for that
    // window's input pipeline, so it's logically per-window).
    /// Keybinding editor state (when keybinding editor modal is open)
    pub(crate) keybinding_editor: Option<keybinding_editor::KeybindingEditor>,

    /// Key translator for input calibration (loaded from config)
    pub(crate) key_translator: crate::input::key_translator::KeyTranslator,

    /// Terminal color capability (true color, 256, or 16 colors)
    color_capability: crate::view::color_support::ColorCapability,

    /// Hunks for the Review Diff tool
    // `review_hunks` moved onto `Window`.

    /// Editor-level popups that float above any buffer regardless of which
    /// one is active. Plugin notifications (showActionPopup) live here so a
    /// switch to a virtual buffer (Dashboard, diagnostics panel, …) doesn't
    /// hide them mid-decision.
    ///
    /// Each plugin popup carries its `popup_id` inside its
    /// `PopupResolver::PluginAction` — no parallel side-channel stack.
    pub(crate) global_popups: crate::view::popup::PopupManager,

    // composite_buffers + composite_view_states moved onto `Window` —
    // composite-buffer panels (Live Grep results, Diagnostics list,
    // References, etc.) belong to the window that opened the panel.
    /// Pending file opens from CLI arguments (processed after TUI starts)
    /// This allows CLI files to go through the same code path as interactive file opens,
    /// ensuring consistent error handling (e.g., encoding confirmation prompts).
    // `pending_file_opens` moved onto `Window`.

    /// When true, apply hot exit recovery after the next batch of pending file opens
    // `pending_hot_exit_recovery` moved onto `Window`.

    /// Tracks buffers opened with --wait: maps buffer_id → (wait_id, has_popup)
    // `wait_tracking` moved onto `Window`.
    /// Wait IDs that have completed (buffer closed or popup dismissed)
    // `completed_waits` moved onto `Window`.

    /// Stdin streaming state (if reading from stdin)
    stdin_stream: stdin_stream::StdinStream,

    /// Incremental line scan state (for non-blocking progress during Go to Line)
    // `line_scan` moved onto `Window`.

    /// Incremental search scan state (for non-blocking search on large files)
    // `search_scan` moved onto `Window`.

    /// Viewport top_byte when search overlays were last refreshed.
    /// Used to detect viewport scrolling so overlays can be updated.
    // `search_overlay_top_byte` moved onto `Window`.

    /// Frame-buffer animation layer. Applied at the end of `render`; the
    /// main loop consults `is_active`/`next_deadline` to keep re-rendering
    /// while animations are running.
    // `animations` moved onto `Window`.

    /// Hardware-cursor screen position from the previous render pass, paired
    /// with the active split that owned the cursor at that time. Used to
    /// detect "jumps" (search, goto-line, click, goto-definition, focus
    /// change between splits, tab/buffer switch, etc.) and animate the
    /// cursor moving from its old screen position to its new one. Cross-
    /// pane jumps animate unconditionally; same-pane jumps animate when
    /// the cursor moved more than two rows or at least ten columns.
    pub(crate) previous_cursor_screen_pos: Option<((u16, u16), LeafId)>,
    /// ID of the most recent cursor-jump animation, kept so successive jumps
    /// cancel the prior one instead of stacking trail effects.
    pub(crate) cursor_jump_animation: Option<crate::view::animation::AnimationId>,

    /// Deferred plugin animations targeting a virtual buffer whose
    /// on-screen Rect wasn't in the cached split layout at command
    /// dispatch time. Drained at the top of each render pass once
    /// `split_areas` has been recomputed, so the animation starts on
    /// the very first frame the buffer actually occupies screen space.
    pub(crate) pending_vb_animations: Vec<(u64, BufferId, fresh_core::api::PluginAnimationKind)>,

    /// Plugin widget panels mounted via `MountWidgetPanel`.
    ///
    /// One entry per active panel. The registry holds the most recent
    /// `WidgetSpec` per panel so future updates can reconcile against
    /// it and so a theme change can re-render every panel without
    /// the originating plugin needing to re-emit. See
    /// `docs/internal/plugin-widget-library-design.md`.
    pub(crate) widget_registry: crate::widgets::WidgetRegistry,

    /// Currently-mounted floating widget panel, if any.
    ///
    /// At most one floating widget panel is shown at a time — the
    /// overlay is modal-ish (input is routed to it) and stacking
    /// floaters would obscure each other without a usable focus
    /// model. Mounting a second one replaces the first.
    ///
    /// This is the **centered modal** slot (`PanelSlot::Floating`): the
    /// orchestrator picker, the new-session form, and other plugin
    /// modals. The persistent left dock lives in a separate slot
    /// (`dock`) so the two can coexist (a modal opens *over* the editor
    /// while the dock stays visible). Routing is by `PanelSlot`.
    pub(crate) floating_widget_panel: Option<FloatingWidgetState>,

    /// The editor-global left **dock** panel (`PanelSlot::Dock`), if
    /// shown. Independent of `floating_widget_panel` so the dock persists
    /// while a centered modal is open. Always rendered as a `LeftDock`.
    pub(crate) dock: Option<FloatingWidgetState>,

    /// Persisted width (columns) of the orchestrator left dock after the
    /// user drags its right border. `None` until first resized; when set,
    /// `FloatingPanelControl{op:"dock"}` restores this instead of the
    /// plugin's default so the width survives toggling the dock off/on.
    pub(crate) dock_width: Option<u16>,
    /// True while the user is dragging the dock's right border to resize.
    pub(crate) dock_resizing: bool,
}

/// Sentinel `BufferId` registered with the widget registry for the
/// floating panel — never appears in the editor's buffer table, so
/// `set_virtual_buffer_content` against it would fail. The mount /
/// rerender paths special-case this id and paint into the overlay
/// rect instead.
pub(crate) const FLOATING_PANEL_BUFFER_ID: BufferId = BufferId(usize::MAX);

/// Sentinel `BufferId` for the editor-global **dock** panel — distinct
/// from `FLOATING_PANEL_BUFFER_ID` so the dock and a centered modal can
/// both live in the widget registry (and be hit-tested) at the same
/// time. See `Editor::dock` and `PanelSlot`.
pub(crate) const DOCK_PANEL_BUFFER_ID: BufferId = BufferId(usize::MAX - 1);

/// Selects which of the two coexisting widget-panel slots an operation
/// targets: the centered modal overlay (`Floating`, the picker /
/// new-session form / plugin modals) or the persistent editor-global
/// left `Dock`. They occupy disjoint screen regions and render together.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PanelSlot {
    Floating,
    Dock,
}

impl PanelSlot {
    pub(crate) fn buffer_id(self) -> BufferId {
        match self {
            PanelSlot::Floating => FLOATING_PANEL_BUFFER_ID,
            PanelSlot::Dock => DOCK_PANEL_BUFFER_ID,
        }
    }
}

/// A widget panel rendered as a centered floating overlay rather
/// than into a virtual buffer. The panel still lives in the shared
/// `widget_registry` (so the existing reconcile / widget-command /
/// mutate paths apply), but its rendered entries are painted into
/// the overlay rect at draw time instead of being written into a
/// virtual buffer.
/// Where a floating widget panel is anchored on screen.
//
// The variants are only *constructed* by the plugin-gated floating-panel
// handlers, but they are matched throughout the shared widget runtime and
// render/input code, so the enum itself stays un-gated. Suppress the
// "never constructed" lint in plugin-less builds.
#[cfg_attr(not(any(feature = "plugins", test)), allow(dead_code))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PanelPlacement {
    /// Centered modal overlay sized by `width_pct`/`height_pct`
    /// (the historical default; dims the background, captures keys).
    Centered,
    /// Full-height column pinned to the left of the entire editor
    /// chrome (left of the menu bar, splits, and status bar). The
    /// chrome is laid out in the remaining width; no background
    /// dimming. Non-modal — see `FloatingWidgetState::focused`.
    LeftDock { width_cols: u16 },
    /// Content-sized popup anchored near a screen cell — a right-click
    /// context menu. Drawn at `(x, y)` (clamped to stay fully on
    /// screen), sized to its rendered content, with **no** background
    /// dimming, so it reads as an unobtrusive popup rather than a modal.
    /// Still input-modal via the `FloatingModal` layer: keys route to it
    /// and a click outside dismisses it.
    Anchored { x: u16, y: u16 },
}

#[derive(Debug, Clone)]
pub(crate) struct FloatingWidgetState {
    /// Composite (plugin, id) identity of the mounted panel.
    pub panel_key: crate::widgets::PanelKey,
    pub width_pct: u8,
    pub height_pct: u8,
    /// On-screen anchor. Defaults to `Centered` on mount; a plugin
    /// re-anchors to a dock via `FloatingPanelControl{op:"dock"}`.
    pub placement: PanelPlacement,
    /// Whether keys route to this panel. Always true for `Centered`
    /// (modal). For `LeftDock` a plugin toggles this via
    /// `FloatingPanelControl{op:"focus"|"blur"}` so the editor
    /// underneath stays keyboard-usable while the dock is visible.
    pub focused: bool,
    /// Most-recently rendered entries. Refreshed on every spec /
    /// command / mutate; painted into the overlay rect at draw
    /// time.
    pub entries: Vec<fresh_core::text_property::TextPropertyEntry>,
    /// Hardware-cursor target when a `TextInput` is focused.
    pub focus_cursor: Option<crate::widgets::FocusCursor>,
    /// Window-embed rectangles reserved by `WindowEmbed`
    /// widgets in the panel's spec. After the entries paint
    /// down their (blank) cells, the floating panel render
    /// walks these and invokes the per-window paint path
    /// scoped to each rect — giving us a live render of the
    /// referenced editor window inside the floating overlay.
    pub embeds: Vec<crate::widgets::EmbedRect>,
    /// Rows produced by `WidgetSpec::Overlay` children. Painted
    /// AFTER `entries` and `embeds`, on top of whatever's at
    /// each `buffer_row`. Used for dropdown completions /
    /// transient popups that should appear next to a focused
    /// widget without reflowing the rest of the panel when
    /// they show or hide.
    pub overlays: Vec<crate::widgets::OverlayRow>,
    /// Scrollable `List` widgets that overflowed, with the geometry
    /// the draw pass uses to paint a scrollbar. Refreshed on every
    /// render alongside `entries`/`embeds`.
    pub scroll_regions: Vec<crate::widgets::ScrollRegion>,
    /// Screen-space scrollbar tracks computed at the last draw — used
    /// by the mouse hit-test to start/continue a scrollbar drag. One
    /// per overflowing list.
    pub scrollbar_tracks: Vec<WidgetScrollbarTrack>,
    /// Shared press/drag/release state for the panel's list
    /// scrollbars (the canonical `ScrollbarMouse`).
    pub scrollbar_mouse: crate::view::ui::scrollbar::ScrollbarMouse,
    /// `list_key` of the scrollbar currently being drag-scrolled.
    pub scrollbar_drag_key: Option<String>,
    /// Inner rect (frame interior) of the last draw — used by the
    /// click hit-test to map terminal coords back to buffer coords.
    pub last_inner_rect: Option<ratatui::layout::Rect>,
    /// Screen-space regions (one per overflowing list) over which the
    /// pointer reveals that list's overlay scrollbar. Refreshed every
    /// draw alongside `scrollbar_tracks`; empty when no list overflows.
    /// Only the dock populates this — its scrollbars are hover-revealed.
    pub scrollbar_hover_zones: Vec<ratatui::layout::Rect>,
    /// Whether the pointer was last seen inside a `scrollbar_hover_zone`.
    /// Memoised so the mouse-move handler only forces a re-render on the
    /// enter/leave transition rather than on every motion event.
    pub scrollbar_zone_hovered: bool,
    /// When true, a `Centered` panel renders over the *entire* frame
    /// (covering the dimmed left dock) instead of being confined to the
    /// chrome area beside the dock. Set via `FloatingPanelControl{op:
    /// "fullscreen"}`. Lets the orchestrator's global modals (the control
    /// room, the New-Session form) take the full screen over their own
    /// dock, while other plugins' floating panels keep the default
    /// coexist-beside-the-dock layout. Ignored for `LeftDock`.
    pub fullscreen: bool,
    /// When true, this panel renders through `render_spec_with_marker`:
    /// every focusable control reserves a two-column gutter for the
    /// `▸ ` focus marker so focus is legible from a plain capture and
    /// the layout stays constant as focus moves. Opt-in at mount
    /// (`MountFloatingWidget.focus_marker`); the Orchestrator New
    /// Session form uses it.
    pub focus_marker: bool,
}

/// A list scrollbar's screen rect + scroll state, captured at draw
/// time so mouse press/drag can hit-test and drive `ScrollbarMouse`.
#[derive(Debug, Clone)]
pub(crate) struct WidgetScrollbarTrack {
    pub list_key: String,
    pub rect: ratatui::layout::Rect,
    pub total: usize,
    pub visible: usize,
    pub scroll: usize,
}

/// A file that should be opened after the TUI starts
#[derive(Debug, Clone)]
pub struct PendingFileOpen {
    /// Path to the file
    pub path: PathBuf,
    /// Line number to navigate to (1-indexed, optional)
    pub line: Option<usize>,
    /// Column number to navigate to (1-indexed, optional)
    pub column: Option<usize>,
    /// End line for range selection (1-indexed, optional)
    pub end_line: Option<usize>,
    /// End column for range selection (1-indexed, optional)
    pub end_column: Option<usize>,
    /// Hover popup message to show after opening (optional)
    pub message: Option<String>,
    /// Wait ID for --wait tracking (if the CLI is blocking until done)
    pub wait_id: Option<u64>,
}

impl Editor {
    /// Load an ANSI background image from a user-provided path
    fn load_ansi_background(&mut self, input: &str) -> AnyhowResult<()> {
        let trimmed = input.trim();

        if trimmed.is_empty() {
            self.ansi_background = None;
            self.ansi_background_path = None;
            self.set_status_message(t!("status.background_cleared").to_string());
            return Ok(());
        }

        let input_path = Path::new(trimmed);
        let resolved = if input_path.is_absolute() {
            input_path.to_path_buf()
        } else {
            self.working_dir().join(input_path)
        };

        let canonical = resolved.canonicalize().unwrap_or_else(|_| resolved.clone());

        let parsed = crate::primitives::ansi_background::AnsiBackground::from_file(&canonical)?;

        self.ansi_background = Some(parsed);
        self.ansi_background_path = Some(canonical.clone());
        self.set_status_message(
            t!(
                "view.background_set",
                path = canonical.display().to_string()
            )
            .to_string(),
        );

        Ok(())
    }

    /// Total number of open buffers across the workspace. Test
    /// support for `EditorTestApi::buffer_count` (Phase 7 of the
    /// scenario migration).
    #[doc(hidden)]
    pub fn buffer_count_for_tests(&self) -> usize {
        self.windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .len()
    }

    /// Buffer IDs in stable order (sorted by inner value). Used by
    /// `EditorTestApi::buffer_paths` so workspace assertions don't
    /// depend on `HashMap` iteration order.
    #[doc(hidden)]
    pub fn all_buffer_ids_for_tests(&self) -> Vec<BufferId> {
        let mut ids: Vec<BufferId> = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .ids();
        ids.sort_by_key(|id| id.0);
        ids
    }

    /// Get the currently active buffer state
    pub fn active_state(&self) -> &EditorState {
        self.windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&self.active_buffer())
            .unwrap()
    }

    /// Get the currently active buffer state (mutable)
    pub fn active_state_mut(&mut self) -> &mut EditorState {
        let __buffer_id = self.active_buffer();
        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&__buffer_id)
            .unwrap()
    }

    /// Get the cursors for the active buffer in the active split.
    /// Uses `effective_active_split` so focused buffer-group panels return
    /// their own cursors (not the outer split's stale ones).
    pub fn active_cursors(&self) -> &Cursors {
        let split_id = self.effective_active_split();
        &self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .unwrap()
            .cursors
    }

    /// Get the cursors for the active buffer in the active split (mutable)
    pub fn active_cursors_mut(&mut self) -> &mut Cursors {
        let split_id = self.effective_active_split();
        &mut self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&split_id)
            .unwrap()
            .cursors
    }

    /// Set completion items for type-to-filter (for testing)
    pub fn set_completion_items(&mut self, items: Vec<lsp_types::CompletionItem>) {
        self.active_window_mut().completion_items = Some(items);
    }

    /// Get the viewport for the active split
    pub fn active_viewport(&self) -> &crate::view::viewport::Viewport {
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        &self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&active_split)
            .unwrap()
            .viewport
    }

    /// Get the viewport for the active split (mutable)
    pub fn active_viewport_mut(&mut self) -> &mut crate::view::viewport::Viewport {
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        &mut self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&active_split)
            .unwrap()
            .viewport
    }

    /// Width (in cells) of the line-number gutter for a given split leaf, or 0
    /// when that split hides line numbers. The same value the renderer uses, so
    /// a frontend can peel the gutter off the buffer text at the exact column.
    pub fn leaf_gutter_width(&self, leaf: fresh_core::LeafId, buffer_id: BufferId) -> u16 {
        let Some(w) = self.windows.get(&self.active_window) else {
            return 0;
        };
        let Some((_, view_states)) = w.buffers.splits() else {
            return 0;
        };
        let Some(vs) = view_states.get(&leaf) else {
            return 0;
        };
        if !vs.show_line_numbers {
            return 0;
        }
        match w.buffers.get(&buffer_id) {
            Some(state) => vs.viewport.gutter_width(&state.buffer) as u16,
            None => 0,
        }
    }

    /// Get the display name for a buffer (filename or virtual buffer name)
    pub fn get_buffer_display_name(&self, buffer_id: BufferId) -> String {
        // Check composite buffers first
        if let Some(composite) = self.active_window().composite_buffers.get(&buffer_id) {
            return composite.name.clone();
        }

        self.active_window()
            .buffer_metadata
            .get(&buffer_id)
            .map(|m| m.display_name.clone())
            .or_else(|| {
                self.windows
                    .get(&self.active_window)
                    .map(|w| &w.buffers)
                    .expect("active window present")
                    .get(&buffer_id)
                    .and_then(|state| {
                        state
                            .buffer
                            .file_path()
                            .and_then(|p| p.file_name())
                            .and_then(|n| n.to_str())
                            .map(|s| s.to_string())
                    })
            })
            .unwrap_or_else(|| "[No Name]".to_string())
    }

    /// Get the event log for the active buffer
    pub fn active_event_log(&self) -> &EventLog {
        self.active_window()
            .event_logs
            .get(&self.active_buffer())
            .unwrap()
    }

    /// Get the event log for the active buffer (mutable)
    pub fn active_event_log_mut(&mut self) -> &mut EventLog {
        let buffer_id = self.active_buffer();
        self.active_window_mut()
            .event_logs
            .get_mut(&buffer_id)
            .unwrap()
    }

    /// Register a status-bar token contributed by a plugin.
    /// Token key is "plugin_name:token_name".
    /// Returns Err if token already registered or inputs are invalid.
    pub fn register_status_bar_element(
        &self,
        plugin_name: &str,
        token_name: &str,
        title: &str,
    ) -> Result<(), String> {
        if plugin_name.is_empty() {
            return Err("Plugin name cannot be empty".to_string());
        }
        if token_name.is_empty() {
            return Err("Token name cannot be empty".to_string());
        }

        let key = format!("{}:{}", plugin_name, token_name);
        let mut registry = self.status_bar_token_registry.lock().unwrap();

        if registry.contains_key(&key) {
            return Err(format!("Token '{}' already registered", key));
        }

        registry.insert(key, title.to_string());
        Ok(())
    }

    /// Set the value for a status-bar token on a specific buffer.
    /// Key format: "plugin_name:token_name". The buffer is located across
    /// every window — buffers are uniquely owned, so at most one window
    /// matches.
    pub fn set_status_bar_value(
        &mut self,
        buffer_id: BufferId,
        key: &str,
        value: String,
    ) -> Result<(), String> {
        for window in self.windows.values_mut() {
            if window.buffers.contains_key(&buffer_id) {
                window
                    .status_bar_values
                    .entry(buffer_id)
                    .or_default()
                    .insert(key.to_string(), value);
                return Ok(());
            }
        }
        Err(format!("Buffer {:?} not found", buffer_id))
    }

    /// Get all registered status bar tokens for Settings UI.
    /// Returns Vec of (token_key_with_braces, title).
    pub fn get_status_bar_elements(&self) -> Vec<(String, String)> {
        self.status_bar_token_registry
            .lock()
            .unwrap()
            .iter()
            .map(|(k, title)| (format!("{{{}}}", k), title.clone()))
            .collect()
    }

    /// Snapshot of token values for a specific buffer (render path).
    pub fn get_status_bar_element_values(&self, buffer_id: BufferId) -> HashMap<String, String> {
        for window in self.windows.values() {
            if let Some(values) = window.status_bar_values.get(&buffer_id) {
                return values.clone();
            }
        }
        HashMap::new()
    }

    /// Current value of a single status-bar token for the given buffer, if any.
    /// Used by the plugin command dispatcher to detect no-op `SetStatusBarValue`
    /// commands (i.e. plugins re-publishing an unchanged value on every render).
    pub fn current_status_bar_value(&self, buffer_id: BufferId, key: &str) -> Option<&str> {
        for window in self.windows.values() {
            if let Some(values) = window.status_bar_values.get(&buffer_id) {
                if let Some(v) = values.get(key) {
                    return Some(v.as_str());
                }
                return None;
            }
        }
        None
    }

    /// Remove every registry entry and per-buffer value belonging to a plugin.
    /// Called when a plugin is unloaded.
    fn remove_plugin_status_bar_elements(&mut self, plugin_name: &str) {
        let prefix = format!("{}:", plugin_name);
        self.status_bar_token_registry
            .lock()
            .unwrap()
            .retain(|k, _| !k.starts_with(&prefix));
        for window in self.windows.values_mut() {
            for values in window.status_bar_values.values_mut() {
                values.retain(|k, _| !k.starts_with(&prefix));
            }
        }
    }
}

/// Parse a key string like "RET", "C-n", "M-x", "q" into KeyCode and KeyModifiers
///
/// Supports:
/// - Single characters: "a", "q", etc.
/// - Function keys: "F1", "F2", etc.
/// - Special keys: "RET", "TAB", "ESC", "SPC", "DEL", "BS"
/// - Modifiers: "C-" (Control), "M-" (Alt/Meta), "S-" (Shift)
/// - Combinations: "C-n", "M-x", "C-M-s", etc.
#[cfg(any(feature = "plugins", test))]
fn parse_key_string(key_str: &str) -> Option<(KeyCode, KeyModifiers)> {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut modifiers = KeyModifiers::NONE;
    let mut remaining = key_str;

    // Parse modifiers
    loop {
        if remaining.starts_with("C-") {
            modifiers |= KeyModifiers::CONTROL;
            remaining = &remaining[2..];
        } else if remaining.starts_with("M-") {
            modifiers |= KeyModifiers::ALT;
            remaining = &remaining[2..];
        } else if remaining.starts_with("S-") {
            modifiers |= KeyModifiers::SHIFT;
            remaining = &remaining[2..];
        } else {
            break;
        }
    }

    // Parse the key
    // Use uppercase for matching special keys, but preserve original for single chars
    let upper = remaining.to_uppercase();
    let code = match upper.as_str() {
        "RET" | "RETURN" | "ENTER" => KeyCode::Enter,
        "TAB" => KeyCode::Tab,
        "BACKTAB" => KeyCode::BackTab,
        "ESC" | "ESCAPE" => KeyCode::Esc,
        "SPC" | "SPACE" => KeyCode::Char(' '),
        "DEL" | "DELETE" => KeyCode::Delete,
        "BS" | "BACKSPACE" => KeyCode::Backspace,
        "UP" => KeyCode::Up,
        "DOWN" => KeyCode::Down,
        "LEFT" => KeyCode::Left,
        "RIGHT" => KeyCode::Right,
        "HOME" => KeyCode::Home,
        "END" => KeyCode::End,
        "PAGEUP" | "PGUP" => KeyCode::PageUp,
        "PAGEDOWN" | "PGDN" => KeyCode::PageDown,
        s if s.starts_with('F') && s.len() > 1 => {
            // Function key (F1-F12)
            if let Ok(n) = s[1..].parse::<u8>() {
                KeyCode::F(n)
            } else {
                return None;
            }
        }
        _ if remaining.len() == 1 => {
            // Single character - use ORIGINAL remaining, not uppercased
            // For uppercase letters, add SHIFT modifier so 'J' != 'j'
            let c = remaining.chars().next()?;
            if c.is_ascii_uppercase() {
                modifiers |= KeyModifiers::SHIFT;
            }
            KeyCode::Char(c.to_ascii_lowercase())
        }
        _ => return None,
    };

    // Plugins commonly spell Shift+Tab as "S-Tab"; terminals deliver
    // BackTab and the lookup-side `normalize_key` strips the redundant
    // SHIFT. Normalize on the binding side too so "S-Tab" and "BackTab"
    // both register as `(BackTab, NONE)` and match.
    if code == KeyCode::Tab && modifiers.contains(KeyModifiers::SHIFT) {
        return Some((KeyCode::BackTab, modifiers.difference(KeyModifiers::SHIFT)));
    }

    Some((code, modifiers))
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{Position, Range as LspRange, TextDocumentContentChangeEvent};
    use tempfile::TempDir;

    /// Create a test DirectoryContext with temp directories
    fn test_dir_context() -> (DirectoryContext, TempDir) {
        let temp_dir = TempDir::new().unwrap();
        let dir_context = DirectoryContext::for_testing(temp_dir.path());
        (dir_context, temp_dir)
    }

    /// Create a test filesystem
    fn test_filesystem() -> Arc<dyn FileSystem + Send + Sync> {
        Arc::new(crate::model::filesystem::StdFileSystem)
    }

    #[test]
    fn parse_key_string_shift_tab_normalizes_to_backtab() {
        use crossterm::event::{KeyCode, KeyModifiers};
        // Plugins write "S-Tab" in their defineMode binding tables; the
        // terminal delivers BackTab (with SHIFT stripped by normalize_key
        // on lookup). Without this normalization, the binding never
        // matches.
        assert_eq!(
            parse_key_string("S-Tab"),
            Some((KeyCode::BackTab, KeyModifiers::NONE)),
        );
        assert_eq!(
            parse_key_string("BackTab"),
            Some((KeyCode::BackTab, KeyModifiers::NONE)),
        );
        // Plain Tab is unaffected.
        assert_eq!(
            parse_key_string("Tab"),
            Some((KeyCode::Tab, KeyModifiers::NONE)),
        );
    }

    #[test]
    fn test_editor_new() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        assert_eq!(editor.buffers().len(), 1);
        assert!(!editor.should_quit());
    }

    fn default_test_editor() -> Editor {
        let (dir_context, temp) = test_dir_context();
        // Keep the temp dir alive for the editor's lifetime.
        std::mem::forget(temp);
        Editor::new(
            Config::default(),
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap()
    }

    fn test_panel(placement: PanelPlacement, focused: bool) -> FloatingWidgetState {
        FloatingWidgetState {
            panel_key: crate::widgets::PanelKey::new("test-plugin", 1),
            width_pct: 50,
            height_pct: 50,
            placement,
            focused,
            entries: Vec::new(),
            focus_cursor: None,
            embeds: Vec::new(),
            overlays: Vec::new(),
            scroll_regions: Vec::new(),
            scrollbar_tracks: Vec::new(),
            scrollbar_mouse: Default::default(),
            scrollbar_drag_key: None,
            last_inner_rect: None,
            scrollbar_hover_zones: Vec::new(),
            scrollbar_zone_hovered: false,
            fullscreen: false,
            focus_marker: false,
        }
    }

    /// The overlay layer stack always terminates in the editor base layer,
    /// which owns the keyboard, so a fresh editor resolves to its active
    /// window's context.
    #[test]
    fn overlay_stack_base_layer_owns_keyboard() {
        use crate::app::overlay::LayerKind;
        use crate::input::keybindings::KeyContext;

        let editor = default_test_editor();
        let layers = editor.overlay_layers();
        let base = layers.last().expect("at least the base layer");
        assert_eq!(base.kind, LayerKind::Editor);
        assert!(base.owns_keyboard);
        assert!(!base.blocks_terminal_input);
        assert_eq!(editor.get_key_context(), KeyContext::Normal);
    }

    /// P1 invariant preserved through the P2 layer walk: a *focused* dock
    /// owns the keyboard (`KeyContext::Dock`); once blurred it falls
    /// through to the editor underneath so the buffer stays usable.
    #[test]
    fn focused_dock_owns_keyboard_blurred_falls_through() {
        use crate::input::keybindings::KeyContext;

        let mut editor = default_test_editor();
        editor.dock = Some(test_panel(
            PanelPlacement::LeftDock { width_cols: 30 },
            true,
        ));
        assert_eq!(editor.get_key_context(), KeyContext::Dock);

        editor.dock.as_mut().unwrap().focused = false;
        assert_eq!(editor.get_key_context(), KeyContext::Normal);
    }

    /// A focused centered modal outranks a focused dock — when the
    /// new-session form opens on top of the dock, the modal owns input.
    #[test]
    fn centered_modal_outranks_dock() {
        use crate::input::keybindings::KeyContext;

        let mut editor = default_test_editor();
        editor.dock = Some(test_panel(
            PanelPlacement::LeftDock { width_cols: 30 },
            true,
        ));
        editor.floating_widget_panel = Some(test_panel(PanelPlacement::Centered, true));
        // The centered modal resolves as Normal (not Dock).
        assert_eq!(editor.get_key_context(), KeyContext::Normal);
    }

    /// F3: hiding the left dock (Toggle Dock → unmount) must request a
    /// full clear+redraw so the freed column is repainted unconditionally,
    /// rather than risking stale glyphs lingering as a left "gutter" until
    /// an unrelated event. The on-screen reflow is covered by the
    /// `dock_close_reflows_buffer_to_full_width` e2e test; this asserts the
    /// redraw *request* the e2e harness can't observe (it drives renders
    /// unconditionally, so it can't see a missing redraw).
    ///
    /// Gated on `plugins`: it drives the unmount through the plugin
    /// command path (`handle_plugin_command`), which only exists when the
    /// plugin runtime is compiled in.
    #[cfg(feature = "plugins")]
    #[test]
    fn unmounting_left_dock_requests_full_redraw() {
        use fresh_core::api::PluginCommand;

        let mut editor = default_test_editor();
        editor.dock = Some(test_panel(
            PanelPlacement::LeftDock { width_cols: 30 },
            true,
        ));
        // Drop any redraw request left over from construction.
        let _ = editor.take_full_redraw_request();

        editor
            .handle_plugin_command(PluginCommand::UnmountFloatingWidget {
                plugin: "test-plugin".to_string(),
                panel_id: 1,
            })
            .unwrap();

        assert!(editor.dock.is_none(), "dock should be unmounted");
        assert!(
            editor.take_full_redraw_request(),
            "hiding the dock must request a full redraw so the freed \
             column is repainted immediately"
        );
    }

    /// The dock-only gate: closing a *centered* modal overlays the
    /// full-width chrome without carving it, so it must NOT force a
    /// full-screen clear (that would only flicker).
    #[cfg(feature = "plugins")]
    #[test]
    fn unmounting_centered_modal_does_not_request_full_redraw() {
        use fresh_core::api::PluginCommand;

        let mut editor = default_test_editor();
        editor.floating_widget_panel = Some(test_panel(PanelPlacement::Centered, true));
        let _ = editor.take_full_redraw_request();

        editor
            .handle_plugin_command(PluginCommand::UnmountFloatingWidget {
                plugin: "test-plugin".to_string(),
                panel_id: 1,
            })
            .unwrap();

        assert!(
            editor.floating_widget_panel.is_none(),
            "centered modal should be unmounted"
        );
        assert!(
            !editor.take_full_redraw_request(),
            "closing a centered modal must not force a full-screen clear"
        );
    }

    #[test]
    fn test_new_buffer() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        let id = editor.new_buffer();
        assert_eq!(editor.buffers().len(), 2);
        assert_eq!(editor.active_buffer(), id);
    }

    #[test]
    #[ignore]
    fn test_clipboard() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Manually set clipboard (using internal to avoid system clipboard in tests)
        editor.clipboard.set_internal("test".to_string());

        // Paste should work
        editor.paste();

        let content = editor.active_state().buffer.to_string().unwrap();
        assert_eq!(content, "test");
    }

    #[test]
    fn test_action_to_events_insert_char() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        let events = editor
            .active_window_mut()
            .action_to_events(Action::InsertChar('a'));
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::Insert { position, text, .. } => {
                assert_eq!(*position, 0);
                assert_eq!(text, "a");
            }
            _ => panic!("Expected Insert event"),
        }
    }

    #[test]
    fn test_action_to_events_move_right() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert some text first
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id,
        });

        let events = editor
            .active_window_mut()
            .action_to_events(Action::MoveRight);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor {
                new_position,
                new_anchor,
                ..
            } => {
                // Cursor was at 5 (end of "hello"), stays at 5 (can't move beyond end)
                assert_eq!(*new_position, 5);
                assert_eq!(*new_anchor, None); // No selection
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_move_up_down() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert multi-line text
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "line1\nline2\nline3".to_string(),
            cursor_id,
        });

        // Move cursor to start of line 2
        editor.apply_event_to_active_buffer(&Event::MoveCursor {
            cursor_id,
            old_position: 0, // TODO: Get actual old position
            new_position: 6,
            old_anchor: None, // TODO: Get actual old anchor
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        });

        // Test move up
        let events = editor.active_window_mut().action_to_events(Action::MoveUp);
        assert!(events.is_some());
        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor { new_position, .. } => {
                assert_eq!(*new_position, 0); // Should be at start of line 1
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_insert_newline() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        let events = editor
            .active_window_mut()
            .action_to_events(Action::InsertNewline);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::Insert { text, .. } => {
                assert_eq!(text, "\n");
            }
            _ => panic!("Expected Insert event"),
        }
    }

    #[test]
    fn test_action_to_events_unimplemented() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // These actions should return None (not yet implemented)
        assert!(editor
            .active_window_mut()
            .action_to_events(Action::Save)
            .is_none());
        assert!(editor
            .active_window_mut()
            .action_to_events(Action::Quit)
            .is_none());
        assert!(editor
            .active_window_mut()
            .action_to_events(Action::Undo)
            .is_none());
    }

    #[test]
    fn test_action_to_events_delete_backward() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert some text first
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id,
        });

        let events = editor
            .active_window_mut()
            .action_to_events(Action::DeleteBackward);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::Delete {
                range,
                deleted_text,
                ..
            } => {
                assert_eq!(range.clone(), 4..5); // Delete 'o'
                assert_eq!(deleted_text, "o");
            }
            _ => panic!("Expected Delete event"),
        }
    }

    #[test]
    fn test_action_to_events_delete_forward() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert some text first
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id,
        });

        // Move cursor to position 0
        editor.apply_event_to_active_buffer(&Event::MoveCursor {
            cursor_id,
            old_position: 0, // TODO: Get actual old position
            new_position: 0,
            old_anchor: None, // TODO: Get actual old anchor
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        });

        let events = editor
            .active_window_mut()
            .action_to_events(Action::DeleteForward);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::Delete {
                range,
                deleted_text,
                ..
            } => {
                assert_eq!(range.clone(), 0..1); // Delete 'h'
                assert_eq!(deleted_text, "h");
            }
            _ => panic!("Expected Delete event"),
        }
    }

    #[test]
    fn test_action_to_events_select_right() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert some text first
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id,
        });

        // Move cursor to position 0
        editor.apply_event_to_active_buffer(&Event::MoveCursor {
            cursor_id,
            old_position: 0, // TODO: Get actual old position
            new_position: 0,
            old_anchor: None, // TODO: Get actual old anchor
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        });

        let events = editor
            .active_window_mut()
            .action_to_events(Action::SelectRight);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor {
                new_position,
                new_anchor,
                ..
            } => {
                assert_eq!(*new_position, 1); // Moved to position 1
                assert_eq!(*new_anchor, Some(0)); // Anchor at start
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_select_all() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert some text first
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "hello world".to_string(),
            cursor_id,
        });

        let events = editor
            .active_window_mut()
            .action_to_events(Action::SelectAll);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor {
                new_position,
                new_anchor,
                ..
            } => {
                assert_eq!(*new_position, 11); // At end of buffer
                assert_eq!(*new_anchor, Some(0)); // Anchor at start
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_document_nav() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert multi-line text
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "line1\nline2\nline3".to_string(),
            cursor_id,
        });

        // Test MoveDocumentStart
        let events = editor
            .active_window_mut()
            .action_to_events(Action::MoveDocumentStart);
        assert!(events.is_some());
        let events = events.unwrap();
        match &events[0] {
            Event::MoveCursor { new_position, .. } => {
                assert_eq!(*new_position, 0);
            }
            _ => panic!("Expected MoveCursor event"),
        }

        // Test MoveDocumentEnd
        let events = editor
            .active_window_mut()
            .action_to_events(Action::MoveDocumentEnd);
        assert!(events.is_some());
        let events = events.unwrap();
        match &events[0] {
            Event::MoveCursor { new_position, .. } => {
                assert_eq!(*new_position, 17); // End of buffer
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_remove_secondary_cursors() {
        use crate::model::event::CursorId;

        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert some text first to have positions to place cursors
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "hello world test".to_string(),
            cursor_id,
        });

        // Add secondary cursors at different positions to avoid normalization merging
        editor.apply_event_to_active_buffer(&Event::AddCursor {
            cursor_id: CursorId(1),
            position: 5,
            anchor: None,
        });
        editor.apply_event_to_active_buffer(&Event::AddCursor {
            cursor_id: CursorId(2),
            position: 10,
            anchor: None,
        });

        assert_eq!(editor.active_cursors().count(), 3);

        // Find the first cursor ID (the one that will be kept)
        let first_id = editor
            .active_cursors()
            .iter()
            .map(|(id, _)| id)
            .min_by_key(|id| id.0)
            .expect("Should have at least one cursor");

        // RemoveSecondaryCursors should generate RemoveCursor events
        let events = editor
            .active_window_mut()
            .action_to_events(Action::RemoveSecondaryCursors);
        assert!(events.is_some());

        let events = events.unwrap();
        // Should have RemoveCursor events for the two secondary cursors
        // Plus ClearAnchor events for all cursors (to clear Emacs mark mode)
        let remove_cursor_events: Vec<_> = events
            .iter()
            .filter_map(|e| match e {
                Event::RemoveCursor { cursor_id, .. } => Some(*cursor_id),
                _ => None,
            })
            .collect();

        // Should have 2 RemoveCursor events (one for each secondary cursor)
        assert_eq!(remove_cursor_events.len(), 2);

        for cursor_id in &remove_cursor_events {
            // Should not be the first cursor (the one we're keeping)
            assert_ne!(*cursor_id, first_id);
        }
    }

    #[test]
    fn test_action_to_events_scroll() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Test ScrollUp
        let events = editor
            .active_window_mut()
            .action_to_events(Action::ScrollUp);
        assert!(events.is_some());
        let events = events.unwrap();
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Scroll { line_offset } => {
                assert_eq!(*line_offset, -1);
            }
            _ => panic!("Expected Scroll event"),
        }

        // Test ScrollDown
        let events = editor
            .active_window_mut()
            .action_to_events(Action::ScrollDown);
        assert!(events.is_some());
        let events = events.unwrap();
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Scroll { line_offset } => {
                assert_eq!(*line_offset, 1);
            }
            _ => panic!("Expected Scroll event"),
        }
    }

    #[test]
    fn test_action_to_events_none() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // None action should return None
        let events = editor.active_window_mut().action_to_events(Action::None);
        assert!(events.is_none());
    }

    #[test]
    fn test_lsp_incremental_insert_generates_correct_range() {
        // Test that insert events generate correct incremental LSP changes
        // with zero-width ranges at the insertion point
        use crate::model::buffer::Buffer;

        let buffer = Buffer::from_str_test("hello\nworld");

        // Insert "NEW" at position 0 (before "hello")
        // Expected LSP range: line 0, char 0 to line 0, char 0 (zero-width)
        let position = 0;
        let (line, character) = buffer.position_to_lsp_position(position);

        assert_eq!(line, 0, "Insertion at start should be line 0");
        assert_eq!(character, 0, "Insertion at start should be char 0");

        // Create the range as we do in notify_lsp_change
        let lsp_pos = Position::new(line as u32, character as u32);
        let lsp_range = LspRange::new(lsp_pos, lsp_pos);

        assert_eq!(lsp_range.start.line, 0);
        assert_eq!(lsp_range.start.character, 0);
        assert_eq!(lsp_range.end.line, 0);
        assert_eq!(lsp_range.end.character, 0);
        assert_eq!(
            lsp_range.start, lsp_range.end,
            "Insert should have zero-width range"
        );

        // Test insertion at middle of first line (position 3, after "hel")
        let position = 3;
        let (line, character) = buffer.position_to_lsp_position(position);

        assert_eq!(line, 0);
        assert_eq!(character, 3);

        // Test insertion at start of second line (position 6, after "hello\n")
        let position = 6;
        let (line, character) = buffer.position_to_lsp_position(position);

        assert_eq!(line, 1, "Position after newline should be line 1");
        assert_eq!(character, 0, "Position at start of line 2 should be char 0");
    }

    #[test]
    fn test_lsp_incremental_delete_generates_correct_range() {
        // Test that delete events generate correct incremental LSP changes
        // with proper start/end ranges
        use crate::model::buffer::Buffer;

        let buffer = Buffer::from_str_test("hello\nworld");

        // Delete "ello" (positions 1-5 on line 0)
        let range_start = 1;
        let range_end = 5;

        let (start_line, start_char) = buffer.position_to_lsp_position(range_start);
        let (end_line, end_char) = buffer.position_to_lsp_position(range_end);

        assert_eq!(start_line, 0);
        assert_eq!(start_char, 1);
        assert_eq!(end_line, 0);
        assert_eq!(end_char, 5);

        let lsp_range = LspRange::new(
            Position::new(start_line as u32, start_char as u32),
            Position::new(end_line as u32, end_char as u32),
        );

        assert_eq!(lsp_range.start.line, 0);
        assert_eq!(lsp_range.start.character, 1);
        assert_eq!(lsp_range.end.line, 0);
        assert_eq!(lsp_range.end.character, 5);
        assert_ne!(
            lsp_range.start, lsp_range.end,
            "Delete should have non-zero range"
        );

        // Test deletion across lines (delete "o\nw" - positions 4-8)
        let range_start = 4;
        let range_end = 8;

        let (start_line, start_char) = buffer.position_to_lsp_position(range_start);
        let (end_line, end_char) = buffer.position_to_lsp_position(range_end);

        assert_eq!(start_line, 0, "Delete start on line 0");
        assert_eq!(start_char, 4, "Delete start at char 4");
        assert_eq!(end_line, 1, "Delete end on line 1");
        assert_eq!(end_char, 2, "Delete end at char 2 of line 1");
    }

    #[test]
    fn test_lsp_incremental_utf16_encoding() {
        // Test that position_to_lsp_position correctly handles UTF-16 encoding
        // LSP uses UTF-16 code units, not byte positions
        use crate::model::buffer::Buffer;

        // Test with emoji (4 bytes in UTF-8, 2 code units in UTF-16)
        let buffer = Buffer::from_str_test("😀hello");

        // Position 4 is after the emoji (4 bytes)
        let (line, character) = buffer.position_to_lsp_position(4);

        assert_eq!(line, 0);
        assert_eq!(character, 2, "Emoji should count as 2 UTF-16 code units");

        // Position 9 is after "😀hell" (4 bytes emoji + 5 bytes text)
        let (line, character) = buffer.position_to_lsp_position(9);

        assert_eq!(line, 0);
        assert_eq!(
            character, 7,
            "Should be 2 (emoji) + 5 (text) = 7 UTF-16 code units"
        );

        // Test with multi-byte character (é is 2 bytes in UTF-8, 1 code unit in UTF-16)
        let buffer = Buffer::from_str_test("café");

        // Position 3 is after "caf" (3 bytes)
        let (line, character) = buffer.position_to_lsp_position(3);

        assert_eq!(line, 0);
        assert_eq!(character, 3);

        // Position 5 is after "café" (3 + 2 bytes)
        let (line, character) = buffer.position_to_lsp_position(5);

        assert_eq!(line, 0);
        assert_eq!(character, 4, "é should count as 1 UTF-16 code unit");
    }

    #[test]
    fn test_lsp_content_change_event_structure() {
        // Test that we can create TextDocumentContentChangeEvent for incremental updates

        // Incremental insert
        let insert_change = TextDocumentContentChangeEvent {
            range: Some(LspRange::new(Position::new(0, 5), Position::new(0, 5))),
            range_length: None,
            text: "NEW".to_string(),
        };

        assert!(insert_change.range.is_some());
        assert_eq!(insert_change.text, "NEW");
        let range = insert_change.range.unwrap();
        assert_eq!(
            range.start, range.end,
            "Insert should have zero-width range"
        );

        // Incremental delete
        let delete_change = TextDocumentContentChangeEvent {
            range: Some(LspRange::new(Position::new(0, 2), Position::new(0, 7))),
            range_length: None,
            text: String::new(),
        };

        assert!(delete_change.range.is_some());
        assert_eq!(delete_change.text, "");
        let range = delete_change.range.unwrap();
        assert_ne!(range.start, range.end, "Delete should have non-zero range");
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 2);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 7);
    }

    #[test]
    fn test_goto_matching_bracket_forward() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert text with brackets
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "fn main() { let x = (1 + 2); }".to_string(),
            cursor_id,
        });

        // Move cursor to opening brace '{'
        editor.apply_event_to_active_buffer(&Event::MoveCursor {
            cursor_id,
            old_position: 31,
            new_position: 10,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        });

        assert_eq!(editor.active_cursors().primary().position, 10);

        // Call goto_matching_bracket
        editor.goto_matching_bracket();

        // Should move to closing brace '}' at position 29
        // "fn main() { let x = (1 + 2); }"
        //            ^                   ^
        //           10                  29
        assert_eq!(editor.active_cursors().primary().position, 29);
    }

    #[test]
    fn test_goto_matching_bracket_backward() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert text with brackets
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "fn main() { let x = (1 + 2); }".to_string(),
            cursor_id,
        });

        // Move cursor to closing paren ')'
        editor.apply_event_to_active_buffer(&Event::MoveCursor {
            cursor_id,
            old_position: 31,
            new_position: 26,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        });

        // Call goto_matching_bracket
        editor.goto_matching_bracket();

        // Should move to opening paren '('
        assert_eq!(editor.active_cursors().primary().position, 20);
    }

    #[test]
    fn test_goto_matching_bracket_nested() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert text with nested brackets
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "{a{b{c}d}e}".to_string(),
            cursor_id,
        });

        // Move cursor to first '{'
        editor.apply_event_to_active_buffer(&Event::MoveCursor {
            cursor_id,
            old_position: 11,
            new_position: 0,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        });

        // Call goto_matching_bracket
        editor.goto_matching_bracket();

        // Should jump to last '}'
        assert_eq!(editor.active_cursors().primary().position, 10);
    }

    #[test]
    fn test_search_case_sensitive() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert text
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "Hello hello HELLO".to_string(),
            cursor_id,
        });

        // Test case-insensitive search (default)
        editor.active_window_mut().search_case_sensitive = false;
        editor.perform_search("hello");

        let search_state = editor.active_window().search_state.as_ref().unwrap();
        assert_eq!(
            search_state.matches.len(),
            3,
            "Should find all 3 matches case-insensitively"
        );

        // Test case-sensitive search
        editor.active_window_mut().search_case_sensitive = true;
        editor.perform_search("hello");

        let search_state = editor.active_window().search_state.as_ref().unwrap();
        assert_eq!(
            search_state.matches.len(),
            1,
            "Should find only 1 exact match"
        );
        assert_eq!(
            search_state.matches[0], 6,
            "Should find 'hello' at position 6"
        );
    }

    #[test]
    fn test_search_whole_word() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert text
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "test testing tested attest test".to_string(),
            cursor_id,
        });

        // Test partial word match (default)
        editor.active_window_mut().search_whole_word = false;
        editor.active_window_mut().search_case_sensitive = true;
        editor.perform_search("test");

        let search_state = editor.active_window().search_state.as_ref().unwrap();
        assert_eq!(
            search_state.matches.len(),
            5,
            "Should find 'test' in all occurrences"
        );

        // Test whole word match
        editor.active_window_mut().search_whole_word = true;
        editor.perform_search("test");

        let search_state = editor.active_window().search_state.as_ref().unwrap();
        assert_eq!(
            search_state.matches.len(),
            2,
            "Should find only whole word 'test'"
        );
        assert_eq!(search_state.matches[0], 0, "First match at position 0");
        assert_eq!(search_state.matches[1], 27, "Second match at position 27");
    }

    #[test]
    #[cfg(feature = "plugins")]
    fn repro_2414_plugin_edit_same_line_next_match() {
        // Issue #2414: after a plugin replaces the first of two matches on the
        // SAME line (delete range + insert, net length change), navigating to
        // the next match lands a few bytes short of the real position.
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Line 0 has TWO occurrences of "<i>"; line 1 has one.
        // "<i>a</i> mid <i>b</i>\n<i>c</i>"
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "<i>a</i> mid <i>b</i>\n<i>c</i>".to_string(),
            cursor_id,
        });

        editor.active_window_mut().search_case_sensitive = true;
        editor.perform_search("<i>");

        // Sanity: three matches at 0, 13, 22.
        let m = editor
            .active_window()
            .search_state
            .as_ref()
            .unwrap()
            .matches
            .clone();
        assert_eq!(m, vec![0, 13, 22], "initial match offsets");

        // Plugin replaces the first element "<i>a</i>" (bytes 0..8) with the
        // longer "<em>a</em>" (10 bytes, net +2) via the plugin edit path.
        let buf = editor.active_buffer();
        editor.handle_delete_range(buf, 0..8);
        editor.handle_insert_text(buf, 0, "<em>a</em>".to_string());

        // The second "<i>" on line 0 is now at byte 15 (13 + 2).
        // Move cursor to the start of line 0 and ask for the next match.
        // Buffer is now "<em>a</em> mid <i>b</i>\n<i>c</i>"; the surviving
        // "<i>" occurrences sit at bytes 15 (same line) and 24 (next line).
        assert_eq!(
            editor.active_state().buffer.to_string().unwrap(),
            "<em>a</em> mid <i>b</i>\n<i>c</i>"
        );

        // From the start of line 0, the next match must be the real second
        // "<i>" at byte 15 — not the phantom overlay (byte 10) left behind when
        // the plugin's delete collapsed the first match's highlight and the
        // following insert pushed it forward.
        editor.active_cursors_mut().primary_mut().move_to(0, false);
        editor.find_next();
        assert_eq!(
            editor.active_cursors().primary().position,
            15,
            "next match should land on the shifted same-line '<i>' (byte 15)"
        );

        // And the following match is the next-line occurrence at byte 24.
        editor.find_next();
        assert_eq!(
            editor.active_cursors().primary().position,
            24,
            "subsequent match should land on the next-line '<i>' (byte 24)"
        );
    }

    #[test]
    fn test_search_scan_completes_when_capped() {
        // Regression test: when the incremental search scan hits MAX_MATCHES
        // early (e.g. at 15% of the file), the scan's `capped` flag is set to
        // true and the batch loop breaks.  The completion check in
        // process_search_scan() must also consider `capped` — otherwise the
        // scan gets stuck in an infinite loop showing "Searching... 15%".
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Manually create a search scan state that is already capped but not
        // at the last chunk (simulating early cap at ~15%).
        let buffer_id = editor.active_buffer();
        let regex = regex::bytes::Regex::new("test").unwrap();
        let fake_chunks = vec![
            crate::model::buffer::LineScanChunk {
                leaf_index: 0,
                byte_len: 100,
                already_known: true,
            },
            crate::model::buffer::LineScanChunk {
                leaf_index: 1,
                byte_len: 100,
                already_known: true,
            },
        ];

        let chunked = crate::model::buffer::ChunkedSearchState {
            chunks: fake_chunks,
            next_chunk: 1, // Only processed 1 of 2 chunks
            next_doc_offset: 100,
            total_bytes: 200,
            scanned_bytes: 100,
            regex,
            matches: vec![
                crate::model::buffer::SearchMatch {
                    byte_offset: 10,
                    length: 4,
                    line: 1,
                    column: 11,
                    context: String::new(),
                },
                crate::model::buffer::SearchMatch {
                    byte_offset: 50,
                    length: 4,
                    line: 1,
                    column: 51,
                    context: String::new(),
                },
            ],
            overlap_tail: Vec::new(),
            overlap_doc_offset: 0,
            max_matches: 10_000,
            capped: true, // Capped early — this is the key condition
            query_len: 4,
            running_line: 1,
        };

        editor.active_window_mut().search_scan.start(
            buffer_id,
            Vec::new(),
            chunked,
            "test".to_string(),
            None,
            false,
            false,
            false,
        );

        // process_search_scan should finalize the search (not loop forever)
        let result = editor.process_search_scan();
        assert!(
            result,
            "process_search_scan should return true (needs render)"
        );

        // The scan state should be consumed (drained)
        assert_eq!(
            editor.active_window().search_scan.buffer_id(),
            None,
            "search_scan should be drained after capped scan completes"
        );

        // Search state should be set with the accumulated matches
        let search_state = editor
            .active_window()
            .search_state
            .as_ref()
            .expect("search_state should be set after scan finishes");
        assert_eq!(search_state.matches.len(), 2, "Should have 2 matches");
        assert_eq!(search_state.query, "test");
        assert!(
            search_state.capped,
            "search_state should be marked as capped"
        );
    }

    #[test]
    fn test_bookmarks() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Insert text
        let cursor_id = editor.active_cursors().primary_id();
        editor.apply_event_to_active_buffer(&Event::Insert {
            position: 0,
            text: "Line 1\nLine 2\nLine 3".to_string(),
            cursor_id,
        });

        // Move cursor to line 2 start (position 7)
        editor.apply_event_to_active_buffer(&Event::MoveCursor {
            cursor_id,
            old_position: 21,
            new_position: 7,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        });

        // Set bookmark '1'
        editor.active_window_mut().set_bookmark('1');
        assert_eq!(
            editor
                .active_window()
                .bookmarks
                .get('1')
                .map(|b| b.position),
            Some(7)
        );

        // Move cursor elsewhere
        editor.apply_event_to_active_buffer(&Event::MoveCursor {
            cursor_id,
            old_position: 7,
            new_position: 14,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        });

        // Jump back to bookmark
        editor.jump_to_bookmark('1');
        assert_eq!(editor.active_cursors().primary().position, 7);

        // Clear bookmark
        editor.active_window_mut().clear_bookmark('1');
        assert_eq!(editor.active_window().bookmarks.get('1'), None);
    }

    #[test]
    fn test_action_enum_new_variants() {
        // Test that new actions can be parsed from strings
        use serde_json::json;

        let args = HashMap::new();
        assert_eq!(
            Action::from_str("smart_home", &args),
            Some(Action::SmartHome)
        );
        assert_eq!(
            Action::from_str("dedent_selection", &args),
            Some(Action::DedentSelection)
        );
        assert_eq!(
            Action::from_str("toggle_comment", &args),
            Some(Action::ToggleComment)
        );
        assert_eq!(
            Action::from_str("goto_matching_bracket", &args),
            Some(Action::GoToMatchingBracket)
        );
        assert_eq!(
            Action::from_str("list_bookmarks", &args),
            Some(Action::ListBookmarks)
        );
        assert_eq!(
            Action::from_str("toggle_search_case_sensitive", &args),
            Some(Action::ToggleSearchCaseSensitive)
        );
        assert_eq!(
            Action::from_str("toggle_search_whole_word", &args),
            Some(Action::ToggleSearchWholeWord)
        );

        // Test bookmark actions with arguments
        let mut args_with_char = HashMap::new();
        args_with_char.insert("char".to_string(), json!("5"));
        assert_eq!(
            Action::from_str("set_bookmark", &args_with_char),
            Some(Action::SetBookmark('5'))
        );
        assert_eq!(
            Action::from_str("jump_to_bookmark", &args_with_char),
            Some(Action::JumpToBookmark('5'))
        );
        assert_eq!(
            Action::from_str("clear_bookmark", &args_with_char),
            Some(Action::ClearBookmark('5'))
        );
    }

    #[test]
    fn test_keybinding_new_defaults() {
        use crossterm::event::{KeyEvent, KeyEventKind, KeyEventState};

        // Test that new keybindings are properly registered in the "default" keymap
        // Note: We explicitly use "default" keymap, not Config::default() which uses
        // platform-specific keymaps (e.g., "macos" on macOS has different bindings)
        let mut config = Config::default();
        config.active_keybinding_map = crate::config::KeybindingMapName("default".to_string());
        let resolver = KeybindingResolver::new(&config);

        // Test Ctrl+/ is ToggleComment (not CommandPalette)
        let event = KeyEvent {
            code: KeyCode::Char('/'),
            modifiers: KeyModifiers::CONTROL,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        };
        let action = resolver.resolve(&event, KeyContext::Normal);
        assert_eq!(action, Action::ToggleComment);

        // Test Ctrl+] is GoToMatchingBracket
        let event = KeyEvent {
            code: KeyCode::Char(']'),
            modifiers: KeyModifiers::CONTROL,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        };
        let action = resolver.resolve(&event, KeyContext::Normal);
        assert_eq!(action, Action::GoToMatchingBracket);

        // Test Shift+Tab is DedentSelection
        let event = KeyEvent {
            code: KeyCode::Tab,
            modifiers: KeyModifiers::SHIFT,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        };
        let action = resolver.resolve(&event, KeyContext::Normal);
        assert_eq!(action, Action::DedentSelection);

        // Test Ctrl+G is GotoLine
        let event = KeyEvent {
            code: KeyCode::Char('g'),
            modifiers: KeyModifiers::CONTROL,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        };
        let action = resolver.resolve(&event, KeyContext::Normal);
        assert_eq!(action, Action::GotoLine);

        // Test bookmark keybindings
        let event = KeyEvent {
            code: KeyCode::Char('5'),
            modifiers: KeyModifiers::CONTROL | KeyModifiers::SHIFT,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        };
        let action = resolver.resolve(&event, KeyContext::Normal);
        assert_eq!(action, Action::SetBookmark('5'));

        let event = KeyEvent {
            code: KeyCode::Char('5'),
            modifiers: KeyModifiers::ALT,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        };
        let action = resolver.resolve(&event, KeyContext::Normal);
        assert_eq!(action, Action::JumpToBookmark('5'));
    }

    /// This test demonstrates the bug where LSP didChange notifications contain
    /// incorrect positions because they're calculated from the already-modified buffer.
    ///
    /// When applying LSP rename edits:
    /// 1. apply_events_to_buffer_as_bulk_edit() applies the edits to the buffer
    /// 2. Then calls notify_lsp_change() which calls collect_lsp_changes()
    /// 3. collect_lsp_changes() converts byte positions to LSP positions using
    ///    the CURRENT buffer state
    ///
    /// But the byte positions in the events are relative to the ORIGINAL buffer,
    /// not the modified one! This causes LSP to receive wrong positions.
    #[test]
    fn test_lsp_rename_didchange_positions_bug() {
        use crate::model::buffer::Buffer;

        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Set buffer content: "fn foo(val: i32) {\n    val + 1\n}\n"
        // Line 0: positions 0-19 (includes newline)
        // Line 1: positions 19-31 (includes newline)
        let initial = "fn foo(val: i32) {\n    val + 1\n}\n";
        editor.active_state_mut().buffer =
            Buffer::from_str(initial, 1024 * 1024, test_filesystem());

        // Simulate LSP rename batch: rename "val" to "value" in two places
        // This is applied in reverse order to preserve positions:
        // 1. Delete "val" at position 23 (line 1, char 4), insert "value"
        // 2. Delete "val" at position 7 (line 0, char 7), insert "value"
        let cursor_id = editor.active_cursors().primary_id();

        let batch = Event::Batch {
            events: vec![
                // Second occurrence first (reverse order for position preservation)
                Event::Delete {
                    range: 23..26, // "val" on line 1
                    deleted_text: "val".to_string(),
                    cursor_id,
                },
                Event::Insert {
                    position: 23,
                    text: "value".to_string(),
                    cursor_id,
                },
                // First occurrence second
                Event::Delete {
                    range: 7..10, // "val" on line 0
                    deleted_text: "val".to_string(),
                    cursor_id,
                },
                Event::Insert {
                    position: 7,
                    text: "value".to_string(),
                    cursor_id,
                },
            ],
            description: "LSP Rename".to_string(),
        };

        // CORRECT: Calculate LSP positions BEFORE applying batch
        let lsp_changes_before = editor.active_window().collect_lsp_changes(&batch);

        // Now apply the batch (this is what apply_events_to_buffer_as_bulk_edit does)
        editor.apply_event_to_active_buffer(&batch);

        // BUG DEMONSTRATION: Calculate LSP positions AFTER applying batch
        // This is what happens when notify_lsp_change is called after state.apply()
        let lsp_changes_after = editor.active_window().collect_lsp_changes(&batch);

        // Verify buffer was correctly modified
        let final_content = editor.active_state().buffer.to_string().unwrap();
        assert_eq!(
            final_content, "fn foo(value: i32) {\n    value + 1\n}\n",
            "Buffer should have 'value' in both places"
        );

        // The CORRECT positions (before applying batch):
        // - Delete at 23..26 should be line 1, char 4-7 (in original buffer)
        // - Insert at 23 should be line 1, char 4 (in original buffer)
        // - Delete at 7..10 should be line 0, char 7-10 (in original buffer)
        // - Insert at 7 should be line 0, char 7 (in original buffer)
        assert_eq!(lsp_changes_before.len(), 4, "Should have 4 changes");

        let first_delete = &lsp_changes_before[0];
        let first_del_range = first_delete.range.unwrap();
        assert_eq!(
            first_del_range.start.line, 1,
            "First delete should be on line 1 (BEFORE)"
        );
        assert_eq!(
            first_del_range.start.character, 4,
            "First delete start should be at char 4 (BEFORE)"
        );

        // The INCORRECT positions (after applying batch):
        // Since the buffer has changed, position 23 now points to different text!
        // Original buffer position 23 was start of "val" on line 1
        // But after rename, the buffer is "fn foo(value: i32) {\n    value + 1\n}\n"
        // Position 23 in new buffer is 'l' in "value" (line 1, offset into "value")
        assert_eq!(lsp_changes_after.len(), 4, "Should have 4 changes");

        let first_delete_after = &lsp_changes_after[0];
        let first_del_range_after = first_delete_after.range.unwrap();

        // THIS IS THE BUG: The positions are WRONG when calculated from modified buffer
        // The first delete's range.end position will be wrong because the buffer changed
        eprintln!("BEFORE modification:");
        eprintln!(
            "  Delete at line {}, char {}-{}",
            first_del_range.start.line,
            first_del_range.start.character,
            first_del_range.end.character
        );
        eprintln!("AFTER modification:");
        eprintln!(
            "  Delete at line {}, char {}-{}",
            first_del_range_after.start.line,
            first_del_range_after.start.character,
            first_del_range_after.end.character
        );

        // The bug causes the position calculation to be wrong.
        // After applying the batch, position 23..26 in the modified buffer
        // is different from what it was in the original buffer.
        //
        // Modified buffer: "fn foo(value: i32) {\n    value + 1\n}\n"
        // Position 23 = 'l' in second "value"
        // Position 26 = 'e' in second "value"
        // This maps to line 1, char 2-5 (wrong!)
        //
        // Original buffer: "fn foo(val: i32) {\n    val + 1\n}\n"
        // Position 23 = 'v' in "val"
        // Position 26 = ' ' after "val"
        // This maps to line 1, char 4-7 (correct!)

        // The positions are different! This demonstrates the bug.
        // Note: Due to how the batch is applied (all operations at once),
        // the exact positions may vary, but they will definitely be wrong.
        assert_ne!(
            first_del_range_after.end.character, first_del_range.end.character,
            "BUG CONFIRMED: LSP positions are different when calculated after buffer modification!"
        );

        eprintln!("\n=== BUG DEMONSTRATED ===");
        eprintln!("When collect_lsp_changes() is called AFTER buffer modification,");
        eprintln!("the positions are WRONG because they're calculated from the");
        eprintln!("modified buffer, not the original buffer.");
        eprintln!("This causes the second rename to fail with 'content modified' error.");
        eprintln!("========================\n");
    }

    #[test]
    fn test_lsp_rename_preserves_cursor_position() {
        use crate::model::buffer::Buffer;

        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Set buffer content: "fn foo(val: i32) {\n    val + 1\n}\n"
        // Line 0: positions 0-19 (includes newline)
        // Line 1: positions 19-31 (includes newline)
        let initial = "fn foo(val: i32) {\n    val + 1\n}\n";
        editor.active_state_mut().buffer =
            Buffer::from_str(initial, 1024 * 1024, test_filesystem());

        // Position cursor at the second "val" (position 23 = 'v' of "val" on line 1)
        let original_cursor_pos = 23;
        editor.active_cursors_mut().primary_mut().position = original_cursor_pos;

        // Verify cursor is at the right position
        let buffer_text = editor.active_state().buffer.to_string().unwrap();
        let text_at_cursor = buffer_text[original_cursor_pos..original_cursor_pos + 3].to_string();
        assert_eq!(text_at_cursor, "val", "Cursor should be at 'val'");

        // Simulate LSP rename batch: rename "val" to "value" in two places
        // Applied in reverse order (from end of file to start)
        let cursor_id = editor.active_cursors().primary_id();
        let buffer_id = editor.active_buffer();

        let events = vec![
            // Second occurrence first (at position 23, line 1)
            Event::Delete {
                range: 23..26, // "val" on line 1
                deleted_text: "val".to_string(),
                cursor_id,
            },
            Event::Insert {
                position: 23,
                text: "value".to_string(),
                cursor_id,
            },
            // First occurrence second (at position 7, line 0)
            Event::Delete {
                range: 7..10, // "val" on line 0
                deleted_text: "val".to_string(),
                cursor_id,
            },
            Event::Insert {
                position: 7,
                text: "value".to_string(),
                cursor_id,
            },
        ];

        // Apply the rename using bulk edit (this should preserve cursor position)
        editor
            .apply_events_to_buffer_as_bulk_edit(buffer_id, events, "LSP Rename".to_string())
            .unwrap();

        // Verify buffer was correctly modified
        let final_content = editor.active_state().buffer.to_string().unwrap();
        assert_eq!(
            final_content, "fn foo(value: i32) {\n    value + 1\n}\n",
            "Buffer should have 'value' in both places"
        );

        // The cursor was originally at position 23 (start of "val" on line 1).
        // After renaming:
        // - The first "val" (at pos 7-10) was replaced with "value" (5 chars instead of 3)
        //   This adds 2 bytes before the cursor.
        // - The second "val" at the cursor position was replaced.
        //
        // Expected cursor position: 23 + 2 = 25 (start of "value" on line 1)
        let final_cursor_pos = editor.active_cursors().primary().position;
        let expected_cursor_pos = 25; // original 23 + 2 (delta from first rename)

        assert_eq!(
            final_cursor_pos, expected_cursor_pos,
            "Cursor should be at position {} (start of 'value' on line 1), but was at {}. \
             Original pos: {}, expected adjustment: +2 for first rename",
            expected_cursor_pos, final_cursor_pos, original_cursor_pos
        );

        // Verify cursor is at start of the renamed symbol
        let text_at_new_cursor = &final_content[final_cursor_pos..final_cursor_pos + 5];
        assert_eq!(
            text_at_new_cursor, "value",
            "Cursor should be at the start of 'value' after rename"
        );
    }

    #[test]
    fn test_lsp_rename_twice_consecutive() {
        // This test reproduces the bug where the second rename fails because
        // LSP positions are calculated incorrectly after the first rename.
        use crate::model::buffer::Buffer;

        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();

        // Initial content: "fn foo(val: i32) {\n    val + 1\n}\n"
        let initial = "fn foo(val: i32) {\n    val + 1\n}\n";
        editor.active_state_mut().buffer =
            Buffer::from_str(initial, 1024 * 1024, test_filesystem());

        let cursor_id = editor.active_cursors().primary_id();
        let buffer_id = editor.active_buffer();

        // === FIRST RENAME: "val" -> "value" ===
        // Create events for first rename (applied in reverse order)
        let events1 = vec![
            // Second occurrence first (at position 23, line 1, char 4)
            Event::Delete {
                range: 23..26,
                deleted_text: "val".to_string(),
                cursor_id,
            },
            Event::Insert {
                position: 23,
                text: "value".to_string(),
                cursor_id,
            },
            // First occurrence (at position 7, line 0, char 7)
            Event::Delete {
                range: 7..10,
                deleted_text: "val".to_string(),
                cursor_id,
            },
            Event::Insert {
                position: 7,
                text: "value".to_string(),
                cursor_id,
            },
        ];

        // Create batch for LSP change verification
        let batch1 = Event::Batch {
            events: events1.clone(),
            description: "LSP Rename 1".to_string(),
        };

        // Collect LSP changes BEFORE applying (this is the fix)
        let lsp_changes1 = editor.active_window().collect_lsp_changes(&batch1);

        // Verify first rename LSP positions are correct
        assert_eq!(
            lsp_changes1.len(),
            4,
            "First rename should have 4 LSP changes"
        );

        // First delete should be at line 1, char 4-7 (second "val")
        let first_del = &lsp_changes1[0];
        let first_del_range = first_del.range.unwrap();
        assert_eq!(first_del_range.start.line, 1, "First delete line");
        assert_eq!(
            first_del_range.start.character, 4,
            "First delete start char"
        );
        assert_eq!(first_del_range.end.character, 7, "First delete end char");

        // Apply first rename using bulk edit
        editor
            .apply_events_to_buffer_as_bulk_edit(buffer_id, events1, "LSP Rename 1".to_string())
            .unwrap();

        // Verify buffer after first rename
        let after_first = editor.active_state().buffer.to_string().unwrap();
        assert_eq!(
            after_first, "fn foo(value: i32) {\n    value + 1\n}\n",
            "After first rename"
        );

        // === SECOND RENAME: "value" -> "x" ===
        // Now "value" is at:
        // - Line 0, char 7-12 (positions 7-12 in buffer)
        // - Line 1, char 4-9 (positions 25-30 in buffer, because line 0 grew by 2)
        //
        // Buffer: "fn foo(value: i32) {\n    value + 1\n}\n"
        //          0123456789...

        // Create events for second rename
        let events2 = vec![
            // Second occurrence first (at position 25, line 1, char 4)
            Event::Delete {
                range: 25..30,
                deleted_text: "value".to_string(),
                cursor_id,
            },
            Event::Insert {
                position: 25,
                text: "x".to_string(),
                cursor_id,
            },
            // First occurrence (at position 7, line 0, char 7)
            Event::Delete {
                range: 7..12,
                deleted_text: "value".to_string(),
                cursor_id,
            },
            Event::Insert {
                position: 7,
                text: "x".to_string(),
                cursor_id,
            },
        ];

        // Create batch for LSP change verification
        let batch2 = Event::Batch {
            events: events2.clone(),
            description: "LSP Rename 2".to_string(),
        };

        // Collect LSP changes BEFORE applying (this is the fix)
        let lsp_changes2 = editor.active_window().collect_lsp_changes(&batch2);

        // Verify second rename LSP positions are correct
        // THIS IS WHERE THE BUG WOULD MANIFEST - if positions are wrong,
        // the LSP server would report "No references found at position"
        assert_eq!(
            lsp_changes2.len(),
            4,
            "Second rename should have 4 LSP changes"
        );

        // First delete should be at line 1, char 4-9 (second "value")
        let second_first_del = &lsp_changes2[0];
        let second_first_del_range = second_first_del.range.unwrap();
        assert_eq!(
            second_first_del_range.start.line, 1,
            "Second rename first delete should be on line 1"
        );
        assert_eq!(
            second_first_del_range.start.character, 4,
            "Second rename first delete start should be at char 4"
        );
        assert_eq!(
            second_first_del_range.end.character, 9,
            "Second rename first delete end should be at char 9 (4 + 5 for 'value')"
        );

        // Third delete should be at line 0, char 7-12 (first "value")
        let second_third_del = &lsp_changes2[2];
        let second_third_del_range = second_third_del.range.unwrap();
        assert_eq!(
            second_third_del_range.start.line, 0,
            "Second rename third delete should be on line 0"
        );
        assert_eq!(
            second_third_del_range.start.character, 7,
            "Second rename third delete start should be at char 7"
        );
        assert_eq!(
            second_third_del_range.end.character, 12,
            "Second rename third delete end should be at char 12 (7 + 5 for 'value')"
        );

        // Apply second rename using bulk edit
        editor
            .apply_events_to_buffer_as_bulk_edit(buffer_id, events2, "LSP Rename 2".to_string())
            .unwrap();

        // Verify buffer after second rename
        let after_second = editor.active_state().buffer.to_string().unwrap();
        assert_eq!(
            after_second, "fn foo(x: i32) {\n    x + 1\n}\n",
            "After second rename"
        );
    }

    #[test]
    fn test_ensure_active_tab_visible_static_offset() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();
        let split_id = editor.split_manager().active_split();

        // Create three buffers with long names to force scrolling.
        let buf1 = editor.new_buffer();
        editor
            .buffers_mut()
            .get_mut(&buf1)
            .unwrap()
            .buffer
            .rename_file_path(std::path::PathBuf::from("aaa_long_name_01.txt"));
        let buf2 = editor.new_buffer();
        editor
            .buffers_mut()
            .get_mut(&buf2)
            .unwrap()
            .buffer
            .rename_file_path(std::path::PathBuf::from("bbb_long_name_02.txt"));
        let buf3 = editor.new_buffer();
        editor
            .buffers_mut()
            .get_mut(&buf3)
            .unwrap()
            .buffer
            .rename_file_path(std::path::PathBuf::from("ccc_long_name_03.txt"));

        {
            use crate::view::split::TabTarget;
            let view_state = editor.split_view_states_mut().get_mut(&split_id).unwrap();
            view_state.open_buffers = vec![
                TabTarget::Buffer(buf1),
                TabTarget::Buffer(buf2),
                TabTarget::Buffer(buf3),
            ];
            view_state.tab_scroll_offset = 50;
        }

        // Force active buffer to first tab and ensure helper brings it into view.
        // Note: available_width must be >= tab width (2 + name_len) for offset to be 0
        // Tab width = 2 + 20 (name length) = 22, so we need at least 22
        editor
            .active_window_mut()
            .ensure_active_tab_visible(split_id, buf1, 25);
        assert_eq!(
            editor
                .split_view_states()
                .get(&split_id)
                .unwrap()
                .tab_scroll_offset,
            0
        );

        // Now make the last tab active and ensure offset moves forward but stays bounded.
        editor
            .active_window_mut()
            .ensure_active_tab_visible(split_id, buf3, 25);
        let view_state = editor.split_view_states().get(&split_id).unwrap();
        assert!(view_state.tab_scroll_offset > 0);
        let buffer_ids: Vec<_> = view_state.buffer_tab_ids_vec();
        let total_width: usize = buffer_ids
            .iter()
            .enumerate()
            .map(|(idx, id)| {
                let state = editor.buffers().get(id).unwrap();
                let name_len = state
                    .buffer
                    .file_path()
                    .and_then(|p| p.file_name())
                    .and_then(|n| n.to_str())
                    .map(|s| s.chars().count())
                    .unwrap_or(0);
                let tab_width = 2 + name_len;
                if idx < buffer_ids.len() - 1 {
                    tab_width + 1 // separator
                } else {
                    tab_width
                }
            })
            .sum();
        assert!(view_state.tab_scroll_offset <= total_width);
    }

    /// Regression for sinelaw/fresh#2229.
    ///
    /// When the tab strip is scrolled (many tabs open) and the user invokes
    /// "Close Others" / "Close to Left" / "Close to Right" / "Close All",
    /// the surviving active tab must stay on-screen. Before the fix the
    /// scroll offset was carried over from the pre-close state, so the only
    /// remaining tab sat to the left of the viewport and the tab bar
    /// looked empty.
    #[test]
    fn close_others_re_anchors_tab_scroll_to_surviving_tab() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            test_filesystem(),
        )
        .unwrap();
        let split_id = editor.split_manager().active_split();

        // Open enough long-named buffers that the strip would scroll on a
        // realistic terminal width.
        use crate::view::split::TabTarget;
        let mut buffers = Vec::new();
        for i in 0..6 {
            let id = editor.new_buffer();
            editor
                .buffers_mut()
                .get_mut(&id)
                .unwrap()
                .buffer
                .rename_file_path(std::path::PathBuf::from(format!(
                    "long_filename_for_tab_{:02}.txt",
                    i
                )));
            buffers.push(id);
        }

        // Make the last buffer the active one and seed a scrolled offset
        // — what the renderer would have computed mid-session — then mark
        // it as the surviving "keep" tab.
        let keep = buffers[5];
        editor
            .active_window_mut()
            .split_manager_mut()
            .unwrap()
            .set_split_buffer(split_id, keep);
        {
            let view_state = editor.split_view_states_mut().get_mut(&split_id).unwrap();
            view_state.open_buffers = buffers
                .iter()
                .map(|b| TabTarget::Buffer(*b))
                .collect::<Vec<_>>();
            view_state.tab_scroll_offset = 80;
        }

        editor.close_other_tabs_in_split(keep, split_id);

        // Only the kept tab remains. Its visual range is [0, tab_width)
        // and it must be inside the viewport — i.e. the offset must fit
        // within the total tab strip width.
        let view_state = editor.split_view_states().get(&split_id).unwrap();
        let remaining_targets = view_state.buffer_tab_ids_vec();
        assert_eq!(
            remaining_targets,
            vec![keep],
            "Close Others should leave only the kept buffer"
        );
        let name_len = editor
            .buffers()
            .get(&keep)
            .unwrap()
            .buffer
            .file_path()
            .and_then(|p| p.file_name())
            .and_then(|n| n.to_str())
            .map(|s| s.chars().count())
            .unwrap_or(0);
        let kept_tab_width = 2 + name_len;
        assert!(
            view_state.tab_scroll_offset < kept_tab_width,
            "scroll offset {} must keep the {}-wide kept tab in view (without the fix it stays at 80)",
            view_state.tab_scroll_offset,
            kept_tab_width,
        );
    }
}
