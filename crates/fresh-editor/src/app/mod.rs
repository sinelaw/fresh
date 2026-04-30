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
pub mod lsp_auto_prompt;
mod lsp_event_notify;
mod lsp_requests;
mod lsp_status;
mod macro_actions;
mod macros;
mod menu_actions;
mod menu_context;
mod mouse_input;
mod navigation;
mod on_save_actions;
mod path_utils;
mod plugin_commands;
mod plugin_dispatch;
mod popup_actions;
mod popup_dialogs;
mod popup_overlay_actions;
mod prompt_actions;
mod prompt_lifecycle;
mod recovery_actions;
mod regex_replace;
mod render;
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
mod terminal_input;
mod terminal_mouse;
mod text_ops;
mod theme_inspect;
mod toggle_actions;
pub mod types;
mod undo_actions;
mod view_actions;
mod virtual_buffers;
pub mod warning_domains;
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

    let async_messages = {
        let _s = tracing::info_span!("process_async_messages").entered();
        editor.process_async_messages()
    };
    if async_messages {
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
    if editor.check_semantic_highlight_timer() {
        needs_render = true;
    }
    if editor.check_completion_trigger_timer() {
        needs_render = true;
    }
    editor.check_diagnostic_pull_timer();
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

pub(crate) use path_utils::normalize_path;

use self::types::{
    CachedLayout, FileExplorerContextMenu, InteractiveReplaceState, LspMessageEntry,
    LspProgressInfo, MouseState, SearchState, TabContextMenu, DEFAULT_BACKGROUND_FILE,
};
use crate::config::Config;
use crate::config_io::DirectoryContext;
use crate::input::buffer_mode::ModeRegistry;
use crate::input::command_registry::CommandRegistry;
use crate::input::keybindings::{Action, KeyContext, KeybindingResolver};
use crate::input::position_history::PositionHistory;
use crate::input::quick_open::{
    BufferProvider, CommandProvider, FileProvider, GotoLineProvider, QuickOpenRegistry,
};
use crate::model::cursor::Cursors;
use crate::model::event::{Event, EventLog, LeafId, SplitDirection};
use crate::model::filesystem::FileSystem;
use crate::services::async_bridge::{AsyncBridge, AsyncMessage};
use crate::services::fs::FsManager;
use crate::services::lsp::manager::LspManager;
use crate::services::plugins::PluginManager;
use crate::services::recovery::{RecoveryConfig, RecoveryService};
use crate::services::time_source::{RealTimeSource, SharedTimeSource};
use crate::state::EditorState;
use crate::types::{LspLanguageConfig, LspServerConfig, ProcessLimits};
use crate::view::file_tree::{FileTree, FileTreeView};
use crate::view::prompt::{Prompt, PromptType};
use crate::view::scroll_sync::ScrollSyncManager;
use crate::view::split::{SplitManager, SplitViewState};
use crate::view::ui::{
    FileExplorerRenderer, SplitRenderer, StatusBarRenderer, SuggestionsRenderer,
};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::{
    layout::{Constraint, Direction, Layout},
    Frame,
};
use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
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
struct SemanticTokenRangeRequest {
    buffer_id: BufferId,
    version: u64,
    range: Range<usize>,
    start_line: usize,
    end_line: usize,
}

#[derive(Clone, Copy, Debug)]
enum SemanticTokensFullRequestKind {
    Full,
    FullDelta,
}

#[derive(Clone, Debug)]
struct SemanticTokenFullRequest {
    buffer_id: BufferId,
    version: u64,
    kind: SemanticTokensFullRequestKind,
}

#[derive(Clone, Debug)]
struct FoldingRangeRequest {
    buffer_id: BufferId,
    version: u64,
}

#[derive(Clone, Debug)]
struct InlayHintsRequest {
    buffer_id: BufferId,
    version: u64,
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
    pub sticky_column: usize,
    pub viewport_top_byte: usize,
    pub viewport_top_view_line_offset: usize,
    pub viewport_left_column: usize,
    pub last_jump_position: usize,
}

/// The main editor struct - manages multiple buffers, clipboard, and rendering
pub struct Editor {
    /// All open buffers
    buffers: HashMap<BufferId, EditorState>,

    // NOTE: There is no `active_buffer` field. The active buffer is derived from
    // `split_manager.active_buffer_id()` to maintain a single source of truth.
    // Use `self.active_buffer()` to get the active buffer ID.
    /// Event log per buffer (for undo/redo)
    event_logs: HashMap<BufferId, EventLog>,

    /// Next buffer ID to assign
    next_buffer_id: usize,

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

    /// Cancellation flag for the current streaming grep search.
    streaming_grep_cancellation: Option<std::sync::Arc<std::sync::atomic::AtomicBool>>,

    /// Plugin callback IDs waiting for the grammar build to complete.
    /// Multiple reloadGrammars() calls may accumulate here; all are resolved
    /// when the background build finishes.
    pending_grammar_callbacks: Vec<fresh_core::api::JsCallbackId>,

    /// Active theme
    theme: crate::view::theme::Theme,

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

    /// Status message (shown in status bar)
    status_message: Option<String>,

    /// Plugin-provided status message (displayed alongside the core status)
    plugin_status_message: Option<String>,

    /// Last terminal window title written via OSC 2. Used so we only write
    /// the escape sequence when the title would actually change, rather
    /// than on every frame.
    last_window_title: Option<String>,

    /// Accumulated plugin errors (for test assertions)
    /// These are collected when plugin error messages are received
    plugin_errors: Vec<String>,

    /// Active prompt (minibuffer)
    prompt: Option<Prompt>,

    /// Terminal dimensions (for creating new buffers)
    terminal_width: u16,
    terminal_height: u16,

    /// LSP manager
    lsp: Option<LspManager>,

    /// Metadata for each buffer (file paths, LSP status, etc.)
    buffer_metadata: HashMap<BufferId, BufferMetadata>,

    /// Buffer mode registry (for buffer-local keybindings)
    mode_registry: ModeRegistry,

    /// Tokio runtime for async I/O tasks
    tokio_runtime: Option<tokio::runtime::Runtime>,

    /// Bridge for async messages from tokio tasks to main loop
    async_bridge: Option<AsyncBridge>,

    /// Split view manager
    split_manager: SplitManager,

    /// Per-split view state (cursors and viewport for each split)
    /// This allows multiple splits showing the same buffer to have independent
    /// cursor positions and scroll positions
    split_view_states: HashMap<LeafId, SplitViewState>,

    /// Previous viewport states for viewport_changed hook detection
    /// Stores (top_byte, width, height) from the end of the last render frame
    /// Used to detect viewport changes that occur between renders (e.g., scroll events)
    previous_viewports: HashMap<LeafId, (usize, u16, u16)>,

    /// Scroll sync manager for anchor-based synchronized scrolling
    /// Used for side-by-side diff views where two panes need to scroll together
    scroll_sync_manager: ScrollSyncManager,

    /// File explorer view (optional, only when open)
    file_explorer: Option<FileTreeView>,

    /// Buffer currently opened in "preview" (ephemeral) mode, together with
    /// the split (pane) it lives in. At most one preview exists editor-wide.
    ///
    /// Invariants:
    /// - The `is_preview` flag on the referenced buffer's metadata is true
    ///   iff this tuple is `Some` and points at that buffer.
    /// - The preview is **anchored to the split it was opened in**. Moving
    ///   focus to a different split, splitting the layout, or closing the
    ///   hosting split promotes the preview to a permanent tab first, so
    ///   layout manipulations never silently destroy the tab the user was
    ///   reading.
    /// - Cleared when the buffer is closed or promoted (edit / double-click
    ///   / tab-click / explicit Enter in the explorer).
    preview: Option<(LeafId, BufferId)>,

    /// One-shot flag: when true, the next `open_file` call skips writing to
    /// the back/forward position history. Set by `open_file_preview` so a
    /// string of exploratory single-clicks doesn't flood the history stack
    /// with entries pointing at tabs that are about to be closed.
    suppress_position_history_once: bool,

    /// Filesystem manager for file explorer
    fs_manager: Arc<FsManager>,

    /// Single backend slot for "where does the editor act?".
    ///
    /// Bundles filesystem, process spawner, terminal wrapper, and
    /// display label. Replaces the old quartet of `filesystem`,
    /// `process_spawner`, `terminal_wrapper`, `authority_display_string`
    /// fields. Always present; the editor boots with `Authority::local()`
    /// and plugins (or the SSH startup flow) install a different one
    /// later via `install_authority`. Pointer-equality on the inner
    /// `Arc`s answers "still the same backend?".
    authority: crate::services::authority::Authority,

    /// Authority queued by `install_authority`, picked up by `main.rs`
    /// right before dropping this editor on restart. `None` in the
    /// steady state. Not durable state — restarts from `main.rs`'s
    /// restart-dir path leave this `None`, and the main loop carries
    /// the authority over through its own channel.
    pending_authority: Option<crate::services::authority::Authority>,

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

    /// Whether file explorer is visible
    file_explorer_visible: bool,

    /// Whether file explorer is being synced to active file (async operation in progress)
    /// When true, we still render the file explorer area even if file_explorer is temporarily None
    file_explorer_sync_in_progress: bool,

    /// File explorer width: either a percent of the terminal width or
    /// an absolute column count. Runtime value, may be modified by
    /// dragging the divider (drag preserves the active variant).
    file_explorer_width: crate::config::ExplorerWidth,

    /// File explorer side placement (left or right)
    file_explorer_side: crate::config::FileExplorerSide,

    /// Pending show_hidden setting to apply when file explorer is initialized (from session restore)
    pending_file_explorer_show_hidden: Option<bool>,

    /// Pending show_gitignored setting to apply when file explorer is initialized (from session restore)
    pending_file_explorer_show_gitignored: Option<bool>,

    /// File explorer decorations by namespace
    file_explorer_decorations: HashMap<String, Vec<crate::view::file_tree::FileExplorerDecoration>>,

    /// Cached file explorer decorations (resolved + bubbled)
    file_explorer_decoration_cache: crate::view::file_tree::FileExplorerDecorationCache,

    /// File explorer clipboard for cut/copy/paste of files and directories
    pub(crate) file_explorer_clipboard: Option<crate::app::file_explorer::FileExplorerClipboard>,

    /// Whether menu bar is visible
    menu_bar_visible: bool,

    /// Whether menu bar was auto-shown (temporarily visible due to menu activation)
    /// When true, the menu bar will be hidden again when the menu is closed
    menu_bar_auto_shown: bool,

    /// Whether tab bar is visible
    tab_bar_visible: bool,

    /// Whether status bar is visible
    status_bar_visible: bool,

    /// Whether prompt line is visible (when no prompt is active)
    prompt_line_visible: bool,

    /// Whether mouse capture is enabled
    mouse_enabled: bool,

    /// Whether same-buffer splits sync their scroll positions
    same_buffer_scroll_sync: bool,

    /// Mouse cursor position (for GPM software cursor rendering)
    /// When GPM is active, we need to draw our own cursor since GPM can't
    /// draw on the alternate screen buffer used by TUI applications.
    mouse_cursor_position: Option<(u16, u16)>,

    /// Whether GPM is being used for mouse input (requires software cursor)
    gpm_active: bool,

    /// Current keybinding context
    key_context: KeyContext,

    /// Menu state (active menu, highlighted item)
    menu_state: crate::view::ui::MenuState,

    /// Menu configuration (built-in menus with i18n support)
    menus: crate::config::MenuConfig,

    /// Working directory for file explorer (set at initialization)
    working_dir: PathBuf,

    /// Position history for back/forward navigation
    pub position_history: PositionHistory,

    /// Flag to prevent recording movements during navigation
    in_navigation: bool,

    /// Next LSP request ID
    next_lsp_request_id: u64,

    /// Pending LSP completion request IDs (supports multiple servers)
    pending_completion_requests: HashSet<u64>,

    /// Original LSP completion items (for type-to-filter)
    /// Stored when completion popup is shown, used for re-filtering as user types
    completion_items: Option<Vec<lsp_types::CompletionItem>>,

    /// Scheduled completion trigger time (for debounced quick suggestions)
    /// When Some, completion will be triggered when this instant is reached
    scheduled_completion_trigger: Option<Instant>,

    /// Pluggable completion service that orchestrates multiple providers
    /// (dabbrev, buffer words, LSP, plugin providers).
    completion_service: crate::services::completion::CompletionService,

    /// Dabbrev cycling state: when the user presses Alt+/ repeatedly, we
    /// cycle through candidates without a popup. `None` when not in a
    /// dabbrev session. Reset when any other action is taken.
    dabbrev_state: Option<DabbrevCycleState>,

    /// Pending LSP go-to-definition request ID (if any)
    pending_goto_definition_request: Option<u64>,

    /// Pending LSP find references request ID (if any)
    pending_references_request: Option<u64>,

    /// Symbol name for pending references request
    pending_references_symbol: String,

    /// Pending LSP signature help request ID (if any)
    pending_signature_help_request: Option<u64>,

    /// Pending LSP code actions request IDs (supports merging from multiple servers)
    pending_code_actions_requests: HashSet<u64>,

    /// Maps pending code action request IDs to server names for attribution
    pending_code_actions_server_names: HashMap<u64, String>,

    /// Stored code actions from the most recent LSP response, used when the
    /// user selects an action from the code-action popup.
    /// Each entry is (server_name, action).
    pending_code_actions: Option<Vec<(String, lsp_types::CodeActionOrCommand)>>,

    /// Pending LSP inlay hints requests keyed by request id. Each entry
    /// carries the originating buffer and the buffer version at dispatch
    /// time so:
    ///   * Responses for multiple concurrent buffer requests (quiescent,
    ///     manual restart, batched saves) are each accepted individually
    ///     instead of clobbering a single shared slot.
    ///   * Responses that race behind a local edit (buffer version moved
    ///     past what we asked about) are dropped rather than applied at
    ///     the wrong offsets. Same pattern as `pending_folding_range_requests`
    ///     and `pending_semantic_token_requests`.
    pending_inlay_hints_requests: HashMap<u64, InlayHintsRequest>,

    /// Pending LSP folding range requests keyed by request ID
    pending_folding_range_requests: HashMap<u64, FoldingRangeRequest>,

    /// Track folding range requests per buffer to prevent duplicate inflight requests
    folding_ranges_in_flight: HashMap<BufferId, (u64, u64)>,

    /// Next time a folding range refresh is allowed for a buffer
    folding_ranges_debounce: HashMap<BufferId, Instant>,

    /// Pending semantic token requests keyed by LSP request ID
    pending_semantic_token_requests: HashMap<u64, SemanticTokenFullRequest>,

    /// Track semantic token requests per buffer to prevent duplicate inflight requests
    semantic_tokens_in_flight: HashMap<BufferId, (u64, u64, SemanticTokensFullRequestKind)>,

    /// Pending semantic token range requests keyed by LSP request ID
    pending_semantic_token_range_requests: HashMap<u64, SemanticTokenRangeRequest>,

    /// Track semantic token range requests per buffer (request_id, start_line, end_line, version)
    semantic_tokens_range_in_flight: HashMap<BufferId, (u64, usize, usize, u64)>,

    /// Track last semantic token range request per buffer (start_line, end_line, version, time)
    semantic_tokens_range_last_request: HashMap<BufferId, (usize, usize, u64, Instant)>,

    /// Track last applied semantic token range per buffer (start_line, end_line, version)
    semantic_tokens_range_applied: HashMap<BufferId, (usize, usize, u64)>,

    /// Next time a full semantic token refresh is allowed for a buffer
    semantic_tokens_full_debounce: HashMap<BufferId, Instant>,

    /// Hover subsystem (pending LSP request correlation, highlighted-symbol
    /// range + overlay handle, popup screen position).
    hover: hover::HoverState,

    /// Search state (if search is active)
    search_state: Option<SearchState>,

    /// Search highlight namespace (for efficient bulk removal)
    search_namespace: crate::view::overlay::OverlayNamespace,

    /// LSP diagnostic namespace (for filtering and bulk removal)
    lsp_diagnostic_namespace: crate::view::overlay::OverlayNamespace,

    /// Pending search range that should be reused when the next search is confirmed
    pending_search_range: Option<Range<usize>>,

    /// Interactive replace state (if interactive replace is active)
    interactive_replace_state: Option<InteractiveReplaceState>,

    /// Mouse state for scrollbar dragging
    mouse_state: MouseState,

    /// Tab context menu state (right-click on tabs)
    tab_context_menu: Option<TabContextMenu>,

    /// File explorer context menu state (right-click in file explorer)
    file_explorer_context_menu: Option<FileExplorerContextMenu>,

    /// Theme inspector popup state (Ctrl+Right-Click)
    theme_info_popup: Option<types::ThemeInfoPopup>,

    /// Cached layout areas from last render (for mouse hit testing)
    pub(crate) cached_layout: CachedLayout,

    /// Command registry for dynamic commands
    command_registry: Arc<RwLock<CommandRegistry>>,

    /// Quick Open registry for unified prompt providers
    quick_open_registry: QuickOpenRegistry,

    /// Plugin manager (handles both enabled and disabled cases)
    plugin_manager: PluginManager,

    /// Active plugin development workspaces (buffer_id → workspace)
    /// These provide LSP support for plugin buffers by creating temp directories
    /// with fresh.d.ts and tsconfig.json
    plugin_dev_workspaces:
        HashMap<BufferId, crate::services::plugins::plugin_dev_workspace::PluginDevWorkspace>,

    /// Track which byte ranges have been seen per buffer (for lines_changed optimization)
    /// Maps buffer_id -> set of (byte_start, byte_end) ranges that have been processed
    /// Using byte ranges instead of line numbers makes this agnostic to line number shifts
    seen_byte_ranges: HashMap<BufferId, std::collections::HashSet<(usize, usize)>>,

    /// Named panel IDs mapping (for idempotent panel operations)
    /// Maps panel ID (e.g., "diagnostics") to buffer ID
    panel_ids: HashMap<String, BufferId>,

    /// Live Grep "Return to Work" cache. Holds the prior query and
    /// selected index so `Action::ResumeLiveGrep` can re-open the
    /// floating overlay (issue #1796) with the same state. Cleared
    /// only when the user starts a fundamentally different search.
    /// `cached_results` is a *display* cache — Resume reuses it
    /// without re-running ripgrep. Editing the query invalidates it.
    pub(crate) live_grep_last_state: Option<crate::services::live_grep_state::LiveGrepLastState>,

    /// Phantom leaf id for the Live Grep floating overlay's preview
    /// pane (issue #1796). The leaf is *not* part of `SplitManager`'s
    /// tree — we allocate a unique `LeafId` and a parallel
    /// `SplitViewState` keyed by that id, then call the same per-leaf
    /// rendering pipeline regular splits use. This gives the preview
    /// real syntax highlighting, gutter, scroll, fold, etc., without
    /// mutating the user's split layout. `None` when the overlay is
    /// not open.
    pub(crate) overlay_preview_leaf: Option<crate::model::event::LeafId>,

    /// Buffer currently displayed in the overlay preview pane (the
    /// `active_buffer` of `overlay_preview_leaf`'s `SplitViewState`).
    /// Tracked here so we can detect when selection navigates to a
    /// different file and swap the leaf's buffer.
    pub(crate) overlay_preview_buffer: Option<BufferId>,

    /// Buffers we loaded only to feed the overlay preview pane. On
    /// overlay close we close any of these that aren't held by
    /// another split. Buffers the user already had open are *not*
    /// added here, so closing the overlay never disturbs the user's
    /// workspace state.
    pub(crate) overlay_preview_loaded_buffers: std::collections::HashSet<BufferId>,

    /// Buffer groups: multiple splits/buffers appearing as one tab
    buffer_groups: HashMap<types::BufferGroupId, types::BufferGroup>,
    /// Reverse index: buffer ID → group ID (for lookups)
    buffer_to_group: HashMap<BufferId, types::BufferGroupId>,
    /// Next buffer group ID
    next_buffer_group_id: usize,

    /// Grouped SplitNode subtrees, keyed by their LeafId (which is what
    /// `TabTarget::Group(leaf_id)` references). Each entry is a
    /// `SplitNode::Grouped` node holding the layout for one buffer group.
    ///
    /// These subtrees are NOT part of the main split tree — they live
    /// here and are dispatched to at render time when the current split's
    /// active target is a `TabTarget::Group`.
    pub(crate) grouped_subtrees:
        HashMap<crate::model::event::LeafId, crate::view::split::SplitNode>,

    /// Background process abort handles for cancellation
    /// Maps process_id to abort handle
    background_process_handles: HashMap<u64, tokio::task::AbortHandle>,

    /// Cancellation senders for host-side processes spawned via
    /// `spawnHostProcess`. Firing the sender (or dropping it) triggers
    /// an in-task `child.start_kill()` so the process is reaped, not
    /// just orphaned. Entries are removed when the spawn task sends
    /// its terminal `PluginProcessOutput`.
    host_process_handles: HashMap<u64, tokio::sync::oneshot::Sender<()>>,

    /// Prompt histories keyed by prompt type name (e.g., "search", "replace", "goto_line", "plugin:custom_name")
    /// This provides a generic history system that works for all prompt types including plugin prompts.
    prompt_histories: HashMap<String, crate::input::input_history::InputHistory>,

    /// Pending async prompt callback ID (for editor.prompt() API)
    /// When the prompt is confirmed, the callback is resolved with the input text.
    /// When cancelled, the callback is resolved with null.
    pending_async_prompt_callback: Option<fresh_core::api::JsCallbackId>,

    /// FIFO queue of plugin `editor.getNextKey()` callbacks awaiting a
    /// keypress. While non-empty, the next key arriving in
    /// `handle_key` is consumed by resolving the front-most callback
    /// rather than dispatching to mode bindings or other handlers.
    pending_next_key_callbacks: std::collections::VecDeque<fresh_core::api::JsCallbackId>,

    /// `true` while a plugin is in a `getNextKey()` loop and has
    /// declared (via `editor.beginKeyCapture()`) that it wants every
    /// key delivered, in order, regardless of timing.  Keys arriving
    /// while no callback is pending are buffered in
    /// `pending_key_capture_buffer` instead of dispatched.  Closes the
    /// race where fast typing or paste outruns the plugin's re-arm.
    key_capture_active: bool,

    /// Keys that arrived while `key_capture_active` was set but no
    /// `getNextKey()` callback was pending. Drained on the next
    /// `AwaitNextKey` (resolved immediately, in order). Cleared when
    /// the plugin ends capture.
    pending_key_capture_buffer: std::collections::VecDeque<fresh_core::api::KeyEventPayload>,

    /// Snapshot of cursor/viewport state saved when a goto-line preview jump
    /// moves the cursor live as the user types a target line. Used by both the
    /// Quick Open `:N` syntax and the standalone `Goto Line` prompt. Restored
    /// on cancel or when the user clears the target from the input.
    goto_line_preview: Option<GotoLinePreviewSnapshot>,

    /// LSP progress tracking (token -> progress info)
    lsp_progress: std::collections::HashMap<String, LspProgressInfo>,

    /// LSP server statuses ((language, server_name) -> status)
    lsp_server_statuses:
        std::collections::HashMap<(String, String), crate::services::async_bridge::LspServerStatus>,

    /// LSP window messages (recent messages from window/showMessage)
    lsp_window_messages: Vec<LspMessageEntry>,

    /// LSP log messages (recent messages from window/logMessage)
    lsp_log_messages: Vec<LspMessageEntry>,

    /// Diagnostic result IDs per URI (for incremental pull diagnostics)
    /// Maps URI string to last result_id received from server
    diagnostic_result_ids: HashMap<String, String>,

    /// Scheduled diagnostic pull time per buffer (debounced after didChange)
    /// When set, diagnostics will be re-pulled when this instant is reached
    scheduled_diagnostic_pull: Option<(BufferId, Instant)>,

    /// Scheduled inlay hints refresh time per buffer (debounced after didChange)
    /// When set, inlay hints will be re-requested when this instant is reached
    scheduled_inlay_hints_request: Option<(BufferId, Instant)>,

    /// Stored LSP diagnostics per URI, per server (push model - publishDiagnostics)
    /// Outer key: URI string, Inner key: server name
    stored_push_diagnostics: HashMap<String, HashMap<String, Vec<lsp_types::Diagnostic>>>,

    /// Stored LSP diagnostics per URI (pull model - native RA diagnostics)
    stored_pull_diagnostics: HashMap<String, Vec<lsp_types::Diagnostic>>,

    /// Merged view of push + pull diagnostics per URI (for plugin access).
    /// `Arc` wrapper: snapshot refresh is a refcount bump, and mutation is
    /// forced through `Arc::make_mut` which CoW-clones while the snapshot
    /// still references the previous map.
    stored_diagnostics: Arc<HashMap<String, Vec<lsp_types::Diagnostic>>>,

    /// Stored LSP folding ranges per URI
    /// Maps file URI string to Vec of folding ranges for that file
    stored_folding_ranges: Arc<HashMap<String, Vec<lsp_types::FoldingRange>>>,

    /// Event broadcaster for control events (observable by external systems)
    event_broadcaster: crate::model::control_event::EventBroadcaster,

    /// Bookmarks (character key -> bookmark)
    bookmarks: bookmarks::BookmarkState,

    /// Global search options (persist across searches)
    search_case_sensitive: bool,
    search_whole_word: bool,
    search_use_regex: bool,
    /// Whether to confirm each replacement (interactive/query-replace mode)
    search_confirm_each: bool,

    /// Macro record/playback subsystem (owns `macros`, `recording`,
    /// `last_register`, and the `playing` guard flag).
    macros: macros::MacroState,

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
    chord_state: Vec<(crossterm::event::KeyCode, crossterm::event::KeyModifiers)>,

    // (Historical `pending_lsp_confirmation` and `pending_lsp_status_popup`
    // fields moved onto `Popup::resolver` — each popup carries its own
    // "how do I confirm?" identity, so `handle_popup_confirm` dispatches
    // by matching the focused popup's resolver instead of racing through
    // a precedence cascade of side-channel `Option`s that a second
    // simultaneously-open popup could steal.)
    /// Languages the user has interactively dismissed from the LSP popup.
    ///
    /// Separate from `LspServerConfig::enabled` (which is the persisted
    /// config flag) so we can keep the status-bar pill visible in a
    /// muted style — giving the user a re-enable surface without
    /// mutating their on-disk config. Session-scoped; dismissal does not
    /// survive editor restarts.
    user_dismissed_lsp_languages: std::collections::HashSet<String>,

    /// Languages for which the auto-start prompt has already been
    /// surfaced this session, so subsequent opens of files in the same
    /// language don't keep re-popping the popup. Cleared only by
    /// editor restart — the persisted `auto_start=true` flag is what
    /// silences the prompt across sessions.
    auto_start_prompted_languages: std::collections::HashSet<String>,

    /// Languages for which an auto-start prompt is queued, waiting
    /// for a buffer of that language to become active. The popup is
    /// shown lazily on render so it attaches to the currently-focused
    /// buffer — this keeps the prompt visible when a session restore
    /// opens several files of the same language back-to-back and the
    /// active buffer ends up being a later one. Once drained, the
    /// language moves to `auto_start_prompted_languages`.
    pending_auto_start_prompts: std::collections::HashSet<String>,

    /// Per-editor flag gating the LSP auto-prompt. Real sessions
    /// default this to `true` (see `lsp_auto_prompt::default_enabled`);
    /// test infrastructure flips the default to `false` in its ctor
    /// to keep the popup from stealing keystrokes from scenarios
    /// that don't care about LSP. Snapshotted at construction so
    /// parallel tests running in the same process don't race on a
    /// shared atomic.
    lsp_auto_prompt_enabled: bool,

    /// Pending close buffer - buffer to close after SaveFileAs completes
    /// Used when closing a modified buffer that needs to be saved first
    pending_close_buffer: Option<BufferId>,

    /// Whether auto-revert mode is enabled (automatically reload files when changed on disk)
    auto_revert_enabled: bool,

    /// Last time we polled for file changes (for auto-revert)
    last_auto_revert_poll: std::time::Instant,

    /// Last time we polled for directory changes (for file tree refresh)
    last_file_tree_poll: std::time::Instant,

    /// Whether we've resolved and seeded the .git/index path in dir_mod_times
    git_index_resolved: bool,

    /// Last known modification times for open files (for auto-revert)
    /// Maps file path to last known modification time
    file_mod_times: HashMap<PathBuf, std::time::SystemTime>,

    /// Last known modification times for expanded directories (for file tree refresh)
    /// Maps directory path to last known modification time
    dir_mod_times: HashMap<PathBuf, std::time::SystemTime>,

    /// Receiver for background file change poll results.
    /// When Some, a background metadata poll is in progress. Results arrive as
    /// `(path, Option<mtime>)` pairs — None means metadata() failed.
    #[allow(clippy::type_complexity)]
    pending_file_poll_rx:
        Option<std::sync::mpsc::Receiver<Vec<(PathBuf, Option<std::time::SystemTime>)>>>,

    /// Receiver for background directory change poll results.
    /// The tuple contains: (dir metadata results, optional git index mtime).
    #[allow(clippy::type_complexity)]
    pending_dir_poll_rx: Option<
        std::sync::mpsc::Receiver<(
            Vec<(
                crate::view::file_tree::NodeId,
                PathBuf,
                Option<std::time::SystemTime>,
            )>,
            Option<(PathBuf, std::time::SystemTime)>,
        )>,
    >,

    /// Tracks rapid file change events for debouncing
    /// Maps file path to (last event time, event count)
    file_rapid_change_counts: HashMap<PathBuf, (std::time::Instant, u32)>,

    /// File open dialog state (when PromptType::OpenFile is active)
    file_open_state: Option<file_open::FileOpenState>,

    /// Cached layout for file browser (for mouse hit testing)
    file_browser_layout: Option<crate::view::ui::FileBrowserLayout>,

    /// Recovery service for auto-recovery-save and crash recovery
    recovery_service: RecoveryService,

    /// Request a full terminal clear and redraw on the next frame
    full_redraw_requested: bool,

    /// Request the event loop to suspend the process (SIGTSTP on Unix).
    /// Consumed by the outer event loop after the current action returns.
    suspend_requested: bool,

    /// Time source for testable time operations
    time_source: SharedTimeSource,

    /// Last auto-recovery-save time for rate limiting
    last_auto_recovery_save: std::time::Instant,

    /// Last persistent auto-save time for rate limiting (disk)
    last_persistent_auto_save: std::time::Instant,

    /// Active custom contexts for command visibility
    /// Plugin-defined contexts like "config-editor" that control command availability
    active_custom_contexts: HashSet<String>,

    /// Plugin-managed global state, isolated per plugin name.
    /// Outer key is plugin name, inner key is the state key set by the plugin.
    plugin_global_state: HashMap<String, HashMap<String, serde_json::Value>>,

    /// Global editor mode for modal editing (e.g., "vi-normal", "vi-insert")
    /// When set, this mode's keybindings take precedence over normal key handling
    editor_mode: Option<String>,

    /// Warning log receiver and path (for tracking warnings)
    warning_log: Option<(std::sync::mpsc::Receiver<()>, PathBuf)>,

    /// Status message log path (for viewing full status history)
    status_log_path: Option<PathBuf>,

    /// Warning domain registry for extensible warning indicators
    /// Contains LSP warnings, general warnings, and can be extended by plugins
    warning_domains: WarningDomainRegistry,

    /// Periodic update checker (checks for new releases every hour)
    update_checker: Option<crate::services::release_checker::PeriodicUpdateChecker>,

    /// Terminal manager for built-in terminal support
    terminal_manager: crate::services::terminal::TerminalManager,

    /// Maps buffer ID to terminal ID (for terminal buffers)
    terminal_buffers: HashMap<BufferId, crate::services::terminal::TerminalId>,

    /// Maps terminal ID to backing file path (for terminal content storage)
    terminal_backing_files: HashMap<crate::services::terminal::TerminalId, std::path::PathBuf>,

    /// Maps terminal ID to raw log file path (full PTY capture)
    terminal_log_files: HashMap<crate::services::terminal::TerminalId, std::path::PathBuf>,

    /// Terminals that should not be persisted to the workspace session file.
    /// A terminal is in this set iff it was created with `persistent = false`
    /// (the default for plugin-created terminals). On workspace save these
    /// terminals are skipped; on close their backing/log files are removed.
    /// User-opened terminals are absent from this set and persist as before.
    ephemeral_terminals: std::collections::HashSet<crate::services::terminal::TerminalId>,

    /// Whether terminal mode is active (input goes to terminal)
    terminal_mode: bool,

    /// Whether keyboard capture is enabled in terminal mode.
    /// When true, ALL keys go to the terminal (except Ctrl+` to toggle).
    /// When false, UI keybindings (split nav, palette, etc.) are processed first.
    keyboard_capture: bool,

    /// Set of terminal buffer IDs that should auto-resume terminal mode when switched back to.
    /// When leaving a terminal while in terminal mode, its ID is added here.
    /// When switching to a terminal in this set, terminal mode is automatically re-entered.
    terminal_mode_resume: std::collections::HashSet<BufferId>,

    /// Timestamp of the previous mouse click (for multi-click detection)
    previous_click_time: Option<std::time::Instant>,

    /// Position of the previous mouse click (for multi-click detection)
    /// Multi-click is only detected if all clicks are at the same position
    previous_click_position: Option<(u16, u16)>,

    /// Click count for multi-click detection (1=single, 2=double, 3=triple)
    click_count: u8,

    /// Settings UI state (when settings modal is open)
    pub(crate) settings_state: Option<crate::view::settings::SettingsState>,

    /// Calibration wizard state (when calibration modal is open)
    pub(crate) calibration_wizard: Option<calibration_wizard::CalibrationWizard>,

    /// Event debug dialog state (when event debug modal is open)
    pub(crate) event_debug: Option<event_debug::EventDebug>,

    /// Keybinding editor state (when keybinding editor modal is open)
    pub(crate) keybinding_editor: Option<keybinding_editor::KeybindingEditor>,

    /// Key translator for input calibration (loaded from config)
    pub(crate) key_translator: crate::input::key_translator::KeyTranslator,

    /// Terminal color capability (true color, 256, or 16 colors)
    color_capability: crate::view::color_support::ColorCapability,

    /// Hunks for the Review Diff tool
    review_hunks: Vec<fresh_core::api::ReviewHunk>,

    /// Editor-level popups that float above any buffer regardless of which
    /// one is active. Plugin notifications (showActionPopup) live here so a
    /// switch to a virtual buffer (Dashboard, diagnostics panel, …) doesn't
    /// hide them mid-decision.
    ///
    /// Each plugin popup carries its `popup_id` inside its
    /// `PopupResolver::PluginAction` — no parallel side-channel stack.
    pub(crate) global_popups: crate::view::popup::PopupManager,

    /// Composite buffers (separate from regular buffers)
    /// These display multiple source buffers in a single tab
    composite_buffers: HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,

    /// View state for composite buffers (per split)
    /// Maps (split_id, buffer_id) to composite view state
    composite_view_states:
        HashMap<(LeafId, BufferId), crate::view::composite_view::CompositeViewState>,

    /// Pending file opens from CLI arguments (processed after TUI starts)
    /// This allows CLI files to go through the same code path as interactive file opens,
    /// ensuring consistent error handling (e.g., encoding confirmation prompts).
    pending_file_opens: Vec<PendingFileOpen>,

    /// When true, apply hot exit recovery after the next batch of pending file opens
    pending_hot_exit_recovery: bool,

    /// Tracks buffers opened with --wait: maps buffer_id → (wait_id, has_popup)
    wait_tracking: HashMap<BufferId, (u64, bool)>,
    /// Wait IDs that have completed (buffer closed or popup dismissed)
    completed_waits: Vec<u64>,

    /// Stdin streaming state (if reading from stdin)
    stdin_stream: stdin_stream::StdinStream,

    /// Incremental line scan state (for non-blocking progress during Go to Line)
    line_scan: line_scan::LineScan,

    /// Incremental search scan state (for non-blocking search on large files)
    search_scan: search_scan::SearchScan,

    /// Viewport top_byte when search overlays were last refreshed.
    /// Used to detect viewport scrolling so overlays can be updated.
    search_overlay_top_byte: Option<usize>,

    /// Frame-buffer animation layer. Applied at the end of `render`; the
    /// main loop consults `is_active`/`next_deadline` to keep re-rendering
    /// while animations are running.
    pub animations: crate::view::animation::AnimationRunner,

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
            self.working_dir.join(input_path)
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

    /// Calculate the effective width available for tabs.
    ///
    /// When the file explorer is visible, tabs only get a portion of the
    /// terminal width. Matches the layout calculation in render.rs.
    fn effective_tabs_width(&self) -> u16 {
        if self.file_explorer_visible && self.file_explorer.is_some() {
            let explorer = self.file_explorer_width.to_cols(self.terminal_width);
            self.terminal_width.saturating_sub(explorer)
        } else {
            self.terminal_width
        }
    }

    /// Get the currently active buffer state
    pub fn active_state(&self) -> &EditorState {
        self.buffers.get(&self.active_buffer()).unwrap()
    }

    /// Get the currently active buffer state (mutable)
    pub fn active_state_mut(&mut self) -> &mut EditorState {
        self.buffers.get_mut(&self.active_buffer()).unwrap()
    }

    /// Get the cursors for the active buffer in the active split.
    /// Uses `effective_active_split` so focused buffer-group panels return
    /// their own cursors (not the outer split's stale ones).
    pub fn active_cursors(&self) -> &Cursors {
        let split_id = self.effective_active_split();
        &self.split_view_states.get(&split_id).unwrap().cursors
    }

    /// Get the cursors for the active buffer in the active split (mutable)
    pub fn active_cursors_mut(&mut self) -> &mut Cursors {
        let split_id = self.effective_active_split();
        &mut self.split_view_states.get_mut(&split_id).unwrap().cursors
    }

    /// Set completion items for type-to-filter (for testing)
    pub fn set_completion_items(&mut self, items: Vec<lsp_types::CompletionItem>) {
        self.completion_items = Some(items);
    }

    /// Get the viewport for the active split
    pub fn active_viewport(&self) -> &crate::view::viewport::Viewport {
        let active_split = self.split_manager.active_split();
        &self.split_view_states.get(&active_split).unwrap().viewport
    }

    /// Get the viewport for the active split (mutable)
    pub fn active_viewport_mut(&mut self) -> &mut crate::view::viewport::Viewport {
        let active_split = self.split_manager.active_split();
        &mut self
            .split_view_states
            .get_mut(&active_split)
            .unwrap()
            .viewport
    }

    /// Get the display name for a buffer (filename or virtual buffer name)
    pub fn get_buffer_display_name(&self, buffer_id: BufferId) -> String {
        // Check composite buffers first
        if let Some(composite) = self.composite_buffers.get(&buffer_id) {
            return composite.name.clone();
        }

        self.buffer_metadata
            .get(&buffer_id)
            .map(|m| m.display_name.clone())
            .or_else(|| {
                self.buffers.get(&buffer_id).and_then(|state| {
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

    /// Apply an event to the active buffer with all cross-cutting concerns.
    /// This is the centralized method that automatically handles:
    /// - Event application to buffer
    /// - Plugin hooks (after-insert, after-delete, etc.)
    /// - LSP notifications
    /// - Any other cross-cutting concerns
    ///

    /// Get the event log for the active buffer
    pub fn active_event_log(&self) -> &EventLog {
        self.event_logs.get(&self.active_buffer()).unwrap()
    }

    /// Get the event log for the active buffer (mutable)
    pub fn active_event_log_mut(&mut self) -> &mut EventLog {
        self.event_logs.get_mut(&self.active_buffer()).unwrap()
    }

    /// Update the buffer's modified flag based on event log position
    /// Call this after undo/redo to correctly track whether the buffer
    /// has returned to its saved state
    pub(super) fn update_modified_from_event_log(&mut self) {
        let is_at_saved = self
            .event_logs
            .get(&self.active_buffer())
            .map(|log| log.is_at_saved_position())
            .unwrap_or(false);

        if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
            state.buffer.set_modified(!is_at_saved);
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

        assert_eq!(editor.buffers.len(), 1);
        assert!(!editor.should_quit());
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
        assert_eq!(editor.buffers.len(), 2);
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

        let events = editor.action_to_events(Action::InsertChar('a'));
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

        let events = editor.action_to_events(Action::MoveRight);
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
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Test move up
        let events = editor.action_to_events(Action::MoveUp);
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

        let events = editor.action_to_events(Action::InsertNewline);
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
        assert!(editor.action_to_events(Action::Save).is_none());
        assert!(editor.action_to_events(Action::Quit).is_none());
        assert!(editor.action_to_events(Action::Undo).is_none());
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

        let events = editor.action_to_events(Action::DeleteBackward);
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
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        let events = editor.action_to_events(Action::DeleteForward);
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
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        let events = editor.action_to_events(Action::SelectRight);
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

        let events = editor.action_to_events(Action::SelectAll);
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
        let events = editor.action_to_events(Action::MoveDocumentStart);
        assert!(events.is_some());
        let events = events.unwrap();
        match &events[0] {
            Event::MoveCursor { new_position, .. } => {
                assert_eq!(*new_position, 0);
            }
            _ => panic!("Expected MoveCursor event"),
        }

        // Test MoveDocumentEnd
        let events = editor.action_to_events(Action::MoveDocumentEnd);
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
        let events = editor.action_to_events(Action::RemoveSecondaryCursors);
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
        let events = editor.action_to_events(Action::ScrollUp);
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
        let events = editor.action_to_events(Action::ScrollDown);
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
        let events = editor.action_to_events(Action::None);
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
            old_sticky_column: 0,
            new_sticky_column: 0,
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
            old_sticky_column: 0,
            new_sticky_column: 0,
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
            old_sticky_column: 0,
            new_sticky_column: 0,
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
        editor.search_case_sensitive = false;
        editor.perform_search("hello");

        let search_state = editor.search_state.as_ref().unwrap();
        assert_eq!(
            search_state.matches.len(),
            3,
            "Should find all 3 matches case-insensitively"
        );

        // Test case-sensitive search
        editor.search_case_sensitive = true;
        editor.perform_search("hello");

        let search_state = editor.search_state.as_ref().unwrap();
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
        editor.search_whole_word = false;
        editor.search_case_sensitive = true;
        editor.perform_search("test");

        let search_state = editor.search_state.as_ref().unwrap();
        assert_eq!(
            search_state.matches.len(),
            5,
            "Should find 'test' in all occurrences"
        );

        // Test whole word match
        editor.search_whole_word = true;
        editor.perform_search("test");

        let search_state = editor.search_state.as_ref().unwrap();
        assert_eq!(
            search_state.matches.len(),
            2,
            "Should find only whole word 'test'"
        );
        assert_eq!(search_state.matches[0], 0, "First match at position 0");
        assert_eq!(search_state.matches[1], 27, "Second match at position 27");
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

        editor.search_scan.start(
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
            editor.search_scan.buffer_id(),
            None,
            "search_scan should be drained after capped scan completes"
        );

        // Search state should be set with the accumulated matches
        let search_state = editor
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
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Set bookmark '1'
        editor.set_bookmark('1');
        assert_eq!(editor.bookmarks.get('1').map(|b| b.position), Some(7));

        // Move cursor elsewhere
        editor.apply_event_to_active_buffer(&Event::MoveCursor {
            cursor_id,
            old_position: 7,
            new_position: 14,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Jump back to bookmark
        editor.jump_to_bookmark('1');
        assert_eq!(editor.active_cursors().primary().position, 7);

        // Clear bookmark
        editor.clear_bookmark('1');
        assert_eq!(editor.bookmarks.get('1'), None);
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
        let lsp_changes_before = editor.collect_lsp_changes(&batch);

        // Now apply the batch (this is what apply_events_to_buffer_as_bulk_edit does)
        editor.apply_event_to_active_buffer(&batch);

        // BUG DEMONSTRATION: Calculate LSP positions AFTER applying batch
        // This is what happens when notify_lsp_change is called after state.apply()
        let lsp_changes_after = editor.collect_lsp_changes(&batch);

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
        let lsp_changes1 = editor.collect_lsp_changes(&batch1);

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
        let lsp_changes2 = editor.collect_lsp_changes(&batch2);

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
        let split_id = editor.split_manager.active_split();

        // Create three buffers with long names to force scrolling.
        let buf1 = editor.new_buffer();
        editor
            .buffers
            .get_mut(&buf1)
            .unwrap()
            .buffer
            .rename_file_path(std::path::PathBuf::from("aaa_long_name_01.txt"));
        let buf2 = editor.new_buffer();
        editor
            .buffers
            .get_mut(&buf2)
            .unwrap()
            .buffer
            .rename_file_path(std::path::PathBuf::from("bbb_long_name_02.txt"));
        let buf3 = editor.new_buffer();
        editor
            .buffers
            .get_mut(&buf3)
            .unwrap()
            .buffer
            .rename_file_path(std::path::PathBuf::from("ccc_long_name_03.txt"));

        {
            use crate::view::split::TabTarget;
            let view_state = editor.split_view_states.get_mut(&split_id).unwrap();
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
        editor.ensure_active_tab_visible(split_id, buf1, 25);
        assert_eq!(
            editor
                .split_view_states
                .get(&split_id)
                .unwrap()
                .tab_scroll_offset,
            0
        );

        // Now make the last tab active and ensure offset moves forward but stays bounded.
        editor.ensure_active_tab_visible(split_id, buf3, 25);
        let view_state = editor.split_view_states.get(&split_id).unwrap();
        assert!(view_state.tab_scroll_offset > 0);
        let buffer_ids: Vec<_> = view_state.buffer_tab_ids_vec();
        let total_width: usize = buffer_ids
            .iter()
            .enumerate()
            .map(|(idx, id)| {
                let state = editor.buffers.get(id).unwrap();
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
}
