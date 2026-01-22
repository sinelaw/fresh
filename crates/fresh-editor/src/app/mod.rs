mod async_messages;
mod buffer_management;
mod calibration_actions;
pub mod calibration_wizard;
mod clipboard;
mod composite_buffer_actions;
mod file_explorer;
pub mod file_open;
mod file_open_input;
mod file_operations;
mod help;
mod input;
mod input_dispatch;
mod lsp_actions;
mod lsp_requests;
mod menu_actions;
mod menu_context;
mod mouse_input;
mod on_save_actions;
mod plugin_commands;
mod popup_actions;
mod prompt_actions;
mod recovery_actions;
mod render;
pub mod session;
mod settings_actions;
mod shell_command;
mod split_actions;
mod tab_drag;
mod terminal;
mod terminal_input;
mod toggle_actions;
pub mod types;
mod undo_actions;
mod view_actions;
pub mod warning_domains;

use anyhow::Result as AnyhowResult;
use rust_i18n::t;
use std::path::Component;

/// Normalize a path by resolving `.` and `..` components without requiring the path to exist.
/// This is similar to canonicalize but works on paths that don't exist yet.
pub(crate) fn normalize_path(path: &std::path::Path) -> std::path::PathBuf {
    let mut components = Vec::new();

    for component in path.components() {
        match component {
            Component::CurDir => {
                // Skip "." components
            }
            Component::ParentDir => {
                // Pop the last component if it's a normal component
                if let Some(Component::Normal(_)) = components.last() {
                    components.pop();
                } else {
                    // Keep ".." if we can't go up further (for relative paths)
                    components.push(component);
                }
            }
            _ => {
                components.push(component);
            }
        }
    }

    if components.is_empty() {
        std::path::PathBuf::from(".")
    } else {
        components.iter().collect()
    }
}

use self::types::{
    Bookmark, CachedLayout, EventLineInfo, InteractiveReplaceState, LspMessageEntry,
    LspProgressInfo, MacroRecordingState, MouseState, SearchState, TabContextMenu,
    DEFAULT_BACKGROUND_FILE,
};
use crate::config::Config;
use crate::config_io::{ConfigLayer, ConfigResolver, DirectoryContext};
use crate::input::actions::action_to_events as convert_action_to_events;
use crate::input::buffer_mode::ModeRegistry;
use crate::input::command_registry::CommandRegistry;
use crate::input::commands::Suggestion;
use crate::input::keybindings::{Action, KeyContext, KeybindingResolver};
use crate::input::position_history::PositionHistory;
use crate::model::event::{Event, EventLog, SplitDirection, SplitId};
use crate::services::async_bridge::{AsyncBridge, AsyncMessage};
use crate::services::fs::{FsBackend, FsManager, LocalFsBackend};
use crate::services::lsp::manager::{detect_language, LspManager};
use crate::services::plugins::PluginManager;
use crate::services::recovery::{RecoveryConfig, RecoveryService};
use crate::services::time_source::{RealTimeSource, SharedTimeSource};
use crate::state::EditorState;
use crate::types::LspServerConfig;
use crate::view::file_tree::{FileTree, FileTreeView};
use crate::view::prompt::{Prompt, PromptType};
use crate::view::scroll_sync::ScrollSyncManager;
use crate::view::split::{SplitManager, SplitViewState};
use crate::view::ui::{
    FileExplorerRenderer, SplitRenderer, StatusBarRenderer, SuggestionsRenderer,
};
use crossterm::event::{KeyCode, KeyModifiers};
#[cfg(feature = "plugins")]
use fresh_core::api::BufferSavedDiff;
use fresh_core::api::PluginCommand;
use lsp_types::{Position, Range as LspRange, TextDocumentContentChangeEvent};
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

/// Helper function to convert lsp_types::Uri to PathBuf
fn uri_to_path(uri: &lsp_types::Uri) -> Result<PathBuf, String> {
    // Convert to url::Url for path conversion
    url::Url::parse(uri.as_str())
        .map_err(|e| format!("Failed to parse URI: {}", e))?
        .to_file_path()
        .map_err(|_| "URI is not a file path".to_string())
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

    /// Configuration
    config: Config,

    /// Directory context for editor state paths
    dir_context: DirectoryContext,

    /// Grammar registry for TextMate syntax highlighting
    grammar_registry: std::sync::Arc<crate::primitives::grammar::GrammarRegistry>,

    /// Active theme
    theme: crate::view::theme::Theme,

    /// Optional ANSI background image
    ansi_background: Option<crate::primitives::ansi_background::AnsiBackground>,

    /// Source path for the currently loaded ANSI background
    ansi_background_path: Option<PathBuf>,

    /// Blend amount for the ANSI background (0..1)
    background_fade: f32,

    /// Keybinding resolver
    keybindings: KeybindingResolver,

    /// Shared clipboard (handles both internal and system clipboard)
    clipboard: crate::services::clipboard::Clipboard,

    /// Should the editor quit?
    should_quit: bool,

    /// If set, the editor should restart with this new working directory
    /// This is used by Open Folder to do a clean context switch
    restart_with_dir: Option<PathBuf>,

    /// Status message (shown in status bar)
    status_message: Option<String>,

    /// Plugin-provided status message (displayed alongside the core status)
    plugin_status_message: Option<String>,

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
    split_view_states: HashMap<SplitId, SplitViewState>,

    /// Previous viewport states for viewport_changed hook detection
    /// Stores (top_byte, width, height) from the end of the last render frame
    /// Used to detect viewport changes that occur between renders (e.g., scroll events)
    previous_viewports: HashMap<SplitId, (usize, u16, u16)>,

    /// Scroll sync manager for anchor-based synchronized scrolling
    /// Used for side-by-side diff views where two panes need to scroll together
    scroll_sync_manager: ScrollSyncManager,

    /// File explorer view (optional, only when open)
    file_explorer: Option<FileTreeView>,

    /// Filesystem manager for file explorer
    fs_manager: Arc<FsManager>,

    /// Whether file explorer is visible
    file_explorer_visible: bool,

    /// Whether file explorer is being synced to active file (async operation in progress)
    /// When true, we still render the file explorer area even if file_explorer is temporarily None
    file_explorer_sync_in_progress: bool,

    /// File explorer width as percentage (0.0 to 1.0)
    /// This is the runtime value that can be modified by dragging the border
    file_explorer_width_percent: f32,

    /// Pending show_hidden setting to apply when file explorer is initialized (from session restore)
    pending_file_explorer_show_hidden: Option<bool>,

    /// Pending show_gitignored setting to apply when file explorer is initialized (from session restore)
    pending_file_explorer_show_gitignored: Option<bool>,

    /// File explorer decorations by namespace
    file_explorer_decorations: HashMap<String, Vec<crate::view::file_tree::FileExplorerDecoration>>,

    /// Cached file explorer decorations (resolved + bubbled)
    file_explorer_decoration_cache: crate::view::file_tree::FileExplorerDecorationCache,

    /// Whether menu bar is visible
    menu_bar_visible: bool,

    /// Whether menu bar was auto-shown (temporarily visible due to menu activation)
    /// When true, the menu bar will be hidden again when the menu is closed
    menu_bar_auto_shown: bool,

    /// Whether tab bar is visible
    tab_bar_visible: bool,

    /// Whether mouse capture is enabled
    mouse_enabled: bool,

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

    /// Pending LSP completion request ID (if any)
    pending_completion_request: Option<u64>,

    /// Original LSP completion items (for type-to-filter)
    /// Stored when completion popup is shown, used for re-filtering as user types
    completion_items: Option<Vec<lsp_types::CompletionItem>>,

    /// Scheduled completion trigger time (for debounced quick suggestions)
    /// When Some, completion will be triggered when this instant is reached
    scheduled_completion_trigger: Option<Instant>,

    /// Pending LSP go-to-definition request ID (if any)
    pending_goto_definition_request: Option<u64>,

    /// Pending LSP hover request ID (if any)
    pending_hover_request: Option<u64>,

    /// Pending LSP find references request ID (if any)
    pending_references_request: Option<u64>,

    /// Symbol name for pending references request
    pending_references_symbol: String,

    /// Pending LSP signature help request ID (if any)
    pending_signature_help_request: Option<u64>,

    /// Pending LSP code actions request ID (if any)
    pending_code_actions_request: Option<u64>,

    /// Pending LSP inlay hints request ID (if any)
    pending_inlay_hints_request: Option<u64>,

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

    /// Hover symbol range (byte offsets) - for highlighting the symbol under hover
    /// Format: (start_byte_offset, end_byte_offset)
    hover_symbol_range: Option<(usize, usize)>,

    /// Hover symbol overlay handle (for removal)
    hover_symbol_overlay: Option<crate::view::overlay::OverlayHandle>,

    /// Mouse hover screen position for popup placement
    /// Set when a mouse-triggered hover request is sent
    mouse_hover_screen_position: Option<(u16, u16)>,

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

    /// LSP status indicator for status bar
    lsp_status: String,

    /// Mouse state for scrollbar dragging
    mouse_state: MouseState,

    /// Tab context menu state (right-click on tabs)
    tab_context_menu: Option<TabContextMenu>,

    /// Cached layout areas from last render (for mouse hit testing)
    pub(crate) cached_layout: CachedLayout,

    /// Command registry for dynamic commands
    command_registry: Arc<RwLock<CommandRegistry>>,

    /// Plugin manager (handles both enabled and disabled cases)
    plugin_manager: PluginManager,

    /// Track which byte ranges have been seen per buffer (for lines_changed optimization)
    /// Maps buffer_id -> set of (byte_start, byte_end) ranges that have been processed
    /// Using byte ranges instead of line numbers makes this agnostic to line number shifts
    seen_byte_ranges: HashMap<BufferId, std::collections::HashSet<(usize, usize)>>,

    /// Named panel IDs mapping (for idempotent panel operations)
    /// Maps panel ID (e.g., "diagnostics") to buffer ID
    panel_ids: HashMap<String, BufferId>,

    /// Background process abort handles for cancellation
    /// Maps process_id to abort handle
    background_process_handles: HashMap<u64, tokio::task::AbortHandle>,

    /// Prompt histories keyed by prompt type name (e.g., "search", "replace", "goto_line", "plugin:custom_name")
    /// This provides a generic history system that works for all prompt types including plugin prompts.
    prompt_histories: HashMap<String, crate::input::input_history::InputHistory>,

    /// Pending async prompt callback ID (for editor.prompt() API)
    /// When the prompt is confirmed, the callback is resolved with the input text.
    /// When cancelled, the callback is resolved with null.
    pending_async_prompt_callback: Option<fresh_core::api::JsCallbackId>,

    /// LSP progress tracking (token -> progress info)
    lsp_progress: std::collections::HashMap<String, LspProgressInfo>,

    /// LSP server statuses (language -> status)
    lsp_server_statuses:
        std::collections::HashMap<String, crate::services::async_bridge::LspServerStatus>,

    /// LSP window messages (recent messages from window/showMessage)
    lsp_window_messages: Vec<LspMessageEntry>,

    /// LSP log messages (recent messages from window/logMessage)
    lsp_log_messages: Vec<LspMessageEntry>,

    /// Diagnostic result IDs per URI (for incremental pull diagnostics)
    /// Maps URI string to last result_id received from server
    diagnostic_result_ids: HashMap<String, String>,

    /// Stored LSP diagnostics per URI
    /// Maps file URI string to Vec of diagnostics for that file
    stored_diagnostics: HashMap<String, Vec<lsp_types::Diagnostic>>,

    /// Event broadcaster for control events (observable by external systems)
    event_broadcaster: crate::model::control_event::EventBroadcaster,

    /// Bookmarks (character key -> bookmark)
    bookmarks: HashMap<char, Bookmark>,

    /// Global search options (persist across searches)
    search_case_sensitive: bool,
    search_whole_word: bool,
    search_use_regex: bool,
    /// Whether to confirm each replacement (interactive/query-replace mode)
    search_confirm_each: bool,

    /// Macro storage (key -> list of recorded actions)
    macros: HashMap<char, Vec<Action>>,

    /// Macro recording state (Some(key) if recording, None otherwise)
    macro_recording: Option<MacroRecordingState>,

    /// Last recorded macro register (for F12 to replay)
    last_macro_register: Option<char>,

    /// Flag to prevent recursive macro playback
    macro_playing: bool,

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

    /// Pending LSP confirmation - language name awaiting user confirmation
    /// When Some, a confirmation popup is shown asking user to approve LSP spawn
    pending_lsp_confirmation: Option<String>,

    /// Pending close buffer - buffer to close after SaveFileAs completes
    /// Used when closing a modified buffer that needs to be saved first
    pending_close_buffer: Option<BufferId>,

    /// Whether auto-revert mode is enabled (automatically reload files when changed on disk)
    auto_revert_enabled: bool,

    /// Last time we polled for file changes (for auto-revert)
    last_auto_revert_poll: std::time::Instant,

    /// Last time we polled for directory changes (for file tree refresh)
    last_file_tree_poll: std::time::Instant,

    /// Last known modification times for open files (for auto-revert)
    /// Maps file path to last known modification time
    file_mod_times: HashMap<PathBuf, std::time::SystemTime>,

    /// Last known modification times for expanded directories (for file tree refresh)
    /// Maps directory path to last known modification time
    dir_mod_times: HashMap<PathBuf, std::time::SystemTime>,

    /// Tracks rapid file change events for debouncing
    /// Maps file path to (last event time, event count)
    file_rapid_change_counts: HashMap<PathBuf, (std::time::Instant, u32)>,

    /// File open dialog state (when PromptType::OpenFile is active)
    file_open_state: Option<file_open::FileOpenState>,

    /// Cached layout for file browser (for mouse hit testing)
    file_browser_layout: Option<crate::view::ui::FileBrowserLayout>,

    /// Recovery service for auto-save and crash recovery
    recovery_service: RecoveryService,

    /// Request a full terminal clear and redraw on the next frame
    full_redraw_requested: bool,

    /// Time source for testable time operations
    time_source: SharedTimeSource,

    /// Last auto-save time for rate limiting
    last_auto_save: std::time::Instant,

    /// Active custom contexts for command visibility
    /// Plugin-defined contexts like "config-editor" that control command availability
    active_custom_contexts: HashSet<String>,

    /// Global editor mode for modal editing (e.g., "vi-normal", "vi-insert")
    /// When set, this mode's keybindings take precedence over normal key handling
    editor_mode: Option<String>,

    /// Warning log receiver and path (for tracking warnings)
    warning_log: Option<(std::sync::mpsc::Receiver<()>, PathBuf)>,

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

    /// Timestamp of the previous mouse click (for double-click detection)
    previous_click_time: Option<std::time::Instant>,

    /// Position of the previous mouse click (for double-click detection)
    /// Double-click is only detected if both clicks are at the same position
    previous_click_position: Option<(u16, u16)>,

    /// Settings UI state (when settings modal is open)
    pub(crate) settings_state: Option<crate::view::settings::SettingsState>,

    /// Calibration wizard state (when calibration modal is open)
    pub(crate) calibration_wizard: Option<calibration_wizard::CalibrationWizard>,

    /// Key translator for input calibration (loaded from config)
    pub(crate) key_translator: crate::input::key_translator::KeyTranslator,

    /// Terminal color capability (true color, 256, or 16 colors)
    color_capability: crate::view::color_support::ColorCapability,

    /// Hunks for the Review Diff tool
    review_hunks: Vec<fresh_core::api::ReviewHunk>,

    /// Active action popup (for plugin showActionPopup API)
    /// Stores (popup_id, Vec<(action_id, action_label)>)
    active_action_popup: Option<(String, Vec<(String, String)>)>,

    /// Composite buffers (separate from regular buffers)
    /// These display multiple source buffers in a single tab
    composite_buffers: HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,

    /// View state for composite buffers (per split)
    /// Maps (split_id, buffer_id) to composite view state
    composite_view_states:
        HashMap<(SplitId, BufferId), crate::view::composite_view::CompositeViewState>,

    /// Stdin streaming state (if reading from stdin)
    stdin_streaming: Option<StdinStreamingState>,
}

/// State for tracking stdin streaming in background
pub struct StdinStreamingState {
    /// Path to temp file where stdin is being written
    pub temp_path: PathBuf,
    /// Buffer ID for the stdin buffer
    pub buffer_id: BufferId,
    /// Last known file size (for detecting growth)
    pub last_known_size: usize,
    /// Whether streaming is complete (background thread finished)
    pub complete: bool,
    /// Background thread handle (for checking completion)
    pub thread_handle: Option<std::thread::JoinHandle<anyhow::Result<()>>>,
}

impl Editor {
    /// Create a new editor with the given configuration and terminal dimensions
    /// Uses system directories for state (recovery, sessions, etc.)
    pub fn new(
        config: Config,
        width: u16,
        height: u16,
        dir_context: DirectoryContext,
        color_capability: crate::view::color_support::ColorCapability,
    ) -> AnyhowResult<Self> {
        Self::with_working_dir(
            config,
            width,
            height,
            None,
            dir_context,
            true,
            color_capability,
        )
    }

    /// Create a new editor with an explicit working directory
    /// This is useful for testing with isolated temporary directories
    pub fn with_working_dir(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        dir_context: DirectoryContext,
        plugins_enabled: bool,
        color_capability: crate::view::color_support::ColorCapability,
    ) -> AnyhowResult<Self> {
        Self::with_options(
            config,
            width,
            height,
            working_dir,
            None,
            plugins_enabled,
            dir_context,
            None,
            color_capability,
            crate::primitives::grammar::GrammarRegistry::for_editor(),
        )
    }

    /// Create a new editor for testing with optional custom backends
    /// Uses empty grammar registry for fast initialization
    #[allow(clippy::too_many_arguments)]
    pub fn for_test(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        dir_context: DirectoryContext,
        color_capability: crate::view::color_support::ColorCapability,
        fs_backend: Option<Arc<dyn FsBackend>>,
        time_source: Option<SharedTimeSource>,
    ) -> AnyhowResult<Self> {
        Self::with_options(
            config,
            width,
            height,
            working_dir,
            fs_backend,
            true,
            dir_context,
            time_source,
            color_capability,
            crate::primitives::grammar::GrammarRegistry::empty(),
        )
    }

    /// Create a new editor with custom options
    /// This is primarily used for testing with slow or mock backends
    /// to verify editor behavior under various I/O conditions
    #[allow(clippy::too_many_arguments)]
    fn with_options(
        mut config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        fs_backend: Option<Arc<dyn FsBackend>>,
        enable_plugins: bool,
        dir_context: DirectoryContext,
        time_source: Option<SharedTimeSource>,
        color_capability: crate::view::color_support::ColorCapability,
        grammar_registry: Arc<crate::primitives::grammar::GrammarRegistry>,
    ) -> AnyhowResult<Self> {
        // Use provided time_source or default to RealTimeSource
        let time_source = time_source.unwrap_or_else(RealTimeSource::shared);
        tracing::info!("Editor::new called with width={}, height={}", width, height);

        // Use provided working_dir or capture from environment
        let working_dir = working_dir
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        // Canonicalize working_dir to resolve symlinks and normalize path components
        // This ensures consistent path comparisons throughout the editor
        let working_dir = working_dir.canonicalize().unwrap_or(working_dir);

        // Load theme from config
        let theme_loader = crate::view::theme::LocalThemeLoader::new();
        let theme = crate::view::theme::Theme::load(&config.theme, &theme_loader)
            .ok_or_else(|| anyhow::anyhow!("Theme '{:?}' not found", config.theme))?;

        // Set terminal cursor color to match theme
        theme.set_terminal_cursor_color();

        tracing::info!(
            "Grammar registry has {} syntaxes",
            grammar_registry.available_syntaxes().len()
        );

        let keybindings = KeybindingResolver::new(&config);

        // Create an empty initial buffer
        let mut buffers = HashMap::new();
        let mut event_logs = HashMap::new();

        let buffer_id = BufferId(0);
        let mut state = EditorState::new(
            width,
            height,
            config.editor.large_file_threshold_bytes as usize,
        );
        // Apply line_numbers default from config (fixes #539)
        state.margins.set_line_numbers(config.editor.line_numbers);
        // Note: line_wrap_enabled is now stored in SplitViewState.viewport
        tracing::info!("EditorState created for buffer {:?}", buffer_id);
        buffers.insert(buffer_id, state);
        event_logs.insert(buffer_id, EventLog::new());

        // Create metadata for the initial empty buffer
        let mut buffer_metadata = HashMap::new();
        buffer_metadata.insert(buffer_id, BufferMetadata::new());

        // Initialize LSP manager with current working directory as root
        let root_uri = url::Url::from_file_path(&working_dir)
            .ok()
            .and_then(|u| u.as_str().parse::<lsp_types::Uri>().ok());

        // Create Tokio runtime for async I/O (LSP, file watching, git, etc.)
        let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2) // Small pool for I/O tasks
            .thread_name("editor-async")
            .enable_all()
            .build()
            .ok();

        // Create async bridge for communication
        let async_bridge = AsyncBridge::new();

        if tokio_runtime.is_none() {
            tracing::warn!("Failed to create Tokio runtime - async features disabled");
        }

        // Create LSP manager with async support
        let mut lsp = LspManager::new(root_uri);

        // Configure runtime and bridge if available
        if let Some(ref runtime) = tokio_runtime {
            lsp.set_runtime(runtime.handle().clone(), async_bridge.clone());
        }

        // Configure LSP servers from config
        for (language, lsp_config) in &config.lsp {
            lsp.set_language_config(language.clone(), lsp_config.clone());
        }

        // Initialize split manager with the initial buffer
        let split_manager = SplitManager::new(buffer_id);

        // Initialize per-split view state for the initial split
        let mut split_view_states = HashMap::new();
        let initial_split_id = split_manager.active_split();
        let mut initial_view_state = SplitViewState::with_buffer(width, height, buffer_id);
        initial_view_state.viewport.line_wrap_enabled = config.editor.line_wrap;
        split_view_states.insert(initial_split_id, initial_view_state);

        // Initialize filesystem manager for file explorer
        // Use provided backend or create default LocalFsBackend
        let fs_backend = fs_backend.unwrap_or_else(|| Arc::new(LocalFsBackend::new()));
        let fs_manager = Arc::new(FsManager::new(fs_backend));

        // Initialize command registry (always available, used by both plugins and core)
        let command_registry = Arc::new(RwLock::new(CommandRegistry::new()));

        // Initialize plugin manager (handles both enabled and disabled cases internally)
        let plugin_manager = PluginManager::new(
            enable_plugins,
            Arc::clone(&command_registry),
            dir_context.clone(),
        );

        // Update the plugin state snapshot with working_dir BEFORE loading plugins
        // This ensures plugins can call getCwd() correctly during initialization
        #[cfg(feature = "plugins")]
        if let Some(snapshot_handle) = plugin_manager.state_snapshot_handle() {
            let mut snapshot = snapshot_handle.write().unwrap();
            snapshot.working_dir = working_dir.clone();
        }

        // Load TypeScript plugins from multiple directories:
        // 1. Next to the executable (for cargo-dist installations)
        // 2. In the working directory (for development/local usage)
        // 3. From embedded plugins (for cargo-binstall, when embed-plugins feature is enabled)
        if plugin_manager.is_active() {
            let mut plugin_dirs: Vec<std::path::PathBuf> = vec![];

            // Check next to executable first (for cargo-dist installations)
            if let Ok(exe_path) = std::env::current_exe() {
                if let Some(exe_dir) = exe_path.parent() {
                    let exe_plugin_dir = exe_dir.join("plugins");
                    if exe_plugin_dir.exists() {
                        plugin_dirs.push(exe_plugin_dir);
                    }
                }
            }

            // Then check working directory (for development)
            let working_plugin_dir = working_dir.join("plugins");
            if working_plugin_dir.exists() && !plugin_dirs.contains(&working_plugin_dir) {
                plugin_dirs.push(working_plugin_dir);
            }

            // If no disk plugins found, try embedded plugins (cargo-binstall builds)
            #[cfg(feature = "embed-plugins")]
            if plugin_dirs.is_empty() {
                if let Some(embedded_dir) =
                    crate::services::plugins::embedded::get_embedded_plugins_dir()
                {
                    tracing::info!("Using embedded plugins from: {:?}", embedded_dir);
                    plugin_dirs.push(embedded_dir.clone());
                }
            }

            if plugin_dirs.is_empty() {
                tracing::debug!(
                    "No plugins directory found next to executable or in working dir: {:?}",
                    working_dir
                );
            }

            // Load from all found plugin directories, respecting config
            for plugin_dir in plugin_dirs {
                tracing::info!("Loading TypeScript plugins from: {:?}", plugin_dir);
                let (errors, discovered_plugins) =
                    plugin_manager.load_plugins_from_dir_with_config(&plugin_dir, &config.plugins);

                // Merge discovered plugins into config
                // discovered_plugins already contains the merged config (saved enabled state + discovered path)
                for (name, plugin_config) in discovered_plugins {
                    config.plugins.insert(name, plugin_config);
                }

                if !errors.is_empty() {
                    for err in &errors {
                        tracing::error!("TypeScript plugin load error: {}", err);
                    }
                    // In debug/test builds, panic to surface plugin loading errors
                    #[cfg(debug_assertions)]
                    panic!(
                        "TypeScript plugin loading failed with {} error(s): {}",
                        errors.len(),
                        errors.join("; ")
                    );
                }
            }
        }

        // Extract config values before moving config into the struct
        let file_explorer_width = config.file_explorer.width;
        let recovery_enabled = config.editor.recovery_enabled;
        let auto_save_interval_secs = config.editor.auto_save_interval_secs;
        let check_for_updates = config.check_for_updates;
        let show_menu_bar = config.editor.show_menu_bar;
        let show_tab_bar = config.editor.show_tab_bar;

        // Start periodic update checker if enabled (also sends daily telemetry)
        let update_checker = if check_for_updates {
            tracing::debug!("Update checking enabled, starting periodic checker");
            Some(
                crate::services::release_checker::start_periodic_update_check(
                    crate::services::release_checker::DEFAULT_RELEASES_URL,
                    time_source.clone(),
                    dir_context.data_dir.clone(),
                ),
            )
        } else {
            tracing::debug!("Update checking disabled by config");
            None
        };

        let mut editor = Editor {
            buffers,
            event_logs,
            next_buffer_id: 1,
            config,
            dir_context: dir_context.clone(),
            grammar_registry,
            theme,
            ansi_background: None,
            ansi_background_path: None,
            background_fade: crate::primitives::ansi_background::DEFAULT_BACKGROUND_FADE,
            keybindings,
            clipboard: crate::services::clipboard::Clipboard::new(),
            should_quit: false,
            restart_with_dir: None,
            status_message: None,
            plugin_status_message: None,
            plugin_errors: Vec::new(),
            prompt: None,
            terminal_width: width,
            terminal_height: height,
            lsp: Some(lsp),
            buffer_metadata,
            mode_registry: ModeRegistry::new(),
            tokio_runtime,
            async_bridge: Some(async_bridge),
            split_manager,
            split_view_states,
            previous_viewports: HashMap::new(),
            scroll_sync_manager: ScrollSyncManager::new(),
            file_explorer: None,
            fs_manager,
            file_explorer_visible: false,
            file_explorer_sync_in_progress: false,
            file_explorer_width_percent: file_explorer_width,
            pending_file_explorer_show_hidden: None,
            pending_file_explorer_show_gitignored: None,
            menu_bar_visible: show_menu_bar,
            file_explorer_decorations: HashMap::new(),
            file_explorer_decoration_cache:
                crate::view::file_tree::FileExplorerDecorationCache::default(),
            menu_bar_auto_shown: false,
            tab_bar_visible: show_tab_bar,
            mouse_enabled: true,
            mouse_cursor_position: None,
            gpm_active: false,
            key_context: KeyContext::Normal,
            menu_state: crate::view::ui::MenuState::new(),
            menus: crate::config::MenuConfig::translated(),
            working_dir,
            position_history: PositionHistory::new(),
            in_navigation: false,
            next_lsp_request_id: 0,
            pending_completion_request: None,
            completion_items: None,
            scheduled_completion_trigger: None,
            pending_goto_definition_request: None,
            pending_hover_request: None,
            pending_references_request: None,
            pending_references_symbol: String::new(),
            pending_signature_help_request: None,
            pending_code_actions_request: None,
            pending_inlay_hints_request: None,
            pending_semantic_token_requests: HashMap::new(),
            semantic_tokens_in_flight: HashMap::new(),
            pending_semantic_token_range_requests: HashMap::new(),
            semantic_tokens_range_in_flight: HashMap::new(),
            semantic_tokens_range_last_request: HashMap::new(),
            semantic_tokens_range_applied: HashMap::new(),
            semantic_tokens_full_debounce: HashMap::new(),
            hover_symbol_range: None,
            hover_symbol_overlay: None,
            mouse_hover_screen_position: None,
            search_state: None,
            search_namespace: crate::view::overlay::OverlayNamespace::from_string(
                "search".to_string(),
            ),
            lsp_diagnostic_namespace: crate::view::overlay::OverlayNamespace::from_string(
                "lsp-diagnostic".to_string(),
            ),
            pending_search_range: None,
            interactive_replace_state: None,
            lsp_status: String::new(),
            mouse_state: MouseState::default(),
            tab_context_menu: None,
            cached_layout: CachedLayout::default(),
            command_registry,
            plugin_manager,
            seen_byte_ranges: HashMap::new(),
            panel_ids: HashMap::new(),
            background_process_handles: HashMap::new(),
            prompt_histories: {
                // Load prompt histories from disk if available
                let mut histories = HashMap::new();
                for history_name in ["search", "replace", "goto_line"] {
                    let path = dir_context.prompt_history_path(history_name);
                    let history = crate::input::input_history::InputHistory::load_from_file(&path)
                        .unwrap_or_else(|e| {
                            tracing::warn!("Failed to load {} history: {}", history_name, e);
                            crate::input::input_history::InputHistory::new()
                        });
                    histories.insert(history_name.to_string(), history);
                }
                histories
            },
            pending_async_prompt_callback: None,
            lsp_progress: std::collections::HashMap::new(),
            lsp_server_statuses: std::collections::HashMap::new(),
            lsp_window_messages: Vec::new(),
            lsp_log_messages: Vec::new(),
            diagnostic_result_ids: HashMap::new(),
            stored_diagnostics: HashMap::new(),
            event_broadcaster: crate::model::control_event::EventBroadcaster::default(),
            bookmarks: HashMap::new(),
            search_case_sensitive: true,
            search_whole_word: false,
            search_use_regex: false,
            search_confirm_each: false,
            macros: HashMap::new(),
            macro_recording: None,
            last_macro_register: None,
            macro_playing: false,
            #[cfg(feature = "plugins")]
            pending_plugin_actions: Vec::new(),
            #[cfg(feature = "plugins")]
            plugin_render_requested: false,
            chord_state: Vec::new(),
            pending_lsp_confirmation: None,
            pending_close_buffer: None,
            auto_revert_enabled: true,
            last_auto_revert_poll: time_source.now(),
            last_file_tree_poll: time_source.now(),
            file_mod_times: HashMap::new(),
            dir_mod_times: HashMap::new(),
            file_rapid_change_counts: HashMap::new(),
            file_open_state: None,
            file_browser_layout: None,
            recovery_service: {
                let recovery_config = RecoveryConfig {
                    enabled: recovery_enabled,
                    auto_save_interval_secs,
                    ..RecoveryConfig::default()
                };
                RecoveryService::with_config_and_dir(recovery_config, dir_context.recovery_dir())
            },
            full_redraw_requested: false,
            time_source: time_source.clone(),
            last_auto_save: time_source.now(),
            active_custom_contexts: HashSet::new(),
            editor_mode: None,
            warning_log: None,
            warning_domains: WarningDomainRegistry::new(),
            update_checker,
            terminal_manager: crate::services::terminal::TerminalManager::new(),
            terminal_buffers: HashMap::new(),
            terminal_backing_files: HashMap::new(),
            terminal_log_files: HashMap::new(),
            terminal_mode: false,
            keyboard_capture: false,
            terminal_mode_resume: std::collections::HashSet::new(),
            previous_click_time: None,
            previous_click_position: None,
            settings_state: None,
            calibration_wizard: None,
            key_translator: crate::input::key_translator::KeyTranslator::load_default()
                .unwrap_or_default(),
            color_capability,
            stdin_streaming: None,
            review_hunks: Vec::new(),
            active_action_popup: None,
            composite_buffers: HashMap::new(),
            composite_view_states: HashMap::new(),
        };

        #[cfg(feature = "plugins")]
        {
            editor.update_plugin_state_snapshot();
            if editor.plugin_manager.is_active() {
                editor.plugin_manager.run_hook(
                    "editor_initialized",
                    crate::services::plugins::hooks::HookArgs::EditorInitialized,
                );
            }
        }

        Ok(editor)
    }

    /// Get a reference to the event broadcaster
    pub fn event_broadcaster(&self) -> &crate::model::control_event::EventBroadcaster {
        &self.event_broadcaster
    }

    /// Get a reference to the async bridge (if available)
    pub fn async_bridge(&self) -> Option<&AsyncBridge> {
        self.async_bridge.as_ref()
    }

    /// Get a reference to the config
    pub fn config(&self) -> &Config {
        &self.config
    }

    /// Get a reference to the key translator (for input calibration)
    pub fn key_translator(&self) -> &crate::input::key_translator::KeyTranslator {
        &self.key_translator
    }

    /// Get a reference to the time source
    pub fn time_source(&self) -> &SharedTimeSource {
        &self.time_source
    }

    /// Emit a control event
    pub fn emit_event(&self, name: impl Into<String>, data: serde_json::Value) {
        self.event_broadcaster.emit_named(name, data);
    }

    /// Send a response to a plugin for an async operation
    fn send_plugin_response(&self, response: fresh_core::api::PluginResponse) {
        self.plugin_manager.deliver_response(response);
    }

    /// Remove a pending semantic token request from tracking maps.
    fn take_pending_semantic_token_request(
        &mut self,
        request_id: u64,
    ) -> Option<SemanticTokenFullRequest> {
        if let Some(request) = self.pending_semantic_token_requests.remove(&request_id) {
            self.semantic_tokens_in_flight.remove(&request.buffer_id);
            Some(request)
        } else {
            None
        }
    }

    /// Remove a pending semantic token range request from tracking maps.
    fn take_pending_semantic_token_range_request(
        &mut self,
        request_id: u64,
    ) -> Option<SemanticTokenRangeRequest> {
        if let Some(request) = self
            .pending_semantic_token_range_requests
            .remove(&request_id)
        {
            self.semantic_tokens_range_in_flight
                .remove(&request.buffer_id);
            Some(request)
        } else {
            None
        }
    }

    /// Get all keybindings as (key, action) pairs
    pub fn get_all_keybindings(&self) -> Vec<(String, String)> {
        self.keybindings.get_all_bindings()
    }

    /// Get the formatted keybinding for a specific action (for display in messages)
    /// Returns None if no keybinding is found for the action
    pub fn get_keybinding_for_action(&self, action_name: &str) -> Option<String> {
        self.keybindings
            .find_keybinding_for_action(action_name, self.key_context)
    }

    /// Get mutable access to the mode registry
    pub fn mode_registry_mut(&mut self) -> &mut ModeRegistry {
        &mut self.mode_registry
    }

    /// Get immutable access to the mode registry
    pub fn mode_registry(&self) -> &ModeRegistry {
        &self.mode_registry
    }

    /// Get the currently active buffer ID.
    ///
    /// This is derived from the split manager (single source of truth).
    /// The editor always has at least one buffer, so this never fails.
    #[inline]
    pub fn active_buffer(&self) -> BufferId {
        self.split_manager
            .active_buffer_id()
            .expect("Editor always has at least one buffer")
    }

    /// Get the mode name for the active buffer (if it's a virtual buffer)
    pub fn active_buffer_mode(&self) -> Option<&str> {
        self.buffer_metadata
            .get(&self.active_buffer())
            .and_then(|meta| meta.virtual_mode())
    }

    /// Check if the active buffer is read-only
    pub fn is_active_buffer_read_only(&self) -> bool {
        if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer()) {
            if metadata.read_only {
                return true;
            }
            // Also check if the mode is read-only
            if let Some(mode_name) = metadata.virtual_mode() {
                return self.mode_registry.is_read_only(mode_name);
            }
        }
        false
    }

    /// Check if editing should be disabled for the active buffer
    /// This returns true when editing_disabled is true (e.g., for read-only virtual buffers)
    pub fn is_editing_disabled(&self) -> bool {
        self.active_state().editing_disabled
    }

    /// Resolve a keybinding for the current mode
    ///
    /// First checks the global editor mode (for vi mode and other modal editing).
    /// If no global mode is set or no binding is found, falls back to the
    /// active buffer's mode (for virtual buffers with custom modes).
    /// Returns the command name if found.
    pub fn resolve_mode_keybinding(
        &self,
        code: KeyCode,
        modifiers: KeyModifiers,
    ) -> Option<String> {
        // First check global editor mode (e.g., "vi-normal", "vi-operator-pending")
        if let Some(ref global_mode) = self.editor_mode {
            if let Some(binding) =
                self.mode_registry
                    .resolve_keybinding(global_mode, code, modifiers)
            {
                return Some(binding);
            }
        }

        // Fall back to buffer-local mode (for virtual buffers)
        let mode_name = self.active_buffer_mode()?;
        self.mode_registry
            .resolve_keybinding(mode_name, code, modifiers)
    }

    /// Check if LSP has any active progress tasks (e.g., indexing)
    pub fn has_active_lsp_progress(&self) -> bool {
        !self.lsp_progress.is_empty()
    }

    /// Get the current LSP progress info (if any)
    pub fn get_lsp_progress(&self) -> Vec<(String, String, Option<String>)> {
        self.lsp_progress
            .iter()
            .map(|(token, info)| (token.clone(), info.title.clone(), info.message.clone()))
            .collect()
    }

    /// Check if LSP server for a given language is running (ready)
    pub fn is_lsp_server_ready(&self, language: &str) -> bool {
        use crate::services::async_bridge::LspServerStatus;
        self.lsp_server_statuses
            .get(language)
            .map(|status| matches!(status, LspServerStatus::Running))
            .unwrap_or(false)
    }

    /// Get the LSP status string (displayed in status bar)
    pub fn get_lsp_status(&self) -> &str {
        &self.lsp_status
    }

    /// Get stored LSP diagnostics (for testing and external access)
    /// Returns a reference to the diagnostics map keyed by file URI
    pub fn get_stored_diagnostics(&self) -> &HashMap<String, Vec<lsp_types::Diagnostic>> {
        &self.stored_diagnostics
    }

    /// Check if an update is available
    pub fn is_update_available(&self) -> bool {
        self.update_checker
            .as_ref()
            .map(|c| c.is_update_available())
            .unwrap_or(false)
    }

    /// Get the latest version string if an update is available
    pub fn latest_version(&self) -> Option<&str> {
        self.update_checker
            .as_ref()
            .and_then(|c| c.latest_version())
    }

    /// Get the cached release check result (for shutdown notification)
    pub fn get_update_result(
        &self,
    ) -> Option<&crate::services::release_checker::ReleaseCheckResult> {
        self.update_checker
            .as_ref()
            .and_then(|c| c.get_cached_result())
    }

    /// Set a custom update checker (for testing)
    ///
    /// This allows injecting a custom PeriodicUpdateChecker that points to a mock server,
    /// enabling E2E tests for the update notification UI.
    #[doc(hidden)]
    pub fn set_update_checker(
        &mut self,
        checker: crate::services::release_checker::PeriodicUpdateChecker,
    ) {
        self.update_checker = Some(checker);
    }

    /// Configure LSP server for a specific language
    pub fn set_lsp_config(&mut self, language: String, config: LspServerConfig) {
        if let Some(ref mut lsp) = self.lsp {
            lsp.set_language_config(language, config);
        }
    }

    /// Get a list of currently running LSP server languages
    pub fn running_lsp_servers(&self) -> Vec<String> {
        self.lsp
            .as_ref()
            .map(|lsp| lsp.running_servers())
            .unwrap_or_default()
    }

    /// Shutdown an LSP server by language (marks it as disabled until manual restart)
    ///
    /// Returns true if the server was found and shutdown, false otherwise
    pub fn shutdown_lsp_server(&mut self, language: &str) -> bool {
        if let Some(ref mut lsp) = self.lsp {
            lsp.shutdown_server(language)
        } else {
            false
        }
    }

    /// Enable event log streaming to a file
    pub fn enable_event_streaming<P: AsRef<Path>>(&mut self, path: P) -> AnyhowResult<()> {
        // Enable streaming for all existing event logs
        for event_log in self.event_logs.values_mut() {
            event_log.enable_streaming(&path)?;
        }
        Ok(())
    }

    /// Log keystroke for debugging
    pub fn log_keystroke(&mut self, key_code: &str, modifiers: &str) {
        if let Some(event_log) = self.event_logs.get_mut(&self.active_buffer()) {
            event_log.log_keystroke(key_code, modifiers);
        }
    }

    /// Set up warning log monitoring
    ///
    /// When warnings/errors are logged, they will be written to the specified path
    /// and the editor will be notified via the receiver.
    pub fn set_warning_log(&mut self, receiver: std::sync::mpsc::Receiver<()>, path: PathBuf) {
        self.warning_log = Some((receiver, path));
    }

    /// Check for and handle any new warnings in the warning log
    ///
    /// Updates the general warning domain for the status bar.
    /// Returns true if new warnings were found.
    pub fn check_warning_log(&mut self) -> bool {
        let Some((receiver, path)) = &self.warning_log else {
            return false;
        };

        // Non-blocking check for any warnings
        let mut new_warning_count = 0usize;
        while receiver.try_recv().is_ok() {
            new_warning_count += 1;
        }

        if new_warning_count > 0 {
            // Update general warning domain (don't auto-open file)
            self.warning_domains.general.add_warnings(new_warning_count);
            self.warning_domains.general.set_log_path(path.clone());
        }

        new_warning_count > 0
    }

    /// Get the warning domain registry
    pub fn get_warning_domains(&self) -> &WarningDomainRegistry {
        &self.warning_domains
    }

    /// Get the warning log path (for opening when user clicks indicator)
    pub fn get_warning_log_path(&self) -> Option<&PathBuf> {
        self.warning_domains.general.log_path.as_ref()
    }

    /// Open the warning log file (user-initiated action)
    pub fn open_warning_log(&mut self) {
        if let Some(path) = self.warning_domains.general.log_path.clone() {
            if let Err(e) = self.open_file(&path) {
                tracing::error!("Failed to open warning log: {}", e);
            }
        }
    }

    /// Clear the general warning indicator (user dismissed)
    pub fn clear_warning_indicator(&mut self) {
        self.warning_domains.general.clear();
    }

    /// Clear all warning indicators (user dismissed via command)
    pub fn clear_warnings(&mut self) {
        self.warning_domains.general.clear();
        self.warning_domains.lsp.clear();
        self.status_message = Some("Warnings cleared".to_string());
    }

    /// Check if any LSP server is in error state
    pub fn has_lsp_error(&self) -> bool {
        self.warning_domains.lsp.level() == WarningLevel::Error
    }

    /// Get the effective warning level for the status bar (LSP indicator)
    /// Returns Error if LSP has errors, Warning if there are warnings, None otherwise
    pub fn get_effective_warning_level(&self) -> WarningLevel {
        self.warning_domains.lsp.level()
    }

    /// Get the general warning level (for the general warning badge)
    pub fn get_general_warning_level(&self) -> WarningLevel {
        self.warning_domains.general.level()
    }

    /// Get the general warning count
    pub fn get_general_warning_count(&self) -> usize {
        self.warning_domains.general.count
    }

    /// Update LSP warning domain from server statuses
    pub fn update_lsp_warning_domain(&mut self) {
        self.warning_domains
            .lsp
            .update_from_statuses(&self.lsp_server_statuses);
    }

    /// Check if mouse hover timer has expired and trigger LSP hover request
    ///
    /// This implements debounced hover - we wait for the configured delay before
    /// sending the request to avoid spamming the LSP server on every mouse move.
    /// Returns true if a hover request was triggered.
    pub fn check_mouse_hover_timer(&mut self) -> bool {
        // Check if mouse hover is enabled
        if !self.config.editor.mouse_hover_enabled {
            return false;
        }

        let hover_delay = std::time::Duration::from_millis(self.config.editor.mouse_hover_delay_ms);

        // Get hover state without borrowing self
        let hover_info = match self.mouse_state.lsp_hover_state {
            Some((byte_pos, start_time, screen_x, screen_y)) => {
                if self.mouse_state.lsp_hover_request_sent {
                    return false; // Already sent request for this position
                }
                if start_time.elapsed() < hover_delay {
                    return false; // Timer hasn't expired yet
                }
                Some((byte_pos, screen_x, screen_y))
            }
            None => return false,
        };

        let Some((byte_pos, screen_x, screen_y)) = hover_info else {
            return false;
        };

        // Mark as sent before requesting (to prevent double-sending)
        self.mouse_state.lsp_hover_request_sent = true;

        // Store mouse position for popup positioning
        self.mouse_hover_screen_position = Some((screen_x, screen_y));

        // Request hover at the byte position
        if let Err(e) = self.request_hover_at_position(byte_pos) {
            tracing::debug!("Failed to request hover: {}", e);
            return false;
        }

        true
    }

    /// Check if semantic highlight debounce timer has expired
    ///
    /// Returns true if a redraw is needed because the debounce period has elapsed
    /// and semantic highlights need to be recomputed.
    pub fn check_semantic_highlight_timer(&self) -> bool {
        // Check all buffers for pending semantic highlight redraws
        for state in self.buffers.values() {
            if let Some(remaining) = state.reference_highlight_overlay.needs_redraw() {
                if remaining.is_zero() {
                    return true;
                }
            }
        }
        false
    }

    /// Check if completion trigger timer has expired and trigger completion if so
    ///
    /// This implements debounced completion - we wait for quick_suggestions_delay_ms
    /// before sending the completion request to avoid spamming the LSP server.
    /// Returns true if a completion request was triggered.
    pub fn check_completion_trigger_timer(&mut self) -> bool {
        // Check if we have a scheduled completion trigger
        let Some(trigger_time) = self.scheduled_completion_trigger else {
            return false;
        };

        // Check if the timer has expired
        if Instant::now() < trigger_time {
            return false;
        }

        // Clear the scheduled trigger
        self.scheduled_completion_trigger = None;

        // Don't trigger if a popup is already visible
        if self.active_state().popups.is_visible() {
            return false;
        }

        // Trigger the completion request
        if let Err(e) = self.request_completion() {
            tracing::debug!("Failed to trigger debounced completion: {}", e);
            return false;
        }

        true
    }

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
    /// When the file explorer is visible, tabs only get a portion of the terminal width
    /// based on `file_explorer_width_percent`. This matches the layout calculation in render.rs.
    fn effective_tabs_width(&self) -> u16 {
        if self.file_explorer_visible && self.file_explorer.is_some() {
            // When file explorer is visible, tabs get (1 - explorer_width) of the terminal width
            let editor_percent = 1.0 - self.file_explorer_width_percent;
            (self.terminal_width as f32 * editor_percent) as u16
        } else {
            self.terminal_width
        }
    }

    /// Set the active buffer and trigger all necessary side effects
    ///
    /// This is the centralized method for switching buffers. It:
    /// - Updates split manager (single source of truth for active buffer)
    /// - Adds buffer to active split's tabs (if not already there)
    /// - Syncs file explorer to the new active file (if visible)
    ///
    /// Use this instead of directly calling split_manager.set_active_buffer_id()
    /// to ensure all side effects happen consistently.
    fn set_active_buffer(&mut self, buffer_id: BufferId) {
        if self.active_buffer() == buffer_id {
            return; // No change
        }

        // Dismiss transient popups and clear hover state when switching buffers
        self.on_editor_focus_lost();

        // Cancel search/replace prompts when switching buffers
        // (they are buffer-specific and don't make sense across buffers)
        self.cancel_search_prompt_if_active();

        // Track the previous buffer for "Switch to Previous Tab" command
        let previous = self.active_buffer();

        // If leaving a terminal buffer while in terminal mode, remember it should resume
        if self.terminal_mode && self.is_terminal_buffer(previous) {
            self.terminal_mode_resume.insert(previous);
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Update split manager (single source of truth)
        self.split_manager.set_active_buffer_id(buffer_id);

        // If switching to a terminal buffer that should resume terminal mode, re-enter it
        if self.terminal_mode_resume.contains(&buffer_id) && self.is_terminal_buffer(buffer_id) {
            self.terminal_mode = true;
            self.key_context = crate::input::keybindings::KeyContext::Terminal;
        } else if self.is_terminal_buffer(buffer_id) {
            // Switching to terminal in read-only mode - sync buffer to show current terminal content
            // This ensures the backing file content and cursor position are up to date
            self.sync_terminal_to_buffer(buffer_id);
        }

        // Add buffer to the active split's open_buffers (tabs) if not already there
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
            // Update the previous buffer tracker
            view_state.previous_buffer = Some(previous);
        }

        // Ensure the newly active tab is visible
        // Use effective_tabs_width() to account for file explorer taking 30% of width
        self.ensure_active_tab_visible(active_split, buffer_id, self.effective_tabs_width());

        // Note: We don't sync file explorer here to avoid flicker during tab switches.
        // File explorer syncs when explicitly focused via focus_file_explorer().

        // Emit buffer_activated hook for plugins
        self.plugin_manager.run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
        );
    }

    /// Focus a split and its buffer, handling all side effects including terminal mode.
    ///
    /// This is the primary method for switching focus between splits via mouse clicks.
    /// It handles:
    /// - Exiting terminal mode when leaving a terminal buffer
    /// - Updating split manager state
    /// - Managing tab state and previous buffer tracking
    /// - Syncing file explorer
    ///
    /// Use this instead of calling set_active_split directly when switching focus.
    pub(super) fn focus_split(
        &mut self,
        split_id: crate::model::event::SplitId,
        buffer_id: BufferId,
    ) {
        let previous_split = self.split_manager.active_split();
        let previous_buffer = self.active_buffer(); // Get BEFORE changing split
        let split_changed = previous_split != split_id;

        if split_changed {
            // Switching to a different split - exit terminal mode if active
            if self.terminal_mode && self.is_terminal_buffer(previous_buffer) {
                self.terminal_mode = false;
                self.key_context = crate::input::keybindings::KeyContext::Normal;
            }

            // Update split manager to focus this split
            self.split_manager.set_active_split(split_id);

            // Update the buffer in the new split
            self.split_manager.set_active_buffer_id(buffer_id);

            // Set key context based on target buffer type
            if self.is_terminal_buffer(buffer_id) {
                self.terminal_mode = true;
                self.key_context = crate::input::keybindings::KeyContext::Terminal;
            } else {
                // Ensure key context is Normal when focusing a non-terminal buffer
                // This handles the case of clicking on editor from FileExplorer context
                self.key_context = crate::input::keybindings::KeyContext::Normal;
            }

            // Handle buffer change side effects
            if previous_buffer != buffer_id {
                self.position_history.commit_pending_movement();
                if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                    view_state.add_buffer(buffer_id);
                    view_state.previous_buffer = Some(previous_buffer);
                }
                // Note: We don't sync file explorer here to avoid flicker during split focus changes.
                // File explorer syncs when explicitly focused via focus_file_explorer().
            }
        } else {
            // Same split, different buffer (tab switch) - use set_active_buffer for terminal resume
            self.set_active_buffer(buffer_id);
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
    /// All event applications MUST go through this method to ensure consistency.
    pub fn apply_event_to_active_buffer(&mut self, event: &Event) {
        // Handle View events at Editor level - View events go to SplitViewState, not EditorState
        // This properly separates Buffer state from View state
        match event {
            Event::Scroll { line_offset } => {
                self.handle_scroll_event(*line_offset);
                return;
            }
            Event::SetViewport { top_line } => {
                self.handle_set_viewport_event(*top_line);
                return;
            }
            Event::Recenter => {
                self.handle_recenter_event();
                return;
            }
            _ => {}
        }

        // IMPORTANT: Calculate LSP changes and line info BEFORE applying to buffer!
        // The byte positions in the events are relative to the ORIGINAL buffer,
        // so we must convert them to LSP positions before modifying the buffer.
        let lsp_changes = self.collect_lsp_changes(event);

        // Calculate line info for plugin hooks (using same pre-modification buffer state)
        let line_info = self.calculate_event_line_info(event);

        // 1. Apply the event to the buffer
        self.active_state_mut().apply(event);

        // 1b. Sync cursors and viewport from EditorState to SplitViewState
        // This keeps the authoritative View state in SplitViewState up to date
        self.sync_editor_state_to_split_view_state();

        // 1c. Invalidate layouts for all views of this buffer after content changes
        // Note: recovery_pending is set automatically by the buffer on edits
        match event {
            Event::Insert { .. } | Event::Delete { .. } | Event::BulkEdit { .. } => {
                self.invalidate_layouts_for_buffer(self.active_buffer());
                self.schedule_semantic_tokens_full_refresh(self.active_buffer());
            }
            Event::Batch { events, .. } => {
                let has_edits = events
                    .iter()
                    .any(|e| matches!(e, Event::Insert { .. } | Event::Delete { .. }));
                if has_edits {
                    self.invalidate_layouts_for_buffer(self.active_buffer());
                    self.schedule_semantic_tokens_full_refresh(self.active_buffer());
                }
            }
            _ => {}
        }

        // 2. Adjust cursors in other splits that share the same buffer
        self.adjust_other_split_cursors_for_event(event);

        // 3. Clear search highlights on edit (Insert/Delete events)
        // This preserves highlights while navigating but clears them when modifying text
        // EXCEPT during interactive replace where we want to keep highlights visible
        let in_interactive_replace = self.interactive_replace_state.is_some();

        // Note: We intentionally do NOT clear search overlays on buffer modification.
        // Overlays have markers that automatically track position changes through edits,
        // which allows F3/Shift+F3 to find matches at their updated positions.
        // The visual highlights may be on text that no longer matches the query,
        // but that's acceptable - user can see where original matches were.
        let _ = in_interactive_replace; // silence unused warning

        // 3. Trigger plugin hooks for this event (with pre-calculated line info)
        self.trigger_plugin_hooks_for_event(event, line_info);

        // 4. Notify LSP of the change using pre-calculated positions
        self.send_lsp_changes_for_buffer(self.active_buffer(), lsp_changes);
    }

    /// Apply multiple Insert/Delete events efficiently using bulk edit optimization.
    ///
    /// This avoids O(n²) complexity by:
    /// 1. Converting events to (position, delete_len, insert_text) tuples
    /// 2. Applying all edits in a single tree pass via apply_bulk_edits
    /// 3. Creating a BulkEdit event for undo (stores tree snapshot via Arc clone = O(1))
    ///
    /// # Arguments
    /// * `events` - Vec of Insert/Delete events (sorted by position descending for correct application)
    /// * `description` - Description for the undo log
    ///
    /// # Returns
    /// The BulkEdit event that was applied, for tracking purposes
    pub fn apply_events_as_bulk_edit(
        &mut self,
        events: Vec<Event>,
        description: String,
    ) -> Option<Event> {
        use crate::model::event::CursorId;

        // Check if any events modify the buffer
        let has_buffer_mods = events
            .iter()
            .any(|e| matches!(e, Event::Insert { .. } | Event::Delete { .. }));

        if !has_buffer_mods {
            // No buffer modifications - use regular Batch
            return None;
        }

        let state = self.active_state_mut();

        // Capture old cursor states
        let old_cursors: Vec<(CursorId, usize, Option<usize>)> = state
            .cursors
            .iter()
            .map(|(id, c)| (id, c.position, c.anchor))
            .collect();

        // Snapshot the tree for undo (O(1) - Arc clone)
        let old_tree = state.buffer.snapshot_piece_tree();

        // Convert events to edit tuples: (position, delete_len, insert_text)
        // Events must be sorted by position descending (later positions first)
        // This ensures earlier edits don't shift positions of later edits
        let mut edits: Vec<(usize, usize, String)> = Vec::new();

        for event in &events {
            match event {
                Event::Insert { position, text, .. } => {
                    edits.push((*position, 0, text.clone()));
                }
                Event::Delete { range, .. } => {
                    edits.push((range.start, range.len(), String::new()));
                }
                _ => {}
            }
        }

        // Sort edits by position descending (required by apply_bulk_edits)
        edits.sort_by(|a, b| b.0.cmp(&a.0));

        // Convert to references for apply_bulk_edits
        let edit_refs: Vec<(usize, usize, &str)> = edits
            .iter()
            .map(|(pos, del, text)| (*pos, *del, text.as_str()))
            .collect();

        // Apply bulk edits
        let _delta = state.buffer.apply_bulk_edits(&edit_refs);

        // Snapshot the tree after edits (for redo) - O(1) Arc clone
        let new_tree = state.buffer.snapshot_piece_tree();

        // Calculate new cursor positions based on events
        // Process cursor movements from the original events
        let mut new_cursors: Vec<(CursorId, usize, Option<usize>)> = old_cursors.clone();

        // Calculate position adjustments from edits (sorted ascending by position)
        // Each entry is (edit_position, delta) where delta = insert_len - delete_len
        let mut position_deltas: Vec<(usize, isize)> = Vec::new();
        for (pos, del_len, text) in &edits {
            let delta = text.len() as isize - *del_len as isize;
            position_deltas.push((*pos, delta));
        }
        position_deltas.sort_by_key(|(pos, _)| *pos);

        // Helper: calculate cumulative shift for a position based on edits at lower positions
        let calc_shift = |original_pos: usize| -> isize {
            let mut shift: isize = 0;
            for (edit_pos, delta) in &position_deltas {
                if *edit_pos < original_pos {
                    shift += delta;
                }
            }
            shift
        };

        // Apply adjustments to cursor positions
        // First check for explicit MoveCursor events (e.g., from indent operations)
        // These take precedence over implicit cursor updates from Insert/Delete
        for (cursor_id, ref mut pos, ref mut anchor) in &mut new_cursors {
            let mut found_move_cursor = false;
            // Save original position before any modifications - needed for shift calculation
            let original_pos = *pos;

            // Check if this cursor has an Insert at its original position (auto-close pattern).
            // For auto-close, Insert is at cursor position and MoveCursor is relative to original state.
            // For other operations (like indent), Insert is elsewhere and MoveCursor already accounts for shifts.
            let insert_at_cursor_pos = events.iter().any(|e| {
                matches!(e, Event::Insert { position, cursor_id: c, .. }
                    if *c == *cursor_id && *position == original_pos)
            });

            // First pass: look for explicit MoveCursor events for this cursor
            for event in &events {
                if let Event::MoveCursor {
                    cursor_id: event_cursor,
                    new_position,
                    new_anchor,
                    ..
                } = event
                {
                    if event_cursor == cursor_id {
                        // Only adjust for shifts if the Insert was at the cursor's original position
                        // (like auto-close). For other operations (like indent where Insert is at
                        // line start), the MoveCursor already accounts for the shift.
                        let shift = if insert_at_cursor_pos {
                            calc_shift(original_pos)
                        } else {
                            0
                        };
                        *pos = (*new_position as isize + shift) as usize;
                        *anchor = *new_anchor;
                        found_move_cursor = true;
                    }
                }
            }

            // If no explicit MoveCursor, derive position from Insert/Delete
            if !found_move_cursor {
                for event in &events {
                    match event {
                        Event::Insert {
                            position,
                            text,
                            cursor_id: event_cursor,
                        } if event_cursor == cursor_id => {
                            // For insert, cursor moves to end of inserted text
                            // Account for shifts from edits at lower positions
                            let shift = calc_shift(*position);
                            let adjusted_pos = (*position as isize + shift) as usize;
                            *pos = adjusted_pos + text.len();
                            *anchor = None;
                        }
                        Event::Delete {
                            range,
                            cursor_id: event_cursor,
                            ..
                        } if event_cursor == cursor_id => {
                            // For delete, cursor moves to start of deleted range
                            // Account for shifts from edits at lower positions
                            let shift = calc_shift(range.start);
                            *pos = (range.start as isize + shift) as usize;
                            *anchor = None;
                        }
                        _ => {}
                    }
                }
            }
        }

        // Update cursors in state
        for (cursor_id, position, anchor) in &new_cursors {
            if let Some(cursor) = state.cursors.get_mut(*cursor_id) {
                cursor.position = *position;
                cursor.anchor = *anchor;
            }
        }

        // Invalidate highlighter
        state.highlighter.invalidate_all();

        // Create BulkEdit event with both tree snapshots
        let bulk_edit = Event::BulkEdit {
            old_tree: Some(old_tree),
            new_tree: Some(new_tree),
            old_cursors,
            new_cursors,
            description,
        };

        // Post-processing (layout invalidation, split cursor sync, etc.)
        self.sync_editor_state_to_split_view_state();
        self.invalidate_layouts_for_buffer(self.active_buffer());
        self.adjust_other_split_cursors_for_event(&bulk_edit);
        // Note: Do NOT clear search overlays - markers track through edits for F3/Shift+F3

        Some(bulk_edit)
    }

    /// Trigger plugin hooks for an event (if any)
    /// line_info contains pre-calculated line numbers from BEFORE buffer modification
    fn trigger_plugin_hooks_for_event(&mut self, event: &Event, line_info: EventLineInfo) {
        let buffer_id = self.active_buffer();

        // Convert event to hook args and fire the appropriate hook
        let hook_args = match event {
            Event::Insert { position, text, .. } => {
                let insert_position = *position;
                let insert_len = text.len();

                // Adjust byte ranges for the insertion
                if let Some(seen) = self.seen_byte_ranges.get_mut(&buffer_id) {
                    // Collect adjusted ranges:
                    // - Ranges ending before insert: keep unchanged
                    // - Ranges containing insert point: remove (content changed)
                    // - Ranges starting after insert: shift by insert_len
                    let adjusted: std::collections::HashSet<(usize, usize)> = seen
                        .iter()
                        .filter_map(|&(start, end)| {
                            if end <= insert_position {
                                // Range ends before insert - unchanged
                                Some((start, end))
                            } else if start >= insert_position {
                                // Range starts at or after insert - shift forward
                                Some((start + insert_len, end + insert_len))
                            } else {
                                // Range contains insert point - invalidate
                                None
                            }
                        })
                        .collect();
                    *seen = adjusted;
                }

                Some((
                    "after_insert",
                    crate::services::plugins::hooks::HookArgs::AfterInsert {
                        buffer_id,
                        position: *position,
                        text: text.clone(),
                        // Byte range of the affected area
                        affected_start: insert_position,
                        affected_end: insert_position + insert_len,
                        // Line info from pre-modification buffer
                        start_line: line_info.start_line,
                        end_line: line_info.end_line,
                        lines_added: line_info.line_delta.max(0) as usize,
                    },
                ))
            }
            Event::Delete {
                range,
                deleted_text,
                ..
            } => {
                let delete_start = range.start;

                // Adjust byte ranges for the deletion
                let delete_end = range.end;
                let delete_len = delete_end - delete_start;
                if let Some(seen) = self.seen_byte_ranges.get_mut(&buffer_id) {
                    // Collect adjusted ranges:
                    // - Ranges ending before delete start: keep unchanged
                    // - Ranges overlapping deletion: remove (content changed)
                    // - Ranges starting after delete end: shift backward by delete_len
                    let adjusted: std::collections::HashSet<(usize, usize)> = seen
                        .iter()
                        .filter_map(|&(start, end)| {
                            if end <= delete_start {
                                // Range ends before delete - unchanged
                                Some((start, end))
                            } else if start >= delete_end {
                                // Range starts after delete - shift backward
                                Some((start - delete_len, end - delete_len))
                            } else {
                                // Range overlaps deletion - invalidate
                                None
                            }
                        })
                        .collect();
                    *seen = adjusted;
                }

                Some((
                    "after_delete",
                    crate::services::plugins::hooks::HookArgs::AfterDelete {
                        buffer_id,
                        range: range.clone(),
                        deleted_text: deleted_text.clone(),
                        // Byte position and length of deleted content
                        affected_start: delete_start,
                        deleted_len: deleted_text.len(),
                        // Line info from pre-modification buffer
                        start_line: line_info.start_line,
                        end_line: line_info.end_line,
                        lines_removed: (-line_info.line_delta).max(0) as usize,
                    },
                ))
            }
            Event::Batch { events, .. } => {
                // Fire hooks for each event in the batch
                // Note: For batches, line info is approximate since buffer already modified
                // Individual events will use the passed line_info which covers the whole batch
                for e in events {
                    // Use default line info for sub-events - they share the batch's line_info
                    // This is a simplification; proper tracking would need per-event pre-calculation
                    let sub_line_info = self.calculate_event_line_info(e);
                    self.trigger_plugin_hooks_for_event(e, sub_line_info);
                }
                None
            }
            Event::MoveCursor {
                cursor_id,
                old_position,
                new_position,
                ..
            } => {
                // Get the line number for the new position (1-indexed for plugins)
                let line = self.active_state().buffer.get_line_number(*new_position) + 1;
                Some((
                    "cursor_moved",
                    crate::services::plugins::hooks::HookArgs::CursorMoved {
                        buffer_id,
                        cursor_id: *cursor_id,
                        old_position: *old_position,
                        new_position: *new_position,
                        line,
                    },
                ))
            }
            _ => None,
        };

        // Fire the hook to TypeScript plugins
        if let Some((hook_name, args)) = hook_args {
            // Update the full plugin state snapshot BEFORE firing the hook
            // This ensures the plugin can read up-to-date state (diff, cursors, viewport, etc.)
            // Without this, there's a race condition where the async hook might read stale data
            #[cfg(feature = "plugins")]
            self.update_plugin_state_snapshot();

            self.plugin_manager.run_hook(hook_name, args);
        }
    }

    /// Handle scroll events using the SplitViewState's viewport
    ///
    /// View events (like Scroll) go to SplitViewState, not EditorState.
    /// This correctly handles scroll limits when view transforms inject headers.
    /// Also syncs to EditorState.viewport for the active split (used in rendering).
    fn handle_scroll_event(&mut self, line_offset: isize) {
        use crate::view::ui::view_pipeline::ViewLineIterator;

        let active_split = self.split_manager.active_split();

        // Check if this split is in a scroll sync group (anchor-based sync for diffs)
        // Mark both splits to skip ensure_visible so cursor doesn't override scroll
        // The sync_scroll_groups() at render time will sync the other split
        if let Some(group) = self.scroll_sync_manager.find_group_for_split(active_split) {
            let left = group.left_split;
            let right = group.right_split;
            if let Some(vs) = self.split_view_states.get_mut(&left) {
                vs.viewport.set_skip_ensure_visible();
            }
            if let Some(vs) = self.split_view_states.get_mut(&right) {
                vs.viewport.set_skip_ensure_visible();
            }
            // Continue to scroll the active split normally below
        }

        // Fall back to simple sync_group (same delta to all splits)
        let sync_group = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.sync_group);
        let splits_to_scroll = if let Some(group_id) = sync_group {
            self.split_manager
                .get_splits_in_group(group_id, &self.split_view_states)
        } else {
            vec![active_split]
        };

        for split_id in splits_to_scroll {
            let buffer_id = if let Some(id) = self.split_manager.buffer_for_split(split_id) {
                id
            } else {
                continue;
            };
            let tab_size = self.config.editor.tab_size;

            // Get view_transform tokens from SplitViewState (if any)
            let view_transform_tokens = self
                .split_view_states
                .get(&split_id)
                .and_then(|vs| vs.view_transform.as_ref())
                .map(|vt| vt.tokens.clone());

            // Get mutable references to both buffer and view state
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                let buffer = &mut state.buffer;
                if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                    if let Some(tokens) = view_transform_tokens {
                        // Use view-aware scrolling with the transform's tokens
                        let view_lines: Vec<_> =
                            ViewLineIterator::new(&tokens, false, false, tab_size).collect();
                        view_state
                            .viewport
                            .scroll_view_lines(&view_lines, line_offset);
                    } else {
                        // No view transform - use traditional buffer-based scrolling
                        if line_offset > 0 {
                            view_state
                                .viewport
                                .scroll_down(buffer, line_offset as usize);
                        } else {
                            view_state
                                .viewport
                                .scroll_up(buffer, line_offset.unsigned_abs());
                        }
                    }
                    // Mark to skip ensure_visible on next render so the scroll isn't undone
                    view_state.viewport.set_skip_ensure_visible();
                }
            }
        }
    }

    /// Handle SetViewport event using SplitViewState's viewport
    fn handle_set_viewport_event(&mut self, top_line: usize) {
        let active_split = self.split_manager.active_split();

        // Check if this split is in a scroll sync group (anchor-based sync for diffs)
        // If so, set the group's scroll_line and let render sync the viewports
        if self.scroll_sync_manager.is_split_synced(active_split) {
            if let Some(group) = self
                .scroll_sync_manager
                .find_group_for_split_mut(active_split)
            {
                // Convert line to left buffer space if coming from right split
                let scroll_line = if group.is_left_split(active_split) {
                    top_line
                } else {
                    group.right_to_left_line(top_line)
                };
                group.set_scroll_line(scroll_line);
            }

            // Mark both splits to skip ensure_visible
            if let Some(group) = self.scroll_sync_manager.find_group_for_split(active_split) {
                let left = group.left_split;
                let right = group.right_split;
                if let Some(vs) = self.split_view_states.get_mut(&left) {
                    vs.viewport.set_skip_ensure_visible();
                }
                if let Some(vs) = self.split_view_states.get_mut(&right) {
                    vs.viewport.set_skip_ensure_visible();
                }
            }
            return;
        }

        // Fall back to simple sync_group (same line to all splits)
        let sync_group = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.sync_group);
        let splits_to_scroll = if let Some(group_id) = sync_group {
            self.split_manager
                .get_splits_in_group(group_id, &self.split_view_states)
        } else {
            vec![active_split]
        };

        for split_id in splits_to_scroll {
            let buffer_id = if let Some(id) = self.split_manager.buffer_for_split(split_id) {
                id
            } else {
                continue;
            };

            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                let buffer = &mut state.buffer;
                if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                    view_state.viewport.scroll_to(buffer, top_line);
                    // Mark to skip ensure_visible on next render so the scroll isn't undone
                    view_state.viewport.set_skip_ensure_visible();
                }
            }
        }
    }

    /// Handle Recenter event using SplitViewState's viewport
    fn handle_recenter_event(&mut self) {
        let active_split = self.split_manager.active_split();

        // Find other splits in the same sync group if any
        let sync_group = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.sync_group);
        let splits_to_recenter = if let Some(group_id) = sync_group {
            self.split_manager
                .get_splits_in_group(group_id, &self.split_view_states)
        } else {
            vec![active_split]
        };

        for split_id in splits_to_recenter {
            let buffer_id = if let Some(id) = self.split_manager.buffer_for_split(split_id) {
                id
            } else {
                continue;
            };

            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                let buffer = &mut state.buffer;
                let view_state = self.split_view_states.get_mut(&split_id);

                if let Some(view_state) = view_state {
                    // Recenter viewport on cursor
                    let cursor = *view_state.cursors.primary();
                    let viewport_height = view_state.viewport.visible_line_count();
                    let target_rows_from_top = viewport_height / 2;

                    // Move backwards from cursor position target_rows_from_top lines
                    let mut iter = buffer.line_iterator(cursor.position, 80);
                    for _ in 0..target_rows_from_top {
                        if iter.prev().is_none() {
                            break;
                        }
                    }
                    let new_top_byte = iter.current_position();
                    view_state.viewport.top_byte = new_top_byte;
                    // Mark to skip ensure_visible on next render so the scroll isn't undone
                    view_state.viewport.set_skip_ensure_visible();
                }
            }
        }
    }

    /// Invalidate layouts for all splits viewing a specific buffer
    ///
    /// Called after buffer content changes (Insert/Delete) to mark
    /// layouts as dirty, forcing rebuild on next access.
    fn invalidate_layouts_for_buffer(&mut self, buffer_id: BufferId) {
        // Find all splits that display this buffer
        let splits_for_buffer = self.split_manager.splits_for_buffer(buffer_id);

        // Invalidate layout for each split
        for split_id in splits_for_buffer {
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.invalidate_layout();
            }
        }
    }

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

    /// Check if the editor should quit
    pub fn should_quit(&self) -> bool {
        self.should_quit
    }

    /// Check if the editor should restart with a new working directory
    pub fn should_restart(&self) -> bool {
        self.restart_with_dir.is_some()
    }

    /// Take the restart directory, clearing the restart request
    /// Returns the new working directory if a restart was requested
    pub fn take_restart_dir(&mut self) -> Option<PathBuf> {
        self.restart_with_dir.take()
    }

    /// Request the editor to restart with a new working directory
    /// This triggers a clean shutdown and restart with the new project root
    /// Request a full hardware terminal clear and redraw on the next frame.
    /// Used after external commands have messed up the terminal state.
    pub fn request_full_redraw(&mut self) {
        self.full_redraw_requested = true;
    }

    /// Check if a full redraw was requested, and clear the flag.
    pub fn take_full_redraw_request(&mut self) -> bool {
        let requested = self.full_redraw_requested;
        self.full_redraw_requested = false;
        requested
    }

    pub fn request_restart(&mut self, new_working_dir: PathBuf) {
        tracing::info!(
            "Restart requested with new working directory: {}",
            new_working_dir.display()
        );
        self.restart_with_dir = Some(new_working_dir);
        // Also signal quit so the event loop exits
        self.should_quit = true;
    }

    /// Get the active theme
    pub fn theme(&self) -> &crate::view::theme::Theme {
        &self.theme
    }

    /// Check if the settings dialog is open and visible
    pub fn is_settings_open(&self) -> bool {
        self.settings_state.as_ref().is_some_and(|s| s.visible)
    }

    /// Request the editor to quit
    pub fn quit(&mut self) {
        // Check for unsaved buffers
        let modified_count = self.count_modified_buffers();
        if modified_count > 0 {
            // Prompt user for confirmation with translated keys
            let discard_key = t!("prompt.key.discard").to_string();
            let cancel_key = t!("prompt.key.cancel").to_string();
            let msg = if modified_count == 1 {
                t!(
                    "prompt.quit_modified_one",
                    discard_key = discard_key,
                    cancel_key = cancel_key
                )
                .to_string()
            } else {
                t!(
                    "prompt.quit_modified_many",
                    count = modified_count,
                    discard_key = discard_key,
                    cancel_key = cancel_key
                )
                .to_string()
            };
            self.start_prompt(msg, PromptType::ConfirmQuitWithModified);
        } else {
            self.should_quit = true;
        }
    }

    /// Count the number of modified buffers
    fn count_modified_buffers(&self) -> usize {
        self.buffers
            .values()
            .filter(|state| state.buffer.is_modified())
            .count()
    }

    /// Resize all buffers to match new terminal size
    pub fn resize(&mut self, width: u16, height: u16) {
        // Update terminal dimensions for future buffer creation
        self.terminal_width = width;
        self.terminal_height = height;

        // Resize all SplitViewState viewports (viewport is now owned by SplitViewState)
        for view_state in self.split_view_states.values_mut() {
            view_state.viewport.resize(width, height);
        }

        // Resize visible terminal PTYs to match new dimensions
        self.resize_visible_terminals();
    }

    // Prompt/Minibuffer control methods

    /// Start a new prompt (enter minibuffer mode)
    pub fn start_prompt(&mut self, message: String, prompt_type: PromptType) {
        self.start_prompt_with_suggestions(message, prompt_type, Vec::new());
    }

    /// Start a search prompt with an optional selection scope
    ///
    /// When `use_selection_range` is true and a single-line selection is present,
    /// the search will be restricted to that range once confirmed.
    fn start_search_prompt(
        &mut self,
        message: String,
        prompt_type: PromptType,
        use_selection_range: bool,
    ) {
        // Reset any previously stored selection range
        self.pending_search_range = None;

        let selection_range = {
            let state = self.active_state();
            state.cursors.primary().selection_range()
        };

        let selected_text = if let Some(range) = selection_range.clone() {
            let state = self.active_state_mut();
            let text = state.get_text_range(range.start, range.end);
            if !text.contains('\n') && !text.is_empty() {
                Some(text)
            } else {
                None
            }
        } else {
            None
        };

        if use_selection_range {
            self.pending_search_range = selection_range;
        }

        // Determine the default text: selection > last history > empty
        let from_history = selected_text.is_none();
        let default_text = selected_text.or_else(|| {
            self.get_prompt_history("search")
                .and_then(|h| h.last().map(|s| s.to_string()))
        });

        // Start the prompt
        self.start_prompt(message, prompt_type);

        // Pre-fill with default text if available
        if let Some(text) = default_text {
            if let Some(ref mut prompt) = self.prompt {
                prompt.set_input(text.clone());
                prompt.selection_anchor = Some(0);
                prompt.cursor_pos = text.len();
            }
            if from_history {
                self.get_or_create_prompt_history("search").init_at_last();
            }
            self.update_search_highlights(&text);
        }
    }

    /// Start a new prompt with autocomplete suggestions
    pub fn start_prompt_with_suggestions(
        &mut self,
        message: String,
        prompt_type: PromptType,
        suggestions: Vec<Suggestion>,
    ) {
        // Dismiss transient popups and clear hover state when opening a prompt
        self.on_editor_focus_lost();

        // Clear search highlights when starting a new search prompt
        // This ensures old highlights from previous searches don't persist
        match prompt_type {
            PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                self.clear_search_highlights();
            }
            _ => {}
        }

        // Check if we need to update suggestions after creating the prompt
        let needs_suggestions = matches!(
            prompt_type,
            PromptType::OpenFile
                | PromptType::SwitchProject
                | PromptType::SaveFileAs
                | PromptType::Command
        );

        self.prompt = Some(Prompt::with_suggestions(message, prompt_type, suggestions));

        // For file and command prompts, populate initial suggestions
        if needs_suggestions {
            self.update_prompt_suggestions();
        }
    }

    /// Start a new prompt with initial text
    pub fn start_prompt_with_initial_text(
        &mut self,
        message: String,
        prompt_type: PromptType,
        initial_text: String,
    ) {
        // Dismiss transient popups and clear hover state when opening a prompt
        self.on_editor_focus_lost();

        self.prompt = Some(Prompt::with_initial_text(
            message,
            prompt_type,
            initial_text,
        ));
    }

    /// Cancel search/replace prompts if one is active.
    /// Called when focus leaves the editor (e.g., switching buffers, focusing file explorer).
    fn cancel_search_prompt_if_active(&mut self) {
        if let Some(ref prompt) = self.prompt {
            if matches!(
                prompt.prompt_type,
                PromptType::Search
                    | PromptType::ReplaceSearch
                    | PromptType::Replace { .. }
                    | PromptType::QueryReplaceSearch
                    | PromptType::QueryReplace { .. }
                    | PromptType::QueryReplaceConfirm
            ) {
                self.prompt = None;
                // Also cancel interactive replace if active
                self.interactive_replace_state = None;
                // Clear search highlights from current buffer
                let ns = self.search_namespace.clone();
                let state = self.active_state_mut();
                state.overlays.clear_namespace(&ns, &mut state.marker_list);
            }
        }
    }

    /// Pre-fill the Open File prompt input with the current buffer directory
    fn prefill_open_file_prompt(&mut self) {
        // With the native file browser, the directory is shown from file_open_state.current_dir
        // in the prompt rendering. The prompt.input is just the filter/filename, so we
        // start with an empty input.
        if let Some(prompt) = self.prompt.as_mut() {
            if prompt.prompt_type == PromptType::OpenFile {
                prompt.input.clear();
                prompt.cursor_pos = 0;
                prompt.selection_anchor = None;
            }
        }
    }

    /// Initialize the file open dialog state
    ///
    /// Called when the Open File prompt is started. Determines the initial directory
    /// (from current buffer's directory or working directory) and triggers async
    /// directory loading.
    fn init_file_open_state(&mut self) {
        // Determine initial directory
        let buffer_id = self.active_buffer();

        // For terminal buffers, use the terminal's initial CWD or fall back to project root
        // This avoids showing the terminal backing file directory which is confusing for users
        let initial_dir = if self.is_terminal_buffer(buffer_id) {
            self.get_terminal_id(buffer_id)
                .and_then(|tid| self.terminal_manager.get(tid))
                .and_then(|handle| handle.cwd())
                .unwrap_or_else(|| self.working_dir.clone())
        } else {
            self.active_state()
                .buffer
                .file_path()
                .and_then(|path| path.parent())
                .map(|p| p.to_path_buf())
                .unwrap_or_else(|| self.working_dir.clone())
        };

        // Create the file open state with config-based show_hidden setting
        let show_hidden = self.config.file_browser.show_hidden;
        self.file_open_state = Some(file_open::FileOpenState::new(
            initial_dir.clone(),
            show_hidden,
        ));

        // Start async directory loading
        self.load_file_open_directory(initial_dir);
    }

    /// Initialize the folder open dialog state
    ///
    /// Called when the Switch Project prompt is started. Starts from the current working
    /// directory and triggers async directory loading.
    fn init_folder_open_state(&mut self) {
        // Start from the current working directory
        let initial_dir = self.working_dir.clone();

        // Create the file open state with config-based show_hidden setting
        let show_hidden = self.config.file_browser.show_hidden;
        self.file_open_state = Some(file_open::FileOpenState::new(
            initial_dir.clone(),
            show_hidden,
        ));

        // Start async directory loading
        self.load_file_open_directory(initial_dir);
    }

    /// Change the working directory to a new path
    ///
    /// This requests a full editor restart with the new working directory.
    /// The main loop will drop the current editor instance and create a fresh
    /// one pointing to the new directory. This ensures:
    /// - All buffers are cleanly closed
    /// - LSP servers are properly shut down and restarted with new root
    /// - Plugins are cleanly restarted
    /// - No state leaks between projects
    pub fn change_working_dir(&mut self, new_path: PathBuf) {
        // Canonicalize the path to resolve symlinks and normalize
        let new_path = new_path.canonicalize().unwrap_or(new_path);

        // Request a restart with the new working directory
        // The main loop will handle creating a fresh editor instance
        self.request_restart(new_path);
    }

    /// Load directory contents for the file open dialog
    fn load_file_open_directory(&mut self, path: PathBuf) {
        // Update state to loading
        if let Some(state) = &mut self.file_open_state {
            state.current_dir = path.clone();
            state.loading = true;
            state.error = None;
            state.update_shortcuts();
        }

        // Use tokio runtime to load directory
        if let Some(ref runtime) = self.tokio_runtime {
            let fs_manager = self.fs_manager.clone();
            let sender = self.async_bridge.as_ref().map(|b| b.sender());

            runtime.spawn(async move {
                let result = fs_manager.list_dir_with_metadata(path).await;
                if let Some(sender) = sender {
                    let _ = sender.send(AsyncMessage::FileOpenDirectoryLoaded(result));
                }
            });
        } else {
            // No runtime, set error
            if let Some(state) = &mut self.file_open_state {
                state.set_error("Async runtime not available".to_string());
            }
        }
    }

    /// Handle file open directory load result
    pub(super) fn handle_file_open_directory_loaded(
        &mut self,
        result: std::io::Result<Vec<crate::services::fs::FsEntry>>,
    ) {
        match result {
            Ok(entries) => {
                if let Some(state) = &mut self.file_open_state {
                    state.set_entries(entries);
                }
                // Re-apply filter from prompt (entries were just loaded, filter needs to select matching entry)
                let filter = self
                    .prompt
                    .as_ref()
                    .map(|p| p.input.clone())
                    .unwrap_or_default();
                if !filter.is_empty() {
                    if let Some(state) = &mut self.file_open_state {
                        state.apply_filter(&filter);
                    }
                }
            }
            Err(e) => {
                if let Some(state) = &mut self.file_open_state {
                    state.set_error(e.to_string());
                }
            }
        }
    }

    /// Cancel the current prompt and return to normal mode
    pub fn cancel_prompt(&mut self) {
        // Extract theme to restore if this is a SelectTheme prompt
        let theme_to_restore = if let Some(ref prompt) = self.prompt {
            if let PromptType::SelectTheme { original_theme } = &prompt.prompt_type {
                Some(original_theme.clone())
            } else {
                None
            }
        } else {
            None
        };

        // Determine prompt type and reset appropriate history navigation
        if let Some(ref prompt) = self.prompt {
            // Reset history navigation for this prompt type
            if let Some(key) = Self::prompt_type_to_history_key(&prompt.prompt_type) {
                if let Some(history) = self.prompt_histories.get_mut(&key) {
                    history.reset_navigation();
                }
            }
            match &prompt.prompt_type {
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                    self.clear_search_highlights();
                }
                PromptType::Plugin { custom_type } => {
                    // Fire plugin hook for prompt cancellation
                    use crate::services::plugins::hooks::HookArgs;
                    self.plugin_manager.run_hook(
                        "prompt_cancelled",
                        HookArgs::PromptCancelled {
                            prompt_type: custom_type.clone(),
                            input: prompt.input.clone(),
                        },
                    );
                }
                PromptType::LspRename { overlay_handle, .. } => {
                    // Remove the rename overlay when cancelling
                    let remove_overlay_event = crate::model::event::Event::RemoveOverlay {
                        handle: overlay_handle.clone(),
                    };
                    self.apply_event_to_active_buffer(&remove_overlay_event);
                }
                PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs => {
                    // Clear file browser state
                    self.file_open_state = None;
                    self.file_browser_layout = None;
                }
                PromptType::AsyncPrompt => {
                    // Resolve the pending async prompt callback with null (cancelled)
                    if let Some(callback_id) = self.pending_async_prompt_callback.take() {
                        self.plugin_manager
                            .resolve_callback(callback_id, "null".to_string());
                    }
                }
                _ => {}
            }
        }

        self.prompt = None;
        self.pending_search_range = None;
        self.status_message = Some(t!("search.cancelled").to_string());

        // Restore original theme if we were in SelectTheme prompt
        if let Some(original_theme) = theme_to_restore {
            self.preview_theme(&original_theme);
        }
    }

    /// Handle mouse wheel scroll in prompt with suggestions.
    /// Returns true if scroll was handled, false if no prompt is active or has no suggestions.
    pub fn handle_prompt_scroll(&mut self, delta: i32) -> bool {
        if let Some(ref mut prompt) = self.prompt {
            if prompt.suggestions.is_empty() {
                return false;
            }

            let current = prompt.selected_suggestion.unwrap_or(0);
            let len = prompt.suggestions.len();

            // Calculate new position based on scroll direction
            // delta < 0 = scroll up, delta > 0 = scroll down
            let new_selected = if delta < 0 {
                // Scroll up - move selection up (decrease index)
                current.saturating_sub((-delta) as usize)
            } else {
                // Scroll down - move selection down (increase index)
                (current + delta as usize).min(len.saturating_sub(1))
            };

            prompt.selected_suggestion = Some(new_selected);

            // Update input to match selected suggestion for non-plugin prompts
            if !matches!(prompt.prompt_type, PromptType::Plugin { .. }) {
                if let Some(suggestion) = prompt.suggestions.get(new_selected) {
                    prompt.input = suggestion.get_value().to_string();
                    prompt.cursor_pos = prompt.input.len();
                }
            }

            return true;
        }
        false
    }

    /// Get the confirmed input and prompt type, consuming the prompt
    /// For command palette, returns the selected suggestion if available, otherwise the raw input
    /// Returns (input, prompt_type, selected_index)
    /// Returns None if trying to confirm a disabled command
    pub fn confirm_prompt(&mut self) -> Option<(String, PromptType, Option<usize>)> {
        if let Some(prompt) = self.prompt.take() {
            let selected_index = prompt.selected_suggestion;
            // For command, file, theme, plugin, and LSP stop prompts, prefer the selected suggestion over raw input
            let final_input = if matches!(
                prompt.prompt_type,
                PromptType::Command
                    | PromptType::OpenFile
                    | PromptType::SwitchProject
                    | PromptType::SaveFileAs
                    | PromptType::StopLspServer
                    | PromptType::SelectTheme { .. }
                    | PromptType::SelectLocale
                    | PromptType::SwitchToTab
                    | PromptType::SetLanguage
                    | PromptType::Plugin { .. }
            ) {
                // Use the selected suggestion if any
                if let Some(selected_idx) = prompt.selected_suggestion {
                    if let Some(suggestion) = prompt.suggestions.get(selected_idx) {
                        // Don't confirm disabled commands, but still record usage for history
                        if suggestion.disabled {
                            // Record usage even for disabled commands so they appear in history
                            if matches!(prompt.prompt_type, PromptType::Command) {
                                self.command_registry
                                    .write()
                                    .unwrap()
                                    .record_usage(&suggestion.text);
                            }
                            self.set_status_message(
                                t!(
                                    "error.command_not_available",
                                    command = suggestion.text.clone()
                                )
                                .to_string(),
                            );
                            return None;
                        }
                        // Use the selected suggestion value
                        suggestion.get_value().to_string()
                    } else {
                        prompt.input.clone()
                    }
                } else {
                    prompt.input.clone()
                }
            } else {
                prompt.input.clone()
            };

            // For StopLspServer, validate that the input matches a running server
            if matches!(prompt.prompt_type, PromptType::StopLspServer) {
                let is_valid = prompt
                    .suggestions
                    .iter()
                    .any(|s| s.text == final_input || s.get_value() == final_input);
                if !is_valid {
                    // Restore the prompt and don't confirm
                    self.prompt = Some(prompt);
                    self.set_status_message(
                        t!("error.no_lsp_match", input = final_input.clone()).to_string(),
                    );
                    return None;
                }
            }

            // Add to appropriate history based on prompt type
            if let Some(key) = Self::prompt_type_to_history_key(&prompt.prompt_type) {
                let history = self.get_or_create_prompt_history(&key);
                history.push(final_input.clone());
                history.reset_navigation();
            }

            Some((final_input, prompt.prompt_type, selected_index))
        } else {
            None
        }
    }

    /// Check if currently in prompt mode
    pub fn is_prompting(&self) -> bool {
        self.prompt.is_some()
    }

    /// Get or create a prompt history for the given key
    fn get_or_create_prompt_history(
        &mut self,
        key: &str,
    ) -> &mut crate::input::input_history::InputHistory {
        self.prompt_histories.entry(key.to_string()).or_default()
    }

    /// Get a prompt history for the given key (immutable)
    fn get_prompt_history(&self, key: &str) -> Option<&crate::input::input_history::InputHistory> {
        self.prompt_histories.get(key)
    }

    /// Get the history key for a prompt type
    fn prompt_type_to_history_key(prompt_type: &crate::view::prompt::PromptType) -> Option<String> {
        use crate::view::prompt::PromptType;
        match prompt_type {
            PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                Some("search".to_string())
            }
            PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                Some("replace".to_string())
            }
            PromptType::GotoLine => Some("goto_line".to_string()),
            PromptType::Plugin { custom_type } => Some(format!("plugin:{}", custom_type)),
            _ => None,
        }
    }

    /// Get the current global editor mode (e.g., "vi-normal", "vi-insert")
    /// Returns None if no special mode is active
    pub fn editor_mode(&self) -> Option<String> {
        self.editor_mode.clone()
    }

    /// Get access to the command registry
    pub fn command_registry(&self) -> &Arc<RwLock<CommandRegistry>> {
        &self.command_registry
    }

    /// Get access to the plugin manager
    pub fn plugin_manager(&self) -> &PluginManager {
        &self.plugin_manager
    }

    /// Get mutable access to the plugin manager
    pub fn plugin_manager_mut(&mut self) -> &mut PluginManager {
        &mut self.plugin_manager
    }

    /// Check if file explorer has focus
    pub fn file_explorer_is_focused(&self) -> bool {
        self.key_context == KeyContext::FileExplorer
    }

    /// Get current prompt input (for display)
    pub fn prompt_input(&self) -> Option<&str> {
        self.prompt.as_ref().map(|p| p.input.as_str())
    }

    /// Check if the active cursor currently has a selection
    pub fn has_active_selection(&self) -> bool {
        self.active_state()
            .cursors
            .primary()
            .selection_range()
            .is_some()
    }

    /// Get mutable reference to prompt (for input handling)
    pub fn prompt_mut(&mut self) -> Option<&mut Prompt> {
        self.prompt.as_mut()
    }

    /// Set a status message to display in the status bar
    pub fn set_status_message(&mut self, message: String) {
        self.plugin_status_message = None;
        self.status_message = Some(message);
    }

    /// Get the current status message
    pub fn get_status_message(&self) -> Option<&String> {
        self.plugin_status_message
            .as_ref()
            .or(self.status_message.as_ref())
    }

    /// Get accumulated plugin errors (for test assertions)
    /// Returns all error messages that were detected in plugin status messages
    pub fn get_plugin_errors(&self) -> &[String] {
        &self.plugin_errors
    }

    /// Clear accumulated plugin errors
    pub fn clear_plugin_errors(&mut self) {
        self.plugin_errors.clear();
    }

    /// Update prompt suggestions based on current input
    pub fn update_prompt_suggestions(&mut self) {
        // Extract prompt type and input to avoid borrow checker issues
        let (prompt_type, input) = if let Some(prompt) = &self.prompt {
            (prompt.prompt_type.clone(), prompt.input.clone())
        } else {
            return;
        };

        match prompt_type {
            PromptType::Command => {
                let selection_active = self.has_active_selection();
                let active_buffer_mode = self
                    .buffer_metadata
                    .get(&self.active_buffer())
                    .and_then(|m| m.virtual_mode());
                if let Some(prompt) = &mut self.prompt {
                    // Use the underlying context (not Prompt context) for filtering
                    prompt.suggestions = self.command_registry.read().unwrap().filter(
                        &input,
                        self.key_context,
                        &self.keybindings,
                        selection_active,
                        &self.active_custom_contexts,
                        active_buffer_mode,
                    );
                    prompt.selected_suggestion = if prompt.suggestions.is_empty() {
                        None
                    } else {
                        Some(0)
                    };
                }
            }
            PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                // Update incremental search highlights as user types
                self.update_search_highlights(&input);
                // Reset history navigation when user types - allows Up to navigate history
                if let Some(history) = self.prompt_histories.get_mut("search") {
                    history.reset_navigation();
                }
            }
            PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                // Reset history navigation when user types - allows Up to navigate history
                if let Some(history) = self.prompt_histories.get_mut("replace") {
                    history.reset_navigation();
                }
            }
            PromptType::GotoLine => {
                // Reset history navigation when user types - allows Up to navigate history
                if let Some(history) = self.prompt_histories.get_mut("goto_line") {
                    history.reset_navigation();
                }
            }
            PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs => {
                // For OpenFile/SwitchProject/SaveFileAs, update the file browser filter (native implementation)
                self.update_file_open_filter();
            }
            PromptType::Plugin { custom_type } => {
                // Reset history navigation when user types - allows Up to navigate history
                let key = format!("plugin:{}", custom_type);
                if let Some(history) = self.prompt_histories.get_mut(&key) {
                    history.reset_navigation();
                }
                // Fire plugin hook for prompt input change
                use crate::services::plugins::hooks::HookArgs;
                self.plugin_manager.run_hook(
                    "prompt_changed",
                    HookArgs::PromptChanged {
                        prompt_type: custom_type,
                        input,
                    },
                );
                // Apply fuzzy filtering if original_suggestions is set.
                // Note: filter_suggestions checks suggestions_set_for_input to skip
                // filtering if the plugin has already provided filtered results for
                // this input (handles the async race condition with run_hook).
                if let Some(prompt) = &mut self.prompt {
                    prompt.filter_suggestions(false);
                }
            }
            PromptType::SwitchToTab
            | PromptType::SelectTheme { .. }
            | PromptType::StopLspServer
            | PromptType::SetLanguage => {
                if let Some(prompt) = &mut self.prompt {
                    prompt.filter_suggestions(false);
                }
            }
            PromptType::SelectLocale => {
                // Locale selection also matches on description (language names)
                if let Some(prompt) = &mut self.prompt {
                    prompt.filter_suggestions(true);
                }
            }
            _ => {}
        }
    }

    /// Process pending async messages from the async bridge
    ///
    /// This should be called each frame in the main loop to handle:
    /// - LSP diagnostics
    /// - LSP initialization/errors
    /// - File system changes (future)
    /// - Git status updates
    pub fn process_async_messages(&mut self) -> bool {
        // Check plugin thread health - will panic if thread died due to error
        // This ensures plugin errors surface quickly instead of causing silent hangs
        self.plugin_manager.check_thread_health();

        let Some(bridge) = &self.async_bridge else {
            return false;
        };

        let messages = bridge.try_recv_all();
        let needs_render = !messages.is_empty();

        for message in messages {
            match message {
                AsyncMessage::LspDiagnostics { uri, diagnostics } => {
                    self.handle_lsp_diagnostics(uri, diagnostics);
                }
                AsyncMessage::LspInitialized {
                    language,
                    completion_trigger_characters,
                    semantic_tokens_legend,
                    semantic_tokens_full,
                    semantic_tokens_full_delta,
                    semantic_tokens_range,
                } => {
                    tracing::info!("LSP server initialized for language: {}", language);
                    tracing::debug!(
                        "LSP completion trigger characters for {}: {:?}",
                        language,
                        completion_trigger_characters
                    );
                    self.status_message = Some(format!("LSP ({}) ready", language));

                    // Store completion trigger characters
                    if let Some(lsp) = &mut self.lsp {
                        lsp.set_completion_trigger_characters(
                            &language,
                            completion_trigger_characters,
                        );
                        lsp.set_semantic_tokens_capabilities(
                            &language,
                            semantic_tokens_legend,
                            semantic_tokens_full,
                            semantic_tokens_full_delta,
                            semantic_tokens_range,
                        );
                    }

                    // Send didOpen for all open buffers of this language
                    self.resend_did_open_for_language(&language);
                    self.request_semantic_tokens_for_language(&language);
                }
                AsyncMessage::LspError {
                    language,
                    error,
                    stderr_log_path,
                } => {
                    tracing::error!("LSP error for {}: {}", language, error);
                    self.status_message = Some(format!("LSP error ({}): {}", language, error));

                    // Get server command from config for the hook
                    let server_command = self
                        .config
                        .lsp
                        .get(&language)
                        .map(|c| c.command.clone())
                        .unwrap_or_else(|| "unknown".to_string());

                    // Determine error type from error message
                    let error_type = if error.contains("not found") || error.contains("NotFound") {
                        "not_found"
                    } else if error.contains("permission") || error.contains("PermissionDenied") {
                        "spawn_failed"
                    } else if error.contains("timeout") {
                        "timeout"
                    } else {
                        "spawn_failed"
                    }
                    .to_string();

                    // Fire the LspServerError hook for plugins
                    self.plugin_manager.run_hook(
                        "lsp_server_error",
                        crate::services::plugins::hooks::HookArgs::LspServerError {
                            language: language.clone(),
                            server_command,
                            error_type,
                            message: error.clone(),
                        },
                    );

                    // Open stderr log as read-only buffer if it exists and has content
                    // Opens in background (new tab) without stealing focus
                    if let Some(log_path) = stderr_log_path {
                        let has_content = log_path.metadata().map(|m| m.len() > 0).unwrap_or(false);
                        if has_content {
                            tracing::info!("Opening LSP stderr log in background: {:?}", log_path);
                            match self.open_file_no_focus(&log_path) {
                                Ok(buffer_id) => {
                                    // Make the buffer read-only
                                    if let Some(state) = self.buffers.get_mut(&buffer_id) {
                                        state.editing_disabled = true;
                                    }
                                    if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id)
                                    {
                                        metadata.read_only = true;
                                    }
                                    self.status_message = Some(format!(
                                        "LSP error ({}): {} - See stderr log",
                                        language, error
                                    ));
                                }
                                Err(e) => {
                                    tracing::error!("Failed to open LSP stderr log: {}", e);
                                }
                            }
                        }
                    }
                }
                AsyncMessage::LspCompletion { request_id, items } => {
                    if let Err(e) = self.handle_completion_response(request_id, items) {
                        tracing::error!("Error handling completion response: {}", e);
                    }
                }
                AsyncMessage::LspGotoDefinition {
                    request_id,
                    locations,
                } => {
                    if let Err(e) = self.handle_goto_definition_response(request_id, locations) {
                        tracing::error!("Error handling goto definition response: {}", e);
                    }
                }
                AsyncMessage::LspRename { request_id, result } => {
                    if let Err(e) = self.handle_rename_response(request_id, result) {
                        tracing::error!("Error handling rename response: {}", e);
                    }
                }
                AsyncMessage::LspHover {
                    request_id,
                    contents,
                    is_markdown,
                    range,
                } => {
                    self.handle_hover_response(request_id, contents, is_markdown, range);
                }
                AsyncMessage::LspReferences {
                    request_id,
                    locations,
                } => {
                    if let Err(e) = self.handle_references_response(request_id, locations) {
                        tracing::error!("Error handling references response: {}", e);
                    }
                }
                AsyncMessage::LspSignatureHelp {
                    request_id,
                    signature_help,
                } => {
                    self.handle_signature_help_response(request_id, signature_help);
                }
                AsyncMessage::LspCodeActions {
                    request_id,
                    actions,
                } => {
                    self.handle_code_actions_response(request_id, actions);
                }
                AsyncMessage::LspPulledDiagnostics {
                    request_id: _,
                    uri,
                    result_id,
                    diagnostics,
                    unchanged,
                } => {
                    self.handle_lsp_pulled_diagnostics(uri, result_id, diagnostics, unchanged);
                }
                AsyncMessage::LspInlayHints {
                    request_id,
                    uri,
                    hints,
                } => {
                    self.handle_lsp_inlay_hints(request_id, uri, hints);
                }
                AsyncMessage::LspSemanticTokens {
                    request_id,
                    uri,
                    response,
                } => {
                    self.handle_lsp_semantic_tokens(request_id, uri, response);
                }
                AsyncMessage::LspServerQuiescent { language } => {
                    self.handle_lsp_server_quiescent(language);
                }
                AsyncMessage::FileChanged { path } => {
                    self.handle_async_file_changed(path);
                }
                AsyncMessage::GitStatusChanged { status } => {
                    tracing::info!("Git status changed: {}", status);
                    // TODO: Handle git status changes
                }
                AsyncMessage::FileExplorerInitialized(view) => {
                    self.handle_file_explorer_initialized(view);
                }
                AsyncMessage::FileExplorerToggleNode(node_id) => {
                    self.handle_file_explorer_toggle_node(node_id);
                }
                AsyncMessage::FileExplorerRefreshNode(node_id) => {
                    self.handle_file_explorer_refresh_node(node_id);
                }
                AsyncMessage::FileExplorerExpandedToPath(view) => {
                    self.handle_file_explorer_expanded_to_path(view);
                }
                AsyncMessage::Plugin(plugin_msg) => {
                    use fresh_core::api::{JsCallbackId, PluginAsyncMessage};
                    match plugin_msg {
                        PluginAsyncMessage::ProcessOutput {
                            process_id,
                            stdout,
                            stderr,
                            exit_code,
                        } => {
                            self.handle_plugin_process_output(
                                JsCallbackId::from(process_id),
                                stdout,
                                stderr,
                                exit_code,
                            );
                        }
                        PluginAsyncMessage::DelayComplete { callback_id } => {
                            self.plugin_manager.resolve_callback(
                                JsCallbackId::from(callback_id),
                                "null".to_string(),
                            );
                        }
                        PluginAsyncMessage::ProcessStdout { process_id, data } => {
                            self.plugin_manager.run_hook(
                                "onProcessStdout",
                                crate::services::plugins::hooks::HookArgs::ProcessOutput {
                                    process_id,
                                    data,
                                },
                            );
                        }
                        PluginAsyncMessage::ProcessStderr { process_id, data } => {
                            self.plugin_manager.run_hook(
                                "onProcessStderr",
                                crate::services::plugins::hooks::HookArgs::ProcessOutput {
                                    process_id,
                                    data,
                                },
                            );
                        }
                        PluginAsyncMessage::ProcessExit {
                            process_id,
                            callback_id,
                            exit_code,
                        } => {
                            self.background_process_handles.remove(&process_id);
                            let result = fresh_core::api::BackgroundProcessResult {
                                process_id,
                                exit_code,
                            };
                            self.plugin_manager.resolve_callback(
                                JsCallbackId::from(callback_id),
                                serde_json::to_string(&result).unwrap(),
                            );
                        }
                        PluginAsyncMessage::LspResponse {
                            language: _,
                            request_id,
                            result,
                        } => {
                            self.handle_plugin_lsp_response(request_id, result);
                        }
                        PluginAsyncMessage::PluginResponse(response) => {
                            self.handle_plugin_response(response);
                        }
                    }
                }
                AsyncMessage::LspProgress {
                    language,
                    token,
                    value,
                } => {
                    self.handle_lsp_progress(language, token, value);
                }
                AsyncMessage::LspWindowMessage {
                    language,
                    message_type,
                    message,
                } => {
                    self.handle_lsp_window_message(language, message_type, message);
                }
                AsyncMessage::LspLogMessage {
                    language,
                    message_type,
                    message,
                } => {
                    self.handle_lsp_log_message(language, message_type, message);
                }
                AsyncMessage::LspStatusUpdate {
                    language,
                    status,
                    message: _,
                } => {
                    self.handle_lsp_status_update(language, status);
                }
                AsyncMessage::FileOpenDirectoryLoaded(result) => {
                    self.handle_file_open_directory_loaded(result);
                }
                AsyncMessage::TerminalOutput { terminal_id } => {
                    // Terminal output received - check if we should auto-jump back to terminal mode
                    tracing::trace!("Terminal output received for {:?}", terminal_id);

                    // If viewing scrollback for this terminal and jump_to_end_on_output is enabled,
                    // automatically re-enter terminal mode
                    if self.config.terminal.jump_to_end_on_output && !self.terminal_mode {
                        // Check if active buffer is this terminal
                        if let Some(&active_terminal_id) =
                            self.terminal_buffers.get(&self.active_buffer())
                        {
                            if active_terminal_id == terminal_id {
                                self.enter_terminal_mode();
                            }
                        }
                    }

                    // When in terminal mode, ensure display stays at bottom (follows new output)
                    if self.terminal_mode {
                        if let Some(handle) = self.terminal_manager.get(terminal_id) {
                            if let Ok(mut state) = handle.state.lock() {
                                state.scroll_to_bottom();
                            }
                        }
                    }
                }
                AsyncMessage::TerminalExited { terminal_id } => {
                    tracing::info!("Terminal {:?} exited", terminal_id);
                    // Find the buffer associated with this terminal
                    if let Some((&buffer_id, _)) = self
                        .terminal_buffers
                        .iter()
                        .find(|(_, &tid)| tid == terminal_id)
                    {
                        // Exit terminal mode if this is the active buffer
                        if self.active_buffer() == buffer_id && self.terminal_mode {
                            self.terminal_mode = false;
                            self.key_context = crate::input::keybindings::KeyContext::Normal;
                        }

                        // Sync terminal content to buffer (final screen state)
                        self.sync_terminal_to_buffer(buffer_id);

                        // Append exit message to the backing file and reload
                        let exit_msg = "\n[Terminal process exited]\n";

                        if let Some(backing_path) =
                            self.terminal_backing_files.get(&terminal_id).cloned()
                        {
                            if let Ok(mut file) =
                                std::fs::OpenOptions::new().append(true).open(&backing_path)
                            {
                                use std::io::Write;
                                let _ = file.write_all(exit_msg.as_bytes());
                            }

                            // Force reload buffer from file to pick up the exit message
                            let _ = self.revert_buffer_by_id(buffer_id, &backing_path);
                        }

                        // Ensure buffer remains read-only with no line numbers
                        if let Some(state) = self.buffers.get_mut(&buffer_id) {
                            state.editing_disabled = true;
                            state.margins.set_line_numbers(false);
                            state.buffer.set_modified(false);
                        }

                        // Remove from terminal_buffers so it's no longer treated as a terminal
                        self.terminal_buffers.remove(&buffer_id);

                        self.set_status_message(
                            t!("terminal.exited", id = terminal_id.0).to_string(),
                        );
                    }
                    self.terminal_manager.close(terminal_id);
                }

                AsyncMessage::LspServerRequest {
                    language,
                    server_command,
                    method,
                    params,
                } => {
                    self.handle_lsp_server_request(language, server_command, method, params);
                }
                AsyncMessage::PluginLspResponse {
                    language: _,
                    request_id,
                    result,
                } => {
                    self.handle_plugin_lsp_response(request_id, result);
                }
                AsyncMessage::PluginProcessOutput {
                    process_id,
                    stdout,
                    stderr,
                    exit_code,
                } => {
                    self.handle_plugin_process_output(
                        fresh_core::api::JsCallbackId::from(process_id),
                        stdout,
                        stderr,
                        exit_code,
                    );
                }
            }
        }

        // Update plugin state snapshot BEFORE processing commands
        // This ensures plugins have access to current editor state (cursor positions, etc.)
        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();

        // Process TypeScript plugin commands
        let processed_any_commands = self.process_plugin_commands();

        // Process pending plugin action completions
        #[cfg(feature = "plugins")]
        self.process_pending_plugin_actions();

        // Process pending LSP server restarts (with exponential backoff)
        self.process_pending_lsp_restarts();

        // Check and clear the plugin render request flag
        #[cfg(feature = "plugins")]
        let plugin_render = {
            let render = self.plugin_render_requested;
            self.plugin_render_requested = false;
            render
        };
        #[cfg(not(feature = "plugins"))]
        let plugin_render = false;

        // Poll periodic update checker for new results
        if let Some(ref mut checker) = self.update_checker {
            // Poll for results but don't act on them - just cache
            let _ = checker.poll_result();
        }

        // Poll for file changes (auto-revert) and file tree changes
        let file_changes = self.poll_file_changes();
        let tree_changes = self.poll_file_tree_changes();

        // Trigger render if any async messages, plugin commands were processed, or plugin requested render
        needs_render || processed_any_commands || plugin_render || file_changes || tree_changes
    }

    /// Update LSP status bar string from active progress operations
    fn update_lsp_status_from_progress(&mut self) {
        if self.lsp_progress.is_empty() {
            // No active progress, update from server statuses
            self.update_lsp_status_from_server_statuses();
            return;
        }

        // Show the first active progress operation
        if let Some((_, info)) = self.lsp_progress.iter().next() {
            let mut status = format!("LSP ({}): {}", info.language, info.title);
            if let Some(ref msg) = info.message {
                status.push_str(&format!(" - {}", msg));
            }
            if let Some(pct) = info.percentage {
                status.push_str(&format!(" ({}%)", pct));
            }
            self.lsp_status = status;
        }
    }

    /// Update LSP status bar string from server statuses
    fn update_lsp_status_from_server_statuses(&mut self) {
        use crate::services::async_bridge::LspServerStatus;

        // Collect all server statuses
        let mut statuses: Vec<(String, LspServerStatus)> = self
            .lsp_server_statuses
            .iter()
            .map(|(lang, status)| (lang.clone(), *status))
            .collect();

        if statuses.is_empty() {
            self.lsp_status = String::new();
            return;
        }

        // Sort by language name for consistent display
        statuses.sort_by(|a, b| a.0.cmp(&b.0));

        // Build status string
        let status_parts: Vec<String> = statuses
            .iter()
            .map(|(lang, status)| {
                let status_str = match status {
                    LspServerStatus::Starting => "starting",
                    LspServerStatus::Initializing => "initializing",
                    LspServerStatus::Running => "ready",
                    LspServerStatus::Error => "error",
                    LspServerStatus::Shutdown => "shutdown",
                };
                format!("{}: {}", lang, status_str)
            })
            .collect();

        self.lsp_status = format!("LSP [{}]", status_parts.join(", "));
    }

    /// Update the plugin state snapshot with current editor state
    #[cfg(feature = "plugins")]
    fn update_plugin_state_snapshot(&mut self) {
        // Update TypeScript plugin manager state
        if let Some(snapshot_handle) = self.plugin_manager.state_snapshot_handle() {
            use fresh_core::api::{BufferInfo, CursorInfo, ViewportInfo};
            let mut snapshot = snapshot_handle.write().unwrap();

            // Update active buffer ID
            snapshot.active_buffer_id = self.active_buffer();

            // Update active split ID
            snapshot.active_split_id = self.split_manager.active_split().0;

            // Clear and update buffer info
            snapshot.buffers.clear();
            snapshot.buffer_saved_diffs.clear();
            snapshot.buffer_cursor_positions.clear();
            snapshot.buffer_text_properties.clear();

            for (buffer_id, state) in &self.buffers {
                let buffer_info = BufferInfo {
                    id: *buffer_id,
                    path: state.buffer.file_path().map(|p| p.to_path_buf()),
                    modified: state.buffer.is_modified(),
                    length: state.buffer.len(),
                };
                snapshot.buffers.insert(*buffer_id, buffer_info);

                // Skip diffing in large file mode - too expensive
                // TODO: Enable when we have an efficient streaming diff algorithm
                let is_large_file = state.buffer.line_count().is_none();
                let diff = if is_large_file {
                    BufferSavedDiff {
                        equal: !state.buffer.is_modified(),
                        byte_ranges: vec![],
                        line_ranges: None,
                    }
                } else {
                    let diff = state.buffer.diff_since_saved();
                    BufferSavedDiff {
                        equal: diff.equal,
                        byte_ranges: diff.byte_ranges.clone(),
                        line_ranges: diff.line_ranges.clone(),
                    }
                };
                snapshot.buffer_saved_diffs.insert(*buffer_id, diff);

                // Store cursor position for this buffer
                let cursor_pos = state.cursors.primary().position;
                snapshot
                    .buffer_cursor_positions
                    .insert(*buffer_id, cursor_pos);

                // Store text properties if this buffer has any
                if !state.text_properties.is_empty() {
                    snapshot
                        .buffer_text_properties
                        .insert(*buffer_id, state.text_properties.all().to_vec());
                }
            }

            // Update cursor information for active buffer
            if let Some(active_state) = self.buffers.get_mut(&self.active_buffer()) {
                // Primary cursor
                let primary = active_state.cursors.primary();
                let primary_position = primary.position;
                let primary_selection = primary.selection_range();

                snapshot.primary_cursor = Some(CursorInfo {
                    position: primary_position,
                    selection: primary_selection.clone(),
                });

                // Selected text from primary cursor (for clipboard plugin)
                snapshot.selected_text = primary_selection
                    .map(|range| active_state.get_text_range(range.start, range.end));

                // All cursors
                snapshot.all_cursors = active_state
                    .cursors
                    .iter()
                    .map(|(_, cursor)| CursorInfo {
                        position: cursor.position,
                        selection: cursor.selection_range(),
                    })
                    .collect();

                // Viewport - get from SplitViewState (the authoritative source)
                let active_split = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get(&active_split) {
                    snapshot.viewport = Some(ViewportInfo {
                        top_byte: view_state.viewport.top_byte,
                        left_column: view_state.viewport.left_column,
                        width: view_state.viewport.width,
                        height: view_state.viewport.height,
                    });
                } else {
                    snapshot.viewport = None;
                }
            } else {
                snapshot.primary_cursor = None;
                snapshot.all_cursors.clear();
                snapshot.viewport = None;
                snapshot.selected_text = None;
            }

            // Update clipboard (provide internal clipboard content to plugins)
            snapshot.clipboard = self.clipboard.get_internal().to_string();

            // Update working directory (for spawning processes in correct directory)
            snapshot.working_dir = self.working_dir.clone();

            // Update LSP diagnostics
            snapshot.diagnostics = self.stored_diagnostics.clone();

            // Update config (serialize the runtime config for plugins)
            snapshot.config = serde_json::to_value(&self.config).unwrap_or(serde_json::Value::Null);

            // Update user config (raw file contents, not merged with defaults)
            // This allows plugins to distinguish between user-set and default values
            snapshot.user_config = Config::read_user_config_raw(&self.working_dir);

            // Update editor mode (for vi mode and other modal editing)
            snapshot.editor_mode = self.editor_mode.clone();
        }
    }

    /// Handle a plugin command - dispatches to specialized handlers in plugin_commands module
    pub fn handle_plugin_command(&mut self, command: PluginCommand) -> AnyhowResult<()> {
        match command {
            // ==================== Text Editing Commands ====================
            PluginCommand::InsertText {
                buffer_id,
                position,
                text,
            } => {
                self.handle_insert_text(buffer_id, position, text);
            }
            PluginCommand::DeleteRange { buffer_id, range } => {
                self.handle_delete_range(buffer_id, range);
            }
            PluginCommand::InsertAtCursor { text } => {
                self.handle_insert_at_cursor(text);
            }
            PluginCommand::DeleteSelection => {
                self.handle_delete_selection();
            }

            // ==================== Overlay Commands ====================
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
                self.handle_add_overlay(
                    buffer_id,
                    namespace,
                    range,
                    color,
                    bg_color,
                    underline,
                    bold,
                    italic,
                    extend_to_line_end,
                );
            }
            PluginCommand::RemoveOverlay { buffer_id, handle } => {
                self.handle_remove_overlay(buffer_id, handle);
            }
            PluginCommand::ClearAllOverlays { buffer_id } => {
                self.handle_clear_all_overlays(buffer_id);
            }
            PluginCommand::ClearNamespace {
                buffer_id,
                namespace,
            } => {
                self.handle_clear_namespace(buffer_id, namespace);
            }
            PluginCommand::ClearOverlaysInRange {
                buffer_id,
                start,
                end,
            } => {
                self.handle_clear_overlays_in_range(buffer_id, start, end);
            }

            // ==================== Virtual Text Commands ====================
            PluginCommand::AddVirtualText {
                buffer_id,
                virtual_text_id,
                position,
                text,
                color,
                use_bg,
                before,
            } => {
                self.handle_add_virtual_text(
                    buffer_id,
                    virtual_text_id,
                    position,
                    text,
                    color,
                    use_bg,
                    before,
                );
            }
            PluginCommand::RemoveVirtualText {
                buffer_id,
                virtual_text_id,
            } => {
                self.handle_remove_virtual_text(buffer_id, virtual_text_id);
            }
            PluginCommand::RemoveVirtualTextsByPrefix { buffer_id, prefix } => {
                self.handle_remove_virtual_texts_by_prefix(buffer_id, prefix);
            }
            PluginCommand::ClearVirtualTexts { buffer_id } => {
                self.handle_clear_virtual_texts(buffer_id);
            }
            PluginCommand::AddVirtualLine {
                buffer_id,
                position,
                text,
                fg_color,
                bg_color,
                above,
                namespace,
                priority,
            } => {
                self.handle_add_virtual_line(
                    buffer_id, position, text, fg_color, bg_color, above, namespace, priority,
                );
            }
            PluginCommand::ClearVirtualTextNamespace {
                buffer_id,
                namespace,
            } => {
                self.handle_clear_virtual_text_namespace(buffer_id, namespace);
            }

            // ==================== Menu Commands ====================
            PluginCommand::AddMenuItem {
                menu_label,
                item,
                position,
            } => {
                self.handle_add_menu_item(menu_label, item, position);
            }
            PluginCommand::AddMenu { menu, position } => {
                self.handle_add_menu(menu, position);
            }
            PluginCommand::RemoveMenuItem {
                menu_label,
                item_label,
            } => {
                self.handle_remove_menu_item(menu_label, item_label);
            }
            PluginCommand::RemoveMenu { menu_label } => {
                self.handle_remove_menu(menu_label);
            }

            // ==================== Split Commands ====================
            PluginCommand::FocusSplit { split_id } => {
                self.handle_focus_split(split_id);
            }
            PluginCommand::SetSplitBuffer {
                split_id,
                buffer_id,
            } => {
                self.handle_set_split_buffer(split_id, buffer_id);
            }
            PluginCommand::SetSplitScroll { split_id, top_byte } => {
                self.handle_set_split_scroll(split_id, top_byte);
            }
            PluginCommand::RequestHighlights {
                buffer_id,
                range,
                request_id,
            } => {
                self.handle_request_highlights(buffer_id, range, request_id);
            }
            PluginCommand::CloseSplit { split_id } => {
                self.handle_close_split(split_id);
            }
            PluginCommand::SetSplitRatio { split_id, ratio } => {
                self.handle_set_split_ratio(split_id, ratio);
            }
            PluginCommand::DistributeSplitsEvenly { split_ids: _ } => {
                self.handle_distribute_splits_evenly();
            }
            PluginCommand::SetBufferCursor {
                buffer_id,
                position,
            } => {
                self.handle_set_buffer_cursor(buffer_id, position);
            }

            // ==================== View/Layout Commands ====================
            PluginCommand::SetLayoutHints {
                buffer_id,
                split_id,
                range: _,
                hints,
            } => {
                self.handle_set_layout_hints(buffer_id, split_id, hints);
            }
            PluginCommand::SetLineNumbers { buffer_id, enabled } => {
                self.handle_set_line_numbers(buffer_id, enabled);
            }
            PluginCommand::SubmitViewTransform {
                buffer_id,
                split_id,
                payload,
            } => {
                self.handle_submit_view_transform(buffer_id, split_id, payload);
            }
            PluginCommand::ClearViewTransform {
                buffer_id: _,
                split_id,
            } => {
                self.handle_clear_view_transform(split_id);
            }
            PluginCommand::RefreshLines { buffer_id } => {
                self.handle_refresh_lines(buffer_id);
            }
            PluginCommand::SetLineIndicator {
                buffer_id,
                line,
                namespace,
                symbol,
                color,
                priority,
            } => {
                self.handle_set_line_indicator(buffer_id, line, namespace, symbol, color, priority);
            }
            PluginCommand::ClearLineIndicators {
                buffer_id,
                namespace,
            } => {
                self.handle_clear_line_indicators(buffer_id, namespace);
            }
            PluginCommand::SetFileExplorerDecorations {
                namespace,
                decorations,
            } => {
                self.handle_set_file_explorer_decorations(namespace, decorations);
            }
            PluginCommand::ClearFileExplorerDecorations { namespace } => {
                self.handle_clear_file_explorer_decorations(&namespace);
            }

            // ==================== Status/Prompt Commands ====================
            PluginCommand::SetStatus { message } => {
                self.handle_set_status(message);
            }
            PluginCommand::ApplyTheme { theme_name } => {
                self.apply_theme(&theme_name);
            }
            PluginCommand::ReloadConfig => {
                self.reload_config();
            }
            PluginCommand::StartPrompt { label, prompt_type } => {
                self.handle_start_prompt(label, prompt_type);
            }
            PluginCommand::StartPromptWithInitial {
                label,
                prompt_type,
                initial_value,
            } => {
                self.handle_start_prompt_with_initial(label, prompt_type, initial_value);
            }
            PluginCommand::StartPromptAsync {
                label,
                initial_value,
                callback_id,
            } => {
                self.handle_start_prompt_async(label, initial_value, callback_id);
            }
            PluginCommand::SetPromptSuggestions { suggestions } => {
                self.handle_set_prompt_suggestions(suggestions);
            }

            // ==================== Command/Mode Registration ====================
            PluginCommand::RegisterCommand { command } => {
                self.handle_register_command(command);
            }
            PluginCommand::UnregisterCommand { name } => {
                self.handle_unregister_command(name);
            }
            PluginCommand::DefineMode {
                name,
                parent,
                bindings,
                read_only,
            } => {
                self.handle_define_mode(name, parent, bindings, read_only);
            }

            // ==================== File/Navigation Commands ====================
            PluginCommand::OpenFileInBackground { path } => {
                self.handle_open_file_in_background(path);
            }
            PluginCommand::OpenFileAtLocation { path, line, column } => {
                return self.handle_open_file_at_location(path, line, column);
            }
            PluginCommand::OpenFileInSplit {
                split_id,
                path,
                line,
                column,
            } => {
                return self.handle_open_file_in_split(split_id, path, line, column);
            }
            PluginCommand::ShowBuffer { buffer_id } => {
                self.handle_show_buffer(buffer_id);
            }
            PluginCommand::CloseBuffer { buffer_id } => {
                self.handle_close_buffer(buffer_id);
            }

            // ==================== LSP Commands ====================
            PluginCommand::SendLspRequest {
                language,
                method,
                params,
                request_id,
            } => {
                self.handle_send_lsp_request(language, method, params, request_id);
            }

            // ==================== Clipboard Commands ====================
            PluginCommand::SetClipboard { text } => {
                self.handle_set_clipboard(text);
            }

            // ==================== Async Plugin Commands ====================
            PluginCommand::SpawnProcess {
                command,
                args,
                cwd,
                callback_id,
            } => {
                // Spawn process asynchronously via tokio
                if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
                    let effective_cwd = cwd.unwrap_or_else(|| {
                        std::env::current_dir()
                            .map(|p| p.to_string_lossy().to_string())
                            .unwrap_or_else(|_| ".".to_string())
                    });
                    let sender = bridge.sender();
                    runtime.spawn(async move {
                        let output = tokio::process::Command::new(&command)
                            .args(&args)
                            .current_dir(&effective_cwd)
                            .output()
                            .await;

                        match output {
                            Ok(output) => {
                                let _ = sender.send(AsyncMessage::PluginProcessOutput {
                                    process_id: callback_id.as_u64(),
                                    stdout: String::from_utf8_lossy(&output.stdout).to_string(),
                                    stderr: String::from_utf8_lossy(&output.stderr).to_string(),
                                    exit_code: output.status.code().unwrap_or(-1),
                                });
                            }
                            Err(e) => {
                                let _ = sender.send(AsyncMessage::PluginProcessOutput {
                                    process_id: callback_id.as_u64(),
                                    stdout: String::new(),
                                    stderr: e.to_string(),
                                    exit_code: -1,
                                });
                            }
                        }
                    });
                } else {
                    // Fallback to blocking if no runtime available
                    let effective_cwd = cwd.unwrap_or_else(|| ".".to_string());
                    match std::process::Command::new(&command)
                        .args(&args)
                        .current_dir(&effective_cwd)
                        .output()
                    {
                        Ok(output) => {
                            // Using SpawnResult struct ensures field names match TypeScript types
                            let result = fresh_core::api::SpawnResult {
                                stdout: String::from_utf8_lossy(&output.stdout).to_string(),
                                stderr: String::from_utf8_lossy(&output.stderr).to_string(),
                                exit_code: output.status.code().unwrap_or(-1),
                            };
                            self.plugin_manager.resolve_callback(
                                callback_id,
                                serde_json::to_string(&result).unwrap(),
                            );
                        }
                        Err(e) => {
                            self.plugin_manager
                                .reject_callback(callback_id, e.to_string());
                        }
                    }
                }
            }

            PluginCommand::SpawnProcessWait {
                process_id,
                callback_id,
            } => {
                // TODO: Implement proper process wait tracking
                // For now, just reject with an error since there's no process tracking yet
                tracing::warn!(
                    "SpawnProcessWait not fully implemented - process_id={}",
                    process_id
                );
                self.plugin_manager.reject_callback(
                    callback_id,
                    format!(
                        "SpawnProcessWait not yet fully implemented for process_id={}",
                        process_id
                    ),
                );
            }

            PluginCommand::Delay {
                callback_id,
                duration_ms,
            } => {
                // Spawn async delay via tokio
                if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
                    let sender = bridge.sender();
                    let callback_id_u64 = callback_id.as_u64();
                    runtime.spawn(async move {
                        tokio::time::sleep(tokio::time::Duration::from_millis(duration_ms)).await;
                        let _ = sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                            fresh_core::api::PluginAsyncMessage::DelayComplete {
                                callback_id: callback_id_u64,
                            },
                        ));
                    });
                } else {
                    // Fallback to blocking if no runtime available
                    std::thread::sleep(std::time::Duration::from_millis(duration_ms));
                    self.plugin_manager
                        .resolve_callback(callback_id, "null".to_string());
                }
            }

            PluginCommand::SpawnBackgroundProcess {
                process_id,
                command,
                args,
                cwd,
                callback_id,
            } => {
                // Spawn background process with streaming output via tokio
                if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
                    use tokio::io::{AsyncBufReadExt, BufReader};
                    use tokio::process::Command as TokioCommand;

                    let effective_cwd = cwd.unwrap_or_else(|| {
                        std::env::current_dir()
                            .map(|p| p.to_string_lossy().to_string())
                            .unwrap_or_else(|_| ".".to_string())
                    });

                    let sender = bridge.sender();
                    let sender_stdout = sender.clone();
                    let sender_stderr = sender.clone();
                    let callback_id_u64 = callback_id.as_u64();

                    let handle = runtime.spawn(async move {
                        let mut child = match TokioCommand::new(&command)
                            .args(&args)
                            .current_dir(&effective_cwd)
                            .stdout(std::process::Stdio::piped())
                            .stderr(std::process::Stdio::piped())
                            .spawn()
                        {
                            Ok(child) => child,
                            Err(e) => {
                                let _ = sender.send(
                                    crate::services::async_bridge::AsyncMessage::Plugin(
                                        fresh_core::api::PluginAsyncMessage::ProcessExit {
                                            process_id,
                                            callback_id: callback_id_u64,
                                            exit_code: -1,
                                        },
                                    ),
                                );
                                tracing::error!("Failed to spawn background process: {}", e);
                                return;
                            }
                        };

                        // Stream stdout
                        let stdout = child.stdout.take();
                        let stderr = child.stderr.take();
                        let pid = process_id;

                        // Spawn stdout reader
                        if let Some(stdout) = stdout {
                            let sender = sender_stdout;
                            tokio::spawn(async move {
                                let reader = BufReader::new(stdout);
                                let mut lines = reader.lines();
                                while let Ok(Some(line)) = lines.next_line().await {
                                    let _ = sender.send(
                                        crate::services::async_bridge::AsyncMessage::Plugin(
                                            fresh_core::api::PluginAsyncMessage::ProcessStdout {
                                                process_id: pid,
                                                data: line + "\n",
                                            },
                                        ),
                                    );
                                }
                            });
                        }

                        // Spawn stderr reader
                        if let Some(stderr) = stderr {
                            let sender = sender_stderr;
                            tokio::spawn(async move {
                                let reader = BufReader::new(stderr);
                                let mut lines = reader.lines();
                                while let Ok(Some(line)) = lines.next_line().await {
                                    let _ = sender.send(
                                        crate::services::async_bridge::AsyncMessage::Plugin(
                                            fresh_core::api::PluginAsyncMessage::ProcessStderr {
                                                process_id: pid,
                                                data: line + "\n",
                                            },
                                        ),
                                    );
                                }
                            });
                        }

                        // Wait for process to complete
                        let exit_code = match child.wait().await {
                            Ok(status) => status.code().unwrap_or(-1),
                            Err(_) => -1,
                        };

                        let _ = sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                            fresh_core::api::PluginAsyncMessage::ProcessExit {
                                process_id,
                                callback_id: callback_id_u64,
                                exit_code,
                            },
                        ));
                    });

                    // Store abort handle for potential kill
                    self.background_process_handles
                        .insert(process_id, handle.abort_handle());
                } else {
                    // No runtime - reject immediately
                    self.plugin_manager
                        .reject_callback(callback_id, "Async runtime not available".to_string());
                }
            }

            PluginCommand::KillBackgroundProcess { process_id } => {
                if let Some(handle) = self.background_process_handles.remove(&process_id) {
                    handle.abort();
                    tracing::debug!("Killed background process {}", process_id);
                }
            }

            // ==================== Virtual Buffer Commands (complex, kept inline) ====================
            PluginCommand::CreateVirtualBuffer {
                name,
                mode,
                read_only,
            } => {
                let buffer_id = self.create_virtual_buffer(name.clone(), mode.clone(), read_only);
                tracing::info!(
                    "Created virtual buffer '{}' with mode '{}' (id={:?})",
                    name,
                    mode,
                    buffer_id
                );
                // TODO: Return buffer_id to plugin via callback or hook
            }
            PluginCommand::CreateVirtualBufferWithContent {
                name,
                mode,
                read_only,
                entries,
                show_line_numbers,
                show_cursors,
                editing_disabled,
                hidden_from_tabs,
                request_id,
            } => {
                let buffer_id = self.create_virtual_buffer(name.clone(), mode.clone(), read_only);
                tracing::info!(
                    "Created virtual buffer '{}' with mode '{}' (id={:?})",
                    name,
                    mode,
                    buffer_id
                );

                // Apply view options to the buffer
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.margins.set_line_numbers(show_line_numbers);
                    state.show_cursors = show_cursors;
                    state.editing_disabled = editing_disabled;
                    tracing::debug!(
                        "Set buffer {:?} view options: show_line_numbers={}, show_cursors={}, editing_disabled={}",
                        buffer_id,
                        show_line_numbers,
                        show_cursors,
                        editing_disabled
                    );
                }

                // Apply hidden_from_tabs to buffer metadata
                if hidden_from_tabs {
                    if let Some(meta) = self.buffer_metadata.get_mut(&buffer_id) {
                        meta.hidden_from_tabs = true;
                    }
                }

                // Now set the content
                match self.set_virtual_buffer_content(buffer_id, entries) {
                    Ok(()) => {
                        tracing::debug!("Set virtual buffer content for {:?}", buffer_id);
                        // Switch to the new buffer to display it
                        self.set_active_buffer(buffer_id);
                        tracing::debug!("Switched to virtual buffer {:?}", buffer_id);

                        // Send response if request_id is present
                        if let Some(req_id) = request_id {
                            tracing::info!(
                                "CreateVirtualBufferWithContent: resolving callback for request_id={}, buffer_id={:?}",
                                req_id,
                                buffer_id
                            );
                            // createVirtualBuffer returns VirtualBufferResult: { bufferId, splitId }
                            let result = fresh_core::api::VirtualBufferResult {
                                buffer_id: buffer_id.0 as u64,
                                split_id: None,
                            };
                            self.plugin_manager.resolve_callback(
                                fresh_core::api::JsCallbackId::from(req_id),
                                serde_json::to_string(&result).unwrap_or_default(),
                            );
                            tracing::info!("CreateVirtualBufferWithContent: resolve_callback sent for request_id={}", req_id);
                        }
                    }
                    Err(e) => {
                        tracing::error!("Failed to set virtual buffer content: {}", e);
                    }
                }
            }
            PluginCommand::CreateVirtualBufferInSplit {
                name,
                mode,
                read_only,
                entries,
                ratio,
                direction,
                panel_id,
                show_line_numbers,
                show_cursors,
                editing_disabled,
                line_wrap,
                request_id,
            } => {
                // Check if this panel already exists (for idempotent operations)
                if let Some(pid) = &panel_id {
                    if let Some(&existing_buffer_id) = self.panel_ids.get(pid) {
                        // Verify the buffer actually exists (defensive check for stale entries)
                        if self.buffers.contains_key(&existing_buffer_id) {
                            // Panel exists, just update its content
                            if let Err(e) =
                                self.set_virtual_buffer_content(existing_buffer_id, entries)
                            {
                                tracing::error!("Failed to update panel content: {}", e);
                            } else {
                                tracing::info!("Updated existing panel '{}' content", pid);
                            }

                            // Find and focus the split that contains this buffer
                            let splits = self.split_manager.splits_for_buffer(existing_buffer_id);
                            if let Some(&split_id) = splits.first() {
                                self.split_manager.set_active_split(split_id);
                                // NOTE: active_buffer is derived from split_manager,
                                // but we need to ensure the split shows the right buffer
                                self.split_manager.set_active_buffer_id(existing_buffer_id);
                                tracing::debug!(
                                    "Focused split {:?} containing panel buffer",
                                    split_id
                                );
                            }

                            // Send response with existing buffer ID and split ID via callback resolution
                            if let Some(req_id) = request_id {
                                let result = fresh_core::api::VirtualBufferResult {
                                    buffer_id: existing_buffer_id.0 as u64,
                                    split_id: splits.first().map(|s| s.0 as u64),
                                };
                                self.plugin_manager.resolve_callback(
                                    fresh_core::api::JsCallbackId::from(req_id),
                                    serde_json::to_string(&result).unwrap_or_default(),
                                );
                            }
                            return Ok(());
                        } else {
                            // Buffer no longer exists, remove stale panel_id entry
                            tracing::warn!(
                                "Removing stale panel_id '{}' pointing to non-existent buffer {:?}",
                                pid,
                                existing_buffer_id
                            );
                            self.panel_ids.remove(pid);
                            // Fall through to create a new buffer
                        }
                    }
                }

                // Create the virtual buffer first
                let buffer_id = self.create_virtual_buffer(name.clone(), mode.clone(), read_only);
                tracing::info!(
                    "Created virtual buffer '{}' with mode '{}' in split (id={:?})",
                    name,
                    mode,
                    buffer_id
                );

                // Apply view options to the buffer
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.margins.set_line_numbers(show_line_numbers);
                    state.show_cursors = show_cursors;
                    state.editing_disabled = editing_disabled;
                    tracing::debug!(
                        "Set buffer {:?} view options: show_line_numbers={}, show_cursors={}, editing_disabled={}",
                        buffer_id,
                        show_line_numbers,
                        show_cursors,
                        editing_disabled
                    );
                }

                // Store the panel ID mapping if provided
                if let Some(pid) = panel_id {
                    self.panel_ids.insert(pid, buffer_id);
                }

                // Set the content
                if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries) {
                    tracing::error!("Failed to set virtual buffer content: {}", e);
                    return Ok(());
                }

                // Save current split's view state
                self.save_current_split_view_state();

                // Determine split direction
                let split_dir = match direction.as_deref() {
                    Some("vertical") => crate::model::event::SplitDirection::Vertical,
                    _ => crate::model::event::SplitDirection::Horizontal,
                };

                // Create a split with the new buffer
                let created_split_id =
                    match self.split_manager.split_active(split_dir, buffer_id, ratio) {
                        Ok(new_split_id) => {
                            // Create independent view state for the new split with the buffer in tabs
                            let mut view_state = SplitViewState::with_buffer(
                                self.terminal_width,
                                self.terminal_height,
                                buffer_id,
                            );
                            view_state.viewport.line_wrap_enabled =
                                line_wrap.unwrap_or(self.config.editor.line_wrap);
                            self.split_view_states.insert(new_split_id, view_state);

                            // Focus the new split (the diagnostics panel)
                            self.split_manager.set_active_split(new_split_id);
                            // NOTE: split tree was updated by split_active, active_buffer derives from it

                            tracing::info!(
                                "Created {:?} split with virtual buffer {:?}",
                                split_dir,
                                buffer_id
                            );
                            Some(new_split_id)
                        }
                        Err(e) => {
                            tracing::error!("Failed to create split: {}", e);
                            // Fall back to just switching to the buffer
                            self.set_active_buffer(buffer_id);
                            None
                        }
                    };

                // Send response with buffer ID and split ID via callback resolution
                // NOTE: Using VirtualBufferResult type for type-safe JSON serialization
                if let Some(req_id) = request_id {
                    tracing::trace!("CreateVirtualBufferInSplit: resolving callback for request_id={}, buffer_id={:?}, split_id={:?}", req_id, buffer_id, created_split_id);
                    let result = fresh_core::api::VirtualBufferResult {
                        buffer_id: buffer_id.0 as u64,
                        split_id: created_split_id.map(|s| s.0 as u64),
                    };
                    self.plugin_manager.resolve_callback(
                        fresh_core::api::JsCallbackId::from(req_id),
                        serde_json::to_string(&result).unwrap_or_default(),
                    );
                }
            }
            PluginCommand::SetVirtualBufferContent { buffer_id, entries } => {
                match self.set_virtual_buffer_content(buffer_id, entries) {
                    Ok(()) => {
                        tracing::debug!("Set virtual buffer content for {:?}", buffer_id);
                    }
                    Err(e) => {
                        tracing::error!("Failed to set virtual buffer content: {}", e);
                    }
                }
            }
            PluginCommand::GetTextPropertiesAtCursor { buffer_id } => {
                // Get text properties at cursor and fire a hook with the data
                if let Some(state) = self.buffers.get(&buffer_id) {
                    let cursor_pos = state.cursors.primary().position;
                    let properties = state.text_properties.get_at(cursor_pos);
                    tracing::debug!(
                        "Text properties at cursor in {:?}: {} properties found",
                        buffer_id,
                        properties.len()
                    );
                    // TODO: Fire hook with properties data for plugins to consume
                }
            }
            PluginCommand::CreateVirtualBufferInExistingSplit {
                name,
                mode,
                read_only,
                entries,
                split_id,
                show_line_numbers,
                show_cursors,
                editing_disabled,
                line_wrap,
                request_id,
            } => {
                // Create the virtual buffer
                let buffer_id = self.create_virtual_buffer(name.clone(), mode.clone(), read_only);
                tracing::info!(
                    "Created virtual buffer '{}' with mode '{}' for existing split {:?} (id={:?})",
                    name,
                    mode,
                    split_id,
                    buffer_id
                );

                // Apply view options to the buffer
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.margins.set_line_numbers(show_line_numbers);
                    state.show_cursors = show_cursors;
                    state.editing_disabled = editing_disabled;
                }

                // Set the content
                if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries) {
                    tracing::error!("Failed to set virtual buffer content: {}", e);
                    return Ok(());
                }

                // Show the buffer in the target split
                if let Err(e) = self.split_manager.set_split_buffer(split_id, buffer_id) {
                    tracing::error!("Failed to set buffer in split {:?}: {}", split_id, e);
                    // Fall back to just switching to the buffer
                    self.set_active_buffer(buffer_id);
                } else {
                    // Focus the target split and set its buffer
                    self.split_manager.set_active_split(split_id);
                    self.split_manager.set_active_buffer_id(buffer_id);

                    // Apply line_wrap setting if provided
                    if let Some(wrap) = line_wrap {
                        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                            view_state.viewport.line_wrap_enabled = wrap;
                        }
                    }

                    tracing::info!(
                        "Displayed virtual buffer {:?} in split {:?}",
                        buffer_id,
                        split_id
                    );
                }

                // Send response with buffer ID and split ID via callback resolution
                if let Some(req_id) = request_id {
                    let result = fresh_core::api::VirtualBufferResult {
                        buffer_id: buffer_id.0 as u64,
                        split_id: Some(split_id.0 as u64),
                    };
                    self.plugin_manager.resolve_callback(
                        fresh_core::api::JsCallbackId::from(req_id),
                        serde_json::to_string(&result).unwrap_or_default(),
                    );
                }
            }

            // ==================== Context Commands ====================
            PluginCommand::SetContext { name, active } => {
                if active {
                    self.active_custom_contexts.insert(name.clone());
                    tracing::debug!("Set custom context: {}", name);
                } else {
                    self.active_custom_contexts.remove(&name);
                    tracing::debug!("Unset custom context: {}", name);
                }
            }

            // ==================== Review Diff Commands ====================
            PluginCommand::SetReviewDiffHunks { hunks } => {
                self.review_hunks = hunks;
                tracing::debug!("Set {} review hunks", self.review_hunks.len());
            }

            // ==================== Vi Mode Commands ====================
            PluginCommand::ExecuteAction { action_name } => {
                self.handle_execute_action(action_name);
            }
            PluginCommand::ExecuteActions { actions } => {
                self.handle_execute_actions(actions);
            }
            PluginCommand::GetBufferText {
                buffer_id,
                start,
                end,
                request_id,
            } => {
                self.handle_get_buffer_text(buffer_id, start, end, request_id);
            }
            PluginCommand::GetLineStartPosition {
                buffer_id,
                line,
                request_id,
            } => {
                self.handle_get_line_start_position(buffer_id, line, request_id);
            }
            PluginCommand::SetEditorMode { mode } => {
                self.handle_set_editor_mode(mode);
            }

            // ==================== LSP Helper Commands ====================
            PluginCommand::ShowActionPopup {
                popup_id,
                title,
                message,
                actions,
            } => {
                tracing::info!(
                    "Action popup requested: id={}, title={}, actions={}",
                    popup_id,
                    title,
                    actions.len()
                );

                // Build popup list items from actions
                let items: Vec<crate::model::event::PopupListItemData> = actions
                    .iter()
                    .map(|action| crate::model::event::PopupListItemData {
                        text: action.label.clone(),
                        detail: None,
                        icon: None,
                        data: Some(action.id.clone()),
                    })
                    .collect();

                // Store action info for when popup is confirmed/cancelled
                let action_ids: Vec<(String, String)> =
                    actions.into_iter().map(|a| (a.id, a.label)).collect();
                self.active_action_popup = Some((popup_id.clone(), action_ids));

                // Create popup with message + action list
                let popup = crate::model::event::PopupData {
                    title: Some(title),
                    description: Some(message),
                    transient: false,
                    content: crate::model::event::PopupContentData::List { items, selected: 0 },
                    position: crate::model::event::PopupPositionData::BottomRight,
                    width: 60,
                    max_height: 15,
                    bordered: true,
                };

                self.show_popup(popup);
                tracing::info!(
                    "Action popup shown: id={}, active_action_popup={:?}",
                    popup_id,
                    self.active_action_popup.as_ref().map(|(id, _)| id)
                );
            }

            PluginCommand::DisableLspForLanguage { language } => {
                tracing::info!("Disabling LSP for language: {}", language);

                // 1. Stop the LSP server for this language if running
                if let Some(ref mut lsp) = self.lsp {
                    lsp.shutdown_server(&language);
                    tracing::info!("Stopped LSP server for {}", language);
                }

                // 2. Update the config to disable the language
                if let Some(lsp_config) = self.config.lsp.get_mut(&language) {
                    lsp_config.enabled = false;
                    lsp_config.auto_start = false;
                    tracing::info!("Disabled LSP config for {}", language);
                }

                // 3. Persist the config change
                if let Err(e) = self.save_config() {
                    tracing::error!("Failed to save config: {}", e);
                    self.status_message = Some(format!(
                        "LSP disabled for {} (config save failed)",
                        language
                    ));
                } else {
                    self.status_message = Some(format!("LSP disabled for {}", language));
                }

                // 4. Clear any LSP-related warnings for this language
                self.warning_domains.lsp.clear();
            }

            PluginCommand::SetLspRootUri { language, uri } => {
                tracing::info!("Plugin setting LSP root URI for {}: {}", language, uri);

                // Parse the URI string into an lsp_types::Uri
                match uri.parse::<lsp_types::Uri>() {
                    Ok(parsed_uri) => {
                        if let Some(ref mut lsp) = self.lsp {
                            let restarted = lsp.set_language_root_uri(&language, parsed_uri);
                            if restarted {
                                self.status_message = Some(format!(
                                    "LSP root updated for {} (restarting server)",
                                    language
                                ));
                            } else {
                                self.status_message =
                                    Some(format!("LSP root set for {}", language));
                            }
                        }
                    }
                    Err(e) => {
                        tracing::error!("Invalid LSP root URI '{}': {}", uri, e);
                        self.status_message = Some(format!("Invalid LSP root URI: {}", e));
                    }
                }
            }

            // ==================== Scroll Sync Commands ====================
            PluginCommand::CreateScrollSyncGroup {
                group_id,
                left_split,
                right_split,
            } => {
                let success = self.scroll_sync_manager.create_group_with_id(
                    group_id,
                    left_split,
                    right_split,
                );
                if success {
                    tracing::debug!(
                        "Created scroll sync group {} for splits {:?} and {:?}",
                        group_id,
                        left_split,
                        right_split
                    );
                } else {
                    tracing::warn!(
                        "Failed to create scroll sync group {} (ID already exists)",
                        group_id
                    );
                }
            }
            PluginCommand::SetScrollSyncAnchors { group_id, anchors } => {
                use crate::view::scroll_sync::SyncAnchor;
                let anchor_count = anchors.len();
                let sync_anchors: Vec<SyncAnchor> = anchors
                    .into_iter()
                    .map(|(left_line, right_line)| SyncAnchor {
                        left_line,
                        right_line,
                    })
                    .collect();
                self.scroll_sync_manager.set_anchors(group_id, sync_anchors);
                tracing::debug!(
                    "Set {} anchors for scroll sync group {}",
                    anchor_count,
                    group_id
                );
            }
            PluginCommand::RemoveScrollSyncGroup { group_id } => {
                if self.scroll_sync_manager.remove_group(group_id) {
                    tracing::debug!("Removed scroll sync group {}", group_id);
                } else {
                    tracing::warn!("Scroll sync group {} not found", group_id);
                }
            }

            // ==================== Composite Buffer Commands ====================
            PluginCommand::CreateCompositeBuffer {
                name,
                mode,
                layout,
                sources,
                hunks,
                request_id,
            } => {
                self.handle_create_composite_buffer(name, mode, layout, sources, hunks, request_id);
            }
            PluginCommand::UpdateCompositeAlignment { buffer_id, hunks } => {
                self.handle_update_composite_alignment(buffer_id, hunks);
            }
            PluginCommand::CloseCompositeBuffer { buffer_id } => {
                self.close_composite_buffer(buffer_id);
            }

            // ==================== File Operations ====================
            PluginCommand::SaveBufferToPath { buffer_id, path } => {
                self.handle_save_buffer_to_path(buffer_id, path);
            }
        }
        Ok(())
    }

    /// Save a buffer to a specific file path (for :w filename)
    fn handle_save_buffer_to_path(&mut self, buffer_id: BufferId, path: std::path::PathBuf) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            // Save to the specified path
            match state.buffer.save_to_file(&path) {
                Ok(()) => {
                    // Update the buffer's file path so future saves go to the same file
                    state.buffer.set_file_path(path.clone());
                    // Run on-save actions (formatting, etc.)
                    let _ = self.finalize_save(Some(path));
                    tracing::debug!("Saved buffer {:?} to path", buffer_id);
                }
                Err(e) => {
                    self.handle_set_status(format!("Error saving: {}", e));
                    tracing::error!("Failed to save buffer to path: {}", e);
                }
            }
        } else {
            self.handle_set_status(format!("Buffer {:?} not found", buffer_id));
            tracing::warn!("SaveBufferToPath: buffer {:?} not found", buffer_id);
        }
    }

    /// Execute an editor action by name (for vi mode plugin)
    fn handle_execute_action(&mut self, action_name: String) {
        use crate::input::keybindings::Action;
        use std::collections::HashMap;

        // Parse the action name into an Action enum
        if let Some(action) = Action::from_str(&action_name, &HashMap::new()) {
            // Execute the action
            if let Err(e) = self.handle_action(action) {
                tracing::warn!("Failed to execute action '{}': {}", action_name, e);
            } else {
                tracing::debug!("Executed action: {}", action_name);
            }
        } else {
            tracing::warn!("Unknown action: {}", action_name);
        }
    }

    /// Execute multiple actions in sequence, each with an optional repeat count
    /// Used by vi mode for count prefix (e.g., "3dw" = delete 3 words)
    fn handle_execute_actions(&mut self, actions: Vec<fresh_core::api::ActionSpec>) {
        use crate::input::keybindings::Action;
        use std::collections::HashMap;

        for action_spec in actions {
            if let Some(action) = Action::from_str(&action_spec.action, &HashMap::new()) {
                // Execute the action `count` times
                for _ in 0..action_spec.count {
                    if let Err(e) = self.handle_action(action.clone()) {
                        tracing::warn!("Failed to execute action '{}': {}", action_spec.action, e);
                        return; // Stop on first error
                    }
                }
                tracing::debug!(
                    "Executed action '{}' {} time(s)",
                    action_spec.action,
                    action_spec.count
                );
            } else {
                tracing::warn!("Unknown action: {}", action_spec.action);
                return; // Stop on unknown action
            }
        }
    }

    /// Get text from a buffer range (for vi mode yank operations)
    fn handle_get_buffer_text(
        &mut self,
        buffer_id: BufferId,
        start: usize,
        end: usize,
        request_id: u64,
    ) {
        let result = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            // Get text from the buffer using the mutable get_text_range method
            let len = state.buffer.len();
            if start <= end && end <= len {
                Ok(state.get_text_range(start, end))
            } else {
                Err(format!(
                    "Invalid range {}..{} for buffer of length {}",
                    start, end, len
                ))
            }
        } else {
            Err(format!("Buffer {:?} not found", buffer_id))
        };

        // Resolve the JavaScript Promise callback directly
        let callback_id = fresh_core::api::JsCallbackId::from(request_id);
        match result {
            Ok(text) => {
                // Serialize text as JSON string
                let json = serde_json::to_string(&text).unwrap_or_else(|_| "null".to_string());
                self.plugin_manager.resolve_callback(callback_id, json);
            }
            Err(error) => {
                self.plugin_manager.reject_callback(callback_id, error);
            }
        }
    }

    /// Set the global editor mode (for vi mode)
    fn handle_set_editor_mode(&mut self, mode: Option<String>) {
        self.editor_mode = mode.clone();
        tracing::debug!("Set editor mode: {:?}", mode);
    }

    /// Get the byte offset of the start of a line in the active buffer
    fn handle_get_line_start_position(&mut self, buffer_id: BufferId, line: u32, request_id: u64) {
        // Use active buffer if buffer_id is 0
        let actual_buffer_id = if buffer_id.0 == 0 {
            self.active_buffer_id()
        } else {
            buffer_id
        };

        let result = if let Some(state) = self.buffers.get_mut(&actual_buffer_id) {
            // Get line start position by iterating through the buffer content
            let line_number = line as usize;
            let buffer_len = state.buffer.len();

            if line_number == 0 {
                // First line always starts at 0
                Some(0)
            } else {
                // Count newlines to find the start of the requested line
                let mut current_line = 0;
                let mut line_start = None;

                // Read buffer content to find newlines using the BufferState's get_text_range
                let content = state.get_text_range(0, buffer_len);
                for (byte_idx, c) in content.char_indices() {
                    if c == '\n' {
                        current_line += 1;
                        if current_line == line_number {
                            // Found the start of the requested line (byte after newline)
                            line_start = Some(byte_idx + 1);
                            break;
                        }
                    }
                }
                line_start
            }
        } else {
            None
        };

        // Resolve the JavaScript Promise callback directly
        let callback_id = fresh_core::api::JsCallbackId::from(request_id);
        // Serialize as JSON (null for None, number for Some)
        let json = serde_json::to_string(&result).unwrap_or_else(|_| "null".to_string());
        self.plugin_manager.resolve_callback(callback_id, json);
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
    use tempfile::TempDir;

    /// Create a test DirectoryContext with temp directories
    fn test_dir_context() -> (DirectoryContext, TempDir) {
        let temp_dir = TempDir::new().unwrap();
        let dir_context = DirectoryContext::for_testing(temp_dir.path());
        (dir_context, temp_dir)
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
        )
        .unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id: state.cursors.primary_id(),
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
        )
        .unwrap();

        // Insert multi-line text
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "line1\nline2\nline3".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to start of line 2
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
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
        )
        .unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id: state.cursors.primary_id(),
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
        )
        .unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to position 0
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
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
        )
        .unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to position 0
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
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
        )
        .unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello world".to_string(),
            cursor_id: state.cursors.primary_id(),
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
        )
        .unwrap();

        // Insert multi-line text
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "line1\nline2\nline3".to_string(),
            cursor_id: state.cursors.primary_id(),
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
        )
        .unwrap();

        // Insert some text first to have positions to place cursors
        {
            let state = editor.active_state_mut();
            state.apply(&Event::Insert {
                position: 0,
                text: "hello world test".to_string(),
                cursor_id: state.cursors.primary_id(),
            });
        }

        // Add secondary cursors at different positions to avoid normalization merging
        {
            let state = editor.active_state_mut();
            state.apply(&Event::AddCursor {
                cursor_id: CursorId(1),
                position: 5,
                anchor: None,
            });
            state.apply(&Event::AddCursor {
                cursor_id: CursorId(2),
                position: 10,
                anchor: None,
            });

            assert_eq!(state.cursors.count(), 3);
        }

        // Find the first cursor ID (the one that will be kept)
        let first_id = editor
            .active_state()
            .cursors
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
        )
        .unwrap();

        // Insert text with brackets
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "fn main() { let x = (1 + 2); }".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to opening brace '{'
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
            old_position: 31,
            new_position: 10,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        assert_eq!(state.cursors.primary().position, 10);

        // Call goto_matching_bracket
        editor.goto_matching_bracket();

        // Should move to closing brace '}' at position 29
        // "fn main() { let x = (1 + 2); }"
        //            ^                   ^
        //           10                  29
        assert_eq!(editor.active_state().cursors.primary().position, 29);
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
        )
        .unwrap();

        // Insert text with brackets
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "fn main() { let x = (1 + 2); }".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to closing paren ')'
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
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
        assert_eq!(editor.active_state().cursors.primary().position, 20);
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
        )
        .unwrap();

        // Insert text with nested brackets
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "{a{b{c}d}e}".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to first '{'
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
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
        assert_eq!(editor.active_state().cursors.primary().position, 10);
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
        )
        .unwrap();

        // Insert text
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "Hello hello HELLO".to_string(),
            cursor_id: state.cursors.primary_id(),
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
        )
        .unwrap();

        // Insert text
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "test testing tested attest test".to_string(),
            cursor_id: state.cursors.primary_id(),
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
    fn test_bookmarks() {
        let config = Config::default();
        let (dir_context, _temp) = test_dir_context();
        let mut editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
        )
        .unwrap();

        // Insert text
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "Line 1\nLine 2\nLine 3".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to line 2 start (position 7)
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
            old_position: 21,
            new_position: 7,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Set bookmark '1'
        editor.set_bookmark('1');
        assert!(editor.bookmarks.contains_key(&'1'));
        assert_eq!(editor.bookmarks.get(&'1').unwrap().position, 7);

        // Move cursor elsewhere
        let state = editor.active_state_mut();
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
            old_position: 7,
            new_position: 14,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Jump back to bookmark
        editor.jump_to_bookmark('1');
        assert_eq!(editor.active_state().cursors.primary().position, 7);

        // Clear bookmark
        editor.clear_bookmark('1');
        assert!(!editor.bookmarks.contains_key(&'1'));
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
        )
        .unwrap();

        // Set buffer content: "fn foo(val: i32) {\n    val + 1\n}\n"
        // Line 0: positions 0-19 (includes newline)
        // Line 1: positions 19-31 (includes newline)
        let initial = "fn foo(val: i32) {\n    val + 1\n}\n";
        editor.active_state_mut().buffer = Buffer::from_str(initial, 1024 * 1024);

        // Simulate LSP rename batch: rename "val" to "value" in two places
        // This is applied in reverse order to preserve positions:
        // 1. Delete "val" at position 23 (line 1, char 4), insert "value"
        // 2. Delete "val" at position 7 (line 0, char 7), insert "value"
        let cursor_id = editor.active_state().cursors.primary_id();

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
        editor.active_state_mut().apply(&batch);

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
        )
        .unwrap();

        // Set buffer content: "fn foo(val: i32) {\n    val + 1\n}\n"
        // Line 0: positions 0-19 (includes newline)
        // Line 1: positions 19-31 (includes newline)
        let initial = "fn foo(val: i32) {\n    val + 1\n}\n";
        editor.active_state_mut().buffer = Buffer::from_str(initial, 1024 * 1024);

        // Position cursor at the second "val" (position 23 = 'v' of "val" on line 1)
        let original_cursor_pos = 23;
        editor.active_state_mut().cursors.primary_mut().position = original_cursor_pos;

        // Verify cursor is at the right position
        let buffer_text = editor.active_state().buffer.to_string().unwrap();
        let text_at_cursor = buffer_text[original_cursor_pos..original_cursor_pos + 3].to_string();
        assert_eq!(text_at_cursor, "val", "Cursor should be at 'val'");

        // Simulate LSP rename batch: rename "val" to "value" in two places
        // Applied in reverse order (from end of file to start)
        let cursor_id = editor.active_state().cursors.primary_id();
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
        let final_cursor_pos = editor.active_state().cursors.primary().position;
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
        )
        .unwrap();

        // Initial content: "fn foo(val: i32) {\n    val + 1\n}\n"
        let initial = "fn foo(val: i32) {\n    val + 1\n}\n";
        editor.active_state_mut().buffer = Buffer::from_str(initial, 1024 * 1024);

        let cursor_id = editor.active_state().cursors.primary_id();
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
            .set_file_path(std::path::PathBuf::from("aaa_long_name_01.txt"));
        let buf2 = editor.new_buffer();
        editor
            .buffers
            .get_mut(&buf2)
            .unwrap()
            .buffer
            .set_file_path(std::path::PathBuf::from("bbb_long_name_02.txt"));
        let buf3 = editor.new_buffer();
        editor
            .buffers
            .get_mut(&buf3)
            .unwrap()
            .buffer
            .set_file_path(std::path::PathBuf::from("ccc_long_name_03.txt"));

        {
            let view_state = editor.split_view_states.get_mut(&split_id).unwrap();
            view_state.open_buffers = vec![buf1, buf2, buf3];
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
        let total_width: usize = view_state
            .open_buffers
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
                if idx < view_state.open_buffers.len() - 1 {
                    tab_width + 1 // separator
                } else {
                    tab_width
                }
            })
            .sum();
        assert!(view_state.tab_scroll_offset <= total_width);
    }
}
