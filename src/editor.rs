use crate::actions::action_to_events as convert_action_to_events;
use crate::async_bridge::{AsyncBridge, AsyncMessage};
use crate::buffer_mode::ModeRegistry;
use crate::command_registry::CommandRegistry;
use crate::commands::Suggestion;
use crate::config::Config;
use crate::event::{CursorId, Event, EventLog, SplitDirection, SplitId};
use crate::file_tree::{FileTree, FileTreeView};
use crate::fs::{FsBackend, FsManager, LocalFsBackend};
use crate::keybindings::{Action, KeyContext, KeybindingResolver};
use crate::lsp::{LspServerConfig};
use crate::lsp_diagnostics;
use crate::lsp_manager::{detect_language, LspManager};
use crate::multi_cursor::{
    add_cursor_above, add_cursor_at_next_match, add_cursor_below, AddCursorResult,
};
use crate::plugin_api::PluginCommand;
use crate::plugin_thread::PluginThreadHandle;
use crate::position_history::PositionHistory;
use crate::prompt::{Prompt, PromptType};
use crate::split::{SplitManager, SplitViewState};
use crate::state::EditorState;
use crate::ui::{
    FileExplorerRenderer, HelpRenderer, SplitRenderer, StatusBarRenderer, SuggestionsRenderer,
};
use crossterm::event::{KeyCode, KeyModifiers};
use lsp_types::{Position, Range as LspRange, TextDocumentContentChangeEvent};
use ratatui::{
    layout::{Constraint, Direction, Layout},
    Frame,
};
use std::collections::HashMap;
use std::io;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

// Re-export BufferId from event module for backward compatibility
pub use crate::event::BufferId;

/// Search state for find/replace functionality
#[derive(Debug, Clone)]
struct SearchState {
    /// The search query
    query: String,
    /// All match positions in the buffer (byte offsets)
    matches: Vec<usize>,
    /// Index of the currently selected match
    current_match_index: Option<usize>,
    /// Whether search wraps around at document boundaries
    wrap_search: bool,
    /// Optional search range (for search in selection)
    search_range: Option<Range<usize>>,
    /// Whether search is case-sensitive (default: true)
    case_sensitive: bool,
    /// Whether to match whole words only (default: false)
    whole_word: bool,
}

/// A bookmark in the editor (position in a specific buffer)
#[derive(Debug, Clone)]
struct Bookmark {
    /// Buffer ID where the bookmark is set
    buffer_id: BufferId,
    /// Byte offset position in the buffer
    position: usize,
}

/// State for interactive replace (query-replace)
#[derive(Debug, Clone)]
struct InteractiveReplaceState {
    /// The search pattern
    search: String,
    /// The replacement text
    replacement: String,
    /// Current match position (byte offset of the match we're at)
    current_match_pos: usize,
    /// Starting position (to detect when we've wrapped around full circle)
    start_pos: usize,
    /// Whether we've wrapped around to the beginning
    has_wrapped: bool,
    /// Number of replacements made so far
    replacements_made: usize,
}

/// The kind of buffer (file-backed or virtual)
#[derive(Debug, Clone, PartialEq)]
pub enum BufferKind {
    /// A buffer backed by a file on disk
    File {
        /// Path to the file
        path: PathBuf,
        /// LSP URI for the file
        uri: Option<lsp_types::Uri>,
    },
    /// A virtual buffer (not backed by a file)
    /// Used for special buffers like *Diagnostics*, *Grep*, etc.
    Virtual {
        /// The buffer's mode (e.g., "diagnostics-list", "grep-results")
        mode: String,
    },
}

/// Metadata associated with a buffer
#[derive(Debug, Clone)]
pub struct BufferMetadata {
    /// The kind of buffer (file or virtual)
    pub kind: BufferKind,

    /// Display name for the buffer (project-relative path or filename or *BufferName*)
    pub display_name: String,

    /// Whether LSP is enabled for this buffer (always false for virtual buffers)
    pub lsp_enabled: bool,

    /// Reason LSP is disabled (if applicable)
    pub lsp_disabled_reason: Option<String>,

    /// Whether the buffer is read-only (typically true for virtual buffers)
    pub read_only: bool,
}

impl BufferMetadata {
    /// Get the file path if this is a file-backed buffer
    pub fn file_path(&self) -> Option<&PathBuf> {
        match &self.kind {
            BufferKind::File { path, .. } => Some(path),
            BufferKind::Virtual { .. } => None,
        }
    }

    /// Get the file URI if this is a file-backed buffer
    pub fn file_uri(&self) -> Option<&lsp_types::Uri> {
        match &self.kind {
            BufferKind::File { uri, .. } => uri.as_ref(),
            BufferKind::Virtual { .. } => None,
        }
    }

    /// Check if this is a virtual buffer
    pub fn is_virtual(&self) -> bool {
        matches!(self.kind, BufferKind::Virtual { .. })
    }

    /// Get the mode name for virtual buffers
    pub fn virtual_mode(&self) -> Option<&str> {
        match &self.kind {
            BufferKind::Virtual { mode } => Some(mode),
            BufferKind::File { .. } => None,
        }
    }
}

impl BufferMetadata {
    /// Create new metadata for a buffer (unnamed, file-backed)
    pub fn new() -> Self {
        Self {
            kind: BufferKind::File {
                path: PathBuf::new(),
                uri: None,
            },
            display_name: "[No Name]".to_string(),
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: false,
        }
    }

    /// Create metadata for a file-backed buffer
    ///
    /// # Arguments
    /// * `path` - The canonical absolute path to the file
    /// * `working_dir` - The canonical working directory for computing relative display name
    pub fn with_file(path: PathBuf, working_dir: &std::path::Path) -> Self {
        // Compute URI from the absolute path
        let file_uri = url::Url::from_file_path(&path)
            .ok()
            .and_then(|u| u.as_str().parse::<lsp_types::Uri>().ok());

        // Compute display name (project-relative if possible, otherwise just filename)
        let display_name = path
            .strip_prefix(working_dir)
            .ok()
            .and_then(|rel_path| rel_path.to_str())
            .or_else(|| path.to_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| "[Unknown]".to_string());

        Self {
            kind: BufferKind::File {
                path,
                uri: file_uri,
            },
            display_name,
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: false,
        }
    }

    /// Create metadata for a virtual buffer (not backed by a file)
    ///
    /// # Arguments
    /// * `name` - Display name (e.g., "*Diagnostics*")
    /// * `mode` - Buffer mode for keybindings (e.g., "diagnostics-list")
    /// * `read_only` - Whether the buffer should be read-only
    pub fn virtual_buffer(name: String, mode: String, read_only: bool) -> Self {
        Self {
            kind: BufferKind::Virtual { mode },
            display_name: name,
            lsp_enabled: false, // Virtual buffers don't use LSP
            lsp_disabled_reason: Some("Virtual buffer".to_string()),
            read_only,
        }
    }

    /// Disable LSP for this buffer with a reason
    pub fn disable_lsp(&mut self, reason: String) {
        self.lsp_enabled = false;
        self.lsp_disabled_reason = Some(reason);
    }
}

/// Helper function to convert lsp_types::Uri to PathBuf
fn uri_to_path(uri: &lsp_types::Uri) -> Result<PathBuf, String> {
    // Convert to url::Url for path conversion
    url::Url::parse(uri.as_str())
        .map_err(|e| format!("Failed to parse URI: {}", e))?
        .to_file_path()
        .map_err(|_| "URI is not a file path".to_string())
}

/// The main editor struct - manages multiple buffers, clipboard, and rendering
pub struct Editor {
    /// All open buffers
    buffers: HashMap<BufferId, EditorState>,

    /// Currently active buffer
    active_buffer: BufferId,

    /// Event log per buffer (for undo/redo)
    event_logs: HashMap<BufferId, EventLog>,

    /// Next buffer ID to assign
    next_buffer_id: usize,

    /// Configuration
    config: Config,

    /// Active theme
    theme: crate::theme::Theme,

    /// Keybinding resolver
    keybindings: KeybindingResolver,

    /// Shared clipboard
    clipboard: String,

    /// Should the editor quit?
    should_quit: bool,

    /// Status message (shown in status bar)
    status_message: Option<String>,

    /// Help renderer
    help_renderer: HelpRenderer,

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

    /// File explorer view (optional, only when open)
    file_explorer: Option<FileTreeView>,

    /// Filesystem manager for file explorer
    fs_manager: Arc<FsManager>,

    /// Whether file explorer is visible
    file_explorer_visible: bool,

    /// Current keybinding context
    key_context: KeyContext,

    /// Menu state (active menu, highlighted item)
    menu_state: crate::ui::MenuState,

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

    /// Hover symbol range (byte offsets) - for highlighting the symbol under hover
    /// Format: (start_byte_offset, end_byte_offset)
    hover_symbol_range: Option<(usize, usize)>,

    /// Search state (if search is active)
    search_state: Option<SearchState>,

    /// Interactive replace state (if interactive replace is active)
    interactive_replace_state: Option<InteractiveReplaceState>,

    /// LSP status indicator for status bar
    lsp_status: String,

    /// Mouse state for scrollbar dragging
    mouse_state: MouseState,

    /// Cached layout areas from last render (for mouse hit testing)
    cached_layout: CachedLayout,

    /// Command registry for dynamic commands
    command_registry: Arc<RwLock<CommandRegistry>>,

    /// TypeScript plugin thread handle
    ts_plugin_manager: Option<PluginThreadHandle>,

    /// Track which lines have been seen per buffer (for lines_changed optimization)
    /// Maps buffer_id -> set of line numbers that have been processed
    seen_lines: HashMap<BufferId, std::collections::HashSet<usize>>,

    /// Named panel IDs mapping (for idempotent panel operations)
    /// Maps panel ID (e.g., "diagnostics") to buffer ID
    panel_ids: HashMap<String, BufferId>,

    /// Search history (for search and find operations)
    search_history: crate::input_history::InputHistory,

    /// Replace history (for replace operations)
    replace_history: crate::input_history::InputHistory,

    /// LSP progress tracking (token -> progress info)
    lsp_progress: std::collections::HashMap<String, LspProgressInfo>,

    /// LSP server statuses (language -> status)
    lsp_server_statuses: std::collections::HashMap<String, crate::async_bridge::LspServerStatus>,

    /// LSP window messages (recent messages from window/showMessage)
    lsp_window_messages: Vec<LspMessageEntry>,

    /// LSP log messages (recent messages from window/logMessage)
    lsp_log_messages: Vec<LspMessageEntry>,

    /// Diagnostic result IDs per URI (for incremental pull diagnostics)
    /// Maps URI string to last result_id received from server
    diagnostic_result_ids: HashMap<String, String>,

    /// Event broadcaster for control events (observable by external systems)
    event_broadcaster: crate::control_event::EventBroadcaster,

    /// Bookmarks (character key -> bookmark)
    bookmarks: HashMap<char, Bookmark>,

    /// Global search options (persist across searches)
    search_case_sensitive: bool,
    search_whole_word: bool,

    /// Macro storage (key -> list of recorded actions)
    macros: HashMap<char, Vec<Action>>,

    /// Macro recording state (Some(key) if recording, None otherwise)
    macro_recording: Option<MacroRecordingState>,

    /// Pending plugin action receivers (for async action execution)
    pending_plugin_actions: Vec<(String, crate::plugin_thread::oneshot::Receiver<anyhow::Result<()>>)>,

    /// Flag set by plugin commands that need a render (e.g., RefreshLines)
    plugin_render_requested: bool,
}

/// State for macro recording
#[derive(Debug, Clone)]
struct MacroRecordingState {
    /// The register key for this macro
    key: char,
    /// Actions recorded so far
    actions: Vec<Action>,
}

/// LSP progress information
#[derive(Debug, Clone)]
struct LspProgressInfo {
    pub language: String,
    pub title: String,
    pub message: Option<String>,
    pub percentage: Option<u32>,
}

/// LSP message entry (for window messages and logs)
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct LspMessageEntry {
    pub language: String,
    pub message_type: crate::async_bridge::LspMessageType,
    pub message: String,
    pub timestamp: std::time::Instant,
}

/// Types of UI elements that can be hovered over
#[derive(Debug, Clone, PartialEq)]
pub enum HoverTarget {
    /// Hovering over a split separator (split_id, direction)
    SplitSeparator(SplitId, SplitDirection),
    /// Hovering over a scrollbar thumb (split_id)
    ScrollbarThumb(SplitId),
    /// Hovering over a scrollbar track (split_id)
    ScrollbarTrack(SplitId),
    /// Hovering over a menu bar item (menu_index)
    MenuBarItem(usize),
    /// Hovering over a menu dropdown item (menu_index, item_index)
    MenuDropdownItem(usize, usize),
    /// Hovering over a popup list item (popup_index in stack, item_index)
    PopupListItem(usize, usize),
    /// Hovering over a suggestion item (item_index)
    SuggestionItem(usize),
}

/// Mouse state tracking
#[derive(Debug, Clone, Default)]
struct MouseState {
    /// Whether we're currently dragging a scrollbar
    dragging_scrollbar: Option<SplitId>,
    /// Last mouse position
    last_position: Option<(u16, u16)>,
    /// Initial mouse row when starting to drag the scrollbar thumb
    /// Used to calculate relative movement rather than jumping
    drag_start_row: Option<u16>,
    /// Initial viewport top_byte when starting to drag the scrollbar thumb
    drag_start_top_byte: Option<usize>,
    /// Whether we're currently dragging a split separator
    /// Stores (split_id, direction) for the separator being dragged
    dragging_separator: Option<(SplitId, SplitDirection)>,
    /// Initial mouse position when starting to drag a separator
    drag_start_position: Option<(u16, u16)>,
    /// Initial split ratio when starting to drag a separator
    drag_start_ratio: Option<f32>,
    /// Current hover target (if any)
    hover_target: Option<HoverTarget>,
}

/// Cached layout information for mouse hit testing
#[derive(Debug, Clone, Default)]
struct CachedLayout {
    /// File explorer area (if visible)
    file_explorer_area: Option<ratatui::layout::Rect>,
    /// Editor content area (excluding file explorer)
    editor_content_area: Option<ratatui::layout::Rect>,
    /// Individual split areas with their scrollbar areas and thumb positions
    /// (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end)
    split_areas: Vec<(
        SplitId,
        BufferId,
        ratatui::layout::Rect,
        ratatui::layout::Rect,
        usize,
        usize,
    )>,
    /// Split separator positions for drag resize
    /// (split_id, direction, x, y, length)
    separator_areas: Vec<(SplitId, SplitDirection, u16, u16, u16)>,
    /// Popup areas for mouse hit testing
    /// (popup_index, rect, inner_rect, scroll_offset, num_items)
    popup_areas: Vec<(usize, ratatui::layout::Rect, ratatui::layout::Rect, usize, usize)>,
    /// Suggestions area for mouse hit testing
    /// (inner_rect, scroll_start_idx, visible_count, total_count)
    suggestions_area: Option<(ratatui::layout::Rect, usize, usize, usize)>,
}

impl Editor {
    /// Create a new editor with the given configuration and terminal dimensions
    /// If working_dir is None, uses the current working directory
    pub fn new(config: Config, width: u16, height: u16) -> io::Result<Self> {
        Self::with_working_dir(config, width, height, None)
    }

    /// Create a new editor with an explicit working directory
    /// This is useful for testing with isolated temporary directories
    pub fn with_working_dir(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
    ) -> io::Result<Self> {
        Self::with_custom_backend(config, width, height, working_dir, None)
    }

    /// Create a new editor with a custom filesystem backend (for testing)
    /// This allows injecting slow or mock backends to test editor behavior
    pub fn with_fs_backend_for_test(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        fs_backend: Arc<dyn FsBackend>,
    ) -> io::Result<Self> {
        Self::with_custom_backend(config, width, height, working_dir, Some(fs_backend))
    }

    /// Create a new editor with a custom filesystem backend
    /// This is primarily used for testing with slow or mock backends
    /// to verify editor behavior under various I/O conditions
    fn with_custom_backend(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        fs_backend: Option<Arc<dyn FsBackend>>,
    ) -> io::Result<Self> {
        tracing::info!("Editor::new called with width={}, height={}", width, height);

        // Use provided working_dir or capture from environment
        let working_dir = working_dir
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        // Canonicalize working_dir to resolve symlinks and normalize path components
        // This ensures consistent path comparisons throughout the editor
        let working_dir = working_dir.canonicalize().unwrap_or_else(|_| working_dir);

        // Load theme from config
        let theme = crate::theme::Theme::from_name(&config.theme);

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
        state.viewport.line_wrap_enabled = config.editor.line_wrap;
        tracing::info!(
            "EditorState created with viewport height: {}",
            state.viewport.height
        );
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

        // Initialize plugin system
        let command_registry = Arc::new(RwLock::new(CommandRegistry::new()));

        // Initialize TypeScript plugin thread
        let ts_plugin_manager = match PluginThreadHandle::spawn(Arc::clone(&command_registry)) {
            Ok(handle) => Some(handle),
            Err(e) => {
                tracing::error!("Failed to spawn TypeScript plugin thread: {}", e);
                // In debug/test builds, panic to surface the error
                #[cfg(debug_assertions)]
                panic!("TypeScript plugin thread creation failed: {}", e);
                #[cfg(not(debug_assertions))]
                None
            }
        };

        // Load TypeScript plugins from plugins directory
        let ts_plugin_manager = ts_plugin_manager;
        if let Some(ref manager) = ts_plugin_manager {
            let plugin_dir = working_dir.join("plugins");
            if plugin_dir.exists() {
                tracing::info!("Loading TypeScript plugins from: {:?}", plugin_dir);
                let errors = manager.load_plugins_from_dir(&plugin_dir);
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
            } else {
                tracing::debug!("No plugins directory found at: {:?}", plugin_dir);
            }
        }

        Ok(Editor {
            buffers,
            active_buffer: buffer_id,
            event_logs,
            next_buffer_id: 1,
            config,
            theme,
            keybindings,
            clipboard: String::new(),
            should_quit: false,
            status_message: None,
            help_renderer: HelpRenderer::new(),
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
            file_explorer: None,
            fs_manager,
            file_explorer_visible: false,
            key_context: KeyContext::Normal,
            menu_state: crate::ui::MenuState::new(),
            working_dir,
            position_history: PositionHistory::new(),
            in_navigation: false,
            next_lsp_request_id: 0,
            pending_completion_request: None,
            pending_goto_definition_request: None,
            pending_hover_request: None,
            pending_references_request: None,
            pending_references_symbol: String::new(),
            pending_signature_help_request: None,
            pending_code_actions_request: None,
            pending_inlay_hints_request: None,
            hover_symbol_range: None,
            search_state: None,
            interactive_replace_state: None,
            lsp_status: String::new(),
            mouse_state: MouseState::default(),
            cached_layout: CachedLayout::default(),
            command_registry,
            ts_plugin_manager,
            seen_lines: HashMap::new(),
            panel_ids: HashMap::new(),
            search_history: {
                // Load search history from disk if available
                match crate::input_history::get_search_history_path() {
                    Ok(path) => {
                        crate::input_history::InputHistory::load_from_file(&path)
                            .unwrap_or_else(|e| {
                                tracing::warn!("Failed to load search history: {}", e);
                                crate::input_history::InputHistory::new()
                            })
                    }
                    Err(e) => {
                        tracing::warn!("Could not determine search history path: {}", e);
                        crate::input_history::InputHistory::new()
                    }
                }
            },
            replace_history: {
                // Load replace history from disk if available
                match crate::input_history::get_replace_history_path() {
                    Ok(path) => {
                        crate::input_history::InputHistory::load_from_file(&path)
                            .unwrap_or_else(|e| {
                                tracing::warn!("Failed to load replace history: {}", e);
                                crate::input_history::InputHistory::new()
                            })
                    }
                    Err(e) => {
                        tracing::warn!("Could not determine replace history path: {}", e);
                        crate::input_history::InputHistory::new()
                    }
                }
            },
            lsp_progress: std::collections::HashMap::new(),
            lsp_server_statuses: std::collections::HashMap::new(),
            lsp_window_messages: Vec::new(),
            lsp_log_messages: Vec::new(),
            diagnostic_result_ids: HashMap::new(),
            event_broadcaster: crate::control_event::EventBroadcaster::default(),
            bookmarks: HashMap::new(),
            search_case_sensitive: true,
            search_whole_word: false,
            macros: HashMap::new(),
            macro_recording: None,
            pending_plugin_actions: Vec::new(),
            plugin_render_requested: false,
        })
    }

    /// Get a reference to the event broadcaster
    pub fn event_broadcaster(&self) -> &crate::control_event::EventBroadcaster {
        &self.event_broadcaster
    }

    /// Get a reference to the async bridge (if available)
    pub fn async_bridge(&self) -> Option<&AsyncBridge> {
        self.async_bridge.as_ref()
    }

    /// Emit a control event
    pub fn emit_event(&self, name: impl Into<String>, data: serde_json::Value) {
        self.event_broadcaster.emit_named(name, data);
    }

    /// Send a response to a plugin for an async operation
    fn send_plugin_response(&self, response: crate::plugin_api::PluginResponse) {
        if let Some(ref manager) = self.ts_plugin_manager {
            manager.deliver_response(response);
        }
    }

    /// Get all keybindings as (key, action) pairs
    pub fn get_all_keybindings(&self) -> Vec<(String, String)> {
        self.keybindings.get_all_bindings()
    }

    /// Get mutable access to the mode registry
    pub fn mode_registry_mut(&mut self) -> &mut ModeRegistry {
        &mut self.mode_registry
    }

    /// Get immutable access to the mode registry
    pub fn mode_registry(&self) -> &ModeRegistry {
        &self.mode_registry
    }

    /// Get the mode name for the active buffer (if it's a virtual buffer)
    pub fn active_buffer_mode(&self) -> Option<&str> {
        self.buffer_metadata
            .get(&self.active_buffer)
            .and_then(|meta| meta.virtual_mode())
    }

    /// Check if the active buffer is read-only
    pub fn is_active_buffer_read_only(&self) -> bool {
        if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer) {
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

    /// Resolve a keybinding for the active buffer's mode
    ///
    /// If the active buffer has a mode (virtual buffer), check if that mode
    /// has a keybinding for the given key. Returns the command name if found.
    pub fn resolve_mode_keybinding(
        &self,
        code: KeyCode,
        modifiers: KeyModifiers,
    ) -> Option<String> {
        let mode_name = self.active_buffer_mode()?;
        self.mode_registry.resolve_keybinding(mode_name, code, modifiers)
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
        use crate::async_bridge::LspServerStatus;
        self.lsp_server_statuses
            .get(language)
            .map(|status| matches!(status, LspServerStatus::Running))
            .unwrap_or(false)
    }

    /// Get the LSP status string (displayed in status bar)
    pub fn get_lsp_status(&self) -> &str {
        &self.lsp_status
    }

    /// Configure LSP server for a specific language
    pub fn set_lsp_config(&mut self, language: String, config: LspServerConfig) {
        if let Some(ref mut lsp) = self.lsp {
            lsp.set_language_config(language, config);
        }
    }

    /// Enable event log streaming to a file
    pub fn enable_event_streaming<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        // Enable streaming for all existing event logs
        for event_log in self.event_logs.values_mut() {
            event_log.enable_streaming(&path)?;
        }
        Ok(())
    }

    /// Log keystroke for debugging
    pub fn log_keystroke(&mut self, key_code: &str, modifiers: &str) {
        if let Some(event_log) = self.event_logs.get_mut(&self.active_buffer) {
            event_log.log_keystroke(key_code, modifiers);
        }
    }

    /// Open a file and return its buffer ID
    pub fn open_file(&mut self, path: &Path) -> io::Result<BufferId> {
        // Canonicalize the path to resolve symlinks and normalize path components
        // This ensures consistent path representation throughout the editor
        let canonical_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let path = canonical_path.as_path();

        // Check if file is already open
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            // Commit pending movement before switching to existing buffer
            if id != self.active_buffer {
                self.position_history.commit_pending_movement();
                self.set_active_buffer(id);
            }
            return Ok(id);
        }

        // If the current buffer is empty and unmodified, replace it instead of creating a new one
        let replace_current = {
            let current_state = self.buffers.get(&self.active_buffer).unwrap();
            current_state.buffer.is_empty()
                && !current_state.buffer.is_modified()
                && current_state.buffer.file_path().is_none()
        };

        let buffer_id = if replace_current {
            // Reuse the current empty buffer
            self.active_buffer
        } else {
            // Create new buffer for this file
            let id = BufferId(self.next_buffer_id);
            self.next_buffer_id += 1;
            id
        };

        let mut state = EditorState::from_file(
            path,
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
        )?;
        state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Create metadata for this buffer
        let mut metadata = BufferMetadata::with_file(path.to_path_buf(), &self.working_dir);

        // Schedule LSP notification asynchronously to avoid blocking
        // This is especially important for large files
        if let Some(lsp) = &mut self.lsp {
            tracing::debug!("LSP manager available for file: {}", path.display());
            if let Some(language) = detect_language(path) {
                tracing::debug!(
                    "Detected language: {} for file: {}",
                    language,
                    path.display()
                );

                // Use the URI from metadata (already computed in with_file)
                if let Some(uri) = metadata.file_uri() {
                    tracing::debug!("Using URI from metadata: {}", uri.as_str());
                    // Get file size to decide whether to send full content
                    let file_size = std::fs::metadata(path).ok().map(|m| m.len()).unwrap_or(0);
                    let large_file_threshold = self.config.editor.large_file_threshold_bytes;

                    if file_size > large_file_threshold {
                        let reason = format!("File too large ({} bytes)", file_size);
                        tracing::warn!(
                            "Skipping LSP for large file: {} ({})",
                            path.display(),
                            reason
                        );
                        metadata.disable_lsp(reason);
                    } else {
                        // Get the text from the buffer we just loaded
                        let text = if let Some(state) = self.buffers.get(&buffer_id) {
                            state.buffer.to_string()
                        } else {
                            String::new()
                        };

                        // Spawn or get existing LSP client (non-blocking now)
                        tracing::debug!(
                            "Attempting to get or spawn LSP client for language: {}",
                            language
                        );
                        if let Some(client) = lsp.get_or_spawn(&language) {
                            tracing::info!("Sending didOpen to LSP for: {}", uri.as_str());
                            if let Err(e) = client.did_open(uri.clone(), text, language) {
                                tracing::warn!("Failed to send didOpen to LSP: {}", e);
                            } else {
                                tracing::info!("Successfully sent didOpen to LSP");

                                // Request pull diagnostics after opening the file
                                // Get previous result_id if we have one (for incremental updates)
                                let previous_result_id = self
                                    .diagnostic_result_ids
                                    .get(uri.as_str())
                                    .cloned();
                                let request_id = self.next_lsp_request_id;
                                self.next_lsp_request_id += 1;

                                if let Err(e) = client.document_diagnostic(
                                    request_id,
                                    uri.clone(),
                                    previous_result_id,
                                ) {
                                    tracing::debug!(
                                        "Failed to request pull diagnostics (server may not support): {}",
                                        e
                                    );
                                } else {
                                    tracing::info!(
                                        "Requested pull diagnostics for {} (request_id={})",
                                        uri.as_str(),
                                        request_id
                                    );
                                }

                                // Request inlay hints for the entire file
                                let request_id = self.next_lsp_request_id;
                                self.next_lsp_request_id += 1;
                                self.pending_inlay_hints_request = Some(request_id);

                                // Get buffer line count for range
                                // LSP uses 0-indexed lines, so last line is line_count - 1
                                let (last_line, last_char) = if let Some(state) = self.buffers.get(&self.active_buffer) {
                                    let line_count = state.buffer.line_count().unwrap_or(1000);
                                    // Use a large character value to include the entire last line
                                    (line_count.saturating_sub(1) as u32, 10000)
                                } else {
                                    (999, 10000) // Default fallback
                                };

                                if let Err(e) = client.inlay_hints(
                                    request_id,
                                    uri.clone(),
                                    0, 0, // start
                                    last_line, last_char, // end - last line with large char to include all content
                                ) {
                                    tracing::debug!(
                                        "Failed to request inlay hints (server may not support): {}",
                                        e
                                    );
                                    self.pending_inlay_hints_request = None;
                                } else {
                                    tracing::info!(
                                        "Requested inlay hints for {} (request_id={})",
                                        uri.as_str(),
                                        request_id
                                    );
                                }
                            }
                        } else {
                            tracing::warn!(
                                "Failed to get or spawn LSP client for language: {}",
                                language
                            );
                        }
                    }
                } else {
                    tracing::warn!(
                        "No URI in metadata for file: {} (failed to compute absolute path)",
                        path.display()
                    );
                }
            } else {
                tracing::debug!("No language detected for file: {}", path.display());
            }
        } else {
            tracing::debug!("No LSP manager available");
        }

        // Store metadata for this buffer
        self.buffer_metadata.insert(buffer_id, metadata);

        // Save current position before switching to new buffer (if not replacing current)
        if !replace_current {
            self.position_history.commit_pending_movement();

            // Explicitly record current position before switching
            let current_state = self.active_state();
            let position = current_state.cursors.primary().position;
            let anchor = current_state.cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer, position, anchor);
            self.position_history.commit_pending_movement();
        }

        self.set_active_buffer(buffer_id);
        // Use display_name from metadata for relative path display
        let display_name = self
            .buffer_metadata
            .get(&buffer_id)
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| path.display().to_string());
        self.status_message = Some(format!("Opened {}", display_name));

        // Emit control event
        self.emit_event(
            crate::control_event::events::FILE_OPENED.name,
            serde_json::json!({
                "path": path.display().to_string(),
                "buffer_id": buffer_id.0
            }),
        );

        Ok(buffer_id)
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> BufferId {
        // Save current position before switching to new buffer
        self.position_history.commit_pending_movement();

        // Explicitly record current position before switching
        let current_state = self.active_state();
        let position = current_state.cursors.primary().position;
        let anchor = current_state.cursors.primary().anchor;
        self.position_history
            .record_movement(self.active_buffer, position, anchor);
        self.position_history.commit_pending_movement();

        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
        );
        state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        self.set_active_buffer(buffer_id);
        self.status_message = Some("New buffer".to_string());

        buffer_id
    }

    /// Create a new virtual buffer (not backed by a file)
    ///
    /// # Arguments
    /// * `name` - Display name (e.g., "*Diagnostics*")
    /// * `mode` - Buffer mode for keybindings (e.g., "diagnostics-list")
    /// * `read_only` - Whether the buffer should be read-only
    ///
    /// # Returns
    /// The BufferId of the created virtual buffer
    pub fn create_virtual_buffer(
        &mut self,
        name: String,
        mode: String,
        read_only: bool,
    ) -> BufferId {
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
        );
        state.viewport.line_wrap_enabled = self.config.editor.line_wrap;

        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Set virtual buffer metadata
        let metadata = BufferMetadata::virtual_buffer(name, mode, read_only);
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add buffer to the active split's open_buffers (tabs)
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
        } else {
            // Create view state if it doesn't exist
            let mut view_state = SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id);
            view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
            self.split_view_states.insert(active_split, view_state);
        }

        buffer_id
    }

    /// Set the content of a virtual buffer with text properties
    ///
    /// # Arguments
    /// * `buffer_id` - The virtual buffer to update
    /// * `entries` - Text entries with embedded properties
    pub fn set_virtual_buffer_content(
        &mut self,
        buffer_id: BufferId,
        entries: Vec<crate::text_property::TextPropertyEntry>,
    ) -> Result<(), String> {
        let state = self
            .buffers
            .get_mut(&buffer_id)
            .ok_or_else(|| "Buffer not found".to_string())?;

        // Build text and properties from entries
        let (text, properties) =
            crate::text_property::TextPropertyManager::from_entries(entries);

        // Replace buffer content
        let current_len = state.buffer.len();
        if current_len > 0 {
            state.buffer.delete_bytes(0, current_len);
        }
        state.buffer.insert(0, &text);

        // Clear modified flag since this is virtual buffer content setting, not user edits
        state.buffer.clear_modified();

        // Set text properties
        state.text_properties = properties;

        // Reset cursor to beginning
        state.cursors.primary_mut().position = 0;
        state.cursors.primary_mut().anchor = None;

        Ok(())
    }

    /// Get text properties at the cursor position in the active buffer
    pub fn get_text_properties_at_cursor(
        &self,
    ) -> Option<Vec<&crate::text_property::TextProperty>> {
        let state = self.buffers.get(&self.active_buffer)?;
        let cursor_pos = state.cursors.primary().position;
        Some(state.text_properties.get_at(cursor_pos))
    }

    /// Close the given buffer
    pub fn close_buffer(&mut self, id: BufferId) -> io::Result<()> {
        // Can't close if it's the only buffer
        if self.buffers.len() == 1 {
            return Err(io::Error::other("Cannot close last buffer"));
        }

        // Check for unsaved changes
        if let Some(state) = self.buffers.get(&id) {
            if state.buffer.is_modified() {
                return Err(io::Error::other("Buffer has unsaved changes"));
            }
        }

        // Find a replacement buffer (any buffer that's not the one being closed)
        let replacement_buffer = *self.buffers.keys().find(|&&bid| bid != id).unwrap();

        // Update all splits that are showing this buffer to show the replacement
        let splits_to_update = self.split_manager.splits_for_buffer(id);
        for split_id in splits_to_update {
            let _ = self.split_manager.set_split_buffer(split_id, replacement_buffer);
        }

        self.buffers.remove(&id);
        self.event_logs.remove(&id);
        self.seen_lines.remove(&id);

        // Remove buffer from panel_ids mapping if it was a panel buffer
        // This prevents stale entries when the same panel_id is reused later
        self.panel_ids.retain(|_, &mut buf_id| buf_id != id);

        // Remove buffer from all splits' open_buffers lists
        for view_state in self.split_view_states.values_mut() {
            view_state.remove_buffer(id);
        }

        // Switch to another buffer if we closed the active one
        if self.active_buffer == id {
            self.set_active_buffer(replacement_buffer);
        }

        Ok(())
    }

    /// Switch to the given buffer
    pub fn switch_buffer(&mut self, id: BufferId) {
        if self.buffers.contains_key(&id) && id != self.active_buffer {
            // Save current position before switching buffers
            self.position_history.commit_pending_movement();

            // Also explicitly record current position (in case there was no pending movement)
            let current_state = self.active_state();
            let position = current_state.cursors.primary().position;
            let anchor = current_state.cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer, position, anchor);
            self.position_history.commit_pending_movement();

            self.set_active_buffer(id);
        }
    }

    /// Switch to next buffer in current split's tabs
    pub fn next_buffer(&mut self) {
        // Get the current split's open buffers
        let active_split = self.split_manager.active_split();
        let ids = if let Some(view_state) = self.split_view_states.get(&active_split) {
            view_state.open_buffers.clone()
        } else {
            // Fallback to all buffers if no view state
            let mut all_ids: Vec<_> = self.buffers.keys().copied().collect();
            all_ids.sort_by_key(|id| id.0);
            all_ids
        };

        if ids.is_empty() {
            return;
        }

        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer) {
            let next_idx = (idx + 1) % ids.len();
            if ids[next_idx] != self.active_buffer {
                // Save current position before switching
                self.position_history.commit_pending_movement();

                // Also explicitly record current position
                let current_state = self.active_state();
                let position = current_state.cursors.primary().position;
                let anchor = current_state.cursors.primary().anchor;
                self.position_history
                    .record_movement(self.active_buffer, position, anchor);
                self.position_history.commit_pending_movement();

                self.set_active_buffer(ids[next_idx]);
            }
        }
    }

    /// Switch to previous buffer in current split's tabs
    pub fn prev_buffer(&mut self) {
        // Get the current split's open buffers
        let active_split = self.split_manager.active_split();
        let ids = if let Some(view_state) = self.split_view_states.get(&active_split) {
            view_state.open_buffers.clone()
        } else {
            // Fallback to all buffers if no view state
            let mut all_ids: Vec<_> = self.buffers.keys().copied().collect();
            all_ids.sort_by_key(|id| id.0);
            all_ids
        };

        if ids.is_empty() {
            return;
        }

        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer) {
            let prev_idx = if idx == 0 { ids.len() - 1 } else { idx - 1 };
            if ids[prev_idx] != self.active_buffer {
                // Save current position before switching
                self.position_history.commit_pending_movement();

                // Also explicitly record current position
                let current_state = self.active_state();
                let position = current_state.cursors.primary().position;
                let anchor = current_state.cursors.primary().anchor;
                self.position_history
                    .record_movement(self.active_buffer, position, anchor);
                self.position_history.commit_pending_movement();

                self.set_active_buffer(ids[prev_idx]);
            }
        }
    }

    /// Navigate back in position history
    pub fn navigate_back(&mut self) {
        // Set flag to prevent recording this navigation movement
        self.in_navigation = true;

        // Commit any pending movement
        self.position_history.commit_pending_movement();

        // If we're at the end of history (haven't used back yet), save current position
        // so we can navigate forward to it later
        if self.position_history.can_go_back() && !self.position_history.can_go_forward() {
            let current_state = self.active_state();
            let position = current_state.cursors.primary().position;
            let anchor = current_state.cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer, position, anchor);
            self.position_history.commit_pending_movement();
        }

        // Navigate to the previous position
        if let Some(entry) = self.position_history.back() {
            let target_buffer = entry.buffer_id;
            let target_position = entry.position;
            let target_anchor = entry.anchor;

            // Switch to the target buffer
            if self.buffers.contains_key(&target_buffer) {
                self.set_active_buffer(target_buffer);

                // Move cursor to the saved position
                let state = self.active_state_mut();
                let cursor_id = state.cursors.primary_id();
                let old_position = state.cursors.primary().position;
                let old_anchor = state.cursors.primary().anchor;
                let old_sticky_column = state.cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                state.apply(&event);
            }
        }

        // Clear the flag
        self.in_navigation = false;
    }

    /// Navigate forward in position history
    pub fn navigate_forward(&mut self) {
        // Set flag to prevent recording this navigation movement
        self.in_navigation = true;

        if let Some(entry) = self.position_history.forward() {
            let target_buffer = entry.buffer_id;
            let target_position = entry.position;
            let target_anchor = entry.anchor;

            // Switch to the target buffer
            if self.buffers.contains_key(&target_buffer) {
                self.set_active_buffer(target_buffer);

                // Move cursor to the saved position
                let state = self.active_state_mut();
                let cursor_id = state.cursors.primary_id();
                let old_position = state.cursors.primary().position;
                let old_anchor = state.cursors.primary().anchor;
                let old_sticky_column = state.cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                state.apply(&event);
            }
        }

        // Clear the flag
        self.in_navigation = false;
    }

    /// Split the current pane horizontally
    pub fn split_pane_horizontal(&mut self) {
        // Save current split's view state before creating a new one
        self.save_current_split_view_state();

        // Share the current buffer with the new split (Emacs-style)
        let current_buffer_id = self.active_buffer;

        // Split the pane
        match self.split_manager.split_active(
            crate::event::SplitDirection::Horizontal,
            current_buffer_id,
            0.5,
        ) {
            Ok(new_split_id) => {
                // Create independent view state for the new split with the current buffer
                let mut view_state =
                    SplitViewState::with_buffer(self.terminal_width, self.terminal_height, current_buffer_id);
                view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                self.split_view_states.insert(new_split_id, view_state);
                // Restore the new split's view state to the buffer
                self.restore_current_split_view_state();
                self.set_status_message("Split pane horizontally".to_string());
            }
            Err(e) => {
                self.set_status_message(format!("Error splitting pane: {}", e));
            }
        }
    }

    /// Split the current pane vertically
    pub fn split_pane_vertical(&mut self) {
        // Save current split's view state before creating a new one
        self.save_current_split_view_state();

        // Share the current buffer with the new split (Emacs-style)
        let current_buffer_id = self.active_buffer;

        // Split the pane
        match self.split_manager.split_active(
            crate::event::SplitDirection::Vertical,
            current_buffer_id,
            0.5,
        ) {
            Ok(new_split_id) => {
                // Create independent view state for the new split with the current buffer
                let mut view_state =
                    SplitViewState::with_buffer(self.terminal_width, self.terminal_height, current_buffer_id);
                view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                self.split_view_states.insert(new_split_id, view_state);
                // Restore the new split's view state to the buffer
                self.restore_current_split_view_state();
                self.set_status_message("Split pane vertically".to_string());
            }
            Err(e) => {
                self.set_status_message(format!("Error splitting pane: {}", e));
            }
        }
    }

    /// Close the active split
    pub fn close_active_split(&mut self) {
        let active_split = self.split_manager.active_split();
        match self.split_manager.close_split(active_split) {
            Ok(_) => {
                // Clean up the view state for the closed split
                self.split_view_states.remove(&active_split);
                self.set_status_message("Closed split".to_string());
            }
            Err(e) => {
                self.set_status_message(format!("Cannot close split: {}", e));
            }
        }
    }

    /// Switch to next split
    pub fn next_split(&mut self) {
        self.save_current_split_view_state();
        self.split_manager.next_split();
        self.restore_current_split_view_state();
        self.set_status_message("Switched to next split".to_string());
    }

    /// Switch to previous split
    pub fn prev_split(&mut self) {
        self.save_current_split_view_state();
        self.split_manager.prev_split();
        self.restore_current_split_view_state();
        self.set_status_message("Switched to previous split".to_string());
    }

    /// Save the current split's cursor and viewport state
    fn save_current_split_view_state(&mut self) {
        let split_id = self.split_manager.active_split();
        if let Some(buffer_state) = self.buffers.get(&self.active_buffer) {
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.cursors = buffer_state.cursors.clone();
                view_state.viewport = buffer_state.viewport.clone();
            }
        }
    }

    /// Restore the current split's cursor and viewport state
    fn restore_current_split_view_state(&mut self) {
        let split_id = self.split_manager.active_split();
        // Update active_buffer based on the new split's buffer
        if let Some(buffer_id) = self.split_manager.active_buffer_id() {
            self.active_buffer = buffer_id;
        }
        // Restore cursor and viewport from split view state
        if let Some(view_state) = self.split_view_states.get(&split_id) {
            if let Some(buffer_state) = self.buffers.get_mut(&self.active_buffer) {
                buffer_state.cursors = view_state.cursors.clone();
                buffer_state.viewport = view_state.viewport.clone();
            }
        }
    }

    /// Adjust cursors in other splits that share the same buffer after an edit
    fn adjust_other_split_cursors_for_event(&mut self, event: &Event) {
        // Find the edit parameters from the event
        let adjustments = match event {
            Event::Insert { position, text, .. } => {
                vec![(*position, 0, text.len())]
            }
            Event::Delete { range, .. } => {
                vec![(range.start, range.len(), 0)]
            }
            Event::Batch { events, .. } => {
                // Collect all edits from the batch
                events
                    .iter()
                    .filter_map(|e| match e {
                        Event::Insert { position, text, .. } => Some((*position, 0, text.len())),
                        Event::Delete { range, .. } => Some((range.start, range.len(), 0)),
                        _ => None,
                    })
                    .collect()
            }
            _ => vec![],
        };

        if adjustments.is_empty() {
            return;
        }

        // Get the current buffer and split
        let current_buffer_id = self.active_buffer;
        let current_split_id = self.split_manager.active_split();

        // Find all other splits that share the same buffer
        let splits_for_buffer = self.split_manager.splits_for_buffer(current_buffer_id);

        // Adjust cursors in each other split's view state
        for split_id in splits_for_buffer {
            if split_id == current_split_id {
                continue; // Skip the current split (already adjusted by BufferState::apply)
            }

            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                for (edit_pos, old_len, new_len) in &adjustments {
                    view_state.cursors.adjust_for_edit(*edit_pos, *old_len, *new_len);
                }
            }
        }
    }

    /// Adjust the size of the active split
    pub fn adjust_split_size(&mut self, delta: f32) {
        let active_split = self.split_manager.active_split();
        if let Err(e) = self.split_manager.adjust_ratio(active_split, delta) {
            self.set_status_message(format!("Cannot adjust split size: {}", e));
        } else {
            self.set_status_message(format!("Adjusted split size by {:.0}%", delta * 100.0));
        }
    }

    /// Get cached separator areas for testing
    /// Returns (split_id, direction, x, y, length) tuples
    pub fn get_separator_areas(&self) -> &[(SplitId, SplitDirection, u16, u16, u16)] {
        &self.cached_layout.separator_areas
    }

    /// Get the ratio of a specific split (for testing)
    pub fn get_split_ratio(&self, split_id: SplitId) -> Option<f32> {
        self.split_manager.get_ratio(split_id)
    }

    /// Check if file explorer is visible
    pub fn file_explorer_visible(&self) -> bool {
        self.file_explorer_visible
    }

    /// Get reference to file explorer (for testing)
    pub fn file_explorer(&self) -> Option<&FileTreeView> {
        self.file_explorer.as_ref()
    }

    /// Toggle file explorer visibility
    pub fn toggle_file_explorer(&mut self) {
        self.file_explorer_visible = !self.file_explorer_visible;

        if self.file_explorer_visible {
            // Initialize file explorer if not already created
            if self.file_explorer.is_none() {
                self.init_file_explorer();
            }
            // Switch focus to file explorer when opening
            self.key_context = KeyContext::FileExplorer;
            self.set_status_message("File explorer opened".to_string());

            // Sync to current active file (fixes bug where explorer shows stale selection
            // after being hidden while user switched tabs)
            self.sync_file_explorer_to_active_file();
        } else {
            // Return focus to editor when closing
            self.key_context = KeyContext::Normal;
            self.set_status_message("File explorer closed".to_string());
        }
    }

    /// Show the file explorer (does not toggle - ensures it's visible)
    pub fn show_file_explorer(&mut self) {
        if !self.file_explorer_visible {
            self.toggle_file_explorer();
        }
    }

    /// Set the active buffer and trigger all necessary side effects
    ///
    /// This is the centralized method for switching buffers. It:
    /// - Updates self.active_buffer
    /// - Updates split manager
    /// - Adds buffer to active split's tabs (if not already there)
    /// - Syncs file explorer to the new active file (if visible)
    ///
    /// Use this instead of directly setting self.active_buffer to ensure
    /// all side effects happen consistently.
    fn set_active_buffer(&mut self, buffer_id: BufferId) {
        if self.active_buffer == buffer_id {
            return; // No change
        }

        self.active_buffer = buffer_id;

        // Update split manager to show this buffer
        self.split_manager.set_active_buffer_id(buffer_id);

        // Add buffer to the active split's open_buffers (tabs) if not already there
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
        }

        // Sync file explorer to the new active file (if visible and applicable)
        self.sync_file_explorer_to_active_file();
    }

    /// Sync file explorer to show the currently active file
    ///
    /// This expands all parent directories and selects the active file in the tree.
    /// Called automatically by set_active_buffer() when switching buffers.
    fn sync_file_explorer_to_active_file(&mut self) {
        // Only sync if file explorer is visible
        if !self.file_explorer_visible {
            return;
        }

        // Get the currently active file path
        if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer) {
            if let Some(file_path) = metadata.file_path() {
                // Clone the file path to avoid borrow checker issues
                let target_path = file_path.clone();
                let working_dir = self.working_dir.clone();

                // Check if the file is under the project root
                if target_path.starts_with(&working_dir) {
                    // Take ownership of the file explorer view to expand it asynchronously
                    if let Some(mut view) = self.file_explorer.take() {
                        if let (Some(runtime), Some(bridge)) =
                            (&self.tokio_runtime, &self.async_bridge)
                        {
                            let sender = bridge.sender();

                            runtime.spawn(async move {
                                // Expand to the target path
                                let _success = view.expand_and_select_file(&target_path).await;

                                // Send the updated view back
                                let _ = sender.send(AsyncMessage::FileExplorerExpandedToPath(view));
                            });
                        } else {
                            // No async runtime, just put the view back
                            self.file_explorer = Some(view);
                        }
                    }
                }
            }
        }
    }

    /// Focus the file explorer
    pub fn focus_file_explorer(&mut self) {
        if self.file_explorer_visible {
            // File explorer is already visible, just switch focus
            self.key_context = KeyContext::FileExplorer;
            self.set_status_message("File explorer focused".to_string());

            // Feature 7: Auto-sync to currently open file
            self.sync_file_explorer_to_active_file();
        } else {
            // Open file explorer if not visible
            self.toggle_file_explorer();
        }
    }

    /// Focus the editor (return from file explorer)
    pub fn focus_editor(&mut self) {
        self.key_context = KeyContext::Normal;
        self.set_status_message("Editor focused".to_string());
    }

    /// Initialize the file explorer
    fn init_file_explorer(&mut self) {
        // Use the captured working directory from initialization time
        let root_path = self.working_dir.clone();

        // Spawn async task to initialize file tree
        if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
            let fs_manager = Arc::clone(&self.fs_manager);
            let sender = bridge.sender();

            runtime.spawn(async move {
                match FileTree::new(root_path, fs_manager).await {
                    Ok(mut tree) => {
                        // Expand the root directory by default so files are visible immediately
                        let root_id = tree.root_id();
                        if let Err(e) = tree.expand_node(root_id).await {
                            tracing::warn!("Failed to expand root directory: {}", e);
                        }

                        let view = FileTreeView::new(tree);
                        let _ = sender.send(AsyncMessage::FileExplorerInitialized(view));
                    }
                    Err(e) => {
                        tracing::error!("Failed to initialize file explorer: {}", e);
                        // Could add an error variant to AsyncMessage if needed
                    }
                }
            });

            self.set_status_message("Initializing file explorer...".to_string());
        }
    }

    /// Handle file explorer navigation up
    pub fn file_explorer_navigate_up(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_prev();
            // Update scroll using stored viewport_height (set during rendering)
            explorer.update_scroll_for_selection();
        }
    }

    /// Handle file explorer navigation down
    pub fn file_explorer_navigate_down(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_next();
            // Update scroll using stored viewport_height (set during rendering)
            explorer.update_scroll_for_selection();
        }
    }

    /// Handle file explorer page up
    pub fn file_explorer_page_up(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_page_up();
            // Update scroll using stored viewport_height (set during rendering)
            explorer.update_scroll_for_selection();
        }
    }

    /// Handle file explorer page down
    pub fn file_explorer_page_down(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_page_down();
            // Update scroll using stored viewport_height (set during rendering)
            explorer.update_scroll_for_selection();
        }
    }

    /// Handle file explorer expand/collapse
    pub fn file_explorer_toggle_expand(&mut self) {
        // Extract needed data first
        let selected_id = if let Some(explorer) = &self.file_explorer {
            explorer.get_selected()
        } else {
            return;
        };

        let Some(selected_id) = selected_id else {
            return;
        };

        // Check if node is a directory and get its state
        let (is_dir, is_expanded, name) = if let Some(explorer) = &self.file_explorer {
            let node = explorer.tree().get_node(selected_id);
            if let Some(node) = node {
                (node.is_dir(), node.is_expanded(), node.entry.name.clone())
            } else {
                return;
            }
        } else {
            return;
        };

        if !is_dir {
            return; // Can't toggle files
        }

        // Show status based on current state
        let status_msg = if is_expanded {
            "Collapsing...".to_string()
        } else {
            format!("Loading {}...", name)
        };
        self.set_status_message(status_msg);

        // TODO: Refactor to use Arc<Mutex<FileTree>> for true non-blocking async
        // Current approach: block_on is acceptable for local FS (<100ms typically)
        // but needs architectural change for network FS support
        if let (Some(runtime), Some(explorer)) = (&self.tokio_runtime, &mut self.file_explorer) {
            let tree = explorer.tree_mut();
            let result = runtime.block_on(tree.toggle_node(selected_id));

            // Get final state for status message
            let final_name = explorer
                .tree()
                .get_node(selected_id)
                .map(|n| n.entry.name.clone());
            let final_expanded = explorer
                .tree()
                .get_node(selected_id)
                .map(|n| n.is_expanded())
                .unwrap_or(false);

            match result {
                Ok(()) => {
                    // If directory was just expanded, load its .gitignore
                    if final_expanded {
                        // Extract the path before calling mutable method
                        let dir_path = explorer
                            .tree()
                            .get_node(selected_id)
                            .map(|n| n.entry.path.clone());

                        if let Some(dir_path) = dir_path {
                            // Load .gitignore for this directory
                            if let Err(e) = explorer.load_gitignore_for_dir(&dir_path) {
                                tracing::warn!(
                                    "Failed to load .gitignore from {:?}: {}",
                                    dir_path,
                                    e
                                );
                                // Don't fail the expansion, just log the warning
                            }
                        }
                    }

                    if let Some(name) = final_name {
                        let msg = if final_expanded {
                            format!("Expanded: {}", name)
                        } else {
                            format!("Collapsed: {}", name)
                        };
                        self.set_status_message(msg);
                    }
                }
                Err(e) => {
                    self.set_status_message(format!("Error: {}", e));
                }
            }
        }
    }

    /// Handle file explorer open file or toggle directory
    /// When Enter is pressed:
    /// - On a directory: toggle expand/collapse
    /// - On a file: open the file and switch focus to editor
    pub fn file_explorer_open_file(&mut self) -> io::Result<()> {
        // Check if selected entry is a directory or file
        let entry_type = self
            .file_explorer
            .as_ref()
            .and_then(|explorer| explorer.get_selected_entry())
            .map(|entry| (entry.is_dir(), entry.path.clone(), entry.name.clone()));

        if let Some((is_dir, path, name)) = entry_type {
            if is_dir {
                // Toggle expand/collapse for directories
                self.file_explorer_toggle_expand();
            } else {
                // Open file and switch focus to editor
                self.open_file(&path)?;
                self.set_status_message(format!("Opened: {}", name));
                // Switch focus to editor after opening file
                self.focus_editor();
            }
        }
        Ok(())
    }

    /// Handle file explorer refresh
    pub fn file_explorer_refresh(&mut self) {
        // Extract needed data first
        let (selected_id, node_name) = if let Some(explorer) = &self.file_explorer {
            if let Some(selected_id) = explorer.get_selected() {
                let node_name = explorer
                    .tree()
                    .get_node(selected_id)
                    .map(|n| n.entry.name.clone());
                (Some(selected_id), node_name)
            } else {
                (None, None)
            }
        } else {
            return;
        };

        let Some(selected_id) = selected_id else {
            return;
        };

        // Show loading status
        if let Some(name) = &node_name {
            self.set_status_message(format!("Refreshing {}...", name));
        }

        // TODO: Refactor to use Arc<Mutex<FileTree>> for true non-blocking async
        // Current approach: block_on is acceptable for local FS (<100ms typically)
        // but needs architectural change for network FS support
        if let (Some(runtime), Some(explorer)) = (&self.tokio_runtime, &mut self.file_explorer) {
            let tree = explorer.tree_mut();
            let result = runtime.block_on(tree.refresh_node(selected_id));
            match result {
                Ok(()) => {
                    if let Some(name) = node_name {
                        self.set_status_message(format!("Refreshed: {}", name));
                    } else {
                        self.set_status_message("Refreshed".to_string());
                    }
                }
                Err(e) => {
                    self.set_status_message(format!("Error refreshing: {}", e));
                }
            }
        }
    }

    /// Handle creating a new file in the file explorer
    pub fn file_explorer_new_file(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            if let Some(selected_id) = explorer.get_selected() {
                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    // Get the directory to create the file in
                    let parent_path = if node.is_dir() {
                        node.entry.path.clone()
                    } else {
                        // If file selected, use its parent directory
                        node.entry
                            .path
                            .parent()
                            .map(|p| p.to_path_buf())
                            .unwrap_or_else(|| node.entry.path.clone())
                    };

                    // TODO: Implement input dialog to get filename from user
                    // For now, use a default name with timestamp to avoid conflicts
                    let timestamp = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs();
                    let filename = format!("untitled_{}.txt", timestamp);
                    let file_path = parent_path.join(&filename);

                    // Create the file asynchronously
                    if let Some(runtime) = &self.tokio_runtime {
                        let path_clone = file_path.clone();
                        let selected_id = selected_id;
                        let result =
                            runtime.block_on(async { tokio::fs::File::create(&path_clone).await });

                        match result {
                            Ok(_) => {
                                // Refresh the parent directory to show the new file
                                let parent_id = if node.is_dir() {
                                    selected_id
                                } else {
                                    explorer
                                        .tree()
                                        .get_node(selected_id)
                                        .and_then(|n| n.parent)
                                        .unwrap_or(selected_id)
                                };

                                let _ =
                                    runtime.block_on(explorer.tree_mut().refresh_node(parent_id));

                                // Try to open the new file
                                if let Ok(_) = self.open_file(&file_path) {
                                    self.set_status_message(format!(
                                        "Created and opened: {}",
                                        filename
                                    ));
                                } else {
                                    self.set_status_message(format!("Created: {}", filename));
                                }
                            }
                            Err(e) => {
                                self.set_status_message(format!("Failed to create file: {}", e));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Handle creating a new directory in the file explorer
    pub fn file_explorer_new_directory(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            if let Some(selected_id) = explorer.get_selected() {
                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    // Get the directory to create the new directory in
                    let parent_path = if node.is_dir() {
                        node.entry.path.clone()
                    } else {
                        // If file selected, use its parent directory
                        node.entry
                            .path
                            .parent()
                            .map(|p| p.to_path_buf())
                            .unwrap_or_else(|| node.entry.path.clone())
                    };

                    // TODO: Implement input dialog to get directory name from user
                    // For now, use a default name with timestamp to avoid conflicts
                    let timestamp = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs();
                    let dirname = format!("new_folder_{}", timestamp);
                    let dir_path = parent_path.join(&dirname);

                    // Create the directory asynchronously
                    if let Some(runtime) = &self.tokio_runtime {
                        let path_clone = dir_path.clone();
                        let result =
                            runtime.block_on(async { tokio::fs::create_dir(&path_clone).await });

                        match result {
                            Ok(_) => {
                                // Refresh the parent directory to show the new folder
                                let parent_id = if node.is_dir() {
                                    selected_id
                                } else {
                                    explorer
                                        .tree()
                                        .get_node(selected_id)
                                        .and_then(|n| n.parent)
                                        .unwrap_or(selected_id)
                                };

                                let _ =
                                    runtime.block_on(explorer.tree_mut().refresh_node(parent_id));
                                self.set_status_message(format!("Created directory: {}", dirname));
                            }
                            Err(e) => {
                                self.set_status_message(format!(
                                    "Failed to create directory: {}",
                                    e
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Handle deleting a file or directory from the file explorer
    pub fn file_explorer_delete(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            if let Some(selected_id) = explorer.get_selected() {
                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    // Don't allow deleting the root
                    if node.parent.is_none() {
                        self.set_status_message("Cannot delete root directory".to_string());
                        return;
                    }

                    let path = node.entry.path.clone();
                    let name = node.entry.name.clone();
                    let is_dir = node.is_dir();
                    let parent_id = node.parent;

                    // TODO: Implement confirmation dialog before deletion
                    // For now, require the user to press delete twice (use a flag)
                    // This is a safety measure to prevent accidental deletion

                    // Simple confirmation: check if path contains certain patterns to prevent accidents
                    if path.to_string_lossy().contains("important") || name.starts_with('.') {
                        self.set_status_message(format!(
                            "Refusing to delete: {} (safety check)",
                            name
                        ));
                        return;
                    }

                    if let Some(runtime) = &self.tokio_runtime {
                        let path_clone = path.clone();
                        let result = runtime.block_on(async move {
                            if is_dir {
                                tokio::fs::remove_dir_all(&path_clone).await
                            } else {
                                tokio::fs::remove_file(&path_clone).await
                            }
                        });

                        match result {
                            Ok(_) => {
                                // Refresh the parent directory
                                if let Some(parent_id) = parent_id {
                                    let _ = runtime
                                        .block_on(explorer.tree_mut().refresh_node(parent_id));
                                }
                                self.set_status_message(format!("Deleted: {}", name));
                            }
                            Err(e) => {
                                self.set_status_message(format!(
                                    "Failed to delete {}: {}",
                                    name, e
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Handle renaming a file or directory in the file explorer
    pub fn file_explorer_rename(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            if let Some(selected_id) = explorer.get_selected() {
                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    // Don't allow renaming the root
                    if node.parent.is_none() {
                        self.set_status_message("Cannot rename root directory".to_string());
                        return;
                    }

                    // Extract name before mutable borrow
                    let node_name = node.entry.name.clone();

                    // TODO: Implement input dialog to get new name from user
                    // For now, just show a message that this needs implementation
                    self.set_status_message(format!(
                        "Rename '{}': Input dialog not yet implemented",
                        node_name
                    ));
                }
            }
        }
    }

    /// Toggle showing hidden files in file explorer
    pub fn file_explorer_toggle_hidden(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.toggle_show_hidden();
            let show = explorer.ignore_patterns().show_hidden();
            let msg = if show {
                "Showing hidden files"
            } else {
                "Hiding hidden files"
            };
            self.set_status_message(msg.to_string());
        }
    }

    /// Toggle showing gitignored files in file explorer
    pub fn file_explorer_toggle_gitignored(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.toggle_show_gitignored();
            let show = explorer.ignore_patterns().show_gitignored();
            let msg = if show {
                "Showing gitignored files"
            } else {
                "Hiding gitignored files"
            };
            self.set_status_message(msg.to_string());
        }
    }

    /// Get the currently active buffer state
    pub fn active_state(&self) -> &EditorState {
        self.buffers.get(&self.active_buffer).unwrap()
    }

    /// Get the currently active buffer state (mutable)
    pub fn active_state_mut(&mut self) -> &mut EditorState {
        self.buffers.get_mut(&self.active_buffer).unwrap()
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
        // IMPORTANT: Calculate LSP changes BEFORE applying to buffer!
        // The byte positions in the events are relative to the ORIGINAL buffer,
        // so we must convert them to LSP positions before modifying the buffer.
        let lsp_changes = self.collect_lsp_changes(event);

        // 1. Apply the event to the buffer
        self.active_state_mut().apply(event);

        // 2. Adjust cursors in other splits that share the same buffer
        self.adjust_other_split_cursors_for_event(event);

        // 3. Clear search highlights on edit (Insert/Delete events)
        // This preserves highlights while navigating but clears them when modifying text
        // EXCEPT during interactive replace where we want to keep highlights visible
        let in_interactive_replace = self.interactive_replace_state.is_some();

        if !in_interactive_replace {
            match event {
                Event::Insert { .. } | Event::Delete { .. } => {
                    self.clear_search_highlights();
                }
                Event::Batch { events, .. } => {
                    // Check if batch contains any Insert/Delete events
                    let has_edits = events
                        .iter()
                        .any(|e| matches!(e, Event::Insert { .. } | Event::Delete { .. }));
                    if has_edits {
                        self.clear_search_highlights();
                    }
                }
                _ => {}
            }
        }

        // 3. Trigger plugin hooks for this event
        self.trigger_plugin_hooks_for_event(event);

        // 4. Notify LSP of the change using pre-calculated positions
        self.send_lsp_changes_for_buffer(self.active_buffer, lsp_changes);
    }

    /// Trigger plugin hooks for an event (if any)
    fn trigger_plugin_hooks_for_event(&mut self, event: &Event) {
        let buffer_id = self.active_buffer;

        // Convert event to hook args and fire the appropriate hook
        let hook_args = match event {
            Event::Insert { position, text, .. } => {
                // Clear seen_lines for this buffer so lines get re-processed
                self.seen_lines.remove(&buffer_id);
                Some((
                    "after-insert",
                    crate::hooks::HookArgs::AfterInsert {
                        buffer_id,
                        position: *position,
                        text: text.clone(),
                    },
                ))
            }
            Event::Delete { range, deleted_text, .. } => {
                // Clear seen_lines for this buffer so lines get re-processed
                self.seen_lines.remove(&buffer_id);
                Some((
                    "after-delete",
                    crate::hooks::HookArgs::AfterDelete {
                        buffer_id,
                        range: range.clone(),
                        deleted_text: deleted_text.clone(),
                    },
                ))
            }
            Event::Batch { events, .. } => {
                // Fire hooks for each event in the batch
                for e in events {
                    self.trigger_plugin_hooks_for_event(e);
                }
                None
            }
            _ => None,
        };

        // Fire the hook to TypeScript plugins
        if let Some((hook_name, args)) = hook_args {
            if let Some(ref ts_manager) = self.ts_plugin_manager {
                ts_manager.run_hook(hook_name, args);
            }
        }
    }

    /// Get the event log for the active buffer
    pub fn active_event_log(&self) -> &EventLog {
        self.event_logs.get(&self.active_buffer).unwrap()
    }

    /// Get the event log for the active buffer (mutable)
    pub fn active_event_log_mut(&mut self) -> &mut EventLog {
        self.event_logs.get_mut(&self.active_buffer).unwrap()
    }

    // ========================================================================
    // Buffer-based clipboard operations
    // ========================================================================
    //
    // NOTE: These operations work on Buffer selections with multi-cursor support
    // and integrate with the event system for undo/redo. They are distinct from
    // the simpler prompt clipboard operations (see src/prompt.rs) which work on
    // plain strings without selections or undo history.
    //
    // MOTIVATION FOR SEPARATION:
    // - Buffer operations need: multi-cursor, selections, event sourcing, undo/redo
    // - Prompt operations need: simple string manipulation, no selection tracking
    // - Sharing code would force prompts to use Buffer (expensive) or buffers to
    //   lose features (selections, multi-cursor, undo)
    //
    // Both use the same clipboard storage (self.clipboard) ensuring copy/paste
    // works across buffer editing and prompt input.

    /// Copy the current selection to clipboard
    pub fn copy_selection(&mut self) {
        // Collect ranges first
        let ranges: Vec<_> = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .filter_map(|(_, cursor)| cursor.selection_range())
                .collect()
        };

        let mut text = String::new();
        let state = self.active_state_mut();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            self.clipboard = text;
            self.status_message = Some("Copied".to_string());
        }
    }

    /// Cut the current selection to clipboard
    pub fn cut_selection(&mut self) {
        self.copy_selection();

        // Get deletions from state
        let deletions: Vec<_> = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .filter_map(|(_, c)| c.selection_range())
                .collect()
        };

        // Get deleted text and cursor id
        let state = self.active_state_mut();
        let primary_id = state.cursors.primary_id();
        let events: Vec<_> = deletions
            .iter()
            .rev()
            .map(|range| {
                let deleted_text = state.get_text_range(range.start, range.end);
                Event::Delete {
                    range: range.clone(),
                    deleted_text,
                    cursor_id: primary_id,
                }
            })
            .collect();

        // Apply events
        for event in events {
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
        }

        if !deletions.is_empty() {
            self.status_message = Some("Cut".to_string());
        }
    }

    /// Paste the clipboard content
    pub fn paste(&mut self) {
        if self.clipboard.is_empty() {
            return;
        }

        let state = self.active_state();
        let cursor_id = state.cursors.primary_id();
        let position = state.cursors.primary().position;

        let event = Event::Insert {
            position,
            text: self.clipboard.clone(),
            cursor_id,
        };

        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);

        self.status_message = Some("Pasted".to_string());
    }

    /// Add a cursor at the next occurrence of the selected text
    /// If no selection, does nothing
    pub fn add_cursor_at_next_match(&mut self) {
        let state = self.active_state_mut();
        match add_cursor_at_next_match(state) {
            AddCursorResult::Success {
                cursor,
                total_cursors,
            } => {
                // Create AddCursor event with the next cursor ID
                let next_id = CursorId(self.active_state().cursors.count());
                let event = Event::AddCursor {
                    cursor_id: next_id,
                    position: cursor.position,
                    anchor: cursor.anchor,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);

                self.status_message = Some(format!("Added cursor at match ({})", total_cursors));
            }
            AddCursorResult::Failed { message } => {
                self.status_message = Some(message);
            }
        }
    }

    /// Add a cursor above the primary cursor at the same column
    pub fn add_cursor_above(&mut self) {
        let state = self.active_state_mut();
        match add_cursor_above(state) {
            AddCursorResult::Success {
                cursor,
                total_cursors,
            } => {
                // Create AddCursor event with the next cursor ID
                let next_id = CursorId(self.active_state().cursors.count());
                let event = Event::AddCursor {
                    cursor_id: next_id,
                    position: cursor.position,
                    anchor: cursor.anchor,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);

                self.status_message = Some(format!("Added cursor above ({})", total_cursors));
            }
            AddCursorResult::Failed { message } => {
                self.status_message = Some(message);
            }
        }
    }

    /// Add a cursor below the primary cursor at the same column
    pub fn add_cursor_below(&mut self) {
        let state = self.active_state_mut();
        match add_cursor_below(state) {
            AddCursorResult::Success {
                cursor,
                total_cursors,
            } => {
                // Create AddCursor event with the next cursor ID
                let next_id = CursorId(self.active_state().cursors.count());
                let event = Event::AddCursor {
                    cursor_id: next_id,
                    position: cursor.position,
                    anchor: cursor.anchor,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);

                self.status_message = Some(format!("Added cursor below ({})", total_cursors));
            }
            AddCursorResult::Failed { message } => {
                self.status_message = Some(message);
            }
        }
    }

    /// Save the active buffer
    pub fn save(&mut self) -> io::Result<()> {
        let path = self.active_state().buffer.file_path().map(|p| p.to_path_buf());
        self.active_state_mut().buffer.save()?;
        self.status_message = Some("Saved".to_string());

        // Notify LSP of save
        self.notify_lsp_save();

        // Emit control event
        if let Some(p) = path {
            self.emit_event(
                crate::control_event::events::FILE_SAVED.name,
                serde_json::json!({
                    "path": p.display().to_string()
                }),
            );
        }

        Ok(())
    }

    /// Check if the editor should quit
    pub fn should_quit(&self) -> bool {
        self.should_quit
    }

    /// Get the active theme
    pub fn theme(&self) -> &crate::theme::Theme {
        &self.theme
    }

    /// Request the editor to quit
    pub fn quit(&mut self) {
        // TODO: Check for unsaved buffers
        self.should_quit = true;
    }

    /// Resize all buffers to match new terminal size
    pub fn resize(&mut self, width: u16, height: u16) {
        // Update terminal dimensions for future buffer creation
        self.terminal_width = width;
        self.terminal_height = height;

        // Resize all existing buffers
        for state in self.buffers.values_mut() {
            state.resize(width, height);
        }
    }

    // Prompt/Minibuffer control methods

    /// Start a new prompt (enter minibuffer mode)
    pub fn start_prompt(&mut self, message: String, prompt_type: PromptType) {
        self.start_prompt_with_suggestions(message, prompt_type, Vec::new());
    }

    /// Start a new prompt with autocomplete suggestions
    pub fn start_prompt_with_suggestions(
        &mut self,
        message: String,
        prompt_type: PromptType,
        suggestions: Vec<Suggestion>,
    ) {
        // Clear search highlights when starting a new search prompt
        // This ensures old highlights from previous searches don't persist
        match prompt_type {
            PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                self.clear_search_highlights();
            }
            _ => {}
        }

        self.prompt = Some(Prompt::with_suggestions(message, prompt_type, suggestions));
    }

    /// Cancel the current prompt and return to normal mode
    pub fn cancel_prompt(&mut self) {
        // Determine prompt type and reset appropriate history navigation
        if let Some(ref prompt) = self.prompt {
            match &prompt.prompt_type {
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                    self.search_history.reset_navigation();
                    self.clear_search_highlights();
                }
                PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                    self.replace_history.reset_navigation();
                }
                PromptType::Plugin { custom_type } => {
                    // Fire plugin hook for prompt cancellation
                    use crate::hooks::HookArgs;
                    let hook_args = HookArgs::PromptCancelled {
                        prompt_type: custom_type.clone(),
                        input: prompt.input.clone(),
                    };

                    if let Some(ref ts_manager) = self.ts_plugin_manager {
                        ts_manager.run_hook("prompt_cancelled", hook_args);
                    }
                }
                PromptType::LspRename { overlay_id, .. } => {
                    // Remove the rename overlay when cancelling
                    let remove_overlay_event = crate::event::Event::RemoveOverlay {
                        overlay_id: overlay_id.clone(),
                    };
                    self.apply_event_to_active_buffer(&remove_overlay_event);
                }
                _ => {}
            }
        }

        self.prompt = None;
        self.status_message = Some("Canceled".to_string());
    }

    /// Get the confirmed input and prompt type, consuming the prompt
    /// For command palette, returns the selected suggestion if available, otherwise the raw input
    /// Returns (input, prompt_type, selected_index)
    /// Returns None if trying to confirm a disabled command
    pub fn confirm_prompt(&mut self) -> Option<(String, PromptType, Option<usize>)> {
        if let Some(prompt) = self.prompt.take() {
            let selected_index = prompt.selected_suggestion;
            // For command prompts, prefer the selected suggestion over raw input
            let final_input = if matches!(prompt.prompt_type, PromptType::Command) {
                // For Command, use the selected suggestion if any
                if let Some(selected_idx) = prompt.selected_suggestion {
                    if let Some(suggestion) = prompt.suggestions.get(selected_idx) {
                        // Don't confirm disabled commands
                        if suggestion.disabled {
                            self.set_status_message(format!(
                                "Command '{}' is not available in current context",
                                suggestion.text
                            ));
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

            // Add to appropriate history based on prompt type
            match prompt.prompt_type {
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                    self.search_history.push(final_input.clone());
                    // Reset navigation state
                    self.search_history.reset_navigation();
                }
                PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                    self.replace_history.push(final_input.clone());
                    // Reset navigation state
                    self.replace_history.reset_navigation();
                }
                _ => {}
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

    /// Get current prompt input (for display)
    pub fn prompt_input(&self) -> Option<&str> {
        self.prompt.as_ref().map(|p| p.input.as_str())
    }

    /// Get mutable reference to prompt (for input handling)
    pub fn prompt_mut(&mut self) -> Option<&mut Prompt> {
        self.prompt.as_mut()
    }

    /// Set a status message to display in the status bar
    pub fn set_status_message(&mut self, message: String) {
        self.status_message = Some(message);
    }

    /// Get the current status message
    pub fn get_status_message(&self) -> Option<&String> {
        self.status_message.as_ref()
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
                if let Some(prompt) = &mut self.prompt {
                    // Use the underlying context (not Prompt context) for filtering
                    prompt.suggestions = self.command_registry.read().unwrap().filter(
                        &input,
                        self.key_context,
                        &self.keybindings,
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
            }
            PromptType::Plugin { custom_type } => {
                // Fire plugin hook for prompt input change
                use crate::hooks::HookArgs;
                let hook_args = HookArgs::PromptChanged {
                    prompt_type: custom_type,
                    input,
                };

                if let Some(ref ts_manager) = self.ts_plugin_manager {
                    ts_manager.run_hook("prompt_changed", hook_args);
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
    /// - Git status updates (future)
    pub fn process_async_messages(&mut self) -> bool {
        let Some(bridge) = &self.async_bridge else {
            return false;
        };

        let messages = bridge.try_recv_all();
        let needs_render = !messages.is_empty();

        for message in messages {
            match message {
                AsyncMessage::LspDiagnostics { uri, diagnostics } => {
                    tracing::debug!(
                        "Processing {} LSP diagnostics for {}",
                        diagnostics.len(),
                        uri
                    );

                    // Find the buffer for this URI by comparing URIs directly
                    if let Ok(diagnostic_url) = uri.parse::<lsp_types::Uri>() {
                        // Find buffer ID by matching URI
                        if let Some((buffer_id, _)) = self
                            .buffer_metadata
                            .iter()
                            .find(|(_, m)| m.file_uri() == Some(&diagnostic_url))
                        {
                            // Convert diagnostics to overlays
                            if let Some(state) = self.buffers.get_mut(buffer_id) {
                                lsp_diagnostics::apply_diagnostics_to_state_cached(
                                    state,
                                    &diagnostics,
                                    &self.theme,
                                );
                                tracing::info!(
                                    "Applied {} diagnostics to buffer {:?}",
                                    diagnostics.len(),
                                    buffer_id
                                );
                            }
                        } else {
                            tracing::debug!("No buffer found for diagnostic URI: {}", uri);
                        }
                    } else {
                        tracing::warn!("Could not parse diagnostic URI: {}", uri);
                    }
                }
                AsyncMessage::LspInitialized { language } => {
                    tracing::info!("LSP server initialized for language: {}", language);
                    self.status_message = Some(format!("LSP ({}) ready", language));
                }
                AsyncMessage::LspError { language, error } => {
                    tracing::error!("LSP error for {}: {}", language, error);
                    self.status_message = Some(format!("LSP error ({}): {}", language, error));
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
                    // Handle pulled diagnostics (LSP 3.17+ pull model)
                    if unchanged {
                        tracing::debug!("Diagnostics unchanged for {} (result_id: {:?})", uri, result_id);
                        // No need to update - diagnostics haven't changed
                    } else {
                        tracing::debug!(
                            "Processing {} pulled diagnostics for {} (result_id: {:?})",
                            diagnostics.len(),
                            uri,
                            result_id
                        );

                        // Find the buffer for this URI
                        if let Ok(diagnostic_url) = uri.parse::<lsp_types::Uri>() {
                            if let Some((buffer_id, _)) = self
                                .buffer_metadata
                                .iter()
                                .find(|(_, m)| m.file_uri() == Some(&diagnostic_url))
                            {
                                // Apply diagnostics to the buffer
                                if let Some(state) = self.buffers.get_mut(buffer_id) {
                                    lsp_diagnostics::apply_diagnostics_to_state_cached(
                                        state,
                                        &diagnostics,
                                        &self.theme,
                                    );
                                    tracing::info!(
                                        "Applied {} pulled diagnostics to buffer {:?}",
                                        diagnostics.len(),
                                        buffer_id
                                    );
                                }
                            } else {
                                tracing::debug!("No buffer found for pulled diagnostic URI: {}", uri);
                            }
                        } else {
                            tracing::warn!("Could not parse pulled diagnostic URI: {}", uri);
                        }
                    }

                    // Store result_id for incremental updates
                    if let Some(result_id) = result_id {
                        self.diagnostic_result_ids.insert(uri, result_id);
                    }
                }
                AsyncMessage::LspInlayHints {
                    request_id,
                    uri,
                    hints,
                } => {
                    // Handle inlay hints response
                    if self.pending_inlay_hints_request == Some(request_id) {
                        self.pending_inlay_hints_request = None;

                        tracing::info!(
                            "Received {} inlay hints for {} (request_id={})",
                            hints.len(),
                            uri,
                            request_id
                        );

                        // Find the buffer for this URI and apply hints
                        if let Ok(hint_url) = uri.parse::<lsp_types::Uri>() {
                            if let Some((buffer_id, _)) = self
                                .buffer_metadata
                                .iter()
                                .find(|(_, m)| m.file_uri() == Some(&hint_url))
                            {
                                if let Some(state) = self.buffers.get_mut(buffer_id) {
                                    Self::apply_inlay_hints_to_state(state, &hints);
                                    tracing::info!(
                                        "Applied {} inlay hints as virtual text to buffer {:?}",
                                        hints.len(),
                                        buffer_id
                                    );
                                }
                            } else {
                                tracing::warn!("No buffer found for inlay hints URI: {}", uri);
                            }
                        } else {
                            tracing::warn!("Could not parse inlay hints URI: {}", uri);
                        }
                    } else {
                        tracing::debug!(
                            "Ignoring stale inlay hints response (request_id={})",
                            request_id
                        );
                    }
                }
                AsyncMessage::LspServerQuiescent { language } => {
                    // rust-analyzer project is fully loaded - re-request inlay hints
                    tracing::info!(
                        "LSP ({}) project fully loaded, re-requesting inlay hints",
                        language
                    );

                    // Re-request inlay hints for all open buffers with this language
                    if let Some(lsp) = self.lsp.as_mut() {
                        if let Some(client) = lsp.get_or_spawn(&language) {
                            // Collect buffer info first to avoid borrow issues
                            let buffer_infos: Vec<_> = self
                                .buffer_metadata
                                .iter()
                                .filter_map(|(buffer_id, metadata)| {
                                    if let Some(uri) = metadata.file_uri() {
                                        let line_count = self
                                            .buffers
                                            .get(buffer_id)
                                            .and_then(|s| s.buffer.line_count())
                                            .unwrap_or(1000);
                                        Some((uri.clone(), line_count))
                                    } else {
                                        None
                                    }
                                })
                                .collect();

                            for (uri, line_count) in buffer_infos {
                                let request_id = self.next_lsp_request_id;
                                self.next_lsp_request_id += 1;
                                self.pending_inlay_hints_request = Some(request_id);

                                let last_line = line_count.saturating_sub(1) as u32;
                                if let Err(e) = client.inlay_hints(
                                    request_id,
                                    uri.clone(),
                                    0, 0,
                                    last_line, 10000,
                                ) {
                                    tracing::debug!(
                                        "Failed to re-request inlay hints for {}: {}",
                                        uri.as_str(),
                                        e
                                    );
                                } else {
                                    tracing::info!(
                                        "Re-requested inlay hints for {} (request_id={})",
                                        uri.as_str(),
                                        request_id
                                    );
                                }
                            }
                        }
                    }
                }
                AsyncMessage::FileChanged { path } => {
                    tracing::info!("File changed externally: {}", path);
                    // TODO: Handle external file changes
                }
                AsyncMessage::GitStatusChanged { status } => {
                    tracing::info!("Git status changed: {}", status);
                    // TODO: Handle git status changes
                }
                AsyncMessage::FileExplorerInitialized(mut view) => {
                    tracing::info!("File explorer initialized");

                    // Load root .gitignore
                    let root_id = view.tree().root_id();
                    let root_path = view.tree().get_node(root_id).map(|n| n.entry.path.clone());

                    if let Some(root_path) = root_path {
                        if let Err(e) = view.load_gitignore_for_dir(&root_path) {
                            tracing::warn!(
                                "Failed to load root .gitignore from {:?}: {}",
                                root_path,
                                e
                            );
                        } else {
                            tracing::debug!("Loaded root .gitignore from {:?}", root_path);
                        }
                    }

                    self.file_explorer = Some(view);
                    self.set_status_message("File explorer ready".to_string());
                }
                AsyncMessage::FileExplorerToggleNode(node_id) => {
                    // Async toggle completed - this message signals the operation is done
                    tracing::debug!("File explorer toggle completed for node {:?}", node_id);
                }
                AsyncMessage::FileExplorerRefreshNode(node_id) => {
                    // Async refresh completed
                    tracing::debug!("File explorer refresh completed for node {:?}", node_id);
                    self.set_status_message("Refreshed".to_string());
                }
                AsyncMessage::FileExplorerExpandedToPath(mut view) => {
                    // File explorer has expanded to the active file path
                    tracing::debug!("File explorer expanded to active file path");

                    // Update scroll to ensure the selected file is visible
                    view.update_scroll_for_selection();

                    self.file_explorer = Some(view);
                }
                AsyncMessage::PluginProcessOutput {
                    process_id,
                    stdout,
                    stderr,
                    exit_code,
                } => {
                    // Plugin process completed - TypeScript uses native async/await, no callback needed
                    tracing::debug!(
                        "Process {} completed: exit_code={}, stdout_len={}, stderr_len={}",
                        process_id,
                        exit_code,
                        stdout.len(),
                        stderr.len()
                    );
                }
                AsyncMessage::LspProgress {
                    language,
                    token,
                    value,
                } => {
                    use crate::async_bridge::LspProgressValue;
                    match value {
                        LspProgressValue::Begin {
                            title,
                            message,
                            percentage,
                        } => {
                            // Store progress info
                            self.lsp_progress.insert(
                                token.clone(),
                                LspProgressInfo {
                                    language: language.clone(),
                                    title,
                                    message,
                                    percentage,
                                },
                            );
                            // Update LSP status to show progress
                            self.update_lsp_status_from_progress();
                        }
                        LspProgressValue::Report {
                            message,
                            percentage,
                        } => {
                            // Update existing progress
                            if let Some(info) = self.lsp_progress.get_mut(&token) {
                                info.message = message;
                                info.percentage = percentage;
                                self.update_lsp_status_from_progress();
                            }
                        }
                        LspProgressValue::End { .. } => {
                            // Remove progress
                            self.lsp_progress.remove(&token);
                            self.update_lsp_status_from_progress();
                        }
                    }
                }
                AsyncMessage::LspWindowMessage {
                    language,
                    message_type,
                    message,
                } => {
                    // Add to window messages list
                    self.lsp_window_messages.push(LspMessageEntry {
                        language: language.clone(),
                        message_type,
                        message: message.clone(),
                        timestamp: std::time::Instant::now(),
                    });

                    // Keep only last 100 messages
                    if self.lsp_window_messages.len() > 100 {
                        self.lsp_window_messages.remove(0);
                    }

                    // Show important messages in status bar
                    use crate::async_bridge::LspMessageType;
                    match message_type {
                        LspMessageType::Error => {
                            self.status_message = Some(format!("LSP ({}): {}", language, message));
                        }
                        LspMessageType::Warning => {
                            self.status_message = Some(format!("LSP ({}): {}", language, message));
                        }
                        _ => {
                            // Info and Log messages are not shown in status bar
                        }
                    }
                }
                AsyncMessage::LspLogMessage {
                    language,
                    message_type,
                    message,
                } => {
                    // Add to log messages list
                    self.lsp_log_messages.push(LspMessageEntry {
                        language,
                        message_type,
                        message,
                        timestamp: std::time::Instant::now(),
                    });

                    // Keep only last 500 log messages
                    if self.lsp_log_messages.len() > 500 {
                        self.lsp_log_messages.remove(0);
                    }
                }
                AsyncMessage::LspStatusUpdate { language, status } => {
                    // Get old status for event
                    let old_status = self.lsp_server_statuses.get(&language).cloned();
                    // Update server status
                    self.lsp_server_statuses.insert(language.clone(), status.clone());
                    self.update_lsp_status_from_server_statuses();

                    // Handle server crash - trigger auto-restart
                    if status == crate::async_bridge::LspServerStatus::Error {
                        // Only trigger restart if transitioning to error from a running state
                        let was_running = old_status.map(|s| {
                            matches!(s,
                                crate::async_bridge::LspServerStatus::Running |
                                crate::async_bridge::LspServerStatus::Initializing
                            )
                        }).unwrap_or(false);

                        if was_running {
                            if let Some(lsp) = self.lsp.as_mut() {
                                let message = lsp.handle_server_crash(&language);
                                self.status_message = Some(message);
                            }
                        }
                    }

                    // Emit control event
                    let status_str = match status {
                        crate::async_bridge::LspServerStatus::Starting => "starting",
                        crate::async_bridge::LspServerStatus::Initializing => "initializing",
                        crate::async_bridge::LspServerStatus::Running => "running",
                        crate::async_bridge::LspServerStatus::Error => "error",
                        crate::async_bridge::LspServerStatus::Shutdown => "shutdown",
                    };
                    let old_status_str = old_status.map(|s| match s {
                        crate::async_bridge::LspServerStatus::Starting => "starting",
                        crate::async_bridge::LspServerStatus::Initializing => "initializing",
                        crate::async_bridge::LspServerStatus::Running => "running",
                        crate::async_bridge::LspServerStatus::Error => "error",
                        crate::async_bridge::LspServerStatus::Shutdown => "shutdown",
                    }).unwrap_or("none");
                    self.emit_event(
                        crate::control_event::events::LSP_STATUS_CHANGED.name,
                        serde_json::json!({
                            "language": language,
                            "old_status": old_status_str,
                            "status": status_str
                        }),
                    );
                }
            }
        }

        // Update plugin state snapshot BEFORE processing commands
        // This ensures plugins have access to current editor state (cursor positions, etc.)
        self.update_plugin_state_snapshot();

        // Process TypeScript plugin commands
        let mut processed_any_commands = false;
        if let Some(ref mut manager) = self.ts_plugin_manager {
            let commands = manager.process_commands();
            tracing::trace!("process_async_messages: got {} commands from plugin manager", commands.len());
            if !commands.is_empty() {
                tracing::trace!("process_plugin_commands: processing {} commands", commands.len());
                processed_any_commands = true;
                for command in commands {
                    tracing::trace!("process_plugin_commands: handling command {:?}", std::mem::discriminant(&command));
                    if let Err(e) = self.handle_plugin_command(command) {
                        tracing::error!("Error handling TypeScript plugin command: {}", e);
                    }
                }
            }
        } else {
            tracing::trace!("process_async_messages: no plugin manager");
        }

        // Process pending plugin action completions
        // Retain only actions that haven't completed yet
        self.pending_plugin_actions.retain(|(action_name, receiver)| {
            match receiver.try_recv() {
                Ok(result) => {
                    match result {
                        Ok(()) => {
                            tracing::info!("Plugin action '{}' executed successfully", action_name);
                        }
                        Err(e) => {
                            tracing::error!("Plugin action '{}' error: {}", action_name, e);
                        }
                    }
                    false // Remove completed action
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {
                    true // Keep pending action
                }
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                    tracing::error!("Plugin thread disconnected during action '{}'", action_name);
                    false // Remove disconnected action
                }
            }
        });

        // Process pending LSP server restarts (with exponential backoff)
        if let Some(lsp) = self.lsp.as_mut() {
            let restart_results = lsp.process_pending_restarts();
            for (language, success, message) in restart_results {
                self.status_message = Some(message.clone());

                // If restart was successful, we need to re-notify about open documents
                if success {
                    // Find all open buffers for this language and re-send didOpen
                    let buffers_for_language: Vec<_> = self
                        .buffer_metadata
                        .iter()
                        .filter_map(|(buf_id, meta)| {
                            if let Some(path) = meta.file_path() {
                                if crate::lsp_manager::detect_language(path) == Some(language.clone()) {
                                    Some((*buf_id, path.clone()))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .collect();

                    // Re-send didOpen for each buffer
                    for (buffer_id, path) in buffers_for_language {
                        if let Some(state) = self.buffers.get(&buffer_id) {
                            let content = state.buffer.to_string();
                            let uri: Option<lsp_types::Uri> = url::Url::from_file_path(&path)
                                .ok()
                                .and_then(|u| u.as_str().parse::<lsp_types::Uri>().ok());
                            if let Some(uri) = uri {
                                if let Some(lang_id) = crate::lsp_manager::detect_language(&path) {
                                    if let Some(lsp) = self.lsp.as_mut() {
                                        if let Some(handle) = lsp.get_or_spawn(&lang_id) {
                                            let _ = handle.did_open(uri, content, lang_id);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Check and clear the plugin render request flag
        let plugin_render = self.plugin_render_requested;
        self.plugin_render_requested = false;

        // Trigger render if any async messages, plugin commands were processed, or plugin requested render
        needs_render || processed_any_commands || plugin_render
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
        use crate::async_bridge::LspServerStatus;

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
    fn update_plugin_state_snapshot(&mut self) {
        // Update TypeScript plugin manager state
        if let Some(ref manager) = self.ts_plugin_manager {
            use crate::plugin_api::{BufferInfo, CursorInfo, ViewportInfo};

            let snapshot_handle = manager.state_snapshot_handle();
            let mut snapshot = snapshot_handle.write().unwrap();

            // Update active buffer ID
            snapshot.active_buffer_id = self.active_buffer;

            // Update active split ID
            snapshot.active_split_id = self.split_manager.active_split().0;

            // Clear and update buffer info
            snapshot.buffers.clear();
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

                // Store cursor position for this buffer
                let cursor_pos = state.cursors.primary().position;
                snapshot.buffer_cursor_positions.insert(*buffer_id, cursor_pos);

                // Store text properties if this buffer has any
                if !state.text_properties.is_empty() {
                    snapshot.buffer_text_properties.insert(
                        *buffer_id,
                        state.text_properties.all().to_vec(),
                    );
                }
            }

            // Update cursor information for active buffer
            if let Some(active_state) = self.buffers.get(&self.active_buffer) {
                // Primary cursor
                let primary = active_state.cursors.primary();
                snapshot.primary_cursor = Some(CursorInfo {
                    position: primary.position,
                    selection: primary.selection_range(),
                });

                // All cursors
                snapshot.all_cursors = active_state
                    .cursors
                    .iter()
                    .map(|(_, cursor)| CursorInfo {
                        position: cursor.position,
                        selection: cursor.selection_range(),
                    })
                    .collect();

                // Viewport
                snapshot.viewport = Some(ViewportInfo {
                    top_byte: active_state.viewport.top_byte,
                    left_column: active_state.viewport.left_column,
                    width: active_state.viewport.width,
                    height: active_state.viewport.height,
                });
            } else {
                snapshot.primary_cursor = None;
                snapshot.all_cursors.clear();
                snapshot.viewport = None;
            }
        }
    }

    /// Handle a plugin command
    fn handle_plugin_command(&mut self, command: PluginCommand) -> io::Result<()> {
        match command {
            PluginCommand::InsertText {
                buffer_id,
                position,
                text,
            } => {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let event = Event::Insert {
                        position,
                        text,
                        cursor_id: CursorId(0),
                    };
                    state.apply(&event);
                    if let Some(log) = self.event_logs.get_mut(&buffer_id) {
                        log.append(event);
                    }
                }
            }
            PluginCommand::DeleteRange { buffer_id, range } => {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let deleted_text = state.get_text_range(range.start, range.end);
                    let event = Event::Delete {
                        range,
                        deleted_text,
                        cursor_id: CursorId(0),
                    };
                    state.apply(&event);
                    if let Some(log) = self.event_logs.get_mut(&buffer_id) {
                        log.append(event);
                    }
                }
            }
            PluginCommand::AddOverlay {
                buffer_id,
                overlay_id,
                range,
                color,
                underline,
            } => {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let face = if underline {
                        crate::event::OverlayFace::Underline {
                            color,
                            style: crate::event::UnderlineStyle::Wavy,
                        }
                    } else {
                        crate::event::OverlayFace::Background { color }
                    };
                    let event = Event::AddOverlay {
                        overlay_id,
                        range,
                        face,
                        priority: 10,
                        message: None,
                    };
                    state.apply(&event);
                    if let Some(log) = self.event_logs.get_mut(&buffer_id) {
                        log.append(event);
                    }
                }
            }
            PluginCommand::RemoveOverlay {
                buffer_id,
                overlay_id,
            } => {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let event = Event::RemoveOverlay { overlay_id };
                    state.apply(&event);
                    if let Some(log) = self.event_logs.get_mut(&buffer_id) {
                        log.append(event);
                    }
                }
            }
            PluginCommand::SetStatus { message } => {
                self.status_message = Some(message);
            }
            PluginCommand::RegisterCommand { command } => {
                self.command_registry.read().unwrap().register(command);
            }
            PluginCommand::UnregisterCommand { name } => {
                self.command_registry.read().unwrap().unregister(&name);
            }
            PluginCommand::OpenFileInBackground { path } => {
                // Open file in a new tab without switching to it
                let current_buffer = self.active_buffer;
                if let Err(e) = self.open_file(&path) {
                    tracing::error!("Failed to open file in background: {}", e);
                } else {
                    // Switch back to the original buffer
                    self.set_active_buffer(current_buffer);
                    tracing::info!("Opened debug log in background: {:?}", path);
                }
            }
            PluginCommand::InsertAtCursor { text } => {
                // Insert text at current cursor position in active buffer
                let state = self.active_state_mut();
                let cursor_pos = state.cursors.primary().position;
                let event = Event::Insert {
                    position: cursor_pos,
                    text,
                    cursor_id: CursorId(0),
                };
                state.apply(&event);
                self.active_event_log_mut().append(event);
            }
            PluginCommand::SpawnProcess {
                command,
                args,
                cwd,
                callback_id: _,
            } => {
                // TypeScript plugins use native async spawn_process op, not callbacks
                // This path is deprecated and should not be used
                tracing::warn!(
                    "SpawnProcess command with callback is deprecated. TypeScript plugins use native async. Command: {}",
                    command
                );
                let _ = args;
                let _ = cwd;
            }
            PluginCommand::ClearAllOverlays { buffer_id } => {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    // Use the OverlayManager's clear method
                    state.overlays.clear(&mut state.marker_list);

                    // Note: We don't add this to the event log because:
                    // 1. Clearing overlays doesn't affect undo/redo (overlays are ephemeral)
                    // 2. This is a plugin-initiated action, not a user edit
                }
            }
            PluginCommand::RemoveOverlaysByPrefix { buffer_id, prefix } => {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    // Use the OverlayManager's remove_by_prefix method
                    state
                        .overlays
                        .remove_by_prefix(&prefix, &mut state.marker_list);

                    // Note: We don't add this to the event log because:
                    // 1. Clearing overlays doesn't affect undo/redo (overlays are ephemeral)
                    // 2. This is a plugin-initiated action, not a user edit
                }
            }
            PluginCommand::RefreshLines { buffer_id } => {
                // Clear seen_lines for this buffer so all visible lines will be re-processed
                // on the next render. This is useful when a plugin is enabled and needs to
                // process lines that were already marked as seen.
                self.seen_lines.remove(&buffer_id);
                // Request a render so the lines_changed hook fires
                self.plugin_render_requested = true;
            }
            PluginCommand::OpenFileAtLocation { path, line, column } => {
                // Open the file
                if let Err(e) = self.open_file(&path) {
                    tracing::error!("Failed to open file from plugin: {}", e);
                    return Ok(());
                }

                // If line/column specified, jump to that location
                if line.is_some() || column.is_some() {
                    let state = self.active_state_mut();

                    // Convert 1-indexed line/column to byte position
                    let target_line = line.unwrap_or(1).saturating_sub(1); // Convert to 0-indexed
                    let column_offset = column.unwrap_or(1).saturating_sub(1); // Convert to 0-indexed

                    let mut iter = state.buffer.line_iterator(0, 80);
                    let mut target_byte = 0;

                    // Iterate through lines until we reach the target
                    for current_line in 0..=target_line {
                        if let Some((line_start, _)) = iter.next() {
                            if current_line == target_line {
                                target_byte = line_start;
                                break;
                            }
                        } else {
                            // Reached end of buffer before target line
                            break;
                        }
                    }

                    // Add the column offset to position within the line
                    // Column offset is byte offset from line start (matching git grep --column behavior)
                    let final_position = target_byte + column_offset;

                    // Ensure we don't go past the buffer end
                    let buffer_len = state.buffer.len();
                    state.cursors.primary_mut().position = final_position.min(buffer_len);
                    state.cursors.primary_mut().anchor = None;

                    // Ensure the position is visible
                    state
                        .viewport
                        .ensure_visible(&mut state.buffer, state.cursors.primary());
                }
            }
            PluginCommand::OpenFileInSplit {
                split_id,
                path,
                line,
                column,
            } => {
                // Save current split's view state before switching
                self.save_current_split_view_state();

                // Switch to the target split
                let target_split_id = SplitId(split_id);
                if !self.split_manager.set_active_split(target_split_id) {
                    tracing::error!("Failed to switch to split {}", split_id);
                    return Ok(());
                }
                self.restore_current_split_view_state();

                // Open the file in the now-active split
                if let Err(e) = self.open_file(&path) {
                    tracing::error!("Failed to open file from plugin: {}", e);
                    return Ok(());
                }

                // Jump to the specified location (or default to start)
                {
                    let state = self.active_state_mut();

                    // Convert 1-indexed line/column to byte position
                    let target_line = line.unwrap_or(1).saturating_sub(1); // Convert to 0-indexed
                    let column_offset = column.unwrap_or(1).saturating_sub(1); // Convert to 0-indexed

                    let mut iter = state.buffer.line_iterator(0, 80);
                    let mut target_byte = 0;

                    // Iterate through lines until we reach the target
                    for current_line in 0..=target_line {
                        if let Some((line_start, _)) = iter.next() {
                            if current_line == target_line {
                                target_byte = line_start;
                                break;
                            }
                        } else {
                            // Reached end of buffer before target line
                            break;
                        }
                    }

                    // Add the column offset to position within the line
                    let final_position = target_byte + column_offset;

                    // Ensure we don't go past the buffer end
                    let buffer_len = state.buffer.len();
                    state.cursors.primary_mut().position = final_position.min(buffer_len);
                    state.cursors.primary_mut().anchor = None;

                    // Ensure the position is visible in the viewport
                    state
                        .viewport
                        .ensure_visible(&mut state.buffer, state.cursors.primary());
                }
            }
            PluginCommand::StartPrompt { label, prompt_type } => {
                // Create a plugin-controlled prompt
                use crate::prompt::{Prompt, PromptType};
                self.prompt = Some(Prompt::new(
                    label,
                    PromptType::Plugin {
                        custom_type: prompt_type.clone(),
                    },
                ));

                // Fire the prompt_changed hook immediately with empty input
                // This allows plugins to initialize the prompt state
                use crate::hooks::HookArgs;
                let hook_args = HookArgs::PromptChanged {
                    prompt_type: prompt_type.clone(),
                    input: String::new(),
                };

                if let Some(ref ts_manager) = self.ts_plugin_manager {
                    ts_manager.run_hook("prompt_changed", hook_args);
                }
            }
            PluginCommand::SetPromptSuggestions { suggestions } => {
                // Update the current prompt's suggestions
                if let Some(prompt) = &mut self.prompt {
                    prompt.suggestions = suggestions;
                    prompt.selected_suggestion = if prompt.suggestions.is_empty() {
                        None
                    } else {
                        Some(0) // Select first suggestion by default
                    };
                }
            }
            PluginCommand::AddMenuItem {
                menu_label,
                item,
                position,
            } => {
                use crate::plugin_api::MenuPosition;

                // Find the target menu (first in config menus, then plugin menus)
                let target_menu = self
                    .config
                    .menu
                    .menus
                    .iter_mut()
                    .find(|m| m.label == menu_label)
                    .or_else(|| {
                        self.menu_state
                            .plugin_menus
                            .iter_mut()
                            .find(|m| m.label == menu_label)
                    });

                if let Some(menu) = target_menu {
                    // Insert at the specified position
                    let insert_idx = match position {
                        MenuPosition::Top => 0,
                        MenuPosition::Bottom => menu.items.len(),
                        MenuPosition::Before(label) => menu
                            .items
                            .iter()
                            .position(|i| match i {
                                crate::config::MenuItem::Action { label: l, .. }
                                | crate::config::MenuItem::Submenu { label: l, .. } => l == &label,
                                _ => false,
                            })
                            .unwrap_or(menu.items.len()),
                        MenuPosition::After(label) => menu
                            .items
                            .iter()
                            .position(|i| match i {
                                crate::config::MenuItem::Action { label: l, .. }
                                | crate::config::MenuItem::Submenu { label: l, .. } => l == &label,
                                _ => false,
                            })
                            .map(|i| i + 1)
                            .unwrap_or(menu.items.len()),
                    };

                    menu.items.insert(insert_idx, item);
                    tracing::info!(
                        "Added menu item to '{}' at position {}",
                        menu_label,
                        insert_idx
                    );
                } else {
                    tracing::warn!("Menu '{}' not found for adding item", menu_label);
                }
            }
            PluginCommand::AddMenu { menu, position } => {
                use crate::plugin_api::MenuPosition;

                // Calculate insert index based on position
                let total_menus =
                    self.config.menu.menus.len() + self.menu_state.plugin_menus.len();

                let insert_idx = match position {
                    MenuPosition::Top => 0,
                    MenuPosition::Bottom => total_menus,
                    MenuPosition::Before(label) => {
                        // Find in config menus first
                        self.config
                            .menu
                            .menus
                            .iter()
                            .position(|m| m.label == label)
                            .or_else(|| {
                                // Then in plugin menus (offset by config menus count)
                                self.menu_state
                                    .plugin_menus
                                    .iter()
                                    .position(|m| m.label == label)
                                    .map(|i| self.config.menu.menus.len() + i)
                            })
                            .unwrap_or(total_menus)
                    }
                    MenuPosition::After(label) => {
                        // Find in config menus first
                        self.config
                            .menu
                            .menus
                            .iter()
                            .position(|m| m.label == label)
                            .map(|i| i + 1)
                            .or_else(|| {
                                // Then in plugin menus (offset by config menus count)
                                self.menu_state
                                    .plugin_menus
                                    .iter()
                                    .position(|m| m.label == label)
                                    .map(|i| self.config.menu.menus.len() + i + 1)
                            })
                            .unwrap_or(total_menus)
                    }
                };

                // If inserting before config menus end, we can't actually insert into config menus
                // So we always add to plugin_menus, but position it logically
                // For now, just append to plugin_menus (they appear after config menus)
                let plugin_idx = if insert_idx >= self.config.menu.menus.len() {
                    insert_idx - self.config.menu.menus.len()
                } else {
                    // Can't insert before config menus, so put at start of plugin menus
                    0
                };

                self.menu_state
                    .plugin_menus
                    .insert(plugin_idx.min(self.menu_state.plugin_menus.len()), menu);
                tracing::info!(
                    "Added plugin menu at index {} (total menus: {})",
                    plugin_idx,
                    self.config.menu.menus.len() + self.menu_state.plugin_menus.len()
                );
            }
            PluginCommand::RemoveMenuItem {
                menu_label,
                item_label,
            } => {
                // Find the target menu (first in config menus, then plugin menus)
                let target_menu = self
                    .config
                    .menu
                    .menus
                    .iter_mut()
                    .find(|m| m.label == menu_label)
                    .or_else(|| {
                        self.menu_state
                            .plugin_menus
                            .iter_mut()
                            .find(|m| m.label == menu_label)
                    });

                if let Some(menu) = target_menu {
                    // Remove item with matching label
                    let original_len = menu.items.len();
                    menu.items.retain(|item| match item {
                        crate::config::MenuItem::Action { label, .. }
                        | crate::config::MenuItem::Submenu { label, .. } => label != &item_label,
                        _ => true, // Keep separators
                    });

                    if menu.items.len() < original_len {
                        tracing::info!("Removed menu item '{}' from '{}'", item_label, menu_label);
                    } else {
                        tracing::warn!(
                            "Menu item '{}' not found in '{}'",
                            item_label,
                            menu_label
                        );
                    }
                } else {
                    tracing::warn!("Menu '{}' not found for removing item", menu_label);
                }
            }
            PluginCommand::RemoveMenu { menu_label } => {
                // Can only remove plugin menus, not config menus
                let original_len = self.menu_state.plugin_menus.len();
                self.menu_state
                    .plugin_menus
                    .retain(|m| m.label != menu_label);

                if self.menu_state.plugin_menus.len() < original_len {
                    tracing::info!("Removed plugin menu '{}'", menu_label);
                } else {
                    tracing::warn!(
                        "Plugin menu '{}' not found (note: cannot remove config menus)",
                        menu_label
                    );
                }
            }
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
            } => {
                let buffer_id = self.create_virtual_buffer(name.clone(), mode.clone(), read_only);
                tracing::info!(
                    "Created virtual buffer '{}' with mode '{}' (id={:?})",
                    name,
                    mode,
                    buffer_id
                );
                // Now set the content
                match self.set_virtual_buffer_content(buffer_id, entries) {
                    Ok(()) => {
                        tracing::debug!("Set virtual buffer content for {:?}", buffer_id);
                        // Switch to the new buffer to display it
                        self.set_active_buffer(buffer_id);
                        tracing::debug!("Switched to virtual buffer {:?}", buffer_id);
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
                panel_id,
                show_line_numbers,
                show_cursors,
                editing_disabled,
                request_id,
            } => {
                // Check if this panel already exists (for idempotent operations)
                if let Some(pid) = &panel_id {
                    if let Some(&existing_buffer_id) = self.panel_ids.get(pid) {
                        // Verify the buffer actually exists (defensive check for stale entries)
                        if self.buffers.contains_key(&existing_buffer_id) {
                            // Panel exists, just update its content
                            if let Err(e) = self.set_virtual_buffer_content(existing_buffer_id, entries)
                            {
                                tracing::error!("Failed to update panel content: {}", e);
                            } else {
                                tracing::info!("Updated existing panel '{}' content", pid);
                            }

                            // Find and focus the split that contains this buffer
                            let splits = self.split_manager.splits_for_buffer(existing_buffer_id);
                            if let Some(&split_id) = splits.first() {
                                self.split_manager.set_active_split(split_id);
                                self.active_buffer = existing_buffer_id;
                                tracing::debug!("Focused split {:?} containing panel buffer", split_id);
                            }

                            // Send response with existing buffer ID
                            if let Some(req_id) = request_id {
                                self.send_plugin_response(crate::plugin_api::PluginResponse::VirtualBufferCreated {
                                    request_id: req_id,
                                    buffer_id: existing_buffer_id,
                                });
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

                // Create a horizontal split with the new buffer
                match self.split_manager.split_active(
                    crate::event::SplitDirection::Horizontal,
                    buffer_id,
                    ratio,
                ) {
                    Ok(new_split_id) => {
                        // Create independent view state for the new split with the buffer in tabs
                        let mut view_state =
                            SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id);
                        view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                        self.split_view_states.insert(new_split_id, view_state);

                        // Focus the new split (the diagnostics panel)
                        self.split_manager.set_active_split(new_split_id);
                        self.active_buffer = buffer_id;

                        tracing::info!(
                            "Created horizontal split with virtual buffer {:?}",
                            buffer_id
                        );
                    }
                    Err(e) => {
                        tracing::error!("Failed to create split: {}", e);
                        // Fall back to just switching to the buffer
                        self.set_active_buffer(buffer_id);
                    }
                }

                // Send response with buffer ID
                if let Some(req_id) = request_id {
                    tracing::trace!("CreateVirtualBufferInSplit: sending response for request_id={}, buffer_id={:?}", req_id, buffer_id);
                    self.send_plugin_response(crate::plugin_api::PluginResponse::VirtualBufferCreated {
                        request_id: req_id,
                        buffer_id,
                    });
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
            PluginCommand::DefineMode {
                name,
                parent,
                bindings,
                read_only,
            } => {
                use crate::buffer_mode::BufferMode;

                let mut mode = BufferMode::new(name.clone()).with_read_only(read_only);

                if let Some(parent_name) = parent {
                    mode = mode.with_parent(parent_name);
                }

                // Parse key bindings from strings
                for (key_str, command) in bindings {
                    if let Some((code, modifiers)) = parse_key_string(&key_str) {
                        mode = mode.with_binding(code, modifiers, command);
                    } else {
                        tracing::warn!("Failed to parse key binding: {}", key_str);
                    }
                }

                self.mode_registry.register(mode);
                tracing::info!("Registered buffer mode '{}'", name);
            }
            PluginCommand::ShowBuffer { buffer_id } => {
                if self.buffers.contains_key(&buffer_id) {
                    self.set_active_buffer(buffer_id);
                    tracing::info!("Switched to buffer {:?}", buffer_id);
                } else {
                    tracing::warn!("Buffer {:?} not found", buffer_id);
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
                    // Focus the target split
                    self.split_manager.set_active_split(split_id);
                    self.active_buffer = buffer_id;
                    tracing::info!(
                        "Displayed virtual buffer {:?} in split {:?}",
                        buffer_id,
                        split_id
                    );
                }

                // Send response with buffer ID
                if let Some(req_id) = request_id {
                    self.send_plugin_response(crate::plugin_api::PluginResponse::VirtualBufferCreated {
                        request_id: req_id,
                        buffer_id,
                    });
                }
            }
            PluginCommand::CloseBuffer { buffer_id } => {
                match self.close_buffer(buffer_id) {
                    Ok(()) => {
                        tracing::info!("Closed buffer {:?}", buffer_id);
                    }
                    Err(e) => {
                        tracing::error!("Failed to close buffer {:?}: {}", buffer_id, e);
                    }
                }
            }
            PluginCommand::FocusSplit { split_id } => {
                if self.split_manager.set_active_split(split_id) {
                    // Update active buffer to match the split's buffer
                    if let Some(buffer_id) = self.split_manager.active_buffer_id() {
                        self.active_buffer = buffer_id;
                    }
                    tracing::info!("Focused split {:?}", split_id);
                } else {
                    tracing::warn!("Split {:?} not found", split_id);
                }
            }
            PluginCommand::SetSplitBuffer { split_id, buffer_id } => {
                // Verify the buffer exists
                if !self.buffers.contains_key(&buffer_id) {
                    tracing::error!("Buffer {:?} not found for SetSplitBuffer", buffer_id);
                    return Ok(());
                }

                match self.split_manager.set_split_buffer(split_id, buffer_id) {
                    Ok(()) => {
                        tracing::info!("Set split {:?} to buffer {:?}", split_id, buffer_id);
                        // If this is the active split, update active buffer with all side effects
                        if self.split_manager.active_split() == split_id {
                            self.set_active_buffer(buffer_id);
                        }
                    }
                    Err(e) => {
                        tracing::error!("Failed to set split buffer: {}", e);
                    }
                }
            }
            PluginCommand::CloseSplit { split_id } => {
                match self.split_manager.close_split(split_id) {
                    Ok(()) => {
                        // Clean up the view state for the closed split
                        self.split_view_states.remove(&split_id);
                        // Restore cursor and viewport state for the new active split
                        self.restore_current_split_view_state();
                        tracing::info!("Closed split {:?}", split_id);
                    }
                    Err(e) => {
                        tracing::warn!("Failed to close split {:?}: {}", split_id, e);
                    }
                }
            }
        }
        Ok(())
    }

    /// Handle LSP completion response
    fn handle_completion_response(
        &mut self,
        request_id: u64,
        items: Vec<lsp_types::CompletionItem>,
    ) -> io::Result<()> {
        // Check if this is the pending completion request
        if self.pending_completion_request != Some(request_id) {
            tracing::debug!(
                "Ignoring completion response for outdated request {}",
                request_id
            );
            return Ok(());
        }

        self.pending_completion_request = None;
        self.lsp_status.clear();

        if items.is_empty() {
            tracing::debug!("No completion items received");
            return Ok(());
        }

        // Get the partial word at cursor to filter completions
        use crate::word_navigation::find_completion_word_start;
        let (word_start, cursor_pos) = {
            let state = self.active_state();
            let cursor_pos = state.cursors.primary().position;
            let word_start = find_completion_word_start(&state.buffer, cursor_pos);
            (word_start, cursor_pos)
        };
        let prefix = if word_start < cursor_pos {
            self.active_state_mut()
                .get_text_range(word_start, cursor_pos)
                .to_lowercase()
        } else {
            String::new()
        };

        // Filter completions to match the typed prefix
        let filtered_items: Vec<&lsp_types::CompletionItem> = if prefix.is_empty() {
            // No prefix - show all completions
            items.iter().collect()
        } else {
            // Filter to items that start with the prefix (case-insensitive)
            items
                .iter()
                .filter(|item| {
                    item.label.to_lowercase().starts_with(&prefix)
                        || item
                            .filter_text
                            .as_ref()
                            .map(|ft| ft.to_lowercase().starts_with(&prefix))
                            .unwrap_or(false)
                })
                .collect()
        };

        if filtered_items.is_empty() {
            tracing::debug!("No completion items match prefix '{}'", prefix);
            return Ok(());
        }

        // Convert CompletionItem to PopupListItem
        use crate::popup::PopupListItem;

        let popup_items: Vec<PopupListItem> = filtered_items
            .iter()
            .map(|item| {
                let text = item.label.clone();
                let detail = item.detail.clone();
                let icon = match item.kind {
                    Some(lsp_types::CompletionItemKind::FUNCTION)
                    | Some(lsp_types::CompletionItemKind::METHOD) => Some("λ".to_string()),
                    Some(lsp_types::CompletionItemKind::VARIABLE) => Some("v".to_string()),
                    Some(lsp_types::CompletionItemKind::STRUCT)
                    | Some(lsp_types::CompletionItemKind::CLASS) => Some("S".to_string()),
                    Some(lsp_types::CompletionItemKind::CONSTANT) => Some("c".to_string()),
                    Some(lsp_types::CompletionItemKind::KEYWORD) => Some("k".to_string()),
                    _ => None,
                };

                let mut list_item = PopupListItem::new(text);
                if let Some(detail) = detail {
                    list_item = list_item.with_detail(detail);
                }
                if let Some(icon) = icon {
                    list_item = list_item.with_icon(icon);
                }
                // Store the insert_text or label as data
                let data = item
                    .insert_text
                    .clone()
                    .or_else(|| Some(item.label.clone()));
                if let Some(data) = data {
                    list_item = list_item.with_data(data);
                }
                list_item
            })
            .collect();

        // Show the popup
        use crate::event::{PopupContentData, PopupData, PopupListItemData, PopupPositionData};
        let popup_data = PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: popup_items
                    .into_iter()
                    .map(|item| PopupListItemData {
                        text: item.text,
                        detail: item.detail,
                        icon: item.icon,
                        data: item.data,
                    })
                    .collect(),
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        };

        self.active_state_mut()
            .apply(&crate::event::Event::ShowPopup { popup: popup_data });

        tracing::info!("Showing completion popup with {} items", items.len());

        Ok(())
    }

    /// Handle LSP go-to-definition response
    fn handle_goto_definition_response(
        &mut self,
        request_id: u64,
        locations: Vec<lsp_types::Location>,
    ) -> io::Result<()> {
        // Check if this is the pending request
        if self.pending_goto_definition_request != Some(request_id) {
            tracing::debug!(
                "Ignoring go-to-definition response for outdated request {}",
                request_id
            );
            return Ok(());
        }

        self.pending_goto_definition_request = None;

        if locations.is_empty() {
            self.status_message = Some("No definition found".to_string());
            return Ok(());
        }

        // For now, just jump to the first location
        let location = &locations[0];

        // Convert URI to file path
        if let Ok(path) = uri_to_path(&location.uri) {
            // Open the file
            let buffer_id = self.open_file(&path)?;

            // Move cursor to the definition position
            let line = location.range.start.line as usize;
            let character = location.range.start.character as usize;

            // Calculate byte position from line and character
            if let Some(state) = self.buffers.get(&buffer_id) {
                let position = state.buffer.line_col_to_position(line, character);

                // Move cursor
                let cursor_id = state.cursors.primary_id();
                let old_position = state.cursors.primary().position;
                let old_anchor = state.cursors.primary().anchor;
                let old_sticky_column = state.cursors.primary().sticky_column;
                let event = crate::event::Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: position,
                    old_anchor,
                    new_anchor: None,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for goto definition
                };

                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.apply(&event);
                }
            }

            self.status_message = Some(format!(
                "Jumped to definition at {}:{}",
                path.display(),
                line + 1
            ));
        } else {
            self.status_message = Some("Could not open definition location".to_string());
        }

        Ok(())
    }

    /// Check if there are any pending LSP requests
    pub fn has_pending_lsp_requests(&self) -> bool {
        self.pending_completion_request.is_some() || self.pending_goto_definition_request.is_some()
    }

    /// Cancel any pending LSP requests
    /// This should be called when the user performs an action that would make
    /// the pending request's results stale (e.g., cursor movement, text editing)
    fn cancel_pending_lsp_requests(&mut self) {
        if let Some(request_id) = self.pending_completion_request.take() {
            tracing::debug!("Canceling pending LSP completion request {}", request_id);
            // Send cancellation to the LSP server
            self.send_lsp_cancel_request(request_id);
            self.lsp_status.clear();
        }
        if let Some(request_id) = self.pending_goto_definition_request.take() {
            tracing::debug!(
                "Canceling pending LSP goto-definition request {}",
                request_id
            );
            // Send cancellation to the LSP server
            self.send_lsp_cancel_request(request_id);
            self.lsp_status.clear();
        }
    }

    /// Send a cancel request to the LSP server for a specific request ID
    fn send_lsp_cancel_request(&mut self, request_id: u64) {
        // Get the current file path to determine language
        let metadata = self.buffer_metadata.get(&self.active_buffer);
        let file_path = metadata.and_then(|meta| meta.file_path());

        if let Some(path) = file_path {
            if let Some(language) = crate::lsp_manager::detect_language(path) {
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        if let Err(e) = handle.cancel_request(request_id) {
                            tracing::warn!("Failed to send LSP cancel request: {}", e);
                        } else {
                            tracing::debug!(
                                "Sent $/cancelRequest for request_id={}",
                                request_id
                            );
                        }
                    }
                }
            }
        }
    }

    /// Request LSP completion at current cursor position
    fn request_completion(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);

        // Get the current file URI and path
        let metadata = self.buffer_metadata.get(&self.active_buffer);
        let (uri, file_path) = if let Some(meta) = metadata {
            (meta.file_uri(), meta.file_path())
        } else {
            (None, None)
        };

        if let (Some(uri), Some(path)) = (uri, file_path) {
            // Detect language from file extension
            if let Some(language) = crate::lsp_manager::detect_language(path) {
                // Get LSP handle
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_completion_request = Some(request_id);
                        self.lsp_status = "LSP: completion...".to_string();

                        let _ = handle.completion(
                            request_id,
                            uri.clone(),
                            line as u32,
                            character as u32,
                        );
                        tracing::info!(
                            "Requested completion at {}:{}:{}",
                            uri.as_str(),
                            line,
                            character
                        );
                    }
                }
            }
        }

        Ok(())
    }

    /// Request LSP go-to-definition at current cursor position
    fn request_goto_definition(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);

        // Get the current file URI and path
        let metadata = self.buffer_metadata.get(&self.active_buffer);
        let (uri, file_path) = if let Some(meta) = metadata {
            (meta.file_uri(), meta.file_path())
        } else {
            (None, None)
        };

        if let (Some(uri), Some(path)) = (uri, file_path) {
            // Detect language from file extension
            if let Some(language) = crate::lsp_manager::detect_language(path) {
                // Get LSP handle
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_goto_definition_request = Some(request_id);

                        let _ = handle.goto_definition(
                            request_id,
                            uri.clone(),
                            line as u32,
                            character as u32,
                        );
                        tracing::info!(
                            "Requested go-to-definition at {}:{}:{}",
                            uri.as_str(),
                            line,
                            character
                        );
                    }
                }
            }
        }

        Ok(())
    }

    /// Request LSP hover documentation at current cursor position
    fn request_hover(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);

        // Debug: Log the position conversion details
        if let Some(pos) = state.buffer.offset_to_position(cursor_pos) {
            tracing::debug!(
                "Hover request: cursor_byte={}, line={}, byte_col={}, utf16_col={}",
                cursor_pos,
                pos.line,
                pos.column,
                character
            );
        }

        // Get the current file URI and path
        let metadata = self.buffer_metadata.get(&self.active_buffer);
        let (uri, file_path) = if let Some(meta) = metadata {
            (meta.file_uri(), meta.file_path())
        } else {
            (None, None)
        };

        if let (Some(uri), Some(path)) = (uri, file_path) {
            // Detect language from file extension
            if let Some(language) = crate::lsp_manager::detect_language(path) {
                // Get LSP handle
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_hover_request = Some(request_id);
                        self.lsp_status = "LSP: hover...".to_string();

                        let _ = handle.hover(
                            request_id,
                            uri.clone(),
                            line as u32,
                            character as u32,
                        );
                        tracing::info!(
                            "Requested hover at {}:{}:{} (byte_pos={})",
                            uri.as_str(),
                            line,
                            character,
                            cursor_pos
                        );
                    }
                }
            }
        }

        Ok(())
    }

    /// Handle hover response from LSP
    fn handle_hover_response(
        &mut self,
        request_id: u64,
        contents: String,
        is_markdown: bool,
        range: Option<((u32, u32), (u32, u32))>,
    ) {
        // Check if this response is for the current pending request
        if self.pending_hover_request != Some(request_id) {
            tracing::debug!("Ignoring stale hover response: {}", request_id);
            return;
        }

        self.pending_hover_request = None;
        self.lsp_status.clear();

        if contents.is_empty() {
            self.set_status_message("No hover information available".to_string());
            self.hover_symbol_range = None;
            return;
        }

        // Convert LSP range to byte offsets for highlighting
        if let Some(((start_line, start_char), (end_line, end_char))) = range {
            let state = self.active_state();
            let start_byte = state
                .buffer
                .lsp_position_to_byte(start_line as usize, start_char as usize);
            let end_byte = state
                .buffer
                .lsp_position_to_byte(end_line as usize, end_char as usize);
            self.hover_symbol_range = Some((start_byte, end_byte));
            tracing::debug!(
                "Hover symbol range: {}..{} (LSP {}:{}..{}:{})",
                start_byte,
                end_byte,
                start_line,
                start_char,
                end_line,
                end_char
            );

            // Add overlay to highlight the hovered symbol
            let event = crate::event::Event::AddOverlay {
                overlay_id: "hover_symbol".to_string(),
                range: start_byte..end_byte,
                face: crate::event::OverlayFace::Background {
                    color: (80, 80, 120), // Subtle highlight for hovered symbol
                },
                priority: 90, // Below rename (100) but above syntax (lower)
                message: None,
            };
            self.apply_event_to_active_buffer(&event);
        } else {
            // No range provided by LSP, clear any previous highlight
            self.hover_symbol_range = None;
        }

        // Create a popup with the hover contents
        use crate::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        // Use markdown rendering if the content is markdown
        let mut popup = if is_markdown {
            Popup::markdown(&contents, &self.theme)
        } else {
            // Plain text - split by lines
            let lines: Vec<String> = contents.lines().map(|s| s.to_string()).collect();
            Popup::text(lines, &self.theme)
        };

        // Configure popup properties
        popup.title = Some("Hover".to_string());
        popup.position = PopupPosition::BelowCursor;
        popup.width = 80;
        popup.max_height = 20;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        // Show the popup
        if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
            state.popups.show(popup);
            tracing::info!("Showing hover popup (markdown={})", is_markdown);
        }
    }

    /// Apply inlay hints to editor state as virtual text
    fn apply_inlay_hints_to_state(state: &mut crate::state::EditorState, hints: &[lsp_types::InlayHint]) {
        use crate::virtual_text::VirtualTextPosition;
        use ratatui::style::{Color, Style};

        // Clear existing inlay hints
        state.virtual_texts.clear(&mut state.marker_list);

        if hints.is_empty() {
            return;
        }

        // Style for inlay hints - dimmed to not distract from actual code
        let hint_style = Style::default().fg(Color::Rgb(128, 128, 128));

        for hint in hints {
            // Convert LSP position to byte offset
            let byte_offset = state.buffer.lsp_position_to_byte(
                hint.position.line as usize,
                hint.position.character as usize,
            );

            // Extract text from hint label
            let text = match &hint.label {
                lsp_types::InlayHintLabel::String(s) => s.clone(),
                lsp_types::InlayHintLabel::LabelParts(parts) => {
                    parts.iter().map(|p| p.value.as_str()).collect::<String>()
                }
            };

            // Determine position based on hint kind
            // Type hints go after, parameter hints go before
            let position = match hint.kind {
                Some(lsp_types::InlayHintKind::TYPE) => VirtualTextPosition::AfterChar,
                Some(lsp_types::InlayHintKind::PARAMETER) => VirtualTextPosition::BeforeChar,
                _ => VirtualTextPosition::AfterChar, // Default to after
            };

            // Use the hint text as-is - spacing is handled during rendering
            let display_text = text;

            state.virtual_texts.add(
                &mut state.marker_list,
                byte_offset,
                display_text,
                hint_style,
                position,
                0, // Default priority
            );
        }

        tracing::debug!("Applied {} inlay hints as virtual text", hints.len());
    }

    /// Request LSP find references at current cursor position
    fn request_references(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Extract the word under cursor for display
        let symbol = {
            let text = state.buffer.to_string();
            let bytes = text.as_bytes();
            let buf_len = bytes.len();

            if cursor_pos <= buf_len {
                // Find word boundaries
                let is_word_char = |c: char| c.is_alphanumeric() || c == '_';

                // Find start of word
                let mut start = cursor_pos;
                while start > 0 {
                    // Move to previous byte
                    start -= 1;
                    // Skip continuation bytes (UTF-8)
                    while start > 0 && (bytes[start] & 0xC0) == 0x80 {
                        start -= 1;
                    }
                    // Get the character at this position
                    if let Some(ch) = text[start..].chars().next() {
                        if !is_word_char(ch) {
                            start += ch.len_utf8();
                            break;
                        }
                    } else {
                        break;
                    }
                }

                // Find end of word
                let mut end = cursor_pos;
                while end < buf_len {
                    if let Some(ch) = text[end..].chars().next() {
                        if is_word_char(ch) {
                            end += ch.len_utf8();
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                if start < end {
                    text[start..end].to_string()
                } else {
                    String::new()
                }
            } else {
                String::new()
            }
        };

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);

        // Get the current file URI and path
        let metadata = self.buffer_metadata.get(&self.active_buffer);
        let (uri, file_path) = if let Some(meta) = metadata {
            (meta.file_uri(), meta.file_path())
        } else {
            (None, None)
        };

        if let (Some(uri), Some(path)) = (uri, file_path) {
            // Detect language from file extension
            if let Some(language) = crate::lsp_manager::detect_language(path) {
                // Get LSP handle
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_references_request = Some(request_id);
                        self.pending_references_symbol = symbol;
                        self.lsp_status = "LSP: finding references...".to_string();

                        let _ = handle.references(
                            request_id,
                            uri.clone(),
                            line as u32,
                            character as u32,
                        );
                        tracing::info!(
                            "Requested find references at {}:{}:{} (byte_pos={})",
                            uri.as_str(),
                            line,
                            character,
                            cursor_pos
                        );
                    }
                }
            }
        }

        Ok(())
    }

    /// Request LSP signature help at current cursor position
    fn request_signature_help(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);

        // Get the current file URI and path
        let metadata = self.buffer_metadata.get(&self.active_buffer);
        let (uri, file_path) = if let Some(meta) = metadata {
            (meta.file_uri(), meta.file_path())
        } else {
            (None, None)
        };

        if let (Some(uri), Some(path)) = (uri, file_path) {
            // Detect language from file extension
            if let Some(language) = crate::lsp_manager::detect_language(path) {
                // Get LSP handle
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_signature_help_request = Some(request_id);
                        self.lsp_status = "LSP: signature help...".to_string();

                        let _ = handle.signature_help(
                            request_id,
                            uri.clone(),
                            line as u32,
                            character as u32,
                        );
                        tracing::info!(
                            "Requested signature help at {}:{}:{} (byte_pos={})",
                            uri.as_str(),
                            line,
                            character,
                            cursor_pos
                        );
                    }
                }
            }
        }

        Ok(())
    }

    /// Handle signature help response from LSP
    fn handle_signature_help_response(
        &mut self,
        request_id: u64,
        signature_help: Option<lsp_types::SignatureHelp>,
    ) {
        // Check if this response is for the current pending request
        if self.pending_signature_help_request != Some(request_id) {
            tracing::debug!("Ignoring stale signature help response: {}", request_id);
            return;
        }

        self.pending_signature_help_request = None;
        self.lsp_status.clear();

        let signature_help = match signature_help {
            Some(help) if !help.signatures.is_empty() => help,
            _ => {
                tracing::debug!("No signature help available");
                return;
            }
        };

        // Get the active signature
        let active_signature_idx = signature_help.active_signature.unwrap_or(0) as usize;
        let signature = match signature_help.signatures.get(active_signature_idx) {
            Some(sig) => sig,
            None => return,
        };

        // Build the display content
        let mut lines: Vec<String> = Vec::new();

        // Add the signature label (function signature)
        lines.push(signature.label.clone());

        // Add parameter highlighting info
        let active_param = signature_help
            .active_parameter
            .or(signature.active_parameter)
            .unwrap_or(0) as usize;

        // If there are parameters, highlight the active one
        if let Some(params) = &signature.parameters {
            if let Some(param) = params.get(active_param) {
                // Get parameter label
                let param_label = match &param.label {
                    lsp_types::ParameterLabel::Simple(s) => s.clone(),
                    lsp_types::ParameterLabel::LabelOffsets(offsets) => {
                        // Extract substring from signature label
                        let start = offsets[0] as usize;
                        let end = offsets[1] as usize;
                        if end <= signature.label.len() {
                            signature.label[start..end].to_string()
                        } else {
                            String::new()
                        }
                    }
                };

                if !param_label.is_empty() {
                    lines.push(format!("▶ {}", param_label));
                }

                // Add parameter documentation if available
                if let Some(doc) = &param.documentation {
                    let doc_text = match doc {
                        lsp_types::Documentation::String(s) => s.clone(),
                        lsp_types::Documentation::MarkupContent(m) => m.value.clone(),
                    };
                    if !doc_text.is_empty() {
                        lines.push(String::new());
                        lines.push(doc_text);
                    }
                }
            }
        }

        // Add function documentation if available
        if let Some(doc) = &signature.documentation {
            let doc_text = match doc {
                lsp_types::Documentation::String(s) => s.clone(),
                lsp_types::Documentation::MarkupContent(m) => m.value.clone(),
            };
            if !doc_text.is_empty() {
                if lines.len() > 1 {
                    lines.push(String::new());
                    lines.push("---".to_string());
                }
                lines.push(doc_text);
            }
        }

        // Create a popup with the signature help
        use crate::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        let mut popup = Popup::text(lines, &self.theme);
        popup.title = Some("Signature Help".to_string());
        popup.position = PopupPosition::BelowCursor;
        popup.width = 60;
        popup.max_height = 10;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        // Show the popup
        if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
            state.popups.show(popup);
            tracing::info!(
                "Showing signature help popup for {} signatures",
                signature_help.signatures.len()
            );
        }
    }

    /// Request LSP code actions at current cursor position
    fn request_code_actions(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);

        // Get selection range (if any) or use cursor position
        let (start_line, start_char, end_line, end_char) =
            if let Some(range) = state.cursors.primary().selection_range() {
                let (s_line, s_char) = state.buffer.position_to_lsp_position(range.start);
                let (e_line, e_char) = state.buffer.position_to_lsp_position(range.end);
                (s_line as u32, s_char as u32, e_line as u32, e_char as u32)
            } else {
                (line as u32, character as u32, line as u32, character as u32)
            };

        // Get diagnostics at cursor position for context
        // TODO: Implement diagnostic retrieval when needed
        let diagnostics = Vec::new();

        // Get the current file URI and path
        let metadata = self.buffer_metadata.get(&self.active_buffer);
        let (uri, file_path) = if let Some(meta) = metadata {
            (meta.file_uri(), meta.file_path())
        } else {
            (None, None)
        };

        if let (Some(uri), Some(path)) = (uri, file_path) {
            // Detect language from file extension
            if let Some(language) = crate::lsp_manager::detect_language(path) {
                // Get LSP handle
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_code_actions_request = Some(request_id);
                        self.lsp_status = "LSP: code actions...".to_string();

                        let _ = handle.code_actions(
                            request_id,
                            uri.clone(),
                            start_line,
                            start_char,
                            end_line,
                            end_char,
                            diagnostics,
                        );
                        tracing::info!(
                            "Requested code actions at {}:{}:{}-{}:{} (byte_pos={})",
                            uri.as_str(),
                            start_line,
                            start_char,
                            end_line,
                            end_char,
                            cursor_pos
                        );
                    }
                }
            }
        }

        Ok(())
    }

    /// Handle code actions response from LSP
    fn handle_code_actions_response(
        &mut self,
        request_id: u64,
        actions: Vec<lsp_types::CodeActionOrCommand>,
    ) {
        // Check if this response is for the current pending request
        if self.pending_code_actions_request != Some(request_id) {
            tracing::debug!("Ignoring stale code actions response: {}", request_id);
            return;
        }

        self.pending_code_actions_request = None;
        self.lsp_status.clear();

        if actions.is_empty() {
            self.set_status_message("No code actions available".to_string());
            return;
        }

        // Build the display content
        let mut lines: Vec<String> = Vec::new();
        lines.push(format!("Code Actions ({}):", actions.len()));
        lines.push(String::new());

        for (i, action) in actions.iter().enumerate() {
            let title = match action {
                lsp_types::CodeActionOrCommand::Command(cmd) => &cmd.title,
                lsp_types::CodeActionOrCommand::CodeAction(ca) => &ca.title,
            };
            lines.push(format!("  {}. {}", i + 1, title));
        }

        lines.push(String::new());
        lines.push("Press number to select, Esc to cancel".to_string());

        // Create a popup with the code actions
        use crate::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        let mut popup = Popup::text(lines, &self.theme);
        popup.title = Some("Code Actions".to_string());
        popup.position = PopupPosition::BelowCursor;
        popup.width = 60;
        popup.max_height = 15;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        // Show the popup
        if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
            state.popups.show(popup);
            tracing::info!("Showing code actions popup with {} actions", actions.len());
        }

        // Note: Executing code actions would require storing the actions and handling
        // key presses to select and apply them. This is left for future enhancement.
        self.set_status_message(format!(
            "Found {} code action(s) - selection not yet implemented",
            actions.len()
        ));
    }

    /// Handle find references response from LSP
    fn handle_references_response(
        &mut self,
        request_id: u64,
        locations: Vec<lsp_types::Location>,
    ) -> io::Result<()> {
        tracing::info!(
            "handle_references_response: received {} locations for request_id={}",
            locations.len(),
            request_id
        );

        // Check if this response is for the current pending request
        if self.pending_references_request != Some(request_id) {
            tracing::debug!("Ignoring stale references response: {}", request_id);
            return Ok(());
        }

        self.pending_references_request = None;
        self.lsp_status.clear();

        if locations.is_empty() {
            self.set_status_message("No references found".to_string());
            return Ok(());
        }

        // Convert locations to hook args format
        let lsp_locations: Vec<crate::hooks::LspLocation> = locations
            .iter()
            .map(|loc| {
                // Convert URI to file path
                let file = if loc.uri.scheme().map(|s| s.as_str()) == Some("file") {
                    // Extract path from file:// URI
                    loc.uri.path().as_str().to_string()
                } else {
                    loc.uri.as_str().to_string()
                };

                crate::hooks::LspLocation {
                    file,
                    line: loc.range.start.line + 1, // LSP is 0-based, convert to 1-based
                    column: loc.range.start.character + 1, // LSP is 0-based
                }
            })
            .collect();

        let count = lsp_locations.len();
        let symbol = std::mem::take(&mut self.pending_references_symbol);
        self.set_status_message(format!("Found {} reference(s) for '{}'", count, symbol));

        // Fire the lsp_references hook so plugins can display the results
        let args = crate::hooks::HookArgs::LspReferences {
            symbol: symbol.clone(),
            locations: lsp_locations,
        };
        if let Some(ref ts_manager) = self.ts_plugin_manager {
            ts_manager.run_hook("lsp_references", args);
        }

        tracing::info!(
            "Fired lsp_references hook with {} locations for symbol '{}'",
            count,
            symbol
        );

        Ok(())
    }

    /// Handle rename response from LSP
    pub fn handle_rename_response(
        &mut self,
        _request_id: u64,
        result: Result<lsp_types::WorkspaceEdit, String>,
    ) -> io::Result<()> {
        self.lsp_status.clear();

        match result {
            Ok(workspace_edit) => {
                // Log the full workspace edit for debugging
                tracing::debug!(
                    "Received WorkspaceEdit: changes={:?}, document_changes={:?}",
                    workspace_edit.changes.as_ref().map(|c| c.len()),
                    workspace_edit.document_changes.as_ref().map(|dc| match dc {
                        lsp_types::DocumentChanges::Edits(e) => format!("{} edits", e.len()),
                        lsp_types::DocumentChanges::Operations(o) =>
                            format!("{} operations", o.len()),
                    })
                );

                // Apply the workspace edit
                let mut total_changes = 0;

                // Handle changes (map of URI -> Vec<TextEdit>)
                if let Some(changes) = workspace_edit.changes {
                    for (uri, edits) in changes {
                        if let Ok(path) = uri_to_path(&uri) {
                            // Open the file if not already open
                            let buffer_id = self.open_file(&path)?;

                            // Sort edits by position (reverse order to avoid offset issues)
                            let mut sorted_edits = edits;
                            sorted_edits.sort_by(|a, b| {
                                b.range
                                    .start
                                    .line
                                    .cmp(&a.range.start.line)
                                    .then(b.range.start.character.cmp(&a.range.start.character))
                            });

                            // Collect all events for this buffer into a batch
                            let mut batch_events = Vec::new();

                            // Create events for all edits
                            for edit in sorted_edits {
                                let state = self.buffers.get_mut(&buffer_id).ok_or_else(|| {
                                    io::Error::new(io::ErrorKind::NotFound, "Buffer not found")
                                })?;

                                // Convert LSP range to byte positions
                                // LSP uses UTF-16 code units, not byte offsets
                                let start_line = edit.range.start.line as usize;
                                let start_char = edit.range.start.character as usize;
                                let end_line = edit.range.end.line as usize;
                                let end_char = edit.range.end.character as usize;

                                let start_pos =
                                    state.buffer.lsp_position_to_byte(start_line, start_char);
                                let end_pos = state.buffer.lsp_position_to_byte(end_line, end_char);
                                let buffer_len = state.buffer.len();

                                // Log the conversion for debugging
                                let old_text = if start_pos < end_pos && end_pos <= buffer_len {
                                    state.get_text_range(start_pos, end_pos)
                                } else {
                                    format!(
                                        "<invalid range: start={}, end={}, buffer_len={}>",
                                        start_pos, end_pos, buffer_len
                                    )
                                };
                                tracing::debug!("  Converting LSP range line {}:{}-{}:{} to bytes {}..{} (replacing {:?} with {:?})",
                                    start_line, start_char, end_line, end_char,
                                    start_pos, end_pos, old_text, edit.new_text);

                                // Delete old text
                                if start_pos < end_pos {
                                    let deleted_text = state.get_text_range(start_pos, end_pos);
                                    let cursor_id = state.cursors.primary_id();
                                    let delete_event = Event::Delete {
                                        range: start_pos..end_pos,
                                        deleted_text,
                                        cursor_id,
                                    };
                                    batch_events.push(delete_event);
                                }

                                // Insert new text
                                if !edit.new_text.is_empty() {
                                    let state = self.buffers.get(&buffer_id).ok_or_else(|| {
                                        io::Error::new(io::ErrorKind::NotFound, "Buffer not found")
                                    })?;
                                    let cursor_id = state.cursors.primary_id();
                                    let insert_event = Event::Insert {
                                        position: start_pos,
                                        text: edit.new_text.clone(),
                                        cursor_id,
                                    };
                                    batch_events.push(insert_event);
                                }

                                total_changes += 1;
                            }

                            // Create a batch event for all rename changes
                            if !batch_events.is_empty() {
                                let batch = Event::Batch {
                                    events: batch_events,
                                    description: "LSP Rename".to_string(),
                                };

                                self.apply_rename_batch_to_buffer(buffer_id, batch)?;
                            }
                        }
                    }
                }

                // Handle document_changes (TextDocumentEdit[])
                // This is what rust-analyzer sends instead of changes
                if let Some(document_changes) = workspace_edit.document_changes {
                    use lsp_types::DocumentChanges;

                    let text_edits = match document_changes {
                        DocumentChanges::Edits(edits) => edits,
                        DocumentChanges::Operations(ops) => {
                            // Extract TextDocumentEdit from operations
                            ops.into_iter()
                                .filter_map(|op| {
                                    if let lsp_types::DocumentChangeOperation::Edit(edit) = op {
                                        Some(edit)
                                    } else {
                                        None
                                    }
                                })
                                .collect()
                        }
                    };

                    for text_doc_edit in text_edits {
                        let uri = text_doc_edit.text_document.uri;

                        if let Ok(path) = uri_to_path(&uri) {
                            // Open the file if not already open
                            let buffer_id = self.open_file(&path)?;

                            // Extract TextEdit from OneOf<TextEdit, AnnotatedTextEdit>
                            let edits: Vec<lsp_types::TextEdit> = text_doc_edit
                                .edits
                                .into_iter()
                                .map(|one_of| match one_of {
                                    lsp_types::OneOf::Left(text_edit) => text_edit,
                                    lsp_types::OneOf::Right(annotated) => annotated.text_edit,
                                })
                                .collect();

                            // Log the edits for debugging
                            tracing::info!(
                                "Applying {} edits from rust-analyzer for {:?}:",
                                edits.len(),
                                path
                            );
                            for (i, edit) in edits.iter().enumerate() {
                                tracing::info!(
                                    "  Edit {}: line {}:{}-{}:{} -> {:?}",
                                    i,
                                    edit.range.start.line,
                                    edit.range.start.character,
                                    edit.range.end.line,
                                    edit.range.end.character,
                                    edit.new_text
                                );
                            }

                            // Sort edits by position (reverse order to avoid offset issues)
                            let mut sorted_edits = edits;
                            sorted_edits.sort_by(|a, b| {
                                b.range
                                    .start
                                    .line
                                    .cmp(&a.range.start.line)
                                    .then(b.range.start.character.cmp(&a.range.start.character))
                            });

                            // Collect all events for this buffer into a batch
                            let mut batch_events = Vec::new();

                            // Create events for all edits
                            for edit in sorted_edits {
                                let state = self.buffers.get_mut(&buffer_id).ok_or_else(|| {
                                    io::Error::new(io::ErrorKind::NotFound, "Buffer not found")
                                })?;

                                // Convert LSP range to byte positions
                                // LSP uses UTF-16 code units, not byte offsets
                                let start_line = edit.range.start.line as usize;
                                let start_char = edit.range.start.character as usize;
                                let end_line = edit.range.end.line as usize;
                                let end_char = edit.range.end.character as usize;

                                let start_pos =
                                    state.buffer.lsp_position_to_byte(start_line, start_char);
                                let end_pos = state.buffer.lsp_position_to_byte(end_line, end_char);
                                let buffer_len = state.buffer.len();

                                // Log the conversion for debugging
                                let old_text = if start_pos < end_pos && end_pos <= buffer_len {
                                    state.get_text_range(start_pos, end_pos)
                                } else {
                                    format!(
                                        "<invalid range: start={}, end={}, buffer_len={}>",
                                        start_pos, end_pos, buffer_len
                                    )
                                };
                                tracing::debug!("  Converting LSP range line {}:{}-{}:{} to bytes {}..{} (replacing {:?} with {:?})",
                                    start_line, start_char, end_line, end_char,
                                    start_pos, end_pos, old_text, edit.new_text);

                                // Delete old text
                                if start_pos < end_pos {
                                    let deleted_text = state.get_text_range(start_pos, end_pos);
                                    let cursor_id = state.cursors.primary_id();
                                    let delete_event = Event::Delete {
                                        range: start_pos..end_pos,
                                        deleted_text,
                                        cursor_id,
                                    };
                                    batch_events.push(delete_event);
                                }

                                // Insert new text
                                if !edit.new_text.is_empty() {
                                    let state = self.buffers.get(&buffer_id).ok_or_else(|| {
                                        io::Error::new(io::ErrorKind::NotFound, "Buffer not found")
                                    })?;
                                    let cursor_id = state.cursors.primary_id();
                                    let insert_event = Event::Insert {
                                        position: start_pos,
                                        text: edit.new_text.clone(),
                                        cursor_id,
                                    };
                                    batch_events.push(insert_event);
                                }

                                total_changes += 1;
                            }

                            // Create a batch event for all rename changes
                            if !batch_events.is_empty() {
                                let batch = Event::Batch {
                                    events: batch_events,
                                    description: "LSP Rename".to_string(),
                                };

                                self.apply_rename_batch_to_buffer(buffer_id, batch)?;
                            }
                        }
                    }
                }

                self.status_message =
                    Some(format!("Renamed successfully ({} changes)", total_changes));
            }
            Err(error) => {
                // Per LSP spec: ContentModified errors (-32801) should NOT be shown to user
                // These are expected when document changes during LSP operations
                // Reference: https://github.com/neovim/neovim/issues/16900
                if error.contains("content modified") || error.contains("-32801") {
                    tracing::debug!(
                        "LSP rename: ContentModified error (expected, ignoring): {}",
                        error
                    );
                    self.status_message =
                        Some("Rename cancelled (document was modified)".to_string());
                } else {
                    // Show other errors to user
                    self.status_message = Some(format!("Rename failed: {}", error));
                }
            }
        }

        Ok(())
    }

    /// Helper to apply a batch of rename events to a specific buffer and notify LSP
    fn apply_rename_batch_to_buffer(
        &mut self,
        buffer_id: BufferId,
        batch: Event,
    ) -> io::Result<()> {
        // Add to event log
        if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
            event_log.append(batch.clone());
        }

        // IMPORTANT: Calculate LSP changes BEFORE applying to buffer!
        // The byte positions in the events are relative to the ORIGINAL buffer,
        // so we must convert them to LSP positions before modifying the buffer.
        // Otherwise, the LSP server will receive incorrect position information.
        let original_active = self.active_buffer;
        self.active_buffer = buffer_id;
        let lsp_changes = self.collect_lsp_changes(&batch);
        self.active_buffer = original_active;

        // Save cursor position before applying batch
        // The batch will move the cursor to each edit location, but we want to
        // preserve the cursor position (adjusted for edits before it)
        let state = self.buffers.get(&buffer_id).ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, "Buffer not found")
        })?;
        let original_cursor_pos = state.cursors.primary().position;
        let original_cursor_anchor = state.cursors.primary().anchor;

        // Calculate cursor position adjustment based on edits
        // Edits are applied in reverse order (end of file to start), but we need
        // to calculate the cumulative delta for all edits before the cursor
        let mut cursor_delta: isize = 0;
        if let Event::Batch { events, .. } = &batch {
            for event in events {
                match event {
                    Event::Delete { range, .. } => {
                        if range.end <= original_cursor_pos {
                            // Delete entirely before cursor - cursor moves back
                            cursor_delta -= range.len() as isize;
                        } else if range.start < original_cursor_pos {
                            // Delete crosses cursor - cursor moves to start of delete
                            cursor_delta = range.start as isize - original_cursor_pos as isize;
                        }
                        // Delete entirely after cursor - no effect
                    }
                    Event::Insert { position, text, .. } => {
                        // Only move cursor if insert is STRICTLY BEFORE cursor position
                        // If insert is AT cursor, cursor should stay at start of new text
                        let adjusted_cursor = (original_cursor_pos as isize + cursor_delta) as usize;
                        if *position < adjusted_cursor {
                            // Insert before cursor - cursor moves forward
                            cursor_delta += text.len() as isize;
                        }
                        // Insert at or after cursor - no effect on cursor position
                    }
                    _ => {}
                }
            }
        }

        // Apply to buffer state
        let state = self.buffers.get_mut(&buffer_id).ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, "Buffer not found")
        })?;
        state.apply(&batch);

        // Restore cursor to adjusted position
        let buffer_len = state.buffer.len();
        let new_cursor_pos = ((original_cursor_pos as isize + cursor_delta).max(0) as usize)
            .min(buffer_len);
        state.cursors.primary_mut().position = new_cursor_pos;

        // Adjust anchor if there was a selection
        if let Some(anchor) = original_cursor_anchor {
            let new_anchor = ((anchor as isize + cursor_delta).max(0) as usize)
                .min(buffer_len);
            state.cursors.primary_mut().anchor = Some(new_anchor);
        }

        // Notify LSP about the changes using pre-calculated positions
        self.send_lsp_changes_for_buffer(buffer_id, lsp_changes);

        Ok(())
    }

    /// Send pre-calculated LSP changes for a specific buffer
    fn send_lsp_changes_for_buffer(
        &mut self,
        buffer_id: BufferId,
        changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
    ) {
        if changes.is_empty() {
            return;
        }

        // Check if LSP is enabled for this buffer
        let metadata = match self.buffer_metadata.get(&buffer_id) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no metadata for buffer {:?}",
                    buffer_id
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            tracing::debug!("send_lsp_changes_for_buffer: LSP disabled for this buffer");
            return;
        }

        // Get the URI
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no URI for buffer (not a file or URI creation failed)"
                );
                return;
            }
        };

        // Get the file path for language detection
        let path = match metadata.file_path() {
            Some(p) => p,
            None => {
                tracing::debug!("send_lsp_changes_for_buffer: no file path for buffer");
                return;
            }
        };

        let language = match detect_language(path) {
            Some(l) => l,
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no language detected for {:?}",
                    path
                );
                return;
            }
        };

        tracing::debug!(
            "send_lsp_changes_for_buffer: sending {} changes to {} in single didChange notification",
            changes.len(),
            uri.as_str()
        );

        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(&language) {
                if let Err(e) = client.did_change(uri, changes) {
                    tracing::warn!("Failed to send didChange to LSP: {}", e);
                } else {
                    tracing::info!("Successfully sent batched didChange to LSP");
                }
            } else {
                tracing::warn!(
                    "send_lsp_changes_for_buffer: failed to get or spawn LSP client for {}",
                    language
                );
            }
        } else {
            tracing::debug!("send_lsp_changes_for_buffer: no LSP manager available");
        }
    }

    /// Start rename mode - select the symbol at cursor and allow inline editing
    fn start_rename(&mut self) -> io::Result<()> {
        use crate::word_navigation::{find_word_end, find_word_start};

        // Get the current buffer and cursor position
        let (word_start, word_end) = {
            let state = self.active_state();
            let cursor_pos = state.cursors.primary().position;

            // Find the word boundaries
            let word_start = find_word_start(&state.buffer, cursor_pos);
            let word_end = find_word_end(&state.buffer, cursor_pos);

            // Check if we're on a word
            if word_start >= word_end {
                self.status_message = Some("No symbol at cursor".to_string());
                return Ok(());
            }

            (word_start, word_end)
        };

        // Get the word text
        let word_text = self.active_state_mut().get_text_range(word_start, word_end);

        // Create an overlay to highlight the symbol being renamed
        let overlay_id = format!("rename_overlay_{}", self.next_lsp_request_id);
        let event = crate::event::Event::AddOverlay {
            overlay_id: overlay_id.clone(),
            range: word_start..word_end,
            face: crate::event::OverlayFace::Background {
                color: (50, 100, 200), // Blue background for rename
            },
            priority: 100,
            message: Some("Renaming".to_string()),
        };

        // Apply the overlay
        if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
            state.apply(&event);
        }

        // Enter rename mode using the Prompt system
        // Store the rename metadata in the PromptType and pre-fill the input with the current name
        let mut prompt = Prompt::new(
            "Rename to: ".to_string(),
            PromptType::LspRename {
                original_text: word_text.clone(),
                start_pos: word_start,
                end_pos: word_end,
                overlay_id,
            },
        );
        // Pre-fill the input with the current name and position cursor at the end
        prompt.set_input(word_text);

        self.prompt = Some(prompt);
        Ok(())
    }

    /// Cancel rename mode - removes overlay if the prompt was for LSP rename
    fn cancel_rename_overlay(&mut self, overlay_id: &str) {
        let remove_overlay_event = crate::event::Event::RemoveOverlay {
            overlay_id: overlay_id.to_string(),
        };
        self.apply_event_to_active_buffer(&remove_overlay_event);
    }

    /// Perform the actual LSP rename request
    fn perform_lsp_rename(
        &mut self,
        new_name: String,
        original_text: String,
        start_pos: usize,
        overlay_id: String,
    ) {
        // Remove the overlay first
        self.cancel_rename_overlay(&overlay_id);

        // Check if the name actually changed
        if new_name == original_text {
            self.status_message = Some("Name unchanged".to_string());
            return;
        }

        // Use the position from when we entered rename mode, NOT the current cursor position
        // This ensures we send the rename request for the correct symbol even if cursor moved
        let rename_pos = start_pos;

        // Convert byte position to LSP position (line, UTF-16 code units)
        // LSP uses UTF-16 code units for character offsets, not byte offsets
        let state = self.active_state();
        let (line, character) = state.buffer.position_to_lsp_position(rename_pos);

        // Get the current file URI and path
        let metadata = self.buffer_metadata.get(&self.active_buffer);
        let (uri, file_path) = if let Some(meta) = metadata {
            (meta.file_uri(), meta.file_path())
        } else {
            (None, None)
        };

        if let (Some(uri), Some(path)) = (uri, file_path) {
            // Detect language from file extension
            if let Some(language) = crate::lsp_manager::detect_language(path) {
                // Get LSP handle
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.lsp_status = "LSP: rename...".to_string();

                        let _ = handle.rename(
                            request_id,
                            uri.clone(),
                            line as u32,
                            character as u32,
                            new_name.clone(),
                        );
                        tracing::info!(
                            "Requested rename at {}:{}:{} to '{}'",
                            uri.as_str(),
                            line,
                            character,
                            new_name
                        );
                    }
                }
            }
        } else {
            self.status_message = Some("Cannot rename in unsaved buffer".to_string());
        }
    }

    /// Determine the current keybinding context based on UI state
    fn get_key_context(&self) -> crate::keybindings::KeyContext {
        use crate::keybindings::KeyContext;

        // Priority order: Help > Menu > Prompt > Popup > Rename > Current context (FileExplorer or Normal)
        if self.help_renderer.is_visible() {
            KeyContext::Help
        } else if self.menu_state.active_menu.is_some() {
            KeyContext::Menu
        } else if self.is_prompting() {
            KeyContext::Prompt
        } else if self.active_state().popups.is_visible() {
            KeyContext::Popup
        } else {
            // Use the current context (can be FileExplorer or Normal)
            self.key_context
        }
    }

    /// Handle a key event and return whether it was handled
    /// This is the central key handling logic used by both main.rs and tests
    pub fn handle_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> std::io::Result<()> {
        use crate::keybindings::Action;

        let _t_total = std::time::Instant::now();

        tracing::debug!(
            "Editor.handle_key: code={:?}, modifiers={:?}",
            code,
            modifiers
        );

        // Determine the current context first
        let mut context = self.get_key_context();

        // Special case: Hover and Signature Help popups should be dismissed on any key press
        if matches!(context, crate::keybindings::KeyContext::Popup) {
            // Check if the current popup is a hover or signature help popup (identified by title)
            let is_dismissable_popup = self
                .active_state()
                .popups
                .top()
                .and_then(|p| p.title.as_ref())
                .is_some_and(|title| title == "Hover" || title == "Signature Help");

            if is_dismissable_popup {
                // Dismiss the popup on any key press
                self.hide_popup();
                tracing::debug!("Dismissed hover/signature help popup on key press");
                // Recalculate context now that popup is gone
                context = self.get_key_context();
            }
        }

        // Only check buffer mode keybindings if we're not in a higher-priority context
        // (Help, Menu, Prompt, Popup should take precedence over mode bindings)
        let should_check_mode_bindings = matches!(
            context,
            crate::keybindings::KeyContext::Normal | crate::keybindings::KeyContext::FileExplorer
        );

        if should_check_mode_bindings {
            // Check buffer mode keybindings (for virtual buffers with custom modes)
            if let Some(command_name) = self.resolve_mode_keybinding(code, modifiers) {
                tracing::debug!("Mode keybinding resolved to command: {}", command_name);
                // Execute the command via the command registry
                let commands = self.command_registry.read().unwrap().get_all();
                if let Some(cmd) = commands.iter().find(|c| c.name == command_name) {
                    let action = cmd.action.clone();
                    drop(commands);
                    return self.handle_action(action);
                } else if command_name == "close-buffer" {
                    // Handle built-in mode commands
                    let buffer_id = self.active_buffer;
                    return self.close_buffer(buffer_id);
                } else if command_name == "revert-buffer" {
                    // Refresh the buffer (for virtual buffers, this would re-query data)
                    self.set_status_message("Refreshing buffer...".to_string());
                    return Ok(());
                } else {
                    // Try as a plugin action
                    let action = Action::PluginAction(command_name.clone());
                    drop(commands);
                    return self.handle_action(action);
                }
            }
        }

        // Resolve the key event to an action
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let action = self.keybindings.resolve(&key_event, context);

        tracing::debug!("Context: {:?} -> Action: {:?}", context, action);

        // Cancel pending LSP requests on user actions (except LSP actions themselves)
        // This ensures stale completions don't show up after the user has moved on
        match action {
            Action::LspCompletion
            | Action::LspGotoDefinition
            | Action::LspReferences
            | Action::LspHover
            | Action::None => {
                // Don't cancel for LSP actions or no-op
            }
            _ => {
                // Cancel any pending LSP requests
                self.cancel_pending_lsp_requests();
            }
        }

        // Handle the action
        match action {
            // Help mode actions
            Action::HelpToggle => {
                self.help_renderer.toggle();
            }
            Action::HelpScrollUp => {
                self.help_renderer.scroll(-1, &self.keybindings);
            }
            Action::HelpScrollDown => {
                self.help_renderer.scroll(1, &self.keybindings);
            }
            Action::HelpPageUp => {
                self.help_renderer.scroll(-10, &self.keybindings);
            }
            Action::HelpPageDown => {
                self.help_renderer.scroll(10, &self.keybindings);
            }

            // Prompt mode actions - delegate to handle_action
            Action::PromptConfirm => {
                return self.handle_action(action);
            }
            Action::PromptCancel => {
                self.cancel_prompt();
            }
            Action::PromptBackspace => {
                if let Some(prompt) = self.prompt_mut() {
                    // If there's a selection, delete it; otherwise delete one character backward
                    if prompt.has_selection() {
                        prompt.delete_selection();
                    } else if prompt.cursor_pos > 0 {
                        let byte_pos = prompt.cursor_pos;
                        let mut char_start = byte_pos - 1;
                        while char_start > 0 && !prompt.input.is_char_boundary(char_start) {
                            char_start -= 1;
                        }
                        prompt.input.remove(char_start);
                        prompt.cursor_pos = char_start;
                    }
                }
                self.update_prompt_suggestions();
            }
            Action::PromptDelete => {
                if let Some(prompt) = self.prompt_mut() {
                    // If there's a selection, delete it; otherwise delete one character forward
                    if prompt.has_selection() {
                        prompt.delete_selection();
                    } else if prompt.cursor_pos < prompt.input.len() {
                        let mut char_end = prompt.cursor_pos + 1;
                        while char_end < prompt.input.len()
                            && !prompt.input.is_char_boundary(char_end)
                        {
                            char_end += 1;
                        }
                        prompt.input.drain(prompt.cursor_pos..char_end);
                    }
                }
                self.update_prompt_suggestions();
            }
            Action::PromptMoveLeft => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.clear_selection();
                    if prompt.cursor_pos > 0 {
                        let mut new_pos = prompt.cursor_pos - 1;
                        while new_pos > 0 && !prompt.input.is_char_boundary(new_pos) {
                            new_pos -= 1;
                        }
                        prompt.cursor_pos = new_pos;
                    }
                }
            }
            Action::PromptMoveRight => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.clear_selection();
                    if prompt.cursor_pos < prompt.input.len() {
                        let mut new_pos = prompt.cursor_pos + 1;
                        while new_pos < prompt.input.len()
                            && !prompt.input.is_char_boundary(new_pos)
                        {
                            new_pos += 1;
                        }
                        prompt.cursor_pos = new_pos;
                    }
                }
            }
            Action::PromptMoveStart => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.clear_selection();
                    prompt.cursor_pos = 0;
                }
            }
            Action::PromptMoveEnd => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.clear_selection();
                    prompt.cursor_pos = prompt.input.len();
                }
            }
            Action::PromptSelectPrev => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        // Suggestions exist: navigate suggestions
                        if let Some(selected) = prompt.selected_suggestion {
                            // Don't wrap around - stay at 0 if already at the beginning
                            prompt.selected_suggestion = if selected == 0 {
                                Some(0)
                            } else {
                                Some(selected - 1)
                            };
                        }
                    } else {
                        // No suggestions: navigate history (Up arrow)
                        let prompt_type = prompt.prompt_type.clone();
                        let current_input = prompt.input.clone();

                        // Get the appropriate history based on prompt type
                        let history_item = match prompt_type {
                            PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch => {
                                self.search_history.navigate_prev(&current_input)
                            }
                            PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                                self.replace_history.navigate_prev(&current_input)
                            }
                            _ => None,
                        };

                        // Update prompt input if history item exists
                        if let Some(history_text) = history_item {
                            if let Some(prompt) = self.prompt_mut() {
                                prompt.set_input(history_text.clone());

                                // For search prompts, update highlights incrementally
                                if matches!(
                                    prompt_type,
                                    PromptType::Search
                                        | PromptType::ReplaceSearch
                                        | PromptType::QueryReplaceSearch
                                ) {
                                    self.update_search_highlights(&history_text);
                                }
                            }
                        }
                    }
                }
            }
            Action::PromptSelectNext => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        // Suggestions exist: navigate suggestions
                        if let Some(selected) = prompt.selected_suggestion {
                            // Don't wrap around - stay at the end if already at the last item
                            let new_pos = selected + 1;
                            prompt.selected_suggestion =
                                Some(new_pos.min(prompt.suggestions.len() - 1));
                        }
                    } else {
                        // No suggestions: navigate history (Down arrow)
                        let prompt_type = prompt.prompt_type.clone();

                        // Get the appropriate history based on prompt type
                        let history_item = match prompt_type {
                            PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch => self.search_history.navigate_next(),
                            PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                                self.replace_history.navigate_next()
                            }
                            _ => None,
                        };

                        // Update prompt input if history item exists
                        if let Some(history_text) = history_item {
                            if let Some(prompt) = self.prompt_mut() {
                                prompt.set_input(history_text.clone());

                                // For search prompts, update highlights incrementally
                                if matches!(
                                    prompt_type,
                                    PromptType::Search
                                        | PromptType::ReplaceSearch
                                        | PromptType::QueryReplaceSearch
                                ) {
                                    self.update_search_highlights(&history_text);
                                }
                            }
                        }
                    }
                }
            }
            Action::PromptPageUp => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        if let Some(selected) = prompt.selected_suggestion {
                            // Move up by 10, but stop at 0 instead of wrapping
                            prompt.selected_suggestion = Some(selected.saturating_sub(10));
                        }
                    }
                }
            }
            Action::PromptPageDown => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        if let Some(selected) = prompt.selected_suggestion {
                            // Move down by 10, but stop at the end instead of wrapping
                            let len = prompt.suggestions.len();
                            let new_pos = selected + 10;
                            prompt.selected_suggestion = Some(new_pos.min(len - 1));
                        }
                    }
                }
            }
            Action::PromptAcceptSuggestion => {
                if let Some(prompt) = self.prompt_mut() {
                    if let Some(selected) = prompt.selected_suggestion {
                        if let Some(suggestion) = prompt.suggestions.get(selected) {
                            // Don't accept disabled suggestions (greyed out commands)
                            if !suggestion.disabled {
                                prompt.input = suggestion.get_value().to_string();
                                prompt.cursor_pos = prompt.input.len();
                            }
                        }
                    }
                }
            }
            Action::PromptMoveWordLeft => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_word_left();
                }
            }
            Action::PromptMoveWordRight => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_word_right();
                }
            }
            // Advanced prompt editing actions
            Action::PromptDeleteWordForward => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.delete_word_forward();
                }
                self.update_prompt_suggestions();
            }
            Action::PromptDeleteWordBackward => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.delete_word_backward();
                }
                self.update_prompt_suggestions();
            }
            Action::PromptCopy => {
                if let Some(prompt) = &self.prompt {
                    // If there's a selection, copy selected text; otherwise copy entire input
                    self.clipboard = if let Some(selected) = prompt.selected_text() {
                        selected
                    } else {
                        prompt.get_text()
                    };
                    self.set_status_message("Copied".to_string());
                }
            }
            Action::PromptCut => {
                // Get text first (selected or entire input)
                let text = if let Some(prompt) = &self.prompt {
                    if let Some(selected) = prompt.selected_text() {
                        selected
                    } else {
                        prompt.get_text()
                    }
                } else {
                    String::new()
                };
                // Update clipboard before taking mutable borrow
                self.clipboard = text;
                // Now cut the text (delete selection or clear entire input)
                if let Some(prompt) = self.prompt_mut() {
                    if prompt.has_selection() {
                        prompt.delete_selection();
                    } else {
                        prompt.clear();
                    }
                }
                self.set_status_message("Cut".to_string());
                self.update_prompt_suggestions();
            }
            Action::PromptPaste => {
                let text = self.clipboard.clone();
                if let Some(prompt) = self.prompt_mut() {
                    prompt.insert_str(&text);
                }
                self.update_prompt_suggestions();
            }
            // Prompt selection actions
            Action::PromptMoveLeftSelecting => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_left_selecting();
                }
            }
            Action::PromptMoveRightSelecting => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_right_selecting();
                }
            }
            Action::PromptMoveHomeSelecting => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_home_selecting();
                }
            }
            Action::PromptMoveEndSelecting => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_end_selecting();
                }
            }
            Action::PromptSelectWordLeft => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_word_left_selecting();
                }
            }
            Action::PromptSelectWordRight => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_word_right_selecting();
                }
            }
            Action::PromptSelectAll => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.selection_anchor = Some(0);
                    prompt.cursor_pos = prompt.input.len();
                }
            }

            // Popup mode actions
            Action::PopupSelectNext => {
                self.popup_select_next();
            }
            Action::PopupSelectPrev => {
                self.popup_select_prev();
            }
            Action::PopupPageUp => {
                self.popup_page_up();
            }
            Action::PopupPageDown => {
                self.popup_page_down();
            }
            Action::PopupConfirm => {
                return self.handle_action(action);
            }
            Action::PopupCancel => {
                return self.handle_action(action);
            }

            // Normal mode actions - delegate to handle_action
            _ => {
                return self.handle_action(action);
            }
        }

        Ok(())
    }

    /// Handle an action (for normal mode and command execution)
    fn handle_action(&mut self, action: Action) -> std::io::Result<()> {
        use crate::keybindings::Action;

        // Record action to macro if recording
        self.record_macro_action(&action);

        match action {
            Action::Quit => self.quit(),
            Action::Save => self.save()?,
            Action::Open => self.start_prompt("Find file: ".to_string(), PromptType::OpenFile),
            Action::GotoLine => {
                self.start_prompt("Go to line: ".to_string(), PromptType::GotoLine)
            }
            Action::New => {
                self.new_buffer();
            }
            Action::Close => {
                let buffer_id = self.active_buffer;
                if let Err(e) = self.close_buffer(buffer_id) {
                    self.set_status_message(format!("Cannot close buffer: {}", e));
                } else {
                    self.set_status_message("Buffer closed".to_string());
                }
            }
            Action::Copy => self.copy_selection(),
            Action::Cut => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                self.cut_selection()
            }
            Action::Paste => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                self.paste()
            }
            Action::Undo => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                let events = self.active_event_log_mut().undo();
                // Apply all inverse events collected during undo
                for event in events {
                    self.apply_event_to_active_buffer(&event);
                }
            }
            Action::Redo => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                let events = self.active_event_log_mut().redo();
                // Apply all events collected during redo
                for event in events {
                    self.apply_event_to_active_buffer(&event);
                }
            }
            Action::ShowHelp => self.help_renderer.toggle(),
            Action::CommandPalette => {
                // Toggle command palette: close if already open, otherwise open it
                if let Some(prompt) = &self.prompt {
                    if prompt.prompt_type == PromptType::Command {
                        self.cancel_prompt();
                        return Ok(());
                    }
                }

                // Use the current context for filtering commands
                let suggestions = self.command_registry.read().unwrap().filter(
                    "",
                    self.key_context,
                    &self.keybindings,
                );
                self.start_prompt_with_suggestions(
                    "Command: ".to_string(),
                    PromptType::Command,
                    suggestions,
                );
            }
            Action::ToggleLineWrap => {
                self.config.editor.line_wrap = !self.config.editor.line_wrap;

                // Update all viewports to reflect the new line wrap setting
                for state in self.buffers.values_mut() {
                    state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                }

                let state = if self.config.editor.line_wrap {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(format!("Line wrap {}", state));
            }
            Action::LspCompletion => {
                self.request_completion()?;
            }
            Action::LspGotoDefinition => {
                self.request_goto_definition()?;
            }
            Action::LspRename => {
                self.start_rename()?;
            }
            Action::LspHover => {
                self.request_hover()?;
            }
            Action::LspReferences => {
                self.request_references()?;
            }
            Action::LspSignatureHelp => {
                self.request_signature_help()?;
            }
            Action::LspCodeActions => {
                self.request_code_actions()?;
            }
            Action::LspRestart => {
                // Get the language for the current buffer
                if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer) {
                    if let Some(path) = metadata.file_path() {
                        if let Some(language) = crate::lsp_manager::detect_language(path) {
                            let restart_result = if let Some(lsp) = self.lsp.as_mut() {
                                Some(lsp.manual_restart(&language))
                            } else {
                                None
                            };

                            if let Some((success, message)) = restart_result {
                                self.status_message = Some(message);
                                if success {
                                    // Re-send didOpen for all buffers of this language
                                    let buffers_for_language: Vec<_> = self
                                        .buffer_metadata
                                        .iter()
                                        .filter_map(|(buf_id, meta)| {
                                            if let Some(p) = meta.file_path() {
                                                if crate::lsp_manager::detect_language(p) == Some(language.clone()) {
                                                    Some((*buf_id, p.clone()))
                                                } else {
                                                    None
                                                }
                                            } else {
                                                None
                                            }
                                        })
                                        .collect();

                                    for (buffer_id, buf_path) in buffers_for_language {
                                        if let Some(state) = self.buffers.get(&buffer_id) {
                                            let content = state.buffer.to_string();
                                            let uri: Option<lsp_types::Uri> = url::Url::from_file_path(&buf_path)
                                                .ok()
                                                .and_then(|u| u.as_str().parse::<lsp_types::Uri>().ok());
                                            if let Some(uri) = uri {
                                                if let Some(lang_id) = crate::lsp_manager::detect_language(&buf_path) {
                                                    if let Some(lsp) = self.lsp.as_mut() {
                                                        if let Some(handle) = lsp.get_or_spawn(&lang_id) {
                                                            let _ = handle.did_open(uri, content, lang_id);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                self.status_message = Some("No LSP manager available".to_string());
                            }
                        } else {
                            self.status_message = Some("No LSP server configured for this file type".to_string());
                        }
                    } else {
                        self.status_message = Some("Current buffer has no associated file".to_string());
                    }
                }
            }
            Action::Search => {
                // Start search prompt
                self.start_prompt("Search: ".to_string(), PromptType::Search);
            }
            Action::Replace => {
                // Always prompt for search query first (with incremental highlighting)
                // TODO: Implement search history - pre-fill with previous search and allow up/down arrows
                self.start_prompt("Replace: ".to_string(), PromptType::ReplaceSearch);
            }
            Action::QueryReplace => {
                // Always prompt for search query first (with incremental highlighting)
                // TODO: Implement search history - pre-fill with previous search and allow up/down arrows
                self.start_prompt(
                    "Query replace: ".to_string(),
                    PromptType::QueryReplaceSearch,
                );
            }
            Action::FindNext => {
                self.find_next();
            }
            Action::FindPrevious => {
                self.find_previous();
            }
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::NextBuffer => self.next_buffer(),
            Action::PrevBuffer => self.prev_buffer(),
            Action::NavigateBack => self.navigate_back(),
            Action::NavigateForward => self.navigate_forward(),
            Action::SplitHorizontal => self.split_pane_horizontal(),
            Action::SplitVertical => self.split_pane_vertical(),
            Action::CloseSplit => self.close_active_split(),
            Action::NextSplit => self.next_split(),
            Action::PrevSplit => self.prev_split(),
            Action::IncreaseSplitSize => self.adjust_split_size(0.05),
            Action::DecreaseSplitSize => self.adjust_split_size(-0.05),
            Action::ToggleFileExplorer => self.toggle_file_explorer(),
            Action::FocusFileExplorer => self.focus_file_explorer(),
            Action::FocusEditor => self.focus_editor(),
            Action::FileExplorerUp => self.file_explorer_navigate_up(),
            Action::FileExplorerDown => self.file_explorer_navigate_down(),
            Action::FileExplorerPageUp => self.file_explorer_page_up(),
            Action::FileExplorerPageDown => self.file_explorer_page_down(),
            Action::FileExplorerExpand => self.file_explorer_toggle_expand(),
            Action::FileExplorerCollapse => self.file_explorer_toggle_expand(), // Same as expand
            Action::FileExplorerOpen => self.file_explorer_open_file()?,
            Action::FileExplorerRefresh => self.file_explorer_refresh(),
            Action::FileExplorerNewFile => self.file_explorer_new_file(),
            Action::FileExplorerNewDirectory => self.file_explorer_new_directory(),
            Action::FileExplorerDelete => self.file_explorer_delete(),
            Action::FileExplorerRename => self.file_explorer_rename(),
            Action::FileExplorerToggleHidden => self.file_explorer_toggle_hidden(),
            Action::FileExplorerToggleGitignored => self.file_explorer_toggle_gitignored(),
            Action::RemoveSecondaryCursors => {
                // Convert action to events and apply them
                if let Some(events) = self.action_to_events(Action::RemoveSecondaryCursors) {
                    // Wrap in batch for atomic undo
                    let batch = Event::Batch {
                        events: events.clone(),
                        description: "Remove secondary cursors".to_string(),
                    };
                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);

                    // Ensure the primary cursor is visible after removing secondary cursors
                    let state = self.active_state_mut();
                    let primary = *state.cursors.primary();
                    state.viewport.ensure_visible(&mut state.buffer, &primary);
                }
            }

            // Menu navigation actions
            Action::MenuActivate => {
                // Open the first menu
                self.menu_state.open_menu(0);
            }
            Action::MenuClose => {
                self.menu_state.close_menu();
            }
            Action::MenuLeft => {
                let total_menus = self.config.menu.menus.len() + self.menu_state.plugin_menus.len();
                self.menu_state.prev_menu(total_menus);
            }
            Action::MenuRight => {
                let total_menus = self.config.menu.menus.len() + self.menu_state.plugin_menus.len();
                self.menu_state.next_menu(total_menus);
            }
            Action::MenuUp => {
                if let Some(active_idx) = self.menu_state.active_menu {
                    let all_menus: Vec<crate::config::Menu> = self.config.menu.menus
                        .iter()
                        .chain(self.menu_state.plugin_menus.iter())
                        .cloned()
                        .collect();
                    if let Some(menu) = all_menus.get(active_idx) {
                        self.menu_state.prev_item(menu);
                    }
                }
            }
            Action::MenuDown => {
                if let Some(active_idx) = self.menu_state.active_menu {
                    let all_menus: Vec<crate::config::Menu> = self.config.menu.menus
                        .iter()
                        .chain(self.menu_state.plugin_menus.iter())
                        .cloned()
                        .collect();
                    if let Some(menu) = all_menus.get(active_idx) {
                        self.menu_state.next_item(menu);
                    }
                }
            }
            Action::MenuExecute => {
                // Execute the highlighted menu item's action
                let all_menus: Vec<crate::config::Menu> = self.config.menu.menus
                    .iter()
                    .chain(self.menu_state.plugin_menus.iter())
                    .cloned()
                    .collect();

                if let Some((action_name, args)) = self.menu_state.get_highlighted_action(&all_menus) {
                    // Close the menu
                    self.menu_state.close_menu();

                    // Parse and execute the action
                    // First try built-in actions, then fall back to plugin actions
                    if let Some(action) = Action::from_str(&action_name, &args) {
                        return self.handle_action(action);
                    } else {
                        // Treat as a plugin action (global Lua function)
                        return self.handle_action(Action::PluginAction(action_name));
                    }
                }
            }
            Action::MenuOpen(menu_name) => {
                // Find the menu by name and open it
                let all_menus: Vec<crate::config::Menu> = self.config.menu.menus
                    .iter()
                    .chain(self.menu_state.plugin_menus.iter())
                    .cloned()
                    .collect();

                for (idx, menu) in all_menus.iter().enumerate() {
                    if menu.label.eq_ignore_ascii_case(&menu_name) {
                        self.menu_state.open_menu(idx);
                        break;
                    }
                }
            }

            Action::SmartHome => {
                self.smart_home();
            }
            Action::IndentSelection => {
                self.indent_selection();
            }
            Action::DedentSelection => {
                self.dedent_selection();
            }
            Action::ToggleComment => {
                self.toggle_comment();
            }
            Action::GoToMatchingBracket => {
                self.goto_matching_bracket();
            }
            Action::JumpToNextError => {
                self.jump_to_next_error();
            }
            Action::JumpToPreviousError => {
                self.jump_to_previous_error();
            }
            Action::SetBookmark(key) => {
                self.set_bookmark(key);
            }
            Action::JumpToBookmark(key) => {
                self.jump_to_bookmark(key);
            }
            Action::ClearBookmark(key) => {
                self.clear_bookmark(key);
            }
            Action::ListBookmarks => {
                self.list_bookmarks();
            }
            Action::ToggleSearchCaseSensitive => {
                self.search_case_sensitive = !self.search_case_sensitive;
                let state = if self.search_case_sensitive {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(format!("Case-sensitive search {}", state));
                // Re-run search if active
                if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::ToggleSearchWholeWord => {
                self.search_whole_word = !self.search_whole_word;
                let state = if self.search_whole_word {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(format!("Whole word search {}", state));
                // Re-run search if active
                if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::StartMacroRecording => {
                // This is a no-op; use ToggleMacroRecording instead
                self.set_status_message(
                    "Use Ctrl+Shift+R to start recording (will prompt for register)".to_string(),
                );
            }
            Action::StopMacroRecording => {
                self.stop_macro_recording();
            }
            Action::PlayMacro(key) => {
                self.play_macro(key);
            }
            Action::ToggleMacroRecording(key) => {
                self.toggle_macro_recording(key);
            }
            Action::ShowMacro(key) => {
                self.show_macro_in_buffer(key);
            }
            Action::ListMacros => {
                self.list_macros_in_buffer();
            }
            Action::None => {}
            Action::DeleteBackward => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                // Normal backspace handling
                if let Some(events) = self.action_to_events(Action::DeleteBackward) {
                    if events.len() > 1 {
                        let batch = Event::Batch {
                            events: events.clone(),
                            description: "Delete backward".to_string(),
                        };
                        self.active_event_log_mut().append(batch.clone());
                        self.apply_event_to_active_buffer(&batch);
                        // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                    } else {
                        for event in events {
                            self.active_event_log_mut().append(event.clone());
                            self.apply_event_to_active_buffer(&event);
                            // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                        }
                    }
                }
            }
            Action::PluginAction(action_name) => {
                // Execute the plugin callback via TypeScript plugin thread
                // Use non-blocking version to avoid deadlock with async plugin ops
                if let Some(ref manager) = self.ts_plugin_manager {
                    match manager.execute_action_async(&action_name) {
                        Ok(receiver) => {
                            // Store pending action for processing in main loop
                            self.pending_plugin_actions.push((action_name.clone(), receiver));
                        }
                        Err(e) => {
                            self.set_status_message(format!("Plugin error: {}", e));
                            tracing::error!("Plugin action error: {}", e);
                        }
                    }
                } else {
                    self.set_status_message("Plugin manager not available".to_string());
                }
            }
            Action::PromptConfirm => {
                // Handle prompt confirmation (same logic as in handle_key)
                if let Some((input, prompt_type, selected_index)) = self.confirm_prompt() {
                    use std::path::Path;
                    match prompt_type {
                        PromptType::OpenFile => {
                            let path = Path::new(&input);
                            if let Err(e) = self.open_file(path) {
                                self.set_status_message(format!("Error opening file: {e}"));
                            } else {
                                self.set_status_message(format!("Opened: {input}"));
                            }
                        }
                        PromptType::SaveFileAs => {
                            self.set_status_message(format!(
                                "Save-as not yet implemented: {input}"
                            ));
                        }
                        PromptType::Search => {
                            self.perform_search(&input);
                        }
                        PromptType::ReplaceSearch => {
                            self.perform_search(&input);
                            self.start_prompt(
                                format!("Replace '{}' with: ", input),
                                PromptType::Replace {
                                    search: input.clone(),
                                },
                            );
                        }
                        PromptType::Replace { search } => {
                            self.perform_replace(&search, &input);
                        }
                        PromptType::QueryReplaceSearch => {
                            self.perform_search(&input);
                            self.start_prompt(
                                format!("Query replace '{}' with: ", input),
                                PromptType::QueryReplace {
                                    search: input.clone(),
                                },
                            );
                        }
                        PromptType::QueryReplace { search } => {
                            self.start_interactive_replace(&search, &input);
                        }
                        PromptType::Command => {
                            let commands = self.command_registry.read().unwrap().get_all();
                            if let Some(cmd) = commands.iter().find(|c| c.name == input) {
                                let action = cmd.action.clone();
                                self.set_status_message(format!("Executing: {}", cmd.name));
                                return self.handle_action(action);
                            } else {
                                self.set_status_message(format!("Unknown command: {input}"));
                            }
                        }
                        PromptType::GotoLine => {
                            match input.trim().parse::<usize>() {
                                Ok(line_num) if line_num > 0 => {
                                    let target_line = line_num.saturating_sub(1);
                                    let buffer_id = self.active_buffer;
                                    if let Some(state) = self.buffers.get(&buffer_id) {
                                        let max_line = state.buffer.line_count().unwrap_or(1).saturating_sub(1);
                                        let actual_line = target_line.min(max_line);
                                        let position = state.buffer.line_col_to_position(actual_line, 0);
                                        let cursor_id = state.cursors.primary_id();
                                        let old_position = state.cursors.primary().position;
                                        let old_anchor = state.cursors.primary().anchor;
                                        let old_sticky_column = state.cursors.primary().sticky_column;
                                        let event = crate::event::Event::MoveCursor {
                                            cursor_id,
                                            old_position,
                                            new_position: position,
                                            old_anchor,
                                            new_anchor: None,
                                            old_sticky_column,
                                            new_sticky_column: 0,
                                        };
                                        if let Some(state) = self.buffers.get_mut(&buffer_id) {
                                            state.apply(&event);
                                        }
                                        if target_line > max_line {
                                            self.set_status_message(format!(
                                                "Line {} doesn't exist, jumped to line {}",
                                                line_num,
                                                actual_line + 1
                                            ));
                                        } else {
                                            self.set_status_message(format!("Jumped to line {}", line_num));
                                        }
                                    }
                                }
                                Ok(_) => {
                                    self.set_status_message("Line number must be positive".to_string());
                                }
                                Err(_) => {
                                    self.set_status_message(format!("Invalid line number: {}", input));
                                }
                            }
                        }
                        PromptType::Plugin { custom_type } => {
                            use crate::hooks::HookArgs;
                            let hook_args = HookArgs::PromptConfirmed {
                                prompt_type: custom_type,
                                input,
                                selected_index,
                            };

                            if let Some(ref ts_manager) = self.ts_plugin_manager {
                                ts_manager.run_hook("prompt_confirmed", hook_args);
                            }
                        }
                        PromptType::LspRename {
                            original_text,
                            start_pos,
                            end_pos: _,
                            overlay_id,
                        } => {
                            // Perform LSP rename with the new name from the prompt input
                            self.perform_lsp_rename(input, original_text, start_pos, overlay_id);
                        }
                    }
                }
            }
            Action::PopupConfirm => {
                // If it's a completion popup, insert the selected item
                let completion_text = if let Some(popup) = self.active_state().popups.top() {
                    if let Some(title) = &popup.title {
                        if title == "Completion" {
                            if let Some(item) = popup.selected_item() {
                                item.data.clone()
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                // Now perform the completion if we have text
                if let Some(text) = completion_text {
                    use crate::word_navigation::find_completion_word_start;

                    let (cursor_id, cursor_pos, word_start) = {
                        let state = self.active_state();
                        let cursor_id = state.cursors.primary_id();
                        let cursor_pos = state.cursors.primary().position;
                        let word_start = find_completion_word_start(&state.buffer, cursor_pos);
                        (cursor_id, cursor_pos, word_start)
                    };

                    let deleted_text = if word_start < cursor_pos {
                        self.active_state_mut()
                            .get_text_range(word_start, cursor_pos)
                    } else {
                        String::new()
                    };

                    if word_start < cursor_pos {
                        let delete_event = crate::event::Event::Delete {
                            range: word_start..cursor_pos,
                            deleted_text,
                            cursor_id,
                        };

                        self.active_event_log_mut().append(delete_event.clone());
                        self.apply_event_to_active_buffer(&delete_event);

                        let buffer_len = self.active_state().buffer.len();
                        let insert_pos = word_start.min(buffer_len);

                        let insert_event = crate::event::Event::Insert {
                            position: insert_pos,
                            text,
                            cursor_id,
                        };

                        self.active_event_log_mut().append(insert_event.clone());
                        self.apply_event_to_active_buffer(&insert_event);
                    } else {
                        let insert_event = crate::event::Event::Insert {
                            position: cursor_pos,
                            text,
                            cursor_id,
                        };

                        self.active_event_log_mut().append(insert_event.clone());
                        self.apply_event_to_active_buffer(&insert_event);
                    }
                }

                self.hide_popup();
            }
            Action::PopupCancel => {
                self.hide_popup();
            }
            Action::InsertChar(c) => {
                // Handle character insertion in interactive replace mode
                if self.interactive_replace_state.is_some() {
                    return self.handle_interactive_replace_key(c);
                // Handle character insertion in prompt mode
                } else if self.is_prompting() {
                    if let Some(prompt) = self.prompt_mut() {
                        // Use insert_str to properly handle selection deletion
                        let s = c.to_string();
                        prompt.insert_str(&s);
                    }
                    self.update_prompt_suggestions();
                } else {
                    // Check if editing is disabled (show_cursors = false)
                    if self.is_editing_disabled() {
                        self.set_status_message("Editing disabled in this buffer".to_string());
                        return Ok(());
                    }
                    // Normal mode character insertion
                    // Cancel any pending LSP requests since the text is changing
                    self.cancel_pending_lsp_requests();

                    if let Some(events) = self.action_to_events(Action::InsertChar(c)) {
                        // Wrap multiple events (multi-cursor) in a Batch for atomic undo
                        if events.len() > 1 {
                            let batch = Event::Batch {
                                events: events.clone(),
                                description: format!("Insert '{}'", c),
                            };
                            self.active_event_log_mut().append(batch.clone());
                            self.apply_event_to_active_buffer(&batch);
                            // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                        } else {
                            // Single cursor - no need for batch
                            for event in events {
                                self.active_event_log_mut().append(event.clone());
                                self.apply_event_to_active_buffer(&event);
                                // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                            }
                        }
                    }

                    // Auto-trigger signature help on '(' and ','
                    if c == '(' || c == ',' {
                        let _ = self.request_signature_help();
                    }
                }
            }
            _ => {
                // Convert action to events and apply them
                // Get description before moving action
                let action_description = format!("{:?}", action);

                // Check if this is an editing action and editing is disabled
                let is_editing_action = matches!(
                    action,
                    Action::InsertNewline
                        | Action::InsertTab
                        | Action::DeleteForward
                        | Action::DeleteWordBackward
                        | Action::DeleteWordForward
                        | Action::DeleteLine
                        | Action::IndentSelection
                        | Action::DedentSelection
                        | Action::ToggleComment
                );

                if is_editing_action && self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }

                if let Some(events) = self.action_to_events(action) {
                    // Wrap multiple events (multi-cursor) in a Batch for atomic undo
                    if events.len() > 1 {
                        let batch = Event::Batch {
                            events: events.clone(),
                            description: action_description,
                        };
                        self.active_event_log_mut().append(batch.clone());
                        self.apply_event_to_active_buffer(&batch);
                        // Note: LSP notifications now handled automatically by apply_event_to_active_buffer

                        // Track position history for all events in the batch
                        for event in &events {
                            // Track cursor movements in position history (but not during navigation)
                            if !self.in_navigation {
                                if let Event::MoveCursor {
                                    new_position,
                                    new_anchor,
                                    ..
                                } = event
                                {
                                    self.position_history.record_movement(
                                        self.active_buffer,
                                        *new_position,
                                        *new_anchor,
                                    );
                                }
                            }
                        }
                    } else {
                        // Single cursor - no need for batch
                        for event in events {
                            self.active_event_log_mut().append(event.clone());
                            self.apply_event_to_active_buffer(&event);
                            // Note: LSP notifications now handled automatically by apply_event_to_active_buffer

                            // Track cursor movements in position history (but not during navigation)
                            if !self.in_navigation {
                                if let Event::MoveCursor {
                                    new_position,
                                    new_anchor,
                                    ..
                                } = event
                                {
                                    self.position_history.record_movement(
                                        self.active_buffer,
                                        new_position,
                                        new_anchor,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Handle a mouse event
    pub fn handle_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> std::io::Result<()> {
        use crossterm::event::{MouseButton, MouseEventKind};

        // Cancel LSP rename prompt on any mouse interaction
        if let Some(ref prompt) = self.prompt {
            if matches!(prompt.prompt_type, PromptType::LspRename { .. }) {
                self.cancel_prompt();
            }
        }

        let col = mouse_event.column;
        let row = mouse_event.row;

        tracing::debug!(
            "handle_mouse: kind={:?}, col={}, row={}",
            mouse_event.kind,
            col,
            row
        );

        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                self.handle_mouse_click(col, row)?;
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                self.handle_mouse_drag(col, row)?;
            }
            MouseEventKind::Up(MouseButton::Left) => {
                // Stop dragging and clear drag state
                self.mouse_state.dragging_scrollbar = None;
                self.mouse_state.drag_start_row = None;
                self.mouse_state.drag_start_top_byte = None;
                self.mouse_state.dragging_separator = None;
                self.mouse_state.drag_start_position = None;
                self.mouse_state.drag_start_ratio = None;
            }
            MouseEventKind::Moved => {
                self.update_hover_target(col, row);
            }
            MouseEventKind::ScrollUp => {
                // Dismiss hover/signature help popups on scroll
                self.dismiss_transient_popups();
                self.handle_mouse_scroll(col, row, -3)?;
            }
            MouseEventKind::ScrollDown => {
                // Dismiss hover/signature help popups on scroll
                self.dismiss_transient_popups();
                self.handle_mouse_scroll(col, row, 3)?;
            }
            _ => {
                // Ignore other mouse events for now
            }
        }

        self.mouse_state.last_position = Some((col, row));
        Ok(())
    }

    /// Update the current hover target based on mouse position
    fn update_hover_target(&mut self, col: u16, row: u16) {
        // Check suggestions area first (command palette, autocomplete)
        if let Some((inner_rect, start_idx, _visible_count, total_count)) =
            &self.cached_layout.suggestions_area
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
            {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = start_idx + relative_row;

                if item_idx < *total_count {
                    self.mouse_state.hover_target = Some(HoverTarget::SuggestionItem(item_idx));
                    return;
                }
            }
        }

        // Check popups (they're rendered on top)
        // Check from top to bottom (reverse order since last popup is on top)
        for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items) in
            self.cached_layout.popup_areas.iter().rev()
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
                && *num_items > 0
            {
                // Calculate which item is being hovered
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;

                if item_idx < *num_items {
                    self.mouse_state.hover_target =
                        Some(HoverTarget::PopupListItem(*popup_idx, item_idx));
                    return;
                }
            }
        }

        // Check menu bar (row 0)
        if row == 0 {
            let all_menus: Vec<crate::config::Menu> = self.config.menu.menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu_idx) = self.menu_state.get_menu_at_position(&all_menus, col) {
                self.mouse_state.hover_target = Some(HoverTarget::MenuBarItem(menu_idx));
                return;
            }
        }

        // Check menu dropdown items if a menu is open
        if let Some(active_idx) = self.menu_state.active_menu {
            let all_menus: Vec<crate::config::Menu> = self.config.menu.menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu) = all_menus.get(active_idx) {
                if let Some(item_idx) = self.menu_state.get_item_at_position(menu, row) {
                    self.mouse_state.hover_target = Some(HoverTarget::MenuDropdownItem(active_idx, item_idx));
                    return;
                }
            }
        }

        // Check split separators
        for (split_id, direction, sep_x, sep_y, sep_length) in &self.cached_layout.separator_areas {
            let is_on_separator = match direction {
                SplitDirection::Horizontal => {
                    row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                }
                SplitDirection::Vertical => {
                    col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                }
            };

            if is_on_separator {
                self.mouse_state.hover_target = Some(HoverTarget::SplitSeparator(*split_id, *direction));
                return;
            }
        }

        // Check scrollbars
        for (split_id, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
            &self.cached_layout.split_areas
        {
            if col >= scrollbar_rect.x
                && col < scrollbar_rect.x + scrollbar_rect.width
                && row >= scrollbar_rect.y
                && row < scrollbar_rect.y + scrollbar_rect.height
            {
                let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;

                if is_on_thumb {
                    self.mouse_state.hover_target = Some(HoverTarget::ScrollbarThumb(*split_id));
                } else {
                    self.mouse_state.hover_target = Some(HoverTarget::ScrollbarTrack(*split_id));
                }
                return;
            }
        }

        // No hover target
        self.mouse_state.hover_target = None;
    }

    /// Handle mouse click (down event)
    fn handle_mouse_click(&mut self, col: u16, row: u16) -> std::io::Result<()> {
        // Check if click is on suggestions (command palette, autocomplete)
        if let Some((inner_rect, start_idx, _visible_count, total_count)) =
            &self.cached_layout.suggestions_area.clone()
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
            {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = start_idx + relative_row;

                if item_idx < *total_count {
                    // Select and execute the clicked suggestion
                    if let Some(prompt) = &mut self.prompt {
                        prompt.selected_suggestion = Some(item_idx);
                    }
                    // Execute the suggestion (same as pressing Enter)
                    return self.handle_action(Action::PromptConfirm);
                }
            }
        }

        // Check if click is on a popup (they're rendered on top)
        for (_popup_idx, _popup_rect, inner_rect, scroll_offset, num_items) in
            self.cached_layout.popup_areas.iter().rev()
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
                && *num_items > 0
            {
                // Calculate which item was clicked
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;

                if item_idx < *num_items {
                    // Select and execute the clicked item
                    let state = self.active_state_mut();
                    if let Some(popup) = state.popups.top_mut() {
                        if let crate::popup::PopupContent::List { items: _, selected } =
                            &mut popup.content
                        {
                            *selected = item_idx;
                        }
                    }
                    // Execute the popup selection (same as pressing Enter)
                    return self.handle_action(Action::PopupConfirm);
                }
            }
        }

        // Check if click is on menu bar (row 0)
        if row == 0 {
            let all_menus: Vec<crate::config::Menu> = self.config.menu.menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu_idx) = self.menu_state.get_menu_at_position(&all_menus, col) {
                // Toggle menu: if same menu is open, close it; otherwise open clicked menu
                if self.menu_state.active_menu == Some(menu_idx) {
                    self.menu_state.close_menu();
                } else {
                    self.menu_state.open_menu(menu_idx);
                }
            } else {
                // Clicked on menu bar but not on a menu label - close any open menu
                self.menu_state.close_menu();
            }
            return Ok(());
        }

        // Check if click is on an open menu dropdown
        if let Some(active_idx) = self.menu_state.active_menu {
            let all_menus: Vec<crate::config::Menu> = self.config.menu.menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu) = all_menus.get(active_idx) {
                // Calculate menu dropdown bounds
                // Menu position: sum of widths of all menus before this one
                let mut menu_x = 0u16;
                for m in all_menus.iter().take(active_idx) {
                    menu_x += m.label.len() as u16 + 3; // " Label " + trailing space
                }

                // Find the widest item to determine dropdown width
                let max_label_len = menu.items.iter().map(|item| match item {
                    crate::config::MenuItem::Action { label, .. } => label.len(),
                    crate::config::MenuItem::Separator { .. } => 0,
                    crate::config::MenuItem::Submenu { label, .. } => label.len(),
                }).max().unwrap_or(0);
                let dropdown_width = max_label_len + 30; // Label + padding + keybinding space

                // Dropdown starts at row 1 (below menu bar), with border at row 1
                // Items start at row 2, and there's a border at the bottom
                let dropdown_height = menu.items.len() as u16 + 2; // items + top/bottom border

                // Check if click is inside dropdown bounds
                if col >= menu_x && col < menu_x + dropdown_width as u16
                    && row >= 1 && row < 1 + dropdown_height
                {
                    // Check if click is on an item (not border)
                    if let Some(item_idx) = self.menu_state.get_item_at_position(menu, row) {
                        // Execute the menu item action
                        if let Some(crate::config::MenuItem::Action { action, args, .. }) = menu.items.get(item_idx) {
                            let action_name = action.clone();
                            let action_args = args.clone();

                            // Close the menu first
                            self.menu_state.close_menu();

                            // Parse and execute the action
                            if let Some(action) = Action::from_str(&action_name, &action_args) {
                                return self.handle_action(action);
                            }
                        }
                    }
                    return Ok(());
                }
            }

            // Click outside the dropdown - close the menu
            self.menu_state.close_menu();
            return Ok(());
        }

        // Check if click is on file explorer
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                self.handle_file_explorer_click(col, row, explorer_area)?;
                return Ok(());
            }
        }

        // Check if click is on a scrollbar
        for (split_id, buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
            &self.cached_layout.split_areas
        {
            if col >= scrollbar_rect.x
                && col < scrollbar_rect.x + scrollbar_rect.width
                && row >= scrollbar_rect.y
                && row < scrollbar_rect.y + scrollbar_rect.height
            {
                // Calculate relative row within scrollbar
                let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;

                // Check if click is on thumb or track
                let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;

                if is_on_thumb {
                    // Click on thumb - start drag from current position (don't jump)
                    self.mouse_state.dragging_scrollbar = Some(*split_id);
                    self.mouse_state.drag_start_row = Some(row);
                    // Record the current viewport position
                    if let Some(state) = self.buffers.get(buffer_id) {
                        self.mouse_state.drag_start_top_byte = Some(state.viewport.top_byte);
                    }
                } else {
                    // Click on track - jump to position
                    self.mouse_state.dragging_scrollbar = Some(*split_id);
                    self.handle_scrollbar_jump(col, row, *buffer_id, *scrollbar_rect)?;
                }
                return Ok(());
            }
        }

        // Check if click is on a split separator (for drag resizing)
        for (split_id, direction, sep_x, sep_y, sep_length) in &self.cached_layout.separator_areas {
            let is_on_separator = match direction {
                SplitDirection::Horizontal => {
                    // Horizontal separator: spans full width at a specific y
                    row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                }
                SplitDirection::Vertical => {
                    // Vertical separator: spans full height at a specific x
                    col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                }
            };

            if is_on_separator {
                // Start separator drag
                self.mouse_state.dragging_separator = Some((*split_id, *direction));
                self.mouse_state.drag_start_position = Some((col, row));
                // Store the initial ratio
                if let Some(ratio) = self.split_manager.get_ratio(*split_id) {
                    self.mouse_state.drag_start_ratio = Some(ratio);
                }
                return Ok(());
            }
        }

        // Check if click is in editor content area
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &self.cached_layout.split_areas
        {
            if col >= content_rect.x
                && col < content_rect.x + content_rect.width
                && row >= content_rect.y
                && row < content_rect.y + content_rect.height
            {
                // Click in editor - focus split and position cursor
                self.handle_editor_click(col, row, *split_id, *buffer_id, *content_rect)?;
                return Ok(());
            }
        }

        Ok(())
    }

    /// Handle mouse drag event
    fn handle_mouse_drag(&mut self, col: u16, row: u16) -> std::io::Result<()> {
        // If dragging scrollbar, update scroll position
        if let Some(dragging_split_id) = self.mouse_state.dragging_scrollbar {
            // Find the buffer and scrollbar rect for this split
            for (split_id, buffer_id, _content_rect, scrollbar_rect, _thumb_start, _thumb_end) in
                &self.cached_layout.split_areas
            {
                if *split_id == dragging_split_id {
                    // Check if we started dragging from the thumb (have drag_start_row)
                    if self.mouse_state.drag_start_row.is_some() {
                        // Relative drag from thumb
                        self.handle_scrollbar_drag_relative(row, *buffer_id, *scrollbar_rect)?;
                    } else {
                        // Jump drag (started from track)
                        self.handle_scrollbar_jump(col, row, *buffer_id, *scrollbar_rect)?;
                    }
                    return Ok(());
                }
            }
        }

        // If dragging separator, update split ratio
        if let Some((split_id, direction)) = self.mouse_state.dragging_separator {
            self.handle_separator_drag(col, row, split_id, direction)?;
            return Ok(());
        }

        Ok(())
    }

    /// Handle separator drag for split resizing
    fn handle_separator_drag(
        &mut self,
        col: u16,
        row: u16,
        split_id: SplitId,
        direction: SplitDirection,
    ) -> std::io::Result<()> {
        let Some((start_col, start_row)) = self.mouse_state.drag_start_position else {
            return Ok(());
        };
        let Some(start_ratio) = self.mouse_state.drag_start_ratio else {
            return Ok(());
        };
        let Some(editor_area) = self.cached_layout.editor_content_area else {
            return Ok(());
        };

        // Calculate the delta in screen space
        let (delta, total_size) = match direction {
            SplitDirection::Horizontal => {
                // For horizontal splits, we move the separator up/down (row changes)
                let delta = row as i32 - start_row as i32;
                let total = editor_area.height as i32;
                (delta, total)
            }
            SplitDirection::Vertical => {
                // For vertical splits, we move the separator left/right (col changes)
                let delta = col as i32 - start_col as i32;
                let total = editor_area.width as i32;
                (delta, total)
            }
        };

        // Convert screen delta to ratio delta
        // The ratio represents the fraction of space the first split gets
        if total_size > 0 {
            let ratio_delta = delta as f32 / total_size as f32;
            let new_ratio = (start_ratio + ratio_delta).clamp(0.1, 0.9);

            // Update the split ratio
            let _ = self.split_manager.set_ratio(split_id, new_ratio);
        }

        Ok(())
    }

    /// Handle mouse wheel scroll event
    fn handle_mouse_scroll(&mut self, col: u16, row: u16, delta: i32) -> std::io::Result<()> {
        // Check if scroll is over the file explorer
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                // Scroll the file explorer
                if let Some(explorer) = &mut self.file_explorer {
                    let visible = explorer.tree().get_visible_nodes();
                    if visible.is_empty() {
                        return Ok(());
                    }

                    // Get current selected index
                    let current_index = explorer.get_selected_index().unwrap_or(0);

                    // Calculate new index based on scroll delta
                    let new_index = if delta < 0 {
                        // Scroll up (negative delta)
                        current_index.saturating_sub(delta.abs() as usize)
                    } else {
                        // Scroll down (positive delta)
                        (current_index + delta as usize).min(visible.len() - 1)
                    };

                    // Set the new selection
                    if let Some(node_id) = explorer.get_node_at_index(new_index) {
                        explorer.set_selected(Some(node_id));
                        explorer.update_scroll_for_selection();
                    }
                }
                return Ok(());
            }
        }

        // Otherwise, scroll the editor in the active split
        if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
            // Scroll the viewport by the delta amount
            if delta < 0 {
                // Scroll up
                let lines_to_scroll = delta.abs() as usize;
                state.viewport.scroll_up(&mut state.buffer, lines_to_scroll);
            } else {
                // Scroll down
                let lines_to_scroll = delta as usize;
                state
                    .viewport
                    .scroll_down(&mut state.buffer, lines_to_scroll);
            }
        }

        Ok(())
    }

    /// Handle scrollbar drag with relative movement (when dragging from thumb)
    fn handle_scrollbar_drag_relative(
        &mut self,
        row: u16,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        let drag_start_row = match self.mouse_state.drag_start_row {
            Some(r) => r,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        let drag_start_top_byte = match self.mouse_state.drag_start_top_byte {
            Some(b) => b,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        // Calculate the offset in rows
        let row_offset = (row as i32) - (drag_start_row as i32);

        // Get the buffer state
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let scrollbar_height = scrollbar_rect.height as usize;
            if scrollbar_height == 0 {
                return Ok(());
            }

            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;
            let viewport_height = state.viewport.height as usize;

            // For small files, use precise line-based calculations
            // For large files, fall back to byte-based estimation
            let new_top_byte = if buffer_len <= large_file_threshold {
                // Small file: use line-based calculation for precision
                // Count total lines
                let total_lines = if buffer_len > 0 {
                    state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                } else {
                    1
                };

                // Calculate max scroll line
                let max_scroll_line = total_lines.saturating_sub(viewport_height);

                if max_scroll_line == 0 {
                    // File fits in viewport, no scrolling
                    0
                } else {
                    // Calculate which line the mouse position corresponds to using linear interpolation
                    // Convert absolute mouse row to relative position within scrollbar
                    let relative_mouse_row = row.saturating_sub(scrollbar_rect.y) as usize;
                    // Divide by (height - 1) to map first row to 0.0 and last row to 1.0
                    let scroll_ratio = if scrollbar_height > 1 {
                        (relative_mouse_row as f64 / (scrollbar_height - 1) as f64).clamp(0.0, 1.0)
                    } else {
                        0.0
                    };

                    // Map scroll ratio to target line
                    let target_line = (scroll_ratio * max_scroll_line as f64).round() as usize;
                    let target_line = target_line.min(max_scroll_line);

                    // Find byte position of target line
                    // We need to iterate 'target_line' times to skip past lines 0..target_line-1,
                    // then one more time to get the position of line 'target_line'
                    let mut iter = state.buffer.line_iterator(0, 80);
                    let mut line_byte = 0;

                    for _ in 0..target_line {
                        if let Some((pos, _content)) = iter.next() {
                            line_byte = pos;
                        } else {
                            break;
                        }
                    }

                    // Get the position of the target line
                    if let Some((pos, _)) = iter.next() {
                        pos
                    } else {
                        line_byte // Reached end of buffer
                    }
                }
            } else {
                // Large file: use byte-based estimation (original logic)
                let bytes_per_pixel = buffer_len as f64 / scrollbar_height as f64;
                let byte_offset = (row_offset as f64 * bytes_per_pixel) as i64;

                let new_top_byte = if byte_offset >= 0 {
                    drag_start_top_byte.saturating_add(byte_offset as usize)
                } else {
                    drag_start_top_byte.saturating_sub((-byte_offset) as usize)
                };

                // Clamp to valid range using byte-based max (avoid iterating entire buffer)
                new_top_byte.min(buffer_len.saturating_sub(1))
            };

            // Find the line start for this byte position
            let iter = state.buffer.line_iterator(new_top_byte, 80);
            let line_start = iter.current_position();

            // Set viewport top to this position
            state.viewport.top_byte = line_start;
        }

        // Move cursor to be visible in the new viewport (after releasing the state borrow)
        self.move_cursor_to_visible_area(buffer_id);

        Ok(())
    }

    /// Handle scrollbar jump (clicking on track or absolute positioning)
    fn handle_scrollbar_jump(
        &mut self,
        _col: u16,
        row: u16,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        // Calculate which line to scroll to based on mouse position
        let scrollbar_height = scrollbar_rect.height as usize;
        if scrollbar_height == 0 {
            return Ok(());
        }

        // Get relative position in scrollbar (0.0 to 1.0)
        // Divide by (height - 1) to map first row to 0.0 and last row to 1.0
        let relative_row = row.saturating_sub(scrollbar_rect.y);
        let ratio = if scrollbar_height > 1 {
            ((relative_row as f64) / ((scrollbar_height - 1) as f64)).clamp(0.0, 1.0)
        } else {
            0.0
        };

        // Get the buffer state
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;
            let viewport_height = state.viewport.height as usize;

            // For small files, use precise line-based calculations
            // For large files, fall back to byte-based estimation
            let target_byte = if buffer_len <= large_file_threshold {
                // Small file: use line-based calculation for precision
                let total_lines = if buffer_len > 0 {
                    state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                } else {
                    1
                };

                let max_scroll_line = total_lines.saturating_sub(viewport_height);

                if max_scroll_line == 0 {
                    // File fits in viewport, no scrolling
                    0
                } else {
                    // Map ratio to target line
                    let target_line = (ratio * max_scroll_line as f64).round() as usize;
                    let target_line = target_line.min(max_scroll_line);

                    // Find byte position of target line
                    // We need to iterate 'target_line' times to skip past lines 0..target_line-1,
                    // then one more time to get the position of line 'target_line'
                    let mut iter = state.buffer.line_iterator(0, 80);
                    let mut line_byte = 0;

                    for _ in 0..target_line {
                        if let Some((pos, _content)) = iter.next() {
                            line_byte = pos;
                        } else {
                            break;
                        }
                    }

                    // Get the position of the target line
                    if let Some((pos, _)) = iter.next() {
                        pos
                    } else {
                        line_byte // Reached end of buffer
                    }
                }
            } else {
                // Large file: use byte-based estimation (original logic)
                let target_byte = (buffer_len as f64 * ratio) as usize;
                target_byte.min(buffer_len.saturating_sub(1))
            };

            // Find the line start for this byte position
            let iter = state.buffer.line_iterator(target_byte, 80);
            let line_start = iter.current_position();

            // Apply scroll limiting
            // Use viewport.height (constant allocated rows) not visible_line_count (varies with content)
            // For large files, use byte-based max to avoid iterating entire buffer
            let max_top_byte = if buffer_len <= large_file_threshold {
                Self::calculate_max_scroll_position(&mut state.buffer, viewport_height)
            } else {
                buffer_len.saturating_sub(1)
            };
            let limited_line_start = line_start.min(max_top_byte);

            // Set viewport top to this position
            state.viewport.top_byte = limited_line_start;
        }

        // Move cursor to be visible in the new viewport (after releasing the state borrow)
        self.move_cursor_to_visible_area(buffer_id);

        Ok(())
    }

    /// Move the cursor to a visible position within the current viewport
    /// This is called after scrollbar operations to ensure the cursor is in view
    fn move_cursor_to_visible_area(&mut self, buffer_id: BufferId) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let top_byte = state.viewport.top_byte;
            let viewport_height = state.viewport.height as usize;
            let buffer_len = state.buffer.len();

            // Find the bottom byte of the viewport
            // We iterate through viewport_height lines starting from top_byte
            let mut iter = state.buffer.line_iterator(top_byte, 80);
            let mut bottom_byte = buffer_len;

            // Consume viewport_height lines to find where the visible area ends
            for _ in 0..viewport_height {
                if let Some((pos, line)) = iter.next() {
                    // The bottom of this line is at pos + line.len()
                    bottom_byte = pos + line.len();
                } else {
                    // Reached end of buffer
                    bottom_byte = buffer_len;
                    break;
                }
            }

            // Check if cursor is outside visible range and move it if needed
            let cursor_pos = state.cursors.primary().position;
            if cursor_pos < top_byte || cursor_pos > bottom_byte {
                // Move cursor to the top of the viewport
                let cursor = state.cursors.primary_mut();
                cursor.position = top_byte;
                // Keep the existing sticky_column value so vertical navigation preserves column
            }
        }
    }

    /// Calculate the maximum allowed scroll position
    /// Ensures the last line is always at the bottom unless the buffer is smaller than viewport
    fn calculate_max_scroll_position(
        buffer: &mut crate::text_buffer::Buffer,
        viewport_height: usize,
    ) -> usize {
        if viewport_height == 0 {
            return 0;
        }

        let buffer_len = buffer.len();
        if buffer_len == 0 {
            return 0;
        }

        // Count total lines in buffer
        let mut line_count = 0;
        let mut iter = buffer.line_iterator(0, 80);
        while iter.next().is_some() {
            line_count += 1;
        }

        // If buffer has fewer lines than viewport, can't scroll at all
        if line_count <= viewport_height {
            return 0;
        }

        // Calculate how many lines from the start we can scroll
        // We want to be able to scroll so that the last line is at the bottom
        let scrollable_lines = line_count.saturating_sub(viewport_height);

        // Find the byte position of the line at scrollable_lines offset
        let mut iter = buffer.line_iterator(0, 80);
        let mut current_line = 0;
        let mut max_byte_pos = 0;

        while current_line < scrollable_lines {
            if let Some((pos, _content)) = iter.next() {
                max_byte_pos = pos;
                current_line += 1;
            } else {
                break;
            }
        }

        max_byte_pos
    }

    /// Handle click in editor content area
    fn handle_editor_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: crate::event::SplitId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        use crate::event::Event;

        // Focus this split
        self.split_manager.set_active_split(split_id);
        if buffer_id != self.active_buffer {
            self.position_history.commit_pending_movement();
            self.set_active_buffer(buffer_id);
        }

        // Calculate clicked position in buffer
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            // Account for left margin (line numbers)
            let gutter_width = state.margins.left_total_width() as u16;

            // Calculate relative position in content area
            let content_col = col.saturating_sub(content_rect.x);
            let content_row = row.saturating_sub(content_rect.y);

            // Skip if click is in the gutter
            if content_col < gutter_width {
                return Ok(());
            }

            // Adjust for gutter
            let text_col = content_col.saturating_sub(gutter_width);

            // Account for horizontal scroll
            let actual_col = (text_col as usize) + state.viewport.left_column;

            // Find the byte position for this line and column
            let mut line_iter = state.buffer.line_iterator(state.viewport.top_byte, 80);

            // Navigate to the clicked line
            let mut line_start = state.viewport.top_byte;
            let target_position;
            for _ in 0..content_row {
                if let Some((pos, _content)) = line_iter.next() {
                    line_start = pos;
                } else {
                    break;
                }
            }

            // Get the content of the target line
            if let Some((pos, line_content)) = line_iter.next() {
                line_start = pos;
                // Calculate byte offset within the line by iterating through characters
                // to properly handle multi-byte UTF-8 characters
                let mut byte_offset = 0;
                let mut col_count = 0;
                for ch in line_content.chars() {
                    if col_count >= actual_col {
                        break;
                    }
                    byte_offset += ch.len_utf8();
                    col_count += 1;
                }
                target_position = line_start + byte_offset;
            } else {
                // If we're past the last line, use the line start
                target_position = line_start;
            }

            // Move the primary cursor to this position
            let primary_cursor_id = state.cursors.primary_id();
            let event = Event::MoveCursor {
                cursor_id: primary_cursor_id,
                old_position: 0, // TODO: Get actual old position
                new_position: target_position,
                old_anchor: None, // TODO: Get actual old anchor
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0, // Reset sticky column for goto line
            };

            // Apply the event
            if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
                event_log.append(event.clone());
            }
            state.apply(&event);

            // Track position history
            if !self.in_navigation {
                self.position_history
                    .record_movement(buffer_id, target_position, None);
            }
        }

        Ok(())
    }

    /// Handle click in file explorer
    fn handle_file_explorer_click(
        &mut self,
        _col: u16,
        row: u16,
        explorer_area: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        // Focus file explorer
        self.key_context = crate::keybindings::KeyContext::FileExplorer;

        // Calculate which item was clicked (accounting for border and title)
        // The file explorer has a 1-line border at top and bottom
        let relative_row = row.saturating_sub(explorer_area.y + 1); // +1 for top border

        if let Some(ref mut explorer) = self.file_explorer {
            let display_nodes = explorer.get_display_nodes();
            let scroll_offset = explorer.get_scroll_offset();
            let clicked_index = (relative_row as usize) + scroll_offset;

            if clicked_index < display_nodes.len() {
                let (node_id, _indent) = display_nodes[clicked_index];

                // Select this node
                explorer.set_selected(Some(node_id));

                // Check if it's a file or directory
                let node = explorer.tree().get_node(node_id);
                if let Some(node) = node {
                    if node.is_dir() {
                        // Toggle expand/collapse using the existing method
                        self.file_explorer_toggle_expand();
                    } else if node.is_file() {
                        // Open the file using the existing method
                        self.file_explorer_open_file()?;
                        // Switch focus back to editor after opening file
                        self.key_context = crate::keybindings::KeyContext::Normal;
                    }
                }
            }
        }

        Ok(())
    }

    /// Render the editor to the terminal
    pub fn render(&mut self, frame: &mut Frame) {
        let _span = tracing::trace_span!("render").entered();
        let size = frame.area();

        // Sync viewport with cursor position if needed (deferred from event processing)
        // This batches multiple cursor movements into a single viewport update
        if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
            let primary_cursor = *state.cursors.primary();
            state
                .viewport
                .sync_with_cursor(&mut state.buffer, &primary_cursor);
        }

        // Prepare all buffers for rendering (pre-load viewport data for lazy loading)
        for (_, state) in &mut self.buffers {
            if let Err(e) = state.prepare_for_render() {
                tracing::error!("Failed to prepare buffer for render: {}", e);
                // Continue with partial rendering
            }
        }

        // If help is visible, render help page instead
        if self.help_renderer.is_visible() {
            self.help_renderer
                .render(frame, size, &self.keybindings, &self.theme);
            return;
        }

        // Refresh search highlights for the current viewport if we have an active search
        // This ensures highlights update when scrolling to show matches in the new viewport
        if let Some(ref search_state) = self.search_state {
            let query = search_state.query.clone();
            self.update_search_highlights(&query);
        }

        // Build main vertical layout: [menu_bar, main_content, status_bar]
        // Suggestions popup now overlays instead of resizing the layout
        let constraints = vec![
            Constraint::Length(1), // Menu bar
            Constraint::Min(0),    // Main content area
            Constraint::Length(1), // Status bar
        ];

        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints)
            .split(size);

        let menu_bar_area = main_chunks[0];
        let main_content_area = main_chunks[1];
        let status_bar_idx = 2;

        // Split main content area based on file explorer visibility
        let editor_content_area;

        if self.file_explorer_visible && self.file_explorer.is_some() {
            // Split horizontally: [file_explorer | editor]
            let horizontal_chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Percentage(30), // File explorer
                    Constraint::Percentage(70), // Editor area
                ])
                .split(main_content_area);

            self.cached_layout.file_explorer_area = Some(horizontal_chunks[0]);
            editor_content_area = horizontal_chunks[1];

            // Render file explorer
            if let Some(ref mut explorer) = self.file_explorer {
                let is_focused = self.key_context == KeyContext::FileExplorer;

                // Build set of files with unsaved changes
                let mut files_with_unsaved_changes = std::collections::HashSet::new();
                for (buffer_id, state) in &self.buffers {
                    if state.buffer.is_modified() {
                        if let Some(metadata) = self.buffer_metadata.get(buffer_id) {
                            if let Some(file_path) = metadata.file_path() {
                                files_with_unsaved_changes.insert(file_path.clone());
                            }
                        }
                    }
                }

                FileExplorerRenderer::render(
                    explorer,
                    frame,
                    horizontal_chunks[0],
                    is_focused,
                    &files_with_unsaved_changes,
                    &self.keybindings,
                    self.key_context,
                );
            }
        } else {
            // No file explorer: use entire main content area for editor
            self.cached_layout.file_explorer_area = None;
            editor_content_area = main_content_area;
        }

        // Note: Tabs are now rendered within each split by SplitRenderer

        // Trigger lines_changed hooks for newly visible lines in all visible buffers
        // This allows plugins to add overlays before rendering
        // Only lines that haven't been seen before are sent (batched for efficiency)
        // Use non-blocking hooks to avoid deadlock when actions are awaiting
        if let Some(ref mut ts_manager) = self.ts_plugin_manager {
            let hooks_start = std::time::Instant::now();
            // Get visible buffers and their areas
            let visible_buffers = self.split_manager.get_visible_buffers(editor_content_area);

            let mut total_new_lines = 0usize;
            for (_, buffer_id, split_area) in visible_buffers {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    // Fire render_start hook once per buffer
                    let render_start_args = crate::hooks::HookArgs::RenderStart { buffer_id };
                    ts_manager.run_hook("render_start", render_start_args);

                    // Use the split area height as visible line count
                    let visible_count = split_area.height as usize;
                    let top_byte = state.viewport.top_byte;

                    // Get or create the seen lines set for this buffer
                    let seen_lines = self.seen_lines.entry(buffer_id).or_insert_with(std::collections::HashSet::new);

                    // Collect only NEW lines (not seen before)
                    let mut new_lines: Vec<crate::hooks::LineInfo> = Vec::new();
                    let mut line_number = state.buffer.get_line_number(top_byte);
                    let mut iter = state.buffer.line_iterator(top_byte, self.config.editor.estimated_line_length);

                    for _ in 0..visible_count {
                        if let Some((line_start, line_content)) = iter.next() {
                            // Only add if not seen before
                            if !seen_lines.contains(&line_number) {
                                let byte_end = line_start + line_content.len();
                                new_lines.push(crate::hooks::LineInfo {
                                    line_number,
                                    byte_start: line_start,
                                    byte_end,
                                    content: line_content,
                                });
                                seen_lines.insert(line_number);
                            }
                            line_number += 1;
                        } else {
                            break;
                        }
                    }

                    // Send batched hook if there are new lines
                    if !new_lines.is_empty() {
                        total_new_lines += new_lines.len();
                        let hook_args = crate::hooks::HookArgs::LinesChanged {
                            buffer_id,
                            lines: new_lines,
                        };
                        ts_manager.run_hook("lines_changed", hook_args);
                    }
                }
            }
            let hooks_elapsed = hooks_start.elapsed();
            tracing::trace!(
                new_lines = total_new_lines,
                elapsed_ms = hooks_elapsed.as_millis(),
                elapsed_us = hooks_elapsed.as_micros(),
                "lines_changed hooks total"
            );

            // Process any plugin commands (like AddOverlay) that resulted from the hooks
            let commands = ts_manager.process_commands();
            for command in commands {
                if let Err(e) = self.handle_plugin_command(command) {
                    tracing::error!("Error handling plugin command: {}", e);
                }
            }
        }

        // Render editor content (same for both layouts)
        let lsp_waiting = self.pending_completion_request.is_some()
            || self.pending_goto_definition_request.is_some();

        let split_areas = SplitRenderer::render_content(
            frame,
            editor_content_area,
            &self.split_manager,
            &mut self.buffers,
            &self.buffer_metadata,
            &mut self.event_logs,
            &self.theme,
            lsp_waiting,
            self.config.editor.large_file_threshold_bytes,
            self.config.editor.line_wrap,
            self.config.editor.estimated_line_length,
            Some(&self.split_view_states),
        );
        self.cached_layout.split_areas = split_areas;
        self.cached_layout.separator_areas = self.split_manager.get_separators_with_ids(editor_content_area);
        self.cached_layout.editor_content_area = Some(editor_content_area);

        // Render hover highlights for separators and scrollbars
        self.render_hover_highlights(frame);

        // Render suggestions as overlay if present (same for both layouts)
        self.cached_layout.suggestions_area = None;
        if let Some(prompt) = &self.prompt {
            if !prompt.suggestions.is_empty() {
                // Calculate overlay area: position above status bar
                let suggestion_count = prompt.suggestions.len().min(10);
                let height = suggestion_count as u16 + 2; // +2 for borders

                // Position suggestions above the status bar
                let suggestions_area = ratatui::layout::Rect {
                    x: 0,
                    y: main_chunks[status_bar_idx].y.saturating_sub(height),
                    width: size.width,
                    height,
                };

                // Clear the area behind the suggestions to obscure underlying text
                frame.render_widget(ratatui::widgets::Clear, suggestions_area);

                self.cached_layout.suggestions_area = SuggestionsRenderer::render_with_hover(
                    frame,
                    suggestions_area,
                    prompt,
                    &self.theme,
                    self.mouse_state.hover_target.as_ref(),
                );
            }
        }

        // Render status bar (same for both layouts)
        // Clone all immutable values before the mutable borrow
        let display_name = self
            .buffer_metadata
            .get(&self.active_buffer)
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| "[No Name]".to_string());
        let status_message = self.status_message.clone();
        let prompt = self.prompt.clone();
        let lsp_status = self.lsp_status.clone();
        let theme = self.theme.clone();

        StatusBarRenderer::render(
            frame,
            main_chunks[status_bar_idx],
            self.active_state_mut(),
            &status_message,
            &prompt,
            &lsp_status,
            &theme,
            &display_name,
        );

        // Render popups from the active buffer state
        // Clone theme to avoid borrow checker issues with active_state_mut()
        let theme_clone = self.theme.clone();
        let hover_target = self.mouse_state.hover_target.clone();

        // Clear popup areas and recalculate
        self.cached_layout.popup_areas.clear();

        // Collect popup information without holding a mutable borrow
        let popup_info: Vec<_> = {
            let state = self.active_state_mut();
            if state.popups.is_visible() {
                // Get the primary cursor position for popup positioning
                let primary_cursor = state.cursors.primary();
                let cursor_screen_pos = state
                    .viewport
                    .cursor_screen_position(&mut state.buffer, primary_cursor);

                // Adjust cursor position to account for tab bar (1 line offset)
                let cursor_screen_pos = (cursor_screen_pos.0, cursor_screen_pos.1 + 1);

                // Collect popup data
                state
                    .popups
                    .all()
                    .iter()
                    .enumerate()
                    .map(|(popup_idx, popup)| {
                        let popup_area = popup.calculate_area(size, Some(cursor_screen_pos));

                        // Track popup area for mouse hit testing
                        let inner_area = if popup.bordered {
                            ratatui::layout::Rect {
                                x: popup_area.x + 1,
                                y: popup_area.y + 1,
                                width: popup_area.width.saturating_sub(2),
                                height: popup_area.height.saturating_sub(2),
                            }
                        } else {
                            popup_area
                        };

                        let num_items = match &popup.content {
                            crate::popup::PopupContent::List { items, .. } => items.len(),
                            _ => 0,
                        };

                        (popup_idx, popup_area, inner_area, popup.scroll_offset, num_items)
                    })
                    .collect()
            } else {
                Vec::new()
            }
        };

        // Store popup areas for mouse hit testing
        self.cached_layout.popup_areas = popup_info.clone();

        // Now render popups
        let state = self.active_state_mut();
        if state.popups.is_visible() {
            for (popup_idx, popup) in state.popups.all().iter().enumerate() {
                if let Some((_, popup_area, _, _, _)) = popup_info.get(popup_idx) {
                    popup.render_with_hover(frame, *popup_area, &theme_clone, hover_target.as_ref());
                }
            }
        }

        // Render menu bar last so dropdown appears on top of all other content
        crate::ui::MenuRenderer::render(
            frame,
            menu_bar_area,
            &self.config.menu,
            &self.menu_state,
            &self.keybindings,
            &self.theme,
            self.mouse_state.hover_target.as_ref(),
        );
    }

    /// Render hover highlights for interactive elements (separators, scrollbars)
    fn render_hover_highlights(&self, frame: &mut Frame) {
        use ratatui::style::Style;
        use ratatui::text::Span;
        use ratatui::widgets::Paragraph;

        match &self.mouse_state.hover_target {
            Some(HoverTarget::SplitSeparator(split_id, direction)) => {
                // Highlight the separator with hover color
                for (sid, dir, x, y, length) in &self.cached_layout.separator_areas {
                    if sid == split_id && dir == direction {
                        let hover_style = Style::default().fg(self.theme.split_separator_hover_fg);
                        match dir {
                            SplitDirection::Horizontal => {
                                let line_text = "─".repeat(*length as usize);
                                let paragraph = Paragraph::new(Span::styled(line_text, hover_style));
                                frame.render_widget(
                                    paragraph,
                                    ratatui::layout::Rect::new(*x, *y, *length, 1),
                                );
                            }
                            SplitDirection::Vertical => {
                                for offset in 0..*length {
                                    let paragraph = Paragraph::new(Span::styled("│", hover_style));
                                    frame.render_widget(
                                        paragraph,
                                        ratatui::layout::Rect::new(*x, y + offset, 1, 1),
                                    );
                                }
                            }
                        }
                    }
                }
            }
            Some(HoverTarget::ScrollbarThumb(split_id)) => {
                // Highlight scrollbar thumb
                for (sid, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
                    &self.cached_layout.split_areas
                {
                    if sid == split_id {
                        let hover_style = Style::default().fg(self.theme.scrollbar_thumb_hover_fg);
                        for row_offset in *thumb_start..*thumb_end {
                            let paragraph = Paragraph::new(Span::styled("█", hover_style));
                            frame.render_widget(
                                paragraph,
                                ratatui::layout::Rect::new(
                                    scrollbar_rect.x,
                                    scrollbar_rect.y + row_offset as u16,
                                    1,
                                    1,
                                ),
                            );
                        }
                    }
                }
            }
            Some(HoverTarget::ScrollbarTrack(split_id)) => {
                // Highlight scrollbar track but preserve the thumb
                for (sid, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
                    &self.cached_layout.split_areas
                {
                    if sid == split_id {
                        let track_hover_style =
                            Style::default().fg(self.theme.scrollbar_track_hover_fg);
                        let thumb_style = Style::default().fg(self.theme.scrollbar_thumb_fg);
                        for row_offset in 0..scrollbar_rect.height {
                            let is_thumb =
                                (row_offset as usize) >= *thumb_start && (row_offset as usize) < *thumb_end;
                            let (char, style) = if is_thumb {
                                ("█", thumb_style)
                            } else {
                                ("│", track_hover_style)
                            };
                            let paragraph = Paragraph::new(Span::styled(char, style));
                            frame.render_widget(
                                paragraph,
                                ratatui::layout::Rect::new(
                                    scrollbar_rect.x,
                                    scrollbar_rect.y + row_offset,
                                    1,
                                    1,
                                ),
                            );
                        }
                    }
                }
            }
            // Menu hover is handled by MenuRenderer
            _ => {}
        }
    }

    // === Overlay Management (Event-Driven) ===

    /// Add an overlay for decorations (underlines, highlights, etc.)
    pub fn add_overlay(
        &mut self,
        overlay_id: String,
        range: Range<usize>,
        face: crate::event::OverlayFace,
        priority: i32,
        message: Option<String>,
    ) {
        let event = Event::AddOverlay {
            overlay_id,
            range,
            face,
            priority,
            message,
        };
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Remove an overlay by ID
    pub fn remove_overlay(&mut self, overlay_id: String) {
        let event = Event::RemoveOverlay { overlay_id };
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Remove all overlays in a range
    pub fn remove_overlays_in_range(&mut self, range: Range<usize>) {
        let event = Event::RemoveOverlaysInRange { range };
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Clear all overlays
    pub fn clear_overlays(&mut self) {
        let event = Event::ClearOverlays;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    // === Popup Management (Event-Driven) ===

    /// Show a popup window
    pub fn show_popup(&mut self, popup: crate::event::PopupData) {
        let event = Event::ShowPopup { popup };
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Hide the topmost popup
    pub fn hide_popup(&mut self) {
        let event = Event::HidePopup;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);

        // Clear hover symbol highlight if present
        if self.hover_symbol_range.is_some() {
            self.hover_symbol_range = None;
            let remove_overlay_event = crate::event::Event::RemoveOverlay {
                overlay_id: "hover_symbol".to_string(),
            };
            self.apply_event_to_active_buffer(&remove_overlay_event);
        }
    }

    /// Dismiss transient popups (Hover, Signature Help) if present
    /// These popups should be dismissed on scroll or other user actions
    fn dismiss_transient_popups(&mut self) {
        let is_transient_popup = self
            .active_state()
            .popups
            .top()
            .and_then(|p| p.title.as_ref())
            .is_some_and(|title| title == "Hover" || title == "Signature Help");

        if is_transient_popup {
            self.hide_popup();
            tracing::debug!("Dismissed transient popup on scroll");
        }
    }

    /// Clear all popups
    pub fn clear_popups(&mut self) {
        let event = Event::ClearPopups;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup selection (next item)
    pub fn popup_select_next(&mut self) {
        let event = Event::PopupSelectNext;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup selection (previous item)
    pub fn popup_select_prev(&mut self) {
        let event = Event::PopupSelectPrev;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup (page down)
    pub fn popup_page_down(&mut self) {
        let event = Event::PopupPageDown;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup (page up)
    pub fn popup_page_up(&mut self) {
        let event = Event::PopupPageUp;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    // === Help Page Management (Delegates to HelpRenderer) ===

    /// Toggle help page visibility
    pub fn toggle_help(&mut self) {
        self.help_renderer.toggle();
    }

    /// Check if help page is visible
    pub fn is_help_visible(&self) -> bool {
        self.help_renderer.is_visible()
    }

    /// Scroll the help page
    pub fn scroll_help(&mut self, delta: isize) {
        self.help_renderer.scroll(delta, &self.keybindings);
    }

    // === LSP Diagnostics Display ===
    // NOTE: Diagnostics are now applied automatically via process_async_messages()
    // when received from the LSP server asynchronously. No manual polling needed!

    /// Notify LSP of a text change event
    fn notify_lsp_change(&mut self, event: &Event) {
        // Collect all changes from the event (handles batches efficiently)
        let changes = self.collect_lsp_changes(event);
        if changes.is_empty() {
            return;
        }

        // Check if LSP is enabled for this buffer
        let metadata = match self.buffer_metadata.get(&self.active_buffer) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "notify_lsp_change: no metadata for buffer {:?}",
                    self.active_buffer
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            // LSP is disabled for this buffer, don't try to spawn or notify
            tracing::debug!("notify_lsp_change: LSP disabled for this buffer");
            return;
        }

        // Get the URI (computed once in with_file)
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!(
                    "notify_lsp_change: no URI for buffer (not a file or URI creation failed)"
                );
                return;
            }
        };

        // Get the file path for language detection
        let path = match metadata.file_path() {
            Some(p) => p,
            None => {
                tracing::debug!("notify_lsp_change: no file path for buffer");
                return;
            }
        };

        let language = match detect_language(path) {
            Some(l) => l,
            None => {
                tracing::debug!("notify_lsp_change: no language detected for {:?}", path);
                return;
            }
        };

        tracing::debug!(
            "notify_lsp_change: sending {} changes to {} in single didChange notification",
            changes.len(),
            uri.as_str()
        );

        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(&language) {
                // Send all changes in a single didChange notification
                // This is much more efficient for batch operations like LSP rename
                if let Err(e) = client.did_change(uri.clone(), changes) {
                    tracing::warn!("Failed to send didChange to LSP: {}", e);
                } else {
                    tracing::info!("Successfully sent batched didChange to LSP");

                    // Request pull diagnostics after the change
                    // TODO: Consider debouncing this to avoid excessive requests during rapid typing
                    let previous_result_id = self
                        .diagnostic_result_ids
                        .get(uri.as_str())
                        .cloned();
                    let request_id = self.next_lsp_request_id;
                    self.next_lsp_request_id += 1;

                    if let Err(e) = client.document_diagnostic(
                        request_id,
                        uri.clone(),
                        previous_result_id,
                    ) {
                        tracing::debug!(
                            "Failed to request pull diagnostics after change (server may not support): {}",
                            e
                        );
                    } else {
                        tracing::debug!(
                            "Requested pull diagnostics after change for {} (request_id={})",
                            uri.as_str(),
                            request_id
                        );
                    }
                }
            } else {
                tracing::warn!(
                    "notify_lsp_change: failed to get or spawn LSP client for {}",
                    language
                );
            }
        } else {
            tracing::debug!("notify_lsp_change: no LSP manager available");
        }
    }

    /// Collect all LSP text document changes from an event (recursively for batches)
    fn collect_lsp_changes(&self, event: &Event) -> Vec<TextDocumentContentChangeEvent> {
        match event {
            Event::Insert { position, text, .. } => {
                tracing::debug!(
                    "collect_lsp_changes: processing Insert at position {}",
                    position
                );
                // For insert: create a zero-width range at the insertion point
                let (line, character) = self
                    .active_state()
                    .buffer
                    .position_to_lsp_position(*position);
                let lsp_pos = Position::new(line as u32, character as u32);
                let lsp_range = LspRange::new(lsp_pos, lsp_pos);
                vec![TextDocumentContentChangeEvent {
                    range: Some(lsp_range),
                    range_length: None,
                    text: text.clone(),
                }]
            }
            Event::Delete { range, .. } => {
                tracing::debug!("collect_lsp_changes: processing Delete range {:?}", range);
                // For delete: create a range from start to end, send empty string
                let (start_line, start_char) = self
                    .active_state()
                    .buffer
                    .position_to_lsp_position(range.start);
                let (end_line, end_char) = self
                    .active_state()
                    .buffer
                    .position_to_lsp_position(range.end);
                let lsp_range = LspRange::new(
                    Position::new(start_line as u32, start_char as u32),
                    Position::new(end_line as u32, end_char as u32),
                );
                vec![TextDocumentContentChangeEvent {
                    range: Some(lsp_range),
                    range_length: None,
                    text: String::new(),
                }]
            }
            Event::Batch { events, .. } => {
                // Collect all changes from sub-events into a single vector
                // This allows sending all changes in one didChange notification
                tracing::debug!(
                    "collect_lsp_changes: processing Batch with {} events",
                    events.len()
                );
                let mut all_changes = Vec::new();
                for sub_event in events {
                    all_changes.extend(self.collect_lsp_changes(sub_event));
                }
                all_changes
            }
            _ => Vec::new(), // Ignore cursor movements and other events
        }
    }

    /// Notify LSP of a file save
    fn notify_lsp_save(&mut self) {
        // Check if LSP is enabled for this buffer
        let metadata = match self.buffer_metadata.get(&self.active_buffer) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "notify_lsp_save: no metadata for buffer {:?}",
                    self.active_buffer
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            tracing::debug!("notify_lsp_save: LSP disabled for this buffer");
            return;
        }

        // Get the URI
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!("notify_lsp_save: no URI for buffer");
                return;
            }
        };

        // Get the file path for language detection
        let path = match metadata.file_path() {
            Some(p) => p,
            None => {
                tracing::debug!("notify_lsp_save: no file path for buffer");
                return;
            }
        };

        let language = match detect_language(path) {
            Some(l) => l,
            None => {
                tracing::debug!("notify_lsp_save: no language detected for {:?}", path);
                return;
            }
        };

        // Get the full text to send with didSave
        let full_text = self.active_state().buffer.to_string();
        tracing::debug!(
            "notify_lsp_save: sending didSave to {} (text length: {} bytes)",
            uri.as_str(),
            full_text.len()
        );

        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(&language) {
                // Send didSave with the full text content
                if let Err(e) = client.did_save(uri, Some(full_text)) {
                    tracing::warn!("Failed to send didSave to LSP: {}", e);
                } else {
                    tracing::info!("Successfully sent didSave to LSP");
                }
            } else {
                tracing::warn!(
                    "notify_lsp_save: failed to get or spawn LSP client for {}",
                    language
                );
            }
        } else {
            tracing::debug!("notify_lsp_save: no LSP manager available");
        }
    }

    /// Convert an action into a list of events to apply to the active buffer
    /// Returns None for actions that don't generate events (like Quit)
    pub fn action_to_events(&mut self, action: Action) -> Option<Vec<Event>> {
        let tab_size = self.config.editor.tab_size;
        let auto_indent = self.config.editor.auto_indent;
        let estimated_line_length = self.config.editor.estimated_line_length;
        convert_action_to_events(
            self.active_state_mut(),
            action,
            tab_size,
            auto_indent,
            estimated_line_length,
        )
    }

    // === Search and Replace Methods ===

    /// Clear all search highlights from the active buffer
    fn clear_search_highlights(&mut self) {
        let state = self.active_state_mut();
        let overlay_ids: Vec<String> = state
            .overlays
            .all()
            .iter()
            .filter_map(|o| {
                o.id.as_ref().and_then(|id| {
                    if id.starts_with("search_highlight_") || id.starts_with("search_match_") {
                        Some(id.clone())
                    } else {
                        None
                    }
                })
            })
            .collect();

        for id in overlay_ids {
            state.overlays.remove_by_id(&id, &mut state.marker_list);
        }

        // Also clear search state
        self.search_state = None;
    }

    /// Update search highlights in visible viewport only (for incremental search)
    /// This is called as the user types in the search prompt for real-time feedback
    fn update_search_highlights(&mut self, query: &str) {
        // If query is empty, clear highlights and return
        if query.is_empty() {
            self.clear_search_highlights();
            return;
        }

        // Get theme color before borrowing state
        let search_bg = self.theme.search_match_bg;

        let state = self.active_state_mut();

        // Clear any existing search highlights
        let overlay_ids: Vec<String> = state
            .overlays
            .all()
            .iter()
            .filter_map(|o| {
                o.id.as_ref().and_then(|id| {
                    if id.starts_with("search_highlight_") {
                        Some(id.clone())
                    } else {
                        None
                    }
                })
            })
            .collect();

        for id in overlay_ids {
            state.overlays.remove_by_id(&id, &mut state.marker_list);
        }

        // Get the visible viewport range
        let viewport = &state.viewport;
        let top_byte = viewport.top_byte;
        let visible_height = viewport.height.saturating_sub(2); // Subtract tab bar and status bar

        // Get the visible content by iterating through visible lines
        let visible_start = top_byte;
        let mut visible_end = top_byte;

        {
            let mut line_iter = state.buffer.line_iterator(top_byte, 80);
            for _ in 0..visible_height {
                if let Some((line_start, line_content)) = line_iter.next() {
                    visible_end = line_start + line_content.len();
                } else {
                    break;
                }
            }
        }

        // Ensure we don't go past buffer end
        visible_end = visible_end.min(state.buffer.len());

        // Get the visible text
        let visible_text = state.get_text_range(visible_start, visible_end);

        // Search for matches in visible area (case-insensitive)
        let query_lower = query.to_lowercase();
        let visible_text_lower = visible_text.to_lowercase();

        let mut match_count = 0;
        let mut start = 0;
        while let Some(pos) = visible_text_lower[start..].find(&query_lower) {
            let absolute_pos = visible_start + start + pos;

            // Add overlay for this match
            let overlay_id = format!("search_highlight_{}", match_count);
            let overlay = crate::overlay::Overlay::with_id(
                &mut state.marker_list,
                absolute_pos..(absolute_pos + query.len()),
                crate::overlay::OverlayFace::Background { color: search_bg },
                overlay_id,
            )
            .with_priority_value(10); // Priority - above syntax highlighting

            state.overlays.add(overlay);

            match_count += 1;
            start = start + pos + 1; // Move past this match
        }
    }

    /// Perform a search and update search state
    fn perform_search(&mut self, query: &str) {
        // Don't clear search highlights here - keep them from incremental search
        // They will be cleared when:
        // 1. User cancels search (Escape)
        // 2. User makes an edit to the buffer
        // 3. User starts a new search (update_search_highlights clears old ones)

        if query.is_empty() {
            self.search_state = None;
            self.set_status_message("Search cancelled.".to_string());
            return;
        }

        // Check if there's a selection for search-in-selection
        let search_range = {
            let state = self.active_state();
            state.cursors.primary().selection_range()
        };

        let buffer_content = {
            let state = self.active_state();
            state.buffer.to_string()
        };

        // Get search settings
        let case_sensitive = self.search_case_sensitive;
        let whole_word = self.search_whole_word;

        // Find all matches
        let mut matches = Vec::new();

        // Determine search boundaries
        let (search_start, search_end) = if let Some(ref range) = search_range {
            (range.start, range.end)
        } else {
            (0, buffer_content.len())
        };

        // Prepare search strings based on case sensitivity
        let (search_buffer, search_query) = if case_sensitive {
            (buffer_content.clone(), query.to_string())
        } else {
            (buffer_content.to_lowercase(), query.to_lowercase())
        };

        // Helper function to check if position is a word boundary
        let is_word_boundary = |pos: usize, at_start: bool| -> bool {
            if !whole_word {
                return true;
            }
            if at_start {
                pos == 0 || !buffer_content.chars().nth(pos.saturating_sub(1)).map(|c| c.is_alphanumeric() || c == '_').unwrap_or(false)
            } else {
                pos >= buffer_content.len() || !buffer_content.chars().nth(pos).map(|c| c.is_alphanumeric() || c == '_').unwrap_or(false)
            }
        };

        // Find all matches within the search range
        let mut start = search_start;
        while start < search_end {
            if let Some(pos) = search_buffer[start..search_end].find(&search_query) {
                let absolute_pos = start + pos;
                let end_pos = absolute_pos + query.len();

                // Check word boundaries if whole word matching is enabled
                if is_word_boundary(absolute_pos, true) && is_word_boundary(end_pos, false) {
                    matches.push(absolute_pos);
                }
                start = absolute_pos + 1;
            } else {
                break;
            }
        }

        if matches.is_empty() {
            self.search_state = None;
            let msg = if search_range.is_some() {
                format!("No matches found for '{}' in selection", query)
            } else {
                format!("No matches found for '{}'", query)
            };
            self.set_status_message(msg);
            return;
        }

        // Find the first match at or after the current cursor position
        let cursor_pos = {
            let state = self.active_state();
            state.cursors.primary().position
        };
        let current_match_index = matches
            .iter()
            .position(|&pos| pos >= cursor_pos)
            .unwrap_or(0);

        // Move cursor to the first match
        let match_pos = matches[current_match_index];
        {
            let state = self.active_state_mut();
            state.cursors.primary_mut().position = match_pos;
            state.cursors.primary_mut().anchor = None;
            state
                .viewport
                .ensure_visible(&mut state.buffer, state.cursors.primary());
        }

        let num_matches = matches.len();

        // Update search state
        self.search_state = Some(SearchState {
            query: query.to_string(),
            matches,
            current_match_index: Some(current_match_index),
            wrap_search: search_range.is_none(), // Only wrap if not searching in selection
            search_range,
            case_sensitive: self.search_case_sensitive,
            whole_word: self.search_whole_word,
        });

        let msg = if self.search_state.as_ref().unwrap().search_range.is_some() {
            format!(
                "Found {} match{} for '{}' in selection",
                num_matches,
                if num_matches == 1 { "" } else { "es" },
                query
            )
        } else {
            format!(
                "Found {} match{} for '{}'",
                num_matches,
                if num_matches == 1 { "" } else { "es" },
                query
            )
        };
        self.set_status_message(msg);
    }

    /// Find the next match
    fn find_next(&mut self) {
        if let Some(ref mut search_state) = self.search_state {
            if search_state.matches.is_empty() {
                return;
            }

            let current_index = search_state.current_match_index.unwrap_or(0);
            let next_index = if current_index + 1 < search_state.matches.len() {
                current_index + 1
            } else if search_state.wrap_search {
                0 // Wrap to beginning
            } else {
                self.set_status_message("No more matches.".to_string());
                return;
            };

            search_state.current_match_index = Some(next_index);
            let match_pos = search_state.matches[next_index];
            let matches_len = search_state.matches.len();

            {
                let state = self.active_state_mut();
                state.cursors.primary_mut().position = match_pos;
                state.cursors.primary_mut().anchor = None;
                state
                    .viewport
                    .ensure_visible(&mut state.buffer, state.cursors.primary());
            }

            self.set_status_message(format!("Match {} of {}", next_index + 1, matches_len));
        } else {
            self.set_status_message("No active search. Press Ctrl+F to search.".to_string());
        }
    }

    /// Find the previous match
    fn find_previous(&mut self) {
        if let Some(ref mut search_state) = self.search_state {
            if search_state.matches.is_empty() {
                return;
            }

            let current_index = search_state.current_match_index.unwrap_or(0);
            let prev_index = if current_index > 0 {
                current_index - 1
            } else if search_state.wrap_search {
                search_state.matches.len() - 1 // Wrap to end
            } else {
                self.set_status_message("No more matches.".to_string());
                return;
            };

            search_state.current_match_index = Some(prev_index);
            let match_pos = search_state.matches[prev_index];
            let matches_len = search_state.matches.len();

            {
                let state = self.active_state_mut();
                state.cursors.primary_mut().position = match_pos;
                state.cursors.primary_mut().anchor = None;
                state
                    .viewport
                    .ensure_visible(&mut state.buffer, state.cursors.primary());
            }

            self.set_status_message(format!("Match {} of {}", prev_index + 1, matches_len));
        } else {
            self.set_status_message("No active search. Press Ctrl+F to search.".to_string());
        }
    }

    /// Perform a replace-all operation
    /// Replaces all occurrences of the search query with the replacement text
    fn perform_replace(&mut self, search: &str, replacement: &str) {
        if search.is_empty() {
            self.set_status_message("Replace: empty search query.".to_string());
            return;
        }

        // Find all matches first (before making any modifications)
        let matches = {
            let state = self.active_state();
            let buffer_len = state.buffer.len();
            let mut matches = Vec::new();
            let mut current_pos = 0;

            while current_pos < buffer_len {
                if let Some(offset) = state.buffer.find_next_in_range(
                    search,
                    current_pos,
                    Some(current_pos..buffer_len),
                ) {
                    matches.push(offset);
                    current_pos = offset + search.len();
                } else {
                    break;
                }
            }
            matches
        };

        let count = matches.len();

        if count == 0 {
            self.set_status_message(format!("No occurrences of '{}' found.", search));
            return;
        }

        // Capture current cursor state for undo
        let cursor_id = self.active_state().cursors.primary_id();
        let cursor = self.active_state().cursors.get(cursor_id).unwrap().clone();
        let old_position = cursor.position;
        let old_anchor = cursor.anchor;
        let old_sticky_column = cursor.sticky_column;

        // Create events for all replacements (in reverse order to preserve positions)
        let mut events = Vec::new();

        // Add MoveCursor at the beginning to save cursor position for undo
        events.push(Event::MoveCursor {
            cursor_id,
            old_position,
            new_position: old_position, // Keep cursor where it is
            old_anchor,
            new_anchor: old_anchor,
            old_sticky_column,
            new_sticky_column: old_sticky_column,
        });

        for match_pos in matches.into_iter().rev() {
            let end = match_pos + search.len();
            let range = match_pos..end;

            // Get the text being deleted
            let deleted_text = self
                .active_state_mut()
                .get_text_range(range.start, range.end);

            // Add Delete event
            events.push(Event::Delete {
                range: range.clone(),
                deleted_text,
                cursor_id,
            });

            // Add Insert event
            events.push(Event::Insert {
                position: match_pos,
                text: replacement.to_string(),
                cursor_id,
            });
        }

        // Wrap all replacement events in a single Batch for atomic undo
        let batch = Event::Batch {
            events,
            description: format!("Replace all '{}' with '{}'", search, replacement),
        };

        // Apply through event log for proper undo support
        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);

        // Clear search state since positions are now invalid
        self.search_state = None;

        // Clear any search highlight overlays
        let state = self.active_state_mut();
        let overlay_ids: Vec<String> = state
            .overlays
            .all()
            .iter()
            .filter_map(|o| {
                o.id.as_ref().and_then(|id| {
                    if id.starts_with("search_highlight_") || id.starts_with("search_match_") {
                        Some(id.clone())
                    } else {
                        None
                    }
                })
            })
            .collect();

        for id in overlay_ids {
            state.overlays.remove_by_id(&id, &mut state.marker_list);
        }

        // Set status message
        self.set_status_message(format!(
            "Replaced {} occurrence{} of '{}' with '{}'",
            count,
            if count == 1 { "" } else { "s" },
            search,
            replacement
        ));
    }

    /// Start interactive replace mode (query-replace)
    fn start_interactive_replace(&mut self, search: &str, replacement: &str) {
        if search.is_empty() {
            self.set_status_message("Query replace: empty search query.".to_string());
            return;
        }

        // Find the first match lazily (don't find all matches upfront)
        let state = self.active_state();
        let start_pos = state.cursors.primary().position;
        let first_match = state.buffer.find_next(search, start_pos);

        let Some(first_match_pos) = first_match else {
            self.set_status_message(format!("No occurrences of '{}' found.", search));
            return;
        };

        // Initialize interactive replace state with just the current match
        self.interactive_replace_state = Some(InteractiveReplaceState {
            search: search.to_string(),
            replacement: replacement.to_string(),
            current_match_pos: first_match_pos,
            start_pos: first_match_pos,
            has_wrapped: false,
            replacements_made: 0,
        });

        // Move cursor to first match and show prompt
        let state = self.active_state_mut();
        state.cursors.primary_mut().position = first_match_pos;
        state.cursors.primary_mut().anchor = None;
        state
            .viewport
            .ensure_visible(&mut state.buffer, state.cursors.primary());

        self.set_status_message("Replace? (y/n/!/q)".to_string());
    }

    /// Handle interactive replace key press (y/n/!/q)
    fn handle_interactive_replace_key(&mut self, c: char) -> std::io::Result<()> {
        let state = self.interactive_replace_state.clone();
        let Some(mut ir_state) = state else {
            return Ok(());
        };

        match c {
            'y' | 'Y' => {
                // Replace current match
                self.replace_current_match(&ir_state)?;
                ir_state.replacements_made += 1;

                // Find next match lazily (after the replacement)
                let search_pos = ir_state.current_match_pos + ir_state.replacement.len();
                if let Some((next_match, wrapped)) =
                    self.find_next_match_for_replace(&ir_state, search_pos)
                {
                    ir_state.current_match_pos = next_match;
                    if wrapped {
                        ir_state.has_wrapped = true;
                    }
                    self.interactive_replace_state = Some(ir_state.clone());
                    self.move_to_current_match(&ir_state);
                } else {
                    self.finish_interactive_replace(ir_state.replacements_made);
                }
            }
            'n' | 'N' => {
                // Skip current match and find next
                let search_pos = ir_state.current_match_pos + ir_state.search.len();
                if let Some((next_match, wrapped)) =
                    self.find_next_match_for_replace(&ir_state, search_pos)
                {
                    ir_state.current_match_pos = next_match;
                    if wrapped {
                        ir_state.has_wrapped = true;
                    }
                    self.interactive_replace_state = Some(ir_state.clone());
                    self.move_to_current_match(&ir_state);
                } else {
                    self.finish_interactive_replace(ir_state.replacements_made);
                }
            }
            '!' => {
                // Replace all remaining matches with SINGLE confirmation
                // Undo behavior: ONE undo step undoes ALL remaining replacements
                // Uses streaming search (doesn't materialize file), but collects positions for batch

                // First replace the current match
                self.replace_current_match(&ir_state)?;
                ir_state.replacements_made += 1;

                // Find all remaining matches using streaming search
                // Collecting positions (Vec<usize>) is low memory cost even for huge files
                let search_pos = ir_state.current_match_pos + ir_state.replacement.len();
                let remaining_matches = {
                    let mut matches = Vec::new();
                    let mut current_pos = search_pos;
                    let mut temp_state = ir_state.clone();

                    // Find matches lazily one at a time, collect positions
                    loop {
                        if let Some((next_match, wrapped)) =
                            self.find_next_match_for_replace(&temp_state, current_pos)
                        {
                            matches.push(next_match);
                            current_pos = next_match + temp_state.search.len();
                            if wrapped {
                                temp_state.has_wrapped = true;
                            }
                        } else {
                            break;
                        }
                    }
                    matches
                };

                let remaining_count = remaining_matches.len();

                if remaining_count > 0 {
                    // Capture current cursor state for undo
                    let cursor_id = self.active_state().cursors.primary_id();
                    let cursor = self.active_state().cursors.get(cursor_id).unwrap().clone();
                    let old_position = cursor.position;
                    let old_anchor = cursor.anchor;
                    let old_sticky_column = cursor.sticky_column;

                    // Create events for all remaining replacements (reverse order preserves positions)
                    let mut events = Vec::new();

                    // Add MoveCursor at the beginning to save cursor position for undo
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position,
                        new_position: old_position, // Keep cursor where it is
                        old_anchor,
                        new_anchor: old_anchor,
                        old_sticky_column,
                        new_sticky_column: old_sticky_column,
                    });

                    for match_pos in remaining_matches.into_iter().rev() {
                        let end = match_pos + ir_state.search.len();
                        let range = match_pos..end;
                        let deleted_text = self
                            .active_state_mut()
                            .get_text_range(range.start, range.end);

                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text,
                            cursor_id,
                        });

                        events.push(Event::Insert {
                            position: match_pos,
                            text: ir_state.replacement.clone(),
                            cursor_id,
                        });
                    }

                    // Single Batch = single undo step for all remaining replacements
                    let batch = Event::Batch {
                        events,
                        description: format!(
                            "Query replace remaining '{}' with '{}'",
                            ir_state.search, ir_state.replacement
                        ),
                    };

                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);

                    ir_state.replacements_made += remaining_count;
                }

                self.finish_interactive_replace(ir_state.replacements_made);
            }
            'q' | 'Q' | '\x1b' => {
                // Escape - quit interactive replace
                self.finish_interactive_replace(ir_state.replacements_made);
            }
            _ => {
                // Unknown key - show help
                self.set_status_message(
                    "Replace this occurrence? (y=yes, n=no, !=all, q=quit)".to_string(),
                );
            }
        }

        Ok(())
    }

    /// Find the next match for interactive replace (lazy search with wrap-around)
    fn find_next_match_for_replace(
        &self,
        ir_state: &InteractiveReplaceState,
        start_pos: usize,
    ) -> Option<(usize, bool)> {
        let state = self.active_state();

        if ir_state.has_wrapped {
            // We've already wrapped - only search from start_pos up to (but not including) the original start position
            // Use find_next_in_range to avoid wrapping again
            let search_range = Some(start_pos..ir_state.start_pos);
            if let Some(match_pos) =
                state
                    .buffer
                    .find_next_in_range(&ir_state.search, start_pos, search_range)
            {
                return Some((match_pos, true));
            }
            None // No more matches before original start position
        } else {
            // Haven't wrapped yet - search normally from start_pos
            // First try from start_pos to end of buffer
            let buffer_len = state.buffer.len();
            let search_range = Some(start_pos..buffer_len);
            if let Some(match_pos) =
                state
                    .buffer
                    .find_next_in_range(&ir_state.search, start_pos, search_range)
            {
                return Some((match_pos, false));
            }

            // No match from start_pos to end - wrap to beginning
            // Search from 0 to start_pos (original position)
            let wrap_range = Some(0..ir_state.start_pos);
            if let Some(match_pos) =
                state
                    .buffer
                    .find_next_in_range(&ir_state.search, 0, wrap_range)
            {
                return Some((match_pos, true)); // Found match after wrapping
            }

            None // No matches found anywhere
        }
    }

    /// Replace the current match in interactive replace mode
    fn replace_current_match(&mut self, ir_state: &InteractiveReplaceState) -> std::io::Result<()> {
        let match_pos = ir_state.current_match_pos;
        let search_len = ir_state.search.len();
        let range = match_pos..(match_pos + search_len);

        // Get the deleted text for the event
        let deleted_text = self
            .active_state_mut()
            .get_text_range(range.start, range.end);

        // Capture current cursor state for undo
        let cursor_id = self.active_state().cursors.primary_id();
        let cursor = self.active_state().cursors.get(cursor_id).unwrap().clone();
        let old_position = cursor.position;
        let old_anchor = cursor.anchor;
        let old_sticky_column = cursor.sticky_column;

        // Create events: MoveCursor, Delete, Insert
        // The MoveCursor saves the cursor position so undo can restore it
        let events = vec![
            Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: match_pos,
                old_anchor,
                new_anchor: None,
                old_sticky_column,
                new_sticky_column: 0,
            },
            Event::Delete {
                range: range.clone(),
                deleted_text,
                cursor_id,
            },
            Event::Insert {
                position: match_pos,
                text: ir_state.replacement.clone(),
                cursor_id,
            },
        ];

        // Wrap in batch for atomic undo
        let batch = Event::Batch {
            events,
            description: format!(
                "Query replace '{}' with '{}'",
                ir_state.search, ir_state.replacement
            ),
        };

        // Apply the batch through the event log
        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);

        Ok(())
    }

    /// Move cursor to the current match in interactive replace
    fn move_to_current_match(&mut self, ir_state: &InteractiveReplaceState) {
        let match_pos = ir_state.current_match_pos;
        let state = self.active_state_mut();
        state.cursors.primary_mut().position = match_pos;
        state.cursors.primary_mut().anchor = None;
        state
            .viewport
            .ensure_visible(&mut state.buffer, state.cursors.primary());

        let msg = if ir_state.has_wrapped {
            "[Wrapped] Replace? (y/n/!/q)".to_string()
        } else {
            "Replace? (y/n/!/q)".to_string()
        };
        self.set_status_message(msg);
    }

    /// Finish interactive replace and show summary
    fn finish_interactive_replace(&mut self, replacements_made: usize) {
        self.interactive_replace_state = None;

        // Clear search highlights
        let state = self.active_state_mut();
        let overlay_ids: Vec<String> = state
            .overlays
            .all()
            .iter()
            .filter_map(|o| {
                o.id.as_ref().and_then(|id| {
                    if id.starts_with("search_highlight_") || id.starts_with("search_match_") {
                        Some(id.clone())
                    } else {
                        None
                    }
                })
            })
            .collect();

        for id in overlay_ids {
            state.overlays.remove_by_id(&id, &mut state.marker_list);
        }

        self.set_status_message(format!(
            "Replaced {} occurrence{}",
            replacements_made,
            if replacements_made == 1 { "" } else { "s" }
        ));
    }

    /// Smart home: toggle between line start and first non-whitespace character
    fn smart_home(&mut self) {
        let estimated_line_length = self.config.editor.estimated_line_length;
        let state = self.active_state_mut();
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

        // Get line information
        let mut iter = state
            .buffer
            .line_iterator(cursor.position, estimated_line_length);
        if let Some((line_start, line_content)) = iter.next() {
            // Find first non-whitespace character
            let first_non_ws = line_content
                .chars()
                .take_while(|c| *c != '\n')
                .position(|c| !c.is_whitespace())
                .map(|offset| line_start + offset)
                .unwrap_or(line_start);

            // Toggle: if at first non-ws, go to line start; otherwise go to first non-ws
            let new_pos = if cursor.position == first_non_ws {
                line_start
            } else {
                first_non_ws
            };

            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };

            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
        }
    }

    /// Indent the selection or current line
    fn indent_selection(&mut self) {
        let tab_size = self.config.editor.tab_size;
        let estimated_line_length = self.config.editor.estimated_line_length;
        let indent_str = " ".repeat(tab_size);

        let state = self.active_state_mut();
        // Collect lines to indent
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

        let (start_pos, end_pos) = if let Some(range) = cursor.selection_range() {
            (range.start, range.end)
        } else {
            // No selection - indent current line
            let iter = state
                .buffer
                .line_iterator(cursor.position, estimated_line_length);
            let line_start = iter.current_position();
            (line_start, cursor.position)
        };

        // Find all line starts in the range
        let buffer_len = state.buffer.len();
        let mut line_starts = Vec::new();
        let mut iter = state
            .buffer
            .line_iterator(start_pos, estimated_line_length);
        let mut current_pos = iter.current_position();
        line_starts.push(current_pos);

        // Collect all line starts by iterating through lines
        loop {
            if let Some((_, content)) = iter.next() {
                current_pos += content.len();
                if current_pos > end_pos || current_pos > buffer_len {
                    break;
                }
                let next_iter = state.buffer.line_iterator(current_pos, estimated_line_length);
                let next_start = next_iter.current_position();
                if next_start != *line_starts.last().unwrap() {
                    line_starts.push(next_start);
                }
                iter = state.buffer.line_iterator(current_pos, estimated_line_length);
            } else {
                break;
            }
        }

        if line_starts.is_empty() {
            return;
        }

        // Create insert events for each line start (in reverse order)
        let mut events = Vec::new();
        for &line_start in line_starts.iter().rev() {
            events.push(Event::Insert {
                position: line_start,
                text: indent_str.clone(),
                cursor_id,
            });
        }

        let batch = Event::Batch {
            events,
            description: "Indent selection".to_string(),
        };

        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);
        self.set_status_message(format!("Indented {} line(s)", line_starts.len()));
    }

    /// Dedent the selection or current line
    fn dedent_selection(&mut self) {
        let tab_size = self.config.editor.tab_size;
        let estimated_line_length = self.config.editor.estimated_line_length;

        let state = self.active_state_mut();
        // Collect lines to dedent
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

        let (start_pos, end_pos) = if let Some(range) = cursor.selection_range() {
            (range.start, range.end)
        } else {
            // No selection - dedent current line
            let iter = state
                .buffer
                .line_iterator(cursor.position, estimated_line_length);
            let line_start = iter.current_position();
            (line_start, cursor.position)
        };

        // Find all line starts in the range (same logic as indent)
        let buffer_len = state.buffer.len();
        let mut line_starts = Vec::new();
        let mut iter = state
            .buffer
            .line_iterator(start_pos, estimated_line_length);
        let mut current_pos = iter.current_position();
        line_starts.push(current_pos);

        loop {
            if let Some((_, content)) = iter.next() {
                current_pos += content.len();
                if current_pos > end_pos || current_pos > buffer_len {
                    break;
                }
                let next_iter = state.buffer.line_iterator(current_pos, estimated_line_length);
                let next_start = next_iter.current_position();
                if next_start != *line_starts.last().unwrap() {
                    line_starts.push(next_start);
                }
                iter = state.buffer.line_iterator(current_pos, estimated_line_length);
            } else {
                break;
            }
        }

        if line_starts.is_empty() {
            return;
        }

        // Create delete events for leading spaces (in reverse order)
        let mut events = Vec::new();
        let mut lines_dedented = 0;

        for &line_start in line_starts.iter().rev() {
            // Check how many leading spaces the line has
            let line_bytes = state.buffer.slice_bytes(line_start..buffer_len.min(line_start + tab_size + 1));
            let spaces_to_remove = line_bytes
                .iter()
                .take(tab_size)
                .take_while(|&&b| b == b' ')
                .count();

            if spaces_to_remove > 0 {
                let deleted_text = " ".repeat(spaces_to_remove);
                events.push(Event::Delete {
                    range: line_start..line_start + spaces_to_remove,
                    deleted_text,
                    cursor_id,
                });
                lines_dedented += 1;
            }
        }

        if events.is_empty() {
            self.set_status_message("No indentation to remove".to_string());
            return;
        }

        let batch = Event::Batch {
            events,
            description: "Dedent selection".to_string(),
        };

        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);
        self.set_status_message(format!("Dedented {} line(s)", lines_dedented));
    }

    /// Toggle comment on the current line or selection
    fn toggle_comment(&mut self) {
        // Determine comment prefix based on file extension
        let comment_prefix = if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer) {
            if let Some(path) = metadata.file_path() {
                match path.extension().and_then(|e| e.to_str()) {
                    Some("rs") | Some("c") | Some("cpp") | Some("h") | Some("hpp") |
                    Some("js") | Some("ts") | Some("jsx") | Some("tsx") | Some("java") |
                    Some("go") | Some("swift") | Some("kt") | Some("scala") => "// ",
                    Some("py") | Some("rb") | Some("sh") | Some("bash") | Some("zsh") |
                    Some("pl") | Some("r") | Some("yml") | Some("yaml") | Some("toml") => "# ",
                    Some("lua") | Some("sql") => "-- ",
                    Some("html") | Some("xml") => "<!-- ",
                    Some("css") | Some("scss") | Some("sass") => "/* ",
                    Some("vim") => "\" ",
                    Some("lisp") | Some("el") | Some("clj") => ";; ",
                    _ => "// "
                }
            } else {
                "// "
            }
        } else {
            "// "
        };

        let estimated_line_length = self.config.editor.estimated_line_length;

        let state = self.active_state_mut();
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

        let (start_pos, end_pos) = if let Some(range) = cursor.selection_range() {
            (range.start, range.end)
        } else {
            let iter = state
                .buffer
                .line_iterator(cursor.position, estimated_line_length);
            let line_start = iter.current_position();
            (line_start, cursor.position)
        };

        // Find all line starts in the range
        let buffer_len = state.buffer.len();
        let mut line_starts = Vec::new();
        let mut iter = state
            .buffer
            .line_iterator(start_pos, estimated_line_length);
        let mut current_pos = iter.current_position();
        line_starts.push(current_pos);

        loop {
            if let Some((_, content)) = iter.next() {
                current_pos += content.len();
                if current_pos > end_pos || current_pos > buffer_len {
                    break;
                }
                let next_iter = state.buffer.line_iterator(current_pos, estimated_line_length);
                let next_start = next_iter.current_position();
                if next_start != *line_starts.last().unwrap() {
                    line_starts.push(next_start);
                }
                iter = state.buffer.line_iterator(current_pos, estimated_line_length);
            } else {
                break;
            }
        }

        // Determine if we should comment or uncomment
        // If all lines are commented, uncomment; otherwise comment
        let all_commented = line_starts.iter().all(|&line_start| {
            let line_bytes = state.buffer.slice_bytes(line_start..buffer_len.min(line_start + comment_prefix.len() + 10));
            let line_str = String::from_utf8_lossy(&line_bytes);
            let trimmed = line_str.trim_start();
            trimmed.starts_with(comment_prefix.trim())
        });

        let mut events = Vec::new();

        if all_commented {
            // Uncomment: remove comment prefix from each line
            for &line_start in line_starts.iter().rev() {
                let line_bytes = state.buffer.slice_bytes(line_start..buffer_len.min(line_start + 100));
                let line_str = String::from_utf8_lossy(&line_bytes);

                // Find where the comment prefix starts (after leading whitespace)
                let leading_ws: usize = line_str.chars().take_while(|c| c.is_whitespace() && *c != '\n').map(|c| c.len_utf8()).sum();
                let rest = &line_str[leading_ws..];

                if rest.starts_with(comment_prefix.trim()) {
                    let remove_len = if rest.starts_with(comment_prefix) {
                        comment_prefix.len()
                    } else {
                        comment_prefix.trim().len()
                    };
                    let deleted_text = String::from_utf8_lossy(&state.buffer.slice_bytes(line_start + leading_ws..line_start + leading_ws + remove_len)).to_string();
                    events.push(Event::Delete {
                        range: (line_start + leading_ws)..(line_start + leading_ws + remove_len),
                        deleted_text,
                        cursor_id,
                    });
                }
            }
        } else {
            // Comment: add comment prefix to each line
            for &line_start in line_starts.iter().rev() {
                events.push(Event::Insert {
                    position: line_start,
                    text: comment_prefix.to_string(),
                    cursor_id,
                });
            }
        }

        if events.is_empty() {
            return;
        }

        let action_desc = if all_commented { "Uncomment" } else { "Comment" };
        let batch = Event::Batch {
            events,
            description: format!("{} lines", action_desc),
        };

        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);
        self.set_status_message(format!("{}ed {} line(s)", action_desc, line_starts.len()));
    }

    /// Go to matching bracket
    fn goto_matching_bracket(&mut self) {
        let state = self.active_state_mut();
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

        let pos = cursor.position;
        if pos >= state.buffer.len() {
            self.set_status_message("No bracket at cursor".to_string());
            return;
        }

        let bytes = state.buffer.slice_bytes(pos..pos + 1);
        if bytes.is_empty() {
            self.set_status_message("No bracket at cursor".to_string());
            return;
        }

        let ch = bytes[0] as char;
        let (opening, closing, forward) = match ch {
            '(' => ('(', ')', true),
            ')' => ('(', ')', false),
            '[' => ('[', ']', true),
            ']' => ('[', ']', false),
            '{' => ('{', '}', true),
            '}' => ('{', '}', false),
            '<' => ('<', '>', true),
            '>' => ('<', '>', false),
            _ => {
                self.set_status_message("No bracket at cursor".to_string());
                return;
            }
        };

        // Find matching bracket
        let buffer_len = state.buffer.len();
        let mut depth = 1;
        let matching_pos = if forward {
            let mut search_pos = pos + 1;
            let mut found = None;
            while search_pos < buffer_len && depth > 0 {
                let b = state.buffer.slice_bytes(search_pos..search_pos + 1);
                if !b.is_empty() {
                    let c = b[0] as char;
                    if c == opening {
                        depth += 1;
                    } else if c == closing {
                        depth -= 1;
                        if depth == 0 {
                            found = Some(search_pos);
                        }
                    }
                }
                search_pos += 1;
            }
            found
        } else {
            let mut search_pos = pos.saturating_sub(1);
            let mut found = None;
            loop {
                let b = state.buffer.slice_bytes(search_pos..search_pos + 1);
                if !b.is_empty() {
                    let c = b[0] as char;
                    if c == closing {
                        depth += 1;
                    } else if c == opening {
                        depth -= 1;
                        if depth == 0 {
                            found = Some(search_pos);
                            break;
                        }
                    }
                }
                if search_pos == 0 {
                    break;
                }
                search_pos -= 1;
            }
            found
        };

        if let Some(new_pos) = matching_pos {
            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
        } else {
            self.set_status_message("No matching bracket found".to_string());
        }
    }

    /// Jump to next error/diagnostic
    fn jump_to_next_error(&mut self) {
        let state = self.active_state_mut();
        let cursor_pos = state.cursors.primary().position;
        let cursor_id = state.cursors.primary_id();
        let cursor = state.cursors.primary().clone();

        // Get all diagnostic overlay positions
        let mut diagnostic_positions: Vec<usize> = state
            .overlays
            .all()
            .iter()
            .filter_map(|overlay| {
                // Only consider LSP diagnostics
                if overlay
                    .id
                    .as_ref()
                    .map(|id| id.starts_with("lsp-diagnostic-"))
                    .unwrap_or(false)
                {
                    Some(overlay.range(&state.marker_list).start)
                } else {
                    None
                }
            })
            .collect();

        if diagnostic_positions.is_empty() {
            self.set_status_message("No diagnostics in current buffer".to_string());
            return;
        }

        // Sort positions
        diagnostic_positions.sort_unstable();
        diagnostic_positions.dedup();

        // Find next diagnostic after cursor position
        let next_pos = diagnostic_positions
            .iter()
            .find(|&&pos| pos > cursor_pos)
            .or_else(|| diagnostic_positions.first()) // Wrap around
            .copied();

        if let Some(new_pos) = next_pos {
            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);

            // Show diagnostic message in status bar
            let state = self.active_state();
            if let Some(msg) = state.overlays.all().iter().find_map(|overlay| {
                let range = overlay.range(&state.marker_list);
                if range.start == new_pos
                    && overlay
                        .id
                        .as_ref()
                        .map(|id| id.starts_with("lsp-diagnostic-"))
                        .unwrap_or(false)
                {
                    overlay.message.clone()
                } else {
                    None
                }
            }) {
                self.set_status_message(msg);
            }
        }
    }

    /// Jump to previous error/diagnostic
    fn jump_to_previous_error(&mut self) {
        let state = self.active_state_mut();
        let cursor_pos = state.cursors.primary().position;
        let cursor_id = state.cursors.primary_id();
        let cursor = state.cursors.primary().clone();

        // Get all diagnostic overlay positions
        let mut diagnostic_positions: Vec<usize> = state
            .overlays
            .all()
            .iter()
            .filter_map(|overlay| {
                // Only consider LSP diagnostics
                if overlay
                    .id
                    .as_ref()
                    .map(|id| id.starts_with("lsp-diagnostic-"))
                    .unwrap_or(false)
                {
                    Some(overlay.range(&state.marker_list).start)
                } else {
                    None
                }
            })
            .collect();

        if diagnostic_positions.is_empty() {
            self.set_status_message("No diagnostics in current buffer".to_string());
            return;
        }

        // Sort positions
        diagnostic_positions.sort_unstable();
        diagnostic_positions.dedup();

        // Find previous diagnostic before cursor position
        let prev_pos = diagnostic_positions
            .iter()
            .rev()
            .find(|&&pos| pos < cursor_pos)
            .or_else(|| diagnostic_positions.last()) // Wrap around
            .copied();

        if let Some(new_pos) = prev_pos {
            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);

            // Show diagnostic message in status bar
            let state = self.active_state();
            if let Some(msg) = state.overlays.all().iter().find_map(|overlay| {
                let range = overlay.range(&state.marker_list);
                if range.start == new_pos
                    && overlay
                        .id
                        .as_ref()
                        .map(|id| id.starts_with("lsp-diagnostic-"))
                        .unwrap_or(false)
                {
                    overlay.message.clone()
                } else {
                    None
                }
            }) {
                self.set_status_message(msg);
            }
        }
    }

    /// Toggle macro recording for the given register
    fn toggle_macro_recording(&mut self, key: char) {
        if let Some(state) = &self.macro_recording {
            if state.key == key {
                // Stop recording
                self.stop_macro_recording();
            } else {
                // Recording to a different key, stop current and start new
                self.stop_macro_recording();
                self.start_macro_recording(key);
            }
        } else {
            // Start recording
            self.start_macro_recording(key);
        }
    }

    /// Start recording a macro
    fn start_macro_recording(&mut self, key: char) {
        self.macro_recording = Some(MacroRecordingState {
            key,
            actions: Vec::new(),
        });
        self.set_status_message(format!("Recording macro '{}' (press Ctrl+Shift+R {} to stop)", key, key));
    }

    /// Stop recording and save the macro
    fn stop_macro_recording(&mut self) {
        if let Some(state) = self.macro_recording.take() {
            let action_count = state.actions.len();
            self.macros.insert(state.key, state.actions);
            self.set_status_message(format!(
                "Macro '{}' saved ({} actions)",
                state.key, action_count
            ));
        } else {
            self.set_status_message("Not recording a macro".to_string());
        }
    }

    /// Play back a recorded macro
    fn play_macro(&mut self, key: char) {
        if let Some(actions) = self.macros.get(&key).cloned() {
            if actions.is_empty() {
                self.set_status_message(format!("Macro '{}' is empty", key));
                return;
            }

            // Temporarily disable recording to avoid recording the playback
            let was_recording = self.macro_recording.take();

            let action_count = actions.len();
            for action in actions {
                let _ = self.handle_action(action);
            }

            // Restore recording state
            self.macro_recording = was_recording;

            self.set_status_message(format!("Played macro '{}' ({} actions)", key, action_count));
        } else {
            self.set_status_message(format!("No macro recorded for '{}'", key));
        }
    }

    /// Record an action to the current macro (if recording)
    fn record_macro_action(&mut self, action: &Action) {
        if let Some(state) = &mut self.macro_recording {
            // Don't record macro control actions themselves
            match action {
                Action::StartMacroRecording
                | Action::StopMacroRecording
                | Action::PlayMacro(_)
                | Action::ToggleMacroRecording(_)
                | Action::ShowMacro(_)
                | Action::ListMacros => {}
                _ => {
                    state.actions.push(action.clone());
                }
            }
        }
    }

    /// Show a macro in a buffer as JSON
    fn show_macro_in_buffer(&mut self, key: char) {
        if let Some(actions) = self.macros.get(&key) {
            // Serialize the macro to JSON
            let json = match serde_json::to_string_pretty(actions) {
                Ok(json) => json,
                Err(e) => {
                    self.set_status_message(format!("Failed to serialize macro: {}", e));
                    return;
                }
            };

            // Create header with macro info
            let content = format!(
                "// Macro '{}' ({} actions)\n// This buffer can be saved as a .json file for persistence\n\n{}",
                key,
                actions.len(),
                json
            );

            // Create a new buffer for the macro
            let buffer_id = BufferId(self.next_buffer_id);
            self.next_buffer_id += 1;

            let state = EditorState::new(
                self.terminal_width.into(),
                self.terminal_height.into(),
                self.config.editor.large_file_threshold_bytes as usize,
            );

            self.buffers.insert(buffer_id, state);
            self.event_logs.insert(buffer_id, EventLog::new());

            // Set buffer content
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.buffer = crate::text_buffer::Buffer::from_str(
                    &content,
                    self.config.editor.large_file_threshold_bytes as usize,
                );
            }

            // Set metadata
            let metadata = BufferMetadata {
                kind: BufferKind::Virtual {
                    mode: "macro-view".to_string(),
                },
                display_name: format!("*Macro {}*", key),
                lsp_enabled: false,
                lsp_disabled_reason: Some("Virtual macro buffer".to_string()),
                read_only: false, // Allow editing for saving
            };
            self.buffer_metadata.insert(buffer_id, metadata);

            // Switch to the new buffer
            self.active_buffer = buffer_id;
            self.set_status_message(format!(
                "Macro '{}' shown in buffer ({} actions) - save as .json for persistence",
                key,
                actions.len()
            ));
        } else {
            self.set_status_message(format!("No macro recorded for '{}'", key));
        }
    }

    /// List all recorded macros in a buffer
    fn list_macros_in_buffer(&mut self) {
        if self.macros.is_empty() {
            self.set_status_message("No macros recorded".to_string());
            return;
        }

        // Build a summary of all macros
        let mut content = String::from("// Recorded Macros\n// Use ShowMacro(key) to see details\n\n");

        let mut keys: Vec<char> = self.macros.keys().copied().collect();
        keys.sort();

        for key in keys {
            if let Some(actions) = self.macros.get(&key) {
                content.push_str(&format!("Macro '{}': {} actions\n", key, actions.len()));

                // Show first few actions as preview
                for (i, action) in actions.iter().take(5).enumerate() {
                    content.push_str(&format!("  {}. {:?}\n", i + 1, action));
                }
                if actions.len() > 5 {
                    content.push_str(&format!("  ... and {} more actions\n", actions.len() - 5));
                }
                content.push('\n');
            }
        }

        // Create a new buffer for the macro list
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let state = EditorState::new(
            self.terminal_width.into(),
            self.terminal_height.into(),
            self.config.editor.large_file_threshold_bytes as usize,
        );

        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Set buffer content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer = crate::text_buffer::Buffer::from_str(
                &content,
                self.config.editor.large_file_threshold_bytes as usize,
            );
        }

        // Set metadata
        let metadata = BufferMetadata {
            kind: BufferKind::Virtual {
                mode: "macro-list".to_string(),
            },
            display_name: "*Macros*".to_string(),
            lsp_enabled: false,
            lsp_disabled_reason: Some("Virtual macro list buffer".to_string()),
            read_only: true,
        };
        self.buffer_metadata.insert(buffer_id, metadata);

        // Switch to the new buffer
        self.active_buffer = buffer_id;
        self.set_status_message(format!(
            "Showing {} recorded macro(s)",
            self.macros.len()
        ));
    }

    /// Set a bookmark at the current position
    fn set_bookmark(&mut self, key: char) {
        let buffer_id = self.active_buffer;
        let position = self.active_state().cursors.primary().position;
        self.bookmarks.insert(key, Bookmark { buffer_id, position });
        self.set_status_message(format!("Bookmark '{}' set", key));
    }

    /// Jump to a bookmark
    fn jump_to_bookmark(&mut self, key: char) {
        if let Some(bookmark) = self.bookmarks.get(&key).cloned() {
            // Switch to the buffer if needed
            if bookmark.buffer_id != self.active_buffer {
                if self.buffers.contains_key(&bookmark.buffer_id) {
                    self.active_buffer = bookmark.buffer_id;
                } else {
                    self.set_status_message(format!("Bookmark '{}': buffer no longer exists", key));
                    self.bookmarks.remove(&key);
                    return;
                }
            }

            // Move cursor to bookmark position
            let state = self.active_state_mut();
            let cursor_id = state.cursors.primary_id();
            let old_pos = state.cursors.primary().position;
            let new_pos = bookmark.position.min(state.buffer.len());

            let event = Event::MoveCursor {
                cursor_id,
                old_position: old_pos,
                new_position: new_pos,
                old_anchor: state.cursors.primary().anchor,
                new_anchor: None,
                old_sticky_column: state.cursors.primary().sticky_column,
                new_sticky_column: 0,
            };

            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
            self.set_status_message(format!("Jumped to bookmark '{}'", key));
        } else {
            self.set_status_message(format!("Bookmark '{}' not set", key));
        }
    }

    /// Clear a bookmark
    fn clear_bookmark(&mut self, key: char) {
        if self.bookmarks.remove(&key).is_some() {
            self.set_status_message(format!("Bookmark '{}' cleared", key));
        } else {
            self.set_status_message(format!("Bookmark '{}' not set", key));
        }
    }

    /// List all bookmarks
    fn list_bookmarks(&mut self) {
        if self.bookmarks.is_empty() {
            self.set_status_message("No bookmarks set".to_string());
            return;
        }

        let mut bookmark_list: Vec<_> = self.bookmarks.iter().collect();
        bookmark_list.sort_by_key(|(k, _)| *k);

        let list_str: String = bookmark_list
            .iter()
            .map(|(k, bm)| {
                let buffer_name = self
                    .buffer_metadata
                    .get(&bm.buffer_id)
                    .map(|m| m.display_name.as_str())
                    .unwrap_or("unknown");
                format!("'{}': {} @ {}", k, buffer_name, bm.position)
            })
            .collect::<Vec<_>>()
            .join(", ");

        self.set_status_message(format!("Bookmarks: {}", list_str));
    }

    /// Save search and replace histories to disk
    /// Called on shutdown to persist history across sessions
    pub fn save_histories(&self) {
        // Save search history
        if let Ok(path) = crate::input_history::get_search_history_path() {
            if let Err(e) = self.search_history.save_to_file(&path) {
                tracing::warn!("Failed to save search history: {}", e);
            } else {
                tracing::debug!("Saved search history to {:?}", path);
            }
        }

        // Save replace history
        if let Ok(path) = crate::input_history::get_replace_history_path() {
            if let Err(e) = self.replace_history.save_to_file(&path) {
                tracing::warn!("Failed to save replace history: {}", e);
            } else {
                tracing::debug!("Saved replace history to {:?}", path);
            }
        }
    }
}

impl Drop for Editor {
    fn drop(&mut self) {
        // Save histories on shutdown
        self.save_histories();
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
    let code = match remaining.to_uppercase().as_str() {
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
        s if s.len() == 1 => {
            // Single character
            let c = s.chars().next()?;
            KeyCode::Char(c.to_ascii_lowercase())
        }
        _ => return None,
    };

    Some((code, modifiers))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_editor_new() {
        let config = Config::default();
        let editor = Editor::new(config, 80, 24).unwrap();

        assert_eq!(editor.buffers.len(), 1);
        assert!(!editor.should_quit());
    }

    #[test]
    fn test_new_buffer() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        let id = editor.new_buffer();
        assert_eq!(editor.buffers.len(), 2);
        assert_eq!(editor.active_buffer, id);
    }

    #[test]
    fn test_clipboard() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Manually set clipboard
        editor.clipboard = "test".to_string();

        // Paste should work
        editor.paste();

        let content = editor.active_state().buffer.to_string();
        assert_eq!(content, "test");
    }

    #[test]
    fn test_action_to_events_insert_char() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // These actions should return None (not yet implemented)
        assert!(editor.action_to_events(Action::Save).is_none());
        assert!(editor.action_to_events(Action::Quit).is_none());
        assert!(editor.action_to_events(Action::Undo).is_none());
    }

    #[test]
    fn test_action_to_events_delete_backward() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        use crate::event::CursorId;

        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        // Should have events for the two secondary cursors
        assert_eq!(events.len(), 2);

        for event in &events {
            match event {
                Event::RemoveCursor { cursor_id, .. } => {
                    // Should not be the first cursor (the one we're keeping)
                    assert_ne!(*cursor_id, first_id);
                }
                _ => panic!("Expected RemoveCursor event"),
            }
        }
    }

    #[test]
    fn test_action_to_events_scroll() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // None action should return None
        let events = editor.action_to_events(Action::None);
        assert!(events.is_none());
    }

    #[test]
    fn test_lsp_incremental_insert_generates_correct_range() {
        // Test that insert events generate correct incremental LSP changes
        // with zero-width ranges at the insertion point
        use crate::text_buffer::Buffer;

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
        use crate::text_buffer::Buffer;

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
        use crate::text_buffer::Buffer;

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        assert_eq!(search_state.matches.len(), 3, "Should find all 3 matches case-insensitively");

        // Test case-sensitive search
        editor.search_case_sensitive = true;
        editor.perform_search("hello");

        let search_state = editor.search_state.as_ref().unwrap();
        assert_eq!(search_state.matches.len(), 1, "Should find only 1 exact match");
        assert_eq!(search_state.matches[0], 6, "Should find 'hello' at position 6");
    }

    #[test]
    fn test_search_whole_word() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        assert_eq!(search_state.matches.len(), 5, "Should find 'test' in all occurrences");

        // Test whole word match
        editor.search_whole_word = true;
        editor.perform_search("test");

        let search_state = editor.search_state.as_ref().unwrap();
        assert_eq!(search_state.matches.len(), 2, "Should find only whole word 'test'");
        assert_eq!(search_state.matches[0], 0, "First match at position 0");
        assert_eq!(search_state.matches[1], 27, "Second match at position 27");
    }

    #[test]
    fn test_bookmarks() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        assert_eq!(Action::from_str("smart_home", &args), Some(Action::SmartHome));
        assert_eq!(Action::from_str("indent_selection", &args), Some(Action::IndentSelection));
        assert_eq!(Action::from_str("dedent_selection", &args), Some(Action::DedentSelection));
        assert_eq!(Action::from_str("toggle_comment", &args), Some(Action::ToggleComment));
        assert_eq!(Action::from_str("goto_matching_bracket", &args), Some(Action::GoToMatchingBracket));
        assert_eq!(Action::from_str("list_bookmarks", &args), Some(Action::ListBookmarks));
        assert_eq!(Action::from_str("toggle_search_case_sensitive", &args), Some(Action::ToggleSearchCaseSensitive));
        assert_eq!(Action::from_str("toggle_search_whole_word", &args), Some(Action::ToggleSearchWholeWord));

        // Test bookmark actions with arguments
        let mut args_with_char = HashMap::new();
        args_with_char.insert("char".to_string(), json!("5"));
        assert_eq!(Action::from_str("set_bookmark", &args_with_char), Some(Action::SetBookmark('5')));
        assert_eq!(Action::from_str("jump_to_bookmark", &args_with_char), Some(Action::JumpToBookmark('5')));
        assert_eq!(Action::from_str("clear_bookmark", &args_with_char), Some(Action::ClearBookmark('5')));
    }

    #[test]
    fn test_keybinding_new_defaults() {
        use crossterm::event::{KeyEvent, KeyEventKind, KeyEventState};

        // Test that new keybindings are properly registered
        let config = Config::default();
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
    /// 1. apply_rename_batch_to_buffer() applies the batch to the buffer
    /// 2. Then calls notify_lsp_change() which calls collect_lsp_changes()
    /// 3. collect_lsp_changes() converts byte positions to LSP positions using
    ///    the CURRENT buffer state
    ///
    /// But the byte positions in the events are relative to the ORIGINAL buffer,
    /// not the modified one! This causes LSP to receive wrong positions.
    #[test]
    fn test_lsp_rename_didchange_positions_bug() {
        use crate::text_buffer::Buffer;

        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

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

        // Now apply the batch (this is what apply_rename_batch_to_buffer does)
        editor.active_state_mut().apply(&batch);

        // BUG DEMONSTRATION: Calculate LSP positions AFTER applying batch
        // This is what happens when notify_lsp_change is called after state.apply()
        let lsp_changes_after = editor.collect_lsp_changes(&batch);

        // Verify buffer was correctly modified
        let final_content = editor.active_state().buffer.to_string();
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
        use crate::text_buffer::Buffer;

        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Set buffer content: "fn foo(val: i32) {\n    val + 1\n}\n"
        // Line 0: positions 0-19 (includes newline)
        // Line 1: positions 19-31 (includes newline)
        let initial = "fn foo(val: i32) {\n    val + 1\n}\n";
        editor.active_state_mut().buffer = Buffer::from_str(initial, 1024 * 1024);

        // Position cursor at the second "val" (position 23 = 'v' of "val" on line 1)
        let original_cursor_pos = 23;
        editor.active_state_mut().cursors.primary_mut().position = original_cursor_pos;

        // Verify cursor is at the right position
        let text_at_cursor = editor.active_state().buffer.to_string()
            [original_cursor_pos..original_cursor_pos + 3]
            .to_string();
        assert_eq!(text_at_cursor, "val", "Cursor should be at 'val'");

        // Simulate LSP rename batch: rename "val" to "value" in two places
        // Applied in reverse order (from end of file to start)
        let cursor_id = editor.active_state().cursors.primary_id();
        let buffer_id = editor.active_buffer;

        let batch = Event::Batch {
            events: vec![
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
            ],
            description: "LSP Rename".to_string(),
        };

        // Apply the rename batch (this should preserve cursor position)
        editor.apply_rename_batch_to_buffer(buffer_id, batch).unwrap();

        // Verify buffer was correctly modified
        let final_content = editor.active_state().buffer.to_string();
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
        use crate::text_buffer::Buffer;

        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Initial content: "fn foo(val: i32) {\n    val + 1\n}\n"
        let initial = "fn foo(val: i32) {\n    val + 1\n}\n";
        editor.active_state_mut().buffer = Buffer::from_str(initial, 1024 * 1024);

        let cursor_id = editor.active_state().cursors.primary_id();
        let buffer_id = editor.active_buffer;

        // === FIRST RENAME: "val" -> "value" ===
        // Create batch for first rename (applied in reverse order)
        let batch1 = Event::Batch {
            events: vec![
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
            ],
            description: "LSP Rename 1".to_string(),
        };

        // Collect LSP changes BEFORE applying (this is the fix)
        let lsp_changes1 = editor.collect_lsp_changes(&batch1);

        // Verify first rename LSP positions are correct
        assert_eq!(lsp_changes1.len(), 4, "First rename should have 4 LSP changes");

        // First delete should be at line 1, char 4-7 (second "val")
        let first_del = &lsp_changes1[0];
        let first_del_range = first_del.range.unwrap();
        assert_eq!(first_del_range.start.line, 1, "First delete line");
        assert_eq!(first_del_range.start.character, 4, "First delete start char");
        assert_eq!(first_del_range.end.character, 7, "First delete end char");

        // Apply first rename
        editor.apply_rename_batch_to_buffer(buffer_id, batch1).unwrap();

        // Verify buffer after first rename
        let after_first = editor.active_state().buffer.to_string();
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

        // Create batch for second rename
        let batch2 = Event::Batch {
            events: vec![
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
            ],
            description: "LSP Rename 2".to_string(),
        };

        // Collect LSP changes BEFORE applying (this is the fix)
        let lsp_changes2 = editor.collect_lsp_changes(&batch2);

        // Verify second rename LSP positions are correct
        // THIS IS WHERE THE BUG WOULD MANIFEST - if positions are wrong,
        // the LSP server would report "No references found at position"
        assert_eq!(lsp_changes2.len(), 4, "Second rename should have 4 LSP changes");

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

        // Apply second rename
        editor.apply_rename_batch_to_buffer(buffer_id, batch2).unwrap();

        // Verify buffer after second rename
        let after_second = editor.active_state().buffer.to_string();
        assert_eq!(
            after_second, "fn foo(x: i32) {\n    x + 1\n}\n",
            "After second rename"
        );
    }
}

