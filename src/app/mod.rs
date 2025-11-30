mod async_messages;
mod file_explorer;
pub mod file_open;
mod file_open_input;
mod help;
mod input;
mod plugin_commands;
mod render;
pub mod script_control;
pub mod session;
mod types;

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
    LspProgressInfo, MacroRecordingState, MouseState, SearchState, DEFAULT_BACKGROUND_FILE,
};
use crate::config::Config;
use crate::input::actions::action_to_events as convert_action_to_events;
use crate::input::buffer_mode::ModeRegistry;
use crate::input::command_registry::CommandRegistry;
use crate::input::commands::Suggestion;
use crate::input::keybindings::{Action, KeyContext, KeybindingResolver};
use crate::input::multi_cursor::{
    add_cursor_above, add_cursor_at_next_match, add_cursor_below, AddCursorResult,
};
use crate::input::position_history::PositionHistory;
use crate::model::event::{CursorId, Event, EventLog, SplitDirection, SplitId};
use crate::services::async_bridge::{AsyncBridge, AsyncMessage};
use crate::services::fs::{FsBackend, FsManager, LocalFsBackend};
use crate::services::lsp::client::LspServerConfig;
use crate::services::lsp::manager::{detect_language, LspManager, LspSpawnResult};
use crate::services::plugins::api::{BufferSavedDiff, PluginCommand};
use crate::services::plugins::thread::PluginThreadHandle;
use crate::services::recovery::{RecoveryConfig, RecoveryService};
use crate::state::EditorState;
use crate::view::file_tree::{FileTree, FileTreeView};
use crate::view::prompt::{Prompt, PromptType};
use crate::view::split::{SplitManager, SplitViewState};
use crate::view::ui::{
    FileExplorerRenderer, SplitRenderer, StatusBarRenderer, SuggestionsRenderer,
};
use crossterm::event::{KeyCode, KeyModifiers};
use lsp_types::{Position, Range as LspRange, TextDocumentContentChangeEvent};
use ratatui::{
    layout::{Constraint, Direction, Layout},
    Frame,
};
use std::collections::{HashMap, HashSet};
use std::io;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

// Re-export BufferId from event module for backward compatibility
pub use self::types::{BufferKind, BufferMetadata, HoverTarget};
pub use crate::model::event::BufferId;

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

    /// Grammar registry for TextMate syntax highlighting
    grammar_registry: std::sync::Arc<crate::primitives::grammar_registry::GrammarRegistry>,

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

    /// Status message (shown in status bar)
    status_message: Option<String>,

    /// Plugin-provided status message (displayed alongside the core status)
    plugin_status_message: Option<String>,

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

    /// File explorer width as percentage (0.0 to 1.0)
    /// This is the runtime value that can be modified by dragging the border
    file_explorer_width_percent: f32,

    /// Whether mouse capture is enabled
    mouse_enabled: bool,

    /// Current keybinding context
    key_context: KeyContext,

    /// Menu state (active menu, highlighted item)
    menu_state: crate::view::ui::MenuState,

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

    /// Hover symbol overlay handle (for removal)
    hover_symbol_overlay: Option<crate::view::overlay::OverlayHandle>,

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

    /// Cached layout areas from last render (for mouse hit testing)
    cached_layout: CachedLayout,

    /// Command registry for dynamic commands
    command_registry: Arc<RwLock<CommandRegistry>>,

    /// TypeScript plugin thread handle
    ts_plugin_manager: Option<PluginThreadHandle>,

    /// Track which byte ranges have been seen per buffer (for lines_changed optimization)
    /// Maps buffer_id -> set of (byte_start, byte_end) ranges that have been processed
    /// Using byte ranges instead of line numbers makes this agnostic to line number shifts
    seen_byte_ranges: HashMap<BufferId, std::collections::HashSet<(usize, usize)>>,

    /// Named panel IDs mapping (for idempotent panel operations)
    /// Maps panel ID (e.g., "diagnostics") to buffer ID
    panel_ids: HashMap<String, BufferId>,

    /// Search history (for search and find operations)
    search_history: crate::input::input_history::InputHistory,

    /// Replace history (for replace operations)
    replace_history: crate::input::input_history::InputHistory,

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

    /// Pending plugin action receivers (for async action execution)
    pending_plugin_actions: Vec<(
        String,
        crate::services::plugins::thread::oneshot::Receiver<anyhow::Result<()>>,
    )>,

    /// Flag set by plugin commands that need a render (e.g., RefreshLines)
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

    /// File watcher for auto-revert functionality
    file_watcher: Option<notify::RecommendedWatcher>,

    /// Directories currently being watched (to avoid duplicate watches)
    /// We watch directories instead of files to handle atomic saves (temp+rename)
    watched_dirs: HashSet<PathBuf>,

    /// Last known modification times for watched files (for conflict detection)
    /// Maps file path to last known modification time
    file_mod_times: HashMap<PathBuf, std::time::SystemTime>,

    /// Tracks rapid file change events for debouncing
    /// Maps file path to (last event time, event count)
    file_rapid_change_counts: HashMap<PathBuf, (std::time::Instant, u32)>,

    /// File open dialog state (when PromptType::OpenFile is active)
    file_open_state: Option<file_open::FileOpenState>,

    /// Cached layout for file browser (for mouse hit testing)
    file_browser_layout: Option<crate::view::ui::FileBrowserLayout>,

    /// Recovery service for auto-save and crash recovery
    recovery_service: RecoveryService,

    /// Last auto-save time for rate limiting
    last_auto_save: std::time::Instant,
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
        Self::with_options(config, width, height, working_dir, None, true)
    }

    /// Create a new editor with plugins disabled
    pub fn with_plugins_disabled(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
    ) -> io::Result<Self> {
        Self::with_options(config, width, height, working_dir, None, false)
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
        Self::with_options(config, width, height, working_dir, Some(fs_backend), true)
    }

    /// Create a new editor with custom options
    /// This is primarily used for testing with slow or mock backends
    /// to verify editor behavior under various I/O conditions
    fn with_options(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        fs_backend: Option<Arc<dyn FsBackend>>,
        enable_plugins: bool,
    ) -> io::Result<Self> {
        tracing::info!("Editor::new called with width={}, height={}", width, height);

        // Use provided working_dir or capture from environment
        let working_dir = working_dir
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        // Canonicalize working_dir to resolve symlinks and normalize path components
        // This ensures consistent path comparisons throughout the editor
        let working_dir = working_dir.canonicalize().unwrap_or_else(|_| working_dir);

        // Load theme from config
        let theme = crate::view::theme::Theme::from_name(&config.theme);

        // Load grammar registry for TextMate syntax highlighting
        let grammar_registry =
            Arc::new(crate::primitives::grammar_registry::GrammarRegistry::load());
        tracing::info!(
            "Loaded grammar registry with {} syntaxes",
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

        // Initialize TypeScript plugin thread (skip if plugins are disabled)
        let ts_plugin_manager = if enable_plugins {
            match PluginThreadHandle::spawn(Arc::clone(&command_registry)) {
                Ok(handle) => Some(handle),
                Err(e) => {
                    tracing::error!("Failed to spawn TypeScript plugin thread: {}", e);
                    // In debug/test builds, panic to surface the error
                    #[cfg(debug_assertions)]
                    panic!("TypeScript plugin thread creation failed: {}", e);
                    #[cfg(not(debug_assertions))]
                    None
                }
            }
        } else {
            tracing::info!("Plugins disabled via --no-plugins flag");
            None
        };

        // Load TypeScript plugins from multiple directories:
        // 1. Next to the executable (for cargo-dist installations)
        // 2. In the working directory (for development/local usage)
        if let Some(ref manager) = ts_plugin_manager {
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

            if plugin_dirs.is_empty() {
                tracing::debug!(
                    "No plugins directory found next to executable or in working dir: {:?}",
                    working_dir
                );
            }

            // Load from all found plugin directories
            for plugin_dir in plugin_dirs {
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
            }
        }

        // Extract config values before moving config into the struct
        let file_explorer_width = config.file_explorer.width;
        let recovery_enabled = config.editor.recovery_enabled;
        let auto_save_interval_secs = config.editor.auto_save_interval_secs;

        Ok(Editor {
            buffers,
            active_buffer: buffer_id,
            event_logs,
            next_buffer_id: 1,
            config,
            grammar_registry,
            theme,
            ansi_background: None,
            ansi_background_path: None,
            background_fade: crate::primitives::ansi_background::DEFAULT_BACKGROUND_FADE,
            keybindings,
            clipboard: crate::services::clipboard::Clipboard::new(),
            should_quit: false,
            status_message: None,
            plugin_status_message: None,
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
            file_explorer_width_percent: file_explorer_width,
            mouse_enabled: true,
            key_context: KeyContext::Normal,
            menu_state: crate::view::ui::MenuState::new(),
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
            hover_symbol_overlay: None,
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
            cached_layout: CachedLayout::default(),
            command_registry,
            ts_plugin_manager,
            seen_byte_ranges: HashMap::new(),
            panel_ids: HashMap::new(),
            search_history: {
                // Load search history from disk if available
                match crate::input::input_history::get_search_history_path() {
                    Ok(path) => crate::input::input_history::InputHistory::load_from_file(&path)
                        .unwrap_or_else(|e| {
                            tracing::warn!("Failed to load search history: {}", e);
                            crate::input::input_history::InputHistory::new()
                        }),
                    Err(e) => {
                        tracing::warn!("Could not determine search history path: {}", e);
                        crate::input::input_history::InputHistory::new()
                    }
                }
            },
            replace_history: {
                // Load replace history from disk if available
                match crate::input::input_history::get_replace_history_path() {
                    Ok(path) => crate::input::input_history::InputHistory::load_from_file(&path)
                        .unwrap_or_else(|e| {
                            tracing::warn!("Failed to load replace history: {}", e);
                            crate::input::input_history::InputHistory::new()
                        }),
                    Err(e) => {
                        tracing::warn!("Could not determine replace history path: {}", e);
                        crate::input::input_history::InputHistory::new()
                    }
                }
            },
            lsp_progress: std::collections::HashMap::new(),
            lsp_server_statuses: std::collections::HashMap::new(),
            lsp_window_messages: Vec::new(),
            lsp_log_messages: Vec::new(),
            diagnostic_result_ids: HashMap::new(),
            event_broadcaster: crate::model::control_event::EventBroadcaster::default(),
            bookmarks: HashMap::new(),
            search_case_sensitive: true,
            search_whole_word: false,
            search_use_regex: false,
            search_confirm_each: false,
            macros: HashMap::new(),
            macro_recording: None,
            last_macro_register: None,
            pending_plugin_actions: Vec::new(),
            plugin_render_requested: false,
            chord_state: Vec::new(),
            pending_lsp_confirmation: None,
            pending_close_buffer: None,
            auto_revert_enabled: true,
            file_watcher: None,
            watched_dirs: HashSet::new(),
            file_mod_times: HashMap::new(),
            file_rapid_change_counts: HashMap::new(),
            file_open_state: None,
            file_browser_layout: None,
            recovery_service: {
                let recovery_config = RecoveryConfig {
                    enabled: recovery_enabled,
                    auto_save_interval_secs,
                    ..RecoveryConfig::default()
                };
                RecoveryService::with_config(recovery_config).unwrap_or_else(|e| {
                    tracing::warn!("Failed to initialize recovery service: {}", e);
                    RecoveryService::default()
                })
            },
            last_auto_save: std::time::Instant::now(),
        })
    }

    /// Get a reference to the event broadcaster
    pub fn event_broadcaster(&self) -> &crate::model::control_event::EventBroadcaster {
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
    fn send_plugin_response(&self, response: crate::services::plugins::api::PluginResponse) {
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

    /// Load an ANSI background image from a user-provided path
    fn load_ansi_background(&mut self, input: &str) -> io::Result<()> {
        let trimmed = input.trim();

        if trimmed.is_empty() {
            self.ansi_background = None;
            self.ansi_background_path = None;
            self.set_status_message("Background cleared".to_string());
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
        self.set_status_message(format!("Background set to {}", canonical.display()));

        Ok(())
    }

    /// Open a file and return its buffer ID
    ///
    /// If the file doesn't exist, creates an unsaved buffer with that filename.
    /// Saving the buffer will create the file.
    pub fn open_file(&mut self, path: &Path) -> io::Result<BufferId> {
        // Determine if we're opening a non-existent file (for creating new files)
        let file_exists = path.exists();

        // Canonicalize the path to resolve symlinks and normalize path components
        // This ensures consistent path representation throughout the editor
        // For non-existent files, we need to canonicalize the parent directory and append the filename
        let canonical_path = if file_exists {
            path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
        } else {
            // For non-existent files, canonicalize parent dir and append filename
            if let Some(parent) = path.parent() {
                let canonical_parent = if parent.as_os_str().is_empty() {
                    // No parent means just a filename, use working dir
                    self.working_dir.clone()
                } else {
                    parent
                        .canonicalize()
                        .unwrap_or_else(|_| parent.to_path_buf())
                };
                if let Some(filename) = path.file_name() {
                    canonical_parent.join(filename)
                } else {
                    path.to_path_buf()
                }
            } else {
                path.to_path_buf()
            }
        };
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

        // Create the editor state - either load from file or create empty buffer
        let mut state = if file_exists {
            EditorState::from_file(
                path,
                self.terminal_width,
                self.terminal_height,
                self.config.editor.large_file_threshold_bytes as usize,
                &self.grammar_registry,
            )?
        } else {
            // File doesn't exist - create empty buffer with the file path set
            let mut new_state = EditorState::new(
                self.terminal_width,
                self.terminal_height,
                self.config.editor.large_file_threshold_bytes as usize,
            );
            // Set the file path so saving will create the file
            new_state.buffer.set_file_path(path.to_path_buf());
            new_state
        };
        state.viewport.line_wrap_enabled = self.config.editor.line_wrap;

        // Check if the buffer contains binary content
        let is_binary = state.buffer.is_binary();
        if is_binary {
            // Make binary buffers read-only
            state.editing_disabled = true;
            tracing::info!("Detected binary file: {}", path.display());
        }

        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Create metadata for this buffer
        let mut metadata = BufferMetadata::with_file(path.to_path_buf(), &self.working_dir);

        // Mark binary files in metadata and disable LSP
        if is_binary {
            metadata.binary = true;
            metadata.read_only = true;
            metadata.disable_lsp("Binary file".to_string());
        }

        // Notify LSP about the newly opened file (skip for binary files)
        if !is_binary {
            self.notify_lsp_file_opened(path, buffer_id, &mut metadata);
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

        // Show appropriate status message for binary vs regular files
        if is_binary {
            self.status_message = Some(format!("Opened {} [binary file, read-only]", display_name));
        } else {
            self.status_message = Some(format!("Opened {}", display_name));
        }

        // Emit control event
        self.emit_event(
            crate::model::control_event::events::FILE_OPENED.name,
            serde_json::json!({
                "path": path.display().to_string(),
                "buffer_id": buffer_id.0
            }),
        );

        // Track file for auto-revert and conflict detection
        self.watch_file(path);

        // Fire AfterFileOpen hook for plugins
        if let Some(ref ts_manager) = self.ts_plugin_manager {
            let hook_args = crate::services::plugins::hooks::HookArgs::AfterFileOpen {
                buffer_id,
                path: path.to_path_buf(),
            };
            ts_manager.run_hook("after_file_open", hook_args);
        }

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

        // Set syntax highlighting based on buffer name (e.g., "*OURS*.c" will get C highlighting)
        state.set_language_from_name(&name, &self.grammar_registry);

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
            let mut view_state =
                SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id);
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
        entries: Vec<crate::primitives::text_property::TextPropertyEntry>,
    ) -> Result<(), String> {
        let state = self
            .buffers
            .get_mut(&buffer_id)
            .ok_or_else(|| "Buffer not found".to_string())?;

        // Build text and properties from entries
        let (text, properties) =
            crate::primitives::text_property::TextPropertyManager::from_entries(entries);

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

    /// Open the built-in help manual in a read-only buffer
    ///
    /// If a help manual buffer already exists, switch to it instead of creating a new one.
    pub fn open_help_manual(&mut self) {
        // Check if help buffer already exists
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::HELP_MANUAL_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            // Switch to existing help buffer
            self.set_active_buffer(buffer_id);
            return;
        }

        // Create new help buffer with "special" mode (has 'q' to close)
        let buffer_id = self.create_virtual_buffer(
            help::HELP_MANUAL_BUFFER_NAME.to_string(),
            "special".to_string(),
            true,
        );

        // Set the content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, help::HELP_MANUAL_CONTENT);
            state.buffer.clear_modified();
            state.editing_disabled = true;

            // Disable line numbers for cleaner display
            state.margins.set_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }

    /// Open the keyboard shortcuts viewer in a read-only buffer
    ///
    /// If a keyboard shortcuts buffer already exists, switch to it instead of creating a new one.
    /// The shortcuts are dynamically generated from the current keybindings configuration.
    pub fn open_keyboard_shortcuts(&mut self) {
        // Check if keyboard shortcuts buffer already exists
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::KEYBOARD_SHORTCUTS_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            // Switch to existing buffer
            self.set_active_buffer(buffer_id);
            return;
        }

        // Get all keybindings
        let bindings = self.keybindings.get_all_bindings();

        // Format the keybindings as readable text
        let mut content = String::from("Keyboard Shortcuts\n");
        content.push_str("==================\n\n");
        content.push_str("Press 'q' to close this buffer.\n\n");

        // Group bindings by context (Normal, Prompt, etc.)
        let mut current_context = String::new();
        for (key, action) in &bindings {
            // Check if action starts with a context prefix like "[Prompt] "
            let (context, action_name) = if let Some(bracket_end) = action.find("] ") {
                let ctx = &action[1..bracket_end];
                let name = &action[bracket_end + 2..];
                (ctx.to_string(), name.to_string())
            } else {
                ("Normal".to_string(), action.clone())
            };

            // Print context header when it changes
            if context != current_context {
                if !current_context.is_empty() {
                    content.push('\n');
                }
                content.push_str(&format!("── {} Mode ──\n\n", context));
                current_context = context;
            }

            // Format: "  Ctrl+S          Save"
            content.push_str(&format!("  {:20} {}\n", key, action_name));
        }

        // Create new keyboard shortcuts buffer with "special" mode (has 'q' to close)
        let buffer_id = self.create_virtual_buffer(
            help::KEYBOARD_SHORTCUTS_BUFFER_NAME.to_string(),
            "special".to_string(),
            true,
        );

        // Set the content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, &content);
            state.buffer.clear_modified();
            state.editing_disabled = true;

            // Disable line numbers for cleaner display
            state.margins.set_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }

    /// Get text properties at the cursor position in the active buffer
    pub fn get_text_properties_at_cursor(
        &self,
    ) -> Option<Vec<&crate::primitives::text_property::TextProperty>> {
        let state = self.buffers.get(&self.active_buffer)?;
        let cursor_pos = state.cursors.primary().position;
        Some(state.text_properties.get_at(cursor_pos))
    }

    /// Close the given buffer
    pub fn close_buffer(&mut self, id: BufferId) -> io::Result<()> {
        // Check for unsaved changes
        if let Some(state) = self.buffers.get(&id) {
            if state.buffer.is_modified() {
                return Err(io::Error::other("Buffer has unsaved changes"));
            }
        }
        self.close_buffer_internal(id)
    }

    /// Force close the given buffer without checking for unsaved changes
    /// Use this when the user has already confirmed they want to discard changes
    pub fn force_close_buffer(&mut self, id: BufferId) -> io::Result<()> {
        self.close_buffer_internal(id)
    }

    /// Internal helper to close a buffer (shared by close_buffer and force_close_buffer)
    fn close_buffer_internal(&mut self, id: BufferId) -> io::Result<()> {
        // If it's the last buffer, create a new anonymous buffer first
        let replacement_buffer = if self.buffers.len() == 1 {
            self.new_buffer()
        } else {
            // Find a replacement buffer (any buffer that's not the one being closed)
            *self.buffers.keys().find(|&&bid| bid != id).unwrap()
        };

        // Update all splits that are showing this buffer to show the replacement
        let splits_to_update = self.split_manager.splits_for_buffer(id);
        for split_id in splits_to_update {
            let _ = self
                .split_manager
                .set_split_buffer(split_id, replacement_buffer);
        }

        self.buffers.remove(&id);
        self.event_logs.remove(&id);
        self.seen_byte_ranges.remove(&id);
        self.buffer_metadata.remove(&id);

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
            crate::model::event::SplitDirection::Horizontal,
            current_buffer_id,
            0.5,
        ) {
            Ok(new_split_id) => {
                // Create independent view state for the new split with the current buffer
                let mut view_state = SplitViewState::with_buffer(
                    self.terminal_width,
                    self.terminal_height,
                    current_buffer_id,
                );
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
            crate::model::event::SplitDirection::Vertical,
            current_buffer_id,
            0.5,
        ) {
            Ok(new_split_id) => {
                // Create independent view state for the new split with the current buffer
                let mut view_state = SplitViewState::with_buffer(
                    self.terminal_width,
                    self.terminal_height,
                    current_buffer_id,
                );
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
        self.sync_split_view_state_to_editor_state();
        // Ensure the active tab is visible in the newly active split
        // Use effective_tabs_width() to account for file explorer taking 30% of width
        self.ensure_active_tab_visible(split_id, self.active_buffer, self.effective_tabs_width());
    }

    /// Sync SplitViewState's cursors and viewport to EditorState
    /// Called when switching splits to restore the split's view state
    fn sync_split_view_state_to_editor_state(&mut self) {
        let split_id = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get(&split_id) {
            if let Some(buffer_state) = self.buffers.get_mut(&self.active_buffer) {
                buffer_state.cursors = view_state.cursors.clone();
                buffer_state.viewport = view_state.viewport.clone();
            }
        }
    }

    /// Sync only viewport DIMENSIONS from SplitViewState to EditorState
    /// Called before action_to_events to ensure correct viewport dimensions for PageDown/PageUp
    /// Note: Does NOT sync scroll position (top_byte) - EditorState is authoritative for that
    fn sync_viewport_from_split_view_state(&mut self) {
        let split_id = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get(&split_id) {
            if let Some(buffer_state) = self.buffers.get_mut(&self.active_buffer) {
                // Only sync dimensions, NOT scroll position
                // EditorState's top_byte is authoritative (set by ensure_visible)
                buffer_state.viewport.width = view_state.viewport.width;
                buffer_state.viewport.height = view_state.viewport.height;
                tracing::trace!(
                    "sync_viewport_from_split: split {:?} dims {}x{} (keeping EditorState top_byte={})",
                    split_id, view_state.viewport.width, view_state.viewport.height, buffer_state.viewport.top_byte
                );
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
                    view_state
                        .cursors
                        .adjust_for_edit(*edit_pos, *old_len, *new_len);
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

    /// Toggle line numbers in the gutter for the active buffer
    pub fn toggle_line_numbers(&mut self) {
        if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
            let currently_shown = state.margins.show_line_numbers;
            state.margins.set_line_numbers(!currently_shown);
            if currently_shown {
                self.set_status_message("Line numbers hidden".to_string());
            } else {
                // Restore proper width based on buffer size
                let total_lines = state.buffer.line_count().unwrap_or(1);
                state.margins.update_width_for_buffer(total_lines);
                self.set_status_message("Line numbers shown".to_string());
            }
        }
    }

    /// Toggle mouse capture on/off
    pub fn toggle_mouse_capture(&mut self) {
        use std::io::stdout;

        self.mouse_enabled = !self.mouse_enabled;

        if self.mouse_enabled {
            let _ = crossterm::execute!(stdout(), crossterm::event::EnableMouseCapture);
            self.set_status_message("Mouse capture enabled".to_string());
        } else {
            let _ = crossterm::execute!(stdout(), crossterm::event::DisableMouseCapture);
            self.set_status_message("Mouse capture disabled".to_string());
        }
    }

    /// Check if mouse capture is enabled
    pub fn is_mouse_enabled(&self) -> bool {
        self.mouse_enabled
    }

    /// Toggle inlay hints visibility
    pub fn toggle_inlay_hints(&mut self) {
        self.config.editor.enable_inlay_hints = !self.config.editor.enable_inlay_hints;

        if self.config.editor.enable_inlay_hints {
            // Re-request inlay hints for the active buffer
            self.request_inlay_hints_for_active_buffer();
            self.set_status_message("Inlay hints enabled".to_string());
        } else {
            // Clear inlay hints from all buffers
            for state in self.buffers.values_mut() {
                state.virtual_texts.clear(&mut state.marker_list);
            }
            self.set_status_message("Inlay hints disabled".to_string());
        }
    }

    /// Request inlay hints for the active buffer (if enabled and LSP available)
    fn request_inlay_hints_for_active_buffer(&mut self) {
        if !self.config.editor.enable_inlay_hints {
            return;
        }

        // Get metadata for the active buffer
        let metadata = match self.buffer_metadata.get(&self.active_buffer) {
            Some(m) => m,
            None => return,
        };

        let uri = match metadata.file_uri() {
            Some(uri) => uri.clone(),
            None => return,
        };

        let path = match metadata.file_path() {
            Some(p) => p.clone(),
            None => return,
        };

        let language = match crate::services::lsp::manager::detect_language(&path) {
            Some(lang) => lang,
            None => return,
        };

        // Get line count from buffer state
        let line_count = if let Some(state) = self.buffers.get(&self.active_buffer) {
            state.buffer.line_count().unwrap_or(1000)
        } else {
            return;
        };
        let last_line = line_count.saturating_sub(1) as u32;

        // Get LSP client for this language
        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(&language) {
                let request_id = self.next_lsp_request_id;
                self.next_lsp_request_id += 1;
                self.pending_inlay_hints_request = Some(request_id);

                if let Err(e) = client.inlay_hints(request_id, uri.clone(), 0, 0, last_line, 10000)
                {
                    tracing::debug!("Failed to request inlay hints: {}", e);
                    self.pending_inlay_hints_request = None;
                } else {
                    tracing::info!(
                        "Requested inlay hints for {} (request_id={})",
                        uri.as_str(),
                        request_id
                    );
                }
            }
        }
    }

    /// Dump the current configuration to the user's config file
    pub fn dump_config(&mut self) {
        // Get the config directory path
        let config_dir = match dirs::config_dir() {
            Some(dir) => dir.join("fresh"),
            None => {
                self.set_status_message("Error: Could not determine config directory".to_string());
                return;
            }
        };

        // Create the config directory if it doesn't exist
        if let Err(e) = std::fs::create_dir_all(&config_dir) {
            self.set_status_message(format!("Error creating config directory: {}", e));
            return;
        }

        let config_path = config_dir.join("config.json");

        // Save the config
        match self.config.save_to_file(&config_path) {
            Ok(()) => {
                // Open the saved config file in a new buffer
                match self.open_file(&config_path) {
                    Ok(_buffer_id) => {
                        self.set_status_message(format!(
                            "Config saved to {}",
                            config_path.display()
                        ));
                    }
                    Err(e) => {
                        self.set_status_message(format!("Config saved but failed to open: {}", e));
                    }
                }
            }
            Err(e) => {
                self.set_status_message(format!("Error saving config: {}", e));
            }
        }
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

        // Cancel search/replace prompts when switching buffers
        // (they are buffer-specific and don't make sense across buffers)
        self.cancel_search_prompt_if_active();

        self.active_buffer = buffer_id;

        // Update split manager to show this buffer
        self.split_manager.set_active_buffer_id(buffer_id);

        // Add buffer to the active split's open_buffers (tabs) if not already there
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
        }

        // Ensure the newly active tab is visible
        // Use effective_tabs_width() to account for file explorer taking 30% of width
        self.ensure_active_tab_visible(active_split, buffer_id, self.effective_tabs_width());

        // Sync file explorer to the new active file (if visible and applicable)
        self.sync_file_explorer_to_active_file();
    }

    /// Get the currently active buffer state
    pub fn active_state(&self) -> &EditorState {
        self.buffers.get(&self.active_buffer).unwrap()
    }

    /// Get the currently active buffer state (mutable)
    pub fn active_state_mut(&mut self) -> &mut EditorState {
        self.buffers.get_mut(&self.active_buffer).unwrap()
    }

    /// Get the display name for a buffer (filename or virtual buffer name)
    pub fn get_buffer_display_name(&self, buffer_id: BufferId) -> String {
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
            Event::Insert { .. } | Event::Delete { .. } => {
                self.invalidate_layouts_for_buffer(self.active_buffer);
            }
            Event::Batch { events, .. } => {
                let has_edits = events
                    .iter()
                    .any(|e| matches!(e, Event::Insert { .. } | Event::Delete { .. }));
                if has_edits {
                    self.invalidate_layouts_for_buffer(self.active_buffer);
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

        // 3. Trigger plugin hooks for this event (with pre-calculated line info)
        self.trigger_plugin_hooks_for_event(event, line_info);

        // 4. Notify LSP of the change using pre-calculated positions
        self.send_lsp_changes_for_buffer(self.active_buffer, lsp_changes);
    }

    /// Trigger plugin hooks for an event (if any)
    /// line_info contains pre-calculated line numbers from BEFORE buffer modification
    fn trigger_plugin_hooks_for_event(&mut self, event: &Event, line_info: EventLineInfo) {
        let buffer_id = self.active_buffer;

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
                    "after-insert",
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
                    "after-delete",
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
            _ => None,
        };

        // Fire the hook to TypeScript plugins
        if let Some((hook_name, args)) = hook_args {
            if let Some(ref ts_manager) = self.ts_plugin_manager {
                ts_manager.run_hook(hook_name, args);
            }
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
        let buffer_id = self.active_buffer;

        // Get view_transform tokens from SplitViewState (if any)
        let view_transform_tokens = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.view_transform.as_ref())
            .map(|vt| vt.tokens.clone());

        // Get mutable references to both buffer and view state
        let buffer = &mut self.buffers.get_mut(&buffer_id).unwrap().buffer;
        let view_state = self.split_view_states.get_mut(&active_split);

        if let Some(view_state) = view_state {
            if let Some(tokens) = view_transform_tokens {
                // Use view-aware scrolling with the transform's tokens
                let view_lines: Vec<_> = ViewLineIterator::new(&tokens).collect();
                view_state
                    .viewport
                    .scroll_view_lines(&view_lines, line_offset);
            } else {
                // No view transform - use traditional buffer-based scrolling
                // Still use SplitViewState's viewport (not EditorState's)
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
        }

        // Also sync to EditorState.viewport for the active split
        // (rendering uses EditorState.viewport for the active split)
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            if let Some(view_state) = self.split_view_states.get(&active_split) {
                state.viewport.top_byte = view_state.viewport.top_byte;
            }
        }
    }

    /// Handle SetViewport event using SplitViewState's viewport
    fn handle_set_viewport_event(&mut self, top_line: usize) {
        let active_split = self.split_manager.active_split();
        let buffer_id = self.active_buffer;

        // Get mutable references to both buffer and view state
        let buffer = self.buffers.get_mut(&buffer_id).map(|s| &mut s.buffer);
        let view_state = self.split_view_states.get_mut(&active_split);

        if let (Some(buffer), Some(view_state)) = (buffer, view_state) {
            view_state.viewport.scroll_to(buffer, top_line);
        }
    }

    /// Handle Recenter event using SplitViewState's viewport and cursors
    fn handle_recenter_event(&mut self) {
        let active_split = self.split_manager.active_split();
        let buffer_id = self.active_buffer;

        // Get cursor position from SplitViewState's cursors
        let cursor_position = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.cursors.iter().next())
            .map(|(_, c)| c.position);

        if let Some(cursor_pos) = cursor_position {
            // Get buffer to calculate line
            if let Some(state) = self.buffers.get(&buffer_id) {
                let cursor_line = state.buffer.position_to_line_col(cursor_pos).0;
                let half_height = self
                    .split_view_states
                    .get(&active_split)
                    .map(|vs| (vs.viewport.height / 2) as usize)
                    .unwrap_or(12);
                let new_top = cursor_line.saturating_sub(half_height);

                // Now scroll the viewport
                let buffer = &mut self.buffers.get_mut(&buffer_id).unwrap().buffer;
                if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                    view_state.viewport.scroll_to(buffer, new_top);
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

    /// Sync cursors and viewport from EditorState to SplitViewState
    ///
    /// This keeps SplitViewState's View state (cursors, viewport) in sync with
    /// EditorState after events are applied. This is necessary because some
    /// events (cursor movements, edits) still update EditorState directly.
    fn sync_editor_state_to_split_view_state(&mut self) {
        let split_id = self.split_manager.active_split();
        if let Some(buffer_state) = self.buffers.get(&self.active_buffer) {
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.cursors = buffer_state.cursors.clone();
                // Also sync viewport because cursor movements may trigger ensure_visible
                // which updates EditorState's viewport
                tracing::trace!(
                    "sync_editor_to_split: EditorState top_byte={} -> split {:?} top_byte={}",
                    buffer_state.viewport.top_byte,
                    split_id,
                    view_state.viewport.top_byte
                );
                view_state.viewport = buffer_state.viewport.clone();
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

    /// Update the buffer's modified flag based on event log position
    /// Call this after undo/redo to correctly track whether the buffer
    /// has returned to its saved state
    pub(super) fn update_modified_from_event_log(&mut self) {
        let is_at_saved = self
            .event_logs
            .get(&self.active_buffer)
            .map(|log| log.is_at_saved_position())
            .unwrap_or(false);

        if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
            state.buffer.set_modified(!is_at_saved);
        }
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
            self.clipboard.copy(text);
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
        // Get content from clipboard (tries system first, falls back to internal)
        let paste_text = match self.clipboard.paste() {
            Some(text) => text,
            None => return,
        };

        let state = self.active_state();
        let cursor_id = state.cursors.primary_id();
        let position = state.cursors.primary().position;

        let event = Event::Insert {
            position,
            text: paste_text,
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
        let path = self
            .active_state()
            .buffer
            .file_path()
            .map(|p| p.to_path_buf());
        self.active_state_mut().buffer.save()?;
        self.status_message = Some("Saved".to_string());

        // Mark the event log position as saved (for undo modified tracking)
        self.active_event_log_mut().mark_saved();

        // Update file modification time after save
        if let Some(ref p) = path {
            if let Ok(metadata) = std::fs::metadata(p) {
                if let Ok(mtime) = metadata.modified() {
                    self.file_mod_times.insert(p.clone(), mtime);
                }
            }
        }

        // Notify LSP of save
        self.notify_lsp_save();

        // Delete recovery file (buffer is now saved)
        let _ = self.delete_buffer_recovery(self.active_buffer);

        // Emit control event
        if let Some(ref p) = path {
            self.emit_event(
                crate::model::control_event::events::FILE_SAVED.name,
                serde_json::json!({
                    "path": p.display().to_string()
                }),
            );
        }

        // Fire AfterFileSave hook for plugins
        if let Some(ref p) = path {
            if let Some(ref ts_manager) = self.ts_plugin_manager {
                let buffer_id = self.active_buffer;
                let hook_args = crate::services::plugins::hooks::HookArgs::AfterFileSave {
                    buffer_id,
                    path: p.clone(),
                };
                ts_manager.run_hook("after_file_save", hook_args);
            }
        }

        Ok(())
    }

    /// Revert the active buffer to the last saved version on disk
    /// Returns Ok(true) if reverted, Ok(false) if no file path, Err on failure
    pub fn revert_file(&mut self) -> io::Result<bool> {
        let path = match self.active_state().buffer.file_path() {
            Some(p) => p.to_path_buf(),
            None => {
                self.status_message = Some("Buffer has no file to revert to".to_string());
                return Ok(false);
            }
        };

        if !path.exists() {
            self.status_message = Some(format!("File does not exist: {}", path.display()));
            return Ok(false);
        }

        // Save scroll position and cursor positions before reloading
        let old_top_byte = self.active_state().viewport.top_byte;
        let old_left_column = self.active_state().viewport.left_column;
        let old_cursors = self.active_state().cursors.clone();

        // Load the file content fresh from disk
        let mut new_state = EditorState::from_file(
            &path,
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            &self.grammar_registry,
        )?;

        // Restore scroll position (clamped to valid range for new file size)
        let new_file_size = new_state.buffer.len();
        new_state.viewport.top_byte = old_top_byte.min(new_file_size);
        new_state.viewport.left_column = old_left_column;

        // Restore cursor positions (clamped to valid range for new file size)
        let mut restored_cursors = old_cursors;
        restored_cursors.map(|cursor| {
            cursor.position = cursor.position.min(new_file_size);
            // Clear selection since the content may have changed
            cursor.clear_selection();
        });
        new_state.cursors = restored_cursors;

        // Replace the current buffer with the new state
        let buffer_id = self.active_buffer;
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            *state = new_state;
            // Apply line wrap setting from config
            state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
        }

        // Clear the undo/redo history for this buffer
        if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
            *event_log = EventLog::new();
        }

        // Clear seen_byte_ranges so plugins get notified of all visible lines
        self.seen_byte_ranges.remove(&buffer_id);

        // Update the file modification time
        if let Ok(metadata) = std::fs::metadata(&path) {
            if let Ok(mtime) = metadata.modified() {
                self.file_mod_times.insert(path.clone(), mtime);
            }
        }

        // Notify LSP that the file was changed
        self.notify_lsp_file_changed(&path);

        self.status_message = Some("Reverted to saved file".to_string());
        Ok(true)
    }

    /// Toggle auto-revert mode
    pub fn toggle_auto_revert(&mut self) {
        self.auto_revert_enabled = !self.auto_revert_enabled;

        if self.auto_revert_enabled {
            // Start file watcher if not already running
            self.start_file_watcher();
            self.status_message = Some("Auto-revert enabled".to_string());
        } else {
            // Stop file watcher
            self.file_watcher = None;
            self.watched_dirs.clear();
            self.status_message = Some("Auto-revert disabled".to_string());
        }
    }

    /// Start the file watcher for auto-revert functionality
    fn start_file_watcher(&mut self) {
        use notify::{RecursiveMode, Watcher};

        // Get the sender for async messages
        let sender = match &self.async_bridge {
            Some(bridge) => bridge.sender(),
            None => {
                tracing::warn!("Cannot start file watcher: no async bridge available");
                return;
            }
        };

        // Create a new watcher
        // We watch directories (not files) to handle atomic saves where editors
        // write to a temp file and rename it, which changes the file's inode
        let watcher_result =
            notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
                match res {
                    Ok(event) => {
                        // Handle modify, create, and rename events
                        // Rename is important for atomic saves (temp file + rename)
                        let dominated = matches!(
                            event.kind,
                            notify::EventKind::Modify(_)
                                | notify::EventKind::Create(_)
                                | notify::EventKind::Remove(_)
                        );
                        if dominated {
                            for path in event.paths {
                                if let Err(e) = sender.send(AsyncMessage::FileChanged {
                                    path: path.display().to_string(),
                                }) {
                                    tracing::error!(
                                        "Failed to send file change notification: {}",
                                        e
                                    );
                                }
                            }
                        }
                    }
                    Err(e) => {
                        tracing::error!("File watcher error: {}", e);
                    }
                }
            });

        match watcher_result {
            Ok(mut watcher) => {
                // Watch parent directories of all currently open files
                for state in self.buffers.values() {
                    if let Some(path) = state.buffer.file_path() {
                        if let Some(parent) = path.parent() {
                            if !self.watched_dirs.contains(parent) {
                                if let Err(e) = watcher.watch(parent, RecursiveMode::NonRecursive) {
                                    tracing::warn!("Failed to watch directory {:?}: {}", parent, e);
                                } else {
                                    self.watched_dirs.insert(parent.to_path_buf());
                                }
                            }
                        }
                    }
                }
                self.file_watcher = Some(watcher);
                tracing::info!("File watcher started");
            }
            Err(e) => {
                tracing::error!("Failed to create file watcher: {}", e);
                self.status_message = Some(format!("Failed to start file watcher: {}", e));
            }
        }
    }

    /// Notify LSP server about a newly opened file
    /// Handles language detection, spawning LSP clients, and sending didOpen notifications
    fn notify_lsp_file_opened(
        &mut self,
        path: &Path,
        buffer_id: BufferId,
        metadata: &mut BufferMetadata,
    ) {
        // Early return checks that don't need mutable lsp borrow
        let Some(language) = detect_language(path) else {
            tracing::debug!("No language detected for file: {}", path.display());
            return;
        };

        let Some(uri) = metadata.file_uri().cloned() else {
            tracing::warn!(
                "No URI in metadata for file: {} (failed to compute absolute path)",
                path.display()
            );
            return;
        };

        // Check file size
        let file_size = std::fs::metadata(path).ok().map(|m| m.len()).unwrap_or(0);
        if file_size > self.config.editor.large_file_threshold_bytes {
            let reason = format!("File too large ({} bytes)", file_size);
            tracing::warn!(
                "Skipping LSP for large file: {} ({})",
                path.display(),
                reason
            );
            metadata.disable_lsp(reason);
            return;
        }

        // Get text before borrowing lsp
        let text = match self.buffers.get(&buffer_id).and_then(|state| state.buffer.to_string()) {
            Some(t) => t,
            None => {
                tracing::debug!("Buffer not fully loaded for LSP notification");
                return;
            }
        };

        let enable_inlay_hints = self.config.editor.enable_inlay_hints;
        let previous_result_id = self.diagnostic_result_ids.get(uri.as_str()).cloned();

        // Get buffer line count for inlay hints
        let (last_line, last_char) = self
            .buffers
            .get(&buffer_id)
            .map(|state| {
                let line_count = state.buffer.line_count().unwrap_or(1000);
                (line_count.saturating_sub(1) as u32, 10000u32)
            })
            .unwrap_or((999, 10000));

        // Now borrow lsp and do all LSP operations
        let Some(lsp) = &mut self.lsp else {
            tracing::debug!("No LSP manager available");
            return;
        };

        tracing::debug!("LSP manager available for file: {}", path.display());
        tracing::debug!(
            "Detected language: {} for file: {}",
            language,
            path.display()
        );
        tracing::debug!("Using URI from metadata: {}", uri.as_str());
        tracing::debug!("Attempting to spawn LSP client for language: {}", language);

        match lsp.try_spawn(&language) {
            LspSpawnResult::Spawned => {
                if let Some(client) = lsp.get_or_spawn(&language) {
                    // Send didOpen
                    tracing::info!("Sending didOpen to LSP for: {}", uri.as_str());
                    if let Err(e) = client.did_open(uri.clone(), text, language.clone()) {
                        tracing::warn!("Failed to send didOpen to LSP: {}", e);
                        return;
                    }
                    tracing::info!("Successfully sent didOpen to LSP");

                    // Request pull diagnostics
                    let request_id = self.next_lsp_request_id;
                    self.next_lsp_request_id += 1;
                    if let Err(e) =
                        client.document_diagnostic(request_id, uri.clone(), previous_result_id)
                    {
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

                    // Request inlay hints
                    if enable_inlay_hints {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_inlay_hints_request = Some(request_id);

                        if let Err(e) =
                            client.inlay_hints(request_id, uri.clone(), 0, 0, last_line, last_char)
                        {
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
                }
            }
            LspSpawnResult::NotAutoStart => {
                tracing::debug!(
                    "LSP for {} not auto-starting (auto_start=false). Use command palette to start manually.",
                    language
                );
            }
            LspSpawnResult::Failed => {
                tracing::warn!("Failed to spawn LSP client for language: {}", language);
            }
        }
    }

    /// Add a file to the file watcher (called when opening files)
    /// We watch the parent directory instead of the file itself to handle
    /// atomic saves (temp file + rename) which change the file's inode
    fn watch_file(&mut self, path: &Path) {
        use notify::{RecursiveMode, Watcher};

        // Record current modification time
        if let Ok(metadata) = std::fs::metadata(path) {
            if let Ok(mtime) = metadata.modified() {
                self.file_mod_times.insert(path.to_path_buf(), mtime);
            }
        }

        // Add parent directory to watcher if auto-revert is enabled
        if self.auto_revert_enabled {
            // Start file watcher if not already running
            if self.file_watcher.is_none() {
                self.start_file_watcher();
            }
            // Watch the parent directory if not already watched
            if let Some(parent) = path.parent() {
                if !self.watched_dirs.contains(parent) {
                    if let Some(watcher) = &mut self.file_watcher {
                        if let Err(e) = watcher.watch(parent, RecursiveMode::NonRecursive) {
                            tracing::warn!("Failed to watch directory {:?}: {}", parent, e);
                        } else {
                            self.watched_dirs.insert(parent.to_path_buf());
                        }
                    }
                }
            }
        }
    }

    /// Notify LSP that a file's contents changed (e.g., after revert)
    fn notify_lsp_file_changed(&mut self, path: &Path) {
        if let Some(lsp) = &mut self.lsp {
            if let Ok(uri) = url::Url::from_file_path(path) {
                if let Ok(lsp_uri) = uri.as_str().parse::<lsp_types::Uri>() {
                    // Detect language for this file
                    if let Some(language) = detect_language(path) {
                        // Get the new content
                        let content = self
                            .buffers
                            .values()
                            .find(|s| s.buffer.file_path() == Some(path))
                            .and_then(|state| state.buffer.to_string());

                        // Use full document sync - send the entire new content
                        if let Some(content) = content {
                            if let Some(client) = lsp.get_or_spawn(&language) {
                                let content_change = TextDocumentContentChangeEvent {
                                    range: None, // None means full document replacement
                                    range_length: None,
                                    text: content,
                                };
                                if let Err(e) = client.did_change(lsp_uri, vec![content_change]) {
                                    tracing::warn!("Failed to notify LSP of file change: {}", e);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Handle a file change notification (from file watcher)
    pub fn handle_file_changed(&mut self, changed_path: &str) {
        let path = PathBuf::from(changed_path);

        // Find buffers that have this file open
        let buffer_ids: Vec<BufferId> = self
            .buffers
            .iter()
            .filter(|(_, state)| state.buffer.file_path() == Some(&path))
            .map(|(id, _)| *id)
            .collect();

        if buffer_ids.is_empty() {
            return;
        }

        for buffer_id in buffer_ids {
            let state = match self.buffers.get(&buffer_id) {
                Some(s) => s,
                None => continue,
            };

            // Check if the file actually changed (compare mod times)
            // We use optimistic concurrency: check mtime, and if we decide to revert,
            // re-check to handle the race where a save completed between our checks.
            let current_mtime = match std::fs::metadata(&path).and_then(|m| m.modified()) {
                Ok(mtime) => mtime,
                Err(_) => continue, // Can't read file, skip
            };

            let dominated_by_stored = self
                .file_mod_times
                .get(&path)
                .map(|stored| current_mtime <= *stored)
                .unwrap_or(false);

            if dominated_by_stored {
                continue;
            }

            // If buffer has local modifications, show a warning (don't auto-revert)
            if state.buffer.is_modified() {
                self.status_message = Some(format!(
                    "File {} changed on disk (buffer has unsaved changes)",
                    path.display()
                ));
                continue;
            }

            // Auto-revert if enabled and buffer is not modified
            if self.auto_revert_enabled {
                // Optimistic concurrency: re-check mtime before reverting.
                // A save may have completed between our first check and now,
                // updating file_mod_times. If so, skip the revert.
                let still_needs_revert = self
                    .file_mod_times
                    .get(&path)
                    .map(|stored| current_mtime > *stored)
                    .unwrap_or(true);

                if !still_needs_revert {
                    continue;
                }

                // Temporarily switch to this buffer to revert it
                let current_active = self.active_buffer;
                self.active_buffer = buffer_id;

                if let Err(e) = self.revert_file() {
                    tracing::error!("Failed to auto-revert file {:?}: {}", path, e);
                } else {
                    tracing::info!("Auto-reverted file: {:?}", path);
                }

                // Switch back to original buffer
                self.active_buffer = current_active;

                // Update the modification time tracking for this file
                self.watch_file(&path);
            }
        }
    }

    /// Check if saving would overwrite changes made by another process
    /// Returns Some(current_mtime) if there's a conflict, None otherwise
    pub fn check_save_conflict(&self) -> Option<std::time::SystemTime> {
        let path = match self.active_state().buffer.file_path() {
            Some(p) => p,
            None => return None,
        };

        // Get current file modification time
        let current_mtime = match std::fs::metadata(path).and_then(|m| m.modified()) {
            Ok(mtime) => mtime,
            Err(_) => return None, // File doesn't exist or can't read metadata
        };

        // Compare with our recorded modification time
        match self.file_mod_times.get(path) {
            Some(recorded_mtime) if current_mtime > *recorded_mtime => {
                // File was modified externally since we last loaded/saved it
                Some(current_mtime)
            }
            _ => None,
        }
    }

    /// Check if the editor should quit
    pub fn should_quit(&self) -> bool {
        self.should_quit
    }

    /// Get the active theme
    pub fn theme(&self) -> &crate::view::theme::Theme {
        &self.theme
    }

    /// Request the editor to quit
    pub fn quit(&mut self) {
        // Check for unsaved buffers
        let modified_count = self.count_modified_buffers();
        if modified_count > 0 {
            // Prompt user for confirmation
            let msg = if modified_count == 1 {
                "1 buffer has unsaved changes. Quit anyway? (y/n): ".to_string()
            } else {
                format!(
                    "{} buffers have unsaved changes. Quit anyway? (y/n): ",
                    modified_count
                )
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

    // ========================================================================
    // Recovery service methods
    // ========================================================================

    /// Start the recovery session (call on editor startup after recovery check)
    pub fn start_recovery_session(&mut self) -> io::Result<()> {
        self.recovery_service.start_session()
    }

    /// End the recovery session cleanly (call on normal shutdown)
    pub fn end_recovery_session(&mut self) -> io::Result<()> {
        self.recovery_service.end_session()
    }

    /// Check if there are files to recover from a crash
    pub fn has_recovery_files(&self) -> io::Result<bool> {
        self.recovery_service.should_offer_recovery()
    }

    /// Get list of recoverable files
    pub fn list_recoverable_files(&self) -> io::Result<Vec<crate::services::recovery::RecoveryEntry>> {
        self.recovery_service.list_recoverable()
    }

    /// Recover all buffers from recovery files
    /// Returns the number of buffers recovered
    pub fn recover_all_buffers(&mut self) -> io::Result<usize> {
        use crate::services::recovery::RecoveryResult;

        let entries = self.recovery_service.list_recoverable()?;
        let mut recovered_count = 0;

        for entry in entries {
            match self.recovery_service.accept_recovery(&entry) {
                Ok(RecoveryResult::Recovered { original_path, content }) => {
                    // Convert content to string
                    let text = String::from_utf8_lossy(&content).into_owned();

                    if let Some(path) = original_path {
                        // Open the file path (this creates the buffer)
                        if self.open_file(&path).is_ok() {
                            // Replace buffer content with recovered content
                            let state = self.active_state_mut();
                            let total = state.buffer.total_bytes();
                            state.buffer.delete(0..total);
                            state.buffer.insert(0, &text);
                            // Mark as modified since it differs from disk
                            state.buffer.set_modified(true);
                            recovered_count += 1;
                            tracing::info!("Recovered buffer: {}", path.display());
                        }
                    } else {
                        // Unsaved buffer - create new buffer with recovered content
                        self.new_buffer();
                        let state = self.active_state_mut();
                        state.buffer.insert(0, &text);
                        state.buffer.set_modified(true);
                        recovered_count += 1;
                        tracing::info!("Recovered unsaved buffer");
                    }
                }
                Ok(RecoveryResult::Corrupted { id, reason }) => {
                    tracing::warn!("Recovery file {} corrupted: {}", id, reason);
                }
                Ok(RecoveryResult::NotFound { id }) => {
                    tracing::warn!("Recovery file {} not found", id);
                }
                Err(e) => {
                    tracing::warn!("Failed to recover {}: {}", entry.id, e);
                }
            }
        }

        Ok(recovered_count)
    }

    /// Discard all recovery files without recovering
    pub fn discard_all_recovery(&mut self) -> io::Result<usize> {
        self.recovery_service.discard_all_recovery()
    }

    /// Perform auto-save for all modified buffers if needed
    /// Returns the number of buffers saved, or an error
    ///
    /// This function is designed to be called frequently (every frame) and will:
    /// - Return immediately if recovery is disabled
    /// - Return immediately if the auto-save interval hasn't passed
    /// - Return immediately if no buffers are modified
    /// - Only save buffers that are marked as needing a save
    pub fn auto_save_dirty_buffers(&mut self) -> io::Result<usize> {
        // Early exit if disabled
        if !self.recovery_service.is_enabled() {
            return Ok(0);
        }

        // Check if enough time has passed since last auto-save
        let interval = std::time::Duration::from_secs(
            self.config.editor.auto_save_interval_secs as u64
        );
        if self.last_auto_save.elapsed() < interval {
            return Ok(0);
        }

        // Collect buffer info first to avoid borrow issues
        // Only include buffers that have pending recovery changes AND need auto-save
        let buffer_info: Vec<_> = self.buffers.iter()
            .filter_map(|(buffer_id, state)| {
                let recovery_pending = state.buffer.is_recovery_pending();
                if recovery_pending {
                    let path = state.buffer.file_path().map(|p| p.to_path_buf());
                    let recovery_id = self.recovery_service.get_buffer_id(path.as_deref());
                    // Only save if enough time has passed since last recovery save
                    if self.recovery_service.needs_auto_save(&recovery_id, recovery_pending) {
                        Some((*buffer_id, recovery_id, path))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        // Early exit if nothing to save
        if buffer_info.is_empty() {
            // Still update the timer to avoid checking buffers too frequently
            self.last_auto_save = std::time::Instant::now();
            return Ok(0);
        }

        let mut saved_count = 0;

        for (buffer_id, recovery_id, path) in buffer_info {
            // Use get_mut to allow lazy loading via get_text_range_mut
            // This is necessary for large files with unloaded regions
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                // Get buffer content using get_text_range_mut which handles lazy loading
                // get_all_text() uses the non-lazy get_text_range() which returns empty
                // for large files with unloaded regions!
                let total_bytes = state.buffer.total_bytes();
                let content = match state.buffer.get_text_range_mut(0, total_bytes) {
                    Ok(bytes) => bytes,
                    Err(e) => {
                        tracing::warn!(
                            "Failed to get buffer content for recovery save: {}",
                            e
                        );
                        continue;
                    }
                };
                let line_count = state.buffer.line_count();

                // Save to recovery
                self.recovery_service.save_buffer(
                    &recovery_id,
                    &content,
                    path.as_deref(),
                    None,
                    line_count,
                )?;

                // Clear recovery_pending flag after successful save
                state.buffer.set_recovery_pending(false);
                saved_count += 1;
            }
        }

        self.last_auto_save = std::time::Instant::now();
        Ok(saved_count)
    }

    /// Check if the active buffer is marked dirty for recovery auto-save
    /// Used for testing to verify that edits properly trigger recovery tracking
    pub fn is_active_buffer_recovery_dirty(&self) -> bool {
        if let Some(state) = self.buffers.get(&self.active_buffer) {
            state.buffer.is_recovery_pending()
        } else {
            false
        }
    }

    /// Delete recovery for a buffer (call after saving or closing)
    pub fn delete_buffer_recovery(&mut self, buffer_id: BufferId) -> io::Result<()> {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let path = state.buffer.file_path().map(|p| p.to_path_buf());
            let recovery_id = self.recovery_service.get_buffer_id(path.as_deref());
            self.recovery_service.delete_buffer_recovery(&recovery_id)?;
            // Clear recovery_pending since buffer is now saved
            state.buffer.set_recovery_pending(false);
        }
        Ok(())
    }

    /// Resize all buffers to match new terminal size
    pub fn resize(&mut self, width: u16, height: u16) {
        // Update terminal dimensions for future buffer creation
        self.terminal_width = width;
        self.terminal_height = height;

        // Resize all existing buffer EditorStates
        for state in self.buffers.values_mut() {
            state.resize(width, height);
        }

        // Resize all SplitViewState viewports
        for view_state in self.split_view_states.values_mut() {
            view_state.viewport.resize(width, height);
        }
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
        let default_text =
            selected_text.or_else(|| self.search_history.last().map(|s| s.to_string()));

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
                self.search_history.init_at_last();
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
            PromptType::OpenFile | PromptType::SaveFileAs | PromptType::Command
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

    /// Compute the default directory text for the Open File prompt
    fn open_file_prompt_directory_hint(&self) -> String {
        let mut directory = self
            .active_state()
            .buffer
            .file_path()
            .and_then(|path| path.parent())
            .map(|parent| {
                parent
                    .strip_prefix(&self.working_dir)
                    .map(|relative| relative.to_string_lossy().to_string())
                    .unwrap_or_else(|_| parent.to_string_lossy().to_string())
            })
            .unwrap_or_default();

        // Only add trailing slash if we have a non-empty directory
        if !directory.is_empty() && !directory.ends_with('/') {
            directory.push('/');
        }

        directory
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
        let initial_dir = self
            .active_state()
            .buffer
            .file_path()
            .and_then(|path| path.parent())
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| self.working_dir.clone());

        // Create the file open state
        self.file_open_state = Some(file_open::FileOpenState::new(initial_dir.clone()));

        // Start async directory loading
        self.load_file_open_directory(initial_dir);
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
                    use crate::services::plugins::hooks::HookArgs;
                    let hook_args = HookArgs::PromptCancelled {
                        prompt_type: custom_type.clone(),
                        input: prompt.input.clone(),
                    };

                    if let Some(ref ts_manager) = self.ts_plugin_manager {
                        ts_manager.run_hook("prompt_cancelled", hook_args);
                    }
                }
                PromptType::LspRename { overlay_handle, .. } => {
                    // Remove the rename overlay when cancelling
                    let remove_overlay_event = crate::model::event::Event::RemoveOverlay {
                        handle: overlay_handle.clone(),
                    };
                    self.apply_event_to_active_buffer(&remove_overlay_event);
                }
                PromptType::OpenFile => {
                    // Clear file browser state
                    self.file_open_state = None;
                    self.file_browser_layout = None;
                }
                _ => {}
            }
        }

        self.prompt = None;
        self.pending_search_range = None;
        self.status_message = Some("Canceled".to_string());
    }

    /// Get the confirmed input and prompt type, consuming the prompt
    /// For command palette, returns the selected suggestion if available, otherwise the raw input
    /// Returns (input, prompt_type, selected_index)
    /// Returns None if trying to confirm a disabled command
    pub fn confirm_prompt(&mut self) -> Option<(String, PromptType, Option<usize>)> {
        if let Some(prompt) = self.prompt.take() {
            let selected_index = prompt.selected_suggestion;
            // For command, file, theme, and LSP stop prompts, prefer the selected suggestion over raw input
            let final_input = if matches!(
                prompt.prompt_type,
                PromptType::Command
                    | PromptType::OpenFile
                    | PromptType::SaveFileAs
                    | PromptType::StopLspServer
                    | PromptType::SelectTheme
            ) {
                // Use the selected suggestion if any
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

            // For StopLspServer, validate that the input matches a running server
            if matches!(prompt.prompt_type, PromptType::StopLspServer) {
                let is_valid = prompt
                    .suggestions
                    .iter()
                    .any(|s| s.text == final_input || s.get_value() == final_input);
                if !is_valid {
                    // Restore the prompt and don't confirm
                    self.prompt = Some(prompt);
                    self.set_status_message(format!(
                        "No running LSP server matches '{}'",
                        final_input
                    ));
                    return None;
                }
            }

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
            .or_else(|| self.status_message.as_ref())
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
                if let Some(prompt) = &mut self.prompt {
                    // Use the underlying context (not Prompt context) for filtering
                    prompt.suggestions = self.command_registry.read().unwrap().filter(
                        &input,
                        self.key_context,
                        &self.keybindings,
                        selection_active,
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
            PromptType::OpenFile => {
                // For OpenFile, update the file browser filter (native implementation)
                self.update_file_open_filter();
            }
            PromptType::SaveFileAs => {
                // Fire plugin hook for file path completion.
                // The hook is processed asynchronously by the plugin thread.
                // Commands (SetPromptSuggestions) will be picked up by the main loop's
                // process_async_messages() -> process_plugin_commands() on the next frame.
                use crate::services::plugins::hooks::HookArgs;
                let hook_args = HookArgs::PromptChanged {
                    prompt_type: "save-file-as".to_string(),
                    input,
                };

                if let Some(ref ts_manager) = self.ts_plugin_manager {
                    ts_manager.run_hook("prompt_changed", hook_args);
                }
            }
            PromptType::Plugin { custom_type } => {
                // Fire plugin hook for prompt input change
                use crate::services::plugins::hooks::HookArgs;
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
                    self.handle_lsp_diagnostics(uri, diagnostics);
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
                    self.handle_lsp_pulled_diagnostics(uri, result_id, diagnostics, unchanged);
                }
                AsyncMessage::LspInlayHints {
                    request_id,
                    uri,
                    hints,
                } => {
                    self.handle_lsp_inlay_hints(request_id, uri, hints);
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
                AsyncMessage::PluginProcessOutput {
                    process_id,
                    stdout,
                    stderr,
                    exit_code,
                } => {
                    self.handle_plugin_process_output(process_id, stdout, stderr, exit_code);
                }
                AsyncMessage::CustomNotification {
                    language,
                    method,
                    params,
                } => {
                    self.handle_custom_notification(language, method, params);
                }
                AsyncMessage::PluginLspResponse {
                    language: _,
                    request_id,
                    result,
                } => {
                    self.handle_plugin_lsp_response(request_id, result);
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
                AsyncMessage::LspStatusUpdate { language, status } => {
                    self.handle_lsp_status_update(language, status);
                }
                AsyncMessage::FileOpenDirectoryLoaded(result) => {
                    self.handle_file_open_directory_loaded(result);
                }
            }
        }

        // Update plugin state snapshot BEFORE processing commands
        // This ensures plugins have access to current editor state (cursor positions, etc.)
        self.update_plugin_state_snapshot();

        // Process TypeScript plugin commands
        let processed_any_commands = self.process_plugin_commands();

        // Process pending plugin action completions
        self.process_pending_plugin_actions();

        // Process pending LSP server restarts (with exponential backoff)
        self.process_pending_lsp_restarts();

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
    fn update_plugin_state_snapshot(&mut self) {
        // Update TypeScript plugin manager state
        if let Some(ref manager) = self.ts_plugin_manager {
            use crate::services::plugins::api::{BufferInfo, CursorInfo, ViewportInfo};

            let snapshot_handle = manager.state_snapshot_handle();
            let mut snapshot = snapshot_handle.write().unwrap();

            // Update active buffer ID
            snapshot.active_buffer_id = self.active_buffer;

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
            if let Some(active_state) = self.buffers.get_mut(&self.active_buffer) {
                // Primary cursor
                let primary = active_state.cursors.primary();
                let primary_position = primary.position;
                let primary_selection = primary.selection_range();

                snapshot.primary_cursor = Some(CursorInfo {
                    position: primary_position,
                    selection: primary_selection.clone(),
                });

                // Selected text from primary cursor (for clipboard plugin)
                snapshot.selected_text = if let Some(range) = primary_selection {
                    Some(active_state.get_text_range(range.start, range.end))
                } else {
                    None
                };

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
                snapshot.selected_text = None;
            }

            // Update clipboard (provide internal clipboard content to plugins)
            snapshot.clipboard = self.clipboard.get_internal().to_string();

            // Update working directory (for spawning processes in correct directory)
            snapshot.working_dir = self.working_dir.clone();
        }
    }

    /// Handle a plugin command - dispatches to specialized handlers in plugin_commands module
    fn handle_plugin_command(&mut self, command: PluginCommand) -> io::Result<()> {
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
                underline,
                bold,
                italic,
            } => {
                self.handle_add_overlay(
                    buffer_id, namespace, range, color, underline, bold, italic,
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
                before,
            } => {
                self.handle_add_virtual_text(
                    buffer_id,
                    virtual_text_id,
                    position,
                    text,
                    color,
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

            // ==================== Status/Prompt Commands ====================
            PluginCommand::SetStatus { message } => {
                self.handle_set_status(message);
            }
            PluginCommand::StartPrompt { label, prompt_type } => {
                self.handle_start_prompt(label, prompt_type);
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

            // ==================== Deprecated Commands ====================
            PluginCommand::SpawnProcess {
                command,
                args,
                cwd,
                callback_id: _,
            } => {
                tracing::warn!(
                    "SpawnProcess command with callback is deprecated. TypeScript plugins use native async. Command: {}",
                    command
                );
                let _ = args;
                let _ = cwd;
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

                // Now set the content
                match self.set_virtual_buffer_content(buffer_id, entries) {
                    Ok(()) => {
                        tracing::debug!("Set virtual buffer content for {:?}", buffer_id);
                        // Switch to the new buffer to display it
                        self.set_active_buffer(buffer_id);
                        tracing::debug!("Switched to virtual buffer {:?}", buffer_id);

                        // Send response if request_id is present
                        if let Some(req_id) = request_id {
                            tracing::trace!(
                                "CreateVirtualBufferWithContent: sending response for request_id={}, buffer_id={:?}",
                                req_id,
                                buffer_id
                            );
                            self.send_plugin_response(
                                crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
                                    buffer_id,
                                    request_id: req_id,
                                    split_id: None, // Created in current split, not a new split
                                },
                            );
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
                                self.active_buffer = existing_buffer_id;
                                tracing::debug!(
                                    "Focused split {:?} containing panel buffer",
                                    split_id
                                );
                            }

                            // Send response with existing buffer ID and split ID
                            if let Some(req_id) = request_id {
                                self.send_plugin_response(
                                    crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
                                        request_id: req_id,
                                        buffer_id: existing_buffer_id,
                                        split_id: splits.first().copied(),
                                    },
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
                            view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                            self.split_view_states.insert(new_split_id, view_state);

                            // Focus the new split (the diagnostics panel)
                            self.split_manager.set_active_split(new_split_id);
                            self.active_buffer = buffer_id;

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

                // Send response with buffer ID and split ID
                if let Some(req_id) = request_id {
                    tracing::trace!("CreateVirtualBufferInSplit: sending response for request_id={}, buffer_id={:?}, split_id={:?}", req_id, buffer_id, created_split_id);
                    self.send_plugin_response(
                        crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
                            request_id: req_id,
                            buffer_id,
                            split_id: created_split_id,
                        },
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

                // Send response with buffer ID and split ID
                if let Some(req_id) = request_id {
                    self.send_plugin_response(
                        crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
                            request_id: req_id,
                            buffer_id,
                            split_id: Some(split_id),
                        },
                    );
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
        use crate::primitives::word_navigation::find_completion_word_start;
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
        use crate::view::popup::PopupListItem;

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
        use crate::model::event::{
            PopupContentData, PopupData, PopupListItemData, PopupPositionData,
        };
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
            .apply(&crate::model::event::Event::ShowPopup { popup: popup_data });

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
                let event = crate::model::event::Event::MoveCursor {
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
            if let Some(language) = crate::services::lsp::manager::detect_language(path) {
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        if let Err(e) = handle.cancel_request(request_id) {
                            tracing::warn!("Failed to send LSP cancel request: {}", e);
                        } else {
                            tracing::debug!("Sent $/cancelRequest for request_id={}", request_id);
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
            if let Some(language) = crate::services::lsp::manager::detect_language(path) {
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
            if let Some(language) = crate::services::lsp::manager::detect_language(path) {
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
            if let Some(language) = crate::services::lsp::manager::detect_language(path) {
                // Get LSP handle
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_hover_request = Some(request_id);
                        self.lsp_status = "LSP: hover...".to_string();

                        let _ =
                            handle.hover(request_id, uri.clone(), line as u32, character as u32);
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

            // Remove previous hover overlay if any
            if let Some(old_handle) = self.hover_symbol_overlay.take() {
                let remove_event = crate::model::event::Event::RemoveOverlay { handle: old_handle };
                self.apply_event_to_active_buffer(&remove_event);
            }

            // Add overlay to highlight the hovered symbol
            let event = crate::model::event::Event::AddOverlay {
                namespace: None,
                range: start_byte..end_byte,
                face: crate::model::event::OverlayFace::Background {
                    color: (80, 80, 120), // Subtle highlight for hovered symbol
                },
                priority: 90, // Below rename (100) but above syntax (lower)
                message: None,
            };
            self.apply_event_to_active_buffer(&event);
            // Store the handle for later removal
            if let Some(state) = self.buffers.get(&self.active_buffer) {
                self.hover_symbol_overlay = state.overlays.all().last().map(|o| o.handle.clone());
            }
        } else {
            // No range provided by LSP, clear any previous highlight
            self.hover_symbol_range = None;
        }

        // Create a popup with the hover contents
        use crate::view::popup::{Popup, PopupPosition};
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
    fn apply_inlay_hints_to_state(
        state: &mut crate::state::EditorState,
        hints: &[lsp_types::InlayHint],
    ) {
        use crate::view::virtual_text::VirtualTextPosition;
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
            let text = match state.buffer.to_string() {
                Some(t) => t,
                None => {
                    self.set_status_message("Buffer not fully loaded".to_string());
                    return Ok(());
                }
            };
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
            if let Some(language) = crate::services::lsp::manager::detect_language(path) {
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
            if let Some(language) = crate::services::lsp::manager::detect_language(path) {
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
        use crate::view::popup::{Popup, PopupPosition};
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
            if let Some(language) = crate::services::lsp::manager::detect_language(path) {
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
        use crate::view::popup::{Popup, PopupPosition};
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
        let lsp_locations: Vec<crate::services::plugins::hooks::LspLocation> = locations
            .iter()
            .map(|loc| {
                // Convert URI to file path
                let file = if loc.uri.scheme().map(|s| s.as_str()) == Some("file") {
                    // Extract path from file:// URI
                    loc.uri.path().as_str().to_string()
                } else {
                    loc.uri.as_str().to_string()
                };

                crate::services::plugins::hooks::LspLocation {
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
        let args = crate::services::plugins::hooks::HookArgs::LspReferences {
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

    /// Apply LSP text edits to a buffer and return the number of changes made.
    /// Edits are sorted in reverse order and applied as a batch.
    fn apply_lsp_text_edits(
        &mut self,
        buffer_id: BufferId,
        mut edits: Vec<lsp_types::TextEdit>,
    ) -> io::Result<usize> {
        if edits.is_empty() {
            return Ok(0);
        }

        // Sort edits by position (reverse order to avoid offset issues)
        edits.sort_by(|a, b| {
            b.range
                .start
                .line
                .cmp(&a.range.start.line)
                .then(b.range.start.character.cmp(&a.range.start.character))
        });

        // Collect all events for this buffer into a batch
        let mut batch_events = Vec::new();
        let mut changes = 0;

        // Create events for all edits
        for edit in edits {
            let state = self
                .buffers
                .get_mut(&buffer_id)
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Buffer not found"))?;

            // Convert LSP range to byte positions
            let start_line = edit.range.start.line as usize;
            let start_char = edit.range.start.character as usize;
            let end_line = edit.range.end.line as usize;
            let end_char = edit.range.end.character as usize;

            let start_pos = state.buffer.lsp_position_to_byte(start_line, start_char);
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
            tracing::debug!(
                "  Converting LSP range line {}:{}-{}:{} to bytes {}..{} (replacing {:?} with {:?})",
                start_line, start_char, end_line, end_char,
                start_pos, end_pos, old_text, edit.new_text
            );

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
                let state = self
                    .buffers
                    .get(&buffer_id)
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Buffer not found"))?;
                let cursor_id = state.cursors.primary_id();
                let insert_event = Event::Insert {
                    position: start_pos,
                    text: edit.new_text.clone(),
                    cursor_id,
                };
                batch_events.push(insert_event);
            }

            changes += 1;
        }

        // Create a batch event for all rename changes
        if !batch_events.is_empty() {
            let batch = Event::Batch {
                events: batch_events,
                description: "LSP Rename".to_string(),
            };
            self.apply_rename_batch_to_buffer(buffer_id, batch)?;
        }

        Ok(changes)
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
                            let buffer_id = self.open_file(&path)?;
                            total_changes += self.apply_lsp_text_edits(buffer_id, edits)?;
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

                            total_changes += self.apply_lsp_text_edits(buffer_id, edits)?;
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
        let state = self
            .buffers
            .get(&buffer_id)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Buffer not found"))?;
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
                        let adjusted_cursor =
                            (original_cursor_pos as isize + cursor_delta) as usize;
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
        let state = self
            .buffers
            .get_mut(&buffer_id)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Buffer not found"))?;
        state.apply(&batch);

        // Restore cursor to adjusted position
        let buffer_len = state.buffer.len();
        let new_cursor_pos =
            ((original_cursor_pos as isize + cursor_delta).max(0) as usize).min(buffer_len);
        state.cursors.primary_mut().position = new_cursor_pos;

        // Adjust anchor if there was a selection
        if let Some(anchor) = original_cursor_anchor {
            let new_anchor = ((anchor as isize + cursor_delta).max(0) as usize).min(buffer_len);
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
        use crate::primitives::word_navigation::{find_word_end, find_word_start};

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
        let overlay_handle = self.add_overlay(
            None,
            word_start..word_end,
            crate::model::event::OverlayFace::Background {
                color: (50, 100, 200), // Blue background for rename
            },
            100,
            Some("Renaming".to_string()),
        );

        // Enter rename mode using the Prompt system
        // Store the rename metadata in the PromptType and pre-fill the input with the current name
        let mut prompt = Prompt::new(
            "Rename to: ".to_string(),
            PromptType::LspRename {
                original_text: word_text.clone(),
                start_pos: word_start,
                end_pos: word_end,
                overlay_handle,
            },
        );
        // Pre-fill the input with the current name and position cursor at the end
        prompt.set_input(word_text);

        self.prompt = Some(prompt);
        Ok(())
    }

    /// Cancel rename mode - removes overlay if the prompt was for LSP rename
    fn cancel_rename_overlay(&mut self, handle: &crate::view::overlay::OverlayHandle) {
        self.remove_overlay(handle.clone());
    }

    /// Perform the actual LSP rename request
    fn perform_lsp_rename(
        &mut self,
        new_name: String,
        original_text: String,
        start_pos: usize,
        overlay_handle: crate::view::overlay::OverlayHandle,
    ) {
        // Remove the overlay first
        self.cancel_rename_overlay(&overlay_handle);

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
            if let Some(language) = crate::services::lsp::manager::detect_language(path) {
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
    #[ignore]
    fn test_clipboard() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

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
        use crate::model::event::CursorId;

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
        assert_eq!(
            Action::from_str("smart_home", &args),
            Some(Action::SmartHome)
        );
        assert_eq!(
            Action::from_str("indent_selection", &args),
            Some(Action::IndentSelection)
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
        use crate::model::buffer::Buffer;

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
        let buffer_text = editor.active_state().buffer.to_string().unwrap();
        let text_at_cursor = buffer_text[original_cursor_pos..original_cursor_pos + 3].to_string();
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
        editor
            .apply_rename_batch_to_buffer(buffer_id, batch)
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

        // Apply first rename
        editor
            .apply_rename_batch_to_buffer(buffer_id, batch1)
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

        // Apply second rename
        editor
            .apply_rename_batch_to_buffer(buffer_id, batch2)
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
        let mut editor = Editor::new(config, 80, 24).unwrap();
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
