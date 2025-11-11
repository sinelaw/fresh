use crate::actions::action_to_events as convert_action_to_events;
use crate::async_bridge::{AsyncBridge, AsyncMessage};
use crate::command_registry::CommandRegistry;
use crate::commands::Suggestion;
use crate::config::Config;
use crate::event::{CursorId, Event, EventLog, SplitId};
use crate::file_tree::{FileTree, FileTreeView};
use crate::fs::{FsBackend, FsManager, LocalFsBackend};
use crate::hooks::HookRegistry;
use crate::keybindings::{Action, KeyContext, KeybindingResolver};
use crate::lsp_diagnostics;
use crate::lsp_manager::{detect_language, LspManager};
use crate::multi_cursor::{
    add_cursor_above, add_cursor_at_next_match, add_cursor_below, AddCursorResult,
};
use crate::plugin_api::PluginCommand;
use crate::plugin_manager::PluginManager;
use crate::position_history::PositionHistory;
use crate::prompt::{Prompt, PromptType};
use crate::split::SplitManager;
use crate::state::EditorState;
use crate::ui::{
    FileExplorerRenderer, HelpRenderer, SplitRenderer, StatusBarRenderer, SuggestionsRenderer,
    TabsRenderer,
};
use lsp_types::TextDocumentContentChangeEvent;
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

/// Rename state for inline renaming
#[derive(Debug, Clone)]
struct RenameState {
    /// The current text being edited
    current_text: String,
    /// The original symbol text (before editing)
    original_text: String,
    /// Start position of the symbol in the buffer
    start_pos: usize,
    /// End position of the symbol in the buffer
    end_pos: usize,
    /// Overlay ID for visual indication
    overlay_id: String,
}

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

/// Metadata associated with a buffer
#[derive(Debug, Clone)]
pub struct BufferMetadata {
    /// File path (if the buffer is associated with a file)
    pub file_path: Option<PathBuf>,

    /// File URI for LSP (computed once from absolute path)
    pub file_uri: Option<lsp_types::Uri>,

    /// Display name for the buffer (project-relative path or filename)
    pub display_name: String,

    /// Whether LSP is enabled for this buffer
    pub lsp_enabled: bool,

    /// Reason LSP is disabled (if applicable)
    pub lsp_disabled_reason: Option<String>,
}

impl BufferMetadata {
    /// Create new metadata for a buffer
    pub fn new() -> Self {
        Self {
            file_path: None,
            file_uri: None,
            display_name: "[No Name]".to_string(),
            lsp_enabled: true,
            lsp_disabled_reason: None,
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
            file_path: Some(path),
            file_uri,
            display_name,
            lsp_enabled: true,
            lsp_disabled_reason: None,
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

    /// Tokio runtime for async I/O tasks
    tokio_runtime: Option<tokio::runtime::Runtime>,

    /// Bridge for async messages from tokio tasks to main loop
    async_bridge: Option<AsyncBridge>,

    /// Split view manager
    split_manager: SplitManager,

    /// File explorer view (optional, only when open)
    file_explorer: Option<FileTreeView>,

    /// Filesystem manager for file explorer
    fs_manager: Arc<FsManager>,

    /// Whether file explorer is visible
    file_explorer_visible: bool,

    /// Current keybinding context
    key_context: KeyContext,

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

    /// Rename state (if rename is active)
    rename_state: Option<RenameState>,

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

    /// Hook registry for plugins
    hook_registry: Arc<RwLock<HookRegistry>>,

    /// Command registry for dynamic commands
    command_registry: Arc<RwLock<CommandRegistry>>,

    /// Plugin manager
    plugin_manager: Option<PluginManager>,

    /// Search history (for search and find operations)
    search_history: crate::input_history::InputHistory,

    /// Replace history (for replace operations)
    replace_history: crate::input_history::InputHistory,
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
        let mut state = EditorState::new(width, height);
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

        // Initialize filesystem manager for file explorer
        // Use provided backend or create default LocalFsBackend
        let fs_backend = fs_backend.unwrap_or_else(|| Arc::new(LocalFsBackend::new()));
        let fs_manager = Arc::new(FsManager::new(fs_backend));

        // Initialize plugin system
        let hook_registry = Arc::new(RwLock::new(HookRegistry::new()));
        let command_registry = Arc::new(RwLock::new(CommandRegistry::new()));

        let mut plugin_manager =
            PluginManager::new(Arc::clone(&hook_registry), Arc::clone(&command_registry)).ok();

        if let Some(ref mut manager) = plugin_manager {
            // Set async bridge sender for spawn support
            manager.set_async_sender(async_bridge.sender());

            // Try to load plugins from the plugins directory
            let plugin_dir = working_dir.join("plugins");
            if plugin_dir.exists() {
                tracing::info!("Loading plugins from: {:?}", plugin_dir);
                let errors = manager.load_plugins_from_dir(&plugin_dir);
                if !errors.is_empty() {
                    for err in errors {
                        tracing::warn!("Plugin load error: {}", err);
                    }
                }
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
            tokio_runtime,
            async_bridge: Some(async_bridge),
            split_manager,
            file_explorer: None,
            fs_manager,
            file_explorer_visible: false,
            key_context: KeyContext::Normal,
            working_dir,
            position_history: PositionHistory::new(),
            in_navigation: false,
            next_lsp_request_id: 0,
            pending_completion_request: None,
            pending_goto_definition_request: None,
            rename_state: None,
            search_state: None,
            interactive_replace_state: None,
            lsp_status: String::new(),
            mouse_state: MouseState::default(),
            cached_layout: CachedLayout::default(),
            hook_registry,
            command_registry,
            plugin_manager,
            search_history: crate::input_history::InputHistory::new(),
            replace_history: crate::input_history::InputHistory::new(),
        })
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

        let mut state = EditorState::from_file(path, self.terminal_width, self.terminal_height)?;
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
                if let Some(uri) = &metadata.file_uri {
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
        self.status_message = Some(format!("Opened {}", path.display()));

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

        let mut state = EditorState::new(self.terminal_width, self.terminal_height);
        state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        self.set_active_buffer(buffer_id);
        self.status_message = Some("New buffer".to_string());

        buffer_id
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

        self.buffers.remove(&id);
        self.event_logs.remove(&id);

        // Switch to another buffer if we closed the active one
        if self.active_buffer == id {
            let next_buffer = *self.buffers.keys().next().unwrap();
            self.set_active_buffer(next_buffer);
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

    /// Switch to next buffer
    pub fn next_buffer(&mut self) {
        let mut ids: Vec<_> = self.buffers.keys().copied().collect();
        ids.sort_by_key(|id| id.0); // Sort by buffer ID to ensure consistent order
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

    /// Switch to previous buffer
    pub fn prev_buffer(&mut self) {
        let mut ids: Vec<_> = self.buffers.keys().copied().collect();
        ids.sort_by_key(|id| id.0); // Sort by buffer ID to ensure consistent order
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
        // Create a new buffer for the new split
        let new_buffer_id = self.new_buffer();

        // Split the pane
        if let Err(e) = self.split_manager.split_active(
            crate::event::SplitDirection::Horizontal,
            new_buffer_id,
            0.5,
        ) {
            self.set_status_message(format!("Error splitting pane: {}", e));
        } else {
            self.set_status_message("Split pane horizontally".to_string());
        }
    }

    /// Split the current pane vertically
    pub fn split_pane_vertical(&mut self) {
        // Create a new buffer for the new split
        let new_buffer_id = self.new_buffer();

        // Split the pane
        if let Err(e) = self.split_manager.split_active(
            crate::event::SplitDirection::Vertical,
            new_buffer_id,
            0.5,
        ) {
            self.set_status_message(format!("Error splitting pane: {}", e));
        } else {
            self.set_status_message("Split pane vertically".to_string());
        }
    }

    /// Close the active split
    pub fn close_active_split(&mut self) {
        let active_split = self.split_manager.active_split();
        match self.split_manager.close_split(active_split) {
            Ok(_) => {
                self.set_status_message("Closed split".to_string());
            }
            Err(e) => {
                self.set_status_message(format!("Cannot close split: {}", e));
            }
        }
    }

    /// Switch to next split
    pub fn next_split(&mut self) {
        self.split_manager.next_split();
        self.set_status_message("Switched to next split".to_string());
    }

    /// Switch to previous split
    pub fn prev_split(&mut self) {
        self.split_manager.prev_split();
        self.set_status_message("Switched to previous split".to_string());
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
            if let Some(file_path) = &metadata.file_path {
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
        // 1. Apply the event to the buffer
        self.active_state_mut().apply(event);

        // 2. Clear search highlights on edit (Insert/Delete events)
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

        // 4. Notify LSP of the change
        self.notify_lsp_change(event);
    }

    /// Trigger plugin hooks for an event (if any)
    fn trigger_plugin_hooks_for_event(&mut self, event: &Event) {
        use crate::event_hooks::EventHooks;
        use crate::hooks::HookArgs;

        if self.plugin_manager.is_some() {
            // Update plugin state snapshot BEFORE calling hooks
            // so plugins can query current state
            self.update_plugin_state_snapshot();

            // Trigger "after" hooks for this event
            if let Some(hook_args) = event.after_hook(self.active_buffer) {
                let hook_name = match &hook_args {
                    HookArgs::AfterInsert { .. } => "after-insert",
                    HookArgs::AfterDelete { .. } => "after-delete",
                    _ => "",
                };

                if !hook_name.is_empty() {
                    if let Some(plugin_manager) = &self.plugin_manager {
                        if let Err(e) = plugin_manager.run_hook(hook_name, &hook_args) {
                            tracing::warn!("Plugin hook '{}' error: {}", hook_name, e);
                        }
                    }
                }
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
        let state = self.active_state();
        let mut text = String::new();

        for (_, cursor) in state.cursors.iter() {
            if let Some(range) = cursor.selection_range() {
                if !text.is_empty() {
                    text.push('\n');
                }
                text.push_str(&state.buffer.slice(range));
            }
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
        let events: Vec<_> = deletions
            .iter()
            .rev()
            .map(|range| {
                let state = self.active_state();
                Event::Delete {
                    range: range.clone(),
                    deleted_text: state.buffer.slice(range.clone()),
                    cursor_id: state.cursors.primary_id(),
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
        let state = self.active_state();
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
        let state = self.active_state();
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
        let state = self.active_state();
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
        self.active_state_mut().buffer.save()?;
        self.status_message = Some("Saved".to_string());

        // Notify LSP of save
        self.notify_lsp_save();

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
            match prompt.prompt_type {
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                    self.search_history.reset_navigation();
                    self.clear_search_highlights();
                }
                PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                    self.replace_history.reset_navigation();
                }
                _ => {}
            }
        }

        self.prompt = None;
        self.status_message = Some("Canceled".to_string());
    }

    /// Get the confirmed input and prompt type, consuming the prompt
    /// For command palette, returns the selected suggestion if available, otherwise the raw input
    /// Returns None if trying to confirm a disabled command
    pub fn confirm_prompt(&mut self) -> Option<(String, PromptType)> {
        if let Some(prompt) = self.prompt.take() {
            // For command prompts, prefer the selected suggestion over raw input
            let final_input = if matches!(
                prompt.prompt_type,
                PromptType::Command | PromptType::GitGrep | PromptType::GitFindFile
            ) {
                // For Command, GitGrep, and GitFindFile, use the selected suggestion if any
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

            Some((final_input, prompt.prompt_type))
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
            PromptType::GitGrep => {
                // Trigger async git grep
                self.request_git_grep(input);
            }
            PromptType::GitFindFile => {
                // Trigger async git ls-files with query
                self.request_git_ls_files(input);
            }
            PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                // Update incremental search highlights as user types
                self.update_search_highlights(&input);
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
    pub fn process_async_messages(&mut self) {
        let Some(bridge) = &self.async_bridge else {
            return;
        };

        let messages = bridge.try_recv_all();

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
                            .find(|(_, m)| m.file_uri.as_ref() == Some(&diagnostic_url))
                        {
                            // Convert diagnostics to overlays
                            if let Some(state) = self.buffers.get_mut(buffer_id) {
                                lsp_diagnostics::apply_diagnostics_to_state(
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
                AsyncMessage::FileChanged { path } => {
                    tracing::info!("File changed externally: {}", path);
                    // TODO: Handle external file changes
                }
                AsyncMessage::GitStatusChanged { status } => {
                    tracing::info!("Git status changed: {}", status);
                    // TODO: Handle git status changes
                }
                AsyncMessage::GitGrepResults { query, results } => {
                    // Update prompt suggestions with git grep results
                    if let Some(prompt) = &mut self.prompt {
                        if matches!(prompt.prompt_type, PromptType::GitGrep)
                            && prompt.input == query
                        {
                            // Convert git grep results to suggestions
                            prompt.suggestions = results
                                .into_iter()
                                .map(|m| {
                                    let text = format!("{}:{}:{}", m.file, m.line, m.column);
                                    Suggestion::with_description(text, m.content)
                                })
                                .collect();

                            // Select first suggestion if any
                            prompt.selected_suggestion = if prompt.suggestions.is_empty() {
                                None
                            } else {
                                Some(0)
                            };
                        }
                    }
                }
                AsyncMessage::GitLsFilesResults { query, files } => {
                    // Update prompt suggestions with git ls-files results
                    if let Some(prompt) = &mut self.prompt {
                        if matches!(prompt.prompt_type, PromptType::GitFindFile)
                            && prompt.input == query
                        {
                            // Convert file list to suggestions
                            prompt.suggestions = files
                                .into_iter()
                                .map(|file| Suggestion::new(file))
                                .collect();

                            // Select first suggestion if any
                            prompt.selected_suggestion = if prompt.suggestions.is_empty() {
                                None
                            } else {
                                Some(0)
                            };
                        }
                    }
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
                    // Plugin process completed - execute callback
                    if let Some(ref mut manager) = self.plugin_manager {
                        if let Err(e) =
                            manager.execute_process_callback(process_id, stdout, stderr, exit_code)
                        {
                            tracing::error!("Error executing process callback: {}", e);
                        }
                    }
                }
            }
        }

        // Process plugin commands and update snapshot only if commands were processed
        let mut processed_any_commands = false;
        if let Some(ref mut manager) = self.plugin_manager {
            let commands = manager.process_commands();
            if !commands.is_empty() {
                processed_any_commands = true;
                for command in commands {
                    if let Err(e) = self.handle_plugin_command(command) {
                        tracing::error!("Error handling plugin command: {}", e);
                    }
                }
            }
        }

        // Only update snapshot if commands were processed (which may have modified buffers)
        if processed_any_commands {
            self.update_plugin_state_snapshot();
        }
    }

    /// Update the plugin state snapshot with current editor state
    fn update_plugin_state_snapshot(&mut self) {
        if let Some(ref manager) = self.plugin_manager {
            use crate::plugin_api::{BufferInfo, CursorInfo, ViewportInfo};

            let snapshot_handle = manager.state_snapshot_handle();
            let mut snapshot = snapshot_handle.write().unwrap();

            // Update active buffer ID
            snapshot.active_buffer_id = self.active_buffer;

            // Clear and update buffer info (buffer content is in a separate cache now)
            snapshot.buffers.clear();

            for (buffer_id, state) in &self.buffers {
                let buffer_info = BufferInfo {
                    id: *buffer_id,
                    path: state.buffer.file_path().map(|p| p.to_path_buf()),
                    modified: state.buffer.is_modified(),
                    length: state.buffer.len(),
                };
                snapshot.buffers.insert(*buffer_id, buffer_info);
            }

            // TODO: Buffer content cache was removed due to fundamental performance issues.
            //
            // The previous implementation called buffer.to_string() on every update, which:
            // - Copies the entire buffer (expensive for large files - 61MB took 3.9 seconds!)
            // - Happened multiple times per keystroke (once per event, once in process_async_messages)
            //
            // If we ever need a buffer cache for plugins, it MUST update incrementally:
            // - Listen to Insert/Delete/Batch events and apply them to the cached string
            // - Avoid calling buffer.to_string() except on initial cache population
            // - This way, text insertion propagates as events to both the buffer AND the cache
            //
            // For now, plugins can call get_buffer_content() which will fetch on-demand.
            // This is acceptable since plugins typically don't need full buffer content often.

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
                    let deleted_text = state.buffer.slice(range.clone()).to_string();
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
                callback_id,
            } => {
                // Spawn async process via plugin manager
                if let Some(ref mut manager) = self.plugin_manager {
                    if let Err(e) = manager.spawn_process(command, args, cwd, callback_id) {
                        tracing::error!("Failed to spawn process: {}", e);
                    }
                }
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
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;
        let word_start = find_completion_word_start(&state.buffer, cursor_pos);
        let prefix = if word_start < cursor_pos {
            state
                .buffer
                .slice(word_start..cursor_pos)
                .to_string()
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
        if self.pending_completion_request.is_some() {
            tracing::debug!("Canceling pending LSP completion request");
            self.pending_completion_request = None;
            self.lsp_status.clear();
        }
        if self.pending_goto_definition_request.is_some() {
            tracing::debug!("Canceling pending LSP goto-definition request");
            self.pending_goto_definition_request = None;
            self.lsp_status.clear();
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
            (meta.file_uri.as_ref(), meta.file_path.as_ref())
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
            (meta.file_uri.as_ref(), meta.file_path.as_ref())
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
                                let state = self.buffers.get(&buffer_id).ok_or_else(|| {
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

                                // Log the conversion for debugging
                                let old_text =
                                    if start_pos < end_pos && end_pos <= state.buffer.len() {
                                        state.buffer.slice(start_pos..end_pos).to_string()
                                    } else {
                                        format!(
                                            "<invalid range: start={}, end={}, buffer_len={}>",
                                            start_pos,
                                            end_pos,
                                            state.buffer.len()
                                        )
                                    };
                                tracing::debug!("  Converting LSP range line {}:{}-{}:{} to bytes {}..{} (replacing {:?} with {:?})",
                                    start_line, start_char, end_line, end_char,
                                    start_pos, end_pos, old_text, edit.new_text);

                                // Delete old text
                                if start_pos < end_pos {
                                    let deleted_text =
                                        state.buffer.slice(start_pos..end_pos).to_string();
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

                                // Add to event log and apply to state
                                if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
                                    event_log.append(batch.clone());
                                }

                                let state = self.buffers.get_mut(&buffer_id).ok_or_else(|| {
                                    io::Error::new(io::ErrorKind::NotFound, "Buffer not found")
                                })?;
                                state.apply(&batch);
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
                                let state = self.buffers.get(&buffer_id).ok_or_else(|| {
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

                                // Log the conversion for debugging
                                let old_text =
                                    if start_pos < end_pos && end_pos <= state.buffer.len() {
                                        state.buffer.slice(start_pos..end_pos).to_string()
                                    } else {
                                        format!(
                                            "<invalid range: start={}, end={}, buffer_len={}>",
                                            start_pos,
                                            end_pos,
                                            state.buffer.len()
                                        )
                                    };
                                tracing::debug!("  Converting LSP range line {}:{}-{}:{} to bytes {}..{} (replacing {:?} with {:?})",
                                    start_line, start_char, end_line, end_char,
                                    start_pos, end_pos, old_text, edit.new_text);

                                // Delete old text
                                if start_pos < end_pos {
                                    let deleted_text =
                                        state.buffer.slice(start_pos..end_pos).to_string();
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

                                // Add to event log and apply to state
                                if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
                                    event_log.append(batch.clone());
                                }

                                let state = self.buffers.get_mut(&buffer_id).ok_or_else(|| {
                                    io::Error::new(io::ErrorKind::NotFound, "Buffer not found")
                                })?;
                                state.apply(&batch);
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

    /// Request git grep results for a query
    fn request_git_grep(&mut self, query: String) {
        if let (Some(bridge), Some(runtime)) = (&self.async_bridge, &self.tokio_runtime) {
            let sender = bridge.sender();
            let query_clone = query.clone();

            // Spawn async task to run git grep
            runtime.spawn(async move {
                crate::git::git_grep(query_clone, sender).await;
            });
        }
    }

    /// Request git ls-files results filtered by query
    fn request_git_ls_files(&mut self, query: String) {
        if let (Some(bridge), Some(runtime)) = (&self.async_bridge, &self.tokio_runtime) {
            let sender = bridge.sender();
            let query_clone = query.clone();

            // Spawn async task to run git ls-files
            runtime.spawn(async move {
                crate::git::git_ls_files(query_clone, sender).await;
            });
        }
    }

    /// Start rename mode - select the symbol at cursor and allow inline editing
    fn start_rename(&mut self) -> io::Result<()> {
        use crate::word_navigation::{find_word_end, find_word_start};

        // Get the current buffer and cursor position
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

        // Get the word text
        let word_text = state.buffer.slice(word_start..word_end).to_string();

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

        // Enter rename mode
        self.rename_state = Some(RenameState {
            current_text: word_text.clone(),
            original_text: word_text,
            start_pos: word_start,
            end_pos: word_end,
            overlay_id,
        });

        self.status_message = Some("Rename mode (Enter to confirm, Esc to cancel)".to_string());
        Ok(())
    }

    /// Cancel rename mode
    fn cancel_rename(&mut self) {
        if let Some(rename_state) = self.rename_state.take() {
            // The buffer was never modified during rename mode, so no need to restore
            // Just remove the overlay and clear the state
            let remove_overlay_event = crate::event::Event::RemoveOverlay {
                overlay_id: rename_state.overlay_id,
            };
            self.apply_event_to_active_buffer(&remove_overlay_event);

            self.status_message = Some("Rename cancelled".to_string());
        }
    }

    /// Confirm rename - send request to LSP
    fn confirm_rename(&mut self) -> io::Result<()> {
        if let Some(rename_state) = self.rename_state.take() {
            // Remove the overlay first
            let event = crate::event::Event::RemoveOverlay {
                overlay_id: rename_state.overlay_id.clone(),
            };
            if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
                state.apply(&event);
            }

            // Check if the name actually changed
            if rename_state.current_text == rename_state.original_text {
                self.status_message = Some("Name unchanged".to_string());
                return Ok(());
            }

            // Use the position from when we entered rename mode, NOT the current cursor position
            // This ensures we send the rename request for the correct symbol even if cursor moved
            let rename_pos = rename_state.start_pos;

            // Convert byte position to LSP position (line, UTF-16 code units)
            // LSP uses UTF-16 code units for character offsets, not byte offsets
            let state = self.active_state();
            let (line, character) = state.buffer.position_to_lsp_position(rename_pos);

            // Get the current file URI and path
            let metadata = self.buffer_metadata.get(&self.active_buffer);
            let (uri, file_path) = if let Some(meta) = metadata {
                (meta.file_uri.as_ref(), meta.file_path.as_ref())
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
                                rename_state.current_text.clone(),
                            );
                            tracing::info!(
                                "Requested rename at {}:{}:{} to '{}'",
                                uri.as_str(),
                                line,
                                character,
                                rename_state.current_text
                            );
                        }
                    }
                }
            } else {
                self.status_message = Some("Cannot rename in unsaved buffer".to_string());
            }
        }

        Ok(())
    }

    /// Determine the current keybinding context based on UI state
    fn get_key_context(&self) -> crate::keybindings::KeyContext {
        use crate::keybindings::KeyContext;

        // Priority order: Help > Prompt > Popup > Rename > Current context (FileExplorer or Normal)
        if self.help_renderer.is_visible() {
            KeyContext::Help
        } else if self.is_prompting() {
            KeyContext::Prompt
        } else if self.active_state().popups.is_visible() {
            KeyContext::Popup
        } else if self.rename_state.is_some() {
            KeyContext::Rename
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

        use std::path::Path;

        let _t_total = std::time::Instant::now();

        tracing::debug!(
            "Editor.handle_key: code={:?}, modifiers={:?}",
            code,
            modifiers
        );

        // Determine the current context
        let context = self.get_key_context();

        // Resolve the key event to an action
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let action = self.keybindings.resolve(&key_event, context);

        tracing::debug!("Context: {:?} -> Action: {:?}", context, action);

        // Cancel pending LSP requests on user actions (except LSP actions themselves)
        // This ensures stale completions don't show up after the user has moved on
        match action {
            Action::LspCompletion | Action::LspGotoDefinition | Action::None => {
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

            // Prompt mode actions
            Action::PromptConfirm => {
                if let Some((input, prompt_type)) = self.confirm_prompt() {
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
                            // Perform search with the given query
                            self.perform_search(&input);
                        }
                        PromptType::ReplaceSearch => {
                            // User entered search query for replace, now prompt for replacement text
                            // First perform the search to highlight matches
                            self.perform_search(&input);
                            // Then open the replacement prompt
                            self.start_prompt(
                                format!("Replace '{}' with: ", input),
                                PromptType::Replace {
                                    search: input.clone(),
                                },
                            );
                        }
                        PromptType::Replace { search } => {
                            // Perform replace of search term with input
                            self.perform_replace(&search, &input);
                        }
                        PromptType::QueryReplaceSearch => {
                            // User entered search query for query-replace, now prompt for replacement text
                            // First perform the search to highlight matches
                            self.perform_search(&input);
                            // Then open the replacement prompt
                            self.start_prompt(
                                format!("Query replace '{}' with: ", input),
                                PromptType::QueryReplace {
                                    search: input.clone(),
                                },
                            );
                        }
                        PromptType::QueryReplace { search } => {
                            // Start interactive replace mode
                            self.start_interactive_replace(&search, &input);
                        }
                        PromptType::Command => {
                            let commands = self.command_registry.read().unwrap().get_all();

                            // input now contains the selected command name (from confirm_prompt)
                            if let Some(cmd) = commands.iter().find(|c| c.name == input) {
                                let action = cmd.action.clone();
                                self.set_status_message(format!("Executing: {}", cmd.name));
                                // Recursively handle the command action
                                return self.handle_action(action);
                            } else {
                                self.set_status_message(format!("Unknown command: {input}"));
                            }
                        }
                        PromptType::GitGrep => {
                            // Parse the selected result: "file:line:column"
                            let parts: Vec<&str> = input.split(':').collect();
                            if parts.len() >= 3 {
                                let file = parts[0];
                                let line = parts[1].parse::<usize>().unwrap_or(1);
                                let column = parts[2].parse::<usize>().unwrap_or(1);

                                // Open the file
                                let path = Path::new(file);
                                if let Err(e) = self.open_file(path) {
                                    self.set_status_message(format!("Error opening file: {e}"));
                                } else {
                                    // Jump to the line and column
                                    let state = self.active_state_mut();

                                    // Find the byte position for the target line
                                    // Git grep returns 1-indexed line numbers and columns
                                    let target_line = line.saturating_sub(1); // Convert to 0-indexed
                                    let column_offset = column.saturating_sub(1); // Convert to 0-indexed
                                    let mut iter = state.buffer.line_iterator(0);
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
                                    // Git grep --column returns byte offsets from line start
                                    let final_position = target_byte + column_offset;

                                    // Ensure we don't go past the buffer end
                                    let buffer_len = state.buffer.len();
                                    state.cursors.primary_mut().position =
                                        final_position.min(buffer_len);
                                    state.cursors.primary_mut().anchor = None;

                                    // Ensure the position is visible
                                    state
                                        .viewport
                                        .ensure_visible(&mut state.buffer, state.cursors.primary());

                                    self.set_status_message(format!(
                                        "Jumped to {}:{}:{}",
                                        file, line, column
                                    ));
                                }
                            } else {
                                self.set_status_message(format!(
                                    "Invalid git grep result format: '{input}'"
                                ));
                            }
                        }
                        PromptType::GitFindFile => {
                            // Open the selected file
                            let path = Path::new(&input);
                            if let Err(e) = self.open_file(path) {
                                self.set_status_message(format!("Error opening file: {e}"));
                            } else {
                                self.set_status_message(format!("Opened: {input}"));
                            }
                        }
                    }
                }
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
                // If it's a completion popup, insert the selected item
                // Clone the completion text first to avoid borrow checker issues
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

                    let state = self.active_state();
                    let cursor_id = state.cursors.primary_id();
                    let cursor_pos = state.cursors.primary().position;

                    // Find the start of the current completion word (stops at delimiters like '.')
                    let word_start = find_completion_word_start(&state.buffer, cursor_pos);

                    // Get the text being deleted (if any) before we mutate
                    let deleted_text = if word_start < cursor_pos {
                        state.buffer.slice(word_start..cursor_pos).to_string()
                    } else {
                        String::new()
                    };

                    // Now we can mutate - delete the partial word and insert completion
                    if word_start < cursor_pos {
                        // Delete the partial word
                        let delete_event = crate::event::Event::Delete {
                            range: word_start..cursor_pos,
                            deleted_text,
                            cursor_id,
                        };

                        self.active_event_log_mut().append(delete_event.clone());
                        self.apply_event_to_active_buffer(&delete_event);

                        // After deletion, ensure insert position is valid
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
                        // No partial word to delete, just insert
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

        match action {
            Action::Quit => self.quit(),
            Action::Save => self.save()?,
            Action::Open => self.start_prompt("Find file: ".to_string(), PromptType::OpenFile),
            Action::New => {
                self.new_buffer();
            }
            Action::Copy => self.copy_selection(),
            Action::Cut => self.cut_selection(),
            Action::Paste => self.paste(),
            Action::Undo => {
                let events = self.active_event_log_mut().undo();
                // Apply all inverse events collected during undo
                for event in events {
                    self.apply_event_to_active_buffer(&event);
                }
            }
            Action::Redo => {
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
            Action::RenameConfirm => {
                self.confirm_rename()?;
            }
            Action::RenameCancel => {
                self.cancel_rename();
            }
            Action::RenameMoveLeft => {
                // Move cursor left, but constrain to rename boundaries
                if let Some(rename_state) = &self.rename_state {
                    let current_pos = self.active_state().cursors.primary().position;
                    if current_pos > rename_state.start_pos {
                        // Use prev_char_boundary to ensure we land on a valid UTF-8 character boundary
                        let new_pos = self
                            .active_state()
                            .buffer
                            .prev_char_boundary(current_pos)
                            .max(rename_state.start_pos);
                        let event = Event::MoveCursor {
                            cursor_id: self.active_state().cursors.primary_id(),
                            old_position: 0, // TODO: Get actual old position
                            new_position: new_pos,
                            old_anchor: None, // TODO: Get actual old anchor
                            new_anchor: None,
                            old_sticky_column: 0,
                            new_sticky_column: 0, // Reset sticky column
                        };
                        self.apply_event_to_active_buffer(&event);
                    }
                }
            }
            Action::RenameMoveRight => {
                // Move cursor right, but constrain to rename boundaries
                if let Some(rename_state) = &self.rename_state {
                    let current_pos = self.active_state().cursors.primary().position;
                    if current_pos < rename_state.end_pos {
                        // Use next_char_boundary to ensure we land on a valid UTF-8 character boundary
                        let new_pos = self
                            .active_state()
                            .buffer
                            .next_char_boundary(current_pos)
                            .min(rename_state.end_pos);
                        let event = Event::MoveCursor {
                            cursor_id: self.active_state().cursors.primary_id(),
                            old_position: 0, // TODO: Get actual old position
                            new_position: new_pos,
                            old_anchor: None, // TODO: Get actual old anchor
                            new_anchor: None,
                            old_sticky_column: 0,
                            new_sticky_column: 0, // Reset sticky column
                        };
                        self.apply_event_to_active_buffer(&event);
                    }
                }
            }
            Action::RenameMoveHome => {
                // Move cursor to start of rename range
                if let Some(rename_state) = &self.rename_state {
                    let event = Event::MoveCursor {
                        cursor_id: self.active_state().cursors.primary_id(),
                        old_position: 0, // TODO: Get actual old position
                        new_position: rename_state.start_pos,
                        old_anchor: None, // TODO: Get actual old anchor
                        new_anchor: None,
                        old_sticky_column: 0,
                        new_sticky_column: 0, // Reset sticky column
                    };
                    self.apply_event_to_active_buffer(&event);
                }
            }
            Action::RenameMoveEnd => {
                // Move cursor to end of rename range
                if let Some(rename_state) = &self.rename_state {
                    let event = Event::MoveCursor {
                        cursor_id: self.active_state().cursors.primary_id(),
                        old_position: 0, // TODO: Get actual old position
                        new_position: rename_state.end_pos,
                        old_anchor: None, // TODO: Get actual old anchor
                        new_anchor: None,
                        old_sticky_column: 0,
                        new_sticky_column: 0, // Reset sticky column
                    };
                    self.apply_event_to_active_buffer(&event);
                }
            }
            Action::GitGrep => {
                // Start git grep prompt with empty suggestions (will be populated as user types)
                self.start_prompt_with_suggestions(
                    "Git grep: ".to_string(),
                    PromptType::GitGrep,
                    vec![],
                );
            }
            Action::GitFindFile => {
                // Start git find file prompt and immediately request all files
                self.start_prompt_with_suggestions(
                    "Find file: ".to_string(),
                    PromptType::GitFindFile,
                    vec![],
                );
                // Trigger initial git ls-files with empty query
                self.request_git_ls_files(String::new());
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
            Action::None => {}
            Action::DeleteBackward => {
                // Handle backspace in rename mode
                if let Some(rename_state) = &mut self.rename_state {
                    if !rename_state.current_text.is_empty() {
                        // Just update the current_text, don't modify the buffer
                        rename_state.current_text.pop();
                        let new_text = rename_state.current_text.clone();

                        // Update status message to show what's being typed
                        self.status_message = Some(format!(
                            "Renaming to: {}",
                            if new_text.is_empty() {
                                "<empty>"
                            } else {
                                &new_text
                            }
                        ));
                    }
                } else {
                    // Normal backspace handling - fall through to default action handling below
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
            }
            Action::PluginAction(action_name) => {
                // Execute the plugin callback
                if let Some(ref manager) = self.plugin_manager {
                    match manager.execute_action(&action_name) {
                        Ok(()) => {
                            tracing::info!("Plugin action '{}' executed successfully", action_name);
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
            Action::InsertChar(c) => {
                // Handle character insertion in interactive replace mode
                if self.interactive_replace_state.is_some() {
                    return self.handle_interactive_replace_key(c);
                // Handle character insertion in rename mode
                } else if let Some(rename_state) = &mut self.rename_state {
                    // Just update the current_text, don't modify the buffer
                    rename_state.current_text.push(c);
                    let new_text = rename_state.current_text.clone();

                    // Update status message to show what's being typed
                    self.status_message = Some(format!("Renaming to: {}", new_text));
                // Handle character insertion in prompt mode
                } else if self.is_prompting() {
                    if let Some(prompt) = self.prompt_mut() {
                        // Use insert_str to properly handle selection deletion
                        let s = c.to_string();
                        prompt.insert_str(&s);
                    }
                    self.update_prompt_suggestions();
                } else {
                    // Normal mode character insertion
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
                }
            }
            _ => {
                // Convert action to events and apply them
                // Get description before moving action
                let action_description = format!("{:?}", action);

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

        // Cancel rename mode on any mouse interaction
        if self.rename_state.is_some() {
            self.cancel_rename();
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
            }
            _ => {
                // Ignore other mouse events for now
            }
        }

        self.mouse_state.last_position = Some((col, row));
        Ok(())
    }

    /// Handle mouse click (down event)
    fn handle_mouse_click(&mut self, col: u16, row: u16) -> std::io::Result<()> {
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
                    let mut iter = state.buffer.line_iterator(0);
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
            let iter = state.buffer.line_iterator(new_top_byte);
            let line_start = iter.current_position();

            // Set viewport top to this position
            state.viewport.top_byte = line_start;
        }

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
                    let mut iter = state.buffer.line_iterator(0);
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
            let iter = state.buffer.line_iterator(target_byte);
            let line_start = iter.current_position();

            // Apply scroll limiting
            // Use viewport.height (constant allocated rows) not visible_line_count (varies with content)
            // For large files, use byte-based max to avoid iterating entire buffer
            let max_top_byte = if buffer_len <= large_file_threshold {
                Self::calculate_max_scroll_position(&state.buffer, viewport_height)
            } else {
                buffer_len.saturating_sub(1)
            };
            let limited_line_start = line_start.min(max_top_byte);

            // Set viewport top to this position
            state.viewport.top_byte = limited_line_start;
        }

        Ok(())
    }

    /// Calculate the maximum allowed scroll position
    /// Ensures the last line is always at the bottom unless the buffer is smaller than viewport
    fn calculate_max_scroll_position(
        buffer: &crate::buffer::Buffer,
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
        let mut iter = buffer.line_iterator(0);
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
        let mut iter = buffer.line_iterator(0);
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
            let mut line_iter = state.buffer.line_iterator(state.viewport.top_byte);

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

        // Check if we need space for suggestions popup
        let suggestion_lines = if let Some(prompt) = &self.prompt {
            if !prompt.suggestions.is_empty() {
                // Show up to 10 suggestions
                prompt.suggestions.len().min(10)
            } else {
                0
            }
        } else {
            0
        };

        // Build main vertical layout: [main_content, suggestions?, status_bar]
        let mut constraints = vec![Constraint::Min(0)]; // Main content area
        if suggestion_lines > 0 {
            constraints.push(Constraint::Length(suggestion_lines as u16 + 2));
        }
        constraints.push(Constraint::Length(1)); // Status bar

        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints)
            .split(size);

        let main_content_area = main_chunks[0];
        let suggestions_idx = if suggestion_lines > 0 { Some(1) } else { None };
        let status_bar_idx = if suggestion_lines > 0 { 2 } else { 1 };

        // Split main content area based on file explorer visibility
        let tabs_area;
        let editor_content_area;

        if self.file_explorer_visible && self.file_explorer.is_some() {
            // Split horizontally: [file_explorer | editor_with_tabs]
            let horizontal_chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Percentage(30), // File explorer
                    Constraint::Percentage(70), // Editor area
                ])
                .split(main_content_area);

            self.cached_layout.file_explorer_area = Some(horizontal_chunks[0]);

            // Split editor area vertically: [tabs | content]
            let editor_vertical_chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(1), // Tabs
                    Constraint::Min(0),    // Content
                ])
                .split(horizontal_chunks[1]);

            tabs_area = editor_vertical_chunks[0];
            editor_content_area = editor_vertical_chunks[1];

            // Render file explorer
            if let Some(ref mut explorer) = self.file_explorer {
                let is_focused = self.key_context == KeyContext::FileExplorer;

                // Build set of files with unsaved changes
                let mut files_with_unsaved_changes = std::collections::HashSet::new();
                for (buffer_id, state) in &self.buffers {
                    if state.buffer.is_modified() {
                        if let Some(metadata) = self.buffer_metadata.get(buffer_id) {
                            if let Some(file_path) = &metadata.file_path {
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
            // No file explorer: split main content vertically: [tabs | content]
            self.cached_layout.file_explorer_area = None;

            let vertical_chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(1), // Tabs
                    Constraint::Min(0),    // Content
                ])
                .split(main_content_area);

            tabs_area = vertical_chunks[0];
            editor_content_area = vertical_chunks[1];
        }

        // Render tabs (same for both layouts)
        TabsRenderer::render(
            frame,
            tabs_area,
            &self.buffers,
            self.active_buffer,
            &self.theme,
        );

        // Render editor content (same for both layouts)
        let lsp_waiting = self.pending_completion_request.is_some()
            || self.pending_goto_definition_request.is_some();

        let split_areas = SplitRenderer::render_content(
            frame,
            editor_content_area,
            &self.split_manager,
            &mut self.buffers,
            &mut self.event_logs,
            &self.theme,
            lsp_waiting,
            self.config.editor.large_file_threshold_bytes,
            self.config.editor.line_wrap,
            Some(&self.hook_registry),
            self.plugin_manager.as_ref(),
        );
        self.cached_layout.split_areas = split_areas;
        self.cached_layout.editor_content_area = Some(editor_content_area);

        // Render suggestions if present (same for both layouts)
        if let Some(idx) = suggestions_idx {
            if let Some(prompt) = &self.prompt {
                SuggestionsRenderer::render(frame, main_chunks[idx], prompt, &self.theme);
            }
        }

        // Render status bar (same for both layouts)
        let display_name = self
            .buffer_metadata
            .get(&self.active_buffer)
            .map(|m| m.display_name.as_str())
            .unwrap_or("[No Name]");
        StatusBarRenderer::render(
            frame,
            main_chunks[status_bar_idx],
            self.active_state(),
            &self.status_message,
            &self.prompt,
            &self.lsp_status,
            &self.theme,
            display_name,
        );

        // Render popups from the active buffer state
        // Clone theme to avoid borrow checker issues with active_state_mut()
        let theme_clone = self.theme.clone();
        let state = self.active_state_mut();
        if state.popups.is_visible() {
            // Get the primary cursor position for popup positioning
            let primary_cursor = state.cursors.primary();
            let cursor_screen_pos = state
                .viewport
                .cursor_screen_position(&mut state.buffer, primary_cursor);

            // Adjust cursor position to account for tab bar (1 line offset)
            let cursor_screen_pos = (cursor_screen_pos.0, cursor_screen_pos.1 + 1);

            // Render all popups (bottom to top)
            for popup in state.popups.all() {
                let popup_area = popup.calculate_area(size, Some(cursor_screen_pos));
                popup.render(frame, popup_area, &theme_clone);
            }
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
        // Only notify for insert and delete events
        match event {
            Event::Insert { .. } | Event::Delete { .. } => {
                tracing::debug!("notify_lsp_change: processing event {:?}", event);
            }
            _ => return, // Ignore cursor movements and other events
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
        let uri = match &metadata.file_uri {
            Some(u) => u.clone(),
            None => {
                tracing::debug!(
                    "notify_lsp_change: no URI for buffer (not a file or URI creation failed)"
                );
                return;
            }
        };

        // Get the file path for language detection
        let path = match &metadata.file_path {
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

        // Get the full text before borrowing lsp mutably
        let full_text = self.active_state().buffer.to_string();
        tracing::debug!(
            "notify_lsp_change: sending didChange to {} (text length: {} bytes)",
            uri.as_str(),
            full_text.len()
        );

        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(&language) {
                // Use full document sync (send entire text after change)
                // This is simpler than incremental sync and works well for small files
                let change = TextDocumentContentChangeEvent {
                    range: None, // Full document sync
                    range_length: None,
                    text: full_text,
                };

                if let Err(e) = client.did_change(uri, vec![change]) {
                    tracing::warn!("Failed to send didChange to LSP: {}", e);
                } else {
                    tracing::info!("Successfully sent didChange to LSP");
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
        let uri = match &metadata.file_uri {
            Some(u) => u.clone(),
            None => {
                tracing::debug!("notify_lsp_save: no URI for buffer");
                return;
            }
        };

        // Get the file path for language detection
        let path = match &metadata.file_path {
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
    pub fn action_to_events(&self, action: Action) -> Option<Vec<Event>> {
        convert_action_to_events(
            self.active_state(),
            action,
            self.config.editor.tab_size,
            self.config.editor.auto_indent,
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
            let mut line_iter = state.buffer.line_iterator(top_byte);
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
        let visible_text = state.buffer.slice(visible_start..visible_end);

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

        // Find all matches (case-insensitive for now)
        let query_lower = query.to_lowercase();
        let mut matches = Vec::new();

        let buffer_lower = buffer_content.to_lowercase();

        // Determine search boundaries
        let (search_start, search_end) = if let Some(ref range) = search_range {
            (range.start, range.end)
        } else {
            (0, buffer_content.len())
        };

        // Find all matches within the search range
        let mut start = search_start;
        while start < search_end {
            if let Some(pos) = buffer_lower[start..search_end].find(&query_lower) {
                let absolute_pos = start + pos;
                matches.push(absolute_pos);
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
            let deleted_text = self.active_state().buffer.slice(range.clone());

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
                        let deleted_text = self.active_state().buffer.slice(range.clone());

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
        let deleted_text = self.active_state().buffer.slice(range.clone());

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
        let editor = Editor::new(config, 80, 24).unwrap();

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
        let editor = Editor::new(config, 80, 24).unwrap();

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
        let editor = Editor::new(config, 80, 24).unwrap();

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
        let editor = Editor::new(config, 80, 24).unwrap();

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
        let editor = Editor::new(config, 80, 24).unwrap();

        // None action should return None
        let events = editor.action_to_events(Action::None);
        assert!(events.is_none());
    }
}
