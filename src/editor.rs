use crate::actions::action_to_events as convert_action_to_events;
use crate::async_bridge::{AsyncBridge, AsyncMessage};
use crate::commands::{filter_commands, get_all_commands, Suggestion};
use crate::config::Config;
use crate::event::{Event, EventLog};
use crate::file_tree::{FileTree, FileTreeView};
use crate::fs::{FsManager, LocalFsBackend};
use crate::keybindings::{Action, KeybindingResolver, KeyContext};
use crate::lsp_diagnostics;
use crate::lsp_manager::{detect_language, LspManager};
use crate::multi_cursor::{add_cursor_above, add_cursor_at_next_match, add_cursor_below, AddCursorResult};
use crate::position_history::PositionHistory;
use crate::prompt::{Prompt, PromptType};
use crate::split::SplitManager;
use crate::state::EditorState;
use crate::ui::{FileExplorerRenderer, HelpRenderer, SplitRenderer, StatusBarRenderer, SuggestionsRenderer, TabsRenderer};
use lsp_types::{TextDocumentContentChangeEvent, Url};
use ratatui::{
    layout::{Constraint, Direction, Layout},
    Frame,
};
use std::collections::HashMap;
use std::io;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

// Re-export BufferId from event module for backward compatibility
pub use crate::event::BufferId;

/// Metadata associated with a buffer
#[derive(Debug, Clone)]
pub struct BufferMetadata {
    /// File path (if the buffer is associated with a file)
    pub file_path: Option<PathBuf>,

    /// File URI for LSP (computed once from absolute path)
    pub file_uri: Option<lsp_types::Url>,

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
            lsp_enabled: true,
            lsp_disabled_reason: None,
        }
    }

    /// Create metadata for a file-backed buffer
    pub fn with_file(path: PathBuf) -> Self {
        // Convert to absolute path and compute URI once
        let absolute_path = if path.is_absolute() {
            path.clone()
        } else {
            std::env::current_dir()
                .ok()
                .and_then(|cwd| cwd.join(&path).canonicalize().ok())
                .unwrap_or_else(|| path.clone())
        };

        let file_uri = lsp_types::Url::from_file_path(&absolute_path).ok();

        Self {
            file_path: Some(path),
            file_uri,
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

    /// LSP status indicator for status bar
    lsp_status: String,
}

impl Editor {
    /// Create a new editor with the given configuration and terminal dimensions
    /// If working_dir is None, uses the current working directory
    pub fn new(config: Config, width: u16, height: u16) -> io::Result<Self> {
        Self::with_working_dir(config, width, height, None)
    }

    /// Create a new editor with an explicit working directory
    /// This is useful for testing with isolated temporary directories
    pub fn with_working_dir(config: Config, width: u16, height: u16, working_dir: Option<PathBuf>) -> io::Result<Self> {
        tracing::info!("Editor::new called with width={}, height={}", width, height);

        // Use provided working_dir or capture from environment
        let working_dir = working_dir.unwrap_or_else(|| {
            std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
        });

        // Load theme from config
        let theme = crate::theme::Theme::from_name(&config.theme);

        let keybindings = KeybindingResolver::new(&config);

        // Create an empty initial buffer
        let mut buffers = HashMap::new();
        let mut event_logs = HashMap::new();

        let buffer_id = BufferId(0);
        let state = EditorState::new(width, height);
        tracing::info!(
            "EditorState created with viewport height: {}",
            state.viewport.height
        );
        buffers.insert(buffer_id, state);
        event_logs.insert(buffer_id, EventLog::new());

        // Initialize LSP manager with current working directory as root
        let root_uri = Url::from_file_path(&working_dir).ok();

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
        let fs_backend = Arc::new(LocalFsBackend::new());
        let fs_manager = Arc::new(FsManager::new(fs_backend));

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
            buffer_metadata: HashMap::new(),
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
            lsp_status: String::new(),
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
        // Check if file is already open
        let already_open = self.buffers.iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            // Commit pending movement before switching to existing buffer
            if id != self.active_buffer {
                self.position_history.commit_pending_movement();
                self.active_buffer = id;
                // Update the split manager to show this buffer
                self.split_manager.set_active_buffer_id(id);
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

        let state = EditorState::from_file(path, self.terminal_width, self.terminal_height)?;
        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Create metadata for this buffer
        let mut metadata = BufferMetadata::with_file(path.to_path_buf());

        // Schedule LSP notification asynchronously to avoid blocking
        // This is especially important for large files
        if let Some(lsp) = &mut self.lsp {
            tracing::debug!("LSP manager available for file: {}", path.display());
            if let Some(language) = detect_language(path) {
                tracing::debug!("Detected language: {} for file: {}", language, path.display());

                // Use the URI from metadata (already computed in with_file)
                if let Some(uri) = &metadata.file_uri {
                    tracing::debug!("Using URI from metadata: {}", uri);
                    // Get file size to decide whether to send full content
                    let file_size = std::fs::metadata(path).ok().map(|m| m.len()).unwrap_or(0);
                    const MAX_LSP_FILE_SIZE: u64 = 1024 * 1024; // 1MB limit

                    if file_size > MAX_LSP_FILE_SIZE {
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
                        tracing::debug!("Attempting to get or spawn LSP client for language: {}", language);
                        if let Some(client) = lsp.get_or_spawn(&language) {
                            tracing::info!("Sending didOpen to LSP for: {}", uri);
                            if let Err(e) = client.did_open(uri.clone(), text, language) {
                                tracing::warn!("Failed to send didOpen to LSP: {}", e);
                            } else {
                                tracing::info!("Successfully sent didOpen to LSP");
                            }
                        } else {
                            tracing::warn!("Failed to get or spawn LSP client for language: {}", language);
                        }
                    }
                } else {
                    tracing::warn!("No URI in metadata for file: {} (failed to compute absolute path)", path.display());
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
            self.position_history.record_movement(self.active_buffer, position, anchor);
            self.position_history.commit_pending_movement();
        }

        self.active_buffer = buffer_id;
        // Update the split manager to show the new buffer
        self.split_manager.set_active_buffer_id(buffer_id);
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
        self.position_history.record_movement(self.active_buffer, position, anchor);
        self.position_history.commit_pending_movement();

        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        self.buffers.insert(
            buffer_id,
            EditorState::new(self.terminal_width, self.terminal_height),
        );
        self.event_logs.insert(buffer_id, EventLog::new());

        self.active_buffer = buffer_id;

        // Update the active split to display the new buffer
        self.split_manager.set_active_buffer_id(buffer_id);

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
            self.active_buffer = *self.buffers.keys().next().unwrap();
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
            self.position_history.record_movement(self.active_buffer, position, anchor);
            self.position_history.commit_pending_movement();

            self.active_buffer = id;
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
                self.position_history.record_movement(self.active_buffer, position, anchor);
                self.position_history.commit_pending_movement();

                self.active_buffer = ids[next_idx];
                // Update the split manager to show the new buffer
                self.split_manager.set_active_buffer_id(ids[next_idx]);
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
                self.position_history.record_movement(self.active_buffer, position, anchor);
                self.position_history.commit_pending_movement();

                self.active_buffer = ids[prev_idx];
                // Update the split manager to show the new buffer
                self.split_manager.set_active_buffer_id(ids[prev_idx]);
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
            self.position_history.record_movement(self.active_buffer, position, anchor);
            self.position_history.commit_pending_movement();
        }

        // Navigate to the previous position
        if let Some(entry) = self.position_history.back() {
            let target_buffer = entry.buffer_id;
            let target_position = entry.position;
            let target_anchor = entry.anchor;

            // Switch to the target buffer
            if self.buffers.contains_key(&target_buffer) {
                self.active_buffer = target_buffer;
                // Update the split manager to show the new buffer
                self.split_manager.set_active_buffer_id(target_buffer);

                // Move cursor to the saved position
                let state = self.active_state_mut();
                let cursor_id = state.cursors.primary_id();
                let event = Event::MoveCursor {
                    cursor_id,
                    position: target_position,
                    anchor: target_anchor,
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
                self.active_buffer = target_buffer;
                // Update the split manager to show the new buffer
                self.split_manager.set_active_buffer_id(target_buffer);

                // Move cursor to the saved position
                let state = self.active_state_mut();
                let cursor_id = state.cursors.primary_id();
                let event = Event::MoveCursor {
                    cursor_id,
                    position: target_position,
                    anchor: target_anchor,
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
        } else {
            // Return focus to editor when closing
            self.key_context = KeyContext::Normal;
            self.set_status_message("File explorer closed".to_string());
        }
    }

    /// Focus the file explorer
    pub fn focus_file_explorer(&mut self) {
        if self.file_explorer_visible {
            // File explorer is already visible, just switch focus
            self.key_context = KeyContext::FileExplorer;
            self.set_status_message("File explorer focused".to_string());
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
                    Ok(tree) => {
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
        }
    }

    /// Handle file explorer navigation down
    pub fn file_explorer_navigate_down(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_next();
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
            let final_name = explorer.tree().get_node(selected_id)
                .map(|n| n.entry.name.clone());
            let final_expanded = explorer.tree().get_node(selected_id)
                .map(|n| n.is_expanded())
                .unwrap_or(false);

            match result {
                Ok(()) => {
                    // If directory was just expanded, load its .gitignore
                    if final_expanded {
                        // Extract the path before calling mutable method
                        let dir_path = explorer.tree().get_node(selected_id)
                            .map(|n| n.entry.path.clone());

                        if let Some(dir_path) = dir_path {
                            // Load .gitignore for this directory
                            if let Err(e) = explorer.load_gitignore_for_dir(&dir_path) {
                                tracing::warn!("Failed to load .gitignore from {:?}: {}", dir_path, e);
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

    /// Handle file explorer open file
    pub fn file_explorer_open_file(&mut self) -> io::Result<()> {
        // Clone the path and name before calling open_file to avoid borrow checker issues
        let file_info = self.file_explorer
            .as_ref()
            .and_then(|explorer| explorer.get_selected_entry())
            .filter(|entry| entry.is_file())
            .map(|entry| (entry.path.clone(), entry.name.clone()));

        if let Some((path, name)) = file_info {
            self.open_file(&path)?;
            self.set_status_message(format!("Opened: {}", name));
        }
        Ok(())
    }

    /// Handle file explorer refresh
    pub fn file_explorer_refresh(&mut self) {
        // Extract needed data first
        let (selected_id, node_name) = if let Some(explorer) = &self.file_explorer {
            if let Some(selected_id) = explorer.get_selected() {
                let node_name = explorer.tree().get_node(selected_id)
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
                        node.entry.path.parent().map(|p| p.to_path_buf())
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
                        let result = runtime.block_on(async {
                            tokio::fs::File::create(&path_clone).await
                        });

                        match result {
                            Ok(_) => {
                                // Refresh the parent directory to show the new file
                                let parent_id = if node.is_dir() { selected_id } else {
                                    explorer.tree().get_node(selected_id)
                                        .and_then(|n| n.parent)
                                        .unwrap_or(selected_id)
                                };

                                let _ = runtime.block_on(explorer.tree_mut().refresh_node(parent_id));

                                // Try to open the new file
                                if let Ok(_) = self.open_file(&file_path) {
                                    self.set_status_message(format!("Created and opened: {}", filename));
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
                        node.entry.path.parent().map(|p| p.to_path_buf())
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
                        let result = runtime.block_on(async {
                            tokio::fs::create_dir(&path_clone).await
                        });

                        match result {
                            Ok(_) => {
                                // Refresh the parent directory to show the new folder
                                let parent_id = if node.is_dir() { selected_id } else {
                                    explorer.tree().get_node(selected_id)
                                        .and_then(|n| n.parent)
                                        .unwrap_or(selected_id)
                                };

                                let _ = runtime.block_on(explorer.tree_mut().refresh_node(parent_id));
                                self.set_status_message(format!("Created directory: {}", dirname));
                            }
                            Err(e) => {
                                self.set_status_message(format!("Failed to create directory: {}", e));
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
                        self.set_status_message(format!("Refusing to delete: {} (safety check)", name));
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
                                    let _ = runtime.block_on(explorer.tree_mut().refresh_node(parent_id));
                                }
                                self.set_status_message(format!("Deleted: {}", name));
                            }
                            Err(e) => {
                                self.set_status_message(format!("Failed to delete {}: {}", name, e));
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
                    self.set_status_message(format!("Rename '{}': Input dialog not yet implemented", node_name));
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

    /// Apply an event to the active buffer
    pub fn apply_event_to_active_buffer(&mut self, event: Event) {
        self.active_state_mut().apply(&event);
    }

    /// Get the event log for the active buffer
    pub fn active_event_log(&self) -> &EventLog {
        self.event_logs.get(&self.active_buffer).unwrap()
    }

    /// Get the event log for the active buffer (mutable)
    pub fn active_event_log_mut(&mut self) -> &mut EventLog {
        self.event_logs.get_mut(&self.active_buffer).unwrap()
    }

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
            self.active_state_mut().apply(&event);
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
        self.active_state_mut().apply(&event);

        self.status_message = Some("Pasted".to_string());
    }

    /// Add a cursor at the next occurrence of the selected text
    /// If no selection, does nothing
    pub fn add_cursor_at_next_match(&mut self) {
        let state = self.active_state();
        match add_cursor_at_next_match(state) {
            AddCursorResult::Success { cursor, total_cursors } => {
                let state_mut = self.active_state_mut();
                state_mut.cursors.add(cursor);
                state_mut.cursors.normalize();
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
            AddCursorResult::Success { cursor, total_cursors } => {
                let state_mut = self.active_state_mut();
                state_mut.cursors.add(cursor);
                state_mut.cursors.normalize();
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
            AddCursorResult::Success { cursor, total_cursors } => {
                let state_mut = self.active_state_mut();
                state_mut.cursors.add(cursor);
                state_mut.cursors.normalize();
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
        self.prompt = Some(Prompt::with_suggestions(message, prompt_type, suggestions));
    }

    /// Cancel the current prompt and return to normal mode
    pub fn cancel_prompt(&mut self) {
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
                    prompt.suggestions = filter_commands(&input, self.key_context);
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
                    if let Ok(diagnostic_url) = Url::parse(&uri) {
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
                AsyncMessage::LspGotoDefinition { request_id, locations } => {
                    if let Err(e) = self.handle_goto_definition_response(request_id, locations) {
                        tracing::error!("Error handling goto definition response: {}", e);
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
                        if matches!(prompt.prompt_type, PromptType::GitGrep) && prompt.input == query {
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
                        if matches!(prompt.prompt_type, PromptType::GitFindFile) && prompt.input == query {
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
                    let root_path = view.tree().get_node(root_id)
                        .map(|n| n.entry.path.clone());

                    if let Some(root_path) = root_path {
                        if let Err(e) = view.load_gitignore_for_dir(&root_path) {
                            tracing::warn!("Failed to load root .gitignore from {:?}: {}", root_path, e);
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
            }
        }
    }

    /// Handle LSP completion response
    fn handle_completion_response(&mut self, request_id: u64, items: Vec<lsp_types::CompletionItem>) -> io::Result<()> {
        // Check if this is the pending completion request
        if self.pending_completion_request != Some(request_id) {
            tracing::debug!("Ignoring completion response for outdated request {}", request_id);
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
            state.buffer.slice(word_start..cursor_pos).to_string().to_lowercase()
        } else {
            String::new()
        };

        // Filter completions to match the typed prefix
        let filtered_items: Vec<&lsp_types::CompletionItem> = if prefix.is_empty() {
            // No prefix - show all completions
            items.iter().collect()
        } else {
            // Filter to items that start with the prefix (case-insensitive)
            items.iter().filter(|item| {
                item.label.to_lowercase().starts_with(&prefix) ||
                item.filter_text.as_ref().map(|ft| ft.to_lowercase().starts_with(&prefix)).unwrap_or(false)
            }).collect()
        };

        if filtered_items.is_empty() {
            tracing::debug!("No completion items match prefix '{}'", prefix);
            return Ok(());
        }

        // Convert CompletionItem to PopupListItem
        use crate::popup::{PopupListItem, PopupContent, Popup, PopupPosition};

        let popup_items: Vec<PopupListItem> = filtered_items.iter().map(|item| {
            let text = item.label.clone();
            let detail = item.detail.clone();
            let icon = match item.kind {
                Some(lsp_types::CompletionItemKind::FUNCTION) | Some(lsp_types::CompletionItemKind::METHOD) => Some("λ".to_string()),
                Some(lsp_types::CompletionItemKind::VARIABLE) => Some("v".to_string()),
                Some(lsp_types::CompletionItemKind::STRUCT) | Some(lsp_types::CompletionItemKind::CLASS) => Some("S".to_string()),
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
            let data = item.insert_text.clone().or_else(|| Some(item.label.clone()));
            if let Some(data) = data {
                list_item = list_item.with_data(data);
            }
            list_item
        }).collect();

        // Show the popup
        use crate::event::{PopupData, PopupContentData, PopupListItemData, PopupPositionData};
        let popup_data = PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: popup_items.into_iter().map(|item| PopupListItemData {
                    text: item.text,
                    detail: item.detail,
                    icon: item.icon,
                    data: item.data,
                }).collect(),
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        };

        self.active_state_mut().apply(&crate::event::Event::ShowPopup { popup: popup_data });

        tracing::info!("Showing completion popup with {} items", items.len());

        Ok(())
    }

    /// Handle LSP go-to-definition response
    fn handle_goto_definition_response(&mut self, request_id: u64, locations: Vec<lsp_types::Location>) -> io::Result<()> {
        // Check if this is the pending request
        if self.pending_goto_definition_request != Some(request_id) {
            tracing::debug!("Ignoring go-to-definition response for outdated request {}", request_id);
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
        if let Ok(path) = location.uri.to_file_path() {
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
                let event = crate::event::Event::MoveCursor {
                    cursor_id,
                    position,
                    anchor: None,
                };

                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.apply(&event);
                }
            }

            self.status_message = Some(format!("Jumped to definition at {}:{}", path.display(), line + 1));
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

        // Convert byte position to line/column
        let (line, character) = state.buffer.position_to_line_col(cursor_pos);

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

                        let _ = handle.completion(request_id, uri.clone(), line as u32, character as u32);
                        tracing::info!("Requested completion at {}:{}:{}", uri, line, character);
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

        // Convert byte position to line/column
        let (line, character) = state.buffer.position_to_line_col(cursor_pos);

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

                        let _ = handle.goto_definition(request_id, uri.clone(), line as u32, character as u32);
                        tracing::info!("Requested go-to-definition at {}:{}:{}", uri, line, character);
                    }
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

    /// Determine the current keybinding context based on UI state
    fn get_key_context(&self) -> crate::keybindings::KeyContext {
        use crate::keybindings::KeyContext;

        // Priority order: Help > Prompt > Popup > Current context (FileExplorer or Normal)
        if self.help_renderer.is_visible() {
            KeyContext::Help
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
        use crossterm::event::{KeyCode, KeyModifiers};
        use std::path::Path;

        tracing::debug!("Editor.handle_key: code={:?}, modifiers={:?}", code, modifiers);

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
                            self.set_status_message(format!("Save-as not yet implemented: {input}"));
                        }
                        PromptType::Search => {
                            self.set_status_message(format!("Search not yet implemented: {input}"));
                        }
                        PromptType::Replace { search: _ } => {
                            self.set_status_message(format!("Replace not yet implemented: {input}"));
                        }
                        PromptType::Command => {
                            let commands = get_all_commands();

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
                                let _column = parts[2].parse::<usize>().unwrap_or(1);

                                // Open the file
                                let path = Path::new(file);
                                if let Err(e) = self.open_file(path) {
                                    self.set_status_message(format!("Error opening file: {e}"));
                                } else {
                                    // Jump to the line
                                    let state = self.active_state_mut();

                                    // Find the byte position for the target line
                                    // Git grep returns 1-indexed line numbers
                                    let target_line = line.saturating_sub(1); // Convert to 0-indexed
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

                                    state.cursors.primary_mut().position = target_byte;
                                    state.cursors.primary_mut().anchor = None;

                                    // Ensure the line is visible
                                    state.viewport.ensure_visible(
                                        &mut state.buffer,
                                        state.cursors.primary(),
                                    );

                                    self.set_status_message(format!("Jumped to {}:{}", file, line));
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
                    if prompt.cursor_pos > 0 {
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
            Action::PromptMoveLeft => {
                if let Some(prompt) = self.prompt_mut() {
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
                    if prompt.cursor_pos < prompt.input.len() {
                        let mut new_pos = prompt.cursor_pos + 1;
                        while new_pos < prompt.input.len() && !prompt.input.is_char_boundary(new_pos) {
                            new_pos += 1;
                        }
                        prompt.cursor_pos = new_pos;
                    }
                }
            }
            Action::PromptMoveStart => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.cursor_pos = 0;
                }
            }
            Action::PromptMoveEnd => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.cursor_pos = prompt.input.len();
                }
            }
            Action::PromptSelectPrev => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        if let Some(selected) = prompt.selected_suggestion {
                            prompt.selected_suggestion = if selected == 0 {
                                Some(prompt.suggestions.len() - 1)
                            } else {
                                Some(selected - 1)
                            };
                        }
                    }
                }
            }
            Action::PromptSelectNext => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        if let Some(selected) = prompt.selected_suggestion {
                            prompt.selected_suggestion = Some((selected + 1) % prompt.suggestions.len());
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
                        self.active_state_mut().apply(&delete_event);
                        self.notify_lsp_change(&delete_event);

                        // After deletion, ensure insert position is valid
                        let buffer_len = self.active_state().buffer.len();
                        let insert_pos = word_start.min(buffer_len);

                        let insert_event = crate::event::Event::Insert {
                            position: insert_pos,
                            text,
                            cursor_id,
                        };

                        self.active_event_log_mut().append(insert_event.clone());
                        self.active_state_mut().apply(&insert_event);
                        self.notify_lsp_change(&insert_event);
                    } else {
                        // No partial word to delete, just insert
                        let insert_event = crate::event::Event::Insert {
                            position: cursor_pos,
                            text,
                            cursor_id,
                        };

                        self.active_event_log_mut().append(insert_event.clone());
                        self.active_state_mut().apply(&insert_event);
                        self.notify_lsp_change(&insert_event);
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
            Action::New => { self.new_buffer(); },
            Action::Copy => self.copy_selection(),
            Action::Cut => self.cut_selection(),
            Action::Paste => self.paste(),
            Action::Undo => {
                if let Some(event) = self.active_event_log_mut().undo() {
                    if let Some(inverse) = event.inverse() {
                        self.active_state_mut().apply(&inverse);
                    }
                }
            }
            Action::Redo => {
                let event_opt = self.active_event_log_mut().redo().cloned();
                if let Some(event) = event_opt {
                    self.active_state_mut().apply(&event);
                }
            }
            Action::ShowHelp => self.help_renderer.toggle(),
            Action::CommandPalette => {
                // Use the current context for filtering commands
                let suggestions = filter_commands("", self.key_context);
                self.start_prompt_with_suggestions(
                    "Command: ".to_string(),
                    PromptType::Command,
                    suggestions,
                );
            }
            Action::LspCompletion => {
                self.request_completion()?;
            }
            Action::LspGotoDefinition => {
                self.request_goto_definition()?;
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
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::RemoveSecondaryCursors => self.active_state_mut().cursors.remove_secondary(),
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
            Action::None => {}
            Action::InsertChar(c) => {
                // Handle character insertion in prompt mode
                if self.is_prompting() {
                    if let Some(prompt) = self.prompt_mut() {
                        prompt.input.insert(prompt.cursor_pos, c);
                        prompt.cursor_pos += c.len_utf8();
                    }
                    self.update_prompt_suggestions();
                } else {
                    // Normal mode character insertion
                    if let Some(events) = self.action_to_events(Action::InsertChar(c)) {
                        for event in events {
                            self.active_event_log_mut().append(event.clone());
                            self.active_state_mut().apply(&event);
                            self.notify_lsp_change(&event);
                        }
                    }
                }
            }
            _ => {
                // Convert action to events and apply them
                if let Some(events) = self.action_to_events(action) {
                    for event in events {
                        self.active_event_log_mut().append(event.clone());
                        self.active_state_mut().apply(&event);
                        self.notify_lsp_change(&event);

                        // Track cursor movements in position history (but not during navigation)
                        if !self.in_navigation {
                            if let Event::MoveCursor { position, anchor, .. } = event {
                                self.position_history.record_movement(
                                    self.active_buffer,
                                    position,
                                    anchor,
                                );
                            }
                        }
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
            self.help_renderer.render(frame, size, &self.keybindings, &self.theme);
            return;
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

        // Split into tabs, content, suggestions (if any), and status bar
        let mut constraints = vec![
            Constraint::Length(1), // Tabs
            Constraint::Min(0),    // Content
        ];

        if suggestion_lines > 0 {
            // Add 2 lines for the border (top and bottom)
            constraints.push(Constraint::Length(suggestion_lines as u16 + 2)); // Suggestions popup with border
        }

        constraints.push(Constraint::Length(1)); // Status bar

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints)
            .split(size);

        // Render tabs
        TabsRenderer::render(frame, chunks[0], &self.buffers, self.active_buffer, &self.theme);

        // Render content (with file explorer if visible)
        let lsp_waiting = self.pending_completion_request.is_some() || self.pending_goto_definition_request.is_some();
        let content_area = chunks[1];
        if self.file_explorer_visible && self.file_explorer.is_some() {
            // Split content area horizontally: file explorer (30%) | content (70%)
            let horizontal_chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Percentage(30),  // File explorer
                    Constraint::Percentage(70),  // Content
                ])
                .split(content_area);

            // Render file explorer on the left
            if let Some(ref explorer) = self.file_explorer {
                let is_focused = self.key_context == KeyContext::FileExplorer;
                FileExplorerRenderer::render(explorer, frame, horizontal_chunks[0], is_focused);
            }

            // Render content on the right
            SplitRenderer::render_content(
                frame,
                horizontal_chunks[1],
                &self.split_manager,
                &mut self.buffers,
                &mut self.event_logs,
                &self.theme,
                lsp_waiting,
            );
        } else {
            // No file explorer, render content normally
            SplitRenderer::render_content(
                frame,
                chunks[1],
                &self.split_manager,
                &mut self.buffers,
                &mut self.event_logs,
                &self.theme,
                lsp_waiting,
            );
        }

        // Render suggestions popup if present
        if suggestion_lines > 0 {
            if let Some(prompt) = &self.prompt {
                SuggestionsRenderer::render(frame, chunks[2], prompt, &self.theme);
            }
            // Status bar is in chunks[3]
            StatusBarRenderer::render(
                frame,
                chunks[3],
                self.active_state(),
                &self.status_message,
                &self.prompt,
                &self.lsp_status,
                &self.theme,
            );
        } else {
            // Status bar is in chunks[2]
            StatusBarRenderer::render(
                frame,
                chunks[2],
                self.active_state(),
                &self.status_message,
                &self.prompt,
                &self.lsp_status,
                &self.theme,
            );
        }

        // Render popups from the active buffer state
        // Clone theme to avoid borrow checker issues with active_state_mut()
        let theme_clone = self.theme.clone();
        let state = self.active_state_mut();
        if state.popups.is_visible() {
            // Get the primary cursor position for popup positioning
            let primary_cursor = state.cursors.primary();
            let cursor_screen_pos = state.viewport.cursor_screen_position(&mut state.buffer, primary_cursor);

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
        self.active_state_mut().apply(&event);
    }

    /// Remove an overlay by ID
    pub fn remove_overlay(&mut self, overlay_id: String) {
        let event = Event::RemoveOverlay { overlay_id };
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
    }

    /// Remove all overlays in a range
    pub fn remove_overlays_in_range(&mut self, range: Range<usize>) {
        let event = Event::RemoveOverlaysInRange { range };
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
    }

    /// Clear all overlays
    pub fn clear_overlays(&mut self) {
        let event = Event::ClearOverlays;
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
    }

    // === Popup Management (Event-Driven) ===

    /// Show a popup window
    pub fn show_popup(&mut self, popup: crate::event::PopupData) {
        let event = Event::ShowPopup { popup };
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
    }

    /// Hide the topmost popup
    pub fn hide_popup(&mut self) {
        let event = Event::HidePopup;
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
    }

    /// Clear all popups
    pub fn clear_popups(&mut self) {
        let event = Event::ClearPopups;
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
    }

    /// Navigate popup selection (next item)
    pub fn popup_select_next(&mut self) {
        let event = Event::PopupSelectNext;
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
    }

    /// Navigate popup selection (previous item)
    pub fn popup_select_prev(&mut self) {
        let event = Event::PopupSelectPrev;
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
    }

    /// Navigate popup (page down)
    pub fn popup_page_down(&mut self) {
        let event = Event::PopupPageDown;
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
    }

    /// Navigate popup (page up)
    pub fn popup_page_up(&mut self) {
        let event = Event::PopupPageUp;
        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);
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
                tracing::debug!("notify_lsp_change: no metadata for buffer {:?}", self.active_buffer);
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
                tracing::debug!("notify_lsp_change: no URI for buffer (not a file or URI creation failed)");
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
        tracing::debug!("notify_lsp_change: sending didChange to {} (text length: {} bytes)", uri, full_text.len());

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
                tracing::warn!("notify_lsp_change: failed to get or spawn LSP client for {}", language);
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
                tracing::debug!("notify_lsp_save: no metadata for buffer {:?}", self.active_buffer);
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
        tracing::debug!("notify_lsp_save: sending didSave to {} (text length: {} bytes)", uri, full_text.len());

        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(&language) {
                // Send didSave with the full text content
                if let Err(e) = client.did_save(uri, Some(full_text)) {
                    tracing::warn!("Failed to send didSave to LSP: {}", e);
                } else {
                    tracing::info!("Successfully sent didSave to LSP");
                }
            } else {
                tracing::warn!("notify_lsp_save: failed to get or spawn LSP client for {}", language);
            }
        } else {
            tracing::debug!("notify_lsp_save: no LSP manager available");
        }
    }

    /// Convert an action into a list of events to apply to the active buffer
    /// Returns None for actions that don't generate events (like Quit)
    pub fn action_to_events(&self, action: Action) -> Option<Vec<Event>> {
        convert_action_to_events(self.active_state(), action, self.config.editor.tab_size)
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
                position, anchor, ..
            } => {
                // Cursor was at 5 (end of "hello"), stays at 5 (can't move beyond end)
                assert_eq!(*position, 5);
                assert_eq!(*anchor, None); // No selection
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
            position: 6, // Start of "line2"
            anchor: None,
        });

        // Test move up
        let events = editor.action_to_events(Action::MoveUp);
        assert!(events.is_some());
        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor { position, .. } => {
                assert_eq!(*position, 0); // Should be at start of line 1
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
            position: 0,
            anchor: None,
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
            position: 0,
            anchor: None,
        });

        let events = editor.action_to_events(Action::SelectRight);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor {
                position, anchor, ..
            } => {
                assert_eq!(*position, 1); // Moved to position 1
                assert_eq!(*anchor, Some(0)); // Anchor at start
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
                position, anchor, ..
            } => {
                assert_eq!(*position, 11); // At end of buffer
                assert_eq!(*anchor, Some(0)); // Anchor at start
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
            Event::MoveCursor { position, .. } => {
                assert_eq!(*position, 0);
            }
            _ => panic!("Expected MoveCursor event"),
        }

        // Test MoveDocumentEnd
        let events = editor.action_to_events(Action::MoveDocumentEnd);
        assert!(events.is_some());
        let events = events.unwrap();
        match &events[0] {
            Event::MoveCursor { position, .. } => {
                assert_eq!(*position, 17); // End of buffer
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

        // Save primary ID before calling action_to_events
        let primary_id = editor.active_state().cursors.primary_id();

        // RemoveSecondaryCursors should generate RemoveCursor events
        let events = editor.action_to_events(Action::RemoveSecondaryCursors);
        assert!(events.is_some());

        let events = events.unwrap();
        // Should have events for the two secondary cursors
        assert_eq!(events.len(), 2);

        for event in &events {
            match event {
                Event::RemoveCursor { cursor_id } => {
                    // Should not be the primary cursor
                    assert_ne!(*cursor_id, primary_id);
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
