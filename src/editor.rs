use crate::actions::action_to_events as convert_action_to_events;
use crate::async_bridge::{AsyncBridge, AsyncMessage};
use crate::commands::{filter_commands, get_all_commands, Suggestion};
use crate::config::Config;
use crate::event::{Event, EventLog};
use crate::keybindings::{Action, KeybindingResolver};
use crate::lsp_diagnostics;
use crate::lsp_manager::{detect_language, LspManager};
use crate::multi_cursor::{add_cursor_above, add_cursor_at_next_match, add_cursor_below, AddCursorResult};
use crate::prompt::{Prompt, PromptType};
use crate::split::SplitManager;
use crate::state::EditorState;
use crate::ui::{HelpRenderer, SplitRenderer, StatusBarRenderer, SuggestionsRenderer, TabsRenderer};
use lsp_types::{TextDocumentContentChangeEvent, Url};
use ratatui::{
    layout::{Constraint, Direction, Layout},
    Frame,
};
use std::collections::HashMap;
use std::io;
use std::ops::Range;
use std::path::{Path, PathBuf};

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
}

impl Editor {
    /// Create a new editor with the given configuration and terminal dimensions
    pub fn new(config: Config, width: u16, height: u16) -> io::Result<Self> {
        tracing::info!("Editor::new called with width={}, height={}", width, height);

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
        let root_uri = std::env::current_dir()
            .ok()
            .and_then(|path| Url::from_file_path(path).ok());

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
        for (id, state) in &self.buffers {
            if state.buffer.file_path() == Some(path) {
                self.active_buffer = *id;
                return Ok(*id);
            }
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

        self.active_buffer = buffer_id;
        self.status_message = Some(format!("Opened {}", path.display()));

        Ok(buffer_id)
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> BufferId {
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        self.buffers.insert(
            buffer_id,
            EditorState::new(self.terminal_width, self.terminal_height),
        );
        self.event_logs.insert(buffer_id, EventLog::new());

        self.active_buffer = buffer_id;
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
        if self.buffers.contains_key(&id) {
            self.active_buffer = id;
        }
    }

    /// Switch to next buffer
    pub fn next_buffer(&mut self) {
        let mut ids: Vec<_> = self.buffers.keys().copied().collect();
        ids.sort_by_key(|id| id.0); // Sort by buffer ID to ensure consistent order
        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer) {
            let next_idx = (idx + 1) % ids.len();
            self.active_buffer = ids[next_idx];
        }
    }

    /// Switch to previous buffer
    pub fn prev_buffer(&mut self) {
        let mut ids: Vec<_> = self.buffers.keys().copied().collect();
        ids.sort_by_key(|id| id.0); // Sort by buffer ID to ensure consistent order
        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer) {
            let prev_idx = if idx == 0 { ids.len() - 1 } else { idx - 1 };
            self.active_buffer = ids[prev_idx];
        }
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

    /// Get the currently active buffer state
    pub fn active_state(&self) -> &EditorState {
        self.buffers.get(&self.active_buffer).unwrap()
    }

    /// Get the currently active buffer state (mutable)
    pub fn active_state_mut(&mut self) -> &mut EditorState {
        self.buffers.get_mut(&self.active_buffer).unwrap()
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
    pub fn confirm_prompt(&mut self) -> Option<(String, PromptType)> {
        if let Some(prompt) = self.prompt.take() {
            Some((prompt.input, prompt.prompt_type))
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
        if let Some(prompt) = &mut self.prompt {
            if matches!(prompt.prompt_type, PromptType::Command) {
                prompt.suggestions = filter_commands(&prompt.input);
                prompt.selected_suggestion = if prompt.suggestions.is_empty() {
                    None
                } else {
                    Some(0)
                };
            }
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
                AsyncMessage::FileChanged { path } => {
                    tracing::info!("File changed externally: {}", path);
                    // TODO: Handle external file changes
                }
                AsyncMessage::GitStatusChanged { status } => {
                    tracing::info!("Git status changed: {}", status);
                    // TODO: Handle git status changes
                }
            }
        }
    }

    /// Determine the current keybinding context based on UI state
    fn get_key_context(&self) -> crate::keybindings::KeyContext {
        use crate::keybindings::KeyContext;

        if self.help_renderer.is_visible() {
            KeyContext::Help
        } else if self.is_prompting() {
            KeyContext::Prompt
        } else if self.active_state().popups.is_visible() {
            KeyContext::Popup
        } else {
            KeyContext::Normal
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
                            if let Some(cmd) = commands.iter().find(|c| c.name == input) {
                                let action = cmd.action.clone();
                                self.set_status_message(format!("Executing: {}", cmd.name));
                                // Recursively handle the command action
                                return self.handle_action(action);
                            } else {
                                self.set_status_message(format!("Unknown command: {input}"));
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
                            prompt.input = suggestion.get_value().to_string();
                            prompt.cursor_pos = prompt.input.len();
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
                let suggestions = filter_commands("");
                self.start_prompt_with_suggestions(
                    "Command: ".to_string(),
                    PromptType::Command,
                    suggestions,
                );
            }
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::RemoveSecondaryCursors => self.active_state_mut().cursors.remove_secondary(),
            Action::NextBuffer => self.next_buffer(),
            Action::PrevBuffer => self.prev_buffer(),
            Action::SplitHorizontal => self.split_pane_horizontal(),
            Action::SplitVertical => self.split_pane_vertical(),
            Action::CloseSplit => self.close_active_split(),
            Action::NextSplit => self.next_split(),
            Action::PrevSplit => self.prev_split(),
            Action::IncreaseSplitSize => self.adjust_split_size(0.05),
            Action::DecreaseSplitSize => self.adjust_split_size(-0.05),
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
            constraints.push(Constraint::Length(suggestion_lines as u16)); // Suggestions popup
        }

        constraints.push(Constraint::Length(1)); // Status bar

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints)
            .split(size);

        // Render tabs
        TabsRenderer::render(frame, chunks[0], &self.buffers, self.active_buffer, &self.theme);

        // Render content
        SplitRenderer::render_content(
            frame,
            chunks[1],
            &self.split_manager,
            &mut self.buffers,
            &mut self.event_logs,
            &self.theme,
        );

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
