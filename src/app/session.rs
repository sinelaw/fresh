//! Session persistence integration for the Editor
//!
//! This module provides conversion between live Editor state and serialized Session data.

use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::Instant;

use crate::model::event::{BufferId, SplitDirection, SplitId};
use crate::services::terminal::TerminalId;
use crate::session::{
    FileExplorerState, SearchOptions, SerializedBookmark, SerializedCursor, SerializedFileState,
    SerializedScroll, SerializedSplitDirection, SerializedSplitNode, SerializedSplitViewState,
    SerializedTabRef, SerializedTerminalSession, SerializedViewMode, Session,
    SessionConfigOverrides, SessionError, SessionHistories, SESSION_VERSION,
};
use crate::state::ViewMode;
use crate::view::split::{SplitNode, SplitViewState};

use super::types::Bookmark;
use super::Editor;

/// Session persistence state tracker
///
/// Tracks dirty state and handles debounced saving for crash resistance.
pub struct SessionTracker {
    /// Whether session has unsaved changes
    dirty: bool,
    /// Last save time
    last_save: Instant,
    /// Minimum interval between saves (debounce)
    save_interval: std::time::Duration,
    /// Whether session persistence is enabled
    enabled: bool,
}

impl SessionTracker {
    /// Create a new session tracker
    pub fn new(enabled: bool) -> Self {
        Self {
            dirty: false,
            last_save: Instant::now(),
            save_interval: std::time::Duration::from_secs(5),
            enabled,
        }
    }

    /// Check if session tracking is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Mark session as needing save
    pub fn mark_dirty(&mut self) {
        if self.enabled {
            self.dirty = true;
        }
    }

    /// Check if a save is needed and enough time has passed
    pub fn should_save(&self) -> bool {
        self.enabled && self.dirty && self.last_save.elapsed() >= self.save_interval
    }

    /// Record that a save was performed
    pub fn record_save(&mut self) {
        self.dirty = false;
        self.last_save = Instant::now();
    }

    /// Check if there are unsaved changes (for shutdown)
    pub fn is_dirty(&self) -> bool {
        self.dirty
    }
}

impl Editor {
    /// Capture current editor state into a Session
    pub fn capture_session(&self) -> Session {
        tracing::debug!("Capturing session for {:?}", self.working_dir);

        // Collect terminal metadata for session restore
        let mut terminals = Vec::new();
        let mut terminal_indices: HashMap<TerminalId, usize> = HashMap::new();
        let mut seen = HashSet::new();
        for terminal_id in self.terminal_buffers.values().copied() {
            if seen.insert(terminal_id) {
                let idx = terminals.len();
                terminal_indices.insert(terminal_id, idx);
                let handle = self.terminal_manager.get(terminal_id);
                let (cols, rows) = handle
                    .map(|h| h.size())
                    .unwrap_or((self.terminal_width, self.terminal_height));
                let cwd = handle.and_then(|h| h.cwd());
                let shell = handle
                    .map(|h| h.shell().to_string())
                    .unwrap_or_else(|| crate::services::terminal::detect_shell());
                let log_path = self
                    .terminal_log_files
                    .get(&terminal_id)
                    .cloned()
                    .unwrap_or_else(|| {
                        let root = self.dir_context.terminal_dir_for(&self.working_dir);
                        root.join(format!("fresh-terminal-{}.log", terminal_id.0))
                    });
                let backing_path = self
                    .terminal_backing_files
                    .get(&terminal_id)
                    .cloned()
                    .unwrap_or_else(|| {
                        let root = self.dir_context.terminal_dir_for(&self.working_dir);
                        root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
                    });

                terminals.push(SerializedTerminalSession {
                    terminal_index: idx,
                    cwd,
                    shell,
                    cols,
                    rows,
                    log_path,
                    backing_path,
                });
            }
        }

        let split_layout = serialize_split_node(
            self.split_manager.root(),
            &self.buffer_metadata,
            &self.working_dir,
            &self.terminal_buffers,
            &terminal_indices,
        );

        // Build a map of split_id -> active_buffer_id from the split tree
        // This tells us which buffer's cursor/scroll to save for each split
        let active_buffers: HashMap<SplitId, BufferId> = self
            .split_manager
            .root()
            .get_leaves_with_rects(ratatui::layout::Rect::default())
            .into_iter()
            .map(|(split_id, buffer_id, _)| (split_id, buffer_id))
            .collect();

        let mut split_states = HashMap::new();
        for (split_id, view_state) in &self.split_view_states {
            let active_buffer = active_buffers.get(split_id).copied();
            let serialized = serialize_split_view_state(
                view_state,
                &self.buffer_metadata,
                &self.working_dir,
                active_buffer,
                &self.terminal_buffers,
                &terminal_indices,
            );
            tracing::trace!(
                "Split {:?}: {} open tabs, active_buffer={:?}",
                split_id,
                serialized.open_tabs.len(),
                active_buffer
            );
            split_states.insert(split_id.0, serialized);
        }

        tracing::debug!(
            "Captured {} split states, active_split={}",
            split_states.len(),
            self.split_manager.active_split().0
        );

        // Capture file explorer state
        let file_explorer = if let Some(ref explorer) = self.file_explorer {
            // Get expanded directories from the tree
            let expanded_dirs = get_expanded_dirs(explorer, &self.working_dir);
            FileExplorerState {
                visible: self.file_explorer_visible,
                width_percent: self.file_explorer_width_percent,
                expanded_dirs,
                scroll_offset: explorer.get_scroll_offset(),
            }
        } else {
            FileExplorerState {
                visible: self.file_explorer_visible,
                width_percent: self.file_explorer_width_percent,
                expanded_dirs: Vec::new(),
                scroll_offset: 0,
            }
        };

        // Capture config overrides (only store deviations from defaults)
        let config_overrides = SessionConfigOverrides {
            line_numbers: Some(self.config.editor.line_numbers),
            relative_line_numbers: Some(self.config.editor.relative_line_numbers),
            line_wrap: Some(self.config.editor.line_wrap),
            syntax_highlighting: Some(self.config.editor.syntax_highlighting),
            enable_inlay_hints: Some(self.config.editor.enable_inlay_hints),
            mouse_enabled: Some(self.mouse_enabled),
        };

        // Capture histories using the items() accessor
        // Note: Only search and replace histories exist in Editor currently.
        // Other history fields are placeholders for future features.
        let histories = SessionHistories {
            search: self.search_history.items().to_vec(),
            replace: self.replace_history.items().to_vec(),
            command_palette: Vec::new(), // Future: when command palette has history
            goto_line: Vec::new(),       // Future: when goto line prompt has history
            open_file: Vec::new(),       // Future: when file open prompt has history
        };
        tracing::trace!(
            "Captured histories: {} search, {} replace",
            histories.search.len(),
            histories.replace.len()
        );

        // Capture search options
        let search_options = SearchOptions {
            case_sensitive: self.search_case_sensitive,
            whole_word: self.search_whole_word,
            use_regex: self.search_use_regex,
            confirm_each: self.search_confirm_each,
        };

        // Capture bookmarks
        let bookmarks =
            serialize_bookmarks(&self.bookmarks, &self.buffer_metadata, &self.working_dir);

        Session {
            version: SESSION_VERSION,
            working_dir: self.working_dir.clone(),
            split_layout,
            active_split_id: self.split_manager.active_split().0,
            split_states,
            config_overrides,
            file_explorer,
            histories,
            search_options,
            bookmarks,
            terminals,
            saved_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        }
    }

    /// Save the current session to disk
    pub fn save_session(&self) -> Result<(), SessionError> {
        let session = self.capture_session();
        session.save()
    }

    /// Try to load and apply a session for the current working directory
    ///
    /// Returns true if a session was successfully loaded and applied.
    pub fn try_restore_session(&mut self) -> Result<bool, SessionError> {
        tracing::debug!("Attempting to restore session for {:?}", self.working_dir);
        match Session::load(&self.working_dir)? {
            Some(session) => {
                tracing::info!("Found session, applying...");
                self.apply_session(&session)?;
                Ok(true)
            }
            None => {
                tracing::debug!("No session found for {:?}", self.working_dir);
                Ok(false)
            }
        }
    }

    /// Apply a loaded session to the editor
    pub fn apply_session(&mut self, session: &Session) -> Result<(), SessionError> {
        tracing::debug!(
            "Applying session with {} split states",
            session.split_states.len()
        );

        // 1. Apply config overrides
        if let Some(line_numbers) = session.config_overrides.line_numbers {
            self.config.editor.line_numbers = line_numbers;
        }
        if let Some(relative_line_numbers) = session.config_overrides.relative_line_numbers {
            self.config.editor.relative_line_numbers = relative_line_numbers;
        }
        if let Some(line_wrap) = session.config_overrides.line_wrap {
            self.config.editor.line_wrap = line_wrap;
        }
        if let Some(syntax_highlighting) = session.config_overrides.syntax_highlighting {
            self.config.editor.syntax_highlighting = syntax_highlighting;
        }
        if let Some(enable_inlay_hints) = session.config_overrides.enable_inlay_hints {
            self.config.editor.enable_inlay_hints = enable_inlay_hints;
        }
        if let Some(mouse_enabled) = session.config_overrides.mouse_enabled {
            self.mouse_enabled = mouse_enabled;
        }

        // 2. Restore search options
        self.search_case_sensitive = session.search_options.case_sensitive;
        self.search_whole_word = session.search_options.whole_word;
        self.search_use_regex = session.search_options.use_regex;
        self.search_confirm_each = session.search_options.confirm_each;

        // 3. Restore histories (merge with any existing)
        tracing::debug!(
            "Restoring histories: {} search, {} replace",
            session.histories.search.len(),
            session.histories.replace.len()
        );
        for item in &session.histories.search {
            self.search_history.push(item.clone());
        }
        for item in &session.histories.replace {
            self.replace_history.push(item.clone());
        }

        // 4. Restore file explorer state
        self.file_explorer_visible = session.file_explorer.visible;
        self.file_explorer_width_percent = session.file_explorer.width_percent;

        // Initialize file explorer if it was visible in the session
        // Note: We keep key_context as Normal so the editor has focus, not the explorer
        if self.file_explorer_visible && self.file_explorer.is_none() {
            self.init_file_explorer();
        }

        // 5. Open files from the session and build buffer mappings
        // Collect all unique file paths from split_states (which tracks all open files per split)
        let file_paths = collect_file_paths_from_states(&session.split_states);
        tracing::debug!(
            "Session has {} files to restore: {:?}",
            file_paths.len(),
            file_paths
        );
        let mut path_to_buffer: HashMap<PathBuf, BufferId> = HashMap::new();

        for rel_path in file_paths {
            let abs_path = self.working_dir.join(&rel_path);
            tracing::trace!(
                "Checking file: {:?} (exists: {})",
                abs_path,
                abs_path.exists()
            );
            if abs_path.exists() {
                // Open the file (this will reuse existing buffer if already open)
                match self.open_file_internal(&abs_path) {
                    Ok(buffer_id) => {
                        tracing::debug!("Opened file {:?} as buffer {:?}", rel_path, buffer_id);
                        path_to_buffer.insert(rel_path, buffer_id);
                    }
                    Err(e) => {
                        tracing::warn!("Failed to open file {:?}: {}", abs_path, e);
                    }
                }
            } else {
                tracing::debug!("Skipping non-existent file: {:?}", abs_path);
            }
        }

        tracing::debug!("Opened {} files from session", path_to_buffer.len());

        // Restore terminals and build index -> buffer map
        let mut terminal_buffer_map: HashMap<usize, BufferId> = HashMap::new();
        if !session.terminals.is_empty() {
            if let Some(ref bridge) = self.async_bridge {
                self.terminal_manager.set_async_bridge(bridge.clone());
            }
            for terminal in &session.terminals {
                if let Some(buffer_id) = self.restore_terminal_from_session(terminal) {
                    terminal_buffer_map.insert(terminal.terminal_index, buffer_id);
                }
            }
        }

        // 6. Rebuild split layout from the saved tree
        // Map old split IDs to new ones as we create splits
        let mut split_id_map: HashMap<usize, SplitId> = HashMap::new();
        self.restore_split_node(
            &session.split_layout,
            &path_to_buffer,
            &terminal_buffer_map,
            &session.split_states,
            &mut split_id_map,
            true, // is_first_leaf - the first leaf reuses the existing split
        );

        // Set the active split based on the saved active_split_id
        // NOTE: active_buffer is now derived from split_manager, which was already
        // correctly set up by restore_split_view_state() via set_split_buffer()
        if let Some(&new_active_split) = split_id_map.get(&session.active_split_id) {
            self.split_manager.set_active_split(new_active_split);
        }

        // 7. Restore bookmarks
        for (key, bookmark) in &session.bookmarks {
            if let Some(&buffer_id) = path_to_buffer.get(&bookmark.file_path) {
                // Verify position is valid
                if let Some(buffer) = self.buffers.get(&buffer_id) {
                    let pos = bookmark.position.min(buffer.buffer.len());
                    self.bookmarks.insert(
                        *key,
                        Bookmark {
                            buffer_id,
                            position: pos,
                        },
                    );
                }
            }
        }

        tracing::debug!(
            "Session restore complete: {} splits, {} buffers",
            self.split_view_states.len(),
            self.buffers.len()
        );

        Ok(())
    }

    /// Restore a terminal from serialized session metadata.
    fn restore_terminal_from_session(
        &mut self,
        terminal: &SerializedTerminalSession,
    ) -> Option<BufferId> {
        // Resolve paths (accept absolute; otherwise treat as relative to terminals dir)
        let terminals_root = self.dir_context.terminal_dir_for(&self.working_dir);
        let log_path = if terminal.log_path.is_absolute() {
            terminal.log_path.clone()
        } else {
            terminals_root.join(&terminal.log_path)
        };
        let backing_path = if terminal.backing_path.is_absolute() {
            terminal.backing_path.clone()
        } else {
            terminals_root.join(&terminal.backing_path)
        };

        let _ = std::fs::create_dir_all(
            log_path
                .parent()
                .or_else(|| backing_path.parent())
                .unwrap_or(&terminals_root),
        );

        // Record paths using the predicted ID so buffer creation can reuse them
        let predicted_id = self.terminal_manager.next_terminal_id();
        self.terminal_log_files
            .insert(predicted_id, log_path.clone());
        self.terminal_backing_files
            .insert(predicted_id, backing_path.clone());

        // Spawn the terminal
        let terminal_id = match self.terminal_manager.spawn(
            terminal.cols,
            terminal.rows,
            terminal.cwd.clone(),
            Some(log_path.clone()),
        ) {
            Ok(id) => id,
            Err(e) => {
                tracing::warn!(
                    "Failed to restore terminal {}: {}",
                    terminal.terminal_index,
                    e
                );
                return None;
            }
        };

        // Ensure maps keyed by actual ID
        if terminal_id != predicted_id {
            self.terminal_log_files
                .insert(terminal_id, log_path.clone());
            self.terminal_backing_files
                .insert(terminal_id, backing_path.clone());
            self.terminal_log_files.remove(&predicted_id);
            self.terminal_backing_files.remove(&predicted_id);
        }

        // Create buffer for this terminal
        let buffer_id = self.create_terminal_buffer_detached(terminal_id);

        // Replay log into terminal emulator for immediate display
        self.replay_terminal_log_into_state(terminal_id, &log_path);
        // Sync rendered content to backing file/buffer
        self.sync_terminal_to_buffer(buffer_id);

        Some(buffer_id)
    }

    fn replay_terminal_log_into_state(&self, terminal_id: TerminalId, log_path: &Path) {
        if let Some(handle) = self.terminal_manager.get(terminal_id) {
            if let Ok(mut state) = handle.state.lock() {
                if let Ok(mut file) = std::fs::File::open(log_path) {
                    let mut buf = [0u8; 4096];
                    while let Ok(n) = file.read(&mut buf) {
                        if n == 0 {
                            break;
                        }
                        state.process_output(&buf[..n]);
                    }
                }
            }
        }
    }

    /// Internal helper to open a file and return its buffer ID
    fn open_file_internal(&mut self, path: &Path) -> Result<BufferId, SessionError> {
        // Check if file is already open
        for (buffer_id, metadata) in &self.buffer_metadata {
            if let Some(file_path) = metadata.file_path() {
                if file_path == path {
                    return Ok(*buffer_id);
                }
            }
        }

        // File not open, open it using the Editor's open_file method
        self.open_file(path).map_err(SessionError::Io)
    }

    /// Recursively restore the split layout from a serialized tree
    fn restore_split_node(
        &mut self,
        node: &SerializedSplitNode,
        path_to_buffer: &HashMap<PathBuf, BufferId>,
        terminal_buffers: &HashMap<usize, BufferId>,
        split_states: &HashMap<usize, SerializedSplitViewState>,
        split_id_map: &mut HashMap<usize, SplitId>,
        is_first_leaf: bool,
    ) {
        match node {
            SerializedSplitNode::Leaf {
                file_path,
                split_id,
            } => {
                // Get the buffer for this file, or use the default buffer
                let buffer_id = file_path
                    .as_ref()
                    .and_then(|p| path_to_buffer.get(p).copied())
                    .unwrap_or(self.active_buffer());

                let current_split_id = if is_first_leaf {
                    // First leaf reuses the existing split
                    let split_id_val = self.split_manager.active_split();
                    let _ = self.split_manager.set_split_buffer(split_id_val, buffer_id);
                    split_id_val
                } else {
                    // Non-first leaves use the active split (created by split_active)
                    self.split_manager.active_split()
                };

                // Map old split ID to new one
                split_id_map.insert(*split_id, current_split_id);

                // Restore the view state for this split
                self.restore_split_view_state(
                    current_split_id,
                    *split_id,
                    split_states,
                    path_to_buffer,
                    terminal_buffers,
                );
            }
            SerializedSplitNode::Terminal {
                terminal_index,
                split_id,
            } => {
                let buffer_id = terminal_buffers
                    .get(terminal_index)
                    .copied()
                    .unwrap_or(self.active_buffer());

                let current_split_id = if is_first_leaf {
                    let split_id_val = self.split_manager.active_split();
                    let _ = self.split_manager.set_split_buffer(split_id_val, buffer_id);
                    split_id_val
                } else {
                    self.split_manager.active_split()
                };

                split_id_map.insert(*split_id, current_split_id);

                let _ = self
                    .split_manager
                    .set_split_buffer(current_split_id, buffer_id);

                self.restore_split_view_state(
                    current_split_id,
                    *split_id,
                    split_states,
                    path_to_buffer,
                    terminal_buffers,
                );
            }
            SerializedSplitNode::Split {
                direction,
                first,
                second,
                ratio,
                split_id,
            } => {
                // First, restore the first child (it uses the current active split)
                self.restore_split_node(
                    first,
                    path_to_buffer,
                    terminal_buffers,
                    split_states,
                    split_id_map,
                    is_first_leaf,
                );

                // Get the buffer for the second child's first leaf
                let second_buffer_id =
                    get_first_leaf_buffer(second, path_to_buffer, terminal_buffers)
                        .unwrap_or(self.active_buffer());

                // Convert direction
                let split_direction = match direction {
                    SerializedSplitDirection::Horizontal => SplitDirection::Horizontal,
                    SerializedSplitDirection::Vertical => SplitDirection::Vertical,
                };

                // Create the split for the second child
                match self
                    .split_manager
                    .split_active(split_direction, second_buffer_id, *ratio)
                {
                    Ok(new_split_id) => {
                        // Create view state for the new split
                        let mut view_state = SplitViewState::with_buffer(
                            self.terminal_width,
                            self.terminal_height,
                            second_buffer_id,
                        );
                        view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                        self.split_view_states.insert(new_split_id, view_state);

                        // Map the container split ID (though we mainly care about leaves)
                        split_id_map.insert(*split_id, new_split_id);

                        // Recursively restore the second child (it's now in the new split)
                        self.restore_split_node(
                            second,
                            path_to_buffer,
                            terminal_buffers,
                            split_states,
                            split_id_map,
                            false,
                        );
                    }
                    Err(e) => {
                        tracing::error!("Failed to create split during session restore: {}", e);
                    }
                }
            }
        }
    }

    /// Restore view state for a specific split
    fn restore_split_view_state(
        &mut self,
        current_split_id: SplitId,
        saved_split_id: usize,
        split_states: &HashMap<usize, SerializedSplitViewState>,
        path_to_buffer: &HashMap<PathBuf, BufferId>,
        terminal_buffers: &HashMap<usize, BufferId>,
    ) {
        // Try to find the saved state for this split
        let Some(split_state) = split_states.get(&saved_split_id) else {
            return;
        };

        let Some(view_state) = self.split_view_states.get_mut(&current_split_id) else {
            return;
        };

        let mut active_buffer_id: Option<BufferId> = None;

        if !split_state.open_tabs.is_empty() {
            for tab in &split_state.open_tabs {
                match tab {
                    SerializedTabRef::File(rel_path) => {
                        if let Some(&buffer_id) = path_to_buffer.get(rel_path) {
                            if !view_state.open_buffers.contains(&buffer_id) {
                                view_state.open_buffers.push(buffer_id);
                            }
                            if terminal_buffers.values().any(|&tid| tid == buffer_id) {
                                view_state.viewport.line_wrap_enabled = false;
                            }
                        }
                    }
                    SerializedTabRef::Terminal(index) => {
                        if let Some(&buffer_id) = terminal_buffers.get(index) {
                            if !view_state.open_buffers.contains(&buffer_id) {
                                view_state.open_buffers.push(buffer_id);
                            }
                            view_state.viewport.line_wrap_enabled = false;
                        }
                    }
                }
            }

            if let Some(active_idx) = split_state.active_tab_index {
                if let Some(tab) = split_state.open_tabs.get(active_idx) {
                    active_buffer_id = match tab {
                        SerializedTabRef::File(rel) => path_to_buffer.get(rel).copied(),
                        SerializedTabRef::Terminal(index) => terminal_buffers.get(index).copied(),
                    };
                }
            }
        } else {
            // Backward compatibility path using open_files/active_file_index
            for rel_path in &split_state.open_files {
                if let Some(&buffer_id) = path_to_buffer.get(rel_path) {
                    if !view_state.open_buffers.contains(&buffer_id) {
                        view_state.open_buffers.push(buffer_id);
                    }
                }
            }

            let active_file_path = split_state.open_files.get(split_state.active_file_index);
            active_buffer_id =
                active_file_path.and_then(|rel_path| path_to_buffer.get(rel_path).copied());
        }

        // Restore cursor and scroll for the active file
        if let Some(active_id) = active_buffer_id {
            // Find the file state for the active buffer
            for (rel_path, file_state) in &split_state.file_states {
                let buffer_for_path = path_to_buffer.get(rel_path).copied();
                if buffer_for_path == Some(active_id) {
                    if let Some(buffer) = self.buffers.get(&active_id) {
                        let max_pos = buffer.buffer.len();
                        let cursor_pos = file_state.cursor.position.min(max_pos);

                        // Set cursor in SplitViewState
                        view_state.cursors.primary_mut().position = cursor_pos;
                        view_state.cursors.primary_mut().anchor =
                            file_state.cursor.anchor.map(|a| a.min(max_pos));
                        view_state.cursors.primary_mut().sticky_column =
                            file_state.cursor.sticky_column;

                        // Set scroll position
                        view_state.viewport.top_byte = file_state.scroll.top_byte.min(max_pos);
                        view_state.viewport.top_view_line_offset =
                            file_state.scroll.top_view_line_offset;
                        view_state.viewport.left_column = file_state.scroll.left_column;
                        // Mark viewport to skip sync on first resize after session restore
                        // This prevents ensure_visible from overwriting the restored scroll position
                        view_state.viewport.set_skip_resize_sync();

                        tracing::trace!(
                            "Restored SplitViewState for {:?}: cursor={}, top_byte={}",
                            rel_path,
                            cursor_pos,
                            view_state.viewport.top_byte
                        );
                    }

                    // Also set cursor in EditorState (authoritative for cursors)
                    if let Some(editor_state) = self.buffers.get_mut(&active_id) {
                        let max_pos = editor_state.buffer.len();
                        let cursor_pos = file_state.cursor.position.min(max_pos);
                        editor_state.cursors.primary_mut().position = cursor_pos;
                        editor_state.cursors.primary_mut().anchor =
                            file_state.cursor.anchor.map(|a| a.min(max_pos));
                        editor_state.cursors.primary_mut().sticky_column =
                            file_state.cursor.sticky_column;
                        // Note: viewport is now exclusively owned by SplitViewState (restored above)
                    }
                    break;
                }
            }

            // Set this buffer as active in the split
            let _ = self
                .split_manager
                .set_split_buffer(current_split_id, active_id);
        }

        // Restore view mode
        view_state.view_mode = match split_state.view_mode {
            SerializedViewMode::Source => ViewMode::Source,
            SerializedViewMode::Compose => ViewMode::Compose,
        };
        view_state.compose_width = split_state.compose_width;
        view_state.tab_scroll_offset = split_state.tab_scroll_offset;
    }
}

/// Helper: Get the buffer ID from the first leaf node in a split tree
fn get_first_leaf_buffer(
    node: &SerializedSplitNode,
    path_to_buffer: &HashMap<PathBuf, BufferId>,
    terminal_buffers: &HashMap<usize, BufferId>,
) -> Option<BufferId> {
    match node {
        SerializedSplitNode::Leaf { file_path, .. } => file_path
            .as_ref()
            .and_then(|p| path_to_buffer.get(p).copied()),
        SerializedSplitNode::Terminal { terminal_index, .. } => {
            terminal_buffers.get(terminal_index).copied()
        }
        SerializedSplitNode::Split { first, .. } => {
            get_first_leaf_buffer(first, path_to_buffer, terminal_buffers)
        }
    }
}

// ============================================================================
// Serialization helpers
// ============================================================================

fn serialize_split_node(
    node: &SplitNode,
    buffer_metadata: &HashMap<BufferId, super::types::BufferMetadata>,
    working_dir: &Path,
    terminal_buffers: &HashMap<BufferId, TerminalId>,
    terminal_indices: &HashMap<TerminalId, usize>,
) -> SerializedSplitNode {
    match node {
        SplitNode::Leaf {
            buffer_id,
            split_id,
        } => {
            if let Some(terminal_id) = terminal_buffers.get(buffer_id) {
                if let Some(index) = terminal_indices.get(terminal_id) {
                    return SerializedSplitNode::Terminal {
                        terminal_index: *index,
                        split_id: split_id.0,
                    };
                }
            }

            let file_path = buffer_metadata
                .get(buffer_id)
                .and_then(|meta| meta.file_path())
                .and_then(|abs_path| {
                    abs_path
                        .strip_prefix(working_dir)
                        .ok()
                        .map(|p| p.to_path_buf())
                });

            SerializedSplitNode::Leaf {
                file_path,
                split_id: split_id.0,
            }
        }
        SplitNode::Split {
            direction,
            first,
            second,
            ratio,
            split_id,
        } => SerializedSplitNode::Split {
            direction: match direction {
                SplitDirection::Horizontal => SerializedSplitDirection::Horizontal,
                SplitDirection::Vertical => SerializedSplitDirection::Vertical,
            },
            first: Box::new(serialize_split_node(
                first,
                buffer_metadata,
                working_dir,
                terminal_buffers,
                terminal_indices,
            )),
            second: Box::new(serialize_split_node(
                second,
                buffer_metadata,
                working_dir,
                terminal_buffers,
                terminal_indices,
            )),
            ratio: *ratio,
            split_id: split_id.0,
        },
    }
}

fn serialize_split_view_state(
    view_state: &crate::view::split::SplitViewState,
    buffer_metadata: &HashMap<BufferId, super::types::BufferMetadata>,
    working_dir: &Path,
    active_buffer: Option<BufferId>,
    terminal_buffers: &HashMap<BufferId, TerminalId>,
    terminal_indices: &HashMap<TerminalId, usize>,
) -> SerializedSplitViewState {
    let mut open_tabs = Vec::new();
    let mut open_files = Vec::new();
    let mut active_tab_index = None;

    for buffer_id in &view_state.open_buffers {
        let tab_index = open_tabs.len();
        if let Some(terminal_id) = terminal_buffers.get(buffer_id) {
            if let Some(idx) = terminal_indices.get(terminal_id) {
                open_tabs.push(SerializedTabRef::Terminal(*idx));
                if Some(*buffer_id) == active_buffer {
                    active_tab_index = Some(tab_index);
                }
                continue;
            }
        }

        if let Some(rel_path) = buffer_metadata
            .get(buffer_id)
            .and_then(|meta| meta.file_path())
            .and_then(|abs_path| abs_path.strip_prefix(working_dir).ok())
        {
            open_tabs.push(SerializedTabRef::File(rel_path.to_path_buf()));
            open_files.push(rel_path.to_path_buf());
            if Some(*buffer_id) == active_buffer {
                active_tab_index = Some(tab_index);
            }
        }
    }

    // Derive active_file_index for backward compatibility
    let active_file_index = active_tab_index
        .and_then(|idx| open_tabs.get(idx))
        .and_then(|tab| match tab {
            SerializedTabRef::File(path) => {
                Some(open_files.iter().position(|p| p == path).unwrap_or(0))
            }
            _ => None,
        })
        .unwrap_or(0);

    // Serialize file states - only save cursor/scroll for the ACTIVE buffer if it is a file
    let mut file_states = HashMap::new();
    if let Some(active_id) = active_buffer {
        if let Some(meta) = buffer_metadata.get(&active_id) {
            if let Some(abs_path) = meta.file_path() {
                if let Ok(rel_path) = abs_path.strip_prefix(working_dir) {
                    let primary_cursor = view_state.cursors.primary();

                    file_states.insert(
                        rel_path.to_path_buf(),
                        SerializedFileState {
                            cursor: SerializedCursor {
                                position: primary_cursor.position,
                                anchor: primary_cursor.anchor,
                                sticky_column: primary_cursor.sticky_column,
                            },
                            additional_cursors: view_state
                                .cursors
                                .iter()
                                .skip(1) // Skip primary
                                .map(|(_, cursor)| SerializedCursor {
                                    position: cursor.position,
                                    anchor: cursor.anchor,
                                    sticky_column: cursor.sticky_column,
                                })
                                .collect(),
                            scroll: SerializedScroll {
                                top_byte: view_state.viewport.top_byte,
                                top_view_line_offset: view_state.viewport.top_view_line_offset,
                                left_column: view_state.viewport.left_column,
                            },
                        },
                    );
                }
            }
        }
    }

    SerializedSplitViewState {
        open_tabs,
        active_tab_index,
        open_files,
        active_file_index,
        file_states,
        tab_scroll_offset: view_state.tab_scroll_offset,
        view_mode: match view_state.view_mode {
            ViewMode::Source => SerializedViewMode::Source,
            ViewMode::Compose => SerializedViewMode::Compose,
        },
        compose_width: view_state.compose_width,
    }
}

fn serialize_bookmarks(
    bookmarks: &HashMap<char, Bookmark>,
    buffer_metadata: &HashMap<BufferId, super::types::BufferMetadata>,
    working_dir: &Path,
) -> HashMap<char, SerializedBookmark> {
    bookmarks
        .iter()
        .filter_map(|(key, bookmark)| {
            buffer_metadata
                .get(&bookmark.buffer_id)
                .and_then(|meta| meta.file_path())
                .and_then(|abs_path| {
                    abs_path.strip_prefix(working_dir).ok().map(|rel_path| {
                        (
                            *key,
                            SerializedBookmark {
                                file_path: rel_path.to_path_buf(),
                                position: bookmark.position,
                            },
                        )
                    })
                })
        })
        .collect()
}

/// Collect all unique file paths from split_states
fn collect_file_paths_from_states(
    split_states: &HashMap<usize, SerializedSplitViewState>,
) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    for state in split_states.values() {
        if !state.open_tabs.is_empty() {
            for tab in &state.open_tabs {
                if let SerializedTabRef::File(path) = tab {
                    if !paths.contains(path) {
                        paths.push(path.clone());
                    }
                }
            }
        } else {
            for path in &state.open_files {
                if !paths.contains(path) {
                    paths.push(path.clone());
                }
            }
        }
    }
    paths
}

/// Get list of expanded directories from a FileTreeView
fn get_expanded_dirs(
    explorer: &crate::view::file_tree::FileTreeView,
    working_dir: &Path,
) -> Vec<PathBuf> {
    let mut expanded = Vec::new();
    let tree = explorer.tree();

    // Iterate through all nodes and collect expanded directories
    for node in tree.all_nodes() {
        if node.is_expanded() && node.is_dir() {
            // Get the path and make it relative to working_dir
            if let Ok(rel_path) = node.entry.path.strip_prefix(working_dir) {
                expanded.push(rel_path.to_path_buf());
            }
        }
    }

    expanded
}
