//! Workspace persistence integration for the Editor
//!
//! This module provides conversion between live Editor state and serialized Workspace data.
//!
//! # Role in Incremental Streaming Architecture
//!
//! This module handles workspace save/restore for terminals.
//! See `crate::services::terminal` for the full architecture diagram.
//!
//! ## Workspace Save
//!
//! [`Editor::save_workspace`] calls [`Editor::sync_all_terminal_backing_files`] to ensure
//! all terminal backing files contain complete state (scrollback + visible screen)
//! before serializing workspace metadata.
//!
//! ## Workspace Restore
//!
//! [`Editor::restore_terminal_from_workspace`] loads the backing file directly as a
//! read-only buffer, skipping the expensive log replay. The user starts in scrollback
//! mode viewing the last workspace state. A new PTY is spawned when they re-enter
//! terminal mode.
//!
//! Performance: O(1) ≈ 10ms (lazy load) vs O(n) ≈ 1000ms (log replay)

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::Instant;

use crate::state::EditorState;

use crate::model::event::{BufferId, SplitDirection, SplitId};
use crate::services::terminal::TerminalId;
use crate::state::ViewMode;
use crate::view::split::{SplitNode, SplitViewState};
use crate::workspace::{
    FileExplorerState, PersistedFileWorkspace, SearchOptions, SerializedBookmark, SerializedCursor,
    SerializedFileState, SerializedScroll, SerializedSplitDirection, SerializedSplitNode,
    SerializedSplitViewState, SerializedTabRef, SerializedTerminalWorkspace, SerializedViewMode,
    Workspace, WorkspaceConfigOverrides, WorkspaceError, WorkspaceHistories, WORKSPACE_VERSION,
};

use super::types::Bookmark;
use super::Editor;

/// Workspace persistence state tracker
///
/// Tracks dirty state and handles debounced saving for crash resistance.
pub struct WorkspaceTracker {
    /// Whether workspace has unsaved changes
    dirty: bool,
    /// Last save time
    last_save: Instant,
    /// Minimum interval between saves (debounce)
    save_interval: std::time::Duration,
    /// Whether workspace persistence is enabled
    enabled: bool,
}

impl WorkspaceTracker {
    /// Create a new workspace tracker
    pub fn new(enabled: bool) -> Self {
        Self {
            dirty: false,
            last_save: Instant::now(),
            save_interval: std::time::Duration::from_secs(5),
            enabled,
        }
    }

    /// Check if workspace tracking is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Mark workspace as needing save
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
    /// Capture current editor state into a Workspace
    pub fn capture_workspace(&self) -> Workspace {
        tracing::debug!("Capturing workspace for {:?}", self.working_dir);

        // Collect terminal metadata for workspace restore
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
                    .unwrap_or_else(crate::services::terminal::detect_shell);
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

                terminals.push(SerializedTerminalWorkspace {
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
            self.split_manager.labels(),
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
                show_hidden: explorer.ignore_patterns().show_hidden(),
                show_gitignored: explorer.ignore_patterns().show_gitignored(),
            }
        } else {
            FileExplorerState {
                visible: self.file_explorer_visible,
                width_percent: self.file_explorer_width_percent,
                expanded_dirs: Vec::new(),
                scroll_offset: 0,
                show_hidden: false,
                show_gitignored: false,
            }
        };

        // Capture config overrides (only store deviations from defaults)
        let config_overrides = WorkspaceConfigOverrides {
            line_numbers: Some(self.config.editor.line_numbers),
            relative_line_numbers: Some(self.config.editor.relative_line_numbers),
            line_wrap: Some(self.config.editor.line_wrap),
            syntax_highlighting: Some(self.config.editor.syntax_highlighting),
            enable_inlay_hints: Some(self.config.editor.enable_inlay_hints),
            mouse_enabled: Some(self.mouse_enabled),
            menu_bar_hidden: Some(!self.menu_bar_visible),
        };

        // Capture histories using the items() accessor from the prompt_histories HashMap
        let histories = WorkspaceHistories {
            search: self
                .prompt_histories
                .get("search")
                .map(|h| h.items().to_vec())
                .unwrap_or_default(),
            replace: self
                .prompt_histories
                .get("replace")
                .map(|h| h.items().to_vec())
                .unwrap_or_default(),
            command_palette: Vec::new(), // Future: when command palette has history
            goto_line: self
                .prompt_histories
                .get("goto_line")
                .map(|h| h.items().to_vec())
                .unwrap_or_default(),
            open_file: Vec::new(), // Future: when file open prompt has history
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

        // Capture external files (files outside working_dir)
        // These are stored as absolute paths since they can't be made relative
        let external_files: Vec<PathBuf> = self
            .buffer_metadata
            .values()
            .filter_map(|meta| meta.file_path())
            .filter(|abs_path| abs_path.strip_prefix(&self.working_dir).is_err())
            .cloned()
            .collect();
        if !external_files.is_empty() {
            tracing::debug!("Captured {} external files", external_files.len());
        }

        Workspace {
            version: WORKSPACE_VERSION,
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
            external_files,
            saved_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        }
    }

    /// Save the current workspace to disk
    ///
    /// Ensures all active terminals have their visible screen synced to
    /// backing files before capturing the workspace.
    /// Also saves global file states (scroll/cursor positions per file).
    pub fn save_workspace(&mut self) -> Result<(), WorkspaceError> {
        // Ensure all terminal backing files have complete state before saving
        self.sync_all_terminal_backing_files();

        // Save global file states for all open file buffers
        self.save_all_global_file_states();

        let workspace = self.capture_workspace();
        workspace.save()
    }

    /// Save global file states for all open file buffers
    fn save_all_global_file_states(&self) {
        // Collect all file states from all splits
        for (split_id, view_state) in &self.split_view_states {
            // Get the active buffer for this split
            let active_buffer = self
                .split_manager
                .root()
                .get_leaves_with_rects(ratatui::layout::Rect::default())
                .into_iter()
                .find(|(sid, _, _)| *sid == *split_id)
                .map(|(_, buffer_id, _)| buffer_id);

            if let Some(buffer_id) = active_buffer {
                self.save_buffer_file_state(buffer_id, view_state);
            }
        }
    }

    /// Save file state for a specific buffer (used when closing files and saving workspace)
    fn save_buffer_file_state(&self, buffer_id: BufferId, view_state: &SplitViewState) {
        // Get the file path for this buffer
        let abs_path = match self.buffer_metadata.get(&buffer_id) {
            Some(metadata) => match metadata.file_path() {
                Some(path) => path.to_path_buf(),
                None => return, // Not a file buffer
            },
            None => return,
        };

        // Capture the current state
        let primary_cursor = view_state.cursors.primary();
        let file_state = SerializedFileState {
            cursor: SerializedCursor {
                position: primary_cursor.position,
                anchor: primary_cursor.anchor,
                sticky_column: primary_cursor.sticky_column,
            },
            additional_cursors: view_state
                .cursors
                .iter()
                .skip(1)
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
            view_mode: Default::default(),
            compose_width: None,
            plugin_state: std::collections::HashMap::new(),
        };

        // Save to disk immediately
        PersistedFileWorkspace::save(&abs_path, file_state);
    }

    /// Sync all active terminal visible screens to their backing files.
    ///
    /// Called before workspace save to ensure backing files contain complete
    /// terminal state (scrollback + visible screen).
    fn sync_all_terminal_backing_files(&mut self) {
        use std::io::BufWriter;

        // Collect terminal IDs and their backing paths
        let terminals_to_sync: Vec<_> = self
            .terminal_buffers
            .values()
            .copied()
            .filter_map(|terminal_id| {
                self.terminal_backing_files
                    .get(&terminal_id)
                    .map(|path| (terminal_id, path.clone()))
            })
            .collect();

        for (terminal_id, backing_path) in terminals_to_sync {
            if let Some(handle) = self.terminal_manager.get(terminal_id) {
                if let Ok(state) = handle.state.lock() {
                    // Append visible screen to backing file
                    if let Ok(mut file) = self.filesystem.open_file_for_append(&backing_path) {
                        let mut writer = BufWriter::new(&mut *file);
                        if let Err(e) = state.append_visible_screen(&mut writer) {
                            tracing::warn!(
                                "Failed to sync terminal {:?} to backing file: {}",
                                terminal_id,
                                e
                            );
                        }
                    }
                }
            }
        }
    }

    /// Try to load and apply a workspace for the current working directory
    ///
    /// Returns true if a workspace was successfully loaded and applied.
    pub fn try_restore_workspace(&mut self) -> Result<bool, WorkspaceError> {
        tracing::debug!("Attempting to restore workspace for {:?}", self.working_dir);
        match Workspace::load(&self.working_dir)? {
            Some(workspace) => {
                tracing::info!("Found workspace, applying...");
                self.apply_workspace(&workspace)?;
                Ok(true)
            }
            None => {
                tracing::debug!("No workspace found for {:?}", self.working_dir);
                Ok(false)
            }
        }
    }

    /// Apply a loaded workspace to the editor
    pub fn apply_workspace(&mut self, workspace: &Workspace) -> Result<(), WorkspaceError> {
        tracing::debug!(
            "Applying workspace with {} split states",
            workspace.split_states.len()
        );

        // 1. Apply config overrides
        if let Some(line_numbers) = workspace.config_overrides.line_numbers {
            self.config.editor.line_numbers = line_numbers;
        }
        if let Some(relative_line_numbers) = workspace.config_overrides.relative_line_numbers {
            self.config.editor.relative_line_numbers = relative_line_numbers;
        }
        if let Some(line_wrap) = workspace.config_overrides.line_wrap {
            self.config.editor.line_wrap = line_wrap;
        }
        if let Some(syntax_highlighting) = workspace.config_overrides.syntax_highlighting {
            self.config.editor.syntax_highlighting = syntax_highlighting;
        }
        if let Some(enable_inlay_hints) = workspace.config_overrides.enable_inlay_hints {
            self.config.editor.enable_inlay_hints = enable_inlay_hints;
        }
        if let Some(mouse_enabled) = workspace.config_overrides.mouse_enabled {
            self.mouse_enabled = mouse_enabled;
        }
        if let Some(menu_bar_hidden) = workspace.config_overrides.menu_bar_hidden {
            self.menu_bar_visible = !menu_bar_hidden;
        }

        // 2. Restore search options
        self.search_case_sensitive = workspace.search_options.case_sensitive;
        self.search_whole_word = workspace.search_options.whole_word;
        self.search_use_regex = workspace.search_options.use_regex;
        self.search_confirm_each = workspace.search_options.confirm_each;

        // 3. Restore histories (merge with any existing)
        tracing::debug!(
            "Restoring histories: {} search, {} replace, {} goto_line",
            workspace.histories.search.len(),
            workspace.histories.replace.len(),
            workspace.histories.goto_line.len()
        );
        for item in &workspace.histories.search {
            self.get_or_create_prompt_history("search")
                .push(item.clone());
        }
        for item in &workspace.histories.replace {
            self.get_or_create_prompt_history("replace")
                .push(item.clone());
        }
        for item in &workspace.histories.goto_line {
            self.get_or_create_prompt_history("goto_line")
                .push(item.clone());
        }

        // 4. Restore file explorer state
        self.file_explorer_visible = workspace.file_explorer.visible;
        self.file_explorer_width_percent = workspace.file_explorer.width_percent;

        // Store pending show_hidden and show_gitignored settings (fixes #569)
        // These will be applied when the file explorer is initialized (async)
        if workspace.file_explorer.show_hidden {
            self.pending_file_explorer_show_hidden = Some(true);
        }
        if workspace.file_explorer.show_gitignored {
            self.pending_file_explorer_show_gitignored = Some(true);
        }

        // Initialize file explorer if it was visible in the workspace
        // Note: We keep key_context as Normal so the editor has focus, not the explorer
        if self.file_explorer_visible && self.file_explorer.is_none() {
            self.init_file_explorer();
        }

        // 5. Open files from the workspace and build buffer mappings
        // Collect all unique file paths from split_states (which tracks all open files per split)
        let file_paths = collect_file_paths_from_states(&workspace.split_states);
        tracing::debug!(
            "Workspace has {} files to restore: {:?}",
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

        tracing::debug!("Opened {} files from workspace", path_to_buffer.len());

        // 5b. Restore external files (files outside the working directory)
        // These are stored as absolute paths
        if !workspace.external_files.is_empty() {
            tracing::debug!(
                "Restoring {} external files: {:?}",
                workspace.external_files.len(),
                workspace.external_files
            );
            for abs_path in &workspace.external_files {
                if abs_path.exists() {
                    match self.open_file_internal(abs_path) {
                        Ok(buffer_id) => {
                            tracing::debug!(
                                "Restored external file {:?} as buffer {:?}",
                                abs_path,
                                buffer_id
                            );
                        }
                        Err(e) => {
                            tracing::warn!("Failed to restore external file {:?}: {}", abs_path, e);
                        }
                    }
                } else {
                    tracing::debug!("Skipping non-existent external file: {:?}", abs_path);
                }
            }
        }

        // Restore terminals and build index -> buffer map
        let mut terminal_buffer_map: HashMap<usize, BufferId> = HashMap::new();
        if !workspace.terminals.is_empty() {
            if let Some(ref bridge) = self.async_bridge {
                self.terminal_manager.set_async_bridge(bridge.clone());
            }
            for terminal in &workspace.terminals {
                if let Some(buffer_id) = self.restore_terminal_from_workspace(terminal) {
                    terminal_buffer_map.insert(terminal.terminal_index, buffer_id);
                }
            }
        }

        // 6. Rebuild split layout from the saved tree
        // Map old split IDs to new ones as we create splits
        let mut split_id_map: HashMap<usize, SplitId> = HashMap::new();
        self.restore_split_node(
            &workspace.split_layout,
            &path_to_buffer,
            &terminal_buffer_map,
            &workspace.split_states,
            &mut split_id_map,
            true, // is_first_leaf - the first leaf reuses the existing split
        );

        // Set the active split based on the saved active_split_id
        // NOTE: active_buffer is now derived from split_manager, which was already
        // correctly set up by restore_split_view_state() via set_split_buffer()
        if let Some(&new_active_split) = split_id_map.get(&workspace.active_split_id) {
            self.split_manager.set_active_split(new_active_split);
        }

        // 7. Restore bookmarks
        for (key, bookmark) in &workspace.bookmarks {
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
            "Workspace restore complete: {} splits, {} buffers",
            self.split_view_states.len(),
            self.buffers.len()
        );

        // Fire buffer_activated for the active buffer so plugins can
        // re-enable compose mode (the plugin's composeBuffers set is empty
        // after restart). Only fires for the active buffer — other buffers
        // will get buffer_activated when the user switches to them.
        #[cfg(feature = "plugins")]
        {
            let buffer_id = self.active_buffer();
            self.update_plugin_state_snapshot();
            tracing::debug!(
                "Firing buffer_activated for active buffer {:?} after workspace restore",
                buffer_id
            );
            self.plugin_manager.run_hook(
                "buffer_activated",
                crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
            );
        }

        Ok(())
    }

    /// Restore a terminal from serialized workspace metadata.
    ///
    /// Uses the incremental streaming architecture for fast restore:
    /// 1. Load backing file directly as read-only buffer (lazy load)
    /// 2. Skip log replay entirely - user sees last workspace state immediately
    /// 3. Spawn new PTY for live terminal when user re-enters terminal mode
    ///
    /// Performance: O(1) for restore vs O(total_history) with log replay
    fn restore_terminal_from_workspace(
        &mut self,
        terminal: &SerializedTerminalWorkspace,
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

        let _ = self.filesystem.create_dir_all(
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

        // Spawn the terminal with backing file for incremental scrollback
        let terminal_id = match self.terminal_manager.spawn(
            terminal.cols,
            terminal.rows,
            terminal.cwd.clone(),
            Some(log_path.clone()),
            Some(backing_path.clone()),
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

        // Load backing file directly as read-only buffer (skip log replay)
        // The backing file already contains complete terminal state from last workspace
        self.load_terminal_backing_file_as_buffer(buffer_id, &backing_path);

        Some(buffer_id)
    }

    /// Load a terminal backing file directly as a read-only buffer.
    ///
    /// This is used for fast workspace restore - we load the pre-rendered backing
    /// file instead of replaying the raw log through the VTE parser.
    fn load_terminal_backing_file_as_buffer(&mut self, buffer_id: BufferId, backing_path: &Path) {
        // Check if backing file exists; if not, terminal starts empty
        if !backing_path.exists() {
            return;
        }

        let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;
        if let Ok(new_state) = EditorState::from_file_with_languages(
            backing_path,
            self.terminal_width,
            self.terminal_height,
            large_file_threshold,
            &self.grammar_registry,
            &self.config.languages,
            std::sync::Arc::clone(&self.filesystem),
        ) {
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                *state = new_state;
                // Move cursor to end of buffer
                let total = state.buffer.total_bytes();
                // Update cursor position in all splits that show this buffer
                for vs in self.split_view_states.values_mut() {
                    if vs.open_buffers.contains(&buffer_id) {
                        vs.cursors.primary_mut().position = total;
                    }
                }
                // Terminal buffers should never be considered "modified"
                state.buffer.set_modified(false);
                // Start in scrollback mode (editing disabled)
                state.editing_disabled = true;
                state.margins.configure_for_line_numbers(false);
            }
        }
    }

    /// Internal helper to open a file and return its buffer ID
    fn open_file_internal(&mut self, path: &Path) -> Result<BufferId, WorkspaceError> {
        // Check if file is already open
        for (buffer_id, metadata) in &self.buffer_metadata {
            if let Some(file_path) = metadata.file_path() {
                if file_path == path {
                    return Ok(*buffer_id);
                }
            }
        }

        // File not open, open it using the Editor's open_file method
        self.open_file(path).map_err(WorkspaceError::Io)
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
                label,
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

                // Restore label if present
                if let Some(label) = label {
                    self.split_manager
                        .set_label(current_split_id, label.clone());
                }

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
                label,
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

                // Restore label if present
                if let Some(label) = label {
                    self.split_manager
                        .set_label(current_split_id, label.clone());
                }

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
                        view_state.rulers = self.config.editor.rulers.clone();
                        view_state.show_line_numbers = self.config.editor.line_numbers;
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
                        tracing::error!("Failed to create split during workspace restore: {}", e);
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
                            // Ensure keyed state exists for this buffer
                            view_state.ensure_buffer_state(buffer_id);
                            if terminal_buffers.values().any(|&tid| tid == buffer_id) {
                                view_state
                                    .buffer_state_mut(buffer_id)
                                    .unwrap()
                                    .viewport
                                    .line_wrap_enabled = false;
                            }
                        }
                    }
                    SerializedTabRef::Terminal(index) => {
                        if let Some(&buffer_id) = terminal_buffers.get(index) {
                            if !view_state.open_buffers.contains(&buffer_id) {
                                view_state.open_buffers.push(buffer_id);
                            }
                            view_state
                                .ensure_buffer_state(buffer_id)
                                .viewport
                                .line_wrap_enabled = false;
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
                    view_state.ensure_buffer_state(buffer_id);
                }
            }

            let active_file_path = split_state.open_files.get(split_state.active_file_index);
            active_buffer_id =
                active_file_path.and_then(|rel_path| path_to_buffer.get(rel_path).copied());
        }

        // Restore cursor, scroll, view_mode, and compose_width for ALL buffers in file_states
        for (rel_path, file_state) in &split_state.file_states {
            let buffer_id = match path_to_buffer.get(rel_path).copied() {
                Some(id) => id,
                None => continue,
            };
            let max_pos = self
                .buffers
                .get(&buffer_id)
                .map(|b| b.buffer.len())
                .unwrap_or(0);

            // Ensure keyed state exists for this buffer
            let buf_state = view_state.ensure_buffer_state(buffer_id);

            let cursor_pos = file_state.cursor.position.min(max_pos);
            buf_state.cursors.primary_mut().position = cursor_pos;
            buf_state.cursors.primary_mut().anchor =
                file_state.cursor.anchor.map(|a| a.min(max_pos));
            buf_state.cursors.primary_mut().sticky_column = file_state.cursor.sticky_column;

            buf_state.viewport.top_byte = file_state.scroll.top_byte.min(max_pos);
            buf_state.viewport.top_view_line_offset = file_state.scroll.top_view_line_offset;
            buf_state.viewport.left_column = file_state.scroll.left_column;
            buf_state.viewport.set_skip_resize_sync();

            // Restore per-buffer view mode and compose width
            buf_state.view_mode = match file_state.view_mode {
                SerializedViewMode::Source => ViewMode::Source,
                SerializedViewMode::Compose => ViewMode::Compose,
            };
            buf_state.compose_width = file_state.compose_width;
            buf_state.plugin_state = file_state.plugin_state.clone();

            tracing::trace!(
                "Restored keyed state for {:?}: cursor={}, top_byte={}, view_mode={:?}",
                rel_path,
                cursor_pos,
                buf_state.viewport.top_byte,
                buf_state.view_mode,
            );
        }

        // For buffers without saved file_state (e.g., terminals), apply split-level
        // view_mode/compose_width as fallback (backward compatibility)
        let restored_view_mode = match split_state.view_mode {
            SerializedViewMode::Source => ViewMode::Source,
            SerializedViewMode::Compose => ViewMode::Compose,
        };

        if let Some(active_id) = active_buffer_id {
            // Switch the split to the active buffer
            view_state.switch_buffer(active_id);

            // If no per-buffer file_state was saved, apply split-level settings
            let active_has_file_state = split_state
                .file_states
                .keys()
                .any(|rel_path| path_to_buffer.get(rel_path).copied() == Some(active_id));
            if !active_has_file_state {
                view_state.active_state_mut().view_mode = restored_view_mode.clone();
                view_state.active_state_mut().compose_width = split_state.compose_width;
            }

            // Cursors now live in SplitViewState, no need to sync to EditorState

            // Set this buffer as active in the split (fires buffer_activated hook)
            let _ = self
                .split_manager
                .set_split_buffer(current_split_id, active_id);
        }
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
    split_labels: &HashMap<SplitId, String>,
) -> SerializedSplitNode {
    match node {
        SplitNode::Leaf {
            buffer_id,
            split_id,
        } => {
            let label = split_labels.get(split_id).cloned();

            if let Some(terminal_id) = terminal_buffers.get(buffer_id) {
                if let Some(index) = terminal_indices.get(terminal_id) {
                    return SerializedSplitNode::Terminal {
                        terminal_index: *index,
                        split_id: split_id.0,
                        label,
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
                label,
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
                split_labels,
            )),
            second: Box::new(serialize_split_node(
                second,
                buffer_metadata,
                working_dir,
                terminal_buffers,
                terminal_indices,
                split_labels,
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

    // Serialize file states for ALL buffers in keyed_states (not just the active one)
    let mut file_states = HashMap::new();
    for (buffer_id, buf_state) in &view_state.keyed_states {
        if let Some(meta) = buffer_metadata.get(buffer_id) {
            if let Some(abs_path) = meta.file_path() {
                if let Ok(rel_path) = abs_path.strip_prefix(working_dir) {
                    let primary_cursor = buf_state.cursors.primary();

                    file_states.insert(
                        rel_path.to_path_buf(),
                        SerializedFileState {
                            cursor: SerializedCursor {
                                position: primary_cursor.position,
                                anchor: primary_cursor.anchor,
                                sticky_column: primary_cursor.sticky_column,
                            },
                            additional_cursors: buf_state
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
                                top_byte: buf_state.viewport.top_byte,
                                top_view_line_offset: buf_state.viewport.top_view_line_offset,
                                left_column: buf_state.viewport.left_column,
                            },
                            view_mode: match buf_state.view_mode {
                                ViewMode::Source => SerializedViewMode::Source,
                                ViewMode::Compose => SerializedViewMode::Compose,
                            },
                            compose_width: buf_state.compose_width,
                            plugin_state: buf_state.plugin_state.clone(),
                        },
                    );
                }
            }
        }
    }

    // Active buffer's view_mode/compose_width for the split-level fields (backward compat)
    let active_view_mode = active_buffer
        .and_then(|id| view_state.keyed_states.get(&id))
        .map(|bs| match bs.view_mode {
            ViewMode::Source => SerializedViewMode::Source,
            ViewMode::Compose => SerializedViewMode::Compose,
        })
        .unwrap_or(SerializedViewMode::Source);
    let active_compose_width = active_buffer
        .and_then(|id| view_state.keyed_states.get(&id))
        .and_then(|bs| bs.compose_width);

    SerializedSplitViewState {
        open_tabs,
        active_tab_index,
        open_files,
        active_file_index,
        file_states,
        tab_scroll_offset: view_state.tab_scroll_offset,
        view_mode: active_view_mode,
        compose_width: active_compose_width,
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
