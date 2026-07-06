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

use crate::model::event::{BufferId, LeafId, SplitDirection, SplitId};
use crate::services::terminal::TerminalId;
use crate::state::ViewMode;
use crate::view::split::{SplitNode, SplitViewState};
use crate::workspace::{
    FileExplorerState, PersistedFileWorkspace, SearchOptions, SerializedBookmark, SerializedCursor,
    SerializedFileState, SerializedFoldRange, SerializedScroll, SerializedSplitDirection,
    SerializedSplitNode, SerializedSplitViewState, SerializedTabRef, SerializedTerminalWorkspace,
    SerializedViewMode, UnnamedBufferRef, Workspace, WorkspaceConfigOverrides, WorkspaceError,
    WorkspaceHistories, WORKSPACE_VERSION,
};

use super::bookmarks::{Bookmark, BookmarkState};
use super::Editor;

/// Resolve a saved fold's header_line against the current buffer, using
/// `header_text` to detect drift from external edits (issue #1568).
///
/// - If no `header_text` is available (older session files), trust the saved
///   line number.
/// - If the text at the saved line still matches, use that line.
/// - Otherwise, search a small window above and below the saved line for the
///   same text (trimmed) — lines may have shifted by a few either way after a
///   local external edit.
/// - If still not found, return `None` so the caller drops the fold rather
///   than re-attaching it to unrelated content.
fn resolve_fold_header_line(
    buffer: &crate::model::buffer::Buffer,
    saved_line: usize,
    header_text: Option<&str>,
) -> Option<usize> {
    let Some(expected) = header_text else {
        // Backward compatibility: no recorded text, trust the line number.
        return Some(saved_line);
    };
    let expected_trimmed = expected.trim();
    let line_matches = |line: usize| -> bool {
        buffer
            .get_line(line)
            .map(|bytes| {
                let text = String::from_utf8_lossy(&bytes);
                text.trim_end_matches('\n').trim_end_matches('\r').trim() == expected_trimmed
            })
            .unwrap_or(false)
    };
    if line_matches(saved_line) {
        return Some(saved_line);
    }
    // Search nearby (expanding outward) for the displaced header.
    const SEARCH_WINDOW: usize = 32;
    for delta in 1..=SEARCH_WINDOW {
        let above = saved_line.checked_sub(delta);
        if let Some(l) = above {
            if line_matches(l) {
                return Some(l);
            }
        }
        let below = saved_line.saturating_add(delta);
        if line_matches(below) {
            return Some(below);
        }
    }
    None
}

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
    /// Capture the active window into a `Workspace`.
    ///
    /// Delegates the per-window snapshot to `Window::capture_workspace`
    /// (rooted at the window's own `root`). Editor-global
    /// `plugin_global_state` is intentionally NOT embedded here — it
    /// persists once to the global `orchestrator/state/` store.
    pub fn capture_workspace(&self) -> Workspace {
        self.active_window().capture_workspace()
    }

    /// Editor-global plugin state (`getGlobalState`/`setGlobalState`),
    /// the live map persisted once to the global `orchestrator/state/`
    /// store. Deliberately separate from `capture_workspace`, which no
    /// longer embeds it per window. Read accessor for tests that assert
    /// a plugin recorded a cross-restart decision.
    pub fn plugin_global_state(
        &self,
    ) -> &std::collections::HashMap<String, std::collections::HashMap<String, serde_json::Value>>
    {
        &self.plugin_global_state
    }

    /// Save the current (active) window's workspace to disk. Thin
    /// active-window wrapper over [`Editor::save_workspace_for`].
    pub fn save_workspace(&mut self) -> Result<(), WorkspaceError> {
        self.save_workspace_for(self.active_window)
    }

    /// Try to load and apply a workspace for the active window. Thin
    /// active-window wrapper over [`Editor::restore_workspace_for`].
    ///
    /// Returns true if a workspace was successfully loaded and applied.
    pub fn try_restore_workspace(&mut self) -> Result<bool, WorkspaceError> {
        self.restore_workspace_for(self.active_window)
    }

    /// The single window-restore flow: load and apply window `id`'s
    /// persisted workspace, then apply the fresh-session file-explorer
    /// default. Shared by eager startup restore
    /// ([`Editor::restore_active_window_on_launch`]) and lazy
    /// dock/preview materialization ([`Editor::materialize_window`]) so a
    /// directory behaves identically however it is (re-)entered — a
    /// deliberately-closed explorer stays closed, a brand-new directory
    /// defaults to the tree. Operates on `windows[id]` directly, so it is
    /// correct for a background window that is not active yet.
    ///
    /// `opened_files` suppresses the explorer default when the launch
    /// opened specific files instead of a bare directory.
    pub fn restore_window(
        &mut self,
        id: fresh_core::WindowId,
        opened_files: bool,
    ) -> Result<bool, WorkspaceError> {
        let restored = self.restore_workspace_for(id)?;
        if let Some(win) = self.windows.get_mut(&id) {
            win.apply_fresh_session_explorer_default(opened_files, restored);
        }
        Ok(restored)
    }

    /// Eager startup restore of the active foreground window, applying
    /// the fresh-session explorer default. The launch-time counterpart to
    /// the lazy [`Editor::materialize_window`]; both funnel through
    /// [`Editor::restore_window`].
    pub fn restore_active_window_on_launch(
        &mut self,
        opened_files: bool,
    ) -> Result<bool, WorkspaceError> {
        self.restore_window(self.active_window, opened_files)
    }

    /// Apply the fresh-session explorer default to the active window for
    /// launch/enter paths that did *not* run a workspace restore (e.g.
    /// `--no-restore`, or a new directory opened into a running instance).
    /// Same rule as [`Editor::restore_window`], just without a restore to
    /// bundle it with.
    pub fn apply_active_window_explorer_default(
        &mut self,
        opened_files: bool,
        workspace_restored: bool,
    ) {
        self.active_window_mut()
            .apply_fresh_session_explorer_default(opened_files, workspace_restored);
    }

    /// Apply hot exit recovery to all currently open file-backed buffers.
    ///
    /// This restores unsaved changes from recovery files for buffers that were
    /// opened via CLI (without workspace restore). Returns the number of buffers
    /// recovered.
    pub fn apply_hot_exit_recovery(&mut self) -> anyhow::Result<usize> {
        if !self.config.editor.hot_exit {
            return Ok(0);
        }

        let entries = self.recovery_service.lock().unwrap().list_recoverable()?;
        if entries.is_empty() {
            return Ok(0);
        }

        // Collect buffer IDs and their file paths
        let buffer_files: Vec<_> = self
            .buffers()
            .iter()
            .filter_map(|(buffer_id, state)| {
                let path = state.buffer.file_path()?.to_path_buf();
                if path.as_os_str().is_empty() {
                    return None; // Skip unnamed buffers
                }
                Some((*buffer_id, path))
            })
            .collect();

        let mut recovered = 0;
        for (buffer_id, file_path) in buffer_files {
            let recovery_id = self
                .recovery_service
                .lock()
                .unwrap()
                .get_buffer_id(Some(&file_path));
            let entry = entries.iter().find(|e| e.id == recovery_id);
            if let Some(entry) = entry {
                let loaded = self.recovery_service.lock().unwrap().load_recovery(entry);
                match loaded {
                    Ok(crate::services::recovery::RecoveryResult::Recovered {
                        content, ..
                    }) => {
                        let mut mutated = false;
                        if let Some(state) = self
                            .windows
                            .get_mut(&self.active_window)
                            .map(|w| &mut w.buffers)
                            .expect("active window present")
                            .get_mut(&buffer_id)
                        {
                            let current_len = state.buffer.total_bytes();
                            let text = String::from_utf8_lossy(&content).into_owned();
                            let current = state.buffer.get_text_range_mut(0, current_len).ok();
                            let current_text = current
                                .as_ref()
                                .map(|b| String::from_utf8_lossy(b).into_owned());
                            if current_text.as_deref() != Some(&text) {
                                state.buffer.delete(0..current_len);
                                state.buffer.insert(0, &text);
                                state.buffer.set_modified(true);
                                state.buffer.set_recovery_pending(false);
                                // Invalidate saved position so undo can't
                                // incorrectly clear the modified flag
                                if let Some(log) =
                                    self.active_window_mut().event_logs.get_mut(&buffer_id)
                                {
                                    log.clear_saved_position();
                                }
                                mutated = true;
                                recovered += 1;
                                tracing::info!(
                                    "Restored unsaved changes for {:?} from hot exit recovery",
                                    file_path
                                );
                            }
                        }
                        if mutated {
                            self.sync_lsp_after_recovery_replay(buffer_id);
                        }
                    }
                    Ok(crate::services::recovery::RecoveryResult::RecoveredChunks {
                        chunks,
                        ..
                    }) => {
                        let mut mutated = false;
                        if let Some(state) = self
                            .windows
                            .get_mut(&self.active_window)
                            .map(|w| &mut w.buffers)
                            .expect("active window present")
                            .get_mut(&buffer_id)
                        {
                            for chunk in chunks.into_iter().rev() {
                                let text = String::from_utf8_lossy(&chunk.content).into_owned();
                                if chunk.original_len > 0 {
                                    state
                                        .buffer
                                        .delete(chunk.offset..chunk.offset + chunk.original_len);
                                }
                                state.buffer.insert(chunk.offset, &text);
                            }
                            state.buffer.set_modified(true);
                            state.buffer.set_recovery_pending(false);
                            // Invalidate saved position so undo can't
                            // incorrectly clear the modified flag
                            if let Some(log) =
                                self.active_window_mut().event_logs.get_mut(&buffer_id)
                            {
                                log.clear_saved_position();
                            }
                            mutated = true;
                            recovered += 1;
                            tracing::info!(
                                "Restored unsaved changes (chunked) for {:?} from hot exit recovery",
                                file_path
                            );
                        }
                        if mutated {
                            self.sync_lsp_after_recovery_replay(buffer_id);
                        }
                    }
                    Ok(crate::services::recovery::RecoveryResult::OriginalFileModified {
                        original_path,
                        ..
                    }) => {
                        let name = original_path
                            .file_name()
                            .unwrap_or_default()
                            .to_string_lossy();
                        tracing::warn!("{} changed on disk; unsaved changes not restored", name);
                        self.set_status_message(format!(
                            "{} changed on disk; unsaved changes not restored",
                            name
                        ));
                    }
                    Ok(_) => {} // Corrupted, NotFound - skip
                    Err(e) => {
                        tracing::debug!(
                            "Failed to load hot exit recovery for {:?}: {}",
                            file_path,
                            e
                        );
                    }
                }
            }
        }

        Ok(recovered)
    }

    /// Apply only the **editor-global** config overrides from a
    /// workspace (the global `Config`). The window-local override
    /// (`mouse_enabled`) is applied by
    /// `Window::apply_workspace_layout`.
    fn restore_config_overrides(&mut self, overrides: &WorkspaceConfigOverrides) {
        if let Some(line_numbers) = overrides.line_numbers {
            self.config_mut().editor.line_numbers = line_numbers;
        }
        if let Some(relative_line_numbers) = overrides.relative_line_numbers {
            self.config_mut().editor.relative_line_numbers = relative_line_numbers;
        }
        if let Some(line_wrap) = overrides.line_wrap {
            self.config_mut().editor.line_wrap = line_wrap;
        }
        if let Some(syntax_highlighting) = overrides.syntax_highlighting {
            self.config_mut().editor.syntax_highlighting = syntax_highlighting;
        }
        if let Some(enable_inlay_hints) = overrides.enable_inlay_hints {
            self.config_mut().editor.enable_inlay_hints = enable_inlay_hints;
        }
        // `overrides.menu_bar_hidden` is a legacy field — kept for serde
        // compatibility with workspaces written by older builds, but no
        // longer applied: menu bar visibility is now a global preference.
        // See issue #1156.
    }

    /// Save a specific window's workspace to disk, keyed by its own
    /// `root`. No active-window flip: reads `windows[id]` directly,
    /// snapshots via `Window::capture_workspace`, and injects the
    /// editor-global `plugin_global_state`.
    pub fn save_workspace_for(&mut self, id: fresh_core::WindowId) -> Result<(), WorkspaceError> {
        let Some(win) = self.windows.get(&id) else {
            return Ok(());
        };

        // Ensure terminal backing files have complete state, and persist
        // per-file global states, before snapshotting.
        win.sync_terminal_backing_files();
        win.save_all_global_file_states();

        let workspace = win.capture_workspace();

        // Refuse to overwrite a non-empty on-disk workspace with an
        // all-virtual snapshot (issue #2027). The protection is for
        // FILE/unnamed content only — terminals are live runtime state, so
        // a terminal-only on-disk workspace must NOT block this save.
        if workspace.has_no_real_content() && win.has_any_virtual_buffer() {
            let root = win.root.clone();
            let on_disk = if let Some(ref session_name) = self.session_name {
                Workspace::load_session(session_name, &root).ok().flatten()
            } else {
                Workspace::load(&root).ok().flatten()
            };
            if let Some(existing) = on_disk {
                if !existing.has_no_preservable_content() {
                    tracing::info!(
                        "Skipping workspace save: only virtual buffers are open, \
                         on-disk workspace already has preservable file content"
                    );
                    return Ok(());
                }
            }
        }

        // For a named daemon, save to the daemon-scoped workspace file
        if let Some(ref session_name) = self.session_name {
            workspace.save_session(session_name)
        } else {
            workspace.save()
        }
    }

    /// Restore a specific window's workspace from disk into
    /// `windows[id]`, keyed by its own `root`. No active-window flip:
    /// the entire window-local layout AND hot-exit recovery now run on
    /// `windows[id]` via `Window::apply_workspace_layout` (the recovery
    /// service is shared into the window via `WindowResources`). Only
    /// genuinely editor-global steps are layered on here:
    /// - `restore_config_overrides` (mutates the shared `Config`),
    /// - `plugin_global_state` assignment,
    /// - and, for the active window ONLY, the post-restore plugin
    ///   snapshot + `buffer_activated` hook (background restores must not
    ///   fire focus side-effects).
    pub fn restore_workspace_for(
        &mut self,
        id: fresh_core::WindowId,
    ) -> Result<bool, WorkspaceError> {
        let Some(root) = self.windows.get(&id).map(|w| w.root.clone()) else {
            return Ok(false);
        };

        let workspace = if let Some(ref session_name) = self.session_name {
            Workspace::load_session(session_name, &root)?
        } else {
            Workspace::load(&root)?
        };
        let Some(workspace) = workspace else {
            tracing::debug!("No workspace found for {:?}", root);
            return Ok(false);
        };
        tracing::info!("Found workspace for {:?}, applying...", root);

        // Editor-global config overrides (the shared `Config`).
        self.restore_config_overrides(&workspace.config_overrides);
        // Editor-global plugin state is NOT taken from per-window
        // workspace files: it has a single canonical home in the
        // global `orchestrator/state/` store, loaded once at boot.
        // Applying a per-window copy here was what let a background
        // window's stale snapshot clobber the live editor-global state.

        let populated = self
            .windows
            .get(&id)
            .map(|w| w.buffers.splits().is_some() && !w.buffers.is_empty())
            .unwrap_or(false);

        let session = self.session_name.clone();
        if populated {
            // Normal path: editor_init has already seeded windows[id], so
            // restore the layout (incl. hot-exit recovery) into it.
            let win = self
                .windows
                .get_mut(&id)
                .expect("window present for restore");
            win.apply_workspace_layout(&workspace, session.as_deref());
            // Restore the workspace's backend spec so a dormant remote
            // workspace knows what to reconnect to (the live authority is still
            // the local placeholder until reconnect).
            win.authority_spec = workspace.authority_spec.clone();
        } else {
            // Never-seeded shell: rebuild the window from the workspace via
            // the `Window::from_workspace` factory, carrying over the shell's
            // identity fields and **moving** its owned authority across (the
            // shell is replaced, so its single-owner backend handle moves into
            // the rebuilt window — never cloned).
            let old = self
                .windows
                .remove(&id)
                .expect("window present for restore");
            let (label, root2, authority, resources, tw, th, pstate) = (
                old.label,
                old.root,
                old.authority,
                old.resources,
                old.terminal_width,
                old.terminal_height,
                old.plugin_state,
            );
            let mut built = crate::app::window::Window::from_workspace(
                id, label, root2, authority, resources, &workspace,
            );
            built.terminal_width = tw;
            built.terminal_height = th;
            built.plugin_state = pstate;
            built.authority_spec = workspace.authority_spec.clone();
            self.windows.insert(id, built);
        }

        // Active-window only: refresh the plugin snapshot and fire
        // buffer_activated for the restored active buffer. Background
        // (inactive) window restores must NOT fire these focus effects.
        if id == self.active_window {
            #[cfg(feature = "plugins")]
            {
                let buffer_id = self.active_buffer();
                self.update_plugin_state_snapshot();
                tracing::debug!(
                    "Firing buffer_activated for active buffer {:?} after workspace restore",
                    buffer_id
                );
                self.plugin_manager.read().unwrap().run_hook(
                    "buffer_activated",
                    crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
                );
            }
        }

        Ok(true)
    }

    /// Save workspaces for every window whose split layout is populated.
    /// Each window's workspace is keyed by its own `root`.
    ///
    /// Returns the first error encountered, if any; logs and continues
    /// past per-window failures so a single bad window can't block the
    /// other quits.
    pub fn save_all_windows_workspaces(&mut self) -> Result<(), WorkspaceError> {
        let targets: Vec<fresh_core::WindowId> = self
            .windows
            .iter()
            // Never overwrite a window we never materialized: it still
            // holds only its empty seed layout, while its on-disk
            // workspace is the authoritative copy. Saving the seed would
            // clobber the real file (issue: lazy restore + per-dir save).
            .filter(|(id, w)| {
                w.buffers.splits().is_some() && !self.materialize_pending.contains(id)
            })
            .map(|(id, _)| *id)
            .collect();

        let mut first_err = None;
        for id in targets {
            if let Err(e) = self.save_workspace_for(id) {
                tracing::warn!("Failed to save workspace for window {id}: {e}");
                if first_err.is_none() {
                    first_err = Some(e);
                }
            }
        }

        match first_err {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    /// Restore window `id`'s persisted workspace from disk the first
    /// time it's dived into or previewed — the lazy counterpart to the
    /// active window's eager `try_restore_workspace`. Idempotent: the
    /// id is cleared from `materialize_pending` up front, so a missing
    /// or corrupt workspace doesn't retry every frame.
    ///
    /// `plugin_global_state` is editor-wide; a background window's
    /// stale copy must not clobber the live one, so it's snapshotted
    /// and restored around the per-window restore (the active window's
    /// state, applied at startup, is the one we keep).
    pub(crate) fn materialize_window(&mut self, id: fresh_core::WindowId) {
        if !self.materialize_pending.remove(&id) {
            return;
        }
        let saved_plugin_state = self.plugin_global_state.clone();
        // Lazy counterpart to the eager startup restore: same
        // `restore_window` flow, so a dock-switched directory applies its
        // persisted explorer visibility (or the fresh-session default)
        // exactly as a cold launch would. No CLI files on this path.
        match self.restore_window(id, false) {
            Ok(true) => tracing::debug!("Materialized window {id} from workspace"),
            Ok(false) => {
                tracing::trace!("No persisted workspace for window {id}; empty seed kept")
            }
            Err(e) => tracing::warn!("Failed to materialize window {id}: {e}"),
        }
        self.plugin_global_state = saved_plugin_state;
    }

    /// Eagerly materialize every not-yet-restored window. Production
    /// startup is lazy (per-window restore on first dive/preview via
    /// `materialize_window`); this eager variant exists only for tests
    /// that need all windows populated up front — chiefly the
    /// orchestrator bring-up render tests, which assert every restored
    /// workspace paints. Not called from production code.
    pub fn materialize_all_windows(&mut self) {
        let pending: Vec<fresh_core::WindowId> = self.materialize_pending.iter().copied().collect();
        for id in pending {
            self.materialize_window(id);
        }
    }
}

impl crate::app::window::Window {
    fn restore_terminals_from_workspace(
        &mut self,
        terminals: &[SerializedTerminalWorkspace],
    ) -> HashMap<usize, BufferId> {
        let mut terminal_buffer_map: HashMap<usize, BufferId> = HashMap::new();
        if terminals.is_empty() {
            return terminal_buffer_map;
        }
        let __window_bridge = self.bridge.clone();
        self.terminal_manager.set_async_bridge(__window_bridge);
        for terminal in terminals {
            if let Some(buffer_id) = self.restore_terminal_from_workspace(terminal) {
                terminal_buffer_map.insert(terminal.terminal_index, buffer_id);
                // The terminal was live when the workspace was saved and the
                // user never explicitly exited it, so focusing it should bring
                // back a live terminal rather than the read-only scrollback
                // view. Mark it Live so `set_active_buffer` re-enters terminal
                // mode when the tab is focused (the editing-disabled completion
                // in `Editor::set_active_buffer` finishes the read-only → live
                // transition). An explicit Ctrl+Space exit later flips it back
                // to Scrollback as usual.
                self.set_terminal_interaction_mode(
                    buffer_id,
                    crate::app::window::TerminalInteractionMode::Live,
                );
            }
        }
        terminal_buffer_map
    }

    /// Re-create bookmarks from the saved workspace, resolving file paths to buffer IDs.
    fn restore_bookmarks_from_workspace(
        &mut self,
        bookmarks: &HashMap<char, SerializedBookmark>,
        path_to_buffer: &HashMap<PathBuf, BufferId>,
    ) {
        for (key, bookmark) in bookmarks {
            let Some(&buffer_id) = path_to_buffer.get(&bookmark.file_path) else {
                continue;
            };
            if let Some(buffer) = self.buffers.get(&buffer_id) {
                let pos = bookmark.position.min(buffer.buffer.len());
                self.bookmarks.set(
                    *key,
                    Bookmark {
                        buffer_id,
                        position: pos,
                    },
                );
            }
        }
    }

    /// Drop the initial empty unnamed buffer if it is no longer referenced by any
    /// split after the workspace has been applied.
    fn clean_orphaned_buffers(&mut self) {
        let referenced: HashSet<BufferId> = self
            .buffers
            .splits()
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .values()
            .flat_map(|vs| vs.buffer_tab_ids())
            .collect();
        let orphans: Vec<BufferId> = self
            .buffers
            .iter()
            .filter(|(id, state)| {
                !referenced.contains(id)
                    && state.buffer.file_path().is_none()
                    && !state.buffer.is_modified()
            })
            .map(|(id, _)| *id)
            .collect();
        for id in orphans {
            tracing::debug!("Removing orphaned empty unnamed buffer {:?}", id);
            self.buffers.remove(&id);
            self.event_logs.remove(&id);
            self.buffer_metadata.remove(&id);
        }
    }

    /// Set a status-bar message summarising how many buffers were restored and from
    /// which daemon, then emit a debug log with split/buffer counts.
    fn log_restore_summary(&mut self, session_name: Option<&str>) {
        tracing::debug!(
            "Workspace restore complete: {} splits, {} buffers",
            self.buffers
                .splits()
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .len(),
            self.buffers.len()
        );
        let restored_count = self.buffers.count_where(|id, _| {
            self.buffer_metadata
                .get(&id)
                .is_some_and(|m| !m.hidden_from_tabs && !m.is_virtual())
        });
        if restored_count == 0 {
            return;
        }
        let msg = match session_name.map(|n| format!("session '{}'", n)) {
            Some(label) => format!("Restored {} ({} buffer(s))", label, restored_count),
            None => format!(
                "Restored {} buffer(s) from previous session",
                restored_count
            ),
        };
        self.set_status_message(msg);
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
        let terminals_root = self
            .resources
            .dir_context
            .terminal_dir_for(self.root.as_path());
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

        // Best-effort directory creation for terminal backing files
        #[allow(clippy::let_underscore_must_use)]
        let _ = crate::app::terminal::terminal_backing_fs().create_dir_all(
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

        // Decide what to run in the restored terminal:
        //  1. an agent-resume argv (rejoin the conversation), when present
        //     and resume is enabled — `claude --resume <id>` / `--continue`;
        //  2. else the launch command (re-run the agent / shell);
        //  3. else the configured shell.
        // The resume argv runs as the PTY child through the authority's
        // wrapper, exactly like a launch command (mirrors
        // `spawn_terminal_session`).
        let resume_argv = terminal
            .agent_resume
            .as_ref()
            .map(|r| &r.argv)
            .filter(|argv| !argv.is_empty() && self.resources.config.terminal.resume_agents);
        let spawn_argv =
            resume_argv.or_else(|| terminal.command.as_ref().filter(|argv| !argv.is_empty()));
        // Run the resume/launch argv through the workspace's backend (local →
        // directly; container → `docker exec … <argv>`) so a restored agent
        // rejoins *inside* its backend, not on the host. For a dormant remote
        // workspace the live authority is still the local placeholder until
        // reconnect — `terminal_command` composes with whatever backend is
        // live, so the reconnect-on-activate step re-runs it in the real one.
        let wrapper_for_spawn = match spawn_argv {
            Some(argv) => self.authority().terminal_command(argv),
            None => self.resolved_terminal_wrapper(),
        };
        let wrapper_for_spawn = self.apply_remote_terminal_env(wrapper_for_spawn);
        let env_delta = self.terminal_env_delta(&wrapper_for_spawn);
        let terminal_id = match self.terminal_manager.spawn(
            terminal.cols,
            terminal.rows,
            terminal.cwd.clone(),
            Some(log_path.clone()),
            Some(backing_path.clone()),
            wrapper_for_spawn,
            env_delta,
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

        // Carry the restore markers forward (even the empty-vec plain-shell
        // marker) so a later save re-persists them and the workspace keeps
        // restoring — and resuming — across multiple restarts.
        if let Some(argv) = terminal.command.as_ref() {
            self.terminal_commands.insert(terminal_id, argv.clone());
        }
        if let Some(resume) = terminal.agent_resume.as_ref() {
            if !resume.argv.is_empty() {
                self.terminal_resume_commands
                    .insert(terminal_id, resume.argv.clone());
            }
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

        let large_file_threshold = self.resources.config.editor.large_file_threshold_bytes as usize;
        if let Ok(new_state) = EditorState::from_file_with_languages(
            backing_path,
            self.terminal_width,
            self.terminal_height,
            large_file_threshold,
            &self.resources.grammar_registry,
            &self.resources.config.languages,
            crate::app::terminal::terminal_backing_fs(),
        ) {
            self.install_terminal_buffer_state(buffer_id, new_state);
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
        self.open_file_no_focus(path).map_err(WorkspaceError::Io)
    }

    /// Recursively restore the split layout from a serialized tree
    #[allow(clippy::too_many_arguments)]
    fn restore_split_node(
        &mut self,
        node: &SerializedSplitNode,
        path_to_buffer: &HashMap<PathBuf, BufferId>,
        terminal_buffers: &HashMap<usize, BufferId>,
        unnamed_buffers: &HashMap<String, BufferId>,
        split_states: &HashMap<usize, SerializedSplitViewState>,
        split_id_map: &mut HashMap<usize, SplitId>,
        is_first_leaf: bool,
    ) {
        match node {
            SerializedSplitNode::Leaf {
                file_path,
                split_id,
                label,
                unnamed_recovery_id,
                role,
            } => {
                // Get the buffer for this leaf: file path, unnamed recovery ID, or default
                let buffer_id = file_path
                    .as_ref()
                    .and_then(|p| path_to_buffer.get(p).copied())
                    .or_else(|| {
                        unnamed_recovery_id
                            .as_ref()
                            .and_then(|id| unnamed_buffers.get(id).copied())
                    })
                    .unwrap_or(self.active_buffer());

                let current_leaf_id = if is_first_leaf {
                    // First leaf reuses the existing split
                    let leaf_id = self
                        .buffers
                        .splits()
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .active_split();
                    self.set_pane_buffer(leaf_id, buffer_id);
                    leaf_id
                } else {
                    // Non-first leaves use the active split (created by split_active)
                    self.buffers
                        .splits()
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .active_split()
                };

                // Map old split ID to new one
                split_id_map.insert(*split_id, current_leaf_id.into());

                // Restore label if present
                if let Some(label) = label {
                    self.buffers
                        .split_manager_mut()
                        .expect("active window must have a populated split layout")
                        .set_label(current_leaf_id, label.clone());
                }

                // Restore role tag if present (clearing any prior holder
                // first to preserve the at-most-one-leaf-per-role invariant).
                if let Some(role) = role {
                    self.buffers
                        .split_manager_mut()
                        .expect("active window must have a populated split layout")
                        .clear_role(*role);
                    self.buffers
                        .split_manager_mut()
                        .expect("active window must have a populated split layout")
                        .set_leaf_role(current_leaf_id, Some(*role));
                }

                // Restore the view state for this split
                self.restore_split_view_state(
                    current_leaf_id,
                    *split_id,
                    split_states,
                    path_to_buffer,
                    terminal_buffers,
                    unnamed_buffers,
                );
            }
            SerializedSplitNode::Terminal {
                terminal_index,
                split_id,
                label,
                role,
            } => {
                let buffer_id = terminal_buffers
                    .get(terminal_index)
                    .copied()
                    .unwrap_or(self.active_buffer());

                let current_leaf_id = if is_first_leaf {
                    let leaf_id = self
                        .buffers
                        .splits()
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .active_split();
                    self.set_pane_buffer(leaf_id, buffer_id);
                    leaf_id
                } else {
                    self.buffers
                        .splits()
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .active_split()
                };

                split_id_map.insert(*split_id, current_leaf_id.into());

                // Restore label if present
                if let Some(label) = label {
                    self.buffers
                        .split_manager_mut()
                        .expect("active window must have a populated split layout")
                        .set_label(current_leaf_id, label.clone());
                }

                // Restore role tag for terminal leaves (same one-per-role
                // invariant as the file-leaf branch above).
                if let Some(role) = role {
                    self.buffers
                        .split_manager_mut()
                        .expect("active window must have a populated split layout")
                        .clear_role(*role);
                    self.buffers
                        .split_manager_mut()
                        .expect("active window must have a populated split layout")
                        .set_leaf_role(current_leaf_id, Some(*role));
                }

                self.buffers
                    .split_manager_mut()
                    .expect("active window must have a populated split layout")
                    .set_split_buffer(current_leaf_id, buffer_id);

                self.restore_split_view_state(
                    current_leaf_id,
                    *split_id,
                    split_states,
                    path_to_buffer,
                    terminal_buffers,
                    unnamed_buffers,
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
                    unnamed_buffers,
                    split_states,
                    split_id_map,
                    is_first_leaf,
                );

                // Get the buffer for the second child's first leaf
                let second_buffer_id = get_first_leaf_buffer(
                    second,
                    path_to_buffer,
                    terminal_buffers,
                    unnamed_buffers,
                )
                .unwrap_or(self.active_buffer());

                // Convert direction
                let split_direction = match direction {
                    SerializedSplitDirection::Horizontal => SplitDirection::Horizontal,
                    SerializedSplitDirection::Vertical => SplitDirection::Vertical,
                };

                // Create the split for the second child
                match self
                    .buffers
                    .split_manager_mut()
                    .expect("active window must have a populated split layout")
                    .split_active(split_direction, second_buffer_id, *ratio)
                {
                    Ok(new_leaf_id) => {
                        // Create view state for the new split
                        let mut view_state = SplitViewState::with_buffer(
                            self.terminal_width,
                            self.terminal_height,
                            second_buffer_id,
                        );
                        view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                            line_numbers: self.resources.config.editor.line_numbers,
                            highlight_current_line: self
                                .resources
                                .config
                                .editor
                                .highlight_current_line,
                            line_wrap: self.resolve_line_wrap_for_buffer(second_buffer_id),
                            wrap_indent: self.resources.config.editor.wrap_indent,
                            wrap_column: self.resolve_wrap_column_for_buffer(second_buffer_id),
                            rulers: self.resources.config.editor.rulers.clone(),
                            scroll_offset: self.resources.config.editor.scroll_offset,
                        });
                        self.buffers
                            .split_view_states_mut()
                            .expect("active window must have a populated split layout")
                            .insert(new_leaf_id, view_state);

                        // Map the container split ID (though we mainly care about leaves)
                        split_id_map.insert(*split_id, new_leaf_id.into());

                        // Recursively restore the second child (it's now in the new split)
                        self.restore_split_node(
                            second,
                            path_to_buffer,
                            terminal_buffers,
                            unnamed_buffers,
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
        current_split_id: LeafId,
        saved_split_id: usize,
        split_states: &HashMap<usize, SerializedSplitViewState>,
        path_to_buffer: &HashMap<PathBuf, BufferId>,
        terminal_buffers: &HashMap<usize, BufferId>,
        unnamed_buffers: &HashMap<String, BufferId>,
    ) {
        // Try to find the saved state for this split
        let Some(split_state) = split_states.get(&saved_split_id) else {
            return;
        };

        // Resolve the split-manager-assigned buffer before taking the
        // &mut borrow on windows so the borrow stays disjoint from
        // any subsequent reads.
        let split_buf_for_current = self
            .buffers
            .split_manager()
            .expect("active window must have a populated split layout")
            .buffer_for_split(current_split_id);
        let active_buffer_id = self
            .buffers
            .with_all_mut(|__buffers_mut, _mgr, vs_map| {
                let view_state = vs_map.get_mut(&current_split_id)?;
                let mut active_buffer_id: Option<BufferId> = None;
                if !split_state.open_tabs.is_empty() {
                    // Clear pre-existing open_buffers (e.g. the initial empty buffer
                    // created at startup) so only the saved tabs appear.
                    view_state.open_buffers.clear();

                    for tab in &split_state.open_tabs {
                        match tab {
                            SerializedTabRef::File(rel_path) => {
                                if let Some(&buffer_id) = path_to_buffer.get(rel_path) {
                                    if !view_state.has_buffer(buffer_id) {
                                        view_state.add_buffer(buffer_id);
                                    }
                                    // Ensure keyed state exists for this buffer
                                    view_state.ensure_buffer_state(buffer_id);
                                    if terminal_buffers.values().any(|&tid| tid == buffer_id) {
                                        let buf_state =
                                            view_state.buffer_state_mut(buffer_id).unwrap();
                                        buf_state.viewport.line_wrap_enabled = false;
                                        // Match the freshly-spawned terminal path: no
                                        // gutter / current-line highlight when this
                                        // tab gets entered after workspace restore.
                                        buf_state.show_line_numbers = false;
                                        buf_state.highlight_current_line = false;
                                    }
                                }
                            }
                            SerializedTabRef::Terminal(index) => {
                                if let Some(&buffer_id) = terminal_buffers.get(index) {
                                    if !view_state.has_buffer(buffer_id) {
                                        view_state.add_buffer(buffer_id);
                                    }
                                    let buf_state = view_state.ensure_buffer_state(buffer_id);
                                    buf_state.viewport.line_wrap_enabled = false;
                                    // Match the freshly-spawned terminal path: no
                                    // gutter / current-line highlight when this
                                    // tab gets entered after workspace restore.
                                    buf_state.show_line_numbers = false;
                                    buf_state.highlight_current_line = false;
                                }
                            }
                            SerializedTabRef::Unnamed(recovery_id) => {
                                if let Some(&buffer_id) = unnamed_buffers.get(recovery_id) {
                                    if !view_state.has_buffer(buffer_id) {
                                        view_state.add_buffer(buffer_id);
                                    }
                                    view_state.ensure_buffer_state(buffer_id);
                                }
                            }
                        }
                    }

                    // If all saved tabs referenced deleted/missing files, open_buffers
                    // is now empty. Re-add the buffer that the split manager assigned to
                    // this split so the orphan cleanup won't remove a buffer the split
                    // manager still points to (#1278).
                    if view_state.open_buffers.is_empty() {
                        if let Some(buf) = split_buf_for_current {
                            view_state.add_buffer(buf);
                            view_state.ensure_buffer_state(buf);
                        }
                    }

                    if let Some(active_idx) = split_state.active_tab_index {
                        if let Some(tab) = split_state.open_tabs.get(active_idx) {
                            active_buffer_id = match tab {
                                SerializedTabRef::File(rel) => path_to_buffer.get(rel).copied(),
                                SerializedTabRef::Terminal(index) => {
                                    terminal_buffers.get(index).copied()
                                }
                                SerializedTabRef::Unnamed(id) => unnamed_buffers.get(id).copied(),
                            };
                        }
                    }
                } else {
                    // Backward compatibility path using open_files/active_file_index
                    for rel_path in &split_state.open_files {
                        if let Some(&buffer_id) = path_to_buffer.get(rel_path) {
                            if !view_state.has_buffer(buffer_id) {
                                view_state.add_buffer(buffer_id);
                            }
                            view_state.ensure_buffer_state(buffer_id);
                        }
                    }

                    let active_file_path =
                        split_state.open_files.get(split_state.active_file_index);
                    active_buffer_id =
                        active_file_path.and_then(|rel_path| path_to_buffer.get(rel_path).copied());
                }

                // Restore cursor, scroll, view_mode, and compose_width for ALL buffers in file_states
                for (rel_path, file_state) in &split_state.file_states {
                    // Look up buffer by path, or by unnamed recovery ID
                    let rel_str = rel_path.to_string_lossy();
                    let buffer_id = if let Some(recovery_id) = rel_str.strip_prefix("__unnamed__") {
                        match unnamed_buffers.get(recovery_id).copied() {
                            Some(id) => id,
                            None => continue,
                        }
                    } else {
                        match path_to_buffer.get(rel_path).copied() {
                            Some(id) => id,
                            None => continue,
                        }
                    };
                    let max_pos = __buffers_mut
                        .get(&buffer_id)
                        .map(|b| b.buffer.len())
                        .unwrap_or(0);

                    // Ensure keyed state exists for this buffer
                    let buf_state = view_state.ensure_buffer_state(buffer_id);

                    let cursor_pos = file_state.cursor.position.min(max_pos);
                    buf_state.cursors.primary_mut().position = cursor_pos;
                    buf_state.cursors.primary_mut().anchor =
                        file_state.cursor.anchor.map(|a| a.min(max_pos));
                    buf_state.cursors.primary_mut().sticky_column =
                        (file_state.cursor.sticky_column != 0)
                            .then_some(file_state.cursor.sticky_column);

                    buf_state.viewport.top_byte = file_state.scroll.top_byte.min(max_pos);
                    buf_state.viewport.top_view_line_offset =
                        file_state.scroll.top_view_line_offset;
                    buf_state.viewport.left_column = file_state.scroll.left_column;
                    buf_state.viewport.set_skip_resize_sync();

                    // Saved cursor and saved viewport are independent fields; if they
                    // were already out of sync at save time (cursor moved off-screen
                    // before the user closed) the restore re-creates an off-screen
                    // cursor that arrow keys can't escape (the wrap-mode early return
                    // in `viewport.rs::ensure_visible` no-ops for any cursor whose
                    // byte position is `>= viewport.top_byte`). Reconcile so the
                    // restored view always shows the cursor (#1689 follow-up).
                    if let Some(state) = __buffers_mut.get_mut(&buffer_id) {
                        super::navigation::reconcile_restored_buffer_view(
                            buf_state,
                            &mut state.buffer,
                        );

                        // Refresh the buffer's cached primary cursor line number.
                        // The cursor-position fields above are written directly
                        // (no MoveCursor event), so without this the cache stays
                        // at EditorState::new's default `Absolute(0)`. Status bar
                        // and plugin-side `getCursorLine` both read this cache —
                        // a Git Blame invoked right after restore would see 0 and
                        // land on Ln 1 even though the cursor is at line 5000.
                        let line = state
                            .buffer
                            .offset_to_position(cursor_pos)
                            .map(|p| p.line)
                            .unwrap_or(0);
                        state.primary_cursor_line_number =
                            crate::model::buffer::LineNumber::Absolute(line);
                    }

                    // Restore per-buffer view mode and compose width
                    buf_state.view_mode = match file_state.view_mode {
                        SerializedViewMode::Source => ViewMode::Source,
                        SerializedViewMode::PageView => ViewMode::PageView,
                    };
                    buf_state.compose_width = file_state.compose_width;
                    // Re-apply explicit per-buffer view overrides (line numbers /
                    // line wrap). Only Some(_) values were persisted, so buffers
                    // the user never pinned keep following the global default.
                    if let Some(line_numbers) = file_state.line_numbers {
                        buf_state.line_numbers_override = Some(line_numbers);
                        buf_state.show_line_numbers = line_numbers;
                    }
                    if let Some(line_wrap) = file_state.line_wrap {
                        buf_state.line_wrap_override = Some(line_wrap);
                        buf_state.viewport.line_wrap_enabled = line_wrap;
                    }
                    buf_state.plugin_state = file_state.plugin_state.clone();
                    if let Some(state) = __buffers_mut.get_mut(&buffer_id) {
                        buf_state.folds.clear(&mut state.marker_list);
                        for fold in &file_state.folds {
                            // Resolve the stored line numbers against the current
                            // buffer content. If a header_text was recorded (issue
                            // #1568), validate — and if necessary relocate — the
                            // fold so it lands on the line it was actually meant
                            // for, even after an external edit shifted line
                            // numbers.
                            let Some(resolved_header) = resolve_fold_header_line(
                                &state.buffer,
                                fold.header_line,
                                fold.header_text.as_deref(),
                            ) else {
                                tracing::debug!(
                                    "Dropping stale fold: header_line={} no longer matches stored \
                             header_text after external edit",
                                    fold.header_line,
                                );
                                continue;
                            };

                            // Adjust end_line by the same shift we applied to the header.
                            let shift = resolved_header as i64 - fold.header_line as i64;
                            let adjusted_end = (fold.end_line as i64 + shift).max(0) as usize;
                            let start_line = resolved_header.saturating_add(1);
                            let end_line = adjusted_end;
                            if start_line > end_line {
                                continue;
                            }
                            let Some(start_byte) = state.buffer.line_start_offset(start_line)
                            else {
                                continue;
                            };
                            let end_byte = state
                                .buffer
                                .line_start_offset(end_line.saturating_add(1))
                                .unwrap_or_else(|| state.buffer.len());
                            buf_state.folds.add(
                                &mut state.marker_list,
                                start_byte,
                                end_byte,
                                fold.placeholder.clone(),
                            );
                        }
                    }

                    tracing::trace!(
                        "Restored keyed state for {:?}: cursor={}, top_byte={}, view_mode={:?}",
                        rel_path,
                        cursor_pos,
                        buf_state.viewport.top_byte,
                        buf_state.view_mode,
                    );
                }

                // Pane-buffer invariant repair (issue #1939): the leaf must end
                // up pointing at a buffer that is one of its restored tabs. If
                // the saved active tab couldn't be resolved — e.g. it referenced
                // an empty `[No Name]` buffer that was never persisted to
                // recovery, or a terminal that failed to respawn —
                // `active_buffer_id` is still `None` here. Leaving it `None`
                // means the leaf keeps pointing at the throwaway seed buffer set
                // by `restore_split_node` (`set_pane_buffer(.., active_buffer())`),
                // which is absent from `open_buffers`. `clean_orphaned_buffers`
                // then removes that seed, leaving the split-manager leaf dangling
                // at a dead `BufferId` — the render path paints it blank while
                // `effective_active_pair` falls back elsewhere for the status
                // bar. Fall back to the first surviving tab so the tree, the
                // view state, and the tab list all agree. (When `open_buffers`
                // is empty the #1278 re-add above already seeded it with the
                // leaf's own buffer, so this keeps that buffer instead.)
                if active_buffer_id.is_none() {
                    active_buffer_id = view_state.buffer_tab_ids().next();
                }

                // For buffers without saved file_state (e.g., terminals), apply split-level
                // view_mode/compose_width as fallback (backward compatibility)
                let restored_view_mode = match split_state.view_mode {
                    SerializedViewMode::Source => ViewMode::Source,
                    SerializedViewMode::PageView => ViewMode::PageView,
                };

                if let Some(active_buf_id) = active_buffer_id {
                    // Switch the split to the active buffer
                    view_state.switch_buffer(active_buf_id);

                    // If no per-buffer file_state was saved, apply split-level settings
                    let active_has_file_state = split_state.file_states.keys().any(|rel_path| {
                        path_to_buffer.get(rel_path).copied() == Some(active_buf_id)
                    });
                    if !active_has_file_state {
                        view_state.active_state_mut().view_mode = restored_view_mode.clone();
                        view_state.active_state_mut().compose_width = split_state.compose_width;
                    }

                    // Cursors now live in SplitViewState, no need to sync to EditorState
                }
                view_state.tab_scroll_offset = split_state.tab_scroll_offset;
                active_buffer_id
            })
            .flatten();

        // Set this buffer as active in the split (fires buffer_activated
        // hook). Done after the view_state borrow ends so we can take a
        // second &mut borrow on self.windows for the split manager.
        if let Some(active_buf_id) = active_buffer_id {
            self.buffers
                .split_manager_mut()
                .expect("active window must have a populated split layout")
                .set_split_buffer(current_split_id, active_buf_id);
        }
    }

    fn restore_search_options(&mut self, opts: &SearchOptions) {
        self.search_case_sensitive = opts.case_sensitive;
        self.search_whole_word = opts.whole_word;
        self.search_use_regex = opts.use_regex;
        self.search_confirm_each = opts.confirm_each;
    }

    fn restore_prompt_histories(&mut self, histories: &WorkspaceHistories) {
        tracing::debug!(
            "Restoring histories: {} search, {} replace, {} goto_line",
            histories.search.len(),
            histories.replace.len(),
            histories.goto_line.len()
        );
        for item in &histories.search {
            self.prompt_histories
                .entry("search".to_string())
                .or_default()
                .push(item.clone());
        }
        for item in &histories.replace {
            self.prompt_histories
                .entry("replace".to_string())
                .or_default()
                .push(item.clone());
        }
        for item in &histories.goto_line {
            self.prompt_histories
                .entry("goto_line".to_string())
                .or_default()
                .push(item.clone());
        }
    }

    fn restore_file_explorer_settings(&mut self, fe: &FileExplorerState) {
        self.file_explorer_visible = fe.visible;
        self.file_explorer_width = fe.width;
        self.file_explorer_side = fe.side;

        // Store pending settings (fixes #569); applied when explorer initialises (async).
        if fe.show_hidden {
            self.pending_file_explorer_show_hidden = Some(true);
        }
        if fe.show_gitignored {
            self.pending_file_explorer_show_gitignored = Some(true);
        }

        // Keep key_context as Normal so the editor (not the explorer) has focus.
        if self.file_explorer_visible && self.file_explorer.is_none() {
            self.init_file_explorer();
        }
    }

    /// The fresh-session file-explorer default: show the tree for a
    /// brand-new directory. This is the single rule shared by every way
    /// of entering a directory — startup restore, the orchestrator
    /// dock/preview materialization, and the new-window (`fresh <dir>`
    /// into a running instance) path — all of which funnel their restore
    /// through [`Editor::restore_window`].
    ///
    /// It fires *only* when nothing was restored: a restored workspace
    /// already carries the explorer's persisted visibility (applied by
    /// [`Window::restore_file_explorer_settings`]), so re-showing here
    /// would reopen a deliberately-closed explorer on every relaunch.
    /// `opened_files` suppresses it when the launch opened specific files
    /// rather than a bare directory. Visibility only — `key_context`
    /// stays Normal so the buffer keeps focus, mirroring
    /// `restore_file_explorer_settings`.
    pub(crate) fn apply_fresh_session_explorer_default(
        &mut self,
        opened_files: bool,
        workspace_restored: bool,
    ) {
        if opened_files || workspace_restored {
            return;
        }
        self.file_explorer_visible = true;
        if self.file_explorer.is_none() {
            self.init_file_explorer();
        }
    }

    /// Open every file referenced by the saved split states, returning a map
    /// from relative (or absolute) path to the new `BufferId`.
    fn open_workspace_files(
        &mut self,
        split_states: &HashMap<usize, SerializedSplitViewState>,
    ) -> HashMap<PathBuf, BufferId> {
        let file_paths = collect_file_paths_from_states(split_states);
        tracing::debug!(
            "Workspace has {} files to restore: {:?}",
            file_paths.len(),
            file_paths
        );
        let mut path_to_buffer: HashMap<PathBuf, BufferId> = HashMap::new();
        for rel_path in file_paths {
            let abs_path = self.root.join(&rel_path);
            tracing::trace!(
                "Checking file: {:?} (exists: {})",
                abs_path,
                abs_path.exists()
            );
            if abs_path.exists() {
                match self.open_file_internal(&abs_path) {
                    Ok(buffer_id) => {
                        tracing::debug!("Opened file {:?} as buffer {:?}", rel_path, buffer_id);
                        path_to_buffer.insert(rel_path, buffer_id);
                    }
                    Err(e) => tracing::warn!("Failed to open file {:?}: {}", abs_path, e),
                }
            } else {
                tracing::debug!("Skipping non-existent file: {:?}", abs_path);
            }
        }
        tracing::debug!("Opened {} files from workspace", path_to_buffer.len());
        path_to_buffer
    }

    /// Restore files that live outside the working directory (stored as absolute paths).
    fn restore_external_files(
        &mut self,
        external_files: &[PathBuf],
        path_to_buffer: &mut HashMap<PathBuf, BufferId>,
    ) {
        if external_files.is_empty() {
            return;
        }
        tracing::debug!(
            "Restoring {} external files: {:?}",
            external_files.len(),
            external_files
        );
        for abs_path in external_files {
            if !abs_path.exists() {
                tracing::debug!("Skipping non-existent external file: {:?}", abs_path);
                continue;
            }
            match self.open_file_internal(abs_path) {
                Ok(buffer_id) => {
                    path_to_buffer.insert(abs_path.clone(), buffer_id);
                    tracing::debug!(
                        "Restored external file {:?} as buffer {:?}",
                        abs_path,
                        buffer_id
                    );
                }
                Err(e) => tracing::warn!("Failed to restore external file {:?}: {}", abs_path, e),
            }
        }
    }

    /// Re-apply read-only flags for files that were locked in the saved workspace.
    /// Paths may be relative (under this window's `root`) or absolute.
    fn apply_read_only_flags(
        &mut self,
        read_only_files: &[PathBuf],
        path_to_buffer: &HashMap<PathBuf, BufferId>,
    ) {
        for ro_path in read_only_files {
            let buffer_id = path_to_buffer
                .get(ro_path)
                .copied()
                .or_else(|| path_to_buffer.get(&self.root.join(ro_path)).copied());
            if let Some(id) = buffer_id {
                self.mark_buffer_read_only(id, true);
            }
        }
    }

    /// True when this window has any virtual buffer (Dashboard, plugin
    /// scratch buffers, etc.) — used by the save path to detect the
    /// Dashboard-only-quit case where the serializer produces an empty
    /// snapshot.
    pub(crate) fn has_any_virtual_buffer(&self) -> bool {
        self.buffer_metadata
            .values()
            .any(|m| matches!(m.kind, crate::app::types::BufferKind::Virtual { .. }))
    }

    /// Persist per-file global state (cursor/scroll) for every file
    /// buffer in this window's splits.
    pub(crate) fn save_all_global_file_states(&self) {
        for (leaf_id, view_state) in self
            .buffers
            .splits()
            .map(|(_, vs)| vs)
            .expect("window must have a populated split layout")
        {
            let active_buffer = self
                .buffers
                .splits()
                .map(|(mgr, _)| mgr)
                .expect("window must have a populated split layout")
                .root()
                .get_leaves_with_rects(ratatui::layout::Rect::default())
                .into_iter()
                .find(|(sid, _, _)| *sid == *leaf_id)
                .map(|(_, buffer_id, _)| buffer_id);

            if let Some(buffer_id) = active_buffer {
                self.save_buffer_file_state(buffer_id, view_state);
            }
        }
    }

    /// Save per-file global state (cursor/scroll) for a specific buffer.
    fn save_buffer_file_state(&self, buffer_id: BufferId, view_state: &SplitViewState) {
        let abs_path = match self.buffer_metadata.get(&buffer_id) {
            Some(metadata) => match metadata.file_path() {
                Some(path) => path.to_path_buf(),
                None => return,
            },
            None => return,
        };

        let primary_cursor = view_state.cursors.primary();
        let file_state = SerializedFileState {
            cursor: SerializedCursor {
                position: primary_cursor.position,
                anchor: primary_cursor.anchor,
                sticky_column: primary_cursor.sticky_column.unwrap_or(0),
            },
            additional_cursors: view_state
                .cursors
                .iter()
                .skip(1)
                .map(|(_, cursor)| SerializedCursor {
                    position: cursor.position,
                    anchor: cursor.anchor,
                    sticky_column: cursor.sticky_column.unwrap_or(0),
                })
                .collect(),
            scroll: SerializedScroll {
                top_byte: view_state.viewport.top_byte,
                top_view_line_offset: view_state.viewport.top_view_line_offset,
                left_column: view_state.viewport.left_column,
            },
            view_mode: Default::default(),
            compose_width: None,
            // Per-buffer overrides are workspace-scoped, not part of the
            // cross-project global per-file state.
            line_numbers: None,
            line_wrap: None,
            plugin_state: std::collections::HashMap::new(),
            folds: Vec::new(),
        };

        PersistedFileWorkspace::save(&abs_path, file_state);
    }

    /// Sync this window's active terminal visible screens to their
    /// backing files (so the snapshot captures complete terminal state).
    pub(crate) fn sync_terminal_backing_files(&self) {
        use std::io::BufWriter;

        let terminals_to_sync: Vec<_> = self
            .terminal_buffers
            .values()
            .map(|tb| tb.terminal_id)
            .filter_map(|terminal_id| {
                self.terminal_backing_files
                    .get(&terminal_id)
                    .map(|path| (terminal_id, path.clone()))
            })
            .collect();

        for (terminal_id, backing_path) in terminals_to_sync {
            if let Some(handle) = self.terminal_manager.get(terminal_id) {
                if let Ok(mut state) = handle.state.lock() {
                    // Persist any scrolled-off lines not yet in the file (e.g.
                    // lines a resize spilled into history on a terminal that was
                    // never viewed before quitting) so a restored workspace keeps
                    // the full scrollback.
                    if let Ok(mut file) = crate::app::terminal::terminal_backing_fs()
                        .open_file_for_append(&backing_path)
                    {
                        let mut writer = BufWriter::new(&mut *file);
                        if let Err(e) = state.flush_new_scrollback(&mut writer) {
                            tracing::warn!(
                                "Failed to flush terminal {:?} scrollback: {}",
                                terminal_id,
                                e
                            );
                        }
                    }

                    if let Ok(mut file) = crate::app::terminal::terminal_backing_fs()
                        .open_file_for_append(&backing_path)
                    {
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

    /// Create an unnamed (unsaved) buffer in this window from recovered
    /// hot-exit content. Window-scoped, no focus side-effects — the
    /// split-layout restore wires it into a tab afterwards.
    pub(crate) fn create_unnamed_recovery_buffer(
        &mut self,
        text: &str,
        recovery_id: String,
        display_name: String,
    ) -> BufferId {
        let buffer_id = self.alloc_buffer_id();
        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.resources.config.editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.authority().filesystem),
        );
        state
            .margins
            .configure_for_line_numbers(self.resources.config.editor.line_numbers);
        state.buffer.set_default_line_ending(
            self.resources
                .config
                .editor
                .default_line_ending
                .to_line_ending(),
        );
        state.buffer.insert(0, text);
        state.buffer.set_modified(true);
        state.buffer.set_recovery_pending(false);
        self.buffers.insert(buffer_id, state);

        let mut log = crate::model::event::EventLog::new();
        log.clear_saved_position();
        self.event_logs.insert(buffer_id, log);

        let mut meta = crate::app::types::BufferMetadata::new();
        meta.recovery_id = Some(recovery_id);
        meta.display_name = display_name;
        self.buffer_metadata.insert(buffer_id, meta);

        buffer_id
    }

    /// Seed this window with the initial empty buffer + single-leaf split
    /// layout, if it doesn't already have a populated layout. Mirrors
    /// `Editor::build_fresh_layout_if_needed`, rooted on `self`.
    pub(crate) fn seed_initial_layout(&mut self) {
        if self.buffers.splits().is_some() && !self.buffers.is_empty() {
            return;
        }
        let buf = self.alloc_buffer_id();
        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.resources.config.editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.authority().filesystem),
        );
        state
            .margins
            .configure_for_line_numbers(self.resources.config.editor.line_numbers);
        state.buffer.set_default_line_ending(
            self.resources
                .config
                .editor
                .default_line_ending
                .to_line_ending(),
        );
        let manager = crate::view::split::SplitManager::new(buf);
        let active_leaf = manager.active_split();
        let mut view_states = HashMap::new();
        view_states.insert(
            active_leaf,
            SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buf),
        );
        self.buffers.set_splits((manager, view_states));
        self.buffers.insert(buf, state);
        self.buffer_metadata
            .insert(buf, crate::app::types::BufferMetadata::new());
        self.event_logs
            .insert(buf, crate::model::event::EventLog::new());
    }

    /// Push a recovered buffer's full content to this window's LSP after
    /// an out-of-band hot-exit replay (the replay edits the buffer
    /// directly, bypassing the event log's `didChange`).
    pub(crate) fn sync_lsp_after_recovery_replay(&mut self, buffer_id: BufferId) {
        let Some(text) = self
            .buffers
            .get(&buffer_id)
            .and_then(|state| state.buffer.to_string())
        else {
            return;
        };
        let full_change = lsp_types::TextDocumentContentChangeEvent {
            range: None,
            range_length: None,
            text,
        };
        self.send_lsp_changes_for_buffer(buffer_id, vec![full_change]);
    }

    /// Restore unnamed (unsaved) buffers into this window from their
    /// hot-exit recovery files (via the shared recovery service in
    /// `self.resources`). Returns a map from `recovery_id` to the new
    /// `BufferId`. No focus side-effects — the split-layout restore wires
    /// each buffer into a tab afterwards.
    fn restore_unnamed_buffers(
        &mut self,
        unnamed_buffers: &[UnnamedBufferRef],
    ) -> HashMap<String, BufferId> {
        let mut unnamed_buffer_map: HashMap<String, BufferId> = HashMap::new();
        if !self.resources.config.editor.hot_exit || unnamed_buffers.is_empty() {
            return unnamed_buffer_map;
        }
        tracing::debug!(
            "Restoring {} unnamed buffers from recovery",
            unnamed_buffers.len()
        );
        for unnamed_ref in unnamed_buffers {
            let entries = match self
                .resources
                .recovery_service
                .lock()
                .unwrap()
                .list_recoverable()
            {
                Ok(e) => e,
                Err(e) => {
                    tracing::warn!("Failed to list recovery entries: {}", e);
                    continue;
                }
            };
            let Some(entry) = entries.iter().find(|e| e.id == unnamed_ref.recovery_id) else {
                tracing::debug!(
                    "Recovery file not found for unnamed buffer {}",
                    unnamed_ref.recovery_id
                );
                continue;
            };
            let loaded = self
                .resources
                .recovery_service
                .lock()
                .unwrap()
                .load_recovery(entry);
            match loaded {
                Ok(crate::services::recovery::RecoveryResult::Recovered { content, .. }) => {
                    let text = String::from_utf8_lossy(&content).into_owned();
                    let buffer_id = self.create_unnamed_recovery_buffer(
                        &text,
                        unnamed_ref.recovery_id.clone(),
                        unnamed_ref.display_name.clone(),
                    );
                    unnamed_buffer_map.insert(unnamed_ref.recovery_id.clone(), buffer_id);
                    tracing::info!(
                        "Restored unnamed buffer '{}' (recovery_id={})",
                        unnamed_ref.display_name,
                        unnamed_ref.recovery_id
                    );
                }
                Ok(other) => {
                    tracing::warn!(
                        "Unexpected recovery result for unnamed buffer {}: {:?}",
                        unnamed_ref.recovery_id,
                        std::mem::discriminant(&other)
                    );
                }
                Err(e) => {
                    tracing::warn!(
                        "Failed to load recovery for unnamed buffer {}: {}",
                        unnamed_ref.recovery_id,
                        e
                    );
                }
            }
        }
        unnamed_buffer_map
    }

    /// Replay hot-exit recovery data onto this window's file-backed
    /// buffers that were modified when the editor last exited (via the
    /// shared recovery service in `self.resources`).
    fn restore_hot_exit_changes(&mut self, path_to_buffer: &HashMap<PathBuf, BufferId>) {
        if !self.resources.config.editor.hot_exit {
            return;
        }
        let entries = self
            .resources
            .recovery_service
            .lock()
            .unwrap()
            .list_recoverable()
            .unwrap_or_default();
        if entries.is_empty() {
            return;
        }
        let buffer_ids: Vec<BufferId> = path_to_buffer.values().copied().collect();
        for buffer_id in buffer_ids {
            let file_path = self
                .buffers
                .get(&buffer_id)
                .and_then(|s| s.buffer.file_path().map(|p| p.to_path_buf()));
            let Some(file_path) = file_path else { continue };

            let recovery_id = self
                .resources
                .recovery_service
                .lock()
                .unwrap()
                .get_buffer_id(Some(&file_path));
            let Some(entry) = entries.iter().find(|e| e.id == recovery_id) else {
                continue;
            };
            let loaded = self
                .resources
                .recovery_service
                .lock()
                .unwrap()
                .load_recovery(entry);
            match loaded {
                Ok(crate::services::recovery::RecoveryResult::Recovered { content, .. }) => {
                    let mut mutated = false;
                    if let Some(state) = self.buffers.get_mut(&buffer_id) {
                        let current_len = state.buffer.total_bytes();
                        let text = String::from_utf8_lossy(&content).into_owned();
                        let current = state.buffer.get_text_range_mut(0, current_len).ok();
                        let current_text = current
                            .as_ref()
                            .map(|b| String::from_utf8_lossy(b).into_owned());
                        if current_text.as_deref() != Some(&text) {
                            state.buffer.delete(0..current_len);
                            state.buffer.insert(0, &text);
                            state.buffer.set_modified(true);
                            state.buffer.set_recovery_pending(false);
                            mutated = true;
                            tracing::info!(
                                "Restored unsaved changes for {:?} from hot exit recovery",
                                file_path
                            );
                        }
                    }
                    if let Some(log) = self.event_logs.get_mut(&buffer_id) {
                        log.clear_saved_position();
                    }
                    if mutated {
                        self.sync_lsp_after_recovery_replay(buffer_id);
                    }
                }
                Ok(crate::services::recovery::RecoveryResult::RecoveredChunks {
                    chunks, ..
                }) => {
                    let mut mutated = false;
                    if let Some(state) = self.buffers.get_mut(&buffer_id) {
                        for chunk in chunks.into_iter().rev() {
                            let text = String::from_utf8_lossy(&chunk.content).into_owned();
                            if chunk.original_len > 0 {
                                state
                                    .buffer
                                    .delete(chunk.offset..chunk.offset + chunk.original_len);
                            }
                            state.buffer.insert(chunk.offset, &text);
                        }
                        state.buffer.set_modified(true);
                        state.buffer.set_recovery_pending(false);
                        mutated = true;
                        tracing::info!(
                            "Restored unsaved changes (chunked) for {:?} from hot exit recovery",
                            file_path
                        );
                    }
                    if let Some(log) = self.event_logs.get_mut(&buffer_id) {
                        log.clear_saved_position();
                    }
                    if mutated {
                        self.sync_lsp_after_recovery_replay(buffer_id);
                    }
                }
                Ok(crate::services::recovery::RecoveryResult::OriginalFileModified {
                    original_path,
                    ..
                }) => {
                    let name = original_path
                        .file_name()
                        .unwrap_or_default()
                        .to_string_lossy();
                    tracing::warn!("{} changed on disk; unsaved changes not restored", name);
                    self.set_status_message(format!(
                        "{} changed on disk; unsaved changes not restored",
                        name
                    ));
                }
                Ok(_) => {} // Corrupted, NotFound — skip
                Err(e) => {
                    tracing::debug!(
                        "Failed to load hot exit recovery for {:?}: {}",
                        file_path,
                        e
                    );
                }
            }
        }
    }

    /// Apply a loaded workspace's layout onto this window — now fully
    /// window-scoped: search options, prompt histories, file-explorer
    /// settings, unnamed-buffer hot-exit recovery (before the split tree,
    /// which references those buffers), the opened files
    /// (`open_file_no_focus`, no focus side-effects), external + read-only
    /// files, terminals, the split tree + per-split view state, bookmarks,
    /// orphan cleanup, the restore summary, and finally hot-exit replay
    /// onto the opened file buffers. Recovery reaches the shared service
    /// via `self.resources.recovery_service`, so no `Editor` involvement
    /// is needed.
    ///
    /// The only steps that stay on `Editor::restore_workspace_for` are the
    /// genuinely editor-global ones: config overrides beyond
    /// `mouse_enabled`, plugin global state, and the active-window plugin
    /// snapshot + `buffer_activated`.
    pub(crate) fn apply_workspace_layout(
        &mut self,
        workspace: &Workspace,
        session_name: Option<&str>,
    ) {
        tracing::debug!(
            "Applying workspace layout with {} split states",
            workspace.split_states.len()
        );

        // Window-local config override (the rest of the overrides mutate
        // the editor-global `Config` and are applied by the caller).
        if let Some(mouse_enabled) = workspace.config_overrides.mouse_enabled {
            self.mouse_enabled = mouse_enabled;
        }

        self.restore_search_options(&workspace.search_options);
        self.restore_prompt_histories(&workspace.histories);
        self.restore_file_explorer_settings(&workspace.file_explorer);

        // Unnamed-buffer recovery must precede the split layout (the tree
        // references those buffers).
        let unnamed_buffer_map = self.restore_unnamed_buffers(&workspace.unnamed_buffers);

        let mut path_to_buffer = self.open_workspace_files(&workspace.split_states);
        self.restore_external_files(&workspace.external_files, &mut path_to_buffer);
        self.apply_read_only_flags(&workspace.read_only_files, &path_to_buffer);

        let terminal_buffer_map = self.restore_terminals_from_workspace(&workspace.terminals);

        let mut split_id_map: HashMap<usize, SplitId> = HashMap::new();
        self.restore_split_node(
            &workspace.split_layout,
            &path_to_buffer,
            &terminal_buffer_map,
            &unnamed_buffer_map,
            &workspace.split_states,
            &mut split_id_map,
            true,
        );

        if let Some(&new_active_split) = split_id_map.get(&workspace.active_split_id) {
            self.buffers
                .split_manager_mut()
                .expect("window must have a populated split layout")
                .set_active_split(LeafId(new_active_split));
        }

        self.restore_bookmarks_from_workspace(&workspace.bookmarks, &path_to_buffer);
        self.clean_orphaned_buffers();
        self.log_restore_summary(session_name);

        // Replay hot-exit changes onto the file-backed buffers we opened.
        self.restore_hot_exit_changes(&path_to_buffer);
    }

    /// Build a `Window` directly from a persisted `Workspace`: construct
    /// a fresh window, seed its initial layout, then apply the workspace
    /// layout into it. The realized "restore is a Window factory" design —
    /// moving the `open_file` core and the recovery service onto `Window`
    /// removed the prior blockers that kept restore on `Editor`.
    pub(crate) fn from_workspace(
        id: fresh_core::WindowId,
        label: impl Into<String>,
        root: PathBuf,
        authority: crate::services::authority::Authority,
        resources: crate::app::window_resources::WindowResources,
        workspace: &Workspace,
    ) -> Self {
        let mut window = Self::new(id, label, root, authority, resources);
        window.seed_initial_layout();
        window.apply_workspace_layout(workspace, None);
        window
    }

    /// Snapshot THIS window's restorable state into a `Workspace`,
    /// rooted at `self.root` and reading only window-owned state +
    /// `self.resources`. The inverse of restore. `plugin_global_state`
    /// is left empty here — it is editor-global, so the `Editor` wrapper
    /// fills it in (see `Editor::capture_workspace`).
    pub(crate) fn capture_workspace(&self) -> Workspace {
        tracing::debug!("Capturing workspace for {:?}", self.root);

        let mut terminals = Vec::new();
        let mut terminal_indices: HashMap<TerminalId, usize> = HashMap::new();
        let mut seen = HashSet::new();
        for terminal_id in self.terminal_buffers.values().map(|tb| tb.terminal_id) {
            if seen.insert(terminal_id) {
                let command = self.terminal_commands.get(&terminal_id).cloned();
                // Ephemeral terminals (plugin tool UIs, agent shells) are
                // normally dropped on save. An ephemeral terminal that
                // carries a spawn command is the exception: it's an agent
                // session whose defining process we *can* reproduce, so we
                // persist a record (with the command) and re-run it on
                // restore. Commandless ephemerals (build output, exec
                // shells) stay transient.
                if self.ephemeral_terminals.contains(&terminal_id) && command.is_none() {
                    continue;
                }
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
                        let root = self.resources.dir_context.terminal_dir_for(&self.root);
                        root.join(format!("fresh-terminal-{}.log", terminal_id.0))
                    });
                let backing_path = self
                    .terminal_backing_files
                    .get(&terminal_id)
                    .cloned()
                    .unwrap_or_else(|| {
                        let root = self.resources.dir_context.terminal_dir_for(&self.root);
                        root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
                    });

                let agent_resume = self
                    .terminal_resume_commands
                    .get(&terminal_id)
                    .filter(|argv| !argv.is_empty())
                    .map(|argv| crate::workspace::AgentResume { argv: argv.clone() });
                terminals.push(SerializedTerminalWorkspace {
                    terminal_index: idx,
                    cwd,
                    shell,
                    cols,
                    rows,
                    log_path,
                    backing_path,
                    command,
                    agent_resume,
                });
            }
        }

        let (mgr, view_states) = self
            .buffers
            .splits()
            .expect("window must have a populated split layout");

        // Serialization helpers only need the buffer→PTY-id association, not
        // the interaction mode, so project the terminal-buffer map down to it.
        let terminal_id_map: HashMap<BufferId, TerminalId> = self
            .terminal_buffers
            .iter()
            .map(|(b, tb)| (*b, tb.terminal_id))
            .collect();

        let split_layout = serialize_split_node(
            mgr.root(),
            &self.buffer_metadata,
            &self.root,
            &terminal_id_map,
            &terminal_indices,
            mgr.labels(),
        );

        let active_buffers: HashMap<LeafId, BufferId> = mgr
            .root()
            .get_leaves_with_rects(ratatui::layout::Rect::default())
            .into_iter()
            .map(|(leaf_id, buffer_id, _)| (leaf_id, buffer_id))
            .collect();

        let mut split_states = HashMap::new();
        for (leaf_id, view_state) in view_states {
            let active_buffer = active_buffers.get(leaf_id).copied();
            let serialized = serialize_split_view_state(
                view_state,
                self.buffers.as_map(),
                &self.buffer_metadata,
                &self.root,
                active_buffer,
                &terminal_id_map,
                &terminal_indices,
            );
            split_states.insert(leaf_id.0 .0, serialized);
        }

        let file_explorer = if let Some(explorer) = self.file_explorer.as_ref() {
            let expanded_dirs = get_expanded_dirs(explorer, &self.root);
            FileExplorerState {
                visible: self.file_explorer_visible,
                width: self.file_explorer_width,
                side: self.file_explorer_side,
                expanded_dirs,
                scroll_offset: explorer.get_scroll_offset(),
                show_hidden: explorer.ignore_patterns().show_hidden(),
                show_gitignored: explorer.ignore_patterns().show_gitignored(),
            }
        } else {
            FileExplorerState {
                visible: self.file_explorer_visible,
                width: self.file_explorer_width,
                side: self.file_explorer_side,
                expanded_dirs: Vec::new(),
                scroll_offset: 0,
                show_hidden: false,
                show_gitignored: false,
            }
        };

        let cfg = &self.resources.config.editor;
        let config_overrides = WorkspaceConfigOverrides {
            line_numbers: Some(cfg.line_numbers),
            relative_line_numbers: Some(cfg.relative_line_numbers),
            line_wrap: Some(cfg.line_wrap),
            syntax_highlighting: Some(cfg.syntax_highlighting),
            enable_inlay_hints: Some(cfg.enable_inlay_hints),
            mouse_enabled: Some(self.mouse_enabled),
            menu_bar_hidden: None,
        };

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
            command_palette: Vec::new(),
            goto_line: self
                .prompt_histories
                .get("goto_line")
                .map(|h| h.items().to_vec())
                .unwrap_or_default(),
            open_file: Vec::new(),
        };

        let search_options = SearchOptions {
            case_sensitive: self.search_case_sensitive,
            whole_word: self.search_whole_word,
            use_regex: self.search_use_regex,
            confirm_each: self.search_confirm_each,
        };

        let bookmarks = serialize_bookmarks(&self.bookmarks, &self.buffer_metadata, &self.root);

        let external_files: Vec<PathBuf> = self
            .buffer_metadata
            .values()
            .filter(|meta| !meta.hidden_from_tabs && !meta.is_virtual())
            .filter_map(|meta| meta.file_path())
            .filter(|abs_path| abs_path.strip_prefix(&self.root).is_err())
            .cloned()
            .collect();

        let read_only_files: Vec<PathBuf> = self
            .buffer_metadata
            .values()
            .filter(|meta| !meta.hidden_from_tabs && !meta.is_virtual())
            .filter(|meta| meta.read_only)
            .filter_map(|meta| meta.file_path().cloned())
            .filter(|p| !p.as_os_str().is_empty())
            .map(|p| {
                p.strip_prefix(&self.root)
                    .map(|rel| rel.to_path_buf())
                    .unwrap_or(p)
            })
            .collect();

        let unnamed_buffers: Vec<UnnamedBufferRef> = if self.resources.config.editor.hot_exit {
            self.buffer_metadata
                .iter()
                .filter_map(|(buffer_id, meta)| {
                    let path = meta.file_path()?;
                    if !path.as_os_str().is_empty() {
                        return None;
                    }
                    if meta.hidden_from_tabs || meta.is_virtual() {
                        return None;
                    }
                    let state = self.buffers.get(buffer_id)?;
                    if state.buffer.total_bytes() == 0 {
                        return None;
                    }
                    let recovery_id = meta.recovery_id.clone()?;
                    Some(UnnamedBufferRef {
                        recovery_id,
                        display_name: meta.display_name.clone(),
                    })
                })
                .collect()
        } else {
            Vec::new()
        };

        Workspace {
            version: WORKSPACE_VERSION,
            working_dir: self.root.clone(),
            split_layout,
            active_split_id: SplitId::from(mgr.active_split()).0,
            split_states,
            config_overrides,
            file_explorer,
            histories,
            search_options,
            bookmarks,
            terminals,
            external_files,
            read_only_files,
            unnamed_buffers,
            plugin_global_state: HashMap::new(),
            saved_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            // Workspace identity (windows.json is gone — the per-dir
            // workspace file is the sole record).
            label: Some(self.label.clone()),
            session_plugin_state: self.plugin_state.clone(),
            // How to rebuild/reconnect this workspace's backend on restore.
            authority_spec: self.authority_spec.clone(),
        }
    }
}

/// Helper: Get the buffer ID from the first leaf node in a split tree
fn get_first_leaf_buffer(
    node: &SerializedSplitNode,
    path_to_buffer: &HashMap<PathBuf, BufferId>,
    terminal_buffers: &HashMap<usize, BufferId>,
    unnamed_buffers: &HashMap<String, BufferId>,
) -> Option<BufferId> {
    match node {
        SerializedSplitNode::Leaf {
            file_path,
            unnamed_recovery_id,
            ..
        } => file_path
            .as_ref()
            .and_then(|p| path_to_buffer.get(p).copied())
            .or_else(|| {
                unnamed_recovery_id
                    .as_ref()
                    .and_then(|id| unnamed_buffers.get(id).copied())
            }),
        SerializedSplitNode::Terminal { terminal_index, .. } => {
            terminal_buffers.get(terminal_index).copied()
        }
        SerializedSplitNode::Split { first, .. } => {
            get_first_leaf_buffer(first, path_to_buffer, terminal_buffers, unnamed_buffers)
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
    serialize_split_node_pruned(
        node,
        buffer_metadata,
        working_dir,
        terminal_buffers,
        terminal_indices,
        split_labels,
    )
    .unwrap_or({
        // Entire tree was virtual buffers — nothing to persist.  Fall back to
        // an empty [No Name] leaf so the restored workspace is still valid.
        SerializedSplitNode::Leaf {
            file_path: None,
            split_id: 0,
            label: None,
            unnamed_recovery_id: None,
            role: None,
        }
    })
}

/// Like `serialize_split_node` but returns `None` for subtrees that only
/// contain transient virtual buffers (e.g. `*Search/Replace*` panels).
/// Virtual buffers can't be rebuilt from disk, so persisting their split
/// would leave an empty or mis-attributed pane on restore (see bug #5).
/// When one child of a Split prunes away, the surviving child is hoisted in
/// place of the whole Split node.
fn serialize_split_node_pruned(
    node: &SplitNode,
    buffer_metadata: &HashMap<BufferId, super::types::BufferMetadata>,
    working_dir: &Path,
    terminal_buffers: &HashMap<BufferId, TerminalId>,
    terminal_indices: &HashMap<TerminalId, usize>,
    split_labels: &HashMap<SplitId, String>,
) -> Option<SerializedSplitNode> {
    match node {
        SplitNode::Grouped { layout, .. } => {
            // Grouped nodes are rebuilt by plugins on load; serialize just
            // the inner layout so the split tree structure is preserved
            // without the group wrapper.
            serialize_split_node_pruned(
                layout,
                buffer_metadata,
                working_dir,
                terminal_buffers,
                terminal_indices,
                split_labels,
            )
        }
        SplitNode::Leaf {
            buffer_id,
            split_id,
            role,
        } => {
            let raw_split_id: SplitId = (*split_id).into();
            let label = split_labels.get(&raw_split_id).cloned();
            let role = *role;

            if let Some(terminal_id) = terminal_buffers.get(buffer_id) {
                if let Some(index) = terminal_indices.get(terminal_id) {
                    return Some(SerializedSplitNode::Terminal {
                        terminal_index: *index,
                        split_id: raw_split_id.0,
                        label,
                        role,
                    });
                }
            }

            let meta = buffer_metadata.get(buffer_id);

            // Virtual buffers (e.g. the *Search/Replace* panel) have no
            // persistent identity — drop them and let the parent Split node
            // collapse to the sibling.
            if meta.map(|m| m.is_virtual()).unwrap_or(false) {
                return None;
            }

            let file_path = meta.and_then(|m| m.file_path()).and_then(|abs_path| {
                if abs_path.as_os_str().is_empty() {
                    None // unnamed buffer
                } else {
                    abs_path
                        .strip_prefix(working_dir)
                        .ok()
                        .map(|p| p.to_path_buf())
                }
            });

            // For unnamed buffers, emit their recovery ID so workspace restore
            // can load content from recovery files
            let unnamed_recovery_id = if file_path.is_none() {
                meta.and_then(|m| m.recovery_id.clone())
            } else {
                None
            };

            Some(SerializedSplitNode::Leaf {
                file_path,
                split_id: raw_split_id.0,
                label,
                unnamed_recovery_id,
                role,
            })
        }
        SplitNode::Split {
            direction,
            first,
            second,
            ratio,
            split_id,
            ..
        } => {
            let raw_split_id: SplitId = (*split_id).into();
            let first = serialize_split_node_pruned(
                first,
                buffer_metadata,
                working_dir,
                terminal_buffers,
                terminal_indices,
                split_labels,
            );
            let second = serialize_split_node_pruned(
                second,
                buffer_metadata,
                working_dir,
                terminal_buffers,
                terminal_indices,
                split_labels,
            );
            match (first, second) {
                (Some(f), Some(s)) => Some(SerializedSplitNode::Split {
                    direction: match direction {
                        SplitDirection::Horizontal => SerializedSplitDirection::Horizontal,
                        SplitDirection::Vertical => SerializedSplitDirection::Vertical,
                    },
                    first: Box::new(f),
                    second: Box::new(s),
                    ratio: *ratio,
                    split_id: raw_split_id.0,
                }),
                // One side was a virtual-buffer-only subtree — collapse to
                // the surviving sibling.
                (Some(only), None) | (None, Some(only)) => Some(only),
                (None, None) => None,
            }
        }
    }
}

fn serialize_split_view_state(
    view_state: &crate::view::split::SplitViewState,
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, super::types::BufferMetadata>,
    working_dir: &Path,
    active_buffer: Option<BufferId>,
    terminal_buffers: &HashMap<BufferId, TerminalId>,
    terminal_indices: &HashMap<TerminalId, usize>,
) -> SerializedSplitViewState {
    let mut open_tabs = Vec::new();
    let mut open_files = Vec::new();
    let mut active_tab_index = None;

    // Only serialize buffer tabs; group tabs are rebuilt by plugins on load.
    for buffer_id in view_state.buffer_tab_ids() {
        let buffer_id = &buffer_id;
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

        if let Some(meta) = buffer_metadata.get(buffer_id) {
            if let Some(abs_path) = meta.file_path() {
                if abs_path.as_os_str().is_empty() {
                    // Unnamed buffer - reference by recovery ID
                    if let Some(ref recovery_id) = meta.recovery_id {
                        open_tabs.push(SerializedTabRef::Unnamed(recovery_id.clone()));
                        if Some(*buffer_id) == active_buffer {
                            active_tab_index = Some(tab_index);
                        }
                    }
                } else if let Ok(rel_path) = abs_path.strip_prefix(working_dir) {
                    open_tabs.push(SerializedTabRef::File(rel_path.to_path_buf()));
                    open_files.push(rel_path.to_path_buf());
                    if Some(*buffer_id) == active_buffer {
                        active_tab_index = Some(tab_index);
                    }
                } else {
                    // External file (outside working_dir) - store absolute path
                    open_tabs.push(SerializedTabRef::File(abs_path.to_path_buf()));
                    if Some(*buffer_id) == active_buffer {
                        active_tab_index = Some(tab_index);
                    }
                }
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
        let Some(meta) = buffer_metadata.get(buffer_id) else {
            continue;
        };
        let Some(abs_path) = meta.file_path() else {
            continue;
        };

        // Determine the key for this buffer's state
        let state_key = if abs_path.as_os_str().is_empty() {
            // Unnamed buffer - use recovery ID as key
            if let Some(ref recovery_id) = meta.recovery_id {
                PathBuf::from(format!("__unnamed__{}", recovery_id))
            } else {
                continue;
            }
        } else if let Ok(rp) = abs_path.strip_prefix(working_dir) {
            rp.to_path_buf()
        } else {
            // External file - use absolute path as key
            abs_path.to_path_buf()
        };

        let primary_cursor = buf_state.cursors.primary();
        let folds = buffers
            .get(buffer_id)
            .map(|state| {
                buf_state
                    .folds
                    .collapsed_line_ranges(&state.buffer, &state.marker_list)
                    .into_iter()
                    .map(|range| SerializedFoldRange {
                        header_line: range.header_line,
                        end_line: range.end_line,
                        placeholder: range.placeholder,
                        header_text: range.header_text,
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        file_states.insert(
            state_key,
            SerializedFileState {
                cursor: SerializedCursor {
                    position: primary_cursor.position,
                    anchor: primary_cursor.anchor,
                    sticky_column: primary_cursor.sticky_column.unwrap_or(0),
                },
                additional_cursors: buf_state
                    .cursors
                    .iter()
                    .skip(1) // Skip primary
                    .map(|(_, cursor)| SerializedCursor {
                        position: cursor.position,
                        anchor: cursor.anchor,
                        sticky_column: cursor.sticky_column.unwrap_or(0),
                    })
                    .collect(),
                scroll: SerializedScroll {
                    top_byte: buf_state.viewport.top_byte,
                    top_view_line_offset: buf_state.viewport.top_view_line_offset,
                    left_column: buf_state.viewport.left_column,
                },
                view_mode: match buf_state.view_mode {
                    ViewMode::Source => SerializedViewMode::Source,
                    ViewMode::PageView => SerializedViewMode::PageView,
                },
                compose_width: buf_state.compose_width,
                line_numbers: buf_state.line_numbers_override,
                line_wrap: buf_state.line_wrap_override,
                plugin_state: buf_state.plugin_state.clone(),
                folds,
            },
        );
    }

    // Active buffer's view_mode/compose_width for the split-level fields (backward compat)
    let active_view_mode = active_buffer
        .and_then(|id| view_state.keyed_states.get(&id))
        .map(|bs| match bs.view_mode {
            ViewMode::Source => SerializedViewMode::Source,
            ViewMode::PageView => SerializedViewMode::PageView,
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
    bookmarks: &BookmarkState,
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
                            key,
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
