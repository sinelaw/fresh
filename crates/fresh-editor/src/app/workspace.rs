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

use rust_i18n::t;
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

    /// Enable/disable workspace persistence for this session. Set to
    /// `false` for a `--no-restore` (alias `--no-session`) run: the flag's
    /// semantics are "this session neither reads nor writes workspace
    /// state", so quit-time saves AND mid-session checkpoints (e.g. the
    /// window-switch checkpoint fired by Extract Tab to New Workspace) are
    /// all suppressed. Without a uniform gate the checkpoint path wrote the
    /// source workspace while the quit path skipped the extracted
    /// co-tenant, silently losing it on the next launch (issue #2735).
    pub fn set_workspace_persistence(&mut self, enabled: bool) {
        self.workspace_persistence_enabled = enabled;
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
    /// workspace (the global `Config`). The shared mouse-capture
    /// override (`mouse_enabled`) is applied by
    /// `Window::apply_workspace_layout`.
    fn restore_config_overrides(&mut self, overrides: &WorkspaceConfigOverrides) {
        // `line_numbers`, `line_wrap`, and `enable_inlay_hints` are legacy
        // fields — read for serde compatibility with workspaces written by
        // older builds, but no longer applied: their global toggles persist
        // to the config file, which is the single source of truth. Stamping
        // a workspace snapshot here silently overrode config edits made in
        // other projects or by hand (same treatment as `menu_bar_hidden`,
        // issue #1156).
        if let Some(relative_line_numbers) = overrides.relative_line_numbers {
            self.config_mut().editor.relative_line_numbers = relative_line_numbers;
        }
        if let Some(syntax_highlighting) = overrides.syntax_highlighting {
            self.config_mut().editor.syntax_highlighting = syntax_highlighting;
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
        // A session still descriptor-backed in `dormant_remote` never had its
        // workspace restored: its window, when present, is only the empty
        // disconnected shell a failed reconnect built. The on-disk workspace —
        // written by the last *connected* session — is authoritative; saving
        // the shell would clobber the real layout (and its terminals).
        if self.dormant_remote.contains_key(&id) {
            return Ok(());
        }
        // A workspace whose contents are still being built has nothing worth
        // recording — and it is temporarily rooted at its *project*
        // directory (its own doesn't exist yet), so writing a workspace file
        // here would file an empty layout against a directory that already
        // has a real workspace of its own. The Orchestrator persists the
        // in-flight create separately and rebuilds the row on the next
        // launch; see `PreparingWindow`.
        if self.preparing_windows.contains_key(&id) {
            return Ok(());
        }
        let Some(win) = self.windows.get(&id) else {
            return Ok(());
        };

        // Ensure terminal backing files have complete state, and persist
        // per-file global states, before snapshotting.
        win.sync_terminal_backing_files();
        win.save_all_global_file_states();

        // `--no-restore` session: never write *workspace* files. This is the
        // single funnel for every workspace save (quit-time saves, restart
        // saves, and the mid-session checkpoints that used to ignore the
        // flag), so gating here keeps them all consistent (issue #2735).
        // Deliberately below the per-file/terminal state flushes: those are
        // not workspace files and a `--no-restore` session still reads them
        // back on open, so suppressing only the writes would be asymmetric.
        if !self.workspace_persistence_enabled {
            tracing::debug!(
                "Skipping workspace save for window {id}: workspace persistence disabled (--no-restore)"
            );
            return Ok(());
        }

        let workspace = win.capture_workspace();

        // Refuse to overwrite a non-empty on-disk workspace with an
        // all-virtual snapshot (issue #2027). The protection is for
        // FILE/unnamed content only — terminals are live runtime state, so
        // a terminal-only on-disk workspace must NOT block this save.
        if workspace.has_no_real_content() && win.has_any_virtual_buffer() {
            let root = win.root.clone();
            let on_disk = Workspace::load(&root).ok().flatten();
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

        // One store, whatever launched this editor. A daemon — named or not —
        // is a host for workspaces, not an owner of a private set of them, so
        // its windows persist exactly where a direct-mode run's do and boot
        // discovery (which only ever scans this store) can see them all.
        workspace.save()
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
        let Some((root, stable_id)) = self
            .windows
            .get(&id)
            .map(|w| (w.root.clone(), w.stable_id.clone()))
        else {
            return Ok(false);
        };

        // One store, whatever launched this editor — see `save_workspace_for`.
        let workspace = if stable_id.is_empty() {
            // No durable id yet (a brand-new window): fall back to the
            // freshest file for the root.
            Workspace::load(&root)?
        } else {
            // Restore THIS window's own identity, not merely the freshest file
            // for the root — several co-tenant workspaces may share the root.
            Workspace::load_by_id(&root, &stable_id)?
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

        // Active-window only: the restored active buffer never went through a
        // focus path, so nothing has derived the terminal live/scrollback
        // state from it. A restored terminal is created live (empty scrollback
        // set), but `key_context` is still `Normal` and the buffer still
        // loads editing-disabled — so without this it comes up *focused but
        // inert*: keys don't reach the PTY and the pane shows the static
        // backing-file view instead of the live grid. With a quiet shell that
        // is invisible (the first keystroke, or output plus
        // `jump_to_end_on_output`, flips it live); with a terminal that prints
        // on its own — an agent, a `tail -f`, anything with a clock — the
        // restored pane just sits there frozen. Deriving the flags here is the
        // same move the remote-reconnect respawn makes for the same reason.
        // A restored *exited* terminal is not a terminal buffer (the exit path
        // drops the binding), so this correctly leaves it read-only.
        if id == self.active_window {
            self.sync_terminal_mode_to_active_buffer();
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

    /// Persist a single window's workspace *now*, as a crash-safety checkpoint
    /// outside the quit path.
    ///
    /// Sessions used to be written only when the editor exited cleanly
    /// (`save_all_windows_workspaces` on quit). A killed or crashed editor
    /// therefore forgot every Orchestrator session created since the last clean
    /// exit — the directory-keyed registry (`workspaces/*.json`) is *the* record
    /// of which sessions exist, and it never got the new file. Calling this at
    /// natural checkpoints — finalizing a new session's identity, and switching
    /// away from a window — keeps that on-disk registry current, so the dock
    /// remembers every open workspace even after a hard kill.
    ///
    /// Mirrors `save_all_windows_workspaces`'s guard exactly: never write a
    /// window still pending lazy materialization (it holds only an empty seed
    /// while its on-disk file is authoritative) or one without a split layout
    /// yet. Best-effort — a failed write is logged and swallowed so a checkpoint
    /// never disrupts the interactive action that triggered it.
    pub(crate) fn checkpoint_window_workspace(&mut self, id: fresh_core::WindowId) {
        let savable = self
            .windows
            .get(&id)
            .is_some_and(|w| w.buffers.splits().is_some())
            && !self.materialize_pending.contains(&id);
        if !savable {
            return;
        }
        if let Err(e) = self.save_workspace_for(id) {
            tracing::warn!("checkpoint_window_workspace: failed to save window {id}: {e}");
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
                // A restored terminal is created with an empty scrollback set,
                // so every split showing it is live by default — focusing it
                // brings back a live terminal rather than read-only scrollback.
                // The buffer loads editing-disabled (`install_terminal_buffer_state`)
                // and `complete_terminal_mode_side_effects` finishes the
                // read-only → live transition on focus. A later Ctrl+Space drops
                // that split into scrollback as usual.
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

        // A terminal that had already exited when the workspace was saved comes
        // back *dead*: its transcript is restored and the restart offer is
        // re-armed, but nothing is spawned. Respawning here would re-run a
        // process the user had already finished with — and for an agent, would
        // silently resume a conversation (and spend tokens) just because the
        // editor reopened. One click restarts it if they want it back.
        if let Some(state) = terminal.exited.as_ref() {
            return self.restore_exited_terminal(
                terminal,
                state,
                predicted_id,
                log_path,
                backing_path,
            );
        }

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
        // A terminal saved with the script grant comes back holding it: mint a
        // token bound to *this* (restored) window and stamp it into the child's
        // environment. The saved workspace records only that the grant existed
        // — the token itself belonged to the editor run that is gone.
        let extra_env = if terminal.script_access {
            self.remint_terminal_script_env(predicted_id)
        } else {
            std::collections::HashMap::new()
        };
        let terminal_id = match self.terminal_manager.spawn(
            terminal.cols,
            terminal.rows,
            terminal.cwd.clone(),
            Some(log_path.clone()),
            Some(backing_path.clone()),
            // Restore: this terminal's own saved transcript — keep streaming
            // into it so the restored buffer keeps its scrollback.
            crate::services::terminal::BackingMode::Continue,
            wrapper_for_spawn,
            env_delta,
            extra_env,
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
        self.rekey_terminal_script_token(predicted_id, terminal_id);
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
        self.apply_restored_terminal_title(buffer_id, terminal.title.as_deref());

        // Load backing file directly as read-only buffer (skip log replay)
        // The backing file already contains complete terminal state from last workspace
        self.load_terminal_backing_file_as_buffer(buffer_id, &backing_path);

        Some(buffer_id)
    }

    /// Rebuild a terminal whose process had already quit at save time: restore
    /// the transcript and re-arm the restart offer, without spawning anything.
    ///
    /// The result is byte-for-byte the state `handle_terminal_exited` leaves
    /// behind — a terminal buffer with no live binding plus an
    /// `exited_terminals` record — so the status-bar indicator, the palette
    /// command and the menu entry all work on a restored dead terminal exactly
    /// as they do on one that died in this session.
    fn restore_exited_terminal(
        &mut self,
        terminal: &SerializedTerminalWorkspace,
        state: &crate::workspace::ExitedTerminalState,
        terminal_id: crate::services::terminal::TerminalId,
        log_path: PathBuf,
        backing_path: PathBuf,
    ) -> Option<BufferId> {
        let command = terminal.command.clone().filter(|argv| !argv.is_empty());
        let resume = terminal
            .agent_resume
            .as_ref()
            .map(|r| r.argv.clone())
            .filter(|argv| !argv.is_empty());
        // Carry the launch/resume argv forward under the reserved id so a
        // *later* save re-persists them and the terminal stays restartable
        // across any number of restarts.
        if let Some(argv) = command.as_ref() {
            self.terminal_commands.insert(terminal_id, argv.clone());
        }
        if let Some(argv) = resume.as_ref() {
            self.terminal_resume_commands
                .insert(terminal_id, argv.clone());
        }

        // Build the buffer the normal way, then drop the live binding the way
        // the exit path does — the PTY this id names no longer exists.
        let buffer_id = self.create_terminal_buffer_detached(terminal_id);
        self.apply_restored_terminal_title(buffer_id, terminal.title.as_deref());
        self.load_terminal_backing_file_as_buffer(buffer_id, &backing_path);
        // A restored-dead tab shows the same "(exited)" marker a tab that died
        // in this session does — the state is identical, so it must read
        // identically.
        if let Some(meta) = self.buffer_metadata.get_mut(&buffer_id) {
            meta.display_name = t!("terminal.tab_exited", name = meta.display_name).to_string();
        }
        self.terminal_buffers.remove(&buffer_id);
        self.exited_terminals.insert(
            buffer_id,
            crate::app::window::ExitedTerminal {
                terminal_id,
                exit_code: state.exit_code,
                cols: terminal.cols,
                rows: terminal.rows,
                cwd: terminal.cwd.clone(),
                backing_path: Some(backing_path),
                log_path: Some(log_path),
                command,
                resume,
                // A restored terminal is re-persisted by the branch above on
                // the next save, so it must not be treated as throwaway.
                ephemeral: false,
                // Nothing is spawned here, so no token is minted — the grant
                // is carried so the restart (which does spawn) mints one.
                script_access: terminal.script_access,
                title: terminal.title.clone(),
            },
        );
        Some(buffer_id)
    }

    /// Re-apply a terminal tab's persisted explicit title after restore.
    ///
    /// Without this a restored agent tab falls back to foreground-process
    /// auto-naming: `bash` / `node` while it runs, and a bare `*Terminal N*`
    /// once it has exited and there is no foreground process left to read.
    /// `None` leaves the tab auto-named, which is what it was before the save.
    fn apply_restored_terminal_title(&mut self, buffer_id: BufferId, title: Option<&str>) {
        let Some(title) = title.filter(|t| !t.is_empty()) else {
            return;
        };
        if let Some(meta) = self.buffer_metadata.get_mut(&buffer_id) {
            meta.display_name = title.to_string();
        }
        // Mark it explicit so `sync_terminal_titles` leaves it alone, exactly
        // as `create_plugin_terminal` does for a freshly-launched agent.
        self.terminal_explicit_titles.insert(buffer_id);
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
                    // Never re-apply persisted state to git-internal files
                    // (COMMIT_EDITMSG etc.): git regenerates them, so the
                    // saved byte offsets point into content that no longer
                    // exists. Also heals workspace files written by older
                    // builds that still carry such entries (#2761).
                    if crate::workspace::is_git_internal_path(rel_path) {
                        continue;
                    }
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

                    buf_state
                        .viewport
                        .set_top_byte(file_state.scroll.top_byte.min(max_pos));
                    buf_state
                        .viewport
                        .set_top_view_line_offset(file_state.scroll.top_view_line_offset);
                    buf_state.viewport.left_column = file_state.scroll.left_column;
                    buf_state.viewport.set_skip_resize_sync();

                    // Saved cursor and saved viewport are independent fields; if they
                    // were already out of sync at save time (cursor moved off-screen
                    // before the user closed) the restore re-creates an off-screen
                    // cursor that arrow keys can't escape (the wrap-mode early return
                    // in `viewport.rs::ensure_visible` no-ops for any cursor whose
                    // byte position is `>= viewport.top_byte()`). Reconcile so the
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
                    if let Some(highlight_current_line) = file_state.highlight_current_line {
                        buf_state.highlight_current_line_override = Some(highlight_current_line);
                        buf_state.highlight_current_line = highlight_current_line;
                    }
                    buf_state.plugin_state = file_state.plugin_state.clone();
                    if let Some(state) = __buffers_mut.get_mut(&buffer_id) {
                        // Re-apply the explicit per-buffer virtual-space
                        // override (buffer-wide, not per-view).
                        if let Some(virtual_space) = file_state.virtual_space {
                            state.buffer_settings.virtual_space = virtual_space;
                            state.buffer_settings.virtual_space_override = Some(virtual_space);
                        }
                        // Same for the per-buffer indentation-guide and
                        // folding-indicator toggles: both are consulted at
                        // render time, so restoring the override is all it
                        // takes for the buffer to come back looking the way
                        // the user left it.
                        // Guide/fold pins are per (split, buffer): they land
                        // on this split's view state, not on the shared
                        // BufferSettings.
                        if let Some(indentation_guide) = file_state.indentation_guide {
                            buf_state.indentation_guide_user_override = Some(indentation_guide);
                        }
                        if let Some(fold_indicators) = file_state.fold_indicators {
                            buf_state.fold_indicators_override = Some(fold_indicators);
                        }
                        if let Some(use_tabs) = file_state.use_tabs {
                            state.buffer_settings.use_tabs = use_tabs;
                            state.buffer_settings.use_tabs_override = Some(use_tabs);
                        }
                        // The whitespace toggles store bools, not the
                        // resolved struct, so the visibility is re-derived from
                        // config here — that way a config edit between sessions
                        // still lands. `buffer_settings.whitespace` is still the
                        // configured value at this point (nothing has toggled
                        // it yet), so it is the right baseline to pass in.
                        if file_state.whitespace_indicators.is_some()
                            || file_state.tab_indicators.is_some()
                        {
                            let configured = state.buffer_settings.whitespace;
                            state.buffer_settings.whitespace_override =
                                file_state.whitespace_indicators;
                            state.buffer_settings.tab_indicators_override =
                                file_state.tab_indicators;
                            state.buffer_settings.apply_whitespace_override(configured);
                        }
                        if let Some(highlight_occurrences) = file_state.highlight_occurrences {
                            state.buffer_settings.highlight_occurrences_override =
                                Some(highlight_occurrences);
                            state.reference_highlight_overlay.enabled = highlight_occurrences;
                        }
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
                        buf_state.viewport.top_byte(),
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

    /// Build the `EditorState` for a restored workspace file. `content` is the
    /// file's bytes when known (a fill), or `None` for an empty placeholder (a
    /// remote restore, before its content streams in off-loop). Language and
    /// buffer config are applied either way, so a placeholder already carries
    /// the right gutter / title / language and the later fill just swaps in the
    /// text.
    pub(crate) fn build_workspace_file_state(
        &self,
        path: &Path,
        content: Option<Vec<u8>>,
    ) -> EditorState {
        let fs = std::sync::Arc::clone(&self.authority().filesystem);
        let threshold = self.resources.config.editor.large_file_threshold_bytes as usize;
        let buffer = match content {
            Some(bytes) => {
                let mut b = crate::model::buffer::Buffer::from_bytes(bytes, fs);
                b.set_file_path(path.to_path_buf());
                b
            }
            None => crate::model::buffer::Buffer::new_with_path(threshold, fs, path.to_path_buf()),
        };
        let first_line = buffer.first_line_lossy();
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                path,
                first_line.as_deref(),
                &self.resources.grammar_registry,
                &self.resources.config.languages,
                self.resources.config.default_language.as_deref(),
            );
        let mut state = EditorState::from_buffer_with_language(buffer, detected);
        state
            .margins
            .configure_for_line_numbers(self.resources.config.editor.line_numbers);
        state.apply_buffer_config(&self.resources.config);
        state
    }

    /// Install an empty placeholder buffer for `abs_path` and queue its content
    /// to be read off-loop (see [`crate::app::window::Window::pending_content_load`]).
    /// Returns the new buffer id so the split layout can reference it at once.
    fn install_remote_placeholder(&mut self, abs_path: &Path) -> BufferId {
        let buffer_id = self.alloc_buffer_id();
        let state = self.build_workspace_file_state(abs_path, None);
        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());
        self.buffer_metadata
            .insert(buffer_id, crate::app::types::BufferMetadata::new());
        self.pending_content_load
            .push((buffer_id, abs_path.to_path_buf()));
        buffer_id
    }

    /// Open every file referenced by the saved split states, returning a map
    /// from relative (or absolute) path to the new `BufferId`.
    fn open_workspace_files(
        &mut self,
        split_states: &HashMap<usize, SerializedSplitViewState>,
    ) -> HashMap<PathBuf, BufferId> {
        // Remote sessions: never *read* persisted file buffers here. These reads
        // run synchronously on the single-threaded editor loop, so over a slow /
        // high-latency link they freeze the whole editor while a dived-into
        // session materializes — the orchestrator-dock freeze. Instead restore
        // each file as an empty placeholder (no I/O) so the layout, tabs and
        // titles come up instantly; the editor then loads their content off-loop
        // and fills them in (see `Editor::drive_pending_content_loads`).
        // Terminals restore separately, unaffected.
        if self
            .authority()
            .filesystem
            .remote_connection_info()
            .is_some()
        {
            let mut path_to_buffer: HashMap<PathBuf, BufferId> = HashMap::new();
            for rel_path in collect_file_paths_from_states(split_states) {
                let abs_path = self.root.join(&rel_path);
                let buffer_id = self.install_remote_placeholder(&abs_path);
                path_to_buffer.insert(rel_path, buffer_id);
            }
            return path_to_buffer;
        }
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
        // Same rationale as `open_workspace_files`: don't read remote files on
        // the editor loop during restore. Restore each as an empty placeholder
        // and let the content load off-loop.
        if self
            .authority()
            .filesystem
            .remote_connection_info()
            .is_some()
        {
            for abs_path in external_files {
                let buffer_id = self.install_remote_placeholder(abs_path);
                path_to_buffer.insert(abs_path.clone(), buffer_id);
            }
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
                top_byte: view_state.viewport.top_byte(),
                top_view_line_offset: view_state.viewport.top_view_line_offset(),
                left_column: view_state.viewport.left_column,
            },
            view_mode: Default::default(),
            compose_width: None,
            // Per-buffer overrides are workspace-scoped, not part of the
            // cross-project global per-file state.
            line_numbers: None,
            line_wrap: None,
            virtual_space: None,
            indentation_guide: None,
            fold_indicators: None,
            use_tabs: None,
            whitespace_indicators: None,
            tab_indicators: None,
            highlight_current_line: None,
            highlight_occurrences: None,
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

        // Adopt the snapshot's durable identity: the window continues the
        // persisted workspace rather than starting a new one, so saves keep
        // landing in the same id-keyed file instead of minting a sibling on
        // every boot. A legacy snapshot without an id keeps the freshly
        // minted one — the next save re-keys the file under it.
        if let Some(id) = &workspace.stable_id {
            self.stable_id = id.clone();
        }

        // Window-local config override (the rest of the overrides mutate
        // the editor-global `Config` and are applied by the caller). Mouse
        // capture is a single global terminal property shared by every window
        // (see `Editor::mouse_capture`); restoring a persisted value updates
        // that shared flag.
        if let Some(mouse_enabled) = workspace.config_overrides.mouse_enabled {
            self.resources
                .mouse_capture
                .store(mouse_enabled, std::sync::atomic::Ordering::Relaxed);
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

        // Sanitize: a workspace written by a pre-fix build can carry a
        // `UtilityDock` role on its sole (root) leaf — the bug where closing
        // the last editor split left the dock's role tag behind (issue
        // #2415). Restoring that tag verbatim makes every later panel open
        // land as a full-window tab, permanently. Clearing it here heals
        // existing poisoned workspace files on load. A snapshot whose split
        // layout could not be rebuilt at all leaves `splits` unseeded, so
        // this stays a no-op rather than a panic.
        if let Some(splits) = self.buffers.split_manager_mut() {
            splits.clear_root_leaf_role();
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
                // Only an *explicit* title is worth persisting; auto-named
                // tabs re-derive theirs from the live process after restore.
                let title = self
                    .terminal_buffers
                    .iter()
                    .find(|(_, tb)| tb.terminal_id == terminal_id)
                    .map(|(b, _)| *b)
                    .filter(|b| self.terminal_explicit_titles.contains(b))
                    .and_then(|b| self.buffer_metadata.get(&b))
                    .map(|meta| meta.display_name.clone());
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
                    exited: None,
                    title,
                    script_access: self.terminal_has_script_access(terminal_id),
                });
            }
        }

        // Terminals whose process quit while their buffer stayed open. Without
        // this they'd be dropped on save — the buffer↔terminal binding is gone,
        // so the loop above can't see them — and a workspace reopened after
        // finishing an agent would come back missing that pane entirely.
        // Persisting them keeps both the transcript and the restart offer.
        let mut exited_buffers: Vec<(BufferId, TerminalId)> = Vec::new();
        for (buffer_id, exited) in &self.exited_terminals {
            // Same rule as live terminals: a commandless ephemeral (a plugin's
            // build output, an exec shell) stays transient.
            if exited.ephemeral && exited.command.is_none() {
                continue;
            }
            if !seen.insert(exited.terminal_id) {
                continue;
            }
            let idx = terminals.len();
            terminal_indices.insert(exited.terminal_id, idx);
            exited_buffers.push((*buffer_id, exited.terminal_id));
            terminals.push(SerializedTerminalWorkspace {
                terminal_index: idx,
                cwd: exited.cwd.clone(),
                shell: crate::services::terminal::detect_shell(),
                cols: exited.cols,
                rows: exited.rows,
                log_path: exited.log_path.clone().unwrap_or_else(|| {
                    let root = self.resources.dir_context.terminal_dir_for(&self.root);
                    root.join(format!("fresh-terminal-{}.log", exited.terminal_id.0))
                }),
                backing_path: exited.backing_path.clone().unwrap_or_else(|| {
                    let root = self.resources.dir_context.terminal_dir_for(&self.root);
                    root.join(format!("fresh-terminal-{}.txt", exited.terminal_id.0))
                }),
                command: exited.command.clone(),
                agent_resume: exited
                    .resume
                    .as_ref()
                    .filter(|argv| !argv.is_empty())
                    .map(|argv| crate::workspace::AgentResume { argv: argv.clone() }),
                exited: Some(crate::workspace::ExitedTerminalState {
                    exit_code: exited.exit_code,
                }),
                // The pre-exit tab title, not the "(exited)" form the tab is
                // showing now — restore re-applies the marker itself.
                title: exited.title.clone(),
                script_access: exited.script_access,
            });
        }

        let (mgr, view_states) = self
            .buffers
            .splits()
            .expect("window must have a populated split layout");

        // Serialization helpers only need the buffer→PTY-id association, not
        // the interaction mode, so project the terminal-buffer map down to it.
        let mut terminal_id_map: HashMap<BufferId, TerminalId> = self
            .terminal_buffers
            .iter()
            .map(|(b, tb)| (*b, tb.terminal_id))
            .collect();
        // Exited terminals have no live binding, so add theirs explicitly —
        // otherwise the split layout would serialize their panes as ordinary
        // file buffers pointing at a backing file in the data dir.
        terminal_id_map.extend(exited_buffers);

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
            // `line_numbers`, `line_wrap`, and `enable_inlay_hints` are no
            // longer snapshotted: their global toggles persist straight to the
            // config file, so a workspace copy could only ever be stale — it
            // shadowed a default the user changed elsewhere (or edited by
            // hand) every time this workspace was opened, forever, because
            // the restore stamped the stale value and the next save
            // re-serialized it. `None` here also self-heals workspaces that
            // still carry a stale value from an older build. The fields that
            // remain are the settings whose toggles are session-scoped — the
            // workspace file is their only persistence.
            line_numbers: None,
            relative_line_numbers: Some(cfg.relative_line_numbers),
            line_wrap: None,
            syntax_highlighting: Some(cfg.syntax_highlighting),
            enable_inlay_hints: None,
            mouse_enabled: Some(
                self.resources
                    .mouse_capture
                    .load(std::sync::atomic::Ordering::Relaxed),
            ),
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
            stable_id: Some(self.stable_id.clone()),
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

        // Git-internal files (COMMIT_EDITMSG, MERGE_MSG, …) are regenerated
        // with fresh content on every git operation — persisted cursor/scroll
        // state for them is always stale (#2761).
        if crate::workspace::is_git_internal_path(abs_path) {
            continue;
        }

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
                    top_byte: buf_state.viewport.top_byte(),
                    top_view_line_offset: buf_state.viewport.top_view_line_offset(),
                    left_column: buf_state.viewport.left_column,
                },
                view_mode: match buf_state.view_mode {
                    ViewMode::Source => SerializedViewMode::Source,
                    ViewMode::PageView => SerializedViewMode::PageView,
                },
                compose_width: buf_state.compose_width,
                line_numbers: buf_state.line_numbers_override,
                line_wrap: buf_state.line_wrap_override,
                virtual_space: buffers
                    .get(buffer_id)
                    .and_then(|state| state.buffer_settings.virtual_space_override),
                indentation_guide: buf_state.indentation_guide_user_override,
                fold_indicators: buf_state.fold_indicators_override,
                use_tabs: buffers
                    .get(buffer_id)
                    .and_then(|state| state.buffer_settings.use_tabs_override),
                whitespace_indicators: buffers
                    .get(buffer_id)
                    .and_then(|state| state.buffer_settings.whitespace_override),
                tab_indicators: buffers
                    .get(buffer_id)
                    .and_then(|state| state.buffer_settings.tab_indicators_override),
                highlight_current_line: buf_state.highlight_current_line_override,
                highlight_occurrences: buffers
                    .get(buffer_id)
                    .and_then(|state| state.buffer_settings.highlight_occurrences_override),
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
