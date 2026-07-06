//! Buffer-close and tab-management orchestrators on `Editor`.
//!
//! Closing a buffer in this editor is non-trivial: it involves removing
//! the buffer from the registry, cleaning up LSP state and semantic
//! tokens, deciding what to focus next via the focus-history LRU,
//! adjusting split tab lists, and (for terminal buffers) tearing down
//! the terminal manager. The whole cluster lives here.
//!
//! Also includes tab navigation (next/prev/cycle, navigate_back/forward,
//! switch_buffer) which depends on the same focus-history machinery.

use rust_i18n::t;

use crate::model::event::{BufferId, Event, LeafId};
use crate::view::prompt::PromptType;

use super::Editor;

/// Which buffer a split should show after the one being closed is removed,
/// chosen by [`Editor::resolve_close_replacement`].
struct CloseReplacement {
    /// Buffer the host split's `active_buffer` becomes.
    buffer: BufferId,
    /// `true` when no other buffer existed and a fresh empty one was created.
    created_empty: bool,
    /// Set when the LRU landing target was a buffer *group* rather than a
    /// buffer: `buffer` is then only housekeeping and the caller re-activates
    /// the group tab on this leaf.
    return_to_group: Option<LeafId>,
}

impl Editor {
    /// Close the given buffer
    pub fn close_buffer(&mut self, id: BufferId) -> anyhow::Result<()> {
        // Check for unsaved changes
        if let Some(state) = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&id)
        {
            if state.buffer.is_modified() {
                return Err(anyhow::anyhow!("Buffer has unsaved changes"));
            }
        }
        self.close_buffer_internal(id)
    }

    /// Force close the given buffer without checking for unsaved changes
    /// Use this when the user has already confirmed they want to discard changes
    pub fn force_close_buffer(&mut self, id: BufferId) -> anyhow::Result<()> {
        self.close_buffer_internal(id)
    }

    /// Internal helper to close a buffer (shared by close_buffer and force_close_buffer)
    fn close_buffer_internal(&mut self, id: BufferId) -> anyhow::Result<()> {
        // Discard any async pastes whose anchors live in this buffer:
        // when the result arrives the buffer state will be gone, and
        // there's no useful place to land the text without it. Done
        // first so the rest of close doesn't observe a transient
        // pending entry that points at a half-torn-down buffer.
        self.cancel_pending_pastes_for_buffer(id);

        // Clear preview tracking if we're closing the current preview buffer.
        // This keeps `preview` from pointing at a freed buffer id.
        if let Some((_, preview_id)) = self.active_window().preview {
            if preview_id == id {
                self.active_window_mut().preview = None;
            }
        }

        // Complete any --wait tracking for this buffer
        if let Some((wait_id, _)) = self.active_window_mut().wait_tracking.remove(&id) {
            self.active_window_mut().completed_waits.push(wait_id);
        }

        // Save file state before closing (for per-file session persistence)
        self.active_window().save_file_state_on_close(id);

        // Delete recovery data for explicitly closed buffers (including unnamed)
        if let Err(e) = self.delete_buffer_recovery(id) {
            tracing::debug!("Failed to delete buffer recovery on close: {}", e);
        }

        // If closing a terminal buffer, tear down its terminal-side state.
        // Removing the entry drops the buffer's remembered mode with it.
        if let Some(tb) = self.active_window_mut().terminal_buffers.remove(&id) {
            self.cleanup_closed_terminal(id, tb.terminal_id);
        }

        // Capture before resolving the replacement: the last-resort
        // `new_buffer()` path calls `set_active_buffer`, which would change
        // `active_buffer()` out from under this check.
        let closing_active = self.active_buffer() == id;

        // The split the replacement lands in.
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();

        let CloseReplacement {
            buffer: replacement_buffer,
            created_empty: created_empty_buffer,
            return_to_group,
        } = self.resolve_close_replacement(id, active_split);

        // Switch to replacement buffer BEFORE updating splits.
        // Only needed when the closing buffer is the one the user is
        // looking at — otherwise the current active buffer stays.
        if closing_active {
            self.set_active_buffer(replacement_buffer);

            // If we landed on a hidden panel buffer to fill the Group-case
            // housekeeping slot, scrub the *visible* side effects
            // (`open_buffers`, `focus_history`) so the panel buffer doesn't
            // appear as a tab. The `keyed_states` entry `switch_buffer`
            // inserted has to stay — `active_state()` requires
            // `active_buffer ∈ keyed_states` — but it's harmless as long as
            // the plugin-snapshot lookup skips it; see
            // `snapshot_source_split` in `update_plugin_state_snapshot`.
            let hidden = self
                .active_window()
                .buffer_metadata
                .get(&replacement_buffer)
                .is_some_and(|m| m.hidden_from_tabs);
            if return_to_group.is_some() && hidden {
                use crate::view::split::TabTarget;
                if let Some(vs) = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .get_mut(&active_split)
                {
                    vs.open_buffers
                        .retain(|t| *t != TabTarget::Buffer(replacement_buffer));
                    vs.focus_history
                        .retain(|t| *t != TabTarget::Buffer(replacement_buffer));
                }
            }
        }

        // Update all splits that are showing this buffer to show the replacement.
        // Routed through `set_pane_buffer` so the split tree and the
        // matching `SplitViewState` stay consistent — updating only the
        // tree left SVS pointing at the buffer we were about to free,
        // which caused the click panic in issue #1620.
        let splits_to_update = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .splits_for_buffer(id);
        for split_id in splits_to_update {
            self.active_window_mut()
                .set_pane_buffer(split_id, replacement_buffer);
        }

        self.purge_buffer_state(id);

        if closing_active {
            if created_empty_buffer && self.config.file_explorer.auto_open_on_last_buffer_close {
                self.focus_file_explorer();
            }
            if let Some(group_leaf) = return_to_group {
                self.activate_group_tab(active_split, group_leaf);
            }
        }

        // Notify plugins so they can reset any state tied to this buffer
        // (e.g. a plugin that owns a buffer group clears its `isOpen` flag
        // when the group is closed via the tab's close button rather than
        // through the plugin's own close command).
        self.plugin_manager.read().unwrap().run_hook(
            "buffer_closed",
            fresh_core::hooks::HookArgs::BufferClosed { buffer_id: id },
        );

        Ok(())
    }

    /// Tear down the terminal-side state for a closing terminal buffer:
    /// stop the process, drop its title / foreground-name caches, retain the
    /// searchable backing log while removing the raw one, and leave terminal
    /// mode if this was the focused terminal.
    fn cleanup_closed_terminal(
        &mut self,
        id: BufferId,
        terminal_id: crate::services::terminal::TerminalId,
    ) {
        // Close the terminal process
        self.active_window_mut().terminal_manager.close(terminal_id);
        // Drop any explicit-title marker / cached foreground name so the
        // id can't carry stale auto-naming state if a future buffer
        // reuses it.
        self.active_window_mut()
            .terminal_explicit_titles
            .remove(&id);
        self.active_window_mut().terminal_fg_cache.remove(&id);

        // Retain the rendered backing file so its scrollback stays
        // searchable after close (Universal Search "Terminals" scope).
        // Rename rather than leave in place: backing files are named
        // by terminal id, which restarts per session, so a future
        // same-id terminal would otherwise clobber this log.
        let backing_file = self
            .active_window_mut()
            .terminal_backing_files
            .remove(&terminal_id);
        if let Some(ref path) = backing_file {
            self.retain_closed_terminal_backing(path);
        }
        // Clean up raw log file
        if let Some(log_file) = self
            .active_window_mut()
            .terminal_log_files
            .remove(&terminal_id)
        {
            if backing_file.as_ref() != Some(&log_file) {
                // Best-effort cleanup of temporary terminal files.
                #[allow(clippy::let_underscore_must_use)]
                let _ = crate::app::terminal::terminal_backing_fs().remove_file(&log_file);
            }
        }

        // The buffer's remembered mode was dropped when its `terminal_buffers`
        // entry was removed by the caller — nothing else to clean up here.

        // Exit terminal mode if we were in it
        if self.active_window().terminal_mode {
            self.active_window_mut().terminal_mode = false;
            self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Normal;
        }
    }

    /// Choose which buffer the host split should show after `id` is closed.
    ///
    /// Walks `active_split`'s focus-history LRU (most recent first) for a
    /// still-valid buffer or group tab, then falls back to any visible
    /// buffer, then any buffer at all, and finally synthesizes a fresh
    /// `[No Name]` buffer — the editor must always hold at least one. This
    /// naturally handles both buffer and group tabs: whichever the user was
    /// looking at most recently wins.
    fn resolve_close_replacement(
        &mut self,
        id: BufferId,
        active_split: LeafId,
    ) -> CloseReplacement {
        let replacement_target: Option<crate::view::split::TabTarget> = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&active_split)
            .and_then(|vs| {
                use crate::view::split::TabTarget;
                vs.focus_history.iter().rev().find_map(|t| match t {
                    TabTarget::Buffer(bid) if *bid == id => None, // skip the closing buffer
                    TabTarget::Buffer(bid) => {
                        // Skip hidden-from-tabs buffers (panel helpers etc.)
                        let hidden = self
                            .active_window()
                            .buffer_metadata
                            .get(bid)
                            .map(|m| m.hidden_from_tabs)
                            .unwrap_or(false);
                        if hidden
                            || !self
                                .windows
                                .get(&self.active_window)
                                .map(|w| &w.buffers)
                                .expect("active window present")
                                .contains_key(bid)
                        {
                            None
                        } else {
                            Some(*t)
                        }
                    }
                    TabTarget::Group(leaf) => {
                        // Only if the group still exists
                        if self.active_window().grouped_subtrees.contains_key(leaf) {
                            Some(*t)
                        } else {
                            None
                        }
                    }
                })
            });

        // Any visible buffer other than the one being closed. Used as the
        // general fallback (no LRU target or LRU points at a gone group).
        let fallback_buffer: Option<BufferId> = self.buffers().find_id(|bid, _| {
            bid != id
                && !self
                    .active_window()
                    .buffer_metadata
                    .get(&bid)
                    .map(|m| m.hidden_from_tabs)
                    .unwrap_or(false)
        });

        // Pick the BufferId that becomes the host split's `active_buffer`.
        // When `return_to_group` is set, `active_buffer` is a housekeeping
        // fiction — nothing renders it — so any existing buffer works; we
        // just need to avoid synthesizing a phantom `[No Name]` when a real
        // option exists. A synthetic buffer fires only when the editor has
        // literally no other buffer left.
        let return_to_group = match replacement_target {
            Some(crate::view::split::TabTarget::Group(leaf)) => Some(leaf),
            _ => None,
        };

        let direct_replacement = match replacement_target {
            Some(crate::view::split::TabTarget::Buffer(bid)) => Some(bid),
            _ => None,
        };

        // Prefer a buffer already keyed in the host split: `switch_buffer`
        // inserts a default BufferViewState for any new active_buffer, which
        // for hidden panel buffers becomes a shadow entry (cursor=0) that
        // the plugin-state snapshot could non-deterministically prefer over
        // the panel split's authoritative copy. Picking something already
        // keyed sidesteps that insert. (We clean up after the fact if a
        // shadow does get created — see the caller.)
        let already_keyed = return_to_group.and_then(|_| {
            self.windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&active_split)?
                .keyed_states
                .keys()
                .find(|&&bid| bid != id)
                .copied()
        });

        // Absolute last-resort pool for the Group case: any buffer at all,
        // including hidden panel ones. The shadow cleanup in the caller keeps
        // those invisible.
        let any_remaining = return_to_group.and_then(|_| {
            self.windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .find_id(|bid, _| bid != id)
        });

        let (buffer, created_empty) = match direct_replacement
            .or(already_keyed)
            .or(fallback_buffer)
            .or(any_remaining)
        {
            Some(bid) => (bid, false),
            None => {
                // Editor invariants require at least one buffer at all times.
                // When the user opted out of auto-creating a visible empty
                // buffer on last close, mark the synthesized buffer as a
                // placeholder: hidden from tabs *and* skipped during pane
                // rendering, so the workspace genuinely looks blank.
                let new_id = self.new_buffer();
                if !self
                    .config
                    .editor
                    .auto_create_empty_buffer_on_last_buffer_close
                {
                    if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&new_id) {
                        meta.hidden_from_tabs = true;
                        meta.synthetic_placeholder = true;
                    }
                }
                (new_id, true)
            }
        };

        CloseReplacement {
            buffer,
            created_empty,
            return_to_group,
        }
    }

    /// Remove every trace of a now-closed buffer from the active window's
    /// per-buffer maps: the buffer registry, cross-window attachments, event
    /// logs, semantic-token bookkeeping, the panel-id mapping, and each
    /// split's open-buffers / focus-history lists.
    fn purge_buffer_state(&mut self, id: BufferId) {
        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .remove(&id);
        self.detach_buffer_from_all_windows(id);
        self.active_window_mut().event_logs.remove(&id);
        self.active_window_mut().seen_byte_ranges.remove(&id);
        self.active_window_mut().buffer_metadata.remove(&id);
        self.active_window_mut().status_bar_values.remove(&id);
        if let Some((request_id, _, _)) = self
            .active_window_mut()
            .semantic_tokens_in_flight
            .remove(&id)
        {
            self.active_window_mut()
                .pending_semantic_token_requests
                .remove(&request_id);
        }
        if let Some((request_id, _, _, _)) = self
            .active_window_mut()
            .semantic_tokens_range_in_flight
            .remove(&id)
        {
            self.active_window_mut()
                .pending_semantic_token_range_requests
                .remove(&request_id);
        }
        self.active_window_mut()
            .semantic_tokens_range_last_request
            .remove(&id);
        self.active_window_mut()
            .semantic_tokens_range_applied
            .remove(&id);
        self.active_window_mut()
            .semantic_tokens_full_debounce
            .remove(&id);

        // Remove buffer from the active window's panel_ids mapping
        // if it was a panel buffer. Prevents stale entries when the
        // same panel_id is reused later.
        self.panel_ids_mut().retain(|_, &mut buf_id| buf_id != id);

        // Remove buffer from all splits' open_buffers lists and focus history
        for view_state in self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .values_mut()
        {
            view_state.remove_buffer(id);
            view_state.remove_from_history(id);
        }
    }

    /// Switch to the given buffer
    pub fn switch_buffer(&mut self, id: BufferId) {
        if self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .contains_key(&id)
            && id != self.active_buffer()
        {
            // Save current position before switching buffers
            self.active_window_mut()
                .position_history
                .commit_pending_movement();

            // Also explicitly record current position (in case there was no pending movement)
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            let buffer_id = self.active_buffer();
            let ph = &mut self.active_window_mut().position_history;
            ph.record_movement(buffer_id, position, anchor);
            ph.commit_pending_movement();

            self.set_active_buffer(id);
        }
    }

    /// Close the current tab in the current split view.
    /// If the tab is the last viewport of the underlying buffer, do the same as close_buffer
    /// (including triggering the save/discard prompt for modified buffers).
    ///
    /// When the active tab is a buffer group (its `active_group_tab` is set),
    /// this closes the entire group rather than the currently-focused inner
    /// panel buffer. Individual panels are internal details of the group —
    /// the user closes them all together by closing the group tab.
    pub fn close_tab(&mut self) {
        // If the active split has a group tab active, close the whole group
        // rather than just the focused panel buffer — only the Close-Tab
        // command (or keybinding) can express "close the group I'm viewing",
        // so this prelude stays here rather than in `close_tab_in_split`.
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        if let Some(group_leaf_id) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&active_split)
            .and_then(|vs| vs.active_group_tab)
        {
            self.close_buffer_group_by_leaf(group_leaf_id);
            self.set_status_message(t!("buffer.tab_closed").to_string());
            return;
        }

        // Delegate to `close_tab_in_split` so the Close-Buffer command,
        // Alt+W, and the mouse × button all run the same code path —
        // there should be no difference in behavior between them.
        let buffer_id = self.active_buffer();
        self.close_tab_in_split(buffer_id, active_split);
    }

    /// Close a specific tab (buffer) in a specific split.
    ///
    /// This is the single shared implementation used by:
    ///   * the mouse × button on a tab,
    ///   * the Close Buffer command (via `close_tab`),
    ///   * the Close Tab command and the `Alt+W` keybinding (via `close_tab`).
    ///
    /// All three paths should behave identically; keep new logic here.
    /// Returns true if the tab was closed without needing a prompt.
    pub fn close_tab_in_split(&mut self, buffer_id: BufferId, split_id: LeafId) -> bool {
        // If closing a terminal buffer while in terminal mode, exit terminal mode
        if self.active_window().terminal_mode && self.active_window().is_terminal_buffer(buffer_id)
        {
            self.active_window_mut().terminal_mode = false;
            self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .iter()
            .filter(|(&sid, view_state)| sid != split_id && view_state.has_buffer(buffer_id))
            .count();

        // Get the split's open buffers
        let split_tabs = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // Last viewport of this buffer - need to close buffer entirely
            if let Some(state) = self
                .windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .get(&buffer_id)
            {
                if state.buffer.is_modified() {
                    // Buffer has unsaved changes - prompt for confirmation
                    let name = self.get_buffer_display_name(buffer_id);
                    let save_key = t!("prompt.key.save").to_string();
                    let discard_key = t!("prompt.key.discard").to_string();
                    let cancel_key = t!("prompt.key.cancel").to_string();
                    self.start_prompt(
                        t!(
                            "prompt.buffer_modified",
                            name = name,
                            save_key = save_key,
                            discard_key = discard_key,
                            cancel_key = cancel_key
                        )
                        .to_string(),
                        PromptType::ConfirmCloseBuffer { buffer_id },
                    );
                    return false;
                }
            }
            // If this is the only tab in this split AND there are other
            // splits, close the split rather than swap it to a fallback
            // buffer.  Mirrors `close_tab()` so mouse-click close and
            // Close Buffer/Close Tab commands behave the same — without
            // this, the × button leaves a leftover split showing some
            // unrelated buffer (observed with the Search/Replace panel).
            let has_other_splits = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .root()
                .count_leaves()
                > 1;
            if split_tabs.len() <= 1 && has_other_splits {
                self.handle_close_split(split_id.into());
                // handle_close_split also disposes the buffer-less split;
                // buffer lifetime cleanup happens via its own path.
                if let Err(e) = self.close_buffer(buffer_id) {
                    tracing::debug!(
                        "close_tab_in_split: buffer cleanup after split close failed: {}",
                        e
                    );
                }
                // Focus snapped to the surviving split via the low-level
                // split-collapse path; restore terminal mode for the now-active
                // buffer. Runs after `close_buffer` so its terminal-mode
                // teardown can't clobber the restore (issue #2485).
                self.sync_terminal_mode_to_active_buffer();
                self.set_status_message(t!("buffer.tab_closed").to_string());
                return true;
            }
            if let Err(e) = self.close_buffer(buffer_id) {
                self.set_status_message(t!("file.cannot_close", error = e.to_string()).to_string());
            } else {
                self.set_status_message(t!("buffer.tab_closed").to_string());
            }
        } else {
            use crate::view::split::TabTarget;

            // There are other viewports of this buffer — just remove it from
            // this split's tabs. Use the full tab list (open_buffers), which
            // includes group tabs (panels); `split_tabs`/`buffer_tab_ids_vec`
            // omits groups, so relying on it here would tear the split down
            // even when a group tab remains to fall back to (the "git log
            // disappears" bug).
            let targets: Vec<TabTarget> = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&split_id)
                .map(|vs| vs.open_buffers.clone())
                .unwrap_or_default();

            let closing = TabTarget::Buffer(buffer_id);
            let closing_idx = targets.iter().position(|t| *t == closing).unwrap_or(0);
            let has_other_tab = targets.iter().any(|t| *t != closing);

            if !has_other_tab {
                // This is genuinely the only tab in this split — close it.
                self.handle_close_split(split_id.into());
                self.sync_terminal_mode_to_active_buffer();
                return true;
            }

            // Pick the tab to activate after removal: the one before the
            // closed tab (or the next one if we closed the first). This
            // mirrors the previous buffer-only behaviour but can also land
            // on a remaining group tab.
            let replacement = if closing_idx > 0 {
                targets[closing_idx - 1]
            } else {
                // First remaining target after the closed one.
                *targets
                    .iter()
                    .find(|t| **t != closing)
                    .expect("has_other_tab")
            };

            // Activate the replacement tab and drop the closed one. The buffer
            // case must move the split tree AND the `SplitViewState.active_buffer`
            // together: routing it through `set_pane_buffer` (not the tree-only
            // `set_split_buffer`) is the fix for the cursor desync — updating
            // only the tree stranded the view-state on the just-closed buffer,
            // so the cursor and render read its zeroed view-state while edits
            // applied to the tree's (different) buffer.
            match replacement {
                TabTarget::Buffer(replacement_buffer) => {
                    self.active_window_mut()
                        .set_pane_buffer(split_id, replacement_buffer);
                    // The replacement is active now, so removing the closed
                    // buffer also frees its keyed view-state (`remove_buffer`
                    // refuses to drop the state of whatever is still active).
                    if let Some(view_state) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .get_mut(&split_id)
                    {
                        view_state.remove_buffer(buffer_id);
                    }
                }
                TabTarget::Group(group_leaf) => {
                    // Drop the closed buffer's tab before activating the group,
                    // matching the original ordering for the group path.
                    if let Some(view_state) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .get_mut(&split_id)
                    {
                        view_state.remove_buffer(buffer_id);
                    }
                    self.activate_group_tab(split_id, group_leaf);
                }
            }

            // The replacement tab was activated through the split manager,
            // bypassing the buffer-focus path; restore terminal mode for it.
            self.sync_terminal_mode_to_active_buffer();
            self.set_status_message(t!("buffer.tab_closed").to_string());
        }
        true
    }

    /// Close all other tabs in a split, keeping only the specified buffer
    pub fn close_other_tabs_in_split(&mut self, keep_buffer_id: BufferId, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        // Close all tabs except the one we want to keep
        let tabs_to_close: Vec<_> = split_tabs
            .iter()
            .filter(|&&id| id != keep_buffer_id)
            .copied()
            .collect();

        let mut closed = 0;
        let mut skipped_modified = 0;
        for buffer_id in tabs_to_close {
            if self.close_tab_in_split_silent(buffer_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        // Make sure the kept buffer is active
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_split_buffer(split_id, keep_buffer_id);

        self.reseat_tab_scroll_for_split(split_id);
        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close tabs to the right of the specified buffer in a split
    pub fn close_tabs_to_right_in_split(&mut self, buffer_id: BufferId, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        // Find the index of the target buffer
        let Some(target_idx) = split_tabs.iter().position(|&id| id == buffer_id) else {
            return;
        };

        // Close all tabs after the target
        let tabs_to_close: Vec<_> = split_tabs.iter().skip(target_idx + 1).copied().collect();

        let mut closed = 0;
        let mut skipped_modified = 0;
        for buf_id in tabs_to_close {
            if self.close_tab_in_split_silent(buf_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        self.reseat_tab_scroll_for_split(split_id);
        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close tabs to the left of the specified buffer in a split
    pub fn close_tabs_to_left_in_split(&mut self, buffer_id: BufferId, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        // Find the index of the target buffer
        let Some(target_idx) = split_tabs.iter().position(|&id| id == buffer_id) else {
            return;
        };

        // Close all tabs before the target
        let tabs_to_close: Vec<_> = split_tabs.iter().take(target_idx).copied().collect();

        let mut closed = 0;
        let mut skipped_modified = 0;
        for buf_id in tabs_to_close {
            if self.close_tab_in_split_silent(buf_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        self.reseat_tab_scroll_for_split(split_id);
        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close all tabs in a split
    pub fn close_all_tabs_in_split(&mut self, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        let mut closed = 0;
        let mut skipped_modified = 0;

        // Close all tabs (this will eventually close the split when empty)
        for buffer_id in split_tabs {
            if self.close_tab_in_split_silent(buffer_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        // If any modified tabs were skipped, the split survives with a reduced
        // tab list. Re-anchor its scroll offset so the surviving tabs stay in
        // view. (When the split was torn down entirely there's no state left to
        // adjust; the no-op is silent.)
        self.reseat_tab_scroll_for_split(split_id);
        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Re-pin a split's tab-scroll offset around its currently-active buffer.
    ///
    /// Batch closes (Close Others / Close to Right / Close to Left / Close All)
    /// shrink the tab strip without going through `set_active_buffer`, so the
    /// scroll offset from the pre-close state can leave the surviving active
    /// tab off-screen — the user sees an empty tab bar after Close Others
    /// (sinelaw/fresh#2229). Calling this after a batch close re-runs the
    /// "make the active tab visible" math against the new tab list using the
    /// editor's effective tabs width. Silently no-ops if the split is gone.
    fn reseat_tab_scroll_for_split(&mut self, split_id: LeafId) {
        let Some(active_buffer) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .and_then(|(mgr, _)| mgr.buffer_for_split(split_id))
        else {
            return;
        };
        let tabs_width = self.active_window().effective_tabs_width();
        self.active_window_mut()
            .ensure_active_tab_visible(split_id, active_buffer, tabs_width);
    }

    /// Set status message for batch close operations
    fn set_batch_close_status_message(&mut self, closed: usize, skipped_modified: usize) {
        let message = match (closed, skipped_modified) {
            (0, 0) => t!("buffer.no_tabs_to_close").to_string(),
            (0, n) => t!("buffer.skipped_modified", count = n).to_string(),
            (n, 0) => t!("buffer.closed_tabs", count = n).to_string(),
            (c, s) => t!("buffer.closed_tabs_skipped", closed = c, skipped = s).to_string(),
        };
        self.set_status_message(message);
    }

    /// Close a tab silently (without setting status message)
    /// Used internally by batch close operations
    /// Returns true if the tab was closed, false if it was skipped (e.g., modified buffer)
    fn close_tab_in_split_silent(&mut self, buffer_id: BufferId, split_id: LeafId) -> bool {
        // If closing a terminal buffer while in terminal mode, exit terminal mode
        if self.active_window().terminal_mode && self.active_window().is_terminal_buffer(buffer_id)
        {
            self.active_window_mut().terminal_mode = false;
            self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .iter()
            .filter(|(&sid, view_state)| sid != split_id && view_state.has_buffer(buffer_id))
            .count();

        // Get the split's open buffers
        let split_tabs = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // Last viewport of this buffer - need to close buffer entirely
            // Skip modified buffers to avoid prompting during batch operations
            if let Some(state) = self
                .windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .get(&buffer_id)
            {
                if state.buffer.is_modified() {
                    // Skip modified buffers - don't close them
                    return false;
                }
            }
            if let Err(e) = self.close_buffer(buffer_id) {
                tracing::warn!("Failed to close buffer: {}", e);
            }
            true
        } else {
            // There are other viewports of this buffer - just remove from this split's tabs
            if split_tabs.len() <= 1 {
                // This is the only tab in this split - close the split
                self.handle_close_split(split_id.into());
                return true;
            }

            // Find replacement buffer for this split
            let current_idx = split_tabs
                .iter()
                .position(|&id| id == buffer_id)
                .unwrap_or(0);
            let replacement_idx = if current_idx > 0 { current_idx - 1 } else { 1 };
            let replacement_buffer = split_tabs.get(replacement_idx).copied();

            // Remove buffer from this split's tabs
            if let Some(view_state) = self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .get_mut(&split_id)
            {
                view_state.remove_buffer(buffer_id);
            }

            // Update the split to show the replacement buffer. Route
            // through set_pane_buffer to keep tree and SVS in lockstep.
            if let Some(replacement) = replacement_buffer {
                self.active_window_mut()
                    .set_pane_buffer(split_id, replacement);
            }
            true
        }
    }

    /// Switch to next buffer in current split's tabs
    pub fn next_buffer(&mut self) {
        self.cycle_tab(1);
    }

    /// Switch to previous buffer in current split's tabs
    pub fn prev_buffer(&mut self) {
        self.cycle_tab(-1);
    }

    /// Cycle through the active split's tab targets (buffers AND groups).
    /// Direction: +1 = next, -1 = previous.
    fn cycle_tab(&mut self, direction: i32) {
        use crate::view::split::TabTarget;

        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        let Some(view_state) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&active_split)
        else {
            return;
        };

        // Collect visible tab targets, filtering out hidden buffers.
        let targets: Vec<TabTarget> = view_state
            .open_buffers
            .iter()
            .copied()
            .filter(|t| match t {
                TabTarget::Buffer(id) => !self
                    .active_window()
                    .buffer_metadata
                    .get(id)
                    .map(|m| m.hidden_from_tabs)
                    .unwrap_or(false),
                TabTarget::Group(_) => true,
            })
            .collect();

        if targets.len() < 2 {
            return;
        }

        let current_target = view_state.active_target();
        let Some(idx) = targets.iter().position(|t| *t == current_target) else {
            return;
        };

        let next_idx = if direction > 0 {
            (idx + 1) % targets.len()
        } else if idx == 0 {
            targets.len() - 1
        } else {
            idx - 1
        };

        if targets[next_idx] == current_target {
            return;
        }

        // Save current position before switching
        self.active_window_mut()
            .position_history
            .commit_pending_movement();
        let cursors = self.active_cursors();
        let position = cursors.primary().position;
        let anchor = cursors.primary().anchor;
        let buffer_id = self.active_buffer();
        let ph = &mut self.active_window_mut().position_history;
        ph.record_movement(buffer_id, position, anchor);
        ph.commit_pending_movement();

        // Start the slide before the switch so the runner's cached
        // last-frame captures the OUTGOING tab's content. The new
        // content gets painted on the next render and the push fires
        // over it. Direction: next-tab pushes from the right, prev
        // from the left. Wraparound still follows the user's intent
        // (Next wraps right, Prev wraps left) so the animation
        // direction matches the keystroke rather than the idx delta.
        self.active_window_mut()
            .animate_tab_switch(active_split, direction.signum());

        match targets[next_idx] {
            TabTarget::Buffer(buffer_id) => {
                self.set_active_buffer(buffer_id);
            }
            TabTarget::Group(group_leaf_id) => {
                self.activate_group_tab(active_split, group_leaf_id);
            }
        }
    }

    /// Navigate back in position history
    pub fn navigate_back(&mut self) {
        // Set flag to prevent recording this navigation movement
        self.active_window_mut().in_navigation = true;

        // Commit any pending movement
        self.active_window_mut()
            .position_history
            .commit_pending_movement();

        // If we're at the end of history (haven't used back yet), save current position
        // so we can navigate forward to it later
        if self.active_window_mut().position_history.can_go_back()
            && !self.active_window_mut().position_history.can_go_forward()
        {
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            let buffer_id = self.active_buffer();
            let ph = &mut self.active_window_mut().position_history;
            ph.record_movement(buffer_id, position, anchor);
            ph.commit_pending_movement();
        }

        // Navigate to the previous position
        if let Some(entry) = self.active_window_mut().position_history.back() {
            let target_buffer = entry.buffer_id;
            let target_position = entry.position;
            let target_anchor = entry.anchor;

            // Switch to the target buffer
            if self
                .windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .contains_key(&target_buffer)
            {
                self.set_active_buffer(target_buffer);

                // Move cursor to the saved position
                let cursors = self.active_cursors();
                let cursor_id = cursors.primary_id();
                let old_position = cursors.primary().position;
                let old_anchor = cursors.primary().anchor;
                let old_sticky_column = cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: None, // Reset sticky column for navigation
                };
                let split_id = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                self.active_window_mut()
                    .apply_event_to_buffer(target_buffer, split_id, &event);
                // Position-history entries can land anywhere in the buffer;
                // the viewport must scroll to the restored cursor or the user
                // sees the same page after Ctrl+- / Ctrl+= (#1689).
                self.active_window_mut()
                    .ensure_active_cursor_visible_for_navigation(true);
            }
        }

        // Clear the flag
        self.active_window_mut().in_navigation = false;
    }

    /// Navigate forward in position history
    pub fn navigate_forward(&mut self) {
        // Set flag to prevent recording this navigation movement
        self.active_window_mut().in_navigation = true;

        if let Some(entry) = self.active_window_mut().position_history.forward() {
            let target_buffer = entry.buffer_id;
            let target_position = entry.position;
            let target_anchor = entry.anchor;

            // Switch to the target buffer
            if self
                .windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .contains_key(&target_buffer)
            {
                self.set_active_buffer(target_buffer);

                // Move cursor to the saved position
                let cursors = self.active_cursors();
                let cursor_id = cursors.primary_id();
                let old_position = cursors.primary().position;
                let old_anchor = cursors.primary().anchor;
                let old_sticky_column = cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: None, // Reset sticky column for navigation
                };
                let split_id = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                self.active_window_mut()
                    .apply_event_to_buffer(target_buffer, split_id, &event);
                // Position-history entries can land anywhere in the buffer;
                // the viewport must scroll to the restored cursor or the user
                // sees the same page after Ctrl+- / Ctrl+= (#1689).
                self.active_window_mut()
                    .ensure_active_cursor_visible_for_navigation(true);
            }
        }

        // Clear the flag
        self.active_window_mut().in_navigation = false;
    }

    /// Retain a closed terminal's rendered backing file so its scrollback
    /// stays searchable (Universal Search "Terminals" scope). Renames it to
    /// a unique `<stem>-closed-<epoch_ms>.txt` so a future terminal that
    /// reuses the same id can't clobber it, then bounds the retained set.
    /// Best-effort throughout — a failure just means that log isn't kept.
    fn retain_closed_terminal_backing(&self, path: &std::path::Path) {
        use std::time::{SystemTime, UNIX_EPOCH};
        let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
            return;
        };
        let Some(parent) = path.parent() else {
            return;
        };
        let epoch_ms = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_millis())
            .unwrap_or(0);
        let retained = parent.join(format!("{stem}-closed-{epoch_ms}.txt"));
        #[allow(clippy::let_underscore_must_use)]
        let _ = crate::app::terminal::terminal_backing_fs().rename(path, &retained);
        self.gc_retained_terminal_backings(parent);
    }

    /// Prune the oldest retained (`-closed-`) terminal backing files in a
    /// directory so they don't grow without bound. Ordering uses the epoch
    /// embedded in the filename, so it needs no filesystem metadata. Live
    /// backing files (no `-closed-` marker) are never touched.
    fn gc_retained_terminal_backings(&self, dir: &std::path::Path) {
        const MAX_RETAINED: usize = 200;
        let Ok(entries) = crate::app::terminal::terminal_backing_fs().read_dir(dir) else {
            return;
        };
        let mut retained: Vec<(u128, std::path::PathBuf)> = entries
            .into_iter()
            .filter_map(|e| {
                let rest = e.name.strip_suffix(".txt")?;
                let idx = rest.rfind("-closed-")?;
                let epoch: u128 = rest[idx + "-closed-".len()..].parse().ok()?;
                Some((epoch, e.path))
            })
            .collect();
        if retained.len() <= MAX_RETAINED {
            return;
        }
        retained.sort_by_key(|(epoch, _)| *epoch);
        let remove_count = retained.len() - MAX_RETAINED;
        for (_, p) in retained.into_iter().take(remove_count) {
            #[allow(clippy::let_underscore_must_use)]
            let _ = crate::app::terminal::terminal_backing_fs().remove_file(&p);
        }
    }
}
