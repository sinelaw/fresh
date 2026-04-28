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

impl Editor {
    /// Close the given buffer
    pub fn close_buffer(&mut self, id: BufferId) -> anyhow::Result<()> {
        // Check for unsaved changes
        if let Some(state) = self.buffers.get(&id) {
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
        // Clear preview tracking if we're closing the current preview buffer.
        // This keeps `preview` from pointing at a freed buffer id.
        if let Some((_, preview_id)) = self.preview {
            if preview_id == id {
                self.preview = None;
            }
        }

        // Complete any --wait tracking for this buffer
        if let Some((wait_id, _)) = self.wait_tracking.remove(&id) {
            self.completed_waits.push(wait_id);
        }

        // Save file state before closing (for per-file session persistence)
        self.save_file_state_on_close(id);

        // Delete recovery data for explicitly closed buffers (including unnamed)
        if let Err(e) = self.delete_buffer_recovery(id) {
            tracing::debug!("Failed to delete buffer recovery on close: {}", e);
        }

        // If closing a terminal buffer, clean up terminal-related data structures
        if let Some(terminal_id) = self.terminal_buffers.remove(&id) {
            // Close the terminal process
            self.terminal_manager.close(terminal_id);

            // Clean up backing/rendering file
            let backing_file = self.terminal_backing_files.remove(&terminal_id);
            if let Some(ref path) = backing_file {
                // Best-effort cleanup of temporary terminal files.
                #[allow(clippy::let_underscore_must_use)]
                let _ = self.authority.filesystem.remove_file(path);
            }
            // Clean up raw log file
            if let Some(log_file) = self.terminal_log_files.remove(&terminal_id) {
                if backing_file.as_ref() != Some(&log_file) {
                    // Best-effort cleanup of temporary terminal files.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = self.authority.filesystem.remove_file(&log_file);
                }
            }

            // Remove from terminal_mode_resume to prevent stale entries
            self.terminal_mode_resume.remove(&id);

            // Exit terminal mode if we were in it
            if self.terminal_mode {
                self.terminal_mode = false;
                self.key_context = crate::input::keybindings::KeyContext::Normal;
            }
        }

        // Walk the focus-history LRU (most recent first) to find the tab
        // the user should land on. This naturally handles both buffer and
        // group tabs — whichever the user was looking at most recently wins.
        let active_split = self.split_manager.active_split();

        let replacement_target: Option<crate::view::split::TabTarget> =
            self.split_view_states.get(&active_split).and_then(|vs| {
                use crate::view::split::TabTarget;
                vs.focus_history.iter().rev().find_map(|t| match t {
                    TabTarget::Buffer(bid) if *bid == id => None, // skip the closing buffer
                    TabTarget::Buffer(bid) => {
                        // Skip hidden-from-tabs buffers (panel helpers etc.)
                        let hidden = self
                            .buffer_metadata
                            .get(bid)
                            .map(|m| m.hidden_from_tabs)
                            .unwrap_or(false);
                        if hidden || !self.buffers.contains_key(bid) {
                            None
                        } else {
                            Some(*t)
                        }
                    }
                    TabTarget::Group(leaf) => {
                        // Only if the group still exists
                        if self.grouped_subtrees.contains_key(leaf) {
                            Some(*t)
                        } else {
                            None
                        }
                    }
                })
            });

        // Any visible buffer other than the one being closed. Used as the
        // general fallback (no LRU target or LRU points at a gone group).
        let fallback_buffer: Option<BufferId> = self
            .buffers
            .keys()
            .find(|&&bid| {
                bid != id
                    && !self
                        .buffer_metadata
                        .get(&bid)
                        .map(|m| m.hidden_from_tabs)
                        .unwrap_or(false)
            })
            .copied();

        // Capture before the replacement computation — new_buffer() has the
        // side effect of calling set_active_buffer which changes active_buffer().
        let closing_active = self.active_buffer() == id;

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
        // shadow does get created — see below.)
        let already_keyed = return_to_group.and_then(|_| {
            self.split_view_states
                .get(&active_split)?
                .keyed_states
                .keys()
                .find(|&&bid| bid != id)
                .copied()
        });

        // Absolute last-resort pool for the Group case: any buffer at all,
        // including hidden panel ones. The shadow cleanup below keeps
        // those invisible.
        let any_remaining =
            return_to_group.and_then(|_| self.buffers.keys().copied().find(|&bid| bid != id));

        let (replacement_buffer, created_empty_buffer) = match direct_replacement
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
                    if let Some(meta) = self.buffer_metadata.get_mut(&new_id) {
                        meta.hidden_from_tabs = true;
                        meta.synthetic_placeholder = true;
                    }
                }
                (new_id, true)
            }
        };

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
                .buffer_metadata
                .get(&replacement_buffer)
                .is_some_and(|m| m.hidden_from_tabs);
            if return_to_group.is_some() && hidden {
                use crate::view::split::TabTarget;
                if let Some(vs) = self.split_view_states.get_mut(&active_split) {
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
        let splits_to_update = self.split_manager.splits_for_buffer(id);
        for split_id in splits_to_update {
            self.set_pane_buffer(split_id, replacement_buffer);
        }

        self.buffers.remove(&id);
        self.event_logs.remove(&id);
        self.seen_byte_ranges.remove(&id);
        self.buffer_metadata.remove(&id);
        if let Some((request_id, _, _)) = self.semantic_tokens_in_flight.remove(&id) {
            self.pending_semantic_token_requests.remove(&request_id);
        }
        if let Some((request_id, _, _, _)) = self.semantic_tokens_range_in_flight.remove(&id) {
            self.pending_semantic_token_range_requests
                .remove(&request_id);
        }
        self.semantic_tokens_range_last_request.remove(&id);
        self.semantic_tokens_range_applied.remove(&id);
        self.semantic_tokens_full_debounce.remove(&id);

        // Remove buffer from panel_ids mapping if it was a panel buffer
        // This prevents stale entries when the same panel_id is reused later
        self.panel_ids.retain(|_, &mut buf_id| buf_id != id);

        // Remove buffer from all splits' open_buffers lists and focus history
        for view_state in self.split_view_states.values_mut() {
            view_state.remove_buffer(id);
            view_state.remove_from_history(id);
        }

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
        self.plugin_manager.run_hook(
            "buffer_closed",
            fresh_core::hooks::HookArgs::BufferClosed { buffer_id: id },
        );

        Ok(())
    }

    /// Switch to the given buffer
    pub fn switch_buffer(&mut self, id: BufferId) {
        if self.buffers.contains_key(&id) && id != self.active_buffer() {
            // Save current position before switching buffers
            self.position_history.commit_pending_movement();

            // Also explicitly record current position (in case there was no pending movement)
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();

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
        let active_split = self.split_manager.active_split();
        if let Some(group_leaf_id) = self
            .split_view_states
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
        if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .split_view_states
            .iter()
            .filter(|(&sid, view_state)| sid != split_id && view_state.has_buffer(buffer_id))
            .count();

        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // Last viewport of this buffer - need to close buffer entirely
            if let Some(state) = self.buffers.get(&buffer_id) {
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
            let has_other_splits = self.split_manager.root().count_leaves() > 1;
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
                self.set_status_message(t!("buffer.tab_closed").to_string());
                return true;
            }
            if let Err(e) = self.close_buffer(buffer_id) {
                self.set_status_message(t!("file.cannot_close", error = e.to_string()).to_string());
            } else {
                self.set_status_message(t!("buffer.tab_closed").to_string());
            }
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
            let replacement_buffer = split_tabs[replacement_idx];

            // Remove buffer from this split's tabs
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.remove_buffer(buffer_id);
            }

            // Update the split to show the replacement buffer
            self.split_manager
                .set_split_buffer(split_id, replacement_buffer);

            self.set_status_message(t!("buffer.tab_closed").to_string());
        }
        true
    }

    /// Close all other tabs in a split, keeping only the specified buffer
    pub fn close_other_tabs_in_split(&mut self, keep_buffer_id: BufferId, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
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
        self.split_manager
            .set_split_buffer(split_id, keep_buffer_id);

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close tabs to the right of the specified buffer in a split
    pub fn close_tabs_to_right_in_split(&mut self, buffer_id: BufferId, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
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

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close tabs to the left of the specified buffer in a split
    pub fn close_tabs_to_left_in_split(&mut self, buffer_id: BufferId, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
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

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close all tabs in a split
    pub fn close_all_tabs_in_split(&mut self, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
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

        self.set_batch_close_status_message(closed, skipped_modified);
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
        if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .split_view_states
            .iter()
            .filter(|(&sid, view_state)| sid != split_id && view_state.has_buffer(buffer_id))
            .count();

        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // Last viewport of this buffer - need to close buffer entirely
            // Skip modified buffers to avoid prompting during batch operations
            if let Some(state) = self.buffers.get(&buffer_id) {
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
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.remove_buffer(buffer_id);
            }

            // Update the split to show the replacement buffer. Route
            // through set_pane_buffer to keep tree and SVS in lockstep.
            if let Some(replacement) = replacement_buffer {
                self.set_pane_buffer(split_id, replacement);
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

        let active_split = self.split_manager.active_split();
        let Some(view_state) = self.split_view_states.get(&active_split) else {
            return;
        };

        // Collect visible tab targets, filtering out hidden buffers.
        let targets: Vec<TabTarget> = view_state
            .open_buffers
            .iter()
            .copied()
            .filter(|t| match t {
                TabTarget::Buffer(id) => !self
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
        self.position_history.commit_pending_movement();
        let cursors = self.active_cursors();
        let position = cursors.primary().position;
        let anchor = cursors.primary().anchor;
        self.position_history
            .record_movement(self.active_buffer(), position, anchor);
        self.position_history.commit_pending_movement();

        // Start the slide before the switch so the runner's cached
        // last-frame captures the OUTGOING tab's content. The new
        // content gets painted on the next render and the push fires
        // over it. Direction: next-tab pushes from the right, prev
        // from the left. Wraparound still follows the user's intent
        // (Next wraps right, Prev wraps left) so the animation
        // direction matches the keystroke rather than the idx delta.
        self.animate_tab_switch(active_split, direction.signum());

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
        self.in_navigation = true;

        // Commit any pending movement
        self.position_history.commit_pending_movement();

        // If we're at the end of history (haven't used back yet), save current position
        // so we can navigate forward to it later
        if self.position_history.can_go_back() && !self.position_history.can_go_forward() {
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
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
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                let split_id = self.split_manager.active_split();
                let state = self.buffers.get_mut(&target_buffer).unwrap();
                let view_state = self.split_view_states.get_mut(&split_id).unwrap();
                state.apply(&mut view_state.cursors, &event);
                // Position-history entries can land anywhere in the buffer;
                // the viewport must scroll to the restored cursor or the user
                // sees the same page after Ctrl+- / Ctrl+= (#1689).
                self.ensure_active_cursor_visible_for_navigation(true);
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
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                let split_id = self.split_manager.active_split();
                let state = self.buffers.get_mut(&target_buffer).unwrap();
                let view_state = self.split_view_states.get_mut(&split_id).unwrap();
                state.apply(&mut view_state.cursors, &event);
                // Position-history entries can land anywhere in the buffer;
                // the viewport must scroll to the restored cursor or the user
                // sees the same page after Ctrl+- / Ctrl+= (#1689).
                self.ensure_active_cursor_visible_for_navigation(true);
            }
        }

        // Clear the flag
        self.in_navigation = false;
    }
}
