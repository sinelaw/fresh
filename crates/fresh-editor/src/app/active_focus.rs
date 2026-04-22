//! Active-buffer / active-split focus management on `Editor`.
//!
//! `set_active_buffer` and `focus_split` are the centralized methods for
//! switching what the user is looking at. Both fire several invariants:
//! split manager updates, tab list updates, file-explorer sync, terminal
//! buffer resume, semantic-token cleanup for deleted buffers, etc.
//!
//! ## Pane-buffer invariant
//!
//! "Which buffer is displayed in leaf split S" is stored in two places
//! for historical reasons: `split_manager`'s tree (as the leaf node's
//! `buffer_id`) and `split_view_states[S]` (as `active_buffer` plus an
//! entry in `keyed_states`). These must agree — callers downstream
//! (notably `apply_event_to_active_buffer`) index one using the other
//! without re-validating.
//!
//! All writes to this fact MUST go through [`Editor::set_pane_buffer`]
//! (or the higher-level wrappers `set_active_buffer` / `focus_split`
//! that call it). Raw `split_manager.set_split_buffer` /
//! `split_manager.set_active_buffer_id` calls updated only one side,
//! which caused issue #1620 (a `None.unwrap()` panic when clicking
//! after a buffer was closed from another split).

use super::*;

impl Editor {
    /// Atomically update both sides of the pane-buffer invariant for a
    /// given leaf split: the split tree's stored buffer AND the matching
    /// `SplitViewState.active_buffer` / `keyed_states` map.
    ///
    /// This is the one place that is allowed to change "which buffer is
    /// shown in pane `leaf`". Every call site that used to poke
    /// `split_manager.set_split_buffer` or
    /// `split_manager.set_active_buffer_id` directly should go through
    /// here instead, so the two stores can never drift (see the
    /// module-level note and issue #1620).
    ///
    /// If the leaf has no `SplitViewState` yet (e.g. mid-session-restore,
    /// when the SVS is registered later), the tree is still updated and
    /// the SVS sync is skipped — the caller is responsible for ensuring
    /// the SVS exists by the time any input is routed.
    pub(super) fn set_pane_buffer(&mut self, leaf: LeafId, buffer_id: BufferId) {
        self.split_manager.set_split_buffer(leaf, buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&leaf) {
            view_state.switch_buffer(buffer_id);
        }
    }

    /// Set the active buffer and trigger all necessary side effects
    ///
    /// This is the centralized method for switching buffers. It:
    /// - Updates split manager (single source of truth for active buffer)
    /// - Adds buffer to active split's tabs (if not already there)
    /// - Syncs file explorer to the new active file (if visible)
    ///
    /// Use this instead of directly calling split_manager.set_active_buffer_id()
    /// to ensure all side effects happen consistently.
    pub(super) fn set_active_buffer(&mut self, buffer_id: BufferId) {
        if self.active_buffer() == buffer_id {
            return; // No change
        }

        // Dismiss transient popups and clear hover state when switching buffers
        self.on_editor_focus_lost();

        // Cancel search/replace prompts when switching buffers
        // (they are buffer-specific and don't make sense across buffers)
        self.cancel_search_prompt_if_active();

        // Track the previous buffer for "Switch to Previous Tab" command
        let previous = self.active_buffer();

        // If leaving a terminal buffer while in terminal mode, remember it should resume
        if self.terminal_mode && self.is_terminal_buffer(previous) {
            self.terminal_mode_resume.insert(previous);
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Capture the previous focus target BEFORE set_pane_buffer runs,
        // so the LRU records the right thing.
        let active_split = self.split_manager.active_split();
        let previous_target = self
            .split_view_states
            .get(&active_split)
            .map(|vs| vs.active_target());

        // Atomic pane-buffer update: tree + SVS in lockstep.
        self.set_pane_buffer(active_split, buffer_id);

        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
            view_state.active_group_tab = None;
            view_state.focused_group_leaf = None;
            if let Some(previous_target) = previous_target {
                view_state.push_focus(previous_target);
            }
        }

        // If switching to a terminal buffer that should resume terminal mode, re-enter it
        if self.terminal_mode_resume.contains(&buffer_id) && self.is_terminal_buffer(buffer_id) {
            self.terminal_mode = true;
            self.key_context = crate::input::keybindings::KeyContext::Terminal;
        } else if self.is_terminal_buffer(buffer_id) {
            // Switching to terminal in read-only mode - sync buffer to show current terminal content
            // This ensures the backing file content and cursor position are up to date
            self.sync_terminal_to_buffer(buffer_id);
        }

        // Ensure the newly active tab is visible
        self.ensure_active_tab_visible(active_split, buffer_id, self.effective_tabs_width());

        // Note: We don't sync file explorer here to avoid flicker during tab switches.
        // File explorer syncs when explicitly focused via focus_file_explorer().

        // Update plugin state snapshot BEFORE firing the hook so that
        // the handler sees the new active buffer, not the old one.
        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();

        // Emit buffer_activated hook for plugins
        self.plugin_manager.run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
        );
    }

    /// Focus a split and its buffer, handling all side effects including terminal mode.
    ///
    /// This is the primary method for switching focus between splits via mouse clicks.
    /// It handles:
    /// - Exiting terminal mode when leaving a terminal buffer
    /// - Updating split manager state
    /// - Managing tab state and previous buffer tracking
    /// - Syncing file explorer
    ///
    /// Use this instead of calling set_active_split directly when switching focus.
    pub(super) fn focus_split(&mut self, split_id: LeafId, buffer_id: BufferId) {
        // Fixed buffer-group panels (toolbars, headers, footers) aren't focus
        // targets: focusing them would route keyboard input at an invisible
        // cursor. Plugins can still detect clicks via the mouse_click hook,
        // which fires in the click handlers before reaching here. Scrollable
        // panels still receive focus even with a hidden cursor.
        if self.is_non_scrollable_buffer(buffer_id) {
            return;
        }

        // Clicking a buffer pane (e.g. a tab) explicitly moves focus to
        // the editor. If the key context was still on the file explorer
        // (because the user's previous click landed there), reset it so
        // subsequent keystrokes target the buffer. The terminal branch
        // below can still upgrade to KeyContext::Terminal when needed.
        // Issue #1540.
        if self.key_context == crate::input::keybindings::KeyContext::FileExplorer {
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        let previous_split = self.split_manager.active_split();
        let previous_buffer = self.active_buffer(); // Get BEFORE changing split
        let split_changed = previous_split != split_id;

        // Preview is anchored to the split it was opened in. Moving focus to
        // a different split commits the preview — walking away is commitment.
        if split_changed {
            self.promote_preview_if_not_in_split(split_id);
        }

        // If `split_id` is not in the main split tree, it must be an inner
        // leaf of a Grouped subtree stashed in `grouped_subtrees`. For those
        // we don't change `split_manager.active_split` (the group's host
        // split remains active). Instead, find the host split and update
        // its `focused_group_leaf` marker so `active_buffer()` routes to
        // the clicked inner panel buffer.
        if !self
            .split_manager
            .root()
            .leaf_split_ids()
            .contains(&split_id)
        {
            // Find which group contains this inner leaf.
            let host_split = self
                .grouped_subtrees
                .iter()
                .find(|(_, node)| {
                    if let crate::view::split::SplitNode::Grouped { layout, .. } = node {
                        layout.find(split_id.into()).is_some()
                    } else {
                        false
                    }
                })
                .map(|(group_leaf_id, _)| *group_leaf_id)
                .and_then(|group_leaf_id| {
                    // Find the split whose open_buffers has this group tab.
                    self.split_view_states
                        .iter()
                        .find(|(_, vs)| vs.has_group(group_leaf_id))
                        .map(|(sid, _)| (*sid, group_leaf_id))
                });

            if let Some((host, group_leaf_id)) = host_split {
                self.split_manager.set_active_split(host);
                if let Some(vs) = self.split_view_states.get_mut(&host) {
                    vs.active_group_tab = Some(group_leaf_id);
                    vs.focused_group_leaf = Some(split_id);
                }
                if let Some(inner_vs) = self.split_view_states.get_mut(&split_id) {
                    inner_vs.switch_buffer(buffer_id);
                }
                self.key_context = crate::input::keybindings::KeyContext::Normal;
                return;
            }
            // Fall through: we couldn't find the group; the original path
            // will set_active_split which will fail silently.
        }

        if split_changed {
            // Switching to a different split - exit terminal mode if active
            if self.terminal_mode && self.is_terminal_buffer(previous_buffer) {
                self.terminal_mode = false;
                self.key_context = crate::input::keybindings::KeyContext::Normal;
            }

            // Update split manager to focus this split
            self.split_manager.set_active_split(split_id);

            // Atomic pane-buffer update: tree + SVS in lockstep. Replaces
            // the previous pair of split_manager.set_active_buffer_id +
            // view_state.switch_buffer that could desync if either leg
            // silently no-op'd (issue #1620).
            self.set_pane_buffer(split_id, buffer_id);

            // Set key context based on target buffer type
            if self.is_terminal_buffer(buffer_id) {
                self.terminal_mode = true;
                self.key_context = crate::input::keybindings::KeyContext::Terminal;
            } else {
                // Ensure key context is Normal when focusing a non-terminal buffer
                // This handles the case of clicking on editor from FileExplorer context
                self.key_context = crate::input::keybindings::KeyContext::Normal;
            }

            // Handle buffer change side effects
            if previous_buffer != buffer_id {
                self.position_history.commit_pending_movement();
                if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                    view_state.add_buffer(buffer_id);
                    view_state.push_focus(crate::view::split::TabTarget::Buffer(previous_buffer));
                }
                // Note: We don't sync file explorer here to avoid flicker during split focus changes.
                // File explorer syncs when explicitly focused via focus_file_explorer().
            }
        } else {
            // Same split, different buffer (tab switch) - use set_active_buffer for terminal resume
            self.set_active_buffer(buffer_id);
        }
    }
}
