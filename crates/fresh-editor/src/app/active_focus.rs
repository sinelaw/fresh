//! Active-buffer / active-split focus management.
//!
//! `set_active_buffer` and `focus_split` are the centralized methods
//! for switching what the user is looking at. The Window-side methods
//! own the per-window state mutation (split manager, view states, tab
//! list, terminal-mode toggles, focus history); the thin `impl Editor`
//! wrappers orchestrate the editor-scoped side-effects that can't yet
//! be expressed without an `Editor` reference (terminal-buffer sync,
//! file-explorer follow, plugin state snapshot, plugin hook).
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
//! All writes to this fact MUST go through [`Window::set_pane_buffer`]
//! (or the higher-level wrappers `set_active_buffer` / `focus_split`
//! that call it). Raw `split_manager.set_split_buffer` /
//! `split_manager.set_active_buffer_id` calls updated only one side,
//! which caused issue #1620 (a `None.unwrap()` panic when clicking
//! after a buffer was closed from another split).

use super::window::Window;
use super::*;

/// Result of [`Window::focus_split`].
#[must_use]
pub(super) enum FocusSplitOutcome {
    /// Click was on a non-scrollable buffer-group panel, or the
    /// inner-leaf focus path completed without changing the active
    /// pane-buffer. No further work for Editor.
    Handled,
    /// "Same split, different buffer" — fall through to the full
    /// active-buffer orchestration (`Editor::set_active_buffer`) so
    /// the editor-wide plugin snapshot + hook fire.
    DelegateToActiveBuffer(BufferId),
}

impl Editor {
    /// Set the active buffer and trigger all necessary side effects.
    ///
    /// The per-window state mutation lives on
    /// [`Window::set_active_buffer`] (and the per-window terminal /
    /// file-explorer sync calls now nested inside it). This thin
    /// wrapper only adds the editor-wide plugin state snapshot
    /// refresh + the `buffer_activated` plugin hook.
    pub(super) fn set_active_buffer(&mut self, buffer_id: BufferId) {
        if !self.active_window_mut().set_active_buffer(buffer_id) {
            return;
        }
        // The Window body already synced the `terminal_mode` flag; finish
        // the restored-terminal transition (re-enable editing, drop the
        // stale screen tail, resize the PTY) that only the Editor level can.
        self.complete_terminal_mode_side_effects();
        // Plugin state snapshot reaches editor-wide state (clipboard,
        // windows list, config cache) so it stays on Editor. Run it
        // BEFORE the hook so the handler sees the new active buffer.
        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();
        self.plugin_manager.read().unwrap().run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
        );
    }

    /// Focus a split and its buffer, handling all side effects including
    /// terminal mode. Window-side body in [`Window::focus_split`].
    pub(super) fn focus_split(&mut self, split_id: LeafId, buffer_id: BufferId) {
        match self.active_window_mut().focus_split(split_id, buffer_id) {
            FocusSplitOutcome::Handled => {
                // Window body synced the flag; finish any restored-terminal
                // transition.
                self.complete_terminal_mode_side_effects();
            }
            FocusSplitOutcome::DelegateToActiveBuffer(target) => {
                self.set_active_buffer(target);
            }
        }
    }

    /// Bring `terminal_mode` / `key_context` fully in line with the active
    /// buffer after a focus change that bypassed [`Editor::set_active_buffer`]
    /// (split focus/navigation, split-collapse on close, opening a terminal
    /// split). This is the single restore authority: a terminal resumes the
    /// live/scrollback mode it had when it lost focus, a non-terminal clears
    /// terminal mode. See issue #2485.
    pub(crate) fn sync_terminal_mode_to_active_buffer(&mut self) {
        self.active_window_mut().sync_terminal_mode_flags();
        self.complete_terminal_mode_side_effects();
    }

    /// Editor-level completion of a transition *into* live terminal mode:
    /// restored / read-only terminals load with editing disabled, so finish
    /// the switch (re-enable editing, truncate the stale screen tail, resize
    /// the PTY) which only [`Editor::enter_terminal_mode`] can do. A no-op
    /// for live terminals and non-terminals.
    pub(super) fn complete_terminal_mode_side_effects(&mut self) {
        if self.active_window().terminal_mode
            && self
                .active_window()
                .is_terminal_buffer(self.active_buffer())
            && self.active_window().is_editing_disabled()
        {
            self.enter_terminal_mode();
        }
    }
}

impl Window {
    /// Window-side body of `set_active_buffer`. Mutates per-window state
    /// (focus loss, prompt cancel, split manager, view-state, terminal
    /// mode toggle, tab visibility, and the dependent terminal-sync /
    /// file-explorer-follow per-window side effects).
    ///
    /// Returns `true` when the active buffer actually changed (so the
    /// caller fires the editor-wide plugin snapshot + hook), `false`
    /// if the requested buffer was already active.
    pub(super) fn set_active_buffer(&mut self, buffer_id: BufferId) -> bool {
        if self.active_buffer() == buffer_id {
            return false;
        }

        // Dismiss transient popups and clear hover state when switching buffers
        self.on_editor_focus_lost();

        // Cancel search/replace prompts when switching buffers
        // (they are buffer-specific and don't make sense across buffers)
        self.cancel_search_prompt_if_active();

        // Capture the previous focus target BEFORE set_pane_buffer runs,
        // so the LRU records the right thing.
        let (mgr, vs) = self
            .buffers
            .splits()
            .expect("active window must have a populated split layout");
        let active_split = mgr.active_split();
        let previous_target = vs.get(&active_split).map(|vs| vs.active_target());

        // Atomic pane-buffer update: tree + SVS in lockstep.
        self.set_pane_buffer(active_split, buffer_id);

        if let Some(view_state) = self
            .split_view_states_mut()
            .expect("active window must have a populated split layout")
            .get_mut(&active_split)
        {
            view_state.add_buffer(buffer_id);
            view_state.active_group_tab = None;
            view_state.focused_group_leaf = None;
            if let Some(previous_target) = previous_target {
                view_state.push_focus(previous_target);
            }
        }

        // Bring terminal mode in line with the new active buffer (its
        // remembered live/scrollback mode). The single authority for the
        // flag half — see `sync_terminal_mode_flags`.
        self.sync_terminal_mode_flags();

        // Window resize events only resize terminals that are currently the
        // active tab in their split (see `resize_visible_terminals`). A
        // terminal hidden behind another tab when the host was resized never
        // sees the new size, so its PTY child keeps reporting stale
        // dimensions when the user switches back. Re-running the visible
        // resize here picks up the now-revealed terminal. Issue #1795.
        if self.is_terminal_buffer(buffer_id) {
            self.resize_visible_terminals();
        }

        // Ensure the newly active tab is visible
        let tabs_width = self.effective_tabs_width();
        self.ensure_active_tab_visible(active_split, buffer_id, tabs_width);

        if self.file_explorer_visible
            && self.resources.config.file_explorer.follow_active_buffer
            && self.key_context != crate::input::keybindings::KeyContext::FileExplorer
        {
            self.sync_file_explorer_to_active_file();
        }

        true
    }

    /// Flag-only half of terminal-mode sync: set `terminal_mode` /
    /// `key_context` from the active buffer and the mode it remembers.
    ///
    /// Each terminal buffer remembers its own interaction mode
    /// ([`TerminalInteractionMode`] on its [`TerminalBuffer`] record), so a
    /// re-focused terminal keeps whatever mode it had when it lost focus.
    /// This is the one place the flag is derived from focus;
    /// every focus path routes through it (directly or via
    /// [`Editor::sync_terminal_mode_to_active_buffer`]), so none can leave a
    /// re-focused terminal in the wrong mode (issue #2485).
    ///
    /// Only owns the Terminal↔Normal edge: when another surface holds input
    /// focus (file explorer, a prompt, a popup) `key_context` is something
    /// other than `Normal`/`Terminal`, and those surfaces manage
    /// `terminal_mode` themselves, so we leave it untouched. The Editor-level
    /// side effects of entering live mode (re-enable editing, truncate the
    /// stale screen tail, resize the PTY) are completed by
    /// [`Editor::complete_terminal_mode_side_effects`].
    pub(super) fn sync_terminal_mode_flags(&mut self) {
        use crate::input::keybindings::KeyContext;
        if !matches!(self.key_context, KeyContext::Normal | KeyContext::Terminal) {
            return;
        }
        let active = self.active_buffer();
        if let Some(terminal) = self.terminal_buffer(active) {
            if terminal.is_live() {
                self.terminal_mode = true;
                self.key_context = KeyContext::Terminal;
            } else {
                // Read-only scrollback terminal: refresh the file-backed view
                // (backing file + cursor) and stop routing keys to the PTY.
                self.sync_terminal_to_buffer(active);
                self.terminal_mode = false;
                self.key_context = KeyContext::Normal;
            }
        } else if self.terminal_mode {
            self.terminal_mode = false;
            self.key_context = KeyContext::Normal;
        }
    }

    /// Window-side body of `focus_split`. Returns a [`FocusSplitOutcome`]
    /// indicating whether the caller should fall through to the full
    /// active-buffer orchestration (for the "same split, different
    /// buffer" branch which needs the deferred Editor side-effects).
    pub(super) fn focus_split(
        &mut self,
        split_id: LeafId,
        buffer_id: BufferId,
    ) -> FocusSplitOutcome {
        // Fixed buffer-group panels (toolbars, headers, footers) aren't focus
        // targets: focusing them would route keyboard input at an invisible
        // cursor. Plugins can still detect clicks via the mouse_click hook,
        // which fires in the click handlers before reaching here. Scrollable
        // panels still receive focus even with a hidden cursor.
        if self.is_non_scrollable_buffer(buffer_id) {
            return FocusSplitOutcome::Handled;
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

        let previous_split = self
            .buffers
            .splits()
            .expect("active window must have a populated split layout")
            .0
            .active_split();
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
        let in_main_tree = self
            .buffers
            .splits()
            .expect("active window must have a populated split layout")
            .0
            .root()
            .leaf_split_ids()
            .contains(&split_id);
        if !in_main_tree {
            // Find which group contains this inner leaf.
            let group_leaf_id = self
                .grouped_subtrees
                .iter()
                .find(|(_, node)| {
                    if let crate::view::split::SplitNode::Grouped { layout, .. } = node {
                        layout.find(split_id.into()).is_some()
                    } else {
                        false
                    }
                })
                .map(|(group_leaf_id, _)| *group_leaf_id);
            let host_split = group_leaf_id.and_then(|group_leaf_id| {
                // Find the split whose open_buffers has this group tab.
                self.buffers
                    .splits()
                    .expect("active window must have a populated split layout")
                    .1
                    .iter()
                    .find(|(_, vs)| vs.has_group(group_leaf_id))
                    .map(|(sid, _)| (*sid, group_leaf_id))
            });

            if let Some((host, group_leaf_id)) = host_split {
                self.split_manager_mut()
                    .expect("active window must have a populated split layout")
                    .set_active_split(host);
                if let Some(vs) = self
                    .split_view_states_mut()
                    .expect("active window must have a populated split layout")
                    .get_mut(&host)
                {
                    vs.active_group_tab = Some(group_leaf_id);
                    vs.focused_group_leaf = Some(split_id);
                }
                if let Some(inner_vs) = self
                    .split_view_states_mut()
                    .expect("active window must have a populated split layout")
                    .get_mut(&split_id)
                {
                    inner_vs.switch_buffer(buffer_id);
                }
                self.key_context = crate::input::keybindings::KeyContext::Normal;
                return FocusSplitOutcome::Handled;
            }
            // Fall through: we couldn't find the group; the original path
            // will set_active_split which will fail silently.
        }

        if split_changed {
            // Update split manager to focus this split
            self.split_manager_mut()
                .expect("active window must have a populated split layout")
                .set_active_split(split_id);

            // Atomic pane-buffer update: tree + SVS in lockstep. Replaces
            // the previous pair of split_manager.set_active_buffer_id +
            // view_state.switch_buffer that could desync if either leg
            // silently no-op'd (issue #1620).
            self.set_pane_buffer(split_id, buffer_id);

            // Bring terminal mode in line with the now-active buffer and its
            // remembered live/scrollback mode — the single flag authority.
            self.sync_terminal_mode_flags();

            // Handle buffer change side effects
            if previous_buffer != buffer_id {
                self.position_history.commit_pending_movement();
                if let Some(view_state) = self
                    .split_view_states_mut()
                    .expect("active window must have a populated split layout")
                    .get_mut(&split_id)
                {
                    view_state.add_buffer(buffer_id);
                    view_state.push_focus(crate::view::split::TabTarget::Buffer(previous_buffer));
                }
                // Note: We don't sync file explorer here to avoid flicker during split focus changes.
                // File explorer syncs when explicitly focused via focus_file_explorer().
            }
            FocusSplitOutcome::Handled
        } else {
            // Same split, different buffer (tab switch) — defer to the
            // full set_active_buffer orchestration so the deferred deps
            // (terminal sync, file-explorer follow, plugin snapshot/hook)
            // run.
            FocusSplitOutcome::DelegateToActiveBuffer(buffer_id)
        }
    }
}
