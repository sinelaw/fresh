//! Split/pane management for the Editor.
//!
//! This module contains all methods related to managing editor splits:
//! - Creating horizontal/vertical splits
//! - Closing splits
//! - Navigating between splits
//! - Managing per-split view states (cursors, viewport)
//! - Split size adjustment and maximize

use rust_i18n::t;

use crate::model::event::{BufferId, ContainerId, LeafId, SplitDirection, SplitId};
use crate::view::folding::CollapsedFoldLineRange;
use crate::view::split::{SplitViewState, TabTarget};

use super::Editor;

impl Editor {
    /// Split the current pane horizontally
    pub fn split_pane_horizontal(&mut self) {
        // Failure is already reported on the status line inside
        // `split_pane_impl`; a keystroke has no caller to hand a Result to.
        if let Err(e) =
            self.split_pane_impl(crate::model::event::SplitDirection::Horizontal, false, 0.5)
        {
            tracing::debug!("split_pane_horizontal: {e}");
        }
    }

    /// Split the current pane vertically
    pub fn split_pane_vertical(&mut self) {
        if let Err(e) =
            self.split_pane_impl(crate::model::event::SplitDirection::Vertical, false, 0.5)
        {
            tracing::debug!("split_pane_vertical: {e}");
        }
    }

    /// Common split creation logic.
    ///
    /// `before` places the new pane first — left for a vertical divider,
    /// above for a horizontal one. The keyboard commands always pass
    /// `false` (new pane right/below); the plugin API's `splitWindow` is
    /// what exposes the other side, so an agent can say "put the terminal
    /// on the left" without a swap dance afterwards.
    ///
    /// `ratio` is the *first* child's share of the space, regardless of
    /// which side the new pane landed on.
    ///
    /// Returns the new pane's id, or the reason it could not be created.
    pub(crate) fn split_pane_impl(
        &mut self,
        direction: crate::model::event::SplitDirection,
        before: bool,
        ratio: f32,
    ) -> Result<crate::model::event::LeafId, String> {
        // Splitting the layout is a commitment gesture for any preview tab:
        // the user is setting up their working environment around it. Promote
        // before touching the split tree so the invariant "preview is anchored
        // to a single split" stays consistent across the operation.
        self.active_window_mut().promote_current_preview();

        let current_buffer_id = self.active_buffer();
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();

        // Copy keyed states from source split so the new split inherits per-buffer state
        let source_keyed_states = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&active_split)
            .map(|vs| {
                vs.keyed_states
                    .iter()
                    .filter(|(&buf_id, _)| buf_id != current_buffer_id)
                    .map(|(&buf_id, buf_state)| {
                        let folds = self
                            .buffers()
                            .get(&buf_id)
                            .map(|state| {
                                buf_state
                                    .folds
                                    .collapsed_line_ranges(&state.buffer, &state.marker_list)
                            })
                            .unwrap_or_default();
                        (buf_id, buf_state.clone(), folds)
                    })
                    .collect::<Vec<(
                        BufferId,
                        crate::view::split::BufferViewState,
                        Vec<CollapsedFoldLineRange>,
                    )>>()
            });

        let split_outcome = self.split_manager_mut().split_active_positioned(
            direction,
            current_buffer_id,
            ratio,
            before,
        );
        match split_outcome {
            Ok(new_split_id) => {
                let mut view_state = SplitViewState::with_buffer(
                    self.terminal_width,
                    self.terminal_height,
                    current_buffer_id,
                );
                view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                    line_numbers: self.config.editor.line_numbers,
                    highlight_current_line: self.config.editor.highlight_current_line,
                    line_wrap: self
                        .active_window()
                        .resolve_line_wrap_for_buffer(current_buffer_id),
                    wrap_indent: self.config.editor.wrap_indent,
                    wrap_column: self
                        .active_window()
                        .resolve_wrap_column_for_buffer(current_buffer_id),
                    rulers: self.config.editor.rulers.clone(),
                    scroll_offset: self.config.editor.scroll_offset,
                });

                // Copy keyed states from source split for OTHER buffers (not the active one).
                // The active buffer gets a fresh cursor in the new split.
                if let Some(source) = source_keyed_states {
                    for (buf_id, mut buf_state, folds) in source {
                        if let Some(state) = self
                            .windows
                            .get_mut(&self.active_window)
                            .map(|w| &mut w.buffers)
                            .expect("active window present")
                            .get_mut(&buf_id)
                        {
                            buf_state.folds.clear(&mut state.marker_list);
                            for fold in folds {
                                let start_line = fold.header_line.saturating_add(1);
                                let end_line = fold.end_line;
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
                        view_state.keyed_states.insert(buf_id, buf_state);
                    }
                }

                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .insert(new_split_id, view_state);
                let msg = match direction {
                    crate::model::event::SplitDirection::Horizontal => t!("split.horizontal"),
                    crate::model::event::SplitDirection::Vertical => t!("split.vertical"),
                };
                self.set_status_message(msg.to_string());
                // A new split changes every sibling pane's width/height.
                // Reflow through the single layout funnel so existing
                // terminals shrink to their new pane immediately, instead of
                // waiting for the next unrelated resize trigger.
                self.relayout();
                Ok(new_split_id)
            }
            Err(e) => {
                self.set_status_message(t!("split.error", error = e.to_string()).to_string());
                self.relayout();
                Err(e)
            }
        }
    }

    /// Close the active split
    pub fn close_active_split(&mut self) {
        // Closing a split rearranges tab ownership (remaining tabs migrate
        // to the new active split). Promote any preview first so it doesn't
        // end up orphaned in a split that no longer exists, or silently
        // migrated to an unrelated pane.
        self.active_window_mut().promote_current_preview();

        let closing_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();

        // Get the tabs from the split we're closing before we close it
        let closing_split_tabs = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&closing_split)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        match self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .close_split(closing_split)
        {
            Ok(_) => {
                // Clean up the view state for the closed split
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .remove(&closing_split);

                // Drop the closed split from every terminal's scrollback set so
                // no terminal keeps a stale (split, scrollback) edge.
                self.active_window_mut()
                    .forget_split_terminal_modes(closing_split);

                // Get the new active split after closing
                let new_active_split = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();

                // Transfer tabs from closed split to the new active split
                if let Some(view_state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .get_mut(&new_active_split)
                {
                    for target in closing_split_tabs {
                        // Only add if not already in the split's tabs
                        if !view_state.open_buffers.contains(&target) {
                            view_state.open_buffers.push(target);
                        }
                    }
                }

                // NOTE: active_buffer is now derived from split_manager, no sync needed

                self.set_status_message(t!("split.closed").to_string());
            }
            Err(e) => {
                self.set_status_message(
                    t!("split.cannot_close", error = e.to_string()).to_string(),
                );
            }
        }

        // Focus snapped to the surviving split through the split manager,
        // bypassing the buffer-focus path — restore terminal mode so a
        // re-focused terminal keeps the mode it remembers (issue #2485).
        self.sync_terminal_mode_to_active_buffer();

        // Closing a split gives its space back to the surviving panes.
        // Reflow through the single layout funnel so their terminals grow
        // into the reclaimed area.
        self.relayout();
    }

    /// Switch to next split
    pub fn next_split(&mut self) {
        self.switch_split(true);
        self.set_status_message(t!("split.next").to_string());
    }

    /// Switch to previous split
    pub fn prev_split(&mut self) {
        self.switch_split(false);
        self.set_status_message(t!("split.prev").to_string());
    }

    /// Common split switching logic
    fn switch_split(&mut self, next: bool) {
        // `next_split`/`prev_split` auto-unmaximize so the newly-active
        // split is visible (issue #1961). Detect that here so terminal
        // PTYs can be resized to match the restored layout.
        let was_maximized = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr.is_maximized())
            .unwrap_or(false);

        if next {
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .next_split();
        } else {
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .prev_split();
        }

        if was_maximized {
            self.relayout();
        }

        // Ensure the active tab is visible in the newly active split
        let split_id = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        // Moving focus to a different split commits the preview — walking
        // away is commitment. Matches the rule applied in `focus_split`.
        self.active_window_mut()
            .promote_preview_if_not_in_split(split_id);
        let buffer = self.active_buffer();
        let tabs_width = self.active_window().split_tabs_width(split_id);
        self.active_window_mut()
            .ensure_active_tab_visible(split_id, buffer, tabs_width);

        let buffer_id = self.active_buffer();

        // Bring terminal mode in line with the newly focused split: a
        // terminal resumes the live/scrollback mode it remembers, a
        // non-terminal clears terminal mode. Single restore authority.
        self.sync_terminal_mode_to_active_buffer();

        // Emit buffer_activated hook for plugins
        self.plugin_manager.read().unwrap().run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
        );
    }

    /// Adjust the size of the active split
    pub fn adjust_split_size(&mut self, delta: f32) {
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        if let Some(container) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .parent_container_of(active_split)
        {
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .adjust_ratio(container, delta);

            let percent = (delta * 100.0) as i32;
            self.set_status_message(t!("split.size_adjusted", percent = percent).to_string());
            // Split ratios changed: reflow through the single layout funnel.
            self.relayout();
        }
    }

    /// Toggle maximize state for the active split
    pub fn toggle_maximize_split(&mut self) {
        match self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .toggle_maximize()
        {
            Ok(maximized) => {
                if maximized {
                    self.set_status_message(t!("split.maximized").to_string());
                } else {
                    self.set_status_message(t!("split.restored").to_string());
                }
                // Maximize/restore changed the split sizes: reflow via funnel.
                self.relayout();
            }
            Err(e) => self.set_status_message(e),
        }
    }

    /// Get cached separator areas for testing
    /// Returns (split_id, direction, x, y, length) tuples
    pub fn get_separator_areas(&self) -> &[(ContainerId, SplitDirection, u16, u16, u16)] {
        &self.active_layout().separator_areas
    }

    /// Get cached tab layouts for testing
    pub fn get_tab_layouts(
        &self,
    ) -> &std::collections::HashMap<LeafId, crate::view::ui::tabs::TabLayout> {
        &self.active_layout().tab_layouts
    }

    /// Get cached split content areas for testing
    /// Returns (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end) tuples
    pub fn get_split_areas(
        &self,
    ) -> &[(
        LeafId,
        BufferId,
        ratatui::layout::Rect,
        ratatui::layout::Rect,
        usize,
        usize,
    )] {
        &self.active_layout().split_areas
    }

    /// Get the ratio of a specific split (for testing).
    ///
    /// Looks in the main split tree first, then falls back to splits
    /// that live inside stashed Grouped subtrees (buffer-group panels).
    pub fn get_split_ratio(&self, split_id: SplitId) -> Option<f32> {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .get_ratio(split_id)
            .or_else(|| self.grouped_split_ratio(crate::model::event::ContainerId(split_id)))
    }

    /// Get the active split ID (for testing)
    pub fn get_active_split(&self) -> LeafId {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split()
    }

    /// Get the buffer ID for a split (for testing)
    pub fn get_split_buffer(&self, split_id: SplitId) -> Option<BufferId> {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .get_buffer_id(split_id)
    }

    /// Get the open buffers (tabs) in a split (for testing)
    pub fn get_split_tabs(&self, split_id: LeafId) -> Vec<BufferId> {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default()
    }

    /// Get the number of splits (for testing)
    pub fn get_split_count(&self) -> usize {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .root()
            .count_leaves()
    }

    /// Compute the drop zone for a tab drag at a given position (for testing)
    pub fn compute_drop_zone(
        &self,
        col: u16,
        row: u16,
        source_split_id: LeafId,
    ) -> Option<super::types::TabDropZone> {
        self.compute_tab_drop_zone(col, row, source_split_id)
    }

    /// Cycle through all splits and tabs in the current window as a flat list.
    ///
    /// The pane ordering is:
    ///   (split_0, tab_0), (split_0, tab_1), …,
    ///   (split_1, tab_0), (split_1, tab_1), …
    ///
    /// Within each split, tabs are ordered by their position in the tab bar.
    /// Splits are ordered by the split-manager's leaf list.
    ///
    /// This is different from `NextSplit`/`PrevSplit` (which only move
    /// between splits, keeping the current tab) and `NextWindow`/`PrevWindow`
    /// (which cycle between separate editor windows by id).  `NextPane` /
    /// `PrevPane` treat every (split, tab) pair as a unique step and cycle
    /// through them as if they were laid out flat on the ground.
    ///
    /// If there is only one target total the action is a no-op.
    pub fn next_pane(&mut self) {
        self.cycle_pane(true);
        self.set_status_message(t!("cmd.next_pane").to_string());
    }

    /// Cycle through all splits and tabs in the current window in reverse order.
    pub fn prev_pane(&mut self) {
        self.cycle_pane(false);
        self.set_status_message(t!("cmd.prev_pane").to_string());
    }

    fn cycle_pane(&mut self, forward: bool) {
        // Build the flat list of (split_id, buffer_id) for the active window.
        let mut targets: Vec<(LeafId, BufferId)> = Vec::new();

        let Some((mgr, vs_map)) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
        else {
            return;
        };

        let leaf_ids = mgr.root().leaf_split_ids();
        for &split_id in &leaf_ids {
            let Some(vs) = vs_map.get(&split_id) else {
                continue;
            };
            for buf_id in vs.buffer_tab_ids() {
                targets.push((split_id, buf_id));
            }
        }

        if targets.is_empty() {
            return;
        }

        // Find the current position in the flat list.
        // We read the split tree's buffer_id (set by the previous call to
        // `cycle_pane` via `set_split_buffer`) rather than the view state's
        // `active_buffer` — the view state gets reset by the render cycle
        // after every command, but the split-tree buffer_id survives.
        let current_split = mgr.active_split();
        let current_buf_from_tree = mgr.active_buffer_id();
        let current_buf = current_buf_from_tree.unwrap_or_else(|| self.active_buffer());
        let current_pos = targets
            .iter()
            .position(|(s, b)| *s == current_split && *b == current_buf)
            .unwrap_or(0);

        let len = targets.len();
        let new_pos = if forward {
            (current_pos + 1) % len
        } else {
            (current_pos + len - 1) % len
        };

        let (next_split, next_buf) = targets[new_pos];

        // Navigate to the target split.
        if let Some((mgr_mut, _)) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.buffers.splits_mut())
        {
            if mgr_mut.active_split() != next_split {
                mgr_mut.set_active_split(next_split);
            }
        }

        // Navigate to the target buffer/tab within the target split.
        if let Some((_, vs_map_mut)) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.buffers.splits_mut())
        {
            if let Some(vs_mut) = vs_map_mut.get_mut(&next_split) {
                // Always switch to the target buffer unless it's already
                // active — even if the current target is a group tab.
                let needs_switch = match vs_mut.active_target() {
                    TabTarget::Buffer(buf_id) => buf_id != next_buf,
                    TabTarget::Group(_) => true,
                };
                if needs_switch {
                    vs_mut.switch_buffer(next_buf);
                }
            }
        }

        // Persist the new buffer into the split tree so that
        // `mgr.active_buffer_id()` returns it on the next invocation.
        // This survives the render cycle which resets the view state's
        // `active_buffer`, keeping the flat-position lookup correct.
        if let Some((mgr_mut, _)) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.buffers.splits_mut())
        {
            mgr_mut.set_split_buffer(next_split, next_buf);
        }

        // Treat the target pane's buffer exactly like `next_split`/`prev_split`
        // and `next_buffer`/`prev_buffer` do: re-derive terminal mode for the
        // newly focused buffer. A terminal resumes its remembered
        // live/scrollback mode (key context → Terminal for a live PTY), and a
        // non-terminal clears terminal mode. Without this the key context kept
        // whatever value it had before the jump, so landing on a terminal via
        // NextPane/PrevPane never routed keys to the PTY, and the non-explicit
        // `ExitTerminalMode` deferred on the way out — which is a no-op that
        // relies on "the upcoming focus change re-derives the key context" —
        // left a stale Terminal context when jumping away. Single restore
        // authority, same as the split/buffer commands.
        self.sync_terminal_mode_to_active_buffer();

        // The target tab may be a terminal that was hidden behind another tab
        // in its split (so a window resize never reached it). Refresh visible
        // terminal sizes so its PTY child sees the pane it now occupies —
        // mirrors `set_active_buffer` (issue #1795).
        self.active_window_mut().resize_visible_terminals();

        // Keep the newly active tab scrolled into view within its split,
        // matching `switch_split` and `set_active_buffer`.
        let tabs_width = self.active_window().split_tabs_width(next_split);
        self.active_window_mut()
            .ensure_active_tab_visible(next_split, next_buf, tabs_width);

        // Emit the buffer_activated hook for plugins, matching every other
        // focus-changing command.
        self.plugin_manager.read().unwrap().run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated {
                buffer_id: next_buf,
            },
        );
    }
}

#[cfg(feature = "plugins")]
impl Editor {
    /// Handle `editor.splitWindow(...)` — the plugin/agent entry point for
    /// creating a pane.
    ///
    /// Everything a caller needs comes back in the response: the new pane's
    /// id *and* its geometry, so "did the terminal land on the left" is
    /// answerable without a follow-up read. The snapshot is refreshed before
    /// answering, so a caller that awaits this can immediately
    /// `listSplits()` / `describeWorkspace()` and see its own change.
    pub(crate) fn handle_split_window(
        &mut self,
        options: fresh_core::api::SplitWindowOptions,
        request_id: u64,
    ) {
        use fresh_core::api::{SplitAxis, SplitCreated, SplitPlacement};

        let direction = match options.direction.unwrap_or_default() {
            SplitAxis::Vertical => crate::model::event::SplitDirection::Vertical,
            SplitAxis::Horizontal => crate::model::event::SplitDirection::Horizontal,
        };
        let before = matches!(options.place.unwrap_or_default(), SplitPlacement::Before);
        // Clamp rather than reject: a ratio outside the usable band would be
        // pinned by the layout anyway, and failing a whole split over it
        // would be a worse answer than the pane the caller asked for.
        let ratio = options.ratio.unwrap_or(0.5).clamp(0.05, 0.95);

        let source_split_id = self.active_split_id();

        let new_split_id = match self.split_pane_impl(direction, before, ratio) {
            Ok(id) => id,
            Err(e) => {
                self.send_plugin_response(fresh_core::api::PluginResponse::SplitWindowCreated {
                    request_id,
                    result: Err(e),
                });
                return;
            }
        };

        // Open the requested file *in the new pane*. Doing it here rather
        // than making the caller follow up with `openFileInSplit` is the
        // difference between one call and two, and removes the window where
        // the new pane briefly shows the wrong buffer.
        if let Some(file) = options.file.as_deref().filter(|f| !f.trim().is_empty()) {
            let path = self.resolve_workspace_path(file);
            if let Err(e) = self.handle_open_file_in_split(new_split_id.0 .0, path, None, None) {
                tracing::warn!("splitWindow: could not open {} in new pane: {}", file, e);
            }
        }

        if options.keep_focus.unwrap_or(false) {
            self.split_manager_mut().set_active_split(source_split_id);
        }

        // Geometry has to be computed after the layout settles, which
        // `split_pane_impl`'s relayout has already done.
        let rect = self.split_rect(new_split_id).unwrap_or_default();
        let buffer_id = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .and_then(|(_, vs)| vs.get(&new_split_id))
            .map(|vs| vs.active_buffer)
            .unwrap_or(self.active_buffer());

        // Refresh before answering: the caller awaiting this response is
        // very likely about to read the layout back.
        self.update_plugin_state_snapshot();
        self.send_plugin_response(fresh_core::api::PluginResponse::SplitWindowCreated {
            request_id,
            result: Ok(SplitCreated {
                split_id: new_split_id.0 .0,
                source_split_id: source_split_id.0 .0,
                buffer_id,
                x: rect.x,
                y: rect.y,
                width: rect.width,
                height: rect.height,
            }),
        });
    }

    /// The active pane's id.
    pub(crate) fn active_split_id(&self) -> crate::model::event::LeafId {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr.active_split())
            .expect("active window must have a populated split layout")
    }

    /// Where a pane currently sits on screen, in editor-area cells.
    /// `None` once the leaf is gone (closed, or collapsed by its last tab).
    pub(crate) fn split_rect(
        &self,
        leaf: crate::model::event::LeafId,
    ) -> Option<ratatui::layout::Rect> {
        let area = self
            .windows
            .get(&self.active_window)
            .map(|w| w.editor_content_area())?;
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .and_then(|(mgr, _)| {
                mgr.get_visible_buffers(area)
                    .into_iter()
                    .find(|(id, _, _)| *id == leaf)
                    .map(|(_, _, rect)| rect)
            })
    }

    /// Resolve a caller-supplied path against the window's root, so a script
    /// can say `"README.md"` and mean the obvious thing.
    #[cfg(feature = "plugins")]
    fn resolve_workspace_path(&self, path: &str) -> std::path::PathBuf {
        let p = std::path::Path::new(path);
        if p.is_absolute() {
            p.to_path_buf()
        } else {
            self.active_window().root.join(p)
        }
    }
}

#[cfg(feature = "plugins")]
impl Editor {
    /// Move a buffer into `target` — show it there, and drop its tab from
    /// whatever pane held it before.
    ///
    /// The difference from `setSplitBuffer` is the second half. Setting a
    /// pane's active buffer leaves the original tab where it was, so a script
    /// arranging panes ends up stranding tabs it never asked to keep.
    pub(crate) fn handle_move_buffer_to_split(
        &mut self,
        buffer_id: fresh_core::BufferId,
        split_id: fresh_core::SplitId,
    ) {
        use crate::model::event::LeafId;

        let target = LeafId(split_id);
        if !self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .contains_key(&buffer_id)
        {
            tracing::error!("Buffer {:?} not found for MoveBufferToSplit", buffer_id);
            return;
        }

        // Every pane whose *tab strip* carries it, except the destination.
        //
        // Read from the view states rather than `splits_for_buffer`, which
        // reports the narrower "is displaying it" relation: a pane can hold a
        // buffer as a background tab without showing it, and those are exactly
        // the copies a move is supposed to clean up.
        let sources: Vec<LeafId> = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| {
                vs.iter()
                    .filter(|(leaf, state)| {
                        **leaf != target
                            && state
                                .open_buffers
                                .iter()
                                .any(|tab| tab.as_buffer() == Some(buffer_id))
                    })
                    .map(|(leaf, _)| *leaf)
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        // Show it in the destination first: dropping the last tab from a pane
        // can collapse that pane, and doing it in this order means the buffer
        // is never momentarily displayed nowhere.
        self.handle_set_split_buffer(split_id, buffer_id);

        for source in sources {
            if let Some(vs) = self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .get_mut(&source)
            {
                vs.remove_buffer(buffer_id);
            }
        }
        self.relayout();
    }
}

impl Editor {
    /// Record which lines of a buffer point somewhere, replacing any previous
    /// set. An empty list clears them.
    pub(crate) fn set_line_targets(
        &mut self,
        buffer_id: fresh_core::BufferId,
        targets: Vec<fresh_core::api::LineTarget>,
    ) {
        if targets.is_empty() {
            self.line_targets.remove(&buffer_id);
        } else {
            self.line_targets.insert(buffer_id, targets);
        }
    }

    /// The 0-indexed line the primary cursor is on, in the active buffer.
    pub(crate) fn cursor_line_in_active_buffer(&self) -> Option<usize> {
        let position = self.active_window().active_cursors().primary().position;
        Some(self.active_state().buffer.get_line_number(position))
    }

    /// The target on `line` of `buffer_id`, if that line has one.
    pub(crate) fn line_target_at(
        &self,
        buffer_id: fresh_core::BufferId,
        line: usize,
    ) -> Option<fresh_core::api::LineTarget> {
        self.line_targets
            .get(&buffer_id)
            .and_then(|targets| targets.iter().find(|t| t.line == line))
            .cloned()
    }

    #[cfg(feature = "plugins")]
    /// Open what a line points at.
    ///
    /// Gated with the rest of the plugin surface: targets can only be set
    /// through `editor.setLineTargets`, so without plugins there is never one
    /// to follow.
    ///
    /// The destination pane is resolved by label when the target names one.
    /// Falling back to *beside* the index — rather than into it — is
    /// deliberate: an index that replaced itself with the first thing you
    /// clicked would destroy the view you were navigating.
    pub(crate) fn follow_line_target(&mut self, target: fresh_core::api::LineTarget) {
        let source_split = self.active_split_id();
        let destination = target
            .into
            .as_deref()
            .and_then(|label| self.split_by_label(label))
            .filter(|leaf| *leaf != source_split);

        let path = {
            let p = std::path::Path::new(&target.path);
            if p.is_absolute() {
                p.to_path_buf()
            } else {
                self.active_window().root.join(p)
            }
        };
        let line = target.target;

        let leaf = match destination {
            Some(leaf) => leaf,
            None => {
                // No usable label: put it in the pane next door, making one if
                // this is the only pane.
                match self.pane_beside(source_split) {
                    Some(leaf) => leaf,
                    None => match self.split_pane_impl(
                        crate::model::event::SplitDirection::Vertical,
                        false,
                        0.5,
                    ) {
                        Ok(leaf) => leaf,
                        Err(e) => {
                            tracing::warn!("line target: could not open a pane: {e}");
                            return;
                        }
                    },
                }
            }
        };

        #[cfg(feature = "plugins")]
        if let Err(e) = self.handle_open_file_in_split(leaf.0 .0, path, line, None) {
            tracing::warn!("line target: could not open {}: {e}", target.path);
            return;
        }

        // Opening leaves a tab for the file in the pane that was active as
        // well as the destination, so following a few entries would silently
        // fill the index's own tab strip with the things you visited. Move it
        // instead: the destination keeps it, everyone else drops it.
        #[cfg(feature = "plugins")]
        {
            let opened = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .and_then(|(_, vs)| vs.get(&leaf))
                .map(|vs| vs.active_buffer);
            if let Some(buffer_id) = opened {
                self.handle_move_buffer_to_split(buffer_id, leaf.0);
            }
        }

        // Focus follows the jump — the user asked to go there.
        self.split_manager_mut().set_active_split(leaf);
    }

    /// The leaf a label names, when it still exists.
    #[cfg(feature = "plugins")]
    fn split_by_label(&self, label: &str) -> Option<crate::model::event::LeafId> {
        let (mgr, _) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())?;
        mgr.labels()
            .iter()
            .find(|(_, l)| l.as_str() == label)
            .map(|(id, _)| crate::model::event::LeafId(*id))
            .filter(|leaf| self.split_rect(*leaf).is_some())
    }

    /// Any visible pane that isn't `this_one`, preferring the one to its right
    /// or below — where a reader would expect the destination to appear.
    #[cfg(feature = "plugins")]
    fn pane_beside(
        &self,
        this_one: crate::model::event::LeafId,
    ) -> Option<crate::model::event::LeafId> {
        let area = self
            .windows
            .get(&self.active_window)
            .map(|w| w.editor_content_area())?;
        let (mgr, _) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())?;
        let panes = mgr.get_visible_buffers(area);
        let here = panes.iter().find(|(id, _, _)| *id == this_one)?.2;
        panes
            .iter()
            .filter(|(id, _, _)| *id != this_one)
            .min_by_key(|(_, _, rect)| {
                let after = rect.x > here.x || rect.y > here.y;
                (!after as u32, rect.x as u32, rect.y as u32)
            })
            .map(|(id, _, _)| *id)
    }
}
