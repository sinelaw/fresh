//! Miscellaneous input helpers on `Editor`.
//!
//! Tab navigation (switch_to_previous_tab, start_switch_to_tab_prompt),
//! character insertion (handle_insert_char_prompt, handle_insert_char_editor,
//! apply_action_as_events), cursor-movement tracking
//! (track_cursor_movement), and composite-buffer key routing
//! (try_route_composite_key). Small helpers grouped here so input.rs can
//! contain only the central key/action dispatch.

use anyhow::Result as AnyhowResult;
use rust_i18n::t;

use crate::input::keybindings::Action;
use crate::model::event::{BufferId, Event};
use crate::view::prompt::PromptType;

use super::Editor;

impl Editor {
    /// Switch to the previously active tab in the current split.
    /// Handles both buffer tabs and group tabs via the focus-history LRU.
    pub(super) fn switch_to_previous_tab(&mut self) {
        use crate::view::split::TabTarget;
        let active_split = self.split_manager.active_split();
        let previous_tab = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.previous_tab());

        match previous_tab {
            Some(TabTarget::Buffer(prev_id)) => {
                let is_valid = self
                    .split_view_states
                    .get(&active_split)
                    .is_some_and(|vs| vs.has_buffer(prev_id));

                if is_valid && prev_id != self.active_buffer() {
                    self.position_history.commit_pending_movement();
                    let cursors = self.active_cursors();
                    let position = cursors.primary().position;
                    let anchor = cursors.primary().anchor;
                    self.position_history
                        .record_movement(self.active_buffer(), position, anchor);
                    self.position_history.commit_pending_movement();
                    self.set_active_buffer(prev_id);
                } else if !is_valid {
                    self.set_status_message(t!("status.previous_tab_closed").to_string());
                }
            }
            Some(TabTarget::Group(leaf_id)) => {
                if self.grouped_subtrees.contains_key(&leaf_id) {
                    self.activate_group_tab(active_split, leaf_id);
                } else {
                    self.set_status_message(t!("status.previous_tab_closed").to_string());
                }
            }
            None => {
                self.set_status_message(t!("status.no_previous_tab").to_string());
            }
        }
    }

    /// Start the switch-to-tab-by-name prompt with suggestions from open buffers
    pub(super) fn start_switch_to_tab_prompt(&mut self) {
        let active_split = self.split_manager.active_split();
        let open_buffers: Vec<BufferId> =
            if let Some(view_state) = self.split_view_states.get(&active_split) {
                view_state.buffer_tab_ids_vec()
            } else {
                return;
            };

        if open_buffers.is_empty() {
            self.set_status_message(t!("status.no_tabs_in_split").to_string());
            return;
        }

        // Find the current buffer's index
        let current_index = open_buffers
            .iter()
            .position(|&id| id == self.active_buffer())
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = open_buffers
            .iter()
            .map(|&buffer_id| {
                let display_name = self
                    .buffer_metadata
                    .get(&buffer_id)
                    .map(|m| m.display_name.clone())
                    .unwrap_or_else(|| format!("Buffer {:?}", buffer_id));

                let is_current = buffer_id == self.active_buffer();
                let is_modified = self
                    .buffers
                    .get(&buffer_id)
                    .is_some_and(|b| b.buffer.is_modified());

                let description = match (is_current, is_modified) {
                    (true, true) => Some("(current, modified)".to_string()),
                    (true, false) => Some("(current)".to_string()),
                    (false, true) => Some("(modified)".to_string()),
                    (false, false) => None,
                };

                crate::input::commands::Suggestion {
                    text: display_name,
                    description,
                    value: Some(buffer_id.0.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Switch to tab: ".to_string(),
            PromptType::SwitchToTab,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
            }
        }
    }

    /// Switch to a tab by its BufferId
    pub(crate) fn switch_to_tab(&mut self, buffer_id: BufferId) {
        // Verify the buffer exists and is open in the current split
        let active_split = self.split_manager.active_split();
        let is_valid = self
            .split_view_states
            .get(&active_split)
            .is_some_and(|vs| vs.has_buffer(buffer_id));

        if !is_valid {
            self.set_status_message(t!("status.tab_not_found").to_string());
            return;
        }

        if buffer_id != self.active_buffer() {
            // Save current position before switching
            self.position_history.commit_pending_movement();

            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();

            self.set_active_buffer(buffer_id);
        }
    }

    /// Handle character insertion in prompt mode.
    pub(super) fn handle_insert_char_prompt(&mut self, c: char) -> AnyhowResult<()> {
        // Check if this is the query-replace confirmation prompt
        if let Some(ref prompt) = self.prompt {
            if prompt.prompt_type == PromptType::QueryReplaceConfirm {
                return self.handle_interactive_replace_key(c);
            }
        }

        // Reset history navigation when user starts typing
        // This allows them to press Up to get back to history items
        // Reset history navigation when typing in a prompt
        if let Some(ref prompt) = self.prompt {
            if let Some(key) = Self::prompt_type_to_history_key(&prompt.prompt_type) {
                if let Some(history) = self.prompt_histories.get_mut(&key) {
                    history.reset_navigation();
                }
            }
        }

        if let Some(prompt) = self.prompt_mut() {
            // Use insert_str to properly handle selection deletion
            let s = c.to_string();
            prompt.insert_str(&s);
        }
        self.update_prompt_suggestions();
        Ok(())
    }

    /// Handle character insertion in normal editor mode.
    pub(super) fn handle_insert_char_editor(&mut self, c: char) -> AnyhowResult<()> {
        // Check if editing is disabled (show_cursors = false)
        if self.is_editing_disabled() {
            self.set_status_message(t!("buffer.editing_disabled").to_string());
            return Ok(());
        }

        // Cancel any pending LSP requests since the text is changing
        self.cancel_pending_lsp_requests();

        if let Some(events) = self.action_to_events(Action::InsertChar(c)) {
            if events.len() > 1 {
                // Multi-cursor: use optimized bulk edit (O(n) instead of O(n²))
                let description = format!("Insert '{}'", c);
                if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description.clone())
                {
                    self.active_event_log_mut().append(bulk_edit);
                }
            } else {
                // Single cursor - apply normally
                for event in events {
                    self.active_event_log_mut().append(event.clone());
                    self.apply_event_to_active_buffer(&event);
                }
            }
        }

        // Auto-trigger signature help on '(' and ','
        if c == '(' || c == ',' {
            self.request_signature_help();
        }

        // Auto-trigger completion on trigger characters
        self.maybe_trigger_completion(c);

        Ok(())
    }

    /// Apply an action by converting it to events.
    ///
    /// This is the catch-all handler for actions that can be converted to buffer events
    /// (cursor movements, text edits, etc.). It handles batching for multi-cursor,
    /// position history tracking, and editing permission checks.
    pub(super) fn apply_action_as_events(&mut self, action: Action) -> AnyhowResult<()> {
        // Check if active buffer is a composite buffer - handle scroll/movement specially
        let buffer_id = self.active_buffer();
        if self.is_composite_buffer(buffer_id) {
            if let Some(_handled) = self.handle_composite_action(buffer_id, &action) {
                return Ok(());
            }
        }

        // Get description before moving action
        let action_description = format!("{:?}", action);

        // Check if this is an editing action and editing is disabled
        let is_editing_action = matches!(
            action,
            Action::InsertNewline
                | Action::InsertTab
                | Action::DeleteForward
                | Action::DeleteWordBackward
                | Action::DeleteWordForward
                | Action::DeleteLine
                | Action::DuplicateLine
                | Action::MoveLineUp
                | Action::MoveLineDown
                | Action::DedentSelection
                | Action::ToggleComment
        );

        if is_editing_action && self.is_editing_disabled() {
            self.set_status_message(t!("buffer.editing_disabled").to_string());
            return Ok(());
        }

        if let Some(events) = self.action_to_events(action) {
            if events.len() > 1 {
                // Check if this batch contains buffer modifications
                let has_buffer_mods = events
                    .iter()
                    .any(|e| matches!(e, Event::Insert { .. } | Event::Delete { .. }));

                if has_buffer_mods {
                    // Multi-cursor buffer edit: use optimized bulk edit (O(n) instead of O(n²))
                    if let Some(bulk_edit) =
                        self.apply_events_as_bulk_edit(events.clone(), action_description)
                    {
                        self.active_event_log_mut().append(bulk_edit);
                    }
                } else {
                    // Multi-cursor non-buffer operation: use Batch for atomic undo
                    let batch = Event::Batch {
                        events: events.clone(),
                        description: action_description,
                    };
                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);
                }

                // Track position history for all events
                for event in &events {
                    self.track_cursor_movement(event);
                }
            } else {
                // Single cursor - apply normally
                for event in events {
                    self.log_and_apply_event(&event);
                    self.track_cursor_movement(&event);
                }
            }
        }

        Ok(())
    }

    /// Track cursor movement in position history if applicable.
    pub(super) fn track_cursor_movement(&mut self, event: &Event) {
        if self.in_navigation {
            return;
        }

        if let Event::MoveCursor {
            new_position,
            new_anchor,
            ..
        } = event
        {
            self.position_history
                .record_movement(self.active_buffer(), *new_position, *new_anchor);
        }
    }

    /// Route a key event through the CompositeInputRouter for a composite
    /// buffer.  Returns `Some(Ok(()))` if the event was handled (or blocked),
    /// `None` if the router returned `Unhandled` (let fallthrough continue).
    pub(super) fn try_route_composite_key(
        &mut self,
        split_id: crate::model::event::LeafId,
        buffer_id: crate::model::event::BufferId,
        key_event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<()>> {
        use crate::input::composite_router::{
            CompositeInputRouter, Direction, RoutedEvent, ScrollAction,
        };

        let composite = self.composite_buffers.get(&buffer_id)?;
        let view_state = self.composite_view_states.get(&(split_id, buffer_id))?;

        match CompositeInputRouter::route_key_event(composite, view_state, key_event) {
            RoutedEvent::Unhandled => None,

            RoutedEvent::CompositeScroll(action) => {
                let delta = match action {
                    ScrollAction::Up(n) => -(n as isize),
                    ScrollAction::Down(n) => n as isize,
                    _ => return Some(Ok(())),
                };
                self.composite_scroll(split_id, buffer_id, delta);
                Some(Ok(()))
            }

            RoutedEvent::SwitchPane(dir) => {
                match dir {
                    Direction::Next => self.composite_focus_next(split_id, buffer_id),
                    Direction::Prev => self.composite_focus_prev(split_id, buffer_id),
                }
                Some(Ok(()))
            }

            // Anything else the router might return — let normal dispatch handle it
            _ => None,
        }
    }
}
