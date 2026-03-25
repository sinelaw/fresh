//! Undo and redo action handlers.

use super::Editor;
use rust_i18n::t;

impl Editor {
    /// Handle Undo action - revert the last edit operation.
    pub fn handle_undo(&mut self) {
        if self.is_editing_disabled() {
            self.set_status_message(t!("buffer.editing_disabled").to_string());
            return;
        }

        let event_log = self.active_event_log_mut();
        let before_idx = event_log.current_index();
        let can_undo = event_log.can_undo();
        let events = event_log.undo();
        let after_idx = self.active_event_log().current_index();

        tracing::debug!(
            "Undo: before_idx={}, after_idx={}, can_undo={}, events_count={}",
            before_idx,
            after_idx,
            can_undo,
            events.len()
        );

        // Apply all inverse events collected during undo.
        // Each event may carry displaced markers that need restoration after apply.
        for (event, displaced_markers) in &events {
            tracing::debug!("Undo applying event: {:?}", event);
            self.apply_event_to_active_buffer(event);

            // Restore displaced markers to their exact original positions.
            // These were captured when the original Delete was logged.
            // Skip for BulkEdit events — they handle displaced markers internally
            // in state.apply(BulkEdit) via the Event's own displaced_markers field.
            if !displaced_markers.is_empty()
                && !matches!(event, crate::model::event::Event::BulkEdit { .. })
            {
                let state = self.active_state_mut();
                for &(tagged_id, original_pos) in displaced_markers {
                    let is_margin = (tagged_id >> 63) == 1;
                    let raw_id = tagged_id & !(1u64 << 63);
                    let marker_id = crate::model::marker::MarkerId(raw_id);
                    if is_margin {
                        state
                            .margins
                            .set_indicator_position(marker_id, original_pos);
                    } else {
                        state.marker_list.set_position(marker_id, original_pos);
                    }
                }
            }
        }

        // Update modified status based on event log position
        self.update_modified_from_event_log();
    }

    /// Handle Redo action - reapply an undone edit operation.
    pub fn handle_redo(&mut self) {
        if self.is_editing_disabled() {
            self.set_status_message(t!("buffer.editing_disabled").to_string());
            return;
        }

        let events = self.active_event_log_mut().redo();

        // Apply all events collected during redo
        for event in events {
            self.apply_event_to_active_buffer(&event);
        }

        // Update modified status based on event log position
        self.update_modified_from_event_log();
    }
}
