//! Undo and redo action handlers.

use super::Editor;

impl Editor {
    /// Handle Undo action - revert the last edit operation.
    pub fn handle_undo(&mut self) {
        if self.is_editing_disabled() {
            self.set_status_message("Editing disabled in this buffer".to_string());
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

        // Apply all inverse events collected during undo
        for event in &events {
            tracing::debug!("Undo applying event: {:?}", event);
            self.apply_event_to_active_buffer(event);
        }

        // Update modified status based on event log position
        self.update_modified_from_event_log();
    }

    /// Handle Redo action - reapply an undone edit operation.
    pub fn handle_redo(&mut self) {
        if self.is_editing_disabled() {
            self.set_status_message("Editing disabled in this buffer".to_string());
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
