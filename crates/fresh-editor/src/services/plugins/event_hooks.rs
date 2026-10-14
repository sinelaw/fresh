//! Event-to-Hook Mapping
//!
//! This module maps editor Events to Hook invocations automatically.
//! This ensures hooks are triggered consistently whenever state changes occur.

use crate::model::event::Event;
use crate::services::plugins::hooks::HookArgs;
use fresh_core::BufferId;

/// Trait for converting Events into Hook invocations
pub trait EventHooks {
    /// Get the "before" hook args for this event (if any)
    fn before_hook(&self, buffer_id: BufferId, window_id: u64) -> Option<HookArgs>;

    /// Get the "after" hook args for this event (if any)
    fn after_hook(&self, buffer_id: BufferId, window_id: u64) -> Option<HookArgs>;
}

impl EventHooks for Event {
    fn before_hook(&self, buffer_id: BufferId, window_id: u64) -> Option<HookArgs> {
        match self {
            Self::Insert {
                position,
                text,
                cursor_id: _,
            } => Some(HookArgs::BeforeInsert {
                buffer_id,
                window_id,
                position: *position,
                text: text.clone(),
            }),
            Self::Delete { range, .. } => Some(HookArgs::BeforeDelete {
                buffer_id,
                window_id,
                start: range.start,
                end: range.end,
            }),
            _ => None, // Most events don't have "before" hooks
        }
    }

    fn after_hook(&self, buffer_id: BufferId, window_id: u64) -> Option<HookArgs> {
        match self {
            Self::Insert {
                position,
                text,
                cursor_id: _,
            } => Some(HookArgs::AfterInsert {
                buffer_id,
                window_id,
                position: *position,
                text: text.clone(),
                affected_start: *position,
                affected_end: *position + text.len(),
                // Line info placeholder - will be filled by caller with buffer access
                start_line: 0,
                end_line: 0,
                lines_added: 0,
            }),
            Self::Delete {
                range,
                deleted_text,
                ..
            } => Some(HookArgs::AfterDelete {
                buffer_id,
                window_id,
                start: range.start,
                end: range.end,
                deleted_text: deleted_text.clone(),
                affected_start: range.start,
                deleted_len: deleted_text.len(),
                // Line info placeholder - will be filled by caller with buffer access
                start_line: 0,
                end_line: 0,
                lines_removed: 0,
            }),
            Self::MoveCursor {
                cursor_id,
                old_position,
                new_position,
                ..
            } => Some(HookArgs::CursorMoved {
                buffer_id,
                window_id,
                cursor_id: *cursor_id,
                old_position: *old_position,
                new_position: *new_position,
                // Placeholders - will be filled by caller with buffer access
                line: 0,
                text_properties: Vec::new(),
            }),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use fresh_core::CursorId;

    #[test]
    fn test_insert_event_has_hooks() {
        let event = Event::Insert {
            position: 0,
            text: "test".to_string(),
            cursor_id: CursorId(0),
        };

        let buffer_id = BufferId(1);

        // Should have both before and after hooks
        assert!(event.before_hook(buffer_id, 1).is_some());
        assert!(event.after_hook(buffer_id, 1).is_some());
    }

    #[test]
    fn test_delete_event_has_hooks() {
        let event = Event::Delete {
            range: 0..5,
            deleted_text: "test".to_string(),
            cursor_id: CursorId(0),
        };

        let buffer_id = BufferId(1);

        assert!(event.before_hook(buffer_id, 1).is_some());
        assert!(event.after_hook(buffer_id, 1).is_some());
    }

    #[test]
    fn test_overlay_event_no_hooks() {
        let event = Event::AddOverlay {
            range: 0..5,
            face: crate::model::event::OverlayFace::Background { color: (255, 0, 0) },
            priority: 10,
            message: None,
            extend_to_line_end: false,
            namespace: None,
            url: None,
        };

        let buffer_id = BufferId(1);

        // Overlay events don't trigger hooks (they're visual only)
        assert!(event.before_hook(buffer_id, 1).is_none());
        assert!(event.after_hook(buffer_id, 1).is_none());
    }
}
