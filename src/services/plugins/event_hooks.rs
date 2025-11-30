//! Event-to-Hook Mapping
//!
//! This module maps editor Events to Hook invocations automatically.
//! This ensures hooks are triggered consistently whenever state changes occur.

use crate::model::event::{BufferId, Event};
use crate::services::plugins::hooks::{HookArgs, HookRegistry};
use std::sync::RwLock;

/// Trait for converting Events into Hook invocations
pub trait EventHooks {
    /// Get the "before" hook args for this event (if any)
    fn before_hook(&self, buffer_id: BufferId) -> Option<HookArgs>;

    /// Get the "after" hook args for this event (if any)
    fn after_hook(&self, buffer_id: BufferId) -> Option<HookArgs>;
}

impl EventHooks for Event {
    fn before_hook(&self, buffer_id: BufferId) -> Option<HookArgs> {
        match self {
            Event::Insert {
                position,
                text,
                cursor_id: _,
            } => Some(HookArgs::BeforeInsert {
                buffer_id,
                position: *position,
                text: text.clone(),
            }),
            Event::Delete { range, .. } => Some(HookArgs::BeforeDelete {
                buffer_id,
                range: range.clone(),
            }),
            _ => None, // Most events don't have "before" hooks
        }
    }

    fn after_hook(&self, buffer_id: BufferId) -> Option<HookArgs> {
        match self {
            Event::Insert {
                position,
                text,
                cursor_id: _,
            } => Some(HookArgs::AfterInsert {
                buffer_id,
                position: *position,
                text: text.clone(),
                affected_start: *position,
                affected_end: *position + text.len(),
                // Line info placeholder - will be filled by caller with buffer access
                start_line: 0,
                end_line: 0,
                lines_added: 0,
            }),
            Event::Delete {
                range,
                deleted_text,
                ..
            } => Some(HookArgs::AfterDelete {
                buffer_id,
                range: range.clone(),
                deleted_text: deleted_text.clone(),
                affected_start: range.start,
                deleted_len: deleted_text.len(),
                // Line info placeholder - will be filled by caller with buffer access
                start_line: 0,
                end_line: 0,
                lines_removed: 0,
            }),
            Event::MoveCursor {
                cursor_id,
                old_position,
                new_position,
                ..
            } => Some(HookArgs::CursorMoved {
                buffer_id,
                cursor_id: *cursor_id,
                old_position: *old_position,
                new_position: *new_position,
            }),
            _ => None,
        }
    }
}

/// Apply an event with automatic hook invocations
pub fn apply_event_with_hooks(
    state: &mut crate::state::EditorState,
    event: &Event,
    buffer_id: BufferId,
    hook_registry: &RwLock<HookRegistry>,
) -> bool {
    // Run "before" hooks
    if let Some(before_args) = event.before_hook(buffer_id) {
        let registry = hook_registry.read().unwrap();
        let hook_name = match &before_args {
            HookArgs::BeforeInsert { .. } => "before-insert",
            HookArgs::BeforeDelete { .. } => "before-delete",
            _ => "",
        };

        if !hook_name.is_empty() {
            let should_continue = registry.run_hooks(hook_name, &before_args);
            if !should_continue {
                // Hook cancelled the operation
                return false;
            }
        }
    }

    // Apply the event
    state.apply(event);

    // Run "after" hooks
    if let Some(after_args) = event.after_hook(buffer_id) {
        let registry = hook_registry.read().unwrap();
        let hook_name = match &after_args {
            HookArgs::AfterInsert { .. } => "after-insert",
            HookArgs::AfterDelete { .. } => "after-delete",
            HookArgs::CursorMoved { .. } => "cursor-moved",
            _ => "",
        };

        if !hook_name.is_empty() {
            registry.run_hooks(hook_name, &after_args);
        }
    }

    true // Event was applied
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::CursorId;
    use crate::services::plugins::hooks::HookRegistry;

    #[test]
    fn test_insert_event_has_hooks() {
        let event = Event::Insert {
            position: 0,
            text: "test".to_string(),
            cursor_id: CursorId(0),
        };

        let buffer_id = BufferId(1);

        // Should have both before and after hooks
        assert!(event.before_hook(buffer_id).is_some());
        assert!(event.after_hook(buffer_id).is_some());
    }

    #[test]
    fn test_delete_event_has_hooks() {
        let event = Event::Delete {
            range: 0..5,
            deleted_text: "test".to_string(),
            cursor_id: CursorId(0),
        };

        let buffer_id = BufferId(1);

        assert!(event.before_hook(buffer_id).is_some());
        assert!(event.after_hook(buffer_id).is_some());
    }

    #[test]
    fn test_overlay_event_no_hooks() {
        let event = Event::AddOverlay {
            namespace: Some(crate::view::overlay::OverlayNamespace::from_string(
                "test".to_string(),
            )),
            range: 0..5,
            face: crate::model::event::OverlayFace::Background { color: (255, 0, 0) },
            priority: 10,
            message: None,
        };

        let buffer_id = BufferId(1);

        // Overlay events don't trigger hooks (they're visual only)
        assert!(event.before_hook(buffer_id).is_none());
        assert!(event.after_hook(buffer_id).is_none());
    }

    #[test]
    fn test_hooks_can_cancel() {
        use crate::state::EditorState;
        use std::sync::RwLock;

        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);
        let hook_registry = RwLock::new(HookRegistry::new());

        // Register a hook that cancels the operation
        {
            let mut registry = hook_registry.write().unwrap();
            registry.add_hook("before-insert", Box::new(|_| false)); // Return false to cancel
        }

        let event = Event::Insert {
            position: 0,
            text: "test".to_string(),
            cursor_id: CursorId(0),
        };

        let buffer_id = BufferId(0);
        let was_applied = apply_event_with_hooks(&mut state, &event, buffer_id, &hook_registry);

        // Event should have been cancelled
        assert!(!was_applied);
        assert_eq!(state.buffer.len(), 0); // Buffer should still be empty
    }

    #[test]
    fn test_hooks_allow_event() {
        use crate::state::EditorState;
        use std::sync::RwLock;

        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);
        let hook_registry = RwLock::new(HookRegistry::new());

        // Register a hook that allows the operation
        {
            let mut registry = hook_registry.write().unwrap();
            registry.add_hook("before-insert", Box::new(|_| true)); // Return true to allow
        }

        let event = Event::Insert {
            position: 0,
            text: "test".to_string(),
            cursor_id: CursorId(0),
        };

        let buffer_id = BufferId(0);
        let was_applied = apply_event_with_hooks(&mut state, &event, buffer_id, &hook_registry);

        // Event should have been applied
        assert!(was_applied);
        assert_eq!(state.buffer.to_string().unwrap(), "test");
    }
}
