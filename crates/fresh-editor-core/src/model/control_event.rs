//! Control Events - Observable notifications about editor state changes
//!
//! Simple, elegant event system:
//! - Events are just (name, data) pairs
//! - Names are namespaced strings: "editor:file_saved", "lsp:status_changed", "plugin:git:branch_changed"
//! - Data is arbitrary JSON
//! - Plugins emit events, editor emits events, anyone can listen
//!
//! IMPORTANT: All core events must be registered in EventRegistry for documentation and validation.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

/// Authoritative registry of all core events.
/// This is the single source of truth for event names and their data schemas.
/// Plugin events (plugin:*) are exempt from this registry.
pub mod events {
    use serde_json::{json, Value};

    /// Event definition with name and data schema
    pub struct EventDef {
        pub name: &'static str,
        pub description: &'static str,
        /// Function that returns the data schema (can't use json! in const)
        pub data_schema_fn: fn() -> Value,
    }

    // ===== Editor Events =====

    pub const FILE_OPENED: EventDef = EventDef {
        name: "editor:file_opened",
        description: "File opened in editor",
        data_schema_fn: || json!({"path": "string", "buffer_id": "number"}),
    };

    pub const FILE_SAVED: EventDef = EventDef {
        name: "editor:file_saved",
        description: "File saved to disk",
        data_schema_fn: || json!({"path": "string"}),
    };

    // ===== LSP Events =====

    pub const LSP_STATUS_CHANGED: EventDef = EventDef {
        name: "lsp:status_changed",
        description: "LSP server status changed",
        data_schema_fn: || json!({"language": "string", "old_status": "string", "status": "string"}),
    };

    /// Get all registered events (for schema generation)
    pub fn all_events() -> Vec<&'static EventDef> {
        vec![&FILE_OPENED, &FILE_SAVED, &LSP_STATUS_CHANGED]
    }

    /// Get schema for all events as JSON
    pub fn schema() -> Value {
        let mut events = serde_json::Map::new();
        for event in all_events() {
            events.insert(
                event.name.to_string(),
                json!({
                    "description": event.description,
                    "data": (event.data_schema_fn)()
                }),
            );
        }
        Value::Object(events)
    }
}

/// A single control event - just a name and some data
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ControlEvent {
    /// Event name (namespaced): "editor:file_saved", "lsp:ready", "plugin:git:status"
    pub name: String,
    /// Arbitrary JSON data
    pub data: Value,
}

impl ControlEvent {
    /// Create a new event
    pub fn new(name: impl Into<String>, data: Value) -> Self {
        Self {
            name: name.into(),
            data,
        }
    }

    /// Create an event with no data
    pub fn simple(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            data: Value::Null,
        }
    }

    /// Check if event name matches a pattern
    /// Patterns can use "*" as wildcard: "lsp:*", "plugin:git:*", "*:error"
    pub fn matches(&self, pattern: &str) -> bool {
        if pattern == "*" {
            return true;
        }

        if !pattern.contains('*') {
            return self.name == pattern;
        }

        // Simple glob matching
        let parts: Vec<&str> = pattern.split('*').collect();
        if parts.len() == 2 {
            let (prefix, suffix) = (parts[0], parts[1]);
            self.name.starts_with(prefix) && self.name.ends_with(suffix)
        } else {
            // More complex patterns - just do exact match for now
            self.name == pattern
        }
    }

    /// Check if data contains expected values (shallow match)
    pub fn data_matches(&self, expected: &Value) -> bool {
        match (expected, &self.data) {
            (Value::Null, _) => true, // Null pattern matches anything
            (Value::Object(exp_map), Value::Object(data_map)) => {
                // All expected keys must match
                exp_map.iter().all(|(k, v)| {
                    data_map.get(k).is_some_and(|data_v| {
                        if v.is_null() {
                            true // Null in pattern means "key exists, any value"
                        } else {
                            v == data_v
                        }
                    })
                })
            }
            _ => expected == &self.data,
        }
    }
}

/// Broadcasts events to subscribers
#[derive(Clone)]
pub struct EventBroadcaster {
    events: Arc<Mutex<VecDeque<ControlEvent>>>,
    max_history: usize,
}

impl EventBroadcaster {
    pub fn new(max_history: usize) -> Self {
        Self {
            events: Arc::new(Mutex::new(VecDeque::with_capacity(max_history))),
            max_history,
        }
    }

    /// Emit an event
    pub fn emit(&self, event: ControlEvent) {
        let mut events = self.events.lock().unwrap();
        if events.len() >= self.max_history {
            events.pop_front();
        }
        events.push_back(event);
    }

    /// Convenience: emit with name and data
    pub fn emit_named(&self, name: impl Into<String>, data: Value) {
        self.emit(ControlEvent::new(name, data));
    }

    /// Convenience: emit simple event (no data)
    pub fn emit_simple(&self, name: impl Into<String>) {
        self.emit(ControlEvent::simple(name));
    }

    /// Check if any event matches pattern
    pub fn has_match(&self, name_pattern: &str, data_pattern: &Value) -> bool {
        let events = self.events.lock().unwrap();
        events
            .iter()
            .any(|e| e.matches(name_pattern) && e.data_matches(data_pattern))
    }

    /// Take first event matching pattern (removes it and all events before it)
    pub fn take_match(&self, name_pattern: &str, data_pattern: &Value) -> Option<ControlEvent> {
        let mut events = self.events.lock().unwrap();
        let pos = events
            .iter()
            .position(|e| e.matches(name_pattern) && e.data_matches(data_pattern));

        if let Some(idx) = pos {
            let event = events.get(idx).cloned();
            events.drain(..=idx);
            event
        } else {
            None
        }
    }

    /// Drain all events
    pub fn drain(&self) -> Vec<ControlEvent> {
        let mut events = self.events.lock().unwrap();
        events.drain(..).collect()
    }

    /// Peek at all events
    pub fn peek(&self) -> Vec<ControlEvent> {
        let events = self.events.lock().unwrap();
        events.iter().cloned().collect()
    }

    /// Clear all events
    pub fn clear(&self) {
        let mut events = self.events.lock().unwrap();
        events.clear();
    }

    /// Number of pending events
    pub fn len(&self) -> usize {
        let events = self.events.lock().unwrap();
        events.len()
    }

    /// Is empty?
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Default for EventBroadcaster {
    fn default() -> Self {
        Self::new(1000)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_event_matching() {
        let event = ControlEvent::new(
            "lsp:status_changed",
            json!({"language": "rust", "status": "running"}),
        );

        assert!(event.matches("lsp:status_changed"));
        assert!(event.matches("lsp:*"));
        assert!(event.matches("*:status_changed"));
        assert!(event.matches("*"));
        assert!(!event.matches("lsp:error"));
        assert!(!event.matches("editor:*"));
    }

    #[test]
    fn test_data_matching() {
        let event = ControlEvent::new("test", json!({"a": 1, "b": "hello", "c": true}));

        // Exact match
        assert!(event.data_matches(&json!({"a": 1})));
        assert!(event.data_matches(&json!({"b": "hello"})));
        assert!(event.data_matches(&json!({"a": 1, "b": "hello"})));

        // Null means "any"
        assert!(event.data_matches(&json!({"a": null})));
        assert!(event.data_matches(&Value::Null));

        // Mismatch
        assert!(!event.data_matches(&json!({"a": 2})));
        assert!(!event.data_matches(&json!({"d": 1})));
    }

    #[test]
    fn test_broadcaster() {
        let bc = EventBroadcaster::new(10);

        bc.emit_simple("editor:init");
        bc.emit_named(
            "lsp:status_changed",
            json!({"language": "rust", "status": "starting"}),
        );
        bc.emit_named(
            "lsp:status_changed",
            json!({"language": "rust", "status": "running"}),
        );

        assert_eq!(bc.len(), 3);

        // Find LSP running event
        assert!(bc.has_match("lsp:status_changed", &json!({"status": "running"})));

        // Take it
        let event = bc.take_match("lsp:status_changed", &json!({"status": "running"}));
        assert!(event.is_some());
        assert_eq!(event.unwrap().data["status"], "running");

        // Only one event left (after the match)
        assert_eq!(bc.len(), 0);
    }

    #[test]
    fn test_max_history() {
        let bc = EventBroadcaster::new(2);

        bc.emit_simple("a");
        bc.emit_simple("b");
        bc.emit_simple("c");

        let events = bc.drain();
        assert_eq!(events.len(), 2);
        assert_eq!(events[0].name, "b");
        assert_eq!(events[1].name, "c");
    }
}
