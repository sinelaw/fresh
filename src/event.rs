use std::ops::Range;
use serde::{Deserialize, Serialize};

/// Unique identifier for a cursor
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CursorId(pub usize);

/// Core event types representing all possible state changes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Event {
    /// Insert text at a position
    Insert {
        position: usize,
        text: String,
        cursor_id: CursorId,
    },

    /// Delete a range of text
    Delete {
        range: Range<usize>,
        deleted_text: String,
        cursor_id: CursorId,
    },

    /// Move a cursor to a new position
    MoveCursor {
        cursor_id: CursorId,
        position: usize,
        anchor: Option<usize>,
    },

    /// Add a new cursor
    AddCursor {
        cursor_id: CursorId,
        position: usize,
        anchor: Option<usize>,
    },

    /// Remove a cursor
    RemoveCursor {
        cursor_id: CursorId,
    },

    /// Scroll the viewport
    Scroll {
        line_offset: isize,
    },

    /// Set viewport to specific position
    SetViewport {
        top_line: usize,
    },

    /// Change mode (if implementing modal editing)
    ChangeMode {
        mode: String,
    },
}

impl Event {
    /// Returns the inverse event for undo functionality
    pub fn inverse(&self) -> Option<Event> {
        match self {
            Event::Insert { position, text, cursor_id } => {
                let range = *position..(position + text.len());
                Some(Event::Delete {
                    range,
                    deleted_text: text.clone(),
                    cursor_id: *cursor_id,
                })
            }
            Event::Delete { range, deleted_text, cursor_id } => {
                Some(Event::Insert {
                    position: range.start,
                    text: deleted_text.clone(),
                    cursor_id: *cursor_id,
                })
            }
            // MoveCursor, AddCursor, RemoveCursor are not automatically invertible
            // They would need to store the previous state
            _ => None,
        }
    }

    /// Returns true if this event modifies the buffer content
    pub fn modifies_buffer(&self) -> bool {
        matches!(self, Event::Insert { .. } | Event::Delete { .. })
    }

    /// Returns the cursor ID associated with this event, if any
    pub fn cursor_id(&self) -> Option<CursorId> {
        match self {
            Event::Insert { cursor_id, .. }
            | Event::Delete { cursor_id, .. }
            | Event::MoveCursor { cursor_id, .. }
            | Event::AddCursor { cursor_id, .. }
            | Event::RemoveCursor { cursor_id } => Some(*cursor_id),
            _ => None,
        }
    }
}

/// A log entry containing an event and metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogEntry {
    /// The event
    pub event: Event,

    /// Timestamp when the event occurred (milliseconds since epoch)
    pub timestamp: u64,

    /// Optional description for debugging
    pub description: Option<String>,
}

impl LogEntry {
    pub fn new(event: Event) -> Self {
        Self {
            event,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_millis() as u64,
            description: None,
        }
    }

    pub fn with_description(mut self, description: String) -> Self {
        self.description = Some(description);
        self
    }
}

/// Snapshot of editor state for fast undo/redo
#[derive(Debug, Clone)]
pub struct Snapshot {
    /// Index in the event log where this snapshot was taken
    pub log_index: usize,

    /// Buffer content at this point (stored as ChunkTree reference)
    /// For now we'll use a placeholder - will be filled in when we implement Buffer
    pub buffer_state: (),

    /// Cursor positions at this point
    pub cursor_positions: Vec<(CursorId, usize, Option<usize>)>,
}

/// The event log - append-only log of all events
pub struct EventLog {
    /// All logged events
    entries: Vec<LogEntry>,

    /// Current position in the log (for undo/redo)
    current_index: usize,

    /// Periodic snapshots for fast seeking
    snapshots: Vec<Snapshot>,

    /// How often to create snapshots (every N events)
    snapshot_interval: usize,
}

impl EventLog {
    /// Create a new empty event log
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            current_index: 0,
            snapshots: Vec::new(),
            snapshot_interval: 100,
        }
    }

    /// Append an event to the log
    pub fn append(&mut self, event: Event) -> usize {
        // If we're not at the end, truncate future events
        if self.current_index < self.entries.len() {
            self.entries.truncate(self.current_index);
        }

        let entry = LogEntry::new(event);
        self.entries.push(entry);
        self.current_index = self.entries.len();

        // Check if we should create a snapshot
        if self.entries.len() % self.snapshot_interval == 0 {
            // Snapshot creation will be implemented when we have Buffer
            // For now, just track that we'd create one here
        }

        self.current_index - 1
    }

    /// Get the current event index
    pub fn current_index(&self) -> usize {
        self.current_index
    }

    /// Can we undo?
    pub fn can_undo(&self) -> bool {
        self.current_index > 0
    }

    /// Can we redo?
    pub fn can_redo(&self) -> bool {
        self.current_index < self.entries.len()
    }

    /// Move back one event (for undo)
    pub fn undo(&mut self) -> Option<&Event> {
        if self.can_undo() {
            self.current_index -= 1;
            Some(&self.entries[self.current_index].event)
        } else {
            None
        }
    }

    /// Move forward one event (for redo)
    pub fn redo(&mut self) -> Option<&Event> {
        if self.can_redo() {
            let event = &self.entries[self.current_index].event;
            self.current_index += 1;
            Some(event)
        } else {
            None
        }
    }

    /// Get all events from the log
    pub fn entries(&self) -> &[LogEntry] {
        &self.entries
    }

    /// Get events in a range
    pub fn range(&self, range: Range<usize>) -> &[LogEntry] {
        &self.entries[range]
    }

    /// Get the most recent event
    pub fn last_event(&self) -> Option<&Event> {
        if self.current_index > 0 {
            Some(&self.entries[self.current_index - 1].event)
        } else {
            None
        }
    }

    /// Clear all events (for testing or reset)
    pub fn clear(&mut self) {
        self.entries.clear();
        self.current_index = 0;
        self.snapshots.clear();
    }

    /// Save event log to JSON Lines format
    pub fn save_to_file(&self, path: &std::path::Path) -> std::io::Result<()> {
        use std::io::Write;
        let file = std::fs::File::create(path)?;
        let mut writer = std::io::BufWriter::new(file);

        for entry in &self.entries {
            let json = serde_json::to_string(entry)?;
            writeln!(writer, "{}", json)?;
        }

        Ok(())
    }

    /// Load event log from JSON Lines format
    pub fn load_from_file(path: &std::path::Path) -> std::io::Result<Self> {
        use std::io::BufRead;
        let file = std::fs::File::open(path)?;
        let reader = std::io::BufReader::new(file);

        let mut log = Self::new();

        for line in reader.lines() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }
            let entry: LogEntry = serde_json::from_str(&line)?;
            log.entries.push(entry);
        }

        log.current_index = log.entries.len();

        Ok(log)
    }

    /// Set snapshot interval
    pub fn set_snapshot_interval(&mut self, interval: usize) {
        self.snapshot_interval = interval;
    }
}

impl Default for EventLog {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_log_append() {
        let mut log = EventLog::new();
        let event = Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id: CursorId(0),
        };

        let index = log.append(event);
        assert_eq!(index, 0);
        assert_eq!(log.current_index(), 1);
        assert_eq!(log.entries().len(), 1);
    }

    #[test]
    fn test_undo_redo() {
        let mut log = EventLog::new();

        log.append(Event::Insert {
            position: 0,
            text: "a".to_string(),
            cursor_id: CursorId(0),
        });

        log.append(Event::Insert {
            position: 1,
            text: "b".to_string(),
            cursor_id: CursorId(0),
        });

        assert_eq!(log.current_index(), 2);
        assert!(log.can_undo());
        assert!(!log.can_redo());

        log.undo();
        assert_eq!(log.current_index(), 1);
        assert!(log.can_undo());
        assert!(log.can_redo());

        log.undo();
        assert_eq!(log.current_index(), 0);
        assert!(!log.can_undo());
        assert!(log.can_redo());

        log.redo();
        assert_eq!(log.current_index(), 1);
    }

    #[test]
    fn test_event_inverse() {
        let insert = Event::Insert {
            position: 5,
            text: "hello".to_string(),
            cursor_id: CursorId(0),
        };

        let inverse = insert.inverse().unwrap();
        match inverse {
            Event::Delete { range, deleted_text, .. } => {
                assert_eq!(range, 5..10);
                assert_eq!(deleted_text, "hello");
            }
            _ => panic!("Expected Delete event"),
        }
    }

    #[test]
    fn test_truncate_on_new_event_after_undo() {
        let mut log = EventLog::new();

        log.append(Event::Insert {
            position: 0,
            text: "a".to_string(),
            cursor_id: CursorId(0),
        });

        log.append(Event::Insert {
            position: 1,
            text: "b".to_string(),
            cursor_id: CursorId(0),
        });

        log.undo();
        assert_eq!(log.entries().len(), 2);

        // Adding new event should truncate the future
        log.append(Event::Insert {
            position: 1,
            text: "c".to_string(),
            cursor_id: CursorId(0),
        });

        assert_eq!(log.entries().len(), 2);
        assert_eq!(log.current_index(), 2);
    }
}
