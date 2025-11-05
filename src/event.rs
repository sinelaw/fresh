use serde::{Deserialize, Serialize};
use std::ops::Range;

/// Unique identifier for a cursor
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CursorId(pub usize);

/// Unique identifier for a split pane (re-exported from split.rs)
/// Note: This is defined in split.rs and re-exported here for events
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SplitId(pub usize);

/// Unique identifier for a buffer (re-exported from editor.rs)
/// Note: This is defined in editor.rs and re-exported here for events
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BufferId(pub usize);

/// Direction of a split (re-exported from split.rs for events)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SplitDirection {
    Horizontal,
    Vertical,
}

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

    /// Remove a cursor (stores cursor state for undo)
    RemoveCursor {
        cursor_id: CursorId,
        position: usize,
        anchor: Option<usize>,
    },

    /// Scroll the viewport
    Scroll { line_offset: isize },

    /// Set viewport to specific position
    SetViewport { top_line: usize },

    /// Change mode (if implementing modal editing)
    ChangeMode { mode: String },

    /// Add an overlay (for decorations like underlines, highlights)
    AddOverlay {
        overlay_id: String,
        range: Range<usize>,
        face: OverlayFace,
        priority: i32,
        message: Option<String>,
    },

    /// Remove overlay by ID
    RemoveOverlay { overlay_id: String },

    /// Remove all overlays in a range
    RemoveOverlaysInRange { range: Range<usize> },

    /// Clear all overlays
    ClearOverlays,

    /// Show a popup
    ShowPopup { popup: PopupData },

    /// Hide the topmost popup
    HidePopup,

    /// Clear all popups
    ClearPopups,

    /// Navigate popup selection (for list popups)
    PopupSelectNext,
    PopupSelectPrev,
    PopupPageDown,
    PopupPageUp,

    /// Margin events
    /// Add a margin annotation
    AddMarginAnnotation {
        line: usize,
        position: MarginPositionData,
        content: MarginContentData,
        annotation_id: Option<String>,
    },

    /// Remove margin annotation by ID
    RemoveMarginAnnotation { annotation_id: String },

    /// Remove all margin annotations at a specific line
    RemoveMarginAnnotationsAtLine {
        line: usize,
        position: MarginPositionData,
    },

    /// Clear all margin annotations in a position
    ClearMarginPosition { position: MarginPositionData },

    /// Clear all margin annotations
    ClearMargins,

    /// Enable/disable line numbers
    SetLineNumbers { enabled: bool },

    /// Split view events
    /// Split the active pane
    SplitPane {
        direction: SplitDirection,
        new_buffer_id: BufferId,
        ratio: f32,
    },

    /// Close a split pane
    CloseSplit { split_id: SplitId },

    /// Set the active split pane
    SetActiveSplit { split_id: SplitId },

    /// Adjust the split ratio
    AdjustSplitRatio { split_id: SplitId, delta: f32 },

    /// Navigate to next split
    NextSplit,

    /// Navigate to previous split
    PrevSplit,

    /// Batch of events that should be undone/redone atomically
    /// Used for multi-cursor operations where all cursors perform the same action
    Batch {
        events: Vec<Event>,
        description: String,
    },
}

/// Overlay face data for events (must be serializable)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OverlayFace {
    Underline {
        color: (u8, u8, u8), // RGB color
        style: UnderlineStyle,
    },
    Background {
        color: (u8, u8, u8),
    },
    Foreground {
        color: (u8, u8, u8),
    },
}

/// Underline style for overlays
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnderlineStyle {
    Straight,
    Wavy,
    Dotted,
    Dashed,
}

/// Popup data for events (must be serializable)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupData {
    pub title: Option<String>,
    pub content: PopupContentData,
    pub position: PopupPositionData,
    pub width: u16,
    pub max_height: u16,
    pub bordered: bool,
}

/// Popup content for events
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PopupContentData {
    Text(Vec<String>),
    List {
        items: Vec<PopupListItemData>,
        selected: usize,
    },
}

/// Popup list item for events
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupListItemData {
    pub text: String,
    pub detail: Option<String>,
    pub icon: Option<String>,
    pub data: Option<String>,
}

/// Popup position for events
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PopupPositionData {
    AtCursor,
    BelowCursor,
    AboveCursor,
    Fixed { x: u16, y: u16 },
    Centered,
}

/// Margin position for events
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MarginPositionData {
    Left,
    Right,
}

/// Margin content for events
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MarginContentData {
    Text(String),
    Symbol {
        text: String,
        color: Option<(u8, u8, u8)>, // RGB color
    },
    Empty,
}

impl Event {
    /// Returns the inverse event for undo functionality
    pub fn inverse(&self) -> Option<Event> {
        match self {
            Event::Insert {
                position,
                text,
                cursor_id,
            } => {
                let range = *position..(position + text.len());
                Some(Event::Delete {
                    range,
                    deleted_text: text.clone(),
                    cursor_id: *cursor_id,
                })
            }
            Event::Delete {
                range,
                deleted_text,
                cursor_id,
            } => Some(Event::Insert {
                position: range.start,
                text: deleted_text.clone(),
                cursor_id: *cursor_id,
            }),
            Event::Batch { events, description } => {
                // Invert all events in the batch in reverse order
                let inverted: Option<Vec<Event>> = events
                    .iter()
                    .rev()
                    .map(|e| e.inverse())
                    .collect();

                inverted.map(|inverted_events| Event::Batch {
                    events: inverted_events,
                    description: format!("Undo: {}", description),
                })
            }
            Event::AddCursor {
                cursor_id,
                position,
                anchor,
            } => {
                // To undo adding a cursor, we remove it (store its state for redo)
                Some(Event::RemoveCursor {
                    cursor_id: *cursor_id,
                    position: *position,
                    anchor: *anchor,
                })
            }
            Event::RemoveCursor {
                cursor_id,
                position,
                anchor,
            } => {
                // To undo removing a cursor, we add it back
                Some(Event::AddCursor {
                    cursor_id: *cursor_id,
                    position: *position,
                    anchor: *anchor,
                })
            }
            Event::MoveCursor {
                cursor_id,
                position,
                anchor,
            } => {
                // We can't automatically invert MoveCursor without knowing the old position
                // This needs to store old_position and old_anchor
                None
            }
            // Other events are not automatically invertible
            _ => None,
        }
    }

    /// Returns true if this event modifies the buffer content
    pub fn modifies_buffer(&self) -> bool {
        match self {
            Event::Insert { .. } | Event::Delete { .. } => true,
            Event::Batch { events, .. } => events.iter().any(|e| e.modifies_buffer()),
            _ => false,
        }
    }

    /// Returns the cursor ID associated with this event, if any
    pub fn cursor_id(&self) -> Option<CursorId> {
        match self {
            Event::Insert { cursor_id, .. }
            | Event::Delete { cursor_id, .. }
            | Event::MoveCursor { cursor_id, .. }
            | Event::AddCursor { cursor_id, .. }
            | Event::RemoveCursor { cursor_id, .. } => Some(*cursor_id),
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

    /// Optional file for streaming events to disk
    stream_file: Option<std::fs::File>,
}

impl EventLog {
    /// Create a new empty event log
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            current_index: 0,
            snapshots: Vec::new(),
            snapshot_interval: 100,
            stream_file: None,
        }
    }

    /// Enable streaming events to a file
    pub fn enable_streaming<P: AsRef<std::path::Path>>(&mut self, path: P) -> std::io::Result<()> {
        use std::io::Write;

        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path)?;

        // Write header
        writeln!(file, "# Event Log Stream")?;
        writeln!(file, "# Started at: {}", chrono::Local::now())?;
        writeln!(file, "# Format: JSON Lines (one event per line)")?;
        writeln!(file, "#")?;

        self.stream_file = Some(file);
        Ok(())
    }

    /// Disable streaming
    pub fn disable_streaming(&mut self) {
        self.stream_file = None;
    }

    /// Log rendering state (for debugging)
    pub fn log_render_state(
        &mut self,
        cursor_pos: usize,
        screen_cursor_x: u16,
        screen_cursor_y: u16,
        buffer_len: usize,
    ) {
        if let Some(ref mut file) = self.stream_file {
            use std::io::Write;

            let render_info = serde_json::json!({
                "type": "render",
                "timestamp": chrono::Local::now().to_rfc3339(),
                "cursor_position": cursor_pos,
                "screen_cursor": {"x": screen_cursor_x, "y": screen_cursor_y},
                "buffer_length": buffer_len,
            });

            if let Err(e) = writeln!(file, "{render_info}") {
                eprintln!("Warning: Failed to write render info to stream: {e}");
            }
            if let Err(e) = file.flush() {
                eprintln!("Warning: Failed to flush event stream: {e}");
            }
        }
    }

    /// Log keystroke (for debugging)
    pub fn log_keystroke(&mut self, key_code: &str, modifiers: &str) {
        if let Some(ref mut file) = self.stream_file {
            use std::io::Write;

            let keystroke_info = serde_json::json!({
                "type": "keystroke",
                "timestamp": chrono::Local::now().to_rfc3339(),
                "key": key_code,
                "modifiers": modifiers,
            });

            if let Err(e) = writeln!(file, "{keystroke_info}") {
                eprintln!("Warning: Failed to write keystroke to stream: {e}");
            }
            if let Err(e) = file.flush() {
                eprintln!("Warning: Failed to flush event stream: {e}");
            }
        }
    }

    /// Append an event to the log
    pub fn append(&mut self, event: Event) -> usize {
        // If we're not at the end, truncate future events
        if self.current_index < self.entries.len() {
            self.entries.truncate(self.current_index);
        }

        // Stream event to file if enabled
        if let Some(ref mut file) = self.stream_file {
            use std::io::Write;

            let stream_entry = serde_json::json!({
                "index": self.entries.len(),
                "timestamp": chrono::Local::now().to_rfc3339(),
                "event": event,
            });

            // Write JSON line and flush immediately for real-time logging
            if let Err(e) = writeln!(file, "{stream_entry}") {
                eprintln!("Warning: Failed to write to event stream: {e}");
            }
            if let Err(e) = file.flush() {
                eprintln!("Warning: Failed to flush event stream: {e}");
            }
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
            writeln!(writer, "{json}")?;
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

    // Property-based tests
    #[cfg(test)]
    mod property_tests {
        use super::*;
        use proptest::prelude::*;

        /// Helper to generate random events
        fn arb_event() -> impl Strategy<Value = Event> {
            prop_oneof![
                // Insert events
                (0usize..1000, ".{1,50}").prop_map(|(pos, text)| Event::Insert {
                    position: pos,
                    text,
                    cursor_id: CursorId(0),
                }),
                // Delete events
                (0usize..1000, 1usize..50).prop_map(|(pos, len)| Event::Delete {
                    range: pos..pos + len,
                    deleted_text: "x".repeat(len),
                    cursor_id: CursorId(0),
                }),
            ]
        }

        proptest! {
            /// Event inverse should be truly inverse
            #[test]
            fn event_inverse_property(event in arb_event()) {
                if let Some(inverse) = event.inverse() {
                    // The inverse of an inverse should be the original
                    // (for commutative operations)
                    if let Some(double_inverse) = inverse.inverse() {
                        match (&event, &double_inverse) {
                            (Event::Insert { position: p1, text: t1, .. },
                             Event::Insert { position: p2, text: t2, .. }) => {
                                assert_eq!(p1, p2);
                                assert_eq!(t1, t2);
                            }
                            (Event::Delete { range: r1, deleted_text: dt1, .. },
                             Event::Delete { range: r2, deleted_text: dt2, .. }) => {
                                assert_eq!(r1, r2);
                                assert_eq!(dt1, dt2);
                            }
                            _ => {}
                        }
                    }
                }
            }

            /// Undo then redo should restore state
            #[test]
            fn undo_redo_inverse(events in prop::collection::vec(arb_event(), 1..20)) {
                let mut log = EventLog::new();

                // Append all events
                for event in &events {
                    log.append(event.clone());
                }

                let after_append = log.current_index();

                // Undo all
                let mut undo_count = 0;
                while log.can_undo() {
                    log.undo();
                    undo_count += 1;
                }

                assert_eq!(log.current_index(), 0);
                assert_eq!(undo_count, events.len());

                // Redo all
                let mut redo_count = 0;
                while log.can_redo() {
                    log.redo();
                    redo_count += 1;
                }

                assert_eq!(log.current_index(), after_append);
                assert_eq!(redo_count, events.len());
            }

            /// Appending after undo should truncate redo history
            #[test]
            fn append_after_undo_truncates(
                initial_events in prop::collection::vec(arb_event(), 2..10),
                new_event in arb_event()
            ) {
                let mut log = EventLog::new();

                for event in &initial_events {
                    log.append(event.clone());
                }

                // Undo at least one
                log.undo();
                let index_after_undo = log.current_index();

                // Append new event
                log.append(new_event);

                // Should not be able to redo past the new event
                assert_eq!(log.current_index(), index_after_undo + 1);
                assert!(!log.can_redo());
            }
        }
    }

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
            Event::Delete {
                range,
                deleted_text,
                ..
            } => {
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
