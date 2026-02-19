use crate::model::buffer::BufferSnapshot;
pub use fresh_core::api::{OverlayColorSpec, OverlayOptions};
pub use fresh_core::overlay::{OverlayHandle, OverlayNamespace};
pub use fresh_core::{BufferId, ContainerId, CursorId, LeafId, SplitDirection, SplitId};
use serde::{Deserialize, Serialize};
use std::ops::Range;
use std::sync::Arc;

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
        old_position: usize,
        new_position: usize,
        old_anchor: Option<usize>,
        new_anchor: Option<usize>,
        old_sticky_column: usize,
        new_sticky_column: usize,
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
    Scroll {
        line_offset: isize,
    },

    /// Set viewport to specific position
    SetViewport {
        top_line: usize,
    },

    /// Center the viewport on the cursor
    Recenter,

    /// Set the anchor (selection start) for a cursor
    SetAnchor {
        cursor_id: CursorId,
        position: usize,
    },

    /// Clear the anchor and reset deselect_on_move for a cursor
    /// Used to cancel Emacs mark mode
    ClearAnchor {
        cursor_id: CursorId,
    },

    /// Change mode (if implementing modal editing)
    ChangeMode {
        mode: String,
    },

    /// Add an overlay (for decorations like underlines, highlights)
    AddOverlay {
        namespace: Option<OverlayNamespace>,
        range: Range<usize>,
        face: OverlayFace,
        priority: i32,
        message: Option<String>,
        /// Whether to extend the overlay's background to the end of the visual line
        extend_to_line_end: bool,
        /// Optional URL for OSC 8 terminal hyperlinks
        url: Option<String>,
    },

    /// Remove overlay by handle
    RemoveOverlay {
        handle: OverlayHandle,
    },

    /// Remove all overlays in a range
    RemoveOverlaysInRange {
        range: Range<usize>,
    },

    /// Clear all overlays in a namespace
    ClearNamespace {
        namespace: OverlayNamespace,
    },

    /// Clear all overlays
    ClearOverlays,

    /// Show a popup
    ShowPopup {
        popup: PopupData,
    },

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
    RemoveMarginAnnotation {
        annotation_id: String,
    },

    /// Remove all margin annotations at a specific line
    RemoveMarginAnnotationsAtLine {
        line: usize,
        position: MarginPositionData,
    },

    /// Clear all margin annotations in a position
    ClearMarginPosition {
        position: MarginPositionData,
    },

    /// Clear all margin annotations
    ClearMargins,

    /// Enable/disable line numbers
    SetLineNumbers {
        enabled: bool,
    },

    /// Split view events
    /// Split the active pane
    SplitPane {
        direction: SplitDirection,
        new_buffer_id: BufferId,
        ratio: f32,
    },

    /// Close a split pane
    CloseSplit {
        split_id: SplitId,
    },

    /// Set the active split pane
    SetActiveSplit {
        split_id: SplitId,
    },

    /// Adjust the split ratio
    AdjustSplitRatio {
        split_id: SplitId,
        delta: f32,
    },

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

    /// Efficient bulk edit that stores tree snapshots for O(1) undo/redo
    /// Used for multi-cursor operations, toggle comment, indent/dedent, etc.
    /// This avoids O(n²) complexity by applying all edits in a single tree pass.
    ///
    /// Key insight: PieceTree uses Arc<PieceTreeNode> (persistent data structure),
    /// so storing trees for undo/redo is O(1) (Arc clone), not O(n) (content copy).
    BulkEdit {
        /// Buffer state before the edit (for undo)
        #[serde(skip)]
        old_snapshot: Option<Arc<BufferSnapshot>>,
        /// Buffer state after the edit (for redo)
        #[serde(skip)]
        new_snapshot: Option<Arc<BufferSnapshot>>,
        /// Cursor states before the edit
        old_cursors: Vec<(CursorId, usize, Option<usize>)>,
        /// Cursor states after the edit
        new_cursors: Vec<(CursorId, usize, Option<usize>)>,
        /// Human-readable description
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
    /// Full style with theme-aware colors
    ///
    /// Uses OverlayOptions which supports both RGB colors and theme keys.
    /// Theme keys are resolved at render time.
    Style {
        options: OverlayOptions,
    },
}

impl OverlayFace {
    /// Create an OverlayFace from OverlayOptions
    pub fn from_options(options: OverlayOptions) -> Self {
        OverlayFace::Style { options }
    }
}

/// Underline style for overlays
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnderlineStyle {
    Straight,
    Wavy,
    Dotted,
    Dashed,
}

/// What kind of popup this is — determines input handling behavior.
///
/// This replaces the old approach of inferring popup kind from the title string,
/// which broke when titles were translated to non-English locales.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum PopupKindHint {
    /// LSP completion popup - supports type-to-filter, Tab/Enter accept
    Completion,
    /// Generic list popup - navigate and select
    #[default]
    List,
    /// Generic text popup - read-only
    Text,
}

/// Popup data for events (must be serializable)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupData {
    /// Popup kind — determines input handling behavior.
    #[serde(default)]
    pub kind: PopupKindHint,
    pub title: Option<String>,
    /// Optional description text shown above the content
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub transient: bool,
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
    BottomRight,
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
    /// Uses UNDO_SENTINEL cursor_id to avoid moving the cursor during undo
    pub fn inverse(&self) -> Option<Self> {
        match self {
            Self::Insert { position, text, .. } => {
                let range = *position..(position + text.len());
                Some(Self::Delete {
                    range,
                    deleted_text: text.clone(),
                    cursor_id: CursorId::UNDO_SENTINEL,
                })
            }
            Self::Delete {
                range,
                deleted_text,
                ..
            } => Some(Self::Insert {
                position: range.start,
                text: deleted_text.clone(),
                cursor_id: CursorId::UNDO_SENTINEL,
            }),
            Self::Batch {
                events,
                description,
            } => {
                // Invert all events in the batch in reverse order
                let inverted: Option<Vec<Self>> =
                    events.iter().rev().map(|e| e.inverse()).collect();

                inverted.map(|inverted_events| Self::Batch {
                    events: inverted_events,
                    description: format!("Undo: {}", description),
                })
            }
            Self::AddCursor {
                cursor_id,
                position,
                anchor,
            } => {
                // To undo adding a cursor, we remove it (store its state for redo)
                Some(Self::RemoveCursor {
                    cursor_id: *cursor_id,
                    position: *position,
                    anchor: *anchor,
                })
            }
            Self::RemoveCursor {
                cursor_id,
                position,
                anchor,
            } => {
                // To undo removing a cursor, we add it back
                Some(Self::AddCursor {
                    cursor_id: *cursor_id,
                    position: *position,
                    anchor: *anchor,
                })
            }
            Self::MoveCursor {
                cursor_id,
                old_position,
                new_position,
                old_anchor,
                new_anchor,
                old_sticky_column,
                new_sticky_column,
            } => {
                // Invert by swapping old and new positions
                Some(Self::MoveCursor {
                    cursor_id: *cursor_id,
                    old_position: *new_position,
                    new_position: *old_position,
                    old_anchor: *new_anchor,
                    new_anchor: *old_anchor,
                    old_sticky_column: *new_sticky_column,
                    new_sticky_column: *old_sticky_column,
                })
            }
            Self::AddOverlay { .. } => {
                // Overlays are ephemeral decorations, not undoable
                None
            }
            Self::RemoveOverlay { .. } => {
                // Overlays are ephemeral decorations, not undoable
                None
            }
            Self::ClearNamespace { .. } => {
                // Overlays are ephemeral decorations, not undoable
                None
            }
            Self::Scroll { line_offset } => Some(Self::Scroll {
                line_offset: -line_offset,
            }),
            Self::SetViewport { top_line: _ } => {
                // Can't invert without knowing old top_line
                None
            }
            Self::ChangeMode { mode: _ } => {
                // Can't invert without knowing old mode
                None
            }
            Self::BulkEdit {
                old_snapshot,
                new_snapshot,
                old_cursors,
                new_cursors,
                description,
            } => {
                // Inverse swaps both snapshots and cursor states
                // For undo: old becomes new, new becomes old
                Some(Self::BulkEdit {
                    old_snapshot: new_snapshot.clone(),
                    new_snapshot: old_snapshot.clone(),
                    old_cursors: new_cursors.clone(),
                    new_cursors: old_cursors.clone(),
                    description: format!("Undo: {}", description),
                })
            }
            // Other events (popups, margins, splits, etc.) are not automatically invertible
            _ => None,
        }
    }

    /// Returns true if this event modifies the buffer content
    pub fn modifies_buffer(&self) -> bool {
        match self {
            Self::Insert { .. } | Self::Delete { .. } | Self::BulkEdit { .. } => true,
            Self::Batch { events, .. } => events.iter().any(|e| e.modifies_buffer()),
            _ => false,
        }
    }

    /// Returns true if this event is a write action (modifies state in a way that should be undoable)
    /// Returns false for readonly actions like cursor movement, scrolling, viewport changes, etc.
    ///
    /// Write actions include:
    /// - Buffer modifications (Insert, Delete)
    /// - Cursor structure changes (AddCursor, RemoveCursor)
    /// - Batches containing write actions
    ///
    /// Readonly actions include:
    /// - Cursor movement (MoveCursor)
    /// - Scrolling and viewport changes (Scroll, SetViewport)
    /// - UI events (overlays, popups, margins, mode changes, etc.)
    pub fn is_write_action(&self) -> bool {
        match self {
            // Buffer modifications are write actions
            Self::Insert { .. } | Self::Delete { .. } | Self::BulkEdit { .. } => true,

            // Adding/removing cursors are write actions (structural changes)
            Self::AddCursor { .. } | Self::RemoveCursor { .. } => true,

            // Batches are write actions if they contain any write actions
            Self::Batch { events, .. } => events.iter().any(|e| e.is_write_action()),

            // All other events are readonly (movement, scrolling, UI, etc.)
            _ => false,
        }
    }

    /// Returns the cursor ID associated with this event, if any
    pub fn cursor_id(&self) -> Option<CursorId> {
        match self {
            Self::Insert { cursor_id, .. }
            | Self::Delete { cursor_id, .. }
            | Self::MoveCursor { cursor_id, .. }
            | Self::AddCursor { cursor_id, .. }
            | Self::RemoveCursor { cursor_id, .. } => Some(*cursor_id),
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

    /// Optional file for streaming events to disk (runtime only)
    #[cfg(feature = "runtime")]
    stream_file: Option<std::fs::File>,

    /// Index at which the buffer was last saved (for tracking modified status)
    /// When current_index equals saved_at_index, the buffer is not modified
    saved_at_index: Option<usize>,
}

impl EventLog {
    /// Create a new empty event log
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            current_index: 0,
            snapshots: Vec::new(),
            snapshot_interval: 100,
            #[cfg(feature = "runtime")]
            stream_file: None,
            saved_at_index: Some(0), // New buffer starts at "saved" state (index 0)
        }
    }

    /// Mark the current position as the saved point
    /// Call this when the buffer is saved to disk
    pub fn mark_saved(&mut self) {
        self.saved_at_index = Some(self.current_index);
    }

    /// Check if the buffer is at the saved position (not modified)
    /// Returns true if we're at the saved position OR if all events between
    /// saved_at_index and current_index are readonly (don't modify buffer content)
    pub fn is_at_saved_position(&self) -> bool {
        match self.saved_at_index {
            None => false,
            Some(saved_idx) if saved_idx == self.current_index => true,
            Some(saved_idx) => {
                // Check if all events between saved position and current position
                // are readonly (don't modify buffer content)
                let (start, end) = if saved_idx < self.current_index {
                    (saved_idx, self.current_index)
                } else {
                    (self.current_index, saved_idx)
                };

                // All events in range [start, end) must be readonly
                self.entries[start..end]
                    .iter()
                    .all(|entry| !entry.event.modifies_buffer())
            }
        }
    }

    /// Enable streaming events to a file (runtime only)
    #[cfg(feature = "runtime")]
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

    /// Disable streaming (runtime only)
    #[cfg(feature = "runtime")]
    pub fn disable_streaming(&mut self) {
        self.stream_file = None;
    }

    /// Log rendering state (for debugging, runtime only)
    #[cfg(feature = "runtime")]
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
                tracing::trace!("Warning: Failed to write render info to stream: {e}");
            }
            if let Err(e) = file.flush() {
                tracing::trace!("Warning: Failed to flush event stream: {e}");
            }
        }
    }

    /// Log keystroke (for debugging, runtime only)
    #[cfg(feature = "runtime")]
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
                tracing::trace!("Warning: Failed to write keystroke to stream: {e}");
            }
            if let Err(e) = file.flush() {
                tracing::trace!("Warning: Failed to flush event stream: {e}");
            }
        }
    }

    /// Append an event to the log
    pub fn append(&mut self, event: Event) -> usize {
        // When redo history exists (after undo), only write actions are logged.
        // Non-write events (MoveCursor, Scroll, etc.) are still applied to the
        // editor state but not recorded in the log, preserving redo history.
        // This matches standard editor behavior (VS Code, Sublime, etc.) where
        // navigation after undo does not destroy the redo chain.
        if self.current_index < self.entries.len() {
            if event.is_write_action() {
                // Write action: truncate redo history and log normally
                self.entries.truncate(self.current_index);

                // Invalidate saved_at_index if it pointed to a truncated entry
                if let Some(saved_idx) = self.saved_at_index {
                    if saved_idx > self.current_index {
                        self.saved_at_index = None;
                    }
                }
            } else {
                // Non-write event while redo exists: skip logging to preserve redo
                return self.current_index;
            }
        }

        // Stream event to file if enabled (runtime only)
        #[cfg(feature = "runtime")]
        if let Some(ref mut file) = self.stream_file {
            use std::io::Write;

            let stream_entry = serde_json::json!({
                "index": self.entries.len(),
                "timestamp": chrono::Local::now().to_rfc3339(),
                "event": event,
            });

            // Write JSON line and flush immediately for real-time logging
            if let Err(e) = writeln!(file, "{stream_entry}") {
                tracing::trace!("Warning: Failed to write to event stream: {e}");
            }
            if let Err(e) = file.flush() {
                tracing::trace!("Warning: Failed to flush event stream: {e}");
            }
        }

        let entry = LogEntry::new(event);
        self.entries.push(entry);
        self.current_index = self.entries.len();

        // Check if we should create a snapshot
        if self.entries.len().is_multiple_of(self.snapshot_interval) {
            // Snapshot creation will be implemented when we have Buffer
            // For now, just track that we'd create one here
        }

        self.current_index - 1
    }

    /// Get the current event index
    pub fn current_index(&self) -> usize {
        self.current_index
    }

    /// Get the number of events in the log
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the event log is empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Can we undo?
    pub fn can_undo(&self) -> bool {
        self.current_index > 0
    }

    /// Can we redo?
    pub fn can_redo(&self) -> bool {
        self.current_index < self.entries.len()
    }

    /// Move back through events (for undo)
    /// Collects all events up to and including the first write action, returns their inverses
    /// This processes readonly events (like scrolling) and stops at write events (like Insert/Delete)
    pub fn undo(&mut self) -> Vec<Event> {
        let mut inverse_events = Vec::new();
        let mut found_write_action = false;

        // Keep moving backward until we find a write action
        while self.can_undo() && !found_write_action {
            self.current_index -= 1;
            let event = &self.entries[self.current_index].event;

            // Check if this is a write action - we'll stop after processing it
            if event.is_write_action() {
                found_write_action = true;
            }

            // Try to get the inverse of this event
            if let Some(inverse) = event.inverse() {
                inverse_events.push(inverse);
            }
            // If no inverse exists (like MoveCursor), we just skip it
        }

        inverse_events
    }

    /// Move forward through events (for redo)
    /// Collects the first write action plus all readonly events after it (until next write action)
    /// This processes readonly events (like scrolling) with write events (like Insert/Delete)
    pub fn redo(&mut self) -> Vec<Event> {
        let mut events = Vec::new();
        let mut found_write_action = false;

        // Keep moving forward to collect write action and subsequent readonly events
        while self.can_redo() {
            let event = self.entries[self.current_index].event.clone();

            // If we've already found a write action and this is another write action, stop
            if found_write_action && event.is_write_action() {
                // Don't include this event, it's the next write action
                break;
            }

            self.current_index += 1;

            // Mark if we found a write action
            if event.is_write_action() {
                found_write_action = true;
            }

            events.push(event);
        }

        events
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

    #[test]
    fn test_navigation_after_undo_preserves_redo() {
        // Regression test: navigation after undo should not destroy redo history.
        // Standard editors (VS Code, Sublime) allow: type, undo, move around, redo.
        let mut log = EventLog::new();

        // Type 'a' (Insert + MoveCursor)
        log.append(Event::Insert {
            position: 0,
            text: "a".to_string(),
            cursor_id: CursorId(0),
        });
        log.append(Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 0,
            new_position: 1,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });
        assert_eq!(log.current_index(), 2);

        // Undo (walks back past MoveCursor and Insert)
        let undo_events = log.undo();
        assert!(!undo_events.is_empty());
        assert_eq!(log.current_index(), 0);
        assert!(log.can_redo());

        // Navigate (MoveCursor) — should NOT destroy redo
        log.append(Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 0,
            new_position: 0,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });
        assert!(
            log.can_redo(),
            "Navigation after undo should preserve redo history"
        );

        // Redo should still work
        let redo_events = log.redo();
        assert!(
            !redo_events.is_empty(),
            "Redo should return events after navigation"
        );
    }

    #[test]
    fn test_write_action_after_undo_clears_redo() {
        // Write actions after undo SHOULD still clear redo history
        let mut log = EventLog::new();

        log.append(Event::Insert {
            position: 0,
            text: "a".to_string(),
            cursor_id: CursorId(0),
        });

        log.undo();
        assert!(log.can_redo());

        // New write action should truncate redo
        log.append(Event::Insert {
            position: 0,
            text: "b".to_string(),
            cursor_id: CursorId(0),
        });
        assert!(
            !log.can_redo(),
            "Write action after undo should clear redo history"
        );
    }

    /// Test for v0.1.77 panic: "range end index 148 out of range for slice of length 125"
    ///
    /// The bug occurs when:
    /// 1. Make many changes (entries grows)
    /// 2. mark_saved() sets saved_at_index to current position
    /// 3. Undo several times
    /// 4. Make new changes - this truncates entries but saved_at_index stays
    /// 5. is_at_saved_position() panics on out-of-bounds slice access
    #[test]
    fn test_is_at_saved_position_after_truncate() {
        let mut log = EventLog::new();

        // Step 1: Make many changes
        for i in 0..150 {
            log.append(Event::Insert {
                position: i,
                text: "x".to_string(),
                cursor_id: CursorId(0),
            });
        }

        assert_eq!(log.entries().len(), 150);
        assert_eq!(log.current_index(), 150);

        // Step 2: Save - this sets saved_at_index = 150
        log.mark_saved();

        // Step 3: Undo 30 times - current_index goes to 120, but entries stay at 150
        for _ in 0..30 {
            log.undo();
        }
        assert_eq!(log.current_index(), 120);
        assert_eq!(log.entries().len(), 150);

        // Step 4: Make new changes - this truncates entries to 120, then adds new
        log.append(Event::Insert {
            position: 0,
            text: "NEW".to_string(),
            cursor_id: CursorId(0),
        });

        // Now entries.len() = 121, but saved_at_index = 150
        assert_eq!(log.entries().len(), 121);
        assert_eq!(log.current_index(), 121);

        // Step 5: Call is_at_saved_position() - THIS PANICS in v0.1.77
        // The code does: self.entries[start..end] where end = saved_at_index = 150
        // but entries.len() = 121, so 150 is out of bounds
        let result = log.is_at_saved_position();

        // After fix: should return false (we're not at saved position, we branched off)
        assert!(
            !result,
            "Should not be at saved position after undo + new edit"
        );
    }
}
