# Event Log Architecture - Lossless Edit History

## Core Principle

**Every state change goes through the event log. No exceptions.**

```
User Input → Event → Log → Apply → New State
                      ↓
                   (Persist)
```

## Event Types

```rust
/// Every possible state change in the editor
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Event {
    /// Text editing events
    Insert {
        position: usize,
        text: String,
        cursor_id: CursorId,
    },
    Delete {
        range: Range<usize>,
        deleted_text: String,  // Store for undo
        cursor_id: CursorId,
    },

    /// Cursor events
    AddCursor {
        cursor: Cursor,
    },
    RemoveCursor {
        cursor_id: CursorId,
    },
    MoveCursor {
        cursor_id: CursorId,
        old_position: usize,
        new_position: usize,
    },
    SetSelection {
        cursor_id: CursorId,
        anchor: Option<usize>,
    },

    /// Viewport events
    Scroll {
        old_position: usize,
        new_position: usize,
    },

    /// Composite events (macro recording)
    Batch {
        events: Vec<Event>,
        description: String,
    },
}

/// Unique identifier for each cursor
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CursorId(u64);

/// Event with metadata
#[derive(Clone, Debug)]
pub struct LogEntry {
    /// Monotonic event ID
    id: EventId,

    /// The actual event
    event: Event,

    /// Timestamp
    timestamp: Instant,

    /// Optional: User-facing description for undo stack
    description: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EventId(u64);
```

## Event Log - The Single Source of Truth

```rust
pub struct EventLog {
    /// All events ever applied (in order)
    events: Vec<LogEntry>,

    /// Current position in history (for undo/redo)
    current: EventId,

    /// Next event ID to assign
    next_id: EventId,

    /// Optional: File handle for persistence
    persist_file: Option<File>,
}

impl EventLog {
    pub fn new() -> Self {
        EventLog {
            events: Vec::new(),
            current: EventId(0),
            next_id: EventId(1),
            persist_file: None,
        }
    }

    /// Record and return a new event (doesn't apply it)
    pub fn record(&mut self, event: Event) -> EventId {
        let id = self.next_id;
        self.next_id = EventId(id.0 + 1);

        let entry = LogEntry {
            id,
            event,
            timestamp: Instant::now(),
            description: None,
        };

        self.events.push(entry);
        self.current = id;

        // Optional: Write to disk
        if let Some(ref mut file) = self.persist_file {
            self.persist_event(&entry, file);
        }

        id
    }

    /// Get event by ID
    pub fn get(&self, id: EventId) -> Option<&LogEntry> {
        self.events.iter().find(|e| e.id == id)
    }

    /// Get all events up to current position
    pub fn history(&self) -> &[LogEntry] {
        let idx = self.events.iter()
            .position(|e| e.id == self.current)
            .unwrap_or(0);
        &self.events[..=idx]
    }

    /// Undo: move current pointer back
    pub fn undo(&mut self) -> Option<EventId> {
        if self.current.0 > 0 {
            self.current = EventId(self.current.0 - 1);
            Some(self.current)
        } else {
            None
        }
    }

    /// Redo: move current pointer forward
    pub fn redo(&mut self) -> Option<EventId> {
        let next = EventId(self.current.0 + 1);
        if self.events.iter().any(|e| e.id == next) {
            self.current = next;
            Some(self.current)
        } else {
            None
        }
    }

    /// Persist event to disk (append-only log)
    fn persist_event(&self, entry: &LogEntry, file: &mut File) {
        // Serialize as JSON lines or bincode
        let json = serde_json::to_string(&entry.event).unwrap();
        writeln!(file, "{}", json).unwrap();
    }
}
```

## State Application - Single Pathway

```rust
pub struct EditorState {
    /// The text content
    buffer: Buffer,

    /// All cursors (indexed by CursorId)
    cursors: HashMap<CursorId, Cursor>,

    /// Primary cursor ID
    primary_cursor: CursorId,

    /// What's visible
    viewport: Viewport,

    /// Next cursor ID to assign
    next_cursor_id: CursorId,
}

impl EditorState {
    /// Apply an event to the state
    /// This is the ONLY way to modify state
    pub fn apply(&mut self, event: &Event) {
        match event {
            Event::Insert { position, text, cursor_id } => {
                // Insert text into buffer
                self.buffer.insert(*position, text);

                // Move cursor forward
                if let Some(cursor) = self.cursors.get_mut(cursor_id) {
                    cursor.position += text.len();
                }

                // Adjust all other cursors after insert position
                for (id, cursor) in &mut self.cursors {
                    if *id != *cursor_id && cursor.position >= *position {
                        cursor.position += text.len();
                    }
                }

                // Smart scroll: keep cursor visible
                self.viewport.ensure_visible(&self.buffer,
                    self.cursors.get(cursor_id).unwrap());
            }

            Event::Delete { range, deleted_text, cursor_id } => {
                // Delete from buffer
                self.buffer.delete(range.clone());

                // Move cursor to deletion start
                if let Some(cursor) = self.cursors.get_mut(cursor_id) {
                    cursor.position = range.start;
                }

                // Adjust all other cursors
                let len = range.len();
                for (id, cursor) in &mut self.cursors {
                    if *id != *cursor_id {
                        if cursor.position >= range.end {
                            cursor.position -= len;
                        } else if cursor.position > range.start {
                            cursor.position = range.start;
                        }
                    }
                }

                // Smart scroll
                self.viewport.ensure_visible(&self.buffer,
                    self.cursors.get(cursor_id).unwrap());
            }

            Event::AddCursor { cursor } => {
                let id = self.next_cursor_id;
                self.next_cursor_id = CursorId(id.0 + 1);
                self.cursors.insert(id, *cursor);
                self.primary_cursor = id;

                // Smart scroll to new cursor
                self.viewport.ensure_visible(&self.buffer, cursor);
            }

            Event::RemoveCursor { cursor_id } => {
                self.cursors.remove(cursor_id);
            }

            Event::MoveCursor { cursor_id, old_position, new_position } => {
                if let Some(cursor) = self.cursors.get_mut(cursor_id) {
                    cursor.position = *new_position;

                    // Smart scroll
                    self.viewport.ensure_visible(&self.buffer, cursor);
                }
            }

            Event::SetSelection { cursor_id, anchor } => {
                if let Some(cursor) = self.cursors.get_mut(cursor_id) {
                    cursor.anchor = *anchor;
                }
            }

            Event::Scroll { old_position, new_position } => {
                self.viewport.scroll_pos = *new_position;
            }

            Event::Batch { events, description } => {
                for event in events {
                    self.apply(event);
                }
            }
        }
    }

    /// Rebuild entire state from event log (for undo)
    pub fn rebuild_from_log(&mut self, log: &EventLog) {
        // Reset to initial state
        *self = EditorState::new();

        // Apply all events up to current
        for entry in log.history() {
            self.apply(&entry.event);
        }
    }
}
```

## Smart Scrolling

```rust
impl Viewport {
    /// Ensure a cursor is visible, scrolling if needed
    pub fn ensure_visible(&mut self, buffer: &Buffer, cursor: &Cursor) {
        let cursor_line = buffer.byte_to_line(cursor.position);
        let cursor_col = cursor.position - buffer.line_to_byte(cursor_line);

        let visible_start = buffer.byte_to_line(self.scroll_pos);
        let visible_end = visible_start + self.height as usize;

        // Vertical scroll
        if cursor_line < visible_start {
            // Cursor above viewport - scroll up
            self.scroll_to_line(buffer, cursor_line);
        } else if cursor_line >= visible_end {
            // Cursor below viewport - scroll down
            self.scroll_to_line(buffer, cursor_line.saturating_sub(self.height as usize - 1));
        }

        // Horizontal scroll (if needed)
        let scroll_col = self.scroll_col;
        if cursor_col < scroll_col {
            self.scroll_col = cursor_col;
        } else if cursor_col >= scroll_col + self.width as usize {
            self.scroll_col = cursor_col.saturating_sub(self.width as usize - 1);
        }
    }

    /// Scroll to show a specific line at the top
    pub fn scroll_to_line(&mut self, buffer: &Buffer, line: usize) {
        self.scroll_pos = buffer.line_to_byte(line);
    }

    /// Smart scroll for multi-cursor: show all cursors if possible
    pub fn ensure_all_visible(&mut self, buffer: &Buffer, cursors: &HashMap<CursorId, Cursor>) {
        if cursors.is_empty() {
            return;
        }

        // Find min and max cursor lines
        let mut min_line = usize::MAX;
        let mut max_line = 0;

        for cursor in cursors.values() {
            let line = buffer.byte_to_line(cursor.position);
            min_line = min_line.min(line);
            max_line = max_line.max(line);
        }

        // If all cursors fit in viewport, center them
        let span = max_line - min_line;
        if span < self.height as usize {
            let center = (min_line + max_line) / 2;
            let top = center.saturating_sub(self.height as usize / 2);
            self.scroll_to_line(buffer, top);
        } else {
            // Can't show all - just show primary cursor
            // (caller should pass primary cursor separately)
        }
    }

    /// Smooth scroll with animation (future enhancement)
    pub fn smooth_scroll_to(&mut self, buffer: &Buffer, target_line: usize) {
        // For now, just jump
        self.scroll_to_line(buffer, target_line);

        // Future: interpolate over frames
        // let current_line = buffer.byte_to_line(self.scroll_pos);
        // animate from current_line to target_line
    }
}
```

## Editor - Event-Driven Architecture

```rust
pub struct Editor {
    /// Current state
    state: EditorState,

    /// Event log (authoritative history)
    log: EventLog,

    /// Syntax highlighting
    highlighter: Highlighter,

    /// Terminal handle
    terminal: Terminal,
}

impl Editor {
    /// Handle user input - generate events
    pub fn handle_event(&mut self, input: crossterm::Event) -> Result<bool> {
        let events = match input {
            crossterm::Event::Key(key) => self.key_to_events(key),
            crossterm::Event::Mouse(mouse) => self.mouse_to_events(mouse),
            _ => vec![],
        };

        // Record and apply each event
        for event in events {
            let id = self.log.record(event.clone());
            self.state.apply(&event);
        }

        Ok(true)
    }

    /// Convert keypress to event(s)
    fn key_to_events(&self, key: KeyEvent) -> Vec<Event> {
        match key.code {
            KeyCode::Char(c) if key.modifiers.is_empty() => {
                // Type a character at each cursor
                self.state.cursors.iter()
                    .map(|(id, cursor)| Event::Insert {
                        position: cursor.position,
                        text: c.to_string(),
                        cursor_id: *id,
                    })
                    .collect()
            }

            KeyCode::Backspace => {
                // Delete before each cursor
                self.state.cursors.iter()
                    .map(|(id, cursor)| {
                        if cursor.position > 0 {
                            let start = cursor.position - 1;
                            let deleted = self.state.buffer.slice(start..cursor.position);
                            Some(Event::Delete {
                                range: start..cursor.position,
                                deleted_text: deleted,
                                cursor_id: *id,
                            })
                        } else {
                            None
                        }
                    })
                    .flatten()
                    .collect()
            }

            KeyCode::Left => {
                // Move each cursor left
                self.state.cursors.iter()
                    .map(|(id, cursor)| {
                        if cursor.position > 0 {
                            let new_pos = cursor.position - 1;
                            Some(Event::MoveCursor {
                                cursor_id: *id,
                                old_position: cursor.position,
                                new_position: new_pos,
                            })
                        } else {
                            None
                        }
                    })
                    .flatten()
                    .collect()
            }

            KeyCode::Char('z') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                // Undo
                if let Some(_prev_id) = self.log.undo() {
                    // State will be rebuilt in handle_undo()
                    vec![]
                } else {
                    vec![]
                }
            }

            KeyCode::Char('d') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                // Add cursor at next occurrence of selection
                // (Sublime-style multi-cursor)
                vec![self.create_next_occurrence_event()]
            }

            _ => vec![],
        }
    }

    /// Handle undo - rebuild state from log
    pub fn handle_undo(&mut self) {
        self.state.rebuild_from_log(&self.log);
    }

    /// Handle redo - rebuild state from log
    pub fn handle_redo(&mut self) {
        self.log.redo();
        self.state.rebuild_from_log(&self.log);
    }
}
```

## Event Log Persistence

```rust
impl EventLog {
    /// Load event log from file
    pub fn load(path: &Path) -> Result<Self> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        let mut log = EventLog::new();
        for line in reader.lines() {
            let json = line?;
            let event: Event = serde_json::from_str(&json)?;
            log.record(event);
        }

        Ok(log)
    }

    /// Enable persistence to file
    pub fn persist_to(&mut self, path: &Path) -> Result<()> {
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)?;
        self.persist_file = Some(file);
        Ok(())
    }
}
```

## Benefits

### Undo/Redo
- **Trivial**: Just move pointer in event log and rebuild state
- **Granular**: Every keystroke is undoable
- **Non-linear**: Can branch (future feature)

### Replay
- **Debugging**: Reproduce any bug by replaying log
- **Testing**: Record user session, replay in tests
- **Demo**: Create tutorials by replaying edits

### Collaboration (Future)
- **OT/CRDT**: Events can be transformed/merged
- **Sync**: Send events over network
- **Conflict resolution**: Event log is the source of truth

### Time Travel
- **Scrub through history**: View state at any point
- **Diff**: Compare any two states
- **Blame**: Who made which edit?

### Analytics
- **Keystroke analysis**: Optimize keybindings
- **Performance**: Identify slow operations
- **Usage patterns**: What features are used?

## Performance Considerations

### Memory
- **Event log size**: ~100 bytes per event
- **10,000 keystrokes**: ~1MB
- **Solution**: Compress old events, snapshot periodically

### Rebuild Cost
- **Full rebuild**: O(n events)
- **Optimization**: Snapshot state every 1000 events
- **Undo**: Only replay from nearest snapshot

```rust
pub struct EventLog {
    events: Vec<LogEntry>,

    /// Snapshots for fast rebuild
    snapshots: Vec<(EventId, EditorState)>,

    /// Snapshot every N events
    snapshot_interval: usize,
}

impl EventLog {
    pub fn rebuild_state(&self) -> EditorState {
        // Find nearest snapshot before current
        let snapshot = self.snapshots.iter()
            .rev()
            .find(|(id, _)| *id <= self.current);

        let mut state = if let Some((snap_id, snap_state)) = snapshot {
            // Start from snapshot
            let mut state = snap_state.clone();

            // Apply events since snapshot
            for entry in self.events.iter() {
                if entry.id > *snap_id && entry.id <= self.current {
                    state.apply(&entry.event);
                }
            }
            state
        } else {
            // No snapshot, rebuild from scratch
            let mut state = EditorState::new();
            for entry in self.history() {
                state.apply(&entry.event);
            }
            state
        };

        state
    }

    pub fn maybe_snapshot(&mut self, state: &EditorState) {
        if self.events.len() % self.snapshot_interval == 0 {
            self.snapshots.push((self.current, state.clone()));
        }
    }
}
```

## Updated File Structure

```
src/
├── main.rs           # Entry point, CLI arg parsing
├── editor.rs         # Editor struct, event loop, input handling
├── event.rs          # Event enum, EventLog, LogEntry
├── state.rs          # EditorState, apply() method
├── buffer.rs         # Buffer struct, line cache, file I/O
├── cursor.rs         # Cursor struct, CursorId
├── viewport.rs       # Viewport, smart scrolling
├── highlighter.rs    # Syntax highlighting with tree-sitter
├── chunk_tree.rs     # (keep existing) Rope implementation
├── keybindings.rs    # Key event -> event(s) conversion
└── render.rs         # Terminal rendering helpers
```

## Updated Task List

### Phase 0: Event System (NEW - 1 day)
- [ ] Implement Event enum with all event types
- [ ] Implement EventLog with record/undo/redo
- [ ] Implement EditorState with apply() method
- [ ] Add event persistence (JSON lines format)
- [ ] Add snapshots for fast rebuild
- [ ] Test: Record events, undo/redo, rebuild state

### Phase 1: Core Editor (2-3 days)
- [ ] Implement Buffer with ChunkTree + line cache
- [ ] Implement Cursor with CursorId
- [ ] Implement Viewport with smart scrolling
- [ ] Implement Editor event loop (event-driven)
- [ ] Implement basic rendering (no highlighting)
- [ ] Basic keybindings via event generation

**Milestone**: Can edit text files, undo/redo works

### Phase 2: Multi-Cursor (1 day)
- [ ] Extend events for multi-cursor operations
- [ ] Implement multi-cursor keybindings (Ctrl+D)
- [ ] Smart scroll for multiple cursors
- [ ] Visual selection rendering

**Milestone**: Full multi-cursor editing with undo

### Phase 3: Syntax Highlighting (1 day)
- [ ] Implement Highlighter with tree-sitter
- [ ] Implement highlight cache
- [ ] Add language detection
- [ ] Render with colors

**Milestone**: Pretty colored code

### Phase 4: Large File Optimization (1 day)
- [ ] Lazy line cache building
- [ ] Profile event application
- [ ] Optimize hot paths
- [ ] Test with 1GB+ files

**Milestone**: Production ready

## Smart Scroll Examples

### Single Edit
```rust
// User types at line 100
Event::Insert { position: 5000, text: "hello", cursor_id: 0 }

// Smart scroll ensures line 100 is visible
// If line 100 is off-screen, scroll to show it
```

### Multi-Cursor Edit
```rust
// User has cursors at lines 10, 50, 100
// Types a character at all cursors
[
  Event::Insert { position: 100, text: "x", cursor_id: 0 },
  Event::Insert { position: 500, text: "x", cursor_id: 1 },
  Event::Insert { position: 1000, text: "x", cursor_id: 2 },
]

// Smart scroll:
// - If all cursors fit on screen, show them all (centered)
// - Otherwise, show primary cursor (last one)
```

### Jump to Definition (Future)
```rust
// User clicks on function name at line 10
// Definition is at line 500

Event::MoveCursor {
    cursor_id: 0,
    old_position: 100,
    new_position: 5000,
}

// Smart scroll jumps to line 500
// Centers it on screen
```

## Key Advantages

1. **Single code path**: All state changes go through events
2. **Testable**: Record inputs, replay, verify outputs
3. **Debuggable**: Log shows exactly what happened
4. **Extensible**: Add new events without touching apply code
5. **Persistent**: Save/load event log = save/load session
6. **Collaborative**: Events can be sent over network

Ready to implement this event-driven architecture?
