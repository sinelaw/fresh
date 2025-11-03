use crate::buffer::Buffer;
use crate::cursor::{Cursor, Cursors};
use crate::event::{CursorId, Event};
use crate::viewport::Viewport;

/// The complete editor state - everything needed to represent the current editing session
pub struct EditorState {
    /// The text buffer
    pub buffer: Buffer,

    /// All cursors
    pub cursors: Cursors,

    /// The viewport
    pub viewport: Viewport,

    /// Current mode (for modal editing, if implemented)
    pub mode: String,
}

impl EditorState {
    /// Create a new editor state with an empty buffer
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            buffer: Buffer::new(),
            cursors: Cursors::new(),
            viewport: Viewport::new(width, height),
            mode: "insert".to_string(),
        }
    }

    /// Create an editor state from a file
    pub fn from_file(path: &std::path::Path, width: u16, height: u16) -> std::io::Result<Self> {
        let buffer = Buffer::load_from_file(path)?;
        Ok(Self {
            buffer,
            cursors: Cursors::new(),
            viewport: Viewport::new(width, height),
            mode: "insert".to_string(),
        })
    }

    /// Apply an event to the state - THE ONLY WAY TO MODIFY STATE
    /// This is the heart of the event-driven architecture
    pub fn apply(&mut self, event: &Event) {
        match event {
            Event::Insert { position, text, cursor_id } => {
                // Insert text into buffer
                self.buffer.insert(*position, text);

                // Adjust all cursors after the edit
                self.cursors.adjust_for_edit(*position, 0, text.len());

                // Move the cursor that made the edit to the end of the insertion
                if let Some(cursor) = self.cursors.get_mut(*cursor_id) {
                    cursor.position = position + text.len();
                    cursor.clear_selection();
                }

                // Smart scroll to keep cursor visible
                if let Some(cursor) = self.cursors.get(*cursor_id) {
                    self.viewport.ensure_visible(&mut self.buffer, cursor);
                }
            }

            Event::Delete { range, cursor_id, .. } => {
                let len = range.len();

                // Delete from buffer
                self.buffer.delete(range.clone());

                // Adjust all cursors after the edit
                self.cursors.adjust_for_edit(range.start, len, 0);

                // Move the cursor that made the edit to the start of deletion
                if let Some(cursor) = self.cursors.get_mut(*cursor_id) {
                    cursor.position = range.start;
                    cursor.clear_selection();
                }

                // Smart scroll to keep cursor visible
                if let Some(cursor) = self.cursors.get(*cursor_id) {
                    self.viewport.ensure_visible(&mut self.buffer, cursor);
                }
            }

            Event::MoveCursor { cursor_id, position, anchor } => {
                if let Some(cursor) = self.cursors.get_mut(*cursor_id) {
                    cursor.position = *position;
                    cursor.anchor = *anchor;

                    // Smart scroll to keep cursor visible
                    self.viewport.ensure_visible(&mut self.buffer, cursor);
                }
            }

            Event::AddCursor { cursor_id, position, anchor } => {
                let cursor = if let Some(anchor) = anchor {
                    Cursor::with_selection(*anchor, *position)
                } else {
                    Cursor::new(*position)
                };

                // Check if cursor already exists
                if self.cursors.get(*cursor_id).is_none() {
                    // If the cursor_id matches our next_id, use add()
                    // Otherwise manually insert (for replaying events)
                    let _ = self.cursors.add(cursor);
                }

                self.cursors.normalize();
            }

            Event::RemoveCursor { cursor_id } => {
                self.cursors.remove(*cursor_id);
            }

            Event::Scroll { line_offset } => {
                if *line_offset > 0 {
                    self.viewport.scroll_down(*line_offset as usize, self.buffer.line_count());
                } else {
                    self.viewport.scroll_up(line_offset.unsigned_abs() as usize);
                }
            }

            Event::SetViewport { top_line } => {
                self.viewport.scroll_to(*top_line, self.buffer.line_count());
            }

            Event::ChangeMode { mode } => {
                self.mode = mode.clone();
            }
        }
    }

    /// Apply multiple events in sequence
    pub fn apply_many(&mut self, events: &[Event]) {
        for event in events {
            self.apply(event);
        }
    }

    /// Get the primary cursor
    pub fn primary_cursor(&self) -> &Cursor {
        self.cursors.primary()
    }

    /// Get the primary cursor mutably (for reading state only, not for modification!)
    pub fn primary_cursor_mut(&mut self) -> &mut Cursor {
        self.cursors.primary_mut()
    }

    /// Get all cursor positions for rendering
    pub fn cursor_positions(&mut self) -> Vec<(u16, u16)> {
        let mut positions = Vec::new();
        for (_, cursor) in self.cursors.iter() {
            let pos = self.viewport.cursor_screen_position(&mut self.buffer, cursor);
            positions.push(pos);
        }
        positions
    }

    /// Resize the viewport
    pub fn resize(&mut self, width: u16, height: u16) {
        self.viewport.resize(width, height);

        // Ensure primary cursor is still visible after resize
        let primary = self.cursors.primary().clone();
        self.viewport.ensure_visible(&mut self.buffer, &primary);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_new() {
        let state = EditorState::new(80, 24);
        assert!(state.buffer.is_empty());
        assert_eq!(state.cursors.count(), 1);
        assert_eq!(state.cursors.primary().position, 0);
    }

    #[test]
    fn test_apply_insert() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id,
        });

        assert_eq!(state.buffer.to_string(), "hello");
        assert_eq!(state.cursors.primary().position, 5);
        assert!(state.buffer.is_modified());
    }

    #[test]
    fn test_apply_delete() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        // Insert then delete
        state.apply(&Event::Insert {
            position: 0,
            text: "hello world".to_string(),
            cursor_id,
        });

        state.apply(&Event::Delete {
            range: 5..11,
            deleted_text: " world".to_string(),
            cursor_id,
        });

        assert_eq!(state.buffer.to_string(), "hello");
        assert_eq!(state.cursors.primary().position, 5);
    }

    #[test]
    fn test_apply_move_cursor() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id,
        });

        state.apply(&Event::MoveCursor {
            cursor_id,
            position: 2,
            anchor: None,
        });

        assert_eq!(state.cursors.primary().position, 2);
    }

    #[test]
    fn test_apply_add_cursor() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = CursorId(1);

        state.apply(&Event::AddCursor {
            cursor_id,
            position: 5,
            anchor: None,
        });

        assert_eq!(state.cursors.count(), 2);
    }

    #[test]
    fn test_apply_many() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        let events = vec![
            Event::Insert {
                position: 0,
                text: "hello ".to_string(),
                cursor_id,
            },
            Event::Insert {
                position: 6,
                text: "world".to_string(),
                cursor_id,
            },
        ];

        state.apply_many(&events);

        assert_eq!(state.buffer.to_string(), "hello world");
    }

    #[test]
    fn test_cursor_adjustment_after_insert() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        // Add a second cursor at position 5
        state.apply(&Event::AddCursor {
            cursor_id: CursorId(1),
            position: 5,
            anchor: None,
        });

        // Insert at position 0 - should push second cursor forward
        state.apply(&Event::Insert {
            position: 0,
            text: "abc".to_string(),
            cursor_id,
        });

        // Second cursor should be at position 5 + 3 = 8
        if let Some(cursor) = state.cursors.get(CursorId(1)) {
            assert_eq!(cursor.position, 8);
        }
    }
}
