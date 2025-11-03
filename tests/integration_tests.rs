// Integration tests - testing how modules work together

mod common;

use editor::{
    event::{CursorId, Event, EventLog},
    state::EditorState,
};

/// Test that cursor positions are correctly adjusted after buffer edits
#[test]
fn test_buffer_cursor_adjustment_on_insert() {
    let mut state = EditorState::new(80, 24);

    // Get the initial primary cursor ID (CursorId(0))
    let original_primary = state.cursors.primary_id();

    // Insert some initial text with the original primary cursor
    state.apply(&Event::Insert {
        position: 0,
        text: "hello world".to_string(),
        cursor_id: original_primary,
    });

    // Original primary cursor should be at end of inserted text (position 11)
    assert_eq!(state.cursors.get(original_primary).unwrap().position, 11);

    // Add a second cursor at position 6 (start of "world")
    // Note: This will make CursorId(1) the new primary
    state.apply(&Event::AddCursor {
        cursor_id: CursorId(1),
        position: 6,
        anchor: None,
    });

    // Verify CursorId(1) is at position 6 and is now primary
    assert_eq!(state.cursors.get(CursorId(1)).unwrap().position, 6);
    assert_eq!(state.cursors.primary_id(), CursorId(1));

    // Insert text at beginning with the ORIGINAL primary cursor (not the new one)
    // This tests that non-editing cursors get adjusted
    let insert_len = "INSERTED ".len();
    state.apply(&Event::Insert {
        position: 0,
        text: "INSERTED ".to_string(),
        cursor_id: original_primary, // Using original cursor, not the new primary
    });

    // The cursor that made the edit (original_primary) should be at position 0 + insert_len = 9
    assert_eq!(
        state.cursors.get(original_primary).unwrap().position,
        insert_len,
        "Cursor that made the edit should be at end of insertion"
    );

    // CursorId(1) was at position 6, should have moved forward by insert_len to position 15
    assert_eq!(
        state.cursors.get(CursorId(1)).unwrap().position,
        6 + insert_len,
        "Non-editing cursor should be adjusted by insertion length"
    );

    // Buffer content should be correct
    assert_eq!(state.buffer.to_string(), "INSERTED hello world");
}

/// Test that cursor positions are correctly adjusted after deletions
#[test]
fn test_buffer_cursor_adjustment_on_delete() {
    let mut state = EditorState::new(80, 24);

    // Insert initial text
    state.apply(&Event::Insert {
        position: 0,
        text: "hello beautiful world".to_string(),
        cursor_id: state.cursors.primary_id(),
    });

    // Add cursor at position 16 (start of "world")
    state.apply(&Event::AddCursor {
        cursor_id: CursorId(1),
        position: 16,
        anchor: None,
    });

    // Delete "beautiful " (positions 6-16)
    state.apply(&Event::Delete {
        range: 6..16,
        deleted_text: "beautiful ".to_string(),
        cursor_id: state.cursors.primary_id(),
    });

    // Second cursor should have moved back to position 6
    if let Some(cursor) = state.cursors.get(CursorId(1)) {
        assert_eq!(cursor.position, 6);
    }

    // Buffer content should be correct
    assert_eq!(state.buffer.to_string(), "hello world");
}

/// Test undo/redo with EditorState and EventLog
#[test]
fn test_state_eventlog_undo_redo() {
    let mut state = EditorState::new(80, 24);
    let mut log = EventLog::new();

    let cursor_id = state.cursors.primary_id();

    // Perform a series of edits - each insert at the END of the buffer
    let event1 = Event::Insert {
        position: 0,
        text: "a".to_string(),
        cursor_id,
    };
    log.append(event1.clone());
    state.apply(&event1);

    let event2 = Event::Insert {
        position: state.buffer.len(),
        text: "b".to_string(),
        cursor_id,
    };
    log.append(event2.clone());
    state.apply(&event2);

    let event3 = Event::Insert {
        position: state.buffer.len(),
        text: "c".to_string(),
        cursor_id,
    };
    log.append(event3.clone());
    state.apply(&event3);

    assert_eq!(state.buffer.to_string(), "abc");

    // Undo all - log.undo() returns the event at that position, we need to compute its inverse
    while log.can_undo() {
        if let Some(event) = log.undo() {
            if let Some(inverse) = event.inverse() {
                state.apply(&inverse);
            }
        }
    }

    assert_eq!(state.buffer.to_string(), "");

    // Redo all - log.redo() returns the original event to replay
    while log.can_redo() {
        if let Some(event) = log.redo() {
            state.apply(event);
        }
    }

    assert_eq!(state.buffer.to_string(), "abc");
}

/// Test that undo/redo maintains cursor positions correctly
#[test]
fn test_undo_redo_cursor_positions() {
    let mut state = EditorState::new(80, 24);
    let mut log = EventLog::new();

    let cursor_id = state.cursors.primary_id();

    // Type "hello" - each character at the end of the buffer
    for ch in "hello".chars() {
        let pos = state.buffer.len();
        let event = Event::Insert {
            position: pos,
            text: ch.to_string(),
            cursor_id,
        };
        log.append(event.clone());
        state.apply(&event);
    }

    assert_eq!(state.buffer.to_string(), "hello");
    let cursor_after_typing = state.cursors.primary().position;
    assert_eq!(cursor_after_typing, 5);

    // Undo twice (remove 'o' and 'l')
    for _ in 0..2 {
        if let Some(event) = log.undo() {
            if let Some(inverse) = event.inverse() {
                state.apply(&inverse);
            }
        }
    }

    assert_eq!(state.buffer.to_string(), "hel");
    assert_eq!(state.cursors.primary().position, 3);

    // Redo twice
    for _ in 0..2 {
        if let Some(event) = log.redo() {
            state.apply(event);
        }
    }

    assert_eq!(state.buffer.to_string(), "hello");
    assert_eq!(state.cursors.primary().position, 5);
}

/// Test viewport ensures cursor stays visible after edits
#[test]
fn test_viewport_tracks_cursor_through_edits() {
    let mut state = EditorState::new(80, 10); // Small viewport

    let cursor_id = state.cursors.primary_id();

    // Insert many lines to make content scroll
    for i in 0..20 {
        let event = Event::Insert {
            position: state.buffer.len(),
            text: format!("Line {i}\n"),
            cursor_id,
        };
        state.apply(&event);
    }

    // Cursor should be at the end
    let cursor_pos = state.cursors.primary().position;
    assert!(cursor_pos > 0);

    // Cursor should be visible (within viewport)
    let cursor_line = state.buffer.byte_to_line_lazy(cursor_pos);
    let visible_range = state.viewport.visible_range();

    assert!(
        visible_range.contains(&cursor_line),
        "Cursor at line {cursor_line} should be in visible range {visible_range:?}"
    );
}

/// Test multi-cursor normalization after overlapping edits
#[test]
fn test_multi_cursor_normalization() {
    let mut state = EditorState::new(80, 24);

    // Insert initial text
    state.apply(&Event::Insert {
        position: 0,
        text: "hello world".to_string(),
        cursor_id: state.cursors.primary_id(),
    });

    // Add overlapping cursors
    state.apply(&Event::AddCursor {
        cursor_id: CursorId(1),
        position: 5,
        anchor: None,
    });

    state.apply(&Event::AddCursor {
        cursor_id: CursorId(2),
        position: 6,
        anchor: None,
    });

    // Should have 3 cursors initially
    assert_eq!(state.cursors.count(), 3);

    // After normalization (which happens in AddCursor), overlapping cursors might be merged
    // This depends on Cursors::normalize() implementation
    // For now, just verify they all exist and are in valid positions
    for (_, cursor) in state.cursors.iter() {
        assert!(cursor.position <= state.buffer.len());
    }
}

/// Test that viewport resizing maintains cursor visibility
#[test]
fn test_viewport_resize_maintains_cursor() {
    let mut state = EditorState::new(80, 24);

    // Insert text and move cursor to middle
    state.apply(&Event::Insert {
        position: 0,
        text: "line1\nline2\nline3\nline4\nline5\n".to_string(),
        cursor_id: state.cursors.primary_id(),
    });

    state.apply(&Event::MoveCursor {
        cursor_id: state.cursors.primary_id(),
        position: 12, // Middle of line 2
        anchor: None,
    });

    // Resize to smaller height
    state.resize(80, 5);

    // Cursor should still be visible
    let cursor_line = state.buffer.byte_to_line_lazy(state.cursors.primary().position);
    let visible_range = state.viewport.visible_range();

    assert!(
        visible_range.contains(&cursor_line),
        "After resize, cursor at line {cursor_line} should be in visible range {visible_range:?}"
    );
}
