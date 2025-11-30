// End-to-end tests for file recovery feature

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::event::{CursorId, Event};

/// Test that typing text marks the buffer as recovery-dirty
/// This ensures the recovery auto-save will trigger after edits
/// Uses a file-backed buffer for stable recovery ID tracking
#[test]
fn test_typing_marks_buffer_recovery_dirty() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a file-backed buffer for stable recovery ID
    let _fixture = TestFixture::new("test_recovery.txt", "initial content").unwrap();
    harness.open_file(&_fixture.path).unwrap();

    // Initially, buffer should not be recovery dirty (just opened, not modified)
    assert!(
        !harness.editor().is_active_buffer_recovery_dirty(),
        "Freshly opened buffer should not be recovery dirty"
    );

    // Type some text
    harness.type_text("Hello").unwrap();

    // Buffer should now be recovery dirty
    assert!(
        harness.editor().is_active_buffer_recovery_dirty(),
        "Buffer should be recovery dirty after typing"
    );
}

/// Test that deleting text marks the buffer as recovery-dirty
#[test]
fn test_delete_marks_buffer_recovery_dirty() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a file-backed buffer
    let _fixture = TestFixture::new("test_recovery_delete.txt", "Test content").unwrap();
    harness.open_file(&_fixture.path).unwrap();

    // Initially not dirty
    assert!(
        !harness.editor().is_active_buffer_recovery_dirty(),
        "Freshly opened buffer should not be recovery dirty"
    );

    // Delete with backspace
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    assert!(
        harness.editor().is_active_buffer_recovery_dirty(),
        "Buffer should be recovery dirty after delete"
    );
}

/// Test that insert events via apply_event mark the buffer as recovery-dirty
#[test]
fn test_insert_event_marks_buffer_recovery_dirty() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a file-backed buffer
    let _fixture = TestFixture::new("test_recovery_insert.txt", "").unwrap();
    harness.open_file(&_fixture.path).unwrap();

    // Initially not dirty
    assert!(
        !harness.editor().is_active_buffer_recovery_dirty(),
        "Freshly opened buffer should not be recovery dirty"
    );

    // Apply an insert event directly
    let event = Event::Insert {
        position: 0,
        text: "test".to_string(),
        cursor_id: CursorId(0),
    };
    harness.apply_event(event).unwrap();

    // Should be recovery dirty
    assert!(
        harness.editor().is_active_buffer_recovery_dirty(),
        "Buffer should be recovery dirty after Insert event"
    );
}

/// Test that delete events via apply_event mark the buffer as recovery-dirty
#[test]
fn test_delete_event_marks_buffer_recovery_dirty() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a file-backed buffer with content
    let _fixture = TestFixture::new("test_recovery_delete_event.txt", "Hello World").unwrap();
    harness.open_file(&_fixture.path).unwrap();

    // Initially not dirty
    assert!(
        !harness.editor().is_active_buffer_recovery_dirty(),
        "Freshly opened buffer should not be recovery dirty"
    );

    // Apply delete event
    let delete_event = Event::Delete {
        range: 0..5,
        deleted_text: "Hello".to_string(),
        cursor_id: CursorId(0),
    };
    harness.apply_event(delete_event).unwrap();

    assert!(
        harness.editor().is_active_buffer_recovery_dirty(),
        "Buffer should be recovery dirty after Delete event"
    );
}

/// Test that batch events containing edits mark the buffer as recovery-dirty
#[test]
fn test_batch_event_marks_buffer_recovery_dirty() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a file-backed buffer
    let _fixture = TestFixture::new("test_recovery_batch.txt", "").unwrap();
    harness.open_file(&_fixture.path).unwrap();

    // Initially not dirty
    assert!(
        !harness.editor().is_active_buffer_recovery_dirty(),
        "Freshly opened buffer should not be recovery dirty"
    );

    // Apply a batch event with inserts
    let batch_event = Event::Batch {
        events: vec![
            Event::Insert {
                position: 0,
                text: "Hello".to_string(),
                cursor_id: CursorId(0),
            },
            Event::Insert {
                position: 5,
                text: " World".to_string(),
                cursor_id: CursorId(0),
            },
        ],
        description: "test batch".to_string(),
    };
    harness.apply_event(batch_event).unwrap();

    // Should be recovery dirty
    assert!(
        harness.editor().is_active_buffer_recovery_dirty(),
        "Buffer should be recovery dirty after Batch event with edits"
    );
}
