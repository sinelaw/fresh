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

/// Test that undo correctly updates the modified flag based on saved state:
/// When undoing after a save, the buffer should become unmodified when we reach
/// the saved state, NOT when we reach the original empty state.
///
/// This test verifies the expected behavior:
/// 1. Open file -> buffer is not modified
/// 2. Type some text -> buffer is modified
/// 3. Save -> buffer is no longer modified
/// 4. Type more -> buffer is modified again
/// 5. Undo back to saved state -> buffer should be NOT modified
#[test]
fn test_undo_returns_to_saved_state_not_original() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a file we can save to
    let fixture = TestFixture::new("test_undo_modified.txt", "").unwrap();
    harness.open_file(&fixture.path).unwrap();

    // 1. Initially unmodified (empty file)
    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "Fresh buffer should not be modified"
    );

    // 2. Type "Hello" -> buffer should be modified
    harness.type_text("Hello").unwrap();
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "Buffer should be modified after typing"
    );

    // 3. Save the buffer (Ctrl+S)
    harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL).unwrap();
    // Need to render to process the save
    harness.render().unwrap();

    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "Buffer should not be modified after save"
    );

    // Verify the save actually worked
    let saved_content = std::fs::read_to_string(&fixture.path).unwrap();
    assert_eq!(saved_content, "Hello", "File should contain 'Hello'");

    // 4. Type " World" -> buffer should be modified again
    harness.type_text(" World").unwrap();
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "Buffer should be modified after typing more"
    );

    // Content should now be "Hello World"
    assert_eq!(
        harness.get_buffer_content(),
        "Hello World"
    );

    // 5. Undo " World" (6 characters) to return to saved state "Hello"
    // Each character is a separate undo step
    for _ in 0..6 {
        harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    }

    // Content should be "Hello" (the saved state)
    assert_eq!(
        harness.get_buffer_content(),
        "Hello",
        "After undo, content should be 'Hello'"
    );

    // Verify the modified flag correctly reflects that we're back at the saved state
    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "After undoing to saved state, buffer should NOT be modified"
    );

    // 6. Now undo PAST the saved state (undo "Hello") -> should become modified again
    // because we're now different from the saved file content "Hello"
    for _ in 0..5 {
        harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    }

    // Content should be "" (empty, the original state)
    assert_eq!(
        harness.get_buffer_content(),
        "",
        "After more undos, content should be empty"
    );

    // Verify: When we undo past the saved state, we should become modified again
    // because the buffer content ("") is now different from the saved file ("Hello").
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "After undoing PAST saved state, buffer SHOULD be modified (content differs from saved file)"
    );
}
