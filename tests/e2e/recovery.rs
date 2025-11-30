// End-to-end tests for file recovery feature

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::buffer::TextBuffer;
use fresh::model::event::{CursorId, Event};
use fresh::services::recovery::{RecoveryChunk, RecoveryStorage};

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

/// Test chunked recovery reconstruction from original file + chunks
///
/// This tests the core chunked recovery mechanism:
/// 1. Create an original file with known content
/// 2. Save chunked recovery data representing modifications
/// 3. Reconstruct the full content from original + chunks
/// 4. Verify the reconstructed content matches expected result
#[test]
fn test_chunked_recovery_reconstruction() {
    use tempfile::TempDir;

    // Create a temp directory for recovery storage
    let temp_dir = TempDir::new().unwrap();
    let storage = RecoveryStorage::with_dir(temp_dir.path().to_path_buf());
    storage.ensure_dir().unwrap();

    // Create an original file with known content
    let original_content = b"Hello, World! This is a test file with some content.";
    let original_file = temp_dir.path().join("original.txt");
    std::fs::write(&original_file, original_content).unwrap();

    // Create chunks that represent modifications:
    // - Replace "World" with "Universe" (at offset 7, original_len 5)
    // - Replace "test" with "sample" (at offset 24, original_len 4)
    // "Hello, World! This is a test file with some content."
    //        ^                  ^
    //        7                  24
    let chunks = vec![
        RecoveryChunk::new(7, 5, b"Universe".to_vec()),  // "World" -> "Universe"
        RecoveryChunk::new(24, 4, b"sample".to_vec()),   // "test" -> "sample"
    ];

    // Save chunked recovery
    let id = "test-chunked-recovery";
    let original_size = original_content.len();
    // Calculate final size: original - replaced + new
    // "Hello, World! This is a test file with some content."
    // "Hello, Universe! This is a sample file with some content."
    let final_size = original_size - 5 + 8 - 4 + 6; // -5 (World) +8 (Universe) -4 (test) +6 (sample)

    storage
        .save_chunked_recovery(
            id,
            chunks,
            Some(&original_file),
            Some("test buffer"),
            Some(1),
            original_size,
            final_size,
        )
        .unwrap();

    // Verify metadata was saved correctly
    let metadata = storage.read_metadata(id).unwrap().unwrap();
    assert!(metadata.is_chunked());
    assert_eq!(metadata.chunk_count, Some(2));
    assert_eq!(metadata.original_file_size, Some(original_size));

    // Reconstruct content from chunks + original
    let reconstructed = storage.reconstruct_from_chunks(id, &original_file).unwrap();
    let reconstructed_str = String::from_utf8(reconstructed).unwrap();

    // Verify the reconstruction
    assert_eq!(
        reconstructed_str,
        "Hello, Universe! This is a sample file with some content."
    );
}

/// Test chunked recovery with insertion (new content longer than replaced)
#[test]
fn test_chunked_recovery_with_insertion() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let storage = RecoveryStorage::with_dir(temp_dir.path().to_path_buf());
    storage.ensure_dir().unwrap();

    // Original: "AB"
    let original_content = b"AB";
    let original_file = temp_dir.path().join("original_insert.txt");
    std::fs::write(&original_file, original_content).unwrap();

    // Insert "XYZ" between A and B (replace 0 chars at position 1)
    let chunks = vec![
        RecoveryChunk::new(1, 0, b"XYZ".to_vec()),
    ];

    let id = "test-chunked-insert";
    storage
        .save_chunked_recovery(
            id,
            chunks,
            Some(&original_file),
            None,
            None,
            original_content.len(),
            5, // "AXYZB"
        )
        .unwrap();

    let reconstructed = storage.reconstruct_from_chunks(id, &original_file).unwrap();
    assert_eq!(String::from_utf8(reconstructed).unwrap(), "AXYZB");
}

/// Test chunked recovery with deletion (replaced content longer than new)
#[test]
fn test_chunked_recovery_with_deletion() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let storage = RecoveryStorage::with_dir(temp_dir.path().to_path_buf());
    storage.ensure_dir().unwrap();

    // Original: "Hello World"
    let original_content = b"Hello World";
    let original_file = temp_dir.path().join("original_delete.txt");
    std::fs::write(&original_file, original_content).unwrap();

    // Delete "llo Wor" (replace 7 chars at position 2 with empty string)
    let chunks = vec![
        RecoveryChunk::new(2, 7, b"".to_vec()),
    ];

    let id = "test-chunked-delete";
    storage
        .save_chunked_recovery(
            id,
            chunks,
            Some(&original_file),
            None,
            None,
            original_content.len(),
            4, // "Held"
        )
        .unwrap();

    let reconstructed = storage.reconstruct_from_chunks(id, &original_file).unwrap();
    assert_eq!(String::from_utf8(reconstructed).unwrap(), "Held");
}

/// Test chunked recovery fails when original file size mismatches
#[test]
fn test_chunked_recovery_size_mismatch() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let storage = RecoveryStorage::with_dir(temp_dir.path().to_path_buf());
    storage.ensure_dir().unwrap();

    // Create recovery with a certain original size expectation
    let original_file = temp_dir.path().join("original_mismatch.txt");
    std::fs::write(&original_file, b"Short").unwrap();

    let chunks = vec![RecoveryChunk::new(0, 1, b"X".to_vec())];

    let id = "test-size-mismatch";
    storage
        .save_chunked_recovery(
            id,
            chunks,
            Some(&original_file),
            None,
            None,
            100, // Wrong size - file is only 5 bytes
            100,
        )
        .unwrap();

    // Reconstruction should fail due to size mismatch
    let result = storage.reconstruct_from_chunks(id, &original_file);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("size mismatch"));
}

/// Test the full recovery flow: open file, replace content, verify via get_all_text
/// This reproduces the flow in Editor::recover_all_buffers to ensure content is
/// properly accessible after recovery replacement.
#[test]
fn test_recovery_content_replacement_flow() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a file with original content
    let fixture = TestFixture::new("recovery_flow.txt", "original file content here").unwrap();
    harness.open_file(&fixture.path).unwrap();

    // Verify original content loaded
    assert_eq!(
        harness.get_buffer_content(),
        "original file content here",
        "File should load correctly"
    );

    // Simulate recovery replacement (same flow as recover_all_buffers)
    let recovered_text = "RECOVERED CONTENT";
    {
        let state = harness.editor_mut().active_state_mut();
        let total = state.buffer.total_bytes();
        state.buffer.delete(0..total);
        state.buffer.insert(0, recovered_text);
        state.buffer.set_modified(true);
    }

    // Verify content via get_buffer_content (which uses get_all_text internally)
    assert_eq!(
        harness.get_buffer_content(),
        "RECOVERED CONTENT",
        "Buffer should contain recovered content after replacement"
    );
}

/// Test the full chunked recovery cycle: save chunks, reconstruct, apply to buffer
/// This test simulates a crash recovery scenario with chunked data
#[test]
fn test_full_chunked_recovery_cycle() {
    use tempfile::TempDir;

    // Create a temp directory for recovery storage
    let temp_dir = TempDir::new().unwrap();
    let storage = RecoveryStorage::with_dir(temp_dir.path().to_path_buf());
    storage.ensure_dir().unwrap();

    // Create an original file - this simulates a file that was being edited
    let original_content = "Hello, World! This is the original file content.";
    let original_file = temp_dir.path().join("test_file.txt");
    std::fs::write(&original_file, original_content).unwrap();

    // Simulate saving chunked recovery (as if user edited and we saved chunks)
    let chunks = vec![
        RecoveryChunk::new(7, 5, b"Universe".to_vec()), // "World" -> "Universe"
    ];
    let id = "test-full-cycle";
    storage
        .save_chunked_recovery(
            id,
            chunks,
            Some(&original_file),
            None,
            Some(1),
            original_content.len(),
            original_content.len() + 3, // "Universe" is 3 chars longer than "World"
        )
        .unwrap();

    // Verify metadata shows chunked format
    let metadata = storage.read_metadata(id).unwrap().unwrap();
    assert!(metadata.is_chunked());

    // Reconstruct the content (this is what load_recovery does for chunked)
    let reconstructed = storage.reconstruct_from_chunks(id, &original_file).unwrap();
    let recovered_text = String::from_utf8(reconstructed).unwrap();

    assert_eq!(
        recovered_text,
        "Hello, Universe! This is the original file content."
    );

    // Now test applying this to a buffer (simulating recover_all_buffers flow)
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the original file
    harness.open_file(&original_file).unwrap();

    // Replace with recovered content
    {
        let state = harness.editor_mut().active_state_mut();
        let total = state.buffer.total_bytes();
        state.buffer.delete(0..total);
        state.buffer.insert(0, &recovered_text);
        state.buffer.set_modified(true);
    }

    // Verify content is correct
    assert_eq!(
        harness.get_buffer_content(),
        "Hello, Universe! This is the original file content.",
        "Buffer should have recovered content"
    );
}

/// Test recovery flow with large buffer that triggers chunked recovery threshold
/// This test verifies that even with large files, the recovery replacement works
#[test]
fn test_recovery_large_file_content_replacement() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a larger file (not huge, but enough to test the flow)
    let original_content = "X".repeat(100_000); // 100KB
    let fixture = TestFixture::new("large_recovery.txt", &original_content).unwrap();
    harness.open_file(&fixture.path).unwrap();

    // Verify original size
    let original_size = harness.editor().active_state().buffer.total_bytes();
    assert_eq!(original_size, 100_000, "File should be 100KB");

    // Simulate recovery replacement
    let recovered_text = "Y".repeat(50_000); // Different content, different size
    {
        let state = harness.editor_mut().active_state_mut();
        let total = state.buffer.total_bytes();
        state.buffer.delete(0..total);
        state.buffer.insert(0, &recovered_text);
        state.buffer.set_modified(true);
    }

    // Verify buffer has correct size
    let new_size = harness.editor().active_state().buffer.total_bytes();
    assert_eq!(new_size, 50_000, "Buffer should have new size after replacement");

    // Verify content is accessible
    let content = harness.get_buffer_content();
    assert_eq!(content.len(), 50_000, "Content should be retrievable");
    assert!(content.chars().all(|c| c == 'Y'), "Content should be all Y's");
}

/// Test that get_all_text works correctly for large files with unloaded regions
///
/// This is a regression test for a bug where:
/// 1. Large file opened with lazy loading (unloaded buffer)
/// 2. User makes edits (creates small loaded regions)
/// 3. get_all_text() returns empty because some regions are unloaded
/// 4. Recovery saves empty content
/// 5. On crash recovery, empty content is loaded
///
/// The fix requires using get_text_range_mut() which handles lazy loading.
#[test]
fn test_large_file_get_all_text_with_unloaded_regions() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_test.txt");

    // Create a file with content
    let original_content = "X".repeat(50_000); // 50KB
    fs::write(&file_path, &original_content).unwrap();

    // Load with a small threshold to trigger large file mode
    let threshold = 1024; // 1KB threshold makes 50KB file "large"
    let mut buffer = TextBuffer::load_from_file(&file_path, threshold).unwrap();

    // Verify we're in large file mode
    assert!(
        buffer.line_count().is_none(),
        "Should be in large file mode (no line indexing)"
    );

    // Make a small edit at the beginning
    buffer.insert_bytes(0, b"EDITED: ".to_vec());

    // The correct way: get_text_range_mut handles lazy loading
    let total_bytes = buffer.total_bytes();
    let content_via_mut = buffer.get_text_range_mut(0, total_bytes).unwrap();

    // This should work - get_text_range_mut loads unloaded regions on demand
    assert!(
        !content_via_mut.is_empty(),
        "get_text_range_mut() should return content for large files with edits. \
         Got {} bytes, expected {} bytes.",
        content_via_mut.len(),
        50_000 + 8
    );

    // Verify content starts with our edit
    let content_str = String::from_utf8_lossy(&content_via_mut);
    assert!(
        content_str.starts_with("EDITED: "),
        "Content should start with our edit"
    );

    // Verify we got all the content
    assert_eq!(
        content_via_mut.len(),
        50_000 + 8,
        "Should have original content plus edit"
    );
}
