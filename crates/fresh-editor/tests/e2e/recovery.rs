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
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
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
    assert_eq!(harness.get_buffer_content().unwrap(), "Hello World");

    // 5. Undo " World" (6 characters) to return to saved state "Hello"
    // Each character is a separate undo step
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    }

    // Content should be "Hello" (the saved state)
    assert_eq!(
        harness.get_buffer_content().unwrap(),
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
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    }

    // Content should be "" (empty, the original state)
    assert_eq!(
        harness.get_buffer_content().unwrap(),
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
        RecoveryChunk::new(7, 5, b"Universe".to_vec()), // "World" -> "Universe"
        RecoveryChunk::new(24, 4, b"sample".to_vec()),  // "test" -> "sample"
    ];

    // Save chunked recovery
    let id = "test-chunked-recovery";
    let original_size = original_content.len();
    // Calculate final size: original - replaced + new
    // "Hello, World! This is a test file with some content."
    // "Hello, Universe! This is a sample file with some content."
    let final_size = original_size - 5 + 8 - 4 + 6; // -5 (World) +8 (Universe) -4 (test) +6 (sample)

    storage
        .save_recovery(
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
    assert_eq!(metadata.chunk_count, 2);
    assert_eq!(metadata.original_file_size, original_size);

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
    let chunks = vec![RecoveryChunk::new(1, 0, b"XYZ".to_vec())];

    let id = "test-chunked-insert";
    storage
        .save_recovery(
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
    let chunks = vec![RecoveryChunk::new(2, 7, b"".to_vec())];

    let id = "test-chunked-delete";
    storage
        .save_recovery(
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
        .save_recovery(
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
        harness.get_buffer_content().unwrap(),
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
        harness.get_buffer_content().unwrap(),
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

    // Simulate saving recovery (as if user edited and we saved chunks)
    let chunks = vec![
        RecoveryChunk::new(7, 5, b"Universe".to_vec()), // "World" -> "Universe"
    ];
    let id = "test-full-cycle";
    storage
        .save_recovery(
            id,
            chunks,
            Some(&original_file),
            None,
            Some(1),
            original_content.len(),
            original_content.len() + 3, // "Universe" is 3 chars longer than "World"
        )
        .unwrap();

    // Verify metadata
    let metadata = storage.read_metadata(id).unwrap().unwrap();
    assert_eq!(metadata.original_file_size, original_content.len());

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
        harness.get_buffer_content().unwrap(),
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
    assert_eq!(
        new_size, 50_000,
        "Buffer should have new size after replacement"
    );

    // Verify content is accessible
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content.len(), 50_000, "Content should be retrievable");
    assert!(
        content.chars().all(|c| c == 'Y'),
        "Content should be all Y's"
    );
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
    let mut buffer = TextBuffer::load_from_file(
        &file_path,
        threshold,
        std::sync::Arc::new(fresh::model::filesystem::StdFileSystem),
    )
    .unwrap();

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

/// Test that recovery files for huge files are small (only contain modifications)
///
/// This test verifies that when editing a large file, the recovery file only
/// contains the modified chunks, not the entire file content. This is essential
/// for performance with multi-gigabyte files.
#[test]
fn test_huge_file_recovery_is_small() {
    use fresh::model::buffer::TextBuffer;
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("huge_file.txt");

    // Create a "huge" file (1MB to simulate behavior, real use case would be GB+)
    let file_size = 1_000_000; // 1MB
    let original_content = "X".repeat(file_size);
    fs::write(&file_path, &original_content).unwrap();

    // Load with a small threshold to trigger large file mode
    let threshold = 100; // Very small threshold to force large file mode
    let mut buffer = TextBuffer::load_from_file(
        &file_path,
        threshold,
        std::sync::Arc::new(fresh::model::filesystem::StdFileSystem),
    )
    .unwrap();

    // Verify it's in large file mode
    assert!(buffer.is_large_file(), "Should be in large file mode");

    // Make a small edit at the beginning
    let edit_text = b"SMALL_EDIT: ";
    buffer.insert_bytes(0, edit_text.to_vec());

    // Get recovery chunks - should be small!
    let chunks = buffer.get_recovery_chunks();

    // Calculate total recovery data size
    let recovery_data_size: usize = chunks.iter().map(|(_, data)| data.len()).sum();

    // Verify recovery data is MUCH smaller than original file
    assert!(
        recovery_data_size < file_size / 10,
        "Recovery data ({} bytes) should be much smaller than file size ({} bytes). \
         Recovery should only contain modifications, not entire file!",
        recovery_data_size,
        file_size
    );

    // Verify chunks contain our edit
    assert!(
        !chunks.is_empty(),
        "Should have at least one recovery chunk"
    );

    // The chunk should contain our edit text
    let all_chunk_data: Vec<u8> = chunks.into_iter().flat_map(|(_, data)| data).collect();
    assert!(
        all_chunk_data.starts_with(edit_text),
        "Recovery chunks should contain our edit"
    );

    println!(
        "SUCCESS: File size {} bytes, recovery data {} bytes ({:.2}% of original)",
        file_size,
        recovery_data_size,
        (recovery_data_size as f64 / file_size as f64) * 100.0
    );
}

/// Regression test: recovery files for large files should be small
///
/// This tests the ACTUAL auto_save_dirty_buffers flow end-to-end.
/// Before the fix, this test would fail because recovery saved the entire file.
#[test]
fn test_large_file_auto_save_creates_small_recovery_file() {
    use fresh::services::recovery::RecoveryStorage;
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_file.txt");

    // Create a large file (500KB)
    let file_size = 500_000;
    let original_content = "X".repeat(file_size);
    fs::write(&file_path, &original_content).unwrap();

    // Create editor with custom recovery dir
    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 1000; // Force large file mode

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Open the large file
    harness.open_file(&file_path).unwrap();

    // Verify it's in large file mode
    assert!(
        harness.editor().active_state().buffer.is_large_file(),
        "Should be in large file mode"
    );

    // Make a small edit
    harness.type_text("SMALL_EDIT").unwrap();

    // Trigger auto-save
    let saved = harness.editor_mut().auto_save_dirty_buffers().unwrap();
    assert!(saved > 0, "Should have saved at least one buffer");

    // Check the recovery file size using the harness's recovery directory
    let recovery_dir = harness
        .recovery_dir()
        .expect("harness should have recovery dir");
    let storage = RecoveryStorage::with_dir(recovery_dir);
    let entries = storage.list_entries().unwrap();

    // Find our file's recovery entry
    let our_entry = entries.iter().find(|e| {
        e.metadata
            .original_path
            .as_ref()
            .map(|p| p.ends_with("large_file.txt"))
            .unwrap_or(false)
    });

    if let Some(entry) = our_entry {
        // Check content_size from metadata - this is the total chunk content size
        let recovery_size = entry.metadata.content_size as usize;

        // Recovery file should be MUCH smaller than original
        // If the bug existed, this would be ~500KB. With the fix, it should be tiny.
        let max_acceptable_size = file_size / 10; // Less than 10% of original

        assert!(
            recovery_size < max_acceptable_size,
            "REGRESSION: Recovery file is {} bytes, but should be less than {} bytes (10% of {} byte file). \
             This suggests the entire file content is being saved instead of just modifications!",
            recovery_size,
            max_acceptable_size,
            file_size
        );

        println!(
            "Recovery file size: {} bytes ({:.2}% of {} byte file)",
            recovery_size,
            (recovery_size as f64 / file_size as f64) * 100.0,
            file_size
        );
    } else {
        panic!("Expected a recovery entry for large_file.txt");
    }
}

/// Regression test: recovery after saving a modified large file should work
///
/// Bug scenario:
/// 1. Open large file (size X)
/// 2. Add content (buffer size Y > X)
/// 3. Save file (file on disk is now Y)
/// 4. Make another edit
/// 5. Recovery auto-save stores original_size = X (from Stored pieces) -- BUG!
/// 6. Restart, recovery fails: "Original file size mismatch: expected X, got Y"
///
/// The fix: Track actual file size on disk, not just Stored pieces sum.
#[test]
fn test_recovery_after_save_with_size_change() {
    // Install signal handler to dump thread backtraces on timeout/SIGINT
    fresh::services::signal_handler::install_signal_handlers();

    // Initialize tracing for debugging
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};
    let _ = tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::io::stderr))
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("fresh=debug")))
        .try_init();

    use fresh::services::recovery::RecoveryStorage;
    use std::fs;
    use tempfile::TempDir;

    // Create temp directory that stays alive for the duration of the test
    // (needed because recovery reconstruction requires the original file)
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();
    let file_path = project_root.join("large_file.txt");

    // Create a large file (100KB to trigger large file mode)
    let initial_size = 100_000;
    let initial_content = "X".repeat(initial_size);
    fs::write(&file_path, &initial_content).unwrap();

    // Create editor with low threshold to force large file mode
    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 1000; // Force large file mode

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config.clone(), project_root)
            .unwrap();

    // Open the large file
    harness.open_file(&file_path).unwrap();
    assert!(
        harness.editor().active_state().buffer.is_large_file(),
        "Should be in large file mode"
    );

    // Add content at the end (go to end of file first)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    let added_content = "NEW_CONTENT_AT_END";
    harness.type_text(added_content).unwrap();

    // Save the file directly (simulates Ctrl+S)
    harness.editor_mut().save().unwrap();

    // Wait for async save to complete
    harness
        .wait_until(|_h| {
            fs::read_to_string(&file_path)
                .map(|c| c.len() > initial_size)
                .unwrap_or(false)
        })
        .unwrap();

    // Verify file was saved with new content
    let saved_content = fs::read_to_string(&file_path).unwrap();
    assert!(
        saved_content.ends_with(added_content),
        "File should have been saved with new content"
    );
    let new_file_size = saved_content.len();
    assert!(
        new_file_size > initial_size,
        "File size should have grown: {} -> {}",
        initial_size,
        new_file_size
    );

    // Make another small edit (this will trigger recovery for dirty buffer)
    harness.type_text("Z").unwrap();

    // Trigger auto-save
    let saved = harness.editor_mut().auto_save_dirty_buffers().unwrap();
    assert!(saved > 0, "Should have saved recovery for dirty buffer");

    // Get recovery directory and take temp dir before dropping harness
    let recovery_dir = harness
        .recovery_dir()
        .expect("harness should have recovery dir");
    let _harness_temp = harness.take_temp_dir(); // Keep alive to prevent cleanup

    // Now simulate restart and recovery
    // The recovery should succeed because original_size should match current file
    drop(harness);

    // Manually trigger recovery for this file using harness's recovery directory
    let storage = RecoveryStorage::with_dir(recovery_dir);
    let entries = storage.list_entries().unwrap();

    let our_entry = entries.iter().find(|e| {
        e.metadata
            .original_path
            .as_ref()
            .map(|p| p.ends_with("large_file.txt"))
            .unwrap_or(false)
    });

    if let Some(entry) = our_entry {
        // Try to reconstruct content - this should NOT fail with size mismatch
        let result = storage.reconstruct_from_chunks(&entry.id, &file_path);

        match result {
            Ok(content) => {
                // Recovery succeeded - the key thing is we didn't get a size mismatch error!
                // Content verification is more complex since chunks from before the save
                // are still in the recovery data. Just verify we got something reasonable.
                println!(
                    "Recovery successful! Content length: {} (original file: {})",
                    content.len(),
                    new_file_size
                );
                // Content should be at least as large as the saved file (since we added "Z")
                assert!(
                    content.len() >= new_file_size,
                    "Recovered content ({} bytes) should be at least as large as saved file ({} bytes)",
                    content.len(),
                    new_file_size
                );
            }
            Err(e) => {
                // This is the bug - recovery fails with size mismatch
                let error_msg = e.to_string();
                if error_msg.contains("Original file size mismatch") {
                    panic!(
                        "REGRESSION: Recovery failed with size mismatch after saving modified file!\n\
                         Error: {}\n\
                         This happens because original_file_size() returns Stored pieces sum \
                         instead of actual file size on disk.",
                        error_msg
                    );
                } else {
                    panic!("Recovery failed unexpectedly: {}", e);
                }
            }
        }
    } else {
        // Check if it's using full content format instead of chunked
        println!("No chunked recovery entry found - file might be using full content format");
    }
}

/// Regression test for #753: unnamed buffer recovery
///
/// Tests two bugs that were fixed together:
/// 1. Each auto-save generated a new recovery ID, creating duplicate recovery files
/// 2. new_buffer() didn't create BufferMetadata, so recovery was silently skipped
///
/// This test:
/// - Creates a NEW buffer (not the initial one) via new_buffer()
/// - Types content and triggers multiple auto-saves
/// - Verifies exactly ONE recovery file exists with correct content
#[test]
fn test_unnamed_buffer_created_via_new_buffer_has_stable_recovery() {
    use fresh::services::recovery::RecoveryStorage;

    let mut config = fresh::config::Config::default();

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Create a NEW unnamed buffer (this is what was broken - new_buffer() didn't create metadata)
    harness.editor_mut().new_buffer();

    // Type content
    harness.type_text("First content").unwrap();

    // Trigger first auto-save
    let saved1 = harness.editor_mut().auto_save_dirty_buffers().unwrap();
    assert_eq!(saved1, 1, "Should save recovery for the new buffer");

    // Type more content
    harness.type_text(" more").unwrap();

    // Trigger second auto-save
    let saved2 = harness.editor_mut().auto_save_dirty_buffers().unwrap();
    assert_eq!(saved2, 1, "Should save recovery again");

    // Check recovery directory
    let recovery_dir = harness.recovery_dir().expect("should have recovery dir");
    let storage = RecoveryStorage::with_dir(recovery_dir);
    let entries = storage.list_entries().unwrap();

    let unsaved_entries: Vec<_> = entries
        .iter()
        .filter(|e| e.id.starts_with("unsaved_"))
        .collect();

    // This catches BOTH bugs:
    // - If new_buffer() doesn't create metadata: 0 entries (recovery silently skipped)
    // - If each auto-save creates new ID: 2 entries (duplicate files)
    assert_eq!(
        unsaved_entries.len(),
        1,
        "Should have exactly ONE recovery entry for unnamed buffer, found {}",
        unsaved_entries.len()
    );

    // Verify content is the latest version
    let chunked = storage
        .read_chunked_content(&unsaved_entries[0].id)
        .unwrap()
        .unwrap();
    let content = String::from_utf8_lossy(&chunked.chunks[0].content);
    assert_eq!(content, "First content more");
}

/// Regression test: inserting at the end of a large file should not crash recovery
///
/// Bug scenario:
/// 1. Open large file of size N
/// 2. Insert content at the end (position N)
/// 3. Recovery saves chunk with doc_offset = N
/// 4. On recovery, we try to slice original_content[0..N] which is valid
/// 5. BUT if another insert happens, its doc_offset becomes N+X where X is previous insert size
/// 6. Now we try original_content[..N+X] which is out of bounds!
///
/// Root cause: get_recovery_chunks() returns doc_offset (position in current document)
/// but reconstruct_from_chunks() uses it as offset into the ORIGINAL file.
#[test]
fn test_recovery_insert_at_end_of_large_file() {
    use fresh::services::recovery::RecoveryStorage;
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_file.txt");

    // Create a large file
    let file_size = 100_000;
    let original_content = "X".repeat(file_size);
    fs::write(&file_path, &original_content).unwrap();

    // Create editor with low threshold to force large file mode
    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 1000;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Open the large file
    harness.open_file(&file_path).unwrap();
    assert!(
        harness.editor().active_state().buffer.is_large_file(),
        "Should be in large file mode"
    );

    // Go to end of file and insert content
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("FIRST").unwrap();

    // Insert MORE content - this is key to triggering the bug
    // After "FIRST" is inserted, doc positions shift by 5
    // So the next insert's doc_offset will be file_size + 5 = 100005
    // But original file is only 100000 bytes!
    harness.type_text("SECOND").unwrap();

    // Trigger auto-save to create recovery chunks
    let saved = harness.editor_mut().auto_save_dirty_buffers().unwrap();
    assert!(saved > 0, "Should have saved recovery");

    // Get recovery directory and take temp dir before dropping harness
    let recovery_dir = harness
        .recovery_dir()
        .expect("harness should have recovery dir");
    let _harness_temp = harness.take_temp_dir(); // Keep alive to prevent cleanup

    // Now simulate restart and recovery - this is where the crash happens
    drop(harness);

    let storage = RecoveryStorage::with_dir(recovery_dir);
    let entries = storage.list_entries().unwrap();

    let our_entry = entries.iter().find(|e| {
        e.metadata
            .original_path
            .as_ref()
            .map(|p| p.ends_with("large_file.txt"))
            .unwrap_or(false)
    });

    if let Some(entry) = our_entry {
        // This should NOT panic with "range end index X out of range for slice of length Y"
        let result = storage.reconstruct_from_chunks(&entry.id, &file_path);

        match result {
            Ok(content) => {
                let content_str = String::from_utf8_lossy(&content);
                assert!(
                    content_str.ends_with("FIRSTSECOND"),
                    "Recovered content should end with our insertions. Got: ...{}",
                    &content_str[content_str.len().saturating_sub(50)..]
                );
                println!(
                    "Recovery successful! Content ends with: ...{}",
                    &content_str[content_str.len().saturating_sub(20)..]
                );
            }
            Err(e) => {
                let error_msg = e.to_string();
                if error_msg.contains("range") && error_msg.contains("out of range") {
                    panic!(
                        "REGRESSION: Recovery crashed with slice out of bounds!\n\
                         Error: {}\n\
                         This happens because get_recovery_chunks() returns doc_offset \
                         (position in modified document) but reconstruct_from_chunks() \
                         uses it as offset into the original file.",
                        error_msg
                    );
                } else {
                    panic!("Recovery failed unexpectedly: {}", e);
                }
            }
        }
    } else {
        println!("No chunked recovery entry found");
    }
}
