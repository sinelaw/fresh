
use crate::model::filesystem::StdFileSystem;
use std::sync::Arc;

fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
    Arc::new(StdFileSystem)
}
use super::*;

#[test]
fn test_empty_buffer() {
    let buffer = TextBuffer::empty(test_fs());
    assert_eq!(buffer.total_bytes(), 0);
    assert_eq!(buffer.line_count(), Some(1)); // Empty doc has 1 line
}

#[test]
fn test_line_positions_multiline() {
    let buffer = TextBuffer::from_bytes(b"Hello\nNew Line\nWorld!".to_vec(), test_fs());

    // Check line count
    assert_eq!(buffer.line_count(), Some(3));

    // Check line starts
    assert_eq!(buffer.line_start_offset(0), Some(0)); // "Hello\n" starts at 0
    assert_eq!(buffer.line_start_offset(1), Some(6)); // "New Line\n" starts at 6
    assert_eq!(buffer.line_start_offset(2), Some(15)); // "World!" starts at 15

    // Check offset_to_position
    assert_eq!(buffer.offset_to_position(0).unwrap().line, 0); // Start of "Hello"
    assert_eq!(buffer.offset_to_position(5).unwrap().line, 0); // End of "Hello" (before \n)
    assert_eq!(buffer.offset_to_position(6).unwrap().line, 1); // Start of "New Line"
    assert_eq!(buffer.offset_to_position(14).unwrap().line, 1); // End of "New Line" (before \n)
    assert_eq!(buffer.offset_to_position(15).unwrap().line, 2); // Start of "World!"

    // Check line_col_to_position
    assert_eq!(buffer.line_col_to_position(0, 5), 5); // End of line 0
    assert_eq!(buffer.line_col_to_position(1, 0), 6); // Start of line 1
    assert_eq!(buffer.line_col_to_position(1, 8), 14); // End of line 1
    assert_eq!(buffer.line_col_to_position(2, 0), 15); // Start of line 2
}

#[test]
fn test_new_from_content() {
    let buffer = TextBuffer::from_bytes(b"hello\nworld".to_vec(), test_fs());
    assert_eq!(buffer.total_bytes(), 11);
    assert_eq!(buffer.line_count(), Some(2));
}

#[test]
fn test_get_all_text() {
    let buffer = TextBuffer::from_bytes(b"hello\nworld".to_vec(), test_fs());
    assert_eq!(buffer.get_all_text().unwrap(), b"hello\nworld");
}

#[test]
fn test_insert_at_start() {
    let mut buffer = TextBuffer::from_bytes(b"world".to_vec(), test_fs());
    buffer.insert_bytes(0, b"hello ".to_vec());

    assert_eq!(buffer.get_all_text().unwrap(), b"hello world");
    assert_eq!(buffer.total_bytes(), 11);
}

#[test]
fn test_insert_in_middle() {
    let mut buffer = TextBuffer::from_bytes(b"helloworld".to_vec(), test_fs());
    buffer.insert_bytes(5, b" ".to_vec());

    assert_eq!(buffer.get_all_text().unwrap(), b"hello world");
    assert_eq!(buffer.total_bytes(), 11);
}

#[test]
fn test_insert_at_end() {
    let mut buffer = TextBuffer::from_bytes(b"hello".to_vec(), test_fs());
    buffer.insert_bytes(5, b" world".to_vec());

    assert_eq!(buffer.get_all_text().unwrap(), b"hello world");
    assert_eq!(buffer.total_bytes(), 11);
}

#[test]
fn test_insert_with_newlines() {
    let mut buffer = TextBuffer::from_bytes(b"hello".to_vec(), test_fs());
    buffer.insert_bytes(5, b"\nworld\ntest".to_vec());

    assert_eq!(buffer.get_all_text().unwrap(), b"hello\nworld\ntest");
    assert_eq!(buffer.line_count(), Some(3));
}

#[test]
fn test_delete_from_start() {
    let mut buffer = TextBuffer::from_bytes(b"hello world".to_vec(), test_fs());
    buffer.delete_bytes(0, 6);

    assert_eq!(buffer.get_all_text().unwrap(), b"world");
    assert_eq!(buffer.total_bytes(), 5);
}

#[test]
fn test_delete_from_middle() {
    let mut buffer = TextBuffer::from_bytes(b"hello world".to_vec(), test_fs());
    buffer.delete_bytes(5, 1);

    assert_eq!(buffer.get_all_text().unwrap(), b"helloworld");
    assert_eq!(buffer.total_bytes(), 10);
}

#[test]
fn test_delete_from_end() {
    let mut buffer = TextBuffer::from_bytes(b"hello world".to_vec(), test_fs());
    buffer.delete_bytes(6, 5);

    assert_eq!(buffer.get_all_text().unwrap(), b"hello ");
    assert_eq!(buffer.total_bytes(), 6);
}

#[test]
fn test_delete_with_newlines() {
    let mut buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());
    buffer.delete_bytes(5, 7); // Delete "\nworld\n"

    assert_eq!(buffer.get_all_text().unwrap(), b"hellotest");
    assert_eq!(buffer.line_count(), Some(1));
}

#[test]
fn test_offset_position_conversions() {
    let buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());

    let pos = buffer.offset_to_position(0);
    assert_eq!(pos, Some(Position { line: 0, column: 0 }));

    let pos = buffer.offset_to_position(6);
    assert_eq!(pos, Some(Position { line: 1, column: 0 }));

    let offset = buffer.position_to_offset(Position { line: 1, column: 0 });
    assert_eq!(offset, 6);
}

#[test]
fn test_insert_at_position() {
    let mut buffer = TextBuffer::from_bytes(b"hello\nworld".to_vec(), test_fs());
    buffer.insert_at_position(Position { line: 1, column: 0 }, b"beautiful ".to_vec());

    assert_eq!(buffer.get_all_text().unwrap(), b"hello\nbeautiful world");
}

#[test]
fn test_delete_range() {
    let mut buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());

    let start = Position { line: 0, column: 5 };
    let end = Position { line: 2, column: 0 };
    buffer.delete_range(start, end);

    assert_eq!(buffer.get_all_text().unwrap(), b"hellotest");
}

#[test]
fn test_get_line() {
    let buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());

    assert_eq!(buffer.get_line(0), Some(b"hello\n".to_vec()));
    assert_eq!(buffer.get_line(1), Some(b"world\n".to_vec()));
    assert_eq!(buffer.get_line(2), Some(b"test".to_vec()));
    assert_eq!(buffer.get_line(3), None);
}

#[test]
fn test_multiple_operations() {
    let mut buffer = TextBuffer::from_bytes(b"line1\nline2\nline3".to_vec(), test_fs());

    buffer.insert_bytes(0, b"start\n".to_vec());
    assert_eq!(buffer.line_count(), Some(4));

    buffer.delete_bytes(6, 6); // Delete "line1\n"
    assert_eq!(buffer.line_count(), Some(3));

    buffer.insert_bytes(6, b"new\n".to_vec());
    assert_eq!(buffer.line_count(), Some(4));

    let text = buffer.get_all_text().unwrap();
    assert_eq!(text, b"start\nnew\nline2\nline3");
}

#[test]
fn test_get_text_range() {
    let buffer = TextBuffer::from_bytes(b"hello world".to_vec(), test_fs());

    assert_eq!(buffer.get_text_range(0, 5), Some(b"hello".to_vec()));
    assert_eq!(buffer.get_text_range(6, 5), Some(b"world".to_vec()));
    assert_eq!(buffer.get_text_range(0, 11), Some(b"hello world".to_vec()));
}

#[test]
fn test_empty_operations() {
    let mut buffer = TextBuffer::from_bytes(b"hello".to_vec(), test_fs());

    buffer.insert_bytes(2, Vec::new());
    assert_eq!(buffer.get_all_text().unwrap(), b"hello");

    buffer.delete_bytes(2, 0);
    assert_eq!(buffer.get_all_text().unwrap(), b"hello");
}

#[test]
fn test_sequential_inserts_at_beginning() {
    // Regression test for piece tree duplicate insertion bug
    let mut buffer = TextBuffer::from_bytes(b"initial\ntext".to_vec(), test_fs());

    // Delete all
    buffer.delete_bytes(0, 12);
    assert_eq!(buffer.get_all_text().unwrap(), b"");

    // Insert 'a' at 0
    buffer.insert_bytes(0, vec![b'a']);
    assert_eq!(buffer.get_all_text().unwrap(), b"a");

    // Insert 'b' at 0 (should give "ba")
    buffer.insert_bytes(0, vec![b'b']);
    assert_eq!(buffer.get_all_text().unwrap(), b"ba");
}

// ===== Phase 1-3: Large File Support Tests =====

mod large_file_support {
    use super::*;
    use crate::model::piece_tree::StringBuffer;
    use std::fs::File;
    use std::io::Write;
    use tempfile::TempDir;

    // Phase 1: Option<usize> Type Safety Tests

    #[test]
    fn test_line_feed_count_is_some_for_loaded_buffer() {
        let buffer = StringBuffer::new(0, b"hello\nworld\ntest".to_vec());
        assert_eq!(buffer.line_feed_count(), Some(2));
    }

    #[test]
    fn test_line_feed_count_is_none_for_unloaded_buffer() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");

        let buffer = StringBuffer::new_unloaded(0, file_path, 0, 100);
        assert_eq!(buffer.line_feed_count(), None);
    }

    #[test]
    fn test_line_count_is_some_for_small_buffer() {
        let buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());
        assert_eq!(buffer.line_count(), Some(3));
    }

    #[test]
    fn test_piece_tree_works_with_none_line_count() {
        // Create a buffer with no line count information
        let buffer = StringBuffer::new_loaded(0, b"hello\nworld".to_vec(), false);
        assert_eq!(buffer.line_feed_count(), None);

        // Create piece tree without line feed count
        use crate::model::piece_tree::{BufferLocation, PieceTree};
        let tree = PieceTree::new(BufferLocation::Stored(0), 0, 11, None);

        // line_count should return None
        assert_eq!(tree.line_count(), None);
    }

    // Phase 2: BufferData Enum Tests

    #[test]
    fn test_buffer_data_loaded_variant() {
        let data = b"hello world".to_vec();
        let buffer = StringBuffer::new_loaded(0, data.clone(), true);

        assert!(buffer.is_loaded());
        assert_eq!(buffer.get_data(), Some(&data[..]));
        assert!(buffer.get_line_starts().is_some());
    }

    #[test]
    fn test_buffer_data_loaded_without_line_starts() {
        let data = b"hello\nworld".to_vec();
        let buffer = StringBuffer::new_loaded(0, data.clone(), false);

        assert!(buffer.is_loaded());
        assert_eq!(buffer.get_data(), Some(&data[..]));
        assert_eq!(buffer.get_line_starts(), None); // No line indexing
    }

    #[test]
    fn test_buffer_data_unloaded_variant() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");

        let buffer = StringBuffer::new_unloaded(0, file_path.clone(), 0, 100);

        assert!(!buffer.is_loaded());
        assert_eq!(buffer.get_data(), None);
        assert_eq!(buffer.get_line_starts(), None);
    }

    #[test]
    fn test_buffer_load_method() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");

        // Create test file
        let test_data = b"hello world";
        File::create(&file_path)
            .unwrap()
            .write_all(test_data)
            .unwrap();

        // Create unloaded buffer
        let mut buffer = StringBuffer::new_unloaded(0, file_path, 0, test_data.len());
        assert!(!buffer.is_loaded());

        // Load the buffer using local filesystem
        let fs = crate::model::filesystem::StdFileSystem;
        buffer.load(&fs).unwrap();

        // Now it should be loaded
        assert!(buffer.is_loaded());
        assert_eq!(buffer.get_data(), Some(&test_data[..]));
    }

    #[test]
    fn test_string_buffer_new_vs_new_loaded() {
        let data = b"hello\nworld".to_vec();

        // StringBuffer::new should compute line starts
        let buf1 = StringBuffer::new(0, data.clone());
        assert!(buf1.is_loaded());
        assert!(buf1.get_line_starts().is_some());
        assert_eq!(buf1.line_feed_count(), Some(1));

        // StringBuffer::new_loaded with compute_lines=false should not
        let buf2 = StringBuffer::new_loaded(0, data.clone(), false);
        assert!(buf2.is_loaded());
        assert_eq!(buf2.get_line_starts(), None);
        assert_eq!(buf2.line_feed_count(), None);
    }

    // Phase 3: Large File Detection Tests

    #[test]
    fn test_load_small_file_eager_loading() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("small.txt");

        // Create a small file (10 bytes < 100MB threshold)
        let test_data = b"hello\ntest";
        File::create(&file_path)
            .unwrap()
            .write_all(test_data)
            .unwrap();

        // Load with default threshold
        let buffer = TextBuffer::load_from_file(&file_path, 0, test_fs()).unwrap();

        // Should be eagerly loaded (not large_file mode)
        assert!(!buffer.file_kind.is_large_file());
        assert_eq!(buffer.total_bytes(), test_data.len());
        assert_eq!(buffer.line_count(), Some(2)); // Has line indexing
        assert_eq!(buffer.get_all_text().unwrap(), test_data);

        // The buffer should be loaded
        assert!(buffer.buffers[0].is_loaded());
    }

    #[test]
    fn test_load_large_file_lazy_loading() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("large.txt");

        // Create a "large" file by using a small threshold
        let test_data = b"hello\nworld\ntest";
        File::create(&file_path)
            .unwrap()
            .write_all(test_data)
            .unwrap();

        // Load with threshold of 10 bytes (file is 17 bytes, so it's "large")
        let buffer = TextBuffer::load_from_file(&file_path, 10, test_fs()).unwrap();

        // Should be in large_file mode
        assert!(buffer.file_kind.is_large_file());
        assert_eq!(buffer.total_bytes(), test_data.len());

        // Should NOT have line indexing
        assert_eq!(buffer.line_count(), None);

        // The buffer should be unloaded
        assert!(!buffer.buffers[0].is_loaded());
        assert_eq!(buffer.buffers[0].get_data(), None);
    }

    /// Test that reproduces issue #657: Search on large plain text files
    ///
    /// The bug: When a large file is opened with lazy loading, buffer.to_string()
    /// returns None because some buffers are unloaded. This causes search to fail
    /// with "Buffer not fully loaded" error.
    ///
    /// The fix: Use get_text_range_mut() which loads the buffer on demand.
    #[test]
    fn test_issue_657_search_on_large_file_unloaded_buffer() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("large_search_test.txt");

        // Create test content with a searchable string
        let test_data = b"line1\nline2\nSEARCH_TARGET\nline4\nline5";
        File::create(&file_path)
            .unwrap()
            .write_all(test_data)
            .unwrap();

        // Load with small threshold to force lazy loading
        let mut buffer = TextBuffer::load_from_file(&file_path, 10, test_fs()).unwrap();

        // Verify we're in large file mode with unloaded buffer
        assert!(
            buffer.file_kind.is_large_file(),
            "Buffer should be in large file mode"
        );
        assert!(
            !buffer.buffers[0].is_loaded(),
            "Buffer should be unloaded initially"
        );

        // REPRODUCE THE BUG: to_string() returns None for unloaded buffers
        // This is what the old perform_search() code did, causing the error
        assert!(
            buffer.to_string().is_none(),
            "BUG REPRODUCED: to_string() returns None for unloaded buffer"
        );

        // THE FIX: get_text_range_mut() loads the buffer on demand
        let total_bytes = buffer.len();
        let content = buffer.get_text_range_mut(0, total_bytes).unwrap();
        let content_str = String::from_utf8_lossy(&content);

        // Verify the content is now available and contains our search target
        assert!(
            content_str.contains("SEARCH_TARGET"),
            "FIX WORKS: get_text_range_mut() loaded the buffer and found the search target"
        );

        // After loading, to_string() should also work
        assert!(
            buffer.to_string().is_some(),
            "After get_text_range_mut(), to_string() should work"
        );
    }

    #[test]
    fn test_large_file_threshold_boundary() {
        let temp_dir = TempDir::new().unwrap();

        // Test exactly at threshold
        let file_path = temp_dir.path().join("at_threshold.txt");
        let test_data = vec![b'x'; 100];
        File::create(&file_path)
            .unwrap()
            .write_all(&test_data)
            .unwrap();

        // Load with threshold of 100 bytes - should be large file (>= threshold)
        let buffer = TextBuffer::load_from_file(&file_path, 100, test_fs()).unwrap();
        assert!(buffer.file_kind.is_large_file());

        // Test just below threshold
        let file_path2 = temp_dir.path().join("below_threshold.txt");
        let test_data2 = vec![b'x'; 99];
        File::create(&file_path2)
            .unwrap()
            .write_all(&test_data2)
            .unwrap();

        // Load with threshold of 100 bytes - should be small file (< threshold)
        let buffer2 = TextBuffer::load_from_file(&file_path2, 100, test_fs()).unwrap();
        assert!(!buffer2.file_kind.is_large_file());
    }

    #[test]
    fn test_large_file_default_threshold() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");

        // Create a small file
        File::create(&file_path)
            .unwrap()
            .write_all(b"hello")
            .unwrap();

        // Load with threshold 0 - should use DEFAULT_LARGE_FILE_THRESHOLD
        let buffer = TextBuffer::load_from_file(&file_path, 0, test_fs()).unwrap();

        // 5 bytes < 100MB, so should not be large file
        assert!(!buffer.file_kind.is_large_file());
    }

    #[test]
    fn test_large_file_has_correct_piece_tree_structure() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("large.txt");

        let test_data = b"hello world";
        File::create(&file_path)
            .unwrap()
            .write_all(test_data)
            .unwrap();

        // Load as large file
        let buffer = TextBuffer::load_from_file(&file_path, 5, test_fs()).unwrap();

        // Should have correct total bytes
        assert_eq!(buffer.total_bytes(), test_data.len());

        // Should have 1 buffer
        assert_eq!(buffer.buffers.len(), 1);

        // Buffer should be unloaded
        assert!(!buffer.buffers[0].is_loaded());
    }

    #[test]
    fn test_empty_large_file() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("empty.txt");

        // Create an empty file
        File::create(&file_path).unwrap();

        // Load as large file
        let buffer = TextBuffer::load_from_file(&file_path, 0, test_fs()).unwrap();

        // Empty file is handled gracefully
        assert_eq!(buffer.total_bytes(), 0);
        assert!(buffer.is_empty());
    }

    #[test]
    fn test_large_file_basic_api_operations() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("large_test.txt");

        // Create a test file with known content
        let test_data = b"line1\nline2\nline3\nline4\n";
        File::create(&file_path)
            .unwrap()
            .write_all(test_data)
            .unwrap();

        // Load as large file (use small threshold to trigger large file mode)
        let mut buffer = TextBuffer::load_from_file(&file_path, 10, test_fs()).unwrap();

        // Verify it's in large file mode
        assert!(buffer.file_kind.is_large_file());
        assert_eq!(buffer.line_count(), None); // No line indexing

        // Test basic access functions
        assert_eq!(buffer.total_bytes(), test_data.len());
        assert!(!buffer.is_empty());
        assert_eq!(buffer.len(), test_data.len());

        // Test reading operations using get_text_range_mut (lazy loads on demand)
        let range_result = buffer.get_text_range_mut(0, 5).unwrap();
        assert_eq!(range_result, b"line1");

        let range_result2 = buffer.get_text_range_mut(6, 5).unwrap();
        assert_eq!(range_result2, b"line2");

        // Test get_all_text (via get_text_range after lazy loading)
        let all_text = buffer.get_all_text().unwrap();
        assert_eq!(all_text, test_data);

        // Test slice_bytes method
        assert_eq!(buffer.slice_bytes(0..5), b"line1");

        // Test basic editing operations
        // Insert at offset 0
        buffer.insert_bytes(0, b"prefix_".to_vec());
        assert_eq!(buffer.total_bytes(), test_data.len() + 7);
        assert!(buffer.is_modified());

        // Verify the insertion worked
        let text_after_insert = buffer.get_all_text().unwrap();
        assert_eq!(&text_after_insert[0..7], b"prefix_");
        assert_eq!(&text_after_insert[7..12], b"line1");

        // Delete some bytes
        buffer.delete_bytes(0, 7);
        assert_eq!(buffer.total_bytes(), test_data.len());

        // Verify deletion worked - should be back to original
        let text_after_delete = buffer.get_all_text().unwrap();
        assert_eq!(text_after_delete, test_data);

        // Insert at end
        let end_offset = buffer.total_bytes();
        buffer.insert_bytes(end_offset, b"suffix".to_vec());
        assert_eq!(buffer.total_bytes(), test_data.len() + 6);

        // Verify end insertion
        let final_text = buffer.get_all_text().unwrap();
        assert!(final_text.ends_with(b"suffix"));
        assert_eq!(&final_text[0..test_data.len()], test_data);

        // Test offset_to_position
        // Note: Without line indexing, position tracking is limited
        // but byte-level operations still work
        let pos = buffer.offset_to_position(0).unwrap();
        assert_eq!(pos.column, 0);

        // Test position_to_offset
        let offset = buffer.position_to_offset(Position { line: 0, column: 0 });
        assert_eq!(offset, 0);

        // Test replace operations
        let replace_result = buffer.replace_range(0..5, "START");
        assert!(replace_result);

        let text_after_replace = buffer.get_all_text().unwrap();
        assert!(text_after_replace.starts_with(b"START"));
    }

    #[test]
    fn test_large_file_chunk_based_loading() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("huge.txt");

        // Create a file larger than LOAD_CHUNK_SIZE (1MB)
        // We'll create a 3MB file with a repeating pattern so we can verify chunks
        let chunk_size = LOAD_CHUNK_SIZE; // 1MB
        let file_size = chunk_size * 3; // 3MB

        // Pattern: "AAAA...AAAA" (1MB of A's), "BBBB...BBBB" (1MB of B's), "CCCC...CCCC" (1MB of C's)
        let mut file = File::create(&file_path).unwrap();
        file.write_all(&vec![b'A'; chunk_size]).unwrap();
        file.write_all(&vec![b'B'; chunk_size]).unwrap();
        file.write_all(&vec![b'C'; chunk_size]).unwrap();
        file.flush().unwrap();

        // Load as large file (use threshold of 1 byte to ensure large file mode)
        let mut buffer = TextBuffer::load_from_file(&file_path, 1, test_fs()).unwrap();

        // Verify it's in large file mode
        assert!(buffer.file_kind.is_large_file());
        assert_eq!(buffer.total_bytes(), file_size);

        // Buffer should be unloaded initially
        assert!(!buffer.buffers[0].is_loaded());

        // Read from the first chunk (should load only first 1MB)
        let first_chunk_data = buffer.get_text_range_mut(0, 1024).unwrap();
        assert_eq!(first_chunk_data.len(), 1024);
        assert!(first_chunk_data.iter().all(|&b| b == b'A'));

        // Read from the middle chunk (offset = 1MB, should load second 1MB)
        let second_chunk_data = buffer.get_text_range_mut(chunk_size, 1024).unwrap();
        assert_eq!(second_chunk_data.len(), 1024);
        assert!(second_chunk_data.iter().all(|&b| b == b'B'));

        // Read from the last chunk (offset = 2MB, should load third 1MB)
        let third_chunk_data = buffer.get_text_range_mut(chunk_size * 2, 1024).unwrap();
        assert_eq!(third_chunk_data.len(), 1024);
        assert!(third_chunk_data.iter().all(|&b| b == b'C'));

        // Verify we can read across chunk boundaries
        // Read from middle of first chunk to middle of second chunk
        let cross_chunk_offset = chunk_size - 512;
        let cross_chunk_data = buffer.get_text_range_mut(cross_chunk_offset, 1024).unwrap();
        assert_eq!(cross_chunk_data.len(), 1024);
        // First 512 bytes should be 'A', next 512 bytes should be 'B'
        assert!(cross_chunk_data[..512].iter().all(|&b| b == b'A'));
        assert!(cross_chunk_data[512..].iter().all(|&b| b == b'B'));

        // After chunk-based loading, verify the piece tree has been split
        // The number of buffers should be greater than 1 (original + chunks)
        assert!(
            buffer.buffers.len() > 1,
            "Expected multiple buffers after chunk-based loading, got {}",
            buffer.buffers.len()
        );

        // Test that editing still works after chunk-based loading
        buffer.insert_bytes(0, b"PREFIX".to_vec());
        assert_eq!(buffer.total_bytes(), file_size + 6);

        let after_insert = buffer.get_text_range_mut(0, 6).unwrap();
        assert_eq!(after_insert, b"PREFIX");

        // Verify the original data is still there after the prefix
        let after_prefix = buffer.get_text_range_mut(6, 10).unwrap();
        assert!(after_prefix.iter().all(|&b| b == b'A'));

        // Most importantly: validate the entire buffer content matches the original file
        // Create a fresh buffer to read the original file
        let mut buffer2 = TextBuffer::load_from_file(&file_path, 1, test_fs()).unwrap();

        // Read the entire file in chunks and verify each chunk
        let chunk_read_size = 64 * 1024; // Read in 64KB chunks for efficiency
        let mut offset = 0;
        while offset < file_size {
            let bytes_to_read = chunk_read_size.min(file_size - offset);
            let chunk_data = buffer2.get_text_range_mut(offset, bytes_to_read).unwrap();

            // Determine which section of the file we're reading
            let first_mb_end = chunk_size;
            let second_mb_end = chunk_size * 2;

            // Validate the data based on which MB section we're in
            for (i, &byte) in chunk_data.iter().enumerate() {
                let file_offset = offset + i;
                let expected = if file_offset < first_mb_end {
                    b'A'
                } else if file_offset < second_mb_end {
                    b'B'
                } else {
                    b'C'
                };
                assert_eq!(
                    byte, expected,
                    "Mismatch at file offset {}: expected {}, got {}",
                    file_offset, expected as char, byte as char
                );
            }

            offset += bytes_to_read;
        }
    }

    /// Test that save_to_file works correctly with partially loaded large files
    /// This is a regression test for a bug where saving would silently produce
    /// an empty file if any buffer regions were still unloaded.
    #[test]
    fn test_large_file_incremental_save() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("large_save_test.txt");

        // Create a small file but use tiny threshold to trigger large file mode
        let chunk_size = 1000; // 1KB chunks
        let file_size = chunk_size * 2; // 2KB total

        let mut file = File::create(&file_path).unwrap();
        // First half: 'A' repeated
        file.write_all(&vec![b'A'; chunk_size]).unwrap();
        // Second half: 'B' repeated
        file.write_all(&vec![b'B'; chunk_size]).unwrap();
        file.flush().unwrap();

        // Load as large file (threshold of 100 bytes)
        let mut buffer = TextBuffer::load_from_file(&file_path, 100, test_fs()).unwrap();
        assert!(buffer.file_kind.is_large_file());
        assert_eq!(buffer.total_bytes(), file_size);

        // Only read from the beginning - this loads only a small region
        let first_bytes = buffer.get_text_range_mut(0, 50).unwrap();
        assert!(first_bytes.iter().all(|&b| b == b'A'));

        // Make an edit at the beginning
        buffer.insert_bytes(0, b"PREFIX_".to_vec());

        // Save to a new file (to avoid issues with reading while writing same file)
        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        // Verify the saved file
        let saved_content = std::fs::read(&save_path).unwrap();

        // Check total size: original + "PREFIX_" (7 bytes)
        assert_eq!(
            saved_content.len(),
            file_size + 7,
            "Saved file should be {} bytes, got {}",
            file_size + 7,
            saved_content.len()
        );

        // Check prefix
        assert_eq!(&saved_content[..7], b"PREFIX_", "Should start with PREFIX_");

        // Check that first chunk (after prefix) contains A's
        assert!(
            saved_content[7..100].iter().all(|&b| b == b'A'),
            "First chunk after prefix should be A's"
        );

        // Check that second chunk contains B's (this was unloaded!)
        let second_chunk_start = 7 + chunk_size;
        assert!(
            saved_content[second_chunk_start..second_chunk_start + 100]
                .iter()
                .all(|&b| b == b'B'),
            "Second chunk should be B's (was unloaded, should be preserved)"
        );
    }

    /// Test that save_to_file handles edits at multiple positions
    #[test]
    fn test_large_file_save_with_multiple_edits() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("multi_edit.txt");

        // Create a ~5KB file with numbered lines for easier verification
        let mut content = Vec::new();
        for i in 0..100 {
            content.extend_from_slice(
                format!("Line {:04}: padding to make it longer\n", i).as_bytes(),
            );
        }
        let original_len = content.len();
        std::fs::write(&file_path, &content).unwrap();

        // Load as large file (threshold of 500 bytes)
        let mut buffer = TextBuffer::load_from_file(&file_path, 500, test_fs()).unwrap();
        assert!(
            buffer.line_count().is_none(),
            "Should be in large file mode"
        );

        // Edit at the beginning
        buffer.insert_bytes(0, b"[START]".to_vec());

        // Edit somewhere in the middle (load that region first)
        let mid_offset = original_len / 2;
        let _mid_bytes = buffer.get_text_range_mut(mid_offset + 7, 10).unwrap(); // +7 for our insert
        buffer.insert_bytes(mid_offset + 7, b"[MIDDLE]".to_vec());

        // Save
        let save_path = temp_dir.path().join("multi_edit_saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        // Verify
        let saved = std::fs::read_to_string(&save_path).unwrap();

        assert!(
            saved.starts_with("[START]Line 0000"),
            "Should start with our edit"
        );
        assert!(saved.contains("[MIDDLE]"), "Should contain middle edit");
        assert!(saved.contains("Line 0099"), "Should preserve end of file");

        // Verify total length
        let expected_len = original_len + 7 + 8; // [START] + [MIDDLE]
        assert_eq!(
            saved.len(),
            expected_len,
            "Length should be original + edits"
        );
    }
}

// ===== Offset to Position Tests =====
// These tests focus on the offset_to_position correctness

#[test]
fn test_offset_to_position_simple() {
    // Create a buffer with known line structure
    // Line 0: "a\n" (bytes 0-1, newline at 1)
    // Line 1: "b\n" (bytes 2-3, newline at 3)
    // Line 2: "c\n" (bytes 4-5, newline at 5)
    // Line 3: "d" (bytes 6, no newline)
    let content = b"a\nb\nc\nd";
    let buffer = TextBuffer::from_bytes(content.to_vec(), test_fs());

    // Verify specific positions
    let pos = buffer
        .offset_to_position(0)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 0, "Byte 0 should be on line 0");
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(1)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 0, "Byte 1 (newline) should be on line 0");
    assert_eq!(pos.column, 1);

    let pos = buffer
        .offset_to_position(2)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 1, "Byte 2 should be on line 1");
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(3)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 1, "Byte 3 (newline) should be on line 1");
    assert_eq!(pos.column, 1);

    let pos = buffer
        .offset_to_position(4)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 2, "Byte 4 should be on line 2");
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(6)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 3, "Byte 6 should be on line 3");
    assert_eq!(pos.column, 0);
}

#[test]
fn test_offset_to_position_after_insert() {
    // Start with simple content
    let mut buffer = TextBuffer::from_bytes(b"a\nb\n".to_vec(), test_fs());

    // Insert at position 2 (start of line 1)
    buffer.insert_at_position(Position { line: 1, column: 0 }, b"x\n".to_vec());

    // After insert, buffer should be: "a\nx\nb\n"
    // Line 0: "a\n" (bytes 0-1)
    // Line 1: "x\n" (bytes 2-3)
    // Line 2: "b\n" (bytes 4-5)

    let pos = buffer
        .offset_to_position(0)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 0, "Byte 0 should still be on line 0");

    let pos = buffer
        .offset_to_position(2)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 1,
        "Byte 2 (start of inserted line) should be on line 1"
    );

    let pos = buffer
        .offset_to_position(4)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 2,
        "Byte 4 (start of 'b') should be on line 2 after insert"
    );
}

#[test]
fn test_offset_to_position_empty_lines() {
    // Test with empty lines: "\n\n\n"
    let buffer = TextBuffer::from_bytes(b"\n\n\n".to_vec(), test_fs());

    // Line 0: "\n" (byte 0)
    // Line 1: "\n" (byte 1)
    // Line 2: "\n" (byte 2)
    // Line 3: "" (empty, after last newline)

    let pos = buffer
        .offset_to_position(0)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 0, "Byte 0 should be on line 0");

    let pos = buffer
        .offset_to_position(1)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 1, "Byte 1 should be on line 1");

    let pos = buffer
        .offset_to_position(2)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 2, "Byte 2 should be on line 2");

    let pos = buffer
        .offset_to_position(3)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 3, "Byte 3 (EOF) should be on line 3");
}

#[test]
fn test_offset_to_position_long_lines() {
    // Test with long lines to ensure it's not just line counting
    let mut content = Vec::new();
    content.extend_from_slice(b"aaaaaaaaaa\n"); // Line 0: 11 bytes (10 'a's + newline)
    content.extend_from_slice(b"bbbbbbbbbb\n"); // Line 1: 11 bytes
    content.extend_from_slice(b"cccccccccc"); // Line 2: 10 bytes (no newline)

    let buffer = TextBuffer::from_bytes(content.clone(), test_fs());

    // Test positions at start of each line
    let pos = buffer
        .offset_to_position(0)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 0, "Byte 0 should be on line 0");
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(11)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 1, "Byte 11 (start of line 1) should be on line 1");
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(22)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 2, "Byte 22 (start of line 2) should be on line 2");
    assert_eq!(pos.column, 0);

    // Test mid-line positions
    let pos = buffer
        .offset_to_position(5)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 0, "Byte 5 should be on line 0");
    assert_eq!(pos.column, 5);

    let pos = buffer
        .offset_to_position(16)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 1, "Byte 16 should be on line 1");
    assert_eq!(pos.column, 5);
}

#[test]
fn test_line_iterator_with_offset_to_position() {
    // This combines line iterator with offset_to_position to find issues
    let mut buffer = TextBuffer::from_bytes(b"line0\nline1\nline2\n".to_vec(), test_fs());

    // Test creating line iterator at various positions
    for byte_pos in 0..=buffer.len() {
        let iter = buffer.line_iterator(byte_pos, 80);
        let iter_pos = iter.current_position();
        let expected_line = buffer
            .offset_to_position(byte_pos)
            .expect("small buffer should have line metadata")
            .line;
        let expected_line_start = buffer.position_to_offset(Position {
            line: expected_line,
            column: 0,
        });

        assert_eq!(
            iter_pos, expected_line_start,
            "LineIterator at byte {} should position at line start {} but got {}",
            byte_pos, expected_line_start, iter_pos
        );
    }
}

#[test]
fn test_piece_tree_line_count_after_insert() {
    // Debug the piece tree structure after insert
    let mut buffer = TextBuffer::from_bytes(b"a\nb\n".to_vec(), test_fs());

    // Insert at line 1, column 0
    buffer.insert_at_position(Position { line: 1, column: 0 }, b"x\n".to_vec());

    // Manually verify line counts
    let content = buffer.slice_bytes(0..buffer.len());
    let newline_count = content.iter().filter(|&&b| b == b'\n').count();
    let expected_line_count = newline_count + 1;
    let actual_line_count = buffer.line_count();

    assert_eq!(
        actual_line_count,
        Some(expected_line_count),
        "Line count mismatch after insert"
    );
}

#[test]
fn test_position_to_lsp_position_after_modification() {
    // This test demonstrates a bug in the piece tree's offset_to_position
    // where column calculation is incorrect after buffer modifications.
    // The position_to_lsp_position function works around this by using
    // line_start_offset to calculate the column correctly.

    // Initial content: "fn foo(val: i32) {\n    val + 1\n}\n"
    let initial = b"fn foo(val: i32) {\n    val + 1\n}\n";
    let mut buffer = TextBuffer::from_bytes(initial.to_vec(), test_fs());

    // Verify initial positions work correctly
    // Position 23 is 'v' of second "val" on line 1
    let (line, char) = buffer.position_to_lsp_position(23);
    assert_eq!(line, 1, "Initial: position 23 should be on line 1");
    assert_eq!(char, 4, "Initial: position 23 should be at char 4");

    // Simulate rename: delete "val" at position 23 (line 1, char 4) and insert "value"
    // Position 23 = line 1, char 4; Position 26 = line 1, char 7
    buffer.delete_range(
        Position { line: 1, column: 4 },
        Position { line: 1, column: 7 },
    );
    buffer.insert_bytes(23, b"value".to_vec()); // Insert "value"

    // Also rename the first occurrence
    // Position 7 = line 0, char 7; Position 10 = line 0, char 10
    buffer.delete_range(
        Position { line: 0, column: 7 },
        Position {
            line: 0,
            column: 10,
        },
    );
    buffer.insert_bytes(7, b"value".to_vec()); // Insert "value"

    // Buffer is now: "fn foo(value: i32) {\n    value + 1\n}\n"
    let content = String::from_utf8_lossy(&buffer.get_all_text().unwrap()).to_string();
    assert_eq!(content, "fn foo(value: i32) {\n    value + 1\n}\n");

    // Position 25 is now 'v' of second "value" on line 1
    // Line 0: "fn foo(value: i32) {\n" = 21 chars (positions 0-20)
    // Line 1: "    value + 1\n" starts at position 21
    // Position 25 = 21 + 4 = line 1, char 4

    // The workaround in position_to_lsp_position should give correct result
    let (line, char) = buffer.position_to_lsp_position(25);
    assert_eq!(
        line, 1,
        "After modification: position 25 should be on line 1"
    );
    assert_eq!(
        char, 4,
        "After modification: position 25 should be at char 4"
    );

    // Also verify position 21 (start of line 1) works
    let (line, char) = buffer.position_to_lsp_position(21);
    assert_eq!(line, 1, "Position 21 should be on line 1");
    assert_eq!(char, 0, "Position 21 should be at char 0 (start of line)");
}

#[test]
fn test_detect_crlf() {
    assert_eq!(
        super::format::detect_line_ending(b"hello\r\nworld\r\n"),
        LineEnding::CRLF
    );
}

#[test]
fn test_detect_lf() {
    assert_eq!(
        super::format::detect_line_ending(b"hello\nworld\n"),
        LineEnding::LF
    );
}

#[test]
fn test_normalize_crlf() {
    let input = b"hello\r\nworld\r\n".to_vec();
    let output = super::format::normalize_line_endings(input);
    assert_eq!(output, b"hello\nworld\n");
}

#[test]
fn test_normalize_empty() {
    let input = Vec::new();
    let output = super::format::normalize_line_endings(input);
    assert_eq!(output, Vec::<u8>::new());
}

/// Regression test: get_all_text() returns empty for large files with unloaded regions
///
/// This was the root cause of a bug where recovery auto-save would save 0 bytes
/// for large files, causing data loss on crash recovery.
///
/// The fix is to use get_text_range_mut() which handles lazy loading.
#[test]
fn test_get_all_text_returns_empty_for_unloaded_buffers() {
    use tempfile::TempDir;
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_test.txt");

    // Create a 50KB file
    let original_content = "X".repeat(50_000);
    std::fs::write(&file_path, &original_content).unwrap();

    // Load with small threshold to trigger large file mode
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024, test_fs()).unwrap();
    assert!(
        buffer.file_kind.is_large_file(),
        "Should be in large file mode"
    );
    assert!(!buffer.buffers[0].is_loaded(), "Buffer should be unloaded");

    // Make a small edit
    buffer.insert_bytes(0, b"EDITED: ".to_vec());

    // get_all_text() now returns None for unloaded buffers instead of empty
    // This is the correct behavior - it signals that content is not available
    let content_immutable = buffer.get_all_text();

    // get_all_text() returns None because it uses get_text_range() which
    // returns None for unloaded regions
    assert!(
        content_immutable.is_none(),
        "get_all_text() should return None for large files with unloaded regions. \
             Got Some({} bytes) instead of None.",
        content_immutable.as_ref().map(|c| c.len()).unwrap_or(0)
    );

    // CORRECT BEHAVIOR: get_text_range_mut() handles lazy loading
    let total = buffer.total_bytes();
    let content_lazy = buffer.get_text_range_mut(0, total).unwrap();
    assert_eq!(
        content_lazy.len(),
        50_000 + 8,
        "get_text_range_mut() should return all content with lazy loading"
    );
    assert!(
        String::from_utf8_lossy(&content_lazy).starts_with("EDITED: "),
        "Content should start with our edit"
    );
}

// ===== Line Ending Conversion Tests =====

mod line_ending_conversion {
    use super::*;

    #[test]
    fn test_convert_lf_to_crlf() {
        let input = b"Line 1\nLine 2\nLine 3\n";
        let result = super::format::convert_line_endings_to(input, LineEnding::CRLF);
        assert_eq!(result, b"Line 1\r\nLine 2\r\nLine 3\r\n");
    }

    #[test]
    fn test_convert_crlf_to_lf() {
        let input = b"Line 1\r\nLine 2\r\nLine 3\r\n";
        let result = super::format::convert_line_endings_to(input, LineEnding::LF);
        assert_eq!(result, b"Line 1\nLine 2\nLine 3\n");
    }

    #[test]
    fn test_convert_cr_to_lf() {
        let input = b"Line 1\rLine 2\rLine 3\r";
        let result = super::format::convert_line_endings_to(input, LineEnding::LF);
        assert_eq!(result, b"Line 1\nLine 2\nLine 3\n");
    }

    #[test]
    fn test_convert_mixed_to_crlf() {
        // Mixed line endings: LF, CRLF, CR
        let input = b"Line 1\nLine 2\r\nLine 3\r";
        let result = super::format::convert_line_endings_to(input, LineEnding::CRLF);
        assert_eq!(result, b"Line 1\r\nLine 2\r\nLine 3\r\n");
    }

    #[test]
    fn test_convert_lf_to_lf_is_noop() {
        let input = b"Line 1\nLine 2\nLine 3\n";
        let result = super::format::convert_line_endings_to(input, LineEnding::LF);
        assert_eq!(result, input.to_vec());
    }

    #[test]
    fn test_convert_empty_content() {
        let input = b"";
        let result = super::format::convert_line_endings_to(input, LineEnding::CRLF);
        assert_eq!(result, b"".to_vec());
    }

    #[test]
    fn test_convert_no_line_endings() {
        let input = b"No line endings here";
        let result = super::format::convert_line_endings_to(input, LineEnding::CRLF);
        assert_eq!(result, b"No line endings here".to_vec());
    }

    #[test]
    fn test_set_line_ending_marks_modified() {
        let mut buffer = TextBuffer::from_bytes(b"Hello\nWorld\n".to_vec(), test_fs());
        assert!(!buffer.is_modified());

        buffer.set_line_ending(LineEnding::CRLF);
        assert!(buffer.is_modified());
    }

    #[test]
    fn test_set_default_line_ending_does_not_mark_modified() {
        let mut buffer = TextBuffer::empty(test_fs());
        assert!(!buffer.is_modified());

        buffer.set_default_line_ending(LineEnding::CRLF);
        assert!(!buffer.is_modified());
        assert_eq!(buffer.line_ending(), LineEnding::CRLF);
    }

    #[test]
    fn test_save_to_file_converts_lf_to_crlf() {
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test_lf_to_crlf.txt");

        // Create a file with LF line endings
        let original_content = b"Line 1\nLine 2\nLine 3\n";
        std::fs::write(&file_path, original_content).unwrap();

        // Load the file
        let mut buffer =
            TextBuffer::load_from_file(&file_path, DEFAULT_LARGE_FILE_THRESHOLD, test_fs())
                .unwrap();
        assert_eq!(buffer.line_ending(), LineEnding::LF);

        // Change line ending to CRLF
        buffer.set_line_ending(LineEnding::CRLF);
        assert_eq!(buffer.line_ending(), LineEnding::CRLF);
        assert!(buffer.is_modified());

        // Save the file
        buffer.save_to_file(&file_path).unwrap();

        // Read back and verify CRLF
        let saved_bytes = std::fs::read(&file_path).unwrap();
        assert_eq!(&saved_bytes, b"Line 1\r\nLine 2\r\nLine 3\r\n");
    }

    #[test]
    fn test_save_to_file_converts_crlf_to_lf() {
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test_crlf_to_lf.txt");

        // Create a file with CRLF line endings
        let original_content = b"Line 1\r\nLine 2\r\nLine 3\r\n";
        std::fs::write(&file_path, original_content).unwrap();

        // Load the file
        let mut buffer =
            TextBuffer::load_from_file(&file_path, DEFAULT_LARGE_FILE_THRESHOLD, test_fs())
                .unwrap();
        assert_eq!(buffer.line_ending(), LineEnding::CRLF);

        // Change line ending to LF
        buffer.set_line_ending(LineEnding::LF);
        assert_eq!(buffer.line_ending(), LineEnding::LF);
        assert!(buffer.is_modified());

        // Save the file
        buffer.save_to_file(&file_path).unwrap();

        // Read back and verify LF (no CRLF)
        let saved_bytes = std::fs::read(&file_path).unwrap();
        assert_eq!(&saved_bytes, b"Line 1\nLine 2\nLine 3\n");
    }

    #[test]
    #[cfg(unix)]
    fn test_save_to_unwritable_file() -> anyhow::Result<()> {
        // Root (uid 0) bypasses Unix file permission checks, so these
        // permission-denied tests are meaningless when running as root.
        if unsafe { libc::getuid() } == 0 {
            eprintln!("Skipping test: root bypasses file permission checks");
            return Ok(());
        }
        use std::fs::Permissions;
        use std::os::unix::fs::PermissionsExt;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let unwritable_dir = temp_dir.path().join("unwritable_dir");
        std::fs::create_dir(&unwritable_dir)?;

        let file_path = unwritable_dir.join("unwritable.txt");
        std::fs::write(&file_path, "original content")?;

        // Make directory unwritable to prevent rename/temp file creation
        std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o555))?;

        let mut buffer = TextBuffer::from_bytes(b"new content".to_vec(), test_fs());
        let result = buffer.save_to_file(&file_path);

        // Verify that it returns SudoSaveRequired
        match result {
            Err(e) => {
                if let Some(sudo_err) = e.downcast_ref::<SudoSaveRequired>() {
                    assert_eq!(sudo_err.dest_path, file_path);
                    assert!(sudo_err.temp_path.exists());
                    // Cleanup temp file
                    drop(std::fs::remove_file(&sudo_err.temp_path));
                } else {
                    panic!("Expected SudoSaveRequired error, got: {:?}", e);
                }
            }
            Ok(_) => panic!("Expected error, but save succeeded"),
        }

        Ok(())
    }

    #[test]
    #[cfg(unix)]
    fn test_save_to_unwritable_directory() -> anyhow::Result<()> {
        // Root (uid 0) bypasses Unix file permission checks, so these
        // permission-denied tests are meaningless when running as root.
        if unsafe { libc::getuid() } == 0 {
            eprintln!("Skipping test: root bypasses file permission checks");
            return Ok(());
        }
        use std::fs::Permissions;
        use std::os::unix::fs::PermissionsExt;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let unwritable_dir = temp_dir.path().join("unwritable_dir");
        std::fs::create_dir(&unwritable_dir)?;

        let file_path = unwritable_dir.join("test.txt");

        // Make directory unwritable (no write allowed)
        std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o555))?;

        let mut buffer = TextBuffer::from_bytes(b"content".to_vec(), test_fs());
        let result = buffer.save_to_file(&file_path);

        match result {
            Err(e) => {
                if let Some(sudo_err) = e.downcast_ref::<SudoSaveRequired>() {
                    assert_eq!(sudo_err.dest_path, file_path);
                    assert!(sudo_err.temp_path.exists());
                    // It should be in /tmp because the directory was not writable
                    assert!(sudo_err.temp_path.starts_with(std::env::temp_dir()));
                    // Cleanup
                    drop(std::fs::remove_file(&sudo_err.temp_path));
                } else {
                    panic!("Expected SudoSaveRequired error, got: {:?}", e);
                }
            }
            Ok(_) => panic!("Expected error, but save succeeded"),
        }

        Ok(())
    }
}

mod large_file_encoding_tests {
    use super::*;

    #[test]
    fn test_large_file_encoding_confirmation_display() {
        let confirmation = LargeFileEncodingConfirmation {
            path: PathBuf::from("/test/file.txt"),
            file_size: 150 * 1024 * 1024, // 150 MB
            encoding: Encoding::ShiftJis,
        };

        let display = format!("{}", confirmation);
        assert!(display.contains("150 MB"), "Display: {}", display);
        assert!(display.contains("Shift-JIS"), "Display: {}", display);
        assert!(
            display.contains("requires full load"),
            "Display: {}",
            display
        );
    }

    #[test]
    fn test_large_file_encoding_confirmation_equality() {
        let a = LargeFileEncodingConfirmation {
            path: PathBuf::from("/test/file.txt"),
            file_size: 100 * 1024 * 1024,
            encoding: Encoding::Gb18030,
        };
        let b = LargeFileEncodingConfirmation {
            path: PathBuf::from("/test/file.txt"),
            file_size: 100 * 1024 * 1024,
            encoding: Encoding::Gb18030,
        };
        let c = LargeFileEncodingConfirmation {
            path: PathBuf::from("/test/other.txt"),
            file_size: 100 * 1024 * 1024,
            encoding: Encoding::Gb18030,
        };

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_encoding_requires_confirmation() {
        // Resynchronizable encodings should NOT require confirmation
        assert!(!Encoding::Utf8.requires_full_file_load());
        assert!(!Encoding::Utf8Bom.requires_full_file_load());
        assert!(!Encoding::Ascii.requires_full_file_load());
        assert!(!Encoding::Latin1.requires_full_file_load());
        assert!(!Encoding::Windows1252.requires_full_file_load());
        assert!(!Encoding::Windows1250.requires_full_file_load());
        assert!(!Encoding::Windows1251.requires_full_file_load());
        assert!(!Encoding::Utf16Le.requires_full_file_load());
        assert!(!Encoding::Utf16Be.requires_full_file_load());

        // Non-resynchronizable CJK encodings SHOULD require confirmation
        assert!(Encoding::Gb18030.requires_full_file_load());
        assert!(Encoding::Gbk.requires_full_file_load());
        assert!(Encoding::ShiftJis.requires_full_file_load());
        assert!(Encoding::EucKr.requires_full_file_load());
    }

    #[test]
    fn test_check_large_file_encoding_small_file() {
        use tempfile::NamedTempFile;

        // Create a small file (well under threshold)
        let temp = NamedTempFile::new().unwrap();
        std::fs::write(temp.path(), b"hello world").unwrap();

        let result = TextBuffer::check_large_file_encoding(temp.path(), test_fs()).unwrap();
        assert!(
            result.is_none(),
            "Small files should not require confirmation"
        );
    }

    #[test]
    fn test_large_file_encoding_error_downcast() {
        // Verify that LargeFileEncodingConfirmation can be used as an anyhow error
        let confirmation = LargeFileEncodingConfirmation {
            path: PathBuf::from("/test/file.txt"),
            file_size: 200 * 1024 * 1024,
            encoding: Encoding::EucKr,
        };

        let error: anyhow::Error = confirmation.clone().into();
        let downcast = error.downcast_ref::<LargeFileEncodingConfirmation>();
        assert!(downcast.is_some());
        assert_eq!(downcast.unwrap().encoding, Encoding::EucKr);
    }
}

mod rebuild_pristine_saved_root_tests {
    use super::*;
    use crate::model::piece_tree::BufferLocation;
    use std::sync::Arc;

    /// Create a large-file-mode TextBuffer from raw bytes, simulating what
    /// `load_from_file` does for files above the large-file threshold.
    fn large_file_buffer(content: &[u8]) -> TextBuffer {
        let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
            Arc::new(crate::model::filesystem::StdFileSystem);
        let bytes = content.len();
        let buffer = crate::model::piece_tree::StringBuffer::new_loaded(0, content.to_vec(), false);
        let piece_tree = if bytes > 0 {
            crate::model::piece_tree::PieceTree::new(BufferLocation::Stored(0), 0, bytes, None)
        } else {
            crate::model::piece_tree::PieceTree::empty()
        };
        let saved_root = piece_tree.root();
        TextBuffer {
            piece_tree,
            buffers: vec![buffer],
            next_buffer_id: 1,
            persistence: Persistence::new(fs, None, saved_root, Some(bytes)),
            file_kind: BufferFileKind::new(true, false),
            format: BufferFormat::new(LineEnding::LF, Encoding::Utf8),
            version: 0,
            config: BufferConfig::default(),
        }
    }

    /// Simulate prepare_line_scan + scanning: pre-split and compute lf counts.
    fn scan_line_feeds(buf: &mut TextBuffer) -> Vec<(usize, usize)> {
        buf.piece_tree.split_leaves_to_chunk_size(LOAD_CHUNK_SIZE);
        let leaves = buf.piece_tree.get_leaves();
        let mut updates = Vec::new();
        for (idx, leaf) in leaves.iter().enumerate() {
            if leaf.line_feed_cnt.is_some() {
                continue;
            }
            let count = buf.scan_leaf(leaf).unwrap();
            updates.push((idx, count));
        }
        updates
    }

    /// Generate a repeating pattern with newlines for testing.
    fn make_content(size: usize) -> Vec<u8> {
        let line = b"abcdefghij0123456789ABCDEFGHIJ0123456789abcdefghij0123456789ABCDEFGHIJ\n";
        let mut out = Vec::with_capacity(size);
        while out.len() < size {
            let remaining = size - out.len();
            let take = remaining.min(line.len());
            out.extend_from_slice(&line[..take]);
        }
        out
    }

    #[test]
    fn test_no_edits_arc_ptr_eq() {
        let content = make_content(2 * 1024 * 1024);
        let expected_lf = content.iter().filter(|&&b| b == b'\n').count();
        let mut buf = large_file_buffer(&content);

        // Before scan, line_count should be None (large file, no indexing).
        assert!(buf.line_count().is_none());

        let updates = scan_line_feeds(&mut buf);
        buf.rebuild_with_pristine_saved_root(&updates);

        // After rebuild, line_count must be Some (exact).
        assert_eq!(buf.line_count(), Some(expected_lf + 1));

        // After rebuild with no edits, roots should be identical (Arc::ptr_eq).
        assert!(Arc::ptr_eq(
            buf.persistence.saved_root(),
            &buf.piece_tree.root()
        ));
        let diff = buf.diff_since_saved();
        assert!(diff.equal);
        assert!(buf.file_kind.has_line_feed_scan());
        assert_eq!(buf.get_all_text().unwrap(), content);
    }

    #[test]
    fn test_single_insertion() {
        let content = make_content(2 * 1024 * 1024);
        let mut buf = large_file_buffer(&content);
        let updates = scan_line_feeds(&mut buf);

        // Insert some text in the middle.
        let insert_offset = 1_000_000;
        let insert_text = b"INSERTED_TEXT\n";
        buf.insert_bytes(insert_offset, insert_text.to_vec());

        buf.rebuild_with_pristine_saved_root(&updates);

        // Content should match the shadow model.
        let mut expected = content.clone();
        expected.splice(insert_offset..insert_offset, insert_text.iter().copied());
        assert_eq!(buf.get_all_text().unwrap(), expected);

        // line_count must be Some (exact) after rebuild, even with edits.
        let expected_lf = expected.iter().filter(|&&b| b == b'\n').count();
        assert_eq!(buf.line_count(), Some(expected_lf + 1));

        // Diff should NOT be equal.
        let diff = buf.diff_since_saved();
        assert!(!diff.equal);
        assert!(!diff.byte_ranges.is_empty());
    }

    /// After rebuild + insert near EOF, diff byte_ranges must be
    /// document-absolute.  The bug: `with_doc_offsets` assigned consecutive
    /// offsets from 0 to the collected leaves, missing skipped (shared)
    /// subtrees' bytes.
    #[test]
    fn test_diff_byte_ranges_are_document_absolute_after_eof_insert() {
        let content = make_content(4 * 1024 * 1024); // 4MB → 4 chunks at 1MB each
        let mut buf = large_file_buffer(&content);
        let updates = scan_line_feeds(&mut buf);
        buf.rebuild_with_pristine_saved_root(&updates);

        // Insert 5 bytes near EOF (last 100 bytes of the file).
        let insert_offset = content.len() - 100;
        buf.insert_bytes(insert_offset, b"HELLO".to_vec());

        let diff = buf.diff_since_saved();
        assert!(!diff.equal, "diff should detect the insertion");
        assert!(
            !diff.byte_ranges.is_empty(),
            "byte_ranges should not be empty"
        );

        // byte_ranges must be near the end of the document, not near 0.
        let first_range = &diff.byte_ranges[0];
        assert!(
            first_range.start >= content.len() - 200,
            "byte_ranges should be document-absolute (near EOF): got {:?}, expected near {}",
            first_range,
            insert_offset,
        );
    }

    #[test]
    fn test_single_deletion() {
        let content = make_content(2 * 1024 * 1024);
        let mut buf = large_file_buffer(&content);
        let updates = scan_line_feeds(&mut buf);

        // Delete a range.
        let del_start = 500_000;
        let del_len = 1000;
        buf.delete_bytes(del_start, del_len);

        buf.rebuild_with_pristine_saved_root(&updates);

        let mut expected = content.clone();
        expected.drain(del_start..del_start + del_len);
        assert_eq!(buf.get_all_text().unwrap(), expected);

        let diff = buf.diff_since_saved();
        assert!(!diff.equal);
    }

    #[test]
    fn test_insert_and_delete() {
        let content = make_content(2 * 1024 * 1024);
        let mut buf = large_file_buffer(&content);
        let updates = scan_line_feeds(&mut buf);

        // Delete near the start, insert near the end.
        let del_start = 100_000;
        let del_len = 500;
        buf.delete_bytes(del_start, del_len);

        let insert_offset = 1_500_000; // in the post-delete document
        let insert_text = b"NEW_CONTENT\n";
        buf.insert_bytes(insert_offset, insert_text.to_vec());

        buf.rebuild_with_pristine_saved_root(&updates);

        // Build expected content.
        let mut expected = content.clone();
        expected.drain(del_start..del_start + del_len);
        expected.splice(insert_offset..insert_offset, insert_text.iter().copied());
        assert_eq!(buf.get_all_text().unwrap(), expected);

        let diff = buf.diff_since_saved();
        assert!(!diff.equal);
    }

    #[test]
    fn test_multiple_scattered_edits() {
        let content = make_content(3 * 1024 * 1024);
        let mut buf = large_file_buffer(&content);
        let updates = scan_line_feeds(&mut buf);
        let mut expected = content.clone();

        // Apply several edits across chunk boundaries, tracking the shadow model.
        // Edit 1: delete at offset 100k
        buf.delete_bytes(100_000, 200);
        expected.drain(100_000..100_200);

        // Edit 2: insert at offset 500k (in current doc, which shifted)
        buf.insert_bytes(500_000, b"AAAA\n".to_vec());
        expected.splice(500_000..500_000, b"AAAA\n".iter().copied());

        // Edit 3: delete at offset 2M
        buf.delete_bytes(2_000_000, 300);
        expected.drain(2_000_000..2_000_300);

        // Edit 4: insert at offset 1M
        buf.insert_bytes(1_000_000, b"BBBB\n".to_vec());
        expected.splice(1_000_000..1_000_000, b"BBBB\n".iter().copied());

        buf.rebuild_with_pristine_saved_root(&updates);

        assert_eq!(buf.get_all_text().unwrap(), expected);
        let diff = buf.diff_since_saved();
        assert!(!diff.equal);
    }

    #[test]
    fn test_content_preserved_after_rebuild() {
        // Verify that get_all_text matches before and after rebuild for
        // a buffer with edits.
        let content = make_content(2 * 1024 * 1024);
        let mut buf = large_file_buffer(&content);
        let updates = scan_line_feeds(&mut buf);

        buf.insert_bytes(0, b"HEADER\n".to_vec());
        buf.delete_bytes(1_000_000, 500);

        let text_before = buf.get_all_text().unwrap();
        buf.rebuild_with_pristine_saved_root(&updates);
        let text_after = buf.get_all_text().unwrap();

        assert_eq!(text_before, text_after);
    }

    /// Create a large-file-mode TextBuffer backed by an actual file on disk
    /// (Unloaded buffer), matching the real `load_from_file` code path.
    fn large_file_buffer_unloaded(path: &std::path::Path, file_size: usize) -> TextBuffer {
        let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
            Arc::new(crate::model::filesystem::StdFileSystem);
        let buffer = crate::model::piece_tree::StringBuffer::new_unloaded(
            0,
            path.to_path_buf(),
            0,
            file_size,
        );
        let piece_tree = if file_size > 0 {
            crate::model::piece_tree::PieceTree::new(BufferLocation::Stored(0), 0, file_size, None)
        } else {
            crate::model::piece_tree::PieceTree::empty()
        };
        let saved_root = piece_tree.root();
        TextBuffer {
            piece_tree,
            buffers: vec![buffer],
            next_buffer_id: 1,
            persistence: Persistence::new(
                fs,
                Some(path.to_path_buf()),
                saved_root,
                Some(file_size),
            ),
            file_kind: BufferFileKind::new(true, false),
            format: BufferFormat::new(LineEnding::LF, Encoding::Utf8),
            version: 0,
            config: BufferConfig::default(),
        }
    }

    #[test]
    fn test_unloaded_buffer_no_edits_line_count() {
        let content = make_content(2 * 1024 * 1024);
        let expected_lf = content.iter().filter(|&&b| b == b'\n').count();

        let tmp = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(tmp.path(), &content).unwrap();
        let mut buf = large_file_buffer_unloaded(tmp.path(), content.len());

        assert!(
            buf.line_count().is_none(),
            "before scan, line_count should be None"
        );

        let updates = scan_line_feeds(&mut buf);
        buf.rebuild_with_pristine_saved_root(&updates);

        assert_eq!(
            buf.line_count(),
            Some(expected_lf + 1),
            "after rebuild, line_count must be exact"
        );
        assert!(buf.file_kind.has_line_feed_scan());
    }

    #[test]
    fn test_unloaded_buffer_with_edits_line_count() {
        let content = make_content(2 * 1024 * 1024);

        let tmp = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(tmp.path(), &content).unwrap();
        let mut buf = large_file_buffer_unloaded(tmp.path(), content.len());

        let updates = scan_line_feeds(&mut buf);

        // Insert text in the middle (creates an Added piece).
        let insert_text = b"INSERTED\n";
        buf.insert_bytes(1_000_000, insert_text.to_vec());

        buf.rebuild_with_pristine_saved_root(&updates);

        let mut expected = content.clone();
        expected.splice(1_000_000..1_000_000, insert_text.iter().copied());
        let expected_lf = expected.iter().filter(|&&b| b == b'\n').count();

        assert_eq!(
            buf.line_count(),
            Some(expected_lf + 1),
            "after rebuild with edits, line_count must be exact"
        );
        assert!(buf.file_kind.has_line_feed_scan());
    }

    /// After rebuild, diff_since_saved should visit a small number of nodes
    /// proportional to edit regions, NOT the full tree. This catches
    /// regressions where Arc pointers are accidentally destroyed (e.g. by
    /// flattening and rebuilding the tree).
    #[test]
    fn test_diff_efficiency_after_rebuild() {
        // Use 32MB so the tree has ~32 leaves (at 1MB chunk size),
        // making the efficiency difference between O(log N) and O(N) clear.
        let content = make_content(32 * 1024 * 1024);
        let mut buf = large_file_buffer(&content);

        let updates = scan_line_feeds(&mut buf);

        // Insert a small piece of text in one chunk.
        buf.insert_bytes(1_000_000, b"HELLO".to_vec());

        buf.rebuild_with_pristine_saved_root(&updates);

        let diff = buf.diff_since_saved();
        assert!(!diff.equal);

        let total_leaves = buf.piece_tree.get_leaves().len();
        // The diff should visit far fewer nodes than the total tree.
        // With path-copying, only the path from root to the edited leaf
        // (and its immediate neighbours) should be visited — roughly
        // O(log N) nodes, not O(N).
        assert!(
            diff.nodes_visited < total_leaves,
            "diff visited {} nodes but tree has {} leaves — \
                 Arc::ptr_eq short-circuiting is not working",
            diff.nodes_visited,
            total_leaves,
        );
    }

    /// After rebuild_with_pristine_saved_root, loading a small viewport
    /// range must NOT cause the entire original file buffer to be loaded.
    /// This is a regression test for a bug where the pristine tree's 1MB
    /// pieces all referenced Stored(0) (the whole-file buffer). Because
    /// piece_view.bytes (1MB) <= LOAD_CHUNK_SIZE, get_text_range_mut took
    /// the "load_small_buffer" path, calling load() on the 814MB buffer.
    #[test]
    fn test_viewport_load_after_rebuild_does_not_load_entire_file() {
        use std::sync::atomic::{AtomicUsize, Ordering};

        /// Filesystem wrapper that tracks the largest read_range call.
        struct TrackingFs {
            inner: crate::model::filesystem::StdFileSystem,
            max_read_range_len: Arc<AtomicUsize>,
        }

        impl crate::model::filesystem::FileSystem for TrackingFs {
            fn read_file(&self, path: &Path) -> std::io::Result<Vec<u8>> {
                self.inner.read_file(path)
            }
            fn read_range(&self, path: &Path, offset: u64, len: usize) -> std::io::Result<Vec<u8>> {
                self.max_read_range_len.fetch_max(len, Ordering::SeqCst);
                self.inner.read_range(path, offset, len)
            }
            fn write_file(&self, path: &Path, data: &[u8]) -> std::io::Result<()> {
                self.inner.write_file(path, data)
            }
            fn create_file(
                &self,
                path: &Path,
            ) -> std::io::Result<Box<dyn crate::model::filesystem::FileWriter>> {
                self.inner.create_file(path)
            }
            fn open_file(
                &self,
                path: &Path,
            ) -> std::io::Result<Box<dyn crate::model::filesystem::FileReader>> {
                self.inner.open_file(path)
            }
            fn open_file_for_write(
                &self,
                path: &Path,
            ) -> std::io::Result<Box<dyn crate::model::filesystem::FileWriter>> {
                self.inner.open_file_for_write(path)
            }
            fn open_file_for_append(
                &self,
                path: &Path,
            ) -> std::io::Result<Box<dyn crate::model::filesystem::FileWriter>> {
                self.inner.open_file_for_append(path)
            }
            fn set_file_length(&self, path: &Path, len: u64) -> std::io::Result<()> {
                self.inner.set_file_length(path, len)
            }
            fn rename(&self, from: &Path, to: &Path) -> std::io::Result<()> {
                self.inner.rename(from, to)
            }
            fn copy(&self, from: &Path, to: &Path) -> std::io::Result<u64> {
                self.inner.copy(from, to)
            }
            fn remove_file(&self, path: &Path) -> std::io::Result<()> {
                self.inner.remove_file(path)
            }
            fn remove_dir(&self, path: &Path) -> std::io::Result<()> {
                self.inner.remove_dir(path)
            }
            fn metadata(
                &self,
                path: &Path,
            ) -> std::io::Result<crate::model::filesystem::FileMetadata> {
                self.inner.metadata(path)
            }
            fn symlink_metadata(
                &self,
                path: &Path,
            ) -> std::io::Result<crate::model::filesystem::FileMetadata> {
                self.inner.symlink_metadata(path)
            }
            fn is_dir(&self, path: &Path) -> std::io::Result<bool> {
                self.inner.is_dir(path)
            }
            fn is_file(&self, path: &Path) -> std::io::Result<bool> {
                self.inner.is_file(path)
            }
            fn set_permissions(
                &self,
                path: &Path,
                permissions: &crate::model::filesystem::FilePermissions,
            ) -> std::io::Result<()> {
                self.inner.set_permissions(path, permissions)
            }
            fn is_owner(&self, path: &Path) -> bool {
                self.inner.is_owner(path)
            }
            fn read_dir(
                &self,
                path: &Path,
            ) -> std::io::Result<Vec<crate::model::filesystem::DirEntry>> {
                self.inner.read_dir(path)
            }
            fn create_dir(&self, path: &Path) -> std::io::Result<()> {
                self.inner.create_dir(path)
            }
            fn create_dir_all(&self, path: &Path) -> std::io::Result<()> {
                self.inner.create_dir_all(path)
            }
            fn canonicalize(&self, path: &Path) -> std::io::Result<PathBuf> {
                self.inner.canonicalize(path)
            }
            fn current_uid(&self) -> u32 {
                self.inner.current_uid()
            }
            fn sudo_write(
                &self,
                path: &Path,
                data: &[u8],
                mode: u32,
                uid: u32,
                gid: u32,
            ) -> std::io::Result<()> {
                self.inner.sudo_write(path, data, mode, uid, gid)
            }
            fn search_file(
                &self,
                path: &Path,
                pattern: &str,
                opts: &crate::model::filesystem::FileSearchOptions,
                cursor: &mut crate::model::filesystem::FileSearchCursor,
            ) -> std::io::Result<Vec<SearchMatch>> {
                crate::model::filesystem::default_search_file(
                    &self.inner,
                    path,
                    pattern,
                    opts,
                    cursor,
                )
            }
            fn walk_files(
                &self,
                root: &Path,
                skip_dirs: &[&str],
                cancel: &std::sync::atomic::AtomicBool,
                on_file: &mut dyn FnMut(&Path, &str) -> bool,
            ) -> std::io::Result<()> {
                self.inner.walk_files(root, skip_dirs, cancel, on_file)
            }
        }

        // Create a 3MB file with newlines (3 chunks at LOAD_CHUNK_SIZE=1MB).
        let file_size = LOAD_CHUNK_SIZE * 3;
        let content = make_content(file_size);

        let tmp = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(tmp.path(), &content).unwrap();

        let max_read = Arc::new(AtomicUsize::new(0));
        let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
            Arc::new(TrackingFs {
                inner: crate::model::filesystem::StdFileSystem,
                max_read_range_len: max_read.clone(),
            });

        // Build an unloaded large-file buffer with the tracking FS.
        let buffer = crate::model::piece_tree::StringBuffer::new_unloaded(
            0,
            tmp.path().to_path_buf(),
            0,
            file_size,
        );
        let piece_tree = PieceTree::new(BufferLocation::Stored(0), 0, file_size, None);
        let saved_root = piece_tree.root();
        let mut buf = TextBuffer {
            piece_tree,
            buffers: vec![buffer],
            next_buffer_id: 1,
            persistence: Persistence::new(
                fs,
                Some(tmp.path().to_path_buf()),
                saved_root,
                Some(file_size),
            ),
            file_kind: BufferFileKind::new(true, false),
            format: BufferFormat::new(LineEnding::LF, Encoding::Utf8),
            version: 0,
            config: BufferConfig::default(),
        };

        // Load a small viewport in the middle (forces chunk splitting).
        let viewport_offset = LOAD_CHUNK_SIZE + 100; // somewhere in chunk 2
        buf.get_text_range_mut(viewport_offset, 4096).unwrap();

        // Run the line-feed scan and rebuild the pristine tree.
        let updates = scan_line_feeds(&mut buf);
        buf.rebuild_with_pristine_saved_root(&updates);

        // Reset the tracker — we only care about reads AFTER the rebuild.
        max_read.store(0, Ordering::SeqCst);

        // Load the same viewport range again.
        buf.get_text_range_mut(viewport_offset, 4096).unwrap();

        let largest_read = max_read.load(Ordering::SeqCst);
        assert!(
            largest_read <= LOAD_CHUNK_SIZE,
            "After rebuild, loading a viewport triggered a read of {} bytes \
                 (file_size={}). This means the entire Stored buffer is being \
                 loaded instead of just the needed chunk.",
            largest_read,
            file_size,
        );
    }

    /// After rebuild_with_pristine_saved_root, loading a viewport must not
    /// destroy the line feed counts on pieces. The chunk-split path in
    /// get_text_range_mut calls split_at_offset, which invokes
    /// compute_line_feeds_static — returning None for unloaded buffers.
    /// This turns exact line numbers back into byte-based estimates.
    #[test]
    fn test_viewport_load_after_rebuild_preserves_line_counts() {
        let file_size = LOAD_CHUNK_SIZE * 3;
        let content = make_content(file_size);

        let tmp = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(tmp.path(), &content).unwrap();
        let mut buf = large_file_buffer_unloaded(tmp.path(), content.len());

        // Scan + rebuild so every leaf has a known line_feed_cnt.
        let updates = scan_line_feeds(&mut buf);
        buf.rebuild_with_pristine_saved_root(&updates);

        let line_count_before = buf.piece_tree.line_count();
        assert!(
            line_count_before.is_some(),
            "line_count must be Some after rebuild"
        );

        // Load a viewport that starts in the MIDDLE of a piece, forcing
        // split_at_offset (not just replace_buffer_reference).
        let mid_piece_offset = LOAD_CHUNK_SIZE + LOAD_CHUNK_SIZE / 2;
        buf.get_text_range_mut(mid_piece_offset, 4096).unwrap();

        let line_count_after = buf.piece_tree.line_count();
        assert!(
            line_count_after.is_some(),
            "line_count must still be Some after viewport load \
                 (was {:?} before, now {:?})",
            line_count_before,
            line_count_after,
        );
        assert_eq!(
            line_count_before, line_count_after,
            "line_count must not change after viewport load"
        );
    }

    /// Same test but with Unloaded data (the fixup path).
    #[test]
    fn test_diff_efficiency_after_rebuild_unloaded() {
        let content = make_content(32 * 1024 * 1024);

        let tmp = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(tmp.path(), &content).unwrap();
        let mut buf = large_file_buffer_unloaded(tmp.path(), content.len());

        let updates = scan_line_feeds(&mut buf);

        buf.insert_bytes(1_000_000, b"HELLO".to_vec());

        buf.rebuild_with_pristine_saved_root(&updates);

        let diff = buf.diff_since_saved();
        assert!(!diff.equal);

        let total_leaves = buf.piece_tree.get_leaves().len();
        assert!(
            diff.nodes_visited < total_leaves,
            "diff visited {} nodes but tree has {} leaves — \
                 Arc::ptr_eq short-circuiting is not working (unloaded path)",
            diff.nodes_visited,
            total_leaves,
        );
    }
}

mod chunked_search {
    use super::*;

    fn make_buffer(content: &[u8]) -> TextBuffer {
        TextBuffer::from_bytes(content.to_vec(), test_fs())
    }

    fn make_regex(pattern: &str) -> regex::bytes::Regex {
        regex::bytes::Regex::new(pattern).unwrap()
    }

    #[test]
    fn single_chunk_line_col_context() {
        let mut buf = make_buffer(b"hello world\nfoo bar\nbaz quux\n");
        let state = buf.search_scan_all(make_regex("bar"), 100, 3).unwrap();
        assert_eq!(state.matches.len(), 1);
        let m = &state.matches[0];
        assert_eq!(m.line, 2);
        assert_eq!(m.column, 5); // "foo bar" → 'b' at column 5
        assert_eq!(m.context, "foo bar");
        assert_eq!(m.byte_offset, 16); // "hello world\nfoo " = 16 bytes
        assert_eq!(m.length, 3);
    }

    #[test]
    fn multiple_matches_correct_lines() {
        let mut buf = make_buffer(b"aaa\nbbb\nccc\naaa\n");
        let state = buf.search_scan_all(make_regex("aaa"), 100, 3).unwrap();
        assert_eq!(state.matches.len(), 2);
        assert_eq!(state.matches[0].line, 1);
        assert_eq!(state.matches[0].context, "aaa");
        assert_eq!(state.matches[1].line, 4);
        assert_eq!(state.matches[1].context, "aaa");
    }

    #[test]
    fn match_on_last_line_no_trailing_newline() {
        let mut buf = make_buffer(b"line1\nline2\ntarget");
        let state = buf.search_scan_all(make_regex("target"), 100, 6).unwrap();
        assert_eq!(state.matches.len(), 1);
        let m = &state.matches[0];
        assert_eq!(m.line, 3);
        assert_eq!(m.column, 1);
        assert_eq!(m.context, "target");
    }

    #[test]
    fn match_at_first_byte() {
        let mut buf = make_buffer(b"target\nother\n");
        let state = buf.search_scan_all(make_regex("target"), 100, 6).unwrap();
        assert_eq!(state.matches.len(), 1);
        let m = &state.matches[0];
        assert_eq!(m.line, 1);
        assert_eq!(m.column, 1);
        assert_eq!(m.byte_offset, 0);
    }

    #[test]
    fn max_matches_caps() {
        let mut buf = make_buffer(b"a\na\na\na\na\n");
        let state = buf.search_scan_all(make_regex("a"), 3, 1).unwrap();
        assert_eq!(state.matches.len(), 3);
        assert!(state.capped);
    }

    #[test]
    fn case_insensitive_regex() {
        let mut buf = make_buffer(b"Hello\nhello\nHELLO\n");
        let state = buf
            .search_scan_all(make_regex("(?i)hello"), 100, 5)
            .unwrap();
        assert_eq!(state.matches.len(), 3);
        assert_eq!(state.matches[0].line, 1);
        assert_eq!(state.matches[1].line, 2);
        assert_eq!(state.matches[2].line, 3);
    }

    #[test]
    fn whole_word_boundary() {
        let mut buf = make_buffer(b"foobar\nfoo bar\nfoo\n");
        let state = buf.search_scan_all(make_regex(r"\bfoo\b"), 100, 3).unwrap();
        assert_eq!(state.matches.len(), 2);
        assert_eq!(state.matches[0].line, 2);
        assert_eq!(state.matches[0].column, 1);
        assert_eq!(state.matches[1].line, 3);
    }

    /// Force multi-chunk processing by creating a large file buffer
    /// with small piece-tree leaves, then verify line numbers are
    /// correct across chunk boundaries.
    #[test]
    fn multi_chunk_line_numbers_correct() {
        // Build content: 100 lines of "line_NNN\n"
        let mut content = Vec::new();
        for i in 1..=100 {
            content.extend_from_slice(format!("line_{:03}\n", i).as_bytes());
        }

        // Load as a "large file" with tiny threshold to force multiple
        // piece-tree leaves (chunks).
        let temp_dir = tempfile::TempDir::new().unwrap();
        let path = temp_dir.path().join("test.txt");
        std::fs::write(&path, &content).unwrap();
        let mut buffer = TextBuffer::load_from_file(&path, 10, test_fs()).unwrap();

        let state = buffer
            .search_scan_all(make_regex("line_050"), 100, 8)
            .unwrap();
        assert_eq!(state.matches.len(), 1);
        let m = &state.matches[0];
        assert_eq!(m.line, 50);
        assert_eq!(m.column, 1);
        assert_eq!(m.context, "line_050");
    }

    /// Verify that matches near chunk boundaries don't produce
    /// duplicate results (overlap deduplication).
    #[test]
    fn multi_chunk_no_duplicate_matches() {
        let mut content = Vec::new();
        for i in 1..=100 {
            content.extend_from_slice(format!("word_{:03}\n", i).as_bytes());
        }

        let temp_dir = tempfile::TempDir::new().unwrap();
        let path = temp_dir.path().join("test.txt");
        std::fs::write(&path, &content).unwrap();
        let mut buffer = TextBuffer::load_from_file(&path, 10, test_fs()).unwrap();

        // Search for a pattern that appears exactly once per line
        let state = buffer.search_scan_all(make_regex("word_"), 200, 5).unwrap();
        assert_eq!(
            state.matches.len(),
            100,
            "Should find exactly 100 matches (one per line), no duplicates"
        );

        // Verify line numbers are sequential 1..=100
        for (i, m) in state.matches.iter().enumerate() {
            assert_eq!(
                m.line,
                i + 1,
                "Match {} should be on line {}, got {}",
                i,
                i + 1,
                m.line
            );
        }
    }

    /// The reviewer's counter-example: verify line counting when
    /// overlap contains part of a line that continues into the
    /// next chunk.
    #[test]
    fn overlap_mid_line_line_numbers() {
        // Create content where a line spans a chunk boundary.
        // Use a large-file load with tiny threshold to force chunking.
        let mut content = Vec::new();
        content.extend_from_slice(b"short\n");
        // A long line that will span chunk boundaries
        content.extend_from_slice(b"AAAA_");
        for _ in 0..50 {
            content.extend_from_slice(b"BBBBBBBBBB"); // 500 bytes of B
        }
        content.extend_from_slice(b"_TARGET_HERE\n");
        content.extend_from_slice(b"after\n");

        let temp_dir = tempfile::TempDir::new().unwrap();
        let path = temp_dir.path().join("test.txt");
        std::fs::write(&path, &content).unwrap();
        let mut buffer = TextBuffer::load_from_file(&path, 10, test_fs()).unwrap();

        let state = buffer
            .search_scan_all(make_regex("TARGET_HERE"), 100, 11)
            .unwrap();
        assert_eq!(state.matches.len(), 1);
        let m = &state.matches[0];
        assert_eq!(m.line, 2, "TARGET_HERE is on line 2 (the long line)");
        assert_eq!(m.length, 11);

        // Also check "after" is on line 3
        let state2 = buffer.search_scan_all(make_regex("after"), 100, 5).unwrap();
        assert_eq!(state2.matches.len(), 1);
        assert_eq!(state2.matches[0].line, 3);
    }

    /// Verify correct results when a match spans the overlap/chunk
    /// boundary (starts in overlap tail, ends in new chunk).
    #[test]
    fn match_spanning_chunk_boundary() {
        // Create content where "SPLIT" can appear at the boundary
        let mut content = Vec::new();
        content.extend_from_slice(b"line1\n");
        // Pad to push "SPLIT" near a chunk boundary
        for _ in 0..60 {
            content.extend_from_slice(b"XXXXXXXXXX"); // 600 bytes
        }
        content.extend_from_slice(b"SPLIT\n");
        content.extend_from_slice(b"end\n");

        let temp_dir = tempfile::TempDir::new().unwrap();
        let path = temp_dir.path().join("test.txt");
        std::fs::write(&path, &content).unwrap();
        let mut buffer = TextBuffer::load_from_file(&path, 10, test_fs()).unwrap();

        let state = buffer.search_scan_all(make_regex("SPLIT"), 100, 5).unwrap();
        assert_eq!(state.matches.len(), 1, "SPLIT should be found exactly once");
        assert_eq!(state.matches[0].line, 2); // Still on line 2 (the long X line)
    }

    #[test]
    fn empty_buffer_no_matches() {
        let mut buf = make_buffer(b"");
        let state = buf.search_scan_all(make_regex("anything"), 100, 8).unwrap();
        assert!(state.matches.is_empty());
        assert!(!state.capped);
    }

    #[test]
    fn single_line_no_newline() {
        let mut buf = make_buffer(b"hello world");
        let state = buf.search_scan_all(make_regex("world"), 100, 5).unwrap();
        assert_eq!(state.matches.len(), 1);
        let m = &state.matches[0];
        assert_eq!(m.line, 1);
        assert_eq!(m.column, 7);
        assert_eq!(m.context, "hello world");
    }

    /// Verify that multiple matches on the same line get the same
    /// line number and correct columns.
    #[test]
    fn multiple_matches_same_line() {
        let mut buf = make_buffer(b"aa bb aa cc aa\nother\n");
        let state = buf.search_scan_all(make_regex("aa"), 100, 2).unwrap();
        assert_eq!(state.matches.len(), 3);
        for m in &state.matches {
            assert_eq!(m.line, 1);
            assert_eq!(m.context, "aa bb aa cc aa");
        }
        assert_eq!(state.matches[0].column, 1);
        assert_eq!(state.matches[1].column, 7);
        assert_eq!(state.matches[2].column, 13);
    }
}

mod hybrid_search {
    use super::*;

    fn make_regex(pattern: &str) -> regex::bytes::Regex {
        regex::bytes::Regex::new(pattern).unwrap()
    }

    fn make_opts() -> crate::model::filesystem::FileSearchOptions {
        crate::model::filesystem::FileSearchOptions {
            fixed_string: false,
            case_sensitive: true,
            whole_word: false,
            max_matches: 100,
        }
    }

    /// Hybrid search on a fully-loaded small buffer should produce
    /// the same results as search_scan_all.
    #[test]
    fn hybrid_matches_scan_all_for_loaded_buffer() {
        let content = b"foo bar baz\nfoo again\nlast line\n";
        let mut buf = TextBuffer::from_bytes(content.to_vec(), test_fs());
        let regex = make_regex("foo");
        let opts = make_opts();

        let hybrid = buf
            .search_hybrid("foo", &opts, regex.clone(), 100, 3)
            .unwrap();
        let scan = buf.search_scan_all(regex, 100, 3).unwrap();

        assert_eq!(hybrid.len(), scan.matches.len());
        for (h, s) in hybrid.iter().zip(scan.matches.iter()) {
            assert_eq!(h.byte_offset, s.byte_offset);
            assert_eq!(h.line, s.line);
            assert_eq!(h.column, s.column);
            assert_eq!(h.length, s.length);
            assert_eq!(h.context, s.context);
        }
    }

    /// Hybrid search on a file-backed buffer (large file with unloaded
    /// regions) should find matches using fs.search_file.
    #[test]
    fn hybrid_finds_matches_in_unloaded_regions() {
        let temp_dir = tempfile::TempDir::new().unwrap();
        let path = temp_dir.path().join("big.txt");

        // Create a file with known content
        let mut content = Vec::new();
        for i in 0..100 {
            content.extend_from_slice(format!("line {:03}\n", i).as_bytes());
        }
        std::fs::write(&path, &content).unwrap();

        // Load as a large file (unloaded mode)
        let mut buf = TextBuffer::load_from_file(&path, 10, test_fs()).unwrap();

        // Verify some leaves are unloaded
        let leaves = buf.piece_tree.get_leaves();
        let has_unloaded = leaves.iter().any(|l| {
            matches!(l.location, BufferLocation::Stored(_))
                && buf
                    .buffers
                    .get(l.location.buffer_id())
                    .map(|b| !b.is_loaded())
                    .unwrap_or(false)
        });

        let regex = make_regex("line 050");
        let opts = make_opts();
        let matches = buf.search_hybrid("line 050", &opts, regex, 100, 8).unwrap();

        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].line, 51); // 1-based
        assert!(matches[0].context.contains("line 050"));
        // If the buffer had unloaded regions, hybrid search used fs.search_file
        if has_unloaded {
            // Just verify it worked — the match was found without loading everything
        }
    }

    /// Hybrid search on a dirty buffer should find matches in both
    /// edited (loaded) and unedited (unloaded) regions.
    #[test]
    fn hybrid_dirty_buffer_finds_all_matches() {
        let temp_dir = tempfile::TempDir::new().unwrap();
        let path = temp_dir.path().join("dirty.txt");

        let mut content = Vec::new();
        for i in 0..50 {
            content.extend_from_slice(format!("target {:02}\n", i).as_bytes());
        }
        std::fs::write(&path, &content).unwrap();

        let mut buf = TextBuffer::load_from_file(&path, 10, test_fs()).unwrap();

        // Make a small edit near the beginning — insert "target XX" at position 0
        buf.insert(0, "target XX\n");

        let regex = make_regex("target");
        let opts = make_opts();
        let matches = buf.search_hybrid("target", &opts, regex, 200, 6).unwrap();

        // Should find the inserted "target XX" plus all 50 original "target NN"
        assert_eq!(matches.len(), 51);
        // First match should be the inserted one
        assert!(matches[0].context.contains("target XX"));
    }

    /// Boundary match: pattern spans loaded→unloaded boundary.
    #[test]
    fn hybrid_boundary_match() {
        let temp_dir = tempfile::TempDir::new().unwrap();
        let path = temp_dir.path().join("boundary.txt");

        // File content: "AAAAABBBBB" (no newlines)
        let content = b"AAAAABBBBB";
        std::fs::write(&path, content).unwrap();

        let mut buf = TextBuffer::from_bytes(content.to_vec(), test_fs());
        buf.rename_file_path(path);

        let regex = make_regex("AAAAABBBBB");
        let opts = make_opts();
        let matches = buf
            .search_hybrid("AAAAABBBBB", &opts, regex, 100, 10)
            .unwrap();

        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].byte_offset, 0);
    }

    /// Max matches limit is respected.
    #[test]
    fn hybrid_max_matches_respected() {
        let content = b"aaa\naaa\naaa\naaa\naaa\n";
        let mut buf = TextBuffer::from_bytes(content.to_vec(), test_fs());
        let regex = make_regex("aaa");
        let opts = crate::model::filesystem::FileSearchOptions {
            max_matches: 3,
            ..make_opts()
        };
        let matches = buf.search_hybrid("aaa", &opts, regex, 3, 3).unwrap();
        assert!(matches.len() <= 3);
    }
}

mod boundary_overlap {
    use super::*;

    fn make_regex(pattern: &str) -> regex::bytes::Regex {
        regex::bytes::Regex::new(pattern).unwrap()
    }

    #[test]
    fn empty_prev_tail_returns_nothing() {
        let matches = search_boundary_overlap(b"", b"hello", 0, 1, &make_regex("hello"), 100);
        assert!(matches.is_empty());
    }

    #[test]
    fn pure_tail_match_skipped() {
        // "foo" is entirely in prev_tail — should NOT be returned
        let matches = search_boundary_overlap(b"foo bar", b" baz", 0, 1, &make_regex("foo"), 100);
        assert!(matches.is_empty());
    }

    #[test]
    fn cross_boundary_match_found() {
        // "SPLIT" spans: prev_tail="...SPL", next_head="IT..."
        let matches = search_boundary_overlap(b"xxSPL", b"ITyy", 0, 1, &make_regex("SPLIT"), 100);
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].byte_offset, 2);
        assert_eq!(matches[0].length, 5);
    }

    #[test]
    fn pure_head_match_skipped() {
        // "baz" is entirely in next_head — should NOT be returned
        // (it starts at offset 4 which is >= overlap_len 3)
        let matches = search_boundary_overlap(b"foo", b" baz", 0, 1, &make_regex("baz"), 100);
        assert!(matches.is_empty());
    }

    #[test]
    fn line_number_tracking() {
        // prev_tail has a newline; running_line=5 means "line 5 at
        // the boundary".  The newline in the tail means SPLIT starts
        // on line 5 (the boundary line).
        let matches =
            search_boundary_overlap(b"line1\nSPL", b"IT end", 0, 5, &make_regex("SPLIT"), 100);
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].line, 5);
    }

    #[test]
    fn max_matches_respected() {
        // Two cross-boundary matches but max is 1
        let matches = search_boundary_overlap(b"aXb", b"Xc", 0, 1, &make_regex("X"), 1);
        assert!(matches.len() <= 1);
    }
}
