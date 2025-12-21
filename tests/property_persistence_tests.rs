// Property-based tests for persistence roundtrip (load → edit → save → load)
//
// These tests verify that the piece tree correctly handles all edge cases
// when saving and reloading files, including:
// - Piece boundary edits
// - Large files with lazy loading
// - Line ending conversions
// - Multiple scattered edits
// - Empty files and edge cases

mod common;

use fresh::model::buffer::TextBuffer;
use proptest::prelude::*;
use std::fs;
use std::io::Write;
use tempfile::TempDir;

// =============================================================================
// Edit Operation Types
// =============================================================================

/// Low-level buffer edit operations
#[derive(Debug, Clone)]
enum BufferOp {
    /// Insert bytes at a specific offset
    Insert {
        offset_percent: u8,
        content: Vec<u8>,
    },
    /// Delete bytes at a specific offset
    Delete { offset_percent: u8, len_percent: u8 },
    /// Insert at the very start
    InsertAtStart { content: Vec<u8> },
    /// Insert at the very end
    InsertAtEnd { content: Vec<u8> },
    /// Delete from the start
    DeleteFromStart { len_percent: u8 },
    /// Delete from the end
    DeleteFromEnd { len_percent: u8 },
}

impl BufferOp {
    /// Apply this operation to a buffer
    fn apply(&self, buffer: &mut TextBuffer) {
        let total = buffer.total_bytes();

        match self {
            BufferOp::Insert {
                offset_percent,
                content,
            } => {
                if !content.is_empty() {
                    let offset = if total == 0 {
                        0
                    } else {
                        (total * (*offset_percent as usize)) / 255
                    };
                    buffer.insert_bytes(offset.min(total), content.clone());
                }
            }
            BufferOp::Delete {
                offset_percent,
                len_percent,
            } => {
                if total > 0 {
                    let offset = (total * (*offset_percent as usize)) / 255;
                    let max_len = total.saturating_sub(offset);
                    let len = if max_len == 0 {
                        0
                    } else {
                        ((max_len * (*len_percent as usize)) / 255)
                            .max(1)
                            .min(max_len)
                    };
                    if len > 0 && offset < total {
                        buffer.delete_bytes(offset, len);
                    }
                }
            }
            BufferOp::InsertAtStart { content } => {
                if !content.is_empty() {
                    buffer.insert_bytes(0, content.clone());
                }
            }
            BufferOp::InsertAtEnd { content } => {
                if !content.is_empty() {
                    buffer.insert_bytes(total, content.clone());
                }
            }
            BufferOp::DeleteFromStart { len_percent } => {
                if total > 0 {
                    let len = ((total * (*len_percent as usize)) / 255).max(1).min(total);
                    buffer.delete_bytes(0, len);
                }
            }
            BufferOp::DeleteFromEnd { len_percent } => {
                if total > 0 {
                    let len = ((total * (*len_percent as usize)) / 255).max(1).min(total);
                    buffer.delete_bytes(total - len, len);
                }
            }
        }
    }

    /// Apply the same operation to a shadow Vec<u8> for verification
    fn apply_to_shadow(&self, shadow: &mut Vec<u8>) {
        let total = shadow.len();

        match self {
            BufferOp::Insert {
                offset_percent,
                content,
            } => {
                if !content.is_empty() {
                    let offset = if total == 0 {
                        0
                    } else {
                        (total * (*offset_percent as usize)) / 255
                    };
                    let offset = offset.min(total);
                    shadow.splice(offset..offset, content.iter().cloned());
                }
            }
            BufferOp::Delete {
                offset_percent,
                len_percent,
            } => {
                if total > 0 {
                    let offset = (total * (*offset_percent as usize)) / 255;
                    let max_len = total.saturating_sub(offset);
                    let len = if max_len == 0 {
                        0
                    } else {
                        ((max_len * (*len_percent as usize)) / 255)
                            .max(1)
                            .min(max_len)
                    };
                    if len > 0 && offset < total {
                        shadow.drain(offset..offset + len);
                    }
                }
            }
            BufferOp::InsertAtStart { content } => {
                if !content.is_empty() {
                    shadow.splice(0..0, content.iter().cloned());
                }
            }
            BufferOp::InsertAtEnd { content } => {
                if !content.is_empty() {
                    shadow.extend(content.iter().cloned());
                }
            }
            BufferOp::DeleteFromStart { len_percent } => {
                if total > 0 {
                    let len = ((total * (*len_percent as usize)) / 255).max(1).min(total);
                    shadow.drain(0..len);
                }
            }
            BufferOp::DeleteFromEnd { len_percent } => {
                if total > 0 {
                    let len = ((total * (*len_percent as usize)) / 255).max(1).min(total);
                    shadow.drain(total - len..total);
                }
            }
        }
    }
}

// =============================================================================
// Proptest Strategies
// =============================================================================

/// Strategy for generating printable ASCII content (no control chars except newline)
fn content_strategy(max_len: usize) -> impl Strategy<Value = Vec<u8>> {
    prop::collection::vec(
        prop_oneof![
            9 => 32u8..127u8, // Printable ASCII
            1 => Just(b'\n'),   // Newlines
        ],
        1..=max_len,
    )
}

/// Strategy for generating buffer operations
fn buffer_op_strategy() -> impl Strategy<Value = BufferOp> {
    prop_oneof![
        // Regular insert/delete at random positions
        3 => (any::<u8>(), content_strategy(50))
            .prop_map(|(offset_percent, content)| BufferOp::Insert { offset_percent, content }),
        2 => (any::<u8>(), any::<u8>())
            .prop_map(|(offset_percent, len_percent)| BufferOp::Delete { offset_percent, len_percent }),
        // Boundary operations (important edge cases)
        2 => content_strategy(30).prop_map(|content| BufferOp::InsertAtStart { content }),
        2 => content_strategy(30).prop_map(|content| BufferOp::InsertAtEnd { content }),
        1 => any::<u8>().prop_map(|len_percent| BufferOp::DeleteFromStart { len_percent }),
        1 => any::<u8>().prop_map(|len_percent| BufferOp::DeleteFromEnd { len_percent }),
    ]
}

/// Strategy for generating initial file content
fn initial_content_strategy() -> impl Strategy<Value = Vec<u8>> {
    prop_oneof![
        // Empty file
        1 => Just(vec![]),
        // Small file (single piece likely)
        3 => content_strategy(100),
        // Medium file (multiple pieces possible)
        3 => content_strategy(1000),
        // Larger file (definitely multiple pieces)
        2 => content_strategy(5000),
        // File with specific patterns
        1 => Just(b"line1\nline2\nline3\n".to_vec()),
        1 => Just(b"a".repeat(1000)),
        1 => Just((0..100).map(|i| format!("line {}\n", i)).collect::<String>().into_bytes()),
    ]
}

/// Strategy for generating aggressive operations that create many pieces
fn aggressive_op_strategy() -> impl Strategy<Value = BufferOp> {
    prop_oneof![
        // Many small inserts to fragment the piece tree
        5 => (any::<u8>(), prop::collection::vec(32u8..127u8, 1..5))
            .prop_map(|(offset_percent, content)| BufferOp::Insert { offset_percent, content }),
        // Small deletes
        3 => (any::<u8>(), 1u8..30u8)
            .prop_map(|(offset_percent, len_percent)| BufferOp::Delete { offset_percent, len_percent }),
        // Boundary inserts
        2 => prop::collection::vec(32u8..127u8, 1..10)
            .prop_map(|content| BufferOp::InsertAtStart { content }),
        2 => prop::collection::vec(32u8..127u8, 1..10)
            .prop_map(|content| BufferOp::InsertAtEnd { content }),
    ]
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Read the entire buffer content
fn read_buffer_content(buffer: &mut TextBuffer) -> Vec<u8> {
    let total = buffer.total_bytes();
    if total == 0 {
        return vec![];
    }
    buffer.get_text_range_mut(0, total).unwrap_or_default()
}

/// Create a temp file with given content and return its path
fn create_temp_file(dir: &TempDir, name: &str, content: &[u8]) -> std::path::PathBuf {
    let path = dir.path().join(name);
    let mut file = fs::File::create(&path).unwrap();
    file.write_all(content).unwrap();
    file.flush().unwrap();
    path
}

// =============================================================================
// Property Tests - Basic Roundtrip
// =============================================================================

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 200,
        max_shrink_iters: 2000,
        ..ProptestConfig::default()
    })]

    /// Core property: load → edit → save → load should preserve content
    ///
    /// This is the fundamental invariant for file persistence:
    /// After any sequence of edits, saving and reloading should give
    /// the exact same content as what was in memory before saving.
    #[test]
    fn prop_roundtrip_preserves_content(
        initial_content in initial_content_strategy(),
        ops in prop::collection::vec(buffer_op_strategy(), 0..30)
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "test.txt", &initial_content);

        // Load the file
        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

        // Apply all operations
        let mut shadow = initial_content.clone();
        for op in &ops {
            op.apply(&mut buffer);
            op.apply_to_shadow(&mut shadow);
        }

        // Verify buffer matches shadow before save
        let content_before_save = read_buffer_content(&mut buffer);
        prop_assert_eq!(
            &content_before_save,
            &shadow,
            "Buffer diverged from shadow before save"
        );

        // Save to a new file
        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        // Reload from the saved file
        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        // The reloaded content should match what we had before saving
        prop_assert_eq!(
            &reloaded_content,
            &content_before_save,
            "Roundtrip failed: reloaded content differs from pre-save content\n\
             Initial content len: {}\n\
             Operations: {}\n\
             Pre-save len: {}\n\
             Reloaded len: {}",
            initial_content.len(),
            ops.len(),
            content_before_save.len(),
            reloaded_content.len()
        );
    }

    /// Property: save to same file (overwrite) works correctly
    #[test]
    fn prop_save_overwrite_roundtrip(
        initial_content in initial_content_strategy(),
        ops in prop::collection::vec(buffer_op_strategy(), 1..20)
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "test.txt", &initial_content);

        // Load the file
        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

        // Apply operations
        let mut shadow = initial_content.clone();
        for op in &ops {
            op.apply(&mut buffer);
            op.apply_to_shadow(&mut shadow);
        }

        let content_before_save = read_buffer_content(&mut buffer);

        // Save to the SAME file (overwrite)
        buffer.save_to_file(&file_path).unwrap();

        // Reload
        let mut reloaded = TextBuffer::load_from_file(&file_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &content_before_save,
            "Overwrite roundtrip failed"
        );
    }

    /// Property: multiple save/load cycles preserve content
    #[test]
    fn prop_multiple_roundtrips(
        initial_content in content_strategy(500),
        ops1 in prop::collection::vec(buffer_op_strategy(), 1..10),
        ops2 in prop::collection::vec(buffer_op_strategy(), 1..10),
        ops3 in prop::collection::vec(buffer_op_strategy(), 1..10),
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "test.txt", &initial_content);

        let mut shadow = initial_content.clone();

        // Round 1: load, edit, save
        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
        for op in &ops1 {
            op.apply(&mut buffer);
            op.apply_to_shadow(&mut shadow);
        }
        buffer.save_to_file(&file_path).unwrap();

        // Round 2: load, edit, save
        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
        for op in &ops2 {
            op.apply(&mut buffer);
            op.apply_to_shadow(&mut shadow);
        }
        buffer.save_to_file(&file_path).unwrap();

        // Round 3: load, edit, save
        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
        for op in &ops3 {
            op.apply(&mut buffer);
            op.apply_to_shadow(&mut shadow);
        }
        buffer.save_to_file(&file_path).unwrap();

        // Final verification
        let mut final_buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
        let final_content = read_buffer_content(&mut final_buffer);

        prop_assert_eq!(
            &final_content,
            &shadow,
            "Multiple roundtrips failed to preserve content"
        );
    }
}

// =============================================================================
// Property Tests - Piece Tree Edge Cases
// =============================================================================

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 100,
        max_shrink_iters: 1000,
        ..ProptestConfig::default()
    })]

    /// Property: many small edits creating fragmented piece tree still roundtrips
    #[test]
    fn prop_fragmented_tree_roundtrip(
        initial_content in content_strategy(200),
        ops in prop::collection::vec(aggressive_op_strategy(), 20..50)
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "test.txt", &initial_content);

        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
        let mut shadow = initial_content.clone();

        for op in &ops {
            op.apply(&mut buffer);
            op.apply_to_shadow(&mut shadow);
        }

        let content_before_save = read_buffer_content(&mut buffer);
        prop_assert_eq!(&content_before_save, &shadow, "Shadow mismatch before save");

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &content_before_save,
            "Fragmented tree roundtrip failed"
        );
    }

    /// Property: alternating insert/delete at same position
    #[test]
    fn prop_alternating_edits_same_position(
        initial_content in content_strategy(100),
        position_percent in 0u8..=255u8,
        iterations in 5usize..20
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "test.txt", &initial_content);

        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
        let mut shadow = initial_content.clone();

        let insert_content = b"XYZ".to_vec();

        for _ in 0..iterations {
            // Insert
            let op = BufferOp::Insert {
                offset_percent: position_percent,
                content: insert_content.clone(),
            };
            op.apply(&mut buffer);
            op.apply_to_shadow(&mut shadow);

            // Delete what we just inserted
            let total = shadow.len();
            if total >= 3 {
                let offset = (total * (position_percent as usize)) / 255;
                let offset = offset.min(total.saturating_sub(3));
                buffer.delete_bytes(offset, 3);
                shadow.drain(offset..offset + 3);
            }
        }

        let content_before_save = read_buffer_content(&mut buffer);
        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &content_before_save,
            "Alternating edits roundtrip failed"
        );
    }

    /// Property: edits exactly at piece boundaries
    #[test]
    fn prop_piece_boundary_edits(
        // Create content with known structure
        line_count in 5usize..20,
        line_len in 10usize..50,
    ) {
        let temp_dir = TempDir::new().unwrap();

        // Create content with predictable line structure
        let line: Vec<u8> = (0..line_len).map(|i| b'a' + (i % 26) as u8).collect();
        let mut initial_content = Vec::new();
        for _ in 0..line_count {
            initial_content.extend(&line);
            initial_content.push(b'\n');
        }

        let file_path = create_temp_file(&temp_dir, "test.txt", &initial_content);
        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
        let mut shadow = initial_content.clone();

        // Insert at line boundaries
        let line_with_newline = line_len + 1;
        for i in (0..line_count).rev() {
            let offset = i * line_with_newline;
            if offset <= shadow.len() {
                let insert = b">>".to_vec();
                buffer.insert_bytes(offset, insert.clone());
                shadow.splice(offset..offset, insert.iter().cloned());
            }
        }

        let content_before_save = read_buffer_content(&mut buffer);
        prop_assert_eq!(&content_before_save, &shadow, "Shadow mismatch");

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &content_before_save,
            "Piece boundary edits roundtrip failed"
        );
    }
}

// =============================================================================
// Property Tests - Large Files with Lazy Loading
// =============================================================================

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 50,
        max_shrink_iters: 500,
        ..ProptestConfig::default()
    })]

    /// Property: large file with unloaded regions roundtrips correctly
    #[test]
    fn prop_large_file_unloaded_regions_roundtrip(
        // Use smaller "large" files for property testing (threshold at 1KB)
        file_size in 2000usize..5000,
        edit_offset_percent in 0u8..=255u8,
        edit_content in content_strategy(50),
    ) {
        let temp_dir = TempDir::new().unwrap();

        // Create a file larger than our test threshold
        let initial_content: Vec<u8> = (0..file_size)
            .map(|i| b'a' + (i % 26) as u8)
            .collect();
        let file_path = create_temp_file(&temp_dir, "large.txt", &initial_content);

        // Load with a low threshold to trigger large file mode
        let mut buffer = TextBuffer::load_from_file(&file_path, 1000).unwrap();

        // Make a single edit (this should only load the affected region)
        let edit_offset = (file_size * (edit_offset_percent as usize)) / 255;
        let edit_offset = edit_offset.min(file_size);
        buffer.insert_bytes(edit_offset, edit_content.clone());

        // Build expected content
        let mut expected = initial_content.clone();
        expected.splice(edit_offset..edit_offset, edit_content.iter().cloned());

        let content_before_save = read_buffer_content(&mut buffer);
        prop_assert_eq!(
            content_before_save.len(),
            expected.len(),
            "Length mismatch before save"
        );

        // Save
        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        // Reload (as small file to verify full content)
        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &expected,
            "Large file roundtrip failed"
        );
    }

    /// Property: large file with multiple scattered edits
    #[test]
    fn prop_large_file_scattered_edits(
        file_size in 3000usize..6000,
        edit_positions in prop::collection::vec(0u8..=255u8, 3..8),
    ) {
        let temp_dir = TempDir::new().unwrap();

        let initial_content: Vec<u8> = (0..file_size)
            .map(|i| b'a' + (i % 26) as u8)
            .collect();
        let file_path = create_temp_file(&temp_dir, "large.txt", &initial_content);

        // Load as large file
        let mut buffer = TextBuffer::load_from_file(&file_path, 1000).unwrap();
        let mut expected = initial_content.clone();

        // Make scattered edits (process in reverse order to maintain offsets)
        let mut positions: Vec<usize> = edit_positions
            .iter()
            .map(|p| (file_size * (*p as usize)) / 255)
            .collect();
        positions.sort();
        positions.dedup();
        positions.reverse();

        for pos in positions {
            let insert = b"[EDIT]".to_vec();
            let pos = pos.min(buffer.total_bytes());
            buffer.insert_bytes(pos, insert.clone());

            let pos = pos.min(expected.len());
            expected.splice(pos..pos, insert.iter().cloned());
        }

        // Save and reload
        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &expected,
            "Scattered edits roundtrip failed"
        );
    }

    /// Property: edit at very start of large file
    #[test]
    fn prop_large_file_edit_at_start(
        file_size in 2000usize..4000,
        insert_content in content_strategy(100),
    ) {
        let temp_dir = TempDir::new().unwrap();
        let initial_content: Vec<u8> = (0..file_size).map(|_| b'x').collect();
        let file_path = create_temp_file(&temp_dir, "large.txt", &initial_content);

        let mut buffer = TextBuffer::load_from_file(&file_path, 1000).unwrap();
        buffer.insert_bytes(0, insert_content.clone());

        let mut expected = insert_content.clone();
        expected.extend(&initial_content);

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(&reloaded_content, &expected, "Edit at start failed");
    }

    /// Property: edit at very end of large file
    #[test]
    fn prop_large_file_edit_at_end(
        file_size in 2000usize..4000,
        insert_content in content_strategy(100),
    ) {
        let temp_dir = TempDir::new().unwrap();
        let initial_content: Vec<u8> = (0..file_size).map(|_| b'x').collect();
        let file_path = create_temp_file(&temp_dir, "large.txt", &initial_content);

        let mut buffer = TextBuffer::load_from_file(&file_path, 1000).unwrap();
        buffer.insert_bytes(file_size, insert_content.clone());

        let mut expected = initial_content.clone();
        expected.extend(&insert_content);

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(&reloaded_content, &expected, "Edit at end failed");
    }
}

// =============================================================================
// Property Tests - Empty File Edge Cases
// =============================================================================

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 100,
        max_shrink_iters: 1000,
        ..ProptestConfig::default()
    })]

    /// Property: empty file can be edited and roundtripped
    #[test]
    fn prop_empty_file_roundtrip(
        ops in prop::collection::vec(buffer_op_strategy(), 1..20)
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "empty.txt", &[]);

        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
        let mut shadow: Vec<u8> = vec![];

        for op in &ops {
            op.apply(&mut buffer);
            op.apply_to_shadow(&mut shadow);
        }

        let content_before_save = read_buffer_content(&mut buffer);
        prop_assert_eq!(&content_before_save, &shadow, "Shadow mismatch");

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &content_before_save,
            "Empty file roundtrip failed"
        );
    }

    /// Property: file can be completely emptied and roundtripped
    #[test]
    fn prop_delete_all_content_roundtrip(
        initial_content in content_strategy(100),
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "test.txt", &initial_content);

        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

        // Delete all content
        let total = buffer.total_bytes();
        if total > 0 {
            buffer.delete_bytes(0, total);
        }

        prop_assert_eq!(buffer.total_bytes(), 0, "Buffer should be empty");

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        prop_assert_eq!(reloaded.total_bytes(), 0, "Reloaded buffer should be empty");
    }
}

// =============================================================================
// Property Tests - Size Changes
// =============================================================================

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 100,
        max_shrink_iters: 1000,
        ..ProptestConfig::default()
    })]

    /// Property: file that grows significantly roundtrips correctly
    #[test]
    fn prop_growing_file_roundtrip(
        initial_size in 50usize..200,
        growth_factor in 2usize..5,
    ) {
        let temp_dir = TempDir::new().unwrap();
        let initial_content: Vec<u8> = (0..initial_size).map(|i| b'a' + (i % 26) as u8).collect();
        let file_path = create_temp_file(&temp_dir, "test.txt", &initial_content);

        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
        let mut expected = initial_content.clone();

        // Add content to grow the file
        let additional: Vec<u8> = (0..initial_size * growth_factor)
            .map(|i| b'A' + (i % 26) as u8)
            .collect();
        buffer.insert_bytes(buffer.total_bytes(), additional.clone());
        expected.extend(&additional);

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(&reloaded_content, &expected, "Growing file roundtrip failed");
    }

    /// Property: file that shrinks significantly roundtrips correctly
    #[test]
    fn prop_shrinking_file_roundtrip(
        initial_size in 200usize..500,
        shrink_percent in 50u8..90,
    ) {
        let temp_dir = TempDir::new().unwrap();
        let initial_content: Vec<u8> = (0..initial_size).map(|i| b'a' + (i % 26) as u8).collect();
        let file_path = create_temp_file(&temp_dir, "test.txt", &initial_content);

        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

        // Delete a percentage of the content
        let delete_amount = (initial_size * (shrink_percent as usize)) / 100;
        buffer.delete_bytes(0, delete_amount);

        let expected: Vec<u8> = initial_content[delete_amount..].to_vec();

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(&reloaded_content, &expected, "Shrinking file roundtrip failed");
    }
}

// =============================================================================
// Targeted Unit Tests for Specific Edge Cases
// =============================================================================

#[test]
fn test_single_byte_file_roundtrip() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = create_temp_file(&temp_dir, "single.txt", b"X");

    let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
    assert_eq!(buffer.total_bytes(), 1);

    // Edit the single byte
    buffer.delete_bytes(0, 1);
    buffer.insert_bytes(0, b"Y".to_vec());

    let save_path = temp_dir.path().join("saved.txt");
    buffer.save_to_file(&save_path).unwrap();

    let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
    let content = read_buffer_content(&mut reloaded);
    assert_eq!(content, b"Y");
}

#[test]
fn test_newline_only_file_roundtrip() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = create_temp_file(&temp_dir, "newlines.txt", b"\n\n\n");

    let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

    // Insert between newlines
    buffer.insert_bytes(1, b"text".to_vec());

    let save_path = temp_dir.path().join("saved.txt");
    buffer.save_to_file(&save_path).unwrap();

    let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
    let content = read_buffer_content(&mut reloaded);
    assert_eq!(content, b"\ntext\n\n");
}

#[test]
fn test_binary_like_content_roundtrip() {
    let temp_dir = TempDir::new().unwrap();

    // Content with bytes that might be problematic - but avoiding CR (13) since
    // the buffer normalizes CRLF -> LF and CR -> LF.
    // Use a range that doesn't include CR to test binary-safe handling of other bytes.
    let mut initial: Vec<u8> = (0u8..13).collect();
    initial.extend(14u8..=255); // Skip CR (13)
    let file_path = create_temp_file(&temp_dir, "binary.bin", &initial);

    let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

    // Insert in the middle
    buffer.insert_bytes(128, b"MIDDLE".to_vec());

    let mut expected = initial.clone();
    expected.splice(128..128, b"MIDDLE".iter().cloned());

    let save_path = temp_dir.path().join("saved.bin");
    buffer.save_to_file(&save_path).unwrap();

    let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
    let content = read_buffer_content(&mut reloaded);
    assert_eq!(content, expected);
}

#[test]
fn test_cr_preserved_in_binary_content() {
    // CR bytes are preserved in the buffer (no normalization)
    let temp_dir = TempDir::new().unwrap();

    // Content with standalone CR (not CRLF)
    let initial = b"hello\rworld".to_vec();
    let file_path = create_temp_file(&temp_dir, "cr.txt", &initial);

    let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
    let content = read_buffer_content(&mut buffer);

    // CR should be preserved
    assert_eq!(content, b"hello\rworld");

    // Save and reload
    let save_path = temp_dir.path().join("saved.txt");
    buffer.save_to_file(&save_path).unwrap();

    let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
    let reloaded_content = read_buffer_content(&mut reloaded);
    assert_eq!(reloaded_content, b"hello\rworld");
}

#[test]
fn test_repeated_save_load_cycles() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = create_temp_file(&temp_dir, "cycles.txt", b"initial");

    let mut content = b"initial".to_vec();

    for i in 0..10 {
        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

        // Add a marker for this cycle
        let marker = format!("[{}]", i).into_bytes();
        buffer.insert_bytes(buffer.total_bytes(), marker.clone());
        content.extend(&marker);

        buffer.save_to_file(&file_path).unwrap();
    }

    let mut final_buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
    let final_content = read_buffer_content(&mut final_buffer);
    assert_eq!(final_content, content);
}

#[test]
fn test_large_file_edit_in_middle_preserves_unloaded() {
    let temp_dir = TempDir::new().unwrap();

    // Create a file that will be loaded lazily
    let size = 10_000;
    let initial: Vec<u8> = (0..size).map(|i| b'a' + (i % 26) as u8).collect();
    let file_path = create_temp_file(&temp_dir, "large.txt", &initial);

    // Load with low threshold to trigger lazy loading
    let mut buffer = TextBuffer::load_from_file(&file_path, 1000).unwrap();

    // Edit exactly in the middle
    let mid = size / 2;
    buffer.insert_bytes(mid, b"[MIDDLE]".to_vec());

    let mut expected = initial.clone();
    expected.splice(mid..mid, b"[MIDDLE]".iter().cloned());

    let save_path = temp_dir.path().join("saved.txt");
    buffer.save_to_file(&save_path).unwrap();

    // Verify the entire file content
    let saved = fs::read(&save_path).unwrap();
    assert_eq!(saved, expected);
}

// =============================================================================
// Property Tests - Line Ending Handling
// =============================================================================

/// Strategy for generating content with specific line endings
fn content_with_crlf_strategy(max_lines: usize) -> impl Strategy<Value = Vec<u8>> {
    prop::collection::vec(
        prop::collection::vec(32u8..127u8, 5..30), // Lines of printable ASCII
        1..=max_lines,
    )
    .prop_map(|lines| {
        let mut result = Vec::new();
        for (i, line) in lines.iter().enumerate() {
            result.extend(line);
            if i < lines.len() - 1 {
                result.extend(b"\r\n"); // CRLF line endings
            }
        }
        result
    })
}

fn content_with_lf_strategy(max_lines: usize) -> impl Strategy<Value = Vec<u8>> {
    prop::collection::vec(prop::collection::vec(32u8..127u8, 5..30), 1..=max_lines).prop_map(
        |lines| {
            let mut result = Vec::new();
            for (i, line) in lines.iter().enumerate() {
                result.extend(line);
                if i < lines.len() - 1 {
                    result.push(b'\n'); // LF line endings
                }
            }
            result
        },
    )
}

fn content_with_mixed_endings_strategy(max_lines: usize) -> impl Strategy<Value = Vec<u8>> {
    prop::collection::vec(
        (prop::collection::vec(32u8..127u8, 5..30), prop::bool::ANY),
        1..=max_lines,
    )
    .prop_map(|lines| {
        let mut result = Vec::new();
        for (i, (line, use_crlf)) in lines.iter().enumerate() {
            result.extend(line);
            if i < lines.len() - 1 {
                if *use_crlf {
                    result.extend(b"\r\n");
                } else {
                    result.push(b'\n');
                }
            }
        }
        result
    })
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 100,
        max_shrink_iters: 1000,
        ..ProptestConfig::default()
    })]

    /// Property: CRLF files are preserved exactly (no normalization)
    #[test]
    fn prop_crlf_file_roundtrip(
        content in content_with_crlf_strategy(20),
        ops in prop::collection::vec(buffer_op_strategy(), 0..10),
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "crlf.txt", &content);

        // Load the file (bytes are preserved, no normalization)
        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

        // Content internally should preserve original bytes including CRLF
        let internal_content = read_buffer_content(&mut buffer);
        prop_assert_eq!(
            &internal_content,
            &content,
            "Internal content should match original file"
        );

        // Apply some operations (shadow also tracks CRLF bytes)
        let mut shadow = content.clone();
        for op in &ops {
            op.apply(&mut buffer);
            op.apply_to_shadow(&mut shadow);
        }

        let content_before_save = read_buffer_content(&mut buffer);
        prop_assert_eq!(&content_before_save, &shadow, "Buffer should match shadow");

        // Save the file
        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        // The saved file should match exactly what was in the buffer
        let saved_bytes = fs::read(&save_path).unwrap();
        prop_assert_eq!(
            &saved_bytes,
            &content_before_save,
            "Saved file should match buffer content exactly"
        );

        // Reload and verify content matches
        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &content_before_save,
            "Reloaded content should match pre-save content"
        );
    }

    /// Property: LF files stay as LF
    #[test]
    fn prop_lf_file_stays_lf(
        content in content_with_lf_strategy(20),
        ops in prop::collection::vec(buffer_op_strategy(), 0..10),
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "lf.txt", &content);

        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

        for op in &ops {
            op.apply(&mut buffer);
        }

        let content_before_save = read_buffer_content(&mut buffer);

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        // Verify saved file has only LF, no CR
        let saved_bytes = fs::read(&save_path).unwrap();
        prop_assert!(
            !saved_bytes.contains(&b'\r'),
            "LF file should not have any CR bytes after save"
        );

        // Reload and verify
        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &content_before_save,
            "LF file roundtrip failed"
        );
    }

    /// Property: mixed line endings are preserved exactly (no normalization)
    #[test]
    fn prop_mixed_endings_preserved(
        content in content_with_mixed_endings_strategy(15),
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_temp_file(&temp_dir, "mixed.txt", &content);

        let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

        // Internal content should preserve original bytes
        let internal_content = read_buffer_content(&mut buffer);
        prop_assert_eq!(
            &internal_content,
            &content,
            "Internal content should match original file"
        );

        // Edit and save
        buffer.insert_bytes(0, b"PREFIX: ".to_vec());

        let mut expected = b"PREFIX: ".to_vec();
        expected.extend(&content);

        let content_before_save = read_buffer_content(&mut buffer);
        prop_assert_eq!(&content_before_save, &expected, "Buffer should match expected");

        let save_path = temp_dir.path().join("saved.txt");
        buffer.save_to_file(&save_path).unwrap();

        let mut reloaded = TextBuffer::load_from_file(&save_path, 0).unwrap();
        let reloaded_content = read_buffer_content(&mut reloaded);

        prop_assert_eq!(
            &reloaded_content,
            &content_before_save,
            "Mixed endings roundtrip failed"
        );
    }
}

// =============================================================================
// Targeted Unit Tests for Line Endings
// =============================================================================

#[test]
fn test_crlf_preserved_after_edit() {
    let temp_dir = TempDir::new().unwrap();
    let crlf_content = b"line1\r\nline2\r\nline3\r\n";
    let file_path = create_temp_file(&temp_dir, "crlf.txt", crlf_content);

    let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

    // Content is preserved as-is: "line1\r\nline2\r\nline3\r\n"
    // Insert after "line1\r\n" (offset 7)
    buffer.insert_bytes(7, b"inserted\r\n".to_vec());

    let save_path = temp_dir.path().join("saved.txt");
    buffer.save_to_file(&save_path).unwrap();

    let saved = fs::read(&save_path).unwrap();

    // Should have CRLF throughout (original preserved, inserted has CRLF too)
    assert_eq!(
        saved, b"line1\r\ninserted\r\nline2\r\nline3\r\n",
        "CRLF should be preserved"
    );
}

#[test]
fn test_lf_file_no_crlf_after_save() {
    let temp_dir = TempDir::new().unwrap();
    let lf_content = b"line1\nline2\nline3\n";
    let file_path = create_temp_file(&temp_dir, "lf.txt", lf_content);

    let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();
    buffer.insert_bytes(6, b"inserted\n".to_vec());

    let save_path = temp_dir.path().join("saved.txt");
    buffer.save_to_file(&save_path).unwrap();

    let saved = fs::read(&save_path).unwrap();

    // Should have only LF
    assert!(!saved.contains(&b'\r'), "LF file should not gain CR bytes");
    assert_eq!(saved, b"line1\ninserted\nline2\nline3\n");
}

#[test]
fn test_empty_lines_with_crlf() {
    let temp_dir = TempDir::new().unwrap();
    // File with empty lines (consecutive CRLFs)
    let content = b"line1\r\n\r\n\r\nline2\r\n";
    let file_path = create_temp_file(&temp_dir, "empty_lines.txt", content);

    let mut buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

    // Content is preserved (no normalization)
    let internal = read_buffer_content(&mut buffer);
    assert_eq!(internal, b"line1\r\n\r\n\r\nline2\r\n");

    // Insert after "line1\r\n" (offset 7)
    buffer.insert_bytes(7, b"X".to_vec());

    let save_path = temp_dir.path().join("saved.txt");
    buffer.save_to_file(&save_path).unwrap();

    let saved = fs::read(&save_path).unwrap();
    assert_eq!(saved, b"line1\r\nX\r\n\r\nline2\r\n");
}
