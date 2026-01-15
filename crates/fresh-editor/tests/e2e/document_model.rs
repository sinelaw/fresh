use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use fresh::model::document_model::{DocumentModel, DocumentPosition};

/// Test DocumentModel with a small file (< 1MB) that has precise line indexing
#[test]
fn test_document_model_small_file() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("small_test.txt");

    // Create a small test file with known content (10 lines, ~500 bytes)
    let content = "Line 1: The quick brown fox\n\
                   Line 2: jumps over the lazy dog\n\
                   Line 3: Lorem ipsum dolor sit amet\n\
                   Line 4: consectetur adipiscing elit\n\
                   Line 5: sed do eiusmod tempor\n\
                   Line 6: incididunt ut labore et\n\
                   Line 7: dolore magna aliqua\n\
                   Line 8: Ut enim ad minim veniam\n\
                   Line 9: quis nostrud exercitation\n\
                   Line 10: ullamco laboris nisi";

    fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Get the editor state to access DocumentModel
    let state = harness.editor_mut().active_state_mut();

    // Test 1: Verify capabilities for small file
    let caps = state.capabilities();
    assert!(
        caps.has_line_index,
        "Small file should have line indexing available"
    );
    assert!(
        !caps.uses_lazy_loading,
        "Small file should not use lazy loading"
    );
    assert_eq!(caps.byte_length, content.len(), "Byte length should match");
    assert_eq!(
        caps.approximate_line_count, 10,
        "Should have 10 lines (content ends without newline)"
    );

    // Test 2: Verify position_to_offset with line/column positions
    let pos_line_0_col_0 = DocumentPosition::line_col(0, 0);
    let offset = state.position_to_offset(pos_line_0_col_0).unwrap();
    assert_eq!(offset, 0, "Line 0, column 0 should be offset 0");

    let pos_line_1_col_0 = DocumentPosition::line_col(1, 0);
    let offset = state.position_to_offset(pos_line_1_col_0).unwrap();
    assert_eq!(
        offset, 28,
        "Line 1, column 0 should be at offset 28 (after first line and newline)"
    );

    // Test 3: Verify offset_to_position returns LineColumn for small files
    let pos = state.offset_to_position(0);
    match pos {
        DocumentPosition::LineColumn { line, column } => {
            assert_eq!(line, 0, "Offset 0 should be line 0");
            assert_eq!(column, 0, "Offset 0 should be column 0");
        }
        DocumentPosition::ByteOffset(_) => {
            panic!("Small file should use LineColumn positions, not ByteOffset");
        }
    }

    // Test 4: Verify get_viewport_content returns correct lines
    let viewport = state
        .get_viewport_content(DocumentPosition::byte(0), 5)
        .unwrap();

    assert_eq!(viewport.lines.len(), 5, "Should return 5 lines");

    // Verify first line content
    assert_eq!(
        viewport.lines[0].content, "Line 1: The quick brown fox",
        "First line should match"
    );
    assert_eq!(
        viewport.lines[0].byte_offset, 0,
        "First line starts at offset 0"
    );
    assert!(
        viewport.lines[0].has_newline,
        "First line should have newline"
    );
    assert_eq!(
        viewport.lines[0].approximate_line_number,
        Some(0),
        "Should have precise line number 0"
    );

    // Verify second line
    assert_eq!(
        viewport.lines[1].content, "Line 2: jumps over the lazy dog",
        "Second line should match"
    );
    assert_eq!(
        viewport.lines[1].byte_offset, 28,
        "Second line starts at offset 28"
    );

    assert!(viewport.has_more, "Should have more lines after these 5");

    // Test 5: Verify get_line_content works for small files (without newline)
    let line_0 = state.get_line_content(0);
    assert_eq!(
        line_0,
        Some("Line 1: The quick brown fox".to_string()),
        "get_line_content should work for small files (returns line without trailing newline)"
    );

    // Test 6: Verify get_range works
    let text = state
        .get_range(DocumentPosition::byte(0), DocumentPosition::byte(11))
        .unwrap();
    assert_eq!(text, "Line 1: The", "get_range should return correct text");

    println!("✓ Small file DocumentModel tests passed");
}

/// Test DocumentModel with a large file (> 100MB) that uses byte offsets
/// Note: This test is ignored because it requires creating a 61MB file
/// which may not complete properly in all test environments
#[test]
#[ignore]
fn test_document_model_large_file() {
    // Get shared large file (61MB)
    let big_txt_path = TestFixture::big_txt_for_test("document_model_large").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();

    let state = harness.editor_mut().active_state_mut();

    // Test 1: Verify capabilities for large file
    let caps = state.capabilities();
    assert!(
        !caps.has_line_index,
        "Large file should not have precise line indexing"
    );
    // Note: uses_lazy_loading is not yet implemented (TODO in EditorState)
    // When implemented, large files will report uses_lazy_loading = true
    assert!(
        caps.byte_length > 60_000_000,
        "Should be a large file (> 60MB)"
    );
    assert!(
        caps.approximate_line_count > 0,
        "Should have estimated line count"
    );

    // Test 2: Verify position_to_offset works with byte offsets
    let pos_byte_0 = DocumentPosition::byte(0);
    let offset = state.position_to_offset(pos_byte_0).unwrap();
    assert_eq!(offset, 0, "Byte offset 0 should map to offset 0");

    let pos_byte_1000 = DocumentPosition::byte(1000);
    let offset = state.position_to_offset(pos_byte_1000).unwrap();
    assert_eq!(offset, 1000, "Byte offset should map directly");

    // Test 3: Verify offset_to_position returns ByteOffset for large files
    let pos = state.offset_to_position(1000);
    match pos {
        DocumentPosition::ByteOffset(offset) => {
            assert_eq!(offset, 1000, "Should return byte offset position");
        }
        DocumentPosition::LineColumn { .. } => {
            panic!("Large file should use ByteOffset positions, not LineColumn");
        }
    }

    // Test 4: Verify get_viewport_content works with lazy loading
    let viewport = state
        .get_viewport_content(DocumentPosition::byte(0), 10)
        .unwrap();

    assert!(
        !viewport.lines.is_empty(),
        "Should return at least some lines"
    );
    assert!(
        viewport.lines.len() <= 10,
        "Should not return more than requested"
    );

    // Verify first line starts at beginning
    assert_eq!(
        viewport.lines[0].byte_offset, 0,
        "First line should start at offset 0"
    );

    // For large files without line index, approximate_line_number should be None
    assert_eq!(
        viewport.lines[0].approximate_line_number, None,
        "Large file should not have precise line numbers"
    );

    // Test 5: Verify get_line_content returns None for large files (no line index)
    let line_0 = state.get_line_content(0);
    assert_eq!(
        line_0, None,
        "get_line_content should return None for large files without line index"
    );

    // Test 6: Verify get_chunk_at_offset works for large files
    let (chunk_offset, chunk_text) = state.get_chunk_at_offset(0, 100).unwrap();
    assert_eq!(chunk_offset, 0, "Chunk should start at requested offset");
    assert!(!chunk_text.is_empty(), "Chunk should contain some text");
    assert!(chunk_text.len() <= 200, "Chunk should be reasonably sized");

    // Test 7: Verify viewport content from middle of file
    let mid_offset = 30_000_000; // ~30MB into the file
    let viewport_mid = state
        .get_viewport_content(DocumentPosition::byte(mid_offset), 5)
        .unwrap();

    assert!(
        !viewport_mid.lines.is_empty(),
        "Should get lines from middle of file"
    );
    // Note: The viewport may start slightly before the requested offset if we're mid-line,
    // as line_iterator aligns to line boundaries
    assert!(
        viewport_mid.lines[0].byte_offset < mid_offset + 100,
        "Viewport should start near the requested offset (within one line)"
    );

    // The test file has lines like "@00000000: xxx", so we can verify content
    let first_line = &viewport_mid.lines[0].content;
    assert!(
        first_line.contains('@'),
        "Line should contain @ marker from test file format"
    );

    println!("✓ Large file DocumentModel tests passed");
}

/// Test DocumentModel editing operations
#[test]
fn test_document_model_editing() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("edit_test.txt");

    let content = "Line 1\nLine 2\nLine 3";
    fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    let state = harness.editor_mut().active_state_mut();

    // Test 1: Insert operation
    let insert_pos = DocumentPosition::byte(7); // After "Line 1\n"
    let inserted_bytes = state
        .insert(insert_pos, "INSERTED ")
        .expect("Insert should succeed");

    assert_eq!(inserted_bytes, 9, "Should have inserted 9 bytes");

    // Verify content after insert
    let text = state
        .get_range(DocumentPosition::byte(0), DocumentPosition::byte(50))
        .unwrap();
    assert_eq!(
        text, "Line 1\nINSERTED Line 2\nLine 3",
        "Insert should add text at correct position"
    );

    // Test 2: Delete operation
    let delete_start = DocumentPosition::byte(7);
    let delete_end = DocumentPosition::byte(16); // Delete "INSERTED "
    state
        .delete(delete_start, delete_end)
        .expect("Delete should succeed");

    // Verify content after delete
    let text_after_delete = state
        .get_range(DocumentPosition::byte(0), DocumentPosition::byte(50))
        .unwrap();
    assert_eq!(
        text_after_delete, "Line 1\nLine 2\nLine 3",
        "Delete should remove text"
    );

    // Test 3: Replace operation
    let replace_start = DocumentPosition::byte(0);
    let replace_end = DocumentPosition::byte(6); // "Line 1"
    state
        .replace(replace_start, replace_end, "REPLACED")
        .expect("Replace should succeed");

    let text_after_replace = state
        .get_range(DocumentPosition::byte(0), DocumentPosition::byte(50))
        .unwrap();
    assert_eq!(
        text_after_replace, "REPLACED\nLine 2\nLine 3",
        "Replace should substitute text"
    );

    println!("✓ DocumentModel editing tests passed");
}

/// Test DocumentModel search operations
#[test]
fn test_document_model_search() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("search_test.txt");

    let content = "The quick brown fox\njumps over the lazy dog\nThe end";
    fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    let state = harness.editor_mut().active_state_mut();

    // Test 1: Find all matches of "the" (case-insensitive search would be in buffer layer)
    let _matches = state.find_matches("the", None).unwrap();

    // Should find "the" in "the lazy dog" and "The" in "The quick" and "The end"
    // But find_matches uses buffer.find_all which is case-sensitive
    // Let's search for "The" specifically
    let matches_the = state.find_matches("The", None).unwrap();
    assert_eq!(matches_the.len(), 2, "Should find 2 occurrences of 'The'");
    assert_eq!(matches_the[0], 0, "First 'The' at offset 0");
    assert_eq!(
        matches_the[1], 44,
        "Second 'The' at offset 44 (after 44 chars)"
    );

    // Test 2: Search in a specific range
    let range_start = DocumentPosition::byte(20);
    let range_end = DocumentPosition::byte(44);
    let matches_in_range = state
        .find_matches("the", Some((range_start, range_end)))
        .unwrap();

    assert_eq!(
        matches_in_range.len(),
        1,
        "Should find 1 occurrence of 'the' in range"
    );
    assert_eq!(
        matches_in_range[0], 31,
        "Should find 'the' at offset 31 in 'the lazy dog'"
    );

    println!("✓ DocumentModel search tests passed");
}
