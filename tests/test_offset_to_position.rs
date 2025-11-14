/// Focused test for offset_to_position bug
/// This test creates a simple buffer and verifies line number calculations
use fresh::text_buffer::TextBuffer;

#[test]
fn test_offset_to_position_simple() {
    // Create a buffer with known line structure
    // Line 0: "a\n" (bytes 0-1, newline at 1)
    // Line 1: "b\n" (bytes 2-3, newline at 3)
    // Line 2: "c\n" (bytes 4-5, newline at 5)
    // Line 3: "d" (bytes 6, no newline)
    let content = b"a\nb\nc\nd";
    let buffer = TextBuffer::from_bytes(content.to_vec());

    println!(
        "Buffer content: {:?}",
        String::from_utf8_lossy(&content.to_vec())
    );
    println!("Buffer length: {}", buffer.len());
    println!("Line count: {:?}", buffer.line_count());

    // Test each byte position
    for byte_pos in 0..=buffer.len() {
        let pos = buffer
            .offset_to_position(byte_pos)
            .expect("small buffer should have line metadata");
        println!(
            "byte_pos={}, line={}, column={}",
            byte_pos, pos.line, pos.column
        );
    }

    // Verify specific positions
    let pos = buffer
        .offset_to_position(0)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 0,
        "Byte 0 should be on line 0, got line {}",
        pos.line
    );
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(1)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 0,
        "Byte 1 (newline) should be on line 0, got line {}",
        pos.line
    );
    assert_eq!(pos.column, 1);

    let pos = buffer
        .offset_to_position(2)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 1,
        "Byte 2 should be on line 1, got line {}",
        pos.line
    );
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(3)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 1,
        "Byte 3 (newline) should be on line 1, got line {}",
        pos.line
    );
    assert_eq!(pos.column, 1);

    let pos = buffer
        .offset_to_position(4)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 2,
        "Byte 4 should be on line 2, got line {}",
        pos.line
    );
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(6)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 3,
        "Byte 6 should be on line 3, got line {}",
        pos.line
    );
    assert_eq!(pos.column, 0);
}

#[test]
fn test_offset_to_position_after_insert() {
    // Start with simple content
    let mut buffer = TextBuffer::from_bytes(b"a\nb\n".to_vec());

    println!("\n=== Initial buffer ===");
    println!("Content: {:?}", buffer.slice_bytes(0..buffer.len()));
    println!("Line count: {:?}", buffer.line_count());

    for byte_pos in 0..=buffer.len() {
        let pos = buffer
            .offset_to_position(byte_pos)
            .expect("small buffer should have line metadata");
        println!(
            "byte_pos={}, line={}, column={}",
            byte_pos, pos.line, pos.column
        );
    }

    // Insert at position 2 (start of line 1)
    buffer.insert_at_position(
        fresh::piece_tree::Position { line: 1, column: 0 },
        b"x\n".to_vec(),
    );

    println!("\n=== After inserting 'x\\n' at line 1, column 0 ===");
    println!("Content: {:?}", buffer.slice_bytes(0..buffer.len()));
    println!("Line count: {:?}", buffer.line_count());

    for byte_pos in 0..=buffer.len() {
        let pos = buffer
            .offset_to_position(byte_pos)
            .expect("small buffer should have line metadata");
        println!(
            "byte_pos={}, line={}, column={}",
            byte_pos, pos.line, pos.column
        );
    }

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
        "Byte 2 (start of inserted line) should be on line 1, got line {}",
        pos.line
    );

    let pos = buffer
        .offset_to_position(4)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 2,
        "Byte 4 (start of 'b') should be on line 2 after insert, got line {}",
        pos.line
    );
}

#[test]
fn test_offset_to_position_empty_lines() {
    // Test with empty lines: "\n\n\n"
    let buffer = TextBuffer::from_bytes(b"\n\n\n".to_vec());

    println!("\n=== Buffer with empty lines ===");
    println!("Content: {:?}", buffer.slice_bytes(0..buffer.len()));
    println!("Line count: {:?}", buffer.line_count());

    for byte_pos in 0..=buffer.len() {
        let pos = buffer
            .offset_to_position(byte_pos)
            .expect("small buffer should have line metadata");
        println!(
            "byte_pos={}, line={}, column={}",
            byte_pos, pos.line, pos.column
        );
    }

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
    assert_eq!(
        pos.line, 3,
        "Byte 3 (EOF) should be on line 3, got line {}",
        pos.line
    );
}

#[test]
fn test_offset_to_position_long_lines() {
    // Test with long lines to ensure it's not just line counting
    let mut content = Vec::new();
    content.extend_from_slice(b"aaaaaaaaaa\n"); // Line 0: 11 bytes (10 'a's + newline)
    content.extend_from_slice(b"bbbbbbbbbb\n"); // Line 1: 11 bytes
    content.extend_from_slice(b"cccccccccc"); // Line 2: 10 bytes (no newline)

    let buffer = TextBuffer::from_bytes(content.clone());

    println!("\n=== Buffer with long lines ===");
    println!("Content length: {}", content.len());
    println!("Line count: {:?}", buffer.line_count());

    // Test positions at start of each line
    let pos = buffer
        .offset_to_position(0)
        .expect("small buffer should have line metadata");
    assert_eq!(pos.line, 0, "Byte 0 should be on line 0");
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(11)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 1,
        "Byte 11 (start of line 1) should be on line 1, got line {}",
        pos.line
    );
    assert_eq!(pos.column, 0);

    let pos = buffer
        .offset_to_position(22)
        .expect("small buffer should have line metadata");
    assert_eq!(
        pos.line, 2,
        "Byte 22 (start of line 2) should be on line 2, got line {}",
        pos.line
    );
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
    let buffer = TextBuffer::from_bytes(b"line0\nline1\nline2\n".to_vec());

    println!("\n=== Testing LineIterator initialization ===");
    println!(
        "Content: {:?}",
        String::from_utf8_lossy(&buffer.slice_bytes(0..buffer.len()))
    );

    // Test creating line iterator at various positions
    for byte_pos in 0..=buffer.len() {
        let iter = buffer.line_iterator(byte_pos);
        let iter_pos = iter.current_position();
        let expected_line = buffer
            .offset_to_position(byte_pos)
            .expect("small buffer should have line metadata")
            .line;
        let expected_line_start = buffer.position_to_offset(fresh::piece_tree::Position {
            line: expected_line,
            column: 0,
        });

        println!(
            "byte_pos={}, expected_line={}, expected_line_start={}, iter.current_position()={}",
            byte_pos, expected_line, expected_line_start, iter_pos
        );

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
    let mut buffer = TextBuffer::from_bytes(b"a\nb\n".to_vec());

    println!("\n=== Piece tree structure BEFORE insert ===");
    print_piece_tree_debug(&buffer);

    // Insert at line 1, column 0
    buffer.insert_at_position(
        fresh::piece_tree::Position { line: 1, column: 0 },
        b"x\n".to_vec(),
    );

    println!("\n=== Piece tree structure AFTER insert ===");
    print_piece_tree_debug(&buffer);

    // Manually verify line counts
    println!("\n=== Manual line verification ===");
    let content = buffer.slice_bytes(0..buffer.len());
    let newline_count = content.iter().filter(|&&b| b == b'\n').count();
    let expected_line_count = newline_count + 1;
    let actual_line_count = buffer.line_count();

    println!("Content: {:?}", String::from_utf8_lossy(&content));
    println!("Newline count: {}", newline_count);
    println!("Expected line count: {}", expected_line_count);
    println!("Actual line count: {:?}", actual_line_count);

    assert_eq!(
        actual_line_count,
        Some(expected_line_count),
        "Line count mismatch after insert"
    );
}

fn print_piece_tree_debug(buffer: &TextBuffer) {
    println!("Total bytes: {}", buffer.len());
    println!("Line count: {:?}", buffer.line_count());

    // Try to get piece tree stats if available
    // Note: This might require adding a debug method to TextBuffer
}
