/// Test offset/position conversions with leading newline
use fresh::text_buffer::TextBuffer;
use fresh::piece_tree::Position;

#[test]
fn test_newline_at_start() {
    let buffer = TextBuffer::from_bytes(b"\na".to_vec());

    println!("Content: {:?}", String::from_utf8_lossy(&buffer.slice_bytes(0..buffer.len())));
    println!("Length: {}", buffer.len());
    println!("Line count: {}", buffer.line_count());

    // Focus on the failing case
    let offset = 1;
    let pos = buffer.offset_to_position(offset);
    println!("\noffset {} -> ({}, {})", offset, pos.line, pos.column);

    let roundtrip = buffer.position_to_offset(Position {
        line: pos.line,
        column: pos.column,
    });
    println!("position_to_offset({}, {}) = {}", pos.line, pos.column, roundtrip);
    println!("Expected: {}", offset);

    assert_eq!(offset, roundtrip,
        "Roundtrip failed: offset {} -> ({}, {}) -> offset {}",
        offset, pos.line, pos.column, roundtrip);
}
