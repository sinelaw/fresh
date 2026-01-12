/// Comprehensive tests for LineIterator to catch position/content bugs
use fresh::model::buffer::TextBuffer;

#[test]
fn test_line_iterator_simple() {
    let mut buffer = TextBuffer::from_bytes(b"Line 1\nLine 2\nLine 3\n".to_vec());

    // Test starting at beginning
    let mut iter = buffer.line_iterator(0, 80);
    assert_eq!(iter.current_position(), 0);

    let (pos, content) = iter.next_line().unwrap();
    assert_eq!(pos, 0);
    assert_eq!(content, "Line 1\n");

    let (pos, content) = iter.next_line().unwrap();
    assert_eq!(pos, 7);
    assert_eq!(content, "Line 2\n");

    let (pos, content) = iter.next_line().unwrap();
    assert_eq!(pos, 14);
    assert_eq!(content, "Line 3\n");
}

#[test]
fn test_line_iterator_empty_lines() {
    let mut buffer = TextBuffer::from_bytes(b"Line 1\n\nLine 3\n".to_vec());

    // Test starting at position 0
    let mut iter = buffer.line_iterator(0, 80);
    let (pos, content) = iter.next_line().unwrap();
    assert_eq!((pos, content.as_str()), (0, "Line 1\n"));

    let (pos, content) = iter.next_line().unwrap();
    assert_eq!(
        (pos, content.as_str()),
        (7, "\n"),
        "Empty line should be just newline"
    );

    let (pos, content) = iter.next_line().unwrap();
    assert_eq!((pos, content.as_str()), (8, "Line 3\n"));

    // Test starting at position 7 (empty line)
    let mut iter = buffer.line_iterator(7, 80);
    assert_eq!(iter.current_position(), 7);

    let (pos, content) = iter.next_line().unwrap();
    assert_eq!(
        (pos, content.as_str()),
        (7, "\n"),
        "Should return empty line, not previous content"
    );

    let (pos, content) = iter.next_line().unwrap();
    assert_eq!((pos, content.as_str()), (8, "Line 3\n"));
}

#[test]
fn test_line_iterator_multiple_empty_lines() {
    let mut buffer = TextBuffer::from_bytes(b"Line 1\n\n\n\nLine 5\n".to_vec());

    let mut iter = buffer.line_iterator(0, 80);
    assert_eq!(iter.next_line().unwrap().0, 0); // "Line 1\n"
    assert_eq!(iter.next_line().unwrap(), (7, "\n".to_string())); // Empty line 2
    assert_eq!(iter.next_line().unwrap(), (8, "\n".to_string())); // Empty line 3
    assert_eq!(iter.next_line().unwrap(), (9, "\n".to_string())); // Empty line 4
    assert_eq!(iter.next_line().unwrap().0, 10); // "Line 5\n"
}

#[test]
fn test_line_iterator_starts_mid_piece() {
    // This test creates a buffer with edits that cause pieces to span multiple lines
    let mut buffer = TextBuffer::from_bytes(b"Line 1\nLine 2\nLine 3\n".to_vec());

    // Insert at beginning to create a new piece
    buffer.insert_bytes(0, b"Prefix\n".to_vec());
    // Content is now "Prefix\nLine 1\nLine 2\nLine 3\n"

    // Test iterating from within a piece
    let mut iter = buffer.line_iterator(7, 80); // Start at "Line 1"
    let (pos, content) = iter.next_line().unwrap();
    assert_eq!((pos, content.as_str()), (7, "Line 1\n"));

    let (pos, content) = iter.next_line().unwrap();
    assert_eq!((pos, content.as_str()), (14, "Line 2\n"));
}

#[test]
fn test_line_iterator_after_multiple_edits() {
    let mut buffer = TextBuffer::from_bytes(b"ABC\n".to_vec());

    // Create multiple pieces through edits
    buffer.insert_bytes(4, b"DEF\n".to_vec()); // "ABC\nDEF\n"
    buffer.insert_bytes(8, b"GHI\n".to_vec()); // "ABC\nDEF\nGHI\n"

    // Test iteration from each line
    let mut iter = buffer.line_iterator(0, 80);
    assert_eq!(iter.next_line().unwrap(), (0, "ABC\n".to_string()));
    assert_eq!(iter.next_line().unwrap(), (4, "DEF\n".to_string()));
    assert_eq!(iter.next_line().unwrap(), (8, "GHI\n".to_string()));

    // Test starting mid-buffer
    let mut iter = buffer.line_iterator(4, 80);
    assert_eq!(iter.current_position(), 4);
    assert_eq!(iter.next_line().unwrap(), (4, "DEF\n".to_string()));
    assert_eq!(iter.next_line().unwrap(), (8, "GHI\n".to_string()));
}

#[test]
fn test_line_iterator_prev() {
    let mut buffer = TextBuffer::from_bytes(b"Line 1\n\nLine 3\n".to_vec());

    let mut iter = buffer.line_iterator(8, 80); // Start at "Line 3"
    assert_eq!(iter.current_position(), 8);

    let (pos, content) = iter.prev().unwrap();
    assert_eq!((pos, content.as_str()), (7, "\n")); // Empty line

    let (pos, content) = iter.prev().unwrap();
    assert_eq!((pos, content.as_str()), (0, "Line 1\n"));

    assert!(iter.prev().is_none()); // No more lines before
}

#[test]
fn test_line_iterator_no_trailing_newline() {
    let mut buffer = TextBuffer::from_bytes(b"Line 1\nLine 2".to_vec());

    let mut iter = buffer.line_iterator(0, 80);
    assert_eq!(iter.next_line().unwrap(), (0, "Line 1\n".to_string()));
    assert_eq!(iter.next_line().unwrap(), (7, "Line 2".to_string())); // No trailing newline

    assert!(iter.next_line().is_none());
}

#[test]
fn test_line_iterator_single_char_lines() {
    let mut buffer = TextBuffer::from_bytes(b"a\nb\nc\n".to_vec());

    let mut iter = buffer.line_iterator(0, 80);
    assert_eq!(iter.next_line().unwrap(), (0, "a\n".to_string()));
    assert_eq!(iter.next_line().unwrap(), (2, "b\n".to_string()));
    assert_eq!(iter.next_line().unwrap(), (4, "c\n".to_string()));
}
