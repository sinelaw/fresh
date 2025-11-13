/// Text buffer that combines PieceTree and LineIndex
/// VSCode-style architecture: separate concerns for byte indexing and line tracking

use crate::line_index::{LineIndex, Position};
use crate::piece_tree::{BufferLocation, Cursor, PieceInfo, PieceTree};

/// A text buffer that manages document content using a piece table
/// and maintains line information separately
pub struct TextBuffer {
    /// The piece tree for efficient text manipulation (tracks bytes only)
    piece_tree: PieceTree,

    /// Line index for mapping between line/column and byte offsets
    line_index: LineIndex,

    /// The original/stored buffer (read-only, from file)
    stored_buffer: Vec<u8>,

    /// The added buffer (in-memory modifications)
    added_buffer: Vec<u8>,
}

impl TextBuffer {
    /// Create a new text buffer from initial content
    pub fn new(content: Vec<u8>) -> Self {
        let bytes = content.len();
        let line_index = LineIndex::build_from_buffer(&content);

        TextBuffer {
            piece_tree: if bytes > 0 {
                PieceTree::new(BufferLocation::Stored, 0, bytes)
            } else {
                PieceTree::empty()
            },
            line_index,
            stored_buffer: content,
            added_buffer: Vec::new(),
        }
    }

    /// Create an empty text buffer
    pub fn empty() -> Self {
        TextBuffer {
            piece_tree: PieceTree::empty(),
            line_index: LineIndex::new(),
            stored_buffer: Vec::new(),
            added_buffer: Vec::new(),
        }
    }

    /// Get the total number of bytes in the document
    pub fn total_bytes(&self) -> usize {
        self.piece_tree.total_bytes()
    }

    /// Get the total number of lines in the document
    pub fn line_count(&self) -> usize {
        self.line_index.line_count()
    }

    /// Convert a byte offset to a line/column position
    pub fn offset_to_position(&self, offset: usize) -> Position {
        self.line_index.offset_to_position(offset)
    }

    /// Convert a line/column position to a byte offset
    pub fn position_to_offset(&self, position: Position) -> usize {
        self.line_index.position_to_offset(position)
    }

    /// Insert text at the given byte offset
    pub fn insert_bytes(&mut self, offset: usize, text: Vec<u8>) -> Cursor {
        if text.is_empty() {
            return self.piece_tree.cursor_at_offset(offset);
        }

        // Add text to the added buffer
        let buffer_offset = self.added_buffer.len();
        self.added_buffer.extend_from_slice(&text);

        // Update piece tree
        let cursor = self.piece_tree.insert(
            offset,
            BufferLocation::Added,
            buffer_offset,
            text.len(),
        );

        // Update line index
        self.line_index.insert(offset, &text);

        cursor
    }

    /// Insert text at a line/column position
    pub fn insert_at_position(&mut self, position: Position, text: Vec<u8>) -> Cursor {
        let offset = self.position_to_offset(position);
        self.insert_bytes(offset, text)
    }

    /// Delete text starting at the given byte offset
    pub fn delete_bytes(&mut self, offset: usize, bytes: usize) {
        if bytes == 0 || offset >= self.total_bytes() {
            return;
        }

        // Get the text that will be deleted (needed for line index update)
        let deleted_text = self.get_text_range(offset, bytes);

        // Update piece tree
        self.piece_tree.delete(offset, bytes);

        // Update line index
        self.line_index.delete(offset, bytes, &deleted_text);
    }

    /// Delete text in a line/column range
    pub fn delete_range(&mut self, start: Position, end: Position) {
        let start_offset = self.position_to_offset(start);
        let end_offset = self.position_to_offset(end);

        if end_offset > start_offset {
            self.delete_bytes(start_offset, end_offset - start_offset);
        }
    }

    /// Get text from a byte offset range
    pub fn get_text_range(&self, offset: usize, bytes: usize) -> Vec<u8> {
        let mut result = Vec::with_capacity(bytes);
        let mut remaining = bytes;
        let mut current_offset = offset;

        while remaining > 0 {
            if let Some(piece_info) = self.piece_tree.find_by_offset(current_offset) {
                // Get the buffer for this piece
                let buffer = match piece_info.location {
                    BufferLocation::Stored => &self.stored_buffer,
                    BufferLocation::Added => &self.added_buffer,
                };

                // Calculate how much to read from this piece
                let start_in_piece = piece_info.offset_in_piece.unwrap_or(0);
                let available_in_piece = piece_info.bytes - start_in_piece;
                let to_read = remaining.min(available_in_piece);

                // Read from buffer
                let buffer_start = piece_info.offset + start_in_piece;
                let buffer_end = buffer_start + to_read;

                if buffer_end <= buffer.len() {
                    result.extend_from_slice(&buffer[buffer_start..buffer_end]);
                } else {
                    // Shouldn't happen, but handle gracefully
                    break;
                }

                remaining -= to_read;
                current_offset += to_read;
            } else {
                break;
            }
        }

        result
    }

    /// Get all text as a single Vec<u8>
    pub fn get_all_text(&self) -> Vec<u8> {
        self.get_text_range(0, self.total_bytes())
    }

    /// Get all text as a String
    pub fn get_all_text_string(&self) -> String {
        String::from_utf8_lossy(&self.get_all_text()).into_owned()
    }

    /// Get text for a specific line
    pub fn get_line(&self, line: usize) -> Option<Vec<u8>> {
        let (start, end) = self.line_index.line_range(line)?;

        let bytes = if let Some(end_offset) = end {
            end_offset - start
        } else {
            self.total_bytes() - start
        };

        Some(self.get_text_range(start, bytes))
    }

    /// Get the byte offset where a line starts
    pub fn line_start_offset(&self, line: usize) -> Option<usize> {
        self.line_index.line_start_offset(line)
    }

    /// Get piece information at a byte offset
    pub fn piece_info_at_offset(&self, offset: usize) -> Option<PieceInfo> {
        self.piece_tree.find_by_offset(offset)
    }

    /// Get tree statistics for debugging (total_bytes, depth, leaf_count)
    pub fn stats(&self) -> (usize, usize, usize) {
        self.piece_tree.stats()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_buffer() {
        let buffer = TextBuffer::empty();
        assert_eq!(buffer.total_bytes(), 0);
        assert_eq!(buffer.line_count(), 1); // Empty doc has 1 line
    }

    #[test]
    fn test_new_from_content() {
        let buffer = TextBuffer::new(b"hello\nworld".to_vec());
        assert_eq!(buffer.total_bytes(), 11);
        assert_eq!(buffer.line_count(), 2);
    }

    #[test]
    fn test_get_all_text() {
        let buffer = TextBuffer::new(b"hello\nworld".to_vec());
        assert_eq!(buffer.get_all_text(), b"hello\nworld");
    }

    #[test]
    fn test_insert_at_start() {
        let mut buffer = TextBuffer::new(b"world".to_vec());
        buffer.insert_bytes(0, b"hello ".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello world");
        assert_eq!(buffer.total_bytes(), 11);
    }

    #[test]
    fn test_insert_in_middle() {
        let mut buffer = TextBuffer::new(b"helloworld".to_vec());
        buffer.insert_bytes(5, b" ".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello world");
        assert_eq!(buffer.total_bytes(), 11);
    }

    #[test]
    fn test_insert_at_end() {
        let mut buffer = TextBuffer::new(b"hello".to_vec());
        buffer.insert_bytes(5, b" world".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello world");
        assert_eq!(buffer.total_bytes(), 11);
    }

    #[test]
    fn test_insert_with_newlines() {
        let mut buffer = TextBuffer::new(b"hello".to_vec());
        buffer.insert_bytes(5, b"\nworld\ntest".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello\nworld\ntest");
        assert_eq!(buffer.line_count(), 3);
    }

    #[test]
    fn test_delete_from_start() {
        let mut buffer = TextBuffer::new(b"hello world".to_vec());
        buffer.delete_bytes(0, 6);

        assert_eq!(buffer.get_all_text(), b"world");
        assert_eq!(buffer.total_bytes(), 5);
    }

    #[test]
    fn test_delete_from_middle() {
        let mut buffer = TextBuffer::new(b"hello world".to_vec());
        buffer.delete_bytes(5, 1);

        assert_eq!(buffer.get_all_text(), b"helloworld");
        assert_eq!(buffer.total_bytes(), 10);
    }

    #[test]
    fn test_delete_from_end() {
        let mut buffer = TextBuffer::new(b"hello world".to_vec());
        buffer.delete_bytes(6, 5);

        assert_eq!(buffer.get_all_text(), b"hello ");
        assert_eq!(buffer.total_bytes(), 6);
    }

    #[test]
    fn test_delete_with_newlines() {
        let mut buffer = TextBuffer::new(b"hello\nworld\ntest".to_vec());
        buffer.delete_bytes(5, 7); // Delete "\nworld\n"

        assert_eq!(buffer.get_all_text(), b"hellotest");
        assert_eq!(buffer.line_count(), 1);
    }

    #[test]
    fn test_offset_position_conversions() {
        let buffer = TextBuffer::new(b"hello\nworld\ntest".to_vec());

        let pos = buffer.offset_to_position(0);
        assert_eq!(pos, Position { line: 0, column: 0 });

        let pos = buffer.offset_to_position(6);
        assert_eq!(pos, Position { line: 1, column: 0 });

        let offset = buffer.position_to_offset(Position { line: 1, column: 0 });
        assert_eq!(offset, 6);
    }

    #[test]
    fn test_insert_at_position() {
        let mut buffer = TextBuffer::new(b"hello\nworld".to_vec());
        buffer.insert_at_position(Position { line: 1, column: 0 }, b"beautiful ".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello\nbeautiful world");
    }

    #[test]
    fn test_delete_range() {
        let mut buffer = TextBuffer::new(b"hello\nworld\ntest".to_vec());

        let start = Position { line: 0, column: 5 };
        let end = Position { line: 2, column: 0 };
        buffer.delete_range(start, end);

        assert_eq!(buffer.get_all_text(), b"hellotest");
    }

    #[test]
    fn test_get_line() {
        let buffer = TextBuffer::new(b"hello\nworld\ntest".to_vec());

        assert_eq!(buffer.get_line(0), Some(b"hello\n".to_vec()));
        assert_eq!(buffer.get_line(1), Some(b"world\n".to_vec()));
        assert_eq!(buffer.get_line(2), Some(b"test".to_vec()));
        assert_eq!(buffer.get_line(3), None);
    }

    #[test]
    fn test_multiple_operations() {
        let mut buffer = TextBuffer::new(b"line1\nline2\nline3".to_vec());

        buffer.insert_bytes(0, b"start\n".to_vec());
        assert_eq!(buffer.line_count(), 4);

        buffer.delete_bytes(6, 6); // Delete "line1\n"
        assert_eq!(buffer.line_count(), 3);

        buffer.insert_bytes(6, b"new\n".to_vec());
        assert_eq!(buffer.line_count(), 4);

        let text = buffer.get_all_text();
        assert_eq!(text, b"start\nnew\nline2\nline3");
    }

    #[test]
    fn test_get_text_range() {
        let buffer = TextBuffer::new(b"hello world".to_vec());

        assert_eq!(buffer.get_text_range(0, 5), b"hello");
        assert_eq!(buffer.get_text_range(6, 5), b"world");
        assert_eq!(buffer.get_text_range(0, 11), b"hello world");
    }

    #[test]
    fn test_empty_operations() {
        let mut buffer = TextBuffer::new(b"hello".to_vec());

        buffer.insert_bytes(2, Vec::new());
        assert_eq!(buffer.get_all_text(), b"hello");

        buffer.delete_bytes(2, 0);
        assert_eq!(buffer.get_all_text(), b"hello");
    }
}

#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;

    // Generate text with some newlines
    fn text_with_newlines() -> impl Strategy<Value = Vec<u8>> {
        prop::collection::vec(
            prop_oneof![
                (b'a'..=b'z').prop_map(|c| c),
                Just(b'\n'),
            ],
            0..100,
        )
    }

    // Strategy to generate operations
    #[derive(Debug, Clone)]
    enum Operation {
        Insert { offset: usize, text: Vec<u8> },
        Delete { offset: usize, bytes: usize },
    }

    fn operation_strategy() -> impl Strategy<Value = Vec<Operation>> {
        prop::collection::vec(
            prop_oneof![
                (0usize..200, text_with_newlines()).prop_map(|(offset, text)| {
                    Operation::Insert { offset, text }
                }),
                (0usize..200, 1usize..50).prop_map(|(offset, bytes)| {
                    Operation::Delete { offset, bytes }
                }),
            ],
            0..50,
        )
    }

    proptest! {
        #[test]
        fn prop_line_count_consistent(text in text_with_newlines()) {
            let buffer = TextBuffer::new(text.clone());

            let newline_count = text.iter().filter(|&&b| b == b'\n').count();
            prop_assert_eq!(buffer.line_count(), newline_count + 1);
        }

        #[test]
        fn prop_get_all_text_matches_original(text in text_with_newlines()) {
            let buffer = TextBuffer::new(text.clone());
            prop_assert_eq!(buffer.get_all_text(), text);
        }

        #[test]
        fn prop_insert_increases_size(
            text in text_with_newlines(),
            offset in 0usize..100,
            insert_text in text_with_newlines()
        ) {
            let mut buffer = TextBuffer::new(text);
            let initial_bytes = buffer.total_bytes();

            let offset = offset.min(buffer.total_bytes());
            buffer.insert_bytes(offset, insert_text.clone());

            prop_assert_eq!(buffer.total_bytes(), initial_bytes + insert_text.len());
        }

        #[test]
        fn prop_delete_decreases_size(
            text in text_with_newlines(),
            offset in 0usize..100,
            delete_bytes in 1usize..50
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let mut buffer = TextBuffer::new(text);
            let initial_bytes = buffer.total_bytes();

            let offset = offset.min(buffer.total_bytes());
            let delete_bytes = delete_bytes.min(buffer.total_bytes() - offset);

            if delete_bytes == 0 {
                return Ok(());
            }

            buffer.delete_bytes(offset, delete_bytes);

            prop_assert_eq!(buffer.total_bytes(), initial_bytes - delete_bytes);
        }

        #[test]
        fn prop_insert_then_delete_restores_original(
            text in text_with_newlines(),
            offset in 0usize..100,
            insert_text in text_with_newlines()
        ) {
            let mut buffer = TextBuffer::new(text.clone());

            let offset = offset.min(buffer.total_bytes());
            buffer.insert_bytes(offset, insert_text.clone());
            buffer.delete_bytes(offset, insert_text.len());

            prop_assert_eq!(buffer.get_all_text(), text);
        }

        #[test]
        fn prop_offset_position_roundtrip(text in text_with_newlines()) {
            let buffer = TextBuffer::new(text.clone());

            for offset in 0..text.len() {
                let pos = buffer.offset_to_position(offset);
                let back = buffer.position_to_offset(pos);
                prop_assert_eq!(back, offset, "Failed roundtrip for offset {}", offset);
            }
        }

        #[test]
        fn prop_get_text_range_valid(
            text in text_with_newlines(),
            offset in 0usize..100,
            length in 1usize..50
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let buffer = TextBuffer::new(text.clone());
            let offset = offset.min(buffer.total_bytes());
            let length = length.min(buffer.total_bytes() - offset);

            if length == 0 {
                return Ok(());
            }

            let result = buffer.get_text_range(offset, length);
            prop_assert_eq!(result, text[offset..offset + length].to_vec());
        }

        #[test]
        fn prop_operations_maintain_consistency(operations in operation_strategy()) {
            let mut buffer = TextBuffer::new(b"initial\ntext".to_vec());
            let mut expected_text = b"initial\ntext".to_vec();

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text.clone());

                        // Update expected
                        let offset = offset.min(expected_text.len());
                        expected_text.splice(offset..offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        if offset < buffer.total_bytes() {
                            let bytes = bytes.min(buffer.total_bytes() - offset);
                            buffer.delete_bytes(offset, bytes);

                            // Update expected
                            if offset < expected_text.len() {
                                let bytes = bytes.min(expected_text.len() - offset);
                                expected_text.drain(offset..offset + bytes);
                            }
                        }
                    }
                }
            }

            prop_assert_eq!(buffer.get_all_text(), expected_text);
        }

        #[test]
        fn prop_line_count_never_zero(operations in operation_strategy()) {
            let mut buffer = TextBuffer::new(b"test".to_vec());

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        buffer.delete_bytes(offset, bytes);
                    }
                }

                // Document always has at least 1 line
                prop_assert!(buffer.line_count() >= 1);
            }
        }

        #[test]
        fn prop_total_bytes_never_negative(operations in operation_strategy()) {
            let mut buffer = TextBuffer::new(b"test".to_vec());

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        buffer.delete_bytes(offset, bytes);
                    }
                }

                // Bytes should never overflow
                prop_assert!(buffer.total_bytes() < 10_000_000);
            }
        }

        #[test]
        fn prop_piece_tree_and_line_index_stay_synced(operations in operation_strategy()) {
            let mut buffer = TextBuffer::new(b"line1\nline2\nline3".to_vec());

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        buffer.delete_bytes(offset, bytes);
                    }
                }

                // Verify we can still convert between offsets and positions
                if buffer.total_bytes() > 0 {
                    let mid_offset = buffer.total_bytes() / 2;
                    let pos = buffer.offset_to_position(mid_offset);
                    let back = buffer.position_to_offset(pos);

                    // Should be able to roundtrip
                    prop_assert!(back <= buffer.total_bytes());
                }
            }
        }
    }
}
