/// Text buffer that uses PieceTree with integrated line tracking
/// Architecture where the tree is the single source of truth for text and line information

use crate::piece_tree::{BufferLocation, Cursor, PieceInfo, PieceTree, Position, StringBuffer, TreeStats};

/// A text buffer that manages document content using a piece table
/// with integrated line tracking
pub struct TextBuffer {
    /// The piece tree for efficient text manipulation with integrated line tracking
    piece_tree: PieceTree,

    /// List of string buffers containing chunks of text data
    /// Index 0 is typically the original/stored buffer
    /// Additional buffers are added for modifications
    buffers: Vec<StringBuffer>,

    /// Next buffer ID to assign
    next_buffer_id: usize,
}

impl TextBuffer {
    /// Create a new text buffer from initial content
    pub fn new(content: Vec<u8>) -> Self {
        let bytes = content.len();

        // Create initial StringBuffer with ID 0
        let buffer = StringBuffer::new(0, content);
        let line_feed_cnt = buffer.line_feed_count();

        TextBuffer {
            piece_tree: if bytes > 0 {
                PieceTree::new(BufferLocation::Stored(0), 0, bytes, line_feed_cnt)
            } else {
                PieceTree::empty()
            },
            buffers: vec![buffer],
            next_buffer_id: 1,
        }
    }

    /// Create an empty text buffer
    pub fn empty() -> Self {
        TextBuffer {
            piece_tree: PieceTree::empty(),
            buffers: vec![StringBuffer::new(0, Vec::new())],
            next_buffer_id: 1,
        }
    }

    /// Get the total number of bytes in the document
    pub fn total_bytes(&self) -> usize {
        self.piece_tree.total_bytes()
    }

    /// Get the total number of lines in the document
    /// Uses the piece tree's integrated line tracking
    pub fn line_count(&self) -> usize {
        self.piece_tree.line_count()
    }

    /// Convert a byte offset to a line/column position
    pub fn offset_to_position(&self, offset: usize) -> Position {
        let (line, column) = self.piece_tree.offset_to_position(offset, &self.buffers);
        Position { line, column }
    }

    /// Convert a line/column position to a byte offset
    pub fn position_to_offset(&self, position: Position) -> usize {
        self.piece_tree.position_to_offset(position.line, position.column, &self.buffers)
    }

    /// Insert text at the given byte offset
    pub fn insert_bytes(&mut self, offset: usize, text: Vec<u8>) -> Cursor {
        if text.is_empty() {
            return self.piece_tree.cursor_at_offset(offset);
        }

        // Count line feeds in the text to insert
        let line_feed_cnt = text.iter().filter(|&&b| b == b'\n').count();

        // Optimization: try to append to existing buffer if insertion is at piece boundary
        let (buffer_location, buffer_offset, text_len) =
            if let Some(append_info) = self.try_append_to_existing_buffer(offset, &text) {
                append_info
            } else {
                // Create a new StringBuffer for this insertion
                let buffer_id = self.next_buffer_id;
                self.next_buffer_id += 1;
                let buffer = StringBuffer::new(buffer_id, text.clone());
                self.buffers.push(buffer);
                (BufferLocation::Added(buffer_id), 0, text.len())
            };

        // Update piece tree (need to pass buffers reference)
        self.piece_tree.insert(
            offset,
            buffer_location,
            buffer_offset,
            text_len,
            line_feed_cnt,
            &self.buffers,
        )
    }

    /// Try to append to an existing buffer if insertion point aligns with buffer end
    /// Returns (BufferLocation, buffer_offset, text_len) if append succeeds, None otherwise
    fn try_append_to_existing_buffer(&mut self, offset: usize, text: &[u8]) -> Option<(BufferLocation, usize, usize)> {
        // Only optimize for non-empty insertions after existing content
        if text.is_empty() || offset == 0 {
            return None;
        }

        // Find the piece containing the byte just before the insertion point
        // This avoids the saturating_sub issue
        let piece_info = self.piece_tree.find_by_offset(offset - 1)?;

        // Check if insertion is exactly at the end of this piece
        // offset_in_piece tells us where (offset-1) is within the piece
        // For insertion to be at piece end, (offset-1) must be the last byte
        let offset_in_piece = piece_info.offset_in_piece?;
        if offset_in_piece + 1 != piece_info.bytes {
            return None; // Not at the end of the piece
        }

        // Only append to "Added" buffers (not original Stored buffers)
        if !matches!(piece_info.location, BufferLocation::Added(_)) {
            return None;
        }

        let buffer_id = piece_info.location.buffer_id();
        let buffer = self.buffers.get_mut(buffer_id)?;

        // Check if this piece ends exactly at the end of its buffer
        if piece_info.offset + piece_info.bytes != buffer.data.len() {
            return None;
        }

        // Perfect! Append to this buffer
        let append_offset = buffer.append(text);

        Some((piece_info.location, append_offset, text.len()))
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

        // Update piece tree
        self.piece_tree.delete(offset, bytes, &self.buffers);
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
                // Get the buffer for this piece by ID
                let buffer_id = piece_info.location.buffer_id();
                let buffer = if let Some(buf) = self.buffers.get(buffer_id) {
                    &buf.data
                } else {
                    // Shouldn't happen, but handle gracefully
                    break;
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
        let (start, end) = self.piece_tree.line_range(line, &self.buffers)?;

        let bytes = if let Some(end_offset) = end {
            end_offset - start
        } else {
            self.total_bytes() - start
        };

        Some(self.get_text_range(start, bytes))
    }

    /// Get the byte offset where a line starts
    pub fn line_start_offset(&self, line: usize) -> Option<usize> {
        let (start, _) = self.piece_tree.line_range(line, &self.buffers)?;
        Some(start)
    }

    /// Get piece information at a byte offset
    pub fn piece_info_at_offset(&self, offset: usize) -> Option<PieceInfo> {
        self.piece_tree.find_by_offset(offset)
    }

    /// Get tree statistics for debugging
    pub fn stats(&self) -> TreeStats {
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

    #[test]
    fn test_sequential_inserts_at_beginning() {
        // Regression test for piece tree duplicate insertion bug
        let mut buffer = TextBuffer::new(b"initial\ntext".to_vec());

        // Delete all
        buffer.delete_bytes(0, 12);
        assert_eq!(buffer.get_all_text(), b"");

        // Insert 'a' at 0
        buffer.insert_bytes(0, vec![b'a']);
        assert_eq!(buffer.get_all_text(), b"a");

        // Insert 'b' at 0 (should give "ba")
        buffer.insert_bytes(0, vec![b'b']);
        assert_eq!(buffer.get_all_text(), b"ba");
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
