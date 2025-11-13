/// Line index for mapping between byte offsets and line/column positions
/// VSCode-style: separate from piece tree, tracks line start offsets

/// A position in the document (line and column)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,   // 0-indexed line number
    pub column: usize, // Byte offset within the line
}

/// Index that maps line numbers to byte offsets
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// Byte offset where each line starts
    /// line_starts[0] is always 0 (start of document)
    /// line_starts[i] is the byte offset of the start of line i
    line_starts: Vec<usize>,
}

impl LineIndex {
    /// Create a new empty line index
    pub fn new() -> Self {
        LineIndex {
            line_starts: vec![0], // Document starts at byte 0, line 0
        }
    }

    /// Build an index by scanning a buffer for newlines
    pub fn build_from_buffer(buffer: &[u8]) -> Self {
        let mut line_starts = vec![0];

        for (i, &byte) in buffer.iter().enumerate() {
            if byte == b'\n' {
                // Next line starts after the newline
                line_starts.push(i + 1);
            }
        }

        LineIndex { line_starts }
    }

    /// Get the number of lines in the document
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// Convert a byte offset to a line/column position
    pub fn offset_to_position(&self, offset: usize) -> Position {
        // Binary search to find the line containing this offset
        let line = match self.line_starts.binary_search(&offset) {
            Ok(exact) => exact, // Offset is exactly at the start of a line
            Err(insert_pos) => insert_pos.saturating_sub(1), // Offset is within a line
        };

        let line_start = self.line_starts[line];
        let column = offset.saturating_sub(line_start);

        Position { line, column }
    }

    /// Convert a line/column position to a byte offset
    pub fn position_to_offset(&self, position: Position) -> usize {
        let line = position.line.min(self.line_starts.len() - 1);
        let line_start = self.line_starts[line];
        line_start + position.column
    }

    /// Update the index after inserting text
    /// offset: byte offset where text was inserted
    /// text: the inserted text (needed to count newlines)
    pub fn insert(&mut self, offset: usize, text: &[u8]) {
        if text.is_empty() {
            return;
        }

        // Count newlines in the inserted text
        let newline_count = text.iter().filter(|&&b| b == b'\n').count();

        if newline_count == 0 {
            // No newlines inserted, just adjust offsets after insertion point
            for line_start in &mut self.line_starts {
                if *line_start > offset {
                    *line_start += text.len();
                }
            }
            return;
        }

        // Find the line containing the insertion point
        let insert_line = match self.line_starts.binary_search(&offset) {
            Ok(exact) => exact,
            Err(insert_pos) => insert_pos.saturating_sub(1),
        };

        // Collect positions of newlines in the inserted text
        let mut new_line_starts = Vec::new();
        let mut current_offset = offset;

        for &byte in text {
            current_offset += 1;
            if byte == b'\n' {
                new_line_starts.push(current_offset);
            }
        }

        // Update existing line starts after insertion point
        for line_start in &mut self.line_starts {
            if *line_start > offset {
                *line_start += text.len();
            }
        }

        // Insert new line starts
        let insert_pos = insert_line + 1;
        self.line_starts.splice(insert_pos..insert_pos, new_line_starts);
    }

    /// Update the index after deleting text
    /// offset: byte offset where deletion starts
    /// deleted_bytes: number of bytes deleted
    /// deleted_text: the deleted text (needed to count newlines that were removed)
    pub fn delete(&mut self, offset: usize, deleted_bytes: usize, deleted_text: &[u8]) {
        if deleted_bytes == 0 {
            return;
        }

        let end_offset = offset + deleted_bytes;

        // Count newlines in deleted text
        let deleted_newlines = deleted_text.iter().filter(|&&b| b == b'\n').count();

        if deleted_newlines == 0 {
            // No newlines deleted, just adjust offsets after deletion point
            for line_start in &mut self.line_starts {
                if *line_start > offset {
                    *line_start = line_start.saturating_sub(deleted_bytes);
                }
            }
            return;
        }

        // Find lines that were partially or fully deleted
        let start_line = match self.line_starts.binary_search(&offset) {
            Ok(exact) => exact,
            Err(insert_pos) => insert_pos.saturating_sub(1),
        };

        // Remove line starts that fall within the deleted range
        let mut lines_to_remove = Vec::new();
        for (i, &line_start) in self.line_starts.iter().enumerate().skip(start_line + 1) {
            if line_start > offset && line_start <= end_offset {
                lines_to_remove.push(i);
            } else if line_start > end_offset {
                break;
            }
        }

        // Remove lines in reverse order to maintain indices
        for &line_idx in lines_to_remove.iter().rev() {
            self.line_starts.remove(line_idx);
        }

        // Adjust remaining line starts after deletion
        for line_start in &mut self.line_starts {
            if *line_start > end_offset {
                *line_start = line_start.saturating_sub(deleted_bytes);
            }
        }
    }

    /// Get the byte offset where a line starts
    pub fn line_start_offset(&self, line: usize) -> Option<usize> {
        self.line_starts.get(line).copied()
    }

    /// Get the byte range for a line (start..end)
    /// The end is the start of the next line, or None if this is the last line
    pub fn line_range(&self, line: usize) -> Option<(usize, Option<usize>)> {
        let start = self.line_starts.get(line).copied()?;
        let end = self.line_starts.get(line + 1).copied();
        Some((start, end))
    }
}

impl Default for LineIndex {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_index() {
        let index = LineIndex::new();
        assert_eq!(index.line_count(), 1); // Empty document has 1 line (line 0)
        assert_eq!(index.line_start_offset(0), Some(0));
    }

    #[test]
    fn test_build_from_buffer() {
        let text = b"hello\nworld\ntest";
        let index = LineIndex::build_from_buffer(text);

        assert_eq!(index.line_count(), 3);
        assert_eq!(index.line_start_offset(0), Some(0));   // "hello\n"
        assert_eq!(index.line_start_offset(1), Some(6));   // "world\n"
        assert_eq!(index.line_start_offset(2), Some(12));  // "test"
    }

    #[test]
    fn test_offset_to_position() {
        let text = b"hello\nworld\ntest";
        let index = LineIndex::build_from_buffer(text);

        // "h" at offset 0
        assert_eq!(index.offset_to_position(0), Position { line: 0, column: 0 });

        // "o" at offset 4
        assert_eq!(index.offset_to_position(4), Position { line: 0, column: 4 });

        // "w" at offset 6 (start of line 1)
        assert_eq!(index.offset_to_position(6), Position { line: 1, column: 0 });

        // "d" at offset 10
        assert_eq!(index.offset_to_position(10), Position { line: 1, column: 4 });

        // "t" at offset 12 (start of line 2)
        assert_eq!(index.offset_to_position(12), Position { line: 2, column: 0 });
    }

    #[test]
    fn test_position_to_offset() {
        let text = b"hello\nworld\ntest";
        let index = LineIndex::build_from_buffer(text);

        assert_eq!(index.position_to_offset(Position { line: 0, column: 0 }), 0);
        assert_eq!(index.position_to_offset(Position { line: 0, column: 4 }), 4);
        assert_eq!(index.position_to_offset(Position { line: 1, column: 0 }), 6);
        assert_eq!(index.position_to_offset(Position { line: 1, column: 4 }), 10);
        assert_eq!(index.position_to_offset(Position { line: 2, column: 0 }), 12);
    }

    #[test]
    fn test_roundtrip() {
        let text = b"hello\nworld\ntest";
        let index = LineIndex::build_from_buffer(text);

        for offset in 0..text.len() {
            let pos = index.offset_to_position(offset);
            let back = index.position_to_offset(pos);
            assert_eq!(back, offset, "Failed roundtrip for offset {}", offset);
        }
    }

    #[test]
    fn test_insert_no_newlines() {
        let mut index = LineIndex::build_from_buffer(b"hello\nworld");

        // Insert "XXX" at offset 2 (within "hello")
        index.insert(2, b"XXX");

        assert_eq!(index.line_count(), 2);
        assert_eq!(index.line_start_offset(0), Some(0));  // "helXXXlo\n"
        assert_eq!(index.line_start_offset(1), Some(9));  // "world" (was 6, now 6+3=9)
    }

    #[test]
    fn test_insert_with_newlines() {
        let mut index = LineIndex::build_from_buffer(b"hello\nworld");

        // Insert "foo\nbar\n" at offset 6 (start of "world")
        index.insert(6, b"foo\nbar\n");

        assert_eq!(index.line_count(), 4);
        assert_eq!(index.line_start_offset(0), Some(0));   // "hello\n"
        assert_eq!(index.line_start_offset(1), Some(6));   // "foo\n"
        assert_eq!(index.line_start_offset(2), Some(10));  // "bar\n"
        assert_eq!(index.line_start_offset(3), Some(14));  // "world"
    }

    #[test]
    fn test_delete_no_newlines() {
        let mut index = LineIndex::build_from_buffer(b"hello\nworld");

        // Delete "ll" at offset 2-4
        index.delete(2, 2, b"ll");

        assert_eq!(index.line_count(), 2);
        assert_eq!(index.line_start_offset(0), Some(0));  // "heo\n"
        assert_eq!(index.line_start_offset(1), Some(4));  // "world" (was 6, now 6-2=4)
    }

    #[test]
    fn test_delete_with_newlines() {
        let mut index = LineIndex::build_from_buffer(b"hello\nworld\ntest");

        // Delete "world\n" at offset 6-12
        index.delete(6, 6, b"world\n");

        assert_eq!(index.line_count(), 2);
        assert_eq!(index.line_start_offset(0), Some(0));  // "hello\n"
        assert_eq!(index.line_start_offset(1), Some(6));  // "test"
    }

    #[test]
    fn test_line_range() {
        let text = b"hello\nworld\ntest";
        let index = LineIndex::build_from_buffer(text);

        assert_eq!(index.line_range(0), Some((0, Some(6))));
        assert_eq!(index.line_range(1), Some((6, Some(12))));
        assert_eq!(index.line_range(2), Some((12, None))); // Last line has no end
    }

    #[test]
    fn test_insert_at_end() {
        let mut index = LineIndex::build_from_buffer(b"hello");
        index.insert(5, b"\nworld");

        assert_eq!(index.line_count(), 2);
        assert_eq!(index.line_start_offset(0), Some(0));
        assert_eq!(index.line_start_offset(1), Some(6));
    }

    #[test]
    fn test_multiple_operations() {
        let mut index = LineIndex::build_from_buffer(b"line1\nline2\nline3");

        // Insert at start
        index.insert(0, b"start\n");
        assert_eq!(index.line_count(), 4);

        // Delete a line
        index.delete(6, 6, b"line1\n");
        assert_eq!(index.line_count(), 3);

        // Insert in middle
        index.insert(6, b"new\n");
        assert_eq!(index.line_count(), 4);
    }
}

#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;

    // Helper to count newlines in a buffer
    fn count_newlines(buffer: &[u8]) -> usize {
        buffer.iter().filter(|&&b| b == b'\n').count()
    }

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

    proptest! {
        #[test]
        fn prop_line_count_matches_newlines(text in text_with_newlines()) {
            let index = LineIndex::build_from_buffer(&text);
            let newline_count = count_newlines(&text);

            // Line count should be newline count + 1 (document always has at least one line)
            prop_assert_eq!(index.line_count(), newline_count + 1);
        }

        #[test]
        fn prop_offset_position_roundtrip(text in text_with_newlines()) {
            let index = LineIndex::build_from_buffer(&text);

            for offset in 0..text.len() {
                let pos = index.offset_to_position(offset);
                let back = index.position_to_offset(pos);
                prop_assert_eq!(back, offset, "Failed roundtrip for offset {}", offset);
            }
        }

        #[test]
        fn prop_insert_preserves_structure(
            text in text_with_newlines(),
            insert_offset in 0usize..100,
            insert_text in text_with_newlines()
        ) {
            let mut index = LineIndex::build_from_buffer(&text);
            let initial_lines = index.line_count();

            let insert_offset = insert_offset.min(text.len());
            index.insert(insert_offset, &insert_text);

            let inserted_newlines = count_newlines(&insert_text);
            prop_assert_eq!(
                index.line_count(),
                initial_lines + inserted_newlines,
                "Line count should increase by number of newlines inserted"
            );
        }

        #[test]
        fn prop_delete_preserves_structure(
            text in text_with_newlines(),
            delete_offset in 0usize..100,
            delete_bytes in 1usize..50
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let mut index = LineIndex::build_from_buffer(&text);
            let initial_lines = index.line_count();

            let delete_offset = delete_offset.min(text.len());
            let delete_bytes = delete_bytes.min(text.len() - delete_offset);

            if delete_bytes == 0 {
                return Ok(());
            }

            let deleted_text = &text[delete_offset..delete_offset + delete_bytes];
            let deleted_newlines = count_newlines(deleted_text);

            index.delete(delete_offset, delete_bytes, deleted_text);

            prop_assert_eq!(
                index.line_count(),
                initial_lines - deleted_newlines,
                "Line count should decrease by number of newlines deleted"
            );
        }

        #[test]
        fn prop_insert_then_delete_restores_original(
            text in text_with_newlines(),
            offset in 0usize..100,
            insert_text in text_with_newlines()
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let mut index = LineIndex::build_from_buffer(&text);
            let original_lines = index.line_count();

            let offset = offset.min(text.len());
            index.insert(offset, &insert_text);
            index.delete(offset, insert_text.len(), &insert_text);

            prop_assert_eq!(index.line_count(), original_lines);
        }

        #[test]
        fn prop_all_line_starts_valid(text in text_with_newlines()) {
            let index = LineIndex::build_from_buffer(&text);

            for line in 0..index.line_count() {
                if let Some(offset) = index.line_start_offset(line) {
                    prop_assert!(offset <= text.len(), "Line start offset {} exceeds buffer length {}", offset, text.len());

                    if line > 0 {
                        // Check that the previous byte is a newline
                        if offset > 0 {
                            prop_assert_eq!(text[offset - 1], b'\n', "Line {} should start after a newline", line);
                        }
                    }
                }
            }
        }

        #[test]
        fn prop_position_column_never_negative(text in text_with_newlines()) {
            let index = LineIndex::build_from_buffer(&text);

            for offset in 0..text.len() {
                let pos = index.offset_to_position(offset);
                prop_assert!(pos.line < index.line_count(), "Line number out of bounds");

                // Column should be reasonable
                if let Some(line_start) = index.line_start_offset(pos.line) {
                    prop_assert!(offset >= line_start, "Offset should be >= line start");
                }
            }
        }

        #[test]
        fn prop_sequential_inserts_maintain_order(
            insert_count in 1usize..10,
            insert_text in text_with_newlines()
        ) {
            let mut index = LineIndex::new();
            let mut expected_lines = 1; // Start with 1 line

            for _ in 0..insert_count {
                let offset = index.line_start_offset(index.line_count() - 1).unwrap_or(0);
                index.insert(offset, &insert_text);
                expected_lines += count_newlines(&insert_text);
            }

            prop_assert_eq!(index.line_count(), expected_lines);
        }
    }
}
