//! Overlapping chunk iterator for efficient pattern matching on large buffers
//!
//! This module provides a VSCode-style "buffered iteration with overlap" approach
//! for searching through large text buffers without materializing the entire content.
//!
//! The iterator yields overlapping chunks of data, with a "valid zone" that ensures
//! patterns spanning chunk boundaries are found exactly once.

use crate::virtual_buffer::ByteIterator;

/// Information about a chunk of data for pattern matching
#[derive(Debug)]
pub struct ChunkInfo {
    /// The buffer containing this chunk's data (includes overlap from previous chunk)
    pub buffer: Vec<u8>,

    /// Absolute position in the file where this buffer starts
    pub absolute_pos: usize,

    /// Offset within buffer where "new" data starts (valid match zone)
    /// Matches starting before this offset were already checked in the previous chunk
    pub valid_start: usize,
}

/// Iterator that yields overlapping chunks for pattern matching
///
/// This iterator implements the VSCode/Sublime approach: pull overlapping chunks
/// from the underlying data structure and use standard search algorithms on them.
///
/// # Algorithm
///
/// ```text
/// Chunk 1: [------------ valid -----------]
/// Chunk 2:      [overlap][---- valid ----]
/// Chunk 3:                   [overlap][-- valid --]
///
/// Only matches starting in the "valid" zone are reported to avoid duplicates.
/// ```
///
/// # Example
///
/// ```ignore
/// let chunks = OverlappingChunks::new(iter, 0, 1000, 4096, 512);
/// for chunk in chunks {
///     // Search only starting from chunk.valid_start
///     if let Some(pos) = search(&chunk.buffer[chunk.valid_start..]) {
///         let absolute_pos = chunk.absolute_pos + chunk.valid_start + pos;
///         return Some(absolute_pos);
///     }
/// }
/// ```
pub struct OverlappingChunks {
    iter: ByteIterator,
    buffer: Vec<u8>,
    buffer_start_pos: usize,
    current_read_pos: usize,
    end: usize,
    chunk_size: usize,
    overlap: usize,
    first_chunk: bool,
}

impl OverlappingChunks {
    /// Create a new overlapping chunks iterator
    ///
    /// # Arguments
    ///
    /// * `iter` - ByteIterator positioned at the start position
    /// * `start` - Start position in the file
    /// * `end` - End position in the file (exclusive)
    /// * `chunk_size` - Target size for each chunk (excluding overlap)
    /// * `overlap` - Number of bytes to overlap between chunks
    ///
    /// # Recommendations
    ///
    /// * For literal string search: `chunk_size=4096, overlap=pattern.len()-1`
    /// * For regex search: `chunk_size=65536, overlap=4096` (or larger for complex patterns)
    pub fn new(
        iter: ByteIterator,
        start: usize,
        end: usize,
        chunk_size: usize,
        overlap: usize,
    ) -> Self {
        Self {
            iter,
            buffer: Vec::with_capacity(chunk_size + overlap),
            buffer_start_pos: start,
            current_read_pos: start,
            end,
            chunk_size,
            overlap,
            first_chunk: true,
        }
    }

    /// Fill the buffer with the next chunk of data
    fn fill_next_chunk(&mut self) -> bool {
        if self.first_chunk {
            // First chunk: fill up to chunk_size
            self.first_chunk = false;
            while self.buffer.len() < self.chunk_size && self.current_read_pos < self.end {
                if let Some(byte) = self.iter.next() {
                    self.buffer.push(byte);
                    self.current_read_pos += 1;
                } else {
                    break;
                }
            }
            !self.buffer.is_empty()
        } else {
            // Subsequent chunks: keep overlap, fill chunk_size NEW bytes
            if self.current_read_pos >= self.end {
                return false;
            }

            // Keep overlap bytes at the end
            if self.buffer.len() > self.overlap {
                let drain_amount = self.buffer.len() - self.overlap;
                self.buffer.drain(0..drain_amount);
                self.buffer_start_pos += drain_amount;
            }

            // Fill chunk_size NEW bytes (in addition to overlap)
            let before_len = self.buffer.len();
            let target_len = self.overlap + self.chunk_size;
            while self.buffer.len() < target_len && self.current_read_pos < self.end {
                if let Some(byte) = self.iter.next() {
                    self.buffer.push(byte);
                    self.current_read_pos += 1;
                } else {
                    break;
                }
            }

            // Return true if we added new data
            self.buffer.len() > before_len
        }
    }
}

impl Iterator for OverlappingChunks {
    type Item = ChunkInfo;

    fn next(&mut self) -> Option<Self::Item> {
        // Track if this is the first chunk before filling
        let is_first = self.buffer_start_pos == self.current_read_pos;

        if !self.fill_next_chunk() {
            return None;
        }

        // First chunk: all data is valid (no overlap from previous)
        // Subsequent chunks: overlap bytes are not valid (already checked)
        let valid_start = if is_first {
            0
        } else {
            self.overlap.min(self.buffer.len())
        };

        Some(ChunkInfo {
            buffer: self.buffer.clone(),
            absolute_pos: self.buffer_start_pos,
            valid_start,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk_tree::ChunkTreeConfig;
    use crate::persistence::ChunkTreePersistence;
    use crate::virtual_buffer::VirtualBuffer;
    use proptest::prelude::*;

    const DEFAULT_CONFIG: ChunkTreeConfig = ChunkTreeConfig::new(4096, 8);

    fn create_test_buffer(content: &str) -> VirtualBuffer {
        let leaked = Box::leak(content.as_bytes().to_vec().into_boxed_slice());
        let persistence = Box::new(ChunkTreePersistence::from_data(leaked, DEFAULT_CONFIG));
        VirtualBuffer::new(persistence)
    }

    #[test]
    fn test_single_chunk() {
        let vbuf = create_test_buffer("hello world");
        let iter = vbuf.iter_at(0);

        let mut chunks = OverlappingChunks::new(iter, 0, 11, 100, 5);

        let chunk = chunks.next().unwrap();
        assert_eq!(chunk.buffer, b"hello world");
        assert_eq!(chunk.absolute_pos, 0);
        assert_eq!(chunk.valid_start, 0);

        assert!(chunks.next().is_none());
    }

    #[test]
    fn test_overlapping_chunks() {
        let vbuf = create_test_buffer("0123456789abcdef");
        let iter = vbuf.iter_at(0);

        let mut chunks = OverlappingChunks::new(iter, 0, 16, 8, 3);

        // First chunk: 8 bytes
        let chunk1 = chunks.next().unwrap();
        assert_eq!(&chunk1.buffer[..], b"01234567");
        assert_eq!(chunk1.absolute_pos, 0);
        assert_eq!(chunk1.valid_start, 0);

        // Second chunk: 3 overlap + 8 new = 11 bytes "56789abcdef"
        let chunk2 = chunks.next().unwrap();
        assert_eq!(&chunk2.buffer[..], b"56789abcdef");
        assert_eq!(chunk2.absolute_pos, 5);
        assert_eq!(chunk2.valid_start, 3); // Overlap of 3

        assert!(chunks.next().is_none());
    }

    #[test]
    fn test_pattern_across_boundary() {
        // Test that a pattern spanning chunk boundary is found
        // Pattern "6789a" spans the boundary between chunk 1 and chunk 2
        let vbuf = create_test_buffer("0123456789abcdef");
        let pattern = b"6789a";

        let iter = vbuf.iter_at(0);
        // Overlap must be pattern.len() - 1 to catch patterns spanning boundaries
        let chunks = OverlappingChunks::new(iter, 0, 16, 8, pattern.len() - 1);

        let mut found = false;
        for chunk in chunks {
            // Search the entire buffer, but only accept matches that END in valid zone
            // This ensures patterns spanning boundaries are found exactly once
            if let Some(pos) = chunk
                .buffer
                .windows(pattern.len())
                .position(|window| window == pattern)
            {
                let match_end = pos + pattern.len();
                // Only report if match ENDS in or after the valid zone
                // This catches patterns that span the overlap/valid boundary
                if match_end > chunk.valid_start {
                    let absolute_pos = chunk.absolute_pos + pos;
                    assert_eq!(absolute_pos, 6);
                    found = true;
                    break;
                }
            }
        }

        assert!(found, "Pattern spanning boundary should be found");
    }

    #[test]
    fn test_empty_range() {
        let vbuf = create_test_buffer("hello");
        let iter = vbuf.iter_at(0);

        let mut chunks = OverlappingChunks::new(iter, 0, 0, 100, 5);
        assert!(chunks.next().is_none());
    }

    #[test]
    fn test_partial_final_chunk() {
        let vbuf = create_test_buffer("hello");
        let iter = vbuf.iter_at(0);

        let mut chunks = OverlappingChunks::new(iter, 0, 5, 100, 2);

        let chunk = chunks.next().unwrap();
        assert_eq!(&chunk.buffer[..], b"hello");
        assert!(chunks.next().is_none());
    }

    // ============================================================================
    // Property-based tests
    // ============================================================================

    proptest! {
        /// Property: Chunking should cover all bytes exactly once in valid zones
        /// This verifies that no data is lost or duplicated across chunks
        #[test]
        fn prop_chunks_cover_all_bytes(
            content in "[a-z]{100,500}",
            chunk_size in 10usize..100,
            overlap in 1usize..10,
        ) {
            let vbuf = create_test_buffer(&content);
            let iter = vbuf.iter_at(0);
            let chunks = OverlappingChunks::new(iter, 0, content.len(), chunk_size, overlap);

            let mut covered = vec![false; content.len()];

            for chunk in chunks {
                // Mark bytes in valid zone as covered
                let valid_end = chunk.buffer.len();
                for i in chunk.valid_start..valid_end {
                    let absolute_pos = chunk.absolute_pos + i;
                    if absolute_pos < content.len() {
                        prop_assert!(!covered[absolute_pos], "Byte {} covered twice", absolute_pos);
                        covered[absolute_pos] = true;
                    }
                }
            }

            // All bytes should be covered
            for (i, &was_covered) in covered.iter().enumerate() {
                prop_assert!(was_covered, "Byte {} was never covered", i);
            }
        }

        /// Property: Pattern found by chunked search matches naive search
        /// This ensures our streaming algorithm is correct
        #[test]
        fn prop_find_matches_naive(
            prefix in "[a-z]{0,100}",
            pattern in "[a-z]{3,10}",
            suffix in "[a-z]{0,100}",
            chunk_size in 5usize..50,
        ) {
            let content = format!("{}{}{}", prefix, pattern, suffix);
            let pattern_bytes = pattern.as_bytes();
            let overlap = pattern_bytes.len().saturating_sub(1);

            // Naive search
            let naive_pos = content.as_bytes()
                .windows(pattern_bytes.len())
                .position(|w| w == pattern_bytes);

            // Chunked search
            let vbuf = create_test_buffer(&content);
            let iter = vbuf.iter_at(0);
            let chunks = OverlappingChunks::new(iter, 0, content.len(), chunk_size, overlap);

            let mut chunked_pos = None;
            for chunk in chunks {
                if let Some(pos) = chunk.buffer
                    .windows(pattern_bytes.len())
                    .position(|w| w == pattern_bytes)
                {
                    let match_end = pos + pattern_bytes.len();
                    if match_end > chunk.valid_start {
                        chunked_pos = Some(chunk.absolute_pos + pos);
                        break;
                    }
                }
            }

            prop_assert_eq!(chunked_pos, naive_pos,
                "Chunked search should find same position as naive search");
        }

        /// Property: Pattern at chunk boundary is found exactly once
        /// This tests the critical "match_end > valid_start" logic
        #[test]
        fn prop_boundary_pattern_found_once(
            pattern in "[a-z]{5,15}",
            chunk_size in 10usize..50,
        ) {
            // Create content where pattern spans a chunk boundary
            // Place pattern such that it starts before and ends after boundary
            let boundary_offset = chunk_size - pattern.len() / 2;
            let prefix = "x".repeat(boundary_offset);
            let suffix = "y".repeat(50);
            let content = format!("{}{}{}", prefix, pattern, suffix);

            let pattern_bytes = pattern.as_bytes();
            let overlap = pattern_bytes.len().saturating_sub(1);

            let vbuf = create_test_buffer(&content);
            let iter = vbuf.iter_at(0);
            let chunks = OverlappingChunks::new(iter, 0, content.len(), chunk_size, overlap);

            let mut match_count = 0;
            for chunk in chunks {
                if let Some(pos) = chunk.buffer
                    .windows(pattern_bytes.len())
                    .position(|w| w == pattern_bytes)
                {
                    let match_end = pos + pattern_bytes.len();
                    if match_end > chunk.valid_start {
                        match_count += 1;
                    }
                }
            }

            prop_assert_eq!(match_count, 1,
                "Pattern spanning boundary should be found exactly once, not {} times",
                match_count);
        }

        /// Property: All occurrences of a pattern are found
        /// Tests that repeated patterns are all discovered
        #[test]
        fn prop_find_all_occurrences(
            pattern in "[a-z]{3,8}",
            separator in "[0-9]{2,5}",
            repetitions in 2usize..10,
            chunk_size in 10usize..30,
        ) {
            // Create content with multiple occurrences
            let parts: Vec<String> = (0..repetitions)
                .map(|_| pattern.clone())
                .collect();
            let content = parts.join(&separator);

            // Count with naive search
            let pattern_bytes = pattern.as_bytes();
            let naive_count = content.as_bytes()
                .windows(pattern_bytes.len())
                .filter(|w| w == &pattern_bytes)
                .count();

            // Count with chunked search
            let overlap = pattern_bytes.len().saturating_sub(1);
            let vbuf = create_test_buffer(&content);
            let iter = vbuf.iter_at(0);
            let chunks = OverlappingChunks::new(iter, 0, content.len(), chunk_size, overlap);

            let mut chunked_count = 0;
            for chunk in chunks {
                for pos in 0..chunk.buffer.len().saturating_sub(pattern_bytes.len() - 1) {
                    if chunk.buffer[pos..].starts_with(pattern_bytes) {
                        let match_end = pos + pattern_bytes.len();
                        if match_end > chunk.valid_start {
                            chunked_count += 1;
                        }
                    }
                }
            }

            prop_assert_eq!(chunked_count, naive_count,
                "Should find all {} occurrences, found {}", naive_count, chunked_count);
        }

        /// Property: Chunk sizes and overlaps work correctly for small buffers
        /// Edge case testing for buffers smaller than chunk size
        #[test]
        fn prop_small_buffer_handling(
            content in "[a-z]{1,20}",
            chunk_size in 50usize..100,
            overlap in 5usize..20,
        ) {
            let vbuf = create_test_buffer(&content);
            let iter = vbuf.iter_at(0);
            let mut chunks = OverlappingChunks::new(iter, 0, content.len(), chunk_size, overlap);

            // Should get exactly one chunk for small buffer
            let first_chunk = chunks.next();
            prop_assert!(first_chunk.is_some(), "Should have at least one chunk");

            let chunk = first_chunk.unwrap();
            prop_assert_eq!(&chunk.buffer[..], content.as_bytes(),
                "Small buffer should be returned in single chunk");
            prop_assert_eq!(chunk.valid_start, 0, "First chunk should have valid_start=0");
            prop_assert!(chunks.next().is_none(), "Should be only one chunk for small buffer");
        }
    }
}
