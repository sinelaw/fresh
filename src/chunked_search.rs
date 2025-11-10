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
}
