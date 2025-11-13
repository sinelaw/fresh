# Chunked Search Analysis and Redesign

## Problem Statement

The current search implementation in `text_buffer.rs` has several issues:

1. **find_pattern() inefficiency**: Uses overlapping chunks but materializes each chunk fully with `get_text_range()`, even though we now have efficient `iter_pieces_in_range()`
2. **find_regex() boundary bug**: Does NOT use overlapping chunks, so regex patterns spanning chunk boundaries are MISSED
3. **Memory overhead**: Materializes 64KB chunks for literal search and 1MB chunks for regex search

## Original Chunked Search Design

The deleted `src/chunked_search.rs` (commit 9eef4773) implemented a VSCode-style overlapping chunk iterator:

### Key Concepts

```text
Chunk 1: [------------ valid -----------]
Chunk 2:      [overlap][---- valid ----]
Chunk 3:                   [overlap][-- valid --]

Only matches starting in the "valid" zone are reported to avoid duplicates.
```

### Algorithm

1. **Overlapping chunks**: Each chunk includes some bytes from the previous chunk
2. **Valid zone**: Only report matches that END after `valid_start` offset
3. **Overlap size**: For literal search: `pattern.len() - 1`; For regex: larger (e.g., 4KB)

### Critical Logic for Boundary Matches

```rust
// Search entire buffer, but only accept matches that END in valid zone
if let Some(pos) = find_match_in(&chunk.buffer) {
    let match_end = pos + pattern.len();
    // CRITICAL: match must END after valid_start
    if match_end > chunk.valid_start {
        return Some(chunk.absolute_pos + pos);
    }
}
```

This ensures patterns spanning chunk boundaries are found exactly once.

## Current Implementation Issues

### Issue 1: find_pattern() - Inefficient but Correct

```rust
// Current implementation (lines 526-568)
fn find_pattern(&self, start: usize, end: usize, pattern: &[u8]) -> Option<usize> {
    const CHUNK_SIZE: usize = 64 * 1024;
    let overlap = pattern.len().saturating_sub(1);
    let mut offset = start;

    while offset < end {
        let chunk_size = CHUNK_SIZE.min(end - offset);
        let text = self.get_text_range(offset, chunk_size);  // Materializes chunk

        if let Some(pos) = Self::find_in_bytes(&text, pattern) {
            // ... return match
        }

        // Overlapping logic
        offset += chunk_size;
        if offset < end {
            offset = offset.saturating_sub(overlap);  // Back up for overlap
        }
    }
}
```

**Problems:**
- Materializes 64KB chunks unnecessarily
- The overlap logic is WRONG: it backs up AFTER advancing, which means chunks don't properly overlap
- Should use `iter_pieces_in_range()` and build chunks on-the-fly

### Issue 2: find_regex() - CRITICAL BUG

```rust
// Current implementation (lines 628-660)
fn find_regex(&self, start: usize, end: usize, regex: &Regex) -> Option<usize> {
    const CHUNK_SIZE: usize = 1024 * 1024;  // 1MB

    while offset < end {
        let text = self.get_text_range(offset, chunk_size);

        if let Some(mat) = regex.find(&text) {
            return Some(offset + mat.start());
        }

        offset += chunk_size;  // NO OVERLAP!
    }
}
```

**BUG:** No overlap means regex patterns spanning 1MB chunk boundaries are MISSED!

Example:
- Chunk 1 ends: "...hello wo"
- Chunk 2 starts: "rld..."
- Regex `/hello world/` will NOT match!

## Proposed Solution

### Design Principles

1. **Use iter_pieces_in_range()**: Don't materialize full chunks upfront
2. **Stream pieces into reusable buffer**: Build chunks incrementally from pieces
3. **Proper overlap**: Keep overlap bytes from previous chunk
4. **Valid zone tracking**: Ensure boundary matches are found exactly once

### New OverlappingChunks Iterator

```rust
pub struct OverlappingChunks<'a> {
    piece_iter: PieceRangeIter,  // From piece_tree
    buffers: &'a [StringBuffer],

    buffer: Vec<u8>,           // Reusable chunk buffer
    buffer_absolute_pos: usize, // Where buffer starts in document

    current_pos: usize,        // Current read position
    end_pos: usize,            // End of search range

    chunk_size: usize,         // Target chunk size
    overlap: usize,            // Overlap size

    first_chunk: bool,         // Track first chunk special case
}
```

### Implementation Strategy

```rust
impl<'a> OverlappingChunks<'a> {
    fn fill_next_chunk(&mut self) -> bool {
        if self.first_chunk {
            // First chunk: fill up to chunk_size
            self.first_chunk = false;
            while self.buffer.len() < self.chunk_size && self.current_pos < self.end_pos {
                // Read from piece_iter into buffer
                // ...
            }
        } else {
            // Subsequent chunks: keep overlap, add chunk_size NEW bytes
            if self.buffer.len() > self.overlap {
                // Remove old bytes, keep overlap at end
                let drain = self.buffer.len() - self.overlap;
                self.buffer.drain(0..drain);
                self.buffer_absolute_pos += drain;
            }

            // Fill chunk_size NEW bytes
            let target = self.overlap + self.chunk_size;
            while self.buffer.len() < target && self.current_pos < self.end_pos {
                // Read from piece_iter into buffer
                // ...
            }
        }
    }
}

impl<'a> Iterator for OverlappingChunks<'a> {
    type Item = ChunkInfo;

    fn next(&mut self) -> Option<ChunkInfo> {
        let is_first = self.buffer_absolute_pos == self.current_pos;

        if !self.fill_next_chunk() {
            return None;
        }

        let valid_start = if is_first { 0 } else { self.overlap };

        Some(ChunkInfo {
            buffer: self.buffer.clone(),  // or return reference
            absolute_pos: self.buffer_absolute_pos,
            valid_start,
        })
    }
}
```

### Updated find_pattern()

```rust
fn find_pattern(&self, start: usize, end: usize, pattern: &[u8]) -> Option<usize> {
    if pattern.is_empty() || start >= end {
        return None;
    }

    const CHUNK_SIZE: usize = 64 * 1024;
    let overlap = pattern.len().saturating_sub(1);

    let chunks = OverlappingChunks::new(
        &self.piece_tree,
        &self.buffers,
        start,
        end,
        CHUNK_SIZE,
        overlap,
    );

    for chunk in chunks {
        // Search entire chunk buffer
        if let Some(pos) = Self::find_in_bytes(&chunk.buffer, pattern) {
            let match_end = pos + pattern.len();
            // Only report if match ENDS in valid zone
            if match_end > chunk.valid_start {
                return Some(chunk.absolute_pos + pos);
            }
        }
    }

    None
}
```

### Updated find_regex()

```rust
fn find_regex(&self, start: usize, end: usize, regex: &Regex) -> Option<usize> {
    if start >= end {
        return None;
    }

    const CHUNK_SIZE: usize = 1024 * 1024;  // 1MB
    const OVERLAP: usize = 4096;  // 4KB overlap for regex

    let chunks = OverlappingChunks::new(
        &self.piece_tree,
        &self.buffers,
        start,
        end,
        CHUNK_SIZE,
        OVERLAP,
    );

    for chunk in chunks {
        // Search entire chunk buffer
        if let Some(mat) = regex.find(&chunk.buffer) {
            let match_end = mat.end();
            // Only report if match ENDS in valid zone
            if match_end > chunk.valid_start {
                return Some(chunk.absolute_pos + mat.start());
            }
        }
    }

    None
}
```

## Benefits

1. **Memory efficiency**: Reuses buffer, streams from pieces
2. **Correctness**: Fixes regex boundary bug
3. **Performance**: Avoids materializing full chunks upfront, directly iterates pieces
4. **Simplicity**: Clean iterator abstraction

## Implementation Plan

1. ✅ Analyze current implementation and original chunked_search.rs
2. ⏳ Implement `OverlappingChunks` iterator in text_buffer.rs
3. Update `find_pattern()` to use `OverlappingChunks`
4. Update `find_regex()` to use `OverlappingChunks`
5. Add tests for boundary cases (especially regex spanning chunks)
6. Verify all existing tests pass

## Test Cases Needed

### Critical Test: Regex Spanning Boundary

```rust
#[test]
fn test_regex_spans_chunk_boundary() {
    let mut buf = TextBuffer::new();
    // Create content where "hello world" spans a 1MB boundary
    let prefix = "x".repeat(1024 * 1024 - 6);  // Ends 6 bytes before 1MB
    let content = format!("{}hello world", prefix);
    buf.insert(0, &content);

    let regex = Regex::new("hello world").unwrap();
    let pos = buf.find_regex(0, buf.len(), &regex);

    assert_eq!(pos, Some(1024 * 1024 - 6));  // Should find it!
}
```

### Property Test: All Matches Found

```rust
proptest! {
    #[test]
    fn prop_chunked_finds_all_literal_matches(
        pattern in "[a-z]{3,10}",
        content in "[a-z0-9 ]{1000,10000}",
    ) {
        // Insert pattern at various positions
        // Verify chunked search finds all occurrences
    }
}
```

## Performance Expectations

- **Memory**: O(chunk_size) instead of O(search_range)
- **CPU**: Minimal overhead from iterator vs direct get_text_range
- **Correctness**: 100% - finds all matches including boundary-spanning ones
