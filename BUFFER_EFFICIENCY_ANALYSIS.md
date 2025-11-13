# TextBuffer Efficiency Analysis

## Executive Summary

The current `text_buffer.rs` implementation performs many redundant tree traversals when using the piece tree. Multiple O(log n) operations are chained together where a single optimized operation could suffice. This analysis identifies 7 major inefficiency patterns and proposes solutions.

## Inefficiency Categories

### 🔴 CRITICAL: Double Tree Traversal

#### 1. `insert_at_position()` - Line 266-268
**Current Implementation:**
```rust
pub fn insert_at_position(&mut self, position: Position, text: Vec<u8>) -> Cursor {
    let offset = self.position_to_offset(position);  // O(log n) tree traversal
    self.insert_bytes(offset, text)                  // O(log n) tree traversal
}
```

**Problem:** Two separate O(log n) tree traversals:
1. `position_to_offset()` traverses tree to find offset from line/column
2. `insert_bytes()` traverses tree again to find insertion point

**Solution:** Add `insert_at_position()` to PieceTree that does both in single traversal:
```rust
// In piece_tree.rs
pub fn insert_at_position(
    &mut self,
    line: usize,
    column: usize,
    location: BufferLocation,
    buffer_offset: usize,
    bytes: usize,
    line_feed_cnt: usize,
    buffers: &[StringBuffer],
) -> Cursor
```

**Performance Impact:** Reduces from 2× O(log n) to 1× O(log n) - **50% improvement**

---

#### 2. `delete_range()` - Line 290-296
**Current Implementation:**
```rust
pub fn delete_range(&mut self, start: Position, end: Position) {
    let start_offset = self.position_to_offset(start);  // O(log n)
    let end_offset = self.position_to_offset(end);      // O(log n)

    if end_offset > start_offset {
        self.delete_bytes(start_offset, end_offset - start_offset);  // O(log n)
    }
}
```

**Problem:** Three O(log n) tree traversals for a single delete operation

**Solution:** Add `delete_range()` to PieceTree that traverses once:
```rust
// In piece_tree.rs
pub fn delete_range(
    &mut self,
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
    buffers: &[StringBuffer]
)
```

**Performance Impact:** Reduces from 3× O(log n) to 1× O(log n) - **66% improvement**

---

### 🟠 HIGH: Multiple Lookups in Same Context

#### 3. `line_col_to_position()` - Line 731-743
**Current Implementation:**
```rust
pub fn line_col_to_position(&self, line: usize, character: usize) -> usize {
    if let Some(line_start) = self.line_start_offset(line) {  // Lookup 1: O(log n)
        let line_bytes = if let Some(line_text) = self.get_line(line) {  // Lookup 2: O(log n)
            line_text.len()
        } else {
            0
        };
        let byte_offset = character.min(line_bytes);
        line_start + byte_offset
    } else {
        self.len()
    }
}
```

**Problem:**
1. `line_start_offset()` does tree traversal via `piece_tree.line_range()`
2. `get_line()` does the SAME tree traversal again via `piece_tree.line_range()`

**Solution:** Combine into single lookup:
```rust
pub fn line_col_to_position(&self, line: usize, character: usize) -> usize {
    if let Some((start, end)) = self.piece_tree.line_range(line, &self.buffers) {
        let line_len = end.map_or(self.total_bytes(), |e| e) - start;
        start + character.min(line_len)
    } else {
        self.len()
    }
}
```

**Performance Impact:** Reduces from 2× O(log n) to 1× O(log n) - **50% improvement**

---

#### 4. `position_to_lsp_position()` - Line 748-762
**Current Implementation:**
```rust
pub fn position_to_lsp_position(&self, byte_pos: usize) -> (usize, usize) {
    let pos = self.offset_to_position(byte_pos);  // Lookup 1: O(log n)
    let line = pos.line;
    let column_bytes = pos.column;

    if let Some(line_bytes) = self.get_line(line) {  // Lookup 2: O(log n)
        // Convert byte offset to UTF-16 code units
        let text_before = &line_bytes[..column_bytes.min(line_bytes.len())];
        let text_str = String::from_utf8_lossy(text_before);
        let utf16_offset = text_str.encode_utf16().count();
        (line, utf16_offset)
    } else {
        (line, 0)
    }
}
```

**Problem:**
1. `offset_to_position()` traverses to find line/column
2. `get_line()` traverses again to get line content

**Solution:** Get line content during position lookup in piece tree, or add helper:
```rust
// In piece_tree.rs
pub fn offset_to_position_with_line_content(
    &self,
    offset: usize,
    buffers: &[StringBuffer]
) -> ((usize, usize), Vec<u8>)
```

**Performance Impact:** Reduces from 2× O(log n) to 1× O(log n) - **50% improvement**

---

#### 5. `lsp_position_to_byte()` - Line 767-792
**Current Implementation:**
```rust
pub fn lsp_position_to_byte(&self, line: usize, utf16_offset: usize) -> usize {
    if let Some(line_start) = self.line_start_offset(line) {  // Lookup 1: O(log n)
        if let Some(line_bytes) = self.get_line(line) {  // Lookup 2: O(log n)
            let line_str = String::from_utf8_lossy(&line_bytes);
            // ... UTF-16 conversion ...
            line_start + byte_offset
        } else {
            line_start
        }
    } else {
        self.len()
    }
}
```

**Problem:** Same as #3 - `line_start_offset` and `get_line` both call `line_range()`

**Solution:** Use single `line_range()` call as shown in #3

**Performance Impact:** Reduces from 2× O(log n) to 1× O(log n) - **50% improvement**

---

### 🟡 MEDIUM: Inefficient Iteration

#### 6. `get_text_range()` - Line 302-341
**Current Implementation:**
```rust
pub fn get_text_range(&self, offset: usize, bytes: usize) -> Vec<u8> {
    let mut result = Vec::with_capacity(bytes);
    let mut remaining = bytes;
    let mut current_offset = offset;

    while remaining > 0 {
        if let Some(piece_info) = self.piece_tree.find_by_offset(current_offset) {
            // ... copy data from this piece ...
            current_offset += to_read;
        } else {
            break;
        }
    }

    result
}
```

**Problem:** Calls `find_by_offset()` once per piece encountered. For a range spanning N pieces, this is N × O(log n) operations.

**Solution:** Add piece iterator to avoid repeated tree traversals:
```rust
// In piece_tree.rs
pub fn iter_pieces_in_range(&self, start: usize, end: usize) -> PieceRangeIter<'_>

// In text_buffer.rs
pub fn get_text_range(&self, offset: usize, bytes: usize) -> Vec<u8> {
    let mut result = Vec::with_capacity(bytes);

    for piece in self.piece_tree.iter_pieces_in_range(offset, offset + bytes) {
        // Copy data from piece without re-traversing tree
    }

    result
}
```

**Performance Impact:** Reduces from N × O(log n) to 1× O(log n) + O(N) - **Massive improvement for large ranges**

---

#### 7. `find_pattern()` - Line 488-530
**Current Implementation:**
```rust
fn find_pattern(&self, start: usize, end: usize, pattern: &[u8]) -> Option<usize> {
    // ...
    const CHUNK_SIZE: usize = 64 * 1024;
    let search_len = end - start;

    if search_len <= CHUNK_SIZE {
        let text = self.get_text_range(start, search_len);  // Multiple find_by_offset calls
        // ... search ...
    } else {
        let overlap = pattern.len().saturating_sub(1);
        let mut offset = start;

        while offset < end {
            let chunk_size = CHUNK_SIZE.min(end - offset);
            let text = self.get_text_range(offset, chunk_size);  // Multiple find_by_offset calls per iteration
            // ... search ...
            offset += chunk_size;
        }
    }
}
```

**Problem:** Each `get_text_range()` call does multiple `find_by_offset()` calls (see #6). For large searches with M chunks, each spanning N pieces, this is M × N × O(log n) operations.

**Solution:** Use piece iterator from #6 solution:
```rust
fn find_pattern(&self, start: usize, end: usize, pattern: &[u8]) -> Option<usize> {
    // Stream through pieces without repeated tree traversals
    let mut searcher = StreamingSearch::new(pattern);

    for piece in self.piece_tree.iter_pieces_in_range(start, end) {
        if let Some(pos) = searcher.feed(piece.data, piece.offset) {
            return Some(pos);
        }
    }

    None
}
```

**Performance Impact:** Reduces from M × N × O(log n) to 1× O(log n) + O(M×N) - **Orders of magnitude improvement for large searches**

---

## Summary of Required PieceTree Additions

To fix these inefficiencies, the following methods should be added to `piece_tree.rs`:

### 1. Direct Position-Based Operations
```rust
impl PieceTree {
    /// Insert at position without converting to offset first
    pub fn insert_at_position(
        &mut self,
        line: usize,
        column: usize,
        location: BufferLocation,
        buffer_offset: usize,
        bytes: usize,
        line_feed_cnt: usize,
        buffers: &[StringBuffer],
    ) -> Cursor;

    /// Delete range by position without converting to offsets first
    pub fn delete_range(
        &mut self,
        start_line: usize,
        start_col: usize,
        end_line: usize,
        end_col: usize,
        buffers: &[StringBuffer],
    );
}
```

### 2. Combined Lookups
```rust
impl PieceTree {
    /// Get position and line content in single traversal
    pub fn offset_to_position_with_line(
        &self,
        offset: usize,
        buffers: &[StringBuffer],
    ) -> Option<((usize, usize), Vec<u8>)>;

    /// Get line start offset and length in single traversal
    /// Returns (start_offset, length)
    pub fn line_info(&self, line: usize, buffers: &[StringBuffer]) -> Option<(usize, usize)>;
}
```

### 3. Efficient Iteration
```rust
/// Iterator over pieces in a byte range
pub struct PieceRangeIter<'a> {
    // Internal state
}

impl PieceTree {
    /// Iterate through pieces overlapping a byte range
    pub fn iter_pieces_in_range(&self, start: usize, end: usize) -> PieceRangeIter<'_>;
}

impl<'a> Iterator for PieceRangeIter<'a> {
    type Item = PieceView<'a>;
    fn next(&mut self) -> Option<Self::Item>;
}

pub struct PieceView<'a> {
    pub data: &'a [u8],
    pub start_offset: usize,  // Where this piece starts in document
    pub location: BufferLocation,
}
```

---

## Performance Impact Summary

| Issue | Current Complexity | Optimized Complexity | Impact |
|-------|-------------------|---------------------|---------|
| insert_at_position | 2 × O(log n) | 1 × O(log n) | 🔴 Critical - 50% improvement |
| delete_range | 3 × O(log n) | 1 × O(log n) | 🔴 Critical - 66% improvement |
| line_col_to_position | 2 × O(log n) | 1 × O(log n) | 🟠 High - 50% improvement |
| position_to_lsp_position | 2 × O(log n) | 1 × O(log n) | 🟠 High - 50% improvement |
| lsp_position_to_byte | 2 × O(log n) | 1 × O(log n) | 🟠 High - 50% improvement |
| get_text_range | N × O(log n) | O(log n) + O(N) | 🟡 Medium - Massive for large N |
| find_pattern | M × N × O(log n) | O(log n) + O(M×N) | 🟡 Medium - Orders of magnitude |

---

## Implementation Priority

### Phase 1: Quick Wins (Low-Hanging Fruit)
1. Fix #3, #4, #5: Combine double lookups in LSP and line_col operations
   - Requires only changing text_buffer.rs logic
   - No piece_tree changes needed
   - ~1-2 hours work

### Phase 2: Core Operations
2. Add piece_tree iteration (#6, #7)
   - Implement PieceRangeIter
   - Update get_text_range and search
   - ~4-6 hours work

### Phase 3: Position-Based Operations
3. Add insert_at_position and delete_range to piece_tree (#1, #2)
   - Most complex changes
   - Requires careful testing
   - ~8-12 hours work

---

## Testing Considerations

For each optimization:
1. **Correctness**: Ensure behavior matches current implementation exactly
2. **Performance**: Benchmark with realistic workloads (1MB, 10MB, 100MB files)
3. **Edge cases**: Empty buffer, single line, end of file, etc.

Suggested benchmark operations:
- Insert at position 1000 times at random line/column positions
- Delete ranges of various sizes
- Search for patterns in 10MB file
- LSP position conversions (common in editor use)

---

## Additional Notes

### Current Architecture Strengths
- PieceTree already has good line tracking integrated
- StringBuffer with line_starts enables O(log n) line lookups within pieces
- Cursor type is well-designed for tracking position state

### Current Architecture Weaknesses
- No iteration interface forces repeated tree traversals
- No position-based operations forces conversion overhead
- Each operation is designed in isolation without considering common patterns

### Long-Term Vision
Consider adding a "cursor" or "view" abstraction that maintains position context across multiple operations, similar to how iterators work in Rust. This would enable even more efficient operation sequences.
