# Optimal PieceTree API Design

## Current State Analysis

### ✅ Already Implemented in PieceTree

| Method | Signature | Purpose | Status |
|--------|-----------|---------|--------|
| `position_to_offset` | `(line, col, buffers) -> offset` | Convert position to byte offset | ✅ Efficient |
| `offset_to_position` | `(offset, buffers) -> (line, col)` | Convert byte offset to position | ✅ Efficient |
| `line_range` | `(line, buffers) -> Option<(start, Option<end>)>` | Get byte range for a line | ✅ Efficient |
| `iter_pieces_in_range` | `(start, end) -> PieceRangeIter` | Iterate pieces in range | ✅ Efficient (ONE traversal!) |
| `insert` | `(offset, location, buffer_offset, bytes, lf_cnt, buffers)` | Insert at byte offset | ✅ Works |
| `delete` | `(offset, bytes, buffers)` | Delete at byte offset | ✅ Works |

### ❌ Current Inefficiencies in text_buffer.rs

#### 1. **insert_at_position** (Line 272-274) - Double Traversal
```rust
pub fn insert_at_position(&mut self, position: Position, text: Vec<u8>) -> Cursor {
    let offset = self.position_to_offset(position);  // Traversal #1
    self.insert_bytes(offset, text)                   // Traversal #2 (via piece_tree.insert)
}
```
**Problem**: Two separate tree traversals to the same location
**Impact**: 🔴 Critical - Used for every position-based insert

---

#### 2. **delete_range** (Line 298-305) - Triple Traversal
```rust
pub fn delete_range(&mut self, start: Position, end: Position) {
    let start_offset = self.position_to_offset(start);  // Traversal #1
    let end_offset = self.position_to_offset(end);      // Traversal #2
    if end_offset > start_offset {
        self.delete_bytes(start_offset, end_offset - start_offset);  // Traversal #3
    }
}
```
**Problem**: Three separate tree traversals
**Impact**: 🔴 Critical - Used for every position-based delete

---

#### 3. **line_col_to_position** (Line 735-748) - Double Lookup
```rust
pub fn line_col_to_position(&self, line: usize, character: usize) -> usize {
    if let Some(line_start) = self.line_start_offset(line) {  // Calls line_range()
        let line_bytes = if let Some(line_text) = self.get_line(line) {  // Calls line_range() AGAIN
            line_text.len()
        } else {
            0
        };
        // ...
    }
}
```
**Problem**: Both `line_start_offset()` and `get_line()` call `piece_tree.line_range()`
**Impact**: 🟠 High - Can be fixed by calling `line_range()` once
**Solution**: NO NEW API NEEDED - just refactor text_buffer

---

#### 4. **position_to_lsp_position** (Line 752-767) - Double Lookup
```rust
pub fn position_to_lsp_position(&self, byte_pos: usize) -> (usize, usize) {
    let pos = self.offset_to_position(byte_pos);  // Traversal #1
    // ...
    if let Some(line_bytes) = self.get_line(line) {  // Traversal #2 (line_range)
        // UTF-16 conversion...
    }
}
```
**Problem**: Getting position, then getting line content separately
**Impact**: 🟠 High - Common in LSP operations
**Solution**: Could benefit from combined API OR use existing APIs smarter

---

#### 5. **lsp_position_to_byte** (Line 771-797) - Double Lookup
```rust
pub fn lsp_position_to_byte(&self, line: usize, utf16_offset: usize) -> usize {
    if let Some(line_start) = self.line_start_offset(line) {  // Calls line_range()
        if let Some(line_bytes) = self.get_line(line) {       // Calls line_range() AGAIN
            // UTF-16 conversion...
        }
    }
}
```
**Problem**: Same as #3 - double call to `line_range()`
**Impact**: 🟠 High - Common in LSP operations
**Solution**: NO NEW API NEEDED - just refactor text_buffer

---

#### 6. **get_text_range** (Line 308-348) - Multiple Traversals
```rust
pub fn get_text_range(&self, offset: usize, bytes: usize) -> Vec<u8> {
    // ...
    while remaining > 0 {
        if let Some(piece_info) = self.piece_tree.find_by_offset(current_offset) {
            // Read from piece...
            current_offset += to_read;
        }
    }
}
```
**Problem**: Calls `find_by_offset()` once per piece (N × O(log n))
**Impact**: 🟡 Medium - Bad for large ranges spanning many pieces
**Solution**: USE EXISTING `iter_pieces_in_range()` - NO NEW API NEEDED!

---

#### 7. **find_pattern** (Line 488+) - Compound Inefficiency
```rust
fn find_pattern(&self, start: usize, end: usize, pattern: &[u8]) -> Option<usize> {
    // ...
    while offset < end {
        let text = self.get_text_range(offset, chunk_size);  // Calls #6 which is inefficient
        // ...
    }
}
```
**Problem**: Calls inefficient `get_text_range()` multiple times
**Impact**: 🟡 Medium - Will be fixed when #6 is fixed
**Solution**: Fixed by fixing #6

---

## Required API Additions

### Priority 1: Position-Based Modifications (Issues #1, #2)

These require NEW piece_tree APIs because they eliminate redundant traversals:

```rust
impl PieceTree {
    /// Insert at a position without converting to offset first
    /// Performs position lookup and insertion in a SINGLE tree traversal
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

    /// Delete a range specified by positions without converting to offsets first
    /// Performs position lookups and deletion in a SINGLE tree traversal
    pub fn delete_position_range(
        &mut self,
        start_line: usize,
        start_column: usize,
        end_line: usize,
        end_column: usize,
        buffers: &[StringBuffer],
    );
}
```

**Rationale**:
- Current approach: position→offset (O(log n)) + insert/delete at offset (O(log n)) = 2-3× O(log n)
- Optimized approach: Direct position-based operation = 1× O(log n)
- **Performance gain: 50-66% reduction in tree traversals**

---

### Priority 2: Use Existing APIs Better (Issues #3, #5, #6)

These DON'T need new APIs - just smarter use of existing ones:

#### Issue #3 & #5: Fix double `line_range()` calls
```rust
// OLD (text_buffer.rs):
let line_start = self.line_start_offset(line);  // Calls piece_tree.line_range()
let line_text = self.get_line(line);            // Calls piece_tree.line_range() AGAIN

// NEW:
if let Some((start, end)) = self.piece_tree.line_range(line, &self.buffers) {
    let line_len = end.map_or(self.total_bytes(), |e| e) - start;
    // Use start and line_len...
}
```

#### Issue #6: Use existing `iter_pieces_in_range()`
```rust
// OLD (text_buffer.rs):
while remaining > 0 {
    let piece_info = self.piece_tree.find_by_offset(current_offset);  // N × O(log n)
    // ...
}

// NEW:
for piece_view in self.piece_tree.iter_pieces_in_range(offset, offset + bytes) {
    let buffer = &self.buffers[piece_view.location.buffer_id()].data;
    let data = &buffer[piece_view.buffer_offset..piece_view.buffer_offset + piece_view.bytes];
    result.extend_from_slice(data);
}
```

---

### Priority 3: Optional Enhancement (Issue #4)

Issue #4 could benefit from a combined API but is less critical:

```rust
impl PieceTree {
    /// Get line content efficiently during position lookup
    /// Returns position and the actual line data in one traversal
    ///
    /// This is useful for operations that need both position info and line content,
    /// such as UTF-16 offset conversions in LSP
    pub fn offset_to_position_with_line_content(
        &self,
        offset: usize,
        buffers: &[StringBuffer],
    ) -> ((usize, usize), Vec<u8>);
}
```

**Note**: This is OPTIONAL - Issue #4 can also be addressed by refactoring text_buffer to use existing APIs better.

---

## Implementation Plan

### Phase 1: Refactor text_buffer to use existing APIs (1-2 hours)
**No piece_tree changes needed!**

- [ ] Fix `line_col_to_position` to call `line_range()` once (Issue #3)
- [ ] Fix `lsp_position_to_byte` to call `line_range()` once (Issue #5)
- [ ] Rewrite `get_text_range` to use `iter_pieces_in_range()` (Issue #6)
- [ ] Verify `find_pattern` benefits from #6 fix (Issue #7)
- [ ] Run tests to ensure correctness

**Expected gain**: 50% reduction in lookups for LSP operations, massive improvement for large range reads

---

### Phase 2: Add position-based operations to piece_tree (4-8 hours)
**Requires piece_tree changes**

- [ ] Implement `insert_at_position()` in PieceTree (Issue #1)
- [ ] Implement `delete_position_range()` in PieceTree (Issue #2)
- [ ] Update text_buffer to use new APIs
- [ ] Run comprehensive tests
- [ ] Benchmark insert/delete operations at positions

**Expected gain**: 50-66% reduction in tree traversals for position-based modifications

---

### Phase 3: Optional LSP enhancement (2-4 hours)
**Optional - only if benchmarks show significant benefit**

- [ ] Implement `offset_to_position_with_line_content()` (Issue #4)
- [ ] Update `position_to_lsp_position()` to use it
- [ ] Benchmark LSP operations

**Expected gain**: 50% reduction for position-to-LSP conversions

---

## Key Insights

### What Already Works Well
1. ✅ `iter_pieces_in_range()` exists but isn't being used!
2. ✅ `line_range()` is efficient but being called multiple times unnecessarily
3. ✅ Position↔offset conversions are already optimal

### What Needs to Change
1. 🔴 **Critical**: Add position-based insert/delete to avoid double traversals
2. 🟠 **High**: Stop calling `line_range()` multiple times via wrapper methods
3. 🟡 **Medium**: Use `iter_pieces_in_range()` instead of repeated `find_by_offset()`

### Architecture Lesson
The piece_tree is already well-designed and line-aware. The inefficiencies are in text_buffer.rs:
- Using wrapper methods (`line_start_offset`, `get_line`) that hide redundant calls
- Not using the efficient `iter_pieces_in_range()` API
- Converting positions to offsets before every modification

---

## Performance Impact Summary

| Issue | Current | Optimized | Phase | Priority |
|-------|---------|-----------|-------|----------|
| #1: insert_at_position | 2 × O(log n) | 1 × O(log n) | 2 | 🔴 Critical |
| #2: delete_range | 3 × O(log n) | 1 × O(log n) | 2 | 🔴 Critical |
| #3: line_col_to_position | 2 × O(log n) | 1 × O(log n) | 1 | 🟠 High |
| #4: position_to_lsp_position | 2 × O(log n) | 1 × O(log n) | 3 | 🟡 Optional |
| #5: lsp_position_to_byte | 2 × O(log n) | 1 × O(log n) | 1 | 🟠 High |
| #6: get_text_range | N × O(log n) | O(log n) + O(N) | 1 | 🟡 Medium |
| #7: find_pattern | M×N × O(log n) | O(log n) + O(M×N) | 1 | 🟡 Medium |

**Total Phase 1 effort**: 1-2 hours, **NO** piece_tree changes
**Total Phase 2 effort**: 4-8 hours, piece_tree changes required
