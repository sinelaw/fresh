# Piece Table Text Buffer

A piece table implementation for efficient text editing with separated byte and line tracking.

## Architecture Overview

The implementation uses **separate concerns for byte-level operations and line tracking**. This consists of three main modules:

1. **PieceTree** - Byte-oriented binary tree for O(log n) text operations
2. **LineIndex** - Separate line tracking for O(log n) line/column conversions
3. **TextBuffer** - High-level API combining both with buffer management

## Goals

1. **Huge file support** (>1GB) - efficient operations without full scanning
2. **Diagnostics support** - index by line number + column for LSP
3. **Jump to byte position** - O(log n) offset lookups
4. **Jump to line/column** - O(log n) position conversions
5. **Iterate lines** - efficient line traversal from known positions
6. **Persist without scanning** - lazy loading support
7. **Multi-cursor editing** - efficient modifications at multiple positions

## Module Details

### PieceTree (`src/piece_tree.rs`)

A balanced binary tree that tracks **bytes only**, with no line information.

**Node Structure:**
```rust
pub enum PieceTreeNode {
    Internal {
        left_bytes: usize,  // Total bytes in left subtree
        left: Arc<PieceTreeNode>,
        right: Arc<PieceTreeNode>,
    },
    Leaf {
        location: BufferLocation,  // Stored or Added
        offset: usize,             // Offset within buffer
        bytes: usize,              // Length in bytes
    },
}
```

**Key Features:**
- "Calculated keys" - left subtree byte totals for O(log n) navigation
- Two-buffer approach: `Stored` (original/immutable) and `Added` (modifications)
- Simple rebuild-based rebalancing (triggered when depth > 2×log₂(N))
- Insert/delete operations split and merge pieces efficiently

**Operations:**
- `find_by_offset(offset)` - Find piece containing byte offset: **O(log n)**
- `insert(offset, location, buffer_offset, bytes)` - Insert at position: **O(log n)**
- `delete(offset, bytes)` - Delete range: **O(log n)**

### LineIndex (`src/line_index.rs`)

Separate index mapping between line/column positions and byte offsets.

**Structure:**
```rust
pub struct LineIndex {
    line_starts: Vec<usize>,  // Byte offset where each line starts
}

pub struct Position {
    pub line: usize,    // 0-indexed line number
    pub column: usize,  // Byte offset within line
}
```

**Key Features:**
- Binary search for O(log n) conversions
- Tracks newline positions during insert/delete
- Independent of piece tree structure

**Operations:**
- `offset_to_position(offset)` - Convert byte offset to line/column: **O(log n)**
- `position_to_offset(position)` - Convert line/column to byte offset: **O(log n)**
- `insert(offset, text)` - Update after insertion: **O(n)** worst case
- `delete(offset, bytes, text)` - Update after deletion: **O(n)** worst case

### TextBuffer (`src/text_buffer.rs`)

High-level API that combines PieceTree + LineIndex + buffer management.

**Structure:**
```rust
pub struct TextBuffer {
    piece_tree: PieceTree,
    line_index: LineIndex,
    stored_buffer: Vec<u8>,  // Original file content
    added_buffer: Vec<u8>,   // In-memory modifications
}
```

**Key Operations:**
```rust
// Insert operations
pub fn insert_bytes(&mut self, offset: usize, text: Vec<u8>) -> Cursor
pub fn insert_at_position(&mut self, position: Position, text: Vec<u8>) -> Cursor

// Delete operations
pub fn delete_bytes(&mut self, offset: usize, bytes: usize)
pub fn delete_range(&mut self, start: Position, end: Position)

// Text retrieval
pub fn get_text_range(&self, offset: usize, bytes: usize) -> Vec<u8>
pub fn get_line(&self, line: usize) -> Option<Vec<u8>>
pub fn get_all_text(&self) -> Vec<u8>

// Position conversions
pub fn offset_to_position(&self, offset: usize) -> Position
pub fn position_to_offset(&self, position: Position) -> usize
```

## Technical Choices

### Why Separate Line Tracking?

**Design:** Piece tree tracks bytes only, separate index tracks line positions.

**Benefits:**
1. **Simpler tree operations** - no line count updates during splits
2. **Accurate line counts** - LineIndex scans actual text for newlines
3. **Flexible** - can support different line ending conventions
4. **Clear separation** - each module has a single responsibility

**Trade-off:** Insert/delete in LineIndex is O(n) for line array updates, but this is acceptable because:
- Amortized over many small edits
- Simpler than maintaining line counts in tree nodes
- LineIndex operations are fast in practice (contiguous memory, cache-friendly)

**Future Optimization:** The O(n) LineIndex updates could be improved to O(log n) by replacing the `Vec<usize>` with a tree-based structure (interval tree or rope). This would maintain line start offsets in a balanced tree, allowing efficient insertions/deletions while preserving the ability to binary search for line positions. The current array-based approach was chosen for simplicity and is sufficient for typical editing patterns.

### Rebalancing Strategy

**Simple rebuild-based rebalancing** instead of AVL/Red-Black rotation:

```rust
// Trigger when depth exceeds threshold
if depth > 2 * log₂(leaf_count) {
    rebuild_tree()
}
```

**Benefits:**
- Simple implementation (no rotation complexity)
- Predictable performance
- Works well for typical editing patterns
- Easy to understand and maintain

**Trade-off:** Occasional O(n) rebuild vs. O(log n) rotations, but rebuilds are infrequent.

### Two-Buffer Design

**Stored Buffer:** Original file content (read-only, can be memory-mapped)
**Added Buffer:** All modifications (append-only during session)

**Benefits:**
1. **Memory efficient** - original content not duplicated
2. **Undo-friendly** - modifications tracked separately
3. **Persistence** - can save only the Added buffer + piece tree
4. **Memory-mapped files** - Stored buffer can point to disk

## Performance Characteristics

| Operation | Time Complexity | Notes |
|-----------|----------------|-------|
| Insert/Delete (bytes) | O(log n) | PieceTree operation + LineIndex update |
| Find by offset | O(log n) | Binary tree traversal |
| Find by position | O(log n) | Binary search in line_starts |
| Get text range | O(k + log n) | k = pieces touched |
| Get line | O(log n + m) | m = line length |
| Rebalance | O(n) | Infrequent, triggered by depth |

Where:
- n = number of pieces in tree
- k = number of pieces in requested range
- m = bytes in requested range

## Testing

Comprehensive test coverage with property-based testing:

- **PieceTree:** 21 tests (12 unit + 9 property tests)
- **LineIndex:** 20 tests (12 unit + 8 property tests)
- **TextBuffer:** 29 tests (18 unit + 11 property tests)

Property tests verify invariants:
- Byte counts always consistent after operations
- Line counts match newline counts
- Offset ↔ Position conversions are bijective
- Insert then delete restores original state
- Tree depth never exceeds balanced threshold

## Usage Example

```rust
use crate::text_buffer::TextBuffer;
use crate::line_index::Position;

// Create buffer from file content
let mut buffer = TextBuffer::new(b"hello\nworld\n".to_vec());

// Insert at line/column
buffer.insert_at_position(
    Position { line: 1, column: 0 },
    b"beautiful ".to_vec()
);
// Result: "hello\nbeautiful world\n"

// Delete range
buffer.delete_range(
    Position { line: 0, column: 5 },
    Position { line: 1, column: 0 }
);
// Result: "hellobeautiful world\n"

// Get text by line
let line1 = buffer.get_line(0).unwrap();
// line1 = b"hellobeautiful world\n"

// Convert between offsets and positions
let pos = buffer.offset_to_position(5);
// pos = Position { line: 0, column: 5 }

let offset = buffer.position_to_offset(Position { line: 0, column: 5 });
// offset = 5
```

## References

- **VSCode's Piece Table:** [Text Buffer Reimplementation](https://code.visualstudio.com/blogs/2018/03/23/text-buffer-reimplementation)
- **Original Paper:** Crowley, C. (1998). "Data structures for text sequences"
- **Implementation inspired by:** VSCode's editor core architecture
