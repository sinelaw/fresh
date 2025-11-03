# Editor Architecture Analysis: VirtualFile Refactoring

## Current Problem

The editor is in a half-refactored state:
- `main.rs` expects the **OLD** VirtualFile API (with `chunk_lines: Vec<LoadedLine>`)
- `virtual_file.rs` has been **partially refactored** to use `ChunkTree` for storage
- The refactoring is **incomplete** - many methods reference non-existent fields/methods

## Original Architecture (What main.rs expects)

### VirtualFile - Old Design
```rust
struct VirtualFile {
    chunk_lines: Vec<LoadedLine>,  // In-memory cache of parsed lines
    line_anchor: i64,               // Which line is at relative index 0
    offset_version: u64,            // Invalidation version for cursors
    // ... file/memstore fields
}
```

**Key Concept**: VirtualFile maintained a Vec of LoadedLine objects, each containing:
- The line content (EditLine)
- File offset metadata (where it came from in the file)

### How main.rs Used VirtualFile

**1. Line-Based Navigation (via LineCursor)**
```rust
// State holds:
struct State {
    lines: VirtualFile,      // The file content manager
    line_index: LineCursor,  // Current cursor position (opaque handle)
    cursor: Position,        // x,y position within the line
    ...
}

// Usage patterns:
- lines.seek(SeekFrom::Start(0)) -> LineCursor
- lines.next_line(&cursor) -> Option<LineCursor>
- lines.prev_line(&cursor) -> Option<LineCursor>
```

**2. Line Access & Mutation**
```rust
// Read-only access
lines.get(&line_index) -> Option<&LoadedLine>
  -> loaded_line.line() -> &EditLine  // Get the actual text

// Mutable access for editing
lines.get_mut(&line_index) -> Option<&mut EditLine>
  -> line.insert(pos, char)
  -> line.remove(pos)
  -> line.overwrite(pos, char)
```

**3. Line Structure Operations**
```rust
// Insert new line after current
lines.insert_after(&line_index, EditLine::new(content))

// Remove entire line, return its content
lines.remove(&line_index) -> Option<EditLine>
```

**4. Rendering**
```rust
// Get multiple lines for display
lines.iter_at(&start_cursor, count) -> Iterator<&LoadedLine>

// Used to render visible window:
let lines_on_screen = self.lines
    .iter_at(&computed_cursor, lines_per_page)
    .collect()
```

### LineCursor Design
```rust
struct LineCursor {
    relative: i64,         // Offset from line_anchor
    offset_version: u64,   // Tied to VirtualFile's version
}
```

**Invalidation Strategy**:
- When VirtualFile reloads chunks or changes anchor, increment `offset_version`
- Old cursors become invalid (version mismatch)
- Forces UI to re-seek and get fresh cursor

## New Architecture (Incomplete Refactoring)

### What Changed in VirtualFile
```rust
struct VirtualFile<'a> {
    loaded_chunks: ChunkTree<'a>,  // NEW: Tree-based storage
    // Missing: chunk_lines field that everything expects!
    line_anchor: i64,
    offset_version: u64,
    ...
}
```

**The Problem**: Code references `self.chunk_lines` everywhere, but it no longer exists!

### Missing Implementations

The commented-out code shows these methods were being refactored:

**1. `to_abs_index(&LineCursor) -> Option<usize>`**
- Convert relative cursor to absolute Vec index
- **Why needed**: chunk_lines was a Vec, needed index math
- **ChunkTree equivalent**: Need to navigate tree by line count

**2. `load_lines(offset: u64)`**
- Load a chunk from file, parse into lines, add to chunk_lines Vec
- **ChunkTree equivalent**: Load chunk bytes, parse, insert into tree

**3. `load_more_lines()`**
- Extend chunk_lines by loading next chunk
- **ChunkTree equivalent**: Append next chunk's parsed lines

**4. `first_key_value()` (called on loaded_chunks)**
- Old code: `self.loaded_chunks.first_key_value()` (expects BTreeMap API)
- **Problem**: ChunkTree doesn't have this method
- **Purpose**: Find the earliest loaded file offset

## What Needs to Happen

### Option 1: Revert to Old Design
**Pros**: main.rs works immediately
**Cons**: Loses ChunkTree benefits (persistent structure, efficient edits)

### Option 2: Complete the Refactoring
**Pros**: Gets the better ChunkTree-based architecture
**Cons**: Significant work to bridge the impedance mismatch

### Option 3: Hybrid Approach (Recommended)
Keep ChunkTree for storage, but maintain a line index:

```rust
struct VirtualFile<'a> {
    // Storage layer - raw bytes organized in tree
    loaded_chunks: ChunkTree<'a>,

    // Line index layer - maps line numbers to byte ranges in tree
    line_starts: Vec<LineInfo>,  // Sorted by line number

    // Cursor versioning
    line_anchor: i64,
    offset_version: u64,

    // Storage backend
    memstore: Memstore<FileLoadStore>,
    file: Arc<File>,
}

struct LineInfo {
    byte_offset: usize,  // Where in ChunkTree this line starts
    byte_length: usize,  // Length of line in bytes
    file_offset: u64,    // Where it came from in backing file
}
```

**How it works**:
1. **Loading**: Read chunk -> parse into lines -> record LineInfo -> store bytes in ChunkTree
2. **Line access**: `line_starts[index]` gives byte range -> extract from ChunkTree
3. **Line editing**: Modify ChunkTree at byte offset -> update LineInfo byte_offsets for subsequent lines
4. **Navigation**: Just index arithmetic on line_starts Vec

## The Editor's Core Use Case

**Goal**: Edit extremely large files (GB+) that don't fit in RAM

**Strategy**:
1. **Chunk loading**: Only load portions of file near cursor
2. **Line-based abstraction**: User navigates by lines, not byte offsets
3. **Lazy loading**: Load more chunks when user scrolls near boundaries
4. **Persistence**: ChunkTree enables undo/redo without copying entire file

**Key Requirement**: Must maintain illusion of full file while only keeping ~few MB in memory

## Rendering Pipeline (main.rs)

```
User Input (arrow keys, typing, etc.)
  ↓
State::handle_key_event()
  ↓ (updates cursor, calls VirtualFile methods)
VirtualFile::{next_line, get_mut, insert_after, etc.}
  ↓ (returns LoadedLine objects)
State::render()
  ↓ (collect visible lines)
iter_at(cursor, count) -> Vec<&LoadedLine>
  ↓ (extract text)
lines.map(|l| l.line().str().as_bytes())
  ↓ (syntax highlighting)
tree_sitter_highlight()
  ↓ (draw to terminal)
ratatui::Frame::render_widget()
```

**Critical Path**:
- **iter_at()** must be fast (called every frame, 60fps target)
- **get/get_mut()** must be fast (called on every keystroke)
- **next_line/prev_line()** must be fast (called when navigating)

## Recommended Implementation Plan

### Phase 1: Minimal Working Editor (No ChunkTree Yet)
- Revert VirtualFile to simple Vec<EditLine> in memory
- Get main.rs working end-to-end
- Support files up to ~100MB
- Establish testing/debugging workflow

### Phase 2: Add Chunk Loading (Still Vec-based)
- Add memstore caching layer
- Load file in chunks on-demand
- Maintain Vec<LoadedLine> but with lazy loading
- Support files up to ~1GB

### Phase 3: Introduce ChunkTree (Final Architecture)
- Replace Vec<EditLine> storage with ChunkTree
- Keep line_starts index for fast line access
- Add edit operations that modify ChunkTree
- Full large file support (10GB+)

### Phase 4: Polish
- Undo/redo using ChunkTree's persistent structure
- Multi-file support
- Search/replace
- Save-to-disk functionality

## Next Steps

**Question for you**: Which approach do you prefer?

A. **Quick Win**: Start with Phase 1 (simple Vec-based editor) to get something working, then iterate
B. **Do It Right**: Complete the ChunkTree refactoring now with the hybrid approach
C. **Different approach**: You have another idea?

Once we decide, I can:
1. Create a task list breaking down the work
2. Start implementing the chosen architecture
3. Get the editor running with basic functionality
