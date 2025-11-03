# Editor - Technical Summary

## Concise Summary

**Rust-based terminal text editor for massive files using persistent immutable rope tree (`ChunkTree<'a>`) with Arc-shared nodes, gaps for sparse ops, O(log n) insert/remove. Lazy-loading virtual file system (`VirtualFile`) chunks data on-demand via trait-abstracted `Memstore` cache. TUI with ratatui/crossterm, tree-sitter syntax highlighting (currently disabled/WIP). ChunkTree extensively tested (combinatorial), configurable chunking/branching. Recent dev: fill() operation, node lifecycle. CI: fmt/clippy/doc/cross-platform tests. Unix FileExt I/O, edition 2021, no-std incompatible.**

---

## Detailed Technical Summary

### Project Overview
**Editor** is a terminal-based text editor written in Rust, designed to efficiently handle extremely large files that don't fit in memory. The project is in active development (evidenced by recent WIP commits) and focuses on memory-efficient text manipulation through novel data structures.

### Core Architecture

#### 1. **Rope-like Data Structure (`chunk_tree.rs`)** - 1,182 lines
The cornerstone of the project is `ChunkTree`, a persistent, immutable tree-based data structure inspired by ropes:

- **Structure**: Tree nodes can be:
  - `Leaf`: Contains byte slices (≤ chunk_size)
  - `Gap`: Efficient sparse representation of empty space
  - `Internal`: Contains multiple children (≤ max_children)

- **Key Features**:
  - Immutable/persistent with structural sharing via `Arc<T>`
  - Configurable via `ChunkTreeConfig` (chunk_size, max_children)
  - Sparse operations: inserting beyond length creates gaps
  - Operations: insert, remove, fill (fills gaps with data), get, collect_bytes

- **Performance**: O(log n) insert/remove, O(1) length, O(n) collection
- **Testing**: Comprehensive test suite (650+ lines) with exhaustive combinatorial tests

#### 2. **Virtual File System (`virtual_file.rs`)** - 566 lines
Provides line-based file access with lazy loading:

- **Purpose**: Edit files larger than available memory
- **Architecture**:
  - Chunks loaded on-demand from backing storage (filesystem)
  - `LineCursor`: Opaque handle for line navigation
  - `Memstore`: Caching layer for file chunks
  - Supports seek operations (Start, End, Current)

- **Operations**:
  - `seek()`: Load chunks at specific offsets
  - `next_line()`, `prev_line()`: Navigate with automatic chunk loading
  - `insert_after()`, `remove()`: Edit operations
  - `iter_at()`: Iterator interface for line ranges

- **Design Trade-offs**:
  - Tolerates high-latency storage (100ms-few seconds)
  - Assumes exclusive file access
  - Trades completeness for reduced storage access

#### 3. **Memory Store (`memstore.rs`)** - 171 lines
Generic caching layer with `LoadStore` trait:

- **Abstraction**: Separates storage backend from caching logic
- **Implementation**: `FileLoadStore` for Unix file I/O via `FileExt`
- **Chunk Management**: HashMap-based cache with dirty tracking
- **Methods**: `get()` (load on miss), `store_all()` (persist dirty chunks)

#### 4. **Text Line Representation (`lines.rs`)** - 56 lines
Simple wrapper around `String` for line editing:

- **Purpose**: Abstraction for character-level operations
- **Operations**: insert, remove, overwrite, split_off, extend
- **Interface**: Provides character-based indexing

#### 5. **Logging Infrastructure (`logs.rs`)** - 34 lines
Conditional logging with compile-time switching:

- **Test mode**: Logs to stdout via `println!`
- **Runtime mode**: Logs to `/tmp/editor.log` with lazy file initialization
- **Thread-safe**: Uses `Mutex<LogState>` for shared file handle

#### 6. **Main Application (`main.rs`)** - 742 lines (mostly commented out)
TUI text editor built with `ratatui` and `crossterm`:

- **State Management**:
  - `VirtualFile` for content
  - Cursor position, window offset, insert/overwrite mode
  - Terminal size tracking

- **Features**:
  - Syntax highlighting via tree-sitter (Rust language support)
  - Keyboard navigation (arrows, Home/End, PageUp/PageDown)
  - Character insert/overwrite/delete
  - Line operations (split with Enter, join with Backspace/Delete)
  - Word-based navigation (Ctrl+Left/Right)
  - Scrolling (Ctrl+Up/Down)
  - Status bar with cursor position

- **Status**: Largely commented out, WIP implementation

### Dependencies

**Runtime**:
- `crossterm` (0.28.1): Cross-platform terminal manipulation
- `ratatui` (0.29.0): TUI framework
- `tree-sitter-highlight` (0.24.7): Syntax highlighting engine
- `tree-sitter-rust` (0.23.2): Rust language grammar
- `tempfile` (3.15.0): Testing utilities

### Build & Quality Assurance

**CI/CD** (GitHub Actions):
- **Formatting**: `cargo fmt --check`
- **Linting**: `clippy` with automatic PR annotations
- **Documentation**: Generated on nightly with unstable features
- **Testing**: Cross-platform (macOS, Windows) with locked dependencies
- **Optimizations**: Cargo caching, concurrency control

**Configuration**: Rust 2021 edition, no external configuration files

### Current Development State

Based on git history:
- Recent focus on `ChunkTree.fill()` operation (commits: "fix tree.fill()", "Fixed fill", ".fill()")
- Working on node management (".get()", "drop single-node after remove")
- Multiple WIP commits indicate active development
- Main editor UI is disabled (commented out in main.rs)

### Key Design Patterns

1. **Persistent Data Structures**: ChunkTree creates new nodes on modification, sharing unchanged subtrees
2. **Lazy Loading**: VirtualFile loads file chunks on-demand
3. **Trait-based Abstraction**: `LoadStore` trait for storage backends
4. **Iterator Pattern**: ChunkTree provides depth-first iteration
5. **Builder Pattern**: ChunkTreeConfig for configuration

### Technical Constraints

- **Unix-specific**: Uses `std::os::unix::fs::FileExt` for positioned I/O
- **UTF-8 assumption**: Line parsing assumes valid UTF-8
- **Memory trade-offs**: Structural sharing via Arc increases memory overhead per version
- **Chunk boundaries**: Lines spanning multiple chunks may be incomplete

### File Structure

```
editor/
├── src/
│   ├── main.rs              (742 lines, TUI editor - mostly commented)
│   ├── chunk_tree.rs        (1,182 lines, persistent rope tree)
│   ├── virtual_file.rs      (566 lines, lazy file loading)
│   ├── memstore.rs          (171 lines, chunk caching)
│   ├── lines.rs             (56 lines, line abstraction)
│   └── logs.rs              (34 lines, logging macros)
├── Cargo.toml               (dependencies & metadata)
├── Cargo.lock               (locked dependencies)
└── .github/workflows/ci.yml (CI pipeline)
```

### Architecture Diagram

```
┌─────────────────────────────────────────────────┐
│             main.rs (TUI Layer)                 │
│  ┌───────────────────────────────────────────┐  │
│  │ ratatui + crossterm + tree-sitter         │  │
│  └───────────────┬───────────────────────────┘  │
└────────────────┬─┴──────────────────────────────┘
                 │
         ┌───────▼───────────┐
         │  virtual_file.rs  │ (Line-based API)
         │  ┌──────────────┐ │
         │  │ LineCursor   │ │
         │  │ LoadedLine   │ │
         │  └──────┬───────┘ │
         └─────────┼─────────┘
                   │
         ┌─────────▼─────────┐
         │   memstore.rs     │ (Chunk Cache)
         │  ┌──────────────┐ │
         │  │ LoadStore    │ │ trait
         │  │ trait impl   │ │
         │  └──────┬───────┘ │
         └─────────┼─────────┘
                   │
         ┌─────────▼─────────┐
         │  chunk_tree.rs    │ (Data Structure)
         │  ┌──────────────┐ │
         │  │ ChunkTree    │ │
         │  │ (persistent) │ │
         │  └──────────────┘ │
         └───────────────────┘
                   │
         ┌─────────▼─────────┐
         │   File System     │
         └───────────────────┘
```

### Key Algorithms

**ChunkTree Insert (chunk_tree.rs:284-352)**:
1. Validate index ≤ length, data non-empty
2. For leaf: split at index, create internal with 3 children
3. For gap: insert creates internal with surrounding gaps
4. For internal: find child containing index, recursively insert
5. Rebalance if children > max_children (split into two nodes)

**ChunkTree Remove (chunk_tree.rs:358-433)**:
1. Validate range within bounds, non-empty
2. For leaf/gap: create internal with remaining parts
3. For internal: iterate children, recursively remove overlapping ranges
4. Optimize: collapse single-child internals

**ChunkTree Fill (chunk_tree.rs:214-278)**:
1. Fills gaps with actual data without growing tree
2. For gap: split into prefix-gap, data, suffix-gap
3. For internal: recursively fill overlapping children
4. Maintains total tree size

**VirtualFile Seek (virtual_file.rs:150-185)**:
1. Resolve offset from SeekFrom
2. Load chunk at offset from memstore
3. Update line anchor to nearest line ≥ offset
4. Return LineCursor with current offset_version

### Testing Strategy

- **Unit tests**: Each module has dedicated test suite
- **Combinatorial testing**: ChunkTree tests all insert/remove positions and lengths
- **Property testing**: Validates invariants across parameter ranges
- **Integration tests**: VirtualFile tests with real temporary files
- **Cross-platform CI**: macOS and Windows validation

### Performance Characteristics

**ChunkTree**:
- Insert: O(log n) tree traversal + O(k) data copy
- Remove: O(log n) tree traversal
- Get: O(log n) traversal to target node
- Collect: O(n) full tree traversal
- Space: O(log n) nodes per version via structural sharing

**VirtualFile**:
- Seek: O(1) if chunk cached, O(chunk_size) if loading
- Next/Prev line: O(1) if cached, O(chunk_size) if loading chunk
- Line operations: O(1) + ChunkTree operations

**Memstore**:
- Get: O(1) HashMap lookup + O(chunk_size) load on miss
- Store: O(n) where n = number of dirty chunks
