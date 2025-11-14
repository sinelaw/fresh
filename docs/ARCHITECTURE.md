# Architecture

Fresh is a high-performance terminal text editor with async I/O and event-driven design.

## Core Design Principles

1. **Event-driven architecture** - All state changes go through an event log (lossless history)
2. **Hybrid async/sync** - Main loop is synchronous (60fps), I/O runs in Tokio tasks
3. **Pluggable backends** - Filesystem, persistence, and LSP servers are abstracted
4. **Emacs-style primitives** - Overlays and popups for flexible, reusable UI components

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   MAIN THREAD (Sync)                   │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────┐  │
│  │ Event Loop   │→ │  Editor      │→ │  Renderer   │  │
│  │ (crossterm)  │  │  (state)     │  │  (ratatui)  │  │
│  └──────────────┘  └──────────────┘  └─────────────┘  │
│         ↓                 ↑                              │
│    Input Queue      EventQueue (mpsc)                   │
└─────────────────────────────────────────────────────────┘
         ↑                      ↑
         │ send events          │ send messages
         │                      │
    ┌────┴──────────┐  ┌────────┴──────────┐
    │ LSP Tasks     │  │ File I/O Tasks    │
    │ (Tokio)       │  │ (Tokio)           │
    └───────────────┘  └───────────────────┘
```

## Key Components

### Buffer Layer
- **`PersistenceLayer` Trait** - An abstraction that decouples the `VirtualBuffer` from its underlying storage backend. This allows the buffer to be backed by different storage mechanisms.
- **`ChunkTree`** - A persistent, rope-like data structure that holds the buffer's content in memory. It provides efficient O(log n) insert and delete operations, making it ideal for text editing. It is the primary implementation of the `PersistenceLayer` via the `ChunkTreePersistence` struct.
- **`VirtualBuffer`** - The high-level, thread-safe interface for the buffer's content. It manages access to the underlying `PersistenceLayer`.
- **`Cache`** - A size-limited, LRU read-through cache for the `VirtualBuffer`. It stores the materialized results of reads from the `PersistenceLayer`. This is a key performance optimization, as it avoids the cost of repeatedly traversing the `ChunkTree` for the same data region. Edits to the buffer clear the cache to prevent stale reads.
- **`LineCache`** - A BTreeMap for O(log n) byte-to-line conversion, used for quickly navigating between byte offsets and line numbers.
- **`EditLog`** - Event history with garbage collection based on active iterator versions.

### UI Layer
- **Overlay System** - Text decorations (underlines, colors) with z-ordering
- **Popup System** - Floating windows for completions, hover, etc.
- **Split View** - Nested horizontal/vertical splits with tree structure
- **Viewport** - Efficient rendering of visible regions only

### LSP Integration
- **LspManager** - One async handle per language server
- **AsyncBridge** - mpsc channel bridging Tokio tasks and main loop
- **Diagnostics** - Async notifications rendered via overlay system
- **Completion/GoToDef** - Request/response with timeout handling

### File System
- **FileTree** - Lazy-loading tree with path-to-node HashMap (O(1) lookup)
- **FsManager** - Request deduplication, batching, LRU metadata cache
- **IgnorePatterns** - Gitignore support using `ignore` crate

## Architectural Decisions and Trade-offs

### Buffer Implementation

The editor uses a **piece table** (`PieceTree`) for its core text buffer, which is a deliberate choice for achieving high performance in text editing operations. The `PieceTree` is a balanced binary tree that tracks text as a sequence of "pieces," which are references to either the original, immutable file buffer (`Stored`) or an in-memory buffer of user additions (`Added`).

**Key characteristics of this design:**

*   **O(log n) Complexity:** All fundamental operations like inserts, deletes, and offset-based lookups have a time complexity of O(log n), where n is the number of pieces. This ensures that performance degrades gracefully as the number of edits increases.
*   **Separation of Concerns:** The `PieceTree` is only concerned with byte offsets. Line and column tracking is handled by a separate `LineIndex` module. This separation simplifies the `PieceTree` implementation, as its nodes do not need to store and update line-based metadata. While this means `LineIndex` updates can be O(n) in the worst case, this is an acceptable trade-off for the simplicity and robustness it provides.
*   **Efficiency through API Design:** The `PieceTree` API is designed to minimize redundant tree traversals. For example, instead of converting a line/column position to a byte offset and then performing an insert (two traversals), the API provides methods like `insert_at_position` that perform both actions in a single traversal. Similarly, the `iter_pieces_in_range` method allows for efficient iteration over a range of pieces without repeated lookups.

This design was chosen over simpler data structures like a gap buffer or a simple `Vec<u8>` because it provides a more robust and scalable foundation for advanced features like multi-cursor editing, non-linear undo/redo, and efficient handling of large files.

### Large File Support

To handle multi-gigabyte files efficiently, the editor employs a **lazy loading** strategy with **optional line indexing**.

*   **Fast Startup:** When a large file is opened (determined by a configurable threshold, e.g., 100MB), the editor does not load the entire file into memory. Instead, it creates an "unloaded" buffer that only stores metadata about the file. This results in near-instantaneous opening times, regardless of file size.
*   **On-Demand Loading:** File content is loaded in chunks (e.g., 1MB) only when a specific region of the file is accessed, for example, by scrolling or jumping to a position. The `PieceTree` is then updated to reflect the loaded regions, splitting pieces as necessary.
*   **Optional Line Indexing:** For large files, the editor disables line indexing by default to conserve memory. Without a line index, line-based navigation is performed using a constant approximation of line length, followed by a local search. This provides a good balance between performance and memory usage.
*   **Graceful Degradation:** While line-based operations may be slower in large files due to the lack of a full line index, byte-based operations remain as efficient as ever. This allows the editor to remain responsive and usable even with very large files.

This approach allows the editor to open and edit files that are much larger than the available RAM, a critical feature for a high-performance text editor.

### Search

The search functionality is designed to be both efficient and correct, especially for large files. It uses a **chunked search** strategy with a crucial **overlap** mechanism.

*   **Chunked Processing:** Instead of loading the entire file into memory for a search, the editor processes the file in chunks (e.g., 64KB for literal search, 1MB for regex search).
*   **Overlap Strategy:** To avoid missing matches that span the boundaries of these chunks, each new chunk includes a small overlap from the end of the previous chunk. The size of the overlap is determined by the length of the search pattern (for literal search) or a fixed size (for regex search).
*   **Valid Zone:** To prevent duplicate matches, a "valid zone" is defined for each chunk. A match is only reported if it ends within the valid zone, ensuring that matches spanning boundaries are counted exactly once.
*   **Streaming from Pieces:** The search implementation is built on top of the `iter_pieces_in_range` method of the `PieceTree`, which allows it to stream data from the underlying pieces without materializing the entire chunk in memory upfront. This makes the search process highly memory-efficient.

This design ensures that search is both fast and correct, even in the largest files, while minimizing memory overhead.

### Line Numbers

Accurate and efficient line number management is critical for a text editor. The editor's design has evolved to address several inefficiencies in this area.

*   **Centralized `LineIndex`:** The `LineIndex` module is the single source of truth for all line number and position conversions. It maintains a sorted list of line start offsets, allowing for O(log n) conversion between byte offsets and line/column positions.
*   **Elimination of Redundant Lookups:** The previous implementation suffered from redundant lookups, where the same line number or position would be calculated multiple times within a single frame. The current design emphasizes caching these values and using the `LineIndex` as efficiently as possible.
*   **Direct Lookups:** Instead of iterating through the buffer to count lines, the editor now uses direct lookups in the `LineIndex` to get the total line count or the byte offset of a specific line. This is significantly more performant, especially for large files.

By centralizing line number management in the `LineIndex` and using it efficiently, the editor avoids the performance pitfalls of manual line counting and redundant calculations, ensuring a smooth and responsive user experience.

### Overlays

Overlays (visual decorations like highlights, underlines, and diagnostic markers) are implemented using a **marker-based system**. This design addresses the common problem of overlays becoming "stale" or "sliding around" when text is inserted or deleted.

*   **Self-Adjusting Markers:** Instead of being defined by absolute byte positions, overlays are anchored to **markers**, which are self-adjusting position trackers. When text is inserted or deleted, the markers automatically update their positions, ensuring that the overlays stay correctly anchored to the content.
*   **Gap-Based Marker List:** The markers are managed in a **gap-based marker list**. This data structure stores markers sequentially with "gaps" (byte counts) between them. When text is edited, only a single gap size needs to be updated, making the process highly efficient.
*   **Efficient Rendering:** This approach also allows for highly efficient rendering. By iterating through the markers and the buffer content simultaneously, the editor can apply overlay styles without performing any range checks per character, which significantly improves rendering performance.

The gap-based marker list was chosen over simpler alternatives like a HashMap of markers because it optimizes for the most common operations: edits and rendering. This design provides a robust and performant foundation for a wide range of features, from syntax highlighting to inline diagnostics.

## Unusual/Interesting Design Choices

### 1. Iterator Edit Resilience
Iterators automatically adjust their position when the buffer is edited. Uses two-level caching (ChunkTree snapshot + 4KB buffer) to achieve ~4096x fewer locks per iteration.

### 2. Gap Support in Rope
ChunkTree supports sparse operations - inserting beyond EOF creates gaps efficiently without allocating space.

### 3. Read-Through Caching for In-Memory Rope
While the `ChunkTree` (rope) is an in-memory data structure, reading from it requires traversing its tree structure to assemble contiguous byte ranges. To optimize this, `VirtualBuffer` uses a read-through LRU cache (`cache.rs`). This cache stores the materialized results of `ChunkTree` reads, turning subsequent accesses for the same region into a fast hash map lookup. This memoization strategy provides a significant performance boost for read operations.

### 4. Viewport-Only Parsing
Syntax highlighting only parses ~50 visible lines at a time, allowing instant load of 1GB+ files.

### 5. Message-Passing Concurrency
No locks in main loop. All async operations communicate via non-blocking mpsc channels checked once per frame.

### 6. Edit Log Garbage Collection
Active iterator versions tracked in BTreeSet. After each edit, find minimum version (low-water mark) and prune older edits.

## Performance Characteristics

- **Insert/Delete**: O(log n) via ChunkTree
- **Line lookup**: O(log n) via LineCache BTreeMap
- **Path lookup**: O(1) via FileTree HashMap
- **Iterator**: O(n/4096) lock operations via 4KB chunk caching
- **Large files**: Instant load via lazy highlighting (viewport only)
- **UI**: ~60fps (16ms polling) with non-blocking I/O

## Points for Clarification
- The architecture is designed to support multiple `PersistenceLayer` backends. While the primary one used for editing is the in-memory `ChunkTreePersistence`, the exact mechanism for loading from and saving to disk, and how a disk-based persistence layer might be used in the application, is not fully detailed here.

## Testing

- **Unit tests** - Core data structures and operations
- **E2E tests** - Full integration via virtual terminal (EditorTestHarness)
- **Property tests** - Invariants and round-trip properties
- **Hermetic tests** - Each test gets isolated temp directory

See [TESTING.md](TESTING.md) for full testing strategy.
