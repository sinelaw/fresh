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

- **165 unit tests** - Core data structures and operations
- **59 E2E tests** - Full integration via virtual terminal (EditorTestHarness)
- **Property tests** - Invariants and round-trip properties
- **Hermetic tests** - Each test gets isolated temp directory

See [TESTING.md](TESTING.md) for full testing strategy.
