# Architecture

A high-performance terminal text editor with async I/O and event-driven design.

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
- **ChunkTree** - Persistent rope-like structure with O(log n) operations and gap support
- **VirtualBuffer** - Arc-based shared buffer with edit tracking and automatic iterator adjustment
- **LineCache** - BTreeMap for O(log n) byte-to-line conversion
- **EditLog** - Event history with garbage collection based on active iterator versions

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

### 3. Viewport-Only Parsing
Syntax highlighting only parses ~50 visible lines at a time, allowing instant load of 1GB+ files.

### 4. Message-Passing Concurrency
No locks in main loop. All async operations communicate via non-blocking mpsc channels checked once per frame.

### 5. Edit Log Garbage Collection
Active iterator versions tracked in BTreeSet. After each edit, find minimum version (low-water mark) and prune older edits.

## Performance Characteristics

- **Insert/Delete**: O(log n) via ChunkTree
- **Line lookup**: O(log n) via LineCache BTreeMap
- **Path lookup**: O(1) via FileTree HashMap
- **Iterator**: O(n/4096) lock operations via 4KB chunk caching
- **Large files**: Instant load via lazy highlighting (viewport only)
- **UI**: ~60fps (16ms polling) with non-blocking I/O

## Testing

- **165 unit tests** - Core data structures and operations
- **59 E2E tests** - Full integration via virtual terminal (EditorTestHarness)
- **Property tests** - Invariants and round-trip properties
- **Hermetic tests** - Each test gets isolated temp directory

See [TESTING.md](TESTING.md) for full testing strategy.
