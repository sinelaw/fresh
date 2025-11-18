# Fresh Architecture

Fresh is a high-performance, terminal-based text editor built in Rust. It's designed to be fast, responsive, and extensible, with a modern architecture that draws inspiration from the best ideas in the world of text editors.

## Core Design Principles

*   **Performance First:** Every architectural decision is made with performance in mind. This includes the choice of data structures, the design of the rendering pipeline, and the implementation of core features.
*   **Event-Driven:** All state changes are represented as events, which are processed by a central event loop. This makes the editor's state predictable and enables features like unlimited undo/redo.
*   **Asynchronous I/O:** All file and process I/O is handled asynchronously on a separate thread pool. This ensures that the editor's UI is never blocked by slow I/O operations.
*   **Extensible:** A powerful TypeScript-based plugin system allows for deep customization and extension of the editor's functionality.

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

## The Document Model

To provide a clean separation between the editor's UI and the underlying text buffer, Fresh uses a `DocumentModel` trait. This abstraction layer is responsible for all interactions with the text buffer and provides a consistent API for both small and large files.

### Dual Position System

To support multi-gigabyte files where line indexing may be unavailable, the `DocumentModel` uses a dual position system:

*   **`DocumentPosition::LineColumn`:** For small files, this provides precise line and column-based positioning.
*   **`DocumentPosition::ByteOffset`:** For large files, this provides byte-offset-based positioning, which is always available and precise.

### The `DocumentModel` Trait

The `DocumentModel` trait defines a set of methods for interacting with the document, including:

*   **`get_viewport_content`:** The core rendering primitive, which returns the content for the visible portion of the screen.
*   **`position_to_offset` and `offset_to_position`:** For converting between the two position types.
*   **`insert`, `delete`, and `replace`:** For modifying the document's content.

This abstraction allows the rest of the editor to be blissfully unaware of the details of the underlying text buffer, such as whether it's a small file with a full line index or a large file with lazy loading.

## The Buffer

The core of the editor is the text buffer, which is implemented as a **`PieceTree`**. A `PieceTree` is a balanced binary tree that represents the text as a sequence of "pieces," which are references to either the original, immutable file buffer or an in-memory buffer of user additions.

This data structure provides several key advantages:

*   **O(log n) Edits:** Inserts and deletes are O(log n), where n is the number of pieces. This makes text editing extremely fast, even in large files.
*   **Efficient Memory Usage:** The `PieceTree` only stores the changes to the file, not the entire file content. This makes it very memory-efficient, especially for large files.
*   **Lazy Loading:** For multi-gigabyte files, Fresh uses a lazy loading strategy. The file is not loaded into memory all at once. Instead, chunks of the file are loaded on demand as the user scrolls through the file.

## The Rendering Pipeline

The rendering pipeline is designed to be as efficient as possible, especially when it comes to overlays (visual decorations like highlights and squiggly lines).

### The Overlay Problem

A naive implementation of overlays, where each character on the screen is checked against a list of all active overlays, can lead to significant performance problems. This is an O(N*M) problem, where N is the number of characters on the screen and M is the number of overlays.

### The Solution: A High-Performance Overlay System

Fresh uses a multi-pronged approach to solve the overlay problem:

1.  **Line-Indexed Overlay Storage:** Instead of storing overlays in a flat list, they are stored in a `BTreeMap<usize, Vec<Overlay>>`, where the key is the starting line number of the overlay. This allows for a very fast lookup of all overlays on a given line.
2.  **Render-Time Overlay Cache:** During each frame, the editor creates a cache of all overlays that are visible in the current viewport. This cache is then used to apply the overlays to the text, avoiding the need to query the overlay manager for each character.
3.  **Diagnostic Hash Check:** For LSP diagnostics, which are a major source of overlays, Fresh uses a hash check to avoid redundant updates. If the set of diagnostics from the language server hasn't changed, no work is done.

This new architecture, which is currently being implemented, will provide a massive performance improvement over the old system and will allow Fresh to handle thousands of overlays without breaking a sweat.

## LSP Integration

Fresh has a deep and robust integration with the Language Server Protocol (LSP), providing features like code completion, diagnostics, and go-to-definition.

The LSP integration is built on a multi-threaded architecture that ensures the editor's UI is never blocked by the language server.

*   **`LspManager`:** A central coordinator that manages multiple language servers (one for each language).
*   **`LspHandle`:** A handle to a specific language server, providing a non-blocking API for sending commands and notifications.
*   **`AsyncBridge`:** An `mpsc` channel that bridges the asynchronous world of the LSP tasks with the synchronous world of the editor's main event loop.

This architecture allows Fresh to communicate with language servers in a highly efficient and non-blocking way, providing a smooth and responsive user experience.