# Composite Buffer Architecture

**Status**: Proposed
**Date**: 2025-12-22
**Context**: Architectural design for high-performance, multi-source views within Fresh Editor. This architecture powers the "Review Diff" tool and the "Notebook" feature.

---

## 1. Executive Summary

Traditional editor buffers are single-source: they map 1:1 to a file or a static string. The **Composite Buffer Architecture** introduces a "Virtual Lens" concept, where a single buffer surface is synthesized from multiple **Source Buffers**.

This architecture enables:
1.  **Review Diff**: A vertical stream of hunks from different files, with live syntax highlighting and direct editing.
2.  **Notebooks**: A sequence of cells (views into separate buffers) with professional box-drawing borders and execution status.
3.  **Cross-Buffer Consistency**: Edits made in a composite view are instantly reflected in the source files, and vice-versa.

---

## 2. Core Concepts

### 2.1 The Source Buffer
Any standard buffer (file-backed or virtual) that holds raw text and runs a syntax highlighting engine.

### 2.2 The Composite Buffer
A "Virtual Lens" buffer that contains no primary text of its own. Its content is defined by a **Composite Layout Script**—a sequence of sections provided by a plugin.

### 2.3 The Section Descriptor
A directive telling the Rust core what to pull and how to frame it:
```rust
struct SectionDescriptor {
    id: String,               // Unique ID for the section
    source_buffer_id: BufferId,
    range: Range<usize>,      // Byte or line range in the source
    style: SectionStyle,      // Border type, markers (+/-), padding
    heading: Option<String>,  // Header text (e.g., filename or "In [5]:")
    is_editable: bool,        // Whether to allow input routing
    metadata: serde_json::Value,
}
```

---

## 3. The Synthesis Pipeline

The rendering pipeline (`view_pipeline.rs`) is extended to support multi-source token synthesis.

### 3.1 Token Fetching
Instead of reading raw bytes and tokenizing them, the Composite Buffer renderer:
1.  Iterates through the layout's `SectionDescriptors`.
2.  For each section, it requests the **already computed tokens** (from the Source Buffer's HighlightEngine).
3.  It translates the source offsets into composite offsets.

### 3.2 Framing (The "Box Engine")
The synthesis engine injects UI-only tokens to create the visual "Frame":
*   **Borders**: Box-drawing characters (`┌`, `│`, `└`) are added as tokens with `source_offset: None`.
*   **Markers**: Diff indicators (`+`, `-`) or Notebook markers (`In [ ]`) are injected into a dedicated gutter column.
*   **Styling**: Framing tokens use specific UI theme colors, while content tokens preserve their original syntax colors.

---

## 4. Live Editing & Input Routing

This is the most advanced part of the architecture. It allows the user to treat a composite box as a real editor.

### 4.1 Coordinate Mapping
The `ViewLine` structure is updated to provide a bidirectional mapping between the **Composite Viewport** and the **Source Buffer**.
*   **Display Column 10** → maps to **Source Buffer 5, Byte Offset 120**.
*   **Display Column 2** (Border) → maps to **None** (Protected).

### 4.2 Event Redirection
When a key is pressed in a Composite split:
1.  The editor identifies the buffer/byte under the cursor via the Mapping Table.
2.  If the mapping exists and `is_editable` is true:
    *   The `Insert` or `Delete` event is **rerouted** to the Source Buffer.
    *   The Source Buffer applies the change and notifies its listeners.
3.  If the cursor is on a protected character (border/header), the input is blocked ("Editing disabled").

### 4.3 Unified Undo/Redo
Edits made via the composite view are recorded in the **Source Buffer's Event Log**. This ensures that undoing a change in the primary file correctly updates the composite view as well.

---

## 5. Feature Implementation Details

### 5.1 Review Diff (Professional TUI)
*   **Layout**: A sequence of sections pairing `Git HEAD` (readonly) and `Working Copy` (editable).
*   **Visuals**: Single-line borders connecting hunks of the same file. `+` / `-` markers in a narrow gutter.
*   **Refresh**: The plugin updates the layout script when `git diff` changes.

### 5.2 Jupyter Notebooks
*   **Layout**: Code cells (editable) followed by Output cells (readonly).
*   **Visuals**: Rounded borders, distinct spacing between cells. `In [x]:` markers.
*   **LSP Support**: Since input is routed to real source buffers, the language's LSP provides full completion and diagnostics within the notebook box.

---

## 6. Implementation Roadmap

1.  **Phase 1: Multi-Source Pipeline**: Update `ViewTokenWire` and `ViewLineIterator` to track `BufferId` per character.
2.  **Phase 2: Layout Script API**: Add `editor.setCompositeLayout` to allow plugins to register section descriptors.
3.  **Phase 3: The Box Engine**: Implement Rust-side synthesis of borders and gutters.
4.  **Phase 4: Input Routing**: Update `Editor::handle_key` to support rerouting based on character mappings.
