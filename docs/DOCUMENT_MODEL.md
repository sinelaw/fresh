# Document Model Architecture

## Overview

This document describes the architectural design for Fresh's document model layer, which provides a clean abstraction between the editor's rendering/editing operations and the underlying text buffer implementation. The design is inspired by VSCode's proven layering approach but adapted to support **huge files** with lazy loading and dual coordinate systems.

## Motivation

### Problems with Current Architecture

1. **No Clear Layering**: Rendering code directly accesses `TextBuffer` methods, creating tight coupling between the view layer and storage layer.

2. **`slice()` is a Leaky Abstraction**: The `slice()` method exposes byte-level details to all callers and returns empty strings when data is unavailable, making it impossible to distinguish "no data loaded" from "empty range".

3. **Lazy Loading Conflicts**:
   - `slice()` needs `&self` (immutable) for rendering
   - Loading chunks needs `&mut self` (mutable)
   - Result: blank screens when data isn't loaded

4. **Single Responsibility Violation**: `TextBuffer` tries to be both a low-level data structure and a high-level document model.

### VSCode's Architecture

VSCode uses a 3-layer architecture:

```
┌─────────────────────────────────────┐
│  View/Editor Layer                  │
│  (rendering, user interaction)      │
└────────────┬────────────────────────┘
             │ Uses ITextModel interface
             ▼
┌─────────────────────────────────────┐
│  ITextModel (Document Model)        │
│  - getLineContent(lineNumber)       │
│  - getValueInRange(range, eol)      │
│  - Coordinates tokenization, etc.   │
└────────────┬────────────────────────┘
             │ Owns ITextBuffer
             ▼
┌─────────────────────────────────────┐
│  ITextBuffer (Piece Tree)           │
│  - @internal - implementation detail│
│  - Position/offset conversion       │
│  - Low-level tree operations        │
└─────────────────────────────────────┘
```

**Key Insights:**
- **No `slice()` method exists** - VSCode uses line-oriented APIs
- `getLineContent(lineNumber)` is the primary rendering primitive
- `PieceTreeTextBuffer` is marked `@internal` - never exposed
- Everything is line-oriented, not byte-oriented

**VSCode's Large File Limitation:**
- Threshold: 20MB file size OR 300K lines
- Strategy: Multiple StringBuffers (avoids V8's 256MB limit)
- **But they still load everything into memory** - no lazy loading

## Fresh's Enhanced Architecture

### Design Goals

1. Support **huge files** (multi-GB) with lazy loading
2. Support both **line-based** and **byte-based** coordinate systems
3. Provide clean abstraction boundaries
4. Make lazy loading transparent to rendering code
5. Handle errors explicitly (no silent failures)

### Dual Position System

For huge files, line indexing may be unavailable or approximate. We need two parallel coordinate systems:

1. **Line-based positions** - For small files with precise line indexing
2. **Byte-based positions** - For huge files, always precise

```rust
/// Position in a document - can be line-based or byte-based
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentPosition {
    /// Line and column (1-indexed line, 0-indexed column in bytes)
    /// Only available when line indexing is enabled
    LineColumn { line: usize, column: usize },

    /// Byte offset from start of file
    /// Always available, even for huge files
    ByteOffset(usize),
}

impl DocumentPosition {
    /// Create a line/column position
    pub fn line_col(line: usize, column: usize) -> Self {
        DocumentPosition::LineColumn { line, column }
    }

    /// Create a byte offset position
    pub fn byte(offset: usize) -> Self {
        DocumentPosition::ByteOffset(offset)
    }
}
```

### Document Capabilities

```rust
/// Information about a document's capabilities
#[derive(Debug, Clone, Copy)]
pub struct DocumentCapabilities {
    /// Whether precise line indexing is available
    pub has_line_index: bool,

    /// Whether the document is using lazy loading
    pub uses_lazy_loading: bool,

    /// Total byte size (always known)
    pub byte_length: usize,

    /// Approximate line count (may be estimated for huge files)
    pub approximate_line_count: usize,
}
```

### DocumentModel Trait

The core abstraction that all editor operations use:

```rust
/// High-level document interface supporting both line and byte operations
pub trait DocumentModel {
    // ===== Capability Queries =====

    /// Get document capabilities
    fn capabilities(&self) -> DocumentCapabilities;

    /// Check if line indexing is available
    fn has_line_index(&self) -> bool {
        self.capabilities().has_line_index
    }

    // ===== Position Queries =====

    /// Get content at a viewport (the core rendering primitive)
    /// Returns lines starting from position, up to max_lines
    /// This works for both line-based and byte-based positions
    fn get_viewport_content(
        &self,
        start_pos: DocumentPosition,
        max_lines: usize,
    ) -> Result<ViewportContent>;

    /// Convert position to byte offset (always works)
    fn position_to_offset(&self, pos: DocumentPosition) -> Result<usize>;

    /// Convert byte offset to a position
    /// For huge files without line index, returns ByteOffset
    /// For small files, returns LineColumn
    fn offset_to_position(&self, offset: usize) -> DocumentPosition;

    // ===== Content Access =====

    /// Get a range of text by positions
    fn get_range(&self, start: DocumentPosition, end: DocumentPosition) -> Result<String>;

    /// Get a single line if line indexing is available
    /// Returns None if line indexing is not available
    fn get_line_content(&self, line_number: usize) -> Option<String>;

    /// Get text around a byte offset (for operations that don't need exact lines)
    /// Returns (offset, content) where offset is the start of returned content
    fn get_chunk_at_offset(&self, offset: usize, size: usize) -> Result<(usize, String)>;

    // ===== Editing Operations =====

    /// Insert text at a position
    fn insert(&mut self, pos: DocumentPosition, text: &str) -> Result<usize>;

    /// Delete a range
    fn delete(&mut self, start: DocumentPosition, end: DocumentPosition) -> Result<()>;

    /// Replace a range
    fn replace(
        &mut self,
        start: DocumentPosition,
        end: DocumentPosition,
        text: &str,
    ) -> Result<()>;

    // ===== Search Operations =====

    /// Find all matches of a pattern in a range
    /// Returns byte offsets (always precise)
    fn find_matches(
        &self,
        pattern: &str,
        search_range: Option<(DocumentPosition, DocumentPosition)>,
    ) -> Result<Vec<usize>>;
}
```

### Viewport Content Types

```rust
/// Content for rendering a viewport
#[derive(Debug)]
pub struct ViewportContent {
    /// The actual start position of the returned content
    /// May differ from requested position if adjusted to line boundary
    pub start_position: DocumentPosition,

    /// Lines of content
    pub lines: Vec<ViewportLine>,

    /// Whether there's more content after these lines
    pub has_more: bool,
}

/// A single line in the viewport
#[derive(Debug)]
pub struct ViewportLine {
    /// Start byte offset of this line in the document
    pub byte_offset: usize,

    /// The line content (without trailing newline for display)
    pub content: String,

    /// Whether this line ends with a newline
    pub has_newline: bool,

    /// Approximate line number (may be estimated for huge files)
    pub approximate_line_number: Option<usize>,
}
```

## Implementation Strategy

### Phase 1: Separate Read and Write Paths

The key insight to avoid RefCell complexity:

```rust
impl TextBuffer {
    /// Read-only text range access (no loading)
    /// Returns None if data is not available
    pub fn get_text_range(&self, offset: usize, bytes: usize) -> Option<Vec<u8>> {
        // Try to read without any loading
        // Returns None if any buffer in range is unloaded
        self.try_get_text_range_no_load(offset, bytes)
    }

    /// Prepare a viewport for rendering
    /// This is called before rendering with &mut access
    /// Pre-loads all data that will be needed
    pub fn prepare_viewport(&mut self, start_offset: usize, line_count: usize) -> Result<()> {
        // Estimate how many bytes we need (pessimistic)
        let estimated_bytes = line_count * 200; // Assume max 200 bytes per line

        // Load with full chunk-splitting support
        self.get_text_range_mut(start_offset, estimated_bytes)?;
        Ok(())
    }
}
```

### Phase 2: Implement DocumentModel for EditorState

```rust
impl DocumentModel for EditorState {
    fn capabilities(&self) -> DocumentCapabilities {
        let line_count = self.buffer.line_count();
        DocumentCapabilities {
            has_line_index: line_count.is_some(),
            uses_lazy_loading: self.buffer.is_large_file(),
            byte_length: self.buffer.len(),
            approximate_line_count: line_count.unwrap_or_else(|| {
                // Estimate assuming ~80 bytes per line
                self.buffer.len() / 80
            }),
        }
    }

    fn get_viewport_content(
        &self,
        start_pos: DocumentPosition,
        max_lines: usize,
    ) -> Result<ViewportContent> {
        // Convert to byte offset
        let start_offset = self.position_to_offset(start_pos)?;

        // Use line iterator starting from this byte offset
        let mut iter = self.buffer.line_iterator(start_offset);
        let mut lines = Vec::with_capacity(max_lines);

        for _ in 0..max_lines {
            if let Some((line_start, line_content)) = iter.next() {
                let has_newline = line_content.ends_with('\n');
                let content = if has_newline {
                    line_content[..line_content.len() - 1].to_string()
                } else {
                    line_content
                };

                // Try to get precise line number if available
                let approximate_line_number = if self.has_line_index() {
                    Some(self.buffer.get_line_number(line_start))
                } else {
                    None
                };

                lines.push(ViewportLine {
                    byte_offset: line_start,
                    content,
                    has_newline,
                    approximate_line_number,
                });
            } else {
                break;
            }
        }

        let has_more = iter.next().is_some();

        Ok(ViewportContent {
            start_position: DocumentPosition::ByteOffset(start_offset),
            lines,
            has_more,
        })
    }

    fn position_to_offset(&self, pos: DocumentPosition) -> Result<usize> {
        match pos {
            DocumentPosition::ByteOffset(offset) => Ok(offset),
            DocumentPosition::LineColumn { line, column } => {
                if !self.has_line_index() {
                    anyhow::bail!("Line indexing not available for this document");
                }
                // Use existing buffer method
                Ok(self.buffer.line_to_byte_offset(line) + column)
            }
        }
    }

    fn offset_to_position(&self, offset: usize) -> DocumentPosition {
        if self.has_line_index() {
            let line = self.buffer.get_line_number(offset);
            let line_start = self.buffer.line_to_byte_offset(line);
            let column = offset - line_start;
            DocumentPosition::LineColumn { line, column }
        } else {
            DocumentPosition::ByteOffset(offset)
        }
    }
}
```

### Phase 3: Viewport with Preparation

```rust
impl Viewport {
    /// Scroll to a specific position
    pub fn scroll_to(&mut self, position: DocumentPosition) {
        self.top_position = position;
        self.cached_content = None; // Invalidate cache
    }

    /// Scroll by lines (estimates for huge files)
    pub fn scroll_by_lines(&mut self, delta: isize, doc: &dyn DocumentModel) {
        match self.top_position {
            DocumentPosition::LineColumn { line, column } => {
                let new_line = (line as isize + delta).max(0) as usize;
                self.top_position = DocumentPosition::LineColumn {
                    line: new_line,
                    column,
                };
            }
            DocumentPosition::ByteOffset(offset) => {
                // Estimate: assume 80 bytes per line
                let bytes_per_line = 80;
                let byte_delta = delta * bytes_per_line;
                let new_offset = (offset as isize + byte_delta)
                    .max(0)
                    .min(doc.capabilities().byte_length as isize) as usize;
                self.top_position = DocumentPosition::ByteOffset(new_offset);
            }
        }
        self.cached_content = None;
    }
}

impl EditorState {
    /// Prepare viewport for rendering (called before frame render)
    pub fn prepare_for_render(&mut self) -> Result<()> {
        let start_offset = self.position_to_offset(self.viewport.top_position)?;
        let line_count = self.viewport.height as usize;
        self.buffer.prepare_viewport(start_offset, line_count)?;
        Ok(())
    }
}
```

### Phase 4: Rendering with Preparation

```rust
impl Editor {
    fn render(&mut self, frame: &mut Frame) {
        // Prepare all buffers before rendering
        for (_, state) in &mut self.buffers {
            if let Err(e) = state.prepare_for_render() {
                tracing::error!("Failed to prepare buffer for render: {}", e);
                // Continue with partial rendering
            }
        }

        // Now render (uses &self, data is already loaded)
        SplitRenderer::render_content(frame, ...);
    }
}

impl SplitRenderer {
    fn render_buffer_in_split(
        frame: &mut Frame,
        state: &EditorState, // Now &self, not &mut
        area: Rect,
        // ... other params
    ) {
        let caps = state.capabilities();

        // Get viewport content (data already loaded)
        let viewport_content = match state.get_viewport_content(
            state.viewport.top_position,
            state.viewport.height as usize,
        ) {
            Ok(content) => content,
            Err(e) => {
                // Show error message instead of blank screen
                let error_msg = format!("Failed to load content: {}", e);
                let paragraph = Paragraph::new(error_msg)
                    .style(Style::default().fg(Color::Red));
                frame.render_widget(paragraph, area);
                return;
            }
        };

        let mut lines = Vec::new();

        for viewport_line in viewport_content.lines.iter() {
            // Render line number or byte offset in gutter
            let line_label = if caps.has_line_index {
                // Precise line number
                viewport_line.approximate_line_number
                    .map(|n| format!("{:>6}", n + 1))
                    .unwrap_or_else(|| "      ".to_string())
            } else {
                // Show byte offset for huge files (hex format)
                format!("{:>10x}", viewport_line.byte_offset)
            };

            let mut line_spans = vec![
                Span::styled(line_label, Style::default().fg(theme.line_number_fg)),
                Span::raw(" │ "),
            ];

            // Render line content with syntax highlighting, selections, etc.
            // All indexed by byte offset, which always works
            let content_spans = self.render_line_content(
                &viewport_line.content,
                viewport_line.byte_offset,
                state,
                theme,
            );

            line_spans.extend(content_spans);
            lines.push(Line::from(line_spans));
        }

        let paragraph = Paragraph::new(lines);
        frame.render_widget(paragraph, area);
    }
}
```

## Benefits of This Design

### 1. Clean Abstraction Boundaries
- Rendering uses `DocumentModel`, never touches `TextBuffer`
- `TextBuffer` becomes an implementation detail
- Easy to swap implementations or add caching layers

### 2. Explicit Error Handling
- Methods return `Result` when operations can fail
- No silent "return empty on error" behavior
- Rendering can show meaningful error messages

### 3. Dual Coordinate Support
- Small files: precise line/column positioning
- Huge files: byte offset positioning with estimated line numbers
- Same APIs work for both modes

### 4. Transparent Lazy Loading
- `prepare_for_render()` pre-loads needed data
- Rendering sees fully-loaded data (no `None` checks)
- No RefCell borrow conflicts

### 5. Better Than VSCode for Huge Files
- VSCode loads everything into memory (with 20MB limit)
- Fresh supports multi-GB files with lazy loading
- Byte-based positioning always works

### 6. Type Safety
- `Option<String>` makes data availability explicit
- `Result<T>` for operations that can fail
- No confusion between "empty" and "unavailable"

## Implementation Plan - Direct Implementation (No Migration)

**Strategy:** Implement the final solution directly without gradual migration. Break the current code temporarily, then fix it all at once. This is faster and cleaner than maintaining two parallel APIs.

### Phase 1: Core Types and Trait (Day 1)
1. **Create `src/document_model.rs`** - New module with all types and trait
   - [ ] `DocumentPosition` enum
   - [ ] `DocumentCapabilities` struct
   - [ ] `ViewportContent` and `ViewportLine` structs
   - [ ] `DocumentModel` trait with all methods
   - [ ] Export from `lib.rs`

### Phase 2: TextBuffer Changes (Day 1-2)
2. **Update `src/text_buffer.rs`** - Separate read/write paths immediately
   - [ ] Add `get_text_range(&self) -> Option<Vec<u8>>` (read-only, no loading)
   - [ ] Add `prepare_viewport(&mut self, offset, line_count) -> Result<()>`
   - [ ] Make `slice()` private (breaking change - code will break temporarily)
   - [ ] Keep internal loading logic as-is

### Phase 3: EditorState Implementation (Day 2)
3. **Implement DocumentModel for EditorState** - This is the core abstraction
   - [ ] Add `impl DocumentModel for EditorState` in `src/editor_state.rs`
   - [ ] Implement all trait methods (capabilities, get_viewport_content, etc.)
   - [ ] Add `prepare_for_render(&mut self)` helper method
   - [ ] Update viewport struct to use `DocumentPosition` instead of byte offset

### Phase 4: Fix Rendering (Day 2-3)
4. **Update all rendering code** - Switch from `slice()` to `DocumentModel`
   - [ ] Update `src/renderer/mod.rs` to call `prepare_for_render()` before render
   - [ ] Update `SplitRenderer::render_buffer_in_split()` to use `get_viewport_content()`
   - [ ] Show byte offsets in gutter for huge files (instead of line numbers)
   - [ ] Add error rendering (red text) when `get_viewport_content()` fails
   - [ ] Remove all direct `.buffer.slice()` calls from rendering code

### Phase 5: Fix Editing Operations (Day 3-4)
5. **Update `src/actions.rs`** - Convert all operations to use `DocumentModel`
   - [ ] Update cursor movement (up/down/left/right) to use `DocumentPosition`
   - [ ] Update insertion operations to use `insert()` method
   - [ ] Update deletion operations to use `delete()` method
   - [ ] Update search/replace to use `find_matches()` and `replace()`
   - [ ] Remove all direct `.buffer.slice()` calls from actions

### Phase 6: Fix Remaining Code (Day 4)
6. **Audit and fix any remaining usages**
   - [ ] Search codebase for all `.slice()` calls - replace with `DocumentModel` methods
   - [ ] Search for direct `.buffer` access - route through `DocumentModel` instead
   - [ ] Update tests to use new APIs
   - [ ] Ensure all compilation errors are resolved

### Phase 7: Validation (Day 5)
7. **Test everything**
   - [ ] Run all unit tests - fix any failures
   - [ ] Run all e2e tests - fix any failures
   - [ ] Add e2e test for small file (< 1MB) with line numbers
   - [ ] Add e2e test for large file (> 100MB) with byte offsets
   - [ ] Add e2e test for editing operations (insert, delete, undo/redo)
   - [ ] Add e2e test for scrolling in both modes (line-based and byte-based)
   - [ ] All tests must pass automatically with `cargo test`

### Phase 8: Cleanup (Day 5)
8. **Final cleanup**
   - [ ] Make `TextBuffer` fields private if not already
   - [ ] Add documentation comments to all public APIs
   - [ ] Remove any dead code or unused methods
   - [ ] Update related docs (README, architecture docs)

## Key Differences from Gradual Migration

### Advantages of Direct Implementation:
1. **Faster**: 5 days instead of 5-6 weeks
2. **Cleaner**: No parallel API maintenance
3. **Simpler**: No compatibility shims or transition code
4. **Better end result**: No technical debt from migration artifacts

### Risks and Mitigation:
1. **Risk**: Breaking the build for several hours/days
   - **Mitigation**: Work in a branch, commit frequently, can revert if needed

2. **Risk**: Forgetting to update some code path
   - **Mitigation**: Use compiler errors as a checklist (make `slice()` private early)

3. **Risk**: Introducing subtle bugs
   - **Mitigation**: Comprehensive tests at each phase, e2e tests catch regressions

### Why This Works:
- Fresh is a single-developer project (so far)
- No production users depending on API stability
- Existing e2e tests will catch major regressions
- Faster to implement correctly once than to maintain two systems

## Testing Strategy

### Compile-Time Validation:
- Making `slice()` private immediately turns all usages into compiler errors
- This gives us a complete checklist of what needs updating
- Can't accidentally miss a code path

### Automated Testing:
All testing must be fully automated and pass with `cargo test`.

1. **Unit Tests** (`src/document_model.rs`, `src/editor_state.rs`)
   - Test each `DocumentModel` method independently
   - Test position conversions (line-based ↔ byte-based)
   - Test error cases (invalid positions, load failures)
   - Test both small and large file modes

2. **Integration Tests** (`tests/`)
   - Test `DocumentModel` + rendering integration
   - Test `DocumentModel` + editing integration
   - Test viewport preparation and content retrieval

3. **E2E Tests** (`tests/e2e/`)
   - **Small file test**: Open file < 1MB, verify line numbers displayed
   - **Large file test**: Open file > 100MB, verify byte offsets displayed
   - **Scroll test**: Scroll through large file, verify no blank screens
   - **Edit test**: Insert/delete in large file, verify changes persist
   - **Undo/redo test**: Undo/redo in large file, verify correctness
   - **Search test**: Search in large file, verify matches found
   - **Save test**: Save large file, verify file written correctly
   - All existing large file e2e tests continue to pass

### Test Data:
- Small test file: Generate < 1MB file with known content
- Large test file: Generate > 100MB file programmatically (don't commit to repo)
- Use temp files for all tests (cleanup automatically)

## Future Extensions

### 1. Remote Files
The `DocumentModel` abstraction makes it easy to support remote files:
- Implement `DocumentModel` for `RemoteDocument`
- Network loading in `prepare_viewport()`
- Progressive loading with placeholders

### 2. Virtual Documents
Computed content (e.g., git diffs, search results):
- Implement `DocumentModel` for `VirtualDocument`
- Generate content on-demand
- Cache computed results

### 3. Read-Only Views
Multiple views of the same document:
- Share underlying `TextBuffer`
- Each view has its own `Viewport`
- Coordinate updates via events

### 4. Collaborative Editing
OT/CRDT integration:
- Position transformations in `DocumentModel`
- Convert between local and remote positions
- Buffer modifications coordinate with sync layer

## References

- [VSCode Text Buffer Blog Post](https://code.visualstudio.com/blogs/2018/03/23/text-buffer-reimplementation)
- [VSCode TextBuffer Source](https://github.com/microsoft/vscode-textbuffer)
- [VSCode TextModel Source](https://github.com/microsoft/vscode/blob/main/src/vs/editor/common/model/textModel.ts)
- Fresh's existing documentation:
  - [piece_table.md](./piece_table.md)
  - [LARGE_FILE_SUPPORT_ANALYSIS.md](./LARGE_FILE_SUPPORT_ANALYSIS.md)
  - [BUFFER_EFFICIENCY_ANALYSIS.md](./BUFFER_EFFICIENCY_ANALYSIS.md)
