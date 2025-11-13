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

## Implementation Phases

### Phase 1: Add Types and Trait (Week 1)
- [ ] Define `DocumentPosition` enum
- [ ] Define `DocumentCapabilities` struct
- [ ] Define `DocumentModel` trait
- [ ] Define `ViewportContent` and `ViewportLine` types

### Phase 2: Implement for EditorState (Week 1-2)
- [ ] Implement `capabilities()`
- [ ] Implement `get_viewport_content()`
- [ ] Implement position conversion methods
- [ ] Add `prepare_for_render()` method

### Phase 3: Update TextBuffer (Week 2)
- [ ] Add `prepare_viewport()` method
- [ ] Separate read-only and mutable paths
- [ ] Make `slice()` private
- [ ] Return `Option<Vec<u8>>` from `get_text_range()`

### Phase 4: Update Viewport (Week 2-3)
- [ ] Change `top_byte` to `top_position: DocumentPosition`
- [ ] Update scroll methods for dual coordinates
- [ ] Add viewport content caching
- [ ] Integrate with `prepare_for_render()`

### Phase 5: Refactor Rendering (Week 3-4)
- [ ] Call `prepare_for_render()` before each frame
- [ ] Use `get_viewport_content()` instead of line iterator
- [ ] Display byte offsets for huge files
- [ ] Handle errors with on-screen messages
- [ ] Remove all direct `.buffer` access

### Phase 6: Refactor Editing (Week 4-5)
- [ ] Update `actions.rs` to use `DocumentPosition`
- [ ] Replace `.slice()` with `get_range()`
- [ ] Update cursor movement for dual coordinates
- [ ] Update undo/redo to use positions

### Phase 7: Cleanup and Optimization (Week 5-6)
- [ ] Make `TextBuffer` module-private
- [ ] Remove public `slice()` method
- [ ] Audit all buffer access patterns
- [ ] Add comprehensive tests
- [ ] Update documentation

## Migration Strategy

### Compatibility During Transition

To avoid breaking everything during migration, we'll use a gradual approach:

1. **Add new APIs alongside old ones**
   - Keep `buffer.slice()` working temporarily
   - Add `DocumentModel` trait implementation
   - Both paths work simultaneously

2. **Migrate rendering first**
   - Rendering is read-only, easier to change
   - Most visible impact (fixes blank screen bug)
   - Tests will validate correctness

3. **Migrate editing operations**
   - Update one operation type at a time
   - Keep comprehensive tests
   - Validate with e2e tests

4. **Remove old APIs**
   - Once all code migrated, remove `slice()`
   - Make `TextBuffer` module-private
   - Final API cleanup

### Testing Strategy

1. **Unit Tests**
   - Test `DocumentModel` implementation
   - Test both coordinate systems
   - Test error cases (load failures)

2. **Integration Tests**
   - Test rendering with large files
   - Test editing operations
   - Test coordinate conversions

3. **E2E Tests**
   - Test with actual huge files (>1GB)
   - Test lazy loading behavior
   - Test error recovery

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
