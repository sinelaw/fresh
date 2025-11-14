/// Document Model Architecture
///
/// This module provides a clean abstraction layer between the editor's rendering/editing
/// operations and the underlying text buffer implementation. It supports both small files
/// with precise line indexing and huge files with lazy loading and byte-based positioning.
///
/// # Overview
///
/// The document model is inspired by VSCode's architecture but enhanced to support huge files
/// (multi-GB) with lazy loading. It provides a three-layer architecture:
///
/// ```text
/// ┌─────────────────────────────────────┐
/// │  View/Editor Layer                  │
/// │  (rendering, user interaction)      │
/// └────────────┬────────────────────────┘
///              │ Uses DocumentModel trait
///              ▼
/// ┌─────────────────────────────────────┐
/// │  DocumentModel (this module)        │
/// │  - get_viewport_content()           │
/// │  - get_range(), insert(), delete()  │
/// │  - Dual coordinate systems          │
/// └────────────┬────────────────────────┘
///              │ Implemented by EditorState
///              ▼
/// ┌─────────────────────────────────────┐
/// │  TextBuffer (implementation)        │
/// │  - Piece tree operations            │
/// │  - Lazy loading for large files     │
/// │  - Line indexing for small files    │
/// └─────────────────────────────────────┘
/// ```
///
/// # Key Concepts
///
/// ## Dual Position System
///
/// Documents support two coordinate systems:
/// - **Line/Column**: For small files with precise line indexing (like VSCode)
/// - **Byte Offset**: For huge files where line indexing may be unavailable or approximate
///
/// ## Transparent Lazy Loading
///
/// For huge files, the document model uses a two-phase rendering approach:
/// 1. **Prepare Phase** (`prepare_for_render()`): Pre-loads viewport data with `&mut` access
/// 2. **Render Phase** (`get_viewport_content()`): Accesses pre-loaded data with `&self`
///
/// This avoids RefCell complexity while supporting lazy loading.
///
/// ## Explicit Error Handling
///
/// Unlike TextBuffer's `slice()` which returns empty strings on error, DocumentModel methods
/// return `Result<T>` or `Option<T>` to make failures explicit and allow proper error messages.
///
/// # Usage Example
///
/// ```rust,ignore
/// use fresh::document_model::{DocumentModel, DocumentPosition};
///
/// // Query document capabilities
/// let caps = state.capabilities();
/// if caps.has_line_index {
///     // Use line/column positioning
///     let pos = DocumentPosition::line_col(10, 5);
/// } else {
///     // Use byte offset positioning
///     let pos = DocumentPosition::byte(1024);
/// }
///
/// // Prepare viewport before rendering
/// state.prepare_for_render()?;
///
/// // Get viewport content for rendering
/// let viewport = state.get_viewport_content(
///     DocumentPosition::byte(0),
///     24  // lines
/// )?;
///
/// for line in viewport.lines {
///     println!("{}: {}", line.byte_offset, line.content);
/// }
/// ```
///
/// # Design Benefits
///
/// 1. **Clean Abstraction**: Rendering never touches TextBuffer directly
/// 2. **Better Than VSCode**: Supports multi-GB files (VSCode has 20MB limit)
/// 3. **Type Safety**: Explicit Optional/Result types prevent silent failures
/// 4. **Extensibility**: Easy to add RemoteDocument, VirtualDocument, etc.
///
/// See also: `docs/DOCUMENT_MODEL.md` for detailed architecture documentation.
use anyhow::Result;

/// Position in a document - can be line-based or byte-based
///
/// For small files with line indexing enabled, LineColumn provides precise positioning.
/// For huge files without line indexing, ByteOffset provides always-available positioning.
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

/// Information about a document's capabilities
///
/// This helps callers understand what operations are available and how to
/// interact with the document efficiently.
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

/// A single line in the viewport
#[derive(Debug, Clone)]
pub struct ViewportLine {
    /// Start byte offset of this line in the document
    pub byte_offset: usize,

    /// The line content (without trailing newline for display)
    pub content: String,

    /// Whether this line ends with a newline
    pub has_newline: bool,

    /// Approximate line number (may be estimated for huge files)
    /// None if line indexing is not available
    pub approximate_line_number: Option<usize>,
}

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

/// High-level document interface supporting both line and byte operations
///
/// This trait provides a clean abstraction for all editor operations, whether
/// rendering, editing, or searching. It works transparently with both small
/// files (with line indexing) and huge files (with lazy loading).
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
    ///
    /// Returns lines starting from position, up to max_lines.
    /// This works for both line-based and byte-based positions.
    ///
    /// Note: Data must be prepared before calling this (via prepare_for_render()).
    fn get_viewport_content(
        &self,
        start_pos: DocumentPosition,
        max_lines: usize,
    ) -> Result<ViewportContent>;

    /// Convert position to byte offset (always works)
    fn position_to_offset(&self, pos: DocumentPosition) -> Result<usize>;

    /// Convert byte offset to a position
    ///
    /// For huge files without line index, returns ByteOffset.
    /// For small files, returns LineColumn.
    fn offset_to_position(&self, offset: usize) -> DocumentPosition;

    // ===== Content Access =====

    /// Get a range of text by positions
    fn get_range(&self, start: DocumentPosition, end: DocumentPosition) -> Result<String>;

    /// Get a single line if line indexing is available
    ///
    /// Returns None if line indexing is not available.
    fn get_line_content(&self, line_number: usize) -> Option<String>;

    /// Get text around a byte offset (for operations that don't need exact lines)
    ///
    /// Returns (offset, content) where offset is the start of returned content.
    fn get_chunk_at_offset(&self, offset: usize, size: usize) -> Result<(usize, String)>;

    // ===== Editing Operations =====

    /// Insert text at a position
    ///
    /// Returns the number of bytes inserted.
    fn insert(&mut self, pos: DocumentPosition, text: &str) -> Result<usize>;

    /// Delete a range
    fn delete(&mut self, start: DocumentPosition, end: DocumentPosition) -> Result<()>;

    /// Replace a range
    fn replace(&mut self, start: DocumentPosition, end: DocumentPosition, text: &str)
        -> Result<()>;

    // ===== Search Operations =====

    /// Find all matches of a pattern in a range
    ///
    /// Returns byte offsets (always precise).
    fn find_matches(
        &self,
        pattern: &str,
        search_range: Option<(DocumentPosition, DocumentPosition)>,
    ) -> Result<Vec<usize>>;
}
