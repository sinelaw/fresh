//! WASM module for running Fresh editor in browsers
//!
//! This module provides the entry point and platform abstractions for running
//! the editor in a WebAssembly environment.
//!
//! # Architecture
//!
//! The WASM build shares the following modules with native:
//! - `model/*` - Buffer, piece tree, cursors, events
//! - `primitives/*` - Pure text manipulation utilities
//!
//! WASM-specific code handles:
//! - Browser event handling (keyboard/mouse via Ratzilla or similar)
//! - Virtual filesystem (in-memory or IndexedDB-backed)
//! - Rendering to browser terminal (via Ratzilla)
//!
//! # Usage
//!
//! Build with: `cargo build --no-default-features --features wasm`

use std::sync::Arc;

// Re-export core types for WASM consumers
pub use crate::model::buffer::{Buffer, LineEnding, TextBuffer};
pub use crate::model::cursor::{Cursor, Cursors};
pub use crate::model::event::{Event, EventLog};
pub use crate::model::filesystem::{FileSystem, NoopFileSystem, StdFileSystem};
pub use crate::model::piece_tree::{PieceTree, Position};

/// Default large file threshold for WASM (100MB)
const LARGE_FILE_THRESHOLD: usize = 100 * 1024 * 1024;

/// WASM-specific editor state
///
/// This provides a simple wrapper around the core Buffer type
/// that uses NoopFileSystem since browsers don't have direct filesystem access.
pub struct WasmEditor {
    buffer: Buffer,
}

impl WasmEditor {
    /// Create a new WASM editor with an empty buffer
    pub fn new() -> Self {
        let fs: Arc<dyn FileSystem + Send + Sync> = Arc::new(NoopFileSystem);
        Self {
            buffer: Buffer::empty(fs),
        }
    }

    /// Create a new WASM editor with initial content
    pub fn with_content(content: &str) -> Self {
        let fs: Arc<dyn FileSystem + Send + Sync> = Arc::new(NoopFileSystem);
        Self {
            buffer: Buffer::from_str(content, LARGE_FILE_THRESHOLD, fs),
        }
    }

    /// Get the buffer content as a string
    ///
    /// Returns None if the buffer contains invalid UTF-8
    pub fn content(&self) -> Option<String> {
        self.buffer.to_string()
    }

    /// Insert text at the given byte offset
    pub fn insert(&mut self, offset: usize, text: &str) {
        self.buffer.insert(offset, text);
    }

    /// Delete a range of text (start..end in bytes)
    pub fn delete(&mut self, start: usize, end: usize) {
        self.buffer.delete(start..end);
    }

    /// Get the total length of the buffer in bytes
    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    /// Check if the buffer is empty
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    /// Get the number of lines in the buffer
    ///
    /// Returns None for lazy-loaded large files
    pub fn line_count(&self) -> Option<usize> {
        self.buffer.line_count()
    }

    /// Get a reference to the underlying buffer
    pub fn buffer(&self) -> &Buffer {
        &self.buffer
    }

    /// Get a mutable reference to the underlying buffer
    pub fn buffer_mut(&mut self) -> &mut Buffer {
        &mut self.buffer
    }
}

impl Default for WasmEditor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wasm_editor_basic() {
        let mut editor = WasmEditor::new();
        assert!(editor.is_empty());
        assert_eq!(editor.len(), 0);

        editor.insert(0, "Hello, World!");
        assert!(!editor.is_empty());
        assert_eq!(editor.len(), 13);
        assert_eq!(editor.content(), Some("Hello, World!".to_string()));
    }

    #[test]
    fn test_wasm_editor_with_content() {
        let editor = WasmEditor::with_content("Initial content");
        assert_eq!(editor.content(), Some("Initial content".to_string()));
        assert_eq!(editor.line_count(), Some(1));
    }

    #[test]
    fn test_wasm_editor_delete() {
        let mut editor = WasmEditor::with_content("Hello, World!");
        editor.delete(5, 13); // Delete ", World!"
        assert_eq!(editor.content(), Some("Hello".to_string()));
    }
}
