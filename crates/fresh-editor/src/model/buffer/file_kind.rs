//! File-kind classification for a `TextBuffer`.
//!
//! Owns the three flags that describe "what sort of file is this":
//! large-file mode (lazy chunked loading), whether the line-feed scan
//! has run over it, and whether the content is binary (read-only,
//! rendered as code points). Composed inside `TextBuffer` as the
//! `file_kind` field.

/// File-kind flags for a `TextBuffer`.
#[derive(Debug, Clone, Copy, Default)]
pub struct BufferFileKind {
    /// Is this a large file (no line indexing, lazy loading enabled)?
    large_file: bool,

    /// Has a line-feed scan been performed on this large file?
    ///
    /// When true, piece-tree leaves have accurate `line_feed_cnt`
    /// values, and edits will ensure the relevant chunk is loaded
    /// before splitting so that `compute_line_feeds_static` can
    /// recount accurately.
    line_feeds_scanned: bool,

    /// Is this a binary file? Binary files are opened read-only and
    /// render unprintable characters as code points.
    is_binary: bool,
}

impl BufferFileKind {
    pub fn new(large_file: bool, is_binary: bool) -> Self {
        Self {
            large_file,
            line_feeds_scanned: false,
            is_binary,
        }
    }

    pub fn is_large_file(&self) -> bool {
        self.large_file
    }

    pub fn has_line_feed_scan(&self) -> bool {
        self.line_feeds_scanned
    }

    pub fn is_binary(&self) -> bool {
        self.is_binary
    }

    pub(super) fn set_large_file(&mut self, v: bool) {
        self.large_file = v;
    }

    pub(super) fn mark_line_feed_scan_complete(&mut self) {
        self.line_feeds_scanned = true;
    }

    pub(super) fn set_binary(&mut self, v: bool) {
        self.is_binary = v;
    }
}
