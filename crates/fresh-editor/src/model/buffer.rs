/// Text buffer that uses PieceTree with integrated line tracking
/// Architecture where the tree is the single source of truth for text and line information
use crate::model::encoding;
use crate::model::filesystem::{FileMetadata, FileSystem, WriteOp};
use crate::model::piece_tree::{
    BufferData, BufferLocation, Cursor, PieceInfo, PieceRangeIter, PieceTree, PieceView, Position,
    StringBuffer, TreeStats,
};
use crate::model::piece_tree_diff::PieceTreeDiff;
use crate::primitives::grapheme;
use anyhow::{Context, Result};
use regex::bytes::Regex;
use std::io::{self, Write};
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

// Re-export Encoding for backward compatibility
pub use encoding::Encoding;

/// Error returned when a file save operation requires elevated privileges.
///
/// This error contains all the information needed to perform the save via sudo
/// in a single operation, preserving original file ownership and permissions.
#[derive(Debug, Clone, PartialEq)]
pub struct SudoSaveRequired {
    /// Path to the temporary file containing the new content
    pub temp_path: PathBuf,
    /// Destination path where the file should be saved
    pub dest_path: PathBuf,
    /// Original file owner (UID)
    pub uid: u32,
    /// Original file group (GID)
    pub gid: u32,
    /// Original file permissions (mode)
    pub mode: u32,
}

impl std::fmt::Display for SudoSaveRequired {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Permission denied saving to {}. Use sudo to complete the operation.",
            self.dest_path.display()
        )
    }
}

impl std::error::Error for SudoSaveRequired {}

/// Error returned when a large file has a non-resynchronizable encoding
/// and requires user confirmation before loading the entire file into memory.
///
/// Non-resynchronizable encodings (like Shift-JIS, GB18030, GBK, EUC-KR) cannot
/// determine character boundaries when jumping into the middle of a file.
/// This means the entire file must be loaded and decoded sequentially.
#[derive(Debug, Clone, PartialEq)]
pub struct LargeFileEncodingConfirmation {
    /// Path to the file
    pub path: PathBuf,
    /// Size of the file in bytes
    pub file_size: usize,
    /// The detected encoding that requires full loading
    pub encoding: Encoding,
}

impl std::fmt::Display for LargeFileEncodingConfirmation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let size_mb = self.file_size as f64 / (1024.0 * 1024.0);
        write!(
            f,
            "{} ({:.0} MB) requires full load. (l)oad, (e)ncoding, (C)ancel? ",
            self.encoding.display_name(),
            size_mb
        )
    }
}

impl std::error::Error for LargeFileEncodingConfirmation {}

/// A work item for incremental line-feed scanning (one per leaf).
#[derive(Debug, Clone)]
pub struct LineScanChunk {
    /// Index of the leaf in the piece tree's leaf array.
    pub leaf_index: usize,
    /// Number of bytes in this leaf.
    pub byte_len: usize,
    /// True if the leaf already had a known line_feed_cnt (no I/O needed).
    pub already_known: bool,
}

// Large file support configuration
/// Default threshold for considering a file "large" (100 MB)
pub const DEFAULT_LARGE_FILE_THRESHOLD: usize = 100 * 1024 * 1024;

/// Chunk size to load when lazy loading (1 MB)
pub const LOAD_CHUNK_SIZE: usize = 1024 * 1024;

/// Chunk alignment for lazy loading (64 KB)
pub const CHUNK_ALIGNMENT: usize = 64 * 1024;

/// Configuration passed to TextBuffer constructors.
#[derive(Debug, Clone)]
pub struct BufferConfig {
    /// Estimated average line length in bytes. Used for approximate line number
    /// display in large files and for goto-line byte offset estimation.
    pub estimated_line_length: usize,
}

impl Default for BufferConfig {
    fn default() -> Self {
        Self {
            estimated_line_length: 80,
        }
    }
}

/// Line ending format used in the file
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LineEnding {
    /// Unix/Linux/Mac format (\n)
    #[default]
    LF,
    /// Windows format (\r\n)
    CRLF,
    /// Old Mac format (\r) - rare but supported
    CR,
}

impl LineEnding {
    /// Get the string representation of this line ending
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::LF => "\n",
            Self::CRLF => "\r\n",
            Self::CR => "\r",
        }
    }

    /// Get the display name for status bar
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::LF => "LF",
            Self::CRLF => "CRLF",
            Self::CR => "CR",
        }
    }
}

/// A write recipe built from the piece tree for saving
struct WriteRecipe {
    /// The source file path for Copy operations (if any)
    src_path: Option<PathBuf>,
    /// Data chunks for Insert operations (owned to avoid lifetime issues)
    insert_data: Vec<Vec<u8>>,
    /// Sequence of actions to build the output file
    actions: Vec<RecipeAction>,
}

/// An action in a write recipe
#[derive(Debug, Clone, Copy)]
enum RecipeAction {
    /// Copy bytes from source file at offset
    Copy { offset: u64, len: u64 },
    /// Insert data from insert_data[index]
    Insert { index: usize },
}

impl WriteRecipe {
    /// Convert the recipe to WriteOp slice for use with filesystem write_patched
    fn to_write_ops(&self) -> Vec<WriteOp<'_>> {
        self.actions
            .iter()
            .map(|action| match action {
                RecipeAction::Copy { offset, len } => WriteOp::Copy {
                    offset: *offset,
                    len: *len,
                },
                RecipeAction::Insert { index } => WriteOp::Insert {
                    data: &self.insert_data[*index],
                },
            })
            .collect()
    }

    /// Check if this recipe has any Copy operations
    fn has_copy_ops(&self) -> bool {
        self.actions
            .iter()
            .any(|a| matches!(a, RecipeAction::Copy { .. }))
    }

    /// Flatten all Insert operations into a single buffer.
    /// Only valid when has_copy_ops() returns false.
    fn flatten_inserts(&self) -> Vec<u8> {
        let mut result = Vec::new();
        for action in &self.actions {
            if let RecipeAction::Insert { index } = action {
                result.extend_from_slice(&self.insert_data[*index]);
            }
        }
        result
    }
}

/// Represents a line number (simplified for new implementation)
/// Legacy enum kept for backwards compatibility - always Absolute now
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineNumber {
    /// Absolute line number - this is the actual line number in the file
    Absolute(usize),
    /// Relative line number (deprecated - now same as Absolute)
    Relative {
        line: usize,
        from_cached_line: usize,
    },
}

impl LineNumber {
    /// Get the line number value
    pub fn value(&self) -> usize {
        match self {
            Self::Absolute(line) | Self::Relative { line, .. } => *line,
        }
    }

    /// Check if this is an absolute line number
    pub fn is_absolute(&self) -> bool {
        matches!(self, LineNumber::Absolute(_))
    }

    /// Check if this is a relative line number
    pub fn is_relative(&self) -> bool {
        matches!(self, LineNumber::Relative { .. })
    }

    /// Format the line number for display
    pub fn format(&self) -> String {
        match self {
            Self::Absolute(line) => format!("{}", line + 1),
            Self::Relative { line, .. } => format!("~{}", line + 1),
        }
    }
}

/// A text buffer that manages document content using a piece table
/// with integrated line tracking
pub struct TextBuffer {
    /// Filesystem abstraction for file I/O operations.
    /// Stored internally so methods can access it without threading through call chains.
    fs: Arc<dyn FileSystem + Send + Sync>,

    /// The piece tree for efficient text manipulation with integrated line tracking
    piece_tree: PieceTree,

    /// Snapshot of the piece tree root at last save (shared via Arc)
    saved_root: Arc<crate::model::piece_tree::PieceTreeNode>,

    /// List of string buffers containing chunks of text data
    /// Index 0 is typically the original/stored buffer
    /// Additional buffers are added for modifications
    buffers: Vec<StringBuffer>,

    /// Next buffer ID to assign
    next_buffer_id: usize,

    /// Optional file path for persistence
    file_path: Option<PathBuf>,

    /// Has the buffer been modified since last save?
    modified: bool,

    /// Does the buffer have unsaved changes for recovery auto-save?
    /// This is separate from `modified` because recovery auto-save doesn't
    /// clear `modified` (buffer still differs from on-disk file).
    recovery_pending: bool,

    /// Is this a large file (no line indexing, lazy loading enabled)?
    large_file: bool,

    /// Has a line feed scan been performed on this large file?
    /// When true, piece tree leaves have accurate `line_feed_cnt` values,
    /// and edits will ensure the relevant chunk is loaded before splitting
    /// so that `compute_line_feeds_static` can recount accurately.
    line_feeds_scanned: bool,

    /// Is this a binary file? Binary files are opened read-only and render
    /// unprintable characters as code points.
    is_binary: bool,

    /// Line ending format detected from the file (or default for new files)
    line_ending: LineEnding,

    /// Original line ending format when file was loaded (used for conversion on save)
    /// This tracks what the file had when loaded, so we can detect if the user
    /// changed the line ending format and needs conversion on save.
    original_line_ending: LineEnding,

    /// Text encoding format detected from the file (or default for new files)
    encoding: Encoding,

    /// Original encoding when file was loaded (used for conversion on save)
    /// Similar to original_line_ending, tracks what the file had when loaded.
    original_encoding: Encoding,

    /// The file size on disk after the last save.
    /// Used for chunked recovery to know the original file size for reconstruction.
    /// Updated when loading from file or after saving.
    saved_file_size: Option<usize>,

    /// Monotonic version counter for change tracking.
    version: u64,

    /// Buffer configuration (estimated line length, etc.)
    config: BufferConfig,
}

/// Snapshot of a TextBuffer's piece tree and associated string buffers.
///
/// Used by BulkEdit undo/redo to capture the complete buffer state.
/// Without this, consolidate_after_save() would destroy the string buffers
/// that a BulkEdit's piece tree snapshot references, causing corruption on undo.
#[derive(Debug, Clone)]
pub struct BufferSnapshot {
    pub piece_tree: PieceTree,
    pub buffers: Vec<StringBuffer>,
    pub next_buffer_id: usize,
}

impl TextBuffer {
    /// Create a new text buffer with the given filesystem implementation.
    /// Note: large_file_threshold is ignored in the new implementation
    pub fn new(_large_file_threshold: usize, fs: Arc<dyn FileSystem + Send + Sync>) -> Self {
        let piece_tree = PieceTree::empty();
        let line_ending = LineEnding::default();
        let encoding = Encoding::default();
        TextBuffer {
            fs,
            saved_root: piece_tree.root(),
            piece_tree,
            buffers: vec![StringBuffer::new(0, Vec::new())],
            next_buffer_id: 1,
            file_path: None,
            modified: false,
            recovery_pending: false,
            large_file: false,
            line_feeds_scanned: false,
            is_binary: false,
            line_ending,
            original_line_ending: line_ending,
            encoding,
            original_encoding: encoding,
            saved_file_size: None,
            version: 0,
            config: BufferConfig::default(),
        }
    }

    /// Create an empty buffer associated with a file path.
    /// Used for files that don't exist yet — the path is set so saving will create the file.
    pub fn new_with_path(
        large_file_threshold: usize,
        fs: Arc<dyn FileSystem + Send + Sync>,
        path: PathBuf,
    ) -> Self {
        let mut buffer = Self::new(large_file_threshold, fs);
        buffer.file_path = Some(path);
        buffer
    }

    /// Current buffer version (monotonic, wraps on overflow)
    pub fn version(&self) -> u64 {
        self.version
    }

    /// Get a reference to the filesystem implementation used by this buffer.
    pub fn filesystem(&self) -> &Arc<dyn FileSystem + Send + Sync> {
        &self.fs
    }

    /// Set the filesystem implementation for this buffer.
    pub fn set_filesystem(&mut self, fs: Arc<dyn FileSystem + Send + Sync>) {
        self.fs = fs;
    }

    #[inline]
    fn bump_version(&mut self) {
        self.version = self.version.wrapping_add(1);
    }

    #[inline]
    fn mark_content_modified(&mut self) {
        self.modified = true;
        self.recovery_pending = true;
        self.bump_version();
    }

    /// Create a text buffer from raw bytes WITHOUT encoding conversion.
    /// Used for binary files where we want to preserve the exact bytes.
    fn from_bytes_raw(content: Vec<u8>, fs: Arc<dyn FileSystem + Send + Sync>) -> Self {
        let bytes = content.len();

        // For binary files, detect line ending but don't convert encoding
        let line_ending = Self::detect_line_ending(&content);

        // Create initial StringBuffer with ID 0
        let buffer = StringBuffer::new(0, content);
        let line_feed_cnt = buffer.line_feed_count();

        let piece_tree = if bytes > 0 {
            PieceTree::new(BufferLocation::Stored(0), 0, bytes, line_feed_cnt)
        } else {
            PieceTree::empty()
        };

        let saved_root = piece_tree.root();

        TextBuffer {
            fs,
            line_ending,
            original_line_ending: line_ending,
            encoding: Encoding::Utf8, // Binary files treated as raw bytes (no conversion)
            original_encoding: Encoding::Utf8,
            piece_tree,
            saved_root,
            buffers: vec![buffer],
            next_buffer_id: 1,
            file_path: None,
            modified: false,
            recovery_pending: false,
            large_file: false,
            line_feeds_scanned: false,
            is_binary: true,
            saved_file_size: Some(bytes),
            version: 0,
            config: BufferConfig::default(),
        }
    }

    /// Create a text buffer from initial content with the given filesystem.
    pub fn from_bytes(content: Vec<u8>, fs: Arc<dyn FileSystem + Send + Sync>) -> Self {
        // Auto-detect encoding and convert to UTF-8 if needed
        let (encoding, utf8_content) = Self::detect_and_convert_encoding(&content);

        let bytes = utf8_content.len();

        // Auto-detect line ending format from content
        let line_ending = Self::detect_line_ending(&utf8_content);

        // Create initial StringBuffer with ID 0
        let buffer = StringBuffer::new(0, utf8_content);
        let line_feed_cnt = buffer.line_feed_count();

        let piece_tree = if bytes > 0 {
            PieceTree::new(BufferLocation::Stored(0), 0, bytes, line_feed_cnt)
        } else {
            PieceTree::empty()
        };

        let saved_root = piece_tree.root();

        TextBuffer {
            fs,
            line_ending,
            original_line_ending: line_ending,
            encoding,
            original_encoding: encoding,
            piece_tree,
            saved_root,
            buffers: vec![buffer],
            next_buffer_id: 1,
            file_path: None,
            modified: false,
            recovery_pending: false,
            large_file: false,
            line_feeds_scanned: false,
            is_binary: false,
            saved_file_size: Some(bytes), // Treat initial content as "saved" state
            version: 0,
            config: BufferConfig::default(),
        }
    }

    /// Create a text buffer from bytes with a specific encoding (no auto-detection).
    pub fn from_bytes_with_encoding(
        content: Vec<u8>,
        encoding: Encoding,
        fs: Arc<dyn FileSystem + Send + Sync>,
    ) -> Self {
        // Convert from specified encoding to UTF-8
        let utf8_content = encoding::convert_to_utf8(&content, encoding);

        let bytes = utf8_content.len();

        // Auto-detect line ending format from content
        let line_ending = Self::detect_line_ending(&utf8_content);

        // Create initial StringBuffer with ID 0
        let buffer = StringBuffer::new(0, utf8_content);
        let line_feed_cnt = buffer.line_feed_count();

        let piece_tree = if bytes > 0 {
            PieceTree::new(BufferLocation::Stored(0), 0, bytes, line_feed_cnt)
        } else {
            PieceTree::empty()
        };

        let saved_root = piece_tree.root();

        TextBuffer {
            fs,
            line_ending,
            original_line_ending: line_ending,
            encoding,
            original_encoding: encoding,
            piece_tree,
            saved_root,
            buffers: vec![buffer],
            next_buffer_id: 1,
            file_path: None,
            modified: false,
            recovery_pending: false,
            large_file: false,
            line_feeds_scanned: false,
            is_binary: false,
            saved_file_size: Some(bytes),
            version: 0,
            config: BufferConfig::default(),
        }
    }

    /// Create a text buffer from a string with the given filesystem.
    pub fn from_str(
        s: &str,
        _large_file_threshold: usize,
        fs: Arc<dyn FileSystem + Send + Sync>,
    ) -> Self {
        Self::from_bytes(s.as_bytes().to_vec(), fs)
    }

    /// Create an empty text buffer with the given filesystem.
    pub fn empty(fs: Arc<dyn FileSystem + Send + Sync>) -> Self {
        let piece_tree = PieceTree::empty();
        let saved_root = piece_tree.root();
        let line_ending = LineEnding::default();
        let encoding = Encoding::default();
        TextBuffer {
            fs,
            piece_tree,
            saved_root,
            buffers: vec![StringBuffer::new(0, Vec::new())],
            next_buffer_id: 1,
            file_path: None,
            modified: false,
            recovery_pending: false,
            large_file: false,
            line_feeds_scanned: false,
            is_binary: false,
            line_ending,
            original_line_ending: line_ending,
            encoding,
            original_encoding: encoding,
            saved_file_size: None,
            version: 0,
            config: BufferConfig::default(),
        }
    }

    /// Load a text buffer from a file using the given filesystem.
    pub fn load_from_file<P: AsRef<Path>>(
        path: P,
        large_file_threshold: usize,
        fs: Arc<dyn FileSystem + Send + Sync>,
    ) -> anyhow::Result<Self> {
        let path = path.as_ref();

        // Get file size to determine loading strategy
        let metadata = fs.metadata(path)?;
        let file_size = metadata.size as usize;

        // Use threshold parameter or default
        let threshold = if large_file_threshold > 0 {
            large_file_threshold
        } else {
            DEFAULT_LARGE_FILE_THRESHOLD
        };

        // Choose loading strategy based on file size
        if file_size >= threshold {
            Self::load_large_file(path, file_size, fs)
        } else {
            Self::load_small_file(path, fs)
        }
    }

    /// Load a text buffer from a file with a specific encoding (no auto-detection).
    pub fn load_from_file_with_encoding<P: AsRef<Path>>(
        path: P,
        encoding: Encoding,
        fs: Arc<dyn FileSystem + Send + Sync>,
        config: BufferConfig,
    ) -> anyhow::Result<Self> {
        let path = path.as_ref();
        let contents = fs.read_file(path)?;

        let mut buffer = Self::from_bytes_with_encoding(contents, encoding, fs);
        buffer.file_path = Some(path.to_path_buf());
        buffer.modified = false;
        buffer.config = config;
        Ok(buffer)
    }

    /// Load a small file with full eager loading and line indexing
    fn load_small_file(path: &Path, fs: Arc<dyn FileSystem + Send + Sync>) -> anyhow::Result<Self> {
        let contents = fs.read_file(path)?;

        // Use unified encoding/binary detection
        let (encoding, is_binary) = Self::detect_encoding_or_binary(&contents);

        // For binary files, skip encoding conversion to preserve raw bytes
        let mut buffer = if is_binary {
            Self::from_bytes_raw(contents, fs)
        } else {
            // from_bytes handles encoding detection/conversion and line ending detection
            Self::from_bytes(contents, fs)
        };
        buffer.file_path = Some(path.to_path_buf());
        buffer.modified = false;
        buffer.large_file = false;
        buffer.is_binary = is_binary;
        // For binary files, ensure encoding matches detection
        if is_binary {
            buffer.encoding = encoding;
            buffer.original_encoding = encoding;
        }
        // Note: line_ending and encoding are already set by from_bytes/from_bytes_raw
        Ok(buffer)
    }

    /// Check if loading a large file requires user confirmation due to encoding.
    ///
    /// Some encodings (like Shift-JIS, GB18030, GBK, EUC-KR) cannot be "resynchronized" -
    /// meaning you cannot determine character boundaries when jumping into the middle
    /// of a file. These encodings require loading the entire file into memory.
    ///
    /// Returns `Some(confirmation)` if user confirmation is needed, `None` if the file
    /// can be loaded with lazy/streaming loading.
    pub fn check_large_file_encoding(
        path: impl AsRef<Path>,
        fs: Arc<dyn FileSystem + Send + Sync>,
    ) -> anyhow::Result<Option<LargeFileEncodingConfirmation>> {
        let path = path.as_ref();
        let metadata = fs.metadata(path)?;
        let file_size = metadata.size as usize;

        // Only check for large files
        if file_size < DEFAULT_LARGE_FILE_THRESHOLD {
            return Ok(None);
        }

        // Read a sample to detect encoding
        let sample_size = file_size.min(8 * 1024);
        let sample = fs.read_range(path, 0, sample_size)?;
        let (encoding, is_binary) = Self::detect_encoding_or_binary(&sample);

        // Binary files don't need confirmation (loaded as-is)
        if is_binary {
            return Ok(None);
        }

        // Check if the encoding requires full file loading
        if encoding.requires_full_file_load() {
            return Ok(Some(LargeFileEncodingConfirmation {
                path: path.to_path_buf(),
                file_size,
                encoding,
            }));
        }

        Ok(None)
    }

    /// Load a large file with unloaded buffer (no line indexing, lazy loading)
    ///
    /// If `force_full_load` is true, loads the entire file regardless of encoding.
    /// This should be set to true after user confirms loading a non-resynchronizable encoding.
    fn load_large_file(
        path: &Path,
        file_size: usize,
        fs: Arc<dyn FileSystem + Send + Sync>,
    ) -> anyhow::Result<Self> {
        Self::load_large_file_internal(path, file_size, fs, false)
    }

    /// Load a large file, optionally forcing full load for non-resynchronizable encodings.
    ///
    /// Called with `force_full_load=true` after user confirms the warning about
    /// non-resynchronizable encodings requiring full file loading.
    pub fn load_large_file_confirmed(
        path: impl AsRef<Path>,
        fs: Arc<dyn FileSystem + Send + Sync>,
    ) -> anyhow::Result<Self> {
        let path = path.as_ref();
        let metadata = fs.metadata(path)?;
        let file_size = metadata.size as usize;
        Self::load_large_file_internal(path, file_size, fs, true)
    }

    /// Internal implementation for loading large files.
    fn load_large_file_internal(
        path: &Path,
        file_size: usize,
        fs: Arc<dyn FileSystem + Send + Sync>,
        force_full_load: bool,
    ) -> anyhow::Result<Self> {
        use crate::model::piece_tree::{BufferData, BufferLocation};

        // Read a sample of the file to detect encoding and whether it's binary
        // We read the first 8KB for detection
        let sample_size = file_size.min(8 * 1024);
        let sample = fs.read_range(path, 0, sample_size)?;

        // Use unified encoding/binary detection
        let (encoding, is_binary) = Self::detect_encoding_or_binary(&sample);

        // Binary files skip encoding conversion to preserve raw bytes
        if is_binary {
            tracing::info!("Large binary file detected, loading without encoding conversion");
            let contents = fs.read_file(path)?;
            let mut buffer = Self::from_bytes_raw(contents, fs);
            buffer.file_path = Some(path.to_path_buf());
            buffer.modified = false;
            buffer.large_file = true;
            buffer.encoding = encoding;
            buffer.original_encoding = encoding;
            return Ok(buffer);
        }

        // Check if encoding requires full file loading
        let requires_full_load = encoding.requires_full_file_load();

        // For non-resynchronizable encodings, require confirmation unless forced
        if requires_full_load && !force_full_load {
            anyhow::bail!(LargeFileEncodingConfirmation {
                path: path.to_path_buf(),
                file_size,
                encoding,
            });
        }

        // For encodings that require full load (non-resynchronizable or non-UTF-8),
        // load the entire file and convert
        if !matches!(encoding, Encoding::Utf8 | Encoding::Ascii) {
            tracing::info!(
                "Large file with non-UTF-8 encoding ({:?}), loading fully for conversion",
                encoding
            );
            let contents = fs.read_file(path)?;
            let mut buffer = Self::from_bytes(contents, fs);
            buffer.file_path = Some(path.to_path_buf());
            buffer.modified = false;
            buffer.large_file = true; // Still mark as large file for UI purposes
            buffer.is_binary = is_binary;
            return Ok(buffer);
        }

        // UTF-8/ASCII files can use lazy loading
        let line_ending = Self::detect_line_ending(&sample);

        // Create an unloaded buffer that references the entire file
        let buffer = StringBuffer {
            id: 0,
            data: BufferData::Unloaded {
                file_path: path.to_path_buf(),
                file_offset: 0,
                bytes: file_size,
            },
            stored_file_offset: None,
        };

        // Create piece tree with a single piece covering the whole file
        // No line feed count (None) since we're not computing line indexing
        let piece_tree = if file_size > 0 {
            PieceTree::new(BufferLocation::Stored(0), 0, file_size, None)
        } else {
            PieceTree::empty()
        };
        let saved_root = piece_tree.root();

        tracing::debug!(
            "Buffer::load_from_file: loaded {} bytes, saved_file_size={}",
            file_size,
            file_size
        );

        Ok(TextBuffer {
            fs,
            piece_tree,
            saved_root,
            buffers: vec![buffer],
            next_buffer_id: 1,
            file_path: Some(path.to_path_buf()),
            modified: false,
            recovery_pending: false,
            large_file: true,
            line_feeds_scanned: false,
            is_binary,
            line_ending,
            original_line_ending: line_ending,
            encoding,
            original_encoding: encoding,
            saved_file_size: Some(file_size),
            version: 0,
            config: BufferConfig::default(),
        })
    }

    /// Save the buffer to its associated file
    pub fn save(&mut self) -> anyhow::Result<()> {
        if let Some(path) = &self.file_path {
            self.save_to_file(path.clone())
        } else {
            anyhow::bail!(io::Error::new(
                io::ErrorKind::NotFound,
                "No file path associated with buffer",
            ))
        }
    }

    /// Check if we should use in-place writing to preserve file ownership.
    /// Returns true if the file exists and is owned by a different user.
    /// On Unix, only root or the file owner can change file ownership with chown.
    /// When the current user is not the file owner, using atomic write (temp file + rename)
    /// would change the file's ownership to the current user. To preserve ownership,
    /// we must write directly to the existing file instead.
    fn should_use_inplace_write(&self, dest_path: &Path) -> bool {
        !self.fs.is_owner(dest_path)
    }

    /// Build a write recipe from the piece tree for saving.
    ///
    /// This creates a recipe of Copy and Insert operations that can reconstruct
    /// the buffer content. Copy operations reference unchanged regions in the
    /// source file, while Insert operations contain new/modified data.
    ///
    /// # Returns
    /// A WriteRecipe with the source path, insert data, and sequence of actions.
    fn build_write_recipe(&self) -> io::Result<WriteRecipe> {
        let total = self.total_bytes();

        // Determine the source file for Copy operations (if any)
        // We can only use Copy if:
        // 1. We have a source file path
        // 2. The source file exists
        // 3. No line ending conversion is needed
        // 4. No encoding conversion is needed
        let needs_line_ending_conversion = self.line_ending != self.original_line_ending;
        // We need encoding conversion if:
        // - NOT a binary file (binary files preserve raw bytes), AND
        // - Either the encoding changed from the original, OR
        // - The target encoding isn't plain UTF-8/ASCII (since internal storage is UTF-8)
        // For example: UTF-8 BOM files are stored as UTF-8, so we need to add BOM on save
        let needs_encoding_conversion = !self.is_binary
            && (self.encoding != self.original_encoding
                || !matches!(self.encoding, Encoding::Utf8 | Encoding::Ascii));
        let needs_conversion = needs_line_ending_conversion || needs_encoding_conversion;

        let src_path_for_copy: Option<&Path> = if needs_conversion {
            None
        } else {
            self.file_path.as_deref().filter(|p| self.fs.exists(p))
        };
        let target_ending = self.line_ending;
        let target_encoding = self.encoding;

        let mut insert_data: Vec<Vec<u8>> = Vec::new();
        let mut actions: Vec<RecipeAction> = Vec::new();

        // Add BOM as the first piece if the target encoding has one
        if let Some(bom) = target_encoding.bom_bytes() {
            insert_data.push(bom.to_vec());
            actions.push(RecipeAction::Insert { index: 0 });
        }

        for piece_view in self.piece_tree.iter_pieces_in_range(0, total) {
            let buffer_id = piece_view.location.buffer_id();
            let buffer = self.buffers.get(buffer_id).ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Buffer {} not found", buffer_id),
                )
            })?;

            match &buffer.data {
                // Unloaded buffer: can use Copy if same source file, else load and send
                BufferData::Unloaded {
                    file_path,
                    file_offset,
                    ..
                } => {
                    // Can only use Copy if:
                    // - This is a Stored piece (original file content)
                    // - We have a valid source for copying
                    // - This buffer is from that source
                    // - No line ending or encoding conversion needed
                    let can_copy = matches!(piece_view.location, BufferLocation::Stored(_))
                        && src_path_for_copy.is_some_and(|src| file_path == src);

                    if can_copy {
                        let src_offset = (*file_offset + piece_view.buffer_offset) as u64;
                        actions.push(RecipeAction::Copy {
                            offset: src_offset,
                            len: piece_view.bytes as u64,
                        });
                        continue;
                    }

                    // Need to load and send this unloaded region
                    // This happens when: different source file, or conversion needed
                    let data = self.fs.read_range(
                        file_path,
                        (*file_offset + piece_view.buffer_offset) as u64,
                        piece_view.bytes,
                    )?;

                    let data = if needs_line_ending_conversion {
                        Self::convert_line_endings_to(&data, target_ending)
                    } else {
                        data
                    };

                    // Convert encoding if needed
                    let data = if needs_encoding_conversion {
                        Self::convert_to_encoding(&data, target_encoding)
                    } else {
                        data
                    };

                    let index = insert_data.len();
                    insert_data.push(data);
                    actions.push(RecipeAction::Insert { index });
                }

                // Loaded data: send as Insert
                BufferData::Loaded { data, .. } => {
                    let start = piece_view.buffer_offset;
                    let end = start + piece_view.bytes;
                    let chunk = &data[start..end];

                    let chunk = if needs_line_ending_conversion {
                        Self::convert_line_endings_to(chunk, target_ending)
                    } else {
                        chunk.to_vec()
                    };

                    // Convert encoding if needed
                    let chunk = if needs_encoding_conversion {
                        Self::convert_to_encoding(&chunk, target_encoding)
                    } else {
                        chunk
                    };

                    let index = insert_data.len();
                    insert_data.push(chunk);
                    actions.push(RecipeAction::Insert { index });
                }
            }
        }

        Ok(WriteRecipe {
            src_path: src_path_for_copy.map(|p| p.to_path_buf()),
            insert_data,
            actions,
        })
    }

    /// Create a temporary file for saving.
    ///
    /// Tries to create the file in the same directory as the destination file first
    /// to allow for an atomic rename. If that fails (e.g., due to directory permissions),
    /// falls back to the system temporary directory.
    fn create_temp_file(
        &self,
        dest_path: &Path,
    ) -> io::Result<(PathBuf, Box<dyn crate::model::filesystem::FileWriter>)> {
        // Try creating in same directory first
        let same_dir_temp = self.fs.temp_path_for(dest_path);
        match self.fs.create_file(&same_dir_temp) {
            Ok(file) => Ok((same_dir_temp, file)),
            Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
                // Fallback to system temp directory
                let temp_path = self.fs.unique_temp_path(dest_path);
                let file = self.fs.create_file(&temp_path)?;
                Ok((temp_path, file))
            }
            Err(e) => Err(e),
        }
    }

    /// Create a temporary file in the recovery directory for in-place writes.
    /// This allows recovery if a crash occurs during the in-place write operation.
    fn create_recovery_temp_file(
        &self,
        dest_path: &Path,
    ) -> io::Result<(PathBuf, Box<dyn crate::model::filesystem::FileWriter>)> {
        // Get recovery directory: $XDG_DATA_HOME/fresh/recovery or ~/.local/share/fresh/recovery
        let recovery_dir = crate::input::input_history::get_data_dir()
            .map(|d| d.join("recovery"))
            .unwrap_or_else(|_| std::env::temp_dir());

        // Ensure directory exists
        self.fs.create_dir_all(&recovery_dir)?;

        // Create unique filename based on destination file and timestamp
        let file_name = dest_path
            .file_name()
            .unwrap_or_else(|| std::ffi::OsStr::new("fresh-save"));
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let pid = std::process::id();

        let temp_name = format!(
            ".inplace-{}-{}-{}.tmp",
            file_name.to_string_lossy(),
            pid,
            timestamp
        );
        let temp_path = recovery_dir.join(temp_name);

        let file = self.fs.create_file(&temp_path)?;
        Ok((temp_path, file))
    }

    /// Get the path for in-place write recovery metadata.
    /// Uses the same recovery directory as temp files.
    fn inplace_recovery_meta_path(&self, dest_path: &Path) -> PathBuf {
        let recovery_dir = crate::input::input_history::get_data_dir()
            .map(|d| d.join("recovery"))
            .unwrap_or_else(|_| std::env::temp_dir());

        let hash = crate::services::recovery::path_hash(dest_path);
        recovery_dir.join(format!("{}.inplace.json", hash))
    }

    /// Write in-place recovery metadata using self.fs.
    /// This is called before the dangerous streaming step so we can recover on crash.
    fn write_inplace_recovery_meta(
        &self,
        meta_path: &Path,
        dest_path: &Path,
        temp_path: &Path,
        original_metadata: &Option<FileMetadata>,
    ) -> io::Result<()> {
        #[cfg(unix)]
        let (uid, gid, mode) = original_metadata
            .as_ref()
            .map(|m| {
                (
                    m.uid.unwrap_or(0),
                    m.gid.unwrap_or(0),
                    m.permissions.as_ref().map(|p| p.mode()).unwrap_or(0o644),
                )
            })
            .unwrap_or((0, 0, 0o644));
        #[cfg(not(unix))]
        let (uid, gid, mode) = (0u32, 0u32, 0o644u32);

        let recovery = crate::services::recovery::InplaceWriteRecovery::new(
            dest_path.to_path_buf(),
            temp_path.to_path_buf(),
            uid,
            gid,
            mode,
        );

        let json = serde_json::to_string_pretty(&recovery)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        self.fs.write_file(meta_path, json.as_bytes())
    }

    /// Save the buffer to a specific file
    ///
    /// Uses the write recipe approach for both local and remote filesystems:
    /// - Copy ops reference unchanged regions in the source file
    /// - Insert ops contain new/modified data
    ///
    /// For remote filesystems, the recipe is sent to the agent which reconstructs
    /// the file server-side, avoiding transfer of unchanged content.
    ///
    /// For local filesystems with ownership concerns (file owned by another user),
    /// uses in-place writing to preserve ownership. Otherwise uses atomic writes.
    ///
    /// If the line ending format has been changed (via set_line_ending), all content
    /// will be converted to the new format during save.
    pub fn save_to_file<P: AsRef<Path>>(&mut self, path: P) -> anyhow::Result<()> {
        let dest_path = path.as_ref();
        let total = self.total_bytes();

        // Handle empty files
        if total == 0 {
            self.fs.write_file(dest_path, &[])?;
            self.finalize_save(dest_path)?;
            return Ok(());
        }

        // Build the write recipe (unified for all filesystem types)
        let recipe = self.build_write_recipe()?;
        let ops = recipe.to_write_ops();

        // Check if we need in-place writing to preserve file ownership (local only)
        // Remote filesystems handle this differently
        let is_local = self.fs.remote_connection_info().is_none();
        let use_inplace = is_local && self.should_use_inplace_write(dest_path);

        if use_inplace {
            // In-place write: write directly to preserve ownership
            self.save_with_inplace_write(dest_path, &recipe)?;
        } else if !recipe.has_copy_ops() && !is_local {
            // Remote with no Copy ops: use write_file directly (more efficient)
            let data = recipe.flatten_inserts();
            self.fs.write_file(dest_path, &data)?;
        } else if is_local {
            // Local: use write_file or write_patched with sudo fallback
            let write_result = if !recipe.has_copy_ops() {
                let data = recipe.flatten_inserts();
                self.fs.write_file(dest_path, &data)
            } else {
                let src_for_patch = recipe.src_path.as_deref().unwrap_or(dest_path);
                self.fs.write_patched(src_for_patch, dest_path, &ops)
            };

            if let Err(e) = write_result {
                if e.kind() == io::ErrorKind::PermissionDenied {
                    // Create temp file and return sudo error
                    let original_metadata = self.fs.metadata_if_exists(dest_path);
                    let (temp_path, mut temp_file) = self.create_temp_file(dest_path)?;
                    self.write_recipe_to_file(&mut temp_file, &recipe)?;
                    temp_file.sync_all()?;
                    drop(temp_file);
                    return Err(self.make_sudo_error(temp_path, dest_path, original_metadata));
                }
                return Err(e.into());
            }
        } else {
            // Remote with Copy ops: use write_patched
            let src_for_patch = recipe.src_path.as_deref().unwrap_or(dest_path);
            self.fs.write_patched(src_for_patch, dest_path, &ops)?;
        }

        self.finalize_save(dest_path)?;
        Ok(())
    }

    /// Write using in-place mode to preserve file ownership.
    ///
    /// This is used when the file is owned by a different user and we need
    /// to write directly to the existing file to preserve its ownership.
    ///
    /// The approach:
    /// 1. Write the recipe to a temp file first (reads from original, writes to temp)
    /// 2. Stream the temp file content to the destination file (truncates and writes)
    /// 3. Delete the temp file
    ///
    /// This avoids the bug where truncating the destination before reading Copy chunks
    /// would corrupt the file. It also works for huge files since we stream in chunks.
    fn save_with_inplace_write(
        &self,
        dest_path: &Path,
        recipe: &WriteRecipe,
    ) -> anyhow::Result<()> {
        let original_metadata = self.fs.metadata_if_exists(dest_path);

        // Optimization: if no Copy ops, we can write directly without a temp file
        // (same as the non-inplace path for small files)
        if !recipe.has_copy_ops() {
            let data = recipe.flatten_inserts();
            return self.write_data_inplace(dest_path, &data, original_metadata);
        }

        // Step 1: Write recipe to a temp file in the recovery directory
        // This reads Copy chunks from the original file (still intact) and writes to temp.
        // Using the recovery directory allows crash recovery if the operation fails.
        let (temp_path, mut temp_file) = self.create_recovery_temp_file(dest_path)?;
        if let Err(e) = self.write_recipe_to_file(&mut temp_file, recipe) {
            // Best-effort cleanup of temp file on write failure
            #[allow(clippy::let_underscore_must_use)]
            let _ = self.fs.remove_file(&temp_path);
            return Err(e.into());
        }
        temp_file.sync_all()?;
        drop(temp_file);

        // Step 1.5: Save recovery metadata before the dangerous step
        // If we crash during step 2, this metadata + temp file allows recovery
        let recovery_meta_path = self.inplace_recovery_meta_path(dest_path);
        // Best effort - don't fail the save if we can't write recovery metadata
        #[allow(clippy::let_underscore_must_use)]
        let _ = self.write_inplace_recovery_meta(
            &recovery_meta_path,
            dest_path,
            &temp_path,
            &original_metadata,
        );

        // Step 2: Stream temp file content to destination
        // Now it's safe to truncate the destination since all data is in temp
        match self.fs.open_file_for_write(dest_path) {
            Ok(mut out_file) => {
                if let Err(e) = self.stream_file_to_writer(&temp_path, &mut out_file) {
                    // Don't delete temp file or recovery metadata - allow recovery
                    return Err(e.into());
                }
                out_file.sync_all()?;
                // Success! Clean up temp file and recovery metadata (best-effort)
                #[allow(clippy::let_underscore_must_use)]
                let _ = self.fs.remove_file(&temp_path);
                #[allow(clippy::let_underscore_must_use)]
                let _ = self.fs.remove_file(&recovery_meta_path);
                Ok(())
            }
            Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
                // Can't write to destination - trigger sudo fallback
                // Keep temp file for sudo to use, clean up recovery metadata (best-effort)
                #[allow(clippy::let_underscore_must_use)]
                let _ = self.fs.remove_file(&recovery_meta_path);
                Err(self.make_sudo_error(temp_path, dest_path, original_metadata))
            }
            Err(e) => {
                // Don't delete temp file or recovery metadata - allow recovery
                Err(e.into())
            }
        }
    }

    /// Write data directly to a file in-place, with sudo fallback on permission denied.
    fn write_data_inplace(
        &self,
        dest_path: &Path,
        data: &[u8],
        original_metadata: Option<FileMetadata>,
    ) -> anyhow::Result<()> {
        match self.fs.open_file_for_write(dest_path) {
            Ok(mut out_file) => {
                out_file.write_all(data)?;
                out_file.sync_all()?;
                Ok(())
            }
            Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
                // Create temp file for sudo fallback
                let (temp_path, mut temp_file) = self.create_temp_file(dest_path)?;
                temp_file.write_all(data)?;
                temp_file.sync_all()?;
                drop(temp_file);
                Err(self.make_sudo_error(temp_path, dest_path, original_metadata))
            }
            Err(e) => Err(e.into()),
        }
    }

    /// Stream a file's content to a writer in chunks to avoid memory issues with large files.
    fn stream_file_to_writer(
        &self,
        src_path: &Path,
        out_file: &mut Box<dyn crate::model::filesystem::FileWriter>,
    ) -> io::Result<()> {
        const CHUNK_SIZE: usize = 1024 * 1024; // 1MB chunks

        let file_size = self.fs.metadata(src_path)?.size as u64;
        let mut offset = 0u64;

        while offset < file_size {
            let remaining = file_size - offset;
            let chunk_len = std::cmp::min(remaining, CHUNK_SIZE as u64) as usize;
            let chunk = self.fs.read_range(src_path, offset, chunk_len)?;
            out_file.write_all(&chunk)?;
            offset += chunk_len as u64;
        }

        Ok(())
    }

    /// Write the recipe content to a file writer.
    fn write_recipe_to_file(
        &self,
        out_file: &mut Box<dyn crate::model::filesystem::FileWriter>,
        recipe: &WriteRecipe,
    ) -> io::Result<()> {
        for action in &recipe.actions {
            match action {
                RecipeAction::Copy { offset, len } => {
                    // Read from source and write to output
                    let src_path = recipe.src_path.as_ref().ok_or_else(|| {
                        io::Error::new(io::ErrorKind::InvalidData, "Copy action without source")
                    })?;
                    let data = self.fs.read_range(src_path, *offset, *len as usize)?;
                    out_file.write_all(&data)?;
                }
                RecipeAction::Insert { index } => {
                    out_file.write_all(&recipe.insert_data[*index])?;
                }
            }
        }
        Ok(())
    }

    /// Finalize save state after successful write.
    fn finalize_save(&mut self, dest_path: &Path) -> anyhow::Result<()> {
        let new_size = self.fs.metadata(dest_path)?.size as usize;
        tracing::debug!(
            "Buffer::save: updating saved_file_size from {:?} to {}",
            self.saved_file_size,
            new_size
        );
        self.saved_file_size = Some(new_size);
        self.file_path = Some(dest_path.to_path_buf());

        // Consolidate the piece tree to synchronize with disk (for large files)
        // or to simplify structure (for small files).
        self.consolidate_after_save(dest_path, new_size);

        self.mark_saved_snapshot();
        self.original_line_ending = self.line_ending;
        self.original_encoding = self.encoding;
        Ok(())
    }

    /// Finalize buffer state after an external save operation (e.g., via sudo).
    ///
    /// This updates the saved snapshot and file size to match the new state on disk.
    pub fn finalize_external_save(&mut self, dest_path: PathBuf) -> anyhow::Result<()> {
        let new_size = self.fs.metadata(&dest_path)?.size as usize;
        self.saved_file_size = Some(new_size);
        self.file_path = Some(dest_path.clone());

        // Consolidate the piece tree to synchronize with disk or simplify structure.
        self.consolidate_after_save(&dest_path, new_size);

        self.mark_saved_snapshot();
        self.original_line_ending = self.line_ending;
        self.original_encoding = self.encoding;
        Ok(())
    }

    /// Consolidate the piece tree into a single piece.
    /// For large files, this creates a reference to the disk file to save memory and sync offsets.
    /// For small files, this flattens all edits into a single in-memory buffer.
    fn consolidate_after_save(&mut self, path: &Path, file_size: usize) {
        if self.large_file {
            self.consolidate_large_file(path, file_size);
        } else {
            self.consolidate_small_file();
        }
    }

    /// Consolidate large file piece tree into a single piece pointing to the new file.
    /// This ensures that subsequent operations correctly reference the new content and offsets.
    /// Preserves total line feed count from the old tree if a scan was previously done.
    fn consolidate_large_file(&mut self, path: &Path, file_size: usize) {
        // Preserve line feed count from the old tree if we had scanned it
        let preserved_lf = if self.line_feeds_scanned {
            self.piece_tree.line_count().map(|c| c.saturating_sub(1))
        } else {
            None
        };

        let buffer = StringBuffer {
            id: 0,
            data: BufferData::Unloaded {
                file_path: path.to_path_buf(),
                file_offset: 0,
                bytes: file_size,
            },
            stored_file_offset: None,
        };

        self.piece_tree = if file_size > 0 {
            PieceTree::new(BufferLocation::Stored(0), 0, file_size, preserved_lf)
        } else {
            PieceTree::empty()
        };

        self.buffers = vec![buffer];
        self.next_buffer_id = 1;

        tracing::debug!(
            "Buffer::consolidate_large_file: consolidated into single piece of {} bytes",
            file_size
        );
    }

    /// Consolidate small file edits into a single in-memory buffer and re-index lines.
    fn consolidate_small_file(&mut self) {
        if let Some(bytes) = self.get_all_text() {
            let line_feed_cnt = bytes.iter().filter(|&&b| b == b'\n').count();
            let len = bytes.len();

            // Create a single loaded buffer with line indexing
            let buffer = StringBuffer::new_loaded(0, bytes, true);

            self.piece_tree = if len > 0 {
                PieceTree::new(BufferLocation::Stored(0), 0, len, Some(line_feed_cnt))
            } else {
                PieceTree::empty()
            };

            self.buffers = vec![buffer];
            self.next_buffer_id = 1;

            tracing::debug!(
                "Buffer::consolidate_small_file: consolidated into single loaded buffer of {} bytes",
                len
            );
        }
    }

    /// Internal helper to create a SudoSaveRequired error.
    fn make_sudo_error(
        &self,
        temp_path: PathBuf,
        dest_path: &Path,
        original_metadata: Option<FileMetadata>,
    ) -> anyhow::Error {
        #[cfg(unix)]
        let (uid, gid, mode) = if let Some(ref meta) = original_metadata {
            (
                meta.uid.unwrap_or(0),
                meta.gid.unwrap_or(0),
                meta.permissions
                    .as_ref()
                    .map(|p| p.mode() & 0o7777)
                    .unwrap_or(0),
            )
        } else {
            (0, 0, 0)
        };
        #[cfg(not(unix))]
        let (uid, gid, mode) = (0u32, 0u32, 0u32);

        let _ = original_metadata; // suppress unused warning on non-Unix

        anyhow::anyhow!(SudoSaveRequired {
            temp_path,
            dest_path: dest_path.to_path_buf(),
            uid,
            gid,
            mode,
        })
    }

    /// Get the total number of bytes in the document
    pub fn total_bytes(&self) -> usize {
        self.piece_tree.total_bytes()
    }

    /// Get the total number of lines in the document
    /// Uses the piece tree's integrated line tracking
    /// Returns None if line count is unknown (e.g., for large files without line indexing)
    pub fn line_count(&self) -> Option<usize> {
        self.piece_tree.line_count()
    }

    /// Snapshot the current tree as the saved baseline
    pub fn mark_saved_snapshot(&mut self) {
        self.saved_root = self.piece_tree.root();
        self.modified = false;
    }

    /// Refresh the saved root to match the current tree structure without
    /// clearing the modified flag.  Call this after structural-only changes
    /// (e.g. chunk_split_and_load during search scan) so that
    /// `diff_since_saved()` can take the fast `Arc::ptr_eq` path.
    pub fn refresh_saved_root_if_unmodified(&mut self) {
        if !self.modified {
            self.saved_root = self.piece_tree.root();
        }
    }

    /// Apply a chunk-load buffer replacement to `saved_root`.
    ///
    /// When viewport loading converts a `Stored(buffer_id)` piece to
    /// `Added(new_buffer_id)` in the current tree and the buffer is already
    /// modified, we must apply the same transformation to `saved_root` so
    /// that `diff_since_saved()` can match loaded-but-unedited regions by
    /// `(location, offset)` identity.
    fn apply_chunk_load_to_saved_root(
        &mut self,
        old_buffer_id: usize,
        chunk_offset_in_buffer: usize,
        chunk_bytes: usize,
        new_buffer_id: usize,
    ) {
        use crate::model::piece_tree::{LeafData, PieceTree};

        let mut leaves = Vec::new();
        self.saved_root.collect_leaves(&mut leaves);

        let mut modified = false;
        let mut new_leaves: Vec<LeafData> = Vec::with_capacity(leaves.len() + 2);

        for leaf in &leaves {
            if leaf.location.buffer_id() != old_buffer_id {
                new_leaves.push(*leaf);
                continue;
            }

            let leaf_start = leaf.offset;
            let leaf_end = leaf.offset + leaf.bytes;
            let chunk_start = chunk_offset_in_buffer;
            let chunk_end = chunk_offset_in_buffer + chunk_bytes;

            // Check if this leaf overlaps the chunk range
            if chunk_start >= leaf_end || chunk_end <= leaf_start {
                // No overlap — keep as-is
                new_leaves.push(*leaf);
                continue;
            }

            modified = true;

            // Prefix: portion of this leaf before the chunk
            if chunk_start > leaf_start {
                new_leaves.push(LeafData::new(
                    leaf.location,
                    leaf.offset,
                    chunk_start - leaf_start,
                    None, // line feed count unknown after split
                ));
            }

            // The chunk itself — replaced with Added(new_buffer_id)
            let actual_start = chunk_start.max(leaf_start);
            let actual_end = chunk_end.min(leaf_end);
            let offset_in_chunk = actual_start - chunk_start;
            new_leaves.push(LeafData::new(
                BufferLocation::Added(new_buffer_id),
                offset_in_chunk,
                actual_end - actual_start,
                None,
            ));

            // Suffix: portion of this leaf after the chunk
            if chunk_end < leaf_end {
                new_leaves.push(LeafData::new(
                    leaf.location,
                    chunk_end,
                    leaf_end - chunk_end,
                    None,
                ));
            }
        }

        if modified {
            self.saved_root = PieceTree::from_leaves(&new_leaves).root();
        }
    }

    /// Diff the current piece tree against the last saved snapshot.
    ///
    /// This compares actual byte content, not just tree structure. This means
    /// that if you delete text and then paste it back, the diff will correctly
    /// show no changes (even though the tree structure differs).
    ///
    /// Uses a two-phase algorithm for efficiency:
    /// - Phase 1: Fast structure-based diff to find changed byte ranges (O(num_leaves))
    /// - Phase 2: Only compare actual content within changed ranges (O(edit_size))
    ///
    /// This is O(edit_size) instead of O(file_size) for small edits in large files.
    pub fn diff_since_saved(&self) -> PieceTreeDiff {
        let _span = tracing::info_span!(
            "diff_since_saved",
            large_file = self.large_file,
            modified = self.modified,
            lf_scanned = self.line_feeds_scanned
        )
        .entered();

        // Fast path: if the buffer hasn't been modified since loading/saving,
        // the content is identical to the saved version by definition.
        // This avoids an expensive O(num_leaves) structure walk when the tree
        // has been restructured for non-edit reasons (viewport chunk loading,
        // line-scan preparation, search-scan splits).
        if !self.modified {
            tracing::info!("diff_since_saved: not modified → equal");
            return PieceTreeDiff {
                equal: true,
                byte_ranges: Vec::new(),
                line_ranges: Some(Vec::new()),
                nodes_visited: 0,
            };
        }

        // Quick check: if tree roots are identical (Arc pointer equality),
        // the content is definitely the same.
        if Arc::ptr_eq(&self.saved_root, &self.piece_tree.root()) {
            tracing::info!("diff_since_saved: Arc::ptr_eq fast path → equal");
            return PieceTreeDiff {
                equal: true,
                byte_ranges: Vec::new(),
                line_ranges: Some(Vec::new()),
                nodes_visited: 0,
            };
        }

        // Phase 1: Fast structure-based diff to find which byte ranges differ
        // This is O(number of leaves) - very fast even for large files
        let structure_diff = self.diff_trees_by_structure();

        // If structure says trees are equal (same pieces in same order), we're done
        if structure_diff.equal {
            tracing::info!(
                "diff_since_saved: structure equal, line_ranges={}",
                structure_diff
                    .line_ranges
                    .as_ref()
                    .map_or("None".to_string(), |r| format!("Some({})", r.len()))
            );
            return structure_diff;
        }

        // Phase 2: For small changed regions, verify with actual content comparison
        // This handles the case where different pieces contain identical content
        // (e.g., delete text then paste it back)
        let total_changed_bytes: usize = structure_diff
            .byte_ranges
            .iter()
            .map(|r| r.end.saturating_sub(r.start))
            .sum();

        // Only do content verification if the changed region is reasonably small
        // For large changes, trust the structure-based diff
        const MAX_VERIFY_BYTES: usize = 64 * 1024; // 64KB threshold for verification

        if total_changed_bytes <= MAX_VERIFY_BYTES && !structure_diff.byte_ranges.is_empty() {
            // Check if content in the changed ranges is actually different
            if self.verify_content_differs_in_ranges(&structure_diff.byte_ranges) {
                tracing::info!(
                    "diff_since_saved: content differs, byte_ranges={}, line_ranges={}",
                    structure_diff.byte_ranges.len(),
                    structure_diff
                        .line_ranges
                        .as_ref()
                        .map_or("None".to_string(), |r| format!("Some({})", r.len()))
                );
                // Content actually differs - return the structure diff result
                return structure_diff;
            } else {
                // Content is the same despite structure differences (rare case: undo/redo)
                return PieceTreeDiff {
                    equal: true,
                    byte_ranges: Vec::new(),
                    line_ranges: Some(Vec::new()),
                    nodes_visited: structure_diff.nodes_visited,
                };
            }
        }

        tracing::info!(
            "diff_since_saved: large change, byte_ranges={}, line_ranges={}, nodes_visited={}",
            structure_diff.byte_ranges.len(),
            structure_diff
                .line_ranges
                .as_ref()
                .map_or("None".to_string(), |r| format!("Some({})", r.len())),
            structure_diff.nodes_visited
        );
        // For large changes or when we can't verify, trust the structure diff
        structure_diff
    }

    /// Check if the actual byte content differs in the given ranges.
    /// Returns true if content differs, false if content is identical.
    fn verify_content_differs_in_ranges(&self, byte_ranges: &[std::ops::Range<usize>]) -> bool {
        let saved_bytes = self.tree_total_bytes(&self.saved_root);
        let current_bytes = self.piece_tree.total_bytes();

        // Different total sizes means content definitely differs
        if saved_bytes != current_bytes {
            return true;
        }

        // For each changed range, compare the actual bytes
        for range in byte_ranges {
            if range.start >= range.end {
                continue;
            }

            // Extract bytes from saved tree for this range
            let saved_slice =
                self.extract_range_from_tree(&self.saved_root, range.start, range.end);
            // Extract bytes from current tree for this range
            let current_slice = self.get_text_range(range.start, range.end);

            match (saved_slice, current_slice) {
                (Some(saved), Some(current)) => {
                    if saved != current {
                        return true; // Content differs
                    }
                }
                _ => {
                    // Couldn't read content, assume it differs to be safe
                    return true;
                }
            }
        }

        // All ranges have identical content
        false
    }

    /// Extract a byte range from a saved tree root
    fn extract_range_from_tree(
        &self,
        root: &Arc<crate::model::piece_tree::PieceTreeNode>,
        start: usize,
        end: usize,
    ) -> Option<Vec<u8>> {
        let mut result = Vec::with_capacity(end.saturating_sub(start));
        self.collect_range_from_node(root, start, end, 0, &mut result)?;
        Some(result)
    }

    /// Recursively collect bytes from a range within a tree node
    fn collect_range_from_node(
        &self,
        node: &Arc<crate::model::piece_tree::PieceTreeNode>,
        range_start: usize,
        range_end: usize,
        node_offset: usize,
        result: &mut Vec<u8>,
    ) -> Option<()> {
        use crate::model::piece_tree::PieceTreeNode;

        match node.as_ref() {
            PieceTreeNode::Internal {
                left_bytes,
                left,
                right,
                ..
            } => {
                let left_end = node_offset + left_bytes;

                // Check if range overlaps with left subtree
                if range_start < left_end {
                    self.collect_range_from_node(
                        left,
                        range_start,
                        range_end,
                        node_offset,
                        result,
                    )?;
                }

                // Check if range overlaps with right subtree
                if range_end > left_end {
                    self.collect_range_from_node(right, range_start, range_end, left_end, result)?;
                }
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
                ..
            } => {
                let node_end = node_offset + bytes;

                // Check if this leaf overlaps with our range
                if range_start < node_end && range_end > node_offset {
                    let buf = self.buffers.get(location.buffer_id())?;
                    let data = buf.get_data()?;

                    // Calculate the slice within this leaf
                    let leaf_start = range_start.saturating_sub(node_offset);
                    let leaf_end = (range_end - node_offset).min(*bytes);

                    if leaf_start < leaf_end {
                        let slice = data.get(*offset + leaf_start..*offset + leaf_end)?;
                        result.extend_from_slice(slice);
                    }
                }
            }
        }
        Some(())
    }

    /// Helper to get total bytes from a tree root
    fn tree_total_bytes(&self, root: &Arc<crate::model::piece_tree::PieceTreeNode>) -> usize {
        use crate::model::piece_tree::PieceTreeNode;
        match root.as_ref() {
            PieceTreeNode::Internal {
                left_bytes, right, ..
            } => left_bytes + self.tree_total_bytes(right),
            PieceTreeNode::Leaf { bytes, .. } => *bytes,
        }
    }

    /// Structure-based diff comparing piece tree leaves
    fn diff_trees_by_structure(&self) -> PieceTreeDiff {
        crate::model::piece_tree_diff::diff_piece_trees(
            &self.saved_root,
            &self.piece_tree.root(),
            &|leaf, start, len| {
                if len == 0 {
                    return Some(0);
                }
                // Try counting from raw byte data first
                if let Some(buf) = self.buffers.get(leaf.location.buffer_id()) {
                    if let Some(data) = buf.get_data() {
                        let start = leaf.offset + start;
                        let end = start + len;
                        if let Some(slice) = data.get(start..end) {
                            let line_feeds = slice.iter().filter(|&&b| b == b'\n').count();
                            return Some(line_feeds);
                        }
                    }
                }
                // Fallback: use the leaf's cached line_feed_cnt when we're
                // querying the entire leaf. This handles unloaded segments in
                // large file mode after line scanning has populated the metadata.
                if start == 0 && len == leaf.bytes {
                    leaf.line_feed_cnt.map(|c| c as usize)
                } else {
                    tracing::warn!(
                        "diff line_counter: returning None for partial leaf query: \
                         loc={:?} offset={} bytes={} lf_cnt={:?} query_start={} query_len={}",
                        leaf.location,
                        leaf.offset,
                        leaf.bytes,
                        leaf.line_feed_cnt,
                        start,
                        len
                    );
                    None
                }
            },
        )
    }

    /// Convert a byte offset to a line/column position
    pub fn offset_to_position(&self, offset: usize) -> Option<Position> {
        self.piece_tree
            .offset_to_position(offset, &self.buffers)
            .map(|(line, column)| Position { line, column })
    }

    /// Convert a line/column position to a byte offset
    pub fn position_to_offset(&self, position: Position) -> usize {
        self.piece_tree
            .position_to_offset(position.line, position.column, &self.buffers)
    }

    /// Insert text at the given byte offset
    pub fn insert_bytes(&mut self, offset: usize, text: Vec<u8>) -> Cursor {
        if text.is_empty() {
            return self.piece_tree.cursor_at_offset(offset);
        }

        // Mark as modified (updates version)
        self.mark_content_modified();

        // Count line feeds in the text to insert
        let line_feed_cnt = Some(text.iter().filter(|&&b| b == b'\n').count());

        // Optimization: try to append to existing buffer if insertion is at piece boundary
        let (buffer_location, buffer_offset, text_len) =
            if let Some(append_info) = self.try_append_to_existing_buffer(offset, &text) {
                append_info
            } else {
                // Create a new StringBuffer for this insertion
                let buffer_id = self.next_buffer_id;
                self.next_buffer_id += 1;
                let buffer = StringBuffer::new(buffer_id, text.clone());
                self.buffers.push(buffer);
                (BufferLocation::Added(buffer_id), 0, text.len())
            };

        // When line feeds have been scanned, ensure the chunk at the insertion
        // point is loaded so compute_line_feeds_static can recount during splits.
        if self.line_feeds_scanned {
            self.ensure_chunk_loaded_at(offset);
        }

        // Update piece tree (need to pass buffers reference)
        self.piece_tree.insert(
            offset,
            buffer_location,
            buffer_offset,
            text_len,
            line_feed_cnt,
            &self.buffers,
        )
    }

    /// Try to append to an existing buffer if insertion point aligns with buffer end
    /// Returns (BufferLocation, buffer_offset, text_len) if append succeeds, None otherwise
    fn try_append_to_existing_buffer(
        &mut self,
        offset: usize,
        text: &[u8],
    ) -> Option<(BufferLocation, usize, usize)> {
        // Only optimize for non-empty insertions after existing content
        if text.is_empty() || offset == 0 {
            return None;
        }

        // Find the piece containing the byte just before the insertion point
        // This avoids the saturating_sub issue
        let piece_info = self.piece_tree.find_by_offset(offset - 1)?;

        // Check if insertion is exactly at the end of this piece
        // offset_in_piece tells us where (offset-1) is within the piece
        // For insertion to be at piece end, (offset-1) must be the last byte
        let offset_in_piece = piece_info.offset_in_piece?;
        if offset_in_piece + 1 != piece_info.bytes {
            return None; // Not at the end of the piece
        }

        // Only append to "Added" buffers (not original Stored buffers)
        if !matches!(piece_info.location, BufferLocation::Added(_)) {
            return None;
        }

        let buffer_id = piece_info.location.buffer_id();
        let buffer = self.buffers.get_mut(buffer_id)?;

        // Check if buffer is loaded
        let buffer_len = buffer.get_data()?.len();

        // Check if this piece ends exactly at the end of its buffer
        if piece_info.offset + piece_info.bytes != buffer_len {
            return None;
        }

        // Perfect! Append to this buffer
        let append_offset = buffer.append(text);

        Some((piece_info.location, append_offset, text.len()))
    }

    /// Insert text (from &str) at the given byte offset
    pub fn insert(&mut self, offset: usize, text: &str) {
        self.insert_bytes(offset, text.as_bytes().to_vec());
    }

    /// Insert text at a line/column position
    /// This now uses the optimized piece_tree.insert_at_position() for a single traversal
    pub fn insert_at_position(&mut self, position: Position, text: Vec<u8>) -> Cursor {
        if text.is_empty() {
            let offset = self.position_to_offset(position);
            return self.piece_tree.cursor_at_offset(offset);
        }

        self.mark_content_modified();

        // Count line feeds in the text to insert
        let line_feed_cnt = text.iter().filter(|&&b| b == b'\n').count();

        // Create a new StringBuffer for this insertion
        let buffer_id = self.next_buffer_id;
        self.next_buffer_id += 1;
        let buffer = StringBuffer::new(buffer_id, text.clone());
        self.buffers.push(buffer);

        // Use the optimized position-based insertion (single traversal)
        self.piece_tree.insert_at_position(
            position.line,
            position.column,
            BufferLocation::Added(buffer_id),
            0,
            text.len(),
            line_feed_cnt,
            &self.buffers,
        )
    }

    /// Delete text starting at the given byte offset
    pub fn delete_bytes(&mut self, offset: usize, bytes: usize) {
        if bytes == 0 || offset >= self.total_bytes() {
            return;
        }

        // When line feeds have been scanned, ensure chunks at delete boundaries
        // are loaded so compute_line_feeds_static can recount during splits.
        if self.line_feeds_scanned {
            self.ensure_chunk_loaded_at(offset);
            let end = (offset + bytes).min(self.total_bytes());
            if end > offset {
                self.ensure_chunk_loaded_at(end.saturating_sub(1));
            }
        }

        // Update piece tree
        self.piece_tree.delete(offset, bytes, &self.buffers);

        self.mark_content_modified();
    }

    /// Delete text in a range
    pub fn delete(&mut self, range: Range<usize>) {
        if range.end > range.start {
            self.delete_bytes(range.start, range.end - range.start);
        }
    }

    /// Delete text in a line/column range
    /// This now uses the optimized piece_tree.delete_position_range() for a single traversal
    pub fn delete_range(&mut self, start: Position, end: Position) {
        // Use the optimized position-based deletion
        self.piece_tree.delete_position_range(
            start.line,
            start.column,
            end.line,
            end.column,
            &self.buffers,
        );
        self.mark_content_modified();
    }

    /// Replace the entire buffer content with new content
    /// This is an O(n) operation that rebuilds the piece tree in a single pass,
    /// avoiding the O(n²) complexity of applying individual edits.
    ///
    /// This is used for bulk operations like "replace all" where applying
    /// individual edits would be prohibitively slow.
    pub fn replace_content(&mut self, new_content: &str) {
        let bytes = new_content.len();
        let content_bytes = new_content.as_bytes().to_vec();

        // Count line feeds in the new content
        let line_feed_cnt = content_bytes.iter().filter(|&&b| b == b'\n').count();

        // Create a new StringBuffer for the new content
        let buffer_id = self.next_buffer_id;
        self.next_buffer_id += 1;
        let buffer = StringBuffer::new(buffer_id, content_bytes);
        self.buffers.push(buffer);

        // Rebuild the piece tree with a single piece containing all the new content
        if bytes > 0 {
            self.piece_tree = PieceTree::new(
                BufferLocation::Added(buffer_id),
                0,
                bytes,
                Some(line_feed_cnt),
            );
        } else {
            self.piece_tree = PieceTree::empty();
        }

        self.mark_content_modified();
    }

    /// Restore a previously saved buffer state (for undo/redo of BulkEdit).
    ///
    /// This restores the piece tree AND the buffers list, which is critical
    /// because consolidate_after_save() replaces self.buffers. Without restoring
    /// buffers, the piece tree would reference buffer IDs that no longer exist.
    pub fn restore_buffer_state(&mut self, snapshot: &BufferSnapshot) {
        self.piece_tree = snapshot.piece_tree.clone();
        self.buffers = snapshot.buffers.clone();
        self.next_buffer_id = snapshot.next_buffer_id;
        self.mark_content_modified();
    }

    /// Snapshot the current buffer state (piece tree + buffers) for BulkEdit undo/redo.
    ///
    /// The snapshot includes buffers because consolidate_after_save() can replace
    /// self.buffers between the snapshot and restore, which would otherwise cause
    /// the restored piece tree to reference nonexistent buffer IDs.
    pub fn snapshot_buffer_state(&self) -> Arc<BufferSnapshot> {
        Arc::new(BufferSnapshot {
            piece_tree: self.piece_tree.clone(),
            buffers: self.buffers.clone(),
            next_buffer_id: self.next_buffer_id,
        })
    }

    /// Apply bulk edits efficiently in a single pass
    /// Returns the net change in bytes
    pub fn apply_bulk_edits(&mut self, edits: &[(usize, usize, &str)]) -> isize {
        // Pre-allocate buffers for all insert texts (only non-empty texts)
        // This avoids the borrow conflict in the closure
        // IMPORTANT: Only add entries for non-empty texts because the closure
        // is only called for edits with non-empty insert text
        let mut buffer_info: Vec<(BufferLocation, usize, usize, Option<usize>)> = Vec::new();

        for (_, _, text) in edits {
            if !text.is_empty() {
                let buffer_id = self.next_buffer_id;
                self.next_buffer_id += 1;
                let content = text.as_bytes().to_vec();
                let lf_cnt = content.iter().filter(|&&b| b == b'\n').count();
                let bytes = content.len();
                let buffer = StringBuffer::new(buffer_id, content);
                self.buffers.push(buffer);
                buffer_info.push((BufferLocation::Added(buffer_id), 0, bytes, Some(lf_cnt)));
            }
            // No placeholder for empty texts - the closure is only called for non-empty texts
        }

        // Now call apply_bulk_edits with a simple index-based closure
        let mut idx = 0;
        let delta = self
            .piece_tree
            .apply_bulk_edits(edits, &self.buffers, |_text| {
                let info = buffer_info[idx];
                idx += 1;
                info
            });

        self.mark_content_modified();
        delta
    }

    /// Get text from a byte offset range
    /// This now uses the optimized piece_tree.iter_pieces_in_range() for a single traversal
    /// Get text from a byte offset range (read-only)
    /// Returns None if any buffer in the range is unloaded
    /// PRIVATE: External code should use get_text_range_mut() which handles lazy loading
    fn get_text_range(&self, offset: usize, bytes: usize) -> Option<Vec<u8>> {
        if bytes == 0 {
            return Some(Vec::new());
        }

        let mut result = Vec::with_capacity(bytes);
        let end_offset = offset + bytes;
        let mut collected = 0;

        // Use the efficient piece iterator (single O(log n) traversal + O(N) iteration)
        for piece_view in self.piece_tree.iter_pieces_in_range(offset, end_offset) {
            let buffer_id = piece_view.location.buffer_id();
            if let Some(buffer) = self.buffers.get(buffer_id) {
                // Calculate the range to read from this piece
                let piece_start_in_doc = piece_view.doc_offset;
                let piece_end_in_doc = piece_view.doc_offset + piece_view.bytes;

                // Clip to the requested range
                let read_start = offset.max(piece_start_in_doc);
                let read_end = end_offset.min(piece_end_in_doc);

                if read_end > read_start {
                    let offset_in_piece = read_start - piece_start_in_doc;
                    let bytes_to_read = read_end - read_start;

                    let buffer_start = piece_view.buffer_offset + offset_in_piece;
                    let buffer_end = buffer_start + bytes_to_read;

                    // Return None if buffer is unloaded (type-safe)
                    let data = buffer.get_data()?;

                    if buffer_end <= data.len() {
                        result.extend_from_slice(&data[buffer_start..buffer_end]);
                        collected += bytes_to_read;

                        if collected >= bytes {
                            break;
                        }
                    }
                }
            }
        }

        Some(result)
    }

    /// Get text from a byte offset range with lazy loading
    /// This will load unloaded chunks on-demand and always returns complete data
    ///
    /// Returns an error if loading fails or if data cannot be read for any reason.
    ///
    /// NOTE: Currently loads entire buffers on-demand. Future optimization would split
    /// large pieces and load only LOAD_CHUNK_SIZE chunks at a time.
    pub fn get_text_range_mut(&mut self, offset: usize, bytes: usize) -> Result<Vec<u8>> {
        let _span = tracing::info_span!("get_text_range_mut", offset, bytes).entered();
        if bytes == 0 {
            return Ok(Vec::new());
        }

        let mut result = Vec::with_capacity(bytes);
        // Clamp end_offset to buffer length to handle reads beyond EOF
        let end_offset = (offset + bytes).min(self.len());
        let mut current_offset = offset;
        let mut iteration_count = 0u32;

        // Keep iterating until we've collected all requested bytes
        while current_offset < end_offset {
            iteration_count += 1;
            let mut made_progress = false;
            let mut restarted_iteration = false;

            // Use the efficient piece iterator (single O(log n) traversal + O(N) iteration)
            for piece_view in self
                .piece_tree
                .iter_pieces_in_range(current_offset, end_offset)
            {
                let buffer_id = piece_view.location.buffer_id();

                // Check if buffer needs loading
                let needs_loading = self
                    .buffers
                    .get(buffer_id)
                    .map(|b| !b.is_loaded())
                    .unwrap_or(false);

                if needs_loading {
                    if self.chunk_split_and_load(&piece_view, current_offset)? {
                        restarted_iteration = true;
                        break;
                    }
                }

                // Calculate the range to read from this piece
                let piece_start_in_doc = piece_view.doc_offset;
                let piece_end_in_doc = piece_view.doc_offset + piece_view.bytes;

                // Clip to the requested range
                let read_start = current_offset.max(piece_start_in_doc);
                let read_end = end_offset.min(piece_end_in_doc);

                if read_end > read_start {
                    let offset_in_piece = read_start - piece_start_in_doc;
                    let bytes_to_read = read_end - read_start;

                    let buffer_start = piece_view.buffer_offset + offset_in_piece;
                    let buffer_end = buffer_start + bytes_to_read;

                    // Buffer should be loaded now
                    let buffer = self.buffers.get(buffer_id).context("Buffer not found")?;
                    let data = buffer
                        .get_data()
                        .context("Buffer data unavailable after load")?;

                    anyhow::ensure!(
                        buffer_end <= data.len(),
                        "Buffer range out of bounds: requested {}..{}, buffer size {}",
                        buffer_start,
                        buffer_end,
                        data.len()
                    );

                    result.extend_from_slice(&data[buffer_start..buffer_end]);
                    current_offset = read_end;
                    made_progress = true;
                }
            }

            // If we didn't make progress and didn't restart iteration, this is an error
            if !made_progress && !restarted_iteration {
                tracing::error!(
                    "get_text_range_mut: No progress at offset {} (requested range: {}..{}, buffer len: {})",
                    current_offset,
                    offset,
                    end_offset,
                    self.len()
                );
                tracing::error!(
                    "Piece tree stats: {} total bytes",
                    self.piece_tree.stats().total_bytes
                );
                anyhow::bail!(
                    "Failed to read data at offset {}: no progress made (requested {}..{}, buffer len: {})",
                    current_offset,
                    offset,
                    end_offset,
                    self.len()
                );
            }
        }

        if iteration_count > 1 {
            tracing::info!(
                iteration_count,
                result_len = result.len(),
                "get_text_range_mut: completed with multiple iterations"
            );
        }

        Ok(result)
    }

    /// Prepare a viewport for rendering
    ///
    /// This is called before rendering with &mut access to pre-load all data
    /// that will be needed for the viewport. It estimates the number of bytes
    /// needed based on the line count and pre-loads them.
    ///
    /// # Arguments
    /// * `start_offset` - The byte offset where the viewport starts
    /// * `line_count` - The number of lines to prepare (estimate)
    ///
    /// # Returns
    /// Ok(()) if preparation succeeded, Err if loading failed
    pub fn prepare_viewport(&mut self, start_offset: usize, line_count: usize) -> Result<()> {
        let _span = tracing::info_span!("prepare_viewport", start_offset, line_count).entered();
        // Estimate how many bytes we need (pessimistic assumption)
        // Average line length is typically 80-100 bytes, but we use 200 to be safe
        let estimated_bytes = line_count.saturating_mul(200);

        // Cap the estimate at the remaining bytes in the document
        let remaining_bytes = self.total_bytes().saturating_sub(start_offset);
        let bytes_to_load = estimated_bytes.min(remaining_bytes);
        tracing::info!(
            bytes_to_load,
            total_bytes = self.total_bytes(),
            "prepare_viewport loading"
        );

        // Pre-load with full chunk-splitting support
        // This may load more than we need, but ensures all data is available
        self.get_text_range_mut(start_offset, bytes_to_load)?;

        Ok(())
    }

    /// Split a piece that references a large unloaded buffer, create a chunk
    /// buffer for the region around `current_offset`, and load it.
    ///
    /// Returns `true` if the piece tree was modified (caller must restart its
    /// iteration), `false` if the piece was small enough to load in-place.
    fn chunk_split_and_load(
        &mut self,
        piece_view: &PieceView,
        current_offset: usize,
    ) -> Result<bool> {
        let buffer_id = piece_view.location.buffer_id();

        // The underlying buffer may be much larger than this piece (e.g. the
        // whole-file Stored buffer after rebuild_with_pristine_saved_root).
        // We must chunk-split if either the piece or its buffer exceeds
        // LOAD_CHUNK_SIZE, because `load()` loads the entire buffer.
        let buffer_bytes = self
            .buffers
            .get(buffer_id)
            .and_then(|b| b.unloaded_bytes())
            .unwrap_or(0);
        let needs_chunk_split =
            piece_view.bytes > LOAD_CHUNK_SIZE || buffer_bytes > piece_view.bytes;

        tracing::info!(
            buffer_id,
            piece_bytes = piece_view.bytes,
            buffer_bytes,
            needs_chunk_split,
            piece_doc_offset = piece_view.doc_offset,
            current_offset,
            "chunk_split_and_load: loading unloaded piece"
        );

        if !needs_chunk_split {
            // Piece is small enough and its buffer matches — load in-place.
            let _span = tracing::info_span!(
                "load_small_buffer",
                piece_bytes = piece_view.bytes,
                buffer_id,
            )
            .entered();
            self.buffers
                .get_mut(buffer_id)
                .context("Buffer not found")?
                .load(&*self.fs)
                .context("Failed to load buffer")?;
            return Ok(false);
        }

        let _span = tracing::info_span!(
            "chunk_split_and_load",
            piece_bytes = piece_view.bytes,
            buffer_id,
        )
        .entered();

        let piece_start_in_doc = piece_view.doc_offset;
        let offset_in_piece = current_offset.saturating_sub(piece_start_in_doc);

        // When the piece already fits within LOAD_CHUNK_SIZE, create a chunk
        // buffer for the exact piece range (no alignment/splitting needed).
        // Alignment rounding is only useful when carving a sub-range out of a
        // piece larger than LOAD_CHUNK_SIZE.
        let (chunk_start_in_buffer, chunk_bytes) = if piece_view.bytes <= LOAD_CHUNK_SIZE {
            (piece_view.buffer_offset, piece_view.bytes)
        } else {
            let start =
                (piece_view.buffer_offset + offset_in_piece) / CHUNK_ALIGNMENT * CHUNK_ALIGNMENT;
            let bytes = LOAD_CHUNK_SIZE
                .min((piece_view.buffer_offset + piece_view.bytes).saturating_sub(start));
            (start, bytes)
        };

        // Calculate document offsets for splitting
        let chunk_start_offset_in_piece =
            chunk_start_in_buffer.saturating_sub(piece_view.buffer_offset);
        let split_start_in_doc = piece_start_in_doc + chunk_start_offset_in_piece;
        let split_end_in_doc = split_start_in_doc + chunk_bytes;

        // Split the piece to isolate the chunk
        if chunk_start_offset_in_piece > 0 {
            self.piece_tree
                .split_at_offset(split_start_in_doc, &self.buffers);
        }
        if split_end_in_doc < piece_start_in_doc + piece_view.bytes {
            self.piece_tree
                .split_at_offset(split_end_in_doc, &self.buffers);
        }

        // Create a new buffer for this chunk
        let chunk_buffer = self
            .buffers
            .get(buffer_id)
            .context("Buffer not found")?
            .create_chunk_buffer(self.next_buffer_id, chunk_start_in_buffer, chunk_bytes)
            .context("Failed to create chunk buffer")?;

        self.next_buffer_id += 1;
        let new_buffer_id = chunk_buffer.id;
        self.buffers.push(chunk_buffer);

        // Update the piece to reference the new chunk buffer
        self.piece_tree.replace_buffer_reference(
            buffer_id,
            piece_view.buffer_offset + chunk_start_offset_in_piece,
            chunk_bytes,
            BufferLocation::Added(new_buffer_id),
        );

        // Load the chunk buffer
        self.buffers
            .get_mut(new_buffer_id)
            .context("Chunk buffer not found")?
            .load(&*self.fs)
            .context("Failed to load chunk")?;

        // split_at_offset uses compute_line_feeds_static which returns None
        // for unloaded buffers, destroying the scanned line feed counts.
        // Fix up: the loaded chunk is counted from memory, remaining unloaded
        // pieces use the filesystem's count_line_feeds_in_range.
        if self.line_feeds_scanned {
            let leaves = self.piece_tree.get_leaves();
            let mut fixups: Vec<(usize, usize)> = Vec::new();
            for (idx, leaf) in leaves.iter().enumerate() {
                if leaf.line_feed_cnt.is_none() {
                    if let Ok(count) = self.scan_leaf(leaf) {
                        fixups.push((idx, count));
                    }
                }
            }
            if !fixups.is_empty() {
                self.piece_tree.update_leaf_line_feeds_path_copy(&fixups);
            }
        }

        // Keep saved_root in sync with viewport-loading tree restructures so
        // that diff_since_saved() can match by (location, offset) identity.
        //
        // When !modified the current tree IS the saved state, so just snapshot.
        // When modified, we must apply the same Stored→Added leaf replacement
        // to saved_root so the diff doesn't see loaded-but-unedited regions as
        // changed.
        if !self.modified {
            self.saved_root = self.piece_tree.root();
        } else {
            self.apply_chunk_load_to_saved_root(
                buffer_id,
                chunk_start_in_buffer,
                chunk_bytes,
                new_buffer_id,
            );
        }

        Ok(true)
    }

    /// Get all text as a single Vec<u8>
    /// Returns None if any buffers are unloaded (lazy loading)
    /// CRATE-PRIVATE: External code should use get_text_range_mut() or DocumentModel methods
    pub(crate) fn get_all_text(&self) -> Option<Vec<u8>> {
        self.get_text_range(0, self.total_bytes())
    }

    /// Get all text as a String
    /// Returns None if any buffers are unloaded (lazy loading)
    /// CRATE-PRIVATE: External code should use get_text_range_mut() or DocumentModel methods
    pub(crate) fn get_all_text_string(&self) -> Option<String> {
        self.get_all_text()
            .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
    }

    /// Get text from a byte range as bytes
    /// CRATE-PRIVATE: Returns empty vector if any buffers are unloaded (silently fails!)
    /// Only use this when you KNOW the data is loaded (e.g., for syntax highlighting small regions)
    /// External code should use get_text_range_mut() or DocumentModel methods
    pub(crate) fn slice_bytes(&self, range: Range<usize>) -> Vec<u8> {
        self.get_text_range(range.start, range.end.saturating_sub(range.start))
            .unwrap_or_default()
    }

    /// Get all text as a String
    /// Returns None if any buffers are unloaded (lazy loading)
    pub fn to_string(&self) -> Option<String> {
        self.get_all_text_string()
    }

    /// Get the total number of bytes
    pub fn len(&self) -> usize {
        self.total_bytes()
    }

    /// Check if the buffer is empty
    pub fn is_empty(&self) -> bool {
        self.total_bytes() == 0
    }

    /// Get the file path associated with this buffer
    pub fn file_path(&self) -> Option<&Path> {
        self.file_path.as_deref()
    }

    /// Update the file path after a rename operation on disk.
    pub fn rename_file_path(&mut self, path: PathBuf) {
        self.file_path = Some(path);
    }

    /// Clear the file path (make buffer unnamed)
    /// Note: This does NOT affect Unloaded chunk file_paths used for lazy loading.
    /// Those still point to the original source file for chunk loading.
    pub fn clear_file_path(&mut self) {
        self.file_path = None;
    }

    /// Extend buffer to include more bytes from a streaming source file.
    /// Used for stdin streaming where the temp file grows over time.
    /// Appends a new Unloaded chunk for the new bytes.
    pub fn extend_streaming(&mut self, source_path: &Path, new_size: usize) {
        let old_size = self.total_bytes();
        if new_size <= old_size {
            return;
        }

        let additional_bytes = new_size - old_size;

        // Create new Unloaded buffer for the appended region
        let buffer_id = self.next_buffer_id;
        self.next_buffer_id += 1;

        let new_buffer = StringBuffer::new_unloaded(
            buffer_id,
            source_path.to_path_buf(),
            old_size,         // file_offset - where this chunk starts in the file
            additional_bytes, // bytes - size of this chunk
        );
        self.buffers.push(new_buffer);

        // Append piece at end of document (insert at offset == total_bytes)
        self.piece_tree.insert(
            old_size,
            BufferLocation::Stored(buffer_id),
            0,
            additional_bytes,
            None, // line_feed_cnt unknown for unloaded chunk
            &self.buffers,
        );
    }

    /// Check if the buffer has been modified since last save
    pub fn is_modified(&self) -> bool {
        self.modified
    }

    /// Clear the modified flag (after save)
    pub fn clear_modified(&mut self) {
        self.modified = false;
    }

    /// Set the modified flag explicitly
    /// Used by undo/redo to restore the correct modified state
    pub fn set_modified(&mut self, modified: bool) {
        self.modified = modified;
    }

    /// Check if buffer has pending changes for recovery auto-save
    pub fn is_recovery_pending(&self) -> bool {
        self.recovery_pending
    }

    /// Mark buffer as needing recovery auto-save (call after edits)
    pub fn set_recovery_pending(&mut self, pending: bool) {
        self.recovery_pending = pending;
    }

    /// Ensure the buffer chunk at the given byte offset is loaded.
    ///
    /// When `line_feeds_scanned` is true, piece splits during insert/delete need
    /// the buffer data to be loaded so `compute_line_feeds_static` can accurately
    /// recount line feeds for each half. This method loads the chunk if needed.
    fn ensure_chunk_loaded_at(&mut self, offset: usize) {
        if let Some(piece_info) = self.piece_tree.find_by_offset(offset) {
            let buffer_id = piece_info.location.buffer_id();
            if let Some(buffer) = self.buffers.get_mut(buffer_id) {
                if !buffer.is_loaded() {
                    let buf_bytes = buffer.unloaded_bytes().unwrap_or(0);
                    tracing::info!(
                        "ensure_chunk_loaded_at: loading buffer {} ({} bytes) for offset {}",
                        buffer_id,
                        buf_bytes,
                        offset
                    );
                    if let Err(e) = buffer.load(&*self.fs) {
                        tracing::warn!("Failed to load chunk at offset {offset}: {e}");
                    }
                }
            }
        }
    }

    /// Check if this is a large file with lazy loading enabled
    pub fn is_large_file(&self) -> bool {
        self.large_file
    }

    /// Check if line feeds have been scanned for this large file.
    /// When true, `line_count()` returns exact values.
    pub fn has_line_feed_scan(&self) -> bool {
        self.line_feeds_scanned
    }

    /// Get the raw piece tree leaves (for storing alongside scan chunks).
    pub fn piece_tree_leaves(&self) -> Vec<crate::model::piece_tree::LeafData> {
        self.piece_tree.get_leaves()
    }

    /// Prepare work items for an incremental line scan.
    ///
    /// First splits any oversized leaves in the piece tree so every leaf is
    /// at most `LOAD_CHUNK_SIZE` bytes.  Then returns one work item per leaf.
    /// After scanning, `get_text_range_mut` will never need to split a scanned
    /// leaf (it's already chunk-sized), so line-feed counts are preserved.
    ///
    /// Returns `(chunks, total_bytes)`.
    pub fn prepare_line_scan(&mut self) -> (Vec<LineScanChunk>, usize) {
        // Pre-split the tree so every leaf ≤ LOAD_CHUNK_SIZE.
        self.piece_tree.split_leaves_to_chunk_size(LOAD_CHUNK_SIZE);

        let leaves = self.piece_tree.get_leaves();
        let total_bytes: usize = leaves.iter().map(|l| l.bytes).sum();
        let mut chunks = Vec::new();

        for (idx, leaf) in leaves.iter().enumerate() {
            chunks.push(LineScanChunk {
                leaf_index: idx,
                byte_len: leaf.bytes,
                already_known: leaf.line_feed_cnt.is_some(),
            });
        }

        (chunks, total_bytes)
    }

    /// Count `\n` bytes in a single leaf.
    ///
    /// Uses `count_line_feeds_in_range` for unloaded buffers, which remote
    /// filesystem implementations can override to count server-side.
    pub fn scan_leaf(&self, leaf: &crate::model::piece_tree::LeafData) -> std::io::Result<usize> {
        let buffer_id = leaf.location.buffer_id();
        let buffer = self
            .buffers
            .get(buffer_id)
            .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "buffer not found"))?;

        let count = match &buffer.data {
            crate::model::piece_tree::BufferData::Loaded { data, .. } => {
                let end = (leaf.offset + leaf.bytes).min(data.len());
                data[leaf.offset..end]
                    .iter()
                    .filter(|&&b| b == b'\n')
                    .count()
            }
            crate::model::piece_tree::BufferData::Unloaded {
                file_path,
                file_offset,
                ..
            } => {
                let read_offset = *file_offset as u64 + leaf.offset as u64;
                self.fs
                    .count_line_feeds_in_range(file_path, read_offset, leaf.bytes)?
            }
        };
        Ok(count)
    }

    /// Return the I/O parameters for an unloaded leaf, or `None` if loaded.
    ///
    /// Used by the incremental scan to distinguish leaves that can be counted
    /// in-memory (via `scan_leaf`) from those that need filesystem I/O.
    pub fn leaf_io_params(
        &self,
        leaf: &crate::model::piece_tree::LeafData,
    ) -> Option<(std::path::PathBuf, u64, usize)> {
        let buffer_id = leaf.location.buffer_id();
        let buffer = self.buffers.get(buffer_id)?;
        match &buffer.data {
            crate::model::piece_tree::BufferData::Loaded { .. } => None,
            crate::model::piece_tree::BufferData::Unloaded {
                file_path,
                file_offset,
                ..
            } => {
                let read_offset = *file_offset as u64 + leaf.offset as u64;
                Some((file_path.clone(), read_offset, leaf.bytes))
            }
        }
    }

    /// Get a reference to the string buffers (for parallel scanning).
    pub fn buffer_slice(&self) -> &[StringBuffer] {
        &self.buffers
    }

    /// Apply the results of an incremental line scan.
    pub fn apply_scan_updates(&mut self, updates: &[(usize, usize)]) {
        self.piece_tree.update_leaf_line_feeds(updates);
        self.line_feeds_scanned = true;
    }

    /// After an incremental line-feed scan completes, rebuild the tree so that
    /// `saved_root` and the current tree share `Arc` pointers for unedited
    /// subtrees. This makes `diff_since_saved()` O(edited regions) instead of
    /// O(file size).
    pub fn rebuild_with_pristine_saved_root(&mut self, scan_updates: &[(usize, usize)]) {
        let file_size = match self.saved_file_size {
            Some(s) => s,
            None => {
                // Fallback: no saved file size means we can't build a pristine
                // tree. Just apply updates the old way.
                self.apply_scan_updates(scan_updates);
                return;
            }
        };

        // --- Walk the current tree to extract deletions and insertions ---
        let total = self.total_bytes();
        // Deletions: gaps in Stored coverage (orig_offset, len).
        let mut deletions: Vec<(usize, usize)> = Vec::new();
        // Insertions: (post_delete_offset, location, buf_offset, bytes, lf_cnt).
        // post_delete_offset = cumulative surviving Stored bytes before this point.
        let mut insertions: Vec<(usize, BufferLocation, usize, usize, Option<usize>)> = Vec::new();
        let mut orig_cursor: usize = 0;
        let mut stored_bytes_in_doc: usize = 0;

        for piece in self.piece_tree.iter_pieces_in_range(0, total) {
            match piece.location {
                BufferLocation::Stored(_) => {
                    if piece.buffer_offset > orig_cursor {
                        deletions.push((orig_cursor, piece.buffer_offset - orig_cursor));
                    }
                    orig_cursor = piece.buffer_offset + piece.bytes;
                    stored_bytes_in_doc += piece.bytes;
                }
                BufferLocation::Added(id) => {
                    // Check if this Added buffer was created by loading a chunk
                    // from the stored file (via get_text_range_mut chunk loading).
                    // If so, treat it as stored content, not a user edit.
                    if let Some(file_off) = self.buffers.get(id).and_then(|b| b.stored_file_offset)
                    {
                        if file_off > orig_cursor {
                            deletions.push((orig_cursor, file_off - orig_cursor));
                        }
                        orig_cursor = file_off + piece.bytes;
                        stored_bytes_in_doc += piece.bytes;
                    } else {
                        insertions.push((
                            stored_bytes_in_doc,
                            piece.location,
                            piece.buffer_offset,
                            piece.bytes,
                            piece.line_feed_cnt,
                        ));
                    }
                }
            }
        }
        // Trailing deletion.
        if orig_cursor < file_size {
            deletions.push((orig_cursor, file_size - orig_cursor));
        }

        // --- Build pristine tree (full original file, pre-split, with lf counts) ---
        let mut pristine = if file_size > 0 {
            PieceTree::new(BufferLocation::Stored(0), 0, file_size, None)
        } else {
            PieceTree::empty()
        };
        pristine.split_leaves_to_chunk_size(LOAD_CHUNK_SIZE);
        pristine.update_leaf_line_feeds(scan_updates);

        // Snapshot the pristine tree as saved_root.
        self.saved_root = pristine.root();

        // If no edits, the pristine tree IS the current tree.
        if deletions.is_empty() && insertions.is_empty() {
            self.piece_tree = pristine;
            self.line_feeds_scanned = true;
            return;
        }

        // --- Replay edits onto a clone of the pristine tree ---
        let mut tree = pristine;

        // Apply deletions from HIGH to LOW offset so earlier offsets stay valid.
        deletions.sort_by(|a, b| b.0.cmp(&a.0));
        for &(offset, len) in &deletions {
            tree.delete(offset, len, &self.buffers);
        }

        // Apply insertions from LOW to HIGH. Each insertion shifts subsequent
        // offsets by its byte count, tracked via insert_delta.
        let mut insert_delta: usize = 0;
        for &(offset, location, buf_offset, bytes, lf_cnt) in &insertions {
            tree.insert(
                offset + insert_delta,
                location,
                buf_offset,
                bytes,
                lf_cnt,
                &self.buffers,
            );
            insert_delta += bytes;
        }

        // Path-copy insert/delete may split Stored leaves whose data is
        // Unloaded, producing fragments with line_feed_cnt = None
        // (compute_line_feeds_static can't read unloaded data). Fix them up
        // by scanning any remaining None leaves.
        let leaves = tree.get_leaves();
        let mut fixups: Vec<(usize, usize)> = Vec::new();
        for (idx, leaf) in leaves.iter().enumerate() {
            if leaf.line_feed_cnt.is_none() {
                if let Ok(count) = self.scan_leaf(leaf) {
                    fixups.push((idx, count));
                }
            }
        }
        if !fixups.is_empty() {
            tree.update_leaf_line_feeds_path_copy(&fixups);
        }

        self.piece_tree = tree;
        self.line_feeds_scanned = true;
    }

    /// Resolve the exact byte offset for a given line number (0-indexed).
    ///
    /// Uses the tree's line feed counts to find the piece containing the target line,
    /// then loads/reads that piece's data to find the exact newline position.
    /// This works even when buffers are unloaded (large file with scanned line index).
    pub fn resolve_line_byte_offset(&mut self, target_line: usize) -> Option<usize> {
        if target_line == 0 {
            return Some(0);
        }

        // Use tree metadata to find the piece containing the target line
        let (doc_offset, buffer_id, piece_offset, piece_bytes, lines_before) =
            self.piece_tree.piece_info_for_line(target_line)?;

        // We need to find the (target_line - lines_before)-th newline within this piece
        let lines_to_skip = target_line - lines_before;

        // Get the piece data — either from loaded buffer or read from disk
        let buffer = self.buffers.get(buffer_id)?;
        let piece_data: Vec<u8> = match &buffer.data {
            crate::model::piece_tree::BufferData::Loaded { data, .. } => {
                let end = (piece_offset + piece_bytes).min(data.len());
                data[piece_offset..end].to_vec()
            }
            crate::model::piece_tree::BufferData::Unloaded {
                file_path,
                file_offset,
                ..
            } => {
                let read_offset = *file_offset as u64 + piece_offset as u64;
                self.fs
                    .read_range(file_path, read_offset, piece_bytes)
                    .ok()?
            }
        };

        // Count newlines to find the target line start
        let mut newlines_found = 0;
        for (i, &byte) in piece_data.iter().enumerate() {
            if byte == b'\n' {
                newlines_found += 1;
                if newlines_found == lines_to_skip {
                    // The target line starts right after this newline
                    return Some(doc_offset + i + 1);
                }
            }
        }

        // If we didn't find enough newlines, the line starts in the next piece
        // Return the end of this piece as an approximation
        Some(doc_offset + piece_bytes)
    }

    /// Get the saved file size (size of the file on disk after last load/save)
    /// For large files, this is used during recovery to know the expected original file size.
    /// Returns None for new unsaved buffers.
    pub fn original_file_size(&self) -> Option<usize> {
        // Return the tracked saved file size - this is updated when the file is
        // loaded or saved, so it always reflects the current file on disk.
        self.saved_file_size
    }

    /// Get recovery chunks for this buffer (only modified portions)
    ///
    /// For large files, this returns only the pieces that come from Added buffers
    /// (i.e., the modifications), not the original file content. This allows
    /// efficient incremental recovery without reading/writing the entire file.
    ///
    /// Returns: Vec of (original_file_offset, data) for each modified chunk
    /// The offset is the position in the ORIGINAL file where this chunk should be inserted.
    pub fn get_recovery_chunks(&self) -> Vec<(usize, Vec<u8>)> {
        use crate::model::piece_tree::BufferLocation;

        let mut chunks = Vec::new();
        let total = self.total_bytes();

        // Track cumulative bytes from Stored pieces as we iterate.
        // This gives us the original file offset for Added pieces.
        // The key insight: Added pieces should be inserted at the position
        // corresponding to where they appear relative to Stored content,
        // not their position in the current document.
        let mut stored_bytes_before = 0;

        for piece in self.piece_tree.iter_pieces_in_range(0, total) {
            match piece.location {
                BufferLocation::Stored(_) => {
                    // Accumulate stored bytes to track position in original file
                    stored_bytes_before += piece.bytes;
                }
                BufferLocation::Added(buffer_id) => {
                    if let Some(buffer) = self.buffers.iter().find(|b| b.id == buffer_id) {
                        // Get the data from the buffer if loaded
                        if let Some(data) = buffer.get_data() {
                            // Extract just the portion this piece references
                            let start = piece.buffer_offset;
                            let end = start + piece.bytes;
                            if end <= data.len() {
                                // Use stored_bytes_before as the original file offset.
                                // This is where this insertion should go relative to
                                // the original file content.
                                chunks.push((stored_bytes_before, data[start..end].to_vec()));
                            }
                        }
                    }
                }
            }
        }

        chunks
    }

    /// Check if this buffer contains binary content
    pub fn is_binary(&self) -> bool {
        self.is_binary
    }

    /// Get the line ending format for this buffer
    pub fn line_ending(&self) -> LineEnding {
        self.line_ending
    }

    /// Set the line ending format for this buffer
    ///
    /// This marks the buffer as modified since the line ending format has changed.
    /// On save, the buffer content will be converted to the new format.
    pub fn set_line_ending(&mut self, line_ending: LineEnding) {
        self.line_ending = line_ending;
        self.mark_content_modified();
    }

    /// Set the default line ending format for a new/empty buffer
    ///
    /// Unlike `set_line_ending`, this does NOT mark the buffer as modified.
    /// This should be used when initializing a new buffer with a configured default.
    pub fn set_default_line_ending(&mut self, line_ending: LineEnding) {
        self.line_ending = line_ending;
        self.original_line_ending = line_ending;
    }

    /// Get the encoding format for this buffer
    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    /// Set the encoding format for this buffer
    ///
    /// This marks the buffer as modified since the encoding format has changed.
    /// On save, the buffer content will be converted to the new encoding.
    pub fn set_encoding(&mut self, encoding: Encoding) {
        self.encoding = encoding;
        self.mark_content_modified();
    }

    /// Set the default encoding format for a new/empty buffer
    ///
    /// Unlike `set_encoding`, this does NOT mark the buffer as modified.
    /// This should be used when initializing a new buffer with a configured default.
    pub fn set_default_encoding(&mut self, encoding: Encoding) {
        self.encoding = encoding;
        self.original_encoding = encoding;
    }

    /// Detect the line ending format from a sample of bytes
    ///
    /// Uses majority voting: counts CRLF, LF-only, and CR-only occurrences
    /// and returns the most common format.
    pub fn detect_line_ending(bytes: &[u8]) -> LineEnding {
        // Only check the first 8KB for line ending detection (same as binary detection)
        let check_len = bytes.len().min(8 * 1024);
        let sample = &bytes[..check_len];

        let mut crlf_count = 0;
        let mut lf_only_count = 0;
        let mut cr_only_count = 0;

        let mut i = 0;
        while i < sample.len() {
            if sample[i] == b'\r' {
                // Check if this is CRLF
                if i + 1 < sample.len() && sample[i + 1] == b'\n' {
                    crlf_count += 1;
                    i += 2; // Skip both \r and \n
                    continue;
                } else {
                    // CR only (old Mac format)
                    cr_only_count += 1;
                }
            } else if sample[i] == b'\n' {
                // LF only (Unix format)
                lf_only_count += 1;
            }
            i += 1;
        }

        // Use majority voting to determine line ending
        if crlf_count > lf_only_count && crlf_count > cr_only_count {
            LineEnding::CRLF
        } else if cr_only_count > lf_only_count && cr_only_count > crlf_count {
            LineEnding::CR
        } else {
            // Default to LF if no clear winner or if LF wins
            LineEnding::LF
        }
    }

    /// Detect the text encoding from a sample of bytes
    ///
    /// Delegates to the encoding module. Use `detect_encoding_or_binary`
    /// when you need to know if the content should be treated as binary.
    pub fn detect_encoding(bytes: &[u8]) -> Encoding {
        encoding::detect_encoding(bytes)
    }

    /// Detect the text encoding and whether content is binary.
    ///
    /// Returns (Encoding, is_binary) where:
    /// - Encoding is the detected encoding (or default if binary)
    /// - is_binary is true if the content should be treated as raw binary
    ///
    /// Delegates to the encoding module for detection logic.
    pub fn detect_encoding_or_binary(bytes: &[u8]) -> (Encoding, bool) {
        encoding::detect_encoding_or_binary(bytes)
    }

    /// Detect encoding and convert bytes to UTF-8
    ///
    /// Returns the detected encoding and the UTF-8 converted content.
    /// This is the core function for normalizing file content to UTF-8 on load.
    pub fn detect_and_convert_encoding(bytes: &[u8]) -> (Encoding, Vec<u8>) {
        encoding::detect_and_convert(bytes)
    }

    /// Convert UTF-8 content to the specified encoding for saving
    ///
    /// Used when saving files to convert internal UTF-8 representation
    /// back to the original (or user-selected) encoding.
    /// Note: This does NOT add BOM - the BOM is handled separately in build_write_recipe.
    pub fn convert_to_encoding(utf8_bytes: &[u8], target_encoding: Encoding) -> Vec<u8> {
        encoding::convert_from_utf8(utf8_bytes, target_encoding)
    }

    /// Normalize line endings in the given bytes to LF only
    ///
    /// Converts CRLF (\r\n) and CR (\r) to LF (\n) for internal representation.
    /// This makes editing and cursor movement simpler while preserving the
    /// original format for saving.
    #[allow(dead_code)] // Kept for tests and potential future use
    pub fn normalize_line_endings(bytes: Vec<u8>) -> Vec<u8> {
        let mut normalized = Vec::with_capacity(bytes.len());
        let mut i = 0;

        while i < bytes.len() {
            if bytes[i] == b'\r' {
                // Check if this is CRLF
                if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                    // CRLF -> LF
                    normalized.push(b'\n');
                    i += 2; // Skip both \r and \n
                    continue;
                } else {
                    // CR only -> LF
                    normalized.push(b'\n');
                }
            } else {
                // Copy byte as-is
                normalized.push(bytes[i]);
            }
            i += 1;
        }

        normalized
    }

    /// Convert line endings from any source format to any target format
    ///
    /// This first normalizes all line endings to LF, then converts to the target format.
    /// Used when saving files after the user has changed the line ending format.
    fn convert_line_endings_to(bytes: &[u8], target_ending: LineEnding) -> Vec<u8> {
        // First pass: normalize everything to LF
        let mut normalized = Vec::with_capacity(bytes.len());
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'\r' {
                // Check if this is CRLF
                if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                    // CRLF -> LF
                    normalized.push(b'\n');
                    i += 2;
                    continue;
                } else {
                    // CR only -> LF
                    normalized.push(b'\n');
                }
            } else {
                normalized.push(bytes[i]);
            }
            i += 1;
        }

        // If target is LF, we're done
        if target_ending == LineEnding::LF {
            return normalized;
        }

        // Second pass: convert LF to target format
        let replacement = target_ending.as_str().as_bytes();
        let mut result = Vec::with_capacity(normalized.len() + normalized.len() / 10);

        for byte in normalized {
            if byte == b'\n' {
                result.extend_from_slice(replacement);
            } else {
                result.push(byte);
            }
        }

        result
    }

    /// Get text for a specific line
    pub fn get_line(&self, line: usize) -> Option<Vec<u8>> {
        let (start, end) = self.piece_tree.line_range(line, &self.buffers)?;

        let bytes = if let Some(end_offset) = end {
            end_offset.saturating_sub(start)
        } else {
            self.total_bytes().saturating_sub(start)
        };

        self.get_text_range(start, bytes)
    }

    /// Get the byte offset where a line starts
    pub fn line_start_offset(&self, line: usize) -> Option<usize> {
        let (start, _) = self.piece_tree.line_range(line, &self.buffers)?;
        Some(start)
    }

    /// Get piece information at a byte offset
    pub fn piece_info_at_offset(&self, offset: usize) -> Option<PieceInfo> {
        self.piece_tree.find_by_offset(offset)
    }

    /// Get tree statistics for debugging
    pub fn stats(&self) -> TreeStats {
        self.piece_tree.stats()
    }

    // Search and Replace Operations

    /// Find the next occurrence of a pattern, with wrap-around
    pub fn find_next(&self, pattern: &str, start_pos: usize) -> Option<usize> {
        if pattern.is_empty() {
            return None;
        }

        let pattern_bytes = pattern.as_bytes();
        let buffer_len = self.len();

        // Search from start_pos to end
        if start_pos < buffer_len {
            if let Some(offset) = self.find_pattern(start_pos, buffer_len, pattern_bytes) {
                return Some(offset);
            }
        }

        // Wrap around: search from beginning to start_pos
        if start_pos > 0 {
            if let Some(offset) = self.find_pattern(0, start_pos, pattern_bytes) {
                return Some(offset);
            }
        }

        None
    }

    /// Find the next occurrence of a pattern within an optional range
    /// If range is None, searches the entire buffer with wrap-around (same as find_next)
    /// If range is Some, searches only within that range without wrap-around
    pub fn find_next_in_range(
        &self,
        pattern: &str,
        start_pos: usize,
        range: Option<Range<usize>>,
    ) -> Option<usize> {
        if pattern.is_empty() {
            return None;
        }

        if let Some(search_range) = range {
            // Search within range only, no wrap-around
            let pattern_bytes = pattern.as_bytes();
            let search_start = start_pos.max(search_range.start);
            let search_end = search_range.end.min(self.len());

            if search_start < search_end {
                self.find_pattern(search_start, search_end, pattern_bytes)
            } else {
                None
            }
        } else {
            // No range specified, use normal find_next with wrap-around
            self.find_next(pattern, start_pos)
        }
    }

    /// Find pattern in a byte range using overlapping chunks
    fn find_pattern(&self, start: usize, end: usize, pattern: &[u8]) -> Option<usize> {
        if pattern.is_empty() || start >= end {
            return None;
        }

        const CHUNK_SIZE: usize = 65536; // 64KB chunks
        let overlap = pattern.len().saturating_sub(1).max(1);

        // Use the overlapping chunks iterator for efficient streaming search
        let chunks = OverlappingChunks::new(self, start, end, CHUNK_SIZE, overlap);

        for chunk in chunks {
            // Search the entire chunk buffer
            if let Some(pos) = Self::find_in_bytes(&chunk.buffer, pattern) {
                let match_end = pos + pattern.len();
                // Only report if match ENDS in or after the valid zone
                // This ensures patterns spanning boundaries are found exactly once
                if match_end > chunk.valid_start {
                    let absolute_pos = chunk.absolute_pos + pos;
                    // Verify the match doesn't extend beyond our search range
                    if absolute_pos + pattern.len() <= end {
                        return Some(absolute_pos);
                    }
                }
            }
        }

        None
    }

    /// Simple byte pattern search using naive algorithm
    fn find_in_bytes(haystack: &[u8], needle: &[u8]) -> Option<usize> {
        if needle.is_empty() || needle.len() > haystack.len() {
            return None;
        }

        (0..=haystack.len() - needle.len()).find(|&i| &haystack[i..i + needle.len()] == needle)
    }

    /// Find the next occurrence of a regex pattern, with wrap-around
    pub fn find_next_regex(&self, regex: &Regex, start_pos: usize) -> Option<usize> {
        let buffer_len = self.len();

        // Search from start_pos to end
        if start_pos < buffer_len {
            if let Some(offset) = self.find_regex(start_pos, buffer_len, regex) {
                return Some(offset);
            }
        }

        // Wrap around: search from beginning to start_pos
        if start_pos > 0 {
            if let Some(offset) = self.find_regex(0, start_pos, regex) {
                return Some(offset);
            }
        }

        None
    }

    /// Find the next occurrence of a regex pattern within an optional range
    pub fn find_next_regex_in_range(
        &self,
        regex: &Regex,
        start_pos: usize,
        range: Option<Range<usize>>,
    ) -> Option<usize> {
        if let Some(search_range) = range {
            let search_start = start_pos.max(search_range.start);
            let search_end = search_range.end.min(self.len());

            if search_start < search_end {
                self.find_regex(search_start, search_end, regex)
            } else {
                None
            }
        } else {
            self.find_next_regex(regex, start_pos)
        }
    }

    /// Find regex pattern in a byte range using overlapping chunks
    fn find_regex(&self, start: usize, end: usize, regex: &Regex) -> Option<usize> {
        if start >= end {
            return None;
        }

        const CHUNK_SIZE: usize = 1048576; // 1MB chunks
        const OVERLAP: usize = 4096; // 4KB overlap for regex

        // Use the overlapping chunks iterator for efficient streaming search
        // This fixes the critical bug where regex patterns spanning chunk boundaries were missed
        let chunks = OverlappingChunks::new(self, start, end, CHUNK_SIZE, OVERLAP);

        for chunk in chunks {
            // Search the entire chunk buffer
            if let Some(mat) = regex.find(&chunk.buffer) {
                let match_end = mat.end();
                // Only report if match ENDS in or after the valid zone
                // This ensures patterns spanning boundaries are found exactly once
                if match_end > chunk.valid_start {
                    let absolute_pos = chunk.absolute_pos + mat.start();
                    // Verify the match doesn't extend beyond our search range
                    let match_len = mat.end() - mat.start();
                    if absolute_pos + match_len <= end {
                        return Some(absolute_pos);
                    }
                }
            }
        }

        None
    }

    /// Replace a range with replacement text
    pub fn replace_range(&mut self, range: Range<usize>, replacement: &str) -> bool {
        if range.start >= self.len() {
            return false;
        }

        let end = range.end.min(self.len());
        if end > range.start {
            self.delete_bytes(range.start, end - range.start);
        }

        if !replacement.is_empty() {
            self.insert(range.start, replacement);
        }

        true
    }

    /// Find and replace the next occurrence of a pattern
    pub fn replace_next(
        &mut self,
        pattern: &str,
        replacement: &str,
        start_pos: usize,
        range: Option<Range<usize>>,
    ) -> Option<usize> {
        if let Some(pos) = self.find_next_in_range(pattern, start_pos, range.clone()) {
            self.replace_range(pos..pos + pattern.len(), replacement);
            Some(pos)
        } else {
            None
        }
    }

    /// Replace all occurrences of a pattern with replacement text
    pub fn replace_all(&mut self, pattern: &str, replacement: &str) -> usize {
        if pattern.is_empty() {
            return 0;
        }

        let mut count = 0;
        let mut pos = 0;

        // Keep searching and replacing
        // Note: we search forward from last replacement to handle growth/shrinkage
        // Find next occurrence (no wrap-around for replace_all)
        while let Some(found_pos) = self.find_next_in_range(pattern, pos, Some(0..self.len())) {
            self.replace_range(found_pos..found_pos + pattern.len(), replacement);
            count += 1;

            // Move past the replacement
            pos = found_pos + replacement.len();

            // If we're at or past the end, stop
            if pos >= self.len() {
                break;
            }
        }

        count
    }

    /// Replace all occurrences of a regex pattern with replacement text
    pub fn replace_all_regex(&mut self, regex: &Regex, replacement: &str) -> Result<usize> {
        let mut count = 0;
        let mut pos = 0;

        while let Some(found_pos) = self.find_next_regex_in_range(regex, pos, Some(0..self.len())) {
            // Get the match to find its length
            let text = self
                .get_text_range_mut(found_pos, self.len() - found_pos)
                .context("Failed to read text for regex match")?;

            if let Some(mat) = regex.find(&text) {
                self.replace_range(found_pos..found_pos + mat.len(), replacement);
                count += 1;
                pos = found_pos + replacement.len();

                if pos >= self.len() {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(count)
    }

    // LSP Support (UTF-16 conversions)

    /// Convert byte position to (line, column) in bytes
    pub fn position_to_line_col(&self, byte_pos: usize) -> (usize, usize) {
        self.offset_to_position(byte_pos)
            .map(|pos| (pos.line, pos.column))
            .unwrap_or_else(|| (byte_pos / 80, 0)) // Estimate if metadata unavailable
    }

    /// Convert (line, character) to byte position - 0-indexed
    /// character is in BYTES, not UTF-16 code units
    /// Optimized to use single line_range() call instead of two
    pub fn line_col_to_position(&self, line: usize, character: usize) -> usize {
        if let Some((start, end)) = self.piece_tree.line_range(line, &self.buffers) {
            // Calculate line length from the range
            let line_len = if let Some(end_offset) = end {
                end_offset.saturating_sub(start)
            } else {
                self.total_bytes().saturating_sub(start)
            };
            let byte_offset = character.min(line_len);
            start + byte_offset
        } else {
            // Line doesn't exist, return end of buffer
            self.len()
        }
    }

    /// Convert byte position to LSP position (line, UTF-16 code units)
    /// LSP protocol uses UTF-16 code units for character offsets
    pub fn position_to_lsp_position(&self, byte_pos: usize) -> (usize, usize) {
        let (line, column_bytes) = self
            .offset_to_position(byte_pos)
            .map(|pos| (pos.line, pos.column))
            .unwrap_or_else(|| (byte_pos / 80, 0)); // Estimate if metadata unavailable

        // Get the line content
        if let Some(line_bytes) = self.get_line(line) {
            // Convert byte offset to UTF-16 code units
            let text_before = &line_bytes[..column_bytes.min(line_bytes.len())];
            let text_str = String::from_utf8_lossy(text_before);
            let utf16_offset = text_str.encode_utf16().count();
            (line, utf16_offset)
        } else {
            (line, 0)
        }
    }

    /// Convert LSP position (line, UTF-16 code units) to byte position
    /// LSP uses UTF-16 code units for character offsets, not bytes
    /// Optimized to use single line_range() call instead of two
    pub fn lsp_position_to_byte(&self, line: usize, utf16_offset: usize) -> usize {
        if let Some((line_start, end)) = self.piece_tree.line_range(line, &self.buffers) {
            // Calculate line length and get line content
            let line_len = if let Some(end_offset) = end {
                end_offset.saturating_sub(line_start)
            } else {
                self.total_bytes().saturating_sub(line_start)
            };

            if line_len > 0 {
                // If data is unloaded, return line_start as fallback
                let Some(line_bytes) = self.get_text_range(line_start, line_len) else {
                    return line_start;
                };
                let line_str = String::from_utf8_lossy(&line_bytes);

                // Convert UTF-16 offset to byte offset
                let mut utf16_count = 0;
                let mut byte_offset = 0;

                for ch in line_str.chars() {
                    if utf16_count >= utf16_offset {
                        break;
                    }
                    utf16_count += ch.len_utf16();
                    byte_offset += ch.len_utf8();
                }

                line_start + byte_offset
            } else {
                line_start
            }
        } else {
            // Line doesn't exist, return end of buffer
            self.len()
        }
    }

    // Navigation helpers

    /// Find the previous character boundary (UTF-8 aware)
    pub fn prev_char_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        // Get a few bytes before pos to find the character boundary
        let start = pos.saturating_sub(4);
        let Some(bytes) = self.get_text_range(start, pos - start) else {
            // Data unloaded, return pos as fallback
            return pos;
        };

        // Walk backwards to find a UTF-8 leading byte
        for i in (0..bytes.len()).rev() {
            let byte = bytes[i];
            // Check if this is a UTF-8 leading byte (not a continuation byte)
            if (byte & 0b1100_0000) != 0b1000_0000 {
                return start + i;
            }
        }

        // Fallback
        pos.saturating_sub(1)
    }

    /// Find the next character boundary (UTF-8 aware)
    pub fn next_char_boundary(&self, pos: usize) -> usize {
        let len = self.len();
        if pos >= len {
            return len;
        }

        // Get a few bytes after pos to find the character boundary
        let end = (pos + 5).min(len);
        let Some(bytes) = self.get_text_range(pos, end - pos) else {
            // Data unloaded, return pos as fallback
            return pos;
        };

        // Start from index 1 (we want the NEXT boundary)
        for (i, &byte) in bytes.iter().enumerate().skip(1) {
            // Check if this is a UTF-8 leading byte (not a continuation byte)
            if (byte & 0b1100_0000) != 0b1000_0000 {
                return pos + i;
            }
        }

        // If we got here, we're at the end or found no boundary in the range
        end
    }

    /// Check if a byte is a UTF-8 continuation byte (not at a char boundary)
    /// UTF-8 continuation bytes have the pattern 10xxxxxx (0x80-0xBF)
    /// This is the same check that str::is_char_boundary uses internally.
    #[inline]
    fn is_utf8_continuation_byte(byte: u8) -> bool {
        (byte & 0b1100_0000) == 0b1000_0000
    }

    /// Snap position to a valid UTF-8 character boundary
    /// If already at a boundary, returns the same position.
    /// Otherwise, moves to the previous valid boundary.
    pub fn snap_to_char_boundary(&self, pos: usize) -> usize {
        let len = self.len();
        if pos == 0 || pos >= len {
            return pos.min(len);
        }

        // Get the byte at pos to check if we're at a character boundary
        let Some(bytes) = self.get_text_range(pos, 1) else {
            // Data unloaded, return pos as fallback
            return pos;
        };

        // A position is at a char boundary if the byte there is NOT a continuation byte
        if !Self::is_utf8_continuation_byte(bytes[0]) {
            // Already at a character boundary
            return pos;
        }

        // Not at a boundary, find the previous one
        self.prev_char_boundary(pos)
    }

    /// Find the previous grapheme cluster boundary (for proper cursor movement with combining characters)
    ///
    /// This handles complex scripts like Thai where multiple Unicode code points
    /// form a single visual character (grapheme cluster). For example, Thai "ที่"
    /// is 3 code points but 1 grapheme cluster.
    pub fn prev_grapheme_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        // Get enough context before pos to find grapheme boundaries
        // Thai combining characters can have multiple marks, so get up to 32 bytes
        // IMPORTANT: Align start to a valid character boundary to avoid invalid UTF-8
        // when get_text_range starts mid-character
        let raw_start = pos.saturating_sub(32);
        let start = if raw_start == 0 {
            0
        } else {
            // Find the character boundary at or before raw_start
            self.prev_char_boundary(raw_start + 1)
        };

        let Some(bytes) = self.get_text_range(start, pos - start) else {
            // Data unloaded, fall back to char boundary
            return self.prev_char_boundary(pos);
        };

        let text = match std::str::from_utf8(&bytes) {
            Ok(s) => s,
            Err(e) => {
                // Still got invalid UTF-8 (shouldn't happen after alignment)
                // Try using just the valid portion
                let valid_bytes = &bytes[..e.valid_up_to()];
                match std::str::from_utf8(valid_bytes) {
                    Ok(s) if !s.is_empty() => s,
                    _ => return self.prev_char_boundary(pos),
                }
            }
        };

        // Use shared grapheme utility with relative position
        let rel_pos = pos - start;
        let new_rel_pos = grapheme::prev_grapheme_boundary(text, rel_pos);

        // If we landed at the start of this chunk and there's more before,
        // we might need to look further back
        if new_rel_pos == 0 && start > 0 {
            return self.prev_grapheme_boundary(start);
        }

        start + new_rel_pos
    }

    /// Find the next grapheme cluster boundary (for proper cursor movement with combining characters)
    ///
    /// This handles complex scripts like Thai where multiple Unicode code points
    /// form a single visual character (grapheme cluster). For example, Thai "ที่"
    /// is 3 code points but 1 grapheme cluster.
    pub fn next_grapheme_boundary(&self, pos: usize) -> usize {
        let len = self.len();
        if pos >= len {
            return len;
        }

        // Get enough context after pos to find grapheme boundaries
        // Thai combining characters can have multiple marks, so get up to 32 bytes
        let end = (pos + 32).min(len);
        let Some(bytes) = self.get_text_range(pos, end - pos) else {
            // Data unloaded, fall back to char boundary
            return self.next_char_boundary(pos);
        };

        // Convert to UTF-8 string, handling the case where we might have
        // grabbed bytes that end mid-character (truncate to valid UTF-8)
        let text = match std::str::from_utf8(&bytes) {
            Ok(s) => s,
            Err(e) => {
                // The bytes end in an incomplete UTF-8 sequence
                // Use only the valid portion (which includes at least the first grapheme)
                let valid_bytes = &bytes[..e.valid_up_to()];
                match std::str::from_utf8(valid_bytes) {
                    Ok(s) if !s.is_empty() => s,
                    _ => return self.next_char_boundary(pos),
                }
            }
        };

        // Use shared grapheme utility
        let new_rel_pos = grapheme::next_grapheme_boundary(text, 0);
        pos + new_rel_pos
    }

    /// Find the previous word boundary
    pub fn prev_word_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        // Get some text before pos
        let start = pos.saturating_sub(256).max(0);
        let Some(bytes) = self.get_text_range(start, pos - start) else {
            // Data unloaded, return pos as fallback
            return pos;
        };
        let text = String::from_utf8_lossy(&bytes);

        let mut found_word_char = false;
        let chars: Vec<char> = text.chars().collect();

        for i in (0..chars.len()).rev() {
            let ch = chars[i];
            let is_word_char = ch.is_alphanumeric() || ch == '_';

            if found_word_char && !is_word_char {
                // We've transitioned from word to non-word
                // Calculate the byte position
                let byte_offset: usize = chars[0..=i].iter().map(|c| c.len_utf8()).sum();
                return start + byte_offset;
            }

            if is_word_char {
                found_word_char = true;
            }
        }

        0
    }

    /// Find the next word boundary
    pub fn next_word_boundary(&self, pos: usize) -> usize {
        let len = self.len();
        if pos >= len {
            return len;
        }

        // Get some text after pos
        let end = (pos + 256).min(len);
        let Some(bytes) = self.get_text_range(pos, end - pos) else {
            // Data unloaded, return pos as fallback
            return pos;
        };
        let text = String::from_utf8_lossy(&bytes);

        let mut found_word_char = false;
        let mut byte_offset = 0;

        for ch in text.chars() {
            let is_word_char = ch.is_alphanumeric() || ch == '_';

            if found_word_char && !is_word_char {
                // We've transitioned from word to non-word
                return pos + byte_offset;
            }

            if is_word_char {
                found_word_char = true;
            }

            byte_offset += ch.len_utf8();
        }

        len
    }

    /// Create a line iterator starting at the given byte position
    ///
    /// This iterator lazily loads chunks as needed, never scanning the entire file.
    /// For large files with unloaded buffers, chunks are loaded on-demand (1MB at a time).
    pub fn line_iterator(
        &mut self,
        byte_pos: usize,
        estimated_line_length: usize,
    ) -> LineIterator<'_> {
        LineIterator::new(self, byte_pos, estimated_line_length)
    }

    /// Iterate over lines starting from a given byte offset, with line numbers
    ///
    /// This is a more efficient alternative to using line_iterator() + offset_to_position()
    /// because it calculates line numbers incrementally during iteration by accumulating
    /// line_feed_cnt from pieces (which is already tracked in the piece tree).
    ///
    /// Returns: Iterator yielding (byte_offset, content, line_number: Option<usize>)
    /// - line_number is Some(n) for small files with line metadata
    /// - line_number is None for large files without line metadata
    ///
    /// # Performance
    /// - O(1) per line for line number calculation (vs O(log n) per line with offset_to_position)
    /// - Uses single source of truth: piece tree's existing line_feed_cnt metadata
    pub fn iter_lines_from(
        &mut self,
        byte_pos: usize,
        max_lines: usize,
    ) -> Result<TextBufferLineIterator> {
        TextBufferLineIterator::new(self, byte_pos, max_lines)
    }

    // Legacy API methods for backwards compatibility

    /// Get the line number for a given byte offset
    ///
    /// Returns exact line number if metadata available, otherwise estimates based on bytes.
    ///
    /// # Behavior by File Size:
    /// - **Small files (< 1MB)**: Returns exact line number from piece tree's `line_starts` metadata
    /// - **Large files (≥ 1MB)**: Returns estimated line number using `byte_offset / estimated_line_length`
    ///
    /// Large files don't maintain line metadata for performance reasons. The estimation
    /// uses the configured `estimated_line_length` (default 80 bytes).
    pub fn get_line_number(&self, byte_offset: usize) -> usize {
        self.offset_to_position(byte_offset)
            .map(|pos| pos.line)
            .unwrap_or_else(|| {
                // Estimate line number based on configured average line length
                byte_offset / self.config.estimated_line_length
            })
    }

    /// Get the configured estimated line length for approximate line number calculations.
    pub fn estimated_line_length(&self) -> usize {
        self.config.estimated_line_length
    }

    /// Get the starting line number at a byte offset (used for viewport rendering)
    ///
    /// # Line Cache Architecture (Post-Refactoring):
    ///
    /// The concept of a separate "line cache" is **now obsolete**. After the refactoring,
    /// line tracking is integrated directly into the piece tree via:
    /// ```rust
    /// BufferData::Loaded {
    ///     data: Vec<u8>,
    ///     line_starts: Option<Vec<usize>>  // None = large file mode (no line metadata)
    /// }
    /// ```
    ///
    /// ## Why This Method Still Exists:
    /// The rendering code needs to know what line number to display in the margin at the
    /// top of the viewport. This method returns that line number, handling both small
    /// and large file modes transparently.
    ///
    /// ## Small vs Large File Modes:
    /// - **Small files**: `line_starts = Some(vec)` → returns exact line number from metadata
    /// - **Large files**: `line_starts = None` → returns estimated line number (byte_offset / estimated_line_length)
    ///
    /// ## Legacy Line Cache Methods:
    /// These methods are now no-ops and can be removed in a future cleanup:
    /// - `invalidate_line_cache_from()` - No-op (piece tree updates automatically)
    /// - `handle_line_cache_insertion()` - No-op (piece tree updates automatically)
    /// - `handle_line_cache_deletion()` - No-op (piece tree updates automatically)
    /// - `clear_line_cache()` - No-op (can't clear piece tree metadata)
    ///
    /// ## Bug Fix (2025-11):
    /// Previously this method always returned `0`, causing line numbers in the margin
    /// to always show 1, 2, 3... regardless of scroll position. Now it correctly returns
    /// the actual line number at `start_byte`.
    pub fn populate_line_cache(&mut self, start_byte: usize, _line_count: usize) -> usize {
        // No-op for cache population: LineIndex maintains all line starts automatically
        // But we need to return the actual line number at start_byte for rendering
        self.get_line_number(start_byte)
    }

    /// Get cached byte offset for line (compatibility method)
    pub fn get_cached_byte_offset_for_line(&self, line_number: usize) -> Option<usize> {
        self.line_start_offset(line_number)
    }

    /// Invalidate line cache from offset (no-op in new implementation)
    pub fn invalidate_line_cache_from(&mut self, _byte_offset: usize) {
        // No-op: LineIndex updates automatically
    }

    /// Handle line cache insertion (no-op in new implementation)
    pub fn handle_line_cache_insertion(&mut self, _byte_offset: usize, _bytes_inserted: usize) {
        // No-op: LineIndex updates automatically during insert
    }

    /// Handle line cache deletion (no-op in new implementation)
    pub fn handle_line_cache_deletion(&mut self, _byte_offset: usize, _bytes_deleted: usize) {
        // No-op: LineIndex updates automatically during delete
    }

    /// Clear line cache (no-op in new implementation)
    pub fn clear_line_cache(&mut self) {
        // No-op: LineIndex can't be cleared
    }

    // Test helper methods

    /// Create a buffer from a string for testing
    #[cfg(test)]
    pub fn from_str_test(s: &str) -> Self {
        Self::from_bytes(
            s.as_bytes().to_vec(),
            std::sync::Arc::new(crate::model::filesystem::StdFileSystem),
        )
    }

    /// Create a new empty buffer for testing
    #[cfg(test)]
    pub fn new_test() -> Self {
        Self::empty(std::sync::Arc::new(crate::model::filesystem::StdFileSystem))
    }
}

/// Type alias for backwards compatibility
pub type Buffer = TextBuffer;

// Re-export LineIterator from the line_iterator module
pub use crate::primitives::line_iterator::LineIterator;

// ============================================================================
// Overlapping Chunks Iterator for Efficient Search
// ============================================================================

/// Information about a chunk of data for pattern matching
#[derive(Debug)]
pub struct ChunkInfo {
    /// The buffer containing this chunk's data (includes overlap from previous chunk)
    pub buffer: Vec<u8>,

    /// Absolute position in the document where this buffer starts
    pub absolute_pos: usize,

    /// Offset within buffer where "new" data starts (valid match zone)
    /// Matches starting before this offset were already checked in the previous chunk
    pub valid_start: usize,
}

/// Iterator that yields overlapping chunks for pattern matching
///
/// This iterator implements the VSCode/Sublime approach: pull overlapping chunks
/// from the underlying piece tree and use standard search algorithms on them.
///
/// # Algorithm
///
/// ```text
/// Chunk 1: [------------ valid -----------]
/// Chunk 2:      [overlap][---- valid ----]
/// Chunk 3:                   [overlap][-- valid --]
///
/// Only matches starting in the "valid" zone are reported to avoid duplicates.
/// ```
///
/// # Example
///
/// ```ignore
/// let chunks = OverlappingChunks::new(&text_buffer, start, end, 4096, pattern.len()-1);
/// for chunk in chunks {
///     // Search only starting from chunk.valid_start
///     if let Some(pos) = search(&chunk.buffer[chunk.valid_start..]) {
///         let absolute_pos = chunk.absolute_pos + chunk.valid_start + pos;
///         return Some(absolute_pos);
///     }
/// }
/// ```
pub struct OverlappingChunks<'a> {
    piece_iter: PieceRangeIter,
    buffers: &'a [StringBuffer],

    // Reusable chunk buffer that we fill from pieces
    buffer: Vec<u8>,
    buffer_absolute_pos: usize,

    // Current state
    current_pos: usize,
    end_pos: usize,

    // Configuration
    chunk_size: usize,
    overlap: usize,

    // Track first chunk special case
    first_chunk: bool,

    // Cached piece data for incremental reading
    current_piece_data: Option<Vec<u8>>,
    current_piece_offset: usize,
}

impl<'a> OverlappingChunks<'a> {
    /// Create a new overlapping chunks iterator
    ///
    /// # Arguments
    ///
    /// * `text_buffer` - The text buffer to iterate over
    /// * `start` - Start position in the document
    /// * `end` - End position in the document (exclusive)
    /// * `chunk_size` - Target size for each chunk (excluding overlap)
    /// * `overlap` - Number of bytes to overlap between chunks
    ///
    /// # Recommendations
    ///
    /// * For literal string search: `chunk_size=65536, overlap=pattern.len()-1`
    /// * For regex search: `chunk_size=1048576, overlap=4096`
    pub fn new(
        text_buffer: &'a TextBuffer,
        start: usize,
        end: usize,
        chunk_size: usize,
        overlap: usize,
    ) -> Self {
        let piece_iter = text_buffer.piece_tree.iter_pieces_in_range(start, end);

        Self {
            piece_iter,
            buffers: &text_buffer.buffers,
            buffer: Vec::with_capacity(chunk_size + overlap),
            buffer_absolute_pos: start,
            current_pos: start,
            end_pos: end,
            chunk_size,
            overlap,
            first_chunk: true,
            current_piece_data: None,
            current_piece_offset: 0,
        }
    }

    /// Read one byte from the piece iterator
    fn read_byte(&mut self) -> Option<u8> {
        loop {
            // If we have cached piece data, read from it
            if let Some(ref data) = self.current_piece_data {
                if self.current_piece_offset < data.len() {
                    let byte = data[self.current_piece_offset];
                    self.current_piece_offset += 1;
                    self.current_pos += 1;
                    return Some(byte);
                } else {
                    // Exhausted current piece, move to next
                    self.current_piece_data = None;
                    self.current_piece_offset = 0;
                }
            }

            // Get next piece
            if let Some(piece_view) = self.piece_iter.next() {
                let buffer_id = piece_view.location.buffer_id();
                if let Some(buffer) = self.buffers.get(buffer_id) {
                    // Extract the relevant slice from this piece
                    let piece_start_in_doc = piece_view.doc_offset;
                    let piece_end_in_doc = piece_view.doc_offset + piece_view.bytes;

                    // Clip to our search range
                    let read_start = self.current_pos.max(piece_start_in_doc);
                    let read_end = self.end_pos.min(piece_end_in_doc);

                    if read_end > read_start {
                        let offset_in_piece = read_start - piece_start_in_doc;
                        let bytes_to_read = read_end - read_start;

                        let buffer_start = piece_view.buffer_offset + offset_in_piece;
                        let buffer_end = buffer_start + bytes_to_read;

                        if let Some(data) = buffer.get_data() {
                            if buffer_end <= data.len() {
                                // Cache this piece's data
                                self.current_piece_data =
                                    Some(data[buffer_start..buffer_end].to_vec());
                                self.current_piece_offset = 0;
                                continue;
                            }
                        }
                    }
                }
            }

            // No more data
            return None;
        }
    }

    /// Fill the buffer with the next chunk of data
    fn fill_next_chunk(&mut self) -> bool {
        if self.first_chunk {
            // First chunk: fill up to chunk_size
            self.first_chunk = false;
            while self.buffer.len() < self.chunk_size && self.current_pos < self.end_pos {
                if let Some(byte) = self.read_byte() {
                    self.buffer.push(byte);
                } else {
                    break;
                }
            }
            !self.buffer.is_empty()
        } else {
            // Subsequent chunks: keep overlap, fill chunk_size NEW bytes
            if self.current_pos >= self.end_pos {
                return false;
            }

            // Keep overlap bytes at the end
            if self.buffer.len() > self.overlap {
                let drain_amount = self.buffer.len() - self.overlap;
                self.buffer.drain(0..drain_amount);
                self.buffer_absolute_pos += drain_amount;
            }

            // Fill chunk_size NEW bytes (in addition to overlap)
            let before_len = self.buffer.len();
            let target_len = self.overlap + self.chunk_size;
            while self.buffer.len() < target_len && self.current_pos < self.end_pos {
                if let Some(byte) = self.read_byte() {
                    self.buffer.push(byte);
                } else {
                    break;
                }
            }

            // Return true if we added new data
            self.buffer.len() > before_len
        }
    }
}

impl<'a> Iterator for OverlappingChunks<'a> {
    type Item = ChunkInfo;

    fn next(&mut self) -> Option<Self::Item> {
        // Track if this is the first chunk before filling
        let is_first = self.buffer_absolute_pos == self.current_pos;

        if !self.fill_next_chunk() {
            return None;
        }

        // First chunk: all data is valid (no overlap from previous)
        // Subsequent chunks: overlap bytes are not valid (already checked)
        let valid_start = if is_first {
            0
        } else {
            self.overlap.min(self.buffer.len())
        };

        Some(ChunkInfo {
            buffer: self.buffer.clone(),
            absolute_pos: self.buffer_absolute_pos,
            valid_start,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::*;

    #[test]
    fn test_empty_buffer() {
        let buffer = TextBuffer::empty(test_fs());
        assert_eq!(buffer.total_bytes(), 0);
        assert_eq!(buffer.line_count(), Some(1)); // Empty doc has 1 line
    }

    #[test]
    fn test_line_positions_multiline() {
        let buffer = TextBuffer::from_bytes(b"Hello\nNew Line\nWorld!".to_vec(), test_fs());

        // Check line count
        assert_eq!(buffer.line_count(), Some(3));

        // Check line starts
        assert_eq!(buffer.line_start_offset(0), Some(0)); // "Hello\n" starts at 0
        assert_eq!(buffer.line_start_offset(1), Some(6)); // "New Line\n" starts at 6
        assert_eq!(buffer.line_start_offset(2), Some(15)); // "World!" starts at 15

        // Check offset_to_position
        assert_eq!(buffer.offset_to_position(0).unwrap().line, 0); // Start of "Hello"
        assert_eq!(buffer.offset_to_position(5).unwrap().line, 0); // End of "Hello" (before \n)
        assert_eq!(buffer.offset_to_position(6).unwrap().line, 1); // Start of "New Line"
        assert_eq!(buffer.offset_to_position(14).unwrap().line, 1); // End of "New Line" (before \n)
        assert_eq!(buffer.offset_to_position(15).unwrap().line, 2); // Start of "World!"

        // Check line_col_to_position
        assert_eq!(buffer.line_col_to_position(0, 5), 5); // End of line 0
        assert_eq!(buffer.line_col_to_position(1, 0), 6); // Start of line 1
        assert_eq!(buffer.line_col_to_position(1, 8), 14); // End of line 1
        assert_eq!(buffer.line_col_to_position(2, 0), 15); // Start of line 2
    }

    #[test]
    fn test_new_from_content() {
        let buffer = TextBuffer::from_bytes(b"hello\nworld".to_vec(), test_fs());
        assert_eq!(buffer.total_bytes(), 11);
        assert_eq!(buffer.line_count(), Some(2));
    }

    #[test]
    fn test_get_all_text() {
        let buffer = TextBuffer::from_bytes(b"hello\nworld".to_vec(), test_fs());
        assert_eq!(buffer.get_all_text().unwrap(), b"hello\nworld");
    }

    #[test]
    fn test_insert_at_start() {
        let mut buffer = TextBuffer::from_bytes(b"world".to_vec(), test_fs());
        buffer.insert_bytes(0, b"hello ".to_vec());

        assert_eq!(buffer.get_all_text().unwrap(), b"hello world");
        assert_eq!(buffer.total_bytes(), 11);
    }

    #[test]
    fn test_insert_in_middle() {
        let mut buffer = TextBuffer::from_bytes(b"helloworld".to_vec(), test_fs());
        buffer.insert_bytes(5, b" ".to_vec());

        assert_eq!(buffer.get_all_text().unwrap(), b"hello world");
        assert_eq!(buffer.total_bytes(), 11);
    }

    #[test]
    fn test_insert_at_end() {
        let mut buffer = TextBuffer::from_bytes(b"hello".to_vec(), test_fs());
        buffer.insert_bytes(5, b" world".to_vec());

        assert_eq!(buffer.get_all_text().unwrap(), b"hello world");
        assert_eq!(buffer.total_bytes(), 11);
    }

    #[test]
    fn test_insert_with_newlines() {
        let mut buffer = TextBuffer::from_bytes(b"hello".to_vec(), test_fs());
        buffer.insert_bytes(5, b"\nworld\ntest".to_vec());

        assert_eq!(buffer.get_all_text().unwrap(), b"hello\nworld\ntest");
        assert_eq!(buffer.line_count(), Some(3));
    }

    #[test]
    fn test_delete_from_start() {
        let mut buffer = TextBuffer::from_bytes(b"hello world".to_vec(), test_fs());
        buffer.delete_bytes(0, 6);

        assert_eq!(buffer.get_all_text().unwrap(), b"world");
        assert_eq!(buffer.total_bytes(), 5);
    }

    #[test]
    fn test_delete_from_middle() {
        let mut buffer = TextBuffer::from_bytes(b"hello world".to_vec(), test_fs());
        buffer.delete_bytes(5, 1);

        assert_eq!(buffer.get_all_text().unwrap(), b"helloworld");
        assert_eq!(buffer.total_bytes(), 10);
    }

    #[test]
    fn test_delete_from_end() {
        let mut buffer = TextBuffer::from_bytes(b"hello world".to_vec(), test_fs());
        buffer.delete_bytes(6, 5);

        assert_eq!(buffer.get_all_text().unwrap(), b"hello ");
        assert_eq!(buffer.total_bytes(), 6);
    }

    #[test]
    fn test_delete_with_newlines() {
        let mut buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());
        buffer.delete_bytes(5, 7); // Delete "\nworld\n"

        assert_eq!(buffer.get_all_text().unwrap(), b"hellotest");
        assert_eq!(buffer.line_count(), Some(1));
    }

    #[test]
    fn test_offset_position_conversions() {
        let buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());

        let pos = buffer.offset_to_position(0);
        assert_eq!(pos, Some(Position { line: 0, column: 0 }));

        let pos = buffer.offset_to_position(6);
        assert_eq!(pos, Some(Position { line: 1, column: 0 }));

        let offset = buffer.position_to_offset(Position { line: 1, column: 0 });
        assert_eq!(offset, 6);
    }

    #[test]
    fn test_insert_at_position() {
        let mut buffer = TextBuffer::from_bytes(b"hello\nworld".to_vec(), test_fs());
        buffer.insert_at_position(Position { line: 1, column: 0 }, b"beautiful ".to_vec());

        assert_eq!(buffer.get_all_text().unwrap(), b"hello\nbeautiful world");
    }

    #[test]
    fn test_delete_range() {
        let mut buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());

        let start = Position { line: 0, column: 5 };
        let end = Position { line: 2, column: 0 };
        buffer.delete_range(start, end);

        assert_eq!(buffer.get_all_text().unwrap(), b"hellotest");
    }

    #[test]
    fn test_get_line() {
        let buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());

        assert_eq!(buffer.get_line(0), Some(b"hello\n".to_vec()));
        assert_eq!(buffer.get_line(1), Some(b"world\n".to_vec()));
        assert_eq!(buffer.get_line(2), Some(b"test".to_vec()));
        assert_eq!(buffer.get_line(3), None);
    }

    #[test]
    fn test_multiple_operations() {
        let mut buffer = TextBuffer::from_bytes(b"line1\nline2\nline3".to_vec(), test_fs());

        buffer.insert_bytes(0, b"start\n".to_vec());
        assert_eq!(buffer.line_count(), Some(4));

        buffer.delete_bytes(6, 6); // Delete "line1\n"
        assert_eq!(buffer.line_count(), Some(3));

        buffer.insert_bytes(6, b"new\n".to_vec());
        assert_eq!(buffer.line_count(), Some(4));

        let text = buffer.get_all_text().unwrap();
        assert_eq!(text, b"start\nnew\nline2\nline3");
    }

    #[test]
    fn test_get_text_range() {
        let buffer = TextBuffer::from_bytes(b"hello world".to_vec(), test_fs());

        assert_eq!(buffer.get_text_range(0, 5), Some(b"hello".to_vec()));
        assert_eq!(buffer.get_text_range(6, 5), Some(b"world".to_vec()));
        assert_eq!(buffer.get_text_range(0, 11), Some(b"hello world".to_vec()));
    }

    #[test]
    fn test_empty_operations() {
        let mut buffer = TextBuffer::from_bytes(b"hello".to_vec(), test_fs());

        buffer.insert_bytes(2, Vec::new());
        assert_eq!(buffer.get_all_text().unwrap(), b"hello");

        buffer.delete_bytes(2, 0);
        assert_eq!(buffer.get_all_text().unwrap(), b"hello");
    }

    #[test]
    fn test_sequential_inserts_at_beginning() {
        // Regression test for piece tree duplicate insertion bug
        let mut buffer = TextBuffer::from_bytes(b"initial\ntext".to_vec(), test_fs());

        // Delete all
        buffer.delete_bytes(0, 12);
        assert_eq!(buffer.get_all_text().unwrap(), b"");

        // Insert 'a' at 0
        buffer.insert_bytes(0, vec![b'a']);
        assert_eq!(buffer.get_all_text().unwrap(), b"a");

        // Insert 'b' at 0 (should give "ba")
        buffer.insert_bytes(0, vec![b'b']);
        assert_eq!(buffer.get_all_text().unwrap(), b"ba");
    }

    // ===== Phase 1-3: Large File Support Tests =====

    mod large_file_support {
        use super::*;
        use crate::model::piece_tree::StringBuffer;
        use std::fs::File;
        use std::io::Write;
        use tempfile::TempDir;

        // Phase 1: Option<usize> Type Safety Tests

        #[test]
        fn test_line_feed_count_is_some_for_loaded_buffer() {
            let buffer = StringBuffer::new(0, b"hello\nworld\ntest".to_vec());
            assert_eq!(buffer.line_feed_count(), Some(2));
        }

        #[test]
        fn test_line_feed_count_is_none_for_unloaded_buffer() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");

            let buffer = StringBuffer::new_unloaded(0, file_path, 0, 100);
            assert_eq!(buffer.line_feed_count(), None);
        }

        #[test]
        fn test_line_count_is_some_for_small_buffer() {
            let buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec(), test_fs());
            assert_eq!(buffer.line_count(), Some(3));
        }

        #[test]
        fn test_piece_tree_works_with_none_line_count() {
            // Create a buffer with no line count information
            let buffer = StringBuffer::new_loaded(0, b"hello\nworld".to_vec(), false);
            assert_eq!(buffer.line_feed_count(), None);

            // Create piece tree without line feed count
            use crate::model::piece_tree::{BufferLocation, PieceTree};
            let tree = PieceTree::new(BufferLocation::Stored(0), 0, 11, None);

            // line_count should return None
            assert_eq!(tree.line_count(), None);
        }

        // Phase 2: BufferData Enum Tests

        #[test]
        fn test_buffer_data_loaded_variant() {
            let data = b"hello world".to_vec();
            let buffer = StringBuffer::new_loaded(0, data.clone(), true);

            assert!(buffer.is_loaded());
            assert_eq!(buffer.get_data(), Some(&data[..]));
            assert!(buffer.get_line_starts().is_some());
        }

        #[test]
        fn test_buffer_data_loaded_without_line_starts() {
            let data = b"hello\nworld".to_vec();
            let buffer = StringBuffer::new_loaded(0, data.clone(), false);

            assert!(buffer.is_loaded());
            assert_eq!(buffer.get_data(), Some(&data[..]));
            assert_eq!(buffer.get_line_starts(), None); // No line indexing
        }

        #[test]
        fn test_buffer_data_unloaded_variant() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");

            let buffer = StringBuffer::new_unloaded(0, file_path.clone(), 0, 100);

            assert!(!buffer.is_loaded());
            assert_eq!(buffer.get_data(), None);
            assert_eq!(buffer.get_line_starts(), None);
        }

        #[test]
        fn test_buffer_load_method() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");

            // Create test file
            let test_data = b"hello world";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Create unloaded buffer
            let mut buffer = StringBuffer::new_unloaded(0, file_path, 0, test_data.len());
            assert!(!buffer.is_loaded());

            // Load the buffer using local filesystem
            let fs = crate::model::filesystem::StdFileSystem;
            buffer.load(&fs).unwrap();

            // Now it should be loaded
            assert!(buffer.is_loaded());
            assert_eq!(buffer.get_data(), Some(&test_data[..]));
        }

        #[test]
        fn test_string_buffer_new_vs_new_loaded() {
            let data = b"hello\nworld".to_vec();

            // StringBuffer::new should compute line starts
            let buf1 = StringBuffer::new(0, data.clone());
            assert!(buf1.is_loaded());
            assert!(buf1.get_line_starts().is_some());
            assert_eq!(buf1.line_feed_count(), Some(1));

            // StringBuffer::new_loaded with compute_lines=false should not
            let buf2 = StringBuffer::new_loaded(0, data.clone(), false);
            assert!(buf2.is_loaded());
            assert_eq!(buf2.get_line_starts(), None);
            assert_eq!(buf2.line_feed_count(), None);
        }

        // Phase 3: Large File Detection Tests

        #[test]
        fn test_load_small_file_eager_loading() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("small.txt");

            // Create a small file (10 bytes < 100MB threshold)
            let test_data = b"hello\ntest";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Load with default threshold
            let buffer = TextBuffer::load_from_file(&file_path, 0, test_fs()).unwrap();

            // Should be eagerly loaded (not large_file mode)
            assert!(!buffer.large_file);
            assert_eq!(buffer.total_bytes(), test_data.len());
            assert_eq!(buffer.line_count(), Some(2)); // Has line indexing
            assert_eq!(buffer.get_all_text().unwrap(), test_data);

            // The buffer should be loaded
            assert!(buffer.buffers[0].is_loaded());
        }

        #[test]
        fn test_load_large_file_lazy_loading() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("large.txt");

            // Create a "large" file by using a small threshold
            let test_data = b"hello\nworld\ntest";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Load with threshold of 10 bytes (file is 17 bytes, so it's "large")
            let buffer = TextBuffer::load_from_file(&file_path, 10, test_fs()).unwrap();

            // Should be in large_file mode
            assert!(buffer.large_file);
            assert_eq!(buffer.total_bytes(), test_data.len());

            // Should NOT have line indexing
            assert_eq!(buffer.line_count(), None);

            // The buffer should be unloaded
            assert!(!buffer.buffers[0].is_loaded());
            assert_eq!(buffer.buffers[0].get_data(), None);
        }

        /// Test that reproduces issue #657: Search on large plain text files
        ///
        /// The bug: When a large file is opened with lazy loading, buffer.to_string()
        /// returns None because some buffers are unloaded. This causes search to fail
        /// with "Buffer not fully loaded" error.
        ///
        /// The fix: Use get_text_range_mut() which loads the buffer on demand.
        #[test]
        fn test_issue_657_search_on_large_file_unloaded_buffer() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("large_search_test.txt");

            // Create test content with a searchable string
            let test_data = b"line1\nline2\nSEARCH_TARGET\nline4\nline5";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Load with small threshold to force lazy loading
            let mut buffer = TextBuffer::load_from_file(&file_path, 10, test_fs()).unwrap();

            // Verify we're in large file mode with unloaded buffer
            assert!(buffer.large_file, "Buffer should be in large file mode");
            assert!(
                !buffer.buffers[0].is_loaded(),
                "Buffer should be unloaded initially"
            );

            // REPRODUCE THE BUG: to_string() returns None for unloaded buffers
            // This is what the old perform_search() code did, causing the error
            assert!(
                buffer.to_string().is_none(),
                "BUG REPRODUCED: to_string() returns None for unloaded buffer"
            );

            // THE FIX: get_text_range_mut() loads the buffer on demand
            let total_bytes = buffer.len();
            let content = buffer.get_text_range_mut(0, total_bytes).unwrap();
            let content_str = String::from_utf8_lossy(&content);

            // Verify the content is now available and contains our search target
            assert!(
                content_str.contains("SEARCH_TARGET"),
                "FIX WORKS: get_text_range_mut() loaded the buffer and found the search target"
            );

            // After loading, to_string() should also work
            assert!(
                buffer.to_string().is_some(),
                "After get_text_range_mut(), to_string() should work"
            );
        }

        #[test]
        fn test_large_file_threshold_boundary() {
            let temp_dir = TempDir::new().unwrap();

            // Test exactly at threshold
            let file_path = temp_dir.path().join("at_threshold.txt");
            let test_data = vec![b'x'; 100];
            File::create(&file_path)
                .unwrap()
                .write_all(&test_data)
                .unwrap();

            // Load with threshold of 100 bytes - should be large file (>= threshold)
            let buffer = TextBuffer::load_from_file(&file_path, 100, test_fs()).unwrap();
            assert!(buffer.large_file);

            // Test just below threshold
            let file_path2 = temp_dir.path().join("below_threshold.txt");
            let test_data2 = vec![b'x'; 99];
            File::create(&file_path2)
                .unwrap()
                .write_all(&test_data2)
                .unwrap();

            // Load with threshold of 100 bytes - should be small file (< threshold)
            let buffer2 = TextBuffer::load_from_file(&file_path2, 100, test_fs()).unwrap();
            assert!(!buffer2.large_file);
        }

        #[test]
        fn test_large_file_default_threshold() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");

            // Create a small file
            File::create(&file_path)
                .unwrap()
                .write_all(b"hello")
                .unwrap();

            // Load with threshold 0 - should use DEFAULT_LARGE_FILE_THRESHOLD
            let buffer = TextBuffer::load_from_file(&file_path, 0, test_fs()).unwrap();

            // 5 bytes < 100MB, so should not be large file
            assert!(!buffer.large_file);
        }

        #[test]
        fn test_large_file_has_correct_piece_tree_structure() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("large.txt");

            let test_data = b"hello world";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Load as large file
            let buffer = TextBuffer::load_from_file(&file_path, 5, test_fs()).unwrap();

            // Should have correct total bytes
            assert_eq!(buffer.total_bytes(), test_data.len());

            // Should have 1 buffer
            assert_eq!(buffer.buffers.len(), 1);

            // Buffer should be unloaded
            assert!(!buffer.buffers[0].is_loaded());
        }

        #[test]
        fn test_empty_large_file() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("empty.txt");

            // Create an empty file
            File::create(&file_path).unwrap();

            // Load as large file
            let buffer = TextBuffer::load_from_file(&file_path, 0, test_fs()).unwrap();

            // Empty file is handled gracefully
            assert_eq!(buffer.total_bytes(), 0);
            assert!(buffer.is_empty());
        }

        #[test]
        fn test_large_file_basic_api_operations() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("large_test.txt");

            // Create a test file with known content
            let test_data = b"line1\nline2\nline3\nline4\n";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Load as large file (use small threshold to trigger large file mode)
            let mut buffer = TextBuffer::load_from_file(&file_path, 10, test_fs()).unwrap();

            // Verify it's in large file mode
            assert!(buffer.large_file);
            assert_eq!(buffer.line_count(), None); // No line indexing

            // Test basic access functions
            assert_eq!(buffer.total_bytes(), test_data.len());
            assert!(!buffer.is_empty());
            assert_eq!(buffer.len(), test_data.len());

            // Test reading operations using get_text_range_mut (lazy loads on demand)
            let range_result = buffer.get_text_range_mut(0, 5).unwrap();
            assert_eq!(range_result, b"line1");

            let range_result2 = buffer.get_text_range_mut(6, 5).unwrap();
            assert_eq!(range_result2, b"line2");

            // Test get_all_text (via get_text_range after lazy loading)
            let all_text = buffer.get_all_text().unwrap();
            assert_eq!(all_text, test_data);

            // Test slice_bytes method
            assert_eq!(buffer.slice_bytes(0..5), b"line1");

            // Test basic editing operations
            // Insert at offset 0
            buffer.insert_bytes(0, b"prefix_".to_vec());
            assert_eq!(buffer.total_bytes(), test_data.len() + 7);
            assert!(buffer.is_modified());

            // Verify the insertion worked
            let text_after_insert = buffer.get_all_text().unwrap();
            assert_eq!(&text_after_insert[0..7], b"prefix_");
            assert_eq!(&text_after_insert[7..12], b"line1");

            // Delete some bytes
            buffer.delete_bytes(0, 7);
            assert_eq!(buffer.total_bytes(), test_data.len());

            // Verify deletion worked - should be back to original
            let text_after_delete = buffer.get_all_text().unwrap();
            assert_eq!(text_after_delete, test_data);

            // Insert at end
            let end_offset = buffer.total_bytes();
            buffer.insert_bytes(end_offset, b"suffix".to_vec());
            assert_eq!(buffer.total_bytes(), test_data.len() + 6);

            // Verify end insertion
            let final_text = buffer.get_all_text().unwrap();
            assert!(final_text.ends_with(b"suffix"));
            assert_eq!(&final_text[0..test_data.len()], test_data);

            // Test offset_to_position
            // Note: Without line indexing, position tracking is limited
            // but byte-level operations still work
            let pos = buffer.offset_to_position(0).unwrap();
            assert_eq!(pos.column, 0);

            // Test position_to_offset
            let offset = buffer.position_to_offset(Position { line: 0, column: 0 });
            assert_eq!(offset, 0);

            // Test replace operations
            let replace_result = buffer.replace_range(0..5, "START");
            assert!(replace_result);

            let text_after_replace = buffer.get_all_text().unwrap();
            assert!(text_after_replace.starts_with(b"START"));
        }

        #[test]
        fn test_large_file_chunk_based_loading() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("huge.txt");

            // Create a file larger than LOAD_CHUNK_SIZE (1MB)
            // We'll create a 3MB file with a repeating pattern so we can verify chunks
            let chunk_size = LOAD_CHUNK_SIZE; // 1MB
            let file_size = chunk_size * 3; // 3MB

            // Pattern: "AAAA...AAAA" (1MB of A's), "BBBB...BBBB" (1MB of B's), "CCCC...CCCC" (1MB of C's)
            let mut file = File::create(&file_path).unwrap();
            file.write_all(&vec![b'A'; chunk_size]).unwrap();
            file.write_all(&vec![b'B'; chunk_size]).unwrap();
            file.write_all(&vec![b'C'; chunk_size]).unwrap();
            file.flush().unwrap();

            // Load as large file (use threshold of 1 byte to ensure large file mode)
            let mut buffer = TextBuffer::load_from_file(&file_path, 1, test_fs()).unwrap();

            // Verify it's in large file mode
            assert!(buffer.large_file);
            assert_eq!(buffer.total_bytes(), file_size);

            // Buffer should be unloaded initially
            assert!(!buffer.buffers[0].is_loaded());

            // Read from the first chunk (should load only first 1MB)
            let first_chunk_data = buffer.get_text_range_mut(0, 1024).unwrap();
            assert_eq!(first_chunk_data.len(), 1024);
            assert!(first_chunk_data.iter().all(|&b| b == b'A'));

            // Read from the middle chunk (offset = 1MB, should load second 1MB)
            let second_chunk_data = buffer.get_text_range_mut(chunk_size, 1024).unwrap();
            assert_eq!(second_chunk_data.len(), 1024);
            assert!(second_chunk_data.iter().all(|&b| b == b'B'));

            // Read from the last chunk (offset = 2MB, should load third 1MB)
            let third_chunk_data = buffer.get_text_range_mut(chunk_size * 2, 1024).unwrap();
            assert_eq!(third_chunk_data.len(), 1024);
            assert!(third_chunk_data.iter().all(|&b| b == b'C'));

            // Verify we can read across chunk boundaries
            // Read from middle of first chunk to middle of second chunk
            let cross_chunk_offset = chunk_size - 512;
            let cross_chunk_data = buffer.get_text_range_mut(cross_chunk_offset, 1024).unwrap();
            assert_eq!(cross_chunk_data.len(), 1024);
            // First 512 bytes should be 'A', next 512 bytes should be 'B'
            assert!(cross_chunk_data[..512].iter().all(|&b| b == b'A'));
            assert!(cross_chunk_data[512..].iter().all(|&b| b == b'B'));

            // After chunk-based loading, verify the piece tree has been split
            // The number of buffers should be greater than 1 (original + chunks)
            assert!(
                buffer.buffers.len() > 1,
                "Expected multiple buffers after chunk-based loading, got {}",
                buffer.buffers.len()
            );

            // Test that editing still works after chunk-based loading
            buffer.insert_bytes(0, b"PREFIX".to_vec());
            assert_eq!(buffer.total_bytes(), file_size + 6);

            let after_insert = buffer.get_text_range_mut(0, 6).unwrap();
            assert_eq!(after_insert, b"PREFIX");

            // Verify the original data is still there after the prefix
            let after_prefix = buffer.get_text_range_mut(6, 10).unwrap();
            assert!(after_prefix.iter().all(|&b| b == b'A'));

            // Most importantly: validate the entire buffer content matches the original file
            // Create a fresh buffer to read the original file
            let mut buffer2 = TextBuffer::load_from_file(&file_path, 1, test_fs()).unwrap();

            // Read the entire file in chunks and verify each chunk
            let chunk_read_size = 64 * 1024; // Read in 64KB chunks for efficiency
            let mut offset = 0;
            while offset < file_size {
                let bytes_to_read = chunk_read_size.min(file_size - offset);
                let chunk_data = buffer2.get_text_range_mut(offset, bytes_to_read).unwrap();

                // Determine which section of the file we're reading
                let first_mb_end = chunk_size;
                let second_mb_end = chunk_size * 2;

                // Validate the data based on which MB section we're in
                for (i, &byte) in chunk_data.iter().enumerate() {
                    let file_offset = offset + i;
                    let expected = if file_offset < first_mb_end {
                        b'A'
                    } else if file_offset < second_mb_end {
                        b'B'
                    } else {
                        b'C'
                    };
                    assert_eq!(
                        byte, expected,
                        "Mismatch at file offset {}: expected {}, got {}",
                        file_offset, expected as char, byte as char
                    );
                }

                offset += bytes_to_read;
            }
        }

        /// Test that save_to_file works correctly with partially loaded large files
        /// This is a regression test for a bug where saving would silently produce
        /// an empty file if any buffer regions were still unloaded.
        #[test]
        fn test_large_file_incremental_save() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("large_save_test.txt");

            // Create a small file but use tiny threshold to trigger large file mode
            let chunk_size = 1000; // 1KB chunks
            let file_size = chunk_size * 2; // 2KB total

            let mut file = File::create(&file_path).unwrap();
            // First half: 'A' repeated
            file.write_all(&vec![b'A'; chunk_size]).unwrap();
            // Second half: 'B' repeated
            file.write_all(&vec![b'B'; chunk_size]).unwrap();
            file.flush().unwrap();

            // Load as large file (threshold of 100 bytes)
            let mut buffer = TextBuffer::load_from_file(&file_path, 100, test_fs()).unwrap();
            assert!(buffer.large_file);
            assert_eq!(buffer.total_bytes(), file_size);

            // Only read from the beginning - this loads only a small region
            let first_bytes = buffer.get_text_range_mut(0, 50).unwrap();
            assert!(first_bytes.iter().all(|&b| b == b'A'));

            // Make an edit at the beginning
            buffer.insert_bytes(0, b"PREFIX_".to_vec());

            // Save to a new file (to avoid issues with reading while writing same file)
            let save_path = temp_dir.path().join("saved.txt");
            buffer.save_to_file(&save_path).unwrap();

            // Verify the saved file
            let saved_content = std::fs::read(&save_path).unwrap();

            // Check total size: original + "PREFIX_" (7 bytes)
            assert_eq!(
                saved_content.len(),
                file_size + 7,
                "Saved file should be {} bytes, got {}",
                file_size + 7,
                saved_content.len()
            );

            // Check prefix
            assert_eq!(&saved_content[..7], b"PREFIX_", "Should start with PREFIX_");

            // Check that first chunk (after prefix) contains A's
            assert!(
                saved_content[7..100].iter().all(|&b| b == b'A'),
                "First chunk after prefix should be A's"
            );

            // Check that second chunk contains B's (this was unloaded!)
            let second_chunk_start = 7 + chunk_size;
            assert!(
                saved_content[second_chunk_start..second_chunk_start + 100]
                    .iter()
                    .all(|&b| b == b'B'),
                "Second chunk should be B's (was unloaded, should be preserved)"
            );
        }

        /// Test that save_to_file handles edits at multiple positions
        #[test]
        fn test_large_file_save_with_multiple_edits() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("multi_edit.txt");

            // Create a ~5KB file with numbered lines for easier verification
            let mut content = Vec::new();
            for i in 0..100 {
                content.extend_from_slice(
                    format!("Line {:04}: padding to make it longer\n", i).as_bytes(),
                );
            }
            let original_len = content.len();
            std::fs::write(&file_path, &content).unwrap();

            // Load as large file (threshold of 500 bytes)
            let mut buffer = TextBuffer::load_from_file(&file_path, 500, test_fs()).unwrap();
            assert!(
                buffer.line_count().is_none(),
                "Should be in large file mode"
            );

            // Edit at the beginning
            buffer.insert_bytes(0, b"[START]".to_vec());

            // Edit somewhere in the middle (load that region first)
            let mid_offset = original_len / 2;
            let _mid_bytes = buffer.get_text_range_mut(mid_offset + 7, 10).unwrap(); // +7 for our insert
            buffer.insert_bytes(mid_offset + 7, b"[MIDDLE]".to_vec());

            // Save
            let save_path = temp_dir.path().join("multi_edit_saved.txt");
            buffer.save_to_file(&save_path).unwrap();

            // Verify
            let saved = std::fs::read_to_string(&save_path).unwrap();

            assert!(
                saved.starts_with("[START]Line 0000"),
                "Should start with our edit"
            );
            assert!(saved.contains("[MIDDLE]"), "Should contain middle edit");
            assert!(saved.contains("Line 0099"), "Should preserve end of file");

            // Verify total length
            let expected_len = original_len + 7 + 8; // [START] + [MIDDLE]
            assert_eq!(
                saved.len(),
                expected_len,
                "Length should be original + edits"
            );
        }
    }

    // ===== Offset to Position Tests =====
    // These tests focus on the offset_to_position correctness

    #[test]
    fn test_offset_to_position_simple() {
        // Create a buffer with known line structure
        // Line 0: "a\n" (bytes 0-1, newline at 1)
        // Line 1: "b\n" (bytes 2-3, newline at 3)
        // Line 2: "c\n" (bytes 4-5, newline at 5)
        // Line 3: "d" (bytes 6, no newline)
        let content = b"a\nb\nc\nd";
        let buffer = TextBuffer::from_bytes(content.to_vec(), test_fs());

        // Verify specific positions
        let pos = buffer
            .offset_to_position(0)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 0, "Byte 0 should be on line 0");
        assert_eq!(pos.column, 0);

        let pos = buffer
            .offset_to_position(1)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 0, "Byte 1 (newline) should be on line 0");
        assert_eq!(pos.column, 1);

        let pos = buffer
            .offset_to_position(2)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 1, "Byte 2 should be on line 1");
        assert_eq!(pos.column, 0);

        let pos = buffer
            .offset_to_position(3)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 1, "Byte 3 (newline) should be on line 1");
        assert_eq!(pos.column, 1);

        let pos = buffer
            .offset_to_position(4)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 2, "Byte 4 should be on line 2");
        assert_eq!(pos.column, 0);

        let pos = buffer
            .offset_to_position(6)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 3, "Byte 6 should be on line 3");
        assert_eq!(pos.column, 0);
    }

    #[test]
    fn test_offset_to_position_after_insert() {
        // Start with simple content
        let mut buffer = TextBuffer::from_bytes(b"a\nb\n".to_vec(), test_fs());

        // Insert at position 2 (start of line 1)
        buffer.insert_at_position(Position { line: 1, column: 0 }, b"x\n".to_vec());

        // After insert, buffer should be: "a\nx\nb\n"
        // Line 0: "a\n" (bytes 0-1)
        // Line 1: "x\n" (bytes 2-3)
        // Line 2: "b\n" (bytes 4-5)

        let pos = buffer
            .offset_to_position(0)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 0, "Byte 0 should still be on line 0");

        let pos = buffer
            .offset_to_position(2)
            .expect("small buffer should have line metadata");
        assert_eq!(
            pos.line, 1,
            "Byte 2 (start of inserted line) should be on line 1"
        );

        let pos = buffer
            .offset_to_position(4)
            .expect("small buffer should have line metadata");
        assert_eq!(
            pos.line, 2,
            "Byte 4 (start of 'b') should be on line 2 after insert"
        );
    }

    #[test]
    fn test_offset_to_position_empty_lines() {
        // Test with empty lines: "\n\n\n"
        let buffer = TextBuffer::from_bytes(b"\n\n\n".to_vec(), test_fs());

        // Line 0: "\n" (byte 0)
        // Line 1: "\n" (byte 1)
        // Line 2: "\n" (byte 2)
        // Line 3: "" (empty, after last newline)

        let pos = buffer
            .offset_to_position(0)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 0, "Byte 0 should be on line 0");

        let pos = buffer
            .offset_to_position(1)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 1, "Byte 1 should be on line 1");

        let pos = buffer
            .offset_to_position(2)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 2, "Byte 2 should be on line 2");

        let pos = buffer
            .offset_to_position(3)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 3, "Byte 3 (EOF) should be on line 3");
    }

    #[test]
    fn test_offset_to_position_long_lines() {
        // Test with long lines to ensure it's not just line counting
        let mut content = Vec::new();
        content.extend_from_slice(b"aaaaaaaaaa\n"); // Line 0: 11 bytes (10 'a's + newline)
        content.extend_from_slice(b"bbbbbbbbbb\n"); // Line 1: 11 bytes
        content.extend_from_slice(b"cccccccccc"); // Line 2: 10 bytes (no newline)

        let buffer = TextBuffer::from_bytes(content.clone(), test_fs());

        // Test positions at start of each line
        let pos = buffer
            .offset_to_position(0)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 0, "Byte 0 should be on line 0");
        assert_eq!(pos.column, 0);

        let pos = buffer
            .offset_to_position(11)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 1, "Byte 11 (start of line 1) should be on line 1");
        assert_eq!(pos.column, 0);

        let pos = buffer
            .offset_to_position(22)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 2, "Byte 22 (start of line 2) should be on line 2");
        assert_eq!(pos.column, 0);

        // Test mid-line positions
        let pos = buffer
            .offset_to_position(5)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 0, "Byte 5 should be on line 0");
        assert_eq!(pos.column, 5);

        let pos = buffer
            .offset_to_position(16)
            .expect("small buffer should have line metadata");
        assert_eq!(pos.line, 1, "Byte 16 should be on line 1");
        assert_eq!(pos.column, 5);
    }

    #[test]
    fn test_line_iterator_with_offset_to_position() {
        // This combines line iterator with offset_to_position to find issues
        let mut buffer = TextBuffer::from_bytes(b"line0\nline1\nline2\n".to_vec(), test_fs());

        // Test creating line iterator at various positions
        for byte_pos in 0..=buffer.len() {
            let iter = buffer.line_iterator(byte_pos, 80);
            let iter_pos = iter.current_position();
            let expected_line = buffer
                .offset_to_position(byte_pos)
                .expect("small buffer should have line metadata")
                .line;
            let expected_line_start = buffer.position_to_offset(Position {
                line: expected_line,
                column: 0,
            });

            assert_eq!(
                iter_pos, expected_line_start,
                "LineIterator at byte {} should position at line start {} but got {}",
                byte_pos, expected_line_start, iter_pos
            );
        }
    }

    #[test]
    fn test_piece_tree_line_count_after_insert() {
        // Debug the piece tree structure after insert
        let mut buffer = TextBuffer::from_bytes(b"a\nb\n".to_vec(), test_fs());

        // Insert at line 1, column 0
        buffer.insert_at_position(Position { line: 1, column: 0 }, b"x\n".to_vec());

        // Manually verify line counts
        let content = buffer.slice_bytes(0..buffer.len());
        let newline_count = content.iter().filter(|&&b| b == b'\n').count();
        let expected_line_count = newline_count + 1;
        let actual_line_count = buffer.line_count();

        assert_eq!(
            actual_line_count,
            Some(expected_line_count),
            "Line count mismatch after insert"
        );
    }

    #[test]
    fn test_position_to_lsp_position_after_modification() {
        // This test demonstrates a bug in the piece tree's offset_to_position
        // where column calculation is incorrect after buffer modifications.
        // The position_to_lsp_position function works around this by using
        // line_start_offset to calculate the column correctly.

        // Initial content: "fn foo(val: i32) {\n    val + 1\n}\n"
        let initial = b"fn foo(val: i32) {\n    val + 1\n}\n";
        let mut buffer = TextBuffer::from_bytes(initial.to_vec(), test_fs());

        // Verify initial positions work correctly
        // Position 23 is 'v' of second "val" on line 1
        let (line, char) = buffer.position_to_lsp_position(23);
        assert_eq!(line, 1, "Initial: position 23 should be on line 1");
        assert_eq!(char, 4, "Initial: position 23 should be at char 4");

        // Simulate rename: delete "val" at position 23 (line 1, char 4) and insert "value"
        // Position 23 = line 1, char 4; Position 26 = line 1, char 7
        buffer.delete_range(
            Position { line: 1, column: 4 },
            Position { line: 1, column: 7 },
        );
        buffer.insert_bytes(23, b"value".to_vec()); // Insert "value"

        // Also rename the first occurrence
        // Position 7 = line 0, char 7; Position 10 = line 0, char 10
        buffer.delete_range(
            Position { line: 0, column: 7 },
            Position {
                line: 0,
                column: 10,
            },
        );
        buffer.insert_bytes(7, b"value".to_vec()); // Insert "value"

        // Buffer is now: "fn foo(value: i32) {\n    value + 1\n}\n"
        let content = String::from_utf8_lossy(&buffer.get_all_text().unwrap()).to_string();
        assert_eq!(content, "fn foo(value: i32) {\n    value + 1\n}\n");

        // Position 25 is now 'v' of second "value" on line 1
        // Line 0: "fn foo(value: i32) {\n" = 21 chars (positions 0-20)
        // Line 1: "    value + 1\n" starts at position 21
        // Position 25 = 21 + 4 = line 1, char 4

        // The workaround in position_to_lsp_position should give correct result
        let (line, char) = buffer.position_to_lsp_position(25);
        assert_eq!(
            line, 1,
            "After modification: position 25 should be on line 1"
        );
        assert_eq!(
            char, 4,
            "After modification: position 25 should be at char 4"
        );

        // Also verify position 21 (start of line 1) works
        let (line, char) = buffer.position_to_lsp_position(21);
        assert_eq!(line, 1, "Position 21 should be on line 1");
        assert_eq!(char, 0, "Position 21 should be at char 0 (start of line)");
    }

    #[test]
    fn test_detect_crlf() {
        assert_eq!(
            TextBuffer::detect_line_ending(b"hello\r\nworld\r\n"),
            LineEnding::CRLF
        );
    }

    #[test]
    fn test_detect_lf() {
        assert_eq!(
            TextBuffer::detect_line_ending(b"hello\nworld\n"),
            LineEnding::LF
        );
    }

    #[test]
    fn test_normalize_crlf() {
        let input = b"hello\r\nworld\r\n".to_vec();
        let output = TextBuffer::normalize_line_endings(input);
        assert_eq!(output, b"hello\nworld\n");
    }

    #[test]
    fn test_normalize_empty() {
        let input = Vec::new();
        let output = TextBuffer::normalize_line_endings(input);
        assert_eq!(output, Vec::<u8>::new());
    }

    /// Regression test: get_all_text() returns empty for large files with unloaded regions
    ///
    /// This was the root cause of a bug where recovery auto-save would save 0 bytes
    /// for large files, causing data loss on crash recovery.
    ///
    /// The fix is to use get_text_range_mut() which handles lazy loading.
    #[test]
    fn test_get_all_text_returns_empty_for_unloaded_buffers() {
        use tempfile::TempDir;
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("large_test.txt");

        // Create a 50KB file
        let original_content = "X".repeat(50_000);
        std::fs::write(&file_path, &original_content).unwrap();

        // Load with small threshold to trigger large file mode
        let mut buffer = TextBuffer::load_from_file(&file_path, 1024, test_fs()).unwrap();
        assert!(buffer.large_file, "Should be in large file mode");
        assert!(!buffer.buffers[0].is_loaded(), "Buffer should be unloaded");

        // Make a small edit
        buffer.insert_bytes(0, b"EDITED: ".to_vec());

        // get_all_text() now returns None for unloaded buffers instead of empty
        // This is the correct behavior - it signals that content is not available
        let content_immutable = buffer.get_all_text();

        // get_all_text() returns None because it uses get_text_range() which
        // returns None for unloaded regions
        assert!(
            content_immutable.is_none(),
            "get_all_text() should return None for large files with unloaded regions. \
             Got Some({} bytes) instead of None.",
            content_immutable.as_ref().map(|c| c.len()).unwrap_or(0)
        );

        // CORRECT BEHAVIOR: get_text_range_mut() handles lazy loading
        let total = buffer.total_bytes();
        let content_lazy = buffer.get_text_range_mut(0, total).unwrap();
        assert_eq!(
            content_lazy.len(),
            50_000 + 8,
            "get_text_range_mut() should return all content with lazy loading"
        );
        assert!(
            String::from_utf8_lossy(&content_lazy).starts_with("EDITED: "),
            "Content should start with our edit"
        );
    }

    // ===== Line Ending Conversion Tests =====

    mod line_ending_conversion {
        use super::*;

        #[test]
        fn test_convert_lf_to_crlf() {
            let input = b"Line 1\nLine 2\nLine 3\n";
            let result = TextBuffer::convert_line_endings_to(input, LineEnding::CRLF);
            assert_eq!(result, b"Line 1\r\nLine 2\r\nLine 3\r\n");
        }

        #[test]
        fn test_convert_crlf_to_lf() {
            let input = b"Line 1\r\nLine 2\r\nLine 3\r\n";
            let result = TextBuffer::convert_line_endings_to(input, LineEnding::LF);
            assert_eq!(result, b"Line 1\nLine 2\nLine 3\n");
        }

        #[test]
        fn test_convert_cr_to_lf() {
            let input = b"Line 1\rLine 2\rLine 3\r";
            let result = TextBuffer::convert_line_endings_to(input, LineEnding::LF);
            assert_eq!(result, b"Line 1\nLine 2\nLine 3\n");
        }

        #[test]
        fn test_convert_mixed_to_crlf() {
            // Mixed line endings: LF, CRLF, CR
            let input = b"Line 1\nLine 2\r\nLine 3\r";
            let result = TextBuffer::convert_line_endings_to(input, LineEnding::CRLF);
            assert_eq!(result, b"Line 1\r\nLine 2\r\nLine 3\r\n");
        }

        #[test]
        fn test_convert_lf_to_lf_is_noop() {
            let input = b"Line 1\nLine 2\nLine 3\n";
            let result = TextBuffer::convert_line_endings_to(input, LineEnding::LF);
            assert_eq!(result, input.to_vec());
        }

        #[test]
        fn test_convert_empty_content() {
            let input = b"";
            let result = TextBuffer::convert_line_endings_to(input, LineEnding::CRLF);
            assert_eq!(result, b"".to_vec());
        }

        #[test]
        fn test_convert_no_line_endings() {
            let input = b"No line endings here";
            let result = TextBuffer::convert_line_endings_to(input, LineEnding::CRLF);
            assert_eq!(result, b"No line endings here".to_vec());
        }

        #[test]
        fn test_set_line_ending_marks_modified() {
            let mut buffer = TextBuffer::from_bytes(b"Hello\nWorld\n".to_vec(), test_fs());
            assert!(!buffer.is_modified());

            buffer.set_line_ending(LineEnding::CRLF);
            assert!(buffer.is_modified());
        }

        #[test]
        fn test_set_default_line_ending_does_not_mark_modified() {
            let mut buffer = TextBuffer::empty(test_fs());
            assert!(!buffer.is_modified());

            buffer.set_default_line_ending(LineEnding::CRLF);
            assert!(!buffer.is_modified());
            assert_eq!(buffer.line_ending(), LineEnding::CRLF);
        }

        #[test]
        fn test_save_to_file_converts_lf_to_crlf() {
            use tempfile::TempDir;

            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test_lf_to_crlf.txt");

            // Create a file with LF line endings
            let original_content = b"Line 1\nLine 2\nLine 3\n";
            std::fs::write(&file_path, original_content).unwrap();

            // Load the file
            let mut buffer =
                TextBuffer::load_from_file(&file_path, DEFAULT_LARGE_FILE_THRESHOLD, test_fs())
                    .unwrap();
            assert_eq!(buffer.line_ending(), LineEnding::LF);

            // Change line ending to CRLF
            buffer.set_line_ending(LineEnding::CRLF);
            assert_eq!(buffer.line_ending(), LineEnding::CRLF);
            assert!(buffer.is_modified());

            // Save the file
            buffer.save_to_file(&file_path).unwrap();

            // Read back and verify CRLF
            let saved_bytes = std::fs::read(&file_path).unwrap();
            assert_eq!(&saved_bytes, b"Line 1\r\nLine 2\r\nLine 3\r\n");
        }

        #[test]
        fn test_save_to_file_converts_crlf_to_lf() {
            use tempfile::TempDir;

            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test_crlf_to_lf.txt");

            // Create a file with CRLF line endings
            let original_content = b"Line 1\r\nLine 2\r\nLine 3\r\n";
            std::fs::write(&file_path, original_content).unwrap();

            // Load the file
            let mut buffer =
                TextBuffer::load_from_file(&file_path, DEFAULT_LARGE_FILE_THRESHOLD, test_fs())
                    .unwrap();
            assert_eq!(buffer.line_ending(), LineEnding::CRLF);

            // Change line ending to LF
            buffer.set_line_ending(LineEnding::LF);
            assert_eq!(buffer.line_ending(), LineEnding::LF);
            assert!(buffer.is_modified());

            // Save the file
            buffer.save_to_file(&file_path).unwrap();

            // Read back and verify LF (no CRLF)
            let saved_bytes = std::fs::read(&file_path).unwrap();
            assert_eq!(&saved_bytes, b"Line 1\nLine 2\nLine 3\n");
        }

        #[test]
        #[cfg(unix)]
        fn test_save_to_unwritable_file() -> anyhow::Result<()> {
            use std::fs::Permissions;
            use std::os::unix::fs::PermissionsExt;
            use tempfile::TempDir;

            let temp_dir = TempDir::new().unwrap();
            let unwritable_dir = temp_dir.path().join("unwritable_dir");
            std::fs::create_dir(&unwritable_dir)?;

            let file_path = unwritable_dir.join("unwritable.txt");
            std::fs::write(&file_path, "original content")?;

            // Make directory unwritable to prevent rename/temp file creation
            std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o555))?;

            let mut buffer = TextBuffer::from_bytes(b"new content".to_vec(), test_fs());
            let result = buffer.save_to_file(&file_path);

            // Verify that it returns SudoSaveRequired
            match result {
                Err(e) => {
                    if let Some(sudo_err) = e.downcast_ref::<SudoSaveRequired>() {
                        assert_eq!(sudo_err.dest_path, file_path);
                        assert!(sudo_err.temp_path.exists());
                        // Cleanup temp file
                        drop(std::fs::remove_file(&sudo_err.temp_path));
                    } else {
                        panic!("Expected SudoSaveRequired error, got: {:?}", e);
                    }
                }
                Ok(_) => panic!("Expected error, but save succeeded"),
            }

            Ok(())
        }

        #[test]
        #[cfg(unix)]
        fn test_save_to_unwritable_directory() -> anyhow::Result<()> {
            use std::fs::Permissions;
            use std::os::unix::fs::PermissionsExt;
            use tempfile::TempDir;

            let temp_dir = TempDir::new().unwrap();
            let unwritable_dir = temp_dir.path().join("unwritable_dir");
            std::fs::create_dir(&unwritable_dir)?;

            let file_path = unwritable_dir.join("test.txt");

            // Make directory unwritable (no write allowed)
            std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o555))?;

            let mut buffer = TextBuffer::from_bytes(b"content".to_vec(), test_fs());
            let result = buffer.save_to_file(&file_path);

            match result {
                Err(e) => {
                    if let Some(sudo_err) = e.downcast_ref::<SudoSaveRequired>() {
                        assert_eq!(sudo_err.dest_path, file_path);
                        assert!(sudo_err.temp_path.exists());
                        // It should be in /tmp because the directory was not writable
                        assert!(sudo_err.temp_path.starts_with(std::env::temp_dir()));
                        // Cleanup
                        drop(std::fs::remove_file(&sudo_err.temp_path));
                    } else {
                        panic!("Expected SudoSaveRequired error, got: {:?}", e);
                    }
                }
                Ok(_) => panic!("Expected error, but save succeeded"),
            }

            Ok(())
        }
    }

    mod large_file_encoding_tests {
        use super::*;

        #[test]
        fn test_large_file_encoding_confirmation_display() {
            let confirmation = LargeFileEncodingConfirmation {
                path: PathBuf::from("/test/file.txt"),
                file_size: 150 * 1024 * 1024, // 150 MB
                encoding: Encoding::ShiftJis,
            };

            let display = format!("{}", confirmation);
            assert!(display.contains("150 MB"), "Display: {}", display);
            assert!(display.contains("Shift-JIS"), "Display: {}", display);
            assert!(
                display.contains("requires full load"),
                "Display: {}",
                display
            );
        }

        #[test]
        fn test_large_file_encoding_confirmation_equality() {
            let a = LargeFileEncodingConfirmation {
                path: PathBuf::from("/test/file.txt"),
                file_size: 100 * 1024 * 1024,
                encoding: Encoding::Gb18030,
            };
            let b = LargeFileEncodingConfirmation {
                path: PathBuf::from("/test/file.txt"),
                file_size: 100 * 1024 * 1024,
                encoding: Encoding::Gb18030,
            };
            let c = LargeFileEncodingConfirmation {
                path: PathBuf::from("/test/other.txt"),
                file_size: 100 * 1024 * 1024,
                encoding: Encoding::Gb18030,
            };

            assert_eq!(a, b);
            assert_ne!(a, c);
        }

        #[test]
        fn test_encoding_requires_confirmation() {
            // Resynchronizable encodings should NOT require confirmation
            assert!(!Encoding::Utf8.requires_full_file_load());
            assert!(!Encoding::Utf8Bom.requires_full_file_load());
            assert!(!Encoding::Ascii.requires_full_file_load());
            assert!(!Encoding::Latin1.requires_full_file_load());
            assert!(!Encoding::Windows1252.requires_full_file_load());
            assert!(!Encoding::Utf16Le.requires_full_file_load());
            assert!(!Encoding::Utf16Be.requires_full_file_load());

            // Non-resynchronizable CJK encodings SHOULD require confirmation
            assert!(Encoding::Gb18030.requires_full_file_load());
            assert!(Encoding::Gbk.requires_full_file_load());
            assert!(Encoding::ShiftJis.requires_full_file_load());
            assert!(Encoding::EucKr.requires_full_file_load());
        }

        #[test]
        fn test_check_large_file_encoding_small_file() {
            use tempfile::NamedTempFile;

            // Create a small file (well under threshold)
            let temp = NamedTempFile::new().unwrap();
            std::fs::write(temp.path(), b"hello world").unwrap();

            let result = TextBuffer::check_large_file_encoding(temp.path(), test_fs()).unwrap();
            assert!(
                result.is_none(),
                "Small files should not require confirmation"
            );
        }

        #[test]
        fn test_large_file_encoding_error_downcast() {
            // Verify that LargeFileEncodingConfirmation can be used as an anyhow error
            let confirmation = LargeFileEncodingConfirmation {
                path: PathBuf::from("/test/file.txt"),
                file_size: 200 * 1024 * 1024,
                encoding: Encoding::EucKr,
            };

            let error: anyhow::Error = confirmation.clone().into();
            let downcast = error.downcast_ref::<LargeFileEncodingConfirmation>();
            assert!(downcast.is_some());
            assert_eq!(downcast.unwrap().encoding, Encoding::EucKr);
        }
    }

    mod rebuild_pristine_saved_root_tests {
        use super::*;
        use crate::model::piece_tree::BufferLocation;
        use std::sync::Arc;

        /// Create a large-file-mode TextBuffer from raw bytes, simulating what
        /// `load_from_file` does for files above the large-file threshold.
        fn large_file_buffer(content: &[u8]) -> TextBuffer {
            let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
                Arc::new(crate::model::filesystem::StdFileSystem);
            let bytes = content.len();
            let buffer =
                crate::model::piece_tree::StringBuffer::new_loaded(0, content.to_vec(), false);
            let piece_tree = if bytes > 0 {
                crate::model::piece_tree::PieceTree::new(BufferLocation::Stored(0), 0, bytes, None)
            } else {
                crate::model::piece_tree::PieceTree::empty()
            };
            let saved_root = piece_tree.root();
            TextBuffer {
                fs,
                piece_tree,
                saved_root,
                buffers: vec![buffer],
                next_buffer_id: 1,
                file_path: None,
                modified: false,
                recovery_pending: false,
                large_file: true,
                line_feeds_scanned: false,
                is_binary: false,
                line_ending: LineEnding::LF,
                original_line_ending: LineEnding::LF,
                encoding: Encoding::Utf8,
                original_encoding: Encoding::Utf8,
                saved_file_size: Some(bytes),
                version: 0,
                config: BufferConfig::default(),
            }
        }

        /// Simulate prepare_line_scan + scanning: pre-split and compute lf counts.
        fn scan_line_feeds(buf: &mut TextBuffer) -> Vec<(usize, usize)> {
            buf.piece_tree.split_leaves_to_chunk_size(LOAD_CHUNK_SIZE);
            let leaves = buf.piece_tree.get_leaves();
            let mut updates = Vec::new();
            for (idx, leaf) in leaves.iter().enumerate() {
                if leaf.line_feed_cnt.is_some() {
                    continue;
                }
                let count = buf.scan_leaf(leaf).unwrap();
                updates.push((idx, count));
            }
            updates
        }

        /// Generate a repeating pattern with newlines for testing.
        fn make_content(size: usize) -> Vec<u8> {
            let line = b"abcdefghij0123456789ABCDEFGHIJ0123456789abcdefghij0123456789ABCDEFGHIJ\n";
            let mut out = Vec::with_capacity(size);
            while out.len() < size {
                let remaining = size - out.len();
                let take = remaining.min(line.len());
                out.extend_from_slice(&line[..take]);
            }
            out
        }

        #[test]
        fn test_no_edits_arc_ptr_eq() {
            let content = make_content(2 * 1024 * 1024);
            let expected_lf = content.iter().filter(|&&b| b == b'\n').count();
            let mut buf = large_file_buffer(&content);

            // Before scan, line_count should be None (large file, no indexing).
            assert!(buf.line_count().is_none());

            let updates = scan_line_feeds(&mut buf);
            buf.rebuild_with_pristine_saved_root(&updates);

            // After rebuild, line_count must be Some (exact).
            assert_eq!(buf.line_count(), Some(expected_lf + 1));

            // After rebuild with no edits, roots should be identical (Arc::ptr_eq).
            assert!(Arc::ptr_eq(&buf.saved_root, &buf.piece_tree.root()));
            let diff = buf.diff_since_saved();
            assert!(diff.equal);
            assert!(buf.line_feeds_scanned);
            assert_eq!(buf.get_all_text().unwrap(), content);
        }

        #[test]
        fn test_single_insertion() {
            let content = make_content(2 * 1024 * 1024);
            let mut buf = large_file_buffer(&content);
            let updates = scan_line_feeds(&mut buf);

            // Insert some text in the middle.
            let insert_offset = 1_000_000;
            let insert_text = b"INSERTED_TEXT\n";
            buf.insert_bytes(insert_offset, insert_text.to_vec());

            buf.rebuild_with_pristine_saved_root(&updates);

            // Content should match the shadow model.
            let mut expected = content.clone();
            expected.splice(insert_offset..insert_offset, insert_text.iter().copied());
            assert_eq!(buf.get_all_text().unwrap(), expected);

            // line_count must be Some (exact) after rebuild, even with edits.
            let expected_lf = expected.iter().filter(|&&b| b == b'\n').count();
            assert_eq!(buf.line_count(), Some(expected_lf + 1));

            // Diff should NOT be equal.
            let diff = buf.diff_since_saved();
            assert!(!diff.equal);
            assert!(!diff.byte_ranges.is_empty());
        }

        /// After rebuild + insert near EOF, diff line_ranges must be
        /// document-absolute.  The bug: `with_doc_offsets` assigned consecutive
        /// offsets from 0 to the collected leaves, missing skipped (shared)
        /// subtrees' bytes.
        #[test]
        fn test_diff_line_ranges_are_document_absolute_after_eof_insert() {
            let content = make_content(4 * 1024 * 1024); // 4MB → 4 chunks at 1MB each
            let total_lf = content.iter().filter(|&&b| b == b'\n').count();
            let mut buf = large_file_buffer(&content);
            let updates = scan_line_feeds(&mut buf);
            buf.rebuild_with_pristine_saved_root(&updates);

            // Insert 5 bytes near EOF (last 100 bytes of the file).
            let insert_offset = content.len() - 100;
            buf.insert_bytes(insert_offset, b"HELLO".to_vec());

            let diff = buf.diff_since_saved();
            assert!(!diff.equal, "diff should detect the insertion");
            assert!(
                !diff.byte_ranges.is_empty(),
                "byte_ranges should not be empty"
            );

            // byte_ranges must be near the end of the document, not near 0.
            let first_range = &diff.byte_ranges[0];
            assert!(
                first_range.start >= content.len() - 200,
                "byte_ranges should be document-absolute (near EOF): got {:?}, expected near {}",
                first_range,
                insert_offset,
            );

            // line_ranges must also be document-absolute.
            let line_ranges = diff
                .line_ranges
                .as_ref()
                .expect("line_ranges should be Some");
            assert!(!line_ranges.is_empty(), "line_ranges should not be empty");
            let first_lr = &line_ranges[0];
            // The insert is near EOF, so the line number should be near total_lf.
            let expected_min_line = total_lf.saturating_sub(10);
            assert!(
                first_lr.start >= expected_min_line,
                "line_ranges should be document-absolute: got {:?}, expected start >= {} (total lines ~{})",
                first_lr,
                expected_min_line,
                total_lf,
            );
        }

        #[test]
        fn test_single_deletion() {
            let content = make_content(2 * 1024 * 1024);
            let mut buf = large_file_buffer(&content);
            let updates = scan_line_feeds(&mut buf);

            // Delete a range.
            let del_start = 500_000;
            let del_len = 1000;
            buf.delete_bytes(del_start, del_len);

            buf.rebuild_with_pristine_saved_root(&updates);

            let mut expected = content.clone();
            expected.drain(del_start..del_start + del_len);
            assert_eq!(buf.get_all_text().unwrap(), expected);

            let diff = buf.diff_since_saved();
            assert!(!diff.equal);
        }

        #[test]
        fn test_insert_and_delete() {
            let content = make_content(2 * 1024 * 1024);
            let mut buf = large_file_buffer(&content);
            let updates = scan_line_feeds(&mut buf);

            // Delete near the start, insert near the end.
            let del_start = 100_000;
            let del_len = 500;
            buf.delete_bytes(del_start, del_len);

            let insert_offset = 1_500_000; // in the post-delete document
            let insert_text = b"NEW_CONTENT\n";
            buf.insert_bytes(insert_offset, insert_text.to_vec());

            buf.rebuild_with_pristine_saved_root(&updates);

            // Build expected content.
            let mut expected = content.clone();
            expected.drain(del_start..del_start + del_len);
            expected.splice(insert_offset..insert_offset, insert_text.iter().copied());
            assert_eq!(buf.get_all_text().unwrap(), expected);

            let diff = buf.diff_since_saved();
            assert!(!diff.equal);
        }

        #[test]
        fn test_multiple_scattered_edits() {
            let content = make_content(3 * 1024 * 1024);
            let mut buf = large_file_buffer(&content);
            let updates = scan_line_feeds(&mut buf);
            let mut expected = content.clone();

            // Apply several edits across chunk boundaries, tracking the shadow model.
            // Edit 1: delete at offset 100k
            buf.delete_bytes(100_000, 200);
            expected.drain(100_000..100_200);

            // Edit 2: insert at offset 500k (in current doc, which shifted)
            buf.insert_bytes(500_000, b"AAAA\n".to_vec());
            expected.splice(500_000..500_000, b"AAAA\n".iter().copied());

            // Edit 3: delete at offset 2M
            buf.delete_bytes(2_000_000, 300);
            expected.drain(2_000_000..2_000_300);

            // Edit 4: insert at offset 1M
            buf.insert_bytes(1_000_000, b"BBBB\n".to_vec());
            expected.splice(1_000_000..1_000_000, b"BBBB\n".iter().copied());

            buf.rebuild_with_pristine_saved_root(&updates);

            assert_eq!(buf.get_all_text().unwrap(), expected);
            let diff = buf.diff_since_saved();
            assert!(!diff.equal);
        }

        #[test]
        fn test_content_preserved_after_rebuild() {
            // Verify that get_all_text matches before and after rebuild for
            // a buffer with edits.
            let content = make_content(2 * 1024 * 1024);
            let mut buf = large_file_buffer(&content);
            let updates = scan_line_feeds(&mut buf);

            buf.insert_bytes(0, b"HEADER\n".to_vec());
            buf.delete_bytes(1_000_000, 500);

            let text_before = buf.get_all_text().unwrap();
            buf.rebuild_with_pristine_saved_root(&updates);
            let text_after = buf.get_all_text().unwrap();

            assert_eq!(text_before, text_after);
        }

        /// Create a large-file-mode TextBuffer backed by an actual file on disk
        /// (Unloaded buffer), matching the real `load_from_file` code path.
        fn large_file_buffer_unloaded(path: &std::path::Path, file_size: usize) -> TextBuffer {
            let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
                Arc::new(crate::model::filesystem::StdFileSystem);
            let buffer = crate::model::piece_tree::StringBuffer::new_unloaded(
                0,
                path.to_path_buf(),
                0,
                file_size,
            );
            let piece_tree = if file_size > 0 {
                crate::model::piece_tree::PieceTree::new(
                    BufferLocation::Stored(0),
                    0,
                    file_size,
                    None,
                )
            } else {
                crate::model::piece_tree::PieceTree::empty()
            };
            let saved_root = piece_tree.root();
            TextBuffer {
                fs,
                piece_tree,
                saved_root,
                buffers: vec![buffer],
                next_buffer_id: 1,
                file_path: Some(path.to_path_buf()),
                modified: false,
                recovery_pending: false,
                large_file: true,
                line_feeds_scanned: false,
                is_binary: false,
                line_ending: LineEnding::LF,
                original_line_ending: LineEnding::LF,
                encoding: Encoding::Utf8,
                original_encoding: Encoding::Utf8,
                saved_file_size: Some(file_size),
                version: 0,
                config: BufferConfig::default(),
            }
        }

        #[test]
        fn test_unloaded_buffer_no_edits_line_count() {
            let content = make_content(2 * 1024 * 1024);
            let expected_lf = content.iter().filter(|&&b| b == b'\n').count();

            let tmp = tempfile::NamedTempFile::new().unwrap();
            std::fs::write(tmp.path(), &content).unwrap();
            let mut buf = large_file_buffer_unloaded(tmp.path(), content.len());

            assert!(
                buf.line_count().is_none(),
                "before scan, line_count should be None"
            );

            let updates = scan_line_feeds(&mut buf);
            buf.rebuild_with_pristine_saved_root(&updates);

            assert_eq!(
                buf.line_count(),
                Some(expected_lf + 1),
                "after rebuild, line_count must be exact"
            );
            assert!(buf.line_feeds_scanned);
        }

        #[test]
        fn test_unloaded_buffer_with_edits_line_count() {
            let content = make_content(2 * 1024 * 1024);

            let tmp = tempfile::NamedTempFile::new().unwrap();
            std::fs::write(tmp.path(), &content).unwrap();
            let mut buf = large_file_buffer_unloaded(tmp.path(), content.len());

            let updates = scan_line_feeds(&mut buf);

            // Insert text in the middle (creates an Added piece).
            let insert_text = b"INSERTED\n";
            buf.insert_bytes(1_000_000, insert_text.to_vec());

            buf.rebuild_with_pristine_saved_root(&updates);

            let mut expected = content.clone();
            expected.splice(1_000_000..1_000_000, insert_text.iter().copied());
            let expected_lf = expected.iter().filter(|&&b| b == b'\n').count();

            assert_eq!(
                buf.line_count(),
                Some(expected_lf + 1),
                "after rebuild with edits, line_count must be exact"
            );
            assert!(buf.line_feeds_scanned);
        }

        /// After rebuild, diff_since_saved should visit a small number of nodes
        /// proportional to edit regions, NOT the full tree. This catches
        /// regressions where Arc pointers are accidentally destroyed (e.g. by
        /// flattening and rebuilding the tree).
        #[test]
        fn test_diff_efficiency_after_rebuild() {
            // Use 32MB so the tree has ~32 leaves (at 1MB chunk size),
            // making the efficiency difference between O(log N) and O(N) clear.
            let content = make_content(32 * 1024 * 1024);
            let mut buf = large_file_buffer(&content);

            let updates = scan_line_feeds(&mut buf);

            // Insert a small piece of text in one chunk.
            buf.insert_bytes(1_000_000, b"HELLO".to_vec());

            buf.rebuild_with_pristine_saved_root(&updates);

            let diff = buf.diff_since_saved();
            assert!(!diff.equal);

            let total_leaves = buf.piece_tree.get_leaves().len();
            // The diff should visit far fewer nodes than the total tree.
            // With path-copying, only the path from root to the edited leaf
            // (and its immediate neighbours) should be visited — roughly
            // O(log N) nodes, not O(N).
            assert!(
                diff.nodes_visited < total_leaves,
                "diff visited {} nodes but tree has {} leaves — \
                 Arc::ptr_eq short-circuiting is not working",
                diff.nodes_visited,
                total_leaves,
            );
        }

        /// After rebuild_with_pristine_saved_root, loading a small viewport
        /// range must NOT cause the entire original file buffer to be loaded.
        /// This is a regression test for a bug where the pristine tree's 1MB
        /// pieces all referenced Stored(0) (the whole-file buffer). Because
        /// piece_view.bytes (1MB) <= LOAD_CHUNK_SIZE, get_text_range_mut took
        /// the "load_small_buffer" path, calling load() on the 814MB buffer.
        #[test]
        fn test_viewport_load_after_rebuild_does_not_load_entire_file() {
            use std::sync::atomic::{AtomicUsize, Ordering};

            /// Filesystem wrapper that tracks the largest read_range call.
            struct TrackingFs {
                inner: crate::model::filesystem::StdFileSystem,
                max_read_range_len: Arc<AtomicUsize>,
            }

            impl crate::model::filesystem::FileSystem for TrackingFs {
                fn read_file(&self, path: &Path) -> std::io::Result<Vec<u8>> {
                    self.inner.read_file(path)
                }
                fn read_range(
                    &self,
                    path: &Path,
                    offset: u64,
                    len: usize,
                ) -> std::io::Result<Vec<u8>> {
                    self.max_read_range_len.fetch_max(len, Ordering::SeqCst);
                    self.inner.read_range(path, offset, len)
                }
                fn write_file(&self, path: &Path, data: &[u8]) -> std::io::Result<()> {
                    self.inner.write_file(path, data)
                }
                fn create_file(
                    &self,
                    path: &Path,
                ) -> std::io::Result<Box<dyn crate::model::filesystem::FileWriter>>
                {
                    self.inner.create_file(path)
                }
                fn open_file(
                    &self,
                    path: &Path,
                ) -> std::io::Result<Box<dyn crate::model::filesystem::FileReader>>
                {
                    self.inner.open_file(path)
                }
                fn open_file_for_write(
                    &self,
                    path: &Path,
                ) -> std::io::Result<Box<dyn crate::model::filesystem::FileWriter>>
                {
                    self.inner.open_file_for_write(path)
                }
                fn open_file_for_append(
                    &self,
                    path: &Path,
                ) -> std::io::Result<Box<dyn crate::model::filesystem::FileWriter>>
                {
                    self.inner.open_file_for_append(path)
                }
                fn set_file_length(&self, path: &Path, len: u64) -> std::io::Result<()> {
                    self.inner.set_file_length(path, len)
                }
                fn rename(&self, from: &Path, to: &Path) -> std::io::Result<()> {
                    self.inner.rename(from, to)
                }
                fn copy(&self, from: &Path, to: &Path) -> std::io::Result<u64> {
                    self.inner.copy(from, to)
                }
                fn remove_file(&self, path: &Path) -> std::io::Result<()> {
                    self.inner.remove_file(path)
                }
                fn remove_dir(&self, path: &Path) -> std::io::Result<()> {
                    self.inner.remove_dir(path)
                }
                fn metadata(
                    &self,
                    path: &Path,
                ) -> std::io::Result<crate::model::filesystem::FileMetadata> {
                    self.inner.metadata(path)
                }
                fn symlink_metadata(
                    &self,
                    path: &Path,
                ) -> std::io::Result<crate::model::filesystem::FileMetadata> {
                    self.inner.symlink_metadata(path)
                }
                fn is_dir(&self, path: &Path) -> std::io::Result<bool> {
                    self.inner.is_dir(path)
                }
                fn is_file(&self, path: &Path) -> std::io::Result<bool> {
                    self.inner.is_file(path)
                }
                fn set_permissions(
                    &self,
                    path: &Path,
                    permissions: &crate::model::filesystem::FilePermissions,
                ) -> std::io::Result<()> {
                    self.inner.set_permissions(path, permissions)
                }
                fn is_owner(&self, path: &Path) -> bool {
                    self.inner.is_owner(path)
                }
                fn read_dir(
                    &self,
                    path: &Path,
                ) -> std::io::Result<Vec<crate::model::filesystem::DirEntry>> {
                    self.inner.read_dir(path)
                }
                fn create_dir(&self, path: &Path) -> std::io::Result<()> {
                    self.inner.create_dir(path)
                }
                fn create_dir_all(&self, path: &Path) -> std::io::Result<()> {
                    self.inner.create_dir_all(path)
                }
                fn canonicalize(&self, path: &Path) -> std::io::Result<PathBuf> {
                    self.inner.canonicalize(path)
                }
                fn current_uid(&self) -> u32 {
                    self.inner.current_uid()
                }
                fn sudo_write(
                    &self,
                    path: &Path,
                    data: &[u8],
                    mode: u32,
                    uid: u32,
                    gid: u32,
                ) -> std::io::Result<()> {
                    self.inner.sudo_write(path, data, mode, uid, gid)
                }
            }

            // Create a 3MB file with newlines (3 chunks at LOAD_CHUNK_SIZE=1MB).
            let file_size = LOAD_CHUNK_SIZE * 3;
            let content = make_content(file_size);

            let tmp = tempfile::NamedTempFile::new().unwrap();
            std::fs::write(tmp.path(), &content).unwrap();

            let max_read = Arc::new(AtomicUsize::new(0));
            let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
                Arc::new(TrackingFs {
                    inner: crate::model::filesystem::StdFileSystem,
                    max_read_range_len: max_read.clone(),
                });

            // Build an unloaded large-file buffer with the tracking FS.
            let buffer = crate::model::piece_tree::StringBuffer::new_unloaded(
                0,
                tmp.path().to_path_buf(),
                0,
                file_size,
            );
            let piece_tree = PieceTree::new(BufferLocation::Stored(0), 0, file_size, None);
            let saved_root = piece_tree.root();
            let mut buf = TextBuffer {
                fs,
                piece_tree,
                saved_root,
                buffers: vec![buffer],
                next_buffer_id: 1,
                file_path: Some(tmp.path().to_path_buf()),
                modified: false,
                recovery_pending: false,
                large_file: true,
                line_feeds_scanned: false,
                is_binary: false,
                line_ending: LineEnding::LF,
                original_line_ending: LineEnding::LF,
                encoding: Encoding::Utf8,
                original_encoding: Encoding::Utf8,
                saved_file_size: Some(file_size),
                version: 0,
                config: BufferConfig::default(),
            };

            // Load a small viewport in the middle (forces chunk splitting).
            let viewport_offset = LOAD_CHUNK_SIZE + 100; // somewhere in chunk 2
            buf.get_text_range_mut(viewport_offset, 4096).unwrap();

            // Run the line-feed scan and rebuild the pristine tree.
            let updates = scan_line_feeds(&mut buf);
            buf.rebuild_with_pristine_saved_root(&updates);

            // Reset the tracker — we only care about reads AFTER the rebuild.
            max_read.store(0, Ordering::SeqCst);

            // Load the same viewport range again.
            buf.get_text_range_mut(viewport_offset, 4096).unwrap();

            let largest_read = max_read.load(Ordering::SeqCst);
            assert!(
                largest_read <= LOAD_CHUNK_SIZE,
                "After rebuild, loading a viewport triggered a read of {} bytes \
                 (file_size={}). This means the entire Stored buffer is being \
                 loaded instead of just the needed chunk.",
                largest_read,
                file_size,
            );
        }

        /// After rebuild_with_pristine_saved_root, loading a viewport must not
        /// destroy the line feed counts on pieces. The chunk-split path in
        /// get_text_range_mut calls split_at_offset, which invokes
        /// compute_line_feeds_static — returning None for unloaded buffers.
        /// This turns exact line numbers back into byte-based estimates.
        #[test]
        fn test_viewport_load_after_rebuild_preserves_line_counts() {
            let file_size = LOAD_CHUNK_SIZE * 3;
            let content = make_content(file_size);

            let tmp = tempfile::NamedTempFile::new().unwrap();
            std::fs::write(tmp.path(), &content).unwrap();
            let mut buf = large_file_buffer_unloaded(tmp.path(), content.len());

            // Scan + rebuild so every leaf has a known line_feed_cnt.
            let updates = scan_line_feeds(&mut buf);
            buf.rebuild_with_pristine_saved_root(&updates);

            let line_count_before = buf.piece_tree.line_count();
            assert!(
                line_count_before.is_some(),
                "line_count must be Some after rebuild"
            );

            // Load a viewport that starts in the MIDDLE of a piece, forcing
            // split_at_offset (not just replace_buffer_reference).
            let mid_piece_offset = LOAD_CHUNK_SIZE + LOAD_CHUNK_SIZE / 2;
            buf.get_text_range_mut(mid_piece_offset, 4096).unwrap();

            let line_count_after = buf.piece_tree.line_count();
            assert!(
                line_count_after.is_some(),
                "line_count must still be Some after viewport load \
                 (was {:?} before, now {:?})",
                line_count_before,
                line_count_after,
            );
            assert_eq!(
                line_count_before, line_count_after,
                "line_count must not change after viewport load"
            );
        }

        /// Same test but with Unloaded data (the fixup path).
        #[test]
        fn test_diff_efficiency_after_rebuild_unloaded() {
            let content = make_content(32 * 1024 * 1024);

            let tmp = tempfile::NamedTempFile::new().unwrap();
            std::fs::write(tmp.path(), &content).unwrap();
            let mut buf = large_file_buffer_unloaded(tmp.path(), content.len());

            let updates = scan_line_feeds(&mut buf);

            buf.insert_bytes(1_000_000, b"HELLO".to_vec());

            buf.rebuild_with_pristine_saved_root(&updates);

            let diff = buf.diff_since_saved();
            assert!(!diff.equal);

            let total_leaves = buf.piece_tree.get_leaves().len();
            assert!(
                diff.nodes_visited < total_leaves,
                "diff visited {} nodes but tree has {} leaves — \
                 Arc::ptr_eq short-circuiting is not working (unloaded path)",
                diff.nodes_visited,
                total_leaves,
            );
        }
    }
}

#[cfg(test)]
mod property_tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::*;
    use proptest::prelude::*;

    // Generate text with some newlines
    fn text_with_newlines() -> impl Strategy<Value = Vec<u8>> {
        prop::collection::vec(
            prop_oneof![(b'a'..=b'z').prop_map(|c| c), Just(b'\n'),],
            0..100,
        )
    }

    // Strategy to generate operations
    #[derive(Debug, Clone)]
    enum Operation {
        Insert { offset: usize, text: Vec<u8> },
        Delete { offset: usize, bytes: usize },
    }

    fn operation_strategy() -> impl Strategy<Value = Vec<Operation>> {
        prop::collection::vec(
            prop_oneof![
                (0usize..200, text_with_newlines())
                    .prop_map(|(offset, text)| { Operation::Insert { offset, text } }),
                (0usize..200, 1usize..50)
                    .prop_map(|(offset, bytes)| { Operation::Delete { offset, bytes } }),
            ],
            0..50,
        )
    }

    proptest! {
        #[test]
        fn prop_line_count_consistent(text in text_with_newlines()) {
            let buffer = TextBuffer::from_bytes(text.clone(), test_fs());

            let newline_count = text.iter().filter(|&&b| b == b'\n').count();
            prop_assert_eq!(buffer.line_count(), Some(newline_count + 1));
        }

        #[test]
        fn prop_get_all_text_matches_original(text in text_with_newlines()) {
            let buffer = TextBuffer::from_bytes(text.clone(), test_fs());
            prop_assert_eq!(buffer.get_all_text().unwrap(), text);
        }

        #[test]
        fn prop_insert_increases_size(
            text in text_with_newlines(),
            offset in 0usize..100,
            insert_text in text_with_newlines()
        ) {
            let mut buffer = TextBuffer::from_bytes(text, test_fs());
            let initial_bytes = buffer.total_bytes();

            let offset = offset.min(buffer.total_bytes());
            buffer.insert_bytes(offset, insert_text.clone());

            prop_assert_eq!(buffer.total_bytes(), initial_bytes + insert_text.len());
        }

        #[test]
        fn prop_delete_decreases_size(
            text in text_with_newlines(),
            offset in 0usize..100,
            delete_bytes in 1usize..50
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let mut buffer = TextBuffer::from_bytes(text, test_fs());
            let initial_bytes = buffer.total_bytes();

            let offset = offset.min(buffer.total_bytes());
            let delete_bytes = delete_bytes.min(buffer.total_bytes() - offset);

            if delete_bytes == 0 {
                return Ok(());
            }

            buffer.delete_bytes(offset, delete_bytes);

            prop_assert_eq!(buffer.total_bytes(), initial_bytes - delete_bytes);
        }

        #[test]
        fn prop_insert_then_delete_restores_original(
            text in text_with_newlines(),
            offset in 0usize..100,
            insert_text in text_with_newlines()
        ) {
            let mut buffer = TextBuffer::from_bytes(text.clone(), test_fs());

            let offset = offset.min(buffer.total_bytes());
            buffer.insert_bytes(offset, insert_text.clone());
            buffer.delete_bytes(offset, insert_text.len());

            prop_assert_eq!(buffer.get_all_text().unwrap(), text);
        }

        #[test]
        fn prop_offset_position_roundtrip(text in text_with_newlines()) {
            let buffer = TextBuffer::from_bytes(text.clone(), test_fs());

            for offset in 0..text.len() {
                let pos = buffer.offset_to_position(offset).expect("offset_to_position should succeed for valid offset");
                let back = buffer.position_to_offset(pos);
                prop_assert_eq!(back, offset, "Failed roundtrip for offset {}", offset);
            }
        }

        #[test]
        fn prop_get_text_range_valid(
            text in text_with_newlines(),
            offset in 0usize..100,
            length in 1usize..50
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let buffer = TextBuffer::from_bytes(text.clone(), test_fs());
            let offset = offset.min(buffer.total_bytes());
            let length = length.min(buffer.total_bytes() - offset);

            if length == 0 {
                return Ok(());
            }

            let result = buffer.get_text_range(offset, length);
            prop_assert_eq!(result, Some(text[offset..offset + length].to_vec()));
        }

        #[test]
        fn prop_operations_maintain_consistency(operations in operation_strategy()) {
            let mut buffer = TextBuffer::from_bytes(b"initial\ntext".to_vec(), test_fs());
            let mut expected_text = b"initial\ntext".to_vec();

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text.clone());

                        // Update expected
                        let offset = offset.min(expected_text.len());
                        expected_text.splice(offset..offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        if offset < buffer.total_bytes() {
                            let bytes = bytes.min(buffer.total_bytes() - offset);
                            buffer.delete_bytes(offset, bytes);

                            // Update expected
                            if offset < expected_text.len() {
                                let bytes = bytes.min(expected_text.len() - offset);
                                expected_text.drain(offset..offset + bytes);
                            }
                        }
                    }
                }
            }

            prop_assert_eq!(buffer.get_all_text().unwrap(), expected_text);
        }

        #[test]
        fn prop_line_count_never_zero(operations in operation_strategy()) {
            let mut buffer = TextBuffer::from_bytes(b"test".to_vec(), test_fs());

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        buffer.delete_bytes(offset, bytes);
                    }
                }

                // Document always has at least 1 line
                prop_assert!(buffer.line_count().unwrap_or(1) >= 1);
            }
        }

        #[test]
        fn prop_total_bytes_never_negative(operations in operation_strategy()) {
            let mut buffer = TextBuffer::from_bytes(b"test".to_vec(), test_fs());

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        buffer.delete_bytes(offset, bytes);
                    }
                }

                // Bytes should never overflow
                prop_assert!(buffer.total_bytes() < 10_000_000);
            }
        }

        #[test]
        fn prop_piece_tree_and_line_index_stay_synced(operations in operation_strategy()) {
            let mut buffer = TextBuffer::from_bytes(b"line1\nline2\nline3".to_vec(), test_fs());

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        buffer.delete_bytes(offset, bytes);
                    }
                }

                // Verify we can still convert between offsets and positions
                if buffer.total_bytes() > 0 {
                    let mid_offset = buffer.total_bytes() / 2;
                    if let Some(pos) = buffer.offset_to_position(mid_offset) {
                        let back = buffer.position_to_offset(pos);

                        // Should be able to roundtrip
                        prop_assert!(back <= buffer.total_bytes());
                    }
                }
            }
        }

        #[test]
        fn prop_write_recipe_matches_content(text in text_with_newlines()) {
            let buffer = TextBuffer::from_bytes(text.clone(), test_fs());
            let recipe = buffer.build_write_recipe().expect("build_write_recipe should succeed");

            // Apply the recipe to get the output
            let output = apply_recipe(&buffer, &recipe);
            prop_assert_eq!(output, text, "Recipe output should match original content");
        }

        #[test]
        fn prop_write_recipe_after_edits(
            initial_text in text_with_newlines(),
            operations in operation_strategy()
        ) {
            let mut buffer = TextBuffer::from_bytes(initial_text, test_fs());

            // Apply random operations
            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        if offset < buffer.total_bytes() {
                            let bytes = bytes.min(buffer.total_bytes() - offset);
                            if bytes > 0 {
                                buffer.delete_bytes(offset, bytes);
                            }
                        }
                    }
                }
            }

            // Build recipe and verify it matches buffer content
            let expected = buffer.get_all_text().unwrap();
            let recipe = buffer.build_write_recipe().expect("build_write_recipe should succeed");
            let output = apply_recipe(&buffer, &recipe);

            prop_assert_eq!(output, expected, "Recipe output should match buffer content after edits");
        }

        #[test]
        fn prop_write_recipe_copy_ops_valid(
            text in prop::collection::vec(prop_oneof![(b'a'..=b'z').prop_map(|c| c), Just(b'\n')], 10..200),
            edit_offset in 0usize..100,
            edit_text in text_with_newlines()
        ) {
            use tempfile::TempDir;

            // Create a temp file with initial content
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");
            std::fs::write(&file_path, &text).unwrap();

            // Load the file (creates unloaded buffer regions)
            let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, test_fs()).unwrap();

            // Make an edit in the middle
            let edit_offset = edit_offset.min(buffer.total_bytes());
            buffer.insert_bytes(edit_offset, edit_text.clone());

            // Build recipe - should have Copy ops for unmodified regions
            let recipe = buffer.build_write_recipe().expect("build_write_recipe should succeed");

            // Verify recipe produces correct output
            let expected = buffer.get_all_text().unwrap();
            let output = apply_recipe(&buffer, &recipe);
            prop_assert_eq!(output, expected, "Recipe with Copy ops should match buffer content");

            // Verify we have at least some Copy ops if the file was large enough
            // (Copy ops reference unloaded regions from the original file)
            if text.len() > 100 && edit_offset > 10 {
                let has_copy = recipe.actions.iter().any(|a| matches!(a, RecipeAction::Copy { .. }));
                // Note: We don't assert this because line ending conversion or other factors
                // might cause all Insert ops, which is valid behavior
                let _ = has_copy;
            }
        }
    }

    /// Helper to apply a WriteRecipe and return the resulting bytes
    fn apply_recipe(buffer: &TextBuffer, recipe: &WriteRecipe) -> Vec<u8> {
        let mut output = Vec::new();
        for action in &recipe.actions {
            match action {
                RecipeAction::Copy { offset, len } => {
                    if let Some(src_path) = &recipe.src_path {
                        let data = buffer
                            .fs
                            .read_range(src_path, *offset, *len as usize)
                            .expect("read_range should succeed for Copy op");
                        output.extend_from_slice(&data);
                    } else {
                        panic!("Copy action without source path");
                    }
                }
                RecipeAction::Insert { index } => {
                    output.extend_from_slice(&recipe.insert_data[*index]);
                }
            }
        }
        output
    }

    /// Helper to check if bytes are detected as binary
    fn is_detected_as_binary(bytes: &[u8]) -> bool {
        TextBuffer::detect_encoding_or_binary(bytes).1
    }

    #[test]
    fn test_detect_binary_text_files() {
        // Plain text should not be detected as binary
        assert!(!is_detected_as_binary(b"Hello, world!"));
        assert!(!is_detected_as_binary(b"Line 1\nLine 2\nLine 3"));
        assert!(!is_detected_as_binary(b"Tabs\tand\tnewlines\n"));
        assert!(!is_detected_as_binary(b"Carriage return\r\n"));

        // Empty content is not binary
        assert!(!is_detected_as_binary(b""));

        // ANSI CSI escape sequences should be treated as text
        assert!(!is_detected_as_binary(b"\x1b[31mRed text\x1b[0m"));
    }

    #[test]
    fn test_detect_binary_binary_files() {
        // Null bytes indicate binary
        assert!(is_detected_as_binary(b"Hello\x00World"));
        assert!(is_detected_as_binary(b"\x00"));

        // Non-printable control characters (except tab, newline, CR, form feed, vertical tab)
        assert!(is_detected_as_binary(b"Text with \x01 control char"));
        assert!(is_detected_as_binary(b"\x02\x03\x04"));

        // DEL character (0x7F)
        assert!(is_detected_as_binary(b"Text with DEL\x7F"));
    }

    #[test]
    fn test_detect_binary_png_file() {
        // PNG file signature: 89 50 4E 47 0D 0A 1A 0A
        // The 0x1A byte (substitute character) is a control character that triggers binary detection
        let png_header: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];
        assert!(is_detected_as_binary(png_header));

        // Simulate a PNG file with more data after header
        let mut png_data = vec![0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];
        png_data.extend_from_slice(b"\x00\x00\x00\x0DIHDR"); // IHDR chunk with null bytes
        assert!(is_detected_as_binary(&png_data));
    }

    #[test]
    fn test_detect_binary_other_image_formats() {
        // JPEG signature: FF D8 FF
        let jpeg_header: &[u8] = &[0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10];
        assert!(is_detected_as_binary(jpeg_header));

        // GIF signature: GIF89a or GIF87a - contains valid ASCII but typically followed by binary
        // GIF header is ASCII but the LSD (Logical Screen Descriptor) contains binary
        let gif_data: &[u8] = &[
            0x47, 0x49, 0x46, 0x38, 0x39, 0x61, // GIF89a
            0x01, 0x00, 0x01, 0x00, // Width=1, Height=1 (little endian)
            0x00, // Packed byte
            0x00, // Background color index
            0x00, // Pixel aspect ratio
        ];
        // The null bytes in the dimensions trigger binary detection
        assert!(is_detected_as_binary(gif_data));

        // BMP signature: BM followed by file size (usually contains null bytes)
        let bmp_header: &[u8] = &[0x42, 0x4D, 0x00, 0x00, 0x00, 0x00];
        assert!(is_detected_as_binary(bmp_header));
    }

    #[test]
    fn test_detect_binary_executable_formats() {
        // ELF signature (Linux executables)
        let elf_header: &[u8] = &[0x7F, 0x45, 0x4C, 0x46, 0x02, 0x01, 0x01, 0x00];
        assert!(is_detected_as_binary(elf_header));

        // Mach-O signature (macOS executables) - magic + cpu type/subtype contain null bytes
        let macho_header: &[u8] = &[0xCF, 0xFA, 0xED, 0xFE, 0x07, 0x00, 0x00, 0x01];
        assert!(is_detected_as_binary(macho_header));

        // PE/COFF (Windows executables) - MZ header
        let pe_header: &[u8] = &[0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00];
        assert!(is_detected_as_binary(pe_header));
    }
}

/// Line data with optional line number
#[derive(Debug, Clone)]
pub struct LineData {
    /// Byte offset where this line starts in the document
    pub byte_offset: usize,
    /// Line content (without trailing newline)
    pub content: String,
    /// Whether this line ends with a newline
    pub has_newline: bool,
    /// Line number (None for large files without line metadata)
    pub line_number: Option<usize>,
}

/// Iterator over lines in a TextBuffer that efficiently tracks line numbers
/// using piece tree metadata (single source of truth)
pub struct TextBufferLineIterator {
    /// Collected lines (we collect all at once since we need mutable access to load chunks)
    lines: Vec<LineData>,
    /// Current index in the lines vector
    current_index: usize,
    /// Whether there are more lines after these
    pub has_more: bool,
}

impl TextBufferLineIterator {
    pub(crate) fn new(buffer: &mut TextBuffer, byte_pos: usize, max_lines: usize) -> Result<Self> {
        let buffer_len = buffer.len();
        if byte_pos >= buffer_len {
            return Ok(Self {
                lines: Vec::new(),
                current_index: 0,
                has_more: false,
            });
        }

        // Check if buffer has line metadata (None for large files > 1MB)
        let has_line_metadata = buffer.line_count().is_some();

        // Determine starting line number by querying piece tree once
        // (only if we have line metadata)
        let mut current_line = if has_line_metadata {
            buffer.offset_to_position(byte_pos).map(|pos| pos.line)
        } else {
            None
        };

        let mut lines = Vec::with_capacity(max_lines);
        let mut current_offset = byte_pos;
        let estimated_line_length = 80; // Use default estimate

        // Collect lines by scanning forward
        for _ in 0..max_lines {
            if current_offset >= buffer_len {
                break;
            }

            let line_start = current_offset;
            let line_number = current_line;

            // Estimate how many bytes to load for this line
            let estimated_max_line_length = estimated_line_length * 3;
            let bytes_to_scan = estimated_max_line_length.min(buffer_len - current_offset);

            // Load chunk (this handles lazy loading)
            let chunk = buffer.get_text_range_mut(current_offset, bytes_to_scan)?;

            // Scan for newline
            let mut line_len = 0;
            let mut found_newline = false;
            for &byte in chunk.iter() {
                line_len += 1;
                if byte == b'\n' {
                    found_newline = true;
                    break;
                }
            }

            // Handle long lines (rare case)
            if !found_newline && current_offset + line_len < buffer_len {
                // Line is longer than expected, load more data
                let remaining = buffer_len - current_offset - line_len;
                let additional_bytes = estimated_max_line_length.min(remaining);
                let more_chunk =
                    buffer.get_text_range_mut(current_offset + line_len, additional_bytes)?;

                let mut extended_chunk = chunk;
                extended_chunk.extend_from_slice(&more_chunk);

                for &byte in more_chunk.iter() {
                    line_len += 1;
                    if byte == b'\n' {
                        found_newline = true;
                        break;
                    }
                }

                let line_string = String::from_utf8_lossy(&extended_chunk[..line_len]).into_owned();
                let has_newline = line_string.ends_with('\n');
                let content = if has_newline {
                    line_string[..line_string.len() - 1].to_string()
                } else {
                    line_string
                };

                lines.push(LineData {
                    byte_offset: line_start,
                    content,
                    has_newline,
                    line_number,
                });

                current_offset += line_len;
                if has_line_metadata && found_newline {
                    current_line = current_line.map(|n| n + 1);
                }
                continue;
            }

            // Normal case
            let line_string = String::from_utf8_lossy(&chunk[..line_len]).into_owned();
            let has_newline = line_string.ends_with('\n');
            let content = if has_newline {
                line_string[..line_string.len() - 1].to_string()
            } else {
                line_string
            };

            lines.push(LineData {
                byte_offset: line_start,
                content,
                has_newline,
                line_number,
            });

            current_offset += line_len;
            // Increment line number if we have metadata and found a newline
            if has_line_metadata && found_newline {
                current_line = current_line.map(|n| n + 1);
            }
        }

        // Check if there are more lines
        let has_more = current_offset < buffer_len;

        Ok(Self {
            lines,
            current_index: 0,
            has_more,
        })
    }
}

impl Iterator for TextBufferLineIterator {
    type Item = LineData;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index < self.lines.len() {
            let line = self.lines[self.current_index].clone();
            self.current_index += 1;
            Some(line)
        } else {
            None
        }
    }
}
