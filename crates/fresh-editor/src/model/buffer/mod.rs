/// Text buffer that uses PieceTree with integrated line tracking
/// Architecture where the tree is the single source of truth for text and line information
use crate::model::encoding;
use crate::model::filesystem::{
    FileSearchOptions, FileSystem,
};
use crate::model::piece_tree::{
    BufferData, BufferLocation, Cursor, PieceInfo, PieceRangeIter, PieceTree, PieceView, Position,
    StringBuffer, TreeStats,
};
use crate::model::piece_tree_diff::PieceTreeDiff;
use crate::primitives::grapheme;
use anyhow::{Context, Result};
use regex::bytes::Regex;
use std::io;

use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

// Re-export Encoding for backward compatibility
pub use encoding::Encoding;

pub mod file_kind;
pub mod format;
pub mod persistence;
pub mod save;
pub mod search;
pub use file_kind::BufferFileKind;
pub use format::{BufferFormat, LineEnding};
pub use persistence::Persistence;
pub use save::SudoSaveRequired;
pub use search::{ChunkedSearchState, HybridSearchPlan};
use search::SearchRegion;
#[cfg(test)]
use search::search_boundary_overlap;
#[cfg(test)]
pub(crate) use save::{RecipeAction, WriteRecipe};


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

// Re-export SearchMatch from filesystem — same type is used by both
// FileSystem::search_file (project grep on disk) and the piece-tree
// search below (in-editor Ctrl+F and dirty buffers).
pub use crate::model::filesystem::SearchMatch;

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
    /// The piece tree for efficient text manipulation with integrated line tracking
    piece_tree: PieceTree,

    /// List of string buffers containing chunks of text data.
    /// Index 0 is typically the original/stored buffer.
    /// Additional buffers are added for modifications.
    buffers: Vec<StringBuffer>,

    /// Next buffer ID to assign.
    next_buffer_id: usize,

    /// Filesystem handle, optional file path, dirty/recovery flags,
    /// saved-root snapshot, and saved-file size — see
    /// `persistence.rs`.
    persistence: Persistence,

    /// File-kind flags (large_file, line_feeds_scanned, is_binary) —
    /// see `file_kind.rs`.
    file_kind: BufferFileKind,

    /// Encoding + line-ending state — see `format.rs`.
    format: BufferFormat,

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
        let saved_root = piece_tree.root();
        let line_ending = LineEnding::default();
        let encoding = Encoding::default();
        TextBuffer {
            piece_tree,
            buffers: vec![StringBuffer::new(0, Vec::new())],
            next_buffer_id: 1,
            persistence: Persistence::new(fs, None, saved_root, None),
            file_kind: BufferFileKind::new(false, false),
            format: BufferFormat::new(line_ending, encoding),
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
        buffer.persistence.set_file_path(path);
        buffer
    }

    /// Current buffer version (monotonic, wraps on overflow)
    pub fn version(&self) -> u64 {
        self.version
    }

    /// Get a reference to the filesystem implementation used by this buffer.
    pub fn filesystem(&self) -> &Arc<dyn FileSystem + Send + Sync> {
        self.persistence.fs()
    }

    /// Set the filesystem implementation for this buffer.
    pub fn set_filesystem(&mut self, fs: Arc<dyn FileSystem + Send + Sync>) {
        self.persistence.set_fs(fs);
    }

    #[inline]
    fn bump_version(&mut self) {
        self.version = self.version.wrapping_add(1);
    }

    #[inline]
    fn mark_content_modified(&mut self) {
        self.persistence.mark_dirty();
        self.bump_version();
    }

    /// Create a text buffer from raw bytes WITHOUT encoding conversion.
    /// Used for binary files where we want to preserve the exact bytes.
    fn from_bytes_raw(content: Vec<u8>, fs: Arc<dyn FileSystem + Send + Sync>) -> Self {
        let bytes = content.len();

        // For binary files, detect line ending but don't convert encoding
        let line_ending = format::detect_line_ending(&content);

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
            piece_tree,
            buffers: vec![buffer],
            next_buffer_id: 1,
            persistence: Persistence::new(fs, None, saved_root, Some(bytes)),
            file_kind: BufferFileKind::new(false, true),
            format: BufferFormat::new(line_ending, Encoding::Utf8),
            version: 0,
            config: BufferConfig::default(),
        }
    }

    /// Create a text buffer from initial content with the given filesystem.
    pub fn from_bytes(content: Vec<u8>, fs: Arc<dyn FileSystem + Send + Sync>) -> Self {
        // Auto-detect encoding and convert to UTF-8 if needed
        let (encoding, utf8_content) = format::detect_and_convert_encoding(&content);

        let bytes = utf8_content.len();

        // Auto-detect line ending format from content
        let line_ending = format::detect_line_ending(&utf8_content);

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
            piece_tree,
            buffers: vec![buffer],
            next_buffer_id: 1,
            persistence: Persistence::new(fs, None, saved_root, Some(bytes)),
            file_kind: BufferFileKind::new(false, false),
            format: BufferFormat::new(line_ending, encoding),
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
        let line_ending = format::detect_line_ending(&utf8_content);

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
            piece_tree,
            buffers: vec![buffer],
            next_buffer_id: 1,
            persistence: Persistence::new(fs, None, saved_root, Some(bytes)),
            file_kind: BufferFileKind::new(false, false),
            format: BufferFormat::new(line_ending, encoding),
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
            piece_tree,
            buffers: vec![StringBuffer::new(0, Vec::new())],
            next_buffer_id: 1,
            persistence: Persistence::new(fs, None, saved_root, None),
            file_kind: BufferFileKind::new(false, false),
            format: BufferFormat::new(line_ending, encoding),
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
        buffer.persistence.set_file_path(path.to_path_buf());
        buffer.persistence.clear_modified();
        buffer.config = config;
        Ok(buffer)
    }

    /// Load a small file with full eager loading and line indexing
    fn load_small_file(path: &Path, fs: Arc<dyn FileSystem + Send + Sync>) -> anyhow::Result<Self> {
        let contents = fs.read_file(path)?;

        // Use unified encoding/binary detection
        let (encoding, is_binary) = format::detect_encoding_or_binary(&contents, false);

        // For binary files, skip encoding conversion to preserve raw bytes
        let mut buffer = if is_binary {
            Self::from_bytes_raw(contents, fs)
        } else {
            // from_bytes handles encoding detection/conversion and line ending detection
            Self::from_bytes(contents, fs)
        };
        buffer.persistence.set_file_path(path.to_path_buf());
        buffer.persistence.clear_modified();
        buffer.file_kind.set_large_file(false);
        buffer.file_kind.set_binary(is_binary);
        // For binary files, ensure encoding matches detection
        if is_binary {
            buffer.format.set_default_encoding(encoding);
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
        let (encoding, is_binary) =
            format::detect_encoding_or_binary(&sample, file_size > sample_size);

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
        let (encoding, is_binary) =
            format::detect_encoding_or_binary(&sample, file_size > sample_size);

        // Binary files skip encoding conversion to preserve raw bytes
        if is_binary {
            tracing::info!("Large binary file detected, loading without encoding conversion");
            let contents = fs.read_file(path)?;
            let mut buffer = Self::from_bytes_raw(contents, fs);
            buffer.persistence.set_file_path(path.to_path_buf());
            buffer.persistence.clear_modified();
            buffer.file_kind.set_large_file(true);
            buffer.format.set_default_encoding(encoding);
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
            buffer.persistence.set_file_path(path.to_path_buf());
            buffer.persistence.clear_modified();
            buffer.file_kind.set_large_file(true); // Still mark as large file for UI purposes
            buffer.file_kind.set_binary(is_binary);
            return Ok(buffer);
        }

        // UTF-8/ASCII files can use lazy loading
        let line_ending = format::detect_line_ending(&sample);

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
            piece_tree,
            buffers: vec![buffer],
            next_buffer_id: 1,
            persistence: Persistence::new(
                fs,
                Some(path.to_path_buf()),
                saved_root,
                Some(file_size),
            ),
            file_kind: BufferFileKind::new(true, is_binary),
            format: BufferFormat::new(line_ending, encoding),
            version: 0,
            config: BufferConfig::default(),
        })
    }

    /// Save the buffer to its associated file
    pub fn save(&mut self) -> anyhow::Result<()> {
        if let Some(path) = self.persistence.file_path_owned() {
            self.save_to_file(path)
        } else {
            anyhow::bail!(io::Error::new(
                io::ErrorKind::NotFound,
                "No file path associated with buffer",
            ))
        }
    }

    /// Build a write recipe from the piece tree for saving.
    ///
    /// Delegates to `save::build_write_recipe`.
    #[cfg(test)]
    pub(crate) fn build_write_recipe(&self) -> io::Result<WriteRecipe> {
        save::build_write_recipe(
            &self.piece_tree,
            &self.buffers,
            &self.format,
            &self.file_kind,
            &self.persistence,
        )
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
            self.persistence.fs().write_file(dest_path, &[])?;
            self.finalize_save(dest_path)?;
            return Ok(());
        }

        // Build the write recipe (unified for all filesystem types)
        let recipe = save::build_write_recipe(
            &self.piece_tree,
            &self.buffers,
            &self.format,
            &self.file_kind,
            &self.persistence,
        )?;
        let ops = recipe.to_write_ops();

        // Check if we need in-place writing to preserve file ownership (local only)
        // Remote filesystems handle this differently
        let fs = self.persistence.fs();
        let is_local = fs.remote_connection_info().is_none();
        let use_inplace = is_local && save::should_use_inplace_write(fs, dest_path);

        if use_inplace {
            // In-place write: write directly to preserve ownership
            save::save_with_inplace_write(fs, dest_path, &recipe)?;
        } else if !recipe.has_copy_ops() && !is_local {
            // Remote with no Copy ops: use write_file directly (more efficient)
            let data = recipe.flatten_inserts();
            fs.write_file(dest_path, &data)?;
        } else if is_local {
            // Local: use write_file or write_patched with sudo fallback
            let write_result = if !recipe.has_copy_ops() {
                let data = recipe.flatten_inserts();
                fs.write_file(dest_path, &data)
            } else {
                let src_for_patch = recipe.src_path.as_deref().unwrap_or(dest_path);
                fs.write_patched(src_for_patch, dest_path, &ops)
            };

            if let Err(e) = write_result {
                if e.kind() == io::ErrorKind::PermissionDenied {
                    // Create temp file and return sudo error
                    let original_metadata = fs.metadata_if_exists(dest_path);
                    let (temp_path, mut temp_file) = save::create_temp_file(fs, dest_path)?;
                    save::write_recipe_to_file(fs, &mut temp_file, &recipe)?;
                    temp_file.sync_all()?;
                    drop(temp_file);
                    return Err(save::make_sudo_error(temp_path, dest_path, original_metadata));
                }
                return Err(e.into());
            }
        } else {
            // Remote with Copy ops: use write_patched
            let src_for_patch = recipe.src_path.as_deref().unwrap_or(dest_path);
            fs.write_patched(src_for_patch, dest_path, &ops)?;
        }

        self.finalize_save(dest_path)?;
        Ok(())
    }





    /// Finalize save state after successful write.
    fn finalize_save(&mut self, dest_path: &Path) -> anyhow::Result<()> {
        let new_size = self.persistence.fs().metadata(dest_path)?.size as usize;
        tracing::debug!(
            "Buffer::save: updating saved_file_size from {:?} to {}",
            self.persistence.saved_file_size(),
            new_size
        );
        self.persistence.set_saved_file_size(Some(new_size));
        self.persistence.set_file_path(dest_path.to_path_buf());

        // Consolidate the piece tree to synchronize with disk (for large files)
        // or to simplify structure (for small files).
        self.consolidate_after_save(dest_path, new_size);

        self.mark_saved_snapshot();
        self.format.promote_current_to_original();
        Ok(())
    }

    /// Finalize buffer state after an external save operation (e.g., via sudo).
    ///
    /// This updates the saved snapshot and file size to match the new state on disk.
    pub fn finalize_external_save(&mut self, dest_path: PathBuf) -> anyhow::Result<()> {
        let new_size = self.persistence.fs().metadata(&dest_path)?.size as usize;
        self.persistence.set_saved_file_size(Some(new_size));
        self.persistence.set_file_path(dest_path.clone());

        // Consolidate the piece tree to synchronize with disk or simplify structure.
        self.consolidate_after_save(&dest_path, new_size);

        self.mark_saved_snapshot();
        self.format.promote_current_to_original();
        Ok(())
    }

    /// Consolidate the piece tree into a single piece.
    /// For large files, this creates a reference to the disk file to save memory and sync offsets.
    /// For small files, this flattens all edits into a single in-memory buffer.
    fn consolidate_after_save(&mut self, path: &Path, file_size: usize) {
        if self.file_kind.is_large_file() {
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
        let preserved_lf = if self.file_kind.has_line_feed_scan() {
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
        self.persistence.mark_saved_snapshot(&self.piece_tree);
    }

    /// Refresh the saved root to match the current tree structure without
    /// clearing the modified flag.  Call this after structural-only changes
    /// (e.g. chunk_split_and_load during search scan) so that
    /// `diff_since_saved()` can take the fast `Arc::ptr_eq` path.
    pub fn refresh_saved_root_if_unmodified(&mut self) {
        self.persistence
            .refresh_saved_root_if_unmodified(&self.piece_tree);
    }

    /// Diff the current piece tree against the last saved snapshot.
    ///
    /// See `Persistence::diff_since_saved` for the algorithm.
    pub fn diff_since_saved(&self) -> PieceTreeDiff {
        let _span = tracing::info_span!(
            "diff_since_saved",
            large_file = self.file_kind.is_large_file(),
            modified = self.persistence.is_modified(),
            lf_scanned = self.file_kind.has_line_feed_scan()
        )
        .entered();

        self.persistence
            .diff_since_saved(&self.piece_tree, &self.buffers)
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
        if self.file_kind.has_line_feed_scan() {
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
        if self.file_kind.has_line_feed_scan() {
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

                if needs_loading && self.chunk_split_and_load(&piece_view, current_offset)? {
                    restarted_iteration = true;
                    break;
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
        tracing::trace!(
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
                .load(&**self.persistence.fs())
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
            .load(&**self.persistence.fs())
            .context("Failed to load chunk")?;

        // split_at_offset uses compute_line_feeds_static which returns None
        // for unloaded buffers, destroying the scanned line feed counts.
        // Fix up: the loaded chunk is counted from memory, remaining unloaded
        // pieces use the filesystem's count_line_feeds_in_range.
        if self.file_kind.has_line_feed_scan() {
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
        if !self.persistence.is_modified() {
            self.persistence.set_saved_root(self.piece_tree.root());
        } else {
            self.persistence.apply_chunk_load_to_saved_root(
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
        self.persistence.file_path()
    }

    /// Update the file path after a rename operation on disk.
    pub fn rename_file_path(&mut self, path: PathBuf) {
        self.persistence.set_file_path(path);
    }

    /// Clear the file path (make buffer unnamed)
    /// Note: This does NOT affect Unloaded chunk file_paths used for lazy loading.
    /// Those still point to the original source file for chunk loading.
    pub fn clear_file_path(&mut self) {
        self.persistence.clear_file_path();
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
        self.persistence.is_modified()
    }

    /// Clear the modified flag (after save)
    pub fn clear_modified(&mut self) {
        self.persistence.clear_modified();
    }

    /// Set the modified flag explicitly
    /// Used by undo/redo to restore the correct modified state
    pub fn set_modified(&mut self, modified: bool) {
        self.persistence.set_modified(modified);
    }

    /// Check if buffer has pending changes for recovery auto-save
    pub fn is_recovery_pending(&self) -> bool {
        self.persistence.is_recovery_pending()
    }

    /// Mark buffer as needing recovery auto-save (call after edits)
    pub fn set_recovery_pending(&mut self, pending: bool) {
        self.persistence.set_recovery_pending(pending);
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
                    if let Err(e) = buffer.load(&**self.persistence.fs()) {
                        tracing::warn!("Failed to load chunk at offset {offset}: {e}");
                    }
                }
            }
        }
    }

    /// Check if this is a large file with lazy loading enabled
    pub fn is_large_file(&self) -> bool {
        self.file_kind.is_large_file()
    }

    /// Check if line feeds have been scanned for this large file.
    /// When true, `line_count()` returns exact values.
    pub fn has_line_feed_scan(&self) -> bool {
        self.file_kind.has_line_feed_scan()
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

    /// Initialize a chunked search scan over this buffer's piece tree.
    ///
    /// Used for in-editor Ctrl+F (incremental, yields to the event loop
    /// between chunks) and for searching dirty buffers during project grep.
    /// For searching files on disk, use `FileSystem::search_file` instead.
    pub fn search_scan_init(
        &mut self,
        regex: regex::bytes::Regex,
        max_matches: usize,
        query_len: usize,
    ) -> ChunkedSearchState {
        let (chunks, total_bytes) = self.prepare_line_scan();
        ChunkedSearchState {
            chunks,
            next_chunk: 0,
            next_doc_offset: 0,
            total_bytes,
            scanned_bytes: 0,
            regex,
            matches: Vec::new(),
            overlap_tail: Vec::new(),
            overlap_doc_offset: 0,
            max_matches,
            capped: false,
            query_len,
            running_line: 1,
        }
    }

    /// Process one chunk of a chunked search scan.
    ///
    /// Loads the next chunk via `get_text_range_mut`, prepends overlap from
    /// the previous chunk, runs the regex, and appends matches to `state`
    /// with line/column/context computed on the fly from the loaded bytes.
    ///
    /// Line numbers are tracked incrementally via `running_line` — each
    /// chunk counts newlines in its non-overlap portion to advance the
    /// counter for the next chunk, and matches use an incremental cursor
    /// so total line-counting work is O(chunk_size), not O(chunk × matches).
    ///
    /// Returns `Ok(true)` if there are more chunks to process, `Ok(false)`
    /// when the scan is complete.
    ///
    /// TODO: For concurrent/parallel search (searching multiple files at once),
    /// chunks would need to return chunk-relative line numbers and have them
    /// fixed up with each file's starting line offset after all chunks complete.
    pub fn search_scan_next_chunk(
        &mut self,
        state: &mut ChunkedSearchState,
    ) -> std::io::Result<bool> {
        if state.is_done() {
            return Ok(false);
        }

        let chunk_info = state.chunks[state.next_chunk].clone();
        let doc_offset = state.next_doc_offset;

        state.next_chunk += 1;
        state.scanned_bytes += chunk_info.byte_len;
        state.next_doc_offset += chunk_info.byte_len;

        // Load the chunk bytes
        let chunk_bytes = self
            .get_text_range_mut(doc_offset, chunk_info.byte_len)
            .map_err(std::io::Error::other)?;

        // Build search buffer: overlap tail + new chunk
        let overlap_len = state.overlap_tail.len();
        let mut search_buf = Vec::with_capacity(overlap_len + chunk_bytes.len());
        search_buf.extend_from_slice(&state.overlap_tail);
        search_buf.extend_from_slice(&chunk_bytes);

        let buf_doc_offset = if overlap_len > 0 {
            state.overlap_doc_offset
        } else {
            doc_offset
        };

        // Line number at buf_doc_offset: running_line tracks the line at
        // doc_offset (start of new chunk data). Count newlines in the overlap
        // prefix to get the line at the start of the full search_buf.
        let newlines_in_overlap = search_buf[..overlap_len]
            .iter()
            .filter(|&&b| b == b'\n')
            .count();
        let mut line_at = state.running_line - newlines_in_overlap;
        let mut counted_to = 0usize;

        // Run regex on the combined buffer
        for m in state.regex.find_iter(&search_buf) {
            // Skip matches entirely within the overlap (already found)
            if overlap_len > 0 && m.end() <= overlap_len {
                continue;
            }

            if state.matches.len() >= state.max_matches {
                state.capped = true;
                break;
            }

            // Advance line counter incrementally to this match
            line_at += search_buf[counted_to..m.start()]
                .iter()
                .filter(|&&b| b == b'\n')
                .count();
            counted_to = m.start();

            // Find line boundaries in search_buf for context
            let line_start = search_buf[..m.start()]
                .iter()
                .rposition(|&b| b == b'\n')
                .map(|p| p + 1)
                .unwrap_or(0);
            let line_end = search_buf[m.start()..]
                .iter()
                .position(|&b| b == b'\n')
                .map(|p| m.start() + p)
                .unwrap_or(search_buf.len());

            let match_doc_offset = buf_doc_offset + m.start();
            let match_len = m.end() - m.start();
            let column = m.start() - line_start + 1;
            let context = String::from_utf8_lossy(&search_buf[line_start..line_end]).into_owned();

            state.matches.push(SearchMatch {
                byte_offset: match_doc_offset,
                length: match_len,
                line: line_at,
                column,
                context,
            });
        }

        // Advance running_line by newlines in the new (non-overlap) chunk data
        let newlines_in_chunk = chunk_bytes.iter().filter(|&&b| b == b'\n').count();
        state.running_line += newlines_in_chunk;

        // Save overlap tail for next chunk
        let max_overlap = state.query_len.max(256).min(chunk_bytes.len());
        let tail_start = chunk_bytes.len().saturating_sub(max_overlap);
        state.overlap_tail = chunk_bytes[tail_start..].to_vec();
        state.overlap_doc_offset = doc_offset + tail_start;

        Ok(!state.is_done())
    }

    /// Run a complete chunked search over the piece tree (all chunks).
    ///
    /// Synchronous variant — used for dirty buffer snapshots in project
    /// grep and in tests.  For on-disk files, use `FileSystem::search_file`.
    pub fn search_scan_all(
        &mut self,
        regex: regex::bytes::Regex,
        max_matches: usize,
        query_len: usize,
    ) -> std::io::Result<ChunkedSearchState> {
        let mut state = self.search_scan_init(regex, max_matches, query_len);
        while self.search_scan_next_chunk(&mut state)? {}
        Ok(state)
    }

    /// Build a hybrid search plan from the piece tree.
    ///
    /// Extracts regions (unloaded file ranges + loaded in-memory data) that
    /// can be searched independently.  The plan is `Send` so it can be
    /// executed on a background thread via `HybridSearchPlan::execute`.
    ///
    /// Returns `None` if the buffer has no file path (caller should fall
    /// back to `search_scan_all`).
    pub fn search_hybrid_plan(&mut self) -> Option<HybridSearchPlan> {
        let file_path = self.persistence.file_path_owned()?;

        self.piece_tree.split_leaves_to_chunk_size(LOAD_CHUNK_SIZE);
        let leaves = self.piece_tree.get_leaves();

        let mut regions: Vec<SearchRegion> = Vec::new();
        let mut doc_offset = 0usize;

        for leaf in &leaves {
            let buf = self.buffers.get(leaf.location.buffer_id());
            let is_unloaded_stored = matches!(
                (&leaf.location, buf),
                (
                    BufferLocation::Stored(_),
                    Some(StringBuffer {
                        data: BufferData::Unloaded { .. },
                        ..
                    }),
                )
            );

            if is_unloaded_stored {
                let file_offset = match buf.unwrap().data {
                    BufferData::Unloaded {
                        file_offset: fo, ..
                    } => fo + leaf.offset,
                    _ => unreachable!(),
                };

                // Merge with previous unloaded region if contiguous
                if let Some(SearchRegion::Unloaded {
                    file_offset: prev_fo,
                    bytes: prev_bytes,
                    ..
                }) = regions.last_mut()
                {
                    if *prev_fo + *prev_bytes == file_offset {
                        *prev_bytes += leaf.bytes;
                        doc_offset += leaf.bytes;
                        continue;
                    }
                }
                regions.push(SearchRegion::Unloaded {
                    file_offset,
                    bytes: leaf.bytes,
                    doc_offset,
                });
            } else {
                let data = match buf.and_then(|b| b.get_data()) {
                    Some(full) => {
                        let end = (leaf.offset + leaf.bytes).min(full.len());
                        full[leaf.offset..end].to_vec()
                    }
                    None => match self.get_text_range_mut(doc_offset, leaf.bytes) {
                        Ok(d) => d,
                        Err(_) => {
                            doc_offset += leaf.bytes;
                            continue;
                        }
                    },
                };

                // Merge with previous loaded region
                if let Some(SearchRegion::Loaded {
                    data: prev_data, ..
                }) = regions.last_mut()
                {
                    prev_data.extend_from_slice(&data);
                    doc_offset += leaf.bytes;
                    continue;
                }
                regions.push(SearchRegion::Loaded { data, doc_offset });
            }

            doc_offset += leaf.bytes;
        }

        Some(HybridSearchPlan { file_path, regions })
    }

    /// Hybrid search: uses `fs.search_file` for unloaded piece-tree regions
    /// (searches where the data lives, no network transfer) and in-memory regex
    /// for loaded/edited regions.  Handles overlap at region boundaries.
    ///
    /// For a huge remote file with a small local edit, this avoids transferring
    /// the entire file — only match metadata crosses the network.
    ///
    /// Falls back to `search_scan_all` when the buffer has no file path or is
    /// fully loaded.
    pub fn search_hybrid(
        &mut self,
        pattern: &str,
        opts: &FileSearchOptions,
        regex: Regex,
        max_matches: usize,
        query_len: usize,
    ) -> io::Result<Vec<SearchMatch>> {
        let plan = match self.search_hybrid_plan() {
            Some(p) => p,
            None => {
                let state = self.search_scan_all(regex, max_matches, query_len)?;
                return Ok(state.matches);
            }
        };
        plan.execute(&**self.persistence.fs(), pattern, opts, &regex, max_matches, query_len)
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
                self.persistence
                    .fs()
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
        self.file_kind.mark_line_feed_scan_complete();
    }

    /// After an incremental line-feed scan completes, rebuild the tree so that
    /// `saved_root` and the current tree share `Arc` pointers for unedited
    /// subtrees. This makes `diff_since_saved()` O(edited regions) instead of
    /// O(file size).
    pub fn rebuild_with_pristine_saved_root(&mut self, scan_updates: &[(usize, usize)]) {
        let file_size = match self.persistence.saved_file_size() {
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
        self.persistence.set_saved_root(pristine.root());

        // If no edits, the pristine tree IS the current tree.
        if deletions.is_empty() && insertions.is_empty() {
            self.piece_tree = pristine;
            self.file_kind.mark_line_feed_scan_complete();
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
        self.file_kind.mark_line_feed_scan_complete();
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
                self.persistence
                    .fs()
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
        self.persistence.saved_file_size()
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
                        // Skip buffers that originate from the original file
                        // (loaded by chunk_split_and_load for viewport display).
                        // These have stored_file_offset set and are not user edits.
                        //
                        // Why Added and not Stored? The piece tree only has two
                        // variants: Stored and Added. chunk_split_and_load marks
                        // loaded chunks as Added(new_id) because
                        // rebuild_with_pristine_saved_root interprets Stored
                        // pieces' buffer_offset as a position in the original
                        // file — but a chunk buffer starts at offset 0, so using
                        // Stored would corrupt the rebuild logic. We rely on
                        // stored_file_offset instead to distinguish "loaded from
                        // disk" from "user edit". A third BufferLocation variant
                        // (e.g. LoadedChunk) would make this distinction explicit
                        // in the type system rather than requiring this runtime
                        // check.
                        if buffer.stored_file_offset.is_some() {
                            stored_bytes_before += piece.bytes;
                            continue;
                        }
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
        self.file_kind.is_binary()
    }

    /// Get the line ending format for this buffer
    pub fn line_ending(&self) -> LineEnding {
        self.format.line_ending()
    }

    /// Set the line ending format for this buffer
    ///
    /// This marks the buffer as modified since the line ending format has changed.
    /// On save, the buffer content will be converted to the new format.
    pub fn set_line_ending(&mut self, line_ending: LineEnding) {
        self.format.set_line_ending(line_ending);
        self.mark_content_modified();
    }

    /// Set the default line ending format for a new/empty buffer
    ///
    /// Unlike `set_line_ending`, this does NOT mark the buffer as modified.
    /// This should be used when initializing a new buffer with a configured default.
    pub fn set_default_line_ending(&mut self, line_ending: LineEnding) {
        self.format.set_default_line_ending(line_ending);
    }

    /// Get the encoding format for this buffer
    pub fn encoding(&self) -> Encoding {
        self.format.encoding()
    }

    /// Set the encoding format for this buffer
    ///
    /// This marks the buffer as modified since the encoding format has changed.
    /// On save, the buffer content will be converted to the new encoding.
    pub fn set_encoding(&mut self, encoding: Encoding) {
        self.format.set_encoding(encoding);
        self.mark_content_modified();
    }

    /// Set the default encoding format for a new/empty buffer
    ///
    /// Unlike `set_encoding`, this does NOT mark the buffer as modified.
    /// This should be used when initializing a new buffer with a configured default.
    pub fn set_default_encoding(&mut self, encoding: Encoding) {
        self.format.set_default_encoding(encoding);
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
    ///
    /// The lookahead window starts at 32 bytes but grows whenever the
    /// returned boundary sits at the start of the chunk — that is, whenever
    /// the chunk might not contain the full grapheme. This matters for ZWJ
    /// emoji sequences and Zalgo strings with many combining marks, which
    /// can easily exceed 32 bytes.
    pub fn prev_grapheme_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        let mut lookback: usize = 32;
        loop {
            // IMPORTANT: Align start to a valid character boundary to avoid invalid UTF-8
            // when get_text_range starts mid-character
            let raw_start = pos.saturating_sub(lookback);
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

            // If the returned boundary is at the start of our chunk, the
            // grapheme may extend further back. Only trust the answer when
            // either we already reached the beginning of the buffer or the
            // boundary sits strictly inside the chunk.
            if new_rel_pos > 0 || start == 0 {
                return start + new_rel_pos;
            }

            // Expand the lookback window and retry. Cap at the full buffer.
            if lookback >= pos {
                return 0;
            }
            lookback = lookback.saturating_mul(2);
        }
    }

    /// Find the next grapheme cluster boundary (for proper cursor movement with combining characters)
    ///
    /// This handles complex scripts like Thai where multiple Unicode code points
    /// form a single visual character (grapheme cluster). For example, Thai "ที่"
    /// is 3 code points but 1 grapheme cluster.
    ///
    /// The lookahead window grows whenever the first grapheme reaches the
    /// end of the chunk — otherwise ZWJ emoji and Zalgo strings whose byte
    /// length exceeds the initial 32-byte window would be split mid-cluster.
    pub fn next_grapheme_boundary(&self, pos: usize) -> usize {
        let len = self.len();
        if pos >= len {
            return len;
        }

        let mut lookahead: usize = 32;
        loop {
            let end = (pos + lookahead).min(len);
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

            let new_rel_pos = grapheme::next_grapheme_boundary(text, 0);

            // If the first grapheme reaches the end of our chunk and there
            // is more buffer left beyond it, the grapheme may extend further.
            // Expand the window and retry.
            if new_rel_pos == text.len() && end < len {
                if lookahead >= len - pos {
                    return len;
                }
                lookahead = lookahead.saturating_mul(2);
                continue;
            }

            return pos + new_rel_pos;
        }
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
mod tests;

#[cfg(test)]
mod property_tests;

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
