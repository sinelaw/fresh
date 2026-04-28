//! Unified filesystem abstraction for platform-independent file and directory operations
//!
//! This module provides a single trait for all filesystem operations, allowing the editor
//! to work with different backends:
//! - `StdFileSystem`: Native filesystem using `std::fs`
//! - `VirtualFileSystem`: In-memory filesystem for WASM/browser (to be implemented)
//! - Custom implementations for remote agents, network filesystems, etc.
//!
//! The trait is synchronous. For async UI operations (like the file explorer),
//! callers should use `spawn_blocking` or similar patterns.

use std::io::{self, Read, Seek, Write};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

// ============================================================================
// Directory Entry Types
// ============================================================================

/// Type of filesystem entry
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EntryType {
    File,
    Directory,
    Symlink,
}

/// A directory entry returned by `read_dir`
#[derive(Debug, Clone)]
pub struct DirEntry {
    /// Full path to the entry
    pub path: PathBuf,
    /// File/directory name (last component of path)
    pub name: String,
    /// Type of entry
    pub entry_type: EntryType,
    /// Optional metadata (can be populated lazily)
    pub metadata: Option<FileMetadata>,
    /// For symlinks, whether the target is a directory
    pub symlink_target_is_dir: bool,
}

impl DirEntry {
    /// Create a new directory entry
    pub fn new(path: PathBuf, name: String, entry_type: EntryType) -> Self {
        Self {
            path,
            name,
            entry_type,
            metadata: None,
            symlink_target_is_dir: false,
        }
    }

    /// Create a symlink entry with target info
    pub fn new_symlink(path: PathBuf, name: String, target_is_dir: bool) -> Self {
        Self {
            path,
            name,
            entry_type: EntryType::Symlink,
            metadata: None,
            symlink_target_is_dir: target_is_dir,
        }
    }

    /// Add metadata to this entry
    pub fn with_metadata(mut self, metadata: FileMetadata) -> Self {
        self.metadata = Some(metadata);
        self
    }

    /// Returns true if this entry is a directory OR a symlink pointing to a directory
    pub fn is_dir(&self) -> bool {
        self.entry_type == EntryType::Directory
            || (self.entry_type == EntryType::Symlink && self.symlink_target_is_dir)
    }

    /// Returns true if this is a regular file (or symlink to file)
    pub fn is_file(&self) -> bool {
        self.entry_type == EntryType::File
            || (self.entry_type == EntryType::Symlink && !self.symlink_target_is_dir)
    }

    /// Returns true if this is a symlink
    pub fn is_symlink(&self) -> bool {
        self.entry_type == EntryType::Symlink
    }
}

// ============================================================================
// Metadata Types
// ============================================================================

/// Metadata about a file or directory
#[derive(Debug, Clone)]
pub struct FileMetadata {
    /// Size in bytes (0 for directories)
    pub size: u64,
    /// Last modification time
    pub modified: Option<SystemTime>,
    /// File permissions (opaque, platform-specific)
    pub permissions: Option<FilePermissions>,
    /// Whether this is a hidden file (starts with . on Unix, hidden attribute on Windows)
    pub is_hidden: bool,
    /// Whether the file is read-only
    pub is_readonly: bool,
    /// File owner UID (Unix only)
    #[cfg(unix)]
    pub uid: Option<u32>,
    /// File owner GID (Unix only)
    #[cfg(unix)]
    pub gid: Option<u32>,
}

impl FileMetadata {
    /// Create minimal metadata with just size
    pub fn new(size: u64) -> Self {
        Self {
            size,
            modified: None,
            permissions: None,
            is_hidden: false,
            is_readonly: false,
            #[cfg(unix)]
            uid: None,
            #[cfg(unix)]
            gid: None,
        }
    }

    /// Builder: set modified time
    pub fn with_modified(mut self, modified: SystemTime) -> Self {
        self.modified = Some(modified);
        self
    }

    /// Builder: set hidden flag
    pub fn with_hidden(mut self, hidden: bool) -> Self {
        self.is_hidden = hidden;
        self
    }

    /// Builder: set readonly flag
    pub fn with_readonly(mut self, readonly: bool) -> Self {
        self.is_readonly = readonly;
        self
    }

    /// Builder: set permissions
    pub fn with_permissions(mut self, permissions: FilePermissions) -> Self {
        self.permissions = Some(permissions);
        self
    }
}

impl Default for FileMetadata {
    fn default() -> Self {
        Self::new(0)
    }
}

/// Opaque file permissions wrapper
#[derive(Debug, Clone)]
pub struct FilePermissions {
    #[cfg(unix)]
    mode: u32,
    #[cfg(not(unix))]
    readonly: bool,
}

impl FilePermissions {
    /// Create from raw Unix mode bits
    #[cfg(unix)]
    pub fn from_mode(mode: u32) -> Self {
        Self { mode }
    }

    /// Create from raw mode (non-Unix: treated as readonly if no write bits)
    #[cfg(not(unix))]
    pub fn from_mode(mode: u32) -> Self {
        Self {
            readonly: mode & 0o222 == 0,
        }
    }

    /// Create from std::fs::Permissions
    #[cfg(unix)]
    pub fn from_std(perms: std::fs::Permissions) -> Self {
        use std::os::unix::fs::PermissionsExt;
        Self { mode: perms.mode() }
    }

    #[cfg(not(unix))]
    pub fn from_std(perms: std::fs::Permissions) -> Self {
        Self {
            readonly: perms.readonly(),
        }
    }

    /// Convert to std::fs::Permissions
    #[cfg(unix)]
    pub fn to_std(&self) -> std::fs::Permissions {
        use std::os::unix::fs::PermissionsExt;
        std::fs::Permissions::from_mode(self.mode)
    }

    #[cfg(not(unix))]
    pub fn to_std(&self) -> std::fs::Permissions {
        let mut perms = std::fs::Permissions::from(std::fs::metadata(".").unwrap().permissions());
        perms.set_readonly(self.readonly);
        perms
    }

    /// Get the Unix mode (if available)
    #[cfg(unix)]
    pub fn mode(&self) -> u32 {
        self.mode
    }

    /// Check if no write bits are set at all (any user).
    ///
    /// NOTE: On Unix, this only checks whether the mode has zero write bits.
    /// It does NOT check whether the *current user* can write. For that,
    /// use [`is_readonly_for_user`] with the appropriate uid/gid.
    pub fn is_readonly(&self) -> bool {
        #[cfg(unix)]
        {
            self.mode & 0o222 == 0
        }
        #[cfg(not(unix))]
        {
            self.readonly
        }
    }

    /// Check if the file is read-only for a specific user identified by
    /// `user_uid` and a set of group IDs the user belongs to.
    ///
    /// On non-Unix platforms, falls back to the simple readonly flag.
    #[cfg(unix)]
    pub fn is_readonly_for_user(
        &self,
        user_uid: u32,
        file_uid: u32,
        file_gid: u32,
        user_groups: &[u32],
    ) -> bool {
        // root can write to anything
        if user_uid == 0 {
            return false;
        }
        if user_uid == file_uid {
            return self.mode & 0o200 == 0;
        }
        if user_groups.contains(&file_gid) {
            return self.mode & 0o020 == 0;
        }
        self.mode & 0o002 == 0
    }
}

// ============================================================================
// File Handle Traits
// ============================================================================

/// A writable file handle
pub trait FileWriter: Write + Send {
    /// Sync all data to disk
    fn sync_all(&self) -> io::Result<()>;
}

// ============================================================================
// Patch Operations for Efficient Remote Saves
// ============================================================================

/// An operation in a patched write - either copy from source or insert new data
#[derive(Debug, Clone)]
pub enum WriteOp<'a> {
    /// Copy bytes from the source file at the given offset
    Copy { offset: u64, len: u64 },
    /// Insert new data
    Insert { data: &'a [u8] },
}

/// Wrapper around std::fs::File that implements FileWriter
struct StdFileWriter(std::fs::File);

impl Write for StdFileWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

impl FileWriter for StdFileWriter {
    fn sync_all(&self) -> io::Result<()> {
        self.0.sync_all()
    }
}

/// A readable and seekable file handle
pub trait FileReader: Read + Seek + Send {}

/// Wrapper around std::fs::File that implements FileReader
struct StdFileReader(std::fs::File);

impl Read for StdFileReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
}

impl Seek for StdFileReader {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.0.seek(pos)
    }
}

impl FileReader for StdFileReader {}

// ============================================================================
// File Search Types
// ============================================================================
//
// Used by `FileSystem::search_file` for project-wide search.  The search
// runs where the data lives: `StdFileSystem` scans locally, `RemoteFileSystem`
// delegates to the remote agent so only matches cross the network.
//
// For searching an already-open buffer's piece tree (in-editor Ctrl+F and
// dirty buffers in project search), see `TextBuffer::search_scan_*` in
// `buffer.rs` which uses the same `SearchMatch` type but reads from the
// in-memory piece tree rather than from disk.

/// Options for searching within a file via `FileSystem::search_file`.
#[derive(Clone, Debug)]
pub struct FileSearchOptions {
    /// If true, treat pattern as a literal string (regex-escape it).
    pub fixed_string: bool,
    /// If true, match case-sensitively.
    pub case_sensitive: bool,
    /// If true, match whole words only (wrap with `\b`).
    pub whole_word: bool,
    /// Maximum number of matches to return per batch.
    pub max_matches: usize,
}

/// Cursor for incremental `search_file` calls.  Each call searches one
/// chunk and advances the cursor; the caller loops until `done`.
#[derive(Clone, Debug)]
pub struct FileSearchCursor {
    /// Byte offset to resume searching from.
    pub offset: usize,
    /// 1-based line number at `offset` (tracks newlines across calls).
    pub running_line: usize,
    /// Set to true when the entire file has been searched.
    pub done: bool,
    /// Optional upper bound — stop searching at this byte offset instead
    /// of EOF.  Used by hybrid search to restrict `search_file` to a
    /// specific file range (e.g. an unloaded piece-tree region).
    pub end_offset: Option<usize>,
}

impl Default for FileSearchCursor {
    fn default() -> Self {
        Self {
            offset: 0,
            running_line: 1,
            done: false,
            end_offset: None,
        }
    }
}

impl FileSearchCursor {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a cursor bounded to a specific file range.
    pub fn for_range(offset: usize, end_offset: usize, running_line: usize) -> Self {
        Self {
            offset,
            running_line,
            done: false,
            end_offset: Some(end_offset),
        }
    }
}

/// A single search match with position and context.
///
/// Shared between `FileSystem::search_file` (project grep on disk) and
/// `TextBuffer::search_scan_*` (in-editor search on piece tree).
#[derive(Clone, Debug)]
pub struct SearchMatch {
    /// Byte offset of the match in the file/buffer.
    pub byte_offset: usize,
    /// Length of the match in bytes.
    pub length: usize,
    /// 1-based line number.
    pub line: usize,
    /// 1-based byte column within the line.
    pub column: usize,
    /// Content of the line containing the match (no trailing newline).
    pub context: String,
}

// ============================================================================
// FileSystem Trait
// ============================================================================

/// Unified trait for all filesystem operations
///
/// This trait provides both file content I/O and directory operations.
/// Implementations can be:
/// - `StdFileSystem`: Native filesystem using `std::fs`
/// - `VirtualFileSystem`: In-memory for WASM/browser
/// - Custom backends for remote agents, network filesystems, etc.
///
/// All methods are synchronous. For async UI operations, use `spawn_blocking`.
pub trait FileSystem: Send + Sync {
    // ========================================================================
    // File Content Operations
    // ========================================================================

    /// Read entire file into memory
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>>;

    /// Read a range of bytes from a file (for lazy loading large files)
    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>>;

    /// Count `\n` bytes in a file range without returning the data.
    ///
    /// Used by the line-feed scanner to count newlines in unloaded chunks.
    /// Remote filesystem implementations can override this to count on the
    /// server side, avoiding the transfer of chunk bytes over the network.
    ///
    /// The default implementation reads the range and counts locally.
    fn count_line_feeds_in_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<usize> {
        let data = self.read_range(path, offset, len)?;
        Ok(data.iter().filter(|&&b| b == b'\n').count())
    }

    /// Write data to file atomically (temp file + rename)
    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()>;

    /// Create a file for writing, returns a writer handle
    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>>;

    /// Open a file for reading, returns a reader handle
    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>>;

    /// Open a file for writing in-place (truncating, preserves ownership on Unix)
    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>>;

    /// Open a file for appending (creates if doesn't exist)
    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>>;

    /// Set file length (truncate or extend with zeros)
    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()>;

    /// Write a file using a patch recipe (optimized for remote filesystems).
    ///
    /// This allows saving edited files by specifying which parts to copy from
    /// the original and which parts are new content. For remote filesystems,
    /// this avoids transferring unchanged portions over the network.
    ///
    /// # Arguments
    /// * `src_path` - The original file to read from (for Copy operations)
    /// * `dst_path` - The destination file (often same as src_path)
    /// * `ops` - The sequence of operations to build the new file
    ///
    /// The default implementation flattens all operations into memory and
    /// calls `write_file`. Remote implementations can override this to send
    /// the recipe and let the remote host do the reconstruction.
    fn write_patched(&self, src_path: &Path, dst_path: &Path, ops: &[WriteOp]) -> io::Result<()> {
        // Default implementation: flatten to buffer and write
        let mut buffer = Vec::new();
        for op in ops {
            match op {
                WriteOp::Copy { offset, len } => {
                    let data = self.read_range(src_path, *offset, *len as usize)?;
                    buffer.extend_from_slice(&data);
                }
                WriteOp::Insert { data } => {
                    buffer.extend_from_slice(data);
                }
            }
        }
        self.write_file(dst_path, &buffer)
    }

    // ========================================================================
    // File Operations
    // ========================================================================

    /// Rename/move a file or directory atomically
    fn rename(&self, from: &Path, to: &Path) -> io::Result<()>;

    /// Copy a file (fallback when rename fails across filesystems)
    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64>;

    /// Remove a file
    fn remove_file(&self, path: &Path) -> io::Result<()>;

    /// Remove an empty directory
    fn remove_dir(&self, path: &Path) -> io::Result<()>;

    /// Recursively remove a directory and all its contents
    fn remove_dir_all(&self, path: &Path) -> io::Result<()> {
        for entry in self.read_dir(path)? {
            if entry.is_dir() {
                self.remove_dir_all(&entry.path)?;
            } else {
                self.remove_file(&entry.path)?;
            }
        }
        self.remove_dir(path)
    }

    /// Recursively copy a directory and all its contents to dst
    fn copy_dir_all(&self, src: &Path, dst: &Path) -> io::Result<()> {
        self.create_dir_all(dst)?;
        for entry in self.read_dir(src)? {
            let dst_child = dst.join(&entry.name);
            if entry.is_dir() {
                self.copy_dir_all(&entry.path, &dst_child)?;
            } else {
                self.copy(&entry.path, &dst_child)?;
            }
        }
        Ok(())
    }

    // ========================================================================
    // Metadata Operations
    // ========================================================================

    /// Get file/directory metadata
    fn metadata(&self, path: &Path) -> io::Result<FileMetadata>;

    /// Get symlink metadata (doesn't follow symlinks)
    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata>;

    /// Check if path exists
    fn exists(&self, path: &Path) -> bool {
        self.metadata(path).is_ok()
    }

    /// Check if path exists, returns metadata if it does
    fn metadata_if_exists(&self, path: &Path) -> Option<FileMetadata> {
        self.metadata(path).ok()
    }

    /// Check if path is a directory
    fn is_dir(&self, path: &Path) -> io::Result<bool>;

    /// Check if path is a file
    fn is_file(&self, path: &Path) -> io::Result<bool>;

    /// Check if the current user has write permission to the given path.
    ///
    /// On Unix, this considers file ownership, group membership (including
    /// supplementary groups), and the relevant permission bits. On other
    /// platforms it falls back to the standard readonly check.
    ///
    /// Returns `false` if the path doesn't exist or metadata can't be read.
    fn is_writable(&self, path: &Path) -> bool {
        self.metadata(path).map(|m| !m.is_readonly).unwrap_or(false)
    }

    /// Set file permissions
    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()>;

    // ========================================================================
    // Directory Operations
    // ========================================================================

    /// List entries in a directory (non-recursive)
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>>;

    /// Create a directory
    fn create_dir(&self, path: &Path) -> io::Result<()>;

    /// Create a directory and all parent directories
    fn create_dir_all(&self, path: &Path) -> io::Result<()>;

    // ========================================================================
    // Path Operations
    // ========================================================================

    /// Get canonical (absolute, normalized) path
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;

    // ========================================================================
    // Utility Methods
    // ========================================================================

    /// Get the current user's UID (Unix only, returns 0 on other platforms)
    fn current_uid(&self) -> u32;

    /// Check if the current user is the owner of the file
    fn is_owner(&self, path: &Path) -> bool {
        #[cfg(unix)]
        {
            if let Ok(meta) = self.metadata(path) {
                if let Some(uid) = meta.uid {
                    return uid == self.current_uid();
                }
            }
            true
        }
        #[cfg(not(unix))]
        {
            let _ = path;
            true
        }
    }

    /// Get a temporary file path for atomic writes
    fn temp_path_for(&self, path: &Path) -> PathBuf {
        path.with_extension("tmp")
    }

    /// Get a unique temporary file path (using timestamp and PID)
    fn unique_temp_path(&self, dest_path: &Path) -> PathBuf {
        let temp_dir = std::env::temp_dir();
        let file_name = dest_path
            .file_name()
            .unwrap_or_else(|| std::ffi::OsStr::new("fresh-save"));
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        temp_dir.join(format!(
            "{}-{}-{}.tmp",
            file_name.to_string_lossy(),
            std::process::id(),
            timestamp
        ))
    }

    // ========================================================================
    // Remote Connection Info
    // ========================================================================

    /// Get remote connection info if this is a remote filesystem
    ///
    /// Returns `Some("user@host")` for remote filesystems, `None` for local.
    /// Used to display remote connection status in the UI.
    fn remote_connection_info(&self) -> Option<&str> {
        None
    }

    /// Check if a remote filesystem is currently connected.
    ///
    /// Returns `true` for local filesystems (always "connected") and for
    /// remote filesystems with a healthy connection. Returns `false` when
    /// the remote connection has been lost (e.g., timeout, SSH disconnect).
    fn is_remote_connected(&self) -> bool {
        true
    }

    /// Get the home directory for this filesystem
    ///
    /// For local filesystems, returns the local home directory.
    /// For remote filesystems, returns the remote home directory.
    fn home_dir(&self) -> io::Result<PathBuf> {
        dirs::home_dir()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "home directory not found"))
    }

    // ========================================================================
    // Search Operations
    // ========================================================================

    /// Search a file on disk for a pattern, returning one batch of matches.
    ///
    /// Call repeatedly with the same cursor until `cursor.done` is true.
    /// Each call searches one chunk; the cursor tracks position and line
    /// numbers across calls.
    ///
    /// The search runs where the data lives: `StdFileSystem` reads and
    /// scans locally; `RemoteFileSystem` sends a stateless RPC to the
    /// remote agent.  Only matches cross the network.
    ///
    /// For searching an already-open buffer with unsaved edits, use
    /// `TextBuffer::search_scan_all` which reads from the piece tree.
    fn search_file(
        &self,
        path: &Path,
        pattern: &str,
        opts: &FileSearchOptions,
        cursor: &mut FileSearchCursor,
    ) -> io::Result<Vec<SearchMatch>>;

    /// Write file using sudo (for root-owned files).
    ///
    /// This writes the file with elevated privileges, preserving the specified
    /// permissions and ownership. Used when normal write fails due to permissions.
    ///
    /// - `path`: Destination file path
    /// - `data`: File contents to write
    /// - `mode`: File permissions (e.g., 0o644)
    /// - `uid`: Owner user ID
    /// - `gid`: Owner group ID
    fn sudo_write(&self, path: &Path, data: &[u8], mode: u32, uid: u32, gid: u32)
        -> io::Result<()>;

    // ========================================================================
    // Directory Walking
    // ========================================================================

    /// Recursively walk a directory tree, invoking `on_file` for each file.
    ///
    /// Skips hidden entries (dot-prefixed names) and directories whose
    /// basename appears in `skip_dirs`.  The walk stops early when:
    /// - `on_file` returns `false` (caller reached its limit), or
    /// - `cancel` is set to `true` (e.g. user closed the dialog).
    ///
    /// `on_file` receives `(absolute_path, path_relative_to_root)`.
    ///
    /// `skip_dirs` entries are **basenames** matched at every depth
    /// (e.g. `"node_modules"` skips every `node_modules` directory in the
    /// tree).
    ///
    /// // TODO: support .gitignore-style glob patterns in addition to
    /// // basename matching, so callers can express richer ignore rules
    /// // (e.g. `build/`, `*.o`, `vendor/**`).
    ///
    /// Each implementation must walk the filesystem it owns.  Local
    /// implementations should iterate `std::fs::read_dir` lazily (not
    /// collect into a Vec) so memory stays O(tree depth).  Remote
    /// implementations should walk server-side and stream results back
    /// via the channel, avoiding per-directory round-trips.
    fn walk_files(
        &self,
        root: &Path,
        skip_dirs: &[&str],
        cancel: &std::sync::atomic::AtomicBool,
        on_file: &mut dyn FnMut(&Path, &str) -> bool,
    ) -> io::Result<()>;
}

// ============================================================================
// FileSystemExt - Async Extension Trait
// ============================================================================

/// Async extension trait for FileSystem
///
/// This trait provides async versions of FileSystem methods using native
/// Rust async fn (no async_trait crate needed). Default implementations
/// simply call the sync methods, which works for local filesystem operations.
///
/// For truly async backends (network FS, remote agents), implementations
/// can override these methods with actual async implementations.
///
/// Note: This trait is NOT object-safe due to async fn. Use generics
/// (`impl FileSystem` or `F: FileSystem`) instead of `dyn FileSystem`
/// when async methods are needed.
///
/// # Example
///
/// ```ignore
/// async fn list_files<F: FileSystem>(fs: &F, path: &Path) -> io::Result<Vec<DirEntry>> {
///     fs.read_dir_async(path).await
/// }
/// ```
pub trait FileSystemExt: FileSystem {
    /// Async version of read_file
    fn read_file_async(
        &self,
        path: &Path,
    ) -> impl std::future::Future<Output = io::Result<Vec<u8>>> + Send {
        async { self.read_file(path) }
    }

    /// Async version of read_range
    fn read_range_async(
        &self,
        path: &Path,
        offset: u64,
        len: usize,
    ) -> impl std::future::Future<Output = io::Result<Vec<u8>>> + Send {
        async move { self.read_range(path, offset, len) }
    }

    /// Async version of count_line_feeds_in_range
    fn count_line_feeds_in_range_async(
        &self,
        path: &Path,
        offset: u64,
        len: usize,
    ) -> impl std::future::Future<Output = io::Result<usize>> + Send {
        async move { self.count_line_feeds_in_range(path, offset, len) }
    }

    /// Async version of write_file
    fn write_file_async(
        &self,
        path: &Path,
        data: &[u8],
    ) -> impl std::future::Future<Output = io::Result<()>> + Send {
        async { self.write_file(path, data) }
    }

    /// Async version of metadata
    fn metadata_async(
        &self,
        path: &Path,
    ) -> impl std::future::Future<Output = io::Result<FileMetadata>> + Send {
        async { self.metadata(path) }
    }

    /// Async version of exists
    fn exists_async(&self, path: &Path) -> impl std::future::Future<Output = bool> + Send {
        async { self.exists(path) }
    }

    /// Async version of is_dir
    fn is_dir_async(
        &self,
        path: &Path,
    ) -> impl std::future::Future<Output = io::Result<bool>> + Send {
        async { self.is_dir(path) }
    }

    /// Async version of is_file
    fn is_file_async(
        &self,
        path: &Path,
    ) -> impl std::future::Future<Output = io::Result<bool>> + Send {
        async { self.is_file(path) }
    }

    /// Async version of read_dir
    fn read_dir_async(
        &self,
        path: &Path,
    ) -> impl std::future::Future<Output = io::Result<Vec<DirEntry>>> + Send {
        async { self.read_dir(path) }
    }

    /// Async version of canonicalize
    fn canonicalize_async(
        &self,
        path: &Path,
    ) -> impl std::future::Future<Output = io::Result<PathBuf>> + Send {
        async { self.canonicalize(path) }
    }
}

/// Blanket implementation: all FileSystem types automatically get async methods
impl<T: FileSystem> FileSystemExt for T {}

// ============================================================================
// Default search_file implementation
// ============================================================================

/// Build a `regex::bytes::Regex` from a user-facing pattern and search options.
pub fn build_search_regex(
    pattern: &str,
    opts: &FileSearchOptions,
) -> io::Result<regex::bytes::Regex> {
    let re_pattern = if opts.fixed_string {
        regex::escape(pattern)
    } else {
        pattern.to_string()
    };
    let re_pattern = if opts.whole_word {
        format!(r"\b{}\b", re_pattern)
    } else {
        re_pattern
    };
    let re_pattern = if opts.case_sensitive {
        re_pattern
    } else {
        format!("(?i){}", re_pattern)
    };
    regex::bytes::Regex::new(&re_pattern)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))
}

/// Default implementation of `FileSystem::search_file` that works for any
/// filesystem backend.  Reads one chunk via `read_range`, scans with the
/// given regex, and returns matches with line/column/context.
pub fn default_search_file(
    fs: &dyn FileSystem,
    path: &Path,
    pattern: &str,
    opts: &FileSearchOptions,
    cursor: &mut FileSearchCursor,
) -> io::Result<Vec<SearchMatch>> {
    if cursor.done {
        return Ok(vec![]);
    }

    const CHUNK_SIZE: usize = 1_048_576; // 1 MB
    let overlap = pattern.len().max(256);

    let file_len = fs.metadata(path)?.size as usize;
    let effective_end = cursor.end_offset.unwrap_or(file_len).min(file_len);

    // Binary check on first call (only when starting from offset 0 with no range bound)
    if cursor.offset == 0 && cursor.end_offset.is_none() {
        if file_len == 0 {
            cursor.done = true;
            return Ok(vec![]);
        }
        let header_len = file_len.min(8192);
        let header = fs.read_range(path, 0, header_len)?;
        if header.contains(&0) {
            cursor.done = true;
            return Ok(vec![]);
        }
    }

    if cursor.offset >= effective_end {
        cursor.done = true;
        return Ok(vec![]);
    }

    let regex = build_search_regex(pattern, opts)?;

    // Read chunk with overlap from previous
    let read_start = cursor.offset.saturating_sub(overlap);
    let read_end = (read_start + CHUNK_SIZE).min(effective_end);
    let chunk = fs.read_range(path, read_start as u64, read_end - read_start)?;

    let overlap_len = cursor.offset - read_start;

    // Incremental line counting (same algorithm as search_scan_next_chunk)
    let newlines_in_overlap = chunk[..overlap_len].iter().filter(|&&b| b == b'\n').count();
    let mut line_at = cursor.running_line.saturating_sub(newlines_in_overlap);
    let mut counted_to = 0usize;
    let mut matches = Vec::new();

    for m in regex.find_iter(&chunk) {
        // Skip matches in overlap region (already reported in previous batch)
        if overlap_len > 0 && m.end() <= overlap_len {
            continue;
        }
        if matches.len() >= opts.max_matches {
            break;
        }

        // Count newlines from last position to this match
        line_at += chunk[counted_to..m.start()]
            .iter()
            .filter(|&&b| b == b'\n')
            .count();
        counted_to = m.start();

        // Find line boundaries for context
        let line_start = chunk[..m.start()]
            .iter()
            .rposition(|&b| b == b'\n')
            .map(|p| p + 1)
            .unwrap_or(0);
        let line_end = chunk[m.start()..]
            .iter()
            .position(|&b| b == b'\n')
            .map(|p| m.start() + p)
            .unwrap_or(chunk.len());

        let column = m.start() - line_start + 1;
        let context = String::from_utf8_lossy(&chunk[line_start..line_end]).into_owned();

        matches.push(SearchMatch {
            byte_offset: read_start + m.start(),
            length: m.end() - m.start(),
            line: line_at,
            column,
            context,
        });
    }

    // Advance cursor
    let new_data = &chunk[overlap_len..];
    cursor.running_line += new_data.iter().filter(|&&b| b == b'\n').count();
    cursor.offset = read_end;
    if read_end >= effective_end {
        cursor.done = true;
    }

    Ok(matches)
}

// ============================================================================
// StdFileSystem Implementation
// ============================================================================

/// Standard filesystem implementation using `std::fs`
///
/// This is the default implementation for native builds.
#[derive(Debug, Clone, Copy, Default)]
pub struct StdFileSystem;

impl StdFileSystem {
    /// Check if a file is hidden (platform-specific)
    fn is_hidden(path: &Path) -> bool {
        path.file_name()
            .and_then(|n| n.to_str())
            .is_some_and(|n| n.starts_with('.'))
    }

    /// Get the current user's effective UID and all group IDs (primary + supplementary).
    #[cfg(unix)]
    pub fn current_user_groups() -> (u32, Vec<u32>) {
        // SAFETY: these libc calls are always safe and have no failure modes
        let euid = unsafe { libc::geteuid() };
        let egid = unsafe { libc::getegid() };
        let mut groups = vec![egid];

        // Get supplementary groups
        let ngroups = unsafe { libc::getgroups(0, std::ptr::null_mut()) };
        if ngroups > 0 {
            let mut sup_groups = vec![0 as libc::gid_t; ngroups as usize];
            let n = unsafe { libc::getgroups(ngroups, sup_groups.as_mut_ptr()) };
            if n > 0 {
                sup_groups.truncate(n as usize);
                for g in sup_groups {
                    if g != egid {
                        groups.push(g);
                    }
                }
            }
        }

        (euid, groups)
    }

    /// Build FileMetadata from std::fs::Metadata
    fn build_metadata(path: &Path, meta: &std::fs::Metadata) -> FileMetadata {
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            let file_uid = meta.uid();
            let file_gid = meta.gid();
            let permissions = FilePermissions::from_std(meta.permissions());
            let (euid, user_groups) = Self::current_user_groups();
            let is_readonly =
                permissions.is_readonly_for_user(euid, file_uid, file_gid, &user_groups);
            FileMetadata {
                size: meta.len(),
                modified: meta.modified().ok(),
                permissions: Some(permissions),
                is_hidden: Self::is_hidden(path),
                is_readonly,
                uid: Some(file_uid),
                gid: Some(file_gid),
            }
        }
        #[cfg(not(unix))]
        {
            FileMetadata {
                size: meta.len(),
                modified: meta.modified().ok(),
                permissions: Some(FilePermissions::from_std(meta.permissions())),
                is_hidden: Self::is_hidden(path),
                is_readonly: meta.permissions().readonly(),
            }
        }
    }
}

impl FileSystem for StdFileSystem {
    // File Content Operations
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        let data = std::fs::read(path)?;
        crate::services::counters::global().inc_disk_bytes_read(data.len() as u64);
        Ok(data)
    }

    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        let mut file = std::fs::File::open(path)?;
        file.seek(io::SeekFrom::Start(offset))?;
        let mut buffer = vec![0u8; len];
        file.read_exact(&mut buffer)?;
        crate::services::counters::global().inc_disk_bytes_read(len as u64);
        Ok(buffer)
    }

    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        let original_metadata = self.metadata_if_exists(path);
        let temp_path = self.temp_path_for(path);
        {
            let mut file = self.create_file(&temp_path)?;
            file.write_all(data)?;
            file.sync_all()?;
        }
        if let Some(ref meta) = original_metadata {
            if let Some(ref perms) = meta.permissions {
                // Best-effort permission restore; rename will proceed regardless
                #[allow(clippy::let_underscore_must_use)]
                let _ = self.set_permissions(&temp_path, perms);
            }
        }
        self.rename(&temp_path, path)?;
        Ok(())
    }

    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        let file = std::fs::File::create(path)?;
        Ok(Box::new(StdFileWriter(file)))
    }

    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        let file = std::fs::File::open(path)?;
        Ok(Box::new(StdFileReader(file)))
    }

    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        let file = std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .open(path)?;
        Ok(Box::new(StdFileWriter(file)))
    }

    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        let file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)?;
        Ok(Box::new(StdFileWriter(file)))
    }

    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
        let file = std::fs::OpenOptions::new().write(true).open(path)?;
        file.set_len(len)
    }

    // File Operations
    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        std::fs::rename(from, to)
    }

    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        std::fs::copy(from, to)
    }

    fn remove_file(&self, path: &Path) -> io::Result<()> {
        std::fs::remove_file(path)
    }

    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        std::fs::remove_dir(path)
    }

    // Metadata Operations
    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        let meta = std::fs::metadata(path)?;
        Ok(Self::build_metadata(path, &meta))
    }

    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        let meta = std::fs::symlink_metadata(path)?;
        Ok(Self::build_metadata(path, &meta))
    }

    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        Ok(std::fs::metadata(path)?.is_dir())
    }

    fn is_file(&self, path: &Path) -> io::Result<bool> {
        Ok(std::fs::metadata(path)?.is_file())
    }

    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
        std::fs::set_permissions(path, permissions.to_std())
    }

    // Directory Operations
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let mut entries = Vec::new();
        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            let name = entry.file_name().to_string_lossy().into_owned();
            let file_type = entry.file_type()?;

            let entry_type = if file_type.is_dir() {
                EntryType::Directory
            } else if file_type.is_symlink() {
                EntryType::Symlink
            } else {
                EntryType::File
            };

            let mut dir_entry = DirEntry::new(path.clone(), name, entry_type);

            // For symlinks, check if target is a directory
            if file_type.is_symlink() {
                dir_entry.symlink_target_is_dir = std::fs::metadata(&path)
                    .map(|m| m.is_dir())
                    .unwrap_or(false);
            }

            entries.push(dir_entry);
        }
        Ok(entries)
    }

    fn create_dir(&self, path: &Path) -> io::Result<()> {
        std::fs::create_dir(path)
    }

    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        std::fs::create_dir_all(path)
    }

    // Path Operations
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        std::fs::canonicalize(path)
    }

    // Utility
    fn current_uid(&self) -> u32 {
        #[cfg(all(unix, feature = "runtime"))]
        {
            // SAFETY: getuid() is a simple syscall with no arguments
            unsafe { libc::getuid() }
        }
        #[cfg(not(all(unix, feature = "runtime")))]
        {
            0
        }
    }

    fn sudo_write(
        &self,
        path: &Path,
        data: &[u8],
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> io::Result<()> {
        use crate::services::process_hidden::HideWindow;
        use std::process::{Command, Stdio};

        // Write data via sudo tee
        let mut child = Command::new("sudo")
            .args(["tee", &path.to_string_lossy()])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .hide_window()
            .spawn()
            .map_err(|e| io::Error::other(format!("failed to spawn sudo: {}", e)))?;

        if let Some(mut stdin) = child.stdin.take() {
            use std::io::Write;
            stdin.write_all(data)?;
        }

        let output = child.wait_with_output()?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                format!("sudo tee failed: {}", stderr.trim()),
            ));
        }

        // Set permissions via sudo chmod
        let status = Command::new("sudo")
            .args(["chmod", &format!("{:o}", mode), &path.to_string_lossy()])
            .hide_window()
            .status()?;
        if !status.success() {
            return Err(io::Error::other("sudo chmod failed"));
        }

        // Set ownership via sudo chown
        let status = Command::new("sudo")
            .args([
                "chown",
                &format!("{}:{}", uid, gid),
                &path.to_string_lossy(),
            ])
            .hide_window()
            .status()?;
        if !status.success() {
            return Err(io::Error::other("sudo chown failed"));
        }

        Ok(())
    }

    fn search_file(
        &self,
        path: &Path,
        pattern: &str,
        opts: &FileSearchOptions,
        cursor: &mut FileSearchCursor,
    ) -> io::Result<Vec<SearchMatch>> {
        default_search_file(self, path, pattern, opts, cursor)
    }

    fn walk_files(
        &self,
        root: &Path,
        skip_dirs: &[&str],
        cancel: &std::sync::atomic::AtomicBool,
        on_file: &mut dyn FnMut(&Path, &str) -> bool,
    ) -> io::Result<()> {
        let mut stack = vec![root.to_path_buf()];
        while let Some(dir) = stack.pop() {
            if cancel.load(std::sync::atomic::Ordering::Relaxed) {
                return Ok(());
            }

            // Use std::fs::read_dir iterator directly — NOT self.read_dir()
            // which collects into a Vec.  This keeps memory O(1) per directory
            // even for directories with millions of entries.
            let iter = match std::fs::read_dir(&dir) {
                Ok(it) => it,
                Err(_) => continue,
            };

            for entry in iter {
                if cancel.load(std::sync::atomic::Ordering::Relaxed) {
                    return Ok(());
                }
                let entry = match entry {
                    Ok(e) => e,
                    Err(_) => continue,
                };
                let name = entry.file_name();
                let name_str = name.to_string_lossy();

                // Skip hidden entries
                if name_str.starts_with('.') {
                    continue;
                }

                let ft = match entry.file_type() {
                    Ok(ft) => ft,
                    Err(_) => continue,
                };
                let path = entry.path();

                if ft.is_file() {
                    if let Ok(rel) = path.strip_prefix(root) {
                        let rel_str = rel.to_string_lossy().replace('\\', "/");
                        if !on_file(&path, &rel_str) {
                            return Ok(());
                        }
                    }
                } else if ft.is_dir() && !skip_dirs.contains(&name_str.as_ref()) {
                    stack.push(path);
                }
            }
        }
        Ok(())
    }
}

// ============================================================================
// NoopFileSystem Implementation
// ============================================================================

/// No-op filesystem that returns errors for all operations
///
/// Used as a placeholder or in WASM builds where a VirtualFileSystem
/// should be used instead.
#[derive(Debug, Clone, Copy, Default)]
pub struct NoopFileSystem;

impl NoopFileSystem {
    fn unsupported<T>() -> io::Result<T> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "Filesystem not available",
        ))
    }
}

impl FileSystem for NoopFileSystem {
    fn read_file(&self, _path: &Path) -> io::Result<Vec<u8>> {
        Self::unsupported()
    }

    fn read_range(&self, _path: &Path, _offset: u64, _len: usize) -> io::Result<Vec<u8>> {
        Self::unsupported()
    }

    fn write_file(&self, _path: &Path, _data: &[u8]) -> io::Result<()> {
        Self::unsupported()
    }

    fn create_file(&self, _path: &Path) -> io::Result<Box<dyn FileWriter>> {
        Self::unsupported()
    }

    fn open_file(&self, _path: &Path) -> io::Result<Box<dyn FileReader>> {
        Self::unsupported()
    }

    fn open_file_for_write(&self, _path: &Path) -> io::Result<Box<dyn FileWriter>> {
        Self::unsupported()
    }

    fn open_file_for_append(&self, _path: &Path) -> io::Result<Box<dyn FileWriter>> {
        Self::unsupported()
    }

    fn set_file_length(&self, _path: &Path, _len: u64) -> io::Result<()> {
        Self::unsupported()
    }

    fn rename(&self, _from: &Path, _to: &Path) -> io::Result<()> {
        Self::unsupported()
    }

    fn copy(&self, _from: &Path, _to: &Path) -> io::Result<u64> {
        Self::unsupported()
    }

    fn remove_file(&self, _path: &Path) -> io::Result<()> {
        Self::unsupported()
    }

    fn remove_dir(&self, _path: &Path) -> io::Result<()> {
        Self::unsupported()
    }

    fn metadata(&self, _path: &Path) -> io::Result<FileMetadata> {
        Self::unsupported()
    }

    fn symlink_metadata(&self, _path: &Path) -> io::Result<FileMetadata> {
        Self::unsupported()
    }

    fn is_dir(&self, _path: &Path) -> io::Result<bool> {
        Self::unsupported()
    }

    fn is_file(&self, _path: &Path) -> io::Result<bool> {
        Self::unsupported()
    }

    fn set_permissions(&self, _path: &Path, _permissions: &FilePermissions) -> io::Result<()> {
        Self::unsupported()
    }

    fn read_dir(&self, _path: &Path) -> io::Result<Vec<DirEntry>> {
        Self::unsupported()
    }

    fn create_dir(&self, _path: &Path) -> io::Result<()> {
        Self::unsupported()
    }

    fn create_dir_all(&self, _path: &Path) -> io::Result<()> {
        Self::unsupported()
    }

    fn canonicalize(&self, _path: &Path) -> io::Result<PathBuf> {
        Self::unsupported()
    }

    fn current_uid(&self) -> u32 {
        0
    }

    fn search_file(
        &self,
        _path: &Path,
        _pattern: &str,
        _opts: &FileSearchOptions,
        _cursor: &mut FileSearchCursor,
    ) -> io::Result<Vec<SearchMatch>> {
        Self::unsupported()
    }

    fn sudo_write(
        &self,
        _path: &Path,
        _data: &[u8],
        _mode: u32,
        _uid: u32,
        _gid: u32,
    ) -> io::Result<()> {
        Self::unsupported()
    }

    fn walk_files(
        &self,
        _root: &Path,
        _skip_dirs: &[&str],
        _cancel: &std::sync::atomic::AtomicBool,
        _on_file: &mut dyn FnMut(&Path, &str) -> bool,
    ) -> io::Result<()> {
        Self::unsupported()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_std_filesystem_read_write() {
        let fs = StdFileSystem;
        let mut temp = NamedTempFile::new().unwrap();
        let path = temp.path().to_path_buf();

        std::io::Write::write_all(&mut temp, b"Hello, World!").unwrap();
        std::io::Write::flush(&mut temp).unwrap();

        let content = fs.read_file(&path).unwrap();
        assert_eq!(content, b"Hello, World!");

        let range = fs.read_range(&path, 7, 5).unwrap();
        assert_eq!(range, b"World");

        let meta = fs.metadata(&path).unwrap();
        assert_eq!(meta.size, 13);
    }

    #[test]
    fn test_noop_filesystem() {
        let fs = NoopFileSystem;
        let path = Path::new("/some/path");

        assert!(fs.read_file(path).is_err());
        assert!(fs.read_range(path, 0, 10).is_err());
        assert!(fs.write_file(path, b"data").is_err());
        assert!(fs.metadata(path).is_err());
        assert!(fs.read_dir(path).is_err());
    }

    #[test]
    fn test_create_and_write_file() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("test.txt");

        {
            let mut writer = fs.create_file(&path).unwrap();
            writer.write_all(b"test content").unwrap();
            writer.sync_all().unwrap();
        }

        let content = fs.read_file(&path).unwrap();
        assert_eq!(content, b"test content");
    }

    #[test]
    fn test_read_dir() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();

        // Create some files and directories
        fs.create_dir(&temp_dir.path().join("subdir")).unwrap();
        fs.write_file(&temp_dir.path().join("file1.txt"), b"content1")
            .unwrap();
        fs.write_file(&temp_dir.path().join("file2.txt"), b"content2")
            .unwrap();

        let entries = fs.read_dir(temp_dir.path()).unwrap();
        assert_eq!(entries.len(), 3);

        let names: Vec<_> = entries.iter().map(|e| e.name.as_str()).collect();
        assert!(names.contains(&"subdir"));
        assert!(names.contains(&"file1.txt"));
        assert!(names.contains(&"file2.txt"));
    }

    #[test]
    fn test_dir_entry_types() {
        let file = DirEntry::new(PathBuf::from("/file"), "file".to_string(), EntryType::File);
        assert!(file.is_file());
        assert!(!file.is_dir());

        let dir = DirEntry::new(
            PathBuf::from("/dir"),
            "dir".to_string(),
            EntryType::Directory,
        );
        assert!(dir.is_dir());
        assert!(!dir.is_file());

        let link_to_dir = DirEntry::new_symlink(PathBuf::from("/link"), "link".to_string(), true);
        assert!(link_to_dir.is_symlink());
        assert!(link_to_dir.is_dir());
    }

    #[test]
    fn test_metadata_builder() {
        let meta = FileMetadata::default()
            .with_hidden(true)
            .with_readonly(true);
        assert!(meta.is_hidden);
        assert!(meta.is_readonly);
    }

    #[test]
    fn test_atomic_write() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("atomic_test.txt");

        fs.write_file(&path, b"initial").unwrap();
        assert_eq!(fs.read_file(&path).unwrap(), b"initial");

        fs.write_file(&path, b"updated").unwrap();
        assert_eq!(fs.read_file(&path).unwrap(), b"updated");
    }

    #[test]
    fn test_write_patched_default_impl() {
        // Test that the default write_patched implementation works correctly
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let src_path = temp_dir.path().join("source.txt");
        let dst_path = temp_dir.path().join("dest.txt");

        // Create source file with known content
        fs.write_file(&src_path, b"AAABBBCCC").unwrap();

        // Apply patch: copy first 3 bytes, insert "XXX", copy last 3 bytes
        let ops = vec![
            WriteOp::Copy { offset: 0, len: 3 }, // "AAA"
            WriteOp::Insert { data: b"XXX" },    // "XXX"
            WriteOp::Copy { offset: 6, len: 3 }, // "CCC"
        ];

        fs.write_patched(&src_path, &dst_path, &ops).unwrap();

        let result = fs.read_file(&dst_path).unwrap();
        assert_eq!(result, b"AAAXXXCCC");
    }

    #[test]
    fn test_write_patched_same_file() {
        // Test patching a file in-place (src == dst)
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("file.txt");

        // Create file
        fs.write_file(&path, b"Hello World").unwrap();

        // Replace "World" with "Rust"
        let ops = vec![
            WriteOp::Copy { offset: 0, len: 6 }, // "Hello "
            WriteOp::Insert { data: b"Rust" },   // "Rust"
        ];

        fs.write_patched(&path, &path, &ops).unwrap();

        let result = fs.read_file(&path).unwrap();
        assert_eq!(result, b"Hello Rust");
    }

    #[test]
    fn test_write_patched_insert_only() {
        // Test a patch with only inserts (new file)
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let src_path = temp_dir.path().join("empty.txt");
        let dst_path = temp_dir.path().join("new.txt");

        // Create empty source (won't be read from)
        fs.write_file(&src_path, b"").unwrap();

        let ops = vec![WriteOp::Insert {
            data: b"All new content",
        }];

        fs.write_patched(&src_path, &dst_path, &ops).unwrap();

        let result = fs.read_file(&dst_path).unwrap();
        assert_eq!(result, b"All new content");
    }

    // ====================================================================
    // search_file tests
    // ====================================================================

    fn make_search_opts(pattern_is_fixed: bool) -> FileSearchOptions {
        FileSearchOptions {
            fixed_string: pattern_is_fixed,
            case_sensitive: true,
            whole_word: false,
            max_matches: 100,
        }
    }

    #[test]
    fn test_search_file_basic() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("test.txt");
        fs.write_file(&path, b"hello world\nfoo bar\nhello again\n")
            .unwrap();

        let opts = make_search_opts(true);
        let mut cursor = FileSearchCursor::new();
        let matches = fs.search_file(&path, "hello", &opts, &mut cursor).unwrap();

        assert!(cursor.done);
        assert_eq!(matches.len(), 2);

        assert_eq!(matches[0].line, 1);
        assert_eq!(matches[0].column, 1);
        assert_eq!(matches[0].context, "hello world");

        assert_eq!(matches[1].line, 3);
        assert_eq!(matches[1].column, 1);
        assert_eq!(matches[1].context, "hello again");
    }

    #[test]
    fn test_search_file_no_matches() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("test.txt");
        fs.write_file(&path, b"hello world\n").unwrap();

        let opts = make_search_opts(true);
        let mut cursor = FileSearchCursor::new();
        let matches = fs
            .search_file(&path, "NOTFOUND", &opts, &mut cursor)
            .unwrap();

        assert!(cursor.done);
        assert!(matches.is_empty());
    }

    #[test]
    fn test_search_file_case_insensitive() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("test.txt");
        fs.write_file(&path, b"Hello HELLO hello\n").unwrap();

        let opts = FileSearchOptions {
            fixed_string: true,
            case_sensitive: false,
            whole_word: false,
            max_matches: 100,
        };
        let mut cursor = FileSearchCursor::new();
        let matches = fs.search_file(&path, "hello", &opts, &mut cursor).unwrap();

        assert_eq!(matches.len(), 3);
    }

    #[test]
    fn test_search_file_whole_word() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("test.txt");
        fs.write_file(&path, b"cat concatenate catalog\n").unwrap();

        let opts = FileSearchOptions {
            fixed_string: true,
            case_sensitive: true,
            whole_word: true,
            max_matches: 100,
        };
        let mut cursor = FileSearchCursor::new();
        let matches = fs.search_file(&path, "cat", &opts, &mut cursor).unwrap();

        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].column, 1);
    }

    #[test]
    fn test_search_file_regex() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("test.txt");
        fs.write_file(&path, b"foo123 bar456 baz\n").unwrap();

        let opts = FileSearchOptions {
            fixed_string: false,
            case_sensitive: true,
            whole_word: false,
            max_matches: 100,
        };
        let mut cursor = FileSearchCursor::new();
        let matches = fs
            .search_file(&path, r"[a-z]+\d+", &opts, &mut cursor)
            .unwrap();

        assert_eq!(matches.len(), 2);
        assert_eq!(matches[0].context, "foo123 bar456 baz");
    }

    #[test]
    fn test_search_file_binary_skipped() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("binary.dat");
        let mut data = b"hello world\n".to_vec();
        data.push(0); // null byte makes it binary
        data.extend_from_slice(b"hello again\n");
        fs.write_file(&path, &data).unwrap();

        let opts = make_search_opts(true);
        let mut cursor = FileSearchCursor::new();
        let matches = fs.search_file(&path, "hello", &opts, &mut cursor).unwrap();

        assert!(cursor.done);
        assert!(matches.is_empty());
    }

    #[test]
    fn test_search_file_empty_file() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("empty.txt");
        fs.write_file(&path, b"").unwrap();

        let opts = make_search_opts(true);
        let mut cursor = FileSearchCursor::new();
        let matches = fs.search_file(&path, "hello", &opts, &mut cursor).unwrap();

        assert!(cursor.done);
        assert!(matches.is_empty());
    }

    #[test]
    fn test_search_file_max_matches() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("test.txt");
        fs.write_file(&path, b"aa bb aa cc aa dd aa\n").unwrap();

        let opts = FileSearchOptions {
            fixed_string: true,
            case_sensitive: true,
            whole_word: false,
            max_matches: 2,
        };
        let mut cursor = FileSearchCursor::new();
        let matches = fs.search_file(&path, "aa", &opts, &mut cursor).unwrap();

        assert_eq!(matches.len(), 2);
    }

    #[test]
    fn test_search_file_cursor_multi_chunk() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("large.txt");

        // Create a file larger than 1MB chunk size to test cursor continuation
        let mut content = Vec::new();
        for i in 0..100_000 {
            content.extend_from_slice(format!("line {} content here\n", i).as_bytes());
        }
        fs.write_file(&path, &content).unwrap();

        let opts = FileSearchOptions {
            fixed_string: true,
            case_sensitive: true,
            whole_word: false,
            max_matches: 1000,
        };
        let mut cursor = FileSearchCursor::new();
        let mut all_matches = Vec::new();

        while !cursor.done {
            let batch = fs
                .search_file(&path, "line 5000", &opts, &mut cursor)
                .unwrap();
            all_matches.extend(batch);
        }

        // "line 5000" matches: "line 5000 ", "line 50000 "..  "line 50009 "
        // = 11 matches (5000, 50000-50009)
        assert_eq!(all_matches.len(), 11);

        // Verify line numbers are correct
        let first = &all_matches[0];
        assert_eq!(first.line, 5001); // 0-indexed lines, 1-based line numbers
        assert_eq!(first.column, 1);
        assert!(first.context.starts_with("line 5000"));
    }

    #[test]
    fn test_search_file_cursor_no_duplicates() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("large.txt");

        // Create file with matches near chunk boundaries
        let mut content = Vec::new();
        for i in 0..100_000 {
            content.extend_from_slice(format!("MARKER_{:06}\n", i).as_bytes());
        }
        fs.write_file(&path, &content).unwrap();

        let opts = FileSearchOptions {
            fixed_string: true,
            case_sensitive: true,
            whole_word: false,
            max_matches: 200_000,
        };
        let mut cursor = FileSearchCursor::new();
        let mut all_matches = Vec::new();
        let mut batches = 0;

        while !cursor.done {
            let batch = fs
                .search_file(&path, "MARKER_", &opts, &mut cursor)
                .unwrap();
            all_matches.extend(batch);
            batches += 1;
        }

        // Must have multiple batches (file > 1MB)
        assert!(batches > 1, "Expected multiple batches, got {}", batches);
        // Exactly one match per line, no duplicates
        assert_eq!(all_matches.len(), 100_000);
        // Check no duplicate byte offsets
        let mut offsets: Vec<usize> = all_matches.iter().map(|m| m.byte_offset).collect();
        offsets.sort();
        offsets.dedup();
        assert_eq!(offsets.len(), 100_000);
    }

    #[test]
    fn test_search_file_line_numbers_across_chunks() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("large.txt");

        // Create file where we know exact line numbers
        let mut content = Vec::new();
        let total_lines = 100_000;
        for i in 0..total_lines {
            if i == 99_999 {
                content.extend_from_slice(b"FINDME at the end\n");
            } else {
                content.extend_from_slice(format!("padding line {}\n", i).as_bytes());
            }
        }
        fs.write_file(&path, &content).unwrap();

        let opts = make_search_opts(true);
        let mut cursor = FileSearchCursor::new();
        let mut all_matches = Vec::new();

        while !cursor.done {
            let batch = fs.search_file(&path, "FINDME", &opts, &mut cursor).unwrap();
            all_matches.extend(batch);
        }

        assert_eq!(all_matches.len(), 1);
        assert_eq!(all_matches[0].line, total_lines); // last line
        assert_eq!(all_matches[0].context, "FINDME at the end");
    }

    #[test]
    fn test_search_file_end_offset_bounds_search() {
        let fs = StdFileSystem;
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("bounded.txt");

        // "AAA\nBBB\nCCC\nDDD\n" — each line is 4 bytes
        fs.write_file(&path, b"AAA\nBBB\nCCC\nDDD\n").unwrap();

        // Search only the first 8 bytes ("AAA\nBBB\n") — should find AAA and BBB
        let opts = make_search_opts(true);
        let mut cursor = FileSearchCursor::for_range(0, 8, 1);
        let mut matches = Vec::new();
        while !cursor.done {
            matches.extend(fs.search_file(&path, "AAA", &opts, &mut cursor).unwrap());
        }
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].context, "AAA");
        assert_eq!(matches[0].line, 1);

        // CCC is at byte 8, outside the first 8 bytes
        let mut cursor = FileSearchCursor::for_range(0, 8, 1);
        let ccc = fs.search_file(&path, "CCC", &opts, &mut cursor).unwrap();
        assert!(ccc.is_empty(), "CCC should not be found in first 8 bytes");

        // Search bytes 8..16 ("CCC\nDDD\n") — should find CCC
        let mut cursor = FileSearchCursor::for_range(8, 16, 3);
        let mut matches = Vec::new();
        while !cursor.done {
            matches.extend(fs.search_file(&path, "CCC", &opts, &mut cursor).unwrap());
        }
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].context, "CCC");
        assert_eq!(matches[0].line, 3);
    }

    // ====================================================================
    // walk_files tests
    // ====================================================================

    /// Helper: create a directory tree for walk_files tests.
    /// Returns the tempdir (must be kept alive for the duration of the test).
    fn make_walk_tree() -> tempfile::TempDir {
        let fs = StdFileSystem;
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        // root/
        //   a.txt
        //   b.txt
        //   sub/
        //     c.txt
        //     deep/
        //       d.txt
        //   .hidden_dir/
        //     secret.txt
        //   .hidden_file
        //   node_modules/
        //     pkg.json
        //   target/
        //     debug.o
        fs.write_file(&root.join("a.txt"), b"a").unwrap();
        fs.write_file(&root.join("b.txt"), b"b").unwrap();
        fs.create_dir_all(&root.join("sub/deep")).unwrap();
        fs.write_file(&root.join("sub/c.txt"), b"c").unwrap();
        fs.write_file(&root.join("sub/deep/d.txt"), b"d").unwrap();
        fs.create_dir_all(&root.join(".hidden_dir")).unwrap();
        fs.write_file(&root.join(".hidden_dir/secret.txt"), b"s")
            .unwrap();
        fs.write_file(&root.join(".hidden_file"), b"h").unwrap();
        fs.create_dir_all(&root.join("node_modules")).unwrap();
        fs.write_file(&root.join("node_modules/pkg.json"), b"{}")
            .unwrap();
        fs.create_dir_all(&root.join("target")).unwrap();
        fs.write_file(&root.join("target/debug.o"), b"elf").unwrap();

        tmp
    }

    #[test]
    fn test_walk_files_std_basic() {
        let tmp = make_walk_tree();
        let fs = StdFileSystem;
        let cancel = std::sync::atomic::AtomicBool::new(false);
        let mut found: Vec<String> = Vec::new();

        fs.walk_files(
            tmp.path(),
            &["node_modules", "target"],
            &cancel,
            &mut |_path, rel| {
                found.push(rel.to_string());
                true
            },
        )
        .unwrap();

        found.sort();
        assert_eq!(found, vec!["a.txt", "b.txt", "sub/c.txt", "sub/deep/d.txt"]);
    }

    #[test]
    fn test_walk_files_std_skips_hidden() {
        let tmp = make_walk_tree();
        let fs = StdFileSystem;
        let cancel = std::sync::atomic::AtomicBool::new(false);
        let mut found: Vec<String> = Vec::new();

        fs.walk_files(tmp.path(), &[], &cancel, &mut |_path, rel| {
            found.push(rel.to_string());
            true
        })
        .unwrap();

        // Hidden files/dirs should be excluded, but node_modules and target
        // are NOT skipped (empty skip list)
        assert!(!found.iter().any(|f| f.contains(".hidden")));
        assert!(found.iter().any(|f| f.contains("node_modules")));
        assert!(found.iter().any(|f| f.contains("target")));
    }

    #[test]
    fn test_walk_files_std_skip_dirs() {
        let tmp = make_walk_tree();
        let fs = StdFileSystem;
        let cancel = std::sync::atomic::AtomicBool::new(false);
        let mut found: Vec<String> = Vec::new();

        fs.walk_files(
            tmp.path(),
            &["node_modules", "target", "deep"],
            &cancel,
            &mut |_path, rel| {
                found.push(rel.to_string());
                true
            },
        )
        .unwrap();

        found.sort();
        // "deep" dir is also skipped, so d.txt should not appear
        assert_eq!(found, vec!["a.txt", "b.txt", "sub/c.txt"]);
    }

    #[test]
    fn test_walk_files_std_cancel() {
        let tmp = make_walk_tree();
        let fs = StdFileSystem;
        let cancel = std::sync::atomic::AtomicBool::new(false);
        let mut found: Vec<String> = Vec::new();

        fs.walk_files(
            tmp.path(),
            &["node_modules", "target"],
            &cancel,
            &mut |_path, rel| {
                found.push(rel.to_string());
                // Cancel after finding the first file
                cancel.store(true, std::sync::atomic::Ordering::Relaxed);
                true
            },
        )
        .unwrap();

        assert_eq!(found.len(), 1, "Should stop after cancel is set");
    }

    #[test]
    fn test_walk_files_std_on_file_returns_false() {
        let tmp = make_walk_tree();
        let fs = StdFileSystem;
        let cancel = std::sync::atomic::AtomicBool::new(false);
        let mut count = 0usize;

        fs.walk_files(
            tmp.path(),
            &["node_modules", "target"],
            &cancel,
            &mut |_path, _rel| {
                count += 1;
                count < 2 // stop after 2 files
            },
        )
        .unwrap();

        assert_eq!(count, 2, "Should stop when on_file returns false");
    }

    #[test]
    fn test_walk_files_std_empty_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let fs = StdFileSystem;
        let cancel = std::sync::atomic::AtomicBool::new(false);
        let mut found: Vec<String> = Vec::new();

        fs.walk_files(tmp.path(), &[], &cancel, &mut |_path, rel| {
            found.push(rel.to_string());
            true
        })
        .unwrap();

        assert!(found.is_empty());
    }

    #[test]
    fn test_walk_files_std_nonexistent_root() {
        let fs = StdFileSystem;
        let cancel = std::sync::atomic::AtomicBool::new(false);
        let mut found: Vec<String> = Vec::new();

        // Non-existent root should not panic, just return Ok with no files
        let result = fs.walk_files(
            Path::new("/nonexistent/path/that/does/not/exist"),
            &[],
            &cancel,
            &mut |_path, rel| {
                found.push(rel.to_string());
                true
            },
        );

        assert!(result.is_ok());
        assert!(found.is_empty());
    }

    #[test]
    fn test_walk_files_std_relative_paths_use_forward_slashes() {
        let tmp = make_walk_tree();
        let fs = StdFileSystem;
        let cancel = std::sync::atomic::AtomicBool::new(false);
        let mut found: Vec<String> = Vec::new();

        fs.walk_files(
            tmp.path(),
            &["node_modules", "target"],
            &cancel,
            &mut |_path, rel| {
                found.push(rel.to_string());
                true
            },
        )
        .unwrap();

        // All paths should use forward slashes (even on Windows)
        for path in &found {
            assert!(!path.contains('\\'), "Path should use / not \\: {}", path);
        }
    }

    #[test]
    fn test_walk_files_noop_returns_error() {
        let fs = NoopFileSystem;
        let cancel = std::sync::atomic::AtomicBool::new(false);

        let result = fs.walk_files(Path::new("/noop/path"), &[], &cancel, &mut |_path, _rel| {
            true
        });

        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), std::io::ErrorKind::Unsupported);
    }
}
