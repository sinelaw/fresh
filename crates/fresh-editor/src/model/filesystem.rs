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

    /// Check if readonly
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
}

// ============================================================================
// File Handle Traits
// ============================================================================

/// A writable file handle
pub trait FileWriter: Write + Send {
    /// Sync all data to disk
    fn sync_all(&self) -> io::Result<()>;
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

    /// Build FileMetadata from std::fs::Metadata
    fn build_metadata(path: &Path, meta: &std::fs::Metadata) -> FileMetadata {
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            FileMetadata {
                size: meta.len(),
                modified: meta.modified().ok(),
                permissions: Some(FilePermissions::from_std(meta.permissions())),
                is_hidden: Self::is_hidden(path),
                is_readonly: meta.permissions().readonly(),
                uid: Some(meta.uid()),
                gid: Some(meta.gid()),
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
        std::fs::read(path)
    }

    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        let mut file = std::fs::File::open(path)?;
        file.seek(io::SeekFrom::Start(offset))?;
        let mut buffer = vec![0u8; len];
        file.read_exact(&mut buffer)?;
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
}
