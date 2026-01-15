use async_trait::async_trait;
use std::io;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

/// Represents a file or directory entry
#[derive(Debug, Clone)]
pub struct FsEntry {
    pub path: PathBuf,
    pub name: String,
    pub entry_type: FsEntryType,
    pub metadata: Option<FsMetadata>,
}

impl FsEntry {
    pub fn new(path: PathBuf, name: String, entry_type: FsEntryType) -> Self {
        Self {
            path,
            name,
            entry_type,
            metadata: None,
        }
    }

    pub fn with_metadata(mut self, metadata: FsMetadata) -> Self {
        self.metadata = Some(metadata);
        self
    }

    pub fn is_dir(&self) -> bool {
        self.entry_type == FsEntryType::Directory
    }

    pub fn is_file(&self) -> bool {
        self.entry_type == FsEntryType::File
    }

    pub fn is_symlink(&self) -> bool {
        self.entry_type == FsEntryType::Symlink
    }
}

/// Type of filesystem entry
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FsEntryType {
    File,
    Directory,
    Symlink,
}

/// Metadata about a filesystem entry
#[derive(Debug, Clone)]
pub struct FsMetadata {
    pub size: Option<u64>,
    pub modified: Option<SystemTime>,
    pub is_hidden: bool,
    pub is_readonly: bool,
}

impl FsMetadata {
    pub fn new() -> Self {
        Self {
            size: None,
            modified: None,
            is_hidden: false,
            is_readonly: false,
        }
    }

    pub fn with_size(mut self, size: u64) -> Self {
        self.size = Some(size);
        self
    }

    pub fn with_modified(mut self, modified: SystemTime) -> Self {
        self.modified = Some(modified);
        self
    }

    pub fn with_hidden(mut self, hidden: bool) -> Self {
        self.is_hidden = hidden;
        self
    }

    pub fn with_readonly(mut self, readonly: bool) -> Self {
        self.is_readonly = readonly;
        self
    }
}

impl Default for FsMetadata {
    fn default() -> Self {
        Self::new()
    }
}

/// Async filesystem backend trait
///
/// This trait abstracts filesystem operations to support different backends
/// (local fs, network fs, virtual fs, etc.) with async operations suitable
/// for slow/network filesystems.
#[async_trait]
pub trait FsBackend: Send + Sync {
    /// List entries in a directory (non-recursive)
    ///
    /// Returns entries without metadata for speed. Use `get_metadata_batch`
    /// to fetch metadata for multiple entries in parallel.
    ///
    /// # Errors
    ///
    /// Returns an error if the directory cannot be read (permission denied,
    /// doesn't exist, not a directory, etc.)
    async fn read_dir(&self, path: &Path) -> io::Result<Vec<FsEntry>>;

    /// Get metadata for multiple paths in parallel
    ///
    /// This is the preferred way to get metadata for multiple files,
    /// as it allows the backend to parallelize operations.
    ///
    /// Returns a result for each path in the same order as the input.
    async fn get_metadata_batch(&self, paths: &[PathBuf]) -> Vec<io::Result<FsMetadata>>;

    /// Check if path exists
    async fn exists(&self, path: &Path) -> bool;

    /// Check if path is a directory
    ///
    /// Returns false if the path doesn't exist or is not a directory.
    async fn is_dir(&self, path: &Path) -> io::Result<bool>;

    /// Get single entry with metadata
    ///
    /// This is a convenience method for getting a complete entry.
    /// For multiple entries, prefer `read_dir` + `get_metadata_batch`.
    async fn get_entry(&self, path: &Path) -> io::Result<FsEntry>;

    /// Get canonical (absolute, normalized) path
    async fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fs_entry_creation() {
        let entry = FsEntry::new(
            PathBuf::from("/test/file.txt"),
            "file.txt".to_string(),
            FsEntryType::File,
        );

        assert_eq!(entry.name, "file.txt");
        assert!(entry.is_file());
        assert!(!entry.is_dir());
        assert!(!entry.is_symlink());
        assert!(entry.metadata.is_none());
    }

    #[test]
    fn test_fs_entry_with_metadata() {
        let metadata = FsMetadata::new()
            .with_size(1024)
            .with_hidden(true)
            .with_readonly(false);

        let entry = FsEntry::new(
            PathBuf::from("/test/file.txt"),
            "file.txt".to_string(),
            FsEntryType::File,
        )
        .with_metadata(metadata);

        assert!(entry.metadata.is_some());
        let meta = entry.metadata.unwrap();
        assert_eq!(meta.size, Some(1024));
        assert!(meta.is_hidden);
        assert!(!meta.is_readonly);
    }

    #[test]
    fn test_fs_entry_types() {
        let file = FsEntry::new(
            PathBuf::from("/file"),
            "file".to_string(),
            FsEntryType::File,
        );
        assert!(file.is_file());

        let dir = FsEntry::new(
            PathBuf::from("/dir"),
            "dir".to_string(),
            FsEntryType::Directory,
        );
        assert!(dir.is_dir());

        let link = FsEntry::new(
            PathBuf::from("/link"),
            "link".to_string(),
            FsEntryType::Symlink,
        );
        assert!(link.is_symlink());
    }

    #[test]
    fn test_metadata_builder() {
        let metadata = FsMetadata::default()
            .with_size(512)
            .with_hidden(false)
            .with_readonly(true);

        assert_eq!(metadata.size, Some(512));
        assert!(!metadata.is_hidden);
        assert!(metadata.is_readonly);
    }
}
