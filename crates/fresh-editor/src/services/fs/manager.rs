use crate::model::filesystem::{DirEntry, FileMetadata, FileSystem};
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::{oneshot, Mutex};

/// Type alias for pending directory requests map
type PendingDirRequests =
    Arc<Mutex<HashMap<PathBuf, Vec<oneshot::Sender<io::Result<Vec<DirEntry>>>>>>>;

/// Manages filesystem operations with request batching and deduplication
///
/// The FsManager sits between the application and the filesystem backend,
/// providing optimizations like:
/// - Request deduplication (multiple requests for the same path)
/// - Batching of metadata requests
/// - Centralized error handling
///
/// This wraps a `FileSystem` trait object and provides async methods
/// using `spawn_blocking` internally.
pub struct FsManager {
    fs: Arc<dyn FileSystem + Send + Sync>,
    /// Pending directory listing requests
    /// Map of path -> list of channels waiting for the result
    pending_dir_requests: PendingDirRequests,
}

impl fmt::Debug for FsManager {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FsManager")
            .field("fs", &"<dyn FileSystem>")
            .field("pending_dir_requests", &"<mutex>")
            .finish()
    }
}

impl FsManager {
    /// Create a new filesystem manager with the given filesystem implementation
    pub fn new(fs: Arc<dyn FileSystem + Send + Sync>) -> Self {
        Self {
            fs,
            pending_dir_requests: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// List directory contents with request deduplication
    ///
    /// If multiple requests for the same directory are made concurrently,
    /// only one filesystem operation will be performed and all requesters
    /// will receive the same result.
    pub async fn list_dir(&self, path: PathBuf) -> io::Result<Vec<DirEntry>> {
        // Check if there's already a pending request for this path
        let (rx, should_execute) = {
            let mut pending = self.pending_dir_requests.lock().await;

            if let Some(senders) = pending.get_mut(&path) {
                // There's already a request in progress, just add our channel
                let (tx, rx) = oneshot::channel();
                senders.push(tx);
                (rx, false)
            } else {
                // We're the first request for this path
                let (tx, rx) = oneshot::channel();
                pending.insert(path.clone(), vec![tx]);
                (rx, true)
            }
        };

        if should_execute {
            // We're responsible for executing the request
            let fs = Arc::clone(&self.fs);
            let path_clone = path.clone();
            let result = tokio::task::spawn_blocking(move || fs.read_dir(&path_clone))
                .await
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

            // Notify all waiting requesters
            let mut pending = self.pending_dir_requests.lock().await;
            if let Some(senders) = pending.remove(&path) {
                for sender in senders {
                    // Clone the result for each waiter
                    let _ = sender.send(
                        result
                            .as_ref()
                            .map(|v| v.clone())
                            .map_err(|e| io::Error::new(e.kind(), e.to_string())),
                    );
                }
            }

            result
        } else {
            // Wait for the other request to complete
            rx.await
                .unwrap_or_else(|_| Err(io::Error::other("Request cancelled")))
        }
    }

    /// Get metadata for multiple paths efficiently
    ///
    /// This fetches metadata in parallel using spawn_blocking.
    ///
    /// Returns a result for each path in the same order as the input.
    pub async fn get_metadata(&self, paths: Vec<PathBuf>) -> Vec<io::Result<FileMetadata>> {
        // Spawn parallel tasks for each path
        let tasks: Vec<_> = paths
            .into_iter()
            .map(|path| {
                let fs = Arc::clone(&self.fs);
                tokio::task::spawn_blocking(move || fs.metadata(&path))
            })
            .collect();

        // Collect results
        let mut results = Vec::with_capacity(tasks.len());
        for task in tasks {
            match task.await {
                Ok(result) => results.push(result),
                Err(e) => results.push(Err(io::Error::new(io::ErrorKind::Other, e.to_string()))),
            }
        }

        results
    }

    /// Get metadata for a single path
    pub async fn get_single_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        let fs = Arc::clone(&self.fs);
        let path = path.to_path_buf();
        tokio::task::spawn_blocking(move || fs.metadata(&path))
            .await
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?
    }

    /// Check if a path exists
    pub async fn exists(&self, path: &Path) -> bool {
        let fs = Arc::clone(&self.fs);
        let path = path.to_path_buf();
        tokio::task::spawn_blocking(move || fs.exists(&path))
            .await
            .unwrap_or(false)
    }

    /// Check if a path is a directory
    pub async fn is_dir(&self, path: &Path) -> io::Result<bool> {
        let fs = Arc::clone(&self.fs);
        let path = path.to_path_buf();
        tokio::task::spawn_blocking(move || fs.is_dir(&path))
            .await
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?
    }

    /// Get a complete entry for a path (with metadata)
    pub async fn get_entry(&self, path: &Path) -> io::Result<DirEntry> {
        let fs = Arc::clone(&self.fs);
        let path_buf = path.to_path_buf();
        tokio::task::spawn_blocking(move || {
            // Handle root paths (e.g., "/" on Unix) which have no file_name component
            let name = path_buf
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_else(|| path_buf.to_string_lossy().into_owned());

            // Get symlink metadata first to check if it's a symlink
            let symlink_meta = fs.symlink_metadata(&path_buf)?;

            // Determine entry type
            let is_symlink = {
                #[cfg(unix)]
                {
                    // Check file type from permissions mode
                    if let Some(ref perms) = symlink_meta.permissions {
                        // S_IFLNK = 0o120000
                        (perms.mode() & 0o170000) == 0o120000
                    } else {
                        false
                    }
                }
                #[cfg(not(unix))]
                {
                    false
                }
            };

            if is_symlink {
                // For symlinks, check what they point to
                let target_is_dir = fs.is_dir(&path_buf).unwrap_or(false);
                Ok(
                    DirEntry::new_symlink(path_buf, name, target_is_dir)
                        .with_metadata(symlink_meta),
                )
            } else {
                // Regular file or directory
                let entry_type = if fs.is_dir(&path_buf).unwrap_or(false) {
                    crate::model::filesystem::EntryType::Directory
                } else {
                    crate::model::filesystem::EntryType::File
                };
                Ok(DirEntry::new(path_buf, name, entry_type).with_metadata(symlink_meta))
            }
        })
        .await
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?
    }

    /// Get canonical path
    pub async fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        let fs = Arc::clone(&self.fs);
        let path = path.to_path_buf();
        tokio::task::spawn_blocking(move || fs.canonicalize(&path))
            .await
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?
    }

    /// List directory and fetch metadata for all entries in parallel
    ///
    /// This is a convenience method that combines `list_dir` with
    /// `get_metadata` to get complete information about all entries.
    pub async fn list_dir_with_metadata(&self, path: PathBuf) -> io::Result<Vec<DirEntry>> {
        let mut entries = self.list_dir(path).await?;

        // Collect paths for metadata batch fetch
        let paths: Vec<_> = entries.iter().map(|e| e.path.clone()).collect();

        // Fetch metadata in parallel
        let metadata_results = self.get_metadata(paths).await;

        // Attach metadata to entries
        for (entry, metadata_result) in entries.iter_mut().zip(metadata_results.into_iter()) {
            if let Ok(metadata) = metadata_result {
                entry.metadata = Some(metadata);
            }
        }

        Ok(entries)
    }

    /// Get the underlying filesystem implementation
    pub fn filesystem(&self) -> &Arc<dyn FileSystem + Send + Sync> {
        &self.fs
    }
}

impl Clone for FsManager {
    fn clone(&self) -> Self {
        Self {
            fs: Arc::clone(&self.fs),
            pending_dir_requests: Arc::clone(&self.pending_dir_requests),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::{EntryType, StdFileSystem};
    use std::fs as std_fs;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_list_dir() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create test structure
        std_fs::write(temp_path.join("file1.txt"), "content1").unwrap();
        std_fs::write(temp_path.join("file2.txt"), "content2").unwrap();
        std_fs::create_dir(temp_path.join("subdir")).unwrap();

        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        let entries = manager.list_dir(temp_path.to_path_buf()).await.unwrap();

        assert_eq!(entries.len(), 3);

        let names: Vec<_> = entries.iter().map(|e| e.name.as_str()).collect();
        assert!(names.contains(&"file1.txt"));
        assert!(names.contains(&"file2.txt"));
        assert!(names.contains(&"subdir"));
    }

    #[tokio::test]
    async fn test_request_deduplication() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create test files
        for i in 0..10 {
            std_fs::write(
                temp_path.join(format!("file{}.txt", i)),
                format!("content{}", i),
            )
            .unwrap();
        }

        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        // Spawn multiple concurrent requests for the same directory
        let mut handles = vec![];
        for _ in 0..10 {
            let manager = manager.clone();
            let path = temp_path.to_path_buf();
            handles.push(tokio::spawn(async move { manager.list_dir(path).await }));
        }

        // All requests should succeed and return the same data
        let mut results = vec![];
        for handle in handles {
            let result = handle.await.unwrap().unwrap();
            results.push(result);
        }

        assert_eq!(results.len(), 10);

        // All results should have the same entries
        let first_len = results[0].len();
        assert!(results.iter().all(|r| r.len() == first_len));
    }

    #[tokio::test]
    async fn test_get_metadata() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        std_fs::write(temp_path.join("file1.txt"), "content1").unwrap();
        std_fs::write(temp_path.join("file2.txt"), "content2").unwrap();

        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        let paths = vec![temp_path.join("file1.txt"), temp_path.join("file2.txt")];

        let results = manager.get_metadata(paths).await;

        assert_eq!(results.len(), 2);
        assert!(results[0].is_ok());
        assert!(results[1].is_ok());
    }

    #[tokio::test]
    async fn test_get_single_metadata() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");

        std_fs::write(&file_path, "content").unwrap();

        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        let metadata = manager.get_single_metadata(&file_path).await.unwrap();
        assert_eq!(metadata.size, 7);
    }

    #[tokio::test]
    async fn test_exists() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");

        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        assert!(!manager.exists(&file_path).await);

        std_fs::write(&file_path, "content").unwrap();

        assert!(manager.exists(&file_path).await);
    }

    #[tokio::test]
    async fn test_is_dir() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");
        let dir_path = temp_path.join("subdir");

        std_fs::write(&file_path, "content").unwrap();
        std_fs::create_dir(&dir_path).unwrap();

        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        assert!(!manager.is_dir(&file_path).await.unwrap());
        assert!(manager.is_dir(&dir_path).await.unwrap());
    }

    #[tokio::test]
    async fn test_get_entry() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");

        std_fs::write(&file_path, "test content").unwrap();

        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        let entry = manager.get_entry(&file_path).await.unwrap();

        assert_eq!(entry.name, "test.txt");
        assert_eq!(entry.entry_type, EntryType::File);
        assert!(entry.metadata.is_some());
        assert_eq!(entry.metadata.unwrap().size, 12);
    }

    /// Test that get_entry works for root path "/" (issue #902)
    #[tokio::test]
    async fn test_get_entry_root_path() {
        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        // Root path "/" has no file_name() component, but should still work
        let root = PathBuf::from("/");
        let entry = manager.get_entry(&root).await.unwrap();

        assert_eq!(entry.name, "/");
        assert_eq!(entry.entry_type, EntryType::Directory);
    }

    #[tokio::test]
    async fn test_list_dir_with_metadata() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        std_fs::write(temp_path.join("file1.txt"), "content1").unwrap();
        std_fs::write(temp_path.join("file2.txt"), "content2").unwrap();
        std_fs::create_dir(temp_path.join("subdir")).unwrap();

        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        let entries = manager
            .list_dir_with_metadata(temp_path.to_path_buf())
            .await
            .unwrap();

        assert_eq!(entries.len(), 3);

        // All entries should have metadata
        assert!(entries.iter().all(|e| e.metadata.is_some()));

        // Check file sizes
        let file1 = entries.iter().find(|e| e.name == "file1.txt").unwrap();
        assert_eq!(file1.metadata.as_ref().unwrap().size, 8);
    }

    #[tokio::test]
    async fn test_concurrent_different_dirs() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create multiple directories
        for i in 0..5 {
            let dir_path = temp_path.join(format!("dir{}", i));
            std_fs::create_dir(&dir_path).unwrap();
            for j in 0..3 {
                std_fs::write(
                    dir_path.join(format!("file{}.txt", j)),
                    format!("content{}", j),
                )
                .unwrap();
            }
        }

        let fs = Arc::new(StdFileSystem);
        let manager = FsManager::new(fs);

        // List all directories concurrently
        let mut handles = vec![];
        for i in 0..5 {
            let manager = manager.clone();
            let path = temp_path.join(format!("dir{}", i));
            handles.push(tokio::spawn(async move { manager.list_dir(path).await }));
        }

        // All should succeed
        for handle in handles {
            let result = handle.await.unwrap().unwrap();
            assert_eq!(result.len(), 3);
        }
    }
}
