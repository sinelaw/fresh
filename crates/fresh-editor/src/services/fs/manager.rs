use super::backend::{FsBackend, FsEntry, FsMetadata};
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::{oneshot, Mutex};

/// Type alias for pending directory requests map
type PendingDirRequests =
    Arc<Mutex<HashMap<PathBuf, Vec<oneshot::Sender<io::Result<Vec<FsEntry>>>>>>>;

/// Manages filesystem operations with request batching and deduplication
///
/// The FsManager sits between the application and the filesystem backend,
/// providing optimizations like:
/// - Request deduplication (multiple requests for the same path)
/// - Batching of metadata requests
/// - Centralized error handling
pub struct FsManager {
    backend: Arc<dyn FsBackend>,
    /// Pending directory listing requests
    /// Map of path -> list of channels waiting for the result
    pending_dir_requests: PendingDirRequests,
}

impl fmt::Debug for FsManager {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FsManager")
            .field("backend", &"<dyn FsBackend>")
            .field("pending_dir_requests", &"<mutex>")
            .finish()
    }
}

impl FsManager {
    /// Create a new filesystem manager with the given backend
    pub fn new(backend: Arc<dyn FsBackend>) -> Self {
        Self {
            backend,
            pending_dir_requests: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// List directory contents with request deduplication
    ///
    /// If multiple requests for the same directory are made concurrently,
    /// only one filesystem operation will be performed and all requesters
    /// will receive the same result.
    pub async fn list_dir(&self, path: PathBuf) -> io::Result<Vec<FsEntry>> {
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
            let result = self.backend.read_dir(&path).await;

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
    /// This delegates to the backend's batch metadata implementation,
    /// which may parallelize the operations.
    pub async fn get_metadata(&self, paths: Vec<PathBuf>) -> Vec<io::Result<FsMetadata>> {
        self.backend.get_metadata_batch(&paths).await
    }

    /// Get metadata for a single path
    pub async fn get_single_metadata(&self, path: &Path) -> io::Result<FsMetadata> {
        let results = self
            .backend
            .get_metadata_batch(std::slice::from_ref(&path.to_path_buf()))
            .await;
        results
            .into_iter()
            .next()
            .unwrap_or_else(|| Err(io::Error::other("No result returned")))
    }

    /// Check if a path exists
    pub async fn exists(&self, path: &Path) -> bool {
        self.backend.exists(path).await
    }

    /// Check if a path is a directory
    pub async fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.backend.is_dir(path).await
    }

    /// Get a complete entry for a path (with metadata)
    pub async fn get_entry(&self, path: &Path) -> io::Result<FsEntry> {
        self.backend.get_entry(path).await
    }

    /// Get canonical path
    pub async fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.backend.canonicalize(path).await
    }

    /// List directory and fetch metadata for all entries in parallel
    ///
    /// This is a convenience method that combines `list_dir` with
    /// `get_metadata` to get complete information about all entries.
    pub async fn list_dir_with_metadata(&self, path: PathBuf) -> io::Result<Vec<FsEntry>> {
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

    /// Get the underlying backend
    pub fn backend(&self) -> &Arc<dyn FsBackend> {
        &self.backend
    }
}

impl Clone for FsManager {
    fn clone(&self) -> Self {
        Self {
            backend: Arc::clone(&self.backend),
            pending_dir_requests: Arc::clone(&self.pending_dir_requests),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::fs::{FsEntryType, LocalFsBackend};
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

        let backend = Arc::new(LocalFsBackend::new());
        let manager = FsManager::new(backend);

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

        let backend = Arc::new(LocalFsBackend::new());
        let manager = FsManager::new(backend);

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

        let backend = Arc::new(LocalFsBackend::new());
        let manager = FsManager::new(backend);

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

        let backend = Arc::new(LocalFsBackend::new());
        let manager = FsManager::new(backend);

        let metadata = manager.get_single_metadata(&file_path).await.unwrap();
        assert_eq!(metadata.size, Some(7));
    }

    #[tokio::test]
    async fn test_exists() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");

        let backend = Arc::new(LocalFsBackend::new());
        let manager = FsManager::new(backend);

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

        let backend = Arc::new(LocalFsBackend::new());
        let manager = FsManager::new(backend);

        assert!(!manager.is_dir(&file_path).await.unwrap());
        assert!(manager.is_dir(&dir_path).await.unwrap());
    }

    #[tokio::test]
    async fn test_get_entry() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");

        std_fs::write(&file_path, "test content").unwrap();

        let backend = Arc::new(LocalFsBackend::new());
        let manager = FsManager::new(backend);

        let entry = manager.get_entry(&file_path).await.unwrap();

        assert_eq!(entry.name, "test.txt");
        assert_eq!(entry.entry_type, FsEntryType::File);
        assert!(entry.metadata.is_some());
        assert_eq!(entry.metadata.unwrap().size, Some(12));
    }

    #[tokio::test]
    async fn test_list_dir_with_metadata() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        std_fs::write(temp_path.join("file1.txt"), "content1").unwrap();
        std_fs::write(temp_path.join("file2.txt"), "content2").unwrap();
        std_fs::create_dir(temp_path.join("subdir")).unwrap();

        let backend = Arc::new(LocalFsBackend::new());
        let manager = FsManager::new(backend);

        let entries = manager
            .list_dir_with_metadata(temp_path.to_path_buf())
            .await
            .unwrap();

        assert_eq!(entries.len(), 3);

        // All entries should have metadata
        assert!(entries.iter().all(|e| e.metadata.is_some()));

        // Check file sizes
        let file1 = entries.iter().find(|e| e.name == "file1.txt").unwrap();
        assert_eq!(file1.metadata.as_ref().unwrap().size, Some(8));
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

        let backend = Arc::new(LocalFsBackend::new());
        let manager = FsManager::new(backend);

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
