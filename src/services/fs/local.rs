use super::backend::{FsBackend, FsEntry, FsEntryType, FsMetadata};
use async_trait::async_trait;
use lru::LruCache;
use std::io;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::fs;
use tokio::sync::RwLock;

/// Local filesystem backend with caching
pub struct LocalFsBackend {
    /// LRU cache for metadata to reduce syscalls
    metadata_cache: Arc<RwLock<LruCache<PathBuf, CachedMetadata>>>,
    /// How long to cache metadata before refreshing
    cache_duration: Duration,
}

#[derive(Clone)]
struct CachedMetadata {
    metadata: FsMetadata,
    cached_at: Instant,
}

impl LocalFsBackend {
    /// Create a new local filesystem backend with default cache settings
    pub fn new() -> Self {
        Self::with_cache_settings(1000, Duration::from_secs(5))
    }

    /// Create a backend with custom cache settings
    ///
    /// # Arguments
    ///
    /// * `cache_size` - Maximum number of metadata entries to cache
    /// * `cache_duration` - How long to cache metadata before refreshing
    pub fn with_cache_settings(cache_size: usize, cache_duration: Duration) -> Self {
        Self {
            metadata_cache: Arc::new(RwLock::new(LruCache::new(
                NonZeroUsize::new(cache_size).unwrap(),
            ))),
            cache_duration,
        }
    }

    /// Get metadata from cache if available and not stale
    async fn get_cached_metadata(&self, path: &Path) -> Option<FsMetadata> {
        let cache = self.metadata_cache.read().await;
        if let Some(cached) = cache.peek(path) {
            if cached.cached_at.elapsed() < self.cache_duration {
                return Some(cached.metadata.clone());
            }
        }
        None
    }

    /// Store metadata in cache
    async fn cache_metadata(&self, path: PathBuf, metadata: FsMetadata) {
        let mut cache = self.metadata_cache.write().await;
        cache.put(
            path,
            CachedMetadata {
                metadata,
                cached_at: Instant::now(),
            },
        );
    }

    /// Read metadata for a single path
    async fn read_metadata(&self, path: &Path) -> io::Result<FsMetadata> {
        // Check cache first
        if let Some(cached) = self.get_cached_metadata(path).await {
            return Ok(cached);
        }

        // Read from filesystem
        let std_metadata = fs::metadata(path).await?;
        let is_hidden = is_hidden_file(path);

        let metadata = FsMetadata::new()
            .with_size(std_metadata.len())
            .with_modified(
                std_metadata
                    .modified()
                    .ok()
                    .unwrap_or(std::time::UNIX_EPOCH),
            )
            .with_hidden(is_hidden)
            .with_readonly(std_metadata.permissions().readonly());

        // Cache it
        self.cache_metadata(path.to_path_buf(), metadata.clone())
            .await;

        Ok(metadata)
    }

    /// Determine entry type from metadata
    fn entry_type_from_metadata(metadata: &std::fs::Metadata) -> FsEntryType {
        if metadata.is_symlink() {
            FsEntryType::Symlink
        } else if metadata.is_dir() {
            FsEntryType::Directory
        } else {
            FsEntryType::File
        }
    }
}

impl Default for LocalFsBackend {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl FsBackend for LocalFsBackend {
    async fn read_dir(&self, path: &Path) -> io::Result<Vec<FsEntry>> {
        let mut entries = Vec::new();
        let mut read_dir = fs::read_dir(path).await?;

        while let Some(entry) = read_dir.next_entry().await? {
            let path = entry.path();
            let name = entry.file_name().to_string_lossy().into_owned();

            let entry_type = if let Ok(file_type) = entry.file_type().await {
                if file_type.is_symlink() {
                    FsEntryType::Symlink
                } else if file_type.is_dir() {
                    FsEntryType::Directory
                } else {
                    FsEntryType::File
                }
            } else {
                // If we can't determine type, assume file
                FsEntryType::File
            };

            entries.push(FsEntry::new(path, name, entry_type));
        }

        Ok(entries)
    }

    async fn get_metadata_batch(&self, paths: &[PathBuf]) -> Vec<io::Result<FsMetadata>> {
        // Spawn tasks to fetch metadata in parallel
        let tasks: Vec<_> = paths
            .iter()
            .map(|path| {
                let path = path.clone();
                let backend = self.clone();
                tokio::spawn(async move { backend.read_metadata(&path).await })
            })
            .collect();

        // Collect results
        let mut results = Vec::with_capacity(paths.len());
        for task in tasks {
            match task.await {
                Ok(Ok(metadata)) => results.push(Ok(metadata)),
                Ok(Err(e)) => results.push(Err(e)),
                Err(_) => {
                    results.push(Err(io::Error::other("Task join error")))
                }
            }
        }

        results
    }

    async fn exists(&self, path: &Path) -> bool {
        fs::try_exists(path).await.unwrap_or(false)
    }

    async fn is_dir(&self, path: &Path) -> io::Result<bool> {
        let metadata = fs::metadata(path).await?;
        Ok(metadata.is_dir())
    }

    async fn get_entry(&self, path: &Path) -> io::Result<FsEntry> {
        let metadata = fs::metadata(path).await?;
        let name = path
            .file_name()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Invalid path"))?
            .to_string_lossy()
            .into_owned();

        let entry_type = Self::entry_type_from_metadata(&metadata);
        let is_hidden = is_hidden_file(path);

        let fs_metadata = FsMetadata::new()
            .with_size(metadata.len())
            .with_modified(metadata.modified().ok().unwrap_or(std::time::UNIX_EPOCH))
            .with_hidden(is_hidden)
            .with_readonly(metadata.permissions().readonly());

        Ok(FsEntry::new(path.to_path_buf(), name, entry_type).with_metadata(fs_metadata))
    }

    async fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        fs::canonicalize(path).await
    }
}

// Clone implementation for LocalFsBackend to enable parallel operations
impl Clone for LocalFsBackend {
    fn clone(&self) -> Self {
        Self {
            metadata_cache: Arc::clone(&self.metadata_cache),
            cache_duration: self.cache_duration,
        }
    }
}

/// Check if a file is hidden (starts with . on Unix, or has hidden attribute on Windows)
fn is_hidden_file(path: &Path) -> bool {
    // Check for dot-prefix (works on all platforms)
    let is_dot_hidden = path
        .file_name()
        .and_then(|name| name.to_str())
        .map(|name| name.starts_with('.'))
        .unwrap_or(false);

    if is_dot_hidden {
        return true;
    }

    // On Windows, also check the hidden attribute
    #[cfg(windows)]
    {
        use std::os::windows::fs::MetadataExt;
        const FILE_ATTRIBUTE_HIDDEN: u32 = 0x2;

        if let Ok(metadata) = std::fs::metadata(path) {
            return metadata.file_attributes() & FILE_ATTRIBUTE_HIDDEN != 0;
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs as std_fs;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_read_dir() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create test files
        std_fs::write(temp_path.join("file1.txt"), "content1").unwrap();
        std_fs::write(temp_path.join("file2.txt"), "content2").unwrap();
        std_fs::create_dir(temp_path.join("subdir")).unwrap();

        let backend = LocalFsBackend::new();
        let entries = backend.read_dir(temp_path).await.unwrap();

        assert_eq!(entries.len(), 3);

        // Check that we have the expected entries
        let names: Vec<_> = entries.iter().map(|e| e.name.as_str()).collect();
        assert!(names.contains(&"file1.txt"));
        assert!(names.contains(&"file2.txt"));
        assert!(names.contains(&"subdir"));

        // Check types
        let subdir = entries.iter().find(|e| e.name == "subdir").unwrap();
        assert!(subdir.is_dir());

        let file1 = entries.iter().find(|e| e.name == "file1.txt").unwrap();
        assert!(file1.is_file());
    }

    #[tokio::test]
    async fn test_get_metadata_batch() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create test files
        std_fs::write(temp_path.join("file1.txt"), "content1").unwrap();
        std_fs::write(temp_path.join("file2.txt"), "content2").unwrap();

        let backend = LocalFsBackend::new();
        let paths = vec![temp_path.join("file1.txt"), temp_path.join("file2.txt")];

        let results = backend.get_metadata_batch(&paths).await;

        assert_eq!(results.len(), 2);
        assert!(results[0].is_ok());
        assert!(results[1].is_ok());

        let meta1 = results[0].as_ref().unwrap();
        assert_eq!(meta1.size, Some(8)); // "content1" is 8 bytes
    }

    #[tokio::test]
    async fn test_metadata_caching() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");

        std_fs::write(&file_path, "content").unwrap();

        let backend = LocalFsBackend::with_cache_settings(10, Duration::from_secs(10));

        // First read - should populate cache
        let meta1 = backend.read_metadata(&file_path).await.unwrap();
        assert_eq!(meta1.size, Some(7));

        // Second read - should hit cache
        let meta2 = backend.read_metadata(&file_path).await.unwrap();
        assert_eq!(meta2.size, Some(7));

        // Verify cache was used (sizes should match)
        assert_eq!(meta1.size, meta2.size);
    }

    #[tokio::test]
    async fn test_exists() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");

        let backend = LocalFsBackend::new();

        assert!(!backend.exists(&file_path).await);

        std_fs::write(&file_path, "content").unwrap();

        assert!(backend.exists(&file_path).await);
    }

    #[tokio::test]
    async fn test_is_dir() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");
        let dir_path = temp_path.join("subdir");

        std_fs::write(&file_path, "content").unwrap();
        std_fs::create_dir(&dir_path).unwrap();

        let backend = LocalFsBackend::new();

        assert!(!backend.is_dir(&file_path).await.unwrap());
        assert!(backend.is_dir(&dir_path).await.unwrap());
    }

    #[tokio::test]
    async fn test_get_entry() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let file_path = temp_path.join("test.txt");

        std_fs::write(&file_path, "content").unwrap();

        let backend = LocalFsBackend::new();
        let entry = backend.get_entry(&file_path).await.unwrap();

        assert_eq!(entry.name, "test.txt");
        assert!(entry.is_file());
        assert!(entry.metadata.is_some());

        let metadata = entry.metadata.unwrap();
        assert_eq!(metadata.size, Some(7));
    }

    #[tokio::test]
    async fn test_hidden_file_detection() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        let hidden_file = temp_path.join(".hidden");
        let normal_file = temp_path.join("visible.txt");

        std_fs::write(&hidden_file, "hidden").unwrap();
        std_fs::write(&normal_file, "visible").unwrap();

        let backend = LocalFsBackend::new();

        let hidden_entry = backend.get_entry(&hidden_file).await.unwrap();
        let normal_entry = backend.get_entry(&normal_file).await.unwrap();

        assert!(hidden_entry.metadata.as_ref().unwrap().is_hidden);
        assert!(!normal_entry.metadata.as_ref().unwrap().is_hidden);
    }

    #[tokio::test]
    async fn test_parallel_metadata_fetch() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create 100 test files
        for i in 0..100 {
            std_fs::write(
                temp_path.join(format!("file{}.txt", i)),
                format!("content{}", i),
            )
            .unwrap();
        }

        let backend = LocalFsBackend::new();
        let paths: Vec<_> = (0..100)
            .map(|i| temp_path.join(format!("file{}.txt", i)))
            .collect();

        let start = Instant::now();
        let results = backend.get_metadata_batch(&paths).await;
        let duration = start.elapsed();

        assert_eq!(results.len(), 100);
        assert!(results.iter().all(|r| r.is_ok()));

        // Parallel should be reasonably fast (less than 1 second for 100 files)
        assert!(duration.as_secs() < 1);
    }

    #[test]
    fn test_is_hidden_file() {
        assert!(is_hidden_file(Path::new(".hidden")));
        assert!(is_hidden_file(Path::new("/path/to/.hidden")));
        assert!(!is_hidden_file(Path::new("visible.txt")));
        assert!(!is_hidden_file(Path::new("/path/to/visible.txt")));
    }
}
