//! Slow filesystem wrapper for testing
//!
//! This module provides a decorator/wrapper around any FileSystem that adds
//! configurable delays to simulate slow I/O operations. This is useful for
//! testing editor responsiveness and performance with slow filesystems (network
//! drives, slow disks, etc.).

use crate::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSystem, FileWriter,
};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

/// Configuration for slow filesystem simulation
#[derive(Debug, Clone)]
pub struct SlowFsConfig {
    /// Delay for read_dir operations
    pub read_dir_delay: Duration,
    /// Delay for metadata operations
    pub metadata_delay: Duration,
    /// Delay for read_file operations
    pub read_file_delay: Duration,
    /// Delay for write_file operations
    pub write_file_delay: Duration,
    /// Delay for other operations (exists, is_dir, etc.)
    pub other_delay: Duration,
}

impl SlowFsConfig {
    /// Create a config with uniform delay for all operations
    pub fn uniform(delay: Duration) -> Self {
        Self {
            read_dir_delay: delay,
            metadata_delay: delay,
            read_file_delay: delay,
            write_file_delay: delay,
            other_delay: delay,
        }
    }

    /// Create a config with no delays (useful as a baseline)
    pub fn none() -> Self {
        Self::uniform(Duration::ZERO)
    }

    /// Create a config simulating a slow network filesystem
    pub fn slow_network() -> Self {
        Self {
            read_dir_delay: Duration::from_millis(500),
            metadata_delay: Duration::from_millis(50),
            read_file_delay: Duration::from_millis(200),
            write_file_delay: Duration::from_millis(300),
            other_delay: Duration::from_millis(30),
        }
    }

    /// Create a config simulating a very slow disk
    pub fn slow_disk() -> Self {
        Self {
            read_dir_delay: Duration::from_millis(200),
            metadata_delay: Duration::from_millis(20),
            read_file_delay: Duration::from_millis(100),
            write_file_delay: Duration::from_millis(150),
            other_delay: Duration::from_millis(10),
        }
    }
}

impl Default for SlowFsConfig {
    fn default() -> Self {
        Self::none()
    }
}

/// Metrics tracking for filesystem operations
#[derive(Debug, Default)]
pub struct BackendMetrics {
    /// Number of read_dir calls
    pub read_dir_calls: AtomicUsize,
    /// Number of metadata calls
    pub metadata_calls: AtomicUsize,
    /// Number of read_file calls
    pub read_file_calls: AtomicUsize,
    /// Number of write_file calls
    pub write_file_calls: AtomicUsize,
    /// Number of other calls
    pub other_calls: AtomicUsize,
}

impl BackendMetrics {
    /// Create new empty metrics
    pub fn new() -> Self {
        Self::default()
    }

    /// Reset all metrics to zero
    pub fn reset(&self) {
        self.read_dir_calls.store(0, Ordering::SeqCst);
        self.metadata_calls.store(0, Ordering::SeqCst);
        self.read_file_calls.store(0, Ordering::SeqCst);
        self.write_file_calls.store(0, Ordering::SeqCst);
        self.other_calls.store(0, Ordering::SeqCst);
    }

    /// Get total number of filesystem calls
    pub fn total_calls(&self) -> usize {
        self.read_dir_calls.load(Ordering::SeqCst)
            + self.metadata_calls.load(Ordering::SeqCst)
            + self.read_file_calls.load(Ordering::SeqCst)
            + self.write_file_calls.load(Ordering::SeqCst)
            + self.other_calls.load(Ordering::SeqCst)
    }
}

/// Slow filesystem wrapper for testing
///
/// Wraps any FileSystem implementation and adds configurable delays to each
/// operation. Also tracks metrics about operation counts.
pub struct SlowFileSystem {
    /// The underlying real filesystem
    inner: Arc<dyn FileSystem>,
    /// Configuration for delays
    config: SlowFsConfig,
    /// Metrics tracking
    metrics: Arc<BackendMetrics>,
}

impl SlowFileSystem {
    /// Create a new slow filesystem wrapper
    pub fn new(inner: Arc<dyn FileSystem>, config: SlowFsConfig) -> Self {
        Self {
            inner,
            config,
            metrics: Arc::new(BackendMetrics::new()),
        }
    }

    /// Create with uniform delay for all operations
    pub fn with_uniform_delay(inner: Arc<dyn FileSystem>, delay: Duration) -> Self {
        Self::new(inner, SlowFsConfig::uniform(delay))
    }

    /// Get a reference to the metrics
    pub fn metrics(&self) -> &Arc<BackendMetrics> {
        &self.metrics
    }

    /// Reset metrics to zero
    pub fn reset_metrics(&self) {
        self.metrics.reset();
    }

    /// Add delay
    fn add_delay(&self, delay: Duration) {
        if !delay.is_zero() {
            std::thread::sleep(delay);
        }
    }
}

impl FileSystem for SlowFileSystem {
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.add_delay(self.config.read_file_delay);
        self.metrics.read_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.read_file(path)
    }

    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.add_delay(self.config.read_file_delay);
        self.metrics.read_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.read_range(path, offset, len)
    }

    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.add_delay(self.config.write_file_delay);
        self.metrics.write_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.write_file(path, data)
    }

    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.add_delay(self.config.write_file_delay);
        self.metrics.write_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.create_file(path)
    }

    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        self.add_delay(self.config.read_file_delay);
        self.metrics.read_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.open_file(path)
    }

    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.add_delay(self.config.write_file_delay);
        self.metrics.write_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.open_file_for_write(path)
    }

    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.add_delay(self.config.write_file_delay);
        self.metrics.write_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.open_file_for_append(path)
    }

    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
        self.add_delay(self.config.write_file_delay);
        self.metrics.write_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.set_file_length(path, len)
    }

    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.add_delay(self.config.other_delay);
        self.metrics.other_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.rename(from, to)
    }

    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        self.add_delay(self.config.write_file_delay);
        self.metrics.write_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.copy(from, to)
    }

    fn remove_file(&self, path: &Path) -> io::Result<()> {
        self.add_delay(self.config.other_delay);
        self.metrics.other_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.remove_file(path)
    }

    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        self.add_delay(self.config.other_delay);
        self.metrics.other_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.remove_dir(path)
    }

    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.add_delay(self.config.metadata_delay);
        self.metrics.metadata_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.metadata(path)
    }

    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.add_delay(self.config.metadata_delay);
        self.metrics.metadata_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.symlink_metadata(path)
    }

    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.add_delay(self.config.other_delay);
        self.metrics.other_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.is_dir(path)
    }

    fn is_file(&self, path: &Path) -> io::Result<bool> {
        self.add_delay(self.config.other_delay);
        self.metrics.other_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.is_file(path)
    }

    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
        self.add_delay(self.config.other_delay);
        self.metrics.other_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.set_permissions(path, permissions)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        self.add_delay(self.config.read_dir_delay);
        self.metrics.read_dir_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.read_dir(path)
    }

    fn create_dir(&self, path: &Path) -> io::Result<()> {
        self.add_delay(self.config.other_delay);
        self.metrics.other_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.create_dir(path)
    }

    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        self.add_delay(self.config.other_delay);
        self.metrics.other_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.create_dir_all(path)
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.add_delay(self.config.other_delay);
        self.metrics.other_calls.fetch_add(1, Ordering::SeqCst);
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
    ) -> io::Result<()> {
        self.add_delay(self.config.write_file_delay);
        self.metrics.write_file_calls.fetch_add(1, Ordering::SeqCst);
        self.inner.sudo_write(path, data, mode, uid, gid)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;
    use std::time::Instant;
    use tempfile::TempDir;

    #[test]
    fn test_slow_fs_adds_delay() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        let inner = Arc::new(StdFileSystem);
        let slow_config = SlowFsConfig::uniform(Duration::from_millis(100));
        let slow = SlowFileSystem::new(inner, slow_config);

        let start = Instant::now();
        drop(slow.read_dir(temp_path));
        let elapsed = start.elapsed();

        // Should take at least 100ms due to artificial delay
        assert!(
            elapsed >= Duration::from_millis(100),
            "Expected at least 100ms delay, got {:?}",
            elapsed
        );

        // Check metrics
        assert_eq!(slow.metrics().read_dir_calls.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_metrics_tracking() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        let inner = Arc::new(StdFileSystem);
        let slow = SlowFileSystem::new(inner, SlowFsConfig::none());

        // Perform various operations
        drop(slow.read_dir(temp_path));
        drop(slow.metadata(temp_path));
        drop(slow.is_dir(temp_path));

        assert_eq!(slow.metrics().read_dir_calls.load(Ordering::SeqCst), 1);
        assert_eq!(slow.metrics().metadata_calls.load(Ordering::SeqCst), 1);
        assert_eq!(slow.metrics().other_calls.load(Ordering::SeqCst), 1);
        assert_eq!(slow.metrics().total_calls(), 3);
    }

    #[test]
    fn test_reset_metrics() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        let inner = Arc::new(StdFileSystem);
        let slow = SlowFileSystem::new(inner, SlowFsConfig::none());

        // Perform some operations
        drop(slow.read_dir(temp_path));
        drop(slow.metadata(temp_path));

        // Verify metrics are non-zero
        assert!(slow.metrics().total_calls() > 0);

        // Reset
        slow.reset_metrics();

        // Verify metrics are zero
        assert_eq!(slow.metrics().total_calls(), 0);
    }

    #[test]
    fn test_preset_configs() {
        let inner = Arc::new(StdFileSystem);

        // Test slow_network preset
        let network_config = SlowFsConfig::slow_network();
        assert_eq!(network_config.read_dir_delay, Duration::from_millis(500));

        // Test slow_disk preset
        let disk_config = SlowFsConfig::slow_disk();
        assert_eq!(disk_config.read_dir_delay, Duration::from_millis(200));

        // Test none preset
        let none_config = SlowFsConfig::none();
        assert_eq!(none_config.read_dir_delay, Duration::ZERO);

        // Ensure they can all be constructed
        let _slow_network = SlowFileSystem::new(inner.clone(), network_config);
        let _slow_disk = SlowFileSystem::new(inner.clone(), disk_config);
        let _no_delay = SlowFileSystem::new(inner, none_config);
    }
}
