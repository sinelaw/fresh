//! Slow filesystem backend for testing
//!
//! This module provides a decorator/wrapper around any FsBackend that adds
//! configurable delays to simulate slow I/O operations. This is useful for
//! testing editor responsiveness and performance with slow filesystems (network
//! drives, slow disks, etc.).

use super::backend::{FsBackend, FsEntry, FsMetadata};
use async_trait::async_trait;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Mutex;

/// Configuration for slow filesystem simulation
#[derive(Debug, Clone)]
pub struct SlowFsConfig {
    /// Delay for read_dir operations
    pub read_dir_delay: Duration,
    /// Delay for get_metadata_batch operations (per item)
    pub metadata_delay: Duration,
    /// Delay for exists operations
    pub exists_delay: Duration,
    /// Delay for is_dir operations
    pub is_dir_delay: Duration,
    /// Delay for get_entry operations
    pub get_entry_delay: Duration,
    /// Delay for canonicalize operations
    pub canonicalize_delay: Duration,
}

impl SlowFsConfig {
    /// Create a config with uniform delay for all operations
    pub fn uniform(delay: Duration) -> Self {
        Self {
            read_dir_delay: delay,
            metadata_delay: delay,
            exists_delay: delay,
            is_dir_delay: delay,
            get_entry_delay: delay,
            canonicalize_delay: delay,
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
            exists_delay: Duration::from_millis(30),
            is_dir_delay: Duration::from_millis(30),
            get_entry_delay: Duration::from_millis(100),
            canonicalize_delay: Duration::from_millis(50),
        }
    }

    /// Create a config simulating a very slow disk
    pub fn slow_disk() -> Self {
        Self {
            read_dir_delay: Duration::from_millis(200),
            metadata_delay: Duration::from_millis(20),
            exists_delay: Duration::from_millis(10),
            is_dir_delay: Duration::from_millis(10),
            get_entry_delay: Duration::from_millis(50),
            canonicalize_delay: Duration::from_millis(20),
        }
    }
}

impl Default for SlowFsConfig {
    fn default() -> Self {
        Self::none()
    }
}

/// Metrics tracking for filesystem operations
#[derive(Debug, Clone, Default)]
pub struct BackendMetrics {
    /// Number of read_dir calls
    pub read_dir_calls: usize,
    /// Number of get_metadata_batch calls
    pub metadata_batch_calls: usize,
    /// Number of individual metadata items fetched
    pub metadata_items: usize,
    /// Number of exists calls
    pub exists_calls: usize,
    /// Number of is_dir calls
    pub is_dir_calls: usize,
    /// Number of get_entry calls
    pub get_entry_calls: usize,
    /// Number of canonicalize calls
    pub canonicalize_calls: usize,
    /// Total time spent in artificial delays
    pub total_delay_time: Duration,
}

impl BackendMetrics {
    /// Create new empty metrics
    pub fn new() -> Self {
        Self::default()
    }

    /// Reset all metrics to zero
    pub fn reset(&mut self) {
        *self = Self::default();
    }

    /// Get total number of filesystem calls
    pub fn total_calls(&self) -> usize {
        self.read_dir_calls
            + self.metadata_batch_calls
            + self.exists_calls
            + self.is_dir_calls
            + self.get_entry_calls
            + self.canonicalize_calls
    }
}

/// Slow filesystem backend wrapper for testing
///
/// Wraps any FsBackend implementation and adds configurable delays to each
/// operation. Also tracks metrics about operation counts and timing.
pub struct SlowFsBackend {
    /// The underlying real backend
    inner: Arc<dyn FsBackend>,
    /// Configuration for delays
    config: SlowFsConfig,
    /// Metrics tracking
    metrics: Arc<Mutex<BackendMetrics>>,
}

impl SlowFsBackend {
    /// Create a new slow filesystem backend
    pub fn new(inner: Arc<dyn FsBackend>, config: SlowFsConfig) -> Self {
        Self {
            inner,
            config,
            metrics: Arc::new(Mutex::new(BackendMetrics::new())),
        }
    }

    /// Create with uniform delay for all operations
    pub fn with_uniform_delay(inner: Arc<dyn FsBackend>, delay: Duration) -> Self {
        Self::new(inner, SlowFsConfig::uniform(delay))
    }

    /// Get a snapshot of current metrics
    pub async fn metrics(&self) -> BackendMetrics {
        self.metrics.lock().await.clone()
    }

    /// Reset metrics to zero
    pub async fn reset_metrics(&self) {
        self.metrics.lock().await.reset();
    }

    /// Get a clone of the metrics Arc for sharing
    pub fn metrics_arc(&self) -> Arc<Mutex<BackendMetrics>> {
        Arc::clone(&self.metrics)
    }

    /// Add delay and update metrics
    async fn add_delay(&self, delay: Duration) {
        if !delay.is_zero() {
            tokio::time::sleep(delay).await;
            self.metrics.lock().await.total_delay_time += delay;
        }
    }
}

#[async_trait]
impl FsBackend for SlowFsBackend {
    async fn read_dir(&self, path: &Path) -> io::Result<Vec<FsEntry>> {
        self.add_delay(self.config.read_dir_delay).await;
        self.metrics.lock().await.read_dir_calls += 1;
        self.inner.read_dir(path).await
    }

    async fn get_metadata_batch(&self, paths: &[PathBuf]) -> Vec<io::Result<FsMetadata>> {
        // Add delay per item to simulate slow stat operations
        let total_delay = self.config.metadata_delay * paths.len() as u32;
        self.add_delay(total_delay).await;

        let mut metrics = self.metrics.lock().await;
        metrics.metadata_batch_calls += 1;
        metrics.metadata_items += paths.len();
        drop(metrics);

        self.inner.get_metadata_batch(paths).await
    }

    async fn exists(&self, path: &Path) -> bool {
        self.add_delay(self.config.exists_delay).await;
        self.metrics.lock().await.exists_calls += 1;
        self.inner.exists(path).await
    }

    async fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.add_delay(self.config.is_dir_delay).await;
        self.metrics.lock().await.is_dir_calls += 1;
        self.inner.is_dir(path).await
    }

    async fn get_entry(&self, path: &Path) -> io::Result<FsEntry> {
        self.add_delay(self.config.get_entry_delay).await;
        self.metrics.lock().await.get_entry_calls += 1;
        self.inner.get_entry(path).await
    }

    async fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.add_delay(self.config.canonicalize_delay).await;
        self.metrics.lock().await.canonicalize_calls += 1;
        self.inner.canonicalize(path).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::fs::LocalFsBackend;
    use std::time::Instant;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_slow_backend_adds_delay() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        let local = Arc::new(LocalFsBackend::new());
        let slow_config = SlowFsConfig::uniform(Duration::from_millis(100));
        let slow = SlowFsBackend::new(local, slow_config);

        let start = Instant::now();
        let _ = slow.read_dir(temp_path).await;
        let elapsed = start.elapsed();

        // Should take at least 100ms due to artificial delay
        assert!(
            elapsed >= Duration::from_millis(100),
            "Expected at least 100ms delay, got {:?}",
            elapsed
        );

        // Check metrics
        let metrics = slow.metrics().await;
        assert_eq!(metrics.read_dir_calls, 1);
        assert!(metrics.total_delay_time >= Duration::from_millis(100));
    }

    #[tokio::test]
    async fn test_metrics_tracking() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        let local = Arc::new(LocalFsBackend::new());
        let slow = SlowFsBackend::new(local, SlowFsConfig::none());

        // Perform various operations
        let _ = slow.read_dir(temp_path).await;
        let _ = slow.exists(temp_path).await;
        let _ = slow.is_dir(temp_path).await;

        let metrics = slow.metrics().await;
        assert_eq!(metrics.read_dir_calls, 1);
        assert_eq!(metrics.exists_calls, 1);
        assert_eq!(metrics.is_dir_calls, 1);
        assert_eq!(metrics.total_calls(), 3);
    }

    #[tokio::test]
    async fn test_metadata_batch_delay() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create test files
        std::fs::write(temp_path.join("file1.txt"), "test").unwrap();
        std::fs::write(temp_path.join("file2.txt"), "test").unwrap();

        let local = Arc::new(LocalFsBackend::new());
        let slow_config = SlowFsConfig {
            metadata_delay: Duration::from_millis(50),
            ..SlowFsConfig::none()
        };
        let slow = SlowFsBackend::new(local, slow_config);

        let paths = vec![temp_path.join("file1.txt"), temp_path.join("file2.txt")];

        let start = Instant::now();
        let _ = slow.get_metadata_batch(&paths).await;
        let elapsed = start.elapsed();

        // Should take at least 100ms (50ms * 2 files)
        assert!(
            elapsed >= Duration::from_millis(100),
            "Expected at least 100ms delay, got {:?}",
            elapsed
        );

        let metrics = slow.metrics().await;
        assert_eq!(metrics.metadata_batch_calls, 1);
        assert_eq!(metrics.metadata_items, 2);
    }

    #[tokio::test]
    async fn test_reset_metrics() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        let local = Arc::new(LocalFsBackend::new());
        let slow = SlowFsBackend::new(local, SlowFsConfig::none());

        // Perform some operations
        let _ = slow.read_dir(temp_path).await;
        let _ = slow.exists(temp_path).await;

        // Verify metrics are non-zero
        let metrics_before = slow.metrics().await;
        assert!(metrics_before.total_calls() > 0);

        // Reset
        slow.reset_metrics().await;

        // Verify metrics are zero
        let metrics_after = slow.metrics().await;
        assert_eq!(metrics_after.total_calls(), 0);
    }

    #[tokio::test]
    async fn test_preset_configs() {
        let local = Arc::new(LocalFsBackend::new());

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
        let _slow_network = SlowFsBackend::new(local.clone(), network_config);
        let _slow_disk = SlowFsBackend::new(local.clone(), disk_config);
        let _no_delay = SlowFsBackend::new(local, none_config);
    }
}
