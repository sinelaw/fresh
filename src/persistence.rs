use crate::chunk_tree::{ChunkTree, ChunkTreeConfig};
use std::io;
use std::ops::Range;

/// Trait for pluggable persistence backends
/// Implementations provide the actual storage mechanism for the virtual buffer
pub trait PersistenceLayer: Send {
    /// Read bytes from the storage
    fn read(&self, offset: usize, len: usize) -> io::Result<Vec<u8>>;

    /// Insert bytes at the given offset
    fn insert(&mut self, offset: usize, data: &[u8]) -> io::Result<()>;

    /// Delete bytes in the given range
    fn delete(&mut self, range: Range<usize>) -> io::Result<()>;

    /// Get the total length of stored data
    fn len(&self) -> usize;

    /// Check if the storage is empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get a snapshot of the underlying ChunkTree for efficient iteration
    /// Returns None if the implementation doesn't support ChunkTree-based iteration
    fn get_chunk_tree_snapshot(&self) -> Option<ChunkTree<'static>> {
        None
    }
}

/// ChunkTree-based persistence layer
/// Wraps the existing ChunkTree as a storage backend
pub struct ChunkTreePersistence {
    tree: ChunkTree<'static>,
}

impl ChunkTreePersistence {
    /// Create a new empty persistence layer
    pub fn new(config: ChunkTreeConfig) -> Self {
        Self {
            tree: ChunkTree::new(config),
        }
    }

    /// Create from existing data
    pub fn from_data(data: &'static [u8], config: ChunkTreeConfig) -> Self {
        Self {
            tree: ChunkTree::from_slice(data, config),
        }
    }

    /// Create from ChunkTree
    pub fn from_tree(tree: ChunkTree<'static>) -> Self {
        Self { tree }
    }
}

impl PersistenceLayer for ChunkTreePersistence {
    fn read(&self, offset: usize, len: usize) -> io::Result<Vec<u8>> {
        if offset >= self.tree.len() {
            return Ok(Vec::new());
        }

        let actual_len = len.min(self.tree.len() - offset);
        let mut iter = self.tree.bytes_at(offset);
        let mut bytes = Vec::with_capacity(actual_len);

        for _ in 0..actual_len {
            if let Some(byte) = iter.next() {
                bytes.push(byte);
            } else {
                break;
            }
        }

        Ok(bytes)
    }

    fn insert(&mut self, offset: usize, data: &[u8]) -> io::Result<()> {
        // We need to convert data to 'static lifetime
        // This is safe because we're copying the data into the tree
        let static_data: &'static [u8] = Box::leak(data.to_vec().into_boxed_slice());
        self.tree = self.tree.insert(offset, static_data);
        Ok(())
    }

    fn delete(&mut self, range: Range<usize>) -> io::Result<()> {
        self.tree = self.tree.remove(range);
        Ok(())
    }

    fn len(&self) -> usize {
        self.tree.len()
    }

    fn get_chunk_tree_snapshot(&self) -> Option<ChunkTree<'static>> {
        // ChunkTree is cheap to clone due to Arc-based sharing
        Some(self.tree.clone())
    }
}

/// Slow persistence layer wrapper for testing
///
/// Wraps any PersistenceLayer implementation and adds configurable delays
/// to simulate slow storage operations. Useful for testing editor responsiveness
/// with slow disks, network storage, etc.
#[cfg(test)]
pub struct SlowPersistenceLayer<T: PersistenceLayer> {
    inner: T,
    config: SlowPersistenceConfig,
    metrics: PersistenceMetrics,
}

#[cfg(test)]
#[derive(Debug, Clone)]
pub struct SlowPersistenceConfig {
    /// Delay for read operations (per byte read)
    pub read_delay_per_byte: std::time::Duration,
    /// Fixed delay for read operations
    pub read_delay_fixed: std::time::Duration,
    /// Delay for insert operations (per byte inserted)
    pub insert_delay_per_byte: std::time::Duration,
    /// Fixed delay for insert operations
    pub insert_delay_fixed: std::time::Duration,
    /// Delay for delete operations (per byte deleted)
    pub delete_delay_per_byte: std::time::Duration,
    /// Fixed delay for delete operations
    pub delete_delay_fixed: std::time::Duration,
}

#[cfg(test)]
impl SlowPersistenceConfig {
    /// Create a config with uniform fixed delay for all operations
    pub fn uniform(delay: std::time::Duration) -> Self {
        Self {
            read_delay_per_byte: std::time::Duration::ZERO,
            read_delay_fixed: delay,
            insert_delay_per_byte: std::time::Duration::ZERO,
            insert_delay_fixed: delay,
            delete_delay_per_byte: std::time::Duration::ZERO,
            delete_delay_fixed: delay,
        }
    }

    /// Create a config with no delays
    pub fn none() -> Self {
        Self::uniform(std::time::Duration::ZERO)
    }

    /// Create a config simulating slow disk with delays proportional to data size
    pub fn slow_disk() -> Self {
        Self {
            read_delay_per_byte: std::time::Duration::from_nanos(10),
            read_delay_fixed: std::time::Duration::from_millis(5),
            insert_delay_per_byte: std::time::Duration::from_nanos(20),
            insert_delay_fixed: std::time::Duration::from_millis(10),
            delete_delay_per_byte: std::time::Duration::from_nanos(15),
            delete_delay_fixed: std::time::Duration::from_millis(8),
        }
    }

    /// Create a config simulating very slow network storage
    pub fn slow_network() -> Self {
        Self {
            read_delay_per_byte: std::time::Duration::from_nanos(50),
            read_delay_fixed: std::time::Duration::from_millis(100),
            insert_delay_per_byte: std::time::Duration::from_nanos(100),
            insert_delay_fixed: std::time::Duration::from_millis(200),
            delete_delay_per_byte: std::time::Duration::from_nanos(75),
            delete_delay_fixed: std::time::Duration::from_millis(150),
        }
    }
}

#[cfg(test)]
impl Default for SlowPersistenceConfig {
    fn default() -> Self {
        Self::none()
    }
}

#[cfg(test)]
#[derive(Debug, Clone, Default)]
pub struct PersistenceMetrics {
    /// Number of read calls
    pub read_calls: usize,
    /// Total bytes read
    pub bytes_read: usize,
    /// Number of insert calls
    pub insert_calls: usize,
    /// Total bytes inserted
    pub bytes_inserted: usize,
    /// Number of delete calls
    pub delete_calls: usize,
    /// Total bytes deleted
    pub bytes_deleted: usize,
    /// Total time spent in artificial delays
    pub total_delay_time: std::time::Duration,
}

#[cfg(test)]
impl PersistenceMetrics {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn reset(&mut self) {
        *self = Self::default();
    }

    pub fn total_operations(&self) -> usize {
        self.read_calls + self.insert_calls + self.delete_calls
    }
}

#[cfg(test)]
impl<T: PersistenceLayer> SlowPersistenceLayer<T> {
    /// Create a new slow persistence layer
    pub fn new(inner: T, config: SlowPersistenceConfig) -> Self {
        Self {
            inner,
            config,
            metrics: PersistenceMetrics::new(),
        }
    }

    /// Create with uniform delay for all operations
    pub fn with_uniform_delay(inner: T, delay: std::time::Duration) -> Self {
        Self::new(inner, SlowPersistenceConfig::uniform(delay))
    }

    /// Get a snapshot of current metrics
    pub fn metrics(&self) -> &PersistenceMetrics {
        &self.metrics
    }

    /// Get a mutable reference to metrics
    pub fn metrics_mut(&mut self) -> &mut PersistenceMetrics {
        &mut self.metrics
    }

    /// Reset metrics to zero
    pub fn reset_metrics(&mut self) {
        self.metrics.reset();
    }

    /// Add delay based on operation size
    fn add_delay(&mut self, fixed: std::time::Duration, per_byte: std::time::Duration, size: usize) {
        let total_delay = fixed + per_byte * size as u32;
        if !total_delay.is_zero() {
            // Note: In real async context, this would be tokio::time::sleep
            // For now we'll just track the delay time in metrics
            // The actual sleep happens during buffer operations
            std::thread::sleep(total_delay);
            self.metrics.total_delay_time += total_delay;
        }
    }

    /// Unwrap to get the inner persistence layer
    pub fn into_inner(self) -> T {
        self.inner
    }
}

#[cfg(test)]
impl<T: PersistenceLayer> PersistenceLayer for SlowPersistenceLayer<T> {
    fn read(&self, offset: usize, len: usize) -> io::Result<Vec<u8>> {
        // Note: We can't add delay in a non-mut method, so we track but don't sleep
        // In practice, the buffer operations would handle this
        let result = self.inner.read(offset, len);
        // Metrics update would happen in a mut context
        result
    }

    fn insert(&mut self, offset: usize, data: &[u8]) -> io::Result<()> {
        let size = data.len();
        self.add_delay(
            self.config.insert_delay_fixed,
            self.config.insert_delay_per_byte,
            size,
        );
        self.metrics.insert_calls += 1;
        self.metrics.bytes_inserted += size;
        self.inner.insert(offset, data)
    }

    fn delete(&mut self, range: Range<usize>) -> io::Result<()> {
        let size = range.end.saturating_sub(range.start);
        self.add_delay(
            self.config.delete_delay_fixed,
            self.config.delete_delay_per_byte,
            size,
        );
        self.metrics.delete_calls += 1;
        self.metrics.bytes_deleted += size;
        self.inner.delete(range)
    }

    fn len(&self) -> usize {
        self.inner.len()
    }

    fn get_chunk_tree_snapshot(&self) -> Option<ChunkTree<'static>> {
        self.inner.get_chunk_tree_snapshot()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DEFAULT_CONFIG: ChunkTreeConfig = ChunkTreeConfig::new(4096, 8);

    #[test]
    fn test_chunktree_persistence_basic() {
        let mut persistence = ChunkTreePersistence::new(DEFAULT_CONFIG);

        // Test insert
        persistence.insert(0, b"hello").unwrap();
        assert_eq!(persistence.len(), 5);

        // Test read
        let data = persistence.read(0, 5).unwrap();
        assert_eq!(data, b"hello");
    }

    #[test]
    fn test_chunktree_persistence_operations() {
        let mut persistence = ChunkTreePersistence::new(DEFAULT_CONFIG);

        persistence.insert(0, b"hello world").unwrap();

        // Insert in middle
        persistence.insert(5, b" beautiful").unwrap();
        let data = persistence.read(0, persistence.len()).unwrap();
        assert_eq!(data, b"hello beautiful world");

        // Delete
        persistence.delete(5..15).unwrap();
        let data = persistence.read(0, persistence.len()).unwrap();
        assert_eq!(data, b"hello world");
    }

    #[test]
    fn test_slow_persistence_basic() {
        let inner = ChunkTreePersistence::new(DEFAULT_CONFIG);
        let mut slow = SlowPersistenceLayer::new(inner, SlowPersistenceConfig::none());

        // Test insert
        slow.insert(0, b"hello").unwrap();
        assert_eq!(slow.len(), 5);

        // Check metrics
        assert_eq!(slow.metrics().insert_calls, 1);
        assert_eq!(slow.metrics().bytes_inserted, 5);
    }

    #[test]
    fn test_slow_persistence_adds_delay() {
        let inner = ChunkTreePersistence::new(DEFAULT_CONFIG);
        let config = SlowPersistenceConfig::uniform(std::time::Duration::from_millis(50));
        let mut slow = SlowPersistenceLayer::new(inner, config);

        let start = std::time::Instant::now();
        slow.insert(0, b"test").unwrap();
        let elapsed = start.elapsed();

        // Should take at least 50ms due to artificial delay
        assert!(
            elapsed >= std::time::Duration::from_millis(50),
            "Expected at least 50ms delay, got {:?}",
            elapsed
        );
    }

    #[test]
    fn test_slow_persistence_metrics() {
        let inner = ChunkTreePersistence::new(DEFAULT_CONFIG);
        let mut slow = SlowPersistenceLayer::new(inner, SlowPersistenceConfig::none());

        slow.insert(0, b"hello").unwrap();
        slow.insert(5, b" world").unwrap();
        slow.delete(0..5).unwrap();

        let metrics = slow.metrics();
        assert_eq!(metrics.insert_calls, 2);
        assert_eq!(metrics.bytes_inserted, 11); // 5 + 6
        assert_eq!(metrics.delete_calls, 1);
        assert_eq!(metrics.bytes_deleted, 5);
        assert_eq!(metrics.total_operations(), 3);
    }

    #[test]
    fn test_slow_persistence_presets() {
        // Test slow_disk preset
        let disk_config = SlowPersistenceConfig::slow_disk();
        let inner1 = ChunkTreePersistence::new(DEFAULT_CONFIG);
        let _slow_disk = SlowPersistenceLayer::new(inner1, disk_config);

        // Test slow_network preset
        let network_config = SlowPersistenceConfig::slow_network();
        let inner2 = ChunkTreePersistence::new(DEFAULT_CONFIG);
        let _slow_network = SlowPersistenceLayer::new(inner2, network_config);

        // Test none preset
        let none_config = SlowPersistenceConfig::none();
        let inner3 = ChunkTreePersistence::new(DEFAULT_CONFIG);
        let _no_delay = SlowPersistenceLayer::new(inner3, none_config);
    }
}
