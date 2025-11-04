use crate::persistence::PersistenceLayer;
use std::collections::BTreeMap;
use std::io;

/// Cache for loaded regions of the buffer
/// Uses a BTreeMap to store regions and implements simple LRU eviction
pub struct Cache {
    /// Map of start_offset -> (data, access_count)
    regions: BTreeMap<usize, (Vec<u8>, u64)>,
    /// Maximum cache size in bytes
    max_size: usize,
    /// Current cache size in bytes
    current_size: usize,
    /// Access counter for LRU tracking
    access_counter: u64,
}

impl Cache {
    /// Create a new cache with the given maximum size
    pub fn new(max_size: usize) -> Self {
        Self {
            regions: BTreeMap::new(),
            max_size,
            current_size: 0,
            access_counter: 0,
        }
    }

    /// Read bytes from the cache if available
    /// Returns None if the requested range is not fully cached
    pub fn read(&mut self, offset: usize, len: usize) -> Option<Vec<u8>> {
        // Find the region containing this offset
        let region_start = self.regions.range(..=offset).next_back().map(|(k, _)| *k)?;

        let (data, access_count) = self.regions.get_mut(&region_start)?;

        // Check if the region contains the full requested range
        let region_offset = offset - region_start;
        if region_offset + len > data.len() {
            return None;
        }

        // Update access count
        self.access_counter += 1;
        *access_count = self.access_counter;

        Some(data[region_offset..region_offset + len].to_vec())
    }

    /// Write data to the cache at the given offset
    /// This will evict old entries if needed to stay under max_size
    pub fn write(&mut self, offset: usize, data: Vec<u8>) {
        let data_len = data.len();

        // Remove old entry at this offset if it exists
        if let Some((old_data, _)) = self.regions.remove(&offset) {
            self.current_size -= old_data.len();
        }

        // Evict if necessary
        while self.current_size + data_len > self.max_size && !self.regions.is_empty() {
            self.evict_one();
        }

        // Insert new entry
        self.access_counter += 1;
        self.regions.insert(offset, (data, self.access_counter));
        self.current_size += data_len;
    }

    /// Ensure the requested range is cached
    /// If not cached, load from persistence
    pub fn ensure_cached<P: PersistenceLayer + ?Sized>(
        &mut self,
        persistence: &P,
        offset: usize,
        len: usize,
    ) -> io::Result<()> {
        // Check if already cached
        if self.read(offset, len).is_some() {
            return Ok(());
        }

        // Load from persistence
        // We load a larger chunk for better cache utilization
        const CHUNK_SIZE: usize = 4096;
        let chunk_start = (offset / CHUNK_SIZE) * CHUNK_SIZE;
        let chunk_end = ((offset + len + CHUNK_SIZE - 1) / CHUNK_SIZE) * CHUNK_SIZE;
        let chunk_len = chunk_end - chunk_start;

        let data = persistence.read(chunk_start, chunk_len)?;
        if !data.is_empty() {
            self.write(chunk_start, data);
        }

        Ok(())
    }

    /// Evict the least recently used entry
    fn evict_one(&mut self) {
        if self.regions.is_empty() {
            return;
        }

        // Find entry with lowest access count
        let (&offset_to_evict, _) = self
            .regions
            .iter()
            .min_by_key(|(_, (_, access_count))| access_count)
            .unwrap();

        if let Some((data, _)) = self.regions.remove(&offset_to_evict) {
            self.current_size -= data.len();
        }
    }

    /// Invalidate all cached regions
    pub fn clear(&mut self) {
        self.regions.clear();
        self.current_size = 0;
    }

    /// Get current cache size in bytes
    pub fn size(&self) -> usize {
        self.current_size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_basic() {
        let mut cache = Cache::new(1024);

        // Write some data
        cache.write(0, b"hello".to_vec());
        assert_eq!(cache.size(), 5);

        // Read it back
        let data = cache.read(0, 5).unwrap();
        assert_eq!(data, b"hello");
    }

    #[test]
    fn test_cache_eviction() {
        let mut cache = Cache::new(10);

        // Fill cache
        cache.write(0, b"hello".to_vec());
        cache.write(10, b"world".to_vec());
        assert_eq!(cache.size(), 10);

        // This should trigger eviction - need to evict at least 7 bytes to fit the new entry
        // Both "hello" and "world" will be evicted (10 bytes total) to fit "!!!!!!!" (7 bytes)
        cache.write(20, b"!!!!!!!".to_vec());

        // First two entries should be evicted
        assert!(cache.read(0, 5).is_none());
        assert!(cache.read(10, 5).is_none());
        // Third entry should be there
        assert!(cache.read(20, 7).is_some());
    }

    #[test]
    fn test_cache_partial_read() {
        let mut cache = Cache::new(1024);

        cache.write(0, b"hello world".to_vec());

        // Read partial range
        let data = cache.read(0, 5).unwrap();
        assert_eq!(data, b"hello");

        let data = cache.read(6, 5).unwrap();
        assert_eq!(data, b"world");
    }
}
