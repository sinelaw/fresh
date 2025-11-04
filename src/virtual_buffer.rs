use crate::cache::Cache;
use crate::edit::{Edit, EditKind};
use crate::persistence::PersistenceLayer;
use std::collections::{BTreeSet, HashMap};
use std::io;
use std::ops::Range;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, RwLock};

/// Shared, internally-mutable state for the virtual buffer
struct InnerBuffer {
    /// Pluggable persistence backend, mutex-protected for writes
    persistence: Mutex<Box<dyn PersistenceLayer>>,

    /// Cache for loaded regions, mutex-protected for reads/writes
    cache: Mutex<Cache>,

    /// Log of edits, read-write locked
    /// (Many iterators read, one edit operation writes)
    edit_log: RwLock<Vec<Edit>>,

    /// Version counter, atomic for lock-free increments
    edit_version: AtomicU64,

    /// Tracks all active iterators' versions for GC
    /// BTreeSet makes finding the minimum (oldest) version fast
    active_iterator_versions: Mutex<BTreeSet<u64>>,
}

/// The public-facing virtual buffer handle
/// Lightweight and cloneable - all state is shared via Arc
#[derive(Clone)]
pub struct VirtualBuffer {
    inner: Arc<InnerBuffer>,
}

impl VirtualBuffer {
    /// Create a new virtual buffer with the given persistence layer
    pub fn new(persistence: Box<dyn PersistenceLayer>) -> Self {
        const DEFAULT_CACHE_SIZE: usize = 16 * 1024 * 1024; // 16MB cache

        Self {
            inner: Arc::new(InnerBuffer {
                persistence: Mutex::new(persistence),
                cache: Mutex::new(Cache::new(DEFAULT_CACHE_SIZE)),
                edit_log: RwLock::new(Vec::new()),
                edit_version: AtomicU64::new(0),
                active_iterator_versions: Mutex::new(BTreeSet::new()),
            }),
        }
    }

    /// Read bytes from the buffer
    pub fn read(&self, offset: usize, len: usize) -> io::Result<Vec<u8>> {
        // Try cache first
        {
            let mut cache = self.inner.cache.lock().unwrap();
            if let Some(data) = cache.read(offset, len) {
                return Ok(data);
            }
        }

        // Cache miss - load from persistence
        let persistence = self.inner.persistence.lock().unwrap();
        let data = persistence.read(offset, len)?;

        // Update cache
        if !data.is_empty() {
            let mut cache = self.inner.cache.lock().unwrap();
            cache.write(offset, data.clone());
        }

        Ok(data)
    }

    /// Insert bytes at the given offset
    pub fn insert(&self, offset: usize, data: &[u8]) -> io::Result<()> {
        if data.is_empty() {
            return Ok(());
        }

        // Update persistence and cache
        {
            let mut persistence = self.inner.persistence.lock().unwrap();
            persistence.insert(offset, data)?;
        }

        {
            let mut cache = self.inner.cache.lock().unwrap();
            // Invalidate cache for simplicity - could be smarter here
            cache.clear();
        }

        // Get new version and create edit
        let new_version = self.inner.edit_version.fetch_add(1, Ordering::SeqCst) + 1;
        let edit = Edit::insert(new_version, offset, data.len());

        // Add to edit log
        {
            let mut edit_log = self.inner.edit_log.write().unwrap();
            edit_log.push(edit);
        }

        // Prune old edits
        self.prune_edit_log();

        Ok(())
    }

    /// Delete bytes in the given range
    pub fn delete(&self, range: Range<usize>) -> io::Result<()> {
        if range.is_empty() {
            return Ok(());
        }

        let len = range.end - range.start;

        // Update persistence and cache
        {
            let mut persistence = self.inner.persistence.lock().unwrap();
            persistence.delete(range.clone())?;
        }

        {
            let mut cache = self.inner.cache.lock().unwrap();
            // Invalidate cache for simplicity - could be smarter here
            cache.clear();
        }

        // Get new version and create edit
        let new_version = self.inner.edit_version.fetch_add(1, Ordering::SeqCst) + 1;
        let edit = Edit::delete(new_version, range.start, len);

        // Add to edit log
        {
            let mut edit_log = self.inner.edit_log.write().unwrap();
            edit_log.push(edit);
        }

        // Prune old edits
        self.prune_edit_log();

        Ok(())
    }

    /// Get the total length of the buffer
    pub fn len(&self) -> usize {
        let persistence = self.inner.persistence.lock().unwrap();
        persistence.len()
    }

    /// Check if the buffer is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Create an iterator at the given position
    pub fn iter_at(&self, position: usize) -> ByteIterator {
        let current_version = self.inner.edit_version.load(Ordering::Relaxed);

        // Register this new iterator's version
        self.inner
            .active_iterator_versions
            .lock()
            .unwrap()
            .insert(current_version);

        // Try to get a ChunkTree snapshot for efficient iteration
        let tree_snapshot = {
            let persistence = self.inner.persistence.lock().unwrap();
            persistence.get_chunk_tree_snapshot()
        };

        ByteIterator {
            buffer: self.inner.clone(),
            position,
            version_at_creation: current_version,
            tree_snapshot,
            chunk_cache: HashMap::new(),
        }
    }

    /// Prune edit log based on oldest active iterator
    fn prune_edit_log(&self) {
        let versions = self.inner.active_iterator_versions.lock().unwrap();

        // Find the oldest iterator version still in use
        let low_water_mark = versions.iter().next().cloned();

        if let Some(oldest_version) = low_water_mark {
            let mut edit_log = self.inner.edit_log.write().unwrap();

            // Find index of first edit to keep
            let first_index_to_keep = edit_log
                .binary_search_by_key(&oldest_version, |e| e.version)
                .unwrap_or_else(|e| e);

            // Drain all edits before that version
            edit_log.drain(..first_index_to_keep);
        }
        // If no iterators exist, keep edits for potential undo
    }
}

/// Bidirectional byte iterator with automatic edit adjustment
pub struct ByteIterator {
    /// Shared reference to inner buffer
    buffer: Arc<InnerBuffer>,

    /// Current position in the buffer (adjusted for edits)
    position: usize,

    /// Track what version this iterator has "caught up" to
    version_at_creation: u64,

    /// Optional ChunkTree snapshot for efficient iteration
    /// When available, we read chunks from it
    tree_snapshot: Option<crate::chunk_tree::ChunkTree<'static>>,

    /// Cache of loaded chunks indexed by their start offset
    /// Maps start_offset => chunk_data
    chunk_cache: HashMap<usize, Vec<u8>>,
}

impl ByteIterator {
    /// Get the next byte, advancing forward
    pub fn next(&mut self) -> Option<u8> {
        // Check for edits and adjust position if needed
        // This is fast when no edits: just an atomic load
        self.adjust_for_edits();

        // Lazy refresh: only get new snapshot when needed
        if self.tree_snapshot.is_none() {
            let persistence = self.buffer.persistence.lock().unwrap();
            if let Some(tree) = persistence.get_chunk_tree_snapshot() {
                self.tree_snapshot = Some(tree);
            }
        }

        // Fast path: Check if we have a cached chunk containing this position
        const CHUNK_SIZE: usize = 4096;
        let chunk_start = (self.position / CHUNK_SIZE) * CHUNK_SIZE;

        if let Some(chunk_data) = self.chunk_cache.get(&chunk_start) {
            let offset = self.position - chunk_start;
            if offset < chunk_data.len() {
                let byte = chunk_data[offset];
                self.position += 1;
                return Some(byte);
            }
        }

        // Need to load new chunk
        if let Some(tree) = &self.tree_snapshot {
            let mut iter = tree.bytes_at(chunk_start);
            let mut chunk_data = Vec::with_capacity(CHUNK_SIZE);

            for _ in 0..CHUNK_SIZE {
                if let Some(byte) = iter.next() {
                    chunk_data.push(byte);
                } else {
                    break;
                }
            }

            if !chunk_data.is_empty() {
                let offset = self.position - chunk_start;
                if offset < chunk_data.len() {
                    let byte = chunk_data[offset];
                    self.chunk_cache.insert(chunk_start, chunk_data);
                    self.position += 1;
                    return Some(byte);
                }
            }
        }

        // No ChunkTree snapshot available or at end of buffer
        None
    }

    /// Get the previous byte, moving backward
    pub fn prev(&mut self) -> Option<u8> {
        // Check for edits and adjust position if needed
        self.adjust_for_edits();

        if self.position == 0 {
            return None;
        }

        self.position -= 1;

        // Lazy refresh: only get new snapshot when needed
        if self.tree_snapshot.is_none() {
            let persistence = self.buffer.persistence.lock().unwrap();
            if let Some(tree) = persistence.get_chunk_tree_snapshot() {
                self.tree_snapshot = Some(tree);
            }
        }

        // Fast path: Check if we have a cached chunk containing this position
        const CHUNK_SIZE: usize = 4096;
        let chunk_start = (self.position / CHUNK_SIZE) * CHUNK_SIZE;

        if let Some(chunk_data) = self.chunk_cache.get(&chunk_start) {
            let offset = self.position - chunk_start;
            if offset < chunk_data.len() {
                return Some(chunk_data[offset]);
            }
        }

        // Need to load chunk containing this position
        if let Some(tree) = &self.tree_snapshot {
            let mut iter = tree.bytes_at(chunk_start);
            let mut chunk_data = Vec::with_capacity(CHUNK_SIZE);

            for _ in 0..CHUNK_SIZE {
                if let Some(byte) = iter.next() {
                    chunk_data.push(byte);
                } else {
                    break;
                }
            }

            if !chunk_data.is_empty() {
                let offset = self.position - chunk_start;
                if offset < chunk_data.len() {
                    let byte = chunk_data[offset];
                    self.chunk_cache.insert(chunk_start, chunk_data);
                    return Some(byte);
                }
            }
        }

        // No ChunkTree snapshot available
        None
    }

    /// Peek at the current byte without advancing
    pub fn peek(&self) -> Option<u8> {
        // Fast path: Use ChunkTree iterator if available
        if let Some(tree) = &self.tree_snapshot {
            let iter = tree.bytes_at(self.position);
            return iter.peek();
        }

        None
    }

    /// Seek to a specific position
    pub fn seek(&mut self, position: usize) {
        self.adjust_for_edits();
        self.position = position;
    }

    /// Get the current position
    pub fn position(&self) -> usize {
        self.position
    }

    /// Get the buffer length (for bounds checking)
    pub fn buffer_len(&self) -> usize {
        let persistence = self.buffer.persistence.lock().unwrap();
        persistence.len()
    }

    /// Adjust position based on edits that occurred since creation
    fn adjust_for_edits(&mut self) {
        let current_version = self.buffer.edit_version.load(Ordering::Relaxed);
        if self.version_at_creation == current_version {
            return; // Fast path: Already up-to-date, no edits
        }

        // Get read lock on edit log
        let edit_log = self.buffer.edit_log.read().unwrap();

        // Find first edit after our version
        let first_edit_index = edit_log
            .binary_search_by_key(&self.version_at_creation, |e| e.version)
            .map(|i| i + 1) // We want edits after our version
            .unwrap_or_else(|e| e); // e is insertion point for our version

        // Apply all edits since version_at_creation to adjust position
        for edit in &edit_log[first_edit_index..] {
            match edit.kind {
                EditKind::Insert { offset, len } if offset <= self.position => {
                    self.position += len;
                }
                EditKind::Delete { offset, len } if offset <= self.position => {
                    self.position = self.position.saturating_sub(len);
                }
                _ => {}
            }
        }

        // CRITICAL: Invalidate the ChunkTree snapshot since edits occurred
        // The next next()/prev() call will get a fresh snapshot
        self.tree_snapshot = None;
        self.chunk_cache.clear();

        // Update version tracking for GC
        let mut versions = self.buffer.active_iterator_versions.lock().unwrap();
        versions.remove(&self.version_at_creation);
        self.version_at_creation = current_version;
        versions.insert(self.version_at_creation);
    }
}

/// Implement Drop to unregister the iterator
impl Drop for ByteIterator {
    fn drop(&mut self) {
        // Remove this iterator's version from the active set
        self.buffer
            .active_iterator_versions
            .lock()
            .unwrap()
            .remove(&self.version_at_creation);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk_tree::ChunkTreeConfig;
    use crate::persistence::ChunkTreePersistence;

    const DEFAULT_CONFIG: ChunkTreeConfig = ChunkTreeConfig::new(4096, 8);

    #[test]
    fn test_virtual_buffer_basic() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"hello world".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        // Test read
        assert_eq!(vbuf.read(0, 5).unwrap(), b"hello");
        assert_eq!(vbuf.len(), 11);
    }

    #[test]
    fn test_virtual_buffer_insert() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"hello world".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        // Test insert
        vbuf.insert(5, b" beautiful").unwrap();
        let data = vbuf.read(0, vbuf.len()).unwrap();
        assert_eq!(data, b"hello beautiful world");
    }

    #[test]
    fn test_virtual_buffer_delete() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"hello world".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        // Test delete
        vbuf.delete(5..11).unwrap();
        let data = vbuf.read(0, vbuf.len()).unwrap();
        assert_eq!(data, b"hello");
    }

    #[test]
    fn test_byte_iterator() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"hello".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        let mut iter = vbuf.iter_at(0);
        assert_eq!(iter.next(), Some(b'h'));
        assert_eq!(iter.next(), Some(b'e'));
        assert_eq!(iter.next(), Some(b'l'));
        assert_eq!(iter.next(), Some(b'l'));
        assert_eq!(iter.next(), Some(b'o'));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_byte_iterator_bidirectional() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"hello".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        let mut iter = vbuf.iter_at(2);
        assert_eq!(iter.next(), Some(b'l'));
        assert_eq!(iter.prev(), Some(b'l'));
        assert_eq!(iter.prev(), Some(b'e'));
        assert_eq!(iter.prev(), Some(b'h'));
        assert_eq!(iter.prev(), None);
    }

    #[test]
    fn test_iterator_edit_adjustment() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"hello world".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        // Create iterator at position 6 (start of "world")
        let mut iter = vbuf.iter_at(6);

        // Insert before iterator position
        vbuf.insert(5, b" beautiful").unwrap();

        // Iterator should adjust its position
        assert_eq!(iter.next(), Some(b'w'));
    }

    #[test]
    fn test_iterator_chunk_boundary_crossing() {
        // Create a buffer larger than the 4KB chunk size
        let data = "x".repeat(8192); // 8KB of data
        let leaked_data = Box::leak(data.into_bytes().into_boxed_slice());

        let persistence = Box::new(ChunkTreePersistence::from_data(leaked_data, DEFAULT_CONFIG));
        let vbuf = VirtualBuffer::new(persistence);

        // Test forward iteration across chunk boundary
        let mut iter = vbuf.iter_at(4090);

        // Read through the 4KB boundary
        for _ in 0..20 {
            assert_eq!(
                iter.next(),
                Some(b'x'),
                "Should read 'x' across chunk boundary"
            );
        }

        // Test backward iteration across chunk boundary
        let mut iter = vbuf.iter_at(4100);
        for _ in 0..20 {
            assert_eq!(
                iter.prev(),
                Some(b'x'),
                "Should read 'x' backwards across chunk boundary"
            );
        }
    }

    #[test]
    fn test_iterator_edit_invalidates_buffer() {
        // Create buffer with repeating pattern
        let data = "abcd".repeat(2000); // 8KB of "abcdabcd..."
        let leaked_data = Box::leak(data.clone().into_bytes().into_boxed_slice());

        let persistence = Box::new(ChunkTreePersistence::from_data(leaked_data, DEFAULT_CONFIG));
        let vbuf = VirtualBuffer::new(persistence);

        // Reference implementation using Vec
        let mut reference = data.into_bytes();
        let mut ref_pos = 0;

        let mut iter = vbuf.iter_at(0);

        // Read some bytes to populate the internal buffer
        assert_eq!(iter.next(), Some(b'a'));
        ref_pos += 1;
        assert_eq!(iter.next(), Some(b'b'));
        ref_pos += 1;
        assert_eq!(iter.next(), Some(b'c'));
        ref_pos += 1;

        // Now insert at position 1 (between 'a' and 'b')
        vbuf.insert(1, b"XYZ").unwrap();

        // Do the same insert in reference
        reference.splice(1..1, b"XYZ".iter().cloned());

        // Adjust reference position for the insert
        // Insert at position 1, which is before ref_pos (3), so add length of insert
        ref_pos += 3;

        // Iterator should see the new data after adjustment
        // The iterator was at position 3, insert at position 1 adjusts it to position 6
        // Position 6 in "aXYZbcdabcd..." is 'd'
        let expected = reference[ref_pos];
        assert_eq!(
            iter.next(),
            Some(expected),
            "After insert, iterator should read from new buffer at adjusted position"
        );
    }

    #[test]
    fn test_iterator_multiple_edits() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"0123456789".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        let mut iter = vbuf.iter_at(5);

        // Multiple inserts before iterator
        vbuf.insert(2, b"AA").unwrap(); // "01AA23456789", iter at 7
        vbuf.insert(0, b"BB").unwrap(); // "BB01AA23456789", iter at 9

        // Iterator should track position correctly
        assert_eq!(iter.next(), Some(b'5'));
        assert_eq!(iter.next(), Some(b'6'));
    }

    #[test]
    fn test_iterator_delete_before_position() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"0123456789".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        let mut iter = vbuf.iter_at(8);

        // Delete before iterator (removes "234")
        vbuf.delete(2..5).unwrap(); // "01456789", iter should move to 5

        // Iterator should read from adjusted position
        assert_eq!(iter.next(), Some(b'8'));
        assert_eq!(iter.next(), Some(b'9'));
    }

    #[test]
    fn test_iterator_at_end_of_buffer() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"hello".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        let mut iter = vbuf.iter_at(5);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None); // Should remain stable at end
    }

    #[test]
    fn test_iterator_peek_consistency() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"hello".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        let mut iter = vbuf.iter_at(1);

        // Peek should not advance
        assert_eq!(iter.peek(), Some(b'e'));
        assert_eq!(iter.peek(), Some(b'e'));

        // Next should advance
        assert_eq!(iter.next(), Some(b'e'));
        assert_eq!(iter.peek(), Some(b'l'));
    }

    #[test]
    fn test_iterator_seek() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"0123456789".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        let mut iter = vbuf.iter_at(0);
        assert_eq!(iter.next(), Some(b'0'));

        iter.seek(5);
        assert_eq!(iter.next(), Some(b'5'));

        iter.seek(9);
        assert_eq!(iter.next(), Some(b'9'));
    }

    #[test]
    fn test_iterator_buffer_reuse() {
        // Test that the internal buffer is reused for sequential reads
        let data = "x".repeat(5000);
        let leaked_data = Box::leak(data.into_bytes().into_boxed_slice());

        let persistence = Box::new(ChunkTreePersistence::from_data(leaked_data, DEFAULT_CONFIG));
        let vbuf = VirtualBuffer::new(persistence);

        let mut iter = vbuf.iter_at(0);

        // Read 4KB - this should fill the internal buffer
        for i in 0..4096 {
            assert_eq!(iter.next(), Some(b'x'), "Failed at position {}", i);
        }

        // Continue reading - should still work as we read the next chunk
        for i in 0..900 {
            assert_eq!(iter.next(), Some(b'x'), "Failed at position {}", 4096 + i);
        }
    }

    #[test]
    fn test_iterator_with_empty_buffer() {
        let persistence = Box::new(ChunkTreePersistence::from_data(
            Box::leak(b"".to_vec().into_boxed_slice()),
            DEFAULT_CONFIG,
        ));
        let vbuf = VirtualBuffer::new(persistence);

        let mut iter = vbuf.iter_at(0);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.prev(), None);
    }
}
