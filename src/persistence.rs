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
}
