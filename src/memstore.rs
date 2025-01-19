use std::collections::HashMap;
pub enum Chunk {
    Loaded { data: Vec<u8>, need_store: bool },
    Empty,
}
pub trait LoadStore {
    fn load(&self, offset: u64, size: u64) -> Option<Vec<u8>>;
    fn store(&mut self, offset: u64, data: &[u8]);
}

#[derive(PartialEq, PartialOrd, Clone, Debug, Eq, Hash)]
pub struct ChunkIndex {
    pub offset: u64,
    pub chunk_size: u64,
}

impl ChunkIndex {
    pub fn new(offset: u64, chunk_size: u64) -> ChunkIndex {
        ChunkIndex { offset, chunk_size }
    }
    pub fn end_offset(&self) -> u64 {
        self.offset + self.chunk_size
    }
}

pub struct Memstore<L>
where
    L: LoadStore,
{
    chunks: HashMap<ChunkIndex, Chunk>,
    load_store: L,
}

impl<L> Memstore<L>
where
    L: LoadStore,
{
    pub fn new(load_store: L) -> Memstore<L> {
        Memstore {
            chunks: HashMap::new(),
            load_store,
        }
    }

    pub fn get(&mut self, chunk_index: &ChunkIndex) -> &Chunk {
        let load_store = &self.load_store;
        return self
            .chunks
            .entry(chunk_index.clone())
            .or_insert_with_key(|index| {
                if let Some(data) = load_store.load(index.offset, index.chunk_size) {
                    Chunk::Loaded {
                        data,
                        need_store: false,
                    }
                } else {
                    Chunk::Empty
                }
            });
    }

    pub fn store_all(&mut self) {
        let load_store = &mut self.load_store;
        for (index, chunk) in self.chunks.iter_mut() {
            if let Chunk::Loaded {
                data,
                need_store: is_modified,
            } = chunk
            {
                if *is_modified {
                    load_store.store(index.offset, data);
                    *is_modified = false;
                }
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    struct MockLoadStore {
        data: HashMap<u64, Vec<u8>>,
    }

    impl MockLoadStore {
        fn new() -> Self {
            MockLoadStore {
                data: HashMap::new(),
            }
        }

        fn with_data(mut self, offset: u64, data: Vec<u8>) -> Self {
            self.data.insert(offset, data);
            self
        }
    }

    impl LoadStore for MockLoadStore {
        fn load(&self, offset: u64, size: u64) -> Option<Vec<u8>> {
            self.data
                .get(&offset)
                .map(|data| data[..size as usize].to_vec())
        }

        fn store(&mut self, offset: u64, data: &[u8]) {
            self.data.insert(offset, data.to_vec());
        }
    }

    #[test]
    fn test_memstore_get_existing_chunk() {
        let load_store = MockLoadStore::new().with_data(0, vec![1, 2, 3, 4]);
        let mut memstore = Memstore::new(load_store);

        let chunk_index = ChunkIndex::new(0, 4);
        let chunk = memstore.get(&chunk_index);

        match chunk {
            Chunk::Loaded { data, need_store } => {
                assert_eq!(data, &vec![1, 2, 3, 4]);
                assert!(!need_store);
            }
            _ => panic!("Expected Chunk::Loaded"),
        }
    }

    #[test]
    fn test_memstore_get_non_existing_chunk() {
        let load_store = MockLoadStore::new();
        let mut memstore = Memstore::new(load_store);

        let chunk_index = ChunkIndex::new(0, 4);
        let chunk = memstore.get(&chunk_index);

        match chunk {
            Chunk::Empty => {}
            _ => panic!("Expected Chunk::Empty"),
        }
    }

    #[test]
    fn test_memstore_store_all() {
        let load_store = MockLoadStore::new();
        let mut memstore = Memstore::new(load_store);

        let chunk_index = ChunkIndex::new(0, 4);
        memstore.chunks.insert(
            chunk_index.clone(),
            Chunk::Loaded {
                data: vec![1, 2, 3, 4],
                need_store: true,
            },
        );

        memstore.store_all();

        let stored_data = memstore.load_store.load(0, 4).unwrap();
        assert_eq!(stored_data, vec![1, 2, 3, 4]);

        let chunk = memstore.get(&chunk_index);
        match chunk {
            Chunk::Loaded { data, need_store } => {
                assert_eq!(data, &vec![1, 2, 3, 4]);
                assert!(!need_store);
            }
            _ => panic!("Expected Chunk::Loaded"),
        }
    }
}
