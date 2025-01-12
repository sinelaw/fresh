use std::collections::HashMap;
pub enum Chunk {
    Loaded { data: Vec<u8>, need_store: bool },
    Empty,
}
pub trait LoadStore {
    fn load(&self, offset: u64) -> Option<Vec<u8>>;
    fn store(&self, offset: u64, data: &[u8]);
}

#[derive(PartialEq, PartialOrd, Clone, Debug, Eq, Hash)]
pub struct ChunkIndex {
    pub index: u64,
    pub chunk_size: u64,
}

impl ChunkIndex {
    pub fn from_offset(offset: u64, chunk_size: u64) -> ChunkIndex {
        ChunkIndex {
            index: offset / chunk_size,
            chunk_size,
        }
    }
    pub fn to_offset(&self) -> u64 {
        self.index * self.chunk_size
    }
    pub fn next(&self) -> ChunkIndex {
        ChunkIndex {
            index: self.index + 1,
            chunk_size: self.chunk_size,
        }
    }
}

pub struct Memstore<L>
where
    L: LoadStore,
{
    chunks: HashMap<ChunkIndex, Chunk>,
    chunk_size: u64,
    load_store: L,
}

impl<L> Memstore<L>
where
    L: LoadStore,
{
    pub fn new(chunk_size: u64, load_store: L) -> Memstore<L> {
        Memstore {
            chunks: HashMap::new(),
            chunk_size,
            load_store,
        }
    }

    pub fn get(&mut self, chunk_index: &ChunkIndex) -> &Chunk {
        assert!(chunk_index.chunk_size == self.chunk_size);
        let load_store = &self.load_store;
        return self
            .chunks
            .entry(chunk_index.clone())
            .or_insert_with_key(|index| {
                if let Some(data) = load_store.load((*index).to_offset()) {
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
        let load_store = &self.load_store;
        for (index, chunk) in self.chunks.iter_mut() {
            if let Chunk::Loaded {
                data,
                need_store: is_modified,
            } = chunk
            {
                if *is_modified {
                    load_store.store((*index).to_offset(), data);
                    *is_modified = false;
                }
            }
        }
    }
}
