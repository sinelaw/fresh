use std::collections::HashMap;
pub enum Chunk {
    Loaded { data: Vec<u8>, need_store: bool },
    Empty,
}
pub trait LoadStore {
    fn load(&self, offset: u64) -> Option<Vec<u8>>;
    fn store(&self, offset: u64, data: &[u8]);
}

pub struct Memstore<L>
where
    L: LoadStore,
{
    chunks: HashMap<u64, Chunk>,
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

    pub fn get(&mut self, offset: u64) -> &Chunk {
        let chunk_index = offset / self.chunk_size;

        let load_store = &self.load_store;
        return self.chunks.entry(chunk_index).or_insert_with_key(|v| {
            if let Some(data) = load_store.load(*v) {
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
                    load_store.store(*index, data);
                    *is_modified = false;
                }
            }
        }
    }
}
