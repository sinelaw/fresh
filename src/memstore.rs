use std::collections::HashMap;
pub enum Chunk {
    Loaded { data: Vec<u8>, need_store: bool },
    Empty,
}

pub struct Memstore<'a> {
    chunks: HashMap<u64, Chunk>,
    chunk_size: u64,
    load_fn: Box<dyn Fn(u64) -> Option<Vec<u8>> + 'a>,
    store_fn: Box<dyn Fn(u64, &[u8]) + 'a>,
}

impl<'a> Memstore<'a> {
    pub fn new(
        chunk_size: u64,
        load_fn: impl Fn(u64) -> Option<Vec<u8>> + 'a,
        store_fn: impl Fn(u64, &[u8]) + 'a,
    ) -> Memstore<'a> {
        Memstore {
            chunks: HashMap::new(),
            chunk_size,
            load_fn: Box::new(load_fn),
            store_fn: Box::new(store_fn),
        }
    }

    pub fn get(&mut self, offset: u64) -> &Chunk {
        let chunk_index = offset / self.chunk_size;

        let load = &self.load_fn;
        return self.chunks.entry(chunk_index).or_insert_with_key(|v| {
            if let Some(data) = (load)(*v) {
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
        let store = &self.store_fn;
        for (index, chunk) in self.chunks.iter_mut() {
            if let Chunk::Loaded {
                data,
                need_store: is_modified,
            } = chunk
            {
                if *is_modified {
                    (store)(*index, data);
                    *is_modified = false;
                }
            }
        }
    }

    pub fn iter_at(&'a mut self, offset: u64) -> ChunkIter<'a> {
        ChunkIter {
            index: offset / self.chunk_size,
            memstore: self,
        }
    }
}

pub struct ChunkIter<'a> {
    memstore: &'a mut Memstore<'a>,
    index: u64,
}

impl<'a> Iterator for ChunkIter<'a> {
    type Item = &'a Chunk;

    fn next(&mut self) -> Option<Self::Item> {
        let chunk = self.memstore.get(self.index * self.memstore.chunk_size);
        self.index += 1;
        Some(chunk)
    }
}
