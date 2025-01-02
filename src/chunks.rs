pub struct LoadedChunk<'a> {
    pub offset: u64,
    pub data: &'a [char],
    pub is_modified: bool,
}

pub struct UnloadedChunk {
    pub offset: u64,
    pub size: u64,
}

impl UnloadedChunk {
    pub fn split(self, offset: u64) -> (UnloadedChunk, UnloadedChunk) {
        let first = UnloadedChunk {
            offset: self.offset,
            size: offset - self.offset,
        };
        let second = UnloadedChunk {
            offset: offset,
            size: self.size - first.size,
        };
        (first, second)
    }
}

pub enum Chunk<'a> {
    Loaded(LoadedChunk<'a>),
    Unloaded(UnloadedChunk),
}

impl Chunk<'_> {
    pub fn unloaded(offset: u64, size: u64) -> Chunk<'static> {
        Chunk::Unloaded(UnloadedChunk { offset, size })
    }
    pub fn loaded(offset: u64, data: &[char], is_modified: bool) -> Chunk<'_> {
        Chunk::Loaded(LoadedChunk {
            offset,
            data,
            is_modified,
        })
    }
}
