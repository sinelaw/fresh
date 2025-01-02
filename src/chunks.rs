pub struct UnloadedChunk {
    offset: u64,
    size: u64,
}
pub struct LoadedChunk<'a> {
    offset: u64,
    data: &'a [char],
    is_modified: bool,
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

    pub fn merge(self, other: UnloadedChunk) -> Option<UnloadedChunk> {
        if self.offset + self.size == other.offset {
            Some(UnloadedChunk {
                offset: self.offset,
                size: self.size + other.size,
            })
        } else if other.offset + other.size == self.offset {
            Some(UnloadedChunk {
                offset: other.offset,
                size: other.size + self.size,
            })
        } else {
            None
        }
    }
}

impl LoadedChunk<'_> {
    pub fn drop(self) -> UnloadedChunk {
        UnloadedChunk {
            offset: self.offset,
            size: self.data.len() as u64,
        }
    }
}

pub enum Chunk<'a> {
    Unloaded(UnloadedChunk),
    Loaded(LoadedChunk<'a>),
}

impl Chunk<'_> {
    pub fn unloaded(offset: u64, size: u64) -> Chunk<'static> {
        Chunk::Unloaded(UnloadedChunk { offset, size })
    }
    pub fn loaded(offset: u64, data: &[char]) -> Chunk<'_> {
        Chunk::Loaded(LoadedChunk {
            offset,
            data,
            is_modified: false,
        })
    }
}
