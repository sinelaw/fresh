use std::os::unix::fs::FileExt;

use crate::{
    lines::LoadedLine,
    memstore::{Chunk, LoadStore, Memstore},
};

struct FileLoadStore {
    chunk_size: u64,
    file: std::fs::File,
}

impl FileLoadStore {
    fn new(chunk_size: u64, file: std::fs::File) -> FileLoadStore {
        FileLoadStore { chunk_size, file }
    }
}

impl LoadStore for FileLoadStore {
    fn load(&self, x: u64) -> Option<Vec<u8>> {
        let mut buf = vec![0; self.chunk_size as usize];
        self.file
            .read_at(&mut buf, x)
            .expect("failed reading from file");
        return Some(buf);
    }

    fn store(&self, x: u64, buf: &[u8]) {
        self.file.write_at(&buf, x).expect("failed writing to file");
    }
}

pub struct VirtualFile {
    offset: u64,
    chunk_lines: Option<Vec<LoadedLine>>,
    memstore: Memstore<FileLoadStore>,
}

impl VirtualFile {
    pub fn new(chunk_size: u64, file: std::fs::File) -> VirtualFile {
        VirtualFile {
            offset: 0,
            chunk_lines: None,
            memstore: Memstore::new(chunk_size, FileLoadStore::new(chunk_size, file)),
        }
    }

    pub fn seek(&mut self, offset: u64) {
        self.offset = offset;
    }

    pub fn next_line(&mut self) -> &mut LoadedLine {
        let chunk = self.memstore.get(self.offset);
        self.chunk_lines = match chunk {
            Chunk::Loaded { data, need_store } => Some(
                String::from_utf8_lossy(data)
                    .split(|c: char| c == '\n')
                    .map(|s| LoadedLine::new(s.to_string()))
                    .collect(),
            ),
            Chunk::Empty => None,
        };
        self.chunk_lines
    }

    pub fn remove(&self, y: usize) -> LoadedLine {
        todo!()
    }

    pub fn insert(&self, y: usize, new_line: LoadedLine) {
        todo!()
    }

    pub fn get(&self, y: usize) -> &LoadedLine {
        todo!()
    }
}
