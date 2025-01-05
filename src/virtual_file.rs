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
    chunk_offset: u64,
    chunk_size: u64,
    line_index_in_chunk: usize,
    chunk_lines: Option<Vec<LoadedLine>>,
    memstore: Memstore<FileLoadStore>,
}

impl VirtualFile {
    pub fn new(chunk_size: u64, file: std::fs::File) -> VirtualFile {
        VirtualFile {
            chunk_offset: 0,
            chunk_size,
            line_index_in_chunk: 0,
            chunk_lines: None,
            memstore: Memstore::new(chunk_size, FileLoadStore::new(chunk_size, file)),
        }
    }

    pub fn seek(&mut self, offset: u64) {
        self.chunk_offset = offset;
        let chunk = self.memstore.get(self.chunk_offset);
        self.chunk_lines = match chunk {
            Chunk::Loaded { data, need_store } => Some(Self::parse_chunk(data)),
            Chunk::Empty => None,
        };
        self.line_index_in_chunk = 0;
    }

    pub fn next_line(&mut self) -> Option<&mut LoadedLine> {
        if let Some(chunk_lines) = self.chunk_lines.as_mut() {
            let index = self.line_index_in_chunk;
            if self.line_index_in_chunk >= chunk_lines.len() {
                self.chunk_offset += self.chunk_size;
                let next_chunk = self.memstore.get(self.chunk_offset);
                let more_lines = match next_chunk {
                    Chunk::Loaded { data, need_store } => Some(Self::parse_chunk(data)),
                    Chunk::Empty => None,
                };
                if let Some(more_lines) = more_lines {
                    chunk_lines.extend(more_lines);
                }
            }
            self.line_index_in_chunk += 1;
            return chunk_lines.get_mut(index);
        }
        None
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

    fn parse_chunk(data: &Vec<u8>) -> Vec<LoadedLine> {
        String::from_utf8_lossy(data)
            .split(|c: char| c == '\n')
            .map(|s| LoadedLine::new(s.to_string()))
            .collect()
    }
}
