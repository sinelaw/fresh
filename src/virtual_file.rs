use std::{os::unix::fs::FileExt, vec};

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
    chunk_index: u64,
    chunk_size: u64,
    line_index_in_chunk: usize,
    chunk_lines: Option<Vec<LoadedLine>>,
    memstore: Memstore<FileLoadStore>,
}

impl VirtualFile {
    pub fn new(chunk_size: u64, file: std::fs::File) -> VirtualFile {
        VirtualFile {
            chunk_index: 0,
            chunk_size,
            line_index_in_chunk: 0,
            chunk_lines: None,
            memstore: Memstore::new(chunk_size, FileLoadStore::new(chunk_size, file)),
        }
    }

    pub fn seek(&mut self, offset: u64) {
        let index = offset / self.chunk_size;
        if self.chunk_index == index {
            return;
        }
        let new_chunk = self.memstore.get(index);
        let new_chunk_lines = match new_chunk {
            Chunk::Loaded { data, need_store } => Some(Self::parse_chunk(data)),
            Chunk::Empty => None,
        };
        self.update_chunk_lines(index, new_chunk_lines);
        self.line_index_in_chunk = 0;
    }

    fn update_chunk_lines(&mut self, new_index: u64, mut new_chunk_lines: Option<Vec<LoadedLine>>) {
        let old_index = self.chunk_index;
        self.chunk_index = new_index;
        let mut empty: Vec<LoadedLine> = vec![];
        if new_index == old_index + 1 {
            // append new lines to existing lines
            // line_index_in_chunk was relative to the old chunk, which is still first, so stays unchanged.
            self.chunk_lines
                .as_mut()
                .unwrap_or(&mut empty)
                .append(&mut new_chunk_lines.unwrap_or(vec![]));
        } else if new_index == old_index - 1 {
            // append existing lines to new lines
            // line_index_in_chunk was relative to the old chunk lines, which are now after the lines we are perpending
            self.line_index_in_chunk += new_chunk_lines.as_ref().map_or(0, |l| l.len());
            std::mem::swap(&mut self.chunk_lines, &mut new_chunk_lines);
            self.chunk_lines
                .as_mut()
                .unwrap_or(&mut empty)
                .append(&mut new_chunk_lines.unwrap_or(vec![]));
        } else {
            // replace existing lines
            self.chunk_lines = new_chunk_lines;
        };
    }

    pub fn next_line(&mut self) -> Option<&mut LoadedLine> {
        let lines_count = self.chunk_lines.as_ref().map_or(0, |lines| lines.len());
        self.line_index_in_chunk += 1;
        if self.line_index_in_chunk >= lines_count {
            self.seek(self.chunk_index + 1);
        }
        let index = self.line_index_in_chunk;
        return self
            .chunk_lines
            .as_mut()
            .map(|x| x.get_mut(index))
            .flatten();
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
