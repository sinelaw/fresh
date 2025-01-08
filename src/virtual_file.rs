use std::{convert::TryInto, iter::Skip, ops::Range, os::unix::fs::FileExt, vec};

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
    // configuration
    chunk_size: u64,

    /// index into chunk_lines
    line_index: usize,

    // indices of chunks loaded in chunk_lines
    loaded_chunks: Range<u64>,

    /// lines loaded from memstore (disk)
    chunk_lines: Vec<LoadedLine>,

    memstore: Memstore<FileLoadStore>,
}

impl VirtualFile {
    pub fn new(chunk_size: u64, file: std::fs::File) -> VirtualFile {
        VirtualFile {
            chunk_size,
            line_index: 0,
            loaded_chunks: Range { start: 0, end: 0 },
            chunk_lines: vec![LoadedLine::empty()],
            memstore: Memstore::new(chunk_size, FileLoadStore::new(chunk_size, file)),
        }
    }

    pub fn seek(&mut self, offset: u64) {
        let index = offset / self.chunk_size;
        if self.loaded_chunks.contains(&index) {
            return;
        }
        let new_chunk = self.memstore.get(index);
        let new_chunk_lines = match new_chunk {
            Chunk::Loaded { data, need_store } => Self::parse_chunk(data),
            Chunk::Empty => vec![],
        };
        self.update_chunk_lines(index, new_chunk_lines);
    }

    fn update_chunk_lines(&mut self, new_index: u64, mut new_chunk_lines: Vec<LoadedLine>) {
        if new_index == self.loaded_chunks.end {
            self.loaded_chunks.end = new_index;
            // append new lines to existing lines
            // line_index is relative to the range start which stays unchanged.
            self.chunk_lines.append(&mut new_chunk_lines);
        } else if new_index + 1 == self.loaded_chunks.start {
            self.loaded_chunks.start = new_index;
            // append existing lines to new lines
            // line_index is relative to the range start, which was pushed up by the new chunk
            self.line_index += new_chunk_lines.len();
            std::mem::swap(&mut self.chunk_lines, &mut new_chunk_lines);
            self.chunk_lines.append(&mut new_chunk_lines);
        } else {
            // replace existing lines
            self.loaded_chunks = Range {
                start: new_index,
                end: new_index + 1,
            };
            self.chunk_lines = new_chunk_lines;
            self.line_index = 0;
        };
    }

    pub fn next_line(&mut self) -> Option<&mut LoadedLine> {
        let lines_count = self.chunk_lines.len();
        self.line_index += 1;
        if self.line_index >= lines_count {
            // seek to next chunk
            self.seek(self.loaded_chunks.end);
        }
        let index = self.line_index;
        return self.chunk_lines.get_mut(index);
    }

    pub fn remove(&mut self) -> LoadedLine {
        let removed_line = self.chunk_lines.remove(self.line_index);
        if self.line_index > 0 {
            self.line_index -= 1;
        } else if self.chunk_lines.len() == 0 {
            // that was the only line left, add one back to avoid empty
            self.chunk_lines.push(LoadedLine::empty());
        }
        return removed_line;
    }

    pub fn insert(&mut self, new_line: LoadedLine) {
        self.chunk_lines.insert(self.line_index, new_line);
    }

    pub fn get(&self) -> &LoadedLine {
        self.chunk_lines.get(self.line_index).unwrap()
    }

    pub fn get_mut(&mut self) -> &mut LoadedLine {
        self.chunk_lines.get_mut(self.line_index).unwrap()
    }

    fn parse_chunk(data: &Vec<u8>) -> Vec<LoadedLine> {
        String::from_utf8_lossy(data)
            .split(|c: char| c == '\n')
            .map(|s| LoadedLine::new(s.to_string()))
            .collect()
    }

    pub fn iter_at(&self, offset_from_line_index: i64) -> impl Iterator<Item = &LoadedLine> {
        let start_index: usize = ((self.line_index as i64) + offset_from_line_index)
            .try_into()
            .unwrap();
        self.chunk_lines.iter().skip(start_index)
    }
}
