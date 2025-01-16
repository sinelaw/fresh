use std::{convert::TryInto, ops::Range, os::unix::fs::FileExt};

use crate::{
    lines::EditLine,
    memstore::{Chunk, ChunkIndex, LoadStore, Memstore},
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
        let result = self
            .file
            .read_at(&mut buf, x)
            .expect("failed reading from file");
        buf.truncate(result);
        return Some(buf);
    }

    fn store(&self, x: u64, buf: &[u8]) {
        self.file.write_at(&buf, x).expect("failed writing to file");
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct LineIndex {
    relative: i64,
    offset_version: u64,
}

impl LineIndex {
    pub fn plus(&self, offset: i64) -> LineIndex {
        LineIndex {
            relative: self.relative + offset,
            offset_version: self.offset_version,
        }
    }
}

pub struct VirtualFile {
    // configuration
    chunk_size: u64,

    line_offset: i64,
    offset_version: u64,

    // indices of chunks loaded in chunk_lines
    loaded_chunks: Range<ChunkIndex>,

    /// lines loaded from memstore (disk)
    chunk_lines: Vec<EditLine>,

    memstore: Memstore<FileLoadStore>,
}

impl VirtualFile {
    pub fn new(chunk_size: u64, file: std::fs::File) -> VirtualFile {
        let chunk_zero = ChunkIndex::from_offset(0, chunk_size);
        let mut res = VirtualFile {
            chunk_size,
            offset_version: 0,
            line_offset: 0,
            loaded_chunks: Range {
                start: chunk_zero.clone(),
                end: chunk_zero.clone(),
            },
            chunk_lines: vec![],
            memstore: Memstore::new(chunk_size, FileLoadStore::new(chunk_size, file)),
        };
        res.seek(0);
        res
    }

    pub fn seek(&mut self, offset: u64) {
        let index = ChunkIndex::from_offset(offset, self.chunk_size);
        if self.loaded_chunks.contains(&index) {
            return;
        }
        let new_chunk = self.memstore.get(&index);
        let new_chunk_lines = match new_chunk {
            Chunk::Loaded {
                data,
                need_store: _,
            } => Self::parse_chunk(data),
            Chunk::Empty => vec![],
        };
        self.update_chunk_lines(index, new_chunk_lines);
    }

    fn update_chunk_lines(&mut self, new_index: ChunkIndex, mut new_chunk_lines: Vec<EditLine>) {
        if new_index == self.loaded_chunks.end && !self.loaded_chunks.is_empty() {
            self.loaded_chunks.end = new_index.next();
            // append new lines to existing lines
            // line_index is relative to the range start which stays unchanged.
            self.chunk_lines
                .last_mut()
                .unwrap()
                .extend(new_chunk_lines.remove(0));
            self.chunk_lines.append(&mut new_chunk_lines);
        } else if new_index.next() == self.loaded_chunks.start && !self.loaded_chunks.is_empty() {
            self.loaded_chunks.start = new_index;
            // append existing lines to new lines
            // line indexes are relative to the range start, which was pushed up by the new chunk
            let len: i64 = new_chunk_lines.len().try_into().unwrap();
            self.line_offset = self.line_offset + len;
            std::mem::swap(&mut self.chunk_lines, &mut new_chunk_lines);
            self.chunk_lines
                .last_mut()
                .unwrap()
                .extend(new_chunk_lines.remove(0));
            self.chunk_lines.append(&mut new_chunk_lines);
        } else {
            // replace existing lines
            self.loaded_chunks = Range {
                start: new_index.clone(),
                end: new_index.next(),
            };
            self.chunk_lines = new_chunk_lines;
            self.line_offset = 0;
            self.offset_version += 1;
        };
    }

    pub fn prev_line(&mut self, line_index: &LineIndex) -> Option<LineIndex> {
        let index = self.to_abs_index(&line_index);
        if index.is_none() {
            return None;
        }
        let index = index.unwrap();
        if index == 0 && self.loaded_chunks.start.index > 0 {
            // seek to previous chunk
            self.seek(self.chunk_size * (self.loaded_chunks.start.index - 1));
            assert!(line_index.offset_version == self.offset_version);
        }
        // after possible seek, index may still be zero if there was nothing to load
        if index > 0 {
            return Some(LineIndex {
                relative: line_index.relative - 1,
                offset_version: line_index.offset_version,
            });
        }
        return Some(line_index.clone());
    }

    pub fn next_line(&mut self, line_index: &LineIndex) -> Option<LineIndex> {
        let index = self.to_abs_index(&line_index);
        if index.is_none() {
            return None;
        }
        let index = index.unwrap();
        if index + 2 >= self.chunk_lines.len() {
            // fetch more lines, after increasing index it will be the last line which may be incomplete
            self.seek(self.loaded_chunks.end.to_offset());
            assert!(line_index.offset_version == self.offset_version);
        }
        if index + 1 < self.chunk_lines.len() {
            return Some(LineIndex {
                relative: line_index.relative + 1,
                offset_version: line_index.offset_version,
            });
        }
        return Some(line_index.clone());
    }

    pub fn remove(&mut self, line_index: &LineIndex) -> Option<EditLine> {
        let index = self.to_abs_index(&line_index);
        if index.is_none() {
            return None;
        }
        let index = index.unwrap();
        if index + 2 >= self.chunk_lines.len() {
            // fetch more lines, after removal it will be the last line which may be incomplete
            self.seek(self.loaded_chunks.end.to_offset());
            assert!(line_index.offset_version == self.offset_version);
        }
        let removed_line = self.chunk_lines.remove(index);
        if self.chunk_lines.len() == 0 {
            // that was the only line left, add one back to avoid empty
            self.chunk_lines.push(EditLine::empty());
        }
        return Some(removed_line);
    }

    pub fn insert_after(&mut self, line_index: &LineIndex, new_line: EditLine) -> Option<()> {
        match self.to_abs_index(&line_index) {
            None => return None,
            Some(index) => {
                self.chunk_lines.insert(index + 1, new_line);
                return Some(());
            }
        }
    }

    pub fn get(&self, line_index: &LineIndex) -> Option<&EditLine> {
        match self.to_abs_index(&line_index) {
            None => return None,
            Some(index) => {
                return self.chunk_lines.get(index);
            }
        }
    }

    pub fn get_mut(&mut self, line_index: &LineIndex) -> Option<&mut EditLine> {
        match self.to_abs_index(&line_index) {
            None => return None,
            Some(index) => {
                return self.chunk_lines.get_mut(index);
            }
        }
    }

    fn parse_chunk(data: &Vec<u8>) -> Vec<EditLine> {
        String::from_utf8_lossy(data)
            .split(|c: char| c == '\n')
            .map(|s| EditLine::new(s.to_string()))
            .collect()
    }

    pub fn get_index(&self) -> LineIndex {
        LineIndex {
            relative: 0,
            offset_version: self.offset_version,
        }
    }

    pub fn iter_at(
        &mut self,
        line_index: &LineIndex,
        count: usize,
    ) -> impl Iterator<Item = &EditLine> {
        match self.to_abs_index(&line_index) {
            None => return [].iter(),
            Some(index) => {
                // materialize 'count' lines
                let mut line_index = line_index.clone();
                for _ in 0..count {
                    line_index = self.next_line(&line_index).unwrap();
                }
                let end_index = usize::min(index + count, self.chunk_lines.len());
                self.chunk_lines[index..end_index].iter()
            }
        }
    }

    fn to_abs_index(&self, line_index: &LineIndex) -> Option<usize> {
        if self.offset_version != line_index.offset_version {
            return None;
        }
        let index = (line_index.relative + self.line_offset).try_into().unwrap();
        assert!(index < self.chunk_lines.len());
        Some(index)
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;
    use tempfile::tempfile;
    use VirtualFile;

    fn create_test_file(content: &str) -> std::fs::File {
        let mut file = tempfile().unwrap();
        file.write_all(content.as_bytes()).unwrap();
        file
    }

    #[test]
    fn test_virtual_file_empty() {
        let file = create_test_file("");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        let line_index = vf.get_index();
        assert_eq!(vf.next_line(&line_index), Some(line_index));
    }

    #[test]
    fn test_virtual_file_new() {
        let file = create_test_file("line1\nline2\nline3\n");
        let _ = VirtualFile::new(10, file);
    }

    #[test]
    fn test_virtual_file_seek() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        vf.seek(11);
        vf.seek(0);
    }

    #[test]
    fn test_virtual_file_next_line() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        let line_index = vf.get_index();
        let line_index = vf.next_line(&line_index).unwrap();
        assert_eq!(vf.get(&line_index).unwrap().str(), "line2");
        let line_index = vf.next_line(&line_index).unwrap();
        assert_eq!(vf.get(&line_index).unwrap().str(), "line3");
        let line_index = vf.next_line(&line_index).unwrap();
        assert_eq!(vf.get(&line_index).unwrap().str(), "");
        let last = vf.next_line(&line_index);
        assert_eq!(last, Some(line_index));
    }

    #[test]
    fn test_virtual_file_remove() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        let line_index = vf.get_index();
        assert_eq!(vf.remove(&line_index).unwrap().str(), "line1");
        assert_eq!(vf.get(&line_index).unwrap().str(), "line2");
    }

    #[test]
    fn test_virtual_file_insert() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        let line_index = vf.get_index();
        vf.insert_after(&line_index, EditLine::new("new_line".to_string()));
        assert_eq!(vf.get(&line_index).unwrap().str(), "line1");
        let line_index = vf.next_line(&line_index).unwrap();
        assert_eq!(vf.get(&line_index).unwrap().str(), "new_line");
    }

    #[test]
    fn test_virtual_file_get() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        let line_index = vf.get_index();
        assert_eq!(vf.get(&line_index).unwrap().str(), "line1");
    }

    #[test]
    fn test_virtual_file_get_mut() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        let line_index = vf.get_index();

        let line = vf.get_mut(&line_index).unwrap();
        line.overwrite(0, 'b');
        assert_eq!(vf.get(&line_index).unwrap().str(), "bine1");
    }

    #[test]
    fn test_virtual_file_iter_at() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        let line_index = vf.get_index();
        let mut iter = vf.iter_at(&line_index, 3);
        assert_eq!(iter.next().unwrap().str(), "line1");
        assert_eq!(iter.next().unwrap().str(), "line2");
        assert_eq!(iter.next().unwrap().str(), "line3");
        assert!(iter.next().is_none());
    }

    #[test]
    fn test_parse_chunk() {
        let data = b"line1\nline2\nline3\n";
        let lines = VirtualFile::parse_chunk(&data.to_vec());
        assert_eq!(lines.len(), 4);
        assert_eq!(lines[0].str(), "line1");
        assert_eq!(lines[1].str(), "line2");
        assert_eq!(lines[2].str(), "line3");
        assert_eq!(lines[3].str(), "");
    }

    #[test]
    fn test_parse_chunk_empty() {
        let data = b"";
        let lines = VirtualFile::parse_chunk(&data.to_vec());
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].str(), "");
    }

    #[test]
    fn test_parse_chunk_no_newline() {
        let data = b"line1";
        let lines = VirtualFile::parse_chunk(&data.to_vec());
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].str(), "line1");
    }

    #[test]
    fn test_parse_chunk_multiple_newlines() {
        let data = b"line1\n\nline2\n";
        let lines = VirtualFile::parse_chunk(&data.to_vec());
        assert_eq!(lines.len(), 4);
        assert_eq!(lines[0].str(), "line1");
        assert_eq!(lines[1].str(), "");
        assert_eq!(lines[2].str(), "line2");
        assert_eq!(lines[3].str(), "");
    }
}
