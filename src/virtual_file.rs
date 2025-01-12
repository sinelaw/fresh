use std::{
    convert::TryInto,
    ops::{Range, RangeBounds},
    os::unix::fs::FileExt,
    vec,
};

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

pub struct VirtualFile {
    // configuration
    chunk_size: u64,

    /// index into chunk_lines
    line_index: usize,

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
            line_index: 0,
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
            // line_index is relative to the range start, which was pushed up by the new chunk
            self.line_index += new_chunk_lines.len();
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
            self.line_index = 0;
        };
    }

    pub fn prev_line(&mut self) -> u16 {
        if self.line_index == 0 && self.loaded_chunks.start.index > 0 {
            // seek to previous chunk
            self.seek(self.chunk_size * (self.loaded_chunks.start.index - 1));
        }
        // after possible seek, line_index may still be zero if there was nothing to load
        if self.line_index > 0 {
            self.line_index -= 1;
            return 1;
        }
        return 0;
    }

    pub fn next_line(&mut self) -> u16 {
        if self.line_index + 2 >= self.chunk_lines.len() {
            // fetch more lines, after increasing index it will be the last line which may be incomplete
            self.seek(self.loaded_chunks.end.to_offset());
        }
        if self.line_index + 1 < self.chunk_lines.len() {
            self.line_index += 1;
            return 1;
        }
        return 0;
    }

    pub fn remove(&mut self) -> EditLine {
        if self.line_index + 2 >= self.chunk_lines.len() {
            // fetch more lines, after removal it will be the last line which may be incomplete
            self.seek(self.loaded_chunks.end.to_offset());
        }
        let removed_line = self.chunk_lines.remove(self.line_index);
        if self.line_index > 0 {
            self.line_index -= 1;
        } else if self.chunk_lines.len() == 0 {
            // that was the only line left, add one back to avoid empty
            self.chunk_lines.push(EditLine::empty());
        }
        return removed_line;
    }

    pub fn get_index(&self) -> usize {
        self.line_index
    }

    pub fn insert_after(&mut self, new_line: EditLine) {
        self.chunk_lines.insert(self.line_index + 1, new_line);
    }

    pub fn get(&self) -> &EditLine {
        self.chunk_lines.get(self.line_index).unwrap()
    }

    pub fn get_mut(&mut self) -> &mut EditLine {
        self.chunk_lines.get_mut(self.line_index).unwrap()
    }

    fn parse_chunk(data: &Vec<u8>) -> Vec<EditLine> {
        String::from_utf8_lossy(data)
            .split(|c: char| c == '\n')
            .map(|s| EditLine::new(s.to_string()))
            .collect()
    }

    pub fn iter_at(
        &mut self,
        offset_from_line_index: i64,
        count: usize,
    ) -> impl Iterator<Item = &EditLine> {
        // TODO: This is inefficient and also clobbers the current line_index
        let mut clobber: i32 = 0;
        if offset_from_line_index < 0 {
            // need to iterate lines backwards
            for _ in offset_from_line_index..0 {
                clobber -= self.prev_line() as i32;
            }
        } else {
            // need to iterate lines forwards
            for _ in 0..offset_from_line_index {
                clobber += self.next_line() as i32;
            }
        }
        // line_index is now at the start of the range
        let start_index = self.line_index;
        // materialize 'count' lines
        for _ in 0..count {
            clobber += self.next_line() as i32;
        }
        // ... and now go back to where line_index was before
        self.line_index = (self.line_index as i32 - clobber).try_into().unwrap();

        self.chunk_lines.iter().skip(start_index)
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
        assert!(vf.next_line() == 0);
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
        vf.next_line();
        assert_eq!(vf.get().str(), "line2");
        vf.next_line();
        assert_eq!(vf.get().str(), "line3");
        vf.next_line();
        assert_eq!(vf.get().str(), "");
        let last = vf.next_line();
        assert_eq!(last, 0);
    }

    #[test]
    fn test_virtual_file_remove() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        assert_eq!(vf.remove().str(), "line1");
        assert_eq!(vf.get().str(), "line2");
    }

    #[test]
    fn test_virtual_file_insert() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        vf.insert_after(EditLine::new("new_line".to_string()));
        assert_eq!(vf.get().str(), "line1");
        assert_eq!(vf.next_line(), 1);
        assert_eq!(vf.get().str(), "new_line");
    }

    #[test]
    fn test_virtual_file_get() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        assert_eq!(vf.get().str(), "line1");
    }

    #[test]
    fn test_virtual_file_get_mut() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        let line = vf.get_mut();
        line.overwrite(0, 'b');
        assert_eq!(vf.get().str(), "bine1");
    }

    #[test]
    fn test_virtual_file_iter_at() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(0);
        let mut iter = vf.iter_at(1, 3);
        assert_eq!(iter.next().unwrap().str(), "line2");
        assert_eq!(iter.next().unwrap().str(), "line3");
        assert_eq!(iter.next().unwrap().str(), "");
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
