use std::{
    collections::BTreeMap, convert::TryInto, io::SeekFrom, os::unix::fs::FileExt, sync::Arc,
};

use crate::{
    lines::EditLine,
    logs::log,
    memstore::{Chunk, ChunkIndex, LoadStore, Memstore},
};

struct FileLoadStore {
    file: Arc<std::fs::File>,
}

impl FileLoadStore {
    fn new(file: Arc<std::fs::File>) -> FileLoadStore {
        FileLoadStore { file }
    }
}

impl LoadStore for FileLoadStore {
    fn load(&self, offset: u64, size: u64) -> Option<Vec<u8>> {
        let mut buf = vec![0; size as usize];
        let result = self
            .file
            .read_at(&mut buf, offset)
            .expect("failed reading from file");
        buf.truncate(result);
        return Some(buf);
    }

    fn store(&mut self, x: u64, buf: &[u8]) {
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

#[derive(Debug, Clone, Copy)]
struct LoadedLoc {
    loaded_offset: u64,
    loaded_size: u64,
}

#[derive(Debug)]
pub struct LoadedLine {
    line: Box<EditLine>,
    loaded_loc: Option<LoadedLoc>,
}

impl LoadedLine {
    pub fn new(line: EditLine) -> LoadedLine {
        LoadedLine {
            line: Box::new(line),
            loaded_loc: None,
        }
    }
    pub fn from_loaded(line: EditLine, offset: u64) -> LoadedLine {
        let line_size: u64 = line.str().bytes().len().try_into().unwrap();
        LoadedLine {
            line: Box::new(line),
            loaded_loc: Some(LoadedLoc {
                loaded_offset: offset,
                loaded_size: line_size,
            }),
        }
    }
    pub fn line(&self) -> &EditLine {
        &*self.line
    }
}

pub struct VirtualFile {
    // configuration
    chunk_size: u64,

    /// current version of line indexes, any line index from older version is invalid
    offset_version: u64,

    /// relative to which line index in chunk_lines is the current line index offset version
    line_anchor: i64,

    /// file offset -> chunk index
    loaded_chunks: BTreeMap<u64, ChunkIndex>,

    /// lines loaded from memstore (disk)
    chunk_lines: Vec<LoadedLine>,

    memstore: Memstore<FileLoadStore>,

    file: Arc<std::fs::File>,
}

impl VirtualFile {
    pub fn new(chunk_size: u64, file: std::fs::File) -> VirtualFile {
        let file = Arc::new(file);
        let mut res = VirtualFile {
            chunk_size,
            offset_version: 0,
            line_anchor: 0,
            loaded_chunks: BTreeMap::new(),
            chunk_lines: vec![],
            file: file.clone(),
            memstore: Memstore::new(FileLoadStore::new(file.clone())),
        };
        res.seek(SeekFrom::Start(0));
        res
    }

    pub fn seek(&mut self, from: SeekFrom) {
        let offset = self.resolve_offset(from);
        self.load_lines(offset);

        // Move the anchor to be as near as possible to the requested seek position:
        for (index, line) in self.chunk_lines.iter().enumerate() {
            log!(
                "seek: Checking line[{:?}] loc: {:?} >= offset {:?}",
                index,
                line.loaded_loc,
                offset
            );
            self.line_anchor = index.try_into().unwrap();
            if line
                .loaded_loc
                .map(|loc| loc.loaded_offset >= offset)
                .unwrap_or(false)
            {
                break;
            }
        }
        log!("seek: Set line_anchor = {:?}", self.line_anchor);
    }

    fn load_lines(&mut self, offset: u64) {
        let aligned_offset = (offset / self.chunk_size) * self.chunk_size;
        let load_index = ChunkIndex::new(aligned_offset, self.chunk_size);
        if self.loaded_chunks.contains_key(&aligned_offset) {
            return;
        }
        let new_chunk = self.memstore.get(&load_index);
        let new_chunk_lines = match new_chunk {
            Chunk::Loaded {
                data,
                need_store: _,
            } => Self::parse_chunk(data),

            Chunk::Empty => vec![],
        };
        log!(
            "loaded {:?} lines from chunk {:?}, loaded_chunks: {:?}",
            new_chunk_lines.len(),
            load_index,
            self.loaded_chunks,
        );
        self.update_chunk_lines(load_index, new_chunk_lines);
    }

    fn resolve_offset(&mut self, from: SeekFrom) -> u64 {
        match from {
            SeekFrom::Start(x) => x,
            SeekFrom::End(x) => (self.file.metadata().unwrap().len() as i64 + x)
                .try_into()
                .unwrap(),
            SeekFrom::Current(x) => x.try_into().unwrap(), // current behaves like start
        }
    }

    pub fn offset_to_line(&mut self, from: SeekFrom) -> LineIndex {
        // TODO This is inefficient
        let offset = self.resolve_offset(from);
        for (index, line) in self.chunk_lines.iter().enumerate() {
            match line.loaded_loc {
                Some(loc) if loc.loaded_offset >= offset => {
                    return LineIndex {
                        relative: index.try_into().unwrap(),
                        offset_version: self.offset_version,
                    };
                }
                _ => {
                    continue;
                }
            };
        }
        return LineIndex {
            relative: self.chunk_lines.len().try_into().unwrap(),
            offset_version: self.offset_version,
        };
    }

    fn update_chunk_lines(&mut self, new_index: ChunkIndex, mut new_chunk_lines: Vec<EditLine>) {
        if !self.loaded_chunks.is_empty()
            && new_index.offset == self.loaded_chunks.last_key_value().unwrap().1.end_offset()
        {
            log!("appending loaded lines after existing lines");
            self.loaded_chunks.insert(new_index.offset, new_index);
            // append new lines to existing lines
            // line_index is relative to the range start which stays unchanged.
            self.chunk_lines
                .last_mut()
                .unwrap()
                .line
                .extend(new_chunk_lines.remove(0));

            populate_lines(new_index, new_chunk_lines, &mut self.chunk_lines);
        } else if !self.loaded_chunks.is_empty()
            && new_index.end_offset() == self.loaded_chunks.first_key_value().unwrap().1.offset
        {
            let len: i64 = new_chunk_lines.len().try_into().unwrap();
            log!("prepending loaded lines before existing lines, old self.line_offset: {:?}, len: {:?}", self.line_anchor, len);
            self.loaded_chunks.insert(0, new_index);
            // append existing lines to new lines
            // line indexes are relative to the range start, which was pushed up by the new chunk
            self.line_anchor = self.line_anchor + len;
            let mut lines: Vec<LoadedLine> = vec![];
            populate_lines(new_index, new_chunk_lines, &mut lines);
            std::mem::swap(&mut self.chunk_lines, &mut lines);
            self.chunk_lines
                .last_mut()
                .unwrap()
                .line
                .extend(*lines.remove(0).line);
            self.chunk_lines.append(&mut lines);
        } else {
            log!("dropping existing lines, replacing with new lines");
            // replace existing lines
            self.loaded_chunks.clear();
            self.loaded_chunks.insert(new_index.offset, new_index);
            self.chunk_lines.clear();
            populate_lines(new_index, new_chunk_lines, &mut self.chunk_lines);
            self.line_anchor = 0;
            self.offset_version += 1;
            log!(
                "self.line_offset: {:?}, chunk_lines.len: {:?}",
                self.line_anchor,
                self.chunk_lines.len()
            );
        };
    }

    pub fn prev_line(&mut self, line_index: &LineIndex) -> Option<LineIndex> {
        if self.offset_version != line_index.offset_version {
            return None;
        }
        let prev_line_index = line_index.plus(-1);
        if let Some(_prev_abs_index) = self.to_abs_index(&prev_line_index) {
            // previous line is loaded
            return Some(prev_line_index);
        }
        if let Some(cur_abs_index) = self.to_abs_index(line_index) {
            let line = self.chunk_lines.get(cur_abs_index).unwrap();
            match line.loaded_loc {
                None => {
                    // current line exists but has no file location
                    return None;
                }
                Some(line_loc) => {
                    // current line is loaded and has a file location, get previous chunk
                    // seek to previous chunk
                    let prev_chunk_offset = line_loc.loaded_offset.saturating_sub(self.chunk_size);
                    self.load_lines(prev_chunk_offset);
                    // shouldn't invalidate the offset version, this chunk should be just before the first loaded chunk
                    assert!(line_index.offset_version == self.offset_version);
                    // after possible seek, index may still be zero if there was nothing to load
                    if line_loc.loaded_offset > 0 {
                        return Some(LineIndex {
                            relative: line_index.relative - 1,
                            offset_version: line_index.offset_version,
                        });
                    }
                    return Some(line_index.clone());
                }
            }
        }
        // No current line, no previous line ???
        return None;
    }

    pub fn next_line(&mut self, line_index: &LineIndex) -> Option<LineIndex> {
        let index = self.to_abs_index(&line_index);
        if index.is_none() {
            return None;
        }
        let index = index.unwrap();
        if index + 2 >= self.chunk_lines.len() {
            self.load_more_lines();
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

    fn load_more_lines(&mut self) {
        match self.loaded_chunks.last_key_value() {
            Some((_, i)) => {
                // fetch more lines, after increasing index it will be the last line which may be incomplete
                self.load_lines(i.end_offset());
            }
            _ => {}
        }
    }

    pub fn remove(&mut self, line_index: &LineIndex) -> Option<EditLine> {
        let index = self.to_abs_index(&line_index);
        if index.is_none() {
            return None;
        }
        let index = index.unwrap();
        if index + 2 >= self.chunk_lines.len() {
            self.load_more_lines();
            assert!(line_index.offset_version == self.offset_version);
        }
        let removed_line = self.chunk_lines.remove(index);
        if self.chunk_lines.len() == 0 {
            // that was the only line left, add one back to avoid empty
            self.chunk_lines.push(LoadedLine::new(EditLine::empty()));
        }
        return Some(*removed_line.line);
    }

    pub fn insert_after(&mut self, line_index: &LineIndex, new_line: EditLine) -> Option<()> {
        match self.to_abs_index(&line_index) {
            None => return None,
            Some(index) => {
                self.chunk_lines
                    .insert(index + 1, LoadedLine::new(new_line));
                return Some(());
            }
        }
    }

    pub fn get(&self, line_index: &LineIndex) -> Option<&EditLine> {
        match self.to_abs_index(&line_index) {
            None => return None,
            Some(index) => {
                return self.chunk_lines.get(index).map(|x| &*x.line);
            }
        }
    }

    pub fn get_mut(&mut self, line_index: &LineIndex) -> Option<&mut EditLine> {
        match self.to_abs_index(&line_index) {
            None => return None,
            Some(index) => {
                return self.chunk_lines.get_mut(index).map(|x| &mut *x.line);
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
    ) -> impl Iterator<Item = &LoadedLine> {
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
        log!(
            "line_index: {:?}, self.offset_version: {:?}, self.line_offset: {:?}",
            line_index,
            self.offset_version,
            self.line_anchor
        );

        if self.offset_version != line_index.offset_version {
            return None;
        }
        let range = std::ops::Range::<i64> {
            start: 0,
            end: self.chunk_lines.len() as i64,
        };
        let index = line_index.relative + self.line_anchor;
        log!("index: {:?}, range: {:?}", index, range);
        if !range.contains(&index) {
            return None;
        }
        Some(index.try_into().unwrap())
    }
}

fn populate_lines(
    new_index: ChunkIndex,
    new_chunk_lines: Vec<EditLine>,
    lines: &mut Vec<LoadedLine>,
) {
    let mut offset = new_index.offset;
    for new_line in new_chunk_lines {
        let loaded_line = LoadedLine::from_loaded(new_line, offset);
        offset += loaded_line.loaded_loc.unwrap().loaded_size;
        lines.push(loaded_line);
    }
    // chunk could have been a 'short read', so total data <= size
    assert!(
        offset <= new_index.end_offset(),
        "{:?} <= {:?} (index: {:?}), lines:\n{:?}",
        offset,
        new_index.end_offset(),
        new_index,
        lines,
    );
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;
    use crate::VirtualFile;
    use tempfile::tempfile;

    fn create_test_file(content: &str) -> std::fs::File {
        let mut file = tempfile().unwrap();
        file.write_all(content.as_bytes()).unwrap();
        file
    }

    #[test]
    fn test_virtual_file_empty() {
        let file = create_test_file("");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(SeekFrom::Start(0));
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
        vf.seek(SeekFrom::Start(0));
        vf.seek(SeekFrom::Start(11));
        vf.seek(SeekFrom::Start(0));
    }

    #[test]
    fn test_virtual_file_next_line() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(SeekFrom::Start(0));
        let line_index = vf.get_index();
        assert_eq!(vf.get(&line_index).unwrap().str(), "line1");
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
    fn test_virtual_file_prev_line() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(SeekFrom::End(0));
        let line_index = vf.get_index();
        log!("line_index: {:?}", line_index);
        assert_eq!(vf.get(&line_index).unwrap().str(), "");
        let line_index = vf.prev_line(&line_index).unwrap();
        assert_eq!(vf.get(&line_index).unwrap().str(), "line3");
        let line_index = vf.prev_line(&line_index).unwrap();
        assert_eq!(vf.get(&line_index).unwrap().str(), "line2");
        let line_index = vf.prev_line(&line_index).unwrap();
        assert_eq!(vf.get(&line_index).unwrap().str(), "line1");
        let line_index = vf.prev_line(&line_index).unwrap();
        assert_eq!(vf.get(&line_index).unwrap().str(), "line1");
        let last = vf.prev_line(&line_index);
        assert_eq!(last, Some(line_index));
    }

    #[test]
    fn test_virtual_file_remove() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(SeekFrom::Start(0));
        let line_index = vf.get_index();
        assert_eq!(vf.remove(&line_index).unwrap().str(), "line1");
        assert_eq!(vf.get(&line_index).unwrap().str(), "line2");
    }

    #[test]
    fn test_virtual_file_insert() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(SeekFrom::Start(0));
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
        vf.seek(SeekFrom::Start(0));
        let line_index = vf.get_index();
        assert_eq!(vf.get(&line_index).unwrap().str(), "line1");
    }

    #[test]
    fn test_virtual_file_get_mut() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(SeekFrom::Start(0));
        let line_index = vf.get_index();

        let line = vf.get_mut(&line_index).unwrap();
        line.overwrite(0, 'b');
        assert_eq!(vf.get(&line_index).unwrap().str(), "bine1");
    }

    #[test]
    fn test_virtual_file_iter_at() {
        let file = create_test_file("line1\nline2\nline3\n");
        let mut vf = VirtualFile::new(10, file);
        vf.seek(SeekFrom::Start(0));
        let line_index = vf.get_index();
        let mut iter = vf.iter_at(&line_index, 3);
        assert_eq!(iter.next().unwrap().line().str(), "line1");
        assert_eq!(iter.next().unwrap().line().str(), "line2");
        assert_eq!(iter.next().unwrap().line().str(), "line3");
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
