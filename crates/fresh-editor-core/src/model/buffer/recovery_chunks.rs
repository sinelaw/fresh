//! The changes a large file's buffer makes to the file on disk, for
//! recovery without writing the whole file.

use super::TextBuffer;
use crate::model::filesystem::FileSystem;
use crate::model::piece_tree::{BufferData, BufferLocation};
use crate::recovery_types::RecoveryChunk;
use std::io;
use std::ops::Range;
use std::path::Path;

/// Changes to a large file this close together are kept as one recovery
/// chunk, the unchanged bytes between them included: each chunk costs index
/// space, and a few hundred bytes cost less.
pub const RECOVERY_MERGE_GAP: usize = 1024;

impl TextBuffer {
    /// The changes to the file on disk that make it this buffer's content.
    ///
    /// Each chunk replaces `original_len` bytes of the ORIGINAL file at
    /// `offset` with `content`; the chunks are sorted and don't overlap, so
    /// applying them to the file in order (or in reverse, to a buffer holding
    /// it) gives this buffer's content. Changes separated by at most
    /// [`RECOVERY_MERGE_GAP`] unchanged bytes are merged into one chunk, the
    /// bytes between them included, so a replace-all of many matches costs
    /// about the bytes it spans, not a chunk per match (issue #3645).
    /// Unchanged bytes not in memory are read from the file, one read per
    /// chunk.
    pub fn get_recovery_chunks(&self) -> io::Result<Vec<RecoveryChunk>> {
        let mut chunker = Chunker {
            fs: self.persistence.fs().as_ref(),
            chunks: Vec::new(),
            pending: None,
            gap: Some((Vec::new(), 0)),
            orig_cursor: 0,
        };
        for piece in self.piece_tree.iter_pieces_in_range(0, self.total_bytes()) {
            let buffer = self.buffers.get(piece.location.buffer_id());
            let range = piece.buffer_offset..piece.buffer_offset + piece.bytes;
            let part = buffer.and_then(|b| match &b.data {
                BufferData::Loaded { data, .. } => data.get(range.clone()).map(Part::Bytes),
                BufferData::Unloaded {
                    file_path,
                    file_offset,
                    ..
                } => Some(Part::File(
                    file_path,
                    file_offset + range.start..file_offset + range.end,
                )),
            });
            // Where this piece's bytes are in the original file, if they are
            // the original's. A chunk loaded from the file for display is an
            // Added buffer that records where it came from (see
            // `rebuild_with_pristine_saved_root`).
            let original_offset = match piece.location {
                BufferLocation::Stored(_) => Some(piece.buffer_offset),
                BufferLocation::Added(_) => buffer
                    .and_then(|b| b.stored_file_offset)
                    .map(|file_offset| file_offset + piece.buffer_offset),
            };
            match original_offset {
                Some(at) if at >= chunker.orig_cursor => {
                    chunker.unchanged(at, piece.bytes, part)?
                }
                // New text, or (never expected) original text out of order,
                // which is new text where it now is
                _ => chunker.change(0, part)?,
            }
        }
        chunker.finish(self.original_file_size().unwrap_or(0))
    }
}

/// A stretch of a chunk's content: bytes in memory, or a range of a file
/// not loaded, read once the chunk is complete.
enum Part<'a> {
    Bytes(&'a [u8]),
    File(&'a Path, Range<usize>),
}

/// A chunk still being added to.
struct Pending<'a> {
    offset: usize,
    original_len: usize,
    parts: Vec<Part<'a>>,
}

/// Builds the chunks from the buffer's pieces, in document order.
struct Chunker<'a> {
    fs: &'a (dyn FileSystem + Send + Sync),
    chunks: Vec<RecoveryChunk>,
    pending: Option<Pending<'a>>,
    /// The unchanged stretch since `pending` ends, and its length, while it
    /// is short enough to be merged into it if another change follows.
    gap: Option<(Vec<Part<'a>>, usize)>,
    /// Where in the original file the next unchanged byte is expected.
    orig_cursor: usize,
}

impl<'a> Chunker<'a> {
    /// `bytes` of the original file at `at`, unchanged, as `part`: whatever
    /// of the file was skipped to reach them is deleted.
    fn unchanged(&mut self, at: usize, bytes: usize, part: Option<Part<'a>>) -> io::Result<()> {
        if at > self.orig_cursor {
            self.change(at - self.orig_cursor, None)?;
        }
        if self.pending.is_some() {
            match (self.gap.as_mut(), part) {
                (Some((unchanged, len)), Some(part)) if *len + bytes <= RECOVERY_MERGE_GAP => {
                    unchanged.push(part);
                    *len += bytes;
                }
                _ => self.gap = None,
            }
        }
        self.orig_cursor = at + bytes;
        Ok(())
    }

    /// `original_len` bytes of the original file at the cursor replaced by
    /// `content` (none: deleted).
    fn change(&mut self, original_len: usize, content: Option<Part<'a>>) -> io::Result<()> {
        match (self.pending.as_mut(), self.gap.take()) {
            (Some(chunk), Some((unchanged, len))) => {
                chunk.original_len += len + original_len;
                chunk.parts.extend(unchanged);
                chunk.parts.extend(content);
            }
            _ => {
                let next = Pending {
                    offset: self.orig_cursor,
                    original_len,
                    parts: content.into_iter().collect(),
                };
                if let Some(done) = self.pending.replace(next) {
                    let chunk = self.complete(done)?;
                    self.chunks.push(chunk);
                }
            }
        }
        self.orig_cursor += original_len;
        self.gap = Some((Vec::new(), 0));
        Ok(())
    }

    /// The chunks, once every piece is seen: what is left of the original
    /// file past the last of them was deleted.
    fn finish(mut self, original_size: usize) -> io::Result<Vec<RecoveryChunk>> {
        if self.orig_cursor < original_size {
            self.change(original_size - self.orig_cursor, None)?;
        }
        if let Some(done) = self.pending.take() {
            let chunk = self.complete(done)?;
            self.chunks.push(chunk);
        }
        Ok(self.chunks)
    }

    fn complete(&self, pending: Pending<'a>) -> io::Result<RecoveryChunk> {
        // The unloaded stretches are of the one file, near each other: read
        // the span they are in at once
        let mut files = pending.parts.iter().filter_map(|part| match part {
            Part::File(path, range) => Some((*path, range)),
            Part::Bytes(_) => None,
        });
        let span = match files.next() {
            Some((path, first)) => {
                let (start, end) = files.fold((first.start, first.end), |(s, e), (p, r)| {
                    debug_assert_eq!(p, path, "one original file");
                    (s.min(r.start), e.max(r.end))
                });
                let bytes = self.fs.read_range(path, start as u64, end - start)?;
                if bytes.len() != end - start {
                    return Err(io::Error::new(
                        io::ErrorKind::UnexpectedEof,
                        format!("{} is shorter than when it was opened", path.display()),
                    ));
                }
                Some((start, bytes))
            }
            None => None,
        };
        let mut content = Vec::new();
        for part in &pending.parts {
            match (part, &span) {
                (Part::Bytes(bytes), _) => content.extend_from_slice(bytes),
                (Part::File(_, range), Some((start, bytes))) => {
                    content.extend_from_slice(&bytes[range.start - start..range.end - start])
                }
                (Part::File(..), None) => unreachable!("a file part makes a span"),
            }
        }
        Ok(RecoveryChunk::new(
            pending.offset,
            pending.original_len,
            content,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;
    use proptest::prelude::*;
    use std::sync::Arc;

    /// `original` with `chunks` applied, as recovery applies them.
    fn apply(original: &[u8], chunks: &[RecoveryChunk]) -> Vec<u8> {
        let mut out = Vec::new();
        let mut pos = 0;
        for chunk in chunks {
            assert!(chunk.offset >= pos, "chunks out of order or overlapping");
            out.extend_from_slice(&original[pos..chunk.offset]);
            out.extend_from_slice(&chunk.content);
            pos = chunk.offset + chunk.original_len;
        }
        out.extend_from_slice(&original[pos..]);
        out
    }

    /// `content` written to a file and opened in large-file mode.
    fn large_file(content: &[u8]) -> (tempfile::TempDir, TextBuffer) {
        let dir = tempfile::TempDir::new().unwrap();
        let path = dir.path().join("large.txt");
        std::fs::write(&path, content).unwrap();
        let buffer = TextBuffer::load_from_file(&path, 64, Arc::new(StdFileSystem)).unwrap();
        assert!(buffer.is_large_file());
        (dir, buffer)
    }

    fn content(buffer: &mut TextBuffer) -> Vec<u8> {
        let total = buffer.total_bytes();
        buffer.get_text_range_mut(0, total).unwrap()
    }

    #[derive(Debug, Clone)]
    enum Edit {
        Insert(prop::sample::Index, Vec<u8>),
        Delete(prop::sample::Index, usize),
    }

    fn edit() -> impl Strategy<Value = Edit> {
        prop_oneof![
            (
                any::<prop::sample::Index>(),
                prop::collection::vec(b'A'..=b'Z', 0..20)
            )
                .prop_map(|(at, text)| Edit::Insert(at, text)),
            (any::<prop::sample::Index>(), 1usize..300).prop_map(|(at, n)| Edit::Delete(at, n)),
        ]
    }

    proptest! {
        /// Recovery round-trip: the chunks applied to the file on disk give
        /// the edited buffer, for any edits, with the file loaded or not.
        /// Chunks are sorted, apart, and (with the bytes between them in
        /// memory or not) merged when closer than `RECOVERY_MERGE_GAP`.
        #[test]
        fn recovery_chunks_rebuild_the_buffer(
            original in prop::collection::vec(prop_oneof![b'a'..=b'z', Just(b'\n')], 100..6000),
            edits in prop::collection::vec(edit(), 0..40),
            preload in any::<bool>(),
        ) {
            let (_dir, mut buffer) = large_file(&original);
            if preload {
                content(&mut buffer);
            }
            let mut expected = original.clone();
            for edit in &edits {
                match edit {
                    Edit::Insert(at, text) => {
                        let at = at.index(expected.len() + 1);
                        buffer.insert_bytes(at, text.clone());
                        expected.splice(at..at, text.iter().copied());
                    }
                    Edit::Delete(at, n) => {
                        if expected.is_empty() {
                            continue;
                        }
                        let at = at.index(expected.len());
                        let n = (*n).min(expected.len() - at);
                        buffer.delete_bytes(at, n);
                        expected.drain(at..at + n);
                    }
                }
            }
            let chunks = buffer.get_recovery_chunks().unwrap();
            prop_assert_eq!(apply(&original, &chunks), expected.clone());
            for pair in chunks.windows(2) {
                let gap = pair[1].offset - (pair[0].offset + pair[0].original_len);
                prop_assert!(gap > RECOVERY_MERGE_GAP, "chunks {} bytes apart not merged", gap);
            }
            prop_assert_eq!(content(&mut buffer), expected);
        }
    }

    /// The issue's case: a replace-all of a match on every line of a file
    /// never scrolled through is one chunk, not one per match (#3645).
    #[test]
    fn replace_all_in_an_unloaded_file_is_one_chunk() {
        let original = "lorem ipsum dolor\n".repeat(5000);
        let (_dir, mut buffer) = large_file(original.as_bytes());
        let matches: Vec<usize> = original.match_indices("lorem").map(|(i, _)| i).collect();
        for &at in matches.iter().rev() {
            buffer.delete_bytes(at, "lorem".len());
            buffer.insert_bytes(at, b"L".to_vec());
        }
        let chunks = buffer.get_recovery_chunks().unwrap();
        assert_eq!(chunks.len(), 1);
        let expected = original.replace("lorem", "L");
        assert_eq!(apply(original.as_bytes(), &chunks), expected.as_bytes());
    }

    /// Edits far apart stay separate chunks, the bytes between them not
    /// written; an unedited buffer has none.
    #[test]
    fn distant_edits_are_separate_chunks() {
        let original = vec![b'x'; 100_000];
        let (_dir, mut buffer) = large_file(&original);
        assert!(buffer.get_recovery_chunks().unwrap().is_empty());
        buffer.insert_bytes(90_000, b"END".to_vec());
        buffer.delete_bytes(50_000, 10);
        buffer.insert_bytes(0, b"START".to_vec());
        let chunks = buffer.get_recovery_chunks().unwrap();
        let shape: Vec<_> = chunks
            .iter()
            .map(|c| (c.offset, c.original_len, c.content.as_slice()))
            .collect();
        assert_eq!(
            shape,
            vec![
                (0, 0, &b"START"[..]),
                (50_000, 10, &b""[..]),
                (90_000, 0, &b"END"[..]),
            ]
        );
    }
}
