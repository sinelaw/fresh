/// Standalone search types and helpers extracted from mod.rs.
///
/// - `ChunkedSearchState` — mutable state for incremental chunked search
/// - `HybridSearchPlan` + `SearchRegion` — plan for hybrid (unloaded + loaded) search
/// - `search_boundary_overlap` — overlap-zone regex matching across region boundaries

use crate::model::filesystem::{FileSearchCursor, FileSearchOptions, FileSystem, SearchMatch};
use regex::bytes::Regex;
use std::io;
use std::path::PathBuf;

use super::LineScanChunk;

// ---------------------------------------------------------------------------
// ChunkedSearchState
// ---------------------------------------------------------------------------

/// Mutable state for an incremental chunked search over a TextBuffer's
/// piece tree.  This is the in-editor search path — it reads chunks via
/// `get_text_range_mut` which loads lazily from disk and works with the
/// piece tree's edit history.
///
/// For searching files on disk (project-wide grep), see
/// `FileSystem::search_file` which uses `read_range` and doesn't need
/// a TextBuffer at all.
///
/// Created by `TextBuffer::search_scan_init`, advanced by
/// `TextBuffer::search_scan_next_chunk`.  The same struct is used by
/// both the Editor's incremental (non-blocking) search and the project-
/// wide search running inside `spawn_blocking`.
#[derive(Debug)]
pub struct ChunkedSearchState {
    /// One work item per piece-tree leaf (after `prepare_line_scan` splits).
    pub chunks: Vec<LineScanChunk>,
    /// Index of the next chunk to process.
    pub next_chunk: usize,
    /// Running document byte offset for the next chunk.
    pub next_doc_offset: usize,
    /// Total bytes in the buffer.
    pub total_bytes: usize,
    /// Bytes scanned so far (for progress reporting).
    pub scanned_bytes: usize,
    /// Compiled regex for searching.
    pub regex: regex::bytes::Regex,
    /// Accumulated match results with line/column/context.
    pub matches: Vec<SearchMatch>,
    /// Tail bytes from the previous chunk for cross-boundary matching.
    pub overlap_tail: Vec<u8>,
    /// Byte offset of the overlap_tail's first byte in the document.
    pub overlap_doc_offset: usize,
    /// Maximum number of matches before capping.
    pub max_matches: usize,
    /// Whether the match count was capped.
    pub capped: bool,
    /// Length of the original query string (for overlap sizing).
    pub query_len: usize,
    /// 1-based line number at the start of the next non-overlap data.
    /// Advanced incrementally as chunks are processed.
    pub(crate) running_line: usize,
}

impl ChunkedSearchState {
    /// Returns true if the scan is complete (all chunks processed or capped).
    pub fn is_done(&self) -> bool {
        self.next_chunk >= self.chunks.len() || self.capped
    }

    /// Progress as a percentage (0–100).
    pub fn progress_percent(&self) -> usize {
        if self.total_bytes > 0 {
            (self.scanned_bytes * 100) / self.total_bytes
        } else {
            100
        }
    }
}

// ---------------------------------------------------------------------------
// SearchRegion / HybridSearchPlan
// ---------------------------------------------------------------------------

/// A region in a hybrid search plan — either an unloaded file range or
/// in-memory data from the piece tree.
#[derive(Debug)]
pub(crate) enum SearchRegion {
    /// Contiguous range on the original file that hasn't been loaded.
    Unloaded {
        file_offset: usize,
        bytes: usize,
        doc_offset: usize,
    },
    /// In-memory data (loaded original content or user edits).
    Loaded { data: Vec<u8>, doc_offset: usize },
}

/// A plan for hybrid search — extracted from a `TextBuffer`'s piece tree
/// on the main thread, executable on any thread.
///
/// For a large remote file with a small edit, the plan captures the few
/// loaded regions (small) and unloaded file ranges (coordinates only).
/// `execute()` then searches unloaded regions via `fs.search_file` (no data
/// transfer) and loaded regions with in-memory regex.
#[derive(Debug)]
pub struct HybridSearchPlan {
    pub(crate) file_path: PathBuf,
    pub(crate) regions: Vec<SearchRegion>,
}

impl HybridSearchPlan {
    /// Execute the search plan.  Can run on any thread — only needs a
    /// `FileSystem` reference for unloaded region searches.
    pub fn execute(
        &self,
        fs: &dyn FileSystem,
        pattern: &str,
        opts: &FileSearchOptions,
        regex: &Regex,
        max_matches: usize,
        query_len: usize,
    ) -> io::Result<Vec<SearchMatch>> {
        if self.regions.is_empty() {
            return Ok(vec![]);
        }

        // Fast path: single unloaded region → search whole file
        if self.regions.len() == 1 {
            if let SearchRegion::Unloaded { .. } = &self.regions[0] {
                let mut cursor = FileSearchCursor::new();
                let mut all_matches = Vec::new();
                while !cursor.done && all_matches.len() < max_matches {
                    let batch = fs.search_file(&self.file_path, pattern, opts, &mut cursor)?;
                    all_matches.extend(batch);
                }
                all_matches.truncate(max_matches);
                return Ok(all_matches);
            }
        }

        let overlap_size = query_len.max(256);
        let mut all_matches: Vec<SearchMatch> = Vec::new();
        let mut running_line: usize = 1;
        let mut prev_tail: Vec<u8> = Vec::new();

        for region in &self.regions {
            if all_matches.len() >= max_matches {
                break;
            }
            let remaining = max_matches - all_matches.len();

            match region {
                SearchRegion::Unloaded {
                    file_offset,
                    bytes,
                    doc_offset: region_doc_offset,
                } => {
                    // Boundary overlap: prev_tail + start of unloaded region
                    if !prev_tail.is_empty() {
                        let overlap_read = (*bytes).min(overlap_size);
                        if let Ok(head) =
                            fs.read_range(&self.file_path, *file_offset as u64, overlap_read)
                        {
                            let boundary = search_boundary_overlap(
                                &prev_tail,
                                &head,
                                *region_doc_offset - prev_tail.len(),
                                running_line,
                                regex,
                                remaining,
                            );
                            all_matches.extend(boundary);
                        }
                    }

                    // Search unloaded range via fs.search_file
                    let mut opts_bounded = opts.clone();
                    opts_bounded.max_matches = remaining.saturating_sub(all_matches.len());
                    let mut cursor = FileSearchCursor::for_range(
                        *file_offset,
                        *file_offset + *bytes,
                        running_line,
                    );
                    while !cursor.done && all_matches.len() < max_matches {
                        let mut batch =
                            fs.search_file(&self.file_path, pattern, &opts_bounded, &mut cursor)?;
                        // Remap byte_offset from file-relative to doc-relative
                        for m in &mut batch {
                            m.byte_offset = *region_doc_offset + (m.byte_offset - *file_offset);
                        }
                        all_matches.extend(batch);
                    }
                    running_line = cursor.running_line;

                    // Save tail for next boundary
                    if *bytes >= overlap_size {
                        let tail_off = *file_offset + *bytes - overlap_size;
                        prev_tail = fs
                            .read_range(&self.file_path, tail_off as u64, overlap_size)
                            .unwrap_or_default();
                    } else {
                        prev_tail = fs
                            .read_range(&self.file_path, *file_offset as u64, *bytes)
                            .unwrap_or_default();
                    }
                }
                SearchRegion::Loaded {
                    data,
                    doc_offset: region_doc_offset,
                } => {
                    // Build search buffer: overlap tail + loaded data
                    let mut search_buf = Vec::with_capacity(prev_tail.len() + data.len());
                    search_buf.extend_from_slice(&prev_tail);
                    search_buf.extend_from_slice(data);

                    let overlap_len = prev_tail.len();
                    let buf_doc_offset = if overlap_len > 0 {
                        *region_doc_offset - overlap_len
                    } else {
                        *region_doc_offset
                    };

                    let newlines_in_overlap = search_buf[..overlap_len]
                        .iter()
                        .filter(|&&b| b == b'\n')
                        .count();
                    let mut line_at = running_line.saturating_sub(newlines_in_overlap);
                    let mut counted_to = 0usize;

                    for m in regex.find_iter(&search_buf) {
                        if overlap_len > 0 && m.end() <= overlap_len {
                            continue;
                        }
                        if all_matches.len() >= max_matches {
                            break;
                        }

                        line_at += search_buf[counted_to..m.start()]
                            .iter()
                            .filter(|&&b| b == b'\n')
                            .count();
                        counted_to = m.start();

                        let line_start = search_buf[..m.start()]
                            .iter()
                            .rposition(|&b| b == b'\n')
                            .map(|p| p + 1)
                            .unwrap_or(0);
                        let line_end = search_buf[m.start()..]
                            .iter()
                            .position(|&b| b == b'\n')
                            .map(|p| m.start() + p)
                            .unwrap_or(search_buf.len());

                        let match_doc_offset = buf_doc_offset + m.start();
                        let column = m.start() - line_start + 1;
                        let context =
                            String::from_utf8_lossy(&search_buf[line_start..line_end]).into_owned();

                        all_matches.push(SearchMatch {
                            byte_offset: match_doc_offset,
                            length: m.end() - m.start(),
                            line: line_at,
                            column,
                            context,
                        });
                    }

                    running_line += data.iter().filter(|&&b| b == b'\n').count();

                    let tail_start = data.len().saturating_sub(overlap_size);
                    prev_tail = data[tail_start..].to_vec();
                }
            }
        }

        all_matches.truncate(max_matches);
        Ok(all_matches)
    }
}

// ---------------------------------------------------------------------------
// search_boundary_overlap
// ---------------------------------------------------------------------------

/// Search the overlap zone between two regions for matches that span the
/// boundary.  `prev_tail` is the tail of the previous region, `next_head`
/// is the head of the next region.  `doc_offset` is the document byte
/// offset of `prev_tail[0]`.  Only matches that cross the boundary (start
/// in tail, end in head) are returned — pure-tail matches were already found.
pub(crate) fn search_boundary_overlap(
    prev_tail: &[u8],
    next_head: &[u8],
    doc_offset: usize,
    running_line: usize,
    regex: &Regex,
    max_matches: usize,
) -> Vec<SearchMatch> {
    let mut buf = Vec::with_capacity(prev_tail.len() + next_head.len());
    buf.extend_from_slice(prev_tail);
    buf.extend_from_slice(next_head);

    let overlap_len = prev_tail.len();
    let newlines_before = prev_tail.iter().filter(|&&b| b == b'\n').count();
    let mut line_at = running_line.saturating_sub(newlines_before);
    let mut counted_to = 0usize;
    let mut matches = Vec::new();

    for m in regex.find_iter(&buf) {
        // Only keep matches that cross the boundary
        if m.start() < overlap_len && m.end() > overlap_len {
            if matches.len() >= max_matches {
                break;
            }

            line_at += buf[counted_to..m.start()]
                .iter()
                .filter(|&&b| b == b'\n')
                .count();
            counted_to = m.start();

            let line_start = buf[..m.start()]
                .iter()
                .rposition(|&b| b == b'\n')
                .map(|p| p + 1)
                .unwrap_or(0);
            let line_end = buf[m.start()..]
                .iter()
                .position(|&b| b == b'\n')
                .map(|p| m.start() + p)
                .unwrap_or(buf.len());

            let column = m.start() - line_start + 1;
            let context = String::from_utf8_lossy(&buf[line_start..line_end]).into_owned();

            matches.push(SearchMatch {
                byte_offset: doc_offset + m.start(),
                length: m.end() - m.start(),
                line: line_at,
                column,
                context,
            });
        }
    }
    matches
}
