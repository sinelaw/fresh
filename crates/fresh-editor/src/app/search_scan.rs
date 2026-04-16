//! Self-contained state for an incremental search scan on large files.
//!
//! The actual chunk-by-chunk matching runs inside
//! `TextBuffer::search_scan_next_chunk` via a `ChunkedSearchState`. This
//! subsystem wraps that with the per-session metadata — which buffer is
//! being searched, the original query string and flags, the leaf snapshot
//! needed to refresh the piece tree on completion — plus the narrow API
//! that the orchestrator on `Editor` calls.
//!
//! Keeping the state here means the take-and-put-back dance the
//! orchestrator used to do on a raw `Option<SearchScanState>` field is
//! expressed as explicit `take_chunked` / `restore_chunked` calls,
//! preserving the "single mutable owner" discipline without coupling the
//! buffer registry into this module.

use std::ops::Range;

use crate::model::buffer::ChunkedSearchState;
use crate::model::event::BufferId;
use crate::model::piece_tree::LeafData;

/// Everything the orchestrator needs at scan finalization.
pub(crate) struct FinishedSearchScan {
    pub buffer_id: BufferId,
    pub query: String,
    pub match_ranges: Vec<(usize, usize)>,
    pub capped: bool,
}

struct Active {
    buffer_id: BufferId,
    #[allow(dead_code)] // retained for future "refresh saved root" hook points
    leaves: Vec<LeafData>,
    /// Inner chunked-search state. Extracted via `take_chunked` while a
    /// batch is running so the caller can pass `&mut self.buffers` into
    /// `search_scan_next_chunk` without violating the borrow checker, and
    /// put back via `restore_chunked`.
    chunked: Option<ChunkedSearchState>,
    query: String,
    #[allow(dead_code)]
    search_range: Option<Range<usize>>,
    #[allow(dead_code)]
    case_sensitive: bool,
    #[allow(dead_code)]
    whole_word: bool,
    #[allow(dead_code)]
    use_regex: bool,
}

/// Owner of the optional in-flight search scan.
#[derive(Default)]
pub(crate) struct SearchScan {
    active: Option<Active>,
}

impl SearchScan {
    // ---- Queries -----------------------------------------------------------

    pub(crate) fn buffer_id(&self) -> Option<BufferId> {
        self.active.as_ref().map(|a| a.buffer_id)
    }

    /// Whether the inner chunked state has no more chunks to process.
    /// Returns `true` when there is no scan — "done by absence".
    pub(crate) fn is_done(&self) -> bool {
        match self.active.as_ref().and_then(|a| a.chunked.as_ref()) {
            Some(c) => c.is_done(),
            None => true,
        }
    }

    pub(crate) fn progress_percent(&self) -> usize {
        self.active
            .as_ref()
            .and_then(|a| a.chunked.as_ref())
            .map(|c| c.progress_percent())
            .unwrap_or(100)
    }

    pub(crate) fn match_count(&self) -> usize {
        self.active
            .as_ref()
            .and_then(|a| a.chunked.as_ref())
            .map(|c| c.matches.len())
            .unwrap_or(0)
    }

    // ---- Lifecycle ---------------------------------------------------------

    /// Begin a new scan. Discards any previous in-flight scan.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn start(
        &mut self,
        buffer_id: BufferId,
        leaves: Vec<LeafData>,
        chunked: ChunkedSearchState,
        query: String,
        search_range: Option<Range<usize>>,
        case_sensitive: bool,
        whole_word: bool,
        use_regex: bool,
    ) {
        self.active = Some(Active {
            buffer_id,
            leaves,
            chunked: Some(chunked),
            query,
            search_range,
            case_sensitive,
            whole_word,
            use_regex,
        });
    }

    /// Extract the inner `ChunkedSearchState` so the caller can pass it
    /// by mutable reference into `TextBuffer::search_scan_next_chunk`
    /// alongside `&mut self.buffers`. The caller **must** call
    /// [`restore_chunked`] afterwards or subsequent calls will see no
    /// active chunked state.
    pub(crate) fn take_chunked(&mut self) -> Option<ChunkedSearchState> {
        self.active.as_mut()?.chunked.take()
    }

    /// Put the chunked state back after a batch has run.
    pub(crate) fn restore_chunked(&mut self, chunked: ChunkedSearchState) {
        if let Some(active) = self.active.as_mut() {
            active.chunked = Some(chunked);
        }
    }

    /// Consume the scan and return the fields the orchestrator needs to
    /// finalize (populate `search_state`, show a status message, or
    /// refresh the piece tree's saved root).
    pub(crate) fn take_finished(&mut self) -> Option<FinishedSearchScan> {
        let active = self.active.take()?;
        let chunked = active.chunked?;
        let match_ranges = chunked
            .matches
            .iter()
            .map(|m| (m.byte_offset, m.length))
            .collect();
        Some(FinishedSearchScan {
            buffer_id: active.buffer_id,
            query: active.query,
            match_ranges,
            capped: chunked.capped,
        })
    }

    /// Drop the scan without finalizing — used on I/O error.
    /// Returns the buffer id of the abandoned scan, if any.
    pub(crate) fn abandon(&mut self) -> Option<BufferId> {
        self.active.take().map(|a| a.buffer_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::ChunkedSearchState;

    fn dummy_chunked(done: bool) -> ChunkedSearchState {
        // `is_done` on ChunkedSearchState is `next_chunk >= chunks.len() ||
        // capped`, so to produce a "not done" scan we need at least one
        // chunk with next_chunk < len.
        let chunks = if done {
            Vec::new()
        } else {
            vec![crate::model::buffer::LineScanChunk {
                leaf_index: 0,
                byte_len: 100,
                already_known: false,
            }]
        };
        ChunkedSearchState {
            chunks,
            next_chunk: 0,
            next_doc_offset: 0,
            total_bytes: if done { 0 } else { 100 },
            scanned_bytes: 0,
            regex: regex::bytes::Regex::new("x").unwrap(),
            matches: Vec::new(),
            overlap_tail: Vec::new(),
            overlap_doc_offset: 0,
            max_matches: 10,
            capped: false,
            query_len: 1,
            running_line: 0,
        }
    }

    #[test]
    fn default_is_inactive_and_vacuously_done() {
        let s = SearchScan::default();
        assert_eq!(s.buffer_id(), None);
        assert!(s.is_done());
        assert_eq!(s.progress_percent(), 100);
        assert_eq!(s.match_count(), 0);
    }

    #[test]
    fn start_populates_queries() {
        let mut s = SearchScan::default();
        s.start(
            BufferId(3),
            Vec::new(),
            dummy_chunked(false),
            "foo".to_string(),
            None,
            true,
            false,
            false,
        );
        assert_eq!(s.buffer_id(), Some(BufferId(3)));
        assert!(!s.is_done());
    }

    #[test]
    fn take_and_restore_chunked_round_trip() {
        let mut s = SearchScan::default();
        s.start(
            BufferId(1),
            Vec::new(),
            dummy_chunked(false),
            "q".to_string(),
            None,
            true,
            false,
            false,
        );

        let chunked = s.take_chunked().expect("chunked available first time");
        // After take, queries that look at the inner chunked fall back.
        assert!(s.is_done()); // no inner chunked => vacuously done
        assert_eq!(s.progress_percent(), 100);

        s.restore_chunked(chunked);
        // Back in the normal state.
        assert!(!s.is_done());
    }

    #[test]
    fn take_chunked_twice_without_restore_returns_none() {
        let mut s = SearchScan::default();
        s.start(
            BufferId(1),
            Vec::new(),
            dummy_chunked(false),
            "q".to_string(),
            None,
            true,
            false,
            false,
        );
        assert!(s.take_chunked().is_some());
        assert!(s.take_chunked().is_none());
    }

    #[test]
    fn take_finished_drains_and_returns_match_ranges() {
        let mut chunked = dummy_chunked(true);
        chunked.matches.push(crate::model::buffer::SearchMatch {
            byte_offset: 12,
            length: 3,
            line: 0,
            column: 0,
            context: String::new(),
        });
        chunked.capped = true;

        let mut s = SearchScan::default();
        s.start(
            BufferId(7),
            Vec::new(),
            chunked,
            "abc".to_string(),
            None,
            true,
            false,
            false,
        );
        let finished = s.take_finished().unwrap();
        assert_eq!(finished.buffer_id, BufferId(7));
        assert_eq!(finished.query, "abc");
        assert_eq!(finished.match_ranges, vec![(12, 3)]);
        assert!(finished.capped);
        // Scan is drained.
        assert_eq!(s.buffer_id(), None);
    }

    #[test]
    fn abandon_clears_state_without_finalizing() {
        let mut s = SearchScan::default();
        s.start(
            BufferId(5),
            Vec::new(),
            dummy_chunked(false),
            "q".to_string(),
            None,
            true,
            false,
            false,
        );
        assert_eq!(s.abandon(), Some(BufferId(5)));
        assert_eq!(s.buffer_id(), None);
        assert!(s.take_finished().is_none());
    }
}
