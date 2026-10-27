//! An in-flight search of one buffer, run a step at a time.
//!
//! A search of a large file runs a slice per editor tick (see
//! `Editor::process_search_scan`) so it never stalls the editor; a small
//! one runs to the end in the call that starts it. Either way the matching
//! is a [`RegexSearch`] over the buffer.

use std::ops::Range;
use std::time::{Duration, Instant};

use crate::app::types::SearchState;
use crate::model::buffer::{RegexSearch, TextBuffer, LOAD_CHUNK_SIZE};
use crate::model::event::BufferId;

/// How long a search of a large file runs in each editor tick.
pub(crate) const SEARCH_SCAN_SLICE: Duration = Duration::from_millis(8);

/// A search of `buffer_id` for `query`, and the matches found so far.
pub(crate) struct ActiveSearchScan {
    pub buffer_id: BufferId,
    pub query: String,
    /// The selection searched within, if any.
    pub search_range: Option<Range<usize>>,
    pub matches: Vec<(usize, usize)>,
    /// Stopped at `SearchState::MAX_MATCHES` with more to find.
    pub capped: bool,
    regex: regex::bytes::Regex,
    overlap: usize,
    /// The buffer version the matches are for.
    version: u64,
    search: RegexSearch,
}

impl ActiveSearchScan {
    pub(crate) fn new(
        buffer_id: BufferId,
        query: String,
        regex: regex::bytes::Regex,
        overlap: usize,
        search_range: Option<Range<usize>>,
        buffer: &TextBuffer,
    ) -> Self {
        let search = Self::search(&regex, overlap, &search_range, buffer);
        Self {
            buffer_id,
            query,
            search_range,
            matches: Vec::new(),
            capped: false,
            regex,
            overlap,
            version: buffer.version(),
            search,
        }
    }

    fn search(
        regex: &regex::bytes::Regex,
        overlap: usize,
        search_range: &Option<Range<usize>>,
        buffer: &TextBuffer,
    ) -> RegexSearch {
        let range = search_range.clone().unwrap_or(0..buffer.len());
        RegexSearch::new(regex.clone(), range, overlap, buffer)
    }

    pub(crate) fn is_done(&self) -> bool {
        self.capped || self.search.is_done()
    }

    pub(crate) fn progress_percent(&self, buffer_len: usize) -> usize {
        let start = self.search_range.as_ref().map_or(0, |r| r.start);
        let end = self.search_range.as_ref().map_or(buffer_len, |r| r.end);
        ((self.search.position().saturating_sub(start)) * 100)
            .checked_div(end.saturating_sub(start))
            .unwrap_or(100)
            .min(100)
    }

    /// Search on for about `budget`, or to the end without one. An edit of
    /// the buffer since the last run starts the search over.
    pub(crate) fn run(
        &mut self,
        buffer: &mut TextBuffer,
        budget: Option<Duration>,
    ) -> anyhow::Result<()> {
        if buffer.version() != self.version {
            self.version = buffer.version();
            self.matches.clear();
            self.capped = false;
            self.search = Self::search(&self.regex, self.overlap, &self.search_range, buffer);
        }
        let deadline = budget.map(|b| Instant::now() + b);
        while !self.is_done() {
            let limit = SearchState::MAX_MATCHES + 1 - self.matches.len();
            let found = self.search.step(buffer, LOAD_CHUNK_SIZE, limit)?;
            self.matches
                .extend(found.into_iter().map(|m| (m.byte_offset, m.length)));
            if self.matches.len() > SearchState::MAX_MATCHES {
                self.matches.truncate(SearchState::MAX_MATCHES);
                self.capped = true;
            }
            if deadline.is_some_and(|d| Instant::now() >= d) {
                break;
            }
        }
        Ok(())
    }
}

/// Owner of the optional in-flight search scan.
#[derive(Default)]
pub(crate) struct SearchScan {
    active: Option<ActiveSearchScan>,
}

impl SearchScan {
    /// Begin a scan, replacing any in flight.
    pub(crate) fn start(&mut self, scan: ActiveSearchScan) {
        self.active = Some(scan);
    }

    /// Take the scan out to run it against its buffer; `start` puts it back.
    pub(crate) fn take(&mut self) -> Option<ActiveSearchScan> {
        self.active.take()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn buffer(text: &str) -> TextBuffer {
        TextBuffer::from_bytes(
            text.as_bytes().to_vec(),
            std::sync::Arc::new(crate::model::filesystem::NoopFileSystem),
        )
    }

    fn scan(query: &str, range: Option<Range<usize>>, buffer: &TextBuffer) -> ActiveSearchScan {
        let regex = regex::bytes::Regex::new(query).unwrap();
        ActiveSearchScan::new(BufferId(1), query.to_string(), regex, 8, range, buffer)
    }

    #[test]
    fn start_and_take() {
        let buf = buffer("abc");
        let mut s = SearchScan::default();
        assert!(s.take().is_none());
        s.start(scan("b", None, &buf));
        assert_eq!(s.take().map(|a| a.buffer_id), Some(BufferId(1)));
        assert!(s.take().is_none());
    }

    #[test]
    fn runs_to_the_end_without_a_budget() {
        let mut buf = buffer("one two one two one");
        let mut s = scan("one", None, &buf);
        s.run(&mut buf, None).unwrap();
        assert!(s.is_done());
        assert_eq!(s.matches, vec![(0, 3), (8, 3), (16, 3)]);
        assert!(!s.capped);
        assert_eq!(s.progress_percent(buf.len()), 100);
    }

    #[test]
    fn searches_only_the_selection() {
        let mut buf = buffer("one two one two one");
        let mut s = scan("one", Some(4..15), &buf);
        s.run(&mut buf, None).unwrap();
        assert_eq!(s.matches, vec![(8, 3)]);
    }

    /// An edit between runs starts the search over, so no match is for
    /// text that is gone.
    #[test]
    fn an_edit_restarts_the_search() {
        let mut buf = buffer("aaa");
        let mut s = scan("a", None, &buf);
        buf.insert(0, "b");
        s.run(&mut buf, None).unwrap();
        assert_eq!(s.matches, vec![(1, 1), (2, 1), (3, 1)]);
    }

    /// Reaching `MAX_MATCHES` with more to find stops the scan, capped —
    /// it must finish rather than run on.
    #[test]
    fn stops_capped_at_max_matches() {
        let mut buf = buffer(&"a".repeat(SearchState::MAX_MATCHES + 5));
        let mut s = scan("a", None, &buf);
        s.run(&mut buf, None).unwrap();
        assert!(s.is_done());
        assert!(s.capped);
        assert_eq!(s.matches.len(), SearchState::MAX_MATCHES);
    }
}
