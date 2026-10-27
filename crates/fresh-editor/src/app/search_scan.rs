//! The in-flight Ctrl+F search of a buffer.
//!
//! A search of a large file runs a slice per editor tick (see
//! `Editor::process_search_scan`) so it never stalls the editor; a small
//! one runs to the end in the call that starts it. Either way it is a
//! [`BufferSearch`], which starts over if the buffer is edited meanwhile.

use std::ops::Range;
use std::time::{Duration, Instant};

use crate::app::types::SearchState;
use crate::model::buffer::{BufferSearch, SearchQuery, TextBuffer};
use crate::model::event::BufferId;

/// How long a search of a large file runs in each editor tick.
pub(crate) const SEARCH_SCAN_SLICE: Duration = Duration::from_millis(8);

/// A search of `buffer_id` for `query`.
pub(crate) struct ActiveSearchScan {
    pub buffer_id: BufferId,
    pub query: String,
    pub search_query: SearchQuery,
    /// The selection searched within, if any.
    pub search_range: Option<Range<usize>>,
    /// One more match than is kept, to tell a capped search from one that
    /// found exactly `SearchState::MAX_MATCHES`.
    search: BufferSearch,
}

impl ActiveSearchScan {
    pub(crate) fn new(
        buffer_id: BufferId,
        query: String,
        search_query: SearchQuery,
        search_range: Option<Range<usize>>,
        buffer: &TextBuffer,
    ) -> Self {
        let search = BufferSearch::new(
            search_query.clone(),
            search_range.clone(),
            SearchState::MAX_MATCHES + 1,
            buffer,
        );
        Self {
            buffer_id,
            query,
            search_query,
            search_range,
            search,
        }
    }

    pub(crate) fn is_done(&self) -> bool {
        self.search.is_done()
    }

    pub(crate) fn match_count(&self) -> usize {
        self.search.found().len().min(SearchState::MAX_MATCHES)
    }

    pub(crate) fn progress_percent(&self, buffer_len: usize) -> usize {
        self.search.progress_percent(buffer_len)
    }

    /// Search on for about `budget`, or to the end without one.
    pub(crate) fn run(
        &mut self,
        buffer: &mut TextBuffer,
        budget: Option<Duration>,
    ) -> anyhow::Result<()> {
        self.search.run(buffer, budget.map(|b| Instant::now() + b))
    }

    /// The matches as `(start, len)`, and whether the search stopped at
    /// `SearchState::MAX_MATCHES` with more to find.
    pub(crate) fn into_matches(self) -> (Vec<(usize, usize)>, bool) {
        let mut found = self.search.into_found();
        let capped = found.len() > SearchState::MAX_MATCHES;
        found.truncate(SearchState::MAX_MATCHES);
        let matches = found.into_iter().map(|m| (m.byte_offset, m.length));
        (matches.collect(), capped)
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
        let search_query = SearchQuery::new(query, false, false, true).unwrap();
        ActiveSearchScan::new(BufferId(1), query.to_string(), search_query, range, buffer)
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
        assert_eq!(s.progress_percent(buf.len()), 100);
        assert_eq!(s.into_matches(), (vec![(0, 3), (8, 3), (16, 3)], false));
    }

    #[test]
    fn searches_only_the_selection() {
        let mut buf = buffer("one two one two one");
        let mut s = scan("one", Some(4..15), &buf);
        s.run(&mut buf, None).unwrap();
        assert_eq!(s.into_matches().0, vec![(8, 3)]);
    }

    /// An edit between runs starts the search over, so no match is for
    /// text that is gone.
    #[test]
    fn an_edit_restarts_the_search() {
        let mut buf = buffer("aaa");
        let mut s = scan("a", None, &buf);
        buf.insert(0, "b");
        s.run(&mut buf, None).unwrap();
        assert_eq!(s.into_matches().0, vec![(1, 1), (2, 1), (3, 1)]);
    }

    /// Reaching `MAX_MATCHES` with more to find stops the scan, capped —
    /// it must finish rather than run on; exactly `MAX_MATCHES` is not
    /// capped.
    #[test]
    fn stops_capped_at_max_matches() {
        let max = SearchState::MAX_MATCHES;
        let mut buf = buffer(&"a".repeat(max + 5));
        let mut s = scan("a", None, &buf);
        s.run(&mut buf, None).unwrap();
        assert!(s.is_done());
        let (matches, capped) = s.into_matches();
        assert_eq!(matches.len(), max);
        assert!(capped);

        let mut buf = buffer(&"a".repeat(max));
        let mut s = scan("a", None, &buf);
        s.run(&mut buf, None).unwrap();
        assert!(!s.into_matches().1);
    }
}
