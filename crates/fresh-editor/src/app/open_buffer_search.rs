//! Project searches of open buffers.
//!
//! A project search reads the files that aren't open from disk, off the
//! editor thread. An open buffer with unsaved edits has to be searched as
//! it is, so it is searched here instead, through its live `TextBuffer` — a
//! [`BufferSearch`] run a slice per editor tick, like a Ctrl+F search of a
//! large file — and the matches go back to the search's task over a channel,
//! or to whatever else asked (a plugin's `searchBuffer`).

use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Instant;

use fresh_core::api::{GrepMatch, SearchHandleState};
use fresh_core::{BufferId, WindowId};

use super::search_scan::SEARCH_SCAN_SLICE;
use super::Editor;
use crate::model::buffer::{BufferSearch, SearchQuery, TextBuffer};

/// A match as a project search reports it: in `file`, attributed to
/// `buffer_id` (0 when the file isn't open).
pub(crate) fn grep_match(
    file: &str,
    buffer_id: usize,
    m: crate::model::buffer::SearchMatch,
) -> GrepMatch {
    GrepMatch {
        file: file.to_string(),
        buffer_id,
        byte_offset: m.byte_offset,
        length: m.length,
        line: m.line,
        column: m.column,
        context: m.context,
        context_offset: m.context_offset,
    }
}

/// A search of one open buffer, for a project search or a plugin's
/// `searchBuffer`.
pub(crate) struct OpenBufferSearch {
    window: WindowId,
    buffer_id: BufferId,
    /// The file name the matches are reported under.
    file: String,
    search: BufferSearch,
    /// The search this is part of; its cancel flag stops this one.
    handle: Arc<SearchHandleState>,
    reply: Box<dyn FnOnce(Vec<GrepMatch>) + Send>,
}

impl OpenBufferSearch {
    /// A search of `buffer_id` in `window` for up to `limit` matches, handed
    /// to `reply` once it is done, or none if `handle` is cancelled first.
    #[allow(clippy::too_many_arguments)]
    #[cfg_attr(not(feature = "plugins"), allow(dead_code))]
    pub(crate) fn new(
        window: WindowId,
        buffer_id: BufferId,
        file: String,
        query: SearchQuery,
        limit: usize,
        buffer: &TextBuffer,
        handle: Arc<SearchHandleState>,
        reply: Box<dyn FnOnce(Vec<GrepMatch>) + Send>,
    ) -> Self {
        Self {
            window,
            buffer_id,
            file,
            search: BufferSearch::new(query, None, true, limit, buffer),
            handle,
            reply,
        }
    }

    fn cancelled(&self) -> bool {
        self.handle.cancel.load(Ordering::Relaxed)
    }

    /// Search on until `deadline`; true once there is nothing more to do.
    fn run(&mut self, buffer: &mut TextBuffer, deadline: Instant) -> bool {
        if let Err(e) = self.search.run(buffer, Some(deadline)) {
            tracing::warn!("Project search of {} failed: {}", self.file, e);
            return true;
        }
        self.search.is_done()
    }

    /// Send what was found, or nothing if the search was cancelled.
    fn finish(self) {
        let found = if self.cancelled() {
            Vec::new()
        } else {
            let (file, id) = (self.file, self.buffer_id.0);
            let found = self.search.into_found().into_iter();
            found.map(|m| grep_match(&file, id, m)).collect()
        };
        (self.reply)(found);
    }
}

impl Editor {
    /// Queue a search of an open buffer for a project search.
    #[cfg_attr(not(feature = "plugins"), allow(dead_code))]
    pub(crate) fn start_open_buffer_search(&mut self, job: OpenBufferSearch) {
        self.open_buffer_searches.push(job);
    }

    /// Run the open-buffer project searches for this tick's slice.
    /// Returns `true` while any is left, so the editor keeps ticking.
    pub(crate) fn process_open_buffer_searches(&mut self) -> bool {
        if self.open_buffer_searches.is_empty() {
            return false;
        }
        let deadline = Instant::now() + SEARCH_SCAN_SLICE;
        for mut job in std::mem::take(&mut self.open_buffer_searches) {
            let buffer = self
                .windows
                .get_mut(&job.window)
                .and_then(|w| w.buffers.get_mut(&job.buffer_id));
            let done = match buffer {
                _ if job.cancelled() => true,
                // The buffer was closed: report what was found.
                None => true,
                Some(_) if Instant::now() >= deadline => false,
                Some(state) => job.run(&mut state.buffer, deadline),
            };
            if done {
                job.finish();
            } else {
                self.open_buffer_searches.push(job);
            }
        }
        // The next tick starts with the next job, so one large buffer can't
        // keep the rest from their turn.
        if !self.open_buffer_searches.is_empty() {
            self.open_buffer_searches.rotate_left(1);
        }
        !self.open_buffer_searches.is_empty()
    }
}
