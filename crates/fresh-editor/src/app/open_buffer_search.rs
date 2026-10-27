//! Project searches of open buffers.
//!
//! A project search reads the files that aren't open from disk, off the
//! editor thread. An open buffer with unsaved edits has to be searched as
//! it is, so it is searched here instead, through its live `TextBuffer` — a
//! [`BufferSearch`] run a slice per editor tick, like a Ctrl+F search of a
//! large file — and the matches go back to the search's task over a channel,
//! or to whatever else asked (a plugin's `searchBuffer`).

use std::time::Instant;

use fresh_core::api::GrepMatch;
use fresh_core::{BufferId, WindowId};
use tokio::sync::oneshot;

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
    }
}

/// A search of one open buffer for a project search.
pub(crate) struct OpenBufferSearch {
    window: WindowId,
    buffer_id: BufferId,
    /// The file name the matches are reported under.
    file: String,
    search: BufferSearch,
    cancelled: Box<dyn Fn() -> bool + Send>,
    reply: Box<dyn FnOnce(Vec<GrepMatch>) + Send>,
}

impl OpenBufferSearch {
    /// A search of `buffer_id` in `window` for up to `limit` matches; they
    /// arrive on the returned receiver once it is done, or empty if it is
    /// `cancelled` first.
    #[allow(clippy::too_many_arguments)]
    #[cfg_attr(not(feature = "plugins"), allow(dead_code))]
    pub(crate) fn new(
        window: WindowId,
        buffer_id: BufferId,
        file: String,
        query: SearchQuery,
        limit: usize,
        buffer: &TextBuffer,
        cancelled: Box<dyn Fn() -> bool + Send>,
    ) -> (Self, oneshot::Receiver<Vec<GrepMatch>>) {
        let (reply, receiver) = oneshot::channel();
        // The search's task may have stopped waiting; nothing to do then.
        let reply = Box::new(move |found| drop(reply.send(found)));
        let job = Self::reporting_to(
            window, buffer_id, file, query, limit, buffer, cancelled, reply,
        );
        (job, receiver)
    }

    /// As [`new`](Self::new), with the matches handed to `reply` instead.
    #[allow(clippy::too_many_arguments)]
    #[cfg_attr(not(feature = "plugins"), allow(dead_code))]
    pub(crate) fn reporting_to(
        window: WindowId,
        buffer_id: BufferId,
        file: String,
        query: SearchQuery,
        limit: usize,
        buffer: &TextBuffer,
        cancelled: Box<dyn Fn() -> bool + Send>,
        reply: Box<dyn FnOnce(Vec<GrepMatch>) + Send>,
    ) -> Self {
        Self {
            window,
            buffer_id,
            file,
            search: BufferSearch::new(query, None, limit, buffer).with_lines(buffer),
            cancelled,
            reply,
        }
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
        let found = if (self.cancelled)() {
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
                _ if (job.cancelled)() => true,
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
        !self.open_buffer_searches.is_empty()
    }
}
