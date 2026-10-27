//! Project searches of open buffers.
//!
//! A project search reads the files that aren't open from disk, off the
//! editor thread. An open buffer with unsaved edits has to be searched as
//! it is, so it is searched here instead, through its live `TextBuffer` — a
//! slice per editor tick, like a Ctrl+F search of a large file — and the
//! matches go back to the search's task over a channel.

use std::time::Instant;

use fresh_core::api::GrepMatch;
use fresh_core::{BufferId, WindowId};
use tokio::sync::oneshot;

use super::search_scan::SEARCH_SCAN_SLICE;
use super::Editor;
use crate::model::buffer::{RegexSearch, TextBuffer, LOAD_CHUNK_SIZE};

/// A search of one open buffer for a project search.
pub(crate) struct OpenBufferSearch {
    window: WindowId,
    buffer_id: BufferId,
    /// The file name the matches are reported under.
    file: String,
    regex: regex::bytes::Regex,
    overlap: usize,
    limit: usize,
    /// The buffer version the matches are for.
    version: u64,
    search: RegexSearch,
    found: Vec<GrepMatch>,
    cancelled: Box<dyn Fn() -> bool + Send>,
    reply: oneshot::Sender<Vec<GrepMatch>>,
}

impl OpenBufferSearch {
    /// A search of `buffer_id` in `window` for up to `limit` matches; they
    /// arrive on the returned receiver once it is done, or empty if it is
    /// `cancelled` first.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        window: WindowId,
        buffer_id: BufferId,
        file: String,
        regex: regex::bytes::Regex,
        overlap: usize,
        limit: usize,
        buffer: &TextBuffer,
        cancelled: Box<dyn Fn() -> bool + Send>,
    ) -> (Self, oneshot::Receiver<Vec<GrepMatch>>) {
        let (reply, receiver) = oneshot::channel();
        let search = Self::search(&regex, overlap, buffer);
        let job = Self {
            window,
            buffer_id,
            file,
            regex,
            overlap,
            limit,
            version: buffer.version(),
            search,
            found: Vec::new(),
            cancelled,
            reply,
        };
        (job, receiver)
    }

    fn search(regex: &regex::bytes::Regex, overlap: usize, buffer: &TextBuffer) -> RegexSearch {
        RegexSearch::new(regex.clone(), 0..buffer.len(), overlap, buffer).with_lines(1)
    }

    /// Search on until `deadline`; true once done. An edit of the buffer
    /// since the last run starts the search over.
    fn run(&mut self, buffer: &mut TextBuffer, deadline: Instant) -> bool {
        if (self.cancelled)() {
            self.found.clear();
            return true;
        }
        if buffer.version() != self.version {
            self.version = buffer.version();
            self.found.clear();
            self.search = Self::search(&self.regex, self.overlap, buffer);
        }
        while !self.search.is_done() && self.found.len() < self.limit {
            let found =
                match self
                    .search
                    .step(buffer, LOAD_CHUNK_SIZE, self.limit - self.found.len())
                {
                    Ok(found) => found,
                    Err(e) => {
                        tracing::warn!("Project search of {} failed: {}", self.file, e);
                        return true;
                    }
                };
            self.found.extend(found.into_iter().map(|m| GrepMatch {
                file: self.file.clone(),
                buffer_id: self.buffer_id.0,
                byte_offset: m.byte_offset,
                length: m.length,
                line: m.line,
                column: m.column,
                context: m.context,
            }));
            if Instant::now() >= deadline {
                break;
            }
        }
        self.search.is_done() || self.found.len() >= self.limit
    }

    fn finish(self) {
        // The search's task may have stopped waiting; nothing to do then.
        self.reply.send(self.found).ok();
    }
}

impl Editor {
    /// Queue a search of an open buffer for a project search.
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
