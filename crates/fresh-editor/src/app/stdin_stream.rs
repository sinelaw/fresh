//! Self-contained stdin-streaming bookkeeping.
//!
//! When the editor is launched with piped stdin (e.g. `cat big.log | fresh`),
//! a background thread spools the input to a temp file and the buffer is
//! extended incrementally as the file grows. `StdinStream` owns the small
//! amount of per-session state needed to track that stream; all of the
//! heavy lifting (reading file size, extending the buffer, setting status
//! messages) lives on the orchestrator on `Editor`, which calls the
//! narrow API defined below.
//!
//! Keeping the mutable state here means unit tests can drive a whole
//! grow/complete lifecycle without constructing an `Editor` or a real
//! background thread — see the tests module.

use std::path::{Path, PathBuf};
use std::thread::JoinHandle;

use crate::model::event::BufferId;

/// Outcome reported when the background streaming thread is observed to
/// have finished between two polls.
#[derive(Debug)]
pub(crate) enum ThreadOutcome {
    /// Thread exited cleanly.
    Success,
    /// Thread returned an error.
    Error(String),
    /// Thread panicked.
    Panic,
}

/// All state for one active stdin-streaming session.
struct ActiveStream {
    temp_path: PathBuf,
    buffer_id: BufferId,
    last_known_size: usize,
    complete: bool,
    thread_handle: Option<JoinHandle<anyhow::Result<()>>>,
}

/// Owner of the optional in-flight stdin stream.
///
/// `Default::default()` produces an inactive instance; call [`start`] once
/// the caller has set up the temp file + background thread.
#[derive(Default)]
pub(crate) struct StdinStream {
    active: Option<ActiveStream>,
}

impl StdinStream {
    // ---- Queries -----------------------------------------------------------

    /// Whether a stream is currently in flight and not yet marked complete.
    pub(crate) fn is_active(&self) -> bool {
        self.active.as_ref().is_some_and(|s| !s.complete)
    }

    /// Buffer id receiving the streamed data, if any.
    pub(crate) fn buffer_id(&self) -> Option<BufferId> {
        self.active.as_ref().map(|s| s.buffer_id)
    }

    /// Path to the temp file being tailed.
    pub(crate) fn temp_path(&self) -> Option<&Path> {
        self.active.as_ref().map(|s| s.temp_path.as_path())
    }

    /// Last observed size in bytes. Returns 0 when no stream is active.
    pub(crate) fn last_known_size(&self) -> usize {
        self.active
            .as_ref()
            .map(|s| s.last_known_size)
            .unwrap_or(0)
    }

    // ---- Lifecycle ---------------------------------------------------------

    /// Begin tracking a new stream. Overwrites any previous one.
    ///
    /// `thread_handle: None` means the stream is already complete (useful
    /// for tests and for the `echo "x" | fresh` case where all data is
    /// present before the editor starts polling).
    pub(crate) fn start(
        &mut self,
        temp_path: PathBuf,
        buffer_id: BufferId,
        initial_size: usize,
        thread_handle: Option<JoinHandle<anyhow::Result<()>>>,
    ) {
        let complete = thread_handle.is_none();
        self.active = Some(ActiveStream {
            temp_path,
            buffer_id,
            last_known_size: initial_size,
            complete,
            thread_handle,
        });
    }

    /// Record a new on-disk size. Returns `true` if this represents growth
    /// (the caller should extend the buffer by the delta).
    ///
    /// Does nothing and returns `false` if no stream is active.
    pub(crate) fn record_growth(&mut self, new_size: usize) -> bool {
        let Some(active) = self.active.as_mut() else {
            return false;
        };
        if new_size > active.last_known_size {
            active.last_known_size = new_size;
            true
        } else {
            false
        }
    }

    /// If the background thread is observed to have finished, take its
    /// handle, join it, and return the outcome.
    ///
    /// Returns `None` when: no stream is active; no thread handle was
    /// attached (already-complete case); or the thread is still running.
    pub(crate) fn take_finished_thread_outcome(&mut self) -> Option<ThreadOutcome> {
        let active = self.active.as_mut()?;
        let handle_ref = active.thread_handle.as_ref()?;
        if !handle_ref.is_finished() {
            return None;
        }
        let handle = active.thread_handle.take()?;
        Some(match handle.join() {
            Ok(Ok(())) => ThreadOutcome::Success,
            Ok(Err(e)) => ThreadOutcome::Error(e.to_string()),
            Err(_) => ThreadOutcome::Panic,
        })
    }

    /// Mark the stream as complete. Subsequent `is_active` queries return
    /// `false`; the bookkeeping itself is retained so the buffer_id and
    /// last-known-size remain observable.
    pub(crate) fn mark_complete(&mut self) {
        if let Some(active) = self.active.as_mut() {
            active.complete = true;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn start_without_thread(s: &mut StdinStream, size: usize) {
        s.start(PathBuf::from("/tmp/stream.txt"), BufferId(7), size, None);
    }

    #[test]
    fn default_is_inactive() {
        let s = StdinStream::default();
        assert!(!s.is_active());
        assert_eq!(s.buffer_id(), None);
        assert_eq!(s.last_known_size(), 0);
    }

    #[test]
    fn start_without_thread_begins_already_complete() {
        // Simulates the "all of stdin arrived before editor started" case.
        let mut s = StdinStream::default();
        start_without_thread(&mut s, 1024);
        // Complete, so not "active" — but buffer_id + size are still visible
        // so the polling orchestrator on Editor can drain the final state.
        assert!(!s.is_active());
        assert_eq!(s.buffer_id(), Some(BufferId(7)));
        assert_eq!(s.last_known_size(), 1024);
    }

    #[test]
    fn record_growth_returns_true_on_increase() {
        let mut s = StdinStream::default();
        start_without_thread(&mut s, 100);
        assert!(s.record_growth(200));
        assert_eq!(s.last_known_size(), 200);
    }

    #[test]
    fn record_growth_returns_false_on_same_or_smaller_size() {
        let mut s = StdinStream::default();
        start_without_thread(&mut s, 100);
        assert!(!s.record_growth(100));
        assert!(!s.record_growth(50));
        assert_eq!(s.last_known_size(), 100);
    }

    #[test]
    fn record_growth_is_noop_when_inactive() {
        let mut s = StdinStream::default();
        assert!(!s.record_growth(100));
        assert_eq!(s.last_known_size(), 0);
    }

    #[test]
    fn mark_complete_disables_is_active() {
        let mut s = StdinStream::default();
        // Simulate a stream with a handle that never completes.
        s.active = Some(ActiveStream {
            temp_path: PathBuf::from("/tmp/x"),
            buffer_id: BufferId(1),
            last_known_size: 0,
            complete: false,
            thread_handle: None,
        });
        assert!(s.is_active());
        s.mark_complete();
        assert!(!s.is_active());
    }

    #[test]
    fn take_finished_thread_outcome_none_when_no_handle() {
        let mut s = StdinStream::default();
        start_without_thread(&mut s, 0);
        assert!(s.take_finished_thread_outcome().is_none());
    }

    #[test]
    fn temp_path_reflects_start_argument() {
        let mut s = StdinStream::default();
        s.start(PathBuf::from("/tmp/foo"), BufferId(0), 0, None);
        assert_eq!(s.temp_path(), Some(Path::new("/tmp/foo")));
    }
}
