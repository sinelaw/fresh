//! Self-contained line-feed scan state.
//!
//! A "line scan" counts newlines per leaf of a buffer's piece tree,
//! chunked so the UI can stay responsive on very large files. The scan
//! is driven one batch per frame by an orchestrator on `Editor`, which
//! reads the chunk list, does the actual counting (in-memory via
//! `scan_leaf` or concurrently via filesystem calls), and feeds the
//! results back into the scan state.
//!
//! `LineScan` below owns only the bookkeeping: what to scan, how much is
//! done, accumulated per-leaf results, and whether to reopen the Go to
//! Line prompt on completion. It does **not** know about buffers,
//! filesystems, or tokio — see the orchestrator methods in
//! `buffer_management.rs` for those.

use crate::model::buffer::LineScanChunk;
use crate::model::event::BufferId;
use crate::model::piece_tree::LeafData;

/// Everything the orchestrator needs to know once a scan has finished:
/// which buffer to rebuild, the accumulated per-leaf line-feed counts,
/// and whether to reopen the Go to Line prompt.
pub(crate) struct FinishedScan {
    pub buffer_id: BufferId,
    pub updates: Vec<(usize, usize)>,
    pub open_goto_line: bool,
}

struct Active {
    buffer_id: BufferId,
    leaves: Vec<LeafData>,
    chunks: Vec<LineScanChunk>,
    next_chunk: usize,
    total_bytes: usize,
    scanned_bytes: usize,
    updates: Vec<(usize, usize)>,
    open_goto_line_on_complete: bool,
}

/// Owner of the optional in-flight line-feed scan.
#[derive(Default)]
pub(crate) struct LineScan {
    active: Option<Active>,
}

impl LineScan {
    // ---- Queries -----------------------------------------------------------

    /// Buffer id being scanned, if any. `None` means no scan is in flight.
    pub(crate) fn buffer_id(&self) -> Option<BufferId> {
        self.active.as_ref().map(|a| a.buffer_id)
    }

    /// Whether every chunk has been consumed — the next call to the
    /// orchestrator should finalize the scan.
    pub(crate) fn is_done(&self) -> bool {
        match &self.active {
            Some(a) => a.next_chunk >= a.chunks.len(),
            None => true,
        }
    }

    /// Progress percent (0..=100), or 100 when there is nothing to scan.
    pub(crate) fn progress_percent(&self) -> usize {
        match &self.active {
            Some(a) => {
                if a.total_bytes == 0 {
                    100
                } else {
                    (a.scanned_bytes * 100) / a.total_bytes
                }
            }
            None => 100,
        }
    }

    /// Immutable view of the leaf list — used by the orchestrator to look
    /// up I/O parameters per chunk.
    pub(crate) fn leaves(&self) -> &[LeafData] {
        self.active.as_ref().map(|a| a.leaves.as_slice()).unwrap_or(&[])
    }

    // ---- Lifecycle ---------------------------------------------------------

    /// Begin a new scan. Discards any previous in-flight scan.
    pub(crate) fn start(
        &mut self,
        buffer_id: BufferId,
        leaves: Vec<LeafData>,
        chunks: Vec<LineScanChunk>,
        total_bytes: usize,
        open_goto_line_on_complete: bool,
    ) {
        self.active = Some(Active {
            buffer_id,
            leaves,
            chunks,
            next_chunk: 0,
            total_bytes,
            scanned_bytes: 0,
            updates: Vec::new(),
            open_goto_line_on_complete,
        });
    }

    /// Pop up to `max` un-processed chunks off the queue. Advances the
    /// internal cursor and accumulates `scanned_bytes` so that
    /// [`progress_percent`] stays in sync.
    ///
    /// Returns an empty vec if no scan is active or nothing is pending.
    pub(crate) fn take_next_chunks(&mut self, max: usize) -> Vec<LineScanChunk> {
        let Some(a) = self.active.as_mut() else {
            return Vec::new();
        };
        let mut out = Vec::new();
        while out.len() < max && a.next_chunk < a.chunks.len() {
            let chunk = a.chunks[a.next_chunk].clone();
            a.next_chunk += 1;
            a.scanned_bytes += chunk.byte_len;
            out.push(chunk);
        }
        out
    }

    /// Append a per-leaf line-feed count produced by the orchestrator.
    pub(crate) fn append_update(&mut self, leaf_index: usize, lf_count: usize) {
        if let Some(a) = self.active.as_mut() {
            a.updates.push((leaf_index, lf_count));
        }
    }

    /// Consume the scan, returning the accumulated data the orchestrator
    /// needs to rebuild the buffer's line metadata. Leaves `self` inactive.
    pub(crate) fn take_finished(&mut self) -> Option<FinishedScan> {
        let a = self.active.take()?;
        Some(FinishedScan {
            buffer_id: a.buffer_id,
            updates: a.updates,
            open_goto_line: a.open_goto_line_on_complete,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn chunk(leaf_index: usize, byte_len: usize, already_known: bool) -> LineScanChunk {
        LineScanChunk {
            leaf_index,
            byte_len,
            already_known,
        }
    }

    fn empty_scan_of(chunks: Vec<LineScanChunk>, total_bytes: usize) -> LineScan {
        let mut s = LineScan::default();
        s.start(BufferId(1), Vec::new(), chunks, total_bytes, false);
        s
    }

    #[test]
    fn default_is_inactive_and_reports_done() {
        let s = LineScan::default();
        assert_eq!(s.buffer_id(), None);
        assert!(s.is_done()); // nothing to scan == vacuously done
        assert_eq!(s.progress_percent(), 100);
    }

    #[test]
    fn start_then_take_walks_chunks_in_order() {
        let mut s = empty_scan_of(
            vec![chunk(0, 100, false), chunk(1, 200, false), chunk(2, 50, true)],
            350,
        );
        assert!(s.buffer_id().is_some());
        assert!(!s.is_done());

        let first_two = s.take_next_chunks(2);
        assert_eq!(first_two.len(), 2);
        assert_eq!(first_two[0].leaf_index, 0);
        assert_eq!(first_two[1].leaf_index, 1);
        assert!(!s.is_done());

        let last = s.take_next_chunks(10);
        assert_eq!(last.len(), 1);
        assert_eq!(last[0].leaf_index, 2);
        assert!(s.is_done());

        // Subsequent calls drain nothing more.
        assert!(s.take_next_chunks(5).is_empty());
    }

    #[test]
    fn take_next_chunks_accumulates_scanned_bytes_for_progress() {
        let mut s = empty_scan_of(vec![chunk(0, 30, false), chunk(1, 70, false)], 100);
        assert_eq!(s.progress_percent(), 0);
        s.take_next_chunks(1);
        assert_eq!(s.progress_percent(), 30);
        s.take_next_chunks(1);
        assert_eq!(s.progress_percent(), 100);
    }

    #[test]
    fn progress_percent_is_100_for_zero_byte_scan() {
        let s = empty_scan_of(Vec::new(), 0);
        assert_eq!(s.progress_percent(), 100);
    }

    #[test]
    fn append_update_is_noop_when_inactive() {
        let mut s = LineScan::default();
        s.append_update(0, 42); // doesn't panic
        assert!(s.take_finished().is_none());
    }

    #[test]
    fn take_finished_drains_state_and_returns_updates() {
        let mut s = empty_scan_of(vec![chunk(0, 10, false)], 10);
        s.take_next_chunks(1);
        s.append_update(0, 7);
        let finished = s.take_finished().expect("has finished data");
        assert_eq!(finished.buffer_id, BufferId(1));
        assert_eq!(finished.updates, vec![(0, 7)]);
        assert!(!finished.open_goto_line);
        // Scan is now inactive.
        assert_eq!(s.buffer_id(), None);
    }

    #[test]
    fn open_goto_line_flag_round_trips() {
        let mut s = LineScan::default();
        s.start(BufferId(2), Vec::new(), Vec::new(), 0, true);
        let finished = s.take_finished().unwrap();
        assert!(finished.open_goto_line);
    }
}
