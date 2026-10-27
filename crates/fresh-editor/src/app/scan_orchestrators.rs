//! Line-scan and search-scan orchestrators on `Editor`.
//!
//! Drive the chunked-scan subsystems extracted in phase 2 (LineScan,
//! SearchScan) one batch per render frame. Coordinate with
//! self.buffers (to read leaves and apply scan results), the tokio
//! runtime (for concurrent filesystem I/O), and the status message
//! (for progress reporting).

use fresh_i18n::t;

use crate::model::event::BufferId;
use crate::view::prompt::PromptType;

use super::Editor;

impl Editor {
    /// Start an incremental line-feed scan for the active buffer.
    ///
    /// Shared by the `Action::ScanLineIndex` command and the Go to Line scan
    /// confirmation prompt. Seeds `LineScan` so that `process_line_scan`
    /// will advance the scan one batch per frame.
    ///
    /// When `open_goto_line` is true (Go to Line flow), the Go to Line prompt
    /// opens automatically when the scan completes.
    pub fn start_incremental_line_scan(&mut self, open_goto_line: bool) {
        let buffer_id = self.active_buffer();
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
            let (chunks, total_bytes) = state.buffer.prepare_line_scan();
            let leaves = state.buffer.piece_tree_leaves();
            self.active_window_mut().line_scan.start(
                buffer_id,
                leaves,
                chunks,
                total_bytes,
                open_goto_line,
            );
            self.set_status_message(t!("goto.scanning_progress", percent = 0).to_string());
        }
    }

    /// Process chunks for the incremental line-feed scan.
    /// Returns `true` if the UI should re-render (progress updated or scan finished).
    pub fn process_line_scan(&mut self) -> bool {
        let _span = tracing::info_span!("process_line_scan").entered();

        let Some(buffer_id) = self.active_window_mut().line_scan.buffer_id() else {
            return false;
        };

        if let Err(e) = self.process_line_scan_batch(buffer_id) {
            tracing::warn!("Line scan error: {e}");
            self.finish_line_scan_with_error(e);
            return true;
        }

        if self.active_window_mut().line_scan.is_done() {
            self.finish_line_scan_ok();
        } else {
            let pct = self.active_window_mut().line_scan.progress_percent();
            self.set_status_message(t!("goto.scanning_progress", percent = pct).to_string());
        }
        true
    }

    /// Process leaves concurrently, yielding for a render after each batch.
    ///
    /// For loaded leaves, delegates to `TextBuffer::scan_leaf` (shared counting
    /// logic). For unloaded leaves, extracts I/O parameters and runs them
    /// concurrently using `tokio::task::spawn_blocking` — each task calls
    /// `count_line_feeds_in_range` on the filesystem, which remote implementations
    /// override to count on the server without transferring data.
    fn process_line_scan_batch(&mut self, buffer_id: BufferId) -> std::io::Result<()> {
        let _span = tracing::info_span!("process_line_scan_batch").entered();
        let concurrency = self.config.editor.read_concurrency.max(1);

        let mut results: Vec<(usize, usize)> = Vec::new();
        let mut io_work: Vec<(usize, std::path::PathBuf, u64, usize)> = Vec::new();
        let active_id = self.active_window;
        let has_state = self
            .windows
            .get(&active_id)
            .expect("active window present")
            .buffers
            .contains_key(&buffer_id);
        if !has_state {
            return Ok(());
        }

        // Pull chunks up to the concurrency budget, skipping already-known
        // leaves. The budget is in terms of actual work items, so we keep
        // asking for more chunks until we fill it or run out.
        'outer: while results.len() + io_work.len() < concurrency {
            let batch = self
                .windows
                .get_mut(&active_id)
                .expect("active window present")
                .line_scan
                .take_next_chunks(concurrency - (results.len() + io_work.len()));
            if batch.is_empty() {
                break;
            }

            for chunk in batch {
                if chunk.already_known {
                    continue;
                }

                let leaf = self
                    .windows
                    .get(&active_id)
                    .expect("active window present")
                    .line_scan
                    .leaves()[chunk.leaf_index];
                let leaf = &leaf;

                let win = self.windows.get(&active_id).expect("active window present");
                let Some(state) = win.buffers.get(&buffer_id) else {
                    break 'outer;
                };
                match state.buffer.leaf_io_params(leaf) {
                    None => {
                        // Loaded: count in-memory via scan_leaf
                        let count = state.buffer.scan_leaf(leaf)?;
                        results.push((chunk.leaf_index, count));
                    }
                    Some((path, offset, len)) => {
                        // Unloaded: batch for concurrent I/O
                        io_work.push((chunk.leaf_index, path, offset, len));
                    }
                }
            }
        }

        // Run I/O concurrently using tokio::task::spawn_blocking
        if !io_work.is_empty() {
            let fs = match self
                .windows
                .get(&active_id)
                .expect("active window present")
                .buffers
                .get(&buffer_id)
            {
                Some(s) => s.buffer.filesystem().clone(),
                None => return Ok(()),
            };

            let rt = self
                .tokio_runtime
                .as_ref()
                .ok_or_else(|| std::io::Error::other("async runtime not available"))?;

            let io_results: Vec<std::io::Result<(usize, usize)>> = rt.block_on(async {
                let mut handles = Vec::with_capacity(io_work.len());
                for (leaf_idx, path, offset, len) in io_work {
                    let fs = fs.clone();
                    handles.push(tokio::task::spawn_blocking(move || {
                        let count = fs.count_line_feeds_in_range(&path, offset, len)?;
                        Ok((leaf_idx, count))
                    }));
                }

                let mut results = Vec::with_capacity(handles.len());
                for handle in handles {
                    results.push(handle.await.unwrap());
                }
                results
            });

            for result in io_results {
                results.push(result?);
            }
        }

        for (leaf_idx, count) in results {
            self.active_window_mut()
                .line_scan
                .append_update(leaf_idx, count);
        }

        Ok(())
    }

    fn finish_line_scan_ok(&mut self) {
        let _span = tracing::info_span!("finish_line_scan_ok").entered();
        let Some(finished) = self.active_window_mut().line_scan.take_finished() else {
            return;
        };
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&finished.buffer_id)
        {
            let _span = tracing::info_span!(
                "rebuild_with_pristine_saved_root",
                updates = finished.updates.len()
            )
            .entered();
            state
                .buffer
                .rebuild_with_pristine_saved_root(&finished.updates);
        }
        self.set_status_message(t!("goto.scan_complete").to_string());
        if finished.open_goto_line {
            self.open_goto_line_if_active(finished.buffer_id);
        }
    }

    fn finish_line_scan_with_error(&mut self, e: std::io::Error) {
        let Some(finished) = self.active_window_mut().line_scan.take_finished() else {
            return;
        };
        self.set_status_message(t!("goto.scan_failed", error = e.to_string()).to_string());
        if finished.open_goto_line {
            self.open_goto_line_if_active(finished.buffer_id);
        }
    }

    fn open_goto_line_if_active(&mut self, buffer_id: BufferId) {
        if self.active_buffer() == buffer_id {
            self.start_prompt(
                t!("file.goto_line_prompt").to_string(),
                PromptType::GotoLine,
            );
        }
    }

    // === Incremental Search Scan ===

    /// Run the in-flight search scan for this tick's slice.
    /// Returns `true` if the UI should re-render (progress updated or scan finished).
    pub fn process_search_scan(&mut self) -> bool {
        let Some(scan) = self.active_window_mut().search_scan.take() else {
            return false;
        };
        self.advance_search_scan(scan, Some(super::search_scan::SEARCH_SCAN_SLICE));
        true
    }

    /// Run `scan` for `budget` (or to the end without one), then finish it
    /// or put it back to go on next tick.
    pub(super) fn advance_search_scan(
        &mut self,
        mut scan: super::search_scan::ActiveSearchScan,
        budget: Option<std::time::Duration>,
    ) {
        let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&scan.buffer_id)
        else {
            return; // The buffer is gone.
        };
        if let Err(e) = scan.run(&mut state.buffer, budget) {
            tracing::warn!("Search scan error: {e}");
            self.set_status_message(format!("Search failed: {e}"));
            return;
        }
        if scan.is_done() {
            self.finish_search_scan(scan);
        } else {
            let pct = scan.progress_percent(state.buffer.len());
            self.set_status_message(format!(
                "Searching... {}% ({} matches so far)",
                pct,
                scan.match_count()
            ));
            self.active_window_mut().search_scan.start(scan);
        }
    }

    /// Hand a finished scan's matches to `finalize_search()`, which sets
    /// search_state, moves the cursor, and creates overlays.
    fn finish_search_scan(&mut self, scan: super::search_scan::ActiveSearchScan) {
        // The matches are for that buffer: if another is active now, they
        // would be shown and selected in the wrong one.
        if self.active_buffer() != scan.buffer_id {
            return;
        }

        // Reading a lazily loaded file loads chunks via
        // chunk_split_and_load, which restructures the piece tree.  Refresh
        // saved_root so that diff_since_saved() can take the fast
        // Arc::ptr_eq path.
        self.active_state_mut()
            .buffer
            .refresh_saved_root_if_unmodified();

        let (query, search_query, search_range) = (
            scan.query.clone(),
            scan.search_query.clone(),
            scan.search_range.clone(),
        );
        let (matches, capped) = scan.into_matches();
        if matches.is_empty() {
            self.active_window_mut().search_state = None;
            let msg = if search_range.is_some() {
                format!("No matches found for '{}' in selection", query)
            } else {
                format!("No matches found for '{}'", query)
            };
            self.set_status_message(msg);
            return;
        }

        self.finalize_search(&query, search_query, matches, capped, search_range);
    }
}
