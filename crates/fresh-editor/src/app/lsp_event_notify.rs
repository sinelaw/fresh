//! LSP change-notification orchestrators on `Editor`.
//!
//! When buffers mutate, the LSP server needs to be notified so its
//! analysis stays in sync. These methods translate Editor `Event`s into
//! `TextDocumentContentChangeEvent`s, compute line-shift metadata for
//! plugin hooks, and send `did_save` notifications.

use lsp_types::{Position, Range as LspRange, TextDocumentContentChangeEvent};

use crate::model::event::{BufferId, Event};

use super::Editor;

impl Editor {
    // === LSP Diagnostics Display ===
    // NOTE: Diagnostics are now applied automatically via process_async_messages()
    // when received from the LSP server asynchronously. No manual polling needed!

    /// Collect all LSP text document changes from an event (recursively for batches)
    pub(super) fn collect_lsp_changes(&self, event: &Event) -> Vec<TextDocumentContentChangeEvent> {
        match event {
            Event::Insert { position, text, .. } => {
                tracing::trace!(
                    "collect_lsp_changes: processing Insert at position {}",
                    position
                );
                // For insert: create a zero-width range at the insertion point
                let (line, character) = self
                    .active_state()
                    .buffer
                    .position_to_lsp_position(*position);
                let lsp_pos = Position::new(line as u32, character as u32);
                let lsp_range = LspRange::new(lsp_pos, lsp_pos);
                vec![TextDocumentContentChangeEvent {
                    range: Some(lsp_range),
                    range_length: None,
                    text: text.clone(),
                }]
            }
            Event::Delete { range, .. } => {
                tracing::trace!("collect_lsp_changes: processing Delete range {:?}", range);
                // For delete: create a range from start to end, send empty string
                let (start_line, start_char) = self
                    .active_state()
                    .buffer
                    .position_to_lsp_position(range.start);
                let (end_line, end_char) = self
                    .active_state()
                    .buffer
                    .position_to_lsp_position(range.end);
                let lsp_range = LspRange::new(
                    Position::new(start_line as u32, start_char as u32),
                    Position::new(end_line as u32, end_char as u32),
                );
                vec![TextDocumentContentChangeEvent {
                    range: Some(lsp_range),
                    range_length: None,
                    text: String::new(),
                }]
            }
            Event::Batch { events, .. } => {
                // Collect all changes from sub-events into a single vector
                // This allows sending all changes in one didChange notification
                tracing::trace!(
                    "collect_lsp_changes: processing Batch with {} events",
                    events.len()
                );
                let mut all_changes = Vec::new();
                for sub_event in events {
                    all_changes.extend(self.collect_lsp_changes(sub_event));
                }
                all_changes
            }
            _ => Vec::new(), // Ignore cursor movements and other events
        }
    }

    /// Calculate line information for an event (before buffer modification)
    /// This provides accurate line numbers for plugin hooks to track changes.
    ///
    /// ## Design Alternatives for Line Tracking
    ///
    /// **Approach 1: Re-diff on every edit (VSCode style)**
    /// - Store original file content, re-run diff algorithm after each edit
    /// - Simpler conceptually, but O(n) per edit for diff computation
    /// - Better for complex scenarios (multi-cursor, large batch edits)
    ///
    /// **Approach 2: Track line shifts (our approach)**
    /// - Calculate line info BEFORE applying edit (like LSP does)
    /// - Pass `lines_added`/`lines_removed` to plugins via hooks
    /// - Plugins shift their stored line numbers accordingly
    /// - O(1) per edit, but requires careful bookkeeping
    ///
    /// We use Approach 2 because:
    /// - Matches existing LSP infrastructure (`collect_lsp_changes`)
    /// - More efficient for typical editing patterns
    /// - Plugins can choose to re-diff if they need more accuracy
    ///
    pub(super) fn calculate_event_line_info(&self, event: &Event) -> super::types::EventLineInfo {
        match event {
            Event::Insert { position, text, .. } => {
                // Get line number at insert position (from original buffer)
                let start_line = self.active_state().buffer.get_line_number(*position);

                // Count newlines in inserted text to determine lines added
                let lines_added = text.matches('\n').count();
                let end_line = start_line + lines_added;

                super::types::EventLineInfo {
                    start_line,
                    end_line,
                    line_delta: lines_added as i32,
                }
            }
            Event::Delete {
                range,
                deleted_text,
                ..
            } => {
                // Get line numbers for the deleted range (from original buffer)
                let start_line = self.active_state().buffer.get_line_number(range.start);
                let end_line = self.active_state().buffer.get_line_number(range.end);

                // Count newlines in deleted text to determine lines removed
                let lines_removed = deleted_text.matches('\n').count();

                super::types::EventLineInfo {
                    start_line,
                    end_line,
                    line_delta: -(lines_removed as i32),
                }
            }
            Event::Batch { events, .. } => {
                // For batches, compute cumulative line info
                // This is a simplification - we report the range covering all changes
                let mut min_line = usize::MAX;
                let mut max_line = 0usize;
                let mut total_delta = 0i32;

                for sub_event in events {
                    let info = self.calculate_event_line_info(sub_event);
                    min_line = min_line.min(info.start_line);
                    max_line = max_line.max(info.end_line);
                    total_delta += info.line_delta;
                }

                if min_line == usize::MAX {
                    min_line = 0;
                }

                super::types::EventLineInfo {
                    start_line: min_line,
                    end_line: max_line,
                    line_delta: total_delta,
                }
            }
            _ => super::types::EventLineInfo::default(),
        }
    }

    /// Notify LSP of a file save
    pub(super) fn notify_lsp_save(&mut self) {
        let buffer_id = self.active_buffer();
        self.notify_lsp_save_buffer(buffer_id);
    }

    /// Notify LSP of a file save for a specific buffer
    pub(super) fn notify_lsp_save_buffer(&mut self, buffer_id: BufferId) {
        // Check if LSP is enabled for this buffer
        let metadata = match self.buffer_metadata.get(&buffer_id) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "notify_lsp_save_buffer: no metadata for buffer {:?}",
                    buffer_id
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            tracing::debug!(
                "notify_lsp_save_buffer: LSP disabled for buffer {:?}",
                buffer_id
            );
            return;
        }

        // Get file path for LSP spawn
        let file_path = metadata.file_path().cloned();

        // Get the URI
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!("notify_lsp_save_buffer: no URI for buffer {:?}", buffer_id);
                return;
            }
        };

        // Get the file path for language detection
        // Use buffer's stored language
        let language = match self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        {
            Some(l) => l,
            None => {
                tracing::debug!("notify_lsp_save: no buffer state");
                return;
            }
        };

        // Get the full text to send with didSave
        let full_text = match self.active_state().buffer.to_string() {
            Some(t) => t,
            None => {
                tracing::debug!("notify_lsp_save: buffer not fully loaded");
                return;
            }
        };
        tracing::debug!(
            "notify_lsp_save: sending didSave to {} (text length: {} bytes)",
            uri.as_str(),
            full_text.len()
        );

        // Only send didSave if LSP is already running (respect auto_start setting)
        if let Some(lsp) = &mut self.lsp {
            use crate::services::lsp::manager::LspSpawnResult;
            if lsp.try_spawn(&language, file_path.as_deref()) != LspSpawnResult::Spawned {
                tracing::debug!(
                    "notify_lsp_save: LSP not running for {} (auto_start disabled)",
                    language
                );
                return;
            }
            // Broadcast didSave to all handles for this language
            let mut any_sent = false;
            for sh in lsp.get_handles_mut(&language) {
                if let Err(e) = sh.handle.did_save(uri.clone(), Some(full_text.clone())) {
                    tracing::warn!("Failed to send didSave to '{}': {}", sh.name, e);
                } else {
                    any_sent = true;
                }
            }
            if any_sent {
                tracing::info!("Successfully sent didSave to LSP");
            } else {
                tracing::warn!("notify_lsp_save: no LSP handles for {}", language);
            }
        } else {
            tracing::debug!("notify_lsp_save: no LSP manager available");
        }
    }
}
