//! Recovery and auto-save operations for the Editor.
//!
//! This module contains crash recovery and auto-save functionality:
//! - Starting/ending recovery sessions
//! - Checking for and listing recoverable files
//! - Recovering buffers from crash
//! - Auto-saving modified buffers
//! - Cleaning up recovery files

use std::io;

use crate::model::event::BufferId;

use super::Editor;

impl Editor {
    /// Start the recovery session (call on editor startup after recovery check)
    pub fn start_recovery_session(&mut self) -> io::Result<()> {
        self.recovery_service.start_session()
    }

    /// End the recovery session cleanly (call on normal shutdown)
    pub fn end_recovery_session(&mut self) -> io::Result<()> {
        self.recovery_service.end_session()
    }

    /// Check if there are files to recover from a crash
    pub fn has_recovery_files(&self) -> io::Result<bool> {
        self.recovery_service.should_offer_recovery()
    }

    /// Get list of recoverable files
    pub fn list_recoverable_files(
        &self,
    ) -> io::Result<Vec<crate::services::recovery::RecoveryEntry>> {
        self.recovery_service.list_recoverable()
    }

    /// Recover all buffers from recovery files
    /// Returns the number of buffers recovered
    pub fn recover_all_buffers(&mut self) -> io::Result<usize> {
        use crate::services::recovery::RecoveryResult;

        let entries = self.recovery_service.list_recoverable()?;
        let mut recovered_count = 0;

        for entry in entries {
            match self.recovery_service.accept_recovery(&entry) {
                Ok(RecoveryResult::Recovered {
                    original_path,
                    content,
                }) => {
                    // Full content recovery (new/small buffers)
                    let text = String::from_utf8_lossy(&content).into_owned();

                    if let Some(path) = original_path {
                        // Open the file path (this creates the buffer)
                        if self.open_file(&path).is_ok() {
                            // Replace buffer content with recovered content
                            let state = self.active_state_mut();
                            let total = state.buffer.total_bytes();
                            state.buffer.delete(0..total);
                            state.buffer.insert(0, &text);
                            // Mark as modified since it differs from disk
                            state.buffer.set_modified(true);
                            recovered_count += 1;
                            tracing::info!("Recovered buffer: {}", path.display());
                        }
                    } else {
                        // Unsaved buffer - create new buffer with recovered content
                        self.new_buffer();
                        let state = self.active_state_mut();
                        state.buffer.insert(0, &text);
                        state.buffer.set_modified(true);
                        recovered_count += 1;
                        tracing::info!("Recovered unsaved buffer");
                    }
                }
                Ok(RecoveryResult::RecoveredChunks {
                    original_path,
                    chunks,
                }) => {
                    // Chunked recovery for large files - apply chunks directly
                    if self.open_file(&original_path).is_ok() {
                        let state = self.active_state_mut();

                        // Apply chunks in reverse order to preserve offsets
                        // Each chunk: delete original_len bytes at offset, then insert content
                        for chunk in chunks.into_iter().rev() {
                            let text = String::from_utf8_lossy(&chunk.content).into_owned();
                            if chunk.original_len > 0 {
                                state
                                    .buffer
                                    .delete(chunk.offset..chunk.offset + chunk.original_len);
                            }
                            state.buffer.insert(chunk.offset, &text);
                        }

                        // Mark as modified since it differs from disk
                        state.buffer.set_modified(true);
                        recovered_count += 1;
                        tracing::info!("Recovered buffer with chunks: {}", original_path.display());
                    }
                }
                Ok(RecoveryResult::OriginalFileModified { id, original_path }) => {
                    tracing::warn!(
                        "Recovery file {} skipped: original file {} was modified",
                        id,
                        original_path.display()
                    );
                    // Delete the recovery file since it's no longer valid
                    let _ = self.recovery_service.discard_recovery(&entry);
                }
                Ok(RecoveryResult::Corrupted { id, reason }) => {
                    tracing::warn!("Recovery file {} corrupted: {}", id, reason);
                }
                Ok(RecoveryResult::NotFound { id }) => {
                    tracing::warn!("Recovery file {} not found", id);
                }
                Err(e) => {
                    tracing::warn!("Failed to recover {}: {}", entry.id, e);
                }
            }
        }

        Ok(recovered_count)
    }

    /// Discard all recovery files without recovering
    pub fn discard_all_recovery(&mut self) -> io::Result<usize> {
        self.recovery_service.discard_all_recovery()
    }

    /// Perform auto-save for all modified buffers if needed
    /// Returns the number of buffers saved, or an error
    ///
    /// This function is designed to be called frequently (every frame) and will:
    /// - Return immediately if recovery is disabled
    /// - Return immediately if the auto-save interval hasn't passed
    /// - Return immediately if no buffers are modified
    /// - Only save buffers that are marked as needing a save
    pub fn auto_save_dirty_buffers(&mut self) -> io::Result<usize> {
        // Early exit if disabled
        if !self.recovery_service.is_enabled() {
            return Ok(0);
        }

        // Check if enough time has passed since last auto-save
        let interval =
            std::time::Duration::from_secs(self.config.editor.auto_save_interval_secs as u64);
        if self.time_source.elapsed_since(self.last_auto_save) < interval {
            return Ok(0);
        }

        // Collect buffer info first to avoid borrow issues
        // Only include buffers that have pending recovery changes AND need auto-save
        let buffer_info: Vec<_> = self
            .buffers
            .iter()
            .filter_map(|(buffer_id, state)| {
                let recovery_pending = state.buffer.is_recovery_pending();
                if recovery_pending {
                    let path = state.buffer.file_path().map(|p| p.to_path_buf());
                    let recovery_id = self.recovery_service.get_buffer_id(path.as_deref());
                    // Only save if enough time has passed since last recovery save
                    if self
                        .recovery_service
                        .needs_auto_save(&recovery_id, recovery_pending)
                    {
                        Some((*buffer_id, recovery_id, path))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        // Early exit if nothing to save
        if buffer_info.is_empty() {
            // Still update the timer to avoid checking buffers too frequently
            self.last_auto_save = self.time_source.now();
            return Ok(0);
        }

        let mut saved_count = 0;

        for (buffer_id, recovery_id, path) in buffer_info {
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                let line_count = state.buffer.line_count();

                // For large files, use chunked recovery to avoid reading entire file
                if state.buffer.is_large_file() {
                    let chunks = state.buffer.get_recovery_chunks();

                    // If no modifications, skip saving (original file is recovery)
                    if chunks.is_empty() {
                        state.buffer.set_recovery_pending(false);
                        continue;
                    }

                    // Convert to RecoveryChunk format
                    let recovery_chunks: Vec<_> = chunks
                        .into_iter()
                        .map(|(offset, content)| {
                            crate::services::recovery::types::RecoveryChunk::new(
                                offset, 0, // For insertions, original_len is 0
                                content,
                            )
                        })
                        .collect();

                    let original_size = state.buffer.original_file_size().unwrap_or(0);
                    let final_size = state.buffer.total_bytes();

                    tracing::debug!(
                        "auto_save_dirty_buffers: large file recovery - original_size={}, final_size={}, path={:?}",
                        original_size,
                        final_size,
                        path
                    );

                    self.recovery_service.save_buffer(
                        &recovery_id,
                        recovery_chunks,
                        path.as_deref(),
                        None,
                        line_count,
                        original_size,
                        final_size,
                    )?;

                    tracing::debug!(
                        "Saved chunked recovery for large file (original: {} bytes, final: {} bytes)",
                        original_size,
                        final_size
                    );
                } else {
                    // For small files, save full content as a single chunk
                    let total_bytes = state.buffer.total_bytes();
                    let content = match state.buffer.get_text_range_mut(0, total_bytes) {
                        Ok(bytes) => bytes,
                        Err(e) => {
                            tracing::warn!("Failed to get buffer content for recovery save: {}", e);
                            continue;
                        }
                    };

                    let chunks = vec![crate::services::recovery::types::RecoveryChunk::new(
                        0, 0, content,
                    )];
                    self.recovery_service.save_buffer(
                        &recovery_id,
                        chunks,
                        path.as_deref(),
                        None,
                        line_count,
                        0,           // original_file_size = 0 for new/small files
                        total_bytes, // final_size
                    )?;
                }

                // Clear recovery_pending flag after successful save
                state.buffer.set_recovery_pending(false);
                saved_count += 1;
            }
        }

        self.last_auto_save = self.time_source.now();
        Ok(saved_count)
    }

    /// Check if the active buffer is marked dirty for recovery auto-save
    /// Used for testing to verify that edits properly trigger recovery tracking
    pub fn is_active_buffer_recovery_dirty(&self) -> bool {
        if let Some(state) = self.buffers.get(&self.active_buffer()) {
            state.buffer.is_recovery_pending()
        } else {
            false
        }
    }

    /// Delete recovery for a buffer (call after saving or closing)
    pub fn delete_buffer_recovery(&mut self, buffer_id: BufferId) -> io::Result<()> {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let path = state.buffer.file_path().map(|p| p.to_path_buf());
            let recovery_id = self.recovery_service.get_buffer_id(path.as_deref());
            self.recovery_service.delete_buffer_recovery(&recovery_id)?;
            // Clear recovery_pending since buffer is now saved
            state.buffer.set_recovery_pending(false);
        }
        Ok(())
    }
}
