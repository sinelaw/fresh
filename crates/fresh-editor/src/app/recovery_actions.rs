//! Recovery and auto-save operations for the Editor.
//!
//! This module contains crash recovery and auto-save functionality:
//! - Starting/ending recovery sessions
//! - Checking for and listing recoverable files
//! - Recovering buffers from crash
//! - Auto-saving modified buffers
//! - Cleaning up recovery files

use anyhow::Result as AnyhowResult;

use crate::model::event::BufferId;

use super::Editor;

impl Editor {
    /// Start the recovery session (call on editor startup after recovery check)
    pub fn start_recovery_session(&mut self) -> AnyhowResult<()> {
        Ok(self.recovery_service.start_session()?)
    }

    /// End the recovery session cleanly (call on normal shutdown)
    pub fn end_recovery_session(&mut self) -> AnyhowResult<()> {
        let hot_exit = self.config.editor.hot_exit;

        if hot_exit {
            // Force all modified buffers to be re-saved by marking them pending,
            // then reuse the existing periodic recovery save logic.
            for (_, state) in self.buffers.iter_mut() {
                if state.buffer.is_modified() {
                    state.buffer.set_recovery_pending(true);
                }
            }
            self.save_pending_recovery_buffers()?;

            // Collect recovery IDs for buffers that should survive this session
            let preserve_ids = self.recovery_ids_to_preserve();
            Ok(self
                .recovery_service
                .end_session_preserving(&preserve_ids)?)
        } else {
            Ok(self.recovery_service.end_session()?)
        }
    }

    /// Collect recovery IDs for all buffers that should be preserved across sessions.
    fn recovery_ids_to_preserve(&self) -> Vec<String> {
        let hot_exit = self.config.editor.hot_exit;

        self.buffer_metadata
            .iter()
            .filter_map(|(buffer_id, meta)| {
                if meta.hidden_from_tabs || meta.is_virtual() {
                    return None;
                }
                if !hot_exit {
                    return None;
                }
                let state = self.buffers.get(buffer_id)?;
                if !state.buffer.is_modified() {
                    return None;
                }
                let path = meta.file_path()?;
                let is_unnamed = path.as_os_str().is_empty();
                if is_unnamed && state.buffer.total_bytes() == 0 {
                    return None;
                }
                // Use stored recovery_id, or compute from path for file-backed buffers
                meta.recovery_id.clone().or_else(|| {
                    let file_path = state.buffer.file_path().map(|p| p.to_path_buf());
                    Some(self.recovery_service.get_buffer_id(file_path.as_deref()))
                })
            })
            .collect()
    }

    /// Check if there are files to recover from a crash
    pub fn has_recovery_files(&self) -> AnyhowResult<bool> {
        Ok(self.recovery_service.should_offer_recovery()?)
    }

    /// Get list of recoverable files
    pub fn list_recoverable_files(
        &self,
    ) -> AnyhowResult<Vec<crate::services::recovery::RecoveryEntry>> {
        Ok(self.recovery_service.list_recoverable()?)
    }

    /// Recover all buffers from recovery files
    /// Returns the number of buffers recovered
    pub fn recover_all_buffers(&mut self) -> AnyhowResult<usize> {
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
                        match self.open_file(&path) {
                            Ok(_) => {
                                // Replace buffer content with recovered content
                                {
                                    let state = self.active_state_mut();
                                    let total = state.buffer.total_bytes();
                                    state.buffer.delete(0..total);
                                    state.buffer.insert(0, &text);
                                    // Mark as modified since it differs from disk
                                    state.buffer.set_modified(true);
                                }
                                // Invalidate the event log's saved position so undo
                                // can't incorrectly clear the modified flag
                                self.active_event_log_mut().clear_saved_position();
                                recovered_count += 1;
                                tracing::info!("Recovered buffer: {}", path.display());
                            }
                            Err(e) => {
                                // Check if this is a large file encoding confirmation error
                                if let Some(confirmation) = e.downcast_ref::<
                                    crate::model::buffer::LargeFileEncodingConfirmation,
                                >() {
                                    self.start_large_file_encoding_confirmation(confirmation);
                                } else {
                                    tracing::warn!("Failed to recover buffer {}: {}", path.display(), e);
                                }
                            }
                        }
                    } else {
                        // Unsaved buffer - create new buffer with recovered content
                        self.new_buffer();
                        {
                            let state = self.active_state_mut();
                            state.buffer.insert(0, &text);
                            state.buffer.set_modified(true);
                        }
                        // Invalidate the event log's saved position so undo
                        // can't incorrectly clear the modified flag
                        self.active_event_log_mut().clear_saved_position();
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
                        {
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
                        }
                        // Invalidate the event log's saved position so undo
                        // can't incorrectly clear the modified flag
                        self.active_event_log_mut().clear_saved_position();
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
                    // Keep the recovery file so the user can manually inspect it.
                    // Show a warning so the user knows unsaved changes exist.
                    let name = original_path
                        .file_name()
                        .unwrap_or_default()
                        .to_string_lossy();
                    self.set_status_message(format!(
                        "{} changed on disk; unsaved changes not restored",
                        name
                    ));
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

    /// Discard all recovery files (user decided not to recover)
    /// Returns the number of recovery files deleted
    pub fn discard_all_recovery(&mut self) -> AnyhowResult<usize> {
        Ok(self.recovery_service.discard_all_recovery()?)
    }

    /// Perform auto-recovery-save for all modified buffers if needed.
    /// Called frequently (every frame); rate-limited by `auto_recovery_save_interval_secs`.
    pub fn auto_recovery_save_dirty_buffers(&mut self) -> AnyhowResult<usize> {
        if !self.recovery_service.is_enabled() {
            return Ok(0);
        }

        let interval = std::time::Duration::from_secs(
            self.config.editor.auto_recovery_save_interval_secs as u64,
        );
        if self.time_source.elapsed_since(self.last_auto_recovery_save) < interval {
            return Ok(0);
        }

        let saved = self.save_pending_recovery_buffers()?;
        self.last_auto_recovery_save = self.time_source.now();
        Ok(saved)
    }

    /// Save all buffers marked `recovery_pending` to recovery storage.
    /// Shared by the periodic auto-save and the exit flush.
    fn save_pending_recovery_buffers(&mut self) -> AnyhowResult<usize> {
        if !self.recovery_service.is_enabled() {
            return Ok(0);
        }

        // Collect buffer IDs that need recovery (immutable pass).
        // Skip composite/hidden buffers — they are not real user content.
        let buffers_needing_recovery: Vec<_> = self
            .buffers
            .iter()
            .filter_map(|(buffer_id, state)| {
                if state.is_composite_buffer {
                    return None;
                }
                if let Some(meta) = self.buffer_metadata.get(buffer_id) {
                    if meta.hidden_from_tabs || meta.is_virtual() {
                        return None;
                    }
                }
                if state.buffer.is_recovery_pending() {
                    Some(*buffer_id)
                } else {
                    None
                }
            })
            .collect();

        // Ensure unnamed buffers have stable recovery IDs (mutable pass).
        for buffer_id in &buffers_needing_recovery {
            let needs_id = self
                .buffer_metadata
                .get(buffer_id)
                .map(|meta| {
                    let path = meta.file_path();
                    let is_unnamed = path.map(|p| p.as_os_str().is_empty()).unwrap_or(true);
                    is_unnamed && meta.recovery_id.is_none()
                })
                .unwrap_or(false);

            if needs_id {
                let new_id = crate::services::recovery::generate_buffer_id();
                if let Some(meta) = self.buffer_metadata.get_mut(buffer_id) {
                    meta.recovery_id = Some(new_id);
                }
            }
        }

        // Collect full buffer info with stable recovery IDs.
        let buffer_info: Vec<_> = buffers_needing_recovery
            .into_iter()
            .filter_map(|buffer_id| {
                let state = self.buffers.get(&buffer_id)?;
                let meta = self.buffer_metadata.get(&buffer_id)?;
                let path = state.buffer.file_path().map(|p| p.to_path_buf());
                let recovery_id = if let Some(ref stored_id) = meta.recovery_id {
                    stored_id.clone()
                } else {
                    self.recovery_service.get_buffer_id(path.as_deref())
                };
                let recovery_pending = state.buffer.is_recovery_pending();
                if self
                    .recovery_service
                    .needs_auto_recovery_save(&recovery_id, recovery_pending)
                {
                    Some((buffer_id, recovery_id, path))
                } else {
                    None
                }
            })
            .collect();

        let mut saved_count = 0;
        for (buffer_id, recovery_id, path) in buffer_info {
            if self.save_buffer_to_recovery(&buffer_id, &recovery_id, path.as_deref())? {
                saved_count += 1;
            }
        }
        Ok(saved_count)
    }

    /// Check if the active buffer is marked dirty for auto-recovery-save
    /// Used for testing to verify that edits properly trigger recovery tracking
    pub fn is_active_buffer_recovery_dirty(&self) -> bool {
        if let Some(state) = self.buffers.get(&self.active_buffer()) {
            state.buffer.is_recovery_pending()
        } else {
            false
        }
    }

    /// Delete recovery for a buffer (call after saving or closing)
    pub fn delete_buffer_recovery(&mut self, buffer_id: BufferId) -> AnyhowResult<()> {
        // Get recovery_id: use stored one for unnamed buffers, compute from path otherwise
        let recovery_id = {
            let meta = self.buffer_metadata.get(&buffer_id);
            let state = self.buffers.get(&buffer_id);

            if let Some(stored_id) = meta.and_then(|m| m.recovery_id.clone()) {
                stored_id
            } else if let Some(state) = state {
                let path = state.buffer.file_path().map(|p| p.to_path_buf());
                self.recovery_service.get_buffer_id(path.as_deref())
            } else {
                return Ok(());
            }
        };

        self.recovery_service.delete_buffer_recovery(&recovery_id)?;

        // Clear recovery_pending since buffer is now saved
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.set_recovery_pending(false);
        }
        Ok(())
    }

    /// Save a single buffer's content to recovery storage.
    ///
    /// For large files, saves only modified chunks (diffs against original).
    /// For small files / unnamed buffers, saves full content.
    /// Returns true if a save was performed, false if skipped.
    fn save_buffer_to_recovery(
        &mut self,
        buffer_id: &BufferId,
        recovery_id: &str,
        path: Option<&std::path::Path>,
    ) -> AnyhowResult<bool> {
        let state = match self.buffers.get_mut(buffer_id) {
            Some(s) => s,
            None => return Ok(false),
        };
        let line_count = state.buffer.line_count();

        if state.buffer.is_large_file() {
            let chunks = state.buffer.get_recovery_chunks();
            if chunks.is_empty() {
                state.buffer.set_recovery_pending(false);
                return Ok(false);
            }
            let recovery_chunks: Vec<_> = chunks
                .into_iter()
                .map(|(offset, content)| {
                    crate::services::recovery::types::RecoveryChunk::new(offset, 0, content)
                })
                .collect();
            let original_size = state.buffer.original_file_size().unwrap_or(0);
            let final_size = state.buffer.total_bytes();
            self.recovery_service.save_buffer(
                recovery_id,
                recovery_chunks,
                path,
                None,
                line_count,
                original_size,
                final_size,
            )?;
        } else {
            let total_bytes = state.buffer.total_bytes();
            let content = match state.buffer.get_text_range_mut(0, total_bytes) {
                Ok(bytes) => bytes,
                Err(e) => {
                    tracing::warn!("Failed to get buffer content for recovery save: {}", e);
                    return Ok(false);
                }
            };
            let chunks = vec![crate::services::recovery::types::RecoveryChunk::new(
                0, 0, content,
            )];
            self.recovery_service.save_buffer(
                recovery_id,
                chunks,
                path,
                None,
                line_count,
                0,
                total_bytes,
            )?;
        }

        state.buffer.set_recovery_pending(false);
        Ok(true)
    }
}
