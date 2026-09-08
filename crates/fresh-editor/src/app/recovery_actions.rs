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
use fresh_core::WindowId;
use std::path::PathBuf;

use super::Editor;

impl Editor {
    /// Push the buffer's full current content to LSP after an out-of-band
    /// mutation (hot-exit recovery replay / crash-recovery replay). The
    /// replay paths edit the buffer directly via `buffer.delete` and
    /// `buffer.insert`, which don't route through the normal event log
    /// that also emits `didChange`. If LSP's `didOpen` already fired with
    /// the on-disk content, the server is left on a stale base and every
    /// position it returns is offset by the net byte delta.
    pub(crate) fn sync_lsp_after_recovery_replay(&mut self, buffer_id: BufferId) {
        self.active_window_mut()
            .sync_lsp_after_recovery_replay(buffer_id);
    }

    /// Start the recovery session (call on editor startup after recovery check)
    pub fn start_recovery_session(&mut self) -> AnyhowResult<()> {
        Ok(self.recovery_service.lock().unwrap().start_session()?)
    }

    /// End the recovery session cleanly (call on normal shutdown)
    ///
    /// Everything here spans **every open workspace**, not just the active
    /// one. Exiting closes them all, so a dirty buffer in a background
    /// Orchestrator workspace has to be flushed to recovery and preserved
    /// exactly like a foreground one; flushing and preserving only the active
    /// window is what let a quit from a clean workspace delete another
    /// workspace's unsaved work (issue #3189).
    pub fn end_recovery_session(&mut self) -> AnyhowResult<()> {
        let hot_exit = self.config.editor.hot_exit;

        if hot_exit {
            // Force all modified buffers to be re-saved by marking them pending,
            // then reuse the existing periodic recovery save logic. The flush
            // matters most for background workspaces: the periodic auto-save
            // only ever runs against the active window, so a buffer edited and
            // then switched away from within the auto-save interval has
            // nothing on disk at all until this runs.
            for window in self.windows.values_mut() {
                for (_, state) in &mut window.buffers {
                    if state.buffer.is_modified() {
                        state.buffer.set_recovery_pending(true);
                    }
                }
            }
            for window_id in self.window_ids_sorted() {
                self.with_window_retargeted(window_id, |editor| {
                    editor.save_pending_recovery_buffers()
                })?;
            }
        }

        // Collect recovery IDs for buffers that should survive this session,
        // plus the ids this session can account for at all. Entries in
        // neither set belong to work no window here ever held — see
        // `end_session_accounting`. With `hot_exit` off nothing is preserved
        // (the quit prompt has already forced every live buffer to be saved
        // or discarded), but the accounting still protects a workspace this
        // session never materialized.
        let preserve_ids = self.recovery_ids_to_preserve();
        let known_ids = self.live_recovery_ids();
        Ok(self
            .recovery_service
            .lock()
            .unwrap()
            .end_session_accounting(&preserve_ids, &known_ids)?)
    }

    /// Flush every modified buffer in `window_id` to recovery storage.
    ///
    /// Called just before a workspace is dropped. `close_window` tears the
    /// `Window` down with its buffers and asks nothing about unsaved changes —
    /// the confirmation belongs to the Orchestrator UI that drives the close
    /// (Delete / Archive / Kill), not to a function the plugin calls after the
    /// user has already decided. What *is* this layer's job is making sure the
    /// content is somewhere recoverable before it goes: without this the
    /// buffer's most recent edits exist nowhere but memory, and closing the
    /// workspace destroys them (issue #3189). Afterwards the entry is one no
    /// live buffer backs, which `end_session_accounting` will preserve rather
    /// than clean up, so it survives to the next start.
    ///
    /// Gated on `hot_exit` to match [`Editor::end_recovery_session`]: with hot
    /// exit off, recovery is a crash net that a clean exit clears, and writing
    /// content nothing will offer back would only leave litter.
    ///
    /// Returns the number of buffers written. Never fails the close: a
    /// recovery-write error is logged by the caller.
    pub(crate) fn flush_window_recovery(&mut self, window_id: WindowId) -> AnyhowResult<usize> {
        if !self.config.editor.hot_exit || !self.windows.contains_key(&window_id) {
            return Ok(0);
        }
        if let Some(window) = self.windows.get_mut(&window_id) {
            for (_, state) in &mut window.buffers {
                if state.buffer.is_modified() {
                    state.buffer.set_recovery_pending(true);
                }
            }
        }
        self.with_window_retargeted(window_id, |editor| editor.save_pending_recovery_buffers())
    }

    /// Collect recovery IDs for all buffers that should be preserved across
    /// sessions, across every open workspace.
    fn recovery_ids_to_preserve(&self) -> Vec<String> {
        let hot_exit = self.config.editor.hot_exit;
        if !hot_exit {
            return Vec::new();
        }

        let mut out = Vec::new();
        for window in self.windows.values() {
            for (buffer_id, meta) in window.buffer_metadata.iter() {
                if meta.hidden_from_tabs || meta.is_virtual() {
                    continue;
                }
                let Some(state) = window.buffers.get(buffer_id) else {
                    continue;
                };
                if !state.buffer.is_modified() {
                    continue;
                }
                let Some(path) = meta.file_path() else {
                    continue;
                };
                let is_unnamed = path.as_os_str().is_empty();
                if is_unnamed && state.buffer.total_bytes() == 0 {
                    continue;
                }
                // Use stored recovery_id, or compute from path for file-backed buffers
                if let Some(id) = meta.recovery_id.clone().or_else(|| {
                    state
                        .buffer
                        .file_path()
                        .filter(|p| !p.as_os_str().is_empty())
                        .map(crate::services::recovery::path_hash)
                }) {
                    out.push(id);
                }
            }
        }
        out
    }

    /// Recovery ids this session actually has a live buffer for, across every
    /// open workspace.
    ///
    /// This is the "accounted for" set handed to
    /// [`RecoveryService::end_session_accounting`]: an on-disk entry in this
    /// set that is not also being preserved was resolved during the session
    /// (saved to disk, or discarded at the quit prompt) and is safe to delete.
    /// An entry outside it was never in memory here, so this session has no
    /// standing to throw it away.
    fn live_recovery_ids(&self) -> Vec<String> {
        let mut out = Vec::new();
        for window in self.windows.values() {
            for (buffer_id, meta) in window.buffer_metadata.iter() {
                if let Some(id) = &meta.recovery_id {
                    out.push(id.clone());
                    continue;
                }
                // File-backed buffers derive their id from the path, so a
                // buffer that never needed a recovery save still owns the
                // entry a previous session may have written for that file.
                let Some(state) = window.buffers.get(buffer_id) else {
                    continue;
                };
                if let Some(path) = state.buffer.file_path() {
                    if !path.as_os_str().is_empty() {
                        out.push(crate::services::recovery::path_hash(path));
                    }
                }
            }
        }
        out
    }

    /// Which open workspace a recovery entry belongs to.
    ///
    /// The `workspace_id` stamped at save time is authoritative — it survives
    /// a file being opened from outside its workspace root, and it separates
    /// two Orchestrator workspaces that deliberately share one worktree.
    /// Entries written before that field existed (and by writers with no
    /// workspace context) fall back to the longest workspace root that is a
    /// prefix of the original path, which is right for the ordinary case of a
    /// file living inside its own project.
    ///
    /// `None` means no open workspace claims it: either it belongs to a
    /// workspace that is not open in this session, or it is a pathless
    /// (unnamed) legacy entry. Such an entry is left strictly alone — never
    /// adopted into an unrelated workspace, and never deleted, since nothing
    /// here can say the user is done with it.
    fn recovery_entry_owner(
        &self,
        entry: &crate::services::recovery::RecoveryEntry,
    ) -> Option<WindowId> {
        if let Some(workspace_id) = entry.metadata.workspace_id.as_deref() {
            return self
                .windows
                .iter()
                .find(|(_, w)| w.stable_id == workspace_id)
                .map(|(id, _)| *id);
        }
        let path = entry.metadata.original_path.as_ref()?;
        self.windows
            .iter()
            .filter(|(_, w)| path.starts_with(&w.root))
            .max_by_key(|(_, w)| w.root.as_os_str().len())
            .map(|(id, _)| *id)
    }

    /// Restore this workspace's own leftover recovery entries into it.
    ///
    /// Run when a workspace becomes active — at startup for the foreground
    /// one, and on each dive for the rest — so unsaved work returns to the
    /// workspace it was done in instead of every entry being piled into
    /// whichever workspace happened to be in front (issue #3189).
    ///
    /// Deliberately narrow. It only opens entries this workspace owns and
    /// does not already have open, and it *reads* them (`load_recovery`)
    /// rather than consuming them: the entry stays on disk, now backed by a
    /// live modified buffer, so the ordinary accounting at exit decides its
    /// fate. Entries belonging to a workspace the user never visits are
    /// simply never touched, which is what keeps them from being lost when
    /// only some workspaces are activated.
    ///
    /// Returns the number of buffers opened.
    ///
    /// `claim_unowned` widens the net to entries no open workspace claims —
    /// pathless legacy entries, and files under no workspace root. Set only
    /// for the one startup pass on the foreground workspace, where the old
    /// behaviour was to open everything there and dropping them silently
    /// would be the regression. It must stay `false` on later activations, or
    /// each workspace in turn would adopt its own copy of the same
    /// unattributable entry.
    pub(crate) fn adopt_recovery_for_active_window(
        &mut self,
        claim_unowned: bool,
    ) -> AnyhowResult<usize> {
        use crate::services::recovery::RecoveryResult;

        let entries = self.recovery_service.lock().unwrap().list_recoverable()?;
        if entries.is_empty() {
            return Ok(0);
        }
        let active = self.active_window;
        let already_open: std::collections::HashSet<PathBuf> = self
            .active_window()
            .buffers
            .iter()
            .filter_map(|(_, state)| state.buffer.file_path().map(|p| p.to_path_buf()))
            .collect();

        let mine: Vec<_> = entries
            .into_iter()
            .filter(|entry| match self.recovery_entry_owner(entry) {
                Some(owner) => owner == active,
                None => claim_unowned,
            })
            .filter(|entry| {
                entry
                    .metadata
                    .original_path
                    .as_ref()
                    .is_none_or(|p| !already_open.contains(p))
            })
            .collect();

        let mut adopted = 0;
        for entry in mine {
            let loaded = self.recovery_service.lock().unwrap().load_recovery(&entry);
            let (path, text) = match loaded {
                Ok(RecoveryResult::Recovered {
                    original_path,
                    content,
                }) => (
                    original_path,
                    String::from_utf8_lossy(&content).into_owned(),
                ),
                Ok(RecoveryResult::RecoveredChunks {
                    original_path,
                    chunks,
                }) => {
                    // Large file: the entry holds deltas against what is on
                    // disk, so open the file and replay them in reverse (later
                    // offsets first) so earlier edits don't shift them.
                    let Ok(buffer_id) = self.open_file(&original_path) else {
                        tracing::warn!("Recovery adopt failed to open {}", original_path.display());
                        continue;
                    };
                    {
                        let state = self.active_state_mut();
                        for chunk in chunks.into_iter().rev() {
                            let text = String::from_utf8_lossy(&chunk.content).into_owned();
                            if chunk.original_len > 0 {
                                state
                                    .buffer
                                    .delete(chunk.offset..chunk.offset + chunk.original_len);
                            }
                            state.buffer.insert(chunk.offset, &text);
                        }
                        state.buffer.set_modified(true);
                        state.buffer.set_recovery_pending(false);
                        state.wrap_indices.damage_all();
                    }
                    self.active_event_log_mut().clear_saved_position();
                    if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id)
                    {
                        meta.recovery_id = Some(entry.id.clone());
                    }
                    self.sync_lsp_after_recovery_replay(buffer_id);
                    adopted += 1;
                    tracing::info!(
                        "Adopted chunked recovery entry {} into workspace {}",
                        entry.id,
                        active
                    );
                    continue;
                }
                Ok(other) => {
                    tracing::debug!("Recovery adopt skipped {}: {:?}", entry.id, other);
                    continue;
                }
                Err(e) => {
                    tracing::warn!("Recovery adopt failed to load {}: {}", entry.id, e);
                    continue;
                }
            };

            let buffer_id = match path {
                Some(ref path) => match self.open_file(path) {
                    Ok(buffer_id) => buffer_id,
                    Err(e) => {
                        tracing::warn!("Recovery adopt failed to open {}: {}", path.display(), e);
                        continue;
                    }
                },
                None => self.new_buffer(),
            };
            {
                let state = self.active_state_mut();
                let total = state.buffer.total_bytes();
                state.buffer.delete(0..total);
                state.buffer.insert(0, &text);
                state.buffer.set_modified(true);
                state.buffer.set_recovery_pending(false);
                // Wholesale replacement, never described as edit damage.
                // See `WrapIndex::damage_all`.
                state.wrap_indices.damage_all();
            }
            self.active_event_log_mut().clear_saved_position();
            if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id) {
                // Keep writing to the same entry, and make it *accounted* so
                // the exit path can resolve it rather than treating it as
                // orphaned forever.
                meta.recovery_id = Some(entry.id.clone());
            }
            self.sync_lsp_after_recovery_replay(buffer_id);
            adopted += 1;
            tracing::info!(
                "Adopted recovery entry {} into workspace {}",
                entry.id,
                active
            );
        }
        Ok(adopted)
    }

    /// Check if there are files to recover from a crash
    pub fn has_recovery_files(&self) -> AnyhowResult<bool> {
        Ok(self
            .recovery_service
            .lock()
            .unwrap()
            .should_offer_recovery()?)
    }

    /// Get list of recoverable files
    pub fn list_recoverable_files(
        &self,
    ) -> AnyhowResult<Vec<crate::services::recovery::RecoveryEntry>> {
        Ok(self.recovery_service.lock().unwrap().list_recoverable()?)
    }

    /// Recover buffers left behind by a crash, into the workspace they came
    /// from.
    ///
    /// The startup half of the per-workspace scheme: it restores what the
    /// *foreground* workspace owns (plus anything no open workspace claims —
    /// see `adopt_recovery_for_active_window`). Every other workspace's
    /// entries stay on disk untouched until that workspace is activated, at
    /// which point `set_active_window` adopts them there. Previously this
    /// opened every entry into whichever workspace happened to be in front,
    /// so a crash reshuffled unsaved work between projects (issue #3189).
    ///
    /// Entries are read, not consumed: each becomes a live modified buffer
    /// that keeps writing to the same entry, and the exit accounting decides
    /// whether it is preserved or cleaned. That is what makes it safe for the
    /// user to quit having visited only some of their workspaces — the ones
    /// they never opened still have their unsaved work on disk.
    ///
    /// Returns the number of buffers recovered.
    pub fn recover_all_buffers(&mut self) -> AnyhowResult<usize> {
        self.adopt_recovery_for_active_window(true)
    }

    /// Restore only the hot-exit content from the previous clean exit:
    /// files with unsaved modifications and unnamed buffers that held
    /// content.  Called when full session restore is opted out (via
    /// `--no-restore` or `editor.restore_previous_session = false`) so
    /// the user does not lose in-progress work just because they asked
    /// to skip restoring the workspace layout.
    ///
    /// Like [`Editor::recover_all_buffers`], this uses `load_recovery` and
    /// leaves the recovery files in place, so the current session's hot-exit
    /// pipeline keeps owning them and the next clean shutdown decides their
    /// fate (`end_session_accounting`).
    ///
    /// Returns the number of buffers restored.
    pub fn try_restore_hot_exit_buffers(&mut self) -> AnyhowResult<usize> {
        use crate::services::recovery::RecoveryResult;

        if !self.config.editor.hot_exit {
            return Ok(0);
        }

        let entries = self.recovery_service.lock().unwrap().list_recoverable()?;
        if entries.is_empty() {
            return Ok(0);
        }

        let mut restored = 0;
        for entry in entries {
            let loaded = self.recovery_service.lock().unwrap().load_recovery(&entry);
            match loaded {
                Ok(RecoveryResult::Recovered {
                    original_path,
                    content,
                }) => {
                    let text = String::from_utf8_lossy(&content).into_owned();
                    if let Some(path) = original_path {
                        match self.open_file(&path) {
                            Ok(buffer_id) => {
                                {
                                    let state = self.active_state_mut();
                                    let total = state.buffer.total_bytes();
                                    state.buffer.delete(0..total);
                                    state.buffer.insert(0, &text);
                                    state.buffer.set_modified(true);
                                    state.buffer.set_recovery_pending(false);
                                    // Wholesale replacement, never described as
                                    // edit damage. See `WrapIndex::damage_all`.
                                    state.wrap_indices.damage_all();
                                }
                                self.active_event_log_mut().clear_saved_position();
                                self.sync_lsp_after_recovery_replay(buffer_id);
                                restored += 1;
                                tracing::info!(
                                    "Hot-exit restore: reopened {} with unsaved changes",
                                    path.display()
                                );
                            }
                            Err(e) => {
                                if let Some(confirmation) = e.downcast_ref::<
                                    crate::model::buffer::LargeFileEncodingConfirmation,
                                >() {
                                    self.start_large_file_encoding_confirmation(confirmation);
                                } else {
                                    tracing::warn!(
                                        "Hot-exit restore failed to open {}: {}",
                                        path.display(),
                                        e
                                    );
                                }
                            }
                        }
                    } else {
                        // Unnamed buffer with content — create a fresh
                        // buffer, drop the recovery ID into metadata so
                        // future hot-exit saves hit the same file.
                        let buffer_id = self.new_buffer();
                        {
                            let state = self.active_state_mut();
                            state.buffer.insert(0, &text);
                            state.buffer.set_modified(true);
                            state.buffer.set_recovery_pending(false);
                        }
                        self.active_event_log_mut().clear_saved_position();
                        if let Some(meta) =
                            self.active_window_mut().buffer_metadata.get_mut(&buffer_id)
                        {
                            meta.recovery_id = Some(entry.id.clone());
                        }
                        self.sync_lsp_after_recovery_replay(buffer_id);
                        restored += 1;
                        tracing::info!(
                            "Hot-exit restore: reopened unnamed buffer (recovery_id={})",
                            entry.id
                        );
                    }
                }
                Ok(RecoveryResult::RecoveredChunks {
                    original_path,
                    chunks,
                }) => match self.open_file(&original_path) {
                    Ok(buffer_id) => {
                        {
                            let state = self.active_state_mut();
                            for chunk in chunks.into_iter().rev() {
                                let text = String::from_utf8_lossy(&chunk.content).into_owned();
                                if chunk.original_len > 0 {
                                    state
                                        .buffer
                                        .delete(chunk.offset..chunk.offset + chunk.original_len);
                                }
                                state.buffer.insert(chunk.offset, &text);
                            }
                            state.buffer.set_modified(true);
                            state.buffer.set_recovery_pending(false);
                        }
                        self.active_event_log_mut().clear_saved_position();
                        self.sync_lsp_after_recovery_replay(buffer_id);
                        restored += 1;
                        tracing::info!(
                            "Hot-exit restore: reopened {} with chunked changes",
                            original_path.display()
                        );
                    }
                    Err(e) => {
                        tracing::warn!(
                            "Hot-exit restore failed to open {}: {}",
                            original_path.display(),
                            e
                        );
                    }
                },
                Ok(RecoveryResult::OriginalFileModified { id, original_path }) => {
                    tracing::warn!(
                        "Hot-exit restore skipped {}: original file {} changed on disk",
                        id,
                        original_path.display()
                    );
                }
                Ok(RecoveryResult::Corrupted { id, reason }) => {
                    tracing::warn!("Hot-exit restore skipped {}: corrupted ({})", id, reason);
                }
                Ok(RecoveryResult::NotFound { id }) => {
                    tracing::warn!("Hot-exit restore: recovery file {} missing", id);
                }
                Err(e) => {
                    tracing::warn!("Hot-exit restore: failed to load {}: {}", entry.id, e);
                }
            }
        }

        Ok(restored)
    }

    /// Perform auto-recovery-save for all modified buffers if needed.
    /// Called frequently (every frame); rate-limited by `auto_recovery_save_interval_secs`.
    ///
    /// Sweeps **every** open workspace, not just the active one. A buffer
    /// edited in one workspace and then left behind for another still holds
    /// unsaved content, and an active-window-only sweep left it with recovery
    /// data no newer than the last tick it was on screen for — so a crash
    /// (where, unlike a clean exit, there is no chance to flush) lost
    /// everything typed since (issue #3189). The sweep is close to free: a
    /// background window's buffers cannot become `recovery_pending` again
    /// without edits, and edits only happen while a window is active, so each
    /// one is written once after the user switches away and then skipped.
    ///
    /// The rate limit still reads the active window's clock — one editor-wide
    /// tick, not one per workspace — and every window's stamp is advanced with
    /// it so a later switch doesn't re-tick immediately on a stale timer.
    pub fn auto_recovery_save_dirty_buffers(&mut self) -> AnyhowResult<usize> {
        if !self.recovery_service.lock().unwrap().is_enabled() {
            return Ok(0);
        }

        let interval = std::time::Duration::from_secs(
            self.config.editor.auto_recovery_save_interval_secs as u64,
        );
        if self
            .time_source
            .elapsed_since(self.active_window().last_auto_recovery_save)
            < interval
        {
            return Ok(0);
        }

        let mut saved = 0;
        for window_id in self.window_ids_sorted() {
            saved += self.with_window_retargeted(window_id, |editor| {
                editor.save_pending_recovery_buffers()
            })?;
        }
        let now = self.time_source.now();
        for window in self.windows.values_mut() {
            window.last_auto_recovery_save = now;
        }
        Ok(saved)
    }

    /// Save all buffers marked `recovery_pending` to recovery storage.
    /// Shared by the periodic auto-save and the exit flush.
    fn save_pending_recovery_buffers(&mut self) -> AnyhowResult<usize> {
        if !self.recovery_service.lock().unwrap().is_enabled() {
            return Ok(0);
        }

        // Collect buffer IDs that need recovery (immutable pass).
        // Skip composite/hidden buffers — they are not real user content.
        let buffers_needing_recovery: Vec<_> = self
            .buffers()
            .iter()
            .filter_map(|(buffer_id, state)| {
                if state.is_composite_buffer {
                    return None;
                }
                if let Some(meta) = self.active_window().buffer_metadata.get(buffer_id) {
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
                .active_window()
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
                if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(buffer_id) {
                    meta.recovery_id = Some(new_id);
                }
            }
        }

        // Collect full buffer info with stable recovery IDs.
        let buffer_info: Vec<_> = buffers_needing_recovery
            .into_iter()
            .filter_map(|buffer_id| {
                let state = self
                    .windows
                    .get(&self.active_window)
                    .map(|w| &w.buffers)
                    .expect("active window present")
                    .get(&buffer_id)?;
                let meta = self.active_window().buffer_metadata.get(&buffer_id)?;
                let path = state.buffer.file_path().map(|p| p.to_path_buf());
                let recovery_id = if let Some(ref stored_id) = meta.recovery_id {
                    stored_id.clone()
                } else {
                    self.recovery_service
                        .lock()
                        .unwrap()
                        .get_buffer_id(path.as_deref())
                };
                let recovery_pending = state.buffer.is_recovery_pending();
                if self
                    .recovery_service
                    .lock()
                    .unwrap()
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
        if let Some(state) = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&self.active_buffer())
        {
            state.buffer.is_recovery_pending()
        } else {
            false
        }
    }

    /// Delete recovery for a buffer (call after saving or closing)
    pub fn delete_buffer_recovery(&mut self, buffer_id: BufferId) -> AnyhowResult<()> {
        // Get recovery_id: use stored one for unnamed buffers, compute from path otherwise
        let recovery_id = {
            let meta = self.active_window().buffer_metadata.get(&buffer_id);
            let state = self
                .windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .get(&buffer_id);

            if let Some(stored_id) = meta.and_then(|m| m.recovery_id.clone()) {
                stored_id
            } else if let Some(state) = state {
                let path = state.buffer.file_path().map(|p| p.to_path_buf());
                self.recovery_service
                    .lock()
                    .unwrap()
                    .get_buffer_id(path.as_deref())
            } else {
                return Ok(());
            }
        };

        // The deletes (exists + remove_file + a chunk-directory scan) are
        // disk I/O — off-loop. The recovery-service mutex is taken inside the
        // spawned effect so a slow disk stalls the runtime worker, not the
        // editor thread. Failure is logged, not returned: the stale recovery
        // file costs one spurious recovery prompt, never a hung close.
        let recovery_service = std::sync::Arc::clone(&self.recovery_service);
        self.spawn_off_loop_effect("delete_buffer_recovery", move || {
            let result = recovery_service
                .lock()
                .unwrap()
                .delete_buffer_recovery(&recovery_id);
            if let Err(e) = result {
                tracing::warn!("off-loop recovery delete failed: {}", e);
            }
        });

        // Clear recovery_pending since buffer is now saved
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
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
        // Read before the mutable borrow of the buffer state below: the entry
        // is stamped with the workspace that owns it (issue #3189).
        let workspace_id = self.active_window().stable_id.clone();
        let state = match self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(buffer_id)
        {
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
            self.recovery_service.lock().unwrap().save_buffer_owned(
                recovery_id,
                recovery_chunks,
                path,
                None,
                line_count,
                original_size,
                final_size,
                Some(&workspace_id),
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
            self.recovery_service.lock().unwrap().save_buffer_owned(
                recovery_id,
                chunks,
                path,
                None,
                line_count,
                0,
                total_bytes,
                Some(&workspace_id),
            )?;
        }

        state.buffer.set_recovery_pending(false);
        Ok(true)
    }
}
