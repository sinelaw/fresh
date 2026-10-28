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
use fresh_i18n::t;
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
    ///
    /// Clears out what interrupted in-place saves left in the top-level
    /// recovery directory and no longer need, whether or not recovery is on:
    /// saves stage there regardless (see
    /// [`crate::model::buffer::save::clean_up_inplace_write_recoveries`]).
    /// That directory is on this host, so it is swept through the local
    /// filesystem even when editing a remote one. What they kept that the
    /// user still has to decide about is then offered
    /// ([`Editor::offer_interrupted_save`]).
    pub fn start_recovery_session(&mut self) -> AnyhowResult<()> {
        let removed = crate::model::buffer::save::clean_up_inplace_write_recoveries(
            &*self.local_filesystem,
            &self.dir_context.recovery_dir(),
        );
        if removed > 0 {
            tracing::info!("Removed {removed} leftover in-place save file(s)");
        }
        self.offer_interrupted_save();
        Ok(())
    }

    /// End the recovery session cleanly (call on normal shutdown).
    ///
    /// Spans every workspace: exiting closes them all, and flushing only the
    /// active one is what let a quit from a clean workspace delete another's
    /// unsaved work (issue #3189).
    pub fn end_recovery_session(&mut self) -> AnyhowResult<()> {
        // Matters most for background workspaces: a buffer edited and
        // switched away from inside the auto-save interval has nothing on
        // disk at all until this runs.
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

        // Whatever is still modified now, the user never chose to discard:
        // the quit prompt's Discard clears the flags, so it is an exit that
        // asked nothing (Force Quit, a daemon shutting down, the GUI window
        // closing) or one hot exit let through. It is kept, `hot_exit` on or
        // off; what this editor held otherwise goes, and nothing it did not
        // hold is touched.
        let keep = self.recovery_ids_to_preserve();
        self.recovery_service.lock().unwrap().end_session(&keep);
        Ok(())
    }

    /// Flush a workspace's unsaved buffers before it is dropped, and let go
    /// of them: they stay in the store as that workspace's unsaved work, for
    /// whichever editor shows it next.
    ///
    /// `close_window` asks nothing about unsaved changes — that confirmation
    /// belongs to the Orchestrator UI driving the close, not to a function the
    /// plugin calls once the user has decided. This layer's job is only to
    /// leave the content somewhere recoverable (issue #3189).
    ///
    /// Done whatever `hot_exit` says: nobody chose to discard this work, and
    /// holding it would have the exit delete it.
    pub(crate) fn flush_window_recovery(&mut self, window_id: WindowId) -> AnyhowResult<usize> {
        let Some(window) = self.windows.get_mut(&window_id) else {
            return Ok(0);
        };
        for (_, state) in &mut window.buffers {
            if state.buffer.is_modified() {
                state.buffer.set_recovery_pending(true);
            }
        }
        let flushed = self
            .with_window_retargeted(window_id, |editor| editor.save_pending_recovery_buffers())?;
        let Some(window) = self.windows.get(&window_id) else {
            return Ok(flushed);
        };
        let mut service = self.recovery_service.lock().unwrap();
        for (buffer_id, meta) in window.buffer_metadata.iter() {
            let Some(id) = &meta.recovery_id else {
                continue;
            };
            let modified = window
                .buffers
                .get(buffer_id)
                .is_some_and(|state| state.buffer.is_modified());
            if modified {
                if let Err(e) = service.release(id) {
                    tracing::warn!("Failed to keep recovery entry {id} of a closed workspace: {e}");
                }
            } else if let Some(dir) = service.entry_dir(id) {
                if let Err(e) = crate::services::recovery::RecoveryService::delete_entry_dir(&dir) {
                    tracing::warn!("Failed to delete recovery entry {id}: {e}");
                }
            }
        }
        Ok(flushed)
    }

    /// The recovery ids of the buffers still holding unsaved work, across
    /// every open workspace: what a clean exit keeps.
    fn recovery_ids_to_preserve(&self) -> std::collections::HashSet<String> {
        let mut out = std::collections::HashSet::new();
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
                let is_unnamed = meta.file_path().is_none_or(|p| p.as_os_str().is_empty());
                if is_unnamed && state.buffer.total_bytes() == 0 {
                    continue;
                }
                if let Some(id) = &meta.recovery_id {
                    out.insert(id.clone());
                }
            }
        }
        out
    }

    /// Hold this editor's lock on every local file it has a tab on, and let
    /// go of those it has closed. A file another running editor has open is
    /// read-only here until that one lets it go, so no file ever has a
    /// second version to keep or to save over the first.
    /// Let go of every file lock now, for an editor about to be dropped
    /// while another is built in its place (the web UI's reset).
    pub fn release_file_locks(&mut self) {
        self.recovery_service
            .lock()
            .unwrap()
            .keep_file_locks(&std::collections::HashSet::new());
    }

    pub(crate) fn sync_file_locks(&mut self) {
        use crate::services::recovery::FileLock;
        let open: Vec<(WindowId, BufferId, PathBuf, bool)> = self
            .windows
            .iter()
            .filter(|(_, w)| w.host_key().is_none())
            .flat_map(|(id, w)| {
                w.file_buffers(true).map(move |(buffer_id, file)| {
                    let modified = w
                        .buffers
                        .get(&buffer_id)
                        .is_some_and(|state| state.buffer.is_modified());
                    (*id, buffer_id, file.to_path_buf(), modified)
                })
            })
            .collect();
        let locks: Vec<FileLock> = {
            let mut service = self.recovery_service.lock().unwrap();
            if !service.is_enabled() {
                return;
            }
            let paths: std::collections::HashSet<PathBuf> =
                open.iter().map(|(_, _, path, _)| path.clone()).collect();
            service.keep_file_locks(&paths);
            open.iter()
                .map(|(_, _, path, _)| service.lock_file(path))
                .collect()
        };
        // Files this editor just got: the editor that let them go may have
        // kept unsaved work on them, which comes here now rather than wait
        let mut released: Vec<(WindowId, PathBuf)> = Vec::new();
        for ((window_id, buffer_id, path, modified), lock) in open.into_iter().zip(locks) {
            let Some(window) = self.windows.get_mut(&window_id) else {
                continue;
            };
            let Some(meta) = window.buffer_metadata.get_mut(&buffer_id) else {
                continue;
            };
            let name = path
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();
            let message = match (lock, meta.open_elsewhere) {
                // This editor's own unsaved work is never locked away from it
                (FileLock::Elsewhere { pid }, None) if !modified => {
                    meta.open_elsewhere = Some(meta.read_only);
                    window.mark_buffer_read_only(buffer_id, true);
                    let pid = pid.map_or_else(|| "?".to_string(), |pid| pid.to_string());
                    t!("buffer.open_in_other_editor", file = name, pid = pid)
                }
                (FileLock::Ours, Some(was_read_only)) => {
                    meta.open_elsewhere = None;
                    window.mark_buffer_read_only(buffer_id, was_read_only);
                    released.push((window_id, path));
                    t!("buffer.open_in_other_editor_released", file = name)
                }
                _ => continue,
            };
            self.set_status_message(message.to_string());
        }
        if !self.adopting_recovery {
            for (window_id, path) in released {
                self.adopt_recovery_for_window(
                    window_id,
                    Some(std::slice::from_ref(&path)),
                    AdoptScope::OpenFiles,
                );
            }
        }
    }

    /// The ids of this editor's windows on the active window's root.
    fn open_workspace_ids_for_active(&self) -> std::collections::HashSet<String> {
        use crate::app::orchestrator_persistence::canonical_key;
        let key = canonical_key(&self.active_window().root);
        self.windows
            .values()
            .filter(|w| canonical_key(&w.root) == key)
            .map(|w| w.stable_id.clone())
            .collect()
    }

    /// Restore into window `id` the unsaved work in the store no running
    /// editor holds that is its to show (see [`AdoptScope`]): what a clean
    /// exit kept, what a crash left, what a launch of any kind left.
    ///
    /// An entry is its buffer's content and nothing about a layout. A file's
    /// goes to whichever window has the file open — one at most, as a file
    /// has one buffer — and an unnamed buffer's to the workspace whose layout
    /// places it ([`Window::restore_unnamed_buffers`]). What neither claims
    /// is offered in a window on the root it was last edited under.
    ///
    /// `only` limits it to entries for those files.
    ///
    /// A window in the background gets its content only into the file
    /// buffers it already has; the rest waits for it to be switched to,
    /// since opening tabs there would have nowhere to show them.
    ///
    /// [`Window::restore_unnamed_buffers`]: crate::app::window::Window
    pub(crate) fn adopt_recovery_for_window(
        &mut self,
        id: WindowId,
        only: Option<&[PathBuf]>,
        scope: AdoptScope,
    ) -> usize {
        let foreground = id == self.active_window;
        self.with_window_retargeted(id, |editor| {
            editor.adopt_recovery_for_active_window(only, foreground, scope)
        })
    }

    fn adopt_recovery_for_active_window(
        &mut self,
        only: Option<&[PathBuf]>,
        open_tabs: bool,
        scope: AdoptScope,
    ) -> usize {
        use crate::services::recovery::RecoveryService;
        use std::collections::HashSet;

        if !self.recovery_service.lock().unwrap().is_enabled() {
            return 0;
        }
        let window = self.active_window();
        let root = window.root.clone();
        let canonical_root = root.canonicalize().unwrap_or_else(|_| root.clone());
        // A file another editor has open is that editor's to restore into
        let open_here: HashSet<PathBuf> = window
            .tab_file_buffers()
            .filter(|(buffer_id, _)| {
                window
                    .buffer_metadata
                    .get(buffer_id)
                    .is_none_or(|meta| meta.open_elsewhere.is_none())
            })
            .map(|(_, file)| file.to_path_buf())
            .collect();
        let host = window.host_key();
        let open_elsewhere: HashSet<PathBuf> = self
            .windows
            .iter()
            .filter(|(wid, w)| **wid != self.active_window && w.host_key() == host)
            .flat_map(|(_, w)| w.tab_file_buffers().map(|(_, file)| file.to_path_buf()))
            .collect();
        // The unnamed buffers other saved workspaces on this root place:
        // theirs to come back with
        let placed_elsewhere = match scope {
            AdoptScope::Workspace => {
                crate::workspace::unnamed_refs_for_root(&root, &window.stable_id)
            }
            AdoptScope::Root | AdoptScope::OpenFiles => HashSet::new(),
        };
        // Only work on this window's host: the same path elsewhere is
        // another file. A remote window not connected yet matches none.
        let known =
            RecoveryService::known_workspace_ids(&root, self.open_workspace_ids_for_active());
        let entries: Vec<_> = self
            .recovery_service
            .lock()
            .unwrap()
            .list_recoverable(Some((&root, &known)), Some(host.as_deref()))
            .into_iter()
            .filter(|entry| {
                let on_this_root = entry.metadata.root.as_deref() == Some(canonical_root.as_path());
                match entry.metadata.original_path.as_deref() {
                    Some(path) => {
                        if !only.is_none_or(|paths| paths.iter().any(|p| p == path)) {
                            false
                        } else if open_here.contains(path) {
                            true
                        } else if open_elsewhere.contains(path) {
                            // That window's to take: switching to it does
                            false
                        } else {
                            scope != AdoptScope::OpenFiles && on_this_root
                        }
                    }
                    None => match scope {
                        AdoptScope::OpenFiles => false,
                        AdoptScope::Root => on_this_root,
                        AdoptScope::Workspace => {
                            on_this_root && !placed_elsewhere.contains(&entry.id)
                        }
                    },
                }
            })
            .collect();

        let mut adopted = 0;
        let nested = std::mem::replace(&mut self.adopting_recovery, true);
        for entry in entries {
            if self.adopt_recovery_entry(entry, open_tabs) {
                adopted += 1;
            }
        }
        self.adopting_recovery = nested;
        if adopted > 0 {
            tracing::info!(
                "Restored {adopted} unsaved buffer(s) into workspace {}",
                self.active_window
            );
        }
        adopted
    }

    /// Restore one entry into the active window. Returns whether it did.
    ///
    /// A file's entry goes into the file's buffer when that holds nothing
    /// of its own: one opened clean, or opened here for it. Otherwise — the
    /// buffer already has unsaved work (a second version of the file, from
    /// another editor or launch), or the file changed on disk since — it
    /// comes back as an unsaved copy beside it rather than over anything.
    fn adopt_recovery_entry(
        &mut self,
        entry: crate::services::recovery::RecoveryEntry,
        open_tabs: bool,
    ) -> bool {
        use crate::services::recovery::RecoveryResult;

        let target = match entry.metadata.original_path.as_deref() {
            Some(path) => match self.buffer_for_path(path) {
                Some(buffer_id) if self.can_take_recovered_content(buffer_id) => {
                    Target::File(Some(buffer_id))
                }
                Some(_) => Target::Copy,
                None => Target::File(None),
            },
            None => Target::Unnamed,
        };
        if !open_tabs && !matches!(target, Target::File(Some(_))) {
            return false;
        }
        // Opening it here would go to the workspace that has it — never
        // restore through that into a buffer this window doesn't own
        if let (Target::File(None), Some(path)) = (&target, entry.metadata.original_path.as_deref())
        {
            if self.file_open_in_other_window(path).is_some() {
                return false;
            }
        }

        let claimed = self.recovery_service.lock().unwrap().claim(&entry);
        let entry = match claimed {
            Ok(Some(entry)) => entry,
            Ok(None) => return false,
            Err(e) => {
                tracing::warn!("Failed to take over recovery entry {}: {}", entry.id, e);
                return false;
            }
        };
        let loaded = self.recovery_service.lock().unwrap().load_recovery(&entry);
        let (content, target) = match (loaded, target) {
            (Ok(RecoveryResult::Recovered { content, .. }), target) => {
                (Recovered::Whole(content), target)
            }
            (Ok(RecoveryResult::RecoveredChunks { chunks, .. }), Target::File(buffer)) => {
                (Recovered::Chunks(chunks), Target::File(buffer))
            }
            // A short read must never replace a file's contents: the next
            // save would write the truncation over the real file. With no
            // file it is the only copy, so it comes back — loudly.
            (
                Ok(RecoveryResult::RecoveredPartial {
                    content,
                    expected,
                    found,
                    original_path,
                }),
                target,
            ) => {
                if original_path.is_some() {
                    tracing::warn!(
                        "Not restoring {}: it holds {found} of {expected} bytes, so the \
                         file on disk is the better copy",
                        entry.id
                    );
                    self.give_back_recovery_entry(&entry);
                    return false;
                }
                self.set_status_message(format!(
                    "Recovered an unsaved buffer incompletely: {found} of {expected} \
                     bytes. The end is missing — check before saving."
                ));
                (Recovered::Whole(content), target)
            }
            // The file changed since: its unsaved version comes back beside
            // it, never over the newer file.
            (Ok(RecoveryResult::OriginalFileModified { original_path, .. }), _)
                if entry.metadata.original_file_size == 0 =>
            {
                let mut unpinned = entry.clone();
                unpinned.metadata.original_path = None;
                unpinned.metadata.original_mtime = None;
                let loaded = self
                    .recovery_service
                    .lock()
                    .unwrap()
                    .load_recovery(&unpinned);
                let Ok(RecoveryResult::Recovered { content, .. }) = loaded else {
                    self.give_back_recovery_entry(&entry);
                    return false;
                };
                tracing::info!(
                    "{} changed on disk since its unsaved changes were kept; \
                     restoring them as a copy",
                    original_path.display()
                );
                self.set_status_message(format!(
                    "{} changed on disk; its unsaved changes are in a copy beside it",
                    original_path.display()
                ));
                (Recovered::Whole(content), Target::Copy)
            }
            (Ok(other), _) => {
                // Deltas against a file that changed, or an entry that can't
                // be read: kept for a later look, and said out loud.
                tracing::warn!("Unsaved work in {} not restored: {other:?}", entry.id);
                if let RecoveryResult::OriginalFileModified { original_path, .. } = other {
                    self.set_status_message(format!(
                        "{} changed on disk; its unsaved changes were kept but not restored",
                        original_path.display()
                    ));
                }
                self.give_back_recovery_entry(&entry);
                return false;
            }
            (Err(e), _) => {
                tracing::warn!("Failed to load recovery entry {}: {}", entry.id, e);
                self.give_back_recovery_entry(&entry);
                return false;
            }
        };

        let buffer_id = match target {
            Target::File(Some(buffer_id)) => buffer_id,
            Target::File(None) => {
                let path = entry.metadata.original_path.clone().unwrap_or_default();
                match self.open_file(&path) {
                    // Opened, but onto a buffer that isn't free to take it:
                    // one this window already had under another spelling of
                    // the path, or one another editor has (read-only here).
                    // Never restored over; kept for a later look.
                    Ok(buffer_id) if !self.can_take_recovered_content(buffer_id) => {
                        self.give_back_recovery_entry(&entry);
                        return false;
                    }
                    Ok(buffer_id) => buffer_id,
                    Err(e) => {
                        if let Some(confirmation) =
                            e.downcast_ref::<crate::model::buffer::LargeFileEncodingConfirmation>()
                        {
                            self.start_large_file_encoding_confirmation(confirmation);
                        } else {
                            tracing::warn!(
                                "Failed to open {} to restore it: {}",
                                path.display(),
                                e
                            );
                        }
                        self.give_back_recovery_entry(&entry);
                        return false;
                    }
                }
            }
            Target::Unnamed => self.buffer_for_recovered_unnamed(),
            Target::Copy => {
                let buffer_id = self.new_buffer();
                let name = entry
                    .metadata
                    .original_path
                    .as_deref()
                    .and_then(|p| p.file_name())
                    .map(|n| n.to_string_lossy().into_owned())
                    .unwrap_or_default();
                if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id) {
                    meta.display_name = format!("{name} (unsaved copy)");
                }
                buffer_id
            }
        };
        self.fill_from_recovery(buffer_id, content, &entry);
        if matches!(target, Target::Copy) {
            // Re-recorded as the unnamed buffer it now is, so it never comes
            // back over the file it was copied from.
            if let Some(state) = self.buffers_mut().get_mut(&buffer_id) {
                state.buffer.set_recovery_pending(true);
            }
        }
        true
    }

    /// The active window's buffer for file `path`.
    fn buffer_for_path(&self, path: &std::path::Path) -> Option<BufferId> {
        self.buffers()
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id)
    }

    /// Whether `buffer_id` has nothing of its own a restore would overwrite:
    /// no unsaved changes, no entry of its own, not another editor's file
    /// (read-only here), and a tab to show the restored work in — a
    /// plugin's hidden buffer has none, and the exit doesn't keep its work.
    fn can_take_recovered_content(&self, buffer_id: BufferId) -> bool {
        let clean = self
            .buffers()
            .get(&buffer_id)
            .is_some_and(|state| !state.buffer.is_modified());
        let free = self
            .active_window()
            .buffer_metadata
            .get(&buffer_id)
            .is_none_or(|meta| {
                meta.recovery_id.is_none()
                    && meta.open_elsewhere.is_none()
                    && !meta.hidden_from_tabs
            });
        clean && free
    }

    /// Put an entry this editor took but did not restore back as unsaved
    /// work no editor holds.
    fn give_back_recovery_entry(&mut self, entry: &crate::services::recovery::RecoveryEntry) {
        if let Err(e) = self.recovery_service.lock().unwrap().give_back(&entry.id) {
            tracing::warn!("Failed to put back recovery entry {}: {}", entry.id, e);
        }
    }

    /// Replace `buffer_id`'s content with what `entry` kept, and make the
    /// buffer the one that entry backs.
    fn fill_from_recovery(
        &mut self,
        buffer_id: BufferId,
        content: Recovered,
        entry: &crate::services::recovery::RecoveryEntry,
    ) {
        let Some(state) = self.buffers_mut().get_mut(&buffer_id) else {
            return;
        };
        match content {
            Recovered::Whole(bytes) => {
                let text = String::from_utf8_lossy(&bytes).into_owned();
                let total = state.buffer.total_bytes();
                state.buffer.delete(0..total);
                state.buffer.insert(0, &text);
            }
            Recovered::Chunks(chunks) => {
                // The entry holds deltas against what is on disk: replay
                // them later offsets first, so earlier ones don't shift them.
                // As bytes: a chunk carries the file's own bytes between
                // nearby edits, which need not be UTF-8.
                for chunk in chunks.into_iter().rev() {
                    if chunk.original_len > 0 {
                        state
                            .buffer
                            .delete(chunk.offset..chunk.offset + chunk.original_len);
                    }
                    state.buffer.insert_bytes(chunk.offset, chunk.content);
                }
            }
        }
        state.buffer.set_modified(true);
        state.buffer.set_backed_by(entry.handle());
        // Wholesale replacement, never described as edit damage.
        // See `WrapIndex::damage_all`.
        state.wrap_indices.damage_all();
        if let Some(log) = self.active_window_mut().event_logs.get_mut(&buffer_id) {
            log.clear_saved_position();
        }
        if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id) {
            meta.recovery_id = Some(entry.id.clone());
        }
        self.sync_lsp_after_recovery_replay(buffer_id);
    }

    /// The buffer an unnamed buffer's recovered content goes into: the
    /// launch's own empty scratch buffer while it is the active one and
    /// untouched, the way opening a file takes it over, else a new one.
    /// A new one beside it left an extra empty "[No Name]" tab after every
    /// restore (issue #3401).
    fn buffer_for_recovered_unnamed(&mut self) -> BufferId {
        let active = self.active_buffer();
        if self.active_window().is_pristine_scratch(active) {
            active
        } else {
            self.new_buffer()
        }
    }

    /// Restore the hot-exit content into the active window when the session
    /// restore is opted out (`--no-restore`, or
    /// `editor.restore_previous_session = false`), so the user does not lose
    /// in-progress work just because they asked to skip restoring the
    /// workspace layout. Returns the number of buffers restored.
    pub fn try_restore_hot_exit_buffers(&mut self) -> AnyhowResult<usize> {
        Ok(self.adopt_recovery_for_window(self.active_window, None, AdoptScope::Root))
    }

    /// Perform auto-recovery-save for all modified buffers if needed.
    /// Called frequently (every frame); rate-limited by `auto_recovery_save_interval_secs`.
    ///
    /// Sweeps every workspace. An active-window-only sweep left a
    /// backgrounded buffer with recovery data no newer than the last tick it
    /// was on screen for, so a crash — which, unlike a clean exit, gets no
    /// chance to flush — lost everything typed since (issue #3189).
    ///
    /// Close to free: a background window's buffers cannot go
    /// `recovery_pending` again without edits, and edits only happen while
    /// active, so each is written once after the switch and then skipped.
    ///
    /// One editor-wide tick off the active window's clock, with every window's
    /// stamp advanced together so a later switch doesn't re-tick on a stale
    /// timer.
    pub fn auto_recovery_save_dirty_buffers(&mut self) -> AnyhowResult<usize> {
        if !self.recovery_service.lock().unwrap().is_enabled() {
            return Ok(0);
        }

        // On a clock of their own: an editor waiting for another to let a
        // file go shouldn't wait as long as the backup interval
        const LOCK_SYNC_INTERVAL: std::time::Duration = std::time::Duration::from_secs(2);
        let now = self.time_source.now();
        if self
            .last_lock_sync
            .is_none_or(|last| now.saturating_duration_since(last) >= LOCK_SYNC_INTERVAL)
        {
            self.last_lock_sync = Some(now);
            self.sync_file_locks();
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

    /// Save all buffers marked `recovery_pending` to recovery storage, and
    /// delete the entries of buffers with no unsaved work left (undone back
    /// to the saved state, reverted), which a crash would otherwise bring
    /// back as unsaved. Shared by the periodic auto-save and the exit flush.
    fn save_pending_recovery_buffers(&mut self) -> AnyhowResult<usize> {
        if !self.recovery_service.lock().unwrap().is_enabled() {
            return Ok(0);
        }

        // Skip composite/hidden buffers — they are not real user content.
        let mut pending = Vec::new();
        let mut resolved = Vec::new();
        for (buffer_id, state) in self.buffers().iter() {
            if state.is_composite_buffer {
                continue;
            }
            let Some(meta) = self.active_window().buffer_metadata.get(buffer_id) else {
                continue;
            };
            if meta.hidden_from_tabs || meta.is_virtual() {
                continue;
            }
            if !state.buffer.is_modified() {
                if meta.recovery_id.is_some() {
                    resolved.push(*buffer_id);
                }
            } else if state.buffer.is_recovery_pending() {
                pending.push(*buffer_id);
            }
        }
        for buffer_id in resolved {
            self.delete_buffer_recovery(buffer_id)?;
        }

        let mut saved_count = 0;
        for buffer_id in pending {
            let window = self.active_window_mut();
            let Some(meta) = window.buffer_metadata.get_mut(&buffer_id) else {
                continue;
            };
            let recovery_id = meta
                .recovery_id
                .get_or_insert_with(crate::services::recovery::RecoveryService::new_entry_id)
                .clone();
            if self.save_buffer_to_recovery(&buffer_id, &recovery_id)? {
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

    /// Delete recovery for a buffer (call after saving or closing), and
    /// retire its id: the next backup of this buffer starts a new entry, so
    /// the delete — which runs off the main loop — can never land on it.
    pub fn delete_buffer_recovery(&mut self, buffer_id: BufferId) -> AnyhowResult<()> {
        let window = self.active_window_mut();
        let recovery_id = window
            .buffer_metadata
            .get_mut(&buffer_id)
            .and_then(|meta| meta.recovery_id.take());
        if let Some(state) = window.buffers.get_mut(&buffer_id) {
            state.buffer.set_recovery_pending(false);
            state.buffer.clear_hot_exit_entry();
        }
        let Some(recovery_id) = recovery_id else {
            return Ok(());
        };
        let dir = self
            .recovery_service
            .lock()
            .unwrap()
            .entry_dir(&recovery_id);
        if let Some(dir) = dir {
            self.delete_recovery_entry_dir(dir);
        }
        Ok(())
    }

    /// Delete the entry this editor keeps in `dir`.
    fn delete_recovery_entry_dir(&mut self, dir: PathBuf) {
        // The delete is disk I/O — off-loop. Failure is logged, not
        // returned: the stale recovery file costs one spurious restore,
        // never a hung close.
        self.spawn_off_loop_effect("delete_buffer_recovery", move || {
            if let Err(e) = crate::services::recovery::RecoveryService::delete_entry_dir(&dir) {
                tracing::warn!("off-loop recovery delete failed: {}", e);
            }
        });
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
    ) -> AnyhowResult<bool> {
        // Read before the mutable borrow of the buffer state below: the root
        // the buffer is last edited under (see `RecoveryMetadata::root`)
        let root = self.active_window().root.clone();
        // The file as a buffer opened on it is keyed, so whoever opens it
        // finds this, however this buffer came to name it (a Save As through
        // a symlink, say)
        let path = self
            .buffers()
            .get(buffer_id)
            .and_then(|state| state.buffer.file_path())
            .filter(|p| !p.as_os_str().is_empty())
            .map(|p| self.active_window().resolve_open_path(p).canonical_path);
        let recovery_service = std::sync::Arc::clone(&self.recovery_service);
        let state = match self.buffers_mut().get_mut(buffer_id) {
            Some(s) => s,
            None => return Ok(false),
        };
        let host = state
            .buffer
            .filesystem()
            .remote_connection_info()
            .map(str::to_owned);
        let line_count = state.buffer.line_count();

        let (chunks, original_size, final_size) = if state.buffer.is_large_file() {
            let chunks = match state.buffer.get_recovery_chunks() {
                Ok(chunks) => chunks,
                Err(e) => {
                    tracing::warn!("Failed to read the changes for a recovery save: {}", e);
                    return Ok(false);
                }
            };
            if chunks.is_empty() {
                state.buffer.set_recovery_pending(false);
                return Ok(false);
            }
            let original_size = state.buffer.original_file_size().unwrap_or(0);
            (chunks, original_size, state.buffer.total_bytes())
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
            (chunks, 0, total_bytes)
        };
        let handle = recovery_service.lock().unwrap().save_buffer(
            &root,
            recovery_id,
            chunks,
            path.as_deref(),
            None,
            line_count,
            original_size,
            final_size,
            host.as_deref(),
        )?;
        match handle {
            Some(handle) => state.buffer.set_backed_by(handle),
            None => state.buffer.set_recovery_pending(false),
        }
        Ok(true)
    }

    /// Offer the user a copy an interrupted in-place save kept (issue
    /// #3383), the oldest first: the save may have left its file torn, and
    /// the copy may be the only intact version of what was being saved.
    /// Asked with a dialog — restore the file from the copy, show the
    /// difference (closing the dialog so the diff can be read), discard the
    /// copy, or decide later. A copy still waiting is asked about again by
    /// the "Review Interrupted Saves" command, and at the next session start.
    /// Called when a session starts, and again after each decision
    /// until none is left. Leaves a prompt that is already up alone.
    ///
    /// Like the cleanup, this reads the top-level recovery directory through
    /// the local filesystem: in-place saves only happen to local files.
    pub(crate) fn offer_interrupted_save(&mut self) {
        if self.active_window().prompt.is_none() {
            self.prompt_next_interrupted_save();
        }
    }

    /// The "Review Interrupted Saves" command: ask again about a copy an
    /// interrupted save kept that is still waiting for a decision (after
    /// "Later", or after reading its diff), or say that none is.
    pub(crate) fn review_interrupted_saves(&mut self) {
        if !self.prompt_next_interrupted_save() {
            self.set_status_message(fresh_i18n::t!("interrupted_save.none_waiting").into_owned());
        }
    }

    /// Open the dialog for the oldest kept copy, if there is one.
    fn prompt_next_interrupted_save(&mut self) -> bool {
        let recovery_dir = self.dir_context.recovery_dir();
        if let Some(recovery) = crate::model::buffer::save::kept_inplace_write_recoveries(
            &*self.local_filesystem,
            &recovery_dir,
        )
        .into_iter()
        .next()
        {
            self.prompt_interrupted_save(recovery.dest_path, recovery.temp_path);
            return true;
        }
        false
    }

    fn prompt_interrupted_save(&mut self, dest_path: PathBuf, copy_path: PathBuf) {
        use crate::view::confirm::{Choice, Confirm, Tone};
        use fresh_i18n::t;

        let name = display_name(&dest_path);
        let body = t!("interrupted_save.body", name = &name).into_owned();
        let mut choices = vec![Choice::new(
            t!("dialog.btn.restore").into_owned(),
            "restore",
            Tone::Destructive,
        )];
        let can_diff = self.can_diff_interrupted_save(&dest_path, &copy_path);
        if can_diff {
            choices.push(Choice::new(
                t!("dialog.btn.show_diff").into_owned(),
                "diff",
                Tone::Safe,
            ));
        }
        choices.push(Choice::new(
            t!("dialog.btn.discard").into_owned(),
            "discard",
            Tone::Destructive,
        ));
        choices.push(Choice::new(
            t!("dialog.btn.later").into_owned(),
            "",
            Tone::Safe,
        ));
        // Open on a choice that changes nothing.
        let initial = if can_diff { 1 } else { choices.len() - 1 };
        let confirm = Confirm::new(
            t!("dialog.title.interrupted_save").into_owned(),
            body.clone(),
            choices,
        )
        .detail(dest_path.display().to_string())
        .selecting(initial);
        self.start_confirm_prompt(
            body,
            crate::view::prompt::PromptType::ConfirmInterruptedSave { dest_path },
            confirm,
        );
    }

    /// Whether the difference between a file and the copy an interrupted
    /// save kept for it is small enough to show: both are read whole.
    fn can_diff_interrupted_save(
        &self,
        dest_path: &std::path::Path,
        copy_path: &std::path::Path,
    ) -> bool {
        let limit = self.config.editor.large_file_threshold_bytes;
        [dest_path, copy_path].iter().all(|path| {
            self.local_filesystem
                .metadata(path)
                .is_ok_and(|meta| meta.size <= limit)
        })
    }

    /// The user's answer to [`Editor::offer_interrupted_save`] for
    /// `dest_path`. A decision removes the copy and its metadata (or, if
    /// restoring fails, keeps them and says why) and moves on to the next
    /// copy; "diff" and "later" leave them for the "Review Interrupted Saves"
    /// command or the next session.
    pub(crate) fn handle_interrupted_save_choice(&mut self, dest_path: PathBuf, input: &str) {
        use fresh_i18n::t;

        let recovery_dir = self.dir_context.recovery_dir();
        let fs = std::sync::Arc::clone(&self.local_filesystem);
        let name = display_name(&dest_path);
        match input {
            "restore" => {
                match crate::model::buffer::save::restore_inplace_write_recovery(
                    &*fs,
                    &recovery_dir,
                    &dest_path,
                ) {
                    Ok(()) => {
                        self.reload_buffers_of_restored_file(&dest_path);
                        self.set_status_message(
                            t!("interrupted_save.restored", name = &name).into_owned(),
                        );
                        self.offer_interrupted_save();
                    }
                    Err(e) => {
                        tracing::warn!("Failed to restore {}: {}", dest_path.display(), e);
                        self.set_status_message(
                            t!(
                                "interrupted_save.restore_failed",
                                name = &name,
                                error = e.to_string()
                            )
                            .into_owned(),
                        );
                    }
                }
            }
            "diff" => {
                let Some(recovery) =
                    crate::model::buffer::save::kept_inplace_write_recoveries(&*fs, &recovery_dir)
                        .into_iter()
                        .find(|recovery| recovery.dest_path == dest_path)
                else {
                    return;
                };
                // The dialog closes so the diff can be read and scrolled;
                // the copy stays until the user decides.
                self.show_interrupted_save_diff(&dest_path, &recovery.temp_path);
                self.set_status_message(t!("interrupted_save.diff_shown").into_owned());
            }
            "discard" => {
                crate::model::buffer::save::resolve_inplace_write_recovery(
                    &*fs,
                    &recovery_dir,
                    &dest_path,
                );
                self.set_status_message(
                    t!("interrupted_save.discarded", name = &name).into_owned(),
                );
                self.offer_interrupted_save();
            }
            _ => {
                let copy =
                    crate::services::recovery::InplaceWriteRecovery::scan(&*fs, &recovery_dir)
                        .into_iter()
                        .find(|(_, recovery)| recovery.dest_path == dest_path)
                        .map(|(_, recovery)| recovery.temp_path.display().to_string())
                        .unwrap_or_default();
                self.set_status_message(
                    t!("interrupted_save.later", name = &name, path = &copy).into_owned(),
                );
            }
        }
    }

    /// `path` was just overwritten from the copy an interrupted save kept,
    /// behind the back of the buffers that have it open (a session restore
    /// opens them before the dialog is answered); a large one reads the
    /// parts it never loaded from the file, at offsets that no longer hold
    /// them. Reload every one that has no unsaved changes, in every window.
    /// One that has keeps them: the file's mtime no longer matches the one
    /// recorded for it, so it counts as changed on disk, and saving it asks
    /// first.
    fn reload_buffers_of_restored_file(&mut self, path: &std::path::Path) {
        // The kept copy's metadata may name the file by another spelling
        // than the buffer does (a symlinked directory, e.g. macOS's
        // /var -> /private/var), so match on the resolved path.
        let local_fs = std::sync::Arc::clone(&self.local_filesystem);
        let resolve =
            |p: &std::path::Path| local_fs.canonicalize(p).unwrap_or_else(|_| p.to_path_buf());
        let target = resolve(path);
        let active_window = self.active_window;
        for window_id in self.window_ids_sorted() {
            self.with_window_retargeted(window_id, |editor| {
                // Restored through the local filesystem: a remote buffer
                // with the same path is another file
                let unmodified: Vec<(BufferId, std::path::PathBuf)> = editor
                    .buffers()
                    .iter()
                    .filter(|(_, state)| {
                        state.buffer.filesystem().remote_connection_info().is_none()
                            && !state.buffer.is_modified()
                    })
                    .filter_map(|(id, state)| {
                        let own = state.buffer.file_path()?;
                        (resolve(own) == target).then(|| (*id, own.to_path_buf()))
                    })
                    .collect();
                for (buffer_id, own_path) in unmodified {
                    // The active buffer's reload keeps its view where it is
                    let reloaded =
                        if window_id == active_window && buffer_id == editor.active_buffer() {
                            editor.revert_file().map(|_| ())
                        } else {
                            editor.revert_buffer_by_id(buffer_id, &own_path)
                        };
                    if let Err(e) = reloaded {
                        tracing::warn!("Failed to reload restored {}: {e}", path.display());
                    }
                }
            });
        }
    }

    /// Show, side by side, `dest_path` as it is on disk and the copy an
    /// interrupted save kept for it, in a tab of their own.
    fn show_interrupted_save_diff(
        &mut self,
        dest_path: &std::path::Path,
        copy_path: &std::path::Path,
    ) {
        use crate::model::composite_buffer::{
            CompositeLayout, DiffHunk, LineAlignment, PaneStyle, SourcePane,
        };
        use crate::primitives::text_property::TextPropertyEntry;
        use fresh_i18n::t;

        let read = |path: &std::path::Path| {
            self.local_filesystem
                .read_file(path)
                .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
        };
        let (on_disk, kept) = match (read(dest_path), read(copy_path)) {
            (Ok(on_disk), Ok(kept)) => (on_disk, kept),
            (Err(e), _) | (_, Err(e)) => {
                tracing::warn!(
                    "Can't show the interrupted save of {}: {}",
                    dest_path.display(),
                    e
                );
                return;
            }
        };
        const MODE: &str = "interrupted-save-diff";
        let on_disk_label = t!("interrupted_save.on_disk").into_owned();
        let kept_label = t!("interrupted_save.kept_copy").into_owned();
        let mut pane = |label: &str, content: &str| {
            // Shown only through the composite, not as tabs of their own.
            let window = self.active_window_mut();
            let id =
                window.create_virtual_buffer_detached(label.to_string(), MODE.to_string(), true);
            if let Some(meta) = window.buffer_metadata.get_mut(&id) {
                meta.hidden_from_tabs = true;
            }
            if let Err(e) =
                self.set_virtual_buffer_content(id, vec![TextPropertyEntry::text(content)])
            {
                tracing::warn!("Failed to fill the interrupted-save diff: {e}");
            }
            id
        };
        let old_id = pane(&on_disk_label, &on_disk);
        let new_id = pane(&kept_label, &kept);
        let sources = vec![
            SourcePane::new(old_id, on_disk_label, false).with_style(PaneStyle::old_diff()),
            SourcePane::new(new_id, kept_label, false).with_style(PaneStyle::new_diff()),
        ];
        let layout = CompositeLayout::SideBySide {
            ratios: vec![0.5, 0.5],
            show_separator: true,
        };
        let name = t!("interrupted_save.diff_tab", name = display_name(dest_path)).into_owned();
        let composite_id = self.create_composite_buffer(name, MODE.to_string(), layout, sources);

        let hunks: Vec<DiffHunk> = fresh_core::diff::compute_line_diff(&on_disk, &kept)
            .into_iter()
            .map(|h| {
                DiffHunk::new(
                    h.old_start as usize,
                    h.old_count as usize,
                    h.new_start as usize,
                    h.new_count as usize,
                )
            })
            .collect();
        let has_hunks = !hunks.is_empty();
        let alignment = LineAlignment::from_hunks(
            &hunks,
            on_disk.split_inclusive('\n').count(),
            kept.split_inclusive('\n').count(),
        );
        let window = self.active_window_mut();
        window.set_composite_alignment(composite_id, alignment);
        if has_hunks {
            if let Some(composite) = window.get_composite_mut(composite_id) {
                composite.initial_focus_hunk = Some(0);
            }
        }
        self.switch_buffer(composite_id);
    }
}

/// Which unsaved work in the store a window takes. Whatever the scope, it
/// takes the work for the files it has open, and never for a file another
/// window here has open (that window's, on a switch to it).
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum AdoptScope {
    /// A workspace restored or switched to: also the work last edited on its
    /// root that nothing else claims — files no window has open, and unnamed
    /// buffers no other saved workspace's layout places.
    Workspace,
    /// The startup of a launch that restores no workspace (`--no-restore`),
    /// which stands in for all of them: all the work last edited on its root.
    Root,
    /// Only the files it has open: a file launch (`fresh FILE`) is about the
    /// files it names, and a file opened during a session takes its own.
    OpenFiles,
}

/// Where an entry being restored goes.
enum Target {
    /// The file's buffer: this one, or one opened for it.
    File(Option<BufferId>),
    /// A new unnamed buffer, or the launch's empty scratch buffer.
    Unnamed,
    /// An unnamed buffer beside the file's, holding this unsaved version of it.
    Copy,
}

/// What a restored buffer gets from its entry.
enum Recovered {
    /// The whole content.
    Whole(Vec<u8>),
    /// Changes to apply over the file on disk (a large file).
    Chunks(Vec<crate::services::recovery::types::RecoveryChunk>),
}

/// A file's name, for messages about it.
fn display_name(path: &std::path::Path) -> String {
    path.file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.display().to_string())
}
