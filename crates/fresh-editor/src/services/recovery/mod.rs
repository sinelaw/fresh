//! File Recovery Service
//!
//! Emacs-style recovery for the Fresh editor: modified buffers are written
//! to the recovery store every few seconds, kept there over a clean exit
//! (hot exit), and brought back the next time their workspace is shown —
//! whichever way the editor is launched. A crash leaves them there too.
//!
//! ## How it works
//!
//! 1. **One store per project root** (see [`store`]), shared by every launch
//!    mode, so any editor showing a workspace finds its unsaved work.
//! 2. **Each running editor holds a lock** and writes only under its own
//!    directory, so other editors leave its live buffers alone, and what it
//!    held becomes recoverable the moment it stops, however it stops.
//! 3. **Each buffer has its own entry**, with a random id and the file it
//!    is for recorded inside, so two buffers on one file never share one.
//! 4. **A save records itself in the entry** before writing the file
//!    ([`types::SaveIntent`]), so a crash on either side of the write is
//!    settled with one `stat`.
//!
//! ## Storage Format
//!
//! All recovery data uses a chunked format:
//! - For small files/new buffers: single chunk containing full content
//! - For large files: only modified regions stored as chunks

mod storage;
mod store;
// The on-disk record types live in `fresh-editor-core`: `model::buffer::save`
// writes them for in-place-write recovery and cannot depend on `services`.
pub use fresh_editor_core::recovery_types as types;

pub use storage::RecoveryStorage;
pub use store::{entries_under, partition_dir};
pub use types::{
    generate_buffer_id, path_hash, ChunkMeta, ChunkedRecoveryData, ChunkedRecoveryIndex,
    HotExitEntry, InplaceWriteRecovery, RecoveryChunk, RecoveryEntry, RecoveryMetadata,
    RecoveryResult, SaveOutcome, SessionInfo, MAX_CHUNK_SIZE,
};

use std::collections::HashSet;
use std::io;
use std::path::{Path, PathBuf};
use store::Store;

/// Configuration for the recovery service
#[derive(Debug, Clone)]
pub struct RecoveryConfig {
    /// Whether recovery is enabled
    pub enabled: bool,
}

impl Default for RecoveryConfig {
    fn default() -> Self {
        Self { enabled: true }
    }
}

/// The main recovery service: this editor's view of the recovery store.
#[derive(Debug)]
pub struct RecoveryService {
    /// `None` when recovery is off, or this editor could not take its lock
    /// (an unwritable store): without the lock, what it wrote would look
    /// abandoned to every other editor, so it writes nothing.
    store: Option<Store>,
}

impl RecoveryService {
    /// The service over the recovery store in `base_recovery_dir`.
    pub fn new(config: RecoveryConfig, base_recovery_dir: &Path) -> Self {
        let store = if config.enabled {
            match Store::open(base_recovery_dir) {
                Ok(store) => Some(store),
                Err(e) => {
                    tracing::warn!(
                        "Recovery is off: could not take a lock in {}: {}",
                        base_recovery_dir.display(),
                        e
                    );
                    None
                }
            }
        } else {
            None
        };
        Self { store }
    }

    /// Check if recovery is enabled
    pub fn is_enabled(&self) -> bool {
        self.store.is_some()
    }

    /// This editor's id in the store, while recovery is on.
    pub fn editor_id(&self) -> Option<&str> {
        self.store.as_ref().map(Store::editor_id)
    }

    /// A new entry id, for a buffer that has none yet or whose last entry
    /// was resolved (saved, discarded): an id is never reused, so a delete
    /// still on its way can't remove the next backup.
    pub fn new_entry_id() -> String {
        generate_buffer_id()
    }

    /// Write buffer content to entry `id`, for a buffer in workspace
    /// `workspace_id` on `root`, and return the handle the buffer keeps.
    #[allow(clippy::too_many_arguments)]
    pub fn save_buffer(
        &mut self,
        root: &Path,
        id: &str,
        chunks: Vec<RecoveryChunk>,
        original_path: Option<&Path>,
        buffer_name: Option<&str>,
        line_count: Option<usize>,
        original_file_size: usize,
        final_size: usize,
        workspace_id: &str,
        remote: bool,
    ) -> io::Result<Option<HotExitEntry>> {
        let Some(store) = self.store.as_mut() else {
            return Ok(None);
        };
        // An entry taken for a window that stands in for every workspace on
        // its root stays the workspace's it names
        let stamp = (!store.keeps_stamp(id)).then_some(workspace_id);
        store.own_entry(root, id).save_recovery_owned(
            id,
            chunks,
            original_path,
            buffer_name,
            line_count,
            original_file_size,
            final_size,
            stamp,
            remote,
        )?;
        Ok(Some(store.own_handle(root, id)))
    }

    /// Where this editor keeps entry `id` for a buffer on `root`, so it can
    /// be deleted off the main loop ([`Self::delete_entry_dir`]).
    pub fn entry_dir(&self, root: &Path, id: &str) -> Option<PathBuf> {
        self.store.as_ref().map(|s| s.own_entry_dir(root, id))
    }

    /// Delete an entry this editor holds, given [`Self::entry_dir`].
    pub fn delete_entry_dir(dir: &Path) -> io::Result<()> {
        match std::fs::remove_dir_all(dir) {
            Err(e) if e.kind() != io::ErrorKind::NotFound => Err(e),
            _ => Ok(()),
        }
    }

    /// Let go of entry `id`: it stays as unsaved work, held by no editor,
    /// for whoever shows its workspace next. For a workspace closed with
    /// unsaved work.
    pub fn release(&mut self, root: &Path, id: &str) -> io::Result<()> {
        match &mut self.store {
            Some(store) => store.release(root, id),
            None => Ok(()),
        }
    }

    /// Let go of entry `id`, taken but not restored, and don't offer it
    /// again this session.
    pub fn give_back(&mut self, root: &Path, id: &str) -> io::Result<()> {
        match &mut self.store {
            Some(store) => store.give_back(root, id),
            None => Ok(()),
        }
    }

    /// The ids of the workspaces on `root`: those with a workspace file,
    /// and `open` — this editor's windows on it, some of which may never
    /// have been saved.
    pub fn known_workspace_ids(
        root: &Path,
        open: impl IntoIterator<Item = String>,
    ) -> HashSet<String> {
        let mut ids = crate::workspace::workspace_ids_for_root(root);
        ids.extend(open);
        ids
    }

    /// The unsaved work for `root` no running editor holds, newest first:
    /// for a window on a remote authority (`Some(true)`), a local one
    /// (`Some(false)`), or either (`None`). `known_ids` is
    /// [`Self::known_workspace_ids`].
    pub fn list_recoverable(
        &self,
        root: &Path,
        known_ids: &HashSet<String>,
        remote: Option<bool>,
    ) -> Vec<RecoveryEntry> {
        let Some(store) = &self.store else {
            return Vec::new();
        };
        let mut entries = store.recoverable(root, known_ids);
        if let Some(remote) = remote {
            entries.retain(|entry| entry.metadata.remote == remote);
        }
        entries
    }

    /// Whether `entry` is workspace `workspace_id`'s to restore: stamped
    /// with it, or with no workspace that is known on its root
    /// ([`Self::known_workspace_ids`]) — an unstamped entry, or one from a
    /// launch that kept no workspace (`fresh FILE`). An entry stamped for
    /// another known workspace on the same root waits for that one.
    pub fn belongs_to(entry: &RecoveryEntry, workspace_id: &str, known: &HashSet<String>) -> bool {
        match entry.metadata.workspace_id.as_deref() {
            Some(stamp) => stamp == workspace_id || !known.contains(stamp),
            None => true,
        }
    }

    /// Take `entry` (from [`Self::list_recoverable`] for `root`) over for a
    /// buffer in workspace `workspace_id`. Returns it as it now is, or
    /// `None` if another editor took it first. `restamp` makes it that
    /// workspace's from now on; a window with no saved identity of its own
    /// passes `false`, so the entry stays the workspace's it names.
    pub fn claim(
        &mut self,
        root: &Path,
        entry: &RecoveryEntry,
        workspace_id: &str,
        restamp: bool,
    ) -> io::Result<Option<RecoveryEntry>> {
        match self.store.as_mut() {
            Some(store) => store.claim(root, entry, workspace_id, restamp),
            None => Ok(None),
        }
    }

    /// End the session cleanly (call on normal editor shutdown): of the
    /// entries this editor holds, those in `keep` — buffers still holding
    /// unsaved work, for hot exit — stay as unsaved work no editor holds,
    /// and the rest are deleted: their buffers were saved, discarded or
    /// closed. Nothing any other editor holds or left is touched (issue
    /// #3189).
    pub fn end_session(&mut self, keep: &HashSet<String>) {
        if let Some(store) = self.store.take() {
            store.end(keep);
        }
    }

    /// Load recovery content for a specific entry
    ///
    /// For entries with original_file_size > 0, returns RecoveredChunks so the caller
    /// can apply chunks directly to the buffer (more efficient than full reconstruction).
    /// For new buffer entries (original_file_size == 0), the full content is in the chunks.
    pub fn load_recovery(&self, entry: &RecoveryEntry) -> io::Result<RecoveryResult> {
        let storage = RecoveryStorage::with_dir(
            entry
                .metadata_path
                .parent()
                .unwrap_or(Path::new("."))
                .to_path_buf(),
        );
        // A remote file's path names nothing on this host
        let local = !entry.metadata.remote;
        // Check if we need the original file for reconstruction
        if entry.metadata.original_file_size > 0 {
            // Large file recovery - return chunks to apply on top of original
            if let Some(ref original_path) = entry.metadata.original_path {
                // Check if original file was modified since recovery was saved
                if entry.original_file_modified() {
                    return Ok(RecoveryResult::OriginalFileModified {
                        id: entry.id.clone(),
                        original_path: original_path.clone(),
                    });
                }

                if local && !original_path.exists() {
                    return Ok(RecoveryResult::Corrupted {
                        id: entry.id.clone(),
                        reason: format!(
                            "Original file not found: {}. Recovery requires the original file.",
                            original_path.display()
                        ),
                    });
                }

                // Load chunks and return them for direct application
                let chunked_data = storage.read_chunked_content(&entry.id)?.ok_or_else(|| {
                    io::Error::new(io::ErrorKind::NotFound, "Chunk content not found")
                })?;

                return Ok(RecoveryResult::RecoveredChunks {
                    original_path: original_path.clone(),
                    chunks: chunked_data.chunks,
                });
            } else {
                return Ok(RecoveryResult::Corrupted {
                    id: entry.id.clone(),
                    reason: "Recovery entry requires original file but path is not set".to_string(),
                });
            }
        }

        // New buffer or small file - chunk contains full content
        // For file-backed small files, check if the original was modified on disk
        if entry.metadata.original_path.is_some() && entry.original_file_modified() {
            return Ok(RecoveryResult::OriginalFileModified {
                id: entry.id.clone(),
                original_path: entry.metadata.original_path.clone().unwrap(),
            });
        }

        // Load the chunk data directly
        let chunked_data = storage
            .read_chunked_content(&entry.id)?
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Chunk content not found"))?;

        // For original_file_size == 0, we expect exactly one chunk with offset=0
        if chunked_data.chunks.len() == 1 && chunked_data.chunks[0].offset == 0 {
            let content = chunked_data.chunks[0].content.clone();
            // The metadata recorded how big the buffer was. Checking it is
            // the difference between handing back the user's work and
            // handing back a prefix of it that looks just as complete.
            let expected = entry.metadata.content_size as usize;
            if content.len() < expected {
                return Ok(RecoveryResult::RecoveredPartial {
                    original_path: entry.metadata.original_path.clone(),
                    found: content.len(),
                    expected,
                    content,
                });
            }
            Ok(RecoveryResult::Recovered {
                original_path: entry.metadata.original_path.clone(),
                content,
            })
        } else {
            Ok(RecoveryResult::Corrupted {
                id: entry.id.clone(),
                reason: "Invalid recovery format: expected single chunk for new buffer".to_string(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn service(base: &TempDir) -> RecoveryService {
        RecoveryService::new(RecoveryConfig::default(), base.path())
    }

    fn project(base: &TempDir) -> PathBuf {
        let root = base.path().join("project");
        std::fs::create_dir_all(&root).unwrap();
        root
    }

    /// Back up `text` for a buffer in workspace "ws"; returns its handle.
    fn back_up(
        service: &mut RecoveryService,
        root: &Path,
        path: Option<&Path>,
        text: &[u8],
    ) -> HotExitEntry {
        let id = RecoveryService::new_entry_id();
        let chunks = vec![RecoveryChunk::new(0, 0, text.to_vec())];
        service
            .save_buffer(
                root,
                &id,
                chunks,
                path,
                None,
                Some(1),
                0,
                text.len(),
                "ws",
                false,
            )
            .unwrap()
            .unwrap()
    }

    fn recoverable(service: &RecoveryService, root: &Path) -> Vec<RecoveryEntry> {
        service.list_recoverable(root, &HashSet::new(), Some(false))
    }

    // The crash tests below stop a save between its steps — record the save
    // in the entry, write the file, delete the entry — and drop the editor
    // without ending its session, as a crash would; the next editor over
    // the store is the next launch.

    /// Crash after the write, before the delete (issue #3626's entry, left
    /// by a crash instead of a missing delete): the content is in the file,
    /// so the entry must not come back as an unsaved buffer, and is cleared
    /// away.
    #[test]
    fn crash_after_the_save_wrote_leaves_nothing_to_recover() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let target = root.join("input-23.txt");
        {
            let mut crashed = service(&base);
            let entry = back_up(&mut crashed, &root, None, b"footnotes");
            assert!(entry.begin_save(&target, 9).unwrap());
            std::fs::write(&target, b"footnotes").unwrap();
        }

        let next = service(&base);
        assert!(recoverable(&next, &root).is_empty());
        assert!(recoverable(&next, &root).is_empty());
    }

    /// The same for a save over an existing file: it changed, so the save
    /// wrote it.
    #[test]
    fn crash_after_saving_over_a_file_leaves_nothing_to_recover() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let target = root.join("notes.txt");
        std::fs::write(&target, b"old").unwrap();
        {
            let mut crashed = service(&base);
            let entry = back_up(&mut crashed, &root, Some(&target), b"new text");
            assert!(entry.begin_save(&target, 8).unwrap());
            std::fs::write(&target, b"new text").unwrap();
        }

        assert!(recoverable(&service(&base), &root).is_empty());
    }

    /// Crash after recording the save, before the write: nothing reached the
    /// file, so the entry is the only copy of the work and comes back, with
    /// the record cleared so that a later change to the file — by anyone —
    /// can't read as that save having happened.
    #[test]
    fn crash_before_the_save_wrote_keeps_the_unsaved_work() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let target = root.join("input-23.txt");
        {
            let mut crashed = service(&base);
            let entry = back_up(&mut crashed, &root, None, b"footnotes");
            assert!(entry.begin_save(&target, 9).unwrap());
        }

        let next = service(&base);
        let entries = recoverable(&next, &root);
        assert_eq!(entries.len(), 1);
        assert!(entries[0].metadata.saving.is_none());
        assert!(matches!(
            next.load_recovery(&entries[0]).unwrap(),
            RecoveryResult::Recovered { content, .. } if content == b"footnotes"
        ));
        std::fs::write(&target, b"someone else's").unwrap();
        assert_eq!(recoverable(&next, &root).len(), 1);
    }

    /// Crash during a save that writes the file in place (one owned by
    /// someone else, or with hard links): the file is torn, holding only the
    /// start of what the save was writing. It changed, but not to what the
    /// save writes, so the entry is still the unsaved work.
    #[test]
    fn crash_during_an_in_place_write_keeps_the_unsaved_work() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let target = root.join("notes.txt");
        std::fs::write(&target, b"old content").unwrap();
        {
            let mut crashed = service(&base);
            let entry = back_up(&mut crashed, &root, Some(&target), b"new content!");
            assert!(entry.begin_save(&target, 12).unwrap());
            // Truncated, then cut off part-way through the new content
            std::fs::write(&target, b"new co").unwrap();
        }

        let next = service(&base);
        assert_eq!(recoverable(&next, &root).len(), 1);
        assert_eq!(recoverable(&next, &root).len(), 1);
    }

    /// A recovery write after a save was recorded is newer content than that
    /// save wrote, so it clears the record: the entry is unsaved work again,
    /// whatever the file does next.
    #[test]
    fn a_recovery_write_clears_a_recorded_save() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let target = root.join("input-23.txt");
        {
            let mut crashed = service(&base);
            let entry = back_up(&mut crashed, &root, None, b"first");
            assert!(entry.begin_save(&target, 5).unwrap());
            let chunks = vec![RecoveryChunk::new(0, 0, b"second".to_vec())];
            crashed
                .save_buffer(
                    &root,
                    entry.id(),
                    chunks,
                    None,
                    None,
                    Some(1),
                    0,
                    6,
                    "ws",
                    false,
                )
                .unwrap();
        }

        std::fs::write(&target, b"first").unwrap();
        let entries = recoverable(&service(&base), &root);
        assert_eq!(entries.len(), 1);
        assert!(entries[0].metadata.saving.is_none());
    }

    /// Issue #3189: a clean exit deletes only what this editor held and
    /// resolved. Unsaved work it keeps survives as anyone's; work another
    /// editor holds, or left, is never touched — however old it is.
    #[test]
    fn a_clean_exit_deletes_only_what_it_held_and_resolved() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let mut other = service(&base);
        back_up(&mut other, &root, None, b"another editor's");
        let mut left = service(&base);
        back_up(&mut left, &root, None, b"left by a crash");
        drop(left);

        let mut this = service(&base);
        let kept = back_up(&mut this, &root, None, b"still unsaved");
        back_up(&mut this, &root, None, b"saved since");
        this.end_session(&HashSet::from([kept.id().to_string()]));

        let mut texts: Vec<Vec<u8>> = recoverable(&service(&base), &root)
            .iter()
            .map(|e| match service(&base).load_recovery(e).unwrap() {
                RecoveryResult::Recovered { content, .. } => content,
                other => panic!("{other:?}"),
            })
            .collect();
        texts.sort();
        assert_eq!(
            texts,
            [b"left by a crash".to_vec(), b"still unsaved".to_vec()]
        );
        drop(other);
        assert_eq!(recoverable(&service(&base), &root).len(), 3);
    }

    /// An entry is its workspace's, or — stamped with no workspace known on
    /// the root — anyone's on that root; one stamped for another known
    /// workspace waits for it.
    #[test]
    fn an_entry_belongs_to_its_workspace_or_to_none_known() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let mut crashed = service(&base);
        back_up(&mut crashed, &root, None, b"x");
        drop(crashed);
        let entry = recoverable(&service(&base), &root).remove(0);

        let known = HashSet::from(["ws".to_string(), "ws-other".to_string()]);
        assert!(RecoveryService::belongs_to(&entry, "ws", &known));
        assert!(!RecoveryService::belongs_to(&entry, "ws-other", &known));
        assert!(RecoveryService::belongs_to(
            &entry,
            "ws-other",
            &HashSet::from(["ws-other".to_string()])
        ));
    }

    /// A remote buffer's entry is restored only into a remote window, and
    /// the other way round: the paths name files on different hosts.
    #[test]
    fn remote_and_local_entries_stay_apart() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let mut crashed = service(&base);
        let chunks = vec![RecoveryChunk::new(0, 0, b"remote".to_vec())];
        crashed
            .save_buffer(
                &root,
                "r",
                chunks,
                Some(Path::new("/srv/app.txt")),
                None,
                Some(1),
                0,
                6,
                "ws",
                true,
            )
            .unwrap();
        drop(crashed);

        let next = service(&base);
        assert!(next
            .list_recoverable(&root, &HashSet::new(), Some(false))
            .is_empty());
        let remote = next.list_recoverable(&root, &HashSet::new(), Some(true));
        assert_eq!(remote.len(), 1);
        assert_eq!(remote[0].metadata.original_mtime, None);
    }

    #[test]
    fn test_disabled_service() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let mut service = RecoveryService::new(RecoveryConfig { enabled: false }, base.path());
        assert!(!service.is_enabled());
        let chunks = vec![RecoveryChunk::new(0, 0, b"content".to_vec())];
        assert!(service
            .save_buffer(&root, "test", chunks, None, None, None, 0, 7, "ws", false)
            .unwrap()
            .is_none());
        assert!(!base.path().join("ws").exists());
    }

    #[test]
    fn test_load_recovery_returns_chunks_for_large_files() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let original_content = b"Hello, this is the original content!";
        let original_path = root.join("original.txt");
        std::fs::write(&original_path, original_content).unwrap();
        {
            let mut crashed = service(&base);
            // Chunk: insert "PREFIX: " at offset 0
            let chunks = vec![RecoveryChunk::new(0, 0, b"PREFIX: ".to_vec())];
            crashed
                .save_buffer(
                    &root,
                    "big",
                    chunks,
                    Some(&original_path),
                    None,
                    Some(1),
                    original_content.len(), // original_file_size > 0
                    original_content.len() + 8,
                    "ws",
                    false,
                )
                .unwrap();
        }

        let next = service(&base);
        let entries = recoverable(&next, &root);
        assert_eq!(entries.len(), 1);
        match next.load_recovery(&entries[0]).unwrap() {
            RecoveryResult::RecoveredChunks {
                original_path: path,
                chunks,
            } => {
                assert_eq!(path, original_path);
                assert_eq!(chunks.len(), 1);
                assert_eq!(chunks[0].offset, 0);
                assert_eq!(chunks[0].original_len, 0);
                assert_eq!(chunks[0].content, b"PREFIX: ");
            }
            other => panic!("Expected RecoveredChunks result, got {other:?}"),
        }
    }

    /// A chunk file shorter than the metadata says must not come back as if
    /// it were the whole buffer.
    ///
    /// Each recovery file is written with its own atomic rename, so a crash
    /// can leave a chunk that is in place but incomplete. The sizes to check
    /// it against are right there in the metadata; without that check the
    /// editor hands back a buffer that looks like the user's unsaved work
    /// minus the end, with nothing saying so, and the next save writes that
    /// over the real thing.
    #[test]
    fn a_truncated_chunk_is_not_reported_as_whole_content() {
        let base = TempDir::new().unwrap();
        let root = project(&base);
        let content = b"COMPLETE-UNSAVED-WORK".to_vec();
        {
            let mut crashed = service(&base);
            let entry = back_up(&mut crashed, &root, None, &content);
            // Truncate the chunk on disk, leaving the metadata's sizes intact.
            let chunk_path = crashed
                .entry_dir(&root, entry.id())
                .unwrap()
                .join(format!("{}.chunk.0", entry.id()));
            let whole = std::fs::read(&chunk_path).unwrap();
            assert_eq!(whole, content, "the chunk holds the content verbatim");
            std::fs::write(&chunk_path, &whole[..8]).unwrap();
        }

        let next = service(&base);
        let entry = recoverable(&next, &root)
            .pop()
            .expect("a truncated entry is still listed");
        match next.load_recovery(&entry).unwrap() {
            RecoveryResult::Recovered { content: got, .. } => panic!(
                "silently returned {} of {} bytes as a complete recovery: {:?}",
                got.len(),
                content.len(),
                String::from_utf8_lossy(&got)
            ),
            other => {
                let text = format!("{other:?}");
                assert!(
                    text.contains('8') && text.contains(&content.len().to_string()),
                    "the result should say how much was found against how much \
                     was expected, got: {text}"
                );
            }
        }
    }
}
