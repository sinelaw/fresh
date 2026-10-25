//! Where recovery entries live: one store, whatever launched the editor.
//!
//! ```text
//! recovery/
//! ├── editors/<editor>.lock          locked by that editor while it runs
//! └── ws/<encoded project root>/      a project's entries
//!     ├── <id>/                       an entry no running editor holds
//!     │   ├── <id>.meta.json
//!     │   └── <id>.chunk.N
//!     └── held/<editor>/<id>/         an entry backing that editor's buffer
//! ```
//!
//! An entry is filed under the root of the workspace whose buffer it backs,
//! the same key as the workspace file that refers to it
//! (`workspaces/<encoded root>.<stable id>.json`), so any editor showing a
//! workspace finds that workspace's unsaved work, however it was launched.
//! Workspaces sharing a root are told apart by the entry's
//! `workspace_id` stamp.
//!
//! Who holds an entry is where it is, not a field to check. An editor
//! writes only under its own `held/<editor>/`, and takes an entry over by
//! renaming its directory there, which exactly one editor can win. An
//! editor's lock is released when it stops, however it stops, so what it
//! held is free to recover from then on; a clean exit moves the entries it
//! keeps up out of `held/` and deletes the rest.
//!
//! Earlier versions kept a store per launch mode (`default/<encoded launch
//! directory>/`, `sessions/<daemon name>/`, and before that the recovery
//! directory itself). Those are read as well, and an entry leaves one only
//! when an editor takes it over, so what nobody has taken stays where an
//! older version can still find it.

use super::storage::RecoveryStorage;
use super::types::{generate_buffer_id, HotExitEntry, RecoveryEntry, SaveOutcome};
use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

const PARTITIONS: &str = "ws";
const HELD: &str = "held";
const EDITORS: &str = "editors";
const LOCK_EXT: &str = "lock";
const LEGACY_DEFAULT: &str = "default";
const LEGACY_SESSIONS: &str = "sessions";

/// The directory a project's entries live in.
pub fn partition_dir(base: &Path, root: &Path) -> PathBuf {
    // Canonicalized as `workspace::workspace_path_for` does, so an entry
    // and the workspace file that refers to it can never be filed under two
    // spellings of one root.
    let canonical = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    base.join(PARTITIONS)
        .join(crate::workspace::encode_path_for_filename(&canonical))
}

/// This editor's hold on the entries under its `held/` directories: an
/// exclusive lock on `editors/<id>.lock` for as long as the editor runs.
/// The lock goes with the process, so a crash leaves nothing holding.
#[derive(Debug)]
pub struct Lease {
    id: String,
    path: PathBuf,
    _file: fs::File,
}

impl Lease {
    pub fn acquire(base: &Path) -> io::Result<Self> {
        use std::io::Write;
        let dir = base.join(EDITORS);
        fs::create_dir_all(&dir)?;
        remove_dead_leases(&dir);
        let id = format!("ed{}", generate_buffer_id().trim_start_matches("unsaved"));
        let path = dir.join(format!("{id}.{LOCK_EXT}"));
        // Locked under a name no other editor looks at, then renamed into
        // place, which keeps the lock: an editor sweeping dead leases must
        // never find this one unlocked and take it for dead.
        let staging = dir.join(format!(".{id}.{LOCK_EXT}.new"));
        let mut file = fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&staging)?;
        // The pid is for a filesystem that cannot lock (see `editor_is_live`)
        write!(file, "{}", std::process::id())?;
        let locked = match file.try_lock() {
            Ok(()) => Ok(()),
            Err(fs::TryLockError::Error(e)) if e.kind() == io::ErrorKind::Unsupported => {
                tracing::warn!(
                    "Cannot lock {} ({e}); other editors will go by its pid",
                    path.display()
                );
                Ok(())
            }
            Err(e) => Err(io::Error::other(e.to_string())),
        };
        if let Err(e) = locked.and_then(|()| fs::rename(&staging, &path)) {
            // Best-effort cleanup; the lock error is what matters
            #[allow(clippy::let_underscore_must_use)]
            let _ = fs::remove_file(&staging);
            return Err(e);
        }
        Ok(Self {
            id,
            path,
            _file: file,
        })
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    /// Give up the lease at a clean exit.
    pub fn end(self) {
        if let Err(e) = fs::remove_file(&self.path) {
            tracing::warn!("Failed to remove {}: {}", self.path.display(), e);
        }
    }
}

/// Whether the editor `id` is running, so that the entries it holds are its
/// live buffers' and not to be touched.
fn editor_is_live(base: &Path, id: &str) -> bool {
    let path = base.join(EDITORS).join(format!("{id}.{LOCK_EXT}"));
    let file = match fs::File::open(&path) {
        Ok(file) => file,
        // A clean exit removes the file; so does a later editor finding it dead
        Err(e) if e.kind() == io::ErrorKind::NotFound => return false,
        // Can't tell, so don't take its work
        Err(_) => return true,
    };
    match file.try_lock() {
        // Nothing holds it: the editor is gone. Released as `file` drops.
        Ok(()) => false,
        Err(fs::TryLockError::WouldBlock) => true,
        Err(fs::TryLockError::Error(e)) if e.kind() == io::ErrorKind::Unsupported => {
            let pid = fs::read_to_string(&path)
                .ok()
                .and_then(|pid| pid.trim().parse::<u32>().ok());
            // Without locks, an editor in this process can't be told from a
            // dropped one, so it counts as running.
            pid.is_none_or(|pid| pid == std::process::id() || super::types::is_process_running(pid))
        }
        Err(_) => true,
    }
}

/// Remove the lock files of editors that are gone. Their entries stay where
/// they are, free for anyone to recover; only the lock is litter.
fn remove_dead_leases(dir: &Path) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    let base = dir.parent().unwrap_or(dir);
    for entry in entries.flatten() {
        let path = entry.path();
        let Some(id) = path
            .file_name()
            .and_then(|n| n.to_str())
            .and_then(|n| n.strip_suffix(&format!(".{LOCK_EXT}")))
        else {
            continue;
        };
        if !editor_is_live(base, id) {
            #[allow(clippy::let_underscore_must_use)]
            let _ = fs::remove_file(&path);
        }
    }
}

/// The recovery directory, as seen by the editor holding `lease`.
#[derive(Debug)]
pub struct Store {
    base: PathBuf,
    lease: Lease,
    /// Partitions this editor holds entries in, for the exit to settle.
    touched: HashSet<PathBuf>,
}

impl Store {
    pub fn open(base: &Path) -> io::Result<Self> {
        Ok(Self {
            base: base.to_path_buf(),
            lease: Lease::acquire(base)?,
            touched: HashSet::new(),
        })
    }

    pub fn editor_id(&self) -> &str {
        self.lease.id()
    }

    fn held_dir(&self, partition: &Path) -> PathBuf {
        partition.join(HELD).join(self.lease.id())
    }

    /// Where this editor keeps entry `id` for a buffer in a workspace on `root`.
    pub fn own_entry_dir(&self, root: &Path, id: &str) -> PathBuf {
        self.held_dir(&partition_dir(&self.base, root)).join(id)
    }

    /// The store this editor writes entry `id` through, for `root`.
    pub fn own_entry(&mut self, root: &Path, id: &str) -> RecoveryStorage {
        let partition = partition_dir(&self.base, root);
        let dir = self.held_dir(&partition).join(id);
        self.touched.insert(partition);
        RecoveryStorage::with_dir(dir)
    }

    /// The handle for entry `id` this editor holds for `root`.
    pub fn own_handle(&self, root: &Path, id: &str) -> HotExitEntry {
        let dir = self.own_entry_dir(root, id);
        HotExitEntry::new(id, dir.join(format!("{id}.meta.json")))
    }

    /// Put entry `id` back as unsaved work no editor holds, for whoever shows
    /// its workspace next.
    pub fn release(&self, root: &Path, id: &str) -> io::Result<()> {
        let from = self.own_entry_dir(root, id);
        if !from.exists() {
            return Ok(());
        }
        let to = partition_dir(&self.base, root).join(id);
        fs::rename(&from, &to)
    }

    /// Every entry for `root` that no running editor holds: free ones, ones
    /// whose editor is gone, and those of earlier versions' stores that
    /// belong here (see [`Self::legacy_entries`]). Saves they record as
    /// interrupted are settled first, and an entry a save wrote is not
    /// listed. Newest first.
    ///
    /// `known_ids` are the workspace ids on `root`: they decide which of a
    /// shared old store's entries are this project's.
    pub fn recoverable(&self, root: &Path, known_ids: &HashSet<String>) -> Vec<RecoveryEntry> {
        let partition = partition_dir(&self.base, root);
        let mut entries = Vec::new();
        if let Ok(dir) = fs::read_dir(&partition) {
            for item in dir.flatten() {
                let path = item.path();
                if item.file_name() == HELD {
                    entries.extend(self.left_by_stopped_editors(&path));
                } else if path.is_dir() {
                    entries.extend(load_entry_dir(&path));
                }
            }
        }
        entries.extend(self.legacy_entries(root, known_ids));
        entries.retain_mut(settle_interrupted_save);
        entries.sort_by_key(|e| std::cmp::Reverse(e.metadata.updated_at));
        entries
    }

    /// What editors that have stopped held under `held`, tidying away the
    /// directories of those that left nothing.
    fn left_by_stopped_editors(&self, held: &Path) -> Vec<RecoveryEntry> {
        let mut entries = Vec::new();
        let Ok(editors) = fs::read_dir(held) else {
            return entries;
        };
        for editor in editors.flatten() {
            let Some(id) = editor.file_name().to_str().map(str::to_string) else {
                continue;
            };
            if id == self.lease.id() || editor_is_live(&self.base, &id) {
                continue;
            }
            let dir = editor.path();
            let before = entries.len();
            if let Ok(items) = fs::read_dir(&dir) {
                for item in items.flatten() {
                    if item.path().is_dir() {
                        entries.extend(load_entry_dir(&item.path()));
                    }
                }
            }
            if entries.len() == before {
                // Only removes it if empty
                #[allow(clippy::let_underscore_must_use)]
                let _ = fs::remove_dir(&dir);
            }
        }
        entries
    }

    /// The entries earlier versions' stores hold for `root`:
    /// * everything in the store of a direct launch in `root`
    ///   (`default/<encoded root>/`), all of it done in a workspace on it;
    /// * from a daemon's store (`sessions/<name>/`) and the flat layout before
    ///   those, the entries stamped with one of `known_ids`, and unstamped
    ///   ones for a file under `root`.
    ///
    /// A store whose editor is still running is its live buffers', and is
    /// skipped.
    fn legacy_entries(&self, root: &Path, known_ids: &HashSet<String>) -> Vec<RecoveryEntry> {
        let canonical = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
        let mut own: Vec<PathBuf> = [root, canonical.as_path()]
            .iter()
            .map(|r| {
                self.base
                    .join(LEGACY_DEFAULT)
                    .join(crate::workspace::encode_path_for_filename(r))
            })
            .collect();
        own.dedup();
        let mut shared = vec![self.base.clone()];
        if let Ok(sessions) = fs::read_dir(self.base.join(LEGACY_SESSIONS)) {
            shared.extend(sessions.flatten().map(|s| s.path()));
        }

        let mut entries = Vec::new();
        for (dir, whole) in own
            .into_iter()
            .map(|d| (d, true))
            .chain(shared.into_iter().map(|d| (d, false)))
        {
            let store = RecoveryStorage::with_dir(dir);
            if store
                .read_session_lock()
                .ok()
                .flatten()
                .is_some_and(|session| session.is_running())
            {
                continue;
            }
            let Ok(listed) = store.list_entries() else {
                continue;
            };
            entries.extend(listed.into_iter().filter(|entry| {
                whole
                    || match &entry.metadata.workspace_id {
                        Some(workspace) => known_ids.contains(workspace),
                        None => entry.metadata.original_path.as_ref().is_some_and(|path| {
                            path.starts_with(&canonical) || path.starts_with(root)
                        }),
                    }
            }));
        }
        entries
    }

    /// Take `entry` (one [`Self::recoverable`] listed for `root`) over for a
    /// buffer of this editor in workspace `workspace_id`, and return it as it
    /// now is. `None` when another editor took it first.
    pub fn claim(
        &mut self,
        root: &Path,
        entry: &RecoveryEntry,
        workspace_id: &str,
    ) -> io::Result<Option<RecoveryEntry>> {
        let partition = partition_dir(&self.base, root);
        let held = self.held_dir(&partition);
        fs::create_dir_all(&held)?;
        self.touched.insert(partition.clone());
        let Some(from) = entry.metadata_path.parent() else {
            return Ok(None);
        };
        let claimed = if from.starts_with(&partition) {
            // One rename: whoever makes it holds the entry
            let to = held.join(&entry.id);
            match fs::rename(from, &to) {
                Ok(()) => Some((to, entry.id.clone())),
                Err(e) if e.kind() == io::ErrorKind::NotFound => None,
                Err(e) => return Err(e),
            }
        } else {
            take_from_legacy_store(from, &entry.id, &held)?
        };
        let Some((dir, id)) = claimed else {
            return Ok(None);
        };
        if let Err(e) =
            super::types::remove_orphaned_temp_files(&crate::model::filesystem::StdFileSystem, &dir)
        {
            tracing::debug!("Could not tidy {}: {}", dir.display(), e);
        }
        let storage = RecoveryStorage::with_dir(dir);
        let Some(mut claimed) = storage.load_entry(&id)? else {
            return Ok(None);
        };
        if claimed.metadata.workspace_id.as_deref() != Some(workspace_id) {
            claimed.handle().stamp_workspace(workspace_id)?;
            claimed.metadata.workspace_id = Some(workspace_id.to_string());
        }
        Ok(Some(claimed))
    }

    /// At a clean exit: of the entries this editor holds, put those in
    /// `keep` back as unsaved work no editor holds, delete the rest (their
    /// buffers were saved, discarded or closed), and give up the lease.
    pub fn end(self, keep: &HashSet<String>) {
        let mut kept = 0;
        let mut removed = 0;
        for partition in &self.touched {
            let held = self.held_dir(partition);
            let Ok(items) = fs::read_dir(&held) else {
                continue;
            };
            for item in items.flatten() {
                let Some(id) = item.file_name().to_str().map(str::to_string) else {
                    continue;
                };
                let result = if keep.contains(&id) {
                    kept += 1;
                    fs::rename(item.path(), partition.join(&id))
                } else {
                    removed += 1;
                    fs::remove_dir_all(item.path())
                };
                if let Err(e) = result {
                    tracing::warn!("Failed to settle recovery entry {id}: {e}");
                }
            }
            #[allow(clippy::let_underscore_must_use)]
            let _ = fs::remove_dir(&held);
            #[allow(clippy::let_underscore_must_use)]
            let _ = fs::remove_dir(partition.join(HELD));
        }
        tracing::info!("Recovery session ended: kept {kept} unsaved buffer(s), removed {removed}");
        self.lease.end();
    }
}

/// Every entry in project directory `partition` (see [`partition_dir`]),
/// held or not: for looking at what is there, not for restoring.
pub fn entries_under(partition: &Path) -> Vec<RecoveryEntry> {
    let mut entries = Vec::new();
    let Ok(items) = fs::read_dir(partition) else {
        return entries;
    };
    for item in items.flatten() {
        let path = item.path();
        if item.file_name() == HELD {
            for editor in fs::read_dir(&path).into_iter().flatten().flatten() {
                for entry in fs::read_dir(editor.path()).into_iter().flatten().flatten() {
                    entries.extend(load_entry_dir(&entry.path()));
                }
            }
        } else if path.is_dir() {
            entries.extend(load_entry_dir(&path));
        }
    }
    entries
}

/// The entry in its own directory, named after it.
fn load_entry_dir(dir: &Path) -> Option<RecoveryEntry> {
    let id = dir.file_name()?.to_str()?;
    match RecoveryStorage::with_dir(dir.to_path_buf()).load_entry(id) {
        Ok(entry) => entry,
        Err(e) => {
            tracing::warn!(
                "Skipping unreadable recovery entry {}: {}",
                dir.display(),
                e
            );
            None
        }
    }
}

/// Settle a save `entry` records as under way when its editor stopped (see
/// [`super::types::SaveIntent`]); returns whether the entry is still
/// unsaved work. One a save wrote is deleted; one whose save never
/// happened has the record cleared, since left in place a later change to
/// the file by anyone would read as that save having written it; one whose
/// file can't be looked at is left for a later look.
///
/// Only for an entry no running editor holds, so no save of it is under way.
fn settle_interrupted_save(entry: &mut RecoveryEntry) -> bool {
    let Some(intent) = &entry.metadata.saving else {
        return true;
    };
    let path = intent.path.clone();
    let (settled, unsaved) = match intent.outcome() {
        SaveOutcome::Written => (delete_entry(entry), false),
        SaveOutcome::NotWritten => {
            let cleared = entry.handle().abandon_save().map(|_| ());
            if cleared.is_ok() {
                entry.metadata.saving = None;
            }
            (cleared, true)
        }
        SaveOutcome::Unknown => return true,
    };
    match settled {
        Ok(()) => tracing::info!(
            "Settled interrupted save of recovery entry {} to {}",
            entry.id,
            path.display()
        ),
        Err(e) => tracing::warn!(
            "Failed to settle interrupted save of recovery entry {}: {}",
            entry.id,
            e
        ),
    }
    unsaved
}

/// Delete `entry`, wherever it is.
fn delete_entry(entry: &RecoveryEntry) -> io::Result<()> {
    let Some(dir) = entry.metadata_path.parent() else {
        return Ok(());
    };
    if dir.file_name().and_then(|n| n.to_str()) == Some(entry.id.as_str()) {
        fs::remove_dir_all(dir)
    } else {
        RecoveryStorage::with_dir(dir.to_path_buf()).delete_recovery(&entry.id)
    }
}

/// Move entry `id` out of an earlier version's store `from` into a
/// directory of its own under `held`, under a new id: the old ones were
/// derived from a path, and two old stores can hold the same one. Returns
/// where it went, or `None` when another editor took it first.
fn take_from_legacy_store(
    from: &Path,
    id: &str,
    held: &Path,
) -> io::Result<Option<(PathBuf, String)>> {
    let new_id = generate_buffer_id();
    let to = held.join(&new_id);
    fs::create_dir_all(&to)?;
    let chunk_prefix = format!("{id}.chunk.");
    let mut chunks = Vec::new();
    for item in fs::read_dir(from)?.flatten() {
        let name = item.file_name();
        let Some(name) = name.to_str() else { continue };
        if let Some(n) = name.strip_prefix(&chunk_prefix) {
            if !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit()) {
                chunks.push(n.to_string());
            }
        }
    }
    // Copied, not moved: until the metadata moves the entry is still the
    // old store's, and a crash here leaves it whole there.
    for n in &chunks {
        fs::copy(
            from.join(format!("{id}.chunk.{n}")),
            to.join(format!("{new_id}.chunk.{n}")),
        )?;
    }
    // Moving the metadata is what takes the entry: one rename, one winner.
    let meta_from = from.join(format!("{id}.meta.json"));
    let meta_to = to.join(format!("{new_id}.meta.json"));
    match fs::rename(&meta_from, &meta_to) {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            #[allow(clippy::let_underscore_must_use)]
            let _ = fs::remove_dir_all(&to);
            return Ok(None);
        }
        Err(_) => {
            // Another filesystem: copy, then remove
            fs::copy(&meta_from, &meta_to)?;
            fs::remove_file(&meta_from)?;
        }
    }
    for n in &chunks {
        #[allow(clippy::let_underscore_must_use)]
        let _ = fs::remove_file(from.join(format!("{id}.chunk.{n}")));
    }
    #[allow(clippy::let_underscore_must_use)]
    let _ = fs::remove_file(from.join(format!("{id}.content")));
    Ok(Some((to, new_id)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::recovery::types::RecoveryChunk;
    use tempfile::TempDir;

    fn root(base: &TempDir) -> PathBuf {
        let root = base.path().join("project");
        fs::create_dir_all(&root).unwrap();
        root
    }

    fn write(store: &mut Store, root: &Path, id: &str, path: Option<&Path>, workspace: &str) {
        store
            .own_entry(root, id)
            .save_recovery_owned(
                id,
                vec![RecoveryChunk::new(0, 0, b"text".to_vec())],
                path,
                None,
                Some(1),
                0,
                4,
                Some(workspace),
                false,
            )
            .unwrap();
    }

    fn ids(entries: &[RecoveryEntry]) -> Vec<String> {
        let mut ids: Vec<_> = entries.iter().map(|e| e.id.clone()).collect();
        ids.sort();
        ids
    }

    /// A running editor's entries are its live buffers': no other editor
    /// sees them. Once it stops — however it stops — they are anyone's.
    #[test]
    fn a_running_editors_entries_are_not_recoverable_until_it_stops() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let mut first = Store::open(base.path()).unwrap();
        write(&mut first, &root, "a", None, "ws");

        let second = Store::open(base.path()).unwrap();
        assert!(second.recoverable(&root, &HashSet::new()).is_empty());

        drop(first); // a crash: nothing settled, the lock let go
        assert_eq!(ids(&second.recoverable(&root, &HashSet::new())), ["a"]);
    }

    /// Only one editor can take an entry over.
    #[test]
    fn an_entry_is_claimed_once() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let mut gone = Store::open(base.path()).unwrap();
        write(&mut gone, &root, "a", None, "ws");
        drop(gone);

        let mut one = Store::open(base.path()).unwrap();
        let mut two = Store::open(base.path()).unwrap();
        let listed = one.recoverable(&root, &HashSet::new());
        assert_eq!(listed.len(), 1);
        let entry = &listed[0];
        assert!(one.claim(&root, entry, "ws").unwrap().is_some());
        assert!(two.claim(&root, entry, "ws").unwrap().is_none());
        assert!(two.recoverable(&root, &HashSet::new()).is_empty());
    }

    /// A clean exit keeps the entries it is told to as anyone's unsaved
    /// work, deletes the rest of its own, and leaves other editors' alone.
    #[test]
    fn a_clean_exit_keeps_what_it_is_told_and_deletes_the_rest_of_its_own() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let mut other = Store::open(base.path()).unwrap();
        write(&mut other, &root, "theirs", None, "ws");

        let mut mine = Store::open(base.path()).unwrap();
        write(&mut mine, &root, "kept", None, "ws");
        write(&mut mine, &root, "saved", None, "ws");
        mine.end(&HashSet::from(["kept".to_string()]));

        let third = Store::open(base.path()).unwrap();
        assert_eq!(ids(&third.recoverable(&root, &HashSet::new())), ["kept"]);
        drop(other);
        assert_eq!(
            ids(&third.recoverable(&root, &HashSet::new())),
            ["kept", "theirs"]
        );
    }

    /// The store of a direct launch in a project is that project's, and an
    /// entry taken from it leaves it under a new id.
    #[test]
    fn an_old_direct_launch_store_is_read_and_taken_from() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let canonical = root.canonicalize().unwrap();
        let old = RecoveryStorage::with_dir(
            base.path()
                .join("default")
                .join(crate::workspace::encode_path_for_filename(&canonical)),
        );
        old.save_recovery(
            "abc",
            vec![RecoveryChunk::new(0, 0, b"old".to_vec())],
            None,
            None,
            Some(1),
            0,
            3,
        )
        .unwrap();

        let mut store = Store::open(base.path()).unwrap();
        let listed = store.recoverable(&root, &HashSet::new());
        assert_eq!(ids(&listed), ["abc"]);
        let claimed = store.claim(&root, &listed[0], "ws").unwrap().unwrap();
        assert_ne!(claimed.id, "abc");
        assert_eq!(claimed.metadata.workspace_id.as_deref(), Some("ws"));
        assert!(old.list_entries().unwrap().is_empty());
        let content =
            RecoveryStorage::with_dir(claimed.metadata_path.parent().unwrap().to_path_buf())
                .read_chunked_content(&claimed.id)
                .unwrap()
                .unwrap();
        assert_eq!(content.chunks[0].content, b"old");
    }

    /// From a daemon's old store, only this project's entries: stamped with
    /// one of its workspaces, or unstamped for a file under it.
    #[test]
    fn an_old_daemon_store_gives_up_only_this_projects_entries() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let old = RecoveryStorage::with_dir(base.path().join("sessions").join("orchestrator"));
        let save = |id: &str, path: Option<&Path>, ws: Option<&str>| {
            old.save_recovery_owned(
                id,
                vec![RecoveryChunk::new(0, 0, b"x".to_vec())],
                path,
                None,
                Some(1),
                0,
                1,
                ws,
                false,
            )
            .unwrap();
        };
        save("mine", None, Some("ws-here"));
        save("theirs", None, Some("ws-elsewhere"));
        save("unstamped", Some(&root.join("a.txt")), None);
        save(
            "unstamped-elsewhere",
            Some(Path::new("/elsewhere/b.txt")),
            None,
        );

        let store = Store::open(base.path()).unwrap();
        let known = HashSet::from(["ws-here".to_string()]);
        assert_eq!(
            ids(&store.recoverable(&root, &known)),
            ["mine", "unstamped"]
        );
    }

    /// A save that wrote its file before the crash leaves nothing to
    /// recover; one that never wrote leaves the work, record cleared.
    #[test]
    fn interrupted_saves_are_settled_when_listed() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let target = root.join("saved.txt");
        let mut gone = Store::open(base.path()).unwrap();
        write(&mut gone, &root, "written", None, "ws");
        write(&mut gone, &root, "not-written", None, "ws");
        gone.own_handle(&root, "written")
            .begin_save(&target, 4)
            .unwrap();
        fs::write(&target, "text").unwrap();
        gone.own_handle(&root, "not-written")
            .begin_save(&root.join("never.txt"), 4)
            .unwrap();
        drop(gone);

        let store = Store::open(base.path()).unwrap();
        let listed = store.recoverable(&root, &HashSet::new());
        assert_eq!(ids(&listed), ["not-written"]);
        assert!(listed[0].metadata.saving.is_none());
        assert_eq!(
            ids(&store.recoverable(&root, &HashSet::new())),
            ["not-written"]
        );
    }
}
