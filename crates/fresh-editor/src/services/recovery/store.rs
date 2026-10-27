//! Where recovery entries live: one flat store, whatever launched the
//! editor and whichever workspace a buffer was in.
//!
//! ```text
//! recovery/
//! ├── editors/<editor>.lock           locked by that editor while it runs
//! └── entries/
//!     ├── <id>/                       an entry no running editor holds
//!     │   ├── <id>.meta.json
//!     │   └── <id>.chunk.N
//!     └── held/<editor>/<id>/         an entry backing that editor's buffer
//! ```
//!
//! An entry is a buffer's unsaved content and nothing about the layout it
//! was shown in: a file's names its file, and a file has one buffer in an
//! editor, so whoever opens the file finds its unsaved work, from any
//! project and any launch. An unnamed buffer's is placed by the workspace
//! layout that refers to its id. Each also records the project root it was
//! last edited in, which decides where work nobody's layout or open file
//! claims is offered.
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

const ENTRIES: &str = "entries";
const HELD: &str = "held";
const EDITORS: &str = "editors";
const LOCK_EXT: &str = "lock";
const LEGACY_DEFAULT: &str = "default";
const LEGACY_SESSIONS: &str = "sessions";

/// The directory entries live in.
pub fn entries_dir(base: &Path) -> PathBuf {
    base.join(ENTRIES)
}

/// This editor's hold on the entries under its `held/` directory: an
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
        // A filesystem that cannot lock (one without lock support, or NFS
        // with no lock daemon) leaves the pid to go by: recovery stays on.
        if let Err(e) = file.try_lock() {
            tracing::warn!(
                "Cannot lock {} ({e}); other editors will go by its pid",
                path.display()
            );
        }
        if let Err(e) = fs::rename(&staging, &path) {
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
        // This filesystem can't lock: go by the pid the lease records.
        Err(fs::TryLockError::Error(_)) => {
            let pid = fs::read_to_string(&path)
                .ok()
                .and_then(|pid| pid.trim().parse::<u32>().ok());
            // Without locks, an editor in this process can't be told from a
            // dropped one, so it counts as running.
            pid.is_none_or(|pid| pid == std::process::id() || super::types::is_process_running(pid))
        }
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
    /// Entries this editor holds only because putting them back failed:
    /// they are not its buffers' work, so the exit keeps them, never deletes.
    must_keep: HashSet<String>,
    /// Entries this editor took and gave back unrestored (unreadable, or
    /// changed on disk under a large file's deltas): not offered again this
    /// session, where every workspace switch would take them back once more.
    given_back: HashSet<String>,
    /// The files this editor has open, locked against a second editor
    /// changing them (see [`super::file_locks`]).
    pub files: super::file_locks::FileLocks,
}

impl Store {
    pub fn open(base: &Path) -> io::Result<Self> {
        Ok(Self {
            base: base.to_path_buf(),
            lease: Lease::acquire(base)?,
            must_keep: HashSet::new(),
            given_back: HashSet::new(),
            files: super::file_locks::FileLocks::new(base),
        })
    }

    pub fn editor_id(&self) -> &str {
        self.lease.id()
    }

    fn held_dir(&self) -> PathBuf {
        entries_dir(&self.base).join(HELD).join(self.lease.id())
    }

    /// Where this editor keeps entry `id`.
    pub fn own_entry_dir(&self, id: &str) -> PathBuf {
        self.held_dir().join(id)
    }

    /// The store this editor writes entry `id` through.
    pub fn own_entry(&self, id: &str) -> RecoveryStorage {
        RecoveryStorage::with_dir(self.own_entry_dir(id))
    }

    /// The handle for entry `id` this editor holds.
    pub fn own_handle(&self, id: &str) -> HotExitEntry {
        HotExitEntry::new(id, self.own_entry_dir(id).join(format!("{id}.meta.json")))
    }

    /// Put entry `id` back as unsaved work no editor holds, for whoever opens
    /// its buffer next. If that fails it stays held, and the exit keeps it.
    pub fn release(&mut self, id: &str) -> io::Result<()> {
        let from = self.own_entry_dir(id);
        if !from.exists() {
            return Ok(());
        }
        let to = entries_dir(&self.base).join(id);
        fs::rename(&from, &to).inspect_err(|_| {
            self.must_keep.insert(id.to_string());
        })
    }

    /// [`Self::release`] an entry this editor took but could not restore,
    /// and stop offering it for the rest of the session.
    pub fn give_back(&mut self, id: &str) -> io::Result<()> {
        self.given_back.insert(id.to_string());
        self.release(id)
    }

    /// Every entry no running editor holds: free ones, and ones whose editor
    /// is gone. With `legacy`, also those of earlier versions' stores that
    /// belong to that root (see [`Self::legacy_entries`]), recorded as last
    /// edited there. Saves they record as interrupted are settled first, and
    /// an entry a save wrote is not listed. Newest first.
    pub fn recoverable(&self, legacy: Option<(&Path, &HashSet<String>)>) -> Vec<RecoveryEntry> {
        let entries_dir = entries_dir(&self.base);
        let mut entries = Vec::new();
        if let Ok(dir) = fs::read_dir(&entries_dir) {
            for item in dir.flatten() {
                let path = item.path();
                if item.file_name() == HELD {
                    entries.extend(self.left_by_stopped_editors(&path));
                } else if path.is_dir() {
                    entries.extend(load_entry_dir(&path));
                }
            }
        }
        if let Some((root, known_ids)) = legacy {
            entries.extend(self.legacy_entries(root, known_ids));
        }

        entries.retain(|entry| !self.given_back.contains(&entry.id));
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

    /// The entries earlier versions' stores hold for `root`, recorded as last
    /// edited there:
    /// * from the store of a launch in `root` (`default/<encoded root>/`),
    ///   everything but what is stamped for a saved workspace on another
    ///   root: an unnamed `fresh -a` kept every window's work there;
    /// * from a daemon's store (`sessions/<name>/`) and the flat layout before
    ///   those, the entries stamped with one of `known_ids` (the workspaces
    ///   on `root`), and unstamped ones for a file under `root`.
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
        // Read only if a launch store has a stamped entry to place
        let mut saved_anywhere: Option<HashSet<String>> = None;
        for (dir, launch_store) in own
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
                match (&entry.metadata.workspace_id, launch_store) {
                    (Some(workspace), _) if known_ids.contains(workspace) => true,
                    (Some(workspace), true) => !saved_anywhere
                        .get_or_insert_with(crate::workspace::all_workspace_ids)
                        .contains(workspace),
                    (Some(_), false) => false,
                    (None, true) => true,
                    (None, false) => {
                        entry.metadata.original_path.as_ref().is_some_and(|path| {
                            path.starts_with(&canonical) || path.starts_with(root)
                        })
                    }
                }
            }));
        }
        for entry in &mut entries {
            entry.metadata.root = Some(canonical.clone());
        }
        entries
    }

    /// Take `entry` (one [`Self::recoverable`] listed) over for a buffer of
    /// this editor, and return it as it now is. `None` when another editor
    /// took it first.
    ///
    /// Anything that fails once the entry is taken puts it back, so an entry
    /// is never left held by an editor with no buffer for it — the exit
    /// would delete it.
    pub fn claim(&mut self, entry: &RecoveryEntry) -> io::Result<Option<RecoveryEntry>> {
        let entries_dir = entries_dir(&self.base);
        let held = self.held_dir();
        fs::create_dir_all(&held)?;
        let Some(from) = entry.metadata_path.parent() else {
            return Ok(None);
        };
        let claimed = if from.starts_with(&entries_dir) {
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
        let taken = RecoveryStorage::with_dir(dir.clone())
            .load_entry(&id)
            .and_then(|loaded| {
                loaded.ok_or_else(|| {
                    io::Error::new(io::ErrorKind::NotFound, "the entry vanished once taken")
                })
            });
        match taken {
            Ok(mut claimed) => {
                // An old store's entry keeps the root it was listed under,
                // on disk too: a crash before its next backup must not leave
                // it with none
                if claimed.metadata.root.is_none() {
                    if let Some(root) = entry.metadata.root.clone() {
                        if let Err(e) = claimed.handle().set_root(&root) {
                            tracing::debug!("Could not record the root of {}: {e}", claimed.id);
                        }
                        claimed.metadata.root = Some(root);
                    }
                }
                Ok(Some(claimed))
            }
            Err(e) => {
                let back = entries_dir.join(&id);
                if let Err(back_e) = fs::rename(&dir, &back) {
                    tracing::warn!(
                        "Could not put recovery entry {id} back after failing to take it \
                         ({e}): {back_e}"
                    );
                    self.must_keep.insert(id.clone());
                }
                self.given_back.insert(id);
                Err(e)
            }
        }
    }

    /// At a clean exit: of the entries this editor holds, put those in
    /// `keep` back as unsaved work no editor holds, delete the rest (their
    /// buffers were saved, discarded or closed), and give up the lease.
    pub fn end(mut self, keep: &HashSet<String>) {
        self.files.release_all();
        let entries_dir = entries_dir(&self.base);
        let held = self.held_dir();
        let mut kept = 0;
        let mut removed = 0;
        if let Ok(items) = fs::read_dir(&held) {
            for item in items.flatten() {
                let Some(id) = item.file_name().to_str().map(str::to_string) else {
                    continue;
                };
                let result = if keep.contains(&id) || self.must_keep.contains(&id) {
                    kept += 1;
                    fs::rename(item.path(), entries_dir.join(&id))
                } else {
                    removed += 1;
                    fs::remove_dir_all(item.path())
                };
                if let Err(e) = result {
                    tracing::warn!("Failed to settle recovery entry {id}: {e}");
                }
            }
        }
        #[allow(clippy::let_underscore_must_use)]
        let _ = fs::remove_dir(&held);
        tracing::info!("Recovery session ended: kept {kept} unsaved buffer(s), removed {removed}");
        self.lease.end();
    }
}

/// Every entry in the store, held or not: for looking at what is there, not
/// for restoring.
pub fn all_entries(base: &Path) -> Vec<RecoveryEntry> {
    let dir = entries_dir(base);
    let mut entries = Vec::new();
    let Ok(items) = fs::read_dir(&dir) else {
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

/// Every entry last edited in a window on `root` (see
/// [`super::types::RecoveryMetadata::root`]), held or not: for looking at
/// what is there, not for restoring.
pub fn entries_for_root(base: &Path, root: &Path) -> Vec<RecoveryEntry> {
    let canonical = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    all_entries(base)
        .into_iter()
        .filter(|entry| entry.metadata.root.as_deref() == Some(canonical.as_path()))
        .collect()
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

    fn write(store: &mut Store, root: &Path, id: &str, path: Option<&Path>) {
        store
            .own_entry(id)
            .save_recovery_owned(
                id,
                vec![RecoveryChunk::new(0, 0, b"text".to_vec())],
                path,
                None,
                Some(1),
                0,
                4,
                None,
                Some(root),
                None,
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
        write(&mut first, &root, "a", None);

        let second = Store::open(base.path()).unwrap();
        assert!(second.recoverable(None).is_empty());

        drop(first); // a crash: nothing settled, the lock let go
        assert_eq!(ids(&second.recoverable(None)), ["a"]);
    }

    /// Only one editor can take an entry over.
    #[test]
    fn an_entry_is_claimed_once() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let mut gone = Store::open(base.path()).unwrap();
        write(&mut gone, &root, "a", None);
        drop(gone);

        let mut one = Store::open(base.path()).unwrap();
        let mut two = Store::open(base.path()).unwrap();
        let listed = one.recoverable(None);
        assert_eq!(listed.len(), 1);
        let entry = &listed[0];
        assert!(one.claim(entry).unwrap().is_some());
        assert!(two.claim(entry).unwrap().is_none());
        assert!(two.recoverable(None).is_empty());
    }

    /// A clean exit keeps the entries it is told to as anyone's unsaved
    /// work, deletes the rest of its own, and leaves other editors' alone.
    #[test]
    fn a_clean_exit_keeps_what_it_is_told_and_deletes_the_rest_of_its_own() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let mut other = Store::open(base.path()).unwrap();
        write(&mut other, &root, "theirs", None);

        let mut mine = Store::open(base.path()).unwrap();
        write(&mut mine, &root, "kept", None);
        write(&mut mine, &root, "saved", None);
        mine.end(&HashSet::from(["kept".to_string()]));

        let third = Store::open(base.path()).unwrap();
        assert_eq!(ids(&third.recoverable(None)), ["kept"]);
        drop(other);
        assert_eq!(ids(&third.recoverable(None)), ["kept", "theirs"]);
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
        assert!(store.recoverable(None).is_empty(), "read for a root only");
        let listed = store.recoverable(Some((&root, &HashSet::new())));
        assert_eq!(ids(&listed), ["abc"]);
        let claimed = store.claim(&listed[0]).unwrap().unwrap();
        assert_ne!(claimed.id, "abc");
        assert_eq!(claimed.metadata.root.as_ref(), Some(&canonical));
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
                None,
                None,
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
            ids(&store.recoverable(Some((&root, &known)))),
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
        write(&mut gone, &root, "written", None);
        write(&mut gone, &root, "not-written", None);
        gone.own_handle("written").begin_save(&target, 4).unwrap();
        fs::write(&target, "text").unwrap();
        gone.own_handle("not-written")
            .begin_save(&root.join("never.txt"), 4)
            .unwrap();
        drop(gone);

        let store = Store::open(base.path()).unwrap();
        let listed = store.recoverable(None);
        assert_eq!(ids(&listed), ["not-written"]);
        assert!(listed[0].metadata.saving.is_none());
        assert_eq!(ids(&store.recoverable(None)), ["not-written"]);
    }

    /// A claim that fails once the entry is taken — here its metadata turns
    /// out unreadable — puts it back where it was, so the exit, which deletes
    /// what this editor holds but did not keep, can't take it with it. And it
    /// isn't offered again this session.
    #[test]
    fn a_claim_that_fails_puts_the_entry_back() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let mut gone = Store::open(base.path()).unwrap();
        write(&mut gone, &root, "a", None);
        drop(gone);

        let mut store = Store::open(base.path()).unwrap();
        let listed = store.recoverable(None);
        assert_eq!(ids(&listed), ["a"]);
        fs::write(&listed[0].metadata_path, b"torn").unwrap();

        assert!(store.claim(&listed[0]).is_err());
        let entries = entries_dir(base.path());
        assert!(entries.join("a").join("a.meta.json").exists());
        assert!(!store.own_entry_dir("a").exists());
        assert!(store.recoverable(None).is_empty());

        store.end(&HashSet::new());
        assert!(entries.join("a").join("a.meta.json").exists());
    }

    /// From an old launch store, everything is this project's but what is
    /// stamped for a saved workspace on another root: an unnamed `fresh -a`
    /// kept every window's work in its launch directory's store.
    #[test]
    fn an_old_launch_store_keeps_other_projects_work_for_them() {
        let base = TempDir::new().unwrap();
        let root = root(&base);
        let data_dir = base.path().join("data");
        fs::create_dir_all(data_dir.join("workspaces")).unwrap();
        fs::write(
            data_dir.join("workspaces").join("elsewhere.ws-other.json"),
            "{}",
        )
        .unwrap();
        let previous = fresh_editor_core::data_dir::set_data_dir_override(Some(data_dir));

        let canonical = root.canonicalize().unwrap();
        let old = RecoveryStorage::with_dir(
            base.path()
                .join("default")
                .join(crate::workspace::encode_path_for_filename(&canonical)),
        );
        for (id, stamp) in [
            ("here", Some("ws-here")),
            ("unsaved-window", Some("ws-never-saved")),
            ("other-project", Some("ws-other")),
            ("unstamped", None),
        ] {
            old.save_recovery_owned(
                id,
                vec![RecoveryChunk::new(0, 0, b"x".to_vec())],
                None,
                None,
                Some(1),
                0,
                1,
                stamp,
                None,
                None,
            )
            .unwrap();
        }

        let store = Store::open(base.path()).unwrap();
        let known = HashSet::from(["ws-here".to_string()]);
        let listed = ids(&store.recoverable(Some((&root, &known))));
        fresh_editor_core::data_dir::set_data_dir_override(previous);
        assert_eq!(listed, ["here", "unsaved-window", "unstamped"]);
    }
}
