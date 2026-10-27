//! Which editor has a file open: an exclusive lock on
//! `recovery/open/<hash of the path>.lock` for as long as one does.
//!
//! A file has one buffer in an editor; these locks carry that across
//! editors. The second editor to open a file gets it read-only until the
//! first lets go, so there is never a second version of it to keep or to
//! save over the first.
//!
//! A lock goes with its process, so a crashed editor holds nothing. The
//! file of a lock let go is removed while still locked; an editor that
//! opened it just before then can end up holding a lock on a removed file
//! beside a new one, both writable as before these locks — narrow, and
//! never worse than that.

use std::collections::HashMap;
use std::fs;
use std::io::{self, Read, Seek, Write};
use std::path::{Path, PathBuf};

const OPEN: &str = "open";

/// Whether this editor may change a file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileLock {
    /// This editor has it: no other one does.
    Ours,
    /// Another running editor has it open: process `pid`, when known.
    Elsewhere { pid: Option<u32> },
}

/// The locks this editor holds on the files it has open.
#[derive(Debug)]
pub struct FileLocks {
    dir: PathBuf,
    held: HashMap<PathBuf, fs::File>,
}

impl FileLocks {
    pub fn new(base: &Path) -> Self {
        Self {
            dir: base.join(OPEN),
            held: HashMap::new(),
        }
    }

    fn lock_path(&self, file: &Path) -> PathBuf {
        self.dir
            .join(format!("{}.lock", super::types::path_hash(file)))
    }

    /// Take the lock on `file` (a canonical local path), or say who has it.
    /// A store where locks can't be taken (no lock support, an unwritable
    /// directory) blocks nobody: the file is this editor's to change.
    pub fn acquire(&mut self, file: &Path) -> FileLock {
        if self.held.contains_key(file) {
            return FileLock::Ours;
        }
        match self.try_acquire(file) {
            Ok(lock) => lock,
            Err(e) => {
                tracing::debug!("No lock on {}: {e}", file.display());
                FileLock::Ours
            }
        }
    }

    fn try_acquire(&mut self, file: &Path) -> io::Result<FileLock> {
        fs::create_dir_all(&self.dir)?;
        let path = self.lock_path(file);
        let mut handle = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&path)?;
        match handle.try_lock() {
            Ok(()) => {
                handle.set_len(0)?;
                handle.rewind()?;
                write!(handle, "{}\n{}", std::process::id(), file.display())?;
                self.held.insert(file.to_path_buf(), handle);
                Ok(FileLock::Ours)
            }
            Err(fs::TryLockError::WouldBlock) => {
                let mut content = String::new();
                handle.read_to_string(&mut content)?;
                let pid = content.lines().next().and_then(|l| l.trim().parse().ok());
                Ok(FileLock::Elsewhere { pid })
            }
            Err(fs::TryLockError::Error(e)) => Err(e),
        }
    }

    /// Let go of the locks on every file not in `open`.
    pub fn keep_only(&mut self, open: &std::collections::HashSet<PathBuf>) {
        let gone: Vec<PathBuf> = self
            .held
            .keys()
            .filter(|file| !open.contains(*file))
            .cloned()
            .collect();
        for file in gone {
            self.release(&file);
        }
    }

    fn release(&mut self, file: &Path) {
        if let Some(handle) = self.held.remove(file) {
            // Removed while still locked, so no editor finds it unlocked and
            // takes it for one nobody holds just as it goes
            if let Err(e) = fs::remove_file(self.lock_path(file)) {
                tracing::debug!("Failed to remove the lock on {}: {e}", file.display());
            }
            drop(handle);
        }
    }

    /// Let go of every lock, at a clean exit.
    pub fn release_all(&mut self) {
        let files: Vec<PathBuf> = self.held.keys().cloned().collect();
        for file in files {
            self.release(&file);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    /// The second editor to ask is told the first has the file, until the
    /// first lets go of it.
    #[test]
    fn a_file_is_one_editors_at_a_time() {
        let base = TempDir::new().unwrap();
        let file = base.path().join("f.txt");
        let mut first = FileLocks::new(base.path());
        let mut second = FileLocks::new(base.path());

        assert_eq!(first.acquire(&file), FileLock::Ours);
        assert_eq!(first.acquire(&file), FileLock::Ours);
        assert_eq!(
            second.acquire(&file),
            FileLock::Elsewhere {
                pid: Some(std::process::id())
            }
        );

        first.keep_only(&std::collections::HashSet::new());
        assert_eq!(second.acquire(&file), FileLock::Ours);
    }

    /// A lock goes with the editor holding it, however it stops.
    #[test]
    fn a_dropped_editors_files_are_free() {
        let base = TempDir::new().unwrap();
        let file = base.path().join("f.txt");
        let mut first = FileLocks::new(base.path());
        assert_eq!(first.acquire(&file), FileLock::Ours);
        drop(first);
        assert_eq!(FileLocks::new(base.path()).acquire(&file), FileLock::Ours);
    }
}
