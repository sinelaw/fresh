//! The spool file behind `… | fresh -`, owned by a handle rather than a name.
//!
//! Piped stdin is drained into a scratch file so a large input can be paged
//! in lazily: the buffer keeps `Unloaded` chunks that are read back on
//! demand, for as long as the buffer lives. That is why the file cannot be
//! deleted when the buffer opens — it has to outlive it.
//!
//! It used to stay *named* for the same reason, which made deleting it the
//! process's job and put an exit guard, two signal handlers and a panic hook
//! in the way of getting it right — none of which could cover a `SIGKILL`, a
//! segfault or the machine losing power (#3134).
//!
//! So the name goes immediately and the descriptor stays. The kernel frees
//! the inode when the last descriptor closes, however the process dies, and
//! there is no cleanup path left to get wrong.
//!
//! The one thing that costs is reading it back: the chunk loader is
//! path-based, and there is no name any more. [`SpoolFileSystem`] closes that
//! gap — a `FileSystem` decorator that recognises this spool's opaque path
//! and serves it from the retained handle, delegating everything else. Reads
//! are *positional* (`pread`/`seek_read`), which is what makes overlapping
//! chunk loads safe: there is no shared cursor to race over. That also keeps
//! the design off per-platform magic paths — `/proc/self/fd` needs procfs
//! mounted and does not exist on macOS or Windows, and macOS documents
//! `/dev/fd/N` as equivalent to `dup(N)`, whose shared offset is exactly the
//! race positional reads avoid.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fresh_editor_core::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchCursor, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch,
};

#[cfg(unix)]
mod imp;
#[cfg(windows)]
#[path = "stdin_spool/imp_windows.rs"]
mod imp;

/// A drained-stdin scratch file that exists only as an open descriptor.
///
/// Cloneable by `Arc` rather than by handle: the buffer, the streaming poll
/// and the decorator all refer to one spool.
#[derive(Debug)]
pub struct StdinSpool {
    /// The read side, used only for positional reads.
    handle: imp::SpoolHandle,
    /// The opaque path recorded in the buffer's `Unloaded` chunks.
    ///
    /// Not a filesystem path — the file has no name. It is a key that
    /// [`SpoolFileSystem`] recognises, kept as a `PathBuf` because that is
    /// what a chunk stores. It is never shown to the user: `open_stdin_buffer`
    /// clears the buffer's file path so the buffer reads as unnamed.
    path: PathBuf,
}

impl StdinSpool {
    /// Create the spool and hand back the write side for the drain thread.
    ///
    /// The file is nameless from the moment it exists — on Unix it is created
    /// and unlinked before anything else can see it; on Windows it is opened
    /// delete-on-close. Either way there is nothing left to clean up.
    pub fn create() -> io::Result<(Self, imp::SpoolWriter)> {
        let (handle, writer) = imp::create()?;
        let spool = Self {
            handle,
            // The pid keeps it distinguishable in logs, matching the old
            // `fresh-stdin-<pid>.tmp` naming it replaces.
            path: PathBuf::from(format!("fresh-stdin://{}", std::process::id())),
        };
        Ok((spool, writer))
    }

    /// A spool already holding `contents`, with nothing left to stream.
    ///
    /// The drain thread is what fills a real spool; this is for the callers
    /// that already have the bytes — tests, and any future path that reads
    /// its input eagerly.
    pub fn with_contents(contents: &[u8]) -> io::Result<Self> {
        use std::io::Write;
        let (spool, mut writer) = Self::create()?;
        writer.write_all(contents)?;
        writer.flush()?;
        Ok(spool)
    }

    /// The key to record in the buffer's chunks.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Whether `path` is this spool's key.
    fn owns(&self, path: &Path) -> bool {
        path == self.path
    }

    /// Bytes at `offset`, without touching any shared cursor.
    ///
    /// Short reads are returned as-is; the caller (`read_range`) decides
    /// whether it got everything it asked for, exactly as a file read would.
    fn read_at(&self, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.handle.read_at(offset, len)
    }

    /// How much has been drained so far. Grows while stdin is still
    /// streaming, which is how the poll notices there is more to show.
    fn len(&self) -> io::Result<u64> {
        self.handle.len()
    }
}

/// Wrap `inner` so this spool's path resolves to its handle.
///
/// Everything else is delegated untouched, so the editor keeps whatever
/// filesystem it already had — including a remote one, which the spool
/// deliberately does not follow: the file is local to this process, and a
/// stdin buffer opened under an SSH authority must still read from here.
pub fn wrap(
    inner: Arc<dyn FileSystem + Send + Sync>,
    spool: Arc<StdinSpool>,
) -> Arc<dyn FileSystem + Send + Sync> {
    Arc::new(SpoolFileSystem { inner, spool })
}

/// A `FileSystem` that answers for one spool and forwards the rest.
struct SpoolFileSystem {
    inner: Arc<dyn FileSystem + Send + Sync>,
    spool: Arc<StdinSpool>,
}

impl std::fmt::Debug for SpoolFileSystem {
    /// Hand-written because `dyn FileSystem` is not `Debug`; the spool's key
    /// is the part worth seeing in a log line anyway.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SpoolFileSystem")
            .field("spool", &self.spool.path)
            .finish_non_exhaustive()
    }
}

impl FileSystem for SpoolFileSystem {
    // --- the two the spool actually answers ------------------------------

    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        if self.spool.owns(path) {
            let data = self.spool.read_at(offset, len)?;
            if data.len() < len {
                // Matches `File::read_exact`, which is what the local
                // filesystem uses: asking for a range past the end is an
                // error rather than a silent truncation.
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "stdin spool: read past the end of the drained region",
                ));
            }
            return Ok(data);
        }
        self.inner.read_range(path, offset, len)
    }

    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        if self.spool.owns(path) {
            return Ok(FileMetadata::new(self.spool.len()?));
        }
        self.inner.metadata(path)
    }

    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        if self.spool.owns(path) {
            // An input below the large-file threshold is loaded whole rather
            // than lazily, so this is the common path for `echo x | fresh -`
            // and not an edge case.
            let len = self.spool.len()?;
            return self.spool.read_at(0, len as usize);
        }
        self.inner.read_file(path)
    }

    // --- everything else is the wrapped filesystem's business -------------

    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.inner.write_file(path, data)
    }
    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_file(path)
    }
    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        self.inner.open_file(path)
    }
    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_write(path)
    }
    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_append(path)
    }
    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
        self.inner.set_file_length(path, len)
    }
    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.inner.rename(from, to)
    }
    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        self.inner.copy(from, to)
    }
    fn remove_file(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_file(path)
    }
    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_dir(path)
    }
    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.symlink_metadata(path)
    }
    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_dir(path)
    }
    fn is_file(&self, path: &Path) -> io::Result<bool> {
        if self.spool.owns(path) {
            return Ok(true);
        }
        self.inner.is_file(path)
    }
    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
        self.inner.set_permissions(path, permissions)
    }
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        self.inner.read_dir(path)
    }
    fn create_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir(path)
    }
    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir_all(path)
    }
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        if self.spool.owns(path) {
            // The spool has no name to resolve; its key *is* its identity.
            return Ok(path.to_path_buf());
        }
        self.inner.canonicalize(path)
    }
    fn current_uid(&self) -> u32 {
        self.inner.current_uid()
    }
    fn search_file(
        &self,
        path: &Path,
        pattern: &str,
        opts: &FileSearchOptions,
        cursor: &mut FileSearchCursor,
    ) -> io::Result<Vec<SearchMatch>> {
        self.inner.search_file(path, pattern, opts, cursor)
    }
    fn sudo_write(
        &self,
        path: &Path,
        data: &[u8],
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> io::Result<()> {
        self.inner.sudo_write(path, data, mode, uid, gid)
    }
    fn walk_files(
        &self,
        root: &Path,
        skip_dirs: &[&str],
        cancel: &std::sync::atomic::AtomicBool,
        on_file: &mut dyn FnMut(&Path, &str) -> bool,
    ) -> io::Result<()> {
        self.inner.walk_files(root, skip_dirs, cancel, on_file)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn spool_with(contents: &[u8]) -> Arc<StdinSpool> {
        let (spool, mut writer) = StdinSpool::create().expect("create spool");
        writer.write_all(contents).expect("write spool contents");
        writer.flush().expect("flush spool");
        Arc::new(spool)
    }

    #[test]
    fn reads_are_positional_and_repeatable() {
        let spool = spool_with(b"0123456789abcdef");

        assert_eq!(spool.read_at(10, 6).unwrap(), b"abcdef");
        assert_eq!(spool.read_at(0, 4).unwrap(), b"0123");
        // Again, out of order: nothing carries a cursor between calls.
        assert_eq!(spool.read_at(10, 6).unwrap(), b"abcdef");
    }

    #[test]
    fn length_tracks_what_has_been_drained() {
        let (spool, mut writer) = StdinSpool::create().unwrap();
        assert_eq!(spool.len().unwrap(), 0);

        writer.write_all(b"first").unwrap();
        writer.flush().unwrap();
        assert_eq!(spool.len().unwrap(), 5);

        writer.write_all(b"-second").unwrap();
        writer.flush().unwrap();
        assert_eq!(spool.len().unwrap(), 12);
        assert_eq!(spool.read_at(5, 7).unwrap(), b"-second");
    }

    #[test]
    fn the_decorator_answers_for_the_spool_and_forwards_the_rest() {
        let spool = spool_with(b"spooled stdin");
        let spool_path = spool.path().to_path_buf();
        let dir = tempfile::tempdir().unwrap();
        let real = dir.path().join("on-disk.txt");
        std::fs::write(&real, b"a real file").unwrap();

        let fs = wrap(
            Arc::new(fresh_editor_core::model::filesystem::StdFileSystem),
            Arc::clone(&spool),
        );

        // The spool resolves without existing on disk...
        assert_eq!(fs.read_range(&spool_path, 8, 5).unwrap(), b"stdin");
        assert_eq!(fs.metadata(&spool_path).unwrap().size, 13);
        // ...and the wrapped filesystem still does its own job.
        assert_eq!(fs.read_range(&real, 2, 4).unwrap(), b"real");
        assert_eq!(fs.read_file(&real).unwrap(), b"a real file");
    }

    #[test]
    fn a_read_past_the_drained_region_is_an_error_not_a_short_read() {
        let spool = spool_with(b"short");
        let fs = wrap(
            Arc::new(fresh_editor_core::model::filesystem::StdFileSystem),
            Arc::clone(&spool),
        );

        let err = fs
            .read_range(spool.path(), 0, 4096)
            .expect_err("reading past the end should fail like read_exact does");
        assert_eq!(err.kind(), io::ErrorKind::UnexpectedEof);
    }

    /// The point of the whole module: there is no name, so there is nothing
    /// for anyone — including a crash — to leave behind.
    #[test]
    fn the_spool_has_no_name_on_disk() {
        let spool = spool_with(b"nameless");
        let path = spool.path();

        assert!(
            !path.exists(),
            "the spool key must not name a real file: {path:?}"
        );
        assert!(
            !std::env::temp_dir()
                .join(format!("fresh-stdin-{}.tmp", std::process::id()))
                .exists(),
            "the old named spool file must not be created any more"
        );
    }
}
