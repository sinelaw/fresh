//! Recovery data types
//!
//! This module defines the core data structures for the file recovery system.
//!
//! ## Storage Format
//!
//! All recovery data uses a chunked format:
//! - `{id}.meta.json` - Metadata with chunk index
//! - `{id}.chunk.0` - Every chunk's content, one after another (an entry
//!   written before this layout has one `{id}.chunk.N` file per chunk; see
//!   [`ChunkedRecoveryIndex::packed`])
//!
//! For small files or new buffers, there's typically a single chunk containing
//! the full content. For large files, only modified regions are stored as chunks.

use crate::model::filesystem::FileSystem;
use serde::{Deserialize, Serialize};
use std::io;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

/// Maximum chunk size for chunked recovery (1 MB)
pub const MAX_CHUNK_SIZE: usize = 1024 * 1024;

/// Metadata for a single chunk (stored in JSON, without binary content)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChunkMeta {
    /// Byte offset in the original file where this chunk applies
    pub offset: usize,
    /// Original length at this position (bytes replaced/deleted from original)
    pub original_len: usize,
    /// Size of the new content in bytes
    pub size: usize,
}

/// A chunk with its binary content (used in memory, not serialized directly)
#[derive(Debug, Clone)]
pub struct RecoveryChunk {
    /// Byte offset in the original file where this chunk starts
    pub offset: usize,
    /// Original length at this position (for replacement)
    pub original_len: usize,
    /// The modified content
    pub content: Vec<u8>,
}

impl RecoveryChunk {
    /// Create a new recovery chunk
    pub fn new(offset: usize, original_len: usize, content: Vec<u8>) -> Self {
        Self {
            offset,
            original_len,
            content,
        }
    }

    /// Size of the chunk content in bytes
    pub fn size(&self) -> usize {
        self.content.len()
    }

    /// Convert to metadata (without content)
    pub fn to_meta(&self) -> ChunkMeta {
        ChunkMeta {
            offset: self.offset,
            original_len: self.original_len,
            size: self.content.len(),
        }
    }
}

/// Chunked recovery index (stored as JSON in .meta.json for Chunked format)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChunkedRecoveryIndex {
    /// Original file size before modifications
    pub original_size: usize,
    /// Total size after applying all chunks
    pub final_size: usize,
    /// Metadata for each chunk
    pub chunks: Vec<ChunkMeta>,
    /// Whether every chunk's content is in the one `.chunk.0` file, in
    /// order. Earlier versions wrote one `.chunk.N` file per chunk, which a
    /// replace-all in a large file made hundreds of thousands of.
    #[serde(default)]
    pub packed: bool,
}

/// Chunked recovery data with full content (used in memory)
#[derive(Debug, Clone)]
pub struct ChunkedRecoveryData {
    /// Original file size before modifications
    pub original_size: usize,
    /// The modified chunks, sorted by offset
    pub chunks: Vec<RecoveryChunk>,
    /// Total size after applying all chunks
    pub final_size: usize,
}

impl ChunkedRecoveryData {
    /// Create a new ChunkedRecoveryData
    pub fn new(original_size: usize, final_size: usize, chunks: Vec<RecoveryChunk>) -> Self {
        Self {
            original_size,
            chunks,
            final_size,
        }
    }

    /// Convert to index format (metadata only, no content)
    pub fn to_index(&self) -> ChunkedRecoveryIndex {
        ChunkedRecoveryIndex {
            original_size: self.original_size,
            final_size: self.final_size,
            chunks: self.chunks.iter().map(|c| c.to_meta()).collect(),
            packed: true,
        }
    }
}

/// Metadata for a recovery file
///
/// This is stored as JSON alongside the chunk files to track
/// the original file path and timestamps.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecoveryMetadata {
    /// Original file path (None for unsaved buffers)
    pub original_path: Option<PathBuf>,

    /// Buffer name/title for unsaved buffers (e.g., "Untitled-1")
    pub buffer_name: Option<String>,

    /// Unix timestamp when this recovery file was created
    pub created_at: u64,

    /// Unix timestamp when this recovery file was last updated
    pub updated_at: u64,

    /// Total size of chunk content in bytes
    pub content_size: u64,

    /// Line count (if known)
    pub line_count: Option<usize>,

    /// Original file's modification time (to detect external changes)
    pub original_mtime: Option<u64>,

    /// Version of the recovery format (for future compatibility)
    pub format_version: u32,

    /// Number of chunks
    #[serde(default)]
    pub chunk_count: usize,

    /// Original file size (0 for new buffers, needed for reconstruction)
    #[serde(default)]
    pub original_file_size: usize,

    /// Owning workspace (`Window::stable_id`), so crash recovery can restore
    /// into the workspace the buffer came from rather than whichever one is in
    /// front (issue #3189). Not the project root: two workspaces may share a
    /// worktree, which makes the root ambiguous.
    ///
    /// `None` predates the field; readers fall back to path-prefix matching.
    ///
    /// Read from earlier versions' stores only: entries are now filed by
    /// nothing but their content, and placed by the file they name or the
    /// workspace layout that refers to them (see [`Self::root`]).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub workspace_id: Option<String>,

    /// The project root of the window the buffer was last edited in. Not
    /// what finds the entry — its file does, or the layout that refers to
    /// it — but where it is offered when neither does: work whose file no
    /// window has open, or an unnamed buffer no saved layout refers to,
    /// comes back in a window on this root.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub root: Option<PathBuf>,

    /// A save of this buffer that was under way when the entry was last
    /// written: recorded just before the save writes its file, so a crash
    /// on either side of that write can be told apart (see [`SaveIntent`]).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub saving: Option<SaveIntent>,

    /// Whether the buffer is a file on another host (a remote authority's
    /// filesystem). The store is always on this one, so for these the
    /// file's path names nothing here: its mtime is not recorded, and a
    /// local file at the same path is never consulted. Restored only into
    /// a window on a remote authority.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub remote: bool,

    /// For a remote file, which host it is on (the authority's connection,
    /// e.g. `user@host`): the same path on two hosts is two files, and a
    /// file's unsaved work is restored only into a window on its own host.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub host: Option<String>,
}

impl RecoveryMetadata {
    /// Current format version
    pub const FORMAT_VERSION: u32 = 2;

    /// Create new metadata
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        original_path: Option<PathBuf>,
        buffer_name: Option<String>,
        content_size: u64,
        line_count: Option<usize>,
        original_mtime: Option<u64>,
        chunk_count: usize,
        original_file_size: usize,
        workspace_id: Option<String>,
    ) -> Self {
        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        Self {
            original_path,
            buffer_name,
            created_at: now,
            updated_at: now,
            content_size,
            line_count,
            original_mtime,
            format_version: Self::FORMAT_VERSION,
            chunk_count,
            original_file_size,
            workspace_id,
            root: None,
            host: None,
            saving: None,
            remote: false,
        }
    }

    /// Update the timestamp
    pub fn update(&mut self, content_size: u64, line_count: Option<usize>, chunk_count: usize) {
        self.updated_at = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        self.content_size = content_size;
        self.line_count = line_count;
        self.chunk_count = chunk_count;
    }

    /// Get a display name for this recovery entry
    pub fn display_name(&self) -> String {
        if let Some(ref path) = self.original_path {
            path.display().to_string()
        } else if let Some(ref name) = self.buffer_name {
            name.clone()
        } else {
            "Unknown buffer".to_string()
        }
    }
}

/// Session information stored in the lock file
///
/// This is used to detect crashes - if the lock file exists but
/// the process is not running, we know the editor crashed.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionInfo {
    /// Process ID of the editor
    pub pid: u32,

    /// Unix timestamp when the session started
    pub started_at: u64,

    /// Working directory
    pub working_dir: Option<PathBuf>,
}

impl SessionInfo {
    /// Create new session info for the current process
    pub fn new() -> Self {
        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        Self {
            pid: std::process::id(),
            started_at: now,
            working_dir: std::env::current_dir().ok(),
        }
    }

    /// Check if this session is still running
    pub fn is_running(&self) -> bool {
        is_process_running(self.pid)
    }
}

impl Default for SessionInfo {
    fn default() -> Self {
        Self::new()
    }
}

/// A recoverable buffer entry (in-memory representation)
#[derive(Debug, Clone)]
pub struct RecoveryEntry {
    /// Unique identifier (hash of path or generated for new buffers)
    pub id: String,

    /// The metadata
    pub metadata: RecoveryMetadata,

    /// Path to the content file
    pub content_path: PathBuf,

    /// Path to the metadata file
    pub metadata_path: PathBuf,
}

impl RecoveryEntry {
    /// Check if the original file has been modified since recovery was saved.
    /// Returns true if the file was modified (recovery may be invalid).
    /// Returns false if the file is unchanged or doesn't exist.
    pub fn original_file_modified(&self) -> bool {
        if self.metadata.remote {
            return false;
        }
        if let Some(ref path) = self.metadata.original_path {
            if let Some(saved_mtime) = self.metadata.original_mtime {
                if let Ok(metadata) = std::fs::metadata(path) {
                    if let Ok(mtime) = metadata.modified() {
                        let current_mtime = mtime
                            .duration_since(SystemTime::UNIX_EPOCH)
                            .map(|d| d.as_secs())
                            .unwrap_or(0);
                        return current_mtime != saved_mtime;
                    }
                }
            }
        }
        false
    }

    /// Whether a save of this buffer wrote its file after the entry was
    /// last written, so the content is on disk and the entry no longer holds
    /// unsaved work. The entry outlives the save only when deleting it was
    /// cut short (a crash, or a delete still in flight); it must not come
    /// back as an unsaved buffer. See [`SaveIntent`].
    pub fn superseded_by_save(&self) -> bool {
        self.metadata
            .saving
            .as_ref()
            .is_some_and(|intent| intent.outcome() == SaveOutcome::Written)
    }

    /// The handle a buffer restored from this entry keeps.
    pub fn handle(&self) -> HotExitEntry {
        HotExitEntry::new(self.id.clone(), self.metadata_path.clone())
    }
}

/// A file's state as one `stat` sees it: enough to notice that it was
/// written, without reading it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct FileStamp {
    pub size: u64,
    /// Modification time, in nanoseconds since the Unix epoch.
    pub mtime_nanos: u64,
}

impl FileStamp {
    /// The stamp of the local file at `path`: `None` when there is no such
    /// file, `Err` when whether there is cannot be told.
    pub fn of(path: &Path) -> io::Result<Option<Self>> {
        match std::fs::metadata(path) {
            Ok(meta) => Ok(Some(Self {
                size: meta.len(),
                mtime_nanos: meta
                    .modified()?
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .map(|d| d.as_nanos() as u64)
                    .unwrap_or(0),
            })),
            Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
            Err(e) => Err(e),
        }
    }
}

/// A save in progress, recorded in the buffer's hot-exit entry just before
/// the save writes `path`.
///
/// The write is the one step that moves the buffer's content to safety, and
/// it cannot be made atomic with deleting the entry, which is a separate
/// file. So the entry says beforehand what the file looked like and how big
/// the save will make it, and whichever side of the write a crash lands on
/// can be read off the file itself: still as it was, the save never
/// happened and the entry is the unsaved work; changed to the size the save
/// writes, the save wrote it and the entry is stale. One `stat`, never a
/// comparison of content.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SaveIntent {
    /// The file the save writes.
    pub path: PathBuf,
    /// Its stamp before the write; `None` when it did not exist yet.
    pub before: Option<FileStamp>,
    /// How many bytes the save writes.
    pub size: u64,
}

/// What became of a recorded save; see [`SaveIntent::outcome`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SaveOutcome {
    /// The file changed since the save began, to the size the save writes:
    /// the save wrote it.
    Written,
    /// The file is as the save found it (or gone, which a save never
    /// leaves): the save did not happen.
    NotWritten,
    /// The file changed to some other size — an in-place write cut short,
    /// or someone else's change — or can't be looked at. Nothing can be
    /// concluded, so the entry stays the unsaved work.
    Unknown,
}

impl SaveIntent {
    pub fn outcome(&self) -> SaveOutcome {
        match FileStamp::of(&self.path) {
            Ok(Some(now)) if Some(now) == self.before => SaveOutcome::NotWritten,
            Ok(Some(now)) if now.size == self.size => SaveOutcome::Written,
            Ok(Some(_)) => SaveOutcome::Unknown,
            Ok(None) => SaveOutcome::NotWritten,
            Err(_) => SaveOutcome::Unknown,
        }
    }
}

/// Where a buffer's hot-exit entry lives, held by the buffer so its save
/// can record a [`SaveIntent`] there however the save was started.
///
/// An entry backs exactly one buffer (its id is the buffer's, never derived
/// from a path), so whatever the buffer does to its content — a save to its
/// own file or to another one — is that entry's business alone.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HotExitEntry {
    id: String,
    metadata_path: PathBuf,
}

impl HotExitEntry {
    pub fn new(id: impl Into<String>, metadata_path: impl Into<PathBuf>) -> Self {
        Self {
            id: id.into(),
            metadata_path: metadata_path.into(),
        }
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    /// The store directory the entry is in.
    pub fn store_dir(&self) -> Option<&Path> {
        self.metadata_path.parent()
    }

    /// Record that the buffer is about to write `size` bytes to `path`.
    /// Returns whether it was recorded: not when there is no entry.
    pub fn begin_save(&self, path: &Path, size: u64) -> io::Result<bool> {
        let intent = SaveIntent {
            path: path.to_path_buf(),
            before: FileStamp::of(path)?,
            size,
        };
        let intent = serde_json::to_value(&intent).map_err(io::Error::other)?;
        self.set_fields(&[("saving", Some(intent))])
    }

    /// Record the project root the buffer was last edited under (see
    /// [`RecoveryMetadata::root`]), for an entry that has none: one moved
    /// out of an earlier version's store.
    pub fn set_root(&self, root: &Path) -> io::Result<bool> {
        let root = serde_json::to_value(root).map_err(io::Error::other)?;
        self.set_fields(&[("root", Some(root))])
    }

    /// Take back [`Self::begin_save`]: the save failed, and the entry is the
    /// unsaved work again.
    pub fn abandon_save(&self) -> io::Result<bool> {
        self.set_fields(&[("saving", None)])
    }

    /// Rewrite the metadata with each named field set (or removed, for
    /// `None`), leaving the rest of it — the chunk index among it — exactly
    /// as it was. Returns whether the entry exists.
    fn set_fields(&self, changes: &[(&str, Option<serde_json::Value>)]) -> io::Result<bool> {
        let json = match std::fs::read(&self.metadata_path) {
            Ok(json) => json,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(false),
            Err(e) => return Err(e),
        };
        let mut meta: serde_json::Value = serde_json::from_slice(&json)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        let Some(fields) = meta.as_object_mut() else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "recovery metadata is not an object",
            ));
        };
        let mut changed = false;
        for (name, value) in changes {
            let before = fields.get(*name).cloned();
            match value {
                Some(value) => {
                    fields.insert(name.to_string(), value.clone());
                }
                None => {
                    fields.remove(*name);
                }
            }
            changed |= fields.get(*name) != before.as_ref();
        }
        if !changed {
            return Ok(true);
        }
        let json = serde_json::to_vec_pretty(&meta).map_err(io::Error::other)?;
        write_atomically(&self.metadata_path, &json)?;
        Ok(true)
    }
}

/// Replace the local file `target` with `content` by writing a sibling temp
/// file and renaming it over, so a crash leaves the old content or the new.
pub fn write_atomically(target: &Path, content: &[u8]) -> io::Result<()> {
    use std::io::Write;
    // A unique sibling name, never an existing file
    let temp_path = crate::model::filesystem::sibling_temp_path(target);
    let mut file = std::fs::File::options()
        .write(true)
        .create_new(true)
        .open(&temp_path)?;
    let written = file.write_all(content);
    drop(file);
    let result = written.and_then(|()| std::fs::rename(&temp_path, target));
    if result.is_err() {
        // Best-effort cleanup; the write error is what matters
        #[allow(clippy::let_underscore_must_use)]
        let _ = std::fs::remove_file(&temp_path);
    }
    result
}

/// Result of a recovery operation
#[derive(Debug)]
pub enum RecoveryResult {
    /// Successfully recovered the buffer content (full content for new/small buffers)
    Recovered {
        original_path: Option<PathBuf>,
        content: Vec<u8>,
    },
    /// Recovery with chunks to apply on top of original file (for large files)
    /// The caller should open the original file and apply these chunks.
    RecoveredChunks {
        original_path: PathBuf,
        chunks: Vec<RecoveryChunk>,
    },
    /// Original file was modified since recovery was saved
    OriginalFileModified { id: String, original_path: PathBuf },
    /// Content was read, but less of it than the metadata recorded.
    ///
    /// Each recovery file is written with its own atomic rename, so a crash
    /// can leave a chunk that is in place but short. The content is still
    /// worth having — it is the user's work, minus the tail — but it must
    /// never be passed off as the whole buffer: the next save would write
    /// the short version over the real file.
    RecoveredPartial {
        original_path: Option<PathBuf>,
        content: Vec<u8>,
        /// What the metadata says the buffer held.
        expected: usize,
        /// What was actually on disk.
        found: usize,
    },
    /// Recovery file was corrupted
    Corrupted { id: String, reason: String },
    /// Recovery file not found
    NotFound { id: String },
}

/// Check if a process with the given PID is running
#[cfg(unix)]
pub fn is_process_running(pid: u32) -> bool {
    // On Unix, we can use kill with signal 0 to check if process exists
    // Returns 0 if process exists and we can signal it
    // Returns -1 with EPERM if process exists but we can't signal it
    // Returns -1 with ESRCH if process doesn't exist
    let result = unsafe { libc::kill(pid as i32, 0) };
    if result == 0 {
        return true;
    }
    // Check errno - EPERM means process exists but we can't signal it
    let errno = std::io::Error::last_os_error().raw_os_error().unwrap_or(0);
    errno == libc::EPERM
}

#[cfg(windows)]
pub fn is_process_running(pid: u32) -> bool {
    use windows_sys::Win32::Foundation::{CloseHandle, STILL_ACTIVE};
    use windows_sys::Win32::System::Threading::{
        GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
    };

    unsafe {
        let handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, 0, pid);
        if handle.is_null() {
            return false;
        }
        let mut exit_code: u32 = 0;
        let result = GetExitCodeProcess(handle, &mut exit_code);
        CloseHandle(handle);
        result != 0 && exit_code == STILL_ACTIVE as u32
    }
}

#[cfg(not(any(unix, windows)))]
pub fn is_process_running(_pid: u32) -> bool {
    // On other platforms, assume not running (safer for recovery)
    false
}

/// Generate a stable hash for a file path (used as recovery file ID)
pub fn path_hash(path: &std::path::Path) -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(path.to_string_lossy().as_bytes());
    // Use first 16 chars for brevity
    format!("{:x}", hasher.finalize())[..16].to_string()
}

/// Generate a unique id for a buffer's recovery entry.
///
/// Every buffer gets its own, named or not, and the file it is for is
/// recorded inside the entry. An id derived from the path was shared by
/// every buffer with that file open, so one's save or exit could supersede
/// or delete another's unsaved work.
pub fn generate_buffer_id() -> String {
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::SystemTime;
    // Unique within a process even on a clock too coarse to tell two
    // buffers apart; the pid keeps two editors started together apart.
    static SEQ: AtomicU64 = AtomicU64::new(0);
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let seq = SEQ.fetch_add(1, Ordering::Relaxed);
    format!("unsaved_{:x}_{:x}_{:x}", now, std::process::id(), seq)
}

/// Metadata for an in-place write operation that can be recovered after a crash.
///
/// When doing an in-place write (to preserve file ownership), we write the content
/// to a temp file first, then stream it to the destination. If a crash occurs
/// during the streaming phase, the destination file may be corrupted but the
/// temp file contains the good data.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InplaceWriteRecovery {
    /// The destination file path that was being written to
    pub dest_path: PathBuf,

    /// The temp file containing the complete new content
    pub temp_path: PathBuf,

    /// Original file owner (UID) - for restoring ownership after recovery
    #[serde(default)]
    pub uid: u32,

    /// Original file group (GID)
    #[serde(default)]
    pub gid: u32,

    /// Original file permissions (mode)
    #[serde(default)]
    pub mode: u32,

    /// Unix timestamp when the in-place write started
    pub started_at: u64,

    /// Process ID that was performing the write
    pub pid: u32,
}

impl InplaceWriteRecovery {
    /// Create new in-place write recovery metadata
    pub fn new(dest_path: PathBuf, temp_path: PathBuf, uid: u32, gid: u32, mode: u32) -> Self {
        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        Self {
            dest_path,
            temp_path,
            uid,
            gid,
            mode,
            started_at: now,
            pid: std::process::id(),
        }
    }

    /// Whether this process wrote it.
    pub fn is_ours(&self) -> bool {
        self.pid == std::process::id()
    }

    /// Whether no other process may still be writing it: this process wrote
    /// it (saves are synchronous, so none of its own is in flight whenever
    /// anything asks), or the process that did is gone.
    pub fn is_settled(&self) -> bool {
        self.is_ours() || !self.is_in_progress()
    }

    /// Whether the process that wrote it may still be running.
    pub fn is_in_progress(&self) -> bool {
        is_process_running(self.pid)
    }

    /// Where the recovery metadata of an in-place write to `dest_path` is
    /// kept in `recovery_dir`: `<path hash>.inplace.json`, so there is at
    /// most one per destination.
    pub fn meta_path(recovery_dir: &Path, dest_path: &Path) -> PathBuf {
        recovery_dir.join(format!("{}{}", path_hash(dest_path), Self::META_SUFFIX))
    }

    const META_SUFFIX: &'static str = ".inplace.json";

    /// Every in-place write recovery whose metadata is in `recovery_dir`,
    /// with the path of that metadata, oldest first. Metadata that can't be
    /// read or parsed is skipped; a directory that can't be listed has none.
    pub fn scan(fs: &dyn FileSystem, recovery_dir: &Path) -> Vec<(PathBuf, Self)> {
        let Ok(entries) = fs.read_dir(recovery_dir) else {
            return Vec::new();
        };
        let mut found: Vec<(PathBuf, Self)> = entries
            .into_iter()
            .filter(|entry| entry.is_file() && entry.name.ends_with(Self::META_SUFFIX))
            .filter_map(|entry| {
                let json = fs.read_file(&entry.path).ok()?;
                let recovery = serde_json::from_slice::<Self>(&json).ok()?;
                Some((entry.path, recovery))
            })
            .collect();
        found.sort_by_key(|(_, recovery)| recovery.started_at);
        found
    }
}

/// Remove the temp files that writes-then-renames into `dir` left behind
/// when their process died between the two steps — names
/// [`crate::model::filesystem::sibling_temp_path`] makes, whose pid is no
/// longer running. Nothing else ever deletes them. A temp file of a process
/// still running (another editor sharing the directory, mid-write) and
/// anything that isn't such a temp file — including the `.inplace-*.tmp`
/// copies in-place saves stage, which are recovery data themselves — is
/// left alone. Returns how many were removed.
pub fn remove_orphaned_temp_files(fs: &dyn FileSystem, dir: &Path) -> io::Result<usize> {
    let entries = match fs.read_dir(dir) {
        Ok(entries) => entries,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(0),
        Err(e) => return Err(e),
    };
    let mut removed = 0;
    for entry in entries {
        let Some(pid) = crate::model::filesystem::sibling_temp_pid(&entry.name) else {
            continue;
        };
        if is_process_running(pid) {
            continue;
        }
        match fs.remove_file(&entry.path) {
            Ok(()) => removed += 1,
            Err(e) => tracing::debug!(
                "Failed to remove orphaned temp file {}: {}",
                entry.path.display(),
                e
            ),
        }
    }
    Ok(removed)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A hot-exit entry's metadata file as the store writes it: the metadata
    /// fields with the chunk index flattened in beside them.
    fn write_entry(dir: &Path, id: &str) -> HotExitEntry {
        let path = dir.join(format!("{id}.meta.json"));
        let meta = serde_json::json!({
            "original_path": null,
            "buffer_name": null,
            "created_at": 1,
            "updated_at": 1,
            "content_size": 5,
            "line_count": 1,
            "original_mtime": null,
            "format_version": 2,
            "chunk_count": 1,
            "original_file_size": 0,
            "workspace_id": "ws-1",
            "chunked_index": {
                "original_size": 0,
                "final_size": 5,
                "chunks": [{ "offset": 0, "original_len": 0, "size": 5 }]
            }
        });
        std::fs::write(&path, serde_json::to_vec(&meta).unwrap()).unwrap();
        HotExitEntry::new(id, path)
    }

    fn read_json(entry: &HotExitEntry) -> serde_json::Value {
        serde_json::from_slice(&std::fs::read(&entry.metadata_path).unwrap()).unwrap()
    }

    fn read_intent(entry: &HotExitEntry) -> Option<SaveIntent> {
        let meta: RecoveryMetadata = serde_json::from_value(read_json(entry)).unwrap();
        meta.saving
    }

    /// Recording a save touches nothing else in the metadata: the chunk
    /// index lives in the same file, and losing it loses the entry.
    #[test]
    fn begin_and_abandon_save_leave_the_rest_of_the_metadata_alone() {
        let dir = tempfile::TempDir::new().unwrap();
        let entry = write_entry(dir.path(), "unsaved_1");
        let before = read_json(&entry);
        let target = dir.path().join("saved.txt");

        assert!(entry.begin_save(&target, 5).unwrap());
        let mut during = read_json(&entry);
        assert_eq!(
            read_intent(&entry),
            Some(SaveIntent {
                path: target.clone(),
                before: None,
                size: 5,
            })
        );
        during.as_object_mut().unwrap().remove("saving");
        assert_eq!(during, before);

        assert!(entry.abandon_save().unwrap());
        assert_eq!(read_json(&entry), before);
    }

    /// No entry on disk, nothing to record: the save goes ahead as it
    /// would without one.
    #[test]
    fn begin_save_without_an_entry_records_nothing() {
        let dir = tempfile::TempDir::new().unwrap();
        let entry = HotExitEntry::new("gone", dir.path().join("gone.meta.json"));
        assert!(!entry.begin_save(&dir.path().join("f.txt"), 1).unwrap());
        assert!(!dir.path().join("gone.meta.json").exists());
    }

    /// An entry is its one buffer's, so a save to another file (Save As)
    /// records into it like any other save.
    #[test]
    fn a_save_to_another_file_records_in_the_buffers_entry() {
        let dir = tempfile::TempDir::new().unwrap();
        let entry = write_entry(dir.path(), "unsaved_1");
        let renamed = dir.path().join("renamed.txt");

        assert!(entry.begin_save(&renamed, 5).unwrap());
        assert_eq!(read_intent(&entry).map(|i| i.path), Some(renamed));
    }

    /// Ids are unique even when minted back to back.
    #[test]
    fn buffer_ids_are_unique() {
        let ids: std::collections::HashSet<String> =
            (0..1000).map(|_| generate_buffer_id()).collect();
        assert_eq!(ids.len(), 1000);
    }

    /// A crash before the write: the file is as the save found it, so the
    /// save never happened and the entry is still the unsaved work. For a
    /// file that exists, and for a new one (Save As).
    #[test]
    fn a_save_that_never_wrote_reads_as_not_written() {
        let dir = tempfile::TempDir::new().unwrap();
        let existing = dir.path().join("existing.txt");
        std::fs::write(&existing, "old").unwrap();
        let new_file = dir.path().join("new.txt");

        for path in [&existing, &new_file] {
            let intent = SaveIntent {
                path: path.clone(),
                before: FileStamp::of(path).unwrap(),
                size: 11,
            };
            assert_eq!(intent.outcome(), SaveOutcome::NotWritten, "{path:?}");
        }
    }

    /// A crash after the write: the file changed since the save began, so
    /// the save wrote it and the entry is a stale copy.
    #[test]
    fn a_save_that_wrote_reads_as_written() {
        let dir = tempfile::TempDir::new().unwrap();
        let existing = dir.path().join("existing.txt");
        std::fs::write(&existing, "old").unwrap();
        let new_file = dir.path().join("new.txt");

        let intents: Vec<SaveIntent> = [&existing, &new_file]
            .into_iter()
            .map(|path| SaveIntent {
                path: path.clone(),
                before: FileStamp::of(path).unwrap(),
                size: "new content".len() as u64,
            })
            .collect();
        std::fs::write(&existing, "new content").unwrap();
        std::fs::write(&new_file, "new content").unwrap();

        for intent in intents {
            assert_eq!(intent.outcome(), SaveOutcome::Written, "{:?}", intent.path);
        }
    }

    /// A save never removes its file, so a file that is gone says the save
    /// did not write it — the entry stays, rather than being taken for a
    /// copy of a file that isn't there.
    #[test]
    fn a_file_removed_since_the_save_began_reads_as_not_written() {
        let dir = tempfile::TempDir::new().unwrap();
        let path = dir.path().join("f.txt");
        std::fs::write(&path, "old").unwrap();
        let intent = SaveIntent {
            path: path.clone(),
            before: FileStamp::of(&path).unwrap(),
            size: 3,
        };
        std::fs::remove_file(&path).unwrap();
        assert_eq!(intent.outcome(), SaveOutcome::NotWritten);
    }

    /// A file changed to some size other than what the save writes was not
    /// (wholly) written by it: an in-place write cut short leaves it torn,
    /// and someone else may have changed it after a crash. Nothing can be
    /// concluded, so the entry must not be taken for a stale copy.
    #[test]
    fn a_file_changed_to_another_size_reads_as_unknown() {
        let dir = tempfile::TempDir::new().unwrap();
        let path = dir.path().join("f.txt");
        std::fs::write(&path, "old").unwrap();
        let intent = SaveIntent {
            path: path.clone(),
            before: FileStamp::of(&path).unwrap(),
            size: "the whole new content".len() as u64,
        };
        // Torn: the start of the new content, the rest never written
        std::fs::write(&path, "the whole").unwrap();
        assert_eq!(intent.outcome(), SaveOutcome::Unknown);
    }

    #[test]
    fn test_path_hash() {
        let path = std::path::Path::new("/home/user/test.rs");
        let hash = path_hash(path);
        assert_eq!(hash.len(), 16);
        // Same path should produce same hash
        assert_eq!(hash, path_hash(path));
    }

    #[test]
    fn test_recovery_metadata_new() {
        let meta = RecoveryMetadata::new(
            Some(PathBuf::from("/test/file.rs")),
            None,
            100,
            Some(10),
            None,
            1,    // chunk_count
            0,    // original_file_size
            None, // workspace_id
        );
        assert_eq!(meta.format_version, RecoveryMetadata::FORMAT_VERSION);
        assert!(meta.created_at > 0);
        assert_eq!(meta.created_at, meta.updated_at);
        assert_eq!(meta.chunk_count, 1);
        assert_eq!(meta.original_file_size, 0);
    }

    #[test]
    fn test_session_info_new() {
        let info = SessionInfo::new();
        assert_eq!(info.pid, std::process::id());
        assert!(info.started_at > 0);
    }

    #[test]
    fn test_is_process_running_self() {
        // Our own process should be running (on Unix and Windows)
        #[cfg(any(unix, windows))]
        assert!(is_process_running(std::process::id()));
        // On other platforms, is_process_running always returns false
        #[cfg(not(any(unix, windows)))]
        assert!(!is_process_running(std::process::id()));
    }

    #[test]
    fn test_is_process_running_invalid() {
        #[cfg(any(unix, windows))]
        {
            // Test with a PID that definitely doesn't exist
            // Find a PID that's not running by searching high PIDs
            let mut test_pid = 999999u32;
            while is_process_running(test_pid) && test_pid > 2 {
                test_pid -= 1;
            }
            if test_pid > 2 {
                assert!(!is_process_running(test_pid));
            }
        }
        #[cfg(not(any(unix, windows)))]
        {
            // On other platforms, is_process_running always returns false
            assert!(!is_process_running(1));
            assert!(!is_process_running(999999999));
        }
    }
}
