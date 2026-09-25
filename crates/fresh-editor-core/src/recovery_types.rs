//! Recovery data types
//!
//! This module defines the core data structures for the file recovery system.
//!
//! ## Storage Format
//!
//! All recovery data uses a chunked format:
//! - `{id}.meta.json` - Metadata with chunk index
//! - `{id}.chunk.0`, `{id}.chunk.1`, ... - Binary chunk content
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
    /// Metadata for each chunk (content stored in separate .chunk.N files)
    pub chunks: Vec<ChunkMeta>,
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
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub workspace_id: Option<String>,
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

/// Generate a unique ID for an unsaved buffer
pub fn generate_buffer_id() -> String {
    use std::time::SystemTime;
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    format!("unsaved_{:x}", now)
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

    /// [`crate::model::filesystem::host_id`] of the host that process ran
    /// on: the recovery directory may be shared with other hosts, whose pids
    /// mean nothing here. `None` in metadata from before it was recorded,
    /// taken to be this host's.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub host: Option<String>,
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
            host: Some(crate::model::filesystem::host_id().to_string()),
        }
    }

    /// Whether this process wrote it.
    pub fn is_ours(&self) -> bool {
        self.pid == std::process::id() && is_this_host(self.host.as_deref())
    }

    /// Whether the process that wrote it may still be running. One on
    /// another host can't be checked, so it counts as running until its
    /// write is older than [`OTHER_HOST_STALE_AGE`].
    pub fn is_in_progress(&self) -> bool {
        if is_this_host(self.host.as_deref()) {
            return is_process_running(self.pid);
        }
        let started = SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(self.started_at);
        !is_older_than(started, OTHER_HOST_STALE_AGE)
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

/// How old a temp file or in-place write of another host sharing the
/// recovery directory must be before a sweep here takes its process for
/// dead: there is no way to ask that host, and no write takes this long.
pub const OTHER_HOST_STALE_AGE: std::time::Duration =
    std::time::Duration::from_secs(7 * 24 * 60 * 60);

/// Whether `host` (a [`crate::model::filesystem::host_id`], or `None` when
/// the writer didn't record one) is this host.
fn is_this_host(host: Option<&str>) -> bool {
    host.is_none_or(|host| host == crate::model::filesystem::host_id())
}

/// Whether `time` is more than `age` ago.
fn is_older_than(time: SystemTime, age: std::time::Duration) -> bool {
    SystemTime::now()
        .duration_since(time)
        .is_ok_and(|elapsed| elapsed > age)
}

/// Remove the temp files that writes-then-renames into `dir` left behind
/// when their process died between the two steps — names
/// [`crate::model::filesystem::sibling_temp_path`] makes, whose process is no
/// longer running. Nothing else ever deletes them. A temp file of a process
/// still running (another editor sharing the directory, mid-write) and
/// anything that isn't such a temp file — including the `.inplace-*.tmp`
/// copies in-place saves stage, which are recovery data themselves — is
/// left alone. So is one made on another host sharing the directory, whose
/// pid can't be checked here, until it is older than
/// [`OTHER_HOST_STALE_AGE`]. Returns how many were removed.
pub fn remove_orphaned_temp_files(fs: &dyn FileSystem, dir: &Path) -> io::Result<usize> {
    let entries = match fs.read_dir(dir) {
        Ok(entries) => entries,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(0),
        Err(e) => return Err(e),
    };
    let mut removed = 0;
    for entry in entries {
        let Some((pid, host)) = crate::model::filesystem::sibling_temp_owner(&entry.name) else {
            continue;
        };
        let orphaned = if is_this_host(host) {
            !is_process_running(pid)
        } else {
            fs.metadata(&entry.path)
                .ok()
                .and_then(|meta| meta.modified)
                .is_some_and(|modified| is_older_than(modified, OTHER_HOST_STALE_AGE))
        };
        if !orphaned {
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

    /// A host id that isn't this one.
    fn other_host() -> &'static str {
        if crate::model::filesystem::host_id() == "00000000" {
            "11111111"
        } else {
            "00000000"
        }
    }

    /// No process has this pid (it's above any pid_max), so on this host it
    /// "crashed"; on another it may be running.
    const DEAD_PID: u32 = 2_000_000_000;

    /// Issue #3410: the recovery directory may be shared with other hosts
    /// (an NFS home, containers with their own pid namespaces), where a pid
    /// that isn't running here may be a live editor mid-write. Their temp
    /// files are kept until they are too old to be anyone's write; this
    /// host's, and ones from before hosts were recorded, go when their
    /// process is gone.
    #[test]
    fn orphaned_temp_sweep_keeps_other_hosts_temp_files() {
        use crate::model::filesystem::{host_id, sibling_temp_owner, sibling_temp_path};
        let dir = tempfile::TempDir::new().unwrap();
        let fs = crate::model::filesystem::StdFileSystem;
        let file = |name: String| {
            let path = dir.path().join(name);
            std::fs::write(&path, "x").unwrap();
            path
        };
        let ours = file(format!(".a.{DEAD_PID}@{}.1.tmp", host_id()));
        let legacy = file(format!(".a.{DEAD_PID}.2.tmp"));
        let other = file(format!(".a.{DEAD_PID}@{}.3.tmp", other_host()));
        let other_old = file(format!(".a.{DEAD_PID}@{}.4.tmp", other_host()));
        std::fs::File::options()
            .write(true)
            .open(&other_old)
            .unwrap()
            .set_modified(SystemTime::now() - OTHER_HOST_STALE_AGE * 2)
            .unwrap();

        assert_eq!(remove_orphaned_temp_files(&fs, dir.path()).unwrap(), 3);

        assert!(
            other.exists(),
            "another host's recent temp file must be kept"
        );
        for gone in [&ours, &legacy, &other_old] {
            assert!(!gone.exists(), "{gone:?} should have been removed");
        }
        // The temp names this process makes say which host made them.
        let made = sibling_temp_path(&dir.path().join("a"));
        assert_eq!(
            sibling_temp_owner(made.file_name().unwrap().to_str().unwrap()),
            Some((std::process::id(), Some(host_id())))
        );
    }

    /// Issue #3410: an in-place write another host sharing the recovery
    /// directory started counts as in progress (so its staged copy and
    /// metadata are left alone) until it is too old to still be running.
    #[test]
    fn other_hosts_inplace_write_counts_as_in_progress() {
        let mut recovery =
            InplaceWriteRecovery::new(PathBuf::from("/d/f"), PathBuf::from("/r/c"), 0, 0, 0o644);
        recovery.pid = DEAD_PID;
        assert!(!recovery.is_in_progress(), "a dead process on this host");
        recovery.host = None;
        assert!(
            !recovery.is_in_progress(),
            "unrecorded host counts as this one"
        );

        recovery.host = Some(other_host().to_string());
        assert!(recovery.is_in_progress(), "another host's recent write");
        recovery.pid = std::process::id();
        assert!(!recovery.is_ours(), "the same pid on another host");
        recovery.started_at -= OTHER_HOST_STALE_AGE.as_secs() * 2;
        assert!(!recovery.is_in_progress(), "another host's stale write");
    }
}
