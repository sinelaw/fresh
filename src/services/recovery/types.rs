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

use serde::{Deserialize, Serialize};
use std::path::PathBuf;
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
}

impl RecoveryMetadata {
    /// Current format version
    pub const FORMAT_VERSION: u32 = 2;

    /// Create new metadata
    pub fn new(
        original_path: Option<PathBuf>,
        buffer_name: Option<String>,
        content_size: u64,
        line_count: Option<usize>,
        original_mtime: Option<u64>,
        chunk_count: usize,
        original_file_size: usize,
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

    /// Get a format description for display
    pub fn format_description(&self) -> String {
        if self.original_file_size > 0 {
            format!(
                "{} chunks, {} bytes original",
                self.chunk_count, self.original_file_size
            )
        } else {
            format!("{} bytes", self.content_size)
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

    /// Get the age of this recovery file in seconds
    pub fn age_seconds(&self) -> u64 {
        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        now.saturating_sub(self.metadata.updated_at)
    }

    /// Format the age as a human-readable string
    pub fn age_display(&self) -> String {
        let secs = self.age_seconds();
        if secs < 60 {
            format!("{secs}s ago")
        } else if secs < 3600 {
            format!("{}m ago", secs / 60)
        } else if secs < 86400 {
            format!("{}h ago", secs / 3600)
        } else {
            format!("{}d ago", secs / 86400)
        }
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
fn is_process_running(pid: u32) -> bool {
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
fn is_process_running(pid: u32) -> bool {
    use windows_sys::Win32::Foundation::{CloseHandle, STILL_ACTIVE};
    use windows_sys::Win32::System::Threading::{
        GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
    };

    unsafe {
        let handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, 0, pid);
        if handle == 0 {
            return false;
        }
        let mut exit_code: u32 = 0;
        let result = GetExitCodeProcess(handle, &mut exit_code);
        CloseHandle(handle);
        result != 0 && exit_code == STILL_ACTIVE as u32
    }
}

#[cfg(not(any(unix, windows)))]
fn is_process_running(_pid: u32) -> bool {
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
            1, // chunk_count
            0, // original_file_size
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
        // Our own process should be running (on Unix)
        #[cfg(unix)]
        assert!(is_process_running(std::process::id()));
        // On non-Unix, is_process_running always returns false
        #[cfg(not(unix))]
        assert!(!is_process_running(std::process::id()));
    }

    #[test]
    fn test_is_process_running_invalid() {
        #[cfg(unix)]
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
        #[cfg(not(unix))]
        {
            // On non-Unix platforms, is_process_running always returns false
            assert!(!is_process_running(1));
            assert!(!is_process_running(999999999));
        }
    }
}
