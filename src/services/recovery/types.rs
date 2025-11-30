//! Recovery data types
//!
//! This module defines the core data structures for the file recovery system.
//!
//! ## Recovery Formats
//!
//! The system supports two recovery formats:
//! - **Full**: Stores the entire buffer content (for small files)
//! - **Chunked**: Stores only modified chunks with byte offsets (for large files)
//!
//! The chunked format is essential for multi-gigabyte files where saving
//! the entire content would be prohibitive.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::time::SystemTime;

/// Threshold for switching to chunked recovery (10 MB)
pub const CHUNKED_RECOVERY_THRESHOLD: usize = 10 * 1024 * 1024;

/// Maximum chunk size for chunked recovery (1 MB)
pub const MAX_CHUNK_SIZE: usize = 1024 * 1024;

/// Recovery format type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RecoveryFormat {
    /// Full content stored in single file
    Full,
    /// Only modified chunks stored with positions
    Chunked,
}

impl Default for RecoveryFormat {
    fn default() -> Self {
        Self::Full
    }
}

/// A modified chunk for chunked recovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecoveryChunk {
    /// Byte offset in the original file where this chunk starts
    pub offset: usize,
    /// Original length at this position (for replacement)
    pub original_len: usize,
    /// The modified content
    pub content: Vec<u8>,
    /// Checksum of this chunk
    pub checksum: String,
}

impl RecoveryChunk {
    /// Create a new recovery chunk
    pub fn new(offset: usize, original_len: usize, content: Vec<u8>) -> Self {
        let checksum = compute_checksum(&content);
        Self {
            offset,
            original_len,
            content,
            checksum,
        }
    }

    /// Verify the chunk's checksum
    pub fn verify(&self) -> bool {
        compute_checksum(&self.content) == self.checksum
    }

    /// Size of the chunk content in bytes
    pub fn size(&self) -> usize {
        self.content.len()
    }
}

/// Chunked recovery data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChunkedRecoveryData {
    /// Original file size before modifications
    pub original_size: usize,
    /// The modified chunks, sorted by offset
    pub chunks: Vec<RecoveryChunk>,
    /// Total size after applying all chunks
    pub final_size: usize,
}

/// Metadata for a recovery file
///
/// This is stored as JSON alongside the content file to track
/// the original file path, timestamps, and content checksum.
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

    /// SHA-256 checksum of the content file for integrity verification
    pub checksum: String,

    /// Size of the content in bytes (for Full format) or total chunks size (for Chunked)
    pub content_size: u64,

    /// Line count (if known)
    pub line_count: Option<usize>,

    /// Original file's modification time (to detect external changes)
    pub original_mtime: Option<u64>,

    /// Version of the recovery format (for future compatibility)
    pub format_version: u32,

    /// Recovery format (Full or Chunked)
    #[serde(default)]
    pub format: RecoveryFormat,

    /// For chunked format: number of chunks
    #[serde(default)]
    pub chunk_count: Option<usize>,

    /// For chunked format: original file size (needed for reconstruction)
    #[serde(default)]
    pub original_file_size: Option<usize>,
}

impl RecoveryMetadata {
    /// Current format version
    pub const FORMAT_VERSION: u32 = 1;

    /// Create new metadata for a buffer (Full format)
    pub fn new(
        original_path: Option<PathBuf>,
        buffer_name: Option<String>,
        checksum: String,
        content_size: u64,
        line_count: Option<usize>,
        original_mtime: Option<u64>,
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
            checksum,
            content_size,
            line_count,
            original_mtime,
            format_version: Self::FORMAT_VERSION,
            format: RecoveryFormat::Full,
            chunk_count: None,
            original_file_size: None,
        }
    }

    /// Create new metadata for chunked recovery (large files)
    pub fn new_chunked(
        original_path: Option<PathBuf>,
        buffer_name: Option<String>,
        checksum: String,
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
            checksum,
            content_size,
            line_count,
            original_mtime,
            format_version: Self::FORMAT_VERSION,
            format: RecoveryFormat::Chunked,
            chunk_count: Some(chunk_count),
            original_file_size: Some(original_file_size),
        }
    }

    /// Update the timestamp and checksum (for Full format)
    pub fn update(&mut self, checksum: String, content_size: u64, line_count: Option<usize>) {
        self.updated_at = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        self.checksum = checksum;
        self.content_size = content_size;
        self.line_count = line_count;
    }

    /// Update for chunked format
    pub fn update_chunked(
        &mut self,
        checksum: String,
        content_size: u64,
        line_count: Option<usize>,
        chunk_count: usize,
    ) {
        self.update(checksum, content_size, line_count);
        self.chunk_count = Some(chunk_count);
    }

    /// Check if this is a chunked recovery
    pub fn is_chunked(&self) -> bool {
        self.format == RecoveryFormat::Chunked
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
        match self.format {
            RecoveryFormat::Full => format!("{} bytes", self.content_size),
            RecoveryFormat::Chunked => {
                let chunks = self.chunk_count.unwrap_or(0);
                let orig = self.original_file_size.unwrap_or(0);
                format!("{} chunks, {} bytes original", chunks, orig)
            }
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
    /// Load content from the recovery file
    pub fn load_content(&self) -> std::io::Result<Vec<u8>> {
        std::fs::read(&self.content_path)
    }

    /// Verify the content checksum matches
    pub fn verify_checksum(&self) -> std::io::Result<bool> {
        let content = self.load_content()?;
        let actual_checksum = compute_checksum(&content);
        Ok(actual_checksum == self.metadata.checksum)
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
    /// Successfully recovered the buffer content
    Recovered {
        original_path: Option<PathBuf>,
        content: Vec<u8>,
    },
    /// Recovery file was corrupted (checksum mismatch)
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

#[cfg(not(unix))]
fn is_process_running(_pid: u32) -> bool {
    // On non-Unix platforms, we can't easily check if a process is running
    // without platform-specific APIs. For safety in recovery scenarios,
    // we assume the process is not running (which will prompt recovery).
    // This is the safer default - it may offer unnecessary recovery prompts
    // but won't lose data.
    false
}

/// Compute SHA-256 checksum of data
pub fn compute_checksum(data: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("{:x}", hasher.finalize())
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
    fn test_compute_checksum() {
        let data = b"Hello, World!";
        let checksum = compute_checksum(data);
        assert!(!checksum.is_empty());
        // SHA-256 produces 64 hex chars
        assert_eq!(checksum.len(), 64);
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
            "abc123".to_string(),
            100,
            Some(10),
            None,
        );
        assert_eq!(meta.format_version, RecoveryMetadata::FORMAT_VERSION);
        assert!(meta.created_at > 0);
        assert_eq!(meta.created_at, meta.updated_at);
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
