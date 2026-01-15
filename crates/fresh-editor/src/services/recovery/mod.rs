//! File Recovery Service
//!
//! This module provides Emacs-style file recovery for the Fresh editor.
//! It automatically saves buffer contents periodically and can recover
//! them if the editor crashes.
//!
//! ## How it works
//!
//! 1. **Session Lock**: On startup, creates a lock file with the process ID
//! 2. **Auto-Save**: Periodically saves modified buffers to recovery directory
//! 3. **Crash Detection**: On startup, checks if lock file exists without running process
//! 4. **Recovery**: If crash detected, offers to recover unsaved changes
//!
//! ## File Layout
//!
//! ```text
//! ~/.local/share/fresh/recovery/
//! ├── session.lock           # Session info (PID, start time)
//! ├── {hash}.meta.json       # Recovery metadata with chunk index
//! ├── {hash}.chunk.0         # Chunk 0 binary content
//! ├── {hash}.chunk.1         # Chunk 1 binary content
//! └── ...
//! ```
//!
//! ## Storage Format
//!
//! All recovery data uses a chunked format:
//! - For small files/new buffers: single chunk containing full content
//! - For large files: only modified regions stored as chunks
//!
//! ## Usage
//!
//! ```rust,ignore
//! use fresh::services::recovery::RecoveryService;
//!
//! // On startup
//! let mut recovery = RecoveryService::new()?;
//! if recovery.should_offer_recovery()? {
//!     let entries = recovery.list_recoverable()?;
//!     // Show recovery prompt to user
//! }
//! recovery.start_session()?;
//!
//! // During editing (call periodically)
//! // For small files/new buffers:
//! recovery.save_buffer("id", chunks, Some(&path), None, Some(10), 0, content_len)?;
//!
//! // On clean shutdown
//! recovery.end_session()?;
//! ```

mod storage;
pub mod types;

pub use storage::RecoveryStorage;
pub use types::{
    generate_buffer_id, path_hash, ChunkMeta, ChunkedRecoveryData, ChunkedRecoveryIndex,
    RecoveryChunk, RecoveryEntry, RecoveryMetadata, RecoveryResult, SessionInfo, MAX_CHUNK_SIZE,
};

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

/// Configuration for the recovery service
#[derive(Debug, Clone)]
pub struct RecoveryConfig {
    /// Whether recovery is enabled
    pub enabled: bool,
    /// Auto-save interval in seconds
    pub auto_save_interval_secs: u32,
    /// Maximum age of recovery files before cleanup (in seconds)
    pub max_recovery_age_secs: u64,
}

impl Default for RecoveryConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            auto_save_interval_secs: 2,
            max_recovery_age_secs: 7 * 24 * 60 * 60, // 7 days
        }
    }
}

/// The main recovery service
///
/// This is the high-level interface for the recovery system.
/// It manages the session lock and coordinates buffer recovery.
#[derive(Debug)]
pub struct RecoveryService {
    /// Storage backend
    storage: RecoveryStorage,
    /// Configuration
    config: RecoveryConfig,
    /// Last auto-save time per buffer
    last_save_times: HashMap<String, Instant>,
    /// Session started flag
    session_started: bool,
}

impl RecoveryService {
    /// Create a new recovery service
    pub fn new() -> io::Result<Self> {
        Ok(Self {
            storage: RecoveryStorage::new()?,
            config: RecoveryConfig::default(),
            last_save_times: HashMap::new(),
            session_started: false,
        })
    }

    /// Create a new recovery service with custom config
    pub fn with_config(config: RecoveryConfig) -> io::Result<Self> {
        Ok(Self {
            storage: RecoveryStorage::new()?,
            config,
            last_save_times: HashMap::new(),
            session_started: false,
        })
    }

    /// Create a new recovery service with a custom storage directory
    /// This is useful for testing with isolated temporary directories
    pub fn with_storage_dir(storage_dir: PathBuf) -> Self {
        Self {
            storage: RecoveryStorage::with_dir(storage_dir),
            config: RecoveryConfig::default(),
            last_save_times: HashMap::new(),
            session_started: false,
        }
    }

    /// Create a new recovery service with custom config and storage directory
    pub fn with_config_and_dir(config: RecoveryConfig, storage_dir: PathBuf) -> Self {
        Self {
            storage: RecoveryStorage::with_dir(storage_dir),
            config,
            last_save_times: HashMap::new(),
            session_started: false,
        }
    }

    /// Check if recovery is enabled
    pub fn is_enabled(&self) -> bool {
        self.config.enabled
    }

    /// Get the storage backend
    pub fn storage(&self) -> &RecoveryStorage {
        &self.storage
    }

    // ========================================================================
    // Session management
    // ========================================================================

    /// Check if we should offer recovery (crash detected)
    pub fn should_offer_recovery(&self) -> io::Result<bool> {
        if !self.config.enabled {
            return Ok(false);
        }

        // Check for crash
        if self.storage.detect_crash()? {
            // Also check if there are any recovery files
            let entries = self.storage.list_entries()?;
            return Ok(!entries.is_empty());
        }

        Ok(false)
    }

    /// Start a new session (call on editor startup after recovery handling)
    pub fn start_session(&mut self) -> io::Result<()> {
        if !self.config.enabled {
            return Ok(());
        }

        self.storage.create_session_lock()?;
        self.session_started = true;
        tracing::info!("Recovery session started");
        Ok(())
    }

    /// End the session cleanly (call on normal editor shutdown)
    pub fn end_session(&mut self) -> io::Result<()> {
        if !self.config.enabled || !self.session_started {
            return Ok(());
        }

        // Clean up all recovery files (user chose to close normally)
        let cleaned = self.storage.cleanup_all()?;
        tracing::info!("Cleaned up {} recovery files", cleaned);

        // Remove session lock
        self.storage.remove_session_lock()?;
        self.session_started = false;
        tracing::info!("Recovery session ended");
        Ok(())
    }

    /// Update session heartbeat (call periodically)
    pub fn heartbeat(&self) -> io::Result<()> {
        if self.config.enabled && self.session_started {
            self.storage.update_session_lock()?;
        }
        Ok(())
    }

    // ========================================================================
    // Buffer tracking
    // ========================================================================

    /// Check if a buffer needs auto-save
    ///
    /// Returns true if recovery_pending is true AND enough time has passed since
    /// the last recovery save. The recovery_pending flag is now tracked on the
    /// buffer itself (TextBuffer.recovery_pending) rather than in this service.
    pub fn needs_auto_save(&self, buffer_id: &str, recovery_pending: bool) -> bool {
        if !self.config.enabled {
            return false;
        }

        // Must have pending recovery changes to need auto-save
        if !recovery_pending {
            return false;
        }

        let interval = Duration::from_secs(self.config.auto_save_interval_secs as u64);
        match self.last_save_times.get(buffer_id) {
            Some(last_time) => last_time.elapsed() >= interval,
            None => true, // Never saved, needs save
        }
    }

    /// Get buffer ID for a path
    pub fn get_buffer_id(&self, path: Option<&Path>) -> String {
        self.storage.get_buffer_id(path)
    }

    // ========================================================================
    // Recovery operations
    // ========================================================================

    /// Save a buffer's content for recovery
    ///
    /// All recovery uses the chunked format:
    /// - For small files/new buffers: pass a single chunk containing full content
    ///   with offset=0, original_len=0, original_file_size=0
    /// - For large files: pass only the modified chunks with their offsets
    ///
    /// ## Parameters
    ///
    /// - `buffer_id`: Unique identifier for the buffer
    /// - `chunks`: The content chunks to save
    /// - `original_path`: Path to the original file (None for new buffers)
    /// - `buffer_name`: Display name for the buffer
    /// - `line_count`: Number of lines in the buffer
    /// - `original_file_size`: Size of the original file (0 for new buffers)
    /// - `final_size`: Total size after applying all modifications
    #[allow(clippy::too_many_arguments)]
    pub fn save_buffer(
        &mut self,
        buffer_id: &str,
        chunks: Vec<RecoveryChunk>,
        original_path: Option<&Path>,
        buffer_name: Option<&str>,
        line_count: Option<usize>,
        original_file_size: usize,
        final_size: usize,
    ) -> io::Result<()> {
        if !self.config.enabled {
            return Ok(());
        }

        self.storage.save_recovery(
            buffer_id,
            chunks,
            original_path,
            buffer_name,
            line_count,
            original_file_size,
            final_size,
        )?;
        self.last_save_times
            .insert(buffer_id.to_string(), Instant::now());

        tracing::trace!(
            "Saved recovery for buffer {} (original: {} bytes, final: {} bytes)",
            buffer_id,
            original_file_size,
            final_size
        );
        Ok(())
    }

    /// Delete recovery for a buffer (call when buffer is saved normally or closed)
    pub fn delete_buffer_recovery(&mut self, buffer_id: &str) -> io::Result<()> {
        if !self.config.enabled {
            return Ok(());
        }

        self.storage.delete_recovery(buffer_id)?;
        self.last_save_times.remove(buffer_id);

        tracing::debug!("Deleted recovery for buffer {}", buffer_id);
        Ok(())
    }

    /// List all recoverable entries
    pub fn list_recoverable(&self) -> io::Result<Vec<RecoveryEntry>> {
        self.storage.list_entries()
    }

    /// Load recovery content for a specific entry
    ///
    /// For entries with original_file_size > 0, returns RecoveredChunks so the caller
    /// can apply chunks directly to the buffer (more efficient than full reconstruction).
    /// For new buffer entries (original_file_size == 0), the full content is in the chunks.
    pub fn load_recovery(&self, entry: &RecoveryEntry) -> io::Result<RecoveryResult> {
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

                if !original_path.exists() {
                    return Ok(RecoveryResult::Corrupted {
                        id: entry.id.clone(),
                        reason: format!(
                            "Original file not found: {}. Recovery requires the original file.",
                            original_path.display()
                        ),
                    });
                }

                // Load chunks and return them for direct application
                let chunked_data =
                    self.storage
                        .read_chunked_content(&entry.id)?
                        .ok_or_else(|| {
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
        // Load the chunk data directly
        let chunked_data = self
            .storage
            .read_chunked_content(&entry.id)?
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Chunk content not found"))?;

        // For original_file_size == 0, we expect exactly one chunk with offset=0
        if chunked_data.chunks.len() == 1 && chunked_data.chunks[0].offset == 0 {
            Ok(RecoveryResult::Recovered {
                original_path: entry.metadata.original_path.clone(),
                content: chunked_data.chunks[0].content.clone(),
            })
        } else {
            Ok(RecoveryResult::Corrupted {
                id: entry.id.clone(),
                reason: "Invalid recovery format: expected single chunk for new buffer".to_string(),
            })
        }
    }

    /// Load recovery with a provided original file path
    ///
    /// Use this when the original file has moved or you want to specify a different source.
    pub fn load_recovery_with_original(
        &self,
        entry: &RecoveryEntry,
        original_file: &Path,
    ) -> io::Result<RecoveryResult> {
        let content = self
            .storage
            .reconstruct_from_chunks(&entry.id, original_file)?;
        Ok(RecoveryResult::Recovered {
            original_path: Some(original_file.to_path_buf()),
            content,
        })
    }

    /// Accept recovery for an entry (load and delete recovery file)
    pub fn accept_recovery(&mut self, entry: &RecoveryEntry) -> io::Result<RecoveryResult> {
        let result = self.load_recovery(entry)?;
        // Delete the recovery file after successful load
        if matches!(result, RecoveryResult::Recovered { .. }) {
            self.storage.delete_recovery(&entry.id)?;
        }
        Ok(result)
    }

    /// Discard recovery for an entry
    pub fn discard_recovery(&mut self, entry: &RecoveryEntry) -> io::Result<()> {
        self.storage.delete_recovery(&entry.id)
    }

    /// Discard all recovery files
    pub fn discard_all_recovery(&mut self) -> io::Result<usize> {
        self.storage.cleanup_all()
    }

    // ========================================================================
    // Maintenance
    // ========================================================================

    /// Clean up old recovery files (older than max_recovery_age_secs)
    pub fn cleanup_old(&self) -> io::Result<usize> {
        if !self.config.enabled {
            return Ok(0);
        }

        let entries = self.storage.list_entries()?;
        let mut cleaned = 0;

        for entry in entries {
            if entry.age_seconds() > self.config.max_recovery_age_secs
                && self.storage.delete_recovery(&entry.id).is_ok()
            {
                cleaned += 1;
            }
        }

        if cleaned > 0 {
            tracing::info!("Cleaned up {} old recovery files", cleaned);
        }

        Ok(cleaned)
    }

    /// Clean up orphaned files
    pub fn cleanup_orphans(&self) -> io::Result<usize> {
        self.storage.cleanup_orphans()
    }
}

impl Default for RecoveryService {
    fn default() -> Self {
        Self::new().unwrap_or_else(|_| Self {
            storage: RecoveryStorage::default(),
            config: RecoveryConfig::default(),
            last_save_times: HashMap::new(),
            session_started: false,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_service() -> (RecoveryService, TempDir) {
        let temp_dir = TempDir::new().unwrap();
        let storage = RecoveryStorage::with_dir(temp_dir.path().to_path_buf());
        let service = RecoveryService {
            storage,
            config: RecoveryConfig::default(),
            last_save_times: HashMap::new(),
            session_started: false,
        };
        (service, temp_dir)
    }

    #[test]
    fn test_session_lifecycle() {
        let (mut service, _temp) = create_test_service();

        // Start session
        service.start_session().unwrap();
        assert!(service.session_started);

        // End session
        service.end_session().unwrap();
        assert!(!service.session_started);
    }

    #[test]
    fn test_save_and_recover() {
        let (mut service, _temp) = create_test_service();
        service.start_session().unwrap();

        let content = b"Test content for recovery";
        let path = Path::new("/test/file.txt");
        let id = service.get_buffer_id(Some(path));

        // Save recovery - create a single chunk with full content (new buffer style)
        let chunks = vec![RecoveryChunk::new(0, 0, content.to_vec())];
        service
            .save_buffer(&id, chunks, Some(path), None, Some(1), 0, content.len())
            .unwrap();

        // List recoverable
        let entries = service.list_recoverable().unwrap();
        assert_eq!(entries.len(), 1);

        // Load recovery
        let entry = &entries[0];
        let result = service.load_recovery(entry).unwrap();
        match result {
            RecoveryResult::Recovered {
                original_path,
                content: loaded,
            } => {
                assert_eq!(original_path, Some(path.to_path_buf()));
                assert_eq!(loaded, content);
            }
            _ => panic!("Expected Recovered result"),
        }
    }

    #[test]
    fn test_needs_auto_save() {
        let (service, _temp) = create_test_service();
        // Use a very short interval for testing
        let mut service = service;
        service.config.auto_save_interval_secs = 0;

        let id = "test-buffer";

        // Not recovery_pending - doesn't need save
        assert!(!service.needs_auto_save(id, false));

        // recovery_pending=true - needs save
        assert!(service.needs_auto_save(id, true));

        // After save, recovery_pending would be false on buffer
        service
            .last_save_times
            .insert(id.to_string(), Instant::now());
        assert!(!service.needs_auto_save(id, false));
    }

    #[test]
    fn test_disabled_service() {
        let (mut service, _temp) = create_test_service();
        service.config.enabled = false;

        // needs_auto_save returns false when disabled
        assert!(!service.needs_auto_save("test", true));

        // save_buffer doesn't error when disabled
        let chunks = vec![RecoveryChunk::new(0, 0, b"content".to_vec())];
        service
            .save_buffer("test", chunks, None, None, None, 0, 7)
            .unwrap();
    }

    #[test]
    fn test_load_recovery_returns_chunks_for_large_files() {
        use std::fs;

        let (mut service, temp_dir) = create_test_service();
        service.start_session().unwrap();

        // Create an original file
        let original_content = b"Hello, this is the original content!";
        let original_path = temp_dir.path().join("original.txt");
        fs::write(&original_path, original_content).unwrap();

        let id = service.get_buffer_id(Some(&original_path));

        // Save recovery with original_file_size > 0 (simulating large file recovery)
        // Chunk: insert "PREFIX: " at offset 0
        let chunks = vec![RecoveryChunk::new(0, 0, b"PREFIX: ".to_vec())];
        service
            .save_buffer(
                &id,
                chunks,
                Some(&original_path),
                None,
                Some(1),
                original_content.len(), // original_file_size > 0
                original_content.len() + 8,
            )
            .unwrap();

        // Load recovery - should return RecoveredChunks, not Recovered
        let entries = service.list_recoverable().unwrap();
        assert_eq!(entries.len(), 1);

        let result = service.load_recovery(&entries[0]).unwrap();
        match result {
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
            _ => panic!("Expected RecoveredChunks result, got {:?}", result),
        }
    }
}
