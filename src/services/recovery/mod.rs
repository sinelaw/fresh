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
//! ├── {hash}.meta.json      # Recovery metadata
//! └── {hash}.content        # Buffer content
//! ```
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
//! recovery.save_buffer("id", &content, Some(&path), None)?;
//!
//! // On clean shutdown
//! recovery.end_session()?;
//! ```

mod storage;
mod types;

pub use storage::RecoveryStorage;
pub use types::{
    compute_checksum, generate_buffer_id, path_hash, ChunkedRecoveryData, RecoveryChunk,
    RecoveryEntry, RecoveryFormat, RecoveryMetadata, RecoveryResult, SessionInfo,
    CHUNKED_RECOVERY_THRESHOLD, MAX_CHUNK_SIZE,
};

use std::collections::{HashMap, HashSet};
use std::io;
use std::path::Path;
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
            auto_save_interval_secs: 30,
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
    /// Set of buffer IDs that have been modified since last auto-save
    dirty_buffers: HashSet<String>,
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
            dirty_buffers: HashSet::new(),
            session_started: false,
        })
    }

    /// Create a new recovery service with custom config
    pub fn with_config(config: RecoveryConfig) -> io::Result<Self> {
        Ok(Self {
            storage: RecoveryStorage::new()?,
            config,
            last_save_times: HashMap::new(),
            dirty_buffers: HashSet::new(),
            session_started: false,
        })
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

    /// Mark a buffer as dirty (modified since last save)
    pub fn mark_dirty(&mut self, buffer_id: &str) {
        if self.config.enabled {
            self.dirty_buffers.insert(buffer_id.to_string());
        }
    }

    /// Mark a buffer as clean (saved or reverted)
    pub fn mark_clean(&mut self, buffer_id: &str) {
        self.dirty_buffers.remove(buffer_id);
    }

    /// Check if a buffer is dirty
    pub fn is_dirty(&self, buffer_id: &str) -> bool {
        self.dirty_buffers.contains(buffer_id)
    }

    /// Check if a buffer needs auto-save
    pub fn needs_auto_save(&self, buffer_id: &str) -> bool {
        if !self.config.enabled || !self.dirty_buffers.contains(buffer_id) {
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
    /// For small files (< CHUNKED_RECOVERY_THRESHOLD), saves full content.
    /// For large files, use save_buffer_chunked instead for efficiency.
    pub fn save_buffer(
        &mut self,
        buffer_id: &str,
        content: &[u8],
        original_path: Option<&Path>,
        buffer_name: Option<&str>,
        line_count: Option<usize>,
    ) -> io::Result<()> {
        if !self.config.enabled {
            return Ok(());
        }

        self.storage
            .save_recovery(buffer_id, content, original_path, buffer_name, line_count)?;
        self.last_save_times
            .insert(buffer_id.to_string(), Instant::now());
        self.dirty_buffers.remove(buffer_id);

        tracing::debug!(
            "Saved recovery for buffer {} ({} bytes)",
            buffer_id,
            content.len()
        );
        Ok(())
    }

    /// Save chunked recovery for large files
    ///
    /// This only saves the modified chunks, not the entire file content.
    /// Essential for multi-gigabyte files.
    pub fn save_buffer_chunked(
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

        self.storage.save_chunked_recovery(
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
        self.dirty_buffers.remove(buffer_id);

        tracing::debug!(
            "Saved chunked recovery for buffer {} (original: {} bytes)",
            buffer_id,
            original_file_size
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
        self.dirty_buffers.remove(buffer_id);

        tracing::debug!("Deleted recovery for buffer {}", buffer_id);
        Ok(())
    }

    /// List all recoverable entries
    pub fn list_recoverable(&self) -> io::Result<Vec<RecoveryEntry>> {
        self.storage.list_entries()
    }

    /// Load recovery content for a specific entry
    ///
    /// For Full format entries, returns the content directly.
    /// For Chunked format entries, requires the original file to reconstruct.
    pub fn load_recovery(&self, entry: &RecoveryEntry) -> io::Result<RecoveryResult> {
        // Verify checksum first
        if !entry.verify_checksum()? {
            return Ok(RecoveryResult::Corrupted {
                id: entry.id.clone(),
                reason: "Checksum mismatch - file may be corrupted".to_string(),
            });
        }

        // Handle chunked vs full format
        if entry.metadata.is_chunked() {
            // For chunked recovery, we need the original file
            if let Some(ref original_path) = entry.metadata.original_path {
                if original_path.exists() {
                    let content = self.storage.reconstruct_from_chunks(&entry.id, original_path)?;
                    return Ok(RecoveryResult::Recovered {
                        original_path: Some(original_path.clone()),
                        content,
                    });
                } else {
                    return Ok(RecoveryResult::Corrupted {
                        id: entry.id.clone(),
                        reason: format!(
                            "Original file not found: {}. Chunked recovery requires the original file.",
                            original_path.display()
                        ),
                    });
                }
            } else {
                return Ok(RecoveryResult::Corrupted {
                    id: entry.id.clone(),
                    reason: "Chunked recovery without original file path".to_string(),
                });
            }
        }

        // Full format - just load the content
        let content = entry.load_content()?;
        Ok(RecoveryResult::Recovered {
            original_path: entry.metadata.original_path.clone(),
            content,
        })
    }

    /// Load chunked recovery with a provided original file path
    ///
    /// Use this when the original file has moved or you want to specify a different source.
    pub fn load_chunked_recovery_with_original(
        &self,
        entry: &RecoveryEntry,
        original_file: &Path,
    ) -> io::Result<RecoveryResult> {
        if !entry.metadata.is_chunked() {
            return Ok(RecoveryResult::Corrupted {
                id: entry.id.clone(),
                reason: "Entry is not in chunked format".to_string(),
            });
        }

        if !entry.verify_checksum()? {
            return Ok(RecoveryResult::Corrupted {
                id: entry.id.clone(),
                reason: "Checksum mismatch - file may be corrupted".to_string(),
            });
        }

        let content = self.storage.reconstruct_from_chunks(&entry.id, original_file)?;
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
            if entry.age_seconds() > self.config.max_recovery_age_secs {
                if self.storage.delete_recovery(&entry.id).is_ok() {
                    cleaned += 1;
                }
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
            dirty_buffers: HashSet::new(),
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
            dirty_buffers: HashSet::new(),
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
    fn test_dirty_tracking() {
        let (mut service, _temp) = create_test_service();

        let id = "test-buffer";
        assert!(!service.is_dirty(id));

        service.mark_dirty(id);
        assert!(service.is_dirty(id));

        service.mark_clean(id);
        assert!(!service.is_dirty(id));
    }

    #[test]
    fn test_save_and_recover() {
        let (mut service, _temp) = create_test_service();
        service.start_session().unwrap();

        let content = b"Test content for recovery";
        let path = Path::new("/test/file.txt");
        let id = service.get_buffer_id(Some(path));

        // Save recovery
        service.mark_dirty(&id);
        service
            .save_buffer(&id, content, Some(path), None, Some(1))
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
        let (mut service, _temp) = create_test_service();
        // Use a very short interval for testing
        service.config.auto_save_interval_secs = 0;

        let id = "test-buffer";

        // Not dirty - doesn't need save
        assert!(!service.needs_auto_save(id));

        // Mark dirty - needs save
        service.mark_dirty(id);
        assert!(service.needs_auto_save(id));

        // After save - doesn't need save anymore
        service
            .last_save_times
            .insert(id.to_string(), Instant::now());
        service.mark_clean(id);
        assert!(!service.needs_auto_save(id));
    }

    #[test]
    fn test_disabled_service() {
        let (mut service, _temp) = create_test_service();
        service.config.enabled = false;

        // All operations should be no-ops when disabled
        service.mark_dirty("test");
        assert!(!service.is_dirty("test")); // Returns false because disabled

        // Actually it still tracks... let me fix the implementation
        // For now, just verify save_buffer doesn't error
        service
            .save_buffer("test", b"content", None, None, None)
            .unwrap();
    }
}
