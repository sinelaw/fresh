//! Recovery file storage operations
//!
//! This module handles reading and writing recovery files with atomic operations
//! to ensure data integrity even during crashes.

use super::types::{
    compute_checksum, generate_buffer_id, path_hash, ChunkedRecoveryData, RecoveryChunk,
    RecoveryEntry, RecoveryFormat, RecoveryMetadata, SessionInfo,
};
use crate::input::input_history::get_data_dir;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

/// Recovery storage manager
///
/// Handles all file I/O for the recovery system with atomic operations.
#[derive(Debug)]
pub struct RecoveryStorage {
    /// Base directory for recovery files
    recovery_dir: PathBuf,
}

impl RecoveryStorage {
    /// File extension for metadata files
    const META_EXT: &'static str = "meta.json";
    /// File extension for content files
    const CONTENT_EXT: &'static str = "content";
    /// Session lock file name
    const SESSION_LOCK: &'static str = "session.lock";

    /// Create a new recovery storage manager
    pub fn new() -> io::Result<Self> {
        let recovery_dir = Self::get_recovery_dir()?;
        Ok(Self { recovery_dir })
    }

    /// Create a recovery storage with a custom directory (for testing)
    pub fn with_dir(recovery_dir: PathBuf) -> Self {
        Self { recovery_dir }
    }

    /// Get the recovery directory path
    pub fn get_recovery_dir() -> io::Result<PathBuf> {
        let data_dir = get_data_dir()?;
        Ok(data_dir.join("recovery"))
    }

    /// Ensure the recovery directory exists
    pub fn ensure_dir(&self) -> io::Result<()> {
        fs::create_dir_all(&self.recovery_dir)
    }

    /// Get the base directory
    pub fn base_dir(&self) -> &Path {
        &self.recovery_dir
    }

    // ========================================================================
    // Session lock management
    // ========================================================================

    /// Get the path to the session lock file
    fn session_lock_path(&self) -> PathBuf {
        self.recovery_dir.join(Self::SESSION_LOCK)
    }

    /// Create a session lock file for crash detection
    pub fn create_session_lock(&self) -> io::Result<SessionInfo> {
        self.ensure_dir()?;

        let info = SessionInfo::new();
        let json =
            serde_json::to_string_pretty(&info).map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

        self.atomic_write(&self.session_lock_path(), json.as_bytes())?;
        Ok(info)
    }

    /// Update the session lock timestamp (heartbeat)
    pub fn update_session_lock(&self) -> io::Result<()> {
        let path = self.session_lock_path();
        if path.exists() {
            // Just update the file's mtime by rewriting it
            let info = SessionInfo::new();
            let json = serde_json::to_string_pretty(&info)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
            self.atomic_write(&path, json.as_bytes())?;
        }
        Ok(())
    }

    /// Remove the session lock file (clean shutdown)
    pub fn remove_session_lock(&self) -> io::Result<()> {
        let path = self.session_lock_path();
        if path.exists() {
            fs::remove_file(path)?;
        }
        Ok(())
    }

    /// Read the session lock file
    pub fn read_session_lock(&self) -> io::Result<Option<SessionInfo>> {
        let path = self.session_lock_path();
        if !path.exists() {
            return Ok(None);
        }

        let content = fs::read_to_string(&path)?;
        let info: SessionInfo =
            serde_json::from_str(&content).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        Ok(Some(info))
    }

    /// Check if there was a crash (lock file exists but process not running)
    pub fn detect_crash(&self) -> io::Result<bool> {
        match self.read_session_lock()? {
            Some(info) => Ok(!info.is_running()),
            None => Ok(false),
        }
    }

    // ========================================================================
    // Recovery file operations
    // ========================================================================

    /// Get the ID for a buffer (path hash or generated ID)
    pub fn get_buffer_id(&self, path: Option<&Path>) -> String {
        match path {
            Some(p) => path_hash(p),
            None => generate_buffer_id(),
        }
    }

    /// Get paths for recovery files
    fn recovery_paths(&self, id: &str) -> (PathBuf, PathBuf) {
        let meta_path = self.recovery_dir.join(format!("{id}.{}", Self::META_EXT));
        let content_path = self.recovery_dir.join(format!("{id}.{}", Self::CONTENT_EXT));
        (meta_path, content_path)
    }

    /// Save a buffer's content to recovery storage
    ///
    /// This performs an atomic write: write to temp files, fsync, then rename.
    pub fn save_recovery(
        &self,
        id: &str,
        content: &[u8],
        original_path: Option<&Path>,
        buffer_name: Option<&str>,
        line_count: Option<usize>,
    ) -> io::Result<RecoveryMetadata> {
        self.ensure_dir()?;

        let (meta_path, content_path) = self.recovery_paths(id);
        let checksum = compute_checksum(content);

        // Get original file's mtime if it exists
        let original_mtime = original_path.and_then(|p| {
            fs::metadata(p)
                .ok()
                .and_then(|m| m.modified().ok())
                .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
                .map(|d| d.as_secs())
        });

        // Check if we already have metadata (update vs create)
        let metadata = if meta_path.exists() {
            let existing = self.read_metadata(id)?;
            let mut meta = existing.unwrap_or_else(|| {
                RecoveryMetadata::new(
                    original_path.map(|p| p.to_path_buf()),
                    buffer_name.map(|s| s.to_string()),
                    checksum.clone(),
                    content.len() as u64,
                    line_count,
                    original_mtime,
                )
            });
            meta.update(checksum, content.len() as u64, line_count);
            meta
        } else {
            RecoveryMetadata::new(
                original_path.map(|p| p.to_path_buf()),
                buffer_name.map(|s| s.to_string()),
                checksum,
                content.len() as u64,
                line_count,
                original_mtime,
            )
        };

        // Write content first (larger, more likely to fail)
        self.atomic_write(&content_path, content)?;

        // Write metadata
        let meta_json = serde_json::to_string_pretty(&metadata)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        self.atomic_write(&meta_path, meta_json.as_bytes())?;

        Ok(metadata)
    }

    /// Save chunked recovery data for large files
    ///
    /// Instead of saving the entire file content, this saves only the modified
    /// chunks with their positions. This is essential for multi-gigabyte files.
    pub fn save_chunked_recovery(
        &self,
        id: &str,
        chunks: Vec<RecoveryChunk>,
        original_path: Option<&Path>,
        buffer_name: Option<&str>,
        line_count: Option<usize>,
        original_file_size: usize,
        final_size: usize,
    ) -> io::Result<RecoveryMetadata> {
        self.ensure_dir()?;

        let (meta_path, content_path) = self.recovery_paths(id);

        // Create chunked data structure
        let chunked_data = ChunkedRecoveryData {
            original_size: original_file_size,
            chunks,
            final_size,
        };

        // Serialize chunks to JSON (efficient enough for reasonable chunk counts)
        let chunks_json = serde_json::to_vec(&chunked_data)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

        let checksum = compute_checksum(&chunks_json);
        let content_size = chunks_json.len() as u64;

        // Get original file's mtime if it exists
        let original_mtime = original_path.and_then(|p| {
            fs::metadata(p)
                .ok()
                .and_then(|m| m.modified().ok())
                .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
                .map(|d| d.as_secs())
        });

        // Create or update metadata
        let metadata = if meta_path.exists() {
            let existing = self.read_metadata(id)?;
            let mut meta = existing.unwrap_or_else(|| {
                RecoveryMetadata::new_chunked(
                    original_path.map(|p| p.to_path_buf()),
                    buffer_name.map(|s| s.to_string()),
                    checksum.clone(),
                    content_size,
                    line_count,
                    original_mtime,
                    chunked_data.chunks.len(),
                    original_file_size,
                )
            });
            meta.format = RecoveryFormat::Chunked;
            meta.original_file_size = Some(original_file_size);
            meta.update_chunked(
                checksum,
                content_size,
                line_count,
                chunked_data.chunks.len(),
            );
            meta
        } else {
            RecoveryMetadata::new_chunked(
                original_path.map(|p| p.to_path_buf()),
                buffer_name.map(|s| s.to_string()),
                checksum,
                content_size,
                line_count,
                original_mtime,
                chunked_data.chunks.len(),
                original_file_size,
            )
        };

        // Write chunks data
        self.atomic_write(&content_path, &chunks_json)?;

        // Write metadata
        let meta_json = serde_json::to_string_pretty(&metadata)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        self.atomic_write(&meta_path, meta_json.as_bytes())?;

        Ok(metadata)
    }

    /// Read chunked recovery data
    pub fn read_chunked_content(&self, id: &str) -> io::Result<Option<ChunkedRecoveryData>> {
        let (_, content_path) = self.recovery_paths(id);
        if !content_path.exists() {
            return Ok(None);
        }

        let content = fs::read(&content_path)?;
        let chunked_data: ChunkedRecoveryData = serde_json::from_slice(&content)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        Ok(Some(chunked_data))
    }

    /// Reconstruct full content from chunked recovery and original file
    ///
    /// This reads the original file and applies the stored chunks to reconstruct
    /// the full modified content.
    pub fn reconstruct_from_chunks(
        &self,
        id: &str,
        original_file: &Path,
    ) -> io::Result<Vec<u8>> {
        let chunked_data = self.read_chunked_content(id)?.ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, "Chunked recovery data not found")
        })?;

        // Read original file
        let original_content = fs::read(original_file)?;

        // Verify original file size matches what we expected
        if original_content.len() != chunked_data.original_size {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "Original file size mismatch: expected {}, got {}",
                    chunked_data.original_size,
                    original_content.len()
                ),
            ));
        }

        // Apply chunks to reconstruct content
        let mut result = Vec::with_capacity(chunked_data.final_size);
        let mut original_pos = 0;

        for chunk in &chunked_data.chunks {
            // Verify chunk integrity
            if !chunk.verify() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Chunk at offset {} failed checksum verification", chunk.offset),
                ));
            }

            // Copy unchanged content before this chunk
            if chunk.offset > original_pos {
                result.extend_from_slice(&original_content[original_pos..chunk.offset]);
            }

            // Insert the modified chunk content
            result.extend_from_slice(&chunk.content);

            // Move past the replaced region in the original
            original_pos = chunk.offset + chunk.original_len;
        }

        // Copy any remaining content after the last chunk
        if original_pos < original_content.len() {
            result.extend_from_slice(&original_content[original_pos..]);
        }

        Ok(result)
    }

    /// Read recovery metadata
    pub fn read_metadata(&self, id: &str) -> io::Result<Option<RecoveryMetadata>> {
        let (meta_path, _) = self.recovery_paths(id);
        if !meta_path.exists() {
            return Ok(None);
        }

        let content = fs::read_to_string(&meta_path)?;
        let metadata: RecoveryMetadata = serde_json::from_str(&content)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        Ok(Some(metadata))
    }

    /// Read recovery content
    pub fn read_content(&self, id: &str) -> io::Result<Option<Vec<u8>>> {
        let (_, content_path) = self.recovery_paths(id);
        if !content_path.exists() {
            return Ok(None);
        }
        Ok(Some(fs::read(&content_path)?))
    }

    /// Load a complete recovery entry
    pub fn load_entry(&self, id: &str) -> io::Result<Option<RecoveryEntry>> {
        let (meta_path, content_path) = self.recovery_paths(id);

        if !meta_path.exists() || !content_path.exists() {
            return Ok(None);
        }

        let metadata = self.read_metadata(id)?.ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, "Metadata file exists but couldn't be read")
        })?;

        Ok(Some(RecoveryEntry {
            id: id.to_string(),
            metadata,
            content_path,
            metadata_path: meta_path,
        }))
    }

    /// Delete recovery files for a buffer
    pub fn delete_recovery(&self, id: &str) -> io::Result<()> {
        let (meta_path, content_path) = self.recovery_paths(id);

        // Delete both files, ignoring "not found" errors
        if content_path.exists() {
            fs::remove_file(&content_path)?;
        }
        if meta_path.exists() {
            fs::remove_file(&meta_path)?;
        }

        Ok(())
    }

    /// List all recovery entries
    pub fn list_entries(&self) -> io::Result<Vec<RecoveryEntry>> {
        if !self.recovery_dir.exists() {
            return Ok(Vec::new());
        }

        let mut entries = Vec::new();

        for entry in fs::read_dir(&self.recovery_dir)? {
            let entry = entry?;
            let path = entry.path();

            // Look for .meta.json files
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if name.ends_with(&format!(".{}", Self::META_EXT)) {
                    // Extract the ID (everything before .meta.json)
                    let id = name.trim_end_matches(&format!(".{}", Self::META_EXT));
                    if let Ok(Some(entry)) = self.load_entry(id) {
                        entries.push(entry);
                    }
                }
            }
        }

        // Sort by update time (newest first)
        entries.sort_by(|a, b| b.metadata.updated_at.cmp(&a.metadata.updated_at));

        Ok(entries)
    }

    /// Clean up orphaned files (content without metadata or vice versa)
    pub fn cleanup_orphans(&self) -> io::Result<usize> {
        if !self.recovery_dir.exists() {
            return Ok(0);
        }

        let mut cleaned = 0;

        for entry in fs::read_dir(&self.recovery_dir)? {
            let entry = entry?;
            let path = entry.path();

            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                // Skip session lock
                if name == Self::SESSION_LOCK {
                    continue;
                }

                let id = if name.ends_with(&format!(".{}", Self::META_EXT)) {
                    name.trim_end_matches(&format!(".{}", Self::META_EXT))
                } else if name.ends_with(&format!(".{}", Self::CONTENT_EXT)) {
                    name.trim_end_matches(&format!(".{}", Self::CONTENT_EXT))
                } else {
                    // Unknown file type, skip
                    continue;
                };

                let (meta_path, content_path) = self.recovery_paths(id);

                // If either file is missing, clean up both
                if !meta_path.exists() || !content_path.exists() {
                    let _ = fs::remove_file(&meta_path);
                    let _ = fs::remove_file(&content_path);
                    cleaned += 1;
                }
            }
        }

        Ok(cleaned)
    }

    /// Clean up all recovery files (after successful recovery or user dismissal)
    pub fn cleanup_all(&self) -> io::Result<usize> {
        if !self.recovery_dir.exists() {
            return Ok(0);
        }

        let mut cleaned = 0;

        for entry in fs::read_dir(&self.recovery_dir)? {
            let entry = entry?;
            let path = entry.path();

            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                // Don't delete the session lock
                if name == Self::SESSION_LOCK {
                    continue;
                }

                if fs::remove_file(&path).is_ok() {
                    cleaned += 1;
                }
            }
        }

        Ok(cleaned)
    }

    // ========================================================================
    // Helper methods
    // ========================================================================

    /// Perform an atomic write: write to temp file, fsync, rename
    fn atomic_write(&self, target: &Path, content: &[u8]) -> io::Result<()> {
        let temp_path = target.with_extension("tmp");

        // Write to temp file
        let mut file = File::create(&temp_path)?;
        file.write_all(content)?;
        file.sync_all()?;
        drop(file);

        // Atomic rename
        fs::rename(&temp_path, target)?;

        Ok(())
    }
}

impl Default for RecoveryStorage {
    fn default() -> Self {
        Self::new().unwrap_or_else(|_| Self {
            recovery_dir: PathBuf::from("/tmp/fresh-recovery"),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_storage() -> (RecoveryStorage, TempDir) {
        let temp_dir = TempDir::new().unwrap();
        let storage = RecoveryStorage {
            recovery_dir: temp_dir.path().to_path_buf(),
        };
        (storage, temp_dir)
    }

    #[test]
    fn test_session_lock_lifecycle() {
        let (storage, _temp) = create_test_storage();

        // Create lock
        let info = storage.create_session_lock().unwrap();
        assert_eq!(info.pid, std::process::id());

        // Read lock
        let read_info = storage.read_session_lock().unwrap().unwrap();
        assert_eq!(read_info.pid, info.pid);

        // Lock should show as running (it's our process)
        assert!(read_info.is_running());

        // Remove lock
        storage.remove_session_lock().unwrap();
        assert!(storage.read_session_lock().unwrap().is_none());
    }

    #[test]
    fn test_save_and_load_recovery() {
        let (storage, _temp) = create_test_storage();

        let content = b"Hello, World!";
        let path = Path::new("/test/file.rs");
        let id = storage.get_buffer_id(Some(path));

        // Save recovery
        let metadata = storage
            .save_recovery(&id, content, Some(path), None, Some(1))
            .unwrap();

        assert_eq!(metadata.content_size, content.len() as u64);
        assert_eq!(metadata.line_count, Some(1));

        // Load entry
        let entry = storage.load_entry(&id).unwrap().unwrap();
        assert_eq!(entry.id, id);

        // Verify content
        let loaded_content = storage.read_content(&id).unwrap().unwrap();
        assert_eq!(loaded_content, content);

        // Verify checksum
        assert!(entry.verify_checksum().unwrap());
    }

    #[test]
    fn test_list_entries() {
        let (storage, _temp) = create_test_storage();

        // Save multiple entries
        storage
            .save_recovery("id1", b"content1", None, Some("Buffer 1"), None)
            .unwrap();
        storage
            .save_recovery("id2", b"content2", None, Some("Buffer 2"), None)
            .unwrap();

        let entries = storage.list_entries().unwrap();
        assert_eq!(entries.len(), 2);
    }

    #[test]
    fn test_delete_recovery() {
        let (storage, _temp) = create_test_storage();

        let id = "test-id";
        storage
            .save_recovery(id, b"content", None, Some("Test"), None)
            .unwrap();

        // Verify it exists
        assert!(storage.load_entry(id).unwrap().is_some());

        // Delete it
        storage.delete_recovery(id).unwrap();

        // Verify it's gone
        assert!(storage.load_entry(id).unwrap().is_none());
    }

    #[test]
    fn test_cleanup_orphans() {
        let (storage, _temp) = create_test_storage();
        storage.ensure_dir().unwrap();

        // Create an orphan content file (no metadata)
        let orphan_content = storage.recovery_dir.join("orphan.content");
        fs::write(&orphan_content, b"orphan").unwrap();

        // Create a complete entry
        storage
            .save_recovery("complete", b"content", None, Some("Test"), None)
            .unwrap();

        // Cleanup should remove the orphan
        let cleaned = storage.cleanup_orphans().unwrap();
        assert_eq!(cleaned, 1);

        // Complete entry should still exist
        assert!(storage.load_entry("complete").unwrap().is_some());
    }

    #[test]
    fn test_atomic_write_integrity() {
        let (storage, _temp) = create_test_storage();
        storage.ensure_dir().unwrap();

        let target = storage.recovery_dir.join("test.txt");
        let content = b"Test content for atomic write";

        storage.atomic_write(&target, content).unwrap();

        // Verify content
        let read_content = fs::read(&target).unwrap();
        assert_eq!(read_content, content);

        // Temp file should not exist
        let temp_path = target.with_extension("tmp");
        assert!(!temp_path.exists());
    }
}
