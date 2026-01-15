//! Recovery file storage operations
//!
//! This module handles reading and writing recovery files with atomic operations
//! to ensure data integrity even during crashes.

use super::types::{
    generate_buffer_id, path_hash, ChunkedRecoveryData, ChunkedRecoveryIndex, RecoveryChunk,
    RecoveryEntry, RecoveryMetadata, SessionInfo,
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
        let json = serde_json::to_string_pretty(&info).map_err(io::Error::other)?;

        self.atomic_write(&self.session_lock_path(), json.as_bytes())?;
        Ok(info)
    }

    /// Update the session lock timestamp (heartbeat)
    pub fn update_session_lock(&self) -> io::Result<()> {
        let path = self.session_lock_path();
        if path.exists() {
            // Just update the file's mtime by rewriting it
            let info = SessionInfo::new();
            let json = serde_json::to_string_pretty(&info).map_err(io::Error::other)?;
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
        let info: SessionInfo = serde_json::from_str(&content)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
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
        let content_path = self
            .recovery_dir
            .join(format!("{id}.{}", Self::CONTENT_EXT));
        (meta_path, content_path)
    }

    /// Get path for a chunk file
    fn chunk_path(&self, id: &str, chunk_index: usize) -> PathBuf {
        self.recovery_dir
            .join(format!("{id}.chunk.{}", chunk_index))
    }

    /// List all chunk files for a given ID
    fn list_chunk_paths(&self, id: &str) -> io::Result<Vec<PathBuf>> {
        let mut paths = Vec::new();
        let prefix = format!("{id}.chunk.");

        if !self.recovery_dir.exists() {
            return Ok(paths);
        }

        for entry in fs::read_dir(&self.recovery_dir)? {
            let entry = entry?;
            if let Some(name) = entry.file_name().to_str() {
                if name.starts_with(&prefix) {
                    paths.push(entry.path());
                }
            }
        }
        paths.sort();
        Ok(paths)
    }

    /// Delete all chunk files for a given ID
    fn delete_chunk_files(&self, id: &str) -> io::Result<()> {
        for path in self.list_chunk_paths(id)? {
            let _ = fs::remove_file(path);
        }
        Ok(())
    }

    /// Save recovery data for a buffer
    ///
    /// Stores chunks to separate files with metadata in JSON.
    /// For small files/new buffers, pass a single chunk with the full content.
    /// For large files, pass only the modified chunks.
    ///
    /// ## File Layout
    ///
    /// - `{id}.meta.json` - Contains RecoveryMetadata with embedded ChunkedRecoveryIndex
    /// - `{id}.chunk.0`, `{id}.chunk.1`, ... - Raw binary content for each chunk
    #[allow(clippy::too_many_arguments)]
    pub fn save_recovery(
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

        let (meta_path, _content_path) = self.recovery_paths(id);

        // Delete any existing chunk files first
        self.delete_chunk_files(id)?;

        // Create chunked data structure
        let chunked_data = ChunkedRecoveryData::new(original_file_size, final_size, chunks);

        // Write each chunk to a separate file
        let mut total_chunk_bytes = 0u64;
        for (i, chunk) in chunked_data.chunks.iter().enumerate() {
            let chunk_path = self.chunk_path(id, i);
            self.atomic_write(&chunk_path, &chunk.content)?;
            total_chunk_bytes += chunk.content.len() as u64;
        }

        // Create the index (metadata without binary content)
        let index = chunked_data.to_index();

        // Get original file's mtime if it exists
        let original_mtime = original_path.and_then(|p| {
            fs::metadata(p)
                .ok()
                .and_then(|m| m.modified().ok())
                .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
                .map(|d| d.as_secs())
        });

        // Create or update metadata
        let mut metadata = if meta_path.exists() {
            self.read_metadata(id)?.unwrap_or_else(|| {
                RecoveryMetadata::new(
                    original_path.map(|p| p.to_path_buf()),
                    buffer_name.map(|s| s.to_string()),
                    total_chunk_bytes,
                    line_count,
                    original_mtime,
                    chunked_data.chunks.len(),
                    original_file_size,
                )
            })
        } else {
            RecoveryMetadata::new(
                original_path.map(|p| p.to_path_buf()),
                buffer_name.map(|s| s.to_string()),
                total_chunk_bytes,
                line_count,
                original_mtime,
                chunked_data.chunks.len(),
                original_file_size,
            )
        };

        // Update metadata fields
        metadata.original_file_size = original_file_size;
        metadata.update(total_chunk_bytes, line_count, chunked_data.chunks.len());

        // Create combined metadata with embedded chunk index
        #[derive(serde::Serialize)]
        struct ChunkedMetadataFile {
            #[serde(flatten)]
            metadata: RecoveryMetadata,
            chunked_index: ChunkedRecoveryIndex,
        }

        let meta_file = ChunkedMetadataFile {
            metadata: metadata.clone(),
            chunked_index: index,
        };

        // Write metadata (includes chunk index)
        let meta_json = serde_json::to_string_pretty(&meta_file).map_err(io::Error::other)?;
        self.atomic_write(&meta_path, meta_json.as_bytes())?;

        Ok(metadata)
    }

    /// Read chunked recovery index from metadata file
    pub fn read_chunked_index(&self, id: &str) -> io::Result<Option<ChunkedRecoveryIndex>> {
        let (meta_path, _) = self.recovery_paths(id);
        if !meta_path.exists() {
            return Ok(None);
        }

        let content = fs::read_to_string(&meta_path)?;

        // Parse the metadata file which contains chunked_index
        #[derive(serde::Deserialize)]
        struct ChunkedMetadataFile {
            #[serde(default)]
            chunked_index: Option<ChunkedRecoveryIndex>,
        }

        let meta_file: ChunkedMetadataFile = serde_json::from_str(&content)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        Ok(meta_file.chunked_index)
    }

    /// Read chunked recovery data (loads index and all chunk content from files)
    ///
    /// This reads the chunk index from metadata and loads each chunk's binary
    /// content from its separate file.
    pub fn read_chunked_content(&self, id: &str) -> io::Result<Option<ChunkedRecoveryData>> {
        // Read the chunk index from metadata
        let index = match self.read_chunked_index(id)? {
            Some(idx) => idx,
            None => return Ok(None),
        };

        // Load content for each chunk from its file
        let mut chunks = Vec::with_capacity(index.chunks.len());
        for (i, chunk_meta) in index.chunks.iter().enumerate() {
            let chunk_path = self.chunk_path(id, i);
            if !chunk_path.exists() {
                return Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("Chunk file {} not found", chunk_path.display()),
                ));
            }

            let content = fs::read(&chunk_path)?;

            chunks.push(RecoveryChunk {
                offset: chunk_meta.offset,
                original_len: chunk_meta.original_len,
                content,
            });
        }

        Ok(Some(ChunkedRecoveryData::new(
            index.original_size,
            index.final_size,
            chunks,
        )))
    }

    /// Reconstruct full content from chunked recovery and original file
    ///
    /// This reads the original file and applies the stored chunks to reconstruct
    /// the full modified content.
    pub fn reconstruct_from_chunks(&self, id: &str, original_file: &Path) -> io::Result<Vec<u8>> {
        let chunked_data = self.read_chunked_content(id)?.ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, "Chunked recovery data not found")
        })?;

        // Read original file
        let original_content = fs::read(original_file)?;

        tracing::debug!(
            "reconstruct_from_chunks: original_file={:?}, file_size_on_disk={}, expected_original_size={}",
            original_file,
            original_content.len(),
            chunked_data.original_size
        );

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

        if !meta_path.exists() {
            return Ok(None);
        }

        let metadata = self.read_metadata(id)?.ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                "Metadata file exists but couldn't be read",
            )
        })?;

        // Require at least one chunk file
        let chunk_paths = self.list_chunk_paths(id)?;
        if chunk_paths.is_empty() {
            return Ok(None);
        }

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

        // Delete content file (for Full format)
        if content_path.exists() {
            fs::remove_file(&content_path)?;
        }

        // Delete chunk files (for Chunked format)
        self.delete_chunk_files(id)?;

        // Delete metadata file
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
        let mut seen_ids: std::collections::HashSet<String> = std::collections::HashSet::new();

        for entry in fs::read_dir(&self.recovery_dir)? {
            let entry = entry?;
            let path = entry.path();

            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                // Skip session lock
                if name == Self::SESSION_LOCK {
                    continue;
                }

                // Extract ID from various file types
                let id = if name.ends_with(&format!(".{}", Self::META_EXT)) {
                    name.trim_end_matches(&format!(".{}", Self::META_EXT))
                        .to_string()
                } else if name.ends_with(&format!(".{}", Self::CONTENT_EXT)) {
                    name.trim_end_matches(&format!(".{}", Self::CONTENT_EXT))
                        .to_string()
                } else if name.contains(".chunk.") {
                    // Handle chunk files like "id.chunk.0"
                    name.split(".chunk.").next().unwrap_or("").to_string()
                } else {
                    // Unknown file type, skip
                    continue;
                };

                if id.is_empty() || seen_ids.contains(&id) {
                    continue;
                }
                seen_ids.insert(id.clone());

                let (meta_path, _content_path) = self.recovery_paths(&id);
                let chunk_paths = self.list_chunk_paths(&id).unwrap_or_default();

                // Need meta + chunk files
                let is_valid = meta_path.exists() && !chunk_paths.is_empty();

                if !is_valid {
                    let _ = fs::remove_file(&meta_path);
                    let _ = self.delete_chunk_files(&id);
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

    /// Perform an atomic write: write to temp file, then rename
    ///
    /// TODO: Migrate to async I/O to avoid blocking the main thread during recovery saves.
    /// Currently we skip fsync for performance - this means editor crashes are safe (OS
    /// page cache survives), but system crashes/power loss could leave corrupted recovery
    /// files. Async I/O would let us have both safety and performance.
    fn atomic_write(&self, target: &Path, content: &[u8]) -> io::Result<()> {
        let temp_path = target.with_extension("tmp");

        // Write to temp file
        let mut file = File::create(&temp_path)?;
        file.write_all(content)?;
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

        // Save recovery - single chunk for full content
        let chunks = vec![RecoveryChunk::new(0, 0, content.to_vec())];
        let metadata = storage
            .save_recovery(&id, chunks, Some(path), None, Some(1), 0, content.len())
            .unwrap();

        assert_eq!(metadata.content_size, content.len() as u64);
        assert_eq!(metadata.line_count, Some(1));

        // Load entry
        let entry = storage.load_entry(&id).unwrap().unwrap();
        assert_eq!(entry.id, id);

        // Verify chunked content
        let chunked_data = storage.read_chunked_content(&id).unwrap().unwrap();
        assert_eq!(chunked_data.chunks.len(), 1);
        assert_eq!(chunked_data.chunks[0].content, content);

        // Verify checksum
    }

    #[test]
    fn test_list_entries() {
        let (storage, _temp) = create_test_storage();

        // Save multiple entries
        let chunks1 = vec![RecoveryChunk::new(0, 0, b"content1".to_vec())];
        storage
            .save_recovery("id1", chunks1, None, Some("Buffer 1"), None, 0, 8)
            .unwrap();
        let chunks2 = vec![RecoveryChunk::new(0, 0, b"content2".to_vec())];
        storage
            .save_recovery("id2", chunks2, None, Some("Buffer 2"), None, 0, 8)
            .unwrap();

        let entries = storage.list_entries().unwrap();
        assert_eq!(entries.len(), 2);
    }

    #[test]
    fn test_delete_recovery() {
        let (storage, _temp) = create_test_storage();

        let id = "test-id";
        let chunks = vec![RecoveryChunk::new(0, 0, b"content".to_vec())];
        storage
            .save_recovery(id, chunks, None, Some("Test"), None, 0, 7)
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
        let chunks = vec![RecoveryChunk::new(0, 0, b"content".to_vec())];
        storage
            .save_recovery("complete", chunks, None, Some("Test"), None, 0, 7)
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

    // ========================================================================
    // Chunked recovery tests
    // ========================================================================

    #[test]
    fn test_chunked_recovery_save_and_load() {
        let (storage, temp_dir) = create_test_storage();

        // Create test chunks simulating modifications to a large file
        let chunk1 = RecoveryChunk::new(0, 0, b"INSERTED_AT_START".to_vec());
        let chunk2 = RecoveryChunk::new(100, 10, b"REPLACED".to_vec());
        let chunks = vec![chunk1, chunk2];

        let id = "test-chunked";
        let original_path = Path::new("/test/large_file.txt");
        let original_size = 1000;
        let final_size = original_size + 17 - 10 + 8; // inserted 17, replaced 10 with 8

        // Save recovery
        let metadata = storage
            .save_recovery(
                id,
                chunks,
                Some(original_path),
                Some("Large File"),
                Some(100),
                original_size,
                final_size,
            )
            .unwrap();

        // Verify metadata
        assert_eq!(metadata.chunk_count, 2);
        assert_eq!(metadata.original_file_size, original_size);

        // Verify chunk files exist
        assert!(storage.chunk_path(id, 0).exists());
        assert!(storage.chunk_path(id, 1).exists());
        assert!(!storage.chunk_path(id, 2).exists()); // Only 2 chunks

        // Verify chunk file contents
        let chunk0_content = fs::read(storage.chunk_path(id, 0)).unwrap();
        assert_eq!(chunk0_content, b"INSERTED_AT_START");

        let chunk1_content = fs::read(storage.chunk_path(id, 1)).unwrap();
        assert_eq!(chunk1_content, b"REPLACED");

        // Verify metadata file contains chunked_index
        let (meta_path, _) = storage.recovery_paths(id);
        let meta_content = fs::read_to_string(&meta_path).unwrap();
        assert!(meta_content.contains("chunked_index"));
        assert!(meta_content.contains("\"original_size\""));
        assert!(meta_content.contains("\"final_size\""));

        // Load entry and verify
        let _entry = storage.load_entry(id).unwrap().unwrap();

        // Test: list entry shows up
        let entries = storage.list_entries().unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].id, id);

        // Clean up temp dir for debugging
        drop(temp_dir);
    }

    #[test]
    fn test_chunked_recovery_read_content() {
        let (storage, _temp) = create_test_storage();

        // Create test chunks
        let chunk1 = RecoveryChunk::new(50, 5, b"NEW_CONTENT".to_vec());
        let chunks = vec![chunk1];

        let id = "test-read-chunked";
        storage
            .save_recovery(id, chunks, None, None, None, 200, 206)
            .unwrap();

        // Read chunked content back
        let chunked_data = storage.read_chunked_content(id).unwrap().unwrap();

        assert_eq!(chunked_data.original_size, 200);
        assert_eq!(chunked_data.final_size, 206);
        assert_eq!(chunked_data.chunks.len(), 1);
        assert_eq!(chunked_data.chunks[0].offset, 50);
        assert_eq!(chunked_data.chunks[0].original_len, 5);
        assert_eq!(chunked_data.chunks[0].content, b"NEW_CONTENT");
    }

    #[test]
    fn test_chunked_recovery_reconstruct() {
        let (storage, temp_dir) = create_test_storage();

        // Create a "large" original file
        // "Hello, this is the original content of the file!"
        //  0     7    12  15  19      27
        let original_content = b"Hello, this is the original content of the file!";
        let original_path = temp_dir.path().join("original.txt");
        fs::write(&original_path, original_content).unwrap();

        // Create chunks that modify the file:
        // 1. Insert "PREFIX: " at the beginning (offset=0, original_len=0)
        // 2. Replace "original" (at position 19, len 8) with "MODIFIED"
        let chunk1 = RecoveryChunk::new(0, 0, b"PREFIX: ".to_vec());
        let chunk2 = RecoveryChunk::new(19, 8, b"MODIFIED".to_vec());
        let chunks = vec![chunk1, chunk2];

        let id = "test-reconstruct";
        let final_size = original_content.len() + 8; // Added 8 bytes prefix, same replacement length

        storage
            .save_recovery(
                id,
                chunks,
                Some(&original_path),
                None,
                None,
                original_content.len(),
                final_size,
            )
            .unwrap();

        // Reconstruct the file
        let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();

        // Expected: "PREFIX: Hello, this is the MODIFIED content of the file!"
        let expected = b"PREFIX: Hello, this is the MODIFIED content of the file!";
        assert_eq!(reconstructed, expected);
    }

    #[test]
    fn test_chunked_recovery_delete() {
        let (storage, _temp) = create_test_storage();

        // Create and save recovery
        let chunks = vec![
            RecoveryChunk::new(0, 0, b"A".to_vec()),
            RecoveryChunk::new(10, 5, b"BB".to_vec()),
            RecoveryChunk::new(20, 3, b"CCC".to_vec()),
        ];

        let id = "test-delete-chunked";
        storage
            .save_recovery(id, chunks, None, None, None, 100, 95)
            .unwrap();

        // Verify files exist
        let (meta_path, _) = storage.recovery_paths(id);
        assert!(meta_path.exists());
        assert!(storage.chunk_path(id, 0).exists());
        assert!(storage.chunk_path(id, 1).exists());
        assert!(storage.chunk_path(id, 2).exists());

        // Delete recovery
        storage.delete_recovery(id).unwrap();

        // Verify all files are gone
        assert!(!meta_path.exists());
        assert!(!storage.chunk_path(id, 0).exists());
        assert!(!storage.chunk_path(id, 1).exists());
        assert!(!storage.chunk_path(id, 2).exists());
    }

    #[test]
    fn test_chunked_recovery_cleanup_orphan_chunks() {
        let (storage, _temp) = create_test_storage();
        storage.ensure_dir().unwrap();

        // Create orphan chunk files (no metadata)
        let orphan_chunk0 = storage.chunk_path("orphan", 0);
        let orphan_chunk1 = storage.chunk_path("orphan", 1);
        fs::write(&orphan_chunk0, b"orphan chunk 0").unwrap();
        fs::write(&orphan_chunk1, b"orphan chunk 1").unwrap();

        // Create a valid entry
        let chunks = vec![RecoveryChunk::new(0, 0, b"valid".to_vec())];
        storage
            .save_recovery("valid", chunks, None, None, None, 100, 105)
            .unwrap();

        // Cleanup orphans
        let cleaned = storage.cleanup_orphans().unwrap();
        assert_eq!(cleaned, 1); // One orphan ID cleaned up

        // Orphan chunks should be gone
        assert!(!orphan_chunk0.exists());
        assert!(!orphan_chunk1.exists());

        // Valid entry should still exist
        assert!(storage.load_entry("valid").unwrap().is_some());
    }

    #[test]
    fn test_multiple_entries() {
        let (storage, _temp) = create_test_storage();

        // Create two entries with different original_file_size
        // First: new buffer (original_file_size = 0)
        let chunks1 = vec![RecoveryChunk::new(0, 0, b"new buffer content".to_vec())];
        storage
            .save_recovery("new-buffer", chunks1, None, Some("New"), None, 0, 18)
            .unwrap();

        // Second: large file (original_file_size > 0)
        let chunks2 = vec![RecoveryChunk::new(0, 0, b"chunk".to_vec())];
        storage
            .save_recovery("large-file", chunks2, None, Some("Large"), None, 100, 105)
            .unwrap();

        // List should show both
        let entries = storage.list_entries().unwrap();
        assert_eq!(entries.len(), 2);

        // Verify original_file_size
        let new_entry = entries.iter().find(|e| e.id == "new-buffer").unwrap();
        let large_entry = entries.iter().find(|e| e.id == "large-file").unwrap();

        assert_eq!(new_entry.metadata.original_file_size, 0);
        assert_eq!(large_entry.metadata.original_file_size, 100);

        // Both should have valid checksums
    }

    // ========================================================================
    // Property tests for recovery persistence/restore layer
    // ========================================================================

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        /// Generate a random original file content (small sizes for effective testing)
        fn original_content_strategy() -> impl Strategy<Value = Vec<u8>> {
            // Use smaller sizes (8-256 bytes) to more effectively test edge cases
            prop::collection::vec(any::<u8>(), 8..256)
        }

        /// A modification operation on a buffer
        #[derive(Debug, Clone)]
        enum Modification {
            /// Insert content at a position (offset, content)
            Insert { offset: usize, content: Vec<u8> },
            /// Replace content at a position (offset, len_to_replace, new_content)
            Replace {
                offset: usize,
                original_len: usize,
                content: Vec<u8>,
            },
            /// Delete content at a position (offset, len)
            Delete { offset: usize, len: usize },
        }

        /// Apply a modification to content, returning (new_content, RecoveryChunk)
        fn apply_modification(
            content: &[u8],
            modification: &Modification,
        ) -> (Vec<u8>, RecoveryChunk) {
            match modification {
                Modification::Insert {
                    offset,
                    content: new_bytes,
                } => {
                    let offset = (*offset).min(content.len());
                    let mut new_content = content[..offset].to_vec();
                    new_content.extend_from_slice(new_bytes);
                    new_content.extend_from_slice(&content[offset..]);
                    let chunk = RecoveryChunk::new(offset, 0, new_bytes.clone());
                    (new_content, chunk)
                }
                Modification::Replace {
                    offset,
                    original_len,
                    content: new_bytes,
                } => {
                    let offset = (*offset).min(content.len());
                    let original_len = (*original_len).min(content.len() - offset);
                    let mut new_content = content[..offset].to_vec();
                    new_content.extend_from_slice(new_bytes);
                    new_content.extend_from_slice(&content[offset + original_len..]);
                    let chunk = RecoveryChunk::new(offset, original_len, new_bytes.clone());
                    (new_content, chunk)
                }
                Modification::Delete { offset, len } => {
                    let offset = (*offset).min(content.len());
                    let len = (*len).min(content.len() - offset);
                    let mut new_content = content[..offset].to_vec();
                    new_content.extend_from_slice(&content[offset + len..]);
                    let chunk = RecoveryChunk::new(offset, len, Vec::new());
                    (new_content, chunk)
                }
            }
        }

        /// Generate modifications at specific positions (beginning, middle, end)
        fn position_targeted_modification(
            buffer_size: usize,
        ) -> impl Strategy<Value = (String, Modification)> {
            if buffer_size == 0 {
                Just((
                    "beginning".to_string(),
                    Modification::Insert {
                        offset: 0,
                        content: b"inserted".to_vec(),
                    },
                ))
                .boxed()
            } else {
                let middle = buffer_size / 2;
                let end = buffer_size;
                prop_oneof![
                    // Beginning: insert/replace/delete at offset 0
                    prop_oneof![
                        prop::collection::vec(any::<u8>(), 1..16)
                            .prop_map(|content| Modification::Insert { offset: 0, content }),
                        prop::collection::vec(any::<u8>(), 0..16).prop_map(move |content| {
                            Modification::Replace {
                                offset: 0,
                                original_len: 1.min(buffer_size),
                                content,
                            }
                        }),
                        Just(Modification::Delete {
                            offset: 0,
                            len: 1.min(buffer_size),
                        }),
                    ]
                    .prop_map(|m| ("beginning".to_string(), m)),
                    // Middle: insert/replace/delete around middle
                    prop_oneof![
                        prop::collection::vec(any::<u8>(), 1..16).prop_map(move |content| {
                            Modification::Insert {
                                offset: middle,
                                content,
                            }
                        }),
                        prop::collection::vec(any::<u8>(), 0..16).prop_map(move |content| {
                            Modification::Replace {
                                offset: middle,
                                original_len: 1.min(buffer_size - middle),
                                content,
                            }
                        }),
                        Just(Modification::Delete {
                            offset: middle,
                            len: 1.min(buffer_size - middle),
                        }),
                    ]
                    .prop_map(|m| ("middle".to_string(), m)),
                    // End: insert at end or modify last bytes
                    prop_oneof![
                        prop::collection::vec(any::<u8>(), 1..16).prop_map(move |content| {
                            Modification::Insert {
                                offset: end,
                                content,
                            }
                        }),
                        prop::collection::vec(any::<u8>(), 0..16).prop_map(move |content| {
                            Modification::Replace {
                                offset: end.saturating_sub(1),
                                original_len: 1.min(buffer_size),
                                content,
                            }
                        }),
                        Just(Modification::Delete {
                            offset: end.saturating_sub(1),
                            len: 1.min(buffer_size),
                        }),
                    ]
                    .prop_map(|m| ("end".to_string(), m)),
                ]
                .boxed()
            }
        }

        proptest! {
            /// Property: Single modification at any position should round-trip correctly
            #[test]
            fn prop_single_modification_roundtrip(
                original in original_content_strategy(),
                seed in any::<usize>()
            ) {
                let (storage, temp_dir) = create_test_storage();

                // Create original file
                let original_path = temp_dir.path().join("original.txt");
                fs::write(&original_path, &original).unwrap();

                // Generate a modification based on content size
                let modification = if original.is_empty() {
                    Modification::Insert {
                        offset: 0,
                        content: format!("seed{}", seed).into_bytes(),
                    }
                } else {
                    // Use seed to deterministically pick modification type and position
                    let mod_type = seed % 3;
                    let offset = seed % (original.len() + 1);
                    match mod_type {
                        0 => Modification::Insert {
                            offset: offset.min(original.len()),
                            content: format!("ins{}", seed).into_bytes(),
                        },
                        1 if offset < original.len() => Modification::Replace {
                            offset,
                            original_len: 1.min(original.len() - offset),
                            content: format!("rep{}", seed).into_bytes(),
                        },
                        _ if offset < original.len() => Modification::Delete {
                            offset,
                            len: 1.min(original.len() - offset),
                        },
                        _ => Modification::Insert {
                            offset: original.len(),
                            content: format!("end{}", seed).into_bytes(),
                        },
                    }
                };

                // Apply modification to get expected result
                let (expected_content, chunk) = apply_modification(&original, &modification);

                // Save recovery
                let id = "test-prop";
                storage
                    .save_recovery(
                        id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        expected_content.len(),
                    )
                    .unwrap();

                // Reconstruct and verify
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                prop_assert_eq!(
                    reconstructed,
                    expected_content,
                    "Reconstructed content doesn't match expected after modification"
                );
            }

            /// Property: Multiple non-overlapping modifications should round-trip correctly
            #[test]
            fn prop_multiple_modifications_roundtrip(
                original in prop::collection::vec(any::<u8>(), 32..128),
                num_mods in 1..5usize,
                seed in any::<u64>()
            ) {

                let (storage, temp_dir) = create_test_storage();

                // Create original file
                let original_path = temp_dir.path().join("original.txt");
                fs::write(&original_path, &original).unwrap();

                // Generate non-overlapping modification positions
                // We'll divide the buffer into segments and modify each segment independently
                let segment_size = original.len() / (num_mods + 1);
                if segment_size < 2 {
                    // Buffer too small for multiple non-overlapping mods
                    return Ok(());
                }

                let mut expected_content = original.clone();
                let mut chunks = Vec::new();

                // Track cumulative offset shift for chunk positions
                let mut offset_shift: isize = 0;

                for i in 0..num_mods {
                    let segment_start = i * segment_size;
                    let segment_end = segment_start + segment_size;

                    // Modification within this segment of the ORIGINAL content
                    let mod_offset_in_original = segment_start + (seed as usize + i) % (segment_size / 2).max(1);
                    let mod_len = 1.min(segment_end - mod_offset_in_original);

                    let mod_type = (seed as usize + i) % 3;
                    let new_bytes: Vec<u8> = format!("m{}", i).into_bytes();

                    // Calculate position in current expected_content
                    let current_offset = (mod_offset_in_original as isize + offset_shift) as usize;

                    match mod_type {
                        0 => {
                            // Insert
                            let insert_pos = current_offset.min(expected_content.len());
                            expected_content.splice(insert_pos..insert_pos, new_bytes.iter().cloned());
                            chunks.push(RecoveryChunk::new(
                                mod_offset_in_original,
                                0,
                                new_bytes.clone(),
                            ));
                            offset_shift += new_bytes.len() as isize;
                        }
                        1 => {
                            // Replace
                            if current_offset < expected_content.len() {
                                let replace_len = mod_len.min(expected_content.len() - current_offset);
                                expected_content.splice(
                                    current_offset..current_offset + replace_len,
                                    new_bytes.iter().cloned(),
                                );
                                chunks.push(RecoveryChunk::new(
                                    mod_offset_in_original,
                                    replace_len,
                                    new_bytes.clone(),
                                ));
                                offset_shift += new_bytes.len() as isize - replace_len as isize;
                            }
                        }
                        _ => {
                            // Delete
                            if current_offset < expected_content.len() {
                                let delete_len = mod_len.min(expected_content.len() - current_offset);
                                expected_content.splice(current_offset..current_offset + delete_len, []);
                                chunks.push(RecoveryChunk::new(mod_offset_in_original, delete_len, Vec::new()));
                                offset_shift -= delete_len as isize;
                            }
                        }
                    }
                }

                if chunks.is_empty() {
                    return Ok(());
                }

                // Save recovery
                let id = "test-multi";
                storage
                    .save_recovery(
                        id,
                        chunks,
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        expected_content.len(),
                    )
                    .unwrap();

                // Reconstruct and verify
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                prop_assert_eq!(
                    reconstructed,
                    expected_content,
                    "Reconstructed content doesn't match expected after multiple modifications"
                );
            }

            /// Property: Modifications at beginning, middle, and end positions
            #[test]
            fn prop_position_targeted_modifications(
                original in prop::collection::vec(any::<u8>(), 16..64),
                (position_name, modification) in prop::collection::vec(any::<u8>(), 16..64)
                    .prop_flat_map(|v| position_targeted_modification(v.len()))
            ) {
                let (storage, temp_dir) = create_test_storage();

                // Create original file
                let original_path = temp_dir.path().join("original.txt");
                fs::write(&original_path, &original).unwrap();

                // Apply modification
                let (expected_content, chunk) = apply_modification(&original, &modification);

                // Save recovery
                let id = format!("test-{}", position_name);
                storage
                    .save_recovery(
                        &id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        expected_content.len(),
                    )
                    .unwrap();

                // Reconstruct and verify
                let reconstructed = storage.reconstruct_from_chunks(&id, &original_path).unwrap();
                prop_assert_eq!(
                    reconstructed,
                    expected_content,
                    "Reconstructed content doesn't match expected for {} modification",
                    position_name
                );
            }

            /// Property: Empty modifications (no change) should return original content
            #[test]
            fn prop_empty_modification(original in original_content_strategy()) {
                if original.is_empty() {
                    return Ok(());
                }

                let (storage, temp_dir) = create_test_storage();

                // Create original file
                let original_path = temp_dir.path().join("original.txt");
                fs::write(&original_path, &original).unwrap();

                // Replace 0 bytes with 0 bytes (no-op)
                let chunk = RecoveryChunk::new(0, 0, Vec::new());

                // Save recovery
                let id = "test-empty";
                storage
                    .save_recovery(
                        id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        original.len(),
                    )
                    .unwrap();

                // Reconstruct and verify
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                prop_assert_eq!(
                    reconstructed,
                    original,
                    "Empty modification should return original content"
                );
            }

            /// Property: Full replacement should work correctly
            #[test]
            fn prop_full_replacement(
                original in prop::collection::vec(any::<u8>(), 8..64),
                replacement in prop::collection::vec(any::<u8>(), 1..64)
            ) {
                let (storage, temp_dir) = create_test_storage();

                // Create original file
                let original_path = temp_dir.path().join("original.txt");
                fs::write(&original_path, &original).unwrap();

                // Replace entire content
                let chunk = RecoveryChunk::new(0, original.len(), replacement.clone());

                // Save recovery
                let id = "test-full-replace";
                storage
                    .save_recovery(
                        id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        replacement.len(),
                    )
                    .unwrap();

                // Reconstruct and verify
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                prop_assert_eq!(
                    reconstructed,
                    replacement,
                    "Full replacement should return new content"
                );
            }

            /// Property: Prepend (insert at beginning) should work correctly
            #[test]
            fn prop_prepend(
                original in prop::collection::vec(any::<u8>(), 1..64),
                prefix in prop::collection::vec(any::<u8>(), 1..32)
            ) {
                let (storage, temp_dir) = create_test_storage();

                // Create original file
                let original_path = temp_dir.path().join("original.txt");
                fs::write(&original_path, &original).unwrap();

                // Insert at beginning (offset=0, original_len=0)
                let chunk = RecoveryChunk::new(0, 0, prefix.clone());

                // Expected: prefix + original
                let mut expected = prefix.clone();
                expected.extend_from_slice(&original);

                // Save recovery
                let id = "test-prepend";
                storage
                    .save_recovery(
                        id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        expected.len(),
                    )
                    .unwrap();

                // Reconstruct and verify
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                prop_assert_eq!(reconstructed, expected, "Prepend should work correctly");
            }

            /// Property: Append (insert at end) should work correctly
            #[test]
            fn prop_append(
                original in prop::collection::vec(any::<u8>(), 1..64),
                suffix in prop::collection::vec(any::<u8>(), 1..32)
            ) {
                let (storage, temp_dir) = create_test_storage();

                // Create original file
                let original_path = temp_dir.path().join("original.txt");
                fs::write(&original_path, &original).unwrap();

                // Insert at end
                let chunk = RecoveryChunk::new(original.len(), 0, suffix.clone());

                // Expected: original + suffix
                let mut expected = original.clone();
                expected.extend_from_slice(&suffix);

                // Save recovery
                let id = "test-append";
                storage
                    .save_recovery(
                        id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        expected.len(),
                    )
                    .unwrap();

                // Reconstruct and verify
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                prop_assert_eq!(reconstructed, expected, "Append should work correctly");
            }

            /// Property: Delete all content should result in empty
            #[test]
            fn prop_delete_all(original in prop::collection::vec(any::<u8>(), 1..64)) {
                let (storage, temp_dir) = create_test_storage();

                // Create original file
                let original_path = temp_dir.path().join("original.txt");
                fs::write(&original_path, &original).unwrap();

                // Delete all content
                let chunk = RecoveryChunk::new(0, original.len(), Vec::new());

                // Save recovery
                let id = "test-delete-all";
                storage
                    .save_recovery(
                        id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        0,
                    )
                    .unwrap();

                // Reconstruct and verify
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                prop_assert!(reconstructed.is_empty(), "Delete all should result in empty content");
            }

            /// Property: Save and load cycle preserves chunk metadata
            #[test]
            fn prop_chunk_metadata_preserved(
                offset in 0..1000usize,
                original_len in 0..100usize,
                content in prop::collection::vec(any::<u8>(), 0..64)
            ) {
                let (storage, _temp) = create_test_storage();

                let chunk = RecoveryChunk::new(offset, original_len, content.clone());

                // Save recovery
                let id = "test-metadata";
                storage
                    .save_recovery(id, vec![chunk], None, None, None, 1000, 1000 - original_len + content.len())
                    .unwrap();

                // Load and verify chunk metadata
                let chunked_data = storage.read_chunked_content(id).unwrap().unwrap();
                prop_assert_eq!(chunked_data.chunks.len(), 1);
                prop_assert_eq!(chunked_data.chunks[0].offset, offset);
                prop_assert_eq!(chunked_data.chunks[0].original_len, original_len);
                prop_assert_eq!(&chunked_data.chunks[0].content, &content);
            }

            /// Property: Multiple chunks in order are preserved
            #[test]
            fn prop_multiple_chunks_order_preserved(
                chunks_data in prop::collection::vec(
                    (0..100usize, 0..10usize, prop::collection::vec(any::<u8>(), 0..16)),
                    1..5
                )
            ) {
                let (storage, _temp) = create_test_storage();

                // Create chunks with increasing offsets
                let mut offset = 0;
                let mut chunks = Vec::new();
                for (delta, original_len, content) in chunks_data {
                    offset += delta;
                    chunks.push(RecoveryChunk::new(offset, original_len, content));
                    offset += original_len.max(1); // Ensure non-overlapping
                }

                // Save recovery
                let id = "test-order";
                let original_size = offset + 100; // Ensure original is large enough
                storage
                    .save_recovery(id, chunks.clone(), None, None, None, original_size, original_size)
                    .unwrap();

                // Load and verify order
                let chunked_data = storage.read_chunked_content(id).unwrap().unwrap();
                prop_assert_eq!(chunked_data.chunks.len(), chunks.len());

                for (i, (saved, loaded)) in chunks.iter().zip(chunked_data.chunks.iter()).enumerate() {
                    prop_assert_eq!(
                        saved.offset, loaded.offset,
                        "Chunk {} offset mismatch", i
                    );
                    prop_assert_eq!(
                        saved.original_len, loaded.original_len,
                        "Chunk {} original_len mismatch", i
                    );
                    prop_assert_eq!(
                        &saved.content, &loaded.content,
                        "Chunk {} content mismatch", i
                    );
                }
            }
        }

        // Non-proptest regression tests for specific edge cases
        #[test]
        fn test_insert_at_every_position() {
            let (storage, temp_dir) = create_test_storage();
            let original = b"ABCDEFGH";

            for insert_pos in 0..=original.len() {
                let original_path = temp_dir.path().join(format!("original_{}.txt", insert_pos));
                fs::write(&original_path, original).unwrap();

                let insert_content = b"XYZ".to_vec();
                let chunk = RecoveryChunk::new(insert_pos, 0, insert_content.clone());

                let mut expected = original[..insert_pos].to_vec();
                expected.extend_from_slice(&insert_content);
                expected.extend_from_slice(&original[insert_pos..]);

                let id = format!("insert-{}", insert_pos);
                storage
                    .save_recovery(
                        &id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        expected.len(),
                    )
                    .unwrap();

                let reconstructed = storage
                    .reconstruct_from_chunks(&id, &original_path)
                    .unwrap();
                assert_eq!(
                    reconstructed, expected,
                    "Insert at position {} failed",
                    insert_pos
                );
            }
        }

        #[test]
        fn test_delete_at_every_position() {
            let (storage, temp_dir) = create_test_storage();
            let original = b"ABCDEFGH";

            for delete_pos in 0..original.len() {
                let original_path = temp_dir.path().join(format!("original_{}.txt", delete_pos));
                fs::write(&original_path, original).unwrap();

                let chunk = RecoveryChunk::new(delete_pos, 1, Vec::new());

                let mut expected = original[..delete_pos].to_vec();
                expected.extend_from_slice(&original[delete_pos + 1..]);

                let id = format!("delete-{}", delete_pos);
                storage
                    .save_recovery(
                        &id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        expected.len(),
                    )
                    .unwrap();

                let reconstructed = storage
                    .reconstruct_from_chunks(&id, &original_path)
                    .unwrap();
                assert_eq!(
                    reconstructed, expected,
                    "Delete at position {} failed",
                    delete_pos
                );
            }
        }

        #[test]
        fn test_replace_at_every_position() {
            let (storage, temp_dir) = create_test_storage();
            let original = b"ABCDEFGH";

            for replace_pos in 0..original.len() {
                let original_path = temp_dir
                    .path()
                    .join(format!("original_{}.txt", replace_pos));
                fs::write(&original_path, original).unwrap();

                let replace_content = b"XY".to_vec();
                let replace_len = 1;
                let chunk = RecoveryChunk::new(replace_pos, replace_len, replace_content.clone());

                let mut expected = original[..replace_pos].to_vec();
                expected.extend_from_slice(&replace_content);
                expected.extend_from_slice(&original[replace_pos + replace_len..]);

                let id = format!("replace-{}", replace_pos);
                storage
                    .save_recovery(
                        &id,
                        vec![chunk],
                        Some(&original_path),
                        None,
                        None,
                        original.len(),
                        expected.len(),
                    )
                    .unwrap();

                let reconstructed = storage
                    .reconstruct_from_chunks(&id, &original_path)
                    .unwrap();
                assert_eq!(
                    reconstructed, expected,
                    "Replace at position {} failed",
                    replace_pos
                );
            }
        }

        #[test]
        fn test_combined_beginning_middle_end_modifications() {
            let (storage, temp_dir) = create_test_storage();
            let original = b"0123456789ABCDEF";

            let original_path = temp_dir.path().join("original.txt");
            fs::write(&original_path, original).unwrap();

            // Three chunks:
            // 1. Insert "PRE-" at beginning (offset 0, len 0)
            // 2. Replace "567" with "XXX" at position 5 (offset 5, len 3)
            // 3. Insert "-POST" at end (offset 16, len 0)
            let chunks = vec![
                RecoveryChunk::new(0, 0, b"PRE-".to_vec()),
                RecoveryChunk::new(5, 3, b"XXX".to_vec()),
                RecoveryChunk::new(16, 0, b"-POST".to_vec()),
            ];

            // Expected: "PRE-01234XXX89ABCDEF-POST"
            // Original: 0123456789ABCDEF
            // After chunk 1 (insert at 0): PRE- + 0123456789ABCDEF = PRE-0123456789ABCDEF
            // After chunk 2 (replace 567): PRE-01234XXX89ABCDEF
            // After chunk 3 (insert at 16): PRE-01234XXX89ABCDEF-POST
            let expected = b"PRE-01234XXX89ABCDEF-POST";

            let id = "combined";
            storage
                .save_recovery(
                    id,
                    chunks,
                    Some(&original_path),
                    None,
                    None,
                    original.len(),
                    expected.len(),
                )
                .unwrap();

            let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
            assert_eq!(reconstructed, expected.to_vec());
        }

        #[test]
        fn test_adjacent_modifications() {
            let (storage, temp_dir) = create_test_storage();
            let original = b"AABBCCDD";

            let original_path = temp_dir.path().join("original.txt");
            fs::write(&original_path, original).unwrap();

            // Adjacent modifications: replace AA with X, then BB with Y
            // These should not overlap in the original positions
            let chunks = vec![
                RecoveryChunk::new(0, 2, b"X".to_vec()), // Replace AA with X
                RecoveryChunk::new(2, 2, b"Y".to_vec()), // Replace BB with Y
            ];

            // Original: AABBCCDD
            // After applying chunks: X + Y + CCDD
            let expected = b"XYCCDD";

            let id = "adjacent";
            storage
                .save_recovery(
                    id,
                    chunks,
                    Some(&original_path),
                    None,
                    None,
                    original.len(),
                    expected.len(),
                )
                .unwrap();

            let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
            assert_eq!(reconstructed, expected.to_vec());
        }

        #[test]
        fn test_single_byte_operations() {
            let (storage, temp_dir) = create_test_storage();
            let original = b"A";

            // Insert before single byte
            {
                let original_path = temp_dir.path().join("single_insert.txt");
                fs::write(&original_path, original).unwrap();
                let chunk = RecoveryChunk::new(0, 0, b"X".to_vec());
                let id = "single-insert";
                storage
                    .save_recovery(id, vec![chunk], Some(&original_path), None, None, 1, 2)
                    .unwrap();
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                assert_eq!(reconstructed, b"XA".to_vec());
            }

            // Replace single byte
            {
                let original_path = temp_dir.path().join("single_replace.txt");
                fs::write(&original_path, original).unwrap();
                let chunk = RecoveryChunk::new(0, 1, b"X".to_vec());
                let id = "single-replace";
                storage
                    .save_recovery(id, vec![chunk], Some(&original_path), None, None, 1, 1)
                    .unwrap();
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                assert_eq!(reconstructed, b"X".to_vec());
            }

            // Delete single byte
            {
                let original_path = temp_dir.path().join("single_delete.txt");
                fs::write(&original_path, original).unwrap();
                let chunk = RecoveryChunk::new(0, 1, Vec::new());
                let id = "single-delete";
                storage
                    .save_recovery(id, vec![chunk], Some(&original_path), None, None, 1, 0)
                    .unwrap();
                let reconstructed = storage.reconstruct_from_chunks(id, &original_path).unwrap();
                assert!(reconstructed.is_empty());
            }
        }
    }
}
