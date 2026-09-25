//! Test for large file in-place write corruption bug
//!
//! This test reproduces a data corruption bug that occurs when:
//! 1. A large file is opened (triggering large file mode with lazy loading)
//! 2. The file is not owned by the current user (triggering in-place write mode)
//! 3. The file is edited and saved
//!
//! The bug: In-place write mode truncates the file BEFORE reading Copy operations
//! from it, causing data loss/corruption.
//!
//! ## Technical Details
//!
//! In `save_with_inplace_write()` (buffer.rs):
//! 1. File is opened with `truncate(true)` - this empties the file
//! 2. `write_recipe_to_file()` iterates through the recipe
//! 3. For Copy actions, it tries to read from the source file (same file we just truncated!)
//! 4. Result: reads fail or return empty data = corruption
//!
//! ## When This Bug Occurs
//!
//! - File must be in large file mode (above `large_file_threshold_bytes`)
//! - File must not be owned by current user (`is_owner()` returns false)
//!   - This happens with group-writable files owned by other users
//!   - Or files with world-write permissions
//!
//! ## Note
//!
//! This is a pre-existing bug, not related to the auto-save feature.
//! This test can be run on the master branch to reproduce the issue.
//! The auto-save feature just makes the bug more likely to trigger automatically.

use fresh::model::buffer::TextBuffer;
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSystem, FileWriter, StdFileSystem,
};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

/// A filesystem wrapper that pretends we don't own any files.
/// This forces the in-place write path to be used instead of atomic writes.
struct NotOwnerFileSystem {
    inner: Arc<dyn FileSystem>,
}

impl NotOwnerFileSystem {
    fn new(inner: Arc<dyn FileSystem>) -> Self {
        Self { inner }
    }
}

impl FileSystem for NotOwnerFileSystem {
    // Override is_owner to always return false, simulating a file owned by another user
    fn is_owner(&self, _path: &Path) -> bool {
        false
    }

    // Delegate all other methods to inner filesystem
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.inner.read_file(path)
    }

    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.inner.read_range(path, offset, len)
    }

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

    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.metadata(path)
    }

    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.symlink_metadata(path)
    }

    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_dir(path)
    }

    fn is_file(&self, path: &Path) -> io::Result<bool> {
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
        self.inner.canonicalize(path)
    }

    fn current_uid(&self) -> u32 {
        self.inner.current_uid()
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

    fn search_file(
        &self,
        path: &Path,
        pattern: &str,
        opts: &fresh::model::filesystem::FileSearchOptions,
        cursor: &mut fresh::model::filesystem::FileSearchCursor,
    ) -> io::Result<Vec<fresh::model::filesystem::SearchMatch>> {
        fresh::model::filesystem::default_search_file(&*self.inner, path, pattern, opts, cursor)
    }

    fn walk(
        &self,
        root: &Path,
        opts: &fresh_editor_core::model::filesystem::WalkOptions<'_>,
        cancel: &std::sync::atomic::AtomicBool,
        on_entry: &mut dyn FnMut(fresh_editor_core::model::filesystem::WalkEntry<'_>) -> bool,
    ) -> std::io::Result<()> {
        self.inner.walk(root, opts, cancel, on_entry)
    }
}

/// Test that saving a large file with in-place write mode preserves all data.
///
/// This is a regression test for a bug where in-place writes corrupt large files:
/// - In-place write is used when the file is owned by a different user
/// - The file is truncated before Copy operations read from it
/// - Copy operations then read from the truncated (empty) file
/// - Result: data corruption
#[test]
#[cfg(unix)] // In-place write is only used on Unix for ownership preservation
fn test_large_file_inplace_write_corruption() {
    use std::fs;

    // In-place writes stage in the recovery dir: keep them out of the real one.
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_inplace_test.txt");

    // Create a large file with recognizable content
    // Use line numbers so we can verify specific regions survived
    let mut content = String::new();
    for i in 0..1000 {
        content.push_str(&format!(
            "Line {:04}: This is test content that should survive the save\n",
            i
        ));
    }
    let original_len = content.len();
    fs::write(&file_path, &content).unwrap();

    // Create a filesystem that lies about ownership (simulates file owned by another user)
    let not_owner_fs = Arc::new(NotOwnerFileSystem::new(Arc::new(StdFileSystem)));

    // Open with large file mode (threshold smaller than file size)
    let threshold = 1024; // 1KB threshold, file is ~60KB
    let mut buffer = TextBuffer::load_from_file(&file_path, threshold, not_owner_fs).unwrap();

    // Verify we're in large file mode
    assert!(
        buffer.line_count().is_none(),
        "Should be in large file mode (no line indexing)"
    );
    assert!(buffer.is_large_file(), "Should report as large file");

    // Make a small edit at the beginning
    // This creates an Insert action, while the rest of the file remains as Copy actions
    buffer.insert_bytes(0, b"EDITED: ".to_vec());

    // Save the file - this is where the bug manifests
    // With the bug: file is truncated, then Copy ops read from truncated file = corruption
    // Without the bug: all content should be preserved
    let save_result = buffer.save();

    // The bug can manifest in two ways:
    // 1. Save fails with "failed to fill whole buffer" because Copy ops can't read truncated file
    // 2. Save "succeeds" but produces a corrupted/truncated file
    //
    // Either way indicates the bug is present.
    if let Err(e) = save_result {
        panic!(
            "BUG CONFIRMED: Save failed with error: {}\n\
             This happens because in-place write truncates the file before Copy operations\n\
             can read from it. The Copy operations then fail because the file is empty/truncated.",
            e
        );
    }

    // Verify the saved content
    let saved_content = fs::read_to_string(&file_path).unwrap();

    // The file should have our edit + all original content
    let expected_len = original_len + 8; // "EDITED: " is 8 bytes

    // Primary check: file size should match expected
    // If the bug is present, the file will be much smaller (only the Insert data)
    assert_eq!(
        saved_content.len(),
        expected_len,
        "File size mismatch! Expected {} bytes but got {} bytes.\n\
         If the file is much smaller, this indicates the in-place write corruption bug:\n\
         - File was truncated before Copy operations could read from it\n\
         - Only Insert data survived, Copy regions were lost\n\
         Saved content starts with: {:?}",
        expected_len,
        saved_content.len(),
        &saved_content[..saved_content.len().min(100)]
    );

    // Verify the edit is present at the beginning
    assert!(
        saved_content.starts_with("EDITED: Line 0000"),
        "Edit should be at the start of file"
    );

    // Verify content from the MIDDLE of the file survived (this would be a Copy region)
    assert!(
        saved_content.contains("Line 0500"),
        "Middle content should survive (Line 0500)"
    );

    // Verify content from the END of the file survived (also a Copy region)
    assert!(
        saved_content.contains("Line 0999"),
        "End content should survive (Line 0999)"
    );
}

/// Additional test: verify the bug with multiple edits at different positions
#[test]
#[cfg(unix)]
fn test_large_file_inplace_write_multiple_edits() {
    use std::fs;

    // In-place writes stage in the recovery dir: keep them out of the real one.
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_multi_edit_test.txt");

    // Create file with 500 lines (each line is ~60 bytes)
    let mut content = String::new();
    for i in 0..500 {
        content.push_str(&format!(
            "Line {:04}: ====================================================\n",
            i
        ));
    }
    let original_len = content.len();
    fs::write(&file_path, &content).unwrap();

    let not_owner_fs = Arc::new(NotOwnerFileSystem::new(Arc::new(StdFileSystem)));
    let threshold = 1024;
    let mut buffer = TextBuffer::load_from_file(&file_path, threshold, not_owner_fs).unwrap();

    assert!(buffer.is_large_file());

    // Make edits at beginning
    buffer.insert_bytes(0, b"START>>>".to_vec());

    // Calculate position for middle edit (roughly line 250)
    // Each line is about 65 bytes
    let middle_pos = 250 * 65 + 8; // +8 for START>>> we just inserted
    buffer.insert_bytes(middle_pos, b"<<<MIDDLE>>>".to_vec());

    // Save - may fail due to the bug
    let save_result = buffer.save();
    if let Err(e) = save_result {
        panic!(
            "BUG CONFIRMED: Save failed with error: {}\n\
             In-place write corrupted the file by truncating before Copy operations.",
            e
        );
    }

    // Verify
    let saved = fs::read_to_string(&file_path).unwrap();

    // Check that file has reasonable size (should be original + edits)
    let expected_min_len = original_len; // At minimum, should have original content
    assert!(
        saved.len() >= expected_min_len,
        "File is too small! Got {} bytes, expected at least {} bytes.\n\
         This indicates data loss from the in-place write bug.",
        saved.len(),
        expected_min_len
    );

    // Check our edits are present
    assert!(
        saved.starts_with("START>>>"),
        "START marker should be present"
    );
    assert!(
        saved.contains("<<<MIDDLE>>>"),
        "MIDDLE marker should be present"
    );

    // Check that content from various regions survived
    assert!(saved.contains("Line 0100"), "Line 100 should survive");
    assert!(saved.contains("Line 0300"), "Line 300 should survive");
    assert!(saved.contains("Line 0499"), "Line 499 should survive");
}

/// A filesystem wrapper that simulates a crash during the streaming phase.
/// It fails on the first write to the destination file (simulating crash mid-stream).
struct CrashDuringStreamFileSystem {
    inner: Arc<dyn FileSystem>,
    /// Path to the destination file (writes to this path will fail)
    dest_path: PathBuf,
    /// When set, opening the destination for writing fails with this error
    /// instead (it is never opened, so never truncated).
    refuse_open: std::sync::Mutex<Option<io::ErrorKind>>,
}

impl CrashDuringStreamFileSystem {
    fn new(inner: Arc<dyn FileSystem>, dest_path: PathBuf) -> Self {
        Self {
            inner,
            dest_path,
            refuse_open: std::sync::Mutex::new(None),
        }
    }

    /// Make opening the destination for writing fail with `kind` from now on.
    fn refuse_open(&self, kind: io::ErrorKind) {
        *self.refuse_open.lock().unwrap() = Some(kind);
    }
}

/// A file writer that simulates crash by failing on write
struct CrashingFileWriter {
    inner: Box<dyn FileWriter>,
    should_crash: bool,
}

impl std::io::Write for CrashingFileWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.should_crash {
            // Simulate crash on write to destination
            return Err(io::Error::other("simulated crash during write"));
        }
        self.inner.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

impl FileWriter for CrashingFileWriter {
    fn sync_all(&self) -> io::Result<()> {
        Ok(())
    }
}

impl FileSystem for CrashDuringStreamFileSystem {
    fn is_owner(&self, _path: &Path) -> bool {
        false // Force in-place write
    }

    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        if path == self.dest_path {
            if let Some(kind) = *self.refuse_open.lock().unwrap() {
                return Err(io::Error::new(kind, "simulated refusal to open"));
            }
        }
        let inner = self.inner.open_file_for_write(path)?;
        if path == self.dest_path {
            // Wrap with crashing writer for destination file
            Ok(Box::new(CrashingFileWriter {
                inner,
                should_crash: true,
            }))
        } else {
            Ok(inner)
        }
    }

    // Delegate all other methods to inner filesystem
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.inner.read_file(path)
    }

    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.inner.read_range(path, offset, len)
    }

    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.inner.write_file(path, data)
    }

    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_file(path)
    }

    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        self.inner.open_file(path)
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

    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.metadata(path)
    }

    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.symlink_metadata(path)
    }

    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_dir(path)
    }

    fn is_file(&self, path: &Path) -> io::Result<bool> {
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
        self.inner.canonicalize(path)
    }

    fn current_uid(&self) -> u32 {
        self.inner.current_uid()
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

    fn search_file(
        &self,
        path: &Path,
        pattern: &str,
        opts: &fresh::model::filesystem::FileSearchOptions,
        cursor: &mut fresh::model::filesystem::FileSearchCursor,
    ) -> io::Result<Vec<fresh::model::filesystem::SearchMatch>> {
        fresh::model::filesystem::default_search_file(&*self.inner, path, pattern, opts, cursor)
    }

    fn walk(
        &self,
        root: &Path,
        opts: &fresh_editor_core::model::filesystem::WalkOptions<'_>,
        cancel: &std::sync::atomic::AtomicBool,
        on_entry: &mut dyn FnMut(fresh_editor_core::model::filesystem::WalkEntry<'_>) -> bool,
    ) -> std::io::Result<()> {
        self.inner.walk(root, opts, cancel, on_entry)
    }
}

/// Test that crash during in-place write streaming leaves recovery files intact.
///
/// This test verifies the crash recovery mechanism:
/// 1. Create a large file and open it with a filesystem that simulates crash
/// 2. Make edits and attempt to save (will fail during streaming)
/// 3. Verify that the temp file and recovery metadata exist
/// 4. Verify that the temp file contains the correct (complete) content
/// 5. Verify that RecoveryStorage can find the orphaned in-place write
#[test]
#[cfg(unix)]
fn test_inplace_write_crash_recovery() {
    use std::fs;

    // In-place writes stage in the recovery dir: keep them out of the real one.
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crash_test.txt");

    // Create a large file with recognizable content
    let mut content = String::new();
    for i in 0..500 {
        content.push_str(&format!(
            "Line {:04}: Original content that should be recoverable\n",
            i
        ));
    }
    let original_len = content.len();
    fs::write(&file_path, &content).unwrap();

    // Create filesystem that crashes during streaming
    let crash_fs = Arc::new(CrashDuringStreamFileSystem::new(
        Arc::new(StdFileSystem),
        file_path.clone(),
    ));

    // Open with large file mode
    let threshold = 1024;
    let mut buffer = TextBuffer::load_from_file(&file_path, threshold, crash_fs).unwrap();

    assert!(buffer.is_large_file(), "Should be in large file mode");

    // Make an edit
    buffer.insert_bytes(0, b"EDITED: ".to_vec());

    // Save should fail due to simulated crash
    let save_result = buffer.save();
    assert!(
        save_result.is_err(),
        "Save should fail due to simulated crash"
    );

    // Verify recovery files exist directly (can't use list_inplace_write_recoveries
    // because it filters out entries from still-running processes - i.e., this test)
    let recovery_dir = fresh::services::recovery::RecoveryStorage::get_recovery_dir().unwrap();
    let hash = fresh::services::recovery::path_hash(&file_path);
    let meta_path = recovery_dir.join(format!("{}.inplace.json", hash));

    assert!(
        meta_path.exists(),
        "Recovery metadata should exist at {:?}",
        meta_path
    );

    // Read and parse the recovery metadata
    let meta_content = fs::read_to_string(&meta_path).unwrap();
    let recovery: fresh::services::recovery::InplaceWriteRecovery =
        serde_json::from_str(&meta_content).unwrap();

    // Verify temp file exists and has correct content
    assert!(
        recovery.temp_path.exists(),
        "Temp file should exist at {:?}",
        recovery.temp_path
    );

    let temp_content = fs::read_to_string(&recovery.temp_path).unwrap();
    let expected_len = original_len + 8; // "EDITED: " is 8 bytes

    assert_eq!(
        temp_content.len(),
        expected_len,
        "Temp file should have complete content ({} bytes), got {} bytes",
        expected_len,
        temp_content.len()
    );

    assert!(
        temp_content.starts_with("EDITED: Line 0000"),
        "Temp file should start with our edit"
    );

    assert!(
        temp_content.contains("Line 0250"),
        "Temp file should contain middle content"
    );

    assert!(
        temp_content.contains("Line 0499"),
        "Temp file should contain end content"
    );

    // Verify recovery metadata has correct information
    assert_eq!(
        recovery.dest_path, file_path,
        "Recovery should point to correct destination"
    );

    // Clean up the recovery entry manually
    let _ = fs::remove_file(&recovery.temp_path);
    let _ = fs::remove_file(&meta_path);

    // Verify cleanup worked
    assert!(
        !recovery.temp_path.exists(),
        "Temp file should be deleted after cleanup"
    );
    assert!(
        !meta_path.exists(),
        "Metadata file should be deleted after cleanup"
    );
}

/// Test that the recovery mechanism can actually restore a file after a crash.
///
/// This test:
/// 1. Simulates a crash during in-place write streaming
/// 2. Verifies recovery files exist
/// 3. Performs the recovery by copying temp file to destination
/// 4. Verifies the file content is correct after recovery
#[test]
#[cfg(unix)]
fn test_inplace_write_recovery_restores_file() {
    use std::fs;

    // In-place writes stage in the recovery dir: keep them out of the real one.
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("recovery_test.txt");

    // Create a large file with recognizable content
    let mut original_content = String::new();
    for i in 0..500 {
        original_content.push_str(&format!(
            "Line {:04}: This content should be recoverable after crash\n",
            i
        ));
    }
    let original_len = original_content.len();
    fs::write(&file_path, &original_content).unwrap();

    // Create filesystem that crashes during streaming
    let crash_fs = Arc::new(CrashDuringStreamFileSystem::new(
        Arc::new(StdFileSystem),
        file_path.clone(),
    ));

    // Open with large file mode
    let threshold = 1024;
    let mut buffer = TextBuffer::load_from_file(&file_path, threshold, crash_fs).unwrap();

    assert!(buffer.is_large_file(), "Should be in large file mode");

    // Make an edit that we'll recover
    let edit_prefix = b"RECOVERED: ";
    buffer.insert_bytes(0, edit_prefix.to_vec());

    // Save should fail due to simulated crash
    let save_result = buffer.save();
    assert!(
        save_result.is_err(),
        "Save should fail due to simulated crash"
    );

    // At this point, the destination file may be corrupted (truncated/partial),
    // but the temp file should have the complete content.

    // Find the recovery entry
    let recovery_dir = fresh::services::recovery::RecoveryStorage::get_recovery_dir().unwrap();
    let hash = fresh::services::recovery::path_hash(&file_path);
    let meta_path = recovery_dir.join(format!("{}.inplace.json", hash));

    assert!(meta_path.exists(), "Recovery metadata should exist");

    // Read the recovery metadata
    let meta_content = fs::read_to_string(&meta_path).unwrap();
    let recovery: fresh::services::recovery::InplaceWriteRecovery =
        serde_json::from_str(&meta_content).unwrap();

    // Verify temp file has complete content
    assert!(
        recovery.temp_path.exists(),
        "Temp file should exist for recovery"
    );
    let temp_content = fs::read(&recovery.temp_path).unwrap();
    let expected_len = original_len + edit_prefix.len();
    assert_eq!(
        temp_content.len(),
        expected_len,
        "Temp file should have complete content"
    );

    // === PERFORM THE RECOVERY ===
    // This simulates what the editor's startup recovery would do:
    // Copy the temp file content to the destination

    fs::write(&file_path, &temp_content).unwrap();

    // Optionally restore permissions (skip in test since we own the file)
    // In real recovery, we'd use chown/chmod if we have permission

    // Verify the recovered file content is correct
    let recovered_content = fs::read_to_string(&file_path).unwrap();

    assert_eq!(
        recovered_content.len(),
        expected_len,
        "Recovered file should have correct size"
    );

    assert!(
        recovered_content.starts_with("RECOVERED: Line 0000"),
        "Recovered file should start with our edit"
    );

    assert!(
        recovered_content.contains("Line 0250"),
        "Recovered file should contain middle content"
    );

    assert!(
        recovered_content.contains("Line 0499"),
        "Recovered file should contain end content"
    );

    // Clean up recovery files (simulating what recovery does after success)
    let _ = fs::remove_file(&recovery.temp_path);
    let _ = fs::remove_file(&meta_path);

    // Verify no recovery files remain
    assert!(
        !recovery.temp_path.exists(),
        "Temp file should be cleaned up after recovery"
    );
    assert!(
        !meta_path.exists(),
        "Metadata should be cleaned up after recovery"
    );
}

/// Test that successful in-place write cleans up recovery files
#[test]
#[cfg(unix)]
fn test_successful_inplace_write_cleans_up_recovery() {
    use fresh::services::recovery::RecoveryStorage;
    use std::fs;

    // In-place writes stage in the recovery dir: keep them out of the real one.
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("success_test.txt");

    // Create a large file
    let mut content = String::new();
    for i in 0..500 {
        content.push_str(&format!("Line {:04}: Test content\n", i));
    }
    fs::write(&file_path, &content).unwrap();

    // Use NotOwnerFileSystem (doesn't crash, just forces in-place write)
    let not_owner_fs = Arc::new(NotOwnerFileSystem::new(Arc::new(StdFileSystem)));

    let threshold = 1024;
    let mut buffer = TextBuffer::load_from_file(&file_path, threshold, not_owner_fs).unwrap();

    assert!(buffer.is_large_file());

    // Make an edit
    buffer.insert_bytes(0, b"SUCCESS: ".to_vec());

    // Save should succeed
    buffer.save().unwrap();

    // Verify NO recovery files remain for this path
    let recovery_storage = RecoveryStorage::default();
    let inplace_recoveries = recovery_storage.list_inplace_write_recoveries().unwrap();

    let our_recovery = inplace_recoveries.iter().find(|r| r.dest_path == file_path);

    assert!(
        our_recovery.is_none(),
        "Successful save should clean up recovery files. Found: {:?}",
        our_recovery
    );

    // Verify the file was saved correctly
    let saved = fs::read_to_string(&file_path).unwrap();
    assert!(
        saved.starts_with("SUCCESS: Line 0000"),
        "File should have been saved correctly"
    );
}

/// Issue #3348: saving an emptied buffer skipped the in-place decision and
/// always replaced the file via temp file + rename, so a file owned by
/// another user took the saver's owner and group (root:root under sudo).
/// It must be rewritten in place like any other save of a file we don't own.
#[test]
#[cfg(unix)]
fn test_emptying_not_owned_file_writes_in_place() {
    use std::os::unix::fs::MetadataExt;

    // In-place writes stage in the recovery dir: keep them out of the real one.
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("not_mine.txt");
    std::fs::write(&file_path, "some content\n").unwrap();
    let ino = std::fs::metadata(&file_path).unwrap().ino();

    let not_owner_fs = Arc::new(NotOwnerFileSystem::new(Arc::new(StdFileSystem)));
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, not_owner_fs).unwrap();
    let len = buffer.len();
    buffer.delete_bytes(0, len);
    buffer.save().unwrap();

    assert_eq!(std::fs::read(&file_path).unwrap(), b"");
    assert_eq!(
        std::fs::metadata(&file_path).unwrap().ino(),
        ino,
        "an emptied file we don't own must be truncated in place, not replaced"
    );
}

/// A small (fully loaded) file written in place used to be truncated and
/// rewritten from memory with no staged copy, so a write failing part-way
/// left the file empty or partial with the new content nowhere on disk. It
/// must be staged in the recovery directory first, like the large-file path.
#[test]
#[cfg(unix)]
fn test_small_file_inplace_write_failure_keeps_staged_copy() {
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("small.txt");
    std::fs::write(&file_path, "original line\n").unwrap();
    let crash_fs = Arc::new(CrashDuringStreamFileSystem::new(
        Arc::new(StdFileSystem),
        file_path.clone(),
    ));
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, crash_fs).unwrap();
    assert!(!buffer.is_large_file());
    buffer.insert_bytes(0, b"EDITED: ".to_vec());

    assert!(buffer.save().is_err(), "the simulated failure must surface");

    let hash = fresh::services::recovery::path_hash(&file_path);
    let meta_path = data_dir
        .path()
        .join("recovery")
        .join(format!("{}.inplace.json", hash));
    let meta = std::fs::read_to_string(&meta_path)
        .unwrap_or_else(|e| panic!("no in-place recovery metadata at {meta_path:?}: {e}"));
    let recovery: fresh::services::recovery::InplaceWriteRecovery =
        serde_json::from_str(&meta).unwrap();
    assert_eq!(recovery.dest_path, file_path);
    assert_eq!(
        std::fs::read_to_string(&recovery.temp_path).unwrap(),
        "EDITED: original line\n",
        "the staged copy must hold the complete new content"
    );
}

/// A successful small in-place write leaves nothing behind in the recovery
/// directory.
#[test]
#[cfg(unix)]
fn test_small_file_inplace_write_cleans_up_staged_copy() {
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("small.txt");
    std::fs::write(&file_path, "original line\n").unwrap();
    let not_owner_fs = Arc::new(NotOwnerFileSystem::new(Arc::new(StdFileSystem)));
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, not_owner_fs).unwrap();
    buffer.insert_bytes(0, b"EDITED: ".to_vec());

    buffer.save().unwrap();

    assert_eq!(
        std::fs::read_to_string(&file_path).unwrap(),
        "EDITED: original line\n"
    );
    let leftovers: Vec<_> = std::fs::read_dir(data_dir.path().join("recovery"))
        .map(|dir| dir.map(|e| e.unwrap().file_name()).collect())
        .unwrap_or_default();
    assert!(leftovers.is_empty(), "left behind: {leftovers:?}");
}

/// The `.inplace-*.tmp` files staged in the recovery directory.
fn staged_copies(recovery_dir: &Path) -> Vec<PathBuf> {
    let mut staged: Vec<PathBuf> = std::fs::read_dir(recovery_dir)
        .map(|dir| dir.map(|e| e.unwrap().path()).collect())
        .unwrap_or_default();
    staged.retain(|p| {
        p.file_name()
            .and_then(|n| n.to_str())
            .is_some_and(|n| n.starts_with(".inplace-"))
    });
    staged
}

/// The copy staged for an in-place write holds the file's content outside
/// the file's own directory, so it must be readable by its owner only — not
/// created with the umask default (0644) in a world-readable data dir.
#[test]
#[cfg(unix)]
fn test_inplace_staged_copy_is_private() {
    use std::os::unix::fs::PermissionsExt;
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("secret.txt");
    std::fs::write(&file_path, "original line\n").unwrap();
    std::fs::set_permissions(&file_path, std::fs::Permissions::from_mode(0o600)).unwrap();
    let crash_fs = Arc::new(CrashDuringStreamFileSystem::new(
        Arc::new(StdFileSystem),
        file_path.clone(),
    ));
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, crash_fs).unwrap();
    buffer.insert_bytes(0, b"EDITED: ".to_vec());
    assert!(buffer.save().is_err(), "the simulated failure must surface");

    let staged = staged_copies(&data_dir.path().join("recovery"));
    assert_eq!(staged.len(), 1, "staged: {staged:?}");
    let mode = std::fs::metadata(&staged[0]).unwrap().permissions().mode();
    assert_eq!(mode & 0o777, 0o600, "staged copy mode {:o}", mode & 0o777);
}

/// Every failed in-place write of a file used to leave its own staged copy
/// and point the file's metadata at the newest, orphaning the rest — one
/// more copy per auto-save interval while the failure lasted. The latest
/// copy supersedes the earlier ones, so only it is kept.
#[test]
#[cfg(unix)]
fn test_repeated_inplace_write_failures_keep_one_staged_copy() {
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());
    let recovery_dir = data_dir.path().join("recovery");

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("small.txt");
    std::fs::write(&file_path, "original line\n").unwrap();
    let crash_fs = Arc::new(CrashDuringStreamFileSystem::new(
        Arc::new(StdFileSystem),
        file_path.clone(),
    ));
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, crash_fs).unwrap();
    for attempt in 0..3 {
        buffer.insert_bytes(0, format!("{attempt}").into_bytes());
        assert!(buffer.save().is_err(), "the simulated failure must surface");
    }

    let staged = staged_copies(&recovery_dir);
    assert_eq!(staged.len(), 1, "staged: {staged:?}");
    let hash = fresh::services::recovery::path_hash(&file_path);
    let meta = std::fs::read_to_string(recovery_dir.join(format!("{hash}.inplace.json"))).unwrap();
    let recovery: fresh::services::recovery::InplaceWriteRecovery =
        serde_json::from_str(&meta).unwrap();
    assert_eq!(recovery.temp_path, staged[0]);
    assert_eq!(
        std::fs::read_to_string(&staged[0]).unwrap(),
        "210original line\n",
        "the copy kept is the latest attempt's"
    );
}

/// What a crash in the middle of an in-place write leaves in the top-level
/// recovery directory is cleaned up when a session starts — where nothing
/// is lost by it: a staged copy its destination already matches, metadata
/// whose staged copy is gone, and a half-written metadata temp file. A
/// staged copy that differs from its destination may be the only copy of
/// what was being saved, and is kept.
#[test]
#[cfg(unix)]
fn test_session_start_cleans_up_resolved_inplace_recoveries() {
    use fresh::services::recovery::{path_hash, InplaceWriteRecovery};
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());
    let recovery_dir = data_dir.path().join("recovery");
    std::fs::create_dir_all(&recovery_dir).unwrap();
    let files = TempDir::new().unwrap();
    // No process has this pid (it's above any pid_max), so it "crashed".
    const DEAD_PID: u32 = 2_000_000_000;

    let plant = |name: &str, on_disk: &str, staged: Option<&str>| -> (PathBuf, PathBuf) {
        let dest = files.path().join(name);
        std::fs::write(&dest, on_disk).unwrap();
        let temp = recovery_dir.join(format!(".inplace-{name}-{DEAD_PID}-1.tmp"));
        if let Some(staged) = staged {
            std::fs::write(&temp, staged).unwrap();
        }
        let mut recovery = InplaceWriteRecovery::new(dest.clone(), temp.clone(), 0, 0, 0o644);
        recovery.pid = DEAD_PID;
        let meta = recovery_dir.join(format!("{}.inplace.json", path_hash(&dest)));
        std::fs::write(&meta, serde_json::to_string(&recovery).unwrap()).unwrap();
        (temp, meta)
    };
    let (done_temp, done_meta) = plant("done.txt", "new\n", Some("new\n"));
    let (torn_temp, torn_meta) = plant("torn.txt", "ne", Some("new\n"));
    let (_, gone_meta) = plant("gone.txt", "x\n", None);
    let meta_temp = recovery_dir.join(format!(".0123456789abcdef.inplace.json.{DEAD_PID}.3.tmp"));
    std::fs::write(&meta_temp, "{").unwrap();
    let live_meta_temp = recovery_dir.join(format!(
        ".0123456789abcdef.inplace.json.{}.4.tmp",
        std::process::id()
    ));
    std::fs::write(&live_meta_temp, "{").unwrap();

    let removed = fresh::model::buffer::save::clean_up_inplace_write_recoveries();

    for gone in [&done_temp, &done_meta, &gone_meta, &meta_temp] {
        assert!(!gone.exists(), "{gone:?} should have been removed");
    }
    for kept in [&torn_temp, &torn_meta, &live_meta_temp] {
        assert!(kept.exists(), "{kept:?} must be kept");
    }
    assert_eq!(removed, 4);
}

/// A write that fails part-way leaves the file torn and its complete new
/// content in a staged copy — the only complete copy on disk. A later
/// attempt that can't even open the file (read-only remount, permission
/// revoked) truncated nothing, yet it used to delete that copy as soon as
/// it staged its own, then discard its own because the open failed (or
/// hand it to a sudo prompt that deletes it when cancelled), leaving the
/// torn file with no copy of anything anywhere.
///
/// A small file's first attempt fails for real. A large file's recipe
/// reads the unchanged parts back from the file itself, which a torn write
/// has truncated, so its earlier copy is planted as one a crashed session
/// left behind.
fn check_refused_retry_keeps_earlier_copy(large: bool, refusal: io::ErrorKind) {
    use fresh::services::recovery::{path_hash, InplaceWriteRecovery};
    let data_dir = TempDir::new().unwrap();
    let _pin = crate::common::global_state::pin_data_dir(data_dir.path());
    let recovery_dir = data_dir.path().join("recovery");
    let meta_path = |file: &Path| recovery_dir.join(format!("{}.inplace.json", path_hash(file)));

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("torn.txt");
    let original: String = if large {
        (0..500)
            .map(|i| format!("Line {i:04}: original\n"))
            .collect()
    } else {
        "original line\n".into()
    };
    std::fs::write(&file_path, &original).unwrap();
    let fs = Arc::new(CrashDuringStreamFileSystem::new(
        Arc::new(StdFileSystem),
        file_path.clone(),
    ));
    let threshold = if large { 1024 } else { 1024 * 1024 };
    let mut buffer = TextBuffer::load_from_file(&file_path, threshold, fs.clone()).unwrap();
    assert_eq!(buffer.is_large_file(), large);

    let first = format!("first {original}");
    if large {
        // No process has this pid (it's above any pid_max), so it "crashed".
        const DEAD_PID: u32 = 2_000_000_000;
        std::fs::create_dir_all(&recovery_dir).unwrap();
        let copy = recovery_dir.join(format!(".inplace-torn.txt-{DEAD_PID}-1.tmp"));
        std::fs::write(&copy, &first).unwrap();
        let mut recovery = InplaceWriteRecovery::new(file_path.clone(), copy, 0, 0, 0o644);
        recovery.pid = DEAD_PID;
        std::fs::write(
            meta_path(&file_path),
            serde_json::to_string(&recovery).unwrap(),
        )
        .unwrap();
    } else {
        // Attempt 1: the write fails part-way.
        buffer.insert_bytes(0, b"first ".to_vec());
        assert!(buffer.save().is_err(), "the simulated failure must surface");
    }

    // Attempt 2: the file can't be opened for writing at all.
    fs.refuse_open(refusal);
    buffer.insert_bytes(0, b"second ".to_vec());
    let err = buffer.save().expect_err("the refusal must surface");
    // A sudo prompt the user cancels deletes the copy handed to it.
    if let Some(sudo) = err.downcast_ref::<fresh::model::buffer::SudoSaveRequired>() {
        std::fs::remove_file(&sudo.temp_path).unwrap();
    }

    let meta = std::fs::read_to_string(meta_path(&file_path))
        .expect("the torn file's recovery metadata must be kept");
    let recovery: InplaceWriteRecovery = serde_json::from_str(&meta).unwrap();
    let kept = std::fs::read_to_string(&recovery.temp_path).ok();
    assert!(
        kept.as_deref() == Some(first.as_str()),
        "the metadata must still point at the earlier complete copy, not {:?}",
        recovery.temp_path
    );
    assert_eq!(
        staged_copies(&recovery_dir),
        vec![recovery.temp_path],
        "nothing else is left staged"
    );
}

#[test]
#[cfg(unix)]
fn test_refused_retry_keeps_earlier_copy_small_file() {
    check_refused_retry_keeps_earlier_copy(false, io::ErrorKind::ReadOnlyFilesystem);
}

#[test]
#[cfg(unix)]
fn test_refused_retry_keeps_earlier_copy_small_file_sudo() {
    check_refused_retry_keeps_earlier_copy(false, io::ErrorKind::PermissionDenied);
}

#[test]
#[cfg(unix)]
fn test_refused_retry_keeps_earlier_copy_large_file() {
    check_refused_retry_keeps_earlier_copy(true, io::ErrorKind::ReadOnlyFilesystem);
}

#[test]
#[cfg(unix)]
fn test_refused_retry_keeps_earlier_copy_large_file_sudo() {
    check_refused_retry_keeps_earlier_copy(true, io::ErrorKind::PermissionDenied);
}
