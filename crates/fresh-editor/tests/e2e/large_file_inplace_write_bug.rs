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
