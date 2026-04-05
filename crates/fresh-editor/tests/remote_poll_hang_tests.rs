//! Tests for poll_file_changes blocking the main event loop
//!
//! These tests verify that the editor's file change polling does not block
//! the main event loop when filesystem operations are slow or hanging.
//! This reproduces the bug where a remote filesystem going down causes
//! the editor to deadlock because poll_file_changes calls metadata()
//! synchronously from the event loop.
//!
//! Current status: These tests FAIL (hang forever) because poll_file_changes
//! calls filesystem.metadata() synchronously with no timeout or async handling.
//! They will pass once polling is made non-blocking for slow/remote filesystems.

mod common;

use common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchCursor, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch, StdFileSystem,
};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

/// A filesystem wrapper that delegates to StdFileSystem but can simulate
/// a connection drop: after `set_hanging(true)`, metadata() blocks forever.
/// Other operations (read, write) always delegate immediately so that
/// file open/save works normally before the "drop" is triggered.
struct DroppableFileSystem {
    inner: StdFileSystem,
    /// When true, metadata() blocks forever (simulates hanging remote)
    hanging: AtomicBool,
    /// When true, all I/O operations return errors immediately (simulates disconnected remote)
    disconnected: AtomicBool,
}

impl DroppableFileSystem {
    fn new() -> Self {
        Self {
            inner: StdFileSystem,
            hanging: AtomicBool::new(false),
            disconnected: AtomicBool::new(false),
        }
    }

    fn set_hanging(&self, val: bool) {
        self.hanging.store(val, Ordering::SeqCst);
    }

    fn set_disconnected(&self, val: bool) {
        self.disconnected.store(val, Ordering::SeqCst);
    }

    /// Block forever if hanging is true (simulates dead remote connection)
    fn block_if_hanging(&self) {
        if self.hanging.load(Ordering::SeqCst) {
            loop {
                std::thread::park();
            }
        }
    }

    /// Return a "connection closed" error if disconnected
    fn check_disconnected(&self) -> io::Result<()> {
        if self.disconnected.load(Ordering::SeqCst) {
            Err(io::Error::new(
                io::ErrorKind::ConnectionReset,
                "Channel closed",
            ))
        } else {
            Ok(())
        }
    }
}

// Delegate all FileSystem methods to StdFileSystem, but intercept metadata()
// and fail on write/read when disconnected.
impl FileSystem for DroppableFileSystem {
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.check_disconnected()?;
        self.inner.read_file(path)
    }
    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.check_disconnected()?;
        self.inner.read_range(path, offset, len)
    }
    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.check_disconnected()?;
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
        self.block_if_hanging();
        self.inner.metadata(path)
    }
    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.block_if_hanging();
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
    fn search_file(
        &self,
        path: &Path,
        pattern: &str,
        opts: &FileSearchOptions,
        cursor: &mut FileSearchCursor,
    ) -> io::Result<Vec<SearchMatch>> {
        self.inner.search_file(path, pattern, opts, cursor)
    }
    fn sudo_write(
        &self,
        path: &Path,
        data: &[u8],
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> io::Result<()> {
        self.check_disconnected()?;
        self.inner.sudo_write(path, data, mode, uid, gid)
    }
    fn remote_connection_info(&self) -> Option<&str> {
        // Pretend to be remote so disconnected checks work
        Some("test@localhost")
    }
    fn is_remote_connected(&self) -> bool {
        !self.disconnected.load(Ordering::SeqCst)
    }
}

/// Test: poll_file_changes must not block the event loop when metadata() hangs.
///
/// Scenario: A file is open in the editor. The filesystem's metadata() call
/// starts hanging (simulating a remote server that stopped responding).
/// When poll_file_changes fires, it should not block the event loop.
///
/// BUG: Currently hangs because poll_file_changes calls filesystem.metadata()
/// synchronously in a loop over all open buffers.
///
/// After the fix, process_async_messages() should return quickly because
/// metadata checks run on a background thread.
#[test]
fn test_poll_file_changes_does_not_hang_with_slow_metadata() {
    let temp_dir = tempfile::tempdir().unwrap();
    let file_path = temp_dir.path().join("test_file.txt");
    std::fs::write(&file_path, "hello").unwrap();

    let fs = Arc::new(DroppableFileSystem::new());

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(Config {
                editor: fresh::config::EditorConfig {
                    auto_revert_poll_interval_ms: 100,
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_filesystem(fs.clone())
            .with_working_dir(temp_dir.path().to_path_buf()),
    )
    .unwrap();

    // Open file while filesystem is healthy (metadata works normally)
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Simulate remote server going down
    fs.set_hanging(true);

    // Advance time past the poll interval so poll_file_changes will fire
    harness.advance_time(Duration::from_millis(200));

    // This call should return quickly, not block forever.
    // The metadata() call will be on a background thread that hangs,
    // but the main loop should not wait for it.
    let _ = harness.editor_mut().process_async_messages();

    // If we get here, the fix is working — polling didn't block.
}

/// Test: poll_file_tree_changes must not block the event loop when metadata() hangs.
///
/// Same issue as poll_file_changes but for directory mtime polling.
#[test]
fn test_poll_file_tree_changes_does_not_hang_with_slow_metadata() {
    let temp_dir = tempfile::tempdir().unwrap();
    let file_path = temp_dir.path().join("test_file.txt");
    std::fs::write(&file_path, "hello").unwrap();

    let fs = Arc::new(DroppableFileSystem::new());

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(Config {
                editor: fresh::config::EditorConfig {
                    auto_revert_poll_interval_ms: 100,
                    file_tree_poll_interval_ms: 100,
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_filesystem(fs.clone())
            .with_working_dir(temp_dir.path().to_path_buf()),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Simulate remote server going down
    fs.set_hanging(true);

    // Advance time past the poll interval
    harness.advance_time(Duration::from_millis(200));

    // This should return quickly, not block.
    let _ = harness.editor_mut().process_async_messages();
}

/// Test: saving a file when the remote server is disconnected must not crash
/// the editor. It should show an error in the status bar instead.
///
/// BUG: Currently the save error propagates through handle_key → run_event_loop
/// and crashes the editor with "Channel closed".
#[test]
fn test_save_when_disconnected_does_not_crash() {
    let temp_dir = tempfile::tempdir().unwrap();
    let file_path = temp_dir.path().join("test_file.txt");
    std::fs::write(&file_path, "hello").unwrap();

    let fs = Arc::new(DroppableFileSystem::new());

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_filesystem(fs.clone())
            .with_working_dir(temp_dir.path().to_path_buf()),
    )
    .unwrap();

    // Open the file while filesystem is healthy
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Make an edit so the buffer is modified
    harness.type_text("X").unwrap();

    // Simulate remote server going down
    fs.set_disconnected(true);

    // Try to save (Ctrl+S). This must NOT crash — it should show an error
    // in the status bar instead of propagating through handle_key.
    //
    // BUG: Without the fix, this panics with "Channel closed" because the
    // save error propagates via ? through handle_action → handle_key →
    // the test harness → unwrap().
    let save_result = harness.send_key(
        crossterm::event::KeyCode::Char('s'),
        crossterm::event::KeyModifiers::CONTROL,
    );
    assert!(
        save_result.is_ok(),
        "Ctrl+S should not crash when disconnected, but got: {:?}",
        save_result.err()
    );

    // Render should also succeed — the editor is still alive
    harness.render().unwrap();

    // Verify the disconnected indicator is visible on screen
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Disconnected"),
        "Status bar should show disconnected state"
    );
}
