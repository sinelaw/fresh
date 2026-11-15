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

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch, StdFileSystem,
};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
use fresh_editor_core::model::host_path::{AsHostPath, HostPath, HostPathBuf};

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
    fn read_file(&self, path: &HostPath) -> io::Result<Vec<u8>> {
        let path = path.as_path();
        self.check_disconnected()?;
        self.inner.read_file(path.as_host())
    }
    fn read_range(&self, path: &HostPath, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        let path = path.as_path();
        self.check_disconnected()?;
        self.inner.read_range(path.as_host(), offset, len)
    }
    fn write_file(&self, path: &HostPath, data: &[u8]) -> io::Result<()> {
        let path = path.as_path();
        self.check_disconnected()?;
        self.inner.write_file(path.as_host(), data)
    }
    fn create_file(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.inner.create_file(path.as_host())
    }
    fn create_new_file(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.inner.create_new_file(path.as_host())
    }
    fn create_new_private_file(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.inner.create_new_private_file(path.as_host())
    }
    fn open_file(&self, path: &HostPath) -> io::Result<Box<dyn FileReader>> {
        let path = path.as_path();
        self.inner.open_file(path.as_host())
    }
    fn open_file_for_write(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.inner.open_file_for_write(path.as_host())
    }
    fn open_file_for_append(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.inner.open_file_for_append(path.as_host())
    }
    fn set_file_length(&self, path: &HostPath, len: u64) -> io::Result<()> {
        let path = path.as_path();
        self.inner.set_file_length(path.as_host(), len)
    }
    fn rename(&self, from: &HostPath, to: &HostPath) -> io::Result<()> {
        let from = from.as_path();
        let to = to.as_path();
        self.inner.rename(from.as_host(), to.as_host())
    }
    fn copy(&self, from: &HostPath, to: &HostPath) -> io::Result<u64> {
        let from = from.as_path();
        let to = to.as_path();
        self.inner.copy(from.as_host(), to.as_host())
    }
    fn remove_file(&self, path: &HostPath) -> io::Result<()> {
        let path = path.as_path();
        self.inner.remove_file(path.as_host())
    }
    fn remove_dir(&self, path: &HostPath) -> io::Result<()> {
        let path = path.as_path();
        self.inner.remove_dir(path.as_host())
    }
    fn metadata(&self, path: &HostPath) -> io::Result<FileMetadata> {
        let path = path.as_path();
        self.block_if_hanging();
        self.inner.metadata(path.as_host())
    }
    fn symlink_metadata(&self, path: &HostPath) -> io::Result<FileMetadata> {
        let path = path.as_path();
        self.block_if_hanging();
        self.inner.symlink_metadata(path.as_host())
    }
    fn is_dir(&self, path: &HostPath) -> io::Result<bool> {
        let path = path.as_path();
        self.inner.is_dir(path.as_host())
    }
    fn is_file(&self, path: &HostPath) -> io::Result<bool> {
        let path = path.as_path();
        self.inner.is_file(path.as_host())
    }
    fn set_permissions(&self, path: &HostPath, permissions: &FilePermissions) -> io::Result<()> {
        let path = path.as_path();
        self.inner.set_permissions(path.as_host(), permissions)
    }
    fn read_dir(&self, path: &HostPath) -> io::Result<Vec<DirEntry>> {
        let path = path.as_path();
        self.inner.read_dir(path.as_host())
    }
    fn create_dir(&self, path: &HostPath) -> io::Result<()> {
        let path = path.as_path();
        self.inner.create_dir(path.as_host())
    }
    fn create_dir_all(&self, path: &HostPath) -> io::Result<()> {
        let path = path.as_path();
        self.inner.create_dir_all(path.as_host())
    }
    fn canonicalize(&self, path: &HostPath) -> io::Result<HostPathBuf> {
        let path = path.as_path();
        self.inner.canonicalize(path.as_host())
    }
    fn current_uid(&self) -> u32 {
        self.inner.current_uid()
    }
    fn search_file(
        &self,
        path: &HostPath,
        pattern: &str,
        opts: &FileSearchOptions,
    ) -> io::Result<Vec<SearchMatch>> {
        let path = path.as_path();
        self.inner.search_file(path.as_host(), pattern, opts)
    }
    fn sudo_write(
        &self,
        path: &HostPath,
        data: &[u8],
        mode: Option<u32>,
        owner: Option<(u32, u32)>,
    ) -> io::Result<()> {
        let path = path.as_path();
        self.check_disconnected()?;
        self.inner.sudo_write(path.as_host(), data, mode, owner)
    }
    fn walk(
        &self,
        root: &HostPath,
            opts: &fresh_editor_core::model::filesystem::WalkOptions<'_>,
            cancel: &std::sync::atomic::AtomicBool,
            on_entry: &mut dyn FnMut(fresh_editor_core::model::filesystem::WalkEntry<'_>) -> bool,
    ) -> std::io::Result<()> {
        let root = root.as_path();
        self.inner.walk(root.as_host(), opts, cancel, on_entry)
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
