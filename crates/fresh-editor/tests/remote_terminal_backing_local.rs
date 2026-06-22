//! Regression test for issue #2424: in a remote SSH workspace, the integrated
//! terminal's scrollback *backing file* must be managed on the **local**
//! filesystem, never through the (remote) session authority filesystem.
//!
//! The integrated terminal's PTY always runs on the local host (an SSH
//! terminal spawns `ssh` as a *local* child), and the PTY read loop renders
//! scrollback into a backing file on local disk. Before the fix, the editor
//! routed the backing file's create / exists / append / truncate / read
//! through `authority().filesystem` — which in remote mode is the SSH
//! filesystem. That made every scrollback-mode toggle do a blocking SSH
//! round-trip against a path that only exists locally: the UI hung and the
//! truncate failed with "Failed to truncate terminal backing file", leaving
//! scrollback empty.
//!
//! This test injects a filesystem that (a) reports itself as a *remote*
//! connection and (b) records every path it is asked to touch. It then drives
//! the exact user flow — open a terminal, produce scrollback, toggle into
//! scrollback mode and back (Ctrl+Space) — and asserts that the remote
//! filesystem was **never** asked to operate on the terminal directory. With
//! the bug present this fails (the remote fs sees `…/terminals/…` paths);
//! with the fix it passes (those ops go to the local filesystem).
//!
//! Skips (rather than fails) when a PTY can't be opened in the environment.

mod common;

use common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchCursor, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch, StdFileSystem,
};
use portable_pty::{native_pty_system, PtySize};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

/// A filesystem that delegates to the local `StdFileSystem` but pretends to be
/// a remote SSH connection and records every path it is asked about. We use
/// the recording to prove the terminal backing file is *not* managed through
/// this (remote) authority filesystem.
struct RecordingRemoteFs {
    inner: StdFileSystem,
    seen: Mutex<Vec<PathBuf>>,
}

impl RecordingRemoteFs {
    fn new() -> Self {
        Self {
            inner: StdFileSystem,
            seen: Mutex::new(Vec::new()),
        }
    }

    fn record(&self, path: &Path) {
        self.seen.lock().unwrap().push(path.to_path_buf());
    }

    /// Paths recorded so far whose components include a `terminals` segment —
    /// i.e. anything under the terminal backing-file directory.
    fn terminal_paths(&self) -> Vec<PathBuf> {
        self.seen
            .lock()
            .unwrap()
            .iter()
            .filter(|p| {
                p.components()
                    .any(|c| c.as_os_str().to_string_lossy() == "terminals")
            })
            .cloned()
            .collect()
    }
}

impl FileSystem for RecordingRemoteFs {
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.record(path);
        self.inner.read_file(path)
    }
    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.record(path);
        self.inner.read_range(path, offset, len)
    }
    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.record(path);
        self.inner.write_file(path, data)
    }
    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.record(path);
        self.inner.create_file(path)
    }
    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        self.record(path);
        self.inner.open_file(path)
    }
    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.record(path);
        self.inner.open_file_for_write(path)
    }
    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.record(path);
        self.inner.open_file_for_append(path)
    }
    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
        self.record(path);
        self.inner.set_file_length(path, len)
    }
    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.record(from);
        self.record(to);
        self.inner.rename(from, to)
    }
    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        self.record(from);
        self.record(to);
        self.inner.copy(from, to)
    }
    fn remove_file(&self, path: &Path) -> io::Result<()> {
        self.record(path);
        self.inner.remove_file(path)
    }
    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        self.record(path);
        self.inner.remove_dir(path)
    }
    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.record(path);
        self.inner.metadata(path)
    }
    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.record(path);
        self.inner.symlink_metadata(path)
    }
    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.record(path);
        self.inner.is_dir(path)
    }
    fn is_file(&self, path: &Path) -> io::Result<bool> {
        self.record(path);
        self.inner.is_file(path)
    }
    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
        self.record(path);
        self.inner.set_permissions(path, permissions)
    }
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        self.record(path);
        self.inner.read_dir(path)
    }
    fn create_dir(&self, path: &Path) -> io::Result<()> {
        self.record(path);
        self.inner.create_dir(path)
    }
    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        self.record(path);
        self.inner.create_dir_all(path)
    }
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.record(path);
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
        self.record(path);
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
        self.record(path);
        self.inner.sudo_write(path, data, mode, uid, gid)
    }
    fn walk_files(
        &self,
        root: &Path,
        skip_dirs: &[&str],
        cancel: &AtomicBool,
        on_file: &mut dyn FnMut(&Path, &str) -> bool,
    ) -> io::Result<()> {
        self.record(root);
        self.inner.walk_files(root, skip_dirs, cancel, on_file)
    }
    // Present as a live remote connection so the editor treats this authority
    // as a remote (SSH) backend, exactly as in the bug report.
    fn remote_connection_info(&self) -> Option<&str> {
        Some("test@remote")
    }
    fn is_remote_connected(&self) -> bool {
        true
    }
}

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

#[test]
#[cfg_attr(target_os = "windows", ignore)] // uses a Unix shell to produce scrollback
fn terminal_backing_file_stays_local_in_remote_mode() {
    if !pty_available() {
        eprintln!("Skipping: PTY not available in this environment");
        return;
    }

    let fs = Arc::new(RecordingRemoteFs::new());
    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new().with_filesystem(fs.clone() as Arc<dyn FileSystem + Send + Sync>),
    )
    .expect("create harness with remote-marker filesystem");

    // Open the integrated terminal and produce more output than fits on screen
    // so scrollback is genuinely streamed to the backing file.
    harness.editor_mut().open_terminal();
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"for i in $(seq 1 100); do echo \"Line $i\"; done\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("Line 100"))
        .expect("terminal should print up to Line 100");

    // Toggle into scrollback mode (Ctrl+Space) and back — the round-trip that
    // truncates the backing file on re-entry (the reported failure point).
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        !harness.editor().is_terminal_mode(),
        "Ctrl+Space should leave terminal mode for scrollback"
    );
    harness.render().unwrap();

    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        harness.editor().is_terminal_mode(),
        "Ctrl+Space should re-enter terminal mode"
    );
    harness.render().unwrap();

    // The terminal backing file is a local artifact: not one of its
    // create/exists/append/truncate/read operations may have been routed
    // through the remote authority filesystem.
    let leaked = fs.terminal_paths();
    assert!(
        leaked.is_empty(),
        "terminal backing-file I/O leaked onto the remote authority filesystem \
         (issue #2424): {leaked:?}"
    );
}
