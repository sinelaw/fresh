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

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch, StdFileSystem,
};
use portable_pty::{native_pty_system, PtySize};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use fresh_editor_core::model::host_path::{AsHostPath, HostPath, HostPathBuf};

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
    fn read_file(&self, path: &HostPath) -> io::Result<Vec<u8>> {
        let path = path.as_path();
        self.record(path);
        self.inner.read_file(path.as_host())
    }
    fn read_range(&self, path: &HostPath, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        let path = path.as_path();
        self.record(path);
        self.inner.read_range(path.as_host(), offset, len)
    }
    fn write_file(&self, path: &HostPath, data: &[u8]) -> io::Result<()> {
        let path = path.as_path();
        self.record(path);
        self.inner.write_file(path.as_host(), data)
    }
    fn create_file(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.record(path);
        self.inner.create_file(path.as_host())
    }
    fn create_new_file(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.record(path);
        self.inner.create_new_file(path.as_host())
    }
    fn create_new_private_file(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.record(path);
        self.inner.create_new_private_file(path.as_host())
    }
    fn open_file(&self, path: &HostPath) -> io::Result<Box<dyn FileReader>> {
        let path = path.as_path();
        self.record(path);
        self.inner.open_file(path.as_host())
    }
    fn open_file_for_write(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.record(path);
        self.inner.open_file_for_write(path.as_host())
    }
    fn open_file_for_append(&self, path: &HostPath) -> io::Result<Box<dyn FileWriter>> {
        let path = path.as_path();
        self.record(path);
        self.inner.open_file_for_append(path.as_host())
    }
    fn set_file_length(&self, path: &HostPath, len: u64) -> io::Result<()> {
        let path = path.as_path();
        self.record(path);
        self.inner.set_file_length(path.as_host(), len)
    }
    fn rename(&self, from: &HostPath, to: &HostPath) -> io::Result<()> {
        let from = from.as_path();
        let to = to.as_path();
        self.record(from);
        self.record(to);
        self.inner.rename(from.as_host(), to.as_host())
    }
    fn copy(&self, from: &HostPath, to: &HostPath) -> io::Result<u64> {
        let from = from.as_path();
        let to = to.as_path();
        self.record(from);
        self.record(to);
        self.inner.copy(from.as_host(), to.as_host())
    }
    fn remove_file(&self, path: &HostPath) -> io::Result<()> {
        let path = path.as_path();
        self.record(path);
        self.inner.remove_file(path.as_host())
    }
    fn remove_dir(&self, path: &HostPath) -> io::Result<()> {
        let path = path.as_path();
        self.record(path);
        self.inner.remove_dir(path.as_host())
    }
    fn metadata(&self, path: &HostPath) -> io::Result<FileMetadata> {
        let path = path.as_path();
        self.record(path);
        self.inner.metadata(path.as_host())
    }
    fn symlink_metadata(&self, path: &HostPath) -> io::Result<FileMetadata> {
        let path = path.as_path();
        self.record(path);
        self.inner.symlink_metadata(path.as_host())
    }
    fn is_dir(&self, path: &HostPath) -> io::Result<bool> {
        let path = path.as_path();
        self.record(path);
        self.inner.is_dir(path.as_host())
    }
    fn is_file(&self, path: &HostPath) -> io::Result<bool> {
        let path = path.as_path();
        self.record(path);
        self.inner.is_file(path.as_host())
    }
    fn set_permissions(&self, path: &HostPath, permissions: &FilePermissions) -> io::Result<()> {
        let path = path.as_path();
        self.record(path);
        self.inner.set_permissions(path.as_host(), permissions)
    }
    fn read_dir(&self, path: &HostPath) -> io::Result<Vec<DirEntry>> {
        let path = path.as_path();
        self.record(path);
        self.inner.read_dir(path.as_host())
    }
    fn create_dir(&self, path: &HostPath) -> io::Result<()> {
        let path = path.as_path();
        self.record(path);
        self.inner.create_dir(path.as_host())
    }
    fn create_dir_all(&self, path: &HostPath) -> io::Result<()> {
        let path = path.as_path();
        self.record(path);
        self.inner.create_dir_all(path.as_host())
    }
    fn canonicalize(&self, path: &HostPath) -> io::Result<HostPathBuf> {
        let path = path.as_path();
        self.record(path);
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
        self.record(path);
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
        self.record(path);
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
