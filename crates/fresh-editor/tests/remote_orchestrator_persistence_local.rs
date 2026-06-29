//! Regression test: in a remote SSH workspace, the orchestrator's session
//! persistence (the per-dir workspace registry under the local `data_dir`)
//! must be read through the **local** filesystem, never the (remote) session
//! authority filesystem.
//!
//! The orchestrator is the local thing that *spawns* remote-backed windows; it
//! is never itself remote-backed. Its registry lives on local disk. Before the
//! fix, startup read `<data_dir>/workspaces/*.json` through
//! `authority().filesystem` — which in remote mode is the SSH filesystem. On a
//! laggy link that turned boot into one blocking SSH round-trip per workspace
//! file (dozens of files → a ~90s "hang" before the UI painted), and it asked
//! the *remote* host for paths that only mean something locally.
//!
//! This test injects a filesystem that (a) reports itself as a *remote*
//! connection and (b) records every path it is asked to touch, plants a
//! workspace file under the local `data_dir`, then builds the editor and
//! asserts the remote filesystem was **never** asked to read the workspace
//! registry. With the bug present this fails (the remote fs sees
//! `…/workspaces/…` paths); with the fix it passes (those reads go local).

mod common;

use common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchCursor, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch, StdFileSystem,
};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

/// A filesystem that delegates to the local `StdFileSystem` but pretends to be
/// a remote SSH connection and records every path it is asked about. We use the
/// recording to prove the orchestrator registry is *not* read through this
/// (remote) authority filesystem.
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

    /// Recorded paths with a `workspaces` segment — i.e. anything under the
    /// orchestrator's session registry directory.
    fn workspace_paths(&self) -> Vec<PathBuf> {
        self.seen
            .lock()
            .unwrap()
            .iter()
            .filter(|p| {
                p.components()
                    .any(|c| c.as_os_str().to_string_lossy() == "workspaces")
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

#[test]
fn orchestrator_registry_read_stays_local_in_remote_mode() {
    fresh::i18n::set_locale("en");

    let sandbox = tempfile::tempdir().unwrap();
    let mk = |n: &str| {
        let p = sandbox.path().join(n);
        std::fs::create_dir_all(&p).unwrap();
        p.canonicalize().unwrap()
    };
    let data_home = mk("data-home");
    let project = mk("project");

    let dir_context = DirectoryContext::for_testing(&data_home);

    // Plant a workspace file in the local registry so boot does the per-file
    // read loop that hung over SSH — not just the directory listing.
    let workspaces = dir_context.data_dir.join("workspaces");
    std::fs::create_dir_all(&workspaces).unwrap();
    std::fs::write(
        workspaces.join("planted.json"),
        br#"{"working_dir":"/nonexistent/planted-session"}"#,
    )
    .unwrap();

    let fs = Arc::new(RecordingRemoteFs::new());
    let _harness = EditorTestHarness::create(
        100,
        40,
        HarnessOptions::new()
            .with_working_dir(project)
            .with_shared_dir_context(dir_context)
            .with_filesystem(fs.clone() as Arc<dyn FileSystem + Send + Sync>)
            .with_empty_plugins_dir(),
    )
    .expect("create harness with remote-marker filesystem");

    // The orchestrator registry is a local artifact: none of its directory
    // listings or per-file reads may have been routed through the remote
    // authority filesystem.
    let leaked = fs.workspace_paths();
    assert!(
        leaked.is_empty(),
        "orchestrator session-registry I/O leaked onto the remote authority \
         filesystem: {leaked:?}"
    );
}
