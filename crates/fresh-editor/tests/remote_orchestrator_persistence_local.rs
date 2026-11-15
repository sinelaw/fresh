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

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch, StdFileSystem,
};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use fresh_editor_core::model::host_path::{AsHostPath, HostPath, HostPathBuf};

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
