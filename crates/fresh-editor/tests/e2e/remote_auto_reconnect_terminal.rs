//! Component test for the *automatic* terminal-respawn path on reconnect.
//!
//! There are two ways a dropped remote link comes back:
//!
//!   * the app-level `RemoteAttachMode::Reconnect` flow (dive into a dormant
//!     session / click "Reconnect"), which re-points the authority and calls
//!     `respawn_terminals_through_authority` directly — covered by
//!     `remote_reconnect_terminal.rs`; and
//!   * the *silent* background transport hot-swap (`spawn_reconnect_task`),
//!     which restores the agent channel underneath the existing authority
//!     without ever routing through that flow.
//!
//! The second path is what `fresh ssh://…` actually uses, and it used to leave
//! embedded terminals dead even after the filesystem/LSP came back — nothing
//! respawned them. `Editor::detect_remote_terminal_reconnects` closes that gap:
//! it samples each remote window's connection state every tick (via
//! `process_async_messages`) and, on a `disconnected → connected` edge, revives
//! the dead PTYs in place.
//!
//! This drives that detector directly with a filesystem whose connected state
//! is a runtime-flippable flag, asserting (a) no respawn while down, (b) respawn
//! on the recover edge, and (c) no double-respawn on a subsequent steady tick.
//! Requires a working PTY (`/dev/ptmx`); skips when unavailable.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchCursor, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch, StdFileSystem,
};
use portable_pty::{native_pty_system, PtySize};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

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

/// A filesystem that advertises a remote backend (so `remote_connection_info()`
/// is `Some`) whose link state is a shared, flippable flag. Real I/O delegates
/// to `StdFileSystem`; only the connection-state answers are synthetic, so the
/// detector can be driven across a disconnect/reconnect with no network.
struct ToggleRemoteFs {
    inner: StdFileSystem,
    connected: Arc<AtomicBool>,
}

impl FileSystem for ToggleRemoteFs {
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
        self.inner.sudo_write(path, data, mode, uid, gid)
    }
    fn walk_files(
        &self,
        root: &Path,
        skip_dirs: &[&str],
        cancel: &AtomicBool,
        on_file: &mut dyn FnMut(&Path, &str) -> bool,
    ) -> io::Result<()> {
        self.inner.walk_files(root, skip_dirs, cancel, on_file)
    }
    fn remote_connection_info(&self) -> Option<&str> {
        Some("root@127.0.0.1")
    }
    fn is_remote_connected(&self) -> bool {
        self.connected.load(Ordering::SeqCst)
    }
}

/// A silent agent-channel reconnect (background transport hot-swap) must revive
/// a terminal that died with the dropped carrier — the bug from issue #2482,
/// where `fresh ssh://…` terminals stayed dead after the link came back.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Unix PTY shell
fn auto_reconnect_respawns_a_dead_remote_terminal() {
    if !pty_available() {
        eprintln!("Skipping auto-reconnect terminal test: PTY not available");
        return;
    }

    let temp = tempfile::tempdir().unwrap();
    let connected = Arc::new(AtomicBool::new(true));
    let fs: Arc<dyn FileSystem + Send + Sync> = Arc::new(ToggleRemoteFs {
        inner: StdFileSystem,
        connected: connected.clone(),
    });
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_working_dir(temp.path().to_path_buf())
            .with_filesystem(fs),
    )
    .unwrap();

    // A live embedded terminal bound to its buffer — the pre-disconnect state.
    let (old_id, buffer_id) = harness
        .editor_mut()
        .active_window_mut()
        .open_terminal_in_window()
        .expect("terminal should spawn");

    // Carrier drop: the link goes down and the PTY dies. We tear down the
    // handle (what the `TerminalExited` handler does) but keep the
    // buffer↔terminal binding (the remote-disconnect preserve path), so the
    // reconnect has something to revive.
    connected.store(false, Ordering::SeqCst);
    harness
        .editor_mut()
        .active_window_mut()
        .terminal_manager
        .close(old_id);

    // A tick while still disconnected records the down edge and must NOT
    // respawn — the link is dead, a new `ssh -t` would just fail.
    harness.editor_mut().process_async_messages();
    assert_eq!(
        harness
            .editor()
            .active_window()
            .terminal_buffers
            .get(&buffer_id),
        Some(&old_id),
        "binding is preserved across the drop, still awaiting respawn"
    );
    assert!(
        harness
            .editor()
            .active_window()
            .terminal_manager
            .get(old_id)
            .is_none(),
        "no respawn happens while the link is still down"
    );

    // The link comes back via the silent hot-swap. The next tick's detector
    // should see the disconnected → connected edge and revive the terminal.
    connected.store(true, Ordering::SeqCst);
    harness.editor_mut().process_async_messages();

    let new_id = *harness
        .editor()
        .active_window()
        .terminal_buffers
        .get(&buffer_id)
        .expect("buffer is still bound to a terminal after auto-reconnect");
    assert_ne!(
        new_id, old_id,
        "auto-reconnect respawns into a fresh terminal id, not the dead one"
    );
    assert!(
        harness
            .editor()
            .active_window()
            .terminal_manager
            .get(new_id)
            .is_some_and(|h| h.is_alive()),
        "the auto-respawned terminal is live"
    );

    // A subsequent steady-state tick (still connected) must not respawn again —
    // the edge detector only fires on the transition, not on every connected
    // tick. Guards against a flip-flop that would orphan PTYs every frame.
    harness.editor_mut().process_async_messages();
    assert_eq!(
        harness
            .editor()
            .active_window()
            .terminal_buffers
            .get(&buffer_id),
        Some(&new_id),
        "no second respawn on a steady connected tick"
    );
}
