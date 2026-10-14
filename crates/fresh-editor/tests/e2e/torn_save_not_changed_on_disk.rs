//! A save that fails part-way through writing its file in place changes the
//! file, but that change is the editor's own, not someone else's.
//!
//! The file's mtime is recorded only after a save that succeeds, so a torn
//! write (the disk filling up mid-write, say) left the file with an mtime
//! the editor hadn't recorded, and every changed-on-disk check took it for
//! an outside change: the file-change poll replaced the save's error in the
//! status bar with "changed on disk", the next Ctrl+S asked about
//! overwriting "someone else's" change, and the quit prompt listed the file
//! as changed on disk.
//!
//! The filesystem double here writes in place (it owns no file) through to
//! the real filesystem, so the mtime really moves, and fails a write with
//! "no space left" once a given number of bytes are written.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSystem, FileWriter, StdFileSystem,
};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime};
use tempfile::TempDir;

const ORIGINAL: &str = "orig1\norig2\n";

/// A local filesystem on which we own no file, so saves write in place, and
/// writes to one file can be made to run out of space part-way.
struct DiskFullFileSystem {
    inner: StdFileSystem,
    /// Writes to this file fail once this many bytes have been written.
    tear: Mutex<Option<(PathBuf, usize)>>,
}

impl DiskFullFileSystem {
    /// Arm the fault for `file`, by its resolved path: the editor opens
    /// files by theirs, which differs where the temp dir is behind a
    /// symlink (macOS's /var -> /private/var).
    fn tear_after(&self, file: &Path, bytes: usize) {
        let file = file.canonicalize().unwrap_or_else(|_| file.to_path_buf());
        *self.tear.lock().unwrap() = Some((file, bytes));
    }

    fn free_space(&self) {
        *self.tear.lock().unwrap() = None;
    }
}

/// Writes through to `inner` until `left` bytes are used up, then fails as
/// a full disk does.
struct DiskFullWriter {
    inner: Box<dyn FileWriter>,
    left: usize,
}

impl io::Write for DiskFullWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.left == 0 {
            return Err(io::Error::new(
                io::ErrorKind::StorageFull,
                "No space left on device",
            ));
        }
        let n = self.inner.write(&buf[..buf.len().min(self.left)])?;
        self.left -= n;
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

impl FileWriter for DiskFullWriter {
    fn sync_all(&self) -> io::Result<()> {
        self.inner.sync_all()
    }
}

impl FileSystem for DiskFullFileSystem {
    fn is_owner(&self, _path: &Path) -> bool {
        false
    }
    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        let inner = self.inner.open_file_for_write(path)?;
        match &*self.tear.lock().unwrap() {
            Some((file, left))
                if *file == path.canonicalize().unwrap_or_else(|_| path.to_path_buf()) =>
            {
                Ok(Box::new(DiskFullWriter { inner, left: *left }))
            }
            _ => Ok(inner),
        }
    }

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
    fn create_new_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_new_file(path)
    }
    fn create_new_private_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_new_private_file(path)
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
        fresh::model::filesystem::default_search_file(&self.inner, path, pattern, opts, cursor)
    }
    fn walk(
        &self,
        root: &Path,
        opts: &fresh::model::filesystem::WalkOptions<'_>,
        cancel: &std::sync::atomic::AtomicBool,
        on_entry: &mut dyn FnMut(fresh::model::filesystem::WalkEntry<'_>) -> bool,
    ) -> io::Result<()> {
        self.inner.walk(root, opts, cancel, on_entry)
    }
}

fn set_mtime(path: &Path, mtime: SystemTime) {
    std::fs::File::options()
        .write(true)
        .open(path)
        .unwrap()
        .set_times(std::fs::FileTimes::new().set_modified(mtime))
        .unwrap();
}

fn run_command(harness: &mut EditorTestHarness, name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(name).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(name);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

fn save(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
}

/// Run the file-change poll over every open file: `other.txt`, clean and in
/// a split of its own, reloads when it has.
fn wait_for_poll(harness: &mut EditorTestHarness, other: &Path, content: &str) {
    std::fs::write(other, format!("{content}\n")).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(content))
        .unwrap();
}

/// `notes.txt`, edited, whose save with Ctrl+S ran out of space part-way
/// through writing it in place; and a clean `other.txt` beside it (see
/// [`wait_for_poll`]).
fn torn_save() -> (
    EditorTestHarness,
    TempDir,
    PathBuf,
    PathBuf,
    Arc<DiskFullFileSystem>,
) {
    let dir = TempDir::new().unwrap();
    let file = dir.path().join("notes.txt");
    let other = dir.path().join("other.txt");
    std::fs::write(&file, ORIGINAL).unwrap();
    std::fs::write(&other, "other\n").unwrap();
    // Well in the past, so the torn write moves it whatever the
    // filesystem's timestamp granularity.
    set_mtime(&file, SystemTime::now() - Duration::from_secs(600));
    let fs = Arc::new(DiskFullFileSystem {
        inner: StdFileSystem,
        tear: Mutex::new(None),
    });
    let mut harness = EditorTestHarness::create(
        160,
        30,
        HarnessOptions::new()
            .with_config(Config::default())
            .with_filesystem(fs.clone())
            .with_working_dir(dir.path().to_path_buf()),
    )
    .unwrap();
    // Wide enough for the poll's "File <full path> changed on disk (...)"
    // whole, however long the temp dir's path (macOS, Windows).
    let path_len = file.canonicalize().unwrap().display().to_string().len() as u16;
    harness.resize(200 + path_len, 30).unwrap();
    harness.open_file(&other).unwrap();
    run_command(&mut harness, "Split Vertical");
    harness.open_file(&file).unwrap();
    harness.type_text("EDIT ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("EDIT orig1");

    fs.tear_after(&file, 3);
    save(&mut harness);
    let status = harness.get_status_bar();
    assert!(
        status.contains("Failed to save") && status.contains("No space left on device"),
        "the save must fail; status was {status:?}"
    );
    assert_ne!(
        std::fs::read_to_string(&file).unwrap(),
        ORIGINAL,
        "the failed save must have written part of the file"
    );
    (harness, dir, file, other, fs)
}

/// The torn write is the editor's own: the poll leaves the save's error in
/// the status bar, the quit prompt calls the buffer unsaved, and Ctrl+S
/// saves again without asking about a change on disk.
#[test]
#[cfg(unix)]
fn a_torn_save_is_not_taken_for_a_change_on_disk() {
    let (mut harness, _dir, file, other, fs) = torn_save();

    wait_for_poll(&mut harness, &other, "reloaded");
    let status = harness.get_status_bar();
    assert!(
        status.contains("Failed to save") && !status.contains("changed on disk"),
        "the poll must leave the save's error alone; status was {status:?}"
    );

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("[ Save and Quit ]");
    harness.assert_screen_contains("unsaved changes");
    harness.assert_screen_not_contains("changed on disk");
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("[ Save and Quit ]");
    assert!(!harness.should_quit());

    fs.free_space();
    save(&mut harness);
    harness.assert_screen_not_contains("File Changed on Disk");
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        format!("EDIT {ORIGINAL}"),
        "the retried save must write the whole buffer"
    );
}

/// A change someone else makes after the torn write moves the mtime on,
/// and is still caught: by the poll, and by Ctrl+S, which asks first.
#[test]
#[cfg(unix)]
fn a_change_on_disk_after_a_torn_save_is_still_caught() {
    let (mut harness, _dir, file, _other, fs) = torn_save();
    fs.free_space();

    std::fs::write(&file, "external\n").unwrap();
    set_mtime(&file, SystemTime::now() + Duration::from_secs(600));
    harness
        .wait_until(|h| {
            h.get_status_bar()
                .contains("changed on disk (buffer has unsaved")
        })
        .unwrap();

    save(&mut harness);
    harness.assert_screen_contains("File Changed on Disk");
    assert_eq!(std::fs::read_to_string(&file).unwrap(), "external\n");
}
