//! A file whose mtime moves without its content changing is not "changed on
//! disk" (issue #3380).
//!
//! On NFS or SMB, the mtime read straight after our own save can differ from
//! the one read later: the client's cached attributes are revalidated
//! against the server's, and with clock skew between the two they disagree.
//! Nobody else touched the file, but since #3346 any mtime difference counts
//! as a change, so auto-save, Save All and Save and Quit refused to write it
//! ("Not saved, changed on disk"), and a clean buffer auto-reverted.
//!
//! The filesystem double here reproduces exactly that: once "revalidated",
//! it reports the file's mtime shifted by a few seconds, content untouched.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSystem, FileWriter, StdFileSystem,
};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tempfile::TempDir;

/// A local filesystem whose reported mtimes are shifted by `skew` once it
/// is set, as a network filesystem's are when a client's cached attributes
/// give way to a skewed server's.
struct SkewedClockFileSystem {
    inner: Arc<dyn FileSystem>,
    skew: Mutex<Duration>,
    /// How many times `notes.txt` was read whole. On a remote filesystem
    /// each is a download of the file.
    notes_reads: AtomicUsize,
}

impl SkewedClockFileSystem {
    fn skewed(&self, mut meta: FileMetadata) -> FileMetadata {
        let skew = *self.skew.lock().unwrap();
        meta.modified = meta.modified.map(|m| m + skew);
        meta
    }
}

impl FileSystem for SkewedClockFileSystem {
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        if path.file_name().is_some_and(|name| name == "notes.txt") {
            self.notes_reads.fetch_add(1, Ordering::SeqCst);
        }
        self.inner.read_file(path)
    }
    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.inner.read_range(path, offset, len)
    }
    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.inner.write_file(path, data)
    }
    fn create_new_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_new_file(path)
    }
    fn create_new_private_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_new_private_file(path)
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
        self.inner.metadata(path).map(|m| self.skewed(m))
    }
    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.symlink_metadata(path).map(|m| self.skewed(m))
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
        opts: &fresh::model::filesystem::WalkOptions<'_>,
        cancel: &std::sync::atomic::AtomicBool,
        on_entry: &mut dyn FnMut(fresh::model::filesystem::WalkEntry<'_>) -> bool,
    ) -> io::Result<()> {
        self.inner.walk(root, opts, cancel, on_entry)
    }
}

/// `notes.txt` opened, edited and saved with Ctrl+S; then the filesystem
/// starts reporting its mtime 5s off, and the buffer is edited again.
fn saved_then_skewed() -> (
    EditorTestHarness,
    TempDir,
    PathBuf,
    Arc<SkewedClockFileSystem>,
) {
    let (mut harness, dir, file, fs) = saved_then_skewed_clean();
    harness.type_text("second ").unwrap();
    harness.render().unwrap();
    (harness, dir, file, fs)
}

/// The same, left clean after the save.
fn saved_then_skewed_clean() -> (
    EditorTestHarness,
    TempDir,
    PathBuf,
    Arc<SkewedClockFileSystem>,
) {
    let dir = TempDir::new().unwrap();
    let file = dir.path().join("notes.txt");
    std::fs::write(&file, "original\n").unwrap();
    let fs = Arc::new(SkewedClockFileSystem {
        inner: Arc::new(StdFileSystem),
        skew: Mutex::new(Duration::ZERO),
        notes_reads: AtomicUsize::new(0),
    });
    let mut harness = EditorTestHarness::create(
        160,
        24,
        HarnessOptions::new()
            .with_config(Config::default())
            .with_filesystem(fs.clone())
            .with_working_dir(dir.path().to_path_buf()),
    )
    .unwrap();
    harness.open_file(&file).unwrap();
    harness.type_text("first ").unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(std::fs::read_to_string(&file).unwrap(), "first original\n");

    *fs.skew.lock().unwrap() = Duration::from_secs(5);
    (harness, dir, file, fs)
}

fn save_all(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Save All").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Save All");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// The mtime moved, the content is what we saved: Save All writes it.
#[test]
fn save_all_writes_a_file_whose_mtime_only_drifted() {
    let (mut harness, _dir, file, _fs) = saved_then_skewed();

    save_all(&mut harness);

    let status = harness.get_status_bar();
    assert!(
        !status.contains("changed on disk"),
        "nothing changed the file; status was {status:?}"
    );
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "first second original\n"
    );
}

/// Ctrl+S doesn't ask about overwriting a change that isn't one.
#[test]
fn save_does_not_ask_about_a_file_whose_mtime_only_drifted() {
    let (mut harness, _dir, file, _fs) = saved_then_skewed();

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_not_contains("File Changed on Disk");
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "first second original\n"
    );
}

/// The check still catches a real change, even one that keeps the file's
/// size and lands under the same skewed clock.
#[test]
fn a_real_change_of_the_same_size_is_still_caught() {
    let (mut harness, _dir, file, _fs) = saved_then_skewed();
    std::fs::write(&file, "FIRST ORIGINAL\n").unwrap();

    save_all(&mut harness);

    harness.assert_screen_contains("Not saved, changed on disk: notes.txt");
    assert_eq!(std::fs::read_to_string(&file).unwrap(), "FIRST ORIGINAL\n");
}

/// A clean buffer is not auto-reverted over a drifted mtime: reloading the
/// same bytes looked harmless but threw away the undo history.
#[test]
fn the_file_poll_does_not_reload_a_file_whose_mtime_only_drifted() {
    let (mut harness, dir, _file, _fs) = saved_then_skewed_clean();
    // A second file in a split of its own, whose reload shows when a poll
    // has run over both.
    let other = dir.path().join("other.txt");
    std::fs::write(&other, "other\n").unwrap();
    harness.open_file(&other).unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Split Vertical").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.open_file(&dir.path().join("notes.txt")).unwrap();
    harness.render().unwrap();

    std::fs::write(&other, "reloaded\n").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("reloaded"))
        .unwrap();

    // The edit before the save can still be undone.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("first original");
}

/// Saving doesn't read the file back to learn what it holds: on a remote
/// filesystem that downloaded the whole file after every Ctrl+S and every
/// auto-save. What the save wrote is known from the save itself.
#[test]
fn saving_does_not_read_the_file_back() {
    let (mut harness, _dir, file, fs) = saved_then_skewed();
    // No drift this time, which the save's own check would read the file
    // to tell apart.
    *fs.skew.lock().unwrap() = Duration::ZERO;
    fs.notes_reads.store(0, Ordering::SeqCst);

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "first second original\n"
    );
    assert_eq!(fs.notes_reads.load(Ordering::SeqCst), 0);
}

/// A same-size change under a modified buffer is read once to tell it from
/// a moved timestamp, not again on every poll after: once the file is
/// known to hold something else, what was saved is no longer compared.
#[test]
fn a_same_size_change_is_read_once_not_on_every_poll() {
    let (mut harness, dir, file, fs) = saved_then_skewed();
    // A second file in a split of its own, whose reload shows when a poll
    // has run over both.
    let other = dir.path().join("other.txt");
    std::fs::write(&other, "other\n").unwrap();
    harness.open_file(&other).unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Split Vertical").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.open_file(&file).unwrap();
    // Wide enough for the poll's "File <full path> changed on disk (...)"
    // whole, however long the temp dir's path (macOS, Windows).
    let path_len = file.canonicalize().unwrap().display().to_string().len() as u16;
    harness.resize(160 + path_len, 24).unwrap();
    harness.render().unwrap();
    fs.notes_reads.store(0, Ordering::SeqCst);

    std::fs::write(&file, "FIRST ORIGINAL\n").unwrap();
    harness
        .wait_until(|h| {
            h.get_status_bar()
                .contains("changed on disk (buffer has unsaved")
        })
        .unwrap();
    for pass in ["reloaded once\n", "reloaded twice\n"] {
        std::fs::write(&other, pass).unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains(pass.trim_end()))
            .unwrap();
    }

    assert_eq!(fs.notes_reads.load(Ordering::SeqCst), 1);
}
