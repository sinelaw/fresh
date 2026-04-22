//! Regression tests for correctness bugs in the explorer cut/copy/paste
//! flow (feat/multi-selection PR follow-up).
//!
//! These tests are written to FAIL against the current implementation and
//! PASS once the corresponding fix lands.
//!
//! Covered bugs:
//! 1. `FileTreeView::multi_selection` keeps dead `NodeId`s after the tree
//!    mutates (e.g. the source nodes go away during a cut+paste). A
//!    subsequent explorer-scoped action (Ctrl+C, Delete, …) silently
//!    operates on an empty set or reports "Cannot copy project root".
//!
//! 2. `perform_file_explorer_paste` falls back to copy+delete on *any*
//!    rename error, not just `EXDEV`. A `rename` refused for a semantic
//!    reason (EACCES, EBUSY, …) then silently succeeds via a different
//!    codepath, which is surprising and data-unsafe.
//!
//! 3. When the copy+delete fallback triggers on a directory cut and the
//!    recursive copy fails partway, the partially-written destination is
//!    left on disk. The user sees an error but half of the source tree
//!    now exists at the destination.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSystem, FileWriter, StdFileSystem,
};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

// ---------------------------------------------------------------------------
// Test filesystem: wraps StdFileSystem and can be armed to inject faults
// into rename() / copy() to reproduce bugs #2 and #3.
// ---------------------------------------------------------------------------

struct FaultInjectingFileSystem {
    inner: Arc<dyn FileSystem>,
    /// When true, rename() returns PermissionDenied (a non-EXDEV error) so
    /// we can observe whether the caller incorrectly falls back to
    /// copy+delete.
    fail_rename_with_eacces: AtomicBool,
    /// When set, copy() returns PermissionDenied whenever the *destination*
    /// path's file name contains this substring. Lets tests simulate a
    /// recursive copy that succeeds for some children and fails for others.
    poison_copy_substring: std::sync::Mutex<Option<String>>,
}

impl FaultInjectingFileSystem {
    fn new(inner: Arc<dyn FileSystem>) -> Self {
        Self {
            inner,
            fail_rename_with_eacces: AtomicBool::new(false),
            poison_copy_substring: std::sync::Mutex::new(None),
        }
    }

    fn arm_rename_eacces(&self) {
        self.fail_rename_with_eacces.store(true, Ordering::SeqCst);
    }

    fn arm_copy_poison(&self, substring: &str) {
        *self.poison_copy_substring.lock().unwrap() = Some(substring.to_string());
    }

    fn poison_match(&self, dst: &Path) -> bool {
        let Some(name) = dst.file_name().and_then(|n| n.to_str()) else {
            return false;
        };
        match &*self.poison_copy_substring.lock().unwrap() {
            Some(s) => name.contains(s),
            None => false,
        }
    }
}

impl FileSystem for FaultInjectingFileSystem {
    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        if self.fail_rename_with_eacces.load(Ordering::SeqCst) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "fault-injected: rename not permitted (EACCES)",
            ));
        }
        self.inner.rename(from, to)
    }

    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        if self.poison_match(to) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "fault-injected: copy of poisoned name refused",
            ));
        }
        self.inner.copy(from, to)
    }

    // ---- boilerplate delegation to the inner filesystem ----
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
        fresh::model::filesystem::default_search_file(&*self.inner, path, pattern, opts, cursor)
    }
    fn walk_files(
        &self,
        root: &Path,
        skip_dirs: &[&str],
        cancel: &std::sync::atomic::AtomicBool,
        on_file: &mut dyn FnMut(&Path, &str) -> bool,
    ) -> io::Result<()> {
        self.inner.walk_files(root, skip_dirs, cancel, on_file)
    }
}

// ---------------------------------------------------------------------------
// Bug #1: stale NodeIds in multi_selection after cut+paste
// ---------------------------------------------------------------------------

/// After a multi-selection cut+paste completes, the NodeIds that were in
/// `multi_selection` point at removed source nodes. A subsequent Ctrl+C
/// must copy the current cursor item (yielding the single-item "Copied:"
/// status), not fall through to "Cannot copy project root" because the
/// stale IDs were the only ones in `effective_selection`.
#[test]
fn test_multi_selection_cleared_after_cut_paste() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // dirs sort first: root → dst/ → a.txt → b.txt → c.txt
    fs::create_dir(project_root.join("dst")).unwrap();
    fs::write(project_root.join("a.txt"), "a").unwrap();
    fs::write(project_root.join("b.txt"), "b").unwrap();
    fs::write(project_root.join("c.txt"), "c").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("a.txt").unwrap();

    // Select a.txt + b.txt + c.txt via Shift+Down extension.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // a.txt
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap(); // extend to b.txt
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap(); // extend to c.txt
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Marked");

    // Navigate to dst/ and paste.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // b.txt
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // a.txt
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // All three files should now live in dst/.
    assert!(project_root.join("dst/a.txt").exists());
    assert!(project_root.join("dst/b.txt").exists());
    assert!(project_root.join("dst/c.txt").exists());

    // Ctrl+C at this point must act on the cursor (now inside dst/), *not*
    // on the stale NodeIds for the removed source files. If the stale IDs
    // are still live, `effective_selection` returns them, they all get
    // filtered out, and the user sees "Cannot copy project root".
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Cannot copy project root"),
        "Stale multi-selection NodeIds caused Ctrl+C to fall through to the \
         empty-selection / root error. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Copied"),
        "Ctrl+C after cut+paste should copy the current cursor item. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// Bug #2: copy+delete fallback fires for any rename error (should be EXDEV-only)
// ---------------------------------------------------------------------------

/// When `rename()` fails with a non-EXDEV error (here: EACCES / permission
/// denied) during a cut+paste, the editor must NOT silently fall back to
/// copy+delete. A permission-refused move surfaces as an error; the source
/// file stays put.
#[test]
fn test_cut_does_not_fall_back_on_permission_denied() {
    let fault_fs = Arc::new(FaultInjectingFileSystem::new(Arc::new(StdFileSystem)));
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_filesystem(fault_fs.clone()),
    )
    .unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::create_dir(project_root.join("dst")).unwrap();
    fs::write(project_root.join("moveme.txt"), "payload").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("moveme.txt").unwrap();

    // dirs first: root → dst/ → moveme.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // moveme.txt
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();

    // Arm the fault just before paste so the initial tree/project setup is
    // unaffected.
    fault_fs.arm_rename_eacces();

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Correct behavior: rename refused → error surfaced, source intact,
    // destination untouched. The buggy behavior silently succeeds via
    // copy+delete, leaving moveme.txt under dst/.
    assert!(
        project_root.join("moveme.txt").exists(),
        "Source file must remain in place when rename is refused with a \
         non-EXDEV error; the buggy fallback incorrectly moves it anyway."
    );
    assert!(
        !project_root.join("dst/moveme.txt").exists(),
        "Destination must not receive the file when rename is refused \
         with a non-EXDEV error; the buggy fallback copies it anyway."
    );
}

// ---------------------------------------------------------------------------
// Bug #3: partial destination left behind when dir cut fallback fails
// ---------------------------------------------------------------------------

/// When a cross-filesystem directory cut falls back to `copy_dir_all` and
/// the recursive copy fails mid-way, the half-written destination must be
/// cleaned up. The buggy implementation returns the error but leaves a
/// half-copy in place.
#[test]
fn test_cut_cleanup_on_partial_dir_copy_failure() {
    let fault_fs = Arc::new(FaultInjectingFileSystem::new(Arc::new(StdFileSystem)));
    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_filesystem(fault_fs.clone()),
    )
    .unwrap();
    let project_root = harness.project_dir().unwrap();

    // src_dir holds three children. We poison the copy() of `poison.txt`
    // so the recursive copy succeeds for `good1.txt`, then fails; `good2`
    // may or may not be reached depending on iteration order.
    let src_dir = project_root.join("src_dir");
    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("good1.txt"), "1").unwrap();
    fs::write(src_dir.join("poison.txt"), "x").unwrap();
    fs::write(src_dir.join("good2.txt"), "2").unwrap();
    fs::create_dir(project_root.join("dst")).unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("src_dir").unwrap();

    // dirs sort first, and src_dir comes before dst alphabetically:
    // root → dst/ → src_dir/.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // src_dir/
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();

    // Force the copy fallback path: rename fails with EXDEV so the code
    // falls back to copy_dir_all, and poison.txt's copy fails.
    fault_fs.arm_rename_eacces();
    fault_fs.arm_copy_poison("poison");

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // After the fix, the half-written dst/src_dir must be cleaned up on
    // failure. The buggy code returns the error but leaves the partial
    // directory in place.
    let partial_dst = project_root.join("dst/src_dir");
    assert!(
        !partial_dst.exists(),
        "Partial destination directory {:?} was left on disk after the \
         recursive copy failed. The implementation must roll back the \
         half-written copy before returning the error.",
        partial_dst
    );

    // The source must also be untouched — the cut only commits to removing
    // it once the copy succeeds end-to-end.
    assert!(
        src_dir.join("good1.txt").exists(),
        "Source files were removed despite the copy failing."
    );
}
