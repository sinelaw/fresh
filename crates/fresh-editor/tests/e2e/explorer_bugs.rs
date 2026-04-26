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
use fresh::config::Config;
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSystem, FileWriter, StdFileSystem,
};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

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
    /// When true, rename() returns CrossesDevices (EXDEV) so the caller
    /// exercises the cross-fs copy+delete fallback path.
    fail_rename_with_exdev: AtomicBool,
    /// When set, copy() returns PermissionDenied whenever the *destination*
    /// path's file name contains this substring. Lets tests simulate a
    /// recursive copy that succeeds for some children and fails for others.
    poison_copy_substring: std::sync::Mutex<Option<String>>,
    /// When true, remove_file / remove_dir return PermissionDenied — used
    /// to exercise the "copy succeeded but source could not be removed"
    /// edge case of the cross-fs cut fallback.
    fail_remove_with_eacces: AtomicBool,
}

impl FaultInjectingFileSystem {
    fn new(inner: Arc<dyn FileSystem>) -> Self {
        Self {
            inner,
            fail_rename_with_eacces: AtomicBool::new(false),
            fail_rename_with_exdev: AtomicBool::new(false),
            poison_copy_substring: std::sync::Mutex::new(None),
            fail_remove_with_eacces: AtomicBool::new(false),
        }
    }

    fn arm_rename_eacces(&self) {
        self.fail_rename_with_eacces.store(true, Ordering::SeqCst);
    }

    fn arm_rename_exdev(&self) {
        self.fail_rename_with_exdev.store(true, Ordering::SeqCst);
    }

    fn arm_remove_eacces(&self) {
        self.fail_remove_with_eacces.store(true, Ordering::SeqCst);
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
        if self.fail_rename_with_exdev.load(Ordering::SeqCst) {
            return Err(io::Error::new(
                io::ErrorKind::CrossesDevices,
                "fault-injected: rename across filesystems (EXDEV)",
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
        if self.fail_remove_with_eacces.load(Ordering::SeqCst) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "fault-injected: remove_file not permitted",
            ));
        }
        self.inner.remove_file(path)
    }
    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        if self.fail_remove_with_eacces.load(Ordering::SeqCst) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "fault-injected: remove_dir not permitted",
            ));
        }
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

// ---------------------------------------------------------------------------
// Bug: pasting a directory into itself infinite-recurses via copy_dir_all
// ---------------------------------------------------------------------------

/// Pasting a directory into itself must be rejected outright. Without the
/// guard, `copy_dir_all(/d, /d/d)` would create `/d/d` and then iterate `/d`
/// — which now contains the just-created `/d/d` — and recurse, creating
/// `/d/d/d`, `/d/d/d/d`, and so on until stack-overflow or disk-full.
///
/// Note: the pre-fix behavior is unsafe to exercise in CI (would hang or
/// fill the test runner's disk), so this test asserts the post-fix
/// behavior: the paste errors out quickly and leaves the tree untouched.
#[test]
fn test_paste_rejects_directory_into_itself() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // A directory with a single file — keep it small, just in case the
    // fix ever regresses and the test accidentally runs the buggy path.
    let d = project_root.join("d");
    fs::create_dir(&d).unwrap();
    fs::write(d.join("inside.txt"), "x").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("d").unwrap();

    // Navigate onto d/, copy it, then paste with the cursor still on d/.
    // dst_dir becomes d/ itself, so dst_path = d/d — inside the source.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → d/
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The paste must not have spawned a recursive copy.
    assert!(
        !d.join("d").exists(),
        "Circular paste was not rejected — a nested copy was created at d/d"
    );

    // The on-disk source must be untouched: exactly the same single child.
    let children: Vec<_> = fs::read_dir(&d)
        .unwrap()
        .filter_map(|e| e.ok().map(|e| e.file_name().to_string_lossy().to_string()))
        .collect();
    assert_eq!(
        children,
        vec!["inside.txt".to_string()],
        "Source directory should be untouched after a circular paste; got {:?}",
        children
    );
}

// ---------------------------------------------------------------------------
// Bug: delete/rename collapse every expanded descendant
// ---------------------------------------------------------------------------

/// Deleting a sibling used to call `refresh_node` on the parent, which
/// collapses and re-expands it — wiping the expansion state of every
/// other child. Switching the refresh to `reload_expanded_node` preserves
/// the state of siblings that didn't go away.
#[test]
fn test_delete_preserves_sibling_expansion_state() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Two directories and a file at root. Each directory has a child so
    // they're expandable.
    fs::create_dir(project_root.join("keep_open")).unwrap();
    fs::write(project_root.join("keep_open/inside.txt"), "x").unwrap();
    fs::create_dir(project_root.join("remove_me")).unwrap();
    fs::write(project_root.join("remove_me/inside.txt"), "y").unwrap();
    fs::write(project_root.join("sibling.txt"), "z").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("sibling.txt").unwrap();

    // Expand keep_open/ so its child is visible.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // keep_open/
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // expand
    harness.render().unwrap();
    // `inside.txt` under keep_open should be visible on screen now.
    harness.assert_screen_contains("inside.txt");

    // Navigate down to sibling.txt (past keep_open's child and remove_me/)
    // and delete it. This triggers the parent refresh.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // keep_open/inside.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // remove_me/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // sibling.txt
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // keep_open/inside.txt must still be visible — the expansion state of
    // an untouched sibling should not be collapsed by deleting another
    // sibling.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("inside.txt"),
        "keep_open/inside.txt should still be visible after the sibling \
         sibling.txt was deleted; the refresh collapsed everything. Screen:\n{}",
        screen
    );
}

/// Same hazard for rename: renaming a sibling must not collapse the
/// expansion state of untouched siblings.
#[test]
fn test_rename_preserves_sibling_expansion_state() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::create_dir(project_root.join("keep_open")).unwrap();
    fs::write(project_root.join("keep_open/inside.txt"), "x").unwrap();
    fs::write(project_root.join("rename_me.txt"), "z").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness
        .wait_for_file_explorer_item("rename_me.txt")
        .unwrap();

    // Expand keep_open/.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // keep_open/
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("inside.txt");

    // Navigate to rename_me.txt and rename via F2.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // keep_open/inside.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // rename_me.txt
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("renamed.txt");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("inside.txt"),
        "keep_open/inside.txt should still be visible after a sibling rename. \
         Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// Bug: Shift+arrow at the tree boundary silently drops the selection
// ---------------------------------------------------------------------------

/// The copy-paste success message must NOT reuse the "put in clipboard"
/// string — otherwise the user sees the same `Copied: foo` after Ctrl+C
/// and after Ctrl+V and has no feedback that the paste actually
/// happened. Assert that a completed paste shows a distinct verb.
#[test]
fn test_paste_success_status_is_distinct_from_clipboard_set() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::create_dir(project_root.join("dst")).unwrap();
    fs::write(project_root.join("thing.txt"), "x").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("thing.txt").unwrap();

    // dirs sort first: root → dst → thing.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // thing.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copied:");

    // Paste into dst/. The status after paste must NOT be literally the
    // same "Copied: thing.txt" we just saw — it should be a distinct word
    // so users can tell the paste actually occurred.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Pasted:");
    // And the file really landed:
    assert!(project_root.join("dst/thing.txt").exists());
}

/// The multi-delete prompt used to say only "Delete 2 items? (y)es, (N)o" —
/// no names. That's safer with: "Delete 2 items ('foo', 'bar')? ..." so
/// the user can eyeball what they're actually about to drop. Check the
/// minibuffer row specifically (the tree pane separately lists file names).
#[test]
fn test_multi_delete_prompt_names_the_items() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    // Use distinctive names that the tree cannot display alongside each other
    // on the minibuffer row (bottom of screen) — we'll assert the prompt row
    // mentions them.
    fs::write(project_root.join("tomato.md"), "a").unwrap();
    fs::write(project_root.join("zucchini.md"), "b").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("tomato.md").unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // tomato
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap(); // extend to zucchini
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // The minibuffer (last non-empty row of the rendered screen) shows the
    // prompt text. Without the fix it reads "Delete 2 items? (y)es, (N)o"
    // with no names. With the fix it should mention the names.
    let screen = harness.screen_to_string();
    let prompt_row = screen
        .lines()
        .rev()
        .find(|l| !l.trim().is_empty())
        .unwrap_or("");
    assert!(
        prompt_row.contains("Delete")
            && prompt_row.contains("tomato")
            && prompt_row.contains("zucchini"),
        "Multi-delete prompt should name the items being deleted. Prompt:\n{}",
        prompt_row
    );

    // Cancel.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// The conflict prompt advertises `(c)ancel` but used to accept *any*
/// unrecognized input as a cancel — including typos. A typo loses the
/// clipboard and the whole paste queue with no recovery. The prompt
/// should re-prompt on unknown input and only cancel on the explicit
/// `c` / Escape keys.
#[test]
fn test_conflict_prompt_typo_re_prompts_does_not_cancel() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::create_dir(project_root.join("dst")).unwrap();
    fs::write(project_root.join("dst/a.txt"), "old").unwrap();
    fs::write(project_root.join("a.txt"), "new").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("a.txt").unwrap();

    // dirs first: root → dst → a.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // a.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.assert_screen_contains("exists");

    // Type a typo (not o / r / c) and submit. The prompt should stay
    // open and re-prompt, rather than silently cancel.
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("z");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("exists"),
        "Unknown input should not cancel the paste — the conflict prompt \
         should re-ask. Screen:\n{}",
        screen
    );

    // Now cancel explicitly with Escape and verify the source file
    // stays in place (cancel doesn't move anything).
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        project_root.join("a.txt").exists(),
        "Explicit Escape cancel should not delete the source"
    );
    // And the destination is untouched (still 'old').
    assert_eq!(
        fs::read_to_string(project_root.join("dst/a.txt")).unwrap(),
        "old",
        "Cancel should leave the destination untouched"
    );
}

/// The Explorer-specific menu (Alt+X) should expose Cut / Copy / Paste
/// alongside the other file operations it already carries (New File,
/// Rename, Delete, ...). Users who reach for that menu for file ops
/// shouldn't have to hunt in the Edit menu.
#[test]
fn test_explorer_menu_exposes_cut_copy_paste() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("anything.txt"), "x").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("anything.txt").unwrap();

    // Select a file so the Cut/Copy menu items would be enabled by
    // can_copy (explorer cursor is on a real file).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open the Explorer menu (Alt+X).
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Cut");
    harness.assert_screen_contains("Copy");
    harness.assert_screen_contains("Paste");
}

/// The rename prompt used to open with the full filename selected, so the
/// first keystroke wiped the prefilled text. "Rename slightly different"
/// — `report.txt` → `report-2.txt` — was awkward because you had to
/// retype the whole thing. The rename prompt should open with the cursor
/// at the end of the existing name, no selection, so append / edit flows
/// work naturally.
#[test]
fn test_rename_prompt_appends_to_existing_name() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("report.txt"), "data").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("report.txt").unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    // Type "-v2" via real key events so any selection-anchor behavior of
    // the prompt applies exactly as it would for a user.
    harness
        .send_key(KeyCode::Char('-'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('2'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The desired rename is "report.txt-v2" — the prefilled text should
    // have been kept and "-v2" appended. If the prompt selected-all the
    // prefilled text, typing would have replaced it, leaving just "-v2".
    assert!(
        project_root.join("report.txt-v2").exists(),
        "Expected 'report.txt-v2' (prefill preserved, appended to). \
         On-disk now: {:?}",
        fs::read_dir(project_root).unwrap().count()
    );
    assert!(
        !project_root.join("-v2").exists(),
        "File was renamed to just '-v2' — the prefilled name got replaced \
         by the first keystroke (select-all behavior). The rename prompt \
         should open with the cursor at end, not select-all."
    );
    assert!(
        !project_root.join("report.txt").exists(),
        "Original report.txt should be gone after rename"
    );
}

/// Pasting into a collapsed destination directory must auto-expand it so
/// the user sees where the file landed. Without this, the pasted file is
/// off-screen (under the closed folder) and there's no visual confirmation
/// of the target.
#[test]
fn test_paste_auto_expands_collapsed_destination() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::create_dir(project_root.join("closed_dst")).unwrap();
    // Put an existing file in the dst so it's a non-empty directory; we want
    // to verify it becomes expanded (children visible), not just refreshed.
    fs::write(project_root.join("closed_dst/existing.txt"), "old").unwrap();
    fs::write(project_root.join("moveme.txt"), "x").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("moveme.txt").unwrap();

    // Sanity: `existing.txt` should NOT be visible up-front (closed_dst is
    // collapsed). If it IS visible, the test setup is wrong.
    let pre = harness.screen_to_string();
    assert!(
        !pre.contains("existing.txt"),
        "Test precondition: closed_dst should be collapsed, but existing.txt is visible. \
         Screen:\n{}",
        pre
    );

    // dirs sort first: root → closed_dst → moveme.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // closed_dst/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // moveme.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // closed_dst/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Destination directory must now be open — both the newly-pasted file
    // and the pre-existing sibling should be visible.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("existing.txt") && screen.contains("moveme.txt"),
        "After paste, closed_dst should auto-expand and show both existing.txt \
         and moveme.txt. Screen:\n{}",
        screen
    );
}

/// Pressing Shift+Up at the top of the tree used to do nothing — the
/// boundary check returned before seeding `multi_selection` with the
/// cursor row. Escape then saw an empty selection and transferred focus
/// to the editor, which the user didn't ask for.
#[test]
fn test_shift_up_at_top_seeds_selection_before_escape_unfocuses() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("only.txt"), "x").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("only.txt").unwrap();

    // Cursor starts on the project root (pos 0 in the visible list).
    // Shift+Up is at the boundary — nothing above.
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Escape should now observe a non-empty multi-selection (seeded with
    // the cursor row) and clear it — keeping focus on the explorer.
    // Without the fix, multi-selection is empty and Escape falls through
    // to `focus_editor`, so the title bar picks up the "(Ctrl+E)" hint
    // that only appears when the explorer is NOT focused.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.assert_screen_not_contains("File Explorer (Ctrl+E)");
    harness.assert_screen_contains("File Explorer");
}

// ---------------------------------------------------------------------------
// Bug: cross-fs cut where copy succeeds but source removal fails
// ---------------------------------------------------------------------------

/// Cross-filesystem cut fallback: after rename fails with EXDEV, we copy to
/// the destination and then delete the source. If the delete fails (source
/// on a read-only volume, permission change, etc.) the user ends up with
/// the file at BOTH locations — the copy at dst and the original source
/// still in place. The status line should say so explicitly, and the
/// clipboard should NOT be cleared so the user can retry or clean up
/// manually.
#[test]
fn test_cross_fs_cut_source_delete_failure_is_reported() {
    let fault_fs = Arc::new(FaultInjectingFileSystem::new(Arc::new(StdFileSystem)));
    // 140×30 instead of 100×30: with `{remote}` on the default
    // status bar the "could not remove source: stuck.txt" message
    // is truncated at 100 cols (`Local | filename | Ln/Col |
    // diagnostics | <message> | <right side>` overflows).
    let mut harness = EditorTestHarness::create(
        140,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_filesystem(fault_fs.clone()),
    )
    .unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::create_dir(project_root.join("dst")).unwrap();
    fs::write(project_root.join("stuck.txt"), "payload").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("stuck.txt").unwrap();

    // dirs first: root → dst → stuck.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // stuck.txt
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();

    // Arm the fault just before paste: rename returns EXDEV so the code
    // falls into the copy+delete fallback; copy succeeds (inner fs is
    // the real StdFileSystem), then remove_file fails.
    fault_fs.arm_rename_exdev();
    fault_fs.arm_remove_eacces();

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Copy landed.
    assert!(
        project_root.join("dst/stuck.txt").exists(),
        "After a cross-fs cut with source-delete failure, the copy should be at dst."
    );
    // Source still there.
    assert!(
        project_root.join("stuck.txt").exists(),
        "Source must still exist when its removal failed after the cross-fs copy."
    );
    // Status line should name BOTH sides so the user knows what happened.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("stuck.txt") && screen.contains("could not"),
        "Status should mention the filename and that the source could not be \
         removed. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// Follow-ups to PR #1665
// ---------------------------------------------------------------------------

/// Pressing Escape in the file explorer after a cut must cancel the cut —
/// there is no other way to dismiss a pending cut without actually pasting
/// somewhere. Before this fix, Escape with no multi-selection and no active
/// search simply transferred focus to the editor; the clipboard stayed
/// primed, so the next Ctrl+V in the explorer (even in an unrelated flow)
/// could move the supposedly-forgotten file.
#[test]
fn test_escape_cancels_pending_cut() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    // dirs first: root → dst/ → a.txt
    fs::create_dir(project_root.join("dst")).unwrap();
    fs::write(project_root.join("a.txt"), "a").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("a.txt").unwrap();

    // Cursor: root → dst/ → a.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // a.txt
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Marked");

    // Escape should clear the pending cut while keeping focus on the explorer.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Move to dst/ and attempt to paste. With the cut cancelled, paste must
    // report "Nothing to paste" — the clipboard is empty — and the source
    // file must stay at its original location.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        project_root.join("a.txt").exists(),
        "Source must remain after Escape cancels the cut. Screen:\n{}",
        screen
    );
    assert!(
        !project_root.join("dst/a.txt").exists(),
        "Cancelled cut must not land the file at dst. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Nothing to paste"),
        "After Escape cancels a cut, a subsequent paste should find an empty \
         clipboard. Screen:\n{}",
        screen
    );
}

/// Trying to paste a cut back into its own directory should cancel the cut
/// rather than surfacing a scary "Cannot paste here" error. Cancellation is
/// the natural outcome — the user effectively changed their mind — and the
/// clipboard must be cleared so a later paste elsewhere doesn't silently
/// move the file after all.
#[test]
fn test_paste_into_same_dir_cancels_cut() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("a.txt"), "a").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("a.txt").unwrap();

    // Cursor on a.txt (root → a.txt).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Paste where cursor already is → same directory. Must cancel the cut.
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(
        project_root.join("a.txt").exists(),
        "File must stay in place when same-dir paste cancels the cut."
    );

    let first_screen = harness.screen_to_string();
    assert!(
        !first_screen.contains("Cannot paste here"),
        "Same-dir paste should be a cancellation, not an error. Screen:\n{}",
        first_screen
    );

    // A second Ctrl+V must now report an empty clipboard — the cut was
    // cancelled, not just no-op'd.
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Nothing to paste"),
        "After same-dir paste cancels a cut, the clipboard must be empty. \
         Screen:\n{}",
        screen
    );
}

/// Cut from an expanded subfolder, paste into the workspace root, then let
/// the background directory poller run. The poller used to call
/// `refresh_node` on any directory whose mtime changed (here: the source
/// parent); `refresh_node` collapses the directory and recycles every
/// descendant NodeId. Observed fallout: the source directory silently
/// collapses after ~poll_interval, and the explorer cursor — which refers
/// to a node by id — points at a freed id, so Up/Down become no-ops
/// (`select_next`/`select_prev` give up when the current id isn't in
/// `visible_nodes`).
#[test]
fn test_poll_after_cut_paste_preserves_expansion_and_cursor() {
    let mut config = Config::default();
    // Short poll interval so the test drives polling through `wait_until`
    // without waiting seconds of wall clock.
    config.editor.file_tree_poll_interval_ms = 50;

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::create_dir(project_root.join("sub")).unwrap();
    fs::write(project_root.join("sub/inner.txt"), "inner").unwrap();
    // A sibling at root so `sub/` expansion stays observable after the move
    // (the pasted file shows up at root, but "sub" should also still be listed).
    fs::write(project_root.join("sibling.txt"), "s").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("sub").unwrap();

    // Cursor: root → sub/. Expand sub/ so inner.txt is visible.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // sub/
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap(); // expand sub/
    harness.wait_for_file_explorer_item("inner.txt").unwrap();

    // Move cursor onto inner.txt and cut it.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // inner.txt
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Paste into the workspace root: move cursor up past sub/ to the root
    // line, then Ctrl+V. dst_dir = root.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // sub/
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // root
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait until the paste has landed on disk AND is reflected on screen.
    let moved_src = project_root.join("sub/inner.txt");
    let moved_dst = project_root.join("inner.txt");
    harness
        .wait_until(|h| {
            !moved_src.exists() && moved_dst.exists() && h.screen_to_string().contains("inner.txt")
        })
        .unwrap();

    // Confirm `sub/` is still rendered as expanded right after the paste.
    let right_after = harness.screen_to_string();
    assert!(
        right_after.lines().any(|l| l.contains("▼ sub")),
        "sub/ should still be expanded immediately after paste. Screen:\n{}",
        right_after
    );

    // Now let the background directory poll fire. Advance logical time past
    // the poll interval and tick the harness until the poll has completed
    // at least one full cycle (spawn bg thread → receive results → process).
    harness.advance_time(Duration::from_millis(200));
    // Give the bg poll-dir-changes thread a couple of ticks to run and
    // deliver its results.
    for _ in 0..20 {
        harness.editor_mut().process_async_messages();
        std::thread::sleep(Duration::from_millis(20));
        harness.advance_time(Duration::from_millis(100));
    }
    harness.render().unwrap();

    let after_poll = harness.screen_to_string();

    // sub/ must still be rendered expanded. The bug shows up here: a stale
    // mtime triggers refresh_node, which collapses sub/ (so the line would
    // render as "> sub" instead).
    assert!(
        after_poll.lines().any(|l| l.contains("▼ sub")),
        "After the background poll, sub/ must remain expanded. Screen:\n{}",
        after_poll
    );

    // Cursor navigation must still work. Before the fix, select_next /
    // select_prev silently no-op when the cursor's NodeId was recycled by
    // refresh_node. Drive Down once and verify the selection actually
    // changed — i.e. the cursor is still live, not stuck on a ghost id.
    let before_nav = harness
        .editor()
        .file_explorer()
        .and_then(|e| e.get_selected())
        .expect("explorer should still have a live selection after poll");
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let after_nav = harness
        .editor()
        .file_explorer()
        .and_then(|e| e.get_selected())
        .expect("explorer should still have a live selection after arrow-down");
    assert_ne!(
        before_nav, after_nav,
        "Arrow-down must move the cursor. If the poll invalidated the \
         cursor's NodeId, select_next silently no-ops and the selection \
         stays put."
    );
}

/// If the cursor was sitting on a node whose path is gone after a tree
/// refresh (file deleted in another terminal, directory pruned, …), the
/// cursor must reset to the tree root rather than hang on a stale NodeId.
/// A stale id is invisible — nothing renders as selected — and Up/Down
/// are no-ops because `select_next`/`select_prev` can't locate the id in
/// the visible list. "Cursor on root" is always a safe, recoverable state.
///
/// User-facing end-to-end: drive the whole scenario with keystrokes, check
/// the result by inspecting the rendered tree. No path comparisons, no
/// internal-state peek — those trip over macOS tempdir symlink quirks
/// (`/var` → `/private/var`) and are tied to implementation details the
/// user doesn't see.
///
/// We drive the refresh path directly (the same method the background
/// poller uses) rather than relying on filesystem mtime detection to
/// trigger it. The mtime-based detection is too environment-sensitive
/// to rely on across CI filesystems; the contract we care about here
/// is "if the cursor's path is gone after a refresh, the cursor is
/// still usable", not "the poller notices a delete".
#[test]
fn test_refresh_resets_cursor_to_root_when_path_disappears() {
    let mut harness = EditorTestHarness::with_temp_project(120, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("doomed.txt"), "d").unwrap();
    fs::write(project_root.join("survivor.txt"), "s").unwrap();

    // Open the explorer and move the cursor onto doomed.txt.
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("doomed.txt").unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // doomed.txt

    // Precondition: the explorer renders the cursor glyph `▌` on the
    // doomed.txt row (see `view/ui/file_explorer.rs` for the highlight).
    let screen_before = harness.screen_to_string();
    let cursor_on_doomed = screen_before
        .lines()
        .any(|line| line.contains("▌") && line.contains("doomed.txt"));
    assert!(
        cursor_on_doomed,
        "precondition: cursor should be on doomed.txt. Screen:\n{}",
        screen_before
    );

    // Delete the file outside the editor, then drive the refresh path
    // (the same one the background poll uses, minus the mtime race).
    // Passing the tree's own root path ensures this works on macOS where
    // tempdirs live under a `/var` symlink: the editor canonicalizes
    // `working_dir` at startup, so the tree stores `/private/var/...`
    // paths; using `editor().working_dir()` matches that form.
    fs::remove_file(project_root.join("doomed.txt")).unwrap();
    let tree_root = harness.editor().working_dir().to_path_buf();
    harness.editor_mut().refresh_file_tree_dirs(&[tree_root]);
    harness.render().unwrap();

    // User-facing outcome #1: the deleted file's row is gone from the
    // rendered tree.
    let screen_after = harness.screen_to_string();
    let tree_lines_after: Vec<&str> = screen_after.lines().filter(|l| l.contains("│")).collect();
    let still_listed = tree_lines_after.iter().any(|l| l.contains("doomed.txt"));
    assert!(
        !still_listed,
        "refresh should have dropped the deleted file from the tree. Screen:\n{}",
        screen_after
    );

    // User-facing outcome #2: the cursor glyph has moved off doomed.txt
    // and is now on the tree root (the live fallback). If the fix were
    // missing, the cursor's NodeId would be stale and nothing would
    // render as selected at all.
    let cursor_on_root = screen_after
        .lines()
        .any(|line| line.contains("▌") && line.contains("project_root"));
    assert!(
        cursor_on_root,
        "cursor should have reset to the project root row. Screen:\n{}",
        screen_after
    );

    // User-facing outcome #3: Down still navigates. Before the fix, a
    // stale cursor id would make `select_next` a silent no-op.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let screen_after_down = harness.screen_to_string();
    let cursor_moved_off_root = screen_after_down
        .lines()
        .any(|line| line.contains("▌") && !line.contains("project_root"));
    assert!(
        cursor_moved_off_root,
        "arrow-down must move the cursor off the root. Screen:\n{}",
        screen_after_down
    );
}

/// Deleting a file in the explorer used to leave any open buffer backed
/// by that file alive. The tab kept rendering with stale content; and
/// Ctrl+S on that buffer wrote back to the trashed path, silently
/// resurrecting the file the user just deleted. Assertion is strict:
/// the tab bar must no longer advertise the file, AND the file must
/// not reappear on disk (any lingering buffer could resurrect it via
/// auto-save or an incidental Ctrl+S).
#[test]
fn test_delete_closes_open_buffer_for_deleted_file() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("victim.txt"), "v").unwrap();
    fs::write(project_root.join("bystander.txt"), "b").unwrap();

    // Open victim.txt as a permanent tab via the explorer (Enter on
    // the selected file is the "open permanently" gesture).
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("victim.txt").unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // bystander.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // victim.txt
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Precondition: the tab bar (row 1) now shows victim.txt.
    let tab_bar_before = harness.screen_row_text(1);
    assert!(
        tab_bar_before.contains("victim.txt"),
        "precondition: victim.txt should be open in a tab. Tab bar: {:?}",
        tab_bar_before
    );

    // Go back to the explorer, navigate to victim.txt, delete it.
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer_item("victim.txt").unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // bystander.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // victim.txt
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    // The deleted file's tab must be closed. Inspect only the tab bar
    // row so a leftover status line or title doesn't create a false
    // positive.
    let tab_bar_after = harness.screen_row_text(1);
    assert!(
        !tab_bar_after.contains("victim.txt"),
        "tab for deleted file must be closed. Tab bar: {:?}",
        tab_bar_after
    );

    // No lingering buffer can be resurrecting the file on save / auto-save.
    assert!(
        !project_root.join("victim.txt").exists(),
        "deleted file must not reappear at its original path"
    );
}

/// Renaming a directory in the explorer used to leave any buffer for a
/// file *under* that directory still pointing at the old path — so a
/// subsequent Ctrl+S wrote to the old (now-gone) location, recreating
/// the old directory alongside the renamed one. Drive the full
/// user-visible flow: open a file, rename its parent dir, type into
/// the buffer, save, and confirm on disk that the save landed at the
/// new path and did NOT resurrect the old one.
#[test]
fn test_rename_directory_redirects_save_to_new_path() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::create_dir(project_root.join("mydir")).unwrap();
    fs::write(project_root.join("mydir").join("inner.txt"), "orig\n").unwrap();

    // Open mydir/inner.txt as a permanent tab (Enter also focuses the editor).
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("mydir").unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // mydir
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap(); // expand mydir
    harness.wait_for_file_explorer_item("inner.txt").unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // inner.txt
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Back to the explorer, cursor onto the directory, F2 to rename.
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer_item("mydir").unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // mydir (back up from inner.txt)
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();
    // Clear the default-filled name and type the new one.
    for _ in 0..16 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    for ch in "renamed_dir".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    let new_inner = project_root.join("renamed_dir").join("inner.txt");
    let old_inner = project_root.join("mydir").join("inner.txt");
    assert!(new_inner.exists(), "rename must have landed on disk");
    assert!(
        !project_root.join("mydir").exists(),
        "old directory must be gone after rename"
    );

    // Switch focus back to the editor (Ctrl+E from explorer focuses the
    // editor), append a sentinel, Ctrl+S to save.
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    for ch in "SENTINEL\n".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();

    // User-facing outcome: the sentinel landed in the renamed-directory
    // file, and nothing resurrected the old directory or the old file.
    harness
        .wait_until(|_| {
            fs::read_to_string(&new_inner)
                .map(|s| s.contains("SENTINEL"))
                .unwrap_or(false)
        })
        .unwrap();
    assert!(
        !old_inner.exists(),
        "save must not recreate the file at the old path: {:?}",
        old_inner
    );
    assert!(
        !project_root.join("mydir").exists(),
        "save must not recreate the old parent directory"
    );
}

/// Cutting a file and pasting into another directory used to leave the
/// buffer backed by the old (now-gone) path — so Ctrl+S recreated the
/// file at its original location, leaving the user with duplicates (a
/// moved copy AND a resurrected ghost). Drive the full flow through
/// keystrokes and check the on-disk layout after a save.
#[test]
fn test_cut_paste_move_redirects_save_to_new_path() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::create_dir(project_root.join("dst")).unwrap();
    fs::write(project_root.join("source.txt"), "orig\n").unwrap();

    // Open source.txt as a permanent tab (Enter also focuses the editor).
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("source.txt").unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst (dirs first)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // source.txt
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Back to explorer, cut source.txt, paste into dst/.
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer_item("source.txt").unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // source.txt
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let new_path = project_root.join("dst").join("source.txt");
    let old_path = project_root.join("source.txt");
    assert!(
        new_path.exists(),
        "file should be at new location after paste"
    );
    assert!(
        !old_path.exists(),
        "file should not be at old location after paste"
    );

    // Focus editor, append sentinel, save.
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    for ch in "SENTINEL\n".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();

    // User-facing outcome: the sentinel is in the moved file at the new
    // location, and the old path was not resurrected.
    harness
        .wait_until(|_| {
            fs::read_to_string(&new_path)
                .map(|s| s.contains("SENTINEL"))
                .unwrap_or(false)
        })
        .unwrap();
    assert!(
        !old_path.exists(),
        "save must not recreate the file at the old path: {:?}",
        old_path
    );
}
