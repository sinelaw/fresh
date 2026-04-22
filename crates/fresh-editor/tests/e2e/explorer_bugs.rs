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
