//! Specification tests for the orchestrator bring-up flow.
//!
//! Sessions are the directories themselves: one per directory,
//! discovered at boot from the per-dir workspace cache
//! (`<data>/orchestrator/../workspaces/*.json`). There is no central
//! windows.json any more (it was dropped; a legacy one is migrated into
//! the workspace files and retired on first read).
//!
//! The spec (CLI dir matched by `root`):
//!   * `fresh <dir>` foregrounds the window rooted at `<dir>`; a
//!     worktree session (root != `<dir>`) is NEVER foregrounded by
//!     passing the project dir — it stays an inactive shell, divable
//!     via the orchestrator.
//!   * passing a worktree dir foregrounds the session rooted there.
//!   * sessions for other directories are preserved as inactive shells.
//!   * a directory that no longer exists is garbage-collected, not
//!     surfaced as a session.
//!
//! Plugins are disabled so the tests exercise only the Rust core path.

mod common;

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::StdFileSystem;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

/// The set of temp dirs a bring-up scenario plays out in. `data_root`
/// is what `DirectoryContext::for_testing` is rooted at; the editor's
/// data dir is therefore `data_root/data`.
struct Scenario {
    project: TempDir,
    worktree: TempDir,
    other: TempDir,
    data_root: TempDir,
    project_canon: PathBuf,
    worktree_canon: PathBuf,
    other_canon: PathBuf,
}

impl Scenario {
    fn new() -> Self {
        let project = TempDir::new().unwrap();
        let worktree = TempDir::new().unwrap();
        let other = TempDir::new().unwrap();
        let data_root = TempDir::new().unwrap();
        let project_canon = project.path().canonicalize().unwrap();
        let worktree_canon = worktree.path().canonicalize().unwrap();
        let other_canon = other.path().canonicalize().unwrap();
        Self {
            project,
            worktree,
            other,
            data_root,
            project_canon,
            worktree_canon,
            other_canon,
        }
    }

    fn data_dir(&self) -> PathBuf {
        self.data_root.path().join("data")
    }

    /// Seed a per-dir workspace file for `root` carrying `label`, the
    /// way a prior session would have left one on disk. This is the
    /// session registry now — discovery picks it up at boot.
    fn place_workspace(&self, root: &Path, label: &str) {
        let ws_dir = self.data_dir().join("workspaces");
        std::fs::create_dir_all(&ws_dir).unwrap();
        let canonical = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
        let mut ws = fresh::workspace::Workspace::new(canonical.clone());
        ws.label = Some(label.to_string());
        let filename = format!(
            "{}.json",
            fresh::workspace::encode_path_for_filename(&canonical)
        );
        std::fs::write(
            ws_dir.join(filename),
            serde_json::to_vec_pretty(&ws).unwrap(),
        )
        .unwrap();
    }

    /// Construct the editor exactly as a `fresh <project>` launch would
    /// (read persistence, discover sessions, pick the foreground window,
    /// build the windows map).
    fn bring_up(&self) -> fresh::app::Editor {
        self.bring_up_in(&self.project_canon)
    }

    /// Like [`Self::bring_up`] but launches with an explicit cwd.
    fn bring_up_in(&self, cwd: &Path) -> fresh::app::Editor {
        let dir_context = DirectoryContext::for_testing(self.data_root.path());
        let filesystem: Arc<dyn fresh::model::filesystem::FileSystem + Send + Sync> =
            Arc::new(StdFileSystem);
        let config = Config {
            check_for_updates: false,
            ..Config::default()
        };
        fresh::app::Editor::for_test(
            config,
            80,
            24,
            Some(cwd.to_path_buf()),
            dir_context,
            fresh::view::color_support::ColorCapability::TrueColor,
            filesystem,
            None,
            None,
            false,
            false,
        )
        .unwrap()
    }
}

/// Enumerate the roots of every window the editor built, sorted, for
/// stable assertions.
fn window_roots(editor: &fresh::app::Editor) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    // Window ids are small monotonic integers; scan a generous range.
    for id in 1..=64u64 {
        if let Some(w) = editor.session(fresh_core::WindowId(id)) {
            roots.push(w.root.clone());
        }
    }
    roots.sort();
    roots
}

// ---------------------------------------------------------------------------
// Branch A: no persisted state at all.
// ---------------------------------------------------------------------------
#[test]
fn no_persistence_boots_clean_base_at_cwd() {
    let s = Scenario::new();
    let editor = s.bring_up();

    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "with no sessions the foreground window is a clean base at the launch cwd"
    );
    assert_eq!(editor.working_dir(), s.project_canon.as_path());
    assert_eq!(editor.session_count(), 1, "only the base window exists");
    assert_eq!(editor.session_name(), None);
}

// ---------------------------------------------------------------------------
// Branch B: a session exists for the cwd. Launching there reopens it.
// ---------------------------------------------------------------------------
#[test]
fn session_for_cwd_reopens_at_cwd() {
    let s = Scenario::new();
    s.place_workspace(&s.project_canon, "project");
    let editor = s.bring_up();

    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "the session rooted at the cwd is foregrounded"
    );
    assert_eq!(editor.session_count(), 1);
}

// ---------------------------------------------------------------------------
// Branch C: a worktree session (root != cwd) exists alongside the cwd
// session. SPEC: passing the project dir foregrounds the project window,
// never the worktree session, which survives as an inactive shell.
// ---------------------------------------------------------------------------
#[test]
fn worktree_session_does_not_hijack_plain_launch() {
    let s = Scenario::new();
    s.place_workspace(&s.project_canon, "project");
    s.place_workspace(&s.worktree_canon, "worktree");
    let editor = s.bring_up();

    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "the project-rooted window is foreground, not the worktree session"
    );
    assert_eq!(
        editor.working_dir(),
        s.project_canon.as_path(),
        "working_dir matches the foreground window's root"
    );
    let roots = window_roots(&editor);
    assert!(
        roots.contains(&s.worktree_canon),
        "the worktree session survives as an inactive shell"
    );
    assert!(roots.contains(&s.project_canon));
}

// ---------------------------------------------------------------------------
// Branch C-converse: launching directly IN the worktree dir foregrounds
// that worktree's session (root == cwd), restoring its label — not a
// fresh clean base.
// ---------------------------------------------------------------------------
#[test]
fn launching_in_a_worktree_foregrounds_that_session() {
    let s = Scenario::new();
    s.place_workspace(&s.project_canon, "project");
    s.place_workspace(&s.worktree_canon, "the-worktree-session");
    let editor = s.bring_up_in(&s.worktree_canon);

    assert_eq!(
        editor.active_window().root,
        s.worktree_canon,
        "launching in the worktree foregrounds a window rooted there"
    );
    assert_eq!(
        editor.active_window().label,
        "the-worktree-session",
        "the persisted session's label is restored, proving it's that session"
    );
    assert_eq!(editor.working_dir(), s.worktree_canon.as_path());
}

// ---------------------------------------------------------------------------
// Branch D: a session exists only for an unrelated directory. Launching
// in our cwd boots a clean base at the cwd (no cross-project bleed) AND
// preserves the unrelated session as an inactive shell.
// ---------------------------------------------------------------------------
#[test]
fn cross_project_only_boots_clean_base_and_preserves_other() {
    let s = Scenario::new();
    s.place_workspace(&s.other_canon, "other");
    let editor = s.bring_up();

    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "no session belongs to the cwd, so a clean base is booted (no cross-project bleed)"
    );
    assert_eq!(
        editor.session_count(),
        2,
        "the unrelated directory's session is preserved (no id-collision drop)"
    );
    assert!(
        window_roots(&editor).contains(&s.other_canon),
        "the unrelated directory's session survives as an inactive shell"
    );
}

// ---------------------------------------------------------------------------
// Branch E: a session whose directory no longer exists is garbage-
// collected — it must not be surfaced as a window. (The workspace cache
// accumulates an entry for every dir ever opened; dead ones are pruned.)
// ---------------------------------------------------------------------------
#[test]
fn stale_session_for_deleted_dir_is_gced() {
    let s = Scenario::new();
    // A session for a dir that we delete before bring-up.
    let gone = TempDir::new().unwrap();
    let gone_canon = gone.path().canonicalize().unwrap();
    s.place_workspace(&gone_canon, "gone");
    s.place_workspace(&s.other_canon, "other");
    drop(gone); // remove the directory from disk

    let editor = s.bring_up();
    let roots = window_roots(&editor);
    assert!(
        !roots.contains(&gone_canon),
        "a session whose directory is gone is GC'd, not surfaced: {roots:?}"
    );
    assert!(
        roots.contains(&s.other_canon),
        "the surviving directory's session is kept"
    );
}

// ---------------------------------------------------------------------------
// Branch F: restore disabled. Even with a cwd session present,
// `restore_previous_session = false` still picks the foreground window
// (phase B) but skips the workspace-content restore (phase C).
// ---------------------------------------------------------------------------
#[test]
fn restore_previous_session_false_still_picks_window_but_skips_workspace() {
    let s = Scenario::new();
    s.place_workspace(&s.project_canon, "project");

    let dir_context = DirectoryContext::for_testing(s.data_root.path());
    let filesystem: Arc<dyn fresh::model::filesystem::FileSystem + Send + Sync> =
        Arc::new(StdFileSystem);
    let mut config = Config {
        check_for_updates: false,
        ..Config::default()
    };
    config.editor.restore_previous_session = false;

    let editor = fresh::app::Editor::for_test(
        config,
        80,
        24,
        Some(s.project_canon.clone()),
        dir_context,
        fresh::view::color_support::ColorCapability::TrueColor,
        filesystem,
        None,
        None,
        false,
        false,
    )
    .unwrap();

    assert_eq!(editor.active_window().root, s.project_canon);
}
