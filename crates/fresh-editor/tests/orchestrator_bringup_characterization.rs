//! Specification tests for the orchestrator bring-up flow (issue #2056).
//!
//! These pin the DESIRED behavior of `Editor` construction + workspace
//! restore when there are persisted orchestrator sessions on disk,
//! across the persistence layouts a real user accumulates:
//!
//!   - v2 global   `<data>/orchestrator/windows.json`
//!   - v1 per-cwd  `<data>/orchestrator/<encoded-cwd>/windows.json`  (migrated on read)
//!   - v0.3.6      `<project>/.fresh/windows.json`                   (in the working tree)
//!
//! These assert the TARGET behavior, satisfied by the issue-#2056 fix
//! (root-matched launch pick + per-window explorer root + foreign-id
//! preservation).
//!
//! The spec (CLI dir matched by `root`):
//!   * `fresh <dir>` activates a window rooted at `<dir>`; a worktree
//!     session (root != `<dir>`) is NEVER activated by passing the
//!     project dir — it stays an inactive shell, divable via the
//!     orchestrator.
//!   * passing a worktree dir restores the session rooted there.
//!   * persisted windows for other projects are preserved as inactive
//!     shells (no id-collision drop).
//!
//! Fixtures live in `tests/fixtures/orchestrator_bringup/*.json` with
//! `__PROJECT__` / `__WORKTREE__` / `__OTHER__` path tokens that the
//! harness substitutes with real canonicalized temp dirs. The real
//! reader parses them during `Editor` construction, so a malformed
//! fixture surfaces as "no sessions" rather than a false pass.

mod common;

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::StdFileSystem;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

const FIXTURES: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/orchestrator_bringup"
);

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

    /// Load a fixture template and substitute the path tokens with
    /// this scenario's real canonicalized dirs.
    fn render_fixture(&self, name: &str) -> String {
        let raw = std::fs::read_to_string(Path::new(FIXTURES).join(name))
            .unwrap_or_else(|e| panic!("read fixture {name}: {e}"));
        raw.replace("__PROJECT__", &json_path(&self.project_canon))
            .replace("__WORKTREE__", &json_path(&self.worktree_canon))
            .replace("__OTHER__", &json_path(&self.other_canon))
    }

    /// Write a fixture to the v2 global location.
    fn place_v2_global(&self, fixture: &str) {
        let orch = self.data_dir().join("orchestrator");
        std::fs::create_dir_all(&orch).unwrap();
        std::fs::write(orch.join("windows.json"), self.render_fixture(fixture)).unwrap();
    }

    /// Write a fixture to the v1 per-cwd location (encoded by the
    /// launch project path), so the first read triggers migration.
    fn place_v1_percwd(&self, fixture: &str) {
        let encoded = fresh::workspace::encode_path_for_filename(&self.project_canon);
        let dir = self.data_dir().join("orchestrator").join(encoded);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("windows.json"), self.render_fixture(fixture)).unwrap();
    }

    /// Write a fixture to the v0.3.6 `<project>/.fresh/` location.
    fn place_v036_dotfresh(&self, fixture: &str) {
        let dir = self.project_canon.join(".fresh");
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("windows.json"), self.render_fixture(fixture)).unwrap();
    }

    /// Construct the editor exactly as a `fresh .` launch in the
    /// project would (phase B of bring-up: read persistence, pick the
    /// active window, build the windows map). Plugins are disabled so
    /// the test exercises only the Rust core path.
    fn bring_up(&self) -> fresh::app::Editor {
        self.bring_up_in(&self.project_canon)
    }

    /// Like [`Self::bring_up`] but launches with an explicit cwd (e.g.
    /// the worktree dir) to exercise the converse of the invariant.
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

/// Render a PathBuf into the JSON string body (without surrounding
/// quotes) using serde so platform path escaping matches what the
/// reader expects. The token in the fixture sits inside `"..."`, so we
/// strip serde's quotes and splice the inner escaped form back in.
fn json_path(p: &Path) -> String {
    let quoted = serde_json::to_string(p).unwrap();
    quoted.trim_matches('"').to_string()
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
        "with no windows.json the active window is a clean base at the launch cwd"
    );
    assert_eq!(editor.working_dir(), s.project_canon.as_path());
    assert_eq!(editor.session_count(), 1, "only the base window exists");
    assert_eq!(editor.session_name(), None);
}

// ---------------------------------------------------------------------------
// Branch B: v2, only a base window rooted at the cwd, active == that.
// ---------------------------------------------------------------------------
#[test]
fn v2_base_only_reopens_base_at_cwd() {
    let s = Scenario::new();
    s.place_v2_global("v2_base_only.json");
    let editor = s.bring_up();

    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "the persisted base window is rooted at the cwd, so it reopens cleanly"
    );
    assert_eq!(editor.session_count(), 1);
}

// ---------------------------------------------------------------------------
// Branch C: v2 worktree session whose project_path == cwd, persisted as
// `active`. SPEC: passing the project dir must activate the project-
// rooted window, NOT the worktree session.
// ---------------------------------------------------------------------------
#[test]
fn v2_worktree_session_does_not_hijack_plain_launch() {
    let s = Scenario::new();
    s.place_v2_global("v2_worktree_session.json");
    let editor = s.bring_up();

    // DESIRED: launching `fresh <project>` activates the window rooted at
    // the project (matched by `root`), never the worktree session.
    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "the project-rooted window is active, not the worktree session"
    );

    // The invariant: working_dir is consistent with the active window.
    assert_eq!(
        editor.working_dir(),
        s.project_canon.as_path(),
        "working_dir matches the active window's root"
    );

    // The worktree session is preserved as an inactive shell (divable via
    // the orchestrator), so both windows still exist.
    let roots = window_roots(&editor);
    assert!(
        roots.contains(&s.worktree_canon),
        "worktree survives as a shell"
    );
    assert!(roots.contains(&s.project_canon));
}

// ---------------------------------------------------------------------------
// Branch C-converse: launching directly IN the worktree dir must restore
// that worktree's session (root == cwd), not boot a fresh clean base.
// This is the other half of the invariant: a worktree dir is restorable
// by passing it, even though the project dir does NOT pull it in.
//
// Discriminating: the project_path-matching (pre-fix) logic would NOT
// match the worktree session when cwd == worktree (its project_path is
// the project, not the worktree), so it would boot a clean base at a
// fresh id with an empty label. Asserting the persisted id + label
// fails on that behavior and passes only with root-matching.
// ---------------------------------------------------------------------------
#[test]
fn launching_in_a_worktree_restores_that_worktree_session() {
    let s = Scenario::new();
    s.place_v2_global("v2_worktree_session.json");
    let editor = s.bring_up_in(&s.worktree_canon);

    assert_eq!(
        editor.active_window().root,
        s.worktree_canon,
        "launching in the worktree activates a window rooted there"
    );
    assert_eq!(
        editor.active_window().id,
        fresh_core::WindowId(2),
        "it is the PERSISTED worktree session (id 2), not a fresh clean base"
    );
    assert_eq!(
        editor.active_window().label,
        "anna-katharine-green_hand-and-ring",
        "the persisted session's label is restored, proving it's that session"
    );
    assert_eq!(editor.working_dir(), s.worktree_canon.as_path());
}

// ---------------------------------------------------------------------------
// Branch D: v2 with a session only for an unrelated project (its base
// window, persisted at id 1). SPEC: launching in our cwd boots a clean
// base at the cwd (no cross-project bleed) AND preserves the unrelated
// project's window as an inactive shell — the clean-base fallback must
// take a non-colliding id rather than reusing id 1 and dropping it.
// ---------------------------------------------------------------------------
#[test]
fn v2_cross_project_only_boots_clean_base_and_preserves_other() {
    let s = Scenario::new();
    s.place_v2_global("v2_cross_project_only.json");
    let editor = s.bring_up();

    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "no window belongs to the cwd, so a clean base is booted (no cross-project bleed)"
    );
    assert_eq!(
        editor.session_count(),
        2,
        "the unrelated project's window is preserved (no id-collision drop)"
    );
    assert!(
        window_roots(&editor).contains(&s.other_canon),
        "the unrelated project's window survives as an inactive shell"
    );
}

// ---------------------------------------------------------------------------
// Branch E: v2 with BOTH a base(cwd) and a worktree(cwd) session, the
// worktree persisted as `active`. SPEC: the cwd-rooted base wins; the
// worktree session is never activated by passing the project dir.
// ---------------------------------------------------------------------------
#[test]
fn v2_base_and_worktree_activates_the_base() {
    let s = Scenario::new();
    s.place_v2_global("v2_base_and_worktree.json");
    let editor = s.bring_up();

    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "the cwd-rooted base is active; the worktree session is not activated"
    );
    assert_eq!(
        editor.working_dir(),
        s.project_canon.as_path(),
        "working_dir matches the active window's root"
    );
    assert!(
        window_roots(&editor).contains(&s.worktree_canon),
        "the worktree session survives as an inactive shell"
    );
}

// ---------------------------------------------------------------------------
// Branch F: v1 per-cwd legacy file present, no global file. First read
// migrates it into the global store. SPEC: even after migration, the
// migrated worktree window (root = worktree) must NOT be activated by
// launching in the project — a clean base at the cwd is activated, and
// the migration side effects still occur.
// ---------------------------------------------------------------------------
#[test]
fn v1_legacy_percwd_migrates_then_activates_base_at_cwd() {
    let s = Scenario::new();
    s.place_v1_percwd("v1_legacy_percwd.json");
    let editor = s.bring_up();

    // DESIRED: the migrated legacy worktree window (root = worktree) is
    // matched by `root`, so it does not match the cwd and is not
    // activated; the launch boots a clean base at the cwd.
    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "launch activates a clean base at the cwd, not the migrated worktree"
    );

    // Migration side effects: a global windows.json is written and the
    // legacy per-cwd file is renamed to `.migrated.bak`.
    let global = s.data_dir().join("orchestrator").join("windows.json");
    assert!(global.exists(), "migration writes the global windows.json");
    let encoded = fresh::workspace::encode_path_for_filename(&s.project_canon);
    let legacy = s
        .data_dir()
        .join("orchestrator")
        .join(&encoded)
        .join("windows.json");
    assert!(
        !legacy.exists(),
        "the legacy per-cwd file is consumed by migration"
    );
}

// ---------------------------------------------------------------------------
// Branch G: v0.3.6 `<project>/.fresh/windows.json` present. The current
// reader only looks under the data dir, so this layout is IGNORED — a
// 0.3.6 -> 0.3.8 upgrade does not surface these sessions at all.
// ---------------------------------------------------------------------------
#[test]
fn v036_dotfresh_is_ignored_on_upgrade() {
    let s = Scenario::new();
    s.place_v036_dotfresh("v036_dotfresh.json");
    let editor = s.bring_up();

    assert_eq!(
        editor.active_window().root,
        s.project_canon,
        "v0.3.6 .fresh/windows.json is not read by the data-dir reader; clean base boots"
    );
    assert_eq!(
        editor.session_count(),
        1,
        "no sessions are imported from the working-tree .fresh layout"
    );
    // And the stray .fresh file is left untouched in the project tree.
    assert!(s.project_canon.join(".fresh").join("windows.json").exists());
}

// ---------------------------------------------------------------------------
// Branch H: restore disabled. Even with a cwd-matching base window
// persisted, `restore_previous_session = false` must skip full restore.
// Characterizes that the active window is still picked (phase B) but the
// workspace contents (phase C) are not restored.
// ---------------------------------------------------------------------------
#[test]
fn restore_previous_session_false_still_picks_window_but_skips_workspace() {
    let s = Scenario::new();
    s.place_v2_global("v2_base_only.json");

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

    // Window pick (phase B) happens during construction regardless of
    // the restore flag; the flag only gates the phase-C workspace
    // restore that the real launch performs afterward.
    assert_eq!(editor.active_window().root, s.project_canon);
}
