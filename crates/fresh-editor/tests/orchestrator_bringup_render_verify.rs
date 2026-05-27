//! Specification tests for the rendered bring-up UI (issue #2056).
//!
//! These pin the DESIRED end-to-end behavior — what the user should see
//! after a faithful bring-up (construct + restore + file-explorer init):
//! the active window, `working_dir`, the file-explorer root, and the
//! window title must all agree on the launch project, and the
//! file-explorer must follow the ACTIVE WINDOW (defect #3), re-rooting
//! when the user dives into another window.
//!
//! These pass with the #2056 fix in place. Plugins are OFF to keep the
//! core path isolated.

mod common;

use common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh_core::WindowId;
use std::path::{Path, PathBuf};

fn json_path(p: &Path) -> String {
    serde_json::to_string(p)
        .unwrap()
        .trim_matches('"')
        .to_string()
}

/// Build a harness in `project` with the worktree-hijack fixture
/// planted, run phase-C restore. Returns (harness, project, worktree).
fn hijack_harness() -> (EditorTestHarness, PathBuf, PathBuf, tempfile::TempDir) {
    fresh::i18n::set_locale("en");
    let sandbox = tempfile::tempdir().unwrap();
    let mk = |n: &str| {
        let p = sandbox.path().join(n);
        std::fs::create_dir_all(&p).unwrap();
        p.canonicalize().unwrap()
    };
    let data_home = mk("data-home");
    let project = mk("project");
    let worktree = mk("worktree");
    std::fs::write(project.join("PROJECT_FILE.md"), "p").unwrap();
    std::fs::write(worktree.join("WORKTREE_FILE.md"), "w").unwrap();

    let dir_context = DirectoryContext::for_testing(&data_home);
    let config = Config {
        check_for_updates: false,
        ..Config::default()
    };
    let mut h = EditorTestHarness::create(
        100,
        40,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .with_shared_dir_context(dir_context)
            .with_config(config)
            .with_empty_plugins_dir(),
    )
    .unwrap();
    h.startup(true, &[]).unwrap();
    // Launch foregrounds the project (the cwd). Open a second window
    // rooted at the worktree so the diving test has two windows to
    // switch between — the runtime equivalent of the old windows.json
    // worktree-session fixture (now that sessions are discovered from
    // the per-dir workspace cache rather than a central registry).
    h.editor_mut()
        .create_window_at(worktree.clone(), "worktree".to_string());
    (h, project, worktree, sandbox)
}

fn pump_explorer_root(h: &mut EditorTestHarness) -> Option<PathBuf> {
    h.editor_mut().show_file_explorer();
    for _ in 0..50 {
        h.render().unwrap();
        h.editor_mut().process_async_messages();
        if let Some(v) = h.editor().file_explorer() {
            return Some(v.tree().root_path().to_path_buf());
        }
    }
    None
}

#[test]
fn launch_in_project_roots_rendered_ui_at_project() {
    fresh::i18n::set_locale("en");

    // Sandbox: data dir (holds windows.json), project (cwd), worktree.
    let sandbox = tempfile::tempdir().unwrap();
    let mk = |n: &str| {
        let p = sandbox.path().join(n);
        std::fs::create_dir_all(&p).unwrap();
        p.canonicalize().unwrap()
    };
    let data_home = mk("data-home");
    let project = mk("project");
    let worktree = mk("worktree");
    // Put a recognizable file in each so a rendered tree is distinguishable.
    std::fs::write(project.join("PROJECT_FILE.md"), "p").unwrap();
    std::fs::write(worktree.join("WORKTREE_FILE.md"), "w").unwrap();

    // Plant the real-captured worktree-hijack fixture at the v2 global
    // location, with this run's paths substituted in.
    let dir_context = DirectoryContext::for_testing(&data_home);
    let orch = dir_context.data_dir.join("orchestrator");
    std::fs::create_dir_all(&orch).unwrap();
    let fixture = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/orchestrator_bringup/v2_worktree_session.json"
    ))
    .unwrap()
    .replace("__PROJECT__", &json_path(&project))
    .replace("__WORKTREE__", &json_path(&worktree));
    std::fs::write(orch.join("windows.json"), fixture).unwrap();

    // Build the editor in the project dir, sharing the planted data dir.
    let config = Config {
        check_for_updates: false,
        ..Config::default()
    };
    let mut h = EditorTestHarness::create(
        100,
        40,
        common::harness::HarnessOptions::new()
            .with_working_dir(project.clone())
            .with_shared_dir_context(dir_context)
            .with_config(config)
            .with_empty_plugins_dir(),
    )
    .unwrap();

    // Phase C: restore (mirrors handle_first_run_setup), plus the
    // inactive-window restore main.rs runs right after.
    h.startup(true, &[]).unwrap();
    h.editor_mut().materialize_all_windows();

    // Phase D: open the file explorer and pump async until its tree
    // initializes (init_file_explorer spawns a tokio task).
    h.editor_mut().show_file_explorer();
    let mut explorer_root: Option<PathBuf> = None;
    for _ in 0..50 {
        h.render().unwrap();
        h.editor_mut().process_async_messages();
        if let Some(v) = h.editor().file_explorer() {
            explorer_root = Some(v.tree().root_path().to_path_buf());
            break;
        }
    }

    let working_dir = h.editor().working_dir().to_path_buf();
    let active_root = h.editor().active_window().root.clone();
    let title_project = working_dir
        .file_name()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string());

    eprintln!("=== OBSERVED (worktree-hijack, plugins off) ===");
    eprintln!("project            = {}", project.display());
    eprintln!("worktree           = {}", worktree.display());
    eprintln!("active_window.root = {}", active_root.display());
    eprintln!("editor.working_dir = {}", working_dir.display());
    eprintln!("file_explorer root = {:?}", explorer_root);
    eprintln!("title project name = {:?}", title_project);
    let screen = h.screen_to_string();
    eprintln!(
        "explorer shows PROJECT_FILE.md = {}",
        screen.contains("PROJECT_FILE")
    );
    eprintln!(
        "explorer shows WORKTREE_FILE.md = {}",
        screen.contains("WORKTREE_FILE")
    );

    // SPEC: launching `fresh <project>` activates the project-rooted
    // window, not the worktree session — and working_dir, the
    // file-explorer root, and the title all agree on the project.
    let explorer_root = explorer_root.expect("file explorer should initialize");
    assert_eq!(
        active_root, project,
        "the active window is the project-rooted one, not the worktree session"
    );
    assert_eq!(
        working_dir, project,
        "working_dir matches the active window's root (the project)"
    );
    assert_eq!(
        explorer_root, project,
        "the file-explorer roots at the active window's root (the project)"
    );
    assert_eq!(
        title_project.as_deref(),
        Some("project"),
        "the window title's project name is the project"
    );
}

/// Find the window id whose root equals `root` (scans the small
/// monotonic id space).
fn window_id_with_root(h: &EditorTestHarness, root: &Path) -> WindowId {
    for id in 1..=64u64 {
        if let Some(w) = h.editor().session(WindowId(id)) {
            if w.root == root {
                return WindowId(id);
            }
        }
    }
    panic!("no window rooted at {}", root.display());
}

/// SPEC: launching in the project gives a consistent project-rooted UI;
/// deliberately diving into the worktree window (an inactive shell)
/// re-roots `working_dir`, the title, AND the file explorer at that
/// window — and diving back restores the project. Pins the invariant
/// `file_explorer root == active_window().root` (defect #3) across
/// switches, instead of the file explorer being keyed off a global
/// `working_dir` and sticking to its first-init root.
#[test]
fn diving_between_windows_roots_the_ui_at_the_active_window() {
    let (mut h, project, worktree, _sandbox) = hijack_harness();

    // At launch the project window is active and everything is
    // consistent (the worktree session is an inactive shell).
    assert_eq!(
        h.editor().active_window().root,
        project,
        "the project window is active at launch"
    );
    assert_eq!(h.editor().working_dir(), project.as_path());
    assert_eq!(
        pump_explorer_root(&mut h),
        Some(project.clone()),
        "explorer roots at the active (project) window"
    );

    // Dive into the worktree window (the shell).
    let wt_id = window_id_with_root(&h, &worktree);
    h.editor_mut().set_active_window(wt_id);
    assert_eq!(
        h.editor().working_dir(),
        worktree.as_path(),
        "diving into the worktree window points working_dir at the worktree"
    );
    assert_eq!(
        h.editor()
            .working_dir()
            .file_name()
            .and_then(|s| s.to_str()),
        Some("worktree"),
        "the title's project name follows the dive"
    );
    assert_eq!(
        pump_explorer_root(&mut h),
        Some(worktree.clone()),
        "the file explorer follows the active window to the worktree"
    );

    // Dive back to the project window: the explorer must NOT be stuck on
    // the worktree — it re-roots at the project.
    let proj_id = window_id_with_root(&h, &project);
    h.editor_mut().set_active_window(proj_id);
    assert_eq!(h.editor().working_dir(), project.as_path());
    assert_eq!(
        pump_explorer_root(&mut h),
        Some(project.clone()),
        "diving back re-roots the explorer at the project (not sticky)"
    );
}

/// Gap 2 + 3: faithful per-window workspace restore.
///
/// The original issue-#2056 *body* symptom was foreign tabs/buffers
/// (incl. `external_files` — files opened outside the project) showing
/// up in a window. After the fix the active window is rooted at the
/// launch cwd, and its per-path workspace (keyed by that root) restores
/// into THAT window. This test:
///   1. opens an in-project file and an OUT-of-project (external) file,
///      saves the project's workspace,
///   2. plants a worktree-hijack windows.json,
///   3. relaunches in the project and asserts the workspace was restored
///      into the project-rooted active window (its own buffers), not the
///      worktree session.
///
/// Discriminating: pre-fix the worktree session was the active window,
/// so the restore landed there and `active_window().root` was the
/// worktree — the `root == project` assertion fails.
#[test]
fn launch_restores_the_projects_own_workspace_into_the_project_window() {
    fresh::i18n::set_locale("en");
    let sandbox = tempfile::tempdir().unwrap();
    let mk = |n: &str| {
        let p = sandbox.path().join(n);
        std::fs::create_dir_all(&p).unwrap();
        p.canonicalize().unwrap()
    };
    let data_home = mk("data-home");
    let project = mk("project");
    let worktree = mk("worktree");
    let external = mk("external-dir");

    let in_project = project.join("in_project.txt");
    std::fs::write(&in_project, "a").unwrap();
    let out_of_project = external.join("foreign.txt");
    std::fs::write(&out_of_project, "b").unwrap();

    let dir_context = DirectoryContext::for_testing(&data_home);
    let config = Config {
        check_for_updates: false,
        ..Config::default()
    };

    // (1) First launch: open both files, save the project's workspace.
    {
        let mut h = EditorTestHarness::create(
            100,
            40,
            HarnessOptions::new()
                .with_working_dir(project.clone())
                .with_shared_dir_context(dir_context.clone())
                .with_config(config.clone())
                .with_empty_plugins_dir(),
        )
        .unwrap();
        h.open_file(&in_project).unwrap();
        h.open_file(&out_of_project).unwrap();
        h.editor_mut().save_workspace().unwrap();
        // Drop without saving orchestrator state, so the windows.json we
        // plant next is what the relaunch reads.
    }

    // (2) Plant the worktree-hijack windows.json.
    let orch = dir_context.data_dir.join("orchestrator");
    std::fs::create_dir_all(&orch).unwrap();
    let fixture = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/orchestrator_bringup/v2_worktree_session.json"
    ))
    .unwrap()
    .replace("__PROJECT__", &json_path(&project))
    .replace("__WORKTREE__", &json_path(&worktree));
    std::fs::write(orch.join("windows.json"), fixture).unwrap();

    // (3) Relaunch in the project and restore.
    let mut h = EditorTestHarness::create(
        100,
        40,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .with_shared_dir_context(dir_context)
            .with_config(config)
            .with_empty_plugins_dir(),
    )
    .unwrap();
    h.startup(true, &[]).unwrap();

    // The restore landed in the project-rooted active window, not the
    // worktree session.
    assert_eq!(
        h.editor().active_window().root,
        project,
        "the project's workspace restores into the project-rooted active window"
    );
    let paths = h.editor().active_window().buffers.paths();
    assert!(
        paths.contains(&in_project),
        "the in-project file is restored into the active window; got {paths:?}"
    );
    assert!(
        paths.contains(&out_of_project),
        "the external file is restored into the active window (faithful), not dropped; got {paths:?}"
    );
}
