//! Fixture GENERATOR (not an assertion suite).
//!
//! Produces the orchestrator bring-up fixtures by running the REAL
//! persistence save path of *this* build, instead of hand-authoring
//! JSON. Run it to (re)generate fixtures after a schema change:
//!
//!     REGEN_ORCH_FIXTURES=1 cargo test -p fresh-editor \
//!         --test orchestrator_fixture_gen -- --nocapture
//!
//! Each generator constructs an `Editor`, drives the real
//! window-creation + `save_orchestrator_state` code, reads the bytes
//! the writer emitted, replaces the run's temp paths with
//! `__PROJECT__` / `__WORKTREE__` / `__OTHER__` tokens, and writes the
//! result into `tests/fixtures/orchestrator_bringup/`. The
//! characterization suite then replays those exact bytes.
//!
//! Without `REGEN_ORCH_FIXTURES=1` the tests are no-ops so a normal
//! `cargo test` run neither rewrites committed fixtures nor fails.

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::StdFileSystem;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

const FIXTURES: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/orchestrator_bringup"
);

fn regen_enabled() -> bool {
    std::env::var("REGEN_ORCH_FIXTURES").is_ok_and(|v| !v.is_empty() && v != "0")
}

fn json_path(p: &Path) -> String {
    serde_json::to_string(p)
        .unwrap()
        .trim_matches('"')
        .to_string()
}

/// Replace concrete run paths with stable tokens, longest first so a
/// worktree nested under the project doesn't get half-substituted.
fn tokenize(body: &str, project: &Path, worktree: &Path, other: &Path) -> String {
    body.replace(&json_path(worktree), "__WORKTREE__")
        .replace(&json_path(other), "__OTHER__")
        .replace(&json_path(project), "__PROJECT__")
}

/// A run sandbox with *named* subdirs so the basenames the writer
/// bakes into window labels are stable and readable across regens
/// (a bare `TempDir` basename like `.tmpC7IIw7` would churn the
/// committed fixture every time).
struct Dirs {
    _temp: TempDir,
    project: PathBuf,
    worktree: PathBuf,
    other: PathBuf,
    data_root: PathBuf,
}

fn dirs() -> Dirs {
    let temp = TempDir::new().unwrap();
    let mk = |name: &str| {
        let p = temp.path().join(name);
        std::fs::create_dir_all(&p).unwrap();
        p.canonicalize().unwrap()
    };
    let project = mk("project");
    let worktree = mk("worktree");
    let other = mk("other-project");
    let data_root = mk("data-root");
    Dirs {
        _temp: temp,
        project,
        worktree,
        other,
        data_root,
    }
}

fn new_editor(project: &Path, data_root: &Path) -> fresh::app::Editor {
    let dir_context = DirectoryContext::for_testing(data_root);
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
        Some(project.to_path_buf()),
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

fn orch_meta(project: &Path, shared: bool) -> HashMap<String, serde_json::Value> {
    let mut m = HashMap::new();
    m.insert(
        "project_path".to_string(),
        serde_json::Value::String(project.to_string_lossy().into_owned()),
    );
    m.insert(
        "shared_worktree".to_string(),
        serde_json::Value::Bool(shared),
    );
    m
}

fn read_global_windows(data_root: &Path) -> String {
    let p = data_root
        .join("data")
        .join("orchestrator")
        .join("windows.json");
    std::fs::read_to_string(&p).unwrap_or_else(|e| panic!("read {p:?}: {e}"))
}

fn write_fixture(name: &str, body: &str) {
    let path = Path::new(FIXTURES).join(name);
    // Ensure trailing newline for clean diffs.
    let body = if body.ends_with('\n') {
        body.to_string()
    } else {
        format!("{body}\n")
    };
    std::fs::write(&path, body).unwrap_or_else(|e| panic!("write {path:?}: {e}"));
    eprintln!("regenerated {}", path.display());
}

/// v2 global: a base window at the project + one worktree orchestrator
/// session whose plugin_state carries `project_path == project`, with
/// the worktree as the persisted `active`.
#[test]
fn gen_v2_worktree_session() {
    if !regen_enabled() {
        return;
    }
    let d = dirs();
    let mut editor = new_editor(&d.project, &d.data_root);
    let id = editor.create_window_at(
        d.worktree.clone(),
        "anna-katharine-green_hand-and-ring".into(),
    );
    editor.set_active_window(id);
    editor
        .active_window_mut()
        .plugin_state
        .insert("orchestrator".to_string(), orch_meta(&d.project, false));
    editor.save_orchestrator_state();

    let raw = read_global_windows(&d.data_root);
    write_fixture(
        "v2_worktree_session.json",
        &tokenize(&raw, &d.project, &d.worktree, &d.other),
    );
}

/// v2 global: just the base window rooted at the project.
#[test]
fn gen_v2_base_only() {
    if !regen_enabled() {
        return;
    }
    let d = dirs();
    let editor = new_editor(&d.project, &d.data_root);
    editor.save_orchestrator_state();

    let raw = read_global_windows(&d.data_root);
    write_fixture(
        "v2_base_only.json",
        &tokenize(&raw, &d.project, &d.worktree, &d.other),
    );
}

/// v2 global: a single session belonging to an UNRELATED project (its
/// own base window), launched/saved from that other project so nothing
/// is rooted at our `project`.
#[test]
fn gen_v2_cross_project_only() {
    if !regen_enabled() {
        return;
    }
    let d = dirs();
    // Launch in `other` so the saved base window is rooted there.
    let mut editor = new_editor(&d.other, &d.data_root);
    editor
        .active_window_mut()
        .plugin_state
        .insert("orchestrator".to_string(), orch_meta(&d.other, false));
    editor.save_orchestrator_state();

    let raw = read_global_windows(&d.data_root);
    write_fixture(
        "v2_cross_project_only.json",
        &tokenize(&raw, &d.project, &d.worktree, &d.other),
    );
}

/// v2 global: base at project + worktree session (project_path ==
/// project), active = worktree. Same shape as `gen_v2_worktree_session`
/// but kept as a distinct fixture/label for the base-vs-worktree test.
#[test]
fn gen_v2_base_and_worktree() {
    if !regen_enabled() {
        return;
    }
    let d = dirs();
    let mut editor = new_editor(&d.project, &d.data_root);
    let id = editor.create_window_at(d.worktree.clone(), "ralestone".into());
    editor.set_active_window(id);
    editor
        .active_window_mut()
        .plugin_state
        .insert("orchestrator".to_string(), orch_meta(&d.project, false));
    editor.save_orchestrator_state();

    let raw = read_global_windows(&d.data_root);
    write_fixture(
        "v2_base_and_worktree.json",
        &tokenize(&raw, &d.project, &d.worktree, &d.other),
    );
}
