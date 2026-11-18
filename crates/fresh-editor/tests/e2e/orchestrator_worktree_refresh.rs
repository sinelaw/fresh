//! The dock's on-disk worktree rows follow git while the dock stays open
//! (discussion #3621: "the Orchestrator panel often keeps showing old
//! worktrees long after they are gone").
//!
//! Discovery used to run only when the dock opened, so a dock left open
//! kept listing removed worktrees and never showed new ones; and a worktree
//! deleted with `rm -rf` stayed listed even across reopens, because git keeps
//! reporting it (as `prunable`) until `git worktree prune`. These drive the
//! real plugin and assert on the rendered dock.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use std::fs;
use std::path::{Path, PathBuf};

fn git(dir: &Path, args: &[&str]) {
    let ok = std::process::Command::new("git")
        .args(["-c", "user.email=t@t", "-c", "user.name=t"])
        .args(args)
        .current_dir(dir)
        .status()
        .unwrap()
        .success();
    assert!(ok, "git {args:?} failed");
}

/// A committed git project with the orchestrator plugin installed. Linked
/// worktrees go next to it, as `<tmp>/wt-<name>` on branch `<name>-wt`.
fn setup_project() -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("proj");
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    fs::write(root.join("readme.txt"), "hello\n").unwrap();
    git(&root, &["init", "-q"]);
    git(&root, &["add", "."]);
    git(&root, &["commit", "-qm", "init"]);
    (temp_dir, root)
}

fn add_worktree(root: &Path, name: &str) {
    git(
        root,
        &[
            "worktree",
            "add",
            "-q",
            &format!("../wt-{name}"),
            "-b",
            &format!("{name}-wt"),
        ],
    );
}

/// The dock opens with "all worktrees" on, so discovered rows are listed,
/// re-scanning every `refresh_secs` (0 = never).
fn config(refresh_secs: u64) -> Config {
    let mut config = Config::default();
    config.plugins.insert(
        "orchestrator".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({
                "showAllWorktrees": true,
                "worktree_refresh_seconds": refresh_secs,
            }),
        },
    );
    config
}

fn open_dock(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("+ New") && h.editor().is_dock_focused())
        .unwrap();
}

fn pos_of(h: &EditorTestHarness, needle: &str) -> (u16, u16) {
    let screen = h.screen_to_string();
    screen
        .lines()
        .enumerate()
        .find_map(|(r, l)| {
            l.find(needle)
                .map(|b| (l[..b].chars().count() as u16, r as u16))
        })
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{screen}"))
}

fn shows(h: &EditorTestHarness, needle: &str) -> bool {
    h.screen_to_string().contains(needle)
}

/// With the dock left open, a worktree removed outside the editor drops out
/// of the list and a new one appears — no toggling the dock.
#[test]
fn open_dock_rescans_worktrees_periodically() {
    let (_tmp, root) = setup_project();
    add_worktree(&root, "alpha");
    add_worktree(&root, "beta");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, config(1), root.clone()).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| shows(h, "alpha-wt") && shows(h, "beta-wt"))
        .unwrap();

    git(&root, &["worktree", "remove", "../wt-beta"]);
    add_worktree(&root, "gamma");

    h.wait_until(|h| shows(h, "gamma-wt") && !shows(h, "beta-wt"))
        .unwrap();
    assert!(shows(&h, "alpha-wt"), "{}", h.screen_to_string());
}

/// Refresh in a row's right-click menu re-scans the worktrees (it used to
/// exist only on the external sessions group, and re-listed only those).
/// A worktree whose directory was deleted without `git worktree remove` —
/// which git still lists, as prunable — goes too.
#[test]
fn context_menu_refresh_rescans_worktrees_and_drops_prunable() {
    let (_tmp, root) = setup_project();
    add_worktree(&root, "alpha");
    add_worktree(&root, "delta");
    // Periodic re-scan off: only the menu can bring the list up to date.
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, config(0), root.clone()).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| shows(h, "alpha-wt") && shows(h, "delta-wt"))
        .unwrap();

    fs::remove_dir_all(root.parent().unwrap().join("wt-delta")).unwrap();
    add_worktree(&root, "gamma");

    let (col, row) = pos_of(&h, "alpha-wt");
    h.mouse_right_click(col + 2, row).unwrap();
    h.wait_until(|h| shows(h, "Refresh")).unwrap();
    let (col, row) = pos_of(&h, "Refresh");
    h.mouse_click(col + 1, row).unwrap();

    h.wait_until(|h| shows(h, "gamma-wt") && !shows(h, "delta-wt"))
        .unwrap();
    assert!(shows(&h, "alpha-wt"), "{}", h.screen_to_string());
}
