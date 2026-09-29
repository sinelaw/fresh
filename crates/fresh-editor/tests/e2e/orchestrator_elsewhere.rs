//! E2E coverage for the dock's Elsewhere group: Claude and Codex sessions
//! open outside this editor, listed by the `live_sessions` plugin.
//!
//! The sources are real CLIs, so the tests point the plugin at fake ones
//! (its `claudeCommand` / `codexCommand` settings): a `claude` whose
//! `agents --json` reports one background job and whose `attach` prints a
//! marker, and a `codex` whose `cloud list --json` reports one task and whose
//! `cloud status` prints a marker. Process scanning and the Claude cloud
//! source are off, so nothing on the machine running the tests leaks in.
//!
//! Driven through keyboard/mouse, asserted on rendered output.
#![cfg(unix)]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

const CLAUDE_JOB_TITLE: &str = "fix-flaky-test";
const CODEX_TASK_TITLE: &str = "Port the parser";

fn write_script(path: &Path, body: &str) {
    fs::write(path, body).unwrap();
    fs::set_permissions(path, fs::Permissions::from_mode(0o755)).unwrap();
}

/// A git project with the orchestrator and the Elsewhere feed installed, the
/// fake CLIs beside it, and a directory the fake Claude job "runs" in.
fn setup() -> (tempfile::TempDir, PathBuf, Config) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("homeproj");
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    copy_plugin(&plugins_dir, "live_sessions");
    fs::write(root.join("readme.txt"), "hello\n").unwrap();
    assert!(std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root)
        .status()
        .unwrap()
        .success());

    let job_dir = temp_dir.path().join("jobdir");
    fs::create_dir(&job_dir).unwrap();
    let bin = temp_dir.path().join("bin");
    fs::create_dir(&bin).unwrap();
    let claude = bin.join("fake-claude");
    write_script(
        &claude,
        &format!(
            r#"#!/bin/sh
case "$1" in
  agents)
    printf '%s\n' '[{{"pid":4242,"id":"job7","cwd":"{cwd}","kind":"background","startedAt":1,"sessionId":"5e55","name":"{title}","state":"blocked","status":"waiting"}}]'
    ;;
  attach)
    echo "ATTACHED-$2"
    exec sleep 30
    ;;
esac
"#,
            cwd = job_dir.display(),
            title = CLAUDE_JOB_TITLE,
        ),
    );
    let codex = bin.join("fake-codex");
    write_script(
        &codex,
        &format!(
            r#"#!/bin/sh
case "$1 $2" in
  "cloud list")
    printf '%s\n' '{{"tasks":[{{"id":"task_a","url":"https://example.invalid/task_a","title":"{title}","status":"pending","updated_at":"2026-01-01T00:00:00Z","environment_label":"acme/api"}}],"cursor":null}}'
    ;;
  "cloud status")
    echo "STATUS-$3"
    ;;
esac
"#,
            title = CODEX_TASK_TITLE,
        ),
    );

    let mut config = Config::default();
    config.plugins.insert(
        "live_sessions".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({
                "claudeCommand": claude.display().to_string(),
                "codexCommand": codex.display().to_string(),
                "claudeCloud": false,
                "codexLocal": false,
                // The fake task's date is fixed; keep it however old it gets.
                "cloudMaxAgeDays": 0,
            }),
        },
    );
    (temp_dir, root, config)
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

/// Screen cell (col, row) where `needle` starts, or panic with the screen.
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

/// Opening the dock lists what the fake CLIs report, under Elsewhere, with
/// the group's count and the "needs you" roll-up of the waiting job.
#[test]
fn elsewhere_group_lists_sessions_open_outside_the_editor() {
    let (_tmp, root, config) = setup();
    let mut h = EditorTestHarness::with_config_and_working_dir(140, 36, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Elsewhere") && s.contains(CLAUDE_JOB_TITLE) && s.contains(CODEX_TASK_TITLE)
    })
    .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("(2)"),
        "the group counts its rows:\n{screen}"
    );
    assert!(
        // The dock is narrow, so a row's tail may be cut; its start is not.
        screen.contains("jobdir · ") && screen.contains("acme/api · "),
        "each row says where it runs:\n{screen}"
    );
    // The workspace itself is still listed above the group.
    let ws_row = pos_of(&h, "homeproj").1;
    let group_row = pos_of(&h, "Elsewhere").1;
    assert!(ws_row < group_row, "workspaces come first:\n{screen}");
}

/// Enter on a background Claude job opens a workspace attached to it
/// (`claude attach <job>`), and the job leaves the group: it is a workspace
/// now.
#[test]
fn opening_an_elsewhere_row_attaches_it_in_a_new_workspace() {
    let (_tmp, root, config) = setup();
    let mut h = EditorTestHarness::with_config_and_working_dir(140, 36, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains(CLAUDE_JOB_TITLE))
        .unwrap();

    let (col, row) = pos_of(&h, CLAUDE_JOB_TITLE);
    h.mouse_click(col, row).unwrap();

    h.wait_until(|h| h.screen_to_string().contains("ATTACHED-job7"))
        .unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Elsewhere") && s.contains("(1)")
    })
    .unwrap();
}

/// Filing an Elsewhere row into a folder materializes it there: a Codex
/// Cloud task becomes a workspace showing its status, filed under the folder
/// (which then counts it), and the group — now empty — goes away.
#[test]
fn moving_an_elsewhere_row_into_a_folder_materializes_it() {
    let (_tmp, root, config) = setup();
    let mut h = EditorTestHarness::with_config_and_working_dir(140, 36, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // A folder to file into, made empty (its "organize" checkbox off).
    let (mcol, mrow) = pos_of(&h, "Menu ▾");
    h.mouse_click(mcol, mrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New folder…"))
        .unwrap();
    let (fcol, frow) = pos_of(&h, "New folder…");
    h.mouse_click(fcol, frow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Folder name"))
        .unwrap();
    h.type_text("Cloud").unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("Folder name") && s.contains("Cloud") && s.contains(CODEX_TASK_TITLE)
    })
    .unwrap();

    // Right-click the task, Move to Folder…, pick "Cloud".
    let (tcol, trow) = pos_of(&h, CODEX_TASK_TITLE);
    h.mouse_right_click(tcol, trow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Move to Folder"))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("Open in Browser"),
        "a row with a page offers it:\n{screen}"
    );
    let (mcol, mrow) = pos_of(&h, "Move to Folder");
    h.mouse_click(mcol, mrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Top level"))
        .unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // The task is a workspace now: its terminal ran `codex cloud status`,
    // and it is filed under Cloud.
    h.wait_until(|h| h.screen_to_string().contains("STATUS-task_a"))
        .unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Cloud") && s.contains("(1)")
    })
    .unwrap();
}

/// A click on a row that opens outside the editor (a Codex Cloud task's page)
/// opens the row's menu instead, and at the pointer: the menu's box starts
/// where the click landed, not at a column the plugin guessed.
#[test]
fn clicking_a_row_opens_its_menu_at_the_pointer() {
    let (_tmp, root, config) = setup();
    let mut h = EditorTestHarness::with_config_and_working_dir(140, 36, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains(CODEX_TASK_TITLE))
        .unwrap();

    let (col, row) = pos_of(&h, CODEX_TASK_TITLE);
    // Well into the title, so a menu at the row's start would be visibly off.
    let click = (col + 8, row);
    h.mouse_click(click.0, click.1).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Open in Browser"))
        .unwrap();

    let screen = h.screen_to_string();
    let (item_col, item_row) = pos_of(&h, "Open in Browser");
    assert!(
        item_row > click.1 && item_row <= click.1 + 3,
        "the menu opens just below the click (row {}), not elsewhere:\n{screen}",
        click.1
    );
    assert!(
        item_col >= click.0 && item_col <= click.0 + 4,
        "the menu opens at the click's column ({}), not the row's start:\n{screen}",
        click.0
    );
    assert!(
        !screen.contains("STATUS-task_a"),
        "a click does not open the task itself:\n{screen}"
    );
}
