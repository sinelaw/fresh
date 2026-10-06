//! E2E coverage for the dock's external sessions groups: Claude and Codex sessions
//! open outside this editor, listed by the `live_sessions` plugin under a
//! group per product ("Claude", "Codex").
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

/// A git project with the orchestrator and the external sessions feed installed, the
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

/// Opening the dock lists what the fake CLIs report, a group per product
/// ("Claude", "Codex"), each with its count.
#[test]
fn external_group_lists_sessions_open_outside_the_editor() {
    let (_tmp, root, config) = setup();
    let mut h = EditorTestHarness::with_config_and_working_dir(140, 36, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("▼ Claude") && s.contains(CLAUDE_JOB_TITLE) && s.contains(CODEX_TASK_TITLE)
    })
    .unwrap();
    let screen = h.screen_to_string();
    let claude_group = pos_of(&h, "▼ Claude").1;
    let codex_group = pos_of(&h, "▼ Codex").1;
    assert!(
        screen
            .lines()
            .nth(claude_group as usize)
            .unwrap()
            .contains("(1)")
            && screen
                .lines()
                .nth(codex_group as usize)
                .unwrap()
                .contains("(1)"),
        "each product has its group, counting its rows:\n{screen}"
    );
    assert!(
        claude_group < pos_of(&h, CLAUDE_JOB_TITLE).1
            && codex_group < pos_of(&h, CODEX_TASK_TITLE).1
            && pos_of(&h, CLAUDE_JOB_TITLE).1 < codex_group,
        "each session sits under its product:\n{screen}"
    );
    // A row is its state and title; what it is and where it runs are the
    // first lines of its menu, read-only.
    let (col, row) = pos_of(&h, CLAUDE_JOB_TITLE);
    h.mouse_right_click(col, row).unwrap();
    h.wait_until(|h| {
        h.screen_to_string()
            .contains("Source: Claude background job")
    })
    .unwrap();
    let menu = h.screen_to_string();
    assert!(
        menu.contains("Where: ") && menu.contains("jobdir") && menu.contains("State: needs you"),
        "the menu says where it runs and its state:\n{menu}"
    );
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Source: "))
        .unwrap();
    // The workspace itself is still listed above the group.
    let ws_row = pos_of(&h, "homeproj").1;
    let group_row = claude_group;
    assert!(ws_row < group_row, "workspaces come first:\n{screen}");
}

/// Enter on a background Claude job opens a workspace attached to it
/// (`claude attach <job>`), and the job leaves the group: it is a workspace
/// now.
#[test]
fn opening_an_external_row_attaches_it_in_a_new_workspace() {
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
    // The Claude group, now empty, goes away; Codex's stays.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("▼ Claude") && s.contains("▼ Codex")
    })
    .unwrap();
}

/// Filing an external session row into a folder moves the row there and
/// nothing else: the Codex Cloud task is not forked or opened (its status
/// never runs), it just shows under the folder (which counts it), and its
/// product group — now empty — goes away.
#[test]
fn moving_an_external_row_into_a_folder_files_it_without_forking() {
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
    // The dock asks for the target; a click on the "Cloud" folder files it.
    h.wait_until(|h| h.screen_to_string().contains("click a folder"))
        .unwrap();
    let (fcol, frow) = pos_of(&h, "▼ Cloud");
    h.mouse_click(fcol + 2, frow).unwrap();

    // The row is under Cloud now, and the Codex group (empty) is gone.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("click a folder") && !s.contains("▼ Codex") && s.contains(CODEX_TASK_TITLE)
    })
    .unwrap();
    let screen = h.screen_to_string();
    let folder = pos_of(&h, "▼ Cloud").1;
    assert!(
        screen.lines().nth(folder as usize).unwrap().contains("(1)")
            && pos_of(&h, CODEX_TASK_TITLE).1 == folder + 1,
        "the task sits under Cloud, which counts it:\n{screen}"
    );
    // Filing forked nothing: no workspace ran the task's status.
    for _ in 0..10 {
        h.render().unwrap();
    }
    assert!(
        !h.screen_to_string().contains("STATUS-task_a"),
        "filing does not open the task:\n{}",
        h.screen_to_string()
    );
}

/// **Dragging an external row onto a folder files it**, as Move to Folder
/// does: the row moves under the folder, nothing is opened, and dragged back
/// onto its product's group it returns there.
#[test]
fn dragging_an_external_row_onto_a_folder_files_it() {
    let (_tmp, root, config) = setup();
    let mut h = EditorTestHarness::with_config_and_working_dir(140, 36, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);

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

    let (tcol, trow) = pos_of(&h, CODEX_TASK_TITLE);
    let (_, frow) = pos_of(&h, "▼ Cloud");
    h.mouse_drag(tcol, trow, tcol, frow).unwrap();

    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("▼ Codex") && s.contains(CODEX_TASK_TITLE)
    })
    .unwrap();
    let screen = h.screen_to_string();
    let folder = pos_of(&h, "▼ Cloud").1;
    assert!(
        screen.lines().nth(folder as usize).unwrap().contains("(1)")
            && pos_of(&h, CODEX_TASK_TITLE).1 == folder + 1,
        "the task sits under Cloud, which counts it:\n{screen}"
    );
    // A drag is not a click: no menu opened, and nothing was forked.
    for _ in 0..10 {
        h.render().unwrap();
    }
    let screen = h.screen_to_string();
    assert!(
        !screen.contains("Open in Browser") && !screen.contains("STATUS-task_a"),
        "dragging the row neither opens its menu nor the task:\n{screen}"
    );

    // Back onto the Claude group's header: an external row goes home to its
    // own product's group, whichever group it is dropped on. While it is held
    // there, that home — the Codex group, empty and so not shown until now —
    // is on screen and lit as where it would land; the Claude group is not.
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
    let at = |kind, col, row| MouseEvent {
        kind,
        column: col,
        row,
        modifiers: crossterm::event::KeyModifiers::empty(),
    };
    let (tcol, trow) = pos_of(&h, CODEX_TASK_TITLE);
    let (_, grow) = pos_of(&h, "▼ Claude");
    let far = 25;
    let plain_ground = h.get_cell_style(far, grow).unwrap().bg;
    h.send_mouse(at(MouseEventKind::Down(MouseButton::Left), tcol, trow))
        .unwrap();
    let step = if grow > trow { 1i32 } else { -1 };
    let mut r = trow as i32;
    while r != grow as i32 {
        r += step;
        h.send_mouse(at(MouseEventKind::Drag(MouseButton::Left), tcol, r as u16))
            .unwrap();
    }
    let lit = |h: &EditorTestHarness, needle: &str| {
        let s = h.screen_to_string();
        s.lines()
            .position(|l| l.contains(needle))
            .is_some_and(|row| h.get_cell_style(far, row as u16).unwrap().bg != plain_ground)
    };
    h.wait_until(|h| lit(h, "▼ Codex") || lit(h, "Codex"))
        .unwrap();
    let screen = h.screen_to_string();
    let (_, crow) = pos_of(&h, "Codex");
    let band = h.get_cell_style(far, crow).unwrap().bg;
    let (_, grow) = pos_of(&h, "Claude");
    // Under the pointer the Claude group wears the hover band; what it must
    // not wear is the drop target's.
    assert_ne!(
        h.get_cell_style(far, grow).unwrap().bg,
        band,
        "the group under the pointer is not where it would land:\n{screen}"
    );
    let (_, grow) = pos_of(&h, "Claude");
    h.send_mouse(at(MouseEventKind::Up(MouseButton::Left), tcol, grow))
        .unwrap();
    h.wait_until(|h| h.screen_to_string().contains("▼ Codex"))
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
    // The menu's box starts one row below the click, at its column, so the
    // clicked row stays in sight above it.
    let top = screen.lines().nth(click.1 as usize + 1).unwrap_or("");
    assert_eq!(
        top.chars().nth(click.0 as usize),
        Some('┌'),
        "the menu's corner sits just below the click ({}, {}):\n{screen}",
        click.0,
        click.1
    );
    let clicked_line = screen.lines().nth(click.1 as usize).unwrap();
    assert!(
        clicked_line.contains(CODEX_TASK_TITLE),
        "the clicked row stays in sight above its menu:\n{screen}"
    );
    // Read-only lines say what the row is.
    assert!(
        screen.contains("Source: Codex Cloud") && screen.contains("Where: acme/api"),
        "the menu says what the row is:\n{screen}"
    );
    assert!(
        !screen.contains("STATUS-task_a"),
        "a click does not open the task itself:\n{screen}"
    );
}

/// Teleporting a Claude cloud session makes a local copy, named after the
/// session, and the cloud session's row stays (teleport copies, it does not
/// move) but says where the copy went and offers to go there first.
///
/// The cloud list is private API, so a stand-in feed plugin (exporting the
/// same `live-sessions` API the real one does) reports one cloud session, and
/// a fake `claude` answers `--teleport` with a marker.
const TELEPORT_TITLE: &str = "Fix the auth bug";

/// A git project, a stand-in feed reporting one Claude cloud session titled
/// [`TELEPORT_TITLE`], and a fake `claude` that answers `--teleport` by
/// printing `TELEPORTED-<id>`; the dock open with the session's row showing.
fn teleport_setup() -> (tempfile::TempDir, PathBuf, EditorTestHarness) {
    const TITLE: &str = TELEPORT_TITLE;
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("homeproj");
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    fs::write(root.join("readme.txt"), "hello\n").unwrap();
    let git = |args: &[&str]| {
        assert!(std::process::Command::new("git")
            .args(args)
            .current_dir(&root)
            .status()
            .unwrap()
            .success());
    };
    git(&["init", "-q"]);
    git(&["add", "readme.txt"]);
    git(&[
        "-c",
        "user.email=t@t",
        "-c",
        "user.name=t",
        "commit",
        "-qm",
        "init",
    ]);

    let claude = temp_dir.path().join("fake-claude");
    write_script(
        &claude,
        "#!/bin/sh\n[ \"$1\" = --teleport ] && echo \"TELEPORTED-$2\"\nexec sleep 30\n",
    );
    let session = serde_json::json!({
        "key": "claude-cloud/session_x",
        "source": "claude-cloud",
        "id": "session_x",
        "agent": "claude",
        "where": "cloud",
        "title": TITLE,
        "state": "idle",
        "repo": "acme/homeproj",
        "url": "https://example.invalid/session_x",
    });
    let snapshot = serde_json::json!({
        "sessions": [session],
        "problems": [],
        "commands": { "claude": claude.display().to_string() },
    });
    fs::write(
        plugins_dir.join("fake_feed.ts"),
        format!(
            "const editor = getEditor();\n\
             const snap = {snapshot};\n\
             editor.exportPluginApi(\"live-sessions\", {{\n\
               refresh: async () => {{ editor.getPluginApi(\"orchestrator\")?.setExternalSessions(snap); }},\n\
               snapshot: () => snap,\n\
               claudeCloudEnabled: () => true,\n\
               setClaudeCloud: async () => {{}},\n\
             }});\n"
        ),
    )
    .unwrap();

    let mut h =
        EditorTestHarness::with_config_and_working_dir(160, 40, Config::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains(TITLE))
        .unwrap();

    (temp_dir, root, h)
}

/// Click the cloud session's row under the Claude group and choose
/// "Fork Here (Teleport)…"; the New Workspace form opens proposing its name.
fn start_teleport(h: &mut EditorTestHarness) {
    let group_row = pos_of(h, "▼ Claude").1;
    let row = h
        .screen_to_string()
        .lines()
        .enumerate()
        .skip(group_row as usize + 1)
        .find(|(_, l)| l.contains(TELEPORT_TITLE))
        .map(|(r, _)| r as u16)
        .expect("the cloud row");
    let col = pos_of(h, "▼ Claude").0 + 4;
    h.mouse_click(col, row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Fork Here (Teleport)"))
        .unwrap();
    let (tcol, trow) = pos_of(h, "Fork Here (Teleport)");
    h.mouse_click(tcol, trow).unwrap();
    h.wait_until(|h| {
        h.screen_to_string()
            .contains("new worktree fix-the-auth-bug")
    })
    .unwrap();
}

/// The worktree folders the project's repository has, by name.
fn worktree_names(root: &Path) -> Vec<String> {
    let out = std::process::Command::new("git")
        .args(["worktree", "list", "--porcelain"])
        .current_dir(root)
        .output()
        .unwrap();
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("worktree "))
        .filter_map(|p| {
            Path::new(p)
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
        })
        .collect()
}

#[test]
fn teleporting_a_cloud_session_names_the_copy_and_marks_the_row() {
    const TITLE: &str = TELEPORT_TITLE;
    let (_tmp, _root, mut h) = teleport_setup();
    // A click on the cloud row offers the teleport (it never runs on a click).
    let (col, row) = pos_of(&h, TITLE);
    h.mouse_click(col, row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Fork Here (Teleport)"))
        .unwrap();
    let (tcol, trow) = pos_of(&h, "Fork Here (Teleport)");
    h.mouse_click(tcol, trow).unwrap();

    // The form carries the session's name, as a branch-safe worktree name.
    h.wait_until(|h| {
        h.screen_to_string()
            .contains("new worktree fix-the-auth-bug")
    })
    .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();

    h.wait_until(|h| h.screen_to_string().contains("TELEPORTED-session_x"))
        .unwrap();
    // The copy is a workspace named as the session; the cloud row stays and
    // says where the copy went.
    h.wait_until(|h| h.screen_to_string().contains("· Fix the auth bug"))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        !screen.contains("homeproj-1"),
        "no generated name for the copy:\n{screen}"
    );

    // The cloud row (the last "Fix the auth bug" on screen, under Claude) is
    // still there; its menu says where the copy went and offers it first.
    let group_row = pos_of(&h, "▼ Claude").1;
    let row = h
        .screen_to_string()
        .lines()
        .enumerate()
        .skip(group_row as usize + 1)
        .find(|(_, l)| l.contains(TITLE))
        .map(|(r, _)| r as u16)
        .expect("the cloud row stays");
    let col = pos_of(&h, "▼ Claude").0 + 4;
    h.mouse_click(col, row).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Go to Fix the auth bug") && s.contains("teleported → Fix the auth bug")
    })
    .unwrap();
}

/// **A second copy of a cloud session gets a worktree of its own.** The form
/// proposes the session's name, and a worktree by that name already exists —
/// the first copy's. Opening the second copy in it would put two workspaces
/// in one directory; it takes the next free name instead.
#[test]
fn teleporting_the_same_session_twice_gives_each_copy_its_own_worktree() {
    let (_tmp, root, mut h) = teleport_setup();
    start_teleport(&mut h);
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|_| {
        worktree_names(&root)
            .iter()
            .any(|n| n == "fix-the-auth-bug")
    })
    .unwrap();
    h.wait_until(|h| h.screen_to_string().contains("· Fix the auth bug"))
        .unwrap();

    start_teleport(&mut h);
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|_| {
        worktree_names(&root)
            .iter()
            .any(|n| n == "fix-the-auth-bug-2")
    })
    .unwrap();
    let names = worktree_names(&root);
    assert_eq!(
        names
            .iter()
            .filter(|n| n.starts_with("fix-the-auth-bug"))
            .count(),
        2,
        "one worktree per copy: {names:?}"
    );
}

/// **A name typed over the proposed one is the user's.** The form proposes
/// the session's name for the worktree and branch, and names the workspace
/// after the session — unless the user wrote a name of their own, which then
/// names both.
#[test]
fn a_name_typed_over_the_proposed_one_names_the_copy() {
    let (_tmp, root, mut h) = teleport_setup();
    start_teleport(&mut h);
    let (dcol, drow) = pos_of(&h, "Details");
    h.mouse_click(dcol, drow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New branch:"))
        .unwrap();
    // Into the name field, past its end, and write over it.
    let (fcol, frow) = pos_of(&h, "[fix-the-auth-bug");
    h.mouse_click(fcol + 1 + "fix-the-auth-bug".len() as u16, frow)
        .unwrap();
    for _ in 0.."fix-the-auth-bug".len() {
        h.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();
    }
    h.type_text("auth-work").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[auth-work"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();

    h.wait_until(|_| worktree_names(&root).iter().any(|n| n == "auth-work"))
        .unwrap();
    h.wait_until(|h| h.screen_to_string().contains("· auth-work"))
        .unwrap();
    // The workspaces are the rows above the Claude group, whose own row
    // for the cloud session keeps the session's title.
    let screen = h.screen_to_string();
    let workspaces: Vec<&str> = screen
        .lines()
        .take_while(|l| !l.contains("▼ Claude"))
        .collect();
    assert!(
        !workspaces.iter().any(|l| l.contains("Fix the auth bug")),
        "the typed name, not the session's, names the workspace:\n{screen}"
    );
}

/// Make an empty dock folder through the Menu (its "organize" box off).
fn create_empty_folder(h: &mut EditorTestHarness, name: &str) {
    let (mcol, mrow) = pos_of(h, "Menu ▾");
    h.mouse_click(mcol, mrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New folder…"))
        .unwrap();
    let (fcol, frow) = pos_of(h, "New folder…");
    h.mouse_click(fcol, frow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Folder name"))
        .unwrap();
    h.type_text(name).unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("Folder name") && s.contains(name)
    })
    .unwrap();
}

/// A mouse event at a cell.
fn mouse_at(
    kind: crossterm::event::MouseEventKind,
    col: u16,
    row: u16,
) -> crossterm::event::MouseEvent {
    crossterm::event::MouseEvent {
        kind,
        column: col,
        row,
        modifiers: KeyModifiers::empty(),
    }
}

/// **A folder's roll-up counts the external sessions filed in it**, so a
/// collapsed folder can't hide one that needs you: a blocked background job
/// dragged into a folder shows as the folder's `●1` once it is folded.
#[test]
fn a_folder_rolls_up_the_external_sessions_filed_in_it() {
    let (_tmp, root, config) = setup();
    let mut h = EditorTestHarness::with_config_and_working_dir(140, 36, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains(CLAUDE_JOB_TITLE))
        .unwrap();
    create_empty_folder(&mut h, "Jobs");

    let (jcol, jrow) = pos_of(&h, CLAUDE_JOB_TITLE);
    let (_, frow) = pos_of(&h, "▼ Jobs");
    h.mouse_drag(jcol, jrow, jcol, frow).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.lines().any(|l| l.contains("▼ Jobs") && l.contains("(1)"))
    })
    .unwrap();

    // Folded, the folder still says one of its sessions needs you.
    let (fcol, frow) = pos_of(&h, "▼ Jobs");
    h.mouse_click(fcol, frow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("▶ Jobs"))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen
            .lines()
            .any(|l| l.contains("▶ Jobs") && l.contains("●1")),
        "the folded folder rolls up its blocked external session:\n{screen}"
    );
}

/// **A drag whose release was lost stops being drawn.** A release that never
/// arrives (let go outside the terminal) leaves the drag to the next press;
/// a press on another row that can be dragged starts a drag of its own, and
/// the first one's lifted row and lit destination must go with it.
#[test]
fn a_drag_whose_release_is_lost_stops_being_drawn() {
    use crossterm::event::{MouseButton, MouseEventKind};
    use ratatui::style::Modifier;
    let (_tmp, root, config) = setup();
    let mut h = EditorTestHarness::with_config_and_working_dir(140, 36, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains(CODEX_TASK_TITLE) && s.contains(CLAUDE_JOB_TITLE)
    })
    .unwrap();

    // The Codex task, held over the Claude group: it would go home to the
    // Codex group, which is lit.
    let (tcol, trow) = pos_of(&h, CODEX_TASK_TITLE);
    let (_, grow) = pos_of(&h, "▼ Claude");
    let far = 25;
    let plain = h.get_cell_style(far, grow).unwrap().bg;
    h.send_mouse(mouse_at(
        MouseEventKind::Down(MouseButton::Left),
        tcol,
        trow,
    ))
    .unwrap();
    let step: i32 = if grow > trow { 1 } else { -1 };
    let mut r = trow as i32;
    while r != grow as i32 {
        r += step;
        h.send_mouse(mouse_at(
            MouseEventKind::Drag(MouseButton::Left),
            tcol,
            r as u16,
        ))
        .unwrap();
    }
    let lifted = |h: &EditorTestHarness| {
        let (c, r) = pos_of(h, CODEX_TASK_TITLE);
        h.get_cell_style(c, r)
            .unwrap()
            .add_modifier
            .contains(Modifier::REVERSED)
    };
    let codex_band = |h: &EditorTestHarness| {
        let r = pos_of(h, "Codex").1;
        h.get_cell_style(far, r).unwrap().bg
    };
    h.wait_until(|h| lifted(h) && codex_band(h) != plain)
        .unwrap();
    let band = codex_band(&h);

    // The release is lost; the next press lands on another draggable row.
    let (jcol, jrow) = pos_of(&h, CLAUDE_JOB_TITLE);
    h.send_mouse(mouse_at(
        MouseEventKind::Down(MouseButton::Left),
        jcol,
        jrow,
    ))
    .unwrap();
    h.wait_until(|h| !lifted(h) && codex_band(h) != band)
        .unwrap();
}
