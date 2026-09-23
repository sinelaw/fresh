//! E2E: the launch dialog's projects, from a new user's first project to
//! launching one on a machine it isn't on yet.
//!
//! Each test drives the keyboard and asserts only on the screen
//! (CONTRIBUTING, Testing §2). A clone shows as done the only way the dialog
//! can show it: the project is saved only once the clone checks out, so
//! `On: Local` after `Clone and add` is the clone. Every wait names a state
//! the step produces (§3) — none is already true when it starts.
//!
//! - `Save as project` from the form's folder, back to the form with it picked;
//! - Add Project from a **Git URL** — cloned into the machine's clone folder,
//!   or `Add` when the path is already a clone of it;
//! - Add Project from a **folder** — its origin found, kept or dropped, a
//!   folder inside a clone resolved to the clone, a plain folder added as is;
//! - Launch on a machine the project isn't on: **it asks** — clone it (and
//!   remember a new clone folder), or use a folder that's already there;
//! - Projects' advanced per-machine rows: forget, pick a folder;
//! - Remove asks first;
//! - a folder on a **remote** machine, checked over ssh (a local shell stands
//!   in for the host, so there is no network);
//! - a registry written by an earlier build still loads;
//! - a host typed by hand can't hold a project, and says so.
//!
//! The "remote" is a bare repository on disk, so a clone needs no network.
#![cfg(all(unix, feature = "plugins"))]

use crate::common::dormant_ssh::local_shell_ssh_on_path;
use crate::common::global_state::{isolated_dir_context, PathPin};
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::launch_form::{
    choose_terminal_agent, focus_stop, open_new_workspace_form, FORM_TITLE,
};
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};
use std::fs;
use std::path::Path;
use std::process::Command;

const PROJECTS_TITLE: &str = "┌ Projects";
const ADD_TITLE: &str = "┌ Add Project";

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize { rows: 1, cols: 1, pixel_width: 0, pixel_height: 0 })
        .is_ok()
}

fn git(dir: &Path, args: &[&str]) {
    let out = Command::new("git")
        .args(args)
        .current_dir(dir)
        .env("GIT_AUTHOR_NAME", "t")
        .env("GIT_AUTHOR_EMAIL", "t@example.com")
        .env("GIT_COMMITTER_NAME", "t")
        .env("GIT_COMMITTER_EMAIL", "t@example.com")
        .output()
        .unwrap();
    assert!(out.status.success(), "git {args:?} failed: {}", String::from_utf8_lossy(&out.stderr));
}

/// A new user's machine: a home with one clone (`~/code/app`, its origin a
/// bare repository standing in for GitHub) and one plain folder
/// (`~/notes`), and Fresh open on the clone.
struct World {
    _tmp: tempfile::TempDir,
    _data_pin: crate::common::global_state::DataDirPin,
    _ssh: Option<PathPin>,
    /// The bare repository, used as the project's URL.
    url: String,
    h: EditorTestHarness,
}

struct Setup {
    /// A `~/.ssh/config` host `gpu`, added as a machine, whose commands run
    /// in a local folder, with a clone of the project at `~/src/api` there.
    ssh_host: bool,
    /// Files to plant in the data dir before Fresh starts (existing users).
    seed: Vec<(&'static str, String)>,
}

impl Default for Setup {
    fn default() -> Self {
        Setup { ssh_host: false, seed: Vec::new() }
    }
}

fn world(setup: Setup) -> World {
    fresh::i18n::set_locale("en");
    let tmp = tempfile::tempdir().unwrap();
    let base = tmp.path().canonicalize().unwrap();
    let (dir_context, data_pin) = isolated_dir_context(&base);
    let home = base.join("home");

    // The "remote": a bare repository with one commit.
    let seed_repo = base.join("seed");
    fs::create_dir_all(&seed_repo).unwrap();
    git(&seed_repo, &["init", "-q", "-b", "main"]);
    fs::write(seed_repo.join("README.md"), "app\n").unwrap();
    git(&seed_repo, &["add", "."]);
    git(&seed_repo, &["commit", "-q", "-m", "init"]);
    let bare = base.join("remotes").join("app.git");
    fs::create_dir_all(bare.parent().unwrap()).unwrap();
    git(&base, &["clone", "-q", "--bare", seed_repo.to_str().unwrap(), bare.to_str().unwrap()]);
    let url = bare.to_string_lossy().into_owned();

    // The user's clone, and a plain folder.
    let app = home.join("code").join("app");
    fs::create_dir_all(app.parent().unwrap()).unwrap();
    git(&base, &["clone", "-q", &url, app.to_str().unwrap()]);
    fs::create_dir_all(app.join("docs")).unwrap();
    fs::create_dir_all(home.join("notes")).unwrap();
    fs::write(home.join("notes").join("todo.md"), "todo\n").unwrap();

    let ssh = if setup.ssh_host {
        fs::create_dir_all(home.join(".ssh")).unwrap();
        fs::write(home.join(".ssh").join("config"), "Host gpu\n  HostName 10.0.0.9\n").unwrap();
        let remote_home = base.join("remote-home");
        fs::create_dir_all(remote_home.join("src")).unwrap();
        git(&base, &["clone", "-q", &url, remote_home.join("src").join("api").to_str().unwrap()]);
        let mut pin = local_shell_ssh_on_path();
        pin.set_env("FAKE_SSH_REMOTE_HOME", &remote_home);
        Some(pin)
    } else {
        None
    };

    let data_dir = dir_context.data_dir.clone();
    if setup.ssh_host {
        crate::common::launch_form::plant_saved_ssh_machine(&data_dir, "gpu", "gpu", "gpu");
    }
    for (rel, body) in &setup.seed {
        let p = data_dir.join(rel);
        fs::create_dir_all(p.parent().unwrap()).unwrap();
        fs::write(p, body).unwrap();
    }

    // Plugins live beside the workspace, outside the clone, so the clone
    // stays clean.
    let plugins_dir = app.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    fs::write(app.join(".git").join("info").join("exclude"), "plugins/\n").unwrap();

    let mut h = EditorTestHarness::create(
        160,
        50,
        HarnessOptions::new().with_working_dir(app).with_shared_dir_context(dir_context),
    )
    .unwrap();
    h.tick_and_render().unwrap();
    h.wait_until(|h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all().iter().any(|c| c.get_localized_name() == "Orchestrator: Projects")
    })
    .unwrap();
    World { _tmp: tmp, _data_pin: data_pin, _ssh: ssh, url, h }
}

fn screen(h: &EditorTestHarness) -> String {
    h.screen_to_string()
}

fn wait_for(h: &mut EditorTestHarness, needle: &str) {
    let n = needle.to_string();
    h.wait_until(move |h| h.screen_to_string().contains(&n)).unwrap();
}

fn key(h: &mut EditorTestHarness, code: KeyCode) {
    h.send_key(code, KeyModifiers::NONE).unwrap();
    h.tick_and_render().unwrap();
}

/// Tab to the control `needle` names — matched *right after* the focus
/// marker, because several rows hold two controls (`[ Cancel ]  [   Add ]`,
/// a field and its `[ Browse… ]`) and a line match alone would stop on the
/// wrong one.
fn stop(h: &mut EditorTestHarness, needle: &str) {
    focus_stop(h, &format!("▸ {needle}"));
}

/// Focus the control `needle` names and press Enter on it.
fn press(h: &mut EditorTestHarness, needle: &str) {
    stop(h, needle);
    key(h, KeyCode::Enter);
}

/// Replace the focused field's text.
fn retype(h: &mut EditorTestHarness, text: &str) {
    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL).unwrap();
    key(h, KeyCode::Backspace);
    h.type_text(text).unwrap();
    h.tick_and_render().unwrap();
}

/// Wait until a launch has landed: the form is gone and the new workspace's
/// terminal is on screen. The form closes before the workspace opens, and
/// the workspace takes the keyboard when it does — a key sent in between can
/// land in its shell.
fn launched(h: &mut EditorTestHarness) {
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains(FORM_TITLE) && s.contains("Terminal 0")
    })
    .unwrap();
}

fn open_projects(h: &mut EditorTestHarness) {
    h.run_palette_command("Orchestrator: Projects").unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains(PROJECTS_TITLE) || s.contains(ADD_TITLE)
    })
    .unwrap();
}

/// Open Add Project from Projects (an empty registry opens it directly).
fn open_add_project(h: &mut EditorTestHarness) {
    open_projects(h);
    if !screen(h).contains(ADD_TITLE) {
        press(h, "[ + Add project");
        wait_for(h, ADD_TITLE);
    }
}

/// Put Add Project's source switch on Git URL (`url`) or Folder.
fn add_from(h: &mut EditorTestHarness, url: bool) {
    let want = if url { "(•) Git URL" } else { "(•) Folder on a machine" };
    // A radio wraps, so an arrow is pressed only to move off the other
    // option; the switch opens on whichever was used last.
    if screen(h).contains(want) {
        return;
    }
    stop(h, "Add from:");
    key(h, if url { KeyCode::Left } else { KeyCode::Right });
    wait_for(h, want);
}

/// Add the project from its URL, cloning it to the default `~/src/app`.
fn add_by_url(w: &mut World) {
    open_add_project(&mut w.h);
    add_from(&mut w.h, true);
    stop(&mut w.h, "URL:");
    let url = w.url.clone();
    w.h.type_text(&url).unwrap();
    wait_for(&mut w.h, "Clone and add");
    press(&mut w.h, "[   Clone and add");
    wait_for(&mut w.h, "Known projects (1)");
}

/// Forget where the selected project is on Local, from its advanced rows.
fn forget_local(h: &mut EditorTestHarness) {
    press(h, "[ ▹ Machines (advanced)");
    wait_for(h, "Local: ");
    press(h, "[ Forget");
    wait_for(h, "Pick folder…");
}

// --- New user --------------------------------------------------------------

/// `Save as project` opens Add Project on the form's own folder, as a folder
/// with its origin found; Add brings the form back with the project picked.
#[test]
fn save_as_project_adds_the_forms_folder_and_picks_it() {
    let mut w = world(Setup::default());
    open_new_workspace_form(&mut w.h);
    press(&mut w.h, "[ Save as project");
    wait_for(&mut w.h, ADD_TITLE);
    wait_for(&mut w.h, "✓ git repo");
    let s = screen(&w.h);
    assert!(s.contains("(•) Folder on a machine"), "opens on the Folder source:\n{s}");
    assert!(s.contains("~/code/app"), "the form's folder is filled in:\n{s}");
    assert!(s.contains("(•) Use its origin"), "the clone's origin is offered as the remote:\n{s}");

    press(&mut w.h, "[   Add");
    wait_for(&mut w.h, FORM_TITLE);
    choose_terminal_agent(&mut w.h);
    wait_for(&mut w.h, "at ~/code/app");
    assert!(screen(&w.h).contains("Project: [app"), "the new project is picked:\n{}", screen(&w.h));
}

/// Git URL: the name comes from the URL, the path is the machine's clone
/// folder plus the name, and the button that clones says so.
#[test]
fn add_by_url_clones_into_the_machines_clone_folder() {
    let mut w = world(Setup::default());
    open_add_project(&mut w.h);
    add_from(&mut w.h, true);
    stop(&mut w.h, "URL:");
    let url = w.url.clone();
    w.h.type_text(&url).unwrap();
    wait_for(&mut w.h, "✓ reachable");
    wait_for(&mut w.h, "it will be cloned here");
    let s = screen(&w.h);
    assert!(s.contains("Name: [app"), "the name follows the URL:\n{s}");
    assert!(s.contains("[~/src/app"), "the path is the clone folder plus the name:\n{s}");

    press(&mut w.h, "[   Clone and add");
    wait_for(&mut w.h, "Known projects (1)");
    assert!(screen(&w.h).contains("On: Local"), "{}", screen(&w.h));
    press(&mut w.h, "[ ▹ Machines (advanced)");
    wait_for(&mut w.h, "Local: ~/src/app");
}

/// Git URL onto a path that already holds a clone of it: nothing to clone,
/// and the button is plain `Add`.
#[test]
fn add_by_url_onto_an_existing_clone_just_adds() {
    let mut w = world(Setup::default());
    open_add_project(&mut w.h);
    add_from(&mut w.h, true);
    stop(&mut w.h, "URL:");
    let url = w.url.clone();
    w.h.type_text(&url).unwrap();
    wait_for(&mut w.h, "✓ reachable");
    stop(&mut w.h, "Path:");
    retype(&mut w.h, "~/code/app");
    wait_for(&mut w.h, "existing clone");
    assert!(!screen(&w.h).contains("Clone and add"), "{}", screen(&w.h));
    press(&mut w.h, "[   Add");
    wait_for(&mut w.h, "Known projects (1)");
    press(&mut w.h, "[ ▹ Machines (advanced)");
    wait_for(&mut w.h, "Local: ~/code/app");
}

/// Folder: a folder inside a clone resolves to the clone; its origin is
/// offered, and `None` keeps the project on this machine, listed by path.
#[test]
fn add_folder_resolves_the_clone_and_can_stay_local_only() {
    let mut w = world(Setup::default());
    open_add_project(&mut w.h);
    add_from(&mut w.h, false);
    stop(&mut w.h, "Folder:");
    w.h.type_text("~/code/app/docs").unwrap();
    wait_for(&mut w.h, "✓ git repo");
    wait_for(&mut w.h, "[~/code/app ");
    stop(&mut w.h, "Remote:");
    key(&mut w.h, KeyCode::Right);
    wait_for(&mut w.h, "(•) None (this machine only)");
    press(&mut w.h, "[   Add");
    wait_for(&mut w.h, "Known projects (1)");
    let s = screen(&w.h);
    assert!(s.contains("app") && s.contains("~/code/app"), "listed by its path:\n{s}");
    assert!(s.contains("none — kept on this machine only"), "{s}");
}

/// A folder that isn't a repository is a plain-folder project: listed by its
/// path, and a launch opens in it as is.
#[test]
fn plain_folder_project_lists_its_path_and_opens_in_place() {
    let mut w = world(Setup::default());
    open_add_project(&mut w.h);
    add_from(&mut w.h, false);
    stop(&mut w.h, "Folder:");
    w.h.type_text("~/notes").unwrap();
    wait_for(&mut w.h, "folder · not a git repository");
    press(&mut w.h, "[   Add");
    wait_for(&mut w.h, "Known projects (1)");
    assert!(screen(&w.h).contains("~/notes"), "{}", screen(&w.h));
    assert!(screen(&w.h).contains("FOLDER") || screen(&w.h).contains("a plain folder"), "{}", screen(&w.h));

    press(&mut w.h, "[ New workspace here");
    wait_for(&mut w.h, FORM_TITLE);
    choose_terminal_agent(&mut w.h);
    wait_for(&mut w.h, "at ~/notes");
    wait_for(&mut w.h, "plain folder");
    if !pty_available() {
        return;
    }
    press(&mut w.h, "[ Launch ]");
    launched(&mut w.h);
}

// --- Launching where the project isn't yet --------------------------------

/// The project isn't on this machine: the form says Launch will ask, Launch
/// asks, and `Clone it` into a folder outside the clone folder offers to
/// remember that folder — which the next Add Project then uses.
#[test]
fn launch_asks_where_the_project_is_and_clones_it() {
    let mut w = world(Setup::default());
    add_by_url(&mut w);
    forget_local(&mut w.h);
    press(&mut w.h, "[ New workspace here");
    wait_for(&mut w.h, FORM_TITLE);
    choose_terminal_agent(&mut w.h);
    wait_for(&mut w.h, "isn't on Local yet — Launch will ask");

    press(&mut w.h, "[ Launch ]");
    wait_for(&mut w.h, "Where is it?");
    let s = screen(&w.h);
    assert!(s.contains("(•) Clone it"), "{s}");
    assert!(s.contains("[~/src/app"), "defaults to the clone folder plus the name:\n{s}");
    assert!(!s.contains("Clone future projects"), "nothing to remember yet:\n{s}");

    stop(&mut w.h, "Clone into:");
    retype(&mut w.h, "~/work/app");
    wait_for(&mut w.h, "Clone future projects on Local into ~/work");
    stop(&mut w.h, "[ ] Clone future projects");
    key(&mut w.h, KeyCode::Char(' '));
    wait_for(&mut w.h, "[v] Clone future projects");

    if !pty_available() {
        return;
    }
    press(&mut w.h, "[   Clone and launch");
    launched(&mut w.h);

    // The answer was kept: Local now has the clone it made.
    open_projects(&mut w.h);
    press(&mut w.h, "[ ▹ Machines (advanced)");
    wait_for(&mut w.h, "Local: ~/work/app");
    key(&mut w.h, KeyCode::Esc);
    w.h.wait_until(|h| !h.screen_to_string().contains(PROJECTS_TITLE)).unwrap();

    // The remembered clone folder is the next default.
    open_add_project(&mut w.h);
    add_from(&mut w.h, true);
    stop(&mut w.h, "URL:");
    w.h.type_text("/nowhere/other.git").unwrap();
    wait_for(&mut w.h, "[~/work/other");
}

/// `It's already there`: a folder that is a clone of the project is checked
/// and used, and the launch goes ahead from it.
#[test]
fn launch_asks_and_uses_a_folder_that_is_already_there() {
    let mut w = world(Setup::default());
    add_by_url(&mut w);
    forget_local(&mut w.h);
    press(&mut w.h, "[ New workspace here");
    wait_for(&mut w.h, FORM_TITLE);
    choose_terminal_agent(&mut w.h);
    press(&mut w.h, "[ Launch ]");
    wait_for(&mut w.h, "Where is it?");
    stop(&mut w.h, "Where:");
    key(&mut w.h, KeyCode::Right);
    wait_for(&mut w.h, "(•) It's already there");
    stop(&mut w.h, "Folder:");
    w.h.type_text("~/src/app").unwrap();
    wait_for(&mut w.h, "origin matches");
    wait_for(&mut w.h, "Use it and launch");

    // Esc drops the question and leaves the form as it was.
    key(&mut w.h, KeyCode::Esc);
    w.h.wait_until(|h| !h.screen_to_string().contains("Where is it?")).unwrap();
    assert!(screen(&w.h).contains(FORM_TITLE), "{}", screen(&w.h));

    if !pty_available() {
        return;
    }
    press(&mut w.h, "[ Launch ]");
    wait_for(&mut w.h, "Where is it?");
    stop(&mut w.h, "Where:");
    key(&mut w.h, KeyCode::Right);
    stop(&mut w.h, "Folder:");
    w.h.type_text("~/src/app").unwrap();
    wait_for(&mut w.h, "origin matches");
    press(&mut w.h, "[   Use it and launch");
    launched(&mut w.h);
}

// --- Projects dialog ---------------------------------------------------------

/// The per-machine rows: forget where the project is, then pick the folder
/// again, answered in the row and saved by its own button.
#[test]
fn machine_rows_forget_and_pick_a_folder() {
    let mut w = world(Setup::default());
    add_by_url(&mut w);
    assert!(screen(&w.h).contains("On: Local"), "{}", screen(&w.h));
    forget_local(&mut w.h);
    assert!(screen(&w.h).contains("On: no machine yet"), "{}", screen(&w.h));

    press(&mut w.h, "[ Pick folder…");
    wait_for(&mut w.h, "(•) It's already there");
    stop(&mut w.h, "Folder:");
    w.h.type_text("~/src/app").unwrap();
    wait_for(&mut w.h, "origin matches");
    press(&mut w.h, "[   Use this folder");
    wait_for(&mut w.h, "On: Local");
    assert!(screen(&w.h).contains("Change…"), "{}", screen(&w.h));
}

/// Remove asks in place; Cancel keeps the project, Remove takes it off the
/// list and leaves its files alone.
#[test]
fn remove_asks_first_and_keeps_the_files() {
    let mut w = world(Setup::default());
    add_by_url(&mut w);
    press(&mut w.h, "[ Remove…");
    wait_for(&mut w.h, "Remove app from Projects?");
    press(&mut w.h, "[ Cancel ]");
    w.h.wait_until(|h| !h.screen_to_string().contains("Remove app from Projects?")).unwrap();
    assert!(screen(&w.h).contains("Known projects (1)"), "{}", screen(&w.h));

    press(&mut w.h, "[ Remove…");
    wait_for(&mut w.h, "Remove app from Projects?");
    stop(&mut w.h, "[ Remove ]");
    key(&mut w.h, KeyCode::Enter);
    wait_for(&mut w.h, "No projects yet.");

    // Its clone is still there, and can be added back.
    press(&mut w.h, "[ + Add project");
    wait_for(&mut w.h, ADD_TITLE);
    add_from(&mut w.h, false);
    stop(&mut w.h, "Folder:");
    w.h.type_text("~/src/app").unwrap();
    wait_for(&mut w.h, "✓ git repo");
}

// --- Remote machines ---------------------------------------------------------

/// A folder on another machine: listed over ssh, checked there, and the
/// project remembers it for that machine.
#[test]
fn add_folder_on_a_remote_machine_checks_it_over_ssh() {
    let mut w = world(Setup { ssh_host: true, ..Default::default() });
    open_add_project(&mut w.h);
    add_from(&mut w.h, false);
    stop(&mut w.h, "Machine:");
    key(&mut w.h, KeyCode::Right);
    wait_for(&mut w.h, "Machine: [gpu");

    // Browse lists the remote home.
    press(&mut w.h, "[ Browse…");
    wait_for(&mut w.h, "gpu : ~");
    wait_for(&mut w.h, "src/");
    key(&mut w.h, KeyCode::Esc);

    stop(&mut w.h, "Folder:");
    w.h.type_text("~/src/api").unwrap();
    wait_for(&mut w.h, "✓ git repo");
    press(&mut w.h, "[   Add");
    wait_for(&mut w.h, "Known projects (1)");
    assert!(screen(&w.h).contains("On: gpu"), "{}", screen(&w.h));
}

// --- Existing users ----------------------------------------------------------

/// A registry written by an earlier build (with `cloneNewTo`, no `kind`)
/// loads: the project is in the Project list and resolves to its clone.
#[test]
fn a_registry_from_an_earlier_build_still_loads() {
    let old = r#"{"id":"r-old","name":"legacy","remote":"https://example.com/legacy.git","cloneNewTo":"~/src/<name>","clones":{"local":"~/code/app"}}"#;
    let mut w = world(Setup { seed: vec![("state/repositories/r-old.json", old.to_string())], ..Default::default() });
    open_new_workspace_form(&mut w.h);
    stop(&mut w.h, "Project:");
    key(&mut w.h, KeyCode::Enter);
    wait_for(&mut w.h, "legacy");
    // `legacy` sits above `Folder…`.
    key(&mut w.h, KeyCode::Up);
    key(&mut w.h, KeyCode::Enter);
    wait_for(&mut w.h, "Project: [legacy");
    wait_for(&mut w.h, "at ~/code/app");
}

/// A remote project's folder is usually written `~/…`; the remote probe has
/// to let the far side expand it. (It was quoted, so a clone at `~/src/api`
/// read as "plain folder" and the form offered no worktree.)
#[test]
fn a_remote_clone_under_home_reads_as_a_repository() {
    let mut w = world(Setup { ssh_host: true, ..Default::default() });
    open_add_project(&mut w.h);
    add_from(&mut w.h, false);
    stop(&mut w.h, "Machine:");
    key(&mut w.h, KeyCode::Right);
    wait_for(&mut w.h, "Machine: [gpu");
    stop(&mut w.h, "Folder:");
    w.h.type_text("~/src/api").unwrap();
    wait_for(&mut w.h, "✓ git repo");
    press(&mut w.h, "[   Add");
    wait_for(&mut w.h, "On: gpu");

    // New workspace on it, on gpu: the form reads the clone as a repository.
    press(&mut w.h, "[ New workspace here");
    wait_for(&mut w.h, FORM_TITLE);
    choose_terminal_agent(&mut w.h);
    wait_for(&mut w.h, "at ~/src/api");
    wait_for(&mut w.h, "new worktree");
    assert!(!screen(&w.h).contains("plain folder"), "{}", screen(&w.h));
}
