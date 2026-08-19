//! E2E coverage for the Orchestrator's *plugin API* — the surface a script
//! (or another plugin) reaches through `editor.getPluginApi("orchestrator")`.
//!
//! The API used to publish only the two create dialogs plus "focus a
//! workspace", so everything a user does *after* a workspace exists — rename
//! it, file it under a folder, change what the dock shows, act on its
//! lifecycle — was reachable from the dock's menus and from nowhere else.
//! These tests pin the verbs that closed that gap.
//!
//! Per CONTRIBUTING.md §2 they drive keyboard/mouse only and assert on
//! rendered output: a probe plugin registers one command per scenario, the
//! command is run from the palette, and the assertion is what the dock (or
//! the status line) then shows.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::PathBuf;

/// A probe plugin: one palette command per API verb under test.
///
/// Results are written into the open buffer rather than the status bar,
/// because the status segment truncates to about twenty columns and the
/// interesting part of a folder listing or a refusal message is past that.
/// The buffer renders at the editor's full width beside the dock.
///
/// Every command resolves its target the way a real caller would — through
/// `listWorkspaces()`, picking the row it marks `active` — so the summary
/// fields are exercised alongside the verbs.
const PROBE_PLUGIN: &str = r#"
const editor = getEditor();

function orch(): any {
    return editor.getPluginApi("orchestrator");
}

// The workspace the editor is focused on, as `listWorkspaces()` reports it.
function me(): any {
    return orch().listWorkspaces().find((w: any) => w.active);
}

// Report a result where a screen assertion can read all of it.
function report(msg: string): void {
    const id = editor.getActiveBufferId();
    if (id !== null && id !== undefined) editor.insertText(id, 0, msg);
}

registerHandler("probe_file_under_folder", function () {
    const o = orch();
    const id = o.createFolder("Reviews");
    o.moveWorkspace(me().workspaceId, id);
    report("PROBE_FILED " + id);
});

registerHandler("probe_rename", function () {
    orch().renameWorkspace(me().windowId, "RenamedByScript");
    report("PROBE_RENAMED");
});

registerHandler("probe_list_folders", function () {
    const o = orch();
    const outer = o.createFolder("Outer");
    o.createFolder("Inner", outer);
    const shown = o.listFolders()
        .map(function (f: any) { return f.depth + "-" + f.name; })
        .join("/");
    report("PROBE_FOLDERS " + shown);
});

registerHandler("probe_compact", function () {
    orch().setDockView("compact");
    report("PROBE_COMPACT");
});

registerHandler("probe_filter_text", function () {
    orch().setDockFilter({ text: "zzzznomatch" });
    report("PROBE_FILTERED");
});

// The launch workspace has no agent terminal, so Stop is refused. The
// picker greys the button out; a caller gets the reason instead.
registerHandler("probe_stop", function () {
    try {
        orch().stopWorkspace(me().windowId);
        report("PROBE_STOP_RAN");
    } catch (e: any) {
        report("PROBE_STOP_REFUSED " + e.message);
    }
});

// An id that matches nothing is a `false`, not a throw — the convention the
// whole surface follows so a caller that is guessing can branch.
registerHandler("probe_unknown_target", function () {
    const o = orch();
    try {
        const ok = o.renameWorkspace("ws-does-not-exist", "nope");
        report(ok ? "PROBE_UNKNOWN_TRUE" : "PROBE_UNKNOWN_FALSE");
    } catch (e: any) {
        report("PROBE_UNKNOWN_THREW");
    }
});

// An unknown *folder* is the other half of the convention: the workspace
// exists, so the refusal is an error rather than a silent top-level filing.
registerHandler("probe_unknown_folder", function () {
    try {
        orch().moveWorkspace(me().windowId, "df-nope");
        report("PROBE_FOLDER_RAN");
    } catch (e: any) {
        report("PROBE_FOLDER_REFUSED " + e.message);
    }
});

// The archive starts empty and stays readable — a caller can ask before
// anything has ever been archived without special-casing the answer.
registerHandler("probe_list_archived", function () {
    const rows = orch().listArchived();
    report("PROBE_ARCHIVED " + rows.length);
});

// Unarchiving something that was never archived is a `false`, not a throw.
registerHandler("probe_unarchive_missing", async function () {
    try {
        const ok = await orch().unarchiveWorkspace("never-archived");
        report(ok ? "PROBE_UNARCH_TRUE" : "PROBE_UNARCH_FALSE");
    } catch (e: any) {
        report("PROBE_UNARCH_THREW " + e.message);
    }
});

// An SSH create with no host is refused before anything is spawned — the
// dialog keeps itself open on the same condition. Proves the API reaches
// the shared spec builder's validation rather than a copy of it.
registerHandler("probe_ssh_no_host", async function () {
    try {
        await orch().newWorkspace({ backend: "ssh", host: "" });
        report("PROBE_SSH_RAN");
    } catch (e: any) {
        report("PROBE_SSH_REFUSED " + e.message);
    }
});

editor.registerCommand("Probe File Under Folder", "", "probe_file_under_folder", null);
editor.registerCommand("Probe Rename Workspace", "", "probe_rename", null);
editor.registerCommand("Probe List Folders", "", "probe_list_folders", null);
editor.registerCommand("Probe Compact View", "", "probe_compact", null);
editor.registerCommand("Probe Filter Text", "", "probe_filter_text", null);
editor.registerCommand("Probe Stop Workspace", "", "probe_stop", null);
editor.registerCommand("Probe Unknown Target", "", "probe_unknown_target", null);
editor.registerCommand("Probe Unknown Folder", "", "probe_unknown_folder", null);
editor.registerCommand("Probe List Archived", "", "probe_list_archived", null);
editor.registerCommand("Probe Unarchive Missing", "", "probe_unarchive_missing", null);
editor.registerCommand("Probe Ssh No Host", "", "probe_ssh_no_host", null);
"#;

/// A git project with the orchestrator plugin and the API probe installed.
fn setup_project() -> (tempfile::TempDir, PathBuf) {
    fresh::i18n::set_locale("en");

    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("alphaproj");
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    fs::write(plugins_dir.join("api_probe.ts"), PROBE_PLUGIN).unwrap();
    fs::write(root.join("readme.txt"), "hello\n").unwrap();
    let ok = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root)
        .status()
        .unwrap()
        .success();
    assert!(ok);
    (temp_dir, root)
}

fn harness() -> (tempfile::TempDir, EditorTestHarness) {
    let (tmp, root) = setup_project();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root).unwrap();
    h.render().unwrap();
    (tmp, h)
}

/// Toggle the dock open and wait for it to render *and* take keyboard focus
/// (the plugin sets focus asynchronously after the mount, so a key dispatched
/// on render alone can land before the dock is listening).
fn open_dock(h: &mut EditorTestHarness) {
    run_command(h, "Orchestrator: Toggle Dock");
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator") && h.editor().is_dock_focused())
        .unwrap();
}

/// Run a command by its palette name.
fn run_command(h: &mut EditorTestHarness, name: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(name).unwrap();
    h.wait_until(|h| h.screen_to_string().contains(name))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
}

/// The dock's own columns of the screen. The editor beside it carries the
/// project name in its own chrome, so any "the dock does / does not list X"
/// assertion has to be scoped to the dock's column or it reads the editor's
/// tab bar by accident. 32 is inside the dock at this harness width (the
/// responsive default lands at 34 for a 120-column terminal).
fn dock_column(screen: &str) -> String {
    screen
        .lines()
        .map(|l| l.chars().take(32).collect::<String>())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Click the toolbar's density button, which sits beside "Filters" rather
/// than inside it. Used to put the dock in card density — the opposite of
/// the compact default — so a probe that switches it *to* compact has
/// somewhere to switch from.
fn click_view_button(h: &mut EditorTestHarness) {
    let screen = h.screen_to_string();
    // Click the button itself, not the start of its row — the density button
    // shares the toolbar row with "Filters", which owns the left edge.
    let (vrow, vcol) = screen
        .lines()
        .enumerate()
        .find_map(|(r, l)| {
            l.find("view:")
                .map(|b| (r as u16, l[..b].chars().count() as u16))
        })
        .unwrap_or_else(|| panic!("screen missing 'view:':\n{screen}"));
    h.mouse_click(vcol + 1, vrow).unwrap();
}

/// Expand the dock's collapsible "Filters" section, which holds the project
/// control and the two checkboxes.
fn expand_filters(h: &mut EditorTestHarness) {
    let screen = h.screen_to_string();
    let frow = screen
        .lines()
        .position(|l| l.contains("Filters"))
        .unwrap_or_else(|| panic!("screen missing 'Filters':\n{screen}")) as u16;
    h.mouse_click(3, frow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Manage"))
        .unwrap();
}

/// `createFolder` + `moveWorkspace` put the workspace under the folder on the
/// dock — the headless twin of "New Folder…" plus "Move to Folder…".
#[test]
fn create_folder_and_move_workspace_files_the_row_on_the_dock() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);
    // The workspace starts at the top level: no folder on the dock at all.
    h.assert_screen_contains("alphaproj");
    h.assert_screen_not_contains("Reviews");

    run_command(&mut h, "Probe File Under Folder");

    // The folder now renders, and the workspace renders underneath it rather
    // than at the top level.
    h.wait_until(|h| h.screen_to_string().contains("Reviews"))
        .unwrap();
    let screen = h.screen_to_string();
    let dock = dock_column(&screen);
    let folder_row = dock
        .lines()
        .position(|l| l.contains("Reviews"))
        .unwrap_or_else(|| panic!("dock missing 'Reviews':\n{screen}"));
    let ws_row = dock
        .lines()
        .skip(folder_row + 1)
        .position(|l| l.contains("alphaproj"))
        .unwrap_or_else(|| panic!("workspace never rendered below its folder:\n{screen}"));
    // A card is a bordered three-line pill, so "directly under" is a few
    // rows, not one — but it is nowhere near the whole dock.
    assert!(
        ws_row < 6,
        "workspace should sit just under its folder, not {ws_row} rows below:\n{screen}"
    );
    // Nothing is left at the top level above the folder.
    assert!(
        !dock
            .lines()
            .take(folder_row)
            .any(|l| l.contains("alphaproj")),
        "workspace still listed above its folder:\n{screen}"
    );
}

/// `renameWorkspace` relabels the row — the headless twin of "Rename…".
#[test]
fn rename_workspace_relabels_the_row_on_the_dock() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);
    h.assert_screen_contains("alphaproj");
    h.assert_screen_not_contains("RenamedByScript");

    run_command(&mut h, "Probe Rename Workspace");

    h.wait_until(|h| h.screen_to_string().contains("RenamedByScript"))
        .unwrap();
}

/// `listFolders` reports what the dock shows, parents before children, with
/// the nesting depth a caller needs to render the tree.
#[test]
fn list_folders_reports_the_tree_the_dock_renders() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);

    run_command(&mut h, "Probe List Folders");

    h.wait_until(|h| h.screen_to_string().contains("PROBE_FOLDERS"))
        .unwrap();
    h.assert_screen_contains("PROBE_FOLDERS 0-Outer/1-Inner");
    // ...and both really are on the dock.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Outer") && s.contains("Inner")
    })
    .unwrap();
}

/// `setDockView` flips the density the dock's own "view" button flips.
#[test]
fn set_dock_view_switches_the_dock_to_compact() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);
    // Start from card, so the probe has a density to change.
    click_view_button(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("view: card"))
        .unwrap();

    run_command(&mut h, "Probe Compact View");

    h.wait_until(|h| h.screen_to_string().contains("view: compact"))
        .unwrap();
    h.assert_screen_not_contains("view: card");
}

/// `setDockFilter` drives the dock's search box: a needle nothing matches
/// empties the list, and the box shows the needle that did it.
#[test]
fn set_dock_filter_narrows_the_dock_list() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);
    h.assert_screen_contains("alphaproj");

    run_command(&mut h, "Probe Filter Text");

    h.wait_until(|h| h.screen_to_string().contains("zzzznomatch"))
        .unwrap();
    let screen = h.screen_to_string();
    // The dock column is the leading ~32 cells of each row; the editor to its
    // right still has the project name in its own chrome, so scope the
    // "no workspace listed" assertion to the dock's own columns.
    let dock: String = screen
        .lines()
        .map(|l| l.chars().take(32).collect::<String>())
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        !dock.contains("alphaproj"),
        "filtered-out workspace still listed on the dock:\n{screen}"
    );
}

/// Stop is refused for a workspace with no agent process, with the reason —
/// the picker greys the button out, a caller gets an error it can read.
#[test]
fn stop_workspace_refuses_a_workspace_with_no_agent() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);

    run_command(&mut h, "Probe Stop Workspace");

    h.wait_until(|h| h.screen_to_string().contains("PROBE_STOP_"))
        .unwrap();
    h.assert_screen_contains("PROBE_STOP_REFUSED");
    h.assert_screen_contains("no agent process to stop");
}

/// An id that matches no workspace returns `false` rather than throwing —
/// the convention that lets a caller branch without try/catch.
#[test]
fn an_unknown_workspace_id_returns_false_instead_of_throwing() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);

    run_command(&mut h, "Probe Unknown Target");

    h.wait_until(|h| h.screen_to_string().contains("PROBE_UNKNOWN_"))
        .unwrap();
    h.assert_screen_contains("PROBE_UNKNOWN_FALSE");
}

/// An unknown folder id is the other half of that convention: the workspace
/// exists, so a move that cannot be honoured throws instead of quietly
/// filing the row at the top level.
#[test]
fn an_unknown_folder_id_throws_instead_of_filing_at_top_level() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);

    run_command(&mut h, "Probe Unknown Folder");

    h.wait_until(|h| h.screen_to_string().contains("PROBE_FOLDER_"))
        .unwrap();
    h.assert_screen_contains("PROBE_FOLDER_REFUSED");
    h.assert_screen_contains("no such folder");
}

/// `listArchived` answers on a machine that has never archived anything —
/// it walks the per-repo manifest directory, and a missing directory is an
/// empty archive, not a failure.
#[test]
fn list_archived_reports_an_empty_archive_rather_than_failing() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);

    run_command(&mut h, "Probe List Archived");

    h.wait_until(|h| h.screen_to_string().contains("PROBE_ARCHIVED"))
        .unwrap();
    h.assert_screen_contains("PROBE_ARCHIVED 0");
}

/// Unarchiving a name that is not in the archive returns `false` — the same
/// "the thing does not exist" convention the workspace verbs follow, rather
/// than an exception a caller has to distinguish from a real restore failure.
#[test]
fn unarchiving_something_not_archived_returns_false() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);

    run_command(&mut h, "Probe Unarchive Missing");

    h.wait_until(|h| h.screen_to_string().contains("PROBE_UNARCH_"))
        .unwrap();
    h.assert_screen_contains("PROBE_UNARCH_FALSE");
}

/// An SSH create with no host is refused with the dialog's own message, and
/// nothing is spawned. The API and the form reach the same validation because
/// they build the spec with the same function.
///
/// The refusal must also leave the plugin host alive. An `async` verb that
/// throws before its first `await` returns an already-rejected promise, and
/// the runtime reports that as an unhandled rejection *even though the caller
/// catches it* — the tracker fires a tick before `await` attaches a handler.
/// Under this harness (`set_panic_on_js_errors`) that kills the plugin thread,
/// so the last step here drives a second command through the plugin and waits
/// for its answer: if the refusal had poisoned the host, nothing would come
/// back.
#[test]
fn an_ssh_create_without_a_host_is_refused_without_killing_the_plugin_host() {
    let (_tmp, mut h) = harness();
    open_dock(&mut h);

    run_command(&mut h, "Probe Ssh No Host");

    h.wait_until(|h| h.screen_to_string().contains("PROBE_SSH_"))
        .unwrap();
    h.assert_screen_contains("PROBE_SSH_REFUSED");
    // The dialog's own localized string for the same condition.
    h.assert_screen_contains("a host is required");
    // No pending row appeared on the dock: the refusal happened before any
    // connect was started.
    let screen = h.screen_to_string();
    assert!(
        !dock_column(&screen).contains("ssh:"),
        "a refused ssh create still added a dock row:\n{screen}"
    );

    // The plugin still answers.
    run_command(&mut h, "Probe List Archived");
    h.wait_until(|h| h.screen_to_string().contains("PROBE_ARCHIVED"))
        .unwrap();
}
