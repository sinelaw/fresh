//! E2E: a workspace whose folder is gone — renamed or deleted after Fresh
//! opened it, the way a project's folder goes missing when it is moved while
//! Fresh is closed.
//!
//! - A host spawn with no folder of its own used to run in the workspace's
//!   folder, so it failed before the program started ("No such file or
//!   directory"). That broke every host probe from such a workspace — among
//!   them the Projects dialog's check of the folder picked to repair it, which
//!   then could not be repaired from there. It now runs.
//! - A new workspace in a folder that isn't there used to open anyway, with
//!   its terminal quietly started in the home folder by the PTY layer. It is
//!   now refused, naming the folder.
//!
//! A probe plugin makes each call from the palette and writes the result into
//! the open buffer, where the assertion reads it off the screen.
#![cfg(all(unix, feature = "plugins"))]

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

const PROBE_PLUGIN: &str = r#"
const editor = getEditor();

function report(msg: string): void {
    const id = editor.getActiveBufferId();
    if (id !== null && id !== undefined) editor.insertText(id, 0, msg + " ");
}

registerHandler("probe_host_spawn", async function () {
    const r = await editor.spawnHostProcess("sh", ["-c", "echo HOST_RAN"]);
    report("PROBE_SPAWN " + r.exit_code + " " + (r.stdout || "").trim());
});

registerHandler("probe_window_in_missing_folder", async function () {
    const cwd = editor.getCwd();
    try {
        await editor.createWindowWithTerminal({ root: cwd, label: "gone", cwd });
        report("PROBE_WINDOW_CREATED");
    } catch (e: any) {
        report("PROBE_WINDOW_REFUSED " + e.message);
    }
});

editor.registerCommand("Probe Host Spawn", "", "probe_host_spawn", null);
editor.registerCommand("Probe Window In Missing Folder", "", "probe_window_in_missing_folder", null);
"#;

/// Fresh open on a folder that is then renamed away. The plugins are loaded
/// before the rename, so they keep running.
fn moved_away() -> (tempfile::TempDir, EditorTestHarness, std::path::PathBuf) {
    fresh::i18n::set_locale("en");
    let tmp = tempfile::TempDir::new().unwrap();
    let base = tmp.path().canonicalize().unwrap();
    let root = base.join("proj");
    let plugins_dir = root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("missing_folder_probe.ts"), PROBE_PLUGIN).unwrap();

    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 30, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    h.wait_until(|h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all().iter().any(|c| c.get_localized_name() == "Probe Host Spawn")
    })
    .unwrap();
    fs::rename(&root, base.join("proj-moved")).unwrap();
    (tmp, h, root)
}

fn run_command(h: &mut EditorTestHarness, name: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(name).unwrap();
    // Twice: the prompt's own input line, and the command's row under it.
    h.wait_until(|h| h.screen_to_string().matches(name).count() >= 2)
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
}

#[test]
fn host_spawn_runs_from_a_workspace_whose_folder_is_gone() {
    let (_tmp, mut h, _root) = moved_away();
    run_command(&mut h, "Probe Host Spawn");
    h.wait_until(|h| h.screen_to_string().contains("PROBE_SPAWN")).unwrap();
    let s = h.screen_to_string();
    assert!(s.contains("PROBE_SPAWN 0 HOST_RAN"), "{s}");
}

#[test]
fn new_workspace_in_a_missing_folder_is_refused_naming_it() {
    let (_tmp, mut h, root) = moved_away();
    run_command(&mut h, "Probe Window In Missing Folder");
    h.wait_until(|h| h.screen_to_string().contains("PROBE_WINDOW_")).unwrap();
    let s = h.screen_to_string();
    assert!(s.contains("PROBE_WINDOW_REFUSED"), "{s}");
    let name = root.file_name().unwrap().to_string_lossy().into_owned();
    assert!(s.contains(&format!("{name} doesn't exist")), "{s}");
}
