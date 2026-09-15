//! Repro: "Open File" in a workspace that was just created from the dock's
//! `[ + New ]` button must leave the keyboard **in the file**.
//!
//! The user flow: the dock is focused, `[ + New ]` opens the New Workspace
//! form, "Create Workspace" builds the workspace and takes the user into it.
//! Running "Open File" there opens the file — and typing must land in it.
//!
//! Per CONTRIBUTING §2 this drives only keyboard/mouse and asserts on
//! rendered output: the marker typed after the open has to appear on screen.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};
use std::fs;
use std::path::PathBuf;

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

/// A **non-git** project with the orchestrator plugin installed: the form
/// demotes "Create a new git worktree" to unchecked for a non-git path, so
/// the create is just "open a workspace here with a terminal" — no `git
/// worktree add` to slow the test down or to fail on a repo with no commits.
fn setup_project(name: &str) -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join(name);
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    fs::write(root.join("notes.txt"), "first line\n").unwrap();
    (temp_dir, root)
}

/// Toggle the dock open and wait for it to render *and* hold the keyboard.
///
/// Polling `is_dock_focused()` is the gate `e2e::orchestrator_dock`'s helper
/// documents: the plugin focuses the dock asynchronously after it mounts, so
/// a key sent on "the column is painted" alone can land before the focus
/// does and fall through to the editor, leaving the next wait blocked on a
/// dock response that never comes.
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
    h.find_text_on_screen(needle)
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{}", h.screen_to_string()))
}

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn open_file_in_a_just_created_workspace_takes_the_keyboard() {
    if !pty_available() {
        eprintln!("Skipping: PTY not available");
        return;
    }
    fresh::i18n::set_locale("en");
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // `[ + New ]` → the New Workspace form.
    let (ncol, nrow) = pos_of(&h, "+ New");
    h.mouse_click(ncol + 1, nrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();

    // "Create Workspace" is the create-and-visit button: it builds the
    // workspace and takes the user into it.
    let (ccol, crow) = pos_of(&h, "Create Workspace");
    h.mouse_click(ccol + 1, crow).unwrap();
    // The workspace is live once the form is gone and its seeded terminal is
    // on screen as the new window's only tab.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("New Workspace") && s.contains("Terminal 0")
    })
    .unwrap();

    // Open a file in it, exactly as the user did.
    h.run_palette_command("Open File").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Open file:"))
        .unwrap();
    h.type_text(root.join("notes.txt").to_str().unwrap())
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // The file is open in the new workspace: its tab, and its content, are
    // on screen beside the terminal the workspace was seeded with.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("notes.txt") && s.contains("first line")
    })
    .unwrap();

    // The file has the keyboard: what is typed lands in it.
    h.type_text("ZZMARKZZ").unwrap();
    h.wait_until_stable(|_| true).unwrap();
    assert!(
        h.screen_to_string().contains("ZZMARKZZ"),
        "typing after Open File must land in the opened file:\n{}",
        h.screen_to_string()
    );
}
