//! E2E coverage for the global Orchestrator dock (the persistent,
//! non-modal left column toggled by "Orchestrator: Toggle Dock").
//!
//! Per CONTRIBUTING.md §2 these drive only keyboard/mouse and assert on
//! rendered output. Each guards a behaviour that regressed during dock
//! bring-up:
//!
//! * the dock renders as a left column beside the editor chrome;
//! * it is non-modal — Ctrl+P while the dock is focused opens the
//!   command palette (the key falls through to the editor) instead of
//!   being swallowed, and the dock stays visible;
//! * the session list order is stable as the active window changes
//!   (the picker's current-project-first sort must not reorder the
//!   persistent dock);
//! * mouse clicks land on dock widgets (the "+ New" button opens the
//!   new-session form).

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::PathBuf;

/// A git project with the orchestrator plugin (+ shared lib) installed.
fn setup_project(name: &str) -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join(name);
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
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

/// Toggle the dock open via the command palette and wait for it to render.
fn open_dock(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ORCHESTRATOR"))
        .unwrap();
}

/// 0-based screen row containing `needle`, or panic with the screen.
fn row_of(h: &EditorTestHarness, needle: &str) -> usize {
    let screen = h.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{screen}"))
}

#[test]
fn dock_renders_as_left_column_beside_chrome() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // The dock and its controls render...
    h.assert_screen_contains("ORCHESTRATOR");
    h.assert_screen_contains("+ New");
    // ...and the editor chrome (menu bar) is still present to its right,
    // i.e. the dock is a column beside the window, not a replacement.
    h.assert_screen_contains("File");
    // The launch session is listed by its project basename.
    h.assert_screen_contains("alphaproj");
}

#[test]
fn ctrl_p_opens_palette_while_dock_focused_and_dock_stays() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // The dock is focused on mount. Ctrl+P must NOT be swallowed: it
    // blurs the dock and falls through to the editor's global binding,
    // opening the command palette. Prove the palette is live by typing a
    // query and seeing a built-in command surface — and the dock must
    // stay visible (non-modal) throughout.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Open File").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Open File"))
        .unwrap();
    h.assert_screen_contains("Open File");
    h.assert_screen_contains("ORCHESTRATOR");
}

#[test]
fn dock_list_order_is_stable_across_active_window_switch() {
    // Two sessions in *different* projects: switching the active window
    // changes the "current project", which the picker would float to the
    // top. The persistent dock must keep a stable order regardless.
    let (_tmp_a, root_a) = setup_project("aaa_project");
    let (_tmp_b, root_b) = {
        let temp_dir = tempfile::TempDir::new().unwrap();
        let root = temp_dir.path().join("zzz_project");
        fs::create_dir(&root).unwrap();
        let ok = std::process::Command::new("git")
            .args(["init", "-q"])
            .current_dir(&root)
            .status()
            .unwrap()
            .success();
        assert!(ok);
        (temp_dir, root)
    };

    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root_a.clone())
            .unwrap();
    // Second session in the other project (launch session is aaa_project).
    h.editor_mut()
        .create_window_at(root_b.clone(), "zzz_project".to_string());
    h.render().unwrap();
    open_dock(&mut h);

    // Both sessions show; aaa sorts above zzz.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("aaa_project") && s.contains("zzz_project")
    })
    .unwrap();
    let aaa_before = row_of(&h, "aaa_project");
    let zzz_before = row_of(&h, "zzz_project");
    assert!(aaa_before < zzz_before, "expected aaa above zzz initially");

    // Arrow down to the second row, which live-switches the active window
    // to the zzz project. Let the switch settle.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_until_stable(|h| h.screen_to_string().contains("zzz_project"))
        .unwrap();

    // Order must be unchanged — aaa still above zzz (the bug floated the
    // now-current zzz project to the top).
    let aaa_after = row_of(&h, "aaa_project");
    let zzz_after = row_of(&h, "zzz_project");
    assert!(
        aaa_after < zzz_after,
        "dock list reordered on switch: aaa now at {aaa_after}, zzz at {zzz_after}"
    );
}

#[test]
fn mouse_click_on_dock_new_button_opens_form() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Click the "+ New" button inside the dock column. A click landing on
    // a dock widget proves mouse hit-testing routes into the panel.
    let new_row = row_of(&h, "+ New") as u16;
    h.mouse_click(4, new_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Session"))
        .unwrap();
    h.assert_screen_contains("New Session");
}
