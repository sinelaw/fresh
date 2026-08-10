//! The Orchestrator dock's dropdowns — "New Task… ▾" and the row context
//! menu's "Move to Folder…" — must be usable with the mouse: clicking an
//! option picks it, and clicking away dismisses the menu.
//!
//! Regression: both dropdowns render as an `Overlay`, a popup the widget
//! renderer paints *over* the rows beneath it without reflowing them. The
//! floating-panel click handler mapped the clicked screen column to a byte
//! offset using the text of the row *underneath* the popup — the divider
//! and session-tree rows it covers — while the overlay's own hit areas were
//! measured against the popup's text. The two coordinate spaces never
//! agreed, so option buttons were unreachable and the click fell through to
//! whatever sat behind the menu. And nothing dismissed the dropdown either:
//! it stayed pinned over the dock until a keyboard Esc.
//!
//! Per CONTRIBUTING §2 these drive only keyboard/mouse and assert on
//! rendered output.

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

/// Toggle the dock open via the command palette and wait for it to render
/// *and* take keyboard focus (the plugin sets focus asynchronously).
fn open_dock(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator") && h.editor().is_dock_focused())
        .unwrap();
}

/// 0-based screen row containing `needle`, or panic with the screen.
fn row_of(h: &EditorTestHarness, needle: &str) -> u16 {
    let screen = h.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{screen}")) as u16
}

/// 0-based screen position (col, row) of the first occurrence of `needle`.
/// `str::find` returns a *byte* offset, but dock rows contain multibyte
/// box-drawing glyphs, so convert to a character column first.
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

fn launch(root: PathBuf) -> EditorTestHarness {
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h
}

/// Open the "New Task… ▾" dropdown by clicking its button.
fn open_new_task_dropdown(h: &mut EditorTestHarness) {
    let new_row = row_of(h, "New Task");
    h.mouse_click(4, new_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Folder"))
        .unwrap();
}

/// Create a folder named `name` through the "New Task… ▾" dropdown, with
/// the "organize the current session under it" checkbox switched off, so
/// the folder starts empty.
fn create_empty_folder(h: &mut EditorTestHarness, name: &str) {
    open_new_task_dropdown(h);
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Folder name"))
        .unwrap();
    h.type_text(name).unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("Folder name") && s.contains(name)
    })
    .unwrap();
}

/// Right-click the session row and pick "Move to Folder…" so the move
/// dropdown is showing over the dock.
fn open_move_dropdown(h: &mut EditorTestHarness, session: &str) {
    let session_row = row_of(h, session);
    h.mouse_right_click(4, session_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Move to Folder"))
        .unwrap();
    let (mcol, mrow) = pos_of(h, "Move to Folder");
    h.mouse_click(mcol, mrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Top level"))
        .unwrap();
}

/// Clicking a folder in the "Move to Folder…" dropdown files the session
/// into it — the same outcome ↓/Enter produces.
#[test]
fn move_to_folder_dropdown_option_is_clickable() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    create_empty_folder(&mut h, "Docs");
    open_move_dropdown(&mut h, "alphaproj");

    let (dcol, drow) = pos_of(&h, "Docs");
    h.mouse_click(dcol, drow).unwrap();

    // The folder now reports one member: the session was filed into it,
    // and the dropdown closed behind the pick.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Docs") && s.contains("(1)") && !s.contains("Top level")
    })
    .unwrap();
}

/// Clicking an option in the "New Task… ▾" dropdown activates it.
///
/// Not a reproducer — this dropdown anchors high enough in the dock that
/// the old base-row byte mapping happened to line up, so it kept working
/// while the move menu did not. It guards the sibling path against the
/// same class of drift.
#[test]
fn new_task_dropdown_option_is_clickable() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    open_new_task_dropdown(&mut h);

    let (fcol, frow) = pos_of(&h, "New Folder");
    h.mouse_click(fcol, frow).unwrap();

    // "New Folder…" opens the folder-creation dialog.
    h.wait_until(|h| h.screen_to_string().contains("Folder name"))
        .unwrap();
}

/// Clicking away from an open dropdown dismisses it, the way any menu
/// behaves — here, a click out in the editor area.
#[test]
fn dock_dropdown_dismisses_on_click_outside() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    create_empty_folder(&mut h, "Docs");
    open_move_dropdown(&mut h, "alphaproj");

    h.mouse_click(90, 20).unwrap();

    // The menu is gone and the dock is still there behind it.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("Top level") && s.contains("New Task")
    })
    .unwrap();
}

/// A dropdown is opaque: a click on its frame — inside the popup but on no
/// option — is swallowed rather than reaching the session tree it covers.
/// Byte-exact hit-testing against the wrong row's text used to let such a
/// click fall through and live-switch the workspace behind the menu.
#[test]
fn dock_dropdown_swallows_clicks_on_its_own_frame() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    create_empty_folder(&mut h, "Docs");
    open_move_dropdown(&mut h, "alphaproj");

    // Column 0 of an option's row is the popup's left border.
    let (_, drow) = pos_of(&h, "Docs");
    h.mouse_click(0, drow).unwrap();

    // Nothing happened: the menu is still up, still unpicked (the folder
    // has no members), and the dock did not dive into a session.
    let screen = h.screen_to_string();
    assert!(
        screen.contains("Top level") && screen.contains("Docs"),
        "a click on the popup frame must leave the menu open; screen:\n{screen}"
    );
    assert!(
        !screen.contains("(1)"),
        "a click on the popup frame must not pick an option; screen:\n{screen}"
    );
    assert!(
        h.editor().is_dock_focused(),
        "a click on the popup frame must not reach the tree behind it and \
         dive out of the dock; screen:\n{screen}"
    );
}
