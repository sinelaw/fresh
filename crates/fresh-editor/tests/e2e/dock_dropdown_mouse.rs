//! The Orchestrator dock's dropdowns — the header's Menu and the row context
//! menu — its "Move to Folder…" pick, and dragging rows onto folders must be
//! usable with the mouse: clicking an
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
    h.wait_until(|h| h.screen_to_string().contains("+ New") && h.editor().is_dock_focused())
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

/// Open the header's Menu by clicking its glyph.
fn open_dock_menu(h: &mut EditorTestHarness) {
    let (mcol, mrow) = pos_of(h, "Menu ▾");
    h.mouse_click(mcol, mrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New folder…"))
        .unwrap();
}

/// Create a folder named `name` through the Menu (its first row), with
/// the "organize the current session under it" checkbox switched off, so
/// the folder starts empty.
fn create_empty_folder(h: &mut EditorTestHarness, name: &str) {
    open_dock_menu(h);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Folder name"))
        .unwrap();
    h.type_text(name).unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    // Ctrl+Enter submits from anywhere in the dialog; a plain Enter here
    // would be the focused checkbox's, toggling it back on.
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("Folder name") && s.contains(name)
    })
    .unwrap();
}

/// "Move to Folder…" from a row's menu: the dock asks for the target — a
/// banner names what is moving — and waits for a folder to be clicked.
fn start_move_pick(h: &mut EditorTestHarness, session: &str) {
    let session_row = row_of(h, session);
    h.mouse_right_click(4, session_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Move to Folder"))
        .unwrap();
    let (mcol, mrow) = pos_of(h, "Move to Folder");
    h.mouse_click(mcol, mrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("click a folder"))
        .unwrap();
}

/// Clicking a folder in the tree while the dock asks for a target files the
/// session into it — the same outcome Enter on the folder produces.
#[test]
fn move_to_folder_pick_files_into_the_clicked_folder() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    create_empty_folder(&mut h, "Docs");
    start_move_pick(&mut h, "alphaproj");

    let (dcol, drow) = pos_of(&h, "Docs");
    h.mouse_click(dcol, drow).unwrap();

    // The folder now reports one member: the session was filed into it,
    // and the banner is gone behind the pick.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Docs") && s.contains("(1)") && !s.contains("click a folder")
    })
    .unwrap();
}

/// While the dock asks for a folder, a workspace row is not a target: a
/// click on it files nothing and the pick stays up. Esc then cancels it,
/// leaving everything where it was.
#[test]
fn move_to_folder_pick_ignores_workspaces_and_esc_cancels() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    create_empty_folder(&mut h, "Docs");
    start_move_pick(&mut h, "alphaproj");

    let (scol, srow) = pos_of(&h, "alphaproj");
    h.mouse_click(scol, srow).unwrap();
    for _ in 0..5 {
        h.render().unwrap();
    }
    let screen = h.screen_to_string();
    assert!(
        screen.contains("click a folder") && !screen.contains("(1)"),
        "a click on a workspace is not a target:\n{screen}"
    );

    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("click a folder"))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(!screen.contains("(1)"), "Esc files nothing:\n{screen}");
}

/// **A workspace row dragged onto a folder is filed into it** — the mouse's
/// way to do what Move to Folder does.
#[test]
fn dragging_a_workspace_onto_a_folder_files_it() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    create_empty_folder(&mut h, "Docs");

    let (scol, srow) = pos_of(&h, "alphaproj");
    let (_, drow) = pos_of(&h, "Docs");
    h.mouse_drag(scol, srow, scol, drow).unwrap();

    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Docs") && s.contains("(1)")
    })
    .unwrap();
    let (_, drow) = pos_of(&h, "Docs");
    assert_eq!(
        pos_of(&h, "alphaproj").1,
        drow + 1,
        "the workspace sits under Docs:\n{}",
        h.screen_to_string()
    );
}

/// **A drag that ends off every row files nothing**: released in the editor,
/// the workspace stays where it was.
#[test]
fn a_workspace_dragged_off_the_tree_stays_put() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    create_empty_folder(&mut h, "Docs");

    let (scol, srow) = pos_of(&h, "alphaproj");
    h.mouse_drag(scol, srow, 90, srow).unwrap();
    for _ in 0..5 {
        h.render().unwrap();
    }
    let screen = h.screen_to_string();
    assert!(!screen.contains("(1)"), "nothing was filed:\n{screen}");
    assert!(
        pos_of(&h, "alphaproj").1 < pos_of(&h, "Docs").1,
        "the workspace is still at the top level:\n{screen}"
    );
}

/// Clicking an option in the Menu activates it.
///
/// Not a reproducer — this dropdown anchors high enough in the dock that
/// the old base-row byte mapping happened to line up, so it kept working
/// while the move menu did not. It guards the sibling path against the
/// same class of drift.
#[test]
fn dock_menu_option_is_clickable() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    open_dock_menu(&mut h);

    let (fcol, frow) = pos_of(&h, "New folder…");
    h.mouse_click(fcol, frow).unwrap();

    // "New Folder…" opens the folder-creation dialog.
    h.wait_until(|h| h.screen_to_string().contains("Folder name"))
        .unwrap();
}

/// Clicking away from the dock while it asks for a folder cancels the pick,
/// the way clicking away dismisses any menu — here, a click out in the
/// editor area.
#[test]
fn move_to_folder_pick_ends_on_click_outside() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    create_empty_folder(&mut h, "Docs");
    start_move_pick(&mut h, "alphaproj");

    h.mouse_click(90, 20).unwrap();

    // The banner is gone, nothing was filed, and the dock is still there.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("click a folder") && s.contains("+ New")
    })
    .unwrap();
    let screen = h.screen_to_string();
    assert!(
        !screen.contains("(1)"),
        "a click away files nothing:\n{screen}"
    );
}

/// A dropdown is opaque: a click on its frame — inside the popup but on no
/// option — is swallowed rather than reaching the session tree it covers.
/// Byte-exact hit-testing against the wrong row's text used to let such a
/// click fall through and live-switch the workspace behind the menu.
#[test]
fn dock_dropdown_swallows_clicks_on_its_own_frame() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    open_dock_menu(&mut h);
    let focused_before = h.editor().is_dock_focused();

    // The popup's left border on the row of its first option.
    let (fcol, frow) = pos_of(&h, "New folder…");
    let line = h
        .screen_to_string()
        .lines()
        .nth(frow as usize)
        .unwrap()
        .to_string();
    let border = line
        .chars()
        .take(fcol as usize)
        .collect::<Vec<_>>()
        .iter()
        .rposition(|&c| c == '│')
        .unwrap_or_else(|| panic!("no popup border left of the option:\n{line}"))
        as u16;
    h.mouse_click(border, frow).unwrap();
    for _ in 0..5 {
        h.render().unwrap();
    }

    // Nothing happened: the menu is still up, no option was picked, and
    // the dock did not dive into a session.
    let screen = h.screen_to_string();
    assert!(
        screen.contains("New folder…") && !screen.contains("Folder name"),
        "a click on the popup frame must leave the menu open, unpicked; screen:\n{screen}"
    );
    assert_eq!(
        h.editor().is_dock_focused(),
        focused_before,
        "a click on the popup frame must not reach the tree behind it and \
         dive out of the dock; screen:\n{screen}"
    );
}

/// **A press outside the Menu closes it**, the way the right-click menu
/// closes: the Menu is an anchored panel, and an anchored panel spends the
/// outside press on its dismissal. A press in the editor closes the Menu;
/// then `+ New` answers as usual.
#[test]
fn dock_menu_closes_on_a_press_outside() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    // The Menu hangs over the action row, so find `+ New` before it opens.
    let (ncol, nrow) = pos_of(&h, "+ New");
    open_dock_menu(&mut h);

    // A press in the editor, well clear of the Menu.
    h.mouse_click(110, 28).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("New folder…"))
        .unwrap();

    h.mouse_click(ncol + 2, nrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Folder:"))
        .unwrap();
}

/// **`Menu ▾` with its menu up closes it, and leaves it closed.** The press
/// arrives twice — once as the layer's dismissal, once as the button's own
/// activation — and the second must not reopen what the first shut.
#[test]
fn pressing_the_dock_menu_glyph_again_closes_the_menu() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = launch(root);
    open_dock_menu(&mut h);

    let (mcol, mrow) = pos_of(&h, "Menu ▾");
    h.mouse_click(mcol, mrow).unwrap();

    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("New folder…") && s.contains("+ New")
    })
    .unwrap();
}
