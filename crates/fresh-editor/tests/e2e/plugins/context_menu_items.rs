//! Plugin items in the file explorer's and the tabs' right-click menus
//! (`addFileExplorerMenuItem` / `addTabMenuItem`): offered only where their
//! scope says, and calling their handler with what the menu was opened on.
//!
//! The test plugin's handlers write what they were called with to the status
//! bar, so every assertion is on the rendered screen.

use crate::common::harness::{copy_plugin_lib, layout, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::{Path, PathBuf};

const TEST_PLUGIN: &str = r###"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

registerHandler("ctx_folder", (t: FileExplorerMenuTarget) => {
  editor.setStatus(`FOLDER ${t.kind} ${editor.pathBasename(t.path.value)}`);
});
registerHandler("ctx_file_tab", (t: TabMenuTarget) => {
  editor.setStatus(`FILE TAB ${t.path === null ? "none" : editor.pathBasename(t.path.value)}`);
});

editor.addFileExplorerMenuItem({
  label: "Inspect This Folder From A Plugin",
  scope: "directory",
  handler: "ctx_folder",
});
editor.addTabMenuItem({
  label: "Name The File",
  scope: "fileTab",
  handler: "ctx_file_tab",
});

// An item that depends on a context, and a command that sets it.
registerHandler("ctx_needs", (t: FileExplorerMenuTarget) => {
  editor.setStatus(`NEEDS ${editor.pathBasename(t.path.value)}`);
});
editor.addFileExplorerMenuItem({
  label: "Needs The Context",
  scope: "file",
  handler: "ctx_needs",
  when: "test-menu-context",
});
registerHandler("ctx_set", () => {
  editor.setContext("test-menu-context", true);
  editor.setStatus("context set");
});
editor.registerCommand("Test Set Menu Context", "", "ctx_set");

// A tab that shows no file.
editor.createVirtualBuffer({ name: "*panel*", readOnly: true, background: true });

// Last: the editor applies a plugin's commands in order, so once this is on
// screen the items above are registered.
editor.setStatus("menu items ready");
"###;

const WIDTH: u16 = 120;
const HEIGHT: u16 = 30;

fn setup() -> (tempfile::TempDir, PathBuf, EditorTestHarness) {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = temp.path().join("project");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("context_menu_items_test.ts"), TEST_PLUGIN).unwrap();
    fs::create_dir(project_root.join("folder")).unwrap();
    fs::write(project_root.join("notes.txt"), "hello\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        WIDTH,
        HEIGHT,
        Config::default(),
        project_root.clone(),
    )
    .unwrap();
    harness
        .wait_for_screen_contains("menu items ready")
        .unwrap();
    (temp, project_root, harness)
}

fn open_explorer(harness: &mut EditorTestHarness, item: &str) {
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item(item).unwrap();
}

/// Right-click `name` where the explorer draws it: the leftmost place it is
/// drawn below the tab bar.
fn right_click_explorer_entry(harness: &mut EditorTestHarness, name: &str) {
    let (col, row) = (layout::TAB_BAR_ROW as u16 + 1..HEIGHT)
        .filter_map(|row| {
            let line = harness.screen_row_text(row);
            let byte = line.find(name)?;
            Some((line[..byte].chars().count() as u16, row))
        })
        .min()
        .unwrap_or_else(|| panic!("{name} not on screen:\n{}", harness.screen_to_string()));
    harness.mouse_right_click(col + 1, row).unwrap();
    harness.render().unwrap();
}

fn right_click_tab(harness: &mut EditorTestHarness, name: &str) {
    let bar = harness.get_tab_bar();
    let byte = bar
        .find(name)
        .unwrap_or_else(|| panic!("{name} not in the tab bar: {bar}"));
    let col = bar[..byte].chars().count() as u16;
    harness
        .mouse_right_click(col + 1, layout::TAB_BAR_ROW as u16)
        .unwrap();
    harness.render().unwrap();
}

fn click_text(harness: &mut EditorTestHarness, text: &str) {
    let (col, row) = harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{text} not on screen:\n{}", harness.screen_to_string()));
    harness.mouse_click(col + 1, row).unwrap();
    harness.render().unwrap();
}

fn dismiss_menu(harness: &mut EditorTestHarness) {
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

fn open_file(harness: &mut EditorTestHarness, path: &Path) {
    harness.open_file(path).unwrap();
    harness.render().unwrap();
}

#[test]
fn a_folder_item_is_offered_on_folders_and_gets_the_folder() {
    let (_temp, _root, mut harness) = setup();
    open_explorer(&mut harness, "folder");

    // Not on a file: the item's scope is "directory".
    right_click_explorer_entry(&mut harness, "notes.txt");
    harness.wait_for_screen_contains("Copy Full Path").unwrap();
    assert!(
        !harness.screen_to_string().contains("Inspect This Folder"),
        "a folder item on a file:\n{}",
        harness.screen_to_string()
    );
    dismiss_menu(&mut harness);

    // On a folder, the whole label shows although it is wider than the
    // editor's own items.
    right_click_explorer_entry(&mut harness, "folder");
    harness
        .wait_for_screen_contains("Inspect This Folder From A Plugin")
        .unwrap();
    click_text(&mut harness, "Inspect This Folder From A Plugin");
    harness
        .wait_for_screen_contains("FOLDER directory folder")
        .unwrap();
}

#[test]
fn a_file_tab_item_is_offered_on_file_tabs_and_gets_the_file() {
    let (_temp, root, mut harness) = setup();
    open_file(&mut harness, &root.join("notes.txt"));

    // Not on a tab that shows no file.
    harness.wait_for_screen_contains("*panel*").unwrap();
    right_click_tab(&mut harness, "*panel*");
    harness.wait_for_screen_contains("Close Others").unwrap();
    assert!(
        !harness.screen_to_string().contains("Name The File"),
        "a file-tab item on a panel's tab:\n{}",
        harness.screen_to_string()
    );
    dismiss_menu(&mut harness);

    right_click_tab(&mut harness, "notes.txt");
    harness.wait_for_screen_contains("Name The File").unwrap();
    click_text(&mut harness, "Name The File");
    harness
        .wait_for_screen_contains("FILE TAB notes.txt")
        .unwrap();
}

#[test]
fn an_item_whose_context_is_not_set_is_shown_but_does_nothing() {
    let (_temp, _root, mut harness) = setup();
    open_explorer(&mut harness, "notes.txt");

    // Shown, but choosing it neither runs it nor closes the menu.
    right_click_explorer_entry(&mut harness, "notes.txt");
    harness
        .wait_for_screen_contains("Needs The Context")
        .unwrap();
    click_text(&mut harness, "Needs The Context");
    assert!(
        harness.screen_to_string().contains("Copy Full Path"),
        "the menu closed:\n{}",
        harness.screen_to_string()
    );
    assert!(!harness.screen_to_string().contains("NEEDS notes.txt"));
    dismiss_menu(&mut harness);

    // Once the plugin sets the context, it runs.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Test Set Menu Context").unwrap();
    harness
        .wait_for_screen_contains("Test Set Menu Context")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("context set").unwrap();

    right_click_explorer_entry(&mut harness, "notes.txt");
    harness
        .wait_for_screen_contains("Needs The Context")
        .unwrap();
    click_text(&mut harness, "Needs The Context");
    harness.wait_for_screen_contains("NEEDS notes.txt").unwrap();
}
