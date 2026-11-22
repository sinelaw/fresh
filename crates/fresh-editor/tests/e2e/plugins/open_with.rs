//! The bundled Open With plugin: open a file as text, in a viewer another
//! plugin provides, or in the system's app, from the file explorer's and the
//! tabs' right-click menus and from the command palette.
//!
//! Uses a test viewer (`tests/plugins/test_open_with_viewer.ts`) that shows
//! `.fakeimg` files as the line `VIEWER <viewer> <file>`. Assertions are on
//! the rendered screen.

use crate::common::harness::{copy_plugin, copy_plugin_lib, layout, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

const TEST_VIEWER: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_open_with_viewer.ts"
));

const WIDTH: u16 = 120;
const HEIGHT: u16 = 30;

/// What the test viewer shows for `photo.fakeimg`.
const VIEWED: &str = "VIEWER fake-viewer photo.fakeimg";
/// The file's text, as the editor's own view shows it.
const TEXT: &str = "fake image text";

fn setup() -> (tempfile::TempDir, PathBuf, EditorTestHarness) {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = temp.path().join("project");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "open_with");
    fs::write(plugins_dir.join("test_open_with_viewer.ts"), TEST_VIEWER).unwrap();
    fs::write(project_root.join("photo.fakeimg"), format!("{TEXT}\n")).unwrap();
    fs::create_dir(project_root.join("folder")).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        WIDTH,
        HEIGHT,
        Config::default(),
        project_root.clone(),
    )
    .unwrap();
    // Open With loads before the viewer, which says so last.
    harness
        .wait_for_screen_contains("fake viewer ready")
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

fn open_file(harness: &mut EditorTestHarness, path: &Path) {
    harness.open_file(path).unwrap();
    harness.render().unwrap();
}

/// Run Open With… from the command palette, for the active tab's file.
fn palette_open_with(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Open With").unwrap();
    harness.wait_for_screen_contains("Open With…").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains(" with: ").unwrap();
}

/// Pick `choice` in the open picker: type to narrow the list to it, confirm.
fn pick(harness: &mut EditorTestHarness, choice: &str) {
    harness.type_text(choice).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

fn tabs_named(harness: &EditorTestHarness, name: &str) -> usize {
    harness.get_tab_bar().matches(name).count()
}

#[test]
fn the_explorer_offers_every_way_a_file_opens() {
    let (_temp, _root, mut harness) = setup();
    open_explorer(&mut harness, "photo.fakeimg");

    right_click_explorer_entry(&mut harness, "photo.fakeimg");
    harness.wait_for_screen_contains("Open With…").unwrap();
    click_text(&mut harness, "Open With…");

    harness
        .wait_for_screen_contains("Open photo.fakeimg with: ")
        .unwrap();
    let screen = harness.screen_to_string();
    for choice in ["Text Editor", "Fake Viewer", "System Default App"] {
        assert!(screen.contains(choice), "{choice} offered:\n{screen}");
    }

    pick(&mut harness, "Fake Viewer");
    harness.wait_for_screen_contains(VIEWED).unwrap();
}

#[test]
fn the_explorer_offers_it_on_files_only() {
    let (_temp, _root, mut harness) = setup();
    open_explorer(&mut harness, "folder");

    right_click_explorer_entry(&mut harness, "folder");
    harness.wait_for_screen_contains("Copy Full Path").unwrap();
    assert!(
        !harness.screen_to_string().contains("Open With…"),
        "Open With… on a folder:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn switching_views_keeps_one_tab_and_marks_the_current_view() {
    let (_temp, root, mut harness) = setup();
    open_file(&mut harness, &root.join("photo.fakeimg"));
    harness.wait_for_screen_contains(TEXT).unwrap();

    // From the palette, the file is shown as text: that is marked current.
    palette_open_with(&mut harness);
    let text_row = harness
        .find_text_on_screen("Text Editor")
        .map(|(_, row)| harness.screen_row_text(row))
        .unwrap();
    assert!(
        text_row.contains("current"),
        "text marked current: {text_row}"
    );
    pick(&mut harness, "Fake Viewer");
    harness.wait_for_screen_contains(VIEWED).unwrap();
    harness
        .wait_until(|h| tabs_named(h, "photo.fakeimg") == 1 && !h.screen_to_string().contains(TEXT))
        .unwrap();

    // From the viewer's tab, back to text: still one tab.
    right_click_tab(&mut harness, "photo.fakeimg");
    harness.wait_for_screen_contains("Open With…").unwrap();
    click_text(&mut harness, "Open With…");
    harness
        .wait_for_screen_contains("Open photo.fakeimg with: ")
        .unwrap();
    pick(&mut harness, "Text Editor");
    harness.wait_for_screen_contains(TEXT).unwrap();
    harness
        .wait_until(|h| {
            tabs_named(h, "photo.fakeimg") == 1 && !h.screen_to_string().contains(VIEWED)
        })
        .unwrap();
}

#[test]
fn unsaved_text_is_not_replaced_by_a_viewer() {
    let (_temp, root, mut harness) = setup();
    open_file(&mut harness, &root.join("photo.fakeimg"));
    harness.wait_for_screen_contains(TEXT).unwrap();
    harness.type_text("edited ").unwrap();
    harness.wait_for_screen_contains("edited ").unwrap();

    palette_open_with(&mut harness);
    pick(&mut harness, "Fake Viewer");
    harness
        .wait_for_screen_contains("photo.fakeimg has unsaved changes")
        .unwrap();
    assert!(
        !harness.screen_to_string().contains(VIEWED),
        "the viewer opened over unsaved text:\n{}",
        harness.screen_to_string()
    );
    assert!(harness.screen_to_string().contains("edited "));
}

#[test]
fn the_system_app_is_asked_to_open_the_file() {
    let (_temp, root, mut harness) = setup();
    // Stands in for the system: launches nothing, and succeeds only for the
    // file it should be asked to open.
    harness
        .editor_mut()
        .set_system_opener(Arc::new(|path: &Path| -> Result<(), String> {
            if path.ends_with("photo.fakeimg") {
                Ok(())
            } else {
                Err(format!("asked for {}", path.display()))
            }
        }));
    open_file(&mut harness, &root.join("photo.fakeimg"));

    palette_open_with(&mut harness);
    pick(&mut harness, "System Default App");
    harness
        .wait_for_screen_contains("Opened photo.fakeimg in the system's default app")
        .unwrap();
    // The file stays open as it was.
    assert!(harness.screen_to_string().contains(TEXT));
}

#[test]
fn a_failing_system_app_is_reported() {
    let (_temp, root, mut harness) = setup();
    harness
        .editor_mut()
        .set_system_opener(Arc::new(|_path: &Path| -> Result<(), String> {
            Err("no handler".to_string())
        }));
    open_file(&mut harness, &root.join("photo.fakeimg"));

    palette_open_with(&mut harness);
    pick(&mut harness, "System Default App");
    // The status bar cuts the reason off at this width.
    harness
        .wait_for_screen_contains("Could not open photo.fakeimg in the system's default")
        .unwrap();
}

#[test]
fn picking_an_open_viewer_closes_the_text_tab() {
    let (_temp, root, mut harness) = setup();
    let photo = root.join("photo.fakeimg");
    open_file(&mut harness, &photo);
    palette_open_with(&mut harness);
    pick(&mut harness, "Fake Viewer");
    harness.wait_for_screen_contains(VIEWED).unwrap();

    // Opening the file the usual way gives it a text tab beside the viewer.
    open_file(&mut harness, &photo);
    harness.wait_for_screen_contains(TEXT).unwrap();
    harness
        .wait_until(|h| tabs_named(h, "photo.fakeimg") == 2)
        .unwrap();

    // Back to the viewer it already has: the text tab goes.
    palette_open_with(&mut harness);
    pick(&mut harness, "Fake Viewer");
    harness.wait_for_screen_contains(VIEWED).unwrap();
    harness
        .wait_until(|h| tabs_named(h, "photo.fakeimg") == 1 && !h.screen_to_string().contains(TEXT))
        .unwrap();
}
