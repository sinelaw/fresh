//! "Open With…": open a file as text, with a plugin file opener, or in the
//! system's default app, from the file explorer's and the tab bar's
//! context menus and from the command palette.
//!
//! Uses the file-opener test plugin (`tests/plugins/test_file_opener.ts`):
//! `.fakeimg` opens by default in "Fake Viewer", and "Fake Option" handles
//! `.fakeopt` only when picked. Both write `VIEWER <opener> <file>` into
//! their buffer; the editor's own view of these binary files shows `[BIN]`.
//! Assertions are on the rendered screen, plus what the test's own stand-in
//! for the system app was asked to open.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

const TEST_PLUGIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_file_opener.ts"
));

const WIDTH: u16 = 120;
const HEIGHT: u16 = 30;
const TAB_BAR_ROW: u16 = 1;
/// The explorer takes the left 30% of the width.
const EXPLORER_WIDTH: u16 = 36;

const BINARY: &[u8] = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x01\x02\x03";

fn setup_project(temp: &Path) -> PathBuf {
    let project_root = temp.join("project");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("test_file_opener.ts"), TEST_PLUGIN).unwrap();
    fs::write(project_root.join("photo.fakeimg"), BINARY).unwrap();
    fs::write(project_root.join("other.fakeopt"), BINARY).unwrap();
    fs::create_dir(project_root.join("folder")).unwrap();
    project_root
}

fn harness(project_root: &Path) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        WIDTH,
        HEIGHT,
        Config::default(),
        project_root.to_path_buf(),
    )
    .unwrap();
    harness.render().unwrap();
    harness
}

fn open_explorer(harness: &mut EditorTestHarness, item: &str) {
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item(item).unwrap();
}

/// Where `text` is drawn in the explorer column.
fn explorer_row_of(harness: &EditorTestHarness, text: &str) -> (u16, u16) {
    (0..HEIGHT)
        .find_map(|row| {
            let line = harness.screen_row_text(row);
            let col = line.find(text)?;
            let col = line[..col].chars().count() as u16;
            (col < EXPLORER_WIDTH).then_some((col, row))
        })
        .unwrap_or_else(|| {
            panic!(
                "{text} not in the explorer:\n{}",
                harness.screen_to_string()
            )
        })
}

fn click_text(harness: &mut EditorTestHarness, text: &str) {
    let (col, row) = harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{text} not on screen:\n{}", harness.screen_to_string()));
    harness.mouse_click(col + 1, row).unwrap();
    harness.render().unwrap();
}

/// Type into the open "Open With…" prompt to pick a choice, and confirm.
fn pick(harness: &mut EditorTestHarness, choice: &str) {
    harness.type_text(choice).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

fn palette_open_with(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Open With").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("with: ").unwrap();
}

fn tab_count(harness: &EditorTestHarness, name: &str) -> usize {
    harness.screen_row_text(TAB_BAR_ROW).matches(name).count()
}

#[test]
fn the_explorer_opens_a_viewed_file_as_text_in_its_place() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path());
    let mut harness = harness(&project_root);
    harness
        .open_file(&project_root.join("photo.fakeimg"))
        .unwrap();
    harness
        .wait_for_screen_contains("VIEWER fake-viewer photo.fakeimg")
        .unwrap();

    open_explorer(&mut harness, "photo.fakeimg");
    let (col, row) = explorer_row_of(&harness, "photo.fakeimg");
    harness.mouse_right_click(col + 1, row).unwrap();
    harness.render().unwrap();
    click_text(&mut harness, "Open With…");

    // Every way the file can open, the current one marked.
    harness
        .wait_for_screen_contains("Open photo.fakeimg with: ")
        .unwrap();
    let screen = harness.screen_to_string();
    for choice in [
        "Text Editor",
        "Fake Viewer",
        "System Default App",
        "Always open .fakeimg files with Text Editor",
    ] {
        assert!(
            screen.contains(choice),
            "{choice} offered; screen:\n{screen}"
        );
    }
    assert!(
        !screen.contains("Always open .fakeimg files with Fake Viewer"),
        "no \"always\" for what already opens it; screen:\n{screen}"
    );

    pick(&mut harness, "Text Editor");
    harness.wait_for_screen_contains("[BIN]").unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("VIEWER"),
        "the viewer tab is replaced; screen:\n{screen}"
    );
    assert_eq!(
        tab_count(&harness, "photo.fakeimg"),
        1,
        "one tab for the file"
    );
}

#[test]
fn a_folder_has_no_open_with() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path());
    let mut harness = harness(&project_root);

    open_explorer(&mut harness, "folder");
    let (col, row) = explorer_row_of(&harness, "folder");
    harness.mouse_right_click(col + 1, row).unwrap();
    harness.render().unwrap();
    harness.wait_for_screen_contains(" Paste ").unwrap();
    assert!(
        !harness.screen_to_string().contains("Open With"),
        "a folder's menu offers no Open With:\n{}",
        harness.screen_to_string()
    );

    // Control: a file's menu does.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (col, row) = explorer_row_of(&harness, "photo.fakeimg");
    harness.mouse_right_click(col + 1, row).unwrap();
    harness.render().unwrap();
    harness.wait_for_screen_contains("Open With…").unwrap();
}

#[test]
fn the_tab_menu_opens_a_file_with_an_option_opener() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path());
    let mut harness = harness(&project_root);
    harness
        .open_file(&project_root.join("other.fakeopt"))
        .unwrap();
    harness.wait_for_screen_contains("[BIN]").unwrap();

    let (col, _) = harness
        .find_text_on_screen("other.fakeopt")
        .expect("the file's tab");
    harness.mouse_right_click(col + 1, TAB_BAR_ROW).unwrap();
    harness.render().unwrap();
    click_text(&mut harness, "Open With…");
    harness
        .wait_for_screen_contains("Open other.fakeopt with: ")
        .unwrap();

    pick(&mut harness, "Fake Option");
    harness
        .wait_for_screen_contains("VIEWER fake-option other.fakeopt")
        .unwrap();
    assert_eq!(
        tab_count(&harness, "other.fakeopt"),
        1,
        "one tab for the file"
    );
}

#[test]
fn always_remembers_the_choice_for_the_extension() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path());
    let mut harness = harness(&project_root);
    let photo = project_root.join("photo.fakeimg");
    harness.open_file(&photo).unwrap();
    harness
        .wait_for_screen_contains("VIEWER fake-viewer photo.fakeimg")
        .unwrap();

    palette_open_with(&mut harness);
    pick(&mut harness, "Always open");
    harness.wait_for_screen_contains("[BIN]").unwrap();

    // Another file of the type opens as text too: the choice stuck.
    let second = project_root.join("second.fakeimg");
    fs::write(&second, BINARY).unwrap();
    harness.open_file(&second).unwrap();
    harness
        .wait_until(|h| {
            h.screen_row_text(TAB_BAR_ROW).contains("second.fakeimg")
                && h.screen_to_string().contains("[BIN]")
        })
        .unwrap();
    for _ in 0..5 {
        harness.process_async_and_render().unwrap();
    }
    assert!(
        !harness
            .screen_to_string()
            .contains("VIEWER fake-viewer second.fakeimg"),
        "the remembered choice applies to the next file of the type:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn the_system_app_is_asked_to_open_the_file() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path());
    let mut harness = harness(&project_root);
    let opened: Arc<Mutex<Vec<PathBuf>>> = Arc::default();
    let record = Arc::clone(&opened);
    harness
        .editor_mut()
        .set_system_opener(Arc::new(move |path: &Path| {
            record.lock().unwrap().push(path.to_path_buf());
            Ok(())
        }));
    let photo = project_root.join("photo.fakeimg");
    harness.open_file(&photo).unwrap();
    harness
        .wait_for_screen_contains("VIEWER fake-viewer photo.fakeimg")
        .unwrap();

    palette_open_with(&mut harness);
    pick(&mut harness, "System Default App");
    // (The status bar cuts the end of the message at this width.)
    harness
        .wait_for_screen_contains("Opened photo.fakeimg in the system's")
        .unwrap();

    let opened = opened.lock().unwrap();
    assert_eq!(opened.len(), 1, "asked once: {opened:?}");
    assert_eq!(
        opened[0].file_name().and_then(|n| n.to_str()),
        Some("photo.fakeimg")
    );
    // The viewer is still there: the system app opens outside the editor.
    harness.assert_screen_contains("VIEWER fake-viewer photo.fakeimg");
}
