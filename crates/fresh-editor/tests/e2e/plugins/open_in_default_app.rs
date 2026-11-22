//! The bundled Open in Default App plugin: open a file in the app this
//! computer uses for its type, from the file explorer's and the tabs'
//! right-click menus and from the command palette.
//!
//! The test swaps the editor's opener for a stand-in that launches nothing
//! and succeeds only for the file it should be asked to open; assertions are
//! on the rendered screen.

use crate::common::context_menu::{click_text, right_click_explorer_entry, right_click_tab};
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

const WIDTH: u16 = 120;
const HEIGHT: u16 = 30;
const ITEM: &str = "Open in Default App";
const OPENED: &str = "Opened notes.txt in the system's default app";

/// Loads after the plugin under test (plugins in one folder load in name
/// order) and says so last: the editor applies plugin commands in order, so
/// once this is on screen the menu items are registered.
const READY_GATE: &str = r#"
const editor = getEditor();
editor.setStatus("plugins ready");
"#;

fn setup() -> (tempfile::TempDir, PathBuf, EditorTestHarness) {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = temp.path().join("project");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "open_in_default_app");
    fs::write(plugins_dir.join("zz_ready_gate.ts"), READY_GATE).unwrap();
    fs::write(project_root.join("notes.txt"), "some notes\n").unwrap();
    fs::create_dir(project_root.join("folder")).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        WIDTH,
        HEIGHT,
        Config::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.wait_for_screen_contains("plugins ready").unwrap();
    // As the frontends do once started; the plugin checks the window again.
    harness.editor_mut().fire_ready_hook();
    (temp, project_root, harness)
}

/// A stand-in for the system's launcher: launches nothing, and succeeds only
/// for the project's notes.txt, named by its full path.
fn expect_notes(harness: &mut EditorTestHarness, root: &Path) {
    let notes = root.join("notes.txt").canonicalize().unwrap();
    harness
        .editor_mut()
        .set_system_opener(Arc::new(move |path: &Path| -> Result<(), String> {
            if path == notes {
                Ok(())
            } else {
                Err(format!("asked for {}", path.display()))
            }
        }));
}

fn open_explorer(harness: &mut EditorTestHarness, item: &str) {
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item(item).unwrap();
}

fn open_file(harness: &mut EditorTestHarness, path: &Path) {
    harness.open_file(path).unwrap();
    harness.render().unwrap();
}

fn run_from_palette(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text(ITEM).unwrap();
    harness.wait_for_screen_contains(ITEM).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn the_explorer_opens_a_file_in_its_default_app() {
    let (_temp, root, mut harness) = setup();
    expect_notes(&mut harness, &root);
    open_explorer(&mut harness, "notes.txt");

    right_click_explorer_entry(&mut harness, "notes.txt");
    harness.wait_for_screen_contains(ITEM).unwrap();
    click_text(&mut harness, ITEM);
    harness.wait_for_screen_contains(OPENED).unwrap();
}

#[test]
fn the_explorer_offers_it_on_files_only() {
    let (_temp, _root, mut harness) = setup();
    open_explorer(&mut harness, "folder");

    right_click_explorer_entry(&mut harness, "folder");
    harness.wait_for_screen_contains("Copy Full Path").unwrap();
    assert!(
        !harness.screen_to_string().contains(ITEM),
        "offered on a folder:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn a_file_tab_opens_its_file_in_its_default_app() {
    let (_temp, root, mut harness) = setup();
    expect_notes(&mut harness, &root);
    open_file(&mut harness, &root.join("notes.txt"));

    right_click_tab(&mut harness, "notes.txt");
    harness.wait_for_screen_contains(ITEM).unwrap();
    click_text(&mut harness, ITEM);
    harness.wait_for_screen_contains(OPENED).unwrap();
}

#[test]
fn the_palette_opens_the_active_file_in_its_default_app() {
    let (_temp, root, mut harness) = setup();
    expect_notes(&mut harness, &root);
    open_file(&mut harness, &root.join("notes.txt"));

    run_from_palette(&mut harness);
    harness.wait_for_screen_contains(OPENED).unwrap();
}

#[test]
fn a_failing_default_app_is_reported() {
    let (_temp, root, mut harness) = setup();
    harness
        .editor_mut()
        .set_system_opener(Arc::new(|_path: &Path| -> Result<(), String> {
            Err("no handler".to_string())
        }));
    open_file(&mut harness, &root.join("notes.txt"));

    run_from_palette(&mut harness);
    // The status bar cuts the reason off at this width.
    harness
        .wait_for_screen_contains("Could not open notes.txt in the system's default")
        .unwrap();
}

#[test]
fn the_editor_keeps_working_while_the_launcher_runs() {
    let (_temp, root, mut harness) = setup();
    // Stands in for a launcher that stays until its app closes: it returns
    // only once the test lets it.
    let (release, released) = std::sync::mpsc::channel::<()>();
    let released = std::sync::Mutex::new(released);
    harness
        .editor_mut()
        .set_system_opener(Arc::new(move |_path: &Path| -> Result<(), String> {
            released
                .lock()
                .map_err(|e| e.to_string())?
                .recv()
                .map_err(|e| e.to_string())
        }));
    open_file(&mut harness, &root.join("notes.txt"));

    run_from_palette(&mut harness);
    harness
        .wait_for_screen_contains("Opening notes.txt")
        .unwrap();
    // The launcher is still running, and typing still reaches the buffer.
    harness.type_text("typed meanwhile ").unwrap();
    harness.wait_for_screen_contains("typed meanwhile").unwrap();
    assert!(
        !harness.screen_to_string().contains(OPENED),
        "reported opened before the launcher finished:\n{}",
        harness.screen_to_string()
    );

    release.send(()).unwrap();
    harness.wait_for_screen_contains(OPENED).unwrap();
}
