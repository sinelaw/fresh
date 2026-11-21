//! `registerFileOpener`: a plugin shows a file type itself, in place of the
//! editor's text or byte view.
//!
//! The test plugin (`tests/plugins/test_file_opener.ts`) registers a default
//! opener for `.fakeimg` and an "option" opener for `.fakeopt`; each writes
//! `VIEWER <opener> <file>` into its buffer. The files are binary, so the
//! editor's own view shows them as `[BIN]` with escaped bytes. Every
//! assertion is on the rendered screen.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::{Path, PathBuf};

const TEST_PLUGIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_file_opener.ts"
));

const TAB_BAR_ROW: u16 = 1;

/// Bytes no text view shows as such: a PNG-like header and NULs.
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
    project_root
}

fn harness(project_root: &Path, config: Config) -> EditorTestHarness {
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root.to_path_buf())
            .unwrap();
    harness.render().unwrap();
    harness
}

fn config_with_openers(entries: &[(&str, &str)]) -> Config {
    let mut config = Config::default();
    for (ext, opener) in entries {
        config
            .file_openers
            .insert(ext.to_string(), opener.to_string());
    }
    config
}

#[test]
fn a_file_type_a_plugin_claims_opens_in_its_viewer() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path());
    let mut harness = harness(&project_root, Config::default());

    harness
        .open_file(&project_root.join("photo.fakeimg"))
        .unwrap();
    harness
        .wait_for_screen_contains("VIEWER fake-viewer photo.fakeimg")
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("[BIN]"),
        "the plugin's viewer replaces the byte view; screen:\n{screen}"
    );
    assert!(
        screen.contains("Opened photo.fakeimg with Fake Viewer"),
        "the status bar names the opener; screen:\n{screen}"
    );
    let tabs = harness.screen_row_text(TAB_BAR_ROW);
    assert_eq!(
        tabs.matches("photo.fakeimg").count(),
        1,
        "one tab for the file, named after it; tab bar:\n{tabs}"
    );
}

#[test]
fn opening_the_file_again_shows_the_same_viewer_tab() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path());
    let mut harness = harness(&project_root, Config::default());
    let photo = project_root.join("photo.fakeimg");

    harness.open_file(&photo).unwrap();
    harness
        .wait_for_screen_contains("VIEWER fake-viewer photo.fakeimg")
        .unwrap();

    // Away to another buffer, then the same file again from the explorer.
    harness.new_buffer().unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("VIEWER fake-viewer"))
        .unwrap();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness
        .wait_for_file_explorer_item("photo.fakeimg")
        .unwrap();
    // Arrowing onto the file previews it, which shows the viewer tab again.
    for _ in 0..8 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        if harness
            .screen_to_string()
            .contains("VIEWER fake-viewer photo.fakeimg")
        {
            break;
        }
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_for_screen_contains("VIEWER fake-viewer photo.fakeimg")
        .unwrap();

    let tabs = harness.screen_row_text(TAB_BAR_ROW);
    assert_eq!(
        tabs.matches("photo.fakeimg").count(),
        1,
        "opening it again must not add a second tab; tab bar:\n{tabs}"
    );
}

#[test]
fn the_setting_opens_a_claimed_file_type_as_text() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path());
    let mut harness = harness(
        &project_root,
        config_with_openers(&[("fakeimg", "text"), ("fakeopt", "fake-option")]),
    );

    harness
        .open_file(&project_root.join("photo.fakeimg"))
        .unwrap();
    harness.wait_for_screen_contains("[BIN]").unwrap();
    // The handler would have run within a few ticks; give it every chance.
    for _ in 0..5 {
        harness.process_async_and_render().unwrap();
    }
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("VIEWER"),
        "\"text\" in file_openers keeps the editor's own view; screen:\n{screen}"
    );

    // Control: the openers are live in this session, so the byte view above
    // is the setting's doing.
    harness
        .open_file(&project_root.join("other.fakeopt"))
        .unwrap();
    harness
        .wait_for_screen_contains("VIEWER fake-option other.fakeopt")
        .unwrap();
}

#[test]
fn an_option_opener_is_used_only_when_the_setting_picks_it() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path());

    // Not picked: the file opens in the editor's own view.
    let mut harness_default = harness(&project_root, Config::default());
    harness_default
        .open_file(&project_root.join("other.fakeopt"))
        .unwrap();
    harness_default.wait_for_screen_contains("[BIN]").unwrap();
    for _ in 0..5 {
        harness_default.process_async_and_render().unwrap();
    }
    let screen = harness_default.screen_to_string();
    assert!(
        !screen.contains("VIEWER"),
        "an \"option\" opener does not take over its file type; screen:\n{screen}"
    );
    drop(harness_default);

    // Picked in the setting, it opens the file — even another type's.
    let mut harness = harness(
        &project_root,
        config_with_openers(&[("fakeopt", "fake-option"), ("bin", "fake-option")]),
    );
    harness
        .open_file(&project_root.join("other.fakeopt"))
        .unwrap();
    harness
        .wait_for_screen_contains("VIEWER fake-option other.fakeopt")
        .unwrap();

    fs::write(project_root.join("data.bin"), BINARY).unwrap();
    harness.open_file(&project_root.join("data.bin")).unwrap();
    harness
        .wait_for_screen_contains("VIEWER fake-option data.bin")
        .unwrap();
}
