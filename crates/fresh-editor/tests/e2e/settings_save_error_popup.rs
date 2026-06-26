//! E2E: a failed settings-save must surface a prominent, centered modal popup
//! — not just a quiet status-bar line — and must leave the (unparseable)
//! config file untouched.
//!
//! Regression for the reported flow: a config the loader can't parse used to be
//! silently clobbered on save, and even after the no-clobber guard the only
//! feedback was a truncated status-bar message that's easy to miss.
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

fn send_text(harness: &mut EditorTestHarness, text: &str) {
    for c in text.chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
}

#[test]
fn failed_settings_save_shows_modal_and_keeps_file() {
    let mut harness = EditorTestHarness::with_temp_project(100, 40).unwrap();
    harness.render().unwrap();

    // The harness stores the user config at <temp>/config/config.json.
    let temp_dir = harness
        .project_dir()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let config_dir = temp_dir.join("config");
    fs::create_dir_all(&config_dir).unwrap();
    let user_config_path = config_dir.join("config.json");

    // A genuinely unparseable config file (missing commas/braces).
    let original = "{\n  \"editor\": {\n    \"tab_size\": 7\n    \"line_numbers\": broken\n";
    fs::write(&user_config_path, original).unwrap();

    // Change a setting and save.
    harness.open_settings().unwrap();
    assert!(harness.editor().is_settings_open());
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    send_text(&mut harness, "tab_size");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // jump to field
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // enter edit mode
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    send_text(&mut harness, "3"); // change tab_size -> 3
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // commit value
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // A prominent popup must be on screen with the failure title — not just the
    // status bar. The title only appears via the popup, so this distinguishes
    // it from the pre-existing status-bar-only behavior.
    harness.assert_screen_contains("Couldn't save settings");
    // And it reassures the user their file is safe.
    harness.assert_screen_contains("left unchanged");

    // The unparseable file must be byte-for-byte intact.
    assert_eq!(
        fs::read_to_string(&user_config_path).unwrap(),
        original,
        "a failed save must not modify the config file"
    );

    // Esc dismisses the modal.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Couldn't save settings");
}
