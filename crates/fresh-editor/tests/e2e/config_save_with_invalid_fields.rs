//! Saves to a config file that holds an invalid field (#3776): the loader
//! ignores the field with a warning, and saving another setting must still
//! write the file, leaving the invalid field for the user to fix.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Select Keybinding Map saves through `save_to_layer`, which parsed the
/// existing file strictly and so silently saved nothing.
#[test]
fn test_select_keybinding_map_saves_despite_invalid_field() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    harness.render().unwrap();
    let config_dir = harness
        .project_dir()
        .unwrap()
        .parent()
        .unwrap()
        .join("config");
    fs::create_dir_all(&config_dir).unwrap();
    let user_config_path = config_dir.join("config.json");
    fs::write(
        &user_config_path,
        r#"{"theme": "dark", "editor": {"tab_size": "four"}}"#,
    )
    .unwrap();

    harness
        .run_palette_command("Select Keybinding Map")
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("emacs").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    let saved: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(&user_config_path).unwrap()).unwrap();
    assert_eq!(saved["active_keybinding_map"], "emacs", "{saved}");
    assert_eq!(saved["theme"], "dark", "{saved}");
    assert_eq!(
        saved["editor"]["tab_size"], "four",
        "the invalid field is left for the user to fix: {saved}"
    );
}
