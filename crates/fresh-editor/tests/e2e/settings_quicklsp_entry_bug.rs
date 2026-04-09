//! E2E regression for the quicklsp Settings-UI entry bug.
//!
//! Reported symptom: when editing the `universal_lsp → quicklsp` entry
//! from Settings, saving writes the outer map key as `""` instead of
//! `"quicklsp"`, e.g.:
//!
//! ```json
//! {
//!   "universal_lsp": {
//!     "": [ ... ]
//!   }
//! }
//! ```
//!
//! Root cause: the nested ArrayItem dialog opened for an `is_single_value`
//! map entry (a map whose value schema is an array — like
//! `LspLanguageConfig`) used to compute its `map_path` as
//! `format!("{}/{}", parent.map_path, parent_item.path)` — dropping the
//! entry key segment whenever `parent_item.path` was `""`. The resulting
//! pending-change path `/universal_lsp/` then got written to disk via
//! `set_json_pointer`, which created an empty-string child key.
//!
//! This test opens Settings, drills into `universal_lsp/quicklsp`, opens
//! the nested item dialog for the existing server config, toggles
//! `Enabled`, saves, and asserts the on-disk `config.json` contains a
//! real `quicklsp` entry and NO empty-string sibling under `universal_lsp`.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use std::fs;
use tempfile::TempDir;

fn send_text(harness: &mut EditorTestHarness, text: &str) {
    for c in text.chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
}

/// Settings → universal_lsp → quicklsp round-trip must not corrupt the
/// outer map key.
#[test]
fn quicklsp_entry_save_preserves_outer_map_key() {
    // Isolated temp dir so the test doesn't touch the host config.
    let temp_dir = TempDir::new().unwrap();
    let config_dir = temp_dir.path().join("config");
    fs::create_dir_all(&config_dir).unwrap();
    let user_config_path = config_dir.join("config.json");

    // Start from an empty user config — the default `quicklsp` entry comes
    // from Config::default()'s built-in universal_lsp map.
    fs::write(&user_config_path, r#"{}"#).unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir_all(&project_root).unwrap();

    let mut harness = EditorTestHarness::create(
        160,
        50,
        HarnessOptions::new()
            .with_working_dir(project_root)
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.render().unwrap();

    // Open Settings.
    harness.open_settings().unwrap();

    // Search for "quicklsp" to jump straight to the universal_lsp map.
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    send_text(&mut harness, "quicklsp");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Navigate down through the universal_lsp map entries until the
    // quicklsp entry is focused (marked by the "[Enter to edit]" hint).
    let mut found_quicklsp = false;
    for _ in 0..30 {
        let screen = harness.screen_to_string();
        for line in screen.lines() {
            if line.contains("quicklsp") && line.contains("[Enter to edit]") {
                found_quicklsp = true;
                break;
            }
        }
        if found_quicklsp {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(
        found_quicklsp,
        "Could not focus the universal_lsp → quicklsp entry. Screen:\n{}",
        harness.screen_to_string()
    );

    // Press Enter to open the outer Edit Value dialog for quicklsp.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Value");
    harness.assert_screen_contains("Key:quicklsp");

    // The outer dialog focus lands on the ObjectArray (the single non-key
    // item for this is_single_value map entry) with its first entry
    // focused. Press Enter to drill into the nested Edit Item dialog.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Item");
    harness.assert_screen_contains("Command");

    // Navigate to the Enabled toggle and flip it. We need a real change so
    // that pending_changes is populated regardless of the buggy/fixed code
    // path — otherwise save_settings would short-circuit on `!has_changes`.
    let mut toggled = false;
    for _ in 0..12 {
        let screen = harness.screen_to_string();
        let enabled_focused = screen.lines().any(|line| {
            line.contains("Enabled") && (line.contains('>') || line.contains("[Space to toggle]"))
        });
        if enabled_focused {
            harness
                .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
                .unwrap();
            harness.render().unwrap();
            toggled = true;
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(
        toggled,
        "Could not reach the Enabled toggle inside the nested Edit Item dialog. \
         Screen:\n{}",
        harness.screen_to_string()
    );

    // Save the nested dialog (Ctrl+Enter saves the current dialog from
    // any mode — see input.rs handle_entry_dialog_input).
    harness
        .send_key(KeyCode::Enter, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Back at the outer dialog. Save it too so the whole entry flushes
    // through the normal save path.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Save the Settings page (Ctrl+S) and wait for it to close.
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..20 {
        harness.render().unwrap();
        if !harness.editor().is_settings_open() {
            break;
        }
    }
    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after Ctrl+S"
    );

    // Read the saved config and verify invariants on universal_lsp.
    let saved_content = fs::read_to_string(&user_config_path).unwrap();
    eprintln!("Saved config after quicklsp edit:\n{}", saved_content);
    let saved_json: serde_json::Value =
        serde_json::from_str(&saved_content).expect("saved config must be valid JSON");

    let universal_lsp = saved_json
        .get("universal_lsp")
        .and_then(|v| v.as_object())
        .expect("expected universal_lsp to be saved as an object after editing quicklsp");

    // CRITICAL: the outer map key must be "quicklsp", not "".
    assert!(
        !universal_lsp.contains_key(""),
        "BUG: universal_lsp contains an empty-string key. The settings \
         save flow dropped the 'quicklsp' key segment. Saved config:\n{}",
        saved_content
    );
    assert!(
        universal_lsp.contains_key("quicklsp"),
        "universal_lsp must contain the 'quicklsp' entry after editing it. \
         Saved config:\n{}",
        saved_content
    );

    drop(temp_dir);
}
