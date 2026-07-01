//! E2E: in the language entry dialog (Settings → Languages → <lang>), the
//! platform-standard edit convention holds for every editable field type —
//! **Enter/Tab commit** the typed value, **Esc reverts** it to what it was
//! before the edit began. This matches Windows, macOS, and the web, where Esc
//! cancels an in-progress edit rather than accepting it.
//!
//! Pre-change the dialog treated Esc the same as Tab (accept), so these Esc
//! assertions fail without the fix. Tests drive only keyboard events and assert
//! on rendered output, per CONTRIBUTING.md ("E2E Tests Observe, Not Inspect").

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// A config with a single `typescript` language entry so the Settings
/// "Languages" map is deterministic to navigate.
fn typescript_only_config() -> Config {
    let mut config = Config::default();
    config.languages.retain(|name, _| name == "typescript");
    assert!(
        config.languages.contains_key("typescript"),
        "precondition: typescript ships as a built-in language"
    );
    config
}

/// Return the full text of the first rendered row that contains `needle`.
fn row_with(harness: &EditorTestHarness, needle: &str) -> String {
    harness
        .screen_to_string()
        .lines()
        .find(|line| line.contains(needle))
        .unwrap_or("")
        .to_string()
}

/// True when the row showing `label` carries the dialog's focus marker (`>`).
fn row_is_focused(harness: &EditorTestHarness, label: &str) -> bool {
    let row = row_with(harness, label);
    row.split(label).next().unwrap_or("").contains('>')
}

/// Open the single language entry's Edit Value dialog. Leaves it open.
fn open_language_dialog(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    for _ in 0..60 {
        if harness.screen_to_string().contains("[Enter to edit]") {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(
        harness.screen_to_string().contains("[Enter to edit]"),
        "language map row should be focused with an edit affordance"
    );
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("Edit Value"),
        "language entry dialog should open; screen was:\n{}",
        harness.screen_to_string()
    );
}

/// Walk the dialog's field focus down until `label`'s row is the focused one.
fn focus_field(harness: &mut EditorTestHarness, label: &str) {
    for _ in 0..80 {
        if row_is_focused(harness, label) {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    panic!(
        "could not focus the {label:?} field; screen was:\n{}",
        harness.screen_to_string()
    );
}

/// Esc while editing a number field (Tab Size) discards the typed digits and
/// restores the inherited value — including the `(Inherited)` badge, proving the
/// field's whole pre-edit state came back, not just the number.
#[test]
fn esc_reverts_number_field_edit() {
    let mut harness = EditorTestHarness::with_config(140, 40, typescript_only_config()).unwrap();
    harness.render().unwrap();

    open_language_dialog(&mut harness);
    focus_field(&mut harness, "Tab Size");
    assert!(
        row_with(&harness, "Tab Size").contains("(Inherited)"),
        "precondition: Tab Size starts inherited; row: {:?}",
        row_with(&harness, "Tab Size")
    );

    // Type a new value...
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("8").unwrap();
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Tab Size").contains("8 ]"),
        "the edit buffer should show the typed 8; row: {:?}",
        row_with(&harness, "Tab Size")
    );

    // ...then Esc — it must revert to the inherited 0, badge and all.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let row = row_with(&harness, "Tab Size");
    assert!(
        row.contains("0 ]") && !row.contains("8"),
        "Esc must revert the number to inherited 0; row: {:?}",
        row
    );
    assert!(
        row.contains("(Inherited)") && !row.contains("[Inherit]"),
        "Esc must restore the inherited state (badge, not override); row: {:?}",
        row
    );
}

/// Esc while editing a text field (Grammar) discards the typed characters and
/// restores the original string.
#[test]
fn esc_reverts_text_field_edit() {
    let mut harness = EditorTestHarness::with_config(140, 40, typescript_only_config()).unwrap();
    harness.render().unwrap();

    open_language_dialog(&mut harness);
    focus_field(&mut harness, "Grammar");
    assert!(
        row_with(&harness, "Grammar").contains("typescript"),
        "precondition: Grammar reads 'typescript'; row: {:?}",
        row_with(&harness, "Grammar")
    );

    // Append junk to the grammar name...
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("ZZZ").unwrap();
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Grammar").contains("typescriptZZZ"),
        "the edit buffer should show the appended text; row: {:?}",
        row_with(&harness, "Grammar")
    );

    // ...then Esc — the junk must be gone and the original restored.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let row = row_with(&harness, "Grammar");
    assert!(
        row.contains("typescript") && !row.contains("ZZZ"),
        "Esc must revert the text field to its original value; row: {:?}",
        row
    );
}

/// The contrast case: Enter still *commits* a text edit (it is not reverted),
/// so the convention genuinely distinguishes the two keys.
#[test]
fn enter_commits_text_field_edit() {
    let mut harness = EditorTestHarness::with_config(140, 40, typescript_only_config()).unwrap();
    harness.render().unwrap();

    open_language_dialog(&mut harness);
    focus_field(&mut harness, "Grammar");

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("ZZZ").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Grammar").contains("typescriptZZZ"),
        "Enter must keep the typed value; row: {:?}",
        row_with(&harness, "Grammar")
    );
}
