//! E2E reproducer: editing a language's **Tab Size** through
//! View → Settings → General → Languages → <lang> must keep the typed value.
//!
//! The language entry dialog accepts a number field's digits into a separate
//! edit buffer that only flushes into the control's `value` on *confirm*. The
//! dialog's commit path (Enter / Tab / Esc all "accept" the field, like a Text
//! field does) used to *cancel* the number edit instead of confirming it, so
//! every Tab Size / Page Width edit reverted to its previous value the instant
//! the user committed the field. This test drives only keyboard events and
//! asserts on rendered output, per CONTRIBUTING.md ("E2E Tests Observe, Not
//! Inspect").

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

/// True when the row showing `label` carries the dialog's focus marker (`>`),
/// which the renderer draws before the focused field's text.
fn row_is_focused(harness: &EditorTestHarness, label: &str) -> bool {
    let row = row_with(harness, label);
    row.split(label).next().unwrap_or("").contains('>')
}

/// From a freshly opened Settings panel, focus the right-hand list, walk down
/// to the single language map entry, and open its Edit Value dialog. Leaves the
/// dialog open. (Mirrors the helper in `issue_2345_language_settings.rs`.)
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

/// Walk the dialog's field focus down until the Tab Size row is the focused one.
/// (Tab Size sits near the bottom of a tall dialog, so it scrolls into view as
/// the focus walks toward it.)
fn focus_tab_size(harness: &mut EditorTestHarness) {
    for _ in 0..80 {
        if row_is_focused(harness, "Tab Size") {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    panic!(
        "could not focus the Tab Size field; screen was:\n{}",
        harness.screen_to_string()
    );
}

/// Typing a new Tab Size and committing the field must keep the typed value.
/// Pre-fix the commit cancelled the number edit, so the field snapped back to
/// its inherited `0`.
#[test]
fn language_tab_size_edit_survives_commit() {
    let mut harness = EditorTestHarness::with_config(140, 40, typescript_only_config()).unwrap();
    harness.render().unwrap();

    open_language_dialog(&mut harness);
    focus_tab_size(&mut harness);

    // Inherited to start with: the value reads 0 and offers the (Inherited) badge.
    assert!(
        row_with(&harness, "Tab Size").contains("0 ]"),
        "precondition: Tab Size starts inherited at 0; row: {:?}",
        row_with(&harness, "Tab Size")
    );

    // Enter edit mode, replace the value with 8, and commit the field with Tab.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("8").unwrap();
    harness.render().unwrap();
    // While editing, the edit buffer shows the typed digit.
    assert!(
        row_with(&harness, "Tab Size").contains("8 ]"),
        "typed digit should show in the edit buffer; row: {:?}",
        row_with(&harness, "Tab Size")
    );

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // After committing, the value must still be 8 — not reverted to 0.
    assert!(
        row_with(&harness, "Tab Size").contains("8 ]"),
        "committed Tab Size must persist as 8, not revert to 0; row: {:?}",
        row_with(&harness, "Tab Size")
    );
}
