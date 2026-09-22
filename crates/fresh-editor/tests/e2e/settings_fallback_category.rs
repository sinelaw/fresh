//! E2E tests: default_language setting in the Syntax & Languages category
//!
//! Validates that the `default_language` field appears as a dropdown
//! populated with the defined language keys, and functions correctly.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Open Settings and jump to the Default Language setting through search.
/// It sits below the Languages map, which is taller than the window, so
/// search is the reliable way to bring it into view with focus on it.
fn jump_to_default_language(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Default Language").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// The screen line showing the Default Language setting.
fn default_language_row(harness: &EditorTestHarness) -> Option<String> {
    harness
        .screen_to_string()
        .lines()
        .find(|l| l.contains("Default Language"))
        .map(str::to_string)
}

/// The default_language field should appear in the Syntax & Languages category.
#[test]
fn test_default_language_in_general_settings() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();
    jump_to_default_language(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        default_language_row(&harness).is_some(),
        "Default Language setting not found. Screen:\n{screen}"
    );
    assert!(
        screen.contains("│Syntax & Languages"),
        "Default Language should be on the Syntax & Languages page. Screen:\n{screen}"
    );
}

/// The default_language field should render as a dropdown (with ▼ indicator),
/// not as a plain text input.
#[test]
fn test_default_language_is_dropdown() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();
    jump_to_default_language(&mut harness);

    let screen = harness.screen_to_string();
    let row = default_language_row(&harness)
        .unwrap_or_else(|| panic!("Default Language dropdown not found. Screen:\n{screen}"));
    // A dropdown shows ▼ or ▲ arrow indicator, and a value indicator —
    // "(none)" for unset or "(Inherited)".
    assert!(
        row.contains('▼') || row.contains('▲'),
        "Default Language should be a dropdown. Screen:\n{screen}"
    );
    assert!(
        row.contains("(none)") || row.contains("(Inherited)"),
        "Dropdown should show (none) or (Inherited) when unset. Screen:\n{screen}"
    );
}

/// Opening the dropdown should show language keys defined in the config.
#[test]
fn test_default_language_dropdown_shows_languages() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    jump_to_default_language(&mut harness);

    // Search leaves focus on the setting; Enter opens the dropdown.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // Should show some of the built-in language keys
    assert!(
        screen.contains("bash"),
        "Dropdown should contain 'bash'. Screen:\n{screen}"
    );
    assert!(
        screen.contains("(none)"),
        "Dropdown should contain '(none)' option. Screen:\n{screen}"
    );
}
