//! E2E tests: default_language setting in the General category
//!
//! Validates that the `default_language` field appears as a dropdown
//! populated with the defined language keys, and functions correctly.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// The default_language field should appear in the General settings category.
#[test]
fn test_default_language_in_general_settings() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();

    harness.open_settings().unwrap();

    // General is the first category, already selected.
    // Scroll down to find "Default Language" in the settings list.
    for _ in 0..30 {
        let screen = harness.screen_to_string();
        if screen.contains("Default Language") {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let screen = harness.screen_to_string();
    panic!("Default Language setting not found in General category. Screen:\n{screen}");
}

/// The default_language field should render as a dropdown (with ▼ indicator),
/// not as a plain text input.
#[test]
fn test_default_language_is_dropdown() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();

    harness.open_settings().unwrap();

    // Navigate within settings panel to Default Language
    // Tab to switch focus to the settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    for _ in 0..15 {
        let screen = harness.screen_to_string();
        // A dropdown shows ▼ or ▲ arrow indicator
        if screen.contains("Default Language") && (screen.contains("▼") || screen.contains("▲"))
        {
            // Verify it contains a value indicator — either "(none)" for unset
            // or a language name
            assert!(
                screen.contains("(none)") || screen.contains("(Inherited)"),
                "Dropdown should show (none) or (Inherited) when unset. Screen:\n{screen}"
            );
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let screen = harness.screen_to_string();
    panic!("Default Language dropdown not found. Screen:\n{screen}");
}

/// Opening the dropdown should show language keys defined in the config.
#[test]
fn test_default_language_dropdown_shows_languages() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();

    harness.open_settings().unwrap();

    // Tab to settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate to Default Language field
    for _ in 0..15 {
        let screen = harness.screen_to_string();
        if screen.contains("Default Language") && screen.contains("▼") {
            // Found it — open the dropdown
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
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let screen = harness.screen_to_string();
    panic!("Could not navigate to Default Language dropdown. Screen:\n{screen}");
}
