//! E2E test: Fallback Language Config appears as a settings category
//!
//! Validates that the `fallback` field (Option<LanguageConfig>) is resolved
//! through its anyOf schema into a proper settings category with individual
//! typed controls, rather than being shown as a raw JSON editor.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// The fallback field should appear as its own category in the sidebar,
/// not as a raw JSON editor inside General.
#[test]
fn test_fallback_is_settings_category() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();

    harness.open_settings().unwrap();

    // "Fallback" should appear as a category in the sidebar
    harness.assert_screen_contains("Fallback");
}

/// The Fallback category should show individual typed controls for
/// LanguageConfig fields (Grammar text input, Auto Indent toggle, etc.)
/// rather than a raw JSON editor.
#[test]
fn test_fallback_shows_typed_controls() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();

    harness.open_settings().unwrap();

    // Navigate down to select the "Fallback" category.
    // Categories are alphabetically: Clipboard, Editor, Fallback, ...
    // General is first (index 0), so Fallback is at index 3.
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        // When Fallback is selected, the right panel shows its sub-fields.
        // "Auto Indent" is a LanguageConfig field that proves we have typed controls.
        // Also check "Comment Prefix" to confirm it's the Fallback panel
        // (not some other category that might also have "Auto Indent").
        if screen.contains("Fallback")
            && screen.contains("Auto Indent")
            && screen.contains("Comment Prefix")
            && screen.contains("Auto Close")
        {
            // Verify these are individual field controls, not a JSON blob
            assert!(
                screen.contains("Extensions"),
                "Fallback should show Extensions as a list control. Screen:\n{screen}"
            );
            return;
        }
    }

    let screen = harness.screen_to_string();
    panic!("Fallback category not found with typed controls. Screen:\n{screen}");
}
