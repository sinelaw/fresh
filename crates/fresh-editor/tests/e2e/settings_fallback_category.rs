//! E2E test: default_language appears as a setting in the General category
//!
//! Validates that the `default_language` field (formerly `fallback`) appears
//! as a simple text/dropdown setting in the General settings page.

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
