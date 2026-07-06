//! E2E reproducer for issue #2566 — "Settings › Env: detector entries all
//! display as (no action)".
//!
//! The Settings **Env** section renders its **Detectors** list with the shared
//! ObjectArray/KeybindingList control, which shows each entry's
//! `x-display-field`. `EnvDetector` never declared one, so every row fell back
//! to the placeholder `(no action)` — the detectors were indistinguishable in
//! the list. With `x-display-field = /name` each row shows the detector's name
//! (`.venv`, `direnv`, …) instead.
//!
//! Drives only keyboard events and asserts on rendered output, per
//! CONTRIBUTING.md ("E2E Tests Observe, Not Inspect").

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Open Settings and walk the category sidebar down to the **Env** section,
/// whose right pane is uniquely identified by the `Detectors:` list label.
fn open_env_settings(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();
    // Sidebar starts focused on the first category; step down until the Env
    // section's Detectors list is on screen. Bounded so a regression that never
    // reaches Env fails instead of looping forever.
    for _ in 0..40 {
        if harness.screen_to_string().contains("Detectors:") {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(
        harness.screen_to_string().contains("Detectors:"),
        "should reach the Env section's Detectors list; screen was:\n{}",
        harness.screen_to_string()
    );
}

/// Issue #2566: each detector row shows its name, not the `(no action)`
/// placeholder. The default detectors include `.venv` and `direnv`.
#[test]
fn issue_2566_env_detectors_show_names_not_placeholder() {
    // Default config carries the built-in env detectors (.venv, venv, direnv,
    // mise, pipenv, poetry).
    let config = Config::default();
    assert!(
        !config.env.detectors.is_empty(),
        "precondition: default config ships env detectors"
    );

    let mut harness = EditorTestHarness::with_config(120, 40, config).unwrap();
    harness.render().unwrap();

    open_env_settings(&mut harness);

    let screen = harness.screen_to_string();
    // Pre-fix, every detector row rendered as the fallback placeholder.
    assert!(
        !screen.contains("(no action)"),
        "detector rows must not render as the `(no action)` placeholder; screen was:\n{}",
        screen
    );
    // Post-fix, rows are labeled by the detector `name`.
    assert!(
        screen.contains(".venv"),
        "detector rows should show their name (e.g. `.venv`); screen was:\n{}",
        screen
    );
    assert!(
        screen.contains("direnv"),
        "detector rows should show their name (e.g. `direnv`); screen was:\n{}",
        screen
    );
}
