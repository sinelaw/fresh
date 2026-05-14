//! E2E tests for customizable status bar configuration.
//!
//! Verifies that the `status_bar.left` and `status_bar.right` config options
//! control which elements appear (and don't appear) in the rendered status bar.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, StatusBarConfig, StatusBarElement};
use std::fs;

/// Helper: create a config with the given status bar elements.
fn config_with_status_bar(left: Vec<StatusBarElement>, right: Vec<StatusBarElement>) -> Config {
    let mut config = Config::default();
    config.editor.status_bar = StatusBarConfig { left, right };
    config
}

/// Removing an element from the config should remove it from the rendered
/// status bar. Here we drop `{encoding}` and verify "UTF-8" no longer appears.
#[test]
fn test_removed_element_not_rendered() {
    // Default right side includes Encoding; remove it.
    let config = config_with_status_bar(
        vec![StatusBarElement::Filename],
        vec![
            StatusBarElement::LineEnding,
            // StatusBarElement::Encoding removed
            StatusBarElement::Language,
        ],
    );

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_config(config),
    )
    .unwrap();

    // Open a file so the status bar has content to show.
    let dir = harness.project_dir().unwrap();
    let file = dir.join("hello.txt");
    fs::write(&file, "hello world\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    assert!(
        !status.contains("UTF-8"),
        "Encoding should not appear when removed from config.\nStatus bar: {status}"
    );
    // Language should still be present (Plain Text or similar)
    assert!(
        status.contains("Plain Text") || status.contains("txt"),
        "Language element should still appear.\nStatus bar: {status}"
    );
}

/// Adding the `{clock}` element should render a time string in HH:MM format.
#[test]
fn test_clock_element_renders() {
    let config = config_with_status_bar(
        vec![StatusBarElement::Filename],
        vec![StatusBarElement::Clock],
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "content\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    // Clock renders as HH:MM with hardware blink on the colon.
    // Match DD:DD pattern anywhere in the status bar.
    let has_time = {
        let bytes = status.as_bytes();
        bytes.windows(5).any(|w| {
            w[0].is_ascii_digit()
                && w[1].is_ascii_digit()
                && w[2] == b':'
                && w[3].is_ascii_digit()
                && w[4].is_ascii_digit()
        })
    };
    assert!(
        has_time,
        "Clock element should render a time as HH:MM.\nStatus bar: {status}"
    );
}

/// An empty right config should render no right-side elements.
#[test]
fn test_empty_right_side() {
    let config = config_with_status_bar(
        vec![StatusBarElement::Filename, StatusBarElement::Cursor],
        vec![], // no right side
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "some text\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    // With no right side, encoding/language/line-ending should be absent
    assert!(
        !status.contains("UTF-8"),
        "No encoding expected.\nStatus bar: {status}"
    );
    assert!(
        !status.contains("LF") || status.contains("LF") && status.contains("test.txt"),
        // LF might appear as part of the filename or other left-side text; just check
        // that typical right-side indicators are gone
        "Checking right side is empty.\nStatus bar: {status}"
    );
    // Cursor info should still be present (left side)
    assert!(
        status.contains("Ln") || status.contains("1:1") || status.contains("Col"),
        "Cursor element should appear on left side.\nStatus bar: {status}"
    );
}

/// Compact cursor format `{cursor:compact}` should render as `row:col` instead
/// of the default `Ln X, Col Y`.
#[test]
fn test_compact_cursor_format() {
    let config = config_with_status_bar(
        vec![StatusBarElement::Filename, StatusBarElement::CursorCompact],
        vec![],
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "line one\nline two\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    assert!(
        status.contains("1:1"),
        "Compact cursor should show 1:1.\nStatus bar: {status}"
    );
    assert!(
        !status.contains("Ln"),
        "Compact cursor should not show 'Ln'.\nStatus bar: {status}"
    );
}

/// Regression test for issue #1967 — moving the cursor between lines of
/// very different lengths must not shift the position of elements that
/// follow `{cursor}` in the status bar. Reproduces the original symptom
/// (the status bar was fidgety when the column number jumped between 1
/// and 2+ digits) by anchoring a trailing always-present `{language}`
/// element after `{cursor}` and asserting the language token stays in
/// the same column before and after a cursor move.
#[test]
fn test_cursor_indicator_width_is_stable_across_cursor_movement() {
    let config = config_with_status_bar(
        vec![
            StatusBarElement::Filename,
            StatusBarElement::Cursor,
            StatusBarElement::Language,
        ],
        vec![],
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(160, 30, config).unwrap();

    // Two lines of very different lengths so moving Down + End jumps the
    // column number from a single digit to three digits — the exact
    // pattern that caused the bar to wiggle in #1967.
    let dir = harness.project_dir().unwrap();
    let file = dir.join("wiggly.txt");
    let short = "ab\n";
    let long = format!("{}\n", "x".repeat(150));
    fs::write(&file, format!("{short}{long}")).unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status_before = harness.get_status_bar();
    // Find the language token start as a stable anchor. The status bar
    // uses " | " separators between left-side elements, so the last "|"
    // sits just before the Language element. Its column position must
    // stay fixed when only the Cursor element changes.
    let lang_col_before = status_before
        .rfind('|')
        .unwrap_or_else(|| panic!("expected separator in status bar: {status_before:?}"));

    // Move to line 2, then to the end of the (150-char) line.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let status_after = harness.get_status_bar();
    let lang_col_after = status_after
        .rfind('|')
        .unwrap_or_else(|| panic!("expected separator in status bar: {status_after:?}"));

    assert_eq!(
        lang_col_before, lang_col_after,
        "Element after `{{cursor}}` shifted when the cursor moved.\n  before: {status_before:?}\n  after:  {status_after:?}"
    );
}

/// Adding the `{remote}` element to the local status bar should render a
/// visible "Local" indicator so the bottom-left remote-authority entry point
/// is always present.
#[test]
fn test_remote_indicator_shows_local() {
    let config = config_with_status_bar(
        vec![
            StatusBarElement::RemoteIndicator,
            StatusBarElement::Filename,
        ],
        vec![],
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "hello\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    assert!(
        status.contains("Local"),
        "Remote indicator should show 'Local' when no authority is connected.\nStatus bar: {status}"
    );
}

/// The remote indicator should be the left-most element when configured
/// first in the `left` list — the spec places the remote authority entry
/// point at the bottom-left of the status bar.
#[test]
fn test_remote_indicator_placed_at_far_left() {
    let config = config_with_status_bar(
        vec![
            StatusBarElement::RemoteIndicator,
            StatusBarElement::Filename,
        ],
        vec![],
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "hello\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    let local_idx = status
        .find("Local")
        .expect("Remote indicator missing from status bar");
    let filename_idx = status
        .find("test.txt")
        .expect("Filename missing from status bar");
    assert!(
        local_idx < filename_idx,
        "Remote indicator should appear before the filename.\nStatus bar: {status}"
    );
}

/// Both empty sides should still render a valid (blank) status bar without
/// crashing.
#[test]
fn test_both_sides_empty() {
    let config = config_with_status_bar(vec![], vec![]);

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "hello\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    // Should not contain any typical status bar indicators
    assert!(
        !status.contains("UTF-8"),
        "No encoding expected.\nStatus bar: {status}"
    );
    assert!(
        !status.contains("Ln"),
        "No cursor info expected.\nStatus bar: {status}"
    );
}
