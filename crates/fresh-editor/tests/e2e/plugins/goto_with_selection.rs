//! End-to-end tests for the goto_with_selection plugin
//!
//! Tests coverage:
//! 1. Absolute line numbers mode:
//!    1.1. Select from line 1 to line 3
//!    1.2. Select from line 4 to line 2 (reverse selection)
//! 2. Relative line numbers mode:
//!    2.1. Start at line 4, select -2 (goes to line 2)
//!    2.2. Start at line 2, select +2 (goes to line 4)

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use tempfile::TempDir;

/// Helper: write a fixture file with `n` lines of the form `LINE<n>\n`.
fn write_numbered_lines(path: &std::path::Path, n: usize) {
    let mut s = String::new();
    for i in 1..=n {
        s.push_str(&format!("LINE{i}\n"));
    }
    fs::write(path, s).unwrap();
}

/// Helper to set up test harness with goto_with_selection plugin.
/// Returns (harness, temp_dir, project_root) - keep temp_dir alive for test duration.
fn setup_harness_with_plugin(
    config: fresh::config::Config,
) -> (EditorTestHarness, TempDir, std::path::PathBuf) {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    fs::create_dir_all(&project_root).unwrap();

    // Create plugins directory and copy the plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "goto_with_selection");

    let harness =
        EditorTestHarness::with_config_and_working_dir(100, 24, config, project_root.clone())
            .unwrap();

    (harness, temp_dir, project_root)
}

/// Helper to execute the select_to_line command via command palette
fn execute_select_to_line(harness: &mut EditorTestHarness, line_number: &str) {
    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Type the command name
    harness
        .type_text("Select from current position to target line")
        .unwrap();
    harness.render().unwrap();

    // Confirm command selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Type the target line number
    harness.type_text(line_number).unwrap();
    harness.render().unwrap();

    // Confirm line number
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();
}

/// Verifies selection contains ONLY expected lines by: copy selection, select all, paste.
/// Then checks that the screen shows EXACTLY the expected content (original replaced with selection).
fn verify_selection_contains_only(harness: &mut EditorTestHarness, expected_lines: &[&str]) {
    harness.editor_mut().set_clipboard_for_test("".to_string());

    // Copy the selection
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Select all (Ctrl+A)
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Paste (Ctrl+V) - replaces all with copied selection
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Now screen should show ONLY the pasted content
    let screen = harness.screen_to_string();

    // Check for the Pasted status message
    assert!(
        screen.contains("Pasted"),
        "Should show 'Pasted' status after operation. Screen:\n{}",
        screen
    );

    // Verify ALL expected lines are present
    for line in expected_lines {
        assert!(
            screen.contains(line),
            "Expected '{}' to be visible after copy-all-paste. Screen:\n{}",
            line,
            screen
        );
    }

    // Verify NO other LINE* content exists (strict)
    for i in 1..=5 {
        let line = format!("LINE{}", i);
        let expected = expected_lines.contains(&line.as_str());
        if !expected {
            assert!(
                !screen.contains(&line),
                "Found unexpected '{}' in screen after copy-all-paste. Screen:\n{}",
                line,
                screen
            );
        }
    }
}

/// Test 1.1: Absolute mode - select from line 1 to line 3
///
/// Verifies the selection command completes and ends at line 3.
#[test]
fn test_absolute_select_line_1_to_3() {
    let (mut harness, _temp, project_root) = setup_harness_with_plugin(Default::default());

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 5);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    // Execute select_to_line with target line 3
    execute_select_to_line(&mut harness, "3");

    // Verify status bar shows we're at line 3 (end of selection)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 3,"),
        "Status bar should show line 3. Screen:\n{}",
        screen
    );

    // Verify selection contains LINE1 and LINE2 by copy-all-paste
    verify_selection_contains_only(&mut harness, &["LINE1", "LINE2"]);
}

/// Test 1.2: Absolute mode - select from line 4 to line 2 (reverse)
///
/// Verifies reverse selection (backward) works correctly.
#[test]
fn test_absolute_select_line_4_to_2() {
    let (mut harness, _temp, project_root) = setup_harness_with_plugin(Default::default());

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 5);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    // Go to line 4 first using Ctrl+G
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("4").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 4,"))
        .unwrap();

    // Execute select_to_line with target line 2
    execute_select_to_line(&mut harness, "2");

    // Verify status bar shows we're at line 2 (end of reverse selection)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 2,"),
        "Status bar should show line 2. Screen:\n{}",
        screen
    );

    // Verify selection contains LINE2 and LINE3 (reverse selection)
    verify_selection_contains_only(&mut harness, &["LINE2", "LINE3"]);
}

/// Test 2.1: Relative mode - at line 4, select -2 (goes to line 2)
///
/// Verifies relative negative offset works.
#[test]
fn test_relative_select_from_line_4_minus_2() {
    let mut config = fresh::config::Config::default();
    config.editor.relative_line_numbers = true;

    let (mut harness, _temp, project_root) = setup_harness_with_plugin(config);

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 5);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    // Go to line 4 first using Ctrl+G
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("4").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 4,"))
        .unwrap();

    // Execute select_to_line with relative target -2
    execute_select_to_line(&mut harness, "-2");

    // Verify status bar shows we're at line 2 (line 4 + -2 = 2)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 2,"),
        "Status bar should show line 2. Screen:\n{}",
        screen
    );

    // Verify selection contains LINE2 and LINE3 (relative -2)
    verify_selection_contains_only(&mut harness, &["LINE2", "LINE3"]);
}

/// Test 2.2: Relative mode - at line 2, select +2 (goes to line 4)
///
/// Verifies relative positive offset works.
#[test]
fn test_relative_select_from_line_2_plus_2() {
    let mut config = fresh::config::Config::default();
    config.editor.relative_line_numbers = true;

    let (mut harness, _temp, project_root) = setup_harness_with_plugin(config);

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 5);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    // Go to line 2 first using Ctrl+G
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("2").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 2,"))
        .unwrap();

    // Execute select_to_line with relative target +2
    execute_select_to_line(&mut harness, "+2");

    // Verify status bar shows we're at line 4 (line 2 + +2 = 4)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 4,"),
        "Status bar should show line 4. Screen:\n{}",
        screen
    );

    // Verify selection contains LINE2 and LINE3 (relative +2)
    verify_selection_contains_only(&mut harness, &["LINE2", "LINE3"]);
}
