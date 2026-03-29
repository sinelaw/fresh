//! E2E tests for LSP commands on languages without LSP configuration
//!
//! Tests that LSP commands are properly disabled when the active buffer's
//! language has no LSP server configured (e.g., plain text files).
//!
//! Refs: https://github.com/anthropics/fresh/issues/1168

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that "Start/Restart LSP" command is disabled for a language with
/// no LSP configured (e.g., plain text).
///
/// Previously, executing this command would log a WARN
/// "Failed to spawn LSP client for language: text" which is confusing
/// since there's no LSP to spawn.
#[test]
fn test_start_lsp_disabled_for_unconfigured_language() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("readme.txt");
    std::fs::write(&test_file, "Hello, this is a plain text file.\n")?;

    let mut harness = EditorTestHarness::with_working_dir(120, 30, temp_dir.path().to_path_buf())?;

    // Open the text file - its language is "text" which has no LSP configured
    harness.open_file(&test_file)?;
    harness.render()?;

    // Open command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.wait_for_prompt()?;

    // Type specific query to match the LSP restart command
    harness.type_text("Start/Restart LSP")?;
    harness.render()?;

    // The "Start/Restart LSP" command should be visible in suggestions
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("LSP"),
        "Command palette should show LSP-related command. Screen:\n{}",
        screen
    );

    // Try to execute the command - it should be disabled
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Should show "not available" since the command is disabled for this language
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("not available") || screen.contains("No LSP server configured"),
        "Should show 'not available' or 'no LSP configured' message for text files. Screen:\n{}",
        screen
    );

    // The warning indicator should NOT be active
    assert!(
        !harness.editor().get_warning_domains().has_any_warnings(),
        "Opening a text file and trying to start LSP should not trigger a warning indicator"
    );

    Ok(())
}

/// Test that "Toggle LSP for Buffer" is also disabled for unconfigured languages.
#[test]
fn test_toggle_lsp_disabled_for_unconfigured_language() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("notes.txt");
    std::fs::write(&test_file, "Just some notes.\n")?;

    let mut harness = EditorTestHarness::with_working_dir(120, 30, temp_dir.path().to_path_buf())?;

    // Open the text file
    harness.open_file(&test_file)?;
    harness.render()?;

    // Open command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.wait_for_prompt()?;

    // Type a single-term query to match only "Toggle LSP for Current Buffer".
    // Using a single term (no spaces) avoids multi-term description matching
    // which could match enabled commands (sorted before disabled ones).
    harness.type_text("ToggleLspBuffer")?;
    harness.render()?;

    // Try to execute the command - it should be disabled
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Should show "not available" since the command is disabled for this language
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("not available") || screen.contains("No LSP server configured"),
        "Should show 'not available' or 'no LSP configured' message. Screen:\n{}",
        screen
    );

    Ok(())
}

/// Test that opening a plain text file does not produce an LSP warning.
///
/// The log level for "no LSP configured" should be debug, not warn.
#[test]
fn test_no_lsp_warning_for_text_file() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("plain.txt");
    std::fs::write(&test_file, "No LSP warning should appear.\n")?;

    let mut harness = EditorTestHarness::with_working_dir(120, 30, temp_dir.path().to_path_buf())?;

    // Open the text file
    harness.open_file(&test_file)?;
    harness.render()?;

    // Process any async messages
    for _ in 0..5 {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(10));
    }

    // No warnings should be active
    assert!(
        !harness.editor().get_warning_domains().has_any_warnings(),
        "Opening a plain text file should not produce any warnings"
    );

    Ok(())
}
