//! E2E test for issue #1078: syntax language case mismatch
//!
//! When opening a .py file, the language is set to "python" (lowercase).
//! But when changing the language via the command palette "Set Language" command
//! (or clicking the status bar language indicator), the language is set to "Python"
//! (capitalized from the syntect syntax name).
//!
//! This causes LSP config lookup to fail since config keys are lowercase.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

/// Helper to run a command from the command palette (same as buffer_settings_commands.rs)
fn run_command(harness: &mut EditorTestHarness, command_name: &str) {
    // Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type the command name
    harness.type_text(command_name).unwrap();
    harness.render().unwrap();

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test that changing the language via "Set Language" command uses the same
/// case as when a file is opened by extension.
///
/// Issue #1078: Opening a .py file sets language to "python" (lowercase),
/// but selecting "Python" from the Set Language prompt sets it to "Python"
/// (capitalized). This causes LSP config lookup to fail.
#[test]
fn test_set_language_case_matches_file_detection() {
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Step 1: Create and open a Python file
    let py_file = project_dir.join("test.py");
    std::fs::write(&py_file, "print('hello')\n").unwrap();
    harness.open_file(&py_file).unwrap();
    harness.render().unwrap();

    // Step 2: Verify the language detected from extension is lowercase "python"
    let language_from_extension = harness.editor().active_state().language.clone();
    eprintln!(
        "Language detected from .py extension: '{}'",
        language_from_extension
    );
    assert_eq!(
        language_from_extension, "python",
        "Opening a .py file should set language to lowercase 'python'"
    );

    // Step 3: Now create a plain text file (no language) and open it
    let txt_file = project_dir.join("test.txt");
    std::fs::write(&txt_file, "some text\n").unwrap();
    harness.open_file(&txt_file).unwrap();
    harness.render().unwrap();

    let initial_language = harness.editor().active_state().language.clone();
    eprintln!("Initial language for .txt file: '{}'", initial_language);

    // Step 4: Use "Set Language" command to change to Python
    run_command(&mut harness, "Set Language");

    // The "Set Language" command opens a second prompt for language selection.
    // After run_command, the language selection prompt should be active.
    harness.wait_for_prompt().unwrap();
    harness.render().unwrap();

    // Type "Python" to filter the language list and select it
    harness.type_text("Python").unwrap();
    harness.render().unwrap();

    // Press Enter to confirm the selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Step 5: Check the language that was set via the command
    let language_from_command = harness.editor().active_state().language.clone();
    eprintln!(
        "Language set via Set Language command: '{}'",
        language_from_command
    );

    // BUG REPRODUCTION: The language from command palette uses the syntect
    // syntax name (e.g., "Python" capitalized) instead of the internal
    // lowercase ID ("python"). This causes LSP config lookup to fail.
    //
    // The assertion below documents the expected behavior (lowercase).
    // Currently it FAILS because the language is set to "Python" (capitalized).
    assert_eq!(
        language_from_command, language_from_extension,
        "Language set via 'Set Language' command ('{}') should match \
         the language detected from file extension ('{}').\n\
         Bug #1078: The command palette uses syntect's capitalized syntax name \
         instead of the internal lowercase language ID, causing LSP config lookup to fail.",
        language_from_command, language_from_extension
    );
}

/// Test that the status bar displays a language identifier that matches
/// what the LSP config expects (lowercase).
///
/// This test verifies the root cause: the Set Language prompt suggestions
/// use syntect syntax display names (capitalized) rather than the internal
/// language IDs (lowercase) that the rest of the system expects.
#[test]
fn test_set_language_preserves_lsp_compatible_id() {
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create a plain text file
    let txt_file = project_dir.join("plain.txt");
    std::fs::write(&txt_file, "x = 1\n").unwrap();
    harness.open_file(&txt_file).unwrap();
    harness.render().unwrap();

    // Use "Set Language" command to set to Rust
    run_command(&mut harness, "Set Language");
    harness.wait_for_prompt().unwrap();
    harness.type_text("Rust").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let language_after_set = harness.editor().active_state().language.clone();
    eprintln!(
        "Language after 'Set Language' to Rust: '{}'",
        language_after_set
    );

    // The language should be the lowercase ID that LSP config uses
    // Currently this fails because it's set to "Rust" instead of "rust"
    assert_eq!(
        language_after_set, "rust",
        "Language set via command should use lowercase ID 'rust', got '{}'. \
         This causes LSP config lookup to fail since config keys are lowercase.",
        language_after_set
    );
}
