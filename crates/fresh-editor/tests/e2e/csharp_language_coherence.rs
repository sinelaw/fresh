//! E2E test for C# language identification coherence.
//!
//! Verifies that C# files are detected correctly, the status bar shows the
//! human-readable display name, and switching back and forth between C# and
//! another language via the "Set Language" command palette keeps everything
//! consistent between the status bar and the prompt.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

/// Helper: open command palette, type a command name, and press Enter.
fn run_command(harness: &mut EditorTestHarness, command_name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text(command_name).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper: use "Set Language" command to switch to a language by typing
/// the given syntax name in the prompt.
fn set_language(harness: &mut EditorTestHarness, syntax_name: &str) {
    run_command(harness, "Set Language");
    harness.wait_for_prompt().unwrap();
    harness.render().unwrap();

    harness.type_text(syntax_name).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Opening a .cs file should detect language as "csharp" internally and
/// show "C#" in the status bar.
#[test]
fn test_cs_file_detected_as_csharp() {
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create and open a C# file
    let cs_file = project_dir.join("Program.cs");
    std::fs::write(
        &cs_file,
        "using System;\nclass Program {\n    static void Main() { }\n}\n",
    )
    .unwrap();
    harness.open_file(&cs_file).unwrap();
    harness.render().unwrap();

    // The internal language ID should be "csharp" (matching the config key)
    let language = harness.editor().active_state().language.clone();
    assert_eq!(
        language, "csharp",
        "Opening a .cs file should set internal language to 'csharp', got '{}'",
        language
    );

    // The display name should be "C#"
    let display = harness.editor().active_state().display_name.clone();
    assert_eq!(
        display, "C#",
        "Display name should be 'C#', got '{}'",
        display
    );

    // The status bar should show the human-readable "C#"
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("C#"),
        "Status bar should contain 'C#'. Got: {}",
        status_bar
    );
}

/// Open the Set Language prompt on a .cs file and verify C# is marked as current.
#[test]
fn test_set_language_prompt_shows_csharp_as_current() {
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    let cs_file = project_dir.join("Hello.cs");
    std::fs::write(&cs_file, "class Hello {}\n").unwrap();
    harness.open_file(&cs_file).unwrap();
    harness.render().unwrap();

    // Open "Set Language" command
    run_command(&mut harness, "Set Language");
    harness.wait_for_prompt().unwrap();
    harness.render().unwrap();

    // Type "C#" to filter — should show C# in the list
    harness.type_text("C#").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("C#"),
        "Set Language prompt should show 'C#' as a language option. Screen:\n{}",
        screen
    );

    // The "current" annotation should appear since this is a .cs file
    assert!(
        screen.contains("current"),
        "C# should be marked as 'current' for a .cs file. Screen:\n{}",
        screen
    );

    // Dismiss the prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Switch from C# to Python via Set Language and back, verifying that the
/// status bar and Set Language prompt always show the same display name.
#[test]
fn test_switch_language_csharp_to_python_and_back() {
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Start with a C# file
    let cs_file = project_dir.join("Main.cs");
    std::fs::write(&cs_file, "class Main {}\n").unwrap();
    harness.open_file(&cs_file).unwrap();
    harness.render().unwrap();

    let lang = harness.editor().active_state().language.clone();
    assert_eq!(lang, "csharp", "Initial language should be 'csharp'");
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("C#"),
        "Status bar should show 'C#' initially. Got: {}",
        status_bar
    );

    // Switch to Python via Set Language
    set_language(&mut harness, "Python");

    let lang = harness.editor().active_state().language.clone();
    assert_eq!(
        lang, "python",
        "After Set Language to Python, language should be 'python', got '{}'",
        lang
    );
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("Python"),
        "Status bar should show 'Python' after switching. Got: {}",
        status_bar
    );

    // Switch back to C# via Set Language
    set_language(&mut harness, "C#");

    let lang = harness.editor().active_state().language.clone();
    assert_eq!(
        lang, "csharp",
        "After Set Language back to C#, language should be 'csharp', got '{}'",
        lang
    );
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("C#"),
        "Status bar should show 'C#' after switching back. Got: {}",
        status_bar
    );
}

/// Switch from a plain text buffer to C# and then to Rust, verifying each transition.
#[test]
fn test_switch_from_text_to_csharp_to_rust() {
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Start with a plain text file
    let txt_file = project_dir.join("notes.txt");
    std::fs::write(&txt_file, "some notes\n").unwrap();
    harness.open_file(&txt_file).unwrap();
    harness.render().unwrap();

    let lang = harness.editor().active_state().language.clone();
    assert_eq!(lang, "text", "Initial language should be 'text'");

    // Switch to C#
    set_language(&mut harness, "C#");

    let lang = harness.editor().active_state().language.clone();
    assert_eq!(
        lang, "csharp",
        "After Set Language to C#, language should be 'csharp', got '{}'",
        lang
    );
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("C#"),
        "Status bar should show 'C#'. Got: {}",
        status_bar
    );

    // Switch to Rust
    set_language(&mut harness, "Rust");

    let lang = harness.editor().active_state().language.clone();
    assert_eq!(
        lang, "rust",
        "After Set Language to Rust, language should be 'rust', got '{}'",
        lang
    );
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("Rust"),
        "Status bar should show 'Rust'. Got: {}",
        status_bar
    );

    // Switch back to C#
    set_language(&mut harness, "C#");

    let lang = harness.editor().active_state().language.clone();
    assert_eq!(
        lang, "csharp",
        "After Set Language back to C#, language should be 'csharp', got '{}'",
        lang
    );
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("C#"),
        "Status bar should show 'C#' after switching back. Got: {}",
        status_bar
    );
}
