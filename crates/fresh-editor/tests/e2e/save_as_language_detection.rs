use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::io::Write;

/// Test that saving a file with shebang outside working directory preserves syntax highlighting
/// Bug: When saving a file without extension (e.g., shell script with shebang) that's outside
/// the working directory, syntax highlighting is lost because only the filename is passed to
/// set_language_from_name, which then tries to read the file relative to the working dir.
#[test]
fn test_save_shebang_detection_outside_workdir() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Create file with NO extension outside project directory
    // Should detect bash from shebang alone
    let temp_dir = std::env::temp_dir();
    let script_name = format!("testscript{}", std::process::id());
    let script_path = temp_dir.join(&script_name);

    eprintln!("Test script path: {}", script_path.display());
    eprintln!("Project dir: {:?}", harness.project_dir());

    {
        let mut file = fs::File::create(&script_path).unwrap();
        writeln!(file, "#!/usr/bin/env bash").unwrap();
        writeln!(file, "echo 'Hello, World!'").unwrap();
        writeln!(file, "date").unwrap();
    }

    // Open file - should detect bash from shebang
    harness.open_file(&script_path).unwrap();
    harness.render().unwrap();

    // Open the file from outside the working directory
    harness.open_file(&script_path).unwrap();
    harness.render().unwrap();

    // Delay and capture initial state
    std::thread::sleep(std::time::Duration::from_millis(200));
    harness.render().unwrap();

    eprintln!("\n=== INITIAL STATE (before save) ===");
    eprintln!("{}", harness.screen_to_string());

    // Find the "echo" keyword and check its color
    let (echo_col, echo_row) = harness
        .find_text_on_screen("echo")
        .expect("Should find 'echo' on screen");
    let initial_echo_color = harness
        .get_cell_style(echo_col, echo_row)
        .expect("Should get style for 'echo'")
        .fg;

    eprintln!(
        "\nInitial 'echo' color at ({}, {}): {:?}",
        echo_col, echo_row, initial_echo_color
    );

    // Bash syntax highlighting should color the "echo" command (not white)
    assert_ne!(
        initial_echo_color,
        Some(ratatui::style::Color::White),
        "Initial 'echo' should be syntax highlighted (not white)"
    );

    // Move down one line so we don't break the shebang when editing
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Go to end of line and add a comment
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("\n# test comment").unwrap();
    harness.render().unwrap();

    // Save the file (Ctrl+S) - this triggers the bug
    // Before fix: set_language_from_name gets just "test_script_123" and tries to read
    // it relative to working dir, fails, and loses syntax highlighting
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for save to complete (buffer becomes unmodified)
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    // Wait for save and delay
    std::thread::sleep(std::time::Duration::from_millis(200));

    // Scroll down and back to force buffer re-render with current highlighter
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    eprintln!("\n=== AFTER SAVE (with re-render) ===");
    eprintln!("{}", harness.screen_to_string());

    // Check the same "echo" on line 2 - it should still be colored
    let (final_echo_col, final_echo_row) = harness
        .find_text_on_screen("echo")
        .expect("Should find 'echo' on screen");
    let final_echo_color = harness
        .get_cell_style(final_echo_col, final_echo_row)
        .expect("Should get style for 'echo'")
        .fg;

    eprintln!(
        "\nFinal 'echo' color at ({}, {}): {:?}",
        final_echo_col, final_echo_row, final_echo_color
    );

    // The key assertion: "echo" should still be highlighted after save
    // Without the fix: highlighter becomes None during save, colors lost after re-render
    // With the fix: highlighter preserved, colors maintained
    assert_eq!(
        final_echo_color, initial_echo_color,
        "Syntax highlighting should be preserved after saving file outside working directory. \
         Bug: set_language_from_name was called with filename only, causing highlighter to fail. \
         Initial 'echo' color: {:?}, Final 'echo' color: {:?}",
        initial_echo_color, final_echo_color
    );

    // Clean up
    let _ = fs::remove_file(&script_path);
}

/// Test that "Save As" correctly detects the language based on the file extension
/// Bug: saving a new file with "Save As" doesn't trigger language detection until the NEXT save
#[test]
fn test_save_as_detects_language() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // 1. Create a new buffer (implicitly text/plain)
    harness.new_buffer().unwrap();

    // Verify initial language is text
    let initial_lang = harness.editor().active_state().language.clone();
    assert_eq!(initial_lang, "text", "New buffer should be text");

    // Type some content
    harness.type_text("fn main() {}").unwrap();
    harness.render().unwrap();

    // 2. Trigger "Save As" via command palette (Ctrl+P -> "Save File As")
    // Note: Ctrl+S on unnamed buffer also triggers Save As, testing that path too
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Wait for the Save As prompt to appear
    harness.wait_for_screen_contains("Save as:").unwrap();

    // 3. Type a filename with a Rust extension
    let filename = "test.rs";
    harness.type_text(filename).unwrap();

    // 4. Confirm save
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for "Saved as" message or file existence
    let file_path = project_dir.join(filename);
    harness.wait_until(|_| file_path.exists()).unwrap();

    // Give it a moment for any async processing (though save should be blocking-ish)
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // 5. Verify the language is now "rust"
    let final_lang = harness.editor().active_state().language.clone();

    // This assertion is expected to FAIL before the fix
    assert_eq!(
        final_lang, "rust",
        "Language should be detected as rust after Save As"
    );
}
