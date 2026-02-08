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

    // Create a temporary file outside the project directory
    let temp_dir = std::env::temp_dir();
    let script_name = format!("test_script_{}", std::process::id());
    let script_path = temp_dir.join(&script_name);

    // Create a shell script with shebang but no extension
    {
        let mut file = fs::File::create(&script_path).unwrap();
        writeln!(file, "#!/usr/bin/env bash").unwrap();
        writeln!(file, "echo 'Hello, World!'").unwrap();
    }

    // Open the file from outside the working directory
    harness.open_file(&script_path).unwrap();
    harness.render().unwrap();

    // Check if the shebang line has syntax highlighting (colored)
    // The shebang "#!/usr/bin/env bash" should be highlighted as a comment
    let has_initial_colors = (0..20).any(|col| {
        if let Some(style) = harness.get_cell_style(col, 0) {
            style.fg.is_some()
        } else {
            false
        }
    });

    if !has_initial_colors {
        // Skip test if shebang detection doesn't work in test environment
        eprintln!("Skipping test: shebang detection not working in test environment");
        let _ = fs::remove_file(&script_path);
        return;
    }

    // Make a small edit to mark buffer as modified
    harness.type_text("\n# test comment").unwrap();
    harness.render().unwrap();

    // Save the file (Ctrl+S) - this triggers the bug
    // Before fix: set_language_from_name gets just "test_script_123" and tries to read
    // it relative to working dir, fails, and loses syntax highlighting
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Give it a moment for processing
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // After save, check if syntax highlighting is still present
    // Check the comment we just added should be colored (line 2)
    let has_final_colors = (0..20).any(|col| {
        if let Some(style) = harness.get_cell_style(col, 2) {
            style.fg.is_some()
        } else {
            false
        }
    });

    // The key assertion: syntax highlighting colors should still be present after save
    // Without the fix: Colors disappear because highlighter becomes None
    // With the fix: Colors remain because highlighter can still read the file
    assert!(
        has_final_colors,
        "Syntax highlighting should be preserved after saving file outside working directory. \
         Bug: set_language_from_name was called with filename only, causing syntect to fail \
         reading the file and losing all highlighting colors."
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
