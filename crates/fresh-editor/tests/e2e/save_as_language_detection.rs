use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

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
