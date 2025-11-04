
use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test command palette trigger and rendering
#[test]
fn test_command_palette_trigger() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that the command prompt is visible
    harness.assert_screen_contains("Command: ");

    // Check that suggestions are visible (should show all commands initially)
    harness.assert_screen_contains("Open File");
    harness.assert_screen_contains("Save File");
    harness.assert_screen_contains("Quit");
}

/// Test command palette autocomplete filtering
#[test]
fn test_command_palette_autocomplete() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "open" to filter commands
    harness.type_text("open").unwrap();

    // Should show filtered results
    harness.assert_screen_contains("Open File");

    // Should not show non-matching commands
    // (We might still see them if there are many results, but "Open File" should be first)
}

/// Test command palette navigation with Up/Down
#[test]
fn test_command_palette_navigation() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command: ");

    // Navigate down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Navigate up
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Commands should still be visible
    harness.assert_screen_contains("Open File");
}

/// Test command palette Tab completion
#[test]
fn test_command_palette_tab_completion() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type partial text
    harness.type_text("op").unwrap();

    // Press Tab to accept first suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The input should be completed to "Open File" (the first matching command)
    harness.assert_screen_contains("Command: Open File");
}

/// Test command palette cancel with Escape
#[test]
fn test_command_palette_cancel() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command: ");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Prompt should be gone
    harness.assert_screen_not_contains("Command: ");
    harness.assert_screen_contains("Canceled");
}

/// Test executing a command from the palette
#[test]
fn test_command_palette_execute() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type the command name
    harness.type_text("Show Help").unwrap();

    // Execute with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Help should now be visible
    harness.assert_screen_contains("KEYBOARD SHORTCUTS");
}

/// Test command palette fuzzy matching
#[test]
fn test_command_palette_fuzzy_matching() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "sf" which should match "Save File" (fuzzy match)
    harness.type_text("sf").unwrap();

    // Should show "Save File" in suggestions
    harness.assert_screen_contains("Save File");
}
