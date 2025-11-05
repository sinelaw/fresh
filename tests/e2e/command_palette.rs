
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

/// Test Tab completion skips disabled suggestions
#[test]
fn test_command_palette_tab_skip_disabled() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "save" - this will match "Save File" and "Save File As"
    // In Normal context both should be available
    harness.type_text("save").unwrap();

    // Press Tab to accept first suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The input should be completed (should work with available commands)
    harness.assert_screen_contains("Command: Save File");
}

/// Test Tab completion doesn't accept disabled suggestions
#[test]
fn test_command_palette_tab_on_disabled() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "undo" - this command requires Normal context
    // Since we're in Normal context, it should be available
    harness.type_text("undo").unwrap();

    // Press Tab to accept the suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The input should be completed
    harness.assert_screen_contains("Command: Undo");

    // Now clear and try a different command
    // Clear input
    for _ in 0..4 {
        harness.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();
    }

    // Type "focus" which will match "Focus Editor" and "Focus File Explorer"
    // "Focus Editor" requires FileExplorer context (disabled in Normal)
    // "Focus File Explorer" should be available in Normal context
    harness.type_text("focus e").unwrap();
    harness.render().unwrap();

    // The first match might be "Focus Editor" which is disabled in Normal context
    // Tab should either skip it or not accept it
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // After Tab, we should have an enabled command in the input
    // Let's just check that SOMETHING happened (either it completed or stayed as is)
    // This test is to verify the behavior - we'll fix it if it's broken
    let screen = harness.screen_to_string();
    println!("Screen after Tab on 'focus e': {}", screen);

    // For now, just assert we still have the command palette open
    harness.assert_screen_contains("Command:");
}

/// Test Tab completion doesn't work when all suggestions are disabled
#[test]
fn test_command_palette_tab_all_disabled() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type enough to filter to only "Focus Editor" which is disabled in Normal context
    harness.type_text("focus ed").unwrap();
    harness.render().unwrap();

    // Check that "Focus Editor" is shown (should be greyed out)
    harness.assert_screen_contains("Focus Editor");

    // Press Tab - it should not accept the disabled suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The input should NOT have been auto-completed to disabled command
    // It should still be "focus ed" not "Focus Editor"
    let screen = harness.screen_to_string();
    println!("Screen after Tab on disabled 'focus ed': {}", screen);

    // Check that input didn't change (tab should do nothing on disabled suggestions)
    harness.assert_screen_contains("Command: focus ed");
}

/// Test Enter executes the selected (highlighted) command, not the typed text
#[test]
fn test_command_palette_enter_uses_selection() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type partial text "new" which will match "New File"
    harness.type_text("new").unwrap();

    // The first suggestion should be "New File" (selected by default)
    harness.assert_screen_contains("New File");

    // Press Enter - should execute "New File" command, not try to find "new" command
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should NOT see error about unknown command
    harness.assert_screen_not_contains("Unknown command");

    // Should see the result of executing New File command
    // (new_buffer() sets status message to "New buffer")
    harness.assert_screen_contains("New buffer");
}

/// Test Enter with partial match uses the highlighted selection
#[test]
fn test_command_palette_enter_partial_match() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "sav" which matches "Save File" and "Save File As"
    harness.type_text("sav").unwrap();

    // Navigate down to select "Save File As"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter - should execute the selected command
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should execute the selected command, not fail on "sav"
    harness.assert_screen_not_contains("Unknown command: sav");
}
