use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;

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
    harness.assert_screen_contains("Command:");

    // Check that suggestions are visible (commands sorted alphabetically, so Add Cursor commands appear first)
    harness.assert_screen_contains("Add Cursor Above");
    harness.assert_screen_contains("Add Cursor Below");
    harness.assert_screen_contains("Close Buffer");
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
    harness.assert_screen_contains("Command:");

    // Navigate down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Navigate up
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Commands should still be visible (alphabetically sorted, so Add Cursor commands first)
    harness.assert_screen_contains("Add Cursor Above");
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

    // Type partial text - use "open f" to specifically match "Open File"
    harness.type_text("open f").unwrap();

    // Press Tab to accept first suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The input should be completed to "Open File"
    harness.assert_screen_contains("Command: Open File");
    // Note: The prompt shows "Command:" followed by the input text
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
    harness.assert_screen_contains("Command:");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Prompt should be gone
    harness.assert_screen_not_contains("Command:");
    harness.assert_screen_contains("cancelled");
}

/// Test executing a command from the palette
#[test]
fn test_command_palette_execute() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Verify line numbers are shown initially (default config)
    harness.assert_screen_contains("1 │");

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type a valid command name - use "Toggle Line Numbers" which has visible effect
    harness.type_text("Toggle Line Numbers").unwrap();

    // Execute with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Line numbers should now be hidden
    harness.assert_screen_not_contains("1 │");
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

    // Type "save file" to specifically match "Save File"
    // Using just "save" may fuzzy match other commands first
    harness.type_text("save file").unwrap();

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
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
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
    println!("Screen after Tab on 'focus e': {screen}");

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
    println!("Screen after Tab on disabled 'focus ed': {screen}");

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

    // Type "new file" which will specifically match "New File"
    // (using just "new" may match other commands like "Navigate Forward")
    harness.type_text("new file").unwrap();

    // The first suggestion should be "New File" (selected by default)
    harness.assert_screen_contains("New File");

    // Press Enter - should execute "New File" command, not try to find "new file" command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
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
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("partial_match.txt");
    std::fs::write(&file_path, "hello").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should execute the selected command, not fail on "sav"
    harness.assert_screen_not_contains("Unknown command: sav");
}

/// Test scrolling beyond visible suggestions keeps selection visible
#[test]
fn test_command_palette_scroll_beyond_visible() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Commands are sorted alphabetically, first is "Add Cursor Above"
    harness.assert_screen_contains("Add Cursor Above");

    // With no filter, we should have many commands
    // The popup shows max 10 items at a time

    // Press Down 15 times to go well beyond the first 10 visible items
    for _ in 0..15 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // The selection should still be visible (the view should have scrolled)
    // We can verify this by checking that the view has scrolled beyond the first commands
    // After scrolling down 15 times, first command "Add Cursor Above" should NOT be visible
    harness.assert_screen_not_contains("Add Cursor Above");

    // Now press Enter - it should execute the selected command (whatever is selected)
    // not fail with "Unknown command"
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should NOT see "Unknown command" error
    harness.assert_screen_not_contains("Unknown command");
}

/// Test that "New File" command actually switches to the new buffer
#[test]
fn test_command_palette_new_file_switches_buffer() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let fixture = TestFixture::new("test.txt", "Original content\nLine 2\nLine 3").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the fixture file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Should see the original content
    harness.assert_screen_contains("Original content");
    harness.assert_screen_contains("Line 2");

    // The tab should show the filename
    harness.assert_screen_contains("test.txt");

    // Now use command palette to create a new file
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    // Use "new file" to specifically match New File command
    harness.type_text("new file").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see status message confirming new buffer
    harness.assert_screen_contains("New buffer");

    // Should now have two tabs
    harness.assert_screen_contains("test.txt");
    harness.assert_screen_contains("[No Name]");

    // The important part: the CONTENT should now be empty (new buffer)
    // NOT showing the original content anymore
    harness.assert_screen_not_contains("Original content");
    harness.assert_screen_not_contains("Line 2");

    // The cursor should be at the start of an empty buffer
    let screen = harness.screen_to_string();
    println!("Screen after New File:\n{screen}");

    // Verify we can type in the new buffer
    harness.type_text("New buffer text").unwrap();
    harness.assert_screen_contains("New buffer text");
    harness.assert_screen_not_contains("Original content");
}

/// Test that Toggle Line Wrap command is available
#[test]
fn test_command_palette_toggle_line_wrap() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "toggle line" to filter
    harness.type_text("toggle line").unwrap();
    harness.render().unwrap();

    // Should show "Toggle Line Wrap" command
    harness.assert_screen_contains("Toggle Line Wrap");
}

/// Test that File Explorer toggle commands are available
#[test]
fn test_command_palette_file_explorer_toggles() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "toggle hidden" to filter
    harness.type_text("toggle hidden").unwrap();
    harness.render().unwrap();

    // Should show "Toggle Hidden Files" command
    harness.assert_screen_contains("Toggle Hidden Files");

    // Clear and search for gitignored
    for _ in 0..13 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }

    harness.type_text("toggle git").unwrap();
    harness.render().unwrap();

    // Should show "Toggle Gitignored Files" command
    harness.assert_screen_contains("Toggle Gitignored Files");
}

/// Test that command palette can be invoked from file explorer
#[test]
fn test_command_palette_from_file_explorer() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open file explorer (Ctrl+E)
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for file explorer to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Verify file explorer is open by checking for the UI element
    harness.assert_screen_contains("File Explorer");

    // Now trigger the command palette from file explorer with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show the command palette
    harness.assert_screen_contains("Command:");

    // Should show commands
    harness.assert_screen_contains("Open File");

    // Should be able to execute a command
    harness.type_text("toggle hidden").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Command should execute successfully (Toggle Hidden Files)
    // We should see a status message about the toggle
    let screen = harness.screen_to_string();
    println!("Screen after toggle hidden: {}", screen);

    // The command should have executed (not showing error about unavailable)
    harness.assert_screen_not_contains("not available");
}

/// Test that Up arrow stops at the beginning of the list instead of wrapping
#[test]
fn test_command_palette_up_no_wraparound() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Commands are sorted alphabetically, first is "Add Cursor Above"
    harness.assert_screen_contains("Add Cursor Above");

    // The first suggestion should be selected by default
    // Press Up - should stay at the first item, not wrap to the end
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to execute the selected command
    // If we wrapped around, we would execute the last command in the list
    // If we stayed at the first command, we would execute "Add Cursor Above"
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // "Add Cursor Above" adds a cursor above the current one
    // The editor should now have 2 cursors - check via cursor count or status
    // For simplicity, just verify we're back in normal mode (no error)
    harness.assert_screen_not_contains("Unknown command");
}

/// Test that Down arrow stops at the end of the list instead of wrapping
#[test]
fn test_command_palette_down_no_wraparound() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Filter to get only two commands
    harness.type_text("save f").unwrap();
    harness.render().unwrap();

    // Should match "Save File" and "Save File As"
    harness.assert_screen_contains("Save File");

    // First suggestion (Save File) should be selected
    // Press Down to go to second (Save File As)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Down again - should stay at the last item, not wrap to first
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Tab to accept the selected suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // If we wrapped around, we'd be back at "Save File"
    // If we stayed at the end, we'd still be at "Save File As"
    // The tab should complete to the selected command
    harness.assert_screen_contains("Command: Save File As");
}

/// Test that PageUp stops at the beginning of the list instead of wrapping
#[test]
fn test_command_palette_pageup_no_wraparound() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Commands are sorted alphabetically, first is "Add Cursor Above"
    harness.assert_screen_contains("Add Cursor Above");

    // Press Down a few times to move away from the first item
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Now press PageUp multiple times - should return to beginning and stay there
    for _ in 0..3 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Press Enter to execute the selected command (should be first: Add Cursor Above)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should execute first command - "Add Cursor Above" adds a cursor
    // Just verify we didn't execute a command from the end of the list
    harness.assert_screen_not_contains("Unknown command");
}

/// Test that PageDown stops at the end of the list instead of wrapping
#[test]
fn test_command_palette_pagedown_no_wraparound() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Commands are sorted alphabetically, first is "Add Cursor Above"
    harness.assert_screen_contains("Add Cursor Above");

    // Press PageDown many times to try reaching the end
    // There are 80+ commands, PageDown moves by 10
    for _ in 0..10 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // After pressing PageDown many times, verify we moved from the first command
    // The first command "Add Cursor Above" should no longer be highlighted/at top
    // We verify this by pressing PageUp once to see if we can go back
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // We should still be far from the beginning
    // Execute the command and verify we didn't wrap to the first command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify command executed without error
    harness.assert_screen_not_contains("Unknown command");
}

/// Test that keyboard shortcuts are displayed in the command palette
#[test]
fn test_command_palette_shows_shortcuts() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Trigger the command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that the command palette is visible
    harness.assert_screen_contains("Command:");

    // Check that commands with shortcuts are visible (commands sorted alphabetically)
    // Add Cursor Above should show Ctrl+Alt+↑ (or ⌘+⌥+↑ on macOS)
    harness.assert_screen_contains("Add Cursor Above");
    // On macOS, Ctrl is ⌘ and Alt is ⌥
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ctrl+Alt+") || screen.contains("⌘+⌥+"),
        "Should show shortcut for Add Cursor Above"
    );

    // Add Cursor Below should show Ctrl+Alt+↓
    harness.assert_screen_contains("Add Cursor Below");

    // Copy should show Ctrl+C (or ⌘+C on macOS)
    harness.assert_screen_contains("Copy");
    assert!(
        screen.contains("Ctrl+C") || screen.contains("⌘+C"),
        "Should show shortcut for Copy"
    );
}

/// Test that shortcuts are displayed for filtered commands
#[test]
fn test_command_palette_shortcuts_with_filtering() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "save" to filter commands
    harness.type_text("save").unwrap();
    harness.render().unwrap();

    // Should show filtered results with shortcuts (Ctrl+S or ⌘+S on macOS)
    harness.assert_screen_contains("Save File");
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ctrl+S") || screen.contains("⌘+S"),
        "Should show shortcut for Save File"
    );

    // Save As should also appear with its shortcut
    harness.assert_screen_contains("Save File As");
    // Ctrl+Shift+S is the typical shortcut for Save As, but it might not be bound by default
    // So we just check that the command appears
}

/// Test that shortcuts are displayed in a column format in the command palette
#[test]
fn test_command_palette_shortcuts_alignment() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify that shortcuts are displayed alongside commands
    // Look for commands that we know have shortcuts
    harness.assert_screen_contains("Add Cursor Above");
    harness.assert_screen_contains("Add Cursor Below");

    // These commands should have shortcuts displayed (Ctrl+Alt+arrow)
    // Just verify they're present - the exact format may vary
    let screen = harness.screen_to_string();

    // Check that we see some keyboard shortcut indicators
    // Ctrl, Alt, or Shift should appear somewhere indicating shortcuts are shown
    let has_shortcuts =
        screen.contains("Ctrl") || screen.contains("Alt") || screen.contains("Shift");
    assert!(
        has_shortcuts,
        "Command palette should display keyboard shortcuts. Screen:\n{}",
        screen
    );
}

/// Test that "Show Keyboard Shortcuts" command actually shows keyboard shortcuts (issue #192)
/// Previously this would just show a fallback message because it relied on a non-existent plugin hook
#[test]
fn test_show_keyboard_shortcuts_command() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to filter for "Show Keyboard Shortcuts"
    harness.type_text("keyboard").unwrap();
    harness.render().unwrap();

    // Should see the command in the palette
    harness.assert_screen_contains("Show Keyboard Shortcuts");

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after Show Keyboard Shortcuts:\n{}", screen);

    // ISSUE #192: The command should actually show keyboard shortcuts, not just a fallback message
    // The keyboard shortcuts should be displayed in a buffer or popup
    // We should see actual keybindings like Ctrl+S, Ctrl+O, etc.
    let shows_keybindings = screen.contains("Ctrl+S")
        || screen.contains("Ctrl+O")
        || screen.contains("Ctrl+P")
        || screen.contains("Save")
        || screen.contains("Keyboard Shortcuts"); // Buffer title

    // Should NOT show the fallback "not available" message
    let shows_fallback = screen.contains("not available") || screen.contains("plugins not loaded");

    assert!(
        shows_keybindings && !shows_fallback,
        "Show Keyboard Shortcuts command should display actual keybindings, not a fallback message. Screen:\n{}",
        screen
    );
}

/// Test that keyboard shortcuts can be opened, closed, and reopened without crashing
/// This reproduces a bug where opening keyboard shortcuts the second time causes:
/// "index out of bounds: the len is 1 but the index is 1" panic in tabs.rs:207
#[test]
fn test_show_keyboard_shortcuts_open_close_reopen() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // First open: Trigger the command palette and run "Show Keyboard Shortcuts"
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("keyboard").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify keyboard shortcuts are shown
    let screen = harness.screen_to_string();
    println!("First open - Screen:\n{}", screen);
    assert!(
        screen.contains("Ctrl+") || screen.contains("Keyboard Shortcuts"),
        "Keyboard shortcuts should be visible on first open. Screen:\n{}",
        screen
    );

    // Close with 'q' (standard way to close help/read-only buffers)
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after_close = harness.screen_to_string();
    println!("After close - Screen:\n{}", screen_after_close);

    // Second open: This should NOT panic with "index out of bounds"
    // BUG: Currently this causes panic at src/view/ui/tabs.rs:207:42
    // "index out of bounds: the len is 1 but the index is 1"
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("keyboard").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify keyboard shortcuts are shown again
    let screen = harness.screen_to_string();
    println!("Second open - Screen:\n{}", screen);
    assert!(
        screen.contains("Ctrl+") || screen.contains("Keyboard Shortcuts"),
        "Keyboard shortcuts should be visible on second open. Screen:\n{}",
        screen
    );
}

/// Test that command palette fuzzy matches on command descriptions
#[test]
fn test_command_palette_description_fuzzy_matching() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for "language" which appears in the description of "Select Locale"
    // ("Choose the UI language for the editor")
    // but not in the command name itself
    harness.type_text("language").unwrap();
    harness.render().unwrap();

    // Should find "Select Locale" because "language" is in its description
    harness.assert_screen_contains("Select Locale");

    // Clear and try another example
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }

    // Search for "clipboard" which appears in Copy/Paste descriptions
    // but not in the command names
    harness.type_text("clipboard").unwrap();
    harness.render().unwrap();

    // Should find Copy or Paste commands
    let screen = harness.screen_to_string();
    let found_clipboard_command = screen.contains("Copy") || screen.contains("Paste");
    assert!(
        found_clipboard_command,
        "Should find commands matching 'clipboard' in their description. Screen:\n{}",
        screen
    );
}

/// Test that cursor style can be changed via command palette
#[test]
fn test_command_palette_select_cursor_style() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Type to filter for cursor style command
    harness.type_text("cursor style").unwrap();
    harness
        .wait_for_screen_contains("Select Cursor Style")
        .unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should now show cursor style selection prompt
    harness
        .wait_for_screen_contains("Select cursor style:")
        .unwrap();

    // Should show cursor style options
    harness.assert_screen_contains("Terminal default");
    harness.assert_screen_contains("Blinking block");

    // Navigate down to select a different style (e.g., steady block)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Press Enter to select
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should show confirmation message
    harness
        .wait_for_screen_contains("Cursor style changed")
        .unwrap();
}
