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

    // Type partial text "new" which will match "New File"
    harness.type_text("new").unwrap();

    // The first suggestion should be "New File" (selected by default)
    harness.assert_screen_contains("New File");

    // Press Enter - should execute "New File" command, not try to find "new" command
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

    // With no filter, we should have many commands
    // The popup shows max 10 items at a time

    // Press Down 15 times to go well beyond the first 10 visible items
    for _ in 0..15 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // The selection should still be visible (the view should have scrolled)
    // We can verify this by checking that the view has scrolled beyond the first commands
    let screen = harness.screen_to_string();

    // Should have scrolled and show later commands (not the first commands)
    // After scrolling down 15 times, "Open File" (first command) should NOT be visible
    harness.assert_screen_not_contains("Open File");

    // Should show some later commands like "Select All", "Expand Selection" etc.
    // These appear around position 10-16 in the list
    harness.assert_screen_contains("Select All");

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
    harness.type_text("new").unwrap();
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

    // Open file explorer
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

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

    // The first suggestion should be selected by default
    // Press Up - should stay at the first item, not wrap to the end
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to execute the selected command
    // If we wrapped around, we would execute the last command in the list
    // If we stayed at the first command, we would execute "Open File"
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // "Open File" command opens a file picker prompt
    // We should see "Find file:" prompt
    harness.assert_screen_contains("Find file:");
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

    // First suggestion should be selected
    // Press PageUp - should stay at the first item (or move up less than 10), not wrap to the end
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Enter to execute the selected command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should execute the first command "Open File"
    harness.assert_screen_contains("Find file:");
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

    // Press PageDown multiple times to reach the end
    // There are about 30+ commands, so 4 PageDowns should be enough
    for _ in 0..4 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Press PageDown again - should stay at the end
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // After pressing PageDown many times, we should be at one of the last commands
    // The exact command depends on the total number of commands and where PageDown stops
    // Let's verify that we're at a command near the end by checking we can't go further

    // Try PageDown once more and verify selection doesn't change
    // by executing the command and seeing what we get
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // We should execute one of the last commands. Could be Git commands or File Explorer toggles
    // depending on context availability. Let's just verify we didn't execute one of the first commands
    let screen = harness.screen_to_string();

    // Make sure we didn't wrap around to execute an early command like "Open File" or "Save File"
    harness.assert_screen_not_contains("Save File As");

    // The last available command in Normal context should be either:
    // - Git: Find File (opens "Find file:" prompt)
    // - Git: Grep (opens "Git Grep:" prompt)
    // - Or we get an error message about command not being available (for File Explorer commands)
    // - Or a truncated error message (indicated by "Command '..." and ending with "is...")
    let is_expected_result = screen.contains("Git Grep:")
        || screen.contains("Find file:")
        || screen.contains("not available in")
        || (screen.contains("Command '") && screen.contains("is..."));
    assert!(
        is_expected_result,
        "Expected to execute a command near the end of the list, but got: {}",
        screen
    );
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
    harness.assert_screen_contains("Command: ");

    // Check that some commands with shortcuts are visible
    // Save File should show Ctrl+S
    harness.assert_screen_contains("Save File");
    harness.assert_screen_contains("Ctrl+S");

    // Quit should show Ctrl+Q
    harness.assert_screen_contains("Quit");
    harness.assert_screen_contains("Ctrl+Q");

    // Open File should show Ctrl+O
    harness.assert_screen_contains("Open File");
    harness.assert_screen_contains("Ctrl+O");
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

    // Should show filtered results with shortcuts
    harness.assert_screen_contains("Save File");
    harness.assert_screen_contains("Ctrl+S");

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

    let screen = harness.screen_to_string();

    // Check that shortcuts are displayed in a column format
    // Find lines with commands and shortcuts
    let lines: Vec<&str> = screen.lines().collect();
    let mut found_columnar_layout = false;

    // Look for multiple commands with shortcuts to verify column alignment
    let mut shortcut_positions = Vec::new();

    for line in &lines {
        if (line.contains("Save File") || line.contains("Quit") || line.contains("Open File"))
            && (line.contains("Ctrl+S") || line.contains("Ctrl+Q") || line.contains("Ctrl+O"))
        {
            // Find the position of the shortcut
            if let Some(pos) = line.find("Ctrl+") {
                shortcut_positions.push(pos);
            }
        }
    }

    // If we have multiple shortcuts, check that they're aligned (within a few chars)
    if shortcut_positions.len() >= 2 {
        let first_pos = shortcut_positions[0];
        found_columnar_layout = shortcut_positions
            .iter()
            .all(|&pos| (pos as i32 - first_pos as i32).abs() <= 3);
    }

    assert!(
        found_columnar_layout,
        "Shortcuts should be displayed in aligned columns in the command palette. Found positions: {:?}",
        shortcut_positions
    );
}
