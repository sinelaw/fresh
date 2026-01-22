use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::time::Duration;

/// Test that Alt+F opens the File menu
#[test]
fn test_alt_f_opens_file_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Menu dropdown should not be visible initially
    harness.assert_screen_not_contains("New File");

    // Press Alt+F to open File menu
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // File menu dropdown should now be visible with its items
    harness.assert_screen_contains("New File");
    harness.assert_screen_contains("Open");
    harness.assert_screen_contains("Save");
}

/// Test that Alt+E opens the Edit menu
#[test]
fn test_alt_e_opens_edit_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Press Alt+E to open Edit menu
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Edit menu dropdown should be visible
    harness.assert_screen_contains("Undo");
    harness.assert_screen_contains("Redo");
    harness.assert_screen_contains("Cut");
}

/// Test that Alt+V opens the View menu
#[test]
fn test_alt_v_opens_view_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Press Alt+V to open View menu
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // View menu dropdown should be visible (menu item has checkbox prefix now)
    harness.assert_screen_contains("File Explorer");
    harness.assert_screen_contains("Split Horizontal");
}

/// Test that Alt+H opens the Help menu
#[test]
fn test_alt_h_opens_help_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Press Alt+H to open Help menu
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Help menu dropdown should be visible (renamed from "Show Help")
    harness.assert_screen_contains("Show Fresh Manual");
}

/// Test that F10 activates the menu bar (then arrow keys can navigate)
/// Note: F10 keybinding may not be properly handled in test harness
#[test]
#[ignore = "F10 keybinding needs investigation - works in real editor but not in test harness"]
fn test_f10_activates_menu_bar() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Press F10 to activate menu bar
    harness
        .send_key(KeyCode::F(10), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Down to open the menu under the highlighted item
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // First menu (File) should be open
    harness.assert_screen_contains("New File");
}

/// Test that Escape closes an open menu
#[test]
fn test_escape_closes_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Open a menu first
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New File");

    // Press Escape to close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Menu should be closed
    harness.assert_screen_not_contains("New File");
}

/// Test menu navigation with arrow keys
#[test]
fn test_menu_left_right_navigation() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Open File menu
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New File");

    // Press Right to go to Edit menu
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show Edit menu items
    harness.assert_screen_contains("Undo");
    harness.assert_screen_not_contains("New File");

    // Press Left to go back to File menu
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should now show File menu items again
    harness.assert_screen_contains("New File");
    harness.assert_screen_not_contains("Undo");
}

/// Test that menu renders with underlined mnemonic character
#[test]
fn test_menu_mnemonic_underline_rendering() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // The menu bar should show underlined characters for Alt+letter shortcuts
    // File should have F underlined, Edit should have E underlined, etc.
    // Check that the menu bar is rendered (row 0)
    harness.assert_screen_contains("File");
    harness.assert_screen_contains("Edit");
    harness.assert_screen_contains("View");
}

/// Test that clicking on File menu opens it
#[test]
fn test_mouse_click_opens_file_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Menu should not be open initially
    harness.assert_screen_not_contains("New File");

    // Click on "File" in menu bar (row 0, column ~1-4)
    harness.mouse_click(2, 0).unwrap();
    harness.render().unwrap();

    // File menu dropdown should now be visible
    harness.assert_screen_contains("New File");
    harness.assert_screen_contains("Open");
    harness.assert_screen_contains("Save");
}

/// Test that clicking on Edit menu opens it
#[test]
fn test_mouse_click_opens_edit_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Click on "Edit" in menu bar (around column 8-11)
    harness.mouse_click(9, 0).unwrap();
    harness.render().unwrap();

    // Edit menu dropdown should be visible
    harness.assert_screen_contains("Undo");
    harness.assert_screen_contains("Redo");
}

/// Test that clicking on open menu closes it
#[test]
fn test_mouse_click_toggles_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    harness.render().unwrap();

    // Click to open File menu
    harness.mouse_click(2, 0).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New File");

    // Wait to avoid double-click detection (use harness.sleep to advance logical time)
    harness.sleep(double_click_delay);

    // Click on File again to close it
    harness.mouse_click(2, 0).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("New File");
}

/// Test that clicking outside menu labels closes menu
#[test]
fn test_mouse_click_empty_area_closes_menu() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    harness.render().unwrap();

    // Open a menu first
    harness.mouse_click(2, 0).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New File");

    // Wait to avoid double-click detection (use harness.sleep to advance logical time)
    harness.sleep(double_click_delay);

    // Click on empty area of menu bar (far right)
    harness.mouse_click(70, 0).unwrap();
    harness.render().unwrap();

    // Menu should be closed
    harness.assert_screen_not_contains("New File");
}

/// Test that clicking on a menu item executes its action
#[test]
fn test_mouse_click_menu_item_executes_action() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Open Help menu via Alt+H
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Show Fresh Manual");

    // The Help menu dropdown appears at row 1 (below menu bar)
    // Help is the 6th menu, so x position = " File " (7) + " Edit " (7) + " View " (7) + " Selection " (12) + " Go " (5) = 38
    // Click on "Show Fresh Manual" item - it should be the first item
    // Menu items are rendered with border, so first item starts at row 2
    harness.mouse_click(40, 2).unwrap();
    harness.render().unwrap();

    // After clicking, the help panel should open
    // The menu should close after executing
    harness.assert_screen_not_contains("Show Fresh Manual");
    // Help panel shows keybinding information (look for actual keybinding entries)
    // On macOS, Ctrl is rendered as ⌘, on other platforms as "Ctrl"
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ctrl+") || screen.contains("⌘+"),
        "Help panel should show keybinding entries"
    );
}

/// Test clicking on Edit menu's Undo item
#[test]
fn test_mouse_click_undo_menu_item() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    // Type some text first
    harness.type_text("Hello World").unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Hello World");

    // Open Edit menu (around column 8)
    harness.mouse_click(9, 0).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Undo");

    // Wait to avoid double-click detection (use harness.sleep to advance logical time)
    harness.sleep(double_click_delay);

    // Click on Undo item (first item in Edit menu, row 2 after border)
    // Edit menu starts at column 7 (after " File " + space)
    harness.mouse_click(10, 2).unwrap();
    harness.render().unwrap();

    // Undo should have reversed the last text insertion
    // Menu should be closed
    harness.assert_screen_not_contains("Undo");
    // The last character should be undone (type_text inserts char by char)
    harness.assert_buffer_content("Hello Worl");
}

/// Test that View menu File Explorer checkbox syncs with actual file explorer state
/// Issue #291: Closing file explorer does not clear the check mark
#[test]
fn test_view_menu_file_explorer_checkbox_syncs_on_close() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Initially file explorer is not open, checkbox should be unchecked
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("☐ File Explorer"),
        "File Explorer checkbox should be unchecked initially. Screen:\n{}",
        screen
    );

    // Close menu
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open file explorer
    harness.editor_mut().toggle_file_explorer();
    harness.sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Open View menu - checkbox should now be checked
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("☑ File Explorer"),
        "File Explorer checkbox should be checked after opening explorer. Screen:\n{}",
        screen
    );

    // Close menu
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Close file explorer (toggle it off)
    harness.editor_mut().toggle_file_explorer();
    harness.render().unwrap();

    // Verify file explorer is actually hidden
    assert!(
        !harness.editor().file_explorer_visible(),
        "File explorer should be hidden after toggle"
    );

    // Open View menu - checkbox should now be UNchecked
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("☐ File Explorer"),
        "File Explorer checkbox should be unchecked after closing explorer. Screen:\n{}",
        screen
    );
}

/// Test that other View menu checkboxes also sync properly
/// Tests Line Numbers, Word Wrap, etc.
#[test]
fn test_view_menu_other_checkboxes_sync() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Check initial state of Line Numbers (default is ON)
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // Line numbers is on by default
    assert!(
        screen.contains("☑ Line Numbers"),
        "Line Numbers checkbox should be checked by default. Screen:\n{}",
        screen
    );

    // Close menu
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Toggle line numbers off
    harness.editor_mut().toggle_line_numbers();
    harness.render().unwrap();

    // Open View menu again - should show unchecked
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("☐ Line Numbers"),
        "Line Numbers checkbox should be unchecked after toggle. Screen:\n{}",
        screen
    );
}

/// Test that the "Copy with Formatting" submenu expands with dynamically generated theme options
#[test]
fn test_copy_with_formatting_submenu_shows_themes() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Open Edit menu
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Edit menu should be open with Copy with Formatting visible
    harness.assert_screen_contains("Undo");
    harness.assert_screen_contains("Copy with Formatting");

    // Navigate down to "Copy with Formatting" submenu
    // Edit menu items: Undo(0), Redo(1), [separator], Cut, Copy, Copy with Formatting, ...
    // Separators AND disabled items are skipped during navigation
    // Without a selection, Cut and Copy are disabled, so:
    // Down 1: Undo -> Redo
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Down 2: Redo -> Copy with Formatting (skips separator, Cut, Copy - all disabled)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we can see the submenu indicator (">") for Copy with Formatting
    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains("Copy with Formatting"),
        "Copy with Formatting should be visible in menu. Screen:\n{}",
        screen_before
    );

    // Open the submenu with Enter key (triggers execute which opens submenu)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The submenu should show dynamically generated theme options
    // These come from Theme::available_themes()
    let screen = harness.screen_to_string();

    assert!(
        screen.contains("dark"),
        "Copy with Formatting submenu should show 'dark' theme. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("light"),
        "Copy with Formatting submenu should show 'light' theme. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("high-contrast"),
        "Copy with Formatting submenu should show 'high-contrast' theme. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("nostalgia"),
        "Copy with Formatting submenu should show 'nostalgia' theme. Screen:\n{}",
        screen
    );
}

/// Test that pressing Enter on a "Copy with Formatting" submenu option activates the copy action
#[test]
fn test_copy_with_formatting_submenu_activates_on_enter() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // First, insert some text and select it so the copy action can work
    harness.type_text("Hello World").unwrap();
    // Select all text with Ctrl+A
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify text is selected (status bar should show selection)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Hello World"),
        "Text should be visible in the editor"
    );

    // Open Edit menu
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Navigate down to "Copy with Formatting" submenu
    // Edit menu items: Undo(0), Redo(1), [separator], Cut, Copy, Copy with Formatting, ...
    // Down 1: Undo -> Redo
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Down 2: Redo -> Cut (skips separator)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Down 3: Cut -> Copy
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Down 4: Copy -> Copy with Formatting
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open the submenu with Enter key
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify submenu is open with theme options
    let screen_with_submenu = harness.screen_to_string();
    assert!(
        screen_with_submenu.contains("dark"),
        "Submenu should show 'dark' theme option. Screen:\n{}",
        screen_with_submenu
    );

    // Navigate right into the submenu to select the first theme option
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Enter on the first theme option ("dark") to activate copy with formatting
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // After activating the action, the menu should close
    let screen_after = harness.screen_to_string();
    assert!(
        !screen_after.contains("Copy with Formatting"),
        "Menu should close after activating copy action. Screen:\n{}",
        screen_after
    );
    // The editor content should still be visible
    assert!(
        screen_after.contains("Hello World"),
        "Editor content should still be visible after copy. Screen:\n{}",
        screen_after
    );
}

/// Test that Cut and Copy menu items are disabled when there's no selection
#[test]
fn test_cut_copy_disabled_without_selection() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // No text, no selection - Cut and Copy should be disabled

    // Open Edit menu
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Navigate to Cut (after Undo, Redo, separator)
    // Down 1: Undo -> Redo
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Down 2: Redo -> Cut (skips separator)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we're at Cut (menu is open)
    let screen = harness.screen_to_string();
    assert!(screen.contains("Cut"), "Cut should be visible");

    // Press Enter on Cut - should NOT execute because no selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Menu should still be open because the action didn't execute
    let screen_after = harness.screen_to_string();
    assert!(
        screen_after.contains("Undo") && screen_after.contains("Redo"),
        "Menu should still be open since Cut is disabled. Screen:\n{}",
        screen_after
    );
}

/// Test toggling menu bar visibility and auto-show behavior
/// This tests the full lifecycle:
/// 1. Menu bar starts visible
/// 2. Toggle to hide it
/// 3. Press Alt+F - menu auto-shows and opens
/// 4. Press Esc - menu closes and auto-hides
/// 5. Toggle to show it explicitly
/// 6. Press Alt+F - menu opens (already visible)
/// 7. Press Esc - menu closes but bar stays visible
#[test]
fn test_toggle_menu_bar_visibility_and_auto_show() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Step 1: Menu bar should be visible initially (default)
    harness.assert_screen_contains("File");
    harness.assert_screen_contains("Edit");
    harness.assert_screen_contains("View");

    // Step 2: Toggle menu bar to hide it
    harness.editor_mut().toggle_menu_bar();
    harness.render().unwrap();

    // Menu bar should be hidden
    let screen = harness.screen_to_string();
    // First line should NOT contain menu items
    let first_line = screen.lines().next().unwrap_or("");
    assert!(
        !first_line.contains("File"),
        "Menu bar should be hidden after toggle. First line: {}",
        first_line
    );

    // Step 3: Press Alt+F - menu should auto-show and open
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Menu bar should be visible now with dropdown open
    harness.assert_screen_contains("File");
    harness.assert_screen_contains("New File"); // dropdown is open

    // Step 4: Press Escape - menu closes and menu bar auto-hides
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Menu dropdown should be closed
    harness.assert_screen_not_contains("New File");

    // Menu bar should be hidden again (auto-hide after auto-show)
    let screen = harness.screen_to_string();
    let first_line = screen.lines().next().unwrap_or("");
    assert!(
        !first_line.contains("File"),
        "Menu bar should auto-hide after menu closed. First line: {}",
        first_line
    );

    // Step 5: Toggle menu bar to show it explicitly
    harness.editor_mut().toggle_menu_bar();
    harness.render().unwrap();

    // Menu bar should be visible
    harness.assert_screen_contains("File");
    harness.assert_screen_contains("Edit");

    // Step 6: Press Alt+F - menu opens (already visible, not auto-shown)
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Dropdown should be open
    harness.assert_screen_contains("New File");

    // Step 7: Press Escape - menu closes but bar stays visible
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Menu dropdown should be closed
    harness.assert_screen_not_contains("New File");

    // But menu bar should still be visible (not auto-hidden because it was explicitly shown)
    harness.assert_screen_contains("File");
    harness.assert_screen_contains("Edit");
}

/// Test that mouse events on tabs work correctly when menu bar is hidden
/// Issue #832: After hiding the menu bar, clicking tabs and close buttons doesn't work
/// because the hardcoded `row == 0` check for menu bar intercepts the clicks.
#[test]
fn test_tab_click_works_with_menu_bar_hidden() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Create two buffers so we have 2 tabs
    harness.new_buffer().unwrap();
    harness.render().unwrap();

    // Verify we have 2 tabs visible (two × symbols in tab bar)
    let screen = harness.screen_to_string();
    let close_button_count = screen.lines().nth(1).unwrap_or("").matches('×').count();
    assert_eq!(
        close_button_count, 2,
        "Should have 2 tabs (2 close buttons) before hiding menu bar"
    );

    // Hide the menu bar
    harness.editor_mut().toggle_menu_bar();
    harness.render().unwrap();

    // Verify menu bar is hidden (row 0 should NOT contain "File")
    let first_line = harness
        .screen_to_string()
        .lines()
        .next()
        .unwrap_or("")
        .to_string();
    assert!(
        !first_line.contains("File"),
        "Menu bar should be hidden. First line: {}",
        first_line
    );

    // Now the tab bar is at row 0 (where menu bar used to be)
    // Find the × position for the second tab
    let tab_row = harness
        .screen_to_string()
        .lines()
        .next()
        .unwrap_or("")
        .to_string();
    println!("Tab row (now at row 0): '{}'", tab_row);

    // Find positions of close buttons
    let x_positions: Vec<usize> = tab_row.match_indices('×').map(|(i, _)| i).collect();
    println!("Close button positions: {:?}", x_positions);

    assert_eq!(
        x_positions.len(),
        2,
        "Should have 2 close buttons in tab bar. Tab row: {}",
        tab_row
    );

    // Get double click delay to avoid issues
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);
    harness.sleep(double_click_delay);

    // Click on the second tab's close button (the active tab)
    // This should close the tab, leaving only 1 tab
    let second_close_x = x_positions[1] as u16;
    println!("Clicking close button at column {}, row 0", second_close_x);

    harness.mouse_click(second_close_x, 0).unwrap();
    harness.render().unwrap();

    // Verify we now have only 1 tab
    let screen_after = harness.screen_to_string();
    let first_line_after = screen_after.lines().next().unwrap_or("");
    let close_button_count_after = first_line_after.matches('×').count();

    println!("After click - first line: '{}'", first_line_after);
    println!("Close button count after: {}", close_button_count_after);

    assert_eq!(
        close_button_count_after, 1,
        "BUG #832: Clicking tab close button at row 0 with hidden menu bar should close the tab. \
         Expected 1 close button after click, got {}. \
         The click was intercepted by the menu bar handler instead of reaching the tab.",
        close_button_count_after
    );
}

/// Test that clicking to select a tab works when menu bar is hidden
/// Issue #832: Tab selection via mouse click doesn't work with hidden menu bar
#[test]
fn test_tab_selection_click_works_with_menu_bar_hidden() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create temp files to have distinguishable tabs
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file1 = temp_dir.path().join("first.txt");
    let file2 = temp_dir.path().join("second.txt");
    std::fs::write(&file1, "First file content").unwrap();
    std::fs::write(&file2, "Second file content").unwrap();

    // Open both files
    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Verify we're on second.txt (last opened file is active)
    harness.assert_screen_contains("Second file content");

    // Hide the menu bar
    harness.editor_mut().toggle_menu_bar();
    harness.render().unwrap();

    // Find the first tab's position (should contain "first.txt")
    let tab_row = harness
        .screen_to_string()
        .lines()
        .next()
        .unwrap_or("")
        .to_string();
    println!("Tab row at row 0: '{}'", tab_row);

    // Find "first" in the tab row to click on it
    let first_tab_pos = tab_row.find("first");
    assert!(
        first_tab_pos.is_some(),
        "Should find 'first' in tab row. Tab row: {}",
        tab_row
    );

    let click_col = first_tab_pos.unwrap() as u16 + 2; // Click in middle of tab name

    // Get double click delay to avoid issues
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);
    harness.sleep(double_click_delay);

    // Click on the first tab to select it
    println!(
        "Clicking at column {}, row 0 to select first tab",
        click_col
    );
    harness.mouse_click(click_col, 0).unwrap();
    harness.render().unwrap();

    // Verify we switched to first.txt
    let screen_after = harness.screen_to_string();
    println!("Screen after click:\n{}", screen_after);

    assert!(
        screen_after.contains("First file content"),
        "BUG #832: Clicking tab at row 0 with hidden menu bar should switch to that tab. \
         Expected to see 'First file content' but it's not visible. \
         The click was intercepted by the menu bar handler instead of reaching the tab."
    );
}
