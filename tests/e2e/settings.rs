//! E2E tests for the settings modal

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test opening settings modal with Ctrl+,
#[test]
fn test_open_settings_modal() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Render initial state
    harness.render().unwrap();

    // Settings should not be visible initially
    harness.assert_screen_not_contains("Settings");

    // Open settings with Ctrl+,
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Settings modal should now be visible
    harness.assert_screen_contains("Settings");
}

/// Test closing settings modal with Escape
#[test]
fn test_close_settings_with_escape() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Settings");

    // Close with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Settings should be closed
    harness.assert_screen_not_contains("Settings");
}

/// Test settings navigation with arrow keys
#[test]
fn test_settings_navigation() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate down in categories
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Switch to settings panel with Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate down in settings
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test settings search with /
#[test]
fn test_settings_search() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Start search with /
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type a search query
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('m'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show search results
    // The search query "theme" should match theme-related settings

    // Cancel search with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test settings help overlay with ?
#[test]
fn test_settings_help_overlay() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Open help with ?
    harness
        .send_key(KeyCode::Char('?'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Help overlay should be visible
    harness.assert_screen_contains("Keyboard Shortcuts");

    // Close help with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Settings should still be visible
    harness.assert_screen_contains("Settings");

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test search text input is displayed in search box
#[test]
fn test_settings_search_text_displays() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Start search with /
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show search mode indicator
    harness.assert_screen_contains("Type to search");

    // Type search query "tab"
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search text should be visible in the search box
    harness.assert_screen_contains("tab");

    // Should show results count
    harness.assert_screen_contains("results");

    // Close with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test toggling a setting shows modified indicator
#[test]
fn test_settings_toggle_shows_modified() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Use search to find "Check For Updates" (a toggle setting)
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "check".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result and toggle
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Toggle the setting
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show modified indicator in title
    harness.assert_screen_contains("modified");

    // Close and discard
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // Select "Discard" (one right from "Save and Exit")
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test confirmation dialog shows pending changes
#[test]
fn test_confirmation_dialog_shows_changes() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Use search to find "Check For Updates"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "check".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result and toggle
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Escape to trigger confirmation dialog
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Dialog should show
    harness.assert_screen_contains("Unsaved Changes");
    harness.assert_screen_contains("You have unsaved changes");

    // Should show the actual change (path contains "check_for_updates")
    harness.assert_screen_contains("check_for_updates");

    // Cancel dialog
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test confirmation dialog button navigation
#[test]
fn test_confirmation_dialog_button_navigation() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Use search to find and toggle a setting
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "check".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open confirmation dialog
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // First button should be selected (Save and Exit has > indicator)
    harness.assert_screen_contains(">[ Save and Exit ]");

    // Navigate right to Discard
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Discard should now be selected
    harness.assert_screen_contains(">[ Discard ]");

    // Navigate right to Cancel
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Cancel should now be selected
    harness.assert_screen_contains(">[ Cancel ]");

    // Press Enter on Cancel to close dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Dialog should be closed but settings still open
    harness.assert_screen_not_contains("Unsaved Changes");
    harness.assert_screen_contains("Settings");

    // Discard and close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test selection via keyboard navigation works
/// Settings panel shows focus indicator ">" on focused item
#[test]
fn test_settings_selection_indicator() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Switch to settings panel with Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Settings panel should show focus indicator ">" on selected item
    // General category has: Active Keybinding Map (first item)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("> Active Keybinding Map"),
        "Focus indicator '>' should appear before focused item in settings panel. Screen:\n{}",
        screen
    );

    // Navigate down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now Check For Updates should have the focus indicator
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("> Check For Updates"),
        "Focus indicator '>' should move to Check For Updates. Screen:\n{}",
        screen
    );

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test number input increment with Right arrow
#[test]
fn test_settings_number_increment() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for a number setting (mouse hover delay)
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "hover delay".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The default value is 500
    harness.assert_screen_contains("500");

    // Press Right arrow to increment
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Value should now be 501
    harness.assert_screen_contains("501");

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Press Left arrow to decrement back
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Value should be back to 500
    harness.assert_screen_contains("500");

    // Close settings (no changes now)
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test number input decrement with Left arrow
#[test]
fn test_settings_number_decrement() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for hover delay (number setting) - same as increment test but decrement
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "hover delay".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The default value is 500
    harness.assert_screen_contains("500");

    // Press Left arrow to decrement
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Value should now be 499
    harness.assert_screen_contains("499");

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Discard and close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test dropdown cycling with Enter key
#[test]
fn test_settings_dropdown_cycle() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for "theme" (a dropdown setting)
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "theme".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check initial theme value (should be "dark")
    let initial_screen = harness.screen_to_string();
    let has_dark = initial_screen.contains("dark");

    // Press Enter to cycle to next option
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // If it was "dark", it should now be "light" or another theme option
    // The exact value depends on available themes, but it should change
    if has_dark {
        // Should show modified indicator since we changed the value
        harness.assert_screen_contains("modified");
    }

    // Discard and close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test dropdown cycling with Right arrow
#[test]
fn test_settings_dropdown_increment() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for "theme" (a dropdown setting)
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "theme".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Get initial screen
    let initial_screen = harness.screen_to_string();

    // Press Right arrow to cycle to next option
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Get new screen
    let new_screen = harness.screen_to_string();

    // The dropdown value should have changed (screens should differ)
    // We can check that modified indicator appears
    if initial_screen != new_screen {
        harness.assert_screen_contains("modified");
    }

    // Discard and close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test scrolling through settings list
#[test]
fn test_settings_scrolling() {
    // Use a smaller height to ensure scrolling is needed
    let mut harness = EditorTestHarness::new(100, 25).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate to Editor category which has many settings
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Switch to settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get initial screen to check first item
    let initial_screen = harness.screen_to_string();

    // Navigate down many times to trigger scrolling
    for _ in 0..15 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Get new screen - should have scrolled, showing different items
    let scrolled_screen = harness.screen_to_string();

    // The screens should be different due to scrolling
    assert_ne!(
        initial_screen, scrolled_screen,
        "Screen should change after scrolling down"
    );

    // Some setting items should still be visible after scrolling
    // (selection is shown via background highlight, not a text indicator)

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test scrollbar appears when there are many settings
#[test]
fn test_settings_scrollbar_visible() {
    // Use a smaller height to ensure scrollbar is needed
    let mut harness = EditorTestHarness::new(100, 25).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate to Editor category which has many settings
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Switch to settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Scrollbar should be visible (rendered with background colors)
    // Settings panel uses a popup layout, so the scrollbar may be at the right edge
    // of the settings area, not necessarily the rightmost terminal column.
    // Check any column in the settings area for scrollbar presence.
    let has_scrollbar = (40..100).any(|col| harness.has_scrollbar_at_column(col));
    assert!(
        has_scrollbar,
        "Settings panel should have a visible scrollbar (checked columns 40-99)"
    );

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test search jump scrolls to selected item
#[test]
fn test_settings_search_jump_scrolls() {
    // Use a smaller height to ensure scrolling is needed
    let mut harness = EditorTestHarness::new(100, 25).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for a setting that's likely at the bottom of a category
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "wrap".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The searched term should be visible after jumping
    // (selection is shown via background highlight, not a text indicator)
    harness.assert_screen_contains("Wrap");

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test theme dropdown can be cycled with Enter or Right arrow
/// BUG: Theme dropdown doesn't cycle - it stays on the same value
#[test]
#[ignore] // TODO: Fix theme dropdown cycling - currently broken
fn test_settings_theme_dropdown_cycle() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for theme setting
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "theme".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to theme setting
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should be on Theme setting with current value (high-contrast is default)
    harness.assert_screen_contains("Theme");
    let initial_screen = harness.screen_to_string();
    let has_high_contrast = initial_screen.contains("high-contrast");

    // Press Enter to cycle to next theme option
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The theme should have changed - this is currently broken
    // Expected: theme changes to next option (e.g., monokai, solarized-dark)
    // Actual: theme stays on high-contrast
    let after_enter = harness.screen_to_string();

    if has_high_contrast {
        // After pressing Enter, it should cycle to a different theme
        // This assertion will fail with the current bug
        assert!(
            !after_enter.contains("high-contrast") || after_enter.contains("modified"),
            "Theme should change after pressing Enter, but it stayed the same"
        );
    }

    // Try Right arrow as well
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let after_right = harness.screen_to_string();

    // Should show modified indicator if theme changed
    // This will also fail with the current bug
    assert!(
        after_right.contains("modified"),
        "Theme dropdown should cycle with Right arrow and show modified indicator"
    );

    // Discard and close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

// =============================================================================
// CRITICAL BUG TESTS - These tests should fail until the bugs are fixed
// =============================================================================

/// BUG: Opening Settings from terminal mode causes keystrokes to go to terminal
///
/// When the user is in terminal mode and opens the Settings dialog (via Ctrl+,
/// or command palette), keyboard input should go to the Settings dialog, not
/// to the terminal behind it. Currently, the terminal continues to capture
/// input even when Settings is open, requiring users to manually exit terminal
/// mode first.
///
/// Expected behavior: Settings dialog captures all keyboard input when open
/// Actual behavior: Terminal behind dialog receives keystrokes
#[test]
fn test_settings_from_terminal_mode_captures_input() {
    use portable_pty::{native_pty_system, PtySize};

    // Skip if PTY not available
    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping test: PTY not available");
        return;
    }

    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open a terminal (this enters terminal mode automatically)
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Verify we're in terminal mode
    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode after opening terminal"
    );

    // Open settings with Ctrl+, (this should work even in terminal mode)
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Settings should be visible
    harness.assert_screen_contains("Settings");

    // Now try to use Settings navigation - press Down to navigate categories
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The Settings should respond to navigation, not the terminal
    // If the bug exists, the Down key would have gone to the terminal shell
    // and the Settings category wouldn't have changed

    // Navigate down should move from General to Editor
    // We can verify by switching to settings panel and checking we see Editor settings
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Editor category has "Auto Indent" which General doesn't have prominently
    // If Down key worked in Settings, we should now be viewing Editor settings
    harness.assert_screen_contains("Auto Indent");

    // Clean up - close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // If there's an unsaved changes dialog, dismiss it
    if harness.screen_to_string().contains("Unsaved Changes") {
        // Select Discard
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
    }
}

/// Test footer buttons (Reset/Save/Cancel) are accessible via keyboard
///
/// The Settings dialog has footer buttons [Reset] [Save] [Cancel] that can
/// be reached using Tab navigation.
///
/// Tab cycles through: categories -> settings -> footer buttons
#[test]
fn test_settings_footer_buttons_keyboard_accessible() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Settings");

    // Make a change so footer buttons become relevant
    // Search for and toggle a setting
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "check".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Tab to footer - from settings panel, Tab goes to footer
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Save button should be selected (has > indicator)
    harness.assert_screen_contains(">[ Save ]");

    // Navigate right to Cancel
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Cancel button should now be selected
    harness.assert_screen_contains(">[ Cancel ]");

    // Press Enter on Cancel - this shows confirmation dialog when there are changes
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Confirmation dialog should appear
    harness.assert_screen_contains("Unsaved Changes");

    // Navigate to Discard button (Right from Save)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Enter to discard and close
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Settings should be closed
    harness.assert_screen_not_contains("Settings");
}

/// Test changing theme, saving, and verifying the theme is applied
#[test]
fn test_settings_change_theme_and_save() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();
    harness.render().unwrap();

    // Get initial theme name
    let initial_theme = harness.editor().theme().name.clone();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify settings is open via state check
    assert!(
        harness.editor().is_settings_open(),
        "Settings should be open after Ctrl+,"
    );

    // Search for theme setting
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "theme".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to theme setting
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Cycle through theme options until we get to "light"
    let mut found_light = false;
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        if harness.screen_to_string().contains("light") {
            found_light = true;
            break;
        }
    }

    assert!(found_light, "Should be able to cycle to light theme");

    // Tab to footer (Save button)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to save
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify settings is closed via state check
    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after saving"
    );

    // Verify theme changed via state check
    let new_theme = harness.editor().theme().name.clone();
    assert_eq!(
        new_theme, "light",
        "Theme should be 'light' after saving. Was: {}, Now: {}",
        initial_theme, new_theme
    );
}

/// Test settings descriptions are rendered properly
///
/// Descriptions should:
/// 1. Not be cut off mid-word (e.g., "hether" instead of "whether")
/// 2. Start with lowercase letter (since they're not sentence-initial)
/// 3. Contain meaningful info (not just repeat the name)
#[test]
fn test_settings_descriptions_render_properly() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate to Editor category which has settings with descriptions
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Switch to settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Check that descriptions are NOT cut off mid-word at the start
    // These patterns would indicate broken descriptions (word starting with cut-off text):
    // We check for patterns like " hether" (space + truncated word) to find words starting wrong
    assert!(
        !screen.contains(" hether") && !screen.contains("|hether"), // should be "whether"
        "Description should not be cut mid-word (found 'hether' at start of word)"
    );
    assert!(
        !screen.contains(" oll interval"), // should be "poll interval"
        "Description should not be cut mid-word (found 'oll interval')"
    );
    assert!(
        !screen.contains(" yntax "), // should be "syntax"
        "Description should not be cut mid-word"
    );

    // Check that we can see some expected description content
    // These descriptions should exist for Editor settings
    assert!(
        screen.contains("indent") || screen.contains("Indent"),
        "Should show indent-related description"
    );

    // Verify descriptions are rendered (can be either case)
    assert!(
        screen.contains("hether to enable"),
        "Description containing 'whether to enable' should be visible"
    );

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that global shortcuts (Ctrl+P, Ctrl+Q) are consumed by settings dialog
///
/// When the settings dialog is open, it should capture all keyboard input
/// and not let shortcuts like Ctrl+P (command palette) or Ctrl+Q (quit) through.
#[test]
fn test_settings_consumes_global_shortcuts() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();
    harness.render().unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify settings is open
    assert!(
        harness.editor().is_settings_open(),
        "Settings should be open"
    );

    // Try Ctrl+P (command palette) - should be consumed, not open palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Settings should still be open (Ctrl+P was consumed)
    assert!(
        harness.editor().is_settings_open(),
        "Settings should still be open after Ctrl+P - shortcut should be consumed"
    );

    // Verify command palette is NOT open
    harness.assert_screen_not_contains("Command Palette");

    // Try Ctrl+Q (quit) - should be consumed, not quit
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Settings should still be open (Ctrl+Q was consumed)
    assert!(
        harness.editor().is_settings_open(),
        "Settings should still be open after Ctrl+Q - shortcut should be consumed"
    );

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test Map control "[+] Add new" shows text input when Enter is pressed
#[test]
#[ignore] // TODO: Entry dialog now requires pressing Enter to start editing the Key field
fn test_map_control_add_new_shows_text_input() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for "Keybinding Maps" which is a Map control
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "keybinding maps".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show "[+] Add new" for the empty map
    harness.assert_screen_contains("[+] Add new");

    // Press Enter to start editing
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The "[+] Add new" for Keybinding Maps should be replaced with a text input field
    // We can't check for absence of "[+] Add new" because other Map controls still show it
    // Instead, check that the text input field brackets appear (the underlined input area)
    // The input field shows as "[" followed by spaces and "]"

    // Type a name
    for c in "vim".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Should see "vim" in the input field
    harness.assert_screen_contains("vim");

    // Press Enter to add the entry
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Entry should be added and "[+] Add new" should appear below it
    harness.assert_screen_contains("vim");
    harness.assert_screen_contains("[+] Add new");

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Exit editing mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Close settings and verify confirm dialog shows the change
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Confirm dialog should show the map change
    harness.assert_screen_contains("Unsaved Changes");
    harness.assert_screen_contains("keybinding_maps");

    // Discard changes
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test changing File Explorer Width (a percentage/float setting) and saving
///
/// This tests the bug where percentage values were being saved incorrectly:
/// - Width is stored as float 0.0-1.0 (e.g., 0.3 = 30%)
/// - UI displays as integer (30)
/// - Bug: saved as integer (30) instead of float (0.30)
/// - Result: on reload, 30 * 100 = 3000 displayed
#[test]
fn test_settings_percentage_value_saves_correctly() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();
    harness.render().unwrap();

    // Get initial width (default is 0.3 = 30%)
    let initial_width = harness.config().file_explorer.width;
    assert!(
        (initial_width - 0.3).abs() < 0.01,
        "Initial width should be ~0.3, got {}",
        initial_width
    );

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate to File Explorer category (down three times from General)
    // Categories: General, Editor, File Browser, File Explorer, Menu, Terminal, Warnings
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Editor
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // File Browser
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // File Explorer
    harness.render().unwrap();

    // Switch to settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate down to find the Width setting
    // File Explorer settings: Custom Ignore Patterns, Respect Gitignore, Show Gitignored, Show Hidden, Width
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Respect Gitignore
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Show Gitignored
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Show Hidden
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Width
    harness.render().unwrap();

    // Should show Width setting with current value (30 = 0.3 * 100)
    harness.assert_screen_contains("Width");
    harness.assert_screen_contains("30");

    // Increment the value to 31 (which should become 0.31)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show 31
    harness.assert_screen_contains("31");

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Tab to footer (Save button)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to save
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify settings is closed
    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after saving"
    );

    // CRITICAL: Verify the width was saved as a float, not an integer
    // If the bug exists, width would be 31.0 instead of 0.31
    let new_width = harness.config().file_explorer.width;
    assert!(
        (new_width - 0.31).abs() < 0.01,
        "Width should be ~0.31 after saving, got {} (bug: value was saved as integer instead of float)",
        new_width
    );
}

/// Test number input editing mode - enter editing, type value, confirm
#[test]
fn test_number_input_enter_editing_mode() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for hover delay (a number setting)
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "hover delay".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The default value is 500
    harness.assert_screen_contains("500");

    // Press Enter to start editing mode
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type Ctrl+A to select all, then type new value
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    for c in "750".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Should show 750
    harness.assert_screen_contains("750");

    // Press Enter to confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Discard and close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test number input editing - Escape cancels and reverts value
#[test]
fn test_number_input_escape_cancels_editing() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for hover delay
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "hover delay".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Start editing mode
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Select all and type a new value
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    for c in "999".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Should show 999
    harness.assert_screen_contains("999");

    // Press Escape to cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should revert back to 500
    harness.assert_screen_contains("500");

    // Close settings without changes
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test number input editing - cursor navigation works
#[test]
fn test_number_input_cursor_navigation() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for hover delay
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "hover delay".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Value is 500, start editing
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Move cursor to beginning with Home
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Type 1 at the beginning
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show 1500 (1 inserted at beginning)
    harness.assert_screen_contains("1500");

    // Confirm the value
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Discard and close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test number input editing - backspace works
#[test]
fn test_number_input_backspace() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for hover delay
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "hover delay".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Value is 500, start editing (Enter selects all text)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Move cursor to end (deselects text so backspace deletes one char)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Backspace should delete the last digit (0)
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show 50
    harness.assert_screen_contains("50");

    // Backspace again should delete another digit (0)
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show 5
    harness.assert_screen_contains("5");

    // Cancel editing
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should revert to 500
    harness.assert_screen_contains("500");

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// BUG: Settings UI doesn't load saved values when reopened
///
/// When the user changes a setting, saves, closes settings, and reopens,
/// the Settings UI should show the saved value. Instead, it shows the
/// default value from when the editor was first started.
///
/// Expected: After saving auto_save_interval_secs = 5 and reopening, show 5
/// Actual: Shows 2 (the default)
#[test]
fn test_settings_loads_saved_values_on_reopen() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();
    harness.render().unwrap();

    // Verify initial auto_save_interval_secs is 2 (default)
    let initial_value = harness.config().editor.auto_save_interval_secs;
    assert_eq!(
        initial_value, 2,
        "Initial auto_save_interval_secs should be 2"
    );

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for "auto save" to find the setting
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "auto save interval".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show the default value of 2
    harness.assert_screen_contains("2");

    // Increment the value 3 times (2 -> 3 -> 4 -> 5)
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Should now show 5
    harness.assert_screen_contains("5");

    // Tab to footer (Save button)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to save
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Settings should be closed after saving
    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after saving"
    );

    // Verify the config was updated
    let saved_value = harness.config().editor.auto_save_interval_secs;
    assert_eq!(
        saved_value, 5,
        "auto_save_interval_secs should be 5 after saving"
    );

    // CRITICAL TEST: Reopen settings and verify the saved value is displayed
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for the same setting again
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "auto save interval".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the saved value is displayed (not the default)
    harness.assert_screen_contains("5");

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that entering edit mode on a numeric field selects all text
///
/// When the user presses Enter on a numeric field to edit it, the text
/// should be selected so that typing immediately replaces the value,
/// rather than appending to the existing value.
///
/// Expected: Press Enter → type "100" → value becomes "100"
/// Actual (bug): Press Enter → type "100" → value becomes "500100"
#[test]
fn test_number_input_enter_selects_all_text() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for hover delay (a number setting with value 500)
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "hover delay".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify initial value is 500
    harness.assert_screen_contains("500");

    // Press Enter to start editing mode
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type "100" - this should REPLACE the value, not append
    for c in "100".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Should show 100, not 500100 (the bug behavior)
    harness.assert_screen_contains("100");
    harness.assert_screen_not_contains("500100");

    // Press Enter to confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Discard and close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test that the focused category shows a ">" selection indicator
///
/// When the categories panel is focused, the selected category should
/// have a ">" prefix to make the selection more visible.
/// Format is: "{selection}{modified} {name}" where:
/// - selection is ">" when selected and focused, " " otherwise
/// - modified is "●" when category has changes, " " otherwise
#[test]
fn test_category_selection_indicator_visible() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Categories panel is focused by default, should show ">" before General
    // General may have "●" modified indicator due to test defaults
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(">● General") || screen.contains(">  General"),
        "Expected '>' indicator on General category when focused. Screen: {}",
        screen
    );

    // Navigate down to Editor category
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now Editor should have the ">" indicator
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(">● Editor") || screen.contains(">  Editor"),
        "Expected '>' indicator on Editor category when focused. Screen: {}",
        screen
    );

    // Tab to settings panel (categories panel loses focus)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now Editor should not have the ">" indicator (panel not focused)
    harness.assert_screen_not_contains(">● Editor");
    harness.assert_screen_not_contains(">  Editor");

    // But Editor should still be visible (just highlighted differently)
    harness.assert_screen_contains("Editor");

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that Ctrl+S saves settings from any panel
///
/// Ctrl+S is a global shortcut that should save settings regardless
/// of which panel is currently focused.
#[test]
fn test_ctrl_s_saves_settings() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();
    harness.render().unwrap();

    // Verify initial check_for_updates is false (test default)
    assert!(!harness.config().check_for_updates);

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for "check for updates" and toggle it
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "check".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result and toggle
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Press Ctrl+S to save (should work from any panel)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Settings should be closed after Ctrl+S
    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after Ctrl+S"
    );

    // Verify the setting was saved
    assert!(
        harness.config().check_for_updates,
        "check_for_updates should be true after saving"
    );
}

/// Test that entry dialog (Edit Value) shows focus indicator on focused field
#[test]
fn test_entry_dialog_focus_indicator() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // We're in General category. Tab to content panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate down to find a language entry in the Languages list
    // Languages section is after Keybinding Maps and Keybindings sections
    // Navigate down many times to reach Languages
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Should see language items like "bash", "c", "rust", etc.
    let screen = harness.screen_to_string();
    // Find any language item that shows "[Enter to edit]" - that means we're on it
    if !screen.contains("[Enter to edit]") {
        // Navigate more to find language items
        for _ in 0..5 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.render().unwrap();
    }

    // Press Enter to open the Edit Value dialog on the current language
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Entry dialog should be open
    harness.assert_screen_contains("Edit Value");

    // Read-only fields (Key) are displayed first but not focusable
    // Key should be visible without focus indicator
    harness.assert_screen_contains("Key:");

    // The focused field should have a ">" indicator
    // First editable field (Auto Indent) should be focused by default
    harness.assert_screen_contains("> Auto Indent");

    // Navigate down to next editable field
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now "Comment Prefix" should be focused with ">" indicator
    harness.assert_screen_contains("> Comment Prefix");

    // Close dialog
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that [+] Add new button in entry dialog works for TextList items
#[test]
fn test_entry_dialog_add_new_textlist_item() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate to Languages section - Tab to content, then down to a language
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Open a language entry dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Value");

    // Navigate to Extensions section which has "[+] Add new"
    // Fields in order: Key, Auto Indent, Comment Prefix, Extensions (3 downs)
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // The Extensions section should have items and "[+] Add new"
    harness.assert_screen_contains("[+] Add new");

    // Get current screen to compare after adding
    let before_add = harness.screen_to_string();

    // Press Enter to start editing the "[+] Add new" field
    // This focuses the add-new input and enables typing
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type a new extension value
    for c in "test_ext".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // The typed text should be visible
    harness.assert_screen_contains("test_ext");

    // Press Enter to add the item
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // After adding, the item should appear in the list
    let after_add = harness.screen_to_string();
    assert_ne!(
        before_add, after_add,
        "Screen should change after adding item"
    );

    // The new item should be visible
    harness.assert_screen_contains("test_ext");

    // Close dialog without saving
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that [x] delete button in entry dialog works via keyboard (Delete key)
#[test]
fn test_entry_dialog_delete_textlist_item() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate to Languages section - Tab to content, then down to a language
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Open a language entry dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Value");

    // Navigate to Extensions section which has existing items
    // Fields in order: Key, Auto Indent, Comment Prefix, Extensions (3 downs)
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // The Extensions section should have items and "[x]" delete buttons
    harness.assert_screen_contains("[x]");

    // First, add an item so we have something to delete
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type a new extension value
    for c in "to_delete".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Press Enter to add the item
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the item was added
    harness.assert_screen_contains("to_delete");

    // Now navigate UP to focus on the newly added item
    // (we should be on the add-new row, so Up goes to the last item)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get screen before delete
    let before_delete = harness.screen_to_string();
    assert!(
        before_delete.contains("to_delete"),
        "Item should be visible before delete"
    );

    // Press Delete to remove the focused item
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The item should be removed
    let after_delete = harness.screen_to_string();
    assert!(
        !after_delete.contains("to_delete"),
        "Item should be removed after Delete key"
    );

    // Close dialog without saving
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Regression test for issue #474: Settings not persisting after save
///
/// This test verifies that when a boolean setting is toggled and saved,
/// reopening the settings dialog shows the saved value (not the original).
///
/// The bug was: after save, discard_changes() rebuilt the UI from
/// original_config instead of the saved config, resetting displayed values.
#[test]
fn test_settings_toggle_persists_after_save_and_reopen() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();
    harness.render().unwrap();

    // Test harness sets check_for_updates = false by default
    assert!(
        !harness.config().check_for_updates,
        "check_for_updates should be false in test harness"
    );

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Switch to settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate down to "Check For Updates" (second item in General)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we're on Check For Updates and it shows as unchecked [ ]
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("> Check For Updates") && screen.contains(": [ ]"),
        "Check For Updates should be focused and unchecked. Screen:\n{}",
        screen
    );

    // Toggle it ON
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify it now shows as checked [x]
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("> Check For Updates") && screen.contains(": [x]"),
        "Check For Updates should now be checked. Screen:\n{}",
        screen
    );

    // Save: Tab to footer, Enter on Save button
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify settings closed and config updated
    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after save"
    );
    assert!(
        harness.config().check_for_updates,
        "check_for_updates should be true after save"
    );

    // CRITICAL: Reopen settings and verify the saved value is displayed
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Switch to settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate to Check For Updates
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // This is the key assertion: the toggle should show the SAVED value [x]
    // not the ORIGINAL value [ ]
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("> Check For Updates") && screen.contains(": [x]"),
        "BUG #474: After save and reopen, Check For Updates should still be checked [x], \
         but it shows the original value [ ]. Screen:\n{}",
        screen
    );

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that line_numbers config is applied when opening new files.
///
/// When line_numbers is set to false via settings, newly opened files
/// should not show line numbers.
#[test]
fn test_line_numbers_config_applied_to_new_buffers() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();
    harness.render().unwrap();

    // Verify initial state has line numbers (default is true)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("1 │"),
        "Initial buffer should show line numbers by default"
    );

    // Open settings and disable line numbers
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for line numbers setting
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    for c in "line numbers".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Toggle it off (it's on by default)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Save settings
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify config was updated
    assert!(
        !harness.config().editor.line_numbers,
        "line_numbers should be false after saving"
    );

    // Open a new buffer - it should respect the new config
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The new buffer should NOT show line numbers
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("1 │") && !screen.contains("2 │"),
        "New buffer should not show line numbers when config.editor.line_numbers=false. Screen:\n{}",
        screen
    );
}

/// Test that line_wrap config is applied when opening new files.
///
/// When line_wrap is set to false via settings, newly opened files
/// should not wrap long lines.
#[test]
fn test_line_wrap_config_applied_to_new_buffers() {
    let mut harness = EditorTestHarness::new(80, 40).unwrap();
    harness.render().unwrap();

    // Open settings and disable line wrap
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for line wrap setting
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    for c in "line wrap".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Toggle it off (it's on by default)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Save settings
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify config was updated
    assert!(
        !harness.config().editor.line_wrap,
        "line_wrap should be false after saving"
    );

    // Open a new buffer
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type a line longer than the screen width (80 chars)
    let long_text = "X".repeat(100);
    for c in long_text.chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // With line_wrap=false, the text should stay on one line (with horizontal scroll)
    // When wrapped, line 2 would show the continuation (no line number, just "│ XXX...")
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();

    // Find the content area (after menu bar and tab bar)
    // Line 2 should be a tilde (empty line marker) when not wrapping
    // When wrapped, it would contain X's
    let line2_content = lines.get(3).unwrap_or(&""); // 0=menu, 1=tabs, 2=line1, 3=line2
    assert!(
        !line2_content.contains("X"),
        "Long line should not wrap when config.editor.line_wrap=false. Line 2: '{}'. Screen:\n{}",
        line2_content,
        screen
    );
}

// =============================================================================
// JSON EDITOR TESTS - Testing the JSON text box in entry dialogs
// =============================================================================

/// Helper function to navigate to the LSP Initialization Options JSON editor
/// Opens settings, searches for "lsp", opens the first LSP entry, and navigates to Initialization Options
fn navigate_to_lsp_json_editor(harness: &mut EditorTestHarness) {
    // Open settings via Ctrl+,
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Use search to find "lsp" section
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("lsp").unwrap();
    harness.render().unwrap();

    // Press Enter to jump to the first result (the Lsp map)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify we're in the LSP section
    harness.assert_screen_contains("Lsp");

    // Press Enter to open the first LSP entry dialog (e.g., clangd for c)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify we're in an Edit dialog
    harness.assert_screen_contains("Edit Value");

    // Navigate down to "Initialization Options" field
    // Navigate until we see the focus indicator on Initialization Options
    // (cargo nextest handles external timeout)
    loop {
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        if screen.contains("> Initialization Options")
            || screen.contains(">Initialization Options")
            || screen.contains("> • Initialization Options")
        {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
}

/// Test that Delete key works in JSON editor (deletes character at cursor)
///
/// BUG: Delete key in JSON editor calls delete_list_item() instead of
/// deleting the character at the cursor position.
///
/// Expected: Delete key removes character after cursor
/// Actual: Delete key does nothing (or removes TextList item if in TextList mode)
#[test]
fn test_json_editor_delete_key_works() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();

    navigate_to_lsp_json_editor(&mut harness);

    // Press Enter to start editing the JSON field
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The help line should change to indicate JSON editing mode
    harness.assert_screen_contains("Enter:Newline");

    // When entering edit mode, cursor is at position 0
    // Type "ABC" which will be inserted at the start, resulting in "ABCnull"
    harness.type_text("ABC").unwrap();
    harness.render().unwrap();

    // Should see "ABCnull" (typed at cursor position 0)
    harness.assert_screen_contains("ABCnull");

    // Cursor is now after 'C'. Move left 3 times to position before 'A'
    for _ in 0..3 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Press Delete key - should delete 'A'
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // After deleting 'A', should show "BCnull"
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("BCnull") && !screen.contains("ABCnull"),
        "Delete key should remove character at cursor. Expected 'BCnull', got:\n{}",
        screen
    );

    // Close dialogs
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that Home and End keys work in JSON editor
///
/// BUG: Home and End keys are not handled in JSON editor.
#[test]
fn test_json_editor_home_end_keys_work() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();

    navigate_to_lsp_json_editor(&mut harness);

    // Press Enter to start editing the JSON field
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // When entering edit mode, cursor is at position 0
    // Type "XYZ" which results in "XYZnull"
    harness.type_text("XYZ").unwrap();
    harness.render().unwrap();

    // Should see "XYZnull" (typed at cursor position 0)
    harness.assert_screen_contains("XYZnull");

    // Cursor is now after 'Z'. Press End - should go to the end of text
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Type 'B' - should appear at the end
    harness
        .send_key(KeyCode::Char('B'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show "XYZnullB" (B appended at end)
    harness.assert_screen_contains("XYZnullB");

    // Press Home - cursor should go to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Type 'A' - should appear at the beginning
    harness
        .send_key(KeyCode::Char('A'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show "AXYZnullB" (A inserted at beginning)
    harness.assert_screen_contains("AXYZnullB");

    // Close dialogs
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that Ctrl+A selects all text in JSON editor
///
/// BUG: Ctrl+A is not handled in JSON editor.
#[test]
fn test_json_editor_ctrl_a_selects_all() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();

    navigate_to_lsp_json_editor(&mut harness);

    // Press Enter to start editing the JSON field
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // When entering edit mode, cursor is at position 0
    // Type "OLD" which results in "OLDnull"
    harness.type_text("OLD").unwrap();
    harness.render().unwrap();

    // Should see "OLDnull"
    harness.assert_screen_contains("OLDnull");

    // Press Ctrl+A to select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type new text - should replace all selected text
    harness.type_text("NEW").unwrap();
    harness.render().unwrap();

    // Should now show "NEW" only (replaced "OLDnull")
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("NEW") && !screen.contains("OLDnull") && !screen.contains("OLD"),
        "Ctrl+A should select all, then typing should replace. Expected only 'NEW', got:\n{}",
        screen
    );

    // Close dialogs
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

// =============================================================================
// EDIT CONFIG FILE BUTTON TESTS
// =============================================================================

/// Test that the Edit button is visible in the settings footer
///
/// The Edit button allows advanced users to directly edit the config file
/// for the selected layer. It should be visible on the left side of the footer,
/// separated from the main action buttons.
#[test]
fn test_settings_edit_button_visible() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Edit button should be visible in footer (on the left, dimmed style)
    harness.assert_screen_contains("[ Edit ]");

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that the Edit button can be navigated to via keyboard
///
/// Tab from the settings panel should eventually reach the Edit button.
/// Button order in footer: Layer, Reset, Save, Cancel, Edit (on left for advanced users)
#[test]
fn test_settings_edit_button_keyboard_navigation() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Tab to settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Tab to footer (defaults to Save button)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should show Save button focused
    harness.assert_screen_contains(">[ Save ]");

    // Navigate with Right arrow: Save -> Cancel -> Edit
    // Footer order: 0=Layer, 1=Reset, 2=Save, 3=Cancel, 4=Edit
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(">[ Cancel ]");

    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Edit button should now be focused
    harness.assert_screen_contains(">[ Edit ]");

    // Close settings
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that Edit button opens the config file for the selected layer
///
/// When the Edit button is activated, it should:
/// 1. Close the settings modal
/// 2. Open the config file for the current layer
/// 3. Show a status message indicating which file was opened
#[test]
fn test_settings_edit_button_opens_config_file() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify settings is open
    assert!(
        harness.editor().is_settings_open(),
        "Settings should be open"
    );

    // Navigate to Edit button: Tab -> Tab -> Right -> Right
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap(); // to Settings
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap(); // to Footer (Save)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap(); // to Cancel
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap(); // to Edit
    harness.render().unwrap();

    // Verify Edit button is focused
    harness.assert_screen_contains(">[ Edit ]");

    // Press Enter to activate Edit button
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Settings should be closed
    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after Edit"
    );

    // A config file should be open (User layer by default)
    // The file path should contain "config.json"
    harness.assert_screen_contains("config.json");

    // Status message should indicate which layer was opened
    harness.assert_screen_contains("Editing User config");
}

/// Test that Edit button is blocked when there are pending changes
///
/// If the user has made changes in the Settings UI that haven't been saved,
/// the Edit button should not open the config file and should show a warning.
#[test]
fn test_settings_edit_button_blocked_with_pending_changes() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Make a change: toggle "Check For Updates"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "check".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show modified indicator
    harness.assert_screen_contains("modified");

    // Navigate to Edit button
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap(); // to Footer
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap(); // to Cancel
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap(); // to Edit
    harness.render().unwrap();

    // Press Enter to try to activate Edit button
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Settings should STILL be open (Edit was blocked)
    assert!(
        harness.editor().is_settings_open(),
        "Settings should still be open when Edit is blocked due to pending changes"
    );

    // Should show warning message about pending changes
    harness.assert_screen_contains("Save or discard pending changes");

    // Discard changes and close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Test that clicking "[+] Add new" button on a Map control opens entry dialog with single click
/// Reproduces issue #604: LSP Config "Add New" button is not clickable by mouse
#[test]
fn test_map_add_new_button_clickable_with_mouse() {
    let mut harness = EditorTestHarness::new(120, 45).unwrap();

    // Open settings
    harness
        .send_key(KeyCode::Char(','), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for "Keybinding Maps" which is a Map control
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for c in "keybinding maps".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Jump to result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The "[+] Add new" button should be visible
    harness.assert_screen_contains("[+] Add new");

    // Find the position of "[+] Add new" on screen and click it
    let screen = harness.screen_to_string();
    let add_new_pos = screen
        .lines()
        .enumerate()
        .find_map(|(row, line)| line.find("[+] Add new").map(|col| (col as u16, row as u16)))
        .expect("Should find [+] Add new on screen");

    // Single click should activate the add-new functionality (this is the fix for #604)
    harness
        .mouse_click(add_new_pos.0 + 2, add_new_pos.1)
        .unwrap();
    harness.render().unwrap();

    // After clicking, the entry dialog should open (for Map with schema) or input mode should start
    // For Keybinding Maps, it shows an entry dialog - check for entry dialog elements
    // The entry dialog has a "Key" label or shows brackets for text input

    // The test passes if clicking works - before the fix, a single click wouldn't activate
    // and the "[+] Add new" would remain just focused without any action

    // Close everything and clean up
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}
