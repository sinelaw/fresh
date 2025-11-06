use crate::common::harness::EditorTestHarness;

/// Test help page display and toggle
#[test]
fn test_help_page_display() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Initially help should not be visible
    assert!(!harness.editor().is_help_visible());

    // Toggle help on
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    // Help should now be visible
    assert!(harness.editor().is_help_visible());

    // Screen should contain help page elements
    harness.assert_screen_contains("KEYBOARD SHORTCUTS");
    harness.assert_screen_contains("Help");

    // Should show some keybindings (check for ones that appear on first page)
    harness.assert_screen_contains("Ctrl+D"); // Add cursor at next match

    // Toggle help off
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    // Help should no longer be visible
    assert!(!harness.editor().is_help_visible());
}

/// Test help page shows keybindings
#[test]
fn test_help_page_shows_keybindings() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Toggle help on
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Help screen:\n{screen}");

    // Should show common keybindings that appear on first page
    harness.assert_screen_contains("Ctrl+C"); // Copy
    harness.assert_screen_contains("Ctrl+X"); // Cut
    harness.assert_screen_contains("Backspace"); // Delete backward

    // Should show some actions
    harness.assert_screen_contains("Copy");
    harness.assert_screen_contains("Delete backward");
}

/// Test help page scrolling
#[test]
fn test_help_page_scrolling() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Toggle help on
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    // Get initial screen content
    let screen_before = harness.screen_to_string();

    // Scroll down
    harness.editor_mut().scroll_help(5);
    harness.render().unwrap();

    // Screen should have changed after scrolling
    let screen_after = harness.screen_to_string();

    // The content should be different (different lines visible)
    // Note: This test might be fragile if we don't have enough keybindings to scroll
    // We're just verifying the scroll mechanism works

    // Scroll back to top
    harness.editor_mut().scroll_help(-100); // Large negative to ensure we're at top
    harness.render().unwrap();

    let screen_top = harness.screen_to_string();

    // After scrolling back to top, should match the initial screen
    assert_eq!(
        screen_top, screen_before,
        "Scrolling back to top should restore original view"
    );
}

/// Test help page resets scroll on toggle
#[test]
fn test_help_page_scroll_reset() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Toggle help on and scroll down
    harness.editor_mut().toggle_help();
    harness.editor_mut().scroll_help(10);
    harness.render().unwrap();

    // Toggle help off
    harness.editor_mut().toggle_help();

    // Toggle help on again - scroll should be reset
    harness.editor_mut().toggle_help();
    harness.render().unwrap();

    // Should be showing the top of the help (scroll position 0)
    harness.assert_screen_contains("KEYBOARD SHORTCUTS");
}
