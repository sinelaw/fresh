//! E2E tests for keyboard navigation bugs in the Settings UI Rulers control.
//!
//! Reproduces the issue where keyboard focus gets trapped inside the TextList
//! control (used by Rulers) after adding a ruler value. Once in editing mode,
//! Tab is consumed silently (doesn't exit the control or cycle focus), and
//! Up/Down navigate within the list items instead of moving to other settings.
//! The only way to save is Ctrl+S; standard navigation (Tab to Save button,
//! Escape then arrow keys) does not work as expected.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Navigate to the Editor > Rulers setting in the settings UI.
///
/// Opens settings, navigates to the Editor category, tabs to the settings
/// panel, and scrolls down to the Rulers item. Returns with focus on Rulers.
fn navigate_to_rulers(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();
    harness.render().unwrap();

    // Navigate to "Editor" category (2 Down from General)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Editor");

    // Tab to settings panel
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Use search to jump directly to rulers
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("rulers").unwrap();
    harness.render().unwrap();

    // Press Enter to jump to the result and exit search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify we can see the Rulers setting
    harness.assert_screen_contains("Rulers");
}

/// While in TextList editing mode (after typing into the Rulers input),
/// pressing Tab should exit editing mode. Currently Tab is silently consumed,
/// so the user cannot leave the TextList control without pressing Escape.
#[test]
fn test_settings_rulers_tab_exits_text_editing_mode() {
    let mut harness = EditorTestHarness::new(110, 40).unwrap();
    let _fixture = harness.load_buffer_from_text("test content").unwrap();

    navigate_to_rulers(&mut harness);

    // Activate the Rulers control (Enter starts editing mode)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type a ruler value
    harness.type_text("80").unwrap();
    harness.render().unwrap();

    // Press Enter to add the ruler (stays in editing mode on the add-new row)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the ruler was added
    harness.assert_screen_contains("80");

    // Capture the screen while still in TextList editing mode
    let screen_in_editing = harness.screen_to_string();

    // Press Tab — this should exit editing mode and move focus forward.
    // Bug: Tab is consumed silently while in TextList editing mode, so
    // the screen doesn't change and focus stays trapped.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after_tab = harness.screen_to_string();

    // The screen should change because Tab should either:
    // (a) exit text editing and move to the next setting, or
    // (b) exit text editing and move to the footer panel
    // If the screen is identical, Tab was consumed without effect.
    assert_ne!(
        screen_in_editing, screen_after_tab,
        "Tab while in TextList editing mode should exit editing and move \
         focus, but the screen did not change — Tab is consumed silently"
    );
}

/// After adding a ruler in settings, pressing Escape should exit text editing
/// mode, and then Up/Down should navigate between settings items (not within
/// the TextList sub-items).
#[test]
fn test_settings_rulers_escape_then_arrows_navigate_settings() {
    let mut harness = EditorTestHarness::new(110, 40).unwrap();
    let _fixture = harness.load_buffer_from_text("test content").unwrap();

    navigate_to_rulers(&mut harness);

    // Enter editing mode and add a ruler
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("120").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("120");

    // Press Escape to exit editing mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Capture screen state with focus on Rulers
    let screen_on_rulers = harness.screen_to_string();

    // Press Down — should move to the next setting below Rulers
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after_down = harness.screen_to_string();

    // Focus should have moved to a different setting
    assert_ne!(
        screen_on_rulers, screen_after_down,
        "Down arrow after Escape should move focus to the next setting \
         below Rulers, but the screen did not change — focus is trapped"
    );

    // Press Up to go back — should return to Rulers (or the setting above)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after_up = harness.screen_to_string();

    assert_ne!(
        screen_after_down, screen_after_up,
        "Up arrow should navigate back from the setting below Rulers, \
         but the screen did not change"
    );
}

/// Up/Down arrows while in TextList editing mode should not trap focus
/// within the list items when there's only one item plus the add-new row.
/// After adding a single ruler, Down should eventually allow leaving
/// the TextList control (not just cycle between the item and add-new row).
#[test]
fn test_settings_rulers_up_down_trapped_in_editing_mode() {
    let mut harness = EditorTestHarness::new(110, 40).unwrap();
    let _fixture = harness.load_buffer_from_text("test content").unwrap();

    navigate_to_rulers(&mut harness);

    // Enter editing mode and add a ruler
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("80").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // We're now in editing mode on the add-new row.
    // Capture screen state.
    let screen_start = harness.screen_to_string();

    // Press Down — in editing mode this calls focus_next() which does nothing
    // when already on the add-new row (last position). The screen won't change.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after_down = harness.screen_to_string();

    // Press Up — moves focus to the "80" item row
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Up again — we're on item 0, focus_prev() does nothing (stays at 0)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after_ups = harness.screen_to_string();

    // Down from add-new row should have no effect (trapped at bottom of list)
    // This demonstrates the focus trap: Down does nothing when at the end,
    // and there's no way to move to the next setting item below.
    assert_eq!(
        screen_start, screen_after_down,
        "Down from add-new row should have no visual effect (focus is trapped \
         at the bottom of the TextList — cannot reach the next setting)"
    );

    // The user is now stuck: Up goes to item 0 and stops, Down goes to
    // add-new and stops. Tab is consumed silently. Only Escape exits.
    // Verify that typing still works (we're in editing mode on item 0)
    harness.type_text("x").unwrap();
    harness.render().unwrap();

    // The "80" item should now show "80x" (cursor was at end after focus_prev)
    harness.assert_screen_contains("80x");
}
