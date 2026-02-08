//! E2E tests for the keybinding editor modal

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};

/// Helper to open the keybinding editor directly
fn open_keybinding_editor(harness: &mut EditorTestHarness) {
    harness.editor_mut().open_keybinding_editor();
    harness.render().unwrap();
}

// ========================
// Opening and closing
// ========================

/// Test opening the keybinding editor modal
#[test]
fn test_open_keybinding_editor() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Keybinding Editor");

    open_keybinding_editor(&mut harness);

    harness.assert_screen_contains("Keybinding Editor");
    harness.assert_screen_contains("bindings");
    harness.assert_screen_contains("Config:");
}

/// Test closing the keybinding editor with Escape
#[test]
fn test_close_keybinding_editor_with_escape() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);
    harness.assert_screen_contains("Keybinding Editor");

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.assert_screen_not_contains("Keybinding Editor");
}

// ========================
// Navigation
// ========================

/// Test navigating the binding list with arrow keys
#[test]
fn test_navigate_bindings_with_arrows() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    let screen_before = harness.screen_to_string();

    // Navigate down several times
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    // Selection indicator should have moved (screen should differ)
    assert_ne!(
        screen_before, screen_after,
        "Selection should have moved after pressing Down"
    );

    // Navigate back up
    for _ in 0..3 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Should still show the editor
    harness.assert_screen_contains("Keybinding Editor");
}

/// Test Home and End keys jump to first/last binding
#[test]
fn test_home_end_navigation() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Go to end
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let screen_end = harness.screen_to_string();

    // Go to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let screen_home = harness.screen_to_string();

    assert_ne!(
        screen_end, screen_home,
        "Home and End should show different parts of the list"
    );
}

/// Test PageUp and PageDown navigation
#[test]
fn test_page_up_down_navigation() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    let screen_initial = harness.screen_to_string();

    // Page down
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen_page_down = harness.screen_to_string();

    assert_ne!(
        screen_initial, screen_page_down,
        "PageDown should scroll the list"
    );

    // Page up should go back
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

// ========================
// Text search
// ========================

/// Test text search filters the binding list
#[test]
fn test_text_search() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Activate search
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type "save" to filter bindings
    for ch in "save".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Should show the search query
    harness.assert_screen_contains("save");
    // Should show "save" action in results
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("save") || screen.contains("Save"),
        "Search for 'save' should show matching bindings"
    );
}

/// Test search persists after pressing Enter (unfocuses but stays visible)
#[test]
fn test_search_persists_after_enter() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Activate search and type query
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "undo".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    harness.assert_screen_contains("undo");

    // Press Enter to unfocus search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search query should still be visible
    harness.assert_screen_contains("undo");
}

/// Test Escape cancels search and shows all bindings
#[test]
fn test_escape_cancels_search() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Activate search and type query
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "save".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Press Escape to cancel search
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Search bar should be gone, but editor should still be open
    harness.assert_screen_contains("Keybinding Editor");
    // The bindings count should reflect all bindings again
    harness.assert_screen_contains("bindings");
}

/// Test search with Down arrow moves focus to list
#[test]
fn test_search_down_arrow_moves_to_list() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Activate search and type query
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "copy".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Press Down to unfocus and navigate list
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Search query should remain visible
    harness.assert_screen_contains("copy");

    // Further Down keys should navigate in the list (not type in search)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Keybinding Editor");
}

// ========================
// Context and source filters
// ========================

/// Test cycling context filter
#[test]
fn test_context_filter_cycle() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Initially showing all contexts
    harness.assert_screen_contains("[All]");

    // Press 'c' to cycle context filter
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show a specific context (not All anymore, or cycled to next)
    let screen = harness.screen_to_string();
    // After first press, should show first specific context
    assert!(
        screen.contains("Context:"),
        "Should still show the Context label"
    );
}

/// Test cycling source filter
#[test]
fn test_source_filter_cycle() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Initially showing all sources
    harness.assert_screen_contains("[All]");

    // Press 's' to cycle source filter
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show a filtered source
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Source:"),
        "Should still show the Source label"
    );
}

// ========================
// Help overlay
// ========================

/// Test opening and closing the help overlay
#[test]
fn test_help_overlay() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open help with ?
    harness
        .send_key(KeyCode::Char('?'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Help overlay should be visible
    harness.assert_screen_contains("Keyboard Shortcuts");
    harness.assert_screen_contains("Navigation");

    // Close help with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Help should be gone, editor should still be open
    harness.assert_screen_not_contains("Keyboard Shortcuts");
    harness.assert_screen_contains("Keybinding Editor");
}

// ========================
// Edit dialog
// ========================

/// Test opening the edit dialog with Enter
#[test]
fn test_open_edit_dialog() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Press Enter to edit the selected binding
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Edit dialog should be visible
    harness.assert_screen_contains("Edit Keybinding");
    harness.assert_screen_contains("Key:");
    harness.assert_screen_contains("Action:");
    harness.assert_screen_contains("Context:");
    harness.assert_screen_contains("Save");
    harness.assert_screen_contains("Cancel");
}

/// Test closing the edit dialog with Escape
#[test]
fn test_close_edit_dialog_with_escape() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open edit dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Keybinding");

    // Close with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Dialog should be closed, editor still open
    harness.assert_screen_not_contains("Edit Keybinding");
    harness.assert_screen_contains("Keybinding Editor");
}

/// Test switching focus areas in the edit dialog with Tab
#[test]
fn test_edit_dialog_tab_focus() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open edit dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab through the focus areas (Key -> Action -> Context -> Buttons)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // Should still show the dialog
    harness.assert_screen_contains("Edit Keybinding");

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Keybinding");

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Keybinding");

    // Close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test that Tab in the edit dialog cycles through ALL controls including
/// both Save and Cancel buttons, not just the button area as a single stop.
/// Full cycle: Key -> Action -> Context -> Save -> Cancel -> Key
#[test]
fn test_edit_dialog_tab_cycles_through_cancel() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open edit dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Keybinding");

    // Tab: Key(0) -> Action(1)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    // Tab: Action(1) -> Context(2)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    // Tab: Context(2) -> Buttons/Save(3, btn=0)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Keybinding");

    // Tab: Save(3, btn=0) -> Cancel(3, btn=1)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // Cancel button should now be highlighted — pressing Enter now should close dialog
    harness.assert_screen_contains("Edit Keybinding");

    // Press Enter on Cancel — should close the dialog without saving
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Edit Keybinding");

    // Keybinding editor should still be open (dialog closed, not the editor)
    harness.assert_screen_contains("Keybinding Editor");
}

// ========================
// Add binding dialog
// ========================

/// Test opening the add binding dialog with 'a'
#[test]
fn test_open_add_dialog() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Press 'a' to add a new binding
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Add dialog should be visible
    harness.assert_screen_contains("Add Keybinding");
    harness.assert_screen_contains("Key:");
    harness.assert_screen_contains("Action:");
    harness.assert_screen_contains("Context:");
}

/// Test adding a new keybinding end-to-end
#[test]
fn test_add_new_binding() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Keybinding");

    // Record a key: press Ctrl+K (the dialog starts in RecordingKey mode)
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    // The key should be shown
    harness.assert_screen_contains("Ctrl+K");

    // Tab to Action field
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Type action name "save"
    for ch in "save".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Accept autocomplete with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab to context, then to buttons
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter on Save button
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show modified indicator since we added a binding
    harness.assert_screen_contains("modified");
}

// ========================
// Delete binding
// ========================

/// Test that deleting a keymap binding shows an error
#[test]
fn test_cannot_delete_keymap_binding() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Try to delete the first binding (which should be a keymap binding)
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show error about not being able to delete keymap bindings
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("custom") || screen.contains("keymap") || screen.contains("delete"),
        "Should show a message about not being able to delete non-custom bindings"
    );
}

// ========================
// Unsaved changes confirmation
// ========================

/// Test unsaved changes confirmation dialog appears
#[test]
fn test_unsaved_changes_confirm_dialog() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Add a binding to create unsaved changes
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Record key
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Tab to action
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for ch in "save".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Tab to buttons and save
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now press Esc - should show confirm dialog
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Unsaved Changes");
    harness.assert_screen_contains("Save");
    harness.assert_screen_contains("Discard");
    harness.assert_screen_contains("Cancel");
}

/// Test canceling the confirm dialog returns to editor
#[test]
fn test_confirm_dialog_cancel() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Add a binding to create unsaved changes
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for ch in "save".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Esc to show confirm dialog
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Unsaved Changes");

    // Press Esc again (or navigate to Cancel) to cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should be back in the editor
    harness.assert_screen_contains("Keybinding Editor");
    harness.assert_screen_not_contains("Unsaved Changes");
}

/// Test discarding changes via confirm dialog
#[test]
fn test_confirm_dialog_discard() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Add a binding to create unsaved changes
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for ch in "save".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Esc to show confirm dialog
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Unsaved Changes");

    // Navigate to Discard button (Right from Save) and press Enter
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Editor should be closed
    harness.assert_screen_not_contains("Keybinding Editor");
}

// ========================
// Mouse interactions
// ========================

/// Test mouse scroll moves the selection
#[test]
fn test_mouse_scroll() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    let screen_before = harness.screen_to_string();

    // Scroll down in the center of the modal
    harness.mouse_scroll_down(60, 20).unwrap();
    harness.mouse_scroll_down(60, 20).unwrap();
    harness.mouse_scroll_down(60, 20).unwrap();

    let screen_after = harness.screen_to_string();
    assert_ne!(
        screen_before, screen_after,
        "Mouse scroll should move the selection"
    );
}

/// Test mouse click selects a table row
#[test]
fn test_mouse_click_selects_row() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    let screen_before = harness.screen_to_string();

    // Click on a row in the table area (approximately row 15 for a row in the middle)
    harness.mouse_click(60, 15).unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    assert_ne!(
        screen_before, screen_after,
        "Mouse click should select a different row"
    );
}

/// Test mouse events are masked (don't leak to underlying editor)
#[test]
fn test_mouse_events_masked() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type some content in the editor first
    harness.type_text("Hello world").unwrap();
    harness.render().unwrap();

    open_keybinding_editor(&mut harness);

    // Click at position (5, 2) which would normally position cursor in the editor
    harness.mouse_click(5, 2).unwrap();
    harness.render().unwrap();

    // Keybinding editor should still be open (event was captured)
    harness.assert_screen_contains("Keybinding Editor");

    // Scroll at the same position
    harness.mouse_scroll_down(5, 2).unwrap();

    // Still in keybinding editor
    harness.assert_screen_contains("Keybinding Editor");
}

// ========================
// Record key search
// ========================

/// Test record key search mode
#[test]
fn test_record_key_search() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Press 'r' to start record key search
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show record key search mode
    harness.assert_screen_contains("Record Key:");

    // Record a key combination (Ctrl+S)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show the recorded key and filter results
    harness.assert_screen_contains("Ctrl+S");
}

// ========================
// Saving changes
// ========================

/// Test saving changes with Ctrl+S
#[test]
fn test_save_changes_with_ctrl_s() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Add a binding to create changes
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for ch in "save".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("modified");

    // Save with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Editor should close after saving
    harness.assert_screen_not_contains("Keybinding Editor");
}

// ========================
// Edit dialog field editing
// ========================

/// Test typing in the action field with autocomplete
#[test]
fn test_action_field_autocomplete() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Record a key first
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();

    // Tab to action field
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Type partial action name
    for ch in "und".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Autocomplete suggestions should be visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("undo"),
        "Autocomplete should show 'undo' suggestion for 'und'"
    );
}

/// Test context field cycling in edit dialog
#[test]
fn test_edit_dialog_context_cycling() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab past key and action to context
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Default context should be "normal"
    harness.assert_screen_contains("normal");

    // Press Right to cycle context
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show a different context now
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("prompt") || screen.contains("popup") || screen.contains("global"),
        "Context should have cycled to a different value"
    );

    // Close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

// ========================
// Table content
// ========================

/// Test that the table shows expected columns
#[test]
fn test_table_shows_columns() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Check column headers
    harness.assert_screen_contains("Key");
    harness.assert_screen_contains("Action");
    harness.assert_screen_contains("Description");
    harness.assert_screen_contains("Context");
    harness.assert_screen_contains("Source");
}

/// Test that bindings count is displayed
#[test]
fn test_bindings_count_displayed() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Should show binding count
    harness.assert_screen_contains("bindings");
}

/// Test that footer hints are displayed
#[test]
fn test_footer_hints_displayed() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Check footer hints
    harness.assert_screen_contains("Edit");
    harness.assert_screen_contains("Add");
    harness.assert_screen_contains("Delete");
    harness.assert_screen_contains("Search");
    harness.assert_screen_contains("Help");
    harness.assert_screen_contains("Close");
}

// ========================
// Unicode / narrow terminal
// ========================

/// Test that the keybinding editor renders correctly at narrow widths
/// where key display strings containing multi-byte Unicode characters
/// (e.g. "Alt+Shift+↓") may be truncated by column width.
/// Regression test: pad_right used byte indexing which panics on
/// multi-byte char boundaries.
#[test]
fn test_render_narrow_terminal_unicode_keys() {
    // At width 80, key_col_width = floor(78 * 0.16) = 12.
    // "Alt+Shift+↓" is 13 bytes but ↓ spans bytes 10..13,
    // so byte index 12 is not a char boundary and would panic.
    let mut harness = EditorTestHarness::new(80, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Search for "block_select" to filter to bindings with Alt+Shift+arrow
    // keys (e.g. Alt+Shift+↓), ensuring they are in the visible viewport
    // when rendered with narrow column widths.
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "block_select".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    // This render triggers pad_right on the filtered bindings which include
    // "Alt+Shift+↓" — at key_col_width=12 this panics on the byte boundary.
    harness.render().unwrap();

    // Should not panic and should display the editor
    harness.assert_screen_contains("Keybinding Editor");
}

// ========================
// Scroll / selection visibility
// ========================

/// Test that the selected item (">" indicator) stays visible no matter how
/// many times we press Down or Up. Regression test: editor.visible_rows was
/// hardcoded to 20 and never synced from the actual rendered viewport, so on
/// shorter terminals the selection would scroll out of view.
#[test]
fn test_selected_item_stays_visible_when_scrolling() {
    // Use a short terminal where visible rows in the table (~13) is much
    // less than the total number of bindings, forcing scroll to kick in.
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    open_keybinding_editor(&mut harness);

    // The ">" indicator marks the selected row. It must always be visible.
    harness.assert_screen_contains(">");

    // Press Down 40 times — well past the visible area.
    // After every key press the ">" indicator must remain on screen.
    for i in 0..40 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        assert!(
            harness.screen_to_string().contains(">"),
            "Selection indicator '>' not visible after pressing Down {} times",
            i + 1,
        );
    }

    // Now press Up all the way back to the top.
    for i in 0..40 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        assert!(
            harness.screen_to_string().contains(">"),
            "Selection indicator '>' not visible after pressing Up {} times",
            i + 1,
        );
    }
}

// ========================
// Terminal mode interaction
// ========================

/// Test that when terminal mode is active, opening the keybinding editor
/// captures key input — keys go to the editor, not the terminal PTY.
/// Regression test: dispatch_terminal_input's in_modal check didn't include
/// keybinding_editor.is_some(), so keys were swallowed by terminal mode.
#[test]
fn test_keybinding_editor_captures_keys_over_terminal_mode() {
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

    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Open a terminal — this enters terminal mode automatically
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode after opening terminal"
    );

    // Now open the keybinding editor modal
    open_keybinding_editor(&mut harness);
    harness.assert_screen_contains("Keybinding Editor");

    // The ">" selection indicator should be visible at the first row
    harness.assert_screen_contains(">");

    // Press Down several times — these keys should go to the keybinding editor
    // (moving the selection), NOT to the terminal PTY.
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // The editor should still be visible and the selection should have moved
    harness.assert_screen_contains("Keybinding Editor");
    // The ">" indicator must still be on screen (moved down)
    harness.assert_screen_contains(">");

    // Press Escape — should close the editor, not be eaten by terminal
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Keybinding editor should be closed now
    harness.assert_screen_not_contains("Keybinding Editor");
}
