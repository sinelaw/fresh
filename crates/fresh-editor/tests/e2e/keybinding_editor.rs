//! E2E tests for the keybinding editor modal

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};

/// Helper to open the keybinding editor directly
fn open_keybinding_editor(harness: &mut EditorTestHarness) {
    harness.editor_mut().open_keybinding_editor();
    harness.render().unwrap();
}

/// Helper to select the first binding row (skipping past section headers)
fn select_first_binding(harness: &mut EditorTestHarness) {
    // The first row is a section header; move down to the first actual binding
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
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
    select_first_binding(&mut harness);

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
    select_first_binding(&mut harness);

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
    select_first_binding(&mut harness);

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
    select_first_binding(&mut harness);

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

    // Enter capture mode, then record Ctrl+K
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
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

/// Test that deleting a keymap binding creates a noop override
/// (disabling the binding rather than removing it from the keymap),
/// and that the original action appears as unbound in the table.
#[test]
fn test_delete_keymap_binding_creates_noop_override() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Search for "save" to find the Ctrl+S keymap binding
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "save".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Navigate to the keymap "save" binding (the one with Ctrl+S)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let mut found_keymap = false;
    for _ in 0..20 {
        let screen = harness.screen_to_string();
        for line in screen.lines() {
            if line.contains(">") && line.contains("keymap") && line.contains("Ctrl+S") {
                found_keymap = true;
                break;
            }
        }
        if found_keymap {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(
        found_keymap,
        "Should find the Ctrl+S save keymap binding in search results"
    );

    // Delete (override) the keymap binding
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show status about keymap being overridden
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("disabled") || screen.contains("override") || screen.contains("noop"),
        "Should show a status message about the keymap binding being disabled.\nScreen:\n{}",
        screen,
    );

    // The editor should be marked as modified
    assert!(
        screen.contains("modified"),
        "Editor should show [modified] after overriding a keymap binding"
    );

    // Cancel the "save" search so we can see all results.
    // Pressing Escape cancels the search.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now search for Ctrl+S (via record-key search) to find the noop override
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The noop override should appear with "custom" source
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("noop"),
        "After overriding keymap binding, Ctrl+S should show 'noop' action.\nScreen:\n{}",
        screen,
    );
    assert!(
        screen.contains("custom"),
        "The noop override should have 'custom' source.\nScreen:\n{}",
        screen,
    );

    // Cancel the record-key search and search for "save" again to verify
    // the original "save" action still appears as unbound (no key).
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    // Search for "^save" specifically
    for ch in "save".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // "save" action should still appear (now unbound — no key, no "keymap" source)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("save"),
        "The 'save' action should still appear in search results.\nScreen:\n{}",
        screen,
    );
    // There should be no "keymap" source for "save" (the keymap entry was overridden)
    // and Ctrl+S should not appear next to "save" (it's now bound to noop)
    // Check that a row has "save" as action but no key
    let has_unbound_save = screen
        .lines()
        .any(|line| line.contains("save") && !line.contains("Ctrl+S") && !line.contains("keymap"));
    assert!(
        has_unbound_save,
        "The 'save' action should appear without Ctrl+S key (unbound).\nScreen:\n{}",
        screen,
    );

    // Save and close
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Keybinding Editor");

    // Ctrl+S should now have no effect (noop override is active).
    // Type something so we can verify save doesn't trigger.
    harness.type_text("x").unwrap();
    harness.render().unwrap();

    let buffer_before = harness.get_buffer_content().unwrap();

    // Press Ctrl+S — if the noop override works, this does nothing
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let buffer_after = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_after, buffer_before,
        "Buffer content should be unchanged — Ctrl+S should be a noop now"
    );
}

/// Test that deleting an unbound action shows an error
#[test]
fn test_cannot_delete_unbound_action() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // The first visible row in the default sort is an unbound action
    // (unbound actions have empty context which sorts first)
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show error about not being able to delete
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Cannot") || screen.contains("cannot") || screen.contains("delete"),
        "Should show a message about not being able to delete unbound actions.\nScreen:\n{}",
        screen,
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

    // Enter capture mode, then record key
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
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
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
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
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
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

/// Compute the scrollbar track position for a 120×40 keybinding editor.
///
/// The modal is centred (`min(W × 0.90, 120) × (H × 0.90)`); the table chunk
/// sits below a 3-row header, and `render_table` reserves the first 2 rows of
/// that chunk for the column-header + separator. The scrollbar is 1 column
/// wide at the rightmost column of the (post-header) table area.
fn scrollbar_track_for_120x40() -> (u16, u16, u16) {
    let modal_w = ((120.0_f32 * 0.90).min(120.0)) as u16;
    let modal_h = (40.0_f32 * 0.90) as u16;
    let modal_x = (120 - modal_w) / 2;
    let modal_y = (40 - modal_h) / 2;
    let inner_x = modal_x + 1;
    let inner_y = modal_y + 1;
    let inner_w = modal_w - 2;
    let inner_h = modal_h - 2;
    let table_chunk_y = inner_y + 3;
    let table_chunk_h = inner_h - 3 - 1;
    let sb_top = table_chunk_y + 2; // skip column-header + separator
    let sb_height = table_chunk_h - 2;
    let sb_col = inner_x + inner_w - 1;
    (sb_col, sb_top, sb_top + sb_height - 1)
}

/// Click on the bottom of the scrollbar — the visible rows should scroll
/// (regression test for issue #1593: scrollbar was unresponsive to mouse).
#[test]
fn test_scrollbar_click_scrolls_table() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    let (sb_col, sb_top, sb_bottom) = scrollbar_track_for_120x40();

    let screen_before = harness.screen_to_string();

    // Click near the bottom of the scrollbar track.
    harness.mouse_click(sb_col, sb_bottom).unwrap();

    let screen_after = harness.screen_to_string();
    assert_ne!(
        screen_before, screen_after,
        "Clicking the bottom of the scrollbar must scroll the table"
    );

    // Click back at the top — should scroll back to the start.
    harness.mouse_click(sb_col, sb_top).unwrap();
    let screen_back = harness.screen_to_string();
    assert_ne!(
        screen_after, screen_back,
        "Clicking the top of the scrollbar must scroll the table back"
    );
}

/// Drag the scrollbar thumb — the viewport should follow the cursor.
#[test]
fn test_scrollbar_drag_scrolls_table() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    let (sb_col, sb_top, sb_bottom) = scrollbar_track_for_120x40();

    let screen_before = harness.screen_to_string();

    harness
        .mouse_drag(sb_col, sb_top, sb_col, sb_bottom)
        .unwrap();

    let screen_after = harness.screen_to_string();
    assert_ne!(
        screen_before, screen_after,
        "Dragging the scrollbar thumb to the bottom must scroll the table"
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
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
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

    // Enter capture mode, then record a key
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
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
// Unbound actions
// ========================

/// Test that actions without a keybinding appear in the editor list
#[test]
fn test_unbound_actions_are_listed() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Search for "duplicate_line" which has no default keybinding
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // The unbound action should appear in search results
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("duplicate_line") || screen.contains("Duplicate"),
        "Unbound action 'duplicate_line' should be listed in the keybinding editor"
    );
}

/// Test that an unbound action can be edited (assign a key to it)
#[test]
fn test_edit_unbound_action() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Search for "duplicate_line" (unbound action)
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    // Press Enter to unfocus search, then navigate past section header to first binding
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    select_first_binding(&mut harness);

    // Press Enter to open edit dialog on the unbound action
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Edit dialog should open
    harness.assert_screen_contains("Edit Keybinding");
    harness.assert_screen_contains("Key:");
    harness.assert_screen_contains("Action:");

    // The action field should show "duplicate_line"
    harness.assert_screen_contains("duplicate_line");

    // Record a key (dialog starts in RecordingKey mode)
    harness
        .send_key(
            KeyCode::Char('d'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Tab to context, then to Save button
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to save
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Dialog should be closed and editor should show modified
    harness.assert_screen_not_contains("Edit Keybinding");
    harness.assert_screen_contains("modified");
}

/// Test that deleting a custom binding makes the action appear as unbound
#[test]
fn test_deleted_binding_appears_as_unbound() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // First, add a custom binding for "duplicate_line"
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Keybinding");

    // Enter capture mode, then record key: Ctrl+Shift+D
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(
            KeyCode::Char('d'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Tab to action field and type "duplicate_line"
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    // Accept autocomplete
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab to context, then to Save button
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to save
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show modified
    harness.assert_screen_contains("modified");

    // Now search for "duplicate_line" and filter to custom source
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    // Enter to unfocus search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The custom binding should be visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("custom"),
        "Custom binding for duplicate_line should show 'custom' source"
    );

    // Navigate to the custom binding row (it should be one of the filtered results)
    // Go to the first result which should be the custom one (or the unbound one)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Find the custom one - cycle through results to find the "custom" row
    // Look for the line with "custom" to know which row has the custom binding
    let mut found_custom = false;
    for _ in 0..5 {
        let current_screen = harness.screen_to_string();
        // Check if the selected row (marked with ">") contains "custom"
        for line in current_screen.lines() {
            if line.contains(">") && line.contains("custom") {
                found_custom = true;
                break;
            }
        }
        if found_custom {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // Delete the custom binding
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now clear the search to see all results for duplicate_line
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Search again to find duplicate_line
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // duplicate_line should still appear (now as unbound - no "custom" or "keymap" source)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("duplicate_line") || screen.contains("Duplicate"),
        "After deleting the custom binding, duplicate_line should still appear as unbound"
    );
    // The "custom" source label should be gone (it's now unbound with empty source)
    assert!(
        !screen.contains("custom"),
        "After deleting the custom binding, there should be no 'custom' source for duplicate_line"
    );
}

/// Test the full delete flow end-to-end:
/// 1. Add a custom binding (Ctrl+Shift+D → duplicate_line), save
/// 2. Verify the binding works (key performs the action) and appears in the table
/// 3. Delete the binding, save
/// 4. Verify the table shows the action without a bound key
/// 5. Verify the key no longer performs the action
#[test]
fn test_delete_binding_full_flow() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // === Phase 1: Add a custom binding Ctrl+Shift+D → duplicate_line ===
    open_keybinding_editor(&mut harness);

    // Press 'a' to open Add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Keybinding");

    // Enter capture mode, then record key: Ctrl+Shift+D
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(
            KeyCode::Char('d'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Ctrl+Shift+D");

    // Tab to Action field
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    // Type "duplicate_line" and accept autocomplete
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab to context, then to Save button, press Enter to save the dialog
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("modified");

    // Save and close keybinding editor with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Keybinding Editor");

    // === Phase 2: Verify binding works (before delete) ===
    // Type some content
    harness.type_text("aaa").unwrap();
    harness.render().unwrap();

    // Move cursor to start of buffer
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Press Ctrl+Shift+D to duplicate the line
    harness
        .send_key(
            KeyCode::Char('d'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "aaa\naaa",
        "Before delete: Ctrl+Shift+D should duplicate the line (binding is active)"
    );

    // === Phase 3: Verify binding in table, then delete it ===
    open_keybinding_editor(&mut harness);

    // Search for "duplicate_line"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    // Enter to unfocus search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the custom binding appears in the table
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ctrl+Shift+D"),
        "Before delete: table should show Ctrl+Shift+D for the binding"
    );
    assert!(
        screen.contains("custom"),
        "Before delete: table should show 'custom' source"
    );

    // Navigate to the custom binding row
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let mut found_custom = false;
    for _ in 0..10 {
        let current_screen = harness.screen_to_string();
        for line in current_screen.lines() {
            if line.contains(">") && line.contains("custom") {
                found_custom = true;
                break;
            }
        }
        if found_custom {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(found_custom, "Should find the custom binding row to delete");

    // Delete the custom binding
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Immediately after delete (before saving/closing): the table should
    // already reflect the removal — the action appears as unbound with no key.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("duplicate_line") || screen.contains("Duplicate"),
        "After delete (before save): action should still be listed"
    );
    assert!(
        !screen.contains("Ctrl+Shift+D"),
        "After delete (before save): Ctrl+Shift+D should be gone from the table immediately"
    );
    assert!(
        !screen.contains("custom"),
        "After delete (before save): 'custom' source should be gone immediately"
    );

    // Save and close keybinding editor with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Keybinding Editor");

    // === Phase 4: After delete - verify table shows action without key ===
    open_keybinding_editor(&mut harness);

    // Search for "duplicate_line"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // The action should still appear (as unbound)
    assert!(
        screen.contains("duplicate_line") || screen.contains("Duplicate"),
        "After delete: action should still be listed in the table"
    );
    // The key should NOT appear in the table
    assert!(
        !screen.contains("Ctrl+Shift+D"),
        "After delete: Ctrl+Shift+D should NOT appear in the table (binding was deleted)"
    );
    // The source should NOT show "custom"
    assert!(
        !screen.contains("custom"),
        "After delete: 'custom' source should NOT appear"
    );

    // Close keybinding editor
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // === Phase 5: After delete - verify key has no effect ===
    // Buffer still has "aaa\naaa" from Phase 2. Record it, then press the
    // key and verify the buffer is unchanged (binding no longer active).
    let buffer_before = harness.get_buffer_content().unwrap();

    // Press Ctrl+Shift+D - should have NO effect since the binding was deleted
    harness
        .send_key(
            KeyCode::Char('d'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    let buffer_after = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_after, buffer_before,
        "After delete: Ctrl+Shift+D should have no effect (binding was removed)"
    );
}

/// Test that adding a binding with a key that's already used shows a conflict warning
#[test]
fn test_add_binding_conflict_warning_for_existing_key() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Press 'a' to open Add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Keybinding");

    // Enter capture mode, then record a key already in use: Ctrl+S (bound to "save")
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show the recorded key
    harness.assert_screen_contains("Ctrl+S");

    // Should show a conflict warning since Ctrl+S is already bound
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Conflict"),
        "Should show conflict warning when using a key already bound to another action.\nScreen:\n{}",
        screen
    );
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

/// Regression test: deleting a custom keybinding that uses Shift+letter fails.
/// The bug: when recording Shift+N, crossterm sends KeyCode::Char('N') (uppercase).
/// key_code_to_config_name stores it as "N". After save+reload, parse_key lowercases
/// it to KeyCode::Char('n'), so key_code_to_config_name returns "n". The deletion
/// matching compares "N" (in config) vs "n" (from resolved round-trip) and fails.
#[test]
fn test_delete_shift_letter_binding_full_flow() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // === Phase 1: Add a custom binding Shift+N → search_next ===
    open_keybinding_editor(&mut harness);

    // Press 'a' to open Add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Keybinding");

    // Enter capture mode, then record key: Shift+N (crossterm sends uppercase 'N' with SHIFT)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('N'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Shift+N");

    // Tab to Action field
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    // Type "duplicate_line" and accept autocomplete
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Tab to context, then to Save button, press Enter to save the dialog
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("modified");

    // Save and close keybinding editor with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Keybinding Editor");

    // === Phase 2: Reopen keybinding editor and delete the binding ===
    open_keybinding_editor(&mut harness);

    // Search for "duplicate_line"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the custom binding appears
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Shift+N"),
        "Before delete: table should show Shift+N.\nScreen:\n{}",
        screen
    );
    assert!(
        screen.contains("custom"),
        "Before delete: table should show 'custom' source.\nScreen:\n{}",
        screen
    );

    // Navigate to the custom binding row
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let mut found_custom = false;
    for _ in 0..10 {
        let current_screen = harness.screen_to_string();
        for line in current_screen.lines() {
            if line.contains(">") && line.contains("custom") {
                found_custom = true;
                break;
            }
        }
        if found_custom {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(found_custom, "Should find the custom binding row to delete");

    // Delete the custom binding
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify deletion happened in the UI
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Shift+N"),
        "After delete (before save): Shift+N should be gone from the table.\nScreen:\n{}",
        screen
    );

    // Save and close
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Keybinding Editor");

    // === Phase 3: Reopen and verify the binding is truly gone (persisted) ===
    open_keybinding_editor(&mut harness);

    // Search for "duplicate_line"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    for ch in "duplicate_line".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Shift+N"),
        "After save+reopen: Shift+N should NOT appear (deletion should persist).\nScreen:\n{}",
        screen
    );
    assert!(
        !screen.contains("custom"),
        "After save+reopen: 'custom' source should NOT appear.\nScreen:\n{}",
        screen
    );
}

// ========================
// Special key capture mode
// ========================

/// Test that pressing Enter on the key field enters capture mode and shows
/// the capture hint text.
#[test]
fn test_capture_mode_shows_hint() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Keybinding");

    // Key field is focused, should show the capture hint
    harness.assert_screen_contains("Enter: capture key");

    // Press Enter to enter capture mode
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show capture-mode instruction
    harness.assert_screen_contains("press any key");
}

/// Test that Escape can be captured as a keybinding via capture mode.
#[test]
fn test_capture_escape_key() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Keybinding");

    // Enter capture mode
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Escape — should be captured, not close the dialog
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Dialog should still be open with Esc recorded
    harness.assert_screen_contains("Add Keybinding");
    harness.assert_screen_contains("Esc");
}

/// Test that Tab can be captured as a keybinding via capture mode.
#[test]
fn test_capture_tab_key() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Enter capture mode, then press Tab
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Dialog should still be open with Tab recorded (not moved focus)
    harness.assert_screen_contains("Add Keybinding");
    harness.assert_screen_contains("Tab");
    // The hint should be back to normal (capture mode exited)
    harness.assert_screen_contains("Enter: capture key");
}

/// Test that Enter can be captured as a keybinding via capture mode.
#[test]
fn test_capture_enter_key() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Enter capture mode (first Enter), then capture Enter (second Enter)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Dialog should still be open with Enter recorded
    harness.assert_screen_contains("Add Keybinding");
    // Check the key field shows "Enter"
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Key:") && screen.contains("Enter"),
        "Key field should show 'Enter' as the captured key.\nScreen:\n{}",
        screen
    );
}

/// Test that Escape still closes the dialog when NOT in capture mode.
#[test]
fn test_escape_still_closes_dialog_without_capture_mode() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Keybinding");

    // Press Escape directly (without entering capture mode) — should close dialog
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.assert_screen_not_contains("Add Keybinding");
    harness.assert_screen_contains("Keybinding Editor");
}

/// Test that pressing a key (e.g. arrow) in the key field does NOT record it
/// unless the user has entered capture mode with Enter first.
#[test]
fn test_key_not_recorded_without_capture_mode() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_keybinding_editor(&mut harness);

    // Open add dialog
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Keybinding");

    // Press Down arrow directly WITHOUT entering capture mode
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The key field should still be empty — the arrow should NOT have been recorded
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("\u{2193}"),
        "Down arrow should NOT be recorded without entering capture mode first.\nScreen:\n{}",
        screen
    );
}
