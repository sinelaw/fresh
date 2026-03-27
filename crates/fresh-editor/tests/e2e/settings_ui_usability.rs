//! E2E tests for Settings UI usability issues found during NNGroup UX audit.
//!
//! C2:  Text fields always in edit mode — Escape has no effect, fields auto-enter
//!      edit mode on navigation, trapping the keyboard.
//! H1:  Tab only toggles between fields and Save button — Delete/Cancel never
//!      reachable via Tab.
//! H5:  Individual TextList items (Root Markers, Args) not keyboard-accessible —
//!      cannot focus or delete individual items.
//! H7:  Status bar is static — does not update to reflect text editing mode vs
//!      navigation mode.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Helper: open settings, search for "lsp", navigate to a language entry,
/// and open the Edit Item dialog for the first server.
fn open_lsp_edit_item(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();

    // Search for "lsp"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("lsp").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Lsp");

    // Find and open a language entry that has a server configured
    let mut opened = false;
    for _ in 0..50 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        if screen.contains("[Enter to edit]") {
            // Found a focused entry — open Edit Value
            harness
                .send_key(KeyCode::Enter, KeyModifiers::NONE)
                .unwrap();
            harness.render().unwrap();

            if harness.screen_to_string().contains("Edit Value") {
                // Now open the Edit Item for the first server
                harness
                    .send_key(KeyCode::Enter, KeyModifiers::NONE)
                    .unwrap();
                harness.render().unwrap();

                if harness.screen_to_string().contains("Edit Item") {
                    opened = true;
                    break;
                }
                // Try navigating down and entering
                harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
                harness.render().unwrap();
                harness
                    .send_key(KeyCode::Enter, KeyModifiers::NONE)
                    .unwrap();
                harness.render().unwrap();
                if harness.screen_to_string().contains("Edit Item") {
                    opened = true;
                    break;
                }
            }
        }
    }

    assert!(
        opened,
        "Could not open Edit Item dialog. Screen:\n{}",
        harness.screen_to_string()
    );
    harness.assert_screen_contains("Command");
}

/// Helper: find which field has the focus indicator on screen.
/// Returns the field name or "__BUTTONS__" if a button is focused.
fn focused_field(screen: &str) -> Option<String> {
    let known_fields = [
        "Command",
        "Enabled",
        "Name",
        "Args",
        "Auto Start",
        "Root Markers",
        "Env",
        "Language Id Overrides",
        "Initialization Options",
        "Only Features",
        "Except Features",
        "Process Limits",
    ];
    for line in screen.lines() {
        // Focus indicator is ">" at start or ">●" pattern
        let trimmed = line.trim();
        if !trimmed.starts_with('>') && !trimmed.contains(">●") && !trimmed.contains("> ●") {
            continue;
        }
        if line.contains("[ Save ]") || line.contains("[ Delete ]") || line.contains("[ Cancel ]")
        {
            // Identify which button is focused
            if line.contains("> [ Save ]") {
                return Some("Save".to_string());
            }
            if line.contains("> [ Delete ]") {
                return Some("Delete".to_string());
            }
            if line.contains("> [ Cancel ]") {
                return Some("Cancel".to_string());
            }
            return Some("__BUTTONS__".to_string());
        }
        for field in &known_fields {
            if line.contains(field) {
                return Some(field.to_string());
            }
        }
    }
    None
}

// ---------------------------------------------------------------------------
// C2: Text fields should not auto-enter edit mode; Escape should exit editing
// ---------------------------------------------------------------------------

/// Navigating to a text field with Down arrow should NOT auto-enter edit mode.
/// The user should have to press Enter to start editing.
#[test]
fn test_text_field_does_not_auto_enter_edit_mode() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    open_lsp_edit_item(&mut harness);

    // Focus should be on Command field initially
    let screen = harness.screen_to_string();
    assert!(
        focused_field(&screen).as_deref() == Some("Command"),
        "Expected initial focus on Command. Screen:\n{}",
        screen
    );

    // Without pressing Enter, type a character
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The character 'z' should NOT be inserted into the Command field.
    // If the field auto-enters edit mode, 'z' gets appended to the command value.
    let screen = harness.screen_to_string();
    for line in screen.lines() {
        if line.contains("Command") && line.contains(">") {
            assert!(
                !line.contains("z"),
                "BUG C2/H6: Text field auto-entered edit mode! Typing 'z' without \
                 pressing Enter inserted it into the Command field.\nLine: {}\nScreen:\n{}",
                line,
                screen
            );
        }
    }
}

/// After pressing Enter to start editing a text field, Escape should exit
/// edit mode and return to field navigation.
#[test]
fn test_escape_exits_text_edit_mode() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    open_lsp_edit_item(&mut harness);

    // Enter edit mode on Command field
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type some text (should be in edit mode now)
    harness.type_text("test").unwrap();
    harness.render().unwrap();

    // Press Escape to exit edit mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // We should still be in the Edit Item dialog (not closed)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Edit Item") || screen.contains("[ Save ]"),
        "BUG C2: Escape closed the entire dialog instead of just exiting \
         text edit mode.\nScreen:\n{}",
        screen
    );

    // Down arrow should now navigate to the next field (not move cursor in text)
    let before = focused_field(&harness.screen_to_string());
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let after = focused_field(&harness.screen_to_string());

    assert_ne!(
        before, after,
        "BUG C2: After Escape, Down arrow did not navigate to the next field. \
         Focus stayed on {:?}. Escape likely did not exit text edit mode.\nScreen:\n{}",
        before,
        harness.screen_to_string()
    );
}

// ---------------------------------------------------------------------------
// H1: Tab should cycle through all buttons (Save, Delete, Cancel)
// ---------------------------------------------------------------------------

/// Tab from fields should reach all buttons, not just Save.
#[test]
fn test_tab_cycles_through_all_buttons() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    open_lsp_edit_item(&mut harness);

    // Collect distinct focused elements via repeated Tab presses.
    // We need to find Save, Delete, and Cancel all reachable via Tab.
    let mut visited: Vec<String> = Vec::new();

    for _ in 0..20 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        if let Some(f) = focused_field(&harness.screen_to_string()) {
            if visited.last().map(|s| s.as_str()) != Some(f.as_str()) {
                visited.push(f.clone());
            }
        }

        // Stop if we've seen all 3 buttons
        if visited.contains(&"Save".to_string())
            && visited.contains(&"Delete".to_string())
            && visited.contains(&"Cancel".to_string())
        {
            break;
        }
    }

    assert!(
        visited.contains(&"Save".to_string()),
        "Tab never reached Save button. Visited: {:?}\nScreen:\n{}",
        visited,
        harness.screen_to_string()
    );
    assert!(
        visited.contains(&"Delete".to_string()),
        "BUG H1: Tab never reached Delete button. Only visited: {:?}\n\
         Delete is only reachable via Right arrow from Save, which is non-standard.\nScreen:\n{}",
        visited,
        harness.screen_to_string()
    );
    assert!(
        visited.contains(&"Cancel".to_string()),
        "BUG H1: Tab never reached Cancel button. Only visited: {:?}\nScreen:\n{}",
        visited,
        harness.screen_to_string()
    );
}

// ---------------------------------------------------------------------------
// H5: TextList items should be individually keyboard-accessible
// ---------------------------------------------------------------------------

/// Individual items in a TextList (Root Markers, Args) should be focusable
/// and deletable via keyboard.
#[test]
fn test_textlist_items_keyboard_accessible() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    open_lsp_edit_item(&mut harness);

    // Navigate to Root Markers (which has multiple items for most LSP configs)
    let mut found_root_markers = false;
    for _ in 0..20 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        if let Some(f) = focused_field(&harness.screen_to_string()) {
            if f == "Root Markers" {
                found_root_markers = true;
                break;
            }
        }
    }

    if !found_root_markers {
        // Root Markers might not exist for all servers; skip gracefully
        eprintln!(
            "Root Markers field not found in Edit Item dialog, skipping test.\nScreen:\n{}",
            harness.screen_to_string()
        );
        return;
    }

    // Press Enter or Down to enter the Root Markers composite control
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // Check if individual marker items are present (e.g., pyproject.toml, .git)
    let has_items = screen.contains(".git")
        || screen.contains("pyproject.toml")
        || screen.contains("Cargo.toml")
        || screen.contains("package.json");

    if !has_items {
        eprintln!(
            "No individual Root Marker items found on screen, skipping.\nScreen:\n{}",
            screen
        );
        return;
    }

    // Navigate Down within Root Markers — individual items should get focus
    let mut focused_an_item = false;
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        // Check if any individual marker item has a focus indicator
        for line in screen.lines() {
            if (line.contains(">") || line.contains("[x]"))
                && (line.contains(".git")
                    || line.contains("pyproject.toml")
                    || line.contains("Cargo.toml")
                    || line.contains("package.json"))
            {
                focused_an_item = true;
                break;
            }
        }
        if focused_an_item {
            break;
        }
    }

    assert!(
        focused_an_item,
        "BUG H5: Could not focus any individual Root Marker item via keyboard. \
         Items are visible but not focusable.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

// ---------------------------------------------------------------------------
// H7: Status bar should update to reflect current interaction mode
// ---------------------------------------------------------------------------

/// The status bar in the Edit Item dialog should change when entering
/// text edit mode vs normal navigation.
#[test]
fn test_status_bar_updates_for_edit_mode() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    open_lsp_edit_item(&mut harness);

    // Capture the status bar in navigation mode
    harness.render().unwrap();
    let screen_nav = harness.screen_to_string();

    // The last 2-3 lines typically contain the status bar / help text
    let nav_footer: String = screen_nav
        .lines()
        .rev()
        .take(3)
        .collect::<Vec<_>>()
        .join("\n");

    // Enter edit mode on a text field
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_edit = harness.screen_to_string();
    let edit_footer: String = screen_edit
        .lines()
        .rev()
        .take(3)
        .collect::<Vec<_>>()
        .join("\n");

    // The status bar text should be different in edit mode
    // Navigation mode typically shows: "Enter:Edit" or "↑↓:Navigate"
    // Edit mode should show something like: "Esc:Stop editing" or "Type to edit"
    assert_ne!(
        nav_footer.trim(),
        edit_footer.trim(),
        "BUG H7: Status bar did not change when entering text edit mode.\n\
         Navigation footer:\n{}\n\
         Edit mode footer:\n{}\n\
         The status bar should update to show editing-specific hints \
         (e.g., 'Esc:Stop editing') when a text field is being edited.",
        nav_footer,
        edit_footer
    );
}

// ---------------------------------------------------------------------------
// M7: Page Up/Down should work in long map lists
// ---------------------------------------------------------------------------

/// Page Down should jump through the LSP or Languages map list
/// rather than requiring one-at-a-time Up/Down navigation.
#[test]
fn test_page_down_works_in_long_map_lists() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    harness.open_settings().unwrap();

    // Navigate to LSP section
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("lsp").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Lsp");

    // Move down a couple times to be inside the list
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Record current screen
    let screen_before = harness.screen_to_string();

    // Press Page Down
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();

    // The screen should have changed significantly — the viewport should
    // have jumped forward, not just moved one item
    assert_ne!(
        screen_before, screen_after,
        "BUG M7: Page Down had no effect in the LSP map list. \
         Long lists (40+ entries) require Page Up/Down for efficient navigation."
    );
}
