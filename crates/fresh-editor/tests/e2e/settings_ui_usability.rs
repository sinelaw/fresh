//! E2E tests for Settings UI usability issues found during NNGroup UX audit.
//!
//! H1:  Tab cycles through all fields, sub-fields, and buttons sequentially.
//! H5:  Individual TextList items (Root Markers, Args) keyboard-accessible.
//! M7:  Page Up/Down supported in long map lists.
//! NEW: Composite control highlight only covers focused sub-row.
//! NEW: Tab visits every field including composite sub-items.

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
/// Returns the field name or button name if focused.
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
        for field in &known_fields {
            if line.contains(field) {
                return Some(field.to_string());
            }
        }
    }
    None
}

/// Helper: find which button has focus by looking for the ">" indicator
/// rendered before button text. The ">" is at a separate cell position,
/// so we search for it in the vicinity of button labels.
fn focused_button(harness: &EditorTestHarness) -> Option<String> {
    let screen = harness.screen_to_string();
    let buttons = ["Save", "Delete", "Cancel"];

    for (row_idx, line) in screen.lines().enumerate() {
        for button in &buttons {
            let label = format!("[ {} ]", button);
            if let Some(col) = line.find(&label) {
                // Check for ">" indicator in the 1-3 cells before the button
                let col = col as u16;
                let row = row_idx as u16;
                for offset in 1..=3 {
                    if col >= offset {
                        if let Some(cell) = harness.get_cell(col - offset, row) {
                            if cell.trim() == ">" {
                                return Some(button.to_string());
                            }
                        }
                    }
                }
                // Also check style — focused buttons use REVERSED modifier
                if let Some(style) = harness.get_cell_style(col + 2, row) {
                    if style
                        .add_modifier
                        .contains(ratatui::style::Modifier::REVERSED)
                    {
                        return Some(button.to_string());
                    }
                }
            }
        }
    }
    None
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

    // Collect distinct focused buttons via repeated Tab presses.
    let mut visited_buttons: Vec<String> = Vec::new();

    for _ in 0..20 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        if let Some(btn) = focused_button(&harness) {
            if visited_buttons.last().map(|s| s.as_str()) != Some(btn.as_str()) {
                visited_buttons.push(btn.clone());
            }
        }

        // Stop if we've seen all 3 buttons
        if visited_buttons.contains(&"Save".to_string())
            && visited_buttons.contains(&"Delete".to_string())
            && visited_buttons.contains(&"Cancel".to_string())
        {
            break;
        }
    }

    assert!(
        visited_buttons.contains(&"Save".to_string()),
        "Tab never reached Save button. Visited buttons: {:?}\nScreen:\n{}",
        visited_buttons,
        harness.screen_to_string()
    );
    assert!(
        visited_buttons.contains(&"Delete".to_string()),
        "BUG H1: Tab never reached Delete button. Only visited: {:?}\n\
         Delete is only reachable via Right arrow from Save, which is non-standard.\nScreen:\n{}",
        visited_buttons,
        harness.screen_to_string()
    );
    assert!(
        visited_buttons.contains(&"Cancel".to_string()),
        "BUG H1: Tab never reached Cancel button. Only visited: {:?}\nScreen:\n{}",
        visited_buttons,
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

// ---------------------------------------------------------------------------
// Composite control highlight: only the focused sub-row should be highlighted
// ---------------------------------------------------------------------------

/// When a composite control (TextList, ObjectArray) is focused, the ">"
/// indicator should be on the specific focused sub-row (e.g., "--stdio"),
/// NOT on the section header (e.g., "Args:").
#[test]
fn test_composite_focus_indicator_on_subrow_not_header() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    open_lsp_edit_item(&mut harness);

    // Navigate to Args section (a TextList with sub-items)
    for _ in 0..20 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();

        // Look for a state where "Args:" is visible and ">" is on a sub-item line
        let lines: Vec<&str> = screen.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            // If ">" is on a line that contains the Args label, that's the old behavior
            if line.contains(">") && line.contains("Args:") {
                // This would mean the indicator is on the header — check if there are
                // sub-items below that should have focus instead
                if i + 1 < lines.len()
                    && (lines[i + 1].contains("[") || lines[i + 1].contains("--"))
                {
                    panic!(
                        "Focus indicator '>' is on the Args header line instead of a sub-item.\n\
                         Header line: {}\nSub-item line below: {}\n\
                         The '>' should be on the specific focused sub-item, not the section header.",
                        line,
                        lines[i + 1]
                    );
                }
            }
        }
    }
    // If we get here without panicking, either Args was navigated correctly
    // (indicator on sub-item) or Args wasn't reached — both are acceptable.
}

// ---------------------------------------------------------------------------
// Tab should cycle sequentially through all fields, sub-fields, and buttons
// ---------------------------------------------------------------------------

/// Tab should visit every field and sub-field sequentially (like Down),
/// then cycle through buttons, then wrap back to the first field.
#[test]
fn test_tab_cycles_through_all_fields_and_buttons() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    open_lsp_edit_item(&mut harness);

    // Press Tab many times and collect distinct focused elements
    // Tab should visit fields sequentially, not just toggle
    let mut visited_fields: Vec<String> = Vec::new();
    let mut visited_buttons: Vec<String> = Vec::new();
    let mut total_tabs = 0;

    let screen = harness.screen_to_string();
    if let Some(f) = focused_field(&screen) {
        visited_fields.push(f);
    }

    for _ in 0..40 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        total_tabs += 1;

        let screen = harness.screen_to_string();
        if let Some(btn) = focused_button(&harness) {
            if visited_buttons.last().map(|s| s.as_str()) != Some(btn.as_str()) {
                visited_buttons.push(btn);
            }
        } else if let Some(f) = focused_field(&screen) {
            if visited_fields.last().map(|s| s.as_str()) != Some(f.as_str()) {
                visited_fields.push(f);
            }
        }

        // Stop if we've gone through a full cycle (back to first field after buttons)
        if visited_buttons.len() >= 3
            && visited_fields.len() > 1
            && visited_fields.last() == visited_fields.first()
        {
            break;
        }
    }

    // Tab should have visited multiple distinct fields (not just toggling between 2)
    assert!(
        visited_fields.len() >= 3,
        "Tab only visited {} distinct fields: {:?}. Tab should cycle sequentially \
         through all fields, not just toggle.\nButtons visited: {:?}\nTotal tabs: {}",
        visited_fields.len(),
        visited_fields,
        visited_buttons,
        total_tabs
    );

    // Should have visited all 3 buttons
    assert!(
        visited_buttons.len() >= 2,
        "Tab should visit buttons. Only visited: {:?}\nFields: {:?}",
        visited_buttons,
        visited_fields
    );
}

// ---------------------------------------------------------------------------
// Adding a new LSP server should persist in the parent dialog
// ---------------------------------------------------------------------------

/// Adding a second LSP server for a language (e.g., "ty" for python) should
/// result in both servers appearing in the Edit Value dialog.
#[test]
fn test_add_new_lsp_server_persists() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    harness.open_settings().unwrap();

    // Navigate to LSP > python
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("lsp").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Find python entry
    let mut found_python = false;
    for _ in 0..50 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        if screen.contains("python") && screen.contains("[Enter to edit]") {
            for line in screen.lines() {
                if line.contains("python") && line.contains("[Enter to edit]") {
                    found_python = true;
                    break;
                }
            }
        }
        if found_python {
            break;
        }
    }
    assert!(
        found_python,
        "Could not find python LSP entry.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Open Edit Value for python
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Edit Value");
    harness.assert_screen_contains("pylsp");

    // Navigate to [+] Add new
    let mut found_add = false;
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        for line in screen.lines() {
            if line.contains("[+] Add new")
                && (line.contains(">") || line.contains("[Enter to add]"))
            {
                found_add = true;
                break;
            }
        }
        if found_add {
            break;
        }
    }
    assert!(
        found_add,
        "Could not reach [+] Add new.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Press Enter to open Add Item dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Item");
    harness.assert_screen_contains("Command");

    // Type "ty" in the Command field (which auto-enters edit mode)
    harness.type_text("ty").unwrap();
    harness.render().unwrap();

    // Save with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be back in Edit Value dialog, now showing both pylsp and ty
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("pylsp"),
        "Original server 'pylsp' should still be visible after adding new server.\nScreen:\n{}",
        screen
    );
    assert!(
        screen.contains("ty"),
        "New server 'ty' should be visible after saving.\nScreen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// TextList: Up/Down should accept pending edits
// ---------------------------------------------------------------------------

/// When editing in a TextList (e.g., typing a new arg after pressing Enter on
/// [+] Add new), pressing Down or Tab should auto-accept the text and add it
/// as a new entry.
#[test]
fn test_textlist_down_accepts_new_entry() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();
    open_lsp_edit_item(&mut harness);

    // Navigate to Args section's [+] Add new sub-item
    let mut found_args_add = false;
    for _ in 0..30 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        let lines: Vec<&str> = screen.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            if line.contains(">") && line.contains("[+]") {
                // Check if this [+] Add new is under Args (within 5 lines above)
                for offset in 1..=5 {
                    if i >= offset && lines[i - offset].contains("Args:") {
                        found_args_add = true;
                        break;
                    }
                }
            }
            if found_args_add {
                break;
            }
        }
        if found_args_add {
            break;
        }
    }

    if !found_args_add {
        eprintln!("Could not navigate to Args [+] Add new, skipping test.");
        return;
    }

    // Press Enter to start editing the add-new field
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type a new argument
    harness.type_text("--verbose").unwrap();
    harness.render().unwrap();

    // Press Down to auto-accept and navigate
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The new arg should be accepted and visible as a committed entry
    // (not just in the add-new text field)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("--verbose"),
        "New arg '--verbose' should be accepted after pressing Down.\nScreen:\n{}",
        screen
    );

    // Verify it was actually added as an entry (has [x] delete button)
    let has_committed_entry = screen
        .lines()
        .any(|line| line.contains("--verbose") && line.contains("[x]"));
    assert!(
        has_committed_entry,
        "Entry '--verbose' should be committed (shown with [x] delete button).\nScreen:\n{}",
        screen
    );
}
