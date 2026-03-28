//! E2E tests reproducing settings UI bugs found during the TUI UX audit (Track Two).
//!
//! Bug A: ObjectArray `[+] Add new` is unreachable via keyboard in entry dialogs.
//!        Specifically, in the LSP Edit Value dialog, Down arrow skips from the
//!        ObjectArray entries directly to buttons, making it impossible to add a
//!        second LSP server for a language.
//!
//! Bug B: Down navigation in the Edit Item dialog is inconsistent — some items are
//!        visited twice (text fields auto-enter edit mode on first Down) and the
//!        navigation cycle doesn't cover all fields reliably.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Helper: open settings, search for "lsp", jump to the LSP section,
/// then navigate Down to the "python" entry and press Enter to open Edit Value.
fn open_python_lsp_edit_value(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();

    // Search for "lsp" to jump directly to the LSP map
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("lsp").unwrap();
    harness.render().unwrap();

    // Jump to LSP section
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Lsp");

    // Navigate down through LSP entries until we find "python"
    for _ in 0..50 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        if screen.contains("python") && screen.contains("[Enter to edit]") {
            // Check that the [Enter to edit] hint is on the python line
            for line in screen.lines() {
                if line.contains("python") && line.contains("[Enter to edit]") {
                    // Found and focused — press Enter to open Edit Value
                    harness
                        .send_key(KeyCode::Enter, KeyModifiers::NONE)
                        .unwrap();
                    harness.render().unwrap();
                    harness.assert_screen_contains("Edit Value");
                    harness.assert_screen_contains("Key:python");
                    return;
                }
            }
        }
    }

    panic!(
        "Could not navigate to python LSP entry. Screen:\n{}",
        harness.screen_to_string()
    );
}

// ---------------------------------------------------------------------------
// Bug A: ObjectArray [+] Add new unreachable via keyboard
// ---------------------------------------------------------------------------

/// Reproduce Bug A: In the LSP Edit Value dialog for python, the `[+] Add new`
/// row inside the ObjectArray should be reachable via Down arrow navigation.
///
/// The ObjectArray shows existing entries (e.g., `pylsp`) and a `[+] Add new` row.
/// Down arrow should cycle: existing entries → [+] Add new → buttons.
/// Currently, Down skips [+] Add new and goes directly to buttons.
#[test]
fn test_lsp_edit_value_add_new_reachable_via_keyboard() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();

    open_python_lsp_edit_value(&mut harness);

    // We're now in the Edit Value dialog for python.
    // It should show: Key:python, Value: (ObjectArray with pylsp), [+] Add new, buttons
    harness.assert_screen_contains("pylsp");
    harness.assert_screen_contains("[+] Add new");

    // Navigate Down through the ObjectArray entries.
    // We should be able to reach [+] Add new before hitting the buttons.
    let mut found_add_new_focused = false;

    // Press Down up to 10 times. At some point, [+] Add new should get a focus
    // indicator or [Enter to add] hint, before we reach the buttons.
    for i in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();

        // Check if [+] Add new is focused (has ">" indicator on its line or [Enter to add] hint)
        for line in screen.lines() {
            if line.contains("[+] Add new")
                && (line.contains(">") || line.contains("[Enter to add]"))
            {
                found_add_new_focused = true;
                break;
            }
        }

        if found_add_new_focused {
            eprintln!("[+] Add new became focused after {} Down presses", i + 1);
            break;
        }

        // If we've already reached the buttons, the bug is reproduced
        for line in screen.lines() {
            if line.contains("> [ Save ]") || line.contains("> [ Cancel ]") {
                panic!(
                    "BUG A REPRODUCED: Down arrow reached buttons without ever focusing \
                     '[+] Add new'. After {} Down presses, focus jumped to buttons.\n\
                     This means adding a new LSP server via keyboard is impossible.\n\
                     Screen:\n{}",
                    i + 1,
                    screen
                );
            }
        }
    }

    assert!(
        found_add_new_focused,
        "Expected '[+] Add new' to become focused via Down arrow navigation, \
         but it was never reached in 10 Down presses.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Verify that pressing Enter on [+] Add new opens the Add Item dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Add Item");

    // Clean up
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Verify that pressing Enter on [+] Add new in the LSP ObjectArray opens the
/// Add Item dialog, which is the entry point for adding a second LSP server.
#[test]
fn test_add_second_lsp_server_for_python_via_keyboard() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();

    open_python_lsp_edit_value(&mut harness);

    // Verify pylsp is the existing server
    harness.assert_screen_contains("pylsp");
    harness.assert_screen_contains("[+] Add new");

    // Navigate to [+] Add new via Down arrows
    let mut reached_add_new = false;
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        for line in screen.lines() {
            if line.contains("[+] Add new")
                && (line.contains(">") || line.contains("[Enter to add]"))
            {
                reached_add_new = true;
                break;
            }
        }
        if reached_add_new {
            break;
        }
    }

    assert!(
        reached_add_new,
        "Could not reach '[+] Add new' via keyboard. Screen:\n{}",
        harness.screen_to_string()
    );

    // Press Enter on [+] Add new — should open the Add Item dialog
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The Add Item dialog should be open with fields for the new LSP server config
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Add Item"),
        "Expected Add Item dialog to open after pressing Enter on [+] Add new.\nScreen:\n{}",
        screen
    );
    assert!(
        screen.contains("Command"),
        "Add Item dialog should contain a Command field.\nScreen:\n{}",
        screen
    );

    // Clean up
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Clean up
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

// ---------------------------------------------------------------------------
// Bug B: Down navigation inconsistencies in Edit Item dialog
// ---------------------------------------------------------------------------

/// Verify that Down navigation visits every top-level field in the Edit Item dialog.
///
/// Composite controls (TextLists like Root Markers, Maps like Env) have internal
/// sub-navigation, so Down may take multiple presses to traverse them. This test
/// verifies that all fields are visited (no skips) and the order is correct.
#[test]
fn test_entry_dialog_down_visits_every_field_once() {
    let mut harness = EditorTestHarness::new(120, 50).unwrap();
    harness.render().unwrap();

    open_python_lsp_edit_value(&mut harness);

    // Open the Edit Item dialog for the pylsp server.
    // Enter on the Value/ObjectArray label should open the nested Edit Item.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    if !screen.contains("Edit Item") {
        harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        open_python_lsp_edit_value(&mut harness);
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    harness.assert_screen_contains("Command");
    harness.assert_screen_contains("Enabled");

    // Known top-level fields in expected order (post-rebase).
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

    // Helper: identify which known field (if any) has the focus indicator ">"
    // For composite controls (TextList, Map, ObjectArray), the ">" indicator may be
    // on a sub-item line below the section header, so we also check nearby lines above.
    let identify_focused = |screen: &str| -> Option<String> {
        let lines: Vec<&str> = screen.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            if !line.contains(">") {
                continue;
            }
            if line.contains("[ Save ]") || line.contains("[ Cancel ]") {
                return Some("__BUTTONS__".to_string());
            }
            // Check the focused line itself
            for field in &known_fields {
                if line.contains(field) {
                    return Some(field.to_string());
                }
            }
            // For composite controls, check up to 10 lines above for the section header
            for offset in 1..=10 {
                if i >= offset {
                    let above = lines[i - offset];
                    for field in &known_fields {
                        if above.contains(field) && above.contains(":") {
                            return Some(field.to_string());
                        }
                    }
                }
            }
        }
        None
    };

    // Collect the distinct field visit order (deduplicating consecutive same-field visits
    // which happen when navigating through composite sub-items).
    let mut distinct_fields: Vec<String> = Vec::new();

    // Record initial focus
    harness.render().unwrap();
    if let Some(f) = identify_focused(&harness.screen_to_string()) {
        if f != "__BUTTONS__" {
            distinct_fields.push(f);
        }
    }

    // Press Down repeatedly until we hit buttons or exhaust attempts
    for _ in 0..60 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        if let Some(f) = identify_focused(&harness.screen_to_string()) {
            if f == "__BUTTONS__" {
                break;
            }
            if distinct_fields.last().map(|s| s.as_str()) != Some(f.as_str()) {
                distinct_fields.push(f);
            }
        }
    }

    // Assert: every known field was visited in order
    let mut missing: Vec<&str> = Vec::new();
    for field in &known_fields {
        if !distinct_fields.iter().any(|f| f.as_str() == *field) {
            missing.push(field);
        }
    }
    assert!(
        missing.is_empty(),
        "Fields never visited during Down navigation: {:?}\n\
         Distinct fields visited: {:?}",
        missing,
        distinct_fields
    );

    // Assert: the visit order matches the expected field order
    let visited_names: Vec<&str> = distinct_fields.iter().map(|s| s.as_str()).collect();
    let expected_names: Vec<&str> = known_fields.iter().copied().collect();
    assert_eq!(
        visited_names, expected_names,
        "Field visit order doesn't match expected order"
    );

    // Clean up
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}
