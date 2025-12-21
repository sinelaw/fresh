//! Tests for buffer settings commands:
//! - Set Tab Size
//! - Toggle Indentation: Spaces ↔ Tabs
//! - Toggle Tab Indicators
//! - Reset Buffer Settings

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Helper to run a command from the command palette
fn run_command(harness: &mut EditorTestHarness, command_name: &str) {
    // Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type the command name
    harness.type_text(command_name).unwrap();
    harness.render().unwrap();

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test that "Toggle Indentation" command toggles between spaces and tabs
#[test]
fn test_toggle_indentation_command() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create an empty Go file (defaults to use_tabs=true)
    std::fs::write(&file_path, "").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify initial state: Tab should insert a tab character in Go files
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "\t", "Initially, Go should use tabs");

    // Undo to clear the tab
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Run "Toggle Indentation" command to switch to spaces
    run_command(&mut harness, "Toggle Indentation");

    // Now Tab should insert spaces
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    let content = harness.get_buffer_content().unwrap();
    assert!(
        !content.contains('\t'),
        "After toggle, should insert spaces, not tabs. Got: {:?}",
        content
    );
    assert!(
        content.contains("    "),
        "After toggle, should have 4 spaces. Got: {:?}",
        content
    );

    // Undo and toggle again to switch back to tabs
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    run_command(&mut harness, "Toggle Indentation");

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "\t",
        "After second toggle, should be back to tabs. Got: {:?}",
        content
    );
}

/// Test that "Toggle Tab Indicators" toggles visibility
#[test]
fn test_toggle_tab_indicators_command() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create a Go file with a tab character
    std::fs::write(&file_path, "\thello").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Go files hide tab indicators by default
    let screen_before = harness.screen_to_string();
    assert!(
        !screen_before.contains('→'),
        "Go files should hide tab indicators by default"
    );

    // Run "Toggle Tab Indicators" command
    run_command(&mut harness, "Toggle Tab Indicators");

    // Now tab indicators should be visible
    let screen_after = harness.screen_to_string();
    assert!(
        screen_after.contains('→'),
        "After toggle, tab indicators should be visible. Screen:\n{}",
        screen_after
    );

    // Toggle again - should hide them
    run_command(&mut harness, "Toggle Tab Indicators");

    let screen_final = harness.screen_to_string();
    assert!(
        !screen_final.contains('→'),
        "After second toggle, tab indicators should be hidden again"
    );
}

/// Test that "Set Tab Size" command changes tab rendering width
#[test]
fn test_set_tab_size_command() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with a tab followed by a marker
    std::fs::write(&file_path, "\tX").unwrap();

    // Start with tab_size = 4 (default)
    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Helper to get X visual column relative to tab indicator
    fn get_x_visual_offset(screen: &str) -> Option<usize> {
        for line in screen.lines() {
            if line.contains('X') {
                let mut indicator_col = None;
                let mut x_col = None;
                for (col, ch) in line.chars().enumerate() {
                    if ch == '→' {
                        indicator_col = Some(col);
                    }
                    if ch == 'X' {
                        x_col = Some(col);
                    }
                }
                return Some(x_col? - indicator_col?);
            }
        }
        None
    }

    // Get initial offset (tab_size=4)
    let screen_4 = harness.screen_to_string();
    let offset_4 = get_x_visual_offset(&screen_4).unwrap();
    assert_eq!(
        offset_4, 4,
        "With default tab_size=4, X should be 4 columns after indicator"
    );

    // Run "Set Tab Size" command and enter "8"
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Set Tab Size").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Clear existing text and type "8"
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap(); // Select all
    harness.type_text("8").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Get new offset (tab_size=8)
    let screen_8 = harness.screen_to_string();
    let offset_8 = get_x_visual_offset(&screen_8).unwrap();
    assert_eq!(
        offset_8, 8,
        "After setting tab_size=8, X should be 8 columns after indicator. Screen:\n{}",
        screen_8
    );
}

/// Test that "Reset Buffer Settings" restores config defaults
#[test]
fn test_reset_buffer_settings_command() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create a Go file with a tab character
    std::fs::write(&file_path, "\thello").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Initial state: Go hides tab indicators and uses tabs
    let screen_initial = harness.screen_to_string();
    assert!(
        !screen_initial.contains('→'),
        "Go files should hide tab indicators initially"
    );

    // Modify settings: toggle tab indicators and toggle indentation to spaces
    run_command(&mut harness, "Toggle Tab Indicators");
    run_command(&mut harness, "Toggle Indentation"); // Go uses tabs by default, so toggle switches to spaces

    // Verify modifications took effect
    let screen_modified = harness.screen_to_string();
    assert!(
        screen_modified.contains('→'),
        "After toggle, tab indicators should be visible"
    );

    // Type a tab to verify spaces are inserted
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    let content_modified = harness.get_buffer_content().unwrap();
    assert!(
        content_modified.contains("    "),
        "After toggling indentation, Tab should insert spaces"
    );

    // Undo the tab
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Reset buffer settings
    run_command(&mut harness, "Reset Buffer Settings");

    // Verify settings are restored to Go defaults
    let screen_reset = harness.screen_to_string();
    assert!(
        !screen_reset.contains('→'),
        "After reset, Go should hide tab indicators again. Screen:\n{}",
        screen_reset
    );

    // Verify tabs are restored - type a tab
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    let content_reset = harness.get_buffer_content().unwrap();
    assert!(
        content_reset.ends_with('\t'),
        "After reset, Go should use tabs again. Got: {:?}",
        content_reset
    );
}
