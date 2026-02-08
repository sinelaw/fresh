//! Tests for buffer settings commands:
//! - Set Tab Size
//! - Toggle Indentation: Spaces ↔ Tabs
//! - Toggle Tab Indicators
//! - Toggle Line Numbers
//! - Reset Buffer Settings

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::time::Duration;
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

/// Delay between file writes to ensure filesystem notifications are received.
const FILE_CHANGE_DELAY: Duration = Duration::from_millis(2100);

/// Write content to a file and sync to disk to ensure filesystem notifications fire.
fn write_and_sync(path: &Path, content: &str) {
    let mut file = File::create(path).unwrap();
    file.write_all(content.as_bytes()).unwrap();
    file.sync_all().unwrap();
    drop(file);

    // Also sync the parent directory to ensure the directory entry is flushed
    if let Some(parent) = path.parent() {
        if let Ok(dir) = File::open(parent) {
            let _ = dir.sync_all();
        }
    }
}

/// Test that "Toggle Line Numbers" persists across external file changes and saves.
///
/// This test verifies that when line numbers are toggled off, the setting is
/// preserved through:
/// 1. External file modifications (auto-revert)
/// 2. User edits
/// 3. File saves
#[test]
#[cfg_attr(target_os = "macos", ignore)] // FSEvents coalescing can cause flaky timing
fn test_toggle_line_numbers_persists_across_file_changes() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create initial file with some content
    write_and_sync(&file_path, "Initial line 1\nInitial line 2\nInitial line 3");

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Step 1: Verify line numbers are enabled by default
    let screen_initial = harness.screen_to_string();
    assert!(
        screen_initial.contains("1 │"),
        "Line numbers should be visible by default. Screen:\n{}",
        screen_initial
    );

    // Step 2: Disable line numbers using command palette
    run_command(&mut harness, "Toggle Line Numbers");

    // Step 3: Verify line numbers are now hidden
    let screen_after_toggle = harness.screen_to_string();
    assert!(
        !screen_after_toggle.contains("1 │"),
        "Line numbers should be hidden after toggle. Screen:\n{}",
        screen_after_toggle
    );
    assert!(
        !screen_after_toggle.contains("2 │"),
        "Line numbers should be hidden after toggle. Screen:\n{}",
        screen_after_toggle
    );

    // Step 4: Overwrite the file externally (not using the editor)
    harness.sleep(FILE_CHANGE_DELAY);
    write_and_sync(&file_path, "Modified line 1\nModified line 2\nModified line 3\nModified line 4");

    // Wait for auto-revert to process the external change
    let expected_content = "Modified line 1\nModified line 2\nModified line 3\nModified line 4";
    harness
        .wait_until(|h| h.get_buffer_content().unwrap() == expected_content)
        .expect("Auto-revert should update buffer content");

    // Step 5: Verify line numbers are still disabled after auto-revert
    harness.render().unwrap();
    let screen_after_revert = harness.screen_to_string();
    assert!(
        !screen_after_revert.contains("1 │"),
        "Line numbers should remain hidden after auto-revert. Screen:\n{}",
        screen_after_revert
    );
    assert!(
        !screen_after_revert.contains("2 │"),
        "Line numbers should remain hidden after auto-revert. Screen:\n{}",
        screen_after_revert
    );
    // Verify the new content is displayed
    harness.assert_screen_contains("Modified line 1");

    // Step 6: Make an edit in the buffer
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("\nEdited line 5").unwrap();
    harness.render().unwrap();

    // Step 7: Verify line numbers are still disabled after editing
    let screen_after_edit = harness.screen_to_string();
    assert!(
        !screen_after_edit.contains("1 │"),
        "Line numbers should remain hidden after editing. Screen:\n{}",
        screen_after_edit
    );
    assert!(
        !screen_after_edit.contains("5 │"),
        "Line numbers should remain hidden after editing. Screen:\n{}",
        screen_after_edit
    );

    // Step 8: Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Step 9: Verify line numbers are still disabled after saving
    let screen_after_save = harness.screen_to_string();
    assert!(
        !screen_after_save.contains("1 │"),
        "Line numbers should remain hidden after save. Screen:\n{}",
        screen_after_save
    );
    assert!(
        !screen_after_save.contains("5 │"),
        "Line numbers should remain hidden after save. Screen:\n{}",
        screen_after_save
    );

    // Verify the edited content is still visible
    harness.assert_screen_contains("Edited line 5");
}
