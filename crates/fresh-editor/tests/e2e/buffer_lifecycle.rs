// End-to-end tests for buffer lifecycle: save, close, quit with modifications

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that saving an unnamed buffer triggers SaveAs prompt (fix for issue #154)
#[test]
fn test_save_unnamed_buffer_shows_save_as_prompt() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a new empty buffer
    harness.new_buffer().unwrap();

    // Type some text
    harness.type_text("Hello world").unwrap();
    harness.render().unwrap();

    // Verify buffer shows modified indicator (*) in tab
    harness.assert_screen_contains("*");

    // Try to save with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show SaveAs prompt (not crash)
    harness.assert_screen_contains("Save as:");
}

/// Test that quitting with modified buffers shows confirmation and doesn't quit immediately
#[test]
fn test_quit_with_modified_buffers_shows_confirmation() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text to modify the buffer
    harness.type_text("Modified content").unwrap();
    harness.render().unwrap();

    // Try to quit with Ctrl+Q
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should NOT quit immediately - there's a confirmation prompt
    assert!(
        !harness.should_quit(),
        "Editor should not quit immediately with unsaved changes"
    );
}

/// Test that quitting without modified buffers works immediately
#[test]
fn test_quit_without_modified_buffers() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Don't type anything - buffer is not modified

    // Quit should work immediately
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Editor should signal quit
    assert!(
        harness.should_quit(),
        "Editor should quit when no modified buffers"
    );
}

/// Test that quitting with confirmation (discard) works
#[test]
fn test_quit_with_confirmation_discard() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Modify buffer
    harness.type_text("Modified").unwrap();
    harness.render().unwrap();

    // Try to quit
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Confirm with 'd' (discard) and Enter
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Editor should quit
    assert!(harness.should_quit(), "Editor should quit after confirming");
}

/// Test that quitting with confirmation (cancel) cancels quit
#[test]
fn test_quit_with_confirmation_cancel() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Modify buffer
    harness.type_text("Modified").unwrap();
    harness.render().unwrap();

    // Try to quit
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Cancel with 'c' and Enter (or any non-'d' key, default is cancel)
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Editor should NOT quit
    assert!(
        !harness.should_quit(),
        "Editor should not quit after canceling"
    );
}

/// Test that undo restores non-dirty status when undoing all changes
#[test]
fn test_undo_restores_non_dirty_status() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Buffer should not show modified indicator initially
    harness.render().unwrap();
    let screen_before = harness.screen_to_string();
    // New buffer shouldn't have * in tab (check tab area, row 1)
    let tab_row: String = screen_before.lines().nth(1).unwrap_or("").to_string();
    assert!(
        !tab_row.contains('*'),
        "New buffer should not show modified indicator"
    );

    // Type some text
    harness.type_text("abc").unwrap();
    harness.render().unwrap();

    // Buffer should show modified indicator
    harness.assert_screen_contains("*");

    // Undo three times to remove all characters
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Buffer should be back to non-modified state (no * in tab)
    let screen_after = harness.screen_to_string();
    let tab_row_after: String = screen_after.lines().nth(1).unwrap_or("").to_string();
    assert!(
        !tab_row_after.contains('*'),
        "Buffer should not show modified indicator after undoing all changes"
    );
}

/// Test that save then undo correctly tracks modified status
#[test]
fn test_undo_after_save_modified_status() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create a file and open it
    let file_path = project_dir.join("test.txt");
    std::fs::write(&file_path, "initial").unwrap();
    harness.open_file(&file_path).unwrap();

    // Buffer should not be modified after opening
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    let tab_row: String = screen.lines().nth(1).unwrap_or("").to_string();
    assert!(
        !tab_row.contains('*'),
        "Buffer should not be modified after opening"
    );

    // Type some text
    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Buffer should be modified
    harness.assert_screen_contains("*");

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Buffer should NOT be modified after save (check for "Saved" message too)
    harness.assert_screen_contains("Saved");
    let screen_after_save = harness.screen_to_string();
    let tab_row_after_save: String = screen_after_save.lines().nth(1).unwrap_or("").to_string();
    assert!(
        !tab_row_after_save.contains('*'),
        "Buffer should not be modified after save"
    );

    // Type more text
    harness.type_text("Y").unwrap();
    harness.render().unwrap();

    // Buffer should be modified again
    harness.assert_screen_contains("*");

    // Undo the 'Y'
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Buffer should be back to saved state (not modified)
    let screen_after_undo = harness.screen_to_string();
    let tab_row_after_undo: String = screen_after_undo.lines().nth(1).unwrap_or("").to_string();
    assert!(
        !tab_row_after_undo.contains('*'),
        "Buffer should not be modified after undoing to saved state"
    );
}

/// Test that tabs show the X close button
#[test]
fn test_tabs_show_close_button() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Render
    harness.render().unwrap();

    // Get the screen content
    let screen = harness.screen_to_string();

    // The tab bar should contain the × character for close button
    // Tab format is " {name}{modified} × "
    assert!(screen.contains('×'), "Tab bar should show close button (×)");
}

/// Test clicking the X button on a tab closes the buffer
#[test]
fn test_click_tab_close_button() {
    use crate::common::harness::layout;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create two temp files
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file1_path = temp_dir.path().join("first.txt");
    let file2_path = temp_dir.path().join("to_close.txt");
    std::fs::write(&file1_path, "First file content").unwrap();
    std::fs::write(&file2_path, "UNIQUE_CONTENT_TO_CLOSE").unwrap();

    // Open first file
    harness.open_file(&file1_path).unwrap();
    harness.render().unwrap();

    // Open second file as a new tab
    harness.open_file(&file2_path).unwrap();
    harness.render().unwrap();

    // Verify the content is visible before closing
    harness.assert_screen_contains("UNIQUE_CONTENT_TO_CLOSE");

    // Find the × character position in the tab bar (row 1)
    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    // Count tabs before close (count × characters)
    let tabs_before = tab_row.matches('×').count();
    assert_eq!(tabs_before, 2, "Should have 2 tabs before close");

    // Find the position of the second × in the tab bar (active tab's close button)
    // The active tab is the one we just opened with content
    let x_positions: Vec<usize> = tab_row.match_indices('×').map(|(i, _)| i).collect();
    let x_pos = x_positions[1]; // Second tab (the one with content)

    // Click on the × button
    harness
        .mouse_click(x_pos as u16, layout::TAB_BAR_ROW as u16)
        .unwrap();
    harness.render().unwrap();

    // Verify the content is no longer visible
    let screen_after = harness.screen_to_string();
    assert!(
        !screen_after.contains("UNIQUE_CONTENT_TO_CLOSE"),
        "Content should no longer be visible after closing tab"
    );

    // Verify there's only one tab now
    let tab_row_after: String = screen_after
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();
    let tabs_after = tab_row_after.matches('×').count();
    assert_eq!(tabs_after, 1, "Should have 1 tab after close");
}

/// Test clicking X on modified buffer shows confirmation prompt
#[test]
fn test_click_tab_close_button_modified_buffer() {
    use crate::common::harness::layout;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a second buffer
    harness.new_buffer().unwrap();

    // Type some text to modify it
    harness.type_text("Modified content").unwrap();
    harness.render().unwrap();

    // Verify buffer is modified (shows *)
    harness.assert_screen_contains("*");

    // Find the × character position in the tab bar for the active (modified) tab
    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    // The active tab should have * before × - find the × that has * before it
    // Tab format: " [No Name]* × "
    if let Some(star_pos) = tab_row.find('*') {
        // The × should be after the * (with a space in between)
        if let Some(x_pos) = tab_row[star_pos..].find('×') {
            let actual_x_pos = star_pos + x_pos;
            // Click on the × button
            harness
                .mouse_click(actual_x_pos as u16, layout::TAB_BAR_ROW as u16)
                .unwrap();
            harness.render().unwrap();

            // Should show confirmation prompt for modified buffer
            harness.assert_screen_contains("modified. (s)ave, (d)iscard, (C)ancel");
        } else {
            panic!("Could not find × close button after * in tab bar");
        }
    } else {
        panic!("Could not find * modified indicator in tab bar");
    }
}

/// Test clicking X on modified buffer and choosing discard
#[test]
fn test_click_tab_close_modified_discard() {
    use crate::common::harness::layout;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a second buffer
    harness.new_buffer().unwrap();

    // Type some text to modify it
    harness.type_text("Will discard").unwrap();
    harness.render().unwrap();

    // Find and click the × button for the modified tab
    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    if let Some(star_pos) = tab_row.find('*') {
        if let Some(x_pos) = tab_row[star_pos..].find('×') {
            let actual_x_pos = star_pos + x_pos;
            harness
                .mouse_click(actual_x_pos as u16, layout::TAB_BAR_ROW as u16)
                .unwrap();
            harness.render().unwrap();

            // Should show prompt
            harness.assert_screen_contains("modified. (s)ave, (d)iscard, (C)ancel");

            // Press 'd' to discard and Enter to confirm
            harness
                .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
                .unwrap();
            harness
                .send_key(KeyCode::Enter, KeyModifiers::NONE)
                .unwrap();
            harness.render().unwrap();

            // Should show discarded message (use shorter match due to status bar truncation)
            harness.assert_screen_contains("discar");
        } else {
            panic!("Could not find × close button after * in tab bar");
        }
    } else {
        panic!("Could not find * modified indicator in tab bar");
    }
}

/// Test clicking X on modified buffer and choosing cancel
#[test]
fn test_click_tab_close_modified_cancel() {
    use crate::common::harness::layout;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a second buffer
    harness.new_buffer().unwrap();

    // Type some text to modify it
    harness.type_text("Keep this").unwrap();
    harness.render().unwrap();

    // Find and click the × button for the modified tab
    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    if let Some(star_pos) = tab_row.find('*') {
        if let Some(x_pos) = tab_row[star_pos..].find('×') {
            let actual_x_pos = star_pos + x_pos;
            harness
                .mouse_click(actual_x_pos as u16, layout::TAB_BAR_ROW as u16)
                .unwrap();
            harness.render().unwrap();

            // Should show prompt
            harness.assert_screen_contains("modified. (s)ave, (d)iscard, (C)ancel");

            // Press 'c' to cancel and Enter to confirm
            harness
                .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
                .unwrap();
            harness
                .send_key(KeyCode::Enter, KeyModifiers::NONE)
                .unwrap();
            harness.render().unwrap();

            // Should show cancelled message
            harness.assert_screen_contains("Close cancelled");
            // Buffer content should still be there
            harness.assert_screen_contains("Keep this");
        } else {
            panic!("Could not find × close button after * in tab bar");
        }
    } else {
        panic!("Could not find * modified indicator in tab bar");
    }
}

/// Test that next/previous buffer commands skip hidden buffers
/// Bug: When cycling through buffers with next_buffer/prev_buffer,
/// the editor would focus hidden buffers instead of skipping them
#[test]
fn test_next_buffer_skips_hidden_buffers() {
    use fresh::primitives::text_property::TextPropertyEntry;
    use fresh::services::plugins::api::PluginCommand;
    use std::collections::HashMap;

    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create two visible files
    let file1_path = project_dir.join("visible1.txt");
    let file2_path = project_dir.join("visible2.txt");
    std::fs::write(&file1_path, "VISIBLE_BUFFER_1_CONTENT").unwrap();
    std::fs::write(&file2_path, "VISIBLE_BUFFER_2_CONTENT").unwrap();

    // Open first visible file
    harness.open_file(&file1_path).unwrap();
    harness.render().unwrap();

    // Create a hidden buffer using the plugin API
    let hidden_cmd = PluginCommand::CreateVirtualBufferWithContent {
        name: "*Hidden*".to_string(),
        mode: "hidden-test".to_string(),
        read_only: true,
        entries: vec![TextPropertyEntry {
            text: "HIDDEN_BUFFER_CONTENT".to_string(),
            properties: HashMap::new(),
        }],
        show_line_numbers: true,
        show_cursors: true,
        editing_disabled: true,
        hidden_from_tabs: true, // <-- This makes it hidden
        request_id: None,
    };
    harness
        .editor_mut()
        .handle_plugin_command(hidden_cmd)
        .unwrap();
    harness.render().unwrap();

    // Open second visible file
    harness.open_file(&file2_path).unwrap();
    harness.render().unwrap();

    // Verify we're on visible2
    harness.assert_screen_contains("VISIBLE_BUFFER_2_CONTENT");

    // Now we have 3 buffers in open_buffers:
    // - visible1.txt (VISIBLE_BUFFER_1_CONTENT)
    // - *Hidden* (hidden_from_tabs=true, HIDDEN_BUFFER_CONTENT)
    // - visible2.txt (VISIBLE_BUFFER_2_CONTENT) - currently active

    // Cycle through buffers using next_buffer (Ctrl+PageDown)
    // We should only ever see visible1.txt or visible2.txt content, never the hidden buffer
    for i in 0..6 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        println!("After next_buffer #{}: screen:\n{}", i + 1, screen);

        // Should NEVER show the hidden buffer content
        assert!(
            !screen.contains("HIDDEN_BUFFER_CONTENT"),
            "next_buffer should skip hidden buffer. Iteration {}. Screen:\n{}",
            i + 1,
            screen
        );

        // Should always show one of the visible buffer contents
        assert!(
            screen.contains("VISIBLE_BUFFER_1_CONTENT")
                || screen.contains("VISIBLE_BUFFER_2_CONTENT"),
            "Should be on a visible buffer. Iteration {}. Screen:\n{}",
            i + 1,
            screen
        );
    }

    // Also test prev_buffer (Ctrl+PageUp)
    for i in 0..6 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        println!("After prev_buffer #{}: screen:\n{}", i + 1, screen);

        // Should NEVER show the hidden buffer content
        assert!(
            !screen.contains("HIDDEN_BUFFER_CONTENT"),
            "prev_buffer should skip hidden buffer. Iteration {}. Screen:\n{}",
            i + 1,
            screen
        );

        // Should always show one of the visible buffer contents
        assert!(
            screen.contains("VISIBLE_BUFFER_1_CONTENT")
                || screen.contains("VISIBLE_BUFFER_2_CONTENT"),
            "Should be on a visible buffer. Iteration {}. Screen:\n{}",
            i + 1,
            screen
        );
    }
}
