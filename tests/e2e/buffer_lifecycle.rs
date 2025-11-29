// End-to-end tests for buffer lifecycle: save, close, quit with modifications

use crate::common::fixtures::TestFixture;
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

/// Test that quitting with modified buffers shows confirmation
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

    // Should show confirmation prompt about unsaved changes
    harness.assert_screen_contains("unsaved");
    harness.assert_screen_contains("Quit");
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

/// Test that quitting with confirmation (y) works
#[test]
fn test_quit_with_confirmation_yes() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Modify buffer
    harness.type_text("Modified").unwrap();
    harness.render().unwrap();

    // Try to quit
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Confirm with 'y' and Enter
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Editor should quit
    assert!(harness.should_quit(), "Editor should quit after confirming");
}

/// Test that quitting with confirmation (n) cancels quit
#[test]
fn test_quit_with_confirmation_no() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Modify buffer
    harness.type_text("Modified").unwrap();
    harness.render().unwrap();

    // Try to quit
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Cancel with 'n' and Enter
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
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
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a file and open it
    let fixture = TestFixture::new("test.txt", "initial").unwrap();
    harness.open_file(&fixture.path).unwrap();

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

    // Create a second buffer so we can close one
    harness.new_buffer().unwrap();
    harness.render().unwrap();

    // Find the × character position in the tab bar (row 1)
    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    // Find the position of the first × in the tab bar
    if let Some(x_pos) = tab_row.find('×') {
        // Click on the × button
        harness
            .mouse_click(x_pos as u16, layout::TAB_BAR_ROW as u16)
            .unwrap();
        harness.render().unwrap();

        // Should close the buffer (show "closed" message)
        harness.assert_screen_contains("Buffer closed");
    } else {
        panic!("Could not find × close button in tab bar");
    }
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

            // Should show discarded message
            harness.assert_screen_contains("discarded");
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
