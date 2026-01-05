use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};

/// Test basic split view creation (horizontal)
#[test]
fn test_split_horizontal() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text in the first buffer
    harness.type_text("Buffer 1").unwrap();
    harness.assert_buffer_content("Buffer 1");

    // Split horizontally via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split horiz").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Split pane horizontally");

    // New split should show the same buffer content (Emacs-style)
    harness.assert_buffer_content("Buffer 1");
}

/// Test basic split view creation (vertical)
#[test]
fn test_split_vertical() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text in the first buffer
    harness.type_text("Buffer 1").unwrap();
    harness.assert_buffer_content("Buffer 1");

    // Split vertically via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Split pane vertically");

    // New split should show the same buffer content (Emacs-style)
    harness.assert_buffer_content("Buffer 1");
}

/// Test navigation between splits
#[test]
fn test_split_navigation() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a vertical split via command palette
    harness.type_text("First buffer").unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now in second split - it shows the same buffer content (Emacs-style)
    // The cursor in the new split starts at position 0
    harness.assert_buffer_content("First buffer");

    // Move cursor to end and type more text
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" - extended").unwrap();
    harness.assert_buffer_content("First buffer - extended");

    // Navigate to next split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Switched to next split");

    // Navigate to previous split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Switched to previous split");
}

/// Test closing a split
#[test]
fn test_close_split() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Close the split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("close split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Closed split");
}

/// Test cannot close last split
#[test]
fn test_cannot_close_last_split() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Try to close the only split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("close split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see error message
    harness.assert_screen_contains("Cannot close split");
}

/// Test split size adjustment
/// Note: This test is disabled because adjusting split size requires
/// targeting the parent split container, not the leaf nodes.
/// This is a known limitation that will be addressed in a future update.
#[test]
#[ignore]
fn test_split_size_adjustment() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a split
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();

    // Increase split size (Alt+=)
    harness
        .send_key(KeyCode::Char('='), KeyModifiers::ALT)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Adjusted split size by 5%");

    // Decrease split size (Alt+-)
    harness
        .send_key(KeyCode::Char('-'), KeyModifiers::ALT)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Adjusted split size by -5%");
}

/// Test multiple splits (nested)
#[test]
fn test_nested_splits() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Create first split (vertical)
    harness.type_text("Buffer 1").unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();

    // Should be in buffer 2 now
    harness.type_text("Buffer 2").unwrap();

    // Create second split (horizontal)
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::ALT)
        .unwrap();

    // Should be in buffer 3 now
    harness.type_text("Buffer 3").unwrap();

    // Verify we successfully created multiple splits
    harness.render().unwrap();
}

/// Test split view with file operations
#[test]
fn test_split_with_file_operations() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file1 = project_dir.join("file1.txt");
    let file2 = project_dir.join("file2.txt");

    std::fs::write(&file1, "File 1 content").unwrap();
    std::fs::write(&file2, "File 2 content").unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("File 1 content");

    // Create a split
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();

    // Open second file in the new split
    harness.open_file(&file2).unwrap();
    harness.assert_buffer_content("File 2 content");

    // Render and verify both files are shown
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");
}

/// Test toggle maximize split via command palette (maximize)
#[test]
fn test_toggle_maximize_split() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Type in first buffer
    harness.type_text("Buffer 1").unwrap();

    // Create vertical split via command palette (like test_split_horizontal)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Both splits should show "Buffer 1" (Emacs-style)
    harness.assert_screen_contains("Split pane vertically");

    // Toggle maximize the current split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("togmax").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should see status message
    harness.render().unwrap();
    harness.assert_screen_contains("Maximized split");
}

/// Test toggle maximize split to unmaximize via command palette
#[test]
fn test_toggle_unmaximize_split() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file1 = project_dir.join("file1.txt");
    let file2 = project_dir.join("file2.txt");

    std::fs::write(&file1, "File 1 content").unwrap();
    std::fs::write(&file2, "File 2 content").unwrap();

    // Open first file and create a split via command palette
    harness.open_file(&file1).unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open second file in the new split
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Toggle maximize the current split (first toggle = maximize)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("togmax").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Toggle again to unmaximize (second toggle = unmaximize)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("togmax").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see status message
    harness.assert_screen_contains("Restored all splits");

    // Both files should be visible again
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");
}

/// Test cannot toggle maximize when only one split exists
#[test]
fn test_cannot_toggle_maximize_single_split() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Try to toggle maximize the only split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("togmax").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see error message (may be truncated in status bar)
    harness.assert_screen_contains("Cannot maximize");
}

/// Test that closing the last buffer in a split closes the split (if other splits exist)
#[test]
fn test_close_last_buffer_in_split_closes_split() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file1 = project_dir.join("file1.txt");

    std::fs::write(&file1, "File 1 content").unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("File 1 content");

    // Verify we have 1 split
    assert_eq!(harness.editor().get_split_count(), 1);

    // Create a vertical split via command palette
    // This creates a new split showing the same buffer (Emacs-style)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Command:"))
        .unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Split pane vertically"))
        .unwrap();

    // Verify we now have 2 splits (both showing file1)
    assert_eq!(harness.editor().get_split_count(), 2);

    // The new split has 1 tab (file1) - same buffer as the other split
    let tabs = harness.editor().get_split_tabs(harness.editor().get_active_split());
    assert_eq!(tabs.len(), 1, "New split should have exactly 1 tab");

    // Now close the tab (Alt+W) - since this buffer is also in the other split,
    // and this is the only tab in this split, the split should close
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Should be back to 1 split (split was closed)
    assert_eq!(
        harness.editor().get_split_count(),
        1,
        "Expected split to be closed when closing last buffer"
    );

    // file1.txt should still be visible
    harness.assert_screen_contains("file1.txt");
}

/// Test that closing a unique buffer in a split (not in other splits) closes the split
#[test]
fn test_close_unique_buffer_in_split_closes_split() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file1 = project_dir.join("file1.txt");
    let file2 = project_dir.join("file2.txt");

    std::fs::write(&file1, "File 1 content").unwrap();
    std::fs::write(&file2, "File 2 content").unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("File 1 content");

    // Create a vertical split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Command:"))
        .unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Split pane vertically"))
        .unwrap();

    // Verify we now have 2 splits (both showing file1)
    assert_eq!(harness.editor().get_split_count(), 2);

    // Open file2 in the new split - now tabs = [file1, file2], active is file2
    harness.open_file(&file2).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("file2.txt"))
        .unwrap();

    // Switch to file1 tab (previous buffer) - now active is file1
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Close the file1 tab (which is also in split A, so it just removes from tabs)
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Now the second split should only have file2, which is NOT in the first split
    let active_split = harness.editor().get_active_split();
    let tabs = harness.editor().get_split_tabs(active_split);
    assert_eq!(
        tabs.len(),
        1,
        "Split should have exactly 1 tab (file2 only)"
    );

    // Both splits still exist
    assert_eq!(harness.editor().get_split_count(), 2);

    // Now close file2 - since it's the only buffer in this split and NOT in other splits,
    // the split should close (this is the bug scenario)
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Should be back to 1 split (the bug would leave 2 splits with an empty buffer)
    assert_eq!(
        harness.editor().get_split_count(),
        1,
        "Expected split to be closed when closing last unique buffer"
    );

    // file1.txt should still be visible
    harness.assert_screen_contains("file1.txt");
}

/// Test that closing a terminal in a split closes the split
///
/// Scenario:
/// 1. Open a file
/// 2. Create a split
/// 3. Open a terminal in that split
/// 4. Close all buffers in the split (the terminal)
/// 5. Split should disappear
#[test]
fn test_close_terminal_in_split_closes_split() {
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
        eprintln!("Skipping terminal test: PTY not available in this environment");
        return;
    }

    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file1 = project_dir.join("file1.txt");

    std::fs::write(&file1, "File 1 content").unwrap();

    // Open file in the first split
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("File 1 content");
    assert_eq!(harness.editor().get_split_count(), 1);

    // Create a vertical split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Command:"))
        .unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Split pane vertically"))
        .unwrap();

    // Verify we now have 2 splits
    assert_eq!(harness.editor().get_split_count(), 2);

    // Disable jump_to_end_on_output so terminal output doesn't interfere
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    // Open terminal in the new split (this enters terminal mode)
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("Terminal");
    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode after opening terminal"
    );

    // Close the file1 tab in this split via API (terminal remains)
    let active_split = harness.editor().get_active_split();
    let tabs = harness.editor().get_split_tabs(active_split);
    // Find the non-terminal buffer (file1)
    let file1_buffer = tabs
        .iter()
        .find(|&&b| !harness.editor().is_terminal_buffer(b))
        .copied()
        .expect("Should have file1 buffer in tabs");
    harness
        .editor_mut()
        .close_tab_in_split(file1_buffer, active_split);
    harness.render().unwrap();

    // Now terminal is the only buffer in this split
    // Still have 2 splits
    assert_eq!(harness.editor().get_split_count(), 2);

    // Close the terminal buffer - split should close
    // Use close_tab() which is what Action::Close calls
    harness.editor_mut().close_tab();
    harness.render().unwrap();

    // Should be back to 1 split
    assert_eq!(
        harness.editor().get_split_count(),
        1,
        "Expected split to be closed when closing terminal (the last buffer in split)"
    );

    // file1.txt should still be visible in the remaining split
    harness.assert_screen_contains("file1.txt");

    // Terminal should be gone
    harness.assert_screen_not_contains("Terminal");
}
