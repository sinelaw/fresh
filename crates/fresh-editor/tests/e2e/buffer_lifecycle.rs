// End-to-end tests for buffer lifecycle: save, close, quit with modifications

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Test that saving an unnamed buffer triggers SaveAs prompt (fix for issue #154)
#[test]
fn test_save_unnamed_buffer_shows_save_as_prompt() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

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
    let mut config = Config::default();
    config.editor.hot_exit = false;
    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();

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
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

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
    let mut config = Config::default();
    config.editor.hot_exit = false;
    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();

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
    let mut config = Config::default();
    config.editor.hot_exit = false;
    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();

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
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

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
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
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
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

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

    let mut harness = EditorTestHarness::new(100, 24).unwrap();

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

    let mut harness = EditorTestHarness::new(100, 24).unwrap();

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

    let mut harness = EditorTestHarness::new(100, 24).unwrap();

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
            harness.assert_screen_contains("Buffer closed");
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

    let mut harness = EditorTestHarness::new(100, 24).unwrap();

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

    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
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
            style: None,
            inline_overlays: Vec::new(),
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

/// Test that closing a buffer returns to the previously focused buffer (not just adjacent tab)
#[test]
fn test_close_returns_to_previous_focused() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Create temp files with identifiable content
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_a = temp_dir.path().join("file_a.txt");
    let file_b = temp_dir.path().join("file_b.txt");
    let file_c = temp_dir.path().join("file_c.txt");
    std::fs::write(&file_a, "CONTENT_A").unwrap();
    std::fs::write(&file_b, "CONTENT_B").unwrap();
    std::fs::write(&file_c, "CONTENT_C").unwrap();

    // Open A, B, C in that order
    harness.open_file(&file_a).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("CONTENT_A");

    harness.open_file(&file_b).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("CONTENT_B");

    harness.open_file(&file_c).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("CONTENT_C");

    // Focus order: A -> B -> C
    // Now switch to A
    // Find file_a tab and click it - but that's complex, let's use the switch command
    // Actually, let's use Ctrl+Tab or similar to switch tabs
    // For simplicity, let's re-open A to make it active (it won't duplicate)

    // Switch to file A by opening it again (it will just switch to it)
    harness.open_file(&file_a).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("CONTENT_A");

    // Focus order is now: B -> C -> A
    // Switch to B
    harness.open_file(&file_b).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("CONTENT_B");

    // Focus order is now: C -> A -> B
    // Now B is active
    // Close B - should return to A (the previously focused buffer), not C (adjacent)
    // Note: Default keybinding for close_tab is Alt+W
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Should now be on A
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("CONTENT_A"),
        "After closing B, should return to previously focused A. Screen:\n{}",
        screen
    );
}

/// Build a harness with a real temp project root but plugin loading disabled,
/// so plugin-driven UI (e.g. the welcome plugin's Dashboard) doesn't race
/// with the editor-core close behavior we want to assert on.
fn isolated_project_harness(config: Config) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_empty_plugins_dir()
            .with_config(config),
    )
    .unwrap()
}

/// Row that the tab bar is rendered on (just below the menu bar).
const TAB_BAR_ROW: u16 = 1;

/// Default: closing the last buffer auto-opens the file explorer and
/// auto-creates a fresh `[No Name]` tab. This is the baseline the two
/// new config flags below opt out of (issue #1753).
#[test]
fn test_close_last_buffer_default_opens_explorer_and_empty_tab() {
    let mut harness = isolated_project_harness(Config::default());
    let project_root = harness.project_dir().unwrap();
    let file = project_root.join("only.txt");
    std::fs::write(&file, "only content").unwrap();

    harness.open_file(&file).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("only content");

    // Close the only buffer.
    harness.editor_mut().close_tab();
    // File explorer init is async — wait for the panel to appear.
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // A fresh empty tab is also auto-created in the tab bar.
    assert!(
        harness.screen_row_text(TAB_BAR_ROW).contains("[No Name]"),
        "Expected `[No Name]` tab in tab bar. Tab bar:\n{}",
        harness.screen_row_text(TAB_BAR_ROW)
    );
}

/// `file_explorer.auto_open_on_last_buffer_close = false` keeps the
/// explorer hidden when the last buffer is closed. The empty `[No Name]`
/// buffer still appears because that toggle is independent.
#[test]
fn test_close_last_buffer_does_not_open_explorer_when_disabled() {
    let mut config = Config::default();
    config.file_explorer.auto_open_on_last_buffer_close = false;
    let mut harness = isolated_project_harness(config);
    let project_root = harness.project_dir().unwrap();
    let file = project_root.join("only.txt");
    std::fs::write(&file, "only content").unwrap();

    harness.open_file(&file).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("only content");

    harness.editor_mut().close_tab();
    // The `[No Name]` tab appearing is the semantic signal that the close
    // has fully propagated.
    harness
        .wait_until(|h| h.screen_row_text(TAB_BAR_ROW).contains("[No Name]"))
        .unwrap();

    // Explorer panel is NOT shown.
    harness.assert_screen_not_contains("File Explorer");
}

/// `editor.auto_create_empty_buffer_on_last_buffer_close = false` hides
/// the synthesized `[No Name]` buffer from the tab bar so the workspace
/// looks blank — the file explorer still opens, which gives us a
/// semantic wait point.
#[test]
fn test_close_last_buffer_hides_empty_tab_when_disabled() {
    let mut config = Config::default();
    config.editor.auto_create_empty_buffer_on_last_buffer_close = false;
    let mut harness = isolated_project_harness(config);
    let project_root = harness.project_dir().unwrap();
    let file = project_root.join("only.txt");
    std::fs::write(&file, "only content").unwrap();

    harness.open_file(&file).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("only content");

    harness.editor_mut().close_tab();
    // Wait until the explorer (which is still allowed to auto-open here) is
    // on screen, then assert the `[No Name]` tab is absent from the tab bar.
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    let tab_bar = harness.screen_row_text(TAB_BAR_ROW);
    assert!(
        !tab_bar.contains("[No Name]"),
        "Expected no `[No Name]` tab. Tab bar:\n{}",
        tab_bar
    );
}

/// Both options off → fully blank workspace: no file explorer, no
/// `[No Name]` tab. This is the workflow requested in issue #1753.
#[test]
fn test_close_last_buffer_blank_workspace_when_both_disabled() {
    let mut config = Config::default();
    config.file_explorer.auto_open_on_last_buffer_close = false;
    config.editor.auto_create_empty_buffer_on_last_buffer_close = false;
    let mut harness = isolated_project_harness(config);
    let project_root = harness.project_dir().unwrap();
    let file = project_root.join("only.txt");
    std::fs::write(&file, "only content").unwrap();

    harness.open_file(&file).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("only content");

    harness.editor_mut().close_tab();
    // Stable-screen wait: the original buffer's contents are gone and
    // nothing replaces them in the editor pane.
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("only content"))
        .unwrap();

    harness.assert_screen_not_contains("File Explorer");
    let tab_bar = harness.screen_row_text(TAB_BAR_ROW);
    assert!(
        !tab_bar.contains("[No Name]"),
        "Expected no `[No Name]` tab. Tab bar:\n{}",
        tab_bar
    );
}
