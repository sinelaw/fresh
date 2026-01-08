use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test file explorer toggle
#[test]
fn test_file_explorer_toggle() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Initially file explorer should not be visible
    harness.render().unwrap();
    let screen_before = harness.screen_to_string();

    // Toggle file explorer on with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for file explorer to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("File Explorer")
        })
        .unwrap();

    // Screen should show file explorer (check for the border or title)
    let screen_after = harness.screen_to_string();

    // Should show "File Explorer" in the UI
    assert!(
        screen_after.contains("File Explorer"),
        "Screen should show file explorer after toggle"
    );

    // Toggle file explorer off with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // File Explorer text should no longer be visible
    let screen_final = harness.screen_to_string();
    if screen_before.contains("File Explorer") {
        // If it was there before, it should still be there
        assert!(screen_final.contains("File Explorer"));
    } else {
        // If it wasn't there before, check that the screen returned to normal
        // (allowing for status message changes)
        assert_eq!(
            screen_before.lines().count(),
            screen_final.lines().count(),
            "Screen should return to similar state when file explorer is toggled off"
        );
    }
}

/// Test file explorer displays directory structure
#[test]
fn test_file_explorer_shows_directory_structure() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create some files and directories
    fs::create_dir(project_root.join("src")).unwrap();
    fs::write(project_root.join("src/main.rs"), "fn main() {}").unwrap();
    fs::write(project_root.join("Cargo.toml"), "[package]").unwrap();
    fs::create_dir(project_root.join("tests")).unwrap();
    fs::write(project_root.join("README.md"), "# Project").unwrap();

    // Toggle file explorer on with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Wait a moment for async file system operations
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // Check that we see the project structure
    // Note: The exact rendering might differ, but we should see some files
    let screen = harness.screen_to_string();
    println!("File explorer screen:\n{screen}");

    // Should show at least the root directory name or some indication of files
    // (This is a basic check - the exact content depends on rendering)
}

/// Test file explorer navigation
#[test]
fn test_file_explorer_navigation() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::write(project_root.join("file1.txt"), "File 1").unwrap();
    fs::write(project_root.join("file2.txt"), "File 2").unwrap();
    fs::write(project_root.join("file3.txt"), "File 3").unwrap();

    // Toggle file explorer on with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();

    // Wait for initialization
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let _screen_initial = harness.screen_to_string();

    // Navigate down with Alt+J
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen_after_down = harness.screen_to_string();

    // Screen should change (selection moved)
    // Note: This might be subtle depending on rendering
    println!("After navigate down:\n{screen_after_down}");

    // Navigate up with Alt+K
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
}

/// Test file explorer expand/collapse
#[test]
fn test_file_explorer_expand_collapse() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::create_dir(project_root.join("src")).unwrap();
    fs::write(project_root.join("src/lib.rs"), "// lib").unwrap();
    fs::write(project_root.join("src/main.rs"), "fn main() {}").unwrap();

    // Toggle file explorer on with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();

    // Wait for initialization
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let screen_before_expand = harness.screen_to_string();
    println!("Before expand:\n{screen_before_expand}");

    // Expand the root directory with Alt+L
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::ALT)
        .unwrap();

    // Wait for async operation
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    let screen_after_expand = harness.screen_to_string();
    println!("After expand:\n{screen_after_expand}");

    // The screen should show more content after expanding
    // (exact assertion depends on rendering details)

    // Collapse with Alt+L (toggle)
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::ALT)
        .unwrap();

    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();
}

/// Test opening a file from file explorer
#[test]
fn test_file_explorer_open_file() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("simple.txt");
    let test_content = "Hello World";
    fs::write(&test_file, test_content).unwrap();

    // Toggle file explorer on with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for file explorer to be visible
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("File Explorer")
        })
        .unwrap();

    let screen_with_explorer = harness.screen_to_string();

    // Verify file explorer is showing
    assert!(
        screen_with_explorer.contains("File Explorer"),
        "File explorer should be visible"
    );

    // The file might already be visible if root auto-expanded (single item case)
    // If not visible, expand the root directory
    if !screen_with_explorer.contains("simple.txt") {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        // Wait for expansion by checking the screen shows the file
        harness
            .wait_until(|h| h.screen_to_string().contains("simple.txt"))
            .unwrap();
    }

    // Navigate down to the file (first child after root)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Try to open with Enter - should work if we're on a file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the file to be opened (content should appear)
    harness
        .wait_until(|h| h.screen_to_string().contains("Hello World"))
        .unwrap();

    // Verify the file was opened
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, test_content,
        "Buffer should contain the opened file's content"
    );
}

/// Test file explorer refresh
#[test]
fn test_file_explorer_refresh() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create initial file
    fs::write(project_root.join("file1.txt"), "File 1").unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();

    // Wait for initialization
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // Add a new file to the directory
    fs::write(project_root.join("file2.txt"), "File 2").unwrap();

    // Refresh the file explorer
    harness.editor_mut().file_explorer_refresh();

    // Wait for refresh
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // The new file should now be visible
    // (This is hard to assert precisely without introspecting the tree structure)
    let screen = harness.screen_to_string();
    println!("After refresh:\n{screen}");
}

/// Test focus switching between file explorer and editor
#[test]
fn test_file_explorer_focus_switching() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Open file explorer
    harness.editor_mut().toggle_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // File explorer should be visible and focused
    assert!(harness.editor().file_explorer_visible());

    // Try using arrow keys - in FileExplorer context, these should navigate the explorer
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    // Toggle file explorer off
    harness.editor_mut().toggle_file_explorer();
    harness.render().unwrap();

    // File explorer should be hidden now
    assert!(!harness.editor().file_explorer_visible());

    // Toggle file explorer back on
    harness.editor_mut().toggle_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Should be visible again
    assert!(harness.editor().file_explorer_visible());

    // Focus the editor (without toggling file explorer off)
    harness.editor_mut().focus_editor();
    harness.render().unwrap();

    // File explorer should still be visible, just not focused
    assert!(harness.editor().file_explorer_visible());
}

/// Test that file explorer keybindings only work when explorer has focus
#[test]
fn test_file_explorer_context_aware_keybindings() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();
    std::fs::write(project_root.join("test.txt"), "content").unwrap();

    // Open file explorer (starts with focus)
    harness.editor_mut().toggle_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Arrow keys should work in file explorer context
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    // Switch to editor context
    harness
        .send_key(KeyCode::Esc, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    // Now arrow keys should work for editor navigation, not file explorer
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();
}

/// Test opening file explorer with focus
#[test]
fn test_focus_file_explorer_action() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Initially, file explorer is not visible
    assert!(!harness.editor().file_explorer_visible());

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // File explorer should now be visible and focused
    assert!(harness.editor().file_explorer_visible());

    // Switch focus back to editor
    harness.editor_mut().focus_editor();
    harness.render().unwrap();

    // File explorer should still be visible
    assert!(harness.editor().file_explorer_visible());

    // Focus file explorer again
    harness.editor_mut().focus_file_explorer();
    harness.render().unwrap();

    // Should still be visible
    assert!(harness.editor().file_explorer_visible());
}

/// Test that opening a file from file explorer actually displays its content
/// This reproduces the bug where a new buffer is created but the content area
/// still shows the old buffer
#[test]
#[ignore = "File explorer directory expansion not working properly - needs investigation of Alt+L keybinding"]
fn test_file_explorer_displays_opened_file_content() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    let file1 = project_root.join("first.txt");
    let file2 = project_root.join("second.txt");
    let content1 = "This is the FIRST file content";
    let content2 = "This is the SECOND file content";

    fs::write(&file1, content1).unwrap();
    fs::write(&file2, content2).unwrap();

    // Open the first file directly
    harness.open_file(&file1).unwrap();
    let screen1 = harness.screen_to_string();
    println!("Screen after opening first file:\n{screen1}");

    // Verify first file content is displayed on screen
    assert!(
        screen1.contains(content1),
        "First file content should be visible on screen after opening"
    );

    // Now open file explorer with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Wait for async file system operations
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Expand the root directory with Alt+L
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::ALT)
        .unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Navigate down to find second.txt with Alt+J
    // We need to find it in the list (first.txt comes before second.txt alphabetically)
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::ALT)
            .unwrap();
    }
    harness.render().unwrap();

    let screen_before_open = harness.screen_to_string();
    println!("Screen before opening second file:\n{screen_before_open}");

    // Open the selected file from file explorer with Alt+Enter
    let result = harness.send_key(KeyCode::Enter, KeyModifiers::ALT);
    assert!(result.is_ok(), "Failed to send Alt+Enter: {result:?}");

    harness.sleep(std::time::Duration::from_millis(50));
    harness.render().unwrap();

    let screen_after_open = harness.screen_to_string();
    println!("Screen after opening second file:\n{screen_after_open}");

    // The critical assertion: the screen should now show the second file's content
    // NOT the first file's content
    assert!(
        screen_after_open.contains(content2),
        "Second file content should be visible on screen after opening from file explorer.\nScreen:\n{screen_after_open}"
    );

    assert!(
        !screen_after_open.contains(content1),
        "First file content should NOT be visible anymore after opening second file.\nScreen:\n{screen_after_open}"
    );
}

/// Test that file_explorer_toggle_hidden can be called (smoke test)
#[test]
fn test_file_explorer_toggle_hidden_smoke() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Call toggle_hidden - should not panic
    harness.editor_mut().file_explorer_toggle_hidden();
    harness.render().unwrap();

    // Call again to toggle back
    harness.editor_mut().file_explorer_toggle_hidden();
    harness.render().unwrap();

    // Test passes if no panic occurs
}

/// Test that file_explorer_toggle_gitignored can be called (smoke test)
#[test]
fn test_file_explorer_toggle_gitignored_smoke() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Call toggle_gitignored - should not panic
    harness.editor_mut().file_explorer_toggle_gitignored();
    harness.render().unwrap();

    // Call again to toggle back
    harness.editor_mut().file_explorer_toggle_gitignored();
    harness.render().unwrap();

    // Test passes if no panic occurs
}

/// Test that file_explorer_new_file can be called (smoke test)
#[test]
fn test_file_explorer_new_file_smoke() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Call new_file - should not panic (actual file creation depends on runtime)
    harness.editor_mut().file_explorer_new_file();
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // Test passes if no panic occurs
}

/// Test that file_explorer_new_directory can be called (smoke test)
#[test]
fn test_file_explorer_new_directory_smoke() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Call new_directory - should not panic (actual creation depends on runtime)
    harness.editor_mut().file_explorer_new_directory();
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // Test passes if no panic occurs
}

/// Test that file_explorer_delete can be called (smoke test)
#[test]
fn test_file_explorer_delete_smoke() {
    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file
    fs::write(project_root.join("test.txt"), "test").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is auto-expanded during init, wait for file to appear
    harness.wait_for_file_explorer_item("test.txt").unwrap();

    // Navigate to the file using Down key (user-facing action)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Call delete using Delete key (user-facing action)
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // Test passes if no panic occurs
}

/// Test that focus returns to file explorer after confirming file deletion
#[test]
fn test_file_explorer_focus_after_delete() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create multiple test files
    fs::write(project_root.join("file1.txt"), "content 1").unwrap();
    fs::write(project_root.join("file2.txt"), "content 2").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("file1.txt").unwrap();

    // Verify we're in file explorer context
    let key_context_before = harness.editor().get_key_context();
    println!("Key context before deletion: {:?}", key_context_before);
    assert!(
        matches!(
            key_context_before,
            fresh::input::keybindings::KeyContext::FileExplorer
        ),
        "Should be in FileExplorer context before deletion"
    );

    // Navigate to file1.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    println!("Screen before deletion:\n{}", screen_before);

    // Initiate deletion using the method directly (since Delete key isn't bound by default)
    harness.editor_mut().file_explorer_delete();
    harness.render().unwrap();

    let screen_prompt = harness.screen_to_string();
    println!("Screen with delete prompt:\n{}", screen_prompt);

    // Should see confirmation prompt
    assert!(
        screen_prompt.contains("Delete") || screen_prompt.contains("delete"),
        "Should show delete confirmation prompt. Screen:\n{}",
        screen_prompt
    );

    // Confirm deletion with 'y'
    harness.type_text("y").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("Screen after deletion:\n{}", screen_after);

    // Check that focus is back to file explorer
    let key_context_after = harness.editor().get_key_context();
    println!("Key context after deletion: {:?}", key_context_after);

    // The critical assertion: focus should be on file explorer after deletion
    assert!(
        matches!(
            key_context_after,
            fresh::input::keybindings::KeyContext::FileExplorer
        ),
        "Should be in FileExplorer context after deletion. Got: {:?}",
        key_context_after
    );

    // Verify file explorer is still visible
    assert!(
        screen_after.contains("File Explorer"),
        "File Explorer should still be visible after deletion"
    );

    // Verify the deleted file is gone from the file explorer tree
    // (but it may appear in status messages like "Moved to trash: file1.txt")
    // Check that the file explorer tree shows "1 item" (only file2.txt remains)
    assert!(
        screen_after.contains("1 item"),
        "File explorer should show only 1 item remaining after deletion"
    );

    // Verify arrow keys work in file explorer (not captured by editor)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // If we can navigate without error, the focus is correctly on file explorer
}

/// Test Feature 1: Enter key on directory toggles expand/collapse
#[test]
fn test_enter_toggles_directory() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a directory with files
    fs::create_dir(project_root.join("testdir")).unwrap();
    fs::write(project_root.join("testdir/file1.txt"), "content1").unwrap();
    fs::write(project_root.join("testdir/file2.txt"), "content2").unwrap();

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root should already be expanded (Feature 3), navigate to testdir
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    let screen_before_expand = harness.screen_to_string();
    println!("Before expand:\n{}", screen_before_expand);

    // Should be on testdir now - verify it's collapsed
    assert!(
        screen_before_expand.contains("> testdir") || screen_before_expand.contains(">  testdir"),
        "testdir should initially be collapsed"
    );

    // Press Enter to expand testdir
    harness
        .send_key(KeyCode::Enter, KeyModifiers::empty())
        .unwrap();
    harness.wait_for_file_explorer_item("file1.txt").unwrap();

    let screen_after_expand = harness.screen_to_string();
    println!("After expand:\n{}", screen_after_expand);

    // After expansion, should see the files inside testdir (file1.txt, file2.txt)
    assert!(
        screen_after_expand.contains("file1.txt") || screen_after_expand.contains("file2.txt"),
        "Should see files inside testdir after expansion"
    );

    // Verify testdir is now expanded
    assert!(
        screen_after_expand.contains("▼ testdir") || screen_after_expand.contains("▼  testdir"),
        "testdir should show expanded indicator (▼)"
    );

    // Press Enter again to collapse testdir
    harness
        .send_key(KeyCode::Enter, KeyModifiers::empty())
        .unwrap();
    // Wait for the directory to collapse (files should be gone)
    harness
        .wait_until(|h| !h.screen_to_string().contains("file1.txt"))
        .unwrap();

    let screen_after_collapse = harness.screen_to_string();

    println!("Screen after collapse:\n{}", screen_after_collapse);

    // After collapsing, directory tree structure should return to original state
    // We check that testdir shows collapsed indicator (>)
    assert!(
        screen_after_collapse.contains("> testdir") || screen_after_collapse.contains(">  testdir"),
        "testdir should be collapsed after pressing Enter again. Screen:\n{}",
        screen_after_collapse
    );

    // Verify files inside testdir are no longer visible
    assert!(
        !screen_after_collapse.contains("file1.txt")
            && !screen_after_collapse.contains("file2.txt"),
        "Files inside testdir should not be visible when collapsed"
    );
}

/// Test Feature 2: Enter key on file opens it and switches focus to editor
#[test]
fn test_enter_opens_file_and_switches_focus() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file with distinctive content
    let test_content = "Feature 2: Enter opens file and switches focus";
    fs::write(project_root.join("testfile.txt"), test_content).unwrap();

    // Open file explorer (should have focus)
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root directory should already be expanded (Feature 3)
    // Navigate down to the file (testfile.txt)
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    println!("Before opening file:\n{}", screen_before);

    // Verify we're on the test file
    // The selected item should be visible in the file explorer
    assert!(
        screen_before.contains("testfile.txt"),
        "testfile.txt should be visible in file explorer"
    );

    // File explorer should be visible and have focus
    assert!(
        screen_before.contains("File Explorer"),
        "File explorer should be visible"
    );

    // Press Enter on the file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::empty())
        .unwrap();
    harness.wait_for_screen_contains(test_content).unwrap();

    let screen_after = harness.screen_to_string();

    // File content should be visible in the editor
    assert!(
        screen_after.contains(test_content),
        "File content should be displayed in editor after pressing Enter"
    );

    // Verify focus switched to editor by checking that arrow keys now move cursor
    // in the editor (not file explorer). We can test this by sending a Down key
    // and checking if editor content area changed (cursor moved)
    harness
        .send_key(KeyCode::Right, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    let screen_after_movement = harness.screen_to_string();

    // After moving right, the cursor column should have changed in the status bar
    // The screen should show cursor position changed
    assert_ne!(
        screen_after, screen_after_movement,
        "Arrow keys should move cursor in editor after opening file (focus should be on editor)"
    );
}

/// Test Feature 3: Project directory should be expanded when file explorer first opens
#[test]
#[ignore]
fn test_project_directory_expanded_on_open() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create some files in the project root
    fs::write(project_root.join("file1.txt"), "content1").unwrap();
    fs::write(project_root.join("file2.txt"), "content2").unwrap();
    fs::create_dir(project_root.join("subdir")).unwrap();

    // Open file explorer for the first time
    harness.editor_mut().focus_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Root directory should be expanded (show ▼ not >)
    assert!(
        screen.contains("▼"),
        "Root directory should be expanded on initial open"
    );

    // Should see files/directories under root
    assert!(
        screen.contains("file1.txt") || screen.contains("file2.txt") || screen.contains("subdir"),
        "Should see files and directories under root when initially opened"
    );

    // Verify we see multiple entries (more than just the root)
    // Count lines that contain file/directory names (have indentation and text)
    let entry_count = screen
        .lines()
        .filter(|l| {
            (l.contains("file1.txt")
                || l.contains("file2.txt")
                || l.contains("subdir")
                || l.contains("project_root"))
                && (l.contains("▼") || l.contains(">") || l.contains("  "))
        })
        .count();

    assert!(
        entry_count > 1,
        "Should see more than just the root directory (found {} entries)",
        entry_count
    );
}

/// Test Feature 4: No [D][T] indicators, only show indicators for unsaved changes
#[test]
fn test_unsaved_change_indicators() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file
    fs::write(project_root.join("test.txt"), "original content").unwrap();
    fs::write(project_root.join("test.rs"), "fn main() {}").unwrap();
    fs::create_dir(project_root.join("mydir")).unwrap();

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let screen_initial = harness.screen_to_string();

    // Should NOT see [D], [T], [F], [R], [P] etc. indicators anymore
    assert!(
        !screen_initial.contains("[D]")
            && !screen_initial.contains("[T]")
            && !screen_initial.contains("[F]")
            && !screen_initial.contains("[R]")
            && !screen_initial.contains("[P]"),
        "Should not show file type indicators like [D], [T], [F], [R], [P] in file explorer"
    );

    // Open a file and modify it without saving
    // Navigate past mydir and other items to test.txt
    // Press down multiple times to get to a file (not directory)
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::empty())
            .unwrap();
        harness.render().unwrap();
    }

    let screen_before_open = harness.screen_to_string();
    println!("Before opening file:\n{}", screen_before_open);

    // Open the selected file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::empty())
        .unwrap();
    // Wait for file content to load
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("original content") || s.contains("fn main")
        })
        .unwrap();

    let screen_after_open = harness.screen_to_string();
    println!("After opening file:\n{}", screen_after_open);

    // Verify we're actually in the editor with file content
    assert!(
        screen_after_open.contains("original content") || screen_after_open.contains("fn main"),
        "Should have opened a file and see its content"
    );

    // Now in editor - type something to make changes
    harness
        .send_key(KeyCode::Char('X'), KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    // Go back to file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let screen_with_unsaved = harness.screen_to_string();

    println!("Screen with unsaved changes:\n{}", screen_with_unsaved);
    println!(
        "File explorer visible: {}",
        harness.editor().file_explorer_visible()
    );

    // Should now see an unsaved change indicator (●) next to test.txt
    assert!(
        screen_with_unsaved.contains("●") || screen_with_unsaved.contains("*"),
        "Should show unsaved change indicator next to modified file. Screen:\n{}",
        screen_with_unsaved
    );

    // test.rs should not have an indicator
    // We can verify by checking the lines containing the filenames
    let test_txt_line = screen_with_unsaved
        .lines()
        .find(|l| l.contains("test.txt"))
        .unwrap_or("");
    let test_rs_line = screen_with_unsaved
        .lines()
        .find(|l| l.contains("test.rs"))
        .unwrap_or("");

    assert!(
        test_txt_line.contains("●") || test_txt_line.contains("*"),
        "test.txt should have unsaved indicator"
    );
    assert!(
        !test_rs_line.contains("●") && !test_rs_line.contains("*"),
        "test.rs should not have unsaved indicator"
    );
}

/// Test Feature 5: Cursor should reach top before scrolling up (like it does for down)
#[test]
fn test_scroll_allows_cursor_to_top() {
    let mut harness = EditorTestHarness::with_temp_project(120, 10).unwrap(); // Small height to force scrolling
    let project_root = harness.project_dir().unwrap();

    // Create many files to force scrolling (need more than viewport height)
    for i in 0..25 {
        fs::write(
            project_root.join(format!("file{:02}.txt", i)),
            format!("content {}", i),
        )
        .unwrap();
    }

    // Open file explorer and wait for it to be ready with files loaded
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    // Wait for files to be loaded (file00.txt should appear)
    harness
        .wait_until(|h| h.screen_to_string().contains("file00.txt"))
        .unwrap();

    let initial_screen = harness.screen_to_string();
    println!("Initial screen:\n{}", initial_screen);

    // Get the viewport height (number of visible rows in file explorer)
    // Terminal height is 10, minus menu bar (1), status bar (1), prompt line (1), tab bar (1) = 6 main area
    // File explorer has borders (1 top) and may share space, so content area is ~5 rows
    let viewport_height = 5;

    // Navigate down to the bottom of the list
    // This will cause the explorer to scroll down
    for _ in 0..25 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::empty())
            .unwrap();
        harness.render().unwrap();
    }

    let screen_at_bottom = harness.screen_to_string();
    println!("Screen at bottom (scrolled down):\n{}", screen_at_bottom);

    // Now we're at the bottom and the view has scrolled down.
    // The test: when we press Up, the cursor should move WITHIN the viewport
    // for (viewport_height - 1) times before the view scrolls.

    // Track which files are visible to detect scrolling
    let get_visible_files = |screen: &str| -> Vec<String> {
        screen
            .lines()
            .filter_map(|line| {
                // Look for lines with file names (fileXX.txt pattern)
                if line.contains("file") && line.contains(".txt") {
                    // Extract the file number
                    for word in line.split_whitespace() {
                        if word.starts_with("file") && word.ends_with(".txt") {
                            return Some(word.to_string());
                        }
                    }
                }
                None
            })
            .collect()
    };

    let initial_visible = get_visible_files(&screen_at_bottom);
    println!("Initially visible files: {:?}", initial_visible);

    // Press Up multiple times (less than viewport_height times)
    // The visible files should stay the same (no scrolling yet)
    for i in 0..(viewport_height - 1) {
        harness
            .send_key(KeyCode::Up, KeyModifiers::empty())
            .unwrap();
        harness.render().unwrap();

        let screen_after_up = harness.screen_to_string();
        let visible_after_up = get_visible_files(&screen_after_up);

        println!("\nAfter {} up presses:", i + 1);
        println!("Visible files: {:?}", visible_after_up);

        // Within the viewport, the same files should still be visible
        // (cursor is moving, but view isn't scrolling)
        assert_eq!(
            initial_visible, visible_after_up,
            "After {} up presses, viewport should not have scrolled yet (cursor should move within viewport first). Initial: {:?}, After: {:?}",
            i + 1,
            initial_visible,
            visible_after_up
        );
    }

    // Now press Up one more time - THIS should cause scrolling
    // because the cursor should now be at the top of the viewport
    harness
        .send_key(KeyCode::Up, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    let screen_after_scroll = harness.screen_to_string();
    let visible_after_scroll = get_visible_files(&screen_after_scroll);

    println!("\nAfter scrolling up:");
    println!("Visible files: {:?}", visible_after_scroll);

    // After this press, the view SHOULD have scrolled (different files visible)
    assert_ne!(
        initial_visible, visible_after_scroll,
        "After cursor reaches top of viewport, the next up should scroll the view. Initial: {:?}, After scroll: {:?}",
        initial_visible,
        visible_after_scroll
    );
}

/// Test Feature 6: Editor tabs should be above editor area only, not above file explorer
#[test]
fn test_tabs_above_editor_area_only() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create and open multiple files to have tabs
    fs::write(project_root.join("file1.txt"), "content1").unwrap();
    fs::write(project_root.join("file2.txt"), "content2").unwrap();

    // Open first file
    harness
        .editor_mut()
        .open_file(&project_root.join("file1.txt"))
        .unwrap();
    harness.render().unwrap();

    // Open second file
    harness
        .editor_mut()
        .open_file(&project_root.join("file2.txt"))
        .unwrap();
    harness.render().unwrap();

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with file explorer and tabs:\n{}", screen);

    // The tabs should be above the editor area, not spanning the full width
    // We can verify this by checking that the file explorer border and tabs
    // are on the same line or the tabs start after the file explorer width

    let lines: Vec<&str> = screen.lines().collect();

    // Find the line with File Explorer title
    let explorer_line_idx = lines.iter().position(|l| l.contains("File Explorer"));

    // Find lines with tab content (file1.txt, file2.txt)
    let tab_line_idx = lines
        .iter()
        .position(|l| l.contains("file1.txt") && l.contains("file2.txt"));

    if let (Some(explorer_idx), Some(tab_idx)) = (explorer_line_idx, tab_line_idx) {
        // Tabs and file explorer should be on the same line (line 0)
        // The tab line should contain both the explorer border and the tabs
        let tab_line = lines.get(tab_idx).unwrap_or(&"");

        println!(
            "Tab line index: {}, Explorer line index: {}",
            tab_idx, explorer_idx
        );
        println!("Tab line: '{}'", tab_line);

        // The critical check: tabs should be on the same line as file explorer header
        // This means they're only above the editor area, not spanning full width
        assert_eq!(
            tab_idx, explorer_idx,
            "Tabs and File Explorer should be on the same line (tabs above editor area only)"
        );

        // The line should contain both file explorer and tabs
        assert!(
            tab_line.contains("File Explorer") && tab_line.contains("file1.txt"),
            "Tab line should contain both file explorer and tab content"
        );
    } else {
        panic!("Could not find both file explorer and tabs in output");
    }
}

/// Test Feature 7: Auto-expand and select file on focus switch
#[test]
fn test_auto_select_file_on_focus_switch() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create files in nested directories
    fs::create_dir_all(project_root.join("src/components")).unwrap();
    fs::write(project_root.join("src/components/App.js"), "app content").unwrap();
    fs::write(project_root.join("src/index.js"), "index content").unwrap();
    fs::write(project_root.join("README.md"), "readme").unwrap();

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Open a deeply nested file
    harness
        .editor_mut()
        .open_file(&project_root.join("src/components/App.js"))
        .unwrap();
    harness.render().unwrap();

    // Switch focus to file explorer
    harness.editor_mut().focus_file_explorer();
    // Wait for file explorer to show App.js (auto-expand to currently edited file)
    harness.wait_for_file_explorer_item("App.js").unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after opening nested file:\n{}", screen);

    // The file explorer should have auto-expanded to show App.js
    // Check that src is expanded (▼ not >)
    let lines: Vec<&str> = screen.lines().collect();
    let src_line = lines.iter().find(|l| l.contains("src")).unwrap_or(&"");

    println!("src line: '{}'", src_line);

    assert!(
        src_line.contains("▼") || src_line.contains("▼  src"),
        "src directory should be expanded (▼). Line: {}",
        src_line
    );

    // Should see components directory in the tree
    assert!(
        screen.contains("components"),
        "Should see components directory in file explorer tree"
    );

    // Should see App.js in the file explorer tree (not just in tabs)
    let app_line = lines
        .iter()
        .find(|l| l.contains("App.js") && l.contains("│"))
        .unwrap_or(&"");
    assert!(
        !app_line.is_empty() && app_line.contains("│"),
        "App.js should be visible in file explorer tree. Found line: {}",
        app_line
    );

    // App.js should be selected (we can't easily verify selection visually,
    // but we can check it's visible which means path was expanded)
    // For a more robust test, we could check the internal state
    // but for e2e, visibility is a good proxy

    // Now open a different file and switch focus again
    harness.editor_mut().focus_editor();
    harness
        .editor_mut()
        .open_file(&project_root.join("README.md"))
        .unwrap();
    harness.render().unwrap();

    // Switch focus back to file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer_item("README.md").unwrap();

    let screen2 = harness.screen_to_string();
    println!("Screen after switching to README.md:\n{}", screen2);

    // Should now show README.md (which is at root level)
    assert!(
        screen2.contains("README.md"),
        "File explorer should show README.md after switching focus"
    );
}

/// Test bug: Explorer sync fails after hide -> tab switch -> show
#[test]
fn test_file_explorer_sync_after_hide_and_tab_switch() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create two files
    fs::write(project_root.join("file1.txt"), "content 1").unwrap();
    fs::write(project_root.join("file2.txt"), "content 2").unwrap();

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Open file1.txt
    harness
        .editor_mut()
        .open_file(&project_root.join("file1.txt"))
        .unwrap();
    harness.render().unwrap();

    // Open file2.txt (should auto-sync explorer to file2)
    harness
        .editor_mut()
        .open_file(&project_root.join("file2.txt"))
        .unwrap();
    harness.render().unwrap();

    // Close (hide) the file explorer
    harness.editor_mut().toggle_file_explorer();
    harness.render().unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Switch to file1.txt (while explorer is hidden)
    harness.editor_mut().prev_buffer();
    harness.render().unwrap();

    // Verify we're on file1.txt
    harness
        .wait_until(|h| h.screen_to_string().contains("file1.txt"))
        .unwrap();

    // Re-open the file explorer
    harness.editor_mut().toggle_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Wait for file explorer to sync to file1.txt
    harness
        .wait_until(|h| {
            h.editor()
                .file_explorer()
                .and_then(|e| e.get_selected_entry())
                .map(|e| e.name.as_str() == "file1.txt")
                .unwrap_or(false)
        })
        .unwrap();
}

/// Test that file explorer shows the keybinding for toggling it (or just the title if no binding)
#[test]
fn test_file_explorer_shows_keybinding_in_title() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    let screen = harness.screen_to_string();

    // File explorer should show in the UI
    // If ToggleFileExplorer has a keybinding, it should appear in parentheses
    // Otherwise, just "File Explorer" should appear
    assert!(
        screen.contains("File Explorer"),
        "File explorer title should be visible. Screen:\n{}",
        screen
    );

    // If a keybinding is shown, verify it's in the correct format
    if screen.contains("File Explorer (") {
        // Keybinding format should be correct (e.g., "Ctrl+E", "Ctrl+Shift+B", etc.)
        let has_valid_format = screen.contains("File Explorer (Ctrl+")
            || screen.contains("File Explorer (Alt+")
            || screen.contains("File Explorer (Shift+");
        assert!(
            has_valid_format,
            "File explorer keybinding should be in a valid format. Screen:\n{}",
            screen
        );
    }
}

/// Test that file explorer keybinding is shown when focused (or just title if no binding)
#[test]
fn test_file_explorer_keybinding_when_focused() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for file explorer to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("File Explorer")
        })
        .unwrap();

    // Focus the file explorer
    harness
        .send_key(KeyCode::Char('\\'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // File explorer should be visible when focused
    assert!(
        screen.contains("File Explorer"),
        "File explorer title should be visible when focused. Screen:\n{}",
        screen
    );
}

/// Test that the file explorer can be toggled and the title is present
#[test]
fn test_file_explorer_keybinding_matches_behavior() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for file explorer to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("File Explorer")
        })
        .unwrap();

    let screen_with_explorer = harness.screen_to_string();

    // File explorer should be visible
    assert!(
        screen_with_explorer.contains("File Explorer"),
        "File explorer title should be visible after toggling on"
    );

    // Toggle it off using the same key
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen_without_explorer = harness.screen_to_string();

    // Screen should change when toggling
    assert_ne!(
        screen_with_explorer, screen_without_explorer,
        "File explorer should toggle off when the keybinding is pressed again"
    );
}

/// Test that Ctrl+E toggles focus between file explorer and editor
/// When in editor context, Ctrl+E focuses the file explorer
/// When in file explorer context, Ctrl+E focuses the editor (keeps explorer open)
#[test]
fn test_ctrl_e_toggles_focus_between_explorer_and_editor() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file
    fs::write(project_root.join("test.txt"), "test content").unwrap();

    // Open the file so we have something in the editor
    harness
        .editor_mut()
        .open_file(&project_root.join("test.txt"))
        .unwrap();
    harness.render().unwrap();

    // Initially, file explorer should not be visible
    assert!(
        !harness.editor().file_explorer_visible(),
        "File explorer should not be visible initially"
    );

    // Press Ctrl+E to open and focus file explorer
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // File explorer should now be visible
    assert!(
        harness.editor().file_explorer_visible(),
        "File explorer should be visible after Ctrl+E"
    );

    let screen_explorer_focused = harness.screen_to_string();
    println!("Screen with explorer focused:\n{}", screen_explorer_focused);

    // Press Ctrl+E again - should switch focus back to editor but keep explorer open
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // File explorer should STILL be visible (not toggled off)
    assert!(
        harness.editor().file_explorer_visible(),
        "File explorer should remain visible after Ctrl+E from explorer context"
    );

    let screen_editor_focused = harness.screen_to_string();
    println!("Screen with editor focused:\n{}", screen_editor_focused);

    // Verify focus is on editor by typing and seeing it appear in the buffer
    harness.type_text("TYPED").unwrap();
    harness.render().unwrap();

    let screen_after_typing = harness.screen_to_string();
    assert!(
        screen_after_typing.contains("TYPED"),
        "Typing should work after Ctrl+E switches focus to editor. Screen:\n{}",
        screen_after_typing
    );

    // File explorer should still be visible
    assert!(
        screen_after_typing.contains("File Explorer"),
        "File explorer should still be visible after typing in editor"
    );
}

/// Test that clicking on empty area in file explorer then clicking on editor allows typing
/// Bug: When clicking on empty area in file explorer (below files), the key_context is set
/// to FileExplorer. When clicking back on the editor, the key_context was not being reset
/// to Normal, so typing would have "No binding found" and do nothing.
#[test]
fn test_click_empty_explorer_area_then_editor_allows_typing() {
    use tracing_subscriber::EnvFilter;

    // Initialize tracing
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a few files so there's empty space below them in the file explorer
    fs::write(project_root.join("file1.txt"), "content 1").unwrap();
    fs::write(project_root.join("file2.txt"), "content 2").unwrap();

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Verify file explorer is visible
    assert!(
        harness.editor().file_explorer_visible(),
        "File explorer should be visible"
    );

    let screen = harness.screen_to_string();
    println!("Screen with file explorer:\n{}", screen);

    // File explorer is on the left side (about 30% width = ~36 columns on 120-width terminal)
    // Click on an empty area below the files in the file explorer
    // The files are near the top, so clicking at row 20+ should be empty area
    let explorer_col = 15; // In the file explorer area
    let empty_row = 25; // Below where files would be displayed

    println!(
        "Clicking empty area in file explorer at ({}, {})",
        explorer_col, empty_row
    );
    harness.mouse_click(explorer_col, empty_row).unwrap();
    harness.render().unwrap();

    // Check key_context after file explorer click
    let key_context_after_explorer = harness.editor().get_key_context();
    println!(
        "Key context after explorer click: {:?}",
        key_context_after_explorer
    );

    // Now click on the editor area (right side of the screen)
    // With file explorer taking ~30% width, editor starts around column 40+
    let editor_col = 70; // Well into the editor area
    let editor_row = 10; // In the content area

    println!("Clicking editor area at ({}, {})", editor_col, editor_row);
    harness.mouse_click(editor_col, editor_row).unwrap();
    harness.render().unwrap();

    // Check key_context after editor click
    let key_context_after_editor = harness.editor().get_key_context();
    println!(
        "Key context after editor click: {:?}",
        key_context_after_editor
    );

    // Get the buffer content before typing
    let content_before = harness.get_buffer_content().unwrap_or_default();
    println!("Buffer content before typing: '{}'", content_before);

    // Now try to type something - this is the key test
    // If key_context is still FileExplorer, this will do nothing (bug)
    // If key_context was properly reset to Normal, this will insert text (fix)
    harness.type_text("TYPED_TEXT").unwrap();
    harness.render().unwrap();

    // Check that the text was actually inserted
    let content_after = harness.get_buffer_content().unwrap_or_default();
    println!("Buffer content after typing: '{}'", content_after);

    // The critical assertion: text should have been inserted
    assert!(
        content_after.contains("TYPED_TEXT"),
        "Typing should work after clicking on empty file explorer area then clicking on editor. \
         Bug: key_context stays as FileExplorer after clicking on empty area, preventing typing. \
         Content before: '{}', Content after: '{}'",
        content_before,
        content_after
    );
}

/// Test that closing the last buffer focuses the file explorer
#[test]
fn test_close_last_buffer_focuses_file_explorer() {
    // Use a fresh editor with no files open initially
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.render().unwrap();

    // The editor starts with a single scratch buffer [No Name]
    let screen_initial = harness.screen_to_string();
    println!("Initial screen:\n{}", screen_initial);

    // File explorer should not be visible initially
    assert!(
        !harness.editor().file_explorer_visible(),
        "File explorer should not be visible initially"
    );

    // Close the scratch buffer (Alt+W)
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();

    // Wait for file explorer to be visible AND rendered on screen
    harness
        .wait_until(|h| {
            h.editor().file_explorer_visible() && h.screen_to_string().contains("File Explorer")
        })
        .unwrap();

    let screen_after_close = harness.screen_to_string();
    println!("Screen after closing last buffer:\n{}", screen_after_close);

    // File explorer should now be visible
    assert!(
        harness.editor().file_explorer_visible(),
        "File explorer should be visible after closing last buffer"
    );

    // Should see File Explorer in the screen
    assert!(
        screen_after_close.contains("File Explorer"),
        "File Explorer should be visible on screen after closing last buffer. Screen:\n{}",
        screen_after_close
    );
}

/// Test that folders containing modified files show a modified indicator (●)
/// This tests Issue #526: Show changed file on folder
#[test]
fn test_folder_shows_modified_indicator_for_unsaved_file() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a folder with a file inside
    fs::create_dir(project_root.join("src")).unwrap();
    fs::write(project_root.join("src/main.rs"), "fn main() {}").unwrap();
    // Also create a file at root level for comparison
    fs::write(project_root.join("README.md"), "readme").unwrap();

    // Open file explorer first to verify initial state
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let screen_initial = harness.screen_to_string();
    println!("Initial screen:\n{}", screen_initial);

    // The src folder should NOT have a modified indicator initially
    // Look for the folder line - it should have "> src" or "▼ src" but NOT "●"
    let src_line_initial = screen_initial
        .lines()
        .find(|l| l.contains("src") && (l.contains(">") || l.contains("▼")))
        .unwrap_or("");
    println!("Initial src line: '{}'", src_line_initial);

    // Expand the src folder to see main.rs
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_file_explorer_item("main.rs").unwrap();

    // Navigate to main.rs and open it
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("fn main").unwrap();

    // Now modify the file without saving
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" // modified").unwrap();
    harness.render().unwrap();

    // Go back to file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let screen_after_modify = harness.screen_to_string();
    println!("Screen after modification:\n{}", screen_after_modify);

    // Now the src folder SHOULD have a modified indicator (●)
    // The line with "src" should contain "●"
    let src_line_after = screen_after_modify
        .lines()
        .find(|l| l.contains("src") && (l.contains(">") || l.contains("▼")))
        .unwrap_or("");
    println!("After modify src line: '{}'", src_line_after);

    assert!(
        src_line_after.contains("●"),
        "src folder should show modified indicator (●) when it contains a modified file. Line: '{}'",
        src_line_after
    );

    // The main.rs file itself should also have a modified indicator
    // Look for main.rs within the file explorer (lines containing "│" border)
    let main_rs_line = screen_after_modify
        .lines()
        .find(|l| l.contains("main.rs") && l.contains("│"))
        .unwrap_or("");
    // If main.rs is visible in the explorer, it should have the indicator
    // Note: main.rs might be inside the collapsed src folder, so we check if it's visible
    if !main_rs_line.is_empty() {
        assert!(
            main_rs_line.contains("●"),
            "main.rs should show modified indicator (●) in file explorer. Line: '{}'",
            main_rs_line
        );
    }

    // README.md should NOT have a modified indicator (it wasn't modified)
    // Look for README.md within the file explorer (lines containing "│" border)
    let readme_line = screen_after_modify
        .lines()
        .find(|l| l.contains("README.md") && l.contains("│"))
        .unwrap_or("");
    if !readme_line.is_empty() {
        assert!(
            !readme_line.contains("●"),
            "README.md should NOT show modified indicator. Line: '{}'",
            readme_line
        );
    }
}

/// Test that nested folder hierarchy shows modified indicator up the tree
#[test]
fn test_nested_folder_shows_modified_indicator() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a nested folder structure: src/components/Button.js
    fs::create_dir_all(project_root.join("src/components")).unwrap();
    fs::write(
        project_root.join("src/components/Button.js"),
        "export default Button;",
    )
    .unwrap();

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Expand src folder
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_file_explorer_item("components").unwrap();

    // Expand components folder
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_file_explorer_item("Button.js").unwrap();

    // Open Button.js
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("export default").unwrap();

    // Modify the file
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" // changed").unwrap();
    harness.render().unwrap();

    // Go back to file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with nested modification:\n{}", screen);

    // Both src and components folders should show modified indicator
    // Look for lines within the file explorer area (containing "│" border)
    let src_line = screen
        .lines()
        .find(|l| l.contains("src") && !l.contains("components") && l.contains("│"))
        .unwrap_or("");
    let components_line = screen
        .lines()
        .find(|l| l.contains("components") && l.contains("│"))
        .unwrap_or("");
    let button_line = screen
        .lines()
        .find(|l| l.contains("Button.js") && l.contains("│"))
        .unwrap_or("");

    println!("src line: '{}'", src_line);
    println!("components line: '{}'", components_line);
    println!("Button.js line: '{}'", button_line);

    assert!(
        src_line.contains("●"),
        "src folder should show modified indicator (●) for nested modified file. Line: '{}'",
        src_line
    );
    assert!(
        components_line.contains("●"),
        "components folder should show modified indicator (●). Line: '{}'",
        components_line
    );
    // Button.js might be inside the collapsed components folder, so check if visible
    if !button_line.is_empty() {
        assert!(
            button_line.contains("●"),
            "Button.js should show modified indicator (●). Line: '{}'",
            button_line
        );
    }
}

/// Test that folder modified indicator is cleared after saving
#[test]
fn test_folder_modified_indicator_cleared_after_save() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a folder with a file
    fs::create_dir(project_root.join("mydir")).unwrap();
    fs::write(project_root.join("mydir/test.txt"), "original").unwrap();

    // Open file explorer and navigate to the file
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Expand mydir
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_file_explorer_item("test.txt").unwrap();

    // Open test.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("original").unwrap();

    // Modify the file
    harness.type_text("modified ").unwrap();
    harness.render().unwrap();

    // Go to file explorer and verify modified indicator
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let screen_before_save = harness.screen_to_string();
    // Look for mydir within the file explorer area (containing "│" border)
    let mydir_line_before = screen_before_save
        .lines()
        .find(|l| l.contains("mydir") && l.contains("│"))
        .unwrap_or("");
    assert!(
        mydir_line_before.contains("●"),
        "mydir should show modified indicator before save. Line: '{}'",
        mydir_line_before
    );

    // Focus editor and save the file
    harness.editor_mut().focus_editor();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Wait a bit for save to complete
    harness.sleep(std::time::Duration::from_millis(100));

    // Go back to file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let screen_after_save = harness.screen_to_string();
    println!("Screen after save:\n{}", screen_after_save);

    // Look for mydir within the file explorer area (containing "│" border)
    let mydir_line_after = screen_after_save
        .lines()
        .find(|l| l.contains("mydir") && l.contains("│"))
        .unwrap_or("");
    println!("mydir line after save: '{}'", mydir_line_after);

    // The modified indicator should be gone after saving
    // Note: The folder line should have either "> " or "▼ " but not "●"
    // We check that there's no "●" next to the folder name
    let has_modified_indicator = mydir_line_after.contains("●");
    assert!(
        !has_modified_indicator,
        "mydir should NOT show modified indicator after save. Line: '{}'",
        mydir_line_after
    );
}

/// Test that creating a new file from file explorer opens a rename prompt
/// and opens the file in the buffer
/// Tests:
/// 1. Prompt starts empty (not showing the randomly generated name)
/// 2. After renaming, focus switches to the editor buffer
/// 3. Buffer tab name is synced with the renamed file name
#[test]
fn test_file_explorer_new_file_opens_rename_prompt_and_buffer() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let screen_before = harness.screen_to_string();
    println!("Screen before new file:\n{}", screen_before);

    // Create new file using the 'n' key
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("Screen after new file:\n{}", screen_after);

    // A rename prompt should be visible (asking for the new file name)
    assert!(
        screen_after.contains("Rename to:"),
        "A rename prompt should appear after creating new file. Screen:\n{}",
        screen_after
    );

    // The prompt should start EMPTY (not showing the generated filename)
    // The generated filename (untitled_XXX.txt) should NOT appear in the prompt input area
    // Note: the file IS created on disk with the generated name, but the prompt is empty
    // so user can type the desired name from scratch
    let prompt_line = screen_after
        .lines()
        .find(|l| l.contains("Rename to:"))
        .unwrap_or("");
    println!("Prompt line: '{}'", prompt_line);

    // Verify prompt is empty (just "Rename to:" without any filename after)
    // The prompt line should end with "Rename to:" followed by only whitespace
    assert!(
        prompt_line.trim() == "Rename to:" || prompt_line.trim().ends_with("Rename to:"),
        "Prompt should start empty (no pre-filled filename). Got: '{}'",
        prompt_line
    );

    // Type the new name (prompt starts empty, so just type directly)
    harness.type_text("my_new_file.rs").unwrap();
    harness.render().unwrap();

    let screen_typing = harness.screen_to_string();
    println!("Screen while typing:\n{}", screen_typing);

    // Confirm the rename
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let screen_final = harness.screen_to_string();
    println!("Screen after rename:\n{}", screen_final);

    // Verify 1: The file should be open in the buffer (shown in tabs)
    assert!(
        screen_final.contains("my_new_file.rs"),
        "The new file should be visible in tabs after renaming. Screen:\n{}",
        screen_final
    );

    // Verify 2: Focus should be on the editor (Normal key context), not file explorer
    let key_context = harness.editor().get_key_context();
    assert!(
        matches!(key_context, fresh::input::keybindings::KeyContext::Normal),
        "Focus should be on editor (Normal context) after rename. Got: {:?}",
        key_context
    );

    // Verify 3: The file should exist on disk with the new name
    assert!(
        project_root.join("my_new_file.rs").exists(),
        "The renamed file should exist on disk"
    );

    // Verify 4: Typing should work in the buffer (proves focus is on editor)
    harness.type_text("fn main() {}").unwrap();
    harness.render().unwrap();

    let buffer_content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        buffer_content.contains("fn main() {}"),
        "Should be able to type in the buffer after rename. Content: '{}'",
        buffer_content
    );

    // Verify 5: The old generated filename should NOT exist on disk
    // (it was renamed to my_new_file.rs)
    let untitled_files: Vec<_> = std::fs::read_dir(&project_root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_name().to_string_lossy().starts_with("untitled_"))
        .collect();
    assert!(
        untitled_files.is_empty(),
        "The old generated filename should not exist on disk. Found: {:?}",
        untitled_files
    );
}
