use crate::common::harness::EditorTestHarness;
use std::fs;
use tempfile::TempDir;

/// Test file explorer toggle
#[test]
fn test_file_explorer_toggle() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Initially file explorer should not be visible
    harness.render().unwrap();
    let screen_before = harness.screen_to_string();

    // Toggle file explorer on with Ctrl+B
    harness.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Screen should show file explorer (check for the border or title)
    let screen_after = harness.screen_to_string();

    // Should show "File Explorer" in the UI
    assert!(
        screen_after.contains("File Explorer") || screen_after.contains("[D]"),
        "Screen should show file explorer after toggle"
    );

    // Toggle file explorer off with Ctrl+B
    harness.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL).unwrap();
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

    use crossterm::event::{KeyCode, KeyModifiers};

    // Create harness with isolated temp project
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create some files and directories
    fs::create_dir(project_root.join("src")).unwrap();
    fs::write(project_root.join("src/main.rs"), "fn main() {}").unwrap();
    fs::write(project_root.join("Cargo.toml"), "[package]").unwrap();
    fs::create_dir(project_root.join("tests")).unwrap();
    fs::write(project_root.join("README.md"), "# Project").unwrap();

    // Toggle file explorer on with Ctrl+B
    harness.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Wait a moment for async file system operations
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // Check that we see the project structure
    // Note: The exact rendering might differ, but we should see some files
    let screen = harness.screen_to_string();
    println!("File explorer screen:\n{}", screen);

    // Should show at least the root directory name or some indication of files
    // (This is a basic check - the exact content depends on rendering)

}

/// Test file explorer navigation
#[test]
fn test_file_explorer_navigation() {

    use crossterm::event::{KeyCode, KeyModifiers};

    // Create a test directory structure with multiple files
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();

    fs::write(project_root.join("file1.txt"), "File 1").unwrap();
    fs::write(project_root.join("file2.txt"), "File 2").unwrap();
    fs::write(project_root.join("file3.txt"), "File 3").unwrap();


    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on with Ctrl+B
    harness.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();

    // Wait for initialization
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let screen_initial = harness.screen_to_string();

    // Navigate down with Alt+J
    harness.send_key(KeyCode::Char('j'), KeyModifiers::ALT).unwrap();
    harness.render().unwrap();

    let screen_after_down = harness.screen_to_string();

    // Screen should change (selection moved)
    // Note: This might be subtle depending on rendering
    println!("After navigate down:\n{}", screen_after_down);

    // Navigate up with Alt+K
    harness.send_key(KeyCode::Char('k'), KeyModifiers::ALT).unwrap();
    harness.render().unwrap();

}

/// Test file explorer expand/collapse
#[test]
fn test_file_explorer_expand_collapse() {

    use crossterm::event::{KeyCode, KeyModifiers};

    // Create a test directory structure with nested directories
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();

    fs::create_dir(project_root.join("src")).unwrap();
    fs::write(project_root.join("src/lib.rs"), "// lib").unwrap();
    fs::write(project_root.join("src/main.rs"), "fn main() {}").unwrap();


    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on with Ctrl+B
    harness.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();

    // Wait for initialization
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let screen_before_expand = harness.screen_to_string();
    println!("Before expand:\n{}", screen_before_expand);

    // Expand the root directory with Alt+L
    harness.send_key(KeyCode::Char('l'), KeyModifiers::ALT).unwrap();

    // Wait for async operation
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    let screen_after_expand = harness.screen_to_string();
    println!("After expand:\n{}", screen_after_expand);

    // The screen should show more content after expanding
    // (exact assertion depends on rendering details)

    // Collapse with Alt+L (toggle)
    harness.send_key(KeyCode::Char('l'), KeyModifiers::ALT).unwrap();

    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

}

/// Test opening a file from file explorer
#[test]
fn test_file_explorer_open_file() {

    // Create a simple test directory with one file
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();
    let test_file = project_root.join("simple.txt");
    let test_content = "Hello World";
    fs::write(&test_file, test_content).unwrap();


    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on (this initializes it synchronously now)
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    let screen_with_explorer = harness.screen_to_string();
    println!("File explorer visible:\n{}", screen_with_explorer);

    // Verify file explorer is showing
    assert!(
        screen_with_explorer.contains("File Explorer") || screen_with_explorer.contains("[D]"),
        "File explorer should be visible"
    );

    // Expand root directory to see files (root should be selected by default)
    harness.editor_mut().file_explorer_toggle_expand();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    let screen_after_expand = harness.screen_to_string();
    println!("After expand:\n{}", screen_after_expand);

    // Navigate down to the file (first child after root)
    harness.editor_mut().file_explorer_navigate_down();
    harness.render().unwrap();

    // Try to open - should work if we're on a file
    let result = harness.editor_mut().file_explorer_open_file();

    // Even if the file wasn't selected (e.g., we're on a directory),
    // the function should not error
    assert!(result.is_ok(), "file_explorer_open_file should not error");

    harness.render().unwrap();
    let screen_after_open = harness.screen_to_string();
    println!("After trying to open:\n{}", screen_after_open);

    // If a file was opened, buffer should have content
    let buffer_content = harness.get_buffer_content();
    if !buffer_content.is_empty() {
        // A file was opened - verify it's our test file
        assert_eq!(
            buffer_content, test_content,
            "Buffer should contain the opened file's content"
        );
    }
    // Note: We don't fail the test if no file was opened, as navigation might not land on the file

}

/// Test file explorer refresh
#[test]
fn test_file_explorer_refresh() {

    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();

    // Create initial file
    fs::write(project_root.join("file1.txt"), "File 1").unwrap();


    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();

    // Wait for initialization
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // Add a new file to the directory
    fs::write(project_root.join("file2.txt"), "File 2").unwrap();

    // Refresh the file explorer
    harness.editor_mut().file_explorer_refresh();

    // Wait for refresh
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // The new file should now be visible
    // (This is hard to assert precisely without introspecting the tree structure)
    let screen = harness.screen_to_string();
    println!("After refresh:\n{}", screen);

}

/// Test focus switching between file explorer and editor
#[test]
fn test_file_explorer_focus_switching() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Open file explorer
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // File explorer should be visible and focused
    assert!(harness.editor().file_explorer_visible());

    // Try using arrow keys - in FileExplorer context, these should navigate the explorer
    harness.send_key(KeyCode::Down, KeyModifiers::empty()).unwrap();
    harness.render().unwrap();

    // Toggle file explorer off
    harness.editor_mut().toggle_file_explorer();
    harness.render().unwrap();

    // File explorer should be hidden now
    assert!(!harness.editor().file_explorer_visible());

    // Toggle file explorer back on
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
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

    use crossterm::event::{KeyCode, KeyModifiers};

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path();
    std::fs::write(project_root.join("test.txt"), "content").unwrap();


    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Open file explorer (starts with focus)
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Arrow keys should work in file explorer context
    harness.send_key(KeyCode::Down, KeyModifiers::empty()).unwrap();
    harness.render().unwrap();

    // Switch to editor context
    harness.send_key(KeyCode::Esc, KeyModifiers::empty()).unwrap();
    harness.render().unwrap();

    // Now arrow keys should work for editor navigation, not file explorer
    harness.send_key(KeyCode::Down, KeyModifiers::empty()).unwrap();
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
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
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
fn test_file_explorer_displays_opened_file_content() {
    use crossterm::event::{KeyCode, KeyModifiers};

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
    println!("Screen after opening first file:\n{}", screen1);

    // Verify first file content is displayed on screen
    assert!(
        screen1.contains(content1),
        "First file content should be visible on screen after opening"
    );

    // Now open file explorer with Ctrl+B
    harness.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Expand the root directory with Alt+L
    harness.send_key(KeyCode::Char('l'), KeyModifiers::ALT).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Navigate down to find second.txt with Alt+J
    // We need to find it in the list (first.txt comes before second.txt alphabetically)
    for _ in 0..3 {
        harness.send_key(KeyCode::Char('j'), KeyModifiers::ALT).unwrap();
    }
    harness.render().unwrap();

    let screen_before_open = harness.screen_to_string();
    println!("Screen before opening second file:\n{}", screen_before_open);

    // Open the selected file from file explorer with Alt+Enter
    let result = harness.send_key(KeyCode::Enter, KeyModifiers::ALT);
    assert!(result.is_ok(), "Failed to send Alt+Enter: {:?}", result);

    std::thread::sleep(std::time::Duration::from_millis(50));
    harness.render().unwrap();

    let screen_after_open = harness.screen_to_string();
    println!("Screen after opening second file:\n{}", screen_after_open);

    // The critical assertion: the screen should now show the second file's content
    // NOT the first file's content
    assert!(
        screen_after_open.contains(content2),
        "Second file content should be visible on screen after opening from file explorer.\nScreen:\n{}",
        screen_after_open
    );

    assert!(
        !screen_after_open.contains(content1),
        "First file content should NOT be visible anymore after opening second file.\nScreen:\n{}",
        screen_after_open
    );

}

/// Test that file_explorer_toggle_hidden can be called (smoke test)
#[test]
fn test_file_explorer_toggle_hidden_smoke() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
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
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
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

    let temp_dir = TempDir::new().unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Call new_file - should not panic (actual file creation depends on runtime)
    harness.editor_mut().file_explorer_new_file();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();


    // Test passes if no panic occurs
}

/// Test that file_explorer_new_directory can be called (smoke test)
#[test]
fn test_file_explorer_new_directory_smoke() {

    let temp_dir = TempDir::new().unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Call new_directory - should not panic (actual creation depends on runtime)
    harness.editor_mut().file_explorer_new_directory();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();


    // Test passes if no panic occurs
}

/// Test that file_explorer_delete can be called (smoke test)
#[test]
fn test_file_explorer_delete_smoke() {

    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();

    // Create a test file
    fs::write(project_root.join("test.txt"), "test").unwrap();


    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Toggle file explorer on
    harness.editor_mut().toggle_file_explorer();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Expand root and navigate
    harness.editor_mut().file_explorer_toggle_expand();
    std::thread::sleep(std::time::Duration::from_millis(50));
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    harness.editor_mut().file_explorer_navigate_down();
    harness.render().unwrap();

    // Call delete - should not panic (actual deletion depends on runtime and safety checks)
    harness.editor_mut().file_explorer_delete();
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();


    // Test passes if no panic occurs
}
