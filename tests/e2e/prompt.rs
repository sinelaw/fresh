use crate::common::harness::EditorTestHarness;

/// Test that the prompt is rendered correctly
#[test]
fn test_prompt_rendering() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that the prompt is visible in the status bar area (bottom line)
    let _screen = harness.screen_to_string();
    harness.assert_screen_contains("Open:");

    // Check the prompt styling
    let buffer = harness.buffer();
    let status_y = buffer.area.height - 1; // Status bar is at the bottom

    // Check a cell in the status bar has the high-contrast theme's prompt background
    // (default theme is high-contrast, which uses Rgb(10, 10, 10) for prompt_bg)
    let first_cell_pos = buffer.index_of(0, status_y);
    let first_cell = &buffer.content[first_cell_pos];
    assert_eq!(
        first_cell.bg,
        ratatui::style::Color::Rgb(10, 10, 10),
        "Prompt should have high-contrast theme prompt background"
    );
}

/// Test prompt input handling (typing, backspace, cursor movement)
#[test]
fn test_prompt_input_handling() {
    use crossterm::event::{KeyCode, KeyModifiers};
    // Use harness with temp project so file paths are relative
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Trigger the open file prompt with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Open:");

    // Type some text
    harness.type_text("test.txt").unwrap();
    harness.assert_screen_contains("test.txt");

    // Test backspace
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("test.tx");
    harness.assert_screen_not_contains("test.txt");

    // Type more
    harness.type_text("t2").unwrap();
    harness.assert_screen_contains("test.txt2");

    // Test Home (move cursor to start)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Type at the beginning
    harness.type_text("my_").unwrap();
    harness.assert_screen_contains("my_test.txt2");

    // Test End (move cursor to end)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("!").unwrap();
    harness.assert_screen_contains("my_test.txt2!");
}

/// Test canceling the prompt with Escape
#[test]
fn test_prompt_cancel() {
    use crossterm::event::{KeyCode, KeyModifiers};
    // Use harness with temp project so file paths are relative
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Trigger the open file prompt
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Open:");

    // Type some text (relative path)
    harness.type_text("test.txt").unwrap();
    harness.assert_screen_contains("test.txt");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Prompt should be gone, and "Canceled" message should appear
    harness.assert_screen_not_contains("Open:");
    harness.assert_screen_contains("Canceled");
}

/// Test the complete open file workflow
#[test]
fn test_open_file_workflow() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    use tempfile::TempDir;

    // Create a temporary directory and file
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test_prompt.txt");
    fs::write(&file_path, "Hello from prompt test!").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the open file prompt
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Open:");

    // Type the file path
    let path_str = file_path.to_str().unwrap();
    harness.type_text(path_str).unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check that the file was opened
    harness.assert_screen_not_contains("Open:");

    // Check that the file content is displayed
    // Note: File content display may require additional renders after async file load
    harness.assert_screen_contains("Hello from prompt test!");

    // Check that the filename appears in the status bar
    harness.assert_screen_contains("test_prompt.txt");
}

/// Test opening a non-existent file creates an unsaved buffer
#[test]
fn test_open_nonexistent_file() {
    // Use harness with temp project so file paths are relative
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let new_file_path = project_dir.join("new_file.txt");

    // Open non-existent file directly (via open_file, not file picker)
    harness.open_file(&new_file_path).unwrap();

    // Should NOT show an error - should open as unsaved buffer
    harness.assert_screen_not_contains("Error opening file");

    // Should show the filename in the tab/status bar
    harness.assert_screen_contains("new_file.txt");

    // Buffer should be empty
    assert_eq!(harness.get_buffer_content().unwrap(), "");

    // Should show "Opened" message
    harness.assert_screen_contains("Opened");
}

/// Test that opening a non-existent file allows editing and saving
#[test]
fn test_open_nonexistent_file_edit_and_save() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    // Use harness with temp project so file paths are relative
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let new_file_path = project_dir.join("created_file.txt");

    // Verify file doesn't exist yet
    assert!(!new_file_path.exists());

    // Open non-existent file directly (via open_file, not file picker)
    harness.open_file(&new_file_path).unwrap();

    // Should open successfully
    harness.assert_screen_not_contains("Error");

    // Type some content
    harness.type_text("Hello, World!").unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "Hello, World!");

    // Save the file with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show "Saved" message
    harness.assert_screen_contains("Saved");

    // Verify file was created on disk with correct content
    assert!(new_file_path.exists());
    let saved_content = fs::read_to_string(&new_file_path).unwrap();
    assert_eq!(saved_content, "Hello, World!");
}

/// Test spawning CLI with non-existent file directly (via open_file)
#[test]
#[cfg_attr(windows, ignore)] // File content is corrupted with terminal output on Windows
fn test_spawn_with_nonexistent_file() {
    use std::fs;
    use tempfile::TempDir;

    // Create a temporary directory
    let temp_dir = TempDir::new().unwrap();
    let new_file_path = temp_dir.path().join("spawn_test.rs");

    // Verify file doesn't exist
    assert!(!new_file_path.exists());

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open non-existent file directly (simulating CLI spawn)
    harness.open_file(&new_file_path).unwrap();

    // Should show the filename
    harness.assert_screen_contains("spawn_test.rs");

    // Buffer should be empty
    assert_eq!(harness.get_buffer_content().unwrap(), "");

    // Type content with trailing newline and save
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.type_text("fn main() {}").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify file was created
    assert!(new_file_path.exists());
    let content = fs::read_to_string(&new_file_path).unwrap();
    assert_eq!(content, "fn main() {}\n");
}

/// Test Save As functionality
#[test]
fn test_save_as_functionality() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;
    use tempfile::TempDir;

    // Create a temporary directory and original file
    let temp_dir = TempDir::new().unwrap();
    let original_path = temp_dir.path().join("original.txt");
    fs::write(&original_path, "Original content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the original file
    harness.open_file(&original_path).unwrap();
    harness.assert_screen_contains("original.txt");
    assert_eq!(harness.get_buffer_content().unwrap(), "Original content");

    // Trigger command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to search for Save As command
    harness.type_text("Save File As").unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for the Save As prompt to appear
    harness.wait_for_screen_contains("Save as:").unwrap();

    // Clear the current filename and type new name
    // First select all with Ctrl+A
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();

    // Type new filename (relative path)
    let new_file_path = temp_dir.path().join("saved_as.txt");
    let new_path_str = new_file_path.to_str().unwrap();
    harness.type_text(new_path_str).unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Note: "Saved as:" status message may be overwritten by auto-revert status
    // We verify the save succeeded by checking the file exists and has correct content below

    // Verify new file was created with correct content
    assert!(new_file_path.exists());
    let new_content = fs::read_to_string(&new_file_path).unwrap();
    assert_eq!(new_content, "Original content");

    // Original file should still exist
    assert!(original_path.exists());

    // Buffer should now show the new filename
    harness.assert_screen_contains("saved_as.txt");
}

/// Test Save As with relative path
#[test]
#[ignore] // Flaky test - ignore for now
fn test_save_as_relative_path() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    eprintln!("[TEST] Starting test_save_as_relative_path");

    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    eprintln!("[TEST] Project dir: {:?}", project_dir);

    // Create and open original file
    let original_path = project_dir.join("original.txt");
    fs::write(&original_path, "Test content").unwrap();
    eprintln!("[TEST] Opening file: {:?}", original_path);
    harness.open_file(&original_path).unwrap();

    eprintln!("[TEST] Opening command palette");
    // Trigger command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    eprintln!("[TEST] Typing 'Save File As'");
    harness.type_text("Save File As").unwrap();

    eprintln!("[TEST] Waiting for 'Save File As' to appear in palette");
    // Wait for command to appear in palette before executing
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            let found = screen.contains("Save File As");
            if !found {
                eprintln!(
                    "[TEST] Still waiting for 'Save File As' in palette. Screen:\n{}",
                    screen
                );
            }
            found
        })
        .unwrap();

    eprintln!("[TEST] Pressing Enter to execute command");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    eprintln!("[TEST] Waiting for 'Save as:' prompt");
    // Wait for the Save As prompt to appear
    harness.wait_for_screen_contains("Save as:").unwrap();

    eprintln!("[TEST] Clearing and typing relative path");
    // Clear the prompt field by selecting all and typing new text
    // Send Ctrl+A to select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for Ctrl+A to take effect (semantic waiting)
    // The prompt should process the selection before we type
    harness.process_async_and_render().unwrap();

    harness.type_text("relative_save.txt").unwrap();

    eprintln!("[TEST] Pressing Enter to confirm save");
    // Confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should save to working directory
    let expected_path = project_dir.join("relative_save.txt");
    eprintln!(
        "[TEST] Waiting for file to be created at: {:?}",
        expected_path
    );

    // Wait for file to be created AND readable
    // Check both existence and readability to handle filesystem caching issues
    harness
        .wait_until(|_| {
            let exists = expected_path.exists();
            let readable = exists && fs::read_to_string(&expected_path).is_ok();
            if !readable {
                eprintln!("[TEST] File not yet created/readable. Exists: {}", exists);
            }
            readable
        })
        .expect(&format!("File should be created at {:?}", expected_path));

    eprintln!("[TEST] File created successfully, verifying content");
    let content = fs::read_to_string(&expected_path).unwrap();
    assert_eq!(content, "Test content");
    eprintln!("[TEST] Test completed successfully");
}

/// Test Save As creates parent directories if needed
#[test]
fn test_save_as_nested_path() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Start with new buffer
    harness.new_buffer().unwrap();

    // Type some content
    harness.type_text("Nested file content").unwrap();

    // Trigger Save As via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Save File As").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for the Save As prompt to appear
    harness.wait_for_screen_contains("Save as:").unwrap();

    // Type nested path (parent dir doesn't exist yet)
    let nested_path = project_dir.join("subdir").join("nested.txt");
    let nested_path_str = nested_path.to_str().unwrap();
    harness.type_text(nested_path_str).unwrap();

    // Confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Note: This test verifies the error case since we don't auto-create parent dirs
    // The file won't be created because the parent directory doesn't exist
    // This documents current behavior - if we want to auto-create dirs, update this test
    if !nested_path.exists() {
        harness.assert_screen_contains("Error saving file");
    }
}

/// Test Save As prompts for confirmation when overwriting an existing file
#[test]
fn test_save_as_overwrite_confirmation() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    // Use with_temp_project to get a working directory with short relative paths
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create two files in the project directory
    let original_path = project_dir.join("original.txt");
    let existing_path = project_dir.join("existing.txt");
    fs::write(&original_path, "Original content").unwrap();
    fs::write(&existing_path, "Existing content").unwrap();

    // Open the original file
    harness.open_file(&original_path).unwrap();
    harness.assert_screen_contains("original.txt");

    // Trigger Save As via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Save File As").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for the Save As prompt to appear
    harness.wait_for_screen_contains("Save as:").unwrap();

    // Clear and type just the relative filename (existing.txt)
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("existing.txt").unwrap();

    // Confirm with Enter - should show overwrite confirmation
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show the overwrite confirmation prompt
    harness
        .wait_for_screen_contains("exists. (o)verwrite, (C)ancel?")
        .unwrap();

    // Cancel the operation
    harness.type_text("c").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify cancellation message
    harness.wait_for_screen_contains("Save cancelled").unwrap();

    // Verify the existing file was NOT overwritten
    let existing_content = fs::read_to_string(&existing_path).unwrap();
    assert_eq!(existing_content, "Existing content");

    // Buffer should still show original filename
    harness.assert_screen_contains("original.txt");
}

/// Test Save As overwrites file when user confirms
#[test]
fn test_save_as_overwrite_confirmed() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    // Use with_temp_project to get a working directory with short relative paths
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create two files in the project directory
    let original_path = project_dir.join("original.txt");
    let existing_path = project_dir.join("existing.txt");
    fs::write(&original_path, "Original content").unwrap();
    fs::write(&existing_path, "Existing content").unwrap();

    // Open the original file
    harness.open_file(&original_path).unwrap();
    harness.assert_screen_contains("original.txt");

    // Trigger Save As via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Save File As").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for the Save As prompt to appear
    harness.wait_for_screen_contains("Save as:").unwrap();

    // Clear and type just the relative filename (existing.txt)
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("existing.txt").unwrap();

    // Confirm with Enter - should show overwrite confirmation
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show the overwrite confirmation prompt
    harness
        .wait_for_screen_contains("exists. (o)verwrite, (C)ancel?")
        .unwrap();

    // Confirm overwrite with 'o'
    harness.type_text("o").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify file was saved
    harness.wait_for_screen_contains("Saved as:").unwrap();

    // Verify the existing file WAS overwritten with original content
    let existing_content = fs::read_to_string(&existing_path).unwrap();
    assert_eq!(existing_content, "Original content");

    // Buffer should now show the new filename
    harness.assert_screen_contains("existing.txt");
}

/// Test Save As to same file does NOT prompt for confirmation
#[test]
fn test_save_as_same_file_no_confirmation() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    // Use with_temp_project to get a working directory with short relative paths
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create a file in the project directory
    let file_path = project_dir.join("test.txt");
    fs::write(&file_path, "Test content").unwrap();

    // Open the file
    harness.open_file(&file_path).unwrap();
    harness.assert_screen_contains("test.txt");

    // Trigger Save As via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Save File As").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for the Save As prompt to appear with current filename
    harness.wait_for_screen_contains("Save as:").unwrap();
    harness.assert_screen_contains("test.txt");

    // Just press Enter to save to the same file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should save directly without confirmation (same file)
    harness.wait_for_screen_contains("Saved as:").unwrap();

    // Should NOT have shown confirmation prompt
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("overwrite"),
        "Should not prompt for confirmation when saving to the same file"
    );
}

/// Test that long paths are truncated in the Open File prompt
///
/// When the path + input would exceed 90% of the prompt width, the path should be
/// truncated to show: /first/[...]/last/components/
#[test]
#[cfg_attr(windows, ignore)] // Path truncation format differs on Windows
fn test_open_file_prompt_truncates_long_paths() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;

    // Create a deeply nested directory structure to get a long path
    let temp_dir = tempfile::TempDir::new().unwrap();
    let mut nested_path = temp_dir.path().to_path_buf();

    // Create a path that's definitely longer than 80 chars
    // e.g., /tmp/.../very/deeply/nested/directory/structure/here
    for name in &[
        "very_long_directory_name",
        "another_long_name",
        "deeply",
        "nested",
        "path",
        "structure",
    ] {
        nested_path = nested_path.join(name);
    }
    fs::create_dir_all(&nested_path).unwrap();

    // Create a test file in the nested directory
    fs::write(nested_path.join("test.txt"), "test content").unwrap();

    // Create harness with the deeply nested working directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), nested_path)
            .unwrap();

    // Trigger Open File with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The prompt should show "Open:" and the truncated path
    harness.assert_screen_contains("Open:");

    // The path should be truncated with "[...]" indicator
    // Since the path is very long, it should show something like:
    // Open: /tmp/[...]/path/structure/
    let screen = harness.screen_to_string();

    // Verify the path is truncated (contains [...])
    assert!(
        screen.contains("[...]"),
        "Long path should be truncated with [...]. Screen:\n{}",
        screen
    );

    // The test.txt file should still be visible in the file browser
    // (wait for directory to load)
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    harness.assert_screen_contains("test.txt");
}

/// Test that Open File prompt shows completions popup immediately when opened (issue #193)
///
/// BUG: The suggestions dropdown/popup doesn't appear until the user types a few characters.
/// Users expect to see file completions immediately when the Open File prompt appears.
#[test]
fn test_open_file_prompt_shows_completions_immediately() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;
    use std::time::Duration;

    // Create a temp directory with test files directly in root
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create test files in root (these names won't appear elsewhere on screen)
    fs::write(project_root.join("alpha.txt"), "alpha content").unwrap();
    fs::write(project_root.join("beta.txt"), "beta content").unwrap();
    fs::write(project_root.join("gamma.txt"), "gamma content").unwrap();

    // Copy the real path_complete.ts plugin to the temp directory
    let real_plugins_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("plugins");
    let temp_plugins_dir = project_root.join("plugins");
    fs::create_dir(&temp_plugins_dir).unwrap();

    // Copy path_complete.ts
    fs::copy(
        real_plugins_dir.join("path_complete.ts"),
        temp_plugins_dir.join("path_complete.ts"),
    )
    .unwrap();

    // Copy the lib/ directory that path_complete.ts might depend on
    let real_lib_dir = real_plugins_dir.join("lib");
    if real_lib_dir.exists() {
        let temp_lib_dir = temp_plugins_dir.join("lib");
        fs::create_dir(&temp_lib_dir).unwrap();
        for entry in fs::read_dir(&real_lib_dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_file() {
                fs::copy(&path, temp_lib_dir.join(path.file_name().unwrap())).unwrap();
            }
        }
    }

    // Create harness with temp directory
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Let plugins load
    harness.render().unwrap();
    for _ in 0..10 {
        let _ = harness.editor_mut().process_async_messages();
        harness.sleep(Duration::from_millis(20));
    }
    harness.render().unwrap();

    // Trigger Open File with Ctrl+O (no file opened first, so prompt starts empty)
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    harness.assert_screen_contains("Open:");

    // ISSUE #193: File completions should appear IMMEDIATELY when the prompt opens
    // The prompt starts empty, so we should see files from cwd right away.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Should see test files in completions (these only appear in suggestions)
            screen.contains("alpha.txt")
                || screen.contains("beta.txt")
                || screen.contains("gamma.txt")
        })
        .expect("Completions should appear immediately when Open File prompt opens");

    let screen = harness.screen_to_string();
    println!("Screen after opening prompt:\n{}", screen);
}
