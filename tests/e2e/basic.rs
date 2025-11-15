use crate::common::harness::EditorTestHarness;
use tempfile::TempDir;

/// Test basic file creation and editing workflow
#[test]
fn test_basic_editing_workflow() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // New buffer should be empty
    harness.assert_buffer_content("");

    // Status bar should show "[No Name]"
    harness.render().unwrap();
    harness.assert_screen_contains("[No Name]");

    // TODO: When action_to_events() is implemented, we can simulate typing:
    // harness.type_text("Hello, World!").unwrap();
    // harness.assert_buffer_content("Hello, World!");
}

/// Test file open and save workflow
#[test]
fn test_file_open_save_workflow() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with some content
    std::fs::write(&file_path, "Initial content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file
    harness.open_file(&file_path).unwrap();

    // Should display the filename
    harness.render().unwrap();
    harness.assert_screen_contains("test.txt");

    // Should show the file content in the buffer
    harness.assert_buffer_content("Initial content");

    // TODO: When action_to_events() is implemented:
    // - Edit the file
    // - Save it
    // - Verify the file on disk has the new content
}

/// Test multi-buffer workflow
#[test]
fn test_multi_buffer_workflow() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");

    std::fs::write(&file1, "File 1 content").unwrap();
    std::fs::write(&file2, "File 2 content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("File 1 content");

    // Open second file
    harness.open_file(&file2).unwrap();
    harness.assert_buffer_content("File 2 content");

    // Should show tabs for both files
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");

    // TODO: Edit both files and verify buffer switching works correctly
}

/// Test buffer switching with keyboard shortcuts
#[test]
fn test_buffer_switching() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("alpha.txt");
    let file2 = temp_dir.path().join("beta.txt");

    std::fs::write(&file1, "Content of alpha").unwrap();
    std::fs::write(&file2, "Content of beta").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("Content of alpha");

    // Open second file (becomes active)
    harness.open_file(&file2).unwrap();
    harness.assert_buffer_content("Content of beta");

    // Verify both tabs are visible and beta is active
    harness.render().unwrap();
    harness.assert_screen_contains("alpha.txt");
    harness.assert_screen_contains("beta.txt");

    // Switch to previous buffer (alpha) using Ctrl+PageUp
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content of alpha");

    // Switch to next buffer (beta) using Ctrl+PageDown
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content of beta");

    // Test cycling: next from beta should go to alpha
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content of alpha");

    // Test cycling backwards: prev from alpha should go to beta
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content of beta");
}

/// Test that opening a file creates viewport with correct dimensions
/// This test captures a bug where open_file() creates the EditorState with
/// hardcoded dimensions (80, 24) instead of using actual terminal dimensions
#[test]
fn test_open_file_viewport_dimensions() {
    use tempfile::TempDir;

    // Create a temp file with some content
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\n").unwrap();

    // Create editor with 131x31 terminal (matching user's scenario)
    let mut harness = EditorTestHarness::new(131, 31).unwrap();

    // Initially, the default buffer has correct viewport dimensions
    let initial_viewport_height = harness.editor().active_state().viewport.height;
    assert_eq!(
        initial_viewport_height, 29,
        "Initial viewport should be 29 (31 - 2)"
    );

    // Open a file
    harness.open_file(&file_path).unwrap();

    // After opening file, viewport height should still match terminal dimensions
    let viewport_height_after_open = harness.editor().active_state().viewport.height;
    assert_eq!(
        viewport_height_after_open, 29,
        "After opening file, viewport height should be 29 (31 - 2), but got {viewport_height_after_open}. \\
         This indicates the file was opened with hardcoded dimensions instead of actual terminal size."
    );

    // Render and verify the viewport displays the correct number of lines
    harness.render().unwrap();

    let visible_count = harness
        .editor()
        .active_state()
        .viewport
        .visible_line_count();

    assert_eq!(
        visible_count, 29,
        "Visible range should be 29 lines, but got {visible_count}"
    );
}

/// Test loading a large file with LSP enabled
/// This test ensures we don't hang or block when opening large files
#[test]
fn test_large_file_with_lsp() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_test.rs");

    // Create a large Rust file (over 1MB to trigger LSP skip)
    let mut content = String::new();
    content.push_str("// Large Rust file for testing\n");
    content.push_str("fn main() {\n");

    // Generate ~1.1MB of content (to exceed the 1MB threshold)
    // Each line is ~50 characters
    for i in 0..25000 {
        content.push_str(&format!(
            "    println!(\"Line number {i} of test content\");\n"
        ));
    }
    content.push_str("}\n");

    std::fs::write(&file_path, &content).unwrap();

    // Verify file is actually large
    let file_size = std::fs::metadata(&file_path).unwrap().len();
    assert!(
        file_size > 1024 * 1024,
        "Test file should be > 1MB (got {file_size} bytes)"
    );

    // Create harness with LSP enabled (default config has LSP)
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the large file - this should NOT hang
    // The timeout on this test will catch hangs
    let result = harness.open_file(&file_path);

    // Should succeed in opening the file
    assert!(
        result.is_ok(),
        "Should be able to open large file without hanging"
    );

    // Verify the file is actually loaded
    harness.render().unwrap();
    harness.assert_screen_contains("large_test.rs");

    // Verify we can see the beginning of the file
    harness.assert_screen_contains("// Large Rust file");
}

/// Test loading a medium-sized file with LSP (under 1MB threshold)
/// This test ensures LSP initialization works correctly for normal-sized files
#[test]
fn test_medium_file_with_lsp() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("medium_test.rs");

    // Create a medium Rust file (under 1MB, so LSP should be enabled)
    let mut content = String::new();
    content.push_str("// Medium Rust file for testing\n");
    content.push_str("fn main() {\n");

    // Generate ~500KB of content
    for i in 0..10000 {
        content.push_str(&format!("    println!(\"Line {i}\");\n"));
    }
    content.push_str("}\n");

    std::fs::write(&file_path, &content).unwrap();

    // Verify file is under 1MB
    let file_size = std::fs::metadata(&file_path).unwrap().len();
    assert!(
        file_size < 1024 * 1024,
        "Test file should be < 1MB (got {file_size} bytes)"
    );

    // Create harness with default config
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file - should succeed with LSP
    let result = harness.open_file(&file_path);

    // Should succeed even with LSP initialization
    assert!(
        result.is_ok(),
        "Should be able to open medium file with LSP"
    );

    // Verify the file is loaded
    harness.render().unwrap();
    harness.assert_screen_contains("medium_test.rs");
    harness.assert_screen_contains("// Medium Rust file");
}

/// Test that we can append text at the end of a file
#[test]
fn test_append_at_end_of_file() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.enable_shadow_validation();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move to end of document
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify cursor is at end
    let cursor_pos = harness.cursor_position();
    assert_eq!(cursor_pos, 20, "Cursor should be at end of Line 3");

    // Type a character - this should append to the end
    harness
        .send_key(KeyCode::Char('!'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the character was appended
    harness.assert_buffer_content("Line 1\nLine 2\nLine 3!");

    // Cursor should now be after the '!'
    let cursor_pos_after = harness.cursor_position();
    assert_eq!(cursor_pos_after, 21, "Cursor should be after '!'");

    // Type another character to ensure we can continue appending
    harness
        .send_key(KeyCode::Char('!'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Line 1\nLine 2\nLine 3!!");

    // Now press Enter to add a new line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Line 1\nLine 2\nLine 3!!\n");

    // Type on the new line
    harness
        .send_key(KeyCode::Char('L'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('4'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("Line 1\nLine 2\nLine 3!!\nLine 4");
}
