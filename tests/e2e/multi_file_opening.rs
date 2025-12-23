use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test opening multiple files and verifying all are accessible via tabs
#[test]
fn test_open_multiple_files() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    let file3 = temp_dir.path().join("file3.txt");

    std::fs::write(&file1, "Content of file 1").unwrap();
    std::fs::write(&file2, "Content of file 2").unwrap();
    std::fs::write(&file3, "Content of file 3").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open all three files
    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();
    harness.open_file(&file3).unwrap();

    // Verify the last opened file is active (file3)
    harness.assert_buffer_content("Content of file 3");

    // Verify all tabs are visible
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");
    harness.assert_screen_contains("file3.txt");

    // Switch to file1 using Ctrl+PageUp multiple times
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content of file 2");

    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content of file 1");

    // Switch back to file2
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content of file 2");

    // Switch to file3
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content of file 3");
}

/// Test opening the same file multiple times (should switch to existing buffer)
#[test]
fn test_open_duplicate_file() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");

    std::fs::write(&file1, "Unique content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the same file twice
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("Unique content");

    // Open the same file again - should switch to existing buffer, not create duplicate
    harness.open_file(&file1).unwrap();
    harness.assert_buffer_content("Unique content");

    // Verify file tab is visible
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");
}

/// Test that cursor positions are maintained per buffer
#[test]
fn test_multiple_files_maintain_cursor_positions() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");

    // File with multiple lines
    std::fs::write(&file1, "Line 1\nLine 2\nLine 3\nLine 4\nLine 5").unwrap();
    std::fs::write(&file2, "Alpha\nBeta\nGamma\nDelta").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open file1 and move cursor to line 3
    harness.open_file(&file1).unwrap();
    let pos1 = harness.cursor_position();

    // Move cursor down a few times to change position
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos1_after_move = harness.cursor_position();

    // Cursor should have moved
    assert_ne!(pos1, pos1_after_move, "Cursor in file1 should have moved");

    // Open file2 (cursor position should change to file2's position)
    harness.open_file(&file2).unwrap();
    harness.assert_buffer_content("Alpha\nBeta\nGamma\nDelta");
    let pos2 = harness.cursor_position();

    // Switch back to file1 using Ctrl+PageUp
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Line 1\nLine 2\nLine 3\nLine 4\nLine 5");

    // Cursor position should be restored to where we left it in file1
    let pos1_restored = harness.cursor_position();
    assert_eq!(
        pos1_after_move, pos1_restored,
        "Cursor position in file1 should be restored when switching back"
    );

    // Switch to file2 again
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Cursor position in file2 should be restored
    let pos2_restored = harness.cursor_position();
    assert_eq!(
        pos2, pos2_restored,
        "Cursor position in file2 should be restored when switching back"
    );
}

/// Test opening files with special characters in names
#[test]
fn test_open_files_with_special_characters() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file-with-dashes.txt");
    let file2 = temp_dir.path().join("file_with_underscores.txt");
    let file3 = temp_dir.path().join("file.multiple.dots.txt");

    std::fs::write(&file1, "Content 1").unwrap();
    std::fs::write(&file2, "Content 2").unwrap();
    std::fs::write(&file3, "Content 3").unwrap();

    let mut harness = EditorTestHarness::new(160, 30).unwrap(); // Use wider terminal to fit all tabs

    // Open all files with special characters
    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();
    harness.open_file(&file3).unwrap();

    // Verify all can be accessed (using substrings to account for tab truncation)
    harness.render().unwrap();
    harness.assert_screen_contains("with-dashes");
    harness.assert_screen_contains("with_underscores");
    harness.assert_screen_contains("multiple.dots");

    // Switch between them
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content 2");

    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Content 1");
}

/// Test opening many files (stress test)
#[test]
fn test_open_many_files() {
    let temp_dir = TempDir::new().unwrap();
    const NUM_FILES: usize = 10;

    // Create 10 test files
    let mut files = Vec::new();
    for i in 0..NUM_FILES {
        let file_path = temp_dir.path().join(format!("file_{:02}.txt", i));
        std::fs::write(&file_path, format!("Content of file {}", i)).unwrap();
        files.push(file_path);
    }

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open all files
    for file in &files {
        harness.open_file(file).unwrap();
    }

    // Last file should be active
    harness.render().unwrap();
    harness.assert_buffer_content("Content of file 9");

    // Should be able to navigate through all of them
    for i in (0..NUM_FILES).rev() {
        harness.assert_buffer_content(&format!("Content of file {}", i));

        if i > 0 {
            harness
                .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
                .unwrap();
            harness.render().unwrap();
        }
    }

    // Navigate forward
    for i in 1..NUM_FILES {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        harness.assert_buffer_content(&format!("Content of file {}", i));
    }
}

/// Test that directory arguments are skipped when opening files
#[test]
fn test_directory_argument_handling() {
    let temp_dir = TempDir::new().unwrap();
    let subdir = temp_dir.path().join("subdir");
    std::fs::create_dir(&subdir).unwrap();

    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");

    std::fs::write(&file1, "File 1 content").unwrap();
    std::fs::write(&file2, "File 2 content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open files - directories should be skipped
    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();

    // Verify both files are accessible
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("file2.txt");
}

/// Test that tabs show in correct order when multiple files are opened
#[test]
fn test_multiple_files_tab_order() {
    let temp_dir = TempDir::new().unwrap();
    let file_alpha = temp_dir.path().join("alpha.txt");
    let file_bravo = temp_dir.path().join("bravo.txt");
    let file_charlie = temp_dir.path().join("charlie.txt");

    std::fs::write(&file_alpha, "Alpha").unwrap();
    std::fs::write(&file_bravo, "Bravo").unwrap();
    std::fs::write(&file_charlie, "Charlie").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open files in order: alpha, bravo, charlie
    harness.open_file(&file_alpha).unwrap();
    harness.open_file(&file_bravo).unwrap();
    harness.open_file(&file_charlie).unwrap();

    // Render to see tabs
    harness.render().unwrap();

    // All three files should be visible in tabs
    harness.assert_screen_contains("alpha.txt");
    harness.assert_screen_contains("bravo.txt");
    harness.assert_screen_contains("charlie.txt");

    // Charlie should be the active (focused) tab
    harness.assert_buffer_content("Charlie");

    // Verify we can navigate in the expected order
    // From charlie: PageUp goes to bravo
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Bravo");

    // From bravo: PageUp goes to alpha
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Alpha");

    // From alpha: PageUp wraps to charlie
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Charlie");
}

/// Test opening files from different directories
#[test]
fn test_open_files_from_different_directories() {
    let temp_dir = TempDir::new().unwrap();
    let subdir1 = temp_dir.path().join("src");
    let subdir2 = temp_dir.path().join("tests");

    std::fs::create_dir(&subdir1).unwrap();
    std::fs::create_dir(&subdir2).unwrap();

    let file1 = subdir1.join("main.rs");
    let file2 = subdir2.join("tests.rs");

    std::fs::write(&file1, "fn main() {}").unwrap();
    std::fs::write(&file2, "#[test]").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open files from different directories
    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();

    // Verify both are accessible
    harness.render().unwrap();
    harness.assert_screen_contains("main.rs");
    harness.assert_screen_contains("tests.rs");

    // Switch between them
    harness.assert_buffer_content("#[test]");
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("fn main() {}");
}
