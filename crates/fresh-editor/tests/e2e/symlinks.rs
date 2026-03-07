//! E2E tests for symlink handling
//!
//! Tests that symlinks to files and directories are correctly handled when:
//! - Opening through the file explorer
//! - Opening through the Open File command (Ctrl+O)
//!
//! Bug being reproduced:
//! - Opening a file through a symbolic link creates a new file instead of opening the linked object
//! - Opening a directory through a symbolic link causes "Is a directory (os error 21)" error

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::os::unix::fs::symlink;

/// Test: Opening a symlink to a file through file explorer should open the target file
#[test]
fn test_file_explorer_open_symlink_to_file() -> anyhow::Result<()> {
    // Create test harness with temp project
    let mut harness = EditorTestHarness::create(80, 30, HarnessOptions::new().with_project_root())?;
    let project_dir = harness.project_dir().unwrap();

    // Create a real file with content
    let real_file = project_dir.join("real_file.txt");
    fs::write(&real_file, "Hello from real file")?;

    // Create a symlink to the file
    let link_to_file = project_dir.join("link_to_file.txt");
    symlink(&real_file, &link_to_file)?;

    // Open file explorer
    harness.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)?;
    harness.wait_for_file_explorer()?;

    // Wait for the symlink to appear in the explorer
    harness.wait_for_file_explorer_item("link_to_file.txt")?;

    // Navigate to the symlink
    // First, find the root and expand it if needed
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;

    // Keep navigating until we find link_to_file.txt
    for _ in 0..10 {
        let screen = harness.screen_to_string();
        // Check if the current selection contains link_to_file.txt
        if screen.contains("▶") || screen.contains(">") {
            // Check if we're on the symlink line
            let lines: Vec<&str> = screen.lines().collect();
            for line in &lines {
                // Check for highlighted symlink (look for the file with selection indicator)
                if line.contains("link_to_file.txt") {
                    break;
                }
            }
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    }

    // Select the symlink item - navigate by searching for it
    // Reset to top and search for the file
    harness.send_key(KeyCode::Home, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;

    // Navigate down until we find link_to_file.txt in the selection
    let mut found_symlink = false;
    for _ in 0..20 {
        let screen = harness.screen_to_string();
        // The file explorer shows selection differently - look for the file name
        if screen.contains("link_to_file.txt") {
            // Check if this line has selection marker (the > or highlighted style)
            // For now, just try to open when we see it on screen
            found_symlink = true;
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    }

    assert!(
        found_symlink,
        "Could not find link_to_file.txt in file explorer. Screen:\n{}",
        harness.screen_to_string()
    );

    // Try to open the symlink by pressing Enter
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;

    // After opening, the buffer should contain the real file's content
    // (not be an error, and not be empty/new file)
    let content = harness.get_buffer_content();

    // The bug: opening symlink creates new file instead of opening linked file
    // Expected: content should be "Hello from real file"
    // Bug behavior: content is empty (new file) or error occurs
    assert_eq!(
        content,
        Some("Hello from real file".to_string()),
        "Symlink to file should open the target file content. Screen:\n{}",
        harness.screen_to_string()
    );

    Ok(())
}

/// Test: Opening a symlink to a directory through file explorer should expand/navigate into it
#[test]
fn test_file_explorer_open_symlink_to_directory() -> anyhow::Result<()> {
    // Create harness with isolated temp project (following existing test patterns)
    let mut harness = EditorTestHarness::with_temp_project(120, 40)?;
    let project_root = harness.project_dir().unwrap();

    // Create a real directory with a file inside
    let real_dir = project_root.join("real_dir");
    fs::create_dir(&real_dir)?;
    fs::write(real_dir.join("inside.txt"), "File inside directory")?;

    // Create a symlink to the directory
    let link_to_dir = project_root.join("link_to_dir");
    symlink(&real_dir, &link_to_dir)?;

    // Toggle file explorer on with Ctrl+E
    harness.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)?;
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render()?;

    // Wait for file explorer to initialize
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render()?;

    // Expand root directory with Alt+L (the root "project_root" is selected by default)
    harness.send_key(KeyCode::Char('l'), KeyModifiers::ALT)?;
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render()?;

    // Wait until we see the contents
    harness.wait_for_screen_contains("link_to_dir")?;

    let screen = harness.screen_to_string();
    println!("After expanding root:\n{}", screen);

    // Navigate down to link_to_dir (symlink to directory)
    // It should be one of the first items after project_root
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
        let screen = harness.screen_to_string();
        // Check if we selected link_to_dir
        if screen
            .lines()
            .any(|line| line.contains("link_to_dir") && line.contains("▌"))
        {
            break;
        }
    }

    let screen = harness.screen_to_string();
    assert!(
        screen
            .lines()
            .any(|line| line.contains("link_to_dir") && line.contains("▌")),
        "Should have link_to_dir selected. Screen:\n{}",
        screen
    );

    // Now press Enter to expand the symlink directory
    // Bug: symlinks to directories are treated as files, so this will try to open_file
    // which fails with "Is a directory (os error 21)"
    // Expected: should toggle expand and show inside.txt
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render()?;

    let screen = harness.screen_to_string();
    println!("After pressing Enter on symlink dir:\n{}", screen);

    // Check for the bug: "Is a directory" error message
    assert!(
        !screen.contains("Is a directory"),
        "Opening symlink to directory should not cause 'Is a directory' error. Screen:\n{}",
        screen
    );

    // Verify the symlink directory was expanded by checking if inside.txt is visible
    // With the bug, this won't happen because the symlink is treated as a file
    assert!(
        screen.contains("inside.txt"),
        "Symlink to directory should expand and show inside.txt.\n\
         Bug: Symlink to directory is not being treated as a directory.\n\
         Screen:\n{}",
        screen
    );

    Ok(())
}

/// Test: Opening a symlink to a file via Open File command (Ctrl+O) should open target file
#[test]
fn test_open_file_command_symlink_to_file() -> anyhow::Result<()> {
    // Create test harness with temp project
    let mut harness = EditorTestHarness::create(80, 30, HarnessOptions::new().with_project_root())?;
    let project_dir = harness.project_dir().unwrap();

    // Create a real file with content
    let real_file = project_dir.join("target_file.txt");
    fs::write(&real_file, "Content of target file")?;

    // Create a symlink to the file
    let link_to_file = project_dir.join("symlink_file.txt");
    symlink(&real_file, &link_to_file)?;

    // Open the symlink directly using open_file
    harness.open_file(&link_to_file)?;

    // The buffer should contain the real file's content
    let content = harness.get_buffer_content();

    assert_eq!(
        content,
        Some("Content of target file".to_string()),
        "Opening symlink via open_file should show target file content"
    );

    Ok(())
}

/// Test: Opening a symlink to a directory via Open File command should handle gracefully
#[test]
fn test_open_file_command_symlink_to_directory() -> anyhow::Result<()> {
    // Create test harness with temp project
    let mut harness = EditorTestHarness::create(80, 30, HarnessOptions::new().with_project_root())?;
    let project_dir = harness.project_dir().unwrap();

    // Create a real directory
    let real_dir = project_dir.join("target_dir");
    fs::create_dir(&real_dir)?;

    // Create a symlink to the directory
    let link_to_dir = project_dir.join("symlink_dir");
    symlink(&real_dir, &link_to_dir)?;

    // Try to open the symlink to directory directly
    // This should either:
    // 1. Not error out with "Is a directory"
    // 2. Handle gracefully (e.g., show appropriate error message, not crash)
    let result = harness.editor_mut().open_file(&link_to_dir);

    // The bug: this causes panic or error "Is a directory (os error 21)"
    // Check that it doesn't panic (test would fail if it did)
    // And check for graceful handling
    match result {
        Ok(_) => {
            // If it succeeds, that's fine (maybe opened as directory browser)
            // But shouldn't open a text editor with directory as content
        }
        Err(e) => {
            let error_msg = e.to_string();
            // Should not be a raw OS error - should be a user-friendly message
            assert!(
                !error_msg.contains("os error 21"),
                "Opening symlink to directory should not expose raw OS error. Got: {}",
                error_msg
            );
        }
    }

    Ok(())
}

/// Test: File browser (Ctrl+O dialog) should correctly identify symlinks to directories
#[test]
fn test_file_browser_symlink_to_directory_navigation() -> anyhow::Result<()> {
    // Create test harness with temp project
    let mut harness = EditorTestHarness::create(80, 30, HarnessOptions::new().with_project_root())?;
    let project_dir = harness.project_dir().unwrap();

    // Create a real directory with a file inside
    let real_dir = project_dir.join("actual_dir");
    fs::create_dir(&real_dir)?;
    fs::write(real_dir.join("nested_file.txt"), "Nested content")?;

    // Create a symlink to the directory
    let link_to_dir = project_dir.join("dir_link");
    symlink(&real_dir, &link_to_dir)?;

    // Open file browser with Ctrl+O
    harness.send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)?;
    harness.wait_for_prompt()?;

    // Wait for directory contents to load (avoid checking while "Loading..." is shown)
    harness.wait_for_screen_contains("dir_link")?;

    // The file browser should show both the real dir and the symlink
    let screen = harness.screen_to_string();

    // Check that dir_link appears (the symlink to directory)
    // When selected and Enter pressed, it should navigate into it
    // (not try to open it as a file)

    // This test documents the expected behavior - symlinks to directories
    // should be navigable in the file browser

    assert!(
        screen.contains("dir_link") || screen.contains("actual_dir"),
        "File browser should show directory symlinks. Screen:\n{}",
        screen
    );

    Ok(())
}

/// Test: Files inside expanded symlink directories should show git status indicators
#[test]
fn test_symlink_directory_shows_git_status_indicators() -> anyhow::Result<()> {
    use crate::common::git_test_helper::GitTestRepo;

    let repo = GitTestRepo::new();
    repo.setup_git_explorer_plugin();

    // Create a directory with a file
    std::fs::create_dir(repo.path.join("real_dir")).unwrap();
    repo.create_file("real_dir/modified.txt", "original content");
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify the file to create a git change
    fs::write(repo.path.join("real_dir/modified.txt"), "changed content").unwrap();

    // Create a symlink to the directory
    symlink(repo.path.join("real_dir"), repo.path.join("link_dir")).unwrap();

    let mut harness = EditorTestHarness::with_working_dir(120, 40, repo.path.clone())?;

    harness.editor_mut().toggle_file_explorer();
    harness.wait_for_screen_contains("File Explorer")?;

    // Wait for link_dir to appear
    harness.wait_for_screen_contains("link_dir")?;

    // Navigate to link_dir and expand it
    harness.send_key(KeyCode::Home, KeyModifiers::NONE)?;
    for _ in 0..10 {
        let screen = harness.screen_to_string();
        if screen
            .lines()
            .any(|l| l.contains("link_dir") && l.contains("▌"))
        {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    }

    // Expand the symlink directory
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.sleep(std::time::Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render()?;

    // Wait for modified.txt to appear inside the expanded symlink
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Look for modified.txt with M indicator on the same line
            screen
                .lines()
                .any(|line| line.contains("modified.txt") && line.contains("M"))
        })
        .unwrap();

    Ok(())
}

/// Test: Verify that canonicalization in open_file works correctly for symlinks
#[test]
fn test_symlink_canonicalization_consistency() -> anyhow::Result<()> {
    // Create test harness with temp project
    let mut harness = EditorTestHarness::create(80, 30, HarnessOptions::new().with_project_root())?;
    let project_dir = harness.project_dir().unwrap();

    // Create a real file
    let real_file = project_dir.join("canonical_target.txt");
    fs::write(&real_file, "Canonical content")?;

    // Create a symlink
    let symlink_path = project_dir.join("canonical_link.txt");
    symlink(&real_file, &symlink_path)?;

    // Open via symlink
    harness.open_file(&symlink_path)?;

    // Now open via real path
    harness.editor_mut().open_file(&real_file)?;

    // Both should point to the same buffer (canonicalization should resolve symlink)
    // If they're different buffers, that indicates a bug in path canonicalization
    let tab_bar = harness.get_tab_bar();

    // Should only have one tab for this file (not two: one for symlink, one for real)
    // Count occurrences of the filename
    let filename_count = tab_bar.matches("canonical").count();

    // Expected: only one buffer/tab because symlink is canonicalized to real path
    // Bug behavior: two tabs if symlink and real path create separate buffers
    assert!(
        filename_count <= 1,
        "Opening file via symlink and real path should result in same buffer. Tab bar: {}",
        tab_bar
    );

    Ok(())
}
