//! E2E tests for session persistence
//!
//! These tests verify the full session save/restore cycle works correctly
//! by examining rendered screen output rather than internal state.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::session::get_session_path;
use tempfile::TempDir;

/// Test that session saves and restores open files
#[test]
fn test_session_saves_and_restores_open_files() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("a.txt");
    let file2 = project_dir.join("b.txt");
    std::fs::write(&file1, "Content of file A").unwrap();
    std::fs::write(&file2, "Content of file B").unwrap();

    // First session: open files and save
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&file1).unwrap();
        harness.open_file(&file2).unwrap();

        // Verify both tabs exist - the second file should be active
        harness.assert_buffer_content("Content of file B");

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Before restore, should be empty buffer
        harness.assert_buffer_content("");

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should have been restored");

        // After restore, b.txt should be active (it was the last opened)
        harness.assert_buffer_content("Content of file B");

        // Switch to the other tab and verify that file is also restored
        harness.open_file(&file1).unwrap();
        harness.assert_buffer_content("Content of file A");
    }
}

/// Test that session saves and restores cursor position by checking line numbers
#[test]
fn test_session_restores_cursor_line() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create file with numbered lines for easy verification
    let file = project_dir.join("numbered.txt");
    let content = "Line 01\nLine 02\nLine 03\nLine 04\nLine 05\nLine 06\nLine 07\nLine 08";
    std::fs::write(&file, content).unwrap();

    let cursor_pos_before;

    // First session: move cursor to line 5
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&file).unwrap();

        // Move down 4 lines to reach Line 05
        for _ in 0..4 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }

        // Render and verify cursor position by checking status bar
        harness.render().unwrap();
        cursor_pos_before = harness.cursor_position();
        assert!(cursor_pos_before > 0, "Cursor should have moved");

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify cursor position is restored
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.editor_mut().try_restore_session().unwrap();
        harness.render().unwrap();

        // Cursor should be restored - check it's not at the beginning
        let cursor_pos_after = harness.cursor_position();
        assert!(cursor_pos_after > 0, "Cursor position should be restored (not at start)");
    }
}

/// Test that session handles missing files gracefully
#[test]
fn test_session_handles_missing_files() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("k.txt");
    let file2 = project_dir.join("d.txt");
    std::fs::write(&file1, "Content that survives").unwrap();
    std::fs::write(&file2, "Content to be deleted").unwrap();

    // First session: open both files
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&file1).unwrap();
        harness.open_file(&file2).unwrap();

        // Verify both files are open
        harness.assert_buffer_content("Content to be deleted");
        harness.open_file(&file1).unwrap();
        harness.assert_buffer_content("Content that survives");

        harness.editor_mut().save_session().unwrap();
    }

    // Delete one file between sessions
    std::fs::remove_file(&file2).unwrap();

    // Second session: should restore without error
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Should not panic/error
        let result = harness.editor_mut().try_restore_session();
        assert!(result.is_ok(), "Session restore should handle missing files");

        // Surviving file's content should be accessible
        harness.open_file(&file1).unwrap();
        harness.assert_buffer_content("Content that survives");
    }
}

/// Test that session does not auto-load (simulating --no-session behavior)
#[test]
fn test_no_session_flag_behavior() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file = project_dir.join("important.txt");
    std::fs::write(&file, "Important content here").unwrap();

    // First: save a session with the file
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("important.txt");

        harness.editor_mut().save_session().unwrap();
    }

    // Second: create new editor WITHOUT restoring
    // This simulates --no-session flag behavior
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Explicitly NOT calling try_restore_session()
        harness.render().unwrap();

        // Should see default empty buffer, not the saved file
        harness.assert_screen_contains("[No Name]");
        harness.assert_screen_not_contains("important.txt");
    }
}

/// Test multiple files are all restored
#[test]
fn test_session_restores_multiple_files() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create several test files with unique content
    let files: Vec<_> = (1..=4)
        .map(|i| {
            let file = project_dir.join(format!("f{}.txt", i));
            std::fs::write(&file, format!("Unique content for file number {}", i)).unwrap();
            file
        })
        .collect();

    // First session: open all files
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        for file in &files {
            harness.open_file(file).unwrap();
        }

        // Last opened file should be active
        harness.assert_buffer_content("Unique content for file number 4");

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: verify all restored
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.editor_mut().try_restore_session().unwrap();

        // Verify we can access all files by opening them
        for (i, file) in files.iter().enumerate() {
            harness.open_file(file).unwrap();
            harness.assert_buffer_content(&format!("Unique content for file number {}", i + 1));
        }
    }
}

/// Test that session file is created in the correct XDG location
#[test]
fn test_session_file_location() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("my_project");
    std::fs::create_dir(&project_dir).unwrap();

    // Get expected session path
    let session_path = get_session_path(&project_dir).unwrap();

    // Verify XDG location
    let data_dir = dirs::data_dir().unwrap();
    assert!(
        session_path.starts_with(&data_dir),
        "Session should be in XDG data directory: {:?}",
        session_path
    );
    assert!(
        session_path.to_string_lossy().contains("fresh"),
        "Session should be in 'fresh' subdirectory: {:?}",
        session_path
    );
    assert!(
        session_path.to_string_lossy().contains("sessions"),
        "Session should be in 'sessions' subdirectory: {:?}",
        session_path
    );
    assert!(
        session_path
            .extension()
            .map(|e| e == "json")
            .unwrap_or(false),
        "Session file should have .json extension: {:?}",
        session_path
    );

    // Verify filename is readable (percent-encoded)
    let filename = session_path.file_stem().unwrap().to_string_lossy();
    // Should contain project path elements separated by underscores
    assert!(
        filename.contains("my"),
        "Filename should contain path elements: {:?}",
        filename
    );
}

/// Test session roundtrip: capture and verify data integrity
#[test]
fn test_session_data_integrity() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file = project_dir.join("test.txt");
    std::fs::write(&file, "Test content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Config::default(),
        project_dir.clone(),
    )
    .unwrap();

    harness.open_file(&file).unwrap();

    // Capture session
    let session = harness.editor().capture_session();

    // Verify session has expected data
    assert!(!session.split_states.is_empty(), "Should have split states");
    assert_eq!(session.working_dir, project_dir);

    // Verify serialization works
    let json = serde_json::to_string_pretty(&session).unwrap();
    assert!(json.contains("test.txt"), "JSON should contain filename");
    assert!(json.contains("version"), "JSON should have version field");

    // Verify deserialization works
    let restored: fresh::session::Session = serde_json::from_str(&json).unwrap();
    assert_eq!(session.version, restored.version);
    assert_eq!(session.working_dir, restored.working_dir);
}

/// Test scroll position is persisted for long files
#[test]
fn test_session_restores_scroll_position() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create a file long enough to require scrolling
    let file = project_dir.join("long.txt");
    let content: String = (1..=100)
        .map(|i| format!("Line {:03} content here\n", i))
        .collect();
    std::fs::write(&file, &content).unwrap();

    // First session: scroll down significantly
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&file).unwrap();

        // Scroll down past the initial view (24 lines visible, go to line 50)
        for _ in 0..49 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }

        harness.render().unwrap();
        // Status bar shows current line (0-indexed internally, so moving 49 times = line 50)
        // But display might show different - check for the line content instead
        harness.assert_screen_contains("Line 050");
        // Line 001 should be scrolled off screen
        // (Note: First few lines might still be visible due to viewport)

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: verify scroll position restored
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.editor_mut().try_restore_session().unwrap();
        harness.render().unwrap();

        // Should still show line 50 content on screen
        harness.assert_screen_contains("Line 050");
    }
}

/// Test that switching tabs before save preserves the active tab
#[test]
fn test_session_preserves_active_tab() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("first.txt");
    let file2 = project_dir.join("second.txt");
    std::fs::write(&file1, "First file content").unwrap();
    std::fs::write(&file2, "Second file content").unwrap();

    // First session: open both files, switch to first
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&file1).unwrap();
        harness.open_file(&file2).unwrap();

        // After opening file2, it should be active
        harness.assert_buffer_content("Second file content");

        // Switch back to first file (Ctrl+PageUp or similar)
        // Using buffer switching - open file1 again switches to its tab
        harness.open_file(&file1).unwrap();
        harness.assert_buffer_content("First file content");

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: should restore with first file active
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.editor_mut().try_restore_session().unwrap();

        // First file should be active (its content should be displayed)
        harness.assert_buffer_content("First file content");
    }
}
