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
        assert!(
            cursor_pos_after > 0,
            "Cursor position should be restored (not at start)"
        );
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
        assert!(
            result.is_ok(),
            "Session restore should handle missing files"
        );

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
    // Canonicalize paths to handle macOS /var -> /private/var symlink
    assert_eq!(
        std::fs::canonicalize(&session.working_dir).unwrap(),
        std::fs::canonicalize(&project_dir).unwrap()
    );

    // Verify serialization works
    let json = serde_json::to_string_pretty(&session).unwrap();
    assert!(json.contains("test.txt"), "JSON should contain filename");
    assert!(json.contains("version"), "JSON should have version field");

    // Verify deserialization works
    let restored: fresh::session::Session = serde_json::from_str(&json).unwrap();
    assert_eq!(session.version, restored.version);
    assert_eq!(
        std::fs::canonicalize(&session.working_dir).unwrap(),
        std::fs::canonicalize(&restored.working_dir).unwrap()
    );
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

/// Helper: Create a vertical split via command palette
fn split_vertical(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper: Navigate to previous split
fn prev_split(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("prev split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test that session saves and restores cursor position in splits
#[test]
fn test_session_restores_cursor_in_splits() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create files with multiple lines
    let file1 = project_dir.join("left.txt");
    let file2 = project_dir.join("right.txt");
    let content1 = "Left Line 1\nLeft Line 2\nLeft Line 3\nLeft Line 4\nLeft Line 5";
    let content2 = "Right Line 1\nRight Line 2\nRight Line 3\nRight Line 4\nRight Line 5";
    std::fs::write(&file1, content1).unwrap();
    std::fs::write(&file2, content2).unwrap();

    let left_cursor_before;
    let right_cursor_before;

    // First session: create splits and move cursors
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Open first file and move cursor down
        harness.open_file(&file1).unwrap();
        for _ in 0..3 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.render().unwrap();
        left_cursor_before = harness.cursor_position();
        eprintln!("[TEST] Left cursor before: {}", left_cursor_before);

        // Create vertical split
        split_vertical(&mut harness);

        // Open second file and move cursor down
        harness.open_file(&file2).unwrap();
        for _ in 0..2 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.render().unwrap();
        right_cursor_before = harness.cursor_position();
        eprintln!("[TEST] Right cursor before: {}", right_cursor_before);

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify cursor positions
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

        // Right split should be active with restored cursor
        let right_cursor_after = harness.cursor_position();
        eprintln!("[TEST] Right cursor after: {}", right_cursor_after);
        assert_eq!(
            right_cursor_after, right_cursor_before,
            "Right split cursor should be restored exactly"
        );

        // Switch to left split and check its cursor
        prev_split(&mut harness);
        harness.render().unwrap();
        let left_cursor_after = harness.cursor_position();
        eprintln!("[TEST] Left cursor after: {}", left_cursor_after);
        assert_eq!(
            left_cursor_after, left_cursor_before,
            "Left split cursor should be restored exactly"
        );
    }
}

/// Test that session saves and restores scroll position in splits
#[test]
fn test_session_restores_scroll_in_splits() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create files long enough to require scrolling (terminal is 24 lines)
    let file1 = project_dir.join("left_long.txt");
    let file2 = project_dir.join("right_long.txt");
    let content1: String = (1..=100).map(|i| format!("Left Line {:03}\n", i)).collect();
    let content2: String = (1..=100)
        .map(|i| format!("Right Line {:03}\n", i))
        .collect();
    std::fs::write(&file1, &content1).unwrap();
    std::fs::write(&file2, &content2).unwrap();

    // First session: create splits and scroll both
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Open first file and scroll down to line 50
        harness.open_file(&file1).unwrap();
        for _ in 0..49 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.render().unwrap();
        harness.assert_screen_contains("Left Line 050");

        // Create vertical split
        split_vertical(&mut harness);

        // Open second file and scroll down to line 30
        harness.open_file(&file2).unwrap();
        for _ in 0..29 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.render().unwrap();
        harness.assert_screen_contains("Right Line 030");

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify scroll positions
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

        // Right split should show line 30
        harness.assert_screen_contains("Right Line 030");

        // Switch to left split and check its scroll
        prev_split(&mut harness);
        harness.render().unwrap();

        // Left split should show line 50
        harness.assert_screen_contains("Left Line 050");
    }
}

/// Test that cursor remains visible after session restore
/// This reproduces the bug where cursor was visible before save but not visible after restore
#[test]
fn test_session_cursor_visible_after_restore() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create a file long enough to require scrolling (terminal is 24 lines)
    let file = project_dir.join("long.txt");
    let content: String = (1..=100)
        .map(|i| format!("Line {:03} of the document\n", i))
        .collect();
    std::fs::write(&file, &content).unwrap();

    // First session: move cursor to middle of file (cursor visible, scroll follows)
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&file).unwrap();

        // Move cursor down to line 50 - cursor stays visible as scroll follows
        for _ in 0..49 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.render().unwrap();

        // Cursor should be visible - line 50 should be on screen
        harness.assert_screen_contains("Line 050");

        // Verify cursor is on screen (y within content area)
        {
            let (_, cursor_y) = harness.screen_cursor_position();
            let (content_start, content_end) = harness.content_area_rows();
            assert!(
                cursor_y >= content_start as u16 && cursor_y <= content_end as u16,
                "Cursor should be visible on screen before save: y={}, content={}..{}",
                cursor_y,
                content_start,
                content_end
            );
        }

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify cursor is STILL visible
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

        // Line 50 should still be visible (cursor was there)
        harness.assert_screen_contains("Line 050");

        // CRITICAL: Cursor must be visible on screen after restore
        let (_, cursor_y) = harness.screen_cursor_position();
        let (content_start, content_end) = harness.content_area_rows();
        assert!(
            cursor_y >= content_start as u16 && cursor_y <= content_end as u16,
            "BUG: Cursor should be visible after session restore: y={}, content={}..{}",
            cursor_y,
            content_start,
            content_end
        );
    }
}

/// Test that cursor remains visible after session restore in splits
/// This reproduces the bug where cursor is visible before save but not after restore in splits
#[test]
fn test_session_cursor_visible_in_splits_after_restore() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create files with 200 lines - cursor will be at line 150 (middle of file)
    let file1 = project_dir.join("left.txt");
    let file2 = project_dir.join("right.txt");
    let content1: String = (1..=200).map(|i| format!("Left Line {:03}\n", i)).collect();
    let content2: String = (1..=200)
        .map(|i| format!("Right Line {:03}\n", i))
        .collect();
    std::fs::write(&file1, &content1).unwrap();
    std::fs::write(&file2, &content2).unwrap();

    // First session: create split and move cursor to line 150
    // Using user's terminal size: 158 columns x 42 lines
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            158,
            42,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Open first file and move cursor to line 150
        harness.open_file(&file1).unwrap();
        for _ in 0..149 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.render().unwrap();
        harness.assert_screen_contains("Left Line 150");

        // Create split
        split_vertical(&mut harness);

        // Open second file and move cursor to line 150
        harness.open_file(&file2).unwrap();
        for _ in 0..149 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.render().unwrap();
        harness.assert_screen_contains("Right Line 150");

        // Scroll viewport DOWN so cursor at line 150 moves UP on screen
        // This simulates user scrolling to look at content below cursor
        for _ in 0..10 {
            harness
                .send_key(KeyCode::Down, KeyModifiers::CONTROL)
                .unwrap();
        }
        harness.render().unwrap();

        eprintln!("[TEST] After scrolling: cursor still at line 150, viewport scrolled down");

        // Verify cursor is visible before save
        {
            let (_, cursor_y) = harness.screen_cursor_position();
            let (content_start, content_end) = harness.content_area_rows();
            assert!(
                cursor_y >= content_start as u16 && cursor_y <= content_end as u16,
                "Cursor should be visible before save: y={}, content={}..{}",
                cursor_y,
                content_start,
                content_end
            );
        }

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify cursor is visible in active split
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            158,
            42,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.editor_mut().try_restore_session().unwrap();

        // Get cursor and scroll BEFORE first render from EditorState
        let cursor_before_render = harness.cursor_position();
        let viewport_before = harness.editor().active_viewport().clone();
        let (line_before, _) = harness
            .editor()
            .active_state()
            .buffer
            .position_to_line_col(cursor_before_render);

        eprintln!(
            "[TEST] Before render: cursor={} (line {}), top_byte={}, top_view_line_offset={}",
            cursor_before_render,
            line_before + 1,
            viewport_before.top_byte,
            viewport_before.top_view_line_offset
        );

        harness.render().unwrap();

        // Get cursor and scroll AFTER render
        let cursor_after_render = harness.cursor_position();
        let viewport_after = harness.editor().active_viewport().clone();
        let (line_after, _) = harness
            .editor()
            .active_state()
            .buffer
            .position_to_line_col(cursor_after_render);
        eprintln!(
            "[TEST] After render: cursor={} (line {}), top_byte={}, top_view_line_offset={}",
            cursor_after_render,
            line_after + 1,
            viewport_after.top_byte,
            viewport_after.top_view_line_offset
        );

        // CRITICAL: Cursor must be on line 150 after restore
        assert_eq!(
            line_after + 1,
            150,
            "BUG: Cursor should be on line 150, but is on line {}",
            line_after + 1
        );

        // Check if scroll position changed
        if viewport_before.top_byte != viewport_after.top_byte {
            eprintln!(
                "[TEST] WARNING: Scroll changed during render! {} -> {}",
                viewport_before.top_byte, viewport_after.top_byte
            );
        }

        // Right split line 150 should be visible
        harness.assert_screen_contains("Right Line 150");

        // CRITICAL: Cursor must be visible after restore
        let (_, cursor_y) = harness.screen_cursor_position();
        let (content_start, content_end) = harness.content_area_rows();
        assert!(
            cursor_y >= content_start as u16 && cursor_y <= content_end as u16,
            "BUG: Cursor should be visible in split after restore: y={}, content={}..{}",
            cursor_y,
            content_start,
            content_end
        );

        // Also check left split
        prev_split(&mut harness);
        harness.render().unwrap();

        // Get cursor line in left split
        let cursor_left = harness.cursor_position();
        let (line_left, _) = harness
            .editor()
            .active_state()
            .buffer
            .position_to_line_col(cursor_left);
        eprintln!(
            "[TEST] Left split: cursor={} (line {})",
            cursor_left,
            line_left + 1
        );

        // CRITICAL: Left split cursor must also be on line 150
        assert_eq!(
            line_left + 1,
            150,
            "BUG: Left split cursor should be on line 150, but is on line {}",
            line_left + 1
        );

        harness.assert_screen_contains("Left Line 150");

        let (_, cursor_y) = harness.screen_cursor_position();
        assert!(
            cursor_y >= content_start as u16 && cursor_y <= content_end as u16,
            "BUG: Cursor should be visible in left split after restore: y={}, content={}..{}",
            cursor_y,
            content_start,
            content_end
        );
    }
}

/// Test that session restores files when active buffer was a plugin buffer (no file path)
///
/// This reproduces a bug where if the active buffer at session save was a plugin buffer
/// (like the config editor), the real files wouldn't be restored because `active_file_index`
/// would be invalid.
#[test]
fn test_session_restores_files_when_plugin_buffer_was_active() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("real_file.txt");
    std::fs::write(&file1, "Real file content that should be restored").unwrap();

    // First session: open a real file, then create a scratch buffer (simulates plugin buffer)
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Open a real file
        harness.open_file(&file1).unwrap();
        harness.assert_buffer_content("Real file content that should be restored");

        // Create a scratch buffer (similar to plugin buffer - no file path)
        // This simulates opening the config editor or other plugin buffer
        harness.editor_mut().new_buffer();
        harness.render().unwrap();

        // The scratch buffer should now be active (empty content)
        harness.assert_buffer_content("");

        // Save session - active buffer is scratch (no file path)
        harness.editor_mut().save_session().unwrap();

        // Verify session was saved with the real file in open_files
        let session = harness.editor().capture_session();
        let split_state = session.split_states.values().next().unwrap();
        assert!(
            !split_state.open_files.is_empty(),
            "open_files should contain the real file even when scratch buffer is active"
        );
        assert!(
            split_state
                .open_files
                .iter()
                .any(|p| p.to_string_lossy().contains("real_file.txt")),
            "real_file.txt should be in open_files: {:?}",
            split_state.open_files
        );
    }

    // Second session: restore and verify the real file is restored
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Before restore, should have empty buffer (default state)
        harness.assert_buffer_content("");

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should have been restored");

        // The real file should be restorable - open it to verify it's in the buffer list
        harness.open_file(&file1).unwrap();
        harness.assert_buffer_content("Real file content that should be restored");
    }
}

/// Test that session saves and restores split layout
#[test]
fn test_session_restores_splits() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("left.txt");
    let file2 = project_dir.join("right.txt");
    std::fs::write(&file1, "Left split content").unwrap();
    std::fs::write(&file2, "Right split content").unwrap();

    // First session: create two splits with different files
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Open first file
        harness.open_file(&file1).unwrap();
        harness.assert_buffer_content("Left split content");

        // Create vertical split (both splits show the same buffer initially)
        split_vertical(&mut harness);

        // Open second file in the new split
        harness.open_file(&file2).unwrap();
        harness.assert_buffer_content("Right split content");

        // Verify we have 2 splits by checking that BOTH file contents are visible
        // on screen at the same time (not just in tabs)
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        assert!(
            screen.contains("Left split content") && screen.contains("Right split content"),
            "Both file contents should be visible in split view before save.\nScreen:\n{}",
            screen
        );

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify splits are recreated
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

        // After restore, BOTH file contents should be visible at the same time
        // This proves we have 2 splits (not just 2 tabs in 1 split)
        let screen = harness.screen_to_string();
        assert!(
            screen.contains("Left split content") && screen.contains("Right split content"),
            "Both file contents should be visible in split view after restore.\nScreen:\n{}",
            screen
        );

        // The active split should have right file content
        harness.assert_buffer_content("Right split content");

        // Navigate to other split and verify it has left file
        prev_split(&mut harness);
        harness.assert_buffer_content("Left split content");
    }
}

/// Test that session saves and restores files outside the project directory
#[test]
fn test_session_restores_external_files() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    let external_dir = temp_dir.path().join("external");
    std::fs::create_dir(&project_dir).unwrap();
    std::fs::create_dir(&external_dir).unwrap();

    // Create files - one in project, one external
    let project_file = project_dir.join("project_file.txt");
    let external_file = external_dir.join("external_file.txt");
    std::fs::write(&project_file, "Content inside project").unwrap();
    std::fs::write(&external_file, "Content outside project").unwrap();

    // First session: open both files and save
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Open project file
        harness.open_file(&project_file).unwrap();
        harness.assert_buffer_content("Content inside project");

        // Open external file (outside project directory)
        harness.open_file(&external_file).unwrap();
        harness.assert_buffer_content("Content outside project");

        // Verify session captures external files
        let session = harness.editor().capture_session();
        assert!(
            !session.external_files.is_empty(),
            "external_files should contain the external file"
        );
        assert!(
            session
                .external_files
                .iter()
                .any(|p| p.to_string_lossy().contains("external_file.txt")),
            "external_file.txt should be in external_files: {:?}",
            session.external_files
        );

        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify both files are available
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

        // External file should be restorable
        harness.open_file(&external_file).unwrap();
        harness.assert_buffer_content("Content outside project");

        // Project file should also be restorable
        harness.open_file(&project_file).unwrap();
        harness.assert_buffer_content("Content inside project");
    }
}

/// Test that session saves and restores file explorer show_hidden and show_gitignored settings
/// Reproduces issue #569: UI preferences not persisting across sessions
#[test]
fn test_session_restores_file_explorer_hidden_and_gitignored_settings() {
    use crate::common::harness::HarnessOptions;
    use fresh::config_io::DirectoryContext;

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create a test file so the file explorer has something to display
    let regular_file = project_dir.join("regular.txt");
    std::fs::write(&regular_file, "regular content").unwrap();

    // Create shared DirectoryContext so both sessions use the same state directories
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // First session: toggle show_hidden and show_gitignored to true and save
    {
        let mut harness = EditorTestHarness::create(
            100,
            30,
            HarnessOptions::new()
                .with_config(Config::default())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        // Focus file explorer (this internally calls init_file_explorer if needed)
        harness.editor_mut().focus_file_explorer();
        harness.wait_for_file_explorer().unwrap();

        // Verify initial state: show_hidden and show_gitignored should be false
        {
            let explorer = harness.editor().file_explorer().unwrap();
            assert!(
                !explorer.ignore_patterns().show_hidden(),
                "show_hidden should start as false"
            );
            assert!(
                !explorer.ignore_patterns().show_gitignored(),
                "show_gitignored should start as false"
            );
        }

        // Toggle both settings to true
        harness.editor_mut().file_explorer_toggle_hidden();
        harness.editor_mut().file_explorer_toggle_gitignored();

        // Verify the toggles worked
        {
            let explorer = harness.editor().file_explorer().unwrap();
            assert!(
                explorer.ignore_patterns().show_hidden(),
                "show_hidden should be true after toggle"
            );
            assert!(
                explorer.ignore_patterns().show_gitignored(),
                "show_gitignored should be true after toggle"
            );
        }

        // Save session
        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify show_hidden and show_gitignored are still true
    {
        let mut harness = EditorTestHarness::create(
            100,
            30,
            HarnessOptions::new()
                .with_config(Config::default())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should have been restored");

        // Wait for file explorer to be initialized (it's async)
        harness.wait_for_file_explorer().unwrap();
        harness.render().unwrap();

        // File explorer should be visible and settings should be restored
        let explorer = harness.editor().file_explorer().expect(
            "File explorer should be visible after session restore (it was visible when saved)",
        );
        assert!(
            explorer.ignore_patterns().show_hidden(),
            "show_hidden should be true after session restore"
        );
        assert!(
            explorer.ignore_patterns().show_gitignored(),
            "show_gitignored should be true after session restore"
        );
    }
}
