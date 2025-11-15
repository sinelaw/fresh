use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crate::common::visual_testing::VisualFlow;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test cursor positioning when moving down in large file mode
/// This test catches a bug where cursor movement with Down arrow key
/// doesn't work correctly after the first few lines in large file mode
#[test]
fn test_large_file_cursor_down_movement() {
    let big_txt_path = TestFixture::big_txt_for_test("cursor_down_movement").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    let mut flow = VisualFlow::new(
        "Large File Cursor Movement",
        "Large File Mode",
        "Testing cursor movement with Down arrow in large file mode",
    );

    // Step 1: Initial state at top of file
    harness
        .capture_visual_step(&mut flow, "initial", "File opened at top")
        .unwrap();

    let initial_pos = harness.cursor_position();
    assert_eq!(initial_pos, 0, "Should start at position 0");

    // Step 2: Move down line by line and verify cursor keeps moving forward
    let mut prev_pos = initial_pos;

    for i in 1..=50 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

        let cursor_pos = harness.cursor_position();
        let screen_pos = harness.screen_cursor_position();

        // The key check: cursor position should keep advancing
        assert!(
            cursor_pos > prev_pos,
            "After {} Down presses, cursor should advance from {} but is at {}",
            i,
            prev_pos,
            cursor_pos
        );

        // Screen cursor Y should increase or stay same (if scrolling)
        // but should definitely be visible
        assert!(
            screen_pos.1 < 24,
            "Screen cursor Y should be within terminal bounds at iteration {}",
            i
        );

        // Visual checkpoints at key moments
        if i == 5 {
            harness
                .capture_visual_step(&mut flow, "after_5_down", "After pressing Down 5 times")
                .unwrap();
        } else if i == 25 {
            harness
                .capture_visual_step(&mut flow, "after_25_down", "After pressing Down 25 times")
                .unwrap();
        } else if i == 50 {
            harness
                .capture_visual_step(&mut flow, "after_50_down", "After pressing Down 50 times")
                .unwrap();
        }

        prev_pos = cursor_pos;
    }

    flow.finalize();
}

/// Test typing characters in large file mode
/// This test catches a bug where typed characters don't appear at the
/// cursor position in large file mode
#[test]
fn test_large_file_typing() {
    let big_txt_path = TestFixture::big_txt_for_test("typing").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    let mut flow = VisualFlow::new(
        "Large File Typing",
        "Large File Mode",
        "Testing that typed characters appear at cursor position in large file mode",
    );

    // Step 1: Initial state
    harness
        .capture_visual_step(&mut flow, "initial", "File opened at top")
        .unwrap();

    // Step 2: Move down several lines to test typing deeper in the file
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    let cursor_pos_before_typing = harness.cursor_position();
    harness
        .capture_visual_step(
            &mut flow,
            "before_typing",
            "Positioned at line 10 before typing",
        )
        .unwrap();

    // Step 3: Type some characters
    let text_to_type = "HELLO";
    harness.type_text(text_to_type).unwrap();

    let cursor_pos_after_typing = harness.cursor_position();
    let screen_pos_after = harness.screen_cursor_position();

    // Verify cursor moved forward by the number of characters typed
    assert_eq!(
        cursor_pos_after_typing,
        cursor_pos_before_typing + text_to_type.len(),
        "Cursor should have moved forward by {} bytes after typing '{}', but moved from {} to {}",
        text_to_type.len(),
        text_to_type,
        cursor_pos_before_typing,
        cursor_pos_after_typing
    );

    // Verify screen cursor is visible and in a reasonable position
    assert!(
        screen_pos_after.0 < 80,
        "Screen cursor X position should be within terminal width"
    );
    assert!(
        screen_pos_after.1 < 24,
        "Screen cursor Y position should be within terminal height"
    );

    harness
        .capture_visual_step(&mut flow, "after_typing", "After typing 'HELLO'")
        .unwrap();

    // Step 4: Continue to move down and type more to verify consistency throughout the file
    for _ in 0..20 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    let cursor_pos_before_second = harness.cursor_position();

    harness.type_text("WORLD").unwrap();

    let cursor_pos_after_second = harness.cursor_position();

    // Verify cursor still moves correctly after typing deeper in the file
    assert_eq!(
        cursor_pos_after_second,
        cursor_pos_before_second + 5,
        "After typing 'WORLD' deeper in file, cursor should advance by 5 bytes"
    );

    harness
        .capture_visual_step(
            &mut flow,
            "after_second_typing",
            "After typing more text deeper in file",
        )
        .unwrap();

    flow.finalize();
}

/// Test cursor positioning when rapidly moving down in large file
/// This stress tests the cursor tracking to ensure it stays in sync
#[test]
fn test_large_file_rapid_cursor_movement() {
    let big_txt_path = TestFixture::big_txt_for_test("rapid_cursor_movement").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    // Rapidly move down 100 lines
    let target_line = 100;
    for i in 1..=target_line {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

        let cursor_pos = harness.cursor_position();
        let expected_byte_pos = i * 80; // Each line is 80 bytes

        assert_eq!(
            cursor_pos, expected_byte_pos,
            "After {} Down presses, cursor should be at byte position {}, but is at {}",
            i, expected_byte_pos, cursor_pos
        );
    }

    // Verify final position
    let final_pos = harness.cursor_position();
    assert_eq!(
        final_pos,
        target_line * 80,
        "Final cursor position should be at line {} (byte {})",
        target_line,
        target_line * 80
    );

    // Verify screen cursor is visible
    let screen_pos = harness.screen_cursor_position();
    assert!(
        screen_pos.0 < 80 && screen_pos.1 < 24,
        "Screen cursor should be within terminal bounds, but is at {:?}",
        screen_pos
    );
}

/// Test cursor and typing interaction in large file mode
/// This combines cursor movement and typing to catch interaction bugs
#[test]
fn test_large_file_cursor_movement_and_typing() {
    let big_txt_path = TestFixture::big_txt_for_test("cursor_and_typing").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    // Move down several lines
    let moves_down = 10;
    for _ in 0..moves_down {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    let pos_after_movement = harness.cursor_position();
    assert_eq!(
        pos_after_movement,
        moves_down * 80,
        "After moving down {} times, should be at byte {}",
        moves_down,
        moves_down * 80
    );

    // Type at this position
    let text = "TEST";
    harness.type_text(text).unwrap();

    let pos_after_typing = harness.cursor_position();
    assert_eq!(
        pos_after_typing,
        pos_after_movement + text.len(),
        "After typing, cursor should advance by text length"
    );

    // Move down again
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    let pos_after_second_movement = harness.cursor_position();
    // After editing, line boundaries may have shifted, but cursor should still advance
    // The important thing is that the cursor moves and doesn't get stuck
    assert!(
        pos_after_second_movement > pos_after_typing,
        "Cursor should advance when pressing Down after typing"
    );

    // Verify screen cursor is visible throughout
    let screen_pos = harness.screen_cursor_position();
    assert!(
        screen_pos.0 < 80 && screen_pos.1 < 24,
        "Screen cursor should remain visible"
    );
}

/// Test that cursor screen position matches logical position in large files
/// This is a visual regression test to ensure the cursor is rendered at the right location
#[test]
fn test_large_file_cursor_screen_position_accuracy() {
    let big_txt_path = TestFixture::big_txt_for_test("cursor_screen_position").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    let mut flow = VisualFlow::new(
        "Large File Cursor Screen Position",
        "Large File Mode",
        "Verifying cursor screen position matches logical position",
    );

    // Capture initial state
    harness
        .capture_visual_step(&mut flow, "line_0", "Cursor at line 0")
        .unwrap();
    let initial_screen_y = harness.screen_cursor_position().1;

    // Move down and capture screen positions
    for i in 1..=10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

        let screen_pos = harness.screen_cursor_position();
        let cursor_pos = harness.cursor_position();

        // Verify logical position
        assert_eq!(
            cursor_pos,
            i * 80,
            "Logical cursor position should match line number * 80"
        );

        // The screen Y position should increase (until we hit viewport scrolling)
        // but we're within the first 10 lines so it should be directly visible
        if i < 20 {
            // Well within viewport
            assert!(
                screen_pos.1 > initial_screen_y,
                "Screen cursor Y should increase when moving down within viewport"
            );
        }

        // Take a visual snapshot every 5 lines
        if i % 5 == 0 {
            harness
                .capture_visual_step(
                    &mut flow,
                    &format!("line_{}", i),
                    &format!("Cursor at line {}", i),
                )
                .unwrap();
        }
    }

    flow.finalize();
}

/// Test load-edit-save flow for both small and large file modes
/// This test validates the complete lifecycle:
/// 1. Load a file (either small or large mode based on threshold)
/// 2. Make edits to the content
/// 3. Save the file
/// 4. Reload and verify changes persisted
#[test]
fn test_load_edit_save_flow_small_and_large_files() {
    use crossterm::event::KeyModifiers;
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();

    // Test 1: Small file mode (under threshold)
    {
        let small_file_path = temp_dir.path().join("small_test.txt");
        let initial_content = "Line 1\nLine 2\nLine 3\n";
        fs::write(&small_file_path, initial_content).unwrap();

        // Use a large threshold to ensure this stays in small file mode
        let mut harness = EditorTestHarness::with_config(
            80,
            24,
            fresh::config::Config {
                editor: fresh::config::EditorConfig {
                    large_file_threshold_bytes: 10 * 1024 * 1024, // 10MB threshold
                    ..Default::default()
                },
                ..Default::default()
            },
        )
        .unwrap();

        // Load the file
        harness.open_file(&small_file_path).unwrap();
        harness.render().unwrap();

        // Verify initial load
        assert_eq!(harness.cursor_position(), 0);

        // Make edits: Move to end of first line and add text
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" EDITED").unwrap();

        // Move to second line and insert text
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        harness.type_text("INSERTED ").unwrap();

        // Save the file (Ctrl+S)
        harness
            .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
            .unwrap();

        // Verify the file was saved by reading it directly
        let saved_content = fs::read_to_string(&small_file_path).unwrap();
        assert!(
            saved_content.contains("Line 1 EDITED"),
            "Expected 'Line 1 EDITED' in saved content, got: {}",
            saved_content
        );
        assert!(
            saved_content.contains("INSERTED Line 2"),
            "Expected 'INSERTED Line 2' in saved content, got: {}",
            saved_content
        );

        // Reload the file in a new harness to verify persistence
        let mut harness2 = EditorTestHarness::with_config(
            80,
            24,
            fresh::config::Config {
                editor: fresh::config::EditorConfig {
                    large_file_threshold_bytes: 10 * 1024 * 1024,
                    ..Default::default()
                },
                ..Default::default()
            },
        )
        .unwrap();

        harness2.open_file(&small_file_path).unwrap();
        harness2.render().unwrap();

        let reloaded_content = harness2.get_buffer_content();
        assert!(
            reloaded_content.contains("Line 1 EDITED"),
            "Reloaded content should contain edits"
        );
        assert!(
            reloaded_content.contains("INSERTED Line 2"),
            "Reloaded content should contain edits"
        );
    }

    // Test 2: Large file mode (over threshold)
    {
        let large_file_path = temp_dir.path().join("large_test.txt");
        // Create content that will exceed our custom threshold
        let mut initial_content = String::new();
        for i in 0..50 {
            initial_content.push_str(&format!("This is line {} with some content\n", i));
        }
        fs::write(&large_file_path, &initial_content).unwrap();

        // Use a small threshold (500 bytes) to force large file mode
        let mut harness = EditorTestHarness::with_config(
            80,
            24,
            fresh::config::Config {
                editor: fresh::config::EditorConfig {
                    large_file_threshold_bytes: 500, // Force large file mode
                    auto_indent: false,
                    ..Default::default()
                },
                ..Default::default()
            },
        )
        .unwrap();

        // Load the file
        harness.open_file(&large_file_path).unwrap();
        harness.render().unwrap();

        // Verify initial load
        assert_eq!(harness.cursor_position(), 0);

        // Make edits in large file mode
        // Move down several lines
        for _ in 0..5 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }

        let pos_before_edit = harness.cursor_position();
        assert!(pos_before_edit > 0, "Cursor should have moved");

        // Add text at this position
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" [LARGE FILE EDIT]").unwrap();

        // Move to a different line and make another edit
        for _ in 0..10 {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        harness.type_text(">>> ").unwrap();

        // Save the file (Ctrl+S)
        harness
            .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
            .unwrap();

        // Verify the file was saved by reading it directly
        let saved_content = fs::read_to_string(&large_file_path).unwrap();
        assert!(
            saved_content.contains("[LARGE FILE EDIT]"),
            "Expected '[LARGE FILE EDIT]' in saved content"
        );
        assert!(
            saved_content.contains(">>>"),
            "Expected '>>>' prefix in saved content"
        );

        // Reload the file in a new harness to verify persistence
        let mut harness2 = EditorTestHarness::with_config(
            80,
            24,
            fresh::config::Config {
                editor: fresh::config::EditorConfig {
                    large_file_threshold_bytes: 500,
                    auto_indent: false,
                    ..Default::default()
                },
                ..Default::default()
            },
        )
        .unwrap();

        harness2.open_file(&large_file_path).unwrap();
        harness2.render().unwrap();

        let reloaded_content = harness2.get_buffer_content();
        assert!(
            reloaded_content.contains("[LARGE FILE EDIT]"),
            "Reloaded content should contain large file edits"
        );
        assert!(
            reloaded_content.contains(">>>"),
            "Reloaded content should contain prefix edits"
        );

        // Verify we can navigate to the edited sections
        // Move down to line 5 where we made the first edit
        for _ in 0..5 {
            harness2.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        // The content should be visible on screen
        harness2.assert_screen_contains("[LARGE FILE EDIT]");
    }

    // Test 3: Verify threshold boundary behavior
    {
        let boundary_file_path = temp_dir.path().join("boundary_test.txt");
        // Create a file exactly at 500 bytes to test threshold boundary
        let content_498 = "x".repeat(498);
        fs::write(&boundary_file_path, &content_498).unwrap();

        let mut harness = EditorTestHarness::with_config(
            80,
            24,
            fresh::config::Config {
                editor: fresh::config::EditorConfig {
                    large_file_threshold_bytes: 500,
                    auto_indent: false,
                    ..Default::default()
                },
                ..Default::default()
            },
        )
        .unwrap();

        harness.open_file(&boundary_file_path).unwrap();
        harness.render().unwrap();

        // Add a few characters to push it over the threshold
        harness.type_text("abc").unwrap();

        // Save
        harness
            .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
            .unwrap();

        // Verify saved
        let saved_content = fs::read_to_string(&boundary_file_path).unwrap();
        assert!(
            saved_content.starts_with("abc"),
            "Should have saved the inserted content"
        );
        assert_eq!(saved_content.len(), 501, "Should be 501 bytes after edit");
    }
}
