use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
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

    let initial_pos = harness.cursor_position();
    assert_eq!(initial_pos, 0, "Should start at position 0");

    // Move down line by line and verify cursor keeps moving forward
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

        prev_pos = cursor_pos;
    }
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

    // Move down several lines to test typing deeper in the file
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    let cursor_pos_before_typing = harness.cursor_position();

    // Type some characters
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

    // Continue to move down and type more to verify consistency throughout the file
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
}

/// Test cursor positioning when rapidly moving down in large file
/// This stress tests the cursor tracking to ensure it stays in sync
#[test]
fn test_large_file_rapid_cursor_movement() {
    let big_txt_path = TestFixture::big_txt_for_test("rapid_cursor_movement").unwrap();

    // Use no-wrap mode since this test expects logical line movement
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();
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

    // Use no-wrap mode since this test expects logical line movement
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();
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
#[test]
fn test_large_file_cursor_screen_position_accuracy() {
    let big_txt_path = TestFixture::big_txt_for_test("cursor_screen_position").unwrap();

    // Use no-wrap mode since this test expects logical line movement
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    let initial_screen_y = harness.screen_cursor_position().1;

    // Move down and verify screen positions
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
    }
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

        // For small files (below threshold), content is fully loaded
        let reloaded_content = harness2.get_buffer_content().unwrap();
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

        // Note: For large files with lazy loading, get_buffer_content() returns None.
        // The save was already verified above via fs::read_to_string().
        // Here we verify the content is accessible in the editor via screen navigation.

        // Verify we can navigate to the edited sections
        // Move down to line 5 where we made the first edit
        for _ in 0..5 {
            harness2
                .send_key(KeyCode::Down, KeyModifiers::NONE)
                .unwrap();
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

/// Test that saving a large file with unloaded regions preserves all data
/// This is a regression test for a bug where save() would silently produce
/// an empty file if any buffer regions were still unloaded.
#[test]
fn test_large_file_save_preserves_unloaded_regions() {
    use fresh::model::buffer::TextBuffer;
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_save_test.txt");

    // Create a file larger than the threshold
    // Use a simple pattern so we can verify integrity: line numbers
    let mut content = String::new();
    for i in 0..1000 {
        content.push_str(&format!(
            "Line {:04}: This is test content for line {}\n",
            i, i
        ));
    }
    let original_len = content.len();
    fs::write(&file_path, &content).unwrap();

    // Open with a threshold that will trigger large file mode
    // The file is ~50KB, use 1KB threshold
    let threshold = 1024;
    let mut buffer = TextBuffer::load_from_file(
        &file_path,
        threshold,
        std::sync::Arc::new(fresh::model::filesystem::StdFileSystem),
    )
    .unwrap();

    // Verify we're in large file mode (line_count returns None for large files)
    assert!(
        buffer.line_count().is_none(),
        "Should be in large file mode (no line indexing)"
    );

    // Make a small edit at the beginning - this should only load a small region
    buffer.insert_bytes(0, b"EDITED: ".to_vec());

    // Save the file
    buffer.save().unwrap();

    // Read back and verify
    let saved_content = fs::read_to_string(&file_path).unwrap();

    // The file should have all original content plus our edit
    let expected_len = original_len + 8; // "EDITED: " is 8 bytes
    assert_eq!(
        saved_content.len(),
        expected_len,
        "Saved file should preserve all content. Got {} bytes, expected {} bytes. \
         If saved file is much smaller, unloaded regions were lost!",
        saved_content.len(),
        expected_len
    );

    // Verify the edit is there
    assert!(
        saved_content.starts_with("EDITED: Line 0000"),
        "Should start with our edit"
    );

    // Verify content from the END of the file is preserved (this would be unloaded)
    assert!(
        saved_content.contains("Line 0999"),
        "Should preserve content from end of file (Line 0999)"
    );

    // Verify content from the MIDDLE of the file is preserved
    assert!(
        saved_content.contains("Line 0500"),
        "Should preserve content from middle of file (Line 0500)"
    );
}

/// Test edits at beginning, middle, and end of a large file using the e2e harness
#[test]
fn test_large_file_edits_beginning_middle_end() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_edit_test.txt");

    // Create 100 lines, ~10KB (enough for 500 byte threshold)
    let mut content = String::new();
    let mut expected_lines = Vec::new();
    let lines = 1_000_000;
    let line_len = format!("Line {:04}  original content\n", 1).len();
    for i in 0..lines {
        let line = format!("Line {:04}  original content\n", i);
        content.push_str(&line);
        expected_lines.push(line);
    }
    fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::with_config(
        80,
        24,
        fresh::config::Config {
            editor: fresh::config::EditorConfig {
                estimated_line_length: line_len,
                ..Default::default()
            },
            ..Default::default()
        },
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Edit lines
    let steps = 17;
    for i in 0..steps {
        let target_line = i * (lines / steps);
        let target_byte = target_line * line_len;
        println!("{}", harness.screen_to_string());
        harness
            .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
            .unwrap();
        // Dismiss the scan confirmation prompt — opens byte offset prompt
        let _ = harness.type_text("n");
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        println!("target byte: {}", target_byte);
        let _ = harness.type_text(&format!("{}B", target_byte).to_string());
        println!("{}", harness.screen_to_string());
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        harness.type_text("MIDDLE_EDIT ").unwrap();
        let edited_screen = harness.screen_to_string();
        println!("{}", edited_screen);
        // find exactly which line was modified and update the equivalent line in expected_lines
        for screen_line in edited_screen.lines() {
            if let Some(match_index) = screen_line.find("MIDDLE_EDIT Line ") {
                let line_num_str: Vec<&str> = screen_line
                    [(match_index + "MIDDLE_EDIT Line ".len())..]
                    .split_whitespace()
                    .collect();
                println!("match: {}", line_num_str[0]);
                let line_num = line_num_str[0].parse::<usize>().unwrap();
                expected_lines[line_num] = format!("MIDDLE_EDIT {}", expected_lines[line_num]);
                println!("expected: {}", expected_lines[line_num]);
            }
        }
    }

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("END_EDIT").unwrap();
    expected_lines.push(format!("END_EDIT"));

    // Save
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();

    // Verify
    let saved_content = fs::read_to_string(&file_path).unwrap();
    let saved_lines: Vec<&str> = saved_content.lines().collect();

    // Note: lines() strips newlines, so we need to compare carefully
    assert_eq!(
        saved_lines.len(),
        expected_lines.len(),
        "Line count mismatch"
    );

    for (i, (got, want)) in saved_lines.iter().zip(expected_lines.iter()).enumerate() {
        let want_trimmed = want.trim_end_matches('\n');
        assert_eq!(
            *got, want_trimmed,
            "Line {} mismatch:\n  got:      {:?}\n  expected: {:?}",
            i, got, want_trimmed
        );
    }
}

/// Test that byte offsets show in the gutter for large files and that scanning
/// switches to exact line numbers.
/// Also tests the Go To Line scan confirmation prompt flow:
/// - Before scan: Ctrl+G shows scan confirmation prompt, gutter shows byte offsets
/// - After answering "n": byte offset prompt opens, user navigates by byte offset
/// - After answering "y": file is scanned, gutter switches to line numbers
/// - After scan: Ctrl+G goes directly to Go To Line prompt (no re-confirmation)
#[test]
fn test_byte_offset_gutter_and_scan() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("byte_offset.txt");

    // Create a file large enough to trigger large file mode (default 1MB threshold)
    let lines = 100_000;
    let mut content = String::new();
    for i in 0..lines {
        content.push_str(&format!("Line {:06} content\n", i));
    }
    let line_len = "Line 000000 content\n".len(); // 20 bytes per line
    fs::write(&file_path, &content).unwrap();

    // Use the temp_dir as working directory so the status bar shows a short
    // relative filename instead of the full absolute path.
    let mut harness =
        EditorTestHarness::with_working_dir(80, 24, temp_dir.path().to_path_buf()).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // === Test 1: Gutter shows byte offsets (pure numeric, no ~ prefix) ===
    let screen = harness.screen_to_string();
    // The first line should show byte offset 0 in the gutter
    let has_byte_offset = screen.lines().any(|line| {
        if let Some(before_sep) = line.split('│').next() {
            let trimmed = before_sep.trim();
            trimmed == "0" || (trimmed.chars().all(|c| c.is_ascii_digit()) && !trimmed.is_empty())
        } else {
            false
        }
    });
    assert!(
        has_byte_offset,
        "Gutter should show byte offsets for large files without line scan.\nScreen:\n{}",
        screen
    );
    // Status bar should show "Byte 0" (not "Ln 1")
    assert!(
        screen.contains("Byte 0"),
        "Status bar should show 'Byte 0' in byte offset mode.\nScreen:\n{}",
        screen
    );

    // === Test 2: Ctrl+G shows scan confirmation prompt ===
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Scan file"),
        "Ctrl+G should show scan confirmation prompt.\nScreen:\n{}",
        screen
    );

    // === Test 3: Dismiss with "n" → byte offset prompt opens ===
    let _ = harness.type_text("n");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("byte offset"),
        "After dismissing scan, byte offset prompt should open.\nScreen:\n{}",
        screen
    );
    // Navigate to byte offset of line 500 (= 500 * 20 = 10000)
    let target_byte = 500 * line_len;
    let _ = harness.type_text(&format!("{}B", target_byte));
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Status bar should show the byte position
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(&format!("Byte {}", target_byte)),
        "Should have jumped to byte offset {}.\nScreen:\n{}",
        target_byte,
        screen
    );

    // === Test 4: Answer "y" to scan, gutter switches to line numbers ===
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let _ = harness.type_text("y");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Drive the incremental scan to completion.
    while harness.editor_mut().process_line_scan() {}
    // After scanning, the Go To Line prompt opens (with exact line numbers now)
    // Cancel it
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Verify gutter now shows line numbers (not byte offsets)
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    // After scan, gutter should have normal line numbers (small values like 501, 502...)
    // and status bar should show "Ln X" instead of "Byte X"
    assert!(
        screen.contains("Ln "),
        "After scanning, status bar should show 'Ln' not 'Byte'.\nScreen:\n{}",
        screen
    );

    // === Test 5: After scan, Ctrl+G goes directly to Go To Line (no scan prompt) ===
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Go to line"),
        "After scan, Ctrl+G should open Go To Line directly (no scan confirmation).\nScreen:\n{}",
        screen
    );
    // Navigate to a specific line
    let _ = harness.type_text("500");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Verify we jumped (status bar should show the line)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 500"),
        "Should have jumped to line 500.\nScreen:\n{}",
        screen
    );
}

/// Test that answering "y" to the scan confirmation shows "Scanning..." progress
/// in the status bar and eventually opens the Go To Line prompt.
#[test]
fn test_line_scan_progress_updates() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("scan_progress.txt");

    // Create a multi-MB file to trigger large file mode.
    let lines = 100_000;
    let mut content = String::new();
    for i in 0..lines {
        content.push_str(&format!("Line {:06} content\n", i));
    }
    fs::write(&file_path, &content).unwrap();

    let mut harness =
        EditorTestHarness::with_working_dir(80, 24, temp_dir.path().to_path_buf()).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Open Ctrl+G scan confirmation and answer "y"
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let _ = harness.type_text("y");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // After answering "y" but before any tick, the status bar should show
    // the initial "Scanning... 0%" progress message.
    harness.render().unwrap();
    let status = harness.get_status_bar();
    assert!(
        status.contains("Scanning"),
        "Status bar should show 'Scanning...' after answering 'y'.\nStatus bar: '{}'",
        status
    );

    // Drive the incremental scan to completion.
    while harness.editor_mut().process_line_scan() {}

    // After the scan completes, the Go To Line prompt should be open
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Go to line"),
        "After scan completes, Go To Line prompt should open.\nScreen:\n{}",
        screen
    );

    // Verify the status bar shows the completion message
    let status = harness.get_status_bar();
    assert!(
        status.contains("Line ind"),
        "Status bar should show scan complete message.\nStatus bar: '{}'",
        status
    );

    // Verify the scan actually worked — press Esc and re-open Ctrl+G,
    // it should go directly to Go To Line (no scan confirmation)
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Go to line"),
        "After scan, Ctrl+G should open Go To Line directly.\nScreen:\n{}",
        screen
    );

    // Navigate to a specific line to verify exact line numbers work
    let _ = harness.type_text("500");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let status = harness.get_status_bar();
    assert!(
        status.contains("Ln 500"),
        "Should have jumped to line 500.\nStatus bar: '{}'",
        status
    );
}

/// End-to-end test for the full large-file line-number lifecycle:
///   1. Open large file (unloaded, byte offset gutter)
///   2. Make edits BEFORE scanning
///   3. Scan line numbers
///   4. Verify exact line numbers (gutter shows line numbers), jump to line
///   5. Make MORE edits in previously-untouched (unloaded) chunks
///   6. Jump around, verify line numbers stay exact
#[test]
fn test_edit_scan_edit_line_numbers_stay_exact() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("edit_scan_edit.txt");

    // Create a multi-MB file to trigger large file mode.
    let lines = 100_000;
    let mut content = String::new();
    for i in 0..lines {
        content.push_str(&format!("Line {:06} content\n", i));
    }
    fs::write(&file_path, &content).unwrap();

    let mut harness =
        EditorTestHarness::with_working_dir(80, 24, temp_dir.path().to_path_buf()).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // === Step 1: Verify byte offset gutter (status bar shows "Byte 0") ===
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Byte 0"),
        "Before scan, status bar should show 'Byte 0'.\nScreen:\n{}",
        screen
    );

    // === Step 2: Make an edit BEFORE scanning ===
    // Type some text at the very beginning of the file.
    harness.type_text("PRE_SCAN_EDIT ").unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("PRE_SCAN_EDIT"),
        "Edit before scan should be visible.\nScreen:\n{}",
        screen
    );

    // === Step 3: Trigger line scan via Ctrl+G → "y" ===
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let _ = harness.type_text("y");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Drive the scan to completion.
    while harness.editor_mut().process_line_scan() {}
    // Cancel the Go To Line prompt that opens after scan.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // === Step 4: Verify exact line numbers (status bar shows "Ln") ===
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln "),
        "After scan, status bar should show 'Ln' (not 'Byte').\nScreen:\n{}",
        screen
    );

    // Jump to a specific line and verify.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Go to line"),
        "After scan, Ctrl+G should open Go To Line directly.\nScreen:\n{}",
        screen
    );
    let _ = harness.type_text("500");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let status = harness.get_status_bar();
    assert!(
        status.contains("Ln 500"),
        "Should have jumped to line 500.\nStatus bar: '{}'",
        status
    );

    // === Step 5: Edit in a previously-untouched (far away) chunk ===
    // Jump to line 50000 — deep in the file, likely in an unloaded chunk.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let _ = harness.type_text("50000");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let status = harness.get_status_bar();
    assert!(
        status.contains("Ln 50000"),
        "Should have jumped to line 50000.\nStatus bar: '{}'",
        status
    );

    // Type text on this line (forces loading the unloaded chunk).
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("POST_SCAN_EDIT ").unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("POST_SCAN_EDIT"),
        "Post-scan edit should be visible.\nScreen:\n{}",
        screen
    );

    // === Step 6: Verify line numbers are STILL exact after the edit ===
    // Status bar should show "Ln" (not "Byte")
    assert!(
        screen.contains("Ln "),
        "After post-scan edit, status bar should still show 'Ln'.\nScreen:\n{}",
        screen
    );

    // Jump to another line to confirm navigation still works with exact numbers.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let _ = harness.type_text("99999");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let status = harness.get_status_bar();
    assert!(
        status.contains("Ln 99999"),
        "Should have jumped to line 99999.\nStatus bar: '{}'",
        status
    );

    // Jump back to the beginning and verify our first edit is still there.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let _ = harness.type_text("1");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("PRE_SCAN_EDIT"),
        "Pre-scan edit should still be visible after jumping around.\nScreen:\n{}",
        screen
    );
}

/// End-to-end test: gutter indicators appear in byte-offset mode (no line scan).
///
/// In large file mode without a line scan, the editor is in byte-offset mode
/// (no line numbers).  Native diff indicators should still appear because
/// `diff_indicators_for_viewport` works purely with byte ranges — it scans
/// viewport bytes for `\n` to find line starts, with no line metadata needed.
///
/// Flow:
///   1. Open a large file (>11 MB) — editor enters byte-offset mode
///   2. Verify byte-offset mode (no line numbers)
///   3. Make edits on three separate lines
///   4. Verify gutter indicators appear on all edited lines (no line scan!)
#[test]
fn test_large_file_gutter_indicators_byte_offset_mode() {
    use std::fs;
    use tempfile::TempDir;

    fn get_content_lines(screen: &str) -> Vec<&str> {
        let lines: Vec<&str> = screen.lines().collect();
        let content_start = 2;
        let content_end = lines.len().saturating_sub(2);
        if content_end > content_start {
            lines[content_start..content_end].to_vec()
        } else {
            vec![]
        }
    }

    fn count_gutter_indicators(screen: &str, symbol: &str) -> usize {
        get_content_lines(screen)
            .iter()
            .filter(|line| {
                line.chars()
                    .next()
                    .map(|c| c.to_string() == symbol)
                    .unwrap_or(false)
            })
            .count()
    }

    fn get_indicator_lines(screen: &str, symbol: &str) -> Vec<usize> {
        get_content_lines(screen)
            .iter()
            .enumerate()
            .filter_map(|(idx, line)| {
                line.chars()
                    .next()
                    .filter(|c| c.to_string() == symbol)
                    .map(|_| idx)
            })
            .collect()
    }

    // ── Setup ──

    let temp_dir = TempDir::new().unwrap();

    // Create a file that exceeds the 11 MB threshold.
    let file_path = temp_dir.path().join("large.txt");
    let line_count = 300_000usize;
    let mut content = String::with_capacity(line_count * 55);
    for i in 0..line_count {
        use std::fmt::Write;
        writeln!(
            content,
            "Line {:06} content for large file testing, padding.",
            i
        )
        .unwrap();
    }
    let file_size = content.len();
    assert!(
        file_size > 11 * 1024 * 1024,
        "Test file should be >11MB, got {} bytes",
        file_size
    );
    fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        fresh::config::Config {
            editor: fresh::config::EditorConfig {
                large_file_threshold_bytes: 11 * 1024 * 1024,
                auto_indent: false,
                ..Default::default()
            },
            ..Default::default()
        },
        temp_dir.path().to_path_buf(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("large.txt"))
        .unwrap();

    // ── Step 1: Verify byte-offset mode ──
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Byte 0"),
        "Should be in byte-offset mode (no line scan).\nScreen:\n{}",
        screen
    );

    // ── Step 2: No indicators before editing ──
    let indicators_before = count_gutter_indicators(&screen, "│");
    assert_eq!(
        indicators_before, 0,
        "No indicators before any edits.\nScreen:\n{}",
        screen
    );

    // ── Step 3: Make edits on three separate lines ──

    // Edit A – line 1 (top of file)
    harness.type_text("EDIT_A ").unwrap();

    // Edit B – 5 lines down
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("EDIT_B ").unwrap();

    // Edit C – 10 more lines down
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("EDIT_C ").unwrap();

    // Go back to top so all edits are visible
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // ── Step 4: Verify indicators appear without line scan ──
    let screen = harness.screen_to_string();
    let indicators = count_gutter_indicators(&screen, "│");
    let indicator_lines = get_indicator_lines(&screen, "│");
    println!(
        "=== Byte-offset mode indicators ===\nCount: {}\nLines: {:?}\nScreen:\n{}",
        indicators, indicator_lines, screen
    );

    // Still in byte-offset mode
    assert!(
        screen.contains("Byte 0"),
        "Should still be in byte-offset mode.\nScreen:\n{}",
        screen
    );

    // All three edits should be visible
    assert!(screen.contains("EDIT_A"), "EDIT_A should be visible");
    assert!(screen.contains("EDIT_B"), "EDIT_B should be visible");
    assert!(screen.contains("EDIT_C"), "EDIT_C should be visible");

    // Exactly the 3 edited lines should have gutter indicators — no more, no less.
    let content_lines = get_content_lines(&screen);
    let edit_a_row = content_lines
        .iter()
        .position(|l| l.contains("EDIT_A"))
        .expect("EDIT_A should be in content area");
    let edit_b_row = content_lines
        .iter()
        .position(|l| l.contains("EDIT_B"))
        .expect("EDIT_B should be in content area");
    let edit_c_row = content_lines
        .iter()
        .position(|l| l.contains("EDIT_C"))
        .expect("EDIT_C should be in content area");

    let expected: Vec<usize> = {
        let mut v = vec![edit_a_row, edit_b_row, edit_c_row];
        v.sort();
        v
    };
    assert_eq!(
        indicator_lines, expected,
        "Only the 3 edited lines should have indicators.\n\
         Expected rows: {:?}\nGot rows: {:?}\nScreen:\n{}",
        expected, indicator_lines, screen
    );
}

/// End-to-end test: large file mode gutter indicators appear after enabling line scan.
///
/// In large file mode, gutter indicators are computed natively from
/// `diff_since_saved()` byte ranges during rendering.  After line scanning
/// completes, line numbers become available in the gutter.
///
/// Flow:
///   1. Open a large file (>11 MB threshold) – editor enters byte-offset mode
///   2. Make edits on several lines – track which lines are edited
///   3. Enable line scanning via Ctrl+G → "y" → line number → Enter
///   4. Verify that the gutter now shows line numbers (not byte offsets)
///   5. Verify that ALL previously-edited lines have gutter indicators
#[test]
#[cfg(feature = "plugins")]
fn test_large_file_gutter_indicators_after_line_scan() {
    use crossterm::event::KeyModifiers;
    use std::fs;
    use tempfile::TempDir;

    // ── Helpers (same logic as plugins/gutter.rs tests) ──

    /// Get content lines from screen (skip menu bar, tab bar, and bottom UI)
    fn get_content_lines(screen: &str) -> Vec<&str> {
        let lines: Vec<&str> = screen.lines().collect();
        let content_start = 2; // after menu bar + tab bar
        let content_end = lines.len().saturating_sub(2); // before status bar + prompt
        if content_end > content_start {
            lines[content_start..content_end].to_vec()
        } else {
            vec![]
        }
    }

    /// Count content lines whose first character is `symbol` (gutter indicator column)
    fn count_gutter_indicators(screen: &str, symbol: &str) -> usize {
        get_content_lines(screen)
            .iter()
            .filter(|line| {
                line.chars()
                    .next()
                    .map(|c| c.to_string() == symbol)
                    .unwrap_or(false)
            })
            .count()
    }

    /// Return 0-indexed content-line indices that have `symbol` as gutter indicator
    fn get_indicator_lines(screen: &str, symbol: &str) -> Vec<usize> {
        get_content_lines(screen)
            .iter()
            .enumerate()
            .filter_map(|(idx, line)| {
                line.chars()
                    .next()
                    .filter(|c| c.to_string() == symbol)
                    .map(|_| idx)
            })
            .collect()
    }

    // ── Setup ──

    let temp_dir = TempDir::new().unwrap();

    // Buffer-modified gutter indicators are now native (no plugin needed).

    // Create a file that exceeds the 11 MB threshold.
    // Each line: "Line NNNNNN content for large file testing, padding." + \n ≈ 55 bytes
    // 300 000 lines × 55 bytes ≈ 16.5 MB (comfortably above 11 MB)
    let file_path = temp_dir.path().join("large.txt");
    let line_count = 300_000usize;
    let mut content = String::with_capacity(line_count * 55);
    for i in 0..line_count {
        use std::fmt::Write;
        writeln!(
            content,
            "Line {:06} content for large file testing, padding.",
            i
        )
        .unwrap();
    }
    let file_size = content.len();
    assert!(
        file_size > 11 * 1024 * 1024,
        "Test file should be >11MB, got {} bytes",
        file_size
    );
    fs::write(&file_path, &content).unwrap();

    // Use 11 MB threshold so the file triggers large file mode
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        fresh::config::Config {
            editor: fresh::config::EditorConfig {
                large_file_threshold_bytes: 11 * 1024 * 1024,
                auto_indent: false,
                ..Default::default()
            },
            ..Default::default()
        },
        temp_dir.path().to_path_buf(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Wait for file and plugin to be ready
    harness
        .wait_until(|h| h.screen_to_string().contains("large.txt"))
        .unwrap();

    // ── Step 1: Verify byte-offset mode (large file, no line scan yet) ──
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Byte 0"),
        "Should be in byte-offset mode before scanning.\nScreen:\n{}",
        screen
    );

    // ── Step 2: Make edits on three separate lines ──

    // Edit A – line 1 (current position, top of file)
    harness.type_text("EDIT_A ").unwrap();

    // Edit B – move down 5 lines, edit at start of line
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("EDIT_B ").unwrap();

    // Edit C – move down 10 more lines, edit at start of line
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("EDIT_C ").unwrap();

    // Go back to top so all edits are visible on screen
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Allow the plugin some time to process (advance test time)
    harness.sleep(std::time::Duration::from_millis(200));
    harness.render().unwrap();

    // Capture pre-scan indicator state
    let screen_before_scan = harness.screen_to_string();
    let indicators_before = count_gutter_indicators(&screen_before_scan, "│");
    println!(
        "=== Before line scan ===\nIndicator count: {}\nIndicator lines: {:?}\nScreen:\n{}",
        indicators_before,
        get_indicator_lines(&screen_before_scan, "│"),
        screen_before_scan
    );

    // Verify that the edits are visible on screen
    assert!(
        screen_before_scan.contains("EDIT_A"),
        "EDIT_A should be visible on screen"
    );
    assert!(
        screen_before_scan.contains("EDIT_B"),
        "EDIT_B should be visible on screen"
    );
    assert!(
        screen_before_scan.contains("EDIT_C"),
        "EDIT_C should be visible on screen"
    );

    // ── Step 3: Enable line scanning via Ctrl+G → "y" ──
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Scan file"),
        "Ctrl+G should show scan confirmation prompt.\nScreen:\n{}",
        screen
    );

    // Answer "y" to start scanning
    let _ = harness.type_text("y");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Drive incremental scan to completion
    while harness.editor_mut().process_line_scan() {}

    // The Go To Line prompt opens after the scan – type a line number and press Enter
    // (this also tests that Go To Line works after scanning)
    let _ = harness.type_text("1");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // ── Step 4: Verify line-number mode ──
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln "),
        "After scanning, status bar should show 'Ln' not 'Byte'.\nScreen:\n{}",
        screen
    );

    // ── Step 5: Trigger plugin re-evaluation ──
    // Make a tiny no-op edit (type + backspace) on line 1 to trigger
    // reapplyIndicatorsFromDiff which now has line_ranges available.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("X").unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // Wait for plugin to process and show gutter indicators
    harness
        .wait_until(|h| count_gutter_indicators(&h.screen_to_string(), "│") >= 3)
        .unwrap();

    // Capture post-scan indicator state
    let screen_after_scan = harness.screen_to_string();
    let indicators_after = count_gutter_indicators(&screen_after_scan, "│");
    let indicator_lines_after = get_indicator_lines(&screen_after_scan, "│");
    println!(
        "=== After line scan ===\nIndicator count: {}\nIndicator lines: {:?}\nScreen:\n{}",
        indicators_after, indicator_lines_after, screen_after_scan
    );

    // Verify the edits are still visible
    assert!(
        screen_after_scan.contains("EDIT_A"),
        "EDIT_A should still be visible after scan"
    );
    assert!(
        screen_after_scan.contains("EDIT_B"),
        "EDIT_B should still be visible after scan"
    );
    assert!(
        screen_after_scan.contains("EDIT_C"),
        "EDIT_C should still be visible after scan"
    );

    // ── Step 6: Verify gutter indicators for ALL edited lines ──
    // Native diff indicators are computed per-frame from diff_since_saved().
    // All three edited lines should now have gutter indicators.
    assert!(
        indicators_after >= 3,
        "After line scan, all 3 edited lines should have gutter indicators. \
         Got {} indicators on lines {:?}.\nScreen:\n{}",
        indicators_after,
        indicator_lines_after,
        screen_after_scan
    );

    // Also verify that the indicators are on the correct content rows.
    // EDIT_A is on content row 0, EDIT_B is on content row 5, EDIT_C is on content row 15.
    // (Content rows are 0-indexed relative to the content area.)
    // Find which content rows have "EDIT_A", "EDIT_B", "EDIT_C"
    let content_lines = get_content_lines(&screen_after_scan);
    let edit_a_row = content_lines
        .iter()
        .position(|l| l.contains("EDIT_A"))
        .expect("EDIT_A should be in content area");
    let edit_b_row = content_lines
        .iter()
        .position(|l| l.contains("EDIT_B"))
        .expect("EDIT_B should be in content area");
    let edit_c_row = content_lines
        .iter()
        .position(|l| l.contains("EDIT_C"))
        .expect("EDIT_C should be in content area");

    assert!(
        indicator_lines_after.contains(&edit_a_row),
        "EDIT_A (content row {}) should have a gutter indicator. \
         Indicator rows: {:?}\nScreen:\n{}",
        edit_a_row,
        indicator_lines_after,
        screen_after_scan
    );
    assert!(
        indicator_lines_after.contains(&edit_b_row),
        "EDIT_B (content row {}) should have a gutter indicator. \
         Indicator rows: {:?}\nScreen:\n{}",
        edit_b_row,
        indicator_lines_after,
        screen_after_scan
    );
    assert!(
        indicator_lines_after.contains(&edit_c_row),
        "EDIT_C (content row {}) should have a gutter indicator. \
         Indicator rows: {:?}\nScreen:\n{}",
        edit_c_row,
        indicator_lines_after,
        screen_after_scan
    );
}

/// Test that gutter indicators update correctly when scrolling through a large file.
///
/// Native diff indicators are viewport-filtered per-frame during rendering.
/// This test verifies:
/// 1. Editing at the top shows indicators for the visible modified lines
/// 2. Jumping to EOF and editing shows indicators at that location (not the old ones)
/// 3. Jumping back to the beginning shows indicators for the originally modified lines
/// 4. Jumping back to EOF shows the EOF edit indicator is still present
#[test]
fn test_large_file_gutter_indicators_viewport_filtering() {
    use crossterm::event::KeyModifiers;
    use tempfile::TempDir;

    // ── Helpers ──

    /// Get content lines from screen (skip menu bar, tab bar, and bottom UI)
    fn get_content_lines(screen: &str) -> Vec<&str> {
        let lines: Vec<&str> = screen.lines().collect();
        let content_start = 2; // after menu bar + tab bar
        let content_end = lines.len().saturating_sub(2); // before status bar + prompt
        if content_end > content_start {
            lines[content_start..content_end].to_vec()
        } else {
            vec![]
        }
    }

    /// Count content lines whose first character is `symbol` (gutter indicator column)
    fn count_gutter_indicators(screen: &str, symbol: &str) -> usize {
        get_content_lines(screen)
            .iter()
            .filter(|line| {
                line.chars()
                    .next()
                    .map(|c| c.to_string() == symbol)
                    .unwrap_or(false)
            })
            .count()
    }

    /// Return 0-indexed content-line indices that have `symbol` as gutter indicator
    fn get_indicator_lines(screen: &str, symbol: &str) -> Vec<usize> {
        get_content_lines(screen)
            .iter()
            .enumerate()
            .filter_map(|(idx, line)| {
                line.chars()
                    .next()
                    .filter(|c| c.to_string() == symbol)
                    .map(|_| idx)
            })
            .collect()
    }

    // ── Setup ──

    // Use a real large file (~848MB) to reproduce the diff offset bug.
    // The bug requires enough chunks (~848) for diff_collect_leaves to skip
    // many identical subtrees via Arc::ptr_eq, producing span-relative
    // line_ranges instead of document-absolute ones.
    let huge_file = std::path::PathBuf::from(
        std::env::var("LARGE_FILE_TEST_PATH")
            .unwrap_or_else(|_| "/home/noam/Desktop/huge.txt".to_string()),
    );
    if !huge_file.exists() {
        eprintln!(
            "Skipping test: large file not found at {:?}. \
             Set LARGE_FILE_TEST_PATH to provide one.",
            huge_file
        );
        return;
    }

    let temp_dir = TempDir::new().unwrap();

    // Buffer-modified gutter indicators are now native (no plugin needed).

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        fresh::config::Config {
            editor: fresh::config::EditorConfig {
                large_file_threshold_bytes: 11 * 1024 * 1024,
                auto_indent: false,
                ..Default::default()
            },
            ..Default::default()
        },
        temp_dir.path().to_path_buf(),
    )
    .unwrap();

    harness.open_file(&huge_file).unwrap();

    // Wait for file to be ready
    harness
        .wait_until(|h| h.screen_to_string().contains("huge.txt"))
        .unwrap();

    // ── Step 1: Enable line scanning via Ctrl+G → "y" ──
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    let _ = harness.type_text("y");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Drive incremental scan to completion
    while harness.editor_mut().process_line_scan() {}

    // The Go To Line prompt opens after scan — go to line 1
    let _ = harness.type_text("1");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Verify line-number mode
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln "))
        .unwrap();
    eprintln!("[test] line numbers active");

    // ── Step 2: Jump to near EOF and type a single char ──
    eprintln!("[test] Step 2: jumping to EOF...");
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    // Move up a few lines so we're not exactly at EOF
    for _ in 0..5 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    eprintln!("[test] Step 2: typing 'X'...");
    harness
        .send_key(KeyCode::Char('X'), KeyModifiers::NONE)
        .unwrap();
    eprintln!("[test] Step 2: typed 'X', waiting for settle...");

    // Wait for the edit to appear, then let viewport_changed settle
    harness
        .wait_until(|h| h.screen_to_string().contains("X"))
        .unwrap();
    harness.wait_until_stable(|_| true).unwrap();
    eprintln!("[test] Step 2: settled");

    // Dump diff data to understand line_ranges
    {
        let diff = harness
            .editor_mut()
            .active_state()
            .buffer
            .diff_since_saved();
        eprintln!(
            "[test] DIFF after edit: equal={} byte_ranges={:?} line_ranges={:?} nodes_visited={}",
            diff.equal, diff.byte_ranges, diff.line_ranges, diff.nodes_visited
        );
    }

    let screen_eof = harness.screen_to_string();
    let eof_indicators = count_gutter_indicators(&screen_eof, "│");
    println!(
        "=== After editing near EOF ===\nIndicator count: {}\nIndicator lines: {:?}\nScreen:\n{}",
        eof_indicators,
        get_indicator_lines(&screen_eof, "│"),
        screen_eof
    );

    // ── Step 3: Jump to top ──
    eprintln!("[test] Step 3: jumping to top...");
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_until_stable(|_| true).unwrap();
    eprintln!("[test] Step 3: at top");

    // ── Step 4: Jump back to EOF — indicator on the X-edited line must persist ──
    eprintln!("[test] Step 4: jumping back to EOF...");
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();

    // Wait for the edited line to be visible, then let viewport_changed settle
    harness
        .wait_until(|h| h.screen_to_string().contains("X"))
        .unwrap();
    harness.wait_until_stable(|_| true).unwrap();
    eprintln!("[test] Step 4: settled at EOF");

    let screen_back_eof = harness.screen_to_string();
    let back_eof_indicators = count_gutter_indicators(&screen_back_eof, "│");
    let back_eof_indicator_lines = get_indicator_lines(&screen_back_eof, "│");
    println!(
        "=== Back at EOF ===\nIndicator count: {}\nIndicator lines: {:?}\nScreen:\n{}",
        back_eof_indicators, back_eof_indicator_lines, screen_back_eof
    );

    // The edited line must have a gutter indicator after jumping back
    assert!(
        back_eof_indicators >= 1,
        "Edited line near EOF should have a gutter indicator after jumping away and back.\n\
         Indicator count: {}\nIndicator lines: {:?}\nScreen:\n{}",
        back_eof_indicators,
        back_eof_indicator_lines,
        screen_back_eof
    );
}
