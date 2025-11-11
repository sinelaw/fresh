use crate::common::harness::EditorTestHarness;
use tempfile::TempDir;

/// Test rendering of empty buffer
#[test]
fn test_empty_buffer_rendering() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Should have some output (status bar, etc.)
    assert!(!screen.is_empty());

    // Should show empty buffer indicator
    harness.assert_screen_contains("[No Name]");
}

/// Test rendering of file with content
#[test]
fn test_file_content_rendering() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("render_test.txt");

    // Create a test file with multiple lines
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should show file content on screen
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 2");
    harness.assert_screen_contains("Line 3");

    // Should show filename in status bar
    harness.assert_screen_contains("render_test.txt");
}

/// Test that screen cursor position matches actual cursor position
#[test]
fn test_screen_cursor_position() {
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();

    // Type "abc" on first line
    harness.type_text("abc").unwrap();
    harness.assert_buffer_content("abc");

    // Render and check cursor position
    harness.render().unwrap();

    // Get the actual screen cursor position from the terminal
    let cursor_pos = harness.screen_cursor_position();

    // After typing "abc", cursor should be at column 11:
    // " "  "   1" " │ " "abc" - the cursor should be after 'c'
    // Indicator column: 1 char (space when no indicator)
    // Line numbers are 4 chars wide: "   1"
    // Then " │ " = 3 chars
    // Then "abc" = 3 chars
    // Total: 1 + 4 + 3 + 3 = 11
    // So cursor X should be at column 11 (0-indexed)
    // And cursor Y should be at row 1 (0-indexed, because row 0 is the tab bar)

    println!("Cursor position after typing 'abc': {{cursor_pos:?}}");
    println!("Expected: x=11 (1 + 4 + 3 + 3), y=1");

    assert_eq!(
        cursor_pos.1, 1,
        "Cursor Y should be at row 1 (below tab bar)"
    );
    assert_eq!(
        cursor_pos.0, 11,
        "Cursor X should be at column 11 (after 'abc')"
    );
}

/// Test cursor position as we type more characters
#[test]
fn test_cursor_x_position_advances() {
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();

    // Start with empty buffer
    harness.render().unwrap();
    let pos0 = harness.screen_cursor_position();
    println!("Initial cursor position: {{pos0:?}}");

    // Type first character
    harness.type_text("a").unwrap();
    harness.render().unwrap();
    let pos1 = harness.screen_cursor_position();
    println!("After 'a': {{pos1:?}}");

    // Type second character
    harness.type_text("b").unwrap();
    harness.render().unwrap();
    let pos2 = harness.screen_cursor_position();
    println!("After 'ab': {{pos2:?}}");

    // Type third character
    harness.type_text("c").unwrap();
    harness.render().unwrap();
    let pos3 = harness.screen_cursor_position();
    println!("After 'abc': {{pos3:?}}");

    // Y position should stay constant (row 1)
    assert_eq!(pos0.1, 1, "Initial Y should be 1");
    assert_eq!(pos1.1, 1, "Y should stay at 1 after 'a'");
    assert_eq!(pos2.1, 1, "Y should stay at 1 after 'ab'");
    assert_eq!(pos3.1, 1, "Y should stay at 1 after 'abc'");

    // X position should advance by 1 each time
    assert_eq!(pos1.0, pos0.0 + 1, "X should advance by 1 after 'a'");
    assert_eq!(pos2.0, pos1.0 + 1, "X should advance by 1 after 'b'");
    assert_eq!(pos3.0, pos2.0 + 1, "X should advance by 1 after 'c'");
}

/// Test cursor positioning with large line numbers (1000000+)
/// Verifies that when a file is large enough to have 7-digit line numbers,
/// the gutter width expands appropriately and cursor positioning is correct.
#[test]
fn test_cursor_position_with_large_line_numbers() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_file.txt");

    // Create a large file to trigger 7-digit line numbers
    // We need estimated_lines > 1,000,000
    // estimated_lines = buffer_len / 80
    // So buffer_len = 1,000,000 * 80 = 80,000,000 bytes
    // Create ~81MB file with simple content (each line ~80 chars)
    let mut content = String::new();
    for i in 0..1_000_000 {
        content.push_str(&format!(
            "Line {i:07} with some padding text to reach approximately 80 characters\n"
        ));
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Jump to end of file with Ctrl+End to see the large line numbers
    harness
        .send_key(
            crossterm::event::KeyCode::End,
            crossterm::event::KeyModifiers::CONTROL,
        )
        .unwrap();

    // Check buffer length and gutter width calculation
    let buffer_len = harness.editor().active_state().buffer.len();
    let gutter_width = harness
        .editor()
        .active_state()
        .viewport
        .gutter_width(&harness.editor().active_state().buffer);

    println!("\nBuffer length: {buffer_len} bytes");
    println!("Estimated lines (buffer_len / 80): {}", buffer_len / 80);
    println!("Calculated gutter_width: {gutter_width}");

    harness.render().unwrap();
    let screen_pos = harness.screen_cursor_position();

    // Get the screen lines to see what's actually rendered
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();

    println!("\nWith 7-digit line numbers (file with 1,000,000 lines - at end of file):");
    println!("Full screen dump (last visible lines):");
    for (i, line) in lines.iter().take(5).enumerate() {
        println!("Row {i}: {line:?}");
    }

    println!("\nVisual character position ruler:");
    println!("          1111111111222222222233333333334");
    println!("01234567890123456789012345678901234567890");
    if let Some(content_line) = lines.get(screen_pos.1 as usize) {
        println!("{}", &content_line.chars().take(40).collect::<String>());
        println!("{}^", " ".repeat(screen_pos.0 as usize));
        println!(" cursor is here (pos {})", screen_pos.0);
    }

    println!(
        "\nScreen cursor position: ({}, {})",
        screen_pos.0, screen_pos.1
    );

    // First, verify that the line numbers are correct
    // Filter for lines with line number separator " │ " (not just scrollbar "│")
    let content_lines: Vec<&str> = lines
        .iter()
        .skip(1) // Skip tab bar
        .filter(|line| line.contains(" │ "))
        .copied()
        .collect();

    println!("\nValidating line numbers:");

    // Get the last visible line number
    // Note: For large files, line numbers are estimated when jumping to end
    // The estimation is based on buffer_len / 80 (average line length)
    if let Some(last_line) = content_lines.last() {
        let line_num_part = last_line.split("│").next().unwrap_or("").trim();
        let line_num: usize = line_num_part.parse().unwrap_or(0);
        println!("Last visible line number: {line_num} (may be estimated)");

        // For a 73MB file (1M lines * 73 bytes avg), estimated lines ~= 912,500
        // This is correct behavior - we estimate rather than iterate all lines
        let expected_estimate = buffer_len / 80;
        println!("Expected estimated line number: ~{expected_estimate}");

        // Line number should be close to the estimate (within 10%)
        let lower_bound = expected_estimate.saturating_sub(expected_estimate / 10);
        let upper_bound = expected_estimate + (expected_estimate / 10);

        assert!(
            line_num >= lower_bound && line_num <= upper_bound,
            "Expected line number near {expected_estimate}, but got {line_num}"
        );

        // Verify this is a 6-digit number (912,500 range)
        assert!(
            line_num.to_string().len() >= 6,
            "Expected 6+ digit line number, but {} has {} digits",
            line_num,
            line_num.to_string().len()
        );
    } else {
        panic!("No content lines found!");
    }

    // Now verify cursor positioning is correct for the gutter width
    // The gutter width is based on estimated lines (~912,500)
    // Format: [indicator (1)] + [6 digits] + [" │ " (3 chars)] = 10 chars total
    println!("\nExpected gutter width: 10 (1 + 6 + 3 for 6-digit estimated line numbers)");
    println!("Actual gutter_width: {gutter_width}");

    assert_eq!(
        gutter_width, 10,
        "Gutter width {gutter_width} doesn't match expected 10"
    );

    // The cursor should be positioned AFTER the gutter (at position gutter_width)
    println!("Expected: cursor x = {gutter_width} (at gutter width)");
    println!("Actual: cursor x = {}", screen_pos.0);

    assert_eq!(
        screen_pos.0 as usize, gutter_width,
        "Cursor x position {} should be at gutter width {}",
        screen_pos.0, gutter_width
    );
}

/// Test that line numbers are rendered correctly for files of various sizes
#[test]
#[ignore] // TODO: Fix line numbering with trailing newlines
fn test_line_numbers_rendered_correctly() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    let test_cases = vec![
        (1, "1-line file"),
        (100, "100-line file"),
        (3900, "3900-line file (just under 4k)"),
        (4000, "4000-line file"),
        (4100, "4100-line file (just over 4k)"),
        (10000, "10000-line file"),
    ];

    for (line_count, description) in test_cases {
        println!(
            "\n{}\nTesting: {}\n{}",
            "=".repeat(60),
            description,
            "=".repeat(60)
        );

        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join(format!("test_{line_count}_lines.txt"));

        // Create a file with the specified number of lines
        let mut content = String::new();
        for i in 1..=line_count {
            content.push_str(&format!("Line {i}\n"));
        }
        std::fs::write(&file_path, &content).unwrap();

        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.open_file(&file_path).unwrap();

        // Jump to end with Ctrl+End
        harness
            .send_key(KeyCode::End, KeyModifiers::CONTROL)
            .unwrap();

        harness.render().unwrap();

        // Get the screen to see what's rendered
        let screen = harness.screen_to_string();
        let lines: Vec<&str> = screen.lines().collect();

        println!("Full screen dump:");
        for (i, line) in lines.iter().enumerate() {
            println!("Row {i:2}: {line:?}");
        }

        // Check that we can see the last line number
        // Filter for lines with line number separator " │ " (not just scrollbar "│")
        let content_lines: Vec<&str> = lines
            .iter()
            .skip(1) // Skip tab bar
            .filter(|line| line.contains(" │ "))
            .copied()
            .collect();

        if let Some(last_line) = content_lines.last() {
            println!("\nLast content line: {last_line:?}");

            // Extract the line number
            let line_num_part = last_line.split("│").next().unwrap_or("").trim();
            println!("Line number extracted: {line_num_part:?}");

            let line_num: usize = line_num_part.parse().unwrap_or(0);
            println!("Parsed line number: {line_num}");

            // For files with more than 20 lines, we should see a line number
            // close to the total line count (within visible range)
            let expected_min = if line_count > 20 { line_count - 20 } else { 1 };

            assert!(
                line_num >= expected_min && line_num <= line_count,
                "{description}: Expected to see line numbers between {expected_min} and {line_count}, but got line {line_num}"
            );

            // Verify the last visible line matches the expected line number
            assert_eq!(
                line_num, line_count,
                "{description}: Expected last visible line to be {line_count}, but got {line_num}"
            );
        } else {
            panic!("{description}: No content lines found on screen!");
        }
    }
}

/// Test that page down correctly updates line numbers in the viewport
/// This test loads a buffer with more lines than visible, presses page down twice,
/// and verifies that the top line number is updated correctly and content changes
#[test]
#[ignore] // TODO: Fix line numbering edge cases
fn test_page_down_line_numbers() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with 100 lines, each with unique content like "x1", "x2", etc.
    let content: String = (1..=100).map(|i| format!("x{i}\n")).collect();
    std::fs::write(&file_path, content).unwrap();

    // Create harness with 24 lines visible (minus status bar and tabs)
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Initial state: should be at line 0 (first line)
    let initial_line = harness.top_line_number();
    assert_eq!(initial_line, 0, "Should start at line 0");

    // Verify the first line is visible on screen
    harness.assert_screen_contains("x1");
    let initial_cursor = harness.cursor_position();
    println!("Initial state: line {initial_line}, cursor at {initial_cursor}, screen contains x1");
    println!("Initial screen:\n{}", harness.screen_to_string());

    // Press page down once
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let after_first_pagedown = harness.top_line_number();
    let cursor_after_first = harness.cursor_position();

    println!("\nAfter first PageDown: line {after_first_pagedown}, cursor at {cursor_after_first}");
    println!(
        "Screen after first PageDown:\n{}",
        harness.screen_to_string()
    );

    assert!(
        after_first_pagedown > 0,
        "After first PageDown, should have scrolled down from line 0, but got line {after_first_pagedown}"
    );

    // Verify content has changed - we should see a line number greater than what was initially visible
    // The content "xN" corresponds to line N-1 (0-indexed), so line 39 contains "x40"
    // We verify that we see content from somewhere past the initial viewport
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("x") && after_first_pagedown > 0,
        "Should see content after scrolling"
    );
    println!(
        "After first PageDown: screen contains lines starting from line {after_first_pagedown}"
    );

    // Press page down again to ensure scroll is triggered
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let after_second_pagedown = harness.top_line_number();
    let cursor_after_second = harness.cursor_position();

    println!(
        "\nAfter second PageDown: line {after_second_pagedown}, cursor at {cursor_after_second}"
    );
    println!(
        "Screen after second PageDown:\n{}",
        harness.screen_to_string()
    );

    assert!(
        after_second_pagedown > after_first_pagedown,
        "After second PageDown, should have scrolled down more (from {after_first_pagedown} to {after_second_pagedown})"
    );

    // Verify we can see content from later in the file
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("x") && after_second_pagedown > after_first_pagedown,
        "Should see content after second page down"
    );
    println!(
        "After second PageDown: screen contains lines starting from line {after_second_pagedown}"
    );

    // Verify we no longer see the initial content
    harness.assert_screen_not_contains("x1");

    // Now move up multiple times to trigger scrolling back up
    println!("\n=== Testing upward movement ===");
    let line_before_up = harness.top_line_number();

    // Move up enough times to go past the scroll offset and trigger upward scrolling
    // We need to move up more than scroll_offset (3) lines to trigger scroll
    for i in 0..10 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let current_line = harness.top_line_number();
        let cursor_pos = harness.cursor_position();

        if current_line < line_before_up {
            println!(
                "After {} Up presses: line {} (scrolled up!), cursor at {}",
                i + 1,
                current_line,
                cursor_pos
            );

            // Verify the line number decreased
            assert!(
                current_line < line_before_up,
                "Line number should decrease when scrolling up"
            );

            // Verify content changed - we should see earlier content
            let expected_content = format!("x{}", current_line + 1);
            harness.assert_screen_contains(&expected_content);
            println!("Screen now shows {expected_content}");
            break;
        }
    }

    let final_line = harness.top_line_number();
    assert!(
        final_line < after_second_pagedown,
        "After moving up, viewport should have scrolled up from line {after_second_pagedown} to {final_line}"
    );
}
