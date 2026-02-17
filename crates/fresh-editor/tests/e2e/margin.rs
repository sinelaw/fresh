use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test that line numbers are rendered correctly with the margin system
#[test]
fn test_margin_line_numbers_rendering() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with 10 lines
    std::fs::write(
        &file_path,
        "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen output:\n{screen}");

    // Should show line numbers in the left margin
    harness.assert_screen_contains("   1 │");
    harness.assert_screen_contains("   2 │");
    harness.assert_screen_contains("   3 │");

    // Should show file content
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 2");
    harness.assert_screen_contains("Line 3");
}

/// Test that margins work correctly in empty buffers
#[test]
fn test_margin_empty_buffer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Empty buffer screen:\n{screen}");

    // Should show line 1 even for empty buffer
    harness.assert_screen_contains("   1 │");
}

/// Test that line_numbers config is respected when launching without a file
/// Reproduces issue #539: Line Numbers won't turn off if you launch Fresh
/// without designating a file to edit.
#[test]
fn test_initial_buffer_respects_line_numbers_config() {
    // Create config with line_numbers disabled
    let mut config = fresh::config::Config::default();
    config.editor.line_numbers = false;

    // Create harness with this config - no file opened, just the initial empty buffer
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Empty buffer screen (line_numbers=false):\n{screen}");

    // Line numbers should NOT be shown because config has line_numbers=false
    // The line number separator pattern " │ " should not appear
    harness.assert_screen_not_contains(" │ ");

    // Content should still be editable
    harness.type_text("Hello").unwrap();
    harness.assert_screen_contains("Hello");
}

/// Test that line numbers adjust width for large files
#[test]
fn test_margin_large_file_line_numbers() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large.txt");

    // Create a file with 1000 lines
    let content: String = (1..=1000).map(|i| format!("Line {i}\n")).collect();
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Jump to end
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Large file screen (at end):\n{screen}");

    // Should show 4-digit line numbers
    // Line 1000 should be visible
    harness.assert_screen_contains("1000 │");
}

/// Test that margins can be disabled via toggle action
#[test]
fn test_margin_disable_line_numbers() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Verify line numbers are shown initially
    harness.assert_screen_contains(" │ ");

    // Toggle line numbers off via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Line Numbers").unwrap();
    harness
        .wait_for_screen_contains("Toggle Line Numbers")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen without line numbers:\n{screen}");

    // Should NOT show line numbers (check for line number separator pattern " │ " with spaces)
    // Note: We can't just check for "│" because the scrollbar also uses that character
    harness.assert_screen_not_contains(" │ ");

    // Should still show content (but without margin)
    harness.assert_screen_contains("Line 1");
}

/// Test adding custom margin annotations (e.g., breakpoint, error)
#[test]
fn test_margin_custom_annotations() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Add a breakpoint annotation at line 2 (0-indexed)
    harness
        .apply_event(fresh::model::event::Event::AddMarginAnnotation {
            line: 2,
            position: fresh::model::event::MarginPositionData::Left,
            content: fresh::model::event::MarginContentData::Symbol {
                text: "●".to_string(),
                color: Some((255, 0, 0)), // Red
            },
            annotation_id: Some("breakpoint-1".to_string()),
        })
        .unwrap();

    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with breakpoint annotation:\n{screen}");

    // Should show the breakpoint symbol on line 3 (1-indexed display)
    // The line should have both line number and breakpoint
    harness.assert_screen_contains("●");

    // Remove the annotation
    harness
        .apply_event(fresh::model::event::Event::RemoveMarginAnnotation {
            annotation_id: "breakpoint-1".to_string(),
        })
        .unwrap();

    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("Screen after removing annotation:\n{screen_after}");

    // Breakpoint should be gone
    // But line numbers should still be there
    harness.assert_screen_contains("   3 │");
}

/// Test that margins work correctly after editing
#[test]
fn test_margin_after_editing() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some lines
    harness.type_text("First line").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Second line").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Third line").unwrap();

    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after typing:\n{screen}");

    // Should show line numbers for all lines
    harness.assert_screen_contains("   1 │");
    harness.assert_screen_contains("   2 │");
    harness.assert_screen_contains("   3 │");

    // Should show typed content
    harness.assert_screen_contains("First line");
    harness.assert_screen_contains("Second line");
    harness.assert_screen_contains("Third line");
}

/// Test cursor position with margin (cursor should account for margin width)
#[test]
fn test_cursor_position_with_margin() {
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();

    harness.type_text("abc").unwrap();
    harness.render().unwrap();

    // Get content area bounds from harness (accounts for menu bar, tab bar, status bar)
    let (content_first_row, _content_last_row) = harness.content_area_rows();

    let cursor_pos = harness.screen_cursor_position();
    println!("Cursor position: {cursor_pos:?}");

    // Format: [indicator (1)] + [line numbers (4)] + [" │ " (3)] = 8 chars gutter
    // cursor after "abc" should be at column 11 (8 + 3)
    assert_eq!(
        cursor_pos.0, 11,
        "Cursor X position should account for margin width"
    );
    assert_eq!(
        cursor_pos.1, content_first_row as u16,
        "Cursor Y position should be on first line (row {content_first_row})"
    );
}

/// Test that margins work with horizontal scrolling
#[test]
fn test_margin_with_horizontal_scroll() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("long_line.txt");

    // Create a file with a very long line
    let long_line = "X".repeat(200);
    std::fs::write(&file_path, &long_line).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move cursor to the right to trigger horizontal scrolling
    for _ in 0..100 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with horizontal scroll:\n{screen}");

    // Line number should still be visible even when horizontally scrolled
    harness.assert_screen_contains("   1 │");

    // Should see X's (the content)
    harness.assert_screen_contains("X");
}

/// Test that margins are per-buffer in split view
/// Each buffer should have its own independent margin state
#[test]
#[ignore = "Splits currently share the same active buffer (architectural limitation). All splits display the currently active buffer, so this test's assumption of independent buffers per split doesn't match current behavior."]
fn test_margin_per_buffer_in_split_view() {
    let temp_dir = TempDir::new().unwrap();

    // Create two files
    let file1_path = temp_dir.path().join("file1.txt");
    let file2_path = temp_dir.path().join("file2.txt");
    std::fs::write(&file1_path, "File 1 Line 1\nFile 1 Line 2\n").unwrap();
    std::fs::write(&file2_path, "File 2 Line 1\nFile 2 Line 2\nFile 2 Line 3\n").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Open first file
    harness.open_file(&file1_path).unwrap();

    // Create a vertical split and open second file
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.open_file(&file2_path).unwrap();

    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Split view screen:\n{screen}");

    // Both splits should show line numbers
    harness.assert_screen_contains("   1 │");

    // Both files should be visible
    harness.assert_screen_contains("File 1 Line 1");
    harness.assert_screen_contains("File 2 Line 1");

    // Now disable line numbers in the active buffer (file2)
    harness
        .apply_event(fresh::model::event::Event::SetLineNumbers { enabled: false })
        .unwrap();

    // Add a custom annotation to file1 (need to switch to file1 first)
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::ALT)
        .unwrap(); // Switch to previous split
    harness
        .apply_event(fresh::model::event::Event::AddMarginAnnotation {
            line: 0,
            position: fresh::model::event::MarginPositionData::Left,
            content: fresh::model::event::MarginContentData::Symbol {
                text: "●".to_string(),
                color: Some((255, 0, 0)),
            },
            annotation_id: Some("file1-marker".to_string()),
        })
        .unwrap();

    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("Split view after modifications:\n{screen_after}");

    // File 1 should still have line numbers
    // Note: The marker might not be visible depending on split layout
    // The key point is that disabling line numbers in file2 doesn't affect file1

    // This verifies that each EditorState has its own MarginManager
    // If margins were shared, disabling in one would affect both
}

/// Test that line numbers update correctly when scrolling down incrementally
/// This reproduces a bug where line numbers in the margin don't update when scrolling
#[test]
fn test_line_numbers_update_during_incremental_scroll() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("scroll_test.txt");

    // Create a file with 100 lines (enough to require scrolling)
    let content: String = (1..=100).map(|i| format!("Line {i}\n")).collect();
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Initial state - should show line 1 at the top
    let screen = harness.screen_to_string();
    println!("Initial screen:\n{screen}");

    harness.assert_screen_contains("   1 │");
    harness.assert_screen_contains("Line 1");

    // Scroll down with PageDown
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after_pagedown = harness.screen_to_string();
    println!("\nScreen after PageDown:\n{screen_after_pagedown}");

    // After PageDown, we should be around line 22-23 (viewport is ~22 lines tall)
    // The line numbers in the margin should have updated to reflect the new position
    // BUG: This assertion will FAIL if line numbers don't update
    let should_contain_line_20_or_higher = screen_after_pagedown.contains("  20 │")
        || screen_after_pagedown.contains("  21 │")
        || screen_after_pagedown.contains("  22 │")
        || screen_after_pagedown.contains("  23 │")
        || screen_after_pagedown.contains("  24 │")
        || screen_after_pagedown.contains("  25 │");

    assert!(
        should_contain_line_20_or_higher,
        "After PageDown, line numbers should show lines around 20-25, but screen shows:\n{}",
        screen_after_pagedown
    );

    // Scroll down a bit more with Down arrow keys
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let screen_after_down = harness.screen_to_string();
    println!("\nScreen after 5x Down:\n{screen_after_down}");

    // Should now show even higher line numbers (around 27-30)
    let should_contain_line_27_or_higher = screen_after_down.contains("  27 │")
        || screen_after_down.contains("  28 │")
        || screen_after_down.contains("  29 │")
        || screen_after_down.contains("  30 │")
        || screen_after_down.contains("  31 │");

    assert!(
        should_contain_line_27_or_higher,
        "After 5 more Down keys, line numbers should show lines around 27-31, but screen shows:\n{}",
        screen_after_down
    );

    // Verify line 1 is no longer visible
    harness.assert_screen_not_contains("   1 │");
}

/// Test that line numbers update correctly with PageUp/PageDown and Ctrl+Home/End
#[test]
fn test_line_numbers_update_with_navigation_keys() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("nav_test.txt");

    // Create a file with 200 lines (enough for multiple page scrolls)
    let content: String = (1..=200).map(|i| format!("Line {i}\n")).collect();
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // === Test 1: Initial state ===
    harness.assert_screen_contains("   1 │");
    harness.assert_screen_contains("Line 1");

    // === Test 2: PageDown multiple times ===
    for i in 1..=3 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        println!("\nScreen after PageDown #{i}:\n{screen}");
    }

    // After 3 PageDowns, should be around line 60-70
    let screen = harness.screen_to_string();
    let should_be_around_line_60 = screen.contains("  60 │")
        || screen.contains("  61 │")
        || screen.contains("  62 │")
        || screen.contains("  63 │")
        || screen.contains("  64 │")
        || screen.contains("  65 │")
        || screen.contains("  66 │")
        || screen.contains("  67 │")
        || screen.contains("  68 │")
        || screen.contains("  69 │")
        || screen.contains("  70 │");

    assert!(
        should_be_around_line_60,
        "After 3 PageDowns, should be around line 60-70, but screen shows:\n{}",
        screen
    );

    // === Test 3: PageUp twice ===
    for i in 1..=2 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        println!("\nScreen after PageUp #{i}:\n{screen}");
    }

    // After 2 PageUps from ~line 65, should be around line 20-25
    let screen = harness.screen_to_string();
    let should_be_around_line_20 = screen.contains("  20 │")
        || screen.contains("  21 │")
        || screen.contains("  22 │")
        || screen.contains("  23 │")
        || screen.contains("  24 │")
        || screen.contains("  25 │")
        || screen.contains("  26 │")
        || screen.contains("  27 │")
        || screen.contains("  28 │");

    assert!(
        should_be_around_line_20,
        "After 2 PageUps, should be around line 20-28, but screen shows:\n{}",
        screen
    );

    // === Test 4: Ctrl+End (jump to end) ===
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("\nScreen after Ctrl+End:\n{screen}");

    // Should show line 200 (last line) and lines near it
    harness.assert_screen_contains(" 200 │");
    harness.assert_screen_contains("Line 200");

    // Line 1 should definitely not be visible
    harness.assert_screen_not_contains("   1 │");

    // Should also see lines in the 180s-190s range (last screenful)
    let has_high_lines = screen.contains(" 180 │")
        || screen.contains(" 185 │")
        || screen.contains(" 190 │")
        || screen.contains(" 195 │")
        || screen.contains(" 199 │");

    assert!(
        has_high_lines,
        "At end of file, should show lines in 180s-190s range, but screen shows:\n{}",
        screen
    );

    // === Test 5: Ctrl+Home (jump to beginning) ===
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("\nScreen after Ctrl+Home:\n{screen}");

    // Should be back to showing line 1
    harness.assert_screen_contains("   1 │");
    harness.assert_screen_contains("Line 1");

    // Line 200 should not be visible
    harness.assert_screen_not_contains(" 200 │");

    // Should see early lines
    harness.assert_screen_contains("   2 │");
    harness.assert_screen_contains("   3 │");
    harness.assert_screen_contains("  10 │");
    harness.assert_screen_contains("  20 │");

    // === Test 6: Jump back and forth to ensure consistency ===
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(" 200 │");

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("   1 │");
}
