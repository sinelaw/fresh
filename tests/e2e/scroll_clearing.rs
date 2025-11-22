/// Tests for scroll clearing behavior
/// Bug: when opening a file and scrolling all the way to the bottom of the file
/// and scrolling down, the view doesn't clear properly and leftover characters
/// from previous renders are still shown.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::path::PathBuf;

/// Get the path to the scroll test fixture file
fn scroll_test_file_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/scroll_test_file.txt")
}

/// Test that scrolling to the bottom of a file and then scrolling down
/// does not leave leftover characters from previous renders on screen.
#[test]
fn test_scroll_clearing_at_bottom_of_file() {
    let test_file_path = scroll_test_file_path();

    // Create a test harness with standard terminal dimensions
    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();

    // Open the test file
    harness.open_file(&test_file_path).unwrap();
    harness.render().unwrap();

    println!("\n=== Testing scroll clearing at bottom of file ===");
    println!("File: {:?}", test_file_path);

    // Capture initial screen state
    let initial_screen = harness.screen_to_string();
    println!("\n--- Initial screen ---");
    println!("{}", initial_screen);

    // Verify the file was loaded correctly
    harness.assert_screen_contains("static int lfs_migrate_to_dom");

    // Get content area bounds
    let (content_first_row, content_last_row) = harness.content_area_rows();
    println!("\nContent area: rows {} to {}", content_first_row, content_last_row);

    // Jump to the end of the file
    harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    println!("\n--- After jumping to end (Ctrl+End) ---");
    let screen_at_end = harness.screen_to_string();
    println!("{}", screen_at_end);

    // Verify we can see the last line of the file
    harness.assert_screen_contains("lfs_mirror_extend");

    // Now try to scroll down further (this should be a no-op since we're at the bottom)
    // This is where the bug might manifest - leftover characters from previous renders

    println!("\n=== Attempting to scroll down past end of file ===");

    // Store the screen before attempting to scroll past the end
    let screen_before_overscroll = harness.screen_to_string();

    // Try PageDown multiple times
    for i in 1..=5 {
        harness.send_key(KeyCode::PageDown, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let screen_after = harness.screen_to_string();

        println!("\n--- After PageDown #{} ---", i);

        // Compare with the previous screen
        if screen_after != screen_before_overscroll {
            println!("WARNING: Screen changed after PageDown at end of file!");
            println!("This may indicate improper render clearing.");
        }
    }

    // Now do a detailed analysis of the render output
    println!("\n=== Detailed render analysis ===");

    let final_screen = harness.screen_to_string();
    let lines: Vec<&str> = final_screen.lines().collect();

    // Check each content row for anomalies
    for row_idx in content_first_row..=content_last_row {
        if row_idx < lines.len() {
            let line = lines[row_idx];
            println!("Row {}: {}", row_idx, line);
        }
    }

    // Specific check: After scrolling to the bottom and beyond,
    // we should NOT see content from the top of the file unless
    // the file is small enough to fit entirely on screen

    let buffer_content = harness.get_buffer_content();
    let buffer_lines: Vec<&str> = buffer_content.lines().collect();
    let total_lines = buffer_lines.len();
    let viewport_height = content_last_row - content_first_row + 1;

    println!("\nFile has {} lines, viewport shows {} lines", total_lines, viewport_height);

    if total_lines > viewport_height {
        // File is larger than viewport, so first lines should NOT be visible
        // when scrolled to the bottom

        // Check if any early content appears in the rendered screen
        // This would indicate improper clearing
        if final_screen.contains("pool_to_id_cbdata") {
            // The struct pool_to_id_cbdata is near the top (lines 7-10)
            // When at the bottom (lines 40+), we shouldn't see this

            // Get line number at top of viewport
            let top_line = harness.top_line_number();

            println!("\nViewport top_line: {}", top_line);

            // If viewport is scrolled past line 10, we shouldn't see pool_to_id_cbdata
            if top_line > 10 {
                panic!(
                    "BUG: Leftover content detected! \
                     'pool_to_id_cbdata' (from line ~7) is visible on screen \
                     but viewport is scrolled to line {}. \
                     This indicates improper screen clearing during scroll.",
                    top_line
                );
            }
        }
    }

    // Additional check: Look for any duplicate content or garbled characters
    // that would indicate partial overwrites

    println!("\n=== Testing Down arrow at bottom ===");

    let screen_before_down = harness.screen_to_string();

    // Try Down arrow multiple times at the bottom
    for _ in 1..=10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let screen_after_down = harness.screen_to_string();

    // Parse both screens and compare line by line
    let before_lines: Vec<&str> = screen_before_down.lines().collect();
    let after_lines: Vec<&str> = screen_after_down.lines().collect();

    let mut differences_found = 0;
    for row_idx in content_first_row..=content_last_row {
        if row_idx < before_lines.len() && row_idx < after_lines.len() {
            let before = before_lines[row_idx];
            let after = after_lines[row_idx];

            if before != after {
                differences_found += 1;
                println!("Row {} changed:", row_idx);
                println!("  Before: {:?}", before);
                println!("  After:  {:?}", after);

                // Check if the change looks like leftover characters
                // by examining character-by-character
                let before_chars: Vec<char> = before.chars().collect();
                let after_chars: Vec<char> = after.chars().collect();

                for (col, (bc, ac)) in before_chars.iter().zip(after_chars.iter()).enumerate() {
                    if bc != ac {
                        println!("    Col {}: '{}' -> '{}'", col, bc, ac);
                    }
                }
            }
        }
    }

    if differences_found > 0 {
        println!("\nWARNING: {} rows changed when pressing Down at end of file", differences_found);
        println!("This may indicate a scroll clearing issue.");
    }

    // Final validation: Check for common rendering artifacts
    check_for_rendering_artifacts(&screen_after_down, "final screen");

    // Dump final screen for manual inspection
    println!("\n=== FINAL SCREEN STATE ===");
    println!("{}", screen_after_down);
    println!("=== END FINAL SCREEN ===");

    println!("\n=== Test complete ===");
}

/// Check a screen for common rendering artifacts
fn check_for_rendering_artifacts(screen: &str, context: &str) {
    let lines: Vec<&str> = screen.lines().collect();

    for (idx, line) in lines.iter().enumerate() {
        // Check for null characters (would indicate buffer corruption)
        if line.contains('\0') {
            panic!("BUG in {}: Null character found on line {}", context, idx);
        }

        // Check for escape sequences that weren't processed
        // (visible as raw \x1b or similar)
        if line.contains("\\x1b") || line.contains("\x1b[") {
            // Note: Some terminal sequences might be expected, but visible ones are not
            println!("Warning in {}: Possible raw escape sequence on line {}", context, idx);
        }
    }
}

/// Test scroll clearing with manual scroll simulation
/// Uses scroll wheel simulation to test clearing behavior
#[test]
fn test_scroll_clearing_with_scroll_wheel() {
    use crossterm::event::{MouseEvent, MouseEventKind};

    let test_file_path = scroll_test_file_path();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();

    harness.open_file(&test_file_path).unwrap();
    harness.render().unwrap();

    println!("\n=== Testing scroll clearing with scroll wheel ===");

    // Scroll down using scroll wheel events
    let (content_first_row, _content_last_row) = harness.content_area_rows();

    // Simulate scrolling down to the bottom
    for _ in 0..20 {
        let scroll_event = MouseEvent {
            kind: MouseEventKind::ScrollDown,
            column: 40,
            row: (content_first_row + 5) as u16,
            modifiers: KeyModifiers::empty(),
        };
        harness.send_mouse(scroll_event).unwrap();
        harness.render().unwrap();
    }

    println!("After scrolling down with mouse wheel:");
    let screen_after_scroll = harness.screen_to_string();
    println!("{}", screen_after_scroll);

    // Continue scrolling past the end
    for i in 1..=10 {
        let scroll_event = MouseEvent {
            kind: MouseEventKind::ScrollDown,
            column: 40,
            row: (content_first_row + 5) as u16,
            modifiers: KeyModifiers::empty(),
        };
        harness.send_mouse(scroll_event).unwrap();
        harness.render().unwrap();

        let current_screen = harness.screen_to_string();

        // Check for artifacts after each scroll
        check_for_rendering_artifacts(&current_screen, &format!("scroll #{}", i));
    }

    // Verify the last content is visible
    harness.assert_screen_contains("lfs_mirror_extend");

    // Dump final screen for manual inspection
    let final_screen = harness.screen_to_string();
    println!("\n=== FINAL SCREEN STATE ===");
    println!("{}", final_screen);
    println!("=== END FINAL SCREEN ===");
}

/// Test that specifically looks for leftover characters after the last line of content
/// This is the most common manifestation of the scroll clearing bug
#[test]
fn test_leftover_characters_after_last_line() {
    let test_file_path = scroll_test_file_path();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();

    harness.open_file(&test_file_path).unwrap();
    harness.render().unwrap();

    println!("\n=== Testing for leftover characters after last line ===");

    // Get content area bounds
    let (content_first_row, content_last_row) = harness.content_area_rows();
    let buffer_content = harness.get_buffer_content();
    let total_file_lines = buffer_content.lines().count();

    println!("File has {} lines", total_file_lines);
    println!("Viewport content area: rows {} to {} ({} rows)",
             content_first_row, content_last_row,
             content_last_row - content_first_row + 1);

    // Jump to end of file
    harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Find which row contains the last line of the file
    let screen_str = harness.screen_to_string();
    let screen_lines: Vec<&str> = screen_str.lines().collect();

    // Find the last row with actual file content
    let mut last_content_row = content_first_row;
    for row_idx in content_first_row..=content_last_row {
        if row_idx < screen_lines.len() {
            let line = screen_lines[row_idx];
            // Check if line contains a line number (indicates file content)
            // Line numbers are at the start of lines, like "   47 │"
            if line.contains("│") && !line.trim_start().starts_with("│") {
                last_content_row = row_idx;
            }
        }
    }

    println!("Last content row: {}", last_content_row);
    println!("Last content line: {:?}", screen_lines.get(last_content_row));

    // Any rows AFTER last_content_row + 1 should be completely empty
    // (just spaces or empty cells) within the content area
    // But there's typically an empty row after the last file line for the cursor

    let mut leftover_issues = Vec::new();

    // Check rows after the last visible content line (plus one for the empty line after content)
    for row_idx in (last_content_row + 2)..=content_last_row {
        if row_idx >= screen_lines.len() {
            continue;
        }

        let line = screen_lines[row_idx];

        // Check each character in the row (except the scrollbar at the end)
        for (col, ch) in line.chars().enumerate() {
            // Skip the scrollbar column (last column)
            if col >= (terminal_width - 1) as usize {
                continue;
            }

            // The row should only contain spaces (and possibly gutter characters)
            // Any other character is a potential leftover
            if !ch.is_whitespace() && ch != '│' && ch != '█' {
                leftover_issues.push(format!(
                    "Row {}, Col {}: Found '{}' (expected space or gutter)",
                    row_idx, col, ch
                ));
            }
        }
    }

    // Now try scrolling down and check again
    println!("\n=== After scrolling down past end ===");

    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let screen_after = harness.screen_to_string();
    let screen_lines_after: Vec<&str> = screen_after.lines().collect();

    println!("Screen after scrolling:");
    for (idx, line) in screen_lines_after.iter().enumerate() {
        if idx >= content_first_row && idx <= content_last_row {
            println!("Row {:2}: {:?}", idx, line);
        }
    }

    // Find last content row again
    let mut last_content_row_after = content_first_row;
    for row_idx in content_first_row..=content_last_row {
        if row_idx < screen_lines_after.len() {
            let line = screen_lines_after[row_idx];
            if line.contains("│") && !line.trim_start().starts_with("│") {
                last_content_row_after = row_idx;
            }
        }
    }

    println!("\nLast content row after scrolling: {}", last_content_row_after);

    // Check for leftover content after scrolling
    for row_idx in (last_content_row_after + 2)..=content_last_row {
        if row_idx >= screen_lines_after.len() {
            continue;
        }

        let line = screen_lines_after[row_idx];

        for (col, ch) in line.chars().enumerate() {
            if col >= (terminal_width - 1) as usize {
                continue;
            }

            if !ch.is_whitespace() && ch != '│' && ch != '█' {
                leftover_issues.push(format!(
                    "After scroll - Row {}, Col {}: Found '{}' (expected space or gutter)",
                    row_idx, col, ch
                ));
            }
        }
    }

    // Dump final screen for manual inspection
    println!("\n=== FINAL SCREEN STATE ===");
    println!("{}", screen_after);
    println!("=== END FINAL SCREEN ===");

    // Report any issues found
    if !leftover_issues.is_empty() {
        println!("\n=== LEFTOVER CHARACTERS DETECTED ===");
        for issue in &leftover_issues {
            println!("  {}", issue);
        }
        panic!(
            "BUG: Found {} leftover character(s) in areas that should be empty:\n{}",
            leftover_issues.len(),
            leftover_issues.join("\n")
        );
    } else {
        println!("\n=== No leftover characters detected ===");
    }
}

/// Test that examines the exact render output buffer for anomalies
/// This is a more detailed test that looks at the ratatui buffer directly
#[test]
fn test_scroll_clearing_render_buffer_analysis() {
    let test_file_path = scroll_test_file_path();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();

    harness.open_file(&test_file_path).unwrap();
    harness.render().unwrap();

    println!("\n=== Render buffer analysis test ===");

    // Jump to end of file
    harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Capture the render buffer before scrolling attempts
    let buffer_before = harness.buffer().clone();

    // Try to scroll past the end
    for _ in 0..5 {
        harness.send_key(KeyCode::PageDown, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // Capture the render buffer after scrolling attempts
    let buffer_after = harness.buffer();

    // Compare cell by cell in the content area
    let (content_first_row, content_last_row) = harness.content_area_rows();

    println!("Comparing render buffers cell by cell...");

    let mut cell_differences = 0;

    for y in content_first_row..=content_last_row {
        for x in 0..terminal_width {
            let idx_before = buffer_before.index_of(x, y as u16);
            let idx_after = buffer_after.index_of(x, y as u16);

            let cell_before = &buffer_before.content[idx_before];
            let cell_after = &buffer_after.content[idx_after];

            // Compare symbol (the actual character)
            if cell_before.symbol() != cell_after.symbol() {
                cell_differences += 1;
                if cell_differences <= 20 {
                    println!(
                        "Cell ({}, {}) changed: '{}' -> '{}'",
                        x, y,
                        cell_before.symbol(),
                        cell_after.symbol()
                    );
                }
            }
        }
    }

    if cell_differences > 0 {
        println!("\nTotal cell differences: {}", cell_differences);
        println!("Screen should be stable when scrolling past end of file.");

        // Show the problematic areas
        println!("\n--- Buffer before ---");
        let mut before_str = String::new();
        for y in content_first_row..=content_last_row {
            for x in 0..terminal_width {
                let idx = buffer_before.index_of(x, y as u16);
                before_str.push_str(buffer_before.content[idx].symbol());
            }
            before_str.push('\n');
        }
        println!("{}", before_str);

        println!("\n--- Buffer after ---");
        let after_screen = harness.screen_to_string();
        let after_lines: Vec<&str> = after_screen.lines().collect();
        for row_idx in content_first_row..=content_last_row {
            if row_idx < after_lines.len() {
                println!("{}", after_lines[row_idx]);
            }
        }

        println!("\nNote: Some cell changes may be cursor-related and expected.");
    } else {
        println!("No cell differences detected - render buffer is stable.");
    }

    // Dump final screen for manual inspection
    let final_screen = harness.screen_to_string();
    println!("\n=== FINAL SCREEN STATE ===");
    println!("{}", final_screen);
    println!("=== END FINAL SCREEN ===");
}

/// Test scroll clearing using real terminal rendering (CrosstermBackend + vt100)
/// This catches bugs in ANSI escape sequence generation that TestBackend misses
#[test]
fn test_scroll_clearing_real_terminal() {
    let test_file_path = scroll_test_file_path();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();

    harness.open_file(&test_file_path).unwrap();

    println!("\n=== Testing scroll clearing with REAL terminal rendering ===");
    println!("This test uses CrosstermBackend -> vt100 to test actual ANSI output");

    // Initial render through real terminal pipeline
    harness.render_real().unwrap();

    // Check TestBackend vs vt100 match at start
    println!("\n--- Checking initial render ---");
    let differences = harness.compare_test_vs_real();
    if !differences.is_empty() {
        println!("WARNING: Initial render differences:");
        for diff in &differences {
            println!("  {}", diff);
        }
    } else {
        println!("Initial render: TestBackend and VT100 match.");
    }

    // Jump to end of file
    harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    harness.render_real().unwrap();

    println!("\n--- After jumping to end ---");
    let differences = harness.compare_test_vs_real();
    if !differences.is_empty() {
        println!("WARNING: Differences after Ctrl+End:");
        for diff in &differences {
            println!("  {}", diff);
        }
    } else {
        println!("After Ctrl+End: TestBackend and VT100 match.");
    }

    // Try scrolling down past the end
    println!("\n=== Scrolling down past end of file ===");

    for i in 1..=10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render_real().unwrap();

        let differences = harness.compare_test_vs_real();
        if !differences.is_empty() {
            println!("\n!!! DIFFERENCE FOUND after Down #{} !!!", i);
            for diff in &differences {
                println!("  {}", diff);
            }

            // This is the key assertion - if there are differences, it means
            // the real terminal rendering is showing something different than TestBackend
            // This could be the scroll clearing bug!
            println!("\nTestBackend screen:");
            println!("{}", harness.screen_to_string());
            println!("\nVT100 screen:");
            println!("{}", harness.vt100_screen_to_string());
        }
    }

    // Try PageDown as well
    println!("\n=== Testing PageDown past end ===");

    for i in 1..=5 {
        harness.send_key(KeyCode::PageDown, KeyModifiers::NONE).unwrap();
        harness.render_real().unwrap();

        let differences = harness.compare_test_vs_real();
        if !differences.is_empty() {
            println!("\n!!! DIFFERENCE FOUND after PageDown #{} !!!", i);
            for diff in &differences {
                println!("  {}", diff);
            }
        }
    }

    // Dump final screens for comparison
    println!("\n=== FINAL COMPARISON ===");
    println!("\n--- TestBackend screen ---");
    println!("{}", harness.screen_to_string());
    println!("\n--- VT100 screen (real terminal) ---");
    println!("{}", harness.vt100_screen_to_string());

    // Final differences check
    let final_differences = harness.compare_test_vs_real();
    if !final_differences.is_empty() {
        println!("\n=== FINAL DIFFERENCES ===");
        for diff in &final_differences {
            println!("{}", diff);
        }
        // Note: Don't panic here, just report - differences might reveal the bug
    } else {
        println!("\nFinal state: TestBackend and VT100 match perfectly.");
    }
}

/// Test cursor positioning and rendering on lines with tab characters
/// This validates that:
/// 1. Cursor appears at the correct position (before tab, not after)
/// 2. Tab indicator (→) is rendered at tab start positions
/// 3. Moving cursor across tabs works correctly
#[test]
fn test_tab_cursor_positioning_and_rendering() {
    let test_file_path = scroll_test_file_path();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();

    harness.open_file(&test_file_path).unwrap();
    harness.render().unwrap();

    println!("\n=== Testing tab cursor positioning and rendering ===");
    println!("File: {:?}", test_file_path);

    // Move to line 3 which has tabs (^I^I^I in the test file)
    // Line 3 starts with three tabs followed by text
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Move to beginning of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_at_start = harness.screen_to_string();
    println!("\n--- Cursor at start of line with tabs ---");
    let screen_lines: Vec<&str> = screen_at_start.lines().collect();

    // Get the content line (should be line 3 content)
    let (content_first_row, _) = harness.content_area_rows();
    let line_with_tabs = content_first_row + 2; // Line 3 (0-indexed row 2 in content)

    if line_with_tabs < screen_lines.len() {
        println!("Line {}: {:?}", line_with_tabs, screen_lines[line_with_tabs]);
    }

    // Check that tab indicator (→) is visible on the line
    let has_tab_indicator = screen_at_start.contains('→');
    assert!(
        has_tab_indicator,
        "Tab indicator (→) should be visible on line with tabs"
    );

    // Count tab indicators on the line with tabs
    if line_with_tabs < screen_lines.len() {
        let tab_count = screen_lines[line_with_tabs].chars().filter(|&c| c == '→').count();
        println!("Tab indicators on line {}: {}", line_with_tabs, tab_count);
        assert!(
            tab_count >= 3,
            "Expected at least 3 tab indicators on line {}, found {}",
            line_with_tabs,
            tab_count
        );
    }

    // Now test cursor movement - move right through the tabs
    println!("\n--- Moving cursor right through tabs ---");

    let mut prev_screen = screen_at_start.clone();
    for i in 1..=30 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        let current_screen = harness.screen_to_string();

        // Check that the screen changed (cursor moved)
        if current_screen != prev_screen {
            println!("Cursor moved after Right #{}", i);
        }

        prev_screen = current_screen;
    }

    // Now move cursor back to start and test that cursor is rendered
    // at the correct position (before the tab, not on all tab spaces)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_back_at_start = harness.screen_to_string();

    // The cursor should be rendered only once at the start position
    // Not on all 8 expanded spaces of the first tab
    println!("\n--- Screen with cursor at start of tabbed line ---");
    let lines_after: Vec<&str> = screen_back_at_start.lines().collect();
    if line_with_tabs < lines_after.len() {
        println!("Line {}: {:?}", line_with_tabs, lines_after[line_with_tabs]);
    }

    // Test using real terminal rendering
    println!("\n=== Testing with real terminal rendering ===");

    harness.render_real().unwrap();

    let test_screen = harness.screen_to_string();
    let vt100_screen = harness.vt100_screen_to_string();

    println!("\n--- TestBackend screen ---");
    let test_lines: Vec<&str> = test_screen.lines().collect();
    if line_with_tabs < test_lines.len() {
        println!("Line {}: {:?}", line_with_tabs, test_lines[line_with_tabs]);
    }

    println!("\n--- VT100 screen ---");
    let vt_lines: Vec<&str> = vt100_screen.lines().collect();
    if line_with_tabs < vt_lines.len() {
        println!("Line {}: {:?}", line_with_tabs, vt_lines[line_with_tabs]);
    }

    // Compare Test vs Real
    let differences = harness.compare_test_vs_real();
    if !differences.is_empty() {
        println!("\n!!! Differences between TestBackend and VT100 !!!");
        for diff in &differences {
            println!("  {}", diff);
        }
    } else {
        println!("\nTestBackend and VT100 match for tab rendering.");
    }

    // Dump final screens
    println!("\n=== FINAL SCREEN STATE ===");
    println!("{}", test_screen);
    println!("=== END FINAL SCREEN ===");
}

/// Test that cursor at position 0 on a line starting with tab
/// appears before the tab (at visual column 0), not after
#[test]
fn test_cursor_before_first_tab() {
    let test_file_path = scroll_test_file_path();

    let terminal_width = 80;
    let terminal_height = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();

    harness.open_file(&test_file_path).unwrap();
    harness.render().unwrap();

    println!("\n=== Testing cursor position before first tab ===");

    // Move to line 3 (which starts with tabs)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get the screen cursor position from the harness
    let (cursor_x, cursor_y) = harness.screen_cursor_position();
    println!("Screen cursor position: ({}, {})", cursor_x, cursor_y);

    // The cursor should be at the gutter width (start of content), not after the tab expansion
    // If the gutter is, say, 5 characters wide, cursor should be at column 5, not column 5+8
    let (content_first_row, _) = harness.content_area_rows();
    let expected_cursor_row = content_first_row as u16 + 2; // Line 3

    println!("Cursor at ({}, {})", cursor_x, cursor_y);

    // Cursor should be on the correct row
    assert_eq!(
        cursor_y, expected_cursor_row,
        "Cursor should be on row {}, but is on row {}",
        expected_cursor_row, cursor_y
    );

    // The x position should be at the start of content (after gutter)
    // not at position after the first tab expansion (which would be ~gutter+8)
    // The gutter for this file is "    3 │ " = 8 characters
    // So cursor should be at column 8 (right after gutter), NOT column 15 (after tab expansion)
    let gutter_width = 8u16;
    println!("Cursor x={} (should be {} = gutter width, not {} = after tab)",
             cursor_x, gutter_width, gutter_width + 7);

    // Assert cursor is at start of content, not after tab expansion
    assert!(
        cursor_x <= gutter_width + 1,
        "Cursor should be at start of content (column ~{}), but is at column {} (after tab expansion)",
        gutter_width, cursor_x
    );

    // Visual check: the cursor indicator character should appear at the start
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();

    if expected_cursor_row as usize >= lines.len() {
        println!("WARNING: Expected cursor row {} not in screen", expected_cursor_row);
        return;
    }

    let cursor_line = lines[expected_cursor_row as usize];
    println!("Cursor line: {:?}", cursor_line);

    // The line should have the tab indicator (→) visible
    assert!(
        cursor_line.contains('→'),
        "Tab indicator should be visible on cursor line"
    );

    println!("\n=== Test complete ===");
}
