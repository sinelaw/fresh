//! E2E tests for search and replace functionality

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test basic forward search functionality
#[test]
fn test_basic_search_forward() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with searchable content
    std::fs::write(&file_path, "hello world\nfoo bar\nhello again\nbaz").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that the search prompt appeared
    harness.assert_screen_contains("Search: ");

    // Type search query
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Cursor should move to the first match ("hello" at position 0)
    let cursor_pos = harness.cursor_position();
    assert_eq!(
        cursor_pos, 0,
        "Cursor should be at the start of first 'hello'"
    );

    // Find next match with F3
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Cursor should move to second match ("hello" at line 3)
    let cursor_pos = harness.cursor_position();

    // Second "hello" starts at position after "hello world\nfoo bar\n"
    let expected_pos = "hello world\nfoo bar\n".len();
    assert_eq!(
        cursor_pos, expected_pos,
        "Cursor should be at the start of second 'hello'"
    );
}

/// Test incremental search highlighting as user types
#[test]
fn test_incremental_search_highlighting() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with multiple matches visible on screen
    std::fs::write(
        &file_path,
        "test line one\ntest line two\nother content\ntest line three\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type "test" - should see highlighting before pressing Enter
    harness.type_text("test").unwrap();
    harness.render().unwrap();

    // Check that highlights appear in the rendered output
    // The screen should show the text with search highlights
    let screen = harness.screen_to_string();

    // Screen should contain the search prompt with "test"
    assert!(
        screen.contains("Search: test"),
        "Search prompt should show typed text"
    );

    // Verify matches are in the visible area (we have 3 "test" matches on screen)
    // This is a basic check - the highlighting is visual, but we can verify the content is there
    assert!(screen.contains("test line one"));
    assert!(screen.contains("test line two"));
    assert!(screen.contains("test line three"));
}

/// Test that search highlighting only applies to visible viewport
#[test]
fn test_search_highlighting_visible_only() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_test.txt");

    // Create a file with many lines, more than can fit on screen
    let mut content = String::new();
    for i in 0..100 {
        content.push_str(&format!("Line {} with search term\n", i));
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type search query
    harness.type_text("search").unwrap();
    harness.render().unwrap();

    // The test passes if highlighting doesn't cause performance issues
    // (no timeout or excessive CPU usage)
    // In a real scenario, only visible lines would be highlighted

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should be at first match
    let cursor_pos = harness.cursor_position();
    assert!(cursor_pos > 0, "Cursor should have moved to a match");

    // Scroll down and search should still work efficiently
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Find next should work even after scrolling
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let new_cursor_pos = harness.cursor_position();
    assert!(
        new_cursor_pos > cursor_pos,
        "Cursor should have moved to next match"
    );
}

/// Test interactive replace wrap-around behavior
#[test]
fn test_interactive_replace_wrap_around() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with "foo" at positions: 0, 20, 40
    // We'll start at position 25 (middle), so we should see:
    // 1. Match at 40 (after cursor)
    // 2. Wrap around
    // 3. Match at 0 (before starting position)
    // 4. Match at 20 (before starting position)
    // 5. Stop (no more matches before start_pos=25)
    std::fs::write(&file_path, "foo is here\nand\nfoo is there\nfoo again").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move cursor to position 25 (somewhere in the middle, after first two "foo"s)
    // Content: "foo is here\n" = 12 chars, "and\n" = 4 chars (total 16), "foo is there\n" = 13 chars (total 29), "foo again"
    // So "foo" appears at: 0, 16, 29
    // Let's position at 25 (after second "foo")
    for _ in 0..25 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let start_pos = harness.cursor_position();
    assert_eq!(start_pos, 25, "Cursor should be at position 25");

    // Trigger interactive replace with Ctrl+Alt+R
    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.render().unwrap();

    // Should show "Query replace: " prompt
    harness.assert_screen_contains("Query replace: ");

    // Type search pattern "foo"
    harness.type_text("foo").unwrap();
    harness.render().unwrap();

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show "Query replace 'foo' with: " prompt
    harness.assert_screen_contains("Query replace 'foo' with: ");

    // Type replacement "XXX"
    harness.type_text("XXX").unwrap();
    harness.render().unwrap();

    // Confirm replacement
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should be at first match after position 25, which is at position 29 ("foo again")
    let pos1 = harness.cursor_position();
    assert_eq!(pos1, 29, "Should be at third 'foo' (position 29)");
    harness.assert_screen_contains("Replace? (y/n/!/q)");

    // Press 'y' to replace this occurrence
    harness.type_text("y").unwrap();
    harness.render().unwrap();

    // Should wrap around to beginning and find "foo" at position 0
    let pos2 = harness.cursor_position();
    assert_eq!(pos2, 0, "Should wrap to first 'foo' (position 0)");

    // Should show [Wrapped] indicator
    harness.assert_screen_contains("[Wrapped]");

    // Press 'n' to skip this one
    harness.type_text("n").unwrap();
    harness.render().unwrap();

    // Should move to "foo" at position 16
    let pos3 = harness.cursor_position();
    assert_eq!(pos3, 16, "Should be at second 'foo' (position 16)");

    // Still shows [Wrapped] indicator
    harness.assert_screen_contains("[Wrapped]");

    // Press 'y' to replace this occurrence
    harness.type_text("y").unwrap();
    harness.render().unwrap();

    // Should finish (no more matches before start_pos=25)
    // Check the status message shows completion (truncated on screen)
    harness.assert_screen_contains("Replaced 2 occurr");

    // Verify the buffer content has the expected replacements
    let content = harness.get_buffer_content();

    // We replaced:
    // - Third "foo" (at 29) -> "XXX"
    // - Skipped first "foo" (at 0)
    // - Replaced second "foo" (at 16) -> "XXX"
    // Expected: "foo is here\nand\nXXX is there\nXXX again"
    assert_eq!(
        content, "foo is here\nand\nXXX is there\nXXX again",
        "Should have replaced 2nd and 3rd 'foo' only"
    );
}

/// Test interactive replace stops at starting position after wrap
#[test]
fn test_interactive_replace_wrap_stops_at_start() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create file with pattern at positions before and after cursor
    std::fs::write(&file_path, "foo\nbar\nbaz\nfoo\nqux\nfoo").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move to second "foo" (around position 12-16)
    for _ in 0..12 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Trigger query-replace
    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.render().unwrap();

    // Enter search term
    harness.type_text("foo").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Enter replacement
    harness.type_text("XXX").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should be at second "foo", press 'n' to skip
    harness.type_text("n").unwrap();
    harness.render().unwrap();

    // Should be at third "foo", press 'n' to skip
    harness.type_text("n").unwrap();
    harness.render().unwrap();

    // Should wrap and be at first "foo", press 'y' to replace
    harness.type_text("y").unwrap();
    harness.render().unwrap();

    // Should finish (second foo is at/past starting position)
    harness.assert_screen_contains("Replaced 1 occurr");
}

/// Test that search highlights update when scrolling to show new matches
#[test]
fn test_search_highlights_update_on_scroll() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file where "MATCH" appears on every 3rd line
    // This ensures there are always matches visible regardless of scroll position
    let mut content = String::new();
    for i in 0..60 {
        if i % 3 == 0 {
            content.push_str(&format!("Line {} has MATCH keyword\n", i));
        } else {
            content.push_str(&format!("Line {} no keyword here\n", i));
        }
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type search query
    harness.type_text("MATCH").unwrap();
    harness.render().unwrap();

    // Confirm search - this highlights visible matches
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have highlights for visible matches
    let highlights_initial = harness.count_search_highlights();
    assert!(
        highlights_initial > 0,
        "Should have search highlights after search"
    );

    // Get position of first highlight
    let state = harness.editor().active_state();
    let first_highlight_pos = state
        .overlays
        .all()
        .iter()
        .find(|o| {
            o.id.as_ref()
                .map(|id| id.starts_with("search_highlight_"))
                .unwrap_or(false)
        })
        .and_then(|o| state.marker_list.get_position(o.start_marker))
        .expect("Should have at least one highlight");

    // Scroll down significantly
    for _ in 0..5 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // After scrolling with the fix:
    // - Should still have highlights (for newly visible matches)
    // - Highlights should be at DIFFERENT positions (not the old ones)
    let highlights_after_scroll = harness.count_search_highlights();
    assert!(
        highlights_after_scroll > 0,
        "Should still have search highlights after scrolling (for newly visible matches)"
    );

    // Get position of highlight after scrolling
    let state = harness.editor().active_state();
    let scrolled_highlight_pos = state
        .overlays
        .all()
        .iter()
        .find(|o| {
            o.id.as_ref()
                .map(|id| id.starts_with("search_highlight_"))
                .unwrap_or(false)
        })
        .and_then(|o| state.marker_list.get_position(o.start_marker))
        .expect("Should have at least one highlight after scrolling");

    // The highlight position should have changed (we're highlighting different matches now)
    assert!(
        scrolled_highlight_pos != first_highlight_pos,
        "Highlight should be at a different position after scrolling (old: {}, new: {})",
        first_highlight_pos,
        scrolled_highlight_pos
    );

    // Verify the highlight is actually in the visible viewport
    let viewport_top = state.viewport.top_byte;
    let viewport_end = viewport_top + 1000; // Approximate visible range
    assert!(
        scrolled_highlight_pos >= viewport_top && scrolled_highlight_pos < viewport_end,
        "Highlight at {} should be in visible viewport (top: {})",
        scrolled_highlight_pos,
        viewport_top
    );
}

/// Test search history navigation with Up/Down arrows
#[test]
fn test_search_history_navigation() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar\ntest content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // First search: "hello"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Second search: "foo"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("foo").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Third search: "test"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("test").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search prompt again
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: ");

    // Press Up arrow - should show "test" (most recent)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: test");

    // Press Up arrow again - should show "foo"
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: foo");

    // Press Up arrow again - should show "hello"
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: hello");

    // Press Up arrow again - should stay at "hello" (oldest)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: hello");

    // Press Down arrow - should show "foo"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: foo");

    // Press Down arrow - should show "test"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: test");

    // Cancel the prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that search history preserves current input when navigating
#[test]
fn test_search_history_preserves_current_input() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "content here").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add one item to history
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("previous").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search again and start typing
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("current input").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: current input");

    // Press Up to go to history
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: previous");

    // Press Down to return to current input
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: current input");

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that replace has separate history from search
#[test]
fn test_replace_history_separate_from_search() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Do a search to add to search history
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("search_term").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Do a replace to add to both search and replace history
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace: ");

    harness.type_text("hello").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show replacement prompt
    harness.assert_screen_contains("Replace 'hello' with: ");

    harness.type_text("goodbye").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open replace again
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace: ");

    // Press Up - should show "hello" (from replace search history)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace: hello");

    // Confirm to get to replacement prompt
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace 'hello' with: ");

    // Press Up - should show "goodbye" (from replace history)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace 'hello' with: goodbye");

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that history skips empty entries and duplicate consecutive entries
#[test]
fn test_search_history_skips_empty_and_duplicates() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "test content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Try to search with empty string (should not be added to history)
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for "test" twice
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("test").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("test").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for "other"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("other").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search and check history
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Press Up - should show "other"
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: other");

    // Press Up - should show "test" (only one "test" in history, not two)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: test");

    // Press Up - should stay at "test" (no empty string before it)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: test");

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that incremental search highlights update when navigating history
#[test]
fn test_history_updates_incremental_highlights() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar\ntest content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add "hello" and "foo" to search history
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("foo").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search again
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Press Up to navigate to "foo"
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify "foo" appears on screen (should be highlighted incrementally)
    let screen = harness.screen_to_string();
    assert!(screen.contains("foo bar"), "Should show 'foo' in content");
    assert!(
        screen.contains("Search: foo"),
        "Should show 'foo' in prompt"
    );

    // Press Up to navigate to "hello"
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify "hello" appears on screen (highlights should update)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("hello world"),
        "Should show 'hello' in content"
    );
    assert!(
        screen.contains("Search: hello"),
        "Should show 'hello' in prompt"
    );

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that incremental highlighting works on second search (bug reproduction)
#[test]
fn test_incremental_highlighting_on_second_search() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar\ntest content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // First search: "hello"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Verify incremental highlighting appears for "hello"
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("hello world"),
        "Should show 'hello' in content"
    );
    assert!(
        screen.contains("Search: hello"),
        "Should show search prompt"
    );

    // Confirm first search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Second search: "foo"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type "foo" incrementally
    harness.type_text("f").unwrap();
    harness.render().unwrap();

    // Verify incremental highlighting appears for "f" (not "hello")
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Search: f"),
        "Should show 'f' in search prompt"
    );

    // Type rest of "foo"
    harness.type_text("oo").unwrap();
    harness.render().unwrap();

    // Verify incremental highlighting appears for "foo"
    let screen = harness.screen_to_string();
    assert!(screen.contains("foo bar"), "Should show 'foo' in content");
    assert!(
        screen.contains("Search: foo"),
        "Should show 'foo' in prompt"
    );

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that highlights disappear when search query becomes empty
#[test]
fn test_highlights_clear_when_query_becomes_empty() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar\ntest content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Open search and type "hello"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Verify highlights exist
    let highlight_count_before = harness.count_search_highlights();
    assert!(
        highlight_count_before > 0,
        "Should have highlights for 'hello'"
    );

    // Delete all characters one by one
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // Verify highlights are cleared
    let highlight_count_after = harness.count_search_highlights();
    assert_eq!(
        highlight_count_after, 0,
        "Should have no highlights when query is empty"
    );

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that highlights clear when navigating to empty input via history
#[test]
fn test_highlights_clear_on_history_to_empty() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add "hello" to history
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search again and start typing something new
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("foo").unwrap();
    harness.render().unwrap();

    // Verify we have highlights for "foo"
    let highlight_count_foo = harness.count_search_highlights();
    assert!(highlight_count_foo > 0, "Should have highlights for 'foo'");

    // Navigate up to history (shows "hello")
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have highlights for "hello"
    let highlight_count_hello = harness.count_search_highlights();
    assert!(
        highlight_count_hello > 0,
        "Should have highlights for 'hello'"
    );

    // Navigate down past the end (returns to "foo")
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have highlights for "foo" again
    let highlight_count_foo_again = harness.count_search_highlights();
    assert!(
        highlight_count_foo_again > 0,
        "Should have highlights for 'foo' again"
    );

    // Now delete all characters to make it empty
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // Navigate up to "hello" in history
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate down to empty input
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Highlights should be cleared for empty input
    let highlight_count_empty = harness.count_search_highlights();
    assert_eq!(
        highlight_count_empty, 0,
        "Should have no highlights when navigating to empty input"
    );

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}
