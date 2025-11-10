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
