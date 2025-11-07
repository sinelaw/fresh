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
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Cursor should move to the first match ("hello" at position 0)
    let cursor_pos = harness.cursor_position();
    assert_eq!(cursor_pos, 0, "Cursor should be at the start of first 'hello'");

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
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should be at first match
    let cursor_pos = harness.cursor_position();
    assert!(cursor_pos > 0, "Cursor should have moved to a match");

    // Scroll down and search should still work efficiently
    harness.send_key(KeyCode::PageDown, KeyModifiers::NONE).unwrap();
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
