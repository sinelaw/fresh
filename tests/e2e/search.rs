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
