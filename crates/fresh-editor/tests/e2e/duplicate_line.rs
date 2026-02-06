use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test duplicating a single line via command palette
/// Issue #591: Duplicate line or selected lines
#[test]
fn test_duplicate_line_basic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type a single line
    harness.type_text("hello world").unwrap();
    harness.render().unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .unwrap();

    // Search for duplicate line command
    harness.type_text("duplicate line").unwrap();
    harness.render().unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The line should be duplicated below
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "hello world\nhello world",
        "Line should be duplicated below"
    );
}

/// Test duplicating a line that ends with newline
#[test]
fn test_duplicate_line_with_newline() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type multiple lines, cursor on first line
    harness.type_text("first\nsecond\nthird").unwrap();

    // Move to start of file
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .unwrap();

    // Execute duplicate line
    harness.type_text("duplicate line").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // First line should be duplicated
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "first\nfirst\nsecond\nthird",
        "First line should be duplicated"
    );
}

/// Test duplicating selected lines
#[test]
fn test_duplicate_selected_lines() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type multiple lines
    harness
        .type_text("line one\nline two\nline three\nline four")
        .unwrap();

    // Move to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Move down to second line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Select two lines (shift+down twice)
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .unwrap();

    // Execute duplicate line
    harness.type_text("duplicate line").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Lines two and three should be duplicated
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("line two\nline three\nline two\nline three"),
        "Selected lines should be duplicated. Got: {}",
        buffer_content
    );
}

/// Test undo after duplicate line
#[test]
fn test_duplicate_line_undo() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello world").unwrap();
    harness.render().unwrap();

    // Duplicate
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .unwrap();
    harness.type_text("duplicate line").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify duplicated
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(buffer_content, "hello world\nhello world");

    // Undo
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be back to original
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "hello world",
        "Undo should restore original content"
    );
}
