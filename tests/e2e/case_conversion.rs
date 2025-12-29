use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test converting selected text to uppercase with Ctrl+Shift+U
#[test]
fn test_to_uppercase() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text
    harness.type_text("hello world").unwrap();

    // Move to start of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Select the word "hello" using Shift+Right (5 times)
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    // Verify we have "hello" selected
    let selected = harness.get_selected_text();
    assert_eq!(selected, "hello", "Should have 'hello' selected");

    // Convert to uppercase with Ctrl+Shift+U
    harness
        .send_key(
            KeyCode::Char('u'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Verify the text was converted to uppercase
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "HELLO world",
        "Selected text should be converted to uppercase"
    );
}

/// Test converting selected text to lowercase with Ctrl+Shift+L
#[test]
fn test_to_lowercase() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text with uppercase
    harness.type_text("HELLO WORLD").unwrap();

    // Move to start of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Select the word "HELLO" using Shift+Right (5 times)
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    // Verify we have "HELLO" selected
    let selected = harness.get_selected_text();
    assert_eq!(selected, "HELLO", "Should have 'HELLO' selected");

    // Convert to lowercase with Ctrl+Shift+L
    harness
        .send_key(
            KeyCode::Char('l'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Verify the text was converted to lowercase
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "hello WORLD",
        "Selected text should be converted to lowercase"
    );
}

/// Test case conversion with no selection (should do nothing)
#[test]
fn test_case_conversion_no_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text
    harness.type_text("hello world").unwrap();

    // Don't select anything, just move cursor to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify no selection
    harness.assert_no_selection();

    // Try to convert to uppercase (should do nothing)
    harness
        .send_key(
            KeyCode::Char('u'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Text should remain unchanged
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "hello world",
        "Text should remain unchanged when no selection"
    );
}

/// Test converting entire line to uppercase
#[test]
fn test_to_uppercase_entire_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text
    harness.type_text("hello world test").unwrap();

    // Select entire line with Ctrl+L
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Convert to uppercase
    harness
        .send_key(
            KeyCode::Char('u'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Verify the entire line was converted
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "HELLO WORLD TEST",
        "Entire line should be converted to uppercase"
    );
}

/// Test converting mixed case text
#[test]
fn test_to_uppercase_mixed_case() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type mixed case text
    harness.type_text("HeLLo WoRLd").unwrap();

    // Select all with Ctrl+A
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Convert to uppercase
    harness
        .send_key(
            KeyCode::Char('u'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Verify conversion
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "HELLO WORLD",
        "Mixed case text should be converted to uppercase"
    );
}

/// Test converting mixed case text to lowercase
#[test]
fn test_to_lowercase_mixed_case() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type mixed case text
    harness.type_text("HeLLo WoRLd").unwrap();

    // Select all with Ctrl+A
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Convert to lowercase
    harness
        .send_key(
            KeyCode::Char('l'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Verify conversion
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "hello world",
        "Mixed case text should be converted to lowercase"
    );
}

/// Test case conversion with special characters (should preserve them)
#[test]
fn test_case_conversion_with_special_chars() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text with special characters
    harness.type_text("hello_world-123!").unwrap();

    // Select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Convert to uppercase
    harness
        .send_key(
            KeyCode::Char('u'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Special characters should be preserved
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "HELLO_WORLD-123!",
        "Special characters should be preserved during case conversion"
    );
}

/// Test case conversion with Unicode characters
#[test]
fn test_case_conversion_unicode() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text with Unicode characters that have case variants
    harness.type_text("café résumé").unwrap();

    // Select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Convert to uppercase
    harness
        .send_key(
            KeyCode::Char('u'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Unicode characters should be properly converted
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "CAFÉ RÉSUMÉ",
        "Unicode characters should be properly converted to uppercase"
    );
}

/// Test case conversion preserves cursor position
#[test]
fn test_case_conversion_preserves_cursor() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text
    harness.type_text("hello world").unwrap();

    // Move to start and select "hello"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    // Get cursor position before conversion
    let cursor_before = harness.cursor_position();

    // Convert to uppercase
    harness
        .send_key(
            KeyCode::Char('u'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Get cursor position after conversion
    let cursor_after = harness.cursor_position();

    // Cursor should be at same position (end of selection)
    assert_eq!(
        cursor_before, cursor_after,
        "Cursor position should be preserved after case conversion"
    );

    // Verify the conversion happened
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(buffer_content, "HELLO world");
}

/// Test case conversion with multi-line selection
#[test]
fn test_case_conversion_multiline() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type multiple lines
    harness.type_text("hello\nworld\ntest").unwrap();

    // Select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Convert to uppercase
    harness
        .send_key(
            KeyCode::Char('u'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // All lines should be converted
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "HELLO\nWORLD\nTEST",
        "Multi-line text should be converted to uppercase"
    );
}

/// Test undo after case conversion
#[test]
fn test_case_conversion_undo() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text
    harness.type_text("hello world").unwrap();

    // Select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Convert to uppercase
    harness
        .send_key(
            KeyCode::Char('u'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // Verify conversion
    let buffer_after_convert = harness.get_buffer_content().unwrap();
    assert_eq!(buffer_after_convert, "HELLO WORLD");

    // Undo with Ctrl+Z
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Text should be restored
    let buffer_after_undo = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_after_undo, "hello world",
        "Undo should restore original text after case conversion"
    );
}

/// Test that case conversion works from command palette
#[test]
fn test_case_conversion_from_command_palette() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text
    harness.type_text("hello world").unwrap();

    // Select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Open command palette with Ctrl+Shift+P
    harness
        .send_key(
            KeyCode::Char('p'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Command Palette"))
        .unwrap();

    // Type to search for uppercase command
    harness.type_text("uppercase").unwrap();
    harness.render().unwrap();

    // The command should appear in the palette
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Uppercase") || screen.contains("uppercase"),
        "Command palette should show uppercase command. Screen:\n{}",
        screen
    );

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the text was converted
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "HELLO WORLD",
        "Text should be converted to uppercase via command palette"
    );
}
