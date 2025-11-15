// Focused test to reproduce the auto-indent bug found by property testing
mod common;

use common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn test_enter_after_brace_no_autoindent() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.enable_shadow_validation();

    // Type a brace
    harness.type_text("{").unwrap();

    // Press Enter
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Should be just "{\n", not "{\n    " (no auto-indent)
    let buffer = harness.get_buffer_content();
    let shadow = harness.get_shadow_string();

    println!("Buffer: {:?}", buffer);
    println!("Shadow: {:?}", shadow);

    assert_eq!(buffer, shadow, "Buffer should match shadow (no auto-indent)");
    assert_eq!(buffer, "{\n", "Buffer should be just brace and newline");
}

#[test]
fn test_simple_sequence_from_e2e() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.enable_shadow_validation();

    // Simplified from test_basic_editing_operations
    harness.type_text("Hello").unwrap();
    harness.type_text("World").unwrap();

    // Move left 5 times to get back to between Hello and World
    for _ in 0..5 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }

    // Press Enter to create newline
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    let buffer = harness.get_buffer_content();
    let shadow = harness.get_shadow_string();

    println!("Buffer: {:?}", buffer);
    println!("Shadow: {:?}", shadow);

    assert_eq!(buffer, shadow, "Buffer should match shadow");
    assert_eq!(buffer, "Hello\nWorld", "Buffer should have newline between words");
}

#[test]
fn test_minimal_proptest_failure() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.enable_shadow_validation();

    println!("\n=== Step-by-step debugging ===");

    // Step 1: Enter
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    println!("After Enter: buffer={:?}, cursor={}", harness.get_buffer_content(), harness.cursor_position());

    // Step 2: Type "a0"
    harness.type_text("a0").unwrap();
    println!("After 'a0': buffer={:?}, cursor={}", harness.get_buffer_content(), harness.cursor_position());

    // Step 3: Left
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    println!("After Left: buffer={:?}, cursor={}", harness.get_buffer_content(), harness.cursor_position());

    // Step 4: Home
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    println!("After Home: buffer={:?}, cursor={}", harness.get_buffer_content(), harness.cursor_position());

    // Step 5: Type "b"
    harness.type_text("b").unwrap();
    println!("After 'b': buffer={:?}, cursor={}", harness.get_buffer_content(), harness.cursor_position());

    let buffer = harness.get_buffer_content();
    let shadow = harness.get_shadow_string();

    println!("\n=== Final state ===");
    println!("Buffer: {:?}", buffer);
    println!("Shadow: {:?}", shadow);
    println!("Cursor: {}", harness.cursor_position());

    assert_eq!(buffer, shadow, "Buffer should match shadow");
    assert_eq!(buffer, "\nba0", "Should be newline, b, a, 0");
}
