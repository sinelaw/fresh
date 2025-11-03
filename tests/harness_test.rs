// Test the EditorTestHarness itself

mod common;

use common::harness::EditorTestHarness;

#[test]
fn test_harness_creation() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    assert!(!harness.should_quit());
}

#[test]
fn test_harness_render() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(!screen.is_empty());
}

#[test]
fn test_buffer_content() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    let content = harness.get_buffer_content();
    assert_eq!(content, ""); // New buffer is empty
}

#[test]
fn test_screen_contains() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Should show status bar with "[No Name]"
    harness.assert_screen_contains("[No Name]");
}
