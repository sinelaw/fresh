
use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test that editor doesn't quit prematurely
#[test]
fn test_editor_lifecycle() {
    let harness = EditorTestHarness::new(80, 24).unwrap();

    // New editor should not want to quit
    assert!(!harness.should_quit());

    // TODO: When action_to_events() is implemented:
    // - Send quit command
    // - Verify should_quit() returns true
}
