//! Tests for Toggle Comment functionality
//!
//! Tests that:
//! - Toggle comment uses language-specific comment prefixes from config
//! - Selection is preserved after commenting/uncommenting

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Helper to run a command from the command palette
fn run_command(harness: &mut EditorTestHarness, command_name: &str) {
    // Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type the command name
    harness.type_text(command_name).unwrap();
    harness.render().unwrap();

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test that Toggle Comment uses // for Rust files
#[test]
fn test_toggle_comment_rust_prefix() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn main() {\n    println!(\"hello\");\n}").unwrap();

    let config = Config::default();
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Toggle comment on first line
    run_command(&mut harness, "Toggle Comment");

    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.starts_with("// fn main()"),
        "Rust files should use // for comments. Got: {:?}",
        content
    );
}

/// Test that Toggle Comment uses # for Python files
#[test]
fn test_toggle_comment_python_prefix() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.py");
    std::fs::write(&file_path, "def main():\n    print(\"hello\")\n").unwrap();

    let config = Config::default();
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Toggle comment on first line
    run_command(&mut harness, "Toggle Comment");

    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.starts_with("# def main()"),
        "Python files should use # for comments. Got: {:?}",
        content
    );
}

/// Test that Toggle Comment uses # for shell files
#[test]
fn test_toggle_comment_shell_prefix() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.sh");
    std::fs::write(&file_path, "echo hello\n").unwrap();

    let config = Config::default();
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Toggle comment on first line
    run_command(&mut harness, "Toggle Comment");

    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.starts_with("# echo"),
        "Shell files should use # for comments. Got: {:?}",
        content
    );
}

/// Test that selection is preserved after commenting multiple lines
#[test]
fn test_toggle_comment_preserves_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "line1\nline2\nline3\nline4").unwrap();

    let config = Config::default();
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move to start of file
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Select lines 1 and 2 (Shift+Down twice)
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Verify we have a selection before commenting
    let cursor_before = *harness.editor().active_state().cursors.primary();
    assert!(
        cursor_before.selection_range().is_some(),
        "Should have selection before toggle comment"
    );
    let selection_before = cursor_before.selection_range().unwrap();
    let selection_len_before = selection_before.end - selection_before.start;

    // Toggle comment
    run_command(&mut harness, "Toggle Comment");

    // Verify content is commented
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.starts_with("// line1\n// line2"),
        "First two lines should be commented. Got: {:?}",
        content
    );

    // Verify selection is still active after commenting
    let cursor_after = *harness.editor().active_state().cursors.primary();
    assert!(
        cursor_after.selection_range().is_some(),
        "Selection should be preserved after toggle comment"
    );

    // Selection should have grown by the added comment prefixes (2 lines * 3 chars "// ")
    let selection_after = cursor_after.selection_range().unwrap();
    let selection_len_after = selection_after.end - selection_after.start;
    assert!(
        selection_len_after > selection_len_before,
        "Selection should have grown after adding comments. Before: {}, After: {}",
        selection_len_before,
        selection_len_after
    );
}

/// Test that selection is preserved after uncommenting multiple lines
#[test]
fn test_toggle_uncomment_preserves_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "// line1\n// line2\n// line3\nline4").unwrap();

    let config = Config::default();
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move to start of file
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Select lines 1 and 2 (Shift+Down twice)
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Verify we have a selection before uncommenting
    let cursor_before = *harness.editor().active_state().cursors.primary();
    assert!(
        cursor_before.selection_range().is_some(),
        "Should have selection before toggle comment"
    );

    // Toggle comment (uncomment)
    run_command(&mut harness, "Toggle Comment");

    // Verify content is uncommented
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.starts_with("line1\nline2"),
        "First two lines should be uncommented. Got: {:?}",
        content
    );

    // Verify selection is still active after uncommenting
    let cursor_after = *harness.editor().active_state().cursors.primary();
    assert!(
        cursor_after.selection_range().is_some(),
        "Selection should be preserved after toggle uncomment"
    );
}

/// Test that commenting and uncommenting is idempotent with selection
#[test]
fn test_toggle_comment_roundtrip_with_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    let original_content = "line1\nline2\nline3";
    std::fs::write(&file_path, original_content).unwrap();

    let config = Config::default();
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Select all lines
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Toggle comment (comment all)
    run_command(&mut harness, "Toggle Comment");

    // Verify content is commented
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains("// line1")
            && content.contains("// line2")
            && content.contains("// line3"),
        "All lines should be commented. Got: {:?}",
        content
    );

    // Re-select all lines (selection may have been modified)
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Toggle comment again (uncomment all)
    run_command(&mut harness, "Toggle Comment");

    // Verify content is back to original
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, original_content,
        "After comment/uncomment roundtrip, content should match original. Got: {:?}",
        content
    );
}

/// Test that toggle comment works on a single line file without trailing newline
/// This is a regression test for an infinite loop bug when selection end equals buffer length
#[test]
fn test_toggle_comment_single_line_no_newline() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.c");
    // No trailing newline - this was causing infinite loop
    std::fs::write(&file_path, "int main() {}").unwrap();

    let config = Config::default();
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Select all (Ctrl+A)
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify we have a selection
    let cursor = *harness.editor().active_state().cursors.primary();
    assert!(
        cursor.selection_range().is_some(),
        "Should have selection after Ctrl+A"
    );

    // Toggle comment - this was causing infinite loop
    run_command(&mut harness, "Toggle Comment");

    // Verify content is commented
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.starts_with("// int main()"),
        "C file should be commented with //. Got: {:?}",
        content
    );
}

/// Test toggle comment on file with selection at exact buffer end
#[test]
fn test_toggle_comment_selection_at_buffer_end() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    // Multiple lines, no trailing newline
    std::fs::write(&file_path, "fn foo() {}\nfn bar() {}").unwrap();

    let config = Config::default();
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Toggle comment
    run_command(&mut harness, "Toggle Comment");

    // Verify both lines are commented
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains("// fn foo()") && content.contains("// fn bar()"),
        "Both lines should be commented. Got: {:?}",
        content
    );
}
