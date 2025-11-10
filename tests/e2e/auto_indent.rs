use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Test basic auto-indent in Rust after opening brace
#[test]
fn test_rust_auto_indent_after_brace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type function signature with opening brace
    harness.type_text("fn main() {").unwrap();
    harness.assert_buffer_content("fn main() {");

    // Press Enter - should auto-indent
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have newline + 4 spaces indent
    let content = harness.get_buffer_content();
    assert!(
        content.contains("fn main() {\n    "),
        "Expected 4-space indent after opening brace, got: {:?}",
        content
    );
}

/// Test auto-indent in Python after colon
#[test]
fn test_python_auto_indent_after_colon() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.py");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type function definition with colon
    harness.type_text("def foo():").unwrap();
    harness.assert_buffer_content("def foo():");

    // Press Enter - should auto-indent
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have newline + 4 spaces indent
    let content = harness.get_buffer_content();
    assert!(
        content.contains("def foo():\n    "),
        "Expected 4-space indent after colon, got: {:?}",
        content
    );
}

/// Test auto-indent in JavaScript after opening brace
#[test]
fn test_javascript_auto_indent_after_brace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.js");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type function with opening brace
    harness.type_text("function test() {").unwrap();
    harness.assert_buffer_content("function test() {");

    // Press Enter - should auto-indent
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have newline + 4 spaces indent
    let content = harness.get_buffer_content();
    assert!(
        content.contains("function test() {\n    "),
        "Expected 4-space indent after opening brace, got: {:?}",
        content
    );
}

/// Test auto-indent with nested blocks
#[test]
fn test_rust_nested_indent() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type outer block
    harness.type_text("fn main() {").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Type inner block at indented level
    harness.type_text("if true {").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have double indent (8 spaces)
    let content = harness.get_buffer_content();
    assert!(
        content.contains("if true {\n        "),
        "Expected 8-space indent for nested block, got: {:?}",
        content
    );
}

/// Test auto-indent preserves existing indent when no tree-sitter info
#[test]
fn test_fallback_copies_previous_indent() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type some indented text (no syntax highlighting for .txt)
    harness.type_text("    indented line").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should copy the 4-space indent from previous line
    let content = harness.get_buffer_content();
    assert!(
        content.contains("    indented line\n    "),
        "Expected fallback to copy 4-space indent, got: {:?}",
        content
    );
}

/// Test auto-indent with multi-cursor
#[test]
fn test_auto_indent_with_multi_cursor() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn foo() {\nfn bar() {").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Position cursors at end of each line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap(); // End of first line
    harness.editor_mut().add_cursor_below(); // Add cursor on second line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap(); // Move both to end

    // Verify we have 2 cursors
    let cursor_count = harness.editor().active_state().cursors.iter().count();
    assert_eq!(cursor_count, 2, "Should have 2 cursors");

    // Press Enter at both cursors
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Both lines should be indented
    let content = harness.get_buffer_content();
    assert!(
        content.contains("fn foo() {\n    "),
        "First function should have indent, got: {:?}",
        content
    );
    assert!(
        content.contains("fn bar() {\n    "),
        "Second function should have indent, got: {:?}",
        content
    );
}

/// Test that auto_indent config flag can disable the feature
#[test]
fn test_auto_indent_disabled_by_config() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    // Create harness with auto_indent disabled
    let mut config = Config::default();
    config.editor.auto_indent = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type function with opening brace
    harness.type_text("fn main() {").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have newline but NO indent
    let content = harness.get_buffer_content();
    assert_eq!(
        content, "fn main() {\n",
        "Should not indent when auto_indent is disabled, got: {:?}",
        content
    );
}

/// Test TypeScript indent with interface
#[test]
fn test_typescript_interface_indent() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.ts");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type interface definition
    harness.type_text("interface User {").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have indent
    let content = harness.get_buffer_content();
    assert!(
        content.contains("interface User {\n    "),
        "Expected indent in TypeScript interface, got: {:?}",
        content
    );
}

/// Test C++ class indent
#[test]
fn test_cpp_class_indent() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.cpp");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type class definition
    harness.type_text("class Foo {").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have indent
    let content = harness.get_buffer_content();
    assert!(
        content.contains("class Foo {\n    "),
        "Expected indent in C++ class, got: {:?}",
        content
    );
}

/// Test Go function indent
#[test]
fn test_go_function_indent() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type function definition
    harness.type_text("func main() {").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have indent
    let content = harness.get_buffer_content();
    assert!(
        content.contains("func main() {\n    "),
        "Expected indent in Go function, got: {:?}",
        content
    );
}

/// Test JSON object indent
#[test]
fn test_json_object_indent() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.json");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type object opening
    harness.type_text("{").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have indent
    let content = harness.get_buffer_content();
    assert!(
        content.contains("{\n    "),
        "Expected indent in JSON object, got: {:?}",
        content
    );
}

/// Test that indent works correctly after typing and then pressing Enter
#[test]
fn test_indent_after_typing_on_same_line() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type complete function signature
    harness.type_text("fn test() {").unwrap();

    // Now press Enter
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should still indent correctly
    let content = harness.get_buffer_content();
    assert!(
        content.contains("fn test() {\n    "),
        "Expected indent even after typing complete line, got: {:?}",
        content
    );
}

/// Test indent with selection (should delete selection then indent)
#[test]
fn test_indent_with_selection_deletes_first() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn main() {old text}").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Select "old text" (positions 12-20)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..11 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }
    // Select from after { to before }
    for _ in 0..8 {
        harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    }

    // Press Enter - should delete selection and indent
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content();
    assert!(
        !content.contains("old text"),
        "Selection should be deleted"
    );
    assert!(
        content.contains("fn main() {\n    "),
        "Should indent after deleting selection, got: {:?}",
        content
    );
}

/// Test that pressing Enter after a closing brace doesn't indent
#[test]
fn test_no_indent_after_close_brace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type a complete struct
    harness.type_text("struct Foo {").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // Auto-indent should give us 4 spaces
    harness.type_text("x: i32,").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // Should maintain 4 spaces, now type closing brace
    harness.type_text("}").unwrap();

    // Now cursor is after the closing brace
    // Pressing Enter should NOT indent (should be 0 spaces)
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content();

    // Check that the content is correct
    assert!(content.contains("struct Foo {"), "Should have struct declaration");
    assert!(content.contains("x: i32"), "Should have field");
    assert!(content.contains("}"), "Should have closing brace");

    // Check that after the closing brace, there's a newline with NO spaces before it
    // The pattern should be "}\n" at the end, not "}\n    "
    assert!(content.ends_with("}\n") || content.ends_with("}\n\n"),
            "After closing brace should have newline with no indent, got: {:?}", content);

    // Verify the line with closing brace has proper indent (0 spaces to match struct level)
    // Auto-dedent should have moved it to column 0
    let lines: Vec<&str> = content.lines().collect();
    assert!(lines.len() >= 3, "Should have at least 3 lines");
    let close_brace_line = lines.iter().find(|l| l.trim() == "}").expect("Should have closing brace line");
    let leading_spaces = close_brace_line.chars().take_while(|&c| c == ' ').count();
    assert_eq!(leading_spaces, 0, "Closing brace should be at column 0 (auto-dedented)");
}

/// Test that typing a closing brace auto-dedents to the correct position
#[test]
fn test_auto_dedent_on_close_brace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type opening brace and press Enter to get indent
    harness.type_text("fn main() {").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have 4 spaces of indent
    let content = harness.get_buffer_content();
    assert!(content.contains("{\n    "), "Should have indent after opening brace");

    // Now type a closing brace - it should auto-dedent to column 0
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content();
    assert!(
        content.contains("{\n}") || content.contains("{\n    }"),
        "Closing brace should dedent to column 0, got: {:?}",
        content
    );

    // Count spaces before the closing brace
    let lines: Vec<&str> = content.lines().collect();
    if lines.len() >= 2 {
        let second_line = lines[1];
        let leading_spaces = second_line.chars().take_while(|&c| c == ' ').count();
        assert_eq!(
            leading_spaces, 0,
            "Closing brace should be at column 0, but found {} spaces",
            leading_spaces
        );
    }
}
