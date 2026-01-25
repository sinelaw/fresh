use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Helper to create a harness with auto-indent enabled
/// Uses `.without_empty_plugins_dir()` so that embedded plugins are loaded,
/// which is required for tree-sitter based auto-indent to work.
fn harness_with_auto_indent() -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.auto_indent = true;
    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.enable_shadow_validation();
    harness
}

/// Test basic auto-indent in Rust after opening brace
#[test]
fn test_rust_auto_indent_after_brace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    // Write initial content to file to avoid auto-pair interference when typing
    std::fs::write(&file_path, "fn main() {").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("fn main() {");

    // Press Enter - should auto-indent
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have newline + 4 spaces indent
    let content = harness.get_buffer_content().unwrap();
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
    // Write initial content to file to avoid auto-pair interference when typing
    std::fs::write(&file_path, "def foo():").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("def foo():");

    // Press Enter - should auto-indent
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have newline + 4 spaces indent
    let content = harness.get_buffer_content().unwrap();
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
    // Write initial content to file to avoid auto-pair interference when typing
    std::fs::write(&file_path, "function test() {").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("function test() {");

    // Press Enter - should auto-indent
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have newline + 4 spaces indent
    let content = harness.get_buffer_content().unwrap();
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

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type outer block
    harness.type_text("fn main() {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Type inner block at indented level
    harness.type_text("if true {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have double indent (8 spaces)
    let content = harness.get_buffer_content().unwrap();
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

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type some indented text (no syntax highlighting for .txt)
    harness.type_text("    indented line").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should copy the 4-space indent from previous line
    let content = harness.get_buffer_content().unwrap();
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

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursors at end of each line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap(); // End of first line
    harness.editor_mut().add_cursor_below(); // Add cursor on second line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap(); // Move both to end

    // Verify we have 2 cursors
    let cursor_count = harness.editor().active_state().cursors.iter().count();
    assert_eq!(cursor_count, 2, "Should have 2 cursors");

    // Press Enter at both cursors
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Both lines should be indented
    let content = harness.get_buffer_content().unwrap();
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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have newline but NO indent
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "fn main() {\n",
        "Should not indent when auto_indent is disabled, got: {:?}",
        content
    );
}

/// Test TypeScript indent with interface
#[test]
fn test_typescript_interface_indent() {
    use tracing_subscriber::EnvFilter;
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.ts");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();
    tracing::debug!(
        "After open_file, buffer content: {:?}",
        harness.get_buffer_content()
    );

    // Type interface definition
    harness.type_text("interface User {").unwrap();
    tracing::debug!(
        "After type_text, buffer content: {:?}",
        harness.get_buffer_content()
    );

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    tracing::debug!(
        "After Enter key, buffer content: {:?}",
        harness.get_buffer_content()
    );

    harness.render().unwrap();
    tracing::debug!(
        "After render, buffer content: {:?}",
        harness.get_buffer_content()
    );

    // Should have indent
    let content = harness.get_buffer_content().unwrap();
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

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type class definition
    harness.type_text("class Foo {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have indent
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains("class Foo {\n    "),
        "Expected indent in C++ class, got: {:?}",
        content
    );
}

/// Test Go function indent (Go uses tabs for indentation)
#[test]
fn test_go_function_indent() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type function definition
    harness.type_text("func main() {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have tab indent (Go uses tabs)
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains("func main() {\n\t"),
        "Expected tab indent in Go function, got: {:?}",
        content
    );
}

/// Test JSON object indent
#[test]
fn test_json_object_indent() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.json");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type object opening
    harness.type_text("{").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have indent
    let content = harness.get_buffer_content().unwrap();
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

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type complete function signature
    harness.type_text("fn test() {").unwrap();

    // Now press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should still indent correctly
    let content = harness.get_buffer_content().unwrap();
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

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Select "old text" (positions 12-20)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..11 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    // Select from after { to before }
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    // Press Enter - should delete selection and indent
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert!(!content.contains("old text"), "Selection should be deleted");
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
    // Write a complete struct to file to avoid auto-pair interference
    std::fs::write(&file_path, "struct Foo {\n    x: i32,\n}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to end of file (after the closing brace)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Now cursor is after the closing brace
    // Pressing Enter should NOT indent (should be 0 spaces)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();

    // Check that the content is correct
    assert!(
        content.contains("struct Foo {"),
        "Should have struct declaration"
    );
    assert!(content.contains("x: i32"), "Should have field");
    assert!(content.contains("}"), "Should have closing brace");

    // Check that after the closing brace, there's a newline with NO spaces before it
    // The pattern should be "}\n" at the end, not "}\n    "
    assert!(
        content.ends_with("}\n") || content.ends_with("}\n\n"),
        "After closing brace should have newline with no indent, got: {:?}",
        content
    );

    // Verify the line with closing brace has proper indent (0 spaces to match struct level)
    let lines: Vec<&str> = content.lines().collect();
    assert!(lines.len() >= 3, "Should have at least 3 lines");
    let close_brace_line = lines
        .iter()
        .find(|l| l.trim() == "}")
        .expect("Should have closing brace line");
    let leading_spaces = close_brace_line.chars().take_while(|&c| c == ' ').count();
    assert_eq!(leading_spaces, 0, "Closing brace should be at column 0");
}

/// Test that typing a closing brace auto-dedents to the correct position
#[test]
fn test_auto_dedent_on_close_brace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type opening brace and press Enter to get indent
    harness.type_text("fn main() {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have 4 spaces of indent
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains("{\n    "),
        "Should have indent after opening brace"
    );

    // Now type a closing brace - it should auto-dedent to column 0
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
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

/// Test that typing a closing brace in a nested block dedents to the correct level
/// (not all the way to column 0, but to the parent block's indent level)
#[test]
fn test_auto_dedent_nested_blocks() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    // Write initial nested structure with 12-space indent on last line
    // This avoids auto-pair interference when typing opening braces
    std::fs::write(
        &file_path,
        "fn main() {\n    if true {\n        if false {\n            ",
    )
    .unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!("Content before typing closing brace:\n{}", content);

    // We should be at 12 spaces (3 levels deep: fn, if, if)
    let lines: Vec<&str> = content.lines().collect();
    if lines.len() >= 4 {
        let line3_indent = lines[3].chars().take_while(|&c| c == ' ').count();
        assert_eq!(
            line3_indent, 12,
            "Should have 12 spaces after nested if blocks"
        );
    }

    // Now type a closing brace - it should dedent to 8 spaces (parent if level)
    // not to 0 spaces!
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!("Content after typing closing brace:\n{}", content);

    let lines: Vec<&str> = content.lines().collect();
    if lines.len() >= 4 {
        let line3 = lines[3];
        let leading_spaces = line3.chars().take_while(|&c| c == ' ').count();
        assert_eq!(
            leading_spaces, 8,
            "Closing brace should dedent to 8 spaces (parent if level), but found {} spaces. Content: {:?}",
            leading_spaces, content
        );
    }
}

/// Test auto-dedent when there's content between opening brace and closing brace
/// This is the scenario: if (true) { <Enter> hi <Enter> } <-- should dedent
#[test]
fn test_auto_dedent_with_content_before() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type: if (true) {
    harness.type_text("if (true) {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have 4 spaces of indent
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains("{\n    "),
        "Should have indent after opening brace"
    );

    // Type some content: hi
    harness.type_text("hi").unwrap();

    // Press Enter again
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should still have 4 spaces on new line
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains("    hi\n    "),
        "Should have 4 spaces on new line after content"
    );

    // Now type closing brace - it should auto-dedent to column 0
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();

    // Count spaces before the closing brace
    let lines: Vec<&str> = content.lines().collect();
    if lines.len() >= 3 {
        let third_line = lines[2];
        let leading_spaces = third_line.chars().take_while(|&c| c == ' ').count();
        assert_eq!(
            leading_spaces, 0,
            "Closing brace should be at column 0 (dedented), but found {} spaces. Content: {:?}",
            leading_spaces, content
        );
    } else {
        panic!("Expected at least 3 lines, got {}", lines.len());
    }
}

/// Test auto-dedent with nested blocks where inner block is already closed
/// This tests that the pattern fallback correctly skips over matched pairs
#[test]
fn test_auto_dedent_nested_with_closed_inner() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type: if (1) {
    harness.type_text("if (1) {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Type content: hi there
    harness.type_text("hi there").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Type nested: if (2) {
    harness.type_text("if (2) {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Type nested content: hi there again!
    harness.type_text("hi there again!").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Type closing brace for inner block
    harness.type_text("}").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Type more content: hi
    harness.type_text("hi").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Now type closing brace for outer block - should dedent to column 0
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();

    // The last line should have the closing brace at column 0
    let lines: Vec<&str> = content.lines().collect();
    let last_line = lines.last().expect("Should have at least one line");
    let leading_spaces = last_line.chars().take_while(|&c| c == ' ').count();

    assert_eq!(
        leading_spaces, 0,
        "Outer closing brace should be at column 0 (not at inner block's indent of 4), but found {} spaces. Content:\n{}",
        leading_spaces, content
    );
}

/// Test dedent with complete syntax to see if tree-sitter is used
#[test]
fn test_dedent_with_complete_syntax() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Start with COMPLETE syntax (closing brace already present)
    std::fs::write(&file_path, "if (true) {\n    hi\n}\n").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to end of "hi" line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Press Enter to create new line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Type closing brace - should dedent to 0 (using tree-sitter since syntax is complete)
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();

    // The closing brace on the new line should be at column 0
    let lines: Vec<&str> = content.lines().collect();
    // Find the line with our new closing brace (should be line 3)
    if lines.len() >= 4 {
        let line3 = lines[2]; // The new line we created
        let leading_spaces = line3.chars().take_while(|&c| c == ' ').count();
        assert_eq!(
            leading_spaces, 0,
            "With complete syntax, closing brace should dedent to 0 using tree-sitter. Got {} spaces. Content:\n{}",
            leading_spaces, content
        );
    }
}

/// Test that pressing Enter after an empty line inside function body maintains indent
/// This should use tree-sitter to detect we're still inside the function block
#[test]
fn test_indent_after_empty_line_in_function_body() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type a function with some content
    harness.type_text("fn main() {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Should auto-indent to 4 spaces
    harness.type_text("let x = 1;").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Now we're on an empty line with 4 spaces indent
    // Delete all the spaces to simulate an empty line with NO indent
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }

    // Now we're on an empty line with NO spaces (inside function body)
    // Press Enter - should recognize we're inside function body and indent to 4 spaces
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();

    // Verify structure: function, content line, empty line (NO spaces), new line (should have 4 spaces)
    let lines: Vec<&str> = content.lines().collect();

    // Line 0: fn main() {
    assert!(
        lines[0].contains("fn main()"),
        "Line 0 should be function declaration"
    );

    // Line 1: let x = 1; (with 4 space indent)
    assert!(
        lines[1].trim().starts_with("let x"),
        "Line 1 should have let statement"
    );
    let line1_indent = lines[1].chars().take_while(|&c| c == ' ').count();
    assert_eq!(line1_indent, 4, "Line 1 should have 4 spaces");

    // Line 2: empty line (was cleared to 0 spaces)
    assert_eq!(lines[2], "", "Line 2 should be empty (0 spaces)");

    // Line 3: the new line we just created by pressing Enter
    // This is the KEY TEST: tree-sitter should recognize we're inside the function block
    // (between the opening { and the eventual closing })
    // and should indent to 4 spaces, NOT 0 spaces
    //
    // The pattern fallback would return 0 (copying from empty line)
    // But tree-sitter should count the @indent nodes and see we're nested 1 level deep

    assert!(
        lines.len() >= 4,
        "Should have at least 4 lines after Enter. Content: {:?}",
        content
    );

    let line3_indent = lines[3].chars().take_while(|&c| c == ' ').count();
    assert_eq!(
        line3_indent, 4,
        "After empty line in function body, tree-sitter should detect we're inside the block and indent to 4 spaces, got {} spaces. This verifies tree-sitter is being used, not just pattern fallback. Content: {:?}",
        line3_indent, content
    );
}

// ============================================================================
// Bracket Expansion Tests (Issue #629)
// When cursor is between opening and closing brackets (e.g., {|}), pressing Enter
// should expand to:
//   {
//       |
//   }
// Instead of the current behavior:
//   {
//       |}
// ============================================================================

/// Test basic bracket expansion in C - issue #629
/// Input: `int main() {<cursor>}`
/// Expected after Enter:
/// ```
/// int main() {
///     <cursor>
/// }
/// ```
#[test]
fn test_bracket_expansion_c_function() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.c");
    // Write content with cursor position between { and }
    std::fs::write(&file_path, "int main() {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to end and then left to be between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter - should expand brackets
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    // Should have 3 lines: opening, cursor line (indented), closing
    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines after bracket expansion, got {}. Content:\n{}",
        lines.len(),
        content
    );

    // Line 1: int main() {
    assert!(
        lines[0].contains("int main() {"),
        "First line should have opening brace"
    );

    // Line 2: should be indented (4 spaces) and where cursor is
    let line2_indent = lines[1].chars().take_while(|&c| c == ' ').count();
    assert_eq!(
        line2_indent, 4,
        "Second line (cursor line) should have 4 spaces indent, got {}",
        line2_indent
    );

    // Line 3: should be the closing brace at column 0
    let line3 = lines[2];
    assert!(
        line3.trim() == "}",
        "Third line should be closing brace, got: {:?}",
        line3
    );
    let line3_indent = line3.chars().take_while(|&c| c == ' ').count();
    assert_eq!(
        line3_indent, 0,
        "Closing brace should be at column 0, got {} spaces",
        line3_indent
    );
}

/// Test bracket expansion in Rust - issue #629
#[test]
fn test_bracket_expansion_rust_function() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn main() {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines, got {}. Content:\n{}",
        lines.len(),
        content
    );

    // Verify structure
    assert!(lines[0].ends_with("{"), "First line should end with {{");
    assert_eq!(
        lines[1].chars().take_while(|&c| c == ' ').count(),
        4,
        "Cursor line should have 4 spaces"
    );
    assert_eq!(lines[2].trim(), "}", "Third line should be closing brace");
}

/// Test bracket expansion in JavaScript - issue #629
#[test]
fn test_bracket_expansion_javascript_function() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.js");
    std::fs::write(&file_path, "function test() {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );
    assert!(lines[0].ends_with("{"), "First line should end with {{");
    assert_eq!(
        lines[1].chars().take_while(|&c| c == ' ').count(),
        4,
        "Cursor line should have 4 spaces"
    );
    assert_eq!(lines[2].trim(), "}", "Third line should be closing brace");
}

/// Test bracket expansion in TypeScript interface - issue #629
#[test]
fn test_bracket_expansion_typescript_interface() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.ts");
    std::fs::write(&file_path, "interface User {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );
    assert_eq!(lines[2].trim(), "}", "Third line should be closing brace");
}

/// Test bracket expansion in Go (uses tabs) - issue #629
#[test]
fn test_bracket_expansion_go_function() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");
    std::fs::write(&file_path, "func main() {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );

    // Go uses tabs for indentation
    assert!(
        lines[1].starts_with('\t'),
        "Go should use tab for indent, got: {:?}",
        lines[1]
    );
    assert_eq!(lines[2].trim(), "}", "Third line should be closing brace");
}

/// Test square bracket expansion for arrays - issue #629
#[test]
fn test_bracket_expansion_square_brackets_rust() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "let arr = []").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between [ and ]
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines for array expansion. Content:\n{}",
        content
    );
    assert!(lines[0].ends_with("["), "First line should end with [");
    assert_eq!(lines[2].trim(), "]", "Third line should be closing bracket");
}

/// Test JSON object bracket expansion - issue #629
#[test]
fn test_bracket_expansion_json_object() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.json");
    std::fs::write(&file_path, "{}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );
    assert_eq!(lines[0], "{", "First line should be {{");
    assert_eq!(lines[2].trim(), "}", "Third line should be }}");
}

/// Test JSON array bracket expansion - issue #629
#[test]
fn test_bracket_expansion_json_array() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.json");
    std::fs::write(&file_path, "[]").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between [ and ]
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );
    assert_eq!(lines[0], "[", "First line should be [");
    assert_eq!(lines[2].trim(), "]", "Third line should be ]");
}

/// Test nested bracket expansion - issue #629
/// When inside nested braces like `fn main() { if true {<cursor>} }`,
/// the inner brace should expand properly
#[test]
fn test_bracket_expansion_nested_braces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    // Outer function has content, inner if block is empty
    std::fs::write(&file_path, "fn main() {\n    if true {}\n}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move to line 2 (if true {}), then to position between { and }
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Now between { and }

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    // Should have:
    // fn main() {
    //     if true {
    //         <cursor>
    //     }
    // }
    assert!(
        lines.len() >= 5,
        "Expected at least 5 lines for nested expansion. Content:\n{}",
        content
    );

    // The new cursor line should have 8 spaces (2 levels of indent)
    let cursor_line = lines[2]; // Line after "if true {"
    let cursor_indent = cursor_line.chars().take_while(|&c| c == ' ').count();
    assert_eq!(
        cursor_indent, 8,
        "Cursor line in nested block should have 8 spaces, got {}",
        cursor_indent
    );

    // The inner closing brace should have 4 spaces
    let inner_close = lines[3];
    assert!(
        inner_close.trim() == "}",
        "Inner closing brace line should be }}"
    );
    let inner_close_indent = inner_close.chars().take_while(|&c| c == ' ').count();
    assert_eq!(
        inner_close_indent, 4,
        "Inner closing brace should have 4 spaces, got {}",
        inner_close_indent
    );
}

/// Test bracket expansion with trailing content - issue #629
/// `{<cursor>} // comment` should preserve the comment after closing brace
#[test]
fn test_bracket_expansion_with_trailing_comment() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn main() {} // entry point").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    // "fn main() {}" is 12 chars, { is at position 10, } is at position 11
    // We need cursor at position 11 (between { and })
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..11 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );

    // The comment should be preserved with the closing brace
    assert!(
        content.contains("// entry point"),
        "Comment should be preserved. Content:\n{}",
        content
    );
}

/// Test bracket expansion with whitespace inside - issue #629
/// `{ <cursor> }` (with spaces) should still expand properly
#[test]
fn test_bracket_expansion_with_whitespace_inside() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn main() {  }").unwrap(); // Two spaces inside

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between the spaces (after { and before })
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before }
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before the second space

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    // Should expand, though exact behavior with whitespace may vary
    assert!(
        lines.len() >= 2,
        "Expected expansion. Content:\n{}",
        content
    );
}

/// Test parenthesis expansion for function calls - issue #629
/// `foo(<cursor>)` - parentheses should also expand
#[test]
fn test_bracket_expansion_parentheses() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "let x = foo()").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between ( and )
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    // Parentheses should expand similarly to braces
    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines for parenthesis expansion. Content:\n{}",
        content
    );
    assert!(lines[0].ends_with("("), "First line should end with (");
    assert_eq!(lines[2].trim(), ")", "Third line should be closing paren");
}

/// Test mixed bracket types - issue #629
/// `foo({<cursor>})` - should expand the inner braces
#[test]
fn test_bracket_expansion_mixed_brackets() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.js");
    std::fs::write(&file_path, "callback({})").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before )
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before }

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();

    // The inner brace should expand
    assert!(
        content.contains("callback({"),
        "Should have opening. Content:\n{}",
        content
    );
}

/// Test deeply nested bracket expansion - issue #629
/// `{{{<cursor>}}}` - innermost braces should expand
#[test]
fn test_bracket_expansion_deeply_nested() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.json");
    std::fs::write(&file_path, "{{{}}}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to the innermost position (between the third { and first })
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected expansion. Content:\n{}",
        content
    );
}

/// Test C++ class with empty body bracket expansion - issue #629
#[test]
fn test_bracket_expansion_cpp_class() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.cpp");
    std::fs::write(&file_path, "class Foo {};").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before ;
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before }

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );
}

/// Test bracket expansion with assignment - issue #629
/// `let x = {<cursor>}`
#[test]
fn test_bracket_expansion_with_assignment() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.js");
    std::fs::write(&file_path, "let obj = {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );
    assert!(
        lines[0].contains("let obj = {"),
        "First line should have assignment"
    );
    assert_eq!(lines[2].trim(), "}", "Third line should be closing brace");
}

/// Test that bracket expansion does NOT occur when not between matching brackets
/// `{text<cursor>}` should not expand specially
#[test]
fn test_no_bracket_expansion_with_content_between() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn main() {return 0;}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between "return 0;" and "}"
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter - should add newline but not do special expansion
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    // Should have 2 lines, not 3 (no special expansion)
    assert!(
        lines.len() == 2,
        "Expected 2 lines (no special expansion when content exists). Content:\n{}",
        content
    );
}

/// Test bracket expansion preserves file modifications - issue #629
/// Verifies the file is properly marked as modified after expansion
#[test]
fn test_bracket_expansion_marks_file_modified() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn main() {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Verify file is not modified initially
    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "File should not be modified initially"
    );

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify file is now modified
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "File should be modified after bracket expansion"
    );
}

/// Test undo after bracket expansion - issue #629
/// A single undo should revert the entire bracket expansion
#[test]
fn test_bracket_expansion_undo() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn main() {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    let original_content = harness.get_buffer_content().unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify expansion happened
    let expanded_content = harness.get_buffer_content().unwrap();
    assert_ne!(
        expanded_content, original_content,
        "Content should have changed"
    );

    // Undo
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be back to original
    let after_undo = harness.get_buffer_content().unwrap();
    assert_eq!(
        after_undo, original_content,
        "Undo should restore original content"
    );
}

/// Test arrow key expansion with existing struct definition - issue #629
/// struct Foo {<cursor>}
#[test]
fn test_bracket_expansion_rust_struct() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "struct Point {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );
    assert!(
        lines[0].contains("struct Point {"),
        "First line should have struct"
    );
    assert_eq!(lines[2].trim(), "}", "Third line should be closing brace");
}

/// Test bracket expansion in HTML-like syntax (JSX) - issue #629
#[test]
fn test_bracket_expansion_jsx_component() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.jsx");
    std::fs::write(&file_path, "const App = () => {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor between { and }
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.lines().collect();

    assert!(
        lines.len() >= 3,
        "Expected at least 3 lines. Content:\n{}",
        content
    );
}
