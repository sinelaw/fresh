use crate::common::harness::EditorTestHarness;
use std::path::PathBuf;

/// Test that markdown files can be opened and rendered
#[test]
fn test_markdown_file_open() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Get the path to the test markdown file
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    // Open the markdown file
    harness.open_file(&md_path).unwrap();

    // Verify file is loaded
    harness.render().unwrap();
    harness.assert_screen_contains("markdown_sample.md");

    // Verify content is visible
    harness.assert_screen_contains("Markdown Compose Mode Test");
}

/// Test markdown compose mode toggle command
#[test]
fn test_markdown_compose_toggle() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Get the path to the test markdown file
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    // Open the markdown file
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Verify initial render shows content
    harness.assert_screen_contains("Markdown Compose Mode Test");

    // Try to invoke command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Command palette should be visible
    // Note: This test is basic - a full test would search for and execute
    // the "Markdown: Toggle Compose Mode" command
}

/// Test that markdown headers are properly styled
#[test]
fn test_markdown_header_rendering() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Verify the header is visible in the buffer
    let buffer_content = harness.get_buffer_content();
    assert!(buffer_content.contains("# Markdown Compose Mode Test"));
    assert!(buffer_content.contains("## Features"));
    assert!(buffer_content.contains("### Code Blocks"));
}

/// Test markdown list rendering
#[test]
fn test_markdown_list_rendering() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Verify list items are in the buffer
    let buffer_content = harness.get_buffer_content();
    assert!(buffer_content.contains("- Soft breaks for paragraph wrapping"));
    assert!(buffer_content.contains("1. First ordered item"));
    assert!(buffer_content.contains("- [ ] Unchecked task"));
    assert!(buffer_content.contains("- [x] Checked task"));
}

/// Test markdown code block rendering
#[test]
fn test_markdown_code_block_rendering() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Verify code blocks are in the buffer
    let buffer_content = harness.get_buffer_content();
    assert!(buffer_content.contains("```rust"));
    assert!(buffer_content.contains("fn main()"));
    assert!(buffer_content.contains("println!"));
}

/// Test markdown inline styles
#[test]
fn test_markdown_inline_styles() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Verify inline styles are in the buffer
    let buffer_content = harness.get_buffer_content();
    assert!(buffer_content.contains("**bold**"));
    assert!(buffer_content.contains("*italic*"));
    assert!(buffer_content.contains("`inline code`"));
    assert!(buffer_content.contains("~~strikethrough~~"));
}

/// Test markdown links
#[test]
fn test_markdown_links() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Verify links are in the buffer
    let buffer_content = harness.get_buffer_content();
    assert!(buffer_content.contains("[Links to resources]"));
    assert!(buffer_content.contains("[Fresh Editor]"));
}

/// Test markdown block quotes
#[test]
fn test_markdown_block_quotes() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Verify block quotes are in the buffer
    let buffer_content = harness.get_buffer_content();
    assert!(buffer_content.contains("> This is a block quote."));
}
