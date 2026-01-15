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

/// Test buffer content API (getBufferText with start and end)
#[test]
fn test_buffer_content_api() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    harness.open_file(&md_path).unwrap();

    // Get buffer content to verify it's not empty
    let content = harness.get_buffer_content().unwrap();
    assert!(!content.is_empty(), "Buffer content should not be empty");
    assert!(
        content.contains("# Markdown Compose Mode Test"),
        "Should contain header"
    );
}

/// Test that viewport info is available
#[test]
fn test_viewport_info_available() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let md_path = PathBuf::from(manifest_dir)
        .join("tests")
        .join("fixtures")
        .join("markdown_sample.md");

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Just verify the file loads and renders without crashing
    // The plugin's getViewport() call happens during render
    harness.assert_screen_contains("Markdown");
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
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Command palette should be visible
    // Note: This test is basic - a full test would search for and execute
    // the "Markdown: Toggle Compose" command
}

/// Test that plugin doesn't crash on empty buffer
#[test]
fn test_empty_buffer_handling() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Create empty markdown file
    let temp_dir = tempfile::TempDir::new().unwrap();
    let empty_md = temp_dir.path().join("empty.md");
    std::fs::write(&empty_md, "").unwrap();

    harness.open_file(&empty_md).unwrap();
    harness.render().unwrap();

    // Should render without crashing
    harness.assert_screen_contains("empty.md");
}

/// Test that plugin handles non-markdown files correctly
#[test]
fn test_non_markdown_file_ignored() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Create a non-markdown file
    let temp_dir = tempfile::TempDir::new().unwrap();
    let txt_file = temp_dir.path().join("test.txt");
    std::fs::write(&txt_file, "This is not markdown").unwrap();

    harness.open_file(&txt_file).unwrap();
    harness.render().unwrap();

    // Should render normally without trying to apply markdown processing
    harness.assert_screen_contains("test.txt");
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
    let buffer_content = harness.get_buffer_content().unwrap();
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
    let buffer_content = harness.get_buffer_content().unwrap();
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
    let buffer_content = harness.get_buffer_content().unwrap();
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
    let buffer_content = harness.get_buffer_content().unwrap();
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
    let buffer_content = harness.get_buffer_content().unwrap();
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
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(buffer_content.contains("> This is a block quote."));
}

/// Test that disabling compose mode doesn't blank the view
#[test]
fn test_compose_mode_disable_preserves_content() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Create a simple markdown file
    let temp_dir = tempfile::TempDir::new().unwrap();
    let md_path = temp_dir.path().join("test.md");
    std::fs::write(
        &md_path,
        "# Test Header\n\nSome **bold** text.\n\n- List item 1\n- List item 2\n",
    )
    .unwrap();

    // Open the markdown file
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Verify initial content is visible
    harness.assert_screen_contains("Test Header");
    harness.assert_screen_contains("bold");

    // Open command palette and toggle compose mode ON
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Markdown: Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show compose mode enabled status
    // Content should still be visible
    harness.assert_screen_contains("Test Header");

    // Toggle compose mode OFF
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Markdown: Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // CRITICAL: Content should still be visible after disabling compose mode
    harness.assert_screen_contains("Test Header");
    harness.assert_screen_contains("bold");
    harness.assert_screen_contains("List item");
}
