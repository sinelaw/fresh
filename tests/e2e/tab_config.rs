//! Tests for per-language tab configuration options:
//! - `show_whitespace_tabs`: Whether to display tab indicators (→) in the editor
//! - `use_tabs`: Whether pressing Tab inserts a tab character or spaces

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

// =============================================================================
// show_whitespace_tabs Tests
// =============================================================================

/// Test that tab characters are rendered with → indicator by default
#[test]
fn test_show_whitespace_tabs_default_shows_arrow() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create a file with a tab character
    std::fs::write(&file_path, "\thello").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen content:\n{}", screen);

    // The tab should be rendered with the → indicator
    harness.assert_screen_contains("→");
    harness.assert_screen_contains("hello");
}

/// Test that tab characters in Go files do NOT show → indicator
/// (Go convention is to use tabs for indentation, so we hide the indicators)
#[test]
fn test_show_whitespace_tabs_go_hides_arrow() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create a Go file with tab characters
    std::fs::write(
        &file_path,
        "\tpackage main\n\n\tfunc main() {\n\t\tfmt.Println(\"hello\")\n\t}",
    )
    .unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen content for Go file:\n{}", screen);

    // The tab should NOT be rendered with the → indicator
    harness.assert_screen_not_contains("→");
    // But the content should still be visible
    harness.assert_screen_contains("package main");
    harness.assert_screen_contains("func main");
}

/// Test that Makefile tabs show → indicator by default
/// (Even though use_tabs is true, show_whitespace_tabs is also true for Makefile)
#[test]
fn test_show_whitespace_tabs_makefile_shows_arrow() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("Makefile");

    // Create a Makefile with tab-indented recipe
    std::fs::write(&file_path, "all:\n\techo \"hello\"").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen content for Makefile:\n{}", screen);

    // Makefile has show_whitespace_tabs: true, so tab indicator should be visible
    harness.assert_screen_contains("→");
    harness.assert_screen_contains("echo");
}

/// Test that custom language config can disable tab indicators
#[test]
fn test_show_whitespace_tabs_custom_config_disables() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create a file with a tab character
    std::fs::write(&file_path, "\thello").unwrap();

    // Create config with show_whitespace_tabs disabled for Rust
    let mut config = Config::default();
    if let Some(rust_config) = config.languages.get_mut("rust") {
        rust_config.show_whitespace_tabs = false;
    }

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen content with custom config:\n{}", screen);

    // With show_whitespace_tabs disabled, no → should appear
    harness.assert_screen_not_contains("→");
    harness.assert_screen_contains("hello");
}

// =============================================================================
// use_tabs Tests
// =============================================================================

/// Test that pressing Tab in a Rust file inserts spaces (default behavior)
#[test]
fn test_use_tabs_rust_inserts_spaces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create an empty Rust file
    std::fs::write(&file_path, "").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Press Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get buffer content - should be spaces, not tab
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Tab: {:?}", content);

    // Default tab_size is 4, so we expect 4 spaces
    assert_eq!(content, "    ", "Tab should insert 4 spaces in Rust files");
    assert!(
        !content.contains('\t'),
        "Buffer should NOT contain tab character"
    );
}

/// Test that pressing Tab in a Go file inserts a tab character
#[test]
fn test_use_tabs_go_inserts_tab_character() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create an empty Go file
    std::fs::write(&file_path, "").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Press Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get buffer content - should be a tab character
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Tab in Go file: {:?}", content);

    assert_eq!(content, "\t", "Tab should insert tab character in Go files");
    assert!(
        content.contains('\t'),
        "Buffer should contain tab character"
    );
}

/// Test that pressing Tab in a Makefile inserts a tab character
#[test]
fn test_use_tabs_makefile_inserts_tab_character() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("Makefile");

    // Create a Makefile with a target line
    std::fs::write(&file_path, "all:\n").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to end of file (after the newline on the recipe line)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();

    // Press Tab to indent the recipe
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get buffer content
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Tab in Makefile: {:?}", content);

    assert!(
        content.contains('\t'),
        "Makefile should contain tab character after pressing Tab"
    );
    assert_eq!(
        content, "all:\n\t",
        "Tab should insert tab character in Makefile"
    );
}

/// Test that saving a file preserves tab vs spaces correctly for Go
#[test]
fn test_use_tabs_go_saved_file_contains_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create an empty Go file
    std::fs::write(&file_path, "").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type some code with tabs
    harness.type_text("package main").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("func main() {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap(); // This should insert a tab
    harness.type_text("println()").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Read the saved file and verify it contains actual tab characters
    let saved_content = std::fs::read_to_string(&file_path).unwrap();
    println!("Saved Go file content: {:?}", saved_content);

    assert!(
        saved_content.contains('\t'),
        "Saved Go file should contain tab character"
    );
    assert!(
        saved_content.contains("\tprintln()"),
        "Tab should be before println()"
    );
}

/// Test that saving a Rust file preserves spaces (not tabs)
#[test]
fn test_use_tabs_rust_saved_file_contains_spaces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create an empty Rust file
    std::fs::write(&file_path, "").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type some code with tabs
    harness.type_text("fn main() {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap(); // This should insert spaces
    harness.type_text("println!();").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Read the saved file and verify it contains spaces, not tabs
    let saved_content = std::fs::read_to_string(&file_path).unwrap();
    println!("Saved Rust file content: {:?}", saved_content);

    assert!(
        !saved_content.contains('\t'),
        "Saved Rust file should NOT contain tab character"
    );
    assert!(
        saved_content.contains("    println!();"),
        "Should have 4 spaces before println!"
    );
}

/// Test custom config can enable use_tabs for a language that normally uses spaces
#[test]
fn test_use_tabs_custom_config_enables_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create an empty Rust file
    std::fs::write(&file_path, "").unwrap();

    // Create config with use_tabs enabled for Rust
    let mut config = Config::default();
    if let Some(rust_config) = config.languages.get_mut("rust") {
        rust_config.use_tabs = true;
    }

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Press Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get buffer content - should be a tab character due to custom config
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content with custom config: {:?}", content);

    assert_eq!(
        content, "\t",
        "Tab should insert tab character with custom use_tabs config"
    );
}

/// Test that multiple tabs work correctly in Go files
#[test]
fn test_use_tabs_go_multiple_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create an empty Go file
    std::fs::write(&file_path, "").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Press Tab three times
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get buffer content - should be three tab characters
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after 3 tabs in Go: {:?}", content);

    assert_eq!(content, "\t\t\t", "Should have three tab characters");
    assert_eq!(
        content.matches('\t').count(),
        3,
        "Should count 3 tab characters"
    );
}

/// Test that multiple tabs work correctly in Rust files (spaces)
#[test]
fn test_use_tabs_rust_multiple_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create an empty Rust file
    std::fs::write(&file_path, "").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Press Tab twice
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Get buffer content - should be 8 spaces (2 x 4)
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after 2 tabs in Rust: {:?}", content);

    assert_eq!(
        content, "        ",
        "Should have 8 spaces (2 tabs x 4 spaces)"
    );
    assert!(
        !content.contains('\t'),
        "Should NOT contain any tab characters"
    );
}
