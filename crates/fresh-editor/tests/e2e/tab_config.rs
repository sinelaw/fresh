//! Tests for per-language tab configuration options:
//! - `show_whitespace_tabs`: Whether to display tab indicators (→) in the editor
//! - `use_tabs`: Whether pressing Tab inserts a tab character or spaces

use crate::common::harness::{EditorTestHarness, HarnessOptions};
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

// =============================================================================
// GitHub Issue #384: Better Tab Indentation Support
// https://github.com/sinelaw/fresh/issues/384
// =============================================================================

/// Issue #384 - Auto-indent should use tabs when use_tabs is true
/// When pressing Enter after `{` in a Go file, the auto-indent should insert
/// tab characters, not spaces.
#[test]
fn test_issue_384_auto_indent_uses_tabs_in_go() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create an empty Go file
    std::fs::write(&file_path, "").unwrap();

    // Need embedded plugins for tree-sitter auto-indent to work
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
    harness.open_file(&file_path).unwrap();

    // Type "func main() {" and press Enter
    harness.type_text("func main() {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Get buffer content - the auto-indent should use a tab, not spaces
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Enter in Go file: {:?}", content);

    // The auto-indent after { should insert a tab character, not spaces
    assert!(
        content.contains("\n\t"),
        "Auto-indent should use tab character in Go files, but got: {:?}",
        content
    );
    assert!(
        !content.contains("\n    "),
        "Auto-indent should NOT use spaces in Go files"
    );
}

/// Issue #384 - Auto-indent should use spaces when use_tabs is false
/// When pressing Enter after `{` in a Rust file, the auto-indent should insert
/// spaces (confirming the opposite case works).
#[test]
fn test_issue_384_auto_indent_uses_spaces_in_rust() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create an empty Rust file
    std::fs::write(&file_path, "").unwrap();

    // Need embedded plugins for tree-sitter auto-indent to work
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
    harness.open_file(&file_path).unwrap();

    // Type "fn main() {" and press Enter
    harness.type_text("fn main() {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Get buffer content - the auto-indent should use spaces
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Enter in Rust file: {:?}", content);

    // The auto-indent after { should insert 4 spaces
    assert!(
        content.contains("\n    "),
        "Auto-indent should use spaces in Rust files, but got: {:?}",
        content
    );
    assert!(
        !content.contains("\n\t"),
        "Auto-indent should NOT use tabs in Rust files"
    );
}

/// Issue #384 - Tab width configuration should affect rendering
/// Compares rendering with tab_size=2 vs tab_size=8 to verify that
/// the tab_size config actually affects how tabs are displayed.
#[test]
fn test_issue_384_tab_width_affects_rendering() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with a tab followed by text marker
    std::fs::write(&file_path, "\tX").unwrap();

    // Helper to get X visual column relative to the tab indicator
    fn get_x_visual_offset_from_indicator(screen: &str) -> Option<usize> {
        let lines: Vec<&str> = screen.lines().collect();
        let x_line = lines.iter().find(|line| line.contains('X'))?;

        // Find visual column positions by iterating through chars
        let mut indicator_col = None;
        let mut x_col = None;
        for (col, ch) in x_line.chars().enumerate() {
            if ch == '→' {
                indicator_col = Some(col);
            }
            if ch == 'X' {
                x_col = Some(col);
            }
        }

        Some(x_col? - indicator_col?)
    }

    // Test with tab_size = 2
    let mut config_2 = Config::default();
    config_2.editor.tab_size = 2;
    let mut harness_2 = EditorTestHarness::with_config(80, 24, config_2).unwrap();
    harness_2.open_file(&file_path).unwrap();
    harness_2.render().unwrap();
    let screen_2 = harness_2.screen_to_string();
    let offset_2 = get_x_visual_offset_from_indicator(&screen_2).unwrap();
    println!("Screen with tab_size=2:\n{}", screen_2);
    println!(
        "X visual offset from indicator with tab_size=2: {}",
        offset_2
    );

    // Test with tab_size = 8
    let mut config_8 = Config::default();
    config_8.editor.tab_size = 8;
    let mut harness_8 = EditorTestHarness::with_config(80, 24, config_8).unwrap();
    harness_8.open_file(&file_path).unwrap();
    harness_8.render().unwrap();
    let screen_8 = harness_8.screen_to_string();
    let offset_8 = get_x_visual_offset_from_indicator(&screen_8).unwrap();
    println!("Screen with tab_size=8:\n{}", screen_8);
    println!(
        "X visual offset from indicator with tab_size=8: {}",
        offset_8
    );

    // The key assertion: with different tab_size values, the X should appear
    // at different visual positions relative to the tab indicator.
    // With tab_size=2: indicator (1 col) + 1 space = 2 visual columns to X
    // With tab_size=8: indicator (1 col) + 7 spaces = 8 visual columns to X
    assert_ne!(
        offset_2, offset_8,
        "Tab width should affect rendering: tab_size=2 offset={}, tab_size=8 offset={}, but they should differ!",
        offset_2, offset_8
    );

    // Visual offset should match tab_size
    assert_eq!(
        offset_2, 2,
        "With tab_size=2, X should be 2 visual columns after the indicator, got {}",
        offset_2
    );
    assert_eq!(
        offset_8, 8,
        "With tab_size=8, X should be 8 visual columns after the indicator, got {}",
        offset_8
    );
}

/// Issue #384 - Indent calculation should respect tab_size when counting existing tabs
/// When a line contains tab characters, the indent calculation should use tab_size
/// to determine the visual width, not a hardcoded value.
#[test]
fn test_issue_384_indent_respects_tab_size_in_calculation() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create a Go file with tab-indented code
    // The existing tabs should be counted with the correct tab_size
    std::fs::write(&file_path, "func main() {\n\tif true {").unwrap();

    // Test with tab_size = 2 (tabs should be counted as 2 spaces)
    // Need embedded plugins for tree-sitter auto-indent to work
    let mut config = Config::default();
    config.editor.tab_size = 2;
    config.editor.auto_indent = true;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();

    // Press Enter - auto-indent should add one more level (tab_size=2 means 2 more columns)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!(
        "Buffer content with tab_size=2 after Enter in Go: {:?}",
        content
    );

    // With tab_size=2, the existing "\t" is 2 columns, so the new indent should be
    // 2 + 2 = 4 columns, which could be "\t\t" (two tabs) if using tabs
    // The key assertion: the indent should respect tab_size
    // If hardcoded to 4, we'd get wrong indentation

    // Count the visual indent of the new line
    // The content should look like: "func main() {\n\tif true {\n<indent>"
    let lines: Vec<&str> = content.lines().collect();
    assert!(lines.len() >= 3, "Should have at least 3 lines");

    let last_line = lines[2];
    println!("Last line after Enter: {:?}", last_line);

    // With use_tabs=true for Go and tab_size=2:
    // - Line 2 has "\tif true {" = 2 visual columns of indent
    // - After Enter and auto-indent (one more level): should be 4 visual columns
    // - With use_tabs=true, this should be 2 tabs ("\t\t") = 4 columns at tab_size=2
    //
    // BUG: Currently the indent calculation might use hardcoded tab width (4)
    // instead of the configured tab_size (2), leading to wrong indent level

    // The new line should have tabs (since Go uses tabs)
    let indent_tabs = last_line.chars().take_while(|c| *c == '\t').count();
    let indent_spaces = last_line
        .chars()
        .skip(indent_tabs)
        .take_while(|c| *c == ' ')
        .count();
    let visual_indent = indent_tabs * 2 + indent_spaces; // tab_size=2

    println!(
        "Indent analysis: {} tabs, {} spaces, {} visual columns",
        indent_tabs, indent_spaces, visual_indent
    );

    // Expected: 4 visual columns (2 from existing + 2 for new level)
    // With tabs, this should be 2 tabs
    assert_eq!(
        visual_indent, 4,
        "With tab_size=2, nested indent should be 4 visual columns (2+2), but got {}",
        visual_indent
    );
}

/// Issue #384 - Auto-indent should maintain indentation on Enter after a normal
/// statement line in Go (using tabs).
///
/// When pressing Enter after `fmt.Println("Hello")` inside a Go function,
/// the new line should be auto-indented to the same level using tab characters.
/// This tests the "maintain current indent" path, not the "increase indent after {" path.
///
/// The bug: pressing Enter after a normal statement produces zero indentation
/// instead of maintaining the current indent level with tabs.
#[test]
fn test_issue_384_auto_indent_maintains_indent_with_tabs_in_go() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create a Go file with a function containing a statement
    std::fs::write(
        &file_path,
        "package main\n\nfunc main() {\n\tfmt.Println(\"Hello\")\n}\n",
    )
    .unwrap();

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
    harness.open_file(&file_path).unwrap();

    // Move to end of the fmt.Println line (line 4, after the `)`):
    // "package main\n\nfunc main() {\n\tfmt.Println(\"Hello\")\n}\n"
    //  ^0                            ^28                   ^49
    // Line 4 is: \tfmt.Println("Hello")
    // Move to end of line 4
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Now on line 4 (0-indexed line 3)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter - auto-indent should maintain the tab indentation
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!("Buffer after Enter on normal Go line: {:?}", content);

    // The new line should have a tab character for indentation (same level as fmt.Println)
    let lines: Vec<&str> = content.lines().collect();
    // Lines should be:
    // 0: "package main"
    // 1: ""
    // 2: "func main() {"
    // 3: "\tfmt.Println(\"Hello\")"
    // 4: "\t"  <-- the new line with auto-indent (should have a tab)
    // 5: "}"
    assert!(
        lines.len() >= 6,
        "Should have at least 6 lines after Enter, got {}: {:?}",
        lines.len(),
        lines
    );

    let new_line = lines[4];
    println!("New line after Enter: {:?}", new_line);

    // The new line should start with a tab (maintaining indent from the previous line)
    assert!(
        new_line.starts_with('\t'),
        "Auto-indent should maintain tab indentation on new line in Go, got: {:?}",
        new_line
    );

    // The indentation should NOT use spaces
    let leading_spaces = new_line.chars().take_while(|c| *c == ' ').count();
    assert_eq!(
        leading_spaces, 0,
        "Auto-indent in Go should not use spaces, got {} leading spaces in: {:?}",
        leading_spaces, new_line
    );
}

// =============================================================================
// GitHub Issue #1068: Go auto-dedent should use tabs, not spaces
// https://github.com/sinelaw/fresh/issues/1068
// =============================================================================

/// Issue #1068 - Auto-dedent for nested closing brace should use tabs in Go files.
///
/// When typing `}` to close an inner block in a Go file, the auto-dedent logic
/// produces the indentation string. This string should use tab characters (since
/// Go's use_tabs=true), not spaces.
///
/// The bug: `handle_auto_dedent` uses `" ".repeat(correct_indent)` which always
/// produces spaces regardless of the language's use_tabs setting. When the closing
/// brace needs non-zero indentation (e.g., closing an inner block), this results
/// in space characters in a Go file where only tabs should be used. This means
/// that changing tab_size later has no visual effect on those lines.
#[test]
fn test_issue_1068_auto_dedent_uses_tabs_not_spaces_in_go() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create a Go file with nested blocks, ending with an empty line where the
    // user will type `}` to close the inner block. The empty line has two tabs
    // of indentation.
    //
    // The file content before the cursor:
    //   func main() {
    //   \tif true {
    //   \t\tfmt.Println("hello")
    //   \t\t                         <-- cursor goes here (end of file)
    //
    // When user types `}`, auto-dedent should compute the correct indent level
    // for closing `if true {` (which is 1 tab = 8 visual columns for Go).
    // The bug: it would produce "        }" (8 spaces) instead of "\t}" (1 tab).
    let file_content = "func main() {\n\tif true {\n\t\tfmt.Println(\"hello\")\n\t\t";
    std::fs::write(&file_path, file_content).unwrap();

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
    harness.open_file(&file_path).unwrap();

    // Move cursor to end of file (end of the "\t\t" line)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let content_before = harness.get_buffer_content().unwrap();
    println!("Buffer before typing }}: {:?}", content_before);

    // Verify cursor position: it should be at the end of the file
    let cursor_pos = harness.editor().active_cursors().primary().position;
    assert_eq!(
        cursor_pos,
        file_content.len(),
        "Cursor should be at end of file"
    );

    // Type `}` - this triggers auto-dedent because:
    // - `}` is a closing delimiter
    // - the line before cursor only has whitespace (two tabs)
    // - auto_indent is enabled
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!("Buffer after typing }}: {:?}", content);

    // Check the last line (where we typed `}`)
    let lines: Vec<&str> = content.lines().collect();
    let last_line = lines.last().unwrap();
    println!("Last line (dedented): {:?}", last_line);

    // The critical assertion: the line must NOT contain spaces for indentation.
    // In Go, all indentation should be tab characters.
    let leading_whitespace: String = last_line
        .chars()
        .take_while(|c| c.is_whitespace())
        .collect();
    assert!(
        !leading_whitespace.contains(' '),
        "Auto-dedent in Go should use tabs, not spaces. Line: {:?}, leading whitespace: {:?}",
        last_line,
        leading_whitespace
    );

    // Also verify no spaces are used for indentation in any line of the buffer
    for (i, line) in lines.iter().enumerate() {
        let leading_spaces = line.chars().take_while(|c| *c == ' ').count();
        assert_eq!(
            leading_spaces, 0,
            "Line {} should not have leading spaces (Go uses tabs), got: {:?}",
            i, line
        );
    }
}

/// Issue #1068 - Skip-over-with-dedent for closing brace should use tabs in Go.
///
/// When typing `}` and the next character is already `}` (e.g., from auto-close
/// bracket expansion), the skip-over-with-dedent logic corrects the indentation
/// and skips over the existing character. The indentation correction should use
/// tabs in Go files.
///
/// The bug: `handle_skip_over_with_dedent` uses `" ".repeat(correct_indent)` which
/// always produces spaces.
#[test]
fn test_issue_1068_skip_over_with_dedent_uses_tabs_in_go() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create an empty Go file and build content interactively to trigger
    // bracket auto-close and then skip-over-with-dedent.
    std::fs::write(&file_path, "").unwrap();

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
    harness.open_file(&file_path).unwrap();

    // Type "func main() {" - auto-close creates "func main() {}" with cursor between
    harness.type_text("func main() {").unwrap();

    // Press Enter - bracket expansion creates:
    // func main() {
    // \t<cursor>
    // }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Type "if true {" - auto-close creates "if true {}" with cursor between
    harness.type_text("if true {").unwrap();

    // Press Enter - bracket expansion creates:
    // func main() {
    // \tif true {
    // \t\t<cursor>
    // \t}
    // }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Type some content
    harness.type_text("x := 1").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let content_before = harness.get_buffer_content().unwrap();
    println!("Buffer before typing }}: {:?}", content_before);

    // Now type `}` - this should trigger skip-over-with-dedent because:
    // - The cursor is on a line with only tab whitespace
    // - The next character after newline is `\t}` (the auto-closed inner brace)
    // Or it may trigger auto-dedent if the next char isn't `}`
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!("Buffer after typing inner }}: {:?}", content);

    // Verify no spaces are used for indentation in any line
    let lines: Vec<&str> = content.lines().collect();
    for (i, line) in lines.iter().enumerate() {
        let leading_spaces = line.chars().take_while(|c| *c == ' ').count();
        assert_eq!(
            leading_spaces, 0,
            "Line {} should not have leading spaces (Go uses tabs), got: {:?}",
            i, line
        );
    }

    // Now type another `}` to close the outer block
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!("Buffer after typing outer }}: {:?}", content);

    // Verify no spaces are used for indentation in any line
    let lines: Vec<&str> = content.lines().collect();
    for (i, line) in lines.iter().enumerate() {
        let leading_spaces = line.chars().take_while(|c| *c == ' ').count();
        assert_eq!(
            leading_spaces, 0,
            "Line {} should not have leading spaces (Go uses tabs), got: {:?}",
            i, line
        );
    }
}
