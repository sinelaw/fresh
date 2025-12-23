//! E2E tests for Tab indent selection functionality
//!
//! Issue #353 - Tab and Shift-Tab while a region of text is selected should
//! add indent or remove indent.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Test that Tab indents selected lines
#[test]
fn test_tab_indents_selected_lines() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create a file with multiple lines
    std::fs::write(
        &file_path,
        "line 1\nline 2\nline 3\nline 4\n",
    )
    .unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to start of line 2
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Select line 2 and 3 (Shift+Down twice)
    harness.send_key(KeyCode::Down, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Press Tab to indent selected lines
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Check that selected lines are indented
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Tab indentation:\n{}", content);

    // Expected: lines 2, 3, and 4 should have 4 spaces prefix
    assert_eq!(
        content,
        "line 1\n    line 2\n    line 3\n    line 4\n",
        "Tab should indent selected lines with 4 spaces"
    );
}

/// Test that Tab with Go file uses tab character (Go has use_tabs=true by default)
#[test]
fn test_tab_indent_selection_with_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");

    // Create a Go file with multiple lines
    std::fs::write(
        &file_path,
        "line 1\nline 2\nline 3\nline 4\n",
    )
    .unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to start of line 2
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Select line 2 and 3
    harness.send_key(KeyCode::Down, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Press Tab to indent selected lines
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Check that selected lines are indented with tabs (Go files use tabs by default)
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Tab with tabs:\n{}", content);

    assert_eq!(
        content,
        "line 1\n\tline 2\n\tline 3\n\tline 4\n",
        "Tab should indent selected lines with tab character for Go files"
    );
}

/// Test that Tab without selection still inserts tab character
#[test]
fn test_tab_without_selection_inserts_tab() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create an empty file
    std::fs::write(&file_path, "line 1\n").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to end of line 1
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Press Tab - should insert 4 spaces (default for Rust)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Tab without selection:\n{}", content);

    assert_eq!(
        content,
        "line 1    \n",
        "Tab without selection should still insert tab character"
    );
}

/// Test that Shift+Tab dedents selected lines
#[test]
fn test_shift_tab_dedents_selected_lines() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create a file with indented lines
    std::fs::write(
        &file_path,
        "line 1\n    line 2\n    line 3\nline 4\n",
    )
    .unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to start of line 2
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Select line 2 and 3
    harness.send_key(KeyCode::Down, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Press Shift+Tab to dedent selected lines
    harness
        .send_key(KeyCode::Tab, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Check that selected lines are dedented
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Shift+Tab dedentation:\n{}", content);

    assert_eq!(
        content,
        "line 1\nline 2\nline 3\nline 4\n",
        "Shift+Tab should dedent selected lines"
    );
}

/// Test that Shift+Tab works on single line without selection
#[test]
fn test_shift_tab_dedents_single_line() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create a file with an indented line
    std::fs::write(&file_path, "    line 1\n").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to the indented line (no selection)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Shift+Tab to dedent
    harness
        .send_key(KeyCode::Tab, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Check that line is dedented
    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Shift+Tab on single line:\n{}", content);

    assert_eq!(
        content,
        "line 1\n",
        "Shift+Tab should dedent the current line without selection"
    );
}

/// Test that multiple Tab presses indent multiple levels
#[test]
fn test_multiple_tabs_indent_multiple_levels() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create a file
    std::fs::write(&file_path, "line 1\nline 2\n").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Select line 2
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Press Tab twice
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after two Tabs:\n{}", content);

    assert_eq!(
        content,
        "line 1\n        line 2\n",
        "Multiple Tab presses should indent multiple levels"
    );
}

/// Test that Tab with partial line selection indents entire lines
#[test]
fn test_tab_partial_line_selection_indents_full_lines() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create a file
    std::fs::write(&file_path, "line 1\nline 2\nline 3\n").unwrap();

    let config = Config::default();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to "in" of "line 2" and select "ine" (partial selection)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap(); // "i"
    harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap(); // "n"
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap(); // "e"
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap(); // select "e"
    harness.render().unwrap();

    // Press Tab - should indent the entire line, not just selected part
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    println!("Buffer content after Tab with partial selection:\n{}", content);

    assert_eq!(
        content,
        "line 1\n    line 2\nline 3\n",
        "Tab should indent entire line even with partial selection"
    );
}
