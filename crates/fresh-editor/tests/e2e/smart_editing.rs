use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

use fresh::view::theme;

/// Helper to create a harness with auto-indent enabled
/// Uses `.without_empty_plugins_dir()` so that embedded plugins are loaded,
/// which is required for tree-sitter based smart editing to work.
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

// =============================================================================
// Bracket Auto-Close Tests
// =============================================================================

/// Test that opening parenthesis auto-closes
#[test]
fn test_auto_close_parenthesis() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type opening paren
    harness.type_text("fn main(").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "fn main()",
        "Opening paren should auto-close with closing paren"
    );

    // Cursor should be between the parens
    let cursor_pos = harness.editor().active_state().cursors.primary().position;
    assert_eq!(
        cursor_pos, 8,
        "Cursor should be between parens (at position 8)"
    );
}

/// Test that opening square bracket auto-closes
#[test]
fn test_auto_close_square_bracket() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type opening bracket
    harness.type_text("let arr = [").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "let arr = []", "Opening bracket should auto-close");
}

/// Test that opening curly brace auto-closes
#[test]
fn test_auto_close_curly_brace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type opening brace
    harness.type_text("struct Foo {").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "struct Foo {}", "Opening brace should auto-close");
}

/// Test that double quotes auto-close
#[test]
fn test_auto_close_double_quotes() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type opening quote
    harness.type_text("let s = \"").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "let s = \"\"",
        "Opening double quote should auto-close"
    );
}

/// Test that single quotes auto-close
#[test]
fn test_auto_close_single_quotes() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type opening single quote
    harness.type_text("let c = '").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "let c = ''",
        "Opening single quote should auto-close"
    );
}

/// Test that backticks auto-close
#[test]
fn test_auto_close_backtick() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.js");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type opening backtick
    harness.type_text("const template = `").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "const template = ``",
        "Opening backtick should auto-close"
    );
}

/// Test that auto-close doesn't happen before alphanumeric characters
#[test]
fn test_no_auto_close_before_alphanumeric() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "test").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursor before "test"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Type opening paren - should NOT auto-close because next char is alphanumeric
    harness.type_text("(").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "(test",
        "Should NOT auto-close when followed by alphanumeric"
    );
}

/// Test that auto-close happens before whitespace
#[test]
fn test_auto_close_before_whitespace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, " world").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursor at beginning (before space)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Type opening paren - should auto-close before whitespace
    harness.type_text("(").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "() world",
        "Should auto-close when followed by whitespace"
    );
}

/// Test auto-close is disabled when auto_indent config is false
#[test]
fn test_no_auto_close_when_config_disabled() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    // Create harness with auto_indent disabled
    let mut config = Config::default();
    config.editor.auto_indent = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();

    // Type opening paren
    harness.type_text("(").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "(",
        "Should NOT auto-close when auto_indent is disabled"
    );
}

// =============================================================================
// Bracket Skip-Over Tests
// =============================================================================

/// Test that typing a closing paren when cursor is before one just moves cursor
#[test]
fn test_skip_over_closing_parenthesis() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type an opening paren - should auto-close to "()"
    harness.type_text("(").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "()", "Opening paren should auto-close");

    // Cursor should be between parens, at position 1
    assert_eq!(
        harness.cursor_position(),
        1,
        "Cursor should be between parens"
    );

    // Type a closing paren - should skip over the existing one, not insert another
    harness.type_text(")").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "()",
        "Typing closing paren should skip over existing one, not create ()))"
    );

    // Cursor should now be after the closing paren
    assert_eq!(
        harness.cursor_position(),
        2,
        "Cursor should be after the paren"
    );
}

/// Test that typing a closing bracket when cursor is before one just moves cursor
#[test]
fn test_skip_over_closing_bracket() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type an opening bracket - should auto-close to "[]"
    harness.type_text("[").unwrap();
    harness.render().unwrap();

    assert_eq!(harness.get_buffer_content().unwrap(), "[]");
    assert_eq!(harness.cursor_position(), 1);

    // Type a closing bracket - should skip over existing one
    harness.type_text("]").unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "[]",
        "Typing closing bracket should skip over existing one"
    );
    assert_eq!(harness.cursor_position(), 2);
}

/// Test that typing a closing brace when cursor is before one just moves cursor
#[test]
fn test_skip_over_closing_brace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type an opening brace - should auto-close to "{}"
    harness.type_text("{").unwrap();
    harness.render().unwrap();

    assert_eq!(harness.get_buffer_content().unwrap(), "{}");
    assert_eq!(harness.cursor_position(), 1);

    // Type a closing brace - should skip over existing one
    harness.type_text("}").unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "{}",
        "Typing closing brace should skip over existing one"
    );
    assert_eq!(harness.cursor_position(), 2);
}

/// Test that typing a closing quote when cursor is before one just moves cursor
#[test]
fn test_skip_over_closing_quote() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Type an opening quote - should auto-close to "\"\""
    harness.type_text("\"").unwrap();
    harness.render().unwrap();

    assert_eq!(harness.get_buffer_content().unwrap(), "\"\"");
    assert_eq!(harness.cursor_position(), 1);

    // Type a closing quote - should skip over existing one
    harness.type_text("\"").unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "\"\"",
        "Typing closing quote should skip over existing one"
    );
    assert_eq!(harness.cursor_position(), 2);
}

/// Test that typing a closing delimiter does NOT skip if the char isn't a match
#[test]
fn test_no_skip_when_different_char() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "(x").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to position 1 (before 'x')
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Type a closing paren - should insert because next char is 'x', not ')'
    harness.type_text(")").unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "()x",
        "Should insert closing paren when next char is not the same"
    );
}

// =============================================================================
// Auto-Pair Deletion Tests
// =============================================================================

/// Test that deleting between matching parentheses deletes both
#[test]
fn test_auto_pair_delete_parentheses() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn test()").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursor between parens (after the opening paren)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before )

    // Delete backward - should delete both ( and )
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "fn test",
        "Deleting between matching parens should delete both"
    );
}

/// Test that deleting between matching square brackets deletes both
#[test]
fn test_auto_pair_delete_square_brackets() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "let arr = []").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursor between brackets
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before ]

    // Delete backward
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "let arr = ",
        "Deleting between matching brackets should delete both"
    );
}

/// Test that deleting between matching curly braces deletes both
#[test]
fn test_auto_pair_delete_curly_braces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "struct Foo {}").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursor between braces
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before }

    // Delete backward
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "struct Foo ",
        "Deleting between matching braces should delete both"
    );
}

/// Test that deleting between matching double quotes deletes both
#[test]
fn test_auto_pair_delete_double_quotes() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "let s = \"\"").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursor between quotes
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before closing "

    // Delete backward
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "let s = ",
        "Deleting between matching double quotes should delete both"
    );
}

/// Test that deleting between matching single quotes deletes both
#[test]
fn test_auto_pair_delete_single_quotes() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "let c = ''").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursor between quotes
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before closing '

    // Delete backward
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "let c = ",
        "Deleting between matching single quotes should delete both"
    );
}

/// Test that deleting with content between pairs only deletes the opening character
#[test]
fn test_no_pair_delete_with_content_between() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn test(x)").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursor after x (between x and closing paren)
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap(); // Before )

    // Delete backward - should only delete x, not the whole pair
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "fn test()",
        "Should only delete the character when not directly between empty pair"
    );
}

// =============================================================================
// Macro Recording and Playback Tests
// =============================================================================

/// Test starting and stopping macro recording
#[test]
fn test_macro_recording_toggle() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "hello").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Start recording macro 0 with Alt+Shift+0
    harness
        .send_key(KeyCode::Char('0'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Verify recording state through status message
    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("Recording macro") && status.contains("'0'"),
        "Should show recording status, got: {}",
        status
    );

    // Stop recording by toggling again (Alt+Shift+0)
    harness
        .send_key(KeyCode::Char('0'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("Stopped recording") || status.contains("saved"),
        "Should show stopped recording, got: {}",
        status
    );
}

/// Test recording and playing back a simple macro
#[test]
fn test_macro_record_and_playback() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "line1\nline2\nline3").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position at beginning of line 1
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Start recording macro 1
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Record actions: go to end of line, type "!"
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("!").unwrap();

    // Move to next line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Stop recording by toggling again (Alt+Shift+1)
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    // After recording, line1 should have "!" appended
    assert!(
        content.contains("line1!"),
        "First line should have ! appended, got: {}",
        content
    );

    // Play macro 1 with Ctrl+1 to process line2
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    // line2 should now have "!" appended
    assert!(
        content.contains("line2!"),
        "Second line should have ! appended after playback, got: {}",
        content
    );

    // Play macro again for line3
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    // line3 should now have "!" appended
    assert!(
        content.contains("line3!"),
        "Third line should have ! appended after playback, got: {}",
        content
    );
}

/// Test that macros can be recorded with different slot numbers
#[test]
fn test_multiple_macro_slots() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "test").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Record macro in slot 5
    harness
        .send_key(KeyCode::Char('5'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("Recording macro") && status.contains("'5'"),
        "Should record in slot 5, got: {}",
        status
    );

    // Stop recording
    harness.send_key(KeyCode::F(5), KeyModifiers::NONE).unwrap();

    // Record macro in slot 9
    harness
        .send_key(KeyCode::Char('9'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("Recording macro") && status.contains("'9'"),
        "Should record in slot 9, got: {}",
        status
    );

    harness.send_key(KeyCode::F(5), KeyModifiers::NONE).unwrap();
}

/// Test playing a macro that doesn't exist
#[test]
fn test_play_nonexistent_macro() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "test").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Try to play macro 8 (which was never recorded)
    // Note: We use 8 instead of 7 because Ctrl+7 is now mapped to toggle_comment
    // (as the terminal equivalent of Ctrl+/)
    harness
        .send_key(KeyCode::Char('8'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("No macro") || status.contains("not found") || status.is_empty(),
        "Should indicate macro not found or be empty, got: {}",
        status
    );

    // Buffer content should be unchanged
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "test", "Content should be unchanged");
}

/// Test that toggle recording starts and stops correctly
#[test]
fn test_toggle_macro_recording() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "test").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Toggle ON - start recording
    harness
        .send_key(KeyCode::Char('2'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("Recording"),
        "Should start recording, got: {}",
        status
    );

    // Toggle OFF - stop recording
    harness
        .send_key(KeyCode::Char('2'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("Stopped") || status.contains("saved"),
        "Should stop recording, got: {}",
        status
    );
}

/// Test that macro recording hint message shows correct keybindings (fix for issue #659)
/// The message should show dynamic keybindings (F5 and command palette) not hardcoded ones.
#[test]
fn test_macro_recording_hint_shows_correct_keybinding() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "test").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Start recording macro 1 using Alt+Shift+1
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Get the status message
    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();

    // Verify recording started
    assert!(
        status.contains("Recording macro") && status.contains("'1'"),
        "Should show recording status, got: {}",
        status
    );

    // The message should NOT contain the hardcoded "Ctrl+Shift+R" (which was never bound)
    assert!(
        !status.contains("Ctrl+Shift+R"),
        "Message should NOT contain hardcoded Ctrl+Shift+R, got: {}",
        status
    );

    // The message should contain the actual keybinding (F5) or mention command palette
    assert!(
        status.contains("F5") || status.contains("Ctrl+P"),
        "Message should mention F5 or Ctrl+P (command palette), got: {}",
        status
    );

    // Verify F5 actually stops recording
    harness.send_key(KeyCode::F(5), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let status_after = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();

    // The status should say "saved" after F5
    assert!(
        status_after.contains("saved"),
        "F5 should stop recording, got: {}",
        status_after
    );

    // The saved message should also contain a play hint mentioning command palette
    assert!(
        status_after.contains("Ctrl+P") || status_after.contains("Play"),
        "Saved message should mention how to play macro, got: {}",
        status_after
    );
}

// =============================================================================
// Jump to Next/Previous Error Tests
// =============================================================================

/// Helper to create and apply diagnostics to the editor state
fn apply_test_diagnostics(
    harness: &mut EditorTestHarness,
    diagnostics: Vec<lsp_types::Diagnostic>,
) {
    let state = harness.editor_mut().active_state_mut();
    let theme = fresh::view::theme::Theme::from_name(theme::THEME_DARK).unwrap();
    fresh::services::lsp::diagnostics::apply_diagnostics_to_state(state, &diagnostics, &theme);
}

/// Create a simple diagnostic at a given position
fn create_diagnostic(
    start_line: u32,
    start_char: u32,
    end_line: u32,
    end_char: u32,
    message: &str,
) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic {
        range: lsp_types::Range {
            start: lsp_types::Position {
                line: start_line,
                character: start_char,
            },
            end: lsp_types::Position {
                line: end_line,
                character: end_char,
            },
        },
        severity: Some(lsp_types::DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: None,
        message: message.to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Test jumping to next error when there are diagnostics
#[test]
fn test_jump_to_next_error() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "line1\nline2\nline3\nline4\nline5").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Apply diagnostics at line 2 and line 4
    let diagnostics = vec![
        create_diagnostic(1, 0, 1, 5, "Error on line 2"),
        create_diagnostic(3, 0, 3, 5, "Error on line 4"),
    ];
    apply_test_diagnostics(&mut harness, diagnostics);

    // Position cursor at start (line 0)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Jump to next error (F8) - should go to line 2 (index 1)
    harness.send_key(KeyCode::F(8), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Check cursor position - should be at start of line 2 (byte offset 6)
    let cursor_pos = harness.editor().active_state().cursors.primary().position;
    // Line 0: "line1\n" = 6 bytes, so line 1 starts at byte 6
    assert_eq!(
        cursor_pos, 6,
        "Cursor should jump to first error (line 2, byte 6)"
    );

    // Check status message shows the error
    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("Error on line 2"),
        "Should show error message, got: {}",
        status
    );
}

/// Test jumping to previous error
#[test]
fn test_jump_to_previous_error() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "line1\nline2\nline3\nline4\nline5").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Apply diagnostics at line 2 and line 4
    let diagnostics = vec![
        create_diagnostic(1, 0, 1, 5, "Error on line 2"),
        create_diagnostic(3, 0, 3, 5, "Error on line 4"),
    ];
    apply_test_diagnostics(&mut harness, diagnostics);

    // Position cursor at end (line 5)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();

    // Jump to previous error (Shift+F8) - should go to line 4 (index 3)
    harness
        .send_key(KeyCode::F(8), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Check cursor position - should be at start of line 4
    // Line 0-2: "line1\nline2\nline3\n" = 18 bytes
    let cursor_pos = harness.editor().active_state().cursors.primary().position;
    assert_eq!(
        cursor_pos, 18,
        "Cursor should jump to last error (line 4, byte 18)"
    );

    // Check status message
    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("Error on line 4"),
        "Should show error message, got: {}",
        status
    );
}

/// Test that jump wraps around when at the end
#[test]
fn test_jump_to_next_error_wraps() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "line1\nline2\nline3").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Apply diagnostic at line 1 only
    let diagnostics = vec![create_diagnostic(0, 0, 0, 5, "Error on line 1")];
    apply_test_diagnostics(&mut harness, diagnostics);

    // Position cursor at line 2 (past the only error)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Jump to next error - should wrap to line 1
    harness.send_key(KeyCode::F(8), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let cursor_pos = harness.editor().active_state().cursors.primary().position;
    assert_eq!(
        cursor_pos, 0,
        "Cursor should wrap to first error (line 1, byte 0)"
    );
}

/// Test that jump shows message when no errors
#[test]
fn test_jump_to_error_no_diagnostics() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "line1\nline2").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // No diagnostics applied

    // Try to jump to next error
    harness.send_key(KeyCode::F(8), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Check status message indicates no errors
    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("No") || status.contains("no") || status.is_empty(),
        "Should indicate no errors, got: {}",
        status
    );

    // Cursor should not have moved
    let cursor_pos = harness.editor().active_state().cursors.primary().position;
    assert_eq!(cursor_pos, 0, "Cursor should not move when no errors");
}

/// Test jumping between multiple errors in sequence
#[test]
fn test_jump_through_multiple_errors() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "a\nb\nc\nd\ne").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Apply diagnostics at lines 1, 2, and 4
    let diagnostics = vec![
        create_diagnostic(0, 0, 0, 1, "Error 1"),
        create_diagnostic(1, 0, 1, 1, "Error 2"),
        create_diagnostic(3, 0, 3, 1, "Error 3"),
    ];
    apply_test_diagnostics(&mut harness, diagnostics);

    // Start at beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Jump to next error - should be at line 0 (already there, or next one at line 1)
    harness.send_key(KeyCode::F(8), KeyModifiers::NONE).unwrap();
    let pos1 = harness.editor().active_state().cursors.primary().position;

    // Jump again
    harness.send_key(KeyCode::F(8), KeyModifiers::NONE).unwrap();
    let pos2 = harness.editor().active_state().cursors.primary().position;

    // Jump again
    harness.send_key(KeyCode::F(8), KeyModifiers::NONE).unwrap();
    let pos3 = harness.editor().active_state().cursors.primary().position;

    // All positions should be different (cycling through errors)
    assert!(
        pos1 != pos2 || pos2 != pos3 || pos1 != pos3,
        "Should cycle through different error positions: pos1={}, pos2={}, pos3={}",
        pos1,
        pos2,
        pos3
    );
}

// =============================================================================
// Block/Rectangular Selection Tests
// =============================================================================

/// Test that block selection starts with Alt+Shift+Right
#[test]
fn test_block_selection_start() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "abc\ndef\nghi").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Cursor starts at position 0
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Start block selection with Alt+Shift+Right
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Check that selection mode is Block
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(
        cursor.selection_mode,
        fresh::model::cursor::SelectionMode::Block,
        "Selection mode should be Block"
    );

    // Block anchor should be set
    assert!(cursor.block_anchor.is_some(), "Block anchor should be set");

    let anchor = cursor.block_anchor.unwrap();
    assert_eq!(anchor.line, 0, "Block anchor line should be 0");
    assert_eq!(anchor.column, 0, "Block anchor column should be 0");

    // Cursor should have moved right
    assert_eq!(
        cursor.position, 1,
        "Cursor position should be 1 after moving right"
    );
}

/// Test that block selection extends vertically
#[test]
fn test_block_selection_vertical() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "abc\ndef\nghi").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Position cursor at column 1 (after 'a')
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Start block selection with Alt+Shift+Down
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let anchor = cursor.block_anchor.unwrap();

    // Anchor should be at line 0, column 1
    assert_eq!(anchor.line, 0);
    assert_eq!(anchor.column, 1);

    // Cursor should now be on line 1 (after moving down)
    let cur_line = harness
        .editor()
        .active_state()
        .buffer
        .get_line_number(cursor.position);
    assert_eq!(cur_line, 1, "Cursor should be on line 1 after moving down");
}

/// Test that block selection extends in multiple directions
#[test]
fn test_block_selection_rectangle() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "abcde\nfghij\nklmno").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to position 1 (after 'a')
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Extend block selection right twice (columns 1-3)
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();

    // Extend block selection down once (lines 0-1)
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let anchor = cursor.block_anchor.unwrap();

    // Block should be: anchor at (0, 1), cursor at line 1
    assert_eq!(anchor.line, 0);
    assert_eq!(anchor.column, 1);

    // Get cursor's 2D position
    let cur_line = harness
        .editor()
        .active_state()
        .buffer
        .get_line_number(cursor.position);
    let line_start = harness
        .editor()
        .active_state()
        .buffer
        .line_start_offset(cur_line)
        .unwrap_or(0);
    let cur_col = cursor.position - line_start;

    // Cursor should be at line 1, column 3 (after moving right twice then down)
    assert_eq!(cur_line, 1, "Cursor should be on line 1");
    assert_eq!(cur_col, 3, "Cursor should be at column 3");
}

/// Test that block selection left works
#[test]
fn test_block_selection_left() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "abcde").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move cursor to position 3
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Start block selection with Alt+Shift+Left
    harness
        .send_key(KeyCode::Left, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let anchor = cursor.block_anchor.unwrap();

    // Anchor should be at column 3
    assert_eq!(anchor.column, 3);

    // Cursor should now be at column 2 (after moving left)
    assert_eq!(cursor.position, 2, "Cursor should be at position 2");
}

/// Test that block selection up works
#[test]
fn test_block_selection_up() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "abc\ndef\nghi").unwrap();

    let mut harness = harness_with_auto_indent();
    harness.open_file(&file_path).unwrap();

    // Move to line 2
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Start block selection with Alt+Shift+Up
    harness
        .send_key(KeyCode::Up, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let anchor = cursor.block_anchor.unwrap();

    // Anchor should be at line 2
    assert_eq!(anchor.line, 2);

    // Cursor should now be on line 1 (after moving up)
    let cur_line = harness
        .editor()
        .active_state()
        .buffer
        .get_line_number(cursor.position);
    assert_eq!(cur_line, 1, "Cursor should be on line 1 after moving up");
}
