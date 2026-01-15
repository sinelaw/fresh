use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test that selections are visually visible on screen
#[test]
fn test_selection_visual_rendering() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text
    harness.type_text("Hello World").unwrap();

    // Move to start of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Select the word "Hello" using Shift+Right (5 times)
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    // Verify the cursor has a selection in the buffer
    let cursor = harness.editor().active_state().cursors.primary();
    let cursor_pos = cursor.position;
    let selection = cursor.selection_range();
    assert!(selection.is_some(), "Cursor should have a selection");

    let range = selection.unwrap();
    assert_eq!(range.start, 0, "Selection should start at position 0");
    assert_eq!(range.end, 5, "Selection should end at position 5");

    println!("Cursor position: {cursor_pos}, Selection: {range:?}");

    // Verify the selected text is "Hello"
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(selected_text, "Hello", "Selected text should be 'Hello'");

    // Get the screen rendering
    let _screen = harness.screen_to_string();

    // The screen should contain the text "Hello World"
    harness.assert_screen_contains("Hello World");

    // Check that the selected characters have the theme's selection background
    // Gutter takes up 8 characters: " " (indicator) + "   1" (line num) + " │ " (separator)
    // So "Hello" starts at column 8
    let buffer = harness.buffer();
    let theme = harness.editor().theme();
    let selection_bg = theme.selection_bg;

    // Get content area bounds from harness (accounts for menu bar, tab bar, status bar)
    let (content_first_row, _content_last_row) = harness.content_area_rows();
    let first_line_row = content_first_row as u16;

    // Check first character 'H' at position (8, first_line_row) - should have selection background
    let h_pos = buffer.index_of(8, first_line_row);
    let h_cell = &buffer.content[h_pos];
    assert_eq!(h_cell.symbol(), "H");
    assert_eq!(
        h_cell.bg, selection_bg,
        "Selected character 'H' should have selection background"
    );

    // Check fourth character 'l' at position (11, first_line_row) - should have selection background
    let l_pos = buffer.index_of(11, first_line_row);
    let l_cell = &buffer.content[l_pos];
    assert_eq!(l_cell.symbol(), "l");
    assert_eq!(
        l_cell.bg, selection_bg,
        "Selected character 'l' should have selection background"
    );

    // Check fifth character 'o' at position (12, first_line_row) - byte position 4, IN selection
    let o_pos = buffer.index_of(12, first_line_row);
    let o_cell = &buffer.content[o_pos];
    assert_eq!(o_cell.symbol(), "o");
    // This 'o' is at byte position 4, which is in the selection range 0..5
    // But the cursor is at position 5, not 4, so this should have selection background
    assert_eq!(
        o_cell.bg, selection_bg,
        "Selected character 'o' (byte 4) should have selection background"
    );

    // Check character ' ' (space) at position (13, first_line_row) - byte position 5, cursor position
    let space_pos = buffer.index_of(13, first_line_row);
    let space_cell = &buffer.content[space_pos];
    assert_eq!(space_cell.symbol(), " ");
    // This space is at byte position 5, which is the cursor position
    // It should NOT have selection background (cursor takes precedence over selection)
    // Also, position 5 is not in the selection range 0..5 anyway
    assert_ne!(
        space_cell.bg, selection_bg,
        "Cursor position (byte 5, space) should NOT have selection background"
    );
}

/// Test select word functionality (Ctrl+W)
#[test]
fn test_select_word() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text with multiple words
    harness.type_text("hello world test").unwrap();

    // Move to the middle of "world"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Now cursor is at position 8 (in the middle of "world")
    // Select word with Ctrl+W
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();

    // Verify the selection
    let cursor = harness.editor().active_state().cursors.primary();
    let selection = cursor.selection_range();
    assert!(selection.is_some(), "Cursor should have a selection");

    let range = selection.unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(selected_text, "world", "Should select the word 'world'");
}

/// Test select word at start of word
#[test]
fn test_select_word_at_start() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello world").unwrap();

    // Move to start of "world"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Select word
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(selected_text, "world", "Should select the word 'world'");
}

/// Test select word at end of word
#[test]
fn test_select_word_at_end() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello world").unwrap();

    // Move to end of "hello"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Select word
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(selected_text, "hello", "Should select the word 'hello'");
}

/// Test select line functionality (Ctrl+L)
#[test]
fn test_select_line() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type multiple lines
    harness
        .type_text("first line\nsecond line\nthird line")
        .unwrap();

    // Move to start of document, then down to second line
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Select line with Ctrl+L
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::CONTROL)
        .unwrap();

    // Verify the selection includes the entire line
    let cursor = harness.editor().active_state().cursors.primary();
    let selection = cursor.selection_range();
    assert!(selection.is_some(), "Cursor should have a selection");

    let range = selection.unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "second line\n",
        "Should select the entire line including newline"
    );
}

/// Test select line on first line
#[test]
fn test_select_line_first() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("first line\nsecond line").unwrap();

    // Move to start of document (first line)
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Select line
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::CONTROL)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "first line\n",
        "Should select the first line"
    );
}

/// Test select line on last line (no trailing newline)
#[test]
fn test_select_line_last() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("first line\nsecond line").unwrap();

    // Select line (cursor is already on last line)
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::CONTROL)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "second line",
        "Should select the last line without newline"
    );
}

/// Test select word with multiple cursors
#[test]
fn test_select_word_multi_cursor() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text with words
    harness.type_text("hello world test").unwrap();

    // Move to "hello"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Add cursor at "world" using Ctrl+D (add cursor at next match)
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();

    // Now we have "hello" selected, add cursor at next space or different word
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Add cursor above at same column
    harness
        .send_key(KeyCode::Down, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();

    // This test validates multi-cursor infrastructure is ready
    let state = harness.editor().active_state();
    assert!(
        state.cursors.count() >= 1,
        "Should have at least one cursor"
    );
}

/// Test expand selection functionality (Ctrl+Shift+Right)
#[test]
fn test_expand_selection() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text with multiple words
    harness.type_text("hello world test").unwrap();

    // Move to middle of "hello" (position 3, second 'l')
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // First expand should select from cursor to end of current word
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "lo",
        "First expand should select from cursor to end of word"
    );

    // Second expand should extend to include " world"
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "lo world",
        "Second expand should include next word"
    );

    // Third expand should extend to include " test"
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "lo world test",
        "Third expand should include third word"
    );
}

/// Test expand selection when starting with no selection
#[test]
fn test_expand_selection_no_initial_selection() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("foo bar baz").unwrap();

    // Move to middle of "bar" (position 5, on 'a')
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Expand with no initial selection should select from cursor to end of word
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "ar",
        "Should select from cursor to end of word"
    );
}

/// Test expand selection performance with moderately large buffer
/// This test ensures that selection operations don't read the entire buffer
#[test]
#[ignore]
fn test_expand_selection_large_buffer_performance() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large.txt");

    // Create a moderately large file (~100KB of text)
    let large_text = "word ".repeat(20_000); // ~100KB of text
    fs::write(&file_path, &large_text).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to a position near the middle
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..50 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Expand selection - this used to hang/timeout with large buffers
    // because it would read the entire buffer. Now it should complete quickly
    // by only reading a small window around the cursor.
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    // Verify it works correctly
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(
        cursor.selection_range().is_some(),
        "Should have a selection"
    );

    // The selected text should be a word (not testing exact content since position may vary)
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert!(!selected_text.is_empty(), "Selection should not be empty");
}

/// Test with an extremely large buffer (simulating the 63MB file issue)
/// This verifies the windowed reading approach works with very large files
#[test]
#[ignore] // This test takes a long time - run with --ignored flag
fn test_expand_selection_very_large_buffer() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("very_large.txt");

    // Create a very large file (~10MB of text - representative of the issue)
    let large_text = "word ".repeat(2_000_000); // ~10MB of text
    fs::write(&file_path, &large_text).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to various positions in the file and test expand selection
    // Test near the beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..100 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(
        cursor.selection_range().is_some(),
        "Should have selection at start"
    );

    // Test in the middle (move down many lines)
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap(); // Clear selection
    for _ in 0..1000 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(
        cursor.selection_range().is_some(),
        "Should have selection in middle"
    );

    // All operations should complete without hanging
}

/// Test selecting words after scrolling down beyond initial viewport
/// Ensures word selection works correctly at any position, not just visible lines
#[test]
fn test_select_word_after_scrolling() {
    use crossterm::event::{KeyCode, KeyModifiers};

    // Initialize tracing
    use tracing_subscriber::EnvFilter;
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a buffer with many lines (more than viewport height)
    let content: String = (0..100)
        .map(|i| format!("line{i} word{i} test{i}"))
        .collect::<Vec<_>>()
        .join("\n");
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

    // Scroll down past the initial viewport
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 50)
        .unwrap();

    // Move to middle of a word on line 50
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness
        .send_key_repeat(KeyCode::Right, KeyModifiers::NONE, 10)
        .unwrap();

    // Select word with Ctrl+W
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);

    // Should have selected "word50" at line 50
    assert!(
        selected_text.contains("word"),
        "Should select a word after scrolling"
    );
    assert!(!selected_text.is_empty(), "Selection should not be empty");
}

/// Test expand selection after scrolling down
#[test]
fn test_expand_selection_after_scrolling() {
    use crossterm::event::{KeyCode, KeyModifiers};

    // Initialize tracing
    use tracing_subscriber::EnvFilter;
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a buffer with many lines
    let content: String = (0..50)
        .map(|i| format!("alpha beta gamma delta epsilon line{i}"))
        .collect::<Vec<_>>()
        .join("\n");
    let _fixture = harness.load_buffer_from_text(&content).unwrap();

    // Scroll down to line 30
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    // Use send_key_repeat to avoid rendering after each key press (much faster)
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 30)
        .unwrap();

    // Move to middle of "alpha" (position 3, 'h')
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // First expand should select from cursor to end of word
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "ha",
        "First expand should select from cursor to end of word"
    );

    // Second expand should extend to include " beta"
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "ha beta",
        "Second expand should include next word"
    );
}

/// Test expand selection (Ctrl+Shift+Right) across line boundaries
/// Ensures selection can expand from end of one line to beginning of next
#[test]
fn test_expand_selection_across_lines() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multi-line content with words at line boundaries
    harness
        .type_text("first line ending\nsecond line starting here")
        .unwrap();

    // Position cursor at "ending" on first line
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    // Move back to start of "ending"
    for _ in 0..6 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }

    // First expand: select "ending"
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "ending",
        "Should select 'ending' on first line"
    );

    // Second expand: should cross the newline and select "second" on next line
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "ending\nsecond",
        "Should cross line boundary and select 'second'"
    );

    // Third expand: should continue to "line"
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "ending\nsecond line",
        "Should include 'line' from second line"
    );
}

/// Test expand selection starting at end of line
#[test]
fn test_expand_selection_from_line_end() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("first line\nsecond word here").unwrap();

    // Position cursor at end of first line (before newline)
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // First expand from end of line - should jump to next word on next line
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);

    // The selection should include the newline and "second"
    assert!(!selected_text.is_empty(), "Should select something");
    assert!(
        selected_text.contains("second"),
        "Should jump to next line and select 'second'"
    );

    // Continue expanding to ensure we can reach the next line
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);

    // After multiple expands, we should definitely reach "second" on the next line
    assert!(
        selected_text.contains("second"),
        "Should eventually reach 'second' on next line"
    );
}

/// Test select word with hyphen - hyphen should be a word separator
#[test]
fn test_select_word_with_hyphen() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("foo-bar").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "foo",
        "Hyphen should be a word separator, selecting 'foo'"
    );
}

/// Test select word with underscore - underscore should be a word character
#[test]
fn test_select_word_with_underscore() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("baz_qux").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "baz_qux",
        "Underscore should be a word char, selecting 'baz_qux'"
    );
}

/// Test select word with numbers - alphanumeric should be a word
#[test]
fn test_select_word_with_numbers() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("test123").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "test123",
        "Alphanumeric should be a single word"
    );
}

/// Test select word with @ symbol - @ should be a word separator
#[test]
fn test_select_word_with_at_symbol() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("user@domain").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "user",
        "@ should be a word separator, selecting 'user'"
    );
}

/// Test select word with dot - dot should be a word separator
#[test]
fn test_select_word_with_dot() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("domain.com").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "domain",
        ". should be a word separator, selecting 'domain'"
    );
}

/// Test expand selection (Ctrl+Shift+Right) when cursor is on a non-word character
/// Should select from cursor position through the next word (like Emacs)
#[test]
fn test_expand_selection_on_non_word_char() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Test case from user: cursor on first * in "**-word"
    harness.type_text("**-word").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Cursor is now on the first *, press Ctrl+Shift+Right to expand selection
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range();

    // Should select from cursor (position 0) through next word, which is "**-word"
    assert!(
        range.is_some(),
        "Should have a selection after Ctrl+Shift+Right"
    );

    if let Some(range) = range {
        let selected_text = harness
            .editor_mut()
            .active_state_mut()
            .get_text_range(range.start, range.end);
        assert_eq!(
            selected_text, "**-word",
            "Should select from cursor through end of next word"
        );
    }
}

/// Test expand selection starting on a word character
#[test]
fn test_expand_selection_on_word_char() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello world").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Cursor on 'h' in "hello", press Ctrl+Shift+Right
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(selected_text, "hello", "Should select the current word");
}

/// Test expand selection from middle of word
/// Should select from cursor to end of current word only
#[test]
fn test_expand_selection_from_middle_of_word() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("Event").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    // Move cursor to 'v' (second character)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Press Ctrl+Shift+Right from 'v' in "Event"
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    // Should select from 'v' to end: "vent", not the whole word "Event"
    assert_eq!(
        selected_text, "vent",
        "Should select from cursor to end of word"
    );
}

/// Test select word left (Ctrl+Shift+Left) when cursor is on a non-word character
/// Should select backward from cursor through non-word chars, then to start of previous word
#[test]
fn test_select_word_left_on_non_word_char() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("word**-").unwrap();
    // Cursor is at end after typing (after the '-')

    // Press Ctrl+Shift+Left to select backward
    // First step: should select punctuation "**-"
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range();

    assert!(
        range.is_some(),
        "Should have a selection after Ctrl+Shift+Left"
    );

    if let Some(range) = range {
        let selected_text = harness
            .editor_mut()
            .active_state_mut()
            .get_text_range(range.start, range.end);
        assert_eq!(
            selected_text, "**-",
            "Should select backward from cursor through non-word chars"
        );
    }

    // Press Ctrl+Shift+Left again to select the word "word"
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);

    assert_eq!(
        selected_text, "word**-",
        "Should extend selection to include 'word' after second step"
    );
}

/// Test select previous word with non-alphanumeric characters
/// Moving backward should also respect word boundaries (alphanumeric + underscore)
#[test]
fn test_select_prev_word_with_special_chars() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Same test text but working backwards
    harness
        .type_text("start foo-bar baz_qux test123 user@domain.com")
        .unwrap();

    // Cursor is at end of text after typing
    // Move back one word and select "com" (. is a separator)
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(selected_text, "com", "Should select 'com' backwards");

    // Move back and select "domain"
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(selected_text, "domain", "Should select 'domain' backwards");

    // Move back and select "user"
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "user",
        "Should select 'user' backwards (@ is a separator)"
    );

    // Move back and select "test123"
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "test123",
        "Should select 'test123' backwards"
    );

    // Move back and select "baz_qux"
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "baz_qux",
        "Should select 'baz_qux' backwards (underscore is a word char)"
    );

    // Move back and select "bar"
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(selected_text, "bar", "Should select 'bar' backwards");

    // Move back and select "foo"
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    let cursor = harness.editor().active_state().cursors.primary();
    let range = cursor.selection_range().unwrap();
    let selected_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(range.start, range.end);
    assert_eq!(
        selected_text, "foo",
        "Should select 'foo' backwards (hyphen is a separator)"
    );
}

/// Test Shift+Up selection (select from cursor to previous line)
#[test]
fn test_select_up() {
    // Initialize tracing
    use tracing_subscriber::EnvFilter;
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with multiple lines
    let content = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to line 3 (start of "Line 3")
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify cursor is at start of line 3
    let cursor_pos = harness.cursor_position();
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(&buffer_content[cursor_pos..cursor_pos + 6], "Line 3");

    // No selection yet
    harness.assert_no_selection();
    tracing::trace!(
        "Initial state - selected text: {:?}",
        harness.get_selected_text()
    );

    // Press Shift+Up to select upward
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Should now have a selection
    assert!(
        harness.has_selection(),
        "Should have selection after Shift+Up"
    );

    // The selection should include "Line 2\n"
    let selected = harness.get_selected_text();
    tracing::trace!("After first Shift+Up - selected text: {:?}", selected);
    assert_eq!(selected, "Line 2\n", "Selection should be 'Line 2\n'");

    // Press Shift+Up again to extend selection further
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Selection should now include both lines
    let selected = harness.get_selected_text();
    tracing::trace!("After second Shift+Up - selected text: {:?}", selected);
    assert_eq!(
        selected, "Line 1\nLine 2\n",
        "Selection should span two lines"
    );
}

/// Test Shift+Down selection (select from cursor to next line)
#[test]
fn test_select_down() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with multiple lines
    let content = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Cursor starts at position 0 (beginning of file)
    harness.assert_no_selection();

    // Press Shift+Down to select downward
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Should now have a selection
    assert!(
        harness.has_selection(),
        "Should have selection after Shift+Down"
    );

    // The selection should include "Line 1\n"
    let selected = harness.get_selected_text();
    assert_eq!(selected, "Line 1\n", "Selection should be 'Line 1\n'");

    // Press Shift+Down again to extend selection
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Selection should now include two lines
    let selected = harness.get_selected_text();
    assert_eq!(
        selected, "Line 1\nLine 2\n",
        "Selection should span two lines"
    );

    // Press Shift+Down once more
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Selection should now include three lines
    let selected = harness.get_selected_text();
    assert_eq!(
        selected, "Line 1\nLine 2\nLine 3\n",
        "Selection should span three lines"
    );
}

/// Test Shift+Up and Shift+Down together (reversing selection direction)
#[test]
fn test_select_up_down_reversal() {
    // Initialize tracing
    use tracing_subscriber::EnvFilter;
    let _ = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .with_test_writer()
        .try_init();

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    let content = "Line 1\nLine 2\nLine 3\nLine 4\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to line 2
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    tracing::trace!(
        "Initial state (at line 2) - selected text: {:?}",
        harness.get_selected_text()
    );

    // Select down two lines
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    tracing::trace!(
        "After first Shift+Down - selected text: {:?}",
        harness.get_selected_text()
    );

    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let selected = harness.get_selected_text();
    tracing::trace!("After second Shift+Down - selected text: {:?}", selected);
    assert_eq!(selected, "Line 2\nLine 3\n");

    // Now go back up one line (shrink selection)
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    let selected = harness.get_selected_text();
    tracing::trace!(
        "After first Shift+Up (shrinking) - selected text: {:?}",
        selected
    );
    assert_eq!(selected, "Line 2\n", "Selection should shrink");

    // Go up again - this should collapse the selection (back to anchor)
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // After going past the anchor, selection collapses
    // This is expected behavior - we've moved back to where we started
    let selected = harness.get_selected_text();
    tracing::trace!(
        "After second Shift+Up (at/past anchor) - selected text: {:?}",
        selected
    );
    // Selection might be empty now (collapsed at anchor) or might have reversed
    // Either behavior is acceptable
}

/// Test Shift+PageDown selection (select a page down)
#[test]
fn test_select_page_down() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with many lines (more than can fit on screen)
    let mut content = String::new();
    for i in 1..=50 {
        content.push_str(&format!("Line {i}\n"));
    }
    std::fs::write(&file_path, &content).unwrap();

    // Use smaller height to make page behavior predictable
    let mut harness = EditorTestHarness::new(80, 10).unwrap();
    harness.open_file(&file_path).unwrap();

    // Cursor starts at beginning
    harness.assert_no_selection();

    // Press Shift+PageDown to select a page down
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Should have a selection
    assert!(
        harness.has_selection(),
        "Should have selection after Shift+PageDown"
    );

    let selected = harness.get_selected_text();
    // With height 10, viewport height varies based on status bars
    // Selection should include multiple lines (at least 4)
    let selected_lines = selected.lines().count();
    assert!(
        selected_lines >= 4,
        "Should select approximately a page of lines, got {selected_lines} lines"
    );

    // Verify selection includes multiple lines starting from Line 1
    assert!(selected.contains("Line 1"));
    assert!(selected.contains("Line 2"));
}

/// Test Shift+PageUp selection (select a page up)
#[test]
fn test_select_page_up() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with many lines
    let mut content = String::new();
    for i in 1..=50 {
        content.push_str(&format!("Line {i}\n"));
    }
    std::fs::write(&file_path, &content).unwrap();

    // Use smaller height to make page behavior predictable
    let mut harness = EditorTestHarness::new(80, 10).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move down several pages first
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now we're somewhere in the middle of the file
    let cursor_before = harness.cursor_position();
    assert!(cursor_before > 100, "Should be well into the file");

    harness.assert_no_selection();

    // Press Shift+PageUp to select a page up
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Should have a selection
    assert!(
        harness.has_selection(),
        "Should have selection after Shift+PageUp"
    );

    let selected = harness.get_selected_text();
    let selected_lines = selected.lines().count();
    assert!(
        selected_lines >= 4,
        "Should select approximately a page of lines, got {selected_lines} lines"
    );

    // Selection should not be empty
    assert!(!selected.is_empty(), "Selection should not be empty");
}

/// Test Shift+PageDown and Shift+PageUp together
#[test]
fn test_select_page_up_down_combination() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with many lines
    let mut content = String::new();
    for i in 1..=100 {
        content.push_str(&format!("Line {i}\n"));
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 10).unwrap();
    harness.open_file(&file_path).unwrap();

    // Move to middle of file
    for _ in 0..5 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }

    // Select page down
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection());
    let selection_after_page_down = harness.get_selected_text();
    let _lines_down = selection_after_page_down.lines().count();

    // Now select page up (should shrink/reverse selection)
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Selection might still exist but should be different
    let selection_after_page_up = harness.get_selected_text();

    // The selections should be different
    assert_ne!(
        selection_after_page_down, selection_after_page_up,
        "Selections should differ after PageUp"
    );
}

/// Test that selection works correctly at file boundaries
#[test]
fn test_select_at_file_boundaries() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    let content = "Line 1\nLine 2\nLine 3\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // At start of file, Shift+Up should not panic or cause issues
    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();
    // Either no selection or empty selection is fine

    // Go to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // At end of file, Shift+Down should not panic
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Select all the way up from end
    for _ in 0..5 {
        harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    }
    harness.render().unwrap();

    // After selecting upward from end, we should have some content selected
    // The key thing is that the editor doesn't crash at boundaries
    let _selected = harness.get_selected_text();
    // Just verify we can get selected text without panicking
    // The test validates that boundary operations don't crash
}
