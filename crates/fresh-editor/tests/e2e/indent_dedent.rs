use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Helper to create a harness with spaces for indentation
fn harness_with_spaces() -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.tab_size = 4;
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.enable_shadow_validation();
    harness
}

/// Helper to create a harness with tabs for indentation (for Go files)
fn harness_with_tabs() -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.tab_size = 4;
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.enable_shadow_validation();
    harness
}

// =============================================================================
// Tab Key Tests (with spaces)
// =============================================================================

/// Test Tab key indents a single line with spaces
#[test]
fn test_tab_indent_single_line_spaces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Hello world").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Move cursor to beginning of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "    Hello world",
        "Tab should indent line with 4 spaces"
    );
}

/// Test Tab key indents multiple selected lines with spaces
#[test]
fn test_tab_indent_multiple_lines_spaces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Select all three lines (from beginning to end)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Press Tab to indent all selected lines
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "    Line 1\n    Line 2\n    Line 3",
        "Tab should indent all selected lines with 4 spaces each"
    );
}

/// Test Tab key indents partial selection (multiple lines)
#[test]
fn test_tab_indent_partial_selection_spaces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Move to middle of Line 1
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Select down to Line 2
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Press Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "    Line 1\n    Line 2\nLine 3",
        "Tab should indent both selected lines even with partial selection"
    );
}

// =============================================================================
// Shift+Tab Key Tests (with spaces)
// =============================================================================

/// Test Shift+Tab dedents a single line with spaces
#[test]
fn test_shift_tab_dedent_single_line_spaces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "    Hello world").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Move cursor somewhere in the line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Shift+Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "Hello world", "Shift+Tab should remove 4 spaces");
}

/// Test Shift+Tab dedents multiple selected lines with spaces
#[test]
fn test_shift_tab_dedent_multiple_lines_spaces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "    Line 1\n    Line 2\n    Line 3").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Select all three lines
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Press Shift+Tab to dedent all selected lines
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "Line 1\nLine 2\nLine 3",
        "Shift+Tab should dedent all selected lines"
    );
}

/// Test Shift+Tab on line with fewer than tab_size spaces
#[test]
fn test_shift_tab_dedent_fewer_spaces() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "  Hello world").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Press Shift+Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "Hello world",
        "Shift+Tab should remove only the 2 spaces present"
    );
}

/// Test Shift+Tab on line with no indentation
#[test]
fn test_shift_tab_dedent_no_indentation() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Hello world").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Press Shift+Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "Hello world",
        "Shift+Tab on non-indented line should do nothing"
    );
}

// =============================================================================
// Tab Character Tests (for languages like Go)
// =============================================================================

/// Test Tab key indents with actual tab character
#[test]
fn test_tab_indent_with_tab_character() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");
    std::fs::write(&file_path, "func main() {").unwrap();

    let mut harness = harness_with_tabs();
    harness.open_file(&file_path).unwrap();

    // Move cursor to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "\tfunc main() {",
        "Tab should insert actual tab character for Go files"
    );
}

/// Test Tab key indents multiple lines with tab characters
#[test]
fn test_tab_indent_multiple_lines_with_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");
    std::fs::write(&file_path, "func main() {\nfmt.Println(\"Hello\")\n}").unwrap();

    let mut harness = harness_with_tabs();
    harness.open_file(&file_path).unwrap();

    // Select all lines
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Press Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "\tfunc main() {\n\tfmt.Println(\"Hello\")\n\t}",
        "Tab should indent all lines with tab characters"
    );
}

/// Test Shift+Tab dedents lines with tab characters
#[test]
fn test_shift_tab_dedent_with_tab_character() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");
    std::fs::write(&file_path, "\tfunc main() {").unwrap();

    let mut harness = harness_with_tabs();
    harness.open_file(&file_path).unwrap();

    // Press Shift+Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "func main() {",
        "Shift+Tab should remove tab character"
    );
}

/// Test Shift+Tab dedents multiple lines with tab characters
#[test]
fn test_shift_tab_dedent_multiple_lines_with_tabs() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.go");
    std::fs::write(&file_path, "\tfunc main() {\n\tfmt.Println(\"Hello\")\n\t}").unwrap();

    let mut harness = harness_with_tabs();
    harness.open_file(&file_path).unwrap();

    // Select all lines
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Press Shift+Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "func main() {\nfmt.Println(\"Hello\")\n}",
        "Shift+Tab should dedent all lines with tab characters"
    );
}

// =============================================================================
// Mixed Indentation Tests
// =============================================================================

/// Test that dedent works on lines with mixed tab/space indentation
#[test]
fn test_shift_tab_dedent_mixed_indentation() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    // Line with tab, line with spaces
    std::fs::write(&file_path, "\tTab line\n    Space line").unwrap();

    let mut harness = harness_with_tabs();
    harness.open_file(&file_path).unwrap();

    // Select all
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Press Shift+Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "Tab line\nSpace line",
        "Shift+Tab should handle both tab and space indentation"
    );
}

// =============================================================================
// Selection Preservation Tests
// =============================================================================

/// Test that Tab preserves selection after indenting
#[test]
fn test_tab_preserves_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Select all three lines
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Press Tab to indent
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify content is indented
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "    Line 1\n    Line 2\n    Line 3",
        "All lines should be indented"
    );

    // Verify selection is still active and covers the indented lines
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(
        cursor.selection_range().is_some(),
        "Selection should be preserved after indenting"
    );

    let selection = cursor.selection_range().unwrap();
    // Selection should start at beginning (position 0 + 4 spaces = 4)
    // and end at the end of file (original 20 chars + 12 spaces = 32)
    assert_eq!(
        selection.start, 4,
        "Selection should start after first indent"
    );
    assert_eq!(
        selection.end, 32,
        "Selection should end at end of indented content"
    );
}

/// Test that Shift+Tab preserves selection after dedenting
#[test]
fn test_shift_tab_preserves_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "    Line 1\n    Line 2\n    Line 3").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Select all three lines
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Press Shift+Tab to dedent
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Verify content is dedented
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "Line 1\nLine 2\nLine 3",
        "All lines should be dedented"
    );

    // Verify selection is still active
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(
        cursor.selection_range().is_some(),
        "Selection should be preserved after dedenting"
    );

    let selection = cursor.selection_range().unwrap();
    // Selection should start at beginning (0) and end at end (20 chars)
    assert_eq!(selection.start, 0, "Selection should start at beginning");
    assert_eq!(
        selection.end, 20,
        "Selection should end at end of dedented content"
    );
}

/// Test that multiple indent/dedent cycles preserve selection
#[test]
fn test_multiple_indent_dedent_preserves_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Select both lines
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Indent twice
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify content
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "        Line 1\n        Line 2");

    // Verify selection is preserved
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(
        cursor.selection_range().is_some(),
        "Selection should still be active"
    );

    // Dedent once
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Verify content
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "    Line 1\n    Line 2");

    // Verify selection is still preserved
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(
        cursor.selection_range().is_some(),
        "Selection should still be active after dedent"
    );
}

/// Test Shift+Tab moves cursor back when dedenting without selection
#[test]
fn test_dedent_moves_cursor_without_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "    Hello world").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Move cursor to position 8 (after the indentation, before 'H')
    // Buffer: "    Hello world"
    //          0123456789...
    // Position 8 is right before 'H'
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Verify cursor is at position 8
    let cursor_before = harness.editor().active_state().cursors.primary().position;
    assert_eq!(
        cursor_before, 8,
        "Cursor should be at position 8 before dedent"
    );

    // Press Shift+Tab to dedent (no selection)
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Verify content was dedented
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "Hello world", "Line should be dedented");

    // Verify cursor moved back by 4 (the amount of indentation removed)
    let cursor_after = harness.editor().active_state().cursors.primary().position;
    assert_eq!(
        cursor_after, 4,
        "Cursor should have moved from position 8 to 4 (moved back by 4)"
    );

    // Verify no selection
    let cursor = harness.editor().active_state().cursors.primary();
    assert!(
        cursor.selection_range().is_none(),
        "Should not have a selection"
    );
}

/// Test multi-cursor indent with selections
#[test]
fn test_multicursor_indent_with_selections() {
    use fresh::model::event::{CursorId, Event};

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    // Create a file with 3 lines
    std::fs::write(&file_path, "Line one\nLine two\nLine three").unwrap();

    let mut harness = harness_with_spaces();
    harness.open_file(&file_path).unwrap();

    // Manually set up multi-cursor with selections
    // Line one: "Line one" (0-8)
    // Line two: "Line two" (9-17)
    // Line three: "Line three" (18-28)

    let editor = harness.editor_mut();
    let state = editor.active_state_mut();

    // Add two more cursors with selections (already have primary cursor)
    // Cursor 1 (CursorId(1)): Select "Line" on first line (anchor 0, position 4)
    state.apply(&Event::AddCursor {
        cursor_id: CursorId(1),
        position: 4,
        anchor: Some(0),
    });

    // Cursor 2 (CursorId(2)): Select "Line" on second line (anchor 9, position 13)
    state.apply(&Event::AddCursor {
        cursor_id: CursorId(2),
        position: 13,
        anchor: Some(9),
    });

    // Cursor 3 (CursorId(3)): Select "Line" on third line (anchor 18, position 22)
    state.apply(&Event::AddCursor {
        cursor_id: CursorId(3),
        position: 22,
        anchor: Some(18),
    });

    let _ = state;
    let _ = editor;

    harness.render().unwrap();

    // Verify initial cursor positions
    {
        let state = harness.editor().active_state();
        let cursors: Vec<_> = state.cursors.iter().collect();
        assert_eq!(
            cursors.len(),
            4,
            "Should have 4 cursors (1 primary + 3 added)"
        );

        // Check cursor 1
        let c1 = state.cursors.get(CursorId(1)).unwrap();
        assert_eq!(c1.position, 4);
        assert_eq!(c1.anchor, Some(0));

        // Check cursor 2
        let c2 = state.cursors.get(CursorId(2)).unwrap();
        assert_eq!(c2.position, 13);
        assert_eq!(c2.anchor, Some(9));

        // Check cursor 3
        let c3 = state.cursors.get(CursorId(3)).unwrap();
        assert_eq!(c3.position, 22);
        assert_eq!(c3.anchor, Some(18));
    }

    // Press Tab 3 times to test multiple consecutive indents
    for indent_count in 1..=3 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        // Verify content after each indent
        let expected_indent = "    ".repeat(indent_count);
        let expected_content = format!(
            "{}Line one\n{}Line two\n{}Line three",
            expected_indent, expected_indent, expected_indent
        );
        let content = harness.get_buffer_content().unwrap();
        assert_eq!(
            content,
            expected_content,
            "After {} indent(s), all lines should have {} spaces",
            indent_count,
            indent_count * 4
        );

        // Verify all cursors and selections shifted correctly
        let state = harness.editor().active_state();
        let indent_offset = indent_count * 4;

        // Cursor 1: original (0-4), shifts by 4 per indent on its line
        let c1 = state.cursors.get(CursorId(1)).unwrap();
        assert_eq!(
            c1.position,
            4 + indent_offset,
            "Indent {}: Cursor 1 position",
            indent_count
        );
        assert_eq!(
            c1.anchor,
            Some(indent_offset),
            "Indent {}: Cursor 1 anchor",
            indent_count
        );

        // Cursor 2: original (9-13), line 1 and line 2 both indented
        let c2 = state.cursors.get(CursorId(2)).unwrap();
        assert_eq!(
            c2.position,
            13 + indent_offset * 2,
            "Indent {}: Cursor 2 position",
            indent_count
        );
        assert_eq!(
            c2.anchor,
            Some(9 + indent_offset * 2),
            "Indent {}: Cursor 2 anchor",
            indent_count
        );

        // Cursor 3: original (18-22), all three lines indented
        let c3 = state.cursors.get(CursorId(3)).unwrap();
        assert_eq!(
            c3.position,
            22 + indent_offset * 3,
            "Indent {}: Cursor 3 position",
            indent_count
        );
        assert_eq!(
            c3.anchor,
            Some(18 + indent_offset * 3),
            "Indent {}: Cursor 3 anchor",
            indent_count
        );
    }

    // Press Shift+Tab 3 times to dedent back to original
    for dedent_count in 1..=3 {
        harness
            .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
            .unwrap();
        harness.render().unwrap();

        let remaining_indents = 3 - dedent_count;
        let expected_indent = "    ".repeat(remaining_indents);
        let expected_content = format!(
            "{}Line one\n{}Line two\n{}Line three",
            expected_indent, expected_indent, expected_indent
        );
        let content = harness.get_buffer_content().unwrap();
        assert_eq!(
            content,
            expected_content,
            "After {} dedent(s), all lines should have {} spaces",
            dedent_count,
            remaining_indents * 4
        );

        // Verify cursor positions
        let state = harness.editor().active_state();
        let indent_offset = remaining_indents * 4;

        let c1 = state.cursors.get(CursorId(1)).unwrap();
        assert_eq!(
            c1.position,
            4 + indent_offset,
            "Dedent {}: Cursor 1 position",
            dedent_count
        );
        assert_eq!(
            c1.anchor,
            Some(indent_offset),
            "Dedent {}: Cursor 1 anchor",
            dedent_count
        );

        let c2 = state.cursors.get(CursorId(2)).unwrap();
        assert_eq!(
            c2.position,
            13 + indent_offset * 2,
            "Dedent {}: Cursor 2 position",
            dedent_count
        );
        assert_eq!(
            c2.anchor,
            Some(9 + indent_offset * 2),
            "Dedent {}: Cursor 2 anchor",
            dedent_count
        );

        let c3 = state.cursors.get(CursorId(3)).unwrap();
        assert_eq!(
            c3.position,
            22 + indent_offset * 3,
            "Dedent {}: Cursor 3 position",
            dedent_count
        );
        assert_eq!(
            c3.anchor,
            Some(18 + indent_offset * 3),
            "Dedent {}: Cursor 3 anchor",
            dedent_count
        );
    }

    // Verify we're back to original state
    {
        let state = harness.editor().active_state();
        let cursors: Vec<_> = state.cursors.iter().collect();
        assert_eq!(
            cursors.len(),
            4,
            "Should still have 4 cursors (1 primary + 3 added)"
        );

        let c1 = state.cursors.get(CursorId(1)).unwrap();
        assert_eq!(c1.position, 4, "Final: Cursor 1 position back to original");
        assert_eq!(
            c1.anchor,
            Some(0),
            "Final: Cursor 1 anchor back to original"
        );

        let c2 = state.cursors.get(CursorId(2)).unwrap();
        assert_eq!(c2.position, 13, "Final: Cursor 2 position back to original");
        assert_eq!(
            c2.anchor,
            Some(9),
            "Final: Cursor 2 anchor back to original"
        );

        let c3 = state.cursors.get(CursorId(3)).unwrap();
        assert_eq!(c3.position, 22, "Final: Cursor 3 position back to original");
        assert_eq!(
            c3.anchor,
            Some(18),
            "Final: Cursor 3 anchor back to original"
        );
    }
}
