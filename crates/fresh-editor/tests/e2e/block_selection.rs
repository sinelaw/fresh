use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test basic block selection with Alt+Shift+Down creates visible selection
#[test]
fn test_block_select_down_basic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines with consistent content
    harness
        .type_text("line1 text here\nline2 text here\nline3 text here")
        .unwrap();

    // Move to start of buffer
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Move to column 6 (start of "text")
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Before block selection - should have no selection
    harness.assert_no_selection();

    // Press Alt+Shift+Down for block selection
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // After block selection - should have a selection
    assert!(
        harness.has_selection(),
        "Should have selection after Alt+Shift+Down"
    );
}

/// Test multiple consecutive block selections don't break state
#[test]
fn test_block_select_multiple_consecutive() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines
    harness
        .type_text("aaaa bbbb cccc\naaaa bbbb cccc\naaaa bbbb cccc\naaaa bbbb cccc\naaaa bbbb cccc")
        .unwrap();

    // Move to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Move to column 5
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // First block select down
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Should have selection after first Alt+Shift+Down"
    );

    // Second block select down - should extend selection
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Should still have selection after second Alt+Shift+Down"
    );

    // Third block select down
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Should still have selection after third Alt+Shift+Down"
    );
}

/// Test block selection followed by Escape clears selection
#[test]
fn test_block_select_then_escape() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness
        .type_text("line1 text\nline2 text\nline3 text")
        .unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Start block selection
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have selection");

    // Press Escape to clear selection
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.assert_no_selection();
}

/// Test block selection in all four directions
#[test]
fn test_block_select_all_directions() {
    // Test Alt+Shift+Down
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.type_text("aaaa\nbbbb\ncccc").unwrap();
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .unwrap();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();

        harness
            .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
        harness.render().unwrap();

        assert!(
            harness.has_selection(),
            "Alt+Shift+Down should create selection"
        );
    }

    // Test Alt+Shift+Up
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.type_text("aaaa\nbbbb\ncccc").unwrap();
        // Move to line 2
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();

        harness
            .send_key(KeyCode::Up, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
        harness.render().unwrap();

        assert!(
            harness.has_selection(),
            "Alt+Shift+Up should create selection"
        );
    }

    // Test Alt+Shift+Right
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.type_text("aaaa\nbbbb\ncccc").unwrap();
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .unwrap();

        harness
            .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
        harness.render().unwrap();

        assert!(
            harness.has_selection(),
            "Alt+Shift+Right should create selection"
        );
    }

    // Test Alt+Shift+Left
    {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.type_text("aaaa\nbbbb\ncccc").unwrap();
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .unwrap();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();

        harness
            .send_key(KeyCode::Left, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
        harness.render().unwrap();

        assert!(
            harness.has_selection(),
            "Alt+Shift+Left should create selection"
        );
    }
}

/// Test that block selection persists through multiple cycles
/// This reproduces the bug where block selection works initially but then
/// stops working after a few uses
#[test]
fn test_block_select_persistence_across_cycles() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create content
    harness
        .type_text("1111 2222 3333\n1111 2222 3333\n1111 2222 3333\n1111 2222 3333")
        .unwrap();

    // First block selection cycle
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "First cycle: should have selection"
    );

    // Clear selection with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.assert_no_selection();

    // Second block selection cycle - this should still work!
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Second cycle: should have selection (fails if there's a state persistence bug)"
    );

    // Third cycle - make sure it still works
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Third cycle: should still have selection"
    );
}

/// Test block selection followed by typing replaces the selection
#[test]
fn test_block_select_then_type() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("aaaa\nbbbb\ncccc").unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Create block selection spanning 2 lines
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Should have selection before typing"
    );

    // Type something - should replace block selection
    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // After typing, selection should be cleared
    harness.assert_no_selection();
}

/// Test that normal selection (Shift+Arrow) followed by block selection works
#[test]
fn test_normal_then_block_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness
        .type_text("aaaa bbbb\ncccc dddd\neeee ffff")
        .unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // First do a normal selection
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.has_selection(), "Should have normal selection");

    // Clear selection
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.assert_no_selection();

    // Now do block selection
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Block selection should work after normal selection was cleared"
    );
}

/// Test block selection visual rendering shows rectangular selection
/// Block selection should only highlight a rectangular region, not the entire
/// range between anchor and cursor
#[test]
fn test_block_selection_renders_rectangular() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create lines with predictable content
    harness
        .type_text("0123456789\n0123456789\n0123456789")
        .unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Move to column 2
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Block select down and right to create a 2x3 rectangle (columns 2-4, lines 0-1)
    harness
        .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Get the screen and verify selection rendering
    let buffer = harness.buffer();
    let theme = harness.editor().theme();
    let selection_bg = theme.selection_bg;

    // Get content area bounds
    let (content_first_row, _content_last_row) = harness.content_area_rows();
    let first_line_row = content_first_row as u16;
    let gutter_width = 8; // " " + "   1" + " │ "

    // Check that characters in the block region have selection background
    // Block should be columns 2-4 on lines 0 and 1

    // Line 0 (first content line), column 2 (character '2') - should be selected
    let pos_line0_col2 = buffer.index_of(gutter_width + 2, first_line_row);
    let cell = &buffer.content[pos_line0_col2];
    assert_eq!(cell.symbol(), "2");
    assert_eq!(
        cell.bg, selection_bg,
        "Block selection: line 0 column 2 should have selection background"
    );

    // Line 1 (second content line), column 2 (character '2') - should be selected
    let pos_line1_col2 = buffer.index_of(gutter_width + 2, first_line_row + 1);
    let cell = &buffer.content[pos_line1_col2];
    assert_eq!(cell.symbol(), "2");
    assert_eq!(
        cell.bg, selection_bg,
        "Block selection: line 1 column 2 should have selection background"
    );

    // Line 0, column 0 (character '0') - should NOT be selected (outside block)
    let pos_line0_col0 = buffer.index_of(gutter_width, first_line_row);
    let cell = &buffer.content[pos_line0_col0];
    assert_eq!(cell.symbol(), "0");
    assert_ne!(
        cell.bg, selection_bg,
        "Block selection: line 0 column 0 should NOT have selection background"
    );

    // Line 1, column 0 (character '0') - should NOT be selected (outside block)
    // This is the key test for block vs normal selection - normal selection would
    // include all characters from anchor to cursor, but block selection only
    // includes the rectangular region
    let pos_line1_col0 = buffer.index_of(gutter_width, first_line_row + 1);
    let cell = &buffer.content[pos_line1_col0];
    assert_eq!(cell.symbol(), "0");
    assert_ne!(
        cell.bg, selection_bg,
        "Block selection: line 1 column 0 should NOT have selection background (this fails if block renders as normal selection)"
    );
}
