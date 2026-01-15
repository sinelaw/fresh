//! E2E tests for virtual lines (LineAbove/LineBelow)
//!
//! Tests the Emacs-like persistent state model where plugins can add virtual lines
//! that appear in the render output without affecting the source buffer.

use crate::common::harness::EditorTestHarness;
use fresh::view::virtual_text::{VirtualTextNamespace, VirtualTextPosition};
use ratatui::style::{Color, Style};
use tempfile::TempDir;

/// Helper to create a dimmed style for virtual lines
fn virtual_line_style() -> Style {
    Style::default().fg(Color::DarkGray)
}

/// Test that virtual lines can be added above source lines
#[test]
fn test_virtual_line_above() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add a virtual line above line 2 (position 7 is start of "Line 2")
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            7, // byte offset of "Line 2"
            "--- Header Above Line 2 ---".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("test".to_string()),
            0,
        );
    }

    harness.render().unwrap();
    let screen = harness.screen_to_string();

    // Virtual line should appear in the rendered output
    assert!(
        screen.contains("--- Header Above Line 2 ---"),
        "Virtual line should be visible. Screen:\n{screen}"
    );

    // Original lines should still be present
    assert!(screen.contains("Line 1"), "Line 1 should be visible");
    assert!(screen.contains("Line 2"), "Line 2 should be visible");
    assert!(screen.contains("Line 3"), "Line 3 should be visible");
}

/// Test that virtual lines can be added below source lines
#[test]
fn test_virtual_line_below() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add a virtual line below line 1 (position 0 is start of "Line 1")
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0, // byte offset of "Line 1"
            "--- Footer Below Line 1 ---".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineBelow,
            VirtualTextNamespace::from_string("test".to_string()),
            0,
        );
    }

    harness.render().unwrap();
    let screen = harness.screen_to_string();

    // Virtual line should appear in the rendered output
    assert!(
        screen.contains("--- Footer Below Line 1 ---"),
        "Virtual line should be visible. Screen:\n{screen}"
    );
}

/// Test that multiple virtual lines can be added at the same position
#[test]
fn test_multiple_virtual_lines_same_position() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add two virtual lines above line 1, with different priorities
    {
        let state = harness.editor_mut().active_state_mut();

        // Lower priority (renders first)
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "First Header".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("test".to_string()),
            0, // priority 0
        );

        // Higher priority (renders second, closer to source line)
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "Second Header".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("test".to_string()),
            10, // priority 10
        );
    }

    harness.render().unwrap();
    let screen = harness.screen_to_string();

    // Both virtual lines should be visible
    assert!(
        screen.contains("First Header"),
        "First header should be visible"
    );
    assert!(
        screen.contains("Second Header"),
        "Second header should be visible"
    );
}

/// Test clearing virtual lines by namespace
#[test]
fn test_clear_namespace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add virtual lines in two different namespaces
    {
        let state = harness.editor_mut().active_state_mut();

        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "Git Blame Header".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("git-blame".to_string()),
            0,
        );

        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "LSP Diagnostic".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("lsp".to_string()),
            0,
        );
    }

    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(screen.contains("Git Blame Header"));
    assert!(screen.contains("LSP Diagnostic"));

    // Clear only the git-blame namespace
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.clear_namespace(
            &mut state.marker_list,
            &VirtualTextNamespace::from_string("git-blame".to_string()),
        );
    }

    harness.render().unwrap();
    let screen = harness.screen_to_string();

    // Git blame header should be gone, LSP diagnostic should remain
    assert!(
        !screen.contains("Git Blame Header"),
        "Git blame header should be cleared"
    );
    assert!(
        screen.contains("LSP Diagnostic"),
        "LSP diagnostic should remain"
    );
}

/// Test that virtual lines don't have line numbers in the gutter
#[test]
fn test_virtual_lines_no_line_numbers() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add a virtual line
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            7, // position of "Line 2"
            "VIRTUAL".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("test".to_string()),
            0,
        );
    }

    harness.render().unwrap();
    let screen = harness.screen_to_string();

    // Find the row with "VIRTUAL" and check it doesn't have a line number
    let lines: Vec<&str> = screen.lines().collect();
    for line in &lines {
        if line.contains("VIRTUAL") {
            // The virtual line shouldn't have a typical line number pattern
            // Line numbers look like "  1 " or " 10 " at the start
            // Virtual lines should have a different gutter appearance
            assert!(
                !line.trim_start().starts_with(|c: char| c.is_ascii_digit()),
                "Virtual line should not start with a line number: {line}"
            );
        }
    }
}

/// Test that virtual lines track position when buffer content changes
#[test]
fn test_virtual_line_position_tracking() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "AAA\nBBB\nCCC").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add a virtual line above "BBB" (position 4)
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            4, // byte offset of "BBB"
            "--- Above BBB ---".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("test".to_string()),
            0,
        );
    }

    harness.render().unwrap();
    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains("--- Above BBB ---"),
        "Virtual line should be visible before edit"
    );

    // Insert a new line at the beginning of the file
    // This should push "BBB" down but the virtual line should stay anchored to it
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("NEW LINE\n").unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();

    // Virtual line should still be visible (marker should have tracked the edit)
    assert!(
        screen_after.contains("--- Above BBB ---"),
        "Virtual line should still be visible after edit. Screen:\n{screen_after}"
    );

    // Both the new line and BBB should be visible
    assert!(
        screen_after.contains("NEW LINE"),
        "New line should be visible"
    );
    assert!(screen_after.contains("BBB"), "BBB should still be visible");
}

/// Test adding virtual lines above and below the same source line
#[test]
fn test_virtual_lines_above_and_below_same_line() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Source Line").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add both above and below virtual lines
    {
        let state = harness.editor_mut().active_state_mut();

        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "=== ABOVE ===".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("test".to_string()),
            0,
        );

        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "=== BELOW ===".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineBelow,
            VirtualTextNamespace::from_string("test".to_string()),
            0,
        );
    }

    harness.render().unwrap();
    let screen = harness.screen_to_string();

    // All three lines should be visible
    assert!(screen.contains("=== ABOVE ==="), "Above line missing");
    assert!(screen.contains("Source Line"), "Source line missing");
    assert!(screen.contains("=== BELOW ==="), "Below line missing");

    // Verify ordering: ABOVE should come before Source, Source before BELOW
    let above_pos = screen.find("=== ABOVE ===").unwrap();
    let source_pos = screen.find("Source Line").unwrap();
    let below_pos = screen.find("=== BELOW ===").unwrap();

    assert!(
        above_pos < source_pos,
        "ABOVE should appear before Source Line"
    );
    assert!(
        source_pos < below_pos,
        "Source Line should appear before BELOW"
    );
}

/// Test that virtual text count is tracked correctly
#[test]
fn test_virtual_text_count() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Initially no virtual texts
    {
        let state = harness.editor().active_state();
        assert_eq!(state.virtual_texts.len(), 0);
        assert!(state.virtual_texts.is_empty());
    }

    // Add some virtual lines
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "Line 1".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("ns1".to_string()),
            0,
        );
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "Line 2".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("ns1".to_string()),
            0,
        );
    }

    {
        let state = harness.editor().active_state();
        assert_eq!(state.virtual_texts.len(), 2);
        assert!(!state.virtual_texts.is_empty());
    }

    // Clear namespace
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.clear_namespace(
            &mut state.marker_list,
            &VirtualTextNamespace::from_string("ns1".to_string()),
        );
    }

    {
        let state = harness.editor().active_state();
        assert_eq!(state.virtual_texts.len(), 0);
        assert!(state.virtual_texts.is_empty());
    }
}
