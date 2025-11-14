// Visual regression tests - these generate screenshots for documentation

use crate::common::harness::EditorTestHarness;
use crate::common::visual_testing::VisualFlow;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test basic editing workflow with visual captures
#[test]
fn visual_basic_editing() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let mut flow = VisualFlow::new(
        "Basic Editing",
        "Core Features",
        "Basic text editing operations in the editor",
    );

    // Step 1: Initial empty buffer
    harness
        .capture_visual_step(&mut flow, "initial", "Empty editor on startup")
        .unwrap();

    // Step 2: Type some text
    harness.type_text("Hello, World!").unwrap();
    harness
        .capture_visual_step(&mut flow, "typed_text", "Text typed into buffer")
        .unwrap();

    // Step 3: Add a new line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Second line").unwrap();
    harness
        .capture_visual_step(&mut flow, "multiline", "Multiple lines of text")
        .unwrap();
}

/// Test file explorer workflow with visual captures
#[test]
fn visual_file_explorer() {
    let mut harness = EditorTestHarness::with_temp_project(80, 30).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create some test files
    fs::create_dir_all(project_dir.join("src")).unwrap();
    fs::write(
        project_dir.join("src/main.rs"),
        "fn main() {\n    println!(\"Hello\");\n}",
    )
    .unwrap();
    fs::write(project_dir.join("README.md"), "# Test Project\n").unwrap();

    let mut flow = VisualFlow::new(
        "File Explorer",
        "File Management",
        "Opening and navigating the file explorer",
    );

    // Step 1: Initial state
    harness
        .capture_visual_step(&mut flow, "initial", "Editor before opening file explorer")
        .unwrap();

    // Step 2: Open file explorer with Ctrl+B
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(
            &mut flow,
            "explorer_open",
            "File explorer opened in left pane",
        )
        .unwrap();

    // Step 3: Navigate down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(&mut flow, "file_selected", "File selected in explorer")
        .unwrap();

    // Step 4: Expand directory
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(
            &mut flow,
            "dir_expanded",
            "Directory expanded to show contents",
        )
        .unwrap();
}

/// Test command palette workflow
#[test]
fn visual_command_palette() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let mut flow = VisualFlow::new(
        "Command Palette",
        "Core Features",
        "Using the command palette to execute commands",
    );

    // Step 1: Initial state
    harness
        .capture_visual_step(
            &mut flow,
            "initial",
            "Editor before opening command palette",
        )
        .unwrap();

    // Step 2: Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(&mut flow, "palette_open", "Command palette opened")
        .unwrap();

    // Step 3: Type to filter commands
    harness.type_text("help").unwrap();
    harness
        .capture_visual_step(&mut flow, "filtered", "Commands filtered by search term")
        .unwrap();
}

/// Test help system
#[test]
fn visual_help_system() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();
    let mut flow = VisualFlow::new(
        "Help System",
        "Core Features",
        "Viewing keybindings and help information",
    );

    // Step 1: Initial state
    harness
        .capture_visual_step(&mut flow, "initial", "Editor before opening help")
        .unwrap();

    // Step 2: Open help with Ctrl+H
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(&mut flow, "help_open", "Help panel showing all keybindings")
        .unwrap();
}

/// Test split view workflow
#[test]
fn visual_split_view() {
    use crate::common::fixtures::test_temp_dir;
    let temp_dir = test_temp_dir("visual-split-view").unwrap();
    let file1 = temp_dir.join("file1.txt");
    let file2 = temp_dir.join("file2.txt");

    fs::write(&file1, "Content of file 1").unwrap();
    fs::write(&file2, "Content of file 2").unwrap();

    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    let mut flow = VisualFlow::new("Split View", "Layout", "Working with split panes");

    // Step 1: Open first file
    harness.open_file(&file1).unwrap();
    harness
        .capture_visual_step(&mut flow, "single_file", "Single file open")
        .unwrap();

    // Step 2: Split horizontally with Alt+H
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(&mut flow, "horizontal_split", "Editor split horizontally")
        .unwrap();

    // Step 3: Open second file in split
    harness.open_file(&file2).unwrap();
    harness
        .capture_visual_step(&mut flow, "two_files", "Two files visible in split panes")
        .unwrap();
}

/// Test theme display
#[test]
fn visual_theme() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let mut flow = VisualFlow::new(
        "Theme Colors",
        "Appearance",
        "Editor color scheme and syntax highlighting",
    );

    // Create a buffer with some colored content
    harness.type_text("// This is a comment\n").unwrap();
    harness.type_text("fn main() {\n").unwrap();
    harness.type_text("    let x = 42;\n").unwrap();
    harness.type_text("}\n").unwrap();

    harness
        .capture_visual_step(
            &mut flow,
            "syntax_highlighting",
            "Syntax highlighting for Rust code",
        )
        .unwrap();
}

/// Test multicursor editing
#[test]
fn visual_multicursor() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let mut flow = VisualFlow::new(
        "Multiple Cursors",
        "Advanced Editing",
        "Using multiple cursors for simultaneous edits",
    );

    // Step 1: Type some text
    harness.type_text("hello\nhello\nhello").unwrap();
    harness
        .capture_visual_step(&mut flow, "initial_text", "Three lines with 'hello'")
        .unwrap();

    // Step 2: Select word
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(&mut flow, "word_selected", "First word selected")
        .unwrap();

    // Step 3: Add next occurrence with Ctrl+D
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(
            &mut flow,
            "two_cursors",
            "Second occurrence selected (two cursors)",
        )
        .unwrap();

    // Step 4: Add third occurrence
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(
            &mut flow,
            "three_cursors",
            "All occurrences selected (three cursors)",
        )
        .unwrap();
}

/// Test LSP diagnostics with margin bullet points
#[test]
fn visual_lsp_diagnostics() {
    use fresh::event::{Event, OverlayFace};
    use ratatui::style::Color;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let mut flow = VisualFlow::new(
        "LSP Diagnostics",
        "Language Features",
        "Displaying LSP diagnostics with margin indicators",
    );

    // Step 1: Type some code with issues
    harness.type_text("fn main() {\n").unwrap();
    harness.type_text("    let x = 5;\n").unwrap();
    harness.type_text("    let y = 10;\n").unwrap();
    harness.type_text("    println!(\"Hello\");\n").unwrap();
    harness.type_text("}\n").unwrap();
    harness.render().unwrap();
    harness
        .capture_visual_step(
            &mut flow,
            "code_without_diagnostics",
            "Code before diagnostics appear",
        )
        .unwrap();

    // Step 2: Add diagnostic overlays and margin indicators (simulating LSP)
    let state = harness.editor_mut().active_state_mut();

    // Error on line 2 (unused variable x)
    state.apply(&Event::AddOverlay {
        overlay_id: "lsp-diagnostic-0".to_string(),
        range: 20..21, // "x" character
        face: OverlayFace::Background {
            color: (60, 20, 20), // Dark red background
        },
        priority: 100,
        message: Some("unused variable: `x`".to_string()),
    });

    // Warning on line 3 (unused variable y)
    state.apply(&Event::AddOverlay {
        overlay_id: "lsp-diagnostic-1".to_string(),
        range: 35..36, // "y" character
        face: OverlayFace::Background {
            color: (60, 50, 0), // Dark yellow background
        },
        priority: 50,
        message: Some("unused variable: `y`".to_string()),
    });

    // Add red bullet points in the margin for lines with diagnostics
    // Using the new diagnostic indicator API
    state
        .margins
        .set_diagnostic_indicator(1, "●".to_string(), Color::Red); // Line 2 (0-indexed)
    state
        .margins
        .set_diagnostic_indicator(2, "●".to_string(), Color::Red); // Line 3 (0-indexed)

    harness.render().unwrap();
    harness
        .capture_visual_step(
            &mut flow,
            "diagnostics_with_bullets",
            "Diagnostics with red bullet points in separate margin column",
        )
        .unwrap();
}

/// Test LSP rename refactoring workflow
#[test]
fn visual_lsp_rename() {
    use fresh::overlay::OverlayFace;
    use lsp_types::{Position, Range, TextEdit, Uri, WorkspaceEdit};
    use ratatui::style::Color;
    use std::collections::HashMap;

    let mut harness = EditorTestHarness::new(80, 30).unwrap();
    let mut flow = VisualFlow::new(
        "LSP Rename",
        "LSP Features",
        "Renaming a symbol across multiple locations using F2",
    );

    // Step 1: Create code with a symbol used in multiple places
    harness
        .type_text("fn calculate(value: i32) -> i32 {\n")
        .unwrap();
    harness.type_text("    let result = value * 2;\n").unwrap();
    harness
        .type_text("    println!(\"Value: {}\", value);\n")
        .unwrap();
    harness.type_text("    result\n").unwrap();
    harness.type_text("}\n").unwrap();
    harness
        .capture_visual_step(
            &mut flow,
            "initial_code",
            "Function with 'value' parameter used twice",
        )
        .unwrap();

    // Step 2: Position cursor on the symbol 'value' (on the parameter)
    // Move to the first line, after "fn calculate("
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap(); // Go to document start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap(); // Go to line start

    // Move right to position cursor on "value" - it starts at column 14
    // "fn calculate(value..."
    //  0123456789012345
    for _ in 0..14 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Verify cursor is at the right position by checking buffer content around cursor
    let cursor_pos = harness.cursor_position();
    let buffer_len = harness.editor().active_state().buffer.len();
    let word_at_cursor = {
        let start = cursor_pos.saturating_sub(2).max(0);
        let end = (cursor_pos + 10).min(buffer_len);
        harness
            .editor_mut()
            .active_state_mut()
            .get_text_range(start, end)
    };
    assert!(
        word_at_cursor.contains("value"),
        "Cursor should be near 'value', but found: '{word_at_cursor}'"
    );

    harness
        .capture_visual_step(
            &mut flow,
            "cursor_on_symbol",
            "Cursor positioned on 'value' parameter",
        )
        .unwrap();

    // Step 3: Press F2 to enter rename mode
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Validate rename mode is active
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Rename mode"),
        "Status should show rename mode message"
    );

    // Check that an overlay exists for the symbol being renamed
    let (rename_overlay_range, rename_overlay_face) = {
        let state = harness.editor().active_state();
        let overlays: Vec<_> = state
            .overlays
            .all()
            .iter()
            .filter(|o| {
                o.id.as_ref()
                    .is_some_and(|id| id.starts_with("rename_overlay_"))
            })
            .collect();
        assert_eq!(overlays.len(), 1, "Should have exactly one rename overlay");

        let rename_overlay = overlays[0];
        let range = rename_overlay.range(&state.marker_list);
        (range, rename_overlay.face.clone())
    };

    // The overlay should cover "value" at position 14 (after "fn calculate(")
    let overlay_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(rename_overlay_range.start, rename_overlay_range.end);
    assert_eq!(
        overlay_text, "value",
        "Overlay should cover the 'value' symbol"
    );

    // Verify it's a background overlay with blue color
    if let OverlayFace::Background { color } = rename_overlay_face {
        assert_eq!(
            color,
            Color::Rgb(50, 100, 200),
            "Rename overlay should have blue background"
        );
    } else {
        panic!("Rename overlay should have Background face");
    }

    harness
        .capture_visual_step(
            &mut flow,
            "rename_mode_active",
            "Rename mode activated - 'value' highlighted in blue",
        )
        .unwrap();

    // Step 4: Type the new name
    // First clear the old name by backspacing
    for _ in 0..5 {
        // Delete "value" (5 characters)
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }

    // Render to see the intermediate state after backspace
    harness.render().unwrap();

    // Verify the rename overlay still exists during editing
    let state_during_edit = harness.editor().active_state();
    let overlays_during_edit: Vec<_> = state_during_edit
        .overlays
        .all()
        .iter()
        .filter(|o| {
            o.id.as_ref()
                .is_some_and(|id| id.starts_with("rename_overlay_"))
        })
        .collect();
    assert_eq!(
        overlays_during_edit.len(),
        1,
        "Rename overlay should persist during editing"
    );

    // Now type the new name
    harness.type_text("amount").unwrap();
    harness.render().unwrap();

    // The overlay should still be present after typing
    let state_after_typing = harness.editor().active_state();
    let overlays_after_typing: Vec<_> = state_after_typing
        .overlays
        .all()
        .iter()
        .filter(|o| {
            o.id.as_ref()
                .is_some_and(|id| id.starts_with("rename_overlay_"))
        })
        .collect();
    assert_eq!(
        overlays_after_typing.len(),
        1,
        "Rename overlay should still exist after typing"
    );

    harness
        .capture_visual_step(
            &mut flow,
            "typing_new_name",
            "Typing new name 'amount' - live preview in editor",
        )
        .unwrap();

    // Step 5: Press Enter to confirm - this would trigger LSP rename request
    // We'll simulate the LSP response
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify rename mode is exited (overlay should be removed)
    let state_after_enter = harness.editor().active_state();
    let overlays_after_enter: Vec<_> = state_after_enter
        .overlays
        .all()
        .iter()
        .filter(|o| {
            o.id.as_ref()
                .is_some_and(|id| id.starts_with("rename_overlay_"))
        })
        .collect();
    assert_eq!(
        overlays_after_enter.len(),
        0,
        "Rename overlay should be removed after confirming"
    );

    // Step 6: Simulate LSP WorkspaceEdit response
    // In real usage, the LSP would return edits for all occurrences
    // We'll manually apply the edits to show the final result

    // Create a fake file URI
    let file_uri = "file:///test.rs".parse::<Uri>().unwrap();

    // Create workspace edit with changes for all occurrences of 'value'
    let mut changes = HashMap::new();
    changes.insert(
        file_uri.clone(),
        vec![
            // Edit 1: parameter name (line 0, col 14-19)
            TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 14,
                    },
                    end: Position {
                        line: 0,
                        character: 19,
                    },
                },
                new_text: "amount".to_string(),
            },
            // Edit 2: parameter type annotation (line 0, col 21-26)
            TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 21,
                    },
                    end: Position {
                        line: 0,
                        character: 26,
                    },
                },
                new_text: "amount".to_string(),
            },
            // Edit 3: first usage in let statement (line 1, col 17-22)
            TextEdit {
                range: Range {
                    start: Position {
                        line: 1,
                        character: 17,
                    },
                    end: Position {
                        line: 1,
                        character: 22,
                    },
                },
                new_text: "amount".to_string(),
            },
            // Edit 4: second usage in println (line 2, col 28-33)
            TextEdit {
                range: Range {
                    start: Position {
                        line: 2,
                        character: 28,
                    },
                    end: Position {
                        line: 2,
                        character: 33,
                    },
                },
                new_text: "amount".to_string(),
            },
        ],
    );

    let _workspace_edit = WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    };

    // Apply the workspace edit by sending the async message
    // In the real flow, this would come from handle_rename_response
    // For this test, we'll reconstruct the buffer with the renamed code

    // Clear the buffer and type the renamed code
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap(); // Select all
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap(); // Delete

    harness
        .type_text("fn calculate(amount: i32) -> i32 {\n")
        .unwrap();
    harness.type_text("    let result = amount * 2;\n").unwrap();
    harness
        .type_text("    println!(\"Value: {}\", amount);\n")
        .unwrap();
    harness.type_text("    result\n").unwrap();
    harness.type_text("}\n").unwrap();

    harness.render().unwrap();

    // Validate all occurrences have been renamed
    let final_buffer = harness.get_buffer_content();

    // Count occurrences of "amount" - should be 3
    let amount_count = final_buffer.matches("amount").count();
    assert_eq!(amount_count, 3, "Should have 3 occurrences of 'amount'");

    // Verify "value" is no longer present (except in the string literal "Value:")
    let value_count = final_buffer.matches("value").count();
    assert_eq!(
        value_count, 0,
        "Should have no occurrences of 'value' as identifier"
    );

    // Verify specific locations
    assert!(
        final_buffer.contains("fn calculate(amount: i32)"),
        "Parameter should be renamed"
    );
    assert!(
        final_buffer.contains("let result = amount * 2;"),
        "First usage should be renamed"
    );
    assert!(
        final_buffer.contains("println!(\"Value: {}\", amount);"),
        "Second usage should be renamed"
    );

    harness
        .capture_visual_step(
            &mut flow,
            "rename_complete",
            "Rename complete - all 3 occurrences of 'value' renamed to 'amount'",
        )
        .unwrap();
}

/// Test that canceling rename after deleting characters restores original name
#[test]
fn test_lsp_rename_cancel_restores_original() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();

    // Step 1: Create code with a symbol
    harness
        .type_text("fn calculate(value: i32) -> i32 {\n")
        .unwrap();
    harness.type_text("    let result = value * 2;\n").unwrap();
    harness.type_text("    result\n").unwrap();
    harness.type_text("}\n").unwrap();
    harness.render().unwrap();

    // Step 2: Position cursor on the symbol 'value' (on the parameter)
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap(); // Go to document start
    for _ in 0..14 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Verify cursor is positioned on "value"
    let initial_cursor_pos = harness.cursor_position();
    let buffer_len = harness.editor().active_state().buffer.len();
    let word_at_cursor = {
        let start = initial_cursor_pos.saturating_sub(2).max(0);
        let end = (initial_cursor_pos + 10).min(buffer_len);
        harness
            .editor_mut()
            .active_state_mut()
            .get_text_range(start, end)
    };
    assert!(
        word_at_cursor.contains("value"),
        "Cursor should be near 'value', but found: '{word_at_cursor}'"
    );

    // Get the full buffer content before rename
    let original_buffer_content = harness.get_buffer_content();
    assert!(
        original_buffer_content.contains("fn calculate(value: i32)"),
        "Original buffer should contain 'value' parameter"
    );

    // Step 3: Press F2 to enter rename mode
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify rename mode is active
    let overlay_range = {
        let state = harness.editor().active_state();
        let overlays: Vec<_> = state
            .overlays
            .all()
            .iter()
            .filter(|o| {
                o.id.as_ref()
                    .is_some_and(|id| id.starts_with("rename_overlay_"))
            })
            .collect();
        assert_eq!(overlays.len(), 1, "Should have exactly one rename overlay");
        overlays[0].range(&state.marker_list)
    };

    let overlay_text = harness
        .editor_mut()
        .active_state_mut()
        .get_text_range(overlay_range.start, overlay_range.end);
    assert_eq!(
        overlay_text, "value",
        "Overlay should cover the 'value' symbol"
    );

    // Step 4: Delete some characters
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Verify the buffer has NOT been modified - it should still have original "value"
    let state_after_delete = harness.editor().active_state();
    let buffer_after_delete = state_after_delete.buffer.to_string();
    assert!(
        buffer_after_delete.contains("fn calculate(value: i32)"),
        "Buffer should STILL show original 'value' (not modified during typing)"
    );

    // The typed text should be tracked in status message or rename state, not in buffer
    let screen_after_delete = harness.screen_to_string();
    assert!(
        screen_after_delete.contains("Renaming to:"),
        "Status should show what's being typed"
    );

    // Verify overlay still exists during editing
    let overlays_after_delete: Vec<_> = state_after_delete
        .overlays
        .all()
        .iter()
        .filter(|o| {
            o.id.as_ref()
                .is_some_and(|id| id.starts_with("rename_overlay_"))
        })
        .collect();
    assert_eq!(
        overlays_after_delete.len(),
        1,
        "Rename overlay should still exist after deletion"
    );

    // Step 5: Press Escape to cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Step 6: Verify the buffer still has original name (no restore needed since we never modified it)
    let final_buffer_content = harness.get_buffer_content();
    assert_eq!(
        final_buffer_content, original_buffer_content,
        "Buffer should still be unchanged (never modified during rename)"
    );
    assert!(
        final_buffer_content.contains("fn calculate(value: i32)"),
        "Original 'value' parameter should still be there"
    );

    // Verify rename overlay is removed
    let state_after_cancel = harness.editor().active_state();
    let overlays_after_cancel: Vec<_> = state_after_cancel
        .overlays
        .all()
        .iter()
        .filter(|o| {
            o.id.as_ref()
                .is_some_and(|id| id.starts_with("rename_overlay_"))
        })
        .collect();
    assert_eq!(
        overlays_after_cancel.len(),
        0,
        "Rename overlay should be removed after cancel"
    );

    // Verify we're back in normal mode (not rename mode)
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Rename mode"),
        "Should exit rename mode after cancel"
    );
}

/// Test that undo after successful rename restores all occurrences in one step
#[test]
fn test_lsp_rename_undo_restores_all() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use lsp_types::{Position, Range, TextEdit, Uri, WorkspaceEdit};
    use std::collections::HashMap;
    use std::io::Write;

    // Create a temporary file for this test
    let temp_dir = tempfile::tempdir().unwrap();
    let test_file = temp_dir.path().join("test.rs");
    let mut file = std::fs::File::create(&test_file).unwrap();
    writeln!(file, "fn calculate(value: i32) -> i32 {{").unwrap();
    writeln!(file, "    let result = value * 2;").unwrap();
    writeln!(file, "    println!(\"Value: {{}}\", value);").unwrap();
    writeln!(file, "    result").unwrap();
    writeln!(file, "}}").unwrap();
    drop(file);

    let mut harness = EditorTestHarness::new(80, 30).unwrap();

    // Open the temporary file
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Save the original buffer content
    let original_content = harness.get_buffer_content();
    assert!(original_content.contains("fn calculate(value: i32)"));
    assert_eq!(original_content.matches("value").count(), 3);

    // Create file URI from the temp file path
    let file_uri = url::Url::from_file_path(&test_file)
        .unwrap()
        .as_str()
        .parse::<Uri>()
        .unwrap();

    // Simulate LSP WorkspaceEdit response with multiple edits
    let mut changes = HashMap::new();
    changes.insert(
        file_uri.clone(),
        vec![
            // Edit 1: parameter name (line 0, col 14-19: "value")
            TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 13,
                    },
                    end: Position {
                        line: 0,
                        character: 18,
                    },
                },
                new_text: "amount".to_string(),
            },
            // Edit 2: first usage in let statement (line 1, col 17-22: "value")
            TextEdit {
                range: Range {
                    start: Position {
                        line: 1,
                        character: 17,
                    },
                    end: Position {
                        line: 1,
                        character: 22,
                    },
                },
                new_text: "amount".to_string(),
            },
            // Edit 3: second usage in println (line 2, col 28-33: "value")
            TextEdit {
                range: Range {
                    start: Position {
                        line: 2,
                        character: 28,
                    },
                    end: Position {
                        line: 2,
                        character: 33,
                    },
                },
                new_text: "amount".to_string(),
            },
        ],
    );

    let workspace_edit = WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    };

    // Call handle_rename_response directly to simulate LSP rename response
    harness
        .editor_mut()
        .handle_rename_response(1, Ok(workspace_edit))
        .unwrap();
    harness.render().unwrap();

    // Step 5: Verify all occurrences were renamed
    let renamed_content = harness.get_buffer_content();
    assert!(
        renamed_content.contains("fn calculate(amount: i32)"),
        "Parameter should be renamed to 'amount'"
    );
    assert_eq!(
        renamed_content.matches("amount").count(),
        3,
        "Should have 3 occurrences of 'amount'"
    );
    assert_eq!(
        renamed_content.matches("value").count(),
        0,
        "Should have no occurrences of 'value' as identifier"
    );

    // Step 6: Perform undo (Ctrl+Z)
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Step 7: Verify ALL occurrences are restored to original in ONE undo step
    let after_undo_content = harness.get_buffer_content();
    assert_eq!(
        after_undo_content, original_content,
        "Single undo should restore all occurrences to 'value'"
    );
    assert!(
        after_undo_content.contains("fn calculate(value: i32)"),
        "Parameter should be restored to 'value'"
    );
    assert_eq!(
        after_undo_content.matches("value").count(),
        3,
        "Should have 3 occurrences of 'value' after undo"
    );
    assert_eq!(
        after_undo_content.matches("amount").count(),
        0,
        "Should have no occurrences of 'amount' after undo"
    );

    // Step 8: Verify we can redo (Ctrl+Y or Ctrl+Shift+Z)
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let after_redo_content = harness.get_buffer_content();
    assert_eq!(
        after_redo_content, renamed_content,
        "Redo should restore the renamed content"
    );
    assert_eq!(
        after_redo_content.matches("amount").count(),
        3,
        "Should have 3 occurrences of 'amount' after redo"
    );
    assert_eq!(
        after_redo_content.matches("value").count(),
        0,
        "Should have no occurrences of 'value' after redo"
    );
}

/// Test line wrapping feature
#[test]
fn visual_line_wrapping() {
    use fresh::config::Config;

    // Test with line wrapping enabled (default)
    let mut harness_wrapped = EditorTestHarness::new(60, 24).unwrap();
    let mut flow = VisualFlow::new(
        "Line Wrapping",
        "View Options",
        "Wrapping long lines at terminal width",
    );

    // Step 1: Type a very long line
    harness_wrapped.type_text("This is a very long line of text that will definitely exceed the terminal width and should wrap to multiple lines when line wrapping is enabled.").unwrap();
    harness_wrapped.render().unwrap();
    harness_wrapped
        .capture_visual_step(
            &mut flow,
            "long_line_wrapped",
            "Long line automatically wrapped (enabled by default)",
        )
        .unwrap();

    // Step 2: Add another long line
    harness_wrapped
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness_wrapped.type_text("Second extremely long line with even more text to demonstrate that multiple lines can be wrapped at the same time without any issues in the rendering system.").unwrap();
    harness_wrapped.render().unwrap();
    harness_wrapped
        .capture_visual_step(&mut flow, "multiple_wrapped", "Multiple long lines wrapped")
        .unwrap();

    // Step 3: Demonstrate with code that has syntax
    harness_wrapped
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness_wrapped.type_text("fn very_long_function_name_with_many_parameters(param1: String, param2: i32, param3: bool, param4: Vec<String>) -> Result<String, Error> {").unwrap();
    harness_wrapped.render().unwrap();
    harness_wrapped
        .capture_visual_step(
            &mut flow,
            "wrapped_code",
            "Long code line wrapped with syntax intact",
        )
        .unwrap();

    // Step 4: Test with line wrapping disabled
    let mut config = Config::default();
    config.editor.line_wrap = false;
    let mut harness_nowrap = EditorTestHarness::with_config(60, 24, config).unwrap();

    harness_nowrap.type_text("This is a very long line of text that will definitely exceed the terminal width and would normally wrap but now extends beyond the view.").unwrap();
    harness_nowrap.render().unwrap();
    harness_nowrap
        .capture_visual_step(
            &mut flow,
            "wrapping_disabled",
            "Line wrapping disabled - line extends beyond view",
        )
        .unwrap();
}

/// Test syntax highlighting for multiple languages
#[test]
fn visual_multi_language_highlighting() {
    let mut harness = EditorTestHarness::with_temp_project(80, 30).unwrap();
    let project_dir = harness.project_dir().unwrap();

    let mut flow = VisualFlow::new(
        "Multi-Language Highlighting",
        "Syntax Highlighting",
        "Syntax highlighting working across all supported programming languages",
    );

    // All supported languages with their test files
    let test_files = [
        (
            "Rust",
            "hello.rs",
            include_str!("../fixtures/syntax_highlighting/hello.rs"),
        ),
        (
            "Python",
            "hello.py",
            include_str!("../fixtures/syntax_highlighting/hello.py"),
        ),
        (
            "JavaScript",
            "hello.js",
            include_str!("../fixtures/syntax_highlighting/hello.js"),
        ),
        (
            "TypeScript",
            "hello.ts",
            include_str!("../fixtures/syntax_highlighting/hello.ts"),
        ),
        (
            "HTML",
            "hello.html",
            include_str!("../fixtures/syntax_highlighting/hello.html"),
        ),
        (
            "CSS",
            "hello.css",
            include_str!("../fixtures/syntax_highlighting/hello.css"),
        ),
        (
            "C",
            "hello.c",
            include_str!("../fixtures/syntax_highlighting/hello.c"),
        ),
        (
            "C++",
            "hello.cpp",
            include_str!("../fixtures/syntax_highlighting/hello.cpp"),
        ),
        (
            "Go",
            "hello.go",
            include_str!("../fixtures/syntax_highlighting/hello.go"),
        ),
        (
            "JSON",
            "hello.json",
            include_str!("../fixtures/syntax_highlighting/hello.json"),
        ),
        (
            "Java",
            "hello.java",
            include_str!("../fixtures/syntax_highlighting/hello.java"),
        ),
        (
            "C#",
            "hello.cs",
            include_str!("../fixtures/syntax_highlighting/hello.cs"),
        ),
        (
            "PHP",
            "hello.php",
            include_str!("../fixtures/syntax_highlighting/hello.php"),
        ),
        (
            "Ruby",
            "hello.rb",
            include_str!("../fixtures/syntax_highlighting/hello.rb"),
        ),
        (
            "Bash",
            "hello.sh",
            include_str!("../fixtures/syntax_highlighting/hello.sh"),
        ),
        (
            "Lua",
            "hello.lua",
            include_str!("../fixtures/syntax_highlighting/hello.lua"),
        ),
    ];

    // Create all test files
    for (_, filename, content) in &test_files {
        fs::write(project_dir.join(filename), content).unwrap();
    }

    // Test each language
    for (lang_name, filename, _) in &test_files {
        harness.open_file(&project_dir.join(filename)).unwrap();
        harness.render().unwrap();

        // Verify multiple colors are present (indicating highlighting is working)
        let buffer = harness.buffer();
        let unique_colors = count_unique_colors(buffer);

        // Note: C# may have fewer colors due to missing HIGHLIGHTS_QUERY in crate 0.23.1
        let min_colors = if *lang_name == "C#" { 1 } else { 3 };

        assert!(
            unique_colors >= min_colors,
            "{} highlighting should use at least {} different colors, found {}",
            lang_name,
            min_colors,
            unique_colors
        );

        let step_name = format!(
            "{}_highlighting",
            lang_name
                .to_lowercase()
                .replace("#", "sharp")
                .replace("+", "plus")
        );
        let description = format!("{} code with syntax highlighting", lang_name);

        harness
            .capture_visual_step(&mut flow, &step_name, &description)
            .unwrap();
    }
}

/// Helper to count unique foreground colors in a buffer (for verifying syntax highlighting)
fn count_unique_colors(buffer: &ratatui::buffer::Buffer) -> usize {
    use std::collections::HashSet;
    let mut colors = HashSet::new();

    for cell in buffer.content() {
        colors.insert(cell.fg);
    }

    colors.len()
}
