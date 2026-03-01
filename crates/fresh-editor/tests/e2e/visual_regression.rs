// Consolidated visual regression tests
// These two tests exercise as many UI states as possible in minimal screenshots
// Additional functional tests preserve important assertions from original tests

use crate::common::harness::EditorTestHarness;
use crate::common::visual_testing::VisualFlow;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::event::{Event, OverlayFace};
use fresh::view::overlay::OverlayNamespace;
use ratatui::style::Color;
use std::fs;

/// Comprehensive visual test A:
/// - File explorer: OPEN
/// - Line wrapping: ON (default)
/// - Multiple cursors: YES (3 cursors selecting "hello")
/// - Syntax highlighting: YES (Rust code)
/// - LSP diagnostics: YES (margin bullets)
/// - Vertical scroll: YES (buffer scrolled partway)
/// - Split view: NO
/// - Command palette: CLOSED
#[test]
#[cfg_attr(windows, ignore)] // Visual regression tests require consistent terminal rendering
fn visual_comprehensive_a() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create test files for the file explorer
    fs::create_dir_all(project_dir.join("src")).unwrap();
    fs::write(
        project_dir.join("src/main.rs"),
        r#"// Main entry point
fn main() {
    let hello = "world";
    let hello = "again";
    let hello = "once more";
    println!("{}", hello);
}

// Helper function
fn helper(x: i32) -> i32 {
    let unused_var = 5;
    let another_unused = 10;
    x * 2
}

// More code to enable scrolling
fn long_function() {
    println!("Line 1");
    println!("Line 2");
    println!("Line 3");
    println!("Line 4");
    println!("Line 5");
}
"#,
    )
    .unwrap();
    fs::write(
        project_dir.join("README.md"),
        "# Test Project\n\nA test project for visual regression.\n",
    )
    .unwrap();
    fs::write(
        project_dir.join("Cargo.toml"),
        "[package]\nname = \"test\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    let mut flow = VisualFlow::new(
        "Comprehensive UI A",
        "Visual Regression",
        "File explorer open, line wrap on, multicursors, diagnostics, scrolled",
    );

    // Open the Rust file
    harness.open_file(&project_dir.join("src/main.rs")).unwrap();
    harness.render().unwrap();

    // Open file explorer with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Wait for file explorer to load
    let _ = harness.wait_until(|h| h.screen_to_string().contains("File explorer ready"));
    harness.render().unwrap();

    // Expand the src directory in the explorer
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Focus back on the editor pane (Ctrl+E when in file explorer focuses editor)
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Add LSP diagnostic markers (simulated)
    {
        // Add diagnostic overlay for "unused_var" on line 11
        harness
            .apply_event(Event::AddOverlay {
                namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
                range: 230..240,
                face: OverlayFace::Background {
                    color: (60, 20, 20),
                },
                priority: 100,
                message: Some("unused variable: `unused_var`".to_string()),
                extend_to_line_end: false,
                url: None,
            })
            .unwrap();

        // Add margin indicators
        let state = harness.editor_mut().active_state_mut();
        state
            .margins
            .set_diagnostic_indicator(10, "●".to_string(), Color::Red);
        state
            .margins
            .set_diagnostic_indicator(11, "●".to_string(), Color::Yellow);
    }

    // Scroll down a bit to show scrolled state
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Create multiple cursors by selecting "hello" occurrences
    // First, search for a word that appears multiple times
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Add next occurrence (Ctrl+D)
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Capture the comprehensive state
    harness
        .capture_visual_step(
            &mut flow,
            "state_a",
            "File explorer open + syntax highlighting + diagnostics + multicursors + scrolled",
        )
        .unwrap();
}

/// Comprehensive visual test B:
/// - File explorer: CLOSED
/// - Command palette: OPEN (with filter text)
/// - Line wrapping: OFF
/// - Horizontal scroll: YES (long line scrolled right)
/// - Split view: YES (horizontal split with two files)
/// - Vertical scroll: YES
/// - Multiple cursors: NO (single cursor)
/// - LSP diagnostics: NO
#[test]
fn visual_comprehensive_b() {
    use fresh::config::Config;

    // Configure with line wrapping disabled
    let mut config = Config::default();
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create test files
    fs::write(
        project_dir.join("file1.rs"),
        r#"// File 1 - Contains a very long line that will require horizontal scrolling to see the end of it completely when line wrapping is disabled
fn main() {
    let very_long_variable_name_that_extends_beyond_normal_view = "This is a string with a lot of content that goes way past the edge";
    println!("{}", very_long_variable_name_that_extends_beyond_normal_view);
}
"#,
    )
    .unwrap();
    fs::write(
        project_dir.join("file2.rs"),
        r#"// File 2 - Secondary file shown in split view
fn helper() {
    let x = 42;
    let y = x * 2;
    println!("Result: {}", y);
}
"#,
    )
    .unwrap();

    let mut flow = VisualFlow::new(
        "Comprehensive UI B",
        "Visual Regression",
        "Command palette open, split view, line wrap off, horizontal scroll",
    );

    // Open first file
    harness.open_file(&project_dir.join("file1.rs")).unwrap();
    harness.render().unwrap();

    // Create horizontal split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split horiz").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open second file in the new split
    harness.open_file(&project_dir.join("file2.rs")).unwrap();
    harness.render().unwrap();

    // Go back to first pane and scroll right to show horizontal scroll
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Move to the long line (line 3)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Scroll right on the long line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now open command palette with some filter text showing
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("help").unwrap();
    harness.render().unwrap();

    // Wait for the command palette to fully render with the "help" filter text
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("help")
        })
        .unwrap();

    // Capture the comprehensive state
    harness
        .capture_visual_step(
            &mut flow,
            "state_b",
            "Split view + command palette open + line wrap off + horizontal scroll",
        )
        .unwrap();
}

// ============================================================================
// Functional tests (no visual captures, but preserve important assertions)
// ============================================================================

/// Test that undo after successful rename restores all occurrences in one step
#[test]
fn test_lsp_rename_undo_restores_all() {
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

    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Open the temporary file
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Save the original buffer content
    let original_content = harness.get_buffer_content().unwrap();
    assert!(original_content.contains("fn calculate(value: i32)"));
    assert_eq!(original_content.matches("value").count(), 3);

    // Create file URI from the temp file path
    let file_uri = url::Url::from_file_path(&test_file)
        .unwrap()
        .as_str()
        .parse::<Uri>()
        .unwrap();

    // Simulate LSP WorkspaceEdit response with multiple edits
    #[allow(clippy::mutable_key_type)]
    let mut changes = HashMap::new();
    changes.insert(
        file_uri.clone(),
        vec![
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

    // Verify all occurrences were renamed
    let renamed_content = harness.get_buffer_content().unwrap();
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

    // Perform undo (Ctrl+Z)
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify ALL occurrences are restored to original in ONE undo step
    let after_undo_content = harness.get_buffer_content().unwrap();
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

    // Verify we can redo (Ctrl+Y)
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let after_redo_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        after_redo_content, renamed_content,
        "Redo should restore the renamed content"
    );
}

/// Test syntax highlighting for multiple languages (functional assertions only)
#[test]
fn test_multi_language_highlighting() {
    let mut harness = EditorTestHarness::with_temp_project(80, 30).unwrap();
    let project_dir = harness.project_dir().unwrap();

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
