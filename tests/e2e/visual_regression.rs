// Visual regression tests - these generate screenshots for documentation

use crate::common::harness::EditorTestHarness;
use crate::common::visual_testing::VisualFlow;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use tempfile::TempDir;

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
    harness.capture_visual_step(&mut flow, "initial", "Empty editor on startup").unwrap();

    // Step 2: Type some text
    harness.type_text("Hello, World!").unwrap();
    harness.capture_visual_step(&mut flow, "typed_text", "Text typed into buffer").unwrap();

    // Step 3: Add a new line
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("Second line").unwrap();
    harness.capture_visual_step(&mut flow, "multiline", "Multiple lines of text").unwrap();
}

/// Test file explorer workflow with visual captures
#[test]
fn visual_file_explorer() {
    let mut harness = EditorTestHarness::with_temp_project(80, 30).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create some test files
    fs::create_dir_all(project_dir.join("src")).unwrap();
    fs::write(project_dir.join("src/main.rs"), "fn main() {\n    println!(\"Hello\");\n}").unwrap();
    fs::write(project_dir.join("README.md"), "# Test Project\n").unwrap();

    let mut flow = VisualFlow::new(
        "File Explorer",
        "File Management",
        "Opening and navigating the file explorer",
    );

    // Step 1: Initial state
    harness.capture_visual_step(&mut flow, "initial", "Editor before opening file explorer").unwrap();

    // Step 2: Open file explorer with Ctrl+B
    harness.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "explorer_open", "File explorer opened in left pane").unwrap();

    // Step 3: Navigate down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "file_selected", "File selected in explorer").unwrap();

    // Step 4: Expand directory
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "dir_expanded", "Directory expanded to show contents").unwrap();
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
    harness.capture_visual_step(&mut flow, "initial", "Editor before opening command palette").unwrap();

    // Step 2: Open command palette with Ctrl+P
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "palette_open", "Command palette opened").unwrap();

    // Step 3: Type to filter commands
    harness.type_text("help").unwrap();
    harness.capture_visual_step(&mut flow, "filtered", "Commands filtered by search term").unwrap();
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
    harness.capture_visual_step(&mut flow, "initial", "Editor before opening help").unwrap();

    // Step 2: Open help with Ctrl+H
    harness.send_key(KeyCode::Char('h'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "help_open", "Help panel showing all keybindings").unwrap();
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
    let mut flow = VisualFlow::new(
        "Split View",
        "Layout",
        "Working with split panes",
    );

    // Step 1: Open first file
    harness.open_file(&file1).unwrap();
    harness.capture_visual_step(&mut flow, "single_file", "Single file open").unwrap();

    // Step 2: Split horizontally with Alt+H
    harness.send_key(KeyCode::Char('h'), KeyModifiers::ALT).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "horizontal_split", "Editor split horizontally").unwrap();

    // Step 3: Open second file in split
    harness.open_file(&file2).unwrap();
    harness.capture_visual_step(&mut flow, "two_files", "Two files visible in split panes").unwrap();
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

    harness.capture_visual_step(&mut flow, "syntax_highlighting", "Syntax highlighting for Rust code").unwrap();
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
    harness.capture_visual_step(&mut flow, "initial_text", "Three lines with 'hello'").unwrap();

    // Step 2: Select word
    harness.send_key(KeyCode::Char('w'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "word_selected", "First word selected").unwrap();

    // Step 3: Add next occurrence with Ctrl+D
    harness.send_key(KeyCode::Char('d'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "two_cursors", "Second occurrence selected (two cursors)").unwrap();

    // Step 4: Add third occurrence
    harness.send_key(KeyCode::Char('d'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "three_cursors", "All occurrences selected (three cursors)").unwrap();
}

/// Test LSP diagnostics with margin bullet points
#[test]
fn visual_lsp_diagnostics() {
    use editor::event::{Event, OverlayFace};
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
    harness.capture_visual_step(&mut flow, "code_without_diagnostics", "Code before diagnostics appear").unwrap();

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
    state.margins.set_diagnostic_indicator(1, "●".to_string(), Color::Red); // Line 2 (0-indexed)
    state.margins.set_diagnostic_indicator(2, "●".to_string(), Color::Red); // Line 3 (0-indexed)

    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "diagnostics_with_bullets", "Diagnostics with red bullet points in separate margin column").unwrap();
}
